#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

#include "putty.h"

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

static Socket s = NULL;

#define	IAC	255		       /* interpret as command: */
#define	DONT	254		       /* you are not to use option */
#define	DO	253		       /* please, you use option */
#define	WONT	252		       /* I won't use option */
#define	WILL	251		       /* I will use option */
#define	SB	250		       /* interpret as subnegotiation */
#define	SE	240		       /* end sub negotiation */

#define GA      249		       /* you may reverse the line */
#define EL      248		       /* erase the current line */
#define EC      247		       /* erase the current character */
#define	AYT	246		       /* are you there */
#define	AO	245		       /* abort output--but let prog finish */
#define	IP	244		       /* interrupt process--permanently */
#define	BREAK	243		       /* break */
#define DM      242		       /* data mark--for connect. cleaning */
#define NOP     241		       /* nop */
#define EOR     239		       /* end of record (transparent mode) */
#define ABORT   238		       /* Abort process */
#define SUSP    237		       /* Suspend process */
#define xEOF    236		       /* End of file: EOF is already used... */

#define TELOPT_BINARY	0	       /* 8-bit data path */
#define TELOPT_ECHO	1	       /* echo */
#define	TELOPT_RCP	2	       /* prepare to reconnect */
#define	TELOPT_SGA	3	       /* suppress go ahead */
#define	TELOPT_NAMS	4	       /* approximate message size */
#define	TELOPT_STATUS	5	       /* give status */
#define	TELOPT_TM	6	       /* timing mark */
#define	TELOPT_RCTE	7	       /* remote controlled transmission and echo */
#define TELOPT_NAOL 	8	       /* negotiate about output line width */
#define TELOPT_NAOP 	9	       /* negotiate about output page size */
#define TELOPT_NAOCRD	10	       /* negotiate about CR disposition */
#define TELOPT_NAOHTS	11	       /* negotiate about horizontal tabstops */
#define TELOPT_NAOHTD	12	       /* negotiate about horizontal tab disposition */
#define TELOPT_NAOFFD	13	       /* negotiate about formfeed disposition */
#define TELOPT_NAOVTS	14	       /* negotiate about vertical tab stops */
#define TELOPT_NAOVTD	15	       /* negotiate about vertical tab disposition */
#define TELOPT_NAOLFD	16	       /* negotiate about output LF disposition */
#define TELOPT_XASCII	17	       /* extended ascic character set */
#define	TELOPT_LOGOUT	18	       /* force logout */
#define	TELOPT_BM	19	       /* byte macro */
#define	TELOPT_DET	20	       /* data entry terminal */
#define	TELOPT_SUPDUP	21	       /* supdup protocol */
#define	TELOPT_SUPDUPOUTPUT 22	       /* supdup output */
#define	TELOPT_SNDLOC	23	       /* send location */
#define	TELOPT_TTYPE	24	       /* terminal type */
#define	TELOPT_EOR	25	       /* end or record */
#define	TELOPT_TUID	26	       /* TACACS user identification */
#define	TELOPT_OUTMRK	27	       /* output marking */
#define	TELOPT_TTYLOC	28	       /* terminal location number */
#define	TELOPT_3270REGIME 29	       /* 3270 regime */
#define	TELOPT_X3PAD	30	       /* X.3 PAD */
#define	TELOPT_NAWS	31	       /* window size */
#define	TELOPT_TSPEED	32	       /* terminal speed */
#define	TELOPT_LFLOW	33	       /* remote flow control */
#define TELOPT_LINEMODE	34	       /* Linemode option */
#define TELOPT_XDISPLOC	35	       /* X Display Location */
#define TELOPT_OLD_ENVIRON 36	       /* Old - Environment variables */
#define	TELOPT_AUTHENTICATION 37       /* Authenticate */
#define	TELOPT_ENCRYPT	38	       /* Encryption option */
#define TELOPT_NEW_ENVIRON 39	       /* New - Environment variables */
#define	TELOPT_EXOPL	255	       /* extended-options-list */

#define	TELQUAL_IS	0	       /* option is... */
#define	TELQUAL_SEND	1	       /* send option */
#define	TELQUAL_INFO	2	       /* ENVIRON: informational version of IS */
#define BSD_VAR 1
#define BSD_VALUE 0
#define RFC_VAR 0
#define RFC_VALUE 1

#define CR 13
#define LF 10
#define NUL 0

#define iswritable(x) ( (x) != IAC && (x) != CR )

static char *telopt(int opt)
{
#define i(x) if (opt == TELOPT_ ## x) return #x;
    i(BINARY);
    i(ECHO);
    i(RCP);
    i(SGA);
    i(NAMS);
    i(STATUS);
    i(TM);
    i(RCTE);
    i(NAOL);
    i(NAOP);
    i(NAOCRD);
    i(NAOHTS);
    i(NAOHTD);
    i(NAOFFD);
    i(NAOVTS);
    i(NAOVTD);
    i(NAOLFD);
    i(XASCII);
    i(LOGOUT);
    i(BM);
    i(DET);
    i(SUPDUP);
    i(SUPDUPOUTPUT);
    i(SNDLOC);
    i(TTYPE);
    i(EOR);
    i(TUID);
    i(OUTMRK);
    i(TTYLOC);
    i(X3PAD);
    i(NAWS);
    i(TSPEED);
    i(LFLOW);
    i(LINEMODE);
    i(XDISPLOC);
    i(OLD_ENVIRON);
    i(AUTHENTICATION);
    i(ENCRYPT);
    i(NEW_ENVIRON);
    i(EXOPL);
#undef i
    return "<unknown>";
}

static void telnet_size(void);

struct Opt {
    int send;			       /* what we initially send */
    int nsend;			       /* -ve send if requested to stop it */
    int ack, nak;		       /* +ve and -ve acknowledgements */
    int option;			       /* the option code */
    enum {
	REQUESTED, ACTIVE, INACTIVE, REALLY_INACTIVE
    } state;
};

static struct Opt o_naws =
    { WILL, WONT, DO, DONT, TELOPT_NAWS, REQUESTED };
static struct Opt o_tspeed =
    { WILL, WONT, DO, DONT, TELOPT_TSPEED, REQUESTED };
static struct Opt o_ttype =
    { WILL, WONT, DO, DONT, TELOPT_TTYPE, REQUESTED };
static struct Opt o_oenv = { WILL, WONT, DO, DONT, TELOPT_OLD_ENVIRON,
    INACTIVE
};
static struct Opt o_nenv = { WILL, WONT, DO, DONT, TELOPT_NEW_ENVIRON,
    REQUESTED
};
static struct Opt o_echo =
    { DO, DONT, WILL, WONT, TELOPT_ECHO, REQUESTED };
static struct Opt o_we_sga =
    { WILL, WONT, DO, DONT, TELOPT_SGA, REQUESTED };
static struct Opt o_they_sga =
    { DO, DONT, WILL, WONT, TELOPT_SGA, REQUESTED };

static struct Opt *opts[] = {
    &o_naws, &o_tspeed, &o_ttype, &o_oenv, &o_nenv, &o_echo,
    &o_we_sga, &o_they_sga, NULL
};

#define TELNET_MAX_BACKLOG 4096

static int echoing = TRUE, editing = TRUE;
static int activated = FALSE;
static int telnet_bufsize;
static int in_synch;
static int sb_opt, sb_len;
static char *sb_buf = NULL;
static int sb_size = 0;
#define SB_DELTA 1024

static void c_write1(int c)
{
    int backlog;
    char cc = (char) c;
    backlog = from_backend(0, &cc, 1);
    sk_set_frozen(s, backlog > TELNET_MAX_BACKLOG);
}

static void log_option(char *sender, int cmd, int option)
{
    char buf[50];
    sprintf(buf, "%s:\t%s %s", sender,
	    (cmd == WILL ? "WILL" : cmd == WONT ? "WONT" :
	     cmd == DO ? "DO" : cmd == DONT ? "DONT" : "<??>"),
	    telopt(option));
    logevent(buf);
}

static void send_opt(int cmd, int option)
{
    unsigned char b[3];

    b[0] = IAC;
    b[1] = cmd;
    b[2] = option;
    telnet_bufsize = sk_write(s, b, 3);
    log_option("client", cmd, option);
}

static void deactivate_option(struct Opt *o)
{
    if (o->state == REQUESTED || o->state == ACTIVE)
	send_opt(o->nsend, o->option);
    o->state = REALLY_INACTIVE;
}

/*
 * Generate side effects of enabling or disabling an option.
 */
static void option_side_effects(struct Opt *o, int enabled)
{
    if (o->option == TELOPT_ECHO && o->send == DO)
	echoing = !enabled;
    else if (o->option == TELOPT_SGA && o->send == DO)
	editing = !enabled;
    ldisc_send(NULL, 0, 0);	       /* cause ldisc to notice the change */

    /* Ensure we get the minimum options */
    if (!activated) {
	if (o_echo.state == INACTIVE) {
	    o_echo.state = REQUESTED;
	    send_opt(o_echo.send, o_echo.option);
	}
	if (o_we_sga.state == INACTIVE) {
	    o_we_sga.state = REQUESTED;
	    send_opt(o_we_sga.send, o_we_sga.option);
	}
	if (o_they_sga.state == INACTIVE) {
	    o_they_sga.state = REQUESTED;
	    send_opt(o_they_sga.send, o_they_sga.option);
	}
	activated = TRUE;
    }
}

static void activate_option(struct Opt *o)
{
    if (o->send == WILL && o->option == TELOPT_NAWS)
	telnet_size();
    if (o->send == WILL &&
	(o->option == TELOPT_NEW_ENVIRON ||
	 o->option == TELOPT_OLD_ENVIRON)) {
	/*
	 * We may only have one kind of ENVIRON going at a time.
	 * This is a hack, but who cares.
	 */
	deactivate_option(o->option ==
			  TELOPT_NEW_ENVIRON ? &o_oenv : &o_nenv);
    }
    option_side_effects(o, 1);
}

static void refused_option(struct Opt *o)
{
    if (o->send == WILL && o->option == TELOPT_NEW_ENVIRON &&
	o_oenv.state == INACTIVE) {
	send_opt(WILL, TELOPT_OLD_ENVIRON);
	o_oenv.state = REQUESTED;
    }
    option_side_effects(o, 0);
}

static void proc_rec_opt(int cmd, int option)
{
    struct Opt **o;

    log_option("server", cmd, option);
    for (o = opts; *o; o++) {
	if ((*o)->option == option && (*o)->ack == cmd) {
	    switch ((*o)->state) {
	      case REQUESTED:
		(*o)->state = ACTIVE;
		activate_option(*o);
		break;
	      case ACTIVE:
		break;
	      case INACTIVE:
		(*o)->state = ACTIVE;
		send_opt((*o)->send, option);
		activate_option(*o);
		break;
	      case REALLY_INACTIVE:
		send_opt((*o)->nsend, option);
		break;
	    }
	    return;
	} else if ((*o)->option == option && (*o)->nak == cmd) {
	    switch ((*o)->state) {
	      case REQUESTED:
		(*o)->state = INACTIVE;
		refused_option(*o);
		break;
	      case ACTIVE:
		(*o)->state = INACTIVE;
		send_opt((*o)->nsend, option);
		option_side_effects(*o, 0);
		break;
	      case INACTIVE:
	      case REALLY_INACTIVE:
		break;
	    }
	    return;
	}
    }
    /*
     * If we reach here, the option was one we weren't prepared to
     * cope with. So send a negative ack.
     */
    send_opt((cmd == WILL ? DONT : WONT), option);
}

static void process_subneg(void)
{
    unsigned char b[2048], *p, *q;
    int var, value, n;
    char *e;

    switch (sb_opt) {
      case TELOPT_TSPEED:
	if (sb_len == 1 && sb_buf[0] == TELQUAL_SEND) {
	    char logbuf[sizeof(cfg.termspeed) + 80];
	    b[0] = IAC;
	    b[1] = SB;
	    b[2] = TELOPT_TSPEED;
	    b[3] = TELQUAL_IS;
	    strcpy(b + 4, cfg.termspeed);
	    n = 4 + strlen(cfg.termspeed);
	    b[n] = IAC;
	    b[n + 1] = SE;
	    telnet_bufsize = sk_write(s, b, n + 2);
	    logevent("server:\tSB TSPEED SEND");
	    sprintf(logbuf, "client:\tSB TSPEED IS %s", cfg.termspeed);
	    logevent(logbuf);
	} else
	    logevent("server:\tSB TSPEED <something weird>");
	break;
      case TELOPT_TTYPE:
	if (sb_len == 1 && sb_buf[0] == TELQUAL_SEND) {
	    char logbuf[sizeof(cfg.termtype) + 80];
	    b[0] = IAC;
	    b[1] = SB;
	    b[2] = TELOPT_TTYPE;
	    b[3] = TELQUAL_IS;
	    for (n = 0; cfg.termtype[n]; n++)
		b[n + 4] = (cfg.termtype[n] >= 'a'
			    && cfg.termtype[n] <=
			    'z' ? cfg.termtype[n] + 'A' -
			    'a' : cfg.termtype[n]);
	    b[n + 4] = IAC;
	    b[n + 5] = SE;
	    telnet_bufsize = sk_write(s, b, n + 6);
	    b[n + 4] = 0;
	    logevent("server:\tSB TTYPE SEND");
	    sprintf(logbuf, "client:\tSB TTYPE IS %s", b + 4);
	    logevent(logbuf);
	} else
	    logevent("server:\tSB TTYPE <something weird>\r\n");
	break;
      case TELOPT_OLD_ENVIRON:
      case TELOPT_NEW_ENVIRON:
	p = sb_buf;
	q = p + sb_len;
	if (p < q && *p == TELQUAL_SEND) {
	    char logbuf[50];
	    p++;
	    sprintf(logbuf, "server:\tSB %s SEND", telopt(sb_opt));
	    logevent(logbuf);
	    if (sb_opt == TELOPT_OLD_ENVIRON) {
		if (cfg.rfc_environ) {
		    value = RFC_VALUE;
		    var = RFC_VAR;
		} else {
		    value = BSD_VALUE;
		    var = BSD_VAR;
		}
		/*
		 * Try to guess the sense of VAR and VALUE.
		 */
		while (p < q) {
		    if (*p == RFC_VAR) {
			value = RFC_VALUE;
			var = RFC_VAR;
		    } else if (*p == BSD_VAR) {
			value = BSD_VALUE;
			var = BSD_VAR;
		    }
		    p++;
		}
	    } else {
		/*
		 * With NEW_ENVIRON, the sense of VAR and VALUE
		 * isn't in doubt.
		 */
		value = RFC_VALUE;
		var = RFC_VAR;
	    }
	    b[0] = IAC;
	    b[1] = SB;
	    b[2] = sb_opt;
	    b[3] = TELQUAL_IS;
	    n = 4;
	    e = cfg.environmt;
	    while (*e) {
		b[n++] = var;
		while (*e && *e != '\t')
		    b[n++] = *e++;
		if (*e == '\t')
		    e++;
		b[n++] = value;
		while (*e)
		    b[n++] = *e++;
		e++;
	    }
	    if (*cfg.username) {
		b[n++] = var;
		b[n++] = 'U';
		b[n++] = 'S';
		b[n++] = 'E';
		b[n++] = 'R';
		b[n++] = value;
		e = cfg.username;
		while (*e)
		    b[n++] = *e++;
	    }
	    b[n++] = IAC;
	    b[n++] = SE;
	    telnet_bufsize = sk_write(s, b, n);
	    sprintf(logbuf, "client:\tSB %s IS %s", telopt(sb_opt),
		    n == 6 ? "<nothing>" : "<stuff>");
	    logevent(logbuf);
	}
	break;
    }
}

static enum {
    TOPLEVEL, SEENIAC, SEENWILL, SEENWONT, SEENDO, SEENDONT,
    SEENSB, SUBNEGOT, SUBNEG_IAC, SEENCR
} telnet_state = TOPLEVEL;

static void do_telnet_read(char *buf, int len)
{

    while (len--) {
	int c = (unsigned char) *buf++;

	switch (telnet_state) {
	  case TOPLEVEL:
	  case SEENCR:
	    if (c == NUL && telnet_state == SEENCR)
		telnet_state = TOPLEVEL;
	    else if (c == IAC)
		telnet_state = SEENIAC;
	    else {
		if (!in_synch)
		    c_write1(c);

#if 1
		/* I can't get the F***ing winsock to insert the urgent IAC
		 * into the right position! Even with SO_OOBINLINE it gives
		 * it to recv too soon. And of course the DM byte (that
		 * arrives in the same packet!) appears several K later!!
		 *
		 * Oh well, we do get the DM in the right place so I'll
		 * just stop hiding on the next 0xf2 and hope for the best.
		 */
		else if (c == DM)
		    in_synch = 0;
#endif
		if (c == CR)
		    telnet_state = SEENCR;
		else
		    telnet_state = TOPLEVEL;
	    }
	    break;
	  case SEENIAC:
	    if (c == DO)
		telnet_state = SEENDO;
	    else if (c == DONT)
		telnet_state = SEENDONT;
	    else if (c == WILL)
		telnet_state = SEENWILL;
	    else if (c == WONT)
		telnet_state = SEENWONT;
	    else if (c == SB)
		telnet_state = SEENSB;
	    else if (c == DM) {
		in_synch = 0;
		telnet_state = TOPLEVEL;
	    } else {
		/* ignore everything else; print it if it's IAC */
		if (c == IAC) {
		    c_write1(c);
		}
		telnet_state = TOPLEVEL;
	    }
	    break;
	  case SEENWILL:
	    proc_rec_opt(WILL, c);
	    telnet_state = TOPLEVEL;
	    break;
	  case SEENWONT:
	    proc_rec_opt(WONT, c);
	    telnet_state = TOPLEVEL;
	    break;
	  case SEENDO:
	    proc_rec_opt(DO, c);
	    telnet_state = TOPLEVEL;
	    break;
	  case SEENDONT:
	    proc_rec_opt(DONT, c);
	    telnet_state = TOPLEVEL;
	    break;
	  case SEENSB:
	    sb_opt = c;
	    sb_len = 0;
	    telnet_state = SUBNEGOT;
	    break;
	  case SUBNEGOT:
	    if (c == IAC)
		telnet_state = SUBNEG_IAC;
	    else {
	      subneg_addchar:
		if (sb_len >= sb_size) {
		    char *newbuf;
		    sb_size += SB_DELTA;
		    newbuf = (sb_buf ?
			      srealloc(sb_buf, sb_size) :
			      smalloc(sb_size));
		    if (newbuf)
			sb_buf = newbuf;
		    else
			sb_size -= SB_DELTA;
		}
		if (sb_len < sb_size)
		    sb_buf[sb_len++] = c;
		telnet_state = SUBNEGOT;	/* in case we came here by goto */
	    }
	    break;
	  case SUBNEG_IAC:
	    if (c != SE)
		goto subneg_addchar;   /* yes, it's a hack, I know, but... */
	    else {
		process_subneg();
		telnet_state = TOPLEVEL;
	    }
	    break;
	}
    }
}

static int telnet_closing(Plug plug, char *error_msg, int error_code,
			  int calling_back)
{
    if (s) {
        sk_close(s);
        s = NULL;
    }
    if (error_msg) {
	/* A socket error has occurred. */
	logevent(error_msg);
	connection_fatal("%s", error_msg);
    }				       /* Otherwise, the remote side closed the connection normally. */
    return 0;
}

static int telnet_receive(Plug plug, int urgent, char *data, int len)
{
    if (urgent)
	in_synch = TRUE;
    do_telnet_read(data, len);
    return 1;
}

static void telnet_sent(Plug plug, int bufsize)
{
    telnet_bufsize = bufsize;
}

/*
 * Called to set up the Telnet connection.
 *
 * Returns an error message, or NULL on success.
 *
 * Also places the canonical host name into `realhost'. It must be
 * freed by the caller.
 */
static char *telnet_init(char *host, int port, char **realhost, int nodelay)
{
    static struct plug_function_table fn_table = {
	telnet_closing,
	telnet_receive,
	telnet_sent
    }, *fn_table_ptr = &fn_table;

    SockAddr addr;
    char *err;

    /*
     * Try to find host.
     */
    {
	char buf[200];
	sprintf(buf, "Looking up host \"%.170s\"", host);
	logevent(buf);
    }
    addr = sk_namelookup(host, realhost);
    if ((err = sk_addr_error(addr)))
	return err;

    if (port < 0)
	port = 23;		       /* default telnet port */

    /*
     * Open socket.
     */
    {
	char buf[200], addrbuf[100];
	sk_getaddr(addr, addrbuf, 100);
	sprintf(buf, "Connecting to %.100s port %d", addrbuf, port);
	logevent(buf);
    }
    s = new_connection(addr, *realhost, port, 0, 1, nodelay, &fn_table_ptr);
    if ((err = sk_socket_error(s)))
	return err;

    sk_addr_free(addr);

    /*
     * Initialise option states.
     */
    if (cfg.passive_telnet) {
	struct Opt **o;

	for (o = opts; *o; o++)
	    if ((*o)->state == REQUESTED)
		(*o)->state = INACTIVE;
    } else {
	struct Opt **o;

	for (o = opts; *o; o++)
	    if ((*o)->state == REQUESTED)
		send_opt((*o)->send, (*o)->option);
	activated = TRUE;
    }

    /*
     * Set up SYNCH state.
     */
    in_synch = FALSE;

    return NULL;
}

/*
 * Called to send data down the Telnet connection.
 */
static int telnet_send(char *buf, int len)
{
    char *p;
    static unsigned char iac[2] = { IAC, IAC };
    static unsigned char cr[2] = { CR, NUL };
#if 0
    static unsigned char nl[2] = { CR, LF };
#endif

    if (s == NULL)
	return 0;

    p = buf;
    while (p < buf + len) {
	char *q = p;

	while (iswritable((unsigned char) *p) && p < buf + len)
	    p++;
	telnet_bufsize = sk_write(s, q, p - q);

	while (p < buf + len && !iswritable((unsigned char) *p)) {
	    telnet_bufsize = 
		sk_write(s, (unsigned char) *p == IAC ? iac : cr, 2);
	    p++;
	}
    }

    return telnet_bufsize;
}

/*
 * Called to query the current socket sendability status.
 */
static int telnet_sendbuffer(void)
{
    return telnet_bufsize;
}

/*
 * Called to set the size of the window from Telnet's POV.
 */
static void telnet_size(void)
{
    unsigned char b[16];
    char logbuf[50];

    if (s == NULL || o_naws.state != ACTIVE)
	return;
    b[0] = IAC;
    b[1] = SB;
    b[2] = TELOPT_NAWS;
    b[3] = cols >> 8;
    b[4] = cols & 0xFF;
    b[5] = rows >> 8;
    b[6] = rows & 0xFF;
    b[7] = IAC;
    b[8] = SE;
    telnet_bufsize = sk_write(s, b, 9);
    sprintf(logbuf, "client:\tSB NAWS %d,%d",
	    ((unsigned char) b[3] << 8) + (unsigned char) b[4],
	    ((unsigned char) b[5] << 8) + (unsigned char) b[6]);
    logevent(logbuf);
}

/*
 * Send Telnet special codes.
 */
static void telnet_special(Telnet_Special code)
{
    unsigned char b[2];

    if (s == NULL)
	return;

    b[0] = IAC;
    switch (code) {
      case TS_AYT:
	b[1] = AYT;
	telnet_bufsize = sk_write(s, b, 2);
	break;
      case TS_BRK:
	b[1] = BREAK;
	telnet_bufsize = sk_write(s, b, 2);
	break;
      case TS_EC:
	b[1] = EC;
	telnet_bufsize = sk_write(s, b, 2);
	break;
      case TS_EL:
	b[1] = EL;
	telnet_bufsize = sk_write(s, b, 2);
	break;
      case TS_GA:
	b[1] = GA;
	telnet_bufsize = sk_write(s, b, 2);
	break;
      case TS_NOP:
	b[1] = NOP;
	telnet_bufsize = sk_write(s, b, 2);
	break;
      case TS_ABORT:
	b[1] = ABORT;
	telnet_bufsize = sk_write(s, b, 2);
	break;
      case TS_AO:
	b[1] = AO;
	telnet_bufsize = sk_write(s, b, 2);
	break;
      case TS_IP:
	b[1] = IP;
	telnet_bufsize = sk_write(s, b, 2);
	break;
      case TS_SUSP:
	b[1] = SUSP;
	telnet_bufsize = sk_write(s, b, 2);
	break;
      case TS_EOR:
	b[1] = EOR;
	telnet_bufsize = sk_write(s, b, 2);
	break;
      case TS_EOF:
	b[1] = xEOF;
	telnet_bufsize = sk_write(s, b, 2);
	break;
      case TS_EOL:
	telnet_bufsize = sk_write(s, "\r\n", 2);
	break;
      case TS_SYNCH:
	b[1] = DM;
	telnet_bufsize = sk_write(s, b, 1);
	telnet_bufsize = sk_write_oob(s, b + 1, 1);
	break;
      case TS_RECHO:
	if (o_echo.state == INACTIVE || o_echo.state == REALLY_INACTIVE) {
	    o_echo.state = REQUESTED;
	    send_opt(o_echo.send, o_echo.option);
	}
	break;
      case TS_LECHO:
	if (o_echo.state == ACTIVE) {
	    o_echo.state = REQUESTED;
	    send_opt(o_echo.nsend, o_echo.option);
	}
	break;
      case TS_PING:
	if (o_they_sga.state == ACTIVE) {
	    b[1] = NOP;
	    telnet_bufsize = sk_write(s, b, 2);
	}
	break;
    }
}

static Socket telnet_socket(void)
{
    return s;
}

static int telnet_sendok(void)
{
    return 1;
}

static void telnet_unthrottle(int backlog)
{
    sk_set_frozen(s, backlog > TELNET_MAX_BACKLOG);
}

static int telnet_ldisc(int option)
{
    if (option == LD_ECHO)
	return echoing;
    if (option == LD_EDIT)
	return editing;
    return FALSE;
}

static int telnet_exitcode(void)
{
    /* Telnet doesn't transmit exit codes back to the client */
    return 0;
}

Backend telnet_backend = {
    telnet_init,
    telnet_send,
    telnet_sendbuffer,
    telnet_size,
    telnet_special,
    telnet_socket,
    telnet_exitcode,
    telnet_sendok,
    telnet_ldisc,
    telnet_unthrottle,
    23
};
