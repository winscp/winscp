/*
 * Platform-independent bits of X11 forwarding.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>

#include "putty.h"
#include "ssh.h"
#include "tree234.h"

#define GET_16BIT(endian, cp) \
  (endian=='B' ? GET_16BIT_MSB_FIRST(cp) : GET_16BIT_LSB_FIRST(cp))

#define PUT_16BIT(endian, cp, val) \
  (endian=='B' ? PUT_16BIT_MSB_FIRST(cp, val) : PUT_16BIT_LSB_FIRST(cp, val))

const char *const x11_authnames[] = {
    "", "MIT-MAGIC-COOKIE-1", "XDM-AUTHORIZATION-1"
};

struct XDMSeen {
    unsigned int time;
    unsigned char clientid[6];
};

struct X11Private {
    const struct plug_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */
    unsigned char firstpkt[12];	       /* first X data packet */
    struct X11Display *disp;
    char *auth_protocol;
    unsigned char *auth_data;
    int data_read, auth_plen, auth_psize, auth_dlen, auth_dsize;
    int verified;
    int throttled, throttle_override;
    unsigned long peer_ip;
    int peer_port;
    void *c;			       /* data used by ssh.c */
    Socket s;
};

static int xdmseen_cmp(void *a, void *b)
{
    struct XDMSeen *sa = a, *sb = b;
    return sa->time > sb->time ? 1 :
	   sa->time < sb->time ? -1 :
           memcmp(sa->clientid, sb->clientid, sizeof(sa->clientid));
}

/* Do-nothing "plug" implementation, used by x11_setup_display() when it
 * creates a trial connection (and then immediately closes it).
 * XXX: bit out of place here, could in principle live in a platform-
 *      independent network.c or something */
static void dummy_plug_log(Plug p, int type, SockAddr addr, int port,
			   const char *error_msg, int error_code) { }
static int dummy_plug_closing
     (Plug p, const char *error_msg, int error_code, int calling_back)
{ return 1; }
static int dummy_plug_receive(Plug p, int urgent, char *data, int len)
{ return 1; }
static void dummy_plug_sent(Plug p, int bufsize) { }
static int dummy_plug_accepting(Plug p, OSSocket sock) { return 1; }
static const struct plug_function_table dummy_plug = {
    dummy_plug_log, dummy_plug_closing, dummy_plug_receive,
    dummy_plug_sent, dummy_plug_accepting
};

struct X11Display *x11_setup_display(char *display, int authtype,
				     const Config *cfg)
{
    struct X11Display *disp = snew(struct X11Display);
    char *localcopy;
    int i;

    if (!display || !*display) {
	localcopy = platform_get_x_display();
	if (!localcopy || !*localcopy) {
	    sfree(localcopy);
	    localcopy = dupstr(":0");  /* plausible default for any platform */
	}
    } else
	localcopy = dupstr(display);

    /*
     * Parse the display name.
     *
     * We expect this to have one of the following forms:
     * 
     *  - the standard X format which looks like
     *    [ [ protocol '/' ] host ] ':' displaynumber [ '.' screennumber ]
     *    (X11 also permits a double colon to indicate DECnet, but
     *    that's not our problem, thankfully!)
     *
     * 	- only seen in the wild on MacOS (so far): a pathname to a
     * 	  Unix-domain socket, which will typically and confusingly
     * 	  end in ":0", and which I'm currently distinguishing from
     * 	  the standard scheme by noting that it starts with '/'.
     */
    if (localcopy[0] == '/') {
	disp->unixsocketpath = localcopy;
	disp->unixdomain = TRUE;
	disp->hostname = NULL;
	disp->displaynum = -1;
	disp->screennum = 0;
	disp->addr = NULL;
    } else {
	char *colon, *dot, *slash;
	char *protocol, *hostname;

	colon = strrchr(localcopy, ':');
	if (!colon) {
	    sfree(disp);
	    sfree(localcopy);
	    return NULL;	       /* FIXME: report a specific error? */
	}

	*colon++ = '\0';
	dot = strchr(colon, '.');
	if (dot)
	    *dot++ = '\0';

	disp->displaynum = atoi(colon);
	if (dot)
	    disp->screennum = atoi(dot);
	else
	    disp->screennum = 0;

	protocol = NULL;
	hostname = localcopy;
	if (colon > localcopy) {
	    slash = strchr(localcopy, '/');
	    if (slash) {
		*slash++ = '\0';
		protocol = localcopy;
		hostname = slash;
	    }
	}

	disp->hostname = *hostname ? dupstr(hostname) : NULL;

	if (protocol)
	    disp->unixdomain = (!strcmp(protocol, "local") ||
				!strcmp(protocol, "unix"));
	else if (!*hostname || !strcmp(hostname, "unix"))
	    disp->unixdomain = platform_uses_x11_unix_by_default;
	else
	    disp->unixdomain = FALSE;

	if (!disp->hostname && !disp->unixdomain)
	    disp->hostname = dupstr("localhost");

	disp->unixsocketpath = NULL;
	disp->addr = NULL;

	sfree(localcopy);
    }

    /*
     * Look up the display hostname, if we need to.
     */
    if (!disp->unixdomain) {
	const char *err;

	disp->port = 6000 + disp->displaynum;
	disp->addr = name_lookup(disp->hostname, disp->port,
				 &disp->realhost, cfg, ADDRTYPE_UNSPEC);
    
	if ((err = sk_addr_error(disp->addr)) != NULL) {
	    sk_addr_free(disp->addr);
	    sfree(disp->hostname);
	    sfree(disp->unixsocketpath);
	    return NULL;	       /* FIXME: report an error */
	}
    }

    /*
     * Try upgrading an IP-style localhost display to a Unix-socket
     * display (as the standard X connection libraries do).
     */
    if (!disp->unixdomain && sk_address_is_local(disp->addr)) {
	SockAddr ux = platform_get_x11_unix_address(NULL, disp->displaynum);
	const char *err = sk_addr_error(ux);
	if (!err) {
	    /* Create trial connection to see if there is a useful Unix-domain
	     * socket */
	    const struct plug_function_table *dummy = &dummy_plug;
	    Socket s = sk_new(sk_addr_dup(ux), 0, 0, 0, 0, 0, (Plug)&dummy);
	    err = sk_socket_error(s);
	    sk_close(s);
	}
	if (err) {
	    sk_addr_free(ux);
	} else {
	    sk_addr_free(disp->addr);
	    disp->unixdomain = TRUE;
	    disp->addr = ux;
	    /* Fill in the rest in a moment */
	}
    }

    if (disp->unixdomain) {
	if (!disp->addr)
	    disp->addr = platform_get_x11_unix_address(disp->unixsocketpath,
						       disp->displaynum);
	if (disp->unixsocketpath)
	    disp->realhost = dupstr(disp->unixsocketpath);
	else
	    disp->realhost = dupprintf("unix:%d", disp->displaynum);
	disp->port = 0;
    }

    /*
     * Invent the remote authorisation details.
     */
    if (authtype == X11_MIT) {
	disp->remoteauthproto = X11_MIT;

	/* MIT-MAGIC-COOKIE-1. Cookie size is 128 bits (16 bytes). */
	disp->remoteauthdata = snewn(16, unsigned char);
	for (i = 0; i < 16; i++)
	    disp->remoteauthdata[i] = random_byte();
	disp->remoteauthdatalen = 16;

	disp->xdmseen = NULL;
    } else {
	assert(authtype == X11_XDM);
	disp->remoteauthproto = X11_XDM;

	/* XDM-AUTHORIZATION-1. Cookie size is 16 bytes; byte 8 is zero. */
	disp->remoteauthdata = snewn(16, unsigned char);
	for (i = 0; i < 16; i++)
	    disp->remoteauthdata[i] = (i == 8 ? 0 : random_byte());
	disp->remoteauthdatalen = 16;

	disp->xdmseen = newtree234(xdmseen_cmp);
    }
    disp->remoteauthprotoname = dupstr(x11_authnames[disp->remoteauthproto]);
    disp->remoteauthdatastring = snewn(disp->remoteauthdatalen * 2 + 1, char);
    for (i = 0; i < disp->remoteauthdatalen; i++)
	sprintf(disp->remoteauthdatastring + i*2, "%02x",
		disp->remoteauthdata[i]);

    /*
     * Fetch the local authorisation details.
     */
    disp->localauthproto = X11_NO_AUTH;
    disp->localauthdata = NULL;
    disp->localauthdatalen = 0;
    platform_get_x11_auth(disp, cfg);

    return disp;
}

void x11_free_display(struct X11Display *disp)
{
    if (disp->xdmseen != NULL) {
	struct XDMSeen *seen;
	while ((seen = delpos234(disp->xdmseen, 0)) != NULL)
	    sfree(seen);
	freetree234(disp->xdmseen);
    }
    sfree(disp->hostname);
    sfree(disp->unixsocketpath);
    if (disp->localauthdata)
	memset(disp->localauthdata, 0, disp->localauthdatalen);
    sfree(disp->localauthdata);
    if (disp->remoteauthdata)
	memset(disp->remoteauthdata, 0, disp->remoteauthdatalen);
    sfree(disp->remoteauthdata);
    sfree(disp->remoteauthprotoname);
    sfree(disp->remoteauthdatastring);
    sk_addr_free(disp->addr);
    sfree(disp);
}

#define XDM_MAXSKEW 20*60      /* 20 minute clock skew should be OK */

static char *x11_verify(unsigned long peer_ip, int peer_port,
			struct X11Display *disp, char *proto,
			unsigned char *data, int dlen)
{
    if (strcmp(proto, x11_authnames[disp->remoteauthproto]) != 0)
	return "wrong authorisation protocol attempted";
    if (disp->remoteauthproto == X11_MIT) {
        if (dlen != disp->remoteauthdatalen)
            return "MIT-MAGIC-COOKIE-1 data was wrong length";
        if (memcmp(disp->remoteauthdata, data, dlen) != 0)
            return "MIT-MAGIC-COOKIE-1 data did not match";
    }
    if (disp->remoteauthproto == X11_XDM) {
	unsigned long t;
	time_t tim;
	int i;
	struct XDMSeen *seen, *ret;

        if (dlen != 24)
            return "XDM-AUTHORIZATION-1 data was wrong length";
	if (peer_port == -1)
            return "cannot do XDM-AUTHORIZATION-1 without remote address data";
	des_decrypt_xdmauth(disp->remoteauthdata+9, data, 24);
        if (memcmp(disp->remoteauthdata, data, 8) != 0)
            return "XDM-AUTHORIZATION-1 data failed check"; /* cookie wrong */
	if (GET_32BIT_MSB_FIRST(data+8) != peer_ip)
            return "XDM-AUTHORIZATION-1 data failed check";   /* IP wrong */
	if ((int)GET_16BIT_MSB_FIRST(data+12) != peer_port)
            return "XDM-AUTHORIZATION-1 data failed check";   /* port wrong */
	t = GET_32BIT_MSB_FIRST(data+14);
	for (i = 18; i < 24; i++)
	    if (data[i] != 0)	       /* zero padding wrong */
		return "XDM-AUTHORIZATION-1 data failed check";
	tim = time(NULL);
	if (abs(t - tim) > XDM_MAXSKEW)
	    return "XDM-AUTHORIZATION-1 time stamp was too far out";
	seen = snew(struct XDMSeen);
	seen->time = t;
	memcpy(seen->clientid, data+8, 6);
	assert(disp->xdmseen != NULL);
	ret = add234(disp->xdmseen, seen);
	if (ret != seen) {
	    sfree(seen);
	    return "XDM-AUTHORIZATION-1 data replayed";
	}
	/* While we're here, purge entries too old to be replayed. */
	for (;;) {
	    seen = index234(disp->xdmseen, 0);
	    assert(seen != NULL);
	    if (t - seen->time <= XDM_MAXSKEW)
		break;
	    sfree(delpos234(disp->xdmseen, 0));
	}
    }
    /* implement other protocols here if ever required */
    return NULL;
}

void x11_get_auth_from_authfile(struct X11Display *disp,
				const char *authfilename)
{
    FILE *authfp;
    char *buf, *ptr, *str[4];
    int len[4];
    int family, protocol;
    int ideal_match = FALSE;
    char *ourhostname = get_hostname();

    /*
     * Normally we should look for precisely the details specified in
     * `disp'. However, there's an oddity when the display is local:
     * displays like "localhost:0" usually have their details stored
     * in a Unix-domain-socket record (even if there isn't actually a
     * real Unix-domain socket available, as with OpenSSH's proxy X11
     * server).
     *
     * This is apparently a fudge to get round the meaninglessness of
     * "localhost" in a shared-home-directory context -- xauth entries
     * for Unix-domain sockets already disambiguate this by storing
     * the *local* hostname in the conveniently-blank hostname field,
     * but IP "localhost" records couldn't do this. So, typically, an
     * IP "localhost" entry in the auth database isn't present and if
     * it were it would be ignored.
     *
     * However, we don't entirely trust that (say) Windows X servers
     * won't rely on a straight "localhost" entry, bad idea though
     * that is; so if we can't find a Unix-domain-socket entry we'll
     * fall back to an IP-based entry if we can find one.
     */
    int localhost = !disp->unixdomain && sk_address_is_local(disp->addr);

    authfp = fopen(authfilename, "rb");
    if (!authfp)
	return;

    /* Records in .Xauthority contain four strings of up to 64K each */
    buf = snewn(65537 * 4, char);

    while (!ideal_match) {
	int c, i, j, match = FALSE;
	
#define GET do { c = fgetc(authfp); if (c == EOF) goto done; c = (unsigned char)c; } while (0)
	/* Expect a big-endian 2-byte number giving address family */
	GET; family = c;
	GET; family = (family << 8) | c;
	/* Then expect four strings, each composed of a big-endian 2-byte
	 * length field followed by that many bytes of data */
	ptr = buf;
	for (i = 0; i < 4; i++) {
	    GET; len[i] = c;
	    GET; len[i] = (len[i] << 8) | c;
	    str[i] = ptr;
	    for (j = 0; j < len[i]; j++) {
		GET; *ptr++ = c;
	    }
	    *ptr++ = '\0';
	}
#undef GET

	/*
	 * Now we have a full X authority record in memory. See
	 * whether it matches the display we're trying to
	 * authenticate to.
	 *
	 * The details we've just read should be interpreted as
	 * follows:
	 * 
	 *  - 'family' is the network address family used to
	 *    connect to the display. 0 means IPv4; 6 means IPv6;
	 *    256 means Unix-domain sockets.
	 * 
	 *  - str[0] is the network address itself. For IPv4 and
	 *    IPv6, this is a string of binary data of the
	 *    appropriate length (respectively 4 and 16 bytes)
	 *    representing the address in big-endian format, e.g.
	 *    7F 00 00 01 means IPv4 localhost. For Unix-domain
	 *    sockets, this is the host name of the machine on
	 *    which the Unix-domain display resides (so that an
	 *    .Xauthority file on a shared file system can contain
	 *    authority entries for Unix-domain displays on
	 *    several machines without them clashing).
	 * 
	 *  - str[1] is the display number. I've no idea why
	 *    .Xauthority stores this as a string when it has a
	 *    perfectly good integer format, but there we go.
	 * 
	 *  - str[2] is the authorisation method, encoded as its
	 *    canonical string name (i.e. "MIT-MAGIC-COOKIE-1",
	 *    "XDM-AUTHORIZATION-1" or something we don't
	 *    recognise).
	 * 
	 *  - str[3] is the actual authorisation data, stored in
	 *    binary form.
	 */

	if (disp->displaynum < 0 || disp->displaynum != atoi(str[1]))
	    continue;		       /* not the one */

	for (protocol = 1; protocol < lenof(x11_authnames); protocol++)
	    if (!strcmp(str[2], x11_authnames[protocol]))
		break;
	if (protocol == lenof(x11_authnames))
	    continue;  /* don't recognise this protocol, look for another */

	switch (family) {
	  case 0:   /* IPv4 */
	    if (!disp->unixdomain &&
		sk_addrtype(disp->addr) == ADDRTYPE_IPV4) {
		char buf[4];
		sk_addrcopy(disp->addr, buf);
		if (len[0] == 4 && !memcmp(str[0], buf, 4)) {
		    match = TRUE;
		    /* If this is a "localhost" entry, note it down
		     * but carry on looking for a Unix-domain entry. */
		    ideal_match = !localhost;
		}
	    }
	    break;
	  case 6:   /* IPv6 */
	    if (!disp->unixdomain &&
		sk_addrtype(disp->addr) == ADDRTYPE_IPV6) {
		char buf[16];
		sk_addrcopy(disp->addr, buf);
		if (len[0] == 16 && !memcmp(str[0], buf, 16)) {
		    match = TRUE;
		    ideal_match = !localhost;
		}
	    }
	    break;
	  case 256: /* Unix-domain / localhost */
	    if ((disp->unixdomain || localhost)
	       	&& ourhostname && !strcmp(ourhostname, str[0]))
		/* A matching Unix-domain socket is always the best
		 * match. */
		match = ideal_match = TRUE;
	    break;
	}

	if (match) {
	    /* Current best guess -- may be overridden if !ideal_match */
	    disp->localauthproto = protocol;
	    sfree(disp->localauthdata); /* free previous guess, if any */
	    disp->localauthdata = snewn(len[3], unsigned char);
	    memcpy(disp->localauthdata, str[3], len[3]);
	    disp->localauthdatalen = len[3];
	}
    }

    done:
    fclose(authfp);
    memset(buf, 0, 65537 * 4);
    sfree(buf);
    sfree(ourhostname);
}

static void x11_log(Plug p, int type, SockAddr addr, int port,
		    const char *error_msg, int error_code)
{
    /* We have no interface to the logging module here, so we drop these. */
}

static int x11_closing(Plug plug, const char *error_msg, int error_code,
		       int calling_back)
{
    struct X11Private *pr = (struct X11Private *) plug;

    /*
     * We have no way to communicate down the forwarded connection,
     * so if an error occurred on the socket, we just ignore it
     * and treat it like a proper close.
     */
    sshfwd_close(pr->c);
    x11_close(pr->s);
    return 1;
}

static int x11_receive(Plug plug, int urgent, char *data, int len)
{
    struct X11Private *pr = (struct X11Private *) plug;

    if (sshfwd_write(pr->c, data, len) > 0) {
	pr->throttled = 1;
	sk_set_frozen(pr->s, 1);
    }

    return 1;
}

static void x11_sent(Plug plug, int bufsize)
{
    struct X11Private *pr = (struct X11Private *) plug;

    sshfwd_unthrottle(pr->c, bufsize);
}

/*
 * When setting up X forwarding, we should send the screen number
 * from the specified local display. This function extracts it from
 * the display string.
 */
int x11_get_screen_number(char *display)
{
    int n;

    n = strcspn(display, ":");
    if (!display[n])
	return 0;
    n = strcspn(display, ".");
    if (!display[n])
	return 0;
    return atoi(display + n + 1);
}

/*
 * Called to set up the raw connection.
 * 
 * Returns an error message, or NULL on success.
 * also, fills the SocketsStructure
 */
extern const char *x11_init(Socket *s, struct X11Display *disp, void *c,
			    const char *peeraddr, int peerport,
			    const Config *cfg)
{
    static const struct plug_function_table fn_table = {
	x11_log,
	x11_closing,
	x11_receive,
	x11_sent,
	NULL
    };

    const char *err;
    struct X11Private *pr;

    /*
     * Open socket.
     */
    pr = snew(struct X11Private);
    pr->fn = &fn_table;
    pr->auth_protocol = NULL;
    pr->disp = disp;
    pr->verified = 0;
    pr->data_read = 0;
    pr->throttled = pr->throttle_override = 0;
    pr->c = c;

    pr->s = *s = new_connection(sk_addr_dup(disp->addr),
				disp->realhost, disp->port,
				0, 1, 0, 0, (Plug) pr, cfg);
    if ((err = sk_socket_error(*s)) != NULL) {
	sfree(pr);
	return err;
    }

    /*
     * See if we can make sense of the peer address we were given.
     */
    {
	int i[4];
	if (peeraddr &&
	    4 == sscanf(peeraddr, "%d.%d.%d.%d", i+0, i+1, i+2, i+3)) {
	    pr->peer_ip = (i[0] << 24) | (i[1] << 16) | (i[2] << 8) | i[3];
	    pr->peer_port = peerport;
	} else {
	    pr->peer_ip = 0;
	    pr->peer_port = -1;
	}
    }

    sk_set_private_ptr(*s, pr);
    return NULL;
}

void x11_close(Socket s)
{
    struct X11Private *pr;
    if (!s)
	return;
    pr = (struct X11Private *) sk_get_private_ptr(s);
    if (pr->auth_protocol) {
	sfree(pr->auth_protocol);
	sfree(pr->auth_data);
    }

    sfree(pr);

    sk_close(s);
}

void x11_unthrottle(Socket s)
{
    struct X11Private *pr;
    if (!s)
	return;
    pr = (struct X11Private *) sk_get_private_ptr(s);

    pr->throttled = 0;
    sk_set_frozen(s, pr->throttled || pr->throttle_override);
}

void x11_override_throttle(Socket s, int enable)
{
    struct X11Private *pr;
    if (!s)
	return;
    pr = (struct X11Private *) sk_get_private_ptr(s);

    pr->throttle_override = enable;
    sk_set_frozen(s, pr->throttled || pr->throttle_override);
}

/*
 * Called to send data down the raw connection.
 */
int x11_send(Socket s, char *data, int len)
{
    struct X11Private *pr;
    if (!s)
	return 0;
    pr = (struct X11Private *) sk_get_private_ptr(s);

    /*
     * Read the first packet.
     */
    while (len > 0 && pr->data_read < 12)
	pr->firstpkt[pr->data_read++] = (unsigned char) (len--, *data++);
    if (pr->data_read < 12)
	return 0;

    /*
     * If we have not allocated the auth_protocol and auth_data
     * strings, do so now.
     */
    if (!pr->auth_protocol) {
	pr->auth_plen = GET_16BIT(pr->firstpkt[0], pr->firstpkt + 6);
	pr->auth_dlen = GET_16BIT(pr->firstpkt[0], pr->firstpkt + 8);
	pr->auth_psize = (pr->auth_plen + 3) & ~3;
	pr->auth_dsize = (pr->auth_dlen + 3) & ~3;
	/* Leave room for a terminating zero, to make our lives easier. */
	pr->auth_protocol = snewn(pr->auth_psize + 1, char);
	pr->auth_data = snewn(pr->auth_dsize, unsigned char);
    }

    /*
     * Read the auth_protocol and auth_data strings.
     */
    while (len > 0 && pr->data_read < 12 + pr->auth_psize)
	pr->auth_protocol[pr->data_read++ - 12] = (len--, *data++);
    while (len > 0 && pr->data_read < 12 + pr->auth_psize + pr->auth_dsize)
	pr->auth_data[pr->data_read++ - 12 -
		      pr->auth_psize] = (unsigned char) (len--, *data++);
    if (pr->data_read < 12 + pr->auth_psize + pr->auth_dsize)
	return 0;

    /*
     * If we haven't verified the authorisation, do so now.
     */
    if (!pr->verified) {
	char *err;

	pr->auth_protocol[pr->auth_plen] = '\0';	/* ASCIZ */
	err = x11_verify(pr->peer_ip, pr->peer_port,
			 pr->disp, pr->auth_protocol,
			 pr->auth_data, pr->auth_dlen);

	/*
	 * If authorisation failed, construct and send an error
	 * packet, then terminate the connection.
	 */
	if (err) {
	    char *message;
	    int msglen, msgsize;
	    unsigned char *reply;

	    message = dupprintf("%s X11 proxy: %s", appname, err);
	    msglen = strlen(message);
	    reply = snewn(8 + msglen+1 + 4, unsigned char); /* include zero */
	    msgsize = (msglen + 3) & ~3;
	    reply[0] = 0;	       /* failure */
	    reply[1] = msglen;	       /* length of reason string */
	    memcpy(reply + 2, pr->firstpkt + 2, 4);	/* major/minor proto vsn */
	    PUT_16BIT(pr->firstpkt[0], reply + 6, msgsize >> 2);/* data len */
	    memset(reply + 8, 0, msgsize);
	    memcpy(reply + 8, message, msglen);
	    sshfwd_write(pr->c, (char *)reply, 8 + msgsize);
	    sshfwd_close(pr->c);
	    x11_close(s);
	    sfree(reply);
	    sfree(message);
	    return 0;
	}

	/*
	 * Now we know we're going to accept the connection. Strip
	 * the fake auth data, and optionally put real auth data in
	 * instead.
	 */
        {
            char realauthdata[64];
            int realauthlen = 0;
            int authstrlen = strlen(x11_authnames[pr->disp->localauthproto]);
	    int buflen = 0;	       /* initialise to placate optimiser */
            static const char zeroes[4] = { 0,0,0,0 };
	    void *buf;

            if (pr->disp->localauthproto == X11_MIT) {
                assert(pr->disp->localauthdatalen <= lenof(realauthdata));
                realauthlen = pr->disp->localauthdatalen;
                memcpy(realauthdata, pr->disp->localauthdata, realauthlen);
            } else if (pr->disp->localauthproto == X11_XDM &&
		       pr->disp->localauthdatalen == 16 &&
		       ((buf = sk_getxdmdata(s, &buflen))!=0)) {
		time_t t;
                realauthlen = (buflen+12+7) & ~7;
		assert(realauthlen <= lenof(realauthdata));
		memset(realauthdata, 0, realauthlen);
		memcpy(realauthdata, pr->disp->localauthdata, 8);
		memcpy(realauthdata+8, buf, buflen);
		t = time(NULL);
		PUT_32BIT_MSB_FIRST(realauthdata+8+buflen, t);
		des_encrypt_xdmauth(pr->disp->localauthdata+9,
				    (unsigned char *)realauthdata,
				    realauthlen);
		sfree(buf);
	    }
            /* implement other auth methods here if required */

            PUT_16BIT(pr->firstpkt[0], pr->firstpkt + 6, authstrlen);
            PUT_16BIT(pr->firstpkt[0], pr->firstpkt + 8, realauthlen);
        
            sk_write(s, (char *)pr->firstpkt, 12);

            if (authstrlen) {
                sk_write(s, x11_authnames[pr->disp->localauthproto],
			 authstrlen);
                sk_write(s, zeroes, 3 & (-authstrlen));
            }
            if (realauthlen) {
                sk_write(s, realauthdata, realauthlen);
                sk_write(s, zeroes, 3 & (-realauthlen));
            }
        }
	pr->verified = 1;
    }

    /*
     * After initialisation, just copy data simply.
     */

    return sk_write(s, data, len);
}
