#include <stdio.h>
#include <stdlib.h>

#include "putty.h"
#include "ssh.h"

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#define GET_32BIT_LSB_FIRST(cp) \
  (((unsigned long)(unsigned char)(cp)[0]) | \
  ((unsigned long)(unsigned char)(cp)[1] << 8) | \
  ((unsigned long)(unsigned char)(cp)[2] << 16) | \
  ((unsigned long)(unsigned char)(cp)[3] << 24))

#define PUT_32BIT_LSB_FIRST(cp, value) ( \
  (cp)[0] = (value), \
  (cp)[1] = (value) >> 8, \
  (cp)[2] = (value) >> 16, \
  (cp)[3] = (value) >> 24 )

#define GET_16BIT_LSB_FIRST(cp) \
  (((unsigned long)(unsigned char)(cp)[0]) | \
  ((unsigned long)(unsigned char)(cp)[1] << 8))

#define PUT_16BIT_LSB_FIRST(cp, value) ( \
  (cp)[0] = (value), \
  (cp)[1] = (value) >> 8 )

#define GET_32BIT_MSB_FIRST(cp) \
  (((unsigned long)(unsigned char)(cp)[0] << 24) | \
  ((unsigned long)(unsigned char)(cp)[1] << 16) | \
  ((unsigned long)(unsigned char)(cp)[2] << 8) | \
  ((unsigned long)(unsigned char)(cp)[3]))

#define PUT_32BIT_MSB_FIRST(cp, value) ( \
  (cp)[0] = (value) >> 24, \
  (cp)[1] = (value) >> 16, \
  (cp)[2] = (value) >> 8, \
  (cp)[3] = (value) )

#define GET_16BIT_MSB_FIRST(cp) \
  (((unsigned long)(unsigned char)(cp)[0] << 8) | \
  ((unsigned long)(unsigned char)(cp)[1]))

#define PUT_16BIT_MSB_FIRST(cp, value) ( \
  (cp)[0] = (value) >> 8, \
  (cp)[1] = (value) )

struct PFwdPrivate {
    const struct plug_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */
    void *c;			       /* (channel) data used by ssh.c */
    void *backhandle;		       /* instance of SSH backend itself */
    /* Note that backhandle need not be filled in if c is non-NULL */
    Socket s;
    int throttled, throttle_override;
    int ready;
    /*
     * `dynamic' does double duty. It's set to 0 for an ordinary
     * forwarded port, and nonzero for SOCKS-style dynamic port
     * forwarding; but it also represents the state of the SOCKS
     * exchange.
     */
    int dynamic;
    /*
     * `hostname' and `port' are the real hostname and port, once
     * we know what we're connecting to; they're unused for this
     * purpose while conducting a local SOCKS exchange, which means
     * we can also use them as a buffer and pointer for reading
     * data from the SOCKS client.
     */
    char hostname[256+8];
    int port;
    /*
     * When doing dynamic port forwarding, we can receive
     * connection data before we are actually able to send it; so
     * we may have to temporarily hold some in a dynamically
     * allocated buffer here.
     */
    void *buffer;
    int buflen;
};

static int pfd_closing(Plug plug, const char *error_msg, int error_code,
		       int calling_back)
{
    struct PFwdPrivate *pr = (struct PFwdPrivate *) plug;

    /*
     * We have no way to communicate down the forwarded connection,
     * so if an error occurred on the socket, we just ignore it
     * and treat it like a proper close.
     */
    if (pr->c)
	sshfwd_close(pr->c);
    pfd_close(pr->s);
    return 1;
}

static int pfd_receive(Plug plug, int urgent, char *data, int len)
{
    struct PFwdPrivate *pr = (struct PFwdPrivate *) plug;
    if (pr->dynamic) {
	while (len--) {
	    /*
	     * Throughout SOCKS negotiation, "hostname" is re-used as a
	     * random protocol buffer with "port" storing the length.
	     */ 
	    if (pr->port >= lenof(pr->hostname)) {
		/* Request too long. */
		if ((pr->dynamic >> 12) == 4) {
		    /* Send back a SOCKS 4 error before closing. */
		    char data[8];
		    memset(data, 0, sizeof(data));
		    data[1] = 91;      /* generic `request rejected' */
		    sk_write(pr->s, data, 8);
		}
		pfd_close(pr->s);
		return 1;
	    }
	    pr->hostname[pr->port++] = *data++;

	    /*
	     * Now check what's in the buffer to see if it's a
	     * valid and complete message in the SOCKS exchange.
	     */
	    if ((pr->dynamic == 1 || (pr->dynamic >> 12) == 4) &&
		pr->hostname[0] == 4) {
		/*
		 * SOCKS 4.
		 */
		if (pr->dynamic == 1)
		    pr->dynamic = 0x4000;
		if (pr->port < 2) continue;/* don't have command code yet */
		if (pr->hostname[1] != 1) {
		    /* Not CONNECT. */
		    /* Send back a SOCKS 4 error before closing. */
		    char data[8];
		    memset(data, 0, sizeof(data));
		    data[1] = 91;      /* generic `request rejected' */
		    sk_write(pr->s, data, 8);
		    pfd_close(pr->s);
		    return 1;
		}
		if (pr->port <= 8) continue; /* haven't started user/hostname */
		if (pr->hostname[pr->port-1] != 0)
		    continue;	       /* haven't _finished_ user/hostname */
		/*
		 * Now we have a full SOCKS 4 request. Check it to
		 * see if it's a SOCKS 4A request.
		 */
		if (pr->hostname[4] == 0 && pr->hostname[5] == 0 &&
		    pr->hostname[6] == 0 && pr->hostname[7] != 0) {
		    /*
		     * It's SOCKS 4A. So if we haven't yet
		     * collected the host name, we should continue
		     * waiting for data in order to do so; if we
		     * have, we can go ahead.
		     */
		    int len;
		    if (pr->dynamic == 0x4000) {
			pr->dynamic = 0x4001;
			pr->port = 8;      /* reset buffer to overwrite name */
			continue;
		    }
		    pr->hostname[0] = 0;   /* reply version code */
		    pr->hostname[1] = 90;   /* request granted */
		    sk_write(pr->s, pr->hostname, 8);
		    len= pr->port - 8;
		    pr->port = GET_16BIT_MSB_FIRST(pr->hostname+2);
		    memmove(pr->hostname, pr->hostname + 8, len);
		    goto connect;
		} else {
		    /*
		     * It's SOCKS 4, which means we should format
		     * the IP address into the hostname string and
		     * then just go.
		     */
		    pr->hostname[0] = 0;   /* reply version code */
		    pr->hostname[1] = 90;   /* request granted */
		    sk_write(pr->s, pr->hostname, 8);
		    pr->port = GET_16BIT_MSB_FIRST(pr->hostname+2);
		    sprintf(pr->hostname, "%d.%d.%d.%d",
			    (unsigned char)pr->hostname[4],
			    (unsigned char)pr->hostname[5],
			    (unsigned char)pr->hostname[6],
			    (unsigned char)pr->hostname[7]);
		    goto connect;
		}
	    }

	    if ((pr->dynamic == 1 || (pr->dynamic >> 12) == 5) &&
		pr->hostname[0] == 5) {
		/*
		 * SOCKS 5.
		 */
		if (pr->dynamic == 1)
		    pr->dynamic = 0x5000;

		if (pr->dynamic == 0x5000) {
		    int i, method;
		    char data[2];
		    /*
		     * We're receiving a set of method identifiers.
		     */
		    if (pr->port < 2) continue;/* no method count yet */
		    if (pr->port < 2 + (unsigned char)pr->hostname[1])
			continue;      /* no methods yet */
		    method = 0xFF;     /* invalid */
		    for (i = 0; i < (unsigned char)pr->hostname[1]; i++)
			if (pr->hostname[2+i] == 0) {
			    method = 0;/* no auth */
			    break;
			}
		    data[0] = 5;
		    data[1] = method;
		    sk_write(pr->s, data, 2);
		    pr->dynamic = 0x5001;
		    pr->port = 0;      /* re-empty the buffer */
		    continue;
		}

		if (pr->dynamic == 0x5001) {
		    /*
		     * We're receiving a SOCKS request.
		     */
		    unsigned char reply[10]; /* SOCKS5 atyp=1 reply */
		    int atype, alen = 0;

		    /*
		     * Pre-fill reply packet.
		     * In all cases, we set BND.{HOST,ADDR} to 0.0.0.0:0
		     * (atyp=1) in the reply; if we succeed, we don't know
		     * the right answers, and if we fail, they should be
		     * ignored.
		     */
		    memset(reply, 0, lenof(reply));
		    reply[0] = 5; /* VER */
		    reply[3] = 1; /* ATYP = 1 (IPv4, 0.0.0.0:0) */

		    if (pr->port < 6) continue;
		    atype = (unsigned char)pr->hostname[3];
		    if (atype == 1)    /* IPv4 address */
			alen = 4;
		    if (atype == 4)    /* IPv6 address */
			alen = 16;
		    if (atype == 3)    /* domain name has leading length */
			alen = 1 + (unsigned char)pr->hostname[4];
		    if (pr->port < 6 + alen) continue;
		    if (pr->hostname[1] != 1 || pr->hostname[2] != 0) {
			/* Not CONNECT or reserved field nonzero - error */
			reply[1] = 1;	/* generic failure */
			sk_write(pr->s, (char *) reply, lenof(reply));
			pfd_close(pr->s);
			return 1;
		    }
		    /*
		     * Now we have a viable connect request. Switch
		     * on atype.
		     */
		    pr->port = GET_16BIT_MSB_FIRST(pr->hostname+4+alen);
		    if (atype == 1) {
			/* REP=0 (success) already */
			sk_write(pr->s, (char *) reply, lenof(reply));
			sprintf(pr->hostname, "%d.%d.%d.%d",
				(unsigned char)pr->hostname[4],
				(unsigned char)pr->hostname[5],
				(unsigned char)pr->hostname[6],
				(unsigned char)pr->hostname[7]);
			goto connect;
		    } else if (atype == 3) {
			/* REP=0 (success) already */
			sk_write(pr->s, (char *) reply, lenof(reply));
			memmove(pr->hostname, pr->hostname + 5, alen-1);
			pr->hostname[alen-1] = '\0';
			goto connect;
		    } else {
			/*
			 * Unknown address type. (FIXME: support IPv6!)
			 */
			reply[1] = 8;	/* atype not supported */
			sk_write(pr->s, (char *) reply, lenof(reply));
			pfd_close(pr->s);
			return 1;
		    }
		}
	    }

	    /*
	     * If we get here without either having done `continue'
	     * or `goto connect', it must be because there is no
	     * sensible interpretation of what's in our buffer. So
	     * close the connection rudely.
	     */
	    pfd_close(pr->s);
	    return 1;
	}
	return 1;

	/*
	 * We come here when we're ready to make an actual
	 * connection.
	 */
	connect:

	pr->c = new_sock_channel(pr->backhandle, pr->s);
	if (pr->c == NULL) {
	    pfd_close(pr->s);
	    return 1;
	} else {
	    /* asks to forward to the specified host/port for this */
	    ssh_send_port_open(pr->c, pr->hostname, pr->port, "forwarding");
	}
	pr->dynamic = 0;

	/*
	 * Now freeze the socket until the SSH server confirms the
	 * connection.
	 */
	sk_set_frozen(pr->s, 1);
	/*
	 * If there's any data remaining in our current buffer,
	 * save it to be sent on pfd_confirm().
	 */
	if (len > 0) {
	    pr->buffer = snewn(len, char);
	    memcpy(pr->buffer, data, len);
	    pr->buflen = len;
	}
    }
    if (pr->ready) {
	if (sshfwd_write(pr->c, data, len) > 0) {
	    pr->throttled = 1;
	    sk_set_frozen(pr->s, 1);
	}
    }
    return 1;
}

static void pfd_sent(Plug plug, int bufsize)
{
    struct PFwdPrivate *pr = (struct PFwdPrivate *) plug;

    if (pr->c)
	sshfwd_unthrottle(pr->c, bufsize);
}

/*
 * Called when receiving a PORT OPEN from the server
 */
const char *pfd_newconnect(Socket *s, char *hostname, int port,
			   void *c, const Config *cfg)
{
    static const struct plug_function_table fn_table = {
	pfd_closing,
	pfd_receive,
	pfd_sent,
	NULL
    };

    SockAddr addr;
    const char *err;
    char *dummy_realhost;
    struct PFwdPrivate *pr;

    /*
     * Try to find host.
     */
    addr = name_lookup(hostname, port, &dummy_realhost, cfg);
    if ((err = sk_addr_error(addr)) != NULL) {
	sk_addr_free(addr);
	return err;
    }

    /*
     * Open socket.
     */
    pr = snew(struct PFwdPrivate);
    pr->buffer = NULL;
    pr->fn = &fn_table;
    pr->throttled = pr->throttle_override = 0;
    pr->ready = 1;
    pr->c = c;
    pr->backhandle = NULL;	       /* we shouldn't need this */
    pr->dynamic = 0;

    pr->s = *s = new_connection(addr, dummy_realhost, port,
				0, 1, 0, 0, (Plug) pr, cfg);
    if ((err = sk_socket_error(*s)) != NULL) {
	sfree(pr);
	return err;
    }

    sk_set_private_ptr(*s, pr);
    return NULL;
}

/*
 called when someone connects to the local port
 */

static int pfd_accepting(Plug p, OSSocket sock)
{
    static const struct plug_function_table fn_table = {
	pfd_closing,
	pfd_receive,
	pfd_sent,
	NULL
    };
    struct PFwdPrivate *pr, *org;
    Socket s;
    const char *err;

    org = (struct PFwdPrivate *)p;
    pr = snew(struct PFwdPrivate);
    pr->buffer = NULL;
    pr->fn = &fn_table;

    pr->c = NULL;
    pr->backhandle = org->backhandle;

    pr->s = s = sk_register(sock, (Plug) pr);
    if ((err = sk_socket_error(s)) != NULL) {
	sfree(pr);
	return err != NULL;
    }

    sk_set_private_ptr(s, pr);

    pr->throttled = pr->throttle_override = 0;
    pr->ready = 0;

    if (org->dynamic) {
	pr->dynamic = 1;
	pr->port = 0;		       /* "hostname" buffer is so far empty */
	sk_set_frozen(s, 0);	       /* we want to receive SOCKS _now_! */
    } else {
	pr->dynamic = 0;
	strcpy(pr->hostname, org->hostname);
	pr->port = org->port;	
	pr->c = new_sock_channel(org->backhandle, s);

	if (pr->c == NULL) {
	    sfree(pr);
	    return 1;
	} else {
	    /* asks to forward to the specified host/port for this */
	    ssh_send_port_open(pr->c, pr->hostname, pr->port, "forwarding");
	}
    }

    return 0;
}


/* Add a new forwarding from port -> desthost:destport
 sets up a listener on the local machine on (srcaddr:)port
 */
const char *pfd_addforward(char *desthost, int destport, char *srcaddr,
			   int port, void *backhandle, const Config *cfg)
{
    static const struct plug_function_table fn_table = {
	pfd_closing,
	pfd_receive,		       /* should not happen... */
	pfd_sent,		       /* also should not happen */
	pfd_accepting
    };

    const char *err;
    struct PFwdPrivate *pr;
    Socket s;

    /*
     * Open socket.
     */
    pr = snew(struct PFwdPrivate);
    pr->buffer = NULL;
    pr->fn = &fn_table;
    pr->c = NULL;
    if (desthost) {
	strcpy(pr->hostname, desthost);
	pr->port = destport;
	pr->dynamic = 0;
    } else
	pr->dynamic = 1;
    pr->throttled = pr->throttle_override = 0;
    pr->ready = 0;
    pr->backhandle = backhandle;

    pr->s = s = new_listener(srcaddr, port, (Plug) pr,
			     !cfg->lport_acceptall, cfg);
    if ((err = sk_socket_error(s)) != NULL) {
	sfree(pr);
	return err;
    }

    sk_set_private_ptr(s, pr);

    return NULL;
}

void pfd_close(Socket s)
{
    struct PFwdPrivate *pr;

    if (!s)
	return;

    pr = (struct PFwdPrivate *) sk_get_private_ptr(s);

    sfree(pr->buffer);
    sfree(pr);

    sk_close(s);
}

void pfd_unthrottle(Socket s)
{
    struct PFwdPrivate *pr;
    if (!s)
	return;
    pr = (struct PFwdPrivate *) sk_get_private_ptr(s);

    pr->throttled = 0;
    sk_set_frozen(s, pr->throttled || pr->throttle_override);
}

void pfd_override_throttle(Socket s, int enable)
{
    struct PFwdPrivate *pr;
    if (!s)
	return;
    pr = (struct PFwdPrivate *) sk_get_private_ptr(s);

    pr->throttle_override = enable;
    sk_set_frozen(s, pr->throttled || pr->throttle_override);
}

/*
 * Called to send data down the raw connection.
 */
int pfd_send(Socket s, char *data, int len)
{
    if (s == NULL)
	return 0;
    return sk_write(s, data, len);
}


void pfd_confirm(Socket s)
{
    struct PFwdPrivate *pr;

    if (s == NULL)
	return;

    pr = (struct PFwdPrivate *) sk_get_private_ptr(s);
    pr->ready = 1;
    sk_set_frozen(s, 0);
    sk_write(s, NULL, 0);
    if (pr->buffer) {
	sshfwd_write(pr->c, pr->buffer, pr->buflen);
	sfree(pr->buffer);
	pr->buffer = NULL;
    }
}
