/*
 * SSH port forwarding.
 */

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
     * we know what we're connecting to.
     */
    char *hostname;
    int port;
    /*
     * `socksbuf' is the buffer we use to accumulate a SOCKS request.
     */
    char *socksbuf;
    int sockslen, sockssize;
    /*
     * When doing dynamic port forwarding, we can receive
     * connection data before we are actually able to send it; so
     * we may have to temporarily hold some in a dynamically
     * allocated buffer here.
     */
    void *buffer;
    int buflen;
};

static struct PFwdPrivate *new_portfwd_private(void)
{
    struct PFwdPrivate *pr = snew(struct PFwdPrivate);
    pr->hostname = NULL;
    pr->socksbuf = NULL;
    pr->sockslen = pr->sockssize = 0;
    pr->buffer = NULL;
    return pr;
}

static void free_portfwd_private(struct PFwdPrivate *pr)
{
    if (!pr)
        return;
    sfree(pr->hostname);
    sfree(pr->socksbuf);
    sfree(pr->buffer);
    sfree(pr);
}

static void pfd_log(Plug plug, int type, SockAddr addr, int port,
		    const char *error_msg, int error_code)
{
    /* we have to dump these since we have no interface to logging.c */
}

static int pfd_closing(Plug plug, const char *error_msg, int error_code,
		       int calling_back)
{
    struct PFwdPrivate *pr = (struct PFwdPrivate *) plug;

    if (error_msg) {
        /*
         * Socket error. Slam the connection instantly shut.
         */
        if (pr->c) {
            sshfwd_unclean_close(pr->c, error_msg);
    } else {
        /*
             * We might not have an SSH channel, if a socket error
             * occurred during SOCKS negotiation. If not, we must
             * clean ourself up without sshfwd_unclean_close's call
             * back to pfd_close.
             */
            pfd_close(pr->s);
        }
    } else {
        /*
         * Ordinary EOF received on socket. Send an EOF on the SSH
         * channel.
         */
        if (pr->c)
            sshfwd_write_eof(pr->c);
    }

    return 1;
}

static int pfd_receive(Plug plug, int urgent, char *data, int len)
{
    struct PFwdPrivate *pr = (struct PFwdPrivate *) plug;
    if (pr->dynamic) {
	while (len--) {
	    if (pr->sockslen >= pr->sockssize) {
                pr->sockssize = pr->sockslen * 5 / 4 + 256;
                pr->socksbuf = sresize(pr->socksbuf, pr->sockssize, char);
	    }
	    pr->socksbuf[pr->sockslen++] = *data++;

	    /*
	     * Now check what's in the buffer to see if it's a
	     * valid and complete message in the SOCKS exchange.
	     */
	    if ((pr->dynamic == 1 || (pr->dynamic >> 12) == 4) &&
		pr->socksbuf[0] == 4) {
		/*
		 * SOCKS 4.
		 */
		if (pr->dynamic == 1)
		    pr->dynamic = 0x4000;
		if (pr->sockslen < 2)
                    continue;        /* don't have command code yet */
		if (pr->socksbuf[1] != 1) {
		    /* Not CONNECT. */
		    /* Send back a SOCKS 4 error before closing. */
		    char data[8];
		    memset(data, 0, sizeof(data));
		    data[1] = 91;      /* generic `request rejected' */
		    sk_write(pr->s, data, 8);
		    pfd_close(pr->s);
		    return 1;
		}
		if (pr->sockslen <= 8)
                    continue;      /* haven't started user/hostname */
		if (pr->socksbuf[pr->sockslen-1] != 0)
		    continue;	       /* haven't _finished_ user/hostname */
		/*
		 * Now we have a full SOCKS 4 request. Check it to
		 * see if it's a SOCKS 4A request.
		 */
		if (pr->socksbuf[4] == 0 && pr->socksbuf[5] == 0 &&
		    pr->socksbuf[6] == 0 && pr->socksbuf[7] != 0) {
		    /*
		     * It's SOCKS 4A. So if we haven't yet
		     * collected the host name, we should continue
		     * waiting for data in order to do so; if we
		     * have, we can go ahead.
		     */
		    int len;
		    if (pr->dynamic == 0x4000) {
			pr->dynamic = 0x4001;
			pr->sockslen = 8; /* reset buffer to overwrite name */
			continue;
		    }
		    pr->socksbuf[0] = 0;   /* reply version code */
		    pr->socksbuf[1] = 90;   /* request granted */
		    sk_write(pr->s, pr->socksbuf, 8);
		    len = pr->sockslen - 8;
		    pr->port = GET_16BIT_MSB_FIRST(pr->socksbuf+2);
                    pr->hostname = snewn(len+1, char);
                    pr->hostname[len] = '\0';
		    memcpy(pr->hostname, pr->socksbuf + 8, len);
		    goto connect;
		} else {
		    /*
		     * It's SOCKS 4, which means we should format
		     * the IP address into the hostname string and
		     * then just go.
		     */
		    pr->socksbuf[0] = 0;   /* reply version code */
		    pr->socksbuf[1] = 90;   /* request granted */
		    sk_write(pr->s, pr->socksbuf, 8);
		    pr->port = GET_16BIT_MSB_FIRST(pr->socksbuf+2);
		    pr->hostname = dupprintf("%d.%d.%d.%d",
                                             (unsigned char)pr->socksbuf[4],
                                             (unsigned char)pr->socksbuf[5],
                                             (unsigned char)pr->socksbuf[6],
                                             (unsigned char)pr->socksbuf[7]);
		    goto connect;
		}
	    }

	    if ((pr->dynamic == 1 || (pr->dynamic >> 12) == 5) &&
		pr->socksbuf[0] == 5) {
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
		    if (pr->sockslen < 2)
                        continue;      /* no method count yet */
		    if (pr->sockslen < 2 + (unsigned char)pr->socksbuf[1])
			continue;      /* no methods yet */
		    method = 0xFF;     /* invalid */
		    for (i = 0; i < (unsigned char)pr->socksbuf[1]; i++)
			if (pr->socksbuf[2+i] == 0) {
			    method = 0;/* no auth */
			    break;
			}
		    data[0] = 5;
		    data[1] = method;
		    sk_write(pr->s, data, 2);
		    pr->dynamic = 0x5001;
		    pr->sockslen = 0;      /* re-empty the buffer */
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

		    if (pr->sockslen < 6) continue;
		    atype = (unsigned char)pr->socksbuf[3];
		    if (atype == 1)    /* IPv4 address */
			alen = 4;
		    if (atype == 4)    /* IPv6 address */
			alen = 16;
		    if (atype == 3)    /* domain name has leading length */
			alen = 1 + (unsigned char)pr->socksbuf[4];
		    if (pr->sockslen < 6 + alen) continue;
		    if (pr->socksbuf[1] != 1 || pr->socksbuf[2] != 0) {
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
		    pr->port = GET_16BIT_MSB_FIRST(pr->socksbuf+4+alen);
		    if (atype == 1) {
			/* REP=0 (success) already */
			sk_write(pr->s, (char *) reply, lenof(reply));
			pr->hostname = dupprintf("%d.%d.%d.%d",
                                                 (unsigned char)pr->socksbuf[4],
                                                 (unsigned char)pr->socksbuf[5],
                                                 (unsigned char)pr->socksbuf[6],
                                                 (unsigned char)pr->socksbuf[7]);
			goto connect;
		    } else if (atype == 3) {
			/* REP=0 (success) already */
			sk_write(pr->s, (char *) reply, lenof(reply));
                        pr->hostname = snewn(alen, char);
			pr->hostname[alen-1] = '\0';
			memcpy(pr->hostname, pr->socksbuf + 5, alen-1);
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
        sfree(pr->socksbuf);
        pr->socksbuf = NULL;

	/*
	 * Freeze the socket until the SSH server confirms the
	 * connection.
	 */
	sk_set_frozen(pr->s, 1);

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
			   void *c, Conf *conf, int addressfamily)
{
    static const struct plug_function_table fn_table = {
	pfd_log,
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
    addr = name_lookup(hostname, port, &dummy_realhost, conf, addressfamily);
    if ((err = sk_addr_error(addr)) != NULL) {
	sk_addr_free(addr);
        sfree(dummy_realhost);
	return err;
    }

    /*
     * Open socket.
     */
    pr = new_portfwd_private();
    pr->fn = &fn_table;
    pr->throttled = pr->throttle_override = 0;
    pr->ready = 1;
    pr->c = c;
    pr->backhandle = NULL;	       /* we shouldn't need this */
    pr->dynamic = 0;

    pr->s = *s = new_connection(addr, dummy_realhost, port,
				0, 1, 0, 0, (Plug) pr, conf);
    sfree(dummy_realhost);
    if ((err = sk_socket_error(*s)) != NULL) {
	free_portfwd_private(pr);
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
	pfd_log,
	pfd_closing,
	pfd_receive,
	pfd_sent,
	NULL
    };
    struct PFwdPrivate *pr, *org;
    Socket s;
    const char *err;

    org = (struct PFwdPrivate *)p;
    pr = new_portfwd_private();
    pr->fn = &fn_table;

    pr->c = NULL;
    pr->backhandle = org->backhandle;

    pr->s = s = sk_register(sock, (Plug) pr);
    if ((err = sk_socket_error(s)) != NULL) {
	free_portfwd_private(pr);
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
	pr->hostname = dupstr(org->hostname);
	pr->port = org->port;	
	pr->c = new_sock_channel(org->backhandle, s);

	if (pr->c == NULL) {
	    free_portfwd_private(pr);
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
			   int port, void *backhandle, Conf *conf,
			   void **sockdata, int address_family)
{
    static const struct plug_function_table fn_table = {
	pfd_log,
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
    pr = new_portfwd_private();
    pr->fn = &fn_table;
    pr->c = NULL;
    if (desthost) {
	pr->hostname = dupstr(desthost);
	pr->port = destport;
	pr->dynamic = 0;
    } else
	pr->dynamic = 1;
    pr->throttled = pr->throttle_override = 0;
    pr->ready = 0;
    pr->backhandle = backhandle;

    pr->s = s = new_listener(srcaddr, port, (Plug) pr,
			     !conf_get_int(conf, CONF_lport_acceptall),
			     conf, address_family);
    if ((err = sk_socket_error(s)) != NULL) {
	free_portfwd_private(pr);
	return err;
    }

    sk_set_private_ptr(s, pr);

    *sockdata = (void *)s;

    return NULL;
}

void pfd_close(Socket s)
{
    struct PFwdPrivate *pr;

    if (!s)
	return;

    pr = (struct PFwdPrivate *) sk_get_private_ptr(s);

#ifdef MPEXT
    // make sure do_select is called before we loose the private members
    sk_close(s);
#endif

    free_portfwd_private(pr);

#ifndef MPEXT
    sk_close(s);
#endif
}

/*
 * Terminate a listener.
 */
void pfd_terminate(void *sv)
{
    pfd_close((Socket)sv);
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

void pfd_send_eof(Socket s)
{
    sk_write_eof(s);
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
