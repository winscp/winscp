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

struct PortForwarding {
    const struct plug_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */
    struct ssh_channel *c;        /* channel structure held by ssh.c */
    void *backhandle;		       /* instance of SSH backend itself */
    /* Note that backhandle need not be filled in if c is non-NULL */
    Socket s;
    int throttled, throttle_override;
    int ready;
    /*
     * `dynamic' does double duty. It's set to 0 for an ordinary
     * forwarded port, and nonzero for SOCKS-style dynamic port
     * forwarding; but the nonzero values are also a state machine
     * tracking where the SOCKS exchange has got to.
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

struct PortListener {
    const struct plug_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */
    void *backhandle;		       /* instance of SSH backend itself */
    Socket s;
    /*
     * `dynamic' is set to 0 for an ordinary forwarded port, and
     * nonzero for SOCKS-style dynamic port forwarding.
     */
    int dynamic;
    /*
     * `hostname' and `port' are the real hostname and port, for
     * ordinary forwardings.
     */
    char *hostname;
    int port;
};

static struct PortForwarding *new_portfwd_state(void)
{
    struct PortForwarding *pf = snew(struct PortForwarding);
    pf->hostname = NULL;
    pf->socksbuf = NULL;
    pf->sockslen = pf->sockssize = 0;
    pf->buffer = NULL;
    return pf;
}

static void free_portfwd_state(struct PortForwarding *pf)
{
    if (!pf)
        return;
    sfree(pf->hostname);
    sfree(pf->socksbuf);
    sfree(pf->buffer);
    sfree(pf);
}

static struct PortListener *new_portlistener_state(void)
{
    struct PortListener *pl = snew(struct PortListener);
    pl->hostname = NULL;
    return pl;
}

static void free_portlistener_state(struct PortListener *pl)
{
    if (!pl)
        return;
    sfree(pl->hostname);
    sfree(pl);
}

static void pfd_log(Plug plug, int type, SockAddr addr, int port,
		    const char *error_msg, int error_code)
{
    /* we have to dump these since we have no interface to logging.c */
}

static void pfl_log(Plug plug, int type, SockAddr addr, int port,
		    const char *error_msg, int error_code)
{
    /* we have to dump these since we have no interface to logging.c */
}

static int pfd_closing(Plug plug, const char *error_msg, int error_code,
		       int calling_back)
{
    struct PortForwarding *pf = (struct PortForwarding *) plug;

    if (error_msg) {
        /*
         * Socket error. Slam the connection instantly shut.
         */
        if (pf->c) {
            sshfwd_unclean_close(pf->c, error_msg);
        } else {
            /*
             * We might not have an SSH channel, if a socket error
             * occurred during SOCKS negotiation. If not, we must
             * clean ourself up without sshfwd_unclean_close's call
             * back to pfd_close.
             */
            pfd_close(pf);
        }
    } else {
        /*
         * Ordinary EOF received on socket. Send an EOF on the SSH
         * channel.
         */
        if (pf->c)
            sshfwd_write_eof(pf->c);
    }

    return 1;
}

static int pfl_closing(Plug plug, const char *error_msg, int error_code,
		       int calling_back)
{
    struct PortListener *pl = (struct PortListener *) plug;
    pfl_terminate(pl);
    return 1;
}

static void wrap_send_port_open(void *channel, char *hostname, int port,
                                Socket s)
{
    char *peerinfo, *description;
    peerinfo = sk_peer_info(s);
    if (peerinfo) {
        description = dupprintf("forwarding from %s", peerinfo);
        sfree(peerinfo);
    } else {
        description = dupstr("forwarding");
    }
    ssh_send_port_open(channel, hostname, port, description);
    sfree(description);
}

static int pfd_receive(Plug plug, int urgent, char *data, int len)
{
    struct PortForwarding *pf = (struct PortForwarding *) plug;
    if (pf->dynamic) {
	while (len--) {
	    if (pf->sockslen >= pf->sockssize) {
                pf->sockssize = pf->sockslen * 5 / 4 + 256;
                pf->socksbuf = sresize(pf->socksbuf, pf->sockssize, char);
	    }
	    pf->socksbuf[pf->sockslen++] = *data++;

	    /*
	     * Now check what's in the buffer to see if it's a
	     * valid and complete message in the SOCKS exchange.
	     */
	    if ((pf->dynamic == 1 || (pf->dynamic >> 12) == 4) &&
		pf->socksbuf[0] == 4) {
		/*
		 * SOCKS 4.
		 */
		if (pf->dynamic == 1)
		    pf->dynamic = 0x4000;
		if (pf->sockslen < 2)
                    continue;        /* don't have command code yet */
		if (pf->socksbuf[1] != 1) {
		    /* Not CONNECT. */
		    /* Send back a SOCKS 4 error before closing. */
		    char data[8];
		    memset(data, 0, sizeof(data));
		    data[1] = 91;      /* generic `request rejected' */
		    sk_write(pf->s, data, 8);
		    pfd_close(pf);
		    return 1;
		}
		if (pf->sockslen <= 8)
                    continue;      /* haven't started user/hostname */
		if (pf->socksbuf[pf->sockslen-1] != 0)
		    continue;	       /* haven't _finished_ user/hostname */
		/*
		 * Now we have a full SOCKS 4 request. Check it to
		 * see if it's a SOCKS 4A request.
		 */
		if (pf->socksbuf[4] == 0 && pf->socksbuf[5] == 0 &&
		    pf->socksbuf[6] == 0 && pf->socksbuf[7] != 0) {
		    /*
		     * It's SOCKS 4A. So if we haven't yet
		     * collected the host name, we should continue
		     * waiting for data in order to do so; if we
		     * have, we can go ahead.
		     */
		    int len;
		    if (pf->dynamic == 0x4000) {
			pf->dynamic = 0x4001;
			pf->sockslen = 8; /* reset buffer to overwrite name */
			continue;
		    }
		    pf->socksbuf[0] = 0;   /* reply version code */
		    pf->socksbuf[1] = 90;   /* request granted */
		    sk_write(pf->s, pf->socksbuf, 8);
		    len = pf->sockslen - 8;
		    pf->port = GET_16BIT_MSB_FIRST(pf->socksbuf+2);
                    pf->hostname = snewn(len+1, char);
                    pf->hostname[len] = '\0';
		    memcpy(pf->hostname, pf->socksbuf + 8, len);
		    goto connect;
		} else {
		    /*
		     * It's SOCKS 4, which means we should format
		     * the IP address into the hostname string and
		     * then just go.
		     */
		    pf->socksbuf[0] = 0;   /* reply version code */
		    pf->socksbuf[1] = 90;   /* request granted */
		    sk_write(pf->s, pf->socksbuf, 8);
		    pf->port = GET_16BIT_MSB_FIRST(pf->socksbuf+2);
		    pf->hostname = dupprintf("%d.%d.%d.%d",
                                             (unsigned char)pf->socksbuf[4],
                                             (unsigned char)pf->socksbuf[5],
                                             (unsigned char)pf->socksbuf[6],
                                             (unsigned char)pf->socksbuf[7]);
		    goto connect;
		}
	    }

	    if ((pf->dynamic == 1 || (pf->dynamic >> 12) == 5) &&
		pf->socksbuf[0] == 5) {
		/*
		 * SOCKS 5.
		 */
		if (pf->dynamic == 1)
		    pf->dynamic = 0x5000;

		if (pf->dynamic == 0x5000) {
		    int i, method;
		    char data[2];
		    /*
		     * We're receiving a set of method identifiers.
		     */
		    if (pf->sockslen < 2)
                        continue;      /* no method count yet */
		    if (pf->sockslen < 2 + (unsigned char)pf->socksbuf[1])
			continue;      /* no methods yet */
		    method = 0xFF;     /* invalid */
		    for (i = 0; i < (unsigned char)pf->socksbuf[1]; i++)
			if (pf->socksbuf[2+i] == 0) {
			    method = 0;/* no auth */
			    break;
			}
		    data[0] = 5;
		    data[1] = method;
		    sk_write(pf->s, data, 2);
		    pf->dynamic = 0x5001;
		    pf->sockslen = 0;      /* re-empty the buffer */
		    continue;
		}

		if (pf->dynamic == 0x5001) {
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

		    if (pf->sockslen < 6) continue;
		    atype = (unsigned char)pf->socksbuf[3];
		    if (atype == 1)    /* IPv4 address */
			alen = 4;
		    if (atype == 4)    /* IPv6 address */
			alen = 16;
		    if (atype == 3)    /* domain name has leading length */
			alen = 1 + (unsigned char)pf->socksbuf[4];
		    if (pf->sockslen < 6 + alen) continue;
		    if (pf->socksbuf[1] != 1 || pf->socksbuf[2] != 0) {
			/* Not CONNECT or reserved field nonzero - error */
			reply[1] = 1;	/* generic failure */
			sk_write(pf->s, (char *) reply, lenof(reply));
			pfd_close(pf);
			return 1;
		    }
		    /*
		     * Now we have a viable connect request. Switch
		     * on atype.
		     */
		    pf->port = GET_16BIT_MSB_FIRST(pf->socksbuf+4+alen);
		    if (atype == 1) {
			/* REP=0 (success) already */
			sk_write(pf->s, (char *) reply, lenof(reply));
			pf->hostname = dupprintf("%d.%d.%d.%d",
                                                 (unsigned char)pf->socksbuf[4],
                                                 (unsigned char)pf->socksbuf[5],
                                                 (unsigned char)pf->socksbuf[6],
                                                 (unsigned char)pf->socksbuf[7]);
			goto connect;
		    } else if (atype == 3) {
			/* REP=0 (success) already */
			sk_write(pf->s, (char *) reply, lenof(reply));
                        pf->hostname = snewn(alen, char);
			pf->hostname[alen-1] = '\0';
			memcpy(pf->hostname, pf->socksbuf + 5, alen-1);
			goto connect;
		    } else {
			/*
			 * Unknown address type. (FIXME: support IPv6!)
			 */
			reply[1] = 8;	/* atype not supported */
			sk_write(pf->s, (char *) reply, lenof(reply));
			pfd_close(pf);
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
	    pfd_close(pf);
	    return 1;
	}
	return 1;

	/*
	 * We come here when we're ready to make an actual
	 * connection.
	 */
	connect:
        sfree(pf->socksbuf);
        pf->socksbuf = NULL;

	/*
	 * Freeze the socket until the SSH server confirms the
	 * connection.
	 */
	sk_set_frozen(pf->s, 1);

	pf->c = new_sock_channel(pf->backhandle, pf);
	if (pf->c == NULL) {
	    pfd_close(pf);
	    return 1;
	} else {
	    /* asks to forward to the specified host/port for this */
	    wrap_send_port_open(pf->c, pf->hostname, pf->port, pf->s);
	}
	pf->dynamic = 0;

	/*
	 * If there's any data remaining in our current buffer,
	 * save it to be sent on pfd_confirm().
	 */
	if (len > 0) {
	    pf->buffer = snewn(len, char);
	    memcpy(pf->buffer, data, len);
	    pf->buflen = len;
	}
    }
    if (pf->ready) {
	if (sshfwd_write(pf->c, data, len) > 0) {
	    pf->throttled = 1;
	    sk_set_frozen(pf->s, 1);
	}
    }
    return 1;
}

static void pfd_sent(Plug plug, int bufsize)
{
    struct PortForwarding *pf = (struct PortForwarding *) plug;

    if (pf->c)
	sshfwd_unthrottle(pf->c, bufsize);
}

/*
 * Called when receiving a PORT OPEN from the server to make a
 * connection to a destination host.
 *
 * On success, returns NULL and fills in *pf_ret. On error, returns a
 * dynamically allocated error message string.
 */
char *pfd_connect(struct PortForwarding **pf_ret, char *hostname,int port,
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
    struct PortForwarding *pf;

    /*
     * Try to find host.
     */
    addr = name_lookup(hostname, port, &dummy_realhost, conf, addressfamily);
    if ((err = sk_addr_error(addr)) != NULL) {
        char *err_ret = dupstr(err);
	sk_addr_free(addr);
        sfree(dummy_realhost);
	return err_ret;
    }

    /*
     * Open socket.
     */
    pf = *pf_ret = new_portfwd_state();
    pf->fn = &fn_table;
    pf->throttled = pf->throttle_override = 0;
    pf->ready = 1;
    pf->c = c;
    pf->backhandle = NULL;	       /* we shouldn't need this */
    pf->dynamic = 0;

    pf->s = new_connection(addr, dummy_realhost, port,
                           0, 1, 0, 0, (Plug) pf, conf);
    sfree(dummy_realhost);
    if ((err = sk_socket_error(pf->s)) != NULL) {
        char *err_ret = dupstr(err);
        sk_close(pf->s);
	free_portfwd_state(pf);
        *pf_ret = NULL;
	return err_ret;
    }

    return NULL;
}

/*
 called when someone connects to the local port
 */

static int pfl_accepting(Plug p, accept_fn_t constructor, accept_ctx_t ctx)
{
    static const struct plug_function_table fn_table = {
	pfd_log,
	pfd_closing,
	pfd_receive,
	pfd_sent,
	NULL
    };
    struct PortForwarding *pf;
    struct PortListener *pl;
    Socket s;
    const char *err;

    pl = (struct PortListener *)p;
    pf = new_portfwd_state();
    pf->fn = &fn_table;

    pf->c = NULL;
    pf->backhandle = pl->backhandle;

    pf->s = s = constructor(ctx, (Plug) pf);
    if ((err = sk_socket_error(s)) != NULL) {
	free_portfwd_state(pf);
	return err != NULL;
    }

    pf->throttled = pf->throttle_override = 0;
    pf->ready = 0;

    if (pl->dynamic) {
	pf->dynamic = 1;
	pf->port = 0;		       /* "hostname" buffer is so far empty */
	sk_set_frozen(s, 0);	       /* we want to receive SOCKS _now_! */
    } else {
	pf->dynamic = 0;
	pf->hostname = dupstr(pl->hostname);
	pf->port = pl->port;	
	pf->c = new_sock_channel(pl->backhandle, pf);

	if (pf->c == NULL) {
	    free_portfwd_state(pf);
	    return 1;
	} else {
	    /* asks to forward to the specified host/port for this */
	    wrap_send_port_open(pf->c, pf->hostname, pf->port, s);
	}
    }

    return 0;
}


/*
 * Add a new port-forwarding listener from srcaddr:port -> desthost:destport.
 *
 * On success, returns NULL and fills in *pl_ret. On error, returns a
 * dynamically allocated error message string.
 */
char *pfl_listen(char *desthost, int destport, char *srcaddr,
                 int port, void *backhandle, Conf *conf,
                 struct PortListener **pl_ret, int address_family)
{
    static const struct plug_function_table fn_table = {
	pfl_log,
	pfl_closing,
        NULL,                          /* recv */
        NULL,                          /* send */
	pfl_accepting
    };

    const char *err;
    struct PortListener *pl;

    /*
     * Open socket.
     */
    pl = *pl_ret = new_portlistener_state();
    pl->fn = &fn_table;
    if (desthost) {
	pl->hostname = dupstr(desthost);
	pl->port = destport;
	pl->dynamic = 0;
    } else
	pl->dynamic = 1;
    pl->backhandle = backhandle;

    pl->s = new_listener(srcaddr, port, (Plug) pl,
                         !conf_get_int(conf, CONF_lport_acceptall),
                         conf, address_family);
    if ((err = sk_socket_error(pl->s)) != NULL) {
        char *err_ret = dupstr(err);
        sk_close(pl->s);
	free_portlistener_state(pl);
        *pl_ret = NULL;
	return err_ret;
    }

    return NULL;
}

void pfd_close(struct PortForwarding *pf)
{
    if (!pf)
	return;

    sk_close(pf->s);
    free_portfwd_state(pf);
}

/*
 * Terminate a listener.
 */
void pfl_terminate(struct PortListener *pl)
{
    if (!pl)
	return;

    sk_close(pl->s);
    free_portlistener_state(pl);
}

void pfd_unthrottle(struct PortForwarding *pf)
{
    if (!pf)
	return;

    pf->throttled = 0;
    sk_set_frozen(pf->s, pf->throttled || pf->throttle_override);
}

void pfd_override_throttle(struct PortForwarding *pf, int enable)
{
    if (!pf)
	return;

    pf->throttle_override = enable;
    sk_set_frozen(pf->s, pf->throttled || pf->throttle_override);
}

/*
 * Called to send data down the raw connection.
 */
int pfd_send(struct PortForwarding *pf, char *data, int len)
{
    if (pf == NULL)
	return 0;
    return sk_write(pf->s, data, len);
}

void pfd_send_eof(struct PortForwarding *pf)
{
    sk_write_eof(pf->s);
}

void pfd_confirm(struct PortForwarding *pf)
{
    if (pf == NULL)
	return;

    pf->ready = 1;
    sk_set_frozen(pf->s, 0);
    sk_write(pf->s, NULL, 0);
    if (pf->buffer) {
	sshfwd_write(pf->c, pf->buffer, pf->buflen);
	sfree(pf->buffer);
	pf->buffer = NULL;
    }
}
