/*
 * SSH port forwarding.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "putty.h"
#include "ssh.h"

/*
 * Enumeration of values that live in the 'socks_state' field of
 * struct PortForwarding.
 */
typedef enum {
    SOCKS_NONE, /* direct connection (no SOCKS, or SOCKS already done) */
    SOCKS_INITIAL,       /* don't know if we're SOCKS 4 or 5 yet */
    SOCKS_4,             /* expect a SOCKS 4 (or 4A) connection message */
    SOCKS_5_INITIAL,     /* expect a SOCKS 5 preliminary message */
    SOCKS_5_CONNECT      /* expect a SOCKS 5 connection message */
} SocksState;

struct PortForwarding {
    struct ssh_channel *c;        /* channel structure held by ssh.c */
    void *backhandle;		       /* instance of SSH backend itself */
    /* Note that backhandle need not be filled in if c is non-NULL */
    Socket s;
    int throttled, throttle_override;
    int ready;
    SocksState socks_state;
    /*
     * `hostname' and `port' are the real hostname and port, once
     * we know what we're connecting to.
     */
    char *hostname;
    int port;
    /*
     * `socksbuf' is the buffer we use to accumulate the initial SOCKS
     * segment of the incoming data, plus anything after that that we
     * receive before we're ready to send data to the SSH server.
     */
    strbuf *socksbuf;
    size_t socksbuf_consumed;

    const Plug_vtable *plugvt;
};

struct PortListener {
    void *backhandle;		       /* instance of SSH backend itself */
    Socket s;
    int is_dynamic;
    /*
     * `hostname' and `port' are the real hostname and port, for
     * ordinary forwardings.
     */
    char *hostname;
    int port;

    const Plug_vtable *plugvt;
};

static struct PortForwarding *new_portfwd_state(void)
{
    struct PortForwarding *pf = snew(struct PortForwarding);
    pf->hostname = NULL;
    pf->socksbuf = NULL;
    return pf;
}

static void free_portfwd_state(struct PortForwarding *pf)
{
    if (!pf)
        return;
    sfree(pf->hostname);
    if (pf->socksbuf)
        strbuf_free(pf->socksbuf);
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

static void pfd_closing(Plug plug, const char *error_msg, int error_code,
			int calling_back)
{
    struct PortForwarding *pf = FROMFIELD(plug, struct PortForwarding, plugvt);

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
}

static void pfl_closing(Plug plug, const char *error_msg, int error_code,
			int calling_back)
{
    struct PortListener *pl = (struct PortListener *) plug;
    pfl_terminate(pl);
}

static void wrap_send_port_open(void *channel, const char *hostname, int port,
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

static char *ipv4_to_string(unsigned ipv4)
{
    return dupprintf("%u.%u.%u.%u",
                     (ipv4 >> 24) & 0xFF, (ipv4 >> 16) & 0xFF,
                     (ipv4 >>  8) & 0xFF, (ipv4      ) & 0xFF);
}

static char *ipv6_to_string(ptrlen ipv6)
{
    const unsigned char *addr = ipv6.ptr;
    assert(ipv6.len == 16);
    return dupprintf("%04x:%04x:%04x:%04x:%04x:%04x:%04x:%04x",
                     (unsigned)GET_16BIT_MSB_FIRST(addr + 0),
                     (unsigned)GET_16BIT_MSB_FIRST(addr + 2),
                     (unsigned)GET_16BIT_MSB_FIRST(addr + 4),
                     (unsigned)GET_16BIT_MSB_FIRST(addr + 6),
                     (unsigned)GET_16BIT_MSB_FIRST(addr + 8),
                     (unsigned)GET_16BIT_MSB_FIRST(addr + 10),
                     (unsigned)GET_16BIT_MSB_FIRST(addr + 12),
                     (unsigned)GET_16BIT_MSB_FIRST(addr + 14));
}

static void pfd_receive(Plug plug, int urgent, char *data, int len)
{
    struct PortForwarding *pf = FROMFIELD(plug, struct PortForwarding, plugvt);

    if (len == 0)
        return;

    if (pf->socks_state != SOCKS_NONE) {
        BinarySource src[1];

        /*
         * Store all the data we've got in socksbuf.
         */
        put_data(pf->socksbuf, data, len);

        /*
         * Check the start of socksbuf to see if it's a valid and
         * complete message in the SOCKS exchange.
         */

        if (pf->socks_state == SOCKS_INITIAL) {
            /* Preliminary: check the first byte of the data (which we
             * _must_ have by now) to find out which SOCKS major
             * version we're speaking. */
            switch (pf->socksbuf->u[0]) {
              case 4:
                pf->socks_state = SOCKS_4;
                break;
              case 5:
                pf->socks_state = SOCKS_5_INITIAL;
                break;
              default:
                pfd_close(pf);         /* unrecognised version */
                return;
            }
        }

        BinarySource_BARE_INIT(src, pf->socksbuf->u, pf->socksbuf->len);
        get_data(src, pf->socksbuf_consumed);

        while (pf->socks_state != SOCKS_NONE) {
            unsigned socks_version, message_type, reserved_byte;
            unsigned reply_code, port, ipv4, method;
            ptrlen methods;
            const char *socks4_hostname;
            strbuf *output;

            switch (pf->socks_state) {
              case SOCKS_INITIAL:
              case SOCKS_NONE:
                assert(0 && "These case values cannot appear");

              case SOCKS_4:
                /* SOCKS 4/4A connect message */
                socks_version = get_byte(src);
                message_type = get_byte(src);

                if (get_err(src) == BSE_OUT_OF_DATA)
                    return;
                if (socks_version == 4 && message_type == 1) {
                    /* CONNECT message */
                    int name_based = FALSE;

                    port = get_uint16(src);
                    ipv4 = get_uint32(src);
                    if (ipv4 > 0x00000000 && ipv4 < 0x00000100) {
                        /*
                         * Addresses in this range indicate the SOCKS 4A
                         * extension to specify a hostname, which comes
                         * after the username.
                         */
                        name_based = TRUE;
                    }
                    get_asciz(src);        /* skip username */
                    socks4_hostname = name_based ? get_asciz(src) : NULL;

                    if (get_err(src) == BSE_OUT_OF_DATA)
                        return;
                    if (get_err(src))
                        goto socks4_reject;

                    pf->port = port;
                    if (name_based) {
                        pf->hostname = dupstr(socks4_hostname);
                    } else {
                        pf->hostname = ipv4_to_string(ipv4);
                    }

                    output = strbuf_new();
                    put_byte(output, 0);       /* reply version */
                    put_byte(output, 90);      /* SOCKS 4 'request granted' */
                    put_uint16(output, 0);     /* null port field */
                    put_uint32(output, 0);     /* null address field */
                    sk_write(pf->s, output->u, output->len);
                    strbuf_free(output);

                    pf->socks_state = SOCKS_NONE;
                    pf->socksbuf_consumed = src->pos;
                    break;
                }

              socks4_reject:
                output = strbuf_new();
                put_byte(output, 0);       /* reply version */
                put_byte(output, 91);      /* SOCKS 4 'request rejected' */
                put_uint16(output, 0);     /* null port field */
                put_uint32(output, 0);     /* null address field */
                sk_write(pf->s, output->u, output->len);
                strbuf_free(output);
                pfd_close(pf);
                return;

              case SOCKS_5_INITIAL:
                /* SOCKS 5 initial method list */
                socks_version = get_byte(src);
                methods = get_pstring(src);

                method = 0xFF;         /* means 'no usable method found' */
                {
                    int i;
                    for (i = 0; i < methods.len; i++) {
                        if (((const unsigned char *)methods.ptr)[i] == 0 ) {
                            method = 0;        /* no auth */
                            break;
                        }
                    }
                }

                if (get_err(src) == BSE_OUT_OF_DATA)
                    return;
                if (get_err(src))
                    method = 0xFF;

                output = strbuf_new();
                put_byte(output, 5);       /* SOCKS version */
                put_byte(output, method);  /* selected auth method */
                sk_write(pf->s, output->u, output->len);
                strbuf_free(output);

                if (method == 0xFF) {
                    pfd_close(pf);
                    return;
                }

                pf->socks_state = SOCKS_5_CONNECT;
                pf->socksbuf_consumed = src->pos;
                break;

              case SOCKS_5_CONNECT:
                /* SOCKS 5 connect message */
                socks_version = get_byte(src);
                message_type = get_byte(src);
                reserved_byte = get_byte(src);

                if (socks_version == 5 && message_type == 1 &&
                    reserved_byte == 0) {

                    reply_code = 0;        /* success */

                    switch (get_byte(src)) {
                      case 1:              /* IPv4 */
                        pf->hostname = ipv4_to_string(get_uint32(src));
                        break;
                      case 4:              /* IPv6 */
                        pf->hostname = ipv6_to_string(get_data(src, 16));
                        break;
                      case 3:              /* unresolved domain name */
                        pf->hostname = mkstr(get_pstring(src));
                        break;
                      default:
                        pf->hostname = NULL;
                        reply_code = 8;    /* address type not supported */
                        break;
                    }

                    pf->port = get_uint16(src);
                } else {
                    reply_code = 7;        /* command not supported */
                }

                if (get_err(src) == BSE_OUT_OF_DATA)
                    return;
                if (get_err(src))
                    reply_code = 1;        /* general server failure */

                output = strbuf_new();
                put_byte(output, 5);       /* SOCKS version */
                put_byte(output, reply_code);
                put_byte(output, 0);       /* reserved */
                put_byte(output, 1);       /* IPv4 address follows */
                put_uint32(output, 0);     /* bound IPv4 address (unused) */
                put_uint16(output, 0);     /* bound port number (unused) */
                sk_write(pf->s, output->u, output->len);
                strbuf_free(output);

                if (reply_code != 0) {
                    pfd_close(pf);
                    return;
                }

                pf->socks_state = SOCKS_NONE;
                pf->socksbuf_consumed = src->pos;
                break;
            }
        }

	/*
	 * We come here when we're ready to make an actual
	 * connection.
	 */

	/*
	 * Freeze the socket until the SSH server confirms the
	 * connection.
	 */
	sk_set_frozen(pf->s, 1);

	pf->c = new_sock_channel(pf->backhandle, pf);
	if (pf->c == NULL) {
	    pfd_close(pf);
	    return;
	} else {
	    /* asks to forward to the specified host/port for this */
	    wrap_send_port_open(pf->c, pf->hostname, pf->port, pf->s);
	}
    }
    if (pf->ready) {
	if (sshfwd_write(pf->c, data, len) > 0) {
	    pf->throttled = 1;
	    sk_set_frozen(pf->s, 1);
	}
    }
}

static void pfd_sent(Plug plug, int bufsize)
{
    struct PortForwarding *pf = FROMFIELD(plug, struct PortForwarding, plugvt);

    if (pf->c)
	sshfwd_unthrottle(pf->c, bufsize);
}

static const Plug_vtable PortForwarding_plugvt = {
    pfd_log,
    pfd_closing,
    pfd_receive,
    pfd_sent,
    NULL
};

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
    SockAddr addr;
    const char *err;
    char *dummy_realhost = NULL;
    struct PortForwarding *pf;

    /*
     * Try to find host.
     */
    addr = name_lookup(hostname, port, &dummy_realhost, conf, addressfamily,
                       NULL, NULL);
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
    pf->plugvt = &PortForwarding_plugvt;
    pf->throttled = pf->throttle_override = 0;
    pf->ready = 1;
    pf->c = c;
    pf->backhandle = NULL;	       /* we shouldn't need this */
    pf->socks_state = SOCKS_NONE;

    pf->s = new_connection(addr, dummy_realhost, port,
                           0, 1, 0, 0, &pf->plugvt, conf);
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
    struct PortForwarding *pf;
    struct PortListener *pl;
    Socket s;
    const char *err;

    pl = FROMFIELD(p, struct PortListener, plugvt);
    pf = new_portfwd_state();
    pf->plugvt = &PortForwarding_plugvt;

    pf->c = NULL;
    pf->backhandle = pl->backhandle;

    pf->s = s = constructor(ctx, &pf->plugvt);
    if ((err = sk_socket_error(s)) != NULL) {
	free_portfwd_state(pf);
	return err != NULL;
    }

    pf->throttled = pf->throttle_override = 0;
    pf->ready = 0;

    if (pl->is_dynamic) {
	pf->socks_state = SOCKS_INITIAL;
        pf->socksbuf = strbuf_new();
        pf->socksbuf_consumed = 0;
	pf->port = 0;		       /* "hostname" buffer is so far empty */
	sk_set_frozen(s, 0);	       /* we want to receive SOCKS _now_! */
    } else {
	pf->socks_state = SOCKS_NONE;
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

static const Plug_vtable PortListener_plugvt = {
    pfl_log,
    pfl_closing,
    NULL,                          /* recv */
    NULL,                          /* send */
    pfl_accepting
};

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
    const char *err;
    struct PortListener *pl;

    /*
     * Open socket.
     */
    pl = *pl_ret = new_portlistener_state();
    pl->plugvt = &PortListener_plugvt;
    if (desthost) {
	pl->hostname = dupstr(desthost);
	pl->port = destport;
	pl->is_dynamic = FALSE;
    } else
	pl->is_dynamic = TRUE;
    pl->backhandle = backhandle;

    pl->s = new_listener(srcaddr, port, &pl->plugvt,
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
int pfd_send(struct PortForwarding *pf, const void *data, int len)
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
    if (pf->socksbuf) {
	sshfwd_write(pf->c, pf->socksbuf->u + pf->socksbuf_consumed,
                     pf->socksbuf->len - pf->socksbuf_consumed);
        strbuf_free(pf->socksbuf);
        pf->socksbuf = NULL;
    }
}
