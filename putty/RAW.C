#include <stdio.h>
#include <stdlib.h>

#include "putty.h"

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#define RAW_MAX_BACKLOG 4096

typedef struct raw_backend_data {
    const struct plug_function_table *fn;
    /* the above field _must_ be first in the structure */

    Socket s;
    int bufsize;
    void *frontend;
} *Raw;

static void raw_size(void *handle, int width, int height);

static void c_write(Raw raw, char *buf, int len)
{
    int backlog = from_backend(raw->frontend, 0, buf, len);
    sk_set_frozen(raw->s, backlog > RAW_MAX_BACKLOG);
}

static int raw_closing(Plug plug, const char *error_msg, int error_code,
		       int calling_back)
{
    Raw raw = (Raw) plug;

    if (raw->s) {
        sk_close(raw->s);
        raw->s = NULL;
    }
    if (error_msg) {
	/* A socket error has occurred. */
	logevent(raw->frontend, error_msg);
	connection_fatal(raw->frontend, "%s", error_msg);
    }				       /* Otherwise, the remote side closed the connection normally. */
    return 0;
}

static int raw_receive(Plug plug, int urgent, char *data, int len)
{
    Raw raw = (Raw) plug;
    c_write(raw, data, len);
    return 1;
}

static void raw_sent(Plug plug, int bufsize)
{
    Raw raw = (Raw) plug;
    raw->bufsize = bufsize;
}

/*
 * Called to set up the raw connection.
 * 
 * Returns an error message, or NULL on success.
 *
 * Also places the canonical host name into `realhost'. It must be
 * freed by the caller.
 */
static const char *raw_init(void *frontend_handle, void **backend_handle,
			    Config *cfg,
			    char *host, int port, char **realhost, int nodelay)
{
    static const struct plug_function_table fn_table = {
	raw_closing,
	raw_receive,
	raw_sent
    };
    SockAddr addr;
    const char *err;
    Raw raw;

    raw = snew(struct raw_backend_data);
    raw->fn = &fn_table;
    raw->s = NULL;
    *backend_handle = raw;

    raw->frontend = frontend_handle;

    /*
     * Try to find host.
     */
    {
	char *buf;
	buf = dupprintf("Looking up host \"%s\"", host);
	logevent(raw->frontend, buf);
	sfree(buf);
    }
    addr = name_lookup(host, port, realhost, cfg);
    if ((err = sk_addr_error(addr)) != NULL) {
	sk_addr_free(addr);
	return err;
    }

    if (port < 0)
	port = 23;		       /* default telnet port */

    /*
     * Open socket.
     */
    {
	char *buf, addrbuf[100];
	sk_getaddr(addr, addrbuf, 100);
	buf = dupprintf("Connecting to %s port %d", addrbuf, port);
	logevent(raw->frontend, buf);
	sfree(buf);
    }
    raw->s = new_connection(addr, *realhost, port, 0, 1, nodelay,
			    (Plug) raw, cfg);
    if ((err = sk_socket_error(raw->s)) != NULL)
	return err;

    return NULL;
}

static void raw_free(void *handle)
{
    Raw raw = (Raw) handle;

    if (raw->s)
	sk_close(raw->s);
    sfree(raw);
}

/*
 * Stub routine (we don't have any need to reconfigure this backend).
 */
static void raw_reconfig(void *handle, Config *cfg)
{
}

/*
 * Called to send data down the raw connection.
 */
static int raw_send(void *handle, char *buf, int len)
{
    Raw raw = (Raw) handle;

    if (raw->s == NULL)
	return 0;

    raw->bufsize = sk_write(raw->s, buf, len);

    return raw->bufsize;
}

/*
 * Called to query the current socket sendability status.
 */
static int raw_sendbuffer(void *handle)
{
    Raw raw = (Raw) handle;
    return raw->bufsize;
}

/*
 * Called to set the size of the window
 */
static void raw_size(void *handle, int width, int height)
{
    /* Do nothing! */
    return;
}

/*
 * Send raw special codes.
 */
static void raw_special(void *handle, Telnet_Special code)
{
    /* Do nothing! */
    return;
}

/*
 * Return a list of the special codes that make sense in this
 * protocol.
 */
static const struct telnet_special *raw_get_specials(void *handle)
{
    return NULL;
}

static Socket raw_socket(void *handle)
{
    Raw raw = (Raw) handle;
    return raw->s;
}

static int raw_sendok(void *handle)
{
    return 1;
}

static void raw_unthrottle(void *handle, int backlog)
{
    Raw raw = (Raw) handle;
    sk_set_frozen(raw->s, backlog > RAW_MAX_BACKLOG);
}

static int raw_ldisc(void *handle, int option)
{
    if (option == LD_EDIT || option == LD_ECHO)
	return 1;
    return 0;
}

static void raw_provide_ldisc(void *handle, void *ldisc)
{
    /* This is a stub. */
}

static void raw_provide_logctx(void *handle, void *logctx)
{
    /* This is a stub. */
}

static int raw_exitcode(void *handle)
{
    Raw raw = (Raw) handle;
    if (raw->s != NULL)
        return -1;                     /* still connected */
    else
        /* Exit codes are a meaningless concept in the Raw protocol */
        return 0;
}

Backend raw_backend = {
    raw_init,
    raw_free,
    raw_reconfig,
    raw_send,
    raw_sendbuffer,
    raw_size,
    raw_special,
    raw_get_specials,
    raw_socket,
    raw_exitcode,
    raw_sendok,
    raw_ldisc,
    raw_provide_ldisc,
    raw_provide_logctx,
    raw_unthrottle,
    1
};
