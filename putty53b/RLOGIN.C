#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "putty.h"

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#define RLOGIN_MAX_BACKLOG 4096

static Socket s = NULL;
static int rlogin_bufsize;

static void rlogin_size(void);

static void c_write(char *buf, int len)
{
    int backlog = from_backend(0, buf, len);
    sk_set_frozen(s, backlog > RLOGIN_MAX_BACKLOG);
}

static int rlogin_closing(Plug plug, char *error_msg, int error_code,
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

static int rlogin_receive(Plug plug, int urgent, char *data, int len)
{
    if (urgent == 2) {
	char c;

	c = *data++;
	len--;
	if (c == '\x80')
	    rlogin_size();
	/*
	 * We should flush everything (aka Telnet SYNCH) if we see
	 * 0x02, and we should turn off and on _local_ flow control
	 * on 0x10 and 0x20 respectively. I'm not convinced it's
	 * worth it...
	 */
    } else {
	/*
	 * Main rlogin protocol. This is really simple: the first
	 * byte is expected to be NULL and is ignored, and the rest
	 * is printed.
	 */
	static int firstbyte = 1;
	if (firstbyte) {
	    if (data[0] == '\0') {
		data++;
		len--;
	    }
	    firstbyte = 0;
	}
	if (len > 0)
            c_write(data, len);
    }
    return 1;
}

static void rlogin_sent(Plug plug, int bufsize)
{
    rlogin_bufsize = bufsize;
}

/*
 * Called to set up the rlogin connection.
 * 
 * Returns an error message, or NULL on success.
 *
 * Also places the canonical host name into `realhost'. It must be
 * freed by the caller.
 */
static char *rlogin_init(char *host, int port, char **realhost, int nodelay)
{
    static struct plug_function_table fn_table = {
	rlogin_closing,
	rlogin_receive,
	rlogin_sent
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
	port = 513;		       /* default rlogin port */

    /*
     * Open socket.
     */
    {
	char buf[200], addrbuf[100];
	sk_getaddr(addr, addrbuf, 100);
	sprintf(buf, "Connecting to %.100s port %d", addrbuf, port);
	logevent(buf);
    }
    s = new_connection(addr, *realhost, port, 1, 0, nodelay, &fn_table_ptr);
    if ((err = sk_socket_error(s)))
	return err;

    sk_addr_free(addr);

    /*
     * Send local username, remote username, terminal/speed
     */

    {
	char z = 0;
	char *p;
	sk_write(s, &z, 1);
	sk_write(s, cfg.localusername, strlen(cfg.localusername));
	sk_write(s, &z, 1);
	sk_write(s, cfg.username, strlen(cfg.username));
	sk_write(s, &z, 1);
	sk_write(s, cfg.termtype, strlen(cfg.termtype));
	sk_write(s, "/", 1);
	for (p = cfg.termspeed; isdigit(*p); p++);
	sk_write(s, cfg.termspeed, p - cfg.termspeed);
	rlogin_bufsize = sk_write(s, &z, 1);
    }

    return NULL;
}

/*
 * Called to send data down the rlogin connection.
 */
static int rlogin_send(char *buf, int len)
{
    if (s == NULL)
	return 0;

    rlogin_bufsize = sk_write(s, buf, len);

    return rlogin_bufsize;
}

/*
 * Called to query the current socket sendability status.
 */
static int rlogin_sendbuffer(void)
{
    return rlogin_bufsize;
}

/*
 * Called to set the size of the window
 */
static void rlogin_size(void)
{
    char b[12] = { '\xFF', '\xFF', 0x73, 0x73, 0, 0, 0, 0, 0, 0, 0, 0 };

    if (s == NULL)
	return;
    
    b[6] = cols >> 8;
    b[7] = cols & 0xFF;
    b[4] = rows >> 8;
    b[5] = rows & 0xFF;
    rlogin_bufsize = sk_write(s, b, 12);
    return;
}

/*
 * Send rlogin special codes.
 */
static void rlogin_special(Telnet_Special code)
{
    /* Do nothing! */
    return;
}

static Socket rlogin_socket(void)
{
    return s;
}

static int rlogin_sendok(void)
{
    return 1;
}

static void rlogin_unthrottle(int backlog)
{
    sk_set_frozen(s, backlog > RLOGIN_MAX_BACKLOG);
}

static int rlogin_ldisc(int option)
{
    return 0;
}

static int rlogin_exitcode(void)
{
    /* If we ever implement RSH, we'll probably need to do this properly */
    return 0;
}

Backend rlogin_backend = {
    rlogin_init,
    rlogin_send,
    rlogin_sendbuffer,
    rlogin_size,
    rlogin_special,
    rlogin_socket,
    rlogin_exitcode,
    rlogin_sendok,
    rlogin_ldisc,
    rlogin_unthrottle,
    1
};
