#include <windows.h>
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

struct pfwd_queue {
    struct pfwd_queue *next;
    char *buf;
};

struct PFwdPrivate {
    struct plug_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */
    void *c;			       /* (channel) data used by ssh.c */
    Socket s;
    char hostname[128];
    int throttled, throttle_override;
    int port;
    int ready;
    struct pfwd_queue *waiting;
};

void pfd_close(Socket s);


static int pfd_closing(Plug plug, char *error_msg, int error_code,
		       int calling_back)
{
    struct PFwdPrivate *pr = (struct PFwdPrivate *) plug;

    /*
     * We have no way to communicate down the forwarded connection,
     * so if an error occurred on the socket, we just ignore it
     * and treat it like a proper close.
     */
    sshfwd_close(pr->c);
    pfd_close(pr->s);
    return 1;
}

static int pfd_receive(Plug plug, int urgent, char *data, int len)
{
    struct PFwdPrivate *pr = (struct PFwdPrivate *) plug;
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

    sshfwd_unthrottle(pr->c, bufsize);
}

/*
 * Called when receiving a PORT OPEN from the server
 */
char *pfd_newconnect(Socket *s, char *hostname, int port, void *c)
{
    static struct plug_function_table fn_table = {
	pfd_closing,
	pfd_receive,
	pfd_sent,
	NULL
    };

    SockAddr addr;
    char *err, *dummy_realhost;
    struct PFwdPrivate *pr;

    /*
     * Try to find host.
     */
    addr = sk_namelookup(hostname, &dummy_realhost);
    if ((err = sk_addr_error(addr)))
	return err;

    /*
     * Open socket.
     */
    pr = (struct PFwdPrivate *) smalloc(sizeof(struct PFwdPrivate));
    pr->fn = &fn_table;
    pr->throttled = pr->throttle_override = 0;
    pr->ready = 1;
    pr->c = c;

    pr->s = *s = new_connection(addr, dummy_realhost, port, 0, 1, 0, (Plug) pr);
    if ((err = sk_socket_error(*s))) {
	sfree(pr);
	return err;
    }

    sk_set_private_ptr(*s, pr);
    sk_addr_free(addr);
    return NULL;
}

/*
 called when someone connects to the local port
 */

static int pfd_accepting(Plug p, void *sock)
{
    static struct plug_function_table fn_table = {
	pfd_closing,
	pfd_receive,
	pfd_sent,
	NULL
    };
    struct PFwdPrivate *pr, *org;
    Socket s;
    char *err;

    org = (struct PFwdPrivate *)p;
    pr = (struct PFwdPrivate *) smalloc(sizeof(struct PFwdPrivate));
    pr->fn = &fn_table;

    pr->c = NULL;

    pr->s = s = sk_register(sock, (Plug) pr);
    if ((err = sk_socket_error(s))) {
	sfree(pr);
	return err != NULL;
    }

    pr->c = new_sock_channel(s);

    strcpy(pr->hostname, org->hostname);
    pr->port = org->port;
    pr->throttled = pr->throttle_override = 0;
    pr->ready = 0;
    pr->waiting = NULL;

    sk_set_private_ptr(s, pr);

    if (pr->c == NULL) {
	sfree(pr);
	return 1;
    } else {
	/* asks to forward to the specified host/port for this */
	ssh_send_port_open(pr->c, pr->hostname, pr->port, "forwarding");
    }

    return 0;
}


/* Add a new forwarding from port -> desthost:destport
 sets up a listener on the local machine on port
 */
char *pfd_addforward(char *desthost, int destport, int port)
{
    static struct plug_function_table fn_table = {
	pfd_closing,
	pfd_receive,		       /* should not happen... */
	pfd_sent,		       /* also should not happen */
	pfd_accepting
    };

    char *err;
    struct PFwdPrivate *pr;
    Socket s;

    /*
     * Open socket.
     */
    pr = (struct PFwdPrivate *) smalloc(sizeof(struct PFwdPrivate));
    pr->fn = &fn_table;
    pr->c = NULL;
    strcpy(pr->hostname, desthost);
    pr->port = destport;
    pr->throttled = pr->throttle_override = 0;
    pr->ready = 0;
    pr->waiting = NULL;

    pr->s = s = new_listener(port, (Plug) pr, !cfg.lport_acceptall);
    if ((err = sk_socket_error(s))) {
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
}
