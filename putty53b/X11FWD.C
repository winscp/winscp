#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

#include "putty.h"
#include "ssh.h"

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

#define GET_16BIT(endian, cp) \
  (endian=='B' ? GET_16BIT_MSB_FIRST(cp) : GET_16BIT_LSB_FIRST(cp))

#define PUT_16BIT(endian, cp, val) \
  (endian=='B' ? PUT_16BIT_MSB_FIRST(cp, val) : PUT_16BIT_LSB_FIRST(cp, val))

struct X11Private {
    struct plug_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */
    unsigned char firstpkt[12];	       /* first X data packet */
    char *auth_protocol;
    unsigned char *auth_data;
    int data_read, auth_plen, auth_psize, auth_dlen, auth_dsize;
    int verified;
    int throttled, throttle_override;
    void *c;			       /* data used by ssh.c */
    Socket s;
};

void x11_close(Socket s);

static unsigned char x11_authdata[64];
static int x11_authdatalen;

void x11_invent_auth(char *proto, int protomaxlen,
		     char *data, int datamaxlen)
{
    char ourdata[64];
    int i;

    /* MIT-MAGIC-COOKIE-1. Cookie size is 128 bits (16 bytes). */
    x11_authdatalen = 16;
    for (i = 0; i < 16; i++)
	x11_authdata[i] = random_byte();

    /* Now format for the recipient. */
    strncpy(proto, "MIT-MAGIC-COOKIE-1", protomaxlen);
    ourdata[0] = '\0';
    for (i = 0; i < x11_authdatalen; i++)
	sprintf(ourdata + strlen(ourdata), "%02x", x11_authdata[i]);
    strncpy(data, ourdata, datamaxlen);
}

static int x11_verify(char *proto, unsigned char *data, int dlen)
{
    if (strcmp(proto, "MIT-MAGIC-COOKIE-1") != 0)
	return 0;		       /* wrong protocol attempted */
    if (dlen != x11_authdatalen)
	return 0;		       /* cookie was wrong length */
    if (memcmp(x11_authdata, data, dlen) != 0)
	return 0;		       /* cookie was wrong cookie! */
    return 1;
}

static int x11_closing(Plug plug, char *error_msg, int error_code,
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
 * Called to set up the raw connection.
 * 
 * Returns an error message, or NULL on success.
 * also, fills the SocketsStructure
 */
char *x11_init(Socket * s, char *display, void *c)
{
    static struct plug_function_table fn_table = {
	x11_closing,
	x11_receive,
	x11_sent,
	NULL
    };

    SockAddr addr;
    int port;
    char *err, *dummy_realhost;
    char host[128];
    int n, displaynum;
    struct X11Private *pr;

    /*
     * Split up display name into host and display-number parts.
     */
    n = strcspn(display, ":");
    if (display[n])
	displaynum = atoi(display + n + 1);
    else
	displaynum = 0;		       /* sensible default */
    if (n > sizeof(host) - 1)
	n = sizeof(host) - 1;
    strncpy(host, display, n);
    host[n] = '\0';

    /*
     * Try to find host.
     */
    addr = sk_namelookup(host, &dummy_realhost);
    if ((err = sk_addr_error(addr)))
	return err;

    port = 6000 + displaynum;

    /*
     * Open socket.
     */
    pr = (struct X11Private *) smalloc(sizeof(struct X11Private));
    pr->fn = &fn_table;
    pr->auth_protocol = NULL;
    pr->verified = 0;
    pr->data_read = 0;
    pr->throttled = pr->throttle_override = 0;
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
    struct X11Private *pr = (struct X11Private *) sk_get_private_ptr(s);

    if (s == NULL)
	return 0;

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
	pr->auth_protocol = (char *) smalloc(pr->auth_psize + 1);
	pr->auth_data = (char *) smalloc(pr->auth_dsize);
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
     * If we haven't verified the authentication, do so now.
     */
    if (!pr->verified) {
	int ret;

	pr->auth_protocol[pr->auth_plen] = '\0';	/* ASCIZ */
	ret = x11_verify(pr->auth_protocol, pr->auth_data, pr->auth_dlen);

	/*
	 * If authentication failed, construct and send an error
	 * packet, then terminate the connection.
	 */
	if (!ret) {
	    char message[] = "Authentication failed at PuTTY X11 proxy";
	    unsigned char reply[8 + sizeof(message) + 4];
	    int msglen = sizeof(message) - 1;	/* skip zero byte */
	    int msgsize = (msglen + 3) & ~3;
	    reply[0] = 0;	       /* failure */
	    reply[1] = msglen;	       /* length of reason string */
	    memcpy(reply + 2, pr->firstpkt + 2, 4);	/* major/minor proto vsn */
	    PUT_16BIT(pr->firstpkt[0], reply + 6, msgsize >> 2);/* data len */
	    memset(reply + 8, 0, msgsize);
	    memcpy(reply + 8, message, msglen);
	    sshfwd_write(pr->c, reply, 8 + msgsize);
	    sshfwd_close(pr->c);
	    x11_close(s);
	    return 0;
	}

	/*
	 * Now we know we're going to accept the connection. Strip
	 * the auth data. (TODO: if we ever work out how, we should
	 * replace some real auth data in here.)
	 */
	PUT_16BIT(pr->firstpkt[0], pr->firstpkt + 6, 0);	/* auth proto */
	PUT_16BIT(pr->firstpkt[0], pr->firstpkt + 8, 0);	/* auth data */
	sk_write(s, pr->firstpkt, 12);
	pr->verified = 1;
    }

    /*
     * After initialisation, just copy data simply.
     */

    return sk_write(s, data, len);
}
