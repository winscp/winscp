#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>

#include "putty.h"
#include "ssh.h"

#define GET_32BIT_LSB_FIRST(cp) \
  (((unsigned long)(unsigned char)(cp)[0]) | \
  ((unsigned long)(unsigned char)(cp)[1] << 8) | \
  ((unsigned long)(unsigned char)(cp)[2] << 16) | \
  ((unsigned long)(unsigned char)(cp)[3] << 24))

#define PUT_32BIT_LSB_FIRST(cp, value) ( \
  (cp)[0] = (char)(value), \
  (cp)[1] = (char)((value) >> 8), \
  (cp)[2] = (char)((value) >> 16), \
  (cp)[3] = (char)((value) >> 24) )

#define GET_16BIT_LSB_FIRST(cp) \
  (((unsigned long)(unsigned char)(cp)[0]) | \
  ((unsigned long)(unsigned char)(cp)[1] << 8))

#define PUT_16BIT_LSB_FIRST(cp, value) ( \
  (cp)[0] = (char)(value), \
  (cp)[1] = (char)((value) >> 8) )

#define GET_32BIT_MSB_FIRST(cp) \
  (((unsigned long)(unsigned char)(cp)[0] << 24) | \
  ((unsigned long)(unsigned char)(cp)[1] << 16) | \
  ((unsigned long)(unsigned char)(cp)[2] << 8) | \
  ((unsigned long)(unsigned char)(cp)[3]))

#define PUT_32BIT_MSB_FIRST(cp, value) ( \
  (cp)[0] = (char)((value) >> 24), \
  (cp)[1] = (char)((value) >> 16), \
  (cp)[2] = (char)((value) >> 8), \
  (cp)[3] = (char)(value) )

#define GET_16BIT_MSB_FIRST(cp) \
  (((unsigned long)(unsigned char)(cp)[0] << 8) | \
  ((unsigned long)(unsigned char)(cp)[1]))

#define PUT_16BIT_MSB_FIRST(cp, value) ( \
  (cp)[0] = (char)((value) >> 8), \
  (cp)[1] = (char)(value) )

#define GET_16BIT(endian, cp) \
  (endian=='B' ? GET_16BIT_MSB_FIRST(cp) : GET_16BIT_LSB_FIRST(cp))

#define PUT_16BIT(endian, cp, val) \
  (endian=='B' ? PUT_16BIT_MSB_FIRST(cp, val) : PUT_16BIT_LSB_FIRST(cp, val))

const char *const x11_authnames[] = {
    "", "MIT-MAGIC-COOKIE-1", "XDM-AUTHORIZATION-1"
};

struct X11Auth {
    unsigned char fakedata[64], realdata[64];
    int fakeproto, realproto;
    int fakelen, reallen;
};

struct X11Private {
    const struct plug_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */
    unsigned char firstpkt[12];	       /* first X data packet */
    struct X11Auth *auth;
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

void *x11_invent_auth(char *proto, int protomaxlen,
		      char *data, int datamaxlen, int proto_id)
{
    struct X11Auth *auth = snew(struct X11Auth);
    char ourdata[64];
    int i;

    if (proto_id == X11_MIT) {
	auth->fakeproto = X11_MIT;

	/* MIT-MAGIC-COOKIE-1. Cookie size is 128 bits (16 bytes). */
	auth->fakelen = 16;
	for (i = 0; i < 16; i++)
	    auth->fakedata[i] = random_byte();
    } else {
	assert(proto_id == X11_XDM);
	auth->fakeproto = X11_XDM;

	/* XDM-AUTHORIZATION-1. Cookie size is 16 bytes; byte 8 is zero. */
	auth->fakelen = 16;
	for (i = 0; i < 16; i++)
	    auth->fakedata[i] = (i == 8 ? 0 : random_byte());
    }

    /* Now format for the recipient. */
    strncpy(proto, x11_authnames[auth->fakeproto], protomaxlen);
    ourdata[0] = '\0';
    for (i = 0; i < auth->fakelen; i++)
	sprintf(ourdata + strlen(ourdata), "%02x", auth->fakedata[i]);
    strncpy(data, ourdata, datamaxlen);

    return auth;
}

void x11_free_auth(void *auth)
{

    sfree(auth);
}

/*
 * Fetch the real auth data for a given display string, and store
 * it in an X11Auth structure. Returns NULL on success, or an error
 * string.
 */
void x11_get_real_auth(void *authv, char *display)
{
    struct X11Auth *auth = (struct X11Auth *)authv;

    auth->realproto = X11_NO_AUTH;     /* in case next call does nothing */

    auth->reallen = sizeof(auth->realdata);
    platform_get_x11_auth(display, &auth->realproto,
                          auth->realdata, &auth->reallen);
}

static char *x11_verify(unsigned long peer_ip, int peer_port,
			struct X11Auth *auth, char *proto,
			unsigned char *data, int dlen)
{
    if (strcmp(proto, x11_authnames[auth->fakeproto]) != 0)
	return "wrong authentication protocol attempted";
    if (auth->fakeproto == X11_MIT) {
        if (dlen != auth->fakelen)
            return "MIT-MAGIC-COOKIE-1 data was wrong length";
        if (memcmp(auth->fakedata, data, dlen) != 0)
            return "MIT-MAGIC-COOKIE-1 data did not match";
    }
    if (auth->fakeproto == X11_XDM) {
	unsigned long t;
	time_t tim;
	int i;

        if (dlen != 24)
            return "XDM-AUTHORIZATION-1 data was wrong length";
	if (peer_port == -1)
            return "cannot do XDM-AUTHORIZATION-1 without remote address data";
	des_decrypt_xdmauth(auth->fakedata+9, data, 24);
        if (memcmp(auth->fakedata, data, 8) != 0)
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
	if (abs(t - tim) > 20*60)      /* 20 minute clock skew should be OK */
	    return "XDM-AUTHORIZATION-1 time stamp was too far out";
    }
    /* implement other protocols here if ever required */
    return NULL;
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
const char *x11_init(Socket * s, char *display, void *c, void *auth,
		     const char *peeraddr, int peerport, const Config *cfg)
{
    static const struct plug_function_table fn_table = {
	x11_closing,
	x11_receive,
	x11_sent,
	NULL
    };

    SockAddr addr;
    int port;
    const char *err;
    char *dummy_realhost;
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
    if (n > 0) {
	strncpy(host, display, n);
	host[n] = '\0';
    } else {
	/*
	 * Local display numbers, particularly on Unix, often omit
	 * the display part completely.
	 */
	strcpy(host, "localhost");
    }

    port = 6000 + displaynum;

    /*
     * Try to find host.
     */
    addr = name_lookup(host, port, &dummy_realhost, cfg);
    if ((err = sk_addr_error(addr)) != NULL) {
	sk_addr_free(addr);
	return err;
    }

    /*
     * Open socket.
     */
    pr = snew(struct X11Private);
    pr->fn = &fn_table;
    pr->auth_protocol = NULL;
    pr->auth = (struct X11Auth *)auth;
    pr->verified = 0;
    pr->data_read = 0;
    pr->throttled = pr->throttle_override = 0;
    pr->c = c;

    pr->s = *s = new_connection(addr, dummy_realhost, port,
				0, 1, 0, (Plug) pr, cfg);
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
     * If we haven't verified the authentication, do so now.
     */
    if (!pr->verified) {
	char *err;

	pr->auth_protocol[pr->auth_plen] = '\0';	/* ASCIZ */
	err = x11_verify(pr->peer_ip, pr->peer_port,
			 pr->auth, pr->auth_protocol,
			 pr->auth_data, pr->auth_dlen);

	/*
	 * If authentication failed, construct and send an error
	 * packet, then terminate the connection.
	 */
	if (err) {
	    char *message;
	    int msglen, msgsize;
	    unsigned char *reply;

	    message = dupprintf("PuTTY X11 proxy: %s", err);
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
            int authstrlen = strlen(x11_authnames[pr->auth->realproto]);
	    unsigned long ip;
	    int port;
            static const char zeroes[4] = { 0,0,0,0 };

            if (pr->auth->realproto == X11_MIT) {
                assert(pr->auth->reallen <= lenof(realauthdata));
                realauthlen = pr->auth->reallen;
                memcpy(realauthdata, pr->auth->realdata, realauthlen);
            } else if (pr->auth->realproto == X11_XDM &&
		       pr->auth->reallen == 16 &&
		       sk_getxdmdata(s, &ip, &port)) {
		time_t t;
                realauthlen = 24;
		memset(realauthdata, 0, 24);
		memcpy(realauthdata, pr->auth->realdata, 8);
		PUT_32BIT_MSB_FIRST(realauthdata+8, ip);
		PUT_16BIT_MSB_FIRST(realauthdata+12, port);
		t = time(NULL);
		PUT_32BIT_MSB_FIRST(realauthdata+14, t);
		des_encrypt_xdmauth(pr->auth->realdata+9,
				    (unsigned char *)realauthdata, 24);
	    }
            /* implement other auth methods here if required */

            PUT_16BIT(pr->firstpkt[0], pr->firstpkt + 6, authstrlen);
            PUT_16BIT(pr->firstpkt[0], pr->firstpkt + 8, realauthlen);
        
            sk_write(s, (char *)pr->firstpkt, 12);

            if (authstrlen) {
                sk_write(s, x11_authnames[pr->auth->realproto], authstrlen);
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
