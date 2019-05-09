/*
 * Platform-independent bits of X11 forwarding.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>

#include "putty.h"
#include "ssh.h"
#include "sshchan.h"
#include "tree234.h"

static inline uint16_t GET_16BIT_X11(char endian, const void *p)
{
    return endian == 'B' ? GET_16BIT_MSB_FIRST(p) : GET_16BIT_LSB_FIRST(p);
}

static inline void PUT_16BIT_X11(char endian, void *p, uint16_t value)
{
    if (endian == 'B')
        PUT_16BIT_MSB_FIRST(p, value);
    else
        PUT_16BIT_LSB_FIRST(p, value);
}

const char *const x11_authnames[] = {
    "", "MIT-MAGIC-COOKIE-1", "XDM-AUTHORIZATION-1"
};

struct XDMSeen {
    unsigned int time;
    unsigned char clientid[6];
};

typedef struct X11Connection {
    unsigned char firstpkt[12];	       /* first X data packet */
    tree234 *authtree;
    struct X11Display *disp;
    char *auth_protocol;
    unsigned char *auth_data;
    int data_read, auth_plen, auth_psize, auth_dlen, auth_dsize;
    bool verified;
    bool input_wanted;
    bool no_data_sent_to_x_client;
    char *peer_addr;
    int peer_port;
    SshChannel *c;               /* channel structure held by SSH backend */
    Socket *s;

    Plug plug;
    Channel chan;
} X11Connection;

static int xdmseen_cmp(void *a, void *b)
{
    struct XDMSeen *sa = a, *sb = b;
    return sa->time > sb->time ? 1 :
	   sa->time < sb->time ? -1 :
           memcmp(sa->clientid, sb->clientid, sizeof(sa->clientid));
}

struct X11FakeAuth *x11_invent_fake_auth(tree234 *authtree, int authtype)
{
    struct X11FakeAuth *auth = snew(struct X11FakeAuth);
    int i;

    /*
     * This function has the job of inventing a set of X11 fake auth
     * data, and adding it to 'authtree'. We must preserve the
     * property that for any given actual authorisation attempt, _at
     * most one_ thing in the tree can possibly match it.
     *
     * For MIT-MAGIC-COOKIE-1, that's not too difficult: the match
     * criterion is simply that the entire cookie is correct, so we
     * just have to make sure we don't make up two cookies the same.
     * (Vanishingly unlikely, but we check anyway to be sure, and go
     * round again inventing a new cookie if add234 tells us the one
     * we thought of is already in use.)
     *
     * For XDM-AUTHORIZATION-1, it's a little more fiddly. The setup
     * with XA1 is that half the cookie is used as a DES key with
     * which to CBC-encrypt an assortment of stuff. Happily, the stuff
     * encrypted _begins_ with the other half of the cookie, and the
     * IV is always zero, which means that any valid XA1 authorisation
     * attempt for a given cookie must begin with the same cipher
     * block, consisting of the DES ECB encryption of the first half
     * of the cookie using the second half as a key. So we compute
     * that cipher block here and now, and use it as the sorting key
     * for distinguishing XA1 entries in the tree.
     */

    if (authtype == X11_MIT) {
	auth->proto = X11_MIT;

	/* MIT-MAGIC-COOKIE-1. Cookie size is 128 bits (16 bytes). */
        auth->datalen = 16;
	auth->data = snewn(auth->datalen, unsigned char);
        auth->xa1_firstblock = NULL;

        while (1) {
            random_read(auth->data, auth->datalen);
            if (add234(authtree, auth) == auth)
                break;
        }

	auth->xdmseen = NULL;
    } else {
	assert(authtype == X11_XDM);
	auth->proto = X11_XDM;

	/* XDM-AUTHORIZATION-1. Cookie size is 16 bytes; byte 8 is zero. */
	auth->datalen = 16;
	auth->data = snewn(auth->datalen, unsigned char);
        auth->xa1_firstblock = snewn(8, unsigned char);
        memset(auth->xa1_firstblock, 0, 8);

        while (1) {
            random_read(auth->data, 15);
            auth->data[15] = auth->data[8];
            auth->data[8] = 0;

            memcpy(auth->xa1_firstblock, auth->data, 8);
            des_encrypt_xdmauth(auth->data + 9, auth->xa1_firstblock, 8);
            if (add234(authtree, auth) == auth)
                break;
        }

        auth->xdmseen = newtree234(xdmseen_cmp);
    }
    auth->protoname = dupstr(x11_authnames[auth->proto]);
    auth->datastring = snewn(auth->datalen * 2 + 1, char);
    for (i = 0; i < auth->datalen; i++)
	sprintf(auth->datastring + i*2, "%02x",
		auth->data[i]);

    auth->disp = NULL;
    auth->share_cs = NULL;
    auth->share_chan = NULL;

    return auth;
}

void x11_free_fake_auth(struct X11FakeAuth *auth)
{
    if (auth->data)
	smemclr(auth->data, auth->datalen);
    sfree(auth->data);
    sfree(auth->protoname);
    sfree(auth->datastring);
    sfree(auth->xa1_firstblock);
    if (auth->xdmseen != NULL) {
	struct XDMSeen *seen;
	while ((seen = delpos234(auth->xdmseen, 0)) != NULL)
	    sfree(seen);
	freetree234(auth->xdmseen);
    }
    sfree(auth);
}

int x11_authcmp(void *av, void *bv)
{
    struct X11FakeAuth *a = (struct X11FakeAuth *)av;
    struct X11FakeAuth *b = (struct X11FakeAuth *)bv;

    if (a->proto < b->proto)
        return -1;
    else if (a->proto > b->proto)
        return +1;

    if (a->proto == X11_MIT) {
        if (a->datalen < b->datalen)
            return -1;
        else if (a->datalen > b->datalen)
            return +1;

        return memcmp(a->data, b->data, a->datalen);
    } else {
        assert(a->proto == X11_XDM);

        return memcmp(a->xa1_firstblock, b->xa1_firstblock, 8);
    }
}

struct X11Display *x11_setup_display(const char *display, Conf *conf,
                                     char **error_msg)
{
    struct X11Display *disp = snew(struct X11Display);
    char *localcopy;

    *error_msg = NULL;

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
	disp->unixdomain = true;
	disp->hostname = NULL;
	disp->displaynum = -1;
	disp->screennum = 0;
	disp->addr = NULL;
    } else {
	char *colon, *dot, *slash;
	char *protocol, *hostname;

	colon = host_strrchr(localcopy, ':');
	if (!colon) {
            *error_msg = dupprintf("display name '%s' has no ':number'"
                                   " suffix", localcopy);

	    sfree(disp);
	    sfree(localcopy);
	    return NULL;
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
	    disp->unixdomain = false;

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
				 &disp->realhost, conf, ADDRTYPE_UNSPEC,
                                 NULL, NULL);
    
	if ((err = sk_addr_error(disp->addr)) != NULL) {
            *error_msg = dupprintf("unable to resolve host name '%s' in "
                                   "display name", disp->hostname);

	    sk_addr_free(disp->addr);
	    sfree(disp->hostname);
	    sfree(disp->unixsocketpath);
	    sfree(disp);
	    return NULL;
	}
    }

    /*
     * Try upgrading an IP-style localhost display to a Unix-socket
     * display (as the standard X connection libraries do).
     */
    if (!disp->unixdomain && sk_address_is_local(disp->addr)) {
	SockAddr *ux = platform_get_x11_unix_address(NULL, disp->displaynum);
	const char *err = sk_addr_error(ux);
	if (!err) {
	    /* Create trial connection to see if there is a useful Unix-domain
	     * socket */
	    Socket *s = sk_new(sk_addr_dup(ux), 0, false, false,
                               false, false, nullplug);
	    err = sk_socket_error(s);
	    sk_close(s);
	}
	if (err) {
	    sk_addr_free(ux);
	} else {
	    sk_addr_free(disp->addr);
	    disp->unixdomain = true;
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
     * Fetch the local authorisation details.
     */
    disp->localauthproto = X11_NO_AUTH;
    disp->localauthdata = NULL;
    disp->localauthdatalen = 0;
    platform_get_x11_auth(disp, conf);

    return disp;
}

void x11_free_display(struct X11Display *disp)
{
    sfree(disp->hostname);
    sfree(disp->unixsocketpath);
    if (disp->localauthdata)
	smemclr(disp->localauthdata, disp->localauthdatalen);
    sfree(disp->localauthdata);
    sk_addr_free(disp->addr);
    sfree(disp);
}

#define XDM_MAXSKEW 20*60      /* 20 minute clock skew should be OK */

static const char *x11_verify(unsigned long peer_ip, int peer_port,
                              tree234 *authtree, char *proto,
                              unsigned char *data, int dlen,
                              struct X11FakeAuth **auth_ret)
{
    struct X11FakeAuth match_dummy;    /* for passing to find234 */
    struct X11FakeAuth *auth;

    /*
     * First, do a lookup in our tree to find the only authorisation
     * record that _might_ match.
     */
    if (!strcmp(proto, x11_authnames[X11_MIT])) {
        /*
         * Just look up the whole cookie that was presented to us,
         * which x11_authcmp will compare against the cookies we
         * currently believe in.
         */
        match_dummy.proto = X11_MIT;
        match_dummy.datalen = dlen;
        match_dummy.data = data;
    } else if (!strcmp(proto, x11_authnames[X11_XDM])) {
        /*
         * Look up the first cipher block, against the stored first
         * cipher blocks for the XDM-AUTHORIZATION-1 cookies we
         * currently know. (See comment in x11_invent_fake_auth.)
         */
        match_dummy.proto = X11_XDM;
        match_dummy.xa1_firstblock = data;
    } else {
        return "Unsupported authorisation protocol";
    }

    if ((auth = find234(authtree, &match_dummy, 0)) == NULL)
        return "Authorisation not recognised";

    /*
     * If we're using MIT-MAGIC-COOKIE-1, that was all we needed. If
     * we're doing XDM-AUTHORIZATION-1, though, we have to check the
     * rest of the auth data.
     */
    if (auth->proto == X11_XDM) {
	unsigned long t;
	time_t tim;
	int i;
	struct XDMSeen *seen, *ret;

        if (dlen != 24)
            return "XDM-AUTHORIZATION-1 data was wrong length";
	if (peer_port == -1)
            return "cannot do XDM-AUTHORIZATION-1 without remote address data";
	des_decrypt_xdmauth(auth->data+9, data, 24);
        if (memcmp(auth->data, data, 8) != 0)
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
	if (((unsigned long)t - (unsigned long)tim
             + XDM_MAXSKEW) > 2*XDM_MAXSKEW)
	    return "XDM-AUTHORIZATION-1 time stamp was too far out";
	seen = snew(struct XDMSeen);
	seen->time = t;
	memcpy(seen->clientid, data+8, 6);
	assert(auth->xdmseen != NULL);
	ret = add234(auth->xdmseen, seen);
	if (ret != seen) {
	    sfree(seen);
	    return "XDM-AUTHORIZATION-1 data replayed";
	}
	/* While we're here, purge entries too old to be replayed. */
	for (;;) {
	    seen = index234(auth->xdmseen, 0);
	    assert(seen != NULL);
	    if (t - seen->time <= XDM_MAXSKEW)
		break;
	    sfree(delpos234(auth->xdmseen, 0));
	}
    }
    /* implement other protocols here if ever required */

    *auth_ret = auth;
    return NULL;
}

ptrlen BinarySource_get_string_xauth(BinarySource *src)
{
    size_t len = get_uint16(src);
    return get_data(src, len);
}
#define get_string_xauth(src) \
    BinarySource_get_string_xauth(BinarySource_UPCAST(src))

void BinarySink_put_stringpl_xauth(BinarySink *bs, ptrlen pl)
{
    assert((pl.len >> 16) == 0);
    put_uint16(bs, pl.len);
    put_datapl(bs, pl);
}
#define put_stringpl_xauth(bs, ptrlen) \
    BinarySink_put_stringpl_xauth(BinarySink_UPCAST(bs),ptrlen)

void x11_get_auth_from_authfile(struct X11Display *disp,
				const char *authfilename)
{
    FILE *authfp;
    char *buf;
    int size;
    BinarySource src[1];
    int family, protocol;
    ptrlen addr, protoname, data;
    char *displaynum_string;
    int displaynum;
    bool ideal_match = false;
    char *ourhostname;

    /* A maximally sized (wildly implausible) .Xauthority record
     * consists of a 16-bit integer to start with, then four strings,
     * each of which has a 16-bit length field followed by that many
     * bytes of data (i.e. up to 0xFFFF bytes). */
    const size_t MAX_RECORD_SIZE = 2 + 4 * (2+0xFFFF);

    /* We'll want a buffer of twice that size (see below). */
    const size_t BUF_SIZE = 2 * MAX_RECORD_SIZE;

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
    bool localhost = !disp->unixdomain && sk_address_is_local(disp->addr);

    authfp = fopen(authfilename, "rb");
    if (!authfp)
	return;

    ourhostname = get_hostname();

    /*
     * Allocate enough space to hold two maximally sized records, so
     * that a full record can start anywhere in the first half. That
     * way we avoid the accidentally-quadratic algorithm that would
     * arise if we moved everything to the front of the buffer after
     * consuming each record; instead, we only move everything to the
     * front after our current position gets past the half-way mark.
     * Before then, there's no need to move anyway; so this guarantees
     * linear time, in that every byte written into this buffer moves
     * at most once (because every move is from the second half of the
     * buffer to the first half).
     */
    buf = snewn(BUF_SIZE, char);
    size = fread(buf, 1, BUF_SIZE, authfp);
    BinarySource_BARE_INIT(src, buf, size);

    while (!ideal_match) {
        bool match = false;

        if (src->pos >= MAX_RECORD_SIZE) {
            size -= src->pos;
            memcpy(buf, buf + src->pos, size);
            size += fread(buf + size, 1, BUF_SIZE - size, authfp);
            BinarySource_BARE_INIT(src, buf, size);
        }

        family = get_uint16(src);
        addr = get_string_xauth(src);
        displaynum_string = mkstr(get_string_xauth(src));
        displaynum = displaynum_string[0] ? atoi(displaynum_string) : -1;
        sfree(displaynum_string);
        protoname = get_string_xauth(src);
        data = get_string_xauth(src);
        if (get_err(src))
            break;

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
	 *  - 'addr' is the network address itself. For IPv4 and
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
	 *  - 'displaynum' is the display number. An empty display
	 *    number is a wildcard for any display number.
	 * 
	 *  - 'protoname' is the authorisation protocol, encoded as
	 *    its canonical string name (i.e. "MIT-MAGIC-COOKIE-1",
	 *    "XDM-AUTHORIZATION-1" or something we don't recognise).
	 * 
	 *  - 'data' is the actual authorisation data, stored in
	 *    binary form.
	 */

	if (disp->displaynum < 0 ||
	    (displaynum >= 0 && disp->displaynum != displaynum))
	    continue;		       /* not the one */

	for (protocol = 1; protocol < lenof(x11_authnames); protocol++)
	    if (ptrlen_eq_string(protoname, x11_authnames[protocol]))
		break;
	if (protocol == lenof(x11_authnames))
	    continue;  /* don't recognise this protocol, look for another */

	switch (family) {
	  case 0:   /* IPv4 */
	    if (!disp->unixdomain &&
		sk_addrtype(disp->addr) == ADDRTYPE_IPV4) {
		char buf[4];
		sk_addrcopy(disp->addr, buf);
		if (addr.len == 4 && !memcmp(addr.ptr, buf, 4)) {
		    match = true;
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
		if (addr.len == 16 && !memcmp(addr.ptr, buf, 16)) {
		    match = true;
		    ideal_match = !localhost;
		}
	    }
	    break;
	  case 256: /* Unix-domain / localhost */
	    if ((disp->unixdomain || localhost)
                && ourhostname && ptrlen_eq_string(addr, ourhostname)) {
		/* A matching Unix-domain socket is always the best
		 * match. */
		match = true;
                ideal_match = true;
            }
	    break;
	}

	if (match) {
	    /* Current best guess -- may be overridden if !ideal_match */
	    disp->localauthproto = protocol;
	    sfree(disp->localauthdata); /* free previous guess, if any */
	    disp->localauthdata = snewn(data.len, unsigned char);
	    memcpy(disp->localauthdata, data.ptr, data.len);
	    disp->localauthdatalen = data.len;
	}
    }

    fclose(authfp);
    smemclr(buf, 2 * MAX_RECORD_SIZE);
    sfree(buf);
    sfree(ourhostname);
}

void x11_format_auth_for_authfile(
    BinarySink *bs, SockAddr *addr, int display_no,
    ptrlen authproto, ptrlen authdata)
{
    if (sk_address_is_special_local(addr)) {
        char *ourhostname = get_hostname();
        put_uint16(bs, 256); /* indicates Unix-domain socket */
        put_stringpl_xauth(bs, ptrlen_from_asciz(ourhostname));
        sfree(ourhostname);
    } else if (sk_addrtype(addr) == ADDRTYPE_IPV4) {
        char ipv4buf[4];
        sk_addrcopy(addr, ipv4buf);
        put_uint16(bs, 0); /* indicates IPv4 */
        put_stringpl_xauth(bs, make_ptrlen(ipv4buf, 4));
    } else if (sk_addrtype(addr) == ADDRTYPE_IPV6) {
        char ipv6buf[16];
        sk_addrcopy(addr, ipv6buf);
        put_uint16(bs, 6); /* indicates IPv6 */
        put_stringpl_xauth(bs, make_ptrlen(ipv6buf, 16));
    } else {
        unreachable("Bad address type in x11_format_auth_for_authfile");
    }

    {
        char *numberbuf = dupprintf("%d", display_no);
        put_stringpl_xauth(bs, ptrlen_from_asciz(numberbuf));
        sfree(numberbuf);
    }

    put_stringpl_xauth(bs, authproto);
    put_stringpl_xauth(bs, authdata);
}

static void x11_log(Plug *p, int type, SockAddr *addr, int port,
		    const char *error_msg, int error_code)
{
    /* We have no interface to the logging module here, so we drop these. */
}

static void x11_send_init_error(struct X11Connection *conn,
                                const char *err_message);

static void x11_closing(Plug *plug, const char *error_msg, int error_code,
			bool calling_back)
{
    struct X11Connection *xconn = container_of(
        plug, struct X11Connection, plug);

    if (error_msg) {
        /*
         * Socket error. If we're still at the connection setup stage,
         * construct an X11 error packet passing on the problem.
         */
        if (xconn->no_data_sent_to_x_client) {
            char *err_message = dupprintf("unable to connect to forwarded "
                                          "X server: %s", error_msg);
            x11_send_init_error(xconn, err_message);
            sfree(err_message);
        }

        /*
         * Whether we did that or not, now we slam the connection
         * shut.
         */
        sshfwd_initiate_close(xconn->c, error_msg);
    } else {
        /*
         * Ordinary EOF received on socket. Send an EOF on the SSH
         * channel.
         */
        if (xconn->c)
            sshfwd_write_eof(xconn->c);
    }
}

static void x11_receive(Plug *plug, int urgent, const char *data, size_t len)
{
    struct X11Connection *xconn = container_of(
        plug, struct X11Connection, plug);

    xconn->no_data_sent_to_x_client = false;
    sshfwd_write(xconn->c, data, len);
}

static void x11_sent(Plug *plug, size_t bufsize)
{
    struct X11Connection *xconn = container_of(
        plug, struct X11Connection, plug);

    sshfwd_unthrottle(xconn->c, bufsize);
}

/*
 * When setting up X forwarding, we should send the screen number
 * from the specified local display. This function extracts it from
 * the display string.
 */
int x11_get_screen_number(char *display)
{
    int n;

    n = host_strcspn(display, ":");
    if (!display[n])
	return 0;
    n = strcspn(display, ".");
    if (!display[n])
	return 0;
    return atoi(display + n + 1);
}

static const PlugVtable X11Connection_plugvt = {
    x11_log,
    x11_closing,
    x11_receive,
    x11_sent,
    NULL
};

static void x11_chan_free(Channel *chan);
static size_t x11_send(
    Channel *chan, bool is_stderr, const void *vdata, size_t len);
static void x11_send_eof(Channel *chan);
static void x11_set_input_wanted(Channel *chan, bool wanted);
static char *x11_log_close_msg(Channel *chan);

static const struct ChannelVtable X11Connection_channelvt = {
    x11_chan_free,
    chan_remotely_opened_confirmation,
    chan_remotely_opened_failure,
    x11_send,
    x11_send_eof,
    x11_set_input_wanted,
    x11_log_close_msg,
    chan_default_want_close,
    chan_no_exit_status,
    chan_no_exit_signal,
    chan_no_exit_signal_numeric,
    chan_no_run_shell,
    chan_no_run_command,
    chan_no_run_subsystem,
    chan_no_enable_x11_forwarding,
    chan_no_enable_agent_forwarding,
    chan_no_allocate_pty,
    chan_no_set_env,
    chan_no_send_break,
    chan_no_send_signal,
    chan_no_change_window_size,
    chan_no_request_response,
};

/*
 * Called to set up the X11Connection structure, though this does not
 * yet connect to an actual server.
 */
Channel *x11_new_channel(tree234 *authtree, SshChannel *c,
                         const char *peeraddr, int peerport,
                         bool connection_sharing_possible)
{
    struct X11Connection *xconn;

    /*
     * Open socket.
     */
    xconn = snew(struct X11Connection);
    xconn->plug.vt = &X11Connection_plugvt;
    xconn->chan.vt = &X11Connection_channelvt;
    xconn->chan.initial_fixed_window_size =
        (connection_sharing_possible ? 128 : 0);
    xconn->auth_protocol = NULL;
    xconn->authtree = authtree;
    xconn->verified = false;
    xconn->data_read = 0;
    xconn->input_wanted = true;
    xconn->no_data_sent_to_x_client = true;
    xconn->c = c;

    /*
     * We don't actually open a local socket to the X server just yet,
     * because we don't know which one it is. Instead, we'll wait
     * until we see the incoming authentication data, which may tell
     * us what display to connect to, or whether we have to divert
     * this X forwarding channel to a connection-sharing downstream
     * rather than handling it ourself.
     */
    xconn->disp = NULL;
    xconn->s = NULL;

    /*
     * Stash the peer address we were given in its original text form.
     */
    xconn->peer_addr = peeraddr ? dupstr(peeraddr) : NULL;
    xconn->peer_port = peerport;

    return &xconn->chan;
}

static void x11_chan_free(Channel *chan)
{
    assert(chan->vt == &X11Connection_channelvt);
    X11Connection *xconn = container_of(chan, X11Connection, chan);

    if (xconn->auth_protocol) {
	sfree(xconn->auth_protocol);
	sfree(xconn->auth_data);
    }

    if (xconn->s)
        sk_close(xconn->s);

    sfree(xconn->peer_addr);
    sfree(xconn);
}

static void x11_set_input_wanted(Channel *chan, bool wanted)
{
    assert(chan->vt == &X11Connection_channelvt);
    X11Connection *xconn = container_of(chan, X11Connection, chan);

    xconn->input_wanted = wanted;
    if (xconn->s)
        sk_set_frozen(xconn->s, !xconn->input_wanted);
}

static void x11_send_init_error(struct X11Connection *xconn,
                                const char *err_message)
{
    char *full_message;
    int msglen, msgsize;
    unsigned char *reply;

    full_message = dupprintf("%s X11 proxy: %s\n", appname, err_message);

    msglen = strlen(full_message);
    reply = snewn(8 + msglen+1 + 4, unsigned char); /* include zero */
    msgsize = (msglen + 3) & ~3;
    reply[0] = 0;	       /* failure */
    reply[1] = msglen;	       /* length of reason string */
    memcpy(reply + 2, xconn->firstpkt + 2, 4);	/* major/minor proto vsn */
    PUT_16BIT_X11(xconn->firstpkt[0], reply + 6, msgsize >> 2);/* data len */
    memset(reply + 8, 0, msgsize);
    memcpy(reply + 8, full_message, msglen);
    sshfwd_write(xconn->c, reply, 8 + msgsize);
    sshfwd_write_eof(xconn->c);
    xconn->no_data_sent_to_x_client = false;
    sfree(reply);
    sfree(full_message);
}

static bool x11_parse_ip(const char *addr_string, unsigned long *ip)
{

    /*
     * See if we can make sense of this string as an IPv4 address, for
     * XDM-AUTHORIZATION-1 purposes.
     */
    int i[4];
    if (addr_string &&
        4 == sscanf(addr_string, "%d.%d.%d.%d", i+0, i+1, i+2, i+3)) {
        *ip = (i[0] << 24) | (i[1] << 16) | (i[2] << 8) | i[3];
        return true;
    } else {
        return false;
    }
}

/*
 * Called to send data down the raw connection.
 */
static size_t x11_send(
    Channel *chan, bool is_stderr, const void *vdata, size_t len)
{
    assert(chan->vt == &X11Connection_channelvt);
    X11Connection *xconn = container_of(chan, X11Connection, chan);
    const char *data = (const char *)vdata;

    /*
     * Read the first packet.
     */
    while (len > 0 && xconn->data_read < 12)
	xconn->firstpkt[xconn->data_read++] = (unsigned char) (len--, *data++);
    if (xconn->data_read < 12)
	return 0;

    /*
     * If we have not allocated the auth_protocol and auth_data
     * strings, do so now.
     */
    if (!xconn->auth_protocol) {
        char endian = xconn->firstpkt[0];
	xconn->auth_plen = GET_16BIT_X11(endian, xconn->firstpkt + 6);
	xconn->auth_dlen = GET_16BIT_X11(endian, xconn->firstpkt + 8);
	xconn->auth_psize = (xconn->auth_plen + 3) & ~3;
	xconn->auth_dsize = (xconn->auth_dlen + 3) & ~3;
	/* Leave room for a terminating zero, to make our lives easier. */
	xconn->auth_protocol = snewn(xconn->auth_psize + 1, char);
	xconn->auth_data = snewn(xconn->auth_dsize, unsigned char);
    }

    /*
     * Read the auth_protocol and auth_data strings.
     */
    while (len > 0 &&
           xconn->data_read < 12 + xconn->auth_psize)
	xconn->auth_protocol[xconn->data_read++ - 12] = (len--, *data++);
    while (len > 0 &&
           xconn->data_read < 12 + xconn->auth_psize + xconn->auth_dsize)
	xconn->auth_data[xconn->data_read++ - 12 -
		      xconn->auth_psize] = (unsigned char) (len--, *data++);
    if (xconn->data_read < 12 + xconn->auth_psize + xconn->auth_dsize)
	return 0;

    /*
     * If we haven't verified the authorisation, do so now.
     */
    if (!xconn->verified) {
	const char *err;
        struct X11FakeAuth *auth_matched = NULL;
        unsigned long peer_ip;
        int peer_port;
        int protomajor, protominor;
        void *greeting;
        int greeting_len;
        unsigned char *socketdata;
        int socketdatalen;
        char new_peer_addr[32];
        int new_peer_port;
        char endian = xconn->firstpkt[0];

        protomajor = GET_16BIT_X11(endian, xconn->firstpkt + 2);
        protominor = GET_16BIT_X11(endian, xconn->firstpkt + 4);

        assert(!xconn->s);

	xconn->auth_protocol[xconn->auth_plen] = '\0';	/* ASCIZ */

        peer_ip = 0;                   /* placate optimiser */
        if (x11_parse_ip(xconn->peer_addr, &peer_ip))
            peer_port = xconn->peer_port;
        else
            peer_port = -1; /* signal no peer address data available */

	err = x11_verify(peer_ip, peer_port,
			 xconn->authtree, xconn->auth_protocol,
			 xconn->auth_data, xconn->auth_dlen, &auth_matched);
	if (err) {
            x11_send_init_error(xconn, err);
            return 0;
        }
        assert(auth_matched);

        /*
         * If this auth points to a connection-sharing downstream
         * rather than an X display we know how to connect to
         * directly, pass it off to the sharing module now. (This will
         * have the side effect of freeing xconn.)
         */
        if (auth_matched->share_cs) {
            sshfwd_x11_sharing_handover(xconn->c, auth_matched->share_cs,
                                        auth_matched->share_chan,
                                        xconn->peer_addr, xconn->peer_port,
                                        xconn->firstpkt[0],
                                        protomajor, protominor, data, len);
            return 0;
        }

        /*
         * Now we know we're going to accept the connection, and what
         * X display to connect to. Actually connect to it.
         */
        xconn->chan.initial_fixed_window_size = 0;
        sshfwd_window_override_removed(xconn->c);
        xconn->disp = auth_matched->disp;
        xconn->s = new_connection(sk_addr_dup(xconn->disp->addr),
                                  xconn->disp->realhost, xconn->disp->port, 
                                  false, true, false, false, &xconn->plug,
                                  sshfwd_get_conf(xconn->c));
        if ((err = sk_socket_error(xconn->s)) != NULL) {
            char *err_message = dupprintf("unable to connect to"
                                          " forwarded X server: %s", err);
            x11_send_init_error(xconn, err_message);
            sfree(err_message);
            return 0;
        }

        /*
         * Write a new connection header containing our replacement
         * auth data.
	 */
        socketdatalen = 0;             /* placate compiler warning */
        socketdata = sk_getxdmdata(xconn->s, &socketdatalen);
        if (socketdata && socketdatalen==6) {
            sprintf(new_peer_addr, "%d.%d.%d.%d", socketdata[0],
                    socketdata[1], socketdata[2], socketdata[3]);
            new_peer_port = GET_16BIT_MSB_FIRST(socketdata + 4);
        } else {
            strcpy(new_peer_addr, "0.0.0.0");
            new_peer_port = 0;
        }

        greeting = x11_make_greeting(xconn->firstpkt[0],
                                     protomajor, protominor,
                                     xconn->disp->localauthproto,
                                     xconn->disp->localauthdata,
                                     xconn->disp->localauthdatalen,
                                     new_peer_addr, new_peer_port,
                                     &greeting_len);
        
        sk_write(xconn->s, greeting, greeting_len);

        smemclr(greeting, greeting_len);
        sfree(greeting);

        /*
         * Now we're done.
         */
	xconn->verified = true;
    }

    /*
     * After initialisation, just copy data simply.
     */

    return sk_write(xconn->s, data, len);
}

static void x11_send_eof(Channel *chan)
{
    assert(chan->vt == &X11Connection_channelvt);
    X11Connection *xconn = container_of(chan, X11Connection, chan);

    if (xconn->s) {
        sk_write_eof(xconn->s);
    } else {
        /*
         * If EOF is received from the X client before we've got to
         * the point of actually connecting to an X server, then we
         * should send an EOF back to the client so that the
         * forwarded channel will be terminated.
         */
        if (xconn->c)
            sshfwd_write_eof(xconn->c);
    }
}

static char *x11_log_close_msg(Channel *chan)
{
    return dupstr("Forwarded X11 connection terminated");
}

/*
 * Utility functions used by connection sharing to convert textual
 * representations of an X11 auth protocol name + hex cookie into our
 * usual integer protocol id and binary auth data.
 */
int x11_identify_auth_proto(ptrlen protoname)
{
    int protocol;

    for (protocol = 1; protocol < lenof(x11_authnames); protocol++)
        if (ptrlen_eq_string(protoname, x11_authnames[protocol]))
            return protocol;
    return -1;
}

void *x11_dehexify(ptrlen hexpl, int *outlen)
{
    int len, i;
    unsigned char *ret;

    len = hexpl.len / 2;
    ret = snewn(len, unsigned char);

    for (i = 0; i < len; i++) {
        char bytestr[3];
        unsigned val = 0;
        bytestr[0] = ((const char *)hexpl.ptr)[2*i];
        bytestr[1] = ((const char *)hexpl.ptr)[2*i+1];
        bytestr[2] = '\0';
        sscanf(bytestr, "%x", &val);
        ret[i] = val;
    }

    *outlen = len;
    return ret;
}

/*
 * Construct an X11 greeting packet, including making up the right
 * authorisation data.
 */
void *x11_make_greeting(int endian, int protomajor, int protominor,
                        int auth_proto, const void *auth_data, int auth_len,
                        const char *peer_addr, int peer_port,
                        int *outlen)
{
    unsigned char *greeting;
    unsigned char realauthdata[64];
    const char *authname;
    const unsigned char *authdata;
    int authnamelen, authnamelen_pad;
    int authdatalen, authdatalen_pad;
    int greeting_len;

    authname = x11_authnames[auth_proto];
    authnamelen = strlen(authname);
    authnamelen_pad = (authnamelen + 3) & ~3;

    if (auth_proto == X11_MIT) {
        authdata = auth_data;
        authdatalen = auth_len;
    } else if (auth_proto == X11_XDM && auth_len == 16) {
        time_t t;
        unsigned long peer_ip = 0;

        x11_parse_ip(peer_addr, &peer_ip);

        authdata = realauthdata;
        authdatalen = 24;
        memset(realauthdata, 0, authdatalen);
        memcpy(realauthdata, auth_data, 8);
        PUT_32BIT_MSB_FIRST(realauthdata+8, peer_ip);
        PUT_16BIT_MSB_FIRST(realauthdata+12, peer_port);
        t = time(NULL);
        PUT_32BIT_MSB_FIRST(realauthdata+14, t);

        des_encrypt_xdmauth((char *)auth_data + 9, realauthdata, authdatalen);
    } else {
        authdata = realauthdata;
        authdatalen = 0;
    }

    authdatalen_pad = (authdatalen + 3) & ~3;
    greeting_len = 12 + authnamelen_pad + authdatalen_pad;

    greeting = snewn(greeting_len, unsigned char);
    memset(greeting, 0, greeting_len);
    greeting[0] = endian;
    PUT_16BIT_X11(endian, greeting+2, protomajor);
    PUT_16BIT_X11(endian, greeting+4, protominor);
    PUT_16BIT_X11(endian, greeting+6, authnamelen);
    PUT_16BIT_X11(endian, greeting+8, authdatalen);
    memcpy(greeting+12, authname, authnamelen);
    memcpy(greeting+12+authnamelen_pad, authdata, authdatalen);

    smemclr(realauthdata, sizeof(realauthdata));

    *outlen = greeting_len;
    return greeting;
}
