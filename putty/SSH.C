#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#include "putty.h"
#include "tree234.h"
#include "ssh.h"
#include "sshint.h"
#include "sshgss.h"


/*
 * Packet type contexts, so that ssh2_pkt_type can correctly decode
 * the ambiguous type numbers back into the correct type strings.
 */
#define SSH2_PKTCTX_DHGROUP          0x0001
#define SSH2_PKTCTX_DHGEX            0x0002
#define SSH2_PKTCTX_KEX_MASK         0x000F
#define SSH2_PKTCTX_PUBLICKEY        0x0010
#define SSH2_PKTCTX_PASSWORD         0x0020
#define SSH2_PKTCTX_KBDINTER         0x0040
#define SSH2_PKTCTX_AUTH_MASK        0x00F0

#define SSH2_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT 1	/* 0x1 */
#define SSH2_DISCONNECT_PROTOCOL_ERROR            2	/* 0x2 */
#define SSH2_DISCONNECT_KEY_EXCHANGE_FAILED       3	/* 0x3 */
#define SSH2_DISCONNECT_HOST_AUTHENTICATION_FAILED 4	/* 0x4 */
#define SSH2_DISCONNECT_MAC_ERROR                 5	/* 0x5 */
#define SSH2_DISCONNECT_COMPRESSION_ERROR         6	/* 0x6 */
#define SSH2_DISCONNECT_SERVICE_NOT_AVAILABLE     7	/* 0x7 */
#define SSH2_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED 8	/* 0x8 */
#define SSH2_DISCONNECT_HOST_KEY_NOT_VERIFIABLE   9	/* 0x9 */
#define SSH2_DISCONNECT_CONNECTION_LOST           10	/* 0xa */
#define SSH2_DISCONNECT_BY_APPLICATION            11	/* 0xb */
#define SSH2_DISCONNECT_TOO_MANY_CONNECTIONS      12	/* 0xc */
#define SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER    13	/* 0xd */
#define SSH2_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE 14	/* 0xe */
#define SSH2_DISCONNECT_ILLEGAL_USER_NAME         15	/* 0xf */

static const char *const ssh2_disconnect_reasons[] = {
    NULL,
    "SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT",
    "SSH_DISCONNECT_PROTOCOL_ERROR",
    "SSH_DISCONNECT_KEY_EXCHANGE_FAILED",
    "SSH_DISCONNECT_HOST_AUTHENTICATION_FAILED",
    "SSH_DISCONNECT_MAC_ERROR",
    "SSH_DISCONNECT_COMPRESSION_ERROR",
    "SSH_DISCONNECT_SERVICE_NOT_AVAILABLE",
    "SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED",
    "SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE",
    "SSH_DISCONNECT_CONNECTION_LOST",
    "SSH_DISCONNECT_BY_APPLICATION",
    "SSH_DISCONNECT_TOO_MANY_CONNECTIONS",
    "SSH_DISCONNECT_AUTH_CANCELLED_BY_USER",
    "SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE",
    "SSH_DISCONNECT_ILLEGAL_USER_NAME",
};

#define SSH2_OPEN_ADMINISTRATIVELY_PROHIBITED     1	/* 0x1 */
#define SSH2_OPEN_CONNECT_FAILED                  2	/* 0x2 */
#define SSH2_OPEN_UNKNOWN_CHANNEL_TYPE            3	/* 0x3 */
#define SSH2_OPEN_RESOURCE_SHORTAGE               4	/* 0x4 */

#define SSH2_EXTENDED_DATA_STDERR                 1	/* 0x1 */

/*
 * Various remote-bug flags.
 */
#define BUG_CHOKES_ON_SSH1_IGNORE                 1
#define BUG_SSH2_HMAC                             2
#define BUG_NEEDS_SSH1_PLAIN_PASSWORD        	  4
#define BUG_CHOKES_ON_RSA	        	  8
#define BUG_SSH2_RSA_PADDING	        	 16
#define BUG_SSH2_DERIVEKEY                       32
#define BUG_SSH2_REKEY                           64
#define BUG_SSH2_PK_SESSIONID                   128

#define translate(x) if (type == x) return #x
#define translatec(x,ctx) if (type == x && (pkt_ctx & ctx)) return #x
static char *ssh1_pkt_type(int type)
{
    translate(SSH1_MSG_DISCONNECT);
    translate(SSH1_SMSG_PUBLIC_KEY);
    translate(SSH1_CMSG_SESSION_KEY);
    translate(SSH1_CMSG_USER);
    translate(SSH1_CMSG_AUTH_RSA);
    translate(SSH1_SMSG_AUTH_RSA_CHALLENGE);
    translate(SSH1_CMSG_AUTH_RSA_RESPONSE);
    translate(SSH1_CMSG_AUTH_PASSWORD);
    translate(SSH1_CMSG_REQUEST_PTY);
    translate(SSH1_CMSG_WINDOW_SIZE);
    translate(SSH1_CMSG_EXEC_SHELL);
    translate(SSH1_CMSG_EXEC_CMD);
    translate(SSH1_SMSG_SUCCESS);
    translate(SSH1_SMSG_FAILURE);
    translate(SSH1_CMSG_STDIN_DATA);
    translate(SSH1_SMSG_STDOUT_DATA);
    translate(SSH1_SMSG_STDERR_DATA);
    translate(SSH1_CMSG_EOF);
    translate(SSH1_SMSG_EXIT_STATUS);
    translate(SSH1_MSG_CHANNEL_OPEN_CONFIRMATION);
    translate(SSH1_MSG_CHANNEL_OPEN_FAILURE);
    translate(SSH1_MSG_CHANNEL_DATA);
    translate(SSH1_MSG_CHANNEL_CLOSE);
    translate(SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION);
    translate(SSH1_SMSG_X11_OPEN);
    translate(SSH1_CMSG_PORT_FORWARD_REQUEST);
    translate(SSH1_MSG_PORT_OPEN);
    translate(SSH1_CMSG_AGENT_REQUEST_FORWARDING);
    translate(SSH1_SMSG_AGENT_OPEN);
    translate(SSH1_MSG_IGNORE);
    translate(SSH1_CMSG_EXIT_CONFIRMATION);
    translate(SSH1_CMSG_X11_REQUEST_FORWARDING);
    translate(SSH1_CMSG_AUTH_RHOSTS_RSA);
    translate(SSH1_MSG_DEBUG);
    translate(SSH1_CMSG_REQUEST_COMPRESSION);
    translate(SSH1_CMSG_AUTH_TIS);
    translate(SSH1_SMSG_AUTH_TIS_CHALLENGE);
    translate(SSH1_CMSG_AUTH_TIS_RESPONSE);
    translate(SSH1_CMSG_AUTH_CCARD);
    translate(SSH1_SMSG_AUTH_CCARD_CHALLENGE);
    translate(SSH1_CMSG_AUTH_CCARD_RESPONSE);
    return "unknown";
}
static char *ssh2_pkt_type(int pkt_ctx, int type)
{
    translate(SSH2_MSG_DISCONNECT);
    translate(SSH2_MSG_IGNORE);
    translate(SSH2_MSG_UNIMPLEMENTED);
    translate(SSH2_MSG_DEBUG);
    translate(SSH2_MSG_SERVICE_REQUEST);
    translate(SSH2_MSG_SERVICE_ACCEPT);
    translate(SSH2_MSG_KEXINIT);
    translate(SSH2_MSG_NEWKEYS);
    translatec(SSH2_MSG_KEXDH_INIT, SSH2_PKTCTX_DHGROUP);
    translatec(SSH2_MSG_KEXDH_REPLY, SSH2_PKTCTX_DHGROUP);
    translatec(SSH2_MSG_KEX_DH_GEX_REQUEST, SSH2_PKTCTX_DHGEX);
    translatec(SSH2_MSG_KEX_DH_GEX_GROUP, SSH2_PKTCTX_DHGEX);
    translatec(SSH2_MSG_KEX_DH_GEX_INIT, SSH2_PKTCTX_DHGEX);
    translatec(SSH2_MSG_KEX_DH_GEX_REPLY, SSH2_PKTCTX_DHGEX);
    translate(SSH2_MSG_USERAUTH_REQUEST);
    translate(SSH2_MSG_USERAUTH_FAILURE);
    translate(SSH2_MSG_USERAUTH_SUCCESS);
    translate(SSH2_MSG_USERAUTH_BANNER);
    translatec(SSH2_MSG_USERAUTH_PK_OK, SSH2_PKTCTX_PUBLICKEY);
    translatec(SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ, SSH2_PKTCTX_PASSWORD);
    translatec(SSH2_MSG_USERAUTH_INFO_REQUEST, SSH2_PKTCTX_KBDINTER);
    translatec(SSH2_MSG_USERAUTH_INFO_RESPONSE, SSH2_PKTCTX_KBDINTER);
    translatec(SSH2_MSG_USERAUTH_GSSAPI_RESPONSE, SSH2_PKTCTX_GSSAPI);
    translatec(SSH2_MSG_USERAUTH_GSSAPI_TOKEN, SSH2_PKTCTX_GSSAPI);
    translatec(SSH2_MSG_USERAUTH_GSSAPI_EXCHANGE_COMPLETE, SSH2_PKTCTX_GSSAPI);
    translatec(SSH2_MSG_USERAUTH_GSSAPI_ERROR, SSH2_PKTCTX_GSSAPI);
    translatec(SSH2_MSG_USERAUTH_GSSAPI_ERRTOK, SSH2_PKTCTX_GSSAPI);
    translatec(SSH2_MSG_USERAUTH_GSSAPI_MIC, SSH2_PKTCTX_GSSAPI);
    translate(SSH2_MSG_GLOBAL_REQUEST);
    translate(SSH2_MSG_REQUEST_SUCCESS);
    translate(SSH2_MSG_REQUEST_FAILURE);
    translate(SSH2_MSG_CHANNEL_OPEN);
    translate(SSH2_MSG_CHANNEL_OPEN_CONFIRMATION);
    translate(SSH2_MSG_CHANNEL_OPEN_FAILURE);
    translate(SSH2_MSG_CHANNEL_WINDOW_ADJUST);
    translate(SSH2_MSG_CHANNEL_DATA);
    translate(SSH2_MSG_CHANNEL_EXTENDED_DATA);
    translate(SSH2_MSG_CHANNEL_EOF);
    translate(SSH2_MSG_CHANNEL_CLOSE);
    translate(SSH2_MSG_CHANNEL_REQUEST);
    translate(SSH2_MSG_CHANNEL_SUCCESS);
    translate(SSH2_MSG_CHANNEL_FAILURE);
    return "unknown";
}
#undef translate
#undef translatec

#define GET_32BIT(cp) \
    (((unsigned long)(unsigned char)(cp)[0] << 24) | \
    ((unsigned long)(unsigned char)(cp)[1] << 16) | \
    ((unsigned long)(unsigned char)(cp)[2] << 8) | \
    ((unsigned long)(unsigned char)(cp)[3]))

#define PUT_32BIT(cp, value) { \
    (cp)[0] = (unsigned char)((value) >> 24); \
    (cp)[1] = (unsigned char)((value) >> 16); \
    (cp)[2] = (unsigned char)((value) >> 8); \
    (cp)[3] = (unsigned char)(value); }

/* Enumeration values for fields in SSH-1 packets */
enum {
    PKT_END, PKT_INT, PKT_CHAR, PKT_DATA, PKT_STR, PKT_BIGNUM,
    /* These values are for communicating relevant semantics of
     * fields to the packet logging code. */
    PKTT_OTHER, PKTT_PASSWORD, PKTT_DATA
};

/*
 * Coroutine mechanics for the sillier bits of the code. If these
 * macros look impenetrable to you, you might find it helpful to
 * read
 * 
 *   http://www.chiark.greenend.org.uk/~sgtatham/coroutines.html
 * 
 * which explains the theory behind these macros.
 * 
 * In particular, if you are getting `case expression not constant'
 * errors when building with MS Visual Studio, this is because MS's
 * Edit and Continue debugging feature causes their compiler to
 * violate ANSI C. To disable Edit and Continue debugging:
 * 
 *  - right-click ssh.c in the FileView
 *  - click Settings
 *  - select the C/C++ tab and the General category
 *  - under `Debug info:', select anything _other_ than `Program
 *    Database for Edit and Continue'.
 */
#define crBegin(v)	{ int *crLine = &v; switch(v) { case 0:;
#define crState(t) \
    struct t *s; \
    if (!ssh->t) ssh->t = snew(struct t); \
    s = ssh->t;
#define crFinish(z)	} *crLine = 0; return (z); }
#define crFinishV	} *crLine = 0; return; }
#define crReturn(z)	\
	do {\
	    *crLine =__LINE__; return (z); case __LINE__:;\
	} while (0)
#define crReturnV	\
	do {\
	    *crLine=__LINE__; return; case __LINE__:;\
	} while (0)
#define crStop(z)	do{ *crLine = 0; return (z); }while(0)
#define crStopV		do{ *crLine = 0; return; }while(0)
#define crWaitUntil(c)	do { crReturn(0); } while (!(c))
#define crWaitUntilV(c)	do { crReturnV; } while (!(c))

static unsigned char *ssh2_mpint_fmt(Bignum b, int *len);
static int do_ssh1_login(Ssh ssh, unsigned char *in, int inlen,
			 struct Packet *pktin);
static void do_ssh2_authconn(Ssh ssh, unsigned char *in, int inlen,
			     struct Packet *pktin);

/*
 * Buffer management constants. There are several of these for
 * various different purposes:
 * 
 *  - SSH1_BUFFER_LIMIT is the amount of backlog that must build up
 *    on a local data stream before we throttle the whole SSH
 *    connection (in SSH-1 only). Throttling the whole connection is
 *    pretty drastic so we set this high in the hope it won't
 *    happen very often.
 * 
 *  - SSH_MAX_BACKLOG is the amount of backlog that must build up
 *    on the SSH connection itself before we defensively throttle
 *    _all_ local data streams. This is pretty drastic too (though
 *    thankfully unlikely in SSH-2 since the window mechanism should
 *    ensure that the server never has any need to throttle its end
 *    of the connection), so we set this high as well.
 * 
 *  - OUR_V2_WINSIZE is the maximum window size we present on SSH-2
 *    channels.
 */

#define SSH1_BUFFER_LIMIT 32768
#define SSH_MAX_BACKLOG 32768
#ifdef MPEXT
#define OUR_V2_MAXPKT 0x4000UL
#define OUR_V2_WINSIZE (OUR_V2_MAXPKT * 4)
#else
#define OUR_V2_WINSIZE 16384
#define OUR_V2_MAXPKT 0x4000UL
#endif

const static struct ssh_signkey *hostkey_algs[] = { &ssh_rsa, &ssh_dss };

static void *nullmac_make_context(void)
{
    return NULL;
}
static void nullmac_free_context(void *handle)
{
}
static void nullmac_key(void *handle, unsigned char *key)
{
}
static void nullmac_generate(void *handle, unsigned char *blk, int len,
			     unsigned long seq)
{
}
static int nullmac_verify(void *handle, unsigned char *blk, int len,
			  unsigned long seq)
{
    return 1;
}
const static struct ssh_mac ssh_mac_none = {
    nullmac_make_context, nullmac_free_context, nullmac_key,
    nullmac_generate, nullmac_verify, "none", 0
};
const static struct ssh_mac *macs[] = {
    &ssh_sha1, &ssh_md5, &ssh_mac_none
};
const static struct ssh_mac *buggymacs[] = {
    &ssh_sha1_buggy, &ssh_md5, &ssh_mac_none
};

static void *ssh_comp_none_init(void)
{
    return NULL;
}
static void ssh_comp_none_cleanup(void *handle)
{
}
static int ssh_comp_none_block(void *handle, unsigned char *block, int len,
			       unsigned char **outblock, int *outlen)
{
    return 0;
}
static int ssh_comp_none_disable(void *handle)
{
    return 0;
}
const static struct ssh_compress ssh_comp_none = {
    "none",
    ssh_comp_none_init, ssh_comp_none_cleanup, ssh_comp_none_block,
    ssh_comp_none_init, ssh_comp_none_cleanup, ssh_comp_none_block,
    ssh_comp_none_disable, NULL
};
extern const struct ssh_compress ssh_zlib;
const static struct ssh_compress *compressions[] = {
    &ssh_zlib, &ssh_comp_none
};

enum {				       /* channel types */
    CHAN_MAINSESSION,
    CHAN_X11,
    CHAN_AGENT,
    CHAN_SOCKDATA,
    CHAN_SOCKDATA_DORMANT	       /* one the remote hasn't confirmed */
};

/*
 * 2-3-4 tree storing channels.
 */
struct ssh_channel {
    Ssh ssh;			       /* pointer back to main context */
    unsigned remoteid, localid;
    int type;
    /* True if we opened this channel but server hasn't confirmed. */
    int halfopen;
    /*
     * In SSH-1, this value contains four bits:
     * 
     *   1   We have sent SSH1_MSG_CHANNEL_CLOSE.
     *   2   We have sent SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION.
     *   4   We have received SSH1_MSG_CHANNEL_CLOSE.
     *   8   We have received SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION.
     * 
     * A channel is completely finished with when all four bits are set.
     */
    int closes;
    union {
	struct ssh1_data_channel {
	    int throttling;
	} v1;
	struct ssh2_data_channel {
	    bufchain outbuffer;
	    unsigned remwindow, remmaxpkt;
	    unsigned locwindow;
	} v2;
    } v;
    union {
	struct ssh_agent_channel {
	    unsigned char *message;
	    unsigned char msglen[4];
	    unsigned lensofar, totallen;
	} a;
	struct ssh_x11_channel {
	    Socket s;
	} x11;
	struct ssh_pfd_channel {
	    Socket s;
	} pfd;
    } u;
};

/*
 * 2-3-4 tree storing remote->local port forwardings. SSH-1 and SSH-2
 * use this structure in different ways, reflecting SSH-2's
 * altogether saner approach to port forwarding.
 * 
 * In SSH-1, you arrange a remote forwarding by sending the server
 * the remote port number, and the local destination host:port.
 * When a connection comes in, the server sends you back that
 * host:port pair, and you connect to it. This is a ready-made
 * security hole if you're not on the ball: a malicious server
 * could send you back _any_ host:port pair, so if you trustingly
 * connect to the address it gives you then you've just opened the
 * entire inside of your corporate network just by connecting
 * through it to a dodgy SSH server. Hence, we must store a list of
 * host:port pairs we _are_ trying to forward to, and reject a
 * connection request from the server if it's not in the list.
 * 
 * In SSH-2, each side of the connection minds its own business and
 * doesn't send unnecessary information to the other. You arrange a
 * remote forwarding by sending the server just the remote port
 * number. When a connection comes in, the server tells you which
 * of its ports was connected to; and _you_ have to remember what
 * local host:port pair went with that port number.
 * 
 * Hence, in SSH-1 this structure is indexed by destination
 * host:port pair, whereas in SSH-2 it is indexed by source port.
 */
struct ssh_portfwd; /* forward declaration */

struct ssh_rportfwd {
    unsigned sport, dport;
    char dhost[256];
    char *sportdesc;
    struct ssh_portfwd *pfrec;
};
#define free_rportfwd(pf) ( \
    ((pf) ? (sfree((pf)->sportdesc)) : (void)0 ), sfree(pf) )

/*
 * Separately to the rportfwd tree (which is for looking up port
 * open requests from the server), a tree of _these_ structures is
 * used to keep track of all the currently open port forwardings,
 * so that we can reconfigure in mid-session if the user requests
 * it.
 */
struct ssh_portfwd {
    enum { DESTROY, KEEP, CREATE } status;
    int type;
    unsigned sport, dport;
    char *saddr, *daddr;
    char *sserv, *dserv;
    struct ssh_rportfwd *remote;
    int addressfamily;
    void *local;
};
#define free_portfwd(pf) ( \
    ((pf) ? (sfree((pf)->saddr), sfree((pf)->daddr), \
	     sfree((pf)->sserv), sfree((pf)->dserv)) : (void)0 ), sfree(pf) )

static void ssh1_protocol(Ssh ssh, void *vin, int inlen,
			  struct Packet *pktin);
static void ssh2_protocol(Ssh ssh, void *vin, int inlen,
			  struct Packet *pktin);
static void ssh1_protocol_setup(Ssh ssh);
static void ssh2_protocol_setup(Ssh ssh);
static void ssh_size(void *handle, int width, int height);
static void ssh_special(void *handle, Telnet_Special);
static int ssh2_try_send(struct ssh_channel *c);
static void ssh2_add_channel_data(struct ssh_channel *c, char *buf, int len);
static void ssh_throttle_all(Ssh ssh, int enable, int bufsize);
static void ssh2_set_window(struct ssh_channel *c, unsigned newwin);
static int ssh_sendbuffer(void *handle);
static void ssh2_timer(void *ctx, long now);
static int do_ssh2_transport(Ssh ssh, void *vin, int inlen,
			     struct Packet *pktin);

struct queued_handler {
    int msg1, msg2;
    chandler_fn_t handler;
    void *ctx;
    struct queued_handler *next;
};

/* logevent, only printf-formatted. */
void logeventf(Ssh ssh, const char *fmt, ...)
{
    va_list ap;
    char *buf;

    va_start(ap, fmt);
    buf = dupvprintf(fmt, ap);
    va_end(ap);
    logevent(buf);
    sfree(buf);
}

/* Functions to leave bits out of the SSH packet log file. */

static void dont_log_password(Ssh ssh, struct Packet *pkt, int blanktype)
{
    if (ssh->cfg.logomitpass)
	pkt->logmode = blanktype;
}

static void dont_log_data(Ssh ssh, struct Packet *pkt, int blanktype)
{
    if (ssh->cfg.logomitdata)
	pkt->logmode = blanktype;
}

static void end_log_omission(Ssh ssh, struct Packet *pkt)
{
    pkt->logmode = PKTLOG_EMIT;
}

static int ssh_channelcmp(void *av, void *bv)
{
    struct ssh_channel *a = (struct ssh_channel *) av;
    struct ssh_channel *b = (struct ssh_channel *) bv;
    if (a->localid < b->localid)
	return -1;
    if (a->localid > b->localid)
	return +1;
    return 0;
}
static int ssh_channelfind(void *av, void *bv)
{
    unsigned *a = (unsigned *) av;
    struct ssh_channel *b = (struct ssh_channel *) bv;
    if (*a < b->localid)
	return -1;
    if (*a > b->localid)
	return +1;
    return 0;
}

static int ssh_rportcmp_ssh1(void *av, void *bv)
{
    struct ssh_rportfwd *a = (struct ssh_rportfwd *) av;
    struct ssh_rportfwd *b = (struct ssh_rportfwd *) bv;
    int i;
    if ( (i = strcmp(a->dhost, b->dhost)) != 0)
	return i < 0 ? -1 : +1;
    if (a->dport > b->dport)
	return +1;
    if (a->dport < b->dport)
	return -1;
    return 0;
}

static int ssh_rportcmp_ssh2(void *av, void *bv)
{
    struct ssh_rportfwd *a = (struct ssh_rportfwd *) av;
    struct ssh_rportfwd *b = (struct ssh_rportfwd *) bv;

    if (a->sport > b->sport)
	return +1;
    if (a->sport < b->sport)
	return -1;
    return 0;
}

/*
 * Special form of strcmp which can cope with NULL inputs. NULL is
 * defined to sort before even the empty string.
 */
static int nullstrcmp(const char *a, const char *b)
{
    if (a == NULL && b == NULL)
	return 0;
    if (a == NULL)
	return -1;
    if (b == NULL)
	return +1;
    return strcmp(a, b);
}

static int ssh_portcmp(void *av, void *bv)
{
    struct ssh_portfwd *a = (struct ssh_portfwd *) av;
    struct ssh_portfwd *b = (struct ssh_portfwd *) bv;
    int i;
    if (a->type > b->type)
	return +1;
    if (a->type < b->type)
	return -1;
    if (a->addressfamily > b->addressfamily)
	return +1;
    if (a->addressfamily < b->addressfamily)
	return -1;
    if ( (i = nullstrcmp(a->saddr, b->saddr)) != 0)
	return i < 0 ? -1 : +1;
    if (a->sport > b->sport)
	return +1;
    if (a->sport < b->sport)
	return -1;
    if (a->type != 'D') {
	if ( (i = nullstrcmp(a->daddr, b->daddr)) != 0)
	    return i < 0 ? -1 : +1;
	if (a->dport > b->dport)
	    return +1;
	if (a->dport < b->dport)
	    return -1;
    }
    return 0;
}

static int alloc_channel_id(Ssh ssh)
{
    const unsigned CHANNEL_NUMBER_OFFSET = 256;
    unsigned low, high, mid;
    int tsize;
    struct ssh_channel *c;

    /*
     * First-fit allocation of channel numbers: always pick the
     * lowest unused one. To do this, binary-search using the
     * counted B-tree to find the largest channel ID which is in a
     * contiguous sequence from the beginning. (Precisely
     * everything in that sequence must have ID equal to its tree
     * index plus CHANNEL_NUMBER_OFFSET.)
     */
    tsize = count234(ssh->channels);

    low = -1;
    high = tsize;
    while (high - low > 1) {
	mid = (high + low) / 2;
	c = index234(ssh->channels, mid);
	if (c->localid == mid + CHANNEL_NUMBER_OFFSET)
	    low = mid;		       /* this one is fine */
	else
	    high = mid;		       /* this one is past it */
    }
    /*
     * Now low points to either -1, or the tree index of the
     * largest ID in the initial sequence.
     */
    {
	unsigned i = low + 1 + CHANNEL_NUMBER_OFFSET;
	assert(NULL == find234(ssh->channels, &i, ssh_channelfind));
    }
    return low + 1 + CHANNEL_NUMBER_OFFSET;
}

void c_write(Ssh ssh, const char *buf, int len)
{
    if ((flags & FLAG_STDERR)) {
	int i;
	for (i = 0; i < len; i++)
	    if (buf[i] != '\r')
		fputc(buf[i], stderr);
	return;
    }
    from_backend(ssh->frontend, 1, buf, len
#ifdef MPEXT
    , 0
#endif
    );
}

void c_write_untrusted(Ssh ssh, const char *buf, int len)
{
    int i;
    for (i = 0; i < len; i++) {
	if (buf[i] == '\n')
#ifdef MPEXT
	    from_backend(ssh->frontend, 1, "\r\n", 2, -1);
#else
	    c_write(ssh, "\r\n", 2);
#endif
	else if ((buf[i] & 0x60) || (buf[i] == '\r'))
#ifdef MPEXT
	    from_backend(ssh->frontend, 1, buf + i, 1, -1);
#else
	    c_write(ssh, buf + i, 1);
#endif
    }
}

void c_write_str(Ssh ssh, const char *buf)
{
    c_write(ssh, buf, strlen(buf));
}

static void ssh_free_packet(struct Packet *pkt)
{
    sfree(pkt->data);
    sfree(pkt);
}
static struct Packet *ssh_new_packet(void)
{
    struct Packet *pkt = snew(struct Packet);

    pkt->data = NULL;
    pkt->maxlen = 0;
    pkt->logmode = PKTLOG_EMIT;
    pkt->nblanks = 0;
    pkt->blanks = NULL;

    return pkt;
}

/*
 * Collect incoming data in the incoming packet buffer.
 * Decipher and verify the packet when it is completely read.
 * Drop SSH1_MSG_DEBUG and SSH1_MSG_IGNORE packets.
 * Update the *data and *datalen variables.
 * Return a Packet structure when a packet is completed.
 */
static struct Packet *ssh1_rdpkt(Ssh ssh, unsigned char **data, int *datalen)
{
    struct rdpkt1_state_tag *st = &ssh->rdpkt1_state;

    crBegin(ssh->ssh1_rdpkt_crstate);

    st->pktin = ssh_new_packet();

    st->pktin->type = 0;
    st->pktin->length = 0;

    for (st->i = st->len = 0; st->i < 4; st->i++) {
	while ((*datalen) == 0)
	    crReturn(NULL);
	st->len = (st->len << 8) + **data;
	(*data)++, (*datalen)--;
    }

    st->pad = 8 - (st->len % 8);
    st->biglen = st->len + st->pad;
    st->pktin->length = st->len - 5;

    if (st->biglen < 0) {
        bombout(("Extremely large packet length from server suggests"
		 " data stream corruption"));
	ssh_free_packet(st->pktin);
        crStop(NULL);
    }

    st->pktin->maxlen = st->biglen;
    st->pktin->data = snewn(st->biglen + APIEXTRA, unsigned char);

    st->to_read = st->biglen;
    st->p = st->pktin->data;
    while (st->to_read > 0) {
	st->chunk = st->to_read;
	while ((*datalen) == 0)
	    crReturn(NULL);
	if (st->chunk > (*datalen))
	    st->chunk = (*datalen);
	memcpy(st->p, *data, st->chunk);
	*data += st->chunk;
	*datalen -= st->chunk;
	st->p += st->chunk;
	st->to_read -= st->chunk;
    }

    if (ssh->cipher && detect_attack(ssh->crcda_ctx, st->pktin->data,
				     st->biglen, NULL)) {
        bombout(("Network attack (CRC compensation) detected!"));
	ssh_free_packet(st->pktin);
        crStop(NULL);
    }

    if (ssh->cipher)
	ssh->cipher->decrypt(ssh->v1_cipher_ctx, st->pktin->data, st->biglen);

    st->realcrc = crc32_compute(st->pktin->data, st->biglen - 4);
    st->gotcrc = GET_32BIT(st->pktin->data + st->biglen - 4);
    if (st->gotcrc != st->realcrc) {
	bombout(("Incorrect CRC received on packet"));
	ssh_free_packet(st->pktin);
	crStop(NULL);
    }

    st->pktin->body = st->pktin->data + st->pad + 1;
    st->pktin->savedpos = 0;

    if (ssh->v1_compressing) {
	unsigned char *decompblk;
	int decomplen;
	if (!zlib_decompress_block(ssh->sc_comp_ctx,
				   st->pktin->body - 1, st->pktin->length + 1,
				   &decompblk, &decomplen)) {
	    bombout(("Zlib decompression encountered invalid data"));
	    ssh_free_packet(st->pktin);
	    crStop(NULL);
	}

	if (st->pktin->maxlen < st->pad + decomplen) {
	    st->pktin->maxlen = st->pad + decomplen;
	    st->pktin->data = sresize(st->pktin->data,
				      st->pktin->maxlen + APIEXTRA,
				      unsigned char);
	    st->pktin->body = st->pktin->data + st->pad + 1;
	}

	memcpy(st->pktin->body - 1, decompblk, decomplen);
	sfree(decompblk);
	st->pktin->length = decomplen - 1;
    }

    st->pktin->type = st->pktin->body[-1];

    /*
     * Log incoming packet, possibly omitting sensitive fields.
     */
    if (ssh->logctx) {
	int nblanks = 0;
	struct logblank_t blank;
	if (ssh->cfg.logomitdata) {
	    int do_blank = FALSE, blank_prefix = 0;
	    /* "Session data" packets - omit the data field */
	    if ((st->pktin->type == SSH1_SMSG_STDOUT_DATA) ||
		(st->pktin->type == SSH1_SMSG_STDERR_DATA)) {
		do_blank = TRUE; blank_prefix = 0;
	    } else if (st->pktin->type == SSH1_MSG_CHANNEL_DATA) {
		do_blank = TRUE; blank_prefix = 4;
	    }
	    if (do_blank) {
		blank.offset = blank_prefix;
		blank.len = st->pktin->length;
		blank.type = PKTLOG_OMIT;
		nblanks = 1;
	    }
	}
	log_packet(ssh->logctx,
		   PKT_INCOMING, st->pktin->type,
		   ssh1_pkt_type(st->pktin->type),
		   st->pktin->body, st->pktin->length,
		   nblanks, &blank);
    }

    crFinish(st->pktin);
}

static struct Packet *ssh2_rdpkt(Ssh ssh, unsigned char **data, int *datalen)
{
    struct rdpkt2_state_tag *st = &ssh->rdpkt2_state;

    crBegin(ssh->ssh2_rdpkt_crstate);

    st->pktin = ssh_new_packet();

    st->pktin->type = 0;
    st->pktin->length = 0;
    if (ssh->sccipher)
	st->cipherblk = ssh->sccipher->blksize;
    else
	st->cipherblk = 8;
    if (st->cipherblk < 8)
	st->cipherblk = 8;

    st->pktin->data = snewn(st->cipherblk + APIEXTRA, unsigned char);

    /*
     * Acquire and decrypt the first block of the packet. This will
     * contain the length and padding details.
     */
    for (st->i = st->len = 0; st->i < st->cipherblk; st->i++) {
	while ((*datalen) == 0)
	    crReturn(NULL);
	st->pktin->data[st->i] = *(*data)++;
	(*datalen)--;
    }

    if (ssh->sccipher)
	ssh->sccipher->decrypt(ssh->sc_cipher_ctx,
			       st->pktin->data, st->cipherblk);

    /*
     * Now get the length and padding figures.
     */
    st->len = GET_32BIT(st->pktin->data);
    st->pad = st->pktin->data[4];

    /*
     * _Completely_ silly lengths should be stomped on before they
     * do us any more damage.
     */
    if (st->len < 0 || st->pad < 0 || st->len + st->pad < 0) {
	bombout(("Incoming packet was garbled on decryption"));
	ssh_free_packet(st->pktin);
	crStop(NULL);
    }

    /*
     * This enables us to deduce the payload length.
     */
    st->payload = st->len - st->pad - 1;

    st->pktin->length = st->payload + 5;

    /*
     * So now we can work out the total packet length.
     */
    st->packetlen = st->len + 4;
    st->maclen = ssh->scmac ? ssh->scmac->len : 0;

    /*
     * Allocate memory for the rest of the packet.
     */
    st->pktin->maxlen = st->packetlen + st->maclen;
    st->pktin->data = sresize(st->pktin->data,
			      st->pktin->maxlen + APIEXTRA,
			      unsigned char);

    /*
     * Read and decrypt the remainder of the packet.
     */
    for (st->i = st->cipherblk; st->i < st->packetlen + st->maclen;
	 st->i++) {
	while ((*datalen) == 0)
	    crReturn(NULL);
	st->pktin->data[st->i] = *(*data)++;
	(*datalen)--;
    }
    /* Decrypt everything _except_ the MAC. */
    if (ssh->sccipher)
	ssh->sccipher->decrypt(ssh->sc_cipher_ctx,
			       st->pktin->data + st->cipherblk,
			       st->packetlen - st->cipherblk);

    st->pktin->encrypted_len = st->packetlen;

    /*
     * Check the MAC.
     */
    if (ssh->scmac
	&& !ssh->scmac->verify(ssh->sc_mac_ctx, st->pktin->data, st->len + 4,
			       st->incoming_sequence)) {
	bombout(("Incorrect MAC received on packet"));
	ssh_free_packet(st->pktin);
	crStop(NULL);
    }

    st->pktin->sequence = st->incoming_sequence++;

    /*
     * Decompress packet payload.
     */
    {
	unsigned char *newpayload;
	int newlen;
	if (ssh->sccomp &&
	    ssh->sccomp->decompress(ssh->sc_comp_ctx,
				    st->pktin->data + 5, st->pktin->length - 5,
				    &newpayload, &newlen)) {
	    if (st->pktin->maxlen < newlen + 5) {
		st->pktin->maxlen = newlen + 5;
		st->pktin->data = sresize(st->pktin->data,
					  st->pktin->maxlen + APIEXTRA,
					  unsigned char);
	    }
	    st->pktin->length = 5 + newlen;
	    memcpy(st->pktin->data + 5, newpayload, newlen);
	    sfree(newpayload);
	}
    }

    st->pktin->savedpos = 6;
    st->pktin->body = st->pktin->data;
    st->pktin->type = st->pktin->data[5];

    /*
     * Log incoming packet, possibly omitting sensitive fields.
     */
    if (ssh->logctx) {
	int nblanks = 0;
	struct logblank_t blank;
	if (ssh->cfg.logomitdata) {
	    int do_blank = FALSE, blank_prefix = 0;
	    /* "Session data" packets - omit the data field */
	    if (st->pktin->type == SSH2_MSG_CHANNEL_DATA) {
		do_blank = TRUE; blank_prefix = 4;
	    } else if (st->pktin->type == SSH2_MSG_CHANNEL_EXTENDED_DATA) {
		do_blank = TRUE; blank_prefix = 8;
	    }
	    if (do_blank) {
		blank.offset = blank_prefix;
		blank.len = (st->pktin->length-6) - blank_prefix;
		blank.type = PKTLOG_OMIT;
		nblanks = 1;
	    }
	}
	log_packet(ssh->logctx, PKT_INCOMING, st->pktin->type,
		   ssh2_pkt_type(ssh->pkt_ctx, st->pktin->type),
		   st->pktin->data+6, st->pktin->length-6,
		   nblanks, &blank);
    }

    crFinish(st->pktin);
}

static void ssh1_pktout_size(struct Packet *pkt, int len)
{
    int pad, biglen;

    len += 5;			       /* type and CRC */
    pad = 8 - (len % 8);
    biglen = len + pad;

    pkt->length = len - 5;
    if (pkt->maxlen < biglen) {
	pkt->maxlen = biglen;
	pkt->data = sresize(pkt->data, biglen + 4 + APIEXTRA, unsigned char);
    }
    pkt->body = pkt->data + 4 + pad + 1;
}

static struct Packet *s_wrpkt_start(int type, int len)
{
    struct Packet *pkt = ssh_new_packet();
    ssh1_pktout_size(pkt, len);
    pkt->type = type;
    /* Initialise log omission state */
    pkt->nblanks = 0;
    pkt->blanks = NULL;
    return pkt;
}

static int s_wrpkt_prepare(Ssh ssh, struct Packet *pkt)
{
    int pad, biglen, i;
    unsigned long crc;
#ifdef __SC__
    /*
     * XXX various versions of SC (including 8.8.4) screw up the
     * register allocation in this function and use the same register
     * (D6) for len and as a temporary, with predictable results.  The
     * following sledgehammer prevents this.
     */
    volatile
#endif
    int len;

    pkt->body[-1] = pkt->type;

    if (ssh->logctx)
	log_packet(ssh->logctx, PKT_OUTGOING, pkt->type,
		   ssh1_pkt_type(pkt->type),
		   pkt->body, pkt->length,
		   pkt->nblanks, pkt->blanks);
    sfree(pkt->blanks); pkt->blanks = NULL;
    pkt->nblanks = 0;

    if (ssh->v1_compressing) {
	unsigned char *compblk;
	int complen;
	zlib_compress_block(ssh->cs_comp_ctx,
			    pkt->body - 1, pkt->length + 1,
			    &compblk, &complen);
	ssh1_pktout_size(pkt, complen - 1);
	memcpy(pkt->body - 1, compblk, complen);
	sfree(compblk);
    }

    len = pkt->length + 5;	       /* type and CRC */
    pad = 8 - (len % 8);
    biglen = len + pad;

    for (i = 0; i < pad; i++)
	pkt->data[i + 4] = random_byte();
    crc = crc32_compute(pkt->data + 4, biglen - 4);
    PUT_32BIT(pkt->data + biglen, crc);
    PUT_32BIT(pkt->data, len);

    if (ssh->cipher)
	ssh->cipher->encrypt(ssh->v1_cipher_ctx, pkt->data + 4, biglen);

    return biglen + 4;
}

static void s_wrpkt(Ssh ssh, struct Packet *pkt)
{
    int len, backlog;
    len = s_wrpkt_prepare(ssh, pkt);
    backlog = sk_write(ssh->s, (char *)pkt->data, len);
    if (backlog > SSH_MAX_BACKLOG)
	ssh_throttle_all(ssh, 1, backlog);
}

static void s_wrpkt_defer(Ssh ssh, struct Packet *pkt)
{
    int len;
    len = s_wrpkt_prepare(ssh, pkt);
    if (ssh->deferred_len + len > ssh->deferred_size) {
	ssh->deferred_size = ssh->deferred_len + len + 128;
	ssh->deferred_send_data = sresize(ssh->deferred_send_data,
					  ssh->deferred_size,
					  unsigned char);
    }
    memcpy(ssh->deferred_send_data + ssh->deferred_len, pkt->data, len);
    ssh->deferred_len += len;
}

/*
 * Construct a packet with the specified contents.
 */
static struct Packet *construct_packet(Ssh ssh, int pkttype,
				       va_list ap1, va_list ap2)
{
    unsigned char *p, *argp, argchar;
    unsigned long argint;
    int pktlen, argtype, arglen;
    Bignum bn;
    struct Packet *pkt;

    pktlen = 0;
    while ((argtype = va_arg(ap1, int)) != PKT_END) {
	switch (argtype) {
	  case PKT_INT:
	    (void) va_arg(ap1, int);
	    pktlen += 4;
	    break;
	  case PKT_CHAR:
	    (void) va_arg(ap1, int);
	    pktlen++;
	    break;
	  case PKT_DATA:
	    (void) va_arg(ap1, unsigned char *);
	    arglen = va_arg(ap1, int);
	    pktlen += arglen;
	    break;
	  case PKT_STR:
	    argp = va_arg(ap1, unsigned char *);
	    arglen = strlen((char *)argp);
	    pktlen += 4 + arglen;
	    break;
	  case PKT_BIGNUM:
	    bn = va_arg(ap1, Bignum);
	    pktlen += ssh1_bignum_length(bn);
	    break;
	  case PKTT_PASSWORD:
	  case PKTT_DATA:
	  case PKTT_OTHER:
	    /* ignore this pass */
	    break;
	  default:
	    assert(0);
	}
    }

    pkt = s_wrpkt_start(pkttype, pktlen);
    p = pkt->body;

    while ((argtype = va_arg(ap2, int)) != PKT_END) {
	int offset = p - pkt->body, len = 0;
	switch (argtype) {
	  /* Actual fields in the packet */
	  case PKT_INT:
	    argint = va_arg(ap2, int);
	    PUT_32BIT(p, argint);
	    len = 4;
	    break;
	  case PKT_CHAR:
	    argchar = (unsigned char) va_arg(ap2, int);
	    *p = argchar;
	    len = 1;
	    break;
	  case PKT_DATA:
	    argp = va_arg(ap2, unsigned char *);
	    arglen = va_arg(ap2, int);
	    memcpy(p, argp, arglen);
	    len = arglen;
	    break;
	  case PKT_STR:
	    argp = va_arg(ap2, unsigned char *);
	    arglen = strlen((char *)argp);
	    PUT_32BIT(p, arglen);
	    memcpy(p + 4, argp, arglen);
	    len = arglen + 4;
	    break;
	  case PKT_BIGNUM:
	    bn = va_arg(ap2, Bignum);
	    len = ssh1_write_bignum(p, bn);
	    break;
	  /* Tokens for modifications to packet logging */
	  case PKTT_PASSWORD:
	    dont_log_password(ssh, pkt, PKTLOG_BLANK);
	    break;
	  case PKTT_DATA:
	    dont_log_data(ssh, pkt, PKTLOG_OMIT);
	    break;
	  case PKTT_OTHER:
	    end_log_omission(ssh, pkt);
	    break;
	}
	p += len;
	/* Deal with logfile omission, if required. */
	if (len && (pkt->logmode != PKTLOG_EMIT)) {
	    pkt->nblanks++;
	    pkt->blanks = sresize(pkt->blanks, pkt->nblanks,
				  struct logblank_t);
	    pkt->blanks[pkt->nblanks-1].offset = offset;
	    pkt->blanks[pkt->nblanks-1].len    = len;
	    pkt->blanks[pkt->nblanks-1].type   = pkt->logmode;
	}
    }

    return pkt;
}

static void send_packet(Ssh ssh, int pkttype, ...)
{
    struct Packet *pkt;
    va_list ap1, ap2;
    va_start(ap1, pkttype);
    va_start(ap2, pkttype);
    pkt = construct_packet(ssh, pkttype, ap1, ap2);
    va_end(ap2);
    va_end(ap1);
    s_wrpkt(ssh, pkt);
    ssh_free_packet(pkt);
}

static void defer_packet(Ssh ssh, int pkttype, ...)
{
    struct Packet *pkt;
    va_list ap1, ap2;
    va_start(ap1, pkttype);
    va_start(ap2, pkttype);
    pkt = construct_packet(ssh, pkttype, ap1, ap2);
    va_end(ap2);
    va_end(ap1);
    s_wrpkt_defer(ssh, pkt);
    ssh_free_packet(pkt);
}

static int ssh_versioncmp(char *a, char *b)
{
    char *ae, *be;
    unsigned long av, bv;

    av = strtoul(a, &ae, 10);
    bv = strtoul(b, &be, 10);
    if (av != bv)
	return (av < bv ? -1 : +1);
    if (*ae == '.')
	ae++;
    if (*be == '.')
	be++;
    av = strtoul(ae, &ae, 10);
    bv = strtoul(be, &be, 10);
    if (av != bv)
	return (av < bv ? -1 : +1);
    return 0;
}

/*
 * Utility routines for putting an SSH-protocol `string' and
 * `uint32' into a SHA state.
 */
#include <stdio.h>
static void sha_string(SHA_State * s, void *str, int len)
{
    unsigned char lenblk[4];
    PUT_32BIT(lenblk, len);
    SHA_Bytes(s, lenblk, 4);
    SHA_Bytes(s, str, len);
}

static void sha_uint32(SHA_State * s, unsigned i)
{
    unsigned char intblk[4];
    PUT_32BIT(intblk, i);
    SHA_Bytes(s, intblk, 4);
}

/*
 * SSH-2 packet construction functions.
 */
static void ssh2_pkt_ensure(struct Packet *pkt, int length)
{
    if (pkt->maxlen < length) {
	pkt->maxlen = length + 256;
	pkt->data = sresize(pkt->data, pkt->maxlen + APIEXTRA, unsigned char);
    }
}
static void ssh2_pkt_adddata(struct Packet *pkt, void *data, int len)
{
    if (pkt->logmode != PKTLOG_EMIT) {
	pkt->nblanks++;
	pkt->blanks = sresize(pkt->blanks, pkt->nblanks, struct logblank_t);
	pkt->blanks[pkt->nblanks-1].offset = pkt->length - 6;
	pkt->blanks[pkt->nblanks-1].len = len;
	pkt->blanks[pkt->nblanks-1].type = pkt->logmode;
    }
    pkt->length += len;
    ssh2_pkt_ensure(pkt, pkt->length);
    memcpy(pkt->data + pkt->length - len, data, len);
}
void ssh2_pkt_addbyte(struct Packet *pkt, unsigned char byte)
{
    ssh2_pkt_adddata(pkt, &byte, 1);
}
struct Packet *ssh2_pkt_init(int pkt_type)
{
    struct Packet *pkt = ssh_new_packet();
    pkt->length = 5;
    pkt->forcepad = 0;
    ssh2_pkt_addbyte(pkt, (unsigned char) pkt_type);
    return pkt;
}
void ssh2_pkt_addbool(struct Packet *pkt, unsigned char value)
{
    ssh2_pkt_adddata(pkt, &value, 1);
}
void ssh2_pkt_adduint32(struct Packet *pkt, unsigned long value)
{
    unsigned char x[4];
    PUT_32BIT(x, value);
    ssh2_pkt_adddata(pkt, x, 4);
}
void ssh2_pkt_addstring_start(struct Packet *pkt)
{
    ssh2_pkt_adduint32(pkt, 0);
    pkt->savedpos = pkt->length;
}
void ssh2_pkt_addstring_str(struct Packet *pkt, char *data)
{
    ssh2_pkt_adddata(pkt, data, strlen(data));
    PUT_32BIT(pkt->data + pkt->savedpos - 4, pkt->length - pkt->savedpos);
}
void ssh2_pkt_addstring_data(struct Packet *pkt, char *data, int len)
{
    ssh2_pkt_adddata(pkt, data, len);
    PUT_32BIT(pkt->data + pkt->savedpos - 4, pkt->length - pkt->savedpos);
}
void ssh2_pkt_addstring(struct Packet *pkt, char *data)
{
    ssh2_pkt_addstring_start(pkt);
    ssh2_pkt_addstring_str(pkt, data);
}
static unsigned char *ssh2_mpint_fmt(Bignum b, int *len)
{
    unsigned char *p;
    int i, n = (bignum_bitcount(b) + 7) / 8;
    p = snewn(n + 1, unsigned char);
    if (!p)
	fatalbox("out of memory");
    p[0] = 0;
    for (i = 1; i <= n; i++)
	p[i] = bignum_byte(b, n - i);
    i = 0;
    while (i <= n && p[i] == 0 && (p[i + 1] & 0x80) == 0)
	i++;
    memmove(p, p + i, n + 1 - i);
    *len = n + 1 - i;
    return p;
}
void ssh2_pkt_addmp(struct Packet *pkt, Bignum b)
{
    unsigned char *p;
    int len;
    p = ssh2_mpint_fmt(b, &len);
    ssh2_pkt_addstring_start(pkt);
    ssh2_pkt_addstring_data(pkt, (char *)p, len);
    sfree(p);
}

/*
 * Construct an SSH-2 final-form packet: compress it, encrypt it,
 * put the MAC on it. Final packet, ready to be sent, is stored in
 * pkt->data. Total length is returned.
 */
static int ssh2_pkt_construct(Ssh ssh, struct Packet *pkt)
{
    int cipherblk, maclen, padding, i;

    if (ssh->logctx)
	log_packet(ssh->logctx, PKT_OUTGOING, pkt->data[5],
		   ssh2_pkt_type(ssh->pkt_ctx, pkt->data[5]),
		   pkt->data + 6, pkt->length - 6,
		   pkt->nblanks, pkt->blanks);
    sfree(pkt->blanks); pkt->blanks = NULL;
    pkt->nblanks = 0;

    /*
     * Compress packet payload.
     */
    {
	unsigned char *newpayload;
	int newlen;
	if (ssh->cscomp &&
	    ssh->cscomp->compress(ssh->cs_comp_ctx, pkt->data + 5,
				  pkt->length - 5,
				  &newpayload, &newlen)) {
	    pkt->length = 5;
	    ssh2_pkt_adddata(pkt, newpayload, newlen);
	    sfree(newpayload);
	}
    }

    /*
     * Add padding. At least four bytes, and must also bring total
     * length (minus MAC) up to a multiple of the block size.
     * If pkt->forcepad is set, make sure the packet is at least that size
     * after padding.
     */
    cipherblk = ssh->cscipher ? ssh->cscipher->blksize : 8;  /* block size */
    cipherblk = cipherblk < 8 ? 8 : cipherblk;	/* or 8 if blksize < 8 */
    padding = 4;
    if (pkt->length + padding < pkt->forcepad)
	padding = pkt->forcepad - pkt->length;
    padding +=
	(cipherblk - (pkt->length + padding) % cipherblk) % cipherblk;
    assert(padding <= 255);
    maclen = ssh->csmac ? ssh->csmac->len : 0;
    ssh2_pkt_ensure(pkt, pkt->length + padding + maclen);
    pkt->data[4] = padding;
    for (i = 0; i < padding; i++)
	pkt->data[pkt->length + i] = random_byte();
    PUT_32BIT(pkt->data, pkt->length + padding - 4);
    if (ssh->csmac)
	ssh->csmac->generate(ssh->cs_mac_ctx, pkt->data,
			     pkt->length + padding,
			     ssh->v2_outgoing_sequence);
    ssh->v2_outgoing_sequence++;       /* whether or not we MACed */

    if (ssh->cscipher)
	ssh->cscipher->encrypt(ssh->cs_cipher_ctx,
			       pkt->data, pkt->length + padding);

    pkt->encrypted_len = pkt->length + padding;

    /* Ready-to-send packet starts at pkt->data. We return length. */
    return pkt->length + padding + maclen;
}

/*
 * Routines called from the main SSH code to send packets. There
 * are quite a few of these, because we have two separate
 * mechanisms for delaying the sending of packets:
 * 
 *  - In order to send an IGNORE message and a password message in
 *    a single fixed-length blob, we require the ability to
 *    concatenate the encrypted forms of those two packets _into_ a
 *    single blob and then pass it to our <network.h> transport
 *    layer in one go. Hence, there's a deferment mechanism which
 *    works after packet encryption.
 * 
 *  - In order to avoid sending any connection-layer messages
 *    during repeat key exchange, we have to queue up any such
 *    outgoing messages _before_ they are encrypted (and in
 *    particular before they're allocated sequence numbers), and
 *    then send them once we've finished.
 * 
 * I call these mechanisms `defer' and `queue' respectively, so as
 * to distinguish them reasonably easily.
 * 
 * The functions send_noqueue() and defer_noqueue() free the packet
 * structure they are passed. Every outgoing packet goes through
 * precisely one of these functions in its life; packets passed to
 * ssh2_pkt_send() or ssh2_pkt_defer() either go straight to one of
 * these or get queued, and then when the queue is later emptied
 * the packets are all passed to defer_noqueue().
 */

/*
 * Send an SSH-2 packet immediately, without queuing or deferring.
 */
void ssh2_pkt_send_noqueue(Ssh ssh, struct Packet *pkt)
{
    int len;
    int backlog;
    len = ssh2_pkt_construct(ssh, pkt);
    backlog = sk_write(ssh->s, (char *)pkt->data, len);
    if (backlog > SSH_MAX_BACKLOG)
	ssh_throttle_all(ssh, 1, backlog);

    ssh->outgoing_data_size += pkt->encrypted_len;
    if (!ssh->kex_in_progress &&
	ssh->max_data_size != 0 &&
	ssh->outgoing_data_size > ssh->max_data_size)
	do_ssh2_transport(ssh, "too much data sent", -1, NULL);

    ssh_free_packet(pkt);
}

/*
 * Defer an SSH-2 packet.
 */
static void ssh2_pkt_defer_noqueue(Ssh ssh, struct Packet *pkt)
{
    int len = ssh2_pkt_construct(ssh, pkt);
    if (ssh->deferred_len + len > ssh->deferred_size) {
	ssh->deferred_size = ssh->deferred_len + len + 128;
	ssh->deferred_send_data = sresize(ssh->deferred_send_data,
					  ssh->deferred_size,
					  unsigned char);
    }
    memcpy(ssh->deferred_send_data + ssh->deferred_len, pkt->data, len);
    ssh->deferred_len += len;
    ssh->deferred_data_size += pkt->encrypted_len;
    ssh_free_packet(pkt);
}

/*
 * Queue an SSH-2 packet.
 */
static void ssh2_pkt_queue(Ssh ssh, struct Packet *pkt)
{
    assert(ssh->queueing);

    if (ssh->queuelen >= ssh->queuesize) {
	ssh->queuesize = ssh->queuelen + 32;
	ssh->queue = sresize(ssh->queue, ssh->queuesize, struct Packet *);
    }

    ssh->queue[ssh->queuelen++] = pkt;
}

/*
 * Either queue or send a packet, depending on whether queueing is
 * set.
 */
void ssh2_pkt_send(Ssh ssh, struct Packet *pkt)
{
    if (ssh->queueing)
	ssh2_pkt_queue(ssh, pkt);
    else
	ssh2_pkt_send_noqueue(ssh, pkt);
}

#if 0 /* disused */
/*
 * Either queue or defer a packet, depending on whether queueing is
 * set.
 */
static void ssh2_pkt_defer(Ssh ssh, struct Packet *pkt)
{
    if (ssh->queueing)
	ssh2_pkt_queue(ssh, pkt);
    else
	ssh2_pkt_defer_noqueue(ssh, pkt);
}
#endif

/*
 * Send the whole deferred data block constructed by
 * ssh2_pkt_defer() or SSH-1's defer_packet().
 * 
 * The expected use of the defer mechanism is that you call
 * ssh2_pkt_defer() a few times, then call ssh_pkt_defersend(). If
 * not currently queueing, this simply sets up deferred_send_data
 * and then sends it. If we _are_ currently queueing, the calls to
 * ssh2_pkt_defer() put the deferred packets on to the queue
 * instead, and therefore ssh_pkt_defersend() has no deferred data
 * to send. Hence, there's no need to make it conditional on
 * ssh->queueing.
 */
static void ssh_pkt_defersend(Ssh ssh)
{
    int backlog;
    backlog = sk_write(ssh->s, (char *)ssh->deferred_send_data,
		       ssh->deferred_len);
    ssh->deferred_len = ssh->deferred_size = 0;
    sfree(ssh->deferred_send_data);
    ssh->deferred_send_data = NULL;
    if (backlog > SSH_MAX_BACKLOG)
	ssh_throttle_all(ssh, 1, backlog);

    ssh->outgoing_data_size += ssh->deferred_data_size;
    if (!ssh->kex_in_progress &&
	ssh->max_data_size != 0 &&
	ssh->outgoing_data_size > ssh->max_data_size)
	do_ssh2_transport(ssh, "too much data sent", -1, NULL);
    ssh->deferred_data_size = 0;
}

/*
 * Send all queued SSH-2 packets. We send them by means of
 * ssh2_pkt_defer_noqueue(), in case they included a pair of
 * packets that needed to be lumped together.
 */
static void ssh2_pkt_queuesend(Ssh ssh)
{
    int i;

    assert(!ssh->queueing);

    for (i = 0; i < ssh->queuelen; i++)
	ssh2_pkt_defer_noqueue(ssh, ssh->queue[i]);
    ssh->queuelen = 0;

    ssh_pkt_defersend(ssh);
}

#if 0
void bndebug(char *string, Bignum b)
{
    unsigned char *p;
    int i, len;
    p = ssh2_mpint_fmt(b, &len);
    debug(("%s", string));
    for (i = 0; i < len; i++)
	debug((" %02x", p[i]));
    debug(("\n"));
    sfree(p);
}
#endif

static void sha_mpint(SHA_State * s, Bignum b)
{
    unsigned char *p;
    int len;
    p = ssh2_mpint_fmt(b, &len);
    sha_string(s, p, len);
    sfree(p);
}

/*
 * Packet decode functions for both SSH-1 and SSH-2.
 */
unsigned long ssh_pkt_getuint32(struct Packet *pkt)
{
    unsigned long value;
    if (pkt->length - pkt->savedpos < 4)
	return 0;		       /* arrgh, no way to decline (FIXME?) */
    value = GET_32BIT(pkt->body + pkt->savedpos);
    pkt->savedpos += 4;
    return value;
}
int ssh2_pkt_getbool(struct Packet *pkt)
{
    unsigned long value;
    if (pkt->length - pkt->savedpos < 1)
	return 0;		       /* arrgh, no way to decline (FIXME?) */
    value = pkt->body[pkt->savedpos] != 0;
    pkt->savedpos++;
    return value;
}
void ssh_pkt_getstring(struct Packet *pkt, char **p, int *length)
{
    int len;
    *p = NULL;
    *length = 0;
    if (pkt->length - pkt->savedpos < 4)
	return;
    len = GET_32BIT(pkt->body + pkt->savedpos);
    if (len < 0)
	return;
    *length = len;
    pkt->savedpos += 4;
    if (pkt->length - pkt->savedpos < *length)
	return;
    *p = (char *)(pkt->body + pkt->savedpos);
    pkt->savedpos += *length;
}
static void *ssh_pkt_getdata(struct Packet *pkt, int length)
{
    if (pkt->length - pkt->savedpos < length)
	return NULL;
    pkt->savedpos += length;
    return pkt->body + (pkt->savedpos - length);
}
static int ssh1_pkt_getrsakey(struct Packet *pkt, struct RSAKey *key,
			      unsigned char **keystr)
{
    int j;

    j = makekey(pkt->body + pkt->savedpos,
		pkt->length - pkt->savedpos,
		key, keystr, 0);

    if (j < 0)
	return FALSE;
    
    pkt->savedpos += j;
    assert(pkt->savedpos < pkt->length);

    return TRUE;
}
static Bignum ssh1_pkt_getmp(struct Packet *pkt)
{
    int j;
    Bignum b;

    j = ssh1_read_bignum(pkt->body + pkt->savedpos,
			 pkt->length - pkt->savedpos, &b);

    if (j < 0)
	return NULL;

    pkt->savedpos += j;
    return b;
}
static Bignum ssh2_pkt_getmp(struct Packet *pkt)
{
    char *p;
    int length;
    Bignum b;

    ssh_pkt_getstring(pkt, &p, &length);
    if (!p)
	return NULL;
    if (p[0] & 0x80)
	return NULL;
    b = bignum_from_bytes((unsigned char *)p, length);
    return b;
}

/*
 * Helper function to add an SSH-2 signature blob to a packet.
 * Expects to be shown the public key blob as well as the signature
 * blob. Normally works just like ssh2_pkt_addstring, but will
 * fiddle with the signature packet if necessary for
 * BUG_SSH2_RSA_PADDING.
 */
static void ssh2_add_sigblob(Ssh ssh, struct Packet *pkt,
			     void *pkblob_v, int pkblob_len,
			     void *sigblob_v, int sigblob_len)
{
    unsigned char *pkblob = (unsigned char *)pkblob_v;
    unsigned char *sigblob = (unsigned char *)sigblob_v;

    /* dmemdump(pkblob, pkblob_len); */
    /* dmemdump(sigblob, sigblob_len); */

    /*
     * See if this is in fact an ssh-rsa signature and a buggy
     * server; otherwise we can just do this the easy way.
     */
    if ((ssh->remote_bugs & BUG_SSH2_RSA_PADDING) &&
	(GET_32BIT(pkblob) == 7 && !memcmp(pkblob+4, "ssh-rsa", 7))) {
	int pos, len, siglen;

	/*
	 * Find the byte length of the modulus.
	 */

	pos = 4+7;		       /* skip over "ssh-rsa" */
	pos += 4 + GET_32BIT(pkblob+pos);   /* skip over exponent */
	len = GET_32BIT(pkblob+pos);   /* find length of modulus */
	pos += 4;		       /* find modulus itself */
	while (len > 0 && pkblob[pos] == 0)
	    len--, pos++;
	/* debug(("modulus length is %d\n", len)); */

	/*
	 * Now find the signature integer.
	 */
	pos = 4+7;		       /* skip over "ssh-rsa" */
	siglen = GET_32BIT(sigblob+pos);
	/* debug(("signature length is %d\n", siglen)); */

	if (len != siglen) {
	    unsigned char newlen[4];
	    ssh2_pkt_addstring_start(pkt);
	    ssh2_pkt_addstring_data(pkt, (char *)sigblob, pos);
	    /* dmemdump(sigblob, pos); */
	    pos += 4;		       /* point to start of actual sig */
	    PUT_32BIT(newlen, len);
	    ssh2_pkt_addstring_data(pkt, (char *)newlen, 4);
	    /* dmemdump(newlen, 4); */
	    newlen[0] = 0;
	    while (len-- > siglen) {
		ssh2_pkt_addstring_data(pkt, (char *)newlen, 1);
		/* dmemdump(newlen, 1); */
	    }
	    ssh2_pkt_addstring_data(pkt, (char *)(sigblob+pos), siglen);
	    /* dmemdump(sigblob+pos, siglen); */
	    return;
	}

	/* Otherwise fall through and do it the easy way. */
    }

    ssh2_pkt_addstring_start(pkt);
    ssh2_pkt_addstring_data(pkt, (char *)sigblob, sigblob_len);
}

/*
 * Examine the remote side's version string and compare it against
 * a list of known buggy implementations.
 */
static void ssh_detect_bugs(Ssh ssh, char *vstring)
{
    char *imp;			       /* pointer to implementation part */
    imp = vstring;
    imp += strcspn(imp, "-");
    if (*imp) imp++;
    imp += strcspn(imp, "-");
    if (*imp) imp++;

    ssh->remote_bugs = 0;

    if (ssh->cfg.sshbug_ignore1 == FORCE_ON ||
	(ssh->cfg.sshbug_ignore1 == AUTO &&
	 (!strcmp(imp, "1.2.18") || !strcmp(imp, "1.2.19") ||
	  !strcmp(imp, "1.2.20") || !strcmp(imp, "1.2.21") ||
	  !strcmp(imp, "1.2.22") || !strcmp(imp, "Cisco-1.25") ||
	  !strcmp(imp, "OSU_1.4alpha3") || !strcmp(imp, "OSU_1.5alpha4")))) {
	/*
	 * These versions don't support SSH1_MSG_IGNORE, so we have
	 * to use a different defence against password length
	 * sniffing.
	 */
	ssh->remote_bugs |= BUG_CHOKES_ON_SSH1_IGNORE;
	logevent("We believe remote version has SSH-1 ignore bug");
    }

    if (ssh->cfg.sshbug_plainpw1 == FORCE_ON ||
	(ssh->cfg.sshbug_plainpw1 == AUTO &&
	 (!strcmp(imp, "Cisco-1.25") || !strcmp(imp, "OSU_1.4alpha3")))) {
	/*
	 * These versions need a plain password sent; they can't
	 * handle having a null and a random length of data after
	 * the password.
	 */
	ssh->remote_bugs |= BUG_NEEDS_SSH1_PLAIN_PASSWORD;
	logevent("We believe remote version needs a plain SSH-1 password");
    }

    if (ssh->cfg.sshbug_rsa1 == FORCE_ON ||
	(ssh->cfg.sshbug_rsa1 == AUTO &&
	 (!strcmp(imp, "Cisco-1.25")))) {
	/*
	 * These versions apparently have no clue whatever about
	 * RSA authentication and will panic and die if they see
	 * an AUTH_RSA message.
	 */
	ssh->remote_bugs |= BUG_CHOKES_ON_RSA;
	logevent("We believe remote version can't handle SSH-1 RSA authentication");
    }

    if (ssh->cfg.sshbug_hmac2 == FORCE_ON ||
	(ssh->cfg.sshbug_hmac2 == AUTO &&
	 !wc_match("* VShell", imp) &&
	 (wc_match("2.1.0*", imp) || wc_match("2.0.*", imp) ||
	  wc_match("2.2.0*", imp) || wc_match("2.3.0*", imp) ||
	  wc_match("2.1 *", imp)))) {
	/*
	 * These versions have the HMAC bug.
	 */
	ssh->remote_bugs |= BUG_SSH2_HMAC;
	logevent("We believe remote version has SSH-2 HMAC bug");
    }

    if (ssh->cfg.sshbug_derivekey2 == FORCE_ON ||
	(ssh->cfg.sshbug_derivekey2 == AUTO &&
	 !wc_match("* VShell", imp) &&
	 (wc_match("2.0.0*", imp) || wc_match("2.0.10*", imp) ))) {
	/*
	 * These versions have the key-derivation bug (failing to
	 * include the literal shared secret in the hashes that
	 * generate the keys).
	 */
	ssh->remote_bugs |= BUG_SSH2_DERIVEKEY;
	logevent("We believe remote version has SSH-2 key-derivation bug");
    }

    if (ssh->cfg.sshbug_rsapad2 == FORCE_ON ||
	(ssh->cfg.sshbug_rsapad2 == AUTO &&
	 (wc_match("OpenSSH_2.[5-9]*", imp) ||
	  wc_match("OpenSSH_3.[0-2]*", imp)))) {
	/*
	 * These versions have the SSH-2 RSA padding bug.
	 */
	ssh->remote_bugs |= BUG_SSH2_RSA_PADDING;
	logevent("We believe remote version has SSH-2 RSA padding bug");
    }

    if (ssh->cfg.sshbug_pksessid2 == FORCE_ON ||
	(ssh->cfg.sshbug_pksessid2 == AUTO &&
	 wc_match("OpenSSH_2.[0-2]*", imp))) {
	/*
	 * These versions have the SSH-2 session-ID bug in
	 * public-key authentication.
	 */
	ssh->remote_bugs |= BUG_SSH2_PK_SESSIONID;
	logevent("We believe remote version has SSH-2 public-key-session-ID bug");
    }

    if (ssh->cfg.sshbug_rekey2 == FORCE_ON ||
	(ssh->cfg.sshbug_rekey2 == AUTO &&
	 (wc_match("OpenSSH_2.[0-4]*", imp) ||
	  wc_match("OpenSSH_2.5.[0-3]*", imp) ||
	  wc_match("Sun_SSH_1.0", imp) ||
	  wc_match("Sun_SSH_1.0.1", imp)))) {
	/*
	 * These versions have the SSH-2 rekey bug.
	 */
	ssh->remote_bugs |= BUG_SSH2_REKEY;
	logevent("We believe remote version has SSH-2 rekey bug");
    }
}

/*
 * The `software version' part of an SSH version string is required
 * to contain no spaces or minus signs.
 */
static void ssh_fix_verstring(char *str)
{
    /* Eat "SSH-<protoversion>-". */
    assert(*str == 'S'); str++;
    assert(*str == 'S'); str++;
    assert(*str == 'H'); str++;
    assert(*str == '-'); str++;
    while (*str && *str != '-') str++;
    assert(*str == '-'); str++;

    /* Convert minus signs and spaces in the remaining string into
     * underscores. */
    while (*str) {
        if (*str == '-' || *str == ' ')
            *str = '_';
        str++;
    }
}

static int do_ssh_init(Ssh ssh, unsigned char c)
{
    struct do_ssh_init_state {
	int vslen;
	char version[10];
	char *vstring;
	int vstrsize;
	int i;
	int proto1, proto2;
    };
    crState(do_ssh_init_state);

    crBegin(ssh->do_ssh_init_crstate);

    /* Search for the string "SSH-" in the input. */
    s->i = 0;
    while (1) {
	static const int transS[] = { 1, 2, 2, 1 };
	static const int transH[] = { 0, 0, 3, 0 };
	static const int transminus[] = { 0, 0, 0, -1 };
	if (c == 'S')
	    s->i = transS[s->i];
	else if (c == 'H')
	    s->i = transH[s->i];
	else if (c == '-')
	    s->i = transminus[s->i];
	else
	    s->i = 0;
	if (s->i < 0)
	    break;
	crReturn(1);		       /* get another character */
    }

    s->vstrsize = 16;
    s->vstring = snewn(s->vstrsize, char);
    strcpy(s->vstring, "SSH-");
    s->vslen = 4;
    s->i = 0;
    while (1) {
	crReturn(1);		       /* get another char */
	if (s->vslen >= s->vstrsize - 1) {
	    s->vstrsize += 16;
	    s->vstring = sresize(s->vstring, s->vstrsize, char);
	}
	s->vstring[s->vslen++] = c;
	if (s->i >= 0) {
	    if (c == '-') {
		s->version[s->i] = '\0';
		s->i = -1;
	    } else if (s->i < sizeof(s->version) - 1)
		s->version[s->i++] = c;
	} else if (c == '\012')
	    break;
    }

    ssh->agentfwd_enabled = FALSE;
    ssh->rdpkt2_state.incoming_sequence = 0;

    s->vstring[s->vslen] = 0;
    s->vstring[strcspn(s->vstring, "\015\012")] = '\0';/* remove EOL chars */
    logeventf(ssh, "Server version: %s", s->vstring);
    ssh_detect_bugs(ssh, s->vstring);

    /*
     * Decide which SSH protocol version to support.
     */

    /* Anything strictly below "2.0" means protocol 1 is supported. */
    s->proto1 = ssh_versioncmp(s->version, "2.0") < 0;
    /* Anything greater or equal to "1.99" means protocol 2 is supported. */
    s->proto2 = ssh_versioncmp(s->version, "1.99") >= 0;

    if (ssh->cfg.sshprot == 0 && !s->proto1) {
	bombout(("SSH protocol version 1 required by user but not provided by server"));
	crStop(0);
    }
    if (ssh->cfg.sshprot == 3 && !s->proto2) {
	bombout(("SSH protocol version 2 required by user but not provided by server"));
	crStop(0);
    }

    {
        char *verstring;

        if (s->proto2 && (ssh->cfg.sshprot >= 2 || !s->proto1)) {
            /*
             * Construct a v2 version string.
             */
            verstring = dupprintf("SSH-2.0-%s\015\012", sshver);
            ssh->version = 2;
        } else {
            /*
             * Construct a v1 version string.
             */
            verstring = dupprintf("SSH-%s-%s\012",
                                  (ssh_versioncmp(s->version, "1.5") <= 0 ?
                                   s->version : "1.5"),
                                  sshver);
            ssh->version = 1;
        }

        ssh_fix_verstring(verstring);

        if (ssh->version == 2) {
            /*
             * Hash our version string and their version string.
             */
            SHA_Init(&ssh->exhashbase);
            sha_string(&ssh->exhashbase, verstring,
                       strcspn(verstring, "\015\012"));
            sha_string(&ssh->exhashbase, s->vstring,
                       strcspn(s->vstring, "\015\012"));

            /*
             * Initialise SSH-2 protocol.
             */
            ssh->protocol = ssh2_protocol;
            ssh2_protocol_setup(ssh);
            ssh->s_rdpkt = ssh2_rdpkt;
        } else {
            /*
             * Initialise SSH-1 protocol.
             */
            ssh->protocol = ssh1_protocol;
            ssh1_protocol_setup(ssh);
            ssh->s_rdpkt = ssh1_rdpkt;
        }
        logeventf(ssh, "We claim version: %.*s",
                  strcspn(verstring, "\015\012"), verstring);
	sk_write(ssh->s, verstring, strlen(verstring));
        sfree(verstring);
    }

    logeventf(ssh, "Using SSH protocol version %d", ssh->version);

    update_specials_menu(ssh->frontend);
    ssh->state = SSH_STATE_BEFORE_SIZE;
    ssh->pinger = pinger_new(&ssh->cfg, &ssh_backend, ssh);

    sfree(s->vstring);

    crFinish(0);
}

static void ssh_process_incoming_data(Ssh ssh,
				      unsigned char **data, int *datalen)
{
    struct Packet *pktin = ssh->s_rdpkt(ssh, data, datalen);
    if (pktin) {
	ssh->protocol(ssh, NULL, 0, pktin);
	ssh_free_packet(pktin);
    }
}

static void ssh_queue_incoming_data(Ssh ssh,
				    unsigned char **data, int *datalen)
{
    bufchain_add(&ssh->queued_incoming_data, *data, *datalen);
    *data += *datalen;
    *datalen = 0;
}

static void ssh_process_queued_incoming_data(Ssh ssh)
{
    void *vdata;
    unsigned char *data;
    int len, origlen;

    while (!ssh->frozen && bufchain_size(&ssh->queued_incoming_data)) {
	bufchain_prefix(&ssh->queued_incoming_data, &vdata, &len);
	data = vdata;
	origlen = len;

	while (!ssh->frozen && len > 0)
	    ssh_process_incoming_data(ssh, &data, &len);

	if (origlen > len)
	    bufchain_consume(&ssh->queued_incoming_data, origlen - len);
    }
}

static void ssh_set_frozen(Ssh ssh, int frozen)
{
    if (ssh->s)
	sk_set_frozen(ssh->s, frozen);
    ssh->frozen = frozen;
}

static void ssh_gotdata(Ssh ssh, unsigned char *data, int datalen)
{
    crBegin(ssh->ssh_gotdata_crstate);

    /*
     * To begin with, feed the characters one by one to the
     * protocol initialisation / selection function do_ssh_init().
     * When that returns 0, we're done with the initial greeting
     * exchange and can move on to packet discipline.
     */
    while (1) {
	int ret;		       /* need not be kept across crReturn */
	if (datalen == 0)
	    crReturnV;		       /* more data please */
	ret = do_ssh_init(ssh, *data);
	data++;
	datalen--;
	if (ret == 0)
	    break;
    }

    /*
     * We emerge from that loop when the initial negotiation is
     * over and we have selected an s_rdpkt function. Now pass
     * everything to s_rdpkt, and then pass the resulting packets
     * to the proper protocol handler.
     */
    if (datalen == 0)
	crReturnV;

    /*
     * Process queued data if there is any.
     */
    ssh_process_queued_incoming_data(ssh);

    while (1) {
	while (datalen > 0) {
	    if (ssh->frozen)
		ssh_queue_incoming_data(ssh, &data, &datalen);

	    ssh_process_incoming_data(ssh, &data, &datalen);

	    if (ssh->state == SSH_STATE_CLOSED)
		return;
	}
	crReturnV;
    }
    crFinishV;
}

int ssh_do_close(Ssh ssh, int notify_exit)
{
    int ret = 0;
    struct ssh_channel *c;

    ssh->state = SSH_STATE_CLOSED;
    expire_timer_context(ssh);
    if (ssh->s) {
        sk_close(ssh->s);
        ssh->s = NULL;
        if (notify_exit)
            notify_remote_exit(ssh->frontend);
        else
            ret = 1;
    }
    /*
     * Now we must shut down any port- and X-forwarded channels going
     * through this connection.
     */
    if (ssh->channels) {
	while (NULL != (c = index234(ssh->channels, 0))) {
	    switch (c->type) {
	      case CHAN_X11:
		x11_close(c->u.x11.s);
		break;
	      case CHAN_SOCKDATA:
		pfd_close(c->u.pfd.s);
		break;
	    }
	    del234(ssh->channels, c); /* moving next one to index 0 */
	    if (ssh->version == 2)
		bufchain_clear(&c->v.v2.outbuffer);
	    sfree(c);
	}
    }
    /*
     * Go through port-forwardings, and close any associated
     * listening sockets.
     */
    if (ssh->portfwds) {
	struct ssh_portfwd *pf;
	while (NULL != (pf = index234(ssh->portfwds, 0))) {
	    /* Dispose of any listening socket. */
	    if (pf->local)
		pfd_terminate(pf->local);
	    del234(ssh->portfwds, pf); /* moving next one to index 0 */
	    free_portfwd(pf);
	}
#ifdef MPEXT
	// fix memory leak
	freetree234(ssh->portfwds);
	ssh->portfwds = NULL;
#endif
    }

    return ret;
}

static void ssh_log(Plug plug, int type, SockAddr addr, int port,
		    const char *error_msg, int error_code)
{
    Ssh ssh = (Ssh) plug;
    char addrbuf[256], *msg;

    sk_getaddr(addr, addrbuf, lenof(addrbuf));

    if (type == 0)
	msg = dupprintf("Connecting to %s port %d", addrbuf, port);
    else
	msg = dupprintf("Failed to connect to %s: %s", addrbuf, error_msg);

    logevent(msg);
    sfree(msg);
}

static int ssh_closing(Plug plug, const char *error_msg, int error_code,
		       int calling_back)
{
    Ssh ssh = (Ssh) plug;
    int need_notify = ssh_do_close(ssh, FALSE);

    if (!error_msg && !ssh->close_expected) {
        error_msg = "Server unexpectedly closed network connection";
    }

    if (need_notify)
        notify_remote_exit(ssh->frontend);

    if (error_msg) {
	/* A socket error has occurred. */
	logevent(error_msg);
	connection_fatal(ssh->frontend, "%s", error_msg);
    } else {
        logevent("Server closed network connection");
    }
    return 0;
}

static int ssh_receive(Plug plug, int urgent, char *data, int len)
{
    Ssh ssh = (Ssh) plug;
    ssh_gotdata(ssh, (unsigned char *)data, len);
    if (ssh->state == SSH_STATE_CLOSED) {
	ssh_do_close(ssh, TRUE);
	return 0;
    }
    return 1;
}

static void ssh_sent(Plug plug, int bufsize)
{
    Ssh ssh = (Ssh) plug;
    /*
     * If the send backlog on the SSH socket itself clears, we
     * should unthrottle the whole world if it was throttled.
     */
    if (bufsize < SSH_MAX_BACKLOG)
	ssh_throttle_all(ssh, 0, bufsize);
}

/*
 * Connect to specified host and port.
 * Returns an error message, or NULL on success.
 * Also places the canonical host name into `realhost'. It must be
 * freed by the caller.
 */
static const char *connect_to_host(Ssh ssh, char *host, int port,
				   char **realhost, int nodelay, int keepalive)
{
    static const struct plug_function_table fn_table = {
	ssh_log,
	ssh_closing,
	ssh_receive,
	ssh_sent,
	NULL
    };

    SockAddr addr;
    const char *err;

    ssh->savedhost = snewn(1 + strlen(host), char);
    if (!ssh->savedhost)
	fatalbox("Out of memory");
    strcpy(ssh->savedhost, host);

    if (port < 0)
	port = 22;		       /* default ssh port */
    ssh->savedport = port;

    /*
     * Try to find host.
     */
    logeventf(ssh, "Looking up host \"%s\"%s", host,
	      (ssh->cfg.addressfamily == ADDRTYPE_IPV4 ? " (IPv4)" :
	       (ssh->cfg.addressfamily == ADDRTYPE_IPV6 ? " (IPv6)" : "")));
    addr = name_lookup(host, port, realhost, &ssh->cfg,
		       ssh->cfg.addressfamily);
    if ((err = sk_addr_error(addr)) != NULL) {
	sk_addr_free(addr);
	return err;
    }

    /*
     * Open socket.
     */
    ssh->fn = &fn_table;
    ssh->s = new_connection(addr, *realhost, port,
			    0, 1, nodelay, keepalive, (Plug) ssh, &ssh->cfg);
    if ((err = sk_socket_error(ssh->s)) != NULL) {
	ssh->s = NULL;
	notify_remote_exit(ssh->frontend);
	return err;
    }

    return NULL;
}

/*
 * Throttle or unthrottle the SSH connection.
 */
static void ssh1_throttle(Ssh ssh, int adjust)
{
    int old_count = ssh->v1_throttle_count;
    ssh->v1_throttle_count += adjust;
    assert(ssh->v1_throttle_count >= 0);
    if (ssh->v1_throttle_count && !old_count) {
	ssh_set_frozen(ssh, 1);
    } else if (!ssh->v1_throttle_count && old_count) {
	ssh_set_frozen(ssh, 0);
    }
}

/*
 * Throttle or unthrottle _all_ local data streams (for when sends
 * on the SSH connection itself back up).
 */
static void ssh_throttle_all(Ssh ssh, int enable, int bufsize)
{
    int i;
    struct ssh_channel *c;

    if (enable == ssh->throttled_all)
	return;
    ssh->throttled_all = enable;
    ssh->overall_bufsize = bufsize;
    if (!ssh->channels)
	return;
    for (i = 0; NULL != (c = index234(ssh->channels, i)); i++) {
	switch (c->type) {
	  case CHAN_MAINSESSION:
	    /*
	     * This is treated separately, outside the switch.
	     */
	    break;
	  case CHAN_X11:
	    x11_override_throttle(c->u.x11.s, enable);
	    break;
	  case CHAN_AGENT:
	    /* Agent channels require no buffer management. */
	    break;
	  case CHAN_SOCKDATA:
	    pfd_override_throttle(c->u.pfd.s, enable);
	    break;
	}
    }
}

/*
 * Username and password input, abstracted off into routines
 * reusable in several places - even between SSH-1 and SSH-2.
 */

/* Set up a username or password input loop on a given buffer. */
static void setup_userpass_input(Ssh ssh, char *buffer, int buflen, int echo)
{
    ssh->userpass_input_buffer = buffer;
    ssh->userpass_input_buflen = buflen;
    ssh->userpass_input_bufpos = 0;
    ssh->userpass_input_echo = echo;
}

/*
 * Process some terminal data in the course of username/password
 * input. Returns >0 for success (line of input returned in
 * buffer), <0 for failure (user hit ^C/^D, bomb out and exit), 0
 * for inconclusive (keep waiting for more input please).
 */
static int process_userpass_input(Ssh ssh, unsigned char *in, int inlen)
{
    char c;

    while (inlen--) {
	switch (c = *in++) {
	  case 10:
	  case 13:
	    ssh->userpass_input_buffer[ssh->userpass_input_bufpos] = 0;
	    ssh->userpass_input_buffer[ssh->userpass_input_buflen-1] = 0;
	    return +1;
	    break;
	  case 8:
	  case 127:
	    if (ssh->userpass_input_bufpos > 0) {
		if (ssh->userpass_input_echo)
		    c_write_str(ssh, "\b \b");
		ssh->userpass_input_bufpos--;
	    }
	    break;
	  case 21:
	  case 27:
	    while (ssh->userpass_input_bufpos > 0) {
		if (ssh->userpass_input_echo)
		    c_write_str(ssh, "\b \b");
		ssh->userpass_input_bufpos--;
	    }
	    break;
	  case 3:
	  case 4:
	    return -1;
	    break;
	  default:
	    /*
	     * This simplistic check for printability is disabled
	     * when we're doing password input, because some people
	     * have control characters in their passwords.o
	     */
	    if ((!ssh->userpass_input_echo ||
		 (c >= ' ' && c <= '~') ||
		 ((unsigned char) c >= 160))
		&& ssh->userpass_input_bufpos < ssh->userpass_input_buflen-1) {
		ssh->userpass_input_buffer[ssh->userpass_input_bufpos++] = c;
		if (ssh->userpass_input_echo)
		    c_write(ssh, &c, 1);
	    }
	    break;
	}
    }
    return 0;
}

static void ssh_agent_callback(void *sshv, void *reply, int replylen)
{
    Ssh ssh = (Ssh) sshv;

    ssh->agent_response = reply;
    ssh->agent_response_len = replylen;

    if (ssh->version == 1)
	do_ssh1_login(ssh, NULL, -1, NULL);
    else
	do_ssh2_authconn(ssh, NULL, -1, NULL);
}

static void ssh_dialog_callback(void *sshv, int ret)
{
    Ssh ssh = (Ssh) sshv;

    ssh->user_response = ret;

    if (ssh->version == 1)
	do_ssh1_login(ssh, NULL, -1, NULL);
    else
	do_ssh2_transport(ssh, NULL, -1, NULL);

    /*
     * This may have unfrozen the SSH connection, so do a
     * queued-data run.
     */
    ssh_process_queued_incoming_data(ssh);
}

static void ssh_agentf_callback(void *cv, void *reply, int replylen)
{
    struct ssh_channel *c = (struct ssh_channel *)cv;
    Ssh ssh = c->ssh;
    void *sentreply = reply;

    if (!sentreply) {
	/* Fake SSH_AGENT_FAILURE. */
	sentreply = "\0\0\0\1\5";
	replylen = 5;
    }
    if (ssh->version == 2) {
	ssh2_add_channel_data(c, sentreply, replylen);
	ssh2_try_send(c);
    } else {
	send_packet(ssh, SSH1_MSG_CHANNEL_DATA,
		    PKT_INT, c->remoteid,
		    PKTT_DATA,
		    PKT_INT, replylen,
		    PKT_DATA, sentreply, replylen,
		    PKTT_OTHER,
		    PKT_END);
    }
    if (reply)
	sfree(reply);
}

/*
 * Handle the key exchange and user authentication phases.
 */
static int do_ssh1_login(Ssh ssh, unsigned char *in, int inlen,
			 struct Packet *pktin)
{
    int i, j, ret;
    unsigned char cookie[8], *ptr;
    struct RSAKey servkey, hostkey;
    struct MD5Context md5c;
    struct do_ssh1_login_state {
	int len;
	unsigned char *rsabuf, *keystr1, *keystr2;
	unsigned long supported_ciphers_mask, supported_auths_mask;
	int tried_publickey, tried_agent;
	int tis_auth_refused, ccard_auth_refused;
	unsigned char session_id[16];
	int cipher_type;
	char username[100];
	void *publickey_blob;
	int publickey_bloblen;
	char password[100];
	char prompt[200];
	int pos;
	char c;
	int pwpkt_type;
	unsigned char request[5], *response, *p;
	int responselen;
	int keyi, nkeys;
	int authed;
	struct RSAKey key;
	Bignum challenge;
	char *commentp;
	int commentlen;
        int dlgret;
    };
    crState(do_ssh1_login_state);

    crBegin(ssh->do_ssh1_login_crstate);

    if (!pktin)
	crWaitUntil(pktin);

    if (pktin->type != SSH1_SMSG_PUBLIC_KEY) {
	bombout(("Public key packet not received"));
	crStop(0);
    }

    logevent("Received public keys");

    ptr = ssh_pkt_getdata(pktin, 8);
    if (!ptr) {
	bombout(("SSH-1 public key packet stopped before random cookie"));
	crStop(0);
    }
    memcpy(cookie, ptr, 8);

    if (!ssh1_pkt_getrsakey(pktin, &servkey, &s->keystr1) ||
	!ssh1_pkt_getrsakey(pktin, &hostkey, &s->keystr2)) {	
	bombout(("Failed to read SSH-1 public keys from public key packet"));
	crStop(0);
    }

    /*
     * Log the host key fingerprint.
     */
    {
	char logmsg[80];
	logevent("Host key fingerprint is:");
	strcpy(logmsg, "      ");
	hostkey.comment = NULL;
	rsa_fingerprint(logmsg + strlen(logmsg),
			sizeof(logmsg) - strlen(logmsg), &hostkey);
	logevent(logmsg);
    }

    ssh->v1_remote_protoflags = ssh_pkt_getuint32(pktin);
    s->supported_ciphers_mask = ssh_pkt_getuint32(pktin);
    s->supported_auths_mask = ssh_pkt_getuint32(pktin);

    ssh->v1_local_protoflags =
	ssh->v1_remote_protoflags & SSH1_PROTOFLAGS_SUPPORTED;
    ssh->v1_local_protoflags |= SSH1_PROTOFLAG_SCREEN_NUMBER;

    MD5Init(&md5c);
    MD5Update(&md5c, s->keystr2, hostkey.bytes);
    MD5Update(&md5c, s->keystr1, servkey.bytes);
    MD5Update(&md5c, cookie, 8);
    MD5Final(s->session_id, &md5c);

    for (i = 0; i < 32; i++)
	ssh->session_key[i] = random_byte();

    /*
     * Verify that the `bits' and `bytes' parameters match.
     */
    if (hostkey.bits > hostkey.bytes * 8 ||
	servkey.bits > servkey.bytes * 8) {
	bombout(("SSH-1 public keys were badly formatted"));
	crStop(0);
    }

    s->len = (hostkey.bytes > servkey.bytes ? hostkey.bytes : servkey.bytes);

    s->rsabuf = snewn(s->len, unsigned char);
    if (!s->rsabuf)
	fatalbox("Out of memory");

    /*
     * Verify the host key.
     */
    {
	/*
	 * First format the key into a string.
	 */
	int len = rsastr_len(&hostkey);
	char fingerprint[100];
	char *keystr = snewn(len, char);
	if (!keystr)
	    fatalbox("Out of memory");
	rsastr_fmt(keystr, &hostkey);
	rsa_fingerprint(fingerprint, sizeof(fingerprint), &hostkey);

        ssh_set_frozen(ssh, 1);
	s->dlgret = verify_ssh_host_key(ssh->frontend,
                                        ssh->savedhost, ssh->savedport,
                                        "rsa", keystr, fingerprint,
                                        ssh_dialog_callback, ssh);
	sfree(keystr);
        if (s->dlgret < 0) {
            do {
                crReturn(0);
                if (pktin) {
                    bombout(("Unexpected data from server while waiting"
                             " for user host key response"));
                    crStop(0);
                }
            } while (pktin || inlen > 0);
            s->dlgret = ssh->user_response;
        }
        ssh_set_frozen(ssh, 0);

        if (s->dlgret == 0) {
            ssh->close_expected = TRUE;
            ssh_closing((Plug)ssh, NULL, 0, 0);
	    crStop(0);
        }
    }

    for (i = 0; i < 32; i++) {
	s->rsabuf[i] = ssh->session_key[i];
	if (i < 16)
	    s->rsabuf[i] ^= s->session_id[i];
    }

    if (hostkey.bytes > servkey.bytes) {
	ret = rsaencrypt(s->rsabuf, 32, &servkey);
	if (ret)
	    ret = rsaencrypt(s->rsabuf, servkey.bytes, &hostkey);
    } else {
	ret = rsaencrypt(s->rsabuf, 32, &hostkey);
	if (ret)
	    ret = rsaencrypt(s->rsabuf, hostkey.bytes, &servkey);
    }
    if (!ret) {
	bombout(("SSH-1 public key encryptions failed due to bad formatting"));
	crStop(0);	
    }

    logevent("Encrypted session key");

    {
	int cipher_chosen = 0, warn = 0;
	char *cipher_string = NULL;
	int i;
	for (i = 0; !cipher_chosen && i < CIPHER_MAX; i++) {
	    int next_cipher = ssh->cfg.ssh_cipherlist[i];
	    if (next_cipher == CIPHER_WARN) {
		/* If/when we choose a cipher, warn about it */
		warn = 1;
	    } else if (next_cipher == CIPHER_AES) {
		/* XXX Probably don't need to mention this. */
		logevent("AES not supported in SSH-1, skipping");
	    } else {
		switch (next_cipher) {
		  case CIPHER_3DES:     s->cipher_type = SSH_CIPHER_3DES;
					cipher_string = "3DES"; break;
		  case CIPHER_BLOWFISH: s->cipher_type = SSH_CIPHER_BLOWFISH;
					cipher_string = "Blowfish"; break;
		  case CIPHER_DES:	s->cipher_type = SSH_CIPHER_DES;
					cipher_string = "single-DES"; break;
		}
		if (s->supported_ciphers_mask & (1 << s->cipher_type))
		    cipher_chosen = 1;
	    }
	}
	if (!cipher_chosen) {
	    if ((s->supported_ciphers_mask & (1 << SSH_CIPHER_3DES)) == 0)
		bombout(("Server violates SSH-1 protocol by not "
			 "supporting 3DES encryption"));
	    else
		/* shouldn't happen */
		bombout(("No supported ciphers found"));
	    crStop(0);
	}

	/* Warn about chosen cipher if necessary. */
	if (warn) {
            ssh_set_frozen(ssh, 1);
	    s->dlgret = askalg(ssh->frontend, "cipher", cipher_string,
			       ssh_dialog_callback, ssh);
	    if (s->dlgret < 0) {
		do {
		    crReturn(0);
		    if (pktin) {
			bombout(("Unexpected data from server while waiting"
				 " for user response"));
			crStop(0);
		    }
		} while (pktin || inlen > 0);
		s->dlgret = ssh->user_response;
	    }
            ssh_set_frozen(ssh, 0);
	    if (s->dlgret == 0) {
		ssh->close_expected = TRUE;
		ssh_closing((Plug)ssh, NULL, 0, 0);
		crStop(0);
	    }
        }
    }

    switch (s->cipher_type) {
      case SSH_CIPHER_3DES:
	logevent("Using 3DES encryption");
	break;
      case SSH_CIPHER_DES:
	logevent("Using single-DES encryption");
	break;
      case SSH_CIPHER_BLOWFISH:
	logevent("Using Blowfish encryption");
	break;
    }

    send_packet(ssh, SSH1_CMSG_SESSION_KEY,
		PKT_CHAR, s->cipher_type,
		PKT_DATA, cookie, 8,
		PKT_CHAR, (s->len * 8) >> 8, PKT_CHAR, (s->len * 8) & 0xFF,
		PKT_DATA, s->rsabuf, s->len,
		PKT_INT, ssh->v1_local_protoflags, PKT_END);

    logevent("Trying to enable encryption...");

    sfree(s->rsabuf);

    ssh->cipher = (s->cipher_type == SSH_CIPHER_BLOWFISH ? &ssh_blowfish_ssh1 :
		   s->cipher_type == SSH_CIPHER_DES ? &ssh_des :
		   &ssh_3des);
    ssh->v1_cipher_ctx = ssh->cipher->make_context();
    ssh->cipher->sesskey(ssh->v1_cipher_ctx, ssh->session_key);
    logeventf(ssh, "Initialised %s encryption", ssh->cipher->text_name);

    ssh->crcda_ctx = crcda_make_context();
    logevent("Installing CRC compensation attack detector");

    if (servkey.modulus) {
	sfree(servkey.modulus);
	servkey.modulus = NULL;
    }
    if (servkey.exponent) {
	sfree(servkey.exponent);
	servkey.exponent = NULL;
    }
    if (hostkey.modulus) {
	sfree(hostkey.modulus);
	hostkey.modulus = NULL;
    }
    if (hostkey.exponent) {
	sfree(hostkey.exponent);
	hostkey.exponent = NULL;
    }
    crWaitUntil(pktin);

    if (pktin->type != SSH1_SMSG_SUCCESS) {
	bombout(("Encryption not successfully enabled"));
	crStop(0);
    }

    logevent("Successfully started encryption");

    fflush(stdout);
    {
	if (!*ssh->cfg.username) {
	    if (ssh_get_line && !ssh_getline_pw_only) {
#ifdef MPEXT
		if (!ssh_get_line(ssh->frontend, "login as: ",
				  s->username, sizeof(s->username), FALSE)) {
#else
		if (!ssh_get_line("login as: ",
				  s->username, sizeof(s->username), FALSE)) {
#endif
		    /*
		     * get_line failed to get a username.
		     * Terminate.
		     */
		    logevent("No username provided. Abandoning session.");
		    ssh->close_expected = TRUE;
                    ssh_closing((Plug)ssh, NULL, 0, 0);
		    crStop(1);
		}
	    } else {
		int ret;	       /* need not be kept over crReturn */
		c_write_str(ssh, "login as: ");
		ssh->send_ok = 1;

		setup_userpass_input(ssh, s->username, sizeof(s->username), 1);
		do {
		    crWaitUntil(!pktin);
		    ret = process_userpass_input(ssh, in, inlen);
		} while (ret == 0);
		if (ret < 0)
		    cleanup_exit(0);
		c_write_str(ssh, "\r\n");
	    }
	} else {
	    strncpy(s->username, ssh->cfg.username, sizeof(s->username));
	    s->username[sizeof(s->username)-1] = '\0';
	}

	send_packet(ssh, SSH1_CMSG_USER, PKT_STR, s->username, PKT_END);
	{
	    char userlog[22 + sizeof(s->username)];
	    sprintf(userlog, "Sent username \"%s\"", s->username);
	    logevent(userlog);
	    if (flags & FLAG_INTERACTIVE &&
		(!((flags & FLAG_STDERR) && (flags & FLAG_VERBOSE)))) {
		strcat(userlog, "\r\n");
		c_write_str(ssh, userlog);
	    }
	}
    }

    crWaitUntil(pktin);

    if ((ssh->remote_bugs & BUG_CHOKES_ON_RSA)) {
	/* We must not attempt PK auth. Pretend we've already tried it. */
	s->tried_publickey = s->tried_agent = 1;
    } else {
	s->tried_publickey = s->tried_agent = 0;
    }
    s->tis_auth_refused = s->ccard_auth_refused = 0;
    /* Load the public half of ssh->cfg.keyfile so we notice if it's in Pageant */
    if (!filename_is_null(ssh->cfg.keyfile)) {
	if (!rsakey_pubblob(&ssh->cfg.keyfile,
			    &s->publickey_blob, &s->publickey_bloblen, NULL))
	    s->publickey_blob = NULL;
    } else
	s->publickey_blob = NULL;

    while (pktin->type == SSH1_SMSG_FAILURE) {
	s->pwpkt_type = SSH1_CMSG_AUTH_PASSWORD;

	if (agent_exists() && !s->tried_agent) {
	    /*
	     * Attempt RSA authentication using Pageant.
	     */
	    void *r;

	    s->authed = FALSE;
	    s->tried_agent = 1;
	    logevent("Pageant is running. Requesting keys.");

	    /* Request the keys held by the agent. */
	    PUT_32BIT(s->request, 1);
	    s->request[4] = SSH1_AGENTC_REQUEST_RSA_IDENTITIES;
	    if (!agent_query(s->request, 5, &r, &s->responselen,
			     ssh_agent_callback, ssh)) {
		do {
		    crReturn(0);
		    if (pktin) {
			bombout(("Unexpected data from server while waiting"
				 " for agent response"));
			crStop(0);
		    }
		} while (pktin || inlen > 0);
		r = ssh->agent_response;
		s->responselen = ssh->agent_response_len;
	    }
	    s->response = (unsigned char *) r;
	    if (s->response && s->responselen >= 5 &&
		s->response[4] == SSH1_AGENT_RSA_IDENTITIES_ANSWER) {
		s->p = s->response + 5;
		s->nkeys = GET_32BIT(s->p);
		s->p += 4;
		logeventf(ssh, "Pageant has %d SSH-1 keys", s->nkeys);
		for (s->keyi = 0; s->keyi < s->nkeys; s->keyi++) {
		    logeventf(ssh, "Trying Pageant key #%d", s->keyi);
		    if (s->publickey_blob &&
			!memcmp(s->p, s->publickey_blob,
				s->publickey_bloblen)) {
			logevent("This key matches configured key file");
			s->tried_publickey = 1;
		    }
		    s->p += 4;
		    {
			int n, ok = FALSE;
			do {	       /* do while (0) to make breaking easy */
			    n = ssh1_read_bignum
				(s->p, s->responselen-(s->p-s->response),
				 &s->key.exponent);
			    if (n < 0)
				break;
			    s->p += n;
			    n = ssh1_read_bignum
				(s->p, s->responselen-(s->p-s->response),
				 &s->key.modulus);
			    if (n < 0)
			    break;
			    s->p += n;
			    if (s->responselen - (s->p-s->response) < 4)
				break;
			    s->commentlen = GET_32BIT(s->p);
			    s->p += 4;
			    if (s->responselen - (s->p-s->response) <
				s->commentlen)
				break;
			    s->commentp = (char *)s->p;
			    s->p += s->commentlen;
			    ok = TRUE;
			} while (0);
			if (!ok) {
			    logevent("Pageant key list packet was truncated");
			    break;
			}
		    }
		    send_packet(ssh, SSH1_CMSG_AUTH_RSA,
				PKT_BIGNUM, s->key.modulus, PKT_END);
		    crWaitUntil(pktin);
		    if (pktin->type != SSH1_SMSG_AUTH_RSA_CHALLENGE) {
			logevent("Key refused");
			continue;
		    }
		    logevent("Received RSA challenge");
		    if ((s->challenge = ssh1_pkt_getmp(pktin)) == NULL) {
			bombout(("Server's RSA challenge was badly formatted"));
			crStop(0);
		    }

		    {
			char *agentreq, *q, *ret;
			void *vret;
			int len, retlen;
			len = 1 + 4;   /* message type, bit count */
			len += ssh1_bignum_length(s->key.exponent);
			len += ssh1_bignum_length(s->key.modulus);
			len += ssh1_bignum_length(s->challenge);
			len += 16;     /* session id */
			len += 4;      /* response format */
			agentreq = snewn(4 + len, char);
			PUT_32BIT(agentreq, len);
			q = agentreq + 4;
			*q++ = SSH1_AGENTC_RSA_CHALLENGE;
			PUT_32BIT(q, bignum_bitcount(s->key.modulus));
			q += 4;
			q += ssh1_write_bignum(q, s->key.exponent);
			q += ssh1_write_bignum(q, s->key.modulus);
			q += ssh1_write_bignum(q, s->challenge);
			memcpy(q, s->session_id, 16);
			q += 16;
			PUT_32BIT(q, 1);	/* response format */
			if (!agent_query(agentreq, len + 4, &vret, &retlen,
					 ssh_agent_callback, ssh)) {
			    sfree(agentreq);
			    do {
				crReturn(0);
				if (pktin) {
				    bombout(("Unexpected data from server"
					     " while waiting for agent"
					     " response"));
				    crStop(0);
				}
			    } while (pktin || inlen > 0);
			    vret = ssh->agent_response;
			    retlen = ssh->agent_response_len;
			} else
			    sfree(agentreq);
			ret = vret;
			if (ret) {
			    if (ret[4] == SSH1_AGENT_RSA_RESPONSE) {
				logevent("Sending Pageant's response");
				send_packet(ssh, SSH1_CMSG_AUTH_RSA_RESPONSE,
					    PKT_DATA, ret + 5, 16,
					    PKT_END);
				sfree(ret);
				crWaitUntil(pktin);
				if (pktin->type == SSH1_SMSG_SUCCESS) {
				    logevent
					("Pageant's response accepted");
				    if (flags & FLAG_VERBOSE) {
					c_write_str(ssh, "Authenticated using"
						    " RSA key \"");
					c_write(ssh, s->commentp,
						s->commentlen);
					c_write_str(ssh, "\" from agent\r\n");
				    }
				    s->authed = TRUE;
				} else
				    logevent
					("Pageant's response not accepted");
			    } else {
				logevent
				    ("Pageant failed to answer challenge");
				sfree(ret);
			    }
			} else {
			    logevent("No reply received from Pageant");
			}
		    }
		    freebn(s->key.exponent);
		    freebn(s->key.modulus);
		    freebn(s->challenge);
		    if (s->authed)
			break;
		}
		sfree(s->response);
	    }
	    if (s->authed)
		break;
	}
	if (!filename_is_null(ssh->cfg.keyfile) && !s->tried_publickey)
	    s->pwpkt_type = SSH1_CMSG_AUTH_RSA;

	if (ssh->cfg.try_tis_auth &&
	    (s->supported_auths_mask & (1 << SSH1_AUTH_TIS)) &&
	    !s->tis_auth_refused) {
	    s->pwpkt_type = SSH1_CMSG_AUTH_TIS_RESPONSE;
	    logevent("Requested TIS authentication");
	    send_packet(ssh, SSH1_CMSG_AUTH_TIS, PKT_END);
	    crWaitUntil(pktin);
	    if (pktin->type != SSH1_SMSG_AUTH_TIS_CHALLENGE) {
		logevent("TIS authentication declined");
		if (flags & FLAG_INTERACTIVE)
		    c_write_str(ssh, "TIS authentication refused.\r\n");
		s->tis_auth_refused = 1;
		continue;
	    } else {
		char *challenge;
		int challengelen;

		ssh_pkt_getstring(pktin, &challenge, &challengelen);
		if (!challenge) {
		    bombout(("TIS challenge packet was badly formed"));
		    crStop(0);
		}
		c_write_str(ssh, "Using TIS authentication.\r\n");
		logevent("Received TIS challenge");
		if (challengelen > sizeof(s->prompt) - 1)
		    challengelen = sizeof(s->prompt) - 1;/* prevent overrun */
		memcpy(s->prompt, challenge, challengelen);
		/* Prompt heuristic comes from OpenSSH */
		strncpy(s->prompt + challengelen,
		        memchr(s->prompt, '\n', challengelen) ?
			"": "\r\nResponse: ",
			(sizeof s->prompt) - challengelen);
		s->prompt[(sizeof s->prompt) - 1] = '\0';
	    }
	}
	if (ssh->cfg.try_tis_auth &&
	    (s->supported_auths_mask & (1 << SSH1_AUTH_CCARD)) &&
	    !s->ccard_auth_refused) {
	    s->pwpkt_type = SSH1_CMSG_AUTH_CCARD_RESPONSE;
	    logevent("Requested CryptoCard authentication");
	    send_packet(ssh, SSH1_CMSG_AUTH_CCARD, PKT_END);
	    crWaitUntil(pktin);
	    if (pktin->type != SSH1_SMSG_AUTH_CCARD_CHALLENGE) {
		logevent("CryptoCard authentication declined");
		c_write_str(ssh, "CryptoCard authentication refused.\r\n");
		s->ccard_auth_refused = 1;
		continue;
	    } else {
		char *challenge;
		int challengelen;

		ssh_pkt_getstring(pktin, &challenge, &challengelen);
		if (!challenge) {
		    bombout(("CryptoCard challenge packet was badly formed"));
		    crStop(0);
		}
		c_write_str(ssh, "Using CryptoCard authentication.\r\n");
		logevent("Received CryptoCard challenge");
		if (challengelen > sizeof(s->prompt) - 1)
		    challengelen = sizeof(s->prompt) - 1;/* prevent overrun */
		memcpy(s->prompt, challenge, challengelen);
		strncpy(s->prompt + challengelen,
		        memchr(s->prompt, '\n', challengelen) ?
			"" : "\r\nResponse: ",
			sizeof(s->prompt) - challengelen);
		s->prompt[sizeof(s->prompt) - 1] = '\0';
	    }
	}
	if (s->pwpkt_type == SSH1_CMSG_AUTH_PASSWORD) {
	    sprintf(s->prompt, "%.90s@%.90s's password: ",
		    s->username, ssh->savedhost);
	}
	if (s->pwpkt_type == SSH1_CMSG_AUTH_RSA) {
	    char *comment = NULL;
	    int type;
	    if (flags & FLAG_VERBOSE)
		c_write_str(ssh, "Trying public key authentication.\r\n");
	    logeventf(ssh, "Trying public key \"%s\"",
		      filename_to_str(&ssh->cfg.keyfile));
	    type = key_type(&ssh->cfg.keyfile);
	    if (type != SSH_KEYTYPE_SSH1) {
		char *msg = dupprintf("Key is of wrong type (%s)",
				      key_type_to_str(type));
		logevent(msg);
		c_write_str(ssh, msg);
		c_write_str(ssh, "\r\n");
		sfree(msg);
		s->tried_publickey = 1;
		continue;
	    }
	    if (!rsakey_encrypted(&ssh->cfg.keyfile, &comment)) {
		if (flags & FLAG_VERBOSE)
		    c_write_str(ssh, "No passphrase required.\r\n");
		goto tryauth;
	    }
	    sprintf(s->prompt, "Passphrase for key \"%.100s\": ", comment);
	    sfree(comment);
	}

	/*
	 * Show password prompt, having first obtained it via a TIS
	 * or CryptoCard exchange if we're doing TIS or CryptoCard
	 * authentication.
	 */
	if (ssh_get_line) {
#ifdef MPEXT
	    if (!ssh_get_line(ssh->frontend, s->prompt, s->password,
			      sizeof(s->password), TRUE)) {
#else
	    if (!ssh_get_line(s->prompt, s->password,
			      sizeof(s->password), TRUE)) {
#endif
		/*
		 * get_line failed to get a password (for example
		 * because one was supplied on the command line
		 * which has already failed to work). Terminate.
		 */
		send_packet(ssh, SSH1_MSG_DISCONNECT,
			    PKT_STR, "No more passwords available to try",
			    PKT_END);
		logevent("Unable to authenticate");
		connection_fatal(ssh->frontend, "Unable to authenticate");
		ssh->close_expected = TRUE;
                ssh_closing((Plug)ssh, NULL, 0, 0);
		crStop(1);
	    }
	} else {
	    /* Prompt may have come from server. We've munged it a bit, so
	     * we know it to be zero-terminated at least once. */
	    int ret;		       /* need not be saved over crReturn */
	    c_write_untrusted(ssh, s->prompt, strlen(s->prompt));
	    s->pos = 0;

	    setup_userpass_input(ssh, s->password, sizeof(s->password), 0);
	    do {
		crWaitUntil(!pktin);
		ret = process_userpass_input(ssh, in, inlen);
	    } while (ret == 0);
	    if (ret < 0)
		cleanup_exit(0);
	    c_write_str(ssh, "\r\n");
	}

      tryauth:
	if (s->pwpkt_type == SSH1_CMSG_AUTH_RSA) {
	    /*
	     * Try public key authentication with the specified
	     * key file.
	     */
	    s->tried_publickey = 1;
	    
	    {
		const char *error = NULL;
		int ret = loadrsakey(&ssh->cfg.keyfile, &s->key, s->password,
				     &error);
		if (ret == 0) {
		    c_write_str(ssh, "Couldn't load private key from ");
		    c_write_str(ssh, filename_to_str(&ssh->cfg.keyfile));
		    c_write_str(ssh, " (");
		    c_write_str(ssh, error);
		    c_write_str(ssh, ").\r\n");
		    continue;	       /* go and try password */
		}
		if (ret == -1) {
		    c_write_str(ssh, "Wrong passphrase.\r\n");
		    s->tried_publickey = 0;
		    continue;	       /* try again */
		}
	    }

	    /*
	     * Send a public key attempt.
	     */
	    send_packet(ssh, SSH1_CMSG_AUTH_RSA,
			PKT_BIGNUM, s->key.modulus, PKT_END);

	    crWaitUntil(pktin);
	    if (pktin->type == SSH1_SMSG_FAILURE) {
		c_write_str(ssh, "Server refused our public key.\r\n");
		continue;	       /* go and try password */
	    }
	    if (pktin->type != SSH1_SMSG_AUTH_RSA_CHALLENGE) {
		bombout(("Bizarre response to offer of public key"));
		crStop(0);
	    }

	    {
		int i;
		unsigned char buffer[32];
		Bignum challenge, response;

		if ((challenge = ssh1_pkt_getmp(pktin)) == NULL) {
		    bombout(("Server's RSA challenge was badly formatted"));
		    crStop(0);
		}
		response = rsadecrypt(challenge, &s->key);
		freebn(s->key.private_exponent);/* burn the evidence */

		for (i = 0; i < 32; i++) {
		    buffer[i] = bignum_byte(response, 31 - i);
		}

		MD5Init(&md5c);
		MD5Update(&md5c, buffer, 32);
		MD5Update(&md5c, s->session_id, 16);
		MD5Final(buffer, &md5c);

		send_packet(ssh, SSH1_CMSG_AUTH_RSA_RESPONSE,
			    PKT_DATA, buffer, 16, PKT_END);

		freebn(challenge);
		freebn(response);
	    }

	    crWaitUntil(pktin);
	    if (pktin->type == SSH1_SMSG_FAILURE) {
		if (flags & FLAG_VERBOSE)
		    c_write_str(ssh, "Failed to authenticate with"
				" our public key.\r\n");
		continue;	       /* go and try password */
	    } else if (pktin->type != SSH1_SMSG_SUCCESS) {
		bombout(("Bizarre response to RSA authentication response"));
		crStop(0);
	    }

	    break;		       /* we're through! */
	} else {
	    if (s->pwpkt_type == SSH1_CMSG_AUTH_PASSWORD) {
		/*
		 * Defence against traffic analysis: we send a
		 * whole bunch of packets containing strings of
		 * different lengths. One of these strings is the
		 * password, in a SSH1_CMSG_AUTH_PASSWORD packet.
		 * The others are all random data in
		 * SSH1_MSG_IGNORE packets. This way a passive
		 * listener can't tell which is the password, and
		 * hence can't deduce the password length.
		 * 
		 * Anybody with a password length greater than 16
		 * bytes is going to have enough entropy in their
		 * password that a listener won't find it _that_
		 * much help to know how long it is. So what we'll
		 * do is:
		 * 
		 *  - if password length < 16, we send 15 packets
		 *    containing string lengths 1 through 15
		 * 
		 *  - otherwise, we let N be the nearest multiple
		 *    of 8 below the password length, and send 8
		 *    packets containing string lengths N through
		 *    N+7. This won't obscure the order of
		 *    magnitude of the password length, but it will
		 *    introduce a bit of extra uncertainty.
		 * 
		 * A few servers (the old 1.2.18 through 1.2.22)
		 * can't deal with SSH1_MSG_IGNORE. For these
		 * servers, we need an alternative defence. We make
		 * use of the fact that the password is interpreted
		 * as a C string: so we can append a NUL, then some
		 * random data.
		 * 
		 * One server (a Cisco one) can deal with neither
		 * SSH1_MSG_IGNORE _nor_ a padded password string.
		 * For this server we are left with no defences
		 * against password length sniffing.
		 */
		if (!(ssh->remote_bugs & BUG_CHOKES_ON_SSH1_IGNORE)) {
		    /*
		     * The server can deal with SSH1_MSG_IGNORE, so
		     * we can use the primary defence.
		     */
		    int bottom, top, pwlen, i;
		    char *randomstr;

		    pwlen = strlen(s->password);
		    if (pwlen < 16) {
			bottom = 0;    /* zero length passwords are OK! :-) */
			top = 15;
		    } else {
			bottom = pwlen & ~7;
			top = bottom + 7;
		    }

		    assert(pwlen >= bottom && pwlen <= top);

		    randomstr = snewn(top + 1, char);

		    for (i = bottom; i <= top; i++) {
			if (i == pwlen) {
			    defer_packet(ssh, s->pwpkt_type,
					 PKTT_PASSWORD, PKT_STR, s->password,
					 PKTT_OTHER, PKT_END);
			} else {
			    for (j = 0; j < i; j++) {
				do {
				    randomstr[j] = random_byte();
				} while (randomstr[j] == '\0');
			    }
			    randomstr[i] = '\0';
			    defer_packet(ssh, SSH1_MSG_IGNORE,
					 PKT_STR, randomstr, PKT_END);
			}
		    }
		    logevent("Sending password with camouflage packets");
		    ssh_pkt_defersend(ssh);
		    sfree(randomstr);
		} 
		else if (!(ssh->remote_bugs & BUG_NEEDS_SSH1_PLAIN_PASSWORD)) {
		    /*
		     * The server can't deal with SSH1_MSG_IGNORE
		     * but can deal with padded passwords, so we
		     * can use the secondary defence.
		     */
		    char string[64];
		    char *ss;
		    int len;

		    len = strlen(s->password);
		    if (len < sizeof(string)) {
			ss = string;
			strcpy(string, s->password);
			len++;	       /* cover the zero byte */
			while (len < sizeof(string)) {
			    string[len++] = (char) random_byte();
			}
		    } else {
			ss = s->password;
		    }
		    logevent("Sending length-padded password");
		    send_packet(ssh, s->pwpkt_type, PKTT_PASSWORD,
				PKT_INT, len, PKT_DATA, ss, len,
			       	PKTT_OTHER, PKT_END);
		} else {
		    /*
		     * The server has _both_
		     * BUG_CHOKES_ON_SSH1_IGNORE and
		     * BUG_NEEDS_SSH1_PLAIN_PASSWORD. There is
		     * therefore nothing we can do.
		     */
		    int len;
		    len = strlen(s->password);
		    logevent("Sending unpadded password");
		    send_packet(ssh, s->pwpkt_type,
				PKTT_PASSWORD, PKT_INT, len,
				PKT_DATA, s->password, len,
				PKTT_OTHER, PKT_END);
		}
	    } else {
		send_packet(ssh, s->pwpkt_type, PKTT_PASSWORD,
			    PKT_STR, s->password, PKTT_OTHER, PKT_END);
	    }
	}
	logevent("Sent password");
	memset(s->password, 0, strlen(s->password));
	crWaitUntil(pktin);
	if (pktin->type == SSH1_SMSG_FAILURE) {
	    if (flags & FLAG_VERBOSE)
		c_write_str(ssh, "Access denied\r\n");
	    logevent("Authentication refused");
	} else if (pktin->type != SSH1_SMSG_SUCCESS) {
	    bombout(("Strange packet received, type %d", pktin->type));
	    crStop(0);
	}
    }

    logevent("Authentication successful");

    crFinish(1);
}

void sshfwd_close(struct ssh_channel *c)
{
    Ssh ssh = c->ssh;

    if (ssh->state == SSH_STATE_CLOSED)
	return;

    if (c && !c->closes) {
	/*
	 * If halfopen is true, we have sent
	 * CHANNEL_OPEN for this channel, but it hasn't even been
	 * acknowledged by the server. So we must set a close flag
	 * on it now, and then when the server acks the channel
	 * open, we can close it then.
	 */
	if (!c->halfopen) {
	    if (ssh->version == 1) {
		send_packet(ssh, SSH1_MSG_CHANNEL_CLOSE, PKT_INT, c->remoteid,
			    PKT_END);
	    } else {
		struct Packet *pktout;
		pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_CLOSE);
		ssh2_pkt_adduint32(pktout, c->remoteid);
		ssh2_pkt_send(ssh, pktout);
	    }
	}
	c->closes = 1;		       /* sent MSG_CLOSE */
	if (c->type == CHAN_X11) {
	    c->u.x11.s = NULL;
	    logevent("Forwarded X11 connection terminated");
	} else if (c->type == CHAN_SOCKDATA ||
		   c->type == CHAN_SOCKDATA_DORMANT) {
	    c->u.pfd.s = NULL;
	    logevent("Forwarded port closed");
	}
    }
}

int sshfwd_write(struct ssh_channel *c, char *buf, int len)
{
    Ssh ssh = c->ssh;

    if (ssh->state == SSH_STATE_CLOSED)
	return 0;

    if (ssh->version == 1) {
	send_packet(ssh, SSH1_MSG_CHANNEL_DATA,
		    PKT_INT, c->remoteid,
		    PKTT_DATA,
		    PKT_INT, len, PKT_DATA, buf, len,
		    PKTT_OTHER, PKT_END);
	/*
	 * In SSH-1 we can return 0 here - implying that forwarded
	 * connections are never individually throttled - because
	 * the only circumstance that can cause throttling will be
	 * the whole SSH connection backing up, in which case
	 * _everything_ will be throttled as a whole.
	 */
	return 0;
    } else {
	ssh2_add_channel_data(c, buf, len);
	return ssh2_try_send(c);
    }
}

void sshfwd_unthrottle(struct ssh_channel *c, int bufsize)
{
    Ssh ssh = c->ssh;

    if (ssh->state == SSH_STATE_CLOSED)
	return;

    if (ssh->version == 1) {
	if (c->v.v1.throttling && bufsize < SSH1_BUFFER_LIMIT) {
	    c->v.v1.throttling = 0;
	    ssh1_throttle(ssh, -1);
	}
    } else {
	ssh2_set_window(c, OUR_V2_WINSIZE - bufsize);
    }
}

static void ssh_queueing_handler(Ssh ssh, struct Packet *pktin)
{
    struct queued_handler *qh = ssh->qhead;

    assert(qh != NULL);

    assert(pktin->type == qh->msg1 || pktin->type == qh->msg2);

    if (qh->msg1 > 0) {
	assert(ssh->packet_dispatch[qh->msg1] == ssh_queueing_handler);
	ssh->packet_dispatch[qh->msg1] = NULL;
    }
    if (qh->msg2 > 0) {
	assert(ssh->packet_dispatch[qh->msg2] == ssh_queueing_handler);
	ssh->packet_dispatch[qh->msg2] = NULL;
    }

    if (qh->next) {
	ssh->qhead = qh->next;

	if (ssh->qhead->msg1 > 0) {
	    assert(ssh->packet_dispatch[ssh->qhead->msg1] == NULL);
	    ssh->packet_dispatch[ssh->qhead->msg1] = ssh_queueing_handler;
	}
	if (ssh->qhead->msg2 > 0) {
	    assert(ssh->packet_dispatch[ssh->qhead->msg2] == NULL);
	    ssh->packet_dispatch[ssh->qhead->msg2] = ssh_queueing_handler;
	}
    } else {
	ssh->qhead = ssh->qtail = NULL;
	ssh->packet_dispatch[pktin->type] = NULL;
    }

    qh->handler(ssh, pktin, qh->ctx);

    sfree(qh);
}

static void ssh_queue_handler(Ssh ssh, int msg1, int msg2,
			      chandler_fn_t handler, void *ctx)
{
    struct queued_handler *qh;

    qh = snew(struct queued_handler);
    qh->msg1 = msg1;
    qh->msg2 = msg2;
    qh->handler = handler;
    qh->ctx = ctx;
    qh->next = NULL;

    if (ssh->qtail == NULL) {
	ssh->qhead = qh;

	if (qh->msg1 > 0) {
	    assert(ssh->packet_dispatch[qh->msg1] == NULL);
	    ssh->packet_dispatch[qh->msg1] = ssh_queueing_handler;
	}
	if (qh->msg2 > 0) {
	    assert(ssh->packet_dispatch[qh->msg2] == NULL);
	    ssh->packet_dispatch[qh->msg2] = ssh_queueing_handler;
	}
    } else {
	ssh->qtail->next = qh;
    }
    ssh->qtail = qh;
}

static void ssh_rportfwd_succfail(Ssh ssh, struct Packet *pktin, void *ctx)
{
    struct ssh_rportfwd *rpf, *pf = (struct ssh_rportfwd *)ctx;

    if (pktin->type == (ssh->version == 1 ? SSH1_SMSG_SUCCESS :
			SSH2_MSG_REQUEST_SUCCESS)) {
	logeventf(ssh, "Remote port forwarding from %s enabled",
		  pf->sportdesc);
    } else {
	logeventf(ssh, "Remote port forwarding from %s refused",
		  pf->sportdesc);

	rpf = del234(ssh->rportfwds, pf);
	assert(rpf == pf);
	free_rportfwd(pf);
    }
}

static void ssh_setup_portfwd(Ssh ssh, const Config *cfg)
{
    const char *portfwd_strptr = cfg->portfwd;
    struct ssh_portfwd *epf;
    int i;

    if (!ssh->portfwds) {
	ssh->portfwds = newtree234(ssh_portcmp);
    } else {
	/*
	 * Go through the existing port forwardings and tag them
	 * with status==DESTROY. Any that we want to keep will be
	 * re-enabled (status==KEEP) as we go through the
	 * configuration and find out which bits are the same as
	 * they were before.
	 */
	struct ssh_portfwd *epf;
	int i;
	for (i = 0; (epf = index234(ssh->portfwds, i)) != NULL; i++)
	    epf->status = DESTROY;
    }

    while (*portfwd_strptr) {
	char address_family, type;
	int sport,dport,sserv,dserv;
	char sports[256], dports[256], saddr[256], host[256];
	int n;

	address_family = 'A';
	type = 'L';
	if (*portfwd_strptr == 'A' ||
	    *portfwd_strptr == '4' ||
	    *portfwd_strptr == '6')
	    address_family = *portfwd_strptr++;
	if (*portfwd_strptr == 'L' ||
	    *portfwd_strptr == 'R' ||
	    *portfwd_strptr == 'D')
	    type = *portfwd_strptr++;

	saddr[0] = '\0';

	n = 0;
	while (*portfwd_strptr && *portfwd_strptr != '\t') {
	    if (*portfwd_strptr == ':') {
		/*
		 * We've seen a colon in the middle of the
		 * source port number. This means that
		 * everything we've seen until now is the
		 * source _address_, so we'll move it into
		 * saddr and start sports from the beginning
		 * again.
		 */
		portfwd_strptr++;
		sports[n] = '\0';
		if (ssh->version == 1 && type == 'R') {
		    logeventf(ssh, "SSH-1 cannot handle remote source address "
			      "spec \"%s\"; ignoring", sports);
		} else
		    strcpy(saddr, sports);
		n = 0;
	    }
	    if (n < lenof(sports)-1) sports[n++] = *portfwd_strptr++;
	}
	sports[n] = 0;
	if (type != 'D') {
	    if (*portfwd_strptr == '\t')
		portfwd_strptr++;
	    n = 0;
	    while (*portfwd_strptr && *portfwd_strptr != ':') {
		if (n < lenof(host)-1) host[n++] = *portfwd_strptr++;
	    }
	    host[n] = 0;
	    if (*portfwd_strptr == ':')
		portfwd_strptr++;
	    n = 0;
	    while (*portfwd_strptr) {
		if (n < lenof(dports)-1) dports[n++] = *portfwd_strptr++;
	    }
	    dports[n] = 0;
	    portfwd_strptr++;
	    dport = atoi(dports);
	    dserv = 0;
	    if (dport == 0) {
		dserv = 1;
		dport = net_service_lookup(dports);
		if (!dport) {
		    logeventf(ssh, "Service lookup failed for destination"
			      " port \"%s\"", dports);
		}
	    }
	} else {
	    while (*portfwd_strptr) portfwd_strptr++;
	    host[0] = 0;
	    dports[0] = 0;
	    dport = dserv = -1;
	    portfwd_strptr++;	       /* eat the NUL and move to next one */
	}
	sport = atoi(sports);
	sserv = 0;
	if (sport == 0) {
	    sserv = 1;
	    sport = net_service_lookup(sports);
	    if (!sport) {
		logeventf(ssh, "Service lookup failed for source"
			  " port \"%s\"", sports);
	    }
	}
	if (sport && dport) {
	    /* Set up a description of the source port. */
	    struct ssh_portfwd *pfrec, *epfrec;

	    pfrec = snew(struct ssh_portfwd);
	    pfrec->type = type;
	    pfrec->saddr = *saddr ? dupstr(saddr) : NULL;
	    pfrec->sserv = sserv ? dupstr(sports) : NULL;
	    pfrec->sport = sport;
	    pfrec->daddr = *host ? dupstr(host) : NULL;
	    pfrec->dserv = dserv ? dupstr(dports) : NULL;
	    pfrec->dport = dport;
	    pfrec->local = NULL;
	    pfrec->remote = NULL;
	    pfrec->addressfamily = (address_family == '4' ? ADDRTYPE_IPV4 :
				    address_family == '6' ? ADDRTYPE_IPV6 :
				    ADDRTYPE_UNSPEC);

	    epfrec = add234(ssh->portfwds, pfrec);
	    if (epfrec != pfrec) {
		/*
		 * We already have a port forwarding with precisely
		 * these parameters. Hence, no need to do anything;
		 * simply tag the existing one as KEEP.
		 */
		epfrec->status = KEEP;
		free_portfwd(pfrec);
	    } else {
		pfrec->status = CREATE;
	    }
	}
    }

    /*
     * Now go through and destroy any port forwardings which were
     * not re-enabled.
     */
    for (i = 0; (epf = index234(ssh->portfwds, i)) != NULL; i++)
	if (epf->status == DESTROY) {
	    char *message;

	    message = dupprintf("%s port forwarding from %s%s%d",
				epf->type == 'L' ? "local" :
				epf->type == 'R' ? "remote" : "dynamic",
				epf->saddr ? epf->saddr : "",
				epf->saddr ? ":" : "",
				epf->sport);

	    if (epf->type != 'D') {
		char *msg2 = dupprintf("%s to %s:%d", message,
				       epf->daddr, epf->dport);
		sfree(message);
		message = msg2;
	    }

	    logeventf(ssh, "Cancelling %s", message);
	    sfree(message);

	    if (epf->remote) {
		struct ssh_rportfwd *rpf = epf->remote;
		struct Packet *pktout;

		/*
		 * Cancel the port forwarding at the server
		 * end.
		 */
		if (ssh->version == 1) {
		    /*
		     * We cannot cancel listening ports on the
		     * server side in SSH-1! There's no message
		     * to support it. Instead, we simply remove
		     * the rportfwd record from the local end
		     * so that any connections the server tries
		     * to make on it are rejected.
		     */
		} else {
		    pktout = ssh2_pkt_init(SSH2_MSG_GLOBAL_REQUEST);
		    ssh2_pkt_addstring(pktout, "cancel-tcpip-forward");
		    ssh2_pkt_addbool(pktout, 0);/* _don't_ want reply */
		    if (epf->saddr) {
			ssh2_pkt_addstring(pktout, epf->saddr);
		    } else if (ssh->cfg.rport_acceptall) {
			/* XXX: ssh->cfg.rport_acceptall may not represent
			 * what was used to open the original connection,
			 * since it's reconfigurable. */
			ssh2_pkt_addstring(pktout, "0.0.0.0");
		    } else {
			ssh2_pkt_addstring(pktout, "127.0.0.1");
		    }
		    ssh2_pkt_adduint32(pktout, epf->sport);
		    ssh2_pkt_send(ssh, pktout);
		}

		del234(ssh->rportfwds, rpf);
		free_rportfwd(rpf);
	    } else if (epf->local) {
		pfd_terminate(epf->local);
	    }

	    delpos234(ssh->portfwds, i);
	    free_portfwd(epf);
	    i--;		       /* so we don't skip one in the list */
	}

    /*
     * And finally, set up any new port forwardings (status==CREATE).
     */
    for (i = 0; (epf = index234(ssh->portfwds, i)) != NULL; i++)
	if (epf->status == CREATE) {
	    char *sportdesc, *dportdesc;
	    sportdesc = dupprintf("%s%s%s%s%d%s",
				  epf->saddr ? epf->saddr : "",
				  epf->saddr ? ":" : "",
				  epf->sserv ? epf->sserv : "",
				  epf->sserv ? "(" : "",
				  epf->sport,
				  epf->sserv ? ")" : "");
	    if (epf->type == 'D') {
		dportdesc = NULL;
	    } else {
		dportdesc = dupprintf("%s:%s%s%d%s",
				      epf->daddr,
				      epf->dserv ? epf->dserv : "",
				      epf->dserv ? "(" : "",
				      epf->dport,
				      epf->dserv ? ")" : "");
	    }

	    if (epf->type == 'L') {
		const char *err = pfd_addforward(epf->daddr, epf->dport,
						 epf->saddr, epf->sport,
						 ssh, cfg,
						 &epf->local,
						 epf->addressfamily);

		logeventf(ssh, "Local %sport %s forwarding to %s%s%s",
			  epf->addressfamily == ADDRTYPE_IPV4 ? "IPv4 " :
			  epf->addressfamily == ADDRTYPE_IPV6 ? "IPv6 " : "",
			  sportdesc, dportdesc,
			  err ? " failed: " : "", err ? err : "");
	    } else if (epf->type == 'D') {
		const char *err = pfd_addforward(NULL, -1,
						 epf->saddr, epf->sport,
						 ssh, cfg,
						 &epf->local,
						 epf->addressfamily);

		logeventf(ssh, "Local %sport %s SOCKS dynamic forwarding%s%s",
			  epf->addressfamily == ADDRTYPE_IPV4 ? "IPv4 " :
			  epf->addressfamily == ADDRTYPE_IPV6 ? "IPv6 " : "",
			  sportdesc,
			  err ? " failed: " : "", err ? err : "");
	    } else {
		struct ssh_rportfwd *pf;

		/*
		 * Ensure the remote port forwardings tree exists.
		 */
		if (!ssh->rportfwds) {
		    if (ssh->version == 1)
			ssh->rportfwds = newtree234(ssh_rportcmp_ssh1);
		    else
			ssh->rportfwds = newtree234(ssh_rportcmp_ssh2);
		}

		pf = snew(struct ssh_rportfwd);
		strncpy(pf->dhost, epf->daddr, lenof(pf->dhost)-1);
		pf->dhost[lenof(pf->dhost)-1] = '\0';
		pf->dport = epf->dport;
		pf->sport = epf->sport;
		if (add234(ssh->rportfwds, pf) != pf) {
		    logeventf(ssh, "Duplicate remote port forwarding to %s:%d",
			      epf->daddr, epf->dport);
		    sfree(pf);
		} else {
		    logeventf(ssh, "Requesting remote port %s"
			      " forward to %s", sportdesc, dportdesc);

		    pf->sportdesc = sportdesc;
		    sportdesc = NULL;
		    epf->remote = pf;
		    pf->pfrec = epf;

		    if (ssh->version == 1) {
			send_packet(ssh, SSH1_CMSG_PORT_FORWARD_REQUEST,
				    PKT_INT, epf->sport,
				    PKT_STR, epf->daddr,
				    PKT_INT, epf->dport,
				    PKT_END);
			ssh_queue_handler(ssh, SSH1_SMSG_SUCCESS,
					  SSH1_SMSG_FAILURE,
					  ssh_rportfwd_succfail, pf);
		    } else {
			struct Packet *pktout;
			pktout = ssh2_pkt_init(SSH2_MSG_GLOBAL_REQUEST);
			ssh2_pkt_addstring(pktout, "tcpip-forward");
			ssh2_pkt_addbool(pktout, 1);/* want reply */
			if (epf->saddr) {
			    ssh2_pkt_addstring(pktout, epf->saddr);
			} else if (cfg->rport_acceptall) {
			    ssh2_pkt_addstring(pktout, "0.0.0.0");
			} else {
			    ssh2_pkt_addstring(pktout, "127.0.0.1");
			}
			ssh2_pkt_adduint32(pktout, epf->sport);
			ssh2_pkt_send(ssh, pktout);

			ssh_queue_handler(ssh, SSH2_MSG_REQUEST_SUCCESS,
					  SSH2_MSG_REQUEST_FAILURE,
					  ssh_rportfwd_succfail, pf);
		    }
		}
	    }
	    sfree(sportdesc);
	    sfree(dportdesc);
	}
}

static void ssh1_smsg_stdout_stderr_data(Ssh ssh, struct Packet *pktin)
{
    char *string;
    int stringlen, bufsize;

    ssh_pkt_getstring(pktin, &string, &stringlen);
    if (string == NULL) {
	bombout(("Incoming terminal data packet was badly formed"));
	return;
    }

    bufsize = from_backend(ssh->frontend, pktin->type == SSH1_SMSG_STDERR_DATA,
			   string, stringlen
#ifdef MPEXT
			   , 1
#endif
			   );
    if (!ssh->v1_stdout_throttling && bufsize > SSH1_BUFFER_LIMIT) {
	ssh->v1_stdout_throttling = 1;
	ssh1_throttle(ssh, +1);
    }
}

static void ssh1_smsg_x11_open(Ssh ssh, struct Packet *pktin)
{
    /* Remote side is trying to open a channel to talk to our
     * X-Server. Give them back a local channel number. */
    struct ssh_channel *c;
    int remoteid = ssh_pkt_getuint32(pktin);

    logevent("Received X11 connect request");
    /* Refuse if X11 forwarding is disabled. */
    if (!ssh->X11_fwd_enabled) {
	send_packet(ssh, SSH1_MSG_CHANNEL_OPEN_FAILURE,
		    PKT_INT, remoteid, PKT_END);
	logevent("Rejected X11 connect request");
    } else {
	c = snew(struct ssh_channel);
	c->ssh = ssh;

	if (x11_init(&c->u.x11.s, ssh->cfg.x11_display, c,
		     ssh->x11auth, NULL, -1, &ssh->cfg) != NULL) {
	    logevent("Opening X11 forward connection failed");
	    sfree(c);
	    send_packet(ssh, SSH1_MSG_CHANNEL_OPEN_FAILURE,
			PKT_INT, remoteid, PKT_END);
	} else {
	    logevent
		("Opening X11 forward connection succeeded");
	    c->remoteid = remoteid;
	    c->halfopen = FALSE;
	    c->localid = alloc_channel_id(ssh);
	    c->closes = 0;
	    c->v.v1.throttling = 0;
	    c->type = CHAN_X11;	/* identify channel type */
	    add234(ssh->channels, c);
	    send_packet(ssh, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION,
			PKT_INT, c->remoteid, PKT_INT,
			c->localid, PKT_END);
	    logevent("Opened X11 forward channel");
	}
    }
}

static void ssh1_smsg_agent_open(Ssh ssh, struct Packet *pktin)
{
    /* Remote side is trying to open a channel to talk to our
     * agent. Give them back a local channel number. */
    struct ssh_channel *c;
    int remoteid = ssh_pkt_getuint32(pktin);

    /* Refuse if agent forwarding is disabled. */
    if (!ssh->agentfwd_enabled) {
	send_packet(ssh, SSH1_MSG_CHANNEL_OPEN_FAILURE,
		    PKT_INT, remoteid, PKT_END);
    } else {
	c = snew(struct ssh_channel);
	c->ssh = ssh;
	c->remoteid = remoteid;
	c->halfopen = FALSE;
	c->localid = alloc_channel_id(ssh);
	c->closes = 0;
	c->v.v1.throttling = 0;
	c->type = CHAN_AGENT;	/* identify channel type */
	c->u.a.lensofar = 0;
	add234(ssh->channels, c);
	send_packet(ssh, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION,
		    PKT_INT, c->remoteid, PKT_INT, c->localid,
		    PKT_END);
    }
}

static void ssh1_msg_port_open(Ssh ssh, struct Packet *pktin)
{
    /* Remote side is trying to open a channel to talk to a
     * forwarded port. Give them back a local channel number. */
    struct ssh_channel *c;
    struct ssh_rportfwd pf, *pfp;
    int remoteid;
    int hostsize, port;
    char *host;
    const char *e;
    c = snew(struct ssh_channel);
    c->ssh = ssh;

    remoteid = ssh_pkt_getuint32(pktin);
    ssh_pkt_getstring(pktin, &host, &hostsize);
    port = ssh_pkt_getuint32(pktin);

    if (hostsize >= lenof(pf.dhost))
	hostsize = lenof(pf.dhost)-1;
    memcpy(pf.dhost, host, hostsize);
    pf.dhost[hostsize] = '\0';
    pf.dport = port;
    pfp = find234(ssh->rportfwds, &pf, NULL);

    if (pfp == NULL) {
	logeventf(ssh, "Rejected remote port open request for %s:%d",
		  pf.dhost, port);
	send_packet(ssh, SSH1_MSG_CHANNEL_OPEN_FAILURE,
		    PKT_INT, remoteid, PKT_END);
    } else {
	logeventf(ssh, "Received remote port open request for %s:%d",
		  pf.dhost, port);
	e = pfd_newconnect(&c->u.pfd.s, pf.dhost, port,
			   c, &ssh->cfg, pfp->pfrec->addressfamily);
	if (e != NULL) {
	    logeventf(ssh, "Port open failed: %s", e);
	    sfree(c);
	    send_packet(ssh, SSH1_MSG_CHANNEL_OPEN_FAILURE,
			PKT_INT, remoteid, PKT_END);
	} else {
	    c->remoteid = remoteid;
	    c->halfopen = FALSE;
	    c->localid = alloc_channel_id(ssh);
	    c->closes = 0;
	    c->v.v1.throttling = 0;
	    c->type = CHAN_SOCKDATA;	/* identify channel type */
	    add234(ssh->channels, c);
	    send_packet(ssh, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION,
			PKT_INT, c->remoteid, PKT_INT,
			c->localid, PKT_END);
	    logevent("Forwarded port opened successfully");
	}
    }
}

static void ssh1_msg_channel_open_confirmation(Ssh ssh, struct Packet *pktin)
{
    unsigned int remoteid = ssh_pkt_getuint32(pktin);
    unsigned int localid = ssh_pkt_getuint32(pktin);
    struct ssh_channel *c;

    c = find234(ssh->channels, &remoteid, ssh_channelfind);
    if (c && c->type == CHAN_SOCKDATA_DORMANT) {
	c->remoteid = localid;
	c->halfopen = FALSE;
	c->type = CHAN_SOCKDATA;
	c->v.v1.throttling = 0;
	pfd_confirm(c->u.pfd.s);
    }

    if (c && c->closes) {
	/*
	 * We have a pending close on this channel,
	 * which we decided on before the server acked
	 * the channel open. So now we know the
	 * remoteid, we can close it again.
	 */
	send_packet(ssh, SSH1_MSG_CHANNEL_CLOSE,
		    PKT_INT, c->remoteid, PKT_END);
    }
}

static void ssh1_msg_channel_open_failure(Ssh ssh, struct Packet *pktin)
{
    unsigned int remoteid = ssh_pkt_getuint32(pktin);
    struct ssh_channel *c;

    c = find234(ssh->channels, &remoteid, ssh_channelfind);
    if (c && c->type == CHAN_SOCKDATA_DORMANT) {
	logevent("Forwarded connection refused by server");
	pfd_close(c->u.pfd.s);
	del234(ssh->channels, c);
	sfree(c);
    }
}

static void ssh1_msg_channel_close(Ssh ssh, struct Packet *pktin)
{
    /* Remote side closes a channel. */
    unsigned i = ssh_pkt_getuint32(pktin);
    struct ssh_channel *c;
    c = find234(ssh->channels, &i, ssh_channelfind);
    if (c && !c->halfopen) {
	int closetype;
	closetype =
	    (pktin->type == SSH1_MSG_CHANNEL_CLOSE ? 1 : 2);

	if ((c->closes == 0) && (c->type == CHAN_X11)) {
	    logevent("Forwarded X11 connection terminated");
	    assert(c->u.x11.s != NULL);
	    x11_close(c->u.x11.s);
	    c->u.x11.s = NULL;
	}
	if ((c->closes == 0) && (c->type == CHAN_SOCKDATA)) {
	    logevent("Forwarded port closed");
	    assert(c->u.pfd.s != NULL);
	    pfd_close(c->u.pfd.s);
	    c->u.pfd.s = NULL;
	}

	c->closes |= (closetype << 2);   /* seen this message */
	if (!(c->closes & closetype)) {
	    send_packet(ssh, pktin->type, PKT_INT, c->remoteid,
			PKT_END);
	    c->closes |= closetype;      /* sent it too */
	}

	if (c->closes == 15) {
	    del234(ssh->channels, c);
	    sfree(c);
	}
    } else {
	bombout(("Received CHANNEL_CLOSE%s for %s channel %d\n",
		 pktin->type == SSH1_MSG_CHANNEL_CLOSE ? "" :
		 "_CONFIRMATION", c ? "half-open" : "nonexistent",
		 i));
    }
}

static void ssh1_msg_channel_data(Ssh ssh, struct Packet *pktin)
{
    /* Data sent down one of our channels. */
    int i = ssh_pkt_getuint32(pktin);
    char *p;
    int len;
    struct ssh_channel *c;

    ssh_pkt_getstring(pktin, &p, &len);

    c = find234(ssh->channels, &i, ssh_channelfind);
    if (c) {
	int bufsize = 0;
	switch (c->type) {
	  case CHAN_X11:
	    bufsize = x11_send(c->u.x11.s, p, len);
	    break;
	  case CHAN_SOCKDATA:
	    bufsize = pfd_send(c->u.pfd.s, p, len);
	    break;
	  case CHAN_AGENT:
	    /* Data for an agent message. Buffer it. */
	    while (len > 0) {
		if (c->u.a.lensofar < 4) {
		    unsigned int l = min(4 - c->u.a.lensofar, len);
		    memcpy(c->u.a.msglen + c->u.a.lensofar, p,
			   l);
		    p += l;
		    len -= l;
		    c->u.a.lensofar += l;
		}
		if (c->u.a.lensofar == 4) {
		    c->u.a.totallen =
			4 + GET_32BIT(c->u.a.msglen);
		    c->u.a.message = snewn(c->u.a.totallen,
					   unsigned char);
		    memcpy(c->u.a.message, c->u.a.msglen, 4);
		}
		if (c->u.a.lensofar >= 4 && len > 0) {
		    unsigned int l =
			min(c->u.a.totallen - c->u.a.lensofar,
			    len);
		    memcpy(c->u.a.message + c->u.a.lensofar, p,
			   l);
		    p += l;
		    len -= l;
		    c->u.a.lensofar += l;
		}
		if (c->u.a.lensofar == c->u.a.totallen) {
		    void *reply;
		    int replylen;
		    if (agent_query(c->u.a.message,
				    c->u.a.totallen,
				    &reply, &replylen,
				    ssh_agentf_callback, c))
			ssh_agentf_callback(c, reply, replylen);
		    sfree(c->u.a.message);
		    c->u.a.lensofar = 0;
		}
	    }
	    bufsize = 0;   /* agent channels never back up */
	    break;
	}
	if (!c->v.v1.throttling && bufsize > SSH1_BUFFER_LIMIT) {
	    c->v.v1.throttling = 1;
	    ssh1_throttle(ssh, +1);
	}
    }
}

static void ssh1_smsg_exit_status(Ssh ssh, struct Packet *pktin)
{
    ssh->exitcode = ssh_pkt_getuint32(pktin);
    logeventf(ssh, "Server sent command exit status %d", ssh->exitcode);
    send_packet(ssh, SSH1_CMSG_EXIT_CONFIRMATION, PKT_END);
    /*
     * In case `helpful' firewalls or proxies tack
     * extra human-readable text on the end of the
     * session which we might mistake for another
     * encrypted packet, we close the session once
     * we've sent EXIT_CONFIRMATION.
     */
    ssh->close_expected = TRUE;
    ssh_closing((Plug)ssh, NULL, 0, 0);
}

static void do_ssh1_connection(Ssh ssh, unsigned char *in, int inlen,
			       struct Packet *pktin)
{
    crBegin(ssh->do_ssh1_connection_crstate);

    ssh->packet_dispatch[SSH1_SMSG_STDOUT_DATA] = 
	ssh->packet_dispatch[SSH1_SMSG_STDERR_DATA] =
	ssh1_smsg_stdout_stderr_data;

    ssh->packet_dispatch[SSH1_MSG_CHANNEL_OPEN_CONFIRMATION] =
	ssh1_msg_channel_open_confirmation;
    ssh->packet_dispatch[SSH1_MSG_CHANNEL_OPEN_FAILURE] =
	ssh1_msg_channel_open_failure;
    ssh->packet_dispatch[SSH1_MSG_CHANNEL_CLOSE] =
	ssh->packet_dispatch[SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION] =
	ssh1_msg_channel_close;
    ssh->packet_dispatch[SSH1_MSG_CHANNEL_DATA] = ssh1_msg_channel_data;
    ssh->packet_dispatch[SSH1_SMSG_EXIT_STATUS] = ssh1_smsg_exit_status;

    if (ssh->cfg.agentfwd && agent_exists()) {
	logevent("Requesting agent forwarding");
	send_packet(ssh, SSH1_CMSG_AGENT_REQUEST_FORWARDING, PKT_END);
	do {
	    crReturnV;
	} while (!pktin);
	if (pktin->type != SSH1_SMSG_SUCCESS
	    && pktin->type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crStopV;
	} else if (pktin->type == SSH1_SMSG_FAILURE) {
	    logevent("Agent forwarding refused");
	} else {
	    logevent("Agent forwarding enabled");
	    ssh->agentfwd_enabled = TRUE;
	    ssh->packet_dispatch[SSH1_SMSG_AGENT_OPEN] = ssh1_smsg_agent_open;
	}
    }

    if (ssh->cfg.x11_forward) {
	char proto[20], data[64];
	logevent("Requesting X11 forwarding");
	ssh->x11auth = x11_invent_auth(proto, sizeof(proto),
				       data, sizeof(data), ssh->cfg.x11_auth);
        x11_get_real_auth(ssh->x11auth, ssh->cfg.x11_display);
	if (ssh->v1_local_protoflags & SSH1_PROTOFLAG_SCREEN_NUMBER) {
	    send_packet(ssh, SSH1_CMSG_X11_REQUEST_FORWARDING,
			PKT_STR, proto, PKT_STR, data,
			PKT_INT, x11_get_screen_number(ssh->cfg.x11_display),
			PKT_END);
	} else {
	    send_packet(ssh, SSH1_CMSG_X11_REQUEST_FORWARDING,
			PKT_STR, proto, PKT_STR, data, PKT_END);
	}
	do {
	    crReturnV;
	} while (!pktin);
	if (pktin->type != SSH1_SMSG_SUCCESS
	    && pktin->type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crStopV;
	} else if (pktin->type == SSH1_SMSG_FAILURE) {
	    logevent("X11 forwarding refused");
	} else {
	    logevent("X11 forwarding enabled");
	    ssh->X11_fwd_enabled = TRUE;
	    ssh->packet_dispatch[SSH1_SMSG_X11_OPEN] = ssh1_smsg_x11_open;
	}
    }

    ssh_setup_portfwd(ssh, &ssh->cfg);
    ssh->packet_dispatch[SSH1_MSG_PORT_OPEN] = ssh1_msg_port_open;

    if (!ssh->cfg.nopty) {
	/* Unpick the terminal-speed string. */
	/* XXX perhaps we should allow no speeds to be sent. */
	ssh->ospeed = 38400; ssh->ispeed = 38400; /* last-resort defaults */
	sscanf(ssh->cfg.termspeed, "%d,%d", &ssh->ospeed, &ssh->ispeed);
	/* Send the pty request. */
	send_packet(ssh, SSH1_CMSG_REQUEST_PTY,
		    PKT_STR, ssh->cfg.termtype,
		    PKT_INT, ssh->term_height,
		    PKT_INT, ssh->term_width,
		    PKT_INT, 0, PKT_INT, 0, /* width,height in pixels */
		    PKT_CHAR, 192, PKT_INT, ssh->ispeed, /* TTY_OP_ISPEED */
		    PKT_CHAR, 193, PKT_INT, ssh->ospeed, /* TTY_OP_OSPEED */
		    PKT_CHAR, 0, PKT_END);
	ssh->state = SSH_STATE_INTERMED;
	do {
	    crReturnV;
	} while (!pktin);
	if (pktin->type != SSH1_SMSG_SUCCESS
	    && pktin->type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crStopV;
	} else if (pktin->type == SSH1_SMSG_FAILURE) {
	    c_write_str(ssh, "Server refused to allocate pty\r\n");
	    ssh->editing = ssh->echoing = 1;
	}
	logeventf(ssh, "Allocated pty (ospeed %dbps, ispeed %dbps)",
		  ssh->ospeed, ssh->ispeed);
    } else {
	ssh->editing = ssh->echoing = 1;
    }

    if (ssh->cfg.compression) {
	send_packet(ssh, SSH1_CMSG_REQUEST_COMPRESSION, PKT_INT, 6, PKT_END);
	do {
	    crReturnV;
	} while (!pktin);
	if (pktin->type != SSH1_SMSG_SUCCESS
	    && pktin->type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crStopV;
	} else if (pktin->type == SSH1_SMSG_FAILURE) {
	    c_write_str(ssh, "Server refused to compress\r\n");
	}
	logevent("Started compression");
	ssh->v1_compressing = TRUE;
	ssh->cs_comp_ctx = zlib_compress_init();
	logevent("Initialised zlib (RFC1950) compression");
	ssh->sc_comp_ctx = zlib_decompress_init();
	logevent("Initialised zlib (RFC1950) decompression");
    }

    /*
     * Start the shell or command.
     * 
     * Special case: if the first-choice command is an SSH-2
     * subsystem (hence not usable here) and the second choice
     * exists, we fall straight back to that.
     */
    {
	char *cmd = ssh->cfg.remote_cmd_ptr;

	if (!cmd) cmd = ssh->cfg.remote_cmd;
	
	if (ssh->cfg.ssh_subsys && ssh->cfg.remote_cmd_ptr2) {
	    cmd = ssh->cfg.remote_cmd_ptr2;
	    ssh->fallback_cmd = TRUE;
	}
	if (*cmd)
	    send_packet(ssh, SSH1_CMSG_EXEC_CMD, PKT_STR, cmd, PKT_END);
	else
	    send_packet(ssh, SSH1_CMSG_EXEC_SHELL, PKT_END);
	logevent("Started session");
    }

    ssh->state = SSH_STATE_SESSION;
    if (ssh->size_needed)
	ssh_size(ssh, ssh->term_width, ssh->term_height);
    if (ssh->eof_needed)
	ssh_special(ssh, TS_EOF);

    if (ssh->ldisc)
	ldisc_send(ssh->ldisc, NULL, 0, 0);/* cause ldisc to notice changes */
    ssh->send_ok = 1;
    ssh->channels = newtree234(ssh_channelcmp);
    while (1) {

	/*
	 * By this point, most incoming packets are already being
	 * handled by the dispatch table, and we need only pay
	 * attention to the unusual ones.
	 */

	crReturnV;
	if (pktin) {
	    if (pktin->type == SSH1_SMSG_SUCCESS) {
		/* may be from EXEC_SHELL on some servers */
	    } else if (pktin->type == SSH1_SMSG_FAILURE) {
		/* may be from EXEC_SHELL on some servers
		 * if no pty is available or in other odd cases. Ignore */
	    } else {
		bombout(("Strange packet received: type %d", pktin->type));
		crStopV;
	    }
	} else {
	    while (inlen > 0) {
		int len = min(inlen, 512);
		send_packet(ssh, SSH1_CMSG_STDIN_DATA, PKTT_DATA,
			    PKT_INT, len, PKT_DATA, in, len,
			    PKTT_OTHER, PKT_END);
		in += len;
		inlen -= len;
	    }
	}
    }

    crFinishV;
}

/*
 * Handle the top-level SSH-2 protocol.
 */
static void ssh1_msg_debug(Ssh ssh, struct Packet *pktin)
{
    char *msg;
    int msglen;

    ssh_pkt_getstring(pktin, &msg, &msglen);
    logeventf(ssh, "Remote debug message: %.*s", msglen, msg);
}

static void ssh1_msg_disconnect(Ssh ssh, struct Packet *pktin)
{
    /* log reason code in disconnect message */
    char *msg;
    int msglen;

    ssh_pkt_getstring(pktin, &msg, &msglen);
    bombout(("Server sent disconnect message:\n\"%.*s\"", msglen, msg));
}

static void ssh_msg_ignore(Ssh ssh, struct Packet *pktin)
{
    /* Do nothing, because we're ignoring it! Duhh. */
}

static void ssh1_protocol_setup(Ssh ssh)
{
    int i;

    /*
     * Most messages are handled by the coroutines.
     */
    for (i = 0; i < 256; i++)
	ssh->packet_dispatch[i] = NULL;

    /*
     * These special message types we install handlers for.
     */
    ssh->packet_dispatch[SSH1_MSG_DISCONNECT] = ssh1_msg_disconnect;
    ssh->packet_dispatch[SSH1_MSG_IGNORE] = ssh_msg_ignore;
    ssh->packet_dispatch[SSH1_MSG_DEBUG] = ssh1_msg_debug;
}

static void ssh1_protocol(Ssh ssh, void *vin, int inlen,
			  struct Packet *pktin)
{
    unsigned char *in=(unsigned char*)vin;
    if (ssh->state == SSH_STATE_CLOSED)
	return;

    if (pktin && ssh->packet_dispatch[pktin->type]) {
	ssh->packet_dispatch[pktin->type](ssh, pktin);
	return;
    }

    if (!ssh->protocol_initial_phase_done) {
	if (do_ssh1_login(ssh, in, inlen, pktin))
	    ssh->protocol_initial_phase_done = TRUE;
	else
	    return;
    }

    do_ssh1_connection(ssh, in, inlen, pktin);
}

/*
 * Utility routine for decoding comma-separated strings in KEXINIT.
 */
static int in_commasep_string(char *needle, char *haystack, int haylen)
{
    int needlen;
    if (!needle || !haystack)	       /* protect against null pointers */
	return 0;
    needlen = strlen(needle);
    while (1) {
	/*
	 * Is it at the start of the string?
	 */
	if (haylen >= needlen &&       /* haystack is long enough */
	    !memcmp(needle, haystack, needlen) &&	/* initial match */
	    (haylen == needlen || haystack[needlen] == ',')
	    /* either , or EOS follows */
	    )
	    return 1;
	/*
	 * If not, search for the next comma and resume after that.
	 * If no comma found, terminate.
	 */
	while (haylen > 0 && *haystack != ',')
	    haylen--, haystack++;
	if (haylen == 0)
	    return 0;
	haylen--, haystack++;	       /* skip over comma itself */
    }
}

/*
 * Similar routine for checking whether we have the first string in a list.
 */
static int first_in_commasep_string(char *needle, char *haystack, int haylen)
{
    int needlen;
    if (!needle || !haystack)	       /* protect against null pointers */
	return 0;
    needlen = strlen(needle);
    /*
     * Is it at the start of the string?
     */
    if (haylen >= needlen &&       /* haystack is long enough */
	!memcmp(needle, haystack, needlen) &&	/* initial match */
	(haylen == needlen || haystack[needlen] == ',')
	/* either , or EOS follows */
	)
	return 1;
    return 0;
}


/*
 * SSH-2 key creation method.
 */
static void ssh2_mkkey(Ssh ssh, Bignum K, unsigned char *H,
		       unsigned char *sessid, char chr,
		       unsigned char *keyspace)
{
    SHA_State s;
    /* First 20 bytes. */
    SHA_Init(&s);
    if (!(ssh->remote_bugs & BUG_SSH2_DERIVEKEY))
	sha_mpint(&s, K);
    SHA_Bytes(&s, H, 20);
    SHA_Bytes(&s, &chr, 1);
    SHA_Bytes(&s, sessid, 20);
    SHA_Final(&s, keyspace);
    /* Next 20 bytes. */
    SHA_Init(&s);
    if (!(ssh->remote_bugs & BUG_SSH2_DERIVEKEY))
	sha_mpint(&s, K);
    SHA_Bytes(&s, H, 20);
    SHA_Bytes(&s, keyspace, 20);
    SHA_Final(&s, keyspace + 20);
}

/*
 * Handle the SSH-2 transport layer.
 */
static int do_ssh2_transport(Ssh ssh, void *vin, int inlen,
			     struct Packet *pktin)
{
    unsigned char *in = (unsigned char *)vin;
    struct do_ssh2_transport_state {
	int nbits, pbits, warn_kex, warn_cscipher, warn_sccipher;
	Bignum p, g, e, f, K;
	int kex_init_value, kex_reply_value;
	const struct ssh_mac **maclist;
	int nmacs;
	const struct ssh2_cipher *cscipher_tobe;
	const struct ssh2_cipher *sccipher_tobe;
	const struct ssh_mac *csmac_tobe;
	const struct ssh_mac *scmac_tobe;
	const struct ssh_compress *cscomp_tobe;
	const struct ssh_compress *sccomp_tobe;
	char *hostkeydata, *sigdata, *keystr, *fingerprint;
	int hostkeylen, siglen;
	void *hkey;		       /* actual host key */
	unsigned char exchange_hash[20];
	int n_preferred_kex;
	const struct ssh_kex *preferred_kex[KEX_MAX];
	int n_preferred_ciphers;
	const struct ssh2_ciphers *preferred_ciphers[CIPHER_MAX];
	const struct ssh_compress *preferred_comp;
	int got_session_id, activated_authconn;
	struct Packet *pktout;
        int dlgret;
	int guessok;
	int ignorepkt;
    };
    crState(do_ssh2_transport_state);

    crBegin(ssh->do_ssh2_transport_crstate);

    s->cscipher_tobe = s->sccipher_tobe = NULL;
    s->csmac_tobe = s->scmac_tobe = NULL;
    s->cscomp_tobe = s->sccomp_tobe = NULL;

    s->got_session_id = s->activated_authconn = FALSE;

    /*
     * Be prepared to work around the buggy MAC problem.
     */
    if (ssh->remote_bugs & BUG_SSH2_HMAC)
	s->maclist = buggymacs, s->nmacs = lenof(buggymacs);
    else
	s->maclist = macs, s->nmacs = lenof(macs);

  begin_key_exchange:
    ssh->pkt_ctx &= ~SSH2_PKTCTX_KEX_MASK;
    {
	int i, j, commalist_started;

	/*
	 * Set up the preferred key exchange. (NULL => warn below here)
	 */
	s->n_preferred_kex = 0;
	for (i = 0; i < KEX_MAX; i++) {
	    switch (ssh->cfg.ssh_kexlist[i]) {
	      case KEX_DHGEX:
		s->preferred_kex[s->n_preferred_kex++] =
		    &ssh_diffiehellman_gex;
		break;
	      case KEX_DHGROUP14:
		s->preferred_kex[s->n_preferred_kex++] =
		    &ssh_diffiehellman_group14;
		break;
	      case KEX_DHGROUP1:
		s->preferred_kex[s->n_preferred_kex++] =
		    &ssh_diffiehellman_group1;
		break;
	      case CIPHER_WARN:
		/* Flag for later. Don't bother if it's the last in
		 * the list. */
		if (i < KEX_MAX - 1) {
		    s->preferred_kex[s->n_preferred_kex++] = NULL;
		}
		break;
	    }
	}

	/*
	 * Set up the preferred ciphers. (NULL => warn below here)
	 */
	s->n_preferred_ciphers = 0;
	for (i = 0; i < CIPHER_MAX; i++) {
	    switch (ssh->cfg.ssh_cipherlist[i]) {
	      case CIPHER_BLOWFISH:
		s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_blowfish;
		break;
	      case CIPHER_DES:
		if (ssh->cfg.ssh2_des_cbc) {
		    s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_des;
		}
		break;
	      case CIPHER_3DES:
		s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_3des;
		break;
	      case CIPHER_AES:
		s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_aes;
		break;
	      case CIPHER_WARN:
		/* Flag for later. Don't bother if it's the last in
		 * the list. */
		if (i < CIPHER_MAX - 1) {
		    s->preferred_ciphers[s->n_preferred_ciphers++] = NULL;
		}
		break;
	    }
	}

	/*
	 * Set up preferred compression.
	 */
	if (ssh->cfg.compression)
	    s->preferred_comp = &ssh_zlib;
	else
	    s->preferred_comp = &ssh_comp_none;

	/*
	 * Enable queueing of outgoing auth- or connection-layer
	 * packets while we are in the middle of a key exchange.
	 */
	ssh->queueing = TRUE;

	/*
	 * Flag that KEX is in progress.
	 */
	ssh->kex_in_progress = TRUE;

	/*
	 * Construct and send our key exchange packet.
	 */
	s->pktout = ssh2_pkt_init(SSH2_MSG_KEXINIT);
	for (i = 0; i < 16; i++)
	    ssh2_pkt_addbyte(s->pktout, (unsigned char) random_byte());
	/* List key exchange algorithms. */
	ssh2_pkt_addstring_start(s->pktout);
	commalist_started = 0;
	for (i = 0; i < s->n_preferred_kex; i++) {
	    const struct ssh_kex *k = s->preferred_kex[i];
	    if (!k) continue;	       /* warning flag */
	    if (commalist_started)
		ssh2_pkt_addstring_str(s->pktout, ",");
	    ssh2_pkt_addstring_str(s->pktout, s->preferred_kex[i]->name);
	    commalist_started = 1;
	}
	/* List server host key algorithms. */
	ssh2_pkt_addstring_start(s->pktout);
	for (i = 0; i < lenof(hostkey_algs); i++) {
	    ssh2_pkt_addstring_str(s->pktout, hostkey_algs[i]->name);
	    if (i < lenof(hostkey_algs) - 1)
		ssh2_pkt_addstring_str(s->pktout, ",");
	}
	/* List client->server encryption algorithms. */
	ssh2_pkt_addstring_start(s->pktout);
	commalist_started = 0;
	for (i = 0; i < s->n_preferred_ciphers; i++) {
	    const struct ssh2_ciphers *c = s->preferred_ciphers[i];
	    if (!c) continue;	       /* warning flag */
	    for (j = 0; j < c->nciphers; j++) {
		if (commalist_started)
		    ssh2_pkt_addstring_str(s->pktout, ",");
		ssh2_pkt_addstring_str(s->pktout, c->list[j]->name);
		commalist_started = 1;
	    }
	}
	/* List server->client encryption algorithms. */
	ssh2_pkt_addstring_start(s->pktout);
	commalist_started = 0;
	for (i = 0; i < s->n_preferred_ciphers; i++) {
	    const struct ssh2_ciphers *c = s->preferred_ciphers[i];
	    if (!c) continue; /* warning flag */
	    for (j = 0; j < c->nciphers; j++) {
		if (commalist_started)
		    ssh2_pkt_addstring_str(s->pktout, ",");
		ssh2_pkt_addstring_str(s->pktout, c->list[j]->name);
		commalist_started = 1;
	    }
	}
	/* List client->server MAC algorithms. */
	ssh2_pkt_addstring_start(s->pktout);
	for (i = 0; i < s->nmacs; i++) {
	    ssh2_pkt_addstring_str(s->pktout, s->maclist[i]->name);
	    if (i < s->nmacs - 1)
		ssh2_pkt_addstring_str(s->pktout, ",");
	}
	/* List server->client MAC algorithms. */
	ssh2_pkt_addstring_start(s->pktout);
	for (i = 0; i < s->nmacs; i++) {
	    ssh2_pkt_addstring_str(s->pktout, s->maclist[i]->name);
	    if (i < s->nmacs - 1)
		ssh2_pkt_addstring_str(s->pktout, ",");
	}
	/* List client->server compression algorithms. */
	ssh2_pkt_addstring_start(s->pktout);
	assert(lenof(compressions) > 1);
	ssh2_pkt_addstring_str(s->pktout, s->preferred_comp->name);
	for (i = 0; i < lenof(compressions); i++) {
	    const struct ssh_compress *c = compressions[i];
	    if (c != s->preferred_comp) {
		ssh2_pkt_addstring_str(s->pktout, ",");
		ssh2_pkt_addstring_str(s->pktout, c->name);
	    }
	}
	/* List server->client compression algorithms. */
	ssh2_pkt_addstring_start(s->pktout);
	assert(lenof(compressions) > 1);
	ssh2_pkt_addstring_str(s->pktout, s->preferred_comp->name);
	for (i = 0; i < lenof(compressions); i++) {
	    const struct ssh_compress *c = compressions[i];
	    if (c != s->preferred_comp) {
		ssh2_pkt_addstring_str(s->pktout, ",");
		ssh2_pkt_addstring_str(s->pktout, c->name);
	    }
	}
	/* List client->server languages. Empty list. */
	ssh2_pkt_addstring_start(s->pktout);
	/* List server->client languages. Empty list. */
	ssh2_pkt_addstring_start(s->pktout);
	/* First KEX packet does _not_ follow, because we're not that brave. */
	ssh2_pkt_addbool(s->pktout, FALSE);
	/* Reserved. */
	ssh2_pkt_adduint32(s->pktout, 0);
    }

    ssh->exhash = ssh->exhashbase;
    sha_string(&ssh->exhash, s->pktout->data + 5, s->pktout->length - 5);

    ssh2_pkt_send_noqueue(ssh, s->pktout);

    if (!pktin)
	crWaitUntil(pktin);
    if (pktin->length > 5)
	sha_string(&ssh->exhash, pktin->data + 5, pktin->length - 5);

    /*
     * Now examine the other side's KEXINIT to see what we're up
     * to.
     */
    {
	char *str, *preferred;
	int i, j, len;

	if (pktin->type != SSH2_MSG_KEXINIT) {
	    bombout(("expected key exchange packet from server"));
	    crStop(0);
	}
	ssh->kex = NULL;
	ssh->hostkey = NULL;
	s->cscipher_tobe = NULL;
	s->sccipher_tobe = NULL;
	s->csmac_tobe = NULL;
	s->scmac_tobe = NULL;
	s->cscomp_tobe = NULL;
	s->sccomp_tobe = NULL;
	s->warn_kex = s->warn_cscipher = s->warn_sccipher = FALSE;

	pktin->savedpos += 16;	        /* skip garbage cookie */
	ssh_pkt_getstring(pktin, &str, &len);    /* key exchange algorithms */

	preferred = NULL;
	for (i = 0; i < s->n_preferred_kex; i++) {
	    const struct ssh_kex *k = s->preferred_kex[i];
	    if (!k) {
		s->warn_kex = TRUE;
	    } else {
		if (!preferred) preferred = k->name;
		if (in_commasep_string(k->name, str, len))
		    ssh->kex = k;
	    }
	    if (ssh->kex)
		break;
	}
	if (!ssh->kex) {
	    bombout(("Couldn't agree a key exchange algorithm (available: %s)",
		     str ? str : "(null)"));
	    crStop(0);
	}
	/*
	 * Note that the server's guess is considered wrong if it doesn't match
	 * the first algorithm in our list, even if it's still the algorithm
	 * we end up using.
	 */
	s->guessok = first_in_commasep_string(preferred, str, len);
	ssh_pkt_getstring(pktin, &str, &len);    /* host key algorithms */
	for (i = 0; i < lenof(hostkey_algs); i++) {
	    if (in_commasep_string(hostkey_algs[i]->name, str, len)) {
		ssh->hostkey = hostkey_algs[i];
		break;
	    }
	}
	s->guessok = s->guessok &&
	    first_in_commasep_string(hostkey_algs[0]->name, str, len);
	ssh_pkt_getstring(pktin, &str, &len);    /* client->server cipher */
	for (i = 0; i < s->n_preferred_ciphers; i++) {
	    const struct ssh2_ciphers *c = s->preferred_ciphers[i];
	    if (!c) {
		s->warn_cscipher = TRUE;
	    } else {
		for (j = 0; j < c->nciphers; j++) {
		    if (in_commasep_string(c->list[j]->name, str, len)) {
			s->cscipher_tobe = c->list[j];
			break;
		    }
		}
	    }
	    if (s->cscipher_tobe)
		break;
	}
	if (!s->cscipher_tobe) {
	    bombout(("Couldn't agree a client-to-server cipher (available: %s)",
		     str ? str : "(null)"));
	    crStop(0);
	}

	ssh_pkt_getstring(pktin, &str, &len);    /* server->client cipher */
	for (i = 0; i < s->n_preferred_ciphers; i++) {
	    const struct ssh2_ciphers *c = s->preferred_ciphers[i];
	    if (!c) {
		s->warn_sccipher = TRUE;
	    } else {
		for (j = 0; j < c->nciphers; j++) {
		    if (in_commasep_string(c->list[j]->name, str, len)) {
			s->sccipher_tobe = c->list[j];
			break;
		    }
		}
	    }
	    if (s->sccipher_tobe)
		break;
	}
	if (!s->sccipher_tobe) {
	    bombout(("Couldn't agree a server-to-client cipher (available: %s)",
		     str ? str : "(null)"));
	    crStop(0);
	}

	ssh_pkt_getstring(pktin, &str, &len);    /* client->server mac */
	for (i = 0; i < s->nmacs; i++) {
	    if (in_commasep_string(s->maclist[i]->name, str, len)) {
		s->csmac_tobe = s->maclist[i];
		break;
	    }
	}
	ssh_pkt_getstring(pktin, &str, &len);    /* server->client mac */
	for (i = 0; i < s->nmacs; i++) {
	    if (in_commasep_string(s->maclist[i]->name, str, len)) {
		s->scmac_tobe = s->maclist[i];
		break;
	    }
	}
	ssh_pkt_getstring(pktin, &str, &len);  /* client->server compression */
	for (i = 0; i < lenof(compressions) + 1; i++) {
	    const struct ssh_compress *c =
		i == 0 ? s->preferred_comp : compressions[i - 1];
	    if (in_commasep_string(c->name, str, len)) {
		s->cscomp_tobe = c;
		break;
	    }
	}
	ssh_pkt_getstring(pktin, &str, &len);  /* server->client compression */
	for (i = 0; i < lenof(compressions) + 1; i++) {
	    const struct ssh_compress *c =
		i == 0 ? s->preferred_comp : compressions[i - 1];
	    if (in_commasep_string(c->name, str, len)) {
		s->sccomp_tobe = c;
		break;
	    }
	}
	ssh_pkt_getstring(pktin, &str, &len);  /* client->server language */
	ssh_pkt_getstring(pktin, &str, &len);  /* server->client language */
	s->ignorepkt = ssh2_pkt_getbool(pktin) && !s->guessok;

	if (s->warn_kex) {
	    ssh_set_frozen(ssh, 1);
	    s->dlgret = askalg(ssh->frontend, "key-exchange algorithm",
			       ssh->kex->name,
			       ssh_dialog_callback, ssh);
	    if (s->dlgret < 0) {
		do {
		    crReturn(0);
		    if (pktin) {
			bombout(("Unexpected data from server while"
				 " waiting for user response"));
			crStop(0);
		    }
		} while (pktin || inlen > 0);
		s->dlgret = ssh->user_response;
	    }
	    ssh_set_frozen(ssh, 0);
	    if (s->dlgret == 0) {
		ssh->close_expected = TRUE;
		ssh_closing((Plug)ssh, NULL, 0, 0);
		crStop(0);
	    }
	}

	if (s->warn_cscipher) {
	    ssh_set_frozen(ssh, 1);
	    s->dlgret = askalg(ssh->frontend,
			       "client-to-server cipher",
			       s->cscipher_tobe->name,
			       ssh_dialog_callback, ssh);
	    if (s->dlgret < 0) {
		do {
		    crReturn(0);
		    if (pktin) {
			bombout(("Unexpected data from server while"
				 " waiting for user response"));
			crStop(0);
		    }
		} while (pktin || inlen > 0);
		s->dlgret = ssh->user_response;
	    }
	    ssh_set_frozen(ssh, 0);
	    if (s->dlgret == 0) {
		ssh->close_expected = TRUE;
		ssh_closing((Plug)ssh, NULL, 0, 0);
		crStop(0);
	    }
	}

	if (s->warn_sccipher) {
	    ssh_set_frozen(ssh, 1);
	    s->dlgret = askalg(ssh->frontend,
			       "server-to-client cipher",
			       s->sccipher_tobe->name,
			       ssh_dialog_callback, ssh);
	    if (s->dlgret < 0) {
		do {
		    crReturn(0);
		    if (pktin) {
			bombout(("Unexpected data from server while"
				 " waiting for user response"));
			crStop(0);
		    }
		} while (pktin || inlen > 0);
		s->dlgret = ssh->user_response;
	    }
	    ssh_set_frozen(ssh, 0);
	    if (s->dlgret == 0) {
		ssh->close_expected = TRUE;
		ssh_closing((Plug)ssh, NULL, 0, 0);
		crStop(0);
	    }
	}

	if (s->ignorepkt) /* first_kex_packet_follows */
	    crWaitUntil(pktin);                /* Ignore packet */
    }

    /*
     * Work out the number of bits of key we will need from the key
     * exchange. We start with the maximum key length of either
     * cipher...
     */
    {
	int csbits, scbits;

	csbits = s->cscipher_tobe->keylen;
	scbits = s->sccipher_tobe->keylen;
	s->nbits = (csbits > scbits ? csbits : scbits);
    }
    /* The keys only have 160-bit entropy, since they're based on
     * a SHA-1 hash. So cap the key size at 160 bits. */
    if (s->nbits > 160)
	s->nbits = 160;

    /*
     * If we're doing Diffie-Hellman group exchange, start by
     * requesting a group.
     */
    if (!ssh->kex->pdata) {
	logevent("Doing Diffie-Hellman group exchange");
	ssh->pkt_ctx |= SSH2_PKTCTX_DHGEX;
	/*
	 * Work out how big a DH group we will need to allow that
	 * much data.
	 */
	s->pbits = 512 << ((s->nbits - 1) / 64);
	s->pktout = ssh2_pkt_init(SSH2_MSG_KEX_DH_GEX_REQUEST);
	ssh2_pkt_adduint32(s->pktout, s->pbits);
	ssh2_pkt_send_noqueue(ssh, s->pktout);

	crWaitUntil(pktin);
	if (pktin->type != SSH2_MSG_KEX_DH_GEX_GROUP) {
	    bombout(("expected key exchange group packet from server"));
	    crStop(0);
	}
	s->p = ssh2_pkt_getmp(pktin);
	s->g = ssh2_pkt_getmp(pktin);
	if (!s->p || !s->g) {
	    bombout(("unable to read mp-ints from incoming group packet"));
	    crStop(0);
	}
	ssh->kex_ctx = dh_setup_gex(s->p, s->g);
	s->kex_init_value = SSH2_MSG_KEX_DH_GEX_INIT;
	s->kex_reply_value = SSH2_MSG_KEX_DH_GEX_REPLY;
    } else {
	ssh->pkt_ctx |= SSH2_PKTCTX_DHGROUP;
	ssh->kex_ctx = dh_setup_group(ssh->kex);
	s->kex_init_value = SSH2_MSG_KEXDH_INIT;
	s->kex_reply_value = SSH2_MSG_KEXDH_REPLY;
	logeventf(ssh, "Using Diffie-Hellman with standard group \"%s\"",
		  ssh->kex->groupname);
    }

    logevent("Doing Diffie-Hellman key exchange");
    /*
     * Now generate and send e for Diffie-Hellman.
     */
    set_busy_status(ssh->frontend, BUSY_CPU); /* this can take a while */
    s->e = dh_create_e(ssh->kex_ctx, s->nbits * 2);
    s->pktout = ssh2_pkt_init(s->kex_init_value);
    ssh2_pkt_addmp(s->pktout, s->e);
    ssh2_pkt_send_noqueue(ssh, s->pktout);

    set_busy_status(ssh->frontend, BUSY_WAITING); /* wait for server */
    crWaitUntil(pktin);
    if (pktin->type != s->kex_reply_value) {
	bombout(("expected key exchange reply packet from server"));
	crStop(0);
    }
    set_busy_status(ssh->frontend, BUSY_CPU); /* cogitate */
    ssh_pkt_getstring(pktin, &s->hostkeydata, &s->hostkeylen);
    s->f = ssh2_pkt_getmp(pktin);
    if (!s->f) {
	bombout(("unable to parse key exchange reply packet"));
	crStop(0);
    }
    ssh_pkt_getstring(pktin, &s->sigdata, &s->siglen);

    s->K = dh_find_K(ssh->kex_ctx, s->f);

    /* We assume everything from now on will be quick, and it might
     * involve user interaction. */
    set_busy_status(ssh->frontend, BUSY_NOT);

    sha_string(&ssh->exhash, s->hostkeydata, s->hostkeylen);
    if (ssh->kex == &ssh_diffiehellman_gex) {
	sha_uint32(&ssh->exhash, s->pbits);
	sha_mpint(&ssh->exhash, s->p);
	sha_mpint(&ssh->exhash, s->g);
    }
    sha_mpint(&ssh->exhash, s->e);
    sha_mpint(&ssh->exhash, s->f);
    sha_mpint(&ssh->exhash, s->K);
    SHA_Final(&ssh->exhash, s->exchange_hash);

    dh_cleanup(ssh->kex_ctx);
    ssh->kex_ctx = NULL;

#if 0
    debug(("Exchange hash is:\n"));
    dmemdump(s->exchange_hash, 20);
#endif

    s->hkey = ssh->hostkey->newkey(s->hostkeydata, s->hostkeylen);
    if (!s->hkey ||
	!ssh->hostkey->verifysig(s->hkey, s->sigdata, s->siglen,
				 (char *)s->exchange_hash, 20)) {
	bombout(("Server's host key did not match the signature supplied"));
	crStop(0);
    }

    /*
     * Authenticate remote host: verify host key. (We've already
     * checked the signature of the exchange hash.)
     */
    s->keystr = ssh->hostkey->fmtkey(s->hkey);
    s->fingerprint = ssh->hostkey->fingerprint(s->hkey);
    ssh_set_frozen(ssh, 1);
    s->dlgret = verify_ssh_host_key(ssh->frontend,
                                    ssh->savedhost, ssh->savedport,
                                    ssh->hostkey->keytype, s->keystr,
				    s->fingerprint,
                                    ssh_dialog_callback, ssh);
    if (s->dlgret < 0) {
        do {
            crReturn(0);
            if (pktin) {
                bombout(("Unexpected data from server while waiting"
                         " for user host key response"));
                    crStop(0);
            }
        } while (pktin || inlen > 0);
        s->dlgret = ssh->user_response;
    }
    ssh_set_frozen(ssh, 0);
    if (s->dlgret == 0) {
        ssh->close_expected = TRUE;
        ssh_closing((Plug)ssh, NULL, 0, 0);
        crStop(0);
    }
    if (!s->got_session_id) {     /* don't bother logging this in rekeys */
	logevent("Host key fingerprint is:");
	logevent(s->fingerprint);
    }
    sfree(s->fingerprint);
    sfree(s->keystr);
    ssh->hostkey->freekey(s->hkey);

    /*
     * The exchange hash from the very first key exchange is also
     * the session id, used in session key construction and
     * authentication.
     */
    if (!s->got_session_id) {
	memcpy(ssh->v2_session_id, s->exchange_hash,
	       sizeof(s->exchange_hash));
	s->got_session_id = TRUE;
    }

    /*
     * Send SSH2_MSG_NEWKEYS.
     */
    s->pktout = ssh2_pkt_init(SSH2_MSG_NEWKEYS);
    ssh2_pkt_send_noqueue(ssh, s->pktout);
    ssh->outgoing_data_size = 0;       /* start counting from here */

    /*
     * We've sent client NEWKEYS, so create and initialise
     * client-to-server session keys.
     */
    if (ssh->cs_cipher_ctx)
	ssh->cscipher->free_context(ssh->cs_cipher_ctx);
    ssh->cscipher = s->cscipher_tobe;
    ssh->cs_cipher_ctx = ssh->cscipher->make_context();

    if (ssh->cs_mac_ctx)
	ssh->csmac->free_context(ssh->cs_mac_ctx);
    ssh->csmac = s->csmac_tobe;
    ssh->cs_mac_ctx = ssh->csmac->make_context();

    if (ssh->cs_comp_ctx)
	ssh->cscomp->compress_cleanup(ssh->cs_comp_ctx);
    ssh->cscomp = s->cscomp_tobe;
    ssh->cs_comp_ctx = ssh->cscomp->compress_init();

    /*
     * Set IVs on client-to-server keys. Here we use the exchange
     * hash from the _first_ key exchange.
     */
    {
	unsigned char keyspace[40];
	ssh2_mkkey(ssh,s->K,s->exchange_hash,ssh->v2_session_id,'C',keyspace);
	ssh->cscipher->setkey(ssh->cs_cipher_ctx, keyspace);
	ssh2_mkkey(ssh,s->K,s->exchange_hash,ssh->v2_session_id,'A',keyspace);
	ssh->cscipher->setiv(ssh->cs_cipher_ctx, keyspace);
	ssh2_mkkey(ssh,s->K,s->exchange_hash,ssh->v2_session_id,'E',keyspace);
	ssh->csmac->setkey(ssh->cs_mac_ctx, keyspace);
    }

    logeventf(ssh, "Initialised %.200s client->server encryption",
	      ssh->cscipher->text_name);
    logeventf(ssh, "Initialised %.200s client->server MAC algorithm",
	      ssh->csmac->text_name);
    if (ssh->cscomp->text_name)
	logeventf(ssh, "Initialised %s compression",
		  ssh->cscomp->text_name);

    /*
     * Now our end of the key exchange is complete, we can send all
     * our queued higher-layer packets.
     */
    ssh->queueing = FALSE;
    ssh2_pkt_queuesend(ssh);

    /*
     * Expect SSH2_MSG_NEWKEYS from server.
     */
    crWaitUntil(pktin);
    if (pktin->type != SSH2_MSG_NEWKEYS) {
	bombout(("expected new-keys packet from server"));
	crStop(0);
    }
    ssh->incoming_data_size = 0;       /* start counting from here */

    /*
     * We've seen server NEWKEYS, so create and initialise
     * server-to-client session keys.
     */
    if (ssh->sc_cipher_ctx)
	ssh->sccipher->free_context(ssh->sc_cipher_ctx);
    ssh->sccipher = s->sccipher_tobe;
    ssh->sc_cipher_ctx = ssh->sccipher->make_context();

    if (ssh->sc_mac_ctx)
	ssh->scmac->free_context(ssh->sc_mac_ctx);
    ssh->scmac = s->scmac_tobe;
    ssh->sc_mac_ctx = ssh->scmac->make_context();

    if (ssh->sc_comp_ctx)
	ssh->sccomp->decompress_cleanup(ssh->sc_comp_ctx);
    ssh->sccomp = s->sccomp_tobe;
    ssh->sc_comp_ctx = ssh->sccomp->decompress_init();

    /*
     * Set IVs on server-to-client keys. Here we use the exchange
     * hash from the _first_ key exchange.
     */
    {
	unsigned char keyspace[40];
	ssh2_mkkey(ssh,s->K,s->exchange_hash,ssh->v2_session_id,'D',keyspace);
	ssh->sccipher->setkey(ssh->sc_cipher_ctx, keyspace);
	ssh2_mkkey(ssh,s->K,s->exchange_hash,ssh->v2_session_id,'B',keyspace);
	ssh->sccipher->setiv(ssh->sc_cipher_ctx, keyspace);
	ssh2_mkkey(ssh,s->K,s->exchange_hash,ssh->v2_session_id,'F',keyspace);
	ssh->scmac->setkey(ssh->sc_mac_ctx, keyspace);
    }
    logeventf(ssh, "Initialised %.200s server->client encryption",
	      ssh->sccipher->text_name);
    logeventf(ssh, "Initialised %.200s server->client MAC algorithm",
	      ssh->scmac->text_name);
    if (ssh->sccomp->text_name)
	logeventf(ssh, "Initialised %s decompression",
		  ssh->sccomp->text_name);

    /*
     * Free key exchange data.
     */
    freebn(s->f);
    freebn(s->K);
    if (ssh->kex == &ssh_diffiehellman_gex) {
	freebn(s->g);
	freebn(s->p);
    }

    /*
     * Key exchange is over. Loop straight back round if we have a
     * deferred rekey reason.
     */
    if (ssh->deferred_rekey_reason) {
	logevent(ssh->deferred_rekey_reason);
	pktin = NULL;
	ssh->deferred_rekey_reason = NULL;
	goto begin_key_exchange;
    }

    /*
     * Otherwise, schedule a timer for our next rekey.
     */
    ssh->kex_in_progress = FALSE;
    ssh->last_rekey = GETTICKCOUNT();
    if (ssh->cfg.ssh_rekey_time != 0)
	ssh->next_rekey = schedule_timer(ssh->cfg.ssh_rekey_time*60*TICKSPERSEC,
					 ssh2_timer, ssh);

    /*
     * If this is the first key exchange phase, we must pass the
     * SSH2_MSG_NEWKEYS packet to the next layer, not because it
     * wants to see it but because it will need time to initialise
     * itself before it sees an actual packet. In subsequent key
     * exchange phases, we don't pass SSH2_MSG_NEWKEYS on, because
     * it would only confuse the layer above.
     */
    if (s->activated_authconn) {
	crReturn(1);
    }
    s->activated_authconn = TRUE;

    /*
     * Now we're encrypting. Begin returning 1 to the protocol main
     * function so that other things can run on top of the
     * transport. If we ever see a KEXINIT, we must go back to the
     * start.
     * 
     * We _also_ go back to the start if we see pktin==NULL and
     * inlen==-1, because this is a special signal meaning
     * `initiate client-driven rekey', and `in' contains a message
     * giving the reason for the rekey.
     */
    while (!((pktin && pktin->type == SSH2_MSG_KEXINIT) ||
	     (!pktin && inlen == -1))) {
        wait_for_rekey:
	crReturn(1);
    }
    if (pktin) {
	logevent("Server initiated key re-exchange");
    } else {
        /*
         * Special case: if the server bug is set that doesn't
         * allow rekeying, we give a different log message and
         * continue waiting. (If such a server _initiates_ a rekey,
         * we process it anyway!)
         */
        if ((ssh->remote_bugs & BUG_SSH2_REKEY)) {
            logeventf(ssh, "Server bug prevents key re-exchange (%s)",
                      (char *)in);
            /* Reset the counters, so that at least this message doesn't
             * hit the event log _too_ often. */
            ssh->outgoing_data_size = 0;
            ssh->incoming_data_size = 0;
            if (ssh->cfg.ssh_rekey_time != 0) {
                ssh->next_rekey =
                    schedule_timer(ssh->cfg.ssh_rekey_time*60*TICKSPERSEC,
                                   ssh2_timer, ssh);
            }
            goto wait_for_rekey;       /* this is utterly horrid */
        } else {
            logeventf(ssh, "Initiating key re-exchange (%s)", (char *)in);
        }
    }
    goto begin_key_exchange;

    crFinish(1);
}

/*
 * Add data to an SSH-2 channel output buffer.
 */
static void ssh2_add_channel_data(struct ssh_channel *c, char *buf,
				  int len)
{
    bufchain_add(&c->v.v2.outbuffer, buf, len);
}

/*
 * Attempt to send data on an SSH-2 channel.
 */
static int ssh2_try_send(struct ssh_channel *c)
{
    Ssh ssh = c->ssh;
    struct Packet *pktout;

    while (c->v.v2.remwindow > 0 && bufchain_size(&c->v.v2.outbuffer) > 0) {
	int len;
	void *data;
	bufchain_prefix(&c->v.v2.outbuffer, &data, &len);
	if ((unsigned)len > c->v.v2.remwindow)
	    len = c->v.v2.remwindow;
	if ((unsigned)len > c->v.v2.remmaxpkt)
	    len = c->v.v2.remmaxpkt;
	pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_DATA);
	ssh2_pkt_adduint32(pktout, c->remoteid);
	dont_log_data(ssh, pktout, PKTLOG_OMIT);
	ssh2_pkt_addstring_start(pktout);
	ssh2_pkt_addstring_data(pktout, data, len);
	end_log_omission(ssh, pktout);
	ssh2_pkt_send(ssh, pktout);
	bufchain_consume(&c->v.v2.outbuffer, len);
	c->v.v2.remwindow -= len;
    }

    /*
     * After having sent as much data as we can, return the amount
     * still buffered.
     */
    return bufchain_size(&c->v.v2.outbuffer);
}

static void ssh2_try_send_and_unthrottle(struct ssh_channel *c)
{
    int bufsize;
    if (c->closes)
	return;			       /* don't send on closing channels */
    bufsize = ssh2_try_send(c);
    if (bufsize == 0) {
	switch (c->type) {
	  case CHAN_MAINSESSION:
	    /* stdin need not receive an unthrottle
	     * notification since it will be polled */
	    break;
	  case CHAN_X11:
	    x11_unthrottle(c->u.x11.s);
	    break;
	  case CHAN_AGENT:
	    /* agent sockets are request/response and need no
	     * buffer management */
	    break;
	  case CHAN_SOCKDATA:
	    pfd_unthrottle(c->u.pfd.s);
	    break;
	}
    }
}

/*
 * Potentially enlarge the window on an SSH-2 channel.
 */
static void ssh2_set_window(struct ssh_channel *c, unsigned newwin)
{
    Ssh ssh = c->ssh;

    /*
     * Never send WINDOW_ADJUST for a channel that the remote side
     * already thinks it's closed; there's no point, since it won't
     * be sending any more data anyway.
     */
    if (c->closes != 0)
	return;

    /*
     * Only send a WINDOW_ADJUST if there's significantly more window
     * available than the other end thinks there is.  This saves us
     * sending a WINDOW_ADJUST for every character in a shell session.
     *
     * "Significant" is arbitrarily defined as half the window size.
     */
    if (newwin > c->v.v2.locwindow * 2) {
	struct Packet *pktout;

	pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_WINDOW_ADJUST);
	ssh2_pkt_adduint32(pktout, c->remoteid);
	ssh2_pkt_adduint32(pktout, newwin - c->v.v2.locwindow);
	ssh2_pkt_send(ssh, pktout);
	c->v.v2.locwindow = newwin;
    }
}

static void ssh2_msg_channel_window_adjust(Ssh ssh, struct Packet *pktin)
{
    unsigned i = ssh_pkt_getuint32(pktin);
    struct ssh_channel *c;
    c = find234(ssh->channels, &i, ssh_channelfind);
    if (c && !c->closes) {
	c->v.v2.remwindow += ssh_pkt_getuint32(pktin);
	ssh2_try_send_and_unthrottle(c);
    }
}

static void ssh2_msg_channel_data(Ssh ssh, struct Packet *pktin)
{
    char *data;
    int length;
    unsigned i = ssh_pkt_getuint32(pktin);
    struct ssh_channel *c;
    c = find234(ssh->channels, &i, ssh_channelfind);
    if (!c)
	return;			       /* nonexistent channel */
    if (pktin->type == SSH2_MSG_CHANNEL_EXTENDED_DATA &&
	ssh_pkt_getuint32(pktin) != SSH2_EXTENDED_DATA_STDERR)
	return;			       /* extended but not stderr */
    ssh_pkt_getstring(pktin, &data, &length);
    if (data) {
	int bufsize = 0;
	c->v.v2.locwindow -= length;
	switch (c->type) {
	  case CHAN_MAINSESSION:
	    bufsize =
		from_backend(ssh->frontend, pktin->type ==
			     SSH2_MSG_CHANNEL_EXTENDED_DATA,
			     data, length
#ifdef MPEXT
			     , 1
#endif
           );
	    break;
	  case CHAN_X11:
	    bufsize = x11_send(c->u.x11.s, data, length);
	    break;
	  case CHAN_SOCKDATA:
	    bufsize = pfd_send(c->u.pfd.s, data, length);
	    break;
	  case CHAN_AGENT:
	    while (length > 0) {
		if (c->u.a.lensofar < 4) {
		    unsigned int l = min(4 - c->u.a.lensofar, length);
		    memcpy(c->u.a.msglen + c->u.a.lensofar,
			   data, l);
		    data += l;
		    length -= l;
		    c->u.a.lensofar += l;
		}
		if (c->u.a.lensofar == 4) {
		    c->u.a.totallen =
			4 + GET_32BIT(c->u.a.msglen);
		    c->u.a.message = snewn(c->u.a.totallen,
					   unsigned char);
		    memcpy(c->u.a.message, c->u.a.msglen, 4);
		}
		if (c->u.a.lensofar >= 4 && length > 0) {
		    unsigned int l =
			min(c->u.a.totallen - c->u.a.lensofar,
			    length);
		    memcpy(c->u.a.message + c->u.a.lensofar,
			   data, l);
		    data += l;
		    length -= l;
		    c->u.a.lensofar += l;
		}
		if (c->u.a.lensofar == c->u.a.totallen) {
		    void *reply;
		    int replylen;
		    if (agent_query(c->u.a.message,
				    c->u.a.totallen,
				    &reply, &replylen,
				    ssh_agentf_callback, c))
			ssh_agentf_callback(c, reply, replylen);
		    sfree(c->u.a.message);
		    c->u.a.lensofar = 0;
		}
	    }
	    bufsize = 0;
	    break;
	}
	/*
	 * If we are not buffering too much data,
	 * enlarge the window again at the remote side.
	 */
	if (bufsize < OUR_V2_WINSIZE)
	    ssh2_set_window(c, OUR_V2_WINSIZE - bufsize);
    }
}

static void ssh2_msg_channel_eof(Ssh ssh, struct Packet *pktin)
{
    unsigned i = ssh_pkt_getuint32(pktin);
    struct ssh_channel *c;

    c = find234(ssh->channels, &i, ssh_channelfind);
    if (!c)
	return;			       /* nonexistent channel */

    if (c->type == CHAN_X11) {
	/*
	 * Remote EOF on an X11 channel means we should
	 * wrap up and close the channel ourselves.
	 */
	x11_close(c->u.x11.s);
	sshfwd_close(c);
    } else if (c->type == CHAN_AGENT) {
	sshfwd_close(c);
    } else if (c->type == CHAN_SOCKDATA) {
	pfd_close(c->u.pfd.s);
	sshfwd_close(c);
    }
}

static void ssh2_msg_channel_close(Ssh ssh, struct Packet *pktin)
{
    unsigned i = ssh_pkt_getuint32(pktin);
    struct ssh_channel *c;
    struct Packet *pktout;

    c = find234(ssh->channels, &i, ssh_channelfind);
    if (!c || c->halfopen) {
	bombout(("Received CHANNEL_CLOSE for %s channel %d\n",
		 c ? "half-open" : "nonexistent", i));
	return;
    }
    /* Do pre-close processing on the channel. */
    switch (c->type) {
      case CHAN_MAINSESSION:
	ssh->mainchan = NULL;
	update_specials_menu(ssh->frontend);
	break;
      case CHAN_X11:
	if (c->u.x11.s != NULL)
	    x11_close(c->u.x11.s);
	sshfwd_close(c);
	break;
      case CHAN_AGENT:
	sshfwd_close(c);
	break;
      case CHAN_SOCKDATA:
	if (c->u.pfd.s != NULL)
	    pfd_close(c->u.pfd.s);
	sshfwd_close(c);
	break;
    }
    if (c->closes == 0) {
	pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_CLOSE);
	ssh2_pkt_adduint32(pktout, c->remoteid);
	ssh2_pkt_send(ssh, pktout);
    }
    del234(ssh->channels, c);
    bufchain_clear(&c->v.v2.outbuffer);
    sfree(c);

    /*
     * See if that was the last channel left open.
     * (This is only our termination condition if we're
     * not running in -N mode.)
     */
    if (!ssh->cfg.ssh_no_shell && count234(ssh->channels) == 0) {
	logevent("All channels closed. Disconnecting");
#if 0
	/*
	 * We used to send SSH_MSG_DISCONNECT here,
	 * because I'd believed that _every_ conforming
	 * SSH-2 connection had to end with a disconnect
	 * being sent by at least one side; apparently
	 * I was wrong and it's perfectly OK to
	 * unceremoniously slam the connection shut
	 * when you're done, and indeed OpenSSH feels
	 * this is more polite than sending a
	 * DISCONNECT. So now we don't.
	 */
	s->pktout = ssh2_pkt_init(SSH2_MSG_DISCONNECT);
	ssh2_pkt_adduint32(s->pktout, SSH2_DISCONNECT_BY_APPLICATION);
	ssh2_pkt_addstring(s->pktout, "All open channels closed");
	ssh2_pkt_addstring(s->pktout, "en");	/* language tag */
	ssh2_pkt_send_noqueue(ssh, s->pktout);
#endif
	ssh->close_expected = TRUE;
	ssh_closing((Plug)ssh, NULL, 0, 0);
    }
}

static void ssh2_msg_channel_open_confirmation(Ssh ssh, struct Packet *pktin)
{
    unsigned i = ssh_pkt_getuint32(pktin);
    struct ssh_channel *c;
    struct Packet *pktout;

    c = find234(ssh->channels, &i, ssh_channelfind);
    if (!c)
	return;			       /* nonexistent channel */
    if (c->type != CHAN_SOCKDATA_DORMANT)
	return;			       /* dunno why they're confirming this */
    c->remoteid = ssh_pkt_getuint32(pktin);
    c->halfopen = FALSE;
    c->type = CHAN_SOCKDATA;
    c->v.v2.remwindow = ssh_pkt_getuint32(pktin);
    c->v.v2.remmaxpkt = ssh_pkt_getuint32(pktin);
    if (c->u.pfd.s)
	pfd_confirm(c->u.pfd.s);
    if (c->closes) {
	/*
	 * We have a pending close on this channel,
	 * which we decided on before the server acked
	 * the channel open. So now we know the
	 * remoteid, we can close it again.
	 */
	pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_CLOSE);
	ssh2_pkt_adduint32(pktout, c->remoteid);
	ssh2_pkt_send(ssh, pktout);
    }
}

static void ssh2_msg_channel_open_failure(Ssh ssh, struct Packet *pktin)
{
    static const char *const reasons[] = {
	"<unknown reason code>",
	    "Administratively prohibited",
	    "Connect failed",
	    "Unknown channel type",
	    "Resource shortage",
    };
    unsigned i = ssh_pkt_getuint32(pktin);
    unsigned reason_code;
    char *reason_string;
    int reason_length;
    struct ssh_channel *c;
    c = find234(ssh->channels, &i, ssh_channelfind);
    if (!c)
	return;			       /* nonexistent channel */
    if (c->type != CHAN_SOCKDATA_DORMANT)
	return;			       /* dunno why they're failing this */

    reason_code = ssh_pkt_getuint32(pktin);
    if (reason_code >= lenof(reasons))
	reason_code = 0; /* ensure reasons[reason_code] in range */
    ssh_pkt_getstring(pktin, &reason_string, &reason_length);
    logeventf(ssh, "Forwarded connection refused by server: %s [%.*s]",
	      reasons[reason_code], reason_length, reason_string);

    pfd_close(c->u.pfd.s);

    del234(ssh->channels, c);
    sfree(c);
}

static void ssh2_msg_channel_request(Ssh ssh, struct Packet *pktin)
{
    unsigned localid;
    char *type;
    int typelen, want_reply;
    int reply = SSH2_MSG_CHANNEL_FAILURE; /* default */
    struct ssh_channel *c;
    struct Packet *pktout;

    localid = ssh_pkt_getuint32(pktin);
    ssh_pkt_getstring(pktin, &type, &typelen);
    want_reply = ssh2_pkt_getbool(pktin);

    /*
     * First, check that the channel exists. Otherwise,
     * we can instantly disconnect with a rude message.
     */
    c = find234(ssh->channels, &localid, ssh_channelfind);
    if (!c) {
	char buf[80];
	sprintf(buf, "Received channel request for nonexistent"
		" channel %d", localid);
	logevent(buf);
	pktout = ssh2_pkt_init(SSH2_MSG_DISCONNECT);
	ssh2_pkt_adduint32(pktout, SSH2_DISCONNECT_BY_APPLICATION);
	ssh2_pkt_addstring(pktout, buf);
	ssh2_pkt_addstring(pktout, "en");	/* language tag */
	ssh2_pkt_send_noqueue(ssh, pktout);
	connection_fatal(ssh->frontend, "%s", buf);
	ssh->close_expected = TRUE;
	ssh_closing((Plug)ssh, NULL, 0, 0);
	return;
    }

    /*
     * Having got the channel number, we now look at
     * the request type string to see if it's something
     * we recognise.
     */
    if (c == ssh->mainchan) {
	/*
	 * We recognise "exit-status" and "exit-signal" on
	 * the primary channel.
	 */
	if (typelen == 11 &&
	    !memcmp(type, "exit-status", 11)) {

	    ssh->exitcode = ssh_pkt_getuint32(pktin);
	    logeventf(ssh, "Server sent command exit status %d",
		      ssh->exitcode);
	    reply = SSH2_MSG_CHANNEL_SUCCESS;

	} else if (typelen == 11 &&
		   !memcmp(type, "exit-signal", 11)) {

	    int is_plausible = TRUE, is_int = FALSE;
	    char *fmt_sig = "", *fmt_msg = "";
	    char *msg;
	    int msglen = 0, core = FALSE;
	    /* ICK: older versions of OpenSSH (e.g. 3.4p1)
	     * provide an `int' for the signal, despite its
	     * having been a `string' in the drafts since at
	     * least 2001. (Fixed in session.c 1.147.) Try to
	     * infer which we can safely parse it as. */
	    {
		unsigned char *p = pktin->body +
		    pktin->savedpos;
		long len = pktin->length - pktin->savedpos;
		unsigned long num = GET_32BIT(p); /* what is it? */
		/* If it's 0, it hardly matters; assume string */
		if (num == 0) {
		    is_int = FALSE;
		} else {
		    int maybe_int = FALSE, maybe_str = FALSE;
#define CHECK_HYPOTHESIS(offset, result) \
    do { \
	long q = offset; \
	if (q >= 0 && q+4 <= len) { \
	    q = q + 4 + GET_32BIT(p+q); \
	    if (q >= 0 && q+4 <= len && \
		    ((q = q + 4 + GET_32BIT(p+q))!= 0) && q == len) \
		result = TRUE; \
	} \
    } while(0)
		    CHECK_HYPOTHESIS(4+1, maybe_int);
		    CHECK_HYPOTHESIS(4+num+1, maybe_str);
#undef CHECK_HYPOTHESIS
		    if (maybe_int && !maybe_str)
			is_int = TRUE;
		    else if (!maybe_int && maybe_str)
			is_int = FALSE;
		    else
			/* Crikey. Either or neither. Panic. */
			is_plausible = FALSE;
		}
	    }
	    if (is_plausible) {
		if (is_int) {
		    /* Old non-standard OpenSSH. */
		    int signum = ssh_pkt_getuint32(pktin);
		    fmt_sig = dupprintf(" %d", signum);
		} else {
		    /* As per the drafts. */
		    char *sig;
		    int siglen;
		    ssh_pkt_getstring(pktin, &sig, &siglen);
		    /* Signal name isn't supposed to be blank, but
		     * let's cope gracefully if it is. */
		    if (siglen) {
			fmt_sig = dupprintf(" \"%.*s\"",
					    siglen, sig);
		    }
		}
		core = ssh2_pkt_getbool(pktin);
		ssh_pkt_getstring(pktin, &msg, &msglen);
		if (msglen) {
		    fmt_msg = dupprintf(" (\"%.*s\")", msglen, msg);
		}
		/* ignore lang tag */
	    } /* else don't attempt to parse */
	    logeventf(ssh, "Server exited on signal%s%s%s",
		      fmt_sig, core ? " (core dumped)" : "",
		      fmt_msg);
	    if (*fmt_sig) sfree(fmt_sig);
	    if (*fmt_msg) sfree(fmt_msg);
	    reply = SSH2_MSG_CHANNEL_SUCCESS;

	}
    } else {
	/*
	 * This is a channel request we don't know
	 * about, so we now either ignore the request
	 * or respond with CHANNEL_FAILURE, depending
	 * on want_reply.
	 */
	reply = SSH2_MSG_CHANNEL_FAILURE;
    }
    if (want_reply) {
	pktout = ssh2_pkt_init(reply);
	ssh2_pkt_adduint32(pktout, c->remoteid);
	ssh2_pkt_send(ssh, pktout);
    }
}

static void ssh2_msg_global_request(Ssh ssh, struct Packet *pktin)
{
    char *type;
    int typelen, want_reply;
    struct Packet *pktout;

    ssh_pkt_getstring(pktin, &type, &typelen);
    want_reply = ssh2_pkt_getbool(pktin);

    /*
     * We currently don't support any global requests
     * at all, so we either ignore the request or
     * respond with REQUEST_FAILURE, depending on
     * want_reply.
     */
    if (want_reply) {
	pktout = ssh2_pkt_init(SSH2_MSG_REQUEST_FAILURE);
	ssh2_pkt_send(ssh, pktout);
    }
}

static void ssh2_msg_channel_open(Ssh ssh, struct Packet *pktin)
{
    char *type;
    int typelen;
    char *peeraddr;
    int peeraddrlen;
    int peerport;
    char *error = NULL;
    struct ssh_channel *c;
    unsigned remid, winsize, pktsize;
    struct Packet *pktout;

    ssh_pkt_getstring(pktin, &type, &typelen);
    c = snew(struct ssh_channel);
    c->ssh = ssh;

    remid = ssh_pkt_getuint32(pktin);
    winsize = ssh_pkt_getuint32(pktin);
    pktsize = ssh_pkt_getuint32(pktin);

    if (typelen == 3 && !memcmp(type, "x11", 3)) {
	char *addrstr;

	ssh_pkt_getstring(pktin, &peeraddr, &peeraddrlen);
	addrstr = snewn(peeraddrlen+1, char);
	memcpy(addrstr, peeraddr, peeraddrlen);
	addrstr[peeraddrlen] = '\0';
	peerport = ssh_pkt_getuint32(pktin);

	logeventf(ssh, "Received X11 connect request from %s:%d",
		  addrstr, peerport);

	if (!ssh->X11_fwd_enabled)
	    error = "X11 forwarding is not enabled";
	else if (x11_init(&c->u.x11.s, ssh->cfg.x11_display, c,
			  ssh->x11auth, addrstr, peerport,
			  &ssh->cfg) != NULL) {
	    error = "Unable to open an X11 connection";
	} else {
	    logevent("Opening X11 forward connection succeeded");
	    c->type = CHAN_X11;
	}

	sfree(addrstr);
    } else if (typelen == 15 &&
	       !memcmp(type, "forwarded-tcpip", 15)) {
	struct ssh_rportfwd pf, *realpf;
	char *dummy;
	int dummylen;
	ssh_pkt_getstring(pktin, &dummy, &dummylen);/* skip address */
	pf.sport = ssh_pkt_getuint32(pktin);
	ssh_pkt_getstring(pktin, &peeraddr, &peeraddrlen);
	peerport = ssh_pkt_getuint32(pktin);
	realpf = find234(ssh->rportfwds, &pf, NULL);
	logeventf(ssh, "Received remote port %d open request "
		  "from %s:%d", pf.sport, peeraddr, peerport);
	if (realpf == NULL) {
	    error = "Remote port is not recognised";
	} else {
	    const char *e = pfd_newconnect(&c->u.pfd.s,
					   realpf->dhost,
					   realpf->dport, c,
					   &ssh->cfg,
					   realpf->pfrec->addressfamily);
	    logeventf(ssh, "Attempting to forward remote port to "
		      "%s:%d", realpf->dhost, realpf->dport);
	    if (e != NULL) {
		logeventf(ssh, "Port open failed: %s", e);
		error = "Port open failed";
	    } else {
		logevent("Forwarded port opened successfully");
		c->type = CHAN_SOCKDATA;
	    }
	}
    } else if (typelen == 22 &&
	       !memcmp(type, "auth-agent@openssh.com", 3)) {
	if (!ssh->agentfwd_enabled)
	    error = "Agent forwarding is not enabled";
	else {
	    c->type = CHAN_AGENT;	/* identify channel type */
	    c->u.a.lensofar = 0;
	}
    } else {
	error = "Unsupported channel type requested";
    }

    c->remoteid = remid;
    c->halfopen = FALSE;
    if (error) {
	pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_OPEN_FAILURE);
	ssh2_pkt_adduint32(pktout, c->remoteid);
	ssh2_pkt_adduint32(pktout, SSH2_OPEN_CONNECT_FAILED);
	ssh2_pkt_addstring(pktout, error);
	ssh2_pkt_addstring(pktout, "en");	/* language tag */
	ssh2_pkt_send(ssh, pktout);
	logeventf(ssh, "Rejected channel open: %s", error);
	sfree(c);
    } else {
	c->localid = alloc_channel_id(ssh);
	c->closes = 0;
	c->v.v2.locwindow = OUR_V2_WINSIZE;
	c->v.v2.remwindow = winsize;
	c->v.v2.remmaxpkt = pktsize;
	bufchain_init(&c->v.v2.outbuffer);
	add234(ssh->channels, c);
	pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_OPEN_CONFIRMATION);
	ssh2_pkt_adduint32(pktout, c->remoteid);
	ssh2_pkt_adduint32(pktout, c->localid);
	ssh2_pkt_adduint32(pktout, c->v.v2.locwindow);
	ssh2_pkt_adduint32(pktout, OUR_V2_MAXPKT);	/* our max pkt size */
	ssh2_pkt_send(ssh, pktout);
    }
}

/*
 * Handle the SSH-2 userauth and connection layers.
 */
static void do_ssh2_authconn(Ssh ssh, unsigned char *in, int inlen,
			     struct Packet *pktin)
{
    struct do_ssh2_authconn_state {
	enum {
	    AUTH_INVALID, AUTH_PUBLICKEY_AGENT, AUTH_PUBLICKEY_FILE,
		AUTH_PASSWORD,
		AUTH_KEYBOARD_INTERACTIVE,
		AUTH_GSSAPI
	} method;
	enum {
	    AUTH_TYPE_NONE,
		AUTH_TYPE_PUBLICKEY,
		AUTH_TYPE_PUBLICKEY_OFFER_LOUD,
		AUTH_TYPE_PUBLICKEY_OFFER_QUIET,
		AUTH_TYPE_PASSWORD,
		AUTH_TYPE_KEYBOARD_INTERACTIVE,
		AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET,
		AUTH_TYPE_GSSAPI
	} type;
	int gotit, need_pw, can_pubkey, can_passwd, can_keyb_inter, can_gssapi;
	int tried_pubkey_config, tried_agent, tried_gssapi;

	int kbd_inter_running, kbd_inter_refused;
	int we_are_in;
	int num_prompts, curr_prompt, echo;
	char username[100];
	int got_username;
	char pwprompt[512];
	char password[100];
	void *publickey_blob;
	int publickey_bloblen;
	unsigned char request[5], *response, *p;
	int responselen;
	int keyi, nkeys;
	int authed;
	char *pkblob, *alg, *commentp;
	int pklen, alglen, commentlen;
	int siglen, retlen, len;
	char *q, *agentreq, *ret;
	int try_send;
	int num_env, env_left, env_ok;
	struct Packet *pktout;
	Gssctxt *gssctxt;
    };
    crState(do_ssh2_authconn_state);

    crBegin(ssh->do_ssh2_authconn_crstate);

    /*
     * Request userauth protocol, and await a response to it.
     */
    s->pktout = ssh2_pkt_init(SSH2_MSG_SERVICE_REQUEST);
    ssh2_pkt_addstring(s->pktout, "ssh-userauth");
    ssh2_pkt_send(ssh, s->pktout);
    crWaitUntilV(pktin);
    if (pktin->type != SSH2_MSG_SERVICE_ACCEPT) {
	bombout(("Server refused user authentication protocol"));
	crStopV;
    }

    /*
     * We repeat this whole loop, including the username prompt,
     * until we manage a successful authentication. If the user
     * types the wrong _password_, they can be sent back to the
     * beginning to try another username, if this is configured on.
     * (If they specify a username in the config, they are never
     * asked, even if they do give a wrong password.)
     * 
     * I think this best serves the needs of
     * 
     *  - the people who have no configuration, no keys, and just
     *    want to try repeated (username,password) pairs until they
     *    type both correctly
     * 
     *  - people who have keys and configuration but occasionally
     *    need to fall back to passwords
     * 
     *  - people with a key held in Pageant, who might not have
     *    logged in to a particular machine before; so they want to
     *    type a username, and then _either_ their key will be
     *    accepted, _or_ they will type a password. If they mistype
     *    the username they will want to be able to get back and
     *    retype it!
     */
    s->username[0] = '\0';
    s->got_username = FALSE;
    do {
	/*
	 * Get a username.
	 */
	if (s->got_username && !ssh->cfg.change_username) {
	    /*
	     * We got a username last time round this loop, and
	     * with change_username turned off we don't try to get
	     * it again.
	     */
	} else if (!*ssh->cfg.username) {
	    if (ssh_get_line && !ssh_getline_pw_only) {
#ifdef MPEXT
		if (!ssh_get_line(ssh->frontend, "login as: ",
				  s->username, sizeof(s->username), FALSE)) {
#else
		if (!ssh_get_line("login as: ",
				  s->username, sizeof(s->username), FALSE)) {
#endif
		    /*
		     * get_line failed to get a username.
		     * Terminate.
		     */
		    logevent("No username provided. Abandoning session.");
		    ssh->close_expected = TRUE;
                    ssh_closing((Plug)ssh, NULL, 0, 0);
		    crStopV;
		}
	    } else {
		int ret;	       /* need not be saved across crReturn */
		c_write_str(ssh, "login as: ");
		ssh->send_ok = 1;
		setup_userpass_input(ssh, s->username, sizeof(s->username), 1);
		do {
		    crWaitUntilV(!pktin);
		    ret = process_userpass_input(ssh, in, inlen);
		} while (ret == 0);
		if (ret < 0)
		    cleanup_exit(0);
		c_write_str(ssh, "\r\n");
	    }
	    s->username[strcspn(s->username, "\n\r")] = '\0';
	} else {
	    char *stuff;
	    strncpy(s->username, ssh->cfg.username, sizeof(s->username));
	    s->username[sizeof(s->username)-1] = '\0';
	    if ((flags & FLAG_VERBOSE) || (flags & FLAG_INTERACTIVE)) {
		stuff = dupprintf("Using username \"%s\".\r\n", s->username);
		c_write_str(ssh, stuff);
		sfree(stuff);
	    }
	}
	s->got_username = TRUE;

	/*
	 * Send an authentication request using method "none": (a)
	 * just in case it succeeds, and (b) so that we know what
	 * authentication methods we can usefully try next.
	 */
	ssh->pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;

	s->pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
	ssh2_pkt_addstring(s->pktout, s->username);
	ssh2_pkt_addstring(s->pktout, "ssh-connection");/* service requested */
	ssh2_pkt_addstring(s->pktout, "none");    /* method */
	ssh2_pkt_send(ssh, s->pktout);
	s->type = AUTH_TYPE_NONE;
	s->gotit = FALSE;
	s->we_are_in = FALSE;

	s->tried_pubkey_config = FALSE;
	s->tried_agent = FALSE;
	s->kbd_inter_running = FALSE;
	s->kbd_inter_refused = FALSE;
	s->gssctxt = NULL;
	s->tried_gssapi = FALSE;

	/* Load the pub half of ssh->cfg.keyfile so we notice if it's in Pageant */
	if (!filename_is_null(ssh->cfg.keyfile)) {
	    int keytype;
	    logeventf(ssh, "Reading private key file \"%.150s\"",
		      filename_to_str(&ssh->cfg.keyfile));
	    keytype = key_type(&ssh->cfg.keyfile);
	    if (keytype == SSH_KEYTYPE_SSH2) {
		s->publickey_blob =
		    ssh2_userkey_loadpub(&ssh->cfg.keyfile, NULL,
					 &s->publickey_bloblen, NULL);
	    } else {
		char *msgbuf;
		logeventf(ssh, "Unable to use this key file (%s)",
			  key_type_to_str(keytype));
		msgbuf = dupprintf("Unable to use key file \"%.150s\""
				   " (%s)\r\n",
				   filename_to_str(&ssh->cfg.keyfile),
				   key_type_to_str(keytype));
		c_write_str(ssh, msgbuf);
		sfree(msgbuf);
		s->publickey_blob = NULL;
	    }
	} else
	    s->publickey_blob = NULL;

	while (1) {
	    /*
	     * Wait for the result of the last authentication request.
	     */
	    if (!s->gotit)
		crWaitUntilV(pktin);
	    while (pktin->type == SSH2_MSG_USERAUTH_BANNER) {
		char *banner;
		int size;
		/*
		 * Don't show the banner if we're operating in
		 * non-verbose non-interactive mode. (It's probably
		 * a script, which means nobody will read the
		 * banner _anyway_, and moreover the printing of
		 * the banner will screw up processing on the
		 * output of (say) plink.)
		 */
		if (flags & (FLAG_VERBOSE | FLAG_INTERACTIVE)) {
		    ssh_pkt_getstring(pktin, &banner, &size);
		    if (banner)
			#ifdef MPEXT
			{
				display_banner(ssh->frontend, banner, size);
			#endif
			c_write_untrusted(ssh, banner, size);
			#ifdef MPEXT
			}
			#endif
		}
		crWaitUntilV(pktin);
	    }
	    if (pktin->type == SSH2_MSG_USERAUTH_SUCCESS) {
		logevent("Access granted");
		s->we_are_in = TRUE;
		break;
	    }

	    if (s->kbd_inter_running &&
		pktin->type == SSH2_MSG_USERAUTH_INFO_REQUEST) {
		/*
		 * This is either a further set-of-prompts packet
		 * in keyboard-interactive authentication, or it's
		 * the same one and we came back here with `gotit'
		 * set. In the former case, we must reset the
		 * curr_prompt variable.
		 */
		if (!s->gotit)
		    s->curr_prompt = 0;
	    } else if (pktin->type == SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ) {
		/* FIXME: perhaps we should support this? */
		bombout(("PASSWD_CHANGEREQ not yet supported"));
		crStopV;
	    } else if (pktin->type != SSH2_MSG_USERAUTH_FAILURE) {
		bombout(("Strange packet received during authentication: type %d",
			 pktin->type));
		crStopV;
	    }

	    s->gotit = FALSE;

	    /*
	     * OK, we're now sitting on a USERAUTH_FAILURE message, so
	     * we can look at the string in it and know what we can
	     * helpfully try next.
	     */
	    if (pktin->type == SSH2_MSG_USERAUTH_FAILURE) {
		char *methods;
		int methlen;
		ssh_pkt_getstring(pktin, &methods, &methlen);
		s->kbd_inter_running = FALSE;
		if (!ssh2_pkt_getbool(pktin)) {
		    /*
		     * We have received an unequivocal Access
		     * Denied. This can translate to a variety of
		     * messages:
		     * 
		     *  - if we'd just tried "none" authentication,
		     *    it's not worth printing anything at all
		     * 
		     *  - if we'd just tried a public key _offer_,
		     *    the message should be "Server refused our
		     *    key" (or no message at all if the key
		     *    came from Pageant)
		     * 
		     *  - if we'd just tried anything else, the
		     *    message really should be "Access denied".
		     * 
		     * Additionally, if we'd just tried password
		     * authentication, we should break out of this
		     * whole loop so as to go back to the username
		     * prompt (iff we're configured to allow
		     * username change attempts).
		     */
		    if (s->type == AUTH_TYPE_NONE) {
			/* do nothing */
		    } else if (s->type == AUTH_TYPE_PUBLICKEY_OFFER_LOUD ||
			       s->type == AUTH_TYPE_PUBLICKEY_OFFER_QUIET) {
			if (s->type == AUTH_TYPE_PUBLICKEY_OFFER_LOUD)
			    c_write_str(ssh, "Server refused our key\r\n");
			logevent("Server refused public key");
		    } else if (s->type==AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET) {
			/* server declined keyboard-interactive; ignore */
		    } else if (s->type == AUTH_TYPE_GSSAPI) {
			/* do nothing */
		    } else {
			c_write_str(ssh, "Access denied\r\n");
			logevent("Access denied");
			if (s->type == AUTH_TYPE_PASSWORD &&
			    ssh->cfg.change_username) {
			    /* XXX perhaps we should allow
			     * keyboard-interactive to do this too? */
			    s->we_are_in = FALSE;
			    break;
			}
		    }
		} else {
		    c_write_str(ssh, "Further authentication required\r\n");
		    logevent("Further authentication required");
		}

		s->can_gssapi = ssh->cfg.try_gssapi_auth &&
		    in_commasep_string("gssapi-with-mic", methods, methlen);
		s->can_pubkey =
		    in_commasep_string("publickey", methods, methlen);
		s->can_passwd =
		    in_commasep_string("password", methods, methlen);
		s->can_keyb_inter = ssh->cfg.try_ki_auth &&
		    in_commasep_string("keyboard-interactive", methods, methlen);
	    }

	    s->method = 0;
	    ssh->pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
	    s->need_pw = FALSE;

	    /*
	     * Most password/passphrase prompts will be
	     * non-echoing, so we set this to 0 by default.
	     * Exception is that some keyboard-interactive prompts
	     * can be echoing, in which case we'll set this to 1.
	     */
	    s->echo = 0;

	    if (!s->method && s->can_gssapi && !s->tried_gssapi) {
		s->tried_gssapi = TRUE;

		logevent("GSSAPI: trying GSSAPI auth");

		if (ssh_gssapi_init(ssh))
		{
		    ssh_gssapi_build_ctx(&s->gssctxt);

		    if (GSS_ERROR(ssh_gssapi_acquire_cred(ssh, &s->gssctxt))) {
			ssh_gssapi_display_status(ssh, s->gssctxt);
			ssh_gssapi_delete_ctx(&s->gssctxt);
		    } else {
			ssh->pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
			ssh->pkt_ctx |= SSH2_PKTCTX_GSSAPI;

			s->pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
			ssh2_pkt_addstring(s->pktout, s->username);
			ssh2_pkt_addstring(s->pktout, "ssh-connection");    /* service requested */
			ssh2_pkt_addstring(s->pktout, "gssapi-with-mic");   /* method */
			ssh2_pkt_adduint32(s->pktout, 1);

			ssh2_pkt_adduint32(s->pktout, gss_mech_krb5->length + 2);
			ssh2_pkt_addbool(s->pktout, (char) SSH_GSS_OIDTYPE);
			ssh2_pkt_addbool(s->pktout, (char) gss_mech_krb5->length);
			ssh2_pkt_adddata(s->pktout, gss_mech_krb5->elements, gss_mech_krb5->length);

			ssh2_pkt_send(ssh, s->pktout);

			s->type = AUTH_TYPE_GSSAPI;

			while (1) {
			    crWaitUntilV(pktin);

			    if (pktin->type == SSH2_MSG_USERAUTH_GSSAPI_RESPONSE) {
				if (input_gssapi_response(ssh, pktin, s->gssctxt, s->username))
				    break;
			    } else if (pktin->type == SSH2_MSG_USERAUTH_GSSAPI_TOKEN) {
				if (input_gssapi_token(ssh, pktin, s->gssctxt, s->username))
				    break;
			    } else if (pktin->type == SSH2_MSG_USERAUTH_GSSAPI_ERROR) {
				input_gssapi_error(ssh, pktin);
				break;
			    } else if (pktin->type == SSH2_MSG_USERAUTH_GSSAPI_ERRTOK) {
				input_gssapi_errtok(ssh, pktin, s->gssctxt);
				break;
			    } else if (pktin->type == SSH2_MSG_USERAUTH_SUCCESS) {
				s->gotit = TRUE;
				break;
			    } else if (pktin->type != SSH2_MSG_USERAUTH_FAILURE) {
				ssh_gssapi_delete_ctx(&s->gssctxt);
				bombout(("ERROR in GSSAPI authentication protocol"));
				crStopV;
			    }
			}

			ssh_gssapi_delete_ctx(&s->gssctxt);

			if (s->gotit)
			    continue;
		    }
		}
	    }

	    if (!s->method && s->can_pubkey &&
		agent_exists() && !s->tried_agent) {
		/*
		 * Attempt public-key authentication using Pageant.
		 */
		void *r;
		s->authed = FALSE;

		ssh->pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
		ssh->pkt_ctx |= SSH2_PKTCTX_PUBLICKEY;

		s->tried_agent = TRUE;

		logevent("Pageant is running. Requesting keys.");

		/* Request the keys held by the agent. */
		PUT_32BIT(s->request, 1);
		s->request[4] = SSH2_AGENTC_REQUEST_IDENTITIES;
		if (!agent_query(s->request, 5, &r, &s->responselen,
				 ssh_agent_callback, ssh)) {
		    do {
			crReturnV;
			if (pktin) {
			    bombout(("Unexpected data from server while"
				     " waiting for agent response"));
			    crStopV;
			}
		    } while (pktin || inlen > 0);
		    r = ssh->agent_response;
		    s->responselen = ssh->agent_response_len;
		}
		s->response = (unsigned char *) r;
		if (s->response && s->responselen >= 5 &&
		    s->response[4] == SSH2_AGENT_IDENTITIES_ANSWER) {
		    s->p = s->response + 5;
		    s->nkeys = GET_32BIT(s->p);
		    s->p += 4;
		    logeventf(ssh, "Pageant has %d SSH-2 keys", s->nkeys);
		    for (s->keyi = 0; s->keyi < s->nkeys; s->keyi++) {
			void *vret;

			logeventf(ssh, "Trying Pageant key #%d", s->keyi);
			s->pklen = GET_32BIT(s->p);
			s->p += 4;
			if (s->publickey_blob &&
			    s->pklen == s->publickey_bloblen &&
			    !memcmp(s->p, s->publickey_blob,
				    s->publickey_bloblen)) {
			    logevent("This key matches configured key file");
			    s->tried_pubkey_config = 1;
			}
			s->pkblob = (char *)s->p;
			s->p += s->pklen;
			s->alglen = GET_32BIT(s->pkblob);
			s->alg = s->pkblob + 4;
			s->commentlen = GET_32BIT(s->p);
			s->p += 4;
			s->commentp = (char *)s->p;
			s->p += s->commentlen;
			s->pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
			ssh2_pkt_addstring(s->pktout, s->username);
			ssh2_pkt_addstring(s->pktout, "ssh-connection");	/* service requested */
			ssh2_pkt_addstring(s->pktout, "publickey");	/* method */
			ssh2_pkt_addbool(s->pktout, FALSE);	/* no signature included */
			ssh2_pkt_addstring_start(s->pktout);
			ssh2_pkt_addstring_data(s->pktout, s->alg, s->alglen);
			ssh2_pkt_addstring_start(s->pktout);
			ssh2_pkt_addstring_data(s->pktout, s->pkblob, s->pklen);
			ssh2_pkt_send(ssh, s->pktout);

			crWaitUntilV(pktin);
			if (pktin->type != SSH2_MSG_USERAUTH_PK_OK) {
			    logevent("Key refused");
			    continue;
			}

			if (flags & FLAG_VERBOSE) {
			    c_write_str(ssh, "Authenticating with "
					"public key \"");
			    c_write(ssh, s->commentp, s->commentlen);
			    c_write_str(ssh, "\" from agent\r\n");
			}

			/*
			 * Server is willing to accept the key.
			 * Construct a SIGN_REQUEST.
			 */
			s->pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
			ssh2_pkt_addstring(s->pktout, s->username);
			ssh2_pkt_addstring(s->pktout, "ssh-connection");	/* service requested */
			ssh2_pkt_addstring(s->pktout, "publickey");	/* method */
			ssh2_pkt_addbool(s->pktout, TRUE);
			ssh2_pkt_addstring_start(s->pktout);
			ssh2_pkt_addstring_data(s->pktout, s->alg, s->alglen);
			ssh2_pkt_addstring_start(s->pktout);
			ssh2_pkt_addstring_data(s->pktout, s->pkblob, s->pklen);

			s->siglen = s->pktout->length - 5 + 4 + 20;
                        if (ssh->remote_bugs & BUG_SSH2_PK_SESSIONID)
                            s->siglen -= 4;
			s->len = 1;       /* message type */
			s->len += 4 + s->pklen;	/* key blob */
			s->len += 4 + s->siglen;	/* data to sign */
			s->len += 4;      /* flags */
			s->agentreq = snewn(4 + s->len, char);
			PUT_32BIT(s->agentreq, s->len);
			s->q = s->agentreq + 4;
			*s->q++ = SSH2_AGENTC_SIGN_REQUEST;
			PUT_32BIT(s->q, s->pklen);
			s->q += 4;
			memcpy(s->q, s->pkblob, s->pklen);
			s->q += s->pklen;
			PUT_32BIT(s->q, s->siglen);
			s->q += 4;
			/* Now the data to be signed... */
                        if (!(ssh->remote_bugs & BUG_SSH2_PK_SESSIONID)) {
                            PUT_32BIT(s->q, 20);
                            s->q += 4;
                        }
			memcpy(s->q, ssh->v2_session_id, 20);
			s->q += 20;
			memcpy(s->q, s->pktout->data + 5,
			       s->pktout->length - 5);
			s->q += s->pktout->length - 5;
			/* And finally the (zero) flags word. */
			PUT_32BIT(s->q, 0);
			if (!agent_query(s->agentreq, s->len + 4,
					 &vret, &s->retlen,
					 ssh_agent_callback, ssh)) {
			    do {
				crReturnV;
				if (pktin) {
				    bombout(("Unexpected data from server"
					     " while waiting for agent"
					     " response"));
				    crStopV;
				}
			    } while (pktin || inlen > 0);
			    vret = ssh->agent_response;
			    s->retlen = ssh->agent_response_len;
			}
			s->ret = vret;
			sfree(s->agentreq);
			if (s->ret) {
			    if (s->ret[4] == SSH2_AGENT_SIGN_RESPONSE) {
				logevent("Sending Pageant's response");
				ssh2_add_sigblob(ssh, s->pktout,
						 s->pkblob, s->pklen,
						 s->ret + 9,
						 GET_32BIT(s->ret + 5));
				ssh2_pkt_send(ssh, s->pktout);
				s->authed = TRUE;
				break;
			    } else {
				logevent
				    ("Pageant failed to answer challenge");
				sfree(s->ret);
			    }
			}
		    }
		    if (s->authed)
			continue;
		}
		sfree(s->response);
	    }

	    if (!s->method && s->can_pubkey && s->publickey_blob
		&& !s->tried_pubkey_config) {
		unsigned char *pub_blob;
		char *algorithm, *comment;
		int pub_blob_len;

		s->tried_pubkey_config = TRUE;

		ssh->pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
		ssh->pkt_ctx |= SSH2_PKTCTX_PUBLICKEY;

		/*
		 * Try the public key supplied in the configuration.
		 *
		 * First, offer the public blob to see if the server is
		 * willing to accept it.
		 */
		pub_blob =
		    (unsigned char *)ssh2_userkey_loadpub(&ssh->cfg.keyfile,
							  &algorithm,
							  &pub_blob_len,
							  NULL);
		if (pub_blob) {
		    s->pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
		    ssh2_pkt_addstring(s->pktout, s->username);
		    ssh2_pkt_addstring(s->pktout, "ssh-connection");	/* service requested */
		    ssh2_pkt_addstring(s->pktout, "publickey");	/* method */
		    ssh2_pkt_addbool(s->pktout, FALSE);	/* no signature included */
		    ssh2_pkt_addstring(s->pktout, algorithm);
		    ssh2_pkt_addstring_start(s->pktout);
		    ssh2_pkt_addstring_data(s->pktout, (char *)pub_blob,
					    pub_blob_len);
		    ssh2_pkt_send(ssh, s->pktout);
		    logevent("Offered public key");

		    crWaitUntilV(pktin);
		    if (pktin->type != SSH2_MSG_USERAUTH_PK_OK) {
			s->gotit = TRUE;
			s->type = AUTH_TYPE_PUBLICKEY_OFFER_LOUD;
			continue;      /* key refused; give up on it */
		    }

		    logevent("Offer of public key accepted");
		    /*
		     * Actually attempt a serious authentication using
		     * the key.
		     */
		    if (ssh2_userkey_encrypted(&ssh->cfg.keyfile, &comment)) {
			sprintf(s->pwprompt,
				"Passphrase for key \"%.100s\": ",
				comment);
			s->need_pw = TRUE;
		    } else {
			s->need_pw = FALSE;
		    }
		    if (flags & FLAG_VERBOSE) {
			c_write_str(ssh, "Authenticating with public key \"");
			c_write_str(ssh, comment);
			c_write_str(ssh, "\"\r\n");
		    }
		    s->method = AUTH_PUBLICKEY_FILE;
		}
	    }

	    if (!s->method && s->can_keyb_inter && !s->kbd_inter_refused &&
		!s->kbd_inter_running) {
		s->method = AUTH_KEYBOARD_INTERACTIVE;
		s->type = AUTH_TYPE_KEYBOARD_INTERACTIVE;

		ssh->pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
		ssh->pkt_ctx |= SSH2_PKTCTX_KBDINTER;

		s->pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
		ssh2_pkt_addstring(s->pktout, s->username);
		ssh2_pkt_addstring(s->pktout, "ssh-connection");	/* service requested */
		ssh2_pkt_addstring(s->pktout, "keyboard-interactive");	/* method */
		ssh2_pkt_addstring(s->pktout, ""); /* lang */
		ssh2_pkt_addstring(s->pktout, "");
		ssh2_pkt_send(ssh, s->pktout);

		crWaitUntilV(pktin);
		if (pktin->type != SSH2_MSG_USERAUTH_INFO_REQUEST) {
		    if (pktin->type == SSH2_MSG_USERAUTH_FAILURE)
			s->gotit = TRUE;
		    logevent("Keyboard-interactive authentication refused");
		    s->type = AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET;
		    s->kbd_inter_refused = TRUE; /* don't try it again */
		    continue;
		}

		c_write_str(ssh, "Using keyboard-interactive authentication.\r\n");
		s->kbd_inter_running = TRUE;
		s->curr_prompt = 0;
	    }

	    if (s->kbd_inter_running) {
		s->method = AUTH_KEYBOARD_INTERACTIVE;
		s->type = AUTH_TYPE_KEYBOARD_INTERACTIVE;

		ssh->pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
		ssh->pkt_ctx |= SSH2_PKTCTX_KBDINTER;

		if (s->curr_prompt == 0) {
		    /*
		     * We've got a fresh USERAUTH_INFO_REQUEST.
		     * Display header data, and start going through
		     * the prompts.
		     */
		    char *name, *inst, *lang;
		    int name_len, inst_len, lang_len;

		    ssh_pkt_getstring(pktin, &name, &name_len);
		    ssh_pkt_getstring(pktin, &inst, &inst_len);
		    ssh_pkt_getstring(pktin, &lang, &lang_len);
		    if (name_len > 0) {
			c_write_untrusted(ssh, name, name_len);
			c_write_str(ssh, "\r\n");
		    }
		    if (inst_len > 0) {
			c_write_untrusted(ssh, inst, inst_len);
			c_write_str(ssh, "\r\n");
		    }
		    s->num_prompts = ssh_pkt_getuint32(pktin);
		}

		/*
		 * If there are prompts remaining in the packet,
		 * display one and get a response.
		 */
		if (s->curr_prompt < s->num_prompts) {
		    char *prompt;
		    int prompt_len;

		    ssh_pkt_getstring(pktin, &prompt, &prompt_len);
		    if (prompt_len > 0) {
			static const char trunc[] = "<prompt truncated>: ";
			static const int prlen = sizeof(s->pwprompt) -
						 lenof(trunc);
			if (prompt_len > prlen) {
			    memcpy(s->pwprompt, prompt, prlen);
			    strcpy(s->pwprompt + prlen, trunc);
			} else {
			    memcpy(s->pwprompt, prompt, prompt_len);
			    s->pwprompt[prompt_len] = '\0';
			}
		    } else {
			strcpy(s->pwprompt,
			       "<server failed to send prompt>: ");
		    }
		    s->echo = ssh2_pkt_getbool(pktin);
		    s->need_pw = TRUE;
		} else
		    s->need_pw = FALSE;
	    }

	    if (!s->method && s->can_passwd) {
		s->method = AUTH_PASSWORD;
		ssh->pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
		ssh->pkt_ctx |= SSH2_PKTCTX_PASSWORD;
#ifdef MPEXT
		// To suppress CodeGuard warning
		snprintf(s->pwprompt, sizeof(s->pwprompt), "%s@%s's password: ", s->username,
			ssh->savedhost);
		s->pwprompt[sizeof(s->pwprompt) - 1] = '\0';
#else
		sprintf(s->pwprompt, "%.90s@%.90s's password: ", s->username,
			ssh->savedhost);
#endif
		s->need_pw = TRUE;
	    }

	    if (s->need_pw) {
		if (ssh_get_line) {
#ifdef MPEXT
		    if (!ssh_get_line(ssh->frontend, s->pwprompt, s->password,
				      sizeof(s->password), TRUE)) {
#else
		    if (!ssh_get_line(s->pwprompt, s->password,
				      sizeof(s->password), TRUE)) {
#endif
			/*
			 * get_line failed to get a password (for
			 * example because one was supplied on the
			 * command line which has already failed to
			 * work). Terminate.
			 */
			s->pktout = ssh2_pkt_init(SSH2_MSG_DISCONNECT);
			ssh2_pkt_adduint32(s->pktout,SSH2_DISCONNECT_BY_APPLICATION);
			ssh2_pkt_addstring(s->pktout, "No more passwords available"
					   " to try");
			ssh2_pkt_addstring(s->pktout, "en");	/* language tag */
			ssh2_pkt_send_noqueue(ssh, s->pktout);
			logevent("Unable to authenticate");
			connection_fatal(ssh->frontend,
					 "Unable to authenticate");
			ssh->close_expected = TRUE;
                        ssh_closing((Plug)ssh, NULL, 0, 0);
			crStopV;
		    }
		} else {
		    int ret;	       /* need not be saved across crReturn */
		    c_write_untrusted(ssh, s->pwprompt, strlen(s->pwprompt));
		    ssh->send_ok = 1;

		    setup_userpass_input(ssh, s->password,
					 sizeof(s->password), s->echo);
		    do {
			crWaitUntilV(!pktin);
			ret = process_userpass_input(ssh, in, inlen);
		    } while (ret == 0);
		    if (ret < 0)
			cleanup_exit(0);
		    c_write_str(ssh, "\r\n");
		}
	    }

	    if (s->method == AUTH_PUBLICKEY_FILE) {
		/*
		 * We have our passphrase. Now try the actual authentication.
		 */
		struct ssh2_userkey *key;
		const char *error = NULL;

		key = ssh2_load_userkey(&ssh->cfg.keyfile, s->password,
					&error);
		if (key == SSH2_WRONG_PASSPHRASE || key == NULL) {
		    if (key == SSH2_WRONG_PASSPHRASE) {
			c_write_str(ssh, "Wrong passphrase\r\n");
			s->tried_pubkey_config = FALSE;
		    } else {
			c_write_str(ssh, "Unable to load private key (");
			c_write_str(ssh, error);
			c_write_str(ssh, ")\r\n");
			s->tried_pubkey_config = TRUE;
		    }
		    /* Send a spurious AUTH_NONE to return to the top. */
		    s->pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
		    ssh2_pkt_addstring(s->pktout, s->username);
		    ssh2_pkt_addstring(s->pktout, "ssh-connection");	/* service requested */
		    ssh2_pkt_addstring(s->pktout, "none");	/* method */
		    ssh2_pkt_send(ssh, s->pktout);
		    s->type = AUTH_TYPE_NONE;
		} else {
		    unsigned char *pkblob, *sigblob, *sigdata;
		    int pkblob_len, sigblob_len, sigdata_len;
                    int p;

		    /*
		     * We have loaded the private key and the server
		     * has announced that it's willing to accept it.
		     * Hallelujah. Generate a signature and send it.
		     */
		    s->pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
		    ssh2_pkt_addstring(s->pktout, s->username);
		    ssh2_pkt_addstring(s->pktout, "ssh-connection");	/* service requested */
		    ssh2_pkt_addstring(s->pktout, "publickey");	/* method */
		    ssh2_pkt_addbool(s->pktout, TRUE);
		    ssh2_pkt_addstring(s->pktout, key->alg->name);
		    pkblob = key->alg->public_blob(key->data, &pkblob_len);
		    ssh2_pkt_addstring_start(s->pktout);
		    ssh2_pkt_addstring_data(s->pktout, (char *)pkblob, pkblob_len);

		    /*
		     * The data to be signed is:
		     *
		     *   string  session-id
		     *
		     * followed by everything so far placed in the
		     * outgoing packet.
		     */
		    sigdata_len = s->pktout->length - 5 + 4 + 20;
                    if (ssh->remote_bugs & BUG_SSH2_PK_SESSIONID)
                        sigdata_len -= 4;
		    sigdata = snewn(sigdata_len, unsigned char);
                    p = 0;
                    if (!(ssh->remote_bugs & BUG_SSH2_PK_SESSIONID)) {
                        PUT_32BIT(sigdata+p, 20);
                        p += 4;
                    }
		    memcpy(sigdata+p, ssh->v2_session_id, 20); p += 20;
		    memcpy(sigdata+p, s->pktout->data + 5,
			   s->pktout->length - 5);
                    p += s->pktout->length - 5;
                    assert(p == sigdata_len);
		    sigblob = key->alg->sign(key->data, (char *)sigdata,
					     sigdata_len, &sigblob_len);
		    ssh2_add_sigblob(ssh, s->pktout, pkblob, pkblob_len,
				     sigblob, sigblob_len);
		    sfree(pkblob);
		    sfree(sigblob);
		    sfree(sigdata);

		    ssh2_pkt_send(ssh, s->pktout);
		    s->type = AUTH_TYPE_PUBLICKEY;
		    key->alg->freekey(key->data);
		}
	    } else if (s->method == AUTH_PASSWORD) {
		/*
		 * We pad out the password packet to 256 bytes to make
		 * it harder for an attacker to find the length of the
		 * user's password.
		 *
		 * Anyone using a password longer than 256 bytes
		 * probably doesn't have much to worry about from
		 * people who find out how long their password is!
		 */
		s->pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
		s->pktout->forcepad = 256;
		ssh2_pkt_addstring(s->pktout, s->username);
		ssh2_pkt_addstring(s->pktout, "ssh-connection");	/* service requested */
		ssh2_pkt_addstring(s->pktout, "password");
		ssh2_pkt_addbool(s->pktout, FALSE);
		dont_log_password(ssh, s->pktout, PKTLOG_BLANK);
		ssh2_pkt_addstring(s->pktout, s->password);
		memset(s->password, 0, sizeof(s->password));
		end_log_omission(ssh, s->pktout);
		ssh2_pkt_send(ssh, s->pktout);
		logevent("Sent password");
		s->type = AUTH_TYPE_PASSWORD;
	    } else if (s->method == AUTH_KEYBOARD_INTERACTIVE) {
		if (s->curr_prompt == 0) {
		    s->pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_INFO_RESPONSE);
		    s->pktout->forcepad = 256;
		    ssh2_pkt_adduint32(s->pktout, s->num_prompts);
		}
		if (s->need_pw) {      /* only add pw if we just got one! */
		    dont_log_password(ssh, s->pktout, PKTLOG_BLANK);
		    ssh2_pkt_addstring(s->pktout, s->password);
		    memset(s->password, 0, sizeof(s->password));
		    end_log_omission(ssh, s->pktout);
		    s->curr_prompt++;
		}
		if (s->curr_prompt >= s->num_prompts) {
		    ssh2_pkt_send(ssh, s->pktout);
		} else {
		    /*
		     * If there are prompts remaining, we set
		     * `gotit' so that we won't attempt to get
		     * another packet. Then we go back round the
		     * loop and will end up retrieving another
		     * prompt out of the existing packet. Funky or
		     * what?
		     */
		    s->gotit = TRUE;
		}
		s->type = AUTH_TYPE_KEYBOARD_INTERACTIVE;
	    } else {
		c_write_str(ssh, "No supported authentication methods"
			    " left to try!\r\n");
		logevent("No supported authentications offered."
			 " Disconnecting");
		s->pktout = ssh2_pkt_init(SSH2_MSG_DISCONNECT);
		ssh2_pkt_adduint32(s->pktout, SSH2_DISCONNECT_BY_APPLICATION);
		ssh2_pkt_addstring(s->pktout, "No supported authentication"
				   " methods available");
		ssh2_pkt_addstring(s->pktout, "en");	/* language tag */
		ssh2_pkt_send_noqueue(ssh, s->pktout);
		ssh->close_expected = TRUE;
                ssh_closing((Plug)ssh, NULL, 0, 0);
		crStopV;
	    }
	}
    } while (!s->we_are_in);

    /*
     * Now we're authenticated for the connection protocol. The
     * connection protocol will automatically have started at this
     * point; there's no need to send SERVICE_REQUEST.
     */

    ssh->channels = newtree234(ssh_channelcmp);

    /*
     * Set up handlers for some connection protocol messages, so we
     * don't have to handle them repeatedly in this coroutine.
     */
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_WINDOW_ADJUST] =
	ssh2_msg_channel_window_adjust;
    ssh->packet_dispatch[SSH2_MSG_GLOBAL_REQUEST] =
	ssh2_msg_global_request;

    /*
     * Create the main session channel.
     */
    if (!ssh->cfg.ssh_no_shell) {
	ssh->mainchan = snew(struct ssh_channel);
	ssh->mainchan->ssh = ssh;
	ssh->mainchan->localid = alloc_channel_id(ssh);
	s->pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_OPEN);
	ssh2_pkt_addstring(s->pktout, "session");
	ssh2_pkt_adduint32(s->pktout, ssh->mainchan->localid);
	ssh->mainchan->v.v2.locwindow = OUR_V2_WINSIZE;
	ssh2_pkt_adduint32(s->pktout, ssh->mainchan->v.v2.locwindow);/* our window size */
	ssh2_pkt_adduint32(s->pktout, OUR_V2_MAXPKT);    /* our max pkt size */
	ssh2_pkt_send(ssh, s->pktout);
	crWaitUntilV(pktin);
	if (pktin->type != SSH2_MSG_CHANNEL_OPEN_CONFIRMATION) {
	    bombout(("Server refused to open a session"));
	    crStopV;
	    /* FIXME: error data comes back in FAILURE packet */
	}
	if (ssh_pkt_getuint32(pktin) != ssh->mainchan->localid) {
	    bombout(("Server's channel confirmation cited wrong channel"));
	    crStopV;
	}
	ssh->mainchan->remoteid = ssh_pkt_getuint32(pktin);
	ssh->mainchan->halfopen = FALSE;
	ssh->mainchan->type = CHAN_MAINSESSION;
	ssh->mainchan->closes = 0;
	ssh->mainchan->v.v2.remwindow = ssh_pkt_getuint32(pktin);
	ssh->mainchan->v.v2.remmaxpkt = ssh_pkt_getuint32(pktin);
	bufchain_init(&ssh->mainchan->v.v2.outbuffer);
	add234(ssh->channels, ssh->mainchan);
	update_specials_menu(ssh->frontend);
	logevent("Opened channel for session");
    } else
	ssh->mainchan = NULL;

    /*
     * Now we have a channel, make dispatch table entries for
     * general channel-based messages.
     */
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_DATA] =
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_EXTENDED_DATA] =
	ssh2_msg_channel_data;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_EOF] = ssh2_msg_channel_eof;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_CLOSE] = ssh2_msg_channel_close;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN_CONFIRMATION] =
	ssh2_msg_channel_open_confirmation;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN_FAILURE] =
	ssh2_msg_channel_open_failure;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_REQUEST] =
	ssh2_msg_channel_request;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN] =
	ssh2_msg_channel_open;

    /*
     * Potentially enable X11 forwarding.
     */
    if (ssh->mainchan && ssh->cfg.x11_forward) {
	char proto[20], data[64];
	logevent("Requesting X11 forwarding");
	ssh->x11auth = x11_invent_auth(proto, sizeof(proto),
				       data, sizeof(data), ssh->cfg.x11_auth);
        x11_get_real_auth(ssh->x11auth, ssh->cfg.x11_display);
	s->pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(s->pktout, ssh->mainchan->remoteid);
	ssh2_pkt_addstring(s->pktout, "x11-req");
	ssh2_pkt_addbool(s->pktout, 1);	       /* want reply */
	ssh2_pkt_addbool(s->pktout, 0);	       /* many connections */
	ssh2_pkt_addstring(s->pktout, proto);
	ssh2_pkt_addstring(s->pktout, data);
	ssh2_pkt_adduint32(s->pktout, x11_get_screen_number(ssh->cfg.x11_display));
	ssh2_pkt_send(ssh, s->pktout);

	crWaitUntilV(pktin);

	if (pktin->type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (pktin->type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout(("Unexpected response to X11 forwarding request:"
			 " packet type %d", pktin->type));
		crStopV;
	    }
	    logevent("X11 forwarding refused");
	} else {
	    logevent("X11 forwarding enabled");
	    ssh->X11_fwd_enabled = TRUE;
	}
    }

    /*
     * Enable port forwardings.
     */
    ssh_setup_portfwd(ssh, &ssh->cfg);

    /*
     * Potentially enable agent forwarding.
     */
    if (ssh->mainchan && ssh->cfg.agentfwd && agent_exists()) {
	logevent("Requesting OpenSSH-style agent forwarding");
	s->pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(s->pktout, ssh->mainchan->remoteid);
	ssh2_pkt_addstring(s->pktout, "auth-agent-req@openssh.com");
	ssh2_pkt_addbool(s->pktout, 1);	       /* want reply */
	ssh2_pkt_send(ssh, s->pktout);

	crWaitUntilV(pktin);

	if (pktin->type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (pktin->type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout(("Unexpected response to agent forwarding request:"
			 " packet type %d", pktin->type));
		crStopV;
	    }
	    logevent("Agent forwarding refused");
	} else {
	    logevent("Agent forwarding enabled");
	    ssh->agentfwd_enabled = TRUE;
	}
    }

    /*
     * Now allocate a pty for the session.
     */
    if (ssh->mainchan && !ssh->cfg.nopty) {
	/* Unpick the terminal-speed string. */
	/* XXX perhaps we should allow no speeds to be sent. */
        ssh->ospeed = 38400; ssh->ispeed = 38400; /* last-resort defaults */
	sscanf(ssh->cfg.termspeed, "%d,%d", &ssh->ospeed, &ssh->ispeed);
	/* Build the pty request. */
	s->pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(s->pktout, ssh->mainchan->remoteid);	/* recipient channel */
	ssh2_pkt_addstring(s->pktout, "pty-req");
	ssh2_pkt_addbool(s->pktout, 1);	       /* want reply */
	ssh2_pkt_addstring(s->pktout, ssh->cfg.termtype);
	ssh2_pkt_adduint32(s->pktout, ssh->term_width);
	ssh2_pkt_adduint32(s->pktout, ssh->term_height);
	ssh2_pkt_adduint32(s->pktout, 0);	       /* pixel width */
	ssh2_pkt_adduint32(s->pktout, 0);	       /* pixel height */
	ssh2_pkt_addstring_start(s->pktout);
	ssh2_pkt_addbyte(s->pktout, 128);	       /* TTY_OP_ISPEED */
	ssh2_pkt_adduint32(s->pktout, ssh->ispeed);
	ssh2_pkt_addbyte(s->pktout, 129);	       /* TTY_OP_OSPEED */
	ssh2_pkt_adduint32(s->pktout, ssh->ospeed);
	ssh2_pkt_addstring_data(s->pktout, "\0", 1); /* TTY_OP_END */
	ssh2_pkt_send(ssh, s->pktout);
	ssh->state = SSH_STATE_INTERMED;

	crWaitUntilV(pktin);

	if (pktin->type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (pktin->type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout(("Unexpected response to pty request:"
			 " packet type %d", pktin->type));
		crStopV;
	    }
	    c_write_str(ssh, "Server refused to allocate pty\r\n");
	    ssh->editing = ssh->echoing = 1;
	} else {
	    logeventf(ssh, "Allocated pty (ospeed %dbps, ispeed %dbps)",
		      ssh->ospeed, ssh->ispeed);
	}
    } else {
	ssh->editing = ssh->echoing = 1;
    }

    /*
     * Send environment variables.
     * 
     * Simplest thing here is to send all the requests at once, and
     * then wait for a whole bunch of successes or failures.
     */
    if (ssh->mainchan && *ssh->cfg.environmt) {
	char *e = ssh->cfg.environmt;
	char *var, *varend, *val;

	s->num_env = 0;

	while (*e) {
	    var = e;
	    while (*e && *e != '\t') e++;
	    varend = e;
	    if (*e == '\t') e++;
	    val = e;
	    while (*e) e++;
	    e++;

	    s->pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_REQUEST);
	    ssh2_pkt_adduint32(s->pktout, ssh->mainchan->remoteid);
	    ssh2_pkt_addstring(s->pktout, "env");
	    ssh2_pkt_addbool(s->pktout, 1);	       /* want reply */
	    ssh2_pkt_addstring_start(s->pktout);
	    ssh2_pkt_addstring_data(s->pktout, var, varend-var);
	    ssh2_pkt_addstring(s->pktout, val);
	    ssh2_pkt_send(ssh, s->pktout);

	    s->num_env++;
	}

	logeventf(ssh, "Sent %d environment variables", s->num_env);

	s->env_ok = 0;
	s->env_left = s->num_env;

	while (s->env_left > 0) {
	    crWaitUntilV(pktin);

	    if (pktin->type != SSH2_MSG_CHANNEL_SUCCESS) {
		if (pktin->type != SSH2_MSG_CHANNEL_FAILURE) {
		    bombout(("Unexpected response to environment request:"
			     " packet type %d", pktin->type));
		    crStopV;
		}
	    } else {
		s->env_ok++;
	    }

	    s->env_left--;
	}

	if (s->env_ok == s->num_env) {
	    logevent("All environment variables successfully set");
	} else if (s->env_ok == 0) {
	    logevent("All environment variables refused");
	    c_write_str(ssh, "Server refused to set environment variables\r\n");
	} else {
	    logeventf(ssh, "%d environment variables refused",
		      s->num_env - s->env_ok);
	    c_write_str(ssh, "Server refused to set all environment variables\r\n");
	}
    }

    /*
     * Start a shell or a remote command. We may have to attempt
     * this twice if the config data has provided a second choice
     * of command.
     */
    if (ssh->mainchan) while (1) {
	int subsys;
	char *cmd;

	if (ssh->fallback_cmd) {
	    subsys = ssh->cfg.ssh_subsys2;
	    cmd = ssh->cfg.remote_cmd_ptr2;
	} else {
	    subsys = ssh->cfg.ssh_subsys;
	    cmd = ssh->cfg.remote_cmd_ptr;
	    if (!cmd) cmd = ssh->cfg.remote_cmd;
	}

	s->pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(s->pktout, ssh->mainchan->remoteid);	/* recipient channel */
	if (subsys) {
	    ssh2_pkt_addstring(s->pktout, "subsystem");
	    ssh2_pkt_addbool(s->pktout, 1);	       /* want reply */
	    ssh2_pkt_addstring(s->pktout, cmd);
	} else if (*cmd) {
	    ssh2_pkt_addstring(s->pktout, "exec");
	    ssh2_pkt_addbool(s->pktout, 1);	       /* want reply */
	    ssh2_pkt_addstring(s->pktout, cmd);
	} else {
	    ssh2_pkt_addstring(s->pktout, "shell");
	    ssh2_pkt_addbool(s->pktout, 1);	       /* want reply */
	}
	ssh2_pkt_send(ssh, s->pktout);

	crWaitUntilV(pktin);

	if (pktin->type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (pktin->type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout(("Unexpected response to shell/command request:"
			 " packet type %d", pktin->type));
		crStopV;
	    }
	    /*
	     * We failed to start the command. If this is the
	     * fallback command, we really are finished; if it's
	     * not, and if the fallback command exists, try falling
	     * back to it before complaining.
	     */
	    if (!ssh->fallback_cmd && ssh->cfg.remote_cmd_ptr2 != NULL) {
		logevent("Primary command failed; attempting fallback");
		ssh->fallback_cmd = TRUE;
		continue;
	    }
	    bombout(("Server refused to start a shell/command"));
	    crStopV;
	} else {
	    logevent("Started a shell/command");
	}
	break;
    }

    ssh->state = SSH_STATE_SESSION;
    if (ssh->size_needed)
	ssh_size(ssh, ssh->term_width, ssh->term_height);
    if (ssh->eof_needed)
	ssh_special(ssh, TS_EOF);

    /*
     * Transfer data!
     */
    if (ssh->ldisc)
	ldisc_send(ssh->ldisc, NULL, 0, 0);/* cause ldisc to notice changes */
    if (ssh->mainchan)
	ssh->send_ok = 1;
    while (1) {
	crReturnV;
	s->try_send = FALSE;
	if (pktin) {

	    /*
	     * _All_ the connection-layer packets we expect to
	     * receive are now handled by the dispatch table.
	     * Anything that reaches here must be bogus.
	     */

	    bombout(("Strange packet received: type %d", pktin->type));
	    crStopV;
	} else if (ssh->mainchan) {
	    /*
	     * We have spare data. Add it to the channel buffer.
	     */
	    ssh2_add_channel_data(ssh->mainchan, (char *)in, inlen);
	    s->try_send = TRUE;
	}
	if (s->try_send) {
	    int i;
	    struct ssh_channel *c;
	    /*
	     * Try to send data on all channels if we can.
	     */
	    for (i = 0; NULL != (c = index234(ssh->channels, i)); i++)
		ssh2_try_send_and_unthrottle(c);
	}
    }

    crFinishV;
}

/*
 * Handlers for SSH-2 messages that might arrive at any moment.
 */
static void ssh2_msg_disconnect(Ssh ssh, struct Packet *pktin)
{
    /* log reason code in disconnect message */
    char *buf, *msg;
    int nowlen, reason, msglen;

    reason = ssh_pkt_getuint32(pktin);
    ssh_pkt_getstring(pktin, &msg, &msglen);

    if (reason > 0 && reason < lenof(ssh2_disconnect_reasons)) {
	buf = dupprintf("Received disconnect message (%s)",
			ssh2_disconnect_reasons[reason]);
    } else {
	buf = dupprintf("Received disconnect message (unknown"
			" type %d)", reason);
    }
    logevent(buf);
    sfree(buf);
    buf = dupprintf("Disconnection message text: %n%.*s",
		    &nowlen, msglen, msg);
    logevent(buf);
    bombout(("Server sent disconnect message\ntype %d (%s):\n\"%s\"",
	     reason,
	     (reason > 0 && reason < lenof(ssh2_disconnect_reasons)) ?
	     ssh2_disconnect_reasons[reason] : "unknown",
	     buf+nowlen));
    sfree(buf);
}

static void ssh2_msg_debug(Ssh ssh, struct Packet *pktin)
{
    /* log the debug message */
    char *msg;
    int msglen;
    int always_display;

    /* XXX maybe we should actually take notice of this */
    always_display = ssh2_pkt_getbool(pktin);
    ssh_pkt_getstring(pktin, &msg, &msglen);

    logeventf(ssh, "Remote debug message: %.*s", msglen, msg);
}

static void ssh2_msg_something_unimplemented(Ssh ssh, struct Packet *pktin)
{
    struct Packet *pktout;
    pktout = ssh2_pkt_init(SSH2_MSG_UNIMPLEMENTED);
    ssh2_pkt_adduint32(pktout, pktin->sequence);
    /*
     * UNIMPLEMENTED messages MUST appear in the same order as the
     * messages they respond to. Hence, never queue them.
     */
    ssh2_pkt_send_noqueue(ssh, pktout);
}

/*
 * Handle the top-level SSH-2 protocol.
 */
static void ssh2_protocol_setup(Ssh ssh)
{
    int i;

    /*
     * Most messages cause SSH2_MSG_UNIMPLEMENTED.
     */
    for (i = 0; i < 256; i++)
	ssh->packet_dispatch[i] = ssh2_msg_something_unimplemented;

    /*
     * Any message we actually understand, we set to NULL so that
     * the coroutines will get it.
     */
    ssh->packet_dispatch[SSH2_MSG_UNIMPLEMENTED] = NULL;
    ssh->packet_dispatch[SSH2_MSG_SERVICE_REQUEST] = NULL;
    ssh->packet_dispatch[SSH2_MSG_SERVICE_ACCEPT] = NULL;
    ssh->packet_dispatch[SSH2_MSG_KEXINIT] = NULL;
    ssh->packet_dispatch[SSH2_MSG_NEWKEYS] = NULL;
    ssh->packet_dispatch[SSH2_MSG_KEXDH_INIT] = NULL;
    ssh->packet_dispatch[SSH2_MSG_KEXDH_REPLY] = NULL;
    /* ssh->packet_dispatch[SSH2_MSG_KEX_DH_GEX_REQUEST] = NULL; duplicate case value */
    /* ssh->packet_dispatch[SSH2_MSG_KEX_DH_GEX_GROUP] = NULL; duplicate case value */
    ssh->packet_dispatch[SSH2_MSG_KEX_DH_GEX_INIT] = NULL;
    ssh->packet_dispatch[SSH2_MSG_KEX_DH_GEX_REPLY] = NULL;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_REQUEST] = NULL;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_FAILURE] = NULL;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_SUCCESS] = NULL;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_BANNER] = NULL;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_PK_OK] = NULL;
    /* ssh->packet_dispatch[SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ] = NULL; duplicate case value */
    /* ssh->packet_dispatch[SSH2_MSG_USERAUTH_INFO_REQUEST] = NULL; duplicate case value */
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_INFO_RESPONSE] = NULL;
    ssh->packet_dispatch[SSH2_MSG_GLOBAL_REQUEST] = NULL;
    ssh->packet_dispatch[SSH2_MSG_REQUEST_SUCCESS] = NULL;
    ssh->packet_dispatch[SSH2_MSG_REQUEST_FAILURE] = NULL;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN] = NULL;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN_CONFIRMATION] = NULL;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN_FAILURE] = NULL;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_WINDOW_ADJUST] = NULL;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_DATA] = NULL;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_EXTENDED_DATA] = NULL;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_EOF] = NULL;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_CLOSE] = NULL;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_REQUEST] = NULL;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_SUCCESS] = NULL;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_FAILURE] = NULL;
    /* ssh->packet_dispatch[SSH2_MSG_USERAUTH_GSSAPI_RESPONSE] = NULL; duplicate */
    /* ssh->packet_dispatch[SSH2_MSG_USERAUTH_GSSAPI_TOKEN] = NULL; duplicate */
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_GSSAPI_EXCHANGE_COMPLETE] = NULL;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_GSSAPI_ERROR] = NULL;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_GSSAPI_ERRTOK] = NULL;

    /*
     * These special message types we install handlers for.
     */
    ssh->packet_dispatch[SSH2_MSG_DISCONNECT] = ssh2_msg_disconnect;
    ssh->packet_dispatch[SSH2_MSG_IGNORE] = ssh_msg_ignore; /* shared with SSH-1 */
    ssh->packet_dispatch[SSH2_MSG_DEBUG] = ssh2_msg_debug;
}

static void ssh2_timer(void *ctx, long now)
{
    Ssh ssh = (Ssh)ctx;

    if (ssh->state == SSH_STATE_CLOSED)
	return;

    if (!ssh->kex_in_progress && ssh->cfg.ssh_rekey_time != 0 &&
	now - ssh->next_rekey >= 0) {
	do_ssh2_transport(ssh, "timeout", -1, NULL);
    }
}

static void ssh2_protocol(Ssh ssh, void *vin, int inlen,
			  struct Packet *pktin)
{
    unsigned char *in = (unsigned char *)vin;
    if (ssh->state == SSH_STATE_CLOSED)
	return;

    if (pktin) {
	ssh->incoming_data_size += pktin->encrypted_len;
	if (!ssh->kex_in_progress &&
	    ssh->max_data_size != 0 &&
	    ssh->incoming_data_size > ssh->max_data_size)
	    do_ssh2_transport(ssh, "too much data received", -1, NULL);
    }

    if (pktin && ssh->packet_dispatch[pktin->type]) {
	ssh->packet_dispatch[pktin->type](ssh, pktin);
	return;
    }

    if (!ssh->protocol_initial_phase_done ||
	(pktin && pktin->type >= 20 && pktin->type < 50)) {
	if (do_ssh2_transport(ssh, in, inlen, pktin) &&
	    !ssh->protocol_initial_phase_done) {
	    ssh->protocol_initial_phase_done = TRUE;
	    /*
	     * Allow authconn to initialise itself.
	     */
	    do_ssh2_authconn(ssh, NULL, 0, NULL);
	}
    } else {
	do_ssh2_authconn(ssh, in, inlen, pktin);
    }
}

/*
 * Called to set up the connection.
 *
 * Returns an error message, or NULL on success.
 */
static const char *ssh_init(void *frontend_handle, void **backend_handle,
			    Config *cfg,
			    char *host, int port, char **realhost, int nodelay,
			    int keepalive)
{
    const char *p;
    Ssh ssh;

    ssh = snew(struct ssh_tag);
    ssh->cfg = *cfg;		       /* STRUCTURE COPY */
    ssh->version = 0;		       /* when not ready yet */
    ssh->s = NULL;
    ssh->cipher = NULL;
    ssh->v1_cipher_ctx = NULL;
    ssh->crcda_ctx = NULL;
    ssh->cscipher = NULL;
    ssh->cs_cipher_ctx = NULL;
    ssh->sccipher = NULL;
    ssh->sc_cipher_ctx = NULL;
    ssh->csmac = NULL;
    ssh->cs_mac_ctx = NULL;
    ssh->scmac = NULL;
    ssh->sc_mac_ctx = NULL;
    ssh->cscomp = NULL;
    ssh->cs_comp_ctx = NULL;
    ssh->sccomp = NULL;
    ssh->sc_comp_ctx = NULL;
    ssh->kex = NULL;
    ssh->kex_ctx = NULL;
    ssh->hostkey = NULL;
    ssh->exitcode = -1;
    ssh->close_expected = FALSE;
    ssh->state = SSH_STATE_PREPACKET;
    ssh->size_needed = FALSE;
    ssh->eof_needed = FALSE;
    ssh->ldisc = NULL;
    ssh->logctx = NULL;
    ssh->deferred_send_data = NULL;
    ssh->deferred_len = 0;
    ssh->deferred_size = 0;
    ssh->fallback_cmd = 0;
    ssh->pkt_ctx = 0;
    ssh->x11auth = NULL;
    ssh->v1_compressing = FALSE;
    ssh->v2_outgoing_sequence = 0;
    ssh->ssh1_rdpkt_crstate = 0;
    ssh->ssh2_rdpkt_crstate = 0;
    ssh->do_ssh_init_crstate = 0;
    ssh->ssh_gotdata_crstate = 0;
    ssh->do_ssh1_connection_crstate = 0;
    ssh->do_ssh1_login_crstate = 0;
    ssh->do_ssh2_transport_crstate = 0;
    ssh->do_ssh2_authconn_crstate = 0;
    ssh->do_ssh_init_state = NULL;
    ssh->do_ssh1_login_state = NULL;
    ssh->do_ssh2_transport_state = NULL;
    ssh->do_ssh2_authconn_state = NULL;
    ssh->mainchan = NULL;
    ssh->throttled_all = 0;
    ssh->v1_stdout_throttling = 0;
    ssh->queue = NULL;
    ssh->queuelen = ssh->queuesize = 0;
    ssh->queueing = FALSE;
    ssh->qhead = ssh->qtail = NULL;
    ssh->deferred_rekey_reason = NULL;
    bufchain_init(&ssh->queued_incoming_data);
    ssh->frozen = FALSE;

    *backend_handle = ssh;

#ifdef MSCRYPTOAPI
    if (crypto_startup() == 0)
	return "Microsoft high encryption pack not installed!";
#endif

    ssh->frontend = frontend_handle;
    ssh->term_width = ssh->cfg.width;
    ssh->term_height = ssh->cfg.height;

    ssh->channels = NULL;
    ssh->rportfwds = NULL;
    ssh->portfwds = NULL;

    ssh->send_ok = 0;
    ssh->editing = 0;
    ssh->echoing = 0;
    ssh->v1_throttle_count = 0;
    ssh->overall_bufsize = 0;
    ssh->fallback_cmd = 0;

    ssh->protocol = NULL;

    ssh->protocol_initial_phase_done = FALSE;

    ssh->pinger = NULL;

    ssh->incoming_data_size = ssh->outgoing_data_size =
	ssh->deferred_data_size = 0L;
    ssh->max_data_size = parse_blocksize(ssh->cfg.ssh_rekey_data);
    ssh->kex_in_progress = FALSE;

    p = connect_to_host(ssh, host, port, realhost, nodelay, keepalive);
    if (p != NULL)
	return p;

    random_ref();

    return NULL;
}

static void ssh_free(void *handle)
{
    Ssh ssh = (Ssh) handle;
    struct ssh_channel *c;
    struct ssh_rportfwd *pf;

    if (ssh->v1_cipher_ctx)
	ssh->cipher->free_context(ssh->v1_cipher_ctx);
    if (ssh->cs_cipher_ctx)
	ssh->cscipher->free_context(ssh->cs_cipher_ctx);
    if (ssh->sc_cipher_ctx)
	ssh->sccipher->free_context(ssh->sc_cipher_ctx);
    if (ssh->cs_mac_ctx)
	ssh->csmac->free_context(ssh->cs_mac_ctx);
    if (ssh->sc_mac_ctx)
	ssh->scmac->free_context(ssh->sc_mac_ctx);
    if (ssh->cs_comp_ctx) {
	if (ssh->cscomp)
	    ssh->cscomp->compress_cleanup(ssh->cs_comp_ctx);
	else
	    zlib_compress_cleanup(ssh->cs_comp_ctx);
    }
    if (ssh->sc_comp_ctx) {
	if (ssh->sccomp)
	    ssh->sccomp->decompress_cleanup(ssh->sc_comp_ctx);
	else
	    zlib_decompress_cleanup(ssh->sc_comp_ctx);
    }
    if (ssh->kex_ctx)
	dh_cleanup(ssh->kex_ctx);
    sfree(ssh->savedhost);

    while (ssh->queuelen-- > 0)
	ssh_free_packet(ssh->queue[ssh->queuelen]);
    sfree(ssh->queue);

    while (ssh->qhead) {
	struct queued_handler *qh = ssh->qhead;
	ssh->qhead = qh->next;
	sfree(ssh->qhead);
    }
    ssh->qhead = ssh->qtail = NULL;

    if (ssh->channels) {
	while ((c = delpos234(ssh->channels, 0)) != NULL) {
	    switch (c->type) {
	      case CHAN_X11:
		if (c->u.x11.s != NULL)
		    x11_close(c->u.x11.s);
		break;
	      case CHAN_SOCKDATA:
		if (c->u.pfd.s != NULL)
		    pfd_close(c->u.pfd.s);
		break;
	    }
	    sfree(c);
	}
	freetree234(ssh->channels);
	ssh->channels = NULL;
    }

    if (ssh->rportfwds) {
	while ((pf = delpos234(ssh->rportfwds, 0)) != NULL)
	    sfree(pf);
	freetree234(ssh->rportfwds);
	ssh->rportfwds = NULL;
    }
    sfree(ssh->deferred_send_data);
    if (ssh->x11auth)
	x11_free_auth(ssh->x11auth);
    sfree(ssh->do_ssh_init_state);
    sfree(ssh->do_ssh1_login_state);
    sfree(ssh->do_ssh2_transport_state);
    sfree(ssh->do_ssh2_authconn_state);
    if (ssh->crcda_ctx) {
	crcda_free_context(ssh->crcda_ctx);
	ssh->crcda_ctx = NULL;
    }
    if (ssh->s)
	ssh_do_close(ssh, TRUE);
    expire_timer_context(ssh);
    if (ssh->pinger)
	pinger_free(ssh->pinger);
    bufchain_clear(&ssh->queued_incoming_data);
    sfree(ssh);

    random_unref();
}

/*
 * Reconfigure the SSH backend.
 */
static void ssh_reconfig(void *handle, Config *cfg)
{
    Ssh ssh = (Ssh) handle;
    char *rekeying = NULL, rekey_mandatory = FALSE;
    unsigned long old_max_data_size;

    pinger_reconfig(ssh->pinger, &ssh->cfg, cfg);
    ssh_setup_portfwd(ssh, cfg);

    if (ssh->cfg.ssh_rekey_time != cfg->ssh_rekey_time &&
	cfg->ssh_rekey_time != 0) {
	long new_next = ssh->last_rekey + cfg->ssh_rekey_time*60*TICKSPERSEC;
	long now = GETTICKCOUNT();

	if (new_next - now < 0) {
	    rekeying = "timeout shortened";
	} else {
	    ssh->next_rekey = schedule_timer(new_next - now, ssh2_timer, ssh);
	}
    }

    old_max_data_size = ssh->max_data_size;
    ssh->max_data_size = parse_blocksize(cfg->ssh_rekey_data);
    if (old_max_data_size != ssh->max_data_size &&
	ssh->max_data_size != 0) {
	if (ssh->outgoing_data_size > ssh->max_data_size ||
	    ssh->incoming_data_size > ssh->max_data_size)
	    rekeying = "data limit lowered";
    }

    if (ssh->cfg.compression != cfg->compression) {
	rekeying = "compression setting changed";
	rekey_mandatory = TRUE;
    }

    if (ssh->cfg.ssh2_des_cbc != cfg->ssh2_des_cbc ||
	memcmp(ssh->cfg.ssh_cipherlist, cfg->ssh_cipherlist,
	       sizeof(ssh->cfg.ssh_cipherlist))) {
	rekeying = "cipher settings changed";
	rekey_mandatory = TRUE;
    }

    ssh->cfg = *cfg;		       /* STRUCTURE COPY */

    if (rekeying) {
	if (!ssh->kex_in_progress) {
	    do_ssh2_transport(ssh, rekeying, -1, NULL);
	} else if (rekey_mandatory) {
	    ssh->deferred_rekey_reason = rekeying;
	}
    }
}

/*
 * Called to send data down the Telnet connection.
 */
static int ssh_send(void *handle, char *buf, int len)
{
    Ssh ssh = (Ssh) handle;

    if (ssh == NULL || ssh->s == NULL || ssh->protocol == NULL)
	return 0;

    ssh->protocol(ssh, (unsigned char *)buf, len, 0);

    return ssh_sendbuffer(ssh);
}

/*
 * Called to query the current amount of buffered stdin data.
 */
static int ssh_sendbuffer(void *handle)
{
    Ssh ssh = (Ssh) handle;
    int override_value;

    if (ssh == NULL || ssh->s == NULL || ssh->protocol == NULL)
	return 0;

    /*
     * If the SSH socket itself has backed up, add the total backup
     * size on that to any individual buffer on the stdin channel.
     */
    override_value = 0;
    if (ssh->throttled_all)
	override_value = ssh->overall_bufsize;

    if (ssh->version == 1) {
	return override_value;
    } else if (ssh->version == 2) {
	if (!ssh->mainchan || ssh->mainchan->closes > 0)
	    return override_value;
	else
	    return (override_value +
		    bufchain_size(&ssh->mainchan->v.v2.outbuffer));
    }

    return 0;
}

/*
 * Called to set the size of the window from SSH's POV.
 */
static void ssh_size(void *handle, int width, int height)
{
    Ssh ssh = (Ssh) handle;
    struct Packet *pktout;

    ssh->term_width = width;
    ssh->term_height = height;

    switch (ssh->state) {
      case SSH_STATE_BEFORE_SIZE:
      case SSH_STATE_PREPACKET:
      case SSH_STATE_CLOSED:
	break;			       /* do nothing */
      case SSH_STATE_INTERMED:
	ssh->size_needed = TRUE;       /* buffer for later */
	break;
      case SSH_STATE_SESSION:
	if (!ssh->cfg.nopty) {
	    if (ssh->version == 1) {
		send_packet(ssh, SSH1_CMSG_WINDOW_SIZE,
			    PKT_INT, ssh->term_height,
			    PKT_INT, ssh->term_width,
			    PKT_INT, 0, PKT_INT, 0, PKT_END);
	    } else if (ssh->mainchan) {
		pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_REQUEST);
		ssh2_pkt_adduint32(pktout, ssh->mainchan->remoteid);
		ssh2_pkt_addstring(pktout, "window-change");
		ssh2_pkt_addbool(pktout, 0);
		ssh2_pkt_adduint32(pktout, ssh->term_width);
		ssh2_pkt_adduint32(pktout, ssh->term_height);
		ssh2_pkt_adduint32(pktout, 0);
		ssh2_pkt_adduint32(pktout, 0);
		ssh2_pkt_send(ssh, pktout);
	    }
	}
	break;
    }
}

/*
 * Return a list of the special codes that make sense in this
 * protocol.
 */
static const struct telnet_special *ssh_get_specials(void *handle)
{
    static const struct telnet_special ssh1_ignore_special[] = {
	{"IGNORE message", TS_NOP}
    };
    static const struct telnet_special ssh2_transport_specials[] = {
	{"IGNORE message", TS_NOP},
	{"Repeat key exchange", TS_REKEY},
    };
    static const struct telnet_special ssh2_session_specials[] = {
	{NULL, TS_SEP},
	{"Break", TS_BRK},
	/* These are the signal names defined by draft-ietf-secsh-connect-23.
	 * They include all the ISO C signals, but are a subset of the POSIX
	 * required signals. */
	{"SIGINT (Interrupt)", TS_SIGINT},
	{"SIGTERM (Terminate)", TS_SIGTERM},
	{"SIGKILL (Kill)", TS_SIGKILL},
	{"SIGQUIT (Quit)", TS_SIGQUIT},
	{"SIGHUP (Hangup)", TS_SIGHUP},
	{"More signals", TS_SUBMENU},
	  {"SIGABRT", TS_SIGABRT}, {"SIGALRM", TS_SIGALRM},
	  {"SIGFPE",  TS_SIGFPE},  {"SIGILL",  TS_SIGILL},
	  {"SIGPIPE", TS_SIGPIPE}, {"SIGSEGV", TS_SIGSEGV},
	  {"SIGUSR1", TS_SIGUSR1}, {"SIGUSR2", TS_SIGUSR2},
	{NULL, TS_EXITMENU}
    };
    static const struct telnet_special specials_end[] = {
	{NULL, TS_EXITMENU}
    };
    /* XXX review this length for any changes: */
    static struct telnet_special ssh_specials[lenof(ssh2_transport_specials) +
					      lenof(ssh2_session_specials) +
					      lenof(specials_end)];
    Ssh ssh = (Ssh) handle;
    int i = 0;
#define ADD_SPECIALS(name) \
    do { \
	assert((i + lenof(name)) <= lenof(ssh_specials)); \
	memcpy(&ssh_specials[i], name, sizeof name); \
	i += lenof(name); \
    } while(0)

    if (ssh->version == 1) {
	/* Don't bother offering IGNORE if we've decided the remote
	 * won't cope with it, since we wouldn't bother sending it if
	 * asked anyway. */
	if (!(ssh->remote_bugs & BUG_CHOKES_ON_SSH1_IGNORE))
	    ADD_SPECIALS(ssh1_ignore_special);
    } else if (ssh->version == 2) {
	ADD_SPECIALS(ssh2_transport_specials);
	if (ssh->mainchan)
	    ADD_SPECIALS(ssh2_session_specials);
    } /* else we're not ready yet */

    if (i) {
	ADD_SPECIALS(specials_end);
	return ssh_specials;
    } else {
	return NULL;
    }
#undef ADD_SPECIALS
}

/*
 * Send Telnet special codes. TS_EOF is useful for `plink', so you
 * can send an EOF and collect resulting output (e.g. `plink
 * hostname sort').
 */
static void ssh_special(void *handle, Telnet_Special code)
{
    Ssh ssh = (Ssh) handle;
    struct Packet *pktout;

    if (code == TS_EOF) {
	if (ssh->state != SSH_STATE_SESSION) {
	    /*
	     * Buffer the EOF in case we are pre-SESSION, so we can
	     * send it as soon as we reach SESSION.
	     */
	    if (code == TS_EOF)
		ssh->eof_needed = TRUE;
	    return;
	}
	if (ssh->version == 1) {
	    send_packet(ssh, SSH1_CMSG_EOF, PKT_END);
	} else if (ssh->mainchan) {
	    struct Packet *pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_EOF);
	    ssh2_pkt_adduint32(pktout, ssh->mainchan->remoteid);
	    ssh2_pkt_send(ssh, pktout);
            ssh->send_ok = 0;          /* now stop trying to read from stdin */
	}
	logevent("Sent EOF message");
    } else if (code == TS_PING || code == TS_NOP) {
	if (ssh->state == SSH_STATE_CLOSED
	    || ssh->state == SSH_STATE_PREPACKET) return;
	if (ssh->version == 1) {
	    if (!(ssh->remote_bugs & BUG_CHOKES_ON_SSH1_IGNORE))
		send_packet(ssh, SSH1_MSG_IGNORE, PKT_STR, "", PKT_END);
	} else {
	    pktout = ssh2_pkt_init(SSH2_MSG_IGNORE);
	    ssh2_pkt_addstring_start(pktout);
	    ssh2_pkt_send_noqueue(ssh, pktout);
	}
    } else if (code == TS_REKEY) {
	if (!ssh->kex_in_progress && ssh->version == 2) {
	    do_ssh2_transport(ssh, "at user request", -1, NULL);
	}
    } else if (code == TS_BRK) {
	if (ssh->state == SSH_STATE_CLOSED
	    || ssh->state == SSH_STATE_PREPACKET) return;
	if (ssh->version == 1) {
	    logevent("Unable to send BREAK signal in SSH-1");
	} else if (ssh->mainchan) {
	    pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_REQUEST);
	    ssh2_pkt_adduint32(pktout, ssh->mainchan->remoteid);
	    ssh2_pkt_addstring(pktout, "break");
	    ssh2_pkt_addbool(pktout, 0);
	    ssh2_pkt_adduint32(pktout, 0);   /* default break length */
	    ssh2_pkt_send(ssh, pktout);
	}
    } else {
	/* Is is a POSIX signal? */
	char *signame = NULL;
	if (code == TS_SIGABRT) signame = "ABRT";
	if (code == TS_SIGALRM) signame = "ALRM";
	if (code == TS_SIGFPE)  signame = "FPE";
	if (code == TS_SIGHUP)  signame = "HUP";
	if (code == TS_SIGILL)  signame = "ILL";
	if (code == TS_SIGINT)  signame = "INT";
	if (code == TS_SIGKILL) signame = "KILL";
	if (code == TS_SIGPIPE) signame = "PIPE";
	if (code == TS_SIGQUIT) signame = "QUIT";
	if (code == TS_SIGSEGV) signame = "SEGV";
	if (code == TS_SIGTERM) signame = "TERM";
	if (code == TS_SIGUSR1) signame = "USR1";
	if (code == TS_SIGUSR2) signame = "USR2";
	/* The SSH-2 protocol does in principle support arbitrary named
	 * signals, including signame@domain, but we don't support those. */
	if (signame) {
	    /* It's a signal. */
	    if (ssh->version == 2 && ssh->mainchan) {
		pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_REQUEST);
		ssh2_pkt_adduint32(pktout, ssh->mainchan->remoteid);
		ssh2_pkt_addstring(pktout, "signal");
		ssh2_pkt_addbool(pktout, 0);
		ssh2_pkt_addstring(pktout, signame);
		ssh2_pkt_send(ssh, pktout);
		logeventf(ssh, "Sent signal SIG%s", signame);
	    }
	} else {
	    /* Never heard of it. Do nothing */
	}
    }
}

void *new_sock_channel(void *handle, Socket s)
{
    Ssh ssh = (Ssh) handle;
    struct ssh_channel *c;
    c = snew(struct ssh_channel);
    c->ssh = ssh;

    if (c) {
	c->halfopen = TRUE;
	c->localid = alloc_channel_id(ssh);
	c->closes = 0;
	c->type = CHAN_SOCKDATA_DORMANT;/* identify channel type */
	c->u.pfd.s = s;
	bufchain_init(&c->v.v2.outbuffer);
	add234(ssh->channels, c);
    }
    return c;
}

/*
 * This is called when stdout/stderr (the entity to which
 * from_backend sends data) manages to clear some backlog.
 */
static void ssh_unthrottle(void *handle, int bufsize)
{
    Ssh ssh = (Ssh) handle;
    if (ssh->version == 1) {
	if (ssh->v1_stdout_throttling && bufsize < SSH1_BUFFER_LIMIT) {
	    ssh->v1_stdout_throttling = 0;
	    ssh1_throttle(ssh, -1);
	}
    } else {
	if (ssh->mainchan && ssh->mainchan->closes == 0)
	    ssh2_set_window(ssh->mainchan, OUR_V2_WINSIZE - bufsize);
    }
}

void ssh_send_port_open(void *channel, char *hostname, int port, char *org)
{
    struct ssh_channel *c = (struct ssh_channel *)channel;
    Ssh ssh = c->ssh;
    struct Packet *pktout;

    logeventf(ssh, "Opening forwarded connection to %s:%d", hostname, port);

    if (ssh->version == 1) {
	send_packet(ssh, SSH1_MSG_PORT_OPEN,
		    PKT_INT, c->localid,
		    PKT_STR, hostname,
		    PKT_INT, port,
		    /* PKT_STR, <org:orgport>, */
		    PKT_END);
    } else {
	pktout = ssh2_pkt_init(SSH2_MSG_CHANNEL_OPEN);
	ssh2_pkt_addstring(pktout, "direct-tcpip");
	ssh2_pkt_adduint32(pktout, c->localid);
	c->v.v2.locwindow = OUR_V2_WINSIZE;
	ssh2_pkt_adduint32(pktout, c->v.v2.locwindow);/* our window size */
	ssh2_pkt_adduint32(pktout, OUR_V2_MAXPKT);      /* our max pkt size */
	ssh2_pkt_addstring(pktout, hostname);
	ssh2_pkt_adduint32(pktout, port);
	/*
	 * We make up values for the originator data; partly it's
	 * too much hassle to keep track, and partly I'm not
	 * convinced the server should be told details like that
	 * about my local network configuration.
	 */
	ssh2_pkt_addstring(pktout, "client-side-connection");
	ssh2_pkt_adduint32(pktout, 0);
	ssh2_pkt_send(ssh, pktout);
    }
}

static Socket ssh_socket(void *handle)
{
    Ssh ssh = (Ssh) handle;
    return ssh->s;
}

static int ssh_sendok(void *handle)
{
    Ssh ssh = (Ssh) handle;
    return ssh->send_ok;
}

static int ssh_ldisc(void *handle, int option)
{
    Ssh ssh = (Ssh) handle;
    if (option == LD_ECHO)
	return ssh->echoing;
    if (option == LD_EDIT)
	return ssh->editing;
    return FALSE;
}

static void ssh_provide_ldisc(void *handle, void *ldisc)
{
    Ssh ssh = (Ssh) handle;
    ssh->ldisc = ldisc;
}

static void ssh_provide_logctx(void *handle, void *logctx)
{
    Ssh ssh = (Ssh) handle;
    ssh->logctx = logctx;
}

static int ssh_return_exitcode(void *handle)
{
    Ssh ssh = (Ssh) handle;
    if (ssh->s != NULL)
        return -1;
    else
        return (ssh->exitcode >= 0 ? ssh->exitcode : 0);
}

/*
 * cfg_info for SSH is the currently running version of the
 * protocol. (1 for 1; 2 for 2; 0 for not-decided-yet.)
 */
static int ssh_cfg_info(void *handle)
{
    Ssh ssh = (Ssh) handle;
    return ssh->version;
}

/*
 * Gross hack: pscp will try to start SFTP but fall back to scp1 if
 * that fails. This variable is the means by which scp.c can reach
 * into the SSH code and find out which one it got.
 */
extern int ssh_fallback_cmd(void *handle)
{
    Ssh ssh = (Ssh) handle;
    return ssh->fallback_cmd;
}

Backend ssh_backend = {
    ssh_init,
    ssh_free,
    ssh_reconfig,
    ssh_send,
    ssh_sendbuffer,
    ssh_size,
    ssh_special,
    ssh_get_specials,
    ssh_socket,
    ssh_return_exitcode,
    ssh_sendok,
    ssh_ldisc,
    ssh_provide_ldisc,
    ssh_provide_logctx,
    ssh_unthrottle,
    ssh_cfg_info,
    22
};

