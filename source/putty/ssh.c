/*
 * SSH backend.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <limits.h>
#include <signal.h>

#include "putty.h"
#include "pageant.h" /* for AGENT_MAX_MSGLEN */
#include "tree234.h"
#include "storage.h"
#include "marshal.h"
#include "ssh.h"
#include "sshcr.h"
#include "sshbpp.h"
#include "sshchan.h"
#ifndef NO_GSSAPI
#include "sshgssc.h"
#include "sshgss.h"
#define MIN_CTXT_LIFETIME 5	/* Avoid rekey with short lifetime (seconds) */
#define GSS_KEX_CAPABLE	(1<<0)	/* Can do GSS KEX */
#define GSS_CRED_UPDATED (1<<1) /* Cred updated since previous delegation */
#define GSS_CTXT_EXPIRES (1<<2)	/* Context expires before next timer */
#define GSS_CTXT_MAYFAIL (1<<3)	/* Context may expire during handshake */
#endif

static const char *const ssh2_disconnect_reasons[] = {
    NULL,
    "host not allowed to connect",
    "protocol error",
    "key exchange failed",
    "host authentication failed",
    "MAC error",
    "compression error",
    "service not available",
    "protocol version not supported",
    "host key not verifiable",
    "connection lost",
    "by application",
    "too many connections",
    "auth cancelled by user",
    "no more auth methods available",
    "illegal user name",
};

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
#define BUG_SSH2_MAXPKT				256
#define BUG_CHOKES_ON_SSH2_IGNORE               512
#define BUG_CHOKES_ON_WINADJ                   1024
#define BUG_SENDS_LATE_REQUEST_REPLY           2048
#define BUG_SSH2_OLDGEX                        4096

#define DH_MIN_SIZE 1024
#define DH_MAX_SIZE 8192

/*
 * Codes for terminal modes.
 * Most of these are the same in SSH-1 and SSH-2.
 * This list is derived from RFC 4254 and
 * SSH-1 RFC-1.2.31.
 */
static const struct ssh_ttymode {
    const char* const mode;
    int opcode;
    enum { TTY_OP_CHAR, TTY_OP_BOOL } type;
} ssh_ttymodes[] = {
    /* "V" prefix discarded for special characters relative to SSH specs */
    { "INTR",	      1, TTY_OP_CHAR },
    { "QUIT",	      2, TTY_OP_CHAR },
    { "ERASE",	      3, TTY_OP_CHAR },
    { "KILL",	      4, TTY_OP_CHAR },
    { "EOF",	      5, TTY_OP_CHAR },
    { "EOL",	      6, TTY_OP_CHAR },
    { "EOL2",	      7, TTY_OP_CHAR },
    { "START",	      8, TTY_OP_CHAR },
    { "STOP",	      9, TTY_OP_CHAR },
    { "SUSP",	     10, TTY_OP_CHAR },
    { "DSUSP",	     11, TTY_OP_CHAR },
    { "REPRINT",     12, TTY_OP_CHAR },
    { "WERASE",	     13, TTY_OP_CHAR },
    { "LNEXT",	     14, TTY_OP_CHAR },
    { "FLUSH",	     15, TTY_OP_CHAR },
    { "SWTCH",	     16, TTY_OP_CHAR },
    { "STATUS",	     17, TTY_OP_CHAR },
    { "DISCARD",     18, TTY_OP_CHAR },
    { "IGNPAR",	     30, TTY_OP_BOOL },
    { "PARMRK",	     31, TTY_OP_BOOL },
    { "INPCK",	     32, TTY_OP_BOOL },
    { "ISTRIP",	     33, TTY_OP_BOOL },
    { "INLCR",	     34, TTY_OP_BOOL },
    { "IGNCR",	     35, TTY_OP_BOOL },
    { "ICRNL",	     36, TTY_OP_BOOL },
    { "IUCLC",	     37, TTY_OP_BOOL },
    { "IXON",	     38, TTY_OP_BOOL },
    { "IXANY",	     39, TTY_OP_BOOL },
    { "IXOFF",	     40, TTY_OP_BOOL },
    { "IMAXBEL",     41, TTY_OP_BOOL },
    { "IUTF8",       42, TTY_OP_BOOL },
    { "ISIG",	     50, TTY_OP_BOOL },
    { "ICANON",	     51, TTY_OP_BOOL },
    { "XCASE",	     52, TTY_OP_BOOL },
    { "ECHO",	     53, TTY_OP_BOOL },
    { "ECHOE",	     54, TTY_OP_BOOL },
    { "ECHOK",	     55, TTY_OP_BOOL },
    { "ECHONL",	     56, TTY_OP_BOOL },
    { "NOFLSH",	     57, TTY_OP_BOOL },
    { "TOSTOP",	     58, TTY_OP_BOOL },
    { "IEXTEN",	     59, TTY_OP_BOOL },
    { "ECHOCTL",     60, TTY_OP_BOOL },
    { "ECHOKE",	     61, TTY_OP_BOOL },
    { "PENDIN",	     62, TTY_OP_BOOL }, /* XXX is this a real mode? */
    { "OPOST",	     70, TTY_OP_BOOL },
    { "OLCUC",	     71, TTY_OP_BOOL },
    { "ONLCR",	     72, TTY_OP_BOOL },
    { "OCRNL",	     73, TTY_OP_BOOL },
    { "ONOCR",	     74, TTY_OP_BOOL },
    { "ONLRET",	     75, TTY_OP_BOOL },
    { "CS7",	     90, TTY_OP_BOOL },
    { "CS8",	     91, TTY_OP_BOOL },
    { "PARENB",	     92, TTY_OP_BOOL },
    { "PARODD",	     93, TTY_OP_BOOL }
};

/* Miscellaneous other tty-related constants. */
#define SSH_TTY_OP_END		  0
/* The opcodes for ISPEED/OSPEED differ between SSH-1 and SSH-2. */
#define SSH1_TTY_OP_ISPEED	192
#define SSH1_TTY_OP_OSPEED	193
#define SSH2_TTY_OP_ISPEED	128
#define SSH2_TTY_OP_OSPEED	129

/* Helper functions for parsing tty-related config. */
static unsigned int ssh_tty_parse_specchar(char *s)
{
    unsigned int ret;
    if (*s) {
	char *next = NULL;
	ret = ctrlparse(s, &next);
	if (!next) ret = s[0];
    } else {
	ret = 255; /* special value meaning "don't set" */
    }
    return ret;
}
static unsigned int ssh_tty_parse_boolean(char *s)
{
    if (stricmp(s, "yes") == 0 ||
	stricmp(s, "on") == 0 ||
	stricmp(s, "true") == 0 ||
	stricmp(s, "+") == 0)
	return 1; /* true */
    else if (stricmp(s, "no") == 0 ||
	     stricmp(s, "off") == 0 ||
	     stricmp(s, "false") == 0 ||
	     stricmp(s, "-") == 0)
	return 0; /* false */
    else
	return (atoi(s) != 0);
}

/* Safely convert rekey_time to unsigned long minutes */
static unsigned long rekey_mins(int rekey_time, unsigned long def)
{
    if (rekey_time < 0 || rekey_time > MAX_TICK_MINS)
        rekey_time = def;
    return (unsigned long)rekey_time;
}

#define translate(x) if (type == x) return #x
#define translatek(x,ctx) if (type == x && (pkt_kctx == ctx)) return #x
#define translatea(x,ctx) if (type == x && (pkt_actx == ctx)) return #x
const char *ssh1_pkt_type(int type)
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
const char *ssh2_pkt_type(Pkt_KCtx pkt_kctx, Pkt_ACtx pkt_actx, int type)
{
    translatea(SSH2_MSG_USERAUTH_GSSAPI_RESPONSE,SSH2_PKTCTX_GSSAPI);
    translatea(SSH2_MSG_USERAUTH_GSSAPI_TOKEN,SSH2_PKTCTX_GSSAPI);
    translatea(SSH2_MSG_USERAUTH_GSSAPI_EXCHANGE_COMPLETE,SSH2_PKTCTX_GSSAPI);
    translatea(SSH2_MSG_USERAUTH_GSSAPI_ERROR,SSH2_PKTCTX_GSSAPI);
    translatea(SSH2_MSG_USERAUTH_GSSAPI_ERRTOK,SSH2_PKTCTX_GSSAPI);
    translatea(SSH2_MSG_USERAUTH_GSSAPI_MIC, SSH2_PKTCTX_GSSAPI);
    translate(SSH2_MSG_DISCONNECT);
    translate(SSH2_MSG_IGNORE);
    translate(SSH2_MSG_UNIMPLEMENTED);
    translate(SSH2_MSG_DEBUG);
    translate(SSH2_MSG_SERVICE_REQUEST);
    translate(SSH2_MSG_SERVICE_ACCEPT);
    translate(SSH2_MSG_KEXINIT);
    translate(SSH2_MSG_NEWKEYS);
    translatek(SSH2_MSG_KEXDH_INIT, SSH2_PKTCTX_DHGROUP);
    translatek(SSH2_MSG_KEXDH_REPLY, SSH2_PKTCTX_DHGROUP);
    translatek(SSH2_MSG_KEX_DH_GEX_REQUEST_OLD, SSH2_PKTCTX_DHGEX);
    translatek(SSH2_MSG_KEX_DH_GEX_REQUEST, SSH2_PKTCTX_DHGEX);
    translatek(SSH2_MSG_KEX_DH_GEX_GROUP, SSH2_PKTCTX_DHGEX);
    translatek(SSH2_MSG_KEX_DH_GEX_INIT, SSH2_PKTCTX_DHGEX);
    translatek(SSH2_MSG_KEX_DH_GEX_REPLY, SSH2_PKTCTX_DHGEX);
    translatek(SSH2_MSG_KEXRSA_PUBKEY, SSH2_PKTCTX_RSAKEX);
    translatek(SSH2_MSG_KEXRSA_SECRET, SSH2_PKTCTX_RSAKEX);
    translatek(SSH2_MSG_KEXRSA_DONE, SSH2_PKTCTX_RSAKEX);
    translatek(SSH2_MSG_KEX_ECDH_INIT, SSH2_PKTCTX_ECDHKEX);
    translatek(SSH2_MSG_KEX_ECDH_REPLY, SSH2_PKTCTX_ECDHKEX);
    translatek(SSH2_MSG_KEXGSS_INIT, SSH2_PKTCTX_GSSKEX);
    translatek(SSH2_MSG_KEXGSS_CONTINUE, SSH2_PKTCTX_GSSKEX);
    translatek(SSH2_MSG_KEXGSS_COMPLETE, SSH2_PKTCTX_GSSKEX);
    translatek(SSH2_MSG_KEXGSS_HOSTKEY, SSH2_PKTCTX_GSSKEX);
    translatek(SSH2_MSG_KEXGSS_ERROR, SSH2_PKTCTX_GSSKEX);
    translatek(SSH2_MSG_KEXGSS_GROUPREQ, SSH2_PKTCTX_GSSKEX);
    translatek(SSH2_MSG_KEXGSS_GROUP, SSH2_PKTCTX_GSSKEX);
    translate(SSH2_MSG_USERAUTH_REQUEST);
    translate(SSH2_MSG_USERAUTH_FAILURE);
    translate(SSH2_MSG_USERAUTH_SUCCESS);
    translate(SSH2_MSG_USERAUTH_BANNER);
    translatea(SSH2_MSG_USERAUTH_PK_OK, SSH2_PKTCTX_PUBLICKEY);
    translatea(SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ, SSH2_PKTCTX_PASSWORD);
    translatea(SSH2_MSG_USERAUTH_INFO_REQUEST, SSH2_PKTCTX_KBDINTER);
    translatea(SSH2_MSG_USERAUTH_INFO_RESPONSE, SSH2_PKTCTX_KBDINTER);
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

/* Enumeration values for fields in SSH-1 packets */
enum {
    PKT_END, PKT_INT, PKT_CHAR, PKT_DATA, PKT_STR, PKT_BIGNUM,
};

static void ssh_pkt_ensure(struct PktOut *, int length);
static void ssh_pkt_adddata(struct PktOut *, const void *data, int len);
static void ssh2_pkt_send(Ssh, struct PktOut *);
static void do_ssh1_login(void *vctx);
static void do_ssh2_userauth(void *vctx);
static void ssh2_connection_setup(Ssh ssh);
static void do_ssh2_connection(void *vctx);
static void ssh_channel_init(struct ssh_channel *c);
static struct ssh_channel *ssh_channel_msg(Ssh ssh, PktIn *pktin);
static void ssh_channel_got_eof(struct ssh_channel *c);
static void ssh2_channel_check_close(struct ssh_channel *c);
static void ssh_channel_close_local(struct ssh_channel *c, char const *reason);
static void ssh_channel_destroy(struct ssh_channel *c);
static void ssh_channel_unthrottle(struct ssh_channel *c, int bufsize);
static void ssh2_msg_something_unimplemented(Ssh ssh, PktIn *pktin);
static void ssh2_general_packet_processing(Ssh ssh, PktIn *pktin);
static void ssh1_login_input(Ssh ssh);
static void ssh2_userauth_input(Ssh ssh);
static void ssh2_connection_input(Ssh ssh);

struct ssh_signkey_with_user_pref_id {
    const ssh_keyalg *alg;
    int id;
};
const static struct ssh_signkey_with_user_pref_id hostkey_algs[] = {
    { &ssh_ecdsa_ed25519, HK_ED25519 },
    { &ssh_ecdsa_nistp256, HK_ECDSA },
    { &ssh_ecdsa_nistp384, HK_ECDSA },
    { &ssh_ecdsa_nistp521, HK_ECDSA },
    { &ssh_dss, HK_DSA },
    { &ssh_rsa, HK_RSA },
};

const static struct ssh2_macalg *const macs[] = {
    &ssh_hmac_sha256, &ssh_hmac_sha1, &ssh_hmac_sha1_96, &ssh_hmac_md5
};
const static struct ssh2_macalg *const buggymacs[] = {
    &ssh_hmac_sha1_buggy, &ssh_hmac_sha1_96_buggy, &ssh_hmac_md5
};

static void *ssh_comp_none_init(void)
{
    return NULL;
}
static void ssh_comp_none_cleanup(void *handle)
{
}
static void ssh_comp_none_block(void *handle, unsigned char *block, int len,
                                unsigned char **outblock, int *outlen,
                                int minlen)
{
}
static int ssh_decomp_none_block(void *handle, unsigned char *block, int len,
                                 unsigned char **outblock, int *outlen)
{
    return 0;
}
const static struct ssh_compress ssh_comp_none = {
    "none", NULL,
    ssh_comp_none_init, ssh_comp_none_cleanup, ssh_comp_none_block,
    ssh_comp_none_init, ssh_comp_none_cleanup, ssh_decomp_none_block,
    NULL
};
extern const struct ssh_compress ssh_zlib;
const static struct ssh_compress *const compressions[] = {
    &ssh_zlib, &ssh_comp_none
};

typedef void (*handler_fn_t)(Ssh ssh, PktIn *pktin);
typedef void (*chandler_fn_t)(Ssh ssh, PktIn *pktin, void *ctx);
typedef void (*cchandler_fn_t)(struct ssh_channel *, PktIn *, void *);

/*
 * Each channel has a queue of outstanding CHANNEL_REQUESTS and their
 * handlers.
 */
struct outstanding_channel_request {
    cchandler_fn_t handler;
    void *ctx;
    struct outstanding_channel_request *next;
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
     *
     * In SSH-2, the four bits mean:
     *
     *   1   We have sent SSH2_MSG_CHANNEL_EOF.
     *   2   We have sent SSH2_MSG_CHANNEL_CLOSE.
     *   4   We have received SSH2_MSG_CHANNEL_EOF.
     *   8   We have received SSH2_MSG_CHANNEL_CLOSE.
     *
     * A channel is completely finished with when we have both sent
     * and received CLOSE.
     *
     * The symbolic constants below use the SSH-2 terminology, which
     * is a bit confusing in SSH-1, but we have to use _something_.
     */
#define CLOSES_SENT_EOF    1
#define CLOSES_SENT_CLOSE  2
#define CLOSES_RCVD_EOF    4
#define CLOSES_RCVD_CLOSE  8
    int closes;

    /*
     * This flag indicates that an EOF is pending on the outgoing side
     * of the channel: that is, wherever we're getting the data for
     * this channel has sent us some data followed by EOF. We can't
     * actually send the EOF until we've finished sending the data, so
     * we set this flag instead to remind us to do so once our buffer
     * is clear.
     */
    int pending_eof;

    /*
     * True if this channel is causing the underlying connection to be
     * throttled.
     */
    int throttling_conn;

    /*
     * True if we currently have backed-up data on the direction of
     * this channel pointing out of the SSH connection, and therefore
     * would prefer the 'Channel' implementation not to read further
     * local input if possible.
     */
    int throttled_by_backlog;

    union {
	struct ssh2_data_channel {
	    bufchain outbuffer;
	    unsigned remwindow, remmaxpkt;
	    /* locwindow is signed so we can cope with excess data. */
	    int locwindow, locmaxwin;
	    /*
	     * remlocwin is the amount of local window that we think
	     * the remote end had available to it after it sent the
	     * last data packet or window adjust ack.
	     */
	    int remlocwin;
	    /*
	     * These store the list of channel requests that haven't
	     * been acked.
	     */
	    struct outstanding_channel_request *chanreq_head, *chanreq_tail;
	    enum { THROTTLED, UNTHROTTLING, UNTHROTTLED } throttle_state;
	} v2;
    } v;

    ssh_sharing_connstate *sharectx; /* sharing context, if this is a
                                      * downstream channel */
    Channel *chan;      /* handle the client side of this channel, if not */
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
    char *shost, *dhost;
    char *sportdesc;
    ssh_sharing_connstate *share_ctx;
    struct ssh_portfwd *pfrec;
};

static void free_rportfwd(struct ssh_rportfwd *pf)
{
    if (pf) {
        sfree(pf->sportdesc);
        sfree(pf->shost);
        sfree(pf->dhost);
        sfree(pf);
    }
}

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
    struct PortListener *local;
};
#define free_portfwd(pf) ( \
    ((pf) ? (sfree((pf)->saddr), sfree((pf)->daddr), \
	     sfree((pf)->sserv), sfree((pf)->dserv)) : (void)0 ), sfree(pf) )

static void ssh1_protocol_setup(Ssh ssh);
static void ssh2_protocol_setup(Ssh ssh);
static void ssh2_bare_connection_protocol_setup(Ssh ssh);
static void ssh_size(Backend *be, int width, int height);
static void ssh_special(Backend *be, Telnet_Special);
static int ssh2_try_send(struct ssh_channel *c);
static int ssh_send_channel_data(struct ssh_channel *c,
				 const char *buf, int len);
static void ssh_throttle_all(Ssh ssh, int enable, int bufsize);
static void ssh2_set_window(struct ssh_channel *c, int newwin);
static int ssh_sendbuffer(Backend *be);
static int ssh_do_close(Ssh ssh, int notify_exit);
static void ssh2_timer(void *ctx, unsigned long now);
static int ssh2_timer_update(Ssh ssh, unsigned long rekey_time);
#ifndef NO_GSSAPI
static void ssh2_gss_update(Ssh ssh, int definitely_rekeying);
static PktOut *ssh2_gss_authpacket(Ssh ssh, Ssh_gss_ctx gss_ctx,
                                   const char *authtype);
#endif
static void ssh2_msg_unexpected(Ssh ssh, PktIn *pktin);

void pq_init(struct PacketQueue *pq)
{
    pq->end.next = pq->end.prev = &pq->end;
}

void pq_push(struct PacketQueue *pq, PktIn *pkt)
{
    PacketQueueNode *node = &pkt->qnode;
    assert(!node->next);
    assert(!node->prev);
    node->next = &pq->end;
    node->prev = pq->end.prev;
    node->next->prev = node;
    node->prev->next = node;
}

void pq_push_front(struct PacketQueue *pq, PktIn *pkt)
{
    PacketQueueNode *node = &pkt->qnode;
    assert(!node->next);
    assert(!node->prev);
    node->prev = &pq->end;
    node->next = pq->end.next;
    node->next->prev = node;
    node->prev->next = node;
}

PktIn *pq_peek(struct PacketQueue *pq)
{
    if (pq->end.next == &pq->end)
        return NULL;
    return FROMFIELD(pq->end.next, PktIn, qnode);
}

PktIn *pq_pop(struct PacketQueue *pq)
{
    PacketQueueNode *node = pq->end.next;
    if (node == &pq->end)
        return NULL;

    node->next->prev = node->prev;
    node->prev->next = node->next;
    node->prev = node->next = NULL;

    return FROMFIELD(node, PktIn, qnode);
}

void pq_clear(struct PacketQueue *pq)
{
    PktIn *pkt;
    while ((pkt = pq_pop(pq)) != NULL)
        ssh_unref_packet(pkt);
}

int pq_empty_on_to_front_of(struct PacketQueue *src, struct PacketQueue *dest)
{
    struct PacketQueueNode *srcfirst, *srclast;

    if (src->end.next == &src->end)
        return FALSE;

    srcfirst = src->end.next;
    srclast = src->end.prev;
    srcfirst->prev = &dest->end;
    srclast->next = dest->end.next;
    srcfirst->prev->next = srcfirst;
    srclast->next->prev = srclast;
    src->end.next = src->end.prev = &src->end;

    return TRUE;
}

struct queued_handler;
struct queued_handler {
    int msg1, msg2;
    chandler_fn_t handler;
    void *ctx;
    struct queued_handler *next;
};

/*
 * Enumeration of high-level classes of reason why we might need to do
 * a repeat key exchange. The full detailed reason in human-readable
 * form for the Event Log is kept in ssh->rekey_reason, but
 * ssh->rekey_class is a variable with this enum type which is used to
 * discriminate between classes of reason that the code needs to treat
 * differently.
 *
 * RK_NONE == 0 is the value indicating that no rekey is currently
 * needed at all. RK_INITIAL indicates that we haven't even done the
 * _first_ key exchange yet. RK_NORMAL is the usual case.
 * RK_GSS_UPDATE indicates that we're rekeying because we've just got
 * new GSSAPI credentials (hence there's no point in doing a
 * preliminary check for new GSS creds, because we already know the
 * answer); RK_POST_USERAUTH indicates that _if_ we're going to need a
 * post-userauth immediate rekey for any reason, this is the moment to
 * do it.
 *
 * So RK_POST_USERAUTH only tells the transport layer to _consider_
 * rekeying, not to definitely do it. Also, that one enum value is
 * special in that do_ssh2_transport fills in the reason text after it
 * decides whether it needs a rekey at all. In the other cases,
 * rekey_reason is set up at the same time as rekey_class.
 */
enum RekeyClass {
    RK_NONE = 0,
    RK_INITIAL,
    RK_NORMAL,
    RK_POST_USERAUTH,
    RK_GSS_UPDATE
};

struct ssh_tag {
    char *v_c, *v_s;
    ssh_hash *exhash;

    Socket s;

    const Plug_vtable *plugvt;
    Backend backend;

    Ldisc *ldisc;
    LogContext *logctx;

    unsigned char session_key[32];
    int v1_remote_protoflags;
    int v1_local_protoflags;
    int agentfwd_enabled;
    int X11_fwd_enabled;
    int remote_bugs;
    const struct ssh_kex *kex;
    const ssh_keyalg *hostkey_alg;
    char *hostkey_str; /* string representation, for easy checking in rekeys */
    unsigned char v2_session_id[SSH2_KEX_MAX_HASH_LEN];
    int v2_session_id_len;
    int v2_cbc_ignore_workaround;
    int v2_out_cipherblksize;
    void *kex_ctx;

    int bare_connection;
    int attempting_connshare;
    ssh_sharing_state *connshare;

    char *savedhost;
    int savedport;
    int send_ok;
    int echoing, editing;

    int session_started;
    Frontend *frontend;

    int ospeed, ispeed;		       /* temporaries */
    int term_width, term_height;

    tree234 *channels;		       /* indexed by local id */
    struct ssh_channel *mainchan;      /* primary session channel */
    int ncmode;			       /* is primary channel direct-tcpip? */
    int exitcode;
    int close_expected;
    int clean_exit;
    int disconnect_message_seen;

    tree234 *rportfwds, *portfwds;

    enum {
	SSH_STATE_PREPACKET,
	SSH_STATE_BEFORE_SIZE,
	SSH_STATE_INTERMED,
	SSH_STATE_SESSION,
	SSH_STATE_CLOSED
    } state;

    int size_needed, eof_needed;
    int sent_console_eof;
    int got_pty;           /* affects EOF behaviour on main channel */

    PktOut **queue;
    int queuelen, queuesize;
    int queueing;

    /*
     * Gross hack: pscp will try to start SFTP but fall back to
     * scp1 if that fails. This variable is the means by which
     * scp.c can reach into the SSH code and find out which one it
     * got.
     */
    int fallback_cmd;

    bufchain banner;	/* accumulates banners during do_ssh2_userauth */

    struct X11Display *x11disp;
    struct X11FakeAuth *x11auth;
    tree234 *x11authtree;

    int version;
    int conn_throttle_count;
    int overall_bufsize;
    int throttled_all;
    int v1_stdout_throttling;

    int do_ssh1_connection_crstate;

    void *do_ssh_init_state;
    void *do_ssh1_login_state;
    void *do_ssh2_transport_state;
    void *do_ssh2_userauth_state;
    void *do_ssh2_connection_state;
    void *do_ssh_connection_init_state;

    bufchain incoming_data;
    struct IdempotentCallback incoming_data_consumer;
    int incoming_data_seen_eof;
    char *incoming_data_eof_message;

    struct PacketQueue pq_full;
    struct IdempotentCallback pq_full_consumer;

    struct PacketQueue pq_ssh1_login;
    struct IdempotentCallback ssh1_login_icb;

    struct PacketQueue pq_ssh1_connection;
    struct IdempotentCallback ssh1_connection_icb;

    struct PacketQueue pq_ssh2_transport;
    struct IdempotentCallback ssh2_transport_icb;

    struct PacketQueue pq_ssh2_userauth;
    struct IdempotentCallback ssh2_userauth_icb;

    struct PacketQueue pq_ssh2_connection;
    struct IdempotentCallback ssh2_connection_icb;

    bufchain user_input;
    struct IdempotentCallback user_input_consumer;

    bufchain outgoing_data;
    struct IdempotentCallback outgoing_data_sender;

    const char *rekey_reason;
    enum RekeyClass rekey_class;

    PacketLogSettings pls;
    BinaryPacketProtocol *bpp;

    void (*general_packet_processing)(Ssh ssh, PktIn *pkt);
    void (*current_incoming_data_fn) (Ssh ssh);
    void (*current_user_input_fn) (Ssh ssh);

    /*
     * We maintain our own copy of a Conf structure here. That way,
     * when we're passed a new one for reconfiguration, we can check
     * the differences and potentially reconfigure port forwardings
     * etc in mid-session.
     */
    Conf *conf;

    /*
     * Dynamically allocated username string created during SSH
     * login. Stored in here rather than in the coroutine state so
     * that it'll be reliably freed if we shut down the SSH session
     * at some unexpected moment.
     */
    char *username;

    /*
     * Used to transfer data back from async callbacks.
     */
    void *agent_response;
    int agent_response_len;
    int user_response;

    /*
     * The SSH connection can be set as `frozen', meaning we are
     * not currently accepting incoming data from the network.
     */
    int frozen;

    /*
     * Dispatch table for packet types that we may have to deal
     * with at any time.
     */
    handler_fn_t packet_dispatch[SSH_MAX_MSG];

    /*
     * Queues of one-off handler functions for success/failure
     * indications from a request.
     */
    struct queued_handler *qhead, *qtail;
    handler_fn_t q_saved_handler1, q_saved_handler2;

    /*
     * This module deals with sending keepalives.
     */
    Pinger pinger;

    /*
     * Track incoming and outgoing data sizes and time, for
     * size-based rekeys.
     */
    unsigned long incoming_data_size, outgoing_data_size;
    unsigned long max_data_size;
    int kex_in_progress;
    unsigned long next_rekey, last_rekey;
    const char *deferred_rekey_reason;

    /*
     * Fully qualified host name, which we need if doing GSSAPI.
     */
    char *fullhostname;

#ifndef NO_GSSAPI
    /*
     * GSSAPI libraries for this session.  We need them at key exchange
     * and userauth time.
     *
     * And the gss_ctx we setup at initial key exchange will be used
     * during gssapi-keyex userauth time as well.
     */
    struct ssh_gss_liblist *gsslibs;
    struct ssh_gss_library *gsslib;
    int gss_status;
    time_t gss_cred_expiry;		/* Re-delegate if newer */
    unsigned long gss_ctxt_lifetime;	/* Re-delegate when short */
    Ssh_gss_name gss_srv_name;		/* Cached for KEXGSS */
    Ssh_gss_ctx gss_ctx;		/* Saved for gssapi-keyex */
    tree234 *transient_hostkey_cache;
#endif
    int gss_kex_used;  /* outside ifdef; always FALSE if NO_GSSAPI */

    /*
     * The last list returned from get_specials.
     */
    struct telnet_special *specials;

    /*
     * List of host key algorithms for which we _don't_ have a stored
     * host key. These are indices into the main hostkey_algs[] array
     */
    int uncert_hostkeys[lenof(hostkey_algs)];
    int n_uncert_hostkeys;

    /*
     * Flag indicating that the current rekey is intended to finish
     * with a newly cross-certified host key.
     */
    int cross_certifying;

    /*
     * Any asynchronous query to our SSH agent that we might have in
     * flight from the main authentication loop. (Queries from
     * agent-forwarding channels live in their channel structure.)
     */
    agent_pending_query *auth_agent_query;

    int need_random_unref;
};

static const char *ssh_pkt_type(Ssh ssh, int type)
{
    if (ssh->version == 1)
	return ssh1_pkt_type(type);
    else
	return ssh2_pkt_type(ssh->pls.kctx, ssh->pls.actx, type);
}

Frontend *ssh_get_frontend(Ssh ssh)
{
    return ssh->frontend;
}

#define logevent(s) logevent(ssh->frontend, s)

/* logevent, only printf-formatted. */
static void logeventf(Ssh ssh, const char *fmt, ...)
{
    va_list ap;
    char *buf;

    va_start(ap, fmt);
    buf = dupvprintf(fmt, ap);
    va_end(ap);
    logevent(buf);
    sfree(buf);
}

static void bomb_out(Ssh ssh, char *text)
{
    ssh_do_close(ssh, FALSE);
    logevent(text);
    connection_fatal(ssh->frontend, "%s", text);
    sfree(text);
}

#define bombout(msg) bomb_out(ssh, dupprintf msg)

/* Helper function for common bits of parsing ttymodes. */
static void parse_ttymodes(
    BinarySink *bs, Ssh ssh,
    void (*do_mode)(BinarySink *, const struct ssh_ttymode *, char *))
{
    int i;
    const struct ssh_ttymode *mode;
    char *val;

    for (i = 0; i < lenof(ssh_ttymodes); i++) {
        mode = ssh_ttymodes + i;
	/* Every mode known to the current version of the code should be
	 * mentioned; this was ensured when settings were loaded. */
        val = conf_get_str_str(ssh->conf, CONF_ttymodes, mode->mode);

	/*
	 * val[0] can be
	 *  - 'V', indicating that an explicit value follows it;
	 *  - 'A', indicating that we should pass the value through from
	 *    the local environment via get_ttymode; or
	 *  - 'N', indicating that we should explicitly not send this
	 *    mode.
	 */
	if (val[0] == 'A') {
	    val = get_ttymode(ssh->frontend, mode->mode);
	    if (val) {
		do_mode(bs, mode, val);
		sfree(val);
	    }
	} else if (val[0] == 'V') {
            do_mode(bs, mode, val + 1);              /* skip the 'V' */
	} /* else 'N', or something from the future we don't understand */
    }
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
    int i;
    if ( (i = strcmp(a->shost, b->shost)) != 0)
	return i < 0 ? -1 : +1;
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

static void c_write_stderr(int trusted, const void *vbuf, int len)
{
    const char *buf = (const char *)vbuf;
    int i;
    for (i = 0; i < len; i++)
	if (buf[i] != '\r' && (trusted || buf[i] == '\n' || (buf[i] & 0x60)))
	    fputc(buf[i], stderr);
}

static void c_write(Ssh ssh, const void *buf, int len)
{
    if (flags & FLAG_STDERR)
	c_write_stderr(1, buf, len);
    else
	from_backend(ssh->frontend, 1, buf, len);
}

static void c_write_untrusted(Ssh ssh, const void *buf, int len)
{
    if (flags & FLAG_STDERR)
	c_write_stderr(0, buf, len);
    else
	from_backend_untrusted(ssh->frontend, buf, len);
}

static void c_write_str(Ssh ssh, const char *buf)
{
    c_write(ssh, buf, strlen(buf));
}

void ssh_unref_packet(PktIn *pkt)
{
    if (--pkt->refcount <= 0)
        sfree(pkt);
}

void ssh_free_pktout(PktOut *pkt)
{
    sfree(pkt->data);
    sfree(pkt);
}

static void ssh_pkt_BinarySink_write(BinarySink *bs,
                                     const void *data, size_t len);
PktOut *ssh_new_packet(void)
{
    PktOut *pkt = snew(PktOut);

    BinarySink_INIT(pkt, ssh_pkt_BinarySink_write);
    pkt->data = NULL;
    pkt->length = 0;
    pkt->maxlen = 0;
    pkt->downstream_id = 0;
    pkt->additional_log_text = NULL;

    return pkt;
}

static int s_write(Ssh ssh, const void *data, int len)
{
    if (len && ssh->logctx)
	log_packet(ssh->logctx, PKT_OUTGOING, -1, NULL, data, len,
		   0, NULL, NULL, 0, NULL);
    if (!ssh->s)
        return 0;
    return sk_write(ssh->s, data, len);
}

static void ssh_pkt_write(Ssh ssh, PktOut *pkt)
{
    if (ssh->version == 2 && ssh->v2_cbc_ignore_workaround &&
        bufchain_size(&ssh->outgoing_data) != 0) {
        /*
         * When using a CBC-mode cipher in SSH-2, it's necessary to
         * ensure that an attacker can't provide data to be encrypted
         * using an IV that they know. We ensure this by prefixing
         * each packet that might contain user data with an
         * SSH_MSG_IGNORE.
         */
	PktOut *ipkt = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_IGNORE);
	put_stringz(ipkt, "");
        ssh_bpp_format_packet(ssh->bpp, ipkt);
    }

    ssh_bpp_format_packet(ssh->bpp, pkt);
    queue_idempotent_callback(&ssh->outgoing_data_sender);
}

static int ssh_versioncmp(const char *a, const char *b)
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
 * Packet construction functions. Mostly shared between SSH-1 and SSH-2.
 */
static void ssh_pkt_ensure(PktOut *pkt, int length)
{
    if (pkt->maxlen < length) {
	pkt->maxlen = length + 256;
	pkt->data = sresize(pkt->data, pkt->maxlen, unsigned char);
    }
}
static void ssh_pkt_adddata(PktOut *pkt, const void *data, int len)
{
    pkt->length += len;
    ssh_pkt_ensure(pkt, pkt->length);
    memcpy(pkt->data + pkt->length - len, data, len);
}

static void ssh_pkt_BinarySink_write(BinarySink *bs,
                                     const void *data, size_t len)
{
    PktOut *pkt = BinarySink_DOWNCAST(bs, PktOut);
    ssh_pkt_adddata(pkt, data, len);
}

/*
 * Queue an SSH-2 packet.
 */
static void ssh2_pkt_queue(Ssh ssh, PktOut *pkt)
{
    assert(ssh->queueing);

    if (ssh->queuelen >= ssh->queuesize) {
	ssh->queuesize = ssh->queuelen + 32;
	ssh->queue = sresize(ssh->queue, ssh->queuesize, PktOut *);
    }

    ssh->queue[ssh->queuelen++] = pkt;
}

/*
 * Either queue or send a packet, depending on whether queueing is
 * set.
 */
static void ssh2_pkt_send(Ssh ssh, PktOut *pkt)
{
    if (ssh->queueing) {
	ssh2_pkt_queue(ssh, pkt);
    } else {
	ssh_pkt_write(ssh, pkt);
    }
}

static void ssh_send_outgoing_data(void *ctx)
{
    Ssh ssh = (Ssh)ctx;

    while (bufchain_size(&ssh->outgoing_data) > 0) {
        void *data;
        int len, backlog;

        bufchain_prefix(&ssh->outgoing_data, &data, &len);
        backlog = s_write(ssh, data, len);
        bufchain_consume(&ssh->outgoing_data, len);

	ssh->outgoing_data_size += len;
        if (ssh->version == 2 && !ssh->kex_in_progress &&
	    !ssh->bare_connection && ssh->max_data_size != 0 &&
	    ssh->outgoing_data_size > ssh->max_data_size) {
            ssh->rekey_reason = "too much data sent";
            ssh->rekey_class = RK_NORMAL;
            queue_idempotent_callback(&ssh->ssh2_transport_icb);
        }

        if (backlog > SSH_MAX_BACKLOG) {
            ssh_throttle_all(ssh, 1, backlog);
            return;
        }
    }
}

/*
 * Send all queued SSH-2 packets.
 */
static void ssh2_pkt_queuesend(Ssh ssh)
{
    int i;

    assert(!ssh->queueing);

    for (i = 0; i < ssh->queuelen; i++)
	ssh_pkt_write(ssh, ssh->queue[i]);
    ssh->queuelen = 0;
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

/*
 * Helper function to add an SSH-2 signature blob to a packet. Expects
 * to be shown the public key blob as well as the signature blob.
 * Normally just appends the sig blob unmodified as a string, except
 * that it optionally breaks it open and fiddle with it to work around
 * BUG_SSH2_RSA_PADDING.
 */
static void ssh2_add_sigblob(Ssh ssh, PktOut *pkt,
			     const void *pkblob, int pkblob_len,
			     const void *sigblob, int sigblob_len)
{
    BinarySource pk[1], sig[1];
    BinarySource_BARE_INIT(pk, pkblob, pkblob_len);
    BinarySource_BARE_INIT(sig, sigblob, sigblob_len);

    /* dmemdump(pkblob, pkblob_len); */
    /* dmemdump(sigblob, sigblob_len); */

    /*
     * See if this is in fact an ssh-rsa signature and a buggy
     * server; otherwise we can just do this the easy way.
     */
    if ((ssh->remote_bugs & BUG_SSH2_RSA_PADDING) &&
        ptrlen_eq_string(get_string(pk), "ssh-rsa") &&
        ptrlen_eq_string(get_string(sig), "ssh-rsa")) {
	ptrlen mod_mp, sig_mp;
        size_t sig_prefix_len;

	/*
	 * Find the modulus and signature integers.
	 */
        get_string(pk);                /* skip over exponent */
        mod_mp = get_string(pk);       /* remember modulus */
        sig_prefix_len = sig->pos;
	sig_mp = get_string(sig);
        if (get_err(pk) || get_err(sig))
            goto give_up;

        /*
         * Find the byte length of the modulus, not counting leading
	 * zeroes.
         */
	while (mod_mp.len > 0 && *(const char *)mod_mp.ptr == 0) {
            mod_mp.len--;
            mod_mp.ptr = (const char *)mod_mp.ptr + 1;
        }

	/* debug(("modulus length is %d\n", len)); */
	/* debug(("signature length is %d\n", siglen)); */

	if (mod_mp.len != sig_mp.len) {
            strbuf *substr = strbuf_new();
	    put_data(substr, sigblob, sig_prefix_len);
	    put_uint32(substr, mod_mp.len);
	    put_padding(substr, mod_mp.len - sig_mp.len, 0);
	    put_data(substr, sig_mp.ptr, sig_mp.len);
            put_stringsb(pkt, substr);
	    return;
	}

	/* Otherwise fall through and do it the easy way. We also come
         * here as a fallback if we discover above that the key blob
         * is misformatted in some way. */
      give_up:;
    }

    put_string(pkt, sigblob, sigblob_len);
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

    /*
     * General notes on server version strings:
     *  - Not all servers reporting "Cisco-1.25" have all the bugs listed
     *    here -- in particular, we've heard of one that's perfectly happy
     *    with SSH1_MSG_IGNOREs -- but this string never seems to change,
     *    so we can't distinguish them.
     */
    if (conf_get_int(ssh->conf, CONF_sshbug_ignore1) == FORCE_ON ||
	(conf_get_int(ssh->conf, CONF_sshbug_ignore1) == AUTO &&
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

    if (conf_get_int(ssh->conf, CONF_sshbug_plainpw1) == FORCE_ON ||
	(conf_get_int(ssh->conf, CONF_sshbug_plainpw1) == AUTO &&
	 (!strcmp(imp, "Cisco-1.25") || !strcmp(imp, "OSU_1.4alpha3")))) {
	/*
	 * These versions need a plain password sent; they can't
	 * handle having a null and a random length of data after
	 * the password.
	 */
	ssh->remote_bugs |= BUG_NEEDS_SSH1_PLAIN_PASSWORD;
	logevent("We believe remote version needs a plain SSH-1 password");
    }

    if (conf_get_int(ssh->conf, CONF_sshbug_rsa1) == FORCE_ON ||
	(conf_get_int(ssh->conf, CONF_sshbug_rsa1) == AUTO &&
	 (!strcmp(imp, "Cisco-1.25")))) {
	/*
	 * These versions apparently have no clue whatever about
	 * RSA authentication and will panic and die if they see
	 * an AUTH_RSA message.
	 */
	ssh->remote_bugs |= BUG_CHOKES_ON_RSA;
	logevent("We believe remote version can't handle SSH-1 RSA authentication");
    }

    if (conf_get_int(ssh->conf, CONF_sshbug_hmac2) == FORCE_ON ||
	(conf_get_int(ssh->conf, CONF_sshbug_hmac2) == AUTO &&
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

    if (conf_get_int(ssh->conf, CONF_sshbug_derivekey2) == FORCE_ON ||
	(conf_get_int(ssh->conf, CONF_sshbug_derivekey2) == AUTO &&
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

    if (conf_get_int(ssh->conf, CONF_sshbug_rsapad2) == FORCE_ON ||
	(conf_get_int(ssh->conf, CONF_sshbug_rsapad2) == AUTO &&
	 (wc_match("OpenSSH_2.[5-9]*", imp) ||
	  wc_match("OpenSSH_3.[0-2]*", imp) ||
	  wc_match("mod_sftp/0.[0-8]*", imp) ||
	  wc_match("mod_sftp/0.9.[0-8]", imp)))) {
	/*
	 * These versions have the SSH-2 RSA padding bug.
	 */
	ssh->remote_bugs |= BUG_SSH2_RSA_PADDING;
	logevent("We believe remote version has SSH-2 RSA padding bug");
    }

    if (conf_get_int(ssh->conf, CONF_sshbug_pksessid2) == FORCE_ON ||
	(conf_get_int(ssh->conf, CONF_sshbug_pksessid2) == AUTO &&
	 wc_match("OpenSSH_2.[0-2]*", imp))) {
	/*
	 * These versions have the SSH-2 session-ID bug in
	 * public-key authentication.
	 */
	ssh->remote_bugs |= BUG_SSH2_PK_SESSIONID;
	logevent("We believe remote version has SSH-2 public-key-session-ID bug");
    }

    if (conf_get_int(ssh->conf, CONF_sshbug_rekey2) == FORCE_ON ||
	(conf_get_int(ssh->conf, CONF_sshbug_rekey2) == AUTO &&
	 (wc_match("DigiSSH_2.0", imp) ||
	  wc_match("OpenSSH_2.[0-4]*", imp) ||
	  wc_match("OpenSSH_2.5.[0-3]*", imp) ||
	  wc_match("Sun_SSH_1.0", imp) ||
	  wc_match("Sun_SSH_1.0.1", imp) ||
	  /* All versions <= 1.2.6 (they changed their format in 1.2.7) */
	  wc_match("WeOnlyDo-*", imp)))) {
	/*
	 * These versions have the SSH-2 rekey bug.
	 */
	ssh->remote_bugs |= BUG_SSH2_REKEY;
	logevent("We believe remote version has SSH-2 rekey bug");
    }

    if (conf_get_int(ssh->conf, CONF_sshbug_maxpkt2) == FORCE_ON ||
	(conf_get_int(ssh->conf, CONF_sshbug_maxpkt2) == AUTO &&
	 (wc_match("1.36_sshlib GlobalSCAPE", imp) ||
          wc_match("1.36 sshlib: GlobalScape", imp)))) {
	/*
	 * This version ignores our makpkt and needs to be throttled.
	 */
	ssh->remote_bugs |= BUG_SSH2_MAXPKT;
	logevent("We believe remote version ignores SSH-2 maximum packet size");
    }

    if (conf_get_int(ssh->conf, CONF_sshbug_ignore2) == FORCE_ON) {
	/*
	 * Servers that don't support SSH2_MSG_IGNORE. Currently,
	 * none detected automatically.
	 */
	ssh->remote_bugs |= BUG_CHOKES_ON_SSH2_IGNORE;
	logevent("We believe remote version has SSH-2 ignore bug");
    }

    if (conf_get_int(ssh->conf, CONF_sshbug_oldgex2) == FORCE_ON ||
	(conf_get_int(ssh->conf, CONF_sshbug_oldgex2) == AUTO &&
	 (wc_match("OpenSSH_2.[235]*", imp)))) {
	/*
	 * These versions only support the original (pre-RFC4419)
	 * SSH-2 GEX request, and disconnect with a protocol error if
	 * we use the newer version.
	 */
	ssh->remote_bugs |= BUG_SSH2_OLDGEX;
	logevent("We believe remote version has outdated SSH-2 GEX");
    }

    if (conf_get_int(ssh->conf, CONF_sshbug_winadj) == FORCE_ON) {
	/*
	 * Servers that don't support our winadj request for one
	 * reason or another. Currently, none detected automatically.
	 */
	ssh->remote_bugs |= BUG_CHOKES_ON_WINADJ;
	logevent("We believe remote version has winadj bug");
    }

    if (conf_get_int(ssh->conf, CONF_sshbug_chanreq) == FORCE_ON ||
	(conf_get_int(ssh->conf, CONF_sshbug_chanreq) == AUTO &&
	 (wc_match("OpenSSH_[2-5].*", imp) ||
	  wc_match("OpenSSH_6.[0-6]*", imp) ||
	  wc_match("dropbear_0.[2-4][0-9]*", imp) ||
	  wc_match("dropbear_0.5[01]*", imp)))) {
	/*
	 * These versions have the SSH-2 channel request bug.
	 * OpenSSH 6.7 and above do not:
	 * https://bugzilla.mindrot.org/show_bug.cgi?id=1818
	 * dropbear_0.52 and above do not:
	 * https://secure.ucc.asn.au/hg/dropbear/rev/cd02449b709c
	 */
	ssh->remote_bugs |= BUG_SENDS_LATE_REQUEST_REPLY;
	logevent("We believe remote version has SSH-2 channel request bug");
    }
}

/*
 * The `software version' part of an SSH version string is required
 * to contain no spaces or minus signs.
 */
static void ssh_fix_verstring(char *str)
{
    /* Eat "<protoversion>-". */
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

/*
 * Send an appropriate SSH version string.
 */
static void ssh_send_verstring(Ssh ssh, const char *protoname, char *svers)
{
    char *verstring;

    if (ssh->version == 2) {
	/*
	 * Construct a v2 version string.
	 */
	verstring = dupprintf("%s2.0-%s\015\012", protoname, sshver);
    } else {
	/*
	 * Construct a v1 version string.
	 */
        assert(!strcmp(protoname, "SSH-")); /* no v1 bare connection protocol */
	verstring = dupprintf("SSH-%s-%s\012",
			      (ssh_versioncmp(svers, "1.5") <= 0 ?
			       svers : "1.5"),
			      sshver);
    }

    ssh_fix_verstring(verstring + strlen(protoname));
#ifdef FUZZING
    /* FUZZING make PuTTY insecure, so make live use difficult. */
    verstring[0] = 'I';
#endif

    if (ssh->version == 2) {
	size_t len;
	/*
	 * Record our version string.
	 */
	len = strcspn(verstring, "\015\012");
	ssh->v_c = snewn(len + 1, char);
	memcpy(ssh->v_c, verstring, len);
	ssh->v_c[len] = 0;
    }

    logeventf(ssh, "We claim version: %.*s",
	      strcspn(verstring, "\015\012"), verstring);
    bufchain_add(&ssh->outgoing_data, verstring, strlen(verstring));
    queue_idempotent_callback(&ssh->outgoing_data_sender);
    sfree(verstring);
}

static void ssh_feed_to_bpp(Ssh ssh)
{
    PacketQueueNode *prev_tail = ssh->pq_full.end.prev;

    assert(ssh->bpp);
    ssh_bpp_handle_input(ssh->bpp);

    if (ssh->bpp->error) {
        bomb_out(ssh, ssh->bpp->error); /* also frees the error string */
        return;
    }

    if (ssh->bpp->seen_disconnect) {
        /*
         * If we've seen a DISCONNECT message, we should unset the
         * close_expected flag, because now we _do_ expect the server
         * to close the network connection afterwards. That way, the
         * more informative connection_fatal message for the
         * disconnect itself won't fight with 'Server unexpectedly
         * closed network connection'.
         */
        ssh->clean_exit = FALSE;
        ssh->close_expected = TRUE;
        ssh->disconnect_message_seen = TRUE;
    }

    if (ssh->pq_full.end.prev != prev_tail)
        queue_idempotent_callback(&ssh->pq_full_consumer);
}

static void do_ssh_init(Ssh ssh)
{
    static const char protoname[] = "SSH-";

    struct do_ssh_init_state {
	int crLine;
	int vslen;
	char *vstring;
	char *version;
	int vstrsize;
	int i;
	int proto1, proto2;
    };
    crState(do_ssh_init_state);
    
    crBeginState;

    /*
     * Search for a line beginning with the protocol name prefix in
     * the input.
     */
    s->i = 0;
    while (1) {
        char prefix[sizeof(protoname)-1];

        /*
         * Every time round this loop, we're at the start of a new
         * line, so look for the prefix.
         */
        crMaybeWaitUntilV(
            bufchain_size(&ssh->incoming_data) >= sizeof(prefix));
        bufchain_fetch(&ssh->incoming_data, prefix, sizeof(prefix));
        if (!memcmp(prefix, protoname, sizeof(prefix))) {
            bufchain_consume(&ssh->incoming_data, sizeof(prefix));
            break;
        }

        /*
         * If we didn't find it, consume data until we see a newline.
         */
        while (1) {
            int len;
            void *data;
            char *nl;

            crMaybeWaitUntilV(bufchain_size(&ssh->incoming_data) > 0);
            bufchain_prefix(&ssh->incoming_data, &data, &len);
            if ((nl = memchr(data, '\012', len)) != NULL) {
                bufchain_consume(&ssh->incoming_data, nl - (char *)data + 1);
                break;
            } else {
                bufchain_consume(&ssh->incoming_data, len);
            }
        }
    }

    ssh->session_started = TRUE;
    ssh->agentfwd_enabled = FALSE;

    /*
     * Now read the rest of the greeting line.
     */
    s->vstrsize = sizeof(protoname) + 16;
    s->vstring = snewn(s->vstrsize, char);
    strcpy(s->vstring, protoname);
    s->vslen = strlen(protoname);
    s->i = 0;
    do {
        int len;
        void *data;
        char *nl;

        crMaybeWaitUntilV(bufchain_size(&ssh->incoming_data) > 0);
        bufchain_prefix(&ssh->incoming_data, &data, &len);
        if ((nl = memchr(data, '\012', len)) != NULL) {
            len = nl - (char *)data + 1;
        }

	if (s->vslen + len >= s->vstrsize - 1) {
	    s->vstrsize = (s->vslen + len) * 5 / 4 + 32;
	    s->vstring = sresize(s->vstring, s->vstrsize, char);
	}

	memcpy(s->vstring + s->vslen, data, len);
        s->vslen += len;
        bufchain_consume(&ssh->incoming_data, len);

    } while (s->vstring[s->vslen-1] != '\012');

    s->vstring[s->vslen] = 0;
    s->vstring[strcspn(s->vstring, "\015\012")] = '\0';/* remove EOL chars */

    logeventf(ssh, "Server version: %s", s->vstring);
    ssh_detect_bugs(ssh, s->vstring);

    /*
     * Decide which SSH protocol version to support.
     */
    s->version = dupprintf(
        "%.*s", (int)strcspn(s->vstring + strlen(protoname), "-"),
        s->vstring + strlen(protoname));

    /* Anything strictly below "2.0" means protocol 1 is supported. */
    s->proto1 = ssh_versioncmp(s->version, "2.0") < 0;
    /* Anything greater or equal to "1.99" means protocol 2 is supported. */
    s->proto2 = ssh_versioncmp(s->version, "1.99") >= 0;

    if (conf_get_int(ssh->conf, CONF_sshprot) == 0) {
	if (!s->proto1) {
	    bombout(("SSH protocol version 1 required by our configuration "
		     "but not provided by server"));
	    crStopV;
	}
    } else if (conf_get_int(ssh->conf, CONF_sshprot) == 3) {
	if (!s->proto2) {
	    bombout(("SSH protocol version 2 required by our configuration "
		     "but server only provides (old, insecure) SSH-1"));
	    crStopV;
	}
    } else {
	/* No longer support values 1 or 2 for CONF_sshprot */
	assert(!"Unexpected value for CONF_sshprot");
    }

    if (s->proto2 && (conf_get_int(ssh->conf, CONF_sshprot) >= 2 || !s->proto1))
	ssh->version = 2;
    else
	ssh->version = 1;

    logeventf(ssh, "Using SSH protocol version %d", ssh->version);

    /* Send the version string, if we haven't already */
    if (conf_get_int(ssh->conf, CONF_sshprot) != 3)
	ssh_send_verstring(ssh, protoname, s->version);

    sfree(s->version);

    if (ssh->version == 2) {
	size_t len;
	/*
	 * Record their version string.
	 */
	len = strcspn(s->vstring, "\015\012");
	ssh->v_s = snewn(len + 1, char);
	memcpy(ssh->v_s, s->vstring, len);
	ssh->v_s[len] = 0;
	    
	/*
	 * Initialise SSH-2 protocol.
	 */
	ssh2_protocol_setup(ssh);
	ssh->general_packet_processing = ssh2_general_packet_processing;
	ssh->current_user_input_fn = NULL;
    } else {
	/*
	 * Initialise SSH-1 protocol.
	 */
	ssh1_protocol_setup(ssh);
	ssh->current_user_input_fn = ssh1_login_input;
    }
    ssh->bpp->out_raw = &ssh->outgoing_data;
    ssh->bpp->in_raw = &ssh->incoming_data;
    ssh->bpp->in_pq = &ssh->pq_full;
    ssh->bpp->pls = &ssh->pls;
    ssh->bpp->logctx = ssh->logctx;
    ssh->current_incoming_data_fn = ssh_feed_to_bpp;

    queue_idempotent_callback(&ssh->incoming_data_consumer);
    queue_idempotent_callback(&ssh->user_input_consumer);
    if (ssh->version == 2)
        queue_idempotent_callback(&ssh->ssh2_transport_icb);

    update_specials_menu(ssh->frontend);
    ssh->state = SSH_STATE_BEFORE_SIZE;
    ssh->pinger = pinger_new(ssh->conf, &ssh->backend);

    sfree(s->vstring);

    crFinishV;
}

static void do_ssh_connection_init(Ssh ssh)
{
    /*
     * Ordinary SSH begins with the banner "SSH-x.y-...". This is just
     * the ssh-connection part, extracted and given a trivial binary
     * packet protocol, so we replace 'SSH-' at the start with a new
     * name. In proper SSH style (though of course this part of the
     * proper SSH protocol _isn't_ subject to this kind of
     * DNS-domain-based extension), we define the new name in our
     * extension space.
     */
    static const char protoname[] =
        "SSHCONNECTION@putty.projects.tartarus.org-";

    struct do_ssh_connection_init_state {
	int crLine;
	int vslen;
	char *vstring;
	char *version;
	int vstrsize;
	int i;
    };
    crState(do_ssh_connection_init_state);
    
    crBeginState;

    /*
     * Search for a line beginning with the protocol name prefix in
     * the input.
     */
    s->i = 0;
    while (1) {
        char prefix[sizeof(protoname)-1];

        /*
         * Every time round this loop, we're at the start of a new
         * line, so look for the prefix.
         */
        crMaybeWaitUntilV(
            bufchain_size(&ssh->incoming_data) >= sizeof(prefix));
        bufchain_fetch(&ssh->incoming_data, prefix, sizeof(prefix));
        if (!memcmp(prefix, protoname, sizeof(prefix))) {
            bufchain_consume(&ssh->incoming_data, sizeof(prefix));
            break;
        }

        /*
         * If we didn't find it, consume data until we see a newline.
         */
        while (1) {
            int len;
            void *data;
            char *nl;

            crMaybeWaitUntilV(bufchain_size(&ssh->incoming_data) > 0);
            bufchain_prefix(&ssh->incoming_data, &data, &len);
            if ((nl = memchr(data, '\012', len)) != NULL) {
                bufchain_consume(&ssh->incoming_data, nl - (char *)data + 1);
                break;
            } else {
                bufchain_consume(&ssh->incoming_data, len);
            }
        }
    }

    /*
     * Now read the rest of the greeting line.
     */
    s->vstrsize = sizeof(protoname) + 16;
    s->vstring = snewn(s->vstrsize, char);
    strcpy(s->vstring, protoname);
    s->vslen = strlen(protoname);
    s->i = 0;
    do {
        int len;
        void *data;
        char *nl;

        crMaybeWaitUntilV(bufchain_size(&ssh->incoming_data) > 0);
        bufchain_prefix(&ssh->incoming_data, &data, &len);
        if ((nl = memchr(data, '\012', len)) != NULL) {
            len = nl - (char *)data + 1;
        }

	if (s->vslen + len >= s->vstrsize - 1) {
	    s->vstrsize = (s->vslen + len) * 5 / 4 + 32;
	    s->vstring = sresize(s->vstring, s->vstrsize, char);
	}

	memcpy(s->vstring + s->vslen, data, len);
        s->vslen += len;
        bufchain_consume(&ssh->incoming_data, len);

    } while (s->vstring[s->vslen-1] != '\012');

    s->vstring[s->vslen] = 0;
    s->vstring[strcspn(s->vstring, "\015\012")] = '\0';/* remove EOL chars */

    ssh->agentfwd_enabled = FALSE;

    logeventf(ssh, "Server version: %s", s->vstring);
    ssh_detect_bugs(ssh, s->vstring);

    /*
     * Decide which SSH protocol version to support. This is easy in
     * bare ssh-connection mode: only 2.0 is legal.
     */
    s->version = dupprintf(
        "%.*s", (int)strcspn(s->vstring + strlen(protoname), "-"),
        s->vstring + strlen(protoname));

    if (ssh_versioncmp(s->version, "2.0") < 0) {
	bombout(("Server announces compatibility with SSH-1 in bare ssh-connection protocol"));
        crStopV;
    }
    if (conf_get_int(ssh->conf, CONF_sshprot) == 0) {
	bombout(("Bare ssh-connection protocol cannot be run in SSH-1-only mode"));
	crStopV;
    }

    ssh->version = 2;

    logeventf(ssh, "Using bare ssh-connection protocol");

    /* Send the version string, if we haven't already */
    ssh_send_verstring(ssh, protoname, s->version);

    sfree(s->version);

    /*
     * Initialise bare connection protocol.
     */
    ssh2_bare_connection_protocol_setup(ssh);
    ssh->bpp->out_raw = &ssh->outgoing_data;
    ssh->bpp->in_raw = &ssh->incoming_data;
    ssh->bpp->in_pq = &ssh->pq_full;
    ssh->bpp->pls = &ssh->pls;
    ssh->bpp->logctx = ssh->logctx;
    ssh->current_incoming_data_fn = ssh_feed_to_bpp;
    queue_idempotent_callback(&ssh->incoming_data_consumer);
    ssh->current_user_input_fn = ssh2_connection_input;
    queue_idempotent_callback(&ssh->user_input_consumer);

    update_specials_menu(ssh->frontend);
    ssh->state = SSH_STATE_BEFORE_SIZE;
    ssh->pinger = pinger_new(ssh->conf, &ssh->backend);

    /*
     * Get connection protocol under way.
     */
    do_ssh2_connection(ssh);

    sfree(s->vstring);

    crFinishV;
}

static void ssh_set_frozen(Ssh ssh, int frozen)
{
    if (ssh->s)
	sk_set_frozen(ssh->s, frozen);
    ssh->frozen = frozen;
}

static void ssh_process_incoming_data(void *ctx)
{
    Ssh ssh = (Ssh)ctx;

    if (ssh->state == SSH_STATE_CLOSED)
        return;

    if (!ssh->frozen)
        ssh->current_incoming_data_fn(ssh);

    if (ssh->state == SSH_STATE_CLOSED) /* yes, check _again_ */
        return;

    if (ssh->incoming_data_seen_eof) {
        int need_notify = ssh_do_close(ssh, FALSE);
        const char *error_msg = ssh->incoming_data_eof_message;

        if (!error_msg) {
            if (!ssh->close_expected)
                error_msg = "Server unexpectedly closed network connection";
            else
                error_msg = "Server closed network connection";
        }

        if (ssh->close_expected && ssh->clean_exit && ssh->exitcode < 0)
            ssh->exitcode = 0;

        if (need_notify)
            notify_remote_exit(ssh->frontend);

        if (error_msg)
            logevent(error_msg);
        if ((!ssh->close_expected || !ssh->clean_exit) &&
            !ssh->disconnect_message_seen)
            connection_fatal(ssh->frontend, "%s", error_msg);
    }
}

static void ssh_process_pq_full(void *ctx)
{
    Ssh ssh = (Ssh)ctx;
    PktIn *pktin;

    while ((pktin = pq_pop(&ssh->pq_full)) != NULL) {
        if (ssh->general_packet_processing)
            ssh->general_packet_processing(ssh, pktin);
	ssh->packet_dispatch[pktin->type](ssh, pktin);
	ssh_unref_packet(pktin);
    }
}

static void ssh_process_user_input(void *ctx)
{
    Ssh ssh = (Ssh)ctx;
    if (ssh->current_user_input_fn)
        ssh->current_user_input_fn(ssh);
}

void chan_remotely_opened_confirmation(Channel *chan)
{
    assert(0 && "this channel type should never receive OPEN_CONFIRMATION");
}

void chan_remotely_opened_failure(Channel *chan, const char *errtext)
{
    assert(0 && "this channel type should never receive OPEN_FAILURE");
}

int chan_no_eager_close(Channel *chan, int sent_local_eof, int rcvd_remote_eof)
{
    return FALSE;     /* default: never proactively ask for a close */
}

/*
 * Trivial channel vtable for handling 'zombie channels' - those whose
 * local source of data has already been shut down or otherwise
 * stopped existing - so that we don't have to give them a null
 * 'Channel *' and special-case that all over the place.
 */

static void zombiechan_free(Channel *chan);
static int zombiechan_send(Channel *chan, int is_stderr, const void *, int);
static void zombiechan_set_input_wanted(Channel *chan, int wanted);
static void zombiechan_do_nothing(Channel *chan);
static void zombiechan_open_failure(Channel *chan, const char *);
static int zombiechan_want_close(Channel *chan, int sent_eof, int rcvd_eof);
static char *zombiechan_log_close_msg(Channel *chan) { return NULL; }

static const struct ChannelVtable zombiechan_channelvt = {
    zombiechan_free,
    zombiechan_do_nothing,             /* open_confirmation */
    zombiechan_open_failure,
    zombiechan_send,
    zombiechan_do_nothing,             /* send_eof */
    zombiechan_set_input_wanted,
    zombiechan_log_close_msg,
    zombiechan_want_close,
};

Channel *zombiechan_new(void)
{
    Channel *chan = snew(Channel);
    chan->vt = &zombiechan_channelvt;
    chan->initial_fixed_window_size = 0;
    return chan;
}

static void zombiechan_free(Channel *chan)
{
    assert(chan->vt == &zombiechan_channelvt);
    sfree(chan);
}

static void zombiechan_do_nothing(Channel *chan)
{
    assert(chan->vt == &zombiechan_channelvt);
}

static void zombiechan_open_failure(Channel *chan, const char *errtext)
{
    assert(chan->vt == &zombiechan_channelvt);
}

static int zombiechan_send(Channel *chan, int is_stderr,
                           const void *data, int length)
{
    assert(chan->vt == &zombiechan_channelvt);
    return 0;
}

static void zombiechan_set_input_wanted(Channel *chan, int enable)
{
    assert(chan->vt == &zombiechan_channelvt);
}

static int zombiechan_want_close(Channel *chan, int sent_eof, int rcvd_eof)
{
    return TRUE;
}

static int ssh_do_close(Ssh ssh, int notify_exit)
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
	    ssh_channel_close_local(c, NULL);
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
		pfl_terminate(pf->local);
	    del234(ssh->portfwds, pf); /* moving next one to index 0 */
	    free_portfwd(pf);
	}
	freetree234(ssh->portfwds);
	ssh->portfwds = NULL;
    }

    /*
     * Also stop attempting to connection-share.
     */
    if (ssh->connshare) {
        sharestate_free(ssh->connshare);
        ssh->connshare = NULL;
    }

    return ret;
}

static void ssh_socket_log(Plug plug, int type, SockAddr addr, int port,
                           const char *error_msg, int error_code)
{
    Ssh ssh = FROMFIELD(plug, struct ssh_tag, plugvt);

    /*
     * While we're attempting connection sharing, don't loudly log
     * everything that happens. Real TCP connections need to be logged
     * when we _start_ trying to connect, because it might be ages
     * before they respond if something goes wrong; but connection
     * sharing is local and quick to respond, and it's sufficient to
     * simply wait and see whether it worked afterwards.
     */

    if (!ssh->attempting_connshare)
        backend_socket_log(ssh->frontend, type, addr, port,
                           error_msg, error_code, ssh->conf,
                           ssh->session_started);
}

void ssh_connshare_log(Ssh ssh, int event, const char *logtext,
                       const char *ds_err, const char *us_err)
{
    if (event == SHARE_NONE) {
        /* In this case, 'logtext' is an error message indicating a
         * reason why connection sharing couldn't be set up _at all_.
         * Failing that, ds_err and us_err indicate why we couldn't be
         * a downstream and an upstream respectively. */
        if (logtext) {
            logeventf(ssh, "Could not set up connection sharing: %s", logtext);
        } else {
            if (ds_err)
                logeventf(ssh, "Could not set up connection sharing"
                          " as downstream: %s", ds_err);
            if (us_err)
                logeventf(ssh, "Could not set up connection sharing"
                          " as upstream: %s", us_err);
        }
    } else if (event == SHARE_DOWNSTREAM) {
        /* In this case, 'logtext' is a local endpoint address */
        logeventf(ssh, "Using existing shared connection at %s", logtext);
        /* Also we should mention this in the console window to avoid
         * confusing users as to why this window doesn't behave the
         * usual way. */
        if ((flags & FLAG_VERBOSE) || (flags & FLAG_INTERACTIVE)) {
            c_write_str(ssh,"Reusing a shared connection to this server.\r\n");
        }
    } else if (event == SHARE_UPSTREAM) {
        /* In this case, 'logtext' is a local endpoint address too */
        logeventf(ssh, "Sharing this connection at %s", logtext);
    }
}

static void ssh_closing(Plug plug, const char *error_msg, int error_code,
			int calling_back)
{
    Ssh ssh = FROMFIELD(plug, struct ssh_tag, plugvt);
    ssh->incoming_data_seen_eof = TRUE;
    ssh->incoming_data_eof_message = dupstr(error_msg);
    queue_idempotent_callback(&ssh->incoming_data_consumer);
}

static void ssh_receive(Plug plug, int urgent, char *data, int len)
{
    Ssh ssh = FROMFIELD(plug, struct ssh_tag, plugvt);

    /* Log raw data, if we're in that mode. */
    if (ssh->logctx)
	log_packet(ssh->logctx, PKT_INCOMING, -1, NULL, data, len,
		   0, NULL, NULL, 0, NULL);

    bufchain_add(&ssh->incoming_data, data, len);
    queue_idempotent_callback(&ssh->incoming_data_consumer);

    if (ssh->state == SSH_STATE_CLOSED) {
	ssh_do_close(ssh, TRUE);
    }
}

static void ssh_sent(Plug plug, int bufsize)
{
    Ssh ssh = FROMFIELD(plug, struct ssh_tag, plugvt);
    /*
     * If the send backlog on the SSH socket itself clears, we should
     * unthrottle the whole world if it was throttled, and also resume
     * sending our bufchain of queued wire data.
     */
    if (bufsize < SSH_MAX_BACKLOG) {
	ssh_throttle_all(ssh, 0, bufsize);
        queue_idempotent_callback(&ssh->outgoing_data_sender);
    }
}

static void ssh_hostport_setup(const char *host, int port, Conf *conf,
                               char **savedhost, int *savedport,
                               char **loghost_ret)
{
    char *loghost = conf_get_str(conf, CONF_loghost);
    if (loghost_ret)
        *loghost_ret = loghost;

    if (*loghost) {
	char *tmphost;
        char *colon;

        tmphost = dupstr(loghost);
	*savedport = 22;	       /* default ssh port */

	/*
	 * A colon suffix on the hostname string also lets us affect
	 * savedport. (Unless there are multiple colons, in which case
	 * we assume this is an unbracketed IPv6 literal.)
	 */
	colon = host_strrchr(tmphost, ':');
	if (colon && colon == host_strchr(tmphost, ':')) {
	    *colon++ = '\0';
	    if (*colon)
		*savedport = atoi(colon);
	}

        *savedhost = host_strduptrim(tmphost);
        sfree(tmphost);
    } else {
	*savedhost = host_strduptrim(host);
	if (port < 0)
	    port = 22;		       /* default ssh port */
	*savedport = port;
    }
}

static int ssh_test_for_upstream(const char *host, int port, Conf *conf)
{
    char *savedhost;
    int savedport;
    int ret;

    random_ref(); /* platform may need this to determine share socket name */
    ssh_hostport_setup(host, port, conf, &savedhost, &savedport, NULL);
    ret = ssh_share_test_for_upstream(savedhost, savedport, conf);
    sfree(savedhost);
    random_unref();

    return ret;
}

static const Plug_vtable Ssh_plugvt = {
    ssh_socket_log,
    ssh_closing,
    ssh_receive,
    ssh_sent,
    NULL
};

/*
 * Connect to specified host and port.
 * Returns an error message, or NULL on success.
 * Also places the canonical host name into `realhost'. It must be
 * freed by the caller.
 */
static const char *connect_to_host(Ssh ssh, const char *host, int port,
				   char **realhost, int nodelay, int keepalive)
{
    SockAddr addr;
    const char *err;
    char *loghost;
    int addressfamily, sshprot;

    ssh_hostport_setup(host, port, ssh->conf,
                       &ssh->savedhost, &ssh->savedport, &loghost);

    ssh->plugvt = &Ssh_plugvt;

    /*
     * Try connection-sharing, in case that means we don't open a
     * socket after all. ssh_connection_sharing_init will connect to a
     * previously established upstream if it can, and failing that,
     * establish a listening socket for _us_ to be the upstream. In
     * the latter case it will return NULL just as if it had done
     * nothing, because here we only need to care if we're a
     * downstream and need to do our connection setup differently.
     */
    ssh->connshare = NULL;
    ssh->attempting_connshare = TRUE;  /* affects socket logging behaviour */
    ssh->s = ssh_connection_sharing_init(
        ssh->savedhost, ssh->savedport, ssh->conf, ssh, &ssh->plugvt,
        &ssh->connshare);
    ssh->attempting_connshare = FALSE;
    if (ssh->s != NULL) {
        /*
         * We are a downstream.
         */
        ssh->bare_connection = TRUE;
        ssh->current_incoming_data_fn = do_ssh_connection_init;
        ssh->fullhostname = NULL;
        *realhost = dupstr(host);      /* best we can do */
    } else {
        /*
         * We're not a downstream, so open a normal socket.
         */
        ssh->current_incoming_data_fn = do_ssh_init;

        /*
         * Try to find host.
         */
        addressfamily = conf_get_int(ssh->conf, CONF_addressfamily);
        addr = name_lookup(host, port, realhost, ssh->conf, addressfamily,
                           ssh->frontend, "SSH connection");
        if ((err = sk_addr_error(addr)) != NULL) {
            sk_addr_free(addr);
            return err;
        }
        ssh->fullhostname = dupstr(*realhost);   /* save in case of GSSAPI */

        ssh->s = new_connection(addr, *realhost, port,
                                0, 1, nodelay, keepalive,
                                &ssh->plugvt, ssh->conf);
        if ((err = sk_socket_error(ssh->s)) != NULL) {
            ssh->s = NULL;
            notify_remote_exit(ssh->frontend);
            return err;
        }
    }

    /*
     * The SSH version number is always fixed (since we no longer support
     * fallback between versions), so set it now, and if it's SSH-2,
     * send the version string now too.
     */
    sshprot = conf_get_int(ssh->conf, CONF_sshprot);
    assert(sshprot == 0 || sshprot == 3);
    if (sshprot == 0)
	/* SSH-1 only */
	ssh->version = 1;
    if (sshprot == 3 && !ssh->bare_connection) {
	/* SSH-2 only */
	ssh->version = 2;
	ssh_send_verstring(ssh, "SSH-", NULL);
    }

    /*
     * loghost, if configured, overrides realhost.
     */
    if (*loghost) {
	sfree(*realhost);
	*realhost = dupstr(loghost);
    }

    return NULL;
}

/*
 * Throttle or unthrottle the SSH connection.
 */
static void ssh_throttle_conn(Ssh ssh, int adjust)
{
    int old_count = ssh->conn_throttle_count;
    ssh->conn_throttle_count += adjust;
    assert(ssh->conn_throttle_count >= 0);
    if (ssh->conn_throttle_count && !old_count) {
	ssh_set_frozen(ssh, 1);
    } else if (!ssh->conn_throttle_count && old_count) {
	ssh_set_frozen(ssh, 0);
    }
}

static void ssh_channel_check_throttle(struct ssh_channel *c)
{
    /*
     * We don't want this channel to read further input if this
     * particular channel has a backed-up SSH window, or if the
     * outgoing side of the whole SSH connection is currently
     * throttled, or if this channel already has an outgoing EOF
     * either sent or pending.
     */
    chan_set_input_wanted(c->chan,
                          !c->throttled_by_backlog &&
                          !c->ssh->throttled_all &&
                          !c->pending_eof &&
                          !(c->closes & CLOSES_SENT_EOF));
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
    for (i = 0; NULL != (c = index234(ssh->channels, i)); i++)
        ssh_channel_check_throttle(c);
}

static void ssh_agent_callback(void *sshv, void *reply, int replylen)
{
    Ssh ssh = (Ssh) sshv;

    ssh->auth_agent_query = NULL;

    ssh->agent_response = reply;
    ssh->agent_response_len = replylen;

    if (ssh->version == 1)
	do_ssh1_login(ssh);
    else
	do_ssh2_userauth(ssh);
}

static void ssh_dialog_callback(void *sshv, int ret)
{
    Ssh ssh = (Ssh) sshv;

    ssh->user_response = ret;

    if (ssh->version == 1)
	do_ssh1_login(ssh);
    else
        queue_idempotent_callback(&ssh->ssh2_transport_icb);

    /*
     * This may have unfrozen the SSH connection.
     */
    if (!ssh->frozen)
        queue_idempotent_callback(&ssh->incoming_data_consumer);
}

/*
 * Client-initiated disconnection. Send a DISCONNECT if `wire_reason'
 * non-NULL, otherwise just close the connection. `client_reason' == NULL
 * => log `wire_reason'.
 */
static void ssh_disconnect(Ssh ssh, const char *client_reason,
                           const char *wire_reason,
			   int code, int clean_exit)
{
    char *error;
    if (!client_reason)
	client_reason = wire_reason;
    if (client_reason)
	error = dupprintf("Disconnected: %s", client_reason);
    else
	error = dupstr("Disconnected");
    if (wire_reason) {
	if (ssh->version == 1) {
	    PktOut *pktout = ssh_bpp_new_pktout(ssh->bpp, SSH1_MSG_DISCONNECT);
            put_stringz(pktout, wire_reason);
            ssh_pkt_write(ssh, pktout);
	} else if (ssh->version == 2) {
	    PktOut *pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_DISCONNECT);
	    put_uint32(pktout, code);
	    put_stringz(pktout, wire_reason);
	    put_stringz(pktout, "en");	/* language tag */
	    ssh_pkt_write(ssh, pktout);
	}
    }
    ssh->close_expected = TRUE;
    ssh->clean_exit = clean_exit;
    ssh_closing(&ssh->plugvt, error, 0, 0);
    sfree(error);
}

int verify_ssh_manual_host_key(Ssh ssh, const char *fingerprint, ssh_key *key)
{
    if (!conf_get_str_nthstrkey(ssh->conf, CONF_ssh_manual_hostkeys, 0)) {
        return -1;                     /* no manual keys configured */
    }

    if (fingerprint) {
        /*
         * The fingerprint string we've been given will have things
         * like 'ssh-rsa 2048' at the front of it. Strip those off and
         * narrow down to just the colon-separated hex block at the
         * end of the string.
         */
        const char *p = strrchr(fingerprint, ' ');
        fingerprint = p ? p+1 : fingerprint;
        /* Quick sanity checks, including making sure it's in lowercase */
        assert(strlen(fingerprint) == 16*3 - 1);
        assert(fingerprint[2] == ':');
        assert(fingerprint[strspn(fingerprint, "0123456789abcdef:")] == 0);

        if (conf_get_str_str_opt(ssh->conf, CONF_ssh_manual_hostkeys,
                                 fingerprint))
            return 1;                  /* success */
    }

    if (key) {
        /*
         * Construct the base64-encoded public key blob and see if
         * that's listed.
         */
        strbuf *binblob;
        char *base64blob;
        int atoms, i;
        binblob = strbuf_new();
        ssh_key_public_blob(key, BinarySink_UPCAST(binblob));
        atoms = (binblob->len + 2) / 3;
        base64blob = snewn(atoms * 4 + 1, char);
        for (i = 0; i < atoms; i++)
            base64_encode_atom(binblob->u + 3*i,
                               binblob->len - 3*i, base64blob + 4*i);
        base64blob[atoms * 4] = '\0';
        strbuf_free(binblob);
        if (conf_get_str_str_opt(ssh->conf, CONF_ssh_manual_hostkeys,
                                 base64blob)) {
            sfree(base64blob);
            return 1;                  /* success */
        }
        sfree(base64blob);
    }

    return 0;
}

static void ssh1_coro_wrapper_initial(Ssh ssh, PktIn *pktin);
static void ssh1_coro_wrapper_session(Ssh ssh, PktIn *pktin);
static void ssh1_connection_input(Ssh ssh);

/*
 * Handle the key exchange and user authentication phases.
 */
static void do_ssh1_login(void *vctx)
{
    Ssh ssh = (Ssh)vctx;
    PktIn *pktin;
    PktOut *pkt;

    int i, j, ret;
    ptrlen pl;
    struct MD5Context md5c;
    struct do_ssh1_login_state {
	int crLine;
	int len;
	unsigned char *rsabuf;
	unsigned long supported_ciphers_mask, supported_auths_mask;
	int tried_publickey, tried_agent;
	int tis_auth_refused, ccard_auth_refused;
        unsigned char cookie[8];
	unsigned char session_id[16];
	int cipher_type;
	strbuf *publickey_blob;
	char *publickey_comment;
	int privatekey_available, privatekey_encrypted;
	prompts_t *cur_prompt;
        int userpass_ret;
	char c;
	int pwpkt_type;
        unsigned char *agent_response;
        BinarySource asrc[1];          /* response from SSH agent */
	int keyi, nkeys;
	int authed;
	struct RSAKey key;
	Bignum challenge;
        ptrlen comment;
        int dlgret;
	Filename *keyfile;
        struct RSAKey servkey, hostkey;
    };
    crState(do_ssh1_login_state);

    crBeginState;

    crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh1_login)) != NULL);

    if (pktin->type != SSH1_SMSG_PUBLIC_KEY) {
	bombout(("Public key packet not received"));
	crStopV;
    }

    logevent("Received public keys");

    pl = get_data(pktin, 8);
    memcpy(s->cookie, pl.ptr, pl.len);

    get_rsa_ssh1_pub(pktin, &s->servkey, RSA_SSH1_EXPONENT_FIRST);
    get_rsa_ssh1_pub(pktin, &s->hostkey, RSA_SSH1_EXPONENT_FIRST);

    s->hostkey.comment = NULL; /* avoid confusing rsa_ssh1_fingerprint */

    /*
     * Log the host key fingerprint.
     */
    if (!get_err(pktin)) {
	char *fingerprint = rsa_ssh1_fingerprint(&s->hostkey);
	logevent("Host key fingerprint is:");
        logeventf(ssh, "      %s", fingerprint);
        sfree(fingerprint);
    }

    ssh->v1_remote_protoflags = get_uint32(pktin);
    s->supported_ciphers_mask = get_uint32(pktin);
    s->supported_auths_mask = get_uint32(pktin);

    if (get_err(pktin)) {
        bombout(("Bad SSH-1 public key packet"));
	crStopV;
    }

    if ((ssh->remote_bugs & BUG_CHOKES_ON_RSA))
	s->supported_auths_mask &= ~(1 << SSH1_AUTH_RSA);

    ssh->v1_local_protoflags =
	ssh->v1_remote_protoflags & SSH1_PROTOFLAGS_SUPPORTED;
    ssh->v1_local_protoflags |= SSH1_PROTOFLAG_SCREEN_NUMBER;

    MD5Init(&md5c);
    {
        int i;
        for (i = (bignum_bitcount(s->hostkey.modulus) + 7) / 8; i-- ;)
            put_byte(&md5c, bignum_byte(s->hostkey.modulus, i));
        for (i = (bignum_bitcount(s->servkey.modulus) + 7) / 8; i-- ;)
            put_byte(&md5c, bignum_byte(s->servkey.modulus, i));
    }
    put_data(&md5c, s->cookie, 8);
    MD5Final(s->session_id, &md5c);

    for (i = 0; i < 32; i++)
	ssh->session_key[i] = random_byte();

    /*
     * Verify that the `bits' and `bytes' parameters match.
     */
    if (s->hostkey.bits > s->hostkey.bytes * 8 ||
	s->servkey.bits > s->servkey.bytes * 8) {
	bombout(("SSH-1 public keys were badly formatted"));
	crStopV;
    }

    s->len = (s->hostkey.bytes > s->servkey.bytes ?
              s->hostkey.bytes : s->servkey.bytes);

    s->rsabuf = snewn(s->len, unsigned char);

    /*
     * Verify the host key.
     */
    {
	/*
	 * First format the key into a string.
	 */
	int len = rsastr_len(&s->hostkey);
	char *fingerprint;
	char *keystr = snewn(len, char);
	rsastr_fmt(keystr, &s->hostkey);
	fingerprint = rsa_ssh1_fingerprint(&s->hostkey);

        /* First check against manually configured host keys. */
        s->dlgret = verify_ssh_manual_host_key(ssh, fingerprint, NULL);
        sfree(fingerprint);
        if (s->dlgret == 0) {          /* did not match */
            bombout(("Host key did not appear in manually configured list"));
            sfree(keystr);
            crStopV;
        } else if (s->dlgret < 0) { /* none configured; use standard handling */
            ssh_set_frozen(ssh, 1);
            s->dlgret = verify_ssh_host_key(ssh->frontend,
                                            ssh->savedhost, ssh->savedport,
                                            "rsa", keystr, fingerprint,
                                            ssh_dialog_callback, ssh);
            sfree(keystr);
#ifdef FUZZING
	    s->dlgret = 1;
#endif
            if (s->dlgret < 0) {
                ssh->user_response = -1;
                crWaitUntilV(ssh->user_response >= 0);
                s->dlgret = ssh->user_response;
            }
            ssh_set_frozen(ssh, 0);

            if (s->dlgret == 0) {
                ssh_disconnect(ssh, "User aborted at host key verification",
                               NULL, 0, TRUE);
                crStopV;
            }
        } else {
            sfree(keystr);
        }
    }

    for (i = 0; i < 32; i++) {
	s->rsabuf[i] = ssh->session_key[i];
	if (i < 16)
	    s->rsabuf[i] ^= s->session_id[i];
    }

    if (s->hostkey.bytes > s->servkey.bytes) {
	ret = rsa_ssh1_encrypt(s->rsabuf, 32, &s->servkey);
	if (ret)
	    ret = rsa_ssh1_encrypt(s->rsabuf, s->servkey.bytes, &s->hostkey);
    } else {
	ret = rsa_ssh1_encrypt(s->rsabuf, 32, &s->hostkey);
	if (ret)
	    ret = rsa_ssh1_encrypt(s->rsabuf, s->hostkey.bytes, &s->servkey);
    }
    if (!ret) {
	bombout(("SSH-1 public key encryptions failed due to bad formatting"));
	crStopV;
    }

    logevent("Encrypted session key");

    {
	int cipher_chosen = 0, warn = 0;
	const char *cipher_string = NULL;
	int i;
	for (i = 0; !cipher_chosen && i < CIPHER_MAX; i++) {
	    int next_cipher = conf_get_int_int(ssh->conf,
					       CONF_ssh_cipherlist, i);
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
	    crStopV;
	}

	/* Warn about chosen cipher if necessary. */
	if (warn) {
            ssh_set_frozen(ssh, 1);
	    s->dlgret = askalg(ssh->frontend, "cipher", cipher_string,
			       ssh_dialog_callback, ssh);
	    if (s->dlgret < 0) {
                ssh->user_response = -1;
                crWaitUntilV(ssh->user_response >= 0);
		s->dlgret = ssh->user_response;
	    }
            ssh_set_frozen(ssh, 0);
	    if (s->dlgret == 0) {
		ssh_disconnect(ssh, "User aborted at cipher warning", NULL,
			       0, TRUE);
		crStopV;
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

    pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_SESSION_KEY);
    put_byte(pkt, s->cipher_type);
    put_data(pkt, s->cookie, 8);
    put_uint16(pkt, s->len * 8);
    put_data(pkt, s->rsabuf, s->len);
    put_uint32(pkt, ssh->v1_local_protoflags);
    ssh_pkt_write(ssh, pkt);

    logevent("Trying to enable encryption...");

    sfree(s->rsabuf);

    {
        const struct ssh1_cipheralg *cipher =
            (s->cipher_type == SSH_CIPHER_BLOWFISH ? &ssh1_blowfish :
             s->cipher_type == SSH_CIPHER_DES ? &ssh1_des : &ssh1_3des);
        ssh1_bpp_new_cipher(ssh->bpp, cipher, ssh->session_key);
        logeventf(ssh, "Initialised %s encryption", cipher->text_name);
    }

    if (s->servkey.modulus) {
	sfree(s->servkey.modulus);
	s->servkey.modulus = NULL;
    }
    if (s->servkey.exponent) {
	sfree(s->servkey.exponent);
	s->servkey.exponent = NULL;
    }
    if (s->hostkey.modulus) {
	sfree(s->hostkey.modulus);
	s->hostkey.modulus = NULL;
    }
    if (s->hostkey.exponent) {
	sfree(s->hostkey.exponent);
	s->hostkey.exponent = NULL;
    }
    crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh1_login)) != NULL);

    if (pktin->type != SSH1_SMSG_SUCCESS) {
	bombout(("Encryption not successfully enabled"));
	crStopV;
    }

    logevent("Successfully started encryption");

    fflush(stdout); /* FIXME eh? */
    if ((ssh->username = get_remote_username(ssh->conf)) == NULL) {
        s->cur_prompt = new_prompts(ssh->frontend);
        s->cur_prompt->to_server = TRUE;
        s->cur_prompt->name = dupstr("SSH login name");
        add_prompt(s->cur_prompt, dupstr("login as: "), TRUE);
        s->userpass_ret = get_userpass_input(s->cur_prompt, NULL);
        while (1) {
            while (s->userpass_ret < 0 &&
                   bufchain_size(&ssh->user_input) > 0)
                s->userpass_ret = get_userpass_input(
                    s->cur_prompt, &ssh->user_input);

            if (s->userpass_ret >= 0)
                break;

            ssh->send_ok = 1;
            crReturnV;
            ssh->send_ok = 0;
        }
        if (!s->userpass_ret) {
            /*
             * Failed to get a username. Terminate.
             */
            free_prompts(s->cur_prompt);
            ssh_disconnect(ssh, "No username provided", NULL, 0, TRUE);
            crStopV;
        }
        ssh->username = dupstr(s->cur_prompt->prompts[0]->result);
        free_prompts(s->cur_prompt);
    }

    pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_USER);
    put_stringz(pkt, ssh->username);
    ssh_pkt_write(ssh, pkt);

    {
        char *userlog = dupprintf("Sent username \"%s\"", ssh->username);
        logevent(userlog);
        if (flags & FLAG_INTERACTIVE &&
            (!((flags & FLAG_STDERR) && (flags & FLAG_VERBOSE)))) {
            c_write_str(ssh, userlog);
            c_write_str(ssh, "\r\n");
        }
        sfree(userlog);
    }

    crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh1_login)) != NULL);

    if ((s->supported_auths_mask & (1 << SSH1_AUTH_RSA)) == 0) {
	/* We must not attempt PK auth. Pretend we've already tried it. */
	s->tried_publickey = s->tried_agent = 1;
    } else {
	s->tried_publickey = s->tried_agent = 0;
    }
    s->tis_auth_refused = s->ccard_auth_refused = 0;
    /*
     * Load the public half of any configured keyfile for later use.
     */
    s->keyfile = conf_get_filename(ssh->conf, CONF_keyfile);
    if (!filename_is_null(s->keyfile)) {
	int keytype;
	logeventf(ssh, "Reading key file \"%.150s\"",
		  filename_to_str(s->keyfile));
	keytype = key_type(s->keyfile);
	if (keytype == SSH_KEYTYPE_SSH1 ||
            keytype == SSH_KEYTYPE_SSH1_PUBLIC) {
	    const char *error;
            s->publickey_blob = strbuf_new();
	    if (rsa_ssh1_loadpub(s->keyfile,
                                 BinarySink_UPCAST(s->publickey_blob),
                                 &s->publickey_comment, &error)) {
                s->privatekey_available = (keytype == SSH_KEYTYPE_SSH1);
                if (!s->privatekey_available)
                    logeventf(ssh, "Key file contains public key only");
		s->privatekey_encrypted = rsa_ssh1_encrypted(s->keyfile, NULL);
	    } else {
		char *msgbuf;
		logeventf(ssh, "Unable to load key (%s)", error);
		msgbuf = dupprintf("Unable to load key file "
				   "\"%.150s\" (%s)\r\n",
				   filename_to_str(s->keyfile),
				   error);
		c_write_str(ssh, msgbuf);
		sfree(msgbuf);
                strbuf_free(s->publickey_blob);
                s->publickey_blob = NULL;
	    }
	} else {
	    char *msgbuf;
	    logeventf(ssh, "Unable to use this key file (%s)",
		      key_type_to_str(keytype));
	    msgbuf = dupprintf("Unable to use key file \"%.150s\""
			       " (%s)\r\n",
			       filename_to_str(s->keyfile),
			       key_type_to_str(keytype));
	    c_write_str(ssh, msgbuf);
	    sfree(msgbuf);
	    s->publickey_blob = NULL;
	}
    } else
	s->publickey_blob = NULL;

    while (pktin->type == SSH1_SMSG_FAILURE) {
	s->pwpkt_type = SSH1_CMSG_AUTH_PASSWORD;

	if (conf_get_int(ssh->conf, CONF_tryagent) && agent_exists() && !s->tried_agent) {
	    /*
	     * Attempt RSA authentication using Pageant.
	     */
	    void *r;
            int rlen;
            strbuf *request;

	    s->authed = FALSE;
	    s->tried_agent = 1;
	    logevent("Pageant is running. Requesting keys.");

	    /* Request the keys held by the agent. */
            request = strbuf_new_for_agent_query();
	    put_byte(request, SSH1_AGENTC_REQUEST_RSA_IDENTITIES);
            ssh->auth_agent_query = agent_query(
                request, &r, &rlen, ssh_agent_callback, ssh);
            strbuf_free(request);
	    if (ssh->auth_agent_query) {
                ssh->agent_response = NULL;
                crWaitUntilV(ssh->agent_response);
		r = ssh->agent_response;
		rlen = ssh->agent_response_len;
	    }
            s->agent_response = r;
            BinarySource_BARE_INIT(s->asrc, r, rlen);
            get_uint32(s->asrc); /* skip length field */
	    if (get_byte(s->asrc) == SSH1_AGENT_RSA_IDENTITIES_ANSWER) {
		s->nkeys = toint(get_uint32(s->asrc));
                if (s->nkeys < 0) {
                    logeventf(ssh, "Pageant reported negative key count %d",
                              s->nkeys);
                    s->nkeys = 0;
                }
		logeventf(ssh, "Pageant has %d SSH-1 keys", s->nkeys);
		for (s->keyi = 0; s->keyi < s->nkeys; s->keyi++) {
                    size_t start, end;
                    start = s->asrc->pos;
                    get_rsa_ssh1_pub(s->asrc, &s->key,
                                     RSA_SSH1_EXPONENT_FIRST);
                    end = s->asrc->pos;
                    s->comment = get_string(s->asrc);
                    if (get_err(s->asrc)) {
                        logevent("Pageant key list packet was truncated");
                        break;
                    }
		    if (s->publickey_blob) {
                        ptrlen keystr = make_ptrlen(
                            (const char *)s->asrc->data + start, end - start);

			if (keystr.len == s->publickey_blob->len &&
                            !memcmp(keystr.ptr, s->publickey_blob->s,
				    s->publickey_blob->len)) {
			    logeventf(ssh, "Pageant key #%d matches "
				      "configured key file", s->keyi);
			    s->tried_publickey = 1;
			} else
			    /* Skip non-configured key */
			    continue;
		    }
		    logeventf(ssh, "Trying Pageant key #%d", s->keyi);
                    pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_AUTH_RSA);
                    put_mp_ssh1(pkt, s->key.modulus);
                    ssh_pkt_write(ssh, pkt);
		    crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh1_login))
                                      != NULL);
		    if (pktin->type != SSH1_SMSG_AUTH_RSA_CHALLENGE) {
			logevent("Key refused");
			continue;
		    }
		    logevent("Received RSA challenge");
                    s->challenge = get_mp_ssh1(pktin);
		    if (get_err(pktin)) {
                        freebn(s->challenge);
			bombout(("Server's RSA challenge was badly formatted"));
			crStopV;
		    }

		    {
                        strbuf *agentreq;
			char *ret;
			void *vret;
			int retlen;

			agentreq = strbuf_new_for_agent_query();
			put_byte(agentreq, SSH1_AGENTC_RSA_CHALLENGE);
			put_uint32(agentreq, bignum_bitcount(s->key.modulus));
			put_mp_ssh1(agentreq, s->key.exponent);
			put_mp_ssh1(agentreq, s->key.modulus);
			put_mp_ssh1(agentreq, s->challenge);
			put_data(agentreq, s->session_id, 16);
			put_uint32(agentreq, 1);    /* response format */
                        ssh->auth_agent_query = agent_query(
                            agentreq, &vret, &retlen,
                            ssh_agent_callback, ssh);
                        strbuf_free(agentreq);

			if (ssh->auth_agent_query) {
                            ssh->agent_response = NULL;
                            crWaitUntilV(ssh->agent_response);
			    vret = ssh->agent_response;
			    retlen = ssh->agent_response_len;
			}

			ret = vret;
			if (ret) {
			    if (ret[4] == SSH1_AGENT_RSA_RESPONSE) {
				logevent("Sending Pageant's response");
                                pkt = ssh_bpp_new_pktout(ssh->bpp,
                                    SSH1_CMSG_AUTH_RSA_RESPONSE);
                                put_data(pkt, ret + 5, 16);
                                ssh_pkt_write(ssh, pkt);
				sfree(ret);
				crMaybeWaitUntilV(
                                    (pktin = pq_pop(&ssh->pq_ssh1_login))
                                    != NULL);
				if (pktin->type == SSH1_SMSG_SUCCESS) {
				    logevent
					("Pageant's response accepted");
				    if (flags & FLAG_VERBOSE) {
					c_write_str(ssh, "Authenticated using"
						    " RSA key \"");
					c_write(ssh, s->comment.ptr,
						s->comment.len);
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
		sfree(s->agent_response);
		if (s->publickey_blob && !s->tried_publickey)
		    logevent("Configured key file not in Pageant");
	    } else {
                logevent("Failed to get reply from Pageant");
            }
	    if (s->authed)
		break;
	}
	if (s->publickey_blob && s->privatekey_available &&
            !s->tried_publickey) {
	    /*
	     * Try public key authentication with the specified
	     * key file.
	     */
	    int got_passphrase; /* need not be kept over crReturn */
	    if (flags & FLAG_VERBOSE)
		c_write_str(ssh, "Trying public key authentication.\r\n");
	    s->keyfile = conf_get_filename(ssh->conf, CONF_keyfile);
	    logeventf(ssh, "Trying public key \"%s\"",
		      filename_to_str(s->keyfile));
	    s->tried_publickey = 1;
	    got_passphrase = FALSE;
	    while (!got_passphrase) {
		/*
		 * Get a passphrase, if necessary.
		 */
		char *passphrase = NULL;    /* only written after crReturn */
		const char *error;
		if (!s->privatekey_encrypted) {
		    if (flags & FLAG_VERBOSE)
			c_write_str(ssh, "No passphrase required.\r\n");
		    passphrase = NULL;
		} else {
		    s->cur_prompt = new_prompts(ssh->frontend);
		    s->cur_prompt->to_server = FALSE;
		    s->cur_prompt->name = dupstr("SSH key passphrase");
		    add_prompt(s->cur_prompt,
			       dupprintf("Passphrase for key \"%.100s\": ",
					 s->publickey_comment), FALSE);
                    s->userpass_ret = get_userpass_input(s->cur_prompt, NULL);
                    while (1) {
                        while (s->userpass_ret < 0 &&
                               bufchain_size(&ssh->user_input) > 0)
                            s->userpass_ret = get_userpass_input(
                                s->cur_prompt, &ssh->user_input);

                        if (s->userpass_ret >= 0)
                            break;

                        ssh->send_ok = 1;
                        crReturnV;
                        ssh->send_ok = 0;
                    }
		    if (!s->userpass_ret) {
			/* Failed to get a passphrase. Terminate. */
			free_prompts(s->cur_prompt);
			ssh_disconnect(ssh, NULL, "Unable to authenticate",
				       0, TRUE);
			crStopV;
		    }
		    passphrase = dupstr(s->cur_prompt->prompts[0]->result);
		    free_prompts(s->cur_prompt);
		}
		/*
		 * Try decrypting key with passphrase.
		 */
		s->keyfile = conf_get_filename(ssh->conf, CONF_keyfile);
		ret = rsa_ssh1_loadkey(
                    s->keyfile, &s->key, passphrase, &error);
		if (passphrase) {
		    smemclr(passphrase, strlen(passphrase));
		    sfree(passphrase);
		}
		if (ret == 1) {
		    /* Correct passphrase. */
		    got_passphrase = TRUE;
		} else if (ret == 0) {
		    c_write_str(ssh, "Couldn't load private key from ");
		    c_write_str(ssh, filename_to_str(s->keyfile));
		    c_write_str(ssh, " (");
		    c_write_str(ssh, error);
		    c_write_str(ssh, ").\r\n");
		    got_passphrase = FALSE;
		    break;	       /* go and try something else */
		} else if (ret == -1) {
		    c_write_str(ssh, "Wrong passphrase.\r\n"); /* FIXME */
		    got_passphrase = FALSE;
		    /* and try again */
		} else {
		    assert(0 && "unexpected return from rsa_ssh1_loadkey()");
		    got_passphrase = FALSE;   /* placate optimisers */
		}
	    }

	    if (got_passphrase) {

		/*
		 * Send a public key attempt.
		 */
                pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_AUTH_RSA);
                put_mp_ssh1(pkt, s->key.modulus);
                ssh_pkt_write(ssh, pkt);

		crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh1_login))
                                  != NULL);
		if (pktin->type == SSH1_SMSG_FAILURE) {
		    c_write_str(ssh, "Server refused our public key.\r\n");
		    continue;	       /* go and try something else */
		}
		if (pktin->type != SSH1_SMSG_AUTH_RSA_CHALLENGE) {
		    bombout(("Bizarre response to offer of public key"));
		    crStopV;
		}

		{
		    int i;
		    unsigned char buffer[32];
		    Bignum challenge, response;

                    challenge = get_mp_ssh1(pktin);
		    if (get_err(pktin)) {
                        freebn(challenge);
			bombout(("Server's RSA challenge was badly formatted"));
			crStopV;
		    }
		    response = rsa_ssh1_decrypt(challenge, &s->key);
		    freebn(s->key.private_exponent);/* burn the evidence */

		    for (i = 0; i < 32; i++) {
			buffer[i] = bignum_byte(response, 31 - i);
		    }

		    MD5Init(&md5c);
		    put_data(&md5c, buffer, 32);
		    put_data(&md5c, s->session_id, 16);
		    MD5Final(buffer, &md5c);

                    pkt = ssh_bpp_new_pktout(
                        ssh->bpp, SSH1_CMSG_AUTH_RSA_RESPONSE);
                    put_data(pkt, buffer, 16);
                    ssh_pkt_write(ssh, pkt);

		    freebn(challenge);
		    freebn(response);
		}

		crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh1_login))
                                  != NULL);
		if (pktin->type == SSH1_SMSG_FAILURE) {
		    if (flags & FLAG_VERBOSE)
			c_write_str(ssh, "Failed to authenticate with"
				    " our public key.\r\n");
		    continue;	       /* go and try something else */
		} else if (pktin->type != SSH1_SMSG_SUCCESS) {
		    bombout(("Bizarre response to RSA authentication response"));
		    crStopV;
		}

		break;		       /* we're through! */
	    }

	}

	/*
	 * Otherwise, try various forms of password-like authentication.
	 */
	s->cur_prompt = new_prompts(ssh->frontend);

	if (conf_get_int(ssh->conf, CONF_try_tis_auth) &&
	    (s->supported_auths_mask & (1 << SSH1_AUTH_TIS)) &&
	    !s->tis_auth_refused) {
	    s->pwpkt_type = SSH1_CMSG_AUTH_TIS_RESPONSE;
	    logevent("Requested TIS authentication");
            pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_AUTH_TIS);
            ssh_pkt_write(ssh, pkt);
	    crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh1_login)) != NULL);
	    if (pktin->type != SSH1_SMSG_AUTH_TIS_CHALLENGE) {
		logevent("TIS authentication declined");
		if (flags & FLAG_INTERACTIVE)
		    c_write_str(ssh, "TIS authentication refused.\r\n");
		s->tis_auth_refused = 1;
		continue;
	    } else {
		ptrlen challenge;
		char *instr_suf, *prompt;

		challenge = get_string(pktin);
		if (get_err(pktin)) {
		    bombout(("TIS challenge packet was badly formed"));
		    crStopV;
		}
		logevent("Received TIS challenge");
		s->cur_prompt->to_server = TRUE;
		s->cur_prompt->name = dupstr("SSH TIS authentication");
		/* Prompt heuristic comes from OpenSSH */
		if (memchr(challenge.ptr, '\n', challenge.len)) {
		    instr_suf = dupstr("");
		    prompt = mkstr(challenge);
		} else {
		    instr_suf = mkstr(challenge);
		    prompt = dupstr("Response: ");
		}
		s->cur_prompt->instruction =
		    dupprintf("Using TIS authentication.%s%s",
			      (*instr_suf) ? "\n" : "",
			      instr_suf);
		s->cur_prompt->instr_reqd = TRUE;
		add_prompt(s->cur_prompt, prompt, FALSE);
		sfree(instr_suf);
	    }
	}
	if (conf_get_int(ssh->conf, CONF_try_tis_auth) &&
	    (s->supported_auths_mask & (1 << SSH1_AUTH_CCARD)) &&
	    !s->ccard_auth_refused) {
	    s->pwpkt_type = SSH1_CMSG_AUTH_CCARD_RESPONSE;
	    logevent("Requested CryptoCard authentication");
            pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_AUTH_CCARD);
            ssh_pkt_write(ssh, pkt);
	    crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh1_login)) != NULL);
	    if (pktin->type != SSH1_SMSG_AUTH_CCARD_CHALLENGE) {
		logevent("CryptoCard authentication declined");
		c_write_str(ssh, "CryptoCard authentication refused.\r\n");
		s->ccard_auth_refused = 1;
		continue;
	    } else {
		ptrlen challenge;
		char *instr_suf, *prompt;

		challenge = get_string(pktin);
		if (get_err(pktin)) {
		    bombout(("CryptoCard challenge packet was badly formed"));
		    crStopV;
		}
		logevent("Received CryptoCard challenge");
		s->cur_prompt->to_server = TRUE;
		s->cur_prompt->name = dupstr("SSH CryptoCard authentication");
		s->cur_prompt->name_reqd = FALSE;
		/* Prompt heuristic comes from OpenSSH */
		if (memchr(challenge.ptr, '\n', challenge.len)) {
		    instr_suf = dupstr("");
		    prompt = mkstr(challenge);
		} else {
		    instr_suf = mkstr(challenge);
		    prompt = dupstr("Response: ");
		}
		s->cur_prompt->instruction =
		    dupprintf("Using CryptoCard authentication.%s%s",
			      (*instr_suf) ? "\n" : "",
			      instr_suf);
		s->cur_prompt->instr_reqd = TRUE;
		add_prompt(s->cur_prompt, prompt, FALSE);
		sfree(instr_suf);
	    }
	}
	if (s->pwpkt_type == SSH1_CMSG_AUTH_PASSWORD) {
	    if ((s->supported_auths_mask & (1 << SSH1_AUTH_PASSWORD)) == 0) {
		bombout(("No supported authentication methods available"));
		crStopV;
	    }
	    s->cur_prompt->to_server = TRUE;
	    s->cur_prompt->name = dupstr("SSH password");
	    add_prompt(s->cur_prompt, dupprintf("%s@%s's password: ",
						ssh->username, ssh->savedhost),
		       FALSE);
	}

	/*
	 * Show password prompt, having first obtained it via a TIS
	 * or CryptoCard exchange if we're doing TIS or CryptoCard
	 * authentication.
	 */
        s->userpass_ret = get_userpass_input(s->cur_prompt, NULL);
        while (1) {
            while (s->userpass_ret < 0 &&
                   bufchain_size(&ssh->user_input) > 0)
                s->userpass_ret = get_userpass_input(
                    s->cur_prompt, &ssh->user_input);

            if (s->userpass_ret >= 0)
                break;

            ssh->send_ok = 1;
            crReturnV;
            ssh->send_ok = 0;
        }
        if (!s->userpass_ret) {
            /*
             * Failed to get a password (for example
             * because one was supplied on the command line
             * which has already failed to work). Terminate.
             */
            free_prompts(s->cur_prompt);
            ssh_disconnect(ssh, NULL, "Unable to authenticate", 0, TRUE);
            crStopV;
        }

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
	     * A few servers can't deal with SSH1_MSG_IGNORE, at
	     * least in this context. For these servers, we need
	     * an alternative defence. We make use of the fact
	     * that the password is interpreted as a C string:
	     * so we can append a NUL, then some random data.
	     * 
	     * A few servers can deal with neither SSH1_MSG_IGNORE
	     * here _nor_ a padded password string.
	     * For these servers we are left with no defences
	     * against password length sniffing.
	     */
	    if (!(ssh->remote_bugs & BUG_CHOKES_ON_SSH1_IGNORE) &&
	        !(ssh->remote_bugs & BUG_NEEDS_SSH1_PLAIN_PASSWORD)) {
		/*
		 * The server can deal with SSH1_MSG_IGNORE, so
		 * we can use the primary defence.
		 */
		int bottom, top, pwlen, i;

		pwlen = strlen(s->cur_prompt->prompts[0]->result);
		if (pwlen < 16) {
		    bottom = 0;    /* zero length passwords are OK! :-) */
		    top = 15;
		} else {
		    bottom = pwlen & ~7;
		    top = bottom + 7;
		}

		assert(pwlen >= bottom && pwlen <= top);

		for (i = bottom; i <= top; i++) {
		    if (i == pwlen) {
                        pkt = ssh_bpp_new_pktout(ssh->bpp, s->pwpkt_type);
                        put_stringz(pkt, s->cur_prompt->prompts[0]->result);
                        ssh_pkt_write(ssh, pkt);
		    } else {
                        strbuf *random_data = strbuf_new();
			for (j = 0; j < i; j++)
                            put_byte(random_data, random_byte());

                        pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_MSG_IGNORE);
                        put_stringsb(pkt, random_data);
                        ssh_pkt_write(ssh, pkt);
		    }
		}
		logevent("Sending password with camouflage packets");
	    } 
	    else if (!(ssh->remote_bugs & BUG_NEEDS_SSH1_PLAIN_PASSWORD)) {
		/*
		 * The server can't deal with SSH1_MSG_IGNORE
		 * but can deal with padded passwords, so we
		 * can use the secondary defence.
		 */
                strbuf *padded_pw = strbuf_new();

		logevent("Sending length-padded password");
                pkt = ssh_bpp_new_pktout(ssh->bpp, s->pwpkt_type);
                put_asciz(padded_pw, s->cur_prompt->prompts[0]->result);
                do {
                    put_byte(padded_pw, random_byte());
                } while (padded_pw->len % 64 != 0);
                put_stringsb(pkt, padded_pw);
                ssh_pkt_write(ssh, pkt);
	    } else {
		/*
		 * The server is believed unable to cope with
		 * any of our password camouflage methods.
		 */
		logevent("Sending unpadded password");
                pkt = ssh_bpp_new_pktout(ssh->bpp, s->pwpkt_type);
                put_stringz(pkt, s->cur_prompt->prompts[0]->result);
                ssh_pkt_write(ssh, pkt);
	    }
	} else {
            pkt = ssh_bpp_new_pktout(ssh->bpp, s->pwpkt_type);
            put_stringz(pkt, s->cur_prompt->prompts[0]->result);
            ssh_pkt_write(ssh, pkt);
	}
	logevent("Sent password");
	free_prompts(s->cur_prompt);
	crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh1_login)) != NULL);
	if (pktin->type == SSH1_SMSG_FAILURE) {
	    if (flags & FLAG_VERBOSE)
		c_write_str(ssh, "Access denied\r\n");
	    logevent("Authentication refused");
	} else if (pktin->type != SSH1_SMSG_SUCCESS) {
	    bombout(("Strange packet received, type %d", pktin->type));
	    crStopV;
	}
    }

    /* Clear up */
    if (s->publickey_blob) {
	strbuf_free(s->publickey_blob);
	sfree(s->publickey_comment);
    }

    logevent("Authentication successful");

    /* Set up for the next phase */
    {
        int i;
        for (i = 0; i < SSH_MAX_MSG; i++)
            if (ssh->packet_dispatch[i] == ssh1_coro_wrapper_initial)
                ssh->packet_dispatch[i] = ssh1_coro_wrapper_session;
        ssh->current_user_input_fn = ssh1_connection_input;
        queue_idempotent_callback(&ssh->user_input_consumer);
    }

    crFinishV;
}

static void ssh_channel_try_eof(struct ssh_channel *c)
{
    Ssh ssh = c->ssh;
    PktOut *pktout;
    assert(c->pending_eof);          /* precondition for calling us */
    if (c->halfopen)
        return;                 /* can't close: not even opened yet */
    if (ssh->version == 2 && bufchain_size(&c->v.v2.outbuffer) > 0)
        return;              /* can't send EOF: pending outgoing data */

    c->pending_eof = FALSE;            /* we're about to send it */
    if (ssh->version == 1) {
        pktout = ssh_bpp_new_pktout(ssh->bpp, SSH1_MSG_CHANNEL_CLOSE);
        put_uint32(pktout, c->remoteid);
        ssh_pkt_write(ssh, pktout);
        c->closes |= CLOSES_SENT_EOF;
    } else {
        pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_CHANNEL_EOF);
        put_uint32(pktout, c->remoteid);
        ssh2_pkt_send(ssh, pktout);
        c->closes |= CLOSES_SENT_EOF;
	ssh2_channel_check_close(c);
    }
}

Conf *sshfwd_get_conf(struct ssh_channel *c)
{
    Ssh ssh = c->ssh;
    return ssh->conf;
}

void sshfwd_write_eof(struct ssh_channel *c)
{
    Ssh ssh = c->ssh;

    if (ssh->state == SSH_STATE_CLOSED)
	return;

    if (c->closes & CLOSES_SENT_EOF)
        return;

    c->pending_eof = TRUE;
    ssh_channel_try_eof(c);
}

void sshfwd_unclean_close(struct ssh_channel *c, const char *err)
{
    Ssh ssh = c->ssh;
    char *reason;

    if (ssh->state == SSH_STATE_CLOSED)
	return;

    reason = dupprintf("due to local error: %s", err);
    ssh_channel_close_local(c, reason);
    sfree(reason);
    c->pending_eof = FALSE;   /* this will confuse a zombie channel */

    ssh2_channel_check_close(c);
}

int sshfwd_write(struct ssh_channel *c, const void *buf, int len)
{
    Ssh ssh = c->ssh;

    if (ssh->state == SSH_STATE_CLOSED)
	return 0;

    return ssh_send_channel_data(c, buf, len);
}

void sshfwd_unthrottle(struct ssh_channel *c, int bufsize)
{
    Ssh ssh = c->ssh;

    if (ssh->state == SSH_STATE_CLOSED)
	return;

    ssh_channel_unthrottle(c, bufsize);
}

static void ssh_queueing_handler(Ssh ssh, PktIn *pktin)
{
    struct queued_handler *qh = ssh->qhead;

    assert(qh != NULL);

    assert(pktin->type == qh->msg1 || pktin->type == qh->msg2);

    if (qh->msg1 > 0) {
	assert(ssh->packet_dispatch[qh->msg1] == ssh_queueing_handler);
	ssh->packet_dispatch[qh->msg1] = ssh->q_saved_handler1;
    }
    if (qh->msg2 > 0) {
	assert(ssh->packet_dispatch[qh->msg2] == ssh_queueing_handler);
	ssh->packet_dispatch[qh->msg2] = ssh->q_saved_handler2;
    }

    if (qh->next) {
	ssh->qhead = qh->next;

	if (ssh->qhead->msg1 > 0) {
	    ssh->q_saved_handler1 = ssh->packet_dispatch[ssh->qhead->msg1];
	    ssh->packet_dispatch[ssh->qhead->msg1] = ssh_queueing_handler;
	}
	if (ssh->qhead->msg2 > 0) {
	    ssh->q_saved_handler2 = ssh->packet_dispatch[ssh->qhead->msg2];
	    ssh->packet_dispatch[ssh->qhead->msg2] = ssh_queueing_handler;
	}
    } else {
	ssh->qhead = ssh->qtail = NULL;
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
	    ssh->q_saved_handler1 = ssh->packet_dispatch[ssh->qhead->msg1];
	    ssh->packet_dispatch[qh->msg1] = ssh_queueing_handler;
	}
	if (qh->msg2 > 0) {
	    ssh->q_saved_handler2 = ssh->packet_dispatch[ssh->qhead->msg2];
	    ssh->packet_dispatch[qh->msg2] = ssh_queueing_handler;
	}
    } else {
	ssh->qtail->next = qh;
    }
    ssh->qtail = qh;
}

static void ssh_rportfwd_succfail(Ssh ssh, PktIn *pktin, void *ctx)
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
	pf->pfrec->remote = NULL;
	free_rportfwd(pf);
    }
}

int ssh_alloc_sharing_rportfwd(Ssh ssh, const char *shost, int sport,
                               ssh_sharing_connstate *share_ctx)
{
    struct ssh_rportfwd *pf = snew(struct ssh_rportfwd);
    pf->dhost = NULL;
    pf->dport = 0;
    pf->share_ctx = share_ctx;
    pf->shost = dupstr(shost);
    pf->sport = sport;
    pf->sportdesc = NULL;
    if (!ssh->rportfwds) {
        assert(ssh->version == 2);
        ssh->rportfwds = newtree234(ssh_rportcmp_ssh2);
    }
    if (add234(ssh->rportfwds, pf) != pf) {
        sfree(pf->shost);
        sfree(pf);
        return FALSE;
    }
    return TRUE;
}

void ssh_remove_sharing_rportfwd(Ssh ssh, const char *shost, int sport,
                                 ssh_sharing_connstate *share_ctx)
{
    struct ssh_rportfwd pf, *realpf;

    assert(ssh->rportfwds);
    pf.shost = dupstr(shost);
    pf.sport = sport;
    realpf = del234(ssh->rportfwds, &pf);
    assert(realpf);
    assert(realpf->share_ctx == share_ctx);
    sfree(realpf->shost);
    sfree(realpf);
}

static void ssh_sharing_global_request_response(Ssh ssh, PktIn *pktin,
                                                void *ctx)
{
    share_got_pkt_from_server(ctx, pktin->type,
                              BinarySource_UPCAST(pktin)->data,
                              BinarySource_UPCAST(pktin)->len);
}

void ssh_sharing_queue_global_request(Ssh ssh,
                                      ssh_sharing_connstate *share_ctx)
{
    ssh_queue_handler(ssh, SSH2_MSG_REQUEST_SUCCESS, SSH2_MSG_REQUEST_FAILURE,
                      ssh_sharing_global_request_response, share_ctx);
}

static void ssh_setup_portfwd(Ssh ssh, Conf *conf)
{
    struct ssh_portfwd *epf;
    int i;
    char *key, *val;

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

    for (val = conf_get_str_strs(conf, CONF_portfwd, NULL, &key);
	 val != NULL;
	 val = conf_get_str_strs(conf, CONF_portfwd, key, &key)) {
	char *kp, *kp2, *vp, *vp2;
	char address_family, type;
	int sport,dport,sserv,dserv;
	char *sports, *dports, *saddr, *host;

	kp = key;

	address_family = 'A';
	type = 'L';
	if (*kp == 'A' || *kp == '4' || *kp == '6')
	    address_family = *kp++;
	if (*kp == 'L' || *kp == 'R')
	    type = *kp++;

	if ((kp2 = host_strchr(kp, ':')) != NULL) {
	    /*
	     * There's a colon in the middle of the source port
	     * string, which means that the part before it is
	     * actually a source address.
	     */
	    char *saddr_tmp = dupprintf("%.*s", (int)(kp2 - kp), kp);
            saddr = host_strduptrim(saddr_tmp);
            sfree(saddr_tmp);
	    sports = kp2+1;
	} else {
	    saddr = NULL;
	    sports = kp;
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

	if (type == 'L' && !strcmp(val, "D")) {
            /* dynamic forwarding */
	    host = NULL;
	    dports = NULL;
	    dport = -1;
	    dserv = 0;
            type = 'D';
        } else {
            /* ordinary forwarding */
	    vp = val;
	    vp2 = vp + host_strcspn(vp, ":");
	    host = dupprintf("%.*s", (int)(vp2 - vp), vp);
	    if (*vp2)
		vp2++;
	    dports = vp2;
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
	}

	if (sport && dport) {
	    /* Set up a description of the source port. */
	    struct ssh_portfwd *pfrec, *epfrec;

	    pfrec = snew(struct ssh_portfwd);
	    pfrec->type = type;
	    pfrec->saddr = saddr;
	    pfrec->sserv = sserv ? dupstr(sports) : NULL;
	    pfrec->sport = sport;
	    pfrec->daddr = host;
	    pfrec->dserv = dserv ? dupstr(dports) : NULL;
	    pfrec->dport = dport;
	    pfrec->local = NULL;
	    pfrec->remote = NULL;
	    pfrec->addressfamily = (address_family == '4' ? ADDRTYPE_IPV4 :
				    address_family == '6' ? ADDRTYPE_IPV6 :
				    ADDRTYPE_UNSPEC);

	    epfrec = add234(ssh->portfwds, pfrec);
	    if (epfrec != pfrec) {
		if (epfrec->status == DESTROY) {
		    /*
		     * We already have a port forwarding up and running
		     * with precisely these parameters. Hence, no need
		     * to do anything; simply re-tag the existing one
		     * as KEEP.
		     */
		    epfrec->status = KEEP;
		}
		/*
		 * Anything else indicates that there was a duplicate
		 * in our input, which we'll silently ignore.
		 */
		free_portfwd(pfrec);
	    } else {
		pfrec->status = CREATE;
	    }
	} else {
	    sfree(saddr);
	    sfree(host);
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

	    /* epf->remote or epf->local may be NULL if setting up a
	     * forwarding failed. */
	    if (epf->remote) {
		struct ssh_rportfwd *rpf = epf->remote;
		PktOut *pktout;

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
		    pktout = ssh_bpp_new_pktout(
                        ssh->bpp, SSH2_MSG_GLOBAL_REQUEST);
		    put_stringz(pktout, "cancel-tcpip-forward");
		    put_bool(pktout, 0);/* _don't_ want reply */
		    if (epf->saddr) {
			put_stringz(pktout, epf->saddr);
		    } else if (conf_get_int(conf, CONF_rport_acceptall)) {
			/* XXX: rport_acceptall may not represent
			 * what was used to open the original connection,
			 * since it's reconfigurable. */
			put_stringz(pktout, "");
		    } else {
			put_stringz(pktout, "localhost");
		    }
		    put_uint32(pktout, epf->sport);
		    ssh2_pkt_send(ssh, pktout);
		}

		del234(ssh->rportfwds, rpf);
		free_rportfwd(rpf);
	    } else if (epf->local) {
		pfl_terminate(epf->local);
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
                char *err = pfl_listen(epf->daddr, epf->dport,
                                       epf->saddr, epf->sport,
                                       ssh, conf, &epf->local,
                                       epf->addressfamily);

		logeventf(ssh, "Local %sport %s forwarding to %s%s%s",
			  epf->addressfamily == ADDRTYPE_IPV4 ? "IPv4 " :
			  epf->addressfamily == ADDRTYPE_IPV6 ? "IPv6 " : "",
			  sportdesc, dportdesc,
			  err ? " failed: " : "", err ? err : "");
                if (err)
                    sfree(err);
	    } else if (epf->type == 'D') {
		char *err = pfl_listen(NULL, -1, epf->saddr, epf->sport,
                                       ssh, conf, &epf->local,
                                       epf->addressfamily);

		logeventf(ssh, "Local %sport %s SOCKS dynamic forwarding%s%s",
			  epf->addressfamily == ADDRTYPE_IPV4 ? "IPv4 " :
			  epf->addressfamily == ADDRTYPE_IPV6 ? "IPv6 " : "",
			  sportdesc,
			  err ? " failed: " : "", err ? err : "");

                if (err)
                    sfree(err);
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
                pf->share_ctx = NULL;
                pf->dhost = dupstr(epf->daddr);
		pf->dport = epf->dport;
                if (epf->saddr) {
                    pf->shost = dupstr(epf->saddr);
                } else if (conf_get_int(conf, CONF_rport_acceptall)) {
                    pf->shost = dupstr("");
                } else {
                    pf->shost = dupstr("localhost");
                }
		pf->sport = epf->sport;
		if (add234(ssh->rportfwds, pf) != pf) {
		    logeventf(ssh, "Duplicate remote port forwarding to %s:%d",
			      epf->daddr, epf->dport);
		    sfree(pf);
		} else {
                    PktOut *pktout;

		    logeventf(ssh, "Requesting remote port %s"
			      " forward to %s", sportdesc, dportdesc);

		    pf->sportdesc = sportdesc;
		    sportdesc = NULL;
		    epf->remote = pf;
		    pf->pfrec = epf;

		    if (ssh->version == 1) {
                        pktout = ssh_bpp_new_pktout(
                            ssh->bpp, SSH1_CMSG_PORT_FORWARD_REQUEST);
                        put_uint32(pktout, epf->sport);
                        put_stringz(pktout, epf->daddr);
                        put_uint32(pktout, epf->dport);
                        ssh_pkt_write(ssh, pktout);
			ssh_queue_handler(ssh, SSH1_SMSG_SUCCESS,
					  SSH1_SMSG_FAILURE,
					  ssh_rportfwd_succfail, pf);
		    } else {
			pktout = ssh_bpp_new_pktout(
                            ssh->bpp, SSH2_MSG_GLOBAL_REQUEST);
			put_stringz(pktout, "tcpip-forward");
			put_bool(pktout, 1);/* want reply */
			put_stringz(pktout, pf->shost);
			put_uint32(pktout, pf->sport);
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

static void ssh1_smsg_stdout_stderr_data(Ssh ssh, PktIn *pktin)
{
    ptrlen string;
    int bufsize;

    string = get_string(pktin);
    if (get_err(pktin)) {
	bombout(("Incoming terminal data packet was badly formed"));
	return;
    }

    bufsize = from_backend(ssh->frontend, pktin->type == SSH1_SMSG_STDERR_DATA,
			   string.ptr, string.len);
    if (!ssh->v1_stdout_throttling && bufsize > SSH1_BUFFER_LIMIT) {
	ssh->v1_stdout_throttling = 1;
	ssh_throttle_conn(ssh, +1);
    }
}

static void ssh1_smsg_x11_open(Ssh ssh, PktIn *pktin)
{
    /* Remote side is trying to open a channel to talk to our
     * X-Server. Give them back a local channel number. */
    struct ssh_channel *c;
    PktOut *pkt;
    int remoteid = get_uint32(pktin);

    logevent("Received X11 connect request");
    /* Refuse if X11 forwarding is disabled. */
    if (!ssh->X11_fwd_enabled) {
        pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_MSG_CHANNEL_OPEN_FAILURE);
        put_uint32(pkt, remoteid);
        ssh_pkt_write(ssh, pkt);
	logevent("Rejected X11 connect request");
    } else {
	c = snew(struct ssh_channel);
	c->ssh = ssh;

	ssh_channel_init(c);
        c->chan = x11_new_channel(ssh->x11authtree, c, NULL, -1, FALSE);
        c->remoteid = remoteid;
        c->halfopen = FALSE;
        pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION);
        put_uint32(pkt, c->remoteid);
        put_uint32(pkt, c->localid);
        ssh_pkt_write(ssh, pkt);
        logevent("Opened X11 forward channel");
    }
}

static void ssh1_smsg_agent_open(Ssh ssh, PktIn *pktin)
{
    /* Remote side is trying to open a channel to talk to our
     * agent. Give them back a local channel number. */
    struct ssh_channel *c;
    PktOut *pkt;
    int remoteid = toint(get_uint32(pktin));

    /* Refuse if agent forwarding is disabled. */
    if (!ssh->agentfwd_enabled) {
        pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_MSG_CHANNEL_OPEN_FAILURE);
        put_uint32(pkt, remoteid);
        ssh_pkt_write(ssh, pkt);
    } else {
	c = snew(struct ssh_channel);
	c->ssh = ssh;
	ssh_channel_init(c);
	c->remoteid = remoteid;
	c->halfopen = FALSE;
        c->chan = agentf_new(c);
        pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION);
        put_uint32(pkt, c->remoteid);
        put_uint32(pkt, c->localid);
        ssh_pkt_write(ssh, pkt);
    }
}

static void ssh1_msg_port_open(Ssh ssh, PktIn *pktin)
{
    /* Remote side is trying to open a channel to talk to a
     * forwarded port. Give them back a local channel number. */
    struct ssh_rportfwd pf, *pfp;
    PktOut *pkt;
    int remoteid;
    int port;
    ptrlen host;
    char *err;

    remoteid = toint(get_uint32(pktin));
    host = get_string(pktin);
    port = toint(get_uint32(pktin));

    pf.dhost = mkstr(host);
    pf.dport = port;
    pfp = find234(ssh->rportfwds, &pf, NULL);

    if (pfp == NULL) {
	logeventf(ssh, "Rejected remote port open request for %s:%d",
		  pf.dhost, port);
        pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_MSG_CHANNEL_OPEN_FAILURE);
        put_uint32(pkt, remoteid);
        ssh_pkt_write(ssh, pkt);
    } else {
        struct ssh_channel *c = snew(struct ssh_channel);
        c->ssh = ssh;

	logeventf(ssh, "Received remote port open request for %s:%d",
		  pf.dhost, port);
        err = pfd_connect(&c->chan, pf.dhost, port,
                          c, ssh->conf, pfp->pfrec->addressfamily);
	if (err != NULL) {
	    logeventf(ssh, "Port open failed: %s", err);
            sfree(err);
	    sfree(c);
            pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_MSG_CHANNEL_OPEN_FAILURE);
            put_uint32(pkt, remoteid);
            ssh_pkt_write(ssh, pkt);
	} else {
	    ssh_channel_init(c);
	    c->remoteid = remoteid;
	    c->halfopen = FALSE;
            pkt = ssh_bpp_new_pktout(
                ssh->bpp, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION);
            put_uint32(pkt, c->remoteid);
            put_uint32(pkt, c->localid);
            ssh_pkt_write(ssh, pkt);
	    logevent("Forwarded port opened successfully");
	}
    }

    sfree(pf.dhost);
}

static void ssh1_msg_channel_open_confirmation(Ssh ssh, PktIn *pktin)
{
    struct ssh_channel *c;

    c = ssh_channel_msg(ssh, pktin);
    chan_open_confirmation(c->chan);
    c->remoteid = get_uint32(pktin);
    c->halfopen = FALSE;
    c->throttling_conn = 0;

    if (c && c->pending_eof) {
	/*
	 * We have a pending close on this channel,
	 * which we decided on before the server acked
	 * the channel open. So now we know the
	 * remoteid, we can close it again.
	 */
        ssh_channel_try_eof(c);
    }
}

static void ssh1_msg_channel_open_failure(Ssh ssh, PktIn *pktin)
{
    struct ssh_channel *c;

    c = ssh_channel_msg(ssh, pktin);
    chan_open_failed(c->chan, NULL);
    chan_free(c->chan);

    del234(ssh->channels, c);
    sfree(c);
}

static void ssh1_msg_channel_close(Ssh ssh, PktIn *pktin)
{
    /* Remote side closes a channel. */
    struct ssh_channel *c;

    c = ssh_channel_msg(ssh, pktin);
    if (c) {

        if (pktin->type == SSH1_MSG_CHANNEL_CLOSE) {
            /*
             * Received CHANNEL_CLOSE, which we translate into
             * outgoing EOF.
             */
	    ssh_channel_got_eof(c);
        }

        if (pktin->type == SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION &&
            !(c->closes & CLOSES_RCVD_CLOSE)) {

            if (!(c->closes & CLOSES_SENT_EOF)) {
                bombout(("Received CHANNEL_CLOSE_CONFIRMATION for channel %u"
                         " for which we never sent CHANNEL_CLOSE\n",
			 c->localid));
            }

            c->closes |= CLOSES_RCVD_CLOSE;
        }

        if (!((CLOSES_SENT_EOF | CLOSES_RCVD_EOF) & ~c->closes) &&
            !(c->closes & CLOSES_SENT_CLOSE)) {
            PktOut *pkt = ssh_bpp_new_pktout(
                ssh->bpp, SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION);
            put_uint32(pkt, c->remoteid);
            ssh_pkt_write(ssh, pkt);
            c->closes |= CLOSES_SENT_CLOSE;
        }

	if (!((CLOSES_SENT_CLOSE | CLOSES_RCVD_CLOSE) & ~c->closes))
            ssh_channel_destroy(c);
    }
}

static int ssh_channel_data(struct ssh_channel *c, int is_stderr,
			    const void *data, int length)
{
    if (c->chan)
        chan_send(c->chan, is_stderr, data, length);
    return 0;
}

static void ssh1_msg_channel_data(Ssh ssh, PktIn *pktin)
{
    /* Data sent down one of our channels. */
    ptrlen data;
    struct ssh_channel *c;

    c = ssh_channel_msg(ssh, pktin);
    data = get_string(pktin);

    if (c) {
	int bufsize = ssh_channel_data(c, FALSE, data.ptr, data.len);
	if (!c->throttling_conn && bufsize > SSH1_BUFFER_LIMIT) {
	    c->throttling_conn = 1;
	    ssh_throttle_conn(ssh, +1);
	}
    }
}

static void ssh1_smsg_exit_status(Ssh ssh, PktIn *pktin)
{
    PktOut *pkt;
    ssh->exitcode = get_uint32(pktin);
    logeventf(ssh, "Server sent command exit status %d", ssh->exitcode);
    pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_EXIT_CONFIRMATION);
    ssh_pkt_write(ssh, pkt);
    /*
     * In case `helpful' firewalls or proxies tack
     * extra human-readable text on the end of the
     * session which we might mistake for another
     * encrypted packet, we close the session once
     * we've sent EXIT_CONFIRMATION.
     */
    ssh_disconnect(ssh, NULL, NULL, 0, TRUE);
}

/* Helper function to deal with sending tty modes for REQUEST_PTY */
static void ssh1_send_ttymode(BinarySink *bs,
                              const struct ssh_ttymode *mode, char *val)
{
    put_byte(bs, mode->opcode);

    switch (mode->type) {
      case TTY_OP_CHAR:
        put_byte(bs, ssh_tty_parse_specchar(val));
	break;
      case TTY_OP_BOOL:
        put_byte(bs, ssh_tty_parse_boolean(val));
	break;
    }
}

int ssh_agent_forwarding_permitted(Ssh ssh)
{
    return conf_get_int(ssh->conf, CONF_agentfwd) && agent_exists();
}

static void do_ssh1_connection(void *vctx)
{
    Ssh ssh = (Ssh)vctx;
    PktIn *pktin;
    PktOut *pkt;

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

    if (ssh_agent_forwarding_permitted(ssh)) {
	logevent("Requesting agent forwarding");
        pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_AGENT_REQUEST_FORWARDING);
        ssh_pkt_write(ssh, pkt);
        crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh1_connection)) != NULL);
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

    if (conf_get_int(ssh->conf, CONF_x11_forward)) {
        ssh->x11disp =
            x11_setup_display(conf_get_str(ssh->conf, CONF_x11_display),
                              ssh->conf);
        if (!ssh->x11disp) {
            /* FIXME: return an error message from x11_setup_display */
            logevent("X11 forwarding not enabled: unable to"
                     " initialise X display");
        } else {
            ssh->x11auth = x11_invent_fake_auth
                (ssh->x11authtree, conf_get_int(ssh->conf, CONF_x11_auth));
            ssh->x11auth->disp = ssh->x11disp;

            logevent("Requesting X11 forwarding");
            pkt = ssh_bpp_new_pktout(
                ssh->bpp, SSH1_CMSG_X11_REQUEST_FORWARDING);
            put_stringz(pkt, ssh->x11auth->protoname);
            put_stringz(pkt, ssh->x11auth->datastring);
            if (ssh->v1_local_protoflags & SSH1_PROTOFLAG_SCREEN_NUMBER)
                put_uint32(pkt, ssh->x11disp->screennum);
            ssh_pkt_write(ssh, pkt);
            crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh1_connection))
                              != NULL);
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
    }

    ssh_setup_portfwd(ssh, ssh->conf);
    ssh->packet_dispatch[SSH1_MSG_PORT_OPEN] = ssh1_msg_port_open;

    if (!conf_get_int(ssh->conf, CONF_nopty)) {
	PktOut *pkt;
	/* Unpick the terminal-speed string. */
	/* XXX perhaps we should allow no speeds to be sent. */
	ssh->ospeed = 38400; ssh->ispeed = 38400; /* last-resort defaults */
	sscanf(conf_get_str(ssh->conf, CONF_termspeed), "%d,%d", &ssh->ospeed, &ssh->ispeed);
	/* Send the pty request. */
	pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_REQUEST_PTY);
	put_stringz(pkt, conf_get_str(ssh->conf, CONF_termtype));
	put_uint32(pkt, ssh->term_height);
	put_uint32(pkt, ssh->term_width);
	put_uint32(pkt, 0); /* width in pixels */
	put_uint32(pkt, 0); /* height in pixels */
	parse_ttymodes(BinarySink_UPCAST(pkt), ssh, ssh1_send_ttymode);
	put_byte(pkt, SSH1_TTY_OP_ISPEED);
	put_uint32(pkt, ssh->ispeed);
	put_byte(pkt, SSH1_TTY_OP_OSPEED);
	put_uint32(pkt, ssh->ospeed);
	put_byte(pkt, SSH_TTY_OP_END);
	ssh_pkt_write(ssh, pkt);
	ssh->state = SSH_STATE_INTERMED;
        crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh1_connection)) != NULL);
	if (pktin->type != SSH1_SMSG_SUCCESS
	    && pktin->type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crStopV;
	} else if (pktin->type == SSH1_SMSG_FAILURE) {
	    c_write_str(ssh, "Server refused to allocate pty\r\n");
	    ssh->editing = ssh->echoing = 1;
	} else {
            logeventf(ssh, "Allocated pty (ospeed %dbps, ispeed %dbps)",
                      ssh->ospeed, ssh->ispeed);
            ssh->got_pty = TRUE;
        }
    } else {
	ssh->editing = ssh->echoing = 1;
    }

    if (conf_get_int(ssh->conf, CONF_compression)) {
        pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_REQUEST_COMPRESSION);
        put_uint32(pkt, 6);            /* gzip compression level */
        ssh_pkt_write(ssh, pkt);
        crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh1_connection)) != NULL);
	if (pktin->type != SSH1_SMSG_SUCCESS
	    && pktin->type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crStopV;
	} else if (pktin->type == SSH1_SMSG_FAILURE) {
	    c_write_str(ssh, "Server refused to compress\r\n");
	}
	logevent("Started zlib (RFC1950) compression");
        ssh1_bpp_start_compression(ssh->bpp);
    }

    /*
     * Start the shell or command.
     * 
     * Special case: if the first-choice command is an SSH-2
     * subsystem (hence not usable here) and the second choice
     * exists, we fall straight back to that.
     */
    {
	char *cmd = conf_get_str(ssh->conf, CONF_remote_cmd);
	
	if (conf_get_int(ssh->conf, CONF_ssh_subsys) &&
	    conf_get_str(ssh->conf, CONF_remote_cmd2)) {
	    cmd = conf_get_str(ssh->conf, CONF_remote_cmd2);
	    ssh->fallback_cmd = TRUE;
	}
	if (*cmd) {
            pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_EXEC_CMD);
            put_stringz(pkt, cmd);
            ssh_pkt_write(ssh, pkt);
        } else {
            pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_EXEC_SHELL);
            ssh_pkt_write(ssh, pkt);
        }
	logevent("Started session");
    }

    ssh->state = SSH_STATE_SESSION;
    if (ssh->size_needed)
        backend_size(&ssh->backend, ssh->term_width, ssh->term_height);
    if (ssh->eof_needed)
        backend_special(&ssh->backend, TS_EOF);

    if (ssh->ldisc)
	ldisc_echoedit_update(ssh->ldisc);  /* cause ldisc to notice changes */
    ssh->send_ok = 1;
    ssh->channels = newtree234(ssh_channelcmp);
    while (1) {

	/*
	 * By this point, most incoming packets are already being
	 * handled by the dispatch table, and we need only pay
	 * attention to the unusual ones.
	 */

	while ((pktin = pq_pop(&ssh->pq_ssh1_connection)) != NULL) {
	    if (pktin->type == SSH1_SMSG_SUCCESS) {
		/* may be from EXEC_SHELL on some servers */
	    } else if (pktin->type == SSH1_SMSG_FAILURE) {
		/* may be from EXEC_SHELL on some servers
		 * if no pty is available or in other odd cases. Ignore */
	    } else {
		bombout(("Strange packet received: type %d", pktin->type));
		crStopV;
	    }
	}
        while (bufchain_size(&ssh->user_input) > 0) {
            void *data;
            int len;
            bufchain_prefix(&ssh->user_input, &data, &len);
            if (len > 512)
                len = 512;
            pkt = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_STDIN_DATA);
            put_string(pkt, data, len);
            ssh_pkt_write(ssh, pkt);
            bufchain_consume(&ssh->user_input, len);
        }
	crReturnV;
    }

    crFinishV;
}

/*
 * Handle the top-level SSH-2 protocol.
 */
static void ssh1_msg_debug(Ssh ssh, PktIn *pktin)
{
    ptrlen msg = get_string(pktin);
    logeventf(ssh, "Remote debug message: %.*s", PTRLEN_PRINTF(msg));
}

static void ssh1_msg_disconnect(Ssh ssh, PktIn *pktin)
{
    ptrlen msg = get_string(pktin);
    bombout(("Server sent disconnect message:\n\"%.*s\"", PTRLEN_PRINTF(msg)));
}

static void ssh_msg_ignore(Ssh ssh, PktIn *pktin)
{
    /* Do nothing, because we're ignoring it! Duhh. */
}

static void ssh1_login_input(Ssh ssh)
{
    do_ssh1_login(ssh);
}

static void ssh1_connection_input(Ssh ssh)
{
    do_ssh1_connection(ssh);
}

static void ssh1_coro_wrapper_initial(Ssh ssh, PktIn *pktin)
{
    pktin->refcount++;   /* avoid packet being freed when we return */
    pq_push(&ssh->pq_ssh1_login, pktin);
    queue_idempotent_callback(&ssh->ssh1_login_icb);
}

static void ssh1_coro_wrapper_session(Ssh ssh, PktIn *pktin)
{
    pktin->refcount++;   /* avoid packet being freed when we return */
    pq_push(&ssh->pq_ssh1_connection, pktin);
    queue_idempotent_callback(&ssh->ssh1_connection_icb);
}

static void ssh1_protocol_setup(Ssh ssh)
{
    int i;

    ssh->bpp = ssh1_bpp_new();

    /*
     * Most messages are handled by the main protocol routine.
     */
    for (i = 0; i < SSH_MAX_MSG; i++)
	ssh->packet_dispatch[i] = ssh1_coro_wrapper_initial;

    /*
     * These special message types we install handlers for.
     */
    ssh->packet_dispatch[SSH1_MSG_DISCONNECT] = ssh1_msg_disconnect;
    ssh->packet_dispatch[SSH1_MSG_IGNORE] = ssh_msg_ignore;
    ssh->packet_dispatch[SSH1_MSG_DEBUG] = ssh1_msg_debug;
}

/*
 * Utility routines for decoding comma-separated strings in KEXINIT.
 */
static int first_in_commasep_string(char const *needle, char const *haystack,
				    int haylen)
{
    int needlen;
    if (!needle || !haystack)	       /* protect against null pointers */
	return 0;
    needlen = strlen(needle);

    if (haylen >= needlen &&       /* haystack is long enough */
	!memcmp(needle, haystack, needlen) &&	/* initial match */
	(haylen == needlen || haystack[needlen] == ',')
	/* either , or EOS follows */
	)
	return 1;
    return 0;
}

static int in_commasep_string(char const *needle, char const *haystack,
			      int haylen)
{
    char *p;

    if (!needle || !haystack)	       /* protect against null pointers */
	return 0;
    /*
     * Is it at the start of the string?
     */
    if (first_in_commasep_string(needle, haystack, haylen))
	return 1;
    /*
     * If not, search for the next comma and resume after that.
     * If no comma found, terminate.
     */
    p = memchr(haystack, ',', haylen);
    if (!p) return 0;
    /* + 1 to skip over comma */
    return in_commasep_string(needle, p + 1, haylen - (p + 1 - haystack));
}

/*
 * Add a value to a strbuf containing a comma-separated list.
 */
static void add_to_commasep(strbuf *buf, const char *data)
{
    if (buf->len > 0)
	put_byte(buf, ',');
    put_data(buf, data, strlen(data));
}


/*
 * SSH-2 key derivation (RFC 4253 section 7.2).
 */
static void ssh2_mkkey(Ssh ssh, strbuf *out, Bignum K, unsigned char *H,
                       char chr, int keylen)
{
    const struct ssh_hashalg *h = ssh->kex->hash;
    int keylen_padded;
    unsigned char *key;
    ssh_hash *s, *s2;

    if (keylen == 0)
        return;

    /*
     * Round the requested amount of key material up to a multiple of
     * the length of the hash we're using to make it. This makes life
     * simpler because then we can just write each hash output block
     * straight into the output buffer without fiddling about
     * truncating the last one. Since it's going into a strbuf, and
     * strbufs are always smemclr()ed on free, there's no need to
     * worry about leaving extra potentially-sensitive data in memory
     * that the caller didn't ask for.
     */
    keylen_padded = ((keylen + h->hlen - 1) / h->hlen) * h->hlen;

    out->len = 0;
    key = strbuf_append(out, keylen_padded);

    /* First hlen bytes. */
    s = ssh_hash_new(h);
    if (!(ssh->remote_bugs & BUG_SSH2_DERIVEKEY))
        put_mp_ssh2(s, K);
    put_data(s, H, h->hlen);
    put_byte(s, chr);
    put_data(s, ssh->v2_session_id, ssh->v2_session_id_len);
    ssh_hash_final(s, key);

    /* Subsequent blocks of hlen bytes. */
    if (keylen_padded > h->hlen) {
        int offset;

        s = ssh_hash_new(h);
        if (!(ssh->remote_bugs & BUG_SSH2_DERIVEKEY))
            put_mp_ssh2(s, K);
        put_data(s, H, h->hlen);

        for (offset = h->hlen; offset < keylen_padded; offset += h->hlen) {
            put_data(s, key + offset - h->hlen, h->hlen);
            s2 = ssh_hash_copy(s);
            ssh_hash_final(s2, key + offset);
        }

        ssh_hash_free(s);
    }
}

/*
 * Structure for constructing KEXINIT algorithm lists.
 */
#define MAXKEXLIST 16
struct kexinit_algorithm {
    const char *name;
    union {
	struct {
	    const struct ssh_kex *kex;
	    int warn;
	} kex;
	struct {
            const ssh_keyalg *hostkey;
            int warn;
        } hk;
	struct {
            const struct ssh2_cipheralg *cipher;
	    int warn;
	} cipher;
	struct {
            const struct ssh2_macalg *mac;
	    int etm;
	} mac;
	const struct ssh_compress *comp;
    } u;
};

/*
 * Find a slot in a KEXINIT algorithm list to use for a new algorithm.
 * If the algorithm is already in the list, return a pointer to its
 * entry, otherwise return an entry from the end of the list.
 * This assumes that every time a particular name is passed in, it
 * comes from the same string constant.  If this isn't true, this
 * function may need to be rewritten to use strcmp() instead.
 */
static struct kexinit_algorithm *ssh2_kexinit_addalg(struct kexinit_algorithm
						     *list, const char *name)
{
    int i;

    for (i = 0; i < MAXKEXLIST; i++)
	if (list[i].name == NULL || list[i].name == name) {
	    list[i].name = name;
	    return &list[i];
	}
    assert(!"No space in KEXINIT list");
    return NULL;
}

#ifndef NO_GSSAPI
/*
 * Data structure managing host keys in sessions based on GSSAPI KEX.
 *
 * In a session we started with a GSSAPI key exchange, the concept of
 * 'host key' has completely different lifetime and security semantics
 * from the usual ones. Per RFC 4462 section 2.1, we assume that any
 * host key delivered to us in the course of a GSSAPI key exchange is
 * _solely_ there to use as a transient fallback within the same
 * session, if at the time of a subsequent rekey the GSS credentials
 * are temporarily invalid and so a non-GSS KEX method has to be used.
 *
 * In particular, in a GSS-based SSH deployment, host keys may not
 * even _be_ persistent identities for the server; it would be
 * legitimate for a server to generate a fresh one routinely if it
 * wanted to, like SSH-1 server keys.
 *
 * So, in this mode, we never touch the persistent host key cache at
 * all, either to check keys against it _or_ to store keys in it.
 * Instead, we maintain an in-memory cache of host keys that have been
 * mentioned in GSS key exchanges within this particular session, and
 * we permit precisely those host keys in non-GSS rekeys.
 */
struct ssh_transient_hostkey_cache_entry {
    const ssh_keyalg *alg;
    strbuf *pub_blob;
};

static int ssh_transient_hostkey_cache_cmp(void *av, void *bv)
{
    const struct ssh_transient_hostkey_cache_entry
        *a = (const struct ssh_transient_hostkey_cache_entry *)av,
        *b = (const struct ssh_transient_hostkey_cache_entry *)bv;
    return strcmp(a->alg->ssh_id, b->alg->ssh_id);
}

static int ssh_transient_hostkey_cache_find(void *av, void *bv)
{
    const ssh_keyalg *aalg = (const ssh_keyalg *)av;
    const struct ssh_transient_hostkey_cache_entry
        *b = (const struct ssh_transient_hostkey_cache_entry *)bv;
    return strcmp(aalg->ssh_id, b->alg->ssh_id);
}

static void ssh_init_transient_hostkey_store(Ssh ssh)
{
    ssh->transient_hostkey_cache =
        newtree234(ssh_transient_hostkey_cache_cmp);
}

static void ssh_cleanup_transient_hostkey_store(Ssh ssh)
{
    struct ssh_transient_hostkey_cache_entry *ent;
    while ((ent = delpos234(ssh->transient_hostkey_cache, 0)) != NULL) {
        strbuf_free(ent->pub_blob);
        sfree(ent);
    }
    freetree234(ssh->transient_hostkey_cache);
}

static void ssh_store_transient_hostkey(Ssh ssh, ssh_key *key)
{
    struct ssh_transient_hostkey_cache_entry *ent, *retd;

    if ((ent = find234(ssh->transient_hostkey_cache, (void *)ssh_key_alg(key),
                       ssh_transient_hostkey_cache_find)) != NULL) {
        strbuf_free(ent->pub_blob);
        sfree(ent);
    }

    ent = snew(struct ssh_transient_hostkey_cache_entry);
    ent->alg = ssh_key_alg(key);
    ent->pub_blob = strbuf_new();
    ssh_key_public_blob(key, BinarySink_UPCAST(ent->pub_blob));
    retd = add234(ssh->transient_hostkey_cache, ent);
    assert(retd == ent);
}

static int ssh_verify_transient_hostkey(Ssh ssh, ssh_key *key)
{
    struct ssh_transient_hostkey_cache_entry *ent;
    int toret = FALSE;

    if ((ent = find234(ssh->transient_hostkey_cache, (void *)ssh_key_alg(key),
                       ssh_transient_hostkey_cache_find)) != NULL) {
        strbuf *this_blob = strbuf_new();
        ssh_key_public_blob(key, BinarySink_UPCAST(this_blob));

        if (this_blob->len == ent->pub_blob->len &&
            !memcmp(this_blob->s, ent->pub_blob->s,
                    this_blob->len))
            toret = TRUE;

        strbuf_free(this_blob);
    }

    return toret;
}

static int ssh_have_transient_hostkey(Ssh ssh, const ssh_keyalg *alg)
{
    struct ssh_transient_hostkey_cache_entry *ent =
        find234(ssh->transient_hostkey_cache, (void *)alg,
                ssh_transient_hostkey_cache_find);
    return ent != NULL;
}

static int ssh_have_any_transient_hostkey(Ssh ssh)
{
    return count234(ssh->transient_hostkey_cache) > 0;
}

#endif /* NO_GSSAPI */

/*
 * Handle the SSH-2 transport layer.
 */
static void do_ssh2_transport(void *vctx)
{
    Ssh ssh = (Ssh)vctx;
    PktIn *pktin;

    enum kexlist {
	KEXLIST_KEX, KEXLIST_HOSTKEY, KEXLIST_CSCIPHER, KEXLIST_SCCIPHER,
	KEXLIST_CSMAC, KEXLIST_SCMAC, KEXLIST_CSCOMP, KEXLIST_SCCOMP,
	NKEXLIST
    };
    const char * kexlist_descr[NKEXLIST] = {
	"key exchange algorithm", "host key algorithm",
	"client-to-server cipher", "server-to-client cipher",
	"client-to-server MAC", "server-to-client MAC",
	"client-to-server compression method",
	"server-to-client compression method" };
    struct do_ssh2_transport_state {
	int crLine;
	int nbits, pbits, warn_kex, warn_hk, warn_cscipher, warn_sccipher;
	Bignum p, g, e, f, K;
	void *our_kexinit;
	int our_kexinitlen;
	int kex_init_value, kex_reply_value;
        const struct ssh2_macalg *const *maclist;
	int nmacs;
        struct {
            const struct ssh2_cipheralg *cipher;
            const struct ssh2_macalg *mac;
            int etm_mode;
            const struct ssh_compress *comp;
        } in, out;
	ptrlen hostkeydata, sigdata;
        char *keystr, *fingerprint;
	ssh_key *hkey;		       /* actual host key */
	struct RSAKey *rsakey;         /* for RSA kex */
        struct ec_key *eckey;          /* for ECDH kex */
	unsigned char exchange_hash[SSH2_KEX_MAX_HASH_LEN];
	int n_preferred_kex;
        int can_gssapi_keyex;
        int need_gss_transient_hostkey;
        int warned_about_no_gss_transient_hostkey;
        const struct ssh_kexes *preferred_kex[KEX_MAX + 1]; /* +1 for GSSAPI */
	int n_preferred_hk;
	int preferred_hk[HK_MAX];
	int n_preferred_ciphers;
	const struct ssh2_ciphers *preferred_ciphers[CIPHER_MAX];
	const struct ssh_compress *preferred_comp;
	int userauth_succeeded;	    /* for delayed compression */
	int pending_compression;
	int got_session_id;
	PktOut *pktout;
        int dlgret;
	int guessok;
	int ignorepkt;
	struct kexinit_algorithm kexlists[NKEXLIST][MAXKEXLIST];
#ifndef NO_GSSAPI
        Ssh_gss_buf gss_buf;
        Ssh_gss_buf gss_rcvtok, gss_sndtok;
        Ssh_gss_stat gss_stat;
        Ssh_gss_ctx gss_ctx;
        Ssh_gss_buf mic;
        int init_token_sent;
        int complete_rcvd;
        int gss_delegate;
        time_t gss_cred_expiry;
#endif
    };
    crState(do_ssh2_transport_state);

    assert(!ssh->bare_connection);
    assert(ssh->version == 2);

    crBeginState;

    s->in.cipher = s->out.cipher = NULL;
    s->in.mac = s->out.mac = NULL;
    s->in.comp = s->out.comp = NULL;

    s->got_session_id = FALSE;
    s->userauth_succeeded = FALSE;
    s->pending_compression = FALSE;
    s->need_gss_transient_hostkey = FALSE;
    s->warned_about_no_gss_transient_hostkey = FALSE;

    /*
     * Be prepared to work around the buggy MAC problem.
     */
    if (ssh->remote_bugs & BUG_SSH2_HMAC)
	s->maclist = buggymacs, s->nmacs = lenof(buggymacs);
    else
	s->maclist = macs, s->nmacs = lenof(macs);

  begin_key_exchange:

#ifndef NO_GSSAPI
    if (s->need_gss_transient_hostkey) {
        /*
         * This flag indicates a special case in which we must not do
         * GSS key exchange even if we could. (See comments below,
         * where the flag was set on the previous key exchange.)
         */
        s->can_gssapi_keyex = FALSE;
    } else if (conf_get_int(ssh->conf, CONF_try_gssapi_kex)) {
        /*
         * We always check if we have GSS creds before we come up with
         * the kex algorithm list, otherwise future rekeys will fail
         * when creds expire. To make this so, this code section must
         * follow the begin_key_exchange label above, otherwise this
         * section would execute just once per-connection.
         *
         * Update GSS state unless the reason we're here is that a
         * timer just checked the GSS state and decided that we should
         * rekey to update delegated credentials. In that case, the
         * state is "fresh".
         */
        if (ssh->rekey_class != RK_GSS_UPDATE)
            ssh2_gss_update(ssh, TRUE);

        /* Do GSSAPI KEX when capable */
        s->can_gssapi_keyex = ssh->gss_status & GSS_KEX_CAPABLE;

        /*
         * But not when failure is likely. [ GSS implementations may
         * attempt (and fail) to use a ticket that is almost expired
         * when retrieved from the ccache that actually expires by the
         * time the server receives it. ]
         *
         * Note: The first time always try KEXGSS if we can, failures
         * will be very rare, and disabling the initial GSS KEX is
         * worse. Some day GSS libraries will ignore cached tickets
         * whose lifetime is critically short, and will instead use
         * fresh ones.
         */
        if (!s->got_session_id && (ssh->gss_status & GSS_CTXT_MAYFAIL) != 0)
            s->can_gssapi_keyex = 0;
        s->gss_delegate = conf_get_int(ssh->conf, CONF_gssapifwd);
    } else {
        s->can_gssapi_keyex = FALSE;
    }
#endif

    ssh->pls.kctx = SSH2_PKTCTX_NOKEX;
    {
	int i, j, k, warn;
	struct kexinit_algorithm *alg;

	/*
	 * Set up the preferred key exchange. (NULL => warn below here)
	 */
	s->n_preferred_kex = 0;
        if (s->can_gssapi_keyex)
            s->preferred_kex[s->n_preferred_kex++] = &ssh_gssk5_sha1_kex;
        for (i = 0; i < KEX_MAX; i++) {
	    switch (conf_get_int_int(ssh->conf, CONF_ssh_kexlist, i)) {
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
	      case KEX_RSA:
		s->preferred_kex[s->n_preferred_kex++] =
		    &ssh_rsa_kex;
		break;
              case KEX_ECDH:
                s->preferred_kex[s->n_preferred_kex++] =
                    &ssh_ecdh_kex;
                break;
	      case KEX_WARN:
		/* Flag for later. Don't bother if it's the last in
		 * the list. */
		if (i < KEX_MAX - 1) {
		    s->preferred_kex[s->n_preferred_kex++] = NULL;
		}
		break;
	    }
	}

	/*
	 * Set up the preferred host key types. These are just the ids
	 * in the enum in putty.h, so 'warn below here' is indicated
	 * by HK_WARN.
	 */
	s->n_preferred_hk = 0;
	for (i = 0; i < HK_MAX; i++) {
            int id = conf_get_int_int(ssh->conf, CONF_ssh_hklist, i);
            /* As above, don't bother with HK_WARN if it's last in the
             * list */
	    if (id != HK_WARN || i < HK_MAX - 1)
                s->preferred_hk[s->n_preferred_hk++] = id;
	}

	/*
	 * Set up the preferred ciphers. (NULL => warn below here)
	 */
	s->n_preferred_ciphers = 0;
	for (i = 0; i < CIPHER_MAX; i++) {
	    switch (conf_get_int_int(ssh->conf, CONF_ssh_cipherlist, i)) {
	      case CIPHER_BLOWFISH:
		s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_blowfish;
		break;
	      case CIPHER_DES:
		if (conf_get_int(ssh->conf, CONF_ssh2_des_cbc)) {
		    s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_des;
		}
		break;
	      case CIPHER_3DES:
		s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_3des;
		break;
	      case CIPHER_AES:
		s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_aes;
		break;
	      case CIPHER_ARCFOUR:
		s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_arcfour;
		break;
              case CIPHER_CHACHA20:
                s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_ccp;
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
	if (conf_get_int(ssh->conf, CONF_compression))
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

	for (i = 0; i < NKEXLIST; i++)
	    for (j = 0; j < MAXKEXLIST; j++)
	        s->kexlists[i][j].name = NULL;
	/* List key exchange algorithms. */
	warn = FALSE;
	for (i = 0; i < s->n_preferred_kex; i++) {
	    const struct ssh_kexes *k = s->preferred_kex[i];
	    if (!k) warn = TRUE;
	    else for (j = 0; j < k->nkexes; j++) {
		alg = ssh2_kexinit_addalg(s->kexlists[KEXLIST_KEX],
					  k->list[j]->name);
	        alg->u.kex.kex = k->list[j];
		alg->u.kex.warn = warn;
	    }
	}
	/* List server host key algorithms. */
        if (!s->got_session_id) {
            /*
             * In the first key exchange, we list all the algorithms
             * we're prepared to cope with, but prefer those algorithms
	     * for which we have a host key for this host.
             *
             * If the host key algorithm is below the warning
             * threshold, we warn even if we did already have a key
             * for it, on the basis that if the user has just
             * reconfigured that host key type to be warned about,
             * they surely _do_ want to be alerted that a server
             * they're actually connecting to is using it.
             */
            warn = FALSE;
            for (i = 0; i < s->n_preferred_hk; i++) {
                if (s->preferred_hk[i] == HK_WARN)
                    warn = TRUE;
                for (j = 0; j < lenof(hostkey_algs); j++) {
                    if (hostkey_algs[j].id != s->preferred_hk[i])
                        continue;
                    if (have_ssh_host_key(ssh->savedhost, ssh->savedport,
                                          hostkey_algs[j].alg->cache_id)) {
                        alg = ssh2_kexinit_addalg(s->kexlists[KEXLIST_HOSTKEY],
                                                  hostkey_algs[j].alg->ssh_id);
                        alg->u.hk.hostkey = hostkey_algs[j].alg;
                        alg->u.hk.warn = warn;
                    }
                }
	    }
            warn = FALSE;
            for (i = 0; i < s->n_preferred_hk; i++) {
                if (s->preferred_hk[i] == HK_WARN)
                    warn = TRUE;
                for (j = 0; j < lenof(hostkey_algs); j++) {
                    if (hostkey_algs[j].id != s->preferred_hk[i])
                        continue;
                    alg = ssh2_kexinit_addalg(s->kexlists[KEXLIST_HOSTKEY],
                                              hostkey_algs[j].alg->ssh_id);
                    alg->u.hk.hostkey = hostkey_algs[j].alg;
                    alg->u.hk.warn = warn;
                }
            }
#ifndef NO_GSSAPI
        } else if (ssh->gss_kex_used && !s->need_gss_transient_hostkey) {
            /*
             * If we've previously done a GSSAPI KEX, then we list
             * precisely the algorithms for which a previous GSS key
             * exchange has delivered us a host key, because we expect
             * one of exactly those keys to be used in any subsequent
             * non-GSS-based rekey.
             *
             * An exception is if this is the key exchange we
             * triggered for the purposes of populating that cache -
             * in which case the cache will currently be empty, which
             * isn't helpful!
             */
            warn = FALSE;
            for (i = 0; i < s->n_preferred_hk; i++) {
                if (s->preferred_hk[i] == HK_WARN)
                    warn = TRUE;
                for (j = 0; j < lenof(hostkey_algs); j++) {
                    if (hostkey_algs[j].id != s->preferred_hk[i])
                        continue;
                    if (ssh_have_transient_hostkey(ssh, hostkey_algs[j].alg)) {
                        alg = ssh2_kexinit_addalg(s->kexlists[KEXLIST_HOSTKEY],
                                                  hostkey_algs[j].alg->ssh_id);
                        alg->u.hk.hostkey = hostkey_algs[j].alg;
                        alg->u.hk.warn = warn;
                    }
                }
            }
#endif
        } else {
            /*
             * In subsequent key exchanges, we list only the kex
             * algorithm that was selected in the first key exchange,
             * so that we keep getting the same host key and hence
             * don't have to interrupt the user's session to ask for
             * reverification.
             */
            assert(ssh->kex);
	    alg = ssh2_kexinit_addalg(s->kexlists[KEXLIST_HOSTKEY],
				      ssh->hostkey_alg->ssh_id);
	    alg->u.hk.hostkey = ssh->hostkey_alg;
            alg->u.hk.warn = FALSE;
        }
        if (s->can_gssapi_keyex) {
            alg = ssh2_kexinit_addalg(s->kexlists[KEXLIST_HOSTKEY], "null");
            alg->u.hk.hostkey = NULL;
        }
	/* List encryption algorithms (client->server then server->client). */
	for (k = KEXLIST_CSCIPHER; k <= KEXLIST_SCCIPHER; k++) {
	    warn = FALSE;
#ifdef FUZZING
	    alg = ssh2_kexinit_addalg(s->kexlists[k], "none");
	    alg->u.cipher.cipher = NULL;
	    alg->u.cipher.warn = warn;
#endif /* FUZZING */
	    for (i = 0; i < s->n_preferred_ciphers; i++) {
		const struct ssh2_ciphers *c = s->preferred_ciphers[i];
		if (!c) warn = TRUE;
		else for (j = 0; j < c->nciphers; j++) {
		    alg = ssh2_kexinit_addalg(s->kexlists[k],
					      c->list[j]->name);
		    alg->u.cipher.cipher = c->list[j];
		    alg->u.cipher.warn = warn;
		}
	    }
	}
	/* List MAC algorithms (client->server then server->client). */
	for (j = KEXLIST_CSMAC; j <= KEXLIST_SCMAC; j++) {
#ifdef FUZZING
	    alg = ssh2_kexinit_addalg(s->kexlists[j], "none");
	    alg->u.mac.mac = NULL;
	    alg->u.mac.etm = FALSE;
#endif /* FUZZING */
	    for (i = 0; i < s->nmacs; i++) {
		alg = ssh2_kexinit_addalg(s->kexlists[j], s->maclist[i]->name);
		alg->u.mac.mac = s->maclist[i];
		alg->u.mac.etm = FALSE;
            }
	    for (i = 0; i < s->nmacs; i++)
                /* For each MAC, there may also be an ETM version,
                 * which we list second. */
                if (s->maclist[i]->etm_name) {
		    alg = ssh2_kexinit_addalg(s->kexlists[j],
					      s->maclist[i]->etm_name);
		    alg->u.mac.mac = s->maclist[i];
		    alg->u.mac.etm = TRUE;
		}
	}
	/* List client->server compression algorithms,
	 * then server->client compression algorithms. (We use the
	 * same set twice.) */
	for (j = KEXLIST_CSCOMP; j <= KEXLIST_SCCOMP; j++) {
	    assert(lenof(compressions) > 1);
	    /* Prefer non-delayed versions */
	    alg = ssh2_kexinit_addalg(s->kexlists[j], s->preferred_comp->name);
	    alg->u.comp = s->preferred_comp;
	    /* We don't even list delayed versions of algorithms until
	     * they're allowed to be used, to avoid a race. See the end of
	     * this function. */
	    if (s->userauth_succeeded && s->preferred_comp->delayed_name) {
		alg = ssh2_kexinit_addalg(s->kexlists[j],
					  s->preferred_comp->delayed_name);
		alg->u.comp = s->preferred_comp;
	    }
	    for (i = 0; i < lenof(compressions); i++) {
		const struct ssh_compress *c = compressions[i];
		alg = ssh2_kexinit_addalg(s->kexlists[j], c->name);
		alg->u.comp = c;
		if (s->userauth_succeeded && c->delayed_name) {
		    alg = ssh2_kexinit_addalg(s->kexlists[j], c->delayed_name);
		    alg->u.comp = c;
		}
	    }
	}
	/*
	 * Construct and send our key exchange packet.
	 */
	s->pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_KEXINIT);
	for (i = 0; i < 16; i++)
	    put_byte(s->pktout, (unsigned char) random_byte());
	for (i = 0; i < NKEXLIST; i++) {
	    strbuf *list = strbuf_new();
	    for (j = 0; j < MAXKEXLIST; j++) {
		if (s->kexlists[i][j].name == NULL) break;
		add_to_commasep(list, s->kexlists[i][j].name);
	    }
            put_stringsb(s->pktout, list);
	}
	/* List client->server languages. Empty list. */
	put_stringz(s->pktout, "");
	/* List server->client languages. Empty list. */
	put_stringz(s->pktout, "");
	/* First KEX packet does _not_ follow, because we're not that brave. */
	put_bool(s->pktout, FALSE);
	/* Reserved. */
	put_uint32(s->pktout, 0);
    }

    s->our_kexinitlen = s->pktout->length - 5;
    s->our_kexinit = snewn(s->our_kexinitlen, unsigned char);
    memcpy(s->our_kexinit, s->pktout->data + 5, s->our_kexinitlen); 

    ssh_pkt_write(ssh, s->pktout);

    crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_transport)) != NULL);

    /*
     * Now examine the other side's KEXINIT to see what we're up
     * to.
     */
    {
        ptrlen str;
	int i, j;

	if (pktin->type != SSH2_MSG_KEXINIT) {
	    bombout(("expected key exchange packet from server"));
	    crStopV;
	}
	ssh->kex = NULL;
	ssh->hostkey_alg = NULL;
	s->in.cipher = s->out.cipher = NULL;
	s->in.mac = s->out.mac = NULL;
	s->in.comp = s->out.comp = NULL;
	s->warn_kex = s->warn_hk = FALSE;
        s->warn_cscipher = s->warn_sccipher = FALSE;

	get_data(pktin, 16);           /* skip garbage cookie */

	s->guessok = FALSE;
	for (i = 0; i < NKEXLIST; i++) {
	    str = get_string(pktin);
	    if (get_err(pktin)) {
		bombout(("KEXINIT packet was incomplete"));
		crStopV;
	    }

            /* If we've already selected a cipher which requires a
             * particular MAC, then just select that, and don't even
             * bother looking through the server's KEXINIT string for
             * MACs. */
            if (i == KEXLIST_CSMAC && s->out.cipher &&
                s->out.cipher->required_mac) {
                s->out.mac = s->out.cipher->required_mac;
                s->out.etm_mode = !!(s->out.mac->etm_name);
                goto matched;
            }
            if (i == KEXLIST_SCMAC && s->in.cipher &&
                s->in.cipher->required_mac) {
                s->in.mac = s->in.cipher->required_mac;
                s->in.etm_mode = !!(s->in.mac->etm_name);
                goto matched;
            }

	    for (j = 0; j < MAXKEXLIST; j++) {
		struct kexinit_algorithm *alg = &s->kexlists[i][j];
		if (alg->name == NULL) break;
		if (in_commasep_string(alg->name, str.ptr, str.len)) {
		    /* We've found a matching algorithm. */
		    if (i == KEXLIST_KEX || i == KEXLIST_HOSTKEY) {
			/* Check if we might need to ignore first kex pkt */
			if (j != 0 ||
			    !first_in_commasep_string(alg->name,
                                                      str.ptr, str.len))
			    s->guessok = FALSE;
		    }
		    if (i == KEXLIST_KEX) {
			ssh->kex = alg->u.kex.kex;
			s->warn_kex = alg->u.kex.warn;
		    } else if (i == KEXLIST_HOSTKEY) {
                        /*
                         * Ignore an unexpected/inappropriate offer of "null",
                         * we offer "null" when we're willing to use GSS KEX,
                         * but it is only acceptable when GSSKEX is actually
                         * selected.
                         */
                        if (alg->u.hk.hostkey == NULL &&
                            ssh->kex->main_type != KEXTYPE_GSS)
                            continue;
			ssh->hostkey_alg = alg->u.hk.hostkey;
                        s->warn_hk = alg->u.hk.warn;
		    } else if (i == KEXLIST_CSCIPHER) {
			s->out.cipher = alg->u.cipher.cipher;
			s->warn_cscipher = alg->u.cipher.warn;
		    } else if (i == KEXLIST_SCCIPHER) {
			s->in.cipher = alg->u.cipher.cipher;
			s->warn_sccipher = alg->u.cipher.warn;
		    } else if (i == KEXLIST_CSMAC) {
			s->out.mac = alg->u.mac.mac;
			s->out.etm_mode = alg->u.mac.etm;
		    } else if (i == KEXLIST_SCMAC) {
			s->in.mac = alg->u.mac.mac;
			s->in.etm_mode = alg->u.mac.etm;
		    } else if (i == KEXLIST_CSCOMP) {
			s->out.comp = alg->u.comp;
		    } else if (i == KEXLIST_SCCOMP) {
			s->in.comp = alg->u.comp;
		    }
		    goto matched;
		}
		if ((i == KEXLIST_CSCOMP || i == KEXLIST_SCCOMP) &&
		    in_commasep_string(alg->u.comp->delayed_name,
                                       str.ptr, str.len) &&
                    !s->userauth_succeeded)
		    s->pending_compression = TRUE;  /* try this later */
	    }
	    bombout(("Couldn't agree a %s (available: %.*s)",
		     kexlist_descr[i], PTRLEN_PRINTF(str)));
	    crStopV;
	  matched:;

            if (i == KEXLIST_HOSTKEY &&
                !ssh->gss_kex_used &&
                ssh->kex->main_type != KEXTYPE_GSS) {
                int j;

                /*
                 * In addition to deciding which host key we're
                 * actually going to use, we should make a list of the
                 * host keys offered by the server which we _don't_
                 * have cached. These will be offered as cross-
                 * certification options by ssh_get_specials.
                 *
                 * We also count the key we're currently using for KEX
                 * as one we've already got, because by the time this
                 * menu becomes visible, it will be.
                 */
                ssh->n_uncert_hostkeys = 0;

                for (j = 0; j < lenof(hostkey_algs); j++) {
                    if (hostkey_algs[j].alg != ssh->hostkey_alg &&
                        in_commasep_string(hostkey_algs[j].alg->ssh_id,
                                           str.ptr, str.len) &&
                        !have_ssh_host_key(ssh->savedhost, ssh->savedport,
                                           hostkey_algs[j].alg->cache_id)) {
                        ssh->uncert_hostkeys[ssh->n_uncert_hostkeys++] = j;
                    }
                }
            }
	}

	if (s->pending_compression) {
	    logevent("Server supports delayed compression; "
		     "will try this later");
	}
	get_string(pktin);  /* client->server language */
	get_string(pktin);  /* server->client language */
	s->ignorepkt = get_bool(pktin) && !s->guessok;

        ssh->exhash = ssh_hash_new(ssh->kex->hash);
        put_stringz(ssh->exhash, ssh->v_c);
        put_stringz(ssh->exhash, ssh->v_s);
        put_string(ssh->exhash, s->our_kexinit, s->our_kexinitlen);
	sfree(s->our_kexinit);
        /* Include the type byte in the hash of server's KEXINIT */
        put_string(ssh->exhash,
                   (const char *)BinarySource_UPCAST(pktin)->data - 1,
                   BinarySource_UPCAST(pktin)->len + 1);

	if (s->warn_kex) {
	    ssh_set_frozen(ssh, 1);
	    s->dlgret = askalg(ssh->frontend, "key-exchange algorithm",
			       ssh->kex->name,
			       ssh_dialog_callback, ssh);
	    if (s->dlgret < 0) {
                ssh->user_response = -1;
		crWaitUntilV(ssh->user_response >= 0);
		s->dlgret = ssh->user_response;
	    }
	    ssh_set_frozen(ssh, 0);
	    if (s->dlgret == 0) {
		ssh_disconnect(ssh, "User aborted at kex warning", NULL,
			       0, TRUE);
		crStopV;
	    }
	}

	if (s->warn_hk) {
            int j, k;
            char *betteralgs;

	    ssh_set_frozen(ssh, 1);

            /*
             * Change warning box wording depending on why we chose a
             * warning-level host key algorithm. If it's because
             * that's all we have *cached*, use the askhk mechanism,
             * and list the host keys we could usefully cross-certify.
             * Otherwise, use askalg for the standard wording.
             */
            betteralgs = NULL;
            for (j = 0; j < ssh->n_uncert_hostkeys; j++) {
                const struct ssh_signkey_with_user_pref_id *hktype =
                    &hostkey_algs[ssh->uncert_hostkeys[j]];
                int better = FALSE;
                for (k = 0; k < HK_MAX; k++) {
                    int id = conf_get_int_int(ssh->conf, CONF_ssh_hklist, k);
                    if (id == HK_WARN) {
                        break;
                    } else if (id == hktype->id) {
                        better = TRUE;
                        break;
                    }
                }
                if (better) {
                    if (betteralgs) {
                        char *old_ba = betteralgs;
                        betteralgs = dupcat(betteralgs, ",",
                                            hktype->alg->ssh_id,
                                            (const char *)NULL);
                        sfree(old_ba);
                    } else {
                        betteralgs = dupstr(hktype->alg->ssh_id);
                    }
                }
            }
            if (betteralgs) {
                s->dlgret = askhk(ssh->frontend, ssh->hostkey_alg->ssh_id,
                                  betteralgs, ssh_dialog_callback, ssh);
                sfree(betteralgs);
            } else {
                s->dlgret = askalg(ssh->frontend, "host key type",
                                   ssh->hostkey_alg->ssh_id,
                                   ssh_dialog_callback, ssh);
            }
	    if (s->dlgret < 0) {
                ssh->user_response = -1;
                crWaitUntilV(ssh->user_response >= 0);
		s->dlgret = ssh->user_response;
	    }
	    ssh_set_frozen(ssh, 0);
	    if (s->dlgret == 0) {
		ssh_disconnect(ssh, "User aborted at host key warning", NULL,
			       0, TRUE);
		crStopV;
	    }
	}

	if (s->warn_cscipher) {
	    ssh_set_frozen(ssh, 1);
	    s->dlgret = askalg(ssh->frontend,
			       "client-to-server cipher",
			       s->out.cipher->name,
			       ssh_dialog_callback, ssh);
	    if (s->dlgret < 0) {
                ssh->user_response = -1;
                crWaitUntilV(ssh->user_response >= 0);
		s->dlgret = ssh->user_response;
	    }
	    ssh_set_frozen(ssh, 0);
	    if (s->dlgret == 0) {
		ssh_disconnect(ssh, "User aborted at cipher warning", NULL,
			       0, TRUE);
		crStopV;
	    }
	}

	if (s->warn_sccipher) {
	    ssh_set_frozen(ssh, 1);
	    s->dlgret = askalg(ssh->frontend,
			       "server-to-client cipher",
			       s->in.cipher->name,
			       ssh_dialog_callback, ssh);
	    if (s->dlgret < 0) {
                ssh->user_response = -1;
                crWaitUntilV(ssh->user_response >= 0);
		s->dlgret = ssh->user_response;
	    }
	    ssh_set_frozen(ssh, 0);
	    if (s->dlgret == 0) {
		ssh_disconnect(ssh, "User aborted at cipher warning", NULL,
			       0, TRUE);
		crStopV;
	    }
	}

	if (s->ignorepkt) /* first_kex_packet_follows */
            crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_transport)) != NULL);
    }

    if (ssh->kex->main_type == KEXTYPE_DH) {
        /*
         * Work out the number of bits of key we will need from the
         * key exchange. We start with the maximum key length of
         * either cipher...
         */
        {
            int csbits, scbits;

            csbits = s->out.cipher ? s->out.cipher->real_keybits : 0;
            scbits = s->in.cipher ? s->in.cipher->real_keybits : 0;
            s->nbits = (csbits > scbits ? csbits : scbits);
        }
        /* The keys only have hlen-bit entropy, since they're based on
         * a hash. So cap the key size at hlen bits. */
        if (s->nbits > ssh->kex->hash->hlen * 8)
            s->nbits = ssh->kex->hash->hlen * 8;

        /*
         * If we're doing Diffie-Hellman group exchange, start by
         * requesting a group.
         */
        if (dh_is_gex(ssh->kex)) {
            logevent("Doing Diffie-Hellman group exchange");
            ssh->pls.kctx = SSH2_PKTCTX_DHGEX;
            /*
             * Work out how big a DH group we will need to allow that
             * much data.
             */
            s->pbits = 512 << ((s->nbits - 1) / 64);
            if (s->pbits < DH_MIN_SIZE)
                s->pbits = DH_MIN_SIZE;
            if (s->pbits > DH_MAX_SIZE)
                s->pbits = DH_MAX_SIZE;
            if ((ssh->remote_bugs & BUG_SSH2_OLDGEX)) {
                s->pktout = ssh_bpp_new_pktout(
                    ssh->bpp, SSH2_MSG_KEX_DH_GEX_REQUEST_OLD);
                put_uint32(s->pktout, s->pbits);
            } else {
                s->pktout = ssh_bpp_new_pktout(
                    ssh->bpp, SSH2_MSG_KEX_DH_GEX_REQUEST);
                put_uint32(s->pktout, DH_MIN_SIZE);
                put_uint32(s->pktout, s->pbits);
                put_uint32(s->pktout, DH_MAX_SIZE);
            }
            ssh_pkt_write(ssh, s->pktout);

            crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_transport)) != NULL);
            if (pktin->type != SSH2_MSG_KEX_DH_GEX_GROUP) {
                bombout(("expected key exchange group packet from server"));
                crStopV;
            }
            s->p = get_mp_ssh2(pktin);
            s->g = get_mp_ssh2(pktin);
            if (get_err(pktin)) {
                freebn(s->p);
                freebn(s->g);
                bombout(("unable to read mp-ints from incoming group packet"));
                crStopV;
            }
            ssh->kex_ctx = dh_setup_gex(s->p, s->g);
            s->kex_init_value = SSH2_MSG_KEX_DH_GEX_INIT;
            s->kex_reply_value = SSH2_MSG_KEX_DH_GEX_REPLY;
        } else {
            ssh->pls.kctx = SSH2_PKTCTX_DHGROUP;
            ssh->kex_ctx = dh_setup_group(ssh->kex);
            s->kex_init_value = SSH2_MSG_KEXDH_INIT;
            s->kex_reply_value = SSH2_MSG_KEXDH_REPLY;
            logeventf(ssh, "Using Diffie-Hellman with standard group \"%s\"",
                      ssh->kex->groupname);
        }

        logeventf(ssh, "Doing Diffie-Hellman key exchange with hash %s",
                  ssh->kex->hash->text_name);
        /*
         * Now generate and send e for Diffie-Hellman.
         */
        set_busy_status(ssh->frontend, BUSY_CPU); /* this can take a while */
        s->e = dh_create_e(ssh->kex_ctx, s->nbits * 2);
        s->pktout = ssh_bpp_new_pktout(ssh->bpp, s->kex_init_value);
        put_mp_ssh2(s->pktout, s->e);
        ssh_pkt_write(ssh, s->pktout);

        set_busy_status(ssh->frontend, BUSY_WAITING); /* wait for server */
        crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_transport)) != NULL);
        if (pktin->type != s->kex_reply_value) {
            bombout(("expected key exchange reply packet from server"));
            crStopV;
        }
        set_busy_status(ssh->frontend, BUSY_CPU); /* cogitate */
        s->hostkeydata = get_string(pktin);
        s->hkey = ssh_key_new_pub(ssh->hostkey_alg, s->hostkeydata);
        s->f = get_mp_ssh2(pktin);
        s->sigdata = get_string(pktin);
        if (get_err(pktin)) {
            bombout(("unable to parse key exchange reply packet"));
            crStopV;
        }

        {
            const char *err = dh_validate_f(ssh->kex_ctx, s->f);
            if (err) {
                bombout(("key exchange reply failed validation: %s", err));
                crStopV;
            }
        }
        s->K = dh_find_K(ssh->kex_ctx, s->f);

        /* We assume everything from now on will be quick, and it might
         * involve user interaction. */
        set_busy_status(ssh->frontend, BUSY_NOT);

        put_stringpl(ssh->exhash, s->hostkeydata);
        if (dh_is_gex(ssh->kex)) {
            if (!(ssh->remote_bugs & BUG_SSH2_OLDGEX))
                put_uint32(ssh->exhash, DH_MIN_SIZE);
            put_uint32(ssh->exhash, s->pbits);
            if (!(ssh->remote_bugs & BUG_SSH2_OLDGEX))
                put_uint32(ssh->exhash, DH_MAX_SIZE);
            put_mp_ssh2(ssh->exhash, s->p);
            put_mp_ssh2(ssh->exhash, s->g);
        }
        put_mp_ssh2(ssh->exhash, s->e);
        put_mp_ssh2(ssh->exhash, s->f);

        dh_cleanup(ssh->kex_ctx);
        freebn(s->f);
        if (dh_is_gex(ssh->kex)) {
            freebn(s->g);
            freebn(s->p);
        }
    } else if (ssh->kex->main_type == KEXTYPE_ECDH) {

        logeventf(ssh, "Doing ECDH key exchange with curve %s and hash %s",
                  ssh_ecdhkex_curve_textname(ssh->kex),
                  ssh->kex->hash->text_name);
        ssh->pls.kctx = SSH2_PKTCTX_ECDHKEX;

        s->eckey = ssh_ecdhkex_newkey(ssh->kex);
        if (!s->eckey) {
            bombout(("Unable to generate key for ECDH"));
            crStopV;
        }

        s->pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_KEX_ECDH_INIT);
        {
            strbuf *pubpoint = strbuf_new();
            ssh_ecdhkex_getpublic(s->eckey, BinarySink_UPCAST(pubpoint));
            put_stringsb(s->pktout, pubpoint);
        }

        ssh_pkt_write(ssh, s->pktout);

        crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_transport)) != NULL);
        if (pktin->type != SSH2_MSG_KEX_ECDH_REPLY) {
            ssh_ecdhkex_freekey(s->eckey);
            bombout(("expected ECDH reply packet from server"));
            crStopV;
        }

        s->hostkeydata = get_string(pktin);
        put_stringpl(ssh->exhash, s->hostkeydata);
        s->hkey = ssh_key_new_pub(ssh->hostkey_alg, s->hostkeydata);

        {
            strbuf *pubpoint = strbuf_new();
            ssh_ecdhkex_getpublic(s->eckey, BinarySink_UPCAST(pubpoint));
            put_string(ssh->exhash, pubpoint->u, pubpoint->len);
            strbuf_free(pubpoint);
        }

        {
            ptrlen keydata = get_string(pktin);
            put_stringpl(ssh->exhash, keydata);
            s->K = ssh_ecdhkex_getkey(s->eckey, keydata.ptr, keydata.len);
            if (!get_err(pktin) && !s->K) {
                ssh_ecdhkex_freekey(s->eckey);
                bombout(("point received in ECDH was not valid"));
                crStopV;
            }
        }

        s->sigdata = get_string(pktin);
        if (get_err(pktin)) {
            bombout(("unable to parse key exchange reply packet"));
            crStopV;
        }

        ssh_ecdhkex_freekey(s->eckey);
#ifndef NO_GSSAPI
    } else if (ssh->kex->main_type == KEXTYPE_GSS) {
        ptrlen data;

        ssh->pls.kctx = SSH2_PKTCTX_GSSKEX;
        s->init_token_sent = 0;
        s->complete_rcvd = 0;
        s->hkey = NULL;
        s->fingerprint = NULL;
        s->keystr = NULL;

        /*
         * Work out the number of bits of key we will need from the
         * key exchange. We start with the maximum key length of
         * either cipher...
         *
         * This is rote from the KEXTYPE_DH section above.
         */
        {
            int csbits, scbits;

            csbits = s->out.cipher->real_keybits;
            scbits = s->in.cipher->real_keybits;
            s->nbits = (csbits > scbits ? csbits : scbits);
        }
        /* The keys only have hlen-bit entropy, since they're based on
         * a hash. So cap the key size at hlen bits. */
        if (s->nbits > ssh->kex->hash->hlen * 8)
            s->nbits = ssh->kex->hash->hlen * 8;

        if (dh_is_gex(ssh->kex)) {
            /*
             * Work out how big a DH group we will need to allow that
             * much data.
             */
            s->pbits = 512 << ((s->nbits - 1) / 64);
            logeventf(ssh, "Doing GSSAPI (with Kerberos V5) Diffie-Hellman "
                      "group exchange, with minimum %d bits", s->pbits);
            s->pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_KEXGSS_GROUPREQ);
            put_uint32(s->pktout, s->pbits); /* min */
            put_uint32(s->pktout, s->pbits); /* preferred */
            put_uint32(s->pktout, s->pbits * 2); /* max */
            ssh_pkt_write(ssh, s->pktout);

            crMaybeWaitUntilV(
                (pktin = pq_pop(&ssh->pq_ssh2_transport)) != NULL);
            if (pktin->type != SSH2_MSG_KEXGSS_GROUP) {
                bombout(("expected key exchange group packet from server"));
                crStopV;
            }
            s->p = get_mp_ssh2(pktin);
            s->g = get_mp_ssh2(pktin);
            if (get_err(pktin)) {
                bombout(("unable to read mp-ints from incoming group packet"));
                crStopV;
            }
            ssh->kex_ctx = dh_setup_gex(s->p, s->g);
        } else {
            ssh->kex_ctx = dh_setup_group(ssh->kex);
            logeventf(ssh, "Using GSSAPI (with Kerberos V5) Diffie-Hellman with standard group \"%s\"",
                      ssh->kex->groupname);
        }

        logeventf(ssh, "Doing GSSAPI (with Kerberos V5) Diffie-Hellman key exchange with hash %s",
                  ssh->kex->hash->text_name);
        /* Now generate e for Diffie-Hellman. */
        set_busy_status(ssh->frontend, BUSY_CPU); /* this can take a while */
        s->e = dh_create_e(ssh->kex_ctx, s->nbits * 2);

        if (ssh->gsslib->gsslogmsg)
            logevent(ssh->gsslib->gsslogmsg);

        /* initial tokens are empty */
        SSH_GSS_CLEAR_BUF(&s->gss_rcvtok);
        SSH_GSS_CLEAR_BUF(&s->gss_sndtok);
        SSH_GSS_CLEAR_BUF(&s->mic);
        s->gss_stat = ssh->gsslib->acquire_cred(ssh->gsslib, &s->gss_ctx,
                                                &s->gss_cred_expiry);
        if (s->gss_stat != SSH_GSS_OK) {
            bombout(("GSSAPI key exchange failed to initialize"));
            crStopV;
        }

        /* now enter the loop */
        assert(ssh->gss_srv_name);
        do {
            /*
             * When acquire_cred yields no useful expiration, go with the
             * service ticket expiration.
             */
            s->gss_stat = ssh->gsslib->init_sec_context
                (ssh->gsslib,
                 &s->gss_ctx,
                 ssh->gss_srv_name,
                 s->gss_delegate,
                 &s->gss_rcvtok,
                 &s->gss_sndtok,
                 (s->gss_cred_expiry == GSS_NO_EXPIRATION ?
                  &s->gss_cred_expiry : NULL),
                 NULL);
            SSH_GSS_CLEAR_BUF(&s->gss_rcvtok);

            if (s->gss_stat == SSH_GSS_S_COMPLETE && s->complete_rcvd)
                break; /* MIC is verified after the loop */

            if (s->gss_stat != SSH_GSS_S_COMPLETE &&
                s->gss_stat != SSH_GSS_S_CONTINUE_NEEDED) {
                if (ssh->gsslib->display_status(ssh->gsslib, s->gss_ctx,
                                                &s->gss_buf) == SSH_GSS_OK) {
                    bombout(("GSSAPI key exchange failed to initialize"
                             " context: %s", (char *)s->gss_buf.value));
                    sfree(s->gss_buf.value);
                    crStopV;
                } else {
                    bombout(("GSSAPI key exchange failed to initialize"
                             " context"));
                    crStopV;
                }
            }
            assert(s->gss_stat == SSH_GSS_S_COMPLETE ||
                   s->gss_stat == SSH_GSS_S_CONTINUE_NEEDED);

            if (!s->init_token_sent) {
                s->init_token_sent = 1;
                s->pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_KEXGSS_INIT);
                if (s->gss_sndtok.length == 0) {
                    bombout(("GSSAPI key exchange failed:"
                             " no initial context token"));
                    crStopV;
                }
                put_string(s->pktout,
                           s->gss_sndtok.value, s->gss_sndtok.length);
                put_mp_ssh2(s->pktout, s->e);
                ssh_pkt_write(ssh, s->pktout);
                ssh->gsslib->free_tok(ssh->gsslib, &s->gss_sndtok);
                logevent("GSSAPI key exchange initialised");
            } else if (s->gss_sndtok.length != 0) {
                s->pktout = ssh_bpp_new_pktout(
                    ssh->bpp, SSH2_MSG_KEXGSS_CONTINUE);
                put_string(s->pktout,
                           s->gss_sndtok.value, s->gss_sndtok.length);
                ssh_pkt_write(ssh, s->pktout);
                ssh->gsslib->free_tok(ssh->gsslib, &s->gss_sndtok);
            }

            if (s->gss_stat == SSH_GSS_S_COMPLETE && s->complete_rcvd)
                break;

          wait_for_gss_token:
            crMaybeWaitUntilV(
                (pktin = pq_pop(&ssh->pq_ssh2_transport)) != NULL);
            switch (pktin->type) {
              case SSH2_MSG_KEXGSS_CONTINUE:
                data = get_string(pktin);
                s->gss_rcvtok.value = (char *)data.ptr;
                s->gss_rcvtok.length = data.len;
                continue;
              case SSH2_MSG_KEXGSS_COMPLETE:
                s->complete_rcvd = 1;
                s->f = get_mp_ssh2(pktin);
                data = get_string(pktin);
                s->mic.value = (char *)data.ptr;
                s->mic.length = data.len;
                /* Save expiration time of cred when delegating */
                if (s->gss_delegate && s->gss_cred_expiry != GSS_NO_EXPIRATION)
                    ssh->gss_cred_expiry = s->gss_cred_expiry;
                /* If there's a final token we loop to consume it */
                if (get_bool(pktin)) {
                    data = get_string(pktin);
                    s->gss_rcvtok.value = (char *)data.ptr;
                    s->gss_rcvtok.length = data.len;
                    continue;
                }
                break;
              case SSH2_MSG_KEXGSS_HOSTKEY:
                s->hostkeydata = get_string(pktin);
                if (ssh->hostkey_alg) {
                    s->hkey = ssh_key_new_pub(ssh->hostkey_alg,
                                              s->hostkeydata);
                    put_string(ssh->exhash,
                               s->hostkeydata.ptr, s->hostkeydata.len);
                }
                /*
                 * Can't loop as we have no token to pass to
                 * init_sec_context.
                 */
                goto wait_for_gss_token;
              case SSH2_MSG_KEXGSS_ERROR:
                /*
                 * We have no use for the server's major and minor
                 * status.  The minor status is really only
                 * meaningful to the server, and with luck the major
                 * status means something to us (but not really all
                 * that much).  The string is more meaningful, and
                 * hopefully the server sends any error tokens, as
                 * that will produce the most useful information for
                 * us.
                 */
                get_uint32(pktin); /* server's major status */
                get_uint32(pktin); /* server's minor status */
                data = get_string(pktin);
                logeventf(ssh, "GSSAPI key exchange failed; "
                          "server's message: %.*s", PTRLEN_PRINTF(data));
                /* Language tag, but we have no use for it */
                get_string(pktin);
                /*
                 * Wait for an error token, if there is one, or the
                 * server's disconnect.  The error token, if there
                 * is one, must follow the SSH2_MSG_KEXGSS_ERROR
                 * message, per the RFC.
                 */
                goto wait_for_gss_token;
              default:
                bombout(("unexpected message type during gss kex"));
                crStopV;
                break;
            }
        } while (s->gss_rcvtok.length ||
                 s->gss_stat == SSH_GSS_S_CONTINUE_NEEDED ||
                 !s->complete_rcvd);

        s->K = dh_find_K(ssh->kex_ctx, s->f);

        /* We assume everything from now on will be quick, and it might
         * involve user interaction. */
        set_busy_status(ssh->frontend, BUSY_NOT);

        if (!s->hkey)
            put_stringz(ssh->exhash, "");
        if (dh_is_gex(ssh->kex)) {
            /* min,  preferred, max */
            put_uint32(ssh->exhash, s->pbits);
            put_uint32(ssh->exhash, s->pbits);
            put_uint32(ssh->exhash, s->pbits * 2);

            put_mp_ssh2(ssh->exhash, s->p);
            put_mp_ssh2(ssh->exhash, s->g);
        }
        put_mp_ssh2(ssh->exhash, s->e);
        put_mp_ssh2(ssh->exhash, s->f);

        /*
         * MIC verification is done below, after we compute the hash
         * used as the MIC input.
         */

        dh_cleanup(ssh->kex_ctx);
        freebn(s->f);
        if (dh_is_gex(ssh->kex)) {
            freebn(s->g);
            freebn(s->p);
        }
#endif
    } else {
        ptrlen rsakeydata;

        assert(ssh->kex->main_type == KEXTYPE_RSA);
	logeventf(ssh, "Doing RSA key exchange with hash %s",
		  ssh->kex->hash->text_name);
	ssh->pls.kctx = SSH2_PKTCTX_RSAKEX;
        /*
         * RSA key exchange. First expect a KEXRSA_PUBKEY packet
         * from the server.
         */
        crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_transport)) != NULL);
        if (pktin->type != SSH2_MSG_KEXRSA_PUBKEY) {
            bombout(("expected RSA public key packet from server"));
            crStopV;
        }

        s->hostkeydata = get_string(pktin);
        put_stringpl(ssh->exhash, s->hostkeydata);
	s->hkey = ssh_key_new_pub(ssh->hostkey_alg, s->hostkeydata);

        rsakeydata = get_string(pktin);

        s->rsakey = ssh_rsakex_newkey(rsakeydata.ptr, rsakeydata.len);
        if (!s->rsakey) {
            bombout(("unable to parse RSA public key from server"));
            crStopV;
        }

        put_stringpl(ssh->exhash, rsakeydata);

        /*
         * Next, set up a shared secret K, of precisely KLEN -
         * 2*HLEN - 49 bits, where KLEN is the bit length of the
         * RSA key modulus and HLEN is the bit length of the hash
         * we're using.
         */
        {
            int klen = ssh_rsakex_klen(s->rsakey);
            int nbits = klen - (2*ssh->kex->hash->hlen*8 + 49);
            int i, byte = 0;
            strbuf *buf;
            unsigned char *outstr;
            int outstrlen;

            s->K = bn_power_2(nbits - 1);

            for (i = 0; i < nbits; i++) {
                if ((i & 7) == 0) {
                    byte = random_byte();
                }
                bignum_set_bit(s->K, i, (byte >> (i & 7)) & 1);
            }

            /*
             * Encode this as an mpint.
             */
            buf = strbuf_new();
            put_mp_ssh2(buf, s->K);

            /*
             * Encrypt it with the given RSA key.
             */
            outstrlen = (klen + 7) / 8;
            outstr = snewn(outstrlen, unsigned char);
            ssh_rsakex_encrypt(ssh->kex->hash, buf->u, buf->len,
			       outstr, outstrlen, s->rsakey);

            /*
             * And send it off in a return packet.
             */
            s->pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_KEXRSA_SECRET);
            put_string(s->pktout, outstr, outstrlen);
            ssh_pkt_write(ssh, s->pktout);

            put_string(ssh->exhash, outstr, outstrlen);

            strbuf_free(buf);
            sfree(outstr);
        }

        ssh_rsakex_freekey(s->rsakey);

        crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_transport)) != NULL);
        if (pktin->type != SSH2_MSG_KEXRSA_DONE) {
            bombout(("expected signature packet from server"));
            crStopV;
        }

        s->sigdata = get_string(pktin);
        if (get_err(pktin)) {
            bombout(("unable to parse signature packet"));
            crStopV;
        }
    }

    put_mp_ssh2(ssh->exhash, s->K);
    assert(ssh_hash_alg(ssh->exhash)->hlen <= sizeof(s->exchange_hash));
    ssh_hash_final(ssh->exhash, s->exchange_hash);

#ifndef NO_GSSAPI
    if (ssh->kex->main_type == KEXTYPE_GSS) {
        Ssh_gss_buf gss_buf;
        SSH_GSS_CLEAR_BUF(&s->gss_buf);

        gss_buf.value = s->exchange_hash;
        gss_buf.length = ssh->kex->hash->hlen;
        s->gss_stat = ssh->gsslib->verify_mic(ssh->gsslib, s->gss_ctx, &gss_buf, &s->mic);
        if (s->gss_stat != SSH_GSS_OK) {
            if (ssh->gsslib->display_status(ssh->gsslib, s->gss_ctx,
                                            &s->gss_buf) == SSH_GSS_OK) {
                bombout(("GSSAPI Key Exchange MIC was not valid: %s",
                        (char *)s->gss_buf.value));
                sfree(s->gss_buf.value);
            } else {
                bombout(("GSSAPI Key Exchange MIC was not valid"));
            }
            crStopV;
        }

        ssh->gss_kex_used = TRUE;

        /*-
         * If this the first KEX, save the GSS context for "gssapi-keyex"
         * authentication.
         *
         * http://tools.ietf.org/html/rfc4462#section-4
         *
         * This method may be used only if the initial key exchange was
         * performed using a GSS-API-based key exchange method defined in
         * accordance with Section 2.  The GSS-API context used with this
         * method is always that established during an initial GSS-API-based
         * key exchange.  Any context established during key exchange for the
         * purpose of rekeying MUST NOT be used with this method.
         */
        if (!s->got_session_id) {
            ssh->gss_ctx = s->gss_ctx;
        } else {
            ssh->gsslib->release_cred(ssh->gsslib, &s->gss_ctx);
        }
        logeventf(ssh, "GSSAPI Key Exchange complete!");
    }
#endif

    ssh->kex_ctx = NULL;

#if 0
    debug(("Exchange hash is:\n"));
    dmemdump(s->exchange_hash, ssh->kex->hash->hlen);
#endif

    /* In GSS keyex there's no hostkey signature to verify */
    if (ssh->kex->main_type != KEXTYPE_GSS) {
        if (!s->hkey) {
            bombout(("Server's host key is invalid"));
            crStopV;
        }

        if (!ssh_key_verify(
                s->hkey, s->sigdata,
                make_ptrlen(s->exchange_hash, ssh->kex->hash->hlen))) {
#ifndef FUZZING
            bombout(("Server's host key did not match the signature "
                     "supplied"));
            crStopV;
#endif
        }
    }

    s->keystr = (s->hkey ? ssh_key_cache_str(s->hkey) : NULL);
#ifndef NO_GSSAPI
    if (ssh->gss_kex_used) {
        /*
         * In a GSS-based session, check the host key (if any) against
         * the transient host key cache. See comment above, at the
         * definition of ssh_transient_hostkey_cache_entry.
         */
        if (ssh->kex->main_type == KEXTYPE_GSS) {

            /*
             * We've just done a GSS key exchange. If it gave us a
             * host key, store it.
             */
            if (s->hkey) {
                s->fingerprint = ssh2_fingerprint(s->hkey);
                logevent("GSS kex provided fallback host key:");
                logevent(s->fingerprint);
                sfree(s->fingerprint);
                s->fingerprint = NULL;
                ssh_store_transient_hostkey(ssh, s->hkey);
            } else if (!ssh_have_any_transient_hostkey(ssh)) {
                /*
                 * But if it didn't, then we currently have no
                 * fallback host key to use in subsequent non-GSS
                 * rekeys. So we should immediately trigger a non-GSS
                 * rekey of our own, to set one up, before the session
                 * keys have been used for anything else.
                 *
                 * This is similar to the cross-certification done at
                 * user request in the permanent host key cache, but
                 * here we do it automatically, once, at session
                 * startup, and only add the key to the transient
                 * cache.
                 */
                if (ssh->hostkey_alg) {
                    s->need_gss_transient_hostkey = TRUE;
                } else {
                    /*
                     * If we negotiated the "null" host key algorithm
                     * in the key exchange, that's an indication that
                     * no host key at all is available from the server
                     * (both because we listed "null" last, and
                     * because RFC 4462 section 5 says that a server
                     * MUST NOT offer "null" as a host key algorithm
                     * unless that is the only algorithm it provides
                     * at all).
                     *
                     * In that case we actually _can't_ perform a
                     * non-GSSAPI key exchange, so it's pointless to
                     * attempt one proactively. This is also likely to
                     * cause trouble later if a rekey is required at a
                     * moment whne GSS credentials are not available,
                     * but someone setting up a server in this
                     * configuration presumably accepts that as a
                     * consequence.
                     */
                    if (!s->warned_about_no_gss_transient_hostkey) {
                        logevent("No fallback host key available");
                        s->warned_about_no_gss_transient_hostkey = TRUE;
                    }
                }
            }
        } else {
            /*
             * We've just done a fallback key exchange, so make
             * sure the host key it used is in the cache of keys
             * we previously received in GSS kexes.
             *
             * An exception is if this was the non-GSS key exchange we
             * triggered on purpose to populate the transient cache.
             */
            assert(s->hkey);  /* only KEXTYPE_GSS lets this be null */
            s->fingerprint = ssh2_fingerprint(s->hkey);

            if (s->need_gss_transient_hostkey) {
                logevent("Post-GSS rekey provided fallback host key:");
                logevent(s->fingerprint);
                ssh_store_transient_hostkey(ssh, s->hkey);
                s->need_gss_transient_hostkey = FALSE;
            } else if (!ssh_verify_transient_hostkey(ssh, s->hkey)) {
                logevent("Non-GSS rekey after initial GSS kex "
                         "used host key:");
                logevent(s->fingerprint);
                bombout(("Host key was not previously sent via GSS kex"));
            }

            sfree(s->fingerprint);
            s->fingerprint = NULL;
        }
    } else
#endif /* NO_GSSAPI */
    if (!s->got_session_id) {
	/*
	 * Make a note of any other host key formats that are available.
	 */
	{
	    int i, j, nkeys = 0;
	    char *list = NULL;
	    for (i = 0; i < lenof(hostkey_algs); i++) {
		if (hostkey_algs[i].alg == ssh->hostkey_alg)
		    continue;

                for (j = 0; j < ssh->n_uncert_hostkeys; j++)
                    if (ssh->uncert_hostkeys[j] == i)
                        break;

                if (j < ssh->n_uncert_hostkeys) {
		    char *newlist;
		    if (list)
			newlist = dupprintf("%s/%s", list,
					    hostkey_algs[i].alg->ssh_id);
		    else
			newlist = dupprintf("%s", hostkey_algs[i].alg->ssh_id);
		    sfree(list);
		    list = newlist;
		    nkeys++;
		}
	    }
	    if (list) {
		logeventf(ssh,
			  "Server also has %s host key%s, but we "
			  "don't know %s", list,
			  nkeys > 1 ? "s" : "",
			  nkeys > 1 ? "any of them" : "it");
		sfree(list);
	    }
	}

        /*
         * Authenticate remote host: verify host key. (We've already
         * checked the signature of the exchange hash.)
         */
        s->fingerprint = ssh2_fingerprint(s->hkey);
        logevent("Host key fingerprint is:");
        logevent(s->fingerprint);
        /* First check against manually configured host keys. */
        s->dlgret = verify_ssh_manual_host_key(ssh, s->fingerprint, s->hkey);
        if (s->dlgret == 0) {          /* did not match */
            bombout(("Host key did not appear in manually configured list"));
            crStopV;
        } else if (s->dlgret < 0) { /* none configured; use standard handling */
            ssh_set_frozen(ssh, 1);
            s->dlgret = verify_ssh_host_key(ssh->frontend,
                                            ssh->savedhost, ssh->savedport,
                                            ssh_key_cache_id(s->hkey),
                                            s->keystr, s->fingerprint,
                                            ssh_dialog_callback, ssh);
#ifdef FUZZING
	    s->dlgret = 1;
#endif
	    if (s->dlgret < 0) {
                ssh->user_response = -1;
                crWaitUntilV(ssh->user_response >= 0);
		s->dlgret = ssh->user_response;
	    }
            ssh_set_frozen(ssh, 0);
            if (s->dlgret == 0) {
                ssh_disconnect(ssh, "Aborted at host key verification", NULL,
                               0, TRUE);
                crStopV;
            }
        }
        sfree(s->fingerprint);
        /*
         * Save this host key, to check against the one presented in
         * subsequent rekeys.
         */
        ssh->hostkey_str = s->keystr;
        s->keystr = NULL;
    } else if (ssh->cross_certifying) {
        s->fingerprint = ssh2_fingerprint(s->hkey);
        logevent("Storing additional host key for this host:");
        logevent(s->fingerprint);
        sfree(s->fingerprint);
        store_host_key(ssh->savedhost, ssh->savedport,
                       ssh_key_cache_id(s->hkey), s->keystr);
        ssh->cross_certifying = FALSE;
        /*
         * Don't forget to store the new key as the one we'll be
         * re-checking in future normal rekeys.
         */
        ssh->hostkey_str = s->keystr;
        s->keystr = NULL;
    } else {
        /*
         * In a rekey, we never present an interactive host key
         * verification request to the user. Instead, we simply
         * enforce that the key we're seeing this time is identical to
         * the one we saw before.
         */
        if (strcmp(ssh->hostkey_str, s->keystr)) {
#ifndef FUZZING
            bombout(("Host key was different in repeat key exchange"));
            crStopV;
#endif
        }
    }
    sfree(s->keystr);
    if (s->hkey) {
        ssh_key_free(s->hkey);
        s->hkey = NULL;
    }

    /*
     * The exchange hash from the very first key exchange is also
     * the session id, used in session key construction and
     * authentication.
     */
    if (!s->got_session_id) {
	assert(sizeof(s->exchange_hash) <= sizeof(ssh->v2_session_id));
	memcpy(ssh->v2_session_id, s->exchange_hash,
	       sizeof(s->exchange_hash));
	ssh->v2_session_id_len = ssh->kex->hash->hlen;
	assert(ssh->v2_session_id_len <= sizeof(ssh->v2_session_id));
	s->got_session_id = TRUE;
    }

    /*
     * Send SSH2_MSG_NEWKEYS.
     */
    s->pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_NEWKEYS);
    ssh_pkt_write(ssh, s->pktout);
    ssh->outgoing_data_size = 0;       /* start counting from here */

    /*
     * We've sent client NEWKEYS, so create and initialise
     * client-to-server session keys.
     */
    {
        strbuf *cipher_key = strbuf_new();
        strbuf *cipher_iv = strbuf_new();
        strbuf *mac_key = strbuf_new();

        if (s->out.cipher) {
            ssh2_mkkey(ssh, cipher_iv, s->K, s->exchange_hash, 'A',
                       s->out.cipher->blksize);
            ssh2_mkkey(ssh, cipher_key, s->K, s->exchange_hash, 'C',
                       s->out.cipher->padded_keybytes);
        }
        if (s->out.mac) {
            ssh2_mkkey(ssh, mac_key, s->K, s->exchange_hash, 'E',
                       s->out.mac->keylen);
        }

        ssh2_bpp_new_outgoing_crypto(
            ssh->bpp,
            s->out.cipher, cipher_key->u, cipher_iv->u,
            s->out.mac, s->out.etm_mode, mac_key->u,
            s->out.comp);

        /*
         * Remember some details we'll need later for making other
         * policy decisions based on the crypto we've just
         * initialised.
         */
        ssh->v2_cbc_ignore_workaround = (
            s->out.cipher &&
            (s->out.cipher->flags & SSH_CIPHER_IS_CBC) &&
            !(ssh->remote_bugs & BUG_CHOKES_ON_SSH2_IGNORE));
        ssh->v2_out_cipherblksize = s->out.cipher->blksize;

        strbuf_free(cipher_key);
        strbuf_free(cipher_iv);
        strbuf_free(mac_key);
    }

    if (s->out.cipher)
        logeventf(ssh, "Initialised %.200s client->server encryption",
                  s->out.cipher->text_name);
    if (s->out.mac)
        logeventf(ssh, "Initialised %.200s client->server"
                  " MAC algorithm%s%s",
                  s->out.mac->text_name,
                  s->out.etm_mode ? " (in ETM mode)" : "",
                  (s->out.cipher->required_mac ?
                   " (required by cipher)" : ""));
    if (s->out.comp->text_name)
        logeventf(ssh, "Initialised %s compression",
                  s->out.comp->text_name);

    /*
     * Now our end of the key exchange is complete, we can send all
     * our queued higher-layer packets.
     */
    ssh->queueing = FALSE;
    ssh2_pkt_queuesend(ssh);

    /*
     * Expect SSH2_MSG_NEWKEYS from server.
     */
    crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_transport)) != NULL);
    if (pktin->type != SSH2_MSG_NEWKEYS) {
	bombout(("expected new-keys packet from server"));
	crStopV;
    }
    ssh->incoming_data_size = 0;       /* start counting from here */

    /*
     * We've seen server NEWKEYS, so create and initialise
     * server-to-client session keys.
     */
    {
        strbuf *cipher_key = strbuf_new();
        strbuf *cipher_iv = strbuf_new();
        strbuf *mac_key = strbuf_new();

        if (s->in.cipher) {
            ssh2_mkkey(ssh, cipher_iv, s->K, s->exchange_hash, 'B',
                       s->in.cipher->blksize);
            ssh2_mkkey(ssh, cipher_key, s->K, s->exchange_hash, 'D',
                       s->in.cipher->padded_keybytes);
        }
        if (s->in.mac) {
            ssh2_mkkey(ssh, mac_key, s->K, s->exchange_hash, 'F',
                       s->in.mac->keylen);
        }

        ssh2_bpp_new_incoming_crypto(
            ssh->bpp,
            s->in.cipher, cipher_key->u, cipher_iv->u,
            s->in.mac, s->in.etm_mode, mac_key->u,
            s->in.comp);

        strbuf_free(cipher_key);
        strbuf_free(cipher_iv);
        strbuf_free(mac_key);
    }

    if (s->in.cipher)
        logeventf(ssh, "Initialised %.200s server->client encryption",
                  s->in.cipher->text_name);
    if (s->in.mac)
        logeventf(ssh, "Initialised %.200s server->client"
                  " MAC algorithm%s%s",
                  s->in.mac->text_name,
                  s->in.etm_mode ? " (in ETM mode)" : "",
                  (s->in.cipher->required_mac ?
                   " (required by cipher)" : ""));
    if (s->in.comp->text_name)
        logeventf(ssh, "Initialised %s decompression",
                  s->in.comp->text_name);

    /*
     * Free shared secret.
     */
    freebn(s->K);

    /*
     * Update the specials menu to list the remaining uncertified host
     * keys.
     */
    update_specials_menu(ssh->frontend);

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
    (void) ssh2_timer_update(ssh, 0);

    /*
     * Now we're encrypting. Get the next-layer protocol started if it
     * hasn't already, and then sit here waiting for reasons to go
     * back to the start and do a repeat key exchange. One of those
     * reasons is that we receive KEXINIT from the other end; the
     * other is if we find ssh->rekey_reason is non-NULL, i.e. we've
     * decided to initiate a rekey ourselves for some reason.
     */
    ssh->rekey_class = RK_NONE;
    while (!pq_peek(&ssh->pq_ssh2_transport) && !ssh->rekey_class) {
        wait_for_rekey:
	if (!ssh->current_user_input_fn) {
	    /*
	     * Allow userauth to initialise itself.
	     */
	    do_ssh2_userauth(ssh);
	    ssh->current_user_input_fn = ssh2_userauth_input;
	}
	crReturnV;
    }
    if ((pktin = pq_pop(&ssh->pq_ssh2_transport)) != NULL) {
        if (pktin->type != SSH2_MSG_KEXINIT) {
            bombout(("unexpected key exchange packet, type %d", pktin->type));
            crStopV;
        }
	logevent("Server initiated key re-exchange");
    } else {
	if (ssh->rekey_class == RK_POST_USERAUTH) {
	    /* 
             * userauth has seen a USERAUTH_SUCCEEDED. For a couple of
             * reasons, this may be the moment to do an immediate
             * rekey with different parameters.
	     *
             * One is to turn on delayed compression. We do this by a
             * rekey to work around a protocol design bug:
	     * draft-miller-secsh-compression-delayed-00 says that you
             * negotiate delayed compression in the first key
             * exchange, and both sides start compressing when the
             * server has sent USERAUTH_SUCCESS. This has a race
             * condition -- the server can't know when the client has
             * seen it, and thus which incoming packets it should
             * treat as compressed.
	     *
             * Instead, we do the initial key exchange without
             * offering the delayed methods, but note if the server
             * offers them; when we get here, if a delayed method was
             * available that was higher on our list than what we got,
             * we initiate a rekey in which we _do_ list the delayed
             * methods (and hopefully get it as a result). Subsequent
             * rekeys will do the same.
             *
             * Another reason for a rekey at this point is if we've
             * done a GSS key exchange and don't have anything in our
             * transient hostkey cache, in which case we should make
             * an attempt to populate the cache now.
	     */
	    assert(!s->userauth_succeeded); /* should only happen once */
	    s->userauth_succeeded = TRUE;
            if (s->pending_compression) {
                ssh->rekey_reason = "enabling delayed compression";
            } else if (s->need_gss_transient_hostkey) {
                ssh->rekey_reason = "populating transient host key cache";
            } else {
		/* Can't see any point rekeying. */
		goto wait_for_rekey;       /* this is utterly horrid */
            }
	    /* else fall through to rekey... */

	    s->pending_compression = FALSE;
	}
        /*
	 * Now we've decided to rekey.
	 *
         * Special case: if the server bug is set that doesn't
         * allow rekeying, we give a different log message and
         * continue waiting. (If such a server _initiates_ a rekey,
         * we process it anyway!)
         */
        if ((ssh->remote_bugs & BUG_SSH2_REKEY)) {
            logeventf(ssh, "Server bug prevents key re-exchange (%s)",
                      ssh->rekey_reason);
            /* Reset the counters, so that at least this message doesn't
             * hit the event log _too_ often. */
            ssh->outgoing_data_size = 0;
            ssh->incoming_data_size = 0;
            (void) ssh2_timer_update(ssh, 0);
            goto wait_for_rekey;       /* this is still utterly horrid */
        } else {
            logeventf(ssh, "Initiating key re-exchange (%s)",
                      ssh->rekey_reason);
        }
    }
    goto begin_key_exchange;

    crFinishV;
}

/*
 * Send data on an SSH channel.  In SSH-2, this involves buffering it
 * first.
 */
static int ssh_send_channel_data(struct ssh_channel *c, const char *buf,
				   int len)
{
    assert(!(c->closes & CLOSES_SENT_EOF));

    if (c->ssh->version == 2) {
	bufchain_add(&c->v.v2.outbuffer, buf, len);
	return ssh2_try_send(c);
    } else {
        PktOut *pkt = ssh_bpp_new_pktout(c->ssh->bpp, SSH1_MSG_CHANNEL_DATA);
        put_uint32(pkt, c->remoteid);
        put_string(pkt, buf, len);
        ssh_pkt_write(c->ssh, pkt);
	/*
	 * In SSH-1 we can return 0 here - implying that channels are
	 * never individually throttled - because the only
	 * circumstance that can cause throttling will be the whole
	 * SSH connection backing up, in which case _everything_ will
	 * be throttled as a whole.
	 */
	return 0;
    }
}

/*
 * Attempt to send data on an SSH-2 channel.
 */
static int ssh2_try_send(struct ssh_channel *c)
{
    Ssh ssh = c->ssh;
    PktOut *pktout;
    int ret;

    while (c->v.v2.remwindow > 0 && bufchain_size(&c->v.v2.outbuffer) > 0) {
	int len;
	void *data;
	bufchain_prefix(&c->v.v2.outbuffer, &data, &len);
	if ((unsigned)len > c->v.v2.remwindow)
	    len = c->v.v2.remwindow;
	if ((unsigned)len > c->v.v2.remmaxpkt)
	    len = c->v.v2.remmaxpkt;
	pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_CHANNEL_DATA);
	put_uint32(pktout, c->remoteid);
        put_string(pktout, data, len);
	ssh2_pkt_send(ssh, pktout);
        if (!ssh->s)   /* a network error might have closed the socket */
            break;
	bufchain_consume(&c->v.v2.outbuffer, len);
	c->v.v2.remwindow -= len;
    }

    /*
     * After having sent as much data as we can, return the amount
     * still buffered.
     */
    ret = bufchain_size(&c->v.v2.outbuffer);

    /*
     * And if there's no data pending but we need to send an EOF, send
     * it.
     */
    if (!ret && c->pending_eof)
        ssh_channel_try_eof(c);

    return ret;
}

static void ssh2_try_send_and_unthrottle(Ssh ssh, struct ssh_channel *c)
{
    int bufsize;
    if (c->closes & CLOSES_SENT_EOF)
	return;                   /* don't send on channels we've EOFed */
    bufsize = ssh2_try_send(c);
    if (bufsize == 0) {
        c->throttled_by_backlog = FALSE;
        ssh_channel_check_throttle(c);
    }
}

static int ssh_is_simple(Ssh ssh)
{
    /*
     * We use the 'simple' variant of the SSH protocol if we're asked
     * to, except not if we're also doing connection-sharing (either
     * tunnelling our packets over an upstream or expecting to be
     * tunnelled over ourselves), since then the assumption that we
     * have only one channel to worry about is not true after all.
     */
    return (conf_get_int(ssh->conf, CONF_ssh_simple) &&
            !ssh->bare_connection && !ssh->connshare);
}

/*
 * Set up most of a new ssh_channel. Nulls out sharectx, but leaves
 * chan untouched (since it will sometimes have been filled in before
 * calling this).
 */
static void ssh_channel_init(struct ssh_channel *c)
{
    Ssh ssh = c->ssh;
    c->localid = alloc_channel_id(ssh);
    c->closes = 0;
    c->pending_eof = FALSE;
    c->throttling_conn = FALSE;
    c->sharectx = NULL;
    if (ssh->version == 2) {
	c->v.v2.locwindow = c->v.v2.locmaxwin = c->v.v2.remlocwin =
	    ssh_is_simple(ssh) ? OUR_V2_BIGWIN : OUR_V2_WINSIZE;
	c->v.v2.chanreq_head = NULL;
	c->v.v2.throttle_state = UNTHROTTLED;
	bufchain_init(&c->v.v2.outbuffer);
    }
    add234(ssh->channels, c);
}

/*
 * Construct the common parts of a CHANNEL_OPEN.
 */
static PktOut *ssh2_chanopen_init(struct ssh_channel *c,
                                         const char *type)
{
    PktOut *pktout;

    pktout = ssh_bpp_new_pktout(c->ssh->bpp, SSH2_MSG_CHANNEL_OPEN);
    put_stringz(pktout, type);
    put_uint32(pktout, c->localid);
    put_uint32(pktout, c->v.v2.locwindow);/* our window size */
    put_uint32(pktout, OUR_V2_MAXPKT);      /* our max pkt size */
    return pktout;
}

/*
 * CHANNEL_FAILURE doesn't come with any indication of what message
 * caused it, so we have to keep track of the outstanding
 * CHANNEL_REQUESTs ourselves.
 */
static void ssh2_queue_chanreq_handler(struct ssh_channel *c,
				       cchandler_fn_t handler, void *ctx)
{
    struct outstanding_channel_request *ocr =
	snew(struct outstanding_channel_request);

    assert(!(c->closes & (CLOSES_SENT_CLOSE | CLOSES_RCVD_CLOSE)));
    ocr->handler = handler;
    ocr->ctx = ctx;
    ocr->next = NULL;
    if (!c->v.v2.chanreq_head)
	c->v.v2.chanreq_head = ocr;
    else
	c->v.v2.chanreq_tail->next = ocr;
    c->v.v2.chanreq_tail = ocr;
}

/*
 * Construct the common parts of a CHANNEL_REQUEST.  If handler is not
 * NULL then a reply will be requested and the handler will be called
 * when it arrives.  The returned packet is ready to have any
 * request-specific data added and be sent.  Note that if a handler is
 * provided, it's essential that the request actually be sent.
 *
 * The handler will usually be passed the response packet in pktin. If
 * pktin is NULL, this means that no reply will ever be forthcoming
 * (e.g. because the entire connection is being destroyed, or because
 * the server initiated channel closure before we saw the response)
 * and the handler should free any storage it's holding.
 */
static PktOut *ssh2_chanreq_init(struct ssh_channel *c,
                                        const char *type,
					cchandler_fn_t handler, void *ctx)
{
    PktOut *pktout;

    assert(!(c->closes & (CLOSES_SENT_CLOSE | CLOSES_RCVD_CLOSE)));
    pktout = ssh_bpp_new_pktout(c->ssh->bpp, SSH2_MSG_CHANNEL_REQUEST);
    put_uint32(pktout, c->remoteid);
    put_stringz(pktout, type);
    put_bool(pktout, handler != NULL);
    if (handler != NULL)
	ssh2_queue_chanreq_handler(c, handler, ctx);
    return pktout;
}

static void ssh_channel_unthrottle(struct ssh_channel *c, int bufsize)
{
    Ssh ssh = c->ssh;
    int buflimit;

    if (ssh->version == 1) {
	buflimit = SSH1_BUFFER_LIMIT;
    } else {
	if (ssh_is_simple(ssh))
	    buflimit = 0;
	else
	    buflimit = c->v.v2.locmaxwin;
	if (bufsize < buflimit)
	    ssh2_set_window(c, buflimit - bufsize);
    }
    if (c->throttling_conn && bufsize <= buflimit) {
	c->throttling_conn = 0;
	ssh_throttle_conn(ssh, -1);
    }
}

/*
 * Potentially enlarge the window on an SSH-2 channel.
 */
static void ssh2_handle_winadj_response(struct ssh_channel *, PktIn *,
					void *);
static void ssh2_set_window(struct ssh_channel *c, int newwin)
{
    Ssh ssh = c->ssh;

    /*
     * Never send WINDOW_ADJUST for a channel that the remote side has
     * already sent EOF on; there's no point, since it won't be
     * sending any more data anyway. Ditto if _we've_ already sent
     * CLOSE.
     */
    if (c->closes & (CLOSES_RCVD_EOF | CLOSES_SENT_CLOSE))
	return;

    /*
     * If the client-side Channel is in an initial setup phase with a
     * fixed window size, e.g. for an X11 channel when we're still
     * waiting to see its initial auth and may yet hand it off to a
     * downstream, don't send any WINDOW_ADJUST either.
     */
    if (c->chan->initial_fixed_window_size)
        return;

    /*
     * If the remote end has a habit of ignoring maxpkt, limit the
     * window so that it has no choice (assuming it doesn't ignore the
     * window as well).
     */
    if ((ssh->remote_bugs & BUG_SSH2_MAXPKT) && newwin > OUR_V2_MAXPKT)
	newwin = OUR_V2_MAXPKT;

    /*
     * Only send a WINDOW_ADJUST if there's significantly more window
     * available than the other end thinks there is.  This saves us
     * sending a WINDOW_ADJUST for every character in a shell session.
     *
     * "Significant" is arbitrarily defined as half the window size.
     */
    if (newwin / 2 >= c->v.v2.locwindow) {
	PktOut *pktout;
	unsigned *up;

	/*
	 * In order to keep track of how much window the client
	 * actually has available, we'd like it to acknowledge each
	 * WINDOW_ADJUST.  We can't do that directly, so we accompany
	 * it with a CHANNEL_REQUEST that has to be acknowledged.
	 *
	 * This is only necessary if we're opening the window wide.
	 * If we're not, then throughput is being constrained by
	 * something other than the maximum window size anyway.
	 */
	if (newwin == c->v.v2.locmaxwin &&
            !(ssh->remote_bugs & BUG_CHOKES_ON_WINADJ)) {
	    up = snew(unsigned);
	    *up = newwin - c->v.v2.locwindow;
	    pktout = ssh2_chanreq_init(c, "winadj@putty.projects.tartarus.org",
				       ssh2_handle_winadj_response, up);
	    ssh2_pkt_send(ssh, pktout);

	    if (c->v.v2.throttle_state != UNTHROTTLED)
		c->v.v2.throttle_state = UNTHROTTLING;
	} else {
	    /* Pretend the WINDOW_ADJUST was acked immediately. */
	    c->v.v2.remlocwin = newwin;
	    c->v.v2.throttle_state = THROTTLED;
	}
	pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_CHANNEL_WINDOW_ADJUST);
	put_uint32(pktout, c->remoteid);
	put_uint32(pktout, newwin - c->v.v2.locwindow);
	ssh2_pkt_send(ssh, pktout);
	c->v.v2.locwindow = newwin;
    }
}

/*
 * Find the channel associated with a message.  If there's no channel,
 * or it's not properly open, make a noise about it and return NULL.
 * If the channel is shared, pass the message on to downstream and
 * also return NULL (meaning the caller should ignore this message).
 */
static struct ssh_channel *ssh_channel_msg(Ssh ssh, PktIn *pktin)
{
    unsigned localid = get_uint32(pktin);
    struct ssh_channel *c;
    int halfopen_ok;

    /* Is this message OK on a half-open connection? */
    if (ssh->version == 1)
	halfopen_ok = (pktin->type == SSH1_MSG_CHANNEL_OPEN_CONFIRMATION ||
		       pktin->type == SSH1_MSG_CHANNEL_OPEN_FAILURE);
    else
	halfopen_ok = (pktin->type == SSH2_MSG_CHANNEL_OPEN_CONFIRMATION ||
		       pktin->type == SSH2_MSG_CHANNEL_OPEN_FAILURE);
    c = find234(ssh->channels, &localid, ssh_channelfind);
    if (c && c->sharectx) {
        share_got_pkt_from_server(c->sharectx, pktin->type,
                                  BinarySource_UPCAST(pktin)->data,
                                  BinarySource_UPCAST(pktin)->len);
        return NULL;
    }
    if (!c || c->halfopen != halfopen_ok) {
	char *buf = dupprintf("Received %s for %s channel %u",
			      ssh_pkt_type(ssh, pktin->type),
			      !c ? "nonexistent" :
			      c->halfopen ? "half-open" : "open",
			      localid);
	ssh_disconnect(ssh, NULL, buf, SSH2_DISCONNECT_PROTOCOL_ERROR, FALSE);
	sfree(buf);
	return NULL;
    }
    return c;
}

static void ssh2_handle_winadj_response(struct ssh_channel *c,
					PktIn *pktin, void *ctx)
{
    unsigned *sizep = ctx;

    /*
     * Winadj responses should always be failures. However, at least
     * one server ("boks_sshd") is known to return SUCCESS for channel
     * requests it's never heard of, such as "winadj@putty". Raised
     * with foxt.com as bug 090916-090424, but for the sake of a quiet
     * life, we don't worry about what kind of response we got.
     */

    c->v.v2.remlocwin += *sizep;
    sfree(sizep);
    /*
     * winadj messages are only sent when the window is fully open, so
     * if we get an ack of one, we know any pending unthrottle is
     * complete.
     */
    if (c->v.v2.throttle_state == UNTHROTTLING)
	c->v.v2.throttle_state = UNTHROTTLED;
}

static void ssh2_msg_channel_response(Ssh ssh, PktIn *pktin)
{
    struct ssh_channel *c = ssh_channel_msg(ssh, pktin);
    struct outstanding_channel_request *ocr;

    if (!c) return;
    ocr = c->v.v2.chanreq_head;
    if (!ocr) {
	ssh2_msg_unexpected(ssh, pktin);
	return;
    }
    ocr->handler(c, pktin, ocr->ctx);
    if (ssh->state == SSH_STATE_CLOSED)
        return; /* in case the handler called bomb_out(), which some can */
    c->v.v2.chanreq_head = ocr->next;
    sfree(ocr);
    /*
     * We may now initiate channel-closing procedures, if that
     * CHANNEL_REQUEST was the last thing outstanding before we send
     * CHANNEL_CLOSE.
     */
    ssh2_channel_check_close(c);
}

static void ssh2_msg_channel_window_adjust(Ssh ssh, PktIn *pktin)
{
    struct ssh_channel *c;
    c = ssh_channel_msg(ssh, pktin);
    if (!c)
	return;
    if (!(c->closes & CLOSES_SENT_EOF)) {
	c->v.v2.remwindow += get_uint32(pktin);
	ssh2_try_send_and_unthrottle(ssh, c);
    }
}

static void ssh2_msg_channel_data(Ssh ssh, PktIn *pktin)
{
    ptrlen data;
    unsigned ext_type = 0; /* 0 means not extended */
    struct ssh_channel *c;
    c = ssh_channel_msg(ssh, pktin);
    if (!c)
	return;
    if (pktin->type == SSH2_MSG_CHANNEL_EXTENDED_DATA)
	ext_type = get_uint32(pktin);
    data = get_string(pktin);
    if (!get_err(pktin)) {
	int bufsize;
	c->v.v2.locwindow -= data.len;
	c->v.v2.remlocwin -= data.len;
	if (ext_type != 0 && ext_type != SSH2_EXTENDED_DATA_STDERR)
	    data.len = 0; /* Don't do anything with unknown extended data. */
	bufsize = ssh_channel_data(c, ext_type == SSH2_EXTENDED_DATA_STDERR,
				   data.ptr, data.len);
	/*
	 * If it looks like the remote end hit the end of its window,
	 * and we didn't want it to do that, think about using a
	 * larger window.
	 */
	if (c->v.v2.remlocwin <= 0 && c->v.v2.throttle_state == UNTHROTTLED &&
	    c->v.v2.locmaxwin < 0x40000000)
	    c->v.v2.locmaxwin += OUR_V2_WINSIZE;
	/*
	 * If we are not buffering too much data,
	 * enlarge the window again at the remote side.
	 * If we are buffering too much, we may still
	 * need to adjust the window if the server's
	 * sent excess data.
	 */
	if (bufsize < c->v.v2.locmaxwin)
	    ssh2_set_window(c, c->v.v2.locmaxwin - bufsize);
	/*
	 * If we're either buffering way too much data, or if we're
	 * buffering anything at all and we're in "simple" mode,
	 * throttle the whole channel.
	 */
	if ((bufsize > c->v.v2.locmaxwin || (ssh_is_simple(ssh) && bufsize>0))
            && !c->throttling_conn) {
	    c->throttling_conn = 1;
	    ssh_throttle_conn(ssh, +1);
	}
    }
}

static void ssh_check_termination(Ssh ssh)
{
    if (ssh->version == 2 &&
        !conf_get_int(ssh->conf, CONF_ssh_no_shell) &&
        (ssh->channels && count234(ssh->channels) == 0) &&
        !(ssh->connshare && share_ndownstreams(ssh->connshare) > 0)) {
        /*
         * We used to send SSH_MSG_DISCONNECT here, because I'd
         * believed that _every_ conforming SSH-2 connection had to
         * end with a disconnect being sent by at least one side;
         * apparently I was wrong and it's perfectly OK to
         * unceremoniously slam the connection shut when you're done,
         * and indeed OpenSSH feels this is more polite than sending a
         * DISCONNECT. So now we don't.
         */
        ssh_disconnect(ssh, "All channels closed", NULL, 0, TRUE);
    }
}

void ssh_sharing_downstream_connected(Ssh ssh, unsigned id,
                                      const char *peerinfo)
{
    if (peerinfo)
        logeventf(ssh, "Connection sharing downstream #%u connected from %s",
                  id, peerinfo);
    else
        logeventf(ssh, "Connection sharing downstream #%u connected", id);
}

void ssh_sharing_downstream_disconnected(Ssh ssh, unsigned id)
{
    logeventf(ssh, "Connection sharing downstream #%u disconnected", id);
    ssh_check_termination(ssh);
}

void ssh_sharing_logf(Ssh ssh, unsigned id, const char *logfmt, ...)
{
    va_list ap;
    char *buf;

    va_start(ap, logfmt);
    buf = dupvprintf(logfmt, ap);
    va_end(ap);
    if (id)
        logeventf(ssh, "Connection sharing downstream #%u: %s", id, buf);
    else
        logeventf(ssh, "Connection sharing: %s", buf);
    sfree(buf);
}

/*
 * Close any local socket and free any local resources associated with
 * a channel.  This converts the channel into a zombie.
 */
static void ssh_channel_close_local(struct ssh_channel *c, char const *reason)
{
    Ssh ssh = c->ssh;
    const char *msg = NULL;

    if (c->sharectx)
        return;

    msg = chan_log_close_msg(c->chan);
    chan_free(c->chan);
    c->chan = zombiechan_new();

    if (msg != NULL) {
	if (reason != NULL)
	    logeventf(ssh, "%s %s", msg, reason);
	else
	    logevent(msg);
    }
}

static void ssh_channel_destroy(struct ssh_channel *c)
{
    Ssh ssh = c->ssh;

    ssh_channel_close_local(c, NULL);

    del234(ssh->channels, c);
    if (ssh->version == 2) {
        bufchain_clear(&c->v.v2.outbuffer);
	assert(c->v.v2.chanreq_head == NULL);
    }
    sfree(c);

    /*
     * If that was the last channel left open, we might need to
     * terminate.
     */
    ssh_check_termination(ssh);
}

static void ssh2_channel_check_close(struct ssh_channel *c)
{
    Ssh ssh = c->ssh;
    PktOut *pktout;

    assert(ssh->version == 2);
    if (c->halfopen) {
        /*
         * If we've sent out our own CHANNEL_OPEN but not yet seen
         * either OPEN_CONFIRMATION or OPEN_FAILURE in response, then
         * it's too early to be sending close messages of any kind.
         */
        return;
    }

    if ((!((CLOSES_SENT_EOF | CLOSES_RCVD_EOF) & ~c->closes) ||
         chan_want_close(c->chan, (c->closes & CLOSES_SENT_EOF),
                         (c->closes & CLOSES_RCVD_EOF))) &&
	!c->v.v2.chanreq_head &&
	!(c->closes & CLOSES_SENT_CLOSE)) {
        /*
         * We have both sent and received EOF (or the channel is a
         * zombie), and we have no outstanding channel requests, which
         * means the channel is in final wind-up. But we haven't sent
         * CLOSE, so let's do so now.
         */
	pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_CHANNEL_CLOSE);
	put_uint32(pktout, c->remoteid);
	ssh2_pkt_send(ssh, pktout);
        c->closes |= CLOSES_SENT_EOF | CLOSES_SENT_CLOSE;
    }

    if (!((CLOSES_SENT_CLOSE | CLOSES_RCVD_CLOSE) & ~c->closes)) {
	assert(c->v.v2.chanreq_head == NULL);
        /*
         * We have both sent and received CLOSE, which means we're
         * completely done with the channel.
         */
        ssh_channel_destroy(c);
    }
}

static void ssh_channel_got_eof(struct ssh_channel *c)
{
    if (c->closes & CLOSES_RCVD_EOF)
        return;                        /* already seen EOF */
    c->closes |= CLOSES_RCVD_EOF;

    chan_send_eof(c->chan);
}

static void ssh2_msg_channel_eof(Ssh ssh, PktIn *pktin)
{
    struct ssh_channel *c;

    c = ssh_channel_msg(ssh, pktin);
    if (!c)
	return;
    ssh_channel_got_eof(c);
    ssh2_channel_check_close(c);
}

static void ssh2_msg_channel_close(Ssh ssh, PktIn *pktin)
{
    struct ssh_channel *c;

    c = ssh_channel_msg(ssh, pktin);
    if (!c)
	return;

    /*
     * When we receive CLOSE on a channel, we assume it comes with an
     * implied EOF if we haven't seen EOF yet.
     */
    ssh_channel_got_eof(c);

    if (!(ssh->remote_bugs & BUG_SENDS_LATE_REQUEST_REPLY)) {
        /*
         * It also means we stop expecting to see replies to any
         * outstanding channel requests, so clean those up too.
         * (ssh_chanreq_init will enforce by assertion that we don't
         * subsequently put anything back on this list.)
         */
        while (c->v.v2.chanreq_head) {
            struct outstanding_channel_request *ocr = c->v.v2.chanreq_head;
            ocr->handler(c, NULL, ocr->ctx);
            c->v.v2.chanreq_head = ocr->next;
            sfree(ocr);
        }
    }

    /*
     * And we also send an outgoing EOF, if we haven't already, on the
     * assumption that CLOSE is a pretty forceful announcement that
     * the remote side is doing away with the entire channel. (If it
     * had wanted to send us EOF and continue receiving data from us,
     * it would have just sent CHANNEL_EOF.)
     */
    if (!(c->closes & CLOSES_SENT_EOF)) {
        /*
         * Abandon any buffered data we still wanted to send to this
         * channel. Receiving a CHANNEL_CLOSE is an indication that
         * the server really wants to get on and _destroy_ this
         * channel, and it isn't going to send us any further
         * WINDOW_ADJUSTs to permit us to send pending stuff.
         */
        bufchain_clear(&c->v.v2.outbuffer);

        /*
         * Send outgoing EOF.
         */
        sshfwd_write_eof(c);

        /*
         * Make sure we don't read any more from whatever our local
         * data source is for this channel. (This will pick up on the
         * changes made by sshfwd_write_eof.)
         */
        ssh_channel_check_throttle(c);
    }

    /*
     * Now process the actual close.
     */
    if (!(c->closes & CLOSES_RCVD_CLOSE)) {
        c->closes |= CLOSES_RCVD_CLOSE;
        ssh2_channel_check_close(c);
    }
}

static void ssh2_msg_channel_open_confirmation(Ssh ssh, PktIn *pktin)
{
    struct ssh_channel *c;

    c = ssh_channel_msg(ssh, pktin);
    if (!c)
	return;
    assert(c->halfopen); /* ssh_channel_msg will have enforced this */
    c->remoteid = get_uint32(pktin);
    c->halfopen = FALSE;
    c->v.v2.remwindow = get_uint32(pktin);
    c->v.v2.remmaxpkt = get_uint32(pktin);

    chan_open_confirmation(c->chan);

    /*
     * Now that the channel is fully open, it's possible in principle
     * to immediately close it. Check whether it wants us to!
     *
     * This can occur if a local socket error occurred between us
     * sending out CHANNEL_OPEN and receiving OPEN_CONFIRMATION. If
     * that happens, all we can do is immediately initiate close
     * proceedings now that we know the server's id to put in the
     * close message. We'll have handled that in this code by having
     * already turned c->chan into a zombie, so its want_close method
     * (which ssh2_channel_check_close will consult) will already be
     * returning TRUE.
     */
    ssh2_channel_check_close(c);

    if (c->pending_eof)
        ssh_channel_try_eof(c);        /* in case we had a pending EOF */
}

static char *ssh2_channel_open_failure_error_text(PktIn *pktin)
{
    static const char *const reasons[] = {
        NULL,
        "Administratively prohibited",
        "Connect failed",
        "Unknown channel type",
        "Resource shortage",
    };
    unsigned reason_code;
    const char *reason_code_string;
    char reason_code_buf[256];
    ptrlen reason;

    reason_code = get_uint32(pktin);
    if (reason_code < lenof(reasons) && reasons[reason_code]) {
        reason_code_string = reasons[reason_code];
    } else {
        reason_code_string = reason_code_buf;
        sprintf(reason_code_buf, "unknown reason code %#x", reason_code);
    }

    reason = get_string(pktin);

    return dupprintf("%s [%.*s]", reason_code_string, PTRLEN_PRINTF(reason));
}

static void ssh2_msg_channel_open_failure(Ssh ssh, PktIn *pktin)
{
    struct ssh_channel *c;

    c = ssh_channel_msg(ssh, pktin);
    if (!c)
	return;
    assert(c->halfopen); /* ssh_channel_msg will have enforced this */

    {
        char *errtext = ssh2_channel_open_failure_error_text(pktin);
        chan_open_failed(c->chan, errtext);
        sfree(errtext);
    }
    chan_free(c->chan);

    del234(ssh->channels, c);
    sfree(c);
}

static void ssh2_msg_channel_request(Ssh ssh, PktIn *pktin)
{
    ptrlen type;
    int want_reply;
    int reply = SSH2_MSG_CHANNEL_FAILURE; /* default */
    struct ssh_channel *c;
    PktOut *pktout;

    c = ssh_channel_msg(ssh, pktin);
    if (!c)
	return;
    type = get_string(pktin);
    want_reply = get_bool(pktin);

    if (c->closes & CLOSES_SENT_CLOSE) {
        /*
         * We don't reply to channel requests after we've sent
         * CHANNEL_CLOSE for the channel, because our reply might
         * cross in the network with the other side's CHANNEL_CLOSE
         * and arrive after they have wound the channel up completely.
         */
        want_reply = FALSE;
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
	if (ptrlen_eq_string(type, "exit-status")) {

	    ssh->exitcode = get_uint32(pktin);
	    logeventf(ssh, "Server sent command exit status %d",
		      ssh->exitcode);
	    reply = SSH2_MSG_CHANNEL_SUCCESS;

	} else if (ptrlen_eq_string(type, "exit-signal")) {
            char *fmt_sig = NULL, *fmt_msg = NULL;
            ptrlen errmsg;
            int core = FALSE;
            int format, exitcode;

            /* ICK: older versions of OpenSSH (e.g. 3.4p1)
	     * provide an `int' for the signal, despite its
	     * having been a `string' in the drafts of RFC 4254 since at
	     * least 2001. (Fixed in session.c 1.147.) Try to
	     * infer which we can safely parse it as. */

            size_t startpos = BinarySource_UPCAST(pktin)->pos;

            for (format = 0; format < 2; format++) {
                BinarySource_UPCAST(pktin)->pos = startpos;
                BinarySource_UPCAST(pktin)->err = BSE_NO_ERROR;

                if (format == 0) { /* standard string-based format */
                    ptrlen signame = get_string(pktin);
                    fmt_sig = dupprintf(" \"%.*s\"", PTRLEN_PRINTF(signame));

		    /*
		     * Really hideous method of translating the
		     * signal description back into a locally
		     * meaningful number.
		     */

		    if (0)
			;
#define TRANSLATE_SIGNAL(s) \
    else if (ptrlen_eq_string(signame, #s)) \
        exitcode = 128 + SIG ## s
#ifdef SIGABRT
		    TRANSLATE_SIGNAL(ABRT);
#endif
#ifdef SIGALRM
		    TRANSLATE_SIGNAL(ALRM);
#endif
#ifdef SIGFPE
		    TRANSLATE_SIGNAL(FPE);
#endif
#ifdef SIGHUP
		    TRANSLATE_SIGNAL(HUP);
#endif
#ifdef SIGILL
		    TRANSLATE_SIGNAL(ILL);
#endif
#ifdef SIGINT
		    TRANSLATE_SIGNAL(INT);
#endif
#ifdef SIGKILL
		    TRANSLATE_SIGNAL(KILL);
#endif
#ifdef SIGPIPE
		    TRANSLATE_SIGNAL(PIPE);
#endif
#ifdef SIGQUIT
		    TRANSLATE_SIGNAL(QUIT);
#endif
#ifdef SIGSEGV
		    TRANSLATE_SIGNAL(SEGV);
#endif
#ifdef SIGTERM
		    TRANSLATE_SIGNAL(TERM);
#endif
#ifdef SIGUSR1
		    TRANSLATE_SIGNAL(USR1);
#endif
#ifdef SIGUSR2
		    TRANSLATE_SIGNAL(USR2);
#endif
#undef TRANSLATE_SIGNAL
		    else
			exitcode = 128;
                } else {              /* nonstandard integer format */
                    unsigned signum = get_uint32(pktin);
		    fmt_sig = dupprintf(" %u", signum);
                    exitcode = 128 + signum;
                }

                core = get_bool(pktin);
                errmsg = get_string(pktin); /* error message */
                get_string(pktin);     /* language tag */
                if (!get_err(pktin) && get_avail(pktin) == 0)
                    break;             /* successful parse */

                sfree(fmt_sig);
            }

            if (format == 2) {
                fmt_sig = NULL;
                exitcode = 128;
            }

            ssh->exitcode = exitcode;
            if (errmsg.len) {
                fmt_msg = dupprintf(" (\"%.*s\")", PTRLEN_PRINTF(errmsg));
            }

	    logeventf(ssh, "Server exited on signal%s%s%s",
		      fmt_sig ? fmt_sig : "",
                      core ? " (core dumped)" : "",
		      fmt_msg ? fmt_msg : "");
	    sfree(fmt_sig);
            sfree(fmt_msg);
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
	pktout = ssh_bpp_new_pktout(ssh->bpp, reply);
	put_uint32(pktout, c->remoteid);
	ssh2_pkt_send(ssh, pktout);
    }
}

static void ssh2_msg_global_request(Ssh ssh, PktIn *pktin)
{
    int want_reply;
    PktOut *pktout;

    get_string(pktin);                 /* ignore request type (see below) */
    want_reply = get_bool(pktin);

    /*
     * We currently don't support any global requests
     * at all, so we either ignore the request or
     * respond with REQUEST_FAILURE, depending on
     * want_reply.
     */
    if (want_reply) {
	pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_REQUEST_FAILURE);
	ssh2_pkt_send(ssh, pktout);
    }
}

struct X11FakeAuth *ssh_sharing_add_x11_display(
    Ssh ssh, int authtype, ssh_sharing_connstate *share_cs,
    share_channel *share_chan)
{
    struct X11FakeAuth *auth;

    /*
     * Make up a new set of fake X11 auth data, and add it to the tree
     * of currently valid ones with an indication of the sharing
     * context that it's relevant to.
     */
    auth = x11_invent_fake_auth(ssh->x11authtree, authtype);
    auth->share_cs = share_cs;
    auth->share_chan = share_chan;

    return auth;
}

void ssh_sharing_remove_x11_display(Ssh ssh, struct X11FakeAuth *auth)
{
    del234(ssh->x11authtree, auth);
    x11_free_fake_auth(auth);
}

static void ssh2_msg_channel_open(Ssh ssh, PktIn *pktin)
{
    ptrlen type;
    int peerport;
    const char *error = NULL;
    struct ssh_channel *c;
    unsigned remid, winsize, pktsize;
    PktOut *pktout;

    type = get_string(pktin);
    c = snew(struct ssh_channel);
    c->ssh = ssh;

    remid = get_uint32(pktin);
    winsize = get_uint32(pktin);
    pktsize = get_uint32(pktin);

    if (ptrlen_eq_string(type, "x11")) {
	char *addrstr = mkstr(get_string(pktin));
	peerport = get_uint32(pktin);

	logeventf(ssh, "Received X11 connect request from %s:%d",
		  addrstr, peerport);

	if (!ssh->X11_fwd_enabled && !ssh->connshare)
	    error = "X11 forwarding is not enabled";
	else {
            c->chan = x11_new_channel(ssh->x11authtree, c, addrstr, peerport,
                                      ssh->connshare != NULL);
            logevent("Opened X11 forward channel");
	}

	sfree(addrstr);
    } else if (ptrlen_eq_string(type, "forwarded-tcpip")) {
	struct ssh_rportfwd pf, *realpf;
        ptrlen peeraddr;

        pf.shost = mkstr(get_string(pktin));
	pf.sport = get_uint32(pktin);
	peeraddr = get_string(pktin);
	peerport = get_uint32(pktin);
	realpf = find234(ssh->rportfwds, &pf, NULL);
	logeventf(ssh, "Received remote port %s:%d open request "
		  "from %.*s:%d", pf.shost, pf.sport,
                  PTRLEN_PRINTF(peeraddr), peerport);
        sfree(pf.shost);

	if (realpf == NULL) {
	    error = "Remote port is not recognised";
	} else {
            char *err;

            if (realpf->share_ctx) {
                /*
                 * This port forwarding is on behalf of a
                 * connection-sharing downstream, so abandon our own
                 * channel-open procedure and just pass the message on
                 * to sshshare.c.
                 */
                share_got_pkt_from_server(realpf->share_ctx, pktin->type,
                                          BinarySource_UPCAST(pktin)->data,
                                          BinarySource_UPCAST(pktin)->len);
                sfree(c);
                return;
            }

            err = pfd_connect(&c->chan, realpf->dhost, realpf->dport,
                              c, ssh->conf, realpf->pfrec->addressfamily);
	    logeventf(ssh, "Attempting to forward remote port to "
		      "%s:%d", realpf->dhost, realpf->dport);
	    if (err != NULL) {
		logeventf(ssh, "Port open failed: %s", err);
                sfree(err);
		error = "Port open failed";
	    } else {
		logevent("Forwarded port opened successfully");
	    }
	}
    } else if (ptrlen_eq_string(type, "auth-agent@openssh.com")) {
	if (!ssh->agentfwd_enabled)
	    error = "Agent forwarding is not enabled";
        else
            c->chan = agentf_new(c);
    } else {
	error = "Unsupported channel type requested";
    }

    c->remoteid = remid;
    c->halfopen = FALSE;
    if (error) {
	pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_CHANNEL_OPEN_FAILURE);
	put_uint32(pktout, c->remoteid);
	put_uint32(pktout, SSH2_OPEN_CONNECT_FAILED);
	put_stringz(pktout, error);
	put_stringz(pktout, "en");	/* language tag */
	ssh2_pkt_send(ssh, pktout);
	logeventf(ssh, "Rejected channel open: %s", error);
	sfree(c);
    } else {
	ssh_channel_init(c);
	c->v.v2.remwindow = winsize;
	c->v.v2.remmaxpkt = pktsize;
        if (c->chan->initial_fixed_window_size) {
            c->v.v2.locwindow = c->v.v2.locmaxwin = c->v.v2.remlocwin =
                c->chan->initial_fixed_window_size;
        }
	pktout = ssh_bpp_new_pktout(
            ssh->bpp, SSH2_MSG_CHANNEL_OPEN_CONFIRMATION);
	put_uint32(pktout, c->remoteid);
	put_uint32(pktout, c->localid);
	put_uint32(pktout, c->v.v2.locwindow);
	put_uint32(pktout, OUR_V2_MAXPKT);	/* our max pkt size */
	ssh2_pkt_send(ssh, pktout);
    }
}

void sshfwd_x11_sharing_handover(struct ssh_channel *c,
                                 ssh_sharing_connstate *share_cs,
                                 share_channel *share_chan,
                                 const char *peer_addr, int peer_port,
                                 int endian, int protomajor, int protominor,
                                 const void *initial_data, int initial_len)
{
    /*
     * This function is called when we've just discovered that an X
     * forwarding channel on which we'd been handling the initial auth
     * ourselves turns out to be destined for a connection-sharing
     * downstream. So we turn the channel into a sharing one, meaning
     * that we completely stop tracking windows and buffering data and
     * just pass more or less unmodified SSH messages back and forth.
     */
    c->sharectx = share_cs;
    share_setup_x11_channel(share_cs, share_chan,
                            c->localid, c->remoteid, c->v.v2.remwindow,
                            c->v.v2.remmaxpkt, c->v.v2.locwindow,
                            peer_addr, peer_port, endian,
                            protomajor, protominor,
                            initial_data, initial_len);
    chan_free(c->chan);
    c->chan = NULL;
}

void sshfwd_window_override_removed(struct ssh_channel *c)
{
    /*
     * This function is called when a client-side Channel has just
     * stopped requiring an initial fixed-size window.
     */
    assert(!c->chan->initial_fixed_window_size);
    if (c->ssh->version == 2)
        ssh2_set_window(
            c, ssh_is_simple(c->ssh) ? OUR_V2_BIGWIN : OUR_V2_WINSIZE);
}

/*
 * Buffer banner messages for later display at some convenient point,
 * if we're going to display them.
 */
static void ssh2_msg_userauth_banner(Ssh ssh, PktIn *pktin)
{
    /* Arbitrary limit to prevent unbounded inflation of buffer */
    if (conf_get_int(ssh->conf, CONF_ssh_show_banner) &&
	bufchain_size(&ssh->banner) <= 131072) {
	ptrlen banner = get_string(pktin);
	if (banner.len)
	    bufchain_add(&ssh->banner, banner.ptr, banner.len);
    }
}

/* Helper function to deal with sending tty modes for "pty-req" */
static void ssh2_send_ttymode(BinarySink *bs,
                              const struct ssh_ttymode *mode, char *val)
{
    put_byte(bs, mode->opcode);

    switch (mode->type) {
      case TTY_OP_CHAR:
        put_uint32(bs, ssh_tty_parse_specchar(val));
	break;
      case TTY_OP_BOOL:
        put_uint32(bs, ssh_tty_parse_boolean(val));
	break;
    }
}

static void ssh2_setup_x11(struct ssh_channel *c, PktIn *pktin,
                           void *ctx)
{
    struct ssh2_setup_x11_state {
	int crLine;
    };
    Ssh ssh = c->ssh;
    PktOut *pktout;
    crStateP(ssh2_setup_x11_state, ctx);

    crBeginState;

    logevent("Requesting X11 forwarding");
    pktout = ssh2_chanreq_init(ssh->mainchan, "x11-req",
                               ssh2_setup_x11, s);
    put_bool(pktout, 0);	       /* many connections */
    put_stringz(pktout, ssh->x11auth->protoname);
    put_stringz(pktout, ssh->x11auth->datastring);
    put_uint32(pktout, ssh->x11disp->screennum);
    ssh2_pkt_send(ssh, pktout);

    /* Wait to be called back with either a response packet, or NULL
     * meaning clean up and free our data */
    crReturnV;

    if (pktin) {
        if (pktin->type == SSH2_MSG_CHANNEL_SUCCESS) {
            logevent("X11 forwarding enabled");
            ssh->X11_fwd_enabled = TRUE;
        } else
            logevent("X11 forwarding refused");
    }

    crFinishFreeV;
}

static void ssh2_setup_agent(struct ssh_channel *c, PktIn *pktin,
				   void *ctx)
{
    struct ssh2_setup_agent_state {
	int crLine;
    };
    Ssh ssh = c->ssh;
    PktOut *pktout;
    crStateP(ssh2_setup_agent_state, ctx);

    crBeginState;

    logevent("Requesting OpenSSH-style agent forwarding");
    pktout = ssh2_chanreq_init(ssh->mainchan, "auth-agent-req@openssh.com",
                               ssh2_setup_agent, s);
    ssh2_pkt_send(ssh, pktout);

    /* Wait to be called back with either a response packet, or NULL
     * meaning clean up and free our data */
    crReturnV;

    if (pktin) {
        if (pktin->type == SSH2_MSG_CHANNEL_SUCCESS) {
            logevent("Agent forwarding enabled");
            ssh->agentfwd_enabled = TRUE;
        } else
            logevent("Agent forwarding refused");
    }

    crFinishFreeV;
}

static void ssh2_setup_pty(struct ssh_channel *c, PktIn *pktin,
				 void *ctx)
{
    struct ssh2_setup_pty_state {
	int crLine;
    };
    Ssh ssh = c->ssh;
    PktOut *pktout;
    crStateP(ssh2_setup_pty_state, ctx);

    crBeginState;

    /* Unpick the terminal-speed string. */
    /* XXX perhaps we should allow no speeds to be sent. */
    ssh->ospeed = 38400; ssh->ispeed = 38400; /* last-resort defaults */
    sscanf(conf_get_str(ssh->conf, CONF_termspeed), "%d,%d", &ssh->ospeed, &ssh->ispeed);
    /* Build the pty request. */
    pktout = ssh2_chanreq_init(ssh->mainchan, "pty-req",
                               ssh2_setup_pty, s);
    put_stringz(pktout, conf_get_str(ssh->conf, CONF_termtype));
    put_uint32(pktout, ssh->term_width);
    put_uint32(pktout, ssh->term_height);
    put_uint32(pktout, 0);	       /* pixel width */
    put_uint32(pktout, 0);	       /* pixel height */
    {
        strbuf *modebuf = strbuf_new();
        parse_ttymodes(BinarySink_UPCAST(modebuf), ssh, ssh2_send_ttymode);
        put_byte(modebuf, SSH2_TTY_OP_ISPEED);
        put_uint32(modebuf, ssh->ispeed);
        put_byte(modebuf, SSH2_TTY_OP_OSPEED);
        put_uint32(modebuf, ssh->ospeed);
        put_byte(modebuf, SSH_TTY_OP_END);
        put_stringsb(pktout, modebuf);
    }
    ssh2_pkt_send(ssh, pktout);
    ssh->state = SSH_STATE_INTERMED;

    /* Wait to be called back with either a response packet, or NULL
     * meaning clean up and free our data */
    crReturnV;

    if (pktin) {
        if (pktin->type == SSH2_MSG_CHANNEL_SUCCESS) {
            logeventf(ssh, "Allocated pty (ospeed %dbps, ispeed %dbps)",
                      ssh->ospeed, ssh->ispeed);
            ssh->got_pty = TRUE;
        } else {
            c_write_str(ssh, "Server refused to allocate pty\r\n");
            ssh->editing = ssh->echoing = 1;
        }
    }

    crFinishFreeV;
}

static void ssh2_setup_env(struct ssh_channel *c, PktIn *pktin,
			   void *ctx)
{
    struct ssh2_setup_env_state {
	int crLine;
	int num_env, env_left, env_ok;
    };
    Ssh ssh = c->ssh;
    PktOut *pktout;
    crStateP(ssh2_setup_env_state, ctx);

    crBeginState;

    /*
     * Send environment variables.
     * 
     * Simplest thing here is to send all the requests at once, and
     * then wait for a whole bunch of successes or failures.
     */
    s->num_env = 0;
    {
	char *key, *val;

	for (val = conf_get_str_strs(ssh->conf, CONF_environmt, NULL, &key);
	     val != NULL;
	     val = conf_get_str_strs(ssh->conf, CONF_environmt, key, &key)) {
	    pktout = ssh2_chanreq_init(ssh->mainchan, "env", ssh2_setup_env, s);
	    put_stringz(pktout, key);
	    put_stringz(pktout, val);
	    ssh2_pkt_send(ssh, pktout);

	    s->num_env++;
	}
	if (s->num_env)
	    logeventf(ssh, "Sent %d environment variables", s->num_env);
    }

    if (s->num_env) {
	s->env_ok = 0;
	s->env_left = s->num_env;

	while (s->env_left > 0) {
            /* Wait to be called back with either a response packet,
             * or NULL meaning clean up and free our data */
            crReturnV;
	    if (!pktin) goto out;
	    if (pktin->type == SSH2_MSG_CHANNEL_SUCCESS)
		s->env_ok++;
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
  out:;
    crFinishFreeV;
}

/*
 * Handle the SSH-2 userauth layer.
 */
static void ssh2_msg_userauth(Ssh ssh, PktIn *pktin)
{
    pktin->refcount++;   /* avoid packet being freed when we return */
    pq_push(&ssh->pq_ssh2_userauth, pktin);
    if (pktin->type == SSH2_MSG_USERAUTH_SUCCESS) {
        /*
         * The very instant we see this message, the connection
         * protocol has officially started, which means we must
         * install the dispatch-table entries for all the
         * connection-layer messages. In particular, we must do this
         * _before_ we return to the loop in ssh_process_pq_full
         * that's processing the currently queued packets through the
         * dispatch table, because if (say) an SSH_MSG_GLOBAL_REQUEST
         * is already pending in pq_full, we can't afford to delay
         * installing its dispatch table entry until after that queue
         * run is done.
         */
        ssh2_connection_setup(ssh);
    }
    queue_idempotent_callback(&ssh->ssh2_userauth_icb);
}

static void do_ssh2_userauth(void *vctx)
{
    Ssh ssh = (Ssh)vctx;
    PktIn *pktin;

    struct do_ssh2_userauth_state {
	int crLine;
	enum {
	    AUTH_TYPE_NONE,
		AUTH_TYPE_PUBLICKEY,
		AUTH_TYPE_PUBLICKEY_OFFER_LOUD,
		AUTH_TYPE_PUBLICKEY_OFFER_QUIET,
		AUTH_TYPE_PASSWORD,
	        AUTH_TYPE_GSSAPI,      /* always QUIET */
		AUTH_TYPE_KEYBOARD_INTERACTIVE,
		AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET
	} type;
	int done_service_req;
	int need_pw, can_pubkey, can_passwd, can_keyb_inter;
        int userpass_ret;
	int tried_pubkey_config, done_agent;
#ifndef NO_GSSAPI
	int can_gssapi;
        int can_gssapi_keyex_auth;
	int tried_gssapi;
        int tried_gssapi_keyex_auth;
        time_t gss_cred_expiry;
#endif
	int kbd_inter_refused;
	int we_are_in, userauth_success;
	prompts_t *cur_prompt;
	int num_prompts;
	char *username;
	char *password;
	int got_username;
	strbuf *publickey_blob;
	int privatekey_available, privatekey_encrypted;
	char *publickey_algorithm;
	char *publickey_comment;
        unsigned char *agent_response;
        BinarySource asrc[1];          /* for reading SSH agent response */
	size_t pkblob_pos_in_agent;
	int keyi, nkeys;
        ptrlen pk, alg, comment;
	int len;
	PktOut *pktout;
	Filename *keyfile;
#ifndef NO_GSSAPI
	Ssh_gss_ctx gss_ctx;
	Ssh_gss_buf gss_buf;
	Ssh_gss_buf gss_rcvtok, gss_sndtok;
	Ssh_gss_stat gss_stat;
#endif
    };
    crState(do_ssh2_userauth_state);

    crBeginState;

    /* Register as a handler for all the messages this coroutine handles. */
    ssh->packet_dispatch[SSH2_MSG_SERVICE_ACCEPT] = ssh2_msg_userauth;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_REQUEST] = ssh2_msg_userauth;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_FAILURE] = ssh2_msg_userauth;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_SUCCESS] = ssh2_msg_userauth;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_BANNER] = ssh2_msg_userauth;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_PK_OK] = ssh2_msg_userauth;
    /* ssh->packet_dispatch[SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ] = ssh2_msg_userauth; duplicate case value */
    /* ssh->packet_dispatch[SSH2_MSG_USERAUTH_INFO_REQUEST] = ssh2_msg_userauth; duplicate case value */
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_INFO_RESPONSE] = ssh2_msg_userauth;
    
    s->done_service_req = FALSE;
    s->we_are_in = s->userauth_success = FALSE;
    s->agent_response = NULL;
#ifndef NO_GSSAPI
    s->tried_gssapi = FALSE;
    s->tried_gssapi_keyex_auth = FALSE;
#endif

    if (!conf_get_int(ssh->conf, CONF_ssh_no_userauth)) {
        /*
         * Request userauth protocol, and await a response to it.
         */
        s->pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_SERVICE_REQUEST);
        put_stringz(s->pktout, "ssh-userauth");
        ssh2_pkt_send(ssh, s->pktout);
        crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_userauth)) != NULL);
        if (pktin->type == SSH2_MSG_SERVICE_ACCEPT)
            s->done_service_req = TRUE;
    }
    if (!s->done_service_req) {
        /*
         * Request connection protocol directly, without authentication.
         */
        s->pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_SERVICE_REQUEST);
        put_stringz(s->pktout, "ssh-connection");
        ssh2_pkt_send(ssh, s->pktout);
        crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_userauth)) != NULL);
        if (pktin->type == SSH2_MSG_SERVICE_ACCEPT) {
            s->we_are_in = TRUE; /* no auth required */
        } else {
            bombout(("Server refused service request"));
            crStopV;
        }
    }

    /* Arrange to be able to deal with any BANNERs that come in.
     * (We do this now as packets may come in during the next bit.) */
    bufchain_init(&ssh->banner);
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_BANNER] =
	ssh2_msg_userauth_banner;

    /*
     * Misc one-time setup for authentication.
     */
    s->publickey_blob = NULL;
    if (!s->we_are_in) {

	/*
	 * Load the public half of any configured public key file
	 * for later use.
	 */
	s->keyfile = conf_get_filename(ssh->conf, CONF_keyfile);
	if (!filename_is_null(s->keyfile)) {
	    int keytype;
	    logeventf(ssh, "Reading key file \"%.150s\"",
		      filename_to_str(s->keyfile));
	    keytype = key_type(s->keyfile);
	    if (keytype == SSH_KEYTYPE_SSH2 ||
                keytype == SSH_KEYTYPE_SSH2_PUBLIC_RFC4716 ||
                keytype == SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH) {
		const char *error;
		s->publickey_blob = strbuf_new();
                if (ssh2_userkey_loadpub(s->keyfile,
					 &s->publickey_algorithm,
					 BinarySink_UPCAST(s->publickey_blob),
					 &s->publickey_comment, &error)) {
		    s->privatekey_available = (keytype == SSH_KEYTYPE_SSH2);
                    if (!s->privatekey_available)
                        logeventf(ssh, "Key file contains public key only");
		    s->privatekey_encrypted =
			ssh2_userkey_encrypted(s->keyfile, NULL);
		} else {
		    char *msgbuf;
		    logeventf(ssh, "Unable to load key (%s)", 
			      error);
		    msgbuf = dupprintf("Unable to load key file "
				       "\"%.150s\" (%s)\r\n",
				       filename_to_str(s->keyfile),
				       error);
		    c_write_str(ssh, msgbuf);
		    sfree(msgbuf);
                    strbuf_free(s->publickey_blob);
                    s->publickey_blob = NULL;
		}
	    } else {
		char *msgbuf;
		logeventf(ssh, "Unable to use this key file (%s)",
			  key_type_to_str(keytype));
		msgbuf = dupprintf("Unable to use key file \"%.150s\""
				   " (%s)\r\n",
				   filename_to_str(s->keyfile),
				   key_type_to_str(keytype));
		c_write_str(ssh, msgbuf);
		sfree(msgbuf);
		s->publickey_blob = NULL;
	    }
	}

	/*
	 * Find out about any keys Pageant has (but if there's a
	 * public key configured, filter out all others).
	 */
	s->nkeys = 0;
	s->agent_response = NULL;
	s->pkblob_pos_in_agent = 0;
	if (conf_get_int(ssh->conf, CONF_tryagent) && agent_exists()) {
	    void *r;
            int rlen;
            strbuf *agent_request;

	    logevent("Pageant is running. Requesting keys.");

	    /* Request the keys held by the agent. */
            agent_request = strbuf_new_for_agent_query();
            put_byte(agent_request, SSH2_AGENTC_REQUEST_IDENTITIES);
            ssh->auth_agent_query = agent_query(
                agent_request, &r, &rlen, ssh_agent_callback, ssh);
            strbuf_free(agent_request);

	    if (ssh->auth_agent_query) {
                ssh->agent_response = NULL;
		crWaitUntilV(ssh->agent_response);
		r = ssh->agent_response;
                rlen = ssh->agent_response_len;
	    }
            s->agent_response = r;
            BinarySource_BARE_INIT(s->asrc, r, rlen);
            get_uint32(s->asrc); /* skip length field */
	    if (get_byte(s->asrc) == SSH2_AGENT_IDENTITIES_ANSWER) {
		int keyi;

		s->nkeys = toint(get_uint32(s->asrc));

                /*
                 * Vet the Pageant response to ensure that the key
                 * count and blob lengths make sense.
                 */
                if (s->nkeys < 0) {
                    logeventf(ssh, "Pageant response contained a negative"
                              " key count %d", s->nkeys);
                    s->nkeys = 0;
                    goto done_agent_query;
                } else {
                    logeventf(ssh, "Pageant has %d SSH-2 keys", s->nkeys);

		    /* See if configured key is in agent. */
                    for (keyi = 0; keyi < s->nkeys; keyi++) {
                        size_t pos = s->asrc->pos;
                        ptrlen blob = get_string(s->asrc);
                        get_string(s->asrc); /* skip comment */
                        if (get_err(s->asrc)) {
                            logeventf(ssh, "Pageant response was truncated");
                            s->nkeys = 0;
                            goto done_agent_query;
                        }

			if (s->publickey_blob &&
                            blob.len == s->publickey_blob->len &&
			    !memcmp(blob.ptr, s->publickey_blob->s,
				    s->publickey_blob->len)) {
			    logeventf(ssh, "Pageant key #%d matches "
				      "configured key file", keyi);
			    s->keyi = keyi;
			    s->pkblob_pos_in_agent = pos;
			    break;
			}
		    }
		    if (s->publickey_blob && !s->pkblob_pos_in_agent) {
			logevent("Configured key file not in Pageant");
			s->nkeys = 0;
		    }
		}
	    } else {
                logevent("Failed to get reply from Pageant");
	    }
          done_agent_query:;
	}

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
    s->got_username = FALSE;
    while (!s->we_are_in) {
	/*
	 * Get a username.
	 */
	if (s->got_username && !conf_get_int(ssh->conf, CONF_change_username)) {
	    /*
	     * We got a username last time round this loop, and
	     * with change_username turned off we don't try to get
	     * it again.
	     */
	} else if ((ssh->username = get_remote_username(ssh->conf)) == NULL) {
	    s->cur_prompt = new_prompts(ssh->frontend);
	    s->cur_prompt->to_server = TRUE;
	    s->cur_prompt->name = dupstr("SSH login name");
	    add_prompt(s->cur_prompt, dupstr("login as: "), TRUE); 
	    s->userpass_ret = get_userpass_input(s->cur_prompt, NULL);
	    while (1) {
                while (s->userpass_ret < 0 &&
                       bufchain_size(&ssh->user_input) > 0)
                    s->userpass_ret = get_userpass_input(
                        s->cur_prompt, &ssh->user_input);

                if (s->userpass_ret >= 0)
                    break;

		ssh->send_ok = 1;
		crReturnV;
		ssh->send_ok = 0;
	    }
	    if (!s->userpass_ret) {
		/*
		 * get_userpass_input() failed to get a username.
		 * Terminate.
		 */
		free_prompts(s->cur_prompt);
		ssh_disconnect(ssh, "No username provided", NULL, 0, TRUE);
		crStopV;
	    }
	    ssh->username = dupstr(s->cur_prompt->prompts[0]->result);
	    free_prompts(s->cur_prompt);
	} else {
	    char *stuff;
	    if ((flags & FLAG_VERBOSE) || (flags & FLAG_INTERACTIVE)) {
		stuff = dupprintf("Using username \"%s\".\r\n", ssh->username);
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
	ssh->pls.actx = SSH2_PKTCTX_NOAUTH;

	s->pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_USERAUTH_REQUEST);
	put_stringz(s->pktout, ssh->username);
	put_stringz(s->pktout, "ssh-connection");/* service requested */
	put_stringz(s->pktout, "none");    /* method */
	ssh2_pkt_send(ssh, s->pktout);
	s->type = AUTH_TYPE_NONE;
	s->we_are_in = FALSE;

	s->tried_pubkey_config = FALSE;
	s->kbd_inter_refused = FALSE;

	/* Reset agent request state. */
	s->done_agent = FALSE;
	if (s->agent_response) {
	    if (s->pkblob_pos_in_agent) {
		s->asrc->pos = s->pkblob_pos_in_agent;
	    } else {
		s->asrc->pos = 9;      /* skip length + type + key count */
		s->keyi = 0;
	    }
	}

	while (1) {
            ptrlen methods;

            methods.ptr = "";
            methods.len = 0;

	    /*
	     * Wait for the result of the last authentication request.
	     */
            crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_userauth)) != NULL);

	    /*
	     * Now is a convenient point to spew any banner material
	     * that we've accumulated. (This should ensure that when
	     * we exit the auth loop, we haven't any left to deal
	     * with.)
	     */
	    {
		int size = bufchain_size(&ssh->banner);
		/*
		 * Don't show the banner if we're operating in
		 * non-verbose non-interactive mode. (It's probably
		 * a script, which means nobody will read the
		 * banner _anyway_, and moreover the printing of
		 * the banner will screw up processing on the
		 * output of (say) plink.)
		 */
		if (size && (flags & (FLAG_VERBOSE | FLAG_INTERACTIVE))) {
		    char *banner = snewn(size, char);
		    bufchain_fetch(&ssh->banner, banner, size);
		    c_write_untrusted(ssh, banner, size);
		    sfree(banner);
		}
		bufchain_clear(&ssh->banner);
	    }
	    if (pktin->type == SSH2_MSG_USERAUTH_SUCCESS) {
		logevent("Access granted");
		s->we_are_in = s->userauth_success = TRUE;
		break;
	    }

	    if (pktin->type != SSH2_MSG_USERAUTH_FAILURE && s->type != AUTH_TYPE_GSSAPI) {
		bombout(("Strange packet received during authentication: "
			 "type %d", pktin->type));
		crStopV;
	    }

	    /*
	     * OK, we're now sitting on a USERAUTH_FAILURE message, so
	     * we can look at the string in it and know what we can
	     * helpfully try next.
	     */
	    if (pktin->type == SSH2_MSG_USERAUTH_FAILURE) {
		methods = get_string(pktin);
		if (!get_bool(pktin)) {
		    /*
		     * We have received an unequivocal Access
		     * Denied. This can translate to a variety of
		     * messages, or no message at all.
                     *
                     * For forms of authentication which are attempted
                     * implicitly, by which I mean without printing
                     * anything in the window indicating that we're
                     * trying them, we should never print 'Access
                     * denied'.
                     *
                     * If we do print a message saying that we're
                     * attempting some kind of authentication, it's OK
                     * to print a followup message saying it failed -
                     * but the message may sometimes be more specific
                     * than simply 'Access denied'.
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
			logevent("Server refused our key");
                    } else if (s->type == AUTH_TYPE_PUBLICKEY) {
                        /* This _shouldn't_ happen except by a
                         * protocol bug causing client and server to
                         * disagree on what is a correct signature. */
                        c_write_str(ssh, "Server refused public-key signature"
                                    " despite accepting key!\r\n");
                        logevent("Server refused public-key signature"
                                 " despite accepting key!");
		    } else if (s->type==AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET) {
                        /* quiet, so no c_write */
                        logevent("Server refused keyboard-interactive authentication");
		    } else if (s->type==AUTH_TYPE_GSSAPI) {
			/* always quiet, so no c_write */
                        /* also, the code down in the GSSAPI block has
                         * already logged this in the Event Log */
		    } else if (s->type == AUTH_TYPE_KEYBOARD_INTERACTIVE) {
                        logevent("Keyboard-interactive authentication failed");
			c_write_str(ssh, "Access denied\r\n");
                    } else {
                        assert(s->type == AUTH_TYPE_PASSWORD);
                        logevent("Password authentication failed");
			c_write_str(ssh, "Access denied\r\n");

			if (conf_get_int(ssh->conf, CONF_change_username)) {
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

		s->can_pubkey =
		    in_commasep_string("publickey", methods.ptr, methods.len);
		s->can_passwd =
		    in_commasep_string("password", methods.ptr, methods.len);
		s->can_keyb_inter =
                    conf_get_int(ssh->conf, CONF_try_ki_auth) &&
		    in_commasep_string("keyboard-interactive",
                                       methods.ptr, methods.len);
#ifndef NO_GSSAPI
                s->can_gssapi =
                    conf_get_int(ssh->conf, CONF_try_gssapi_auth) &&
                    in_commasep_string("gssapi-with-mic",
                                       methods.ptr, methods.len) &&
                    ssh->gsslibs->nlibraries > 0;
                s->can_gssapi_keyex_auth =
                    conf_get_int(ssh->conf, CONF_try_gssapi_kex) &&
                    in_commasep_string("gssapi-keyex",
                                       methods.ptr, methods.len) &&
                    ssh->gsslibs->nlibraries > 0 &&
                    ssh->gss_ctx;
#endif
	    }

	    ssh->pls.actx = SSH2_PKTCTX_NOAUTH;

#ifndef NO_GSSAPI
            if (s->can_gssapi_keyex_auth && !s->tried_gssapi_keyex_auth) {

                /* gssapi-keyex authentication */

                s->type = AUTH_TYPE_GSSAPI;
                s->tried_gssapi_keyex_auth = TRUE;
                ssh->pls.actx = SSH2_PKTCTX_GSSAPI;

                if (ssh->gsslib->gsslogmsg)
                    logevent(ssh->gsslib->gsslogmsg);

                logeventf(ssh, "Trying gssapi-keyex...");
                s->pktout =
                    ssh2_gss_authpacket(ssh, ssh->gss_ctx, "gssapi-keyex");
                ssh2_pkt_send(ssh, s->pktout);
                ssh->gsslib->release_cred(ssh->gsslib, &ssh->gss_ctx);
                ssh->gss_ctx = NULL;

                continue;
            } else
#endif /* NO_GSSAPI */

	    if (s->can_pubkey && !s->done_agent && s->nkeys) {

		/*
		 * Attempt public-key authentication using a key from Pageant.
		 */

		ssh->pls.actx = SSH2_PKTCTX_PUBLICKEY;

		logeventf(ssh, "Trying Pageant key #%d", s->keyi);

		/* Unpack key from agent response */
                s->pk = get_string(s->asrc);
                s->comment = get_string(s->asrc);
                {
                    BinarySource src[1];
                    BinarySource_BARE_INIT(src, s->pk.ptr, s->pk.len);
                    s->alg = get_string(src);
                }

		/* See if server will accept it */
		s->pktout = ssh_bpp_new_pktout(
                    ssh->bpp, SSH2_MSG_USERAUTH_REQUEST);
		put_stringz(s->pktout, ssh->username);
		put_stringz(s->pktout, "ssh-connection");
						    /* service requested */
		put_stringz(s->pktout, "publickey");
						    /* method */
		put_bool(s->pktout, FALSE); /* no signature included */
		put_stringpl(s->pktout, s->alg);
		put_stringpl(s->pktout, s->pk);
		ssh2_pkt_send(ssh, s->pktout);
		s->type = AUTH_TYPE_PUBLICKEY_OFFER_QUIET;

		crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_userauth)) != NULL);
		if (pktin->type != SSH2_MSG_USERAUTH_PK_OK) {

		    /* Offer of key refused, presumably via
                     * USERAUTH_FAILURE. Requeue for the next iteration. */
		    pq_push_front(&ssh->pq_ssh2_userauth, pktin);

		} else {
                    strbuf *agentreq, *sigdata;
		    void *r;
                    int rlen;

		    if (flags & FLAG_VERBOSE) {
			c_write_str(ssh, "Authenticating with "
				    "public key \"");
			c_write(ssh, s->comment.ptr, s->comment.len);
			c_write_str(ssh, "\" from agent\r\n");
		    }

		    /*
		     * Server is willing to accept the key.
		     * Construct a SIGN_REQUEST.
		     */
		    s->pktout = ssh_bpp_new_pktout(
                        ssh->bpp, SSH2_MSG_USERAUTH_REQUEST);
		    put_stringz(s->pktout, ssh->username);
		    put_stringz(s->pktout, "ssh-connection");
							/* service requested */
		    put_stringz(s->pktout, "publickey");
							/* method */
		    put_bool(s->pktout, TRUE);  /* signature included */
		    put_stringpl(s->pktout, s->alg);
		    put_stringpl(s->pktout, s->pk);

		    /* Ask agent for signature. */
		    agentreq = strbuf_new_for_agent_query();
		    put_byte(agentreq, SSH2_AGENTC_SIGN_REQUEST);
		    put_stringpl(agentreq, s->pk);
		    /* Now the data to be signed... */
                    sigdata = strbuf_new();
		    if (ssh->remote_bugs & BUG_SSH2_PK_SESSIONID) {
                        put_data(sigdata, ssh->v2_session_id,
                                 ssh->v2_session_id_len);
                    } else {
                        put_string(sigdata, ssh->v2_session_id,
                                   ssh->v2_session_id_len);
                    }
		    put_data(sigdata, s->pktout->data + 5,
                             s->pktout->length - 5);
                    put_stringsb(agentreq, sigdata);
		    /* And finally the (zero) flags word. */
		    put_uint32(agentreq, 0);
                    ssh->auth_agent_query = agent_query(
                        agentreq, &r, &rlen, ssh_agent_callback, ssh);
                    strbuf_free(agentreq);

                    if (ssh->auth_agent_query) {
                        ssh->agent_response = NULL;
                        crWaitUntilV(ssh->agent_response);
			r = ssh->agent_response;
			rlen = ssh->agent_response_len;
		    }
		    if (r) {
                        ptrlen sigblob;
                        BinarySource src[1];
                        BinarySource_BARE_INIT(src, r, rlen);
                        get_uint32(src); /* skip length field */
			if (get_byte(src) == SSH2_AGENT_SIGN_RESPONSE &&
                            (sigblob = get_string(src), !get_err(src))) {
			    logevent("Sending Pageant's response");
			    ssh2_add_sigblob(ssh, s->pktout,
					     s->pk.ptr, s->pk.len,
					     sigblob.ptr, sigblob.len);
			    ssh2_pkt_send(ssh, s->pktout);
			    s->type = AUTH_TYPE_PUBLICKEY;
			} else {
			    /* FIXME: less drastic response */
			    bombout(("Pageant failed to answer challenge"));
			    crStopV;
			}
		    }
		}

		/* Do we have any keys left to try? */
		if (s->pkblob_pos_in_agent) {
		    s->done_agent = TRUE;
		    s->tried_pubkey_config = TRUE;
		} else {
		    s->keyi++;
		    if (s->keyi >= s->nkeys)
			s->done_agent = TRUE;
		}

	    } else if (s->can_pubkey && s->publickey_blob &&
		       s->privatekey_available && !s->tried_pubkey_config) {

		struct ssh2_userkey *key;   /* not live over crReturn */
		char *passphrase;	    /* not live over crReturn */

		ssh->pls.actx = SSH2_PKTCTX_PUBLICKEY;

		s->tried_pubkey_config = TRUE;

		/*
		 * Try the public key supplied in the configuration.
		 *
		 * First, offer the public blob to see if the server is
		 * willing to accept it.
		 */
		s->pktout = ssh_bpp_new_pktout(
                    ssh->bpp, SSH2_MSG_USERAUTH_REQUEST);
		put_stringz(s->pktout, ssh->username);
		put_stringz(s->pktout, "ssh-connection");
						/* service requested */
		put_stringz(s->pktout, "publickey");	/* method */
		put_bool(s->pktout, FALSE);
						/* no signature included */
		put_stringz(s->pktout, s->publickey_algorithm);
		put_string(s->pktout, s->publickey_blob->s,
                           s->publickey_blob->len);
		ssh2_pkt_send(ssh, s->pktout);
		logevent("Offered public key");

		crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_userauth)) != NULL);
		if (pktin->type != SSH2_MSG_USERAUTH_PK_OK) {
		    /* Key refused. Give up. */
		    pq_push_front(&ssh->pq_ssh2_userauth, pktin);
		    s->type = AUTH_TYPE_PUBLICKEY_OFFER_LOUD;
		    continue; /* process this new message */
		}
		logevent("Offer of public key accepted");

		/*
		 * Actually attempt a serious authentication using
		 * the key.
		 */
		if (flags & FLAG_VERBOSE) {
		    c_write_str(ssh, "Authenticating with public key \"");
		    c_write_str(ssh, s->publickey_comment);
		    c_write_str(ssh, "\"\r\n");
		}
		key = NULL;
		while (!key) {
		    const char *error;  /* not live over crReturn */
		    if (s->privatekey_encrypted) {
			/*
			 * Get a passphrase from the user.
			 */
			s->cur_prompt = new_prompts(ssh->frontend);
			s->cur_prompt->to_server = FALSE;
			s->cur_prompt->name = dupstr("SSH key passphrase");
			add_prompt(s->cur_prompt,
				   dupprintf("Passphrase for key \"%.100s\": ",
					     s->publickey_comment),
				   FALSE);
			s->userpass_ret = get_userpass_input(
                            s->cur_prompt, NULL);
			while (1) {
                            while (s->userpass_ret < 0 &&
                                   bufchain_size(&ssh->user_input) > 0)
                                s->userpass_ret = get_userpass_input(
                                    s->cur_prompt, &ssh->user_input);

                            if (s->userpass_ret >= 0)
                                break;

			    ssh->send_ok = 1;
                            crReturnV;
			    ssh->send_ok = 0;
			}
			if (!s->userpass_ret) {
			    /* Failed to get a passphrase. Terminate. */
			    free_prompts(s->cur_prompt);
			    ssh_disconnect(ssh, NULL,
					   "Unable to authenticate",
					   SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER,
					   TRUE);
			    crStopV;
			}
			passphrase =
			    dupstr(s->cur_prompt->prompts[0]->result);
			free_prompts(s->cur_prompt);
		    } else {
			passphrase = NULL; /* no passphrase needed */
		    }

		    /*
		     * Try decrypting the key.
		     */
		    s->keyfile = conf_get_filename(ssh->conf, CONF_keyfile);
		    key = ssh2_load_userkey(s->keyfile, passphrase, &error);
		    if (passphrase) {
			/* burn the evidence */
			smemclr(passphrase, strlen(passphrase));
			sfree(passphrase);
		    }
		    if (key == SSH2_WRONG_PASSPHRASE || key == NULL) {
			if (passphrase &&
			    (key == SSH2_WRONG_PASSPHRASE)) {
			    c_write_str(ssh, "Wrong passphrase\r\n");
			    key = NULL;
			    /* and loop again */
			} else {
			    c_write_str(ssh, "Unable to load private key (");
			    c_write_str(ssh, error);
			    c_write_str(ssh, ")\r\n");
			    key = NULL;
			    break; /* try something else */
			}
		    }
		}

		if (key) {
                    strbuf *pkblob, *sigdata, *sigblob;

		    /*
		     * We have loaded the private key and the server
		     * has announced that it's willing to accept it.
		     * Hallelujah. Generate a signature and send it.
		     */
		    s->pktout = ssh_bpp_new_pktout(
                        ssh->bpp, SSH2_MSG_USERAUTH_REQUEST);
		    put_stringz(s->pktout, ssh->username);
		    put_stringz(s->pktout, "ssh-connection");
						    /* service requested */
		    put_stringz(s->pktout, "publickey"); /* method */
		    put_bool(s->pktout, TRUE); /* signature follows */
		    put_stringz(s->pktout, ssh_key_ssh_id(key->key));
		    pkblob = strbuf_new();
                    ssh_key_public_blob(key->key, BinarySink_UPCAST(pkblob));
		    put_string(s->pktout, pkblob->s, pkblob->len);

		    /*
		     * The data to be signed is:
		     *
		     *   string  session-id
		     *
		     * followed by everything so far placed in the
		     * outgoing packet.
		     */
                    sigdata = strbuf_new();
		    if (ssh->remote_bugs & BUG_SSH2_PK_SESSIONID) {
                        put_data(sigdata, ssh->v2_session_id,
                                 ssh->v2_session_id_len);
                    } else {
                        put_string(sigdata, ssh->v2_session_id,
                                   ssh->v2_session_id_len);
                    }
		    put_data(sigdata, s->pktout->data + 5,
                             s->pktout->length - 5);
		    sigblob = strbuf_new();
                    ssh_key_sign(key->key, sigdata->s, sigdata->len,
                                 BinarySink_UPCAST(sigblob));
                    strbuf_free(sigdata);
		    ssh2_add_sigblob(ssh, s->pktout, pkblob->s, pkblob->len,
				     sigblob->s, sigblob->len);
		    strbuf_free(pkblob);
		    strbuf_free(sigblob);

		    ssh2_pkt_send(ssh, s->pktout);
                    logevent("Sent public key signature");
		    s->type = AUTH_TYPE_PUBLICKEY;
		    ssh_key_free(key->key);
                    sfree(key->comment);
                    sfree(key);
		}

#ifndef NO_GSSAPI
	    } else if (s->can_gssapi && !s->tried_gssapi) {

                /* gssapi-with-mic authentication */

		ptrlen data;

		s->type = AUTH_TYPE_GSSAPI;
		s->tried_gssapi = TRUE;
		ssh->pls.actx = SSH2_PKTCTX_GSSAPI;

                if (ssh->gsslib->gsslogmsg)
                    logevent(ssh->gsslib->gsslogmsg);

		/* Sending USERAUTH_REQUEST with "gssapi-with-mic" method */
                logeventf(ssh, "Trying gssapi-with-mic...");
		s->pktout = ssh_bpp_new_pktout(
                    ssh->bpp, SSH2_MSG_USERAUTH_REQUEST);
		put_stringz(s->pktout, ssh->username);
		put_stringz(s->pktout, "ssh-connection");
		put_stringz(s->pktout, "gssapi-with-mic");
                logevent("Attempting GSSAPI authentication");

		/* add mechanism info */
                ssh->gsslib->indicate_mech(ssh->gsslib, &s->gss_buf);

		/* number of GSSAPI mechanisms */
		put_uint32(s->pktout, 1);

		/* length of OID + 2 */
		put_uint32(s->pktout, s->gss_buf.length + 2);
		put_byte(s->pktout, SSH2_GSS_OIDTYPE);

		/* length of OID */
		put_byte(s->pktout, s->gss_buf.length);

		put_data(s->pktout, s->gss_buf.value, s->gss_buf.length);
		ssh2_pkt_send(ssh, s->pktout);
		crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_userauth)) != NULL);
		if (pktin->type != SSH2_MSG_USERAUTH_GSSAPI_RESPONSE) {
		    logevent("GSSAPI authentication request refused");
		    pq_push_front(&ssh->pq_ssh2_userauth, pktin);
		    continue;
		}

		/* check returned packet ... */

		data = get_string(pktin);
		s->gss_rcvtok.value = (char *)data.ptr;
		s->gss_rcvtok.length = data.len;
		if (s->gss_rcvtok.length != s->gss_buf.length + 2 ||
		    ((char *)s->gss_rcvtok.value)[0] != SSH2_GSS_OIDTYPE ||
		    ((char *)s->gss_rcvtok.value)[1] != s->gss_buf.length ||
		    memcmp((char *)s->gss_rcvtok.value + 2,
			   s->gss_buf.value,s->gss_buf.length) ) {
		    logevent("GSSAPI authentication - wrong response from server");
		    continue;
		}

                /* Import server name if not cached from KEX */
                if (ssh->gss_srv_name == GSS_C_NO_NAME) {
                    s->gss_stat = ssh->gsslib->import_name(ssh->gsslib,
                                                           ssh->fullhostname,
                                                           &ssh->gss_srv_name);
                    if (s->gss_stat != SSH_GSS_OK) {
                        if (s->gss_stat == SSH_GSS_BAD_HOST_NAME)
                            logevent("GSSAPI import name failed -"
                                     " Bad service name");
                        else
                            logevent("GSSAPI import name failed");
                        continue;
                    }
		}

                /* Allocate our gss_ctx */
                s->gss_stat = ssh->gsslib->acquire_cred(ssh->gsslib,
                                                        &s->gss_ctx, NULL);
		if (s->gss_stat != SSH_GSS_OK) {
		    logevent("GSSAPI authentication failed to get credentials");
		    continue;
		}

		/* initial tokens are empty */
		SSH_GSS_CLEAR_BUF(&s->gss_rcvtok);
		SSH_GSS_CLEAR_BUF(&s->gss_sndtok);

		/* now enter the loop */
		do {
                    /*
                     * When acquire_cred yields no useful expiration, go with
                     * the service ticket expiration.
                     */
                    s->gss_stat = ssh->gsslib->init_sec_context
                        (ssh->gsslib,
			 &s->gss_ctx,
                         ssh->gss_srv_name,
			 conf_get_int(ssh->conf, CONF_gssapifwd),
			 &s->gss_rcvtok,
                         &s->gss_sndtok,
                         NULL,
                         NULL);

		    if (s->gss_stat!=SSH_GSS_S_COMPLETE &&
			s->gss_stat!=SSH_GSS_S_CONTINUE_NEEDED) {
			logevent("GSSAPI authentication initialisation failed");

                        if (ssh->gsslib->display_status(ssh->gsslib,
                                s->gss_ctx, &s->gss_buf) == SSH_GSS_OK) {
			    logevent(s->gss_buf.value);
			    sfree(s->gss_buf.value);
			}

                        pq_push_front(&ssh->pq_ssh2_userauth, pktin);
			break;
		    }
		    logevent("GSSAPI authentication initialised");

                    /*
                     * Client and server now exchange tokens until GSSAPI
                     * no longer says CONTINUE_NEEDED
                     */
		    if (s->gss_sndtok.length != 0) {
                        s->pktout =
                            ssh_bpp_new_pktout(
                                ssh->bpp, SSH2_MSG_USERAUTH_GSSAPI_TOKEN);
			put_string(s->pktout,
                                   s->gss_sndtok.value, s->gss_sndtok.length);
			ssh2_pkt_send(ssh, s->pktout);
                        ssh->gsslib->free_tok(ssh->gsslib, &s->gss_sndtok);
		    }

		    if (s->gss_stat == SSH_GSS_S_CONTINUE_NEEDED) {
			crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_userauth)) != NULL);
			if (pktin->type != SSH2_MSG_USERAUTH_GSSAPI_TOKEN) {
                            logevent("GSSAPI authentication -"
                                     " bad server response");
			    s->gss_stat = SSH_GSS_FAILURE;
                            pq_push_front(&ssh->pq_ssh2_userauth, pktin);
			    break;
			}
			data = get_string(pktin);
			s->gss_rcvtok.value = (char *)data.ptr;
			s->gss_rcvtok.length = data.len;
		    }
		} while (s-> gss_stat == SSH_GSS_S_CONTINUE_NEEDED);

		if (s->gss_stat != SSH_GSS_OK) {
                    ssh->gsslib->release_cred(ssh->gsslib, &s->gss_ctx);
		    continue;
		}
		logevent("GSSAPI authentication loop finished OK");

		/* Now send the MIC */

                s->pktout =
                    ssh2_gss_authpacket(ssh, s->gss_ctx, "gssapi-with-mic");
		ssh2_pkt_send(ssh, s->pktout);

                ssh->gsslib->release_cred(ssh->gsslib, &s->gss_ctx);
		continue;
#endif
	    } else if (s->can_keyb_inter && !s->kbd_inter_refused) {

		/*
		 * Keyboard-interactive authentication.
		 */

		s->type = AUTH_TYPE_KEYBOARD_INTERACTIVE;

		ssh->pls.actx = SSH2_PKTCTX_KBDINTER;

		s->pktout = ssh_bpp_new_pktout(
                    ssh->bpp, SSH2_MSG_USERAUTH_REQUEST);
		put_stringz(s->pktout, ssh->username);
		put_stringz(s->pktout, "ssh-connection");
							/* service requested */
		put_stringz(s->pktout, "keyboard-interactive");
							/* method */
		put_stringz(s->pktout, "");	/* lang */
		put_stringz(s->pktout, "");	/* submethods */
		ssh2_pkt_send(ssh, s->pktout);
                
                logevent("Attempting keyboard-interactive authentication");

		crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_userauth)) != NULL);
		if (pktin->type != SSH2_MSG_USERAUTH_INFO_REQUEST) {
		    /* Server is not willing to do keyboard-interactive
		     * at all (or, bizarrely but legally, accepts the
		     * user without actually issuing any prompts).
		     * Give up on it entirely. */
                    pq_push_front(&ssh->pq_ssh2_userauth, pktin);
		    s->type = AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET;
		    s->kbd_inter_refused = TRUE; /* don't try it again */
		    continue;
		}

		/*
		 * Loop while the server continues to send INFO_REQUESTs.
		 */
		while (pktin->type == SSH2_MSG_USERAUTH_INFO_REQUEST) {

		    ptrlen name, inst;
		    int i;

		    /*
		     * We've got a fresh USERAUTH_INFO_REQUEST.
		     * Get the preamble and start building a prompt.
		     */
		    name = get_string(pktin);
		    inst = get_string(pktin);
		    get_string(pktin); /* skip language tag */
		    s->cur_prompt = new_prompts(ssh->frontend);
		    s->cur_prompt->to_server = TRUE;

		    /*
		     * Get any prompt(s) from the packet.
		     */
		    s->num_prompts = get_uint32(pktin);
		    for (i = 0; i < s->num_prompts; i++) {
			ptrlen prompt;
			int echo;
			static char noprompt[] =
			    "<server failed to send prompt>: ";

			prompt = get_string(pktin);
			echo = get_bool(pktin);
			if (!prompt.len) {
			    prompt.ptr = noprompt;
			    prompt.len = lenof(noprompt)-1;
			}
			add_prompt(s->cur_prompt, mkstr(prompt), echo);
		    }

		    if (name.len) {
			/* FIXME: better prefix to distinguish from
			 * local prompts? */
			s->cur_prompt->name =
			    dupprintf("SSH server: %.*s", PTRLEN_PRINTF(name));
			s->cur_prompt->name_reqd = TRUE;
		    } else {
			s->cur_prompt->name =
			    dupstr("SSH server authentication");
			s->cur_prompt->name_reqd = FALSE;
		    }
		    /* We add a prefix to try to make it clear that a prompt
		     * has come from the server.
		     * FIXME: ugly to print "Using..." in prompt _every_
		     * time round. Can this be done more subtly? */
		    /* Special case: for reasons best known to themselves,
		     * some servers send k-i requests with no prompts and
		     * nothing to display. Keep quiet in this case. */
		    if (s->num_prompts || name.len || inst.len) {
			s->cur_prompt->instruction =
			    dupprintf("Using keyboard-interactive "
                                      "authentication.%s%.*s",
                                      inst.len ? "\n" : "",
                                      PTRLEN_PRINTF(inst));
			s->cur_prompt->instr_reqd = TRUE;
		    } else {
			s->cur_prompt->instr_reqd = FALSE;
		    }

		    /*
                     * Display any instructions, and get the user's
                     * response(s).
		     */
                    s->userpass_ret = get_userpass_input(s->cur_prompt, NULL);
                    while (1) {
                        while (s->userpass_ret < 0 &&
                               bufchain_size(&ssh->user_input) > 0)
                            s->userpass_ret = get_userpass_input(
                                s->cur_prompt, &ssh->user_input);

                        if (s->userpass_ret >= 0)
                            break;

                        ssh->send_ok = 1;
                        crReturnV;
                        ssh->send_ok = 0;
                    }
                    if (!s->userpass_ret) {
                        /*
                         * Failed to get responses. Terminate.
                         */
                        free_prompts(s->cur_prompt);
                        ssh_disconnect(ssh, NULL, "Unable to authenticate",
                                       SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER,
                                       TRUE);
                        crStopV;
                    }

		    /*
		     * Send the response(s) to the server, padding
		     * them to disguise their true length.
		     */
		    s->pktout = ssh_bpp_new_pktout(
                        ssh->bpp, SSH2_MSG_USERAUTH_INFO_RESPONSE);
		    put_uint32(s->pktout, s->num_prompts);
		    for (i=0; i < s->num_prompts; i++) {
			put_stringz(s->pktout,
                                    s->cur_prompt->prompts[i]->result);
		    }
                    s->pktout->minlen = 256;
		    ssh2_pkt_send(ssh, s->pktout);

                    /*
                     * Free the prompts structure from this iteration.
                     * If there's another, a new one will be allocated
                     * when we return to the top of this while loop.
                     */
                    free_prompts(s->cur_prompt);

		    /*
		     * Get the next packet in case it's another
		     * INFO_REQUEST.
		     */
		    crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_userauth)) != NULL);

		}

		/*
		 * We should have SUCCESS or FAILURE now.
		 */
                pq_push_front(&ssh->pq_ssh2_userauth, pktin);

	    } else if (s->can_passwd) {

		/*
		 * Plain old password authentication.
		 */
		int changereq_first_time; /* not live over crReturn */

		ssh->pls.actx = SSH2_PKTCTX_PASSWORD;

		s->cur_prompt = new_prompts(ssh->frontend);
		s->cur_prompt->to_server = TRUE;
		s->cur_prompt->name = dupstr("SSH password");
		add_prompt(s->cur_prompt, dupprintf("%s@%s's password: ",
						    ssh->username,
						    ssh->savedhost),
			   FALSE);

		s->userpass_ret = get_userpass_input(s->cur_prompt, NULL);
		while (1) {
                    while (s->userpass_ret < 0 &&
                           bufchain_size(&ssh->user_input) > 0)
                        s->userpass_ret = get_userpass_input(
                            s->cur_prompt, &ssh->user_input);

                    if (s->userpass_ret >= 0)
                        break;

		    ssh->send_ok = 1;
                    crReturnV;
		    ssh->send_ok = 0;
		}
		if (!s->userpass_ret) {
		    /*
		     * Failed to get responses. Terminate.
		     */
		    free_prompts(s->cur_prompt);
		    ssh_disconnect(ssh, NULL, "Unable to authenticate",
				   SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER,
				   TRUE);
		    crStopV;
		}
		/*
		 * Squirrel away the password. (We may need it later if
		 * asked to change it.)
		 */
		s->password = dupstr(s->cur_prompt->prompts[0]->result);
		free_prompts(s->cur_prompt);

		/*
		 * Send the password packet.
		 *
		 * We pad out the password packet to 256 bytes to make
		 * it harder for an attacker to find the length of the
		 * user's password.
		 *
		 * Anyone using a password longer than 256 bytes
		 * probably doesn't have much to worry about from
		 * people who find out how long their password is!
		 */
		s->pktout = ssh_bpp_new_pktout(
                    ssh->bpp, SSH2_MSG_USERAUTH_REQUEST);
		put_stringz(s->pktout, ssh->username);
		put_stringz(s->pktout, "ssh-connection");
							/* service requested */
		put_stringz(s->pktout, "password");
		put_bool(s->pktout, FALSE);
		put_stringz(s->pktout, s->password);
                s->pktout->minlen = 256;
                ssh2_pkt_send(ssh, s->pktout);
		logevent("Sent password");
		s->type = AUTH_TYPE_PASSWORD;

		/*
		 * Wait for next packet, in case it's a password change
		 * request.
		 */
		crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_userauth)) != NULL);
		changereq_first_time = TRUE;

		while (pktin->type == SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ) {

		    /* 
		     * We're being asked for a new password
		     * (perhaps not for the first time).
		     * Loop until the server accepts it.
		     */

		    int got_new = FALSE; /* not live over crReturn */
		    ptrlen prompt;  /* not live over crReturn */
		    
		    {
			const char *msg;
			if (changereq_first_time)
			    msg = "Server requested password change";
			else
			    msg = "Server rejected new password";
			logevent(msg);
			c_write_str(ssh, msg);
			c_write_str(ssh, "\r\n");
		    }

		    prompt = get_string(pktin);

		    s->cur_prompt = new_prompts(ssh->frontend);
		    s->cur_prompt->to_server = TRUE;
		    s->cur_prompt->name = dupstr("New SSH password");
		    s->cur_prompt->instruction = mkstr(prompt);
		    s->cur_prompt->instr_reqd = TRUE;
		    /*
		     * There's no explicit requirement in the protocol
		     * for the "old" passwords in the original and
		     * password-change messages to be the same, and
		     * apparently some Cisco kit supports password change
		     * by the user entering a blank password originally
		     * and the real password subsequently, so,
		     * reluctantly, we prompt for the old password again.
		     *
		     * (On the other hand, some servers don't even bother
		     * to check this field.)
		     */
		    add_prompt(s->cur_prompt,
			       dupstr("Current password (blank for previously entered password): "),
			       FALSE);
		    add_prompt(s->cur_prompt, dupstr("Enter new password: "),
			       FALSE);
		    add_prompt(s->cur_prompt, dupstr("Confirm new password: "),
			       FALSE);

		    /*
		     * Loop until the user manages to enter the same
		     * password twice.
		     */
		    while (!got_new) {
			s->userpass_ret = get_userpass_input(
                            s->cur_prompt, NULL);
			while (1) {
                            while (s->userpass_ret < 0 &&
                                   bufchain_size(&ssh->user_input) > 0)
                                s->userpass_ret = get_userpass_input(
                                    s->cur_prompt, &ssh->user_input);

                            if (s->userpass_ret >= 0)
                                break;

			    ssh->send_ok = 1;
                            crReturnV;
			    ssh->send_ok = 0;
			}
			if (!s->userpass_ret) {
			    /*
			     * Failed to get responses. Terminate.
			     */
			    /* burn the evidence */
			    free_prompts(s->cur_prompt);
			    smemclr(s->password, strlen(s->password));
			    sfree(s->password);
			    ssh_disconnect(ssh, NULL, "Unable to authenticate",
					   SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER,
					   TRUE);
			    crStopV;
			}

			/*
			 * If the user specified a new original password
			 * (IYSWIM), overwrite any previously specified
			 * one.
			 * (A side effect is that the user doesn't have to
			 * re-enter it if they louse up the new password.)
			 */
			if (s->cur_prompt->prompts[0]->result[0]) {
			    smemclr(s->password, strlen(s->password));
				/* burn the evidence */
			    sfree(s->password);
			    s->password =
				dupstr(s->cur_prompt->prompts[0]->result);
			}

			/*
			 * Check the two new passwords match.
			 */
			got_new = (strcmp(s->cur_prompt->prompts[1]->result,
					  s->cur_prompt->prompts[2]->result)
				   == 0);
			if (!got_new)
			    /* They don't. Silly user. */
			    c_write_str(ssh, "Passwords do not match\r\n");

		    }

		    /*
		     * Send the new password (along with the old one).
		     * (see above for padding rationale)
		     */
		    s->pktout = ssh_bpp_new_pktout(
                        ssh->bpp, SSH2_MSG_USERAUTH_REQUEST);
		    put_stringz(s->pktout, ssh->username);
		    put_stringz(s->pktout, "ssh-connection");
							/* service requested */
		    put_stringz(s->pktout, "password");
		    put_bool(s->pktout, TRUE);
		    put_stringz(s->pktout, s->password);
		    put_stringz(s->pktout,
				       s->cur_prompt->prompts[1]->result);
		    free_prompts(s->cur_prompt);
                    s->pktout->minlen = 256;
                    ssh2_pkt_send(ssh, s->pktout);
		    logevent("Sent new password");
		    
		    /*
		     * Now see what the server has to say about it.
		     * (If it's CHANGEREQ again, it's not happy with the
		     * new password.)
		     */
		    crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_userauth)) != NULL);
		    changereq_first_time = FALSE;

		}

		/*
		 * We need to reexamine the current pktin at the top
		 * of the loop. Either:
		 *  - we weren't asked to change password at all, in
		 *    which case it's a SUCCESS or FAILURE with the
		 *    usual meaning
		 *  - we sent a new password, and the server was
		 *    either OK with it (SUCCESS or FAILURE w/partial
		 *    success) or unhappy with the _old_ password
		 *    (FAILURE w/o partial success)
		 * In any of these cases, we go back to the top of
		 * the loop and start again.
		 */
                pq_push_front(&ssh->pq_ssh2_userauth, pktin);

		/*
		 * We don't need the old password any more, in any
		 * case. Burn the evidence.
		 */
		smemclr(s->password, strlen(s->password));
		sfree(s->password);

	    } else {
		char *str = dupprintf(
                    "No supported authentication methods available"
                    " (server sent: %.*s)", PTRLEN_PRINTF(methods));

		ssh_disconnect(ssh, str,
			       "No supported authentication methods available",
			       SSH2_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE,
			       FALSE);
		sfree(str);

		crStopV;

	    }

	}
    }
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_BANNER] = NULL;

    /* Clear up various bits and pieces from authentication. */
    if (s->publickey_blob) {
	sfree(s->publickey_algorithm);
	strbuf_free(s->publickey_blob);
	sfree(s->publickey_comment);
    }
    if (s->agent_response)
	sfree(s->agent_response);

    if (s->userauth_success) {
	/*
         * We've just received USERAUTH_SUCCESS, and we haven't sent
         * any packets since. Signal the transport layer to consider
         * doing an immediate rekey, if it has any reason to want to.
	 *
         * (Relying on we_are_in is not sufficient. One of the reasons
         * to do a post-userauth rekey is OpenSSH delayed compression;
         * draft-miller-secsh-compression-delayed is quite clear that
         * that triggers on USERAUTH_SUCCESS specifically, and
         * we_are_in can become set for other reasons.)
	 */
        ssh->rekey_reason = NULL;      /* will be filled in later */
        ssh->rekey_class = RK_POST_USERAUTH;
        queue_idempotent_callback(&ssh->ssh2_transport_icb);
    }

    /*
     * Finally, hand over to the connection layer.
     */
    do_ssh2_connection(ssh);
    ssh->current_user_input_fn = ssh2_connection_input;
    queue_idempotent_callback(&ssh->user_input_consumer);

    crFinishV;
}

static void ssh2_userauth_input(Ssh ssh)
{
    do_ssh2_userauth(ssh);
}

/*
 * Handle the SSH-2 connection layer.
 */
static void ssh2_msg_connection(Ssh ssh, PktIn *pktin)
{
    pktin->refcount++;   /* avoid packet being freed when we return */
    pq_push(&ssh->pq_ssh2_connection, pktin);
    queue_idempotent_callback(&ssh->ssh2_connection_icb);
}

static void ssh2_response_connection(struct ssh_channel *c,
                                     PktIn *pktin, void *ctx)
{
    if (pktin)
        ssh2_msg_connection(c->ssh, pktin);
}

static void ssh2_connection_setup(Ssh ssh)
{
    /*
     * Initially, most connection-protocol messages go to the function
     * that queues them for handling by the main do_ssh2_connection
     * coroutine.
     */
    ssh->packet_dispatch[SSH2_MSG_REQUEST_SUCCESS] = ssh2_msg_connection;
    ssh->packet_dispatch[SSH2_MSG_REQUEST_FAILURE] = ssh2_msg_connection;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN] = ssh2_msg_connection;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN_CONFIRMATION] =
        ssh2_msg_connection;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN_FAILURE] = ssh2_msg_connection;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_DATA] = ssh2_msg_connection;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_EXTENDED_DATA] = ssh2_msg_connection;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_EOF] = ssh2_msg_connection;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_CLOSE] = ssh2_msg_connection;

    /*
     * But a couple of them are easier to pass to special handler
     * functions right from the start.
     */
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_WINDOW_ADJUST] =
	ssh2_msg_channel_window_adjust;
    ssh->packet_dispatch[SSH2_MSG_GLOBAL_REQUEST] =
	ssh2_msg_global_request;

    /*
     * Ensure our channels tree exists.
     */
    if (!ssh->channels)
        ssh->channels = newtree234(ssh_channelcmp);
}

typedef struct mainchan {
    Ssh ssh;
    struct ssh_channel *c;

    Channel chan;
} mainchan;

static void mainchan_free(Channel *chan);
static void mainchan_open_confirmation(Channel *chan);
static void mainchan_open_failure(Channel *chan, const char *errtext);
static int mainchan_send(Channel *chan, int is_stderr, const void *, int);
static void mainchan_send_eof(Channel *chan);
static void mainchan_set_input_wanted(Channel *chan, int wanted);
static char *mainchan_log_close_msg(Channel *chan);

static const struct ChannelVtable mainchan_channelvt = {
    mainchan_free,
    mainchan_open_confirmation,
    mainchan_open_failure,
    mainchan_send,
    mainchan_send_eof,
    mainchan_set_input_wanted,
    mainchan_log_close_msg,
    chan_no_eager_close,
};

static mainchan *mainchan_new(Ssh ssh)
{
    mainchan *mc = snew(mainchan);
    mc->ssh = ssh;
    mc->c = NULL;
    mc->chan.vt = &mainchan_channelvt;
    mc->chan.initial_fixed_window_size = 0;
    return mc;
}

static void mainchan_free(Channel *chan)
{
    assert(chan->vt == &mainchan_channelvt);
    mainchan *mc = FROMFIELD(chan, mainchan, chan);
    mc->ssh->mainchan = NULL;
    sfree(mc);
}

static void mainchan_open_confirmation(Channel *chan)
{
    assert(FALSE && "OPEN_CONFIRMATION for main channel should be "
           "handled by connection layer setup");
}

static void mainchan_open_failure(Channel *chan, const char *errtext)
{
    assert(FALSE && "OPEN_FAILURE for main channel should be "
           "handled by connection layer setup");
}

static int mainchan_send(Channel *chan, int is_stderr,
                         const void *data, int length)
{
    assert(chan->vt == &mainchan_channelvt);
    mainchan *mc = FROMFIELD(chan, mainchan, chan);
    return from_backend(mc->ssh->frontend, is_stderr, data, length);
}

static void mainchan_send_eof(Channel *chan)
{
    assert(chan->vt == &mainchan_channelvt);
    mainchan *mc = FROMFIELD(chan, mainchan, chan);

    if (!mc->ssh->sent_console_eof &&
        (from_backend_eof(mc->ssh->frontend) || mc->ssh->got_pty)) {
        /*
         * Either from_backend_eof told us that the front end wants us
         * to close the outgoing side of the connection as soon as we
         * see EOF from the far end, or else we've unilaterally
         * decided to do that because we've allocated a remote pty and
         * hence EOF isn't a particularly meaningful concept.
         */
        sshfwd_write_eof(mc->c);
    }
    mc->ssh->sent_console_eof = TRUE;
}

static void mainchan_set_input_wanted(Channel *chan, int wanted)
{
    assert(chan->vt == &mainchan_channelvt);
    mainchan *mc = FROMFIELD(chan, mainchan, chan);

    /*
     * This is the main channel of the SSH session, i.e. the one tied
     * to the standard input (or GUI) of the primary SSH client user
     * interface. So ssh->send_ok is how we control whether we're
     * reading from that input.
     */
    mc->ssh->send_ok = wanted;
}

static char *mainchan_log_close_msg(Channel *chan)
{
    return dupstr("Main session channel closed");
}

static void do_ssh2_connection(void *vctx)
{
    Ssh ssh = (Ssh)vctx;
    PktIn *pktin;

    struct do_ssh2_connection_state {
	int crLine;
	PktOut *pktout;
    };

    crState(do_ssh2_connection_state);

    crBeginState;

    /*
     * Initialise our dispatch table entries. Normally this will have
     * been done as a side effect of USERAUTH_SUCCESS, but in some
     * cases, it might not (e.g. if we bypassed userauth, or if we're
     * running the bare-connection protocol).
     */
    ssh2_connection_setup(ssh);

    /*
     * Create the main session channel.
     */
    if (conf_get_int(ssh->conf, CONF_ssh_no_shell)) {
	ssh->mainchan = NULL;
    } else {
        mainchan *mc = mainchan_new(ssh);

	if (*conf_get_str(ssh->conf, CONF_ssh_nc_host)) {
	    /*
	     * Just start a direct-tcpip channel and use it as the main
	     * channel.
	     */
            ssh->mainchan = mc->c = ssh_send_port_open
                (ssh, conf_get_str(ssh->conf, CONF_ssh_nc_host),
                 conf_get_int(ssh->conf, CONF_ssh_nc_port),
                 "main channel", &mc->chan);
	    ssh->ncmode = TRUE;
	} else {
            ssh->mainchan = mc->c = snew(struct ssh_channel);
            ssh->mainchan->ssh = ssh;
            ssh_channel_init(ssh->mainchan);
            ssh->mainchan->chan = &mc->chan;
	    s->pktout = ssh2_chanopen_init(ssh->mainchan, "session");
	    logevent("Opening session as main channel");
	    ssh2_pkt_send(ssh, s->pktout);
	    ssh->ncmode = FALSE;
	}
	crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_connection)) != NULL);
        if (pktin->type != SSH2_MSG_CHANNEL_OPEN_CONFIRMATION &&
            pktin->type != SSH2_MSG_CHANNEL_OPEN_FAILURE) {
            bombout(("Server sent strange packet %d in response to main "
                     "channel open request", pktin->type));
	    crStopV;
        }
	if (get_uint32(pktin) != ssh->mainchan->localid) {
	    bombout(("Server's response to main channel open cited wrong"
                     " channel number"));
	    crStopV;
	}
	if (pktin->type == SSH2_MSG_CHANNEL_OPEN_FAILURE) {
            char *errtext = ssh2_channel_open_failure_error_text(pktin);
            bombout(("Server refused to open main channel: %s", errtext));
            sfree(errtext);
	    crStopV;
	}

	ssh->mainchan->remoteid = get_uint32(pktin);
	ssh->mainchan->halfopen = FALSE;
	ssh->mainchan->v.v2.remwindow = get_uint32(pktin);
	ssh->mainchan->v.v2.remmaxpkt = get_uint32(pktin);
	update_specials_menu(ssh->frontend);
	logevent("Opened main channel");
    }

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
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_SUCCESS] = ssh2_msg_channel_response;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_FAILURE] = ssh2_msg_channel_response;

    /*
     * Put our current pending packet queue back to the front of
     * pq_full, and then schedule a callback to re-process those
     * packets (if any). That way, anything already in our queue that
     * matches any of the table entries we've just modified will go to
     * the right handler function, and won't come here to confuse us.
     */
    if (pq_empty_on_to_front_of(&ssh->pq_ssh2_connection, &ssh->pq_full))
        queue_idempotent_callback(&ssh->pq_full_consumer);

    /*
     * Now the connection protocol is properly up and running, with
     * all those dispatch table entries, so it's safe to let
     * downstreams start trying to open extra channels through us.
     */
    if (ssh->connshare)
        share_activate(ssh->connshare, ssh->v_s);

    if (ssh->mainchan && ssh_is_simple(ssh)) {
	/*
	 * This message indicates to the server that we promise
	 * not to try to run any other channel in parallel with
	 * this one, so it's safe for it to advertise a very large
	 * window and leave the flow control to TCP.
	 */
	s->pktout = ssh2_chanreq_init(ssh->mainchan,
				      "simple@putty.projects.tartarus.org",
				      NULL, NULL);
	ssh2_pkt_send(ssh, s->pktout);
    }

    /*
     * Enable port forwardings.
     */
    ssh_setup_portfwd(ssh, ssh->conf);

    if (ssh->mainchan && !ssh->ncmode) {
	/*
	 * Send the CHANNEL_REQUESTS for the main session channel.
	 * Each one is handled by its own little asynchronous
	 * co-routine.
	 */

	/* Potentially enable X11 forwarding. */
	if (conf_get_int(ssh->conf, CONF_x11_forward)) {
            ssh->x11disp =
                x11_setup_display(conf_get_str(ssh->conf, CONF_x11_display),
                                  ssh->conf);
            if (!ssh->x11disp) {
                /* FIXME: return an error message from x11_setup_display */
                logevent("X11 forwarding not enabled: unable to"
                         " initialise X display");
            } else {
                ssh->x11auth = x11_invent_fake_auth
                    (ssh->x11authtree, conf_get_int(ssh->conf, CONF_x11_auth));
                ssh->x11auth->disp = ssh->x11disp;

                ssh2_setup_x11(ssh->mainchan, NULL, NULL);
            }
        }

	/* Potentially enable agent forwarding. */
	if (ssh_agent_forwarding_permitted(ssh))
	    ssh2_setup_agent(ssh->mainchan, NULL, NULL);

	/* Now allocate a pty for the session. */
	if (!conf_get_int(ssh->conf, CONF_nopty))
	    ssh2_setup_pty(ssh->mainchan, NULL, NULL);

	/* Send environment variables. */
	ssh2_setup_env(ssh->mainchan, NULL, NULL);

	/*
	 * Start a shell or a remote command. We may have to attempt
	 * this twice if the config data has provided a second choice
	 * of command.
	 */
	while (1) {
	    int subsys;
	    char *cmd;

	    if (ssh->fallback_cmd) {
		subsys = conf_get_int(ssh->conf, CONF_ssh_subsys2);
		cmd = conf_get_str(ssh->conf, CONF_remote_cmd2);
	    } else {
		subsys = conf_get_int(ssh->conf, CONF_ssh_subsys);
		cmd = conf_get_str(ssh->conf, CONF_remote_cmd);
	    }

	    if (subsys) {
		s->pktout = ssh2_chanreq_init(ssh->mainchan, "subsystem",
					      ssh2_response_connection, NULL);
		put_stringz(s->pktout, cmd);
	    } else if (*cmd) {
		s->pktout = ssh2_chanreq_init(ssh->mainchan, "exec",
					      ssh2_response_connection, NULL);
		put_stringz(s->pktout, cmd);
	    } else {
		s->pktout = ssh2_chanreq_init(ssh->mainchan, "shell",
					      ssh2_response_connection, NULL);
	    }
	    ssh2_pkt_send(ssh, s->pktout);

	    crMaybeWaitUntilV((pktin = pq_pop(&ssh->pq_ssh2_connection)) != NULL);

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
		if (!ssh->fallback_cmd &&
		    *conf_get_str(ssh->conf, CONF_remote_cmd2)) {
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
    } else {
	ssh->editing = ssh->echoing = TRUE;
    }

    ssh->state = SSH_STATE_SESSION;
    if (ssh->size_needed)
        backend_size(&ssh->backend, ssh->term_width, ssh->term_height);
    if (ssh->eof_needed)
        backend_special(&ssh->backend, TS_EOF);

    /*
     * Transfer data!
     */
    if (ssh->ldisc)
	ldisc_echoedit_update(ssh->ldisc);  /* cause ldisc to notice changes */
    if (ssh->mainchan)
	ssh->send_ok = 1;
    while (1) {
	if ((pktin = pq_pop(&ssh->pq_ssh2_connection)) != NULL) {

	    /*
	     * _All_ the connection-layer packets we expect to
	     * receive are now handled by the dispatch table.
	     * Anything that reaches here must be bogus.
	     */

	    bombout(("Strange packet received: type %d", pktin->type));
	    crStopV;
	}
        while (ssh->mainchan && bufchain_size(&ssh->user_input) > 0) {
	    /*
	     * Add user input to the main channel's buffer.
	     */
            void *data;
            int len;
            bufchain_prefix(&ssh->user_input, &data, &len);
	    ssh_send_channel_data(ssh->mainchan, data, len);
            bufchain_consume(&ssh->user_input, len);
	}
	crReturnV;
    }

    crFinishV;
}

static void ssh2_connection_input(Ssh ssh)
{
    do_ssh2_connection(ssh);
}

/*
 * Handlers for SSH-2 messages that might arrive at any moment.
 */
static void ssh2_msg_disconnect(Ssh ssh, PktIn *pktin)
{
    /* log reason code in disconnect message */
    char *buf;
    ptrlen msg;
    int reason;

    reason = get_uint32(pktin);
    msg = get_string(pktin);

    if (reason > 0 && reason < lenof(ssh2_disconnect_reasons)) {
	buf = dupprintf("Received disconnect message (%s)",
			ssh2_disconnect_reasons[reason]);
    } else {
	buf = dupprintf("Received disconnect message (unknown"
			" type %d)", reason);
    }
    logevent(buf);
    sfree(buf);
    buf = dupprintf("Disconnection message text: %.*s", PTRLEN_PRINTF(msg));
    logevent(buf);
    bombout(("Server sent disconnect message\ntype %d (%s):\n\"%.*s\"",
	     reason,
	     (reason > 0 && reason < lenof(ssh2_disconnect_reasons)) ?
	     ssh2_disconnect_reasons[reason] : "unknown", PTRLEN_PRINTF(msg)));
    sfree(buf);
}

static void ssh2_msg_debug(Ssh ssh, PktIn *pktin)
{
    /* log the debug message */
    ptrlen msg;

    /* XXX maybe we should actually take notice of the return value */
    get_bool(pktin);
    msg = get_string(pktin);

    logeventf(ssh, "Remote debug message: %.*s", PTRLEN_PRINTF(msg));
}

static void ssh2_msg_transport(Ssh ssh, PktIn *pktin)
{
    pktin->refcount++;   /* avoid packet being freed when we return */
    pq_push(&ssh->pq_ssh2_transport, pktin);
    queue_idempotent_callback(&ssh->ssh2_transport_icb);
}

/*
 * Called if we receive a packet that isn't allowed by the protocol.
 * This only applies to packets whose meaning PuTTY understands.
 * Entirely unknown packets are handled below.
 */
static void ssh2_msg_unexpected(Ssh ssh, PktIn *pktin)
{
    char *buf = dupprintf("Server protocol violation: unexpected %s packet",
			  ssh2_pkt_type(ssh->pls.kctx, ssh->pls.actx,
					pktin->type));
    ssh_disconnect(ssh, NULL, buf, SSH2_DISCONNECT_PROTOCOL_ERROR, FALSE);
    sfree(buf);
}

static void ssh2_msg_something_unimplemented(Ssh ssh, PktIn *pktin)
{
    PktOut *pktout;
    pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_UNIMPLEMENTED);
    put_uint32(pktout, pktin->sequence);
    /*
     * UNIMPLEMENTED messages MUST appear in the same order as the
     * messages they respond to. Hence, never queue them.
     */
    ssh_pkt_write(ssh, pktout);
}

/*
 * Handle the top-level SSH-2 protocol.
 */
static void ssh2_protocol_setup(Ssh ssh)
{
    int i;

    ssh->bpp = ssh2_bpp_new();

#ifndef NO_GSSAPI
    /* Load and pick the highest GSS library on the preference list. */
    if (!ssh->gsslibs)
        ssh->gsslibs = ssh_gss_setup(ssh->conf);
    ssh->gsslib = NULL;
    if (ssh->gsslibs->nlibraries > 0) {
        int i, j;
        for (i = 0; i < ngsslibs; i++) {
            int want_id = conf_get_int_int(ssh->conf,
                                           CONF_ssh_gsslist, i);
            for (j = 0; j < ssh->gsslibs->nlibraries; j++)
                if (ssh->gsslibs->libraries[j].id == want_id) {
                    ssh->gsslib = &ssh->gsslibs->libraries[j];
                    goto got_gsslib;   /* double break */
                }
        }
        got_gsslib:
        /*
         * We always expect to have found something in
         * the above loop: we only came here if there
         * was at least one viable GSS library, and the
         * preference list should always mention
         * everything and only change the order.
         */
        assert(ssh->gsslib);
    }
#endif

    /*
     * Most messages cause SSH2_MSG_UNIMPLEMENTED.
     */
    for (i = 0; i < SSH_MAX_MSG; i++)
	ssh->packet_dispatch[i] = ssh2_msg_something_unimplemented;

    /*
     * Initially, we only accept transport messages (and a few generic
     * ones). do_ssh2_userauth and do_ssh2_connection will each add
     * more when they start. Messages that are understood but not
     * currently acceptable go to ssh2_msg_unexpected.
     */
    ssh->packet_dispatch[SSH2_MSG_UNIMPLEMENTED] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_SERVICE_REQUEST] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_SERVICE_ACCEPT] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_KEXINIT] = ssh2_msg_transport;
    ssh->packet_dispatch[SSH2_MSG_NEWKEYS] = ssh2_msg_transport;
    ssh->packet_dispatch[SSH2_MSG_KEXDH_INIT] = ssh2_msg_transport;
    ssh->packet_dispatch[SSH2_MSG_KEXDH_REPLY] = ssh2_msg_transport;
    /* ssh->packet_dispatch[SSH2_MSG_KEX_DH_GEX_REQUEST] = ssh2_msg_transport; duplicate case value */
    /* ssh->packet_dispatch[SSH2_MSG_KEX_DH_GEX_GROUP] = ssh2_msg_transport; duplicate case value */
    ssh->packet_dispatch[SSH2_MSG_KEX_DH_GEX_INIT] = ssh2_msg_transport;
    ssh->packet_dispatch[SSH2_MSG_KEX_DH_GEX_REPLY] = ssh2_msg_transport;
    ssh->packet_dispatch[SSH2_MSG_KEXGSS_GROUP] = ssh2_msg_transport;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_REQUEST] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_FAILURE] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_SUCCESS] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_BANNER] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_PK_OK] = ssh2_msg_unexpected;
    /* ssh->packet_dispatch[SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ] = ssh2_msg_unexpected; duplicate case value */
    /* ssh->packet_dispatch[SSH2_MSG_USERAUTH_INFO_REQUEST] = ssh2_msg_unexpected; duplicate case value */
    ssh->packet_dispatch[SSH2_MSG_USERAUTH_INFO_RESPONSE] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_GLOBAL_REQUEST] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_REQUEST_SUCCESS] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_REQUEST_FAILURE] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN_CONFIRMATION] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN_FAILURE] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_WINDOW_ADJUST] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_DATA] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_EXTENDED_DATA] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_EOF] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_CLOSE] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_REQUEST] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_SUCCESS] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_FAILURE] = ssh2_msg_unexpected;

    /*
     * These messages have a special handler from the start.
     */
    ssh->packet_dispatch[SSH2_MSG_DISCONNECT] = ssh2_msg_disconnect;
    ssh->packet_dispatch[SSH2_MSG_IGNORE] = ssh_msg_ignore; /* shared with SSH-1 */
    ssh->packet_dispatch[SSH2_MSG_DEBUG] = ssh2_msg_debug;
}

static void ssh2_bare_connection_protocol_setup(Ssh ssh)
{
    int i;

    ssh->bpp = ssh2_bare_bpp_new();

    /*
     * Most messages cause SSH2_MSG_UNIMPLEMENTED.
     */
    for (i = 0; i < SSH_MAX_MSG; i++)
	ssh->packet_dispatch[i] = ssh2_msg_something_unimplemented;

    /*
     * Initially, we set all ssh-connection messages to 'unexpected';
     * do_ssh2_connection will fill things in properly. We also handle
     * a couple of messages from the transport protocol which aren't
     * related to key exchange (UNIMPLEMENTED, IGNORE, DEBUG,
     * DISCONNECT).
     */
    ssh->packet_dispatch[SSH2_MSG_GLOBAL_REQUEST] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_REQUEST_SUCCESS] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_REQUEST_FAILURE] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN_CONFIRMATION] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_OPEN_FAILURE] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_WINDOW_ADJUST] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_DATA] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_EXTENDED_DATA] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_EOF] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_CLOSE] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_REQUEST] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_SUCCESS] = ssh2_msg_unexpected;
    ssh->packet_dispatch[SSH2_MSG_CHANNEL_FAILURE] = ssh2_msg_unexpected;

    ssh->packet_dispatch[SSH2_MSG_UNIMPLEMENTED] = ssh2_msg_unexpected;

    /*
     * These messages have a special handler from the start.
     */
    ssh->packet_dispatch[SSH2_MSG_DISCONNECT] = ssh2_msg_disconnect;
    ssh->packet_dispatch[SSH2_MSG_IGNORE] = ssh_msg_ignore;
    ssh->packet_dispatch[SSH2_MSG_DEBUG] = ssh2_msg_debug;
}

#ifndef NO_GSSAPI
static PktOut *ssh2_gss_authpacket(Ssh ssh, Ssh_gss_ctx gss_ctx,
                                   const char *authtype)
{
    strbuf *sb;
    PktOut *p;
    Ssh_gss_buf buf;
    Ssh_gss_buf mic;

    /*
     * The mic is computed over the session id + intended
     * USERAUTH_REQUEST packet.
     */
    sb = strbuf_new();
    put_string(sb, ssh->v2_session_id, ssh->v2_session_id_len);
    put_byte(sb, SSH2_MSG_USERAUTH_REQUEST);
    put_stringz(sb, ssh->username);
    put_stringz(sb, "ssh-connection");
    put_stringz(sb, authtype);

    /* Compute the mic */
    buf.value = sb->s;
    buf.length = sb->len;
    ssh->gsslib->get_mic(ssh->gsslib, gss_ctx, &buf, &mic);
    strbuf_free(sb);

    /* Now we can build the real packet */
    if (strcmp(authtype, "gssapi-with-mic") == 0) {
        p = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_USERAUTH_GSSAPI_MIC);
    } else {
        p = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_USERAUTH_REQUEST);
        put_stringz(p, ssh->username);
        put_stringz(p, "ssh-connection");
        put_stringz(p, authtype);
    }
    put_string(p, mic.value, mic.length);

    return p;
}

/*
 * This is called at the beginning of each SSH rekey to determine whether we are
 * GSS capable, and if we did GSS key exchange, and are delegating credentials,
 * it is also called periodically to determine whether we should rekey in order
 * to delegate (more) fresh credentials.  This is called "credential cascading".
 *
 * On Windows, with SSPI, we may not get the credential expiration, as Windows
 * automatically renews from cached passwords, so the credential effectively
 * never expires.  Since we still want to cascade when the local TGT is updated,
 * we use the expiration of a newly obtained context as a proxy for the
 * expiration of the TGT.
 */
static void ssh2_gss_update(Ssh ssh, int definitely_rekeying)
{
    int gss_stat;
    time_t gss_cred_expiry;
    unsigned long mins;
    Ssh_gss_buf gss_sndtok;
    Ssh_gss_buf gss_rcvtok;
    Ssh_gss_ctx gss_ctx;

    ssh->gss_status = 0;

    /*
     * Nothing to do if no GSSAPI libraries are configured or GSSAPI auth is not
     * enabled.
     */
    if (ssh->gsslibs->nlibraries == 0)
        return;
    if (!conf_get_int(ssh->conf, CONF_try_gssapi_auth) &&
        !conf_get_int(ssh->conf, CONF_try_gssapi_kex))
        return;

    /* Import server name and cache it */
    if (ssh->gss_srv_name == GSS_C_NO_NAME) {
        gss_stat = ssh->gsslib->import_name(ssh->gsslib,
                                            ssh->fullhostname,
                                            &ssh->gss_srv_name);
        if (gss_stat != SSH_GSS_OK) {
            if (gss_stat == SSH_GSS_BAD_HOST_NAME)
                logevent("GSSAPI import name failed -"
                         " Bad service name; won't use GSS key exchange");
            else
                logevent("GSSAPI import name failed;"
                         " won't use GSS key exchange");
            return;
        }
    }

    /*
     * Do we (still) have credentials?  Capture the credential expiration when
     * available
     */
    gss_stat = ssh->gsslib->acquire_cred(ssh->gsslib,
                                         &gss_ctx,
                                         &gss_cred_expiry);
    if (gss_stat != SSH_GSS_OK)
        return;

    SSH_GSS_CLEAR_BUF(&gss_sndtok);
    SSH_GSS_CLEAR_BUF(&gss_rcvtok);

    /*
     * When acquire_cred yields no useful expiration, get a proxy for the cred
     * expiration from the context expiration.
     */
    gss_stat = ssh->gsslib->init_sec_context(
        ssh->gsslib, &gss_ctx, ssh->gss_srv_name,
        0 /* don't delegate */, &gss_rcvtok, &gss_sndtok,
        (gss_cred_expiry == GSS_NO_EXPIRATION ? &gss_cred_expiry : NULL),
        &ssh->gss_ctxt_lifetime);

    /* This context was for testing only. */
    if (gss_ctx)
        ssh->gsslib->release_cred(ssh->gsslib, &gss_ctx);

    if (gss_stat != SSH_GSS_OK &&
        gss_stat != SSH_GSS_S_CONTINUE_NEEDED) {
        /*
         * No point in verbosely interrupting the user to tell them we
         * couldn't get GSS credentials, if this was only a check
         * between key exchanges to see if fresh ones were available.
         * When we do do a rekey, this message (if displayed) will
         * appear among the standard rekey blurb, but when we're not,
         * it shouldn't pop up all the time regardless.
         */
        if (definitely_rekeying)
            logeventf(ssh, "No GSSAPI security context available");

        return;
    }

    if (gss_sndtok.length)
        ssh->gsslib->free_tok(ssh->gsslib, &gss_sndtok);

    ssh->gss_status |= GSS_KEX_CAPABLE;

    /*
     * When rekeying to cascade, avoding doing this too close to the context
     * expiration time, since the key exchange might fail.
     */
    if (ssh->gss_ctxt_lifetime < MIN_CTXT_LIFETIME)
        ssh->gss_status |= GSS_CTXT_MAYFAIL;

    /*
     * If we're not delegating credentials, rekeying is not used to refresh
     * them.  We must avoid setting GSS_CRED_UPDATED or GSS_CTXT_EXPIRES when
     * credential delegation is disabled.
     */
    if (conf_get_int(ssh->conf, CONF_gssapifwd) == 0)
        return;

    if (ssh->gss_cred_expiry != GSS_NO_EXPIRATION &&
        difftime(gss_cred_expiry, ssh->gss_cred_expiry) > 0)
        ssh->gss_status |= GSS_CRED_UPDATED;

    mins = conf_get_int(ssh->conf, CONF_gssapirekey);
    mins = rekey_mins(mins, GSS_DEF_REKEY_MINS);
    if (mins > 0 && ssh->gss_ctxt_lifetime <= mins * 60)
        ssh->gss_status |= GSS_CTXT_EXPIRES;
}
#endif

/*
 * The rekey_time is zero except when re-configuring.
 *
 * We either schedule the next timer and return 0, or return 1 to run the
 * callback now, which will call us again to re-schedule on completion.
 */
static int ssh2_timer_update(Ssh ssh, unsigned long rekey_time)
{
    unsigned long mins;
    unsigned long ticks;

    mins = conf_get_int(ssh->conf, CONF_ssh_rekey_time);
    mins = rekey_mins(mins, 60);
    ticks = mins * 60 * TICKSPERSEC;

    /* Handle change from previous setting */
    if (rekey_time != 0 && rekey_time != mins) {
	unsigned long next;
	unsigned long now = GETTICKCOUNT();

	mins = rekey_time;
	ticks = mins * 60 * TICKSPERSEC;
	next = ssh->last_rekey + ticks;

	/* If overdue, caller will rekey synchronously now */
	if (now - ssh->last_rekey > ticks)
	    return 1;
	ticks = next - now;
    }

#ifndef NO_GSSAPI
    if (ssh->gss_kex_used) {
        /*
         * If we've used GSSAPI key exchange, then we should
         * periodically check whether we need to do another one to
         * pass new credentials to the server.
         */
        unsigned long gssmins;

        /* Check cascade conditions more frequently if configured */
        gssmins = conf_get_int(ssh->conf, CONF_gssapirekey);
        gssmins = rekey_mins(gssmins, GSS_DEF_REKEY_MINS);
        if (gssmins > 0) {
            if (gssmins < mins)
                ticks = (mins = gssmins) * 60 * TICKSPERSEC;

            if ((ssh->gss_status & GSS_KEX_CAPABLE) != 0) {
                /*
                 * Run next timer even sooner if it would otherwise be too close
                 * to the context expiration time
                 */
                if ((ssh->gss_status & GSS_CTXT_EXPIRES) == 0 &&
                    ssh->gss_ctxt_lifetime - mins * 60 < 2 * MIN_CTXT_LIFETIME)
                    ticks -= 2 * MIN_CTXT_LIFETIME * TICKSPERSEC;
            }
        }
    }
#endif

    /* Schedule the next timer */
    ssh->next_rekey = schedule_timer(ticks, ssh2_timer, ssh);
    return 0;
}

static void ssh2_timer(void *ctx, unsigned long now)
{
    Ssh ssh = (Ssh)ctx;
    unsigned long mins;
    unsigned long ticks;

    if (ssh->state == SSH_STATE_CLOSED ||
        ssh->kex_in_progress ||
        ssh->bare_connection ||
        now != ssh->next_rekey)
	return;

    mins = conf_get_int(ssh->conf, CONF_ssh_rekey_time);
    mins = rekey_mins(mins, 60);
    if (mins == 0)
	return;

    /* Rekey if enough time has elapsed */
    ticks = mins * 60 * TICKSPERSEC;
    if (now - ssh->last_rekey > ticks - 30*TICKSPERSEC) {
        ssh->rekey_reason = "timeout";
        ssh->rekey_class = RK_NORMAL;
        queue_idempotent_callback(&ssh->ssh2_transport_icb);
        return;
    }

#ifndef NO_GSSAPI
    /*
     * Rekey now if we have a new cred or context expires this cycle, but not if
     * this is unsafe.
     */
    if (conf_get_int(ssh->conf, CONF_gssapirekey)) {
        ssh2_gss_update(ssh, FALSE);
        if ((ssh->gss_status & GSS_KEX_CAPABLE) != 0 &&
            (ssh->gss_status & GSS_CTXT_MAYFAIL) == 0 &&
            (ssh->gss_status & (GSS_CRED_UPDATED|GSS_CTXT_EXPIRES)) != 0) {
            ssh->rekey_reason = "GSS credentials updated";
            ssh->rekey_class = RK_GSS_UPDATE;
            queue_idempotent_callback(&ssh->ssh2_transport_icb);
            return;
        }
    }
#endif

    /* Try again later. */
    (void) ssh2_timer_update(ssh, 0);
}

static void ssh2_general_packet_processing(Ssh ssh, PktIn *pktin)
{
    ssh->incoming_data_size += pktin->encrypted_len;
    if (!ssh->kex_in_progress &&
        ssh->max_data_size != 0 &&
        ssh->incoming_data_size > ssh->max_data_size) {
        ssh->rekey_reason = "too much data received";
        ssh->rekey_class = RK_NORMAL;
        queue_idempotent_callback(&ssh->ssh2_transport_icb);
    }
}

static void ssh_cache_conf_values(Ssh ssh)
{
    ssh->pls.omit_passwords = conf_get_int(ssh->conf, CONF_logomitpass);
    ssh->pls.omit_data = conf_get_int(ssh->conf, CONF_logomitdata);
}

/*
 * Called to set up the connection.
 *
 * Returns an error message, or NULL on success.
 */
static const char *ssh_init(Frontend *frontend, Backend **backend_handle,
			    Conf *conf,
                            const char *host, int port, char **realhost,
			    int nodelay, int keepalive)
{
    const char *p;
    Ssh ssh;

    ssh = snew(struct ssh_tag);
    ssh->conf = conf_copy(conf);
    ssh_cache_conf_values(ssh);
    ssh->version = 0;		       /* when not ready yet */
    ssh->s = NULL;
    ssh->kex = NULL;
    ssh->kex_ctx = NULL;
    ssh->hostkey_alg = NULL;
    ssh->hostkey_str = NULL;
    ssh->exitcode = -1;
    ssh->close_expected = FALSE;
    ssh->clean_exit = FALSE;
    ssh->disconnect_message_seen = FALSE;
    ssh->state = SSH_STATE_PREPACKET;
    ssh->size_needed = FALSE;
    ssh->eof_needed = FALSE;
    ssh->ldisc = NULL;
    ssh->logctx = NULL;
    ssh->fallback_cmd = 0;
    ssh->pls.kctx = SSH2_PKTCTX_NOKEX;
    ssh->pls.actx = SSH2_PKTCTX_NOAUTH;
    ssh->x11disp = NULL;
    ssh->x11auth = NULL;
    ssh->x11authtree = newtree234(x11_authcmp);
    ssh->v2_cbc_ignore_workaround = FALSE;
    ssh->bpp = NULL;
    ssh->do_ssh1_connection_crstate = 0;
    ssh->do_ssh_init_state = NULL;
    ssh->do_ssh_connection_init_state = NULL;
    ssh->do_ssh1_login_state = NULL;
    ssh->do_ssh2_transport_state = NULL;
    ssh->do_ssh2_userauth_state = NULL;
    ssh->do_ssh2_connection_state = NULL;
    bufchain_init(&ssh->incoming_data);
    ssh->incoming_data_seen_eof = FALSE;
    ssh->incoming_data_eof_message = NULL;
    ssh->incoming_data_consumer.fn = ssh_process_incoming_data;
    ssh->incoming_data_consumer.ctx = ssh;
    ssh->incoming_data_consumer.queued = FALSE;
    pq_init(&ssh->pq_full);
    ssh->pq_full_consumer.fn = ssh_process_pq_full;
    ssh->pq_full_consumer.ctx = ssh;
    ssh->pq_full_consumer.queued = FALSE;
    pq_init(&ssh->pq_ssh1_login);
    ssh->ssh1_login_icb.fn = do_ssh1_login;
    ssh->ssh1_login_icb.ctx = ssh;
    ssh->ssh1_login_icb.queued = FALSE;
    pq_init(&ssh->pq_ssh1_connection);
    ssh->ssh1_connection_icb.fn = do_ssh1_connection;
    ssh->ssh1_connection_icb.ctx = ssh;
    ssh->ssh1_connection_icb.queued = FALSE;
    pq_init(&ssh->pq_ssh2_transport);
    ssh->ssh2_transport_icb.fn = do_ssh2_transport;
    ssh->ssh2_transport_icb.ctx = ssh;
    ssh->ssh2_transport_icb.queued = FALSE;
    pq_init(&ssh->pq_ssh2_userauth);
    ssh->ssh2_userauth_icb.fn = do_ssh2_userauth;
    ssh->ssh2_userauth_icb.ctx = ssh;
    ssh->ssh2_userauth_icb.queued = FALSE;
    pq_init(&ssh->pq_ssh2_connection);
    ssh->ssh2_connection_icb.fn = do_ssh2_connection;
    ssh->ssh2_connection_icb.ctx = ssh;
    ssh->ssh2_connection_icb.queued = FALSE;
    bufchain_init(&ssh->user_input);
    ssh->user_input_consumer.fn = ssh_process_user_input;
    ssh->user_input_consumer.ctx = ssh;
    ssh->user_input_consumer.queued = FALSE;
    bufchain_init(&ssh->outgoing_data);
    ssh->outgoing_data_sender.fn = ssh_send_outgoing_data;
    ssh->outgoing_data_sender.ctx = ssh;
    ssh->outgoing_data_sender.queued = FALSE;
    ssh->current_user_input_fn = NULL;
    ssh->rekey_reason = NULL;
    ssh->rekey_class = RK_INITIAL;
    ssh->v_c = NULL;
    ssh->v_s = NULL;
    ssh->mainchan = NULL;
    ssh->throttled_all = 0;
    ssh->v1_stdout_throttling = 0;
    ssh->queue = NULL;
    ssh->queuelen = ssh->queuesize = 0;
    ssh->queueing = FALSE;
    ssh->qhead = ssh->qtail = NULL;
    ssh->deferred_rekey_reason = NULL;
    ssh->frozen = FALSE;
    ssh->username = NULL;
    ssh->sent_console_eof = FALSE;
    ssh->got_pty = FALSE;
    ssh->bare_connection = FALSE;
    ssh->X11_fwd_enabled = FALSE;
    ssh->connshare = NULL;
    ssh->attempting_connshare = FALSE;
    ssh->session_started = FALSE;
    ssh->specials = NULL;
    ssh->n_uncert_hostkeys = 0;
    ssh->cross_certifying = FALSE;

#ifndef NO_GSSAPI
    ssh->gss_cred_expiry = GSS_NO_EXPIRATION;
    ssh->gss_srv_name = GSS_C_NO_NAME;
    ssh->gss_ctx = NULL;
    ssh_init_transient_hostkey_store(ssh);
#endif
    ssh->gss_kex_used = FALSE;

    ssh->backend.vt = &ssh_backend;
    *backend_handle = &ssh->backend;

    ssh->frontend = frontend;
    ssh->term_width = conf_get_int(ssh->conf, CONF_width);
    ssh->term_height = conf_get_int(ssh->conf, CONF_height);

    ssh->channels = NULL;
    ssh->rportfwds = NULL;
    ssh->portfwds = NULL;

    ssh->send_ok = 0;
    ssh->editing = 0;
    ssh->echoing = 0;
    ssh->conn_throttle_count = 0;
    ssh->overall_bufsize = 0;
    ssh->fallback_cmd = 0;

    ssh->general_packet_processing = NULL;

    ssh->pinger = NULL;

    ssh->incoming_data_size = ssh->outgoing_data_size = 0L;
    ssh->max_data_size = parse_blocksize(conf_get_str(ssh->conf,
						      CONF_ssh_rekey_data));
    ssh->kex_in_progress = FALSE;

    ssh->auth_agent_query = NULL;

#ifndef NO_GSSAPI
    ssh->gsslibs = NULL;
#endif

    random_ref(); /* do this now - may be needed by sharing setup code */
    ssh->need_random_unref = TRUE;

    p = connect_to_host(ssh, host, port, realhost, nodelay, keepalive);
    if (p != NULL) {
        /* Call random_unref now instead of waiting until the caller
         * frees this useless Ssh object, in case the caller is
         * impatient and just exits without bothering, in which case
         * the random seed won't be re-saved. */
        ssh->need_random_unref = FALSE;
        random_unref();
	return p;
    }

    return NULL;
}

static void ssh_free(Backend *be)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);
    struct ssh_channel *c;
    struct ssh_rportfwd *pf;
    struct X11FakeAuth *auth;
    int need_random_unref;

    if (ssh->kex_ctx)
	dh_cleanup(ssh->kex_ctx);
    sfree(ssh->savedhost);

    while (ssh->queuelen-- > 0)
	ssh_free_pktout(ssh->queue[ssh->queuelen]);
    sfree(ssh->queue);

    while (ssh->qhead) {
	struct queued_handler *qh = ssh->qhead;
	ssh->qhead = qh->next;
	sfree(qh);
    }
    ssh->qhead = ssh->qtail = NULL;

    if (ssh->channels) {
	while ((c = delpos234(ssh->channels, 0)) != NULL) {
	    ssh_channel_close_local(c, NULL);
	    if (ssh->version == 2) {
		struct outstanding_channel_request *ocr, *nocr;
		ocr = c->v.v2.chanreq_head;
		while (ocr) {
		    ocr->handler(c, NULL, ocr->ctx);
		    nocr = ocr->next;
		    sfree(ocr);
		    ocr = nocr;
		}
		bufchain_clear(&c->v.v2.outbuffer);
	    }
	    sfree(c);
	}
	freetree234(ssh->channels);
	ssh->channels = NULL;
    }

    if (ssh->connshare) {
        sharestate_free(ssh->connshare);
        ssh->connshare = NULL;
    }

    if (ssh->rportfwds) {
	while ((pf = delpos234(ssh->rportfwds, 0)) != NULL)
	    free_rportfwd(pf);
	freetree234(ssh->rportfwds);
	ssh->rportfwds = NULL;
    }
    if (ssh->x11disp)
	x11_free_display(ssh->x11disp);
    while ((auth = delpos234(ssh->x11authtree, 0)) != NULL)
        x11_free_fake_auth(auth);
    if (ssh->bpp)
        ssh_bpp_free(ssh->bpp);
    freetree234(ssh->x11authtree);
    sfree(ssh->do_ssh_init_state);
    sfree(ssh->do_ssh1_login_state);
    sfree(ssh->do_ssh2_transport_state);
    sfree(ssh->do_ssh2_userauth_state);
    sfree(ssh->do_ssh2_connection_state);
    bufchain_clear(&ssh->incoming_data);
    bufchain_clear(&ssh->outgoing_data);
    sfree(ssh->incoming_data_eof_message);
    pq_clear(&ssh->pq_full);
    pq_clear(&ssh->pq_ssh1_login);
    pq_clear(&ssh->pq_ssh1_connection);
    pq_clear(&ssh->pq_ssh2_transport);
    pq_clear(&ssh->pq_ssh2_userauth);
    pq_clear(&ssh->pq_ssh2_connection);
    bufchain_clear(&ssh->user_input);
    sfree(ssh->v_c);
    sfree(ssh->v_s);
    sfree(ssh->fullhostname);
    sfree(ssh->hostkey_str);
    sfree(ssh->specials);
    if (ssh->s)
	ssh_do_close(ssh, TRUE);
    expire_timer_context(ssh);
    if (ssh->pinger)
	pinger_free(ssh->pinger);
    sfree(ssh->username);
    conf_free(ssh->conf);

    if (ssh->auth_agent_query)
        agent_cancel_query(ssh->auth_agent_query);

#ifndef NO_GSSAPI
    if (ssh->gss_srv_name)
        ssh->gsslib->release_name(ssh->gsslib, &ssh->gss_srv_name);
    if (ssh->gss_ctx != NULL)
        ssh->gsslib->release_cred(ssh->gsslib, &ssh->gss_ctx);
    if (ssh->gsslibs)
	ssh_gss_cleanup(ssh->gsslibs);
    ssh_cleanup_transient_hostkey_store(ssh);
#endif
    need_random_unref = ssh->need_random_unref;
    sfree(ssh);

    if (need_random_unref)
        random_unref();
}

/*
 * Reconfigure the SSH backend.
 */
static void ssh_reconfig(Backend *be, Conf *conf)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);
    const char *rekeying = NULL;
    int rekey_mandatory = FALSE;
    unsigned long old_max_data_size;
    int i, rekey_time;

    pinger_reconfig(ssh->pinger, ssh->conf, conf);
    if (ssh->portfwds)
	ssh_setup_portfwd(ssh, conf);

    rekey_time = conf_get_int(conf, CONF_ssh_rekey_time);
    if (ssh2_timer_update(ssh, rekey_mins(rekey_time, 60)))
        rekeying = "timeout shortened";

    old_max_data_size = ssh->max_data_size;
    ssh->max_data_size = parse_blocksize(conf_get_str(ssh->conf,
						      CONF_ssh_rekey_data));
    if (old_max_data_size != ssh->max_data_size &&
	ssh->max_data_size != 0) {
	if (ssh->outgoing_data_size > ssh->max_data_size ||
	    ssh->incoming_data_size > ssh->max_data_size)
	    rekeying = "data limit lowered";
    }

    if (conf_get_int(ssh->conf, CONF_compression) !=
	conf_get_int(conf, CONF_compression)) {
	rekeying = "compression setting changed";
	rekey_mandatory = TRUE;
    }

    for (i = 0; i < CIPHER_MAX; i++)
	if (conf_get_int_int(ssh->conf, CONF_ssh_cipherlist, i) !=
	    conf_get_int_int(conf, CONF_ssh_cipherlist, i)) {
	rekeying = "cipher settings changed";
	rekey_mandatory = TRUE;
    }
    if (conf_get_int(ssh->conf, CONF_ssh2_des_cbc) !=
	conf_get_int(conf, CONF_ssh2_des_cbc)) {
	rekeying = "cipher settings changed";
	rekey_mandatory = TRUE;
    }

    conf_free(ssh->conf);
    ssh->conf = conf_copy(conf);
    ssh_cache_conf_values(ssh);

    if (!ssh->bare_connection && rekeying) {
	if (!ssh->kex_in_progress) {
            ssh->rekey_reason = rekeying;
            ssh->rekey_class = RK_NORMAL;
            queue_idempotent_callback(&ssh->ssh2_transport_icb);
	} else if (rekey_mandatory) {
	    ssh->deferred_rekey_reason = rekeying;
	}
    }
}

/*
 * Called to send data down the SSH connection.
 */
static int ssh_send(Backend *be, const char *buf, int len)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);

    if (ssh == NULL || ssh->s == NULL)
	return 0;

    bufchain_add(&ssh->user_input, buf, len);
    queue_idempotent_callback(&ssh->user_input_consumer);

    return backend_sendbuffer(&ssh->backend);
}

/*
 * Called to query the current amount of buffered stdin data.
 */
static int ssh_sendbuffer(Backend *be)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);
    int override_value;

    if (ssh == NULL || ssh->s == NULL)
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
	if (!ssh->mainchan)
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
static void ssh_size(Backend *be, int width, int height)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);
    PktOut *pktout;

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
	if (!conf_get_int(ssh->conf, CONF_nopty)) {
	    if (ssh->version == 1) {
                pktout = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_WINDOW_SIZE);
                put_uint32(pktout, ssh->term_height);
                put_uint32(pktout, ssh->term_width);
                put_uint32(pktout, 0);
                put_uint32(pktout, 0);
                ssh_pkt_write(ssh, pktout);
	    } else if (ssh->mainchan) {
		pktout = ssh2_chanreq_init(ssh->mainchan, "window-change",
					   NULL, NULL);
		put_uint32(pktout, ssh->term_width);
		put_uint32(pktout, ssh->term_height);
		put_uint32(pktout, 0);
		put_uint32(pktout, 0);
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
static const struct telnet_special *ssh_get_specials(Backend *be)
{
    static const struct telnet_special ssh1_ignore_special[] = {
	{"IGNORE message", TS_NOP}
    };
    static const struct telnet_special ssh2_ignore_special[] = {
	{"IGNORE message", TS_NOP},
    };
    static const struct telnet_special ssh2_rekey_special[] = {
	{"Repeat key exchange", TS_REKEY},
    };
    static const struct telnet_special ssh2_session_specials[] = {
	{NULL, TS_SEP},
	{"Break", TS_BRK},
	/* These are the signal names defined by RFC 4254.
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

    struct telnet_special *specials = NULL;
    int nspecials = 0, specialsize = 0;

    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);

    sfree(ssh->specials);

#define ADD_SPECIALS(name) do                                           \
    {                                                                   \
        int len = lenof(name);                                          \
        if (nspecials + len > specialsize) {                            \
            specialsize = (nspecials + len) * 5 / 4 + 32;               \
            specials = sresize(specials, specialsize, struct telnet_special); \
        }                                                               \
	memcpy(specials+nspecials, name, len*sizeof(struct telnet_special)); \
        nspecials += len;                                               \
    } while (0)

    if (ssh->version == 1) {
	/* Don't bother offering IGNORE if we've decided the remote
	 * won't cope with it, since we wouldn't bother sending it if
	 * asked anyway. */
	if (!(ssh->remote_bugs & BUG_CHOKES_ON_SSH1_IGNORE))
	    ADD_SPECIALS(ssh1_ignore_special);
    } else if (ssh->version == 2) {
	if (!(ssh->remote_bugs & BUG_CHOKES_ON_SSH2_IGNORE))
	    ADD_SPECIALS(ssh2_ignore_special);
	if (!(ssh->remote_bugs & BUG_SSH2_REKEY) && !ssh->bare_connection)
	    ADD_SPECIALS(ssh2_rekey_special);
	if (ssh->mainchan)
	    ADD_SPECIALS(ssh2_session_specials);

        if (ssh->n_uncert_hostkeys) {
            static const struct telnet_special uncert_start[] = {
                {NULL, TS_SEP},
                {"Cache new host key type", TS_SUBMENU},
            };
            static const struct telnet_special uncert_end[] = {
                {NULL, TS_EXITMENU},
            };
            int i;

            ADD_SPECIALS(uncert_start);
            for (i = 0; i < ssh->n_uncert_hostkeys; i++) {
                struct telnet_special uncert[1];
                const ssh_keyalg *alg =
                    hostkey_algs[ssh->uncert_hostkeys[i]].alg;
                uncert[0].name = alg->ssh_id;
                uncert[0].code = TS_LOCALSTART + ssh->uncert_hostkeys[i];
                ADD_SPECIALS(uncert);
            }
            ADD_SPECIALS(uncert_end);
        }
    } /* else we're not ready yet */

    if (nspecials)
	ADD_SPECIALS(specials_end);

    ssh->specials = specials;

    if (nspecials) {
        return specials;
    } else {
	return NULL;
    }
#undef ADD_SPECIALS
}

/*
 * Send special codes. TS_EOF is useful for `plink', so you
 * can send an EOF and collect resulting output (e.g. `plink
 * hostname sort').
 */
static void ssh_special(Backend *be, Telnet_Special code)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);
    PktOut *pktout;

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
            pktout = ssh_bpp_new_pktout(ssh->bpp, SSH1_CMSG_EOF);
            ssh_pkt_write(ssh, pktout);
	} else if (ssh->mainchan) {
            sshfwd_write_eof(ssh->mainchan);
            ssh->send_ok = 0;          /* now stop trying to read from stdin */
	}
	logevent("Sent EOF message");
    } else if (code == TS_PING || code == TS_NOP) {
	if (ssh->state == SSH_STATE_CLOSED
	    || ssh->state == SSH_STATE_PREPACKET) return;
	if (ssh->version == 1) {
	    if (!(ssh->remote_bugs & BUG_CHOKES_ON_SSH1_IGNORE)) {
                pktout = ssh_bpp_new_pktout(ssh->bpp, SSH1_MSG_IGNORE);
                put_stringz(pktout, "");
                ssh_pkt_write(ssh, pktout);
            }
	} else {
	    if (!(ssh->remote_bugs & BUG_CHOKES_ON_SSH2_IGNORE)) {
		pktout = ssh_bpp_new_pktout(ssh->bpp, SSH2_MSG_IGNORE);
		put_stringz(pktout, "");
		ssh_pkt_write(ssh, pktout);
	    }
	}
    } else if (code == TS_REKEY) {
	if (!ssh->kex_in_progress && !ssh->bare_connection &&
            ssh->version == 2) {
            ssh->rekey_reason = "at user request";
            ssh->rekey_class = RK_NORMAL;
            queue_idempotent_callback(&ssh->ssh2_transport_icb);
	}
    } else if (code >= TS_LOCALSTART) {
        ssh->hostkey_alg = hostkey_algs[code - TS_LOCALSTART].alg;
        ssh->cross_certifying = TRUE;
	if (!ssh->kex_in_progress && !ssh->bare_connection &&
            ssh->version == 2) {
            ssh->rekey_reason = "cross-certifying new host key";
            ssh->rekey_class = RK_NORMAL;
            queue_idempotent_callback(&ssh->ssh2_transport_icb);
	}
    } else if (code == TS_BRK) {
	if (ssh->state == SSH_STATE_CLOSED
	    || ssh->state == SSH_STATE_PREPACKET) return;
	if (ssh->version == 1) {
	    logevent("Unable to send BREAK signal in SSH-1");
	} else if (ssh->mainchan) {
	    pktout = ssh2_chanreq_init(ssh->mainchan, "break", NULL, NULL);
	    put_uint32(pktout, 0);   /* default break length */
	    ssh2_pkt_send(ssh, pktout);
	}
    } else {
	/* Is is a POSIX signal? */
	const char *signame = NULL;
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
		pktout = ssh2_chanreq_init(ssh->mainchan, "signal", NULL, NULL);
		put_stringz(pktout, signame);
		ssh2_pkt_send(ssh, pktout);
		logeventf(ssh, "Sent signal SIG%s", signame);
	    }
	} else {
	    /* Never heard of it. Do nothing */
	}
    }
}

unsigned ssh_alloc_sharing_channel(Ssh ssh, ssh_sharing_connstate *connstate)
{
    struct ssh_channel *c;
    c = snew(struct ssh_channel);

    c->ssh = ssh;
    ssh_channel_init(c);
    c->chan = NULL;
    c->sharectx = connstate;
    return c->localid;
}

void ssh_delete_sharing_channel(Ssh ssh, unsigned localid)
{
    struct ssh_channel *c;

    c = find234(ssh->channels, &localid, ssh_channelfind);
    if (c)
        ssh_channel_destroy(c);
}

void ssh_send_packet_from_downstream(Ssh ssh, unsigned id, int type,
                                     const void *data, int datalen,
                                     const char *additional_log_text)
{
    PktOut *pkt;

    pkt = ssh_bpp_new_pktout(ssh->bpp, type);
    pkt->downstream_id = id;
    pkt->additional_log_text = additional_log_text;
    put_data(pkt, data, datalen);
    ssh2_pkt_send(ssh, pkt);
}

/*
 * This is called when stdout/stderr (the entity to which
 * from_backend sends data) manages to clear some backlog.
 */
static void ssh_unthrottle(Backend *be, int bufsize)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);

    if (ssh->version == 1) {
	if (ssh->v1_stdout_throttling && bufsize < SSH1_BUFFER_LIMIT) {
	    ssh->v1_stdout_throttling = 0;
	    ssh_throttle_conn(ssh, -1);
	}
    } else {
	if (ssh->mainchan)
	    ssh_channel_unthrottle(ssh->mainchan, bufsize);
    }

    /*
     * Now process any SSH connection data that was stashed in our
     * queue while we were frozen.
     */
    queue_idempotent_callback(&ssh->incoming_data_consumer);
}

struct ssh_channel *ssh_send_port_open(Ssh ssh, const char *hostname, int port,
                                       const char *org, Channel *chan)
{
    struct ssh_channel *c = snew(struct ssh_channel);
    PktOut *pktout;

    c->ssh = ssh;
    ssh_channel_init(c);
    c->halfopen = TRUE;
    c->chan = chan;

    logeventf(ssh, "Opening connection to %s:%d for %s", hostname, port, org);

    if (ssh->version == 1) {
        pktout = ssh_bpp_new_pktout(ssh->bpp, SSH1_MSG_PORT_OPEN);
        put_uint32(pktout, c->localid);
        put_stringz(pktout, hostname);
        put_uint32(pktout, port);
        /* originator string would go here, but we didn't specify
         * SSH_PROTOFLAG_HOST_IN_FWD_OPEN */
        ssh_pkt_write(ssh, pktout);
    } else {
	pktout = ssh2_chanopen_init(c, "direct-tcpip");
        {
            char *trimmed_host = host_strduptrim(hostname);
            put_stringz(pktout, trimmed_host);
            sfree(trimmed_host);
        }
	put_uint32(pktout, port);
	/*
	 * We make up values for the originator data; partly it's
	 * too much hassle to keep track, and partly I'm not
	 * convinced the server should be told details like that
	 * about my local network configuration.
	 * The "originator IP address" is syntactically a numeric
	 * IP address, and some servers (e.g., Tectia) get upset
	 * if it doesn't match this syntax.
	 */
	put_stringz(pktout, "0.0.0.0");
	put_uint32(pktout, 0);
	ssh2_pkt_send(ssh, pktout);
    }

    return c;
}

static int ssh_connected(Backend *be)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);
    return ssh->s != NULL;
}

static int ssh_sendok(Backend *be)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);
    return ssh->send_ok;
}

static int ssh_ldisc(Backend *be, int option)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);
    if (option == LD_ECHO)
	return ssh->echoing;
    if (option == LD_EDIT)
	return ssh->editing;
    return FALSE;
}

static void ssh_provide_ldisc(Backend *be, Ldisc *ldisc)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);
    ssh->ldisc = ldisc;
}

static void ssh_provide_logctx(Backend *be, LogContext *logctx)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);
    ssh->logctx = logctx;
}

static int ssh_return_exitcode(Backend *be)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);
    if (ssh->s != NULL)
        return -1;
    else
        return (ssh->exitcode >= 0 ? ssh->exitcode : INT_MAX);
}

/*
 * cfg_info for SSH is the protocol running in this session.
 * (1 or 2 for the full SSH-1 or SSH-2 protocol; -1 for the bare
 * SSH-2 connection protocol, i.e. a downstream; 0 for not-decided-yet.)
 */
static int ssh_cfg_info(Backend *be)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);
    if (ssh->version == 0)
	return 0; /* don't know yet */
    else if (ssh->bare_connection)
	return -1;
    else
	return ssh->version;
}

/*
 * Gross hack: pscp will try to start SFTP but fall back to scp1 if
 * that fails. This variable is the means by which scp.c can reach
 * into the SSH code and find out which one it got.
 */
extern int ssh_fallback_cmd(Backend *be)
{
    Ssh ssh = FROMFIELD(be, struct ssh_tag, backend);
    return ssh->fallback_cmd;
}

const struct Backend_vtable ssh_backend = {
    ssh_init,
    ssh_free,
    ssh_reconfig,
    ssh_send,
    ssh_sendbuffer,
    ssh_size,
    ssh_special,
    ssh_get_specials,
    ssh_connected,
    ssh_return_exitcode,
    ssh_sendok,
    ssh_ldisc,
    ssh_provide_ldisc,
    ssh_provide_logctx,
    ssh_unthrottle,
    ssh_cfg_info,
    ssh_test_for_upstream,
    "ssh",
    PROT_SSH,
    22
};
