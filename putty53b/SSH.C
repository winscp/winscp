#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#include "putty.h"
#include "tree234.h"
#include "ssh.h"

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#define logevent(s) { logevent(s); \
                      if ((flags & FLAG_STDERR) && (flags & FLAG_VERBOSE)) \
                      { fprintf(stderr, "%s\n", s); fflush(stderr); } }

/* logevent, only printf-formatted. */
void logeventf(char *fmt, ...)
{
    va_list ap;
    char stuff[512];

    va_start(ap, fmt);
    vsprintf(stuff, fmt, ap);
    va_end(ap);
    logevent(stuff);
}

#define bombout(msg) ( ssh_state = SSH_STATE_CLOSED, \
                          (s ? sk_close(s), s = NULL : 0), \
                          logeventf msg, connection_fatal msg )

#define SSH1_MSG_DISCONNECT                       1	/* 0x1 */
#define SSH1_SMSG_PUBLIC_KEY                      2	/* 0x2 */
#define SSH1_CMSG_SESSION_KEY                     3	/* 0x3 */
#define SSH1_CMSG_USER                            4	/* 0x4 */
#define SSH1_CMSG_AUTH_RSA                        6	/* 0x6 */
#define SSH1_SMSG_AUTH_RSA_CHALLENGE              7	/* 0x7 */
#define SSH1_CMSG_AUTH_RSA_RESPONSE               8	/* 0x8 */
#define SSH1_CMSG_AUTH_PASSWORD                   9	/* 0x9 */
#define SSH1_CMSG_REQUEST_PTY                     10	/* 0xa */
#define SSH1_CMSG_WINDOW_SIZE                     11	/* 0xb */
#define SSH1_CMSG_EXEC_SHELL                      12	/* 0xc */
#define SSH1_CMSG_EXEC_CMD                        13	/* 0xd */
#define SSH1_SMSG_SUCCESS                         14	/* 0xe */
#define SSH1_SMSG_FAILURE                         15	/* 0xf */
#define SSH1_CMSG_STDIN_DATA                      16	/* 0x10 */
#define SSH1_SMSG_STDOUT_DATA                     17	/* 0x11 */
#define SSH1_SMSG_STDERR_DATA                     18	/* 0x12 */
#define SSH1_CMSG_EOF                             19	/* 0x13 */
#define SSH1_SMSG_EXIT_STATUS                     20	/* 0x14 */
#define SSH1_MSG_CHANNEL_OPEN_CONFIRMATION        21	/* 0x15 */
#define SSH1_MSG_CHANNEL_OPEN_FAILURE             22	/* 0x16 */
#define SSH1_MSG_CHANNEL_DATA                     23	/* 0x17 */
#define SSH1_MSG_CHANNEL_CLOSE                    24	/* 0x18 */
#define SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION       25	/* 0x19 */
#define SSH1_SMSG_X11_OPEN                        27	/* 0x1b */
#define SSH1_CMSG_PORT_FORWARD_REQUEST            28	/* 0x1c */
#define SSH1_MSG_PORT_OPEN                        29	/* 0x1d */
#define SSH1_CMSG_AGENT_REQUEST_FORWARDING        30	/* 0x1e */
#define SSH1_SMSG_AGENT_OPEN                      31	/* 0x1f */
#define SSH1_MSG_IGNORE                           32	/* 0x20 */
#define SSH1_CMSG_EXIT_CONFIRMATION               33	/* 0x21 */
#define SSH1_CMSG_X11_REQUEST_FORWARDING          34	/* 0x22 */
#define SSH1_CMSG_AUTH_RHOSTS_RSA                 35	/* 0x23 */
#define SSH1_MSG_DEBUG                            36	/* 0x24 */
#define SSH1_CMSG_REQUEST_COMPRESSION             37	/* 0x25 */
#define SSH1_CMSG_AUTH_TIS                        39	/* 0x27 */
#define SSH1_SMSG_AUTH_TIS_CHALLENGE              40	/* 0x28 */
#define SSH1_CMSG_AUTH_TIS_RESPONSE               41	/* 0x29 */
#define SSH1_CMSG_AUTH_CCARD                      70	/* 0x46 */
#define SSH1_SMSG_AUTH_CCARD_CHALLENGE            71	/* 0x47 */
#define SSH1_CMSG_AUTH_CCARD_RESPONSE             72	/* 0x48 */

#define SSH1_AUTH_TIS                             5	/* 0x5 */
#define SSH1_AUTH_CCARD                           16	/* 0x10 */

#define SSH1_PROTOFLAG_SCREEN_NUMBER              1	/* 0x1 */
/* Mask for protoflags we will echo back to server if seen */
#define SSH1_PROTOFLAGS_SUPPORTED                 0	/* 0x1 */

#define SSH2_MSG_DISCONNECT                       1	/* 0x1 */
#define SSH2_MSG_IGNORE                           2	/* 0x2 */
#define SSH2_MSG_UNIMPLEMENTED                    3	/* 0x3 */
#define SSH2_MSG_DEBUG                            4	/* 0x4 */
#define SSH2_MSG_SERVICE_REQUEST                  5	/* 0x5 */
#define SSH2_MSG_SERVICE_ACCEPT                   6	/* 0x6 */
#define SSH2_MSG_KEXINIT                          20	/* 0x14 */
#define SSH2_MSG_NEWKEYS                          21	/* 0x15 */
#define SSH2_MSG_KEXDH_INIT                       30	/* 0x1e */
#define SSH2_MSG_KEXDH_REPLY                      31	/* 0x1f */
#define SSH2_MSG_KEX_DH_GEX_REQUEST               30	/* 0x1e */
#define SSH2_MSG_KEX_DH_GEX_GROUP                 31	/* 0x1f */
#define SSH2_MSG_KEX_DH_GEX_INIT                  32	/* 0x20 */
#define SSH2_MSG_KEX_DH_GEX_REPLY                 33	/* 0x21 */
#define SSH2_MSG_USERAUTH_REQUEST                 50	/* 0x32 */
#define SSH2_MSG_USERAUTH_FAILURE                 51	/* 0x33 */
#define SSH2_MSG_USERAUTH_SUCCESS                 52	/* 0x34 */
#define SSH2_MSG_USERAUTH_BANNER                  53	/* 0x35 */
#define SSH2_MSG_USERAUTH_PK_OK                   60	/* 0x3c */
#define SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ        60	/* 0x3c */
#define SSH2_MSG_USERAUTH_INFO_REQUEST            60	/* 0x3c */
#define SSH2_MSG_USERAUTH_INFO_RESPONSE           61	/* 0x3d */
#define SSH2_MSG_GLOBAL_REQUEST                   80	/* 0x50 */
#define SSH2_MSG_REQUEST_SUCCESS                  81	/* 0x51 */
#define SSH2_MSG_REQUEST_FAILURE                  82	/* 0x52 */
#define SSH2_MSG_CHANNEL_OPEN                     90	/* 0x5a */
#define SSH2_MSG_CHANNEL_OPEN_CONFIRMATION        91	/* 0x5b */
#define SSH2_MSG_CHANNEL_OPEN_FAILURE             92	/* 0x5c */
#define SSH2_MSG_CHANNEL_WINDOW_ADJUST            93	/* 0x5d */
#define SSH2_MSG_CHANNEL_DATA                     94	/* 0x5e */
#define SSH2_MSG_CHANNEL_EXTENDED_DATA            95	/* 0x5f */
#define SSH2_MSG_CHANNEL_EOF                      96	/* 0x60 */
#define SSH2_MSG_CHANNEL_CLOSE                    97	/* 0x61 */
#define SSH2_MSG_CHANNEL_REQUEST                  98	/* 0x62 */
#define SSH2_MSG_CHANNEL_SUCCESS                  99	/* 0x63 */
#define SSH2_MSG_CHANNEL_FAILURE                  100	/* 0x64 */

/*
 * Packet type contexts, so that ssh2_pkt_type can correctly decode
 * the ambiguous type numbers back into the correct type strings.
 */
#define SSH2_PKTCTX_DHGROUP1         0x0001
#define SSH2_PKTCTX_DHGEX            0x0002
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
#define BUG_SSH2_DH_GEX                          64

static int ssh_pkt_ctx = 0;

#define translate(x) if (type == x) return #x
#define translatec(x,ctx) if (type == x && (ssh_pkt_ctx & ctx)) return #x
char *ssh1_pkt_type(int type)
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
char *ssh2_pkt_type(int type)
{
    translate(SSH2_MSG_DISCONNECT);
    translate(SSH2_MSG_IGNORE);
    translate(SSH2_MSG_UNIMPLEMENTED);
    translate(SSH2_MSG_DEBUG);
    translate(SSH2_MSG_SERVICE_REQUEST);
    translate(SSH2_MSG_SERVICE_ACCEPT);
    translate(SSH2_MSG_KEXINIT);
    translate(SSH2_MSG_NEWKEYS);
    translatec(SSH2_MSG_KEXDH_INIT, SSH2_PKTCTX_DHGROUP1);
    translatec(SSH2_MSG_KEXDH_REPLY, SSH2_PKTCTX_DHGROUP1);
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

enum { PKT_END, PKT_INT, PKT_CHAR, PKT_DATA, PKT_STR, PKT_BIGNUM };

/* Coroutine mechanics for the sillier bits of the code */
#define crBegin1	static int crLine = 0;
#define crBegin2	switch(crLine) { case 0:;
#define crBegin		crBegin1; crBegin2;
#define crFinish(z)	} crLine = 0; return (z)
#define crFinishV	} crLine = 0; return
#define crReturn(z)	\
	do {\
	    crLine=__LINE__; return (z); case __LINE__:;\
	} while (0)
#define crReturnV	\
	do {\
	    crLine=__LINE__; return; case __LINE__:;\
	} while (0)
#define crStop(z)	do{ crLine = 0; return (z); }while(0)
#define crStopV		do{ crLine = 0; return; }while(0)
#define crWaitUntil(c)	do { crReturn(0); } while (!(c))
#define crWaitUntilV(c)	do { crReturnV; } while (!(c))

extern char *x11_init(Socket *, char *, void *);
extern void x11_close(Socket);
extern int x11_send(Socket, char *, int);
extern void x11_invent_auth(char *, int, char *, int);
extern void x11_unthrottle(Socket s);
extern void x11_override_throttle(Socket s, int enable);

extern char *pfd_newconnect(Socket * s, char *hostname, int port, void *c);
extern char *pfd_addforward(char *desthost, int destport, int port);
extern void pfd_close(Socket s);
extern int pfd_send(Socket s, char *data, int len);
extern void pfd_confirm(Socket s);
extern void pfd_unthrottle(Socket s);
extern void pfd_override_throttle(Socket s, int enable);

static void ssh2_pkt_init(int pkt_type);
static void ssh2_pkt_addbool(unsigned char value);
static void ssh2_pkt_adduint32(unsigned long value);
static void ssh2_pkt_addstring_start(void);
static void ssh2_pkt_addstring_str(char *data);
static void ssh2_pkt_addstring_data(char *data, int len);
static void ssh2_pkt_addstring(char *data);
static char *ssh2_mpint_fmt(Bignum b, int *len);
static void ssh2_pkt_addmp(Bignum b);
static int ssh2_pkt_construct(void);
static void ssh2_pkt_send(void);

/*
 * Buffer management constants. There are several of these for
 * various different purposes:
 * 
 *  - SSH1_BUFFER_LIMIT is the amount of backlog that must build up
 *    on a local data stream before we throttle the whole SSH
 *    connection (in SSH1 only). Throttling the whole connection is
 *    pretty drastic so we set this high in the hope it won't
 *    happen very often.
 * 
 *  - SSH_MAX_BACKLOG is the amount of backlog that must build up
 *    on the SSH connection itself before we defensively throttle
 *    _all_ local data streams. This is pretty drastic too (though
 *    thankfully unlikely in SSH2 since the window mechanism should
 *    ensure that the server never has any need to throttle its end
 *    of the connection), so we set this high as well.
 * 
 *  - OUR_V2_WINSIZE is the maximum window size we present on SSH2
 *    channels.
 */

#define SSH1_BUFFER_LIMIT 32768
#define SSH_MAX_BACKLOG 32768
#define OUR_V2_WINSIZE 16384

const static struct ssh_kex *kex_algs[] = {
    &ssh_diffiehellman_gex,
    &ssh_diffiehellman
};

const static struct ssh_signkey *hostkey_algs[] = { &ssh_rsa, &ssh_dss };

static void nullmac_key(unsigned char *key)
{
}
static void nullmac_generate(unsigned char *blk, int len,
			     unsigned long seq)
{
}
static int nullmac_verify(unsigned char *blk, int len, unsigned long seq)
{
    return 1;
}
const static struct ssh_mac ssh_mac_none = {
    nullmac_key, nullmac_key, nullmac_generate, nullmac_verify, "none", 0
};
const static struct ssh_mac *macs[] = {
    &ssh_sha1, &ssh_md5, &ssh_mac_none
};
const static struct ssh_mac *buggymacs[] = {
    &ssh_sha1_buggy, &ssh_md5, &ssh_mac_none
};

static void ssh_comp_none_init(void)
{
}
static int ssh_comp_none_block(unsigned char *block, int len,
			       unsigned char **outblock, int *outlen)
{
    return 0;
}
static int ssh_comp_none_disable(void)
{
    return 0;
}
const static struct ssh_compress ssh_comp_none = {
    "none",
    ssh_comp_none_init, ssh_comp_none_block,
    ssh_comp_none_init, ssh_comp_none_block,
    ssh_comp_none_disable
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
    unsigned remoteid, localid;
    int type;
    /*
     * In SSH1, this value contains four bits:
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
	    int lensofar, totallen;
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
 * 2-3-4 tree storing remote->local port forwardings. SSH 1 and SSH
 * 2 use this structure in different ways, reflecting SSH 2's
 * altogether saner approach to port forwarding.
 * 
 * In SSH 1, you arrange a remote forwarding by sending the server
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
 * In SSH 2, each side of the connection minds its own business and
 * doesn't send unnecessary information to the other. You arrange a
 * remote forwarding by sending the server just the remote port
 * number. When a connection comes in, the server tells you which
 * of its ports was connected to; and _you_ have to remember what
 * local host:port pair went with that port number.
 * 
 * Hence: in SSH 1 this structure stores host:port pairs we intend
 * to allow connections to, and is indexed by those host:port
 * pairs. In SSH 2 it stores a mapping from source port to
 * destination host:port pair, and is indexed by source port.
 */
struct ssh_rportfwd {
    unsigned sport, dport;
    char dhost[256];
};

struct Packet {
    long length;
    int type;
    unsigned char *data;
    unsigned char *body;
    long savedpos;
    long maxlen;
};

static SHA_State exhash, exhashbase;

static Socket s = NULL;

static unsigned char session_key[32];
static int ssh1_compressing;
static int ssh1_remote_protoflags;
static int ssh1_local_protoflags;
static int ssh_agentfwd_enabled;
static int ssh_X11_fwd_enabled;
static int ssh_remote_bugs;
static const struct ssh_cipher *cipher = NULL;
static const struct ssh2_cipher *cscipher = NULL;
static const struct ssh2_cipher *sccipher = NULL;
static const struct ssh_mac *csmac = NULL;
static const struct ssh_mac *scmac = NULL;
static const struct ssh_compress *cscomp = NULL;
static const struct ssh_compress *sccomp = NULL;
static const struct ssh_kex *kex = NULL;
static const struct ssh_signkey *hostkey = NULL;
static unsigned char ssh2_session_id[20];

static char *savedhost;
static int savedport;
static int ssh_send_ok;
static int ssh_echoing, ssh_editing;

static tree234 *ssh_channels;	       /* indexed by local id */
static struct ssh_channel *mainchan;   /* primary session channel */
static int ssh_exitcode = -1;

static tree234 *ssh_rportfwds;

static enum {
    SSH_STATE_PREPACKET,
    SSH_STATE_BEFORE_SIZE,
    SSH_STATE_INTERMED,
    SSH_STATE_SESSION,
    SSH_STATE_CLOSED
} ssh_state = SSH_STATE_PREPACKET;

static int size_needed = FALSE, eof_needed = FALSE;

static struct Packet pktin = { 0, 0, NULL, NULL, 0 };
static struct Packet pktout = { 0, 0, NULL, NULL, 0 };
static unsigned char *deferred_send_data = NULL;
static int deferred_len = 0, deferred_size = 0;

/*
 * Gross hack: pscp will try to start SFTP but fall back to scp1 if
 * that fails. This variable is the means by which scp.c can reach
 * into the SSH code and find out which one it got.
 */
int ssh_fallback_cmd = 0;

static int ssh_version;
static int ssh1_throttle_count;
static int ssh_overall_bufsize;
static int ssh_throttled_all;
static int ssh1_stdout_throttling;
static void (*ssh_protocol) (unsigned char *in, int inlen, int ispkt);
static void ssh1_protocol(unsigned char *in, int inlen, int ispkt);
static void ssh2_protocol(unsigned char *in, int inlen, int ispkt);
static void ssh_size(void);
static void ssh_special(Telnet_Special);
static int ssh2_try_send(struct ssh_channel *c);
static void ssh2_add_channel_data(struct ssh_channel *c, char *buf,
				  int len);
static void ssh_throttle_all(int enable, int bufsize);
static void ssh2_set_window(struct ssh_channel *c, unsigned newwin);
static int (*s_rdpkt) (unsigned char **data, int *datalen);
static int ssh_sendbuffer(void);

static struct rdpkt1_state_tag {
    long len, pad, biglen, to_read;
    unsigned long realcrc, gotcrc;
    unsigned char *p;
    int i;
    int chunk;
} rdpkt1_state;

static struct rdpkt2_state_tag {
    long len, pad, payload, packetlen, maclen;
    int i;
    int cipherblk;
    unsigned long incoming_sequence;
} rdpkt2_state;

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

static int alloc_channel_id(void)
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
    tsize = count234(ssh_channels);

    low = -1;
    high = tsize;
    while (high - low > 1) {
	mid = (high + low) / 2;
	c = index234(ssh_channels, mid);
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
	assert(NULL == find234(ssh_channels, &i, ssh_channelfind));
    }
    return low + 1 + CHANNEL_NUMBER_OFFSET;
}

static void c_write(char *buf, int len)
{
    if ((flags & FLAG_STDERR)) {
	int i;
	for (i = 0; i < len; i++)
	    if (buf[i] != '\r')
		fputc(buf[i], stderr);
	return;
    }
    from_backend(1, buf, len);
}

static void c_write_untrusted(char *buf, int len)
{
    int i;
    for (i = 0; i < len; i++) {
	if (buf[i] == '\n')
	    c_write("\r\n", 2);
	else if ((buf[i] & 0x60) || (buf[i] == '\r'))
	    c_write(buf + i, 1);
    }
}

static void c_write_str(char *buf)
{
    c_write(buf, strlen(buf));
}

/*
 * Collect incoming data in the incoming packet buffer.
 * Decipher and verify the packet when it is completely read.
 * Drop SSH1_MSG_DEBUG and SSH1_MSG_IGNORE packets.
 * Update the *data and *datalen variables.
 * Return the additional nr of bytes needed, or 0 when
 * a complete packet is available.
 */
static int ssh1_rdpkt(unsigned char **data, int *datalen)
{
    struct rdpkt1_state_tag *st = &rdpkt1_state;

    crBegin;

  next_packet:

    pktin.type = 0;
    pktin.length = 0;

    for (st->i = st->len = 0; st->i < 4; st->i++) {
	while ((*datalen) == 0)
	    crReturn(4 - st->i);
	st->len = (st->len << 8) + **data;
	(*data)++, (*datalen)--;
    }

    st->pad = 8 - (st->len % 8);
    st->biglen = st->len + st->pad;
    pktin.length = st->len - 5;

    if (pktin.maxlen < st->biglen) {
	pktin.maxlen = st->biglen;
	pktin.data = (pktin.data == NULL ? smalloc(st->biglen + APIEXTRA) :
		      srealloc(pktin.data, st->biglen + APIEXTRA));
	if (!pktin.data)
	    fatalbox("Out of memory");
    }

    st->to_read = st->biglen;
    st->p = pktin.data;
    while (st->to_read > 0) {
	st->chunk = st->to_read;
	while ((*datalen) == 0)
	    crReturn(st->to_read);
	if (st->chunk > (*datalen))
	    st->chunk = (*datalen);
	memcpy(st->p, *data, st->chunk);
	*data += st->chunk;
	*datalen -= st->chunk;
	st->p += st->chunk;
	st->to_read -= st->chunk;
    }

    if (cipher && detect_attack(pktin.data, st->biglen, NULL)) {
        bombout(("Network attack (CRC compensation) detected!"));
        crReturn(0);
    }

    if (cipher)
	cipher->decrypt(pktin.data, st->biglen);

    st->realcrc = crc32(pktin.data, st->biglen - 4);
    st->gotcrc = GET_32BIT(pktin.data + st->biglen - 4);
    if (st->gotcrc != st->realcrc) {
	bombout(("Incorrect CRC received on packet"));
	crReturn(0);
    }

    pktin.body = pktin.data + st->pad + 1;

    if (ssh1_compressing) {
	unsigned char *decompblk;
	int decomplen;
	zlib_decompress_block(pktin.body - 1, pktin.length + 1,
			      &decompblk, &decomplen);

	if (pktin.maxlen < st->pad + decomplen) {
	    pktin.maxlen = st->pad + decomplen;
	    pktin.data = srealloc(pktin.data, pktin.maxlen + APIEXTRA);
	    pktin.body = pktin.data + st->pad + 1;
	    if (!pktin.data)
		fatalbox("Out of memory");
	}

	memcpy(pktin.body - 1, decompblk, decomplen);
	sfree(decompblk);
	pktin.length = decomplen - 1;
    }

    pktin.type = pktin.body[-1];

    log_packet(PKT_INCOMING, pktin.type, ssh1_pkt_type(pktin.type),
	       pktin.body, pktin.length);

    if (pktin.type == SSH1_SMSG_STDOUT_DATA ||
	pktin.type == SSH1_SMSG_STDERR_DATA ||
	pktin.type == SSH1_MSG_DEBUG ||
	pktin.type == SSH1_SMSG_AUTH_TIS_CHALLENGE ||
	pktin.type == SSH1_SMSG_AUTH_CCARD_CHALLENGE) {
	long stringlen = GET_32BIT(pktin.body);
	if (stringlen + 4 != pktin.length) {
	    bombout(("Received data packet with bogus string length"));
	    crReturn(0);
	}
    }

    if (pktin.type == SSH1_MSG_DEBUG) {
	/* log debug message */
	char buf[512];
	int stringlen = GET_32BIT(pktin.body);
	strcpy(buf, "Remote debug message: ");
	if (stringlen > 480)
	    stringlen = 480;
	memcpy(buf + 8, pktin.body + 4, stringlen);
	buf[8 + stringlen] = '\0';
	logevent(buf);
	goto next_packet;
    } else if (pktin.type == SSH1_MSG_IGNORE) {
	/* do nothing */
	goto next_packet;
    }

    if (pktin.type == SSH1_MSG_DISCONNECT) {
	/* log reason code in disconnect message */
	char buf[256];
	unsigned msglen = GET_32BIT(pktin.body);
	unsigned nowlen;
	strcpy(buf, "Remote sent disconnect: ");
	nowlen = strlen(buf);
	if (msglen > sizeof(buf) - nowlen - 1)
	    msglen = sizeof(buf) - nowlen - 1;
	memcpy(buf + nowlen, pktin.body + 4, msglen);
	buf[nowlen + msglen] = '\0';
	/* logevent(buf); (this is now done within the bombout macro) */
	bombout(("Server sent disconnect message:\n\"%s\"", buf+nowlen));
	crReturn(0);
    }

    crFinish(0);
}

static int ssh2_rdpkt(unsigned char **data, int *datalen)
{
    struct rdpkt2_state_tag *st = &rdpkt2_state;

    crBegin;

  next_packet:
    pktin.type = 0;
    pktin.length = 0;
    if (sccipher)
	st->cipherblk = sccipher->blksize;
    else
	st->cipherblk = 8;
    if (st->cipherblk < 8)
	st->cipherblk = 8;

    if (pktin.maxlen < st->cipherblk) {
	pktin.maxlen = st->cipherblk;
	pktin.data =
	    (pktin.data ==
	     NULL ? smalloc(st->cipherblk +
			    APIEXTRA) : srealloc(pktin.data,
						 st->cipherblk +
						 APIEXTRA));
	if (!pktin.data)
	    fatalbox("Out of memory");
    }

    /*
     * Acquire and decrypt the first block of the packet. This will
     * contain the length and padding details.
     */
    for (st->i = st->len = 0; st->i < st->cipherblk; st->i++) {
	while ((*datalen) == 0)
	    crReturn(st->cipherblk - st->i);
	pktin.data[st->i] = *(*data)++;
	(*datalen)--;
    }

    if (sccipher)
	sccipher->decrypt(pktin.data, st->cipherblk);

    /*
     * Now get the length and padding figures.
     */
    st->len = GET_32BIT(pktin.data);
    st->pad = pktin.data[4];

    /*
     * _Completely_ silly lengths should be stomped on before they
     * do us any more damage.
     */
    if (st->len < 0 || st->pad < 0 || st->len + st->pad < 0) {
	bombout(("Incoming packet was garbled on decryption"));
	crReturn(0);
    }

    /*
     * This enables us to deduce the payload length.
     */
    st->payload = st->len - st->pad - 1;

    pktin.length = st->payload + 5;

    /*
     * So now we can work out the total packet length.
     */
    st->packetlen = st->len + 4;
    st->maclen = scmac ? scmac->len : 0;

    /*
     * Adjust memory allocation if packet is too big.
     */
    if (pktin.maxlen < st->packetlen + st->maclen) {
	pktin.maxlen = st->packetlen + st->maclen;
	pktin.data =
	    (pktin.data ==
	     NULL ? smalloc(pktin.maxlen + APIEXTRA) : srealloc(pktin.data,
								pktin.maxlen
								+
								APIEXTRA));
	if (!pktin.data)
	    fatalbox("Out of memory");
    }

    /*
     * Read and decrypt the remainder of the packet.
     */
    for (st->i = st->cipherblk; st->i < st->packetlen + st->maclen;
	 st->i++) {
	while ((*datalen) == 0)
	    crReturn(st->packetlen + st->maclen - st->i);
	pktin.data[st->i] = *(*data)++;
	(*datalen)--;
    }
    /* Decrypt everything _except_ the MAC. */
    if (sccipher)
	sccipher->decrypt(pktin.data + st->cipherblk,
			  st->packetlen - st->cipherblk);

    /*
     * Check the MAC.
     */
    if (scmac
	&& !scmac->verify(pktin.data, st->len + 4,
			  st->incoming_sequence)) {
	bombout(("Incorrect MAC received on packet"));
	crReturn(0);
    }
    st->incoming_sequence++;	       /* whether or not we MACed */

    /*
     * Decompress packet payload.
     */
    {
	unsigned char *newpayload;
	int newlen;
	if (sccomp && sccomp->decompress(pktin.data + 5, pktin.length - 5,
					 &newpayload, &newlen)) {
	    if (pktin.maxlen < newlen + 5) {
		pktin.maxlen = newlen + 5;
		pktin.data =
		    (pktin.data ==
		     NULL ? smalloc(pktin.maxlen +
				    APIEXTRA) : srealloc(pktin.data,
							 pktin.maxlen +
							 APIEXTRA));
		if (!pktin.data)
		    fatalbox("Out of memory");
	    }
	    pktin.length = 5 + newlen;
	    memcpy(pktin.data + 5, newpayload, newlen);
	    sfree(newpayload);
	}
    }

    pktin.savedpos = 6;
    pktin.type = pktin.data[5];

    log_packet(PKT_INCOMING, pktin.type, ssh2_pkt_type(pktin.type),
	       pktin.data+6, pktin.length-6);

    switch (pktin.type) {
        /*
         * These packets we must handle instantly.
         */
      case SSH2_MSG_DISCONNECT:
        {
            /* log reason code in disconnect message */
            char buf[256];
            int reason = GET_32BIT(pktin.data + 6);
            unsigned msglen = GET_32BIT(pktin.data + 10);
            unsigned nowlen;
            if (reason > 0 && reason < lenof(ssh2_disconnect_reasons)) {
                sprintf(buf, "Received disconnect message (%s)",
                        ssh2_disconnect_reasons[reason]);
            } else {
                sprintf(buf, "Received disconnect message (unknown type %d)",
                        reason);
            }
            logevent(buf);
            strcpy(buf, "Disconnection message text: ");
            nowlen = strlen(buf);
            if (msglen > sizeof(buf) - nowlen - 1)
                msglen = sizeof(buf) - nowlen - 1;
            memcpy(buf + nowlen, pktin.data + 14, msglen);
            buf[nowlen + msglen] = '\0';
            logevent(buf);
            bombout(("Server sent disconnect message\ntype %d (%s):\n\"%s\"",
                     reason,
                     (reason > 0 && reason < lenof(ssh2_disconnect_reasons)) ?
                     ssh2_disconnect_reasons[reason] : "unknown",
                     buf+nowlen));
            crReturn(0);
        }
        break;
      case SSH2_MSG_IGNORE:
	goto next_packet;
      case SSH2_MSG_DEBUG:
	{
	    /* log the debug message */
	    char buf[512];
	    /* int display = pktin.body[6]; */
	    int stringlen = GET_32BIT(pktin.data+7);
	    int prefix;
	    strcpy(buf, "Remote debug message: ");
	    prefix = strlen(buf);
	    if (stringlen > sizeof(buf)-prefix-1)
		stringlen = sizeof(buf)-prefix-1;
	    memcpy(buf + prefix, pktin.data + 11, stringlen);
	    buf[prefix + stringlen] = '\0';
	    logevent(buf);
	}
        goto next_packet;              /* FIXME: print the debug message */

        /*
         * These packets we need do nothing about here.
         */
      case SSH2_MSG_UNIMPLEMENTED:
      case SSH2_MSG_SERVICE_REQUEST:
      case SSH2_MSG_SERVICE_ACCEPT:
      case SSH2_MSG_KEXINIT:
      case SSH2_MSG_NEWKEYS:
      case SSH2_MSG_KEXDH_INIT:
      case SSH2_MSG_KEXDH_REPLY:
      /* case SSH2_MSG_KEX_DH_GEX_REQUEST: duplicate case value */
      /* case SSH2_MSG_KEX_DH_GEX_GROUP: duplicate case value */
      case SSH2_MSG_KEX_DH_GEX_INIT:
      case SSH2_MSG_KEX_DH_GEX_REPLY:
      case SSH2_MSG_USERAUTH_REQUEST:
      case SSH2_MSG_USERAUTH_FAILURE:
      case SSH2_MSG_USERAUTH_SUCCESS:
      case SSH2_MSG_USERAUTH_BANNER:
      case SSH2_MSG_USERAUTH_PK_OK:
      /* case SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ: duplicate case value */
      /* case SSH2_MSG_USERAUTH_INFO_REQUEST: duplicate case value */
      case SSH2_MSG_USERAUTH_INFO_RESPONSE:
      case SSH2_MSG_GLOBAL_REQUEST:
      case SSH2_MSG_REQUEST_SUCCESS:
      case SSH2_MSG_REQUEST_FAILURE:
      case SSH2_MSG_CHANNEL_OPEN:
      case SSH2_MSG_CHANNEL_OPEN_CONFIRMATION:
      case SSH2_MSG_CHANNEL_OPEN_FAILURE:
      case SSH2_MSG_CHANNEL_WINDOW_ADJUST:
      case SSH2_MSG_CHANNEL_DATA:
      case SSH2_MSG_CHANNEL_EXTENDED_DATA:
      case SSH2_MSG_CHANNEL_EOF:
      case SSH2_MSG_CHANNEL_CLOSE:
      case SSH2_MSG_CHANNEL_REQUEST:
      case SSH2_MSG_CHANNEL_SUCCESS:
      case SSH2_MSG_CHANNEL_FAILURE:
        break;

        /*
         * For anything else we send SSH2_MSG_UNIMPLEMENTED.
         */
      default:
	ssh2_pkt_init(SSH2_MSG_UNIMPLEMENTED);
	ssh2_pkt_adduint32(st->incoming_sequence - 1);
	ssh2_pkt_send();
        break;
    }

    crFinish(0);
}

static void ssh1_pktout_size(int len)
{
    int pad, biglen;

    len += 5;			       /* type and CRC */
    pad = 8 - (len % 8);
    biglen = len + pad;

    pktout.length = len - 5;
    if (pktout.maxlen < biglen) {
	pktout.maxlen = biglen;
#ifdef MSCRYPTOAPI
	/* Allocate enough buffer space for extra block
	 * for MS CryptEncrypt() */
	pktout.data = (pktout.data == NULL ? smalloc(biglen + 12) :
		       srealloc(pktout.data, biglen + 12));
#else
	pktout.data = (pktout.data == NULL ? smalloc(biglen + 4) :
		       srealloc(pktout.data, biglen + 4));
#endif
	if (!pktout.data)
	    fatalbox("Out of memory");
    }
    pktout.body = pktout.data + 4 + pad + 1;
}

static void s_wrpkt_start(int type, int len)
{
    ssh1_pktout_size(len);
    pktout.type = type;
}

static int s_wrpkt_prepare(void)
{
    int pad, len, biglen, i;
    unsigned long crc;

    pktout.body[-1] = pktout.type;

    log_packet(PKT_OUTGOING, pktout.type, ssh1_pkt_type(pktout.type),
	       pktout.body, pktout.length);

    if (ssh1_compressing) {
	unsigned char *compblk;
	int complen;
	zlib_compress_block(pktout.body - 1, pktout.length + 1,
			    &compblk, &complen);
	ssh1_pktout_size(complen - 1);
	memcpy(pktout.body - 1, compblk, complen);
	sfree(compblk);
    }

    len = pktout.length + 5;	       /* type and CRC */
    pad = 8 - (len % 8);
    biglen = len + pad;

    for (i = 0; i < pad; i++)
	pktout.data[i + 4] = random_byte();
    crc = crc32(pktout.data + 4, biglen - 4);
    PUT_32BIT(pktout.data + biglen, crc);
    PUT_32BIT(pktout.data, len);

    if (cipher)
	cipher->encrypt(pktout.data + 4, biglen);

    return biglen + 4;
}

static void s_wrpkt(void)
{
    int len, backlog;
    len = s_wrpkt_prepare();
    backlog = sk_write(s, pktout.data, len);
    if (backlog > SSH_MAX_BACKLOG)
	ssh_throttle_all(1, backlog);
}

static void s_wrpkt_defer(void)
{
    int len;
    len = s_wrpkt_prepare();
    if (deferred_len + len > deferred_size) {
	deferred_size = deferred_len + len + 128;
	deferred_send_data = srealloc(deferred_send_data, deferred_size);
    }
    memcpy(deferred_send_data + deferred_len, pktout.data, len);
    deferred_len += len;
}

/*
 * Construct a packet with the specified contents.
 */
static void construct_packet(int pkttype, va_list ap1, va_list ap2)
{
    unsigned char *p, *argp, argchar;
    unsigned long argint;
    int pktlen, argtype, arglen;
    Bignum bn;

    pktlen = 0;
    while ((argtype = va_arg(ap1, int)) != PKT_END) {
	switch (argtype) {
	  case PKT_INT:
	    (void) va_arg(ap1, int);
	    pktlen += 4;
	    break;
	  case PKT_CHAR:
	    (void) va_arg(ap1, char);
	    pktlen++;
	    break;
	  case PKT_DATA:
	    (void) va_arg(ap1, unsigned char *);
	    arglen = va_arg(ap1, int);
	    pktlen += arglen;
	    break;
	  case PKT_STR:
	    argp = va_arg(ap1, unsigned char *);
	    arglen = strlen(argp);
	    pktlen += 4 + arglen;
	    break;
	  case PKT_BIGNUM:
	    bn = va_arg(ap1, Bignum);
	    pktlen += ssh1_bignum_length(bn);
	    break;
	  default:
	    assert(0);
	}
    }

    s_wrpkt_start(pkttype, pktlen);
    p = pktout.body;

    while ((argtype = va_arg(ap2, int)) != PKT_END) {
	switch (argtype) {
	  case PKT_INT:
	    argint = va_arg(ap2, int);
	    PUT_32BIT(p, argint);
	    p += 4;
	    break;
	  case PKT_CHAR:
	    argchar = va_arg(ap2, unsigned char);
	    *p = argchar;
	    p++;
	    break;
	  case PKT_DATA:
	    argp = va_arg(ap2, unsigned char *);
	    arglen = va_arg(ap2, int);
	    memcpy(p, argp, arglen);
	    p += arglen;
	    break;
	  case PKT_STR:
	    argp = va_arg(ap2, unsigned char *);
	    arglen = strlen(argp);
	    PUT_32BIT(p, arglen);
	    memcpy(p + 4, argp, arglen);
	    p += 4 + arglen;
	    break;
	  case PKT_BIGNUM:
	    bn = va_arg(ap2, Bignum);
	    p += ssh1_write_bignum(p, bn);
	    break;
	}
    }
}

static void send_packet(int pkttype, ...)
{
    va_list ap1, ap2;
    va_start(ap1, pkttype);
    va_start(ap2, pkttype);
    construct_packet(pkttype, ap1, ap2);
    s_wrpkt();
}

static void defer_packet(int pkttype, ...)
{
    va_list ap1, ap2;
    va_start(ap1, pkttype);
    va_start(ap2, pkttype);
    construct_packet(pkttype, ap1, ap2);
    s_wrpkt_defer();
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
 * SSH2 packet construction functions.
 */
static void ssh2_pkt_ensure(int length)
{
    if (pktout.maxlen < length) {
	pktout.maxlen = length + 256;
	pktout.data =
	    (pktout.data ==
	     NULL ? smalloc(pktout.maxlen +
			    APIEXTRA) : srealloc(pktout.data,
						 pktout.maxlen +
						 APIEXTRA));
	if (!pktout.data)
	    fatalbox("Out of memory");
    }
}
static void ssh2_pkt_adddata(void *data, int len)
{
    pktout.length += len;
    ssh2_pkt_ensure(pktout.length);
    memcpy(pktout.data + pktout.length - len, data, len);
}
static void ssh2_pkt_addbyte(unsigned char byte)
{
    ssh2_pkt_adddata(&byte, 1);
}
static void ssh2_pkt_init(int pkt_type)
{
    pktout.length = 5;
    ssh2_pkt_addbyte((unsigned char) pkt_type);
}
static void ssh2_pkt_addbool(unsigned char value)
{
    ssh2_pkt_adddata(&value, 1);
}
static void ssh2_pkt_adduint32(unsigned long value)
{
    unsigned char x[4];
    PUT_32BIT(x, value);
    ssh2_pkt_adddata(x, 4);
}
static void ssh2_pkt_addstring_start(void)
{
    ssh2_pkt_adduint32(0);
    pktout.savedpos = pktout.length;
}
static void ssh2_pkt_addstring_str(char *data)
{
    ssh2_pkt_adddata(data, strlen(data));
    PUT_32BIT(pktout.data + pktout.savedpos - 4,
	      pktout.length - pktout.savedpos);
}
static void ssh2_pkt_addstring_data(char *data, int len)
{
    ssh2_pkt_adddata(data, len);
    PUT_32BIT(pktout.data + pktout.savedpos - 4,
	      pktout.length - pktout.savedpos);
}
static void ssh2_pkt_addstring(char *data)
{
    ssh2_pkt_addstring_start();
    ssh2_pkt_addstring_str(data);
}
static char *ssh2_mpint_fmt(Bignum b, int *len)
{
    unsigned char *p;
    int i, n = (bignum_bitcount(b) + 7) / 8;
    p = smalloc(n + 1);
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
static void ssh2_pkt_addmp(Bignum b)
{
    unsigned char *p;
    int len;
    p = ssh2_mpint_fmt(b, &len);
    ssh2_pkt_addstring_start();
    ssh2_pkt_addstring_data(p, len);
    sfree(p);
}

/*
 * Construct an SSH2 final-form packet: compress it, encrypt it,
 * put the MAC on it. Final packet, ready to be sent, is stored in
 * pktout.data. Total length is returned.
 */
static int ssh2_pkt_construct(void)
{
    int cipherblk, maclen, padding, i;
    static unsigned long outgoing_sequence = 0;

    log_packet(PKT_OUTGOING, pktout.data[5], ssh2_pkt_type(pktout.data[5]),
	       pktout.data + 6, pktout.length - 6);

    /*
     * Compress packet payload.
     */
    {
	unsigned char *newpayload;
	int newlen;
	if (cscomp && cscomp->compress(pktout.data + 5, pktout.length - 5,
				       &newpayload, &newlen)) {
	    pktout.length = 5;
	    ssh2_pkt_adddata(newpayload, newlen);
	    sfree(newpayload);
	}
    }

    /*
     * Add padding. At least four bytes, and must also bring total
     * length (minus MAC) up to a multiple of the block size.
     */
    cipherblk = cscipher ? cscipher->blksize : 8;	/* block size */
    cipherblk = cipherblk < 8 ? 8 : cipherblk;	/* or 8 if blksize < 8 */
    padding = 4;
    padding +=
	(cipherblk - (pktout.length + padding) % cipherblk) % cipherblk;
    maclen = csmac ? csmac->len : 0;
    ssh2_pkt_ensure(pktout.length + padding + maclen);
    pktout.data[4] = padding;
    for (i = 0; i < padding; i++)
	pktout.data[pktout.length + i] = random_byte();
    PUT_32BIT(pktout.data, pktout.length + padding - 4);
    if (csmac)
	csmac->generate(pktout.data, pktout.length + padding,
			outgoing_sequence);
    outgoing_sequence++;	       /* whether or not we MACed */

    if (cscipher)
	cscipher->encrypt(pktout.data, pktout.length + padding);

    /* Ready-to-send packet starts at pktout.data. We return length. */
    return pktout.length + padding + maclen;
}

/*
 * Construct and send an SSH2 packet immediately.
 */
static void ssh2_pkt_send(void)
{
    int len;
    int backlog;
    len = ssh2_pkt_construct();
    backlog = sk_write(s, pktout.data, len);
    if (backlog > SSH_MAX_BACKLOG)
	ssh_throttle_all(1, backlog);
}

/*
 * Construct an SSH2 packet and add it to a deferred data block.
 * Useful for sending multiple packets in a single sk_write() call,
 * to prevent a traffic-analysing listener from being able to work
 * out the length of any particular packet (such as the password
 * packet).
 * 
 * Note that because SSH2 sequence-numbers its packets, this can
 * NOT be used as an m4-style `defer' allowing packets to be
 * constructed in one order and sent in another.
 */
static void ssh2_pkt_defer(void)
{
    int len = ssh2_pkt_construct();
    if (deferred_len + len > deferred_size) {
	deferred_size = deferred_len + len + 128;
	deferred_send_data = srealloc(deferred_send_data, deferred_size);
    }
    memcpy(deferred_send_data + deferred_len, pktout.data, len);
    deferred_len += len;
}

/*
 * Send the whole deferred data block constructed by
 * ssh2_pkt_defer() or SSH1's defer_packet().
 */
static void ssh_pkt_defersend(void)
{
    int backlog;
    backlog = sk_write(s, deferred_send_data, deferred_len);
    deferred_len = deferred_size = 0;
    sfree(deferred_send_data);
    deferred_send_data = NULL;
    if (backlog > SSH_MAX_BACKLOG)
	ssh_throttle_all(1, backlog);
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
 * SSH2 packet decode functions.
 */
static unsigned long ssh2_pkt_getuint32(void)
{
    unsigned long value;
    if (pktin.length - pktin.savedpos < 4)
	return 0;		       /* arrgh, no way to decline (FIXME?) */
    value = GET_32BIT(pktin.data + pktin.savedpos);
    pktin.savedpos += 4;
    return value;
}
static int ssh2_pkt_getbool(void)
{
    unsigned long value;
    if (pktin.length - pktin.savedpos < 1)
	return 0;		       /* arrgh, no way to decline (FIXME?) */
    value = pktin.data[pktin.savedpos] != 0;
    pktin.savedpos++;
    return value;
}
static void ssh2_pkt_getstring(char **p, int *length)
{
    int len;
    *p = NULL;
    *length = 0;
    if (pktin.length - pktin.savedpos < 4)
	return;
    len = GET_32BIT(pktin.data + pktin.savedpos);
    if (len < 0)
	return;
    *length = len;
    pktin.savedpos += 4;
    if (pktin.length - pktin.savedpos < *length)
	return;
    *p = pktin.data + pktin.savedpos;
    pktin.savedpos += *length;
}
static Bignum ssh2_pkt_getmp(void)
{
    char *p;
    int length;
    Bignum b;

    ssh2_pkt_getstring(&p, &length);
    if (!p)
	return NULL;
    if (p[0] & 0x80) {
	bombout(("internal error: Can't handle negative mpints"));
	return NULL;
    }
    b = bignum_from_bytes(p, length);
    return b;
}

/*
 * Helper function to add an SSH2 signature blob to a packet.
 * Expects to be shown the public key blob as well as the signature
 * blob. Normally works just like ssh2_pkt_addstring, but will
 * fiddle with the signature packet if necessary for
 * BUG_SSH2_RSA_PADDING.
 */
static void ssh2_add_sigblob(void *pkblob_v, int pkblob_len,
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
    if ((ssh_remote_bugs & BUG_SSH2_RSA_PADDING) &&
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
	    ssh2_pkt_addstring_start();
	    ssh2_pkt_addstring_data(sigblob, pos);
	    /* dmemdump(sigblob, pos); */
	    pos += 4;		       /* point to start of actual sig */
	    PUT_32BIT(newlen, len);
	    ssh2_pkt_addstring_data(newlen, 4);
	    /* dmemdump(newlen, 4); */
	    newlen[0] = 0;
	    while (len-- > siglen) {
		ssh2_pkt_addstring_data(newlen, 1);
		/* dmemdump(newlen, 1); */
	    }
	    ssh2_pkt_addstring_data(sigblob+pos, siglen);
	    /* dmemdump(sigblob+pos, siglen); */
	    return;
	}

	/* Otherwise fall through and do it the easy way. */
    }

    ssh2_pkt_addstring_start();
    ssh2_pkt_addstring_data(sigblob, sigblob_len);
}

/*
 * Examine the remote side's version string and compare it against
 * a list of known buggy implementations.
 */
static void ssh_detect_bugs(char *vstring)
{
    char *imp;			       /* pointer to implementation part */
    imp = vstring;
    imp += strcspn(imp, "-");
    if (*imp) imp++;
    imp += strcspn(imp, "-");
    if (*imp) imp++;

    ssh_remote_bugs = 0;

    if (cfg.sshbug_ignore1 == BUG_ON ||
	(cfg.sshbug_ignore1 == BUG_AUTO &&
	 (!strcmp(imp, "1.2.18") || !strcmp(imp, "1.2.19") ||
	  !strcmp(imp, "1.2.20") || !strcmp(imp, "1.2.21") ||
	  !strcmp(imp, "1.2.22") || !strcmp(imp, "Cisco-1.25")))) {
	/*
	 * These versions don't support SSH1_MSG_IGNORE, so we have
	 * to use a different defence against password length
	 * sniffing.
	 */
	ssh_remote_bugs |= BUG_CHOKES_ON_SSH1_IGNORE;
	logevent("We believe remote version has SSH1 ignore bug");
    }

    if (cfg.sshbug_plainpw1 == BUG_ON ||
	(cfg.sshbug_plainpw1 == BUG_AUTO &&
	 (!strcmp(imp, "Cisco-1.25")))) {
	/*
	 * These versions need a plain password sent; they can't
	 * handle having a null and a random length of data after
	 * the password.
	 */
	ssh_remote_bugs |= BUG_NEEDS_SSH1_PLAIN_PASSWORD;
	logevent("We believe remote version needs a plain SSH1 password");
    }

    if (cfg.sshbug_rsa1 == BUG_ON ||
	(cfg.sshbug_rsa1 == BUG_AUTO &&
	 (!strcmp(imp, "Cisco-1.25")))) {
	/*
	 * These versions apparently have no clue whatever about
	 * RSA authentication and will panic and die if they see
	 * an AUTH_RSA message.
	 */
	ssh_remote_bugs |= BUG_CHOKES_ON_RSA;
	logevent("We believe remote version can't handle RSA authentication");
    }

    if (cfg.sshbug_hmac2 == BUG_ON ||
	(cfg.sshbug_hmac2 == BUG_AUTO &&
	 (!strncmp(imp, "2.1.0", 5) || !strncmp(imp, "2.0.", 4) ||
	  !strncmp(imp, "2.2.0", 5) || !strncmp(imp, "2.3.0", 5) ||
	  !strncmp(imp, "2.1 ", 4)))) {
	/*
	 * These versions have the HMAC bug.
	 */
	ssh_remote_bugs |= BUG_SSH2_HMAC;
	logevent("We believe remote version has SSH2 HMAC bug");
    }

    if (cfg.sshbug_derivekey2 == BUG_ON ||
	(cfg.sshbug_derivekey2 == BUG_AUTO &&
	 (!strncmp(imp, "2.0.", 4)))) {
	/*
	 * These versions have the key-derivation bug (failing to
	 * include the literal shared secret in the hashes that
	 * generate the keys).
	 */
	ssh_remote_bugs |= BUG_SSH2_DERIVEKEY;
	logevent("We believe remote version has SSH2 key-derivation bug");
    }

    if (cfg.sshbug_rsapad2 == BUG_ON ||
	(cfg.sshbug_rsapad2 == BUG_AUTO &&
	 ((!strncmp(imp, "OpenSSH_2.", 10) && imp[10]>='5' && imp[10]<='9') ||
	  (!strncmp(imp, "OpenSSH_3.", 10) && imp[10]>='0' && imp[10]<='2')))){
	/*
	 * These versions have the SSH2 RSA padding bug.
	 */
	ssh_remote_bugs |= BUG_SSH2_RSA_PADDING;
	logevent("We believe remote version has SSH2 RSA padding bug");
    }

    if (cfg.sshbug_dhgex2 == BUG_ON) {
	/*
	 * These versions have the SSH2 DH GEX bug.
	 */
	ssh_remote_bugs |= BUG_SSH2_DH_GEX;
	logevent("We believe remote version has SSH2 DH group exchange bug");
    }
}

static int do_ssh_init(unsigned char c)
{
    static int vslen;
    static char version[10];
    static char *vstring;
    static int vstrsize;
    static char *vlog;
    static int i;
    static int proto1, proto2;

    crBegin;

    /* Search for the string "SSH-" in the input. */
    i = 0;
    while (1) {
	static const int transS[] = { 1, 2, 2, 1 };
	static const int transH[] = { 0, 0, 3, 0 };
	static const int transminus[] = { 0, 0, 0, -1 };
	if (c == 'S')
	    i = transS[i];
	else if (c == 'H')
	    i = transH[i];
	else if (c == '-')
	    i = transminus[i];
	else
	    i = 0;
	if (i < 0)
	    break;
	crReturn(1);		       /* get another character */
    }

    vstrsize = 16;
    vstring = smalloc(vstrsize);
    strcpy(vstring, "SSH-");
    vslen = 4;
    i = 0;
    while (1) {
	crReturn(1);		       /* get another char */
	if (vslen >= vstrsize - 1) {
	    vstrsize += 16;
	    vstring = srealloc(vstring, vstrsize);
	}
	vstring[vslen++] = c;
	if (i >= 0) {
	    if (c == '-') {
		version[i] = '\0';
		i = -1;
	    } else if (i < sizeof(version) - 1)
		version[i++] = c;
	} else if (c == '\n')
	    break;
    }

    ssh_agentfwd_enabled = FALSE;
    rdpkt2_state.incoming_sequence = 0;

    vstring[vslen] = 0;
    vlog = smalloc(20 + vslen);
    vstring[strcspn (vstring, "\r\n")] = '\0'; /* remove end-of-line chars */
    sprintf(vlog, "Server version: %s", vstring);
    logevent(vlog);
    ssh_detect_bugs(vstring);
    sfree(vlog);

    /*
     * Decide which SSH protocol version to support.
     */

    /* Anything strictly below "2.0" means protocol 1 is supported. */
    proto1 = ssh_versioncmp(version, "2.0") < 0;
    /* Anything greater or equal to "1.99" means protocol 2 is supported. */
    proto2 = ssh_versioncmp(version, "1.99") >= 0;

    if (cfg.sshprot == 0 && !proto1) {
	bombout(("SSH protocol version 1 required by user but not provided by server"));
	crReturn(0);
    }
    if (cfg.sshprot == 3 && !proto2) {
	bombout(("SSH protocol version 2 required by user but not provided by server"));
	crReturn(0);
    }

    if (proto2 && (cfg.sshprot >= 2 || !proto1)) {
	/*
	 * Use v2 protocol.
	 */
	char verstring[80], vlog[100];
	sprintf(verstring, "SSH-2.0-%s", sshver);
	SHA_Init(&exhashbase);
	/*
	 * Hash our version string and their version string.
	 */
	sha_string(&exhashbase, verstring, strlen(verstring));
	sha_string(&exhashbase, vstring, strcspn(vstring, "\r\n"));
	sprintf(vlog, "We claim version: %s", verstring);
	logevent(vlog);
	strcat(verstring, "\n");
	logevent("Using SSH protocol version 2");
	sk_write(s, verstring, strlen(verstring));
	ssh_protocol = ssh2_protocol;
	ssh_version = 2;
	s_rdpkt = ssh2_rdpkt;
    } else {
	/*
	 * Use v1 protocol.
	 */
	char verstring[80], vlog[100];
	sprintf(verstring, "SSH-%s-%s",
		(ssh_versioncmp(version, "1.5") <= 0 ? version : "1.5"),
		sshver);
	sprintf(vlog, "We claim version: %s", verstring);
	logevent(vlog);
	strcat(verstring, "\n");

	logevent("Using SSH protocol version 1");
	sk_write(s, verstring, strlen(verstring));
	ssh_protocol = ssh1_protocol;
	ssh_version = 1;
	s_rdpkt = ssh1_rdpkt;
    }
    ssh_state = SSH_STATE_BEFORE_SIZE;

    sfree(vstring);

    crFinish(0);
}

static void ssh_gotdata(unsigned char *data, int datalen)
{
    crBegin;

    /*
     * To begin with, feed the characters one by one to the
     * protocol initialisation / selection function do_ssh_init().
     * When that returns 0, we're done with the initial greeting
     * exchange and can move on to packet discipline.
     */
    while (1) {
	int ret;
	if (datalen == 0)
	    crReturnV;		       /* more data please */
	ret = do_ssh_init(*data);
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
    while (1) {
	while (datalen > 0) {
	    if (s_rdpkt(&data, &datalen) == 0) {
		if (ssh_state == SSH_STATE_CLOSED) {
		    return;
		}
		ssh_protocol(NULL, 0, 1);
		if (ssh_state == SSH_STATE_CLOSED) {
		    return;
		}
	    }
	}
	crReturnV;
    }
    crFinishV;
}

static int ssh_closing(Plug plug, char *error_msg, int error_code,
		       int calling_back)
{
    ssh_state = SSH_STATE_CLOSED;
    if (s) {
        sk_close(s);
        s = NULL;
    }
    if (error_msg) {
	/* A socket error has occurred. */
	logevent(error_msg);
	connection_fatal(error_msg);
    } else {
	/* Otherwise, the remote side closed the connection normally. */
    }
    return 0;
}

static int ssh_receive(Plug plug, int urgent, char *data, int len)
{
    ssh_gotdata(data, len);
    if (ssh_state == SSH_STATE_CLOSED) {
	if (s) {
	    sk_close(s);
	    s = NULL;
	}
	return 0;
    }
    return 1;
}

static void ssh_sent(Plug plug, int bufsize)
{
    /*
     * If the send backlog on the SSH socket itself clears, we
     * should unthrottle the whole world if it was throttled.
     */
    if (bufsize < SSH_MAX_BACKLOG)
	ssh_throttle_all(0, bufsize);
}

/*
 * Connect to specified host and port.
 * Returns an error message, or NULL on success.
 * Also places the canonical host name into `realhost'. It must be
 * freed by the caller.
 */
static char *connect_to_host(char *host, int port, char **realhost, int nodelay)
{
    static struct plug_function_table fn_table = {
	ssh_closing,
	ssh_receive,
	ssh_sent,
	NULL
    }, *fn_table_ptr = &fn_table;

    SockAddr addr;
    char *err;
#ifdef FWHACK
    char *FWhost;
    int FWport;
#endif

    savedhost = smalloc(1 + strlen(host));
    if (!savedhost)
	fatalbox("Out of memory");
    strcpy(savedhost, host);

    if (port < 0)
	port = 22;		       /* default ssh port */
    savedport = port;

#ifdef FWHACK
    FWhost = host;
    FWport = port;
    host = FWSTR;
    port = 23;
#endif

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

#ifdef FWHACK
    *realhost = strdup(FWhost);
#endif

    /*
     * Open socket.
     */
    {
	char buf[200], addrbuf[100];
	sk_getaddr(addr, addrbuf, 100);
	sprintf(buf, "Connecting to %.100s port %d", addrbuf, port);
	logevent(buf);
    }
    s = new_connection(addr, *realhost, port, 0, 1, nodelay, &fn_table_ptr);
    if ((err = sk_socket_error(s))) {
	s = NULL;
	return err;
    }

#ifdef FWHACK
    sk_write(s, "connect ", 8);
    sk_write(s, FWhost, strlen(FWhost));
    {
	char buf[20];
	sprintf(buf, " %d\n", FWport);
	sk_write(s, buf, strlen(buf));
    }
#endif

    return NULL;
}

/*
 * Throttle or unthrottle the SSH connection.
 */
static void ssh1_throttle(int adjust)
{
    int old_count = ssh1_throttle_count;
    ssh1_throttle_count += adjust;
    assert(ssh1_throttle_count >= 0);
    if (ssh1_throttle_count && !old_count) {
	sk_set_frozen(s, 1);
    } else if (!ssh1_throttle_count && old_count) {
	sk_set_frozen(s, 0);
    }
}

/*
 * Throttle or unthrottle _all_ local data streams (for when sends
 * on the SSH connection itself back up).
 */
static void ssh_throttle_all(int enable, int bufsize)
{
    int i;
    struct ssh_channel *c;

    if (enable == ssh_throttled_all)
	return;
    ssh_throttled_all = enable;
    ssh_overall_bufsize = bufsize;
    if (!ssh_channels)
	return;
    for (i = 0; NULL != (c = index234(ssh_channels, i)); i++) {
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
	    pfd_override_throttle(c->u.x11.s, enable);
	    break;
	}
    }
}

/*
 * Username and password input, abstracted off into reusable
 * routines (hopefully even reusable between SSH1 and SSH2!).
 */
static char *ssh_userpass_input_buffer;
static int ssh_userpass_input_buflen;
static int ssh_userpass_input_bufpos;
static int ssh_userpass_input_echo;

/* Set up a username or password input loop on a given buffer. */
void setup_userpass_input(char *buffer, int buflen, int echo)
{
    ssh_userpass_input_buffer = buffer;
    ssh_userpass_input_buflen = buflen;
    ssh_userpass_input_bufpos = 0;
    ssh_userpass_input_echo = echo;
}

/*
 * Process some terminal data in the course of username/password
 * input. Returns >0 for success (line of input returned in
 * buffer), <0 for failure (user hit ^C/^D, bomb out and exit), 0
 * for inconclusive (keep waiting for more input please).
 */
int process_userpass_input(unsigned char *in, int inlen)
{
    char c;

    while (inlen--) {
	switch (c = *in++) {
	  case 10:
	  case 13:
	    ssh_userpass_input_buffer[ssh_userpass_input_bufpos] = 0;
	    ssh_userpass_input_buffer[ssh_userpass_input_buflen-1] = 0;
	    return +1;
	    break;
	  case 8:
	  case 127:
	    if (ssh_userpass_input_bufpos > 0) {
		if (ssh_userpass_input_echo)
		    c_write_str("\b \b");
		ssh_userpass_input_bufpos--;
	    }
	    break;
	  case 21:
	  case 27:
	    while (ssh_userpass_input_bufpos > 0) {
		if (ssh_userpass_input_echo)
		    c_write_str("\b \b");
		ssh_userpass_input_bufpos--;
	    }
	    break;
	  case 3:
	  case 4:
	    return -1;
	    break;
	  default:
	    if (((c >= ' ' && c <= '~') ||
		 ((unsigned char) c >= 160))
		&& ssh_userpass_input_bufpos < ssh_userpass_input_buflen-1) {
		ssh_userpass_input_buffer[ssh_userpass_input_bufpos++] = c;
		if (ssh_userpass_input_echo)
		    c_write(&c, 1);
	    }
	    break;
	}
    }
    return 0;
}

/*
 * Handle the key exchange and user authentication phases.
 */
static int do_ssh1_login(unsigned char *in, int inlen, int ispkt)
{
    int i, j;
    static int len;
    static unsigned char *rsabuf, *keystr1, *keystr2;
    unsigned char cookie[8];
    struct RSAKey servkey, hostkey;
    struct MD5Context md5c;
    static unsigned long supported_ciphers_mask, supported_auths_mask;
    static int tried_publickey, tried_agent;
    static int tis_auth_refused, ccard_auth_refused;
    static unsigned char session_id[16];
    static int cipher_type;
    static char username[100];
    static void *publickey_blob;
    int publickey_bloblen;

    crBegin;

    if (!ispkt)
	crWaitUntil(ispkt);

    if (pktin.type != SSH1_SMSG_PUBLIC_KEY) {
	bombout(("Public key packet not received"));
	crReturn(0);
    }

    logevent("Received public keys");

    memcpy(cookie, pktin.body, 8);

    i = makekey(pktin.body + 8, &servkey, &keystr1, 0);
    j = makekey(pktin.body + 8 + i, &hostkey, &keystr2, 0);

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

    ssh1_remote_protoflags = GET_32BIT(pktin.body + 8 + i + j);
    supported_ciphers_mask = GET_32BIT(pktin.body + 12 + i + j);
    supported_auths_mask = GET_32BIT(pktin.body + 16 + i + j);

    ssh1_local_protoflags =
	ssh1_remote_protoflags & SSH1_PROTOFLAGS_SUPPORTED;
    ssh1_local_protoflags |= SSH1_PROTOFLAG_SCREEN_NUMBER;

    MD5Init(&md5c);
    MD5Update(&md5c, keystr2, hostkey.bytes);
    MD5Update(&md5c, keystr1, servkey.bytes);
    MD5Update(&md5c, pktin.body, 8);
    MD5Final(session_id, &md5c);

    for (i = 0; i < 32; i++)
	session_key[i] = random_byte();

    len = (hostkey.bytes > servkey.bytes ? hostkey.bytes : servkey.bytes);

    rsabuf = smalloc(len);
    if (!rsabuf)
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
	char *keystr = smalloc(len);
	if (!keystr)
	    fatalbox("Out of memory");
	rsastr_fmt(keystr, &hostkey);
	rsa_fingerprint(fingerprint, sizeof(fingerprint), &hostkey);
	verify_ssh_host_key(savedhost, savedport, "rsa", keystr,
			    fingerprint);
	sfree(keystr);
    }

    for (i = 0; i < 32; i++) {
	rsabuf[i] = session_key[i];
	if (i < 16)
	    rsabuf[i] ^= session_id[i];
    }

    if (hostkey.bytes > servkey.bytes) {
	rsaencrypt(rsabuf, 32, &servkey);
	rsaencrypt(rsabuf, servkey.bytes, &hostkey);
    } else {
	rsaencrypt(rsabuf, 32, &hostkey);
	rsaencrypt(rsabuf, hostkey.bytes, &servkey);
    }

    logevent("Encrypted session key");

    {
	int cipher_chosen = 0, warn = 0;
	char *cipher_string = NULL;
	for (i = 0; !cipher_chosen && i < CIPHER_MAX; i++) {
	    int next_cipher = cfg.ssh_cipherlist[i];
	    if (next_cipher == CIPHER_WARN) {
		/* If/when we choose a cipher, warn about it */
		warn = 1;
	    } else if (next_cipher == CIPHER_AES) {
		/* XXX Probably don't need to mention this. */
		logevent("AES not supported in SSH1, skipping");
	    } else {
		switch (next_cipher) {
		  case CIPHER_3DES:     cipher_type = SSH_CIPHER_3DES;
					cipher_string = "3DES"; break;
		  case CIPHER_BLOWFISH: cipher_type = SSH_CIPHER_BLOWFISH;
					cipher_string = "Blowfish"; break;
		  case CIPHER_DES:	cipher_type = SSH_CIPHER_DES;
					cipher_string = "single-DES"; break;
		}
		if (supported_ciphers_mask & (1 << cipher_type))
		    cipher_chosen = 1;
	    }
	}
	if (!cipher_chosen) {
	    if ((supported_ciphers_mask & (1 << SSH_CIPHER_3DES)) == 0)
		bombout(("Server violates SSH 1 protocol by not "
			 "supporting 3DES encryption"));
	    else
		/* shouldn't happen */
		bombout(("No supported ciphers found"));
	    crReturn(0);
	}

	/* Warn about chosen cipher if necessary. */
	if (warn)
	    askcipher(cipher_string, 0);
    }

    switch (cipher_type) {
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

    send_packet(SSH1_CMSG_SESSION_KEY,
		PKT_CHAR, cipher_type,
		PKT_DATA, cookie, 8,
		PKT_CHAR, (len * 8) >> 8, PKT_CHAR, (len * 8) & 0xFF,
		PKT_DATA, rsabuf, len,
		PKT_INT, ssh1_local_protoflags, PKT_END);

    logevent("Trying to enable encryption...");

    sfree(rsabuf);

    cipher = cipher_type == SSH_CIPHER_BLOWFISH ? &ssh_blowfish_ssh1 :
	cipher_type == SSH_CIPHER_DES ? &ssh_des : &ssh_3des;
    cipher->sesskey(session_key);

    crWaitUntil(ispkt);

    if (pktin.type != SSH1_SMSG_SUCCESS) {
	bombout(("Encryption not successfully enabled"));
	crReturn(0);
    }

    logevent("Successfully started encryption");

    fflush(stdout);
    {
	if ((flags & FLAG_INTERACTIVE) && !*cfg.username) {
	    if (ssh_get_line && !ssh_getline_pw_only) {
		if (!ssh_get_line("login as: ",
				  username, sizeof(username), FALSE)) {
		    /*
		     * get_line failed to get a username.
		     * Terminate.
		     */
		    logevent("No username provided. Abandoning session.");
		    ssh_state = SSH_STATE_CLOSED;
		    crReturn(1);
		}
	    } else {
		static int ret;
		c_write_str("login as: ");
		ssh_send_ok = 1;

		setup_userpass_input(username, sizeof(username), 1);
		do {
		    crWaitUntil(!ispkt);
		    ret = process_userpass_input(in, inlen);
		} while (ret == 0);
		if (ret < 0)
		    cleanup_exit(0);
		c_write_str("\r\n");
	    }
	} else {
	    strncpy(username, cfg.username, sizeof(username));
	    username[sizeof(username)-1] = '\0';
	}

	send_packet(SSH1_CMSG_USER, PKT_STR, username, PKT_END);
	{
	    char userlog[22 + sizeof(username)];
	    sprintf(userlog, "Sent username \"%s\"", username);
	    logevent(userlog);
	    if (flags & FLAG_INTERACTIVE &&
		(!((flags & FLAG_STDERR) && (flags & FLAG_VERBOSE)))) {
		strcat(userlog, "\r\n");
		c_write_str(userlog);
	    }
	}
    }

    crWaitUntil(ispkt);

    if ((ssh_remote_bugs & BUG_CHOKES_ON_RSA)) {
	/* We must not attempt PK auth. Pretend we've already tried it. */
	tried_publickey = tried_agent = 1;
    } else {
	tried_publickey = tried_agent = 0;
    }
    tis_auth_refused = ccard_auth_refused = 0;
    /* Load the public half of cfg.keyfile so we notice if it's in Pageant */
    if (*cfg.keyfile) {
	if (!rsakey_pubblob(cfg.keyfile, &publickey_blob, &publickey_bloblen))
	    publickey_blob = NULL;
    } else
	publickey_blob = NULL;

    while (pktin.type == SSH1_SMSG_FAILURE) {
	static char password[100];
	static char prompt[200];
	static int pos;
	static char c;
	static int pwpkt_type;
	pwpkt_type = SSH1_CMSG_AUTH_PASSWORD;

	if (agent_exists() && !tried_agent) {
	    /*
	     * Attempt RSA authentication using Pageant.
	     */
	    static unsigned char request[5], *response, *p;
	    static int responselen;
	    static int i, nkeys;
	    static int authed = FALSE;
	    void *r;

	    tried_agent = 1;
	    logevent("Pageant is running. Requesting keys.");

	    /* Request the keys held by the agent. */
	    PUT_32BIT(request, 1);
	    request[4] = SSH1_AGENTC_REQUEST_RSA_IDENTITIES;
	    agent_query(request, 5, &r, &responselen);
	    response = (unsigned char *) r;
	    if (response && responselen >= 5 &&
		response[4] == SSH1_AGENT_RSA_IDENTITIES_ANSWER) {
		p = response + 5;
		nkeys = GET_32BIT(p);
		p += 4;
		{
		    char buf[64];
		    sprintf(buf, "Pageant has %d SSH1 keys", nkeys);
		    logevent(buf);
		}
		for (i = 0; i < nkeys; i++) {
		    static struct RSAKey key;
		    static Bignum challenge;
		    static char *commentp;
		    static int commentlen;

		    {
			char buf[64];
			sprintf(buf, "Trying Pageant key #%d", i);
			logevent(buf);
		    }
		    if (publickey_blob &&
			!memcmp(p, publickey_blob, publickey_bloblen)) {
			logevent("This key matches configured key file");
			tried_publickey = 1;
		    }
		    p += 4;
		    p += ssh1_read_bignum(p, &key.exponent);
		    p += ssh1_read_bignum(p, &key.modulus);
		    commentlen = GET_32BIT(p);
		    p += 4;
		    commentp = p;
		    p += commentlen;
		    send_packet(SSH1_CMSG_AUTH_RSA,
				PKT_BIGNUM, key.modulus, PKT_END);
		    crWaitUntil(ispkt);
		    if (pktin.type != SSH1_SMSG_AUTH_RSA_CHALLENGE) {
			logevent("Key refused");
			continue;
		    }
		    logevent("Received RSA challenge");
		    ssh1_read_bignum(pktin.body, &challenge);
		    {
			char *agentreq, *q, *ret;
			void *vret;
			int len, retlen;
			len = 1 + 4;   /* message type, bit count */
			len += ssh1_bignum_length(key.exponent);
			len += ssh1_bignum_length(key.modulus);
			len += ssh1_bignum_length(challenge);
			len += 16;     /* session id */
			len += 4;      /* response format */
			agentreq = smalloc(4 + len);
			PUT_32BIT(agentreq, len);
			q = agentreq + 4;
			*q++ = SSH1_AGENTC_RSA_CHALLENGE;
			PUT_32BIT(q, bignum_bitcount(key.modulus));
			q += 4;
			q += ssh1_write_bignum(q, key.exponent);
			q += ssh1_write_bignum(q, key.modulus);
			q += ssh1_write_bignum(q, challenge);
			memcpy(q, session_id, 16);
			q += 16;
			PUT_32BIT(q, 1);	/* response format */
			agent_query(agentreq, len + 4, &vret, &retlen);
			ret = vret;
			sfree(agentreq);
			if (ret) {
			    if (ret[4] == SSH1_AGENT_RSA_RESPONSE) {
				logevent("Sending Pageant's response");
				send_packet(SSH1_CMSG_AUTH_RSA_RESPONSE,
					    PKT_DATA, ret + 5, 16,
					    PKT_END);
				sfree(ret);
				crWaitUntil(ispkt);
				if (pktin.type == SSH1_SMSG_SUCCESS) {
				    logevent
					("Pageant's response accepted");
				    if (flags & FLAG_VERBOSE) {
					c_write_str
					    ("Authenticated using RSA key \"");
					c_write(commentp, commentlen);
					c_write_str("\" from agent\r\n");
				    }
				    authed = TRUE;
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
		    freebn(key.exponent);
		    freebn(key.modulus);
		    freebn(challenge);
		    if (authed)
			break;
		}
	    }
	    if (authed)
		break;
	}
	if (*cfg.keyfile && !tried_publickey)
	    pwpkt_type = SSH1_CMSG_AUTH_RSA;

	if (cfg.try_tis_auth &&
	    (supported_auths_mask & (1 << SSH1_AUTH_TIS)) &&
	    !tis_auth_refused) {
	    pwpkt_type = SSH1_CMSG_AUTH_TIS_RESPONSE;
	    logevent("Requested TIS authentication");
	    send_packet(SSH1_CMSG_AUTH_TIS, PKT_END);
	    crWaitUntil(ispkt);
	    if (pktin.type != SSH1_SMSG_AUTH_TIS_CHALLENGE) {
		logevent("TIS authentication declined");
		if (flags & FLAG_INTERACTIVE)
		    c_write_str("TIS authentication refused.\r\n");
		tis_auth_refused = 1;
		continue;
	    } else {
		int challengelen = ((pktin.body[0] << 24) |
				    (pktin.body[1] << 16) |
				    (pktin.body[2] << 8) |
				    (pktin.body[3]));
		logevent("Received TIS challenge");
		if (challengelen > sizeof(prompt) - 1)
		    challengelen = sizeof(prompt) - 1;	/* prevent overrun */
		memcpy(prompt, pktin.body + 4, challengelen);
		/* Prompt heuristic comes from OpenSSH */
		strncpy(prompt + challengelen,
		        memchr(prompt, '\n', challengelen) ?
			"": "\r\nResponse: ",
			(sizeof prompt) - challengelen);
		prompt[(sizeof prompt) - 1] = '\0';
	    }
	}
	if (cfg.try_tis_auth &&
	    (supported_auths_mask & (1 << SSH1_AUTH_CCARD)) &&
	    !ccard_auth_refused) {
	    pwpkt_type = SSH1_CMSG_AUTH_CCARD_RESPONSE;
	    logevent("Requested CryptoCard authentication");
	    send_packet(SSH1_CMSG_AUTH_CCARD, PKT_END);
	    crWaitUntil(ispkt);
	    if (pktin.type != SSH1_SMSG_AUTH_CCARD_CHALLENGE) {
		logevent("CryptoCard authentication declined");
		c_write_str("CryptoCard authentication refused.\r\n");
		ccard_auth_refused = 1;
		continue;
	    } else {
		int challengelen = ((pktin.body[0] << 24) |
				    (pktin.body[1] << 16) |
				    (pktin.body[2] << 8) |
				    (pktin.body[3]));
		logevent("Received CryptoCard challenge");
		if (challengelen > sizeof(prompt) - 1)
		    challengelen = sizeof(prompt) - 1;	/* prevent overrun */
		memcpy(prompt, pktin.body + 4, challengelen);
		strncpy(prompt + challengelen,
		        memchr(prompt, '\n', challengelen) ?
			"" : "\r\nResponse: ",
			sizeof(prompt) - challengelen);
		prompt[sizeof(prompt) - 1] = '\0';
	    }
	}
	if (pwpkt_type == SSH1_CMSG_AUTH_PASSWORD) {
	    sprintf(prompt, "%.90s@%.90s's password: ",
		    username, savedhost);
	}
	if (pwpkt_type == SSH1_CMSG_AUTH_RSA) {
	    char *comment = NULL;
	    int type;
	    char msgbuf[256];
	    if (flags & FLAG_VERBOSE)
		c_write_str("Trying public key authentication.\r\n");
	    sprintf(msgbuf, "Trying public key \"%.200s\"", cfg.keyfile);
	    logevent(msgbuf);
	    type = key_type(cfg.keyfile);
	    if (type != SSH_KEYTYPE_SSH1) {
		sprintf(msgbuf, "Key is of wrong type (%s)",
			key_type_to_str(type));
		logevent(msgbuf);
		c_write_str(msgbuf);
		c_write_str("\r\n");
		tried_publickey = 1;
		continue;
	    }
	    if (!rsakey_encrypted(cfg.keyfile, &comment)) {
		if (flags & FLAG_VERBOSE)
		    c_write_str("No passphrase required.\r\n");
		goto tryauth;
	    }
	    sprintf(prompt, "Passphrase for key \"%.100s\": ", comment);
	    sfree(comment);
	}

	/*
	 * Show password prompt, having first obtained it via a TIS
	 * or CryptoCard exchange if we're doing TIS or CryptoCard
	 * authentication.
	 */
	if (ssh_get_line) {
	    if (!ssh_get_line(prompt, password, sizeof(password), TRUE)) {
		/*
		 * get_line failed to get a password (for example
		 * because one was supplied on the command line
		 * which has already failed to work). Terminate.
		 */
		send_packet(SSH1_MSG_DISCONNECT,
			    PKT_STR, "No more passwords available to try",
			    PKT_END);
		logevent("Unable to authenticate");
		connection_fatal("Unable to authenticate");
		ssh_state = SSH_STATE_CLOSED;
		crReturn(1);
	    }
	} else {
	    /* Prompt may have come from server. We've munged it a bit, so
	     * we know it to be zero-terminated at least once. */
	    static int ret;
	    c_write_untrusted(prompt, strlen(prompt));
	    pos = 0;

	    setup_userpass_input(password, sizeof(password), 0);
	    do {
		crWaitUntil(!ispkt);
		ret = process_userpass_input(in, inlen);
	    } while (ret == 0);
	    if (ret < 0)
		cleanup_exit(0);
	    c_write_str("\r\n");
	}

      tryauth:
	if (pwpkt_type == SSH1_CMSG_AUTH_RSA) {
	    /*
	     * Try public key authentication with the specified
	     * key file.
	     */
	    static struct RSAKey pubkey;
	    static Bignum challenge, response;
	    static int i;
	    static unsigned char buffer[32];

	    tried_publickey = 1;
	    i = loadrsakey(cfg.keyfile, &pubkey, password);
	    if (i == 0) {
		c_write_str("Couldn't load private key from ");
		c_write_str(cfg.keyfile);
		c_write_str(".\r\n");
		continue;	       /* go and try password */
	    }
	    if (i == -1) {
		c_write_str("Wrong passphrase.\r\n");
		tried_publickey = 0;
		continue;	       /* try again */
	    }

	    /*
	     * Send a public key attempt.
	     */
	    send_packet(SSH1_CMSG_AUTH_RSA,
			PKT_BIGNUM, pubkey.modulus, PKT_END);

	    crWaitUntil(ispkt);
	    if (pktin.type == SSH1_SMSG_FAILURE) {
		c_write_str("Server refused our public key.\r\n");
		continue;	       /* go and try password */
	    }
	    if (pktin.type != SSH1_SMSG_AUTH_RSA_CHALLENGE) {
		bombout(("Bizarre response to offer of public key"));
		crReturn(0);
	    }
	    ssh1_read_bignum(pktin.body, &challenge);
	    response = rsadecrypt(challenge, &pubkey);
	    freebn(pubkey.private_exponent);	/* burn the evidence */

	    for (i = 0; i < 32; i++) {
		buffer[i] = bignum_byte(response, 31 - i);
	    }

	    MD5Init(&md5c);
	    MD5Update(&md5c, buffer, 32);
	    MD5Update(&md5c, session_id, 16);
	    MD5Final(buffer, &md5c);

	    send_packet(SSH1_CMSG_AUTH_RSA_RESPONSE,
			PKT_DATA, buffer, 16, PKT_END);

	    crWaitUntil(ispkt);
	    if (pktin.type == SSH1_SMSG_FAILURE) {
		if (flags & FLAG_VERBOSE)
		    c_write_str
			("Failed to authenticate with our public key.\r\n");
		continue;	       /* go and try password */
	    } else if (pktin.type != SSH1_SMSG_SUCCESS) {
		bombout(
			("Bizarre response to RSA authentication response"));
		crReturn(0);
	    }

	    break;		       /* we're through! */
	} else {
	    if (pwpkt_type == SSH1_CMSG_AUTH_PASSWORD) {
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
		if (!(ssh_remote_bugs & BUG_CHOKES_ON_SSH1_IGNORE)) {
		    /*
		     * The server can deal with SSH1_MSG_IGNORE, so
		     * we can use the primary defence.
		     */
		    int bottom, top, pwlen, i;
		    char *randomstr;

		    pwlen = strlen(password);
		    if (pwlen < 16) {
			bottom = 0;    /* zero length passwords are OK! :-) */
			top = 15;
		    } else {
			bottom = pwlen & ~7;
			top = bottom + 7;
		    }

		    assert(pwlen >= bottom && pwlen <= top);

		    randomstr = smalloc(top + 1);

		    for (i = bottom; i <= top; i++) {
			if (i == pwlen)
			    defer_packet(pwpkt_type, PKT_STR, password,
					 PKT_END);
			else {
			    for (j = 0; j < i; j++) {
				do {
				    randomstr[j] = random_byte();
				} while (randomstr[j] == '\0');
			    }
			    randomstr[i] = '\0';
			    defer_packet(SSH1_MSG_IGNORE,
					 PKT_STR, randomstr, PKT_END);
			}
		    }
		    logevent("Sending password with camouflage packets");
		    ssh_pkt_defersend();
		} 
		else if (!(ssh_remote_bugs & BUG_NEEDS_SSH1_PLAIN_PASSWORD)) {
		    /*
		     * The server can't deal with SSH1_MSG_IGNORE
		     * but can deal with padded passwords, so we
		     * can use the secondary defence.
		     */
		    char string[64];
		    char *s;
		    int len;

		    len = strlen(password);
		    if (len < sizeof(string)) {
			s = string;
			strcpy(string, password);
			len++;	       /* cover the zero byte */
			while (len < sizeof(string)) {
			    string[len++] = (char) random_byte();
			}
		    } else {
			s = password;
		    }
		    logevent("Sending length-padded password");
		    send_packet(pwpkt_type, PKT_INT, len,
				PKT_DATA, s, len, PKT_END);
		} else {
		    /*
		     * The server has _both_
		     * BUG_CHOKES_ON_SSH1_IGNORE and
		     * BUG_NEEDS_SSH1_PLAIN_PASSWORD. There is
		     * therefore nothing we can do.
		     */
		    int len;
		    len = strlen(password);
		    logevent("Sending unpadded password");
		    send_packet(pwpkt_type, PKT_INT, len,
				PKT_DATA, password, len, PKT_END);
		}
	    } else {
		send_packet(pwpkt_type, PKT_STR, password, PKT_END);
	    }
	}
	logevent("Sent password");
	memset(password, 0, strlen(password));
	crWaitUntil(ispkt);
	if (pktin.type == SSH1_SMSG_FAILURE) {
	    if (flags & FLAG_VERBOSE)
		c_write_str("Access denied\r\n");
	    logevent("Authentication refused");
	} else if (pktin.type == SSH1_MSG_DISCONNECT) {
	    logevent("Received disconnect request");
	    ssh_state = SSH_STATE_CLOSED;
	    crReturn(1);
	} else if (pktin.type != SSH1_SMSG_SUCCESS) {
	    bombout(("Strange packet received, type %d", pktin.type));
	    crReturn(0);
	}
    }

    logevent("Authentication successful");

    crFinish(1);
}

void sshfwd_close(struct ssh_channel *c)
{
    if (c && !c->closes) {
	/*
	 * If the channel's remoteid is -1, we have sent
	 * CHANNEL_OPEN for this channel, but it hasn't even been
	 * acknowledged by the server. So we must set a close flag
	 * on it now, and then when the server acks the channel
	 * open, we can close it then.
	 */
	if (((int)c->remoteid) != -1) {
	    if (ssh_version == 1) {
		send_packet(SSH1_MSG_CHANNEL_CLOSE, PKT_INT, c->remoteid,
			    PKT_END);
	    } else {
		ssh2_pkt_init(SSH2_MSG_CHANNEL_CLOSE);
		ssh2_pkt_adduint32(c->remoteid);
		ssh2_pkt_send();
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
    if (ssh_version == 1) {
	send_packet(SSH1_MSG_CHANNEL_DATA,
		    PKT_INT, c->remoteid,
		    PKT_INT, len, PKT_DATA, buf, len, PKT_END);
	/*
	 * In SSH1 we can return 0 here - implying that forwarded
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
    if (ssh_version == 1) {
	if (c->v.v1.throttling && bufsize < SSH1_BUFFER_LIMIT) {
	    c->v.v1.throttling = 0;
	    ssh1_throttle(-1);
	}
    } else {
	ssh2_set_window(c, OUR_V2_WINSIZE - bufsize);
    }
}

static void ssh1_protocol(unsigned char *in, int inlen, int ispkt)
{
    crBegin;

    random_init();

    while (!do_ssh1_login(in, inlen, ispkt)) {
	crReturnV;
    }
    if (ssh_state == SSH_STATE_CLOSED)
	crReturnV;

    if (cfg.agentfwd && agent_exists()) {
	logevent("Requesting agent forwarding");
	send_packet(SSH1_CMSG_AGENT_REQUEST_FORWARDING, PKT_END);
	do {
	    crReturnV;
	} while (!ispkt);
	if (pktin.type != SSH1_SMSG_SUCCESS
	    && pktin.type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crReturnV;
	} else if (pktin.type == SSH1_SMSG_FAILURE) {
	    logevent("Agent forwarding refused");
	} else {
	    logevent("Agent forwarding enabled");
	    ssh_agentfwd_enabled = TRUE;
	}
    }

    if (cfg.x11_forward) {
	char proto[20], data[64];
	logevent("Requesting X11 forwarding");
	x11_invent_auth(proto, sizeof(proto), data, sizeof(data));
	if (ssh1_local_protoflags & SSH1_PROTOFLAG_SCREEN_NUMBER) {
	    send_packet(SSH1_CMSG_X11_REQUEST_FORWARDING,
			PKT_STR, proto, PKT_STR, data,
			PKT_INT, 0, PKT_END);
	} else {
	    send_packet(SSH1_CMSG_X11_REQUEST_FORWARDING,
			PKT_STR, proto, PKT_STR, data, PKT_END);
	}
	do {
	    crReturnV;
	} while (!ispkt);
	if (pktin.type != SSH1_SMSG_SUCCESS
	    && pktin.type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crReturnV;
	} else if (pktin.type == SSH1_SMSG_FAILURE) {
	    logevent("X11 forwarding refused");
	} else {
	    logevent("X11 forwarding enabled");
	    ssh_X11_fwd_enabled = TRUE;
	}
    }

    {
	char type;
	static char *e;
	int n;
	int sport,dport,sserv,dserv;
	char sports[256], dports[256], host[256];
	char buf[1024];
	struct servent *se;

	ssh_rportfwds = newtree234(ssh_rportcmp_ssh1);
        /* Add port forwardings. */
	e = cfg.portfwd;
	while (*e) {
	    type = *e++;
	    n = 0;
	    while (*e && *e != '\t')
		sports[n++] = *e++;
	    sports[n] = 0;
	    if (*e == '\t')
		e++;
	    n = 0;
	    while (*e && *e != ':')
		host[n++] = *e++;
	    host[n] = 0;
	    if (*e == ':')
		e++;
	    n = 0;
	    while (*e)
		dports[n++] = *e++;
	    dports[n] = 0;
	    e++;
	    dport = atoi(dports);
	    dserv = 0;
	    if (dport == 0) {
		dserv = 1;
		se = getservbyname(dports, NULL);
		if (se != NULL) {
		    dport = ntohs(se->s_port);
		} else {
		    sprintf(buf,
			    "Service lookup failed for destination port \"%s\"",
			    dports);
		    logevent(buf);
		}
	    }
	    sport = atoi(sports);
	    sserv = 0;
	    if (sport == 0) {
		sserv = 1;
		se = getservbyname(sports, NULL);
		if (se != NULL) {
		    sport = ntohs(se->s_port);
		} else {
		    sprintf(buf,
			    "Service lookup failed for source port \"%s\"",
			    sports);
		    logevent(buf);
		}
	    }
	    if (sport && dport) {
		if (type == 'L') {
		    pfd_addforward(host, dport, sport);
		    sprintf(buf, "Local port %.*s%.*s%d%.*s forwarding to"
			    " %s:%.*s%.*s%d%.*s",
			    sserv ? strlen(sports) : 0, sports,
			    sserv, "(", sport, sserv, ")",
			    host,
			    dserv ? strlen(dports) : 0, dports,
			    dserv, "(", dport, dserv, ")");
		    logevent(buf);
		} else {
		    struct ssh_rportfwd *pf;
		    pf = smalloc(sizeof(*pf));
		    strcpy(pf->dhost, host);
		    pf->dport = dport;
		    if (add234(ssh_rportfwds, pf) != pf) {
			sprintf(buf, 
				"Duplicate remote port forwarding to %s:%d",
				host, dport);
			logevent(buf);
			sfree(pf);
		    } else {
			sprintf(buf, "Requesting remote port %.*s%.*s%d%.*s"
				" forward to %s:%.*s%.*s%d%.*s",
			    sserv ? strlen(sports) : 0, sports,
			    sserv, "(", sport, sserv, ")",
			    host,
			    dserv ? strlen(dports) : 0, dports,
			    dserv, "(", dport, dserv, ")");
			logevent(buf);
			send_packet(SSH1_CMSG_PORT_FORWARD_REQUEST,
				    PKT_INT, sport,
				    PKT_STR, host,
				    PKT_INT, dport,
				    PKT_END);
			do {
			    crReturnV;
			} while (!ispkt);
			if (pktin.type != SSH1_SMSG_SUCCESS
			    && pktin.type != SSH1_SMSG_FAILURE) {
			    bombout(("Protocol confusion"));
			    crReturnV;
			} else if (pktin.type == SSH1_SMSG_FAILURE) {
			    c_write_str("Server refused port forwarding\r\n");
			    ssh_editing = ssh_echoing = 1;
			}
			logevent("Remote port forwarding enabled");
		    }
		}
	    }
	}
    }

    if (!cfg.nopty) {
	send_packet(SSH1_CMSG_REQUEST_PTY,
		    PKT_STR, cfg.termtype,
		    PKT_INT, rows, PKT_INT, cols,
		    PKT_INT, 0, PKT_INT, 0, PKT_CHAR, 0, PKT_END);
	ssh_state = SSH_STATE_INTERMED;
	do {
	    crReturnV;
	} while (!ispkt);
	if (pktin.type != SSH1_SMSG_SUCCESS
	    && pktin.type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crReturnV;
	} else if (pktin.type == SSH1_SMSG_FAILURE) {
	    c_write_str("Server refused to allocate pty\r\n");
	    ssh_editing = ssh_echoing = 1;
	}
	logevent("Allocated pty");
    } else {
	ssh_editing = ssh_echoing = 1;
    }

    if (cfg.compression) {
	send_packet(SSH1_CMSG_REQUEST_COMPRESSION, PKT_INT, 6, PKT_END);
	do {
	    crReturnV;
	} while (!ispkt);
	if (pktin.type != SSH1_SMSG_SUCCESS
	    && pktin.type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crReturnV;
	} else if (pktin.type == SSH1_SMSG_FAILURE) {
	    c_write_str("Server refused to compress\r\n");
	}
	logevent("Started compression");
	ssh1_compressing = TRUE;
	zlib_compress_init();
	zlib_decompress_init();
    }

    /*
     * Start the shell or command.
     * 
     * Special case: if the first-choice command is an SSH2
     * subsystem (hence not usable here) and the second choice
     * exists, we fall straight back to that.
     */
    {
	char *cmd = cfg.remote_cmd_ptr;
	
	if (cfg.ssh_subsys && cfg.remote_cmd_ptr2) {
	    cmd = cfg.remote_cmd_ptr2;
	    ssh_fallback_cmd = TRUE;
	}
	if (*cmd)
	    send_packet(SSH1_CMSG_EXEC_CMD, PKT_STR, cmd, PKT_END);
	else
	    send_packet(SSH1_CMSG_EXEC_SHELL, PKT_END);
	logevent("Started session");
    }

    ssh_state = SSH_STATE_SESSION;
    if (size_needed)
	ssh_size();
    if (eof_needed)
	ssh_special(TS_EOF);

    ldisc_send(NULL, 0, 0);	       /* cause ldisc to notice changes */
    ssh_send_ok = 1;
    ssh_channels = newtree234(ssh_channelcmp);
    while (1) {
	crReturnV;
	if (ispkt) {
	    if (pktin.type == SSH1_SMSG_STDOUT_DATA ||
		pktin.type == SSH1_SMSG_STDERR_DATA) {
		long len = GET_32BIT(pktin.body);
		int bufsize =
		    from_backend(pktin.type == SSH1_SMSG_STDERR_DATA,
				 pktin.body + 4, len);
		if (!ssh1_stdout_throttling && bufsize > SSH1_BUFFER_LIMIT) {
		    ssh1_stdout_throttling = 1;
		    ssh1_throttle(+1);
		}
	    } else if (pktin.type == SSH1_MSG_DISCONNECT) {
		ssh_state = SSH_STATE_CLOSED;
		logevent("Received disconnect request");
		crReturnV;
	    } else if (pktin.type == SSH1_SMSG_X11_OPEN) {
		/* Remote side is trying to open a channel to talk to our
		 * X-Server. Give them back a local channel number. */
		struct ssh_channel *c;

		logevent("Received X11 connect request");
		/* Refuse if X11 forwarding is disabled. */
		if (!ssh_X11_fwd_enabled) {
		    send_packet(SSH1_MSG_CHANNEL_OPEN_FAILURE,
				PKT_INT, GET_32BIT(pktin.body), PKT_END);
		    logevent("Rejected X11 connect request");
		} else {
		    c = smalloc(sizeof(struct ssh_channel));

		    if (x11_init(&c->u.x11.s, cfg.x11_display, c) != NULL) {
			logevent("opening X11 forward connection failed");
			sfree(c);
			send_packet(SSH1_MSG_CHANNEL_OPEN_FAILURE,
				    PKT_INT, GET_32BIT(pktin.body),
				    PKT_END);
		    } else {
			logevent
			    ("opening X11 forward connection succeeded");
			c->remoteid = GET_32BIT(pktin.body);
			c->localid = alloc_channel_id();
			c->closes = 0;
			c->v.v1.throttling = 0;
			c->type = CHAN_X11;	/* identify channel type */
			add234(ssh_channels, c);
			send_packet(SSH1_MSG_CHANNEL_OPEN_CONFIRMATION,
				    PKT_INT, c->remoteid, PKT_INT,
				    c->localid, PKT_END);
			logevent("Opened X11 forward channel");
		    }
		}
	    } else if (pktin.type == SSH1_SMSG_AGENT_OPEN) {
		/* Remote side is trying to open a channel to talk to our
		 * agent. Give them back a local channel number. */
		struct ssh_channel *c;

		/* Refuse if agent forwarding is disabled. */
		if (!ssh_agentfwd_enabled) {
		    send_packet(SSH1_MSG_CHANNEL_OPEN_FAILURE,
				PKT_INT, GET_32BIT(pktin.body), PKT_END);
		} else {
		    c = smalloc(sizeof(struct ssh_channel));
		    c->remoteid = GET_32BIT(pktin.body);
		    c->localid = alloc_channel_id();
		    c->closes = 0;
		    c->v.v1.throttling = 0;
		    c->type = CHAN_AGENT;	/* identify channel type */
		    c->u.a.lensofar = 0;
		    add234(ssh_channels, c);
		    send_packet(SSH1_MSG_CHANNEL_OPEN_CONFIRMATION,
				PKT_INT, c->remoteid, PKT_INT, c->localid,
				PKT_END);
		}
	    } else if (pktin.type == SSH1_MSG_PORT_OPEN) {
   		/* Remote side is trying to open a channel to talk to a
		 * forwarded port. Give them back a local channel number. */
		struct ssh_channel *c;
		struct ssh_rportfwd pf;
		int hostsize, port;
		char host[256], buf[1024];
		char *p, *h, *e;
		c = smalloc(sizeof(struct ssh_channel));

		hostsize = GET_32BIT(pktin.body+4);
		for(h = host, p = pktin.body+8; hostsize != 0; hostsize--) {
		    if (h+1 < host+sizeof(host))
			*h++ = *p;
		    p++;
		}
		*h = 0;
		port = GET_32BIT(p);

		strcpy(pf.dhost, host);
		pf.dport = port;

		if (find234(ssh_rportfwds, &pf, NULL) == NULL) {
		    sprintf(buf, "Rejected remote port open request for %s:%d",
			    host, port);
		    logevent(buf);
                    send_packet(SSH1_MSG_CHANNEL_OPEN_FAILURE,
                                PKT_INT, GET_32BIT(pktin.body), PKT_END);
		} else {
		    sprintf(buf, "Received remote port open request for %s:%d",
			    host, port);
		    logevent(buf);
		    e = pfd_newconnect(&c->u.pfd.s, host, port, c);
		    if (e != NULL) {
			char buf[256];
			sprintf(buf, "Port open failed: %s", e);
			logevent(buf);
			sfree(c);
			send_packet(SSH1_MSG_CHANNEL_OPEN_FAILURE,
				    PKT_INT, GET_32BIT(pktin.body),
				    PKT_END);
		    } else {
			c->remoteid = GET_32BIT(pktin.body);
			c->localid = alloc_channel_id();
			c->closes = 0;
			c->v.v1.throttling = 0;
			c->type = CHAN_SOCKDATA;	/* identify channel type */
			add234(ssh_channels, c);
			send_packet(SSH1_MSG_CHANNEL_OPEN_CONFIRMATION,
				    PKT_INT, c->remoteid, PKT_INT,
				    c->localid, PKT_END);
			logevent("Forwarded port opened successfully");
		    }
		}

	    } else if (pktin.type == SSH1_MSG_CHANNEL_OPEN_CONFIRMATION) {
		unsigned int remoteid = GET_32BIT(pktin.body);
		unsigned int localid = GET_32BIT(pktin.body+4);
		struct ssh_channel *c;

		c = find234(ssh_channels, &remoteid, ssh_channelfind);
		if (c && c->type == CHAN_SOCKDATA_DORMANT) {
		    c->remoteid = localid;
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
		    send_packet(SSH1_MSG_CHANNEL_CLOSE, PKT_INT, c->remoteid,
				PKT_END);
		}

	    } else if (pktin.type == SSH1_MSG_CHANNEL_OPEN_FAILURE) {
		unsigned int remoteid = GET_32BIT(pktin.body);
		unsigned int localid = GET_32BIT(pktin.body+4);
		struct ssh_channel *c;

		c = find234(ssh_channels, &remoteid, ssh_channelfind);
		if (c && c->type == CHAN_SOCKDATA_DORMANT) {
		    logevent("Forwarded connection refused by server");
		    pfd_close(c->u.pfd.s);
		    del234(ssh_channels, c);
		    sfree(c);
		}

	    } else if (pktin.type == SSH1_MSG_CHANNEL_CLOSE ||
		       pktin.type == SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION) {
		/* Remote side closes a channel. */
		unsigned i = GET_32BIT(pktin.body);
		struct ssh_channel *c;
		c = find234(ssh_channels, &i, ssh_channelfind);
		if (c && ((int)c->remoteid) != -1) {
		    int closetype;
		    closetype =
			(pktin.type == SSH1_MSG_CHANNEL_CLOSE ? 1 : 2);

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
			send_packet(pktin.type, PKT_INT, c->remoteid,
				    PKT_END);
			c->closes |= closetype;      /* sent it too */
		    }

		    if (c->closes == 15) {
			del234(ssh_channels, c);
			sfree(c);
		    }
		} else {
		    bombout(("Received CHANNEL_CLOSE%s for %s channel %d\n",
			     pktin.type == SSH1_MSG_CHANNEL_CLOSE ? "" :
			     "_CONFIRMATION", c ? "half-open" : "nonexistent",
			     i));
		}
	    } else if (pktin.type == SSH1_MSG_CHANNEL_DATA) {
		/* Data sent down one of our channels. */
		int i = GET_32BIT(pktin.body);
		int len = GET_32BIT(pktin.body + 4);
		unsigned char *p = pktin.body + 8;
		struct ssh_channel *c;
		c = find234(ssh_channels, &i, ssh_channelfind);
		if (c) {
		    int bufsize;
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
				int l = min(4 - c->u.a.lensofar, len);
				memcpy(c->u.a.msglen + c->u.a.lensofar, p,
				       l);
				p += l;
				len -= l;
				c->u.a.lensofar += l;
			    }
			    if (c->u.a.lensofar == 4) {
				c->u.a.totallen =
				    4 + GET_32BIT(c->u.a.msglen);
				c->u.a.message = smalloc(c->u.a.totallen);
				memcpy(c->u.a.message, c->u.a.msglen, 4);
			    }
			    if (c->u.a.lensofar >= 4 && len > 0) {
				int l =
				    min(c->u.a.totallen - c->u.a.lensofar,
					len);
				memcpy(c->u.a.message + c->u.a.lensofar, p,
				       l);
				p += l;
				len -= l;
				c->u.a.lensofar += l;
			    }
			    if (c->u.a.lensofar == c->u.a.totallen) {
				void *reply, *sentreply;
				int replylen;
				agent_query(c->u.a.message,
					    c->u.a.totallen, &reply,
					    &replylen);
				if (reply)
				    sentreply = reply;
				else {
				    /* Fake SSH_AGENT_FAILURE. */
				    sentreply = "\0\0\0\1\5";
				    replylen = 5;
				}
				send_packet(SSH1_MSG_CHANNEL_DATA,
					    PKT_INT, c->remoteid,
					    PKT_INT, replylen,
					    PKT_DATA, sentreply, replylen,
					    PKT_END);
				if (reply)
				    sfree(reply);
				sfree(c->u.a.message);
				c->u.a.lensofar = 0;
			    }
			}
			bufsize = 0;   /* agent channels never back up */
			break;
		    }
		    if (!c->v.v1.throttling && bufsize > SSH1_BUFFER_LIMIT) {
			c->v.v1.throttling = 1;
			ssh1_throttle(+1);
		    }
		}
	    } else if (pktin.type == SSH1_SMSG_SUCCESS) {
		/* may be from EXEC_SHELL on some servers */
	    } else if (pktin.type == SSH1_SMSG_FAILURE) {
		/* may be from EXEC_SHELL on some servers
		 * if no pty is available or in other odd cases. Ignore */
	    } else if (pktin.type == SSH1_SMSG_EXIT_STATUS) {
		char buf[100];
		ssh_exitcode = GET_32BIT(pktin.body);
		sprintf(buf, "Server sent command exit status %d",
			ssh_exitcode);
		logevent(buf);
		send_packet(SSH1_CMSG_EXIT_CONFIRMATION, PKT_END);
                /*
                 * In case `helpful' firewalls or proxies tack
                 * extra human-readable text on the end of the
                 * session which we might mistake for another
                 * encrypted packet, we close the session once
                 * we've sent EXIT_CONFIRMATION.
                 */
                ssh_state = SSH_STATE_CLOSED;
                crReturnV;
	    } else {
		bombout(("Strange packet received: type %d", pktin.type));
		crReturnV;
	    }
	} else {
	    while (inlen > 0) {
		int len = min(inlen, 512);
		send_packet(SSH1_CMSG_STDIN_DATA,
			    PKT_INT, len, PKT_DATA, in, len, PKT_END);
		in += len;
		inlen -= len;
	    }
	}
    }

    crFinishV;
}

/*
 * Utility routine for decoding comma-separated strings in KEXINIT.
 */
static int in_commasep_string(char *needle, char *haystack, int haylen)
{
    int needlen;
    if (!needle || !haystack)
	return 0;		       /* protect against null pointers */
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
 * SSH2 key creation method.
 */
static void ssh2_mkkey(Bignum K, char *H, char *sessid, char chr,
		       char *keyspace)
{
    SHA_State s;
    /* First 20 bytes. */
    SHA_Init(&s);
    if (!(ssh_remote_bugs & BUG_SSH2_DERIVEKEY))
	sha_mpint(&s, K);
    SHA_Bytes(&s, H, 20);
    SHA_Bytes(&s, &chr, 1);
    SHA_Bytes(&s, sessid, 20);
    SHA_Final(&s, keyspace);
    /* Next 20 bytes. */
    SHA_Init(&s);
    if (!(ssh_remote_bugs & BUG_SSH2_DERIVEKEY))
	sha_mpint(&s, K);
    SHA_Bytes(&s, H, 20);
    SHA_Bytes(&s, keyspace, 20);
    SHA_Final(&s, keyspace + 20);
}

/*
 * Handle the SSH2 transport layer.
 */
static int do_ssh2_transport(unsigned char *in, int inlen, int ispkt)
{
    static int i, j, len, nbits, pbits, warn;
    static char *str;
    static Bignum p, g, e, f, K;
    static int kex_init_value, kex_reply_value;
    static const struct ssh_mac **maclist;
    static int nmacs;
    static const struct ssh2_cipher *cscipher_tobe = NULL;
    static const struct ssh2_cipher *sccipher_tobe = NULL;
    static const struct ssh_mac *csmac_tobe = NULL;
    static const struct ssh_mac *scmac_tobe = NULL;
    static const struct ssh_compress *cscomp_tobe = NULL;
    static const struct ssh_compress *sccomp_tobe = NULL;
    static char *hostkeydata, *sigdata, *keystr, *fingerprint;
    static int hostkeylen, siglen;
    static void *hkey;		       /* actual host key */
    static unsigned char exchange_hash[20];
    static unsigned char keyspace[40];
    static int n_preferred_ciphers;
    static const struct ssh2_ciphers *preferred_ciphers[CIPHER_MAX];
    static const struct ssh_compress *preferred_comp;
    static int cipherstr_started;
    static int first_kex;

    crBegin;
    random_init();
    first_kex = 1;

    /*
     * Set up the preferred ciphers. (NULL => warn below here)
     */
    n_preferred_ciphers = 0;
    for (i = 0; i < CIPHER_MAX; i++) {
	switch (cfg.ssh_cipherlist[i]) {
	  case CIPHER_BLOWFISH:
	    preferred_ciphers[n_preferred_ciphers] = &ssh2_blowfish;
	    n_preferred_ciphers++;
	    break;
	  case CIPHER_DES:
	    if (cfg.ssh2_des_cbc) {
		preferred_ciphers[n_preferred_ciphers] = &ssh2_des;
		n_preferred_ciphers++;
	    }
	    break;
	  case CIPHER_3DES:
	    preferred_ciphers[n_preferred_ciphers] = &ssh2_3des;
	    n_preferred_ciphers++;
	    break;
	  case CIPHER_AES:
	    preferred_ciphers[n_preferred_ciphers] = &ssh2_aes;
	    n_preferred_ciphers++;
	    break;
	  case CIPHER_WARN:
	    /* Flag for later. Don't bother if it's the last in
	     * the list. */
	    if (i < CIPHER_MAX - 1) {
		preferred_ciphers[n_preferred_ciphers] = NULL;
		n_preferred_ciphers++;
	    }
	    break;
	}
    }

    /*
     * Set up preferred compression.
     */
    if (cfg.compression)
	preferred_comp = &ssh_zlib;
    else
	preferred_comp = &ssh_comp_none;

    /*
     * Be prepared to work around the buggy MAC problem.
     */
    if (ssh_remote_bugs & BUG_SSH2_HMAC)
	maclist = buggymacs, nmacs = lenof(buggymacs);
    else
	maclist = macs, nmacs = lenof(macs);

  begin_key_exchange:
    /*
     * Construct and send our key exchange packet.
     */
    ssh2_pkt_init(SSH2_MSG_KEXINIT);
    for (i = 0; i < 16; i++)
	ssh2_pkt_addbyte((unsigned char) random_byte());
    /* List key exchange algorithms. */
    ssh2_pkt_addstring_start();
    for (i = 0; i < lenof(kex_algs); i++) {
	if (kex_algs[i] == &ssh_diffiehellman_gex &&
	    (ssh_remote_bugs & BUG_SSH2_DH_GEX))
	    continue;
	ssh2_pkt_addstring_str(kex_algs[i]->name);
	if (i < lenof(kex_algs) - 1)
	    ssh2_pkt_addstring_str(",");
    }
    /* List server host key algorithms. */
    ssh2_pkt_addstring_start();
    for (i = 0; i < lenof(hostkey_algs); i++) {
	ssh2_pkt_addstring_str(hostkey_algs[i]->name);
	if (i < lenof(hostkey_algs) - 1)
	    ssh2_pkt_addstring_str(",");
    }
    /* List client->server encryption algorithms. */
    ssh2_pkt_addstring_start();
    cipherstr_started = 0;
    for (i = 0; i < n_preferred_ciphers; i++) {
	const struct ssh2_ciphers *c = preferred_ciphers[i];
	if (!c) continue;	       /* warning flag */
	for (j = 0; j < c->nciphers; j++) {
	    if (cipherstr_started)
		ssh2_pkt_addstring_str(",");
	    ssh2_pkt_addstring_str(c->list[j]->name);
	    cipherstr_started = 1;
	}
    }
    /* List server->client encryption algorithms. */
    ssh2_pkt_addstring_start();
    cipherstr_started = 0;
    for (i = 0; i < n_preferred_ciphers; i++) {
	const struct ssh2_ciphers *c = preferred_ciphers[i];
	if (!c) continue; /* warning flag */
	for (j = 0; j < c->nciphers; j++) {
	    if (cipherstr_started)
		ssh2_pkt_addstring_str(",");
	    ssh2_pkt_addstring_str(c->list[j]->name);
	    cipherstr_started = 1;
	}
    }
    /* List client->server MAC algorithms. */
    ssh2_pkt_addstring_start();
    for (i = 0; i < nmacs; i++) {
	ssh2_pkt_addstring_str(maclist[i]->name);
	if (i < nmacs - 1)
	    ssh2_pkt_addstring_str(",");
    }
    /* List server->client MAC algorithms. */
    ssh2_pkt_addstring_start();
    for (i = 0; i < nmacs; i++) {
	ssh2_pkt_addstring_str(maclist[i]->name);
	if (i < nmacs - 1)
	    ssh2_pkt_addstring_str(",");
    }
    /* List client->server compression algorithms. */
    ssh2_pkt_addstring_start();
    for (i = 0; i < lenof(compressions) + 1; i++) {
	const struct ssh_compress *c =
	    i == 0 ? preferred_comp : compressions[i - 1];
	ssh2_pkt_addstring_str(c->name);
	if (i < lenof(compressions))
	    ssh2_pkt_addstring_str(",");
    }
    /* List server->client compression algorithms. */
    ssh2_pkt_addstring_start();
    for (i = 0; i < lenof(compressions) + 1; i++) {
	const struct ssh_compress *c =
	    i == 0 ? preferred_comp : compressions[i - 1];
	ssh2_pkt_addstring_str(c->name);
	if (i < lenof(compressions))
	    ssh2_pkt_addstring_str(",");
    }
    /* List client->server languages. Empty list. */
    ssh2_pkt_addstring_start();
    /* List server->client languages. Empty list. */
    ssh2_pkt_addstring_start();
    /* First KEX packet does _not_ follow, because we're not that brave. */
    ssh2_pkt_addbool(FALSE);
    /* Reserved. */
    ssh2_pkt_adduint32(0);

    exhash = exhashbase;
    sha_string(&exhash, pktout.data + 5, pktout.length - 5);

    ssh2_pkt_send();

    if (!ispkt)
	crWaitUntil(ispkt);
    if (pktin.length > 5)
	sha_string(&exhash, pktin.data + 5, pktin.length - 5);

    /*
     * Now examine the other side's KEXINIT to see what we're up
     * to.
     */
    if (pktin.type != SSH2_MSG_KEXINIT) {
	bombout(("expected key exchange packet from server"));
	crReturn(0);
    }
    kex = NULL;
    hostkey = NULL;
    cscipher_tobe = NULL;
    sccipher_tobe = NULL;
    csmac_tobe = NULL;
    scmac_tobe = NULL;
    cscomp_tobe = NULL;
    sccomp_tobe = NULL;
    pktin.savedpos += 16;	       /* skip garbage cookie */
    ssh2_pkt_getstring(&str, &len);    /* key exchange algorithms */
    for (i = 0; i < lenof(kex_algs); i++) {
	if (kex_algs[i] == &ssh_diffiehellman_gex &&
	    (ssh_remote_bugs & BUG_SSH2_DH_GEX))
	    continue;
	if (in_commasep_string(kex_algs[i]->name, str, len)) {
	    kex = kex_algs[i];
	    break;
	}
    }
    ssh2_pkt_getstring(&str, &len);    /* host key algorithms */
    for (i = 0; i < lenof(hostkey_algs); i++) {
	if (in_commasep_string(hostkey_algs[i]->name, str, len)) {
	    hostkey = hostkey_algs[i];
	    break;
	}
    }
    ssh2_pkt_getstring(&str, &len);    /* client->server cipher */
    warn = 0;
    for (i = 0; i < n_preferred_ciphers; i++) {
	const struct ssh2_ciphers *c = preferred_ciphers[i];
	if (!c) {
	    warn = 1;
	} else {
	    for (j = 0; j < c->nciphers; j++) {
		if (in_commasep_string(c->list[j]->name, str, len)) {
		    cscipher_tobe = c->list[j];
		    break;
		}
	    }
	}
	if (cscipher_tobe) {
	    if (warn)
		askcipher(cscipher_tobe->name, 1);
	    break;
	}
    }
    if (!cscipher_tobe) {
	bombout(("Couldn't agree a client-to-server cipher (available: %.450s)",
		 str));
	crReturn(0);
    }

    ssh2_pkt_getstring(&str, &len);    /* server->client cipher */
    warn = 0;
    for (i = 0; i < n_preferred_ciphers; i++) {
	const struct ssh2_ciphers *c = preferred_ciphers[i];
	if (!c) {
	    warn = 1;
	} else {
	    for (j = 0; j < c->nciphers; j++) {
		if (in_commasep_string(c->list[j]->name, str, len)) {
		    sccipher_tobe = c->list[j];
		    break;
		}
	    }
	}
	if (sccipher_tobe) {
	    if (warn)
		askcipher(sccipher_tobe->name, 2);
	    break;
	}
    }
    if (!sccipher_tobe) {
	bombout(("Couldn't agree a server-to-client cipher (available: %.450s)",
		 str));
	crReturn(0);
    }

    ssh2_pkt_getstring(&str, &len);    /* client->server mac */
    for (i = 0; i < nmacs; i++) {
	if (in_commasep_string(maclist[i]->name, str, len)) {
	    csmac_tobe = maclist[i];
	    break;
	}
    }
    ssh2_pkt_getstring(&str, &len);    /* server->client mac */
    for (i = 0; i < nmacs; i++) {
	if (in_commasep_string(maclist[i]->name, str, len)) {
	    scmac_tobe = maclist[i];
	    break;
	}
    }
    ssh2_pkt_getstring(&str, &len);    /* client->server compression */
    for (i = 0; i < lenof(compressions) + 1; i++) {
	const struct ssh_compress *c =
	    i == 0 ? preferred_comp : compressions[i - 1];
	if (in_commasep_string(c->name, str, len)) {
	    cscomp_tobe = c;
	    break;
	}
    }
    ssh2_pkt_getstring(&str, &len);    /* server->client compression */
    for (i = 0; i < lenof(compressions) + 1; i++) {
	const struct ssh_compress *c =
	    i == 0 ? preferred_comp : compressions[i - 1];
	if (in_commasep_string(c->name, str, len)) {
	    sccomp_tobe = c;
	    break;
	}
    }

    /*
     * Work out the number of bits of key we will need from the key
     * exchange. We start with the maximum key length of either
     * cipher...
     */
    {
	int csbits, scbits;

	csbits = cscipher_tobe->keylen;
	scbits = sccipher_tobe->keylen;
	nbits = (csbits > scbits ? csbits : scbits);
    }
    /* The keys only have 160-bit entropy, since they're based on
     * a SHA-1 hash. So cap the key size at 160 bits. */
    if (nbits > 160)
	nbits = 160;

    /*
     * If we're doing Diffie-Hellman group exchange, start by
     * requesting a group.
     */
    if (kex == &ssh_diffiehellman_gex) {
	logevent("Doing Diffie-Hellman group exchange");
	ssh_pkt_ctx |= SSH2_PKTCTX_DHGEX;
	/*
	 * Work out how big a DH group we will need to allow that
	 * much data.
	 */
	pbits = 512 << ((nbits - 1) / 64);
	ssh2_pkt_init(SSH2_MSG_KEX_DH_GEX_REQUEST);
	ssh2_pkt_adduint32(pbits);
	ssh2_pkt_send();

	crWaitUntil(ispkt);
	if (pktin.type != SSH2_MSG_KEX_DH_GEX_GROUP) {
	    bombout(("expected key exchange group packet from server"));
	    crReturn(0);
	}
	p = ssh2_pkt_getmp();
	g = ssh2_pkt_getmp();
	dh_setup_group(p, g);
	kex_init_value = SSH2_MSG_KEX_DH_GEX_INIT;
	kex_reply_value = SSH2_MSG_KEX_DH_GEX_REPLY;
    } else {
	ssh_pkt_ctx |= SSH2_PKTCTX_DHGROUP1;
	dh_setup_group1();
	kex_init_value = SSH2_MSG_KEXDH_INIT;
	kex_reply_value = SSH2_MSG_KEXDH_REPLY;
    }

    logevent("Doing Diffie-Hellman key exchange");
    /*
     * Now generate and send e for Diffie-Hellman.
     */
    e = dh_create_e(nbits * 2);
    ssh2_pkt_init(kex_init_value);
    ssh2_pkt_addmp(e);
    ssh2_pkt_send();

    crWaitUntil(ispkt);
    if (pktin.type != kex_reply_value) {
	bombout(("expected key exchange reply packet from server"));
	crReturn(0);
    }
    ssh2_pkt_getstring(&hostkeydata, &hostkeylen);
    f = ssh2_pkt_getmp();
    ssh2_pkt_getstring(&sigdata, &siglen);

    K = dh_find_K(f);

    sha_string(&exhash, hostkeydata, hostkeylen);
    if (kex == &ssh_diffiehellman_gex) {
	sha_uint32(&exhash, pbits);
	sha_mpint(&exhash, p);
	sha_mpint(&exhash, g);
    }
    sha_mpint(&exhash, e);
    sha_mpint(&exhash, f);
    sha_mpint(&exhash, K);
    SHA_Final(&exhash, exchange_hash);

    dh_cleanup();

#if 0
    debug(("Exchange hash is:\n"));
    dmemdump(exchange_hash, 20);
#endif

    hkey = hostkey->newkey(hostkeydata, hostkeylen);
    if (!hkey ||
	!hostkey->verifysig(hkey, sigdata, siglen, exchange_hash, 20)) {
	bombout(("Server's host key did not match the signature supplied"));
	crReturn(0);
    }

    /*
     * Authenticate remote host: verify host key. (We've already
     * checked the signature of the exchange hash.)
     */
    keystr = hostkey->fmtkey(hkey);
    fingerprint = hostkey->fingerprint(hkey);
    verify_ssh_host_key(savedhost, savedport, hostkey->keytype,
			keystr, fingerprint);
    if (first_kex) {		       /* don't bother logging this in rekeys */
	logevent("Host key fingerprint is:");
	logevent(fingerprint);
    }
    sfree(fingerprint);
    sfree(keystr);
    hostkey->freekey(hkey);

    /*
     * Send SSH2_MSG_NEWKEYS.
     */
    ssh2_pkt_init(SSH2_MSG_NEWKEYS);
    ssh2_pkt_send();

    /*
     * Expect SSH2_MSG_NEWKEYS from server.
     */
    crWaitUntil(ispkt);
    if (pktin.type != SSH2_MSG_NEWKEYS) {
	bombout(("expected new-keys packet from server"));
	crReturn(0);
    }

    /*
     * Create and initialise session keys.
     */
    cscipher = cscipher_tobe;
    sccipher = sccipher_tobe;
    csmac = csmac_tobe;
    scmac = scmac_tobe;
    cscomp = cscomp_tobe;
    sccomp = sccomp_tobe;
    cscomp->compress_init();
    sccomp->decompress_init();
    /*
     * Set IVs after keys. Here we use the exchange hash from the
     * _first_ key exchange.
     */
    if (first_kex)
	memcpy(ssh2_session_id, exchange_hash, sizeof(exchange_hash));
    ssh2_mkkey(K, exchange_hash, ssh2_session_id, 'C', keyspace);
    cscipher->setcskey(keyspace);
    ssh2_mkkey(K, exchange_hash, ssh2_session_id, 'D', keyspace);
    sccipher->setsckey(keyspace);
    ssh2_mkkey(K, exchange_hash, ssh2_session_id, 'A', keyspace);
    cscipher->setcsiv(keyspace);
    ssh2_mkkey(K, exchange_hash, ssh2_session_id, 'B', keyspace);
    sccipher->setsciv(keyspace);
    ssh2_mkkey(K, exchange_hash, ssh2_session_id, 'E', keyspace);
    csmac->setcskey(keyspace);
    ssh2_mkkey(K, exchange_hash, ssh2_session_id, 'F', keyspace);
    scmac->setsckey(keyspace);

    /*
     * If this is the first key exchange phase, we must pass the
     * SSH2_MSG_NEWKEYS packet to the next layer, not because it
     * wants to see it but because it will need time to initialise
     * itself before it sees an actual packet. In subsequent key
     * exchange phases, we don't pass SSH2_MSG_NEWKEYS on, because
     * it would only confuse the layer above.
     */
    if (!first_kex) {
	crReturn(0);
    }
    first_kex = 0;

    /*
     * Now we're encrypting. Begin returning 1 to the protocol main
     * function so that other things can run on top of the
     * transport. If we ever see a KEXINIT, we must go back to the
     * start.
     */
    while (!(ispkt && pktin.type == SSH2_MSG_KEXINIT)) {
	crReturn(1);
    }
    logevent("Server initiated key re-exchange");
    goto begin_key_exchange;

    crFinish(1);
}

/*
 * Add data to an SSH2 channel output buffer.
 */
static void ssh2_add_channel_data(struct ssh_channel *c, char *buf,
				  int len)
{
    bufchain_add(&c->v.v2.outbuffer, buf, len);
}

/*
 * Attempt to send data on an SSH2 channel.
 */
static int ssh2_try_send(struct ssh_channel *c)
{
    while (c->v.v2.remwindow > 0 && bufchain_size(&c->v.v2.outbuffer) > 0) {
	int len;
	void *data;
	bufchain_prefix(&c->v.v2.outbuffer, &data, &len);
	if ((unsigned)len > c->v.v2.remwindow)
	    len = c->v.v2.remwindow;
	if ((unsigned)len > c->v.v2.remmaxpkt)
	    len = c->v.v2.remmaxpkt;
	ssh2_pkt_init(SSH2_MSG_CHANNEL_DATA);
	ssh2_pkt_adduint32(c->remoteid);
	ssh2_pkt_addstring_start();
	ssh2_pkt_addstring_data(data, len);
	ssh2_pkt_send();
	bufchain_consume(&c->v.v2.outbuffer, len);
	c->v.v2.remwindow -= len;
    }

    /*
     * After having sent as much data as we can, return the amount
     * still buffered.
     */
    return bufchain_size(&c->v.v2.outbuffer);
}

/*
 * Potentially enlarge the window on an SSH2 channel.
 */
static void ssh2_set_window(struct ssh_channel *c, unsigned newwin)
{
    /*
     * Never send WINDOW_ADJUST for a channel that the remote side
     * already thinks it's closed; there's no point, since it won't
     * be sending any more data anyway.
     */
    if (c->closes != 0)
	return;

    if (newwin > c->v.v2.locwindow) {
	ssh2_pkt_init(SSH2_MSG_CHANNEL_WINDOW_ADJUST);
	ssh2_pkt_adduint32(c->remoteid);
	ssh2_pkt_adduint32(newwin - c->v.v2.locwindow);
	ssh2_pkt_send();
	c->v.v2.locwindow = newwin;
    }
}

/*
 * Handle the SSH2 userauth and connection layers.
 */
static void do_ssh2_authconn(unsigned char *in, int inlen, int ispkt)
{
    static enum {
	AUTH_INVALID, AUTH_PUBLICKEY_AGENT, AUTH_PUBLICKEY_FILE,
	AUTH_PASSWORD,
        AUTH_KEYBOARD_INTERACTIVE
    } method;
    static enum {
	AUTH_TYPE_NONE,
	AUTH_TYPE_PUBLICKEY,
	AUTH_TYPE_PUBLICKEY_OFFER_LOUD,
	AUTH_TYPE_PUBLICKEY_OFFER_QUIET,
	AUTH_TYPE_PASSWORD,
	AUTH_TYPE_KEYBOARD_INTERACTIVE,
	AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET
    } type;
    static int gotit, need_pw, can_pubkey, can_passwd, can_keyb_inter;
    static int tried_pubkey_config, tried_agent, tried_keyb_inter;
    static int kbd_inter_running;
    static int we_are_in;
    static int num_prompts, curr_prompt, echo;
    static char username[100];
    static int got_username;
    static char pwprompt[200];
    static char password[100];
    static void *publickey_blob;
    static int publickey_bloblen;

    crBegin;

    /*
     * Request userauth protocol, and await a response to it.
     */
    ssh2_pkt_init(SSH2_MSG_SERVICE_REQUEST);
    ssh2_pkt_addstring("ssh-userauth");
    ssh2_pkt_send();
    crWaitUntilV(ispkt);
    if (pktin.type != SSH2_MSG_SERVICE_ACCEPT) {
	bombout(("Server refused user authentication protocol"));
	crReturnV;
    }

    /*
     * We repeat this whole loop, including the username prompt,
     * until we manage a successful authentication. If the user
     * types the wrong _password_, they are sent back to the
     * beginning to try another username. (If they specify a
     * username in the config, they are never asked, even if they
     * do give a wrong password.)
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
    username[0] = '\0';
    got_username = FALSE;
    do {
	/*
	 * Get a username.
	 */
	if (got_username && !cfg.change_username) {
	    /*
	     * We got a username last time round this loop, and
	     * with change_username turned off we don't try to get
	     * it again.
	     */
	} else if ((flags & FLAG_INTERACTIVE) && !*cfg.username) {
	    if (ssh_get_line && !ssh_getline_pw_only) {
		if (!ssh_get_line("login as: ",
				  username, sizeof(username), FALSE)) {
		    /*
		     * get_line failed to get a username.
		     * Terminate.
		     */
		    logevent("No username provided. Abandoning session.");
		    ssh_state = SSH_STATE_CLOSED;
		    crReturnV;
		}
	    } else {
		static int ret;
		c_write_str("login as: ");
		ssh_send_ok = 1;
		setup_userpass_input(username, sizeof(username), 1);
		do {
		    crWaitUntilV(!ispkt);
		    ret = process_userpass_input(in, inlen);
		} while (ret == 0);
		if (ret < 0)
		    cleanup_exit(0);
	    }
	    c_write_str("\r\n");
	    username[strcspn(username, "\n\r")] = '\0';
	} else {
	    char stuff[200];
	    strncpy(username, cfg.username, sizeof(username));
	    username[sizeof(username)-1] = '\0';
	    if ((flags & FLAG_VERBOSE) || (flags & FLAG_INTERACTIVE)) {
		sprintf(stuff, "Using username \"%s\".\r\n", username);
		c_write_str(stuff);
	    }
	}
	got_username = TRUE;

	/*
	 * Send an authentication request using method "none": (a)
	 * just in case it succeeds, and (b) so that we know what
	 * authentication methods we can usefully try next.
	 */
	ssh_pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;

	ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
	ssh2_pkt_addstring(username);
	ssh2_pkt_addstring("ssh-connection");	/* service requested */
	ssh2_pkt_addstring("none");    /* method */
	ssh2_pkt_send();
	type = AUTH_TYPE_NONE;
	gotit = FALSE;
	we_are_in = FALSE;

	tried_pubkey_config = FALSE;
	tried_agent = FALSE;
	tried_keyb_inter = FALSE;
	kbd_inter_running = FALSE;
	/* Load the pub half of cfg.keyfile so we notice if it's in Pageant */
	if (*cfg.keyfile) {
	    int keytype;
	    logeventf("Reading private key file \"%.150s\"", cfg.keyfile);
	    keytype = key_type(cfg.keyfile);
	    if (keytype == SSH_KEYTYPE_SSH2)
		publickey_blob = ssh2_userkey_loadpub(cfg.keyfile, NULL,
						      &publickey_bloblen);
	    else {
		char msgbuf[256];
		logeventf("Unable to use this key file (%s)",
			key_type_to_str(keytype));
		sprintf(msgbuf, "Unable to use key file \"%.150s\" (%s)\r\n",
			cfg.keyfile, key_type_to_str(keytype));
		c_write_str(msgbuf);
		publickey_blob = NULL;
	    }
	} else
	    publickey_blob = NULL;

	while (1) {
	    /*
	     * Wait for the result of the last authentication request.
	     */
	    if (!gotit)
		crWaitUntilV(ispkt);
	    while (pktin.type == SSH2_MSG_USERAUTH_BANNER) {
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
		    ssh2_pkt_getstring(&banner, &size);
		    if (banner)
			c_write_untrusted(banner, size);
		}
		crWaitUntilV(ispkt);
	    }
	    if (pktin.type == SSH2_MSG_USERAUTH_SUCCESS) {
		logevent("Access granted");
		we_are_in = TRUE;
		break;
	    }

	    if (kbd_inter_running &&
		pktin.type == SSH2_MSG_USERAUTH_INFO_REQUEST) {
		/*
		 * This is either a further set-of-prompts packet
		 * in keyboard-interactive authentication, or it's
		 * the same one and we came back here with `gotit'
		 * set. In the former case, we must reset the
		 * curr_prompt variable.
		 */
		if (!gotit)
		    curr_prompt = 0;
	    } else if (pktin.type != SSH2_MSG_USERAUTH_FAILURE) {
		bombout(("Strange packet received during authentication: type %d",
			 pktin.type));
		crReturnV;
	    }

	    gotit = FALSE;

	    /*
	     * OK, we're now sitting on a USERAUTH_FAILURE message, so
	     * we can look at the string in it and know what we can
	     * helpfully try next.
	     */
	    if (pktin.type == SSH2_MSG_USERAUTH_FAILURE) {
		char *methods;
		int methlen;
		ssh2_pkt_getstring(&methods, &methlen);
		kbd_inter_running = FALSE;
		if (!ssh2_pkt_getbool()) {
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
		     * prompt.
		     */
		    if (type == AUTH_TYPE_NONE) {
			/* do nothing */
		    } else if (type == AUTH_TYPE_PUBLICKEY_OFFER_LOUD ||
			       type == AUTH_TYPE_PUBLICKEY_OFFER_QUIET) {
			if (type == AUTH_TYPE_PUBLICKEY_OFFER_LOUD)
			    c_write_str("Server refused our key\r\n");
			logevent("Server refused public key");
		    } else if (type == AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET) {
			/* server declined keyboard-interactive; ignore */
		    } else {
			c_write_str("Access denied\r\n");
			logevent("Access denied");
			if (type == AUTH_TYPE_PASSWORD) {
			    we_are_in = FALSE;
			    break;
			}
		    }
		} else {
		    c_write_str("Further authentication required\r\n");
		    logevent("Further authentication required");
		}

		can_pubkey =
		    in_commasep_string("publickey", methods, methlen);
		can_passwd =
		    in_commasep_string("password", methods, methlen);
		can_keyb_inter = cfg.try_ki_auth &&
		    in_commasep_string("keyboard-interactive", methods, methlen);
	    }

	    method = 0;
	    ssh_pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;

	    /*
	     * Most password/passphrase prompts will be
	     * non-echoing, so we set this to 0 by default.
	     * Exception is that some keyboard-interactive prompts
	     * can be echoing, in which case we'll set this to 1.
	     */
	    echo = 0;

	    if (!method && can_pubkey && agent_exists() && !tried_agent) {
		/*
		 * Attempt public-key authentication using Pageant.
		 */
		static unsigned char request[5], *response, *p;
		static int responselen;
		static int i, nkeys;
		static int authed = FALSE;
		void *r;

		ssh_pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
		ssh_pkt_ctx |= SSH2_PKTCTX_PUBLICKEY;

		tried_agent = TRUE;

		logevent("Pageant is running. Requesting keys.");

		/* Request the keys held by the agent. */
		PUT_32BIT(request, 1);
		request[4] = SSH2_AGENTC_REQUEST_IDENTITIES;
		agent_query(request, 5, &r, &responselen);
		response = (unsigned char *) r;
		if (response && responselen >= 5 &&
		    response[4] == SSH2_AGENT_IDENTITIES_ANSWER) {
		    p = response + 5;
		    nkeys = GET_32BIT(p);
		    p += 4;
		    {
			char buf[64];
			sprintf(buf, "Pageant has %d SSH2 keys", nkeys);
			logevent(buf);
		    }
		    for (i = 0; i < nkeys; i++) {
			static char *pkblob, *alg, *commentp;
			static int pklen, alglen, commentlen;
			static int siglen, retlen, len;
			static char *q, *agentreq, *ret;
			void *vret;

			{
			    char buf[64];
			    sprintf(buf, "Trying Pageant key #%d", i);
			    logevent(buf);
			}
			pklen = GET_32BIT(p);
			p += 4;
			if (publickey_blob &&
			    pklen == publickey_bloblen &&
			    !memcmp(p, publickey_blob, publickey_bloblen)) {
			    logevent("This key matches configured key file");
			    tried_pubkey_config = 1;
			}
			pkblob = p;
			p += pklen;
			alglen = GET_32BIT(pkblob);
			alg = pkblob + 4;
			commentlen = GET_32BIT(p);
			p += 4;
			commentp = p;
			p += commentlen;
			ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
			ssh2_pkt_addstring(username);
			ssh2_pkt_addstring("ssh-connection");	/* service requested */
			ssh2_pkt_addstring("publickey");	/* method */
			ssh2_pkt_addbool(FALSE);	/* no signature included */
			ssh2_pkt_addstring_start();
			ssh2_pkt_addstring_data(alg, alglen);
			ssh2_pkt_addstring_start();
			ssh2_pkt_addstring_data(pkblob, pklen);
			ssh2_pkt_send();

			crWaitUntilV(ispkt);
			if (pktin.type != SSH2_MSG_USERAUTH_PK_OK) {
			    logevent("Key refused");
			    continue;
			}

			if (flags & FLAG_VERBOSE) {
			    c_write_str
				("Authenticating with public key \"");
			    c_write(commentp, commentlen);
			    c_write_str("\" from agent\r\n");
			}

			/*
			 * Server is willing to accept the key.
			 * Construct a SIGN_REQUEST.
			 */
			ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
			ssh2_pkt_addstring(username);
			ssh2_pkt_addstring("ssh-connection");	/* service requested */
			ssh2_pkt_addstring("publickey");	/* method */
			ssh2_pkt_addbool(TRUE);
			ssh2_pkt_addstring_start();
			ssh2_pkt_addstring_data(alg, alglen);
			ssh2_pkt_addstring_start();
			ssh2_pkt_addstring_data(pkblob, pklen);

			siglen = pktout.length - 5 + 4 + 20;
			len = 1;       /* message type */
			len += 4 + pklen;	/* key blob */
			len += 4 + siglen;	/* data to sign */
			len += 4;      /* flags */
			agentreq = smalloc(4 + len);
			PUT_32BIT(agentreq, len);
			q = agentreq + 4;
			*q++ = SSH2_AGENTC_SIGN_REQUEST;
			PUT_32BIT(q, pklen);
			q += 4;
			memcpy(q, pkblob, pklen);
			q += pklen;
			PUT_32BIT(q, siglen);
			q += 4;
			/* Now the data to be signed... */
			PUT_32BIT(q, 20);
			q += 4;
			memcpy(q, ssh2_session_id, 20);
			q += 20;
			memcpy(q, pktout.data + 5, pktout.length - 5);
			q += pktout.length - 5;
			/* And finally the (zero) flags word. */
			PUT_32BIT(q, 0);
			agent_query(agentreq, len + 4, &vret, &retlen);
			ret = vret;
			sfree(agentreq);
			if (ret) {
			    if (ret[4] == SSH2_AGENT_SIGN_RESPONSE) {
				logevent("Sending Pageant's response");
				ssh2_add_sigblob(pkblob, pklen,
						 ret + 9, GET_32BIT(ret + 5));
				ssh2_pkt_send();
				authed = TRUE;
				break;
			    } else {
				logevent
				    ("Pageant failed to answer challenge");
				sfree(ret);
			    }
			}
		    }
		    if (authed)
			continue;
		}
	    }

	    if (!method && can_pubkey && publickey_blob
		&& !tried_pubkey_config) {
		unsigned char *pub_blob;
		char *algorithm, *comment;
		int pub_blob_len;

		tried_pubkey_config = TRUE;

		ssh_pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
		ssh_pkt_ctx |= SSH2_PKTCTX_PUBLICKEY;

		/*
		 * Try the public key supplied in the configuration.
		 *
		 * First, offer the public blob to see if the server is
		 * willing to accept it.
		 */
		pub_blob = ssh2_userkey_loadpub(cfg.keyfile, &algorithm,
						&pub_blob_len);
		if (pub_blob) {
		    ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
		    ssh2_pkt_addstring(username);
		    ssh2_pkt_addstring("ssh-connection");	/* service requested */
		    ssh2_pkt_addstring("publickey");	/* method */
		    ssh2_pkt_addbool(FALSE);	/* no signature included */
		    ssh2_pkt_addstring(algorithm);
		    ssh2_pkt_addstring_start();
		    ssh2_pkt_addstring_data(pub_blob, pub_blob_len);
		    ssh2_pkt_send();
		    logevent("Offered public key");	/* FIXME */

		    crWaitUntilV(ispkt);
		    if (pktin.type != SSH2_MSG_USERAUTH_PK_OK) {
			gotit = TRUE;
			type = AUTH_TYPE_PUBLICKEY_OFFER_LOUD;
			continue;      /* key refused; give up on it */
		    }

		    logevent("Offer of public key accepted");
		    /*
		     * Actually attempt a serious authentication using
		     * the key.
		     */
		    if (ssh2_userkey_encrypted(cfg.keyfile, &comment)) {
			sprintf(pwprompt,
				"Passphrase for key \"%.100s\": ",
				comment);
			need_pw = TRUE;
		    } else {
			need_pw = FALSE;
		    }
		    c_write_str("Authenticating with public key \"");
		    c_write_str(comment);
		    c_write_str("\"\r\n");
		    method = AUTH_PUBLICKEY_FILE;
		}
	    }

	    if (!method && can_keyb_inter && !tried_keyb_inter) {
		method = AUTH_KEYBOARD_INTERACTIVE;
		type = AUTH_TYPE_KEYBOARD_INTERACTIVE;
		tried_keyb_inter = TRUE;

		ssh_pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
		ssh_pkt_ctx |= SSH2_PKTCTX_KBDINTER;

		ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
		ssh2_pkt_addstring(username);
		ssh2_pkt_addstring("ssh-connection");	/* service requested */
		ssh2_pkt_addstring("keyboard-interactive");	/* method */
		ssh2_pkt_addstring(""); /* lang */
		ssh2_pkt_addstring("");
		ssh2_pkt_send();

		crWaitUntilV(ispkt);
		if (pktin.type != SSH2_MSG_USERAUTH_INFO_REQUEST) {
		    if (pktin.type == SSH2_MSG_USERAUTH_FAILURE)
			gotit = TRUE;
		    logevent("Keyboard-interactive authentication refused");
		    type = AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET;
		    continue;
		}

		kbd_inter_running = TRUE;
		curr_prompt = 0;
	    }

	    if (kbd_inter_running) {
		method = AUTH_KEYBOARD_INTERACTIVE;
		type = AUTH_TYPE_KEYBOARD_INTERACTIVE;
		tried_keyb_inter = TRUE;

		ssh_pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
		ssh_pkt_ctx |= SSH2_PKTCTX_KBDINTER;

		if (curr_prompt == 0) {
		    /*
		     * We've got a fresh USERAUTH_INFO_REQUEST.
		     * Display header data, and start going through
		     * the prompts.
		     */
		    char *name, *inst, *lang;
		    int name_len, inst_len, lang_len;

		    ssh2_pkt_getstring(&name, &name_len);
		    ssh2_pkt_getstring(&inst, &inst_len);
		    ssh2_pkt_getstring(&lang, &lang_len);
		    if (name_len > 0) {
			c_write_untrusted(name, name_len);
			c_write_str("\r\n");
		    }
		    if (inst_len > 0) {
			c_write_untrusted(inst, inst_len);
			c_write_str("\r\n");
		    }
		    num_prompts = ssh2_pkt_getuint32();
		}

		/*
		 * If there are prompts remaining in the packet,
		 * display one and get a response.
		 */
		if (curr_prompt < num_prompts) {
		    char *prompt;
		    int prompt_len;

		    ssh2_pkt_getstring(&prompt, &prompt_len);
		    if (prompt_len > 0) {
			strncpy(pwprompt, prompt, sizeof(pwprompt));
			pwprompt[prompt_len < sizeof(pwprompt) ?
				 prompt_len : sizeof(pwprompt)-1] = '\0';
		    } else {
			strcpy(pwprompt,
			       "<server failed to send prompt>: ");
		    }
		    echo = ssh2_pkt_getbool();
		    need_pw = TRUE;
		} else
		    need_pw = FALSE;
	    }

	    if (!method && can_passwd) {
		method = AUTH_PASSWORD;
		ssh_pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
		ssh_pkt_ctx |= SSH2_PKTCTX_PASSWORD;
		sprintf(pwprompt, "%.90s@%.90s's password: ", username,
			savedhost);
		need_pw = TRUE;
	    }

	    if (need_pw) {
		if (ssh_get_line) {
		    if (!ssh_get_line(pwprompt, password,
				      sizeof(password), TRUE)) {
			/*
			 * get_line failed to get a password (for
			 * example because one was supplied on the
			 * command line which has already failed to
			 * work). Terminate.
			 */
			ssh2_pkt_init(SSH2_MSG_DISCONNECT);
			ssh2_pkt_adduint32(SSH2_DISCONNECT_BY_APPLICATION);
			ssh2_pkt_addstring
			    ("No more passwords available to try");
			ssh2_pkt_addstring("en");	/* language tag */
			ssh2_pkt_send();
			logevent("Unable to authenticate");
			connection_fatal("Unable to authenticate");
			ssh_state = SSH_STATE_CLOSED;
			crReturnV;
		    }
		} else {
		    static int ret;
		    c_write_untrusted(pwprompt, strlen(pwprompt));
		    ssh_send_ok = 1;

		    setup_userpass_input(password, sizeof(password), echo);
		    do {
			crWaitUntilV(!ispkt);
			ret = process_userpass_input(in, inlen);
		    } while (ret == 0);
		    if (ret < 0)
			cleanup_exit(0);
		    c_write_str("\r\n");
		}
	    }

	    if (method == AUTH_PUBLICKEY_FILE) {
		/*
		 * We have our passphrase. Now try the actual authentication.
		 */
		struct ssh2_userkey *key;

		key = ssh2_load_userkey(cfg.keyfile, password);
		if (key == SSH2_WRONG_PASSPHRASE || key == NULL) {
		    if (key == SSH2_WRONG_PASSPHRASE) {
			c_write_str("Wrong passphrase\r\n");
			tried_pubkey_config = FALSE;
		    } else {
			c_write_str("Unable to load private key\r\n");
			tried_pubkey_config = TRUE;
		    }
		    /* Send a spurious AUTH_NONE to return to the top. */
		    ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
		    ssh2_pkt_addstring(username);
		    ssh2_pkt_addstring("ssh-connection");	/* service requested */
		    ssh2_pkt_addstring("none");	/* method */
		    ssh2_pkt_send();
		    type = AUTH_TYPE_NONE;
		} else {
		    unsigned char *pkblob, *sigblob, *sigdata;
		    int pkblob_len, sigblob_len, sigdata_len;

		    /*
		     * We have loaded the private key and the server
		     * has announced that it's willing to accept it.
		     * Hallelujah. Generate a signature and send it.
		     */
		    ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
		    ssh2_pkt_addstring(username);
		    ssh2_pkt_addstring("ssh-connection");	/* service requested */
		    ssh2_pkt_addstring("publickey");	/* method */
		    ssh2_pkt_addbool(TRUE);
		    ssh2_pkt_addstring(key->alg->name);
		    pkblob = key->alg->public_blob(key->data, &pkblob_len);
		    ssh2_pkt_addstring_start();
		    ssh2_pkt_addstring_data(pkblob, pkblob_len);

		    /*
		     * The data to be signed is:
		     *
		     *   string  session-id
		     *
		     * followed by everything so far placed in the
		     * outgoing packet.
		     */
		    sigdata_len = pktout.length - 5 + 4 + 20;
		    sigdata = smalloc(sigdata_len);
		    PUT_32BIT(sigdata, 20);
		    memcpy(sigdata + 4, ssh2_session_id, 20);
		    memcpy(sigdata + 24, pktout.data + 5,
			   pktout.length - 5);
		    sigblob = key->alg->sign(key->data, sigdata,
					     sigdata_len, &sigblob_len);
		    ssh2_add_sigblob(pkblob, pkblob_len,
				     sigblob, sigblob_len);
		    sfree(pkblob);
		    sfree(sigblob);
		    sfree(sigdata);

		    ssh2_pkt_send();
		    type = AUTH_TYPE_PUBLICKEY;
		}
	    } else if (method == AUTH_PASSWORD) {
		/*
		 * We send the password packet lumped tightly together with
		 * an SSH_MSG_IGNORE packet. The IGNORE packet contains a
		 * string long enough to make the total length of the two
		 * packets constant. This should ensure that a passive
		 * listener doing traffic analyis can't work out the length
		 * of the password.
		 *
		 * For this to work, we need an assumption about the
		 * maximum length of the password packet. I think 256 is
		 * pretty conservative. Anyone using a password longer than
		 * that probably doesn't have much to worry about from
		 * people who find out how long their password is!
		 */
		ssh2_pkt_init(SSH2_MSG_USERAUTH_REQUEST);
		ssh2_pkt_addstring(username);
		ssh2_pkt_addstring("ssh-connection");	/* service requested */
		ssh2_pkt_addstring("password");
		ssh2_pkt_addbool(FALSE);
		ssh2_pkt_addstring(password);
		ssh2_pkt_defer();
		/*
		 * We'll include a string that's an exact multiple of the
		 * cipher block size. If the cipher is NULL for some
		 * reason, we don't do this trick at all because we gain
		 * nothing by it.
		 */
		if (cscipher) {
		    int stringlen, i;

		    stringlen = (256 - deferred_len);
		    stringlen += cscipher->blksize - 1;
		    stringlen -= (stringlen % cscipher->blksize);
		    if (cscomp) {
			/*
			 * Temporarily disable actual compression,
			 * so we can guarantee to get this string
			 * exactly the length we want it. The
			 * compression-disabling routine should
			 * return an integer indicating how many
			 * bytes we should adjust our string length
			 * by.
			 */
			stringlen -= cscomp->disable_compression();
		    }
		    ssh2_pkt_init(SSH2_MSG_IGNORE);
		    ssh2_pkt_addstring_start();
		    for (i = 0; i < stringlen; i++) {
			char c = (char) random_byte();
			ssh2_pkt_addstring_data(&c, 1);
		    }
		    ssh2_pkt_defer();
		}
		ssh_pkt_defersend();
		logevent("Sent password");
		type = AUTH_TYPE_PASSWORD;
	    } else if (method == AUTH_KEYBOARD_INTERACTIVE) {
		if (curr_prompt == 0) {
		    ssh2_pkt_init(SSH2_MSG_USERAUTH_INFO_RESPONSE);
		    ssh2_pkt_adduint32(num_prompts);
		}
		if (need_pw) {	       /* only add pw if we just got one! */
		    ssh2_pkt_addstring(password);
		    memset(password, 0, sizeof(password));
		    curr_prompt++;
		}
		if (curr_prompt >= num_prompts) {
		    ssh2_pkt_send();
		} else {
		    /*
		     * If there are prompts remaining, we set
		     * `gotit' so that we won't attempt to get
		     * another packet. Then we go back round the
		     * loop and will end up retrieving another
		     * prompt out of the existing packet. Funky or
		     * what?
		     */
		    gotit = TRUE;
		}
		type = AUTH_TYPE_KEYBOARD_INTERACTIVE;
	    } else {
		c_write_str
		    ("No supported authentication methods left to try!\r\n");
		logevent
		    ("No supported authentications offered. Disconnecting");
		ssh2_pkt_init(SSH2_MSG_DISCONNECT);
		ssh2_pkt_adduint32(SSH2_DISCONNECT_BY_APPLICATION);
		ssh2_pkt_addstring
		    ("No supported authentication methods available");
		ssh2_pkt_addstring("en");	/* language tag */
		ssh2_pkt_send();
		ssh_state = SSH_STATE_CLOSED;
		crReturnV;
	    }
	}
    } while (!we_are_in);

    /*
     * Now we're authenticated for the connection protocol. The
     * connection protocol will automatically have started at this
     * point; there's no need to send SERVICE_REQUEST.
     */

    /*
     * So now create a channel with a session in it.
     */
    ssh_channels = newtree234(ssh_channelcmp);
    mainchan = smalloc(sizeof(struct ssh_channel));
    mainchan->localid = alloc_channel_id();
    ssh2_pkt_init(SSH2_MSG_CHANNEL_OPEN);
    ssh2_pkt_addstring("session");
    ssh2_pkt_adduint32(mainchan->localid);
    mainchan->v.v2.locwindow = OUR_V2_WINSIZE;
    ssh2_pkt_adduint32(mainchan->v.v2.locwindow);      /* our window size */
    ssh2_pkt_adduint32(0x4000UL);      /* our max pkt size */
    ssh2_pkt_send();
    crWaitUntilV(ispkt);
    if (pktin.type != SSH2_MSG_CHANNEL_OPEN_CONFIRMATION) {
	bombout(("Server refused to open a session"));
	crReturnV;
	/* FIXME: error data comes back in FAILURE packet */
    }
    if (ssh2_pkt_getuint32() != mainchan->localid) {
	bombout(("Server's channel confirmation cited wrong channel"));
	crReturnV;
    }
    mainchan->remoteid = ssh2_pkt_getuint32();
    mainchan->type = CHAN_MAINSESSION;
    mainchan->closes = 0;
    mainchan->v.v2.remwindow = ssh2_pkt_getuint32();
    mainchan->v.v2.remmaxpkt = ssh2_pkt_getuint32();
    bufchain_init(&mainchan->v.v2.outbuffer);
    add234(ssh_channels, mainchan);
    logevent("Opened channel for session");

    /*
     * Potentially enable X11 forwarding.
     */
    if (cfg.x11_forward) {
	char proto[20], data[64];
	logevent("Requesting X11 forwarding");
	x11_invent_auth(proto, sizeof(proto), data, sizeof(data));
	ssh2_pkt_init(SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(mainchan->remoteid);
	ssh2_pkt_addstring("x11-req");
	ssh2_pkt_addbool(1);	       /* want reply */
	ssh2_pkt_addbool(0);	       /* many connections */
	ssh2_pkt_addstring(proto);
	ssh2_pkt_addstring(data);
	ssh2_pkt_adduint32(0);	       /* screen number */
	ssh2_pkt_send();

	do {
	    crWaitUntilV(ispkt);
	    if (pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32();
		struct ssh_channel *c;
		c = find234(ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32();
	    }
	} while (pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);

	if (pktin.type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (pktin.type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout(("Unexpected response to X11 forwarding request:"
			 " packet type %d", pktin.type));
		crReturnV;
	    }
	    logevent("X11 forwarding refused");
	} else {
	    logevent("X11 forwarding enabled");
	    ssh_X11_fwd_enabled = TRUE;
	}
    }

    /*
     * Enable port forwardings.
     */
    {
	static char *e;		       /* preserve across crReturn */
	char type;
	int n;
	int sport,dport,sserv,dserv;
	char sports[256], dports[256], host[256];
	char buf[1024];
	struct servent *se;

	ssh_rportfwds = newtree234(ssh_rportcmp_ssh2);
        /* Add port forwardings. */
	e = cfg.portfwd;
	while (*e) {
	    type = *e++;
	    n = 0;
	    while (*e && *e != '\t')
		sports[n++] = *e++;
	    sports[n] = 0;
	    if (*e == '\t')
		e++;
	    n = 0;
	    while (*e && *e != ':')
		host[n++] = *e++;
	    host[n] = 0;
	    if (*e == ':')
		e++;
	    n = 0;
	    while (*e)
		dports[n++] = *e++;
	    dports[n] = 0;
	    e++;
	    dport = atoi(dports);
	    dserv = 0;
	    if (dport == 0) {
		dserv = 1;
		se = getservbyname(dports, NULL);
		if (se != NULL) {
		    dport = ntohs(se->s_port);
		} else {
		    sprintf(buf,
			    "Service lookup failed for destination port \"%s\"",
			    dports);
		    logevent(buf);
		}
	    }
	    sport = atoi(sports);
	    sserv = 0;
	    if (sport == 0) {
		sserv = 1;
		se = getservbyname(sports, NULL);
		if (se != NULL) {
		    sport = ntohs(se->s_port);
		} else {
		    sprintf(buf,
			    "Service lookup failed for source port \"%s\"",
			    sports);
		    logevent(buf);
		}
	    }
	    if (sport && dport) {
		if (type == 'L') {
		    pfd_addforward(host, dport, sport);
		    sprintf(buf, "Local port %.*s%.*s%d%.*s forwarding to"
			    " %s:%.*s%.*s%d%.*s",
			    sserv ? strlen(sports) : 0, sports,
			    sserv, "(", sport, sserv, ")",
			    host,
			    dserv ? strlen(dports) : 0, dports,
			    dserv, "(", dport, dserv, ")");
		    logevent(buf);
		} else {
		    struct ssh_rportfwd *pf;
		    pf = smalloc(sizeof(*pf));
		    strcpy(pf->dhost, host);
		    pf->dport = dport;
		    pf->sport = sport;
		    if (add234(ssh_rportfwds, pf) != pf) {
			sprintf(buf, 
				"Duplicate remote port forwarding to %s:%d",
				host, dport);
			logevent(buf);
			sfree(pf);
		    } else {
			sprintf(buf, "Requesting remote port %.*s%.*s%d%.*s"
				" forward to %s:%.*s%.*s%d%.*s",
			    sserv ? strlen(sports) : 0, sports,
			    sserv, "(", sport, sserv, ")",
			    host,
			    dserv ? strlen(dports) : 0, dports,
			    dserv, "(", dport, dserv, ")");
			logevent(buf);
			ssh2_pkt_init(SSH2_MSG_GLOBAL_REQUEST);
			ssh2_pkt_addstring("tcpip-forward");
			ssh2_pkt_addbool(1);/* want reply */
			if (cfg.rport_acceptall)
			    ssh2_pkt_addstring("0.0.0.0");
			else
			    ssh2_pkt_addstring("127.0.0.1");
			ssh2_pkt_adduint32(sport);
			ssh2_pkt_send();

			do {
			    crWaitUntilV(ispkt);
			    if (pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
				unsigned i = ssh2_pkt_getuint32();
				struct ssh_channel *c;
				c = find234(ssh_channels, &i, ssh_channelfind);
				if (!c)
				    continue;/* nonexistent channel */
				c->v.v2.remwindow += ssh2_pkt_getuint32();
			    }
			} while (pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);

			if (pktin.type != SSH2_MSG_REQUEST_SUCCESS) {
			    if (pktin.type != SSH2_MSG_REQUEST_FAILURE) {
				bombout(("Unexpected response to port "
					 "forwarding request: packet type %d",
					 pktin.type));
				crReturnV;
			    }
			    logevent("Server refused this port forwarding");
			} else {
			    logevent("Remote port forwarding enabled");
			}
		    }
		}
	    }
	}
    }

    /*
     * Potentially enable agent forwarding.
     */
    if (cfg.agentfwd && agent_exists()) {
	logevent("Requesting OpenSSH-style agent forwarding");
	ssh2_pkt_init(SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(mainchan->remoteid);
	ssh2_pkt_addstring("auth-agent-req@openssh.com");
	ssh2_pkt_addbool(1);	       /* want reply */
	ssh2_pkt_send();

	do {
	    crWaitUntilV(ispkt);
	    if (pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32();
		struct ssh_channel *c;
		c = find234(ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32();
	    }
	} while (pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);

	if (pktin.type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (pktin.type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout(("Unexpected response to agent forwarding request:"
			 " packet type %d", pktin.type));
		crReturnV;
	    }
	    logevent("Agent forwarding refused");
	} else {
	    logevent("Agent forwarding enabled");
	    ssh_agentfwd_enabled = TRUE;
	}
    }

    /*
     * Now allocate a pty for the session.
     */
    if (!cfg.nopty) {
	ssh2_pkt_init(SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(mainchan->remoteid);	/* recipient channel */
	ssh2_pkt_addstring("pty-req");
	ssh2_pkt_addbool(1);	       /* want reply */
	ssh2_pkt_addstring(cfg.termtype);
	ssh2_pkt_adduint32(cols);
	ssh2_pkt_adduint32(rows);
	ssh2_pkt_adduint32(0);	       /* pixel width */
	ssh2_pkt_adduint32(0);	       /* pixel height */
	ssh2_pkt_addstring_start();
	ssh2_pkt_addstring_data("\0", 1);	/* TTY_OP_END, no special options */
	ssh2_pkt_send();
	ssh_state = SSH_STATE_INTERMED;

	do {
	    crWaitUntilV(ispkt);
	    if (pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32();
		struct ssh_channel *c;
		c = find234(ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32();
	    }
	} while (pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);

	if (pktin.type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (pktin.type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout(("Unexpected response to pty request:"
			 " packet type %d", pktin.type));
		crReturnV;
	    }
	    c_write_str("Server refused to allocate pty\r\n");
	    ssh_editing = ssh_echoing = 1;
	} else {
	    logevent("Allocated pty");
	}
    } else {
	ssh_editing = ssh_echoing = 1;
    }

    /*
     * Start a shell or a remote command. We may have to attempt
     * this twice if the config data has provided a second choice
     * of command.
     */
    while (1) {
	int subsys;
	char *cmd;

	if (ssh_fallback_cmd) {
	    subsys = cfg.ssh_subsys2;
	    cmd = cfg.remote_cmd_ptr2;
	} else {
	    subsys = cfg.ssh_subsys;
	    cmd = cfg.remote_cmd_ptr;
	}

	ssh2_pkt_init(SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(mainchan->remoteid);	/* recipient channel */
	if (subsys) {
	    ssh2_pkt_addstring("subsystem");
	    ssh2_pkt_addbool(1);	       /* want reply */
	    ssh2_pkt_addstring(cmd);
	} else if (*cmd) {
	    ssh2_pkt_addstring("exec");
	    ssh2_pkt_addbool(1);	       /* want reply */
	    ssh2_pkt_addstring(cmd);
	} else {
	    ssh2_pkt_addstring("shell");
	    ssh2_pkt_addbool(1);	       /* want reply */
	}
	ssh2_pkt_send();
	do {
	    crWaitUntilV(ispkt);
	    if (pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32();
		struct ssh_channel *c;
		c = find234(ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32();
	    }
	} while (pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);
	if (pktin.type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (pktin.type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout(("Unexpected response to shell/command request:"
			 " packet type %d", pktin.type));
		crReturnV;
	    }
	    /*
	     * We failed to start the command. If this is the
	     * fallback command, we really are finished; if it's
	     * not, and if the fallback command exists, try falling
	     * back to it before complaining.
	     */
	    if (!ssh_fallback_cmd && cfg.remote_cmd_ptr2 != NULL) {
		logevent("Primary command failed; attempting fallback");
		ssh_fallback_cmd = TRUE;
		continue;
	    }
	    bombout(("Server refused to start a shell/command"));
	    crReturnV;
	} else {
	    logevent("Started a shell/command");
	}
	break;
    }

    ssh_state = SSH_STATE_SESSION;
    if (size_needed)
	ssh_size();
    if (eof_needed)
	ssh_special(TS_EOF);

    /*
     * Transfer data!
     */
    ldisc_send(NULL, 0, 0);	       /* cause ldisc to notice changes */
    ssh_send_ok = 1;
    while (1) {
	static int try_send;
	crReturnV;
	try_send = FALSE;
	if (ispkt) {
	    if (pktin.type == SSH2_MSG_CHANNEL_DATA ||
		pktin.type == SSH2_MSG_CHANNEL_EXTENDED_DATA) {
		char *data;
		int length;
		unsigned i = ssh2_pkt_getuint32();
		struct ssh_channel *c;
		c = find234(ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		if (pktin.type == SSH2_MSG_CHANNEL_EXTENDED_DATA &&
		    ssh2_pkt_getuint32() != SSH2_EXTENDED_DATA_STDERR)
		    continue;	       /* extended but not stderr */
		ssh2_pkt_getstring(&data, &length);
		if (data) {
		    int bufsize;
		    c->v.v2.locwindow -= length;
		    switch (c->type) {
		      case CHAN_MAINSESSION:
			bufsize =
			    from_backend(pktin.type ==
					 SSH2_MSG_CHANNEL_EXTENDED_DATA,
					 data, length);
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
				int l = min(4 - c->u.a.lensofar, length);
				memcpy(c->u.a.msglen + c->u.a.lensofar,
				       data, l);
				data += l;
				length -= l;
				c->u.a.lensofar += l;
			    }
			    if (c->u.a.lensofar == 4) {
				c->u.a.totallen =
				    4 + GET_32BIT(c->u.a.msglen);
				c->u.a.message = smalloc(c->u.a.totallen);
				memcpy(c->u.a.message, c->u.a.msglen, 4);
			    }
			    if (c->u.a.lensofar >= 4 && length > 0) {
				int l =
				    min(c->u.a.totallen - c->u.a.lensofar,
					length);
				memcpy(c->u.a.message + c->u.a.lensofar,
				       data, l);
				data += l;
				length -= l;
				c->u.a.lensofar += l;
			    }
			    if (c->u.a.lensofar == c->u.a.totallen) {
				void *reply, *sentreply;
				int replylen;
				agent_query(c->u.a.message,
					    c->u.a.totallen, &reply,
					    &replylen);
				if (reply)
				    sentreply = reply;
				else {
				    /* Fake SSH_AGENT_FAILURE. */
				    sentreply = "\0\0\0\1\5";
				    replylen = 5;
				}
				ssh2_add_channel_data(c, sentreply,
						      replylen);
				try_send = TRUE;
				if (reply)
				    sfree(reply);
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
	    } else if (pktin.type == SSH2_MSG_DISCONNECT) {
		ssh_state = SSH_STATE_CLOSED;
		logevent("Received disconnect message");
		crReturnV;
	    } else if (pktin.type == SSH2_MSG_CHANNEL_EOF) {
		unsigned i = ssh2_pkt_getuint32();
		struct ssh_channel *c;

		c = find234(ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */

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
	    } else if (pktin.type == SSH2_MSG_CHANNEL_CLOSE) {
		unsigned i = ssh2_pkt_getuint32();
		struct ssh_channel *c;

		c = find234(ssh_channels, &i, ssh_channelfind);
		if (!c || ((int)c->remoteid) == -1) {
		    bombout(("Received CHANNEL_CLOSE for %s channel %d\n",
			     c ? "half-open" : "nonexistent", i));
		}
		/* Do pre-close processing on the channel. */
		switch (c->type) {
		  case CHAN_MAINSESSION:
		    break;	       /* nothing to see here, move along */
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
		    ssh2_pkt_init(SSH2_MSG_CHANNEL_CLOSE);
		    ssh2_pkt_adduint32(c->remoteid);
		    ssh2_pkt_send();
		}
		del234(ssh_channels, c);
		bufchain_clear(&c->v.v2.outbuffer);
		sfree(c);

		/*
		 * See if that was the last channel left open.
		 */
		if (count234(ssh_channels) == 0) {
#if 0
                    /*
                     * We used to send SSH_MSG_DISCONNECT here,
                     * because I'd believed that _every_ conforming
                     * SSH2 connection had to end with a disconnect
                     * being sent by at least one side; apparently
                     * I was wrong and it's perfectly OK to
                     * unceremoniously slam the connection shut
                     * when you're done, and indeed OpenSSH feels
                     * this is more polite than sending a
                     * DISCONNECT. So now we don't.
                     */
		    logevent("All channels closed. Disconnecting");
		    ssh2_pkt_init(SSH2_MSG_DISCONNECT);
		    ssh2_pkt_adduint32(SSH2_DISCONNECT_BY_APPLICATION);
		    ssh2_pkt_addstring("All open channels closed");
		    ssh2_pkt_addstring("en");	/* language tag */
		    ssh2_pkt_send();
#endif
		    ssh_state = SSH_STATE_CLOSED;
		    crReturnV;
		}
		continue;	       /* remote sends close; ignore (FIXME) */
	    } else if (pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32();
		struct ssh_channel *c;
		c = find234(ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32();
		try_send = TRUE;
	    } else if (pktin.type == SSH2_MSG_CHANNEL_OPEN_CONFIRMATION) {
		unsigned i = ssh2_pkt_getuint32();
		struct ssh_channel *c;
		c = find234(ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		if (c->type != CHAN_SOCKDATA_DORMANT)
		    continue;	       /* dunno why they're confirming this */
		c->remoteid = ssh2_pkt_getuint32();
		c->type = CHAN_SOCKDATA;
		c->v.v2.remwindow = ssh2_pkt_getuint32();
		c->v.v2.remmaxpkt = ssh2_pkt_getuint32();
		if (c->u.pfd.s)
		    pfd_confirm(c->u.pfd.s);
		if (c->closes) {
		    /*
		     * We have a pending close on this channel,
		     * which we decided on before the server acked
		     * the channel open. So now we know the
		     * remoteid, we can close it again.
		     */
		    ssh2_pkt_init(SSH2_MSG_CHANNEL_CLOSE);
		    ssh2_pkt_adduint32(c->remoteid);
		    ssh2_pkt_send();
		}
	    } else if (pktin.type == SSH2_MSG_CHANNEL_OPEN_FAILURE) {
		unsigned i = ssh2_pkt_getuint32();
		struct ssh_channel *c;
		c = find234(ssh_channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		if (c->type != CHAN_SOCKDATA_DORMANT)
		    continue;	       /* dunno why they're failing this */

		logevent("Forwarded connection refused by server");

		pfd_close(c->u.pfd.s);

		del234(ssh_channels, c);
		sfree(c);
	    } else if (pktin.type == SSH2_MSG_CHANNEL_REQUEST) {
 		unsigned localid;
		char *type;
		int typelen, want_reply;
		struct ssh_channel *c;

		localid = ssh2_pkt_getuint32();
		ssh2_pkt_getstring(&type, &typelen);
		want_reply = ssh2_pkt_getbool();

		/*
		 * First, check that the channel exists. Otherwise,
		 * we can instantly disconnect with a rude message.
		 */
		c = find234(ssh_channels, &localid, ssh_channelfind);
		if (!c) {
		    char buf[80];
		    sprintf(buf, "Received channel request for nonexistent"
			    " channel %d", localid);
		    logevent(buf);
		    ssh2_pkt_init(SSH2_MSG_DISCONNECT);
		    ssh2_pkt_adduint32(SSH2_DISCONNECT_BY_APPLICATION);
		    ssh2_pkt_addstring(buf);
		    ssh2_pkt_addstring("en");	/* language tag */
		    ssh2_pkt_send();
		    connection_fatal("%s", buf);
		    ssh_state = SSH_STATE_CLOSED;
		    crReturnV;
		}

		/*
		 * Having got the channel number, we now look at
		 * the request type string to see if it's something
		 * we recognise.
		 */
		if (typelen == 11 && !memcmp(type, "exit-status", 11) &&
		    c == mainchan) {
		    /* We recognise "exit-status" on the primary channel. */
		    char buf[100];
		    ssh_exitcode = ssh2_pkt_getuint32();
		    sprintf(buf, "Server sent command exit status %d",
			    ssh_exitcode);
		    logevent(buf);
		    if (want_reply) {
			ssh2_pkt_init(SSH2_MSG_CHANNEL_SUCCESS);
			ssh2_pkt_adduint32(c->remoteid);
			ssh2_pkt_send();
		    }
		} else {
		    /*
		     * This is a channel request we don't know
		     * about, so we now either ignore the request
		     * or respond with CHANNEL_FAILURE, depending
		     * on want_reply.
		     */
		    if (want_reply) {
			ssh2_pkt_init(SSH2_MSG_CHANNEL_FAILURE);
			ssh2_pkt_adduint32(c->remoteid);
			ssh2_pkt_send();
		    }
		}
	    } else if (pktin.type == SSH2_MSG_GLOBAL_REQUEST) {
		char *type;
		int typelen, want_reply;

		ssh2_pkt_getstring(&type, &typelen);
		want_reply = ssh2_pkt_getbool();

                /*
                 * We currently don't support any global requests
                 * at all, so we either ignore the request or
                 * respond with REQUEST_FAILURE, depending on
                 * want_reply.
                 */
                if (want_reply) {
                    ssh2_pkt_init(SSH2_MSG_REQUEST_FAILURE);
                    ssh2_pkt_send();
		}
	    } else if (pktin.type == SSH2_MSG_CHANNEL_OPEN) {
		char *type;
		int typelen;
		char *error = NULL;
		struct ssh_channel *c;
		unsigned remid, winsize, pktsize;
		ssh2_pkt_getstring(&type, &typelen);
		c = smalloc(sizeof(struct ssh_channel));

		remid = ssh2_pkt_getuint32();
		winsize = ssh2_pkt_getuint32();
		pktsize = ssh2_pkt_getuint32();

		if (typelen == 3 && !memcmp(type, "x11", 3)) {
		    if (!ssh_X11_fwd_enabled)
			error = "X11 forwarding is not enabled";
		    else if (x11_init(&c->u.x11.s, cfg.x11_display, c) !=
			     NULL) {
			error = "Unable to open an X11 connection";
		    } else {
			c->type = CHAN_X11;
		    }
		} else if (typelen == 15 &&
			   !memcmp(type, "forwarded-tcpip", 15)) {
		    struct ssh_rportfwd pf, *realpf;
		    char *dummy;
		    int dummylen;
		    ssh2_pkt_getstring(&dummy, &dummylen);/* skip address */
		    pf.sport = ssh2_pkt_getuint32();
		    realpf = find234(ssh_rportfwds, &pf, NULL);
		    if (realpf == NULL) {
			error = "Remote port is not recognised";
		    } else {
			char *e = pfd_newconnect(&c->u.pfd.s, realpf->dhost,
						 realpf->dport, c);
			char buf[1024];
			sprintf(buf, "Received remote port open request for %s:%d",
				realpf->dhost, realpf->dport);
			logevent(buf);
			if (e != NULL) {
			    sprintf(buf, "Port open failed: %s", e);
			    logevent(buf);
			    error = "Port open failed";
			} else {
			    logevent("Forwarded port opened successfully");
			    c->type = CHAN_SOCKDATA;
			}
		    }
		} else if (typelen == 22 &&
			   !memcmp(type, "auth-agent@openssh.com", 3)) {
		    if (!ssh_agentfwd_enabled)
			error = "Agent forwarding is not enabled";
		    else {
			c->type = CHAN_AGENT;	/* identify channel type */
			c->u.a.lensofar = 0;
		    }
		} else {
		    error = "Unsupported channel type requested";
		}

		c->remoteid = remid;
		if (error) {
		    ssh2_pkt_init(SSH2_MSG_CHANNEL_OPEN_FAILURE);
		    ssh2_pkt_adduint32(c->remoteid);
		    ssh2_pkt_adduint32(SSH2_OPEN_CONNECT_FAILED);
		    ssh2_pkt_addstring(error);
		    ssh2_pkt_addstring("en");	/* language tag */
		    ssh2_pkt_send();
		    sfree(c);
		} else {
		    c->localid = alloc_channel_id();
		    c->closes = 0;
		    c->v.v2.locwindow = OUR_V2_WINSIZE;
		    c->v.v2.remwindow = winsize;
		    c->v.v2.remmaxpkt = pktsize;
		    bufchain_init(&c->v.v2.outbuffer);
		    add234(ssh_channels, c);
		    ssh2_pkt_init(SSH2_MSG_CHANNEL_OPEN_CONFIRMATION);
		    ssh2_pkt_adduint32(c->remoteid);
		    ssh2_pkt_adduint32(c->localid);
		    ssh2_pkt_adduint32(c->v.v2.locwindow);
		    ssh2_pkt_adduint32(0x4000UL);	/* our max pkt size */
		    ssh2_pkt_send();
		}
	    } else {
		bombout(("Strange packet received: type %d", pktin.type));
		crReturnV;
	    }
	} else {
	    /*
	     * We have spare data. Add it to the channel buffer.
	     */
	    ssh2_add_channel_data(mainchan, in, inlen);
	    try_send = TRUE;
	}
	if (try_send) {
	    int i;
	    struct ssh_channel *c;
	    /*
	     * Try to send data on all channels if we can.
	     */
	    for (i = 0; NULL != (c = index234(ssh_channels, i)); i++) {
		int bufsize = ssh2_try_send(c);
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
	}
    }

    crFinishV;
}

/*
 * Handle the top-level SSH2 protocol.
 */
static void ssh2_protocol(unsigned char *in, int inlen, int ispkt)
{
    if (do_ssh2_transport(in, inlen, ispkt) == 0)
	return;
    do_ssh2_authconn(in, inlen, ispkt);
}

/*
 * Called to set up the connection.
 *
 * Returns an error message, or NULL on success.
 */
static char *ssh_init(char *host, int port, char **realhost, int nodelay)
{
    char *p;

#ifdef MSCRYPTOAPI
    if (crypto_startup() == 0)
	return "Microsoft high encryption pack not installed!";
#endif

    ssh_send_ok = 0;
    ssh_editing = 0;
    ssh_echoing = 0;
    ssh1_throttle_count = 0;
    ssh_overall_bufsize = 0;
    ssh_fallback_cmd = 0;

    p = connect_to_host(host, port, realhost, nodelay);
    if (p != NULL)
	return p;

    return NULL;
}

/*
 * Called to send data down the Telnet connection.
 */
static int ssh_send(char *buf, int len)
{
    if (s == NULL || ssh_protocol == NULL)
	return 0;

    ssh_protocol(buf, len, 0);

    return ssh_sendbuffer();
}

/*
 * Called to query the current amount of buffered stdin data.
 */
static int ssh_sendbuffer(void)
{
    int override_value;

    if (s == NULL || ssh_protocol == NULL)
	return 0;

    /*
     * If the SSH socket itself has backed up, add the total backup
     * size on that to any individual buffer on the stdin channel.
     */
    override_value = 0;
    if (ssh_throttled_all)
	override_value = ssh_overall_bufsize;

    if (ssh_version == 1) {
	return override_value;
    } else if (ssh_version == 2) {
	if (!mainchan || mainchan->closes > 0)
	    return override_value;
	else
	    return override_value + bufchain_size(&mainchan->v.v2.outbuffer);
    }

    return 0;
}

/*
 * Called to set the size of the window from SSH's POV.
 */
static void ssh_size(void)
{
    switch (ssh_state) {
      case SSH_STATE_BEFORE_SIZE:
      case SSH_STATE_PREPACKET:
      case SSH_STATE_CLOSED:
	break;			       /* do nothing */
      case SSH_STATE_INTERMED:
	size_needed = TRUE;	       /* buffer for later */
	break;
      case SSH_STATE_SESSION:
	if (!cfg.nopty) {
	    if (ssh_version == 1) {
		send_packet(SSH1_CMSG_WINDOW_SIZE,
			    PKT_INT, rows, PKT_INT, cols,
			    PKT_INT, 0, PKT_INT, 0, PKT_END);
	    } else {
		ssh2_pkt_init(SSH2_MSG_CHANNEL_REQUEST);
		ssh2_pkt_adduint32(mainchan->remoteid);
		ssh2_pkt_addstring("window-change");
		ssh2_pkt_addbool(0);
		ssh2_pkt_adduint32(cols);
		ssh2_pkt_adduint32(rows);
		ssh2_pkt_adduint32(0);
		ssh2_pkt_adduint32(0);
		ssh2_pkt_send();
	    }
	}
	break;
    }
}

/*
 * Send Telnet special codes. TS_EOF is useful for `plink', so you
 * can send an EOF and collect resulting output (e.g. `plink
 * hostname sort').
 */
static void ssh_special(Telnet_Special code)
{
    if (code == TS_EOF) {
	if (ssh_state != SSH_STATE_SESSION) {
	    /*
	     * Buffer the EOF in case we are pre-SESSION, so we can
	     * send it as soon as we reach SESSION.
	     */
	    if (code == TS_EOF)
		eof_needed = TRUE;
	    return;
	}
	if (ssh_version == 1) {
	    send_packet(SSH1_CMSG_EOF, PKT_END);
	} else {
	    ssh2_pkt_init(SSH2_MSG_CHANNEL_EOF);
	    ssh2_pkt_adduint32(mainchan->remoteid);
	    ssh2_pkt_send();
	}
	logevent("Sent EOF message");
    } else if (code == TS_PING) {
	if (ssh_state == SSH_STATE_CLOSED
	    || ssh_state == SSH_STATE_PREPACKET) return;
	if (ssh_version == 1) {
	    if (!(ssh_remote_bugs & BUG_CHOKES_ON_SSH1_IGNORE))
		send_packet(SSH1_MSG_IGNORE, PKT_STR, "", PKT_END);
	} else {
	    ssh2_pkt_init(SSH2_MSG_IGNORE);
	    ssh2_pkt_addstring_start();
	    ssh2_pkt_send();
	}
    } else {
	/* do nothing */
    }
}

void *new_sock_channel(Socket s)
{
    struct ssh_channel *c;
    c = smalloc(sizeof(struct ssh_channel));

    if (c) {
	c->remoteid = -1;	       /* to be set when open confirmed */
	c->localid = alloc_channel_id();
	c->closes = 0;
	c->type = CHAN_SOCKDATA_DORMANT;/* identify channel type */
	c->u.pfd.s = s;
	bufchain_init(&c->v.v2.outbuffer);
	add234(ssh_channels, c);
    }
    return c;
}

/*
 * This is called when stdout/stderr (the entity to which
 * from_backend sends data) manages to clear some backlog.
 */
void ssh_unthrottle(int bufsize)
{
    if (ssh_version == 1) {
	if (ssh1_stdout_throttling && bufsize < SSH1_BUFFER_LIMIT) {
	    ssh1_stdout_throttling = 0;
	    ssh1_throttle(-1);
	}
    } else {
	if (mainchan && mainchan->closes == 0)
	    ssh2_set_window(mainchan, OUR_V2_WINSIZE - bufsize);
    }
}

void ssh_send_port_open(void *channel, char *hostname, int port, char *org)
{
    struct ssh_channel *c = (struct ssh_channel *)channel;
    char buf[1024];

    sprintf(buf, "Opening forwarded connection to %.512s:%d", hostname, port);
    logevent(buf);

    if (ssh_version == 1) {
	send_packet(SSH1_MSG_PORT_OPEN,
		    PKT_INT, c->localid,
		    PKT_STR, hostname,
		    PKT_INT, port,
		    //PKT_STR, <org:orgport>,
		    PKT_END);
    } else {
	ssh2_pkt_init(SSH2_MSG_CHANNEL_OPEN);
	ssh2_pkt_addstring("direct-tcpip");
	ssh2_pkt_adduint32(c->localid);
	c->v.v2.locwindow = OUR_V2_WINSIZE;
	ssh2_pkt_adduint32(c->v.v2.locwindow);/* our window size */
	ssh2_pkt_adduint32(0x4000UL);      /* our max pkt size */
	ssh2_pkt_addstring(hostname);
	ssh2_pkt_adduint32(port);
	/*
	 * We make up values for the originator data; partly it's
	 * too much hassle to keep track, and partly I'm not
	 * convinced the server should be told details like that
	 * about my local network configuration.
	 */
	ssh2_pkt_addstring("client-side-connection");
	ssh2_pkt_adduint32(0);
	ssh2_pkt_send();
    }
}


static Socket ssh_socket(void)
{
    return s;
}

static int ssh_sendok(void)
{
    return ssh_send_ok;
}

static int ssh_ldisc(int option)
{
    if (option == LD_ECHO)
	return ssh_echoing;
    if (option == LD_EDIT)
	return ssh_editing;
    return FALSE;
}

static int ssh_return_exitcode(void)
{
    return ssh_exitcode;
}

Backend ssh_backend = {
    ssh_init,
    ssh_send,
    ssh_sendbuffer,
    ssh_size,
    ssh_special,
    ssh_socket,
    ssh_return_exitcode,
    ssh_sendok,
    ssh_ldisc,
    ssh_unthrottle,
    22
};
