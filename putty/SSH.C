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

typedef struct ssh_tag *Ssh;

static void ssh2_pkt_init(Ssh, int pkt_type);
static void ssh2_pkt_addbool(Ssh, unsigned char value);
static void ssh2_pkt_adduint32(Ssh, unsigned long value);
static void ssh2_pkt_addstring_start(Ssh);
static void ssh2_pkt_addstring_str(Ssh, char *data);
static void ssh2_pkt_addstring_data(Ssh, char *data, int len);
static void ssh2_pkt_addstring(Ssh, char *data);
static unsigned char *ssh2_mpint_fmt(Bignum b, int *len);
static void ssh2_pkt_addmp(Ssh, Bignum b);
static int ssh2_pkt_construct(Ssh);
static void ssh2_pkt_send(Ssh);
static int do_ssh1_login(Ssh ssh, unsigned char *in, int inlen, int ispkt);
static void do_ssh2_authconn(Ssh ssh, unsigned char *in, int inlen, int ispkt);

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

static void ssh1_protocol(Ssh ssh, unsigned char *in, int inlen, int ispkt);
static void ssh2_protocol(Ssh ssh, unsigned char *in, int inlen, int ispkt);
static void ssh_size(void *handle, int width, int height);
static void ssh_special(void *handle, Telnet_Special);
static int ssh2_try_send(struct ssh_channel *c);
static void ssh2_add_channel_data(struct ssh_channel *c, char *buf, int len);
static void ssh_throttle_all(Ssh ssh, int enable, int bufsize);
static void ssh2_set_window(struct ssh_channel *c, unsigned newwin);
static int ssh_sendbuffer(void *handle);
static void ssh_do_close(Ssh ssh);

struct rdpkt1_state_tag {
    long len, pad, biglen, to_read;
    unsigned long realcrc, gotcrc;
    unsigned char *p;
    int i;
    int chunk;
};

struct rdpkt2_state_tag {
    long len, pad, payload, packetlen, maclen;
    int i;
    int cipherblk;
    unsigned long incoming_sequence;
};

struct ssh_tag {
    const struct plug_function_table *fn;
    /* the above field _must_ be first in the structure */

    SHA_State exhash, exhashbase;

    Socket s;

    void *ldisc;
    void *logctx;

    unsigned char session_key[32];
    int v1_compressing;
    int v1_remote_protoflags;
    int v1_local_protoflags;
    int agentfwd_enabled;
    int X11_fwd_enabled;
    int remote_bugs;
    const struct ssh_cipher *cipher;
    void *v1_cipher_ctx;
    void *crcda_ctx;
    const struct ssh2_cipher *cscipher, *sccipher;
    void *cs_cipher_ctx, *sc_cipher_ctx;
    const struct ssh_mac *csmac, *scmac;
    void *cs_mac_ctx, *sc_mac_ctx;
    const struct ssh_compress *cscomp, *sccomp;
    void *cs_comp_ctx, *sc_comp_ctx;
    const struct ssh_kex *kex;
    const struct ssh_signkey *hostkey;
    unsigned char v2_session_id[20];
    void *kex_ctx;

    char *savedhost;
    int savedport;
    int send_ok;
    int echoing, editing;

    void *frontend;

    int term_width, term_height;

    tree234 *channels;		       /* indexed by local id */
    struct ssh_channel *mainchan;      /* primary session channel */
    int exitcode;

    tree234 *rportfwds;

    enum {
	SSH_STATE_PREPACKET,
	SSH_STATE_BEFORE_SIZE,
	SSH_STATE_INTERMED,
	SSH_STATE_SESSION,
	SSH_STATE_CLOSED
    } state;

    int size_needed, eof_needed;

    struct Packet pktin;
    struct Packet pktout;
    unsigned char *deferred_send_data;
    int deferred_len, deferred_size;

    /*
     * Gross hack: pscp will try to start SFTP but fall back to
     * scp1 if that fails. This variable is the means by which
     * scp.c can reach into the SSH code and find out which one it
     * got.
     */
    int fallback_cmd;

    /*
     * Used for username and password input.
     */
    char *userpass_input_buffer;
    int userpass_input_buflen;
    int userpass_input_bufpos;
    int userpass_input_echo;

    char *portfwd_strptr;
    int pkt_ctx;

    void *x11auth;

    int version;
    int v1_throttle_count;
    int overall_bufsize;
    int throttled_all;
    int v1_stdout_throttling;
    int v2_outgoing_sequence;

    int ssh1_rdpkt_crstate;
    int ssh2_rdpkt_crstate;
    int do_ssh_init_crstate;
    int ssh_gotdata_crstate;
    int ssh1_protocol_crstate;
    int do_ssh1_login_crstate;
    int do_ssh2_transport_crstate;
    int do_ssh2_authconn_crstate;

    void *do_ssh_init_state;
    void *do_ssh1_login_state;
    void *do_ssh2_transport_state;
    void *do_ssh2_authconn_state;

    struct rdpkt1_state_tag rdpkt1_state;
    struct rdpkt2_state_tag rdpkt2_state;

    void (*protocol) (Ssh ssh, unsigned char *in, int inlen, int ispkt);
    int (*s_rdpkt) (Ssh ssh, unsigned char **data, int *datalen);

    /*
     * We maintain a full _copy_ of a Config structure here, not
     * merely a pointer to it. That way, when we're passed a new
     * one for reconfiguration, we can check the differences and
     * potentially reconfigure port forwardings etc in mid-session.
     */
    Config cfg;

    /*
     * Used to transfer data back from async agent callbacks.
     */
    void *agent_response;
    int agent_response_len;
};

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

#define bombout(msg) \
    do { \
        char *text = dupprintf msg; \
	ssh_do_close(ssh); \
        logevent(text); \
        connection_fatal(ssh->frontend, "%s", text); \
        sfree(text); \
    } while (0)

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

static void c_write(Ssh ssh, const char *buf, int len)
{
    if ((flags & FLAG_STDERR)) {
	int i;
	for (i = 0; i < len; i++)
	    if (buf[i] != '\r')
		fputc(buf[i], stderr);
	return;
    }
    from_backend(ssh->frontend, 1, buf, len);
}

static void c_write_untrusted(Ssh ssh, const char *buf, int len)
{
    int i;
    for (i = 0; i < len; i++) {
	if (buf[i] == '\n')
	    c_write(ssh, "\r\n", 2);
	else if ((buf[i] & 0x60) || (buf[i] == '\r'))
	    c_write(ssh, buf + i, 1);
    }
}

static void c_write_str(Ssh ssh, const char *buf)
{
    c_write(ssh, buf, strlen(buf));
}

/*
 * Collect incoming data in the incoming packet buffer.
 * Decipher and verify the packet when it is completely read.
 * Drop SSH1_MSG_DEBUG and SSH1_MSG_IGNORE packets.
 * Update the *data and *datalen variables.
 * Return the additional nr of bytes needed, or 0 when
 * a complete packet is available.
 */
static int ssh1_rdpkt(Ssh ssh, unsigned char **data, int *datalen)
{
    struct rdpkt1_state_tag *st = &ssh->rdpkt1_state;

    crBegin(ssh->ssh1_rdpkt_crstate);

  next_packet:

    ssh->pktin.type = 0;
    ssh->pktin.length = 0;

    for (st->i = st->len = 0; st->i < 4; st->i++) {
	while ((*datalen) == 0)
	    crReturn(4 - st->i);
	st->len = (st->len << 8) + **data;
	(*data)++, (*datalen)--;
    }

    st->pad = 8 - (st->len % 8);
    st->biglen = st->len + st->pad;
    ssh->pktin.length = st->len - 5;

    if (ssh->pktin.maxlen < st->biglen) {
	ssh->pktin.maxlen = st->biglen;
	ssh->pktin.data = sresize(ssh->pktin.data, st->biglen + APIEXTRA,
				  unsigned char);
    }

    st->to_read = st->biglen;
    st->p = ssh->pktin.data;
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

    if (ssh->cipher && detect_attack(ssh->crcda_ctx, ssh->pktin.data,
				     st->biglen, NULL)) {
        bombout(("Network attack (CRC compensation) detected!"));
        crStop(0);
    }

    if (ssh->cipher)
	ssh->cipher->decrypt(ssh->v1_cipher_ctx, ssh->pktin.data, st->biglen);

    st->realcrc = crc32_compute(ssh->pktin.data, st->biglen - 4);
    st->gotcrc = GET_32BIT(ssh->pktin.data + st->biglen - 4);
    if (st->gotcrc != st->realcrc) {
	bombout(("Incorrect CRC received on packet"));
	crStop(0);
    }

    ssh->pktin.body = ssh->pktin.data + st->pad + 1;

    if (ssh->v1_compressing) {
	unsigned char *decompblk;
	int decomplen;
	if (!zlib_decompress_block(ssh->sc_comp_ctx,
				   ssh->pktin.body - 1, ssh->pktin.length + 1,
				   &decompblk, &decomplen)) {
	    bombout(("Zlib decompression encountered invalid data"));
	    crStop(0);
	}

	if (ssh->pktin.maxlen < st->pad + decomplen) {
	    ssh->pktin.maxlen = st->pad + decomplen;
	    ssh->pktin.data = sresize(ssh->pktin.data,
				      ssh->pktin.maxlen + APIEXTRA,
				      unsigned char);
	    ssh->pktin.body = ssh->pktin.data + st->pad + 1;
	}

	memcpy(ssh->pktin.body - 1, decompblk, decomplen);
	sfree(decompblk);
	ssh->pktin.length = decomplen - 1;
    }

    ssh->pktin.type = ssh->pktin.body[-1];

    if (ssh->logctx)
	log_packet(ssh->logctx,
		   PKT_INCOMING, ssh->pktin.type,
		   ssh1_pkt_type(ssh->pktin.type),
		   ssh->pktin.body, ssh->pktin.length);

    if (ssh->pktin.type == SSH1_SMSG_STDOUT_DATA ||
	ssh->pktin.type == SSH1_SMSG_STDERR_DATA ||
	ssh->pktin.type == SSH1_MSG_DEBUG ||
	ssh->pktin.type == SSH1_SMSG_AUTH_TIS_CHALLENGE ||
	ssh->pktin.type == SSH1_SMSG_AUTH_CCARD_CHALLENGE) {
	long stringlen = GET_32BIT(ssh->pktin.body);
	if (stringlen + 4 != ssh->pktin.length) {
	    bombout(("Received data packet with bogus string length"));
	    crStop(0);
	}
    }

    if (ssh->pktin.type == SSH1_MSG_DEBUG) {
	/* log debug message */
	char buf[512];
	int stringlen = GET_32BIT(ssh->pktin.body);
	strcpy(buf, "Remote debug message: ");
	if (stringlen > 480)
	    stringlen = 480;
	memcpy(buf + 8, ssh->pktin.body + 4, stringlen);
	buf[8 + stringlen] = '\0';
	logevent(buf);
	goto next_packet;
    } else if (ssh->pktin.type == SSH1_MSG_IGNORE) {
	/* do nothing */
	goto next_packet;
    }

    if (ssh->pktin.type == SSH1_MSG_DISCONNECT) {
	/* log reason code in disconnect message */
	char buf[256];
	unsigned msglen = GET_32BIT(ssh->pktin.body);
	unsigned nowlen;
	strcpy(buf, "Remote sent disconnect: ");
	nowlen = strlen(buf);
	if (msglen > sizeof(buf) - nowlen - 1)
	    msglen = sizeof(buf) - nowlen - 1;
	memcpy(buf + nowlen, ssh->pktin.body + 4, msglen);
	buf[nowlen + msglen] = '\0';
	/* logevent(buf); (this is now done within the bombout macro) */
	bombout(("Server sent disconnect message:\n\"%s\"", buf+nowlen));
	crStop(0);
    }

    crFinish(0);
}

static int ssh2_rdpkt(Ssh ssh, unsigned char **data, int *datalen)
{
    struct rdpkt2_state_tag *st = &ssh->rdpkt2_state;

    crBegin(ssh->ssh2_rdpkt_crstate);

  next_packet:
    ssh->pktin.type = 0;
    ssh->pktin.length = 0;
    if (ssh->sccipher)
	st->cipherblk = ssh->sccipher->blksize;
    else
	st->cipherblk = 8;
    if (st->cipherblk < 8)
	st->cipherblk = 8;

    if (ssh->pktin.maxlen < st->cipherblk) {
	ssh->pktin.maxlen = st->cipherblk;
	ssh->pktin.data = sresize(ssh->pktin.data, st->cipherblk + APIEXTRA,
				  unsigned char);
    }

    /*
     * Acquire and decrypt the first block of the packet. This will
     * contain the length and padding details.
     */
    for (st->i = st->len = 0; st->i < st->cipherblk; st->i++) {
	while ((*datalen) == 0)
	    crReturn(st->cipherblk - st->i);
	ssh->pktin.data[st->i] = *(*data)++;
	(*datalen)--;
    }

    if (ssh->sccipher)
	ssh->sccipher->decrypt(ssh->sc_cipher_ctx,
			       ssh->pktin.data, st->cipherblk);

    /*
     * Now get the length and padding figures.
     */
    st->len = GET_32BIT(ssh->pktin.data);
    st->pad = ssh->pktin.data[4];

    /*
     * _Completely_ silly lengths should be stomped on before they
     * do us any more damage.
     */
    if (st->len < 0 || st->pad < 0 || st->len + st->pad < 0) {
	bombout(("Incoming packet was garbled on decryption"));
	crStop(0);
    }

    /*
     * This enables us to deduce the payload length.
     */
    st->payload = st->len - st->pad - 1;

    ssh->pktin.length = st->payload + 5;

    /*
     * So now we can work out the total packet length.
     */
    st->packetlen = st->len + 4;
    st->maclen = ssh->scmac ? ssh->scmac->len : 0;

    /*
     * Adjust memory allocation if packet is too big.
     */
    if (ssh->pktin.maxlen < st->packetlen + st->maclen) {
	ssh->pktin.maxlen = st->packetlen + st->maclen;
	ssh->pktin.data = sresize(ssh->pktin.data,
				  ssh->pktin.maxlen + APIEXTRA,
				  unsigned char);
    }

    /*
     * Read and decrypt the remainder of the packet.
     */
    for (st->i = st->cipherblk; st->i < st->packetlen + st->maclen;
	 st->i++) {
	while ((*datalen) == 0)
	    crReturn(st->packetlen + st->maclen - st->i);
	ssh->pktin.data[st->i] = *(*data)++;
	(*datalen)--;
    }
    /* Decrypt everything _except_ the MAC. */
    if (ssh->sccipher)
	ssh->sccipher->decrypt(ssh->sc_cipher_ctx,
			       ssh->pktin.data + st->cipherblk,
			       st->packetlen - st->cipherblk);

    /*
     * Check the MAC.
     */
    if (ssh->scmac
	&& !ssh->scmac->verify(ssh->sc_mac_ctx, ssh->pktin.data, st->len + 4,
			       st->incoming_sequence)) {
	bombout(("Incorrect MAC received on packet"));
	crStop(0);
    }
    st->incoming_sequence++;	       /* whether or not we MACed */

    /*
     * Decompress packet payload.
     */
    {
	unsigned char *newpayload;
	int newlen;
	if (ssh->sccomp &&
	    ssh->sccomp->decompress(ssh->sc_comp_ctx,
				    ssh->pktin.data + 5, ssh->pktin.length - 5,
				    &newpayload, &newlen)) {
	    if (ssh->pktin.maxlen < newlen + 5) {
		ssh->pktin.maxlen = newlen + 5;
		ssh->pktin.data = sresize(ssh->pktin.data,
					  ssh->pktin.maxlen + APIEXTRA,
					  unsigned char);
	    }
	    ssh->pktin.length = 5 + newlen;
	    memcpy(ssh->pktin.data + 5, newpayload, newlen);
	    sfree(newpayload);
	}
    }

    ssh->pktin.savedpos = 6;
    ssh->pktin.type = ssh->pktin.data[5];

    if (ssh->logctx)
	log_packet(ssh->logctx, PKT_INCOMING, ssh->pktin.type,
		   ssh2_pkt_type(ssh->pkt_ctx, ssh->pktin.type),
		   ssh->pktin.data+6, ssh->pktin.length-6);

    switch (ssh->pktin.type) {
        /*
         * These packets we must handle instantly.
         */
      case SSH2_MSG_DISCONNECT:
        {
            /* log reason code in disconnect message */
            char *buf;
	    int nowlen;
            int reason = GET_32BIT(ssh->pktin.data + 6);
            unsigned msglen = GET_32BIT(ssh->pktin.data + 10);

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
			    &nowlen, msglen, ssh->pktin.data + 14);
            logevent(buf);
            bombout(("Server sent disconnect message\ntype %d (%s):\n\"%s\"",
                     reason,
                     (reason > 0 && reason < lenof(ssh2_disconnect_reasons)) ?
                     ssh2_disconnect_reasons[reason] : "unknown",
                     buf+nowlen));
	    sfree(buf);
            crStop(0);
        }
        break;
      case SSH2_MSG_IGNORE:
	goto next_packet;
      case SSH2_MSG_DEBUG:
	{
	    /* log the debug message */
	    char buf[512];
	    /* int display = ssh->pktin.body[6]; */
	    int stringlen = GET_32BIT(ssh->pktin.data+7);
	    int prefix;
	    strcpy(buf, "Remote debug message: ");
	    prefix = strlen(buf);
	    if (stringlen > (int)(sizeof(buf)-prefix-1))
		stringlen = sizeof(buf)-prefix-1;
	    memcpy(buf + prefix, ssh->pktin.data + 11, stringlen);
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
	ssh2_pkt_init(ssh, SSH2_MSG_UNIMPLEMENTED);
	ssh2_pkt_adduint32(ssh, st->incoming_sequence - 1);
	ssh2_pkt_send(ssh);
        break;
    }

    crFinish(0);
}

static void ssh1_pktout_size(Ssh ssh, int len)
{
    int pad, biglen;

    len += 5;			       /* type and CRC */
    pad = 8 - (len % 8);
    biglen = len + pad;

    ssh->pktout.length = len - 5;
    if (ssh->pktout.maxlen < biglen) {
	ssh->pktout.maxlen = biglen;
#ifdef MSCRYPTOAPI
	/* Allocate enough buffer space for extra block
	 * for MS CryptEncrypt() */
	ssh->pktout.data = sresize(ssh->pktout.data, biglen + 12,
				   unsigned char);
#else
	ssh->pktout.data = sresize(ssh->pktout.data, biglen + 4,
				   unsigned char);
#endif
    }
    ssh->pktout.body = ssh->pktout.data + 4 + pad + 1;
}

static void s_wrpkt_start(Ssh ssh, int type, int len)
{
    ssh1_pktout_size(ssh, len);
    ssh->pktout.type = type;
}

static int s_wrpkt_prepare(Ssh ssh)
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

    ssh->pktout.body[-1] = ssh->pktout.type;

    if (ssh->logctx)
	log_packet(ssh->logctx, PKT_OUTGOING, ssh->pktout.type,
		   ssh1_pkt_type(ssh->pktout.type),
		   ssh->pktout.body, ssh->pktout.length);

    if (ssh->v1_compressing) {
	unsigned char *compblk;
	int complen;
	zlib_compress_block(ssh->cs_comp_ctx,
			    ssh->pktout.body - 1, ssh->pktout.length + 1,
			    &compblk, &complen);
	ssh1_pktout_size(ssh, complen - 1);
	memcpy(ssh->pktout.body - 1, compblk, complen);
	sfree(compblk);
    }

    len = ssh->pktout.length + 5;	       /* type and CRC */
    pad = 8 - (len % 8);
    biglen = len + pad;

    for (i = 0; i < pad; i++)
	ssh->pktout.data[i + 4] = random_byte();
    crc = crc32_compute(ssh->pktout.data + 4, biglen - 4);
    PUT_32BIT(ssh->pktout.data + biglen, crc);
    PUT_32BIT(ssh->pktout.data, len);

    if (ssh->cipher)
	ssh->cipher->encrypt(ssh->v1_cipher_ctx, ssh->pktout.data + 4, biglen);

    return biglen + 4;
}

static void s_wrpkt(Ssh ssh)
{
    int len, backlog;
    len = s_wrpkt_prepare(ssh);
    backlog = sk_write(ssh->s, (char *)ssh->pktout.data, len);
    if (backlog > SSH_MAX_BACKLOG)
	ssh_throttle_all(ssh, 1, backlog);
}

static void s_wrpkt_defer(Ssh ssh)
{
    int len;
    len = s_wrpkt_prepare(ssh);
    if (ssh->deferred_len + len > ssh->deferred_size) {
	ssh->deferred_size = ssh->deferred_len + len + 128;
	ssh->deferred_send_data = sresize(ssh->deferred_send_data,
					  ssh->deferred_size,
					  unsigned char);
    }
    memcpy(ssh->deferred_send_data + ssh->deferred_len, ssh->pktout.data, len);
    ssh->deferred_len += len;
}

/*
 * Construct a packet with the specified contents.
 */
static void construct_packet(Ssh ssh, int pkttype, va_list ap1, va_list ap2)
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
	  default:
	    assert(0);
	}
    }

    s_wrpkt_start(ssh, pkttype, pktlen);
    p = ssh->pktout.body;

    while ((argtype = va_arg(ap2, int)) != PKT_END) {
	switch (argtype) {
	  case PKT_INT:
	    argint = va_arg(ap2, int);
	    PUT_32BIT(p, argint);
	    p += 4;
	    break;
	  case PKT_CHAR:
	    argchar = (unsigned char) va_arg(ap2, int);
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
	    arglen = strlen((char *)argp);
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

static void send_packet(Ssh ssh, int pkttype, ...)
{
    va_list ap1, ap2;
    va_start(ap1, pkttype);
    va_start(ap2, pkttype);
    construct_packet(ssh, pkttype, ap1, ap2);
    s_wrpkt(ssh);
}

static void defer_packet(Ssh ssh, int pkttype, ...)
{
    va_list ap1, ap2;
    va_start(ap1, pkttype);
    va_start(ap2, pkttype);
    construct_packet(ssh, pkttype, ap1, ap2);
    s_wrpkt_defer(ssh);
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
static void ssh2_pkt_ensure(Ssh ssh, int length)
{
    if (ssh->pktout.maxlen < length) {
	ssh->pktout.maxlen = length + 256;
	ssh->pktout.data = sresize(ssh->pktout.data,
				   ssh->pktout.maxlen + APIEXTRA,
				   unsigned char);
	if (!ssh->pktout.data)
	    fatalbox("Out of memory");
    }
}
static void ssh2_pkt_adddata(Ssh ssh, void *data, int len)
{
    ssh->pktout.length += len;
    ssh2_pkt_ensure(ssh, ssh->pktout.length);
    memcpy(ssh->pktout.data + ssh->pktout.length - len, data, len);
}
static void ssh2_pkt_addbyte(Ssh ssh, unsigned char byte)
{
    ssh2_pkt_adddata(ssh, &byte, 1);
}
static void ssh2_pkt_init(Ssh ssh, int pkt_type)
{
    ssh->pktout.length = 5;
    ssh2_pkt_addbyte(ssh, (unsigned char) pkt_type);
}
static void ssh2_pkt_addbool(Ssh ssh, unsigned char value)
{
    ssh2_pkt_adddata(ssh, &value, 1);
}
static void ssh2_pkt_adduint32(Ssh ssh, unsigned long value)
{
    unsigned char x[4];
    PUT_32BIT(x, value);
    ssh2_pkt_adddata(ssh, x, 4);
}
static void ssh2_pkt_addstring_start(Ssh ssh)
{
    ssh2_pkt_adduint32(ssh, 0);
    ssh->pktout.savedpos = ssh->pktout.length;
}
static void ssh2_pkt_addstring_str(Ssh ssh, char *data)
{
    ssh2_pkt_adddata(ssh, data, strlen(data));
    PUT_32BIT(ssh->pktout.data + ssh->pktout.savedpos - 4,
	      ssh->pktout.length - ssh->pktout.savedpos);
}
static void ssh2_pkt_addstring_data(Ssh ssh, char *data, int len)
{
    ssh2_pkt_adddata(ssh, data, len);
    PUT_32BIT(ssh->pktout.data + ssh->pktout.savedpos - 4,
	      ssh->pktout.length - ssh->pktout.savedpos);
}
static void ssh2_pkt_addstring(Ssh ssh, char *data)
{
    ssh2_pkt_addstring_start(ssh);
    ssh2_pkt_addstring_str(ssh, data);
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
static void ssh2_pkt_addmp(Ssh ssh, Bignum b)
{
    unsigned char *p;
    int len;
    p = ssh2_mpint_fmt(b, &len);
    ssh2_pkt_addstring_start(ssh);
    ssh2_pkt_addstring_data(ssh, (char *)p, len);
    sfree(p);
}

/*
 * Construct an SSH2 final-form packet: compress it, encrypt it,
 * put the MAC on it. Final packet, ready to be sent, is stored in
 * ssh->pktout.data. Total length is returned.
 */
static int ssh2_pkt_construct(Ssh ssh)
{
    int cipherblk, maclen, padding, i;

    if (ssh->logctx)
	log_packet(ssh->logctx, PKT_OUTGOING, ssh->pktout.data[5],
		   ssh2_pkt_type(ssh->pkt_ctx, ssh->pktout.data[5]),
		   ssh->pktout.data + 6, ssh->pktout.length - 6);

    /*
     * Compress packet payload.
     */
    {
	unsigned char *newpayload;
	int newlen;
	if (ssh->cscomp &&
	    ssh->cscomp->compress(ssh->cs_comp_ctx, ssh->pktout.data + 5,
				  ssh->pktout.length - 5,
				  &newpayload, &newlen)) {
	    ssh->pktout.length = 5;
	    ssh2_pkt_adddata(ssh, newpayload, newlen);
	    sfree(newpayload);
	}
    }

    /*
     * Add padding. At least four bytes, and must also bring total
     * length (minus MAC) up to a multiple of the block size.
     */
    cipherblk = ssh->cscipher ? ssh->cscipher->blksize : 8;  /* block size */
    cipherblk = cipherblk < 8 ? 8 : cipherblk;	/* or 8 if blksize < 8 */
    padding = 4;
    padding +=
	(cipherblk - (ssh->pktout.length + padding) % cipherblk) % cipherblk;
    maclen = ssh->csmac ? ssh->csmac->len : 0;
    ssh2_pkt_ensure(ssh, ssh->pktout.length + padding + maclen);
    ssh->pktout.data[4] = padding;
    for (i = 0; i < padding; i++)
	ssh->pktout.data[ssh->pktout.length + i] = random_byte();
    PUT_32BIT(ssh->pktout.data, ssh->pktout.length + padding - 4);
    if (ssh->csmac)
	ssh->csmac->generate(ssh->cs_mac_ctx, ssh->pktout.data,
			     ssh->pktout.length + padding,
			     ssh->v2_outgoing_sequence);
    ssh->v2_outgoing_sequence++;       /* whether or not we MACed */

    if (ssh->cscipher)
	ssh->cscipher->encrypt(ssh->cs_cipher_ctx,
			       ssh->pktout.data, ssh->pktout.length + padding);

    /* Ready-to-send packet starts at ssh->pktout.data. We return length. */
    return ssh->pktout.length + padding + maclen;
}

/*
 * Construct and send an SSH2 packet immediately.
 */
static void ssh2_pkt_send(Ssh ssh)
{
    int len;
    int backlog;
    len = ssh2_pkt_construct(ssh);
    backlog = sk_write(ssh->s, (char *)ssh->pktout.data, len);
    if (backlog > SSH_MAX_BACKLOG)
	ssh_throttle_all(ssh, 1, backlog);
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
static void ssh2_pkt_defer(Ssh ssh)
{
    int len = ssh2_pkt_construct(ssh);
    if (ssh->deferred_len + len > ssh->deferred_size) {
	ssh->deferred_size = ssh->deferred_len + len + 128;
	ssh->deferred_send_data = sresize(ssh->deferred_send_data,
					  ssh->deferred_size,
					  unsigned char);
    }
    memcpy(ssh->deferred_send_data + ssh->deferred_len, ssh->pktout.data, len);
    ssh->deferred_len += len;
}

/*
 * Send the whole deferred data block constructed by
 * ssh2_pkt_defer() or SSH1's defer_packet().
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
static unsigned long ssh2_pkt_getuint32(Ssh ssh)
{
    unsigned long value;
    if (ssh->pktin.length - ssh->pktin.savedpos < 4)
	return 0;		       /* arrgh, no way to decline (FIXME?) */
    value = GET_32BIT(ssh->pktin.data + ssh->pktin.savedpos);
    ssh->pktin.savedpos += 4;
    return value;
}
static int ssh2_pkt_getbool(Ssh ssh)
{
    unsigned long value;
    if (ssh->pktin.length - ssh->pktin.savedpos < 1)
	return 0;		       /* arrgh, no way to decline (FIXME?) */
    value = ssh->pktin.data[ssh->pktin.savedpos] != 0;
    ssh->pktin.savedpos++;
    return value;
}
static void ssh2_pkt_getstring(Ssh ssh, char **p, int *length)
{
    int len;
    *p = NULL;
    *length = 0;
    if (ssh->pktin.length - ssh->pktin.savedpos < 4)
	return;
    len = GET_32BIT(ssh->pktin.data + ssh->pktin.savedpos);
    if (len < 0)
	return;
    *length = len;
    ssh->pktin.savedpos += 4;
    if (ssh->pktin.length - ssh->pktin.savedpos < *length)
	return;
    *p = (char *)(ssh->pktin.data + ssh->pktin.savedpos);
    ssh->pktin.savedpos += *length;
}
static Bignum ssh2_pkt_getmp(Ssh ssh)
{
    char *p;
    int length;
    Bignum b;

    ssh2_pkt_getstring(ssh, &p, &length);
    if (!p)
	return NULL;
    if (p[0] & 0x80) {
	bombout(("internal error: Can't handle negative mpints"));
	return NULL;
    }
    b = bignum_from_bytes((unsigned char *)p, length);
    return b;
}

/*
 * Helper function to add an SSH2 signature blob to a packet.
 * Expects to be shown the public key blob as well as the signature
 * blob. Normally works just like ssh2_pkt_addstring, but will
 * fiddle with the signature packet if necessary for
 * BUG_SSH2_RSA_PADDING.
 */
static void ssh2_add_sigblob(Ssh ssh, void *pkblob_v, int pkblob_len,
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
	    ssh2_pkt_addstring_start(ssh);
	    ssh2_pkt_addstring_data(ssh, (char *)sigblob, pos);
	    /* dmemdump(sigblob, pos); */
	    pos += 4;		       /* point to start of actual sig */
	    PUT_32BIT(newlen, len);
	    ssh2_pkt_addstring_data(ssh, (char *)newlen, 4);
	    /* dmemdump(newlen, 4); */
	    newlen[0] = 0;
	    while (len-- > siglen) {
		ssh2_pkt_addstring_data(ssh, (char *)newlen, 1);
		/* dmemdump(newlen, 1); */
	    }
	    ssh2_pkt_addstring_data(ssh, (char *)(sigblob+pos), siglen);
	    /* dmemdump(sigblob+pos, siglen); */
	    return;
	}

	/* Otherwise fall through and do it the easy way. */
    }

    ssh2_pkt_addstring_start(ssh);
    ssh2_pkt_addstring_data(ssh, (char *)sigblob, sigblob_len);
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
	  !strcmp(imp, "OSU_1.4alpha3")))) {
	/*
	 * These versions don't support SSH1_MSG_IGNORE, so we have
	 * to use a different defence against password length
	 * sniffing.
	 */
	ssh->remote_bugs |= BUG_CHOKES_ON_SSH1_IGNORE;
	logevent("We believe remote version has SSH1 ignore bug");
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
	logevent("We believe remote version needs a plain SSH1 password");
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
	logevent("We believe remote version can't handle RSA authentication");
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
	logevent("We believe remote version has SSH2 HMAC bug");
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
	logevent("We believe remote version has SSH2 key-derivation bug");
    }

    if (ssh->cfg.sshbug_rsapad2 == FORCE_ON ||
	(ssh->cfg.sshbug_rsapad2 == AUTO &&
	 (wc_match("OpenSSH_2.[5-9]*", imp) ||
	  wc_match("OpenSSH_3.[0-2]*", imp)))) {
	/*
	 * These versions have the SSH2 RSA padding bug.
	 */
	ssh->remote_bugs |= BUG_SSH2_RSA_PADDING;
	logevent("We believe remote version has SSH2 RSA padding bug");
    }

    if (ssh->cfg.sshbug_pksessid2 == FORCE_ON ||
	(ssh->cfg.sshbug_pksessid2 == AUTO &&
	 wc_match("OpenSSH_2.[0-2]*", imp))) {
	/*
	 * These versions have the SSH2 session-ID bug in
	 * public-key authentication.
	 */
	ssh->remote_bugs |= BUG_SSH2_PK_SESSIONID;
	logevent("We believe remote version has SSH2 public-key-session-ID bug");
    }

    if (ssh->cfg.sshbug_dhgex2 == FORCE_ON) {
	/*
	 * User specified the SSH2 DH GEX bug.
	 */
	ssh->remote_bugs |= BUG_SSH2_DH_GEX;
	logevent("We believe remote version has SSH2 DH group exchange bug");
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
    s->vstring[strcspn(s->vstring, "\r\n")] = '\0';/* remove EOL chars */
    {
	char *vlog;
	vlog = snewn(20 + s->vslen, char);
	sprintf(vlog, "Server version: %s", s->vstring);
	logevent(vlog);
	sfree(vlog);
    }
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

    if (s->proto2 && (ssh->cfg.sshprot >= 2 || !s->proto1)) {
	/*
	 * Use v2 protocol.
	 */
	char verstring[80], vlog[100];
	sprintf(verstring, "SSH-2.0-%s", sshver);
	SHA_Init(&ssh->exhashbase);
	/*
	 * Hash our version string and their version string.
	 */
	sha_string(&ssh->exhashbase, verstring, strlen(verstring));
	sha_string(&ssh->exhashbase, s->vstring, strcspn(s->vstring, "\r\n"));
	sprintf(vlog, "We claim version: %s", verstring);
	logevent(vlog);
	strcat(verstring, "\012");
	logevent("Using SSH protocol version 2");
	sk_write(ssh->s, verstring, strlen(verstring));
	ssh->protocol = ssh2_protocol;
	ssh->version = 2;
	ssh->s_rdpkt = ssh2_rdpkt;
    } else {
	/*
	 * Use v1 protocol.
	 */
	char verstring[80], vlog[100];
	sprintf(verstring, "SSH-%s-%s",
		(ssh_versioncmp(s->version, "1.5") <= 0 ? s->version : "1.5"),
		sshver);
	sprintf(vlog, "We claim version: %s", verstring);
	logevent(vlog);
	strcat(verstring, "\012");

	logevent("Using SSH protocol version 1");
	sk_write(ssh->s, verstring, strlen(verstring));
	ssh->protocol = ssh1_protocol;
	ssh->version = 1;
	ssh->s_rdpkt = ssh1_rdpkt;
    }
    update_specials_menu(ssh->frontend);
    ssh->state = SSH_STATE_BEFORE_SIZE;

    sfree(s->vstring);

    crFinish(0);
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
    while (1) {
	while (datalen > 0) {
	    if (ssh->s_rdpkt(ssh, &data, &datalen) == 0) {
		if (ssh->state == SSH_STATE_CLOSED) {
		    return;
		}
		ssh->protocol(ssh, NULL, 0, 1);
		if (ssh->state == SSH_STATE_CLOSED) {
		    return;
		}
	    }
	}
	crReturnV;
    }
    crFinishV;
}

static void ssh_do_close(Ssh ssh)
{
    int i;
    struct ssh_channel *c;

    ssh->state = SSH_STATE_CLOSED;
    if (ssh->s) {
        sk_close(ssh->s);
        ssh->s = NULL;
    }
    /*
     * Now we must shut down any port and X forwardings going
     * through this connection.
     */
    if (ssh->channels) {
	for (i = 0; NULL != (c = index234(ssh->channels, i)); i++) {
	    switch (c->type) {
	      case CHAN_X11:
		x11_close(c->u.x11.s);
		break;
	      case CHAN_SOCKDATA:
		pfd_close(c->u.pfd.s);
		break;
	    }
	    del234(ssh->channels, c);
	    if (ssh->version == 2)
		bufchain_clear(&c->v.v2.outbuffer);
	    sfree(c);
	}
    }
}

static int ssh_closing(Plug plug, const char *error_msg, int error_code,
		       int calling_back)
{
    Ssh ssh = (Ssh) plug;
    ssh_do_close(ssh);
    if (error_msg) {
	/* A socket error has occurred. */
	logevent(error_msg);
	connection_fatal(ssh->frontend, "%s", error_msg);
    } else {
	/* Otherwise, the remote side closed the connection normally. */
    }
    return 0;
}

static int ssh_receive(Plug plug, int urgent, char *data, int len)
{
    Ssh ssh = (Ssh) plug;
    ssh_gotdata(ssh, (unsigned char *)data, len);
    if (ssh->state == SSH_STATE_CLOSED) {
	ssh_do_close(ssh);
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
				   char **realhost, int nodelay)
{
    static const struct plug_function_table fn_table = {
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
    logeventf(ssh, "Looking up host \"%s\"", host);
    addr = name_lookup(host, port, realhost, &ssh->cfg);
    if ((err = sk_addr_error(addr)) != NULL) {
	sk_addr_free(addr);
	return err;
    }

    /*
     * Open socket.
     */
    {
	char addrbuf[100];
	sk_getaddr(addr, addrbuf, 100);
	logeventf(ssh, "Connecting to %s port %d", addrbuf, port);
    }
    ssh->fn = &fn_table;
    ssh->s = new_connection(addr, *realhost, port,
			    0, 1, nodelay, (Plug) ssh, &ssh->cfg);
    if ((err = sk_socket_error(ssh->s)) != NULL) {
	ssh->s = NULL;
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
	sk_set_frozen(ssh->s, 1);
    } else if (!ssh->v1_throttle_count && old_count) {
	sk_set_frozen(ssh->s, 0);
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
 * reusable in several places - even between SSH1 and SSH2.
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
	do_ssh1_login(ssh, NULL, -1, 0);
    else
	do_ssh2_authconn(ssh, NULL, -1, 0);
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
		    PKT_INT, replylen,
		    PKT_DATA, sentreply, replylen,
		    PKT_END);
    }
    if (reply)
	sfree(reply);
}

/*
 * Handle the key exchange and user authentication phases.
 */
static int do_ssh1_login(Ssh ssh, unsigned char *in, int inlen, int ispkt)
{
    int i, j;
    unsigned char cookie[8];
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
    };
    crState(do_ssh1_login_state);

    crBegin(ssh->do_ssh1_login_crstate);

    if (!ispkt)
	crWaitUntil(ispkt);

    if (ssh->pktin.type != SSH1_SMSG_PUBLIC_KEY) {
	bombout(("Public key packet not received"));
	crStop(0);
    }

    logevent("Received public keys");

    memcpy(cookie, ssh->pktin.body, 8);

    i = makekey(ssh->pktin.body + 8, &servkey, &s->keystr1, 0);
    j = makekey(ssh->pktin.body + 8 + i, &hostkey, &s->keystr2, 0);

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

    ssh->v1_remote_protoflags = GET_32BIT(ssh->pktin.body + 8 + i + j);
    s->supported_ciphers_mask = GET_32BIT(ssh->pktin.body + 12 + i + j);
    s->supported_auths_mask = GET_32BIT(ssh->pktin.body + 16 + i + j);

    ssh->v1_local_protoflags =
	ssh->v1_remote_protoflags & SSH1_PROTOFLAGS_SUPPORTED;
    ssh->v1_local_protoflags |= SSH1_PROTOFLAG_SCREEN_NUMBER;

    MD5Init(&md5c);
    MD5Update(&md5c, s->keystr2, hostkey.bytes);
    MD5Update(&md5c, s->keystr1, servkey.bytes);
    MD5Update(&md5c, ssh->pktin.body, 8);
    MD5Final(s->session_id, &md5c);

    for (i = 0; i < 32; i++)
	ssh->session_key[i] = random_byte();

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
	verify_ssh_host_key(ssh->frontend,
			    ssh->savedhost, ssh->savedport, "rsa", keystr,
			    fingerprint);
	sfree(keystr);
    }

    for (i = 0; i < 32; i++) {
	s->rsabuf[i] = ssh->session_key[i];
	if (i < 16)
	    s->rsabuf[i] ^= s->session_id[i];
    }

    if (hostkey.bytes > servkey.bytes) {
	rsaencrypt(s->rsabuf, 32, &servkey);
	rsaencrypt(s->rsabuf, servkey.bytes, &hostkey);
    } else {
	rsaencrypt(s->rsabuf, 32, &hostkey);
	rsaencrypt(s->rsabuf, hostkey.bytes, &servkey);
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
		logevent("AES not supported in SSH1, skipping");
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
		bombout(("Server violates SSH 1 protocol by not "
			 "supporting 3DES encryption"));
	    else
		/* shouldn't happen */
		bombout(("No supported ciphers found"));
	    crStop(0);
	}

	/* Warn about chosen cipher if necessary. */
	if (warn)
	    askcipher(ssh->frontend, cipher_string, 0);
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
    crWaitUntil(ispkt);

    if (ssh->pktin.type != SSH1_SMSG_SUCCESS) {
	bombout(("Encryption not successfully enabled"));
	crStop(0);
    }

    logevent("Successfully started encryption");

    fflush(stdout);
    {
	if ((flags & FLAG_INTERACTIVE) && !*ssh->cfg.username) {
	    if (ssh_get_line && !ssh_getline_pw_only) {
		if (!ssh_get_line("login as: ",
				  s->username, sizeof(s->username), FALSE)) {
		    /*
		     * get_line failed to get a username.
		     * Terminate.
		     */
		    logevent("No username provided. Abandoning session.");
                    ssh_closing((Plug)ssh, NULL, 0, 0);
		    crStop(1);
		}
	    } else {
		int ret;	       /* need not be kept over crReturn */
		c_write_str(ssh, "login as: ");
		ssh->send_ok = 1;

		setup_userpass_input(ssh, s->username, sizeof(s->username), 1);
		do {
		    crWaitUntil(!ispkt);
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

    crWaitUntil(ispkt);

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

    while (ssh->pktin.type == SSH1_SMSG_FAILURE) {
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
		    if (ispkt) {
			bombout(("Unexpected data from server while waiting"
				 " for agent response"));
			crStop(0);
		    }
		} while (ispkt || inlen > 0);
		r = ssh->agent_response;
		s->responselen = ssh->agent_response_len;
	    }
	    s->response = (unsigned char *) r;
	    if (s->response && s->responselen >= 5 &&
		s->response[4] == SSH1_AGENT_RSA_IDENTITIES_ANSWER) {
		s->p = s->response + 5;
		s->nkeys = GET_32BIT(s->p);
		s->p += 4;
		{
		    char buf[64];
		    sprintf(buf, "Pageant has %d SSH1 keys", s->nkeys);
		    logevent(buf);
		}
		for (s->keyi = 0; s->keyi < s->nkeys; s->keyi++) {
		    {
			char buf[64];
			sprintf(buf, "Trying Pageant key #%d", s->keyi);
			logevent(buf);
		    }
		    if (s->publickey_blob &&
			!memcmp(s->p, s->publickey_blob,
				s->publickey_bloblen)) {
			logevent("This key matches configured key file");
			s->tried_publickey = 1;
		    }
		    s->p += 4;
		    s->p += ssh1_read_bignum(s->p, &s->key.exponent);
		    s->p += ssh1_read_bignum(s->p, &s->key.modulus);
		    s->commentlen = GET_32BIT(s->p);
		    s->p += 4;
		    s->commentp = (char *)s->p;
		    s->p += s->commentlen;
		    send_packet(ssh, SSH1_CMSG_AUTH_RSA,
				PKT_BIGNUM, s->key.modulus, PKT_END);
		    crWaitUntil(ispkt);
		    if (ssh->pktin.type != SSH1_SMSG_AUTH_RSA_CHALLENGE) {
			logevent("Key refused");
			continue;
		    }
		    logevent("Received RSA challenge");
		    ssh1_read_bignum(ssh->pktin.body, &s->challenge);
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
				if (ispkt) {
				    bombout(("Unexpected data from server"
					     " while waiting for agent"
					     " response"));
				    crStop(0);
				}
			    } while (ispkt || inlen > 0);
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
				crWaitUntil(ispkt);
				if (ssh->pktin.type == SSH1_SMSG_SUCCESS) {
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
	    crWaitUntil(ispkt);
	    if (ssh->pktin.type != SSH1_SMSG_AUTH_TIS_CHALLENGE) {
		logevent("TIS authentication declined");
		if (flags & FLAG_INTERACTIVE)
		    c_write_str(ssh, "TIS authentication refused.\r\n");
		s->tis_auth_refused = 1;
		continue;
	    } else {
		int challengelen = GET_32BIT(ssh->pktin.body);
		logevent("Received TIS challenge");
		if (challengelen > sizeof(s->prompt) - 1)
		    challengelen = sizeof(s->prompt) - 1;/* prevent overrun */
		memcpy(s->prompt, ssh->pktin.body + 4, challengelen);
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
	    crWaitUntil(ispkt);
	    if (ssh->pktin.type != SSH1_SMSG_AUTH_CCARD_CHALLENGE) {
		logevent("CryptoCard authentication declined");
		c_write_str(ssh, "CryptoCard authentication refused.\r\n");
		s->ccard_auth_refused = 1;
		continue;
	    } else {
		int challengelen = GET_32BIT(ssh->pktin.body);
		logevent("Received CryptoCard challenge");
		if (challengelen > sizeof(s->prompt) - 1)
		    challengelen = sizeof(s->prompt) - 1;/* prevent overrun */
		memcpy(s->prompt, ssh->pktin.body + 4, challengelen);
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
	    char msgbuf[256];
	    if (flags & FLAG_VERBOSE)
		c_write_str(ssh, "Trying public key authentication.\r\n");
	    logeventf(ssh, "Trying public key \"%s\"",
		      filename_to_str(&ssh->cfg.keyfile));
	    type = key_type(&ssh->cfg.keyfile);
	    if (type != SSH_KEYTYPE_SSH1) {
		sprintf(msgbuf, "Key is of wrong type (%s)",
			key_type_to_str(type));
		logevent(msgbuf);
		c_write_str(ssh, msgbuf);
		c_write_str(ssh, "\r\n");
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
	    if (!ssh_get_line(s->prompt, s->password,
			      sizeof(s->password), TRUE)) {
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
		crWaitUntil(!ispkt);
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

	    crWaitUntil(ispkt);
	    if (ssh->pktin.type == SSH1_SMSG_FAILURE) {
		c_write_str(ssh, "Server refused our public key.\r\n");
		continue;	       /* go and try password */
	    }
	    if (ssh->pktin.type != SSH1_SMSG_AUTH_RSA_CHALLENGE) {
		bombout(("Bizarre response to offer of public key"));
		crStop(0);
	    }

	    {
		int i;
		unsigned char buffer[32];
		Bignum challenge, response;

		ssh1_read_bignum(ssh->pktin.body, &challenge);
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

	    crWaitUntil(ispkt);
	    if (ssh->pktin.type == SSH1_SMSG_FAILURE) {
		if (flags & FLAG_VERBOSE)
		    c_write_str(ssh, "Failed to authenticate with"
				" our public key.\r\n");
		continue;	       /* go and try password */
	    } else if (ssh->pktin.type != SSH1_SMSG_SUCCESS) {
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
			if (i == pwlen)
			    defer_packet(ssh, s->pwpkt_type,
					 PKT_STR, s->password, PKT_END);
			else {
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
		    send_packet(ssh, s->pwpkt_type, PKT_INT, len,
				PKT_DATA, ss, len, PKT_END);
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
		    send_packet(ssh, s->pwpkt_type, PKT_INT, len,
				PKT_DATA, s->password, len, PKT_END);
		}
	    } else {
		send_packet(ssh, s->pwpkt_type, PKT_STR, s->password, PKT_END);
	    }
	}
	logevent("Sent password");
	memset(s->password, 0, strlen(s->password));
	crWaitUntil(ispkt);
	if (ssh->pktin.type == SSH1_SMSG_FAILURE) {
	    if (flags & FLAG_VERBOSE)
		c_write_str(ssh, "Access denied\r\n");
	    logevent("Authentication refused");
	} else if (ssh->pktin.type != SSH1_SMSG_SUCCESS) {
	    bombout(("Strange packet received, type %d", ssh->pktin.type));
	    crStop(0);
	}
    }

    logevent("Authentication successful");

    crFinish(1);
}

void sshfwd_close(struct ssh_channel *c)
{
    Ssh ssh = c->ssh;

    if (ssh->state != SSH_STATE_SESSION) {
	assert(ssh->state == SSH_STATE_CLOSED);
	return;
    }

    if (c && !c->closes) {
	/*
	 * If the channel's remoteid is -1, we have sent
	 * CHANNEL_OPEN for this channel, but it hasn't even been
	 * acknowledged by the server. So we must set a close flag
	 * on it now, and then when the server acks the channel
	 * open, we can close it then.
	 */
	if (((int)c->remoteid) != -1) {
	    if (ssh->version == 1) {
		send_packet(ssh, SSH1_MSG_CHANNEL_CLOSE, PKT_INT, c->remoteid,
			    PKT_END);
	    } else {
		ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_CLOSE);
		ssh2_pkt_adduint32(ssh, c->remoteid);
		ssh2_pkt_send(ssh);
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

    if (ssh->state != SSH_STATE_SESSION) {
	assert(ssh->state == SSH_STATE_CLOSED);
	return 0;
    }

    if (ssh->version == 1) {
	send_packet(ssh, SSH1_MSG_CHANNEL_DATA,
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
    Ssh ssh = c->ssh;

    if (ssh->state != SSH_STATE_SESSION) {
	assert(ssh->state == SSH_STATE_CLOSED);
	return;
    }

    if (ssh->version == 1) {
	if (c->v.v1.throttling && bufsize < SSH1_BUFFER_LIMIT) {
	    c->v.v1.throttling = 0;
	    ssh1_throttle(ssh, -1);
	}
    } else {
	ssh2_set_window(c, OUR_V2_WINSIZE - bufsize);
    }
}

static void ssh1_protocol(Ssh ssh, unsigned char *in, int inlen, int ispkt)
{
    crBegin(ssh->ssh1_protocol_crstate);

    random_init();

    while (!do_ssh1_login(ssh, in, inlen, ispkt)) {
	crReturnV;
    }
    if (ssh->state == SSH_STATE_CLOSED)
	crReturnV;

    if (ssh->cfg.agentfwd && agent_exists()) {
	logevent("Requesting agent forwarding");
	send_packet(ssh, SSH1_CMSG_AGENT_REQUEST_FORWARDING, PKT_END);
	do {
	    crReturnV;
	} while (!ispkt);
	if (ssh->pktin.type != SSH1_SMSG_SUCCESS
	    && ssh->pktin.type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crStopV;
	} else if (ssh->pktin.type == SSH1_SMSG_FAILURE) {
	    logevent("Agent forwarding refused");
	} else {
	    logevent("Agent forwarding enabled");
	    ssh->agentfwd_enabled = TRUE;
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
	} while (!ispkt);
	if (ssh->pktin.type != SSH1_SMSG_SUCCESS
	    && ssh->pktin.type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crStopV;
	} else if (ssh->pktin.type == SSH1_SMSG_FAILURE) {
	    logevent("X11 forwarding refused");
	} else {
	    logevent("X11 forwarding enabled");
	    ssh->X11_fwd_enabled = TRUE;
	}
    }

    {
	char type;
	int n;
	int sport,dport,sserv,dserv;
	char sports[256], dports[256], saddr[256], host[256];

	ssh->rportfwds = newtree234(ssh_rportcmp_ssh1);
        /* Add port forwardings. */
	ssh->portfwd_strptr = ssh->cfg.portfwd;
	while (*ssh->portfwd_strptr) {
	    type = *ssh->portfwd_strptr++;
	    saddr[0] = '\0';
	    n = 0;
	    while (*ssh->portfwd_strptr && *ssh->portfwd_strptr != '\t') {
		if (*ssh->portfwd_strptr == ':') {
		    /*
		     * We've seen a colon in the middle of the
		     * source port number. This means that
		     * everything we've seen until now is the
		     * source _address_, so we'll move it into
		     * saddr and start sports from the beginning
		     * again.
		     */
		    ssh->portfwd_strptr++;
		    sports[n] = '\0';
		    strcpy(saddr, sports);
		    n = 0;
		}
		if (n < 255) sports[n++] = *ssh->portfwd_strptr++;
	    }
	    sports[n] = 0;
	    if (type != 'D') {
		if (*ssh->portfwd_strptr == '\t')
		    ssh->portfwd_strptr++;
		n = 0;
		while (*ssh->portfwd_strptr && *ssh->portfwd_strptr != ':') {
		    if (n < 255) host[n++] = *ssh->portfwd_strptr++;
		}
		host[n] = 0;
		if (*ssh->portfwd_strptr == ':')
		    ssh->portfwd_strptr++;
		n = 0;
		while (*ssh->portfwd_strptr) {
		    if (n < 255) dports[n++] = *ssh->portfwd_strptr++;
		}
		dports[n] = 0;
		ssh->portfwd_strptr++;
		dport = atoi(dports);
		dserv = 0;
		if (dport == 0) {
		    dserv = 1;
		    dport = net_service_lookup(dports);
		    if (!dport) {
			logeventf(ssh, "Service lookup failed for"
				  " destination port \"%s\"", dports);
		    }
		}
	    } else {
		while (*ssh->portfwd_strptr) ssh->portfwd_strptr++;
		dport = dserv = -1;
		ssh->portfwd_strptr++; /* eat the NUL and move to next one */
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
		if (type == 'L') {
		    pfd_addforward(host, dport, *saddr ? saddr : NULL,
				   sport, ssh, &ssh->cfg);
		    logeventf(ssh, "Local port %.*s%.*s%.*s%.*s%d%.*s"
			      " forwarding to %s:%.*s%.*s%d%.*s",
			      (int)(*saddr?strlen(saddr):0), *saddr?saddr:NULL,
			      (int)(*saddr?1:0), ":",
			      (int)(sserv ? strlen(sports) : 0), sports,
			      sserv, "(", sport, sserv, ")",
			      host,
			      (int)(dserv ? strlen(dports) : 0), dports,
			      dserv, "(", dport, dserv, ")");
		} else if (type == 'D') {
		    pfd_addforward(NULL, -1, *saddr ? saddr : NULL,
				   sport, ssh, &ssh->cfg);
		    logeventf(ssh, "Local port %.*s%.*s%.*s%.*s%d%.*s"
			      " doing SOCKS dynamic forwarding",
			      (int)(*saddr?strlen(saddr):0), *saddr?saddr:NULL,
			      (int)(*saddr?1:0), ":",
			      (int)(sserv ? strlen(sports) : 0), sports,
			      sserv, "(", sport, sserv, ")");
		} else {
		    struct ssh_rportfwd *pf;
		    pf = snew(struct ssh_rportfwd);
		    strcpy(pf->dhost, host);
		    pf->dport = dport;
		    if (saddr) {
			logeventf(ssh,
				  "SSH1 cannot handle source address spec \"%s:%d\"; ignoring",
				  saddr, sport);
		    }
		    if (add234(ssh->rportfwds, pf) != pf) {
			logeventf(ssh, 
				  "Duplicate remote port forwarding to %s:%d",
				  host, dport);
			sfree(pf);
		    } else {
			logeventf(ssh, "Requesting remote port %.*s%.*s%d%.*s"
				  " forward to %s:%.*s%.*s%d%.*s",
				  (int)(sserv ? strlen(sports) : 0), sports,
				  sserv, "(", sport, sserv, ")",
				  host,
				  (int)(dserv ? strlen(dports) : 0), dports,
				  dserv, "(", dport, dserv, ")");
			send_packet(ssh, SSH1_CMSG_PORT_FORWARD_REQUEST,
				    PKT_INT, sport,
				    PKT_STR, host,
				    PKT_INT, dport,
				    PKT_END);
			do {
			    crReturnV;
			} while (!ispkt);
			if (ssh->pktin.type != SSH1_SMSG_SUCCESS
			    && ssh->pktin.type != SSH1_SMSG_FAILURE) {
			    bombout(("Protocol confusion"));
			    crStopV;
			} else if (ssh->pktin.type == SSH1_SMSG_FAILURE) {
			    c_write_str(ssh, "Server refused port"
					" forwarding\r\n");
			}
			logevent("Remote port forwarding enabled");
		    }
		}
	    }
	}
    }

    if (!ssh->cfg.nopty) {
	send_packet(ssh, SSH1_CMSG_REQUEST_PTY,
		    PKT_STR, ssh->cfg.termtype,
		    PKT_INT, ssh->term_height,
		    PKT_INT, ssh->term_width,
		    PKT_INT, 0, PKT_INT, 0, PKT_CHAR, 0, PKT_END);
	ssh->state = SSH_STATE_INTERMED;
	do {
	    crReturnV;
	} while (!ispkt);
	if (ssh->pktin.type != SSH1_SMSG_SUCCESS
	    && ssh->pktin.type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crStopV;
	} else if (ssh->pktin.type == SSH1_SMSG_FAILURE) {
	    c_write_str(ssh, "Server refused to allocate pty\r\n");
	    ssh->editing = ssh->echoing = 1;
	}
	logevent("Allocated pty");
    } else {
	ssh->editing = ssh->echoing = 1;
    }

    if (ssh->cfg.compression) {
	send_packet(ssh, SSH1_CMSG_REQUEST_COMPRESSION, PKT_INT, 6, PKT_END);
	do {
	    crReturnV;
	} while (!ispkt);
	if (ssh->pktin.type != SSH1_SMSG_SUCCESS
	    && ssh->pktin.type != SSH1_SMSG_FAILURE) {
	    bombout(("Protocol confusion"));
	    crStopV;
	} else if (ssh->pktin.type == SSH1_SMSG_FAILURE) {
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
     * Special case: if the first-choice command is an SSH2
     * subsystem (hence not usable here) and the second choice
     * exists, we fall straight back to that.
     */
    {
	char *cmd = ssh->cfg.remote_cmd_ptr;
	
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
	crReturnV;
	if (ispkt) {
	    if (ssh->pktin.type == SSH1_SMSG_STDOUT_DATA ||
		ssh->pktin.type == SSH1_SMSG_STDERR_DATA) {
		long len = GET_32BIT(ssh->pktin.body);
		int bufsize =
		    from_backend(ssh->frontend,
				 ssh->pktin.type == SSH1_SMSG_STDERR_DATA,
				 (char *)(ssh->pktin.body) + 4, len);
		if (!ssh->v1_stdout_throttling && bufsize > SSH1_BUFFER_LIMIT) {
		    ssh->v1_stdout_throttling = 1;
		    ssh1_throttle(ssh, +1);
		}
	    } else if (ssh->pktin.type == SSH1_MSG_DISCONNECT) {
                ssh_closing((Plug)ssh, NULL, 0, 0);
		logevent("Received disconnect request");
		crStopV;
	    } else if (ssh->pktin.type == SSH1_SMSG_X11_OPEN) {
		/* Remote side is trying to open a channel to talk to our
		 * X-Server. Give them back a local channel number. */
		struct ssh_channel *c;

		logevent("Received X11 connect request");
		/* Refuse if X11 forwarding is disabled. */
		if (!ssh->X11_fwd_enabled) {
		    send_packet(ssh, SSH1_MSG_CHANNEL_OPEN_FAILURE,
				PKT_INT, GET_32BIT(ssh->pktin.body), PKT_END);
		    logevent("Rejected X11 connect request");
		} else {
		    c = snew(struct ssh_channel);
		    c->ssh = ssh;

		    if (x11_init(&c->u.x11.s, ssh->cfg.x11_display, c,
				 ssh->x11auth, NULL, -1, &ssh->cfg) != NULL) {
			logevent("opening X11 forward connection failed");
			sfree(c);
			send_packet(ssh, SSH1_MSG_CHANNEL_OPEN_FAILURE,
				    PKT_INT, GET_32BIT(ssh->pktin.body),
				    PKT_END);
		    } else {
			logevent
			    ("opening X11 forward connection succeeded");
			c->remoteid = GET_32BIT(ssh->pktin.body);
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
	    } else if (ssh->pktin.type == SSH1_SMSG_AGENT_OPEN) {
		/* Remote side is trying to open a channel to talk to our
		 * agent. Give them back a local channel number. */
		struct ssh_channel *c;

		/* Refuse if agent forwarding is disabled. */
		if (!ssh->agentfwd_enabled) {
		    send_packet(ssh, SSH1_MSG_CHANNEL_OPEN_FAILURE,
				PKT_INT, GET_32BIT(ssh->pktin.body), PKT_END);
		} else {
		    c = snew(struct ssh_channel);
		    c->ssh = ssh;
		    c->remoteid = GET_32BIT(ssh->pktin.body);
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
	    } else if (ssh->pktin.type == SSH1_MSG_PORT_OPEN) {
   		/* Remote side is trying to open a channel to talk to a
		 * forwarded port. Give them back a local channel number. */
		struct ssh_channel *c;
		struct ssh_rportfwd pf;
		int hostsize, port;
		char host[256], buf[1024];
		char *p, *h;
		const char *e;
		c = snew(struct ssh_channel);
		c->ssh = ssh;

		hostsize = GET_32BIT(ssh->pktin.body+4);
		for (h = host, p = (char *)(ssh->pktin.body+8);
		     hostsize != 0; hostsize--) {
		    if (h+1 < host+sizeof(host))
			*h++ = *p;
		    p++;
		}
		*h = 0;
		port = GET_32BIT(p);

		strcpy(pf.dhost, host);
		pf.dport = port;

		if (find234(ssh->rportfwds, &pf, NULL) == NULL) {
		    sprintf(buf, "Rejected remote port open request for %s:%d",
			    host, port);
		    logevent(buf);
                    send_packet(ssh, SSH1_MSG_CHANNEL_OPEN_FAILURE,
                                PKT_INT, GET_32BIT(ssh->pktin.body), PKT_END);
		} else {
		    sprintf(buf, "Received remote port open request for %s:%d",
			    host, port);
		    logevent(buf);
		    e = pfd_newconnect(&c->u.pfd.s, host, port, c, &ssh->cfg);
		    if (e != NULL) {
			char buf[256];
			sprintf(buf, "Port open failed: %s", e);
			logevent(buf);
			sfree(c);
			send_packet(ssh, SSH1_MSG_CHANNEL_OPEN_FAILURE,
				    PKT_INT, GET_32BIT(ssh->pktin.body),
				    PKT_END);
		    } else {
			c->remoteid = GET_32BIT(ssh->pktin.body);
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

	    } else if (ssh->pktin.type == SSH1_MSG_CHANNEL_OPEN_CONFIRMATION) {
		unsigned int remoteid = GET_32BIT(ssh->pktin.body);
		unsigned int localid = GET_32BIT(ssh->pktin.body+4);
		struct ssh_channel *c;

		c = find234(ssh->channels, &remoteid, ssh_channelfind);
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
		    send_packet(ssh, SSH1_MSG_CHANNEL_CLOSE,
				PKT_INT, c->remoteid, PKT_END);
		}

	    } else if (ssh->pktin.type == SSH1_MSG_CHANNEL_OPEN_FAILURE) {
		unsigned int remoteid = GET_32BIT(ssh->pktin.body);
		struct ssh_channel *c;

		c = find234(ssh->channels, &remoteid, ssh_channelfind);
		if (c && c->type == CHAN_SOCKDATA_DORMANT) {
		    logevent("Forwarded connection refused by server");
		    pfd_close(c->u.pfd.s);
		    del234(ssh->channels, c);
		    sfree(c);
		}

	    } else if (ssh->pktin.type == SSH1_MSG_CHANNEL_CLOSE ||
		       ssh->pktin.type == SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION) {
		/* Remote side closes a channel. */
		unsigned i = GET_32BIT(ssh->pktin.body);
		struct ssh_channel *c;
		c = find234(ssh->channels, &i, ssh_channelfind);
		if (c && ((int)c->remoteid) != -1) {
		    int closetype;
		    closetype =
			(ssh->pktin.type == SSH1_MSG_CHANNEL_CLOSE ? 1 : 2);

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
			send_packet(ssh, ssh->pktin.type, PKT_INT, c->remoteid,
				    PKT_END);
			c->closes |= closetype;      /* sent it too */
		    }

		    if (c->closes == 15) {
			del234(ssh->channels, c);
			sfree(c);
		    }
		} else {
		    bombout(("Received CHANNEL_CLOSE%s for %s channel %d\n",
			     ssh->pktin.type == SSH1_MSG_CHANNEL_CLOSE ? "" :
			     "_CONFIRMATION", c ? "half-open" : "nonexistent",
			     i));
		    crStopV;
		}
	    } else if (ssh->pktin.type == SSH1_MSG_CHANNEL_DATA) {
		/* Data sent down one of our channels. */
		int i = GET_32BIT(ssh->pktin.body);
		int len = GET_32BIT(ssh->pktin.body + 4);
		unsigned char *p = ssh->pktin.body + 8;
		struct ssh_channel *c;
		c = find234(ssh->channels, &i, ssh_channelfind);
		if (c) {
		    int bufsize = 0;
		    switch (c->type) {
		      case CHAN_X11:
			bufsize = x11_send(c->u.x11.s, (char *)p, len);
			break;
		      case CHAN_SOCKDATA:
			bufsize = pfd_send(c->u.pfd.s, (char *)p, len);
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
				c->u.a.message = snewn(c->u.a.totallen,
						       unsigned char);
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
	    } else if (ssh->pktin.type == SSH1_SMSG_SUCCESS) {
		/* may be from EXEC_SHELL on some servers */
	    } else if (ssh->pktin.type == SSH1_SMSG_FAILURE) {
		/* may be from EXEC_SHELL on some servers
		 * if no pty is available or in other odd cases. Ignore */
	    } else if (ssh->pktin.type == SSH1_SMSG_EXIT_STATUS) {
		char buf[100];
		ssh->exitcode = GET_32BIT(ssh->pktin.body);
		sprintf(buf, "Server sent command exit status %d",
			ssh->exitcode);
		logevent(buf);
		send_packet(ssh, SSH1_CMSG_EXIT_CONFIRMATION, PKT_END);
                /*
                 * In case `helpful' firewalls or proxies tack
                 * extra human-readable text on the end of the
                 * session which we might mistake for another
                 * encrypted packet, we close the session once
                 * we've sent EXIT_CONFIRMATION.
                 */
                ssh_closing((Plug)ssh, NULL, 0, 0);
                crStopV;
	    } else {
		bombout(("Strange packet received: type %d", ssh->pktin.type));
		crStopV;
	    }
	} else {
	    while (inlen > 0) {
		int len = min(inlen, 512);
		send_packet(ssh, SSH1_CMSG_STDIN_DATA,
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
 * SSH2 key creation method.
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
 * Handle the SSH2 transport layer.
 */
static int do_ssh2_transport(Ssh ssh, unsigned char *in, int inlen, int ispkt)
{
    struct do_ssh2_transport_state {
	int nbits, pbits, warn;
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
	int n_preferred_ciphers;
	const struct ssh2_ciphers *preferred_ciphers[CIPHER_MAX];
	const struct ssh_compress *preferred_comp;
	int first_kex;
    };
    crState(do_ssh2_transport_state);

    crBegin(ssh->do_ssh2_transport_crstate);

    s->cscipher_tobe = s->sccipher_tobe = NULL;
    s->csmac_tobe = s->scmac_tobe = NULL;
    s->cscomp_tobe = s->sccomp_tobe = NULL;

    random_init();
    s->first_kex = 1;

    {
	int i;
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
    }

    /*
     * Set up preferred compression.
     */
    if (ssh->cfg.compression)
	s->preferred_comp = &ssh_zlib;
    else
	s->preferred_comp = &ssh_comp_none;

    /*
     * Be prepared to work around the buggy MAC problem.
     */
    if (ssh->remote_bugs & BUG_SSH2_HMAC)
	s->maclist = buggymacs, s->nmacs = lenof(buggymacs);
    else
	s->maclist = macs, s->nmacs = lenof(macs);

  begin_key_exchange:
    {
	int i, j, cipherstr_started;

	/*
	 * Construct and send our key exchange packet.
	 */
	ssh2_pkt_init(ssh, SSH2_MSG_KEXINIT);
	for (i = 0; i < 16; i++)
	    ssh2_pkt_addbyte(ssh, (unsigned char) random_byte());
	/* List key exchange algorithms. */
	ssh2_pkt_addstring_start(ssh);
	for (i = 0; i < lenof(kex_algs); i++) {
	    if (kex_algs[i] == &ssh_diffiehellman_gex &&
		(ssh->remote_bugs & BUG_SSH2_DH_GEX))
		continue;
	    ssh2_pkt_addstring_str(ssh, kex_algs[i]->name);
	    if (i < lenof(kex_algs) - 1)
		ssh2_pkt_addstring_str(ssh, ",");
	}
	/* List server host key algorithms. */
	ssh2_pkt_addstring_start(ssh);
	for (i = 0; i < lenof(hostkey_algs); i++) {
	    ssh2_pkt_addstring_str(ssh, hostkey_algs[i]->name);
	    if (i < lenof(hostkey_algs) - 1)
		ssh2_pkt_addstring_str(ssh, ",");
	}
	/* List client->server encryption algorithms. */
	ssh2_pkt_addstring_start(ssh);
	cipherstr_started = 0;
	for (i = 0; i < s->n_preferred_ciphers; i++) {
	    const struct ssh2_ciphers *c = s->preferred_ciphers[i];
	    if (!c) continue;	       /* warning flag */
	    for (j = 0; j < c->nciphers; j++) {
		if (cipherstr_started)
		    ssh2_pkt_addstring_str(ssh, ",");
		ssh2_pkt_addstring_str(ssh, c->list[j]->name);
		cipherstr_started = 1;
	    }
	}
	/* List server->client encryption algorithms. */
	ssh2_pkt_addstring_start(ssh);
	cipherstr_started = 0;
	for (i = 0; i < s->n_preferred_ciphers; i++) {
	    const struct ssh2_ciphers *c = s->preferred_ciphers[i];
	    if (!c) continue; /* warning flag */
	    for (j = 0; j < c->nciphers; j++) {
		if (cipherstr_started)
		    ssh2_pkt_addstring_str(ssh, ",");
		ssh2_pkt_addstring_str(ssh, c->list[j]->name);
		cipherstr_started = 1;
	    }
	}
	/* List client->server MAC algorithms. */
	ssh2_pkt_addstring_start(ssh);
	for (i = 0; i < s->nmacs; i++) {
	    ssh2_pkt_addstring_str(ssh, s->maclist[i]->name);
	    if (i < s->nmacs - 1)
		ssh2_pkt_addstring_str(ssh, ",");
	}
	/* List server->client MAC algorithms. */
	ssh2_pkt_addstring_start(ssh);
	for (i = 0; i < s->nmacs; i++) {
	    ssh2_pkt_addstring_str(ssh, s->maclist[i]->name);
	    if (i < s->nmacs - 1)
		ssh2_pkt_addstring_str(ssh, ",");
	}
	/* List client->server compression algorithms. */
	ssh2_pkt_addstring_start(ssh);
	for (i = 0; i < lenof(compressions) + 1; i++) {
	    const struct ssh_compress *c =
		i == 0 ? s->preferred_comp : compressions[i - 1];
	    ssh2_pkt_addstring_str(ssh, c->name);
	    if (i < lenof(compressions))
		ssh2_pkt_addstring_str(ssh, ",");
	}
	/* List server->client compression algorithms. */
	ssh2_pkt_addstring_start(ssh);
	for (i = 0; i < lenof(compressions) + 1; i++) {
	    const struct ssh_compress *c =
		i == 0 ? s->preferred_comp : compressions[i - 1];
	    ssh2_pkt_addstring_str(ssh, c->name);
	    if (i < lenof(compressions))
		ssh2_pkt_addstring_str(ssh, ",");
	}
	/* List client->server languages. Empty list. */
	ssh2_pkt_addstring_start(ssh);
	/* List server->client languages. Empty list. */
	ssh2_pkt_addstring_start(ssh);
	/* First KEX packet does _not_ follow, because we're not that brave. */
	ssh2_pkt_addbool(ssh, FALSE);
	/* Reserved. */
	ssh2_pkt_adduint32(ssh, 0);
    }

    ssh->exhash = ssh->exhashbase;
    sha_string(&ssh->exhash, ssh->pktout.data + 5, ssh->pktout.length - 5);

    ssh2_pkt_send(ssh);

    if (!ispkt)
	crWaitUntil(ispkt);
    if (ssh->pktin.length > 5)
	sha_string(&ssh->exhash, ssh->pktin.data + 5, ssh->pktin.length - 5);

    /*
     * Now examine the other side's KEXINIT to see what we're up
     * to.
     */
    {
	char *str;
	int i, j, len;

	if (ssh->pktin.type != SSH2_MSG_KEXINIT) {
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
	ssh->pktin.savedpos += 16;	        /* skip garbage cookie */
	ssh2_pkt_getstring(ssh, &str, &len);    /* key exchange algorithms */
	for (i = 0; i < lenof(kex_algs); i++) {
	    if (kex_algs[i] == &ssh_diffiehellman_gex &&
		(ssh->remote_bugs & BUG_SSH2_DH_GEX))
		continue;
	    if (in_commasep_string(kex_algs[i]->name, str, len)) {
		ssh->kex = kex_algs[i];
		break;
	    }
	}
	ssh2_pkt_getstring(ssh, &str, &len);    /* host key algorithms */
	for (i = 0; i < lenof(hostkey_algs); i++) {
	    if (in_commasep_string(hostkey_algs[i]->name, str, len)) {
		ssh->hostkey = hostkey_algs[i];
		break;
	    }
	}
	ssh2_pkt_getstring(ssh, &str, &len);    /* client->server cipher */
	s->warn = 0;
	for (i = 0; i < s->n_preferred_ciphers; i++) {
	    const struct ssh2_ciphers *c = s->preferred_ciphers[i];
	    if (!c) {
		s->warn = 1;
	    } else {
		for (j = 0; j < c->nciphers; j++) {
		    if (in_commasep_string(c->list[j]->name, str, len)) {
			s->cscipher_tobe = c->list[j];
			break;
		    }
		}
	    }
	    if (s->cscipher_tobe) {
		if (s->warn)
		    askcipher(ssh->frontend, s->cscipher_tobe->name, 1);
		break;
	    }
	}
	if (!s->cscipher_tobe) {
	    bombout(("Couldn't agree a client-to-server cipher (available: %s)",
		     str ? str : "(null)"));
	    crStop(0);
	}

	ssh2_pkt_getstring(ssh, &str, &len);    /* server->client cipher */
	s->warn = 0;
	for (i = 0; i < s->n_preferred_ciphers; i++) {
	    const struct ssh2_ciphers *c = s->preferred_ciphers[i];
	    if (!c) {
		s->warn = 1;
	    } else {
		for (j = 0; j < c->nciphers; j++) {
		    if (in_commasep_string(c->list[j]->name, str, len)) {
			s->sccipher_tobe = c->list[j];
			break;
		    }
		}
	    }
	    if (s->sccipher_tobe) {
		if (s->warn)
		    askcipher(ssh->frontend, s->sccipher_tobe->name, 2);
		break;
	    }
	}
	if (!s->sccipher_tobe) {
	    bombout(("Couldn't agree a server-to-client cipher (available: %s)",
		     str ? str : "(null)"));
	    crStop(0);
	}

	ssh2_pkt_getstring(ssh, &str, &len);    /* client->server mac */
	for (i = 0; i < s->nmacs; i++) {
	    if (in_commasep_string(s->maclist[i]->name, str, len)) {
		s->csmac_tobe = s->maclist[i];
		break;
	    }
	}
	ssh2_pkt_getstring(ssh, &str, &len);    /* server->client mac */
	for (i = 0; i < s->nmacs; i++) {
	    if (in_commasep_string(s->maclist[i]->name, str, len)) {
		s->scmac_tobe = s->maclist[i];
		break;
	    }
	}
	ssh2_pkt_getstring(ssh, &str, &len);  /* client->server compression */
	for (i = 0; i < lenof(compressions) + 1; i++) {
	    const struct ssh_compress *c =
		i == 0 ? s->preferred_comp : compressions[i - 1];
	    if (in_commasep_string(c->name, str, len)) {
		s->cscomp_tobe = c;
		break;
	    }
	}
	ssh2_pkt_getstring(ssh, &str, &len);  /* server->client compression */
	for (i = 0; i < lenof(compressions) + 1; i++) {
	    const struct ssh_compress *c =
		i == 0 ? s->preferred_comp : compressions[i - 1];
	    if (in_commasep_string(c->name, str, len)) {
		s->sccomp_tobe = c;
		break;
	    }
	}
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
    if (ssh->kex == &ssh_diffiehellman_gex) {
	logevent("Doing Diffie-Hellman group exchange");
	ssh->pkt_ctx |= SSH2_PKTCTX_DHGEX;
	/*
	 * Work out how big a DH group we will need to allow that
	 * much data.
	 */
	s->pbits = 512 << ((s->nbits - 1) / 64);
	ssh2_pkt_init(ssh, SSH2_MSG_KEX_DH_GEX_REQUEST);
	ssh2_pkt_adduint32(ssh, s->pbits);
	ssh2_pkt_send(ssh);

	crWaitUntil(ispkt);
	if (ssh->pktin.type != SSH2_MSG_KEX_DH_GEX_GROUP) {
	    bombout(("expected key exchange group packet from server"));
	    crStop(0);
	}
	s->p = ssh2_pkt_getmp(ssh);
	s->g = ssh2_pkt_getmp(ssh);
	ssh->kex_ctx = dh_setup_group(s->p, s->g);
	s->kex_init_value = SSH2_MSG_KEX_DH_GEX_INIT;
	s->kex_reply_value = SSH2_MSG_KEX_DH_GEX_REPLY;
    } else {
	ssh->pkt_ctx |= SSH2_PKTCTX_DHGROUP1;
	ssh->kex_ctx = dh_setup_group1();
	s->kex_init_value = SSH2_MSG_KEXDH_INIT;
	s->kex_reply_value = SSH2_MSG_KEXDH_REPLY;
    }

    logevent("Doing Diffie-Hellman key exchange");
    /*
     * Now generate and send e for Diffie-Hellman.
     */
    s->e = dh_create_e(ssh->kex_ctx, s->nbits * 2);
    ssh2_pkt_init(ssh, s->kex_init_value);
    ssh2_pkt_addmp(ssh, s->e);
    ssh2_pkt_send(ssh);

    crWaitUntil(ispkt);
    if (ssh->pktin.type != s->kex_reply_value) {
	bombout(("expected key exchange reply packet from server"));
	crStop(0);
    }
    ssh2_pkt_getstring(ssh, &s->hostkeydata, &s->hostkeylen);
    s->f = ssh2_pkt_getmp(ssh);
    ssh2_pkt_getstring(ssh, &s->sigdata, &s->siglen);

    s->K = dh_find_K(ssh->kex_ctx, s->f);

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
    verify_ssh_host_key(ssh->frontend,
			ssh->savedhost, ssh->savedport, ssh->hostkey->keytype,
			s->keystr, s->fingerprint);
    if (s->first_kex) {		       /* don't bother logging this in rekeys */
	logevent("Host key fingerprint is:");
	logevent(s->fingerprint);
    }
    sfree(s->fingerprint);
    sfree(s->keystr);
    ssh->hostkey->freekey(s->hkey);

    /*
     * Send SSH2_MSG_NEWKEYS.
     */
    ssh2_pkt_init(ssh, SSH2_MSG_NEWKEYS);
    ssh2_pkt_send(ssh);

    /*
     * Expect SSH2_MSG_NEWKEYS from server.
     */
    crWaitUntil(ispkt);
    if (ssh->pktin.type != SSH2_MSG_NEWKEYS) {
	bombout(("expected new-keys packet from server"));
	crStop(0);
    }

    /*
     * Create and initialise session keys.
     */
    if (ssh->cs_cipher_ctx)
	ssh->cscipher->free_context(ssh->cs_cipher_ctx);
    ssh->cscipher = s->cscipher_tobe;
    ssh->cs_cipher_ctx = ssh->cscipher->make_context();

    if (ssh->sc_cipher_ctx)
	ssh->sccipher->free_context(ssh->sc_cipher_ctx);
    ssh->sccipher = s->sccipher_tobe;
    ssh->sc_cipher_ctx = ssh->sccipher->make_context();

    if (ssh->cs_mac_ctx)
	ssh->csmac->free_context(ssh->cs_mac_ctx);
    ssh->csmac = s->csmac_tobe;
    ssh->cs_mac_ctx = ssh->csmac->make_context();

    if (ssh->sc_mac_ctx)
	ssh->scmac->free_context(ssh->sc_mac_ctx);
    ssh->scmac = s->scmac_tobe;
    ssh->sc_mac_ctx = ssh->scmac->make_context();

    if (ssh->cs_comp_ctx)
	ssh->cscomp->compress_cleanup(ssh->cs_comp_ctx);
    ssh->cscomp = s->cscomp_tobe;
    ssh->cs_comp_ctx = ssh->cscomp->compress_init();

    if (ssh->sc_comp_ctx)
	ssh->sccomp->decompress_cleanup(ssh->sc_comp_ctx);
    ssh->sccomp = s->sccomp_tobe;
    ssh->sc_comp_ctx = ssh->sccomp->decompress_init();

    /*
     * Set IVs after keys. Here we use the exchange hash from the
     * _first_ key exchange.
     */
    {
	unsigned char keyspace[40];
	if (s->first_kex)
	    memcpy(ssh->v2_session_id, s->exchange_hash,
		   sizeof(s->exchange_hash));
	ssh2_mkkey(ssh,s->K,s->exchange_hash,ssh->v2_session_id,'C',keyspace);
	ssh->cscipher->setkey(ssh->cs_cipher_ctx, keyspace);
	ssh2_mkkey(ssh,s->K,s->exchange_hash,ssh->v2_session_id,'D',keyspace);
	ssh->sccipher->setkey(ssh->sc_cipher_ctx, keyspace);
	ssh2_mkkey(ssh,s->K,s->exchange_hash,ssh->v2_session_id,'A',keyspace);
	ssh->cscipher->setiv(ssh->cs_cipher_ctx, keyspace);
	ssh2_mkkey(ssh,s->K,s->exchange_hash,ssh->v2_session_id,'B',keyspace);
	ssh->sccipher->setiv(ssh->sc_cipher_ctx, keyspace);
	ssh2_mkkey(ssh,s->K,s->exchange_hash,ssh->v2_session_id,'E',keyspace);
	ssh->csmac->setkey(ssh->cs_mac_ctx, keyspace);
	ssh2_mkkey(ssh,s->K,s->exchange_hash,ssh->v2_session_id,'F',keyspace);
	ssh->scmac->setkey(ssh->sc_mac_ctx, keyspace);
    }
    logeventf(ssh, "Initialised %.200s client->server encryption",
	      ssh->cscipher->text_name);
    logeventf(ssh, "Initialised %.200s server->client encryption",
	      ssh->sccipher->text_name);
    if (ssh->cscomp->text_name)
	logeventf(ssh, "Initialised %s compression",
		  ssh->cscomp->text_name);
    if (ssh->sccomp->text_name)
	logeventf(ssh, "Initialised %s decompression",
		  ssh->sccomp->text_name);
    freebn(s->f);
    freebn(s->K);
    if (ssh->kex == &ssh_diffiehellman_gex) {
	freebn(s->g);
	freebn(s->p);
    }

    /*
     * If this is the first key exchange phase, we must pass the
     * SSH2_MSG_NEWKEYS packet to the next layer, not because it
     * wants to see it but because it will need time to initialise
     * itself before it sees an actual packet. In subsequent key
     * exchange phases, we don't pass SSH2_MSG_NEWKEYS on, because
     * it would only confuse the layer above.
     */
    if (!s->first_kex) {
	crReturn(0);
    }
    s->first_kex = 0;

    /*
     * Now we're encrypting. Begin returning 1 to the protocol main
     * function so that other things can run on top of the
     * transport. If we ever see a KEXINIT, we must go back to the
     * start.
     */
    while (!(ispkt && ssh->pktin.type == SSH2_MSG_KEXINIT)) {
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
    Ssh ssh = c->ssh;

    while (c->v.v2.remwindow > 0 && bufchain_size(&c->v.v2.outbuffer) > 0) {
	int len;
	void *data;
	bufchain_prefix(&c->v.v2.outbuffer, &data, &len);
	if ((unsigned)len > c->v.v2.remwindow)
	    len = c->v.v2.remwindow;
	if ((unsigned)len > c->v.v2.remmaxpkt)
	    len = c->v.v2.remmaxpkt;
	ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_DATA);
	ssh2_pkt_adduint32(ssh, c->remoteid);
	ssh2_pkt_addstring_start(ssh);
	ssh2_pkt_addstring_data(ssh, data, len);
	ssh2_pkt_send(ssh);
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
    Ssh ssh = c->ssh;

    /*
     * Never send WINDOW_ADJUST for a channel that the remote side
     * already thinks it's closed; there's no point, since it won't
     * be sending any more data anyway.
     */
    if (c->closes != 0)
	return;

    if (newwin > c->v.v2.locwindow) {
	ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_WINDOW_ADJUST);
	ssh2_pkt_adduint32(ssh, c->remoteid);
	ssh2_pkt_adduint32(ssh, newwin - c->v.v2.locwindow);
	ssh2_pkt_send(ssh);
	c->v.v2.locwindow = newwin;
    }
}

/*
 * Handle the SSH2 userauth and connection layers.
 */
static void do_ssh2_authconn(Ssh ssh, unsigned char *in, int inlen, int ispkt)
{
    struct do_ssh2_authconn_state {
	enum {
	    AUTH_INVALID, AUTH_PUBLICKEY_AGENT, AUTH_PUBLICKEY_FILE,
		AUTH_PASSWORD,
		AUTH_KEYBOARD_INTERACTIVE
	} method;
	enum {
	    AUTH_TYPE_NONE,
		AUTH_TYPE_PUBLICKEY,
		AUTH_TYPE_PUBLICKEY_OFFER_LOUD,
		AUTH_TYPE_PUBLICKEY_OFFER_QUIET,
		AUTH_TYPE_PASSWORD,
		AUTH_TYPE_KEYBOARD_INTERACTIVE,
		AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET
	} type;
	int gotit, need_pw, can_pubkey, can_passwd, can_keyb_inter;
	int tried_pubkey_config, tried_agent, tried_keyb_inter;
	int kbd_inter_running;
	int we_are_in;
	int num_prompts, curr_prompt, echo;
	char username[100];
	int got_username;
	char pwprompt[200];
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
    };
    crState(do_ssh2_authconn_state);

    crBegin(ssh->do_ssh2_authconn_crstate);

    /*
     * Request userauth protocol, and await a response to it.
     */
    ssh2_pkt_init(ssh, SSH2_MSG_SERVICE_REQUEST);
    ssh2_pkt_addstring(ssh, "ssh-userauth");
    ssh2_pkt_send(ssh);
    crWaitUntilV(ispkt);
    if (ssh->pktin.type != SSH2_MSG_SERVICE_ACCEPT) {
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
	} else if ((flags & FLAG_INTERACTIVE) && !*ssh->cfg.username) {
	    if (ssh_get_line && !ssh_getline_pw_only) {
		if (!ssh_get_line("login as: ",
				  s->username, sizeof(s->username), FALSE)) {
		    /*
		     * get_line failed to get a username.
		     * Terminate.
		     */
		    logevent("No username provided. Abandoning session.");
                    ssh_closing((Plug)ssh, NULL, 0, 0);
		    crStopV;
		}
	    } else {
		int ret;	       /* need not be saved across crReturn */
		c_write_str(ssh, "login as: ");
		ssh->send_ok = 1;
		setup_userpass_input(ssh, s->username, sizeof(s->username), 1);
		do {
		    crWaitUntilV(!ispkt);
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

	ssh2_pkt_init(ssh, SSH2_MSG_USERAUTH_REQUEST);
	ssh2_pkt_addstring(ssh, s->username);
	ssh2_pkt_addstring(ssh, "ssh-connection");/* service requested */
	ssh2_pkt_addstring(ssh, "none");    /* method */
	ssh2_pkt_send(ssh);
	s->type = AUTH_TYPE_NONE;
	s->gotit = FALSE;
	s->we_are_in = FALSE;

	s->tried_pubkey_config = FALSE;
	s->tried_agent = FALSE;
	s->tried_keyb_inter = FALSE;
	s->kbd_inter_running = FALSE;
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
		crWaitUntilV(ispkt);
	    while (ssh->pktin.type == SSH2_MSG_USERAUTH_BANNER) {
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
		    ssh2_pkt_getstring(ssh, &banner, &size);
		    if (banner)
			c_write_untrusted(ssh, banner, size);
		}
		crWaitUntilV(ispkt);
	    }
	    if (ssh->pktin.type == SSH2_MSG_USERAUTH_SUCCESS) {
		logevent("Access granted");
		s->we_are_in = TRUE;
		break;
	    }

	    if (s->kbd_inter_running &&
		ssh->pktin.type == SSH2_MSG_USERAUTH_INFO_REQUEST) {
		/*
		 * This is either a further set-of-prompts packet
		 * in keyboard-interactive authentication, or it's
		 * the same one and we came back here with `gotit'
		 * set. In the former case, we must reset the
		 * curr_prompt variable.
		 */
		if (!s->gotit)
		    s->curr_prompt = 0;
	    } else if (ssh->pktin.type != SSH2_MSG_USERAUTH_FAILURE) {
		bombout(("Strange packet received during authentication: type %d",
			 ssh->pktin.type));
		crStopV;
	    }

	    s->gotit = FALSE;

	    /*
	     * OK, we're now sitting on a USERAUTH_FAILURE message, so
	     * we can look at the string in it and know what we can
	     * helpfully try next.
	     */
	    if (ssh->pktin.type == SSH2_MSG_USERAUTH_FAILURE) {
		char *methods;
		int methlen;
		ssh2_pkt_getstring(ssh, &methods, &methlen);
		s->kbd_inter_running = FALSE;
		if (!ssh2_pkt_getbool(ssh)) {
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
		    if (s->type == AUTH_TYPE_NONE) {
			/* do nothing */
		    } else if (s->type == AUTH_TYPE_PUBLICKEY_OFFER_LOUD ||
			       s->type == AUTH_TYPE_PUBLICKEY_OFFER_QUIET) {
			if (s->type == AUTH_TYPE_PUBLICKEY_OFFER_LOUD)
			    c_write_str(ssh, "Server refused our key\r\n");
			logevent("Server refused public key");
		    } else if (s->type==AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET) {
			/* server declined keyboard-interactive; ignore */
		    } else {
			c_write_str(ssh, "Access denied\r\n");
			logevent("Access denied");
			if (s->type == AUTH_TYPE_PASSWORD) {
			    s->we_are_in = FALSE;
			    break;
			}
		    }
		} else {
		    c_write_str(ssh, "Further authentication required\r\n");
		    logevent("Further authentication required");
		}

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
			if (ispkt) {
			    bombout(("Unexpected data from server while"
				     " waiting for agent response"));
			    crStopV;
			}
		    } while (ispkt || inlen > 0);
		    r = ssh->agent_response;
		    s->responselen = ssh->agent_response_len;
		}
		s->response = (unsigned char *) r;
		if (s->response && s->responselen >= 5 &&
		    s->response[4] == SSH2_AGENT_IDENTITIES_ANSWER) {
		    s->p = s->response + 5;
		    s->nkeys = GET_32BIT(s->p);
		    s->p += 4;
		    {
			char buf[64];
			sprintf(buf, "Pageant has %d SSH2 keys", s->nkeys);
			logevent(buf);
		    }
		    for (s->keyi = 0; s->keyi < s->nkeys; s->keyi++) {
			void *vret;

			{
			    char buf[64];
			    sprintf(buf, "Trying Pageant key #%d", s->keyi);
			    logevent(buf);
			}
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
			ssh2_pkt_init(ssh, SSH2_MSG_USERAUTH_REQUEST);
			ssh2_pkt_addstring(ssh, s->username);
			ssh2_pkt_addstring(ssh, "ssh-connection");	/* service requested */
			ssh2_pkt_addstring(ssh, "publickey");	/* method */
			ssh2_pkt_addbool(ssh, FALSE);	/* no signature included */
			ssh2_pkt_addstring_start(ssh);
			ssh2_pkt_addstring_data(ssh, s->alg, s->alglen);
			ssh2_pkt_addstring_start(ssh);
			ssh2_pkt_addstring_data(ssh, s->pkblob, s->pklen);
			ssh2_pkt_send(ssh);

			crWaitUntilV(ispkt);
			if (ssh->pktin.type != SSH2_MSG_USERAUTH_PK_OK) {
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
			ssh2_pkt_init(ssh, SSH2_MSG_USERAUTH_REQUEST);
			ssh2_pkt_addstring(ssh, s->username);
			ssh2_pkt_addstring(ssh, "ssh-connection");	/* service requested */
			ssh2_pkt_addstring(ssh, "publickey");	/* method */
			ssh2_pkt_addbool(ssh, TRUE);
			ssh2_pkt_addstring_start(ssh);
			ssh2_pkt_addstring_data(ssh, s->alg, s->alglen);
			ssh2_pkt_addstring_start(ssh);
			ssh2_pkt_addstring_data(ssh, s->pkblob, s->pklen);

			s->siglen = ssh->pktout.length - 5 + 4 + 20;
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
			memcpy(s->q, ssh->pktout.data + 5,
			       ssh->pktout.length - 5);
			s->q += ssh->pktout.length - 5;
			/* And finally the (zero) flags word. */
			PUT_32BIT(s->q, 0);
			if (!agent_query(s->agentreq, s->len + 4,
					 &vret, &s->retlen,
					 ssh_agent_callback, ssh)) {
			    do {
				crReturnV;
				if (ispkt) {
				    bombout(("Unexpected data from server"
					     " while waiting for agent"
					     " response"));
				    crStopV;
				}
			    } while (ispkt || inlen > 0);
			    vret = ssh->agent_response;
			    s->retlen = ssh->agent_response_len;
			}
			s->ret = vret;
			sfree(s->agentreq);
			if (s->ret) {
			    if (s->ret[4] == SSH2_AGENT_SIGN_RESPONSE) {
				logevent("Sending Pageant's response");
				ssh2_add_sigblob(ssh, s->pkblob, s->pklen,
						 s->ret + 9,
						 GET_32BIT(s->ret + 5));
				ssh2_pkt_send(ssh);
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
		    ssh2_pkt_init(ssh, SSH2_MSG_USERAUTH_REQUEST);
		    ssh2_pkt_addstring(ssh, s->username);
		    ssh2_pkt_addstring(ssh, "ssh-connection");	/* service requested */
		    ssh2_pkt_addstring(ssh, "publickey");	/* method */
		    ssh2_pkt_addbool(ssh, FALSE);	/* no signature included */
		    ssh2_pkt_addstring(ssh, algorithm);
		    ssh2_pkt_addstring_start(ssh);
		    ssh2_pkt_addstring_data(ssh, (char *)pub_blob,
					    pub_blob_len);
		    ssh2_pkt_send(ssh);
		    logevent("Offered public key");	/* FIXME */

		    crWaitUntilV(ispkt);
		    if (ssh->pktin.type != SSH2_MSG_USERAUTH_PK_OK) {
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
		    c_write_str(ssh, "Authenticating with public key \"");
		    c_write_str(ssh, comment);
		    c_write_str(ssh, "\"\r\n");
		    s->method = AUTH_PUBLICKEY_FILE;
		}
	    }

	    if (!s->method && s->can_keyb_inter && !s->tried_keyb_inter) {
		s->method = AUTH_KEYBOARD_INTERACTIVE;
		s->type = AUTH_TYPE_KEYBOARD_INTERACTIVE;
		s->tried_keyb_inter = TRUE;

		ssh->pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
		ssh->pkt_ctx |= SSH2_PKTCTX_KBDINTER;

		ssh2_pkt_init(ssh, SSH2_MSG_USERAUTH_REQUEST);
		ssh2_pkt_addstring(ssh, s->username);
		ssh2_pkt_addstring(ssh, "ssh-connection");	/* service requested */
		ssh2_pkt_addstring(ssh, "keyboard-interactive");	/* method */
		ssh2_pkt_addstring(ssh, ""); /* lang */
		ssh2_pkt_addstring(ssh, "");
		ssh2_pkt_send(ssh);

		crWaitUntilV(ispkt);
		if (ssh->pktin.type != SSH2_MSG_USERAUTH_INFO_REQUEST) {
		    if (ssh->pktin.type == SSH2_MSG_USERAUTH_FAILURE)
			s->gotit = TRUE;
		    logevent("Keyboard-interactive authentication refused");
		    s->type = AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET;
		    continue;
		}

		s->kbd_inter_running = TRUE;
		s->curr_prompt = 0;
	    }

	    if (s->kbd_inter_running) {
		s->method = AUTH_KEYBOARD_INTERACTIVE;
		s->type = AUTH_TYPE_KEYBOARD_INTERACTIVE;
		s->tried_keyb_inter = TRUE;

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

		    ssh2_pkt_getstring(ssh, &name, &name_len);
		    ssh2_pkt_getstring(ssh, &inst, &inst_len);
		    ssh2_pkt_getstring(ssh, &lang, &lang_len);
		    if (name_len > 0) {
			c_write_untrusted(ssh, name, name_len);
			c_write_str(ssh, "\r\n");
		    }
		    if (inst_len > 0) {
			c_write_untrusted(ssh, inst, inst_len);
			c_write_str(ssh, "\r\n");
		    }
		    s->num_prompts = ssh2_pkt_getuint32(ssh);
		}

		/*
		 * If there are prompts remaining in the packet,
		 * display one and get a response.
		 */
		if (s->curr_prompt < s->num_prompts) {
		    char *prompt;
		    int prompt_len;

		    ssh2_pkt_getstring(ssh, &prompt, &prompt_len);
		    if (prompt_len > 0) {
			strncpy(s->pwprompt, prompt, sizeof(s->pwprompt));
			s->pwprompt[prompt_len < sizeof(s->pwprompt) ?
				    prompt_len : sizeof(s->pwprompt)-1] = '\0';
		    } else {
			strcpy(s->pwprompt,
			       "<server failed to send prompt>: ");
		    }
		    s->echo = ssh2_pkt_getbool(ssh);
		    s->need_pw = TRUE;
		} else
		    s->need_pw = FALSE;
	    }

	    if (!s->method && s->can_passwd) {
		s->method = AUTH_PASSWORD;
		ssh->pkt_ctx &= ~SSH2_PKTCTX_AUTH_MASK;
		ssh->pkt_ctx |= SSH2_PKTCTX_PASSWORD;
		sprintf(s->pwprompt, "%.90s@%.90s's password: ", s->username,
			ssh->savedhost);
		s->need_pw = TRUE;
	    }

	    if (s->need_pw) {
		if (ssh_get_line) {
		    if (!ssh_get_line(s->pwprompt, s->password,
				      sizeof(s->password), TRUE)) {
			/*
			 * get_line failed to get a password (for
			 * example because one was supplied on the
			 * command line which has already failed to
			 * work). Terminate.
			 */
			ssh2_pkt_init(ssh, SSH2_MSG_DISCONNECT);
			ssh2_pkt_adduint32(ssh,SSH2_DISCONNECT_BY_APPLICATION);
			ssh2_pkt_addstring(ssh, "No more passwords available"
					   " to try");
			ssh2_pkt_addstring(ssh, "en");	/* language tag */
			ssh2_pkt_send(ssh);
			logevent("Unable to authenticate");
			connection_fatal(ssh->frontend,
					 "Unable to authenticate");
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
			crWaitUntilV(!ispkt);
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
		    ssh2_pkt_init(ssh, SSH2_MSG_USERAUTH_REQUEST);
		    ssh2_pkt_addstring(ssh, s->username);
		    ssh2_pkt_addstring(ssh, "ssh-connection");	/* service requested */
		    ssh2_pkt_addstring(ssh, "none");	/* method */
		    ssh2_pkt_send(ssh);
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
		    ssh2_pkt_init(ssh, SSH2_MSG_USERAUTH_REQUEST);
		    ssh2_pkt_addstring(ssh, s->username);
		    ssh2_pkt_addstring(ssh, "ssh-connection");	/* service requested */
		    ssh2_pkt_addstring(ssh, "publickey");	/* method */
		    ssh2_pkt_addbool(ssh, TRUE);
		    ssh2_pkt_addstring(ssh, key->alg->name);
		    pkblob = key->alg->public_blob(key->data, &pkblob_len);
		    ssh2_pkt_addstring_start(ssh);
		    ssh2_pkt_addstring_data(ssh, (char *)pkblob, pkblob_len);

		    /*
		     * The data to be signed is:
		     *
		     *   string  session-id
		     *
		     * followed by everything so far placed in the
		     * outgoing packet.
		     */
		    sigdata_len = ssh->pktout.length - 5 + 4 + 20;
                    if (ssh->remote_bugs & BUG_SSH2_PK_SESSIONID)
                        sigdata_len -= 4;
		    sigdata = snewn(sigdata_len, unsigned char);
                    p = 0;
                    if (!(ssh->remote_bugs & BUG_SSH2_PK_SESSIONID)) {
                        PUT_32BIT(sigdata+p, 20);
                        p += 4;
                    }
		    memcpy(sigdata+p, ssh->v2_session_id, 20); p += 20;
		    memcpy(sigdata+p, ssh->pktout.data + 5,
			   ssh->pktout.length - 5);
                    p += ssh->pktout.length - 5;
                    assert(p == sigdata_len);
		    sigblob = key->alg->sign(key->data, (char *)sigdata,
					     sigdata_len, &sigblob_len);
		    ssh2_add_sigblob(ssh, pkblob, pkblob_len,
				     sigblob, sigblob_len);
		    sfree(pkblob);
		    sfree(sigblob);
		    sfree(sigdata);

		    ssh2_pkt_send(ssh);
		    s->type = AUTH_TYPE_PUBLICKEY;
		    key->alg->freekey(key->data);
		}
	    } else if (s->method == AUTH_PASSWORD) {
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
		ssh2_pkt_init(ssh, SSH2_MSG_USERAUTH_REQUEST);
		ssh2_pkt_addstring(ssh, s->username);
		ssh2_pkt_addstring(ssh, "ssh-connection");	/* service requested */
		ssh2_pkt_addstring(ssh, "password");
		ssh2_pkt_addbool(ssh, FALSE);
		ssh2_pkt_addstring(ssh, s->password);
		memset(s->password, 0, sizeof(s->password));
		ssh2_pkt_defer(ssh);
		/*
		 * We'll include a string that's an exact multiple of the
		 * cipher block size. If the cipher is NULL for some
		 * reason, we don't do this trick at all because we gain
		 * nothing by it.
		 */
		if (ssh->cscipher) {
		    int stringlen, i;

		    stringlen = (256 - ssh->deferred_len);
		    stringlen += ssh->cscipher->blksize - 1;
		    stringlen -= (stringlen % ssh->cscipher->blksize);
		    if (ssh->cscomp) {
			/*
			 * Temporarily disable actual compression,
			 * so we can guarantee to get this string
			 * exactly the length we want it. The
			 * compression-disabling routine should
			 * return an integer indicating how many
			 * bytes we should adjust our string length
			 * by.
			 */
			stringlen -= 
			    ssh->cscomp->disable_compression(ssh->cs_comp_ctx);
		    }
		    ssh2_pkt_init(ssh, SSH2_MSG_IGNORE);
		    ssh2_pkt_addstring_start(ssh);
		    for (i = 0; i < stringlen; i++) {
			char c = (char) random_byte();
			ssh2_pkt_addstring_data(ssh, &c, 1);
		    }
		    ssh2_pkt_defer(ssh);
		}
		ssh_pkt_defersend(ssh);
		logevent("Sent password");
		s->type = AUTH_TYPE_PASSWORD;
	    } else if (s->method == AUTH_KEYBOARD_INTERACTIVE) {
		if (s->curr_prompt == 0) {
		    ssh2_pkt_init(ssh, SSH2_MSG_USERAUTH_INFO_RESPONSE);
		    ssh2_pkt_adduint32(ssh, s->num_prompts);
		}
		if (s->need_pw) {      /* only add pw if we just got one! */
		    ssh2_pkt_addstring(ssh, s->password);
		    memset(s->password, 0, sizeof(s->password));
		    s->curr_prompt++;
		}
		if (s->curr_prompt >= s->num_prompts) {
		    ssh2_pkt_send(ssh);
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
		ssh2_pkt_init(ssh, SSH2_MSG_DISCONNECT);
		ssh2_pkt_adduint32(ssh, SSH2_DISCONNECT_BY_APPLICATION);
		ssh2_pkt_addstring(ssh, "No supported authentication"
				   " methods available");
		ssh2_pkt_addstring(ssh, "en");	/* language tag */
		ssh2_pkt_send(ssh);
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

    /*
     * So now create a channel with a session in it.
     */
    ssh->channels = newtree234(ssh_channelcmp);
    ssh->mainchan = snew(struct ssh_channel);
    ssh->mainchan->ssh = ssh;
    ssh->mainchan->localid = alloc_channel_id(ssh);
    ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_OPEN);
    ssh2_pkt_addstring(ssh, "session");
    ssh2_pkt_adduint32(ssh, ssh->mainchan->localid);
    ssh->mainchan->v.v2.locwindow = OUR_V2_WINSIZE;
    ssh2_pkt_adduint32(ssh, ssh->mainchan->v.v2.locwindow);/* our window size */
    ssh2_pkt_adduint32(ssh, 0x4000UL);      /* our max pkt size */
    ssh2_pkt_send(ssh);
    crWaitUntilV(ispkt);
    if (ssh->pktin.type != SSH2_MSG_CHANNEL_OPEN_CONFIRMATION) {
	bombout(("Server refused to open a session"));
	crStopV;
	/* FIXME: error data comes back in FAILURE packet */
    }
    if (ssh2_pkt_getuint32(ssh) != ssh->mainchan->localid) {
	bombout(("Server's channel confirmation cited wrong channel"));
	crStopV;
    }
    ssh->mainchan->remoteid = ssh2_pkt_getuint32(ssh);
    ssh->mainchan->type = CHAN_MAINSESSION;
    ssh->mainchan->closes = 0;
    ssh->mainchan->v.v2.remwindow = ssh2_pkt_getuint32(ssh);
    ssh->mainchan->v.v2.remmaxpkt = ssh2_pkt_getuint32(ssh);
    bufchain_init(&ssh->mainchan->v.v2.outbuffer);
    add234(ssh->channels, ssh->mainchan);
    logevent("Opened channel for session");

    /*
     * Potentially enable X11 forwarding.
     */
    if (ssh->cfg.x11_forward) {
	char proto[20], data[64];
	logevent("Requesting X11 forwarding");
	ssh->x11auth = x11_invent_auth(proto, sizeof(proto),
				       data, sizeof(data), ssh->cfg.x11_auth);
        x11_get_real_auth(ssh->x11auth, ssh->cfg.x11_display);
	ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(ssh, ssh->mainchan->remoteid);
	ssh2_pkt_addstring(ssh, "x11-req");
	ssh2_pkt_addbool(ssh, 1);	       /* want reply */
	ssh2_pkt_addbool(ssh, 0);	       /* many connections */
	ssh2_pkt_addstring(ssh, proto);
	ssh2_pkt_addstring(ssh, data);
	ssh2_pkt_adduint32(ssh, x11_get_screen_number(ssh->cfg.x11_display));
	ssh2_pkt_send(ssh);

	do {
	    crWaitUntilV(ispkt);
	    if (ssh->pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32(ssh);
		struct ssh_channel *c;
		c = find234(ssh->channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32(ssh);
	    }
	} while (ssh->pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);

	if (ssh->pktin.type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (ssh->pktin.type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout(("Unexpected response to X11 forwarding request:"
			 " packet type %d", ssh->pktin.type));
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
    {
	char type;
	int n;
	int sport,dport,sserv,dserv;
	char sports[256], dports[256], saddr[256], host[256];

	ssh->rportfwds = newtree234(ssh_rportcmp_ssh2);
        /* Add port forwardings. */
	ssh->portfwd_strptr = ssh->cfg.portfwd;
	while (*ssh->portfwd_strptr) {
	    type = *ssh->portfwd_strptr++;
	    saddr[0] = '\0';
	    n = 0;
	    while (*ssh->portfwd_strptr && *ssh->portfwd_strptr != '\t') {
		if (*ssh->portfwd_strptr == ':') {
		    /*
		     * We've seen a colon in the middle of the
		     * source port number. This means that
		     * everything we've seen until now is the
		     * source _address_, so we'll move it into
		     * saddr and start sports from the beginning
		     * again.
		     */
		    ssh->portfwd_strptr++;
		    sports[n] = '\0';
		    strcpy(saddr, sports);
		    n = 0;
		}
		if (n < 255) sports[n++] = *ssh->portfwd_strptr++;
	    }
	    sports[n] = 0;
	    if (type != 'D') {
		if (*ssh->portfwd_strptr == '\t')
		    ssh->portfwd_strptr++;
		n = 0;
		while (*ssh->portfwd_strptr && *ssh->portfwd_strptr != ':') {
		    if (n < 255) host[n++] = *ssh->portfwd_strptr++;
		}
		host[n] = 0;
		if (*ssh->portfwd_strptr == ':')
		    ssh->portfwd_strptr++;
		n = 0;
		while (*ssh->portfwd_strptr) {
		    if (n < 255) dports[n++] = *ssh->portfwd_strptr++;
		}
		dports[n] = 0;
		ssh->portfwd_strptr++;
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
		while (*ssh->portfwd_strptr) ssh->portfwd_strptr++;
		dport = dserv = -1;
		ssh->portfwd_strptr++; /* eat the NUL and move to next one */
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
		if (type == 'L') {
		    pfd_addforward(host, dport, *saddr ? saddr : NULL,
				   sport, ssh, &ssh->cfg);
		    logeventf(ssh, "Local port %.*s%.*s%.*s%.*s%d%.*s"
			      " forwarding to %s:%.*s%.*s%d%.*s",
			      (int)(*saddr?strlen(saddr):0), *saddr?saddr:NULL,
			      (int)(*saddr?1:0), ":",
			      (int)(sserv ? strlen(sports) : 0), sports,
			      sserv, "(", sport, sserv, ")",
			      host,
			      (int)(dserv ? strlen(dports) : 0), dports,
			      dserv, "(", dport, dserv, ")");
		} else if (type == 'D') {
		    pfd_addforward(NULL, -1, *saddr ? saddr : NULL,
				   sport, ssh, &ssh->cfg);
		    logeventf(ssh, "Local port %.*s%.*s%.*s%.*s%d%.*s"
			      " doing SOCKS dynamic forwarding",
			      (int)(*saddr?strlen(saddr):0), *saddr?saddr:NULL,
			      (int)(*saddr?1:0), ":",
			      (int)(sserv ? strlen(sports) : 0), sports,
			      sserv, "(", sport, sserv, ")");
		} else {
		    struct ssh_rportfwd *pf;
		    pf = snew(struct ssh_rportfwd);
		    strcpy(pf->dhost, host);
		    pf->dport = dport;
		    pf->sport = sport;
		    if (add234(ssh->rportfwds, pf) != pf) {
			logeventf(ssh, "Duplicate remote port forwarding"
				  " to %s:%d", host, dport);
			sfree(pf);
		    } else {
			logeventf(ssh, "Requesting remote port "
				  "%.*s%.*s%.*s%.*s%d%.*s"
				  " forward to %s:%.*s%.*s%d%.*s",
				  (int)(*saddr?strlen(saddr):0),
				  *saddr?saddr:NULL,
				  (int)(*saddr?1:0), ":",
				  (int)(sserv ? strlen(sports) : 0), sports,
				  sserv, "(", sport, sserv, ")",
				  host,
				  (int)(dserv ? strlen(dports) : 0), dports,
				  dserv, "(", dport, dserv, ")");
			ssh2_pkt_init(ssh, SSH2_MSG_GLOBAL_REQUEST);
			ssh2_pkt_addstring(ssh, "tcpip-forward");
			ssh2_pkt_addbool(ssh, 1);/* want reply */
			if (*saddr)
			    ssh2_pkt_addstring(ssh, saddr);
			if (ssh->cfg.rport_acceptall)
			    ssh2_pkt_addstring(ssh, "0.0.0.0");
			else
			    ssh2_pkt_addstring(ssh, "127.0.0.1");
			ssh2_pkt_adduint32(ssh, sport);
			ssh2_pkt_send(ssh);

			do {
			    crWaitUntilV(ispkt);
			    if (ssh->pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
				unsigned i = ssh2_pkt_getuint32(ssh);
				struct ssh_channel *c;
				c = find234(ssh->channels, &i, ssh_channelfind);
				if (!c)
				    continue;/* nonexistent channel */
				c->v.v2.remwindow += ssh2_pkt_getuint32(ssh);
			    }
			} while (ssh->pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);

			if (ssh->pktin.type != SSH2_MSG_REQUEST_SUCCESS) {
			    if (ssh->pktin.type != SSH2_MSG_REQUEST_FAILURE) {
				bombout(("Unexpected response to port "
					 "forwarding request: packet type %d",
					 ssh->pktin.type));
				crStopV;
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
    if (ssh->cfg.agentfwd && agent_exists()) {
	logevent("Requesting OpenSSH-style agent forwarding");
	ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(ssh, ssh->mainchan->remoteid);
	ssh2_pkt_addstring(ssh, "auth-agent-req@openssh.com");
	ssh2_pkt_addbool(ssh, 1);	       /* want reply */
	ssh2_pkt_send(ssh);

	do {
	    crWaitUntilV(ispkt);
	    if (ssh->pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32(ssh);
		struct ssh_channel *c;
		c = find234(ssh->channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32(ssh);
	    }
	} while (ssh->pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);

	if (ssh->pktin.type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (ssh->pktin.type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout(("Unexpected response to agent forwarding request:"
			 " packet type %d", ssh->pktin.type));
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
    if (!ssh->cfg.nopty) {
	ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(ssh, ssh->mainchan->remoteid);	/* recipient channel */
	ssh2_pkt_addstring(ssh, "pty-req");
	ssh2_pkt_addbool(ssh, 1);	       /* want reply */
	ssh2_pkt_addstring(ssh, ssh->cfg.termtype);
	ssh2_pkt_adduint32(ssh, ssh->term_width);
	ssh2_pkt_adduint32(ssh, ssh->term_height);
	ssh2_pkt_adduint32(ssh, 0);	       /* pixel width */
	ssh2_pkt_adduint32(ssh, 0);	       /* pixel height */
	ssh2_pkt_addstring_start(ssh);
	ssh2_pkt_addstring_data(ssh, "\0", 1);	/* TTY_OP_END, no special options */
	ssh2_pkt_send(ssh);
	ssh->state = SSH_STATE_INTERMED;

	do {
	    crWaitUntilV(ispkt);
	    if (ssh->pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32(ssh);
		struct ssh_channel *c;
		c = find234(ssh->channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32(ssh);
	    }
	} while (ssh->pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);

	if (ssh->pktin.type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (ssh->pktin.type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout(("Unexpected response to pty request:"
			 " packet type %d", ssh->pktin.type));
		crStopV;
	    }
	    c_write_str(ssh, "Server refused to allocate pty\r\n");
	    ssh->editing = ssh->echoing = 1;
	} else {
	    logevent("Allocated pty");
	}
    } else {
	ssh->editing = ssh->echoing = 1;
    }

    /*
     * Start a shell or a remote command. We may have to attempt
     * this twice if the config data has provided a second choice
     * of command.
     */
    while (1) {
	int subsys;
	char *cmd;

	if (ssh->fallback_cmd) {
	    subsys = ssh->cfg.ssh_subsys2;
	    cmd = ssh->cfg.remote_cmd_ptr2;
	} else {
	    subsys = ssh->cfg.ssh_subsys;
	    cmd = ssh->cfg.remote_cmd_ptr;
	}

	ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_REQUEST);
	ssh2_pkt_adduint32(ssh, ssh->mainchan->remoteid);	/* recipient channel */
	if (subsys) {
	    ssh2_pkt_addstring(ssh, "subsystem");
	    ssh2_pkt_addbool(ssh, 1);	       /* want reply */
	    ssh2_pkt_addstring(ssh, cmd);
	} else if (*cmd) {
	    ssh2_pkt_addstring(ssh, "exec");
	    ssh2_pkt_addbool(ssh, 1);	       /* want reply */
	    ssh2_pkt_addstring(ssh, cmd);
	} else {
	    ssh2_pkt_addstring(ssh, "shell");
	    ssh2_pkt_addbool(ssh, 1);	       /* want reply */
	}
	ssh2_pkt_send(ssh);
	do {
	    crWaitUntilV(ispkt);
	    if (ssh->pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32(ssh);
		struct ssh_channel *c;
		c = find234(ssh->channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32(ssh);
	    }
	} while (ssh->pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST);
	if (ssh->pktin.type != SSH2_MSG_CHANNEL_SUCCESS) {
	    if (ssh->pktin.type != SSH2_MSG_CHANNEL_FAILURE) {
		bombout(("Unexpected response to shell/command request:"
			 " packet type %d", ssh->pktin.type));
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
    ssh->send_ok = 1;
    while (1) {
	crReturnV;
	s->try_send = FALSE;
	if (ispkt) {
	    if (ssh->pktin.type == SSH2_MSG_CHANNEL_DATA ||
		ssh->pktin.type == SSH2_MSG_CHANNEL_EXTENDED_DATA) {
		char *data;
		int length;
		unsigned i = ssh2_pkt_getuint32(ssh);
		struct ssh_channel *c;
		c = find234(ssh->channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		if (ssh->pktin.type == SSH2_MSG_CHANNEL_EXTENDED_DATA &&
		    ssh2_pkt_getuint32(ssh) != SSH2_EXTENDED_DATA_STDERR)
		    continue;	       /* extended but not stderr */
		ssh2_pkt_getstring(ssh, &data, &length);
		if (data) {
		    int bufsize = 0;
		    c->v.v2.locwindow -= length;
		    switch (c->type) {
		      case CHAN_MAINSESSION:
			bufsize =
			    from_backend(ssh->frontend, ssh->pktin.type ==
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
				c->u.a.message = snewn(c->u.a.totallen,
						       unsigned char);
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
	    } else if (ssh->pktin.type == SSH2_MSG_CHANNEL_EOF) {
		unsigned i = ssh2_pkt_getuint32(ssh);
		struct ssh_channel *c;

		c = find234(ssh->channels, &i, ssh_channelfind);
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
	    } else if (ssh->pktin.type == SSH2_MSG_CHANNEL_CLOSE) {
		unsigned i = ssh2_pkt_getuint32(ssh);
		struct ssh_channel *c;

		c = find234(ssh->channels, &i, ssh_channelfind);
		if (!c || ((int)c->remoteid) == -1) {
		    bombout(("Received CHANNEL_CLOSE for %s channel %d\n",
			     c ? "half-open" : "nonexistent", i));
		    crStopV;
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
		    ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_CLOSE);
		    ssh2_pkt_adduint32(ssh, c->remoteid);
		    ssh2_pkt_send(ssh);
		}
		del234(ssh->channels, c);
		bufchain_clear(&c->v.v2.outbuffer);
		sfree(c);

		/*
		 * See if that was the last channel left open.
		 */
		if (count234(ssh->channels) == 0) {
		    logevent("All channels closed. Disconnecting");
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
		    ssh2_pkt_init(ssh, SSH2_MSG_DISCONNECT);
		    ssh2_pkt_adduint32(ssh, SSH2_DISCONNECT_BY_APPLICATION);
		    ssh2_pkt_addstring(ssh, "All open channels closed");
		    ssh2_pkt_addstring(ssh, "en");	/* language tag */
		    ssh2_pkt_send(ssh);
#endif
                    ssh_closing((Plug)ssh, NULL, 0, 0);
		    crStopV;
		}
		continue;	       /* remote sends close; ignore (FIXME) */
	    } else if (ssh->pktin.type == SSH2_MSG_CHANNEL_WINDOW_ADJUST) {
		unsigned i = ssh2_pkt_getuint32(ssh);
		struct ssh_channel *c;
		c = find234(ssh->channels, &i, ssh_channelfind);
		if (!c || c->closes)
		    continue;	       /* nonexistent or closing channel */
		c->v.v2.remwindow += ssh2_pkt_getuint32(ssh);
		s->try_send = TRUE;
	    } else if (ssh->pktin.type == SSH2_MSG_CHANNEL_OPEN_CONFIRMATION) {
		unsigned i = ssh2_pkt_getuint32(ssh);
		struct ssh_channel *c;
		c = find234(ssh->channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		if (c->type != CHAN_SOCKDATA_DORMANT)
		    continue;	       /* dunno why they're confirming this */
		c->remoteid = ssh2_pkt_getuint32(ssh);
		c->type = CHAN_SOCKDATA;
		c->v.v2.remwindow = ssh2_pkt_getuint32(ssh);
		c->v.v2.remmaxpkt = ssh2_pkt_getuint32(ssh);
		if (c->u.pfd.s)
		    pfd_confirm(c->u.pfd.s);
		if (c->closes) {
		    /*
		     * We have a pending close on this channel,
		     * which we decided on before the server acked
		     * the channel open. So now we know the
		     * remoteid, we can close it again.
		     */
		    ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_CLOSE);
		    ssh2_pkt_adduint32(ssh, c->remoteid);
		    ssh2_pkt_send(ssh);
		}
	    } else if (ssh->pktin.type == SSH2_MSG_CHANNEL_OPEN_FAILURE) {
		unsigned i = ssh2_pkt_getuint32(ssh);
		struct ssh_channel *c;
		c = find234(ssh->channels, &i, ssh_channelfind);
		if (!c)
		    continue;	       /* nonexistent channel */
		if (c->type != CHAN_SOCKDATA_DORMANT)
		    continue;	       /* dunno why they're failing this */

		logevent("Forwarded connection refused by server");

		pfd_close(c->u.pfd.s);

		del234(ssh->channels, c);
		sfree(c);
	    } else if (ssh->pktin.type == SSH2_MSG_CHANNEL_REQUEST) {
 		unsigned localid;
		char *type;
		int typelen, want_reply;
		struct ssh_channel *c;

		localid = ssh2_pkt_getuint32(ssh);
		ssh2_pkt_getstring(ssh, &type, &typelen);
		want_reply = ssh2_pkt_getbool(ssh);

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
		    ssh2_pkt_init(ssh, SSH2_MSG_DISCONNECT);
		    ssh2_pkt_adduint32(ssh, SSH2_DISCONNECT_BY_APPLICATION);
		    ssh2_pkt_addstring(ssh, buf);
		    ssh2_pkt_addstring(ssh, "en");	/* language tag */
		    ssh2_pkt_send(ssh);
		    connection_fatal(ssh->frontend, "%s", buf);
                    ssh_closing((Plug)ssh, NULL, 0, 0);
		    crStopV;
		}

		/*
		 * Having got the channel number, we now look at
		 * the request type string to see if it's something
		 * we recognise.
		 */
		if (typelen == 11 && !memcmp(type, "exit-status", 11) &&
		    c == ssh->mainchan) {
		    /* We recognise "exit-status" on the primary channel. */
		    char buf[100];
		    ssh->exitcode = ssh2_pkt_getuint32(ssh);
		    sprintf(buf, "Server sent command exit status %d",
			    ssh->exitcode);
		    logevent(buf);
		    if (want_reply) {
			ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_SUCCESS);
			ssh2_pkt_adduint32(ssh, c->remoteid);
			ssh2_pkt_send(ssh);
		    }
		} else {
		    /*
		     * This is a channel request we don't know
		     * about, so we now either ignore the request
		     * or respond with CHANNEL_FAILURE, depending
		     * on want_reply.
		     */
		    if (want_reply) {
			ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_FAILURE);
			ssh2_pkt_adduint32(ssh, c->remoteid);
			ssh2_pkt_send(ssh);
		    }
		}
	    } else if (ssh->pktin.type == SSH2_MSG_GLOBAL_REQUEST) {
		char *type;
		int typelen, want_reply;

		ssh2_pkt_getstring(ssh, &type, &typelen);
		want_reply = ssh2_pkt_getbool(ssh);

                /*
                 * We currently don't support any global requests
                 * at all, so we either ignore the request or
                 * respond with REQUEST_FAILURE, depending on
                 * want_reply.
                 */
                if (want_reply) {
                    ssh2_pkt_init(ssh, SSH2_MSG_REQUEST_FAILURE);
                    ssh2_pkt_send(ssh);
		}
	    } else if (ssh->pktin.type == SSH2_MSG_CHANNEL_OPEN) {
		char *type;
		int typelen;
		char *peeraddr;
		int peeraddrlen;
		int peerport;
		char *error = NULL;
		struct ssh_channel *c;
		unsigned remid, winsize, pktsize;
		ssh2_pkt_getstring(ssh, &type, &typelen);
		c = snew(struct ssh_channel);
		c->ssh = ssh;

		remid = ssh2_pkt_getuint32(ssh);
		winsize = ssh2_pkt_getuint32(ssh);
		pktsize = ssh2_pkt_getuint32(ssh);

		if (typelen == 3 && !memcmp(type, "x11", 3)) {
		    char *addrstr;

                    ssh2_pkt_getstring(ssh, &peeraddr, &peeraddrlen);
		    addrstr = snewn(peeraddrlen+1, char);
		    memcpy(addrstr, peeraddr, peeraddrlen);
		    peeraddr[peeraddrlen] = '\0';
                    peerport = ssh2_pkt_getuint32(ssh);

		    if (!ssh->X11_fwd_enabled)
			error = "X11 forwarding is not enabled";
		    else if (x11_init(&c->u.x11.s, ssh->cfg.x11_display, c,
				      ssh->x11auth, addrstr, peerport,
				      &ssh->cfg) != NULL) {
			error = "Unable to open an X11 connection";
		    } else {
			c->type = CHAN_X11;
		    }

		    sfree(addrstr);
		} else if (typelen == 15 &&
			   !memcmp(type, "forwarded-tcpip", 15)) {
		    struct ssh_rportfwd pf, *realpf;
		    char *dummy;
		    int dummylen;
		    ssh2_pkt_getstring(ssh, &dummy, &dummylen);/* skip address */
		    pf.sport = ssh2_pkt_getuint32(ssh);
                    ssh2_pkt_getstring(ssh, &peeraddr, &peeraddrlen);
                    peerport = ssh2_pkt_getuint32(ssh);
		    realpf = find234(ssh->rportfwds, &pf, NULL);
		    if (realpf == NULL) {
			error = "Remote port is not recognised";
		    } else {
			const char *e = pfd_newconnect(&c->u.pfd.s,
						       realpf->dhost,
						       realpf->dport, c,
						       &ssh->cfg);
			logeventf(ssh, "Received remote port open request"
				  " for %s:%d", realpf->dhost, realpf->dport);
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
		if (error) {
		    ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_OPEN_FAILURE);
		    ssh2_pkt_adduint32(ssh, c->remoteid);
		    ssh2_pkt_adduint32(ssh, SSH2_OPEN_CONNECT_FAILED);
		    ssh2_pkt_addstring(ssh, error);
		    ssh2_pkt_addstring(ssh, "en");	/* language tag */
		    ssh2_pkt_send(ssh);
		    sfree(c);
		} else {
		    c->localid = alloc_channel_id(ssh);
		    c->closes = 0;
		    c->v.v2.locwindow = OUR_V2_WINSIZE;
		    c->v.v2.remwindow = winsize;
		    c->v.v2.remmaxpkt = pktsize;
		    bufchain_init(&c->v.v2.outbuffer);
		    add234(ssh->channels, c);
		    ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_OPEN_CONFIRMATION);
		    ssh2_pkt_adduint32(ssh, c->remoteid);
		    ssh2_pkt_adduint32(ssh, c->localid);
		    ssh2_pkt_adduint32(ssh, c->v.v2.locwindow);
		    ssh2_pkt_adduint32(ssh, 0x4000UL);	/* our max pkt size */
		    ssh2_pkt_send(ssh);
		}
	    } else {
		bombout(("Strange packet received: type %d", ssh->pktin.type));
		crStopV;
	    }
	} else {
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
	    for (i = 0; NULL != (c = index234(ssh->channels, i)); i++) {
		int bufsize;
		if (c->closes)
		    continue;	       /* don't send on closing channels */
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
	}
    }

    crFinishV;
}

/*
 * Handle the top-level SSH2 protocol.
 */
static void ssh2_protocol(Ssh ssh, unsigned char *in, int inlen, int ispkt)
{
    if (do_ssh2_transport(ssh, in, inlen, ispkt) == 0)
	return;
    do_ssh2_authconn(ssh, in, inlen, ispkt);
}

/*
 * Called to set up the connection.
 *
 * Returns an error message, or NULL on success.
 */
static const char *ssh_init(void *frontend_handle, void **backend_handle,
			    Config *cfg,
			    char *host, int port, char **realhost, int nodelay)
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
    ssh->state = SSH_STATE_PREPACKET;
    ssh->size_needed = FALSE;
    ssh->eof_needed = FALSE;
    ssh->ldisc = NULL;
    ssh->logctx = NULL;
    {
	static const struct Packet empty = { 0, 0, NULL, NULL, 0 };
	ssh->pktin = ssh->pktout = empty;
    }
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
    ssh->ssh1_protocol_crstate = 0;
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

    ssh->send_ok = 0;
    ssh->editing = 0;
    ssh->echoing = 0;
    ssh->v1_throttle_count = 0;
    ssh->overall_bufsize = 0;
    ssh->fallback_cmd = 0;

    ssh->protocol = NULL;

    p = connect_to_host(ssh, host, port, realhost, nodelay);
    if (p != NULL)
	return p;

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
    }

    if (ssh->rportfwds) {
	while ((pf = delpos234(ssh->rportfwds, 0)) != NULL)
	    sfree(pf);
	freetree234(ssh->rportfwds);
    }
    sfree(ssh->deferred_send_data);
    if (ssh->x11auth)
	x11_free_auth(ssh->x11auth);
    sfree(ssh->do_ssh_init_state);
    sfree(ssh->do_ssh1_login_state);
    sfree(ssh->do_ssh2_transport_state);
    sfree(ssh->do_ssh2_authconn_state);
    if (ssh->pktout.data) {
	sfree(ssh->pktout.data);
	ssh->pktout.data = NULL;
    }
    if (ssh->pktin.data) {
	sfree(ssh->pktin.data);
	ssh->pktin.data = NULL;
    }
    if (ssh->crcda_ctx) {
	crcda_free_context(ssh->crcda_ctx);
	ssh->crcda_ctx = NULL;
    }
    if (ssh->logctx) {
	log_free(ssh->logctx);
	ssh->logctx = NULL;
    }
    if (ssh->s)
	ssh_do_close(ssh);
    sfree(ssh);
}

/*
 * Reconfigure the SSH backend.
 * 
 * Currently, this function does nothing very useful. In future,
 * however, we could do some handy things with it. For example, we
 * could make the port forwarding configurer active in the Change
 * Settings box, and this routine could close down existing
 * forwardings and open up new ones in response to changes.
 */
static void ssh_reconfig(void *handle, Config *cfg)
{
    Ssh ssh = (Ssh) handle;
    ssh->cfg = *cfg;		       /* STRUCTURE COPY */
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
	    } else {
		ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_REQUEST);
		ssh2_pkt_adduint32(ssh, ssh->mainchan->remoteid);
		ssh2_pkt_addstring(ssh, "window-change");
		ssh2_pkt_addbool(ssh, 0);
		ssh2_pkt_adduint32(ssh, ssh->term_width);
		ssh2_pkt_adduint32(ssh, ssh->term_height);
		ssh2_pkt_adduint32(ssh, 0);
		ssh2_pkt_adduint32(ssh, 0);
		ssh2_pkt_send(ssh);
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
    Ssh ssh = (Ssh) handle;

    if (ssh->version == 1) {
	static const struct telnet_special ssh1_specials[] = {
	    {"IGNORE message", TS_NOP},
	    {NULL, 0}
	};
	return ssh1_specials;
    } else if (ssh->version == 2) {
	static const struct telnet_special ssh2_specials[] = {
	    {"Break", TS_BRK},
	    {"IGNORE message", TS_NOP},
	    {NULL, 0}
	};
	return ssh2_specials;
    } else
	return NULL;
}

/*
 * Send Telnet special codes. TS_EOF is useful for `plink', so you
 * can send an EOF and collect resulting output (e.g. `plink
 * hostname sort').
 */
static void ssh_special(void *handle, Telnet_Special code)
{
    Ssh ssh = (Ssh) handle;

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
	} else {
	    ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_EOF);
	    ssh2_pkt_adduint32(ssh, ssh->mainchan->remoteid);
	    ssh2_pkt_send(ssh);
	}
	logevent("Sent EOF message");
    } else if (code == TS_PING || code == TS_NOP) {
	if (ssh->state == SSH_STATE_CLOSED
	    || ssh->state == SSH_STATE_PREPACKET) return;
	if (ssh->version == 1) {
	    if (!(ssh->remote_bugs & BUG_CHOKES_ON_SSH1_IGNORE))
		send_packet(ssh, SSH1_MSG_IGNORE, PKT_STR, "", PKT_END);
	} else {
	    ssh2_pkt_init(ssh, SSH2_MSG_IGNORE);
	    ssh2_pkt_addstring_start(ssh);
	    ssh2_pkt_send(ssh);
	}
    } else if (code == TS_BRK) {
	if (ssh->state == SSH_STATE_CLOSED
	    || ssh->state == SSH_STATE_PREPACKET) return;
	if (ssh->version == 1) {
	    logevent("Unable to send BREAK signal in SSH1");
	} else {
	    ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_REQUEST);
	    ssh2_pkt_adduint32(ssh, ssh->mainchan->remoteid);
	    ssh2_pkt_addstring(ssh, "break");
	    ssh2_pkt_addbool(ssh, 0);
	    ssh2_pkt_adduint32(ssh, 0);   /* default break length */
	    ssh2_pkt_send(ssh);
	}
    } else {
	/* do nothing */
    }
}

void *new_sock_channel(void *handle, Socket s)
{
    Ssh ssh = (Ssh) handle;
    struct ssh_channel *c;
    c = snew(struct ssh_channel);
    c->ssh = ssh;

    if (c) {
	c->remoteid = -1;	       /* to be set when open confirmed */
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

    logeventf(ssh, "Opening forwarded connection to %s:%d", hostname, port);

    if (ssh->version == 1) {
	send_packet(ssh, SSH1_MSG_PORT_OPEN,
		    PKT_INT, c->localid,
		    PKT_STR, hostname,
		    PKT_INT, port,
		    //PKT_STR, <org:orgport>,
		    PKT_END);
    } else {
	ssh2_pkt_init(ssh, SSH2_MSG_CHANNEL_OPEN);
	ssh2_pkt_addstring(ssh, "direct-tcpip");
	ssh2_pkt_adduint32(ssh, c->localid);
	c->v.v2.locwindow = OUR_V2_WINSIZE;
	ssh2_pkt_adduint32(ssh, c->v.v2.locwindow);/* our window size */
	ssh2_pkt_adduint32(ssh, 0x4000UL);      /* our max pkt size */
	ssh2_pkt_addstring(ssh, hostname);
	ssh2_pkt_adduint32(ssh, port);
	/*
	 * We make up values for the originator data; partly it's
	 * too much hassle to keep track, and partly I'm not
	 * convinced the server should be told details like that
	 * about my local network configuration.
	 */
	ssh2_pkt_addstring(ssh, "client-side-connection");
	ssh2_pkt_adduint32(ssh, 0);
	ssh2_pkt_send(ssh);
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
    22
};
