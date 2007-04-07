/*
 * sshint.h: internal functions and types declared in ssh.c and
 * exported to other modules which are conceptually an integral
 * part of the SSH back end.
 */

#ifndef PUTTY_SSHINT_H
#define PUTTY_SSHINT_H

#include "ssh.h"
#include "tree234.h"

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

typedef struct ssh_tag *Ssh;

struct Packet {
    long length;
    long forcepad; /* Force padding to at least this length */
    int type;
    unsigned long sequence;
    unsigned char *data;
    unsigned char *body;
    long savedpos;
    long maxlen;
    long encrypted_len;		       /* for SSH2 total-size counting */

    /*
     * State associated with packet logging
     */
    int logmode;
    int nblanks;
    struct logblank_t *blanks;
};

struct rdpkt1_state_tag {
    long len, pad, biglen, to_read;
    unsigned long realcrc, gotcrc;
    unsigned char *p;
    int i;
    int chunk;
    struct Packet *pktin;
};

struct rdpkt2_state_tag {
    long len, pad, payload, packetlen, maclen;
    int i;
    int cipherblk;
    unsigned long incoming_sequence;
    struct Packet *pktin;
};

typedef void (*handler_fn_t)(Ssh ssh, struct Packet *pktin);
typedef void (*chandler_fn_t)(Ssh ssh, struct Packet *pktin, void *ctx);

struct queued_handler;

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

    int ospeed, ispeed;		       /* temporaries */
    int term_width, term_height;

    tree234 *channels;		       /* indexed by local id */
    struct ssh_channel *mainchan;      /* primary session channel */
    int exitcode;
    int close_expected;

    tree234 *rportfwds, *portfwds;

    enum {
	SSH_STATE_PREPACKET,
	SSH_STATE_BEFORE_SIZE,
	SSH_STATE_INTERMED,
	SSH_STATE_SESSION,
	SSH_STATE_CLOSED
    } state;

    int size_needed, eof_needed;

    struct Packet **queue;
    int queuelen, queuesize;
    int queueing;
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

    int pkt_ctx;

    void *x11auth;

    int version;
    int v1_throttle_count;
    int overall_bufsize;
    int throttled_all;
    int v1_stdout_throttling;
    unsigned long v2_outgoing_sequence;

    int ssh1_rdpkt_crstate;
    int ssh2_rdpkt_crstate;
    int do_ssh_init_crstate;
    int ssh_gotdata_crstate;
    int do_ssh1_login_crstate;
    int do_ssh1_connection_crstate;
    int do_ssh2_transport_crstate;
    int do_ssh2_authconn_crstate;

    void *do_ssh_init_state;
    void *do_ssh1_login_state;
    void *do_ssh2_transport_state;
    void *do_ssh2_authconn_state;

    struct rdpkt1_state_tag rdpkt1_state;
    struct rdpkt2_state_tag rdpkt2_state;

    /* ssh1 and ssh2 use this for different things, but both use it */
    int protocol_initial_phase_done;

    void (*protocol) (Ssh ssh, void *vin, int inlen,
		      struct Packet *pkt);
    struct Packet *(*s_rdpkt) (Ssh ssh, unsigned char **data, int *datalen);

    /*
     * We maintain a full _copy_ of a Config structure here, not
     * merely a pointer to it. That way, when we're passed a new
     * one for reconfiguration, we can check the differences and
     * potentially reconfigure port forwardings etc in mid-session.
     */
    Config cfg;

    /*
     * Used to transfer data back from async callbacks.
     */
    void *agent_response;
    int agent_response_len;
    int user_response;

    /*
     * The SSH connection can be set as `frozen', meaning we are
     * not currently accepting incoming data from the network. This
     * is slightly more serious than setting the _socket_ as
     * frozen, because we may already have had data passed to us
     * from the network which we need to delay processing until
     * after the freeze is lifted, so we also need a bufchain to
     * store that data.
     */
    int frozen;
    bufchain queued_incoming_data;

    /*
     * Dispatch table for packet types that we may have to deal
     * with at any time.
     */
    handler_fn_t packet_dispatch[256];

    /*
     * Queues of one-off handler functions for success/failure
     * indications from a request.
     */
    struct queued_handler *qhead, *qtail;

    /*
     * This module deals with sending keepalives.
     */
    Pinger pinger;

    /*
     * Track incoming and outgoing data sizes and time, for
     * size-based rekeys.
     */
    unsigned long incoming_data_size, outgoing_data_size, deferred_data_size;
    unsigned long max_data_size;
    int kex_in_progress;
    long next_rekey, last_rekey;
    char *deferred_rekey_reason;    /* points to STATIC string; don't free */
};

struct Packet *ssh2_pkt_init(int pkt_type);
void ssh2_pkt_addbool(struct Packet *, unsigned char value);
void ssh2_pkt_addbyte(struct Packet *pkt, unsigned char byte);
void ssh2_pkt_adduint32(struct Packet *, unsigned long value);
void ssh2_pkt_addstring_start(struct Packet *);
void ssh2_pkt_addstring_str(struct Packet *, char *data);
void ssh2_pkt_addstring_data(struct Packet *, char *data, int len);
void ssh2_pkt_addstring(struct Packet *, char *data);
void ssh2_pkt_addmp(struct Packet *, Bignum b);
void ssh2_pkt_send(Ssh, struct Packet *);
unsigned long ssh_pkt_getuint32(struct Packet *pkt);
int ssh2_pkt_getbool(struct Packet *pkt);
void ssh_pkt_getstring(struct Packet *pkt, char **p, int *length);

int ssh_do_close(Ssh ssh, int notify_exit);

void c_write(Ssh ssh, const char *buf, int len);
void c_write_untrusted(Ssh ssh, const char *buf, int len);
void c_write_str(Ssh ssh, const char *buf);

#define logevent(s) logevent(ssh->frontend, s)
void ssh_logeventf(Ssh ssh, const char *fmt, ...);
#define logeventf ssh_logeventf

#define bombout(msg) \
    do { \
        char *text = dupprintf msg; \
	ssh_do_close(ssh, FALSE); \
        logevent(text); \
        connection_fatal(ssh->frontend, "%s", text); \
        sfree(text); \
    } while (0)

#endif /* PUTTY_SSHINT_H */
