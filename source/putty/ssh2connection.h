#ifndef PUTTY_SSH2CONNECTION_H
#define PUTTY_SSH2CONNECTION_H

struct outstanding_channel_request;
struct outstanding_global_request;

struct ssh2_connection_state {
    int crState;

    Ssh *ssh;

    ssh_sharing_state *connshare;
    char *peer_verstring;

    mainchan *mainchan;
    SshChannel *mainchan_sc;
    bool ldisc_opts[LD_N_OPTIONS];
    int session_attempt, session_status;
    int term_width, term_height;
    bool want_user_input;

    bool ssh_is_simple;
    bool persistent;

    Conf *conf;

    tree234 *channels;		       /* indexed by local id */
    bool all_channels_throttled;

    bool X11_fwd_enabled;
    tree234 *x11authtree;

    bool got_pty;
    bool agent_fwd_enabled;

    tree234 *rportfwds;
    PortFwdManager *portfwdmgr;
    bool portfwdmgr_configured;

    prompts_t *antispoof_prompt;
    int antispoof_ret;

    const SftpServerVtable *sftpserver_vt;

    /*
     * These store the list of global requests that we're waiting for
     * replies to. (REQUEST_FAILURE doesn't come with any indication
     * of what message caused it, so we have to keep track of the
     * queue ourselves.)
     */
    struct outstanding_global_request *globreq_head, *globreq_tail;

    ConnectionLayer cl;
    PacketProtocolLayer ppl;
};

typedef void (*gr_handler_fn_t)(struct ssh2_connection_state *s,
                                PktIn *pktin, void *ctx);
void ssh2_queue_global_request_handler(
    struct ssh2_connection_state *s, gr_handler_fn_t handler, void *ctx);

struct ssh2_channel {
    struct ssh2_connection_state *connlayer;

    unsigned remoteid, localid;
    int type;
    /* True if we opened this channel but server hasn't confirmed. */
    bool halfopen;

    /* Bitmap of whether we've sent/received CHANNEL_EOF and
     * CHANNEL_CLOSE. */
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
    bool pending_eof;

    /*
     * True if this channel is causing the underlying connection to be
     * throttled.
     */
    bool throttling_conn;

    /*
     * True if we currently have backed-up data on the direction of
     * this channel pointing out of the SSH connection, and therefore
     * would prefer the 'Channel' implementation not to read further
     * local input if possible.
     */
    bool throttled_by_backlog;

    bufchain outbuffer, errbuffer;
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
     * These store the list of channel requests that we're waiting for
     * replies to. (CHANNEL_FAILURE doesn't come with any indication
     * of what message caused it, so we have to keep track of the
     * queue ourselves.)
     */
    struct outstanding_channel_request *chanreq_head, *chanreq_tail;

    enum { THROTTLED, UNTHROTTLING, UNTHROTTLED } throttle_state;

    ssh_sharing_connstate *sharectx; /* sharing context, if this is a
                                      * downstream channel */
    Channel *chan;      /* handle the client side of this channel, if not */
    SshChannel sc;      /* entry point for chan to talk back to */
};

typedef void (*cr_handler_fn_t)(struct ssh2_channel *, PktIn *, void *);

void ssh2_channel_init(struct ssh2_channel *c);
PktOut *ssh2_chanreq_init(struct ssh2_channel *c, const char *type,
                          cr_handler_fn_t handler, void *ctx);

typedef enum ChanopenOutcome {
    CHANOPEN_RESULT_FAILURE,
    CHANOPEN_RESULT_SUCCESS,
    CHANOPEN_RESULT_DOWNSTREAM,
} ChanopenOutcome;

typedef struct ChanopenResult {
    ChanopenOutcome outcome;
    union {
        struct {
            char *wire_message;        /* must be freed by recipient */
            unsigned reason_code;
        } failure;
        struct {
            Channel *channel;
        } success;
        struct {
            ssh_sharing_connstate *share_ctx;
        } downstream;
    } u;
} ChanopenResult;

PktOut *ssh2_chanopen_init(struct ssh2_channel *c, const char *type);

PktOut *ssh2_portfwd_chanopen(
    struct ssh2_connection_state *s, struct ssh2_channel *c,
    const char *hostname, int port,
    const char *description, const SocketPeerInfo *peerinfo);

struct ssh_rportfwd *ssh2_rportfwd_alloc(
    ConnectionLayer *cl,
    const char *shost, int sport, const char *dhost, int dport,
    int addressfamily, const char *log_description, PortFwdRecord *pfr,
    ssh_sharing_connstate *share_ctx);
void ssh2_rportfwd_remove(
    ConnectionLayer *cl, struct ssh_rportfwd *rpf);
SshChannel *ssh2_session_open(ConnectionLayer *cl, Channel *chan);
SshChannel *ssh2_serverside_x11_open(
    ConnectionLayer *cl, Channel *chan, const SocketPeerInfo *pi);
SshChannel *ssh2_serverside_agent_open(ConnectionLayer *cl, Channel *chan);

void ssh2channel_send_exit_status(SshChannel *c, int status);
void ssh2channel_send_exit_signal(
    SshChannel *c, ptrlen signame, bool core_dumped, ptrlen msg);
void ssh2channel_send_exit_signal_numeric(
    SshChannel *c, int signum, bool core_dumped, ptrlen msg);
void ssh2channel_request_x11_forwarding(
    SshChannel *c, bool want_reply, const char *authproto,
    const char *authdata, int screen_number, bool oneshot);
void ssh2channel_request_agent_forwarding(SshChannel *c, bool want_reply);
void ssh2channel_request_pty(
    SshChannel *c, bool want_reply, Conf *conf, int w, int h);
bool ssh2channel_send_env_var(
    SshChannel *c, bool want_reply, const char *var, const char *value);
void ssh2channel_start_shell(SshChannel *c, bool want_reply);
void ssh2channel_start_command(
    SshChannel *c, bool want_reply, const char *command);
bool ssh2channel_start_subsystem(
    SshChannel *c, bool want_reply, const char *subsystem);
bool ssh2channel_send_env_var(
    SshChannel *c, bool want_reply, const char *var, const char *value);
bool ssh2channel_send_serial_break(
    SshChannel *c, bool want_reply, int length);
bool ssh2channel_send_signal(
    SshChannel *c, bool want_reply, const char *signame);
void ssh2channel_send_terminal_size_change(SshChannel *c, int w, int h);

#define CHANOPEN_RETURN_FAILURE(code, msgparams) do             \
    {                                                           \
        ChanopenResult toret;                                   \
        toret.outcome = CHANOPEN_RESULT_FAILURE;                \
        toret.u.failure.reason_code = code;                     \
        toret.u.failure.wire_message = dupprintf msgparams;     \
        return toret;                                           \
    } while (0)

#define CHANOPEN_RETURN_SUCCESS(chan) do                \
    {                                                   \
        ChanopenResult toret;                           \
        toret.outcome = CHANOPEN_RESULT_SUCCESS;        \
        toret.u.success.channel = chan;                 \
        return toret;                                   \
    } while (0)

#define CHANOPEN_RETURN_DOWNSTREAM(shctx) do            \
    {                                                   \
        ChanopenResult toret;                           \
        toret.outcome = CHANOPEN_RESULT_DOWNSTREAM;     \
        toret.u.downstream.share_ctx = shctx;           \
        return toret;                                   \
    } while (0)

ChanopenResult ssh2_connection_parse_channel_open(
    struct ssh2_connection_state *s, ptrlen type,
    PktIn *pktin, SshChannel *sc);

bool ssh2_connection_parse_global_request(
    struct ssh2_connection_state *s, ptrlen type, PktIn *pktin);

bool ssh2_connection_need_antispoof_prompt(struct ssh2_connection_state *s);

#endif /* PUTTY_SSH2CONNECTION_H */
