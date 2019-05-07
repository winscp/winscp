struct ssh1_channel;

struct outstanding_succfail;

struct ssh1_connection_state {
    int crState;

    Ssh *ssh;

    Conf *conf;
    int local_protoflags, remote_protoflags;

    tree234 *channels;		       /* indexed by local id */

    /* In SSH-1, the main session doesn't take the form of a 'channel'
     * according to the wire protocol. But we want to use the same API
     * for it, so we define an SshChannel here - but one that uses a
     * separate vtable from the usual one, so it doesn't map to a
     * struct ssh1_channel as all the others do. */
    SshChannel mainchan_sc;
    Channel *mainchan_chan;            /* the other end of mainchan_sc */
    mainchan *mainchan;                /* and its subtype */

    bool got_pty;
    bool ldisc_opts[LD_N_OPTIONS];
    bool stdout_throttling;
    bool want_user_input;
    bool session_terminated;
    int term_width, term_height, term_width_orig, term_height_orig;

    bool X11_fwd_enabled;
    struct X11Display *x11disp;
    struct X11FakeAuth *x11auth;
    tree234 *x11authtree;

    bool agent_fwd_enabled;

    tree234 *rportfwds;
    PortFwdManager *portfwdmgr;
    bool portfwdmgr_configured;

    bool finished_setup;

    /*
     * These store the list of requests that we're waiting for
     * SSH_SMSG_{SUCCESS,FAILURE} replies to. (Those messages don't
     * come with any indication of what they're in response to, so we
     * have to keep track of the queue ourselves.)
     */
    struct outstanding_succfail *succfail_head, *succfail_tail;

    bool compressing;                  /* used in server mode only */
    bool sent_exit_status;             /* also for server mode */

    ConnectionLayer cl;
    PacketProtocolLayer ppl;
};

struct ssh1_channel {
    struct ssh1_connection_state *connlayer;

    unsigned remoteid, localid;
    int type;
    /* True if we opened this channel but server hasn't confirmed. */
    bool halfopen;

    /* Bitmap of whether we've sent/received CHANNEL_CLOSE and
     * CHANNEL_CLOSE_CONFIRMATION. */
#define CLOSES_SENT_CLOSE      1
#define CLOSES_SENT_CLOSECONF  2
#define CLOSES_RCVD_CLOSE      4
#define CLOSES_RCVD_CLOSECONF  8
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

    Channel *chan;      /* handle the client side of this channel, if not */
    SshChannel sc;      /* entry point for chan to talk back to */
};

SshChannel *ssh1_session_open(ConnectionLayer *cl, Channel *chan);
void ssh1_channel_init(struct ssh1_channel *c);
void ssh1_channel_free(struct ssh1_channel *c);
struct ssh_rportfwd *ssh1_rportfwd_alloc(
    ConnectionLayer *cl,
    const char *shost, int sport, const char *dhost, int dport,
    int addressfamily, const char *log_description, PortFwdRecord *pfr,
    ssh_sharing_connstate *share_ctx);
SshChannel *ssh1_serverside_x11_open(
    ConnectionLayer *cl, Channel *chan, const SocketPeerInfo *pi);
SshChannel *ssh1_serverside_agent_open(ConnectionLayer *cl, Channel *chan);

void ssh1_connection_direction_specific_setup(
    struct ssh1_connection_state *s);
bool ssh1_handle_direction_specific_packet(
    struct ssh1_connection_state *s, PktIn *pktin);

bool ssh1_check_termination(struct ssh1_connection_state *s);
