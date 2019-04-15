/*
 * Abstraction of the various layers of SSH packet-level protocol,
 * general enough to take in all three of the main SSH-2 layers and
 * both of the SSH-1 phases.
 */

#ifndef PUTTY_SSHPPL_H
#define PUTTY_SSHPPL_H

typedef void (*packet_handler_fn_t)(PacketProtocolLayer *ppl, PktIn *pktin);
typedef void (*add_special_fn_t)(
    void *ctx, const char *text, SessionSpecialCode code, int arg);

struct PacketProtocolLayerVtable {
    void (*free)(PacketProtocolLayer *); 
    void (*process_queue)(PacketProtocolLayer *ppl);
    int (*get_specials)(
        PacketProtocolLayer *ppl, add_special_fn_t add_special, void *ctx);
    void (*special_cmd)(
        PacketProtocolLayer *ppl, SessionSpecialCode code, int arg);
    int (*want_user_input)(PacketProtocolLayer *ppl);
    void (*got_user_input)(PacketProtocolLayer *ppl);
    void (*reconfigure)(PacketProtocolLayer *ppl, Conf *conf);

    /* Protocol-level name of this layer. */
    const char *name;
};

struct PacketProtocolLayer {
    const struct PacketProtocolLayerVtable *vt;

    /* Link to the underlying SSH BPP. */
    BinaryPacketProtocol *bpp;

    /* Queue from which the layer receives its input packets, and one
     * to put its output packets on. */
    PktInQueue *in_pq;
    PktOutQueue *out_pq;

    /* Idempotent callback that in_pq will be linked to, causing a
     * call to the process_queue method. in_pq points to this, so it
     * will be automatically triggered by pushing things on the
     * layer's input queue, but it can also be triggered on purpose. */
    IdempotentCallback ic_process_queue;

    /* Owner's pointer to this layer. Permits a layer to unilaterally
     * abdicate in favour of a replacement, by overwriting this
     * pointer and then freeing itself. */
    PacketProtocolLayer **selfptr;

    /* Bufchain of keyboard input from the user, for login prompts and
     * similar. */
    bufchain *user_input;

    /* Logging and error-reporting facilities. */
    LogContext *logctx;
    Seat *seat;             /* for dialog boxes, session output etc */
    Ssh *ssh;   /* for session termination + assorted connection-layer ops */

    /* Known bugs in the remote implementation. */
    unsigned remote_bugs;
};

#define ssh_ppl_process_queue(ppl) ((ppl)->vt->process_queue(ppl))
#define ssh_ppl_get_specials(ppl, add, ctx) \
    ((ppl)->vt->get_specials(ppl, add, ctx))
#define ssh_ppl_special_cmd(ppl, code, arg) \
    ((ppl)->vt->special_cmd(ppl, code, arg))
#define ssh_ppl_want_user_input(ppl) ((ppl)->vt->want_user_input(ppl))
#define ssh_ppl_got_user_input(ppl) ((ppl)->vt->got_user_input(ppl))
#define ssh_ppl_reconfigure(ppl, conf) ((ppl)->vt->reconfigure(ppl, conf))

/* ssh_ppl_free is more than just a macro wrapper on the vtable; it
 * does centralised parts of the freeing too. */
void ssh_ppl_free(PacketProtocolLayer *ppl);

/* Helper routine to point a PPL at its input and output queues. Also
 * sets up the IdempotentCallback on the input queue to trigger a call
 * to process_queue whenever packets are added to it. */
void ssh_ppl_setup_queues(PacketProtocolLayer *ppl,
                          PktInQueue *inq, PktOutQueue *outq);

/* Routine a PPL can call to abdicate in favour of a replacement, by
 * overwriting ppl->selfptr. Has the side effect of freeing 'old', so
 * if 'old' actually called this (which is likely) then it should
 * avoid dereferencing itself on return from this function! */
void ssh_ppl_replace(PacketProtocolLayer *old, PacketProtocolLayer *new);

PacketProtocolLayer *ssh1_login_new(
    Conf *conf, const char *host, int port,
    PacketProtocolLayer *successor_layer);
PacketProtocolLayer *ssh1_connection_new(
    Ssh *ssh, Conf *conf, ConnectionLayer **cl_out);

struct DataTransferStats;
struct ssh_connection_shared_gss_state;
PacketProtocolLayer *ssh2_transport_new(
    Conf *conf, const char *host, int port, const char *fullhostname,
    const char *client_greeting, const char *server_greeting,
    struct ssh_connection_shared_gss_state *shgss,
    struct DataTransferStats *stats,
    PacketProtocolLayer *higher_layer);
PacketProtocolLayer *ssh2_userauth_new(
    PacketProtocolLayer *successor_layer,
    const char *hostname, const char *fullhostname,
    Filename *keyfile, int tryagent,
    const char *default_username, int change_username,
    int try_ki_auth,
    int try_gssapi_auth, int try_gssapi_kex_auth,
    int gssapi_fwd, struct ssh_connection_shared_gss_state *shgss);
PacketProtocolLayer *ssh2_connection_new(
    Ssh *ssh, ssh_sharing_state *connshare, int is_simple,
    Conf *conf, const char *peer_verstring, ConnectionLayer **cl_out);

/* Can't put this in the userauth constructor without having a
 * dependency loop at setup time (transport and userauth can't _both_
 * be constructed second and given a pointer to the other). */
void ssh2_userauth_set_transport_layer(PacketProtocolLayer *userauth,
                                       PacketProtocolLayer *transport);

/* Convenience macro for protocol layers to send formatted strings to
 * the Event Log. Assumes a function parameter called 'ppl' is in
 * scope, and takes a double pair of parens because it passes a whole
 * argument list to dupprintf. */
#define ppl_logevent(params) ( \
        logevent_and_free((ppl)->logctx, dupprintf params))

/* Convenience macro for protocol layers to send formatted strings to
 * the terminal. Also expects 'ppl' to be in scope and takes double
 * parens. */
#define ppl_printf(params) \
    ssh_ppl_user_output_string_and_free(ppl, dupprintf params)
void ssh_ppl_user_output_string_and_free(PacketProtocolLayer *ppl, char *text);

/* Methods for userauth to communicate back to the transport layer */
ptrlen ssh2_transport_get_session_id(PacketProtocolLayer *ssh2_transport_ptr);
void ssh2_transport_notify_auth_done(PacketProtocolLayer *ssh2_transport_ptr);

/* Methods for ssh1login to pass protocol flags to ssh1connection */
void ssh1_connection_set_local_protoflags(PacketProtocolLayer *ppl, int flags);

/* Shared get_specials method between the two ssh1 layers */
int ssh1_common_get_specials(PacketProtocolLayer *, add_special_fn_t, void *);

#endif /* PUTTY_SSHPPL_H */
