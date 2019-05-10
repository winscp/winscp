/*
 * Abstraction of the various ways to handle the local end of an SSH
 * connection-layer channel.
 */

#ifndef PUTTY_SSHCHAN_H
#define PUTTY_SSHCHAN_H

struct ChannelVtable {
    void (*free)(Channel *);

    /* Called for channel types that were created at the same time as
     * we sent an outgoing CHANNEL_OPEN, when the confirmation comes
     * back from the server indicating that the channel has been
     * opened, or the failure message indicating that it hasn't,
     * respectively. In the latter case, this must _not_ free the
     * Channel structure - the client will call the free method
     * separately. But it might do logging or other local cleanup. */
    void (*open_confirmation)(Channel *);
    void (*open_failed)(Channel *, const char *error_text);

    size_t (*send)(Channel *, bool is_stderr, const void *buf, size_t len);
    void (*send_eof)(Channel *);
    void (*set_input_wanted)(Channel *, bool wanted);

    char *(*log_close_msg)(Channel *);

    bool (*want_close)(Channel *, bool sent_local_eof, bool rcvd_remote_eof);

    /* A method for every channel request we know of. All of these
     * return true for success or false for failure. */
    bool (*rcvd_exit_status)(Channel *, int status);
    bool (*rcvd_exit_signal)(
        Channel *chan, ptrlen signame, bool core_dumped, ptrlen msg);
    bool (*rcvd_exit_signal_numeric)(
        Channel *chan, int signum, bool core_dumped, ptrlen msg);
    bool (*run_shell)(Channel *chan);
    bool (*run_command)(Channel *chan, ptrlen command);
    bool (*run_subsystem)(Channel *chan, ptrlen subsys);
    bool (*enable_x11_forwarding)(
        Channel *chan, bool oneshot, ptrlen authproto, ptrlen authdata,
        unsigned screen_number);
    bool (*enable_agent_forwarding)(Channel *chan);
    bool (*allocate_pty)(
        Channel *chan, ptrlen termtype, unsigned width, unsigned height,
        unsigned pixwidth, unsigned pixheight, struct ssh_ttymodes modes);
    bool (*set_env)(Channel *chan, ptrlen var, ptrlen value);
    bool (*send_break)(Channel *chan, unsigned length);
    bool (*send_signal)(Channel *chan, ptrlen signame);
    bool (*change_window_size)(
        Channel *chan, unsigned width, unsigned height,
        unsigned pixwidth, unsigned pixheight);

    /* A method for signalling success/failure responses to channel
     * requests initiated from the SshChannel vtable with want_reply
     * true. */
    void (*request_response)(Channel *, bool success);
};

struct Channel {
    const struct ChannelVtable *vt;
    unsigned initial_fixed_window_size;
};

static inline void chan_free(Channel *ch)
{ ch->vt->free(ch); }
static inline void chan_open_confirmation(Channel *ch)
{ ch->vt->open_confirmation(ch); }
static inline void chan_open_failed(Channel *ch, const char *err)
{ ch->vt->open_failed(ch, err); }
static inline size_t chan_send(
    Channel *ch, bool err, const void *buf, size_t len)
{ return ch->vt->send(ch, err, buf, len); }
static inline void chan_send_eof(Channel *ch)
{ ch->vt->send_eof(ch); }
static inline void chan_set_input_wanted(Channel *ch, bool wanted)
{ ch->vt->set_input_wanted(ch, wanted); }
static inline char *chan_log_close_msg(Channel *ch)
{ return ch->vt->log_close_msg(ch); }
static inline bool chan_want_close(Channel *ch, bool leof, bool reof)
{ return ch->vt->want_close(ch, leof, reof); }
static inline bool chan_rcvd_exit_status(Channel *ch, int status)
{ return ch->vt->rcvd_exit_status(ch, status); }
static inline bool chan_rcvd_exit_signal(
        Channel *ch, ptrlen sig, bool core, ptrlen msg)
{ return ch->vt->rcvd_exit_signal(ch, sig, core, msg); }
static inline bool chan_rcvd_exit_signal_numeric(
        Channel *ch, int sig, bool core, ptrlen msg)
{ return ch->vt->rcvd_exit_signal_numeric(ch, sig, core, msg); }
static inline bool chan_run_shell(Channel *ch)
{ return ch->vt->run_shell(ch); }
static inline bool chan_run_command(Channel *ch, ptrlen cmd)
{ return ch->vt->run_command(ch, cmd); }
static inline bool chan_run_subsystem(Channel *ch, ptrlen subsys)
{ return ch->vt->run_subsystem(ch, subsys); }
static inline bool chan_enable_x11_forwarding(
    Channel *ch, bool oneshot, ptrlen ap, ptrlen ad, unsigned scr)
{ return ch->vt->enable_x11_forwarding(ch, oneshot, ap, ad, scr); }
static inline bool chan_enable_agent_forwarding(Channel *ch)
{ return ch->vt->enable_agent_forwarding(ch); }
static inline bool chan_allocate_pty(
    Channel *ch, ptrlen termtype, unsigned w, unsigned h,
    unsigned pw, unsigned ph, struct ssh_ttymodes modes)
{ return ch->vt->allocate_pty(ch, termtype, w, h, pw, ph, modes); }
static inline bool chan_set_env(Channel *ch, ptrlen var, ptrlen value)
{ return ch->vt->set_env(ch, var, value); }
static inline bool chan_send_break(Channel *ch, unsigned length)
{ return ch->vt->send_break(ch, length); }
static inline bool chan_send_signal(Channel *ch, ptrlen signame)
{ return ch->vt->send_signal(ch, signame); }
static inline bool chan_change_window_size(
    Channel *ch, unsigned w, unsigned h, unsigned pw, unsigned ph)
{ return ch->vt->change_window_size(ch, w, h, pw, ph); }
static inline void chan_request_response(Channel *ch, bool success)
{ ch->vt->request_response(ch, success); }

/*
 * Reusable methods you can put in vtables to give default handling of
 * some of those functions.
 */

/* open_confirmation / open_failed for any channel it doesn't apply to */
void chan_remotely_opened_confirmation(Channel *chan);
void chan_remotely_opened_failure(Channel *chan, const char *errtext);

/* want_close for any channel that wants the default behaviour of not
 * closing until both directions have had an EOF */
bool chan_default_want_close(Channel *, bool, bool);

/* default implementations that refuse all the channel requests */
bool chan_no_exit_status(Channel *, int);
bool chan_no_exit_signal(Channel *, ptrlen, bool, ptrlen);
bool chan_no_exit_signal_numeric(Channel *, int, bool, ptrlen);
bool chan_no_run_shell(Channel *chan);
bool chan_no_run_command(Channel *chan, ptrlen command);
bool chan_no_run_subsystem(Channel *chan, ptrlen subsys);
bool chan_no_enable_x11_forwarding(
    Channel *chan, bool oneshot, ptrlen authproto, ptrlen authdata,
    unsigned screen_number);
bool chan_no_enable_agent_forwarding(Channel *chan);
bool chan_no_allocate_pty(
    Channel *chan, ptrlen termtype, unsigned width, unsigned height,
    unsigned pixwidth, unsigned pixheight, struct ssh_ttymodes modes);
bool chan_no_set_env(Channel *chan, ptrlen var, ptrlen value);
bool chan_no_send_break(Channel *chan, unsigned length);
bool chan_no_send_signal(Channel *chan, ptrlen signame);
bool chan_no_change_window_size(
    Channel *chan, unsigned width, unsigned height,
    unsigned pixwidth, unsigned pixheight);

/* default implementation that never expects to receive a response */
void chan_no_request_response(Channel *, bool);

/*
 * Constructor for a trivial do-nothing implementation of
 * ChannelVtable. Used for 'zombie' channels, i.e. channels whose
 * proper local source of data has been shut down or otherwise stopped
 * existing, but the SSH side is still there and needs some kind of a
 * Channel implementation to talk to. In particular, the want_close
 * method for this channel always returns 'yes, please close this
 * channel asap', regardless of whether local and/or remote EOF have
 * been sent - indeed, even if _neither_ has.
 */
Channel *zombiechan_new(void);

/* ----------------------------------------------------------------------
 * This structure is owned by an SSH connection layer, and identifies
 * the connection layer's end of the channel, for the Channel
 * implementation to talk back to.
 */

struct SshChannelVtable {
    size_t (*write)(SshChannel *c, bool is_stderr, const void *, size_t);
    void (*write_eof)(SshChannel *c);
    void (*initiate_close)(SshChannel *c, const char *err);
    void (*unthrottle)(SshChannel *c, size_t bufsize);
    Conf *(*get_conf)(SshChannel *c);
    void (*window_override_removed)(SshChannel *c);
    void (*x11_sharing_handover)(SshChannel *c,
                                 ssh_sharing_connstate *share_cs,
                                 share_channel *share_chan,
                                 const char *peer_addr, int peer_port,
                                 int endian, int protomajor, int protominor,
                                 const void *initial_data, int initial_len);

    /*
     * All the outgoing channel requests we support. Each one has a
     * want_reply flag, which will cause a callback to
     * chan_request_response when the result is available.
     *
     * The ones that return 'bool' use it to indicate that the SSH
     * protocol in use doesn't support this request at all.
     *
     * (It's also intentional that not all of them have a want_reply
     * flag: the ones that don't are because SSH-1 has no method for
     * signalling success or failure of that request, or because we
     * wouldn't do anything usefully different with the reply in any
     * case.)
     */
    void (*send_exit_status)(SshChannel *c, int status);
    void (*send_exit_signal)(
        SshChannel *c, ptrlen signame, bool core_dumped, ptrlen msg);
    void (*send_exit_signal_numeric)(
        SshChannel *c, int signum, bool core_dumped, ptrlen msg);
    void (*request_x11_forwarding)(
        SshChannel *c, bool want_reply, const char *authproto,
        const char *authdata, int screen_number, bool oneshot);
    void (*request_agent_forwarding)(
        SshChannel *c, bool want_reply);
    void (*request_pty)(
        SshChannel *c, bool want_reply, Conf *conf, int w, int h);
    bool (*send_env_var)(
        SshChannel *c, bool want_reply, const char *var, const char *value);
    void (*start_shell)(
        SshChannel *c, bool want_reply);
    void (*start_command)(
        SshChannel *c, bool want_reply, const char *command);
    bool (*start_subsystem)(
        SshChannel *c, bool want_reply, const char *subsystem);
    bool (*send_serial_break)(
        SshChannel *c, bool want_reply, int length); /* length=0 for default */
    bool (*send_signal)(
        SshChannel *c, bool want_reply, const char *signame);
    void (*send_terminal_size_change)(
        SshChannel *c, int w, int h);
    void (*hint_channel_is_simple)(SshChannel *c);
};

struct SshChannel {
    const struct SshChannelVtable *vt;
    ConnectionLayer *cl;
};

static inline size_t sshfwd_write_ext(
    SshChannel *c, bool is_stderr, const void *data, size_t len)
{ return c->vt->write(c, is_stderr, data, len); }
static inline size_t sshfwd_write(SshChannel *c, const void *data, size_t len)
{ return sshfwd_write_ext(c, false, data, len); }
static inline void sshfwd_write_eof(SshChannel *c)
{ c->vt->write_eof(c); }
static inline void sshfwd_initiate_close(SshChannel *c, const char *err)
{ c->vt->initiate_close(c, err); }
static inline void sshfwd_unthrottle(SshChannel *c, size_t bufsize)
{ c->vt->unthrottle(c, bufsize); }
static inline Conf *sshfwd_get_conf(SshChannel *c)
{ return c->vt->get_conf(c); }
static inline void sshfwd_window_override_removed(SshChannel *c)
{ c->vt->window_override_removed(c); }
static inline void sshfwd_x11_sharing_handover(
    SshChannel *c, ssh_sharing_connstate *cs, share_channel *sch,
    const char *addr, int port, int endian, int maj, int min,
    const void *idata, int ilen)
{ return c->vt->x11_sharing_handover(c, cs, sch, addr, port, endian,
                                     maj, min, idata, ilen); }
static inline void sshfwd_send_exit_status(SshChannel *c, int status)
{ c->vt->send_exit_status(c, status); }
static inline void sshfwd_send_exit_signal(
    SshChannel *c, ptrlen signame, bool core_dumped, ptrlen msg)
{ c->vt->send_exit_signal(c, signame, core_dumped, msg); }
static inline void sshfwd_send_exit_signal_numeric(
    SshChannel *c, int signum, bool core_dumped, ptrlen msg)
{ c->vt->send_exit_signal_numeric(c, signum, core_dumped, msg); }
static inline void sshfwd_request_x11_forwarding(
    SshChannel *c, bool want_reply, const char *proto,
    const char *data, int scr, bool once)
{ c->vt->request_x11_forwarding(c, want_reply, proto, data, scr, once); }
static inline void sshfwd_request_agent_forwarding(
    SshChannel *c, bool want_reply)
{ c->vt->request_agent_forwarding(c, want_reply); }
static inline void sshfwd_request_pty(
    SshChannel *c, bool want_reply, Conf *conf, int w, int h)
{ c->vt->request_pty(c, want_reply, conf, w, h); }
static inline bool sshfwd_send_env_var(
    SshChannel *c, bool want_reply, const char *var, const char *value)
{ return c->vt->send_env_var(c, want_reply, var, value); }
static inline void sshfwd_start_shell(
    SshChannel *c, bool want_reply)
{ c->vt->start_shell(c, want_reply); }
static inline void sshfwd_start_command(
    SshChannel *c, bool want_reply, const char *command)
{ c->vt->start_command(c, want_reply, command); }
static inline bool sshfwd_start_subsystem(
    SshChannel *c, bool want_reply, const char *subsystem)
{ return c->vt->start_subsystem(c, want_reply, subsystem); }
static inline bool sshfwd_send_serial_break(
    SshChannel *c, bool want_reply, int length)
{ return c->vt->send_serial_break(c, want_reply, length); }
static inline bool sshfwd_send_signal(
    SshChannel *c, bool want_reply, const char *signame)
{ return c->vt->send_signal(c, want_reply, signame); }
static inline void sshfwd_send_terminal_size_change(
    SshChannel *c, int w, int h)
{ c->vt->send_terminal_size_change(c, w, h); }
static inline void sshfwd_hint_channel_is_simple(SshChannel *c)
{ c->vt->hint_channel_is_simple(c); }

/* ----------------------------------------------------------------------
 * The 'main' or primary channel of the SSH connection is special,
 * because it's the one that's connected directly to parts of the
 * frontend such as the terminal and the specials menu. So it exposes
 * a richer API.
 */

mainchan *mainchan_new(
    PacketProtocolLayer *ppl, ConnectionLayer *cl, Conf *conf,
    int term_width, int term_height, bool is_simple, SshChannel **sc_out);
void mainchan_get_specials(
    mainchan *mc, add_special_fn_t add_special, void *ctx);
void mainchan_special_cmd(mainchan *mc, SessionSpecialCode code, int arg);
void mainchan_terminal_size(mainchan *mc, int width, int height);

#endif /* PUTTY_SSHCHAN_H */
