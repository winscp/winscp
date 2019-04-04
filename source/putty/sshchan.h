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

    int (*send)(Channel *, int is_stderr, const void *buf, int len);
    void (*send_eof)(Channel *);
    void (*set_input_wanted)(Channel *, int wanted);

    char *(*log_close_msg)(Channel *);

    int (*want_close)(Channel *, int sent_local_eof, int rcvd_remote_eof);
};

struct Channel {
    const struct ChannelVtable *vt;
    unsigned initial_fixed_window_size;
};

#define chan_free(ch) ((ch)->vt->free(ch))
#define chan_open_confirmation(ch) ((ch)->vt->open_confirmation(ch))
#define chan_open_failed(ch, err) ((ch)->vt->open_failed(ch, err))
#define chan_send(ch, err, buf, len) ((ch)->vt->send(ch, err, buf, len))
#define chan_send_eof(ch) ((ch)->vt->send_eof(ch))
#define chan_set_input_wanted(ch, wanted) \
    ((ch)->vt->set_input_wanted(ch, wanted))
#define chan_log_close_msg(ch) ((ch)->vt->log_close_msg(ch))
#define chan_want_close(ch, leof, reof) ((ch)->vt->want_close(ch, leof, reof))

/*
 * Reusable methods you can put in vtables to give default handling of
 * some of those functions.
 */

/* open_confirmation / open_failed for any channel it doesn't apply to */
void chan_remotely_opened_confirmation(Channel *chan);
void chan_remotely_opened_failure(Channel *chan, const char *errtext);

/* want_close for any channel that wants the default behaviour of not
 * closing until both directions have had an EOF */
int chan_no_eager_close(Channel *, int, int);

#endif /* PUTTY_SSHCHAN_H */
