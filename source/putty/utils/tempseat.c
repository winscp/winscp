/*
 * Implementation of the Seat trait that buffers output and other
 * events until it can give them back to a real Seat.
 *
 * This is used by the SSH proxying code, which temporarily takes over
 * the real user-facing Seat so that it can issue host key warnings,
 * password prompts etc for the proxy SSH connection. While it's got
 * the real Seat, it gives the primary connection's backend one of
 * these temporary Seats in the interim, so that if the backend wants
 * to send some kind of initial output, or start by reconfiguring the
 * trust status, or what have you, then it can do that without having
 * to keep careful track of the fact that its Seat is out on loan.
 */

#include "putty.h"

struct output_chunk {
    struct output_chunk *next;
    SeatOutputType type;
    size_t size;
};

typedef struct TempSeat TempSeat;
struct TempSeat {
    Seat *realseat;

    /*
     * Single bufchain to hold all the buffered output, regardless of
     * its type.
     */
    bufchain output;

    /*
     * List of pieces of that bufchain that are intended for one or
     * another output destination
     */
    struct output_chunk *outchunk_head, *outchunk_tail;

    bool seen_session_started;
    bool seen_remote_exit;
    bool seen_remote_disconnect;
    bool seen_update_specials_menu;
    bool seen_echoedit_update, echoing, editing;
    bool seen_trust_status, trusted;

    Seat seat;
};

/* ----------------------------------------------------------------------
 * Methods we can usefully buffer, and pass their results on to the
 * real Seat in tempseat_flush().
 */

static size_t tempseat_output(Seat *seat, SeatOutputType type,
                              const void *data, size_t len)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);

    bufchain_add(&ts->output, data, len);

    if (!(ts->outchunk_tail && ts->outchunk_tail->type == type)) {
        struct output_chunk *new_chunk = snew(struct output_chunk);

        new_chunk->type = type;
        new_chunk->size = 0;

        new_chunk->next = NULL;
        if (ts->outchunk_tail)
            ts->outchunk_tail->next = new_chunk;
        else
            ts->outchunk_head = new_chunk;
        ts->outchunk_tail = new_chunk;
    }
    ts->outchunk_tail->size += len;

    return bufchain_size(&ts->output);
}

static void tempseat_notify_session_started(Seat *seat)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    ts->seen_session_started = true;
}

static void tempseat_notify_remote_exit(Seat *seat)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    ts->seen_remote_exit = true;
}

static void tempseat_notify_remote_disconnect(Seat *seat)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    ts->seen_remote_disconnect = true;
}

static void tempseat_update_specials_menu(Seat *seat)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    ts->seen_update_specials_menu = true;
}

static void tempseat_echoedit_update(Seat *seat, bool echoing, bool editing)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    ts->seen_echoedit_update = true;
    ts->echoing = echoing;
    ts->editing = editing;
}

static void tempseat_set_trust_status(Seat *seat, bool trusted)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    ts->seen_trust_status = true;
    ts->trusted = trusted;
}

/* ----------------------------------------------------------------------
 * Methods we can safely pass straight on to the real Seat, usually
 * (but not in every case) because they're read-only queries.
 */

static char *tempseat_get_ttymode(Seat *seat, const char *mode)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    return seat_get_ttymode(ts->realseat, mode);
}

static void tempseat_set_busy_status(Seat *seat, BusyStatus status)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    /*
     * set_busy_status is generally called when something is about to
     * do some single-threaded, event-loop blocking computation. This
     * _shouldn't_ happen in a backend while it's waiting for a
     * network connection to be made, but if for some reason it were
     * to, there's no reason we can't just pass this straight to the
     * real seat, because we expect that it will mark itself busy,
     * compute, and mark itself unbusy, all between yields to the
     * event loop that might give whatever else is using the real Seat
     * an opportunity to do anything.
     */
    seat_set_busy_status(ts->realseat, status);
}

static bool tempseat_is_utf8(Seat *seat)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    return seat_is_utf8(ts->realseat);
}

static const char *tempseat_get_x_display(Seat *seat)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    return seat_get_x_display(ts->realseat);
}

static bool tempseat_get_windowid(Seat *seat, long *id_out)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    return seat_get_windowid(ts->realseat, id_out);
}

static bool tempseat_get_window_pixel_size(Seat *seat, int *width, int *height)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    return seat_get_window_pixel_size(ts->realseat, width, height);
}

static StripCtrlChars *tempseat_stripctrl_new(
    Seat *seat, BinarySink *bs_out, SeatInteractionContext sic)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    return seat_stripctrl_new(ts->realseat, bs_out, sic);
}

static bool tempseat_verbose(Seat *seat)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    return seat_verbose(ts->realseat);
}

static bool tempseat_interactive(Seat *seat)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    return seat_interactive(ts->realseat);
}

static bool tempseat_get_cursor_position(Seat *seat, int *x, int *y)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    return seat_get_cursor_position(ts->realseat, x, y);
}

static bool tempseat_can_set_trust_status(Seat *seat)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    return seat_can_set_trust_status(ts->realseat);
}

static bool tempseat_has_mixed_input_stream(Seat *seat)
{
    TempSeat *ts = container_of(seat, TempSeat, seat);
    return seat_has_mixed_input_stream(ts->realseat);
}

static const SeatDialogPromptDescriptions *tempseat_prompt_descriptions(
    Seat *seat)
{
    /* It might be OK to put this in the 'unreachable' category, but I
     * think it's equally good to put it here, which allows for
     * someone _preparing_ a prompt right now that they intend to
     * present once the TempSeat has given way to the real one. */
    TempSeat *ts = container_of(seat, TempSeat, seat);
    return seat_prompt_descriptions(ts->realseat);
}

/* ----------------------------------------------------------------------
 * Methods that should never be called on a TempSeat, so we can put an
 * unreachable() in them.
 *
 * A backend in possession of a TempSeat ought to be sitting and
 * patiently waiting for a network connection attempt to either
 * succeed or fail. And it should be aware of the possibility that the
 * proxy setup code to which it has lent the real Seat might need to
 * present interactive prompts - that's the whole point of lending out
 * the Seat in the first place - so it absolutely shouldn't get any
 * ideas about issuing some kind of prompt of its own while it waits
 * for the network connection.
 */

static SeatPromptResult tempseat_get_userpass_input(Seat *seat, prompts_t *p)
{
    /*
     * Interactive prompts of this nature are a thing that a backend
     * MUST NOT do while not in possession of the real Seat, because
     * the whole point of temporarily lending the real Seat to
     * something else is that so it can have a clear field to do
     * interactive stuff of its own while making a network connection.
     */
    unreachable("get_userpass_input should never be called on TempSeat");
}

static size_t tempseat_banner(Seat *seat, const void *data, size_t len)
{
    unreachable("banner should never be called on TempSeat");
}

static SeatPromptResult tempseat_confirm_ssh_host_key(
    Seat *seat, const char *host, int port, const char *keytype,
    char *keystr, SeatDialogText *text, HelpCtx helpctx,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx,
    char **fingerprints, bool is_certificate, int ca_count, bool already_verified) // WINSCP
{
    unreachable("confirm_ssh_host_key should never be called on TempSeat");
}

static SeatPromptResult tempseat_confirm_weak_crypto_primitive(
    Seat *seat, SeatDialogText *text,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx,
    const char *algtype, const char *algname, int wcr) // WINSCP
{
    unreachable("confirm_weak_crypto_primitive "
                "should never be called on TempSeat");
}

static SeatPromptResult tempseat_confirm_weak_cached_hostkey(
    Seat *seat, SeatDialogText *text,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx)
{
    unreachable("confirm_weak_cached_hostkey "
                "should never be called on TempSeat");
}

static void tempseat_connection_fatal(Seat *seat, const char *message)
{
    /*
     * Fatal errors are another thing a backend should not have any
     * reason to encounter while waiting to hear back about its
     * network connection setup.
     *
     * Also, if a backend _did_ call this, it would be hellish to
     * unpick all the error handling. Just passing on the fatal error
     * to the real Seat wouldn't be good enough: what about freeing
     * all the various things that are confusingly holding pointers to
     * each other? Better to leave this as an assertion-failure level
     * issue, so that if it does ever happen by accident, we'll know
     * it's a bug.
     */
    unreachable("connection_fatal should never be called on TempSeat");
}

static void tempseat_nonfatal(Seat *seat, const char *message)
{
    /*
     * Non-fatal errors specific to a Seat should also not occur,
     * because those will be for things like I/O errors writing the
     * host key collection, and a backend's not _doing_ that when we
     * haven't connected it to the host yet.
     */
    unreachable("nonfatal should never be called on TempSeat");
}

static bool tempseat_eof(Seat *seat)
{
    /*
     * EOF is _very nearly_ something that we could buffer, and pass
     * on to the real Seat at flush time. The only difficulty is that
     * sometimes the front end wants to respond to an incoming EOF by
     * instructing the back end to send an outgoing one, which it does
     * by returning a bool from its eof method.
     *
     * So we'd have to arrange that tempseat_flush caught that return
     * value and passed it on to the calling backend. And then every
     * backend would have to deal with tempseat_flush maybe returning
     * it an 'actually, please start closing down now' indication,
     * which could only happen _in theory_, if it had for some reason
     * called seat_eof on the TempSeat.
     *
     * But in fact, we don't expect back ends to call seat_eof on the
     * TempSeat in the first place, so all of that effort would be a
     * total waste. Hence, we'll put EOF in the category of things we
     * expect backends never to do while the real Seat is out on loan.
     */
    unreachable("eof should never be called on TempSeat");
}

/* ----------------------------------------------------------------------
 * Done with the TempSeat methods. Here's the vtable definition and
 * the main setup/teardown code.
 */

static const struct SeatVtable tempseat_vt = {
    // WINSCP
    /*.output =*/ tempseat_output,
    /*.eof =*/ tempseat_eof,
    /*.sent =*/ nullseat_sent,
    /*.banner =*/ tempseat_banner,
    /*.get_userpass_input =*/ tempseat_get_userpass_input,
    /*.notify_session_started =*/ tempseat_notify_session_started,
    /*.notify_remote_exit =*/ tempseat_notify_remote_exit,
    /*.notify_remote_disconnect =*/ tempseat_notify_remote_disconnect,
    /*.connection_fatal =*/ tempseat_connection_fatal,
    /*.nonfatal =*/ tempseat_nonfatal,
    /*.update_specials_menu =*/ tempseat_update_specials_menu,
    /*.get_ttymode =*/ tempseat_get_ttymode,
    /*.set_busy_status =*/ tempseat_set_busy_status,
    /*.confirm_ssh_host_key =*/ tempseat_confirm_ssh_host_key,
    /*.confirm_weak_crypto_primitive =*/ tempseat_confirm_weak_crypto_primitive,
    /*.confirm_weak_cached_hostkey =*/ tempseat_confirm_weak_cached_hostkey,
    /*.prompt_descriptions =*/ tempseat_prompt_descriptions,
    /*.is_utf8 =*/ tempseat_is_utf8,
    /*.echoedit_update =*/ tempseat_echoedit_update,
    /*.get_x_display =*/ tempseat_get_x_display,
    /*.get_windowid =*/ tempseat_get_windowid,
    /*.get_window_pixel_size =*/ tempseat_get_window_pixel_size,
    /*.stripctrl_new =*/ tempseat_stripctrl_new,
    /*.set_trust_status =*/ tempseat_set_trust_status,
    /*.can_set_trust_status =*/ tempseat_can_set_trust_status,
    /*.has_mixed_input_stream =*/ tempseat_has_mixed_input_stream,
    /*.verbose =*/ tempseat_verbose,
    /*.interactive =*/ tempseat_interactive,
    /*.get_cursor_position =*/ tempseat_get_cursor_position,
};

Seat *tempseat_new(Seat *realseat)
{
    TempSeat *ts = snew(TempSeat);
    memset(ts, 0, sizeof(*ts));
    ts->seat.vt = &tempseat_vt;

    ts->realseat = realseat;
    bufchain_init(&ts->output);
    ts->outchunk_head = ts->outchunk_tail = NULL;

    return &ts->seat;
}

bool is_tempseat(Seat *seat)
{
    return seat->vt == &tempseat_vt;
}

Seat *tempseat_get_real(Seat *seat)
{
    pinitassert(seat->vt == &tempseat_vt);
    TempSeat *ts = container_of(seat, TempSeat, seat);
    return ts->realseat;
}

void tempseat_free(Seat *seat)
{
    pinitassert(seat->vt == &tempseat_vt);
    TempSeat *ts = container_of(seat, TempSeat, seat);
    bufchain_clear(&ts->output);
    while (ts->outchunk_head) {
        struct output_chunk *chunk = ts->outchunk_head;
        ts->outchunk_head = chunk->next;
        sfree(chunk);
    }
    sfree(ts);
}

void tempseat_flush(Seat *seat)
{
    pinitassert(seat->vt == &tempseat_vt);
    TempSeat *ts = container_of(seat, TempSeat, seat);

    /* Empty the output bufchains into the real seat, taking care to
     * preserve both separation and interleaving */
    while (bufchain_size(&ts->output)) {
        ptrlen pl = bufchain_prefix(&ts->output);

        pinitassert(ts->outchunk_head);
        struct output_chunk *chunk = ts->outchunk_head;

        if (pl.len > chunk->size)
            pl.len = chunk->size;

        seat_output(ts->realseat, chunk->type, pl.ptr, pl.len);
        bufchain_consume(&ts->output, pl.len);
        chunk->size -= pl.len;
        if (chunk->size == 0) {
            ts->outchunk_head = chunk->next;
            sfree(chunk);
        }
    }

    /* That should have exactly emptied the output chunk list too */
    assert(!ts->outchunk_head);

    /* Pass on any other kinds of event we've buffered */
    if (ts->seen_session_started)
        seat_notify_session_started(ts->realseat);
    if (ts->seen_remote_exit)
        seat_notify_remote_exit(ts->realseat);
    if (ts->seen_remote_disconnect)
        seat_notify_remote_disconnect(ts->realseat);
    if (ts->seen_update_specials_menu)
        seat_update_specials_menu(ts->realseat);
    if (ts->seen_echoedit_update)
        seat_echoedit_update(ts->realseat, ts->echoing, ts->editing);
    if (ts->seen_trust_status)
        seat_set_trust_status(ts->realseat, ts->trusted);
}
