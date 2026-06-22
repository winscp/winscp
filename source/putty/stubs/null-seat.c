/*
 * Stub methods usable by Seat implementations.
 */

#include "putty.h"

size_t nullseat_output(
    Seat *seat, SeatOutputType type, const void *data, size_t len) {return 0;}
bool nullseat_eof(Seat *seat) { return true; }
void nullseat_sent(Seat *seat, size_t bufsize) {}
size_t nullseat_banner(Seat *seat, const void *data, size_t len) {return 0;}
size_t nullseat_banner_to_stderr(Seat *seat, const void *data, size_t len)
{ return seat_output(seat, SEAT_OUTPUT_STDERR, data, len); }
SeatPromptResult nullseat_get_userpass_input(Seat *seat, prompts_t *p)
{ return SPR_SW_ABORT("this seat can't handle interactive prompts"); }
void nullseat_notify_session_started(Seat *seat) {}
void nullseat_notify_remote_exit(Seat *seat) {}
void nullseat_notify_remote_disconnect(Seat *seat) {}
void nullseat_connection_fatal(Seat *seat, const char *message) {}
void nullseat_nonfatal(Seat *seat, const char *message) {}
void nullseat_update_specials_menu(Seat *seat) {}
char *nullseat_get_ttymode(Seat *seat, const char *mode) { return NULL; }
void nullseat_set_busy_status(Seat *seat, BusyStatus status) {}
SeatPromptResult nullseat_confirm_ssh_host_key(
    Seat *seat, const char *host, int port, const char *keytype,
    char *keystr, SeatDialogText *text, HelpCtx helpctx,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx,
    char **fingerprints, bool is_certificate, int ca_count, bool already_verified) // WINSCP
{ return SPR_SW_ABORT("this seat can't handle interactive prompts"); }
SeatPromptResult nullseat_confirm_weak_crypto_primitive(
    Seat *seat, SeatDialogText *text,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx,
    const char *algtype, const char *algname, int wcr) // WINSCP
{ return SPR_SW_ABORT("this seat can't handle interactive prompts"); }
SeatPromptResult nullseat_confirm_weak_cached_hostkey(
    Seat *seat, SeatDialogText *text,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx)
{ return SPR_SW_ABORT("this seat can't handle interactive prompts"); }
bool nullseat_is_never_utf8(Seat *seat) { return false; }
bool nullseat_is_always_utf8(Seat *seat) { return true; }
void nullseat_echoedit_update(Seat *seat, bool echoing, bool editing) {}
const char *nullseat_get_x_display(Seat *seat) { return NULL; }
bool nullseat_get_windowid(Seat *seat, long *id_out) { return false; }
bool nullseat_get_window_pixel_size(
    Seat *seat, int *width, int *height) { return false; }
StripCtrlChars *nullseat_stripctrl_new(
    Seat *seat, BinarySink *bs_out, SeatInteractionContext sic) {return NULL;}
void nullseat_set_trust_status(Seat *seat, bool trusted) {}
bool nullseat_can_set_trust_status_yes(Seat *seat) { return true; }
bool nullseat_can_set_trust_status_no(Seat *seat) { return false; }
bool nullseat_has_mixed_input_stream_yes(Seat *seat) { return true; }
bool nullseat_has_mixed_input_stream_no(Seat *seat) { return false; }
bool nullseat_verbose_no(Seat *seat) { return false; }
bool nullseat_verbose_yes(Seat *seat) { return true; }
bool nullseat_interactive_no(Seat *seat) { return false; }
bool nullseat_interactive_yes(Seat *seat) { return true; }
bool nullseat_get_cursor_position(Seat *seat, int *x, int *y) { return false; }

const SeatDialogPromptDescriptions *nullseat_prompt_descriptions(Seat *seat)
{
    static const SeatDialogPromptDescriptions descs = {
        /*.hk_accept_action =*/ "",
        /*.hk_connect_once_action =*/ "",
        /*.hk_cancel_action =*/ "",
        /*.hk_cancel_action_Participle =*/ "",
        /*.weak_accept_action =*/ "",
        /*.weak_cancel_action =*/ "",
    };
    return &descs;
}
