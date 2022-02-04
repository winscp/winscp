/*
 * Supporting routines used in common by all the various components of
 * the SSH system.
 */

#include <assert.h>
#include <stdlib.h>

#include "putty.h"
#include "ssh.h"
#include "ssh/channel.h"

/* ----------------------------------------------------------------------
 * Centralised standard methods for other channel implementations to
 * borrow.
 */

void chan_remotely_opened_confirmation(Channel *chan)
{
    unreachable("this channel type should never receive OPEN_CONFIRMATION");
}

void chan_remotely_opened_failure(Channel *chan, const char *errtext)
{
    unreachable("this channel type should never receive OPEN_FAILURE");
}

bool chan_default_want_close(
    Channel *chan, bool sent_local_eof, bool rcvd_remote_eof)
{
    /*
     * Default close policy: we start initiating the CHANNEL_CLOSE
     * procedure as soon as both sides of the channel have seen EOF.
     */
    return sent_local_eof && rcvd_remote_eof;
}

bool chan_no_exit_status(Channel *chan, int status)
{
    return false;
}

bool chan_no_exit_signal(
    Channel *chan, ptrlen signame, bool core_dumped, ptrlen msg)
{
    return false;
}

bool chan_no_exit_signal_numeric(
    Channel *chan, int signum, bool core_dumped, ptrlen msg)
{
    return false;
}

bool chan_no_run_shell(Channel *chan)
{
    return false;
}

bool chan_no_run_command(Channel *chan, ptrlen command)
{
    return false;
}

bool chan_no_run_subsystem(Channel *chan, ptrlen subsys)
{
    return false;
}

bool chan_no_enable_x11_forwarding(
    Channel *chan, bool oneshot, ptrlen authproto, ptrlen authdata,
    unsigned screen_number)
{
    return false;
}

bool chan_no_enable_agent_forwarding(Channel *chan)
{
    return false;
}

bool chan_no_allocate_pty(
    Channel *chan, ptrlen termtype, unsigned width, unsigned height,
    unsigned pixwidth, unsigned pixheight, struct ssh_ttymodes modes)
{
    return false;
}

bool chan_no_set_env(Channel *chan, ptrlen var, ptrlen value)
{
    return false;
}

bool chan_no_send_break(Channel *chan, unsigned length)
{
    return false;
}

bool chan_no_send_signal(Channel *chan, ptrlen signame)
{
    return false;
}

bool chan_no_change_window_size(
    Channel *chan, unsigned width, unsigned height,
    unsigned pixwidth, unsigned pixheight)
{
    return false;
}

void chan_no_request_response(Channel *chan, bool success)
{
    unreachable("this channel type should never send a want-reply request");
}

/* ----------------------------------------------------------------------
 * Other miscellaneous utility functions.
 */

void free_rportfwd(struct ssh_rportfwd *rpf)
{
    if (rpf) {
        sfree(rpf->log_description);
        sfree(rpf->shost);
        sfree(rpf->dhost);
        sfree(rpf);
    }
}
