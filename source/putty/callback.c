/*
 * Facility for queueing callback functions to be run from the
 * top-level event loop after the current top-level activity finishes.
 */

#include <stddef.h>

#include "putty.h"

struct callback {
    struct callback *next;

    toplevel_callback_fn_t fn;
    void *ctx;
};

#ifdef MPEXT
// PuTTY has one thread only, so run_toplevel_callbacks does not cater for multi threaded uses.
// It would call callbacks registered any on thread from the thread that happens to call it.
// We need to create separate callback queue for every SSH session.
#define CALLBACK_SET_VAR callback_set_v
#define CALLBACK_SET_VAR_PARAM CALLBACK_SET_VAR,
#define cbcurr CALLBACK_SET_VAR->cbcurr
#define cbhead CALLBACK_SET_VAR->cbhead
#define cbtail CALLBACK_SET_VAR->cbtail
#else
#define CALLBACK_SET_VAR_PARAM
struct callback *cbcurr = NULL, *cbhead = NULL, *cbtail = NULL;
#endif

#ifndef MPEXT
toplevel_callback_notify_fn_t notify_frontend = NULL;
void *frontend = NULL;

void request_callback_notifications(toplevel_callback_notify_fn_t fn,
                                    void *fr)
{
    notify_frontend = fn;
    frontend = fr;
}
#endif

static void run_idempotent_callback(void *ctx)
{
    struct IdempotentCallback *ic = (struct IdempotentCallback *)ctx;
    ic->queued = FALSE;
    ic->fn(ic->ctx);
}

void queue_idempotent_callback(CALLBACK_SET struct IdempotentCallback *ic)
{
    if (ic->queued)
        return;
    ic->queued = TRUE;
    queue_toplevel_callback(CALLBACK_SET_VAR_PARAM run_idempotent_callback, ic);
}

#ifndef MPEXT
void delete_callbacks_for_context(void *ctx)
{
    struct callback *newhead, *newtail;

    newhead = newtail = NULL;
    while (cbhead) {
        struct callback *cb = cbhead;
        cbhead = cbhead->next;
        if (cb->ctx == ctx ||
            (cb->fn == run_idempotent_callback &&
             ((struct IdempotentCallback *)cb->ctx)->ctx == ctx)) {
            sfree(cb);
        } else {
            if (!newhead)
                newhead = cb;
            else
                newtail->next = cb;

            newtail = cb;
        }
    }

    cbhead = newhead;
    cbtail = newtail;
}
#endif

void queue_toplevel_callback(CALLBACK_SET toplevel_callback_fn_t fn, void *ctx)
{
    struct callback *cb;

    cb = snew(struct callback);
    cb->fn = fn;
    cb->ctx = ctx;

#ifndef MPEXT
    /*
     * If the front end has requested notification of pending
     * callbacks, and we didn't already have one queued, let it know
     * we do have one now.
     *
     * If cbcurr is non-NULL, i.e. we are actually in the middle of
     * executing a callback right now, then we count that as the queue
     * already having been non-empty. That saves the front end getting
     * a constant stream of needless re-notifications if the last
     * callback keeps re-scheduling itself.
     */
    if (notify_frontend && !cbhead && !cbcurr)
        notify_frontend(frontend);
#endif

    if (cbtail)
        cbtail->next = cb;
    else
        cbhead = cb;
    cbtail = cb;
    cb->next = NULL;
}

int run_toplevel_callbacks(CALLBACK_SET_ONLY)
{
    int done_something = FALSE;

    if (cbhead) {
        /*
         * Transfer the head callback into cbcurr to indicate that
         * it's being executed. Then operations which transform the
         * queue, like delete_callbacks_for_context, can proceed as if
         * it's not there.
         */
        cbcurr = cbhead;
        cbhead = cbhead->next;
        if (!cbhead)
            cbtail = NULL;

        /*
         * Now run the callback, and then clear it out of cbcurr.
         */
        cbcurr->fn(cbcurr->ctx);
        sfree(cbcurr);
        cbcurr = NULL;

        done_something = TRUE;
    }
    return done_something;
}

int toplevel_callback_pending(CALLBACK_SET_ONLY)
{
    // MP does not have to be guarded
    return cbcurr != NULL || cbhead != NULL;
}
