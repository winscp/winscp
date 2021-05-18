/*
 * Facility for queueing callback functions to be run from the
 * top-level event loop after the current top-level activity finishes.
 */

#include <stddef.h>
#include <assert.h> // WINSCP

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
#define cbcurr CALLBACK_SET_VAR->cbcurr
#define cbhead CALLBACK_SET_VAR->cbhead
#define cbtail CALLBACK_SET_VAR->cbtail
#else
static struct callback *cbcurr = NULL, *cbhead = NULL, *cbtail = NULL;
#endif

#ifndef MPEXT
static toplevel_callback_notify_fn_t notify_frontend = NULL;
static void *notify_ctx = NULL;

void request_callback_notifications(toplevel_callback_notify_fn_t fn,
                                    void *ctx)
{
    notify_frontend = fn;
    notify_ctx = ctx;
}
#endif

static void run_idempotent_callback(void *ctx)
{
    struct IdempotentCallback *ic = (struct IdempotentCallback *)ctx;
    ic->queued = false;
    ic->fn(ic->ctx);
}

void queue_idempotent_callback(struct IdempotentCallback *ic)
{
    if (ic->queued)
        return;
    ic->queued = true;
#ifdef MPEXT
    assert(ic->set != NULL);
#endif
    queue_toplevel_callback(ic->set, run_idempotent_callback, ic);
}

void delete_callbacks_for_context(CALLBACK_SET void *ctx)
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
    if (newtail)
        newtail->next = NULL;
}

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
        notify_frontend(notify_ctx);
#endif

    if (cbtail)
        cbtail->next = cb;
    else
        cbhead = cb;
    cbtail = cb;
    cb->next = NULL;
}

bool run_toplevel_callbacks(CALLBACK_SET_ONLY)
{
    bool done_something = false;

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
        // WINSCP: this does not happen, when exception (disconnect) occurs while the callback is called.
        // See also the comment in TSecureShell::FreeBackend().
        cbcurr = NULL;

        done_something = true;
    }
    return done_something;
}

bool toplevel_callback_pending(CALLBACK_SET_ONLY)
{
    // MP does not have to be guarded
    return cbcurr != NULL || cbhead != NULL;
}

// WINSCP
bool is_idempotent_callback_pending(CALLBACK_SET struct IdempotentCallback *ic)
{
    return
      (cbhead != NULL) &&
      (cbhead->fn == run_idempotent_callback) &&
      (cbhead->ctx == ic);
}
