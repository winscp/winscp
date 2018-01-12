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

struct callback *cbhead = NULL, *cbtail = NULL;

toplevel_callback_notify_fn_t notify_frontend = NULL;
void *frontend = NULL;

void request_callback_notifications(toplevel_callback_notify_fn_t fn,
                                    void *fr)
{
    MPEXT_PUTTY_SECTION_ENTER;
    notify_frontend = fn;
    frontend = fr;
    MPEXT_PUTTY_SECTION_LEAVE;
}

void queue_toplevel_callback(toplevel_callback_fn_t fn, void *ctx)
{
    struct callback *cb;

    MPEXT_PUTTY_SECTION_ENTER;
    cb = snew(struct callback);
    cb->fn = fn;
    cb->ctx = ctx;

    /* If the front end has requested notification of pending
     * callbacks, and we didn't already have one queued, let it know
     * we do have one now. */
    if (notify_frontend && !cbhead)
        notify_frontend(frontend);

    if (cbtail)
        cbtail->next = cb;
    else
        cbhead = cb;
    cbtail = cb;
    cb->next = NULL;
    MPEXT_PUTTY_SECTION_LEAVE;
}

void run_toplevel_callbacks(void)
{
    MPEXT_PUTTY_SECTION_ENTER;
    if (cbhead) {
        struct callback *cb = cbhead;
        #ifdef MPEXT
        toplevel_callback_fn_t fn = cb->fn;
        void * ctx = cb->ctx;
        cbhead = cb->next;
        if (!cbhead)
            cbtail = NULL;
        cbhead = cb->next;
        sfree(cb);
        MPEXT_PUTTY_SECTION_LEAVE;
        fn(ctx);
        #else
        /*
         * Careful ordering here. We call the function _before_
         * advancing cbhead (though, of course, we must free cb
         * _after_ advancing it). This means that if the very last
         * callback schedules another callback, cbhead does not become
         * NULL at any point, and so the frontend notification
         * function won't be needlessly pestered.
         */
        cb->fn(cb->ctx);
        cbhead = cb->next;
        sfree(cb);
        if (!cbhead)
            cbtail = NULL;
        #endif
    }
    #ifdef MPEXT
    else
    {
      MPEXT_PUTTY_SECTION_LEAVE;
    }
    #endif
}

int toplevel_callback_pending(void)
{
    // MP does not have to be guarded
    return cbhead != NULL;
}
