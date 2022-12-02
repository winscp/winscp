/*
 * Null implementation of DeferredSocketOpener. Doesn't even bother to
 * allocate and free itself: there's just one static implementation
 * which we hand out to any caller.
 */

#include "putty.h"

static void null_opener_free(DeferredSocketOpener *opener) {}

static const DeferredSocketOpenerVtable NullOpener_vt = {
    /*.free =*/ null_opener_free,
};

static DeferredSocketOpener null_opener = { /*.vt =*/ &NullOpener_vt };

DeferredSocketOpener *null_deferred_socket_opener(void)
{
    return &null_opener;
}
