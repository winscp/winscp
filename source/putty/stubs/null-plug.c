/*
 * nullplug.c: provide a null implementation of the Plug vtable which
 * ignores all calls. Occasionally useful in cases where we want to
 * make a network connection just to see if it works, but not do
 * anything with it afterwards except close it again.
 */

#include "putty.h"

void nullplug_log(Plug *plug, Socket *s, PlugLogType type, SockAddr *addr,
                  int port, const char *err_msg, int err_code)
{
}

void nullplug_closing(Plug *plug, PlugCloseType type, const char *error_msg)
{
}

void nullplug_receive(Plug *plug, int urgent, const char *data, size_t len)
{
}

void nullplug_sent(Plug *plug, size_t bufsize)
{
}

static const PlugVtable nullplug_plugvt = {
    // WINSCP
    /*.log =*/ nullplug_log,
    /*.closing =*/ nullplug_closing,
    /*.receive =*/ nullplug_receive,
    /*.sent =*/ nullplug_sent,
    NULL, // WINSCP
};

static Plug nullplug_plug = { &nullplug_plugvt };

/*
 * There's a singleton instance of nullplug, because it's not
 * interesting enough to worry about making more than one of them.
 */
Plug *const nullplug = &nullplug_plug;
