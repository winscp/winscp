/*
 * nullplug.c: provide a null implementation of the Plug vtable which
 * ignores all calls. Occasionally useful in cases where we want to
 * make a network connection just to see if it works, but not do
 * anything with it afterwards except close it again.
 */

#include "putty.h"

static void nullplug_socket_log(Plug *plug, int type, SockAddr *addr, int port,
                                const char *error_msg, int error_code)
{
}

static void nullplug_closing(Plug *plug, const char *error_msg, int error_code,
			     bool calling_back)
{
}

static void nullplug_receive(
    Plug *plug, int urgent, const char *data, size_t len)
{
}

static void nullplug_sent(Plug *plug, size_t bufsize)
{
}

static const PlugVtable nullplug_plugvt = {
    nullplug_socket_log,
    nullplug_closing,
    nullplug_receive,
    nullplug_sent,
    NULL
};

static Plug nullplug_plug = { &nullplug_plugvt };

/*
 * There's a singleton instance of nullplug, because it's not
 * interesting enough to worry about making more than one of them.
 */
Plug *const nullplug = &nullplug_plug;
