/*
 * General mechanism for wrapping up reading/writing of Windows
 * HANDLEs into a PuTTY Socket abstraction.
 */

#include <stdio.h>
#include <assert.h>
#include <limits.h>

#define DEFINE_PLUG_METHOD_MACROS
#include "tree234.h"
#include "putty.h"
#include "network.h"

typedef struct Socket_handle_tag *Handle_Socket;

struct Socket_handle_tag {
    const struct socket_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */

    HANDLE send_H, recv_H;
    struct handle *send_h, *recv_h;

    /*
     * Freezing one of these sockets is a slightly fiddly business,
     * because the reads from the handle are happening in a separate
     * thread as blocking system calls and so once one is in progress
     * it can't sensibly be interrupted. Hence, after the user tries
     * to freeze one of these sockets, it's unavoidable that we may
     * receive one more load of data before we manage to get
     * winhandl.c to stop reading.
     */
    enum {
        UNFROZEN,  /* reading as normal */
        FREEZING,  /* have been set to frozen but winhandl is still reading */
        FROZEN,    /* really frozen - winhandl has been throttled */
        THAWING    /* we're gradually releasing our remaining data */
    } frozen;
    /* We buffer data here if we receive it from winhandl while frozen. */
    bufchain inputdata;

    char *error;

    Plug plug;
};

static int handle_gotdata(struct handle *h, void *data, int len)
{
    Handle_Socket ps = (Handle_Socket) handle_get_privdata(h);

    if (len < 0) {
	return plug_closing(ps->plug, "Read error from handle",
			    0, 0);
    } else if (len == 0) {
	return plug_closing(ps->plug, NULL, 0, 0);
    } else {
        assert(ps->frozen != FREEZING && ps->frozen != THAWING);
        if (ps->frozen == FREEZING) {
            /*
             * If we've received data while this socket is supposed to
             * be frozen (because the read winhandl.c started before
             * sk_set_frozen was called has now returned) then buffer
             * the data for when we unfreeze.
             */
            bufchain_add(&ps->inputdata, data, len);

            /*
             * And return a very large backlog, to prevent further
             * data arriving from winhandl until we unfreeze.
             */
            return INT_MAX;
        } else {
            return plug_receive(ps->plug, 0, data, len);
        }
    }
}

static void handle_sentdata(struct handle *h, int new_backlog)
{
    Handle_Socket ps = (Handle_Socket) handle_get_privdata(h);
    
    plug_sent(ps->plug, new_backlog);
}

static Plug sk_handle_plug(Socket s, Plug p)
{
    Handle_Socket ps = (Handle_Socket) s;
    Plug ret = ps->plug;
    if (p)
	ps->plug = p;
    return ret;
}

static void sk_handle_close(Socket s)
{
    Handle_Socket ps = (Handle_Socket) s;

    handle_free(ps->send_h);
    handle_free(ps->recv_h);
    CloseHandle(ps->send_H);
    if (ps->recv_H != ps->send_H)
        CloseHandle(ps->recv_H);
    bufchain_clear(&ps->inputdata);

    sfree(ps);
}

static int sk_handle_write(Socket s, const char *data, int len)
{
    Handle_Socket ps = (Handle_Socket) s;

    return handle_write(ps->send_h, data, len);
}

static int sk_handle_write_oob(Socket s, const char *data, int len)
{
    /*
     * oob data is treated as inband; nasty, but nothing really
     * better we can do
     */
    return sk_handle_write(s, data, len);
}

static void sk_handle_write_eof(Socket s)
{
    Handle_Socket ps = (Handle_Socket) s;

    handle_write_eof(ps->send_h);
}

static void sk_handle_flush(Socket s)
{
    /* Handle_Socket ps = (Handle_Socket) s; */
    /* do nothing */
}

static void handle_socket_unfreeze(void *psv)
{
    Handle_Socket ps = (Handle_Socket) psv;
    void *data;
    int len, new_backlog;

    /*
     * If we've been put into a state other than THAWING since the
     * last callback, then we're done.
     */
    if (ps->frozen != THAWING)
        return;

    /*
     * Get some of the data we've buffered.
     */
    bufchain_prefix(&ps->inputdata, &data, &len);
    assert(len > 0);

    /*
     * Hand it off to the plug.
     */
    new_backlog = plug_receive(ps->plug, 0, data, len);

    if (bufchain_size(&ps->inputdata) > 0) {
        /*
         * If there's still data in our buffer, stay in THAWING state,
         * and reschedule ourself.
         */
        queue_toplevel_callback(handle_socket_unfreeze, ps);
    } else {
        /*
         * Otherwise, we've successfully thawed!
         */
        ps->frozen = UNFROZEN;
        handle_unthrottle(ps->recv_h, new_backlog);
    }
}

static void sk_handle_set_frozen(Socket s, int is_frozen)
{
    Handle_Socket ps = (Handle_Socket) s;

    if (is_frozen) {
        switch (ps->frozen) {
          case FREEZING:
          case FROZEN:
            return;                    /* nothing to do */

          case THAWING:
            /*
             * We were in the middle of emptying our bufchain, and got
             * frozen again. In that case, winhandl.c is already
             * throttled, so just return to FROZEN state. The toplevel
             * callback will notice and disable itself.
             */
            ps->frozen = FROZEN;
            break;

          case UNFROZEN:
            /*
             * The normal case. Go to FREEZING, and expect one more
             * load of data from winhandl if we're unlucky.
             */
            ps->frozen = FREEZING;
            break;
        }
    } else {
        switch (ps->frozen) {
          case UNFROZEN:
          case THAWING:
            return;                    /* nothing to do */

          case FREEZING:
            /*
             * If winhandl didn't send us any data throughout the time
             * we were frozen, then we'll still be in this state and
             * can just unfreeze in the trivial way.
             */
            assert(bufchain_size(&ps->inputdata) == 0);
            ps->frozen = UNFROZEN;
            break;

          case FROZEN:
            /*
             * If we have buffered data, go to THAWING and start
             * releasing it in top-level callbacks.
             */
            ps->frozen = THAWING;
            queue_toplevel_callback(handle_socket_unfreeze, ps);
        }
    }
}

static const char *sk_handle_socket_error(Socket s)
{
    Handle_Socket ps = (Handle_Socket) s;
    return ps->error;
}

static char *sk_handle_peer_info(Socket s)
{
    Handle_Socket ps = (Handle_Socket) s;
    ULONG pid;
    static HMODULE kernel32_module;
    DECL_WINDOWS_FUNCTION(static, BOOL, GetNamedPipeClientProcessId,
                          (HANDLE, PULONG));

    if (!kernel32_module) {
        kernel32_module = load_system32_dll("kernel32.dll");
        GET_WINDOWS_FUNCTION(kernel32_module, GetNamedPipeClientProcessId);
    }

    /*
     * Of course, not all handles managed by this module will be
     * server ends of named pipes, but if they are, then it's useful
     * to log what we can find out about the client end.
     */
    if (p_GetNamedPipeClientProcessId &&
        p_GetNamedPipeClientProcessId(ps->send_H, &pid))
        return dupprintf("process id %lu", (unsigned long)pid);

    return NULL;
}

Socket make_handle_socket(HANDLE send_H, HANDLE recv_H, Plug plug,
                          int overlapped)
{
    static const struct socket_function_table socket_fn_table = {
	sk_handle_plug,
	sk_handle_close,
	sk_handle_write,
	sk_handle_write_oob,
	sk_handle_write_eof,
	sk_handle_flush,
	sk_handle_set_frozen,
	sk_handle_socket_error,
        sk_handle_peer_info,
    };

    Handle_Socket ret;
    int flags = (overlapped ? HANDLE_FLAG_OVERLAPPED : 0);

    ret = snew(struct Socket_handle_tag);
    ret->fn = &socket_fn_table;
    ret->plug = plug;
    ret->error = NULL;
    ret->frozen = UNFROZEN;
    bufchain_init(&ret->inputdata);

    ret->recv_H = recv_H;
    ret->recv_h = handle_input_new(ret->recv_H, handle_gotdata, ret, flags);
    ret->send_H = send_H;
    ret->send_h = handle_output_new(ret->send_H, handle_sentdata, ret, flags);

    return (Socket) ret;
}
