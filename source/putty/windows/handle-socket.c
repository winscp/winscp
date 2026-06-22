/*
 * General mechanism for wrapping up reading/writing of Windows
 * HANDLEs into a PuTTY Socket abstraction.
 */

#include <stdio.h>
#include <assert.h>
#include <limits.h>

#include "tree234.h"
#include "putty.h"
#include "network.h"

#ifdef MPEXT
#define queue_toplevel_callback(FN, CTX) queue_toplevel_callback(get_callback_set(CTX->plug), FN, CTX)
#endif

/*
 * Freezing one of these sockets is a slightly fiddly business,
 * because the reads from the handle are happening in a separate
 * thread as blocking system calls and so once one is in progress it
 * can't sensibly be interrupted. Hence, after the user tries to
 * freeze one of these sockets, it's unavoidable that we may receive
 * one more load of data before we manage to get handle-io.c to stop
 * reading.
 */
typedef enum HandleSocketFreezeState {
    UNFROZEN,  /* reading as normal */
    FREEZING,  /* have been set to frozen but winhandl is still reading */
    FROZEN,    /* really frozen - winhandl has been throttled */
    THAWING    /* we're gradually releasing our remaining data */
} HandleSocketFreezeState;

typedef struct HandleSocket {
    union {
        struct {
            HANDLE send_H, recv_H, stderr_H;
            struct handle *send_h, *recv_h, *stderr_h;

            HandleSocketFreezeState frozen;
            /* We buffer data here if we receive it from winhandl
             * while frozen. */
            bufchain inputdata;

            /* Handle logging proxy error messages from stderr_H, if
             * we have one */
            ProxyStderrBuf psb;

            bool defer_close, deferred_close;   /* in case of re-entrance */
        };
        struct {
            DeferredSocketOpener *opener;

            /* We buffer data here if we receive it via sk_write
             * before the socket is opened. */
            bufchain outputdata;

            bool output_eof_pending;

            bool start_frozen;
        };
    };

    char *error;

    SockAddr *addr;
    int port;
    Plug *plug;

    Socket sock;
} HandleSocket;

static size_t handle_gotdata(
    struct handle *h, const void *data, size_t len, int err)
{
    HandleSocket *hs = (HandleSocket *)handle_get_privdata(h);

    if (err) {
        plug_closing_error(hs->plug, "Read error from handle");
        return 0;
    } else if (len == 0) {
        plug_closing_normal(hs->plug);
        return 0;
    } else {
        assert(hs->frozen != FROZEN && hs->frozen != THAWING);
        if (hs->frozen == FREEZING) {
            /*
             * If we've received data while this socket is supposed to
             * be frozen (because the read handle-io.c started before
             * sk_set_frozen was called has now returned) then buffer
             * the data for when we unfreeze.
             */
            bufchain_add(&hs->inputdata, data, len);
            hs->frozen = FROZEN;

            /*
             * And return a very large backlog, to prevent further
             * data arriving from winhandl until we unfreeze.
             */
            return INT_MAX;
        } else {
            plug_receive(hs->plug, 0, data, len);
            return 0;
        }
    }
}

static size_t handle_stderr(
    struct handle *h, const void *data, size_t len, int err)
{
    HandleSocket *hs = (HandleSocket *)handle_get_privdata(h);

    if (!err && len > 0)
        log_proxy_stderr(hs->plug, &hs->sock, &hs->psb, data, len);

    return 0;
}

static void handle_sentdata(struct handle *h, size_t new_backlog, int err,
                            bool close)
{
    HandleSocket *hs = (HandleSocket *)handle_get_privdata(h);

    if (close) {
        if (hs->send_H != INVALID_HANDLE_VALUE)
            CloseHandle(hs->send_H);
        if (hs->recv_H != INVALID_HANDLE_VALUE && hs->recv_H != hs->send_H)
            CloseHandle(hs->recv_H);
        hs->send_H = hs->recv_H = INVALID_HANDLE_VALUE;
    }

    if (err) {
        plug_closing_system_error(hs->plug, err);
        return;
    }

    plug_sent(hs->plug, new_backlog);
}

static Plug *sk_handle_plug(Socket *s, Plug *p)
{
    HandleSocket *hs = container_of(s, HandleSocket, sock);
    Plug *ret = hs->plug;
    if (p)
        hs->plug = p;
    return ret;
}

static void sk_handle_close(Socket *s)
{
    HandleSocket *hs = container_of(s, HandleSocket, sock);

    if (hs->defer_close) {
        hs->deferred_close = true;
        return;
    }

    #ifdef MPEXT
    // WinSCP core uses do_select as signalization of connection up/down
    do_select(hs->plug, INVALID_SOCKET, 0);
    #endif

    handle_free(hs->send_h);
    handle_free(hs->recv_h);
    if (hs->send_H != INVALID_HANDLE_VALUE)
        CloseHandle(hs->send_H);
    if (hs->recv_H != INVALID_HANDLE_VALUE && hs->recv_H != hs->send_H)
        CloseHandle(hs->recv_H);
    bufchain_clear(&hs->inputdata);
#ifdef MPEXT
    if (hs->stderr_h)
    {
        handle_free(hs->stderr_h);
    }
    if (hs->stderr_H)
    {
        CloseHandle(hs->stderr_H);
    }
#endif

    if (hs->addr)
        sk_addr_free(hs->addr);

    delete_callbacks_for_context(get_callback_set(hs->plug), hs);

    sfree(hs);
}

static size_t sk_handle_write(Socket *s, const void *data, size_t len)
{
    HandleSocket *hs = container_of(s, HandleSocket, sock);

    return handle_write(hs->send_h, data, len);
}

static size_t sk_handle_write_oob(Socket *s, const void *data, size_t len)
{
    /*
     * oob data is treated as inband; nasty, but nothing really
     * better we can do
     */
    return sk_handle_write(s, data, len);
}

static void sk_handle_write_eof(Socket *s)
{
    HandleSocket *hs = container_of(s, HandleSocket, sock);

    handle_write_eof(hs->send_h);
}

static void handle_socket_unfreeze(void *hsv)
{
    HandleSocket *hs = (HandleSocket *)hsv;

    /*
     * If we've been put into a state other than THAWING since the
     * last callback, then we're done.
     */
    if (hs->frozen != THAWING)
        return;

    /*
     * Get some of the data we've buffered.
     */
    { // WINSCP
    ptrlen data = bufchain_prefix(&hs->inputdata);
    assert(data.len > 0);

    /*
     * Hand it off to the plug. Be careful of re-entrance - that might
     * have the effect of trying to close this socket.
     */
    hs->defer_close = true;
    plug_receive(hs->plug, 0, data.ptr, data.len);
    bufchain_consume(&hs->inputdata, data.len);
    hs->defer_close = false;
    if (hs->deferred_close) {
        sk_handle_close(&hs->sock);
        return;
    }

    if (bufchain_size(&hs->inputdata) > 0) {
        /*
         * If there's still data in our buffer, stay in THAWING state,
         * and reschedule ourself.
         */
        queue_toplevel_callback(handle_socket_unfreeze, hs);
    } else {
        /*
         * Otherwise, we've successfully thawed!
         */
        hs->frozen = UNFROZEN;
        handle_unthrottle(hs->recv_h, 0);
    }
    } // WINSCP
}

static void sk_handle_set_frozen(Socket *s, bool is_frozen)
{
    HandleSocket *hs = container_of(s, HandleSocket, sock);

    if (is_frozen) {
        switch (hs->frozen) {
          case FREEZING:
          case FROZEN:
            return;                    /* nothing to do */

          case THAWING:
            /*
             * We were in the middle of emptying our bufchain, and got
             * frozen again. In that case, handle-io.c is already
             * throttled, so just return to FROZEN state. The toplevel
             * callback will notice and disable itself.
             */
            hs->frozen = FROZEN;
            break;

          case UNFROZEN:
            /*
             * The normal case. Go to FREEZING, and expect one more
             * load of data from winhandl if we're unlucky.
             */
            hs->frozen = FREEZING;
            break;
        }
    } else {
        switch (hs->frozen) {
          case UNFROZEN:
          case THAWING:
            return;                    /* nothing to do */

          case FREEZING:
            /*
             * If winhandl didn't send us any data throughout the time
             * we were frozen, then we'll still be in this state and
             * can just unfreeze in the trivial way.
             */
            assert(bufchain_size(&hs->inputdata) == 0);
            hs->frozen = UNFROZEN;
            break;

          case FROZEN:
            /*
             * If we have buffered data, go to THAWING and start
             * releasing it in top-level callbacks.
             */
            hs->frozen = THAWING;
            queue_toplevel_callback(handle_socket_unfreeze, hs);
        }
    }
}

static const char *sk_handle_socket_error(Socket *s)
{
    HandleSocket *hs = container_of(s, HandleSocket, sock);
    return hs->error;
}

static SocketEndpointInfo *sk_handle_endpoint_info(Socket *s, bool peer)
{
    HandleSocket *hs = container_of(s, HandleSocket, sock);
    ULONG pid;
    static HMODULE kernel32_module;
    DECL_WINDOWS_FUNCTION(static, BOOL, GetNamedPipeClientProcessId,
                          (HANDLE, PULONG));

    if (!peer)
        return NULL;

    if (!kernel32_module) {
        kernel32_module = load_system32_dll("kernel32.dll");
#if !HAVE_GETNAMEDPIPECLIENTPROCESSID
        /* For older Visual Studio, and MinGW too (at least as of
         * Ubuntu 16.04), this function isn't available in the header
         * files to type-check. Ditto the toolchain I use for
         * Coveritying the Windows code. */
        GET_WINDOWS_FUNCTION_NO_TYPECHECK(
            kernel32_module, GetNamedPipeClientProcessId);
#else
        GET_WINDOWS_FUNCTION(
            kernel32_module, GetNamedPipeClientProcessId);
#endif
    }

    /*
     * Of course, not all handles managed by this module will be
     * server ends of named pipes, but if they are, then it's useful
     * to log what we can find out about the client end.
     */
    if (p_GetNamedPipeClientProcessId &&
        p_GetNamedPipeClientProcessId(hs->send_H, &pid)) {
        SocketEndpointInfo *pi = snew(SocketEndpointInfo);
        pi->addressfamily = ADDRTYPE_LOCAL;
        pi->addr_text = NULL;
        pi->port = -1;
        pi->log_text = dupprintf("process id %lu", (unsigned long)pid);
        return pi;
    }

    return NULL;
}

static const SocketVtable HandleSocket_sockvt = {
    // WINSCP
    /*.plug =*/ sk_handle_plug,
    /*.close =*/ sk_handle_close,
    /*.write =*/ sk_handle_write,
    /*.write_oob =*/ sk_handle_write_oob,
    /*.write_eof =*/ sk_handle_write_eof,
    /*.set_frozen =*/ sk_handle_set_frozen,
    /*.socket_error =*/ sk_handle_socket_error,
    /*.endpoint_info =*/ sk_handle_endpoint_info,
};

static void sk_handle_connect_success_callback(void *ctx)
{
    HandleSocket *hs = (HandleSocket *)ctx;
    plug_log(hs->plug, &hs->sock, PLUGLOG_CONNECT_SUCCESS, hs->addr, hs->port,
             NULL, 0);
}

Socket *make_handle_socket(HANDLE send_H, HANDLE recv_H, HANDLE stderr_H,
                           SockAddr *addr, int port, Plug *plug,
                           bool overlapped)
{
    HandleSocket *hs;
    int flags = (overlapped ? HANDLE_FLAG_OVERLAPPED : 0);
    struct callback_set * callback_set = get_callback_set(plug); // WINSCP

    hs = snew(HandleSocket);
    hs->sock.vt = &HandleSocket_sockvt;
    hs->addr = addr;
    hs->port = port;
    hs->plug = plug;
    hs->error = NULL;

    hs->frozen = UNFROZEN;
    bufchain_init(&hs->inputdata);
    psb_init(&hs->psb);

    hs->recv_H = recv_H;
    hs->recv_h = handle_input_new(callback_set, hs->recv_H, handle_gotdata, hs, flags); // WINSCP
    hs->send_H = send_H;
    hs->send_h = handle_output_new(callback_set, hs->send_H, handle_sentdata, hs, flags); // WINSCP
    hs->stderr_H = stderr_H;
    hs->stderr_h = NULL; // WINSCP
    if (hs->stderr_H)
        hs->stderr_h = handle_input_new(callback_set, hs->stderr_H, handle_stderr, // WINSCP
                                        hs, flags);

    hs->defer_close = hs->deferred_close = false;

    #ifdef MPEXT
    // WinSCP core uses do_select as signalization of connection up/down
    do_select(plug, INVALID_SOCKET, 1);
    #endif

    queue_toplevel_callback(sk_handle_connect_success_callback, hs);

    return &hs->sock;
}

void handle_socket_set_psb_prefix(Socket *s, const char *prefix)
{
    HandleSocket *hs = container_of(s, HandleSocket, sock);
    assert(hs->sock.vt == &HandleSocket_sockvt);
    psb_set_prefix(&hs->psb, prefix);
}

static void sk_handle_deferred_close(Socket *s)
{
    HandleSocket *hs = container_of(s, HandleSocket, sock);

    deferred_socket_opener_free(hs->opener);
    bufchain_clear(&hs->outputdata);

    if (hs->addr)
        sk_addr_free(hs->addr);

    delete_callbacks_for_context(get_callback_set(hs->plug), hs);

    sfree(hs);
}

static size_t sk_handle_deferred_write(Socket *s, const void *data, size_t len)
{
    HandleSocket *hs = container_of(s, HandleSocket, sock);
    assert(!hs->output_eof_pending);
    bufchain_add(&hs->outputdata, data, len);
    return bufchain_size(&hs->outputdata);
}

static void sk_handle_deferred_write_eof(Socket *s)
{
    HandleSocket *hs = container_of(s, HandleSocket, sock);
    assert(!hs->output_eof_pending);
    hs->output_eof_pending = true;
}

static void sk_handle_deferred_set_frozen(Socket *s, bool is_frozen)
{
    HandleSocket *hs = container_of(s, HandleSocket, sock);
    hs->frozen = is_frozen;
}

static SocketEndpointInfo *sk_handle_deferred_endpoint_info(
    Socket *s, bool peer)
{
    return NULL;
}

static const SocketVtable HandleSocket_deferred_sockvt = {
    // WINSCP
    /*.plug =*/ sk_handle_plug,
    /*.close =*/ sk_handle_deferred_close,
    /*.write =*/ sk_handle_deferred_write,
    /*.write_oob =*/ sk_handle_deferred_write,
    /*.write_eof =*/ sk_handle_deferred_write_eof,
    /*.set_frozen =*/ sk_handle_deferred_set_frozen,
    /*.socket_error =*/ sk_handle_socket_error,
    /*.endpoint_info =*/ sk_handle_deferred_endpoint_info,
};

Socket *make_deferred_handle_socket(DeferredSocketOpener *opener,
                                    SockAddr *addr, int port, Plug *plug)
{
    HandleSocket *hs = snew(HandleSocket);
    hs->sock.vt = &HandleSocket_deferred_sockvt;
    hs->addr = addr;
    hs->port = port;
    hs->plug = plug;
    hs->error = NULL;

    hs->opener = opener;
    bufchain_init(&hs->outputdata);
    hs->output_eof_pending = false;
    hs->start_frozen = false;

    return &hs->sock;
}

void setup_handle_socket(Socket *s, HANDLE send_H, HANDLE recv_H,
                         HANDLE stderr_H, bool overlapped)
{
    HandleSocket *hs = container_of(s, HandleSocket, sock);
    pinitassert(hs->sock.vt == &HandleSocket_deferred_sockvt);
    struct callback_set * callback_set = get_callback_set(hs->plug);

    int flags = (overlapped ? HANDLE_FLAG_OVERLAPPED : 0);

    struct handle *recv_h = handle_input_new(
        callback_set, recv_H, handle_gotdata, hs, flags);
    struct handle *send_h = handle_output_new(
        callback_set, send_H, handle_sentdata, hs, flags);
    struct handle *stderr_h = !stderr_H ? NULL : handle_input_new(
        callback_set, stderr_H, handle_stderr, hs, flags);

    while (bufchain_size(&hs->outputdata)) {
        ptrlen data = bufchain_prefix(&hs->outputdata);
        handle_write(send_h, data.ptr, data.len);
        bufchain_consume(&hs->outputdata, data.len);
    }

    if (hs->output_eof_pending)
        handle_write_eof(send_h);

    { // WINSCP
    bool start_frozen = hs->start_frozen;

    deferred_socket_opener_free(hs->opener);
    bufchain_clear(&hs->outputdata);

    hs->sock.vt = &HandleSocket_sockvt;
    hs->frozen = start_frozen ? FREEZING : UNFROZEN;
    bufchain_init(&hs->inputdata);
    psb_init(&hs->psb);

    hs->recv_H = recv_H;
    hs->recv_h = recv_h;
    hs->send_H = send_H;
    hs->send_h = send_h;
    hs->stderr_H = stderr_H;
    hs->stderr_h = stderr_h;

    hs->defer_close = hs->deferred_close = false;

    #ifdef MPEXT
    // WinSCP core uses do_select as signalization of connection up/down
    do_select(hs->plug, INVALID_SOCKET, 1);
    #endif

    queue_toplevel_callback(sk_handle_connect_success_callback, hs);
    } // WINSCP
}
