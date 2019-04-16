/*
 * A dummy Socket implementation which just holds an error message.
 */

#include <stdio.h>
#include <assert.h>

#include "tree234.h"
#include "putty.h"
#include "network.h"

typedef struct {
    char *error;
    Plug *plug;

    Socket sock;
} ErrorSocket;

static Plug *sk_error_plug(Socket *s, Plug *p)
{
    ErrorSocket *es = container_of(s, ErrorSocket, sock);
    Plug *ret = es->plug;
    if (p)
	es->plug = p;
    return ret;
}

static void sk_error_close(Socket *s)
{
    ErrorSocket *es = container_of(s, ErrorSocket, sock);

    sfree(es->error);
    sfree(es);
}

static const char *sk_error_socket_error(Socket *s)
{
    ErrorSocket *es = container_of(s, ErrorSocket, sock);
    return es->error;
}

static SocketPeerInfo *sk_error_peer_info(Socket *s)
{
    return NULL;
}

static const SocketVtable ErrorSocket_sockvt = {
    sk_error_plug,
    sk_error_close,
    NULL /* write */,
    NULL /* write_oob */,
    NULL /* write_eof */,
    NULL /* flush */,
    NULL /* set_frozen */,
    sk_error_socket_error,
    sk_error_peer_info,
};

static Socket *new_error_socket_internal(char *errmsg, Plug *plug)
{
    ErrorSocket *es = snew(ErrorSocket);
    es->sock.vt = &ErrorSocket_sockvt;
    es->plug = plug;
    es->error = errmsg;
    return &es->sock;
}

Socket *new_error_socket_fmt(Plug *plug, const char *fmt, ...)
{
    va_list ap;
    char *msg;

    va_start(ap, fmt);
    msg = dupvprintf(fmt, ap);
    va_end(ap);

    return new_error_socket_internal(msg, plug);
}
