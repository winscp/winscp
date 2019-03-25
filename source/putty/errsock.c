/*
 * A dummy Socket implementation which just holds an error message.
 */

#include <stdio.h>
#include <assert.h>

#define DEFINE_PLUG_METHOD_MACROS
#include "tree234.h"
#include "putty.h"
#include "network.h"

typedef struct {
    char *error;
    Plug plug;

    const Socket_vtable *sockvt;
} ErrorSocket;

static Plug sk_error_plug(Socket s, Plug p)
{
    ErrorSocket *es = FROMFIELD(s, ErrorSocket, sockvt);
    Plug ret = es->plug;
    if (p)
	es->plug = p;
    return ret;
}

static void sk_error_close(Socket s)
{
    ErrorSocket *es = FROMFIELD(s, ErrorSocket, sockvt);

    sfree(es->error);
    sfree(es);
}

static const char *sk_error_socket_error(Socket s)
{
    ErrorSocket *es = FROMFIELD(s, ErrorSocket, sockvt);
    return es->error;
}

static char *sk_error_peer_info(Socket s)
{
    return NULL;
}

static const Socket_vtable ErrorSocket_sockvt = {
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

Socket new_error_socket(const char *errmsg, Plug plug)
{
    ErrorSocket *es = snew(ErrorSocket);
    es->sockvt = &ErrorSocket_sockvt;
    es->plug = plug;
    es->error = dupstr(errmsg);
    return &es->sockvt;
}
