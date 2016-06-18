/*
 * A dummy Socket implementation which just holds an error message.
 */

#include <stdio.h>
#include <assert.h>

#define DEFINE_PLUG_METHOD_MACROS
#include "tree234.h"
#include "putty.h"
#include "network.h"

typedef struct Socket_error_tag *Error_Socket;

struct Socket_error_tag {
    const struct socket_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */

    char *error;
    Plug plug;
};

static Plug sk_error_plug(Socket s, Plug p)
{
    Error_Socket ps = (Error_Socket) s;
    Plug ret = ps->plug;
    if (p)
	ps->plug = p;
    return ret;
}

static void sk_error_close(Socket s)
{
    Error_Socket ps = (Error_Socket) s;

    sfree(ps->error);
    sfree(ps);
}

static const char *sk_error_socket_error(Socket s)
{
    Error_Socket ps = (Error_Socket) s;
    return ps->error;
}

static char *sk_error_peer_info(Socket s)
{
    return NULL;
}

Socket new_error_socket(const char *errmsg, Plug plug)
{
    static const struct socket_function_table socket_fn_table = {
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

    Error_Socket ret;

    ret = snew(struct Socket_error_tag);
    ret->fn = &socket_fn_table;
    ret->plug = plug;
    ret->error = dupstr(errmsg);

    return (Socket) ret;
}
