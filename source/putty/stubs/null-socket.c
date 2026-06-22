/*
 * null-socket.c: provide a null implementation of any Socket vtable
 * method that might otherwise need to be reimplemented in multiple
 * places as a no-op.
 */

#include "putty.h"

SocketEndpointInfo *nullsock_endpoint_info(Socket *s, bool peer)
{
    return NULL;
}
