/*
 * pproxy.c: dummy implementation of platform_new_connection(), to
 * be supplanted on any platform which has its own local proxy
 * method.
 */

#include "putty.h"
#include "network.h"
#include "proxy.h"

Socket platform_new_connection(SockAddr addr, char *hostname,
			       int port, int privport,
			       int oobinline, int nodelay, int keepalive,
			       Plug plug, const Config *cfg)
{
    return NULL;
}
