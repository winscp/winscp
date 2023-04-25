/*
 * nosshproxy.c: stub implementation of sshproxy_new_connection().
 */

#include "putty.h"
#include "network.h"

const bool ssh_proxy_supported = false;

Socket *sshproxy_new_connection(SockAddr *addr, const char *hostname,
                                int port, bool privport,
                                bool oobinline, bool nodelay, bool keepalive,
                                Plug *plug, Conf *conf, Interactor *itr)
{
    return NULL;
}
