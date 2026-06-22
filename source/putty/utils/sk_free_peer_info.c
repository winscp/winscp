/*
 * Free a SocketEndpointInfo, and everything that dangles off it.
 */

#include "putty.h"

void sk_free_endpoint_info(SocketEndpointInfo *ei)
{
    if (ei) {
        sfree((char *)ei->addr_text);
        sfree((char *)ei->log_text);
        sfree(ei);
    }
}
