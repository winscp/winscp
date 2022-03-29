/*
 * Free a SocketPeerInfo, and everything that dangles off it.
 */

#include "putty.h"

void sk_free_peer_info(SocketPeerInfo *pi)
{
    if (pi) {
        sfree((char *)pi->addr_text);
        sfree((char *)pi->log_text);
        sfree(pi);
    }
}
