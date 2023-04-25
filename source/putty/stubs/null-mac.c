/*
 * Implementation of shared trivial routines that ssh2_mac
 * implementations might use.
 */

#include "ssh.h"

void nullmac_next_message(ssh2_mac *m)
{
    /* Most MACs don't do anything at all with this */
}
