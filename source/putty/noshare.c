/*
 * Stub implementation of SSH connection-sharing IPC, for any
 * platform which can't support it at all.
 */

#include <stdio.h>
#include <assert.h>
#include <errno.h>

#include "tree234.h"
#include "putty.h"
#include "ssh.h"
#include "network.h"

int platform_ssh_share(const char *name, Conf *conf,
                       Plug downplug, Plug upplug, Socket *sock,
                       char **logtext, char **ds_err, char **us_err,
                       int can_upstream, int can_downstream)
{
    return SHARE_NONE;
}

void platform_ssh_share_cleanup(const char *name)
{
}
