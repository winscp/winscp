/*
 * Linking module for PuTTY proper: list the available backends
 * including ssh.
 */

#include <windows.h>
#include <stdio.h>
#include "putty.h"

struct backend_list backends[] = {
    {PROT_SSH, "ssh", &ssh_backend},
    {PROT_TELNET, "telnet", &telnet_backend},
    {PROT_RLOGIN, "rlogin", &rlogin_backend},
    {PROT_RAW, "raw", &raw_backend},
    {0, NULL}
};
