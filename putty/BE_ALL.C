/*
 * Linking module for PuTTY proper: list the available backends
 * including ssh.
 */

#include <stdio.h>
#include "putty.h"

/*
 * This appname is not strictly in the right place, since Plink
 * also uses this module. However, Plink doesn't currently use any
 * of the dialog-box sorts of things that make use of appname, so
 * it shouldn't do any harm here. I'm trying to avoid having to
 * have tiny little source modules containing nothing but
 * declarations of appname, for as long as I can...
 */
const char *const appname = "PuTTY";

#ifdef TELNET_DEFAULT
const int be_default_protocol = PROT_TELNET;
#else
const int be_default_protocol = PROT_SSH;
#endif

struct backend_list backends[] = {
    {PROT_SSH, "ssh", &ssh_backend},
    {PROT_TELNET, "telnet", &telnet_backend},
    {PROT_RLOGIN, "rlogin", &rlogin_backend},
    {PROT_RAW, "raw", &raw_backend},
    {0, NULL}
};
