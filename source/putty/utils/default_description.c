/*
 * Construct a description string for a backend to use as
 * backend_description(), or a plug as plug_description().
 *
 * For some backends this will be overridden: e.g. SSH prefers to
 * think in terms of _logical_ host names (i.e. the one associated
 * with the host key) rather than the physical details of where you're
 * connecting to. But this default is good for simpler backends.
 */

#include "putty.h"

char *default_description(const BackendVtable *backvt,
                          const char *host, int port)
{
    const char *be_name = backvt->displayname_lc;

    if (backvt->default_port && port == backvt->default_port)
        return dupprintf("%s connection to %s", be_name, host);
    else
        return dupprintf("%s connection to %s port %d", be_name, host, port);
}
