/*
 * Return the full pathname of the named pipe Pageant will listen on.
 * Used by both the Pageant server code and client code.
 */

#include "putty.h"
#include "cryptoapi.h"

char *agent_named_pipe_name(void)
{
    char *username = get_username();
    char *suffix = capi_obfuscate_string("Pageant");
    char *pipename = dupprintf("\\\\.\\pipe\\pageant.%s.%s", username, suffix);
    sfree(username);
    sfree(suffix);
    return pipename;
}
