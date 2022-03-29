/*
 * Implementation of get_username() for Windows.
 */

#include "putty.h"

#ifndef SECURITY_WIN32
#define SECURITY_WIN32
#endif
#include <security.h>

char *get_username(void)
{
    DWORD namelen;
    char *user;
    bool got_username = false;
    DECL_WINDOWS_FUNCTION(static, BOOLEAN, GetUserNameExA,
                          (EXTENDED_NAME_FORMAT, LPSTR, PULONG));

    {
        static bool tried_usernameex = false;
        if (!tried_usernameex) {
            /* Not available on Win9x, so load dynamically */
            HMODULE secur32 = load_system32_dll("secur32.dll");
            /* If MIT Kerberos is installed, the following call to
               GET_WINDOWS_FUNCTION makes Windows implicitly load
               sspicli.dll WITHOUT proper path sanitizing, so better
               load it properly before */
            HMODULE sspicli = load_system32_dll("sspicli.dll");
            (void)sspicli; /* squash compiler warning about unused variable */
            GET_WINDOWS_FUNCTION(secur32, GetUserNameExA);
            tried_usernameex = true;
        }
    }

    if (p_GetUserNameExA) {
        /*
         * If available, use the principal -- this avoids the problem
         * that the local username is case-insensitive but Kerberos
         * usernames are case-sensitive.
         */

        /* Get the length */
        namelen = 0;
        (void) p_GetUserNameExA(NameUserPrincipal, NULL, &namelen);

        user = snewn(namelen, char);
        got_username = p_GetUserNameExA(NameUserPrincipal, user, &namelen);
        if (got_username) {
            char *p = strchr(user, '@');
            if (p) *p = 0;
        } else {
            sfree(user);
        }
    }

    if (!got_username) {
        /* Fall back to local user name */
        namelen = 0;
        if (!GetUserName(NULL, &namelen)) {
            /*
             * Apparently this doesn't work at least on Windows XP SP2.
             * Thus assume a maximum of 256. It will fail again if it
             * doesn't fit.
             */
            namelen = 256;
        }

        user = snewn(namelen, char);
        got_username = GetUserName(user, &namelen);
        if (!got_username) {
            sfree(user);
        }
    }

    return got_username ? user : NULL;
}
