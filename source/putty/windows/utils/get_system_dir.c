/*
 * Wrapper function around GetSystemDirectory that deals with
 * allocating the output buffer, and also caches the result for future
 * calls.
 */

#include "putty.h"

#ifdef WINSCP

bool free_sysdir = false;

void win_misc_cleanup()
{
    free_sysdir = true;
    get_system_dir();
    free_sysdir = false;
}

#endif

const char *get_system_dir(void)
{
    static char *sysdir = NULL;
    static size_t sysdirsize = 0;

    #ifdef WINSCP
    if (free_sysdir)
    {
        sfree(sysdir);
        return NULL;
    }
    #endif

    if (!sysdir) {
        size_t len;
        while ((len = GetSystemDirectory(sysdir, sysdirsize)) >= sysdirsize)
            sgrowarray(sysdir, sysdirsize, len);
    }

    return sysdir;
}
