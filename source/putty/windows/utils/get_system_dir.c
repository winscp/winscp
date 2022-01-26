/*
 * Wrapper function around GetSystemDirectory that deals with
 * allocating the output buffer, and also caches the result for future
 * calls.
 */

#include "putty.h"

const char *get_system_dir(void)
{
    static char *sysdir = NULL;
    static size_t sysdirsize = 0;

    if (!sysdir) {
        size_t len;
        while ((len = GetSystemDirectory(sysdir, sysdirsize)) >= sysdirsize)
            sgrowarray(sysdir, sysdirsize, len);
    }

    return sysdir;
}
