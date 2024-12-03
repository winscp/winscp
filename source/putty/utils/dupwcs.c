/*
 * Allocate a duplicate of a NUL-terminated wchar_t string.
 */

#include <wchar.h>

#include "defs.h"
#include "misc.h"

wchar_t *dupwcs(const wchar_t *s)
{
    wchar_t *p = NULL;
    if (s) {
        int len = wcslen(s);
        p = snewn(len + 1, wchar_t);
        wcscpy(p, s);
    }
    return p;
}
