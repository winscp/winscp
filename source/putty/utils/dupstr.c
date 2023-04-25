/*
 * Allocate a duplicate of an ordinary C NUL-terminated string.
 */

#include <string.h>

#include "defs.h"
#include "misc.h"

char *dupstr(const char *s)
{
    char *p = NULL;
    if (s) {
        int len = strlen(s);
        p = snewn(len + 1, char);
        strcpy(p, s);
    }
    return p;
}
