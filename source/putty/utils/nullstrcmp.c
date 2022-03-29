/*
 * Compare two strings, just like strcmp, except that we tolerate null
 * pointers as a legal input, and define them to compare before any
 * non-null string (even the empty string).
 */

#include <string.h>

#include "defs.h"
#include "misc.h"

int nullstrcmp(const char *a, const char *b)
{
    if (a == NULL && b == NULL)
        return 0;
    if (a == NULL)
        return -1;
    if (b == NULL)
        return +1;
    return strcmp(a, b);
}
