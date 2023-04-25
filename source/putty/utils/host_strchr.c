/*
 * strchr-like wrapper around host_strchr_internal.
 */

#include <stdbool.h>
#include <string.h>

#include "defs.h"
#include "misc.h"
#include "utils/utils.h"

char *host_strchr(const char *s, int c)
{
    char set[2];
    set[0] = c;
    set[1] = '\0';
    return (char *) host_strchr_internal(s, set, true);
}
