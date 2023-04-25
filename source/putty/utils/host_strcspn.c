/*
 * strcspn-like wrapper around host_strchr_internal.
 */

#include <stdbool.h>
#include <string.h>

#include "defs.h"
#include "misc.h"
#include "utils/utils.h"

size_t host_strcspn(const char *s, const char *set)
{
    const char *answer = host_strchr_internal(s, set, true);
    if (answer)
        return answer - s;
    else
        return strlen(s);
}
