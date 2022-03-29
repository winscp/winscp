/*
 * Convert a size_t value to int, by saturating it at INT_MAX. Useful
 * if you want to use the printf idiom "%.*s", where the '*' precision
 * specifier expects an int in the variadic argument list, but what
 * you have is not an int but a size_t. This method of converting to
 * int will at least do something _safe_ with overlong values, even if
 * (due to the limitation of printf itself) the whole string still
 * won't be printed.
 */

#include <limits.h>

#include "defs.h"
#include "misc.h"

int string_length_for_printf(size_t s)
{
    if (s > INT_MAX)
        return INT_MAX;
    return s;
}
