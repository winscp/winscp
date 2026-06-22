/*
 * 'Burn' a dynamically allocated wide string, in the sense of
 * destroying it beyond recovery: overwrite it with zeroes and then
 * free it.
 */

#include <wchar.h>

#include "defs.h"
#include "misc.h"

void burnwcs(wchar_t *string)
{
    if (string) {
        smemclr(string, sizeof(*string) * wcslen(string));
        sfree(string);
    }
}
