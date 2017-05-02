/*
 * Centralised Unicode-related helper functions, separate from misc.c
 * so that they can be omitted from tools that aren't including
 * Unicode handling.
 */

#include "putty.h"
#include "misc.h"

wchar_t *dup_mb_to_wc_c(int codepage, int flags, const char *string, int len)
{
    int mult;
    for (mult = 1 ;; mult++) {
        wchar_t *ret = snewn(mult*len + 2, wchar_t);
        int outlen;
        outlen = mb_to_wc(codepage, flags, string, len, ret, mult*len + 1);
        if (outlen < mult*len+1) {
            ret[outlen] = L'\0';
            return ret;
        }
        sfree(ret);
    }
}

wchar_t *dup_mb_to_wc(int codepage, int flags, const char *string)
{
    return dup_mb_to_wc_c(codepage, flags, string, strlen(string));
}
