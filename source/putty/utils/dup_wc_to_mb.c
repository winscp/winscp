/*
 * dup_wc_to_mb: memory-allocating wrapper on wc_to_mb.
 *
 * Also dup_wc_to_mb_c: same but you already know the length of the
 * wide string, and you get told the length of the returned string.
 * (But it's still NUL-terminated, for convenience.).
 */

#include <wchar.h>

#include "putty.h"
#include "misc.h"

char *dup_wc_to_mb_c(int codepage, const wchar_t *string,
                     size_t inlen, const char *defchr, size_t *outlen_p)
{
    strbuf *sb = strbuf_new();
    put_wc_to_mb(sb, codepage, string, inlen, defchr);
    if (outlen_p)
        *outlen_p = sb->len;
    return strbuf_to_str(sb);
}

char *dup_wc_to_mb(int codepage, const wchar_t *string,
                   const char *defchr)
{
    return dup_wc_to_mb_c(codepage, string, wcslen(string), defchr, NULL);
}
