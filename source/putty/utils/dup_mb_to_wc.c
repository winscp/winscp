/*
 * dup_mb_to_wc: memory-allocating wrapper on mb_to_wc.
 *
 * Also dup_mb_to_wc_c: same but you already know the length of the
 * string, and you get told the length of the returned wide string.
 * (But it's still NUL-terminated, for convenience.)
 */

#include "putty.h"
#include "misc.h"

wchar_t *dup_mb_to_wc_c(int codepage, const char *string,
                        size_t inlen, size_t *outlen_p)
{
    strbuf *sb = strbuf_new();
    put_mb_to_wc(sb, codepage, string, inlen);
    if (outlen_p)
        *outlen_p = sb->len / sizeof(wchar_t);

    /* Append a trailing L'\0'. For this we only need to write one
     * byte _fewer_ than sizeof(wchar_t), because strbuf will append a
     * byte '\0' for us. */
    put_padding(sb, sizeof(wchar_t) - 1, 0);
    return (wchar_t *)strbuf_to_str(sb);
}

wchar_t *dup_mb_to_wc(int codepage, const char *string)
{
    return dup_mb_to_wc_c(codepage, string, strlen(string), NULL);
}
