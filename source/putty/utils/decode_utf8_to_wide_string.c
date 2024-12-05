/*
 * Decode a string of UTF-8 to a wchar_t string.
 */

#include "misc.h"

wchar_t *decode_utf8_to_wide_string(const char *s)
{
    wchar_t *ws = NULL;
    size_t wlen = 0, wsize = 0;

    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, ptrlen_from_asciz(s));

    while (get_avail(src) > 0) {
        /*
         * decode_utf8_to_wchar might emit up to 2 wchar_t if wchar_t
         * is 16 bits (because of UTF-16 surrogates), but will emit at
         * most one if wchar_t is 32-bit
         */
        sgrowarrayn(ws, wsize, wlen, 1 + (sizeof(wchar_t) < 4));

        /* We ignore 'err': if it is set, then the character decode
         * function will have emitted U+FFFD REPLACEMENT CHARACTER,
         * which is what we'd have done in response anyway. */
        { // WINSCP
        DecodeUTF8Failure err;
        wlen += decode_utf8_to_wchar(src, ws + wlen, &err);
        } // WINSCP
    }

    /* Reallocate to the final size and append the trailing NUL */
    ws = sresize(ws, wlen + 1, wchar_t);
    ws[wlen] = L'\0';

    return ws;
}
