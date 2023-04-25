/*
 * %-encoding in URL style.
 *
 * Defaults to escaping % itself (necessary for decoding to even
 * work), and any C0 escape character. Further bad characters can be
 * provided in 'badchars'.
 */

#include "misc.h"

void percent_encode_bs(BinarySink *bs, ptrlen data, const char *badchars)
{
    const char *p, *e; // WINSCP
    for (p = data.ptr, e = ptrlen_end(data); p < e; p++) {
        char c = *p;
        if (c == '%' || c < ' ' || (badchars && strchr(badchars, c)))
            put_fmt(bs, "%%%02X", (unsigned char)c);
        else
            put_byte(bs, c);
    }
}

void percent_encode_fp(FILE *fp, ptrlen data, const char *badchars)
{
    stdio_sink ss;
    stdio_sink_init(&ss, fp);
    percent_encode_bs(BinarySink_UPCAST(&ss), data, badchars);
}

strbuf *percent_encode_sb(ptrlen data, const char *badchars)
{
    strbuf *sb = strbuf_new();
    percent_encode_bs(BinarySink_UPCAST(sb), data, badchars);
    return sb;
}
