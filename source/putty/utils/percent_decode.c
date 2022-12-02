/*
 * Decode %-encoding in URL style.
 */

#include <ctype.h>

#include "misc.h"

void percent_decode_bs(BinarySink *bs, ptrlen data)
{
    const char *p, *e; // WINSCP
    for (p = data.ptr, e = ptrlen_end(data); p < e; p++) {
        char c = *p;
        if (c == '%' && e-p >= 3 &&
            isxdigit((unsigned char)p[1]) &&
            isxdigit((unsigned char)p[2])) {
            char hex[3];
            hex[0] = p[1];
            hex[1] = p[2];
            hex[2] = '\0';
            put_byte(bs, strtoul(hex, NULL, 16));
            p += 2;
        } else {
            put_byte(bs, c);
        }
    }

}

void percent_decode_fp(FILE *fp, ptrlen data)
{
    stdio_sink ss;
    stdio_sink_init(&ss, fp);
    percent_decode_bs(BinarySink_UPCAST(&ss), data);
}

strbuf *percent_decode_sb(ptrlen data)
{
    strbuf *sb = strbuf_new();
    percent_decode_bs(BinarySink_UPCAST(sb), data);
    return sb;
}
