/*
 * Encode a single code point as UTF-8.
 */

#include "defs.h"
#include "misc.h"

size_t encode_utf8(void *output, unsigned long ch)
{
    unsigned char *start = (unsigned char *)output, *p = start;

    if (ch < 0x80) {
        *p++ = ch;
    } else if (ch < 0x800) {
        *p++ = 0xC0 | (ch >> 6);
        *p++ = 0x80 | (ch & 0x3F);
    } else if (ch < 0x10000) {
        *p++ = 0xE0 | (ch >> 12);
        *p++ = 0x80 | ((ch >> 6) & 0x3F);
        *p++ = 0x80 | (ch & 0x3F);
    } else {
        assert(ch <= 0x10FFFF);
        *p++ = 0xF0 | (ch >> 18);
        *p++ = 0x80 | ((ch >> 12) & 0x3F);
        *p++ = 0x80 | ((ch >> 6) & 0x3F);
        *p++ = 0x80 | (ch & 0x3F);
    }
    return p - start;
}
