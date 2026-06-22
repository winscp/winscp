/*
 * Encode a single code point as UTF-8.
 */

#include "defs.h"
#include "misc.h"

void BinarySink_put_utf8_char(BinarySink *output, unsigned ch)
{
    if (ch < 0x80) {
        put_byte(output, ch);
    } else if (ch < 0x800) {
        put_byte(output, 0xC0 | (ch >> 6));
        put_byte(output, 0x80 | (ch & 0x3F));
    } else if (ch < 0x10000) {
        put_byte(output, 0xE0 | (ch >> 12));
        put_byte(output, 0x80 | ((ch >> 6) & 0x3F));
        put_byte(output, 0x80 | (ch & 0x3F));
    } else {
        assert(ch <= 0x10FFFF);
        put_byte(output, 0xF0 | (ch >> 18));
        put_byte(output, 0x80 | ((ch >> 12) & 0x3F));
        put_byte(output, 0x80 | ((ch >> 6) & 0x3F));
        put_byte(output, 0x80 | (ch & 0x3F));
    }
}
