/*
 * Core routine to encode a single atomic base64 chunk.
 */

#include "defs.h"
#include "misc.h"

void base64_encode_atom(const unsigned char *data, int n, char *out)
{
    static const char base64_chars[] =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    unsigned word;

    word = data[0] << 16;
    if (n > 1)
        word |= data[1] << 8;
    if (n > 2)
        word |= data[2];
    out[0] = base64_chars[(word >> 18) & 0x3F];
    out[1] = base64_chars[(word >> 12) & 0x3F];
    if (n > 1)
        out[2] = base64_chars[(word >> 6) & 0x3F];
    else
        out[2] = '=';
    if (n > 2)
        out[3] = base64_chars[word & 0x3F];
    else
        out[3] = '=';
}
