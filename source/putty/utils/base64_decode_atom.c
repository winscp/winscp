/*
 * Core routine to decode a single atomic base64 chunk.
 */

#include "defs.h"
#include "misc.h"

int base64_decode_atom(const char *atom, unsigned char *out)
{
    int vals[4];
    int i, v, len;
    unsigned word;
    char c;

    for (i = 0; i < 4; i++) {
        c = atom[i];
        if (c >= 'A' && c <= 'Z')
            v = c - 'A';
        else if (c >= 'a' && c <= 'z')
            v = c - 'a' + 26;
        else if (c >= '0' && c <= '9')
            v = c - '0' + 52;
        else if (c == '+')
            v = 62;
        else if (c == '/')
            v = 63;
        else if (c == '=')
            v = -1;
        else
            return 0;                  /* invalid atom */
        vals[i] = v;
    }

    if (vals[0] == -1 || vals[1] == -1)
        return 0;
    if (vals[2] == -1 && vals[3] != -1)
        return 0;

    if (vals[3] != -1)
        len = 3;
    else if (vals[2] != -1)
        len = 2;
    else
        len = 1;

    word = ((vals[0] << 18) |
            (vals[1] << 12) | ((vals[2] & 0x3F) << 6) | (vals[3] & 0x3F));
    out[0] = (word >> 16) & 0xFF;
    if (len > 1)
        out[1] = (word >> 8) & 0xFF;
    if (len > 2)
        out[2] = word & 0xFF;
    return len;
}
