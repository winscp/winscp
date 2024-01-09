/*
 * Determine whether a string looks like valid base64-encoded data.
 */

#include "misc.h"

static inline bool valid_char_main(char c)
{
    return ((c >= 'A' && c <= 'Z') ||
            (c >= 'a' && c <= 'z') ||
            (c >= '0' && c <= '9') ||
            c == '+' || c == '/');
}

bool base64_valid(ptrlen data)
{
    size_t blocklen = 0, nequals = 0;

    size_t i; // WINSCP
    for (i = 0; i < data.len; i++) {
        char c = ((const char *)data.ptr)[i];

        if (c == '\n' || c == '\r')
            continue;

        if (valid_char_main(c)) {
            if (nequals)               /* can't go back to data after = */
                return false;
            blocklen++;
            if (blocklen == 4)
                blocklen = 0;
            continue;
        }

        if (c == '=') {
            if (blocklen == 0 && nequals) /* started a fresh block */
                return false;

            nequals++;
            blocklen++;
            if (blocklen == 4) {
                if (nequals > 2)
                    return false;      /* nonsensical final block */
                blocklen = 0;
            }
            continue;
        }

        return false;                  /* bad character */
    }

    if (blocklen == 0 || blocklen == 2 || blocklen == 3)
        return true;                   /* permit eliding the trailing = */
    return false;
}
