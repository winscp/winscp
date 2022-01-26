/*
 * Read an entire file into a BinarySink.
 */

#include <stdio.h>

#include "defs.h"
#include "misc.h"

bool read_file_into(BinarySink *bs, FILE *fp)
{
    char buf[4096];
    while (1) {
        size_t retd = fread(buf, 1, sizeof(buf), fp);
        if (retd == 0)
            return !ferror(fp);
        put_data(bs, buf, retd);
    }
}
