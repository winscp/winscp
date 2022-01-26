/*
 * Parse a string block size specification. This is approximately a
 * subset of the block size specs supported by GNU fileutils:
 *  "nk" = n kilobytes
 *  "nM" = n megabytes
 *  "nG" = n gigabytes
 * All numbers are decimal, and suffixes refer to powers of two.
 * Case-insensitive.
 */

#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "defs.h"
#include "misc.h"

unsigned long parse_blocksize(const char *bs)
{
    char *suf;
    unsigned long r = strtoul(bs, &suf, 10);
    if (*suf != '\0') {
        while (*suf && isspace((unsigned char)*suf)) suf++;
        switch (*suf) {
          case 'k': case 'K':
            r *= 1024ul;
            break;
          case 'm': case 'M':
            r *= 1024ul * 1024ul;
            break;
          case 'g': case 'G':
            r *= 1024ul * 1024ul * 1024ul;
            break;
          case '\0':
          default:
            break;
        }
    }
    return r;
}
