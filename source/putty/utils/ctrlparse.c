/*
 * Parse a ^C style character specification.
 * Returns NULL in `next' if we didn't recognise it as a control character,
 * in which case `c' should be ignored.
 * The precise current parsing is an oddity inherited from the terminal
 * answerback-string parsing code. All sequences start with ^; all except
 * ^<123> are two characters. The ones that are worth keeping are probably:
 *   ^?             127
 *   ^@A-Z[\]^_     0-31
 *   a-z            1-26
 *   <num>          specified by number (decimal, 0octal, 0xHEX)
 *   ~              ^ escape
 */

#include <stdlib.h>

#include "defs.h"
#include "misc.h"

char ctrlparse(char *s, char **next)
{
    char c = 0;
    if (*s != '^') {
        *next = NULL;
    } else {
        s++;
        if (*s == '\0') {
            *next = NULL;
        } else if (*s == '<') {
            s++;
            c = (char)strtol(s, next, 0);
            if ((*next == s) || (**next != '>')) {
                c = 0;
                *next = NULL;
            } else
                (*next)++;
        } else if (*s >= 'a' && *s <= 'z') {
            c = (*s - ('a' - 1));
            *next = s+1;
        } else if ((*s >= '@' && *s <= '_') || *s == '?' || (*s & 0x80)) {
            c = ('@' ^ *s);
            *next = s+1;
        } else if (*s == '~') {
            c = '^';
            *next = s+1;
        }
    }
    return c;
}
