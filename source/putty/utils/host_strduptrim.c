/*
 * Trim square brackets off the outside of an IPv6 address literal.
 * Leave all other strings unchanged. Returns a fresh dynamically
 * allocated string.
 */

#include <ctype.h>
#include <string.h>

#include "defs.h"
#include "misc.h"

char *host_strduptrim(const char *s)
{
    if (s[0] == '[') {
        const char *p = s+1;
        int colons = 0;
        while (*p && *p != ']') {
            if (isxdigit((unsigned char)*p))
                /* OK */;
            else if (*p == ':')
                colons++;
            else
                break;
            p++;
        }
        if (*p == '%') {
            /*
             * This delimiter character introduces an RFC 4007 scope
             * id suffix (e.g. suffixing the address literal with
             * %eth1 or %2 or some such). There's no syntax
             * specification for the scope id, so just accept anything
             * except the closing ].
             */
            p += strcspn(p, "]");
        }
        if (*p == ']' && !p[1] && colons > 1) {
            /*
             * This looks like an IPv6 address literal (hex digits and
             * at least two colons, plus optional scope id, contained
             * in square brackets). Trim off the brackets.
             */
            return dupprintf("%.*s", (int)(p - (s+1)), s+1);
        }
    }

    /*
     * Any other shape of string is simply duplicated.
     */
    return dupstr(s);
}
