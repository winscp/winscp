/** **************************************************************************
 * util.c
 * 
 * Copyright 2008 Bryan Ischo <bryan@ischo.com>
 * 
 * This file is part of libs3.
 * 
 * libs3 is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, version 3 of the License.
 *
 * In addition, as a special exception, the copyright holders give
 * permission to link the code of this library and its programs with the
 * OpenSSL library, and distribute linked combinations including the two.
 *
 * libs3 is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * version 3 along with libs3, in a file named COPYING.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 ************************************************************************** **/

#include <ctype.h>
#include <string.h>
#include "util.h"


// Convenience utility for making the code look nicer.  Tests a string
// against a format; only the characters specified in the format are
// checked (i.e. if the string is longer than the format, the string still
// checks out ok).  Format characters are:
// d - is a digit
// anything else - is that character
// Returns nonzero the string checks out, zero if it does not.
static int checkString(const char *str, const char *format)
{
    while (*format) {
        if (*format == 'd') {
            if (!isdigit(*str)) {
                return 0;
            }
        }
        else if (*str != *format) {
            return 0;
        }
        str++, format++;
    }

    return 1;
}

/*
 * Encode rules:
 * 1. Every byte except: 'A'-'Z', 'a'-'z', '0'-'9', '-', '.', '_', and '~'
 * 2. The space must be encoded as "%20" (and not as "+")
 * 3. Letters in the hexadecimal value must be uppercase, for example "%1A"
 * 4. Encode the forward slash character, '/', everywhere except in the object key name
 */
int urlEncode(char *dest, const char *src, int maxSrcSize, int encodeSlash)
{
    static const char *hex = "0123456789ABCDEF";

    int len = 0;

    if (src) while (*src) {
        if (++len > maxSrcSize) {
            *dest = 0;
            return 0;
        }
        unsigned char c = *src;
        if (isalnum(c) ||
            (c == '-') || (c == '_') || (c == '.') ||
            (c == '~') || (c == '/' && !encodeSlash)) {
            *dest++ = c;
        }
        else {
            *dest++ = '%';
            *dest++ = hex[c >> 4];
            *dest++ = hex[c & 15];
        }
        src++;
    }

    *dest = 0;

    return 1;
}


int64_t parseIso8601Time(const char *str)
{
    // Check to make sure that it has a valid format
    if (!checkString(str, "dddd-dd-ddTdd:dd:dd")) {
        return -1;
    }

#define nextnum() (((*str - '0') * 10) + (*(str + 1) - '0'))

    // Convert it
    struct tm stm;
    memset(&stm, 0, sizeof(stm));

    stm.tm_year = (nextnum() - 19) * 100;
    str += 2;
    stm.tm_year += nextnum();
    str += 3;

    stm.tm_mon = nextnum() - 1;
    str += 3;

    stm.tm_mday = nextnum();
    str += 3;

    stm.tm_hour = nextnum();
    str += 3;

    stm.tm_min = nextnum();
    str += 3;

    stm.tm_sec = nextnum();
    str += 2;

    stm.tm_isdst = -1;

    int64_t ret = mktime(&stm);

    // Skip the millis

    if (*str == '.') {
        str++;
        while (isdigit(*str)) {
            str++;
        }
    }

    if (checkString(str, "-dd:dd") || checkString(str, "+dd:dd")) {
        int sign = (*str++ == '-') ? -1 : 1;
        int hours = nextnum();
        str += 3;
        int minutes = nextnum();
        ret += (-sign * (((hours * 60) + minutes) * 60));
    }
    // Else it should be Z to be a conformant time string, but we just assume
    // that it is rather than enforcing that

    return ret;
}


uint64_t parseUnsignedInt(const char *str)
{
    // Skip whitespace
    while (is_blank(*str)) {
        str++;
    }

    uint64_t ret = 0;

    while (isdigit(*str)) {
        ret *= 10;
        ret += (*str++ - '0');
    }

    return ret;
}


int is_blank(char c)
{
    return ((c == ' ') || (c == '\t'));
}
