/* 
   Character lookup table generator
   Copyright (C) 2022, Joe Orton <joe@manyfish.co.uk>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA

*/

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

typedef unsigned char (*generator)(unsigned char ch);

static unsigned char gen_clean(unsigned char ch)
{
    return isprint(ch) ? ch : 0x20;
}

static unsigned char gen_lower(unsigned char ch)
{
    return tolower(ch);
}

static unsigned char gen_quote(unsigned char ch)
{
    return !isascii(ch) || !isprint(ch) ? 4 : 1;
}

/* VALID_B64: fail if 'ch' is not a valid base64 character */
#define VALID_B64(ch) (((ch) >= 'A' && (ch) <= 'Z') || \
                       ((ch) >= 'a' && (ch) <= 'z') || \
                       ((ch) >= '0' && (ch) <= '9') || \
                       (ch) == '/' || (ch) == '+' || (ch) == '=')

/* DECODE_B64: decodes a valid base64 character. */
#define DECODE_B64(ch) ((ch) >= 'a' ? ((ch) + 26 - 'a') : \
                        ((ch) >= 'A' ? ((ch) - 'A') : \
                         ((ch) >= '0' ? ((ch) + 52 - '0') : \
                          ((ch) == '+' ? 62 : 63))))

static unsigned char valid_b64(unsigned char ch)
{
    return VALID_B64(ch) ? 1 : 0;
}

static unsigned char decode_b64(unsigned char ch)
{
    return DECODE_B64(ch);
}

/* Username safety lookup table; "SF" for characters which are safe to
 * include in 2617-style username parameter values, else "NS" for
 * non-safe characters.  Determined from RFC7230§3.2.6 - everything in
 * qdtext EXCEPT obs-text (which is "non-ASCII") is treated as safe to
 * include in a quoted username:
      qdtext         = HTAB / SP /%x21 / %x23-5B / %x5D-7E / obs-text
     obs-text       = %x80-FF
*/
static unsigned char safe_username(unsigned char ch)
{
    return (ch == '\t' || ch == ' ' || ch == 0x21
            || (ch >= 0x23 && ch <= 0x7E && ch != 0x5C))
            ? 0 : 1;
}

#define QT 3
#define NQ 1

static unsigned char gen_extparam(unsigned char ch)
{
    switch (ch) {
    case '!': case '#': case '$': case '&': case '+': case '-': case '.':
    case '^': case '_': case '`': case '|': case '~':
        return NQ;
    default:
        return isalnum(ch) ? NQ : QT;
    }
}

/*
 * Map: '0'-'9' => 0-9
 * reason-phrase characters => 0-10
 * bad things => 99
 *  
 * RFC 9112: reason-phrase  = 1*( HTAB / SP / VCHAR / obs-text )
 * RFC 5234: VCHAR          = %x21-7E
 * RFC 9110: obs-text       = %x80-FF
 */
static unsigned char gen_status_line(unsigned char ch)
{
    if (ch >= '0' && ch <= '9')
        return ch - '0';

    if (ch == '\t' || ch == ' ' 
        || (ch >= 0x21 && ch != 0x7F)) {
        return 10;
    }

    return 99;
}

/* https://www.rfc-editor.org/rfc/rfc9110#name-tokens
 token          = 1*tchar

 tchar          = "!" / "#" / "$" / "%" / "&" / "'" / "*"
                   / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~"
                   / DIGIT / ALPHA
                   ; any VCHAR, except delimiters

 VCHAR          =  %x21-7E
 delimiters = (DQUOTE and "(),/:;<=>?@[\]{}")
*/

static unsigned char gen_token(unsigned char ch)
{
    switch (ch) {
    case '!': case '#': case '$': case '%': case '&': case '\'': case '*':
    case '+': case '-': case '.': case '^': case '_': case '`': case '|':
    case '~':
        return ch;
    case '(': case ')': case ',': case '/': case ':': case ';': case '<':
    case '=': case '>': case '?': case '@': case '[': case '\\': case ']':
    case '{': case '}': case '"':
        return 0;
    default:
        return ch >= 0x21 && ch <= 0x7E ? tolower(ch) : 0;
    }
}

#define FLAG_DECIMAL (0x01)
#define FLAG_SHORT   (0x02)

static const struct {
    const char *name;
    generator fn;
    unsigned flags;
} generators[] = {
    { "strclean", gen_clean, 0 },
    { "tolower", gen_lower, 0 },
    { "validb64", valid_b64, FLAG_DECIMAL | FLAG_SHORT },
    { "decodeb64", decode_b64, 0 },
    { "quote", gen_quote, FLAG_DECIMAL | FLAG_SHORT },
    { "status_line", gen_status_line, FLAG_DECIMAL | FLAG_SHORT },
    { "extparam", gen_extparam, FLAG_DECIMAL | FLAG_SHORT },
    { "safe_username", safe_username, FLAG_DECIMAL | FLAG_SHORT },
    { "http_token", gen_token, 0 },
};

static void fail(const char *err, const char *arg)
{
    printf("mktable: %s (%s)\n", err, arg);
    exit(1);
}

int main(int argc, const char **argv)
{
    generator fn = NULL;
    unsigned n, wrap, flags = 0;

    if (argc != 2) {
        for (n = 0; n < sizeof(generators) / sizeof(generators[0]); n++) {
            puts(generators[n].name);
        }
        return 0;
    }

    for (n = 0; n < sizeof(generators) / sizeof(generators[0]); n++) {
        if (strcmp(generators[n].name, argv[1]) == 0) {
            fn = generators[n].fn;
            flags = generators[n].flags;
            break;
        }
    }

    if (fn == NULL) {
        fail("Unrecognized generator name", argv[1]);
    }

    wrap = (flags & FLAG_SHORT) ? 16 : 8;
    
    printf("/* Generated with 'mktable %s', do not alter here -- */\n", argv[1]);
    printf("static const unsigned char table_%s[256] = {", argv[1]);
    for (n = 0; n < 256; n++) {
        unsigned ch = fn(n);
        if (n % wrap == 0)
            printf("%s\n/* x%02X */ ", n > 0 ? "," : "", n);
        else
            printf(", ");
        if (flags & FLAG_DECIMAL)
            printf("%u", ch);
        else
            printf("0x%02x", ch);
    }

    printf("\n}; /* -- Generated code from 'mktable %s' ends. */\n", argv[1]);

    return 0;
}
