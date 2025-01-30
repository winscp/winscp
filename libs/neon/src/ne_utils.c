/* 
   HTTP utility functions
   Copyright (C) 1999-2024, Joe Orton <joe@manyfish.co.uk>

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

#include "config.h"

#include <sys/types.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <stdio.h>
#include <ctype.h> /* isdigit() for ne_parse_statusline */

#ifdef NE_HAVE_ZLIB
#include <zlib.h>
#endif

#ifdef HAVE_OPENSSL
#include <openssl/opensslv.h>
#endif

#ifdef HAVE_GNUTLS
#include <gnutls/gnutls.h>
#endif

/* libxml2: pick up the version string. */
#if defined(HAVE_LIBXML)
#include <libxml/xmlversion.h>
#elif defined(HAVE_EXPAT) && !defined(HAVE_XMLPARSE_H)
#include <expat.h>
#endif

#include "ne_utils.h"
#include "ne_string.h" /* for ne_strdup */
#include "ne_dates.h"

int ne_debug_mask = 0;

#ifdef WINSCP

void * ne_debug_context = NULL;

#else

FILE *ne_debug_stream = NULL;

void ne_debug_init(FILE *stream, int mask)
{
    ne_debug_stream = stream;
    ne_debug_mask = mask;
#if defined(HAVE_SETVBUF) && defined(_IONBF)
    /* If possible, turn off buffering on the debug log.  this is very
     * helpful if debugging segfaults. */
    if (stream) setvbuf(stream, NULL, _IONBF, 0);
#endif        
}

void ne_debug(int ch, const char *template, ...) 
{
    va_list params;
    if ((ch & ne_debug_mask) == 0) return;
    fflush(stdout);
    va_start(params, template);
    vfprintf(ne_debug_stream, template, params);
    va_end(params);
    if ((ch & NE_DBG_FLUSH) == NE_DBG_FLUSH)
	fflush(ne_debug_stream);
}

#endif /* WINSCP */

#define NE_STRINGIFY(x) # x
#define NE_EXPAT_VER(x,y,z) NE_STRINGIFY(x) "." NE_STRINGIFY(y) "." NE_STRINGIFY(z)

static const char version_string[] = "neon " NEON_VERSION ": " 
#ifdef NEON_IS_LIBRARY
  "Library build"
#else
  "Bundled build"
#endif
#ifdef NE_HAVE_IPV6
   ", IPv6"
#endif
#ifdef HAVE_EXPAT
  ", Expat"
/* expat >=1.95.2 exported the version */
#ifdef XML_MAJOR_VERSION
" " NE_EXPAT_VER(XML_MAJOR_VERSION, XML_MINOR_VERSION, XML_MICRO_VERSION)
#endif
#else /* !HAVE_EXPAT */
#ifdef HAVE_LIBXML
  ", libxml " LIBXML_DOTTED_VERSION
#endif /* HAVE_LIBXML */
#endif /* !HAVE_EXPAT */
#if defined(NE_HAVE_ZLIB) && defined(ZLIB_VERSION)
  ", zlib " ZLIB_VERSION
#endif /* NE_HAVE_ZLIB && ... */
#ifdef NE_HAVE_SOCKS
   ", SOCKSv5"
#endif
#ifdef NE_HAVE_LFS
   ", LFS"
#endif
#ifdef HAVE_OPENSSL
#ifdef OPENSSL_VERSION_TEXT
    ", " OPENSSL_VERSION_TEXT
#else
   "OpenSSL (unknown version)"
#endif /* OPENSSL_VERSION_TEXT */
#ifdef NE_HAVE_TS_SSL
    " (thread-safe)"
#endif
#endif /* HAVE_OPENSSL */
#ifdef HAVE_GNUTLS
    ", GNU TLS " LIBGNUTLS_VERSION
#endif /* HAVE_GNUTLS */
#ifdef HAVE_SSPI
    ", SSPI"
#endif /* HAVE_SSPI */
#ifdef HAVE_PAKCHOIS
    ", PKCS#11"
#endif
#ifdef NE_HAVE_LIBPXY
    ", libproxy"
#endif
   "."
;

const char *ne_version_string(void)
{
    return version_string;
}

#define LAST_COMPAT_ZERO_MINOR (27)

int ne_version_match(int major, int minor)
{
    return !
        (NE_VERSION_MAJOR == 0 &&
         (minor <= NE_VERSION_MINOR && minor >= LAST_COMPAT_ZERO_MINOR));
}

int ne_has_support(int feature)
{
    switch (feature) {
#if defined(NE_HAVE_SSL) || defined(NE_HAVE_ZLIB) || defined(NE_HAVE_IPV6) \
    || defined(NE_HAVE_SOCKS) || defined(NE_HAVE_LFS) \
    || defined(NE_HAVE_TS_SSL) || defined(NE_HAVE_I18N) || defined(HAVE_SSPI)
#ifdef NE_HAVE_SSL
    case NE_FEATURE_SSL:
#endif
#ifdef NE_HAVE_ZLIB
    case NE_FEATURE_ZLIB:
#endif
#ifdef NE_HAVE_IPV6
    case NE_FEATURE_IPV6:
#endif
#ifdef NE_HAVE_SOCKS
    case NE_FEATURE_SOCKS:
#endif
#ifdef NE_HAVE_LFS
    case NE_FEATURE_LFS:
#endif
#ifdef NE_HAVE_TS_SSL
    case NE_FEATURE_TS_SSL:
#endif
#ifdef NE_HAVE_I18N
    case NE_FEATURE_I18N:
#endif
#ifdef HAVE_SSPI
    case NE_FEATURE_SSPI:
#endif
#ifdef NE_HAVE_GSSAPI
    case NE_FEATURE_GSSAPI:
#endif
#ifdef NE_HAVE_LIBPXY
    case NE_FEATURE_LIBPXY:
#endif
        return 1;
#endif /* NE_HAVE_* */
    default:
        return 0;
    }
}

/* Lookup table - digit values=0-9, reason_phrase=0-10. */

/* Generated with 'mktable status_line', do not alter here -- */
static const unsigned char table_status_line[256] = {
/* x00 */ 99, 99, 99, 99, 99, 99, 99, 99, 99, 10, 99, 99, 99, 99, 99, 99,
/* x10 */ 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
/* x20 */ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
/* x30 */ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10, 10, 10,
/* x40 */ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
/* x50 */ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
/* x60 */ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
/* x70 */ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 99,
/* x80 */ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
/* x90 */ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
/* xA0 */ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
/* xB0 */ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
/* xC0 */ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
/* xD0 */ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
/* xE0 */ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
/* xF0 */ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10
}; /* -- Generated code from 'mktable status_line' ends. */

/* Strict parser per RFC9112ẞ4:
 *
 *    status-line = HTTP-version SP status-code SP [ reason-phrase ]
 *  HTTP-version  = HTTP-name "/" DIGIT "." DIGIT
 * reason-phrase  = 1*( HTAB / SP / VCHAR / obs-text )
 */
int ne_parse_statusline(const char *status_line, ne_status *st)
{
    const unsigned char *p = (const unsigned char *)status_line, *rp;
    unsigned int major, minor, status_code, klass;

    /* p => status-line */
    if (strncmp((const char *)p, "HTTP/", 5) != 0)
        return -1;

    minor = major = 0;
    p += 5;

    /* X.Y */
    if ((major = table_status_line[*p++]) > 9)
        return -1;
    if (*p++ != '.')
        return -1;
    if ((minor = table_status_line[*p++]) > 9)
        return -1;

    if (*p++ != ' ') return -1;

    if ((klass = table_status_line[p[0]]) > 5 /* note 5xx maximum */
        || table_status_line[p[1]] > 9 || table_status_line[p[2]] > 9
        || p[3] != ' ')
        return -1;

    status_code = klass * 100 + table_status_line[p[1]] * 10
        + table_status_line[p[2]];

    rp = p += 4; /* p => [ reason-phrase ] */
    while (table_status_line[*p] < 11) /* note this terminates for *p == '\0' */
        p++;

    /* Fill in the results */
    st->major_version = major;
    st->minor_version = minor;
    st->reason_phrase = ne_malloc(p - rp + 1);
    ne_strnzcpy(st->reason_phrase, (const char *)rp, p - rp + 1);
    ne_strclean(st->reason_phrase);
    st->code = status_code;
    st->klass = klass;
    return 0;
}
