/* 
   Common SSL/TLS handling routines
   Copyright (C) 2001-2026, Joe Orton <joe@manyfish.co.uk>

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

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include "ne_alloc.h"
#include "ne_utils.h"
#include "ne_internal.h"
#include "ne_string.h"
#include "ne_privssl.h"

#ifdef NE_HAVE_SSL
/* This doesn't actually implement complete RFC 2818 logic; omits
 * "f*.example.com" support for simplicity. */
int ne__ssl_match_hostname(const char *cn, size_t cnlen, const char *hostname)
{
    const char *dot;

    if (!hostname) {
        return 0;
    }

    NE_DEBUG(NE_DBG_SSL, "ssl: Match common name '%s' against '%s'\n",
             cn, hostname);

    if (strncmp(cn, "*.", 2) == 0 && cnlen > 2
        && (dot = strchr(hostname, '.')) != NULL) {
	hostname = dot + 1;
	cn += 2;
        cnlen -= 2;
    }

    return cnlen == strlen(hostname) && !ne_strcasecmp(cn, hostname);
}

void ne_ssl_context_set_ccprovide(ne_ssl_context *ctx,
                                  ne_ssl_ccprovide_fn provider, void *userdata)
{
    ctx->provider = provider;
    ctx->provider_ud = userdata;
}

#endif
