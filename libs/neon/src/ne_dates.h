/* 
   Date manipulation routines
   Copyright (C) 1999-2021, Joe Orton <joe@manyfish.co.uk>

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

#ifndef NE_DATES_H
#define NE_DATES_H

#include <sys/types.h>

#include "ne_defs.h"

NE_BEGIN_DECLS

/* Convert time to a string following the Internet Message Format RFC
 * standard (historically known as "RFC1123", currently known as
 * RFC5322). Returns a malloc-allocated string, or NULL if there is an
 * error converting the time. */
char *ne_rfc1123_date(time_t anytime)
    ne_attribute_malloc;

/* Parses a date/time using the ISO8601 format. Returns -1 on
 * error. */
time_t ne_iso8601_parse(const char *date)
    ne_attribute((nonnull));

/* Parses a date/time using the IMF-fixdate format (historically
 * known as "RFC1123". Returns -1 on error. */
time_t ne_rfc1123_parse(const char *date)
    ne_attribute((nonnull));

/* Parses a date/time using the RFC1036 format. Returns -1 on
 * error. */
time_t ne_rfc1036_parse(const char *date)
    ne_attribute((nonnull));

/* Parses a libc "asctime" format date. Returns -1 on error. */
time_t ne_asctime_parse(const char *date)
    ne_attribute((nonnull));

/* Parse an HTTP-date as perq RFC2616. Returns -1 on error. */
time_t ne_httpdate_parse(const char *date)
    ne_attribute((nonnull));

NE_END_DECLS

#endif /* NE_DATES_H */
