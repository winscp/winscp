/** **************************************************************************
 * util.h
 * 
 * Copyright 2008 Bryan Ischo <bryan@ischo.com>
 *
 * This file is part of libs3.
 *
 * libs3 is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, version 3 or above of the License.  You can also
 * redistribute and/or modify it under the terms of the GNU General Public
 * License, version 2 or above of the License.
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
 * <https://www.gnu.org/licenses/>.
 *
 * You should also have received a copy of the GNU General Public License
 * version 2 along with libs3, in a file named COPYING-GPLv2.  If not, see
 * <https://www.gnu.org/licenses/>.
 *
 ************************************************************************** **/

#ifndef UTIL_H
#define UTIL_H

#ifndef WINSCP
#include <curl/curl.h>
#include <curl/multi.h>
#endif
#include <stdint.h>
#include "libs3.h"

// acl groups
#define ACS_URL "http://acs.amazonaws.com/groups/"

#define ACS_GROUP_ALL_USERS     ACS_URL "global/AllUsers"
#define ACS_GROUP_AWS_USERS     ACS_URL "global/AuthenticatedUsers"
#define ACS_GROUP_LOG_DELIVERY  ACS_URL "s3/LogDelivery"


// Derived from S3 documentation

// This is the maximum number of bytes needed in a "compacted meta header"
// buffer, which is a buffer storing all of the compacted meta headers.
#define COMPACTED_METADATA_BUFFER_SIZE \
    (S3_MAX_METADATA_COUNT * sizeof(S3_METADATA_HEADER_NAME_PREFIX "n: v"))

// Maximum url encoded key size; since every single character could require
// URL encoding, it's 3 times the size of a key (since each url encoded
// character takes 3 characters: %NN)
#define MAX_URLENCODED_KEY_SIZE (3 * S3_MAX_KEY_SIZE)

// This is the maximum size of a URI that could be passed to S3:
// https://s3.amazonaws.com/${BUCKET}/${KEY}?acl
// 255 is the maximum bucket length
#define MAX_URI_SIZE \
    ((sizeof("https:///") - 1) + S3_MAX_HOSTNAME_SIZE + 255 + 1 +       \
     MAX_URLENCODED_KEY_SIZE + (sizeof("?torrent") - 1) + 1)

// Maximum size of a canonicalized resource
#define MAX_CANONICALIZED_RESOURCE_SIZE \
    (1 + 255 + 1 + MAX_URLENCODED_KEY_SIZE + (sizeof("?torrent") - 1) + 1)

#define MAX_ACCESS_KEY_ID_LENGTH S3_MAX_ACCESS_KEY_ID_LENGTH

#define S3_MAX_SCOPE_LENGTH 3 // WINSCP "s3"/"sts"

// Maximum length of a credential string
// <access key>/<yyyymmdd>/<region>/<scope>/aws4_request // WINSCP
#define MAX_CREDENTIAL_SIZE \
   (MAX_ACCESS_KEY_ID_LENGTH + 1) + 8 + 1 + S3_MAX_REGION_LENGTH + sizeof("//aws4_request") + S3_MAX_SCOPE_LENGTH // WINSCP

// Utilities -----------------------------------------------------------------

// URL-encodes a string from [src] into [dest].  [dest] must have at least
// 3x the number of characters that [source] has.   At most [maxSrcSize] bytes
// from [src] are encoded; if more are present in [src], 0 is returned from
// urlEncode, else nonzero is returned.
int urlEncode(char *dest, const char *src, int maxSrcSize, int encodeSlash);

// Returns < 0 on failure >= 0 on success
int64_t parseIso8601Time(const char *str);

uint64_t parseUnsignedInt(const char *str);

// Because Windows seems to be missing isblank(), use our own; it's a very
// easy function to write in any case
int is_blank(char c);

#endif /* UTIL_H */
