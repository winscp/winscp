/** **************************************************************************
 * response_headers_handler.c
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
#ifndef WINSCP
#include <strings.h>
#endif
#include "response_headers_handler.h"
#ifdef WINSCP
#include "ne_dates.h"
#endif


void response_headers_handler_initialize(ResponseHeadersHandler *handler)
{
    handler->responseProperties.requestId = 0;
    handler->responseProperties.requestId2 = 0;
    handler->responseProperties.contentType = 0;
    handler->responseProperties.contentLength = 0;
    handler->responseProperties.server = 0;
    handler->responseProperties.eTag = 0;
    handler->responseProperties.lastModified = -1;
    handler->responseProperties.metaDataCount = 0;
    handler->responseProperties.metaData = 0;
    handler->responseProperties.usesServerSideEncryption = 0;
    handler->done = 0;
    string_multibuffer_initialize(handler->responsePropertyStrings);
    string_multibuffer_initialize(handler->responseMetaDataStrings);
}


void response_headers_handler_add(ResponseHeadersHandler *handler,
                                  const char * header_name, const char * header_value) // WINSCP (neon API)
{
    S3ResponseProperties *responseProperties = &(handler->responseProperties);
    
    // Curl might call back the header function after the body has been
    // received, for 'chunked encoded' contents.  We don't handle this as of
    // yet, and it's not clear that it would ever be useful.
    if (handler->done) {
        return;
    }

    // If we've already filled up the response headers, ignore this data.
    // This sucks, but it shouldn't happen - S3 should not be sending back
    // really long headers.
    if (handler->responsePropertyStringsSize == 
        (int)(sizeof(handler->responsePropertyStrings) - 1)) { // WINSCP (cast)
        return;
    }

    int valuelen = strlen(header_value), fit;
    const char * c = header_value;
    int namelen = strlen(header_name);
    const char * header = header_name;

    #define strncasecmp strnicmp // WINSCP

    if (!strncasecmp(header, "x-amz-request-id", namelen)) {
        responseProperties->requestId = 
            string_multibuffer_current(handler->responsePropertyStrings);
        string_multibuffer_add(handler->responsePropertyStrings, c, 
                               valuelen, fit);
    }
    else if (!strncasecmp(header, "x-amz-id-2", namelen)) {
        responseProperties->requestId2 = 
            string_multibuffer_current(handler->responsePropertyStrings);
        string_multibuffer_add(handler->responsePropertyStrings, c, 
                               valuelen, fit);
    }
    else if (!strncasecmp(header, "Content-Type", namelen)) {
        responseProperties->contentType = 
            string_multibuffer_current(handler->responsePropertyStrings);
        string_multibuffer_add(handler->responsePropertyStrings, c, 
                               valuelen, fit);
    }
    else if (!strncasecmp(header, "Content-Length", namelen)) {
        handler->responseProperties.contentLength = 0;
        while (*c) {
            handler->responseProperties.contentLength *= 10;
            handler->responseProperties.contentLength += (*c++ - '0');
        }
    }
    else if (!strncasecmp(header, "Server", namelen)) {
        responseProperties->server = 
            string_multibuffer_current(handler->responsePropertyStrings);
        string_multibuffer_add(handler->responsePropertyStrings, c, 
                               valuelen, fit);
    }
    else if (!strncasecmp(header, "ETag", namelen)) {
        responseProperties->eTag = 
            string_multibuffer_current(handler->responsePropertyStrings);
        string_multibuffer_add(handler->responsePropertyStrings, c, 
                               valuelen, fit);
    }
    else if (!strncasecmp(header, S3_METADATA_HEADER_NAME_PREFIX, 
                      sizeof(S3_METADATA_HEADER_NAME_PREFIX) - 1)) {
        // Make sure there is room for another x-amz-meta header
        if (handler->responseProperties.metaDataCount ==
            (int)sizeof(handler->responseMetaData)) { // WINSCP (cast)
            return;
        }
        // Copy the name in
        const char *metaName = &(header[sizeof(S3_METADATA_HEADER_NAME_PREFIX) - 1]);
        int metaNameLen = 
            (namelen - (sizeof(S3_METADATA_HEADER_NAME_PREFIX) - 1));
        char *copiedName = 
            string_multibuffer_current(handler->responseMetaDataStrings);
        string_multibuffer_add(handler->responseMetaDataStrings, metaName,
                               metaNameLen, fit);
        if (!fit) {
            return;
        }

        // Copy the value in
        char *copiedValue = 
            string_multibuffer_current(handler->responseMetaDataStrings);
        string_multibuffer_add(handler->responseMetaDataStrings,
                               c, valuelen, fit);
        if (!fit) {
            return;
        }

        if (!handler->responseProperties.metaDataCount) {
            handler->responseProperties.metaData = 
                handler->responseMetaData;
        }

        S3NameValue *metaHeader = 
            &(handler->responseMetaData
              [handler->responseProperties.metaDataCount++]);
        metaHeader->name = copiedName;
        metaHeader->value = copiedValue;
    }
    else if (!strncasecmp(header, "x-amz-server-side-encryption", namelen)) {
        if (!strncmp(c, "AES256", sizeof("AES256") - 1)) {
            responseProperties->usesServerSideEncryption = 1;
        }
        // Ignore other values - only AES256 is expected, anything else is
        // assumed to be "None" or some other value indicating no server-side
        // encryption
    }
}


void response_headers_handler_done(ResponseHeadersHandler *handler, ne_request *NeonRequest) // WINSCP (neon API)
{
    const char * value = ne_get_response_header(NeonRequest, "Last-Modified");
    if (value != NULL) {
        handler->responseProperties.lastModified = ne_httpdate_parse(value);
    }
    
    handler->done = 1;
}
