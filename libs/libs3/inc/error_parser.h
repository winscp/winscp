/** **************************************************************************
 * error_parser.h
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
 * <http://www.gnu.org/licenses/>.
 *
 * You should also have received a copy of the GNU General Public License
 * version 2 along with libs3, in a file named COPYING-GPLv2.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 ************************************************************************** **/

#ifndef ERROR_PARSER_H
#define ERROR_PARSER_H

#include "libs3.h"
#include "simplexml.h"
#include "string_buffer.h"


#define EXTRA_DETAILS_SIZE 8

typedef struct ErrorParser
{
    // This is the S3ErrorDetails that this ErrorParser fills in from the
    // data that it parses
    S3ErrorDetails s3ErrorDetails;

    // This is the error XML parser
    SimpleXml errorXmlParser;

    // Set to 1 after the first call to add
    int errorXmlParserInitialized;

    // Used to buffer the S3 Error Code as it is read in
    string_buffer(code, 1024);

    // Used to buffer the S3 Error Message as it is read in
    string_buffer(message, 1024);

    // Used to buffer the S3 Error Resource as it is read in
    string_buffer(resource, 1024);

    // Used to buffer the S3 Error Further Details as it is read in
    string_buffer(furtherDetails, 1024);
    
    // The extra details; we support up to EXTRA_DETAILS_SIZE of them
    S3NameValue extraDetails[EXTRA_DETAILS_SIZE];

    // This is the buffer from which the names and values used in extraDetails
    // are allocated
    string_multibuffer(extraDetailsNamesValues, EXTRA_DETAILS_SIZE * 1024);
} ErrorParser;


// Always call this
void error_parser_initialize(ErrorParser *errorParser);

S3Status error_parser_add(ErrorParser *errorParser, char *buffer,
                          int bufferSize);

void error_parser_convert_status(ErrorParser *errorParser, S3Status *status);

// Always call this
void error_parser_deinitialize(ErrorParser *errorParser);


#endif /* ERROR_PARSER_H */
