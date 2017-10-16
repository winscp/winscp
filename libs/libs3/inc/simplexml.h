/** **************************************************************************
 * simplexml.h
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

#ifndef SIMPLEXML_H
#define SIMPLEXML_H

#include "libs3.h"


// Simple XML callback.
//
// elementPath: is the full "path" of the element; i.e.
// <foo><bar><baz>data</baz></bar></foo> would have 'data' in the element
// foo/bar/baz.
// 
// Return of anything other than S3StatusOK causes the calling
// simplexml_add() function to immediately stop and return the status.
//
// data is passed in as 0 on end of element
typedef S3Status (SimpleXmlCallback)(const char *elementPath, const char *data,
                                     int dataLen, void *callbackData);

typedef struct SimpleXml
{
    void *xmlParser;

    SimpleXmlCallback *callback;

    void *callbackData;

    char elementPath[512];

    int elementPathLen;

    S3Status status;
} SimpleXml;


// Simple XML parsing
// ----------------------------------------------------------------------------

// Always call this, even if the simplexml doesn't end up being used
void simplexml_initialize(SimpleXml *simpleXml, SimpleXmlCallback *callback,
                          void *callbackData);

S3Status simplexml_add(SimpleXml *simpleXml, const char *data, int dataLen);


// Always call this
void simplexml_deinitialize(SimpleXml *simpleXml);


#endif /* SIMPLEXML_H */
