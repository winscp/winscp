/** **************************************************************************
 * request_context.h
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

#ifndef REQUEST_CONTEXT_H
#define REQUEST_CONTEXT_H

#include "libs3.h"


typedef enum
{
    S3CurlModeMultiPerform                                  ,
    S3CurlModeMultiSocket                                   ,
} S3CurlMode;


struct S3RequestContext
{
    CURLM *curlm;
    S3CurlMode curl_mode;
    
    int verifyPeerSet;
    long verifyPeer;

    struct Request *requests;

    S3SetupCurlCallback setupCurlCallback;
    void *setupCurlCallbackData;
};


#endif /* REQUEST_CONTEXT_H */
