/** **************************************************************************
 * bucket.c
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

#include <string.h>
#include <stdlib.h>
#include "libs3.h"
#include "request.h"
#include "simplexml.h"

// test bucket ---------------------------------------------------------------

typedef struct TestBucketData
{
    SimpleXml simpleXml;

    S3ResponsePropertiesCallback *responsePropertiesCallback;
    S3ResponseCompleteCallback *responseCompleteCallback;
    void *callbackData;

    int locationConstraintReturnSize;
    char *locationConstraintReturn;

    string_buffer(locationConstraint, 256);
} TestBucketData;


static S3Status testBucketXmlCallback(const char *elementPath,
                                      const char *data, int dataLen,
                                      void *callbackData)
{
    TestBucketData *tbData = (TestBucketData *) callbackData;

    int fit;

    if (data && !strcmp(elementPath, "LocationConstraint")) {
        string_buffer_append(tbData->locationConstraint, data, dataLen, fit);
    }

    /* Avoid compiler error about variable set but not used */
    (void) fit;

    return S3StatusOK;
}


static S3Status testBucketPropertiesCallback
    (const S3ResponseProperties *responseProperties, void *callbackData)
{
    TestBucketData *tbData = (TestBucketData *) callbackData;

    return (*(tbData->responsePropertiesCallback))
        (responseProperties, tbData->callbackData);
}


static S3Status testBucketDataCallback(int bufferSize, const char *buffer,
                                       void *callbackData)
{
    TestBucketData *tbData = (TestBucketData *) callbackData;

    return simplexml_add(&(tbData->simpleXml), buffer, bufferSize);
}


static void testBucketCompleteCallback(S3Status requestStatus,
                                       const S3ErrorDetails *s3ErrorDetails,
                                       void *callbackData)
{
    TestBucketData *tbData = (TestBucketData *) callbackData;

    // Copy the location constraint into the return buffer
    snprintf(tbData->locationConstraintReturn,
             tbData->locationConstraintReturnSize, "%s",
             tbData->locationConstraint);

    (*(tbData->responseCompleteCallback))
        (requestStatus, s3ErrorDetails, tbData->callbackData);

    simplexml_deinitialize(&(tbData->simpleXml));

    free(tbData);
}

void S3_test_bucket(S3Protocol protocol, S3UriStyle uriStyle,
                    const char *accessKeyId, const char *secretAccessKey,
                    const char *securityToken, const char *hostName,
                    const char *bucketName, const char *authRegion,
                    int locationConstraintReturnSize,
                    char *locationConstraintReturn,
                    S3RequestContext *requestContext,
                    int timeoutMs,
                    const S3ResponseHandler *handler, void *callbackData)
{
    // Create the callback data
    TestBucketData *tbData =
        (TestBucketData *) malloc(sizeof(TestBucketData));
    if (!tbData) {
        (*(handler->completeCallback))(S3StatusOutOfMemory, 0, callbackData);
        return;
    }

    simplexml_initialize(&(tbData->simpleXml), &testBucketXmlCallback, tbData);

    tbData->responsePropertiesCallback = handler->propertiesCallback;
    tbData->responseCompleteCallback = handler->completeCallback;
    tbData->callbackData = callbackData;

    tbData->locationConstraintReturnSize = locationConstraintReturnSize;
    tbData->locationConstraintReturn = locationConstraintReturn;
    string_buffer_initialize(tbData->locationConstraint);

    // Set up the RequestParams
    RequestParams params =
    {
        HttpRequestTypeGET,                           // httpRequestType
        { hostName,                                   // hostName
          bucketName,                                 // bucketName
          protocol,                                   // protocol
          uriStyle,                                   // uriStyle
          accessKeyId,                                // accessKeyId
          secretAccessKey,                            // secretAccessKey
          securityToken,                              // securityToken
          authRegion },                               // authRegion
        0,                                            // key
        0,                                            // queryParams
        "location",                                   // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        0,                                            // putProperties
        &testBucketPropertiesCallback,                // propertiesCallback
        0,                                            // toS3Callback
        0,                                            // toS3CallbackTotalSize
        &testBucketDataCallback,                      // fromS3Callback
        &testBucketCompleteCallback,                  // completeCallback
        tbData,                                       // callbackData
        timeoutMs                                     // timeoutMs
    };

    // Perform the request
    request_perform(&params, requestContext);
}


// create bucket -------------------------------------------------------------

typedef struct CreateBucketData
{
    S3ResponsePropertiesCallback *responsePropertiesCallback;
    S3ResponseCompleteCallback *responseCompleteCallback;
    void *callbackData;

    char doc[1024];
    int docLen, docBytesWritten;
} CreateBucketData;


static S3Status createBucketPropertiesCallback
    (const S3ResponseProperties *responseProperties, void *callbackData)
{
    CreateBucketData *cbData = (CreateBucketData *) callbackData;

    return (*(cbData->responsePropertiesCallback))
        (responseProperties, cbData->callbackData);
}


static int createBucketDataCallback(int bufferSize, char *buffer,
                                    void *callbackData)
{
    CreateBucketData *cbData = (CreateBucketData *) callbackData;

    if (!cbData->docLen) {
        return 0;
    }

    int remaining = (cbData->docLen - cbData->docBytesWritten);

    int toCopy = bufferSize > remaining ? remaining : bufferSize;

    if (!toCopy) {
        return 0;
    }

    memcpy(buffer, &(cbData->doc[cbData->docBytesWritten]), toCopy);

    cbData->docBytesWritten += toCopy;

    return toCopy;
}


static void createBucketCompleteCallback(S3Status requestStatus,
                                         const S3ErrorDetails *s3ErrorDetails,
                                         void *callbackData)
{
    CreateBucketData *cbData = (CreateBucketData *) callbackData;

    (*(cbData->responseCompleteCallback))
        (requestStatus, s3ErrorDetails, cbData->callbackData);

    free(cbData);
}

static S3Status createBucketFromS3Callback(int bufferSize, const char *buffer,
                                           void *callbackData)
{
    // Sometimes S3 sends response body. We sillently ignore it.

    (void)bufferSize;  // avoid unused parameter warning
    (void)buffer;  // avoid unused parameter warning
    (void)callbackData;  // avoid unused parameter warning

    return S3StatusOK;
}

void S3_create_bucket(S3Protocol protocol, const char *accessKeyId,
                      const char *secretAccessKey, const char *securityToken,
                      const char *hostName, const char *bucketName,
                      const char *authRegion, S3CannedAcl cannedAcl,
                      const char *locationConstraint,
                      S3RequestContext *requestContext,
                      int timeoutMs,
                      const S3ResponseHandler *handler, void *callbackData)
{
    // Create the callback data
    CreateBucketData *cbData =
        (CreateBucketData *) malloc(sizeof(CreateBucketData));
    if (!cbData) {
        (*(handler->completeCallback))(S3StatusOutOfMemory, 0, callbackData);
        return;
    }

    cbData->responsePropertiesCallback = handler->propertiesCallback;
    cbData->responseCompleteCallback = handler->completeCallback;
    cbData->callbackData = callbackData;

    if (locationConstraint) {
        cbData->docLen =
            snprintf(cbData->doc, sizeof(cbData->doc),
                     "<CreateBucketConfiguration><LocationConstraint>"
                     "%s</LocationConstraint></CreateBucketConfiguration>",
                     locationConstraint);
        cbData->docBytesWritten = 0;
    }
    else {
        cbData->docLen = 0;
    }

    // Set up S3PutProperties
    S3PutProperties properties =
    {
        0,                                       // contentType
        0,                                       // md5
        0,                                       // cacheControl
        0,                                       // contentDispositionFilename
        0,                                       // contentEncoding
       -1,                                       // expires
        cannedAcl,                               // cannedAcl
        0,                                       // metaDataCount
        0,                                       // metaData
        0                                        // useServerSideEncryption
    };

    // Set up the RequestParams
    RequestParams params =
    {
        HttpRequestTypePUT,                           // httpRequestType
        { hostName,                                   // hostName
          bucketName,                                 // bucketName
          protocol,                                   // protocol
          S3UriStylePath,                             // uriStyle
          accessKeyId,                                // accessKeyId
          secretAccessKey,                            // secretAccessKey
          securityToken,                              // securityToken
          authRegion },                               // authRegion
        0,                                            // key
        0,                                            // queryParams
        0,                                            // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        &properties,                                  // putProperties
        &createBucketPropertiesCallback,              // propertiesCallback
        &createBucketDataCallback,                    // toS3Callback
        cbData->docLen,                               // toS3CallbackTotalSize
        createBucketFromS3Callback,                   // fromS3Callback
        &createBucketCompleteCallback,                // completeCallback
        cbData,                                       // callbackData
        timeoutMs                                     // timeoutMs
    };

    // Perform the request
    request_perform(&params, requestContext);
}


// delete bucket -------------------------------------------------------------

typedef struct DeleteBucketData
{
    S3ResponsePropertiesCallback *responsePropertiesCallback;
    S3ResponseCompleteCallback *responseCompleteCallback;
    void *callbackData;
} DeleteBucketData;


static S3Status deleteBucketPropertiesCallback
    (const S3ResponseProperties *responseProperties, void *callbackData)
{
    DeleteBucketData *dbData = (DeleteBucketData *) callbackData;

    return (*(dbData->responsePropertiesCallback))
        (responseProperties, dbData->callbackData);
}


static void deleteBucketCompleteCallback(S3Status requestStatus,
                                         const S3ErrorDetails *s3ErrorDetails,
                                         void *callbackData)
{
    DeleteBucketData *dbData = (DeleteBucketData *) callbackData;

    (*(dbData->responseCompleteCallback))
        (requestStatus, s3ErrorDetails, dbData->callbackData);

    free(dbData);
}


void S3_delete_bucket(S3Protocol protocol, S3UriStyle uriStyle,
                      const char *accessKeyId, const char *secretAccessKey,
                      const char *securityToken, const char *hostName,
                      const char *bucketName, const char *authRegion,
                      S3RequestContext *requestContext,
                      int timeoutMs,
                      const S3ResponseHandler *handler, void *callbackData)
{
    // Create the callback data
    DeleteBucketData *dbData =
        (DeleteBucketData *) malloc(sizeof(DeleteBucketData));
    if (!dbData) {
        (*(handler->completeCallback))(S3StatusOutOfMemory, 0, callbackData);
        return;
    }

    dbData->responsePropertiesCallback = handler->propertiesCallback;
    dbData->responseCompleteCallback = handler->completeCallback;
    dbData->callbackData = callbackData;

    // Set up the RequestParams
    RequestParams params =
    {
        HttpRequestTypeDELETE,                        // httpRequestType
        { hostName,                                   // hostName
          bucketName,                                 // bucketName
          protocol,                                   // protocol
          uriStyle,                                   // uriStyle
          accessKeyId,                                // accessKeyId
          secretAccessKey,                            // secretAccessKey
          securityToken,                              // securityToken
          authRegion },                               // authRegion
        0,                                            // key
        0,                                            // queryParams
        0,                                            // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        0,                                            // putProperties
        &deleteBucketPropertiesCallback,              // propertiesCallback
        0,                                            // toS3Callback
        0,                                            // toS3CallbackTotalSize
        0,                                            // fromS3Callback
        &deleteBucketCompleteCallback,                // completeCallback
        dbData,                                       // callbackData
        timeoutMs                                     // timeoutMs
    };

    // Perform the request
    request_perform(&params, requestContext);
}


// list bucket ----------------------------------------------------------------

typedef struct ListBucketContents
{
    string_buffer(key, 1024);
    string_buffer(lastModified, 256);
    string_buffer(eTag, 256);
    string_buffer(size, 24);
    string_buffer(ownerId, 256);
    string_buffer(ownerDisplayName, 256);
} ListBucketContents;


static void initialize_list_bucket_contents(ListBucketContents *contents)
{
    string_buffer_initialize(contents->key);
    string_buffer_initialize(contents->lastModified);
    string_buffer_initialize(contents->eTag);
    string_buffer_initialize(contents->size);
    string_buffer_initialize(contents->ownerId);
    string_buffer_initialize(contents->ownerDisplayName);
}

// We read up to 32 Contents at a time
#define MAX_CONTENTS 32
// We read up to 8 CommonPrefixes at a time
#define MAX_COMMON_PREFIXES 8

typedef struct ListBucketData
{
    SimpleXml simpleXml;

    S3ResponsePropertiesCallback *responsePropertiesCallback;
    S3ListBucketCallback *listBucketCallback;
    S3ResponseCompleteCallback *responseCompleteCallback;
    void *callbackData;

    string_buffer(isTruncated, 64);
    string_buffer(nextMarker, 1024);

    int contentsCount;
    ListBucketContents contents[MAX_CONTENTS];

    int commonPrefixesCount;
    char commonPrefixes[MAX_COMMON_PREFIXES][1024];
    int commonPrefixLens[MAX_COMMON_PREFIXES];
} ListBucketData;


static void initialize_list_bucket_data(ListBucketData *lbData)
{
    lbData->contentsCount = 0;
    initialize_list_bucket_contents(lbData->contents);
    lbData->commonPrefixesCount = 0;
    lbData->commonPrefixes[0][0] = 0;
    lbData->commonPrefixLens[0] = 0;
}


static S3Status make_list_bucket_callback(ListBucketData *lbData)
{
    int i;

    // Convert IsTruncated
    int isTruncated = (!strcmp(lbData->isTruncated, "true") ||
                       !strcmp(lbData->isTruncated, "1")) ? 1 : 0;

    // Convert the contents
    S3ListBucketContent contents[lbData->contentsCount];

    int contentsCount = lbData->contentsCount;
    for (i = 0; i < contentsCount; i++) {
        S3ListBucketContent *contentDest = &(contents[i]);
        ListBucketContents *contentSrc = &(lbData->contents[i]);
        contentDest->key = contentSrc->key;
        contentDest->lastModified =
            parseIso8601Time(contentSrc->lastModified);
        contentDest->eTag = contentSrc->eTag;
        contentDest->size = parseUnsignedInt(contentSrc->size);
        contentDest->ownerId =
            contentSrc->ownerId[0] ?contentSrc->ownerId : 0;
        contentDest->ownerDisplayName = (contentSrc->ownerDisplayName[0] ?
                                         contentSrc->ownerDisplayName : 0);
    }

    // Make the common prefixes array
    int commonPrefixesCount = lbData->commonPrefixesCount;
    char *commonPrefixes[commonPrefixesCount];
    for (i = 0; i < commonPrefixesCount; i++) {
        commonPrefixes[i] = lbData->commonPrefixes[i];
    }

    return (*(lbData->listBucketCallback))
        (isTruncated, lbData->nextMarker,
         contentsCount, contents, commonPrefixesCount,
         (const char **) commonPrefixes, lbData->callbackData);
}


static S3Status listBucketXmlCallback(const char *elementPath,
                                      const char *data, int dataLen,
                                      void *callbackData)
{
    ListBucketData *lbData = (ListBucketData *) callbackData;

    int fit;

    if (data) {
        if (!strcmp(elementPath, "ListBucketResult/IsTruncated")) {
            string_buffer_append(lbData->isTruncated, data, dataLen, fit);
        }
        else if (!strcmp(elementPath, "ListBucketResult/NextMarker")) {
            string_buffer_append(lbData->nextMarker, data, dataLen, fit);
        }
        else if (!strcmp(elementPath, "ListBucketResult/Contents/Key")) {
            ListBucketContents *contents =
                &(lbData->contents[lbData->contentsCount]);
            string_buffer_append(contents->key, data, dataLen, fit);
        }
        else if (!strcmp(elementPath,
                         "ListBucketResult/Contents/LastModified")) {
            ListBucketContents *contents =
                &(lbData->contents[lbData->contentsCount]);
            string_buffer_append(contents->lastModified, data, dataLen, fit);
        }
        else if (!strcmp(elementPath, "ListBucketResult/Contents/ETag")) {
            ListBucketContents *contents =
                &(lbData->contents[lbData->contentsCount]);
            string_buffer_append(contents->eTag, data, dataLen, fit);
        }
        else if (!strcmp(elementPath, "ListBucketResult/Contents/Size")) {
            ListBucketContents *contents =
                &(lbData->contents[lbData->contentsCount]);
            string_buffer_append(contents->size, data, dataLen, fit);
        }
        else if (!strcmp(elementPath, "ListBucketResult/Contents/Owner/ID")) {
            ListBucketContents *contents =
                &(lbData->contents[lbData->contentsCount]);
            string_buffer_append(contents->ownerId, data, dataLen, fit);
        }
        else if (!strcmp(elementPath,
                         "ListBucketResult/Contents/Owner/DisplayName")) {
            ListBucketContents *contents =
                &(lbData->contents[lbData->contentsCount]);
            string_buffer_append
                (contents->ownerDisplayName, data, dataLen, fit);
        }
        else if (!strcmp(elementPath,
                         "ListBucketResult/CommonPrefixes/Prefix")) {
            int which = lbData->commonPrefixesCount;
            size_t oldLen = lbData->commonPrefixLens[which];
            lbData->commonPrefixLens[which] +=
                snprintf(lbData->commonPrefixes[which]+oldLen,
                         sizeof(lbData->commonPrefixes[which]) -
                         oldLen - 1,
                         "%.*s", dataLen, data);
            if (lbData->commonPrefixLens[which] >=
                (int) sizeof(lbData->commonPrefixes[which])) {
                return S3StatusXmlParseFailure;
            }
        }
    }
    else {
        if (!strcmp(elementPath, "ListBucketResult/Contents")) {
            // Finished a Contents
            lbData->contentsCount++;
            if (lbData->contentsCount == MAX_CONTENTS) {
                // Make the callback
                S3Status status = make_list_bucket_callback(lbData);
                if (status != S3StatusOK) {
                    return status;
                }
                initialize_list_bucket_data(lbData);
            }
            else {
                // Initialize the next one
                initialize_list_bucket_contents
                    (&(lbData->contents[lbData->contentsCount]));
            }
        }
        else if (!strcmp(elementPath,
                         "ListBucketResult/CommonPrefixes/Prefix")) {
            // Finished a Prefix
            lbData->commonPrefixesCount++;
            if (lbData->commonPrefixesCount == MAX_COMMON_PREFIXES) {
                // Make the callback
                S3Status status = make_list_bucket_callback(lbData);
                if (status != S3StatusOK) {
                    return status;
                }
                initialize_list_bucket_data(lbData);
            }
            else {
                // Initialize the next one
                lbData->commonPrefixes[lbData->commonPrefixesCount][0] = 0;
                lbData->commonPrefixLens[lbData->commonPrefixesCount] = 0;
            }
        }
    }

    /* Avoid compiler error about variable set but not used */
    (void) fit;

    return S3StatusOK;
}


static S3Status listBucketPropertiesCallback
    (const S3ResponseProperties *responseProperties, void *callbackData)
{
    ListBucketData *lbData = (ListBucketData *) callbackData;

    return (*(lbData->responsePropertiesCallback))
        (responseProperties, lbData->callbackData);
}


static S3Status listBucketDataCallback(int bufferSize, const char *buffer,
                                       void *callbackData)
{
    ListBucketData *lbData = (ListBucketData *) callbackData;

    return simplexml_add(&(lbData->simpleXml), buffer, bufferSize);
}


static void listBucketCompleteCallback(S3Status requestStatus,
                                       const S3ErrorDetails *s3ErrorDetails,
                                       void *callbackData)
{
    ListBucketData *lbData = (ListBucketData *) callbackData;

    // Make the callback if there is anything
    if (lbData->contentsCount || lbData->commonPrefixesCount) {
        make_list_bucket_callback(lbData);
    }

    (*(lbData->responseCompleteCallback))
        (requestStatus, s3ErrorDetails, lbData->callbackData);

    simplexml_deinitialize(&(lbData->simpleXml));

    free(lbData);
}


void S3_list_bucket(const S3BucketContext *bucketContext, const char *prefix,
                    const char *marker, const char *delimiter, int maxkeys,
                    S3RequestContext *requestContext,
                    int timeoutMs,
                    const S3ListBucketHandler *handler, void *callbackData)
{
    // Compose the query params
    string_buffer(queryParams, 4096);
    string_buffer_initialize(queryParams);

#define safe_append(name, value)                                        \
    do {                                                                \
        int fit;                                                        \
        if (amp) {                                                      \
            string_buffer_append(queryParams, "&", 1, fit);             \
            if (!fit) {                                                 \
                (*(handler->responseHandler.completeCallback))          \
                    (S3StatusQueryParamsTooLong, 0, callbackData);      \
                return;                                                 \
            }                                                           \
        }                                                               \
        string_buffer_append(queryParams, name "=",                     \
                             sizeof(name "=") - 1, fit);                \
        if (!fit) {                                                     \
            (*(handler->responseHandler.completeCallback))              \
                (S3StatusQueryParamsTooLong, 0, callbackData);          \
            return;                                                     \
        }                                                               \
        amp = 1;                                                        \
        char encoded[3 * 1024];                                         \
        if (!urlEncode(encoded, value, 1024, 1)) {                   \
            (*(handler->responseHandler.completeCallback))              \
                (S3StatusQueryParamsTooLong, 0, callbackData);          \
            return;                                                     \
        }                                                               \
        string_buffer_append(queryParams, encoded, strlen(encoded),     \
                             fit);                                      \
        if (!fit) {                                                     \
            (*(handler->responseHandler.completeCallback))              \
                (S3StatusQueryParamsTooLong, 0, callbackData);          \
            return;                                                     \
        }                                                               \
    } while (0)


    int amp = 0;
    if (prefix && *prefix) {
        safe_append("prefix", prefix);
    }
    if (marker && *marker) {
        safe_append("marker", marker);
    }
    if (delimiter && *delimiter) {
        safe_append("delimiter", delimiter);
    }
    if (maxkeys) {
        char maxKeysString[64];
        snprintf(maxKeysString, sizeof(maxKeysString), "%d", maxkeys);
        safe_append("max-keys", maxKeysString);
    }

    ListBucketData *lbData =
        (ListBucketData *) malloc(sizeof(ListBucketData));

    if (!lbData) {
        (*(handler->responseHandler.completeCallback))
            (S3StatusOutOfMemory, 0, callbackData);
        return;
    }

    simplexml_initialize(&(lbData->simpleXml), &listBucketXmlCallback, lbData);

    lbData->responsePropertiesCallback =
        handler->responseHandler.propertiesCallback;
    lbData->listBucketCallback = handler->listBucketCallback;
    lbData->responseCompleteCallback =
        handler->responseHandler.completeCallback;
    lbData->callbackData = callbackData;

    string_buffer_initialize(lbData->isTruncated);
    string_buffer_initialize(lbData->nextMarker);
    initialize_list_bucket_data(lbData);

    // Set up the RequestParams
    RequestParams params =
    {
        HttpRequestTypeGET,                           // httpRequestType
        { bucketContext->hostName,                    // hostName
          bucketContext->bucketName,                  // bucketName
          bucketContext->protocol,                    // protocol
          bucketContext->uriStyle,                    // uriStyle
          bucketContext->accessKeyId,                 // accessKeyId
          bucketContext->secretAccessKey,             // secretAccessKey
          bucketContext->securityToken,               // securityToken
          bucketContext->authRegion },                // authRegion
        0,                                            // key
        queryParams[0] ? queryParams : 0,             // queryParams
        0,                                            // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        0,                                            // putProperties
        &listBucketPropertiesCallback,                // propertiesCallback
        0,                                            // toS3Callback
        0,                                            // toS3CallbackTotalSize
        &listBucketDataCallback,                      // fromS3Callback
        &listBucketCompleteCallback,                  // completeCallback
        lbData,                                       // callbackData
        timeoutMs                                     // timeoutMs
    };

    // Perform the request
    request_perform(&params, requestContext);
}
