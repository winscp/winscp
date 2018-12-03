/** **************************************************************************
 * multipart.c
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


typedef struct InitialMultipartData
{
    SimpleXml simpleXml;
    int len;
    S3MultipartInitialHandler *handler;
    string_buffer(upload_id, 256);
    void *userdata;
} InitialMultipartData;

static S3Status InitialMultipartCallback(int bufferSize, const char *buffer,
                                         void *callbackData)
{
    InitialMultipartData *mdata = (InitialMultipartData *) callbackData;
    return simplexml_add(&(mdata->simpleXml), buffer, bufferSize);
}

static void InitialMultipartCompleteCallback
    (S3Status requestStatus, const S3ErrorDetails *s3ErrorDetails,
     void *callbackData)
{
    InitialMultipartData *mdata = (InitialMultipartData *) callbackData;

    if (mdata->handler->responseHandler.completeCallback) {
        (*mdata->handler->responseHandler.completeCallback)
            (requestStatus, s3ErrorDetails, mdata->userdata);
    }

    if (mdata->handler->responseXmlCallback) {
        (*mdata->handler->responseXmlCallback)
            (mdata->upload_id, mdata->userdata);
    }

    simplexml_deinitialize(&(mdata->simpleXml));
    free(mdata);
}

static void AbortMultipartUploadCompleteCallback
    (S3Status requestStatus, const S3ErrorDetails *s3ErrorDetails,
     void *callbackData)
{
    (void) callbackData;
    (void) s3ErrorDetails;
    fprintf(stderr, "\nERROR: %s\n", S3_get_status_name(requestStatus));

}

static S3Status initialMultipartXmlCallback(const char *elementPath,
                                            const char *data,
                                            int dataLen,
                                            void *callbackData)
{
    InitialMultipartData *mdata = (InitialMultipartData *) callbackData;
    int fit;
    if (data) {
        if (!strcmp(elementPath, "InitiateMultipartUploadResult/UploadId")) {
            string_buffer_append(mdata->upload_id,data, dataLen, fit);
        }
    }

    (void) fit;
    return S3StatusOK;
}

void S3_initiate_multipart(S3BucketContext *bucketContext, const char *key,
                          S3PutProperties *putProperties,
                          S3MultipartInitialHandler *handler,
                          S3RequestContext *requestContext,
                          int timeoutMs,
                          void *callbackData)
{
    InitialMultipartData *mdata =
        (InitialMultipartData *) malloc(sizeof(InitialMultipartData));
    simplexml_initialize(&(mdata->simpleXml), &initialMultipartXmlCallback,
                         mdata);
    string_buffer_initialize(mdata->upload_id);
    mdata->handler= handler;
    mdata->userdata = callbackData;

    RequestParams params =
    {
        HttpRequestTypePOST,                          // httpRequestType
        { bucketContext->hostName,                    // hostName
          bucketContext->bucketName,                  // bucketName
          bucketContext->protocol,                    // protocol
          bucketContext->uriStyle,                    // uriStyle
          bucketContext->accessKeyId,                 // accessKeyId
          bucketContext->secretAccessKey,             // secretAccessKey
          bucketContext->securityToken,               // securityToken
          bucketContext->authRegion },                // authRegion
        key,                                          // key
        0,                                            // queryParams
        "uploads",                                    // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        putProperties,                                // putProperties
        handler->responseHandler.propertiesCallback,  // propertiesCallback
        0,                                            // toS3Callback
        0,                                            // toS3CallbackTotalSize
        InitialMultipartCallback,                     // fromS3Callback
        InitialMultipartCompleteCallback,             // completeCallback
        mdata,                                        // callbackData
        timeoutMs                                     // timeoutMs
    };

    // Perform the request
    request_perform(&params, requestContext);
}


void S3_abort_multipart_upload(S3BucketContext *bucketContext, const char *key,
                               const char *uploadId,
                               int timeoutMs,
                               S3AbortMultipartUploadHandler *handler)
{
    char subResource[512];
    snprintf(subResource, 512, "uploadId=%s", uploadId);

    RequestParams params =
    {
        HttpRequestTypeDELETE,                        // httpRequestType
        { bucketContext->hostName,                    // hostName
     	  bucketContext->bucketName,                  // bucketName
          bucketContext->protocol,                    // protocol
          bucketContext->uriStyle,                    // uriStyle
          bucketContext->accessKeyId,                 // accessKeyId
          bucketContext->secretAccessKey,             // secretAccessKey
          bucketContext->securityToken,               // securityToken
          bucketContext->authRegion },                // authRegion
        key,                                          // key
        0,                                            // queryParams
        subResource,                                  // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        0,                                            // putProperties
        handler->responseHandler.propertiesCallback,  // propertiesCallback
        0,                                            // toS3Callback
        0,                                            // toS3CallbackTotalSize
        0,                                            // fromS3Callback
        AbortMultipartUploadCompleteCallback,         // completeCallback
        0,                                            // callbackData
        timeoutMs                                     // timeoutMs
    };

    // Perform the request
    request_perform(&params, 0);
}


/*
 * S3 Upload Part
 */

void S3_upload_part(S3BucketContext *bucketContext, const char *key,
                    S3PutProperties *putProperties,
                    S3PutObjectHandler *handler, int seq,
                    const char *upload_id, int partContentLength,
                    S3RequestContext *requestContext,
                    int timeoutMs,
                    void *callbackData)
{
    char queryParams[512];
    snprintf(queryParams, 512, "partNumber=%d&uploadId=%s", seq, upload_id);

    RequestParams params =
    {
        HttpRequestTypePUT,                           // httpRequestType
        { bucketContext->hostName,                    // hostName
          bucketContext->bucketName,                  // bucketName
          bucketContext->protocol,                    // protocol
          bucketContext->uriStyle,                    // uriStyle
          bucketContext->accessKeyId,                 // accessKeyId
          bucketContext->secretAccessKey,             // secretAccessKey
          bucketContext->securityToken,               // securityToken
          bucketContext->authRegion },                // authRegion
        key,                                          // key
        queryParams,                                  // queryParams
        0,                                            // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        putProperties,                                // putProperties
        handler->responseHandler.propertiesCallback,  // propertiesCallback
        handler->putObjectDataCallback,               // toS3Callback
        partContentLength,                            // toS3CallbackTotalSize
        0,                                            // fromS3Callback
        handler->responseHandler.completeCallback,    // completeCallback
        callbackData,                                 // callbackData
        timeoutMs                                     // timeoutMs
    };

    request_perform(&params, requestContext);
}


/*
 * S3 commit multipart
 *
 */

typedef struct CommitMultiPartData {
    SimpleXml simplexml;
    void *userdata;
    S3MultipartCommitHandler *handler;
    //response parsed from
    string_buffer(location,128);
    string_buffer(etag,128);
} CommitMultiPartData;


static S3Status commitMultipartResponseXMLcallback(const char *elementPath,
                                                   const char *data,
                                                   int dataLen,
                                                   void *callbackData)
{
    int fit;
    CommitMultiPartData *commit_data = (CommitMultiPartData *) callbackData;
    if (data) {
        if (!strcmp(elementPath, "CompleteMultipartUploadResult/Location")) {
            string_buffer_append(commit_data->location, data, dataLen, fit);
        }
        else if (!strcmp(elementPath, "CompleteMultipartUploadResult/ETag")) {
            string_buffer_append(commit_data->etag, data, dataLen, fit);
        }
    }
    (void) fit;

    return S3StatusOK;
}


static S3Status commitMultipartCallback(int bufferSize, const char *buffer,
                                        void *callbackData)
{
    CommitMultiPartData *data = (CommitMultiPartData *) callbackData;
    return simplexml_add(&(data->simplexml), buffer, bufferSize);
}


static S3Status commitMultipartPropertiesCallback
    (const S3ResponseProperties *responseProperties, void *callbackData)
{
    CommitMultiPartData *data = (CommitMultiPartData *) callbackData;

    if (data->handler->responseHandler.propertiesCallback) {
        (*(data->handler->responseHandler.propertiesCallback))
            (responseProperties, data->userdata);
    }
    return S3StatusOK;
}

static void commitMultipartCompleteCallback
    (S3Status requestStatus, const S3ErrorDetails *s3ErrorDetails,
     void *callbackData)
{
    CommitMultiPartData *data = (CommitMultiPartData*) callbackData;
    if (data->handler->responseHandler.completeCallback) {
        (*(data->handler->responseHandler.completeCallback))
            (requestStatus, s3ErrorDetails, data->userdata);
    }
    if (data->handler->responseXmlCallback) {
        (*data->handler->responseXmlCallback)(data->location, data->etag,
                                              data->userdata);
    }
    simplexml_deinitialize(&(data->simplexml));
    free(data);
}


static int commitMultipartPutObject(int bufferSize, char *buffer,
                                    void *callbackData)
{
    CommitMultiPartData *data = (CommitMultiPartData*) callbackData;
    if (data->handler->putObjectDataCallback) {
        return data->handler->putObjectDataCallback(bufferSize, buffer,
                                                    data->userdata);
    }
    else {
        return -1;
    }
}

void S3_complete_multipart_upload(S3BucketContext *bucketContext,
                                  const char *key,
                                  S3MultipartCommitHandler *handler,
                                  const char *upload_id, int contentLength,
                                  S3RequestContext *requestContext,
                                  int timeoutMs,
                                  void *callbackData)
{
    char queryParams[512];
    snprintf(queryParams, 512, "uploadId=%s", upload_id);
    CommitMultiPartData *data =
        (CommitMultiPartData *) malloc(sizeof(CommitMultiPartData));
    data->userdata = callbackData;
    data->handler = handler;
    string_buffer_initialize(data->location);
    string_buffer_initialize(data->etag);

    simplexml_initialize(&(data->simplexml),
                         commitMultipartResponseXMLcallback, data);

    RequestParams params =
    {
        HttpRequestTypePOST,                          // httpRequestType
        { bucketContext->hostName,                    // hostName
          bucketContext->bucketName,                  // bucketName
          bucketContext->protocol,                    // protocol
          bucketContext->uriStyle,                    // uriStyle
          bucketContext->accessKeyId,                 // accessKeyId
          bucketContext->secretAccessKey,             // secretAccessKey
          bucketContext->securityToken,               // securityToken
          bucketContext->authRegion },                // authRegion
        key,                                          // key
        queryParams,                                  // queryParams
        0,                                            // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        0,                                            // putProperties
        commitMultipartPropertiesCallback,            // propertiesCallback
        commitMultipartPutObject,                     // toS3Callback
        contentLength,                                // toS3CallbackTotalSize
        commitMultipartCallback,                      // fromS3Callback
        commitMultipartCompleteCallback,              // completeCallback
        data,                                         // callbackData
        timeoutMs                                     // timeoutMs
    };

    request_perform(&params, requestContext);
}

// We read up to 32 Uploads at a time
#define MAX_UPLOADS 32
// We read up to 8 CommonPrefixes at a time
#define MAX_COMMON_PREFIXES 8
#define MAX_PARTS 32


typedef struct ListMultipartUpload
{
    string_buffer(key, 1024);
    string_buffer(uploadId, 256);
    string_buffer(initiatorId, 256);
    string_buffer(initiatorDisplayName, 256);
    string_buffer(ownerId, 256);
    string_buffer(ownerDisplayName, 256);
    string_buffer(storageClass, 256);
    string_buffer(initiated, 256);
} ListMultipartUpload;


typedef struct ListPart
{
    string_buffer(eTag, 1024);
    string_buffer(partNumber, 24);
    string_buffer(size, 256);
    string_buffer(lastModified, 256);
} ListPart;


typedef struct ListMultipartData
{
    SimpleXml simpleXml;

    S3ResponsePropertiesCallback *responsePropertiesCallback;
    S3ListMultipartUploadsResponseCallback *listMultipartCallback;
    S3ResponseCompleteCallback *responseCompleteCallback;
    void *callbackData;

    string_buffer(isTruncated, 64);
    string_buffer(nextKeyMarker, 1024);
    string_buffer(nextUploadIdMarker, 1024);

    int uploadsCount;
    ListMultipartUpload uploads[MAX_UPLOADS];

    int commonPrefixesCount;
    char commonPrefixes[MAX_COMMON_PREFIXES][1024];
    int commonPrefixLens[MAX_COMMON_PREFIXES];
} ListMultipartData;


typedef struct ListPartsData
{
    SimpleXml simpleXml;

    S3ResponsePropertiesCallback *responsePropertiesCallback;
    S3ListPartsResponseCallback *listPartsCallback;
    S3ResponseCompleteCallback *responseCompleteCallback;
    void *callbackData;

    string_buffer(isTruncated, 64);
    string_buffer(nextPartNumberMarker, 1024);
    string_buffer(initiatorId, 256);
    string_buffer(initiatorDisplayName, 256);
    string_buffer(ownerId, 256);
    string_buffer(ownerDisplayName, 256);
    string_buffer(storageClass, 256);

    int handlePartsStart;
    int partsCount;
    ListPart parts[MAX_PARTS];

} ListPartsData;


static void initialize_list_multipart_upload(ListMultipartUpload *upload)
{
    string_buffer_initialize(upload->key);
    string_buffer_initialize(upload->uploadId);
    string_buffer_initialize(upload->initiatorId);
    string_buffer_initialize(upload->initiatorDisplayName);
    string_buffer_initialize(upload->ownerId);
    string_buffer_initialize(upload->ownerDisplayName);
    string_buffer_initialize(upload->storageClass);
    string_buffer_initialize(upload->initiated);
}


static void initialize_list_part(ListPart *part)
{
    string_buffer_initialize(part->eTag);
    string_buffer_initialize(part->partNumber);
    string_buffer_initialize(part->size);
    string_buffer_initialize(part->lastModified);
}

static void initialize_list_multipart_data(ListMultipartData *lmData)
{
    lmData->uploadsCount = 0;
    initialize_list_multipart_upload(lmData->uploads);
    lmData->commonPrefixesCount = 0;
    lmData->commonPrefixes[0][0] = 0;
    lmData->commonPrefixLens[0] = 0;
}

static void initialize_list_parts_data(ListPartsData *lpData)
{
    lpData->partsCount = 0;
    initialize_list_part(lpData->parts);
}


static S3Status listMultipartPropertiesCallback
    (const S3ResponseProperties *responseProperties, void *callbackData)
{
    ListMultipartData *lmData = (ListMultipartData *) callbackData;

    return (*(lmData->responsePropertiesCallback))
        (responseProperties, lmData->callbackData);
}


static S3Status listPartsPropertiesCallback
    (const S3ResponseProperties *responseProperties, void *callbackData)
{
    ListPartsData *lpData = (ListPartsData *) callbackData;

    return (*(lpData->responsePropertiesCallback))
        (responseProperties, lpData->callbackData);
}


static S3Status listMultipartDataCallback(int bufferSize, const char *buffer,
                                       void *callbackData)
{
    ListMultipartData *lmData = (ListMultipartData *) callbackData;

    return simplexml_add(&(lmData->simpleXml), buffer, bufferSize);
}


static S3Status listPartsDataCallback(int bufferSize, const char *buffer,
                                       void *callbackData)
{
    ListPartsData *lpData = (ListPartsData *) callbackData;

    return simplexml_add(&(lpData->simpleXml), buffer, bufferSize);
}


static S3Status make_list_multipart_callback(ListMultipartData *lmData)
{
    int i;

    // Convert IsTruncated
    int isTruncated = (!strcmp(lmData->isTruncated, "true") ||
                       !strcmp(lmData->isTruncated, "1")) ? 1 : 0;

    // Convert the contents
    S3ListMultipartUpload uploads[lmData->uploadsCount];

    int uploadsCount = lmData->uploadsCount;
    for (i = 0; i < uploadsCount; i++) {
        S3ListMultipartUpload *uploadDest = &(uploads[i]);
        ListMultipartUpload *uploadSrc = &(lmData->uploads[i]);
        uploadDest->key = uploadSrc->key;
        uploadDest->uploadId = uploadSrc->uploadId;
        uploadDest->initiatorId = uploadSrc->initiatorId;
        uploadDest->initiatorDisplayName = uploadSrc->initiatorDisplayName;
        uploadDest->ownerId =
            uploadSrc->ownerId[0] ?uploadSrc->ownerId : 0;
        uploadDest->ownerDisplayName = (uploadSrc->ownerDisplayName[0] ?
                                        uploadSrc->ownerDisplayName : 0);
        uploadDest->storageClass = uploadSrc->storageClass;
        uploadDest->initiated = parseIso8601Time(uploadSrc->initiated);
    }

    // Make the common prefixes array
    int commonPrefixesCount = lmData->commonPrefixesCount;
    char *commonPrefixes[commonPrefixesCount];
    for (i = 0; i < commonPrefixesCount; i++) {
        commonPrefixes[i] = lmData->commonPrefixes[i];
    }

    return (*(lmData->listMultipartCallback))
        (isTruncated, lmData->nextKeyMarker, lmData->nextUploadIdMarker,
         uploadsCount, uploads, commonPrefixesCount,
         (const char **) commonPrefixes, lmData->callbackData);
}


static S3Status make_list_parts_callback(ListPartsData *lpData)
{
    int i;

    // Convert IsTruncated
    int isTruncated = (!strcmp(lpData->isTruncated, "true") ||
                       !strcmp(lpData->isTruncated, "1")) ? 1 : 0;

    // Convert the contents
    S3ListPart Parts[lpData->partsCount];
    int partsCount = lpData->partsCount;
    for (i = 0; i < partsCount; i++) {
        S3ListPart *partDest = &(Parts[i]);
        ListPart *partSrc = &(lpData->parts[i]);
        partDest->eTag = partSrc->eTag;
        partDest->partNumber = parseUnsignedInt(partSrc->partNumber);
        partDest->size = parseUnsignedInt(partSrc->size);
        partDest->lastModified = parseIso8601Time(partSrc->lastModified);
    }

    return (*(lpData->listPartsCallback))
        (isTruncated, lpData->nextPartNumberMarker, lpData->initiatorId,
         lpData->initiatorDisplayName, lpData->ownerId,
         lpData->ownerDisplayName, lpData->storageClass, partsCount,
         lpData->handlePartsStart, Parts, lpData->callbackData);
}


static void listMultipartCompleteCallback(S3Status requestStatus,
                                          const S3ErrorDetails *s3ErrorDetails,
                                          void *callbackData)
{
    ListMultipartData *lmData = (ListMultipartData *) callbackData;

    // Make the callback if there is anything
    if (lmData->uploadsCount || lmData->commonPrefixesCount) {
        make_list_multipart_callback(lmData);
    }

    (*(lmData->responseCompleteCallback))
        (requestStatus, s3ErrorDetails, lmData->callbackData);

    simplexml_deinitialize(&(lmData->simpleXml));

    free(lmData);
}


static void listPartsCompleteCallback(S3Status requestStatus,
                                      const S3ErrorDetails *s3ErrorDetails,
                                      void *callbackData)
{
    ListPartsData *lpData = (ListPartsData *) callbackData;

    // Make the callback if there is anything
    if (lpData->partsCount) {
        make_list_parts_callback(lpData);
    }

    (*(lpData->responseCompleteCallback))
        (requestStatus, s3ErrorDetails, lpData->callbackData);

    simplexml_deinitialize(&(lpData->simpleXml));

    free(lpData);
}


static S3Status listMultipartXmlCallback(const char *elementPath,
                                         const char *data, int dataLen,
                                         void *callbackData)
{
    ListMultipartData *lmData = (ListMultipartData *) callbackData;

    int fit;

    if (data) {
        if (!strcmp(elementPath, "ListMultipartUploadsResult/IsTruncated")) {
            string_buffer_append(lmData->isTruncated, data, dataLen, fit);
        }
        else if (!strcmp(elementPath,
                         "ListMultipartUploadsResult/NextKeyMarker")) {
            string_buffer_append(lmData->nextKeyMarker, data, dataLen, fit);
        }
        else if (!strcmp(elementPath,
                         "ListMultipartUploadsResult/NextUploadIdMarker")) {
            string_buffer_append(lmData->nextUploadIdMarker, data, dataLen,
                                 fit);
        }
        else if (!strcmp(elementPath,
                         "ListMultipartUploadsResult/Upload/Key")) {
            ListMultipartUpload *uploads =
                &(lmData->uploads[lmData->uploadsCount]);
            string_buffer_append(uploads->key, data, dataLen, fit);
        }
        else if (!strcmp(elementPath,
                         "ListMultipartUploadsResult/Upload/Initiated")) {
            ListMultipartUpload *uploads =
                &(lmData->uploads[lmData->uploadsCount]);
            string_buffer_append(uploads->initiated, data, dataLen, fit);
        }
        else if (!strcmp(elementPath,
                         "ListMultipartUploadsResult/Upload/UploadId")) {
            ListMultipartUpload *uploads =
                &(lmData->uploads[lmData->uploadsCount]);
            string_buffer_append(uploads->uploadId, data, dataLen, fit);
        }
        else if (!strcmp(elementPath,
                         "ListMultipartUploadsResult/Upload/Initiator/ID")) {
            ListMultipartUpload *uploads =
                &(lmData->uploads[lmData->uploadsCount]);
            string_buffer_append(uploads->initiatorId, data, dataLen, fit);
        }
        else if (!strcmp
                 (elementPath,
                  "ListMultipartUploadsResult/Upload/Initiator/DisplayName")) {
            ListMultipartUpload *uploads =
                &(lmData->uploads[lmData->uploadsCount]);
            string_buffer_append(uploads->initiatorDisplayName, data, dataLen,
                                 fit);
        }
        else if (!strcmp(elementPath,
                         "ListMultipartUploadsResult/Upload/Owner/ID")) {
            ListMultipartUpload *uploads =
                &(lmData->uploads[lmData->uploadsCount]);
            string_buffer_append(uploads->ownerId, data, dataLen, fit);
        }
        else if (!strcmp
                 (elementPath,
                  "ListMultipartUploadsResult/Upload/Owner/DisplayName")) {
            ListMultipartUpload *uploads =
                &(lmData->uploads[lmData->uploadsCount]);
            string_buffer_append
                (uploads->ownerDisplayName, data, dataLen, fit);
        }
        else if (!strcmp(elementPath,
                         "ListMultipartUploadsResult/Upload/StorageClass")) {
            ListMultipartUpload *uploads =
                &(lmData->uploads[lmData->uploadsCount]);
            string_buffer_append(uploads->storageClass, data, dataLen, fit);
        }
        else if (!strcmp(elementPath,
                         "ListMultipartUploadsResult/CommonPrefixes/Prefix")) {
            int which = lmData->commonPrefixesCount;
            lmData->commonPrefixLens[which] +=
                snprintf(lmData->commonPrefixes[which],
                         sizeof(lmData->commonPrefixes[which]) -
                         lmData->commonPrefixLens[which] - 1,
                         "%.*s", dataLen, data);
            if (lmData->commonPrefixLens[which] >=
                (int) sizeof(lmData->commonPrefixes[which])) {
                return S3StatusXmlParseFailure;
            }
        }
    }
    else {
        if (!strcmp(elementPath, "ListMultipartUploadsResult/Upload")) {
            // Finished a Contents
            lmData->uploadsCount++;
            if (lmData->uploadsCount == MAX_UPLOADS) {
                // Make the callback
                S3Status status = make_list_multipart_callback(lmData);
                if (status != S3StatusOK) {
                    return status;
                }
                initialize_list_multipart_data(lmData);
            }
            else {
                // Initialize the next one
                initialize_list_multipart_upload
                    (&(lmData->uploads[lmData->uploadsCount]));
            }
        }
        else if (!strcmp(elementPath,
                         "ListMultipartUploadsResult/CommonPrefixes/Prefix")) {
            // Finished a Prefix
            lmData->commonPrefixesCount++;
            if (lmData->commonPrefixesCount == MAX_COMMON_PREFIXES) {
                // Make the callback
                S3Status status = make_list_multipart_callback(lmData);
                if (status != S3StatusOK) {
                    return status;
                }
                initialize_list_multipart_data(lmData);
            }
            else {
                // Initialize the next one
                lmData->commonPrefixes[lmData->commonPrefixesCount][0] = 0;
                lmData->commonPrefixLens[lmData->commonPrefixesCount] = 0;
            }
        }
    }

    /* Avoid compiler error about variable set but not used */
    (void) fit;

    return S3StatusOK;
}


static S3Status listPartsXmlCallback(const char *elementPath,
                                      const char *data, int dataLen,
                                      void *callbackData)
{
    ListPartsData *lpData = (ListPartsData *) callbackData;
    int fit;
    if (data) {
        if (!strcmp(elementPath, "ListPartsResult/IsTruncated")) {
            string_buffer_append(lpData->isTruncated, data, dataLen, fit);
        }
        else if (!strcmp(elementPath,
                         "ListPartsResult/NextPartNumberMarker")) {
            string_buffer_append(lpData->nextPartNumberMarker, data, dataLen,
                                 fit);
        }
        else if (!strcmp(elementPath, "ListPartsResult/StorageClass")) {
            string_buffer_append(lpData->storageClass, data, dataLen, fit);
        }
        else if (!strcmp(elementPath, "ListPartsResult/Initiator/ID")) {
            string_buffer_append(lpData->initiatorId, data, dataLen, fit);
        }
        else if (!strcmp(elementPath,
                         "ListPartsResult/Initiator/DisplayName")) {
            string_buffer_append(lpData->initiatorDisplayName, data, dataLen,
                                 fit);
        }
        else if (!strcmp(elementPath, "ListPartsResult/Owner/ID")) {
            string_buffer_append(lpData->ownerId, data, dataLen, fit);
        }
        else if (!strcmp(elementPath, "ListPartsResult/Owner/DisplayName")) {
            string_buffer_append(lpData->ownerDisplayName, data, dataLen, fit);
        }
        else if (!strcmp(elementPath, "ListPartsResult/Part/PartNumber")) {
            ListPart *parts = &(lpData->parts[lpData->partsCount]);
            string_buffer_append(parts->partNumber, data, dataLen, fit);
        }
        else if (!strcmp(elementPath, "ListPartsResult/Part/LastModified")) {
            ListPart *parts = &(lpData->parts[lpData->partsCount]);
            string_buffer_append(parts->lastModified, data, dataLen, fit);
        }
        else if (!strcmp(elementPath, "ListPartsResult/Part/ETag")) {
            ListPart *parts = &(lpData->parts[lpData->partsCount]);
            string_buffer_append(parts->eTag, data, dataLen, fit);
        }
        else if (!strcmp(elementPath, "ListPartsResult/Part/Size")) {
            ListPart *parts = &(lpData->parts[lpData->partsCount]);
            string_buffer_append(parts->size, data, dataLen, fit);
        }
    }
    else {
        if (!strcmp(elementPath, "ListPartsResult/Part")) {
            // Finished a Contents
            lpData->partsCount++;
            if (lpData->partsCount == MAX_PARTS) {
                // Make the callback
                S3Status status = make_list_parts_callback(lpData);
                if (status != S3StatusOK) {
                    return status;
                }
                lpData->handlePartsStart += lpData->partsCount;
                initialize_list_parts_data(lpData);
            }
            else {
                // Initialize the next one
                initialize_list_part(&(lpData->parts[lpData->partsCount]));
            }
        }
    }

    /* Avoid compiler error about variable set but not used */
    (void) fit;

    return S3StatusOK;
}


void S3_list_multipart_uploads(S3BucketContext *bucketContext,
                               const char *prefix, const char *keymarker,
                               const char *uploadidmarker,
                               const char *encodingtype, const char *delimiter,
                               int maxuploads, S3RequestContext *requestContext,
                               int timeoutMs,
                               const S3ListMultipartUploadsHandler *handler,
                               void *callbackData)
{
    // Compose the query params
    string_buffer(queryParams, 4096);
    string_buffer_initialize(queryParams);

#define safe_append(name, value)                                            \
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
            if (!urlEncode(encoded, value, 1024, 1)) {                      \
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
        if (keymarker && *keymarker) {
            safe_append("key-marker", keymarker);
        }
        if (delimiter && *delimiter) {
            safe_append("delimiter", delimiter);
        }
        if (uploadidmarker && *uploadidmarker) {
            safe_append("upload-id-marker", uploadidmarker);
        }
        if (encodingtype && *encodingtype) {
            safe_append("encoding-type", encodingtype);
        }
        if (maxuploads) {
            char maxUploadsString[64];
            snprintf(maxUploadsString, sizeof(maxUploadsString), "%d",
                     maxuploads);
            safe_append("max-uploads", maxUploadsString);
        }

        ListMultipartData *lmData =
            (ListMultipartData *) malloc(sizeof(ListMultipartData));

        if (!lmData) {
            (*(handler->responseHandler.completeCallback))
                (S3StatusOutOfMemory, 0, callbackData);
            return;
        }

        simplexml_initialize(&(lmData->simpleXml), &listMultipartXmlCallback,
                             lmData);

        lmData->responsePropertiesCallback =
            handler->responseHandler.propertiesCallback;
        lmData->listMultipartCallback = handler->responseXmlCallback;
        lmData->responseCompleteCallback =
            handler->responseHandler.completeCallback;
        lmData->callbackData = callbackData;

        string_buffer_initialize(lmData->isTruncated);
        string_buffer_initialize(lmData->nextKeyMarker);
        string_buffer_initialize(lmData->nextUploadIdMarker);
        initialize_list_multipart_data(lmData);

        // Set up the RequestParams
        RequestParams params =
        {
            HttpRequestTypeGET,                      // httpRequestType
            { bucketContext->hostName,               // hostName
              bucketContext->bucketName,             // bucketName
              bucketContext->protocol,               // protocol
              bucketContext->uriStyle,               // uriStyle
              bucketContext->accessKeyId,            // accessKeyId
              bucketContext->secretAccessKey,        // secretAccessKey
              bucketContext->securityToken,          // securityToken
              bucketContext->authRegion },           // authRegion
            0,                                       // key
            queryParams[0] ? queryParams : 0,        // queryParams
            "uploads",                               // subResource
            0,                                       // copySourceBucketName
            0,                                       // copySourceKey
            0,                                       // getConditions
            0,                                       // startByte
            0,                                       // byteCount
            0,                                       // putProperties
            &listMultipartPropertiesCallback,        // propertiesCallback
            0,                                       // toS3Callback
            0,                                       // toS3CallbackTotalSize
            &listMultipartDataCallback,              // fromS3Callback
            &listMultipartCompleteCallback,          // completeCallback
            lmData,                                  // callbackData
            timeoutMs                                // timeoutMs
        };

        // Perform the request
        request_perform(&params, requestContext);
}


void S3_list_parts(S3BucketContext *bucketContext, const char *key,
                   const char *partnumbermarker, const char *uploadid,
                   const char *encodingtype, int maxparts,
                   S3RequestContext *requestContext,
                   int timeoutMs,
                   const S3ListPartsHandler *handler, void *callbackData)
{
    // Compose the query params
    string_buffer(queryParams, 4096);
    string_buffer_initialize(queryParams);

#define safe_append(name, value)                                            \
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
            if (!urlEncode(encoded, value, 1024, 1)) {                      \
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

        char subResource[512];
        snprintf(subResource, 512, "uploadId=%s", uploadid);
        int amp = 0;

        if (partnumbermarker && *partnumbermarker) {
            safe_append("part-number-marker", partnumbermarker);
        }
        if (encodingtype && *encodingtype) {
            safe_append("encoding-type", encodingtype);
        }
        if (maxparts) {
            char maxPartsString[64];
            snprintf(maxPartsString, sizeof(maxPartsString), "%d", maxparts);
            safe_append("max-parts", maxPartsString);
        }

        ListPartsData *lpData =
            (ListPartsData *) malloc(sizeof(ListPartsData));

        if (!lpData) {
            (*(handler->responseHandler.completeCallback))
                (S3StatusOutOfMemory, 0, callbackData);
            return;
        }

        simplexml_initialize(&(lpData->simpleXml), &listPartsXmlCallback,
                             lpData);

        lpData->responsePropertiesCallback =
            handler->responseHandler.propertiesCallback;
        lpData->listPartsCallback = handler->responseXmlCallback;
        lpData->responseCompleteCallback =
            handler->responseHandler.completeCallback;
        lpData->callbackData = callbackData;

        string_buffer_initialize(lpData->isTruncated);
        string_buffer_initialize(lpData->nextPartNumberMarker);
        string_buffer_initialize(lpData->initiatorId);
        string_buffer_initialize(lpData->initiatorDisplayName);
        string_buffer_initialize(lpData->ownerId);
        string_buffer_initialize(lpData->ownerDisplayName);
        string_buffer_initialize(lpData->storageClass);
        initialize_list_parts_data(lpData);
        lpData->handlePartsStart = 0;
        // Set up the RequestParams
        RequestParams params =
        {
            HttpRequestTypeGET,                      // httpRequestType
            { bucketContext->hostName,               // hostName
              bucketContext->bucketName,             // bucketName
              bucketContext->protocol,               // protocol
              bucketContext->uriStyle,               // uriStyle
              bucketContext->accessKeyId,            // accessKeyId
              bucketContext->secretAccessKey,        // secretAccessKey
              bucketContext->securityToken,          // securityToken
              bucketContext->authRegion },           // authRegion
            key,                                     // key
            queryParams[0] ? queryParams : 0,        // queryParams
            subResource,                             // subResource
            0,                                       // copySourceBucketName
            0,                                       // copySourceKey
            0,                                       // getConditions
            0,                                       // startByte
            0,                                       // byteCount
            0,                                       // putProperties
            &listPartsPropertiesCallback,            // propertiesCallback
            0,                                       // toS3Callback
            0,                                       // toS3CallbackTotalSize
            &listPartsDataCallback,                  // fromS3Callback
            &listPartsCompleteCallback,              // completeCallback
            lpData,                                  // callbackData
            timeoutMs                                // timeoutMs
        };

        // Perform the request
        request_perform(&params, requestContext);
}
