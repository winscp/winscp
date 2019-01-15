/** **************************************************************************
 * bucket_metadata.c
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

#include <stdlib.h>
#include <string.h>

#ifndef __APPLE__
    #include <openssl/md5.h>
    #include <openssl/bio.h>
    #include <openssl/evp.h>
    #include <openssl/buffer.h>
#endif

#include "libs3.h"
#include "request.h"

// Use a rather arbitrary max size for the document of 64K
#define ACL_XML_DOC_MAXSIZE (64 * 1024)


// get acl -------------------------------------------------------------------

typedef struct GetAclData
{
    SimpleXml simpleXml;

    S3ResponsePropertiesCallback *responsePropertiesCallback;
    S3ResponseCompleteCallback *responseCompleteCallback;
    void *callbackData;

    int *aclGrantCountReturn;
    S3AclGrant *aclGrants;
    char *ownerId;
    char *ownerDisplayName;
    string_buffer(xmlDocument, ACL_XML_DOC_MAXSIZE);
} GetAclData;


static S3Status getAclPropertiesCallback
    (const S3ResponseProperties *responseProperties, void *callbackData)
{
    GetAclData *gaData = (GetAclData *) callbackData;

    return (*(gaData->responsePropertiesCallback))
        (responseProperties, gaData->callbackData);
}


static S3Status getAclDataCallback(int bufferSize, const char *buffer,
                                   void *callbackData)
{
    GetAclData *gaData = (GetAclData *) callbackData;

    int fit;

    string_buffer_append(gaData->xmlDocument, buffer, bufferSize, fit);

    return fit ? S3StatusOK : S3StatusXmlDocumentTooLarge;
}


static void getAclCompleteCallback(S3Status requestStatus,
                                   const S3ErrorDetails *s3ErrorDetails,
                                   void *callbackData)
{
    GetAclData *gaData = (GetAclData *) callbackData;

    if (requestStatus == S3StatusOK) {
        // Parse the document
        requestStatus = S3_convert_acl
            (gaData->xmlDocument, gaData->ownerId, gaData->ownerDisplayName,
             gaData->aclGrantCountReturn, gaData->aclGrants);
    }

    (*(gaData->responseCompleteCallback))
        (requestStatus, s3ErrorDetails, gaData->callbackData);

    free(gaData);
}


void S3_get_acl(const S3BucketContext *bucketContext, const char *key,
                char *ownerId, char *ownerDisplayName,
                int *aclGrantCountReturn, S3AclGrant *aclGrants,
                S3RequestContext *requestContext,
                int timeoutMs,
                const S3ResponseHandler *handler, void *callbackData)
{
    // Create the callback data
    GetAclData *gaData = (GetAclData *) malloc(sizeof(GetAclData));
    if (!gaData) {
        (*(handler->completeCallback))(S3StatusOutOfMemory, 0, callbackData);
        return;
    }

    gaData->responsePropertiesCallback = handler->propertiesCallback;
    gaData->responseCompleteCallback = handler->completeCallback;
    gaData->callbackData = callbackData;

    gaData->aclGrantCountReturn = aclGrantCountReturn;
    gaData->aclGrants = aclGrants;
    gaData->ownerId = ownerId;
    gaData->ownerDisplayName = ownerDisplayName;
    string_buffer_initialize(gaData->xmlDocument);
    *aclGrantCountReturn = 0;

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
        key,                                          // key
        0,                                            // queryParams
        "acl",                                        // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        0,                                            // putProperties
        &getAclPropertiesCallback,                    // propertiesCallback
        0,                                            // toS3Callback
        0,                                            // toS3CallbackTotalSize
        &getAclDataCallback,                          // fromS3Callback
        &getAclCompleteCallback,                      // completeCallback
        gaData,                                       // callbackData
        timeoutMs                                     // timeoutMs
    };

    // Perform the request
    request_perform(&params, requestContext);
}


// set acl -------------------------------------------------------------------

static S3Status generateAclXmlDocument(const char *ownerId,
                                       const char *ownerDisplayName,
                                       int aclGrantCount,
                                       const S3AclGrant *aclGrants,
                                       int *xmlDocumentLenReturn,
                                       char *xmlDocument,
                                       int xmlDocumentBufferSize)
{
    *xmlDocumentLenReturn = 0;

#define append(fmt, ...)                                        \
    do {                                                        \
        *xmlDocumentLenReturn += snprintf                       \
            (&(xmlDocument[*xmlDocumentLenReturn]),             \
             xmlDocumentBufferSize - *xmlDocumentLenReturn - 1, \
             fmt, __VA_ARGS__);                                 \
        if (*xmlDocumentLenReturn >= xmlDocumentBufferSize) {   \
            return S3StatusXmlDocumentTooLarge;                 \
        } \
    } while (0)

    append("<AccessControlPolicy><Owner><ID>%s</ID><DisplayName>%s"
           "</DisplayName></Owner><AccessControlList>", ownerId,
           ownerDisplayName);

    int i;
    for (i = 0; i < aclGrantCount; i++) {
        append("%s", "<Grant><Grantee xmlns:xsi=\"http://www.w3.org/2001/"
               "XMLSchema-instance\" xsi:type=\"");
        const S3AclGrant *grant = &(aclGrants[i]);
        switch (grant->granteeType) {
        case S3GranteeTypeAmazonCustomerByEmail:
            append("AmazonCustomerByEmail\"><EmailAddress>%s</EmailAddress>",
                   grant->grantee.amazonCustomerByEmail.emailAddress);
            break;
        case S3GranteeTypeCanonicalUser:
            append("CanonicalUser\"><ID>%s</ID><DisplayName>%s</DisplayName>",
                   grant->grantee.canonicalUser.id,
                   grant->grantee.canonicalUser.displayName);
            break;
        default: { // case S3GranteeTypeAllAwsUsers/S3GranteeTypeAllUsers:
            const char *grantee;
            switch (grant->granteeType) {
            case S3GranteeTypeAllAwsUsers:
                grantee = ACS_GROUP_AWS_USERS;
                break;
            case S3GranteeTypeAllUsers:
                grantee = ACS_GROUP_ALL_USERS;
                break;
            default:
                grantee = ACS_GROUP_LOG_DELIVERY;
                break;
            }
            append("Group\"><URI>%s</URI>", grantee);
        }
            break;
        }
        append("</Grantee><Permission>%s</Permission></Grant>",
               ((grant->permission == S3PermissionRead) ? "READ" :
                (grant->permission == S3PermissionWrite) ? "WRITE" :
                (grant->permission == S3PermissionReadACP) ? "READ_ACP" :
                (grant->permission == S3PermissionWriteACP) ? "WRITE_ACP" :
                "FULL_CONTROL"));
    }

    append("%s", "</AccessControlList></AccessControlPolicy>");

    return S3StatusOK;
}


typedef struct SetXmlData
{
    S3ResponsePropertiesCallback *responsePropertiesCallback;
    S3ResponseCompleteCallback *responseCompleteCallback;
    void *callbackData;

    int xmlDocumentLen;
    const char *xmlDocument;
    int xmlDocumentBytesWritten;

} SetXmlData;


static S3Status setXmlPropertiesCallback
    (const S3ResponseProperties *responseProperties, void *callbackData)
{
    SetXmlData *paData = (SetXmlData *) callbackData;

    return (*(paData->responsePropertiesCallback))
        (responseProperties, paData->callbackData);
}


static int setXmlDataCallback(int bufferSize, char *buffer, void *callbackData)
{
    SetXmlData *paData = (SetXmlData *) callbackData;

    int remaining = (paData->xmlDocumentLen -
                     paData->xmlDocumentBytesWritten);

    int toCopy = bufferSize > remaining ? remaining : bufferSize;

    if (!toCopy) {
        return 0;
    }

    memcpy(buffer, &(paData->xmlDocument
                     [paData->xmlDocumentBytesWritten]), toCopy);

    paData->xmlDocumentBytesWritten += toCopy;

    return toCopy;
}


static void setXmlCompleteCallback(S3Status requestStatus,
                                   const S3ErrorDetails *s3ErrorDetails,
                                   void *callbackData)
{
    SetXmlData *paData = (SetXmlData *) callbackData;

    (*(paData->responseCompleteCallback))
        (requestStatus, s3ErrorDetails, paData->callbackData);

    free(paData);
}


void S3_set_acl(const S3BucketContext *bucketContext, const char *key,
                const char *ownerId, const char *ownerDisplayName,
                int aclGrantCount, const S3AclGrant *aclGrants,
                S3RequestContext *requestContext,
                int timeoutMs,
                const S3ResponseHandler *handler, void *callbackData)
{
    char aclBuffer[ACL_XML_DOC_MAXSIZE];

    if (aclGrantCount > S3_MAX_ACL_GRANT_COUNT) {
        (*(handler->completeCallback))
            (S3StatusTooManyGrants, 0, callbackData);
        return;
    }

    SetXmlData *data = (SetXmlData *) malloc(sizeof(SetXmlData));
    if (!data) {
        (*(handler->completeCallback))(S3StatusOutOfMemory, 0, callbackData);
        return;
    }

    data->xmlDocument = aclBuffer;

    // Convert aclGrants to XML document
    S3Status status = generateAclXmlDocument
        (ownerId, ownerDisplayName, aclGrantCount, aclGrants,
         &(data->xmlDocumentLen), aclBuffer,
         sizeof(aclBuffer));
    if (status != S3StatusOK) {
        free(data);
        (*(handler->completeCallback))(status, 0, callbackData);
        return;
    }

    data->responsePropertiesCallback = handler->propertiesCallback;
    data->responseCompleteCallback = handler->completeCallback;
    data->callbackData = callbackData;

    data->xmlDocumentBytesWritten = 0;

    // Set up the RequestParams
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
        0,                                            // queryParams
        "acl",                                        // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        0,                                            // putProperties
        &setXmlPropertiesCallback,                    // propertiesCallback
        &setXmlDataCallback,                          // toS3Callback
        data->xmlDocumentLen,                         // toS3CallbackTotalSize
        0,                                            // fromS3Callback
        &setXmlCompleteCallback,                      // completeCallback
        data,                                         // callbackData
        timeoutMs                                     // timeoutMs
    };

    // Perform the request
    request_perform(&params, requestContext);
}


// get lifecycle -------------------------------------------------------------------

typedef struct GetLifecycleData
{
    S3ResponsePropertiesCallback *responsePropertiesCallback;
    S3ResponseCompleteCallback *responseCompleteCallback;
    void *callbackData;

    char *lifecycleXmlDocumentReturn;
    int lifecycleXmlDocumentBufferSize;
    int lifecycleXmlDocumentWritten;
} GetLifecycleData;


static S3Status getLifecyclePropertiesCallback
    (const S3ResponseProperties *responseProperties, void *callbackData)
{
    GetLifecycleData *gaData = (GetLifecycleData *) callbackData;

    return (*(gaData->responsePropertiesCallback))
        (responseProperties, gaData->callbackData);
}


static S3Status getLifecycleDataCallback(int bufferSize, const char *buffer,
                                   void *callbackData)
{
    GetLifecycleData *gaData = (GetLifecycleData *) callbackData;

    if ((gaData->lifecycleXmlDocumentWritten + bufferSize) >= gaData->lifecycleXmlDocumentBufferSize)
        return S3StatusXmlDocumentTooLarge;

    snprintf(gaData->lifecycleXmlDocumentReturn + gaData->lifecycleXmlDocumentWritten, bufferSize + 1, "%s", buffer);
    gaData->lifecycleXmlDocumentWritten += bufferSize;

    return S3StatusOK;
}


static void getLifecycleCompleteCallback(S3Status requestStatus,
                                         const S3ErrorDetails *s3ErrorDetails,
                                         void *callbackData)
{
    GetLifecycleData *gaData = (GetLifecycleData *) callbackData;

    (*(gaData->responseCompleteCallback))
        (requestStatus, s3ErrorDetails, gaData->callbackData);

    free(gaData);
}


void S3_get_lifecycle(const S3BucketContext *bucketContext,
                      char *lifecycleXmlDocumentReturn, int lifecycleXmlDocumentBufferSize,
                      S3RequestContext *requestContext,
                      int timeoutMs,
                      const S3ResponseHandler *handler, void *callbackData)
{
    // Create the callback data
    GetLifecycleData *gaData = (GetLifecycleData *) malloc(sizeof(GetLifecycleData));
    if (!gaData) {
        (*(handler->completeCallback))(S3StatusOutOfMemory, 0, callbackData);
        return;
    }

    gaData->responsePropertiesCallback = handler->propertiesCallback;
    gaData->responseCompleteCallback = handler->completeCallback;
    gaData->callbackData = callbackData;

    gaData->lifecycleXmlDocumentReturn = lifecycleXmlDocumentReturn;
    gaData->lifecycleXmlDocumentBufferSize = lifecycleXmlDocumentBufferSize;
    gaData->lifecycleXmlDocumentWritten = 0;

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
        0,                                            // queryParams
        "lifecycle",                                  // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        0,                                            // putProperties
        &getLifecyclePropertiesCallback,              // propertiesCallback
        0,                                            // toS3Callback
        0,                                            // toS3CallbackTotalSize
        &getLifecycleDataCallback,                    // fromS3Callback
        &getLifecycleCompleteCallback,                // completeCallback
        gaData,                                       // callbackData
        timeoutMs                                     // timeoutMs
    };

    // Perform the request
    request_perform(&params, requestContext);
}


#ifndef __APPLE__
// Calculate MD5 and encode it as base64
void generate_content_md5(const char* data, int size,
                          char* retBuffer, int retBufferSize) {
    MD5_CTX mdContext;
    BIO *bio, *b64;
    BUF_MEM *bufferPtr;

    char md5Buffer[MD5_DIGEST_LENGTH];

    MD5_Init(&mdContext);
    MD5_Update(&mdContext, data, size);
    MD5_Final((unsigned char*)md5Buffer, &mdContext);


    b64 = BIO_new(BIO_f_base64());
    bio = BIO_new(BIO_s_mem());
    bio = BIO_push(b64, bio);

    BIO_set_flags(bio, BIO_FLAGS_BASE64_NO_NL); //Ignore newlines - write everything in one line
    BIO_write(bio, md5Buffer, sizeof(md5Buffer));
    (void) BIO_flush(bio);
    BIO_get_mem_ptr(bio, &bufferPtr);
    (void) BIO_set_close(bio, BIO_NOCLOSE);

    if ((unsigned int)retBufferSize + 1 < bufferPtr->length) {
        retBuffer[0] = '\0';
        BIO_free_all(bio);
        return;
    }

    memcpy(retBuffer, bufferPtr->data, bufferPtr->length);
    retBuffer[bufferPtr->length] = '\0';

    BIO_free_all(bio);
}
#endif


void S3_set_lifecycle(const S3BucketContext *bucketContext,
                      const char *lifecycleXmlDocument,
                      S3RequestContext *requestContext,
                      int timeoutMs,
                      const S3ResponseHandler *handler, void *callbackData)
{
#ifdef __APPLE__
    /* This request requires calculating MD5 sum.
     * MD5 sum requires OpenSSL library, which is not used on Apple.
     * TODO Implement some MD5+Base64 caculation on Apple
     */
    (*(handler->completeCallback))(S3StatusNotSupported, 0, callbackData);
    return;
#else
    char md5Base64[MD5_DIGEST_LENGTH * 2];

    SetXmlData *data = (SetXmlData *) malloc(sizeof(SetXmlData));
    if (!data) {
        (*(handler->completeCallback))(S3StatusOutOfMemory, 0, callbackData);
        return;
    }


    data->xmlDocument = lifecycleXmlDocument;
    data->xmlDocumentLen = strlen(lifecycleXmlDocument);

    data->responsePropertiesCallback = handler->propertiesCallback;
    data->responseCompleteCallback = handler->completeCallback;
    data->callbackData = callbackData;

    data->xmlDocumentBytesWritten = 0;

    generate_content_md5(data->xmlDocument, data->xmlDocumentLen,
                         md5Base64, sizeof (md5Base64));

    // Set up S3PutProperties
    S3PutProperties properties =
    {
        0,                                       // contentType
        md5Base64,                               // md5
        0,                                       // cacheControl
        0,                                       // contentDispositionFilename
        0,                                       // contentEncoding
       -1,                                       // expires
        0,                                       // cannedAcl
        0,                                       // metaDataCount
        0,                                       // metaData
        0                                        // useServerSideEncryption
    };

    // Set up the RequestParams
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
        0,                                            // key
        0,                                            // queryParams
        "lifecycle",                                  // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        &properties,                                  // putProperties
        &setXmlPropertiesCallback,                    // propertiesCallback
        &setXmlDataCallback,                          // toS3Callback
        data->xmlDocumentLen,                         // toS3CallbackTotalSize
        0,                                            // fromS3Callback
        &setXmlCompleteCallback,                      // completeCallback
        data,                                         // callbackData
        timeoutMs                                     // timeoutMs
    };

    // Perform the request
    request_perform(&params, requestContext);
#endif
}

