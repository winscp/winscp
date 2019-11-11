/** **************************************************************************
 * server_access_logging.c
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

#include <stdlib.h>
#include <string.h>
#include "libs3.h"
#include "request.h"


// get server access logging---------------------------------------------------

typedef struct ConvertBlsData
{
    char *targetBucketReturn;
    int targetBucketReturnLen;
    char *targetPrefixReturn;
    int targetPrefixReturnLen;
    int *aclGrantCountReturn;
    S3AclGrant *aclGrants;

    string_buffer(emailAddress, S3_MAX_GRANTEE_EMAIL_ADDRESS_SIZE);
    string_buffer(userId, S3_MAX_GRANTEE_USER_ID_SIZE);
    string_buffer(userDisplayName, S3_MAX_GRANTEE_DISPLAY_NAME_SIZE);
    string_buffer(groupUri, 128);
    string_buffer(permission, 32);
} ConvertBlsData;


static S3Status convertBlsXmlCallback(const char *elementPath,
                                      const char *data, int dataLen,
                                      void *callbackData)
{
    ConvertBlsData *caData = (ConvertBlsData *) callbackData;

    int fit;

    if (data) {
        if (!strcmp(elementPath, "BucketLoggingStatus/LoggingEnabled/"
                    "TargetBucket")) {
            caData->targetBucketReturnLen +=
                snprintf(&(caData->targetBucketReturn
                           [caData->targetBucketReturnLen]),
                         255 - caData->targetBucketReturnLen - 1,
                         "%.*s", dataLen, data);
            if (caData->targetBucketReturnLen >= 255) {
                return S3StatusTargetBucketTooLong;
            }
        }
        else if (!strcmp(elementPath, "BucketLoggingStatus/LoggingEnabled/"
                    "TargetPrefix")) {
            caData->targetPrefixReturnLen +=
                snprintf(&(caData->targetPrefixReturn
                           [caData->targetPrefixReturnLen]),
                         255 - caData->targetPrefixReturnLen - 1,
                         "%.*s", dataLen, data);
            if (caData->targetPrefixReturnLen >= 255) {
                return S3StatusTargetPrefixTooLong;
            }
        }
        else if (!strcmp(elementPath, "BucketLoggingStatus/LoggingEnabled/"
                         "TargetGrants/Grant/Grantee/EmailAddress")) {
            // AmazonCustomerByEmail
            string_buffer_append(caData->emailAddress, data, dataLen, fit);
            if (!fit) {
                return S3StatusEmailAddressTooLong;
            }
        }
        else if (!strcmp(elementPath,
                         "AccessControlPolicy/AccessControlList/Grant/"
                         "Grantee/ID")) {
            // CanonicalUser
            string_buffer_append(caData->userId, data, dataLen, fit);
            if (!fit) {
                return S3StatusUserIdTooLong;
            }
        }
        else if (!strcmp(elementPath, "BucketLoggingStatus/LoggingEnabled/"
                         "TargetGrants/Grant/Grantee/DisplayName")) {
            // CanonicalUser
            string_buffer_append(caData->userDisplayName, data, dataLen, fit);
            if (!fit) {
                return S3StatusUserDisplayNameTooLong;
            }
        }
        else if (!strcmp(elementPath, "BucketLoggingStatus/LoggingEnabled/"
                         "TargetGrants/Grant/Grantee/URI")) {
            // Group
            string_buffer_append(caData->groupUri, data, dataLen, fit);
            if (!fit) {
                return S3StatusGroupUriTooLong;
            }
        }
        else if (!strcmp(elementPath, "BucketLoggingStatus/LoggingEnabled/"
                         "TargetGrants/Grant/Permission")) {
            // Permission
            string_buffer_append(caData->permission, data, dataLen, fit);
            if (!fit) {
                return S3StatusPermissionTooLong;
            }
        }
    }
    else {
        if (!strcmp(elementPath, "BucketLoggingStatus/LoggingEnabled/"
                    "TargetGrants/Grant")) {
            // A grant has just been completed; so add the next S3AclGrant
            // based on the values read
            if (*(caData->aclGrantCountReturn) == S3_MAX_ACL_GRANT_COUNT) {
                return S3StatusTooManyGrants;
            }

            S3AclGrant *grant = &(caData->aclGrants
                                  [*(caData->aclGrantCountReturn)]);

            if (caData->emailAddress[0]) {
                grant->granteeType = S3GranteeTypeAmazonCustomerByEmail;
                strcpy(grant->grantee.amazonCustomerByEmail.emailAddress,
                       caData->emailAddress);
            }
            else if (caData->userId[0] && caData->userDisplayName[0]) {
                grant->granteeType = S3GranteeTypeCanonicalUser;
                strcpy(grant->grantee.canonicalUser.id, caData->userId);
                strcpy(grant->grantee.canonicalUser.displayName,
                       caData->userDisplayName);
            }
            else if (caData->groupUri[0]) {
                if (!strcmp(caData->groupUri,
                            ACS_GROUP_AWS_USERS)) {
                    grant->granteeType = S3GranteeTypeAllAwsUsers;
                }
                else if (!strcmp(caData->groupUri,
                                 ACS_GROUP_ALL_USERS)) {
                    grant->granteeType = S3GranteeTypeAllUsers;
                }
                else {
                    return S3StatusBadGrantee;
                }
            }
            else {
                return S3StatusBadGrantee;
            }

            if (!strcmp(caData->permission, "READ")) {
                grant->permission = S3PermissionRead;
            }
            else if (!strcmp(caData->permission, "WRITE")) {
                grant->permission = S3PermissionWrite;
            }
            else if (!strcmp(caData->permission, "READ_ACP")) {
                grant->permission = S3PermissionReadACP;
            }
            else if (!strcmp(caData->permission, "WRITE_ACP")) {
                grant->permission = S3PermissionWriteACP;
            }
            else if (!strcmp(caData->permission, "FULL_CONTROL")) {
                grant->permission = S3PermissionFullControl;
            }
            else {
                return S3StatusBadPermission;
            }

            (*(caData->aclGrantCountReturn))++;

            string_buffer_initialize(caData->emailAddress);
            string_buffer_initialize(caData->userId);
            string_buffer_initialize(caData->userDisplayName);
            string_buffer_initialize(caData->groupUri);
            string_buffer_initialize(caData->permission);
        }
    }

    return S3StatusOK;
}


static S3Status convert_bls(char *blsXml, char *targetBucketReturn,
                            char *targetPrefixReturn, int *aclGrantCountReturn,
                            S3AclGrant *aclGrants)
{
    ConvertBlsData data;

    data.targetBucketReturn = targetBucketReturn;
    data.targetBucketReturn[0] = 0;
    data.targetBucketReturnLen = 0;
    data.targetPrefixReturn = targetPrefixReturn;
    data.targetPrefixReturn[0] = 0;
    data.targetPrefixReturnLen = 0;
    data.aclGrantCountReturn = aclGrantCountReturn;
    data.aclGrants = aclGrants;
    *aclGrantCountReturn = 0;
    string_buffer_initialize(data.emailAddress);
    string_buffer_initialize(data.userId);
    string_buffer_initialize(data.userDisplayName);
    string_buffer_initialize(data.groupUri);
    string_buffer_initialize(data.permission);

    // Use a simplexml parser
    SimpleXml simpleXml;
    simplexml_initialize(&simpleXml, &convertBlsXmlCallback, &data);

    S3Status status = simplexml_add(&simpleXml, blsXml, strlen(blsXml));

    simplexml_deinitialize(&simpleXml);

    return status;
}


// Use a rather arbitrary max size for the document of 64K
#define BLS_XML_DOC_MAXSIZE (64 * 1024)


typedef struct GetBlsData
{
    SimpleXml simpleXml;

    S3ResponsePropertiesCallback *responsePropertiesCallback;
    S3ResponseCompleteCallback *responseCompleteCallback;
    void *callbackData;

    char *targetBucketReturn;
    char *targetPrefixReturn;
    int *aclGrantCountReturn;
    S3AclGrant *aclGrants;
    string_buffer(blsXmlDocument, BLS_XML_DOC_MAXSIZE);
} GetBlsData;


static S3Status getBlsPropertiesCallback
    (const S3ResponseProperties *responseProperties, void *callbackData)
{
    GetBlsData *gsData = (GetBlsData *) callbackData;

    return (*(gsData->responsePropertiesCallback))
        (responseProperties, gsData->callbackData);
}


static S3Status getBlsDataCallback(int bufferSize, const char *buffer,
                                   void *callbackData)
{
    GetBlsData *gsData = (GetBlsData *) callbackData;

    int fit;

    string_buffer_append(gsData->blsXmlDocument, buffer, bufferSize, fit);

    return fit ? S3StatusOK : S3StatusXmlDocumentTooLarge;
}


static void getBlsCompleteCallback(S3Status requestStatus,
                                   const S3ErrorDetails *s3ErrorDetails,
                                   void *callbackData)
{
    GetBlsData *gsData = (GetBlsData *) callbackData;

    if (requestStatus == S3StatusOK) {
        // Parse the document
        requestStatus = convert_bls
            (gsData->blsXmlDocument, gsData->targetBucketReturn,
             gsData->targetPrefixReturn, gsData->aclGrantCountReturn,
             gsData->aclGrants);
    }

    (*(gsData->responseCompleteCallback))
        (requestStatus, s3ErrorDetails, gsData->callbackData);

    free(gsData);
}


void S3_get_server_access_logging(const S3BucketContext *bucketContext,
                                  char *targetBucketReturn,
                                  char *targetPrefixReturn,
                                  int *aclGrantCountReturn,
                                  S3AclGrant *aclGrants,
                                  S3RequestContext *requestContext,
                                  int timeoutMs,
                                  const S3ResponseHandler *handler,
                                  void *callbackData)
{
    // Create the callback data
    GetBlsData *gsData = (GetBlsData *) malloc(sizeof(GetBlsData));
    if (!gsData) {
        (*(handler->completeCallback))(S3StatusOutOfMemory, 0, callbackData);
        return;
    }

    gsData->responsePropertiesCallback = handler->propertiesCallback;
    gsData->responseCompleteCallback = handler->completeCallback;
    gsData->callbackData = callbackData;

    gsData->targetBucketReturn = targetBucketReturn;
    gsData->targetPrefixReturn = targetPrefixReturn;
    gsData->aclGrantCountReturn = aclGrantCountReturn;
    gsData->aclGrants = aclGrants;
    string_buffer_initialize(gsData->blsXmlDocument);
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
        0,                                            // key
        0,                                            // queryParams
        "logging",                                    // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        0,                                            // putProperties
        &getBlsPropertiesCallback,                    // propertiesCallback
        0,                                            // toS3Callback
        0,                                            // toS3CallbackTotalSize
        &getBlsDataCallback,                          // fromS3Callback
        &getBlsCompleteCallback,                      // completeCallback
        gsData,                                       // callbackData
        timeoutMs                                     // timeoutMs
    };

    // Perform the request
    request_perform(&params, requestContext);
}



// set server access logging---------------------------------------------------

static S3Status generateSalXmlDocument(const char *targetBucket,
                                       const char *targetPrefix,
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

    append("%s", "<BucketLoggingStatus "
           "xmlns=\"http://doc.s3.amazonaws.com/2006-03-01\">");

    if (targetBucket && targetBucket[0]) {
        append("<LoggingEnabled><TargetBucket>%s</TargetBucket>", targetBucket);
        append("<TargetPrefix>%s</TargetPrefix>",
               targetPrefix ? targetPrefix : "");

        if (aclGrantCount) {
            append("%s", "<TargetGrants>");
            int i;
            for (i = 0; i < aclGrantCount; i++) {
                append("%s", "<Grant><Grantee "
                       "xmlns:xsi=\"http://www.w3.org/2001/"
                       "XMLSchema-instance\" xsi:type=\"");
                const S3AclGrant *grant = &(aclGrants[i]);
                switch (grant->granteeType) {
                case S3GranteeTypeAmazonCustomerByEmail:
                    append("AmazonCustomerByEmail\"><EmailAddress>%s"
                           "</EmailAddress>",
                           grant->grantee.amazonCustomerByEmail.emailAddress);
                    break;
                case S3GranteeTypeCanonicalUser:
                    append("CanonicalUser\"><ID>%s</ID><DisplayName>%s"
                           "</DisplayName>",
                           grant->grantee.canonicalUser.id,
                           grant->grantee.canonicalUser.displayName);
                    break;
                default: // case S3GranteeTypeAllAwsUsers/S3GranteeTypeAllUsers:
                    append("Group\"><URI>%s</URI>",
                           (grant->granteeType == S3GranteeTypeAllAwsUsers) ?
                           ACS_GROUP_AWS_USERS : ACS_GROUP_ALL_USERS);
                    break;
                }
                append("</Grantee><Permission>%s</Permission></Grant>",
                       ((grant->permission == S3PermissionRead) ? "READ" :
                        (grant->permission == S3PermissionWrite) ? "WRITE" :
                        (grant->permission ==
                         S3PermissionReadACP) ? "READ_ACP" :
                        (grant->permission ==
                         S3PermissionWriteACP) ? "WRITE_ACP" : "FULL_CONTROL"));
            }
            append("%s", "</TargetGrants>");
        }
        append("%s", "</LoggingEnabled>");
    }

    append("%s", "</BucketLoggingStatus>");

    return S3StatusOK;
}


typedef struct SetSalData
{
    S3ResponsePropertiesCallback *responsePropertiesCallback;
    S3ResponseCompleteCallback *responseCompleteCallback;
    void *callbackData;

    int salXmlDocumentLen;
    char salXmlDocument[BLS_XML_DOC_MAXSIZE];
    int salXmlDocumentBytesWritten;

} SetSalData;


static S3Status setSalPropertiesCallback
    (const S3ResponseProperties *responseProperties, void *callbackData)
{
    SetSalData *paData = (SetSalData *) callbackData;

    return (*(paData->responsePropertiesCallback))
        (responseProperties, paData->callbackData);
}


static int setSalDataCallback(int bufferSize, char *buffer, void *callbackData)
{
    SetSalData *paData = (SetSalData *) callbackData;

    int remaining = (paData->salXmlDocumentLen -
                     paData->salXmlDocumentBytesWritten);

    int toCopy = bufferSize > remaining ? remaining : bufferSize;

    if (!toCopy) {
        return 0;
    }

    memcpy(buffer, &(paData->salXmlDocument
                     [paData->salXmlDocumentBytesWritten]), toCopy);

    paData->salXmlDocumentBytesWritten += toCopy;

    return toCopy;
}


static void setSalCompleteCallback(S3Status requestStatus,
                                   const S3ErrorDetails *s3ErrorDetails,
                                   void *callbackData)
{
    SetSalData *paData = (SetSalData *) callbackData;

    (*(paData->responseCompleteCallback))
        (requestStatus, s3ErrorDetails, paData->callbackData);

    free(paData);
}


void S3_set_server_access_logging(const S3BucketContext *bucketContext,
                                  const char *targetBucket,
                                  const char *targetPrefix, int aclGrantCount,
                                  const S3AclGrant *aclGrants,
                                  S3RequestContext *requestContext,
                                  int timeoutMs,
                                  const S3ResponseHandler *handler,
                                  void *callbackData)
{
    if (aclGrantCount > S3_MAX_ACL_GRANT_COUNT) {
        (*(handler->completeCallback))
            (S3StatusTooManyGrants, 0, callbackData);
        return;
    }

    SetSalData *data = (SetSalData *) malloc(sizeof(SetSalData));
    if (!data) {
        (*(handler->completeCallback))(S3StatusOutOfMemory, 0, callbackData);
        return;
    }

    // Convert aclGrants to XML document
    S3Status status = generateSalXmlDocument
        (targetBucket, targetPrefix, aclGrantCount, aclGrants,
         &(data->salXmlDocumentLen), data->salXmlDocument,
         sizeof(data->salXmlDocument));
    if (status != S3StatusOK) {
        free(data);
        (*(handler->completeCallback))(status, 0, callbackData);
        return;
    }

    data->responsePropertiesCallback = handler->propertiesCallback;
    data->responseCompleteCallback = handler->completeCallback;
    data->callbackData = callbackData;

    data->salXmlDocumentBytesWritten = 0;

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
        "logging",                                    // subResource
        0,                                            // copySourceBucketName
        0,                                            // copySourceKey
        0,                                            // getConditions
        0,                                            // startByte
        0,                                            // byteCount
        0,                                            // putProperties
        &setSalPropertiesCallback,                    // propertiesCallback
        &setSalDataCallback,                          // toS3Callback
        data->salXmlDocumentLen,                      // toS3CallbackTotalSize
        0,                                            // fromS3Callback
        &setSalCompleteCallback,                      // completeCallback
        data,                                         // callbackData
        timeoutMs                                     // timeoutMs
    };

    // Perform the request
    request_perform(&params, requestContext);
}
