/** **************************************************************************
 * general.c
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

#include <ctype.h>
#include <string.h>
#include "request.h"
#include "simplexml.h"
#include "util.h"

static int initializeCountG = 0;

S3Status S3_initialize(const char *userAgentInfo, int flags,
                       const char *defaultS3HostName)
{
    if (initializeCountG++) {
        return S3StatusOK;
    }

    return request_api_initialize(userAgentInfo, flags, defaultS3HostName);
}


void S3_deinitialize()
{
    if (--initializeCountG) {
        return;
    }

    request_api_deinitialize();
}

const char *S3_get_status_name(S3Status status)
{
    switch (status) {
#define handlecase(s)                           \
        case S3Status##s:                       \
            return #s

        handlecase(OK);
        handlecase(InternalError);
        handlecase(OutOfMemory);
        handlecase(Interrupted);
        handlecase(InvalidBucketNameTooLong);
        handlecase(InvalidBucketNameFirstCharacter);
        handlecase(InvalidBucketNameCharacter);
        handlecase(InvalidBucketNameCharacterSequence);
        handlecase(InvalidBucketNameTooShort);
        handlecase(InvalidBucketNameDotQuadNotation);
        handlecase(QueryParamsTooLong);
        handlecase(FailedToInitializeRequest);
        handlecase(MetaDataHeadersTooLong);
        handlecase(BadMetaData);
        handlecase(BadContentType);
        handlecase(ContentTypeTooLong);
        handlecase(BadMD5);
        handlecase(MD5TooLong);
        handlecase(BadCacheControl);
        handlecase(CacheControlTooLong);
        handlecase(BadContentDispositionFilename);
        handlecase(ContentDispositionFilenameTooLong);
        handlecase(BadContentEncoding);
        handlecase(ContentEncodingTooLong);
        handlecase(BadIfMatchETag);
        handlecase(IfMatchETagTooLong);
        handlecase(BadIfNotMatchETag);
        handlecase(IfNotMatchETagTooLong);
        handlecase(HeadersTooLong);
        handlecase(KeyTooLong);
        handlecase(UriTooLong);
        handlecase(XmlParseFailure);
        handlecase(EmailAddressTooLong);
        handlecase(UserIdTooLong);
        handlecase(UserDisplayNameTooLong);
        handlecase(GroupUriTooLong);
        handlecase(PermissionTooLong);
        handlecase(TargetBucketTooLong);
        handlecase(TargetPrefixTooLong);
        handlecase(TooManyGrants);
        handlecase(BadGrantee);
        handlecase(BadPermission);
        handlecase(XmlDocumentTooLarge);
        handlecase(NameLookupError);
        handlecase(FailedToConnect);
        handlecase(ServerFailedVerification);
        handlecase(ConnectionFailed);
        handlecase(AbortedByCallback);
        handlecase(NotSupported);
        handlecase(ErrorAccessDenied);
        handlecase(ErrorAccountProblem);
        handlecase(ErrorAmbiguousGrantByEmailAddress);
        handlecase(ErrorBadDigest);
        handlecase(ErrorBucketAlreadyExists);
        handlecase(ErrorBucketAlreadyOwnedByYou);
        handlecase(ErrorBucketNotEmpty);
        handlecase(ErrorCredentialsNotSupported);
        handlecase(ErrorCrossLocationLoggingProhibited);
        handlecase(ErrorEntityTooSmall);
        handlecase(ErrorEntityTooLarge);
        handlecase(ErrorExpiredToken);
        handlecase(ErrorIllegalVersioningConfigurationException);
        handlecase(ErrorIncompleteBody);
        handlecase(ErrorIncorrectNumberOfFilesInPostRequest);
        handlecase(ErrorInlineDataTooLarge);
        handlecase(ErrorInternalError);
        handlecase(ErrorInvalidAccessKeyId);
        handlecase(ErrorInvalidAddressingHeader);
        handlecase(ErrorInvalidArgument);
        handlecase(ErrorInvalidBucketName);
        handlecase(ErrorInvalidBucketState);
        handlecase(ErrorInvalidDigest);
        handlecase(ErrorInvalidEncryptionAlgorithmError);
        handlecase(ErrorInvalidLocationConstraint);
        handlecase(ErrorInvalidObjectState);
        handlecase(ErrorInvalidPart);
        handlecase(ErrorInvalidPartOrder);
        handlecase(ErrorInvalidPayer);
        handlecase(ErrorInvalidPolicyDocument);
        handlecase(ErrorInvalidRange);
        handlecase(ErrorInvalidRequest);
        handlecase(ErrorInvalidSecurity);
        handlecase(ErrorInvalidSOAPRequest);
        handlecase(ErrorInvalidStorageClass);
        handlecase(ErrorInvalidTargetBucketForLogging);
        handlecase(ErrorInvalidToken);
        handlecase(ErrorInvalidURI);
        handlecase(ErrorKeyTooLong);
        handlecase(ErrorMalformedACLError);
        handlecase(ErrorMalformedPOSTRequest);
        handlecase(ErrorMalformedXML);
        handlecase(ErrorMaxMessageLengthExceeded);
        handlecase(ErrorMaxPostPreDataLengthExceededError);
        handlecase(ErrorMetadataTooLarge);
        handlecase(ErrorMethodNotAllowed);
        handlecase(ErrorMissingAttachment);
        handlecase(ErrorMissingContentLength);
        handlecase(ErrorMissingRequestBodyError);
        handlecase(ErrorMissingSecurityElement);
        handlecase(ErrorMissingSecurityHeader);
        handlecase(ErrorNoLoggingStatusForKey);
        handlecase(ErrorNoSuchBucket);
        handlecase(ErrorNoSuchKey);
        handlecase(ErrorNoSuchLifecycleConfiguration);
        handlecase(ErrorNoSuchUpload);
        handlecase(ErrorNoSuchVersion);
        handlecase(ErrorNotImplemented);
        handlecase(ErrorNotSignedUp);
        handlecase(ErrorNoSuchBucketPolicy);
        handlecase(ErrorOperationAborted);
        handlecase(ErrorPermanentRedirect);
        handlecase(ErrorPreconditionFailed);
        handlecase(ErrorRedirect);
        handlecase(ErrorRestoreAlreadyInProgress);
        handlecase(ErrorRequestIsNotMultiPartContent);
        handlecase(ErrorRequestTimeout);
        handlecase(ErrorRequestTimeTooSkewed);
        handlecase(ErrorRequestTorrentOfBucketError);
        handlecase(ErrorSignatureDoesNotMatch);
        handlecase(ErrorServiceUnavailable);
        handlecase(ErrorSlowDown);
        handlecase(ErrorTemporaryRedirect);
        handlecase(ErrorTokenRefreshRequired);
        handlecase(ErrorTooManyBuckets);
        handlecase(ErrorUnexpectedContent);
        handlecase(ErrorUnresolvableGrantByEmailAddress);
        handlecase(ErrorUserKeyMustBeSpecified);
        handlecase(ErrorQuotaExceeded);
        handlecase(ErrorUnknown);
        handlecase(HttpErrorMovedTemporarily);
        handlecase(HttpErrorBadRequest);
        handlecase(HttpErrorForbidden);
        handlecase(HttpErrorNotFound);
        handlecase(HttpErrorConflict);
        handlecase(HttpErrorUnknown);
    }

    return "Unknown";
}


S3Status S3_validate_bucket_name(const char *bucketName, S3UriStyle uriStyle)
{
    int virtualHostStyle = (uriStyle == S3UriStyleVirtualHost);
    int len = 0, maxlen = virtualHostStyle ? 63 : 255;
    const char *b = bucketName;

    int hasDot = 0;
    int hasNonDigit = 0;

    while (*b) {
        if (len == maxlen) {
            return S3StatusInvalidBucketNameTooLong;
        }
        else if (isalpha(*b)) {
            len++, b++;
            hasNonDigit = 1;
        }
        else if (isdigit(*b)) {
            len++, b++;
        }
        else if (len == 0) {
            return S3StatusInvalidBucketNameFirstCharacter;
        }
        else if (*b == '_') {
            /* Virtual host style bucket names cannot have underscores */
            if (virtualHostStyle) {
                return S3StatusInvalidBucketNameCharacter;
            }
            len++, b++;
            hasNonDigit = 1;
        }
        else if (*b == '-') {
            /* Virtual host style bucket names cannot have .- */
            if (virtualHostStyle && (b > bucketName) && (*(b - 1) == '.')) {
                return S3StatusInvalidBucketNameCharacterSequence;
            }
            len++, b++;
            hasNonDigit = 1;
        }
        else if (*b == '.') {
            /* Virtual host style bucket names cannot have -. */
            if (virtualHostStyle && (b > bucketName) && (*(b - 1) == '-')) {
                return S3StatusInvalidBucketNameCharacterSequence;
            }
            len++, b++;
            hasDot = 1;
        }
        else {
            return S3StatusInvalidBucketNameCharacter;
        }
    }

    if (len < 3) {
        return S3StatusInvalidBucketNameTooShort;
    }

    /* It's not clear from Amazon's documentation exactly what 'IP address
       style' means.  In its strictest sense, it could mean 'could be a valid
       IP address', which would mean that 255.255.255.255 would be invalid,
       wherase 256.256.256.256 would be valid.  Or it could mean 'has 4 sets
       of digits separated by dots'.  Who knows.  Let's just be really
       conservative here: if it has any dots, and no non-digit characters,
       then we reject it */
    if (hasDot && !hasNonDigit) {
        return S3StatusInvalidBucketNameDotQuadNotation;
    }

    return S3StatusOK;
}


typedef struct ConvertAclData
{
    char *ownerId;
    int ownerIdLen;
    char *ownerDisplayName;
    int ownerDisplayNameLen;
    int *aclGrantCountReturn;
    S3AclGrant *aclGrants;

    string_buffer(emailAddress, S3_MAX_GRANTEE_EMAIL_ADDRESS_SIZE);
    string_buffer(userId, S3_MAX_GRANTEE_USER_ID_SIZE);
    string_buffer(userDisplayName, S3_MAX_GRANTEE_DISPLAY_NAME_SIZE);
    string_buffer(groupUri, 128);
    string_buffer(permission, 32);
} ConvertAclData;


static S3Status convertAclXmlCallback(const char *elementPath,
                                      const char *data, int dataLen,
                                      void *callbackData)
{
    ConvertAclData *caData = (ConvertAclData *) callbackData;

    int fit;

    if (data) {
        if (!strcmp(elementPath, "AccessControlPolicy/Owner/ID")) {
            caData->ownerIdLen +=
                snprintf(&(caData->ownerId[caData->ownerIdLen]),
                         S3_MAX_GRANTEE_USER_ID_SIZE - caData->ownerIdLen - 1,
                         "%.*s", dataLen, data);
            if (caData->ownerIdLen >= S3_MAX_GRANTEE_USER_ID_SIZE) {
                return S3StatusUserIdTooLong;
            }
        }
        else if (!strcmp(elementPath, "AccessControlPolicy/Owner/"
                         "DisplayName")) {
            caData->ownerDisplayNameLen +=
                snprintf(&(caData->ownerDisplayName
                           [caData->ownerDisplayNameLen]),
                         S3_MAX_GRANTEE_DISPLAY_NAME_SIZE -
                         caData->ownerDisplayNameLen - 1,
                         "%.*s", dataLen, data);
            if (caData->ownerDisplayNameLen >=
                S3_MAX_GRANTEE_DISPLAY_NAME_SIZE) {
                return S3StatusUserDisplayNameTooLong;
            }
        }
        else if (!strcmp(elementPath,
                    "AccessControlPolicy/AccessControlList/Grant/"
                    "Grantee/EmailAddress")) {
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
        else if (!strcmp(elementPath,
                         "AccessControlPolicy/AccessControlList/Grant/"
                         "Grantee/DisplayName")) {
            // CanonicalUser
            string_buffer_append(caData->userDisplayName, data, dataLen, fit);
            if (!fit) {
                return S3StatusUserDisplayNameTooLong;
            }
        }
        else if (!strcmp(elementPath,
                         "AccessControlPolicy/AccessControlList/Grant/"
                         "Grantee/URI")) {
            // Group
            string_buffer_append(caData->groupUri, data, dataLen, fit);
            if (!fit) {
                return S3StatusGroupUriTooLong;
            }
        }
        else if (!strcmp(elementPath,
                         "AccessControlPolicy/AccessControlList/Grant/"
                         "Permission")) {
            // Permission
            string_buffer_append(caData->permission, data, dataLen, fit);
            if (!fit) {
                return S3StatusPermissionTooLong;
            }
        }
    }
    else {
        if (!strcmp(elementPath, "AccessControlPolicy/AccessControlList/"
                    "Grant")) {
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
                else if (!strcmp(caData->groupUri,
                                 ACS_GROUP_LOG_DELIVERY)) {
                    grant->granteeType = S3GranteeTypeLogDelivery;
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


S3Status S3_convert_acl(char *aclXml, char *ownerId, char *ownerDisplayName,
                        int *aclGrantCountReturn, S3AclGrant *aclGrants)
{
    ConvertAclData data;

    data.ownerId = ownerId;
    data.ownerIdLen = 0;
    data.ownerId[0] = 0;
    data.ownerDisplayName = ownerDisplayName;
    data.ownerDisplayNameLen = 0;
    data.ownerDisplayName[0] = 0;
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
    simplexml_initialize(&simpleXml, &convertAclXmlCallback, &data);

    S3Status status = simplexml_add(&simpleXml, aclXml, strlen(aclXml));

    simplexml_deinitialize(&simpleXml);

    return status;
}


int S3_status_is_retryable(S3Status status)
{
    switch (status) {
    case S3StatusNameLookupError:
    case S3StatusFailedToConnect:
    case S3StatusConnectionFailed:
    case S3StatusErrorInternalError:
    case S3StatusErrorOperationAborted:
    case S3StatusErrorRequestTimeout:
        return 1;
    default:
        return 0;
    }
}
