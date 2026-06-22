/** **************************************************************************
 * error_parser.c
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

#include <string.h>
#include "error_parser.h"


static S3Status errorXmlCallback(const char *elementPath, const char *data,
                                 int dataLen, void *callbackData)
{
    // We ignore end of element callbacks because we don't care about them
    if (!data) {
        return S3StatusOK;
    }

    ErrorParser *errorParser = (ErrorParser *) callbackData;

    int fit;

    if (!strcmp(elementPath, "Error")) {
        // Ignore, this is the Error element itself, we only care about subs
    }
    // WINSCP: ErrorResponse is for STS
    else if (!strcmp(elementPath, "Error/Code") || !strcmp(elementPath, "ErrorResponse/Error/Code")) {
        string_buffer_append(errorParser->code, data, dataLen, fit);
    }
    else if (!strcmp(elementPath, "Error/Message") || !strcmp(elementPath, "ErrorResponse/Error/Message")) {
        string_buffer_append(errorParser->message, data, dataLen, fit);
        errorParser->s3ErrorDetails.message = errorParser->message;
    }
    else if (!strcmp(elementPath, "Error/Resource")) {
        string_buffer_append(errorParser->resource, data, dataLen, fit);
        errorParser->s3ErrorDetails.resource = errorParser->resource;
    }
    else if (!strcmp(elementPath, "Error/FurtherDetails")) {
        string_buffer_append(errorParser->furtherDetails, data, dataLen, fit);
        errorParser->s3ErrorDetails.furtherDetails = 
            errorParser->furtherDetails;
    }
    else {
        if (strncmp(elementPath, "Error/", sizeof("Error/") - 1)) {
            // If for some weird reason it's not within the Error element,
            // ignore it
            return S3StatusOK;
        }
        // It's an unknown error element.  See if it matches the most
        // recent error element.
        const char *elementName = &(elementPath[sizeof("Error/") - 1]);
        if (errorParser->s3ErrorDetails.extraDetailsCount && 
            !strcmp(elementName, errorParser->s3ErrorDetails.extraDetails
                    [errorParser->s3ErrorDetails.extraDetailsCount - 1].name)) {
            // Append the value
            string_multibuffer_append(errorParser->extraDetailsNamesValues,
                                      data, dataLen, fit);
            // If it didn't fit, remove this extra
            if (!fit) {
                errorParser->s3ErrorDetails.extraDetailsCount--;
            }
            return S3StatusOK;
        }
        // OK, must add another unknown error element, if it will fit.
        if (errorParser->s3ErrorDetails.extraDetailsCount ==
            (int)sizeof(errorParser->extraDetails)) { // WINSCP (cast)
            // Won't fit.  Ignore this one.
            return S3StatusOK;
        }
        // Copy in the name and value
        char *name = string_multibuffer_current
            (errorParser->extraDetailsNamesValues);
        int nameLen = strlen(elementName);
        string_multibuffer_add(errorParser->extraDetailsNamesValues,
                               elementName, nameLen, fit);
        if (!fit) {
            // Name didn't fit; ignore this one.
            return S3StatusOK;
        }
        char *value = string_multibuffer_current
            (errorParser->extraDetailsNamesValues);
        string_multibuffer_add(errorParser->extraDetailsNamesValues,
                               data, dataLen, fit);
        if (!fit) {
            // Value didn't fit; ignore this one.
            return S3StatusOK;
        }
        S3NameValue *nv = 
            &(errorParser->extraDetails
              [errorParser->s3ErrorDetails.extraDetailsCount++]);
        nv->name = name;
        nv->value = value;
    }

    return S3StatusOK;
}


void error_parser_initialize(ErrorParser *errorParser)
{
    errorParser->s3ErrorDetails.message = 0;
    errorParser->s3ErrorDetails.resource = 0;
    errorParser->s3ErrorDetails.furtherDetails = 0;
    errorParser->s3ErrorDetails.extraDetailsCount = 0;
    errorParser->s3ErrorDetails.extraDetails = errorParser->extraDetails;
    errorParser->errorXmlParserInitialized = 0;
    string_buffer_initialize(errorParser->code);
    string_buffer_initialize(errorParser->message);
    string_buffer_initialize(errorParser->resource);
    string_buffer_initialize(errorParser->furtherDetails);
    string_multibuffer_initialize(errorParser->extraDetailsNamesValues);
}


S3Status error_parser_add(ErrorParser *errorParser, const /*WINSCP (const)*/ char *buffer,
                          int bufferSize)
{
    if (!errorParser->errorXmlParserInitialized) {
        simplexml_initialize(&(errorParser->errorXmlParser), &errorXmlCallback,
                             errorParser);
        errorParser->errorXmlParserInitialized = 1;
    }

    return simplexml_add(&(errorParser->errorXmlParser), buffer, bufferSize);
}


void error_parser_convert_status(ErrorParser *errorParser, S3Status *status)
{
    // Convert the error status string into a code
    if (!errorParser->codeLen) {
        return;
    }

#define HANDLE_CODE(name)                                       \
    do {                                                        \
        if (!strcmp(errorParser->code, #name)) {                \
            *status = S3StatusError##name;                      \
            goto code_set;                                      \
        }                                                       \
    } while (0)
    
    HANDLE_CODE(AccessDenied);
    HANDLE_CODE(AccountProblem);
    HANDLE_CODE(AmbiguousGrantByEmailAddress);
    HANDLE_CODE(BadDigest);
    HANDLE_CODE(BucketAlreadyExists);
    HANDLE_CODE(BucketAlreadyOwnedByYou);
    HANDLE_CODE(BucketNotEmpty);
    HANDLE_CODE(CredentialsNotSupported);
    HANDLE_CODE(CrossLocationLoggingProhibited);
    HANDLE_CODE(EntityTooSmall);
    HANDLE_CODE(EntityTooLarge);
    HANDLE_CODE(ExpiredToken);
    HANDLE_CODE(IllegalVersioningConfigurationException); 
    HANDLE_CODE(IncompleteBody);
    HANDLE_CODE(IncorrectNumberOfFilesInPostRequest);
    HANDLE_CODE(InlineDataTooLarge);
    HANDLE_CODE(InternalError);
    HANDLE_CODE(InvalidAccessKeyId);
    HANDLE_CODE(InvalidAddressingHeader);
    HANDLE_CODE(InvalidArgument);
    HANDLE_CODE(InvalidBucketName);
    HANDLE_CODE(InvalidBucketState); 
    HANDLE_CODE(InvalidDigest);
    HANDLE_CODE(InvalidEncryptionAlgorithmError);
    HANDLE_CODE(InvalidLocationConstraint);
    HANDLE_CODE(InvalidObjectState); 
    HANDLE_CODE(InvalidPart); 
    HANDLE_CODE(InvalidPartOrder);
    HANDLE_CODE(InvalidPayer);
    HANDLE_CODE(InvalidPolicyDocument);
    HANDLE_CODE(InvalidRange);
    HANDLE_CODE(InvalidRequest);
    HANDLE_CODE(InvalidSecurity);
    HANDLE_CODE(InvalidSOAPRequest);
    HANDLE_CODE(InvalidStorageClass);
    HANDLE_CODE(InvalidTargetBucketForLogging);
    HANDLE_CODE(InvalidToken);
    HANDLE_CODE(InvalidURI);
    HANDLE_CODE(KeyTooLong);
    HANDLE_CODE(MalformedACLError);
    HANDLE_CODE(MalformedPOSTRequest);
    HANDLE_CODE(MalformedXML);
    HANDLE_CODE(MaxMessageLengthExceeded);
    HANDLE_CODE(MaxPostPreDataLengthExceededError);
    HANDLE_CODE(MetadataTooLarge);
    HANDLE_CODE(MethodNotAllowed);
    HANDLE_CODE(MissingAttachment);
    HANDLE_CODE(MissingContentLength);
    HANDLE_CODE(MissingRequestBodyError);
    HANDLE_CODE(MissingSecurityElement);
    HANDLE_CODE(MissingSecurityHeader);
    HANDLE_CODE(NoLoggingStatusForKey);
    HANDLE_CODE(NoSuchBucket);
    HANDLE_CODE(NoSuchKey);
    HANDLE_CODE(NoSuchLifecycleConfiguration);
    HANDLE_CODE(NoSuchUpload);
    HANDLE_CODE(NoSuchVersion);
    HANDLE_CODE(NotImplemented);
    HANDLE_CODE(NotSignedUp);
    HANDLE_CODE(NoSuchBucketPolicy);
    HANDLE_CODE(OperationAborted);
    HANDLE_CODE(PermanentRedirect);
    HANDLE_CODE(PreconditionFailed);
    HANDLE_CODE(Redirect);
    HANDLE_CODE(RestoreAlreadyInProgress);
    HANDLE_CODE(RequestIsNotMultiPartContent);
    HANDLE_CODE(RequestTimeout);
    HANDLE_CODE(RequestTimeTooSkewed);
    HANDLE_CODE(RequestTorrentOfBucketError);
    HANDLE_CODE(SignatureDoesNotMatch);
    HANDLE_CODE(ServiceUnavailable);
    HANDLE_CODE(SlowDown);
    HANDLE_CODE(TemporaryRedirect);
    HANDLE_CODE(TokenRefreshRequired);
    HANDLE_CODE(TooManyBuckets);
    HANDLE_CODE(UnexpectedContent);
    HANDLE_CODE(UnresolvableGrantByEmailAddress);
    HANDLE_CODE(UserKeyMustBeSpecified);
    HANDLE_CODE(QuotaExceeded);
    HANDLE_CODE(AuthorizationHeaderMalformed); // WINSCP
    *status = S3StatusErrorUnknown;

 code_set:

    return;
}


// Always call this
void error_parser_deinitialize(ErrorParser *errorParser)
{
    if (errorParser->errorXmlParserInitialized) {
        simplexml_deinitialize(&(errorParser->errorXmlParser));
    }
}
