/** **************************************************************************
 * @file libs3.h
 * @details
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

#ifndef LIBS3_H
#define LIBS3_H

#include <stdint.h>
#ifndef WINSCP
#include <sys/select.h>
#endif

#ifdef WINSCP
#define LIBS3_VER_MAJOR "4"
#define LIBS3_VER_MINOR "1"
#endif


#ifdef __cplusplus
extern "C" {
#endif


/** **************************************************************************
 * @mainpage
 * Overview
 * --------
 *
 * This library provides an API for using Amazon's S3 service (see
 * https://s3.amazonaws.com/).  Its design goals are:
 *
 * - To provide a simple and straightforward API for accessing all of S3's
 *   functionality
 * - To not require the developer using libs3 to need to know anything about:
 *     - HTTP
 *     - XML
 *     - SSL
 *
 *   In other words, this API is meant to stand on its own, without requiring
 *   any implicit knowledge of how S3 services are accessed using HTTP
 *   protocols.
 *
 * - To be usable from multithreaded code
 * - To be usable by code which wants to process multiple S3 requests
 *   simultaneously from a single thread
 * - To be usable in the simple, straightforward way using sequentialized
 *   blocking requests
 *
 * The general usage pattern of libs3 is:
 *
 * - Initialize libs3 once per program by calling S3_initialize() at program
 *   start up time
 * - Make any number of requests to S3 for getting, putting, or listing
 *   S3 buckets or objects, or modifying the ACLs associated with buckets
 *   or objects, using one of three general approaches:
 *   1. Simple blocking requests, one at a time
 *   2. Multiple threads each making simple blocking requests
 *   3. From a single thread, managing multiple S3 requests simultaneously
 *      using file descriptors and a select()/poll() loop
 * - Shut down libs3 at program exit time by calling S3_deinitialize()
 *
 * All functions which send requests to S3 return their results via a set of
 * callback functions which must be supplied to libs3 at the time that the
 * request is initiated.  libs3 will call these functions back in the thread
 * calling the libs3 function if blocking requests are made (i.e., if the
 * S3RequestContext for the function invocation is passed in as NULL).
 * If an S3RequestContext is used to drive multiple S3 requests
 * simultaneously, then the callbacks will be made from the thread which
 * calls S3_runall_request_context() or S3_runonce_request_context(), or
 * possibly from the thread which calls S3_destroy_request_context(), if
 * S3 requests are in progress at the time that this function is called.
 *
 * NOTE: Response headers from Amazon S3 are limited to 4K (2K of metas is all
 * that Amazon supports, and libs3 allows Amazon an additional 2K of headers).
 *
 * NOTE: Because HTTP and the S3 REST protocol are highly under-specified,
 * libs3 must make some assumptions about the maximum length of certain HTTP
 * elements (such as headers) that it will accept.  While efforts have been
 * made to enforce maximums which are beyond that expected to be needed by any
 * user of S3, it is always possible that these maximums may be too low in
 * some rare circumstances.  Bug reports should this unlikely situation occur
 * would be most appreciated.
 *
 * Threading Rules
 * ---------------
 *
 * 1. All arguments passed to any function must not be modified directly until
 *    the function returns.
 * 2. All S3RequestContext and S3Request arguments passed to all functions may
 *    not be passed to any other libs3 function by any other thread until the
 *    function returns.
 * 3. All functions may be called simultaneously by multiple threads as long
 *    as (1) and (2) are observed, EXCEPT for S3_initialize(), which must be
 *    called from one thread at a time only.
 * 4. All callbacks will be made in the thread of the caller of the function
 *    which invoked them, so the caller of all libs3 functions should not hold
 *    locks that it would try to re-acquire in a callback, as this may
 *    deadlock.
 ************************************************************************** **/


/** **************************************************************************
 * Constants
 ************************************************************************** **/

/**
 * S3_MAX_HOSTNAME_SIZE is the maximum size we allow for a host name
 **/
#define S3_MAX_HOSTNAME_SIZE               255

/**
 * This is the default hostname that is being used for the S3 requests
 **/
#define S3_DEFAULT_HOSTNAME                "s3.amazonaws.com"

#define S3_SERVICE                         "s3" // WINSCP


/**
 * S3_MAX_BUCKET_NAME_SIZE is the maximum size of a bucket name.
 **/

#define S3_MAX_BUCKET_NAME_SIZE            255

/**
 * S3_MAX_KEY_SIZE is the maximum size of keys that Amazon S3 supports.
 **/
#define S3_MAX_KEY_SIZE                    1024


/**
 * S3_MAX_METADATA_SIZE is the maximum number of bytes allowed for
 * x-amz-meta header names and values in any request passed to Amazon S3
 **/
#define S3_MAX_METADATA_SIZE               2048


/**
 * S3_METADATA_HEADER_NAME_PREFIX is the prefix of an S3 "meta header"
 **/
#define S3_METADATA_HEADER_NAME_PREFIX     "x-amz-meta-"


/**
 * S3_MAX_METADATA_COUNT is the maximum number of x-amz-meta- headers that
 * could be included in a request to S3.  The smallest meta header is
 * "x-amz-meta-n: v".  Since S3 doesn't count the ": " against the total, the
 * smallest amount of data to count for a header would be the length of
 * "x-amz-meta-nv".
 **/
#define S3_MAX_METADATA_COUNT \
    (S3_MAX_METADATA_SIZE / (sizeof(S3_METADATA_HEADER_NAME_PREFIX "nv") - 1))


/**
 * S3_MAX_ACL_GRANT_COUNT is the maximum number of ACL grants that may be
 * set on a bucket or object at one time.  It is also the maximum number of
 * ACL grants that the XML ACL parsing routine will parse.
 **/
#define S3_MAX_ACL_GRANT_COUNT             100


/**
 * This is the maximum number of characters (including terminating \0) that
 * libs3 supports in an ACL grantee email address.
 **/
#define S3_MAX_GRANTEE_EMAIL_ADDRESS_SIZE  128


/**
 * This is the maximum number of characters (including terminating \0) that
 * libs3 supports in an ACL grantee user id.
 **/
#define S3_MAX_GRANTEE_USER_ID_SIZE        128


/**
 * This is the maximum number of characters (including terminating \0) that
 * libs3 supports in an ACL grantee user display name.
 **/
#define S3_MAX_GRANTEE_DISPLAY_NAME_SIZE   128


/**
 * This is the maximum number of characters that will be stored in the
 * return buffer for the utility function which computes an HTTP authenticated
 * query string
 **/
#define S3_MAX_AUTHENTICATED_QUERY_STRING_SIZE \
    (sizeof("https:///") + S3_MAX_HOSTNAME_SIZE + (S3_MAX_KEY_SIZE * 3) + \
     sizeof("?AWSAccessKeyId=") + 32 + sizeof("&Expires=") + 32 + \
     sizeof("&Signature=") + 28 + 1)


/**
 * This constant is used by the S3_initialize() function, to specify that
 * the winsock library should be initialized by libs3; only relevent on
 * Microsoft Windows platforms.
 **/
#define S3_INIT_WINSOCK                    1
/**
 * This constant is used by the S3_initialize() function, to enable peer SSL
 * certificate by default for all requests.  If this is not set in the
 * flags passed to S3_initialize(), then the SSL certificate of the peer
 * will NOT be verified by default (but can still be enabled on a per request
 * basis by calling S3_set_request_context_verify_peer).
 */
#define S3_INIT_VERIFY_PEER                2


/**
 * This convenience constant is used by the S3_initialize() function to
 * indicate that all libraries required by libs3 should be initialized.
 **/
#define S3_INIT_ALL                        (S3_INIT_WINSOCK)


/**
 * The default region identifier used to scope the signing key
 */
#define S3_DEFAULT_REGION                  "us-east-1"

// WINSCP
// according to https://docs.aws.amazon.com/IAM/latest/APIReference/API_AccessKey.html max length is nowadays 128
#define S3_MAX_ACCESS_KEY_ID_LENGTH 128
#define S3_MAX_REGION_LENGTH 32


/** **************************************************************************
 * Enumerations
 ************************************************************************** **/

/**
 * S3Status is a status code as returned by a libs3 function.  The meaning of
 * each status code is defined in the comments for each function which returns
 * that status.
 **/
typedef enum
{
    S3StatusOK                                              ,

    /**
     * Errors that prevent the S3 request from being issued or response from
     * being read
     **/
    S3StatusInternalError                                   ,
    S3StatusOutOfMemory                                     ,
    S3StatusInterrupted                                     ,
    S3StatusInvalidBucketNameTooLong                        ,
    S3StatusInvalidBucketNameFirstCharacter                 ,
    S3StatusInvalidBucketNameCharacter                      ,
    S3StatusInvalidBucketNameCharacterSequence              ,
    S3StatusInvalidBucketNameTooShort                       ,
    S3StatusInvalidBucketNameDotQuadNotation                ,
    S3StatusQueryParamsTooLong                              ,
    S3StatusFailedToInitializeRequest                       ,
    S3StatusMetaDataHeadersTooLong                          ,
    S3StatusBadMetaData                                     ,
    S3StatusBadContentType                                  ,
    S3StatusContentTypeTooLong                              ,
    S3StatusBadMD5                                          ,
    S3StatusMD5TooLong                                      ,
    S3StatusBadCacheControl                                 ,
    S3StatusCacheControlTooLong                             ,
    S3StatusBadContentDispositionFilename                   ,
    S3StatusContentDispositionFilenameTooLong               ,
    S3StatusBadContentEncoding                              ,
    S3StatusContentEncodingTooLong                          ,
    S3StatusBadIfMatchETag                                  ,
    S3StatusIfMatchETagTooLong                              ,
    S3StatusBadIfNotMatchETag                               ,
    S3StatusIfNotMatchETagTooLong                           ,
    S3StatusHeadersTooLong                                  ,
    S3StatusKeyTooLong                                      ,
    S3StatusUriTooLong                                      ,
    S3StatusXmlParseFailure                                 ,
    S3StatusEmailAddressTooLong                             ,
    S3StatusUserIdTooLong                                   ,
    S3StatusUserDisplayNameTooLong                          ,
    S3StatusGroupUriTooLong                                 ,
    S3StatusPermissionTooLong                               ,
    S3StatusTargetBucketTooLong                             ,
    S3StatusTargetPrefixTooLong                             ,
    S3StatusTooManyGrants                                   ,
    S3StatusBadGrantee                                      ,
    S3StatusBadPermission                                   ,
    S3StatusXmlDocumentTooLarge                             ,
    S3StatusNameLookupError                                 ,
    S3StatusFailedToConnect                                 ,
    S3StatusServerFailedVerification                        ,
    S3StatusConnectionFailed                                ,
    S3StatusAbortedByCallback                               ,
    S3StatusNotSupported                                    ,
    S3StatusUploadIdTooLong                                 , // WINSCP

    /**
     * Errors from the S3 service
     **/
    S3StatusErrorAccessDenied                               ,
    S3StatusErrorAccountProblem                             ,
    S3StatusErrorAmbiguousGrantByEmailAddress               ,
    S3StatusErrorBadDigest                                  ,
    S3StatusErrorBucketAlreadyExists                        ,
    S3StatusErrorBucketAlreadyOwnedByYou                    ,
    S3StatusErrorBucketNotEmpty                             ,
    S3StatusErrorCredentialsNotSupported                    ,
    S3StatusErrorCrossLocationLoggingProhibited             ,
    S3StatusErrorEntityTooSmall                             ,
    S3StatusErrorEntityTooLarge                             ,
    S3StatusErrorExpiredToken                               ,
    S3StatusErrorIllegalVersioningConfigurationException    ,
    S3StatusErrorIncompleteBody                             ,
    S3StatusErrorIncorrectNumberOfFilesInPostRequest        ,
    S3StatusErrorInlineDataTooLarge                         ,
    S3StatusErrorInternalError                              ,
    S3StatusErrorInvalidAccessKeyId                         ,
    S3StatusErrorInvalidAddressingHeader                    ,
    S3StatusErrorInvalidArgument                            ,
    S3StatusErrorInvalidBucketName                          ,
    S3StatusErrorInvalidBucketState                         ,
    S3StatusErrorInvalidDigest                              ,
    S3StatusErrorInvalidEncryptionAlgorithmError            ,
    S3StatusErrorInvalidLocationConstraint                  ,
    S3StatusErrorInvalidObjectState                         ,
    S3StatusErrorInvalidPart                                ,
    S3StatusErrorInvalidPartOrder                           ,
    S3StatusErrorInvalidPayer                               ,
    S3StatusErrorInvalidPolicyDocument                      ,
    S3StatusErrorInvalidRange                               ,
    S3StatusErrorInvalidRequest                             ,
    S3StatusErrorInvalidSecurity                            ,
    S3StatusErrorInvalidSOAPRequest                         ,
    S3StatusErrorInvalidStorageClass                        ,
    S3StatusErrorInvalidTargetBucketForLogging              ,
    S3StatusErrorInvalidToken                               ,
    S3StatusErrorInvalidURI                                 ,
    S3StatusErrorKeyTooLong                                 ,
    S3StatusErrorMalformedACLError                          ,
    S3StatusErrorMalformedPOSTRequest                       ,
    S3StatusErrorMalformedXML                               ,
    S3StatusErrorMaxMessageLengthExceeded                   ,
    S3StatusErrorMaxPostPreDataLengthExceededError          ,
    S3StatusErrorMetadataTooLarge                           ,
    S3StatusErrorMethodNotAllowed                           ,
    S3StatusErrorMissingAttachment                          ,
    S3StatusErrorMissingContentLength                       ,
    S3StatusErrorMissingRequestBodyError                    ,
    S3StatusErrorMissingSecurityElement                     ,
    S3StatusErrorMissingSecurityHeader                      ,
    S3StatusErrorNoLoggingStatusForKey                      ,
    S3StatusErrorNoSuchBucket                               ,
    S3StatusErrorNoSuchKey                                  ,
    S3StatusErrorNoSuchLifecycleConfiguration               ,
    S3StatusErrorNoSuchUpload                               ,
    S3StatusErrorNoSuchVersion                              ,
    S3StatusErrorNotImplemented                             ,
    S3StatusErrorNotSignedUp                                ,
    S3StatusErrorNoSuchBucketPolicy                         ,
    S3StatusErrorOperationAborted                           ,
    S3StatusErrorPermanentRedirect                          ,
    S3StatusErrorPreconditionFailed                         ,
    S3StatusErrorRedirect                                   ,
    S3StatusErrorRestoreAlreadyInProgress                   ,
    S3StatusErrorRequestIsNotMultiPartContent               ,
    S3StatusErrorRequestTimeout                             ,
    S3StatusErrorRequestTimeTooSkewed                       ,
    S3StatusErrorRequestTorrentOfBucketError                ,
    S3StatusErrorSignatureDoesNotMatch                      ,
    S3StatusErrorServiceUnavailable                         ,
    S3StatusErrorSlowDown                                   ,
    S3StatusErrorTemporaryRedirect                          ,
    S3StatusErrorTokenRefreshRequired                       ,
    S3StatusErrorTooManyBuckets                             ,
    S3StatusErrorUnexpectedContent                          ,
    S3StatusErrorUnresolvableGrantByEmailAddress            ,
    S3StatusErrorUserKeyMustBeSpecified                     ,
    S3StatusErrorQuotaExceeded                              ,
    S3StatusErrorAuthorizationHeaderMalformed               , // WINSCP
    S3StatusErrorUnknown                                    ,

    /**
     * The following are HTTP errors returned by S3 without enough detail to
     * distinguish any of the above S3StatusError conditions
     **/
    S3StatusHttpErrorMovedTemporarily                       ,
    S3StatusHttpErrorBadRequest                             ,
    S3StatusHttpErrorForbidden                              ,
    S3StatusHttpErrorNotFound                               ,
    S3StatusHttpErrorConflict                               ,
    S3StatusHttpErrorUnknown
} S3Status;


/**
 * S3Protocol represents a protocol that may be used for communicating a
 * request to the Amazon S3 service.
 *
 * In general, HTTPS is greatly preferred (and should be the default of any
 * application using libs3) because it protects any data being sent to or
 * from S3 using strong encryption.  However, HTTPS is much more CPU intensive
 * than HTTP, and if the caller is absolutely certain that it is OK for the
 * data to be viewable by anyone in transit, then HTTP can be used.
 **/
typedef enum
{
    S3ProtocolHTTPS                     = 0,
    S3ProtocolHTTP                      = 1
} S3Protocol;


/**
 * S3UriStyle defines the form that an Amazon S3 URI identifying a bucket or
 * object can take.  They are of these forms:
 *
 * Virtual Host: ${protocol}://${bucket}.s3.amazonaws.com/[${key}]
 * Path: ${protocol}://s3.amazonaws.com/${bucket}/[${key}]
 *
 * It is generally better to use the Virual Host URI form, because it ensures
 * that the bucket name used is compatible with normal HTTP GETs and POSTs of
 * data to/from the bucket.  However, if DNS lookups for the bucket are too
 * slow or unreliable for some reason, Path URI form may be used.
 **/
typedef enum
{
    S3UriStyleVirtualHost               = 0,
    S3UriStylePath                      = 1
} S3UriStyle;


/**
 * S3GranteeType defines the type of Grantee used in an S3 ACL Grant.
 * Amazon Customer By Email - identifies the Grantee using their Amazon S3
 *     account email address
 * Canonical User - identifies the Grantee by S3 User ID and Display Name,
 *     which can only be obtained by making requests to S3, for example, by
 *     listing owned buckets
 * All AWS Users - identifies all authenticated AWS users
 * All Users - identifies all users
 * Log Delivery - identifies the Amazon group responsible for writing
 *                server access logs into buckets
 **/
typedef enum
{
    S3GranteeTypeAmazonCustomerByEmail  = 0,
    S3GranteeTypeCanonicalUser          = 1,
    S3GranteeTypeAllAwsUsers            = 2,
    S3GranteeTypeAllUsers               = 3,
    S3GranteeTypeLogDelivery            = 4
} S3GranteeType;


/**
 * This is an individual permission granted to a grantee in an S3 ACL Grant.
 * Read permission gives the Grantee the permission to list the bucket, or
 *     read the object or its metadata
 * Write permission gives the Grantee the permission to create, overwrite, or
 *     delete any object in the bucket, and is not supported for objects
 * ReadACP permission gives the Grantee the permission to read the ACP for
 *     the bucket or object; the owner of the bucket or object always has
 *     this permission implicitly
 * WriteACP permission gives the Grantee the permission to overwrite the ACP
 *     for the bucket or object; the owner of the bucket or object always has
 *     this permission implicitly
 * FullControl permission gives the Grantee all permissions specified by the
 *     Read, Write, ReadACP, and WriteACP permissions
 **/
typedef enum
{
    S3PermissionRead                    = 0,
    S3PermissionWrite                   = 1,
    S3PermissionReadACP                 = 2,
    S3PermissionWriteACP                = 3,
    S3PermissionFullControl             = 4
} S3Permission;


/**
 * S3CannedAcl is an ACL that can be specified when an object is created or
 * updated.  Each canned ACL has a predefined value when expanded to a full
 * set of S3 ACL Grants.
 * Private canned ACL gives the owner FULL_CONTROL and no other permissions
 *     are issued
 * Public Read canned ACL gives the owner FULL_CONTROL and all users Read
 *     permission
 * Public Read Write canned ACL gives the owner FULL_CONTROL and all users
 *     Read and Write permission
 * AuthenticatedRead canned ACL gives the owner FULL_CONTROL and authenticated
 *     S3 users Read permission
 **/
typedef enum
{
    S3CannedAclPrivate                  = 0, /* private */
    S3CannedAclPublicRead               = 1, /* public-read */
    S3CannedAclPublicReadWrite          = 2, /* public-read-write */
    S3CannedAclAuthenticatedRead        = 3, /* authenticated-read */
    S3CannedAclBucketOwnerFullControl   = 4  /* bucket-owner-full-control */
} S3CannedAcl;


/** **************************************************************************
 * Data Types
 ************************************************************************** **/

/**
 * An S3RequestContext manages multiple S3 requests simultaneously; see the
 * S3_XXX_request_context functions below for details
 **/
typedef struct S3RequestContext S3RequestContext;


/**
 * S3NameValue represents a single Name - Value pair, used to represent either
 * S3 metadata associated with a key, or S3 error details.
 **/
typedef struct S3NameValue
{
    /**
     * The name part of the Name - Value pair
     **/
    const char *name;

    /**
     * The value part of the Name - Value pair
     **/
    const char *value;
} S3NameValue;


/**
 * S3ResponseProperties is passed to the properties callback function which is
 * called when the complete response properties have been received.  Some of
 * the fields of this structure are optional and may not be provided in the
 * response, and some will always be provided in the response.
 **/
typedef struct S3ResponseProperties
{
    /**
     * This optional field identifies the request ID and may be used when
     * reporting problems to Amazon.
     **/
    const char *requestId;

    /**
     * This optional field identifies the request ID and may be used when
     * reporting problems to Amazon.
     **/
    const char *requestId2;

    /**
     * This optional field is the content type of the data which is returned
     * by the request.  If not provided, the default can be assumed to be
     * "binary/octet-stream".
     **/
    const char *contentType;

    /**
     * This optional field is the content length of the data which is returned
     * in the response.  A negative value means that this value was not
     * provided in the response.  A value of 0 means that there is no content
     * provided.  A positive value gives the number of bytes in the content of
     * the response.
     **/
    uint64_t contentLength;

    /**
     * This optional field names the server which serviced the request.
     **/
    const char *server;

    /**
     * This optional field provides a string identifying the unique contents
     * of the resource identified by the request, such that the contents can
     * be assumed not to be changed if the same eTag is returned at a later
     * time decribing the same resource.  This is an MD5 sum of the contents.
     **/
    const char *eTag;

    /**
     * This optional field provides the last modified time, relative to the
     * Unix epoch, of the contents.  If this value is < 0, then the last
     * modified time was not provided in the response.  If this value is >= 0,
     * then the last modified date of the contents are available as a number
     * of seconds since the UNIX epoch.
     *
     **/
    int64_t lastModified;

    /**
     * This is the number of user-provided meta data associated with the
     * resource.
     **/
    int metaDataCount;

    /**
     * These are the meta data associated with the resource.  In each case,
     * the name will not include any S3-specific header prefixes
     * (i.e. x-amz-meta- will have been removed from the beginning), and
     * leading and trailing whitespace will have been stripped from the value.
     **/
    const S3NameValue *metaData;

    /**
     * This optional field provides an indication of whether or not
     * server-side encryption was used for the object.  This field is only
     * meaningful if the request was an object put, copy, get, or head
     * request.
     * If this value is 0, then server-side encryption is not in effect for
     * the object (or the request was one for which server-side encryption is
     * not a meaningful value); if this value is non-zero, then server-side
     * encryption is in effect for the object.
     **/
    char usesServerSideEncryption;
} S3ResponseProperties;


/**
 * S3AclGrant identifies a single grant in the ACL for a bucket or object.  An
 * ACL is composed of any number of grants, which specify a grantee and the
 * permissions given to that grantee.  S3 does not normalize ACLs in any way,
 * so a redundant ACL specification will lead to a redundant ACL stored in S3.
 **/
typedef struct S3AclGrant
{
    /**
     * The granteeType gives the type of grantee specified by this grant.
     **/
    S3GranteeType granteeType;
    /**
     * The identifier of the grantee that is set is determined by the
     * granteeType:
     *
     * S3GranteeTypeAmazonCustomerByEmail - amazonCustomerByEmail.emailAddress
     * S3GranteeTypeCanonicalUser - canonicalUser.id, canonicalUser.displayName
     * S3GranteeTypeAllAwsUsers - none
     * S3GranteeTypeAllUsers - none
     **/
    union
    {
        /**
         * This structure is used iff the granteeType is
         * S3GranteeTypeAmazonCustomerByEmail.
         **/
        struct
        {
            /**
             * This is the email address of the Amazon Customer being granted
             * permissions by this S3AclGrant.
             **/
            char emailAddress[S3_MAX_GRANTEE_EMAIL_ADDRESS_SIZE];
        } amazonCustomerByEmail;
        /**
         * This structure is used iff the granteeType is
         * S3GranteeTypeCanonicalUser.
         **/
        struct
        {
            /**
             * This is the CanonicalUser ID of the grantee
             **/
            char id[S3_MAX_GRANTEE_USER_ID_SIZE];
            /**
             * This is the display name of the grantee
             **/
            char displayName[S3_MAX_GRANTEE_DISPLAY_NAME_SIZE];
        } canonicalUser;
    } grantee;
    /**
     * This is the S3Permission to be granted to the grantee
     **/
    S3Permission permission;
} S3AclGrant;


/**
 * A context for working with objects within a bucket.  A bucket context holds
 * all information necessary for working with a bucket, and may be used
 * repeatedly over many consecutive (or simultaneous) calls into libs3 bucket
 * operation functions.
 **/
typedef struct S3BucketContext
{
    /**
     * The name of the host to connect to when making S3 requests.  If set to
     * NULL, the default S3 hostname passed in to S3_initialize will be used.
     **/
    // WINSCP: Can contain port number
    const char *hostName;

    /**
     * The name of the bucket to use in the bucket context
     **/
    const char *bucketName;

    /**
     * The protocol to use when accessing the bucket
     **/
    S3Protocol protocol;

    /**
     * The URI style to use for all URIs sent to Amazon S3 while working with
     * this bucket context
     **/
    S3UriStyle uriStyle;

    /**
     * The Amazon Access Key ID to use for access to the bucket
     **/
    const char *accessKeyId;

    /**
     *  The Amazon Secret Access Key to use for access to the bucket
     **/
    const char *secretAccessKey;

    /**
     *  The Amazon Security Token used to generate Temporary Security Credentials
     **/
    const char *securityToken;

    /**
     * The AWS region to which to scope the signing key used for authorization.
     * If NULL, the default region ("us-east-1") will be used.
     */
    const char *authRegion;

    const char *service; // WINSCP
} S3BucketContext;


/**
 * This is a single entry supplied to the list bucket callback by a call to
 * S3_list_bucket.  It identifies a single matching key from the list
 * operation.
 **/
typedef struct S3ListBucketContent
{
    /**
     * This is the next key in the list bucket results.
     **/
    const char *key;

    /**
     * This is the number of seconds since UNIX epoch of the last modified
     * date of the object identified by the key.
     **/
    int64_t lastModified;
    const char *lastModifiedStr; // WINSCP 

    /**
     * This gives a tag which gives a signature of the contents of the object,
     * which is the MD5 of the contents of the object.
     **/
    const char *eTag;

    /**
     * This is the size of the object in bytes.
     **/
    uint64_t size;

    /**
     * This is the ID of the owner of the key; it is present only if access
     * permissions allow it to be viewed.
     **/
    const char *ownerId;

    /**
     * This is the display name of the owner of the key; it is present only if
     * access permissions allow it to be viewed.
     **/
    const char *ownerDisplayName;
} S3ListBucketContent;


/**
 * This is a single entry supplied to the list bucket callback by a call to
 * S3_list_bucket.  It identifies a single matching key from the list
 * operation.
 **/
typedef struct S3ListMultipartUpload
{
    /**
     * This is the next key in the list bucket results.
     **/
    const char *key;

    const char *uploadId;
    const char *initiatorId;
    const char *initiatorDisplayName;

    /**
     * This is the ID of the owner of the key; it is present only if access
     * permissions allow it to be viewed.
     **/
    const char *ownerId;

    /**
     * This is the display name of the owner of the key; it is present only if
     * access permissions allow it to be viewed.
     **/
    const char *ownerDisplayName;

    const char *storageClass;

    /**
     * This is the number of seconds since UNIX epoch of the last modified
     * date of the object identified by the key.
     **/
    int64_t initiated;

} S3ListMultipartUpload;


typedef struct S3ListPart
{
    const char *eTag;

    /**
     * This is the number of seconds since UNIX epoch of the last modified
     * date of the object identified by the key.
     **/
    int64_t lastModified;
    uint64_t partNumber;
    uint64_t size;
} S3ListPart;


/**
 * S3PutProperties is the set of properties that may optionally be set by the
 * user when putting objects to S3.  Each field of this structure is optional
 * and may or may not be present.
 **/
typedef struct S3PutProperties
{
    /**
     * If present, this is the Content-Type that should be associated with the
     * object.  If not provided, S3 defaults to "binary/octet-stream".
     **/
    const char *contentType;

    /**
     * If present, this provides the MD5 signature of the contents, and is
     * used to validate the contents.  This is highly recommended by Amazon
     * but not required.  Its format is as a base64-encoded MD5 sum.
     **/
    const char *md5;

    /**
     * If present, this gives a Cache-Control header string to be supplied to
     * HTTP clients which download this
     **/
    const char *cacheControl;

    /**
     * If present, this gives the filename to save the downloaded file to,
     * whenever the object is downloaded via a web browser.  This is only
     * relevent for objects which are intended to be shared to users via web
     * browsers and which is additionally intended to be downloaded rather
     * than viewed.
     **/
    const char *contentDispositionFilename;

    /**
     * If present, this identifies the content encoding of the object.  This
     * is only applicable to encoded (usually, compressed) content, and only
     * relevent if the object is intended to be downloaded via a browser.
     **/
    const char *contentEncoding;

    /**
     * If >= 0, this gives an expiration date for the content.  This
     * information is typically only delivered to users who download the
     * content via a web browser.
     **/
    int64_t expires;

    /**
     * This identifies the "canned ACL" that should be used for this object.
     * The default (0) gives only the owner of the object access to it.
     **/
    S3CannedAcl cannedAcl;

    /**
     * This is the number of values in the metaData field.
     **/
    int metaDataCount;

    /**
     * These are the meta data to pass to S3.  In each case, the name part of
     * the Name - Value pair should not include any special S3 HTTP header
     * prefix (i.e., should be of the form 'foo', NOT 'x-amz-meta-foo').
     **/
    const S3NameValue *metaData;

    /**
     * This a boolean value indicating whether or not the object should be
     * stored by Amazon S3 using server-side encryption, wherein the data is
     * encrypted by Amazon before being stored on permanent medium.
     * Server-side encryption does not affect the data as it is sent to or
     * received by Amazon, the encryption is applied by Amazon when objects
     * are put and then de-encryption is applied when the objects are read by
     * clients.
     * If this value is 0, then server-side encryption is not used; if this
     * value is non-zero, then server-side encryption is used.  Note that the
     * encryption status of the object can be checked by ensuring that the put
     * response has the usesServerSideEncryption flag set.
     **/
    char useServerSideEncryption;
} S3PutProperties;


/**
 * S3GetConditions is used for the get_object operation, and specifies
 * conditions which the object must meet in order to be successfully returned.
 **/
typedef struct S3GetConditions
{
    /**
     * The request will be processed if the Last-Modification header of the
     * object is greater than or equal to this value, specified as a number of
     * seconds since Unix epoch.  If this value is less than zero, it will not
     * be used in the conditional.
     **/
    int64_t ifModifiedSince;

    /**
     * The request will be processed if the Last-Modification header of the
     * object is less than this value, specified as a number of seconds since
     * Unix epoch.  If this value is less than zero, it will not be used in
     * the conditional.
     **/
    int64_t ifNotModifiedSince;

    /**
     * If non-NULL, this gives an eTag header value which the object must
     * match in order to be returned.  Note that altough the eTag is simply an
     * MD5, this must be presented in the S3 eTag form, which typically
     * includes double-quotes.
     **/
    const char *ifMatchETag;

    /**
     * If non-NULL, this gives an eTag header value which the object must not
     * match in order to be returned.  Note that altough the eTag is simply an
     * MD5, this must be presented in the S3 eTag form, which typically
     * includes double-quotes.
     **/
    const char *ifNotMatchETag;
} S3GetConditions;


/**
 * S3ErrorDetails provides detailed information describing an S3 error.  This
 * is only presented when the error is an S3-generated error (i.e. one of the
 * S3StatusErrorXXX values).
 **/
typedef struct S3ErrorDetails
{
    /**
     * This is the human-readable message that Amazon supplied describing the
     * error
     **/
    const char *message;

    /**
     * This identifies the resource for which the error occurred
     **/
    const char *resource;

    /**
     * This gives human-readable further details describing the specifics of
     * this error
     **/
    const char *furtherDetails;

    /**
     * This gives the number of S3NameValue pairs present in the extraDetails
     * array
     **/
    int extraDetailsCount;

    /**
     * S3 can provide extra details in a freeform Name - Value pair format.
     * Each error can have any number of these, and this array provides these
     * additional extra details.
     **/
    S3NameValue *extraDetails;
} S3ErrorDetails;


/** **************************************************************************
 * Callback Signatures
 ************************************************************************** **/

/**
 * This callback is made whenever the response properties become available for
 * any request.
 *
 * @param properties are the properties that are available from the response
 * @param callbackData is the callback data as specified when the request
 *        was issued.
 * @return S3StatusOK to continue processing the request, anything else to
 *         immediately abort the request with a status which will be
 *         passed to the S3ResponseCompleteCallback for this request.
 *         Typically, this will return either S3StatusOK or
 *         S3StatusAbortedByCallback.
 **/
typedef S3Status (S3ResponsePropertiesCallback)
    (const S3ResponseProperties *properties, void *callbackData);


/**
 * This callback is made when the response has been completely received, or an
 * error has occurred which has prematurely aborted the request, or one of the
 * other user-supplied callbacks returned a value intended to abort the
 * request.  This callback is always made for every request, as the very last
 * callback made for that request.
 *
 * @param status gives the overall status of the response, indicating success
 *        or failure; use S3_status_is_retryable() as a simple way to detect
 *        whether or not the status indicates that the request failed but may
 *        be retried.
 * @param errorDetails if non-NULL, gives details as returned by the S3
 *        service, describing the error
 * @param callbackData is the callback data as specified when the request
 *        was issued.
 **/
typedef void (S3ResponseCompleteCallback)(S3Status status,
                                          const S3ErrorDetails *errorDetails,
                                          void *callbackData);


/**
 * This callback is made for each bucket resulting from a list service
 * operation.
 *
 * @param ownerId is the ID of the owner of the bucket
 * @param ownerDisplayName is the owner display name of the owner of the bucket
 * @param bucketName is the name of the bucket
 * @param creationDateSeconds if < 0 indicates that no creation date was
 *        supplied for the bucket; if >= 0 indicates the number of seconds
 *        since UNIX Epoch of the creation date of the bucket
 * @param callbackData is the callback data as specified when the request
 *        was issued.
 * @return S3StatusOK to continue processing the request, anything else to
 *         immediately abort the request with a status which will be
 *         passed to the S3ResponseCompleteCallback for this request.
 *         Typically, this will return either S3StatusOK or
 *         S3StatusAbortedByCallback.
 **/
typedef S3Status (S3ListServiceCallback)(const char *ownerId,
                                         const char *ownerDisplayName,
                                         const char *bucketName,
                                         int64_t creationDateSeconds,
                                         void *callbackData);


/**
 * This callback is made repeatedly as a list bucket operation progresses.
 * The contents reported via this callback are only reported once per list
 * bucket operation, but multiple calls to this callback may be necessary to
 * report all items resulting from the list bucket operation.
 *
 * @param isTruncated is true if the list bucket request was truncated by the
 *        S3 service, in which case the remainder of the list may be obtained
 *        by querying again using the Marker parameter to start the query
 *        after this set of results
 * @param nextMarker if present, gives the largest (alphabetically) key
 *        returned in the response, which, if isTruncated is true, may be used
 *        as the marker in a subsequent list buckets operation to continue
 *        listing
 * @param contentsCount is the number of ListBucketContent structures in the
 *        contents parameter
 * @param contents is an array of ListBucketContent structures, each one
 *        describing an object in the bucket
 * @param commonPrefixesCount is the number of common prefixes strings in the
 *        commonPrefixes parameter
 * @param commonPrefixes is an array of strings, each specifing one of the
 *        common prefixes as returned by S3
 * @param callbackData is the callback data as specified when the request
 *        was issued.
 * @return S3StatusOK to continue processing the request, anything else to
 *         immediately abort the request with a status which will be
 *         passed to the S3ResponseCompleteCallback for this request.
 *         Typically, this will return either S3StatusOK or
 *         S3StatusAbortedByCallback.
 **/
typedef S3Status (S3ListBucketCallback)(int isTruncated,
                                        const char *nextMarker,
                                        int contentsCount,
                                        const S3ListBucketContent *contents,
                                        int commonPrefixesCount,
                                        const char **commonPrefixes,
                                        void *callbackData);


/**
 * This callback is made during a put object operation, to obtain the next
 * chunk of data to put to the S3 service as the contents of the object.  This
 * callback is made repeatedly, each time acquiring the next chunk of data to
 * write to the service, until a negative or 0 value is returned.
 *
 * @param bufferSize gives the maximum number of bytes that may be written
 *        into the buffer parameter by this callback
 * @param buffer gives the buffer to fill with at most bufferSize bytes of
 *        data as the next chunk of data to send to S3 as the contents of this
 *        object
 * @param callbackData is the callback data as specified when the request
 *        was issued.
 * @return < 0 to abort the request with the S3StatusAbortedByCallback, which
 *        will be pased to the response complete callback for this request, or
 *        0 to indicate the end of data, or > 0 to identify the number of
 *        bytes that were written into the buffer by this callback
 **/
typedef int (S3PutObjectDataCallback)(int bufferSize, char *buffer,
                                      void *callbackData);


/**
 * This callback is made during a get object operation, to provide the next
 * chunk of data available from the S3 service constituting the contents of
 * the object being fetched.  This callback is made repeatedly, each time
 * providing the next chunk of data read, until the complete object contents
 * have been passed through the callback in this way, or the callback
 * returns an error status.
 *
 * @param bufferSize gives the number of bytes in buffer
 * @param buffer is the data being passed into the callback
 * @param callbackData is the callback data as specified when the request
 *        was issued.
 * @return S3StatusOK to continue processing the request, anything else to
 *         immediately abort the request with a status which will be
 *         passed to the S3ResponseCompleteCallback for this request.
 *         Typically, this will return either S3StatusOK or
 *         S3StatusAbortedByCallback.
 **/
typedef S3Status (S3GetObjectDataCallback)(int bufferSize, const char *buffer,
                                           void *callbackData);


/**
 * This callback is made after initiation of a multipart upload operation.  It
 * indicates that the multi part upload has been created and provides the
 * id that can be used to associate multi upload parts with the multi upload
 * operation
 *
 * @param upload_id is the unique identifier if this multi part upload
 *        operation, to be used in calls to S3_upload_part and
 *        S3_complete_multipart_upload
 * @param callbackData is the callback data as specified when the request
 *        was issued.
 * @return S3StatusOK to continue processing the request, anything else to
 *         immediately abort the request with a status which will be
 *         passed to the S3ResponseCompleteCallback for this request.
 *         Typically, this will return either S3StatusOK or
 *         S3StatusAbortedByCallback.
 **/
typedef S3Status (S3MultipartInitialResponseCallback)(const char *upload_id,
                                                      void *callbackData);


/**
 * This callback is made after completion of a part of a multipart upload
 * operation.  It is a status callback indicating that the multipart upload is
 * in progress and that some sub-set of the parts have completed upload.  The
 * multipart upload is not considered fully complete in total until the commit
 * response callback is made.
 *
 * @param isTruncated is ??? someone document this please
 * @param nextKeyMarker is ??? someone document this please
 * @param nextUploadIdMarker is ??? someone document this please
 * @param uploadsCount is the number of elements in the [uploads] array
 * @param uploads is an array of ??? someone document this please
 * @param commonPrefixesCount is the number of elements in the
 *        [commonPrefixes] array
 * @param commonPrefixes is ??? someone document this please
 * @param callbackData is the callback data as specified when the request
 *        was issued.
 * @return S3StatusOK to continue processing the request, anything else to
 *         immediately abort the request with a status which will be
 *         passed to the S3ResponseCompleteCallback for this request.
 *         Typically, this will return either S3StatusOK or
 *         S3StatusAbortedByCallback.
 **/
typedef S3Status (S3ListMultipartUploadsResponseCallback)
    (int isTruncated, const char *nextKeyMarker,
     const char *nextUploadIdMarker, int uploadsCount,
     const S3ListMultipartUpload *uploads, int commonPrefixesCount,
     const char **commonPrefixes, void *callbackData);


/**
 * This callback is made with the result of a succesful "list parts" request
 * to list the parts of a multi part upload (in progress???).
 *
 * @param isTruncated is ??? someone document this please
 * @param nextPartNumberMarker is ??? someone document this please
 * @param intiatorId is ??? someone document this please
 * @param initiatorDisplayName is ??? someone document this please
 * @param ownerId is ??? someone document this please
 * @param ownerDisplayName is ??? someone document this please
 * @param storageClass is ??? someone document this please
 * @param partsCount is ??? someone document this please
 * @param lastPartNumber is ??? someone document this please
 * @param parts is ??? someone document this please
 * @param callbackData is the callback data as specified when the request
 *        was issued.
 * @return S3StatusOK to continue processing the request, anything else to
 *         immediately abort the request with a status which will be
 *         passed to the S3ResponseCompleteCallback for this request.
 *         Typically, this will return either S3StatusOK or
 *         S3StatusAbortedByCallback.
 **/
typedef S3Status (S3ListPartsResponseCallback)
    (int isTruncated, const char *nextPartNumberMarker,
     const char *initiatorId, const char *initiatorDisplayName,
     const char *ownerId, const char *ownerDisplayName,
     const char *storageClass, int partsCount, int lastPartNumber,
     const S3ListPart *parts, void *callbackData);


/**
 * This callback is made after commit of a multipart upload operation.  It
 * indicates that the data uploaded via the multipart upload operation has
 * been committed.
 *
 * @param location is ??? someone please document this
 * @param etag is the S3 etag of the complete object after the multipart
 *        upload
 * @param callbackData is the callback data as specified when the request
 *        was issued.
 * @return S3StatusOK to continue processing the request, anything else to
 *         immediately abort the request with a status which will be
 *         passed to the S3ResponseCompleteCallback for this request.
 *         Typically, this will return either S3StatusOK or
 *         S3StatusAbortedByCallback.
 **/
typedef S3Status (S3MultipartCommitResponseCallback)(const char *location,
                                                     const char *etag,
                                                     void *callbackData);


/**
 * Mechanism for S3 application to customize each CURL easy request
 * associated with the given S3 request context.
 *
 * This callback can be optinally configured using S3_create_request_context_ex
 * and will be invoked every time a new CURL request is created in the
 * context of the given CURLM handle. Invocation will occur after
 * libs3 has finished configuring its own options of CURL, but before
 * CURL is started.
 *
 * @param curl_multi is the CURLM handle associated with this context.
 * @param curl_easy is the CURL request being created.
 * @param setupData is the setupCurlCallbackData parameter passed to
 *        S3_create_request_context_ex.
 * @return S3StatusOK to continue processing the request, anything else to
 *         immediately abort the request and pass this status
 *         to the S3ResponseCompleteCallback for this request.
 **/
typedef S3Status (*S3SetupCurlCallback)(void *curlMulti, void *curlEasy,
                                        void *setupData);


/** **************************************************************************
 * Callback Structures
 ************************************************************************** **/


/**
 * An S3ResponseHandler defines the callbacks which are made for any
 * request.
 **/
typedef struct S3ResponseHandler
{
    /**
     * The propertiesCallback is made when the response properties have
     * successfully been returned from S3.  This function may not be called
     * if the response properties were not successfully returned from S3.
     **/
    S3ResponsePropertiesCallback *propertiesCallback;

    /**
     * The completeCallback is always called for every request made to S3,
     * regardless of the outcome of the request.  It provides the status of
     * the request upon its completion, as well as extra error details in the
     * event of an S3 error.
     **/
    S3ResponseCompleteCallback *completeCallback;
} S3ResponseHandler;


/**
 * An S3ListServiceHandler defines the callbacks which are made for
 * list_service requests.
 **/
typedef struct S3ListServiceHandler
{
    /**
     * responseHandler provides the properties and complete callback
     **/
    S3ResponseHandler responseHandler;

    /**
     * The listServiceCallback is called as items are reported back from S3 as
     * responses to the request
     **/
    S3ListServiceCallback *listServiceCallback;
} S3ListServiceHandler;


/**
 * An S3ListBucketHandler defines the callbacks which are made for
 * list_bucket requests.
 **/
typedef struct S3ListBucketHandler
{
    /**
     * responseHandler provides the properties and complete callback
     **/
    S3ResponseHandler responseHandler;

    /**
     * The listBucketCallback is called as items are reported back from S3 as
     * responses to the request.  This may be called more than one time per
     * list bucket request, each time providing more items from the list
     * operation.
     **/
    S3ListBucketCallback *listBucketCallback;
} S3ListBucketHandler;


/**
 * An S3PutObjectHandler defines the callbacks which are made for
 * put_object requests.
 **/
typedef struct S3PutObjectHandler
{
    /**
     * responseHandler provides the properties and complete callback
     **/
    S3ResponseHandler responseHandler;

    /**
     * The putObjectDataCallback is called to acquire data to send to S3 as
     * the contents of the put_object request.  It is made repeatedly until it
     * returns a negative number (indicating that the request should be
     * aborted), or 0 (indicating that all data has been supplied).
     **/
    S3PutObjectDataCallback *putObjectDataCallback;
} S3PutObjectHandler;


/**
 * An S3GetObjectHandler defines the callbacks which are made for
 * get_object requests.
 **/
typedef struct S3GetObjectHandler
{
    /**
     * responseHandler provides the properties and complete callback
     **/
    S3ResponseHandler responseHandler;

    /**
     * The getObjectDataCallback is called as data is read from S3 as the
     * contents of the object being read in the get_object request.  It is
     * called repeatedly until there is no more data provided in the request,
     * or until the callback returns an error status indicating that the
     * request should be aborted.
     **/
    S3GetObjectDataCallback *getObjectDataCallback;
} S3GetObjectHandler;


typedef struct S3MultipartInitialHandler {
    /**
     * responseHandler provides the properties and complete callback
     **/
    S3ResponseHandler responseHandler;

    S3MultipartInitialResponseCallback *responseXmlCallback;
} S3MultipartInitialHandler;

typedef struct S3MultipartCommitHandler
{
    /**
     * responseHandler provides the properties and complete callback
     **/
    S3ResponseHandler responseHandler;

    /**
     * The putObjectDataCallback is called to acquire data to send to S3 as
     * the contents of the put_object request.  It is made repeatedly until it
     * returns a negative number (indicating that the request should be
     * aborted), or 0 (indicating that all data has been supplied).
     **/
    S3PutObjectDataCallback *putObjectDataCallback;
    S3MultipartCommitResponseCallback *responseXmlCallback;
} S3MultipartCommitHandler;

typedef struct S3ListMultipartUploadsHandler
{
    /**
     * responseHandler provides the properties and complete callback
     **/
    S3ResponseHandler responseHandler;

    S3ListMultipartUploadsResponseCallback *responseXmlCallback;
} S3ListMultipartUploadsHandler;

typedef struct S3ListPartsHandler
{
    /**
     * responseHandler provides the properties and complete callback
     **/
    S3ResponseHandler responseHandler;

    S3ListPartsResponseCallback *responseXmlCallback;
} S3ListPartsHandler;

typedef struct S3AbortMultipartUploadHandler
{
    /**
     * responseHandler provides the properties and complete callback
     **/
    S3ResponseHandler responseHandler;

} S3AbortMultipartUploadHandler;

/** **************************************************************************
 * General Library Functions
 ************************************************************************** **/

/**
 * Initializes libs3 for use.  This function must be called before any other
 * libs3 function is called.  It may be called multiple times, with the same
 * effect as calling it once, as long as S3_deinitialize() is called an
 * equal number of times when the program has finished.  This function is NOT
 * thread-safe and must only be called by one thread at a time.
 *
 * @param userAgentInfo is a string that will be included in the User-Agent
 *        header of every request made to the S3 service.  You may provide
 *        NULL or the empty string if you don't care about this.  The value
 *        will not be copied by this function and must remain unaltered by the
 *        caller until S3_deinitialize() is called.
 * @param flags is a bitmask of some combination of S3_INIT_XXX flag, or
 *        S3_INIT_ALL, indicating which of the libraries that libs3 depends
 *        upon should be initialized by S3_initialize().  Only if your program
 *        initializes one of these dependency libraries itself should anything
 *        other than S3_INIT_ALL be passed in for this bitmask.
 *
 *        You should pass S3_INIT_WINSOCK if and only if your application does
 *        not initialize winsock elsewhere.  On non-Microsoft Windows
 *        platforms it has no effect.
 *
 *        As a convenience, the macro S3_INIT_ALL is provided, which will do
 *        all necessary initialization; however, be warned that things may
 *        break if your application re-initializes the dependent libraries
 *        later.
 * @param defaultS3HostName is a string the specifies the default S3 server
 *        hostname to use when making S3 requests; this value is used
 *        whenever the hostName of an S3BucketContext is NULL.  If NULL is
 *        passed here then the default of S3_DEFAULT_HOSTNAME will be used.
 * @return One of:
 *         S3StatusOK on success
 *         S3StatusUriTooLong if the defaultS3HostName is longer than
 *             S3_MAX_HOSTNAME_SIZE
 *         S3StatusInternalError if dependent libraries could not be
 *             initialized
 *         S3StatusOutOfMemory on failure due to out of memory
 **/
S3Status S3_initialize(const char *userAgentInfo, int flags,
                       const char *defaultS3HostName);


/**
 * Must be called once per program for each call to libs3_initialize().  After
 * this call is complete, no libs3 function may be called except
 * S3_initialize().
 **/
void S3_deinitialize();


/**
 * Returns a string with the textual name of an S3Status code
 *
 * @param status is S3Status code for which the textual name will be returned
 * @return a string with the textual name of an S3Status code
 **/
const char *S3_get_status_name(S3Status status);


/**
 * This function may be used to validate an S3 bucket name as being in the
 * correct form for use with the S3 service.  Amazon S3 limits the allowed
 * characters in S3 bucket names, as well as imposing some additional rules on
 * the length of bucket names and their structure.  There are actually two
 * limits; one for bucket names used only in path-style URIs, and a more
 * strict limit used for bucket names used in virtual-host-style URIs.  It is
 * advisable to use only bucket names which meet the more strict requirements
 * regardless of how the bucket expected to be used.
 *
 * This method does NOT validate that the bucket is available for use in the
 * S3 service, so the return value of this function cannot be used to decide
 * whether or not a bucket with the give name already exists in Amazon S3 or
 * is accessible by the caller.  It merely validates that the bucket name is
 * valid for use with S3.
 *
 * @param bucketName is the bucket name to validate
 * @param uriStyle gives the URI style to validate the bucket name against.
 *        It is advisable to always use S3UriStyleVirtuallHost.
 * @return One of:
 *         S3StatusOK if the bucket name was validates successfully
 *         S3StatusConnectionFailed if the socket connection to the server
 *             failed
 *         S3StatusServerFailedVerification if the SSL certificate of the
 *             server could not be verified.
 *         S3StatusInvalidBucketNameTooLong if the bucket name exceeded the
 *             length limitation for the URI style, which is 255 bytes for
 *             path style URIs and 63 bytes for virtual host type URIs
 *         S3StatusInvalidBucketNameTooShort if the bucket name is less than
 *             3 characters
 *         S3StatusInvalidBucketNameFirstCharacter if the bucket name as an
 *             invalid first character, which is anything other than
 *             an alphanumeric character
 *         S3StatusInvalidBucketNameCharacterSequence if the bucket name
 *             includes an invalid character sequence, which for virtual host
 *             style buckets is ".-" or "-."
 *         S3StatusInvalidBucketNameCharacter if the bucket name includes an
 *             invalid character, which is anything other than alphanumeric,
 *             '-', '.', or for path style URIs only, '_'.
 *         S3StatusInvalidBucketNameDotQuadNotation if the bucket name is in
 *             dot-quad notation, i.e. the form of an IP address, which is
 *             not allowed by Amazon S3.
 **/
S3Status S3_validate_bucket_name(const char *bucketName, S3UriStyle uriStyle);


/**
 * Converts an XML representation of an ACL to a libs3 structured
 * representation.  This method is not strictly necessary for working with
 * ACLs using libs3, but may be convenient for users of the library who read
 * ACLs from elsewhere in XML format and need to use these ACLs with libs3.
 *
 * @param aclXml is the XML representation of the ACL.  This must be a
 *        zero-terminated character string.
 * @param ownerId will be filled in with the Owner ID specified in the XML.
 *        At most MAX_GRANTEE_USER_ID_SIZE bytes will be stored at this
 *        location.
 * @param ownerDisplayName will be filled in with the Owner Display Name
 *        specified in the XML.  At most MAX_GRANTEE_DISPLAY_NAME_SIZE bytes
 *        will be stored at this location.
 * @param aclGrantCountReturn returns the number of S3AclGrant structures
 *        returned in the aclGrantsReturned array
 * @param aclGrants must be passed in as an array of at least S3_ACL_MAXCOUNT
 *        structures, and on return from this function, the first
 *        aclGrantCountReturn structures will be filled in with the ACLs
 *        represented by the input XML.
 * @return One of:
 *         S3StatusOK on successful conversion of the ACL
 *         S3StatusInternalError on internal error representing a bug in the
 *             libs3 library
 *         S3StatusXmlParseFailure if the XML document was malformed
 **/
S3Status S3_convert_acl(char *aclXml, char *ownerId, char *ownerDisplayName,
                        int *aclGrantCountReturn, S3AclGrant *aclGrants);


/**
 * Returns nonzero if the status indicates that the request should be
 * immediately retried, because the status indicates an error of a nature that
 * is likely due to transient conditions on the local system or S3, such as
 * network failures, or internal retryable errors reported by S3.  Returns
 * zero otherwise.
 *
 * @param status is the status to evaluate
 * @return nonzero if the status indicates a retryable error, 0 otherwise
 **/
int S3_status_is_retryable(S3Status status);


/** **************************************************************************
 * Request Context Management Functions
 ************************************************************************** **/

/**
 * An S3RequestContext allows muliple requests to be serviced by the same
 * thread simultaneously.  It is an optional parameter to all libs3 request
 * functions, and if provided, the request is managed by the S3RequestContext;
 * if not, the request is handled synchronously and is complete when the libs3
 * request function has returned.
 *
 * @param requestContextReturn returns the newly-created S3RequestContext
 *        structure, which if successfully returned, must be destroyed via a
 *        call to S3_destroy_request_context when it is no longer needed.  If
 *        an error status is returned from this function, then
 *        requestContextReturn will not have been filled in, and
 *        S3_destroy_request_context should not be called on it
 * @return One of:
 *         S3StatusOK if the request context was successfully created
 *         S3StatusOutOfMemory if the request context could not be created due
 *             to an out of memory error
 **/
S3Status S3_create_request_context(S3RequestContext **requestContextReturn);


/**
 * Extended version of S3_create_request_context used to create S3RequestContext
 * for curl_multi_socket_action CURLM handles that will be managed by libs3 user.
 * This type of handles offer better performance for applications with large
 * number of simultaneous connections. For details, see MULTI_SOCKET chapter here:
 * https://curl.se/libcurl/c/libcurl-multi.html
 *
 * In this mode libs3 user will
 *  - create its own CURLM using curl_multi_init()
 *  - configure it for its own handlers using
 *    CURLMOPT_SOCKETFUNCTION/CURLMOPT_TIMERFUNCTION/etc
 *  - use S3_create_request_context_ex to create S3RequestContext
 *    for the above CURLM handle
 *  - start S3 request
 *  - every time setupCurlCallback is called, will configure new CURL
 *    object with its own handlers using
 *    CURLOPT_OPENSOCKETFUNCTION/CURLOPT_CLOSESOCKETFUNCTION/etc
 *  - the moment libs3 adds CURL object to CURLM handle, curl will start
 *    communicating directly with libs3 user to drive socket operations,
 *    where libs3 user will be responsible for calling curl_multi_socket_action
 *    when necessary.
 *  - whenever curl_multi_socket_action indicates change in running_handles
 *    libs3 user should call S3_process_request_context to let libs3 process
 *    any completed curl transfers and notify back to libs3 user if that was
 *    the final transfer for a given S3 request.
 *
 * @param requestContextReturn returns the newly-created S3RequestContext
 *        structure, which if successfully returned, must be destroyed via a
 *        call to S3_destroy_request_context when it is no longer needed.  If
 *        an error status is returned from this function, then
 *        requestContextReturn will not have been filled in, and
 *        S3_destroy_request_context should not be called on it
 * @param curlMulti is the CURLM handle to be associated with this context.
 * @param setupCurlCallback is an optional callback routine to be invoked
 *        by libs3 every time another CURL request is being created for
 *        use in this context.
 * @param setupCurlCallbackData is an opaque data to be passed to
 *        setupCurlCallback.
 **/
S3Status S3_create_request_context_ex(S3RequestContext **requestContextReturn,
                                      void *curlMulti,
                                      S3SetupCurlCallback setupCurlCallback,
                                      void *setupCurlCallbackData);


/**
 * Destroys an S3RequestContext which was created with
 * S3_create_request_context.  Any requests which are currently being
 * processed by the S3RequestContext will immediately be aborted and their
 * request completed callbacks made with the status S3StatusInterrupted.
 *
 * @param requestContext is the S3RequestContext to destroy
 **/
void S3_destroy_request_context(S3RequestContext *requestContext);


#ifdef WINSCP
struct ne_session_s;
typedef void (S3SessionCallback)(ne_session_s *session, void *callbackData);
void S3_set_request_context_session_callback(S3RequestContext *requestContext,
                                             S3SessionCallback sessionCallback,
                                             void * sessionCallbackData);

struct ne_ssl_certificate_s;
typedef int (S3SslCallback)(int failures, const ne_ssl_certificate_s *certificate, void *callbackData);
void S3_set_request_context_ssl_callback(S3RequestContext *requestContext,
                                         S3SslCallback sslCallback,
                                         void * sslCallbackData);

typedef void (S3ResponseDataCallback)(const char *data, size_t size, void *callbackData);
void S3_set_request_context_response_data_callback(S3RequestContext *requestContext,
                                                   S3ResponseDataCallback responseDataCallback,
                                                   void * responseDataCallbackData);

void S3_set_request_context_requester_pays(S3RequestContext *requestContext, int requesterPays);
#else
/**
 * Runs the S3RequestContext until all requests within it have completed,
 * or until an error occurs.
 *
 * @param requestContext is the S3RequestContext to run until all requests
 *            within it have completed or until an error occurs
 * @return One of:
 *         S3StatusOK if all requests were successfully run to completion
 *         S3StatusConnectionFailed if the socket connection to the server
 *             failed
 *         S3StatusServerFailedVerification if the SSL certificate of the
 *             server could not be verified.
 *         S3StatusInternalError if an internal error prevented the
 *             S3RequestContext from running one or more requests
 *         S3StatusOutOfMemory if requests could not be run to completion
 *             due to an out of memory error
 **/
S3Status S3_runall_request_context(S3RequestContext *requestContext);


/**
 * Does some processing of requests within the S3RequestContext.  One or more
 * requests may have callbacks made on them and may complete.  This function
 * processes any requests which have immediately available I/O, and will not
 * block waiting for I/O on any request.  This function would normally be used
 * with S3_get_request_context_fdsets.
 *
 * @param requestContext is the S3RequestContext to process
 * @param requestsRemainingReturn returns the number of requests remaining
 *            and not yet completed within the S3RequestContext after this
 *            function returns.
 * @return One of:
 *         S3StatusOK if request processing proceeded without error
 *         S3StatusConnectionFailed if the socket connection to the server
 *             failed
 *         S3StatusServerFailedVerification if the SSL certificate of the
 *             server could not be verified.
 *         S3StatusInternalError if an internal error prevented the
 *             S3RequestContext from running one or more requests
 *         S3StatusOutOfMemory if requests could not be processed due to
 *             an out of memory error
 **/
S3Status S3_runonce_request_context(S3RequestContext *requestContext,
                                    int *requestsRemainingReturn);


/**
 * Extract and finish requests completed by curl multi handle mechanism
 * in curl_multi_socket_action mode. Should be called by libs3 user when
 * curl_multi_socket_action indicates a change in running_handles.
 *
 * @param requestContext is the S3RequestContext to process
 * @return One of:
 *         S3StatusOK if request processing proceeded without error
 *         S3StatusConnectionFailed if the socket connection to the server
 *             failed
 *         S3StatusServerFailedVerification if the SSL certificate of the
 *             server could not be verified.
 *         S3StatusInternalError if an internal error prevented the
 *             S3RequestContext from running one or more requests
 *         S3StatusOutOfMemory if requests could not be processed due to
 *             an out of memory error
 **/
S3Status S3_process_request_context(S3RequestContext *requestContext);


/**
 * This function, in conjunction allows callers to manually manage a set of
 * requests using an S3RequestContext.  This function returns the set of file
 * descriptors which the caller can watch (typically using select()), along
 * with any other file descriptors of interest to the caller, and using
 * whatever timeout (if any) the caller wishes, until one or more file
 * descriptors in the returned sets become ready for I/O, at which point
 * S3_runonce_request_context can be called to process requests with available
 * I/O.
 *
 * @param requestContext is the S3RequestContext to get fd_sets from
 * @param readFdSet is a pointer to an fd_set which will have all file
 *        descriptors to watch for read events for the requests in the
 *        S3RequestContext set into it upon return.  Should be zero'd out
 *        (using FD_ZERO) before being passed into this function.
 * @param writeFdSet is a pointer to an fd_set which will have all file
 *        descriptors to watch for write events for the requests in the
 *        S3RequestContext set into it upon return.  Should be zero'd out
 *        (using FD_ZERO) before being passed into this function.
 * @param exceptFdSet is a pointer to an fd_set which will have all file
 *        descriptors to watch for exception events for the requests in the
 *        S3RequestContext set into it upon return.  Should be zero'd out
 *        (using FD_ZERO) before being passed into this function.
 * @param maxFd returns the highest file descriptor set into any of the
 *        fd_sets, or -1 if no file descriptors were set
 * @return One of:
 *         S3StatusOK if all fd_sets were successfully set
 *         S3StatusInternalError if an internal error prevented this function
 *             from completing successfully
 **/
S3Status S3_get_request_context_fdsets(S3RequestContext *requestContext,
                                       fd_set *readFdSet, fd_set *writeFdSet,
                                       fd_set *exceptFdSet, int *maxFd);


/**
 * This function returns the maximum number of milliseconds that the caller of
 * S3_runonce_request_context should wait on the fdsets obtained via a call to
 * S3_get_request_context_fdsets.  In other words, this is essentially the
 * select() timeout that needs to be used (shorter values are OK, but no
 * longer than this) to ensure that internal timeout code of libs3 can work
 * properly.  This function should be called right before select() each time
 * select() on the request_context fdsets are to be performed by the libs3
 * user.
 *
 * @param requestContext is the S3RequestContext to get the timeout from
 * @return the maximum number of milliseconds to select() on fdsets.  Callers
 *         could wait a shorter time if they wish, but not longer.
 **/
int64_t S3_get_request_context_timeout(S3RequestContext *requestContext);

/**
 * This function enables SSL peer certificate verification on a per-request
 * context basis. If this is called, the context's value of verifyPeer will
 * be used when processing requests. Otherwise, the default set by the
 * flags to S3_initialize() are used.
 *
 * @param requestContext the S3RequestContext to set the verifyPeer flag on.
 * @param verifyPeer a boolean value indicating whether to verify the peer
 *        certificate or not.
 */
void S3_set_request_context_verify_peer(S3RequestContext *requestContext,
                                        int verifyPeer);
#endif


/** **************************************************************************
 * S3 Utility Functions
 ************************************************************************** **/

#ifndef WINSCP
/**
 * Generates an HTTP authenticated query string, which may then be used by
 * a browser (or other web client) to issue the request.  The request is
 * implicitly a GET request; Amazon S3 is documented to only support this type
 * of authenticated query string request.
 *
 * @param buffer is the output buffer for the authenticated query string.
 *        It must be at least S3_MAX_AUTHENTICATED_QUERY_STRING_SIZE bytes in
 *        length.
 * @param bucketContext gives the bucket and associated parameters for the
 *        request to generate.
 * @param key gives the key which the authenticated request will GET.
 * @param expires gives the number of seconds since Unix epoch for the
 *        expiration date of the request; after this time, the request will
 *        no longer be valid.  If this value is negative, the largest
 *        expiration interval possible is used (one week).
 * @param resource gives a sub-resource to be fetched for the request, or NULL
 *        for none.  This should be of the form "?<resource>", i.e.
 *        "?torrent".
 * @param httpMethod the HTTP request method that will be used with the
 *        generated query string (e.g. "GET").
 * @return One of:
 *         S3StatusUriTooLong if, due to an internal error, the generated URI
 *             is longer than S3_MAX_AUTHENTICATED_QUERY_STRING_SIZE bytes in
 *             length and thus will not fit into the supplied buffer
 *         S3StatusOK on success
 **/
S3Status S3_generate_authenticated_query_string
    (char *buffer, const S3BucketContext *bucketContext,
     const char *key, int expires, const char *resource,
     const char *httpMethod);
#endif


/** **************************************************************************
 * Service Functions
 ************************************************************************** **/

/**
 * Lists all S3 buckets belonging to the access key id.
 *
 * @param protocol gives the protocol to use for this request
 * @param accessKeyId gives the Amazon Access Key ID for which to list owned
 *        buckets
 * @param secretAccessKey gives the Amazon Secret Access Key for which to list
 *        owned buckets
 * @param securityToken gives the security token used to generate the Temporary
 *        Security Credentials
 * @param hostName is the S3 host name to use; if NULL is passed in, the
 *        default S3 host as provided to S3_initialize() will be used.
 * @param authRegion is the AWS region to use for the authorization signature
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_list_service(S3Protocol protocol, const char *accessKeyId,
                     const char *secretAccessKey, const char *securityToken,
                     const char *hostName, const char *authRegion,
                     int maxkeys, // WINSCP
                     S3RequestContext *requestContext,
                     int timeoutMs,
                     const S3ListServiceHandler *handler, void *callbackData);


/** **************************************************************************
 * Bucket Functions
 ************************************************************************** **/
/**
 * Tests the existence of an S3 bucket, additionally returning the bucket's
 * location if it exists and is accessible.
 *
 * @param protocol gives the protocol to use for this request
 * @param uriStyle gives the URI style to use for this request
 * @param accessKeyId gives the Amazon Access Key ID for which to list owned
 *        buckets
 * @param secretAccessKey gives the Amazon Secret Access Key for which to list
 *        owned buckets
 * @param securityToken gives the security token used to generate the Temporary
 *        Security Credentials
 * @param hostName is the S3 host name to use; if NULL is passed in, the
 *        default S3 host as provided to S3_initialize() will be used.
 * @param bucketName is the bucket name to test
 * @param authRegion is the AWS region to use for the authorization signature
 * @param locationConstraintReturnSize gives the number of bytes in the
 *        locationConstraintReturn parameter
 * @param locationConstraintReturn provides the location into which to write
 *        the name of the location constraint naming the geographic location
 *        of the S3 bucket.  This must have at least as many characters in it
 *        as specified by locationConstraintReturn, and should start out
 *        NULL-terminated.  On successful completion of this request, this
 *        will be set to the name of the geographic location of S3 bucket, or
 *        will be left as a zero-length string if no location was available.
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_test_bucket(S3Protocol protocol, S3UriStyle uriStyle,
                    const char *accessKeyId, const char *secretAccessKey,
                    const char *securityToken, const char *hostName,
                    const char *bucketName, const char *authRegion,
                    int locationConstraintReturnSize,
                    char *locationConstraintReturn,
                    S3RequestContext *requestContext,
                    int timeoutMs,
                    const S3ResponseHandler *handler, void *callbackData);


/**
 * Creates a new bucket.
 *
 * @param protocol gives the protocol to use for this request
 * @param accessKeyId gives the Amazon Access Key ID for which to list owned
 *        buckets
 * @param secretAccessKey gives the Amazon Secret Access Key for which to list
 *        owned buckets
 * @param securityToken gives the security token used to generate the Temporary
 *        Security Credentials
 * @param hostName is the S3 host name to use; if NULL is passed in, the
 *        default S3 host as provided to S3_initialize() will be used.
 * @param bucketName is the name of the bucket to be created
 * @param authRegion is the AWS region to use for the authorization signature
 * @param cannedAcl gives the "REST canned ACL" to use for the created bucket
 * @param locationConstraint if non-NULL, gives the geographic location for
 *        the bucket to create.
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_create_bucket(S3Protocol protocol, const char *accessKeyId,
                      const char *secretAccessKey, const char *securityToken,
                      const char *hostName, const char *bucketName,
                      const char *authRegion, S3CannedAcl cannedAcl,
                      const char *locationConstraint,
                      S3RequestContext *requestContext,
                      int timeoutMs,
                      const S3ResponseHandler *handler, void *callbackData);


/**
 * Deletes a bucket.  The bucket must be empty, or the status
 * S3StatusErrorBucketNotEmpty will result.
 *
 * @param protocol gives the protocol to use for this request
 * @param uriStyle gives the URI style to use for this request
 * @param accessKeyId gives the Amazon Access Key ID for which to list owned
 *        buckets
 * @param secretAccessKey gives the Amazon Secret Access Key for which to list
 *        owned buckets
 * @param securityToken gives the security token used to generate the Temporary
 *        Security Credentials
 * @param hostName is the S3 host name to use; if NULL is passed in, the
 *        default S3 host as provided to S3_initialize() will be used.
 * @param bucketName is the name of the bucket to be deleted
 * @param authRegion is the AWS region to use for the authorization signature
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_delete_bucket(S3Protocol protocol, S3UriStyle uriStyle,
                      const char *accessKeyId, const char *secretAccessKey,
                      const char *securityToken, const char *hostName,
                      const char *bucketName, const char *authRegion,
                      S3RequestContext *requestContext,
                      int timeoutMs,
                      const S3ResponseHandler *handler, void *callbackData);


/**
 * Lists keys within a bucket.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request
 * @param prefix if present and non-empty, gives a prefix for matching keys
 * @param marker if present and non-empty, only keys occuring after this value
 *        will be listed
 * @param delimiter if present and non-empty, causes keys that contain the
 *        same string between the prefix and the first occurrence of the
 *        delimiter to be rolled up into a single result element
 * @param maxkeys is the maximum number of keys to return
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_list_bucket(const S3BucketContext *bucketContext,
                    const char *prefix, const char *marker,
                    const char *delimiter, int maxkeys,
                    S3RequestContext *requestContext,
                    int timeoutMs,
                    const S3ListBucketHandler *handler, void *callbackData);


/** **************************************************************************
 * Object Functions
 ************************************************************************** **/

/**
 * Puts object data to S3.  This overwrites any existing object at that key;
 * note that S3 currently only supports full-object upload.  The data to
 * upload will be acquired by calling the handler's putObjectDataCallback.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request
 * @param key is the key of the object to put to
 * @param contentLength is required and gives the total number of bytes that
 *        will be put
 * @param putProperties optionally provides additional properties to apply to
 *        the object that is being put to
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_put_object(const S3BucketContext *bucketContext, const char *key,
                   uint64_t contentLength,
                   const S3PutProperties *putProperties,
                   S3RequestContext *requestContext,
                   int timeoutMs,
                   const S3PutObjectHandler *handler, void *callbackData);


/**
 * Copies an object from one location to another.  The object may be copied
 * back to itself, which is useful for replacing metadata without changing
 * the object.
 *
 * @param bucketContext gives the source bucket and associated parameters for
 *        this request
 * @param key is the source key
 * @param destinationBucket gives the destination bucket into which to copy
 *        the object.  If NULL, the source bucket will be used.
 * @param destinationKey gives the destination key into which to copy the
 *        object.  If NULL, the source key will be used.
 * @param putProperties optionally provides properties to apply to the object
 *        that is being put to.  If not supplied (i.e. NULL is passed in),
 *        then the copied object will retain the metadata of the copied
 *        object.
 * @param lastModifiedReturn returns the last modified date of the copied
 *        object
 * @param eTagReturnSize specifies the number of bytes provided in the
 *        eTagReturn buffer
 * @param eTagReturn is a buffer into which the resulting eTag of the copied
 *        object will be written
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_copy_object(const S3BucketContext *bucketContext,
                    const char *key, const char *destinationBucket,
                    const char *destinationKey,
                    const S3PutProperties *putProperties,
                    int64_t *lastModifiedReturn, int eTagReturnSize,
                    char *eTagReturn, S3RequestContext *requestContext,
                    int timeoutMs,
                    const S3ResponseHandler *handler, void *callbackData);


/**
 * Copies portion of an object from one location to another.  The object may
 * be copied back to itself, which is useful for replacing metadata without
 * changing the object.  Required when doing >5GB object copies.
 *
 * @param bucketContext gives the source bucket and associated parameters for
 *        this request
 * @param key is the source key
 * @param destinationBucket gives the destination bucket into which to copy
 *        the object.  If NULL, the source bucket will be used.
 * @param destinationKey gives the destination key into which to copy the
 *        object.  If NULL, the source key will be used.
 * @param partNo is the sequence numebr of any multipart upload, 0 = non-multipart
 * @param uploadId is the ID returned for a multipart initialize request, ignored
 *        if partNo = 0
 * @param startOffset is the starting point in original object to copy.
 * @param count is the number of bytes starting at startOffset in original
 *        object to copy.  0 indicates no-range (i.e. all)
 * @param putProperties optionally provides properties to apply to the object
 *        that is being put to.  If not supplied (i.e. NULL is passed in),
 *        then the copied object will retain the metadata of the copied
 *        object.
 * @param lastModifiedReturn returns the last modified date of the copied
 *        object
 * @param eTagReturnSize specifies the number of bytes provided in the
 *        eTagReturn buffer
 * @param eTagReturn is a buffer into which the resulting eTag of the copied
 *        object will be written
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_copy_object_range(const S3BucketContext *bucketContext,
                          const char *key, const char *destinationBucket,
                          const char *destinationKey,
                          const int partNo, const char *uploadId,
                          const unsigned long startOffset, const unsigned long count,
                          const S3PutProperties *putProperties,
                          int64_t *lastModifiedReturn, int eTagReturnSize,
                          char *eTagReturn, S3RequestContext *requestContext,
                          int timeoutMs,
                          const S3ResponseHandler *handler, void *callbackData);


/**
 * Gets an object from S3.  The contents of the object are returned in the
 * handler's getObjectDataCallback.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request
 * @param key is the key of the object to get
 * @param getConditions if non-NULL, gives a set of conditions which must be
 *        met in order for the request to succeed
 * @param startByte gives the start byte for the byte range of the contents
 *        to be returned
 * @param byteCount gives the number of bytes to return; a value of 0
 *        indicates that the contents up to the end should be returned
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_get_object(const S3BucketContext *bucketContext, const char *key,
                   const S3GetConditions *getConditions,
                   uint64_t startByte, uint64_t byteCount,
                   S3RequestContext *requestContext,
                   int timeoutMs,
                   const S3GetObjectHandler *handler, void *callbackData);


/**
 * Gets the response properties for the object, but not the object contents.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request
 * @param key is the key of the object to get the properties of
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_head_object(const S3BucketContext *bucketContext, const char *key,
                    S3RequestContext *requestContext,
                    int timeoutMs,
                    const S3ResponseHandler *handler, void *callbackData);

/**
 * Deletes an object from S3.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request
 * @param key is the key of the object to delete
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_delete_object(const S3BucketContext *bucketContext, const char *key,
                      S3RequestContext *requestContext,
                      int timeoutMs,
                      const S3ResponseHandler *handler, void *callbackData);


/** **************************************************************************
 * Access Control List Functions
 ************************************************************************** **/

/**
 * Gets the ACL for the given bucket or object.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request
 * @param key is the key of the object to get the ACL of; or NULL to get the
 *        ACL of the bucket
 * @param ownerId must be supplied as a buffer of at least
 *        S3_MAX_GRANTEE_USER_ID_SIZE bytes, and will be filled in with the
 *        owner ID of the object/bucket
 * @param ownerDisplayName must be supplied as a buffer of at least
 *        S3_MAX_GRANTEE_DISPLAY_NAME_SIZE bytes, and will be filled in with
 *        the display name of the object/bucket
 * @param aclGrantCountReturn returns the number of S3AclGrant structures
 *        returned in the aclGrants parameter
 * @param aclGrants must be passed in as an array of at least
 *        S3_MAX_ACL_GRANT_COUNT S3AclGrant structures, which will be filled
 *        in with the grant information for the ACL
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_get_acl(const S3BucketContext *bucketContext, const char *key,
                char *ownerId, char *ownerDisplayName,
                int *aclGrantCountReturn, S3AclGrant *aclGrants,
                S3RequestContext *requestContext,
                int timeoutMs,
                const S3ResponseHandler *handler, void *callbackData);


/**
 * Sets the ACL for the given bucket or object.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request
 * @param key is the key of the object to set the ACL for; or NULL to set the
 *        ACL for the bucket
 * @param ownerId is the owner ID of the object/bucket.  Unfortunately, S3
 *        requires this to be valid and thus it must have been fetched by a
 *        previous S3 request, such as a list_buckets request.
 * @param ownerDisplayName is the owner display name of the object/bucket.
 *        Unfortunately, S3 requires this to be valid and thus it must have
 *        been fetched by a previous S3 request, such as a list_buckets
 *        request.
 * @param aclGrantCount is the number of ACL grants to set for the
 *        object/bucket
 * @param aclGrants are the ACL grants to set for the object/bucket
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_set_acl(const S3BucketContext *bucketContext, const char *key,
                const char *ownerId, const char *ownerDisplayName,
                int aclGrantCount, const S3AclGrant *aclGrants,
                S3RequestContext *requestContext,
                int timeoutMs,
                const S3ResponseHandler *handler, void *callbackData);


/** **************************************************************************
 * Lifecycle Control Functions
 ************************************************************************** **/

/**
 * Gets the lifecycle for the given bucket
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request
 * @param lifecycleXmlDocumentReturn buffer for lifecycle XML document
 * @param lifecycleXmlDocumentBufferSize size of the buffer
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_get_lifecycle(const S3BucketContext *bucketContext,
                      char *lifecycleXmlDocumentReturn, int lifecycleXmlDocumentBufferSize,
                      S3RequestContext *requestContext,
                      int timeoutMs,
                      const S3ResponseHandler *handler, void *callbackData);


/**
 * Sets the lifecycle for the given bucket
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request
 * @param lifecycleXmlDocument Lifecycle configuration as an XML document
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_set_lifecycle(const S3BucketContext *bucketContext,
                      const char *lifecycleXmlDocument,
                      S3RequestContext *requestContext,
                      int timeoutMs,
                      const S3ResponseHandler *handler, void *callbackData);

/** **************************************************************************
 * Server Access Log Functions
 ************************************************************************** **/

/**
 * Gets the service access logging settings for a bucket.  The service access
 * logging settings specify whether or not the S3 service will write service
 * access logs for requests made for the given bucket, and if so, several
 * settings controlling how these logs will be written.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request; this is the bucket for which service access logging is
 *        being requested
 * @param targetBucketReturn must be passed in as a buffer of at least
 *        (S3_MAX_BUCKET_NAME_SIZE + 1) bytes in length, and will be filled
 *        in with the target bucket name for access logging for the given
 *        bucket, which is the bucket into which access logs for the specified
 *        bucket will be written.  This is returned as an empty string if
 *        service access logging is not enabled for the given bucket.
 * @param targetPrefixReturn must be passed in as a buffer of at least
 *        (S3_MAX_KEY_SIZE + 1) bytes in length, and will be filled in
 *        with the key prefix for server access logs for the given bucket,
 *        or the empty string if no such prefix is specified.
 * @param aclGrantCountReturn returns the number of ACL grants that are
 *        associated with the server access logging for the given bucket.
 * @param aclGrants must be passed in as an array of at least
 *        S3_MAX_ACL_GRANT_COUNT S3AclGrant structures, and these will be
 *        filled in with the target grants associated with the server access
 *        logging for the given bucket, whose number is returned in the
 *        aclGrantCountReturn parameter.  These grants will be applied to the
 *        ACL of any server access logging log files generated by the S3
 *        service for the given bucket.
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_get_server_access_logging(const S3BucketContext *bucketContext,
                                  char *targetBucketReturn,
                                  char *targetPrefixReturn,
                                  int *aclGrantCountReturn,
                                  S3AclGrant *aclGrants,
                                  S3RequestContext *requestContext,
                                  int timeoutMs,
                                  const S3ResponseHandler *handler,
                                  void *callbackData);


/**
 * Sets the service access logging settings for a bucket.  The service access
 * logging settings specify whether or not the S3 service will write service
 * access logs for requests made for the given bucket, and if so, several
 * settings controlling how these logs will be written.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request; this is the bucket for which service access logging is
 *        being set
 * @param targetBucket gives the target bucket name for access logging for the
 *        given bucket, which is the bucket into which access logs for the
 *        specified bucket will be written.
 * @param targetPrefix is an option parameter which specifies the key prefix
 *        for server access logs for the given bucket, or NULL if no such
 *        prefix is to be used.
 * @param aclGrantCount specifies the number of ACL grants that are to be
 *        associated with the server access logging for the given bucket.
 * @param aclGrants is as an array of S3AclGrant structures, whose number is
 *        given by the aclGrantCount parameter.  These grants will be applied
 *        to the ACL of any server access logging log files generated by the
 *        S3 service for the given bucket.
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_set_server_access_logging(const S3BucketContext *bucketContext,
                                  const char *targetBucket,
                                  const char *targetPrefix, int aclGrantCount,
                                  const S3AclGrant *aclGrants,
                                  S3RequestContext *requestContext,
                                  int timeoutMs,
                                  const S3ResponseHandler *handler,
                                  void *callbackData);


/**
 * This operation initiates a multipart upload and returns an upload ID.
 * This upload ID is used to associate all the parts in the specific
 * multipart upload. You specify this upload ID in each of your subsequent
 * upload part requests
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request; this is the bucket for which service access logging is
 *        being set
 * @param key is the source key
 * @param putProperties optionally provides additional properties to apply to
 *        the object that is being put to
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_initiate_multipart(S3BucketContext *bucketContext, const char *key,
                           S3PutProperties *putProperties,
                           S3MultipartInitialHandler *handler,
                           S3RequestContext *requestContext,
                           int timeoutMs,
                           void *callbackData);


/**
 * This operation uploads a part in a multipart upload.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request; this is the bucket for which service access logging is
 *        being set
 * @param key is the source key
 * @param putProperties optionally provides additional properties to apply to
 *        the object that is being put to
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param seq is a part number uniquely identifies a part and also
 *        defines its position within the object being created.
 * @param upload_id get from S3_initiate_multipart return
 * @param partContentLength gives the size of the part, in bytes
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_upload_part(S3BucketContext *bucketContext, const char *key,
                    S3PutProperties * putProperties,
                    S3PutObjectHandler *handler,
                    int seq, const char *upload_id, int partContentLength,
                    S3RequestContext *requestContext,
                    int timeoutMs,
                    void *callbackData);


/**
 * This operation completes a multipart upload by assembling previously
 * uploaded parts.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request; this is the bucket for which service access logging is
 *        being set
 * @param key is the source key
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param upload_id get from S3_initiate_multipart return
 * @param contentLength gives the total size of the commit message, in bytes
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_complete_multipart_upload(S3BucketContext *bucketContext,
                                  const char *key,
                                  S3MultipartCommitHandler *handler,
                                  const char *upload_id,
                                  int contentLength,
                                  S3RequestContext *requestContext,
                                  int timeoutMs,
                                  void *callbackData);


/**
 * This operation lists the parts that have been uploaded for a specific
 * multipart upload.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request; this is the bucket for which service access logging is
 *        being set
 * @param key is the source key
 * @param partnumbermarker if present and non-empty, specifies the part after
 *        which listing should begin.  Only parts with higher part numbers
 *        will be listed.
 * @param uploadid identifying the multipart upload whose parts are being
 *        listed.
 * @param encodingtype if present and non-empty, requests Amazon S3 to encode
 *        the response and specifies the encoding method to use.
 * @param maxparts Sets the maximum number of parts to return in the response
 *        body. Default: 1,000
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_list_parts(S3BucketContext *bucketContext, const char *key,
                   const char *partnumbermarker,
                   const char *uploadid, const char *encodingtype,
                   int maxparts, S3RequestContext *requestContext,
                   int timeoutMs,
                   const S3ListPartsHandler *handler, void *callbackData);


/**
 * This operation aborts a multipart upload. After a multipart upload is
 * aborted, no additional parts can be uploaded using that upload ID.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request; this is the bucket for which service access logging is
 *        being set
 * @param key is the source key
 * @param uploadId identifying the multipart upload whose parts are being
 *        listed.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 **/
void S3_abort_multipart_upload(S3BucketContext *bucketContext, const char *key,
                               const char *uploadId,
                               int timeoutMs,
                               S3AbortMultipartUploadHandler *handler,
                               S3RequestContext *requestContext, // WINSCP
                               void *callbackData); // WINSCP


/**
 * This operation lists in-progress multipart uploads. An in-progress
 * multipart upload is a multipart upload that has been initiated,
 * using the Initiate Multipart Upload request, but has not yet been
 * completed or aborted.
 *
 * @param bucketContext gives the bucket and associated parameters for this
 *        request; this is the bucket for which service access logging is
 *        being set
 * @param prefix if present and non-empty, lists in-progress uploads only for
 *        those keys that begin with the specified prefix.
 * @param keymarker if present and non-empty, together with upload-id-marker,
 *        this parameter specifies the multipart upload after which listing
 *        should begin.
 * @param uploadidmarker if present and non-empty, together with key-marker,
 *        specifies the multipart upload after which listing should begin.
 * @param encodingtype if present and non-empty, requests Amazon S3 to encode
 *        the response and specifies the encoding method to use.
 * @param delimiter if present and non-empty, is the character you use to
 *        group keys.
 * @param maxuploads sets the maximum number of multipart uploads,
 *        from 1 to 1,000, to return in the response body.
 * @param requestContext if non-NULL, gives the S3RequestContext to add this
 *        request to, and does not perform the request immediately.  If NULL,
 *        performs the request immediately and synchronously.
 * @param timeoutMs if not 0 contains total request timeout in milliseconds
 * @param handler gives the callbacks to call as the request is processed and
 *        completed
 * @param callbackData will be passed in as the callbackData parameter to
 *        all callbacks for this request
 **/
void S3_list_multipart_uploads(S3BucketContext *bucketContext,
                               const char *prefix, const char *keymarker,
                               const char *uploadidmarker,
                               const char *encodingtype, const char *delimiter,
                               int maxuploads, S3RequestContext *requestContext,
                               int timeoutMs,
                               const S3ListMultipartUploadsHandler *handler,
                               void *callbackData);

#ifdef WINSCP
int snprintf_S(char * s, size_t n, const char * format, size_t len, const char * data);
#endif

#ifdef __cplusplus
}
#endif

#endif /* LIBS3_H */
