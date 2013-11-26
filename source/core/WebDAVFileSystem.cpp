//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <malloc.h>
#include <stdio.h>
#include <wincrypt.h>

#include <apr_hash.h>
#include <apr_strings.h>
#include <apr_tables.h>
#include <apr_file_io.h>
#include <apr_portable.h>
#include <apr_atomic.h>

#include <ne_basic.h>
#include <ne_auth.h>
#include <ne_compress.h>
#include <ne_props.h>
#include <ne_defs.h>
#include <ne_uri.h>
#include <ne_session.h>
#include <ne_request.h>
#include <ne_xml.h>
#include <ne_pkcs11.h>

#include "WebDAVFileSystem.h"

#include "Interface.h"
#include "Common.h"
#include "Exceptions.h"
#include "Terminal.h"
#include "TextsCore.h"
#include "SecureShell.h"
#include "FileZillaIntf.h"
#include "HelpCore.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const int tfFirstLevel = 0x01;
const int tfAutoResume = 0x02;
//---------------------------------------------------------------------------
struct TSinkFileParams
{
  UnicodeString TargetDir;
  const TCopyParamType * CopyParam;
  int Params;
  TFileOperationProgressType * OperationProgress;
  bool Skipped;
  unsigned int Flags;
};
//---------------------------------------------------------------------------
struct TFileTransferData
{
  TFileTransferData()
  {
    Params = 0;
    AutoResume = false;
    OverwriteResult = -1;
    CopyParam = NULL;
  }

  UnicodeString FileName;
  int Params;
  bool AutoResume;
  int OverwriteResult;
  const TCopyParamType * CopyParam;
};
//---------------------------------------------------------------------------
struct TClipboardHandler
{
  UnicodeString Text;

  void __fastcall Copy(TObject * /*Sender*/)
  {
    CopyToClipboard(Text);
  }
};
//---------------------------------------------------------------------------

namespace webdav {

#pragma warn -8004

const AnsiString __cdecl Format(const char * format, va_list args)
{
  int len = AnsiString().vprintf(format, args);
  AnsiString Result;
  Result.SetLength(len + 1);
  vsprintf(&Result[1], format, args);
  return Result.c_str();
}

const AnsiString __cdecl Format(const char * format, ...)
{
  va_list args;
  va_start(args, format);
  AnsiString Result = Format(format, args);
  va_end(args);
  return Result;
}

//---------------------------------------------------------------------------
struct auth_baton_t;
struct vtable_t;
struct stream_t;
struct client_ctx_t;
struct auth_iterstate_t;
//---------------------------------------------------------------------------

typedef enum tristate_t
{
  tristate_false = 2,
  tristate_true,
  tristate_unknown
} tristate_t;

//------------------------------------------------------------------------------
// Userdata for the `proxy_auth' function.
struct proxy_auth_baton_t
{
  const char * username; // Cannot be NULL, but "" is okay.
  const char * password; // Cannot be NULL, but "" is okay.
};

//------------------------------------------------------------------------------
// from svn_types.h

typedef error_t (*cancel_func_t)(void * cancel_baton);

//------------------------------------------------------------------------------
// from svn_ra.h

typedef void (*progress_notify_func_t)(
  apr_off_t progress,
  apr_off_t total,
  void * baton,
  apr_pool_t * pool);

typedef error_t (*get_client_string_func_t)(void * baton,
  const char ** name,
  apr_pool_t * pool);

typedef struct callbacks2_t
{
  auth_baton_t * auth_baton;

  progress_notify_func_t progress_func;
  void * progress_baton;

  cancel_func_t cancel_func;
  get_client_string_func_t get_client_string;
} callbacks2_t;

typedef struct callback_baton_t
{
  client_ctx_t * ctx;
  apr_pool_t * pool;
} callback_baton_t;

//------------------------------------------------------------------------------
// from ra_loader.h

typedef error_t (*init_func_t)(const vtable_t ** vtable,
  apr_pool_t * pool);

//------------------------------------------------------------------------------
// from svn_string.h

// A simple counted string.
typedef struct string_t
{
  const char * data; //< pointer to the bytestring
  apr_size_t len;   //< length of bytestring
} string_t;

// A buffered string, capable of appending without an allocation and copy
// for each append.
typedef struct stringbuf_t
{
  apr_pool_t * pool;
  char * data;
  apr_size_t len;
  apr_size_t blocksize;
} stringbuf_t;

//------------------------------------------------------------------------------
// from ra_neon.h

// Rename these types and constants to abstract from Neon

#define NEON_XML_DECLINE NE_XML_DECLINE
#define NEON_XML_INVALID NE_XML_ABORT

#define NEON_XML_CDATA   (1<<1)
#define NEON_XML_COLLECT ((1<<2) | NEON_XML_CDATA)

// Related to anonymous enum below?
typedef int neon_xml_elmid;

typedef struct neon_xml_elm_t
{
  const char * nspace;
  const char * name;
  neon_xml_elmid id;

  // Processing flags for this namespace:tag.
  // 0 (zero)          - regular element, may have children,
  // NEON_XML_CDATA    - child-less element,
  // NEON_XML_COLLECT  - complete contents of such element must be
  //                     collected as CDATA, includes *_CDATA flag.
  unsigned int flags;

} neon_xml_elm_t;

typedef struct neon_session_t
{
  apr_pool_t * pool;
  stringbuf_t * url;                    // original, unparsed session url
  ne_uri root;                          // parsed version of above
  const char * webdav_root;             // URL for WebDAV resource root

  ne_session * ne_sess;                 // HTTP session to server

  const callbacks2_t * callbacks;       // callbacks to get auth data
  void * callback_baton;

  auth_iterstate_t * auth_iterstate;    // state of authentication retries
  bool auth_used;                       // Save authorization state after
                                        // successful usage

  auth_iterstate_t * p11pin_iterstate;  // state of PKCS#11 pin retries

  bool compression;                     // should we use http compression?

  progress_notify_func_t progress_func;
  void * progress_baton;

  apr_off_t total_progress;             // Total number of bytes sent in this
                                        // session with a -1 total marker
  apr_hash_t * capabilities;
} neon_session_t;

typedef struct neon_request_t
{
  ne_request * ne_req;                  // neon request structure
  ne_session * ne_sess;                 // neon session structure
  neon_session_t * sess;                // DAV session structure
  const char * method;
  const char * url;
  int rv;                               // Return value from
                                        // ne_request_dispatch() or -1 if
                                        // not dispatched yet.
  int code;                             // HTTP return code, or 0 if none
  const char * code_desc;               // Textual description of CODE
  error_t err;                          // error encountered while executing
                                        // the request
  bool marshalled_error;                // TRUE if the error was server-side
  apr_pool_t * pool;                    // where this struct is allocated
  apr_pool_t * iterpool;                // iteration pool
                                        // for use within callbacks
} neon_request_t;

// Related to neon_xml_elmid?
// add WEBDAV_NEON_ to these to prefix conflicts with (sys) headers?
enum
{
  // Redefine Neon elements
  // With the new API, we need to be able to use element id also as a return
  // value from the new `startelm' callback, hence all element ids must be
  // positive. Root element id is the only id that is not positive, it's zero.
  // `Root state' is never returned by a callback, it's only passed into it.
  // Therefore, negative element ids are forbidden from now on.
  ELEM_unknown = 1, // was (-1), see above why it's (1) now
  ELEM_root = NE_XML_STATEROOT, // (0)
  ELEM_UNUSED = 100,
  ELEM_207_first = ELEM_UNUSED,
  ELEM_multistatus = ELEM_207_first,
  ELEM_response = ELEM_207_first + 1,
  ELEM_responsedescription = ELEM_207_first + 2,
  ELEM_href = ELEM_207_first + 3,
  ELEM_propstat = ELEM_207_first + 4,
  ELEM_prop = ELEM_207_first + 5, // `prop' tag in the DAV namespace
  ELEM_status = ELEM_207_first + 6,
  ELEM_207_UNUSED = ELEM_UNUSED + 100,
  ELEM_PROPS_UNUSED = ELEM_207_UNUSED + 100,

  // DAV elements
  ELEM_collection = ELEM_207_UNUSED,
  ELEM_comment,
  ELEM_creationdate,
  ELEM_creator_displayname,
  ELEM_options_response,
  ELEM_set_prop,
  ELEM_remove_prop,
  ELEM_resourcetype,
  ELEM_get_content_length,
  ELEM_get_last_modified,
  ELEM_updated_set,
  ELEM_error,

  ELEM_human_readable,
};

// The session object.
struct session_t
{
  const vtable_t * vtable;

  // Pool used to manage this session.
  apr_pool_t * pool;

  // Private data for the RA implementation.
  void * priv;
};

typedef std::vector<TListDataEntry> listdataentry_vector_t;

// Baton used when listing directory entries.
typedef struct list_func_baton_t
{
  bool verbose;
  listdataentry_vector_t * entries;
  session_t * session;
  apr_pool_t * pool;
} list_func_baton_t;

//------------------------------------------------------------------------------

// timeout (in seconds)
#define DEFAULT_HTTP_TIMEOUT 10

#define WEBDAV_ERR_DAV_SOCK_INIT 1000
#define WEBDAV_ERR_XML_MALFORMED 1001
#define WEBDAV_ERR_DAV_OPTIONS_REQ_FAILED 1002
#define WEBDAV_ERR_DAV_REQUEST_FAILED 1003
#define WEBDAV_ERR_INCORRECT_PARAMS 1004
#define WEBDAV_ERR_FS_NOT_FOUND 1005
#define WEBDAV_ERR_FS_PROP_BASEVALUE_MISMATCH 1006
#define WEBDAV_ERR_DAV_FORBIDDEN 1010
#define WEBDAV_ERR_DAV_RELOCATED 1011
#define WEBDAV_ERR_NOT_AUTHORIZED 1012
#define WEBDAV_ERR_ILLEGAL_URL 1013
#define WEBDAV_ERR_DAV_NOT_IMPLEMENTED 1014
#define WEBDAV_ERR_NOT_IMPLEMENTED 1015

#define WEBDAV_ERR_AUTHN_NO_PROVIDER 1020
#define WEBDAV_ERR_CLIENT_CYCLE_DETECTED 1021
#define WEBDAV_ERR_DAV_MALFORMED_DATA 1022
#define WEBDAV_ERR_IO_PIPE_WRITE_ERROR 1023
#define WEBDAV_ERR_BAD_DATE 1024
#define WEBDAV_ERR_CLIENT_UNRELATED_RESOURCES 1025
#define WEBDAV_ERR_ROOT_URL_MISMATCH 1027
#define WEBDAV_ERR_BAD_FILENAME 1028
#define WEBDAV_ERR_ENTRY_MISSING_URL 1029
#define WEBDAV_ERR_FS_NOT_FILE 1030
#define WEBDAV_ERR_ATOMIC_INIT_FAILURE 1033
#define WEBDAV_ERR_IO_UNIQUE_NAMES_EXHAUSTED 1034
#define WEBDAV_ERR_STREAM_SEEK_NOT_SUPPORTED 1035
#define WEBDAV_ERR_BAD_CONFIG_VALUE 1040
#define WEBDAV_ERR_CANCELLED 1050
#define WEBDAV_ERR_DAV_INVALID_CONFIG_VALUE 1051
#define WEBDAV_ERR_DAV_PROPPATCH_FAILED 1060
#define WEBDAV_ERR_CANNOT_PUT_FILE 1061
#define WEBDAV_ERR_CANNOT_DELETE_FILE 1062
#define WEBDAV_ERR_CANNOT_MKCOL 1063
#define WEBDAV_ERR_CANNOT_MOVE 1064
#define WEBDAV_ERR_CANNOT_PROPFIND 1065
#define WEBDAV_ERR_BAD_PARAM 1070

// The application doesn't want any providers to save credentials
// to disk. Property value is irrelevant; only property's existence
// matters.
#define AUTH_PARAM_NO_AUTH_CACHE AUTH_PARAM_PREFIX "no-auth-cache"
// #define HASH_KEY_STRING ""

#define MAX_REDIRECT_ATTEMPTS 3 // TODO:  Make configurable.
//---------------------------------------------------------------------------
// #define WEBDAV_ARRAY_IDX(ary,i,type) ((type)(ary)[i])
#define WEBDAV_NO_ERROR 0
#define WEBDAV_UNKNOWN_ERROR 1
#define WEBDAV_ERR(expr)              \
  do {                                \
  webdav::error_t err__temp = (expr); \
  if (err__temp)                      \
    return err__temp;                 \
  } while (0)

// A statement macro, very similar to WEBDAV_ERR.
// This macro will wrap the error with the specified text before
// returning the error.

#define WEBDAV_ERR_W(expr, wrap_msg)                            \
  do {                                                          \
    webdav::error_t err__temp = (expr);                         \
    if (err__temp)                                              \
      return webdav::error_create(err__temp, NULL, wrap_msg);   \
  } while (0)

#define WEBDAV_ERR_ASSERT(expr)                                 \
  do {                                                          \
    if (!(expr))                                                \
      WEBDAV_ERR(WEBDAV_UNKNOWN_ERROR);                         \
  } while (0)

#define error_trace(expr)  (expr)

// Create a pool as a subpool of parent_pool
#define webdav_pool_create(parent_pool) webdav::pool_create_ex(parent_pool, NULL)
#define webdav_pool_clear apr_pool_clear
// Destroy a pool and all of its children.
// This define for webdav_pool_destroy exists for symmetry and
// completeness.

#define webdav_pool_destroy apr_pool_destroy

// Destroy request REQ and any associated resources
#define neon_request_destroy(req) webdav_pool_destroy((req)->pool)

// Statement macro to set the request error,
// making sure we don't leak any in case we encounter more than one error.
// Sets the 'err' field of REQ to the value obtained by evaluating NEW_ERR.

#define NEON_REQ_ERR(req, new_err)               \
   do {                                          \
     error_t err__tmp = (new_err);               \
     if ((req)->err && !(req)->marshalled_error) \
       error_clear(&err__tmp);                   \
     else if (err__tmp)                          \
       {                                         \
         error_clear(&(req)->err);               \
         (req)->err = err__tmp;                  \
         (req)->marshalled_error = false;        \
       }                                         \
   } while (0)

//===========================================================================

//------------------------------------------------------------------------------
// from error.c

static void
error_clear(error_t * err)
{
  if (err) *err = 0;
}

static error_t
make_error_internal(
  apr_status_t apr_err,
  error_t * child)
{
  error_t new_error = apr_err;
  return new_error;
}

static error_t
error_create(
  apr_status_t apr_err,
  error_t * child,
  const char * message)
{
  return apr_err;
}

static error_t
error_createf(
  apr_status_t apr_err,
  error_t * child,
  const char * fmt,
  ...)
{
  va_list args;

  va_start(args, fmt);
  AnsiString Message = Format(fmt, args);
  va_end(args);

  AnsiString Message2 = Format("Error, code: %d, message: %s", apr_err, Message.c_str());
  throw ExtException(UnicodeString(Message2), NULL);
}

static error_t
error_wrap_apr(
  apr_status_t status,
  const char * fmt,
  ...)
{
  error_t err = 0;
  va_list args;

  err = make_error_internal(status, NULL);

  va_start(args, fmt);
  AnsiString Message = Format(fmt, args);
  va_end(args);

  err = error_create(err, NULL, Message.c_str());

  return err;
}

//------------------------------------------------------------------------------
// from utils.c

// a cleanup routine attached to the pool that contains the RA session
// root URI.
static apr_status_t
cleanup_uri(void * uri)
{
  ne_uri_free(static_cast<ne_uri *>(uri));
  return APR_SUCCESS;
}

static error_t
parse_url(
  const char * url, ne_uri * uri)
{
  if (ne_uri_parse(url, uri) ||
      (uri->host == NULL) || (uri->path == NULL) || (uri->scheme == NULL))
  {
    ne_uri_free(uri);
    return 0;
  }
  if (uri->port == 0)
    uri->port = ne_uri_defaultport(uri->scheme);

  return 1;
}

static error_t
parse_ne_uri(
  ne_uri ** uri,
  const char * webdav_url,
  apr_pool_t * pool)
{
  // Sanity check the URI
  *uri = static_cast<ne_uri *>(apr_pcalloc(pool, sizeof(**uri)));
  if (!parse_url(webdav_url, *uri))
  {
    return error_createf(WEBDAV_ERR_ILLEGAL_URL, NULL,
      "URL '%s' is malformed or the "
      "scheme or host or path is missing", webdav_url);
  }
  // make sure we eventually destroy the uri
  apr_pool_cleanup_register(pool, *uri, cleanup_uri, apr_pool_cleanup_null);
  return WEBDAV_NO_ERROR;
}

//------------------------------------------------------------------------------
// from svn_types.h

#define atoui64(X) ((apr_uint64_t) apr_atoi64(X))

// An indication that you are interested in the kind field
#define WEBDAV_DIRENT_KIND        0x00001

// An indication that you are interested in the size field
#define WEBDAV_DIRENT_SIZE        0x00002

// An indication that you are interested in the time field
#define WEBDAV_DIRENT_TIME        0x00010

// A combination of all the dirent fields
#define WEBDAV_DIRENT_ALL ~((apr_uint32_t ) 0)

// The various types of nodes in filesystem.
typedef enum node_kind_t
{
  // absent
  node_none,

  // regular file
  node_file,

  // directory
  node_dir,

  // something's here, but we don't know what
  node_unknown
} node_kind_t;

// A general directory entry.
typedef struct dirent_t
{
  // node kind
  node_kind_t kind;

  // length of file text, or 0 for directories
  apr_int64_t size;

  // time of mod-time
  apr_time_t time;
} dirent_t;

typedef enum depth_t
{
  // Just the named directory D, no entries.  Updates will not pull in
  // any files or subdirectories not already present.
  depth_empty      =  0,

  // D + its file children, but not subdirs.  Updates will pull in any
  // files not already present, but not subdirectories.
  depth_files      =  1,

  // D + immediate children (D and its entries).  Updates will pull in
  // any files or subdirectories not already present; those
  // subdirectories' this_dir entries will have depth-empty.
  depth_immediates =  2,

  // D + all descendants (full recursion from D).  Updates will pull
  // in any files or subdirectories not already present; those
  // subdirectories' this_dir entries will have depth-infinity.
  // Equivalent to the pre-1.5 default update behavior.
  depth_infinity   =  3

} depth_t;

//------------------------------------------------------------------------------
// from utf.h

static error_t
utf_cstring_to_utf8(
  const char ** dest,
  const char * src,
  apr_pool_t * pool);

static error_t
utf_cstring_from_utf8(
  const char ** dest,
  const char * src,
  apr_pool_t * pool);

// Local wrapper of path_cstring_from_utf8() that does no copying on
// operating systems where APR always uses utf-8 as native path format
static error_t
cstring_from_utf8(
  const char ** path_apr,
  const char * path_utf8,
  apr_pool_t * pool);

//------------------------------------------------------------------------------
// from ra_neon.h

#ifdef WEBDAV_DEBUG
#define DEBUG_CR "\n"
#else
#define DEBUG_CR ""
#endif

#define NEON_DEPTH_ZERO      0
#define NEON_DEPTH_ONE       1
#define NEON_DEPTH_INFINITE -1

// NEON_PROP_*: properties that we fetch from the server
// These are simply symbolic names for some standard properties that we fetch.

#define NEON_PROP_CREATIONDATE   "DAV:creationdate"
#define NEON_PROP_GETCONTENTLENGTH "DAV:getcontentlength"

typedef struct neon_resource_t
{
  // what is the URL for this resource
  const char * url;

  // is this resource a collection? (from the DAV:resourcetype element)
  int is_collection;

  // PROPSET: NAME -> VALUE (const char * -> const string_t *)
  apr_hash_t * propset;

  // --- only used during response processing ---
  // when we see a DAV:href element, what element is the parent?
  int href_parent;

  apr_pool_t * pool;

} neon_resource_t;

// Our equivalent of ne_xml_startelm_cb, the difference being that it
// returns errors in a error_t, and returns the element type via
// ELEM.  To ignore the element *ELEM should be set to
// NEON_XML_DECLINE and WEBDAV_NO_ERROR should be returned.
// *ELEM can be set to NEON_XML_INVALID to indicate invalid XML
// (and abort the parse).

typedef error_t (*neon_startelm_cb_t)(int * elem,
  void * baton,
  int parent,
  const char * nspace,
  const char * name,
  const char ** atts);

// Our equivalent of ne_xml_cdata_cb, the difference being that it returns
// errors in a error_t.

typedef error_t (*neon_cdata_cb_t)(void * baton,
  int state,
  const char * cdata,
  size_t len);

// Our equivalent of ne_xml_endelm_cb, the difference being that it returns
// errors in a error_t.

typedef error_t (*neon_endelm_cb_t)(void * baton,
  int state,
  const char * nspace,
  const char * name);

static ne_xml_parser *
neon_xml_parser_create(
  neon_request_t * req,
  ne_accept_response accpt,
  neon_startelm_cb_t startelm_cb,
  neon_cdata_cb_t cdata_cb,
  neon_endelm_cb_t endelm_cb,
  void * baton);

static error_t
neon_request_dispatch(
  int * code_p,
  neon_request_t * req,
  apr_hash_t * extra_headers,
  const char * body,
  int okay_1,
  int okay_2,
  bool check_errors,
  apr_pool_t * pool);

static error_t
neon_check_parse_error(
  const char * method,
  ne_xml_parser * xml_parser,
  const char * url);

static error_t
neon_request_create(
  neon_request_t ** request,
  neon_session_t * sess,
  const char * method, const char * url,
  apr_pool_t * pool);

static const neon_xml_elm_t *
neon_lookup_xml_elem(
  const neon_xml_elm_t * table,
  const char * nspace,
  const char * name);

static error_t
neon_xml_collect_cdata(
  void * baton, int state,
  const char * cdata, size_t len);

static const char *
neon_request_get_location(
  neon_request_t * request,
  apr_pool_t * pool);

// Our version of ne_block_reader, which returns an
// error_t instead of an int.
typedef error_t (*neon_block_reader)(
  void * baton,
  const char * data,
  size_t len);

//------------------------------------------------------------------------------
// from dirent_uri.h

static bool
dirent_is_root(
  const char * dirent,
  apr_size_t len);

//------------------------------------------------------------------------------
// from svn_props.h

static error_t
neon_get_props_resource(
  neon_resource_t ** rsrc,
  neon_session_t * sess,
  const char * url,
  const ne_propname * which_props,
  bool check_errors,
  apr_pool_t * pool);

//------------------------------------------------------------------------------
// from ra_loader.h

// The RA layer vtable.
typedef struct vtable_t
{
  // Return a short description of the RA implementation, as a localized
  // string.
  const char * (*get_description)(void);

  // Return a list of actual URI schemes supported by this implementation.
  // The returned array is NULL-terminated.
  const char * const * (*get_schemes)(apr_pool_t * pool);

  // Implementations of the public API functions.

  // See session_open().
  // All fields in SESSION, except priv, have been initialized by the
  // time this is called.  SESSION->priv may be set by this function.
  error_t (*open_session)(session_t * session,
                          const char ** corrected_url,
                          const char * session_URL,
                          const callbacks2_t * callbacks,
                          void * callback_baton,
                          apr_pool_t * pool);
  // See reparent().
  // URL is guaranteed to have what get_webdav_resource_root() returns as a prefix.
  error_t (*reparent)(session_t * session,
                      const char * url,
                      apr_pool_t * pool);
  // See get_session_url().
  error_t (*get_session_url)(session_t * session,
                             const char ** url,
                             apr_pool_t * pool);

  // See get_file().
  error_t (*get_file)(session_t * session,
                      const char * path,
                      stream_t * stream,
                      apr_hash_t ** props,
                      apr_pool_t * pool);
  // See get_dir2().
  error_t (*get_dir)(session_t * session,
                     apr_hash_t ** dirents,
                     const char * path,
                     apr_uint32_t dirent_fields,
                     apr_pool_t * pool);
  // See check_path().
  error_t (*check_path)(session_t * session,
                        const char * path,
                        node_kind_t * kind,
                        apr_pool_t * pool);
  // See stat().
  error_t (*stat)(session_t * session,
                  const char * path,
                  dirent_t ** dirent,
                  apr_pool_t * pool);
  // See get_webdav_resource_root2().
  error_t (*get_webdav_resource_root)(session_t * session,
                            const char ** url,
                            apr_pool_t * pool);
  // See has_capability().
  // error_t (*has_capability)(session_t *session,
  // bool *has,
  // const char *capability,
  // apr_pool_t *pool);
} vtable_t;

static error_t
get_path_relative_to_root(
  session_t * session,
  const char ** rel_path,
  const char * url,
  apr_pool_t * pool);

static error_t
neon_init(
  const vtable_t ** vtable,
  apr_pool_t * pool);

//------------------------------------------------------------------------------
// from svn_ctype.h
// Table of flags for character classification.
extern const apr_uint32_t * const ctype_table;

// Check if c is in the character class described by flags.
// The flags is a bitwise-or combination of WEBDAV_CTYPE_*
// constants. Uses #ctype_table.

#define ctype_test(c, flags) \
  (0 != (ctype_table[(unsigned char)(c)] & (flags)))

// Basic character classes
#define WEBDAV_CTYPE_CNTRL    0x0001 //< Control character
#define WEBDAV_CTYPE_SPACE    0x0002 //< Whitespace
#define WEBDAV_CTYPE_DIGIT    0x0004 //< Decimal digit
#define WEBDAV_CTYPE_UPPER    0x0008 //< Uppercase letter
#define WEBDAV_CTYPE_LOWER    0x0010 //< Lowercase letter
#define WEBDAV_CTYPE_PUNCT    0x0020 //< Punctuation mark
#define WEBDAV_CTYPE_XALPHA   0x0040 //< Hexadecimal digits A to F
#define WEBDAV_CTYPE_ASCII    0x0080 //< ASCII subset*/

// Derived character classes
// ASCII letter
#define WEBDAV_CTYPE_ALPHA    (WEBDAV_CTYPE_LOWER | WEBDAV_CTYPE_UPPER)
// ASCII letter or decimal digit
#define WEBDAV_CTYPE_ALNUM    (WEBDAV_CTYPE_ALPHA | WEBDAV_CTYPE_DIGIT)
// ASCII hexadecimal digit
#define WEBDAV_CTYPE_XDIGIT   (WEBDAV_CTYPE_DIGIT | WEBDAV_CTYPE_XALPHA)
// Printable ASCII except space
#define WEBDAV_CTYPE_GRAPH    (WEBDAV_CTYPE_PUNCT | WEBDAV_CTYPE_ALNUM)
// All printable ASCII
#define WEBDAV_CTYPE_PRINT    (WEBDAV_CTYPE_GRAPH | WEBDAV_CTYPE_SPACE)

// Check if c is an ASCII control character.
#define ctype_iscntrl(c)  ctype_test((c), WEBDAV_CTYPE_CNTRL)

// Check if c is an ASCII whitespace character.
#define ctype_isspace(c)  ctype_test((c), WEBDAV_CTYPE_SPACE)

// Check if c is an ASCII digit.
#define ctype_isdigit(c)  ctype_test((c), WEBDAV_CTYPE_DIGIT)

// Check if c is an ASCII uppercase letter.
#define ctype_isupper(c)  ctype_test((c), WEBDAV_CTYPE_UPPER)

// Check if c is an ASCII lowercase letter.
#define ctype_islower(c)  ctype_test((c), WEBDAV_CTYPE_LOWER)

// Check if c is an ASCII punctuation mark.
#define ctype_ispunct(c)  ctype_test((c), WEBDAV_CTYPE_PUNCT)

// Check if c is an ASCII character.
#define ctype_isascii(c)  ctype_test((c), WEBDAV_CTYPE_ASCII)

// Check if c is an ASCII letter.
#define ctype_isalpha(c)  ctype_test((c), WEBDAV_CTYPE_ALPHA)

// Check if c is an ASCII letter or decimal digit.
#define ctype_isalnum(c)  ctype_test((c), WEBDAV_CTYPE_ALNUM)

// Check if c is an ASCII hexadecimal digit.
#define ctype_isxdigit(c) ctype_test((c), WEBDAV_CTYPE_XDIGIT)

// Check if c is an ASCII graphical (visible printable) character.
#define ctype_isgraph(c)  ctype_test((c), WEBDAV_CTYPE_GRAPH)

// Check if c is an ASCII printable character.
#define ctype_isprint(c)  ctype_test((c), WEBDAV_CTYPE_PRINT)

// Basic extended character classes
#define WEBDAV_CTYPE_UTF8LEAD 0x0100 //< UTF-8 multibyte lead byte
#define WEBDAV_CTYPE_UTF8CONT 0x0200 //< UTF-8 multibyte non-lead byte
#define WEBDAV_CTYPE_XMLNAME  0x0400
#define WEBDAV_CTYPE_URISAFE  0x0800

// Derived extended character classes
// Part of a UTF-8 multibyte character.
#define WEBDAV_CTYPE_UTF8MBC  (WEBDAV_CTYPE_UTF8LEAD | WEBDAV_CTYPE_UTF8CONT)
// All valid UTF-8 bytes.
#define WEBDAV_CTYPE_UTF8     (WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UTF8MBC)

// Check if c is a UTF-8 multibyte lead byte.
#define ctype_isutf8lead(c) ctype_test((c), WEBDAV_CTYPE_UTF8LEAD)

// Check if c is a UTF-8 multibyte continuation (non-lead) byte.
#define ctype_isutf8cont(c) ctype_test((c), WEBDAV_CTYLE_UTF8CONT)

// Check if c is part of a UTF-8 multibyte character.
#define ctype_isutf8mbc(c)  ctype_test((c), WEBDAV_CTYPE_UTF8MBC)

// Check if c is valid in UTF-8.
#define ctype_isutf8(c)     ctype_test((c), WEBDAV_CTYPE_UTF8)

#define WEBDAV_CTYPE_ASCII_MINUS            45 //< ASCII value of '-'
#define WEBDAV_CTYPE_ASCII_DOT              46 //< ASCII value of '.'
#define WEBDAV_CTYPE_ASCII_COLON            58 //< ASCII value of ':'
#define WEBDAV_CTYPE_ASCII_UNDERSCORE       95 //< ASCII value of '_'
#define WEBDAV_CTYPE_ASCII_TAB               9 //< ASCII value of a tab
#define WEBDAV_CTYPE_ASCII_LINEFEED         10 //< ASCII value of a line feed
#define WEBDAV_CTYPE_ASCII_CARRIAGERETURN   13
//< ASCII value of a carriage return
#define WEBDAV_CTYPE_ASCII_DELETE          127
//< ASCII value of a delete character

/*
 * Compare two characters a and b, treating case-equivalent
 * unaccented Latin (ASCII subset) letters as equal.
 *
 * Returns in integer greater than, equal to, or less than 0,
 * according to whether a is considered greater than, equal to,
 * or less than b.
 */

static int
ctype_casecmp(int a, int b);

//------------------------------------------------------------------------------
// from svn_string.c

static APR_INLINE bool
string_compare(
  const char * str1,
  const char * str2,
  apr_size_t len1,
  apr_size_t len2)
{
  // easy way out :)
  if (len1 != len2)
    return FALSE;

  // now the strings must have identical lengths

  if ((memcmp(str1, str2, len1)) == 0)
    return TRUE;
  else
    return FALSE;
}

// Our own realloc, since APR doesn't have one.  Note: this is a
// generic realloc for memory pools, *not* for strings.
static void *
my_realloc(
  char * data,
  apr_size_t oldsize,
  apr_size_t request,
  apr_pool_t * pool)
{
  void * new_area = NULL;

  // todo: it's a pity APR doesn't give us this -- sometimes it
  // could realloc the block merely by extending in place, sparing us
  // a memcpy(), but only the pool would know enough to be able to do
  // this.  We should add a realloc() to APR if someone hasn't
  // already.

  // malloc new area
  new_area = apr_pcalloc(pool, request);

  // copy data to new area
  memcpy(new_area, data, oldsize);

  // I'm NOT freeing old area here -- cuz we're using pools, ugh.

  // return new area
  return new_area;
}

// string functions

static stringbuf_t *
stringbuf_create_ensure(
  apr_size_t blocksize,
  apr_pool_t * pool)
{
  void * mem = NULL;
  stringbuf_t * new_string = NULL;

  // apr_pcalloc will allocate multiples of 8.
  // Thus, we would waste some of that memory if we stuck to the
  // smaller size. Note that this is safe even if apr_pcalloc would
  // use some other alignment or none at all.

  ++blocksize; // + space for '\0'
  blocksize = APR_ALIGN_DEFAULT(blocksize);

  // Allocate memory for string_t and data in one chunk.
  mem = apr_pcalloc(pool, sizeof(*new_string) + blocksize);

  // Initialize header and string
  new_string = static_cast<stringbuf_t *>(mem);

  new_string->data = (char *)mem + sizeof(*new_string);
  new_string->data[0] = '\0';
  new_string->len = 0;
  new_string->blocksize = blocksize;
  new_string->pool = pool;

  return new_string;
}

static stringbuf_t *
stringbuf_ncreate(
  const char * bytes,
  apr_size_t size,
  apr_pool_t * pool)
{
  stringbuf_t * strbuf = stringbuf_create_ensure(size, pool);
  memcpy(strbuf->data, bytes, size);

  // Null termination is the convention -- even if we suspect the data
  // to be binary, it's not up to us to decide, it's the caller's
  // call.  Heck, that's why they call it the caller!
  strbuf->data[size] = '\0';
  strbuf->len = size;

  return strbuf;
}

static stringbuf_t *
stringbuf_create(
  const char * cstring,
  apr_pool_t * pool)
{
  return stringbuf_ncreate(cstring, strlen(cstring), pool);
}

static void
stringbuf_ensure(
  stringbuf_t * str,
  apr_size_t minimum_size)
{
  // Keep doubling capacity until have enough.
  if (str->blocksize < minimum_size)
  {
    if (str->blocksize == 0)
      // APR will increase odd allocation sizes to the next
      // multiple for 8, for instance. Take advantage of that
      // knowledge and allow for the extra size to be used.
      str->blocksize = APR_ALIGN_DEFAULT(minimum_size);
    else
      while (str->blocksize < minimum_size)
      {
        // str->blocksize is aligned;
        // doubling it should keep it aligned
        apr_size_t prev_size = str->blocksize;
        str->blocksize *= 2;

        // check for apr_size_t overflow
        if (prev_size > str->blocksize)
        {
          str->blocksize = minimum_size;
          break;
        }
      }

    str->data = (char *) my_realloc(str->data,
      str->len + 1,
      // We need to maintain (and thus copy)
      // the trailing null
      str->blocksize,
      str->pool);
  }
}

static void
stringbuf_set(
  stringbuf_t * str,
  const char * value)
{
  apr_size_t amt = strlen(value);

  stringbuf_ensure(str, amt + 1);
  memcpy(str->data, value, amt + 1);
  str->len = amt;
}

static void
stringbuf_setempty(
  stringbuf_t * str)
{
  if (str->len > 0)
    str->data[0] = '\0';

  str->len = 0;
}

static bool
stringbuf_isempty(
  const stringbuf_t * str)
{
  return (str->len == 0);
}

// Return a new string_t object, allocated in POOL, initialized with
// DATA and SIZE.  Do not copy the contents of DATA, just store the pointer.
// SIZE is the length in bytes of DATA, excluding the required NUL
// terminator.
static string_t *
create_string(
  const char * data,
  apr_size_t size,
  apr_pool_t * pool)
{
  string_t * new_string;

  new_string = static_cast<string_t *>(apr_pcalloc(pool, sizeof(*new_string)));

  new_string->data = data;
  new_string->len = size;

  return new_string;
}

static string_t *
string_ncreate(
  const char * bytes,
  apr_size_t size,
  apr_pool_t * pool)
{
  void * mem = NULL;
  char * data = NULL;
  string_t * new_string;

  // Allocate memory for string_t and data in one chunk.
  mem = apr_pcalloc(pool, sizeof(*new_string) + size + 1);
  data = (char *)mem + sizeof(*new_string);

  new_string = static_cast<string_t *>(mem);
  new_string->data = data;
  new_string->len = size;

  memcpy(data, bytes, size);

  // Null termination is the convention -- even if we suspect the data
  // to be binary, it's not up to us to decide, it's the caller's
  // call.  Heck, that's why they call it the caller!
  data[size] = '\0';

  return new_string;
}

static string_t *
string_create(
  const char * cstring,
  apr_pool_t * pool)
{
  return string_ncreate(cstring, strlen(cstring), pool);
}

static string_t *
string_createv(
  apr_pool_t * pool,
  const char * fmt,
  va_list ap)
{
  char * data = apr_pvsprintf(pool, fmt, ap);

  // wrap an string_t around the new data
  return create_string(data, strlen(data), pool);
}

static string_t *
string_createf(
  apr_pool_t * pool,
  const char * fmt,
  ...)
{
  string_t * str = NULL;

  va_list ap;
  va_start(ap, fmt);
  str = string_createv(pool, fmt, ap);
  va_end(ap);

  return str;
}

static bool
string_compare(
  const string_t * str1,
  const string_t * str2)
{
  return
    string_compare(str1->data, str2->data, str1->len, str2->len);
}

static void
stringbuf_appendbytes(
  stringbuf_t * str,
  const char * bytes,
  apr_size_t count)
{
  apr_size_t total_len = 0;
  void * start_address = NULL;

  total_len = str->len + count;  // total size needed

  // +1 for null terminator.
  stringbuf_ensure(str, (total_len + 1));

  // get address 1 byte beyond end of original bytestring
  start_address = (str->data + str->len);

  memcpy(start_address, bytes, count);
  str->len = total_len;

  str->data[str->len] = '\0';  // We don't know if this is binary
                               // data or not, but convention is
                               // to null-terminate.
}

static void
stringbuf_appendstr(
  stringbuf_t * targetstr,
  const stringbuf_t * appendstr)
{
  stringbuf_appendbytes(targetstr, appendstr->data, appendstr->len);
}

static void
stringbuf_appendcstr(
  stringbuf_t * targetstr,
  const char * cstr)
{
  stringbuf_appendbytes(targetstr, cstr, strlen(cstr));
}

static error_t
cstring_strtoi64(
  apr_int64_t * n,
  const char * str,
  apr_int64_t minval,
  apr_int64_t maxval,
  int base)
{
  apr_int64_t val = 0;
  char * endptr = NULL;

  // We assume errno is thread-safe.
  errno = 0; // APR-0.9 doesn't always set errno

  val = apr_strtoi64(str, &endptr, base);
  if (errno == EINVAL || endptr == str || str[0] == '\0' || *endptr != '\0')
    return error_createf(WEBDAV_ERR_INCORRECT_PARAMS, NULL,
      "Could not convert '%s' into a number",
      str);
  if ((errno == ERANGE && (val == APR_INT64_MIN || val == APR_INT64_MAX)) ||
      val < minval || val > maxval)
    // Mark this for translation when gettext doesn't choke on macros.
    return error_createf(WEBDAV_ERR_INCORRECT_PARAMS, NULL,
      "Number '%s' is out of range "
      "'[%" APR_INT64_T_FMT ", %" APR_INT64_T_FMT "]'",
      str, minval, maxval);
  *n = val;
  return WEBDAV_NO_ERROR;
}

static error_t
cstring_atoi64(
  apr_int64_t * n,
  const char * str)
{
  return error_trace(cstring_strtoi64(n, str, APR_INT64_MIN,
    APR_INT64_MAX, 10));
}

static int
cstring_casecmp(
  const char * str1,
  const char * str2)
{
  for (;;)
  {
    const int a = *str1++;
    const int b = *str2++;
    const int cmp = ctype_casecmp(a, b);
    if (cmp || !a || !b)
      return cmp;
  }
}

static stringbuf_t *
create_stringbuf(
  char * data,
  apr_size_t size,
  apr_size_t blocksize,
  apr_pool_t * pool)
{
  stringbuf_t * new_string;

  new_string = static_cast<stringbuf_t *>(apr_pcalloc(pool, sizeof(*new_string)));

  new_string->data = data;
  new_string->len = size;
  new_string->blocksize = blocksize;
  new_string->pool = pool;

  return new_string;
}

static stringbuf_t *
stringbuf_createv(
  apr_pool_t * pool,
  const char * fmt,
  va_list ap)
{
  char * data = apr_pvsprintf(pool, fmt, ap);
  apr_size_t size = strlen(data);

  // wrap an stringbuf_t around the new data
  return create_stringbuf(data, size, size + 1, pool);
}

static stringbuf_t *
stringbuf_createf(
  apr_pool_t * pool,
  const char * fmt,
  ...)
{
  stringbuf_t * str = NULL;

  va_list ap;
  va_start(ap, fmt);
  str = stringbuf_createv(pool, fmt, ap);
  va_end(ap);

  return str;
}

static void
cstring_split_append(
  apr_array_header_t * array,
  const char * input,
  const char * sep_chars,
  bool chop_whitespace,
  apr_pool_t * pool)
{
  char * last = NULL;

  char * pats = apr_pstrdup(pool, input); // strtok wants non-const data
  char * p = apr_strtok(pats, sep_chars, &last);

  while (p)
  {
    if (chop_whitespace)
    {
      while (ctype_isspace(*p))
        p++;

      {
        char * e = p + (strlen(p) - 1);
        while ((e >= p) && (ctype_isspace(*e)))
          e--;
        *(++e) = '\0';
      }
    }

    if (p[0] != '\0')
      APR_ARRAY_PUSH(array, const char *) = p;

    p = apr_strtok(NULL, sep_chars, &last);
  }

  return;
}

static apr_array_header_t *
cstring_split(
  const char * input,
  const char * sep_chars,
  bool chop_whitespace,
  apr_pool_t * pool)
{
  apr_array_header_t * a = apr_array_make(pool, 5, sizeof(input));
  cstring_split_append(a, input, sep_chars, chop_whitespace, pool);
  return a;
}

//------------------------------------------------------------------------------
// from ctype.c
static const apr_uint32_t ctype_table_internal[256] =
{
  // **** DO NOT EDIT! ****
  // This table was generated by genctype.py, make changes there.
  /* nul */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* soh */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* stx */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* etx */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* eot */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* enq */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* ack */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* bel */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* bs  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* ht  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL | WEBDAV_CTYPE_SPACE,
  /* nl  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL | WEBDAV_CTYPE_SPACE,
  /* vt  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL | WEBDAV_CTYPE_SPACE,
  /* np  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL | WEBDAV_CTYPE_SPACE,
  /* cr  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL | WEBDAV_CTYPE_SPACE,
  /* so  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* si  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* dle */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* dc1 */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* dc2 */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* dc3 */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* dc4 */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* nak */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* syn */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* etb */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* can */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* em  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* sub */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* esc */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* fs  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* gs  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* rs  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* us  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* sp  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_SPACE,
  /*  !  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  "  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  #  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  $  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  %  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  &  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  '  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  (  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  )  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  *  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  +  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  ,  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  -  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  .  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  /  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  0  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_DIGIT,
  /*  1  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_DIGIT,
  /*  2  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_DIGIT,
  /*  3  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_DIGIT,
  /*  4  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_DIGIT,
  /*  5  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_DIGIT,
  /*  6  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_DIGIT,
  /*  7  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_DIGIT,
  /*  8  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_DIGIT,
  /*  9  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_DIGIT,
  /*  :  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  ;  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  <  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  =  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  >  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  ?  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  @  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  A  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER | WEBDAV_CTYPE_XALPHA,
  /*  B  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER | WEBDAV_CTYPE_XALPHA,
  /*  C  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER | WEBDAV_CTYPE_XALPHA,
  /*  D  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER | WEBDAV_CTYPE_XALPHA,
  /*  E  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER | WEBDAV_CTYPE_XALPHA,
  /*  F  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER | WEBDAV_CTYPE_XALPHA,
  /*  G  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  H  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  I  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  J  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  K  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  L  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  M  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  N  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  O  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  P  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  Q  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  R  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  S  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  T  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  U  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  V  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  W  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  X  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  Y  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  Z  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_UPPER,
  /*  [  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  \  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  ]  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  ^  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  _  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  `  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  a  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER | WEBDAV_CTYPE_XALPHA,
  /*  b  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER | WEBDAV_CTYPE_XALPHA,
  /*  c  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER | WEBDAV_CTYPE_XALPHA,
  /*  d  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER | WEBDAV_CTYPE_XALPHA,
  /*  e  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER | WEBDAV_CTYPE_XALPHA,
  /*  f  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER | WEBDAV_CTYPE_XALPHA,
  /*  g  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  h  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  i  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  j  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  k  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  l  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  m  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  n  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  o  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  p  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  q  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  r  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  s  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  t  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  u  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  v  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  w  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  x  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  y  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  z  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_LOWER,
  /*  {  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  |  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  }  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /*  ~  */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_PUNCT,
  /* del */ WEBDAV_CTYPE_ASCII | WEBDAV_CTYPE_CNTRL,
  /* x80 */ WEBDAV_CTYPE_UTF8CONT,
  /* x81 */ WEBDAV_CTYPE_UTF8CONT,
  /* x82 */ WEBDAV_CTYPE_UTF8CONT,
  /* x83 */ WEBDAV_CTYPE_UTF8CONT,
  /* x84 */ WEBDAV_CTYPE_UTF8CONT,
  /* x85 */ WEBDAV_CTYPE_UTF8CONT,
  /* x86 */ WEBDAV_CTYPE_UTF8CONT,
  /* x87 */ WEBDAV_CTYPE_UTF8CONT,
  /* x88 */ WEBDAV_CTYPE_UTF8CONT,
  /* x89 */ WEBDAV_CTYPE_UTF8CONT,
  /* x8a */ WEBDAV_CTYPE_UTF8CONT,
  /* x8b */ WEBDAV_CTYPE_UTF8CONT,
  /* x8c */ WEBDAV_CTYPE_UTF8CONT,
  /* x8d */ WEBDAV_CTYPE_UTF8CONT,
  /* x8e */ WEBDAV_CTYPE_UTF8CONT,
  /* x8f */ WEBDAV_CTYPE_UTF8CONT,
  /* x90 */ WEBDAV_CTYPE_UTF8CONT,
  /* x91 */ WEBDAV_CTYPE_UTF8CONT,
  /* x92 */ WEBDAV_CTYPE_UTF8CONT,
  /* x93 */ WEBDAV_CTYPE_UTF8CONT,
  /* x94 */ WEBDAV_CTYPE_UTF8CONT,
  /* x95 */ WEBDAV_CTYPE_UTF8CONT,
  /* x96 */ WEBDAV_CTYPE_UTF8CONT,
  /* x97 */ WEBDAV_CTYPE_UTF8CONT,
  /* x98 */ WEBDAV_CTYPE_UTF8CONT,
  /* x99 */ WEBDAV_CTYPE_UTF8CONT,
  /* x9a */ WEBDAV_CTYPE_UTF8CONT,
  /* x9b */ WEBDAV_CTYPE_UTF8CONT,
  /* x9c */ WEBDAV_CTYPE_UTF8CONT,
  /* x9d */ WEBDAV_CTYPE_UTF8CONT,
  /* x9e */ WEBDAV_CTYPE_UTF8CONT,
  /* x9f */ WEBDAV_CTYPE_UTF8CONT,
  /* xa0 */ WEBDAV_CTYPE_UTF8CONT,
  /* xa1 */ WEBDAV_CTYPE_UTF8CONT,
  /* xa2 */ WEBDAV_CTYPE_UTF8CONT,
  /* xa3 */ WEBDAV_CTYPE_UTF8CONT,
  /* xa4 */ WEBDAV_CTYPE_UTF8CONT,
  /* xa5 */ WEBDAV_CTYPE_UTF8CONT,
  /* xa6 */ WEBDAV_CTYPE_UTF8CONT,
  /* xa7 */ WEBDAV_CTYPE_UTF8CONT,
  /* xa8 */ WEBDAV_CTYPE_UTF8CONT,
  /* xa9 */ WEBDAV_CTYPE_UTF8CONT,
  /* xaa */ WEBDAV_CTYPE_UTF8CONT,
  /* xab */ WEBDAV_CTYPE_UTF8CONT,
  /* xac */ WEBDAV_CTYPE_UTF8CONT,
  /* xad */ WEBDAV_CTYPE_UTF8CONT,
  /* xae */ WEBDAV_CTYPE_UTF8CONT,
  /* xaf */ WEBDAV_CTYPE_UTF8CONT,
  /* xb0 */ WEBDAV_CTYPE_UTF8CONT,
  /* xb1 */ WEBDAV_CTYPE_UTF8CONT,
  /* xb2 */ WEBDAV_CTYPE_UTF8CONT,
  /* xb3 */ WEBDAV_CTYPE_UTF8CONT,
  /* xb4 */ WEBDAV_CTYPE_UTF8CONT,
  /* xb5 */ WEBDAV_CTYPE_UTF8CONT,
  /* xb6 */ WEBDAV_CTYPE_UTF8CONT,
  /* xb7 */ WEBDAV_CTYPE_UTF8CONT,
  /* xb8 */ WEBDAV_CTYPE_UTF8CONT,
  /* xb9 */ WEBDAV_CTYPE_UTF8CONT,
  /* xba */ WEBDAV_CTYPE_UTF8CONT,
  /* xbb */ WEBDAV_CTYPE_UTF8CONT,
  /* xbc */ WEBDAV_CTYPE_UTF8CONT,
  /* xbd */ WEBDAV_CTYPE_UTF8CONT,
  /* xbe */ WEBDAV_CTYPE_UTF8CONT,
  /* xbf */ WEBDAV_CTYPE_UTF8CONT,
  /* xc0 */ 0,
  /* xc1 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xc2 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xc3 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xc4 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xc5 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xc6 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xc7 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xc8 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xc9 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xca */ WEBDAV_CTYPE_UTF8LEAD,
  /* xcb */ WEBDAV_CTYPE_UTF8LEAD,
  /* xcc */ WEBDAV_CTYPE_UTF8LEAD,
  /* xcd */ WEBDAV_CTYPE_UTF8LEAD,
  /* xce */ WEBDAV_CTYPE_UTF8LEAD,
  /* xcf */ WEBDAV_CTYPE_UTF8LEAD,
  /* xd0 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xd1 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xd2 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xd3 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xd4 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xd5 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xd6 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xd7 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xd8 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xd9 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xda */ WEBDAV_CTYPE_UTF8LEAD,
  /* xdb */ WEBDAV_CTYPE_UTF8LEAD,
  /* xdc */ WEBDAV_CTYPE_UTF8LEAD,
  /* xdd */ WEBDAV_CTYPE_UTF8LEAD,
  /* xde */ WEBDAV_CTYPE_UTF8LEAD,
  /* xdf */ WEBDAV_CTYPE_UTF8LEAD,
  /* xe0 */ 0,
  /* xe1 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xe2 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xe3 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xe4 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xe5 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xe6 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xe7 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xe8 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xe9 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xea */ WEBDAV_CTYPE_UTF8LEAD,
  /* xeb */ WEBDAV_CTYPE_UTF8LEAD,
  /* xec */ WEBDAV_CTYPE_UTF8LEAD,
  /* xed */ WEBDAV_CTYPE_UTF8LEAD,
  /* xee */ WEBDAV_CTYPE_UTF8LEAD,
  /* xef */ WEBDAV_CTYPE_UTF8LEAD,
  /* xf0 */ 0,
  /* xf1 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xf2 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xf3 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xf4 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xf5 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xf6 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xf7 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xf8 */ 0,
  /* xf9 */ WEBDAV_CTYPE_UTF8LEAD,
  /* xfa */ WEBDAV_CTYPE_UTF8LEAD,
  /* xfb */ WEBDAV_CTYPE_UTF8LEAD,
  /* xfc */ 0,
  /* xfd */ WEBDAV_CTYPE_UTF8LEAD,
  /* xfe */ 0,
  /* xff */ 0
};

const apr_uint32_t * const ctype_table = ctype_table_internal;

static const unsigned char casefold_table[256] =
{
  // Identity, except {97:122} => {65:90}
  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
  32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
  64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
  80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
  96, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
  80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,123,124,125,126,127,
  128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,
  144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,
  160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,
  176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,
  192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,
  208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,
  224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,
  240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255
};

static int
ctype_casecmp(int a, int b)
{
  const int A = casefold_table[(unsigned char)a];
  const int B = casefold_table[(unsigned char)b];
  return A - B;
}

//------------------------------------------------------------------------------
// from svn_pool.c

// Pool allocation handler which just aborts, since we aren't generally
// prepared to deal with out-of-memory errors.

static int
abort_on_pool_failure(int retcode)
{
  // Don't translate this string! It requires memory allocation to do so!
  // And we don't have any of it...
  printf("Out of memory - terminating application.\n");
  abort();
}

static apr_pool_t *
pool_create_ex(
  apr_pool_t * parent_pool,
  apr_allocator_t * allocator)
{
  apr_pool_t * pool;
  apr_pool_create_ex(&pool, parent_pool, abort_on_pool_failure, allocator);
  return pool;
}

//------------------------------------------------------------------------------
// from time.c

#define OLD_TIMESTAMP_FORMAT \
        "%3s %d %3s %d %02d:%02d:%02d.%06d (day %03d, dst %d, gmt_off %06d)"

static apr_size_t
find_matching_string(
  char * str,
  apr_size_t size,
  const char strings[][4])
{
  for (apr_size_t i = 0; i < size; i++)
    if (strings[i] && (strcmp(str, strings[i]) == 0))
      return i;

  return (apr_size_t)-1;
}

static error_t
time_from_cstring(
  apr_time_t * when,
  const char * data,
  apr_pool_t * pool)
{
  apr_time_exp_t exploded_time;
  apr_status_t apr_err = 0;
  char wday[4] = {0}, month[4] = {0};
  char * c = NULL;

  // Open-code parsing of the new timestamp format, as this
  // is a hot path for reading the entries file.  This format looks
  // like:  "2001-08-31T04:24:14.966996Z"
  exploded_time.tm_year = strtol(data, &c, 10);
  if (*c++ != '-') goto fail;
  exploded_time.tm_mon = strtol(c, &c, 10);
  if (*c++ != '-') goto fail;
  exploded_time.tm_mday = strtol(c, &c, 10);
  if (*c++ != 'T') goto fail;
  exploded_time.tm_hour = strtol(c, &c, 10);
  if (*c++ != ':') goto fail;
  exploded_time.tm_min = strtol(c, &c, 10);
  if (*c++ != ':') goto fail;
  exploded_time.tm_sec = strtol(c, &c, 10);
  if (*c != 'Z')
  {
    if (*c++ != '.') goto fail;
    exploded_time.tm_usec = strtol(c, &c, 10);
    if (*c++ != 'Z') goto fail;
  }

  exploded_time.tm_year  -= 1900;
  exploded_time.tm_mon   -= 1;
  exploded_time.tm_wday   = 0;
  exploded_time.tm_yday   = 0;
  exploded_time.tm_isdst  = 0;
  exploded_time.tm_gmtoff = 0;

  apr_err = apr_time_exp_gmt_get(when, &exploded_time);
  if (apr_err == APR_SUCCESS)
    return WEBDAV_NO_ERROR;

  return error_createf(WEBDAV_ERR_BAD_DATE, NULL, "error parsing date: %s", data);

fail:

  // 2012-09-11T14:18:40+07:00
  char gmt_shift = 0;
  int gmt_hour = 0;
  int gmt_min = 0;
  if (sscanf(data,
        "%04d-%02d-%02dT%02d:%02d:%02d%c%02d:%02d",
        &exploded_time.tm_year,
        &exploded_time.tm_mon,
        &exploded_time.tm_mday,
        &exploded_time.tm_hour,
        &exploded_time.tm_min,
        &exploded_time.tm_sec,
        &gmt_shift,
        &gmt_hour,
        &gmt_min) == 9)
  {
    exploded_time.tm_year -= 1900;
    exploded_time.tm_mon   -= 1;
    exploded_time.tm_wday   = 0;
    exploded_time.tm_yday   = 0;
    exploded_time.tm_isdst  = 0;
    exploded_time.tm_gmtoff = (gmt_shift == '-' ? -1 : 1) * gmt_hour * SecsPerHour + gmt_min * SecsPerMin;
    exploded_time.tm_usec = 0;

    apr_err = apr_time_exp_gmt_get(when, &exploded_time);
    if (apr_err != APR_SUCCESS)
      return error_createf(WEBDAV_ERR_BAD_DATE, NULL, "error parsing date: %s", data);

    return WEBDAV_NO_ERROR;
  }

  // 2012-09-11T03:56:20
  if (sscanf(data,
        "%04d-%02d-%02dT%02d:%02d:%02d",
        &exploded_time.tm_year,
        &exploded_time.tm_mon,
        &exploded_time.tm_mday,
        &exploded_time.tm_hour,
        &exploded_time.tm_min,
        &exploded_time.tm_sec) == 6)
  {
    exploded_time.tm_year -= 1900;
    exploded_time.tm_mon   -= 1;
    exploded_time.tm_wday   = 0;
    exploded_time.tm_yday   = 0;
    exploded_time.tm_isdst  = 0;
    exploded_time.tm_gmtoff = 0;
    exploded_time.tm_usec = 0;

    apr_err = apr_time_exp_gmt_get(when, &exploded_time);
    if (apr_err != APR_SUCCESS)
      return error_createf(WEBDAV_ERR_BAD_DATE, NULL, "error parsing date: %s", data);

    return WEBDAV_NO_ERROR;
  }

  // Try the compatibility option.  This does not need to be fast,
  // as this format is no longer generated and the client will convert
  // an old-format entries file the first time it reads it.
  if (sscanf(data,
            OLD_TIMESTAMP_FORMAT,
            wday,
            &exploded_time.tm_mday,
            month,
            &exploded_time.tm_year,
            &exploded_time.tm_hour,
            &exploded_time.tm_min,
            &exploded_time.tm_sec,
            &exploded_time.tm_usec,
            &exploded_time.tm_yday,
            &exploded_time.tm_isdst,
            &exploded_time.tm_gmtoff) == 11)
  {
    exploded_time.tm_year -= 1900;
    exploded_time.tm_yday -= 1;
    // Using hard coded limits for the arrays - they are going away
    // soon in any case.
    exploded_time.tm_wday = (apr_int32_t)find_matching_string(wday, 7, apr_day_snames);
    exploded_time.tm_mon = (apr_int32_t)find_matching_string(month, 12, apr_month_snames);

    apr_err = apr_time_exp_gmt_get(when, &exploded_time);
    if (apr_err != APR_SUCCESS)
      return error_createf(WEBDAV_ERR_BAD_DATE, NULL, "error parsing date: %s", data);

    return WEBDAV_NO_ERROR;
  }

  // Timestamp is something we do not recognize.
  // return error_createf(WEBDAV_ERR_BAD_DATE, NULL, "error parsing date: %s", data);
  *when = apr_time_now();
  return WEBDAV_NO_ERROR;
}

//------------------------------------------------------------------------------
// from path.c

// TRUE if s is the canonical empty path, FALSE otherwise
#define WEBDAV_PATH_IS_EMPTY(s) ((s)[0] == '\0')

// TRUE if s,n is the platform's empty path ("."), FALSE otherwise. Can
// this be changed?  Well, the path library will work, not so sure about
// the OS!
#define WEBDAV_PATH_IS_PLATFORM_EMPTY(s,n) ((n) == 1 && (s)[0] == '.')

/* Here is the BNF for path components in a URI. "pchar" is a
   character in a path component.

      pchar       = unreserved | escaped |
                    ":" | "@" | "&" | "=" | "+" | "$" | ","
      unreserved  = alphanum | mark
      mark        = "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"

   Note that "escaped" doesn't really apply to what users can put in
   their paths, so that really means the set of characters is:

      alphanum | mark | ":" | "@" | "&" | "=" | "+" | "$" | ","
*/
static const char uri_char_validity[256] =
{
  0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 1, 0, 1, 1,   1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,   1, 1, 1, 0, 0, 1, 0, 0,

  // 64
  1, 1, 1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,   1, 1, 1, 0, 0, 0, 0, 1,
  0, 1, 1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,   1, 1, 1, 0, 0, 0, 1, 0,

  // 128
  0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,

  // 192
  0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
};

// URI-encode each character c in PATH for which TABLE[c] is 0.
// If no encoding was needed, return PATH, else return a new string allocated
// in POOL.
static const char *
uri_escape(
  const char * path,
  const char table[],
  apr_pool_t * pool)
{
  stringbuf_t * retstr = NULL;
  size_t i = 0, copied = 0;
  int c = 0;

  retstr = stringbuf_create_ensure(strlen(path), pool);
  for (i = 0; path[i]; i++)
  {
    c = (unsigned char)path[i];
    if (table[c])
      continue;

    // If we got here, we're looking at a character that isn't
    // supported by the (or at least, our) URI encoding scheme.  We
    // need to escape this character.

    // First things first, copy all the good stuff that we haven't
    // yet copied into our output buffer.
    if (i - copied)
      stringbuf_appendbytes(retstr, path + copied,
        i - copied);

    // Now, write in our escaped character, consisting of the
    // '%' and two digits.  We cast the C to unsigned char here because
    // the 'X' format character will be tempted to treat it as an unsigned
    // int...which causes problem when messing with 0x80-0xFF chars.
    // We also need space for a null as apr_snprintf will write one.
    stringbuf_ensure(retstr, retstr->len + 4);
    apr_snprintf(retstr->data + retstr->len, 4, "%%%02X", (unsigned char)c);
    retstr->len += 3;

    // Finally, update our copy counter.
    copied = i + 1;
  }

  // If we didn't encode anything, we don't need to duplicate the string.
  if (retstr->len == 0)
    return path;

  // Anything left to copy?
  if (i - copied)
    stringbuf_appendbytes(retstr, path + copied, i - copied);

  // retstr is null-terminated either by apr_snprintf or the stringbuf
  // functions.

  return retstr->data;
}

static const char *
path_uri_decode(
  const char * path,
  apr_pool_t * pool)
{
  stringbuf_t * retstr = NULL;
  size_t i = 0;
  bool query_start = FALSE;

  // avoid repeated realloc
  retstr = stringbuf_create_ensure(strlen(path) + 1, pool);

  retstr->len = 0;
  for (i = 0; path[i]; i++)
  {
    char c = path[i];

    if (c == '?')
    {
      // Mark the start of the query string, if it exists.
      query_start = TRUE;
    }
    else if (c == '+' && query_start)
    {
      // Only do this if we are into the query string.
      // RFC 2396, section 3.3
      c = ' ';
    }
    else if (c == '%' && ctype_isxdigit(path[i + 1]) && ctype_isxdigit(path[i+2]))
    {
      char digitz[3];
      digitz[0] = path[++i];
      digitz[1] = path[++i];
      digitz[2] = '\0';
      c = (char)(strtol(digitz, NULL, 16));
    }

    retstr->data[retstr->len++] = c;
  }

  // Null-terminate this bad-boy.
  retstr->data[retstr->len] = 0;

  return retstr->data;
}

static const char *
path_uri_encode(
  const char * path,
  apr_pool_t * pool)
{
  const char * ret = uri_escape(path, uri_char_validity, pool);

  // Our interface guarantees a copy.
  if (ret == path)
    return apr_pstrdup(pool, path);
  else
    return ret;
}

static char *
path_join(
  const char * base,
  const char * component,
  apr_pool_t * pool)
{
  apr_size_t blen = strlen(base);
  apr_size_t clen = strlen(component);
  char * path = NULL;

  // assert(path_is_canonical(base, pool));
  // assert(path_is_canonical(component, pool));

  // If the component is absolute, then return it.
  if (*component == '/')
    return (char *)apr_pmemdup(pool, component, clen + 1);

  // If either is empty return the other
  if (WEBDAV_PATH_IS_EMPTY(base))
    return (char *)apr_pmemdup(pool, component, clen + 1);
  if (WEBDAV_PATH_IS_EMPTY(component))
    return (char *)apr_pmemdup(pool, base, blen + 1);

  if ((blen == 1) && (base[0] == '/'))
    blen = 0; // Ignore base, just return separator + component

  // Construct the new, combined path.
  path = (char *)apr_pcalloc(pool, blen + 1 + clen + 1);
  memcpy(path, base, blen);
  path[blen] = '/';
  memcpy(path + blen + 1, component, clen + 1);

  return path;
}

static const char *
path_url_add_component2(
  const char * url,
  const char * component,
  apr_pool_t * pool)
{
  // = path_uri_encode() but without always copying
  component = uri_escape(component, uri_char_validity, pool);

  return path_join(url, component, pool);
}

// Get APR's internal path encoding.
static error_t
get_path_encoding(
  bool * path_is_utf8,
  apr_pool_t * pool)
{
  apr_status_t apr_err = 0;
  int encoding_style = 0;

  apr_err = apr_filepath_encoding(&encoding_style, pool);
  if (apr_err)
    return error_wrap_apr(apr_err,
      "Can't determine the native path encoding");

  // What to do about APR_FILEPATH_ENCODING_UNKNOWN?
  // Well, for now we'll just punt to the utf_ functions;
  // those will at least do the ASCII-subset check.
  *path_is_utf8 = (encoding_style == APR_FILEPATH_ENCODING_UTF8);
  return WEBDAV_NO_ERROR;
}

static error_t
path_cstring_to_utf8(
  const char ** path_utf8,
  const char * path_apr,
  apr_pool_t * pool)
{
  bool path_is_utf8 = FALSE;
  WEBDAV_ERR(get_path_encoding(&path_is_utf8, pool));
  if (path_is_utf8)
  {
    *path_utf8 = apr_pstrdup(pool, path_apr);
    return WEBDAV_NO_ERROR;
  }
  else
    return utf_cstring_to_utf8(path_utf8, path_apr, pool);
}

static apr_size_t
path_component_count(
  const char * path)
{
  if (!path) return 0;
  apr_size_t count = 0;

  while (*path)
  {
    const char * start;

    while (*path == '/')
      ++path;

    start = path;

    while (*path && (*path != '/'))
      ++path;

    if (path != start)
      ++count;
  }

  return count;
}

// Return the length of substring necessary to encompass the entire
// previous path segment in PATH, which should be a LEN byte string.
// A trailing slash will not be included in the returned length except
// in the case in which PATH is absolute and there are no more
// previous segments.

static apr_size_t
previous_segment(
  const char * path,
  apr_size_t len)
{
  if (len == 0)
    return 0;

  while ((len > 0) && (path[--len] != '/'))
    ;

  if ((len == 0) && (path[0] == '/'))
    return 1;
  else
    return len;
}

static void
path_remove_component(
  stringbuf_t * path)
{
  path->len = previous_segment(path->data, path->len);
  path->data[path->len] = '\0';
}

static void
path_remove_components(
  stringbuf_t * path,
  apr_size_t n)
{
  while (n > 0)
  {
    path_remove_component(path);
    n--;
  }
}

static error_t
path_cstring_from_utf8(
  const char ** path_apr,
  const char * path_utf8,
  apr_pool_t * pool)
{
  bool path_is_utf8 = FALSE;
  WEBDAV_ERR(get_path_encoding(&path_is_utf8, pool));
  if (path_is_utf8)
  {
    *path_apr = apr_pstrdup(pool, path_utf8);
    return WEBDAV_NO_ERROR;
  }
  else
    return utf_cstring_from_utf8(path_apr, path_utf8, pool);
}

static bool
path_is_backpath_present(
  const char * path)
{
  size_t len;

  // 0 and 1-length paths do not have a backpath
  if ((path[0] == '\0') || (path[1] == '\0'))
    return FALSE;

  // Handle ".." or a leading "../"
  if ((path[0] == '.') && (path[1] == '.') && ((path[2] == '\0') || (path[2] == '/')))
    return TRUE;

  // Paths of length 2 (at this point) have no backpath present.
  if (path[2] == '\0')
    return FALSE;

  // If any segment is "..", then a backpath is present.
  if (strstr(path, "/../") != NULL)
    return TRUE;

  // Does the path end in "/.." ?
  len = strlen(path);
  return (path[len - 3] == '/') && (path[len - 2] == '.') && (path[len - 1] == '.');
}

//------------------------------------------------------------------------------
// from svn_client.h

typedef error_t (*client_list_func_t)(
  void * baton,
  const char * path,
  const dirent_t * dirent,
  const char * abs_path,
  apr_pool_t * pool);

typedef struct client_ctx_t
{
  auth_baton_t * auth_baton;

  cancel_func_t cancel_func;
  void * cancel_baton;

  progress_notify_func_t progress_func;
  void * progress_baton;
  const char * client_name;
} client_ctx_t;

//------------------------------------------------------------------------------
// from win32_xlate.c

static apr_status_t
subr_win32_xlate_to_stringbuf(// win32_xlate_t *handle,
  const char * src_data,
  apr_size_t src_length,
  stringbuf_t ** dest,
  apr_pool_t * pool)
{
  WCHAR * wide_str = NULL;
  int retval = 0, wide_size = 0;

  if (src_length == 0)
  {
    *dest = stringbuf_create("", pool);
    return APR_SUCCESS;
  }
  int from_page_id = CP_ACP;
  retval = MultiByteToWideChar(from_page_id, // CP_UTF8, // handle->from_page_id,
    0, src_data, (int)src_length, NULL, 0);
  if (retval == 0)
    return apr_get_os_error();

  wide_size = retval;

  // Allocate temporary buffer for small strings on stack instead of heap.
  if (wide_size <= MAX_PATH)
  {
    wide_str = static_cast<WCHAR *>(alloca(wide_size * sizeof(WCHAR)));
  }
  else
  {
    wide_str = static_cast<WCHAR *>(apr_pcalloc(pool, wide_size * sizeof(WCHAR)));
  }

  retval = MultiByteToWideChar(from_page_id, // handle->from_page_id,
    0, src_data, (int)src_length, wide_str, wide_size);

  if (retval == 0)
    return apr_get_os_error();

  int to_page_id = CP_UTF8;
  retval = WideCharToMultiByte(to_page_id, // handle->to_page_id,
    0, wide_str, wide_size, NULL, 0, NULL, NULL);

  if (retval == 0)
    return apr_get_os_error();

  // Ensure that buffer is enough to hold result string and termination
  // character.
  *dest = stringbuf_create_ensure(retval + 1, pool);
  (*dest)->len = retval;

  retval = WideCharToMultiByte(to_page_id, // handle->to_page_id,
    0, wide_str, wide_size, (*dest)->data, (int)(*dest)->len, NULL, NULL);
  if (retval == 0)
    return apr_get_os_error();

  (*dest)->len = retval;
  return APR_SUCCESS;
}

static apr_status_t
utf8_to_unicode(
  WCHAR ** retstr,
  const char * srcstr,
  apr_pool_t * pool)
{
  /*apr_size_t retlen = 0;
  apr_size_t srcremains = strlen(srcstr) + 1;
  *retstr = static_cast<WCHAR *>(apr_pcalloc(pool, srcremains * 4));
  apr_status_t rv;
  if (rv = apr_conv_utf8_to_ucs2(srcstr, &srcremains, *retstr, &retlen)) {
    return (rv == APR_INCOMPLETE) ? APR_EINVAL : rv;
  }
  if (srcremains) {
    return APR_ENAMETOOLONG;
  }*/
  WCHAR * wide_str = NULL;
  int retval = 0, wide_size = 0;
  apr_size_t src_length = strlen(srcstr);

  if (src_length == 0)
  {
    *retstr = L"";
    return APR_SUCCESS;
  }

  retval = MultiByteToWideChar(CP_UTF8, // handle->from_page_id,
    0, srcstr, (int)src_length, NULL, 0);
  if (retval == 0)
    return apr_get_os_error();

  wide_size = retval;

  wide_str = static_cast<WCHAR *>(apr_pcalloc(pool, (wide_size + 1) * sizeof(WCHAR)));

  retval = MultiByteToWideChar(CP_UTF8, // handle->from_page_id,
    0, srcstr, (int)src_length, wide_str, wide_size);

  if (retval == 0)
    return apr_get_os_error();

  *retstr = wide_str;
  (*retstr)[wide_size] = L'\0';
  return APR_SUCCESS;
}

//------------------------------------------------------------------------------
// from utf.c

// Verify that the sequence DATA of length LEN is valid UTF-8.
// If it is not, return an error with code APR_EINVAL.
static error_t
check_utf8(
  const char * data,
  apr_size_t len,
  apr_pool_t * pool)
{
  // TODO: if (!utf_is_valid(data, len)))
    // return invalid_utf8(data, len, pool);
  return WEBDAV_NO_ERROR;
}

// Convert SRC_LENGTH bytes of SRC_DATA in NODE->handle, store the result
// in *DEST, which is allocated in POOL.
static error_t
convert_to_stringbuf(// xlate_handle_node_t *node,
  const char * src_data,
  apr_size_t src_length,
  stringbuf_t ** dest,
  apr_pool_t * pool)
{
  apr_status_t apr_err = 0;

  apr_err = subr_win32_xlate_to_stringbuf(// (win32_xlate_t *) node->handle,
              src_data, src_length,
              dest, pool);

  // If we exited the loop with an error, return the error.
  if (apr_err)
  {
    const char * errstr = NULL;
    error_t err = 0;

    // Can't use error_wrap_apr here because it calls functions in
    // this file, leading to infinite recursion.
    errstr = apr_psprintf(pool, "Can't convert string"); //  from native encoding to '%s':"),

    err = error_create(apr_err, NULL, "");
    return error_create(apr_err, &err, errstr);
  }
  // Else, exited due to success. Trim the result buffer down to the
  // right length.
  (*dest)->data[(*dest)->len] = '\0';

  return WEBDAV_NO_ERROR;
}

// Common implementation for utf_cstring_to_utf8,
// utf_cstring_to_utf8_ex, utf_cstring_from_utf8 and
// utf_cstring_from_utf8_ex. Convert SRC to DEST using NODE->handle as
// the translator and allocating from POOL.
static error_t
convert_cstring(
  const char ** dest,
  const char * src,
  apr_pool_t * pool)
{
  stringbuf_t * destbuf;
  WEBDAV_ERR(convert_to_stringbuf(// node,
    src, strlen(src), &destbuf, pool));
  *dest = destbuf->data;
  return WEBDAV_NO_ERROR;
}

// Verify that the NULL terminated sequence DATA is valid UTF-8.
// If it is not, return an error with code APR_EINVAL.
static error_t
check_cstring_utf8(
  const char * data,
  apr_pool_t * pool)
{

  // TODO: if (!utf_cstring_is_valid(data))
  //   return invalid_utf8(data, strlen(data), pool);
  return WEBDAV_NO_ERROR;
}

static error_t
utf_cstring_to_utf8(
  const char ** dest,
  const char * src,
  apr_pool_t * pool)
{
  // TODO: implement
  error_t err = convert_cstring(dest, src, // node,
    pool);
  WEBDAV_ERR(err);
  return check_cstring_utf8(*dest, pool);
}

static error_t
utf_cstring_from_utf8(
  const char ** dest,
  const char * src,
  apr_pool_t * pool)
{
  WEBDAV_ERR(check_utf8(src, strlen(src), pool));

  error_t err = convert_cstring(dest, src, pool);
  return err;
}

//------------------------------------------------------------------------------
// from xml.c

static const char *
xml_get_attr_value(
  const char * name,
  const char ** atts)
{
  while (atts && (*atts))
  {
    if (strcmp(atts[0], name) == 0)
      return atts[1];
    else
      atts += 2; // continue looping
  }

  // Else no such attribute name seen.
  return NULL;
}

//------------------------------------------------------------------------------
// from apr_base64.c

static const char basis_64[] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

// This is the same as apr_base64_encode() except on EBCDIC machines, where
// the conversion of the input to ascii is left out.

static int
apr_base64_encode_binary(
  char * encoded,
  const unsigned char * string,
  int len)
{
  int i;
  char * p;

  p = encoded;
  for (i = 0; i < len - 2; i += 3)
  {
    *p++ = basis_64[(string[i] >> 2) & 0x3F];
    *p++ = basis_64[((string[i] & 0x3) << 4) |
                    ((int)(string[i + 1] & 0xF0) >> 4)];
    *p++ = basis_64[((string[i + 1] & 0xF) << 2) |
                    ((int)(string[i + 2] & 0xC0) >> 6)];
    *p++ = basis_64[string[i + 2] & 0x3F];
  }
  if (i < len)
  {
    *p++ = basis_64[(string[i] >> 2) & 0x3F];
    if (i == (len - 1))
    {
      *p++ = basis_64[((string[i] & 0x3) << 4)];
      *p++ = '=';
    }
    else
    {
      *p++ = basis_64[((string[i] & 0x3) << 4) |
                      ((int)(string[i + 1] & 0xF0) >> 4)];
      *p++ = basis_64[((string[i + 1] & 0xF) << 2)];
    }
    *p++ = '=';
  }

  *p++ = '\0';
  return (int)(p - encoded);
}

static int
apr_base64_encode(
  char * encoded,
  const char * string,
  int len)
{
  return apr_base64_encode_binary(encoded, (const unsigned char *) string, len);
}

static int
apr_base64_encode_len(int len)
{
  return ((len + 2) / 3 * 4) + 1;
}

// aaaack but it's fast and const should make it shared text page.
static const unsigned char pr2six[256] =
{
  // ASCII table
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 62, 64, 64, 64, 63,
  52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 64, 64, 64, 64, 64, 64,
  64,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
  15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64,
  64, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
  41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64
};

static int
apr_base64_decode_len(
  const char * bufcoded)
{
  int nbytesdecoded;
  register const unsigned char * bufin;
  register apr_size_t nprbytes;

  bufin = (const unsigned char *) bufcoded;
  while (pr2six[*(bufin++)] <= 63);

  nprbytes = (bufin - (const unsigned char *) bufcoded) - 1;
  nbytesdecoded = (((int)nprbytes + 3) / 4) * 3;

  return nbytesdecoded + 1;
}

// This is the same as apr_base64_decode() except on EBCDIC machines, where
// the conversion of the output to ebcdic is left out.

static int
apr_base64_decode_binary(
  unsigned char * bufplain,
  const char * bufcoded)
{
  int nbytesdecoded;
  register const unsigned char * bufin;
  register unsigned char * bufout;
  register apr_size_t nprbytes;

  bufin = (const unsigned char *) bufcoded;
  while (pr2six[*(bufin++)] <= 63);
  nprbytes = (bufin - (const unsigned char *) bufcoded) - 1;
  nbytesdecoded = (((int)nprbytes + 3) / 4) * 3;

  bufout = (unsigned char *) bufplain;
  bufin = (const unsigned char *) bufcoded;

  while (nprbytes > 4)
  {
    *(bufout++) =
      (unsigned char)((pr2six[*bufin] << 2) | (pr2six[bufin[1]] >> 4));
    *(bufout++) =
      (unsigned char)((pr2six[bufin[1]] << 4) | (pr2six[bufin[2]] >> 2));
    *(bufout++) =
      (unsigned char)((pr2six[bufin[2]] << 6) | pr2six[bufin[3]]);
    bufin += 4;
    nprbytes -= 4;
  }

  // Note: (nprbytes == 1) would be an error, so just ignore that case
  if (nprbytes > 1)
  {
    *(bufout++) =
      (unsigned char)((pr2six[*bufin] << 2) | (pr2six[bufin[1]] >> 4));
  }
  if (nprbytes > 2)
  {
    *(bufout++) =
      (unsigned char)((pr2six[bufin[1]] << 4) | (pr2six[bufin[2]] >> 2));
  }
  if (nprbytes > 3)
  {
    *(bufout++) =
      (unsigned char)((pr2six[bufin[2]] << 6) | pr2six[bufin[3]]);
  }

  nbytesdecoded -= (4 - (int)nprbytes) & 3;
  return nbytesdecoded;
}

static int
apr_base64_decode(
  char * bufplain,
  const char * bufcoded)
{
  int len = apr_base64_decode_binary((unsigned char *) bufplain, bufcoded);
  bufplain[len] = '\0';
  return len;
}

//---------------------------------------------------------------------------
// from user.c

// Get the current user's name from the OS
static const char *
get_os_username(
  apr_pool_t * pool)
{
#if APR_HAS_USER
  char * username;
  apr_uid_t uid;
  apr_gid_t gid;

  if (apr_uid_current(&uid, &gid, pool) == APR_SUCCESS &&
      apr_uid_name_get(&username, uid, pool) == APR_SUCCESS)
    return username;
#endif

  return NULL;
}

// Return a UTF8 version of STR, or NULL on error.
// Use POOL for any necessary allocation.
static const char *
utf8_or_nothing(
  const char * str,
  apr_pool_t * pool)
{
  if (str)
  {
    const char * utf8_str;
    error_t err = utf_cstring_to_utf8(&utf8_str, str, pool);
    if (!err)
      return utf8_str;
    error_clear(&err);
  }
  return NULL;
}

static const char *
user_get_name(
  apr_pool_t * pool)
{
  const char * username = get_os_username(pool);
  return utf8_or_nothing(username, pool);
}

//---------------------------------------------------------------------------
// from sorts.c

typedef struct sort_item_t
{
  const void * key;
  apr_ssize_t klen;
  void * value;
} sort_item_t;

static int
sort_compare_items_lexically(
  const sort_item_t * a,
  const sort_item_t * b)
{
  // Compare bytes of a's key and b's key up to the common length.
  apr_size_t len = (a->klen < b->klen) ? a->klen : b->klen;
  int val = memcmp(a->key, b->key, len);
  if (val != 0)
    return val;

  // They match up until one of them ends; whichever is longer is greater.
  return (a->klen < b->klen) ? -1 : (a->klen > b->klen) ? 1 : 0;
}

static apr_array_header_t *
sort_hash(apr_hash_t * ht,
  int (*comparison_func)(const sort_item_t *,
                         const sort_item_t *),
  apr_pool_t * pool)
{
  apr_array_header_t * ary = NULL;
  bool sorted = FALSE;
  sort_item_t * prev_item = NULL;

  // allocate an array with enough elements to hold all the keys.
  ary = apr_array_make(pool, apr_hash_count(ht), sizeof(sort_item_t));

  // loop over hash table and push all keys into the array
  sorted = TRUE;
  prev_item = NULL;
  for (apr_hash_index_t * hi = apr_hash_first(pool, ht); hi;
       hi = apr_hash_next(hi))
  {
    sort_item_t * item = static_cast<sort_item_t *>(apr_array_push(ary));

    apr_hash_this(hi, &item->key, &item->klen, &item->value);

    if (prev_item == NULL)
    {
      prev_item = item;
      continue;
    }

    if (sorted)
    {
      sorted = (comparison_func(prev_item, item) < 0);
      prev_item = item;
    }
  }

  // quicksort the array if it isn't already sorted.
  if (!sorted)
    qsort(ary->elts, ary->nelts, ary->elt_size,
          (int ( *)(const void *, const void *))comparison_func);

  return ary;
}

//---------------------------------------------------------------------------
// from svn_config.h

#define WEBDAV_CONFIG_TRUE  "TRUE"
#define WEBDAV_CONFIG_FALSE "FALSE"
#define WEBDAV_CONFIG_ASK   "ASK"

// Default values for some options. Should be passed as default values
// to config_get and friends, instead of hard-coding the defaults in
// multiple places.
#define WEBDAV_CONFIG_DEFAULT_OPTION_STORE_PASSWORDS            TRUE
#define WEBDAV_CONFIG_DEFAULT_OPTION_STORE_PLAINTEXT_PASSWORDS  WEBDAV_CONFIG_ASK
#define WEBDAV_CONFIG_DEFAULT_OPTION_STORE_AUTH_CREDS           TRUE
#define WEBDAV_CONFIG_DEFAULT_OPTION_STORE_SSL_CLIENT_CERT_PP   TRUE
#define WEBDAV_CONFIG_DEFAULT_OPTION_STORE_SSL_CLIENT_CERT_PP_PLAINTEXT \
                                                             WEBDAV_CONFIG_ASK

//---------------------------------------------------------------------------
// from svn_auth.h

// Certificate is not yet valid.
#define WEBDAV_AUTH_SSL_NOTYETVALID 0x00000001
// Certificate has expired.
#define WEBDAV_AUTH_SSL_EXPIRED     0x00000002
// Certificate's CN (hostname) does not match the remote hostname.
#define WEBDAV_AUTH_SSL_CNMISMATCH  0x00000004
// Certificate authority is unknown (i.e. not trusted)
#define WEBDAV_AUTH_SSL_UNKNOWNCA   0x00000008
// Other failure. This can happen if neon has introduced a new
// failure bit that we do not handle yet.
#define WEBDAV_AUTH_SSL_OTHER       0x40000000

#define AUTH_CRED_SSL_CLIENT_CERT_PW "ssl.client-passphrase"
#define AUTH_CRED_SSL_CLIENT_CERT "ssl.client-cert"

// The auth-hash prefix indicating that the parameter is global.
#define AUTH_PARAM_PREFIX "webdav:auth:"

// The following property is for SSL server cert providers. This
// provides a pointer to an apr_uint32_t containing the failures
// detected by the certificate validator.
#define AUTH_PARAM_SSL_SERVER_FAILURES AUTH_PARAM_PREFIX \
  "ssl:failures"

// The following property is for SSL server cert providers. This
// provides the cert info (auth_ssl_server_cert_info_t).
#define AUTH_PARAM_SSL_SERVER_CERT_INFO AUTH_PARAM_PREFIX \
  "ssl:cert-info"

#define AUTH_CRED_SSL_SERVER_TRUST "webdav.ssl.server"

#define AUTH_CRED_SIMPLE "webdav.simple"
#define WEBDAV_AUTH_CRED_USERNAME "webdav.username"

#define WEBDAV_AUTH_PARAM_DEFAULT_USERNAME  AUTH_PARAM_PREFIX "username"
#define WEBDAV_AUTH_PARAM_DEFAULT_PASSWORD  AUTH_PARAM_PREFIX "password"
#define WEBDAV_AUTH_PARAM_NON_INTERACTIVE  AUTH_PARAM_PREFIX "non-interactive"
#define WEBDAV_AUTH_PARAM_DONT_STORE_PASSWORDS  AUTH_PARAM_PREFIX \
                                                 "dont-store-passwords"
#define WEBDAV_AUTH_PARAM_STORE_PLAINTEXT_PASSWORDS  AUTH_PARAM_PREFIX \
                                                  "store-plaintext-passwords"
#define WEBDAV_AUTH_PARAM_DONT_STORE_SSL_CLIENT_CERT_PP \
  AUTH_PARAM_PREFIX "dont-store-ssl-client-cert-pp"
#define WEBDAV_AUTH_PARAM_STORE_SSL_CLIENT_CERT_PP_PLAINTEXT \
  AUTH_PARAM_PREFIX "store-ssl-client-cert-pp-plaintext"
#define WEBDAV_AUTH_PARAM_NO_AUTH_CACHE  AUTH_PARAM_PREFIX "no-auth-cache"

#define WEBDAV_AUTH_SIMPLE_PASSWORD_TYPE             "simple"
#define WEBDAV_AUTH_WINCRYPT_PASSWORD_TYPE           "wincrypt"

// This effectively defines a single table.  Every provider in this
// array returns the same kind of credentials.
typedef struct provider_set_t
{
  // ordered array of auth_provider_object_t
  apr_array_header_t * providers;

} provider_set_t;

// Abstracted iteration baton
struct auth_iterstate_t
{
  provider_set_t * table;       // the table being searched
  int provider_idx;             // the current provider (row)
  bool got_first;               // did we get the provider's first creds?
  void * provider_iter_baton;   // the provider's own iteration context
  const char * realmstring;     // The original realmstring passed in
  const char * cache_key;       // key to use in auth_baton's creds_cache
  auth_baton_t * auth_baton;    // the original auth_baton.
};

// The main auth baton.
typedef struct auth_baton_t
{
  // a collection of tables.  maps cred_kind -> provider_set
  apr_hash_t * tables;

  // the pool I'm allocated in.
  apr_pool_t * pool;

  // run-time parameters needed by providers.
  apr_hash_t * parameters;

  // run-time credentials cache.
  apr_hash_t * creds_cache;
} auth_baton_t;

// The main authentication "provider" vtable.
typedef struct auth_provider_t
{
  // The kind of credentials this provider knows how to retrieve.
  const char * cred_kind;

  error_t (*first_credentials)(void ** credentials,
                               void ** iter_baton,
                               void * provider_baton,
                               apr_hash_t * parameters,
                               const char * realmstring,
                               apr_pool_t * pool);

  error_t (*next_credentials)(void ** credentials,
                              void * iter_baton,
                              void * provider_baton,
                              apr_hash_t * parameters,
                              const char * realmstring,
                              apr_pool_t * pool);

  error_t (*save_credentials)(bool * saved,
                              void * credentials,
                              void * provider_baton,
                              apr_hash_t * parameters,
                              const char * realmstring,
                              apr_pool_t * pool);

} auth_provider_t;

// A provider object, ready to be put into an array and given to
// create_baton_open().
typedef struct auth_provider_object_t
{
  const auth_provider_t * vtable;
  void * provider_baton;

} auth_provider_object_t;

typedef error_t (*auth_plaintext_prompt_func_t)(
  bool * may_save_plaintext,
  const char * realmstring,
  void * baton,
  apr_pool_t * pool);

typedef error_t (*auth_password_get_t)(
  bool * done,
  const char ** password,
  apr_hash_t * creds,
  const char * realmstring,
  const char * username,
  apr_hash_t * parameters,
  bool non_interactive,
  apr_pool_t * pool);

typedef struct auth_cred_simple_t
{
  // Username
  const char * username;
  // Password
  const char * password;
  bool may_save;
} auth_cred_simple_t;

typedef struct auth_cred_username_t
{
  // Username
  const char * username;
  bool may_save;
} auth_cred_username_t;

typedef error_t (*auth_password_set_t)(
  bool * done,
  apr_hash_t * creds,
  const char * realmstring,
  const char * username,
  const char * password,
  apr_hash_t * parameters,
  bool non_interactive,
  apr_pool_t * pool);

// AUTH_CRED_SSL_CLIENT_CERT credentials.
typedef struct auth_cred_ssl_client_cert_t
{
  // Absolute path to the certificate file
  const char * cert_file;
  bool may_save;
} auth_cred_ssl_client_cert_t;

typedef error_t (*auth_ssl_client_cert_prompt_func_t)(
  auth_cred_ssl_client_cert_t ** cred,
  void * baton,
  const char * realm,
  bool may_save,
  apr_pool_t * pool);

typedef error_t (*auth_plaintext_passphrase_prompt_func_t)(
  bool * may_save_plaintext,
  const char * realmstring,
  void * baton,
  apr_pool_t * pool);

// AUTH_CRED_SSL_CLIENT_CERT_PW credentials.
typedef struct auth_cred_ssl_client_cert_pw_t
{
  // Certificate password
  const char * password;
  bool may_save;
} auth_cred_ssl_client_cert_pw_t;

// AUTH_CRED_SSL_SERVER_TRUST credentials.
typedef struct auth_cred_ssl_server_trust_t
{
  bool may_save;
  // Bit mask of the accepted failures
  apr_uint32_t accepted_failures;
} auth_cred_ssl_server_trust_t;

// SSL server certificate information used by
// AUTH_CRED_SSL_SERVER_TRUST providers.

typedef struct auth_ssl_server_cert_info_t
{
  // Primary CN
  const char * hostname;
  // ASCII fingerprint
  const char * fingerprint;
  // ASCII date from which the certificate is valid
  const char * valid_from;
  // ASCII date until which the certificate is valid
  const char * valid_until;
  // DN of the certificate issuer
  const char * issuer_dname;
  // Base-64 encoded DER certificate representation
  const char * ascii_cert;
} auth_ssl_server_cert_info_t;

typedef error_t (*auth_ssl_client_cert_pw_prompt_func_t)(
  auth_cred_ssl_client_cert_pw_t ** cred,
  void * baton,
  const char * realm,
  bool may_save,
  apr_pool_t * pool);

typedef error_t (*auth_ssl_server_trust_prompt_func_t)(
  auth_cred_ssl_server_trust_t ** cred,
  void * baton,
  const char * realm,
  apr_uint32_t failures,
  const auth_ssl_server_cert_info_t * cert_info,
  bool may_save,
  apr_pool_t * pool);

typedef error_t (*auth_simple_prompt_func_t)(
  auth_cred_simple_t ** cred,
  void * baton,
  const char * realm,
  const char * username,
  bool may_save,
  apr_pool_t * pool);

typedef error_t (*auth_username_prompt_func_t)(
  auth_cred_username_t ** cred,
  void * baton,
  const char * realm,
  bool may_save,
  apr_pool_t * pool);

//---------------------------------------------------------------------------
// from config_auth.c

#define CONST_FS_KEY "fs"
#define CONST_FINGERPRINT_KEY "fingerprint"

// The keys that will be stored on disk.  These serve the same role as
// similar constants in other providers.
#define AUTHN_ASCII_CERT_KEY            "ascii_cert"
#define AUTHN_FAILURES_KEY              "failures"

static const char CertificateStorageKey[] = "HttpsCertificates";

static error_t
config_read_auth_data(
  apr_hash_t ** hash,
  const char * cred_kind,
  const char * realmstring,
  TWebDAVFileSystem * fs,
  apr_pool_t * pool)
{
  const char * subkey = CertificateStorageKey;
  THierarchicalStorage * Storage = NULL;
  WEBDAV_ERR(fs->CreateStorage(Storage));
  assert(Storage);
  std::unique_ptr<THierarchicalStorage> StoragePtr;
  StoragePtr.reset(Storage);
  Storage->AccessMode = smRead;
  if (!Storage->OpenSubKey(UnicodeString(subkey), false))
    return WEBDAV_ERR_BAD_PARAM;

  *hash = apr_hash_make(pool);
  TStrings * Keys = new TStringList();
  try
  {
    Storage->GetValueNames(Keys);
    for (int Index = 0; Index < Keys->Count; ++Index)
    {
      UnicodeString Key = Keys->Strings[Index];
      UnicodeString Value = Storage->ReadStringRaw(Key, L"");
      apr_hash_set(*hash, AUTHN_ASCII_CERT_KEY, APR_HASH_KEY_STRING,
        string_create(AnsiString(Key).c_str(), pool));
      apr_hash_set(*hash, AUTHN_FAILURES_KEY, APR_HASH_KEY_STRING,
        string_createf(pool, "%lu", (unsigned long)
          StrToIntDef(Value, 0)));
    }
  }
  __finally
  {
    delete Keys;
  }
  return WEBDAV_NO_ERROR;
}

static error_t
config_write_auth_data(
  apr_hash_t * hash,
  const char * cred_kind,
  const char * realmstring,
  TWebDAVFileSystem * fs,
  apr_pool_t * pool)
{
  const char * subkey = CertificateStorageKey;
  assert(fs);
  THierarchicalStorage * Storage = NULL;
  WEBDAV_ERR(fs->CreateStorage(Storage));
  assert(Storage);
  std::unique_ptr<THierarchicalStorage> StoragePtr;
  StoragePtr.reset(Storage);
  Storage->AccessMode = smReadWrite;

  if (!Storage->OpenSubKey(UnicodeString(subkey), true))
    return WEBDAV_ERR_BAD_PARAM;
  string_t * trusted_cert = static_cast<string_t *>(apr_hash_get(hash, AUTHN_ASCII_CERT_KEY,
    APR_HASH_KEY_STRING));
  string_t * failstr = static_cast<string_t *>(apr_hash_get(hash, AUTHN_FAILURES_KEY,
    APR_HASH_KEY_STRING));
  if (trusted_cert && failstr)
    Storage->WriteString(UnicodeString(trusted_cert->data), UnicodeString(failstr->data));
  return WEBDAV_NO_ERROR;
}

//---------------------------------------------------------------------------
// from simple_providers.c

// The keys that will be stored on disk.  These serve the same role as
// similar constants in other providers.
#define AUTHN_USERNAME_KEY            "username"
#define AUTHN_PASSWORD_KEY            "password"
#define AUTHN_PASSTYPE_KEY            "passtype"

// Baton type for the simple provider.
typedef struct simple_provider_baton_t
{
  auth_plaintext_prompt_func_t plaintext_prompt_func;
  void * prompt_baton;
  // We cache the user's answer to the plaintext prompt, keyed
  // by realm, in case we'll be called multiple times for the
  // same realm.
  apr_hash_t * plaintext_answers;
} simple_provider_baton_t;

// Implementation of auth_password_get_t that retrieves
// the plaintext password from CREDS.
static error_t
auth_simple_password_get(
  bool * done,
  const char ** password,
  apr_hash_t * creds,
  const char * realmstring,
  const char * username,
  apr_hash_t * parameters,
  bool non_interactive,
  apr_pool_t * pool)
{
  *done = FALSE;

  string_t * str = static_cast<string_t *>(apr_hash_get(creds, AUTHN_USERNAME_KEY, APR_HASH_KEY_STRING));
  if (str && username && strcmp(str->data, username) == 0)
  {
    str = static_cast<string_t *>(apr_hash_get(creds, AUTHN_PASSWORD_KEY, APR_HASH_KEY_STRING));
    if (str && str->data)
    {
      *password = str->data;
      *done = TRUE;
    }
  }
  return WEBDAV_NO_ERROR;
}

// Implementation of auth_password_set_t that stores
// the plaintext password in CREDS.
static error_t
auth_simple_password_set(
  bool * done,
  apr_hash_t * creds,
  const char * realmstring,
  const char * username,
  const char * password,
  apr_hash_t * parameters,
  bool non_interactive,
  apr_pool_t * pool)
{
  apr_hash_set(creds, AUTHN_PASSWORD_KEY, APR_HASH_KEY_STRING,
    string_create(password, pool));
  *done = TRUE;
  return WEBDAV_NO_ERROR;
}

// Set **USERNAME to the username retrieved from CREDS; ignore
// other parameters. *USERNAME will have the same lifetime as CREDS.
static bool
simple_username_get(
  const char ** username,
  apr_hash_t * creds,
  const char * realmstring,
  bool non_interactive)
{
  string_t * str = static_cast<string_t *>(apr_hash_get(creds, AUTHN_USERNAME_KEY, APR_HASH_KEY_STRING));
  if (str && str->data)
  {
    *username = str->data;
    return TRUE;
  }
  return FALSE;
}

// Common implementation for simple_first_creds. Uses PARAMETERS, REALMSTRING
// and the simple auth provider's username and password cache to fill a set of
// CREDENTIALS. PASSWORD_GET is used to obtain the password value.
// PASSTYPE identifies the type of the cached password. CREDENTIALS are
// allocated from POOL.
static error_t
auth_simple_first_creds_helper(
  void ** credentials,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  auth_password_get_t password_get,
  const char * passtype,
  apr_pool_t * pool)
{
  const char * username = static_cast<const char *>(apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_DEFAULT_USERNAME,
    APR_HASH_KEY_STRING));
  const char * password = static_cast<const char *>(apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_DEFAULT_PASSWORD,
    APR_HASH_KEY_STRING));
  bool non_interactive = apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_NON_INTERACTIVE,
    APR_HASH_KEY_STRING) != NULL;
  const char * default_username = NULL; // Default username from cache.
  const char * default_password = NULL; // Default password from cache.

  // This checks if we should save the CREDS, if saving the credentials is
  // allowed by the run-time configuration.
  bool need_to_save = FALSE;
  apr_hash_t * creds_hash = NULL;
  error_t err = 0;

  // Try to load credentials from a file on disk, based on the
  // realmstring.  Don't throw an error, though: if something went
  // wrong reading the file, no big deal.  What really matters is that
  // we failed to get the creds, so allow the auth system to try the
  // next provider.
  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);
  err = config_read_auth_data(&creds_hash, AUTH_CRED_SIMPLE,
    realmstring, fs, pool);
  if (err)
  {
    error_clear(&err);
    err = NULL;
  }
  else if (creds_hash)
  {
    // We have something in the auth cache for this realm.
    bool have_passtype = FALSE;

    // The password type in the auth data must match the
    // mangler's type, otherwise the password must be
    // interpreted by another provider.
    string_t * str = static_cast<string_t *>(apr_hash_get(creds_hash, AUTHN_PASSTYPE_KEY, APR_HASH_KEY_STRING));
    if (str && str->data)
      if (passtype && (0 == strcmp(str->data, passtype)))
        have_passtype = TRUE;

    // See if we need to save this username if it is not present in
    // auth cache.
    if (username)
    {
      if (!simple_username_get(&default_username, creds_hash, realmstring,
        non_interactive))
      {
        need_to_save = TRUE;
      }
      else
      {
        if (0 == strcmp(default_username, username))
          need_to_save = FALSE;
        else
          need_to_save = TRUE;
      }
    }

    // See if we need to save this password if it is not present in
    // auth cache.
    if (password)
    {
      if (have_passtype)
      {
        bool done;
        WEBDAV_ERR(password_get(&done, &default_password, creds_hash, realmstring,
          username, parameters, non_interactive, pool));
        if (!done)
        {
          need_to_save = TRUE;
        }
        else
        {
          if (0 == strcmp(default_password, password))
            need_to_save = FALSE;
          else
            need_to_save = TRUE;
        }
      }
    }

    // If we don't have a username and a password yet, we try the
    // auth cache
    if (!(username && password))
    {
      if (!username)
        if (!simple_username_get(&username, creds_hash, realmstring,
          non_interactive))
          username = NULL;

      if (username && !password)
      {
        if (!have_passtype)
          password = NULL;
        else
        {
          bool done;
          WEBDAV_ERR(password_get(&done, &password, creds_hash, realmstring,
                 username, parameters, non_interactive,
                 pool));
          if (!done)
            password = NULL;

          // If the auth data didn't contain a password type,
          // force a write to upgrade the format of the auth
          // data file.
          if (password && !have_passtype)
            need_to_save = TRUE;
        }
      }
    }
  }
  else
  {
    // Nothing was present in the auth cache, so indicate that these
    // credentials should be saved.
    need_to_save = TRUE;
  }

  // Ask the OS for the username if we have a password but no username.
  if (password && !username)
    username = user_get_name(pool);

  if (username && password)
  {
    auth_cred_simple_t * creds = static_cast<auth_cred_simple_t *>(apr_pcalloc(pool, sizeof(*creds)));
    creds->username = username;
    creds->password = password;
    creds->may_save = need_to_save;
    *credentials = creds;
  }
  else
    *credentials = NULL;

  *iter_baton = NULL;

  return WEBDAV_NO_ERROR;
}

// Common implementation for simple_save_creds. Uses PARAMETERS and
// REALMSTRING to save a set of CREDENTIALS to the simple auth provider's
// username and password cache. PASSWORD_SET is used to store the password.
// PASSTYPE identifies the type of the cached password. Allocates from POOL.
static error_t
auth_simple_save_creds_helper(
  bool * saved,
  void * credentials,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  auth_password_set_t password_set,
  const char * passtype,
  apr_pool_t * pool)
{
  auth_cred_simple_t * creds = static_cast<auth_cred_simple_t *>(credentials);
  apr_hash_t * creds_hash = NULL;
  error_t err = 0;
  bool dont_store_passwords =
    apr_hash_get(parameters,
      WEBDAV_AUTH_PARAM_DONT_STORE_PASSWORDS,
      APR_HASH_KEY_STRING) != NULL;
  const char * store_plaintext_passwords = static_cast<const char *>(
    apr_hash_get(parameters,
      WEBDAV_AUTH_PARAM_STORE_PLAINTEXT_PASSWORDS,
      APR_HASH_KEY_STRING));
  bool non_interactive = apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_NON_INTERACTIVE,
    APR_HASH_KEY_STRING) != NULL;
  simple_provider_baton_t * b = (simple_provider_baton_t *)provider_baton;

  bool no_auth_cache = (!creds->may_save) ||
    (apr_hash_get(parameters,
       WEBDAV_AUTH_PARAM_NO_AUTH_CACHE,
       APR_HASH_KEY_STRING) != NULL);

  // Make sure we've been passed a passtype.
  WEBDAV_ERR_ASSERT(passtype != NULL);

  *saved = FALSE;

  if (no_auth_cache)
    return WEBDAV_NO_ERROR;

  // Put the username into the credentials hash.
  creds_hash = apr_hash_make(pool);
  apr_hash_set(creds_hash, AUTHN_USERNAME_KEY, APR_HASH_KEY_STRING,
    string_create(creds->username, pool));

  // Don't store passwords in any form if the user has told
  // us not to do so.
  if (!dont_store_passwords)
  {
    bool may_save_password = FALSE;

    // If the password is going to be stored encrypted, go right
    // ahead and store it to disk. Else determine whether saving
    // in plaintext is OK.
    if (passtype &&
        (strcmp(passtype, WEBDAV_AUTH_WINCRYPT_PASSWORD_TYPE) == 0))
    {
      may_save_password = TRUE;
    }
    else
    {
      if (store_plaintext_passwords && cstring_casecmp(store_plaintext_passwords,
          WEBDAV_CONFIG_ASK) == 0)
      {
        if (non_interactive)
          // In non-interactive mode, the default behaviour is
          // to not store the password, because it is usually
          // passed on the command line.
          may_save_password = FALSE;
        else if (b && b->plaintext_prompt_func)
        {
          // We're interactive, and the client provided a
          // prompt callback. So we can ask the user.
          // Check for a cached answer before prompting.
          bool * cached_answer;
          cached_answer = static_cast<bool *>(apr_hash_get(b->plaintext_answers,
            realmstring,
            APR_HASH_KEY_STRING));
          if (cached_answer != NULL)
            may_save_password = *cached_answer;
          else
          {
            apr_pool_t * cached_answer_pool;

            // Nothing cached for this realm, prompt the user.
            WEBDAV_ERR((*b->plaintext_prompt_func)(&may_save_password,
              realmstring,
              b->prompt_baton,
              pool));

            cached_answer_pool = apr_hash_pool_get(b->plaintext_answers);
            cached_answer = static_cast<bool *>(apr_pcalloc(cached_answer_pool,
              sizeof(bool)));
            *cached_answer = may_save_password;
            apr_hash_set(b->plaintext_answers, realmstring,
              APR_HASH_KEY_STRING, cached_answer);
          }
        }
        else
        {
          may_save_password = TRUE;
        }
      }
      else if (store_plaintext_passwords && cstring_casecmp(store_plaintext_passwords,
              WEBDAV_CONFIG_FALSE) == 0)
      {
        may_save_password = FALSE;
      }
      else if (store_plaintext_passwords && cstring_casecmp(store_plaintext_passwords,
              WEBDAV_CONFIG_TRUE) == 0)
      {
        may_save_password = TRUE;
      }
      else
      {
        /*return error_createf
          (WEBDAV_ERR_BAD_CONFIG_VALUE, NULL,
           "Config error: invalid value '%s' for option '%s'",
          store_plaintext_passwords,
          WEBDAV_AUTH_PARAM_STORE_PLAINTEXT_PASSWORDS);*/
        may_save_password = FALSE;
      }
    }

    if (may_save_password)
    {
      WEBDAV_ERR(password_set(saved, creds_hash, realmstring,
        creds->username, creds->password,
        parameters, non_interactive, pool));
      if (*saved && passtype)
        // Store the password type with the auth data, so that we
        // know which provider owns the password.
        apr_hash_set(creds_hash, AUTHN_PASSTYPE_KEY, APR_HASH_KEY_STRING,
          string_create(passtype, pool));
    }
  }

  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);
  // Save credentials to disk.
  err = config_write_auth_data(creds_hash, AUTH_CRED_SIMPLE,
    realmstring, fs, pool);
  error_clear(&err);

  return WEBDAV_NO_ERROR;
}

// Get cached (unencrypted) credentials from the simple provider's cache.
static error_t
simple_first_creds(
  void ** credentials,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  return auth_simple_first_creds_helper(credentials,
    iter_baton,
    provider_baton,
    parameters,
    realmstring,
    auth_simple_password_get,
    WEBDAV_AUTH_SIMPLE_PASSWORD_TYPE,
    pool);
}

// Save (unencrypted) credentials to the simple provider's cache.
static error_t
simple_save_creds(
  bool * saved,
  void * credentials,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  return auth_simple_save_creds_helper(saved, credentials,
    provider_baton,
    parameters,
    realmstring,
    auth_simple_password_set,
    WEBDAV_AUTH_SIMPLE_PASSWORD_TYPE,
    pool);
}

static const auth_provider_t simple_provider =
{
  AUTH_CRED_SIMPLE,
  simple_first_creds,
  NULL,
  simple_save_creds
};

// Public API
static void
auth_get_simple_provider2(
  auth_provider_object_t ** provider,
  auth_plaintext_prompt_func_t plaintext_prompt_func,
  void * prompt_baton,
  apr_pool_t * pool)
{
  auth_provider_object_t * po = static_cast<auth_provider_object_t *>(apr_pcalloc(pool, sizeof(*po)));
  simple_provider_baton_t * pb = static_cast<simple_provider_baton_t *>(apr_pcalloc(pool, sizeof(*pb)));

  pb->plaintext_prompt_func = plaintext_prompt_func;
  pb->prompt_baton = prompt_baton;
  pb->plaintext_answers = apr_hash_make(pool);

  po->vtable = &simple_provider;
  po->provider_baton = pb;
  *provider = po;
}

// Baton type for username/password prompting.
typedef struct simple_prompt_provider_baton_t
{
  auth_simple_prompt_func_t prompt_func;
  void * prompt_baton;

  // how many times to re-prompt after the first one fails
  int retry_limit;
} simple_prompt_provider_baton_t;

// Iteration baton type for username/password prompting.
typedef struct simple_prompt_iter_baton_t
{
  // how many times we've reprompted
  int retries;
} simple_prompt_iter_baton_t;

// Helper Functions
static error_t
prompt_for_simple_creds(
  auth_cred_simple_t ** cred_p,
  simple_prompt_provider_baton_t * pb,
  apr_hash_t * parameters,
  const char * realmstring,
  bool first_time,
  bool may_save,
  apr_pool_t * pool)
{
  const char * default_username = NULL;
  const char * default_password = NULL;

  *cred_p = NULL;

  // If we're allowed to check for default usernames and passwords, do so.
  if (first_time)
  {
    default_username = static_cast<const char *>(apr_hash_get(parameters,
      WEBDAV_AUTH_PARAM_DEFAULT_USERNAME,
      APR_HASH_KEY_STRING));

    // No default username?  Try the auth cache.
    if (!default_username)
    {
      apr_hash_t * creds_hash = NULL;
      string_t * str;
      error_t err;

      TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(parameters,
        CONST_FS_KEY,
        APR_HASH_KEY_STRING));
      assert(fs);
      err = config_read_auth_data(&creds_hash, AUTH_CRED_SIMPLE,
        realmstring, fs, pool);
      error_clear(&err);
      if (!err && creds_hash)
      {
        str = static_cast<string_t *>(apr_hash_get(creds_hash, AUTHN_USERNAME_KEY,
          APR_HASH_KEY_STRING));
        if (str && str->data)
          default_username = str->data;
      }
    }

    // Still no default username?  Try the 'servers' file.
    if (!default_username)
    {
      default_username = NULL;
    }

    // Still no default username?  Try the UID.
    if (!default_username)
      default_username = user_get_name(pool);

    default_password = static_cast<const char *>(apr_hash_get(parameters,
      WEBDAV_AUTH_PARAM_DEFAULT_PASSWORD,
      APR_HASH_KEY_STRING));
  }

  // If we have defaults, just build the cred here and return it.
  // I do wonder why this is here instead of in a separate
  // 'defaults' provider that would run before the prompt
  // provider... Hmmm.

  if (default_username && default_password)
  {
    *cred_p = static_cast<auth_cred_simple_t *>(apr_pcalloc(pool, sizeof(**cred_p)));
    (*cred_p)->username = apr_pstrdup(pool, default_username);
    (*cred_p)->password = apr_pstrdup(pool, default_password);
    (*cred_p)->may_save = TRUE;
  }
  else
  {
    WEBDAV_ERR(pb->prompt_func(cred_p, pb->prompt_baton, realmstring,
      default_username, may_save, pool));
  }

  return WEBDAV_NO_ERROR;
}

// Our first attempt will use any default username/password passed
// in, and prompt for the remaining stuff.
static error_t
simple_prompt_first_creds(
  void ** credentials_p,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  simple_prompt_provider_baton_t * pb =
    static_cast<simple_prompt_provider_baton_t *>(provider_baton);
  simple_prompt_iter_baton_t * ibaton =
    static_cast<simple_prompt_iter_baton_t *>(apr_pcalloc(pool, sizeof(*ibaton)));
  const char * no_auth_cache = static_cast<const char *>(apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_NO_AUTH_CACHE,
    APR_HASH_KEY_STRING));

  WEBDAV_ERR(prompt_for_simple_creds((auth_cred_simple_t **) credentials_p,
    pb, parameters, realmstring, TRUE,
    !no_auth_cache, pool));

  ibaton->retries = 0;
  *iter_baton = ibaton;

  return WEBDAV_NO_ERROR;
}

// Subsequent attempts to fetch will ignore the default values, and
// simply re-prompt for both, up to a maximum of ib->pb->retry_limit.
static error_t
simple_prompt_next_creds(
  void ** credentials_p,
  void * iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  simple_prompt_iter_baton_t * ib =
    static_cast<simple_prompt_iter_baton_t *>(iter_baton);
  simple_prompt_provider_baton_t * pb =
    static_cast<simple_prompt_provider_baton_t *>(provider_baton);
  const char * no_auth_cache = static_cast<const char *>(apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_NO_AUTH_CACHE,
    APR_HASH_KEY_STRING));

  if ((pb->retry_limit >= 0) && (ib->retries >= pb->retry_limit))
  {
    // give up, go on to next provider.
    *credentials_p = NULL;
    return WEBDAV_NO_ERROR;
  }
  ib->retries++;

  return prompt_for_simple_creds((auth_cred_simple_t **) credentials_p,
    pb, parameters, realmstring, FALSE,
    !no_auth_cache, pool);
}

static const auth_provider_t simple_prompt_provider =
{
  AUTH_CRED_SIMPLE,
  simple_prompt_first_creds,
  simple_prompt_next_creds,
  NULL,
};

// Public API
static void
auth_get_simple_prompt_provider(
  auth_provider_object_t ** provider,
  auth_simple_prompt_func_t prompt_func,
  void * prompt_baton,
  int retry_limit,
  apr_pool_t * pool)
{
  auth_provider_object_t * po =
    static_cast<auth_provider_object_t *>(apr_pcalloc(pool, sizeof(*po)));
  simple_prompt_provider_baton_t * pb =
    static_cast<simple_prompt_provider_baton_t *>(apr_pcalloc(pool, sizeof(*pb)));

  pb->prompt_func = prompt_func;
  pb->prompt_baton = prompt_baton;
  pb->retry_limit = retry_limit;

  po->vtable = &simple_prompt_provider;
  po->provider_baton = pb;
  *provider = po;
}

//---------------------------------------------------------------------------
// from ssl_client_cert_pw_providers.c

// The keys that will be stored on disk.  These serve the same role as
// similar constants in other providers.
// AUTHN_PASSTYPE_KEY just records the passphrase type next to the
// passphrase, so that anyone who is manually editing their authn
// files can know which provider owns the password.

#define AUTHN_PASSPHRASE_KEY            "passphrase"
#define AUTHN_PASSTYPE_KEY              "passtype"

// Baton type for the ssl client cert passphrase provider.
typedef struct ssl_client_cert_pw_file_provider_baton_t
{
  auth_plaintext_passphrase_prompt_func_t plaintext_passphrase_prompt_func;
  void * prompt_baton;
  // We cache the user's answer to the plaintext prompt, keyed
  // by realm, in case we'll be called multiple times for the
  // same realm.  So: keys are 'const char *' realm strings, and
  // values are 'bool *'.
  apr_hash_t * plaintext_answers;
} ssl_client_cert_pw_file_provider_baton_t;

// This implements the auth_password_get_t interface.
// Set **PASSPHRASE to the plaintext passphrase retrieved from CREDS;
// ignore other parameters.
static error_t
auth_ssl_client_cert_pw_get(
  bool * done,
  const char ** passphrase,
  apr_hash_t * creds,
  const char * realmstring,
  const char * username,
  apr_hash_t * parameters,
  bool non_interactive,
  apr_pool_t * pool)
{
  string_t * str = static_cast<string_t *>(apr_hash_get(creds, AUTHN_PASSPHRASE_KEY, APR_HASH_KEY_STRING));
  if (str && str->data)
  {
    *passphrase = str->data;
    *done = TRUE;
    return WEBDAV_NO_ERROR;
  }
  *done = FALSE;
  return WEBDAV_NO_ERROR;
}

// This implements the auth_password_set_t interface.
// Store PASSPHRASE in CREDS; ignore other parameters.
static error_t
auth_ssl_client_cert_pw_set(
  bool * done,
  apr_hash_t * creds,
  const char * realmstring,
  const char * username,
  const char * passphrase,
  apr_hash_t * parameters,
  bool non_interactive,
  apr_pool_t * pool)
{
  apr_hash_set(creds, AUTHN_PASSPHRASE_KEY, APR_HASH_KEY_STRING,
    string_create(passphrase, pool));
  *done = TRUE;
  return WEBDAV_NO_ERROR;
}

static error_t
auth_ssl_client_cert_pw_file_first_creds_helper(
  void ** credentials_p,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  auth_password_get_t passphrase_get,
  const char * passtype,
  apr_pool_t * pool)
{
  bool non_interactive = apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_NON_INTERACTIVE,
    APR_HASH_KEY_STRING) != NULL;
  const char * password = NULL;
  if (!password)
  {
    error_t err;
    apr_hash_t * creds_hash = NULL;

    TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(parameters,
      CONST_FS_KEY,
      APR_HASH_KEY_STRING));
    assert(fs);
    // Try to load passphrase from the auth/ cache.
    err = config_read_auth_data(&creds_hash,
      AUTH_CRED_SSL_CLIENT_CERT_PW,
      realmstring, fs, pool);
    error_clear(&err);
    if (!err && creds_hash)
    {
      bool done;
      WEBDAV_ERR(passphrase_get(&done, &password, creds_hash, realmstring,
             NULL, parameters, non_interactive, pool));
      if (!done)
        password = NULL;
    }
  }

  if (password)
  {
    auth_cred_ssl_client_cert_pw_t * cred =
      static_cast<auth_cred_ssl_client_cert_pw_t *>(apr_pcalloc(pool, sizeof(*cred)));
    cred->password = password;
    cred->may_save = FALSE;
    *credentials_p = cred;
  }
  else *credentials_p = NULL;
  *iter_baton = NULL;
  return WEBDAV_NO_ERROR;
}

static error_t
auth_ssl_client_cert_pw_file_save_creds_helper(
  bool * saved,
  void * credentials,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  auth_password_set_t passphrase_set,
  const char * passtype,
  apr_pool_t * pool)
{
  auth_cred_ssl_client_cert_pw_t * creds =
    static_cast<auth_cred_ssl_client_cert_pw_t *>(credentials);
  apr_hash_t * creds_hash = NULL;
  error_t err = 0;
  bool dont_store_passphrase =
    apr_hash_get(parameters,
      WEBDAV_AUTH_PARAM_DONT_STORE_SSL_CLIENT_CERT_PP,
      APR_HASH_KEY_STRING) != NULL;
  const char * store_ssl_client_cert_pp_plaintext =
    static_cast<const char *>(apr_hash_get(parameters,
      WEBDAV_AUTH_PARAM_STORE_SSL_CLIENT_CERT_PP_PLAINTEXT,
      APR_HASH_KEY_STRING));
  bool non_interactive = apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_NON_INTERACTIVE,
    APR_HASH_KEY_STRING) != NULL;
  ssl_client_cert_pw_file_provider_baton_t * b =
    (ssl_client_cert_pw_file_provider_baton_t *)provider_baton;

  bool no_auth_cache = (!creds->may_save) ||
    (apr_hash_get(parameters,
       WEBDAV_AUTH_PARAM_NO_AUTH_CACHE,
       APR_HASH_KEY_STRING) != NULL);

  *saved = FALSE;

  if (no_auth_cache)
    return WEBDAV_NO_ERROR;

  creds_hash = apr_hash_make(pool);

  // Don't store passphrase in any form if the user has told
  // us not to do so.
  if (!dont_store_passphrase)
  {
    bool may_save_passphrase = FALSE;

    // If the passphrase is going to be stored encrypted, go right
    // ahead and store it to disk. Else determine whether saving
    // in plaintext is OK.
    if (strcmp(passtype, WEBDAV_AUTH_WINCRYPT_PASSWORD_TYPE) == 0)
    {
      may_save_passphrase = TRUE;
    }
    else
    {
      if (cstring_casecmp(store_ssl_client_cert_pp_plaintext,
                         WEBDAV_CONFIG_ASK) == 0)
      {
        if (non_interactive)
        {
          // In non-interactive mode, the default behaviour is
          // to not store the passphrase
          may_save_passphrase = FALSE;
        }
        else if (b->plaintext_passphrase_prompt_func)
        {
          bool * cached_answer =
            static_cast<bool *>(apr_hash_get(b->plaintext_answers, realmstring,
              APR_HASH_KEY_STRING));

          if (cached_answer != NULL)
          {
            may_save_passphrase = *cached_answer;
          }
          else
          {
            apr_pool_t * cached_answer_pool;

            // Nothing cached for this realm, prompt the user.
            WEBDAV_ERR((*b->plaintext_passphrase_prompt_func)(
              &may_save_passphrase,
              realmstring,
              b->prompt_baton,
              pool));

            cached_answer_pool = apr_hash_pool_get(b->plaintext_answers);
            cached_answer = static_cast<bool *>(apr_pcalloc(cached_answer_pool,
              sizeof(*cached_answer)));
            *cached_answer = may_save_passphrase;
            apr_hash_set(b->plaintext_answers, realmstring,
              APR_HASH_KEY_STRING, cached_answer);
          }
        }
        else
        {
          may_save_passphrase = FALSE;
        }
      }
      else if (cstring_casecmp(store_ssl_client_cert_pp_plaintext,
                 WEBDAV_CONFIG_FALSE) == 0)
      {
        may_save_passphrase = FALSE;
      }
      else if (cstring_casecmp(store_ssl_client_cert_pp_plaintext,
                 WEBDAV_CONFIG_TRUE) == 0)
      {
        may_save_passphrase = TRUE;
      }
      else
      {
        return error_createf(WEBDAV_ERR_DAV_INVALID_CONFIG_VALUE, NULL,
          "Config error: invalid value '%s' for option '%s'",
          store_ssl_client_cert_pp_plaintext,
          WEBDAV_AUTH_PARAM_STORE_SSL_CLIENT_CERT_PP_PLAINTEXT);
      }
    }

    if (may_save_passphrase)
    {
      WEBDAV_ERR(passphrase_set(saved, creds_hash, realmstring,
                   NULL, creds->password, parameters,
                   non_interactive, pool));

      if (*saved && passtype)
      {
        apr_hash_set(creds_hash, AUTHN_PASSTYPE_KEY,
          APR_HASH_KEY_STRING,
          string_create(passtype, pool));
      }

      TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(parameters,
        CONST_FS_KEY,
        APR_HASH_KEY_STRING));
      assert(fs);
      // Save credentials to disk.
      err = config_write_auth_data(creds_hash,
        AUTH_CRED_SSL_CLIENT_CERT_PW,
        realmstring, fs, pool);
      error_clear(&err);
      *saved = !err;
    }
  }

  return WEBDAV_NO_ERROR;
}

// This implements the auth_provider_t.first_credentials API.
// It gets cached (unencrypted) credentials from the ssl client cert
// password provider's cache.
static error_t
ssl_client_cert_pw_file_first_credentials(
  void ** credentials_p,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  return auth_ssl_client_cert_pw_file_first_creds_helper(
    credentials_p,
    iter_baton,
    provider_baton,
    parameters,
    realmstring,
    auth_ssl_client_cert_pw_get,
    WEBDAV_AUTH_SIMPLE_PASSWORD_TYPE,
    pool);
}

// This implements the auth_provider_t.save_credentials API.
// It saves the credentials unencrypted.
static error_t
ssl_client_cert_pw_file_save_credentials(
  bool * saved,
  void * credentials,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  return auth_ssl_client_cert_pw_file_save_creds_helper(
    saved, credentials,
    provider_baton,
    parameters,
    realmstring,
    auth_ssl_client_cert_pw_set,
    WEBDAV_AUTH_SIMPLE_PASSWORD_TYPE,
    pool);
}

static const auth_provider_t ssl_client_cert_pw_file_provider =
{
  AUTH_CRED_SSL_CLIENT_CERT_PW,
  ssl_client_cert_pw_file_first_credentials,
  NULL,
  ssl_client_cert_pw_file_save_credentials
};

// Public API to SSL file providers.
static void
auth_get_ssl_client_cert_pw_file_provider2(
  auth_provider_object_t ** provider,
  auth_plaintext_passphrase_prompt_func_t plaintext_passphrase_prompt_func,
  void * prompt_baton,
  apr_pool_t * pool)
{
  auth_provider_object_t * po =
    static_cast<auth_provider_object_t *>(apr_pcalloc(pool, sizeof(*po)));
  ssl_client_cert_pw_file_provider_baton_t * pb =
    static_cast<ssl_client_cert_pw_file_provider_baton_t *>(apr_pcalloc(pool,
        sizeof(*pb)));

  pb->plaintext_passphrase_prompt_func = plaintext_passphrase_prompt_func;
  pb->prompt_baton = prompt_baton;
  pb->plaintext_answers = apr_hash_make(pool);

  po->vtable = &ssl_client_cert_pw_file_provider;
  po->provider_baton = pb;
  *provider = po;
}

/*-----------------------------------------------------------------------*/
// Prompt provider
/*-----------------------------------------------------------------------*/

// Baton type for client passphrase prompting.
// There is no iteration baton type.
typedef struct ssl_client_cert_pw_prompt_provider_baton_t
{
  auth_ssl_client_cert_pw_prompt_func_t prompt_func;
  void * prompt_baton;

  // how many times to re-prompt after the first one fails
  int retry_limit;
} ssl_client_cert_pw_prompt_provider_baton_t;

// Iteration baton.
typedef struct ssl_client_cert_pw_prompt_iter_baton_t
{
  // The original provider baton
  ssl_client_cert_pw_prompt_provider_baton_t * pb;

  // The original realmstring
  const char * realmstring;

  // how many times we've reprompted
  int retries;
} ssl_client_cert_pw_prompt_iter_baton_t;

static error_t
ssl_client_cert_pw_prompt_first_cred(
  void ** credentials_p,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  ssl_client_cert_pw_prompt_provider_baton_t * pb =
    static_cast<ssl_client_cert_pw_prompt_provider_baton_t *>(provider_baton);
  ssl_client_cert_pw_prompt_iter_baton_t * ib =
    static_cast<ssl_client_cert_pw_prompt_iter_baton_t *>(apr_pcalloc(pool, sizeof(*ib)));
  const char * no_auth_cache = static_cast<const char *>(apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_NO_AUTH_CACHE,
    APR_HASH_KEY_STRING));

  WEBDAV_ERR(pb->prompt_func((auth_cred_ssl_client_cert_pw_t **)
    credentials_p, pb->prompt_baton, realmstring,
    !no_auth_cache, pool));

  ib->pb = pb;
  ib->realmstring = apr_pstrdup(pool, realmstring);
  ib->retries = 0;
  *iter_baton = ib;

  return WEBDAV_NO_ERROR;
}

static error_t
ssl_client_cert_pw_prompt_next_cred(
  void ** credentials_p,
  void * iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  ssl_client_cert_pw_prompt_iter_baton_t * ib =
    static_cast<ssl_client_cert_pw_prompt_iter_baton_t *>(iter_baton);
  const char * no_auth_cache = static_cast<const char *>(apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_NO_AUTH_CACHE,
    APR_HASH_KEY_STRING));

  if ((ib->pb->retry_limit >= 0) && (ib->retries >= ib->pb->retry_limit))
  {
    // give up, go on to next provider.
    *credentials_p = NULL;
    return WEBDAV_NO_ERROR;
  }
  ib->retries++;

  return ib->pb->prompt_func((auth_cred_ssl_client_cert_pw_t **)
    credentials_p, ib->pb->prompt_baton,
    ib->realmstring, !no_auth_cache, pool);
}

static const auth_provider_t client_cert_pw_prompt_provider =
{
  AUTH_CRED_SSL_CLIENT_CERT_PW,
  ssl_client_cert_pw_prompt_first_cred,
  ssl_client_cert_pw_prompt_next_cred,
  NULL
};

static void
auth_get_ssl_client_cert_pw_prompt_provider(
  auth_provider_object_t ** provider,
  auth_ssl_client_cert_pw_prompt_func_t prompt_func,
  void * prompt_baton,
  int retry_limit,
  apr_pool_t * pool)
{
  auth_provider_object_t * po =
    static_cast<auth_provider_object_t *>(apr_pcalloc(pool, sizeof(*po)));
  ssl_client_cert_pw_prompt_provider_baton_t * pb =
    static_cast<ssl_client_cert_pw_prompt_provider_baton_t *>(apr_pcalloc(pool, sizeof(*pb)));

  pb->prompt_func = prompt_func;
  pb->prompt_baton = prompt_baton;
  pb->retry_limit = retry_limit;

  po->vtable = &client_cert_pw_prompt_provider;
  po->provider_baton = pb;
  *provider = po;
}

//------------------------------------------------------------------------------
// from win32_crypto.c

// The description string that's combined with unencrypted data by the
// Windows CryptoAPI. Used during decryption to verify that the
// encrypted data were valid.
static const WCHAR description[] = L"auth.simple.wincrypt";

// Implementation of auth_password_set_t that encrypts
// the incoming password using the Windows CryptoAPI.
static error_t
windows_password_encrypter(
  bool * done,
  apr_hash_t * creds,
  const char * realmstring,
  const char * username,
  const char * in,
  apr_hash_t * parameters,
  bool non_interactive,
  apr_pool_t * pool)
{
  DATA_BLOB blobin;
  DATA_BLOB blobout;
  BOOL crypted = FALSE;

  blobin.cbData = (DWORD)strlen(in);
  blobin.pbData = (BYTE *) in;
  crypted = CryptProtectData(&blobin, description, NULL, NULL, NULL,
    CRYPTPROTECT_UI_FORBIDDEN, &blobout);
  if (crypted)
  {
    char * coded = static_cast<char *>(apr_pcalloc(pool, apr_base64_encode_len(blobout.cbData)));
    apr_base64_encode(coded, (const char *)blobout.pbData, blobout.cbData);
    WEBDAV_ERR(auth_simple_password_set(done, creds, realmstring, username,
      coded, parameters,
      non_interactive, pool));
    LocalFree(blobout.pbData);
  }

  return WEBDAV_NO_ERROR;
}

// Implementation of auth_password_get_t that decrypts
// the incoming password using the Windows CryptoAPI and verifies its
// validity.
static error_t
windows_password_decrypter(
  bool * done,
  const char ** out,
  apr_hash_t * creds,
  const char * realmstring,
  const char * username,
  apr_hash_t * parameters,
  bool non_interactive,
  apr_pool_t * pool)
{
  DATA_BLOB blobin;
  DATA_BLOB blobout;
  LPWSTR descr;
  BOOL decrypted = FALSE;
  const char * in = NULL;

  WEBDAV_ERR(auth_simple_password_get(done, &in, creds, realmstring, username,
         parameters, non_interactive, pool));
  if (!*done)
    return WEBDAV_NO_ERROR;

  blobin.cbData = (DWORD)strlen(in);
  blobin.pbData = static_cast<BYTE *>(apr_pcalloc(pool, apr_base64_decode_len(in)));
  apr_base64_decode((char *)blobin.pbData, in);
  decrypted = CryptUnprotectData(&blobin, &descr, NULL, NULL, NULL,
    CRYPTPROTECT_UI_FORBIDDEN, &blobout);
  if (decrypted)
  {
    if (0 == lstrcmpW(descr, description))
      *out = apr_pstrndup(pool, (const char *)blobout.pbData, blobout.cbData);
    else
      decrypted = FALSE;
    LocalFree(blobout.pbData);
    LocalFree(descr);
  }

  *done = decrypted != 0;
  return WEBDAV_NO_ERROR;
}

// Get cached encrypted credentials from the simple provider's cache.
static error_t
windows_simple_first_creds(
  void ** credentials,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  return auth_simple_first_creds_helper(credentials,
    iter_baton,
    provider_baton,
    parameters,
    realmstring,
    windows_password_decrypter,
    WEBDAV_AUTH_WINCRYPT_PASSWORD_TYPE,
    pool);
}

// Save encrypted credentials to the simple provider's cache.
static error_t
windows_simple_save_creds(
  bool * saved,
  void * credentials,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  return auth_simple_save_creds_helper(saved, credentials,
    provider_baton,
    parameters,
    realmstring,
    windows_password_encrypter,
    WEBDAV_AUTH_WINCRYPT_PASSWORD_TYPE,
    pool);
}

static const auth_provider_t windows_simple_provider =
{
  AUTH_CRED_SIMPLE,
  windows_simple_first_creds,
  NULL,
  windows_simple_save_creds
};

// Public API
static void
auth_get_windows_simple_provider(
  auth_provider_object_t ** provider,
  apr_pool_t * pool)
{
  auth_provider_object_t * po =
    static_cast<auth_provider_object_t *>(apr_pcalloc(pool, sizeof(*po)));

  po->vtable = &windows_simple_provider;
  *provider = po;
}

/*-----------------------------------------------------------------------*/
// Windows SSL server trust provider, validates ssl certificate using
// CryptoApi.
/*-----------------------------------------------------------------------*/

// Implementation of auth_password_set_t that encrypts
// the incoming password using the Windows CryptoAPI.
static error_t
windows_ssl_client_cert_pw_encrypter(
  bool * done,
  apr_hash_t * creds,
  const char * realmstring,
  const char * username,
  const char * in,
  apr_hash_t * parameters,
  bool non_interactive,
  apr_pool_t * pool)
{
  DATA_BLOB blobin;
  DATA_BLOB blobout;
  BOOL crypted;

  blobin.cbData = (DWORD)strlen(in);
  blobin.pbData = (BYTE *) in;
  crypted = CryptProtectData(&blobin, description, NULL, NULL, NULL,
    CRYPTPROTECT_UI_FORBIDDEN, &blobout);
  if (crypted)
  {
    char * coded = static_cast<char *>(apr_pcalloc(pool, apr_base64_encode_len(blobout.cbData)));
    apr_base64_encode(coded, (const char *)blobout.pbData, blobout.cbData);
    WEBDAV_ERR(auth_ssl_client_cert_pw_set(done, creds, realmstring, username,
      coded, parameters,
      non_interactive, pool));
    LocalFree(blobout.pbData);
  }

  return WEBDAV_NO_ERROR;
}

// Implementation of auth_password_get_t that decrypts
// the incoming password using the Windows CryptoAPI and verifies its
// validity.
static error_t
windows_ssl_client_cert_pw_decrypter(
  bool * done,
  const char ** out,
  apr_hash_t * creds,
  const char * realmstring,
  const char * username,
  apr_hash_t * parameters,
  bool non_interactive,
  apr_pool_t * pool)
{
  DATA_BLOB blobin;
  DATA_BLOB blobout;
  LPWSTR descr;
  BOOL decrypted;
  const char * in = NULL;

  WEBDAV_ERR(auth_ssl_client_cert_pw_get(done, &in, creds, realmstring, username,
         parameters, non_interactive, pool));
  if (!*done)
    return WEBDAV_NO_ERROR;

  blobin.cbData = (DWORD)strlen(in);
  blobin.pbData = static_cast<BYTE *>(apr_pcalloc(pool, apr_base64_decode_len(in)));
  apr_base64_decode((char *)blobin.pbData, in);
  decrypted = CryptUnprotectData(&blobin, &descr, NULL, NULL, NULL,
    CRYPTPROTECT_UI_FORBIDDEN, &blobout);
  if (decrypted)
  {
    if (0 == lstrcmpW(descr, description))
      *out = apr_pstrndup(pool, (const char *)blobout.pbData, blobout.cbData);
    else
      decrypted = FALSE;
    LocalFree(blobout.pbData);
    LocalFree(descr);
  }

  *done = decrypted != 0;
  return WEBDAV_NO_ERROR;
}

// Get cached encrypted credentials from the simple provider's cache.
static error_t
windows_ssl_client_cert_pw_first_creds(
  void ** credentials,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  return auth_ssl_client_cert_pw_file_first_creds_helper(
    credentials,
    iter_baton,
    provider_baton,
    parameters,
    realmstring,
    windows_ssl_client_cert_pw_decrypter,
    WEBDAV_AUTH_WINCRYPT_PASSWORD_TYPE,
    pool);
}

// Save encrypted credentials to the simple provider's cache.
static error_t
windows_ssl_client_cert_pw_save_creds(
  bool * saved,
  void * credentials,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  return auth_ssl_client_cert_pw_file_save_creds_helper(
    saved,
    credentials,
    provider_baton,
    parameters,
    realmstring,
    windows_ssl_client_cert_pw_encrypter,
    WEBDAV_AUTH_WINCRYPT_PASSWORD_TYPE,
    pool);
}

static const auth_provider_t windows_ssl_client_cert_pw_provider =
{
  AUTH_CRED_SSL_CLIENT_CERT_PW,
  windows_ssl_client_cert_pw_first_creds,
  NULL,
  windows_ssl_client_cert_pw_save_creds
};

// Public API
static void
auth_get_windows_ssl_client_cert_pw_provider(
  auth_provider_object_t ** provider,
  apr_pool_t * pool)
{
  auth_provider_object_t * po =
    static_cast<auth_provider_object_t *>(apr_pcalloc(pool, sizeof(*po)));

  po->vtable = &windows_ssl_client_cert_pw_provider;
  *provider = po;
}

/*-----------------------------------------------------------------------*/
// Windows SSL server trust provider, validates ssl certificate using
// CryptoApi.
/*-----------------------------------------------------------------------*/

// Helper to create CryptoAPI CERT_CONTEXT from base64 encoded BASE64_CERT.
// Returns NULL on error.

static PCCERT_CONTEXT
certcontext_from_base64(
  const char * base64_cert,
  apr_pool_t * pool)
{
  PCCERT_CONTEXT cert_context = NULL;
  int cert_len = 0;
  BYTE * binary_cert = NULL;

  // Use apr-util as CryptStringToBinaryA is available only on XP+.
  binary_cert = static_cast<BYTE *>(apr_pcalloc(pool,
                                    apr_base64_decode_len(base64_cert)));
  cert_len = apr_base64_decode((char *)binary_cert, base64_cert);

  // Parse the certificate into a context.
  cert_context = CertCreateCertificateContext(X509_ASN_ENCODING | PKCS_7_ASN_ENCODING,
    binary_cert, cert_len);

  return cert_context;
}

// Helper for windows_ssl_server_trust_first_credentials for validating
// certificate using CryptoApi. Sets *OK_P to TRUE if base64 encoded ASCII_CERT
// certificate considered as valid.

static error_t
windows_validate_certificate(
  bool * ok_p,
  const char * ascii_cert,
  apr_pool_t * pool)
{
  PCCERT_CONTEXT cert_context = NULL;
  CERT_CHAIN_PARA chain_para;
  PCCERT_CHAIN_CONTEXT chain_context = NULL;

  *ok_p = FALSE;

  // Parse the certificate into a context.
  cert_context = certcontext_from_base64(ascii_cert, pool);

  if (cert_context)
  {
    // Retrieve the certificate chain of the certificate
    // (a certificate without a valid root does not have a chain).
    memset(&chain_para, 0, sizeof(chain_para));
    chain_para.cbSize = sizeof(chain_para);

    HCERTCHAINENGINE chain_engine;
    CERT_CHAIN_ENGINE_CONFIG chain_config;

    chain_config.cbSize = sizeof(CERT_CHAIN_ENGINE_CONFIG);
    chain_config.hRestrictedRoot = NULL;
    chain_config.hRestrictedTrust = NULL;
    chain_config.hRestrictedOther = NULL;
    chain_config.cAdditionalStore = 0;
    chain_config.rghAdditionalStore = NULL;
    chain_config.dwFlags = CERT_CHAIN_CACHE_END_CERT;
    chain_config.dwUrlRetrievalTimeout = 0;
    chain_config.MaximumCachedCertificates =0;
    chain_config.CycleDetectionModulus = 0;

    CertCreateCertificateChainEngine(
        &chain_config,
        &chain_engine);

    if (CertGetCertificateChain(chain_engine, cert_context, NULL, NULL, &chain_para,
          CERT_CHAIN_CACHE_END_CERT |
          CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT,
          NULL, &chain_context))
    {
      CERT_CHAIN_POLICY_PARA policy_para;
      CERT_CHAIN_POLICY_STATUS policy_status;

      policy_para.cbSize = sizeof(policy_para);
      policy_para.dwFlags = 0;
      policy_para.pvExtraPolicyPara = NULL;

      policy_status.cbSize = sizeof(policy_status);

      if (CertVerifyCertificateChainPolicy(CERT_CHAIN_POLICY_SSL,
            chain_context, &policy_para,
            &policy_status))
      {
        if (policy_status.dwError == S_OK)
        {
          // Windows thinks the certificate is valid.
          *ok_p = TRUE;
        }
      }

      CertFreeCertificateChain(chain_context);
    }
    CertFreeCertificateContext(cert_context);
    CertFreeCertificateChainEngine(chain_engine);
  }

  return WEBDAV_NO_ERROR;
}

// Retrieve ssl server CA failure overrides (if any) from CryptoApi.
static error_t
windows_ssl_server_trust_first_credentials(
  void ** credentials,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  apr_uint32_t * failures = static_cast<apr_uint32_t *>(apr_hash_get(parameters,
    AUTH_PARAM_SSL_SERVER_FAILURES,
    APR_HASH_KEY_STRING));
  const auth_ssl_server_cert_info_t * cert_info =
    static_cast<const auth_ssl_server_cert_info_t *>(apr_hash_get(parameters,
        AUTH_PARAM_SSL_SERVER_CERT_INFO,
        APR_HASH_KEY_STRING));

  *credentials = NULL;
  *iter_baton = NULL;

  // We can accept only unknown certificate authority.
  if (*failures & WEBDAV_AUTH_SSL_UNKNOWNCA)
  {
    bool ok;

    WEBDAV_ERR(windows_validate_certificate(&ok, cert_info->ascii_cert, pool));

    // Windows thinks that certificate is ok.
    if (ok)
    {
      // Clear failure flag.
      *failures &= ~WEBDAV_AUTH_SSL_UNKNOWNCA;
    }
  }

  // If all failures are cleared now, we return the creds
  if (!*failures)
  {
    auth_cred_ssl_server_trust_t * creds =
      static_cast<auth_cred_ssl_server_trust_t *>(apr_pcalloc(pool, sizeof(*creds)));
    creds->may_save = FALSE; // No need to save it.
    *credentials = creds;
  }

  return WEBDAV_NO_ERROR;
}

static const auth_provider_t windows_server_trust_provider =
{
  AUTH_CRED_SSL_SERVER_TRUST,
  windows_ssl_server_trust_first_credentials,
  NULL,
  NULL,
};

// Public API
static void
auth_get_windows_ssl_server_trust_provider(
  auth_provider_object_t ** provider,
  apr_pool_t * pool)
{
  auth_provider_object_t * po =
    static_cast<auth_provider_object_t *>(apr_pcalloc(pool, sizeof(*po)));

  po->vtable = &windows_server_trust_provider;
  *provider = po;
}

//------------------------------------------------------------------------------
// from username_providers.c

// Username-only Provider
static error_t
username_first_creds(
  void ** credentials,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  const char * username = static_cast<const char *>(apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_DEFAULT_USERNAME,
    APR_HASH_KEY_STRING));
  bool may_save = !!username;
  error_t err = 0;

  // If we don't have a usename yet, try the auth cache
  if (!username)
  {
    apr_hash_t * creds_hash = NULL;

    TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(parameters,
      CONST_FS_KEY,
      APR_HASH_KEY_STRING));
    assert(fs);
    // Try to load credentials from a file on disk, based on the
    // realmstring.  Don't throw an error, though: if something went
    // wrong reading the file, no big deal.  What really matters is that
    // we failed to get the creds, so allow the auth system to try the
    // next provider.
    err = config_read_auth_data(&creds_hash, WEBDAV_AUTH_CRED_USERNAME,
                                realmstring, fs,
                                pool);
    error_clear(&err);
    if (!err && creds_hash)
    {
      string_t * str = static_cast<string_t *>(apr_hash_get(creds_hash, AUTHN_USERNAME_KEY,
                       APR_HASH_KEY_STRING));
      if (str && str->data)
        username = str->data;
    }
  }

  // If that failed, ask the OS for the username
  if (!username)
    username = user_get_name(pool);

  if (username)
  {
    auth_cred_simple_t * creds =
      static_cast<auth_cred_simple_t *>(apr_pcalloc(pool, sizeof(*creds)));
    creds->username = username;
    creds->may_save = may_save;
    *credentials = creds;
  }
  else
    *credentials = NULL;

  *iter_baton = NULL;

  return WEBDAV_NO_ERROR;
}

static error_t
username_save_creds(
  bool * saved,
  void * credentials,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  auth_cred_simple_t * creds =
    static_cast<auth_cred_simple_t *>(credentials);
  apr_hash_t * creds_hash = NULL;
  error_t err;

  *saved = FALSE;

  if (!creds->may_save)
    return WEBDAV_NO_ERROR;

  // Put the credentials in a hash and save it to disk
  creds_hash = apr_hash_make(pool);
  apr_hash_set(creds_hash, AUTHN_USERNAME_KEY, APR_HASH_KEY_STRING,
    string_create(creds->username, pool));

  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);
  err = config_write_auth_data(creds_hash, WEBDAV_AUTH_CRED_USERNAME,
    realmstring,
    fs,
    pool);
  error_clear(&err);
  *saved = !err;

  return WEBDAV_NO_ERROR;
}

static const auth_provider_t username_provider =
{
  WEBDAV_AUTH_CRED_USERNAME,
  username_first_creds,
  NULL,
  username_save_creds
};

// Public API
static void
auth_get_username_provider(
  auth_provider_object_t ** provider,
  apr_pool_t * pool)
{
  auth_provider_object_t * po =
    static_cast<auth_provider_object_t *>(apr_pcalloc(pool, sizeof(*po)));

  po->vtable = &username_provider;
  *provider = po;
}

// Baton type for username-only prompting.
typedef struct username_prompt_provider_baton_t
{
  auth_username_prompt_func_t prompt_func;
  void * prompt_baton;

  // how many times to re-prompt after the first one fails
  int retry_limit;
} username_prompt_provider_baton_t;

// Iteration baton type for username-only prompting.
typedef struct username_prompt_iter_baton_t
{
  // how many times we've reprompted
  int retries;

} username_prompt_iter_baton_t;

// Helper Functions
static error_t
prompt_for_username_creds(
  auth_cred_username_t ** cred_p,
  username_prompt_provider_baton_t * pb,
  apr_hash_t * parameters,
  const char * realmstring,
  bool first_time,
  bool may_save,
  apr_pool_t * pool)
{
  const char * def_username = NULL;

  *cred_p = NULL;

  // If we're allowed to check for default usernames, do so.
  if (first_time)
    def_username = static_cast<const char *>(apr_hash_get(parameters,
                   WEBDAV_AUTH_PARAM_DEFAULT_USERNAME,
                   APR_HASH_KEY_STRING));

  // If we have defaults, just build the cred here and return it.
  // I do wonder why this is here instead of in a separate
  // 'defaults' provider that would run before the prompt
  // provider... Hmmm.

  if (def_username)
  {
    *cred_p = static_cast<auth_cred_username_t *>(apr_pcalloc(pool, sizeof(**cred_p)));
    (*cred_p)->username = apr_pstrdup(pool, def_username);
    (*cred_p)->may_save = TRUE;
  }
  else
  {
    WEBDAV_ERR(pb->prompt_func(cred_p, pb->prompt_baton, realmstring,
                               may_save, pool));
  }

  return WEBDAV_NO_ERROR;
}

// Our first attempt will use any default username passed
// in, and prompt for the remaining stuff.
static error_t
username_prompt_first_creds(
  void ** credentials_p,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  username_prompt_provider_baton_t * pb =
    static_cast<username_prompt_provider_baton_t *>(provider_baton);
  username_prompt_iter_baton_t * ibaton =
    static_cast<username_prompt_iter_baton_t *>(apr_pcalloc(pool, sizeof(*ibaton)));
  const char * no_auth_cache = static_cast<const char *>(apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_NO_AUTH_CACHE,
    APR_HASH_KEY_STRING));

  WEBDAV_ERR(prompt_for_username_creds((auth_cred_username_t **) credentials_p, pb,
    parameters, realmstring, TRUE, !no_auth_cache, pool));

  ibaton->retries = 0;
  *iter_baton = ibaton;

  return WEBDAV_NO_ERROR;
}

// Subsequent attempts to fetch will ignore the default username
// value, and simply re-prompt for the username, up to a maximum of
// ib->pb->retry_limit.
static error_t
username_prompt_next_creds(
  void ** credentials_p,
  void * iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  username_prompt_iter_baton_t * ib =
    static_cast<username_prompt_iter_baton_t *>(iter_baton);
  username_prompt_provider_baton_t * pb =
    static_cast<username_prompt_provider_baton_t *>(provider_baton);
  const char * no_auth_cache = static_cast<const char *>(apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_NO_AUTH_CACHE,
    APR_HASH_KEY_STRING));

  if ((pb->retry_limit >= 0) && (ib->retries >= pb->retry_limit))
  {
    // give up, go on to next provider.
    *credentials_p = NULL;
    return WEBDAV_NO_ERROR;
  }
  ib->retries++;

  return prompt_for_username_creds((auth_cred_username_t **) credentials_p, pb,
    parameters, realmstring, FALSE, !no_auth_cache, pool);
}

static const auth_provider_t username_prompt_provider =
{
  WEBDAV_AUTH_CRED_USERNAME,
  username_prompt_first_creds,
  username_prompt_next_creds,
  NULL,
};

// Public API
static void
auth_get_username_prompt_provider(
  auth_provider_object_t ** provider,
  auth_username_prompt_func_t prompt_func,
  void * prompt_baton,
  int retry_limit,
  apr_pool_t * pool)
{
  auth_provider_object_t * po =
    static_cast<auth_provider_object_t *>(apr_pcalloc(pool, sizeof(*po)));
  username_prompt_provider_baton_t * pb =
    static_cast<username_prompt_provider_baton_t *>(apr_pcalloc(pool, sizeof(*pb)));

  pb->prompt_func = prompt_func;
  pb->prompt_baton = prompt_baton;
  pb->retry_limit = retry_limit;

  po->vtable = &username_prompt_provider;
  po->provider_baton = pb;
  *provider = po;
}

//------------------------------------------------------------------------------
// from auth.c

static void
auth_baton_set_parameter(
  auth_baton_t * auth_baton,
  const char * name,
  const void * value)
{
  apr_hash_set(auth_baton->parameters, name, APR_HASH_KEY_STRING, value);
}

static const void *
auth_baton_get_parameter(
  auth_baton_t * auth_baton,
  const char * name)
{
  return apr_hash_get(auth_baton->parameters, name, APR_HASH_KEY_STRING);
}

static error_t
auth_first_credentials(
  void ** credentials,
  auth_iterstate_t ** state,
  const char * cred_kind,
  const char * realmstring,
  auth_baton_t * auth_baton,
  apr_pool_t * pool)
{
  int i = 0;
  provider_set_t * table = NULL;
  auth_provider_object_t * provider = NULL;
  void * creds = NULL;
  void * iter_baton = NULL;
  bool got_first = FALSE;
  auth_iterstate_t * iterstate = NULL;
  const char * cache_key = NULL;

  // Get the appropriate table of providers for CRED_KIND.
  table = static_cast<provider_set_t *>(apr_hash_get(auth_baton->tables, cred_kind, APR_HASH_KEY_STRING));
  if (!table)
    return error_createf(WEBDAV_ERR_AUTHN_NO_PROVIDER, NULL,
      "No provider registered for '%s' credentials",
      cred_kind);

  // First, see if we have cached creds in the auth_baton.
  cache_key = apr_pstrcat(pool, cred_kind, ":", realmstring, (char *)NULL);
  creds = static_cast<void *>(apr_hash_get(auth_baton->creds_cache,
    cache_key, APR_HASH_KEY_STRING));
  if (creds)
  {
    got_first = false;
  }
  else
  // If not, find a provider that can give "first" credentials.
  {
    // Find a provider that can give "first" credentials.
    for (i = 0; i < table->providers->nelts; i++)
    {
      provider = APR_ARRAY_IDX(table->providers, i,
        auth_provider_object_t *);
      WEBDAV_ERR(provider->vtable->first_credentials(
        &creds, &iter_baton, provider->provider_baton,
        auth_baton->parameters, realmstring, auth_baton->pool));

      if (creds != NULL)
      {
        got_first = true;
        break;
      }
    }
  }

  if (!creds)
    *state = NULL;
  else
  {
    // Build an abstract iteration state.
    iterstate = static_cast<auth_iterstate_t *>(apr_pcalloc(pool, sizeof(*iterstate)));
    iterstate->table = table;
    iterstate->provider_idx = i;
    iterstate->got_first = got_first;
    iterstate->provider_iter_baton = iter_baton;
    iterstate->realmstring = apr_pstrdup(pool, realmstring);
    iterstate->cache_key = cache_key;
    iterstate->auth_baton = auth_baton;
    *state = iterstate;

    // Put the creds in the cache
    apr_hash_set(auth_baton->creds_cache,
                 apr_pstrdup(auth_baton->pool, cache_key),
                 APR_HASH_KEY_STRING,
                 creds);
  }

  *credentials = creds;

  return WEBDAV_NO_ERROR;
}

static error_t
auth_next_credentials(
  void ** credentials,
  auth_iterstate_t * state,
  apr_pool_t * pool)
{
  auth_baton_t * auth_baton = state->auth_baton;
  auth_provider_object_t * provider = NULL;
  provider_set_t * table = state->table;
  void * creds = NULL;

  // Continue traversing the table from where we left off.
  for (/* no init */;
       state->provider_idx < table->providers->nelts;
       state->provider_idx++)
  {
    provider = APR_ARRAY_IDX(table->providers,
      state->provider_idx,
      auth_provider_object_t *);
    if (!state->got_first)
    {
      WEBDAV_ERR(provider->vtable->first_credentials(
        &creds, &(state->provider_iter_baton),
        provider->provider_baton, auth_baton->parameters,
        state->realmstring, auth_baton->pool));
      state->got_first = TRUE;
    }
    else
    {
      if (provider->vtable->next_credentials)
        WEBDAV_ERR(provider->vtable->next_credentials(
          &creds, state->provider_iter_baton,
          provider->provider_baton, auth_baton->parameters,
          state->realmstring, auth_baton->pool));
    }

    if (creds != NULL)
    {
      // Put the creds in the cache
      apr_hash_set(auth_baton->creds_cache,
        state->cache_key, APR_HASH_KEY_STRING,
        creds);
      break;
    }

    state->got_first = FALSE;
  }

  *credentials = creds;

  return WEBDAV_NO_ERROR;
}

static error_t
auth_save_credentials(
  auth_iterstate_t * state,
  apr_pool_t * pool)
{
  int i = 0;
  auth_provider_object_t * provider = NULL;
  bool save_succeeded = FALSE;
  const char * no_auth_cache = NULL;
  auth_baton_t * auth_baton = NULL;
  void * creds = NULL;

  if (!state || state->table->providers->nelts <= state->provider_idx)
    return WEBDAV_NO_ERROR;

  auth_baton = state->auth_baton;
  creds = apr_hash_get(state->auth_baton->creds_cache,
    state->cache_key, APR_HASH_KEY_STRING);
  if (!creds)
    return WEBDAV_NO_ERROR;

  // Do not save the creds if AUTH_PARAM_NO_AUTH_CACHE is set
  no_auth_cache = static_cast<const char *>(apr_hash_get(auth_baton->parameters,
    AUTH_PARAM_NO_AUTH_CACHE,
    APR_HASH_KEY_STRING));
  if (no_auth_cache)
    return WEBDAV_NO_ERROR;

  // First, try to save the creds using the provider that produced them.
  provider = APR_ARRAY_IDX(state->table->providers,
    state->provider_idx,
    auth_provider_object_t *);
  if (provider->vtable->save_credentials)
    WEBDAV_ERR(provider->vtable->save_credentials(&save_succeeded,
               creds,
               provider->provider_baton,
               auth_baton->parameters,
               state->realmstring,
               pool));
  if (save_succeeded)
    return WEBDAV_NO_ERROR;

  // Otherwise, loop from the top of the list, asking every provider
  // to attempt a save.  todo: someday optimize so we don't
  // necessarily start from the top of the list.
  for (i = 0; i < state->table->providers->nelts; i++)
  {
    provider = APR_ARRAY_IDX(state->table->providers, i,
      auth_provider_object_t *);
    if (provider->vtable->save_credentials)
      WEBDAV_ERR(provider->vtable->save_credentials(
        &save_succeeded, creds,
        provider->provider_baton,
        auth_baton->parameters,
        state->realmstring,
        pool));

    if (save_succeeded)
      break;
  }

  // notice that at the moment, if no provider can save, there's
  // no way the caller will know.

  return WEBDAV_NO_ERROR;
}

static error_t
auth_get_platform_specific_provider(
  auth_provider_object_t ** provider,
  const char * provider_name,
  const char * provider_type,
  apr_pool_t * pool)
{
  *provider = NULL;

  {
    if (strcmp(provider_name, "windows") == 0 &&
        strcmp(provider_type, "simple") == 0)
    {
      auth_get_windows_simple_provider(provider, pool);
    }
    else if (strcmp(provider_name, "windows") == 0 &&
            strcmp(provider_type, "ssl_client_cert_pw") == 0)
    {
      auth_get_windows_ssl_client_cert_pw_provider(provider, pool);
    }
    else if (strcmp(provider_name, "windows") == 0 &&
            strcmp(provider_type, "ssl_server_trust") == 0)
    {
      auth_get_windows_ssl_server_trust_provider(provider, pool);
    }
  }

  return WEBDAV_NO_ERROR;
}

#define WEBDAV_MAYBE_ADD_PROVIDER(list, p) \
  { if (p) APR_ARRAY_PUSH(list, auth_provider_object_t *) = p; }

static error_t
auth_get_platform_specific_client_providers(
  apr_array_header_t ** providers,
  apr_pool_t * pool)
{
  auth_provider_object_t * provider;
  const char * password_stores_config_option;
  apr_array_header_t * password_stores;
  int i;

  password_stores_config_option = "windows-cryptoapi";

  *providers = apr_array_make(pool, 12, sizeof(auth_provider_object_t *));

  password_stores = cstring_split(password_stores_config_option, " ,", TRUE, pool);

  for (i = 0; i < password_stores->nelts; i++)
  {
    const char * password_store = APR_ARRAY_IDX(password_stores, i,
                                  const char *);

    // Windows
    if (apr_strnatcmp(password_store, "windows-cryptoapi") == 0)
    {
      WEBDAV_ERR(auth_get_platform_specific_provider(&provider,
                 "windows",
                 "simple",
                 pool));

      WEBDAV_MAYBE_ADD_PROVIDER(*providers, provider);

      WEBDAV_ERR(auth_get_platform_specific_provider(&provider,
                 "windows",
                 "ssl_client_cert_pw",
                 pool));

      WEBDAV_MAYBE_ADD_PROVIDER(*providers, provider);

      continue;
    }

    return error_createf(WEBDAV_ERR_BAD_CONFIG_VALUE, NULL,
                         "Invalid config: unknown password store "
                         "'%s'",
                         password_store);
  }

  return WEBDAV_NO_ERROR;
}

//------------------------------------------------------------------------------
// from dirent_uri.c

// TRUE if s is the canonical empty path, FALSE otherwise
#define PATH_IS_EMPTY(s) ((s)[0] == '\0')

// Path separator for local filesystem
#define WEBDAV_PATH_LOCAL_SEPARATOR '\\'

// Path type definition. Used only by internal functions.
typedef enum path_type_t
{
  type_uri,
  type_dirent,
  type_relpath
} path_type_t;

// Locale insensitive tolower() for converting parts of dirents and urls
// while canonicalizing
static char
canonicalize_to_lower(char c)
{
  if (!IsUpperCaseLetter(c))
    return c;
  else
    return c - 'A' + 'a';
}

// Locale insensitive toupper() for converting parts of dirents and urls
// while canonicalizing
static char
canonicalize_to_upper(char c)
{
  if (!IsLowerCaseLetter(c))
    return c;
  else
    return c - 'a' + 'A';
}

// Return the canonicalized version of PATH, of type TYPE, allocated in POOL.

static const char *
canonicalize(
  path_type_t type,
  const char * path,
  apr_pool_t * pool)
{
  char * canon = NULL, *dst = NULL;
  const char * src = NULL;
  size_t seglen = 0;
  size_t schemelen = 0;
  size_t canon_segments = 0;
  bool url = FALSE;
  char * schema_data = NULL;

  // "" is already canonical, so just return it; note that later code
  // depends on path not being zero-length.
  if (PATH_IS_EMPTY(path))
  {
    assert(type != type_uri);
    return "";
  }

  dst = canon = static_cast<char *>(apr_pcalloc(pool, strlen(path) + 1));

  // If this is supposed to be an URI, it should start with
  // "scheme://".  We'll copy the scheme, host name, etc. to DST and
  // set URL = TRUE.
  src = path;
  if (type == type_uri)
  {
    assert(*src != '/');

    while (*src && (*src != '/') && (*src != ':'))
      src++;

    if ((*src == ':') && (*(src+1) == '/') && (*(src+2) == '/'))
    {
      const char * seg;

      url = TRUE;

      // Found a scheme, convert to lowercase and copy to dst.
      src = path;
      while (*src != ':')
      {
        *(dst++) = canonicalize_to_lower((*src++));
        schemelen++;
      }
      *(dst++) = ':';
      *(dst++) = '/';
      *(dst++) = '/';
      src += 3;
      schemelen += 3;

      // This might be the hostname
      seg = src;
      while (*src && (*src != '/') && (*src != '@'))
        src++;

      if (*src == '@')
      {
        // Copy the username & password.
        seglen = src - seg + 1;
        memcpy(dst, seg, seglen);
        dst += seglen;
        src++;
      }
      else
        src = seg;

      // Found a hostname, convert to lowercase and copy to dst.
      while (*src && (*src != '/') && (*src != ':'))
        *(dst++) = canonicalize_to_lower((*src++));

      if (*src == ':')
      {
        // We probably have a port number: Is it a default portnumber
        // which doesn't belong in a canonical url?
        if ((src[1] == '8') && (src[2] == '0') &&
            ((src[3] == '/') || !src[3]) &&
            !strncmp(canon, "http:", 5))
        {
          src += 3;
        }
        else if ((src[1] == '4') && (src[2] == '4') && (src[3] == '3') &&
                 ((src[4] == '/') || !src[4]) &&
                 !strncmp(canon, "https:", 6))
        {
          src += 4;
        }
        else if ((src[1] == '/') || !src[1])
        {
          src += 1;
        }

        while (*src && (*src != '/'))
          *(dst++) = canonicalize_to_lower((*src++));
      }

      // Copy trailing slash, or null-terminator.
      *(dst) = *(src);

      // Move src and dst forward only if we are not
      // at null-terminator yet.
      if (*src)
      {
        src++;
        dst++;
        schema_data = dst;
      }

      canon_segments = 1;
    }
  }

  // Copy to DST any separator or drive letter that must come before the
  // first regular path segment.
  if (!url && type != type_relpath)
  {
    src = path;
    // If this is an absolute path, then just copy over the initial
    // separator character.
    if (*src == '/')
    {
      *(dst++) = *(src++);

      // On Windows permit two leading separator characters which means an
      // UNC path.
      if ((type == type_dirent) && (*src == '/'))
        *(dst++) = *(src++);
    }
    // On Windows the first segment can be a drive letter, which we normalize
    // to upper case.
    else if ((type == type_dirent) &&
            IsLetter(*src) &&
            (src[1] == ':'))
    {
      *(dst++) = canonicalize_to_upper(*(src++));
      // Leave the ':' to be processed as (or as part of) a path segment
      // by the following code block, so we need not care whether it has
      // a slash after it.
    }
  }

  while (*src)
  {
    // Parse each segment, finding the closing '/' (which might look
    // like '%2F' for URIs).
    const char * next = src;
    size_t slash_len = 0;

    while (*next &&
           (next[0] != '/') &&
           (!((type == type_uri) && (next[0] == '%') && (next[1] == '2') &&
                (canonicalize_to_upper(next[2]) == 'F'))))
    {
      ++next;
    }

    // Record how long our "slash" is.
    if (next[0] == '/')
      slash_len = 1;
    else if ((type == type_uri) && (next[0] == '%'))
      slash_len = 3;

    seglen = next - src;

    if ((seglen == 0) ||
        ((seglen == 1) && (src[0] == '.')) ||
        ((type == type_uri) && (seglen == 3) && (src[0] == '%') && (src[1] == '2') &&
         (canonicalize_to_upper(src[2]) == 'E')))
    {
      // Empty or noop segment, so do nothing.  (For URIs, '%2E'
      // is equivalent to '.').
    }
    // If this is the first path segment of a file:// URI and it contains a
    // windows drive letter, convert the drive letter to upper case.
    else if (url && (canon_segments == 1) && (seglen == 2) &&
            (strncmp(canon, "file:", 5) == 0) &&
            IsLowerCaseLetter(src[0]) && (src[1] == ':'))
    {
      *(dst++) = canonicalize_to_upper(src[0]);
      *(dst++) = ':';
      if (*next)
        *(dst++) = *next;
      canon_segments++;
    }
    else
    {
      // An actual segment, append it to the destination path
      memcpy(dst, src, seglen);
      dst += seglen;
      if (slash_len)
      {
        *(dst++) = '/';
      }
      canon_segments++;
    }

    // Skip over trailing slash to the next segment.
    src = next + slash_len;
  }

  *dst = '\0';

  // Skip leading double slashes when there are less than 2
  // canon segments. UNC paths *MUST* have two segments.
  if ((type == type_dirent) && (canon[0] == '/') && (canon[1] == '/'))
  {
    if (canon_segments < 2)
      return canon + 1;
    else
    {
      // Now we're sure this is a valid UNC path, convert the server name
      // (the first path segment) to lowercase as Windows treats it as case
      // insensitive.
      // Note: normally the share name is treated as case insensitive too,
      // but it seems to be possible to configure Samba to treat those as
      // case sensitive, so better leave that alone.
      for (dst = canon + 2; *dst && (*dst != '/'); dst++)
        *dst = canonicalize_to_lower(*dst);
    }
  }

  // Check the normalization of characters in a uri
  if (schema_data)
  {
    int need_extra = 0;
    src = schema_data;

    while (*src)
    {
      switch (*src)
      {
        case '/':
          break;
        case '%':
          if (!ctype_isxdigit(*(src+1)) ||
              !ctype_isxdigit(*(src+2)))
            need_extra += 2;
          else
            src += 2;
          break;
        default:
          if (!uri_char_validity[(unsigned char)*src])
            need_extra += 2;
          break;
      }
      src++;
    }

    if (need_extra > 0)
    {
      size_t pre_schema_size = (size_t)(schema_data - canon);

      dst = static_cast<char *>(apr_pcalloc(pool, (size_t)(src - canon) + need_extra + 1));
      memcpy(dst, canon, pre_schema_size);
      canon = dst;

      dst += pre_schema_size;
    }
    else
      dst = schema_data;

    src = schema_data;

    while (*src)
    {
      switch (*src)
      {
        case '/':
          *(dst++) = '/';
          break;
        case '%':
          if (!ctype_isxdigit(*(src+1)) ||
              !ctype_isxdigit(*(src+2)))
          {
            *(dst++) = '%';
            *(dst++) = '2';
            *(dst++) = '5';
          }
          else
          {
            char digitz[3];
            int val;

            digitz[0] = *(++src);
            digitz[1] = *(++src);
            digitz[2] = 0;

            val = (int)strtol(digitz, NULL, 16);

            if (uri_char_validity[(unsigned char)val])
              *(dst++) = (char)val;
            else
            {
              *(dst++) = '%';
              *(dst++) = canonicalize_to_upper(digitz[0]);
              *(dst++) = canonicalize_to_upper(digitz[1]);
            }
          }
          break;
        default:
          if (!uri_char_validity[(unsigned char)*src])
          {
            apr_snprintf(dst, 4, "%%%02X", (unsigned char)*src);
            dst += 3;
          }
          else
            *(dst++) = *src;
          break;
      }
      src++;
    }
    *dst = '\0';
  }

  return canon;
}

static const char *
uri_canonicalize(
  const char * uri,
  apr_pool_t * pool)
{
  return canonicalize(type_uri, uri, pool);
}

static const char *
relpath_canonicalize(
  const char * relpath,
  apr_pool_t * pool)
{
  return canonicalize(type_relpath, relpath, pool);
}

static const char *
fspath_canonicalize(
  const char * fspath,
  apr_pool_t * pool)
{
  if ((fspath[0] == '/') && (fspath[1] == '\0'))
    return "/";

  return apr_pstrcat(pool, "/", relpath_canonicalize(fspath, pool),
    (char *)NULL);
}

// Examine PATH as a potential URI, and return a substring of PATH
// that immediately follows the (scheme):// portion of the URI, or
// NULL if PATH doesn't appear to be a valid URI. The returned value
// is not allocated -- it shares memory with PATH.
static const char *
skip_uri_scheme(
  const char * path)
{
  size_t j = 0;

  // A scheme is terminated by a : and cannot contain any /'s.
  for (j = 0; path[j] && (path[j] != ':'); ++j)
    if (path[j] == '/')
      return NULL;

  if ((j > 0) && (path[j] == ':') && (path[j+1] == '/') && (path[j+2] == '/'))
    return path + j + 3;

  return NULL;
}

static bool
path_is_url(
  const char * path)
{
  // This function is reaaaaaaaaaaaaaally stupid right now.
  // We're just going to look for:
  //   (scheme)://(optional_stuff)
  // Where (scheme) has no ':' or '/' characters.
  // Someday it might be nice to have an actual URI parser here.

  return skip_uri_scheme(path) != NULL;
}

static const char *
urlpath_canonicalize(
  const char * uri,
  apr_pool_t * pool)
{
  if (path_is_url(uri))
  {
    uri = uri_canonicalize(uri, pool);
  }
  else
  {
    uri = fspath_canonicalize(uri, pool);
    // Do a little dance to normalize hex encoding.
    uri = path_uri_decode(uri, pool);
    uri = path_uri_encode(uri, pool);
  }
  return uri;
}

// We decided against using apr_filepath_root here because of the negative
// performance impact (creating a pool and converting strings ).
static bool
dirent_is_root(
  const char * dirent,
  apr_size_t len)
{
  // On Windows and Cygwin, 'H:' or 'H:/' (where 'H' is any letter)
  // are also root directories
  if ((len == 2 || ((len == 3) && (dirent[2] == '/'))) &&
      (dirent[1] == ':') &&
      IsLetter(dirent[0]))
    return TRUE;

  // On Windows and Cygwin //server/share is a root directory,
  // and on Cygwin //drive is a drive alias
  if ((len >= 2) && (dirent[0] == '/') && (dirent[1] == '/') &&
      (dirent[len - 1] != '/'))
  {
    int segments = 0;
    for (size_t i = len; i >= 2; i--)
    {
      if (dirent[i] == '/')
      {
        segments++;
        if (segments > 1)
          return FALSE;
      }
    }
    return (segments == 1); // //drive is invalid on plain Windows
  }

  // directory is root if it's equal to '/'
  if ((len == 1) && (dirent[0] == '/'))
    return TRUE;

  return FALSE;
}

static bool
relpath_is_canonical(
  const char * relpath)
{
  const char * ptr = relpath, *seg = relpath;

  // RELPATH is canonical if it has:
  // - no '.' segments
  // - no start and closing '/'
  // - no '//'

  if (*relpath == '\0')
    return TRUE;

  if (*ptr == '/')
    return FALSE;

  // Now validate the rest of the path.
  while (1)
  {
    apr_size_t seglen = ptr - seg;

    if ((seglen == 1) && (*seg == '.'))
      return FALSE;  //  /./

    if ((*ptr == '/') && (*(ptr+1) == '/'))
      return FALSE;  //  //

    if (!*ptr && (*(ptr - 1) == '/'))
      return FALSE;  // foo/

    if (!*ptr)
      break;

    if (*ptr == '/')
      ptr++;
    seg = ptr;

    while (*ptr && (*ptr != '/'))
      ptr++;
  }

  return TRUE;
}

static const char *
relpath_basename(
  const char * relpath,
  apr_pool_t * pool)
{
  apr_size_t len = strlen(relpath);
  apr_size_t start = 0;

  assert(relpath_is_canonical(relpath));

  start = len;
  while ((start > 0) && (relpath[start - 1] != '/'))
    --start;

  if (pool)
    return apr_pstrmemdup(pool, relpath + start, len - start);
  else
    return relpath + start;
}

static char *
relpath_join(
  const char * base,
  const char * component,
  apr_pool_t * pool)
{
  apr_size_t blen = strlen(base);
  apr_size_t clen = strlen(component);
  char * path = NULL;

  assert(relpath_is_canonical(base));
  assert(relpath_is_canonical(component));

  // If either is empty return the other
  if (blen == 0)
    return static_cast<char *>(apr_pmemdup(pool, component, clen + 1));
  if (clen == 0)
    return static_cast<char *>(apr_pmemdup(pool, base, blen + 1));

  path = static_cast<char *>(apr_pcalloc(pool, blen + 1 + clen + 1));
  memcpy(path, base, blen);
  path[blen] = '/';
  memcpy(path + blen + 1, component, clen + 1);

  return path;
}

static const char *
dirent_canonicalize(
  const char * dirent,
  apr_pool_t * pool)
{
  const char * dst = canonicalize(type_dirent, dirent, pool);

  // Handle a specific case on Windows where path == "X:/". Here we have to
  // append the final '/', as path_canonicalize will chop this of.
  if (IsLetter(dirent[0]) &&
      (dirent[1] == ':') && (dirent[2] == '/') &&
      (dst[3] == '\0'))
  {
    char * dst_slash = static_cast<char *>(apr_pcalloc(pool, 4));
    dst_slash[0] = canonicalize_to_upper(dirent[0]);
    dst_slash[1] = ':';
    dst_slash[2] = '/';
    dst_slash[3] = '\0';

    return dst_slash;
  }

  return dst;
}

static bool
dirent_is_canonical(
  const char * dirent,
  apr_pool_t * pool)
{
  const char * ptr = dirent;
  if (*ptr == '/')
  {
    ptr++;
    // Check for UNC paths
    if (*ptr == '/')
    {
      // TODO: Scan hostname and sharename and fall back to part code

      // Fall back to old implementation
      return (strcmp(dirent, dirent_canonicalize(dirent, pool)) == 0);
    }
  }
  else if (IsLetter(*ptr) &&
          (ptr[1] == ':'))
  {
    // The only canonical drive names are "A:"..."Z:", no lower case
    if (!IsUpperCaseLetter(*ptr))
      return FALSE;

    ptr += 2;

    if (*ptr == '/')
      ptr++;
  }

  return relpath_is_canonical(ptr);
}

static const char *
dirent_basename(
  const char * dirent,
  apr_pool_t * pool)
{
  apr_size_t len = strlen(dirent);
  apr_size_t start = 0;

  assert(!pool || dirent_is_canonical(dirent, pool));

  if (dirent_is_root(dirent, len))
  {
    return "";
  }
  else
  {
    start = len;
    while ((start > 0) && (dirent[start - 1] != '/') &&
      (dirent[start - 1] != ':'))
    {
      --start;
    }
  }

  if (pool)
    return apr_pstrmemdup(pool, dirent + start, len - start);
  else
    return dirent + start;
}

static const char *
uri_skip_ancestor(
  const char * parent_uri,
  const char * child_uri)
{
  apr_size_t len = strlen(parent_uri);

  if (0 != strncmp(parent_uri, child_uri, len))
    return NULL; // parent_uri is no ancestor of child_uri

  if (child_uri[len] == 0)
    return ""; // parent_uri == child_uri

  if (child_uri[len] == '/')
    return child_uri + len + 1;

  return NULL;
}

static const char *
uri_skip_ancestor(
  const char * parent_uri,
  const char * child_uri,
  apr_pool_t * result_pool)
{
  const char * result = uri_skip_ancestor(parent_uri, child_uri);

  return result ? path_uri_decode(result, result_pool) : NULL;
}

static bool
dirent_is_rooted(
  const char * dirent)
{
  if (!dirent)
    return FALSE;

  // Root on all systems
  if (dirent[0] == '/')
    return TRUE;

  // On Windows, dirent is also absolute when it starts with 'H:' or 'H:/'
  // where 'H' is any letter.
  if (IsLetter(dirent[0]) &&
      (dirent[1] == ':'))
  {
    return TRUE;
  }

  return FALSE;
}

static const char *
is_child(
  path_type_t type,
  const char * path1,
  const char * path2,
  apr_pool_t * pool)
{
  apr_size_t i = 0;

  // Allow "" and "foo" or "H:foo" to be parent/child
  if (WEBDAV_PATH_IS_EMPTY(path1))                // "" is the parent
  {
    if (WEBDAV_PATH_IS_EMPTY(path2))             // "" not a child
    {
      return NULL;
    }

    // check if this is an absolute path
    if ((type == type_uri) ||
        (type == type_dirent && dirent_is_rooted(path2)))
    {
      return NULL;
    }
    else
    {
      // everything else is child
      return pool ? apr_pstrdup(pool, path2) : path2;
    }
  }

  for (i = 0; path1[i] && path2[i]; i++)
    if (path1[i] != path2[i])
    {
      return NULL;
    }

  /* FIXME: This comment does not really match
     the checks made in the code it refers to:
     There are two cases that are parent/child
          ...      path1[i] == '\0'
          .../foo  path2[i] == '/'
      or
          /        path1[i] == '\0'
          /foo     path2[i] != '/'

     Other root paths (like X:/) fall under the former case:
          X:/        path1[i] == '\0'
          X:/foo     path2[i] != '/'

     Check for '//' to avoid matching '/' and '//srv'.
  */
  if ((path1[i] == '\0') && path2[i])
  {
    if ((path1[i - 1] == '/') ||
        ((type == type_dirent) && path1[i - 1] == ':'))
    {
      if (path2[i] == '/')
        /* .../
         * ..../
         *     i */
        return NULL;
      else
        /* .../
         * .../foo
         *     i */
        return pool ? apr_pstrdup(pool, path2 + i) : path2 + i;
    }
    else if (path2[i] == '/')
    {
      if (path2[i + 1])
        /* ...
         * .../foo
         *    i   */
        return pool ? apr_pstrdup(pool, path2 + i + 1) : path2 + i + 1;
      else
        /* ...
         * .../
         *    i   */
        return NULL;
    }
  }

  // Otherwise, path2 isn't a child.
  return NULL;
}

static const char *
uri_is_child(
  const char * parent_uri,
  const char * child_uri,
  apr_pool_t * pool)
{
  const char * relpath = NULL;

  assert(pool); // hysterical raisins.

  relpath = is_child(type_uri, parent_uri, child_uri, pool);
  if (relpath)
    relpath = path_uri_decode(relpath, pool);
  return relpath;
}

static bool
dirent_is_absolute(
  const char * dirent)
{
  if (!dirent)
    return FALSE;

  // dirent is absolute if it starts with '/' on non-Windows platforms
  // or with '//' on Windows platforms
  if ((dirent[0] == '/') &&
      (dirent[1] == '/')) // Single '/' depends on current drive
  {
    return TRUE;
  }
  // On Windows, dirent is also absolute when it starts with 'H:/'
  // where 'H' is any letter.
  if (IsUpperCaseLetter(dirent[0]) &&
      (dirent[1] == ':') && (dirent[2] == '/'))
  {
    return TRUE;
  }

  return FALSE;
}

static error_t
dirent_get_absolute(
  const char ** pabsolute,
  const char * relative,
  apr_pool_t * pool)
{
  char * buffer = NULL;
  apr_status_t apr_err = 0;
  const char * path_apr = NULL;

  WEBDAV_ERR_ASSERT(!path_is_url(relative));

  // Merge the current working directory with the relative dirent.
  WEBDAV_ERR(path_cstring_from_utf8(&path_apr, relative, pool));

  apr_err = apr_filepath_merge(&buffer, NULL,
    path_apr,
    APR_FILEPATH_NOTRELATIVE,
    pool);
  if (apr_err)
  {
    // In some cases when the passed path or its ancestor(s) do not exist
    // or no longer exist apr returns an error.

    // In many of these cases we would like to return a path anyway, when the
    // passed path was already a safe absolute path. So check for that now to
    // avoid an error.

    // dirent_is_absolute() doesn't perform the necessary checks to see
    // if the path doesn't need post processing to be in the canonical absolute
    // format.

    if (dirent_is_absolute(relative) &&
        dirent_is_canonical(relative, pool) &&
        !path_is_backpath_present(relative))
      {
        *pabsolute = apr_pstrdup(pool, relative);
        return WEBDAV_NO_ERROR;
      }

    return error_createf(WEBDAV_ERR_BAD_FILENAME,
      NULL,
      "Couldn't determine absolute path of '%s'", relative);
  }

  WEBDAV_ERR(path_cstring_to_utf8(pabsolute, buffer, pool));
  *pabsolute = dirent_canonicalize(*pabsolute, pool);
  return WEBDAV_NO_ERROR;
}

//------------------------------------------------------------------------------
// from atomic.c

#define atomic_t apr_uint32_t

// Magic values for atomic initialization

#define WEBDAV_ATOMIC_UNINITIALIZED 0
#define WEBDAV_ATOMIC_START_INIT    1
#define WEBDAV_ATOMIC_INIT_FAILED   2
#define WEBDAV_ATOMIC_INITIALIZED   3

static error_t
atomic_init_once(
  volatile atomic_t * global_status,
  error_t (*init_func)(void *,apr_pool_t *),
  void * baton,
  apr_pool_t * pool)
{
  // We have to call init_func exactly once.  Because APR
  // doesn't have statically-initialized mutexes, we implement a poor
  // man's spinlock using atomic_cas.
  atomic_t status = apr_atomic_cas32(global_status,
    WEBDAV_ATOMIC_START_INIT,
    WEBDAV_ATOMIC_UNINITIALIZED);

  if (status == WEBDAV_ATOMIC_UNINITIALIZED)
  {
    error_t err = init_func(baton, pool);
    if (err)
    {
#if APR_HAS_THREADS
      // Tell other threads that the initialization failed.
      apr_atomic_cas32(global_status,
        WEBDAV_ATOMIC_INIT_FAILED,
        WEBDAV_ATOMIC_START_INIT);
#endif
      return error_create(WEBDAV_ERR_ATOMIC_INIT_FAILURE, &err,
        "Couldn't perform atomic initialization");
    }
    apr_atomic_cas32(global_status,
      WEBDAV_ATOMIC_INITIALIZED,
      WEBDAV_ATOMIC_START_INIT);
  }
#if APR_HAS_THREADS
  // Wait for whichever thread is performing initialization to finish.
  // XXX FIXME: Should we have a maximum wait here, like we have in
  // the Windows file IO spinner?
  else while (status != WEBDAV_ATOMIC_INITIALIZED)
  {
    if (status == WEBDAV_ATOMIC_INIT_FAILED)
      return error_create(WEBDAV_ERR_ATOMIC_INIT_FAILURE, NULL,
        "Couldn't perform atomic initialization");

    apr_sleep(APR_USEC_PER_SEC / 1000);
    status = apr_atomic_cas32(global_status,
      WEBDAV_ATOMIC_UNINITIALIZED,
      WEBDAV_ATOMIC_UNINITIALIZED);
  }
#endif // APR_HAS_THREADS

  return WEBDAV_NO_ERROR;
}

//------------------------------------------------------------------------------
// from io.c

#define RETRY_MAX_ATTEMPTS 2
#define RETRY_INITIAL_SLEEP 1000
#define RETRY_MAX_SLEEP 128000

// Suppress warning: Condition is always true
#pragma warn -8008
#define RETRY_LOOP(err, expr, retry_test, sleep_test)                      \
  do                                                                       \
  {                                                                        \
    apr_status_t os_err = APR_TO_OS_ERROR(err);                            \
    int sleep_count = RETRY_INITIAL_SLEEP;                                 \
    int retries;                                                           \
    for (retries = 0;                                                      \
         retries < RETRY_MAX_ATTEMPTS && (retry_test);                     \
         os_err = APR_TO_OS_ERROR(err))                                    \
    {                                                                      \
      if (sleep_test)                                                      \
      {                                                                    \
        ++retries;                                                         \
        apr_sleep(sleep_count);                                            \
        if (sleep_count < RETRY_MAX_SLEEP)                                 \
          sleep_count *= 2;                                                \
      }                                                                    \
      (err) = (expr);                                                      \
    }                                                                      \
  }                                                                        \
  while (0)

#if defined(EDEADLK) && APR_HAS_THREADS
#define FILE_LOCK_RETRY_LOOP(err, expr)                                    \
  RETRY_LOOP(err,                                                          \
             expr,                                                         \
             (APR_STATUS_IS_EINTR(err) || os_err == EDEADLK),              \
             (!APR_STATUS_IS_EINTR(err)))
#else
#define FILE_LOCK_RETRY_LOOP(err, expr)                                    \
  RETRY_LOOP(err,                                                          \
             expr,                                                         \
             (APR_STATUS_IS_EINTR(err)),                                   \
             0)
#endif

#ifndef WIN32_RETRY_LOOP
#if !defined(WEBDAV_NO_WIN32_RETRY_LOOP)
#define WIN32_RETRY_LOOP(err, expr)                                        \
  RETRY_LOOP(err, expr, (os_err == ERROR_ACCESS_DENIED ||                  \
                         os_err == ERROR_SHARING_VIOLATION ||              \
                         os_err == ERROR_DIR_NOT_EMPTY),                   \
             1)
#else
#define WIN32_RETRY_LOOP(err, expr) ((void)0)
#endif
#endif

// Not specifying any of these means no removal at all.
typedef enum io_file_del_t
{
  // No deletion ever
  io_file_del_none = 0,
  // Remove when the file is closed
  io_file_del_on_close,
  // Remove when the associated pool is cleared
  io_file_del_on_pool_cleanup
} io_file_del_t;

// Wrapper for apr_file_open(), taking an APR-encoded filename.
static apr_status_t
file_open(
  apr_file_t ** file,
  const char * fname_apr,
  apr_int32_t flags,
  apr_fileperms_t perm,
  bool retry_on_failure,
  apr_pool_t * pool)
{
  apr_status_t status = apr_file_open(file, fname_apr, flags, perm, pool);

  if (retry_on_failure)
  {
    WIN32_RETRY_LOOP(status, apr_file_open(file, fname_apr, flags, perm, pool));
  }
  return status;
}
#pragma warn +8008

static error_t
io_file_open(
  apr_file_t ** new_file,
  const char * fname,
  apr_int32_t flags,
  apr_fileperms_t perms,
  apr_pool_t * pool)
{
  const char * fname_apr = NULL;
  apr_status_t status = 0;

  WEBDAV_ERR(cstring_from_utf8(&fname_apr, fname, pool));
  status = file_open(new_file, fname_apr, flags | APR_BINARY, perms,
    /* retry_on_failure */ FALSE,
    pool);

  if (status)
    return error_wrap_apr(status, "Can't open file '%s'", fname);
  else
    return WEBDAV_NO_ERROR;
}

static error_t
io_file_open_writable(
  apr_file_t ** new_file,
  apr_os_file_t * thefile,
  apr_int32_t flags,
  apr_pool_t * pool)
{
  apr_status_t status = 0;

  status = apr_os_file_put(new_file, thefile,
    flags | APR_BINARY,
    pool);

  if (status)
    return error_wrap_apr(status, "Can't open file");
  else
    return WEBDAV_NO_ERROR;
}

// Wrapper for apr_file_name_get(), passing out a UTF8-encoded filename.
static error_t
io_file_name_get(
  const char ** filename,
  apr_file_t * file,
  apr_pool_t * pool)
{
  const char * fname_apr = NULL;
  apr_status_t status = 0;

  status = apr_file_name_get(&fname_apr, file);
  if (status)
    return error_wrap_apr(status, "Can't get file name");

  if (fname_apr)
    WEBDAV_ERR(path_cstring_to_utf8(filename, fname_apr, pool));
  else
    *filename = NULL;

  return WEBDAV_NO_ERROR;
}

static APR_INLINE error_t
do_io_file_wrapper_cleanup(
  apr_file_t * file,
  apr_status_t status,
  const char * msg,
  const char * msg_no_name,
  apr_pool_t * pool)
{
  const char * name = NULL;
  error_t err = 0;

  if (!status)
    return WEBDAV_NO_ERROR;

  err = io_file_name_get(&name, file, pool);
  if (err)
    name = NULL;
  error_clear(&err);

  // Issue #3014: Return a specific error for broken pipes,
  // with a single element in the error chain.
  if (APR_STATUS_IS_EPIPE(status))
    return error_create(WEBDAV_ERR_IO_PIPE_WRITE_ERROR, NULL, NULL);

  if (name)
    return error_wrap_apr(status, msg, name);
  else
    return error_wrap_apr(status, "%s", msg_no_name);
}

static error_t
io_file_close(
  apr_file_t * file,
  apr_pool_t * pool)
{
  return do_io_file_wrapper_cleanup(file, apr_file_close(file),
    "Can't close file '%s'",
    "Can't close stream",
    pool);
}

static error_t
io_file_getc(
  char * ch,
  apr_file_t * file,
  apr_pool_t * pool)
{
  return do_io_file_wrapper_cleanup(file, apr_file_getc(ch, file),
    "Can't read file '%s'",
    "Can't read stream",
    pool);
}

static error_t
io_file_write_full(
  apr_file_t * file,
  const void * buf,
  apr_size_t nbytes,
  apr_size_t * bytes_written,
  apr_pool_t * pool)
{
  // We cannot simply call apr_file_write_full on Win32 as it may fail
  // for larger values of NBYTES. In that case, we have to emulate the
  // "_full" part here. Thus, always call apr_file_write directly on
  // Win32 as this minimizes overhead for small data buffers.
#define MAXBUFSIZE 64*1024
  apr_size_t bw = nbytes;
  apr_size_t to_write = nbytes;

  // try a simple "write everything at once" first
  apr_status_t rv = apr_file_write(file, buf, &bw);
  buf = (char *)buf + bw;
  to_write -= bw;

  // if the OS cannot handle that, use smaller chunks
  if ((rv == APR_FROM_OS_ERROR(ERROR_NOT_ENOUGH_MEMORY)) &&
      (nbytes > MAXBUFSIZE))
  {
    do
    {
      bw = to_write > MAXBUFSIZE ? MAXBUFSIZE : to_write;
      rv = apr_file_write(file, buf, &bw);
      buf = (char *)buf + bw;
      to_write -= bw;
    }
    while (rv == APR_SUCCESS && to_write > 0);
  }

  // bytes_written may actually be NULL
  if (bytes_written)
    *bytes_written = nbytes - to_write;
#undef MAXBUFSIZE

  return error_trace(do_io_file_wrapper_cleanup(file, rv,
    "Can't write to file '%s'",
    "Can't write to stream",
    pool));
}

static error_t
io_file_seek(
  apr_file_t * file,
  apr_seek_where_t where,
  apr_off_t * offset,
  apr_pool_t * pool)
{
  return do_io_file_wrapper_cleanup(file, apr_file_seek(file, where, offset),
    "Can't set position pointer in file '%s'",
    "Can't set position pointer in stream",
    pool);
}

static error_t
io_file_putc(
  char ch,
  apr_file_t * file,
  apr_pool_t * pool)
{
  return do_io_file_wrapper_cleanup(file, apr_file_putc(ch, file),
    "Can't write file '%s'",
    "Can't write stream",
    pool);
}

static error_t
io_file_read(
  apr_file_t * file,
  void * buf,
  apr_size_t * nbytes,
  apr_pool_t * pool)
{
  return do_io_file_wrapper_cleanup(file, apr_file_read(file, buf, nbytes),
   "Can't read file '%s'",
   "Can't read stream",
   pool);
}

// Local wrapper of path_cstring_from_utf8() that does no copying on
// operating systems where APR always uses utf-8 as native path format
static error_t
cstring_from_utf8(
  const char ** path_apr,
  const char * path_utf8,
  apr_pool_t * pool)
{
  *path_apr = path_utf8;
  return WEBDAV_NO_ERROR;
}

static error_t
io_file_read_full2(
  apr_file_t * file,
  void * buf,
  apr_size_t nbytes,
  apr_size_t * bytes_read,
  bool * hit_eof,
  apr_pool_t * pool)
{
  apr_status_t status = apr_file_read_full(file, buf, nbytes, bytes_read);
  if (hit_eof)
  {
    if (APR_STATUS_IS_EOF(status))
    {
      *hit_eof = TRUE;
      return WEBDAV_NO_ERROR;
    }
    else
      *hit_eof = FALSE;
  }

  return do_io_file_wrapper_cleanup(file, status,
    "Can't read file '%s'",
    "Can't read stream",
    pool);
}

//------------------------------------------------------------------------------
// from svn_io.h

// Read handler function for a generic stream. see stream_t.
typedef error_t (*read_fn_t)(
  void * baton,
  char * buffer,
  apr_size_t * len);

// Skip data handler function for a generic stream. see stream_t
// and stream_skip().

typedef error_t (*stream_skip_fn_t)(
  void * baton,
  apr_size_t len);

// Write handler function for a generic stream. see stream_t.
typedef error_t (*write_fn_t)(
  void * baton,
  const char * data,
  apr_size_t * len);

// Close handler function for a generic stream. see stream_t.
typedef error_t (*close_fn_t)(void * baton);

typedef struct stream_mark_t stream_mark_t;

typedef error_t (*stream_mark_fn_t)(
  void * baton,
  stream_mark_t ** mark,
  apr_pool_t * pool);

typedef error_t (*stream_seek_fn_t)(
  void * baton,
  const stream_mark_t * mark);

typedef bool (*stream_is_buffered_fn_t)(void * baton);

//------------------------------------------------------------------------------
// from stream.c

typedef struct stream_t
{
  void * baton;
  read_fn_t read_fn;
  stream_skip_fn_t skip_fn;
  write_fn_t write_fn;
  close_fn_t close_fn;
  stream_mark_fn_t mark_fn;
  stream_seek_fn_t seek_fn;
  stream_is_buffered_fn_t is_buffered_fn;
} stream_t;

// Generic stream for APR files
struct baton_apr_t
{
  apr_file_t * file;
  apr_pool_t * pool;
};

// stream_mark_t for streams backed by APR files.
struct mark_apr_t
{
  apr_off_t off;
};

static error_t
read_handler_apr(
  void * baton,
  char * buffer,
  apr_size_t * len)
{
  struct baton_apr_t * btn = static_cast<baton_apr_t *>(baton);
  error_t err = 0;
  bool eof = FALSE;

  if (*len == 1)
  {
    err = io_file_getc(buffer, btn->file, btn->pool);
    if (err)
    {
      *len = 0;
      if (APR_STATUS_IS_EOF(err))
      {
        error_clear(&err);
        err = WEBDAV_NO_ERROR;
      }
    }
  }
  else
    err = io_file_read_full2(btn->file, buffer, *len, len,
      &eof, btn->pool);

  return err;
}

static error_t
skip_handler_apr(
  void * baton,
  apr_size_t len)
{
  struct baton_apr_t * btn = static_cast<baton_apr_t *>(baton);
  apr_off_t offset = len;

  return io_file_seek(btn->file, APR_CUR, &offset, btn->pool);
}

static error_t
write_handler_apr(
  void * baton,
  const char * data,
  apr_size_t * len)
{
  struct baton_apr_t * btn = static_cast<baton_apr_t *>(baton);
  error_t err = 0;

  if (*len == 1)
  {
    err = io_file_putc(*data, btn->file, btn->pool);
    if (err)
      *len = 0;
  }
  else
    err = io_file_write_full(btn->file, data, *len, len, btn->pool);

  return err;
}

static error_t
close_handler_apr(
  void * baton)
{
  struct baton_apr_t * btn = static_cast<baton_apr_t *>(baton);

  return io_file_close(btn->file, btn->pool);
}

static error_t
mark_handler_apr(
  void * baton,
  stream_mark_t ** mark,
  apr_pool_t * pool)
{
  struct baton_apr_t * btn = static_cast<baton_apr_t *>(baton);
  struct mark_apr_t * mark_apr = 0;

  mark_apr = static_cast<mark_apr_t *>(apr_pcalloc(pool, sizeof(*mark_apr)));
  mark_apr->off = 0;
  WEBDAV_ERR(io_file_seek(btn->file, APR_CUR, &mark_apr->off, btn->pool));
  *mark = (stream_mark_t *)mark_apr;
  return WEBDAV_NO_ERROR;
}

static error_t
seek_handler_apr(
  void * baton,
  const stream_mark_t * mark)
{
  struct baton_apr_t * btn = static_cast<baton_apr_t *>(baton);
  apr_off_t offset = (mark != NULL) ? ((const struct mark_apr_t *)mark)->off : 0;

  WEBDAV_ERR(io_file_seek(btn->file, APR_SET, &offset, btn->pool));

  return WEBDAV_NO_ERROR;
}

static bool
is_buffered_handler_apr(
  void * baton)
{
  struct baton_apr_t * btn = static_cast<baton_apr_t *>(baton);
  return (apr_file_flags_get(btn->file) & APR_BUFFERED) != 0;
}

static stream_t *
stream_create(
  void * baton,
  apr_pool_t * pool)
{
  stream_t * stream = static_cast<stream_t *>(apr_pcalloc(pool, sizeof(*stream)));
  stream->baton = baton;
  stream->read_fn = NULL;
  stream->skip_fn = NULL;
  stream->write_fn = NULL;
  stream->close_fn = NULL;
  stream->mark_fn = NULL;
  stream->seek_fn = NULL;
  stream->is_buffered_fn = NULL;
  return stream;
}

static void
stream_set_read(
  stream_t * stream,
  read_fn_t read_fn)
{
  stream->read_fn = read_fn;
}

static void
stream_set_skip(
  stream_t * stream,
  stream_skip_fn_t skip_fn)
{
  stream->skip_fn = skip_fn;
}

static void
stream_set_write(
  stream_t * stream,
  write_fn_t write_fn)
{
  stream->write_fn = write_fn;
}

static void
stream_set_close(
  stream_t * stream,
  close_fn_t close_fn)
{
  stream->close_fn = close_fn;
}

static void
stream_set_mark(
  stream_t * stream,
  stream_mark_fn_t mark_fn)
{
  stream->mark_fn = mark_fn;
}

static void
stream_set_seek(
  stream_t * stream,
  stream_seek_fn_t seek_fn)
{
  stream->seek_fn = seek_fn;
}

static void
stream_set_is_buffered(
  stream_t * stream,
  stream_is_buffered_fn_t is_buffered_fn)
{
  stream->is_buffered_fn = is_buffered_fn;
}

static error_t
read_handler_empty(
  void * baton,
  char * buffer,
  apr_size_t * len)
{
  *len = 0;
  return WEBDAV_NO_ERROR;
}

static error_t
write_handler_empty(
  void * baton,
  const char * data,
  apr_size_t * len)
{
  return WEBDAV_NO_ERROR;
}

static error_t
mark_handler_empty(
  void * baton,
  stream_mark_t ** mark,
  apr_pool_t * pool)
{
  *mark = NULL; // Seek to start of stream marker
  return WEBDAV_NO_ERROR;
}

static error_t
seek_handler_empty(
  void * baton,
  const stream_mark_t * mark)
{
  return WEBDAV_NO_ERROR;
}

static bool
is_buffered_handler_empty(void * baton)
{
  return FALSE;
}

static stream_t *
stream_empty(
  apr_pool_t * pool)
{
  stream_t * stream = stream_create(NULL, pool);
  stream_set_read(stream, read_handler_empty);
  stream_set_write(stream, write_handler_empty);
  stream_set_mark(stream, mark_handler_empty);
  stream_set_seek(stream, seek_handler_empty);
  stream_set_is_buffered(stream, is_buffered_handler_empty);
  return stream;
}

static stream_t *
stream_from_aprfile2(
  apr_file_t * file,
  bool disown,
  apr_pool_t * pool)
{
  stream_t * stream = NULL;

  if (file == NULL)
    return stream_empty(pool);

  baton_apr_t * baton = static_cast<baton_apr_t *>(apr_pcalloc(pool, sizeof(*baton)));
  baton->file = file;
  baton->pool = pool;
  stream = stream_create(baton, pool);
  stream_set_read(stream, read_handler_apr);
  stream_set_write(stream, write_handler_apr);
  stream_set_skip(stream, skip_handler_apr);
  stream_set_mark(stream, mark_handler_apr);
  stream_set_seek(stream, seek_handler_apr);
  stream_set_is_buffered(stream, is_buffered_handler_apr);

  if (!disown)
    stream_set_close(stream, close_handler_apr);

  return stream;
}

static error_t
stream_open_writable(
  stream_t ** stream,
  apr_os_file_t * thefile,
  apr_pool_t * result_pool,
  apr_pool_t * scratch_pool)
{
  apr_file_t * file = NULL;
  WEBDAV_ERR(io_file_open_writable(&file,
    thefile,
    APR_WRITE
    | APR_BUFFERED
    | APR_BINARY
    | APR_CREATE,
    // | APR_EXCL,
    result_pool));
  *stream = stream_from_aprfile2(file, FALSE, result_pool);

  return WEBDAV_NO_ERROR;
}

static error_t
stream_write(
  stream_t * stream,
  const char * data,
  apr_size_t * len)
{
  WEBDAV_ERR_ASSERT(stream->write_fn != NULL);
  return stream->write_fn(stream->baton, data, len);
}

static error_t
stream_close(
  stream_t * stream)
{
  if (stream->close_fn == NULL)
    return WEBDAV_NO_ERROR;
  return stream->close_fn(stream->baton);
}

//------------------------------------------------------------------------------
// from util.c

static apr_status_t
dav_request_cleanup(
  void * baton);

static apr_status_t
dav_request_sess_cleanup(
  void * baton)
{
  neon_request_t * req = static_cast<neon_request_t *>(baton);

  // Make sure we don't run the 'child' cleanup anymore:
  // the pool it refers to probably doesn't exist anymore when it
  // finally does get run if it hasn't by now.
  apr_pool_cleanup_kill(req->pool, req, dav_request_cleanup);

  if (req->ne_req)
    ne_request_destroy(req->ne_req);

  return APR_SUCCESS;
}

static apr_status_t
dav_request_cleanup(
  void * baton)
{
  neon_request_t * req = static_cast<neon_request_t *>(baton);
  apr_pool_cleanup_run(req->sess->pool, req, dav_request_sess_cleanup);

  return APR_SUCCESS;
}

// Return a path-absolute relative URL, given a URL reference (which may
// be absolute or relative).
static const char *
path_from_url(
  const char * url)
{
  const char * p = NULL;

  // Look for the scheme/authority separator.  Stop if we see a path
  // separator - that indicates that this definitely isn't an absolute URL.
  for (p = url; *p; p++)
  {
    if ((*p == ':') || (*p == '/'))
      break;
  }

  // Check whether we found the scheme/authority separator.
  if ((*p++ != ':') || (*p++ != '/') || (*p++ != '/'))
  {
    // No separator, so it must already be relative.
    return url;
  }

  // Find the end of the authority section, indicated by the start of
  // a path, query, or fragment section.
  for (; *p; p++)
  {
    if ((*p == '/') || (*p == '?') || (*p == '#'))
      break;
  }

  // Return a pointer to the rest of the URL, or to "/" if there
  // was no next section.
  return *p == '\0' ? "/" : p;
}

// Simple multi-status parser
// For the purpose of 'simple' requests which - if it weren't
// for our custom error parser - could use the ne_basic.h interfaces.

// List of XML elements expected in 207 Multi-Status responses.
static const neon_xml_elm_t multistatus_elements[] =
{
  { "DAV:", "multistatus", ELEM_multistatus, 0 },
  { "DAV:", "response", ELEM_response, 0 },
  {
    "DAV:", "responsedescription", ELEM_responsedescription,
    NEON_XML_CDATA
  },
  { "DAV:", "status", ELEM_status, NEON_XML_CDATA },
  { "DAV:", "href", ELEM_href, NEON_XML_CDATA },
  { "DAV:", "propstat", ELEM_propstat, NEON_XML_CDATA },
  { "DAV:", "prop", ELEM_prop, NEON_XML_CDATA },

  // We start out basic and are not interested in other elements
  { "", "", ELEM_unknown, 0 },

  { NULL }
};

static const int multistatus_nesting_table[][5] =
{
  { ELEM_root, ELEM_multistatus, NEON_XML_INVALID },
  {
    ELEM_multistatus, ELEM_response, ELEM_responsedescription,
    NEON_XML_DECLINE
  },
  { ELEM_responsedescription, NEON_XML_INVALID },
  {
    ELEM_response, ELEM_href, ELEM_status, ELEM_propstat,
    NEON_XML_DECLINE
  },
  { ELEM_status, NEON_XML_INVALID },
  { ELEM_href, NEON_XML_INVALID },
  {
    ELEM_propstat, ELEM_prop, ELEM_status, ELEM_responsedescription,
    NEON_XML_INVALID
  },
  { ELEM_prop, NEON_XML_DECLINE },
  { NEON_XML_DECLINE },
};

static int
multistatus_validate_element(
  int parent,
  int child)
{
  int i = 0;
  int j = 0;

  while ((parent != multistatus_nesting_table[i][0]) &&
         (multistatus_nesting_table[i][0] > 0 || i == 0))
    i++;

  if (parent == multistatus_nesting_table[i][0])
    while ((multistatus_nesting_table[i][++j] != child) &&
           (multistatus_nesting_table[i][j] > 0))
    ;

  return multistatus_nesting_table[i][j];
}

typedef struct multistatus_baton_t
{
  stringbuf_t * want_cdata;
  stringbuf_t * cdata;

  bool in_propstat;
  bool propstat_has_error;
  stringbuf_t * propname;
  stringbuf_t * propstat_description;

  neon_request_t * req;
  stringbuf_t * description;
  bool contains_error;
  bool contains_precondition_error;
} multistatus_baton_t;

// Implements neon_startelm_cb_t.
static error_t
start_207_element(
  int * elem,
  void * baton,
  int parent,
  const char * nspace,
  const char * name,
  const char ** atts)
{
  multistatus_baton_t * b = static_cast<multistatus_baton_t *>(baton);
  const neon_xml_elm_t * elm =
    neon_lookup_xml_elem(multistatus_elements, nspace, name);
  *elem = elm ? multistatus_validate_element(parent, elm->id) : NEON_XML_DECLINE;

  if (parent == ELEM_prop)
  {
    stringbuf_setempty(b->propname);
    if (strcmp(nspace, "DAV:") == 0)
      stringbuf_set(b->propname, "DAV:");

    stringbuf_appendcstr(b->propname, name);
  }

  if (*elem < 1)  // ! > 0
    return WEBDAV_NO_ERROR;

  switch (*elem)
  {
    case ELEM_propstat:
      b->in_propstat = TRUE;
      b->propstat_has_error = FALSE;
      break;

    default:
      break;
  }

  // We're guaranteed to have ELM now: NEON_XML_DECLINE < 1
  if (elm->flags & NEON_XML_CDATA)
  {
    stringbuf_setempty(b->cdata);
    b->want_cdata = b->cdata;
  }

  return WEBDAV_NO_ERROR;
}

// Implements neon_endelm_cb_t
static error_t
end_207_element(
  void * baton,
  int state,
  const char * nspace,
  const char * name)
{
  multistatus_baton_t * b = static_cast<multistatus_baton_t *>(baton);

  switch (state)
  {
    case ELEM_multistatus:
      if (b->contains_error)
      {
        if (stringbuf_isempty(b->description))
          return error_create(WEBDAV_ERR_DAV_REQUEST_FAILED, NULL,
            "The request response contained at least one error");
        else if (b->contains_precondition_error)
          return error_create(WEBDAV_ERR_FS_PROP_BASEVALUE_MISMATCH, NULL,
                              b->description->data);
        else
          return error_create(WEBDAV_ERR_DAV_REQUEST_FAILED, NULL,
                              b->description->data);
      }
      break;

    case ELEM_responsedescription:
      if (b->in_propstat)
        stringbuf_set(b->propstat_description, b->cdata->data);
      else
      {
        if (!stringbuf_isempty(b->description))
          stringbuf_appendcstr(b->description, "\n");
        stringbuf_appendstr(b->description, b->cdata);
      }
      break;

    case ELEM_status:
    {
      ne_status status;

      if (ne_parse_statusline(b->cdata->data, &status) == 0)
      {
        // I wanted ||=, but I guess the end result is the same
        if (!b->in_propstat)
          b->contains_error |= (status.klass != 2);
        else
          b->propstat_has_error = (status.klass != 2);

        // Handle "412 Precondition Failed" specially
        if (status.code == 412)
          b->contains_precondition_error = TRUE;

        ne_free(status.reason_phrase);
      }
      else
        return error_create(WEBDAV_ERR_DAV_REQUEST_FAILED, NULL,
          "The response contains a non-conforming HTTP status line");
    }
    break;

    case ELEM_propstat:
      b->in_propstat = FALSE;
      b->contains_error |= b->propstat_has_error;
      stringbuf_appendcstr(b->description,
        apr_psprintf(b->req->pool,
          "Error setting property '%s': ",
          b->propname->data));
      stringbuf_appendstr(b->description,
        b->propstat_description);

    default:
      // do nothing
      break;
  }

  // When we have an element which wants cdata,
  // we'll set it all up in start_207_element() again
  b->want_cdata = NULL;

  return WEBDAV_NO_ERROR;
}

// Create a status parser attached to the request REQ.  Detected errors
// will be returned there.
static void
multistatus_parser_create(
  neon_request_t * req)
{
  multistatus_baton_t * b = static_cast<multistatus_baton_t *>(apr_pcalloc(req->pool, sizeof(*b)));

  // Create a parser, attached to REQ. (Ignore the return value.)
  neon_xml_parser_create(req, ne_accept_207,
    start_207_element,
    neon_xml_collect_cdata,
    end_207_element, b);
  b->cdata = stringbuf_create("", req->pool);
  b->description = stringbuf_create("", req->pool);
  b->req = req;

  b->propname = stringbuf_create("", req->pool);
  b->propstat_description = stringbuf_create("", req->pool);
}

static apr_status_t
compressed_body_reader_cleanup(
  void * baton)
{
  if (baton)
    ne_decompress_destroy(static_cast<ne_decompress *>(baton));

  return APR_SUCCESS;
}

// Attach READER as a response reader for the request REQ, with the
// acceptance function ACCPT.  The response body data will be decompressed,
// if compressed, before being passed to READER.  USERDATA will be passed as
// the first argument to the acceptance and reader callbacks.
static void
attach_ne_body_reader(
  neon_request_t * req,
  ne_accept_response accpt,
  ne_block_reader reader,
  void * userdata)
{
  if (req->sess->compression)
  {
    ne_decompress * decompress =
      ne_decompress_reader(req->ne_req, accpt, reader, userdata);

    apr_pool_cleanup_register(req->pool,
      decompress,
      compressed_body_reader_cleanup,
      apr_pool_cleanup_null);
  }
  else
    ne_add_response_body_reader(req->ne_req, accpt, reader, userdata);
}

typedef struct cancellation_baton_t
{
  ne_block_reader real_cb;
  void * real_userdata;
  neon_request_t * req;
} cancellation_baton_t;

static int
cancellation_callback(
  void * userdata,
  const char * block,
  size_t len)
{
  cancellation_baton_t * b = static_cast<cancellation_baton_t *>(userdata);
  neon_session_t * ras = b->req->sess;

  if (ras->callbacks->cancel_func)
  {
    NEON_REQ_ERR(b->req, (ras->callbacks->cancel_func)(ras->callback_baton));
  }
  if (b->req->err)
    return 1;
  else
    return (b->real_cb)(b->real_userdata, block, len);
}

static cancellation_baton_t *
get_cancellation_baton(
  neon_request_t * req,
  ne_block_reader real_cb,
  void * real_userdata,
  apr_pool_t * pool)
{
  cancellation_baton_t * b = static_cast<cancellation_baton_t *>(apr_pcalloc(pool, sizeof(*b)));

  b->real_cb = real_cb;
  b->real_userdata = real_userdata;
  b->req = req;

  return b;
}

typedef struct body_provider_baton_t
{
  neon_request_t * req;
  apr_file_t * body_file;
} body_provider_baton_t;

static ssize_t
ra_neon_body_provider(
  void * userdata,
  char * buffer,
  size_t buflen)
{
  body_provider_baton_t * b = static_cast<body_provider_baton_t *>(userdata);
  neon_request_t * req = b->req;
  apr_file_t * body_file = b->body_file;

  if (req->sess->callbacks &&
      req->sess->callbacks->cancel_func)
  {
    NEON_REQ_ERR(req, (req->sess->callbacks->cancel_func)(
      req->sess->callback_baton));
  }

  if (req->err)
    return -1;

  webdav_pool_clear(req->iterpool);
  if (buflen == 0)
  {
    // This is the beginning of a new body pull. Rewind the file.
    apr_off_t offset = 0;
    NEON_REQ_ERR(b->req, io_file_seek(body_file, APR_SET, &offset, req->iterpool));
    return (req->err ? -1 : 0);
  }
  else
  {
    callback_baton_t * cb = static_cast<callback_baton_t *>(req->sess->callback_baton);

    TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(cb->ctx->auth_baton->parameters,
      CONST_FS_KEY,
      APR_HASH_KEY_STRING));
    assert(fs);
    fs->AdjustToCPSLimit(buflen);

    apr_size_t nbytes = buflen;
    error_t err = io_file_read(body_file, buffer, &nbytes,
      req->iterpool);
    if (err)
    {
      if (APR_STATUS_IS_EOF(err))
      {
        error_clear(&err);
        return 0;
      }

      NEON_REQ_ERR(req, err);
      return -1;
    }
    else
      return (ssize_t)nbytes;
  }
}

static error_t
neon_set_neon_body_provider(
  neon_request_t * req,
  apr_file_t * body_file)
{
  apr_status_t status = 0;
  apr_finfo_t finfo = {0};
  body_provider_baton_t * b = static_cast<body_provider_baton_t *>(apr_pcalloc(req->pool, sizeof(*b)));

  status = apr_file_info_get(&finfo, APR_FINFO_SIZE, body_file);
  if (status)
    return error_wrap_apr(status,
      "Can't calculate the request body size");

  b->body_file = body_file;
  b->req = req;

  ne_set_request_body_provider(req->ne_req, (ne_off_t)finfo.size,
    ra_neon_body_provider, b);

  return WEBDAV_NO_ERROR;
}

// See doc string for neon_parsed_request.
static error_t
parsed_request(
  neon_request_t * req,
  neon_session_t * ras,
  const char * method,
  const char * url,
  const char * body,
  apr_file_t * body_file,
  void set_parser(ne_xml_parser * parser, void * baton),
  neon_startelm_cb_t startelm_cb,
  neon_cdata_cb_t cdata_cb,
  neon_endelm_cb_t endelm_cb,
  void * baton,
  apr_hash_t * extra_headers,
  int * status_code,
  bool check_errors,
  apr_pool_t * pool)
{
  ne_xml_parser * success_parser = NULL;

  if (body == NULL)
    WEBDAV_ERR(neon_set_neon_body_provider(req, body_file));

  // use a symbolic name somewhere for this MIME type?
  ne_add_request_header(req->ne_req, "Content-Type", "text/xml");

  // create a parser to read the normal response body
  success_parser = neon_xml_parser_create(req, NULL,
    startelm_cb, cdata_cb,
    endelm_cb, baton);

  // if our caller is interested in having access to this parser, call
  // the SET_PARSER callback with BATON.
  if (set_parser != NULL)
    set_parser(success_parser, baton);

  // Register the "main" accepter and body-reader with the request --
  // the one to use when the HTTP status is 2XX.
  attach_ne_body_reader(req, ne_accept_2xx, cancellation_callback,
    get_cancellation_baton(req, ne_xml_parse_v,
    success_parser, pool));

  // run the request and get the resulting status code.
  WEBDAV_ERR(neon_request_dispatch(
    status_code, req, extra_headers, body,
    (strcmp(method, "PROPFIND") == 0) ? 207 : 200,
    0,
    check_errors,
    pool));

  WEBDAV_ERR(neon_check_parse_error(method, success_parser, url));

  return WEBDAV_NO_ERROR;
}

static error_t
neon_parsed_request(
  neon_session_t * sess,
  const char * method,
  const char * url,
  const char * body,
  apr_file_t * body_file,
  void set_parser(ne_xml_parser * parser, void * baton),
  neon_startelm_cb_t startelm_cb,
  neon_cdata_cb_t cdata_cb,
  neon_endelm_cb_t endelm_cb,
  void * baton,
  apr_hash_t * extra_headers,
  int * status_code,
  bool check_errors,
  apr_pool_t * pool)
{
  // create/prep the request
  neon_request_t * req = NULL;
  error_t err = 0;

  WEBDAV_ERR(neon_request_create(&req, sess, method, url, pool));

  err = parsed_request(req, sess, method, url, body, body_file,
    set_parser, startelm_cb, cdata_cb, endelm_cb,
    baton, extra_headers, status_code,
    check_errors,
    pool);

  neon_request_destroy(req);
  return err;
}

static error_t
neon_simple_request(
  int * code,
  neon_session_t * ras,
  const char * method,
  const char * url,
  apr_hash_t * extra_headers,
  const char * body,
  int okay_1, int okay_2, apr_pool_t * pool)
{
  neon_request_t * req = NULL;
  error_t err = 0;

  WEBDAV_ERR(neon_request_create(&req, ras, method, url, pool));

  multistatus_parser_create(req);

  // neon_request_dispatch() adds the custom error response
  // reader. Neon will take care of the Content-Length calculation
  err = neon_request_dispatch(code, req, extra_headers,
    body ? body : "",
    okay_1, okay_2, false, pool);
  neon_request_destroy(req);

  return err;
}

static void
neon_add_depth_header(
  apr_hash_t * extra_headers,
  int depth)
{
  assert(extra_headers != NULL);
  assert(depth == NEON_DEPTH_ZERO ||
         depth == NEON_DEPTH_ONE ||
         depth == NEON_DEPTH_INFINITE);
  apr_hash_set(extra_headers, "Depth", APR_HASH_KEY_STRING,
               (depth == NEON_DEPTH_INFINITE) ?
                 "infinity" : (depth == NEON_DEPTH_ZERO) ? "0" : "1");

  return;
}

static const neon_xml_elm_t *
neon_lookup_xml_elem(
  const neon_xml_elm_t * table,
  const char * nspace,
  const char * name)
{
  // placeholder for `unknown' element if it's present
  const neon_xml_elm_t * elem_unknown = NULL;
  const neon_xml_elm_t * elem = NULL;

  for (elem = table; elem->nspace; ++elem)
  {
    if ((strcmp(elem->nspace, nspace) == 0) &&
        (strcmp(elem->name, name) == 0))
      return elem;

    // Use a single loop to save CPU cycles.
    // Maybe this element is defined as `unknown'?
    if (elem->id == ELEM_unknown)
      elem_unknown = elem;
  }

  // ELEM_unknown position in the table or NULL
  return elem_unknown;
}

static error_t
neon_xml_collect_cdata(
  void * baton,
  int state,
  const char * cdata,
  size_t len)
{
  stringbuf_t ** b = static_cast<stringbuf_t **>(baton);

  if (*b)
    stringbuf_appendbytes(*b, cdata, len);

  return WEBDAV_NO_ERROR;
}

// Custom function of type ne_accept_response.
static int
ra_neon_error_accepter(
  void * userdata,
  ne_request * req,
  const ne_status * st)
{
  // Before, this function was being run for *all* responses including
  // the 401 auth challenge.  In neon 0.24.x that was harmless.  But
  // in neon 0.25.0, trying to parse a 401 response as XML using
  // ne_xml_parse_v aborts the response; so the auth hooks never got a
  // chance.
  ne_content_type ctype = {0};

  // Only accept non-2xx responses with text/xml content-type
  if (st->klass != 2 && ne_get_content_type(req, &ctype) == 0)
  {
    int is_xml = (strcmp(ctype.type, "text") == 0) &&
      (strcmp(ctype.subtype, "xml") == 0);
    ne_free(ctype.value);
    return is_xml;
  }
  else
    return 0;
}

static const neon_xml_elm_t error_elements[] =
{
  { "DAV:", "error", ELEM_error, 0 },
  {
    "http://apache.org/dav/xmlns", "human-readable",
    ELEM_human_readable, NEON_XML_CDATA
  },

  // our validator doesn't yet recognize the rich, specific
  // <D:some-condition-failed/> objects as defined by DeltaV.

  { NULL }
};

static error_t
xml_parser_cleanup(void * baton)
{
  ne_xml_destroy(static_cast<ne_xml_parser *>(baton));

  return WEBDAV_NO_ERROR;
}

static ne_xml_parser *
xml_parser_create(
  neon_request_t * req)
{
  ne_xml_parser * p = ne_xml_create();

  // HACK: Set the parser's error to the empty string.  Someday we
  // hope neon will let us have an easy way to tell the difference
  // between XML parsing errors, and errors that occur while handling
  // the XML tags that we get.  Until then, trust that whenever neon
  // has an error somewhere below the API, it sets its own error to
  // something non-empty (the API promises non-NULL, at least).
  ne_xml_set_error(p, "");

  apr_pool_cleanup_register(req->pool, p,
    xml_parser_cleanup,
    apr_pool_cleanup_null);

  return p;
}

static int
validate_error_elements(
  neon_xml_elmid parent,
  neon_xml_elmid child)
{
  switch (parent)
  {
    case ELEM_root:
      if (child == ELEM_error)
        return child;
      else
        return NEON_XML_INVALID;

    case ELEM_error:
      if ((child == ELEM_error) ||
          (child == ELEM_human_readable))
        return child;
      else
        return NEON_XML_DECLINE;  // ignore if something else
                                  // was in there
    default:
      return NEON_XML_DECLINE;
  }

  // NOTREACHED
}

static error_t
generate_error(
  neon_request_t * req,
  apr_pool_t * pool)
{
  int errcode = WEBDAV_ERR_DAV_REQUEST_FAILED;
  const char * context =
    apr_psprintf(req->pool, "%s of '%s'", req->method, req->url);
  const char * msg = NULL;
  const char * hostport = NULL;

  // Convert the return codes.
  switch (req->rv)
  {
    case NE_OK:
      switch (req->code)
      {
        case 404:
          return error_create(WEBDAV_ERR_FS_NOT_FOUND, NULL,
            apr_psprintf(pool, "'%s' path not found", req->url));
        case 403:
          return error_create(WEBDAV_ERR_DAV_FORBIDDEN, NULL,
            apr_psprintf(pool, "Access to '%s' forbidden",
              req->url));

        case 301:
        case 302:
        case 307:
          return error_create(WEBDAV_ERR_DAV_RELOCATED, NULL,
            apr_psprintf(pool, (req->code == 301) ?
              "WebDAV resource moved permanently to '%s';"
              " please relocate"
              : "WebDAV resource moved temporarily to '%s';"
              " please relocate",
              neon_request_get_location(req, pool)));

        default:
          return error_create(errcode, NULL,
             apr_psprintf(pool,
               "Server sent unexpected return value (%d %s) "
               "in response to %s request for '%s'", req->code,
               req->code_desc, req->method, req->url));
      }
    case NE_AUTH:
    case NE_PROXYAUTH:
      errcode = WEBDAV_ERR_NOT_AUTHORIZED;
      // neon >= 0.27 gives a descriptive error message after auth
      // failure; expose this since it's a useful diagnostic e.g. for
      // an unsupported challenge scheme, or a local GSSAPI error due
      // to an expired ticket.
      WEBDAV_ERR(utf_cstring_to_utf8(&msg, ne_get_error(req->ne_sess), pool));
      msg = apr_psprintf(pool, "authorization failed: %s", msg);
      break;

    case NE_CONNECT:
      msg = "could not connect to server";
      break;

    case NE_TIMEOUT:
      msg = "timed out waiting for server";
      break;

    default:
      // Get the error string from neon and convert to UTF-8.
      WEBDAV_ERR(utf_cstring_to_utf8(&msg, ne_get_error(req->ne_sess), pool));
      break;
  }

  // The hostname may contain non-ASCII characters, so convert it to UTF-8.
  WEBDAV_ERR(utf_cstring_to_utf8(&hostport,
    ne_get_server_hostport(req->ne_sess), pool));

  // This is a translation nightmare. Make sure to compose full strings
  // and mark those for translation.
  return error_createf(errcode, NULL, "%s: %s (%s://%s)",
    context, msg, ne_get_scheme(req->ne_sess),
    hostport);
}

typedef struct error_parser_baton
{
  stringbuf_t * want_cdata;
  stringbuf_t * cdata;

  error_t * dst_err;
  error_t tmp_err;
  bool * marshalled_error;
} error_parser_baton_t;

static int
start_err_element(
  void * baton,
  int parent,
  const char * nspace,
  const char * name,
  const char ** atts)
{
  const neon_xml_elm_t * elm = neon_lookup_xml_elem(error_elements, nspace, name);
  int acc = elm ? validate_error_elements(parent, elm->id) : NEON_XML_DECLINE;
  error_parser_baton_t * b = static_cast<error_parser_baton_t *>(baton);
  error_t * err = &(b->tmp_err);

  if (acc < 1)  // ! > 0
    return acc;

  switch (elm->id)
  {
    case ELEM_error:
    {
      // allocate the error_t.  Hopefully the value will be
      // overwritten by the <human-readable> tag, or even someday by
      // a <D:failed-precondition/> tag.
      *err = error_create(APR_EGENERAL, NULL,
        "General svn error from server");
      break;
    }
    case ELEM_human_readable:
    {
      // get the errorcode attribute if present
      const char * errcode_str =
        xml_get_attr_value("errcode", // make constant in
                                      // some mod_dav header?
                           atts);

      if (errcode_str && *err)
      {
        apr_int64_t val;
        error_t err2;

        err2 = cstring_atoi64(&val, errcode_str);
        if (err2)
        {
          error_clear(&err2);
          break;
        }
      }

      break;
    }

    default:
      break;
  }

  switch (elm->id)
  {
    case ELEM_human_readable:
      b->want_cdata = b->cdata;
      stringbuf_setempty(b->want_cdata);
      break;

    default:
      b->want_cdata = NULL;
      break;
  }

  return elm->id;
}

static int
end_err_element(
  void * baton,
  int state,
  const char * nspace,
  const char * name)
{
  error_parser_baton_t * b = static_cast<error_parser_baton_t *>(baton);
  error_t * err = &(b->tmp_err);

  switch (state)
  {
    case ELEM_human_readable:
    {
      if (b->cdata->data && err)
      {
        // On the server dav_error_response_tag() will add a leading
        // and trailing newline if DEBUG_CR is defined in mod_dav.h,
        // so remove any such characters here.
        apr_size_t len;
        const char * cd = b->cdata->data;
        if (*cd == '\n')
          ++cd;
        len = strlen(cd);
        if (len > 0 && cd[len - 1] == '\n')
          --len;
      }
      break;
    }

    case ELEM_error:
    {
      if (b->dst_err)
        error_clear(&b->tmp_err);
      else if (b->tmp_err)
      {
        b->dst_err = &b->tmp_err;
        if (b->marshalled_error)
          *(b->marshalled_error) = TRUE;
      }
      b->tmp_err = NULL;
      break;
    }

    default:
      break;
  }

  return 0;
}

static int
collect_error_cdata(
  void * baton,
  int state,
  const char * cdata,
  size_t len)
{
  stringbuf_t ** b = static_cast<stringbuf_t **>(baton);

  if (*b)
    stringbuf_appendbytes(*b, cdata, len);

  return 0;
}

static apr_status_t
error_parser_baton_cleanup(
  void * baton)
{
  error_parser_baton_t * b = static_cast<error_parser_baton_t *>(baton);

  if (b->tmp_err)
    error_clear(&b->tmp_err);

  return APR_SUCCESS;
}

static ne_xml_parser *
error_parser_create(
  neon_request_t * req)
{
  error_parser_baton_t * b = static_cast<error_parser_baton_t *>(apr_pcalloc(req->pool, sizeof(*b)));
  ne_xml_parser * error_parser = NULL;

  b->dst_err = &(req->err);
  b->marshalled_error = &(req->marshalled_error);
  b->tmp_err = NULL;

  b->want_cdata = NULL;
  b->cdata = stringbuf_create("", req->pool);

  // attach a standard <D:error> body parser to the request
  error_parser = xml_parser_create(req);
  ne_xml_push_handler(error_parser,
    start_err_element,
    collect_error_cdata,
    end_err_element, b);

  apr_pool_cleanup_register(req->pool, b,
    error_parser_baton_cleanup,
    apr_pool_cleanup_null);

  // Register the "error" accepter and body-reader with the request --
  // the one to use when HTTP status is *not* 2XX
  attach_ne_body_reader(req, ra_neon_error_accepter,
    ne_xml_parse_v, error_parser);

  return error_parser;
}

static error_t
neon_maybe_store_auth_info(
  neon_session_t * ras,
  apr_pool_t * pool)
{
  // No auth_baton?  Never mind.
  if (!ras->callbacks->auth_baton)
    return WEBDAV_NO_ERROR;

  // If we ever got credentials, ask the iter_baton to save them.
  return auth_save_credentials(ras->auth_iterstate, pool);
}

// A baton that is used along with a set of Neon ne_startelm_cb,
// ne_cdata_cb, and ne_endelm_cb callbacks to handle conversion
// from our style errors to Neon style errors.
// The underlying callbacks are called, and if errors
// are returned they are stored in this baton and a Neon level
// error code is returned to the parser.

typedef struct parser_wrapper_baton_t
{
  neon_request_t * req;
  ne_xml_parser * parser;

  void * baton;
  neon_startelm_cb_t startelm_cb;
  neon_cdata_cb_t cdata_cb;
  neon_endelm_cb_t endelm_cb;
} parser_wrapper_baton_t;

static int
wrapper_startelm_cb(
  void * baton,
  int parent,
  const char * nspace,
  const char * name,
  const char ** atts)
{
  parser_wrapper_baton_t * pwb = static_cast<parser_wrapper_baton_t *>(baton);
  int elem = NEON_XML_DECLINE;

  if (pwb->startelm_cb)
  {
    NEON_REQ_ERR(pwb->req, pwb->startelm_cb(&elem, pwb->baton, parent, nspace,
      name, atts));
  }

  if (elem == NEON_XML_INVALID)
  {
    NEON_REQ_ERR(pwb->req, error_create(WEBDAV_ERR_XML_MALFORMED, NULL, NULL));
  }

  if (pwb->req->err)
    return NE_XML_ABORT;

  return elem;
}

static int
wrapper_cdata_cb(
  void * baton,
  int state,
  const char * cdata,
  size_t len)
{
  parser_wrapper_baton_t * pwb = static_cast<parser_wrapper_baton_t *>(baton);

  if (pwb->cdata_cb)
  {
    NEON_REQ_ERR(pwb->req, pwb->cdata_cb(pwb->baton, state, cdata, len));
  }

  if (pwb->req->err)
    return NE_XML_ABORT;

  return 0;
}

static int
wrapper_endelm_cb(
  void * baton,
  int state,
  const char * nspace,
  const char * name)
{
  parser_wrapper_baton_t * pwb = static_cast<parser_wrapper_baton_t *>(baton);

  if (pwb->endelm_cb)
  {
    NEON_REQ_ERR(pwb->req, pwb->endelm_cb(pwb->baton, state, nspace, name));
  }

  if (pwb->req->err)
    return NE_XML_ABORT;

  return 0;
}

static error_t
neon_check_parse_error(
  const char * method,
  ne_xml_parser * xml_parser,
  const char * url)
{
  const char * msg = ne_xml_get_error(xml_parser);
  if (msg != NULL && *msg != '\0')
    return error_createf(WEBDAV_ERR_DAV_REQUEST_FAILED, NULL,
      "The %s request returned invalid XML "
      "in the response: %s (%s)",
      method, msg, url);
  return WEBDAV_NO_ERROR;
}

static int
wrapper_reader_cb(
  void * baton,
  const char * data,
  size_t len)
{
  parser_wrapper_baton_t * pwb = static_cast<parser_wrapper_baton_t *>(baton);
  neon_session_t * sess = pwb->req->sess;
  int parser_status = 0;

  if (pwb->req->err)
    return 1;

  if (sess->callbacks->cancel_func)
  {
    NEON_REQ_ERR(pwb->req, (sess->callbacks->cancel_func)(sess->callback_baton));
  }

  if (pwb->req->err)
    return 1;
  if (len)
    parser_status = ne_xml_parse(pwb->parser, data, len);
  if (parser_status)
  {
    // Pass XML parser error.
    NEON_REQ_ERR(pwb->req, neon_check_parse_error(pwb->req->method,
      pwb->parser,
      pwb->req->url));
  }

  return parser_status;
}

// Create a Neon xml parser with callbacks STARTELM_CB, ENDELM_CB and
// CDATA_CB.  The created parser wraps the Neon callbacks and marshals any
// errors returned by the callbacks through the Neon layer.  Any errors
// raised will be returned by neon_request_dispatch() unless
// an earlier error occurred.

// Register a pool cleanup on the pool of REQ to clean up any allocated
// Neon resources.

// Return the new parser.  Also attach it to REQ if ACCPT is non-null.
// ACCPT indicates whether the parser wants to read the response body
// or not.  Pass NULL for ACCPT when you don't want the returned parser
// to be attached to REQ.

static ne_xml_parser *
neon_xml_parser_create(
  neon_request_t * req,
  ne_accept_response accpt,
  neon_startelm_cb_t startelm_cb,
  neon_cdata_cb_t cdata_cb,
  neon_endelm_cb_t endelm_cb,
  void * baton)
{
  ne_xml_parser * p = xml_parser_create(req);
  parser_wrapper_baton_t * pwb = static_cast<parser_wrapper_baton_t *>(apr_pcalloc(req->pool, sizeof(*pwb)));

  pwb->req = req;
  pwb->parser = p;
  pwb->baton = baton;
  pwb->startelm_cb = startelm_cb;
  pwb->cdata_cb = cdata_cb;
  pwb->endelm_cb = endelm_cb;

  ne_xml_push_handler(p,
    wrapper_startelm_cb,
    wrapper_cdata_cb,
    wrapper_endelm_cb, pwb);

  if (accpt)
    attach_ne_body_reader(req, accpt, wrapper_reader_cb, pwb);

  return p;
}

static error_t
neon_request_dispatch(
  int * code_p,
  neon_request_t * req,
  apr_hash_t * extra_headers,
  const char * body,
  int okay_1,
  int okay_2,
  bool check_errors,
  apr_pool_t * pool)
{
  // add any extra headers passed in by caller.
  if (extra_headers != NULL)
  {
    for (apr_hash_index_t * hi = apr_hash_first(pool, extra_headers); hi;
         hi = apr_hash_next(hi))
    {
      const void * key;
      void * val;
      apr_hash_this(hi, &key, NULL, &val);
      ne_add_request_header(req->ne_req,
        static_cast<const char *>(key), static_cast<const char *>(val));
    }
  }

  if (body)
    ne_set_request_body_buffer(req->ne_req, body, strlen(body));

  // attach a standard <D:error> body parser to the request
  ne_xml_parser * error_parser = error_parser_create(req);

  if (check_errors)
    multistatus_parser_create(req);
  // run the request, see what comes back.
  req->rv = ne_request_dispatch(req->ne_req);

  // Save values from the request
  const ne_status * statstruct = ne_get_status(req->ne_req);
  req->code_desc = apr_pstrdup(pool, statstruct->reason_phrase);
  req->code = statstruct->code;

  // If we see a successful request that used authentication, we should store
  // the credentials for future use.
  if ((req->sess->auth_used) && (statstruct->code < 400))
  {
    req->sess->auth_used = FALSE;
    WEBDAV_ERR(neon_maybe_store_auth_info(req->sess, pool));
  }

  if (code_p)
    *code_p = req->code;

  if (!req->marshalled_error && req->err)
    WEBDAV_ERR(req->err);

  // If the status code was one of the two that we expected, then go
  // ahead and return now. IGNORE any marshalled error.
  if ((req->rv == NE_OK) && (req->code == okay_1 || req->code == okay_2))
    return WEBDAV_NO_ERROR;

  // Any other errors? Report them
  if (req->err)
    WEBDAV_ERR(req->err);

  WEBDAV_ERR(neon_check_parse_error(req->method, error_parser, req->url));

  // We either have a neon error, or some other error
  // that we didn't expect.
  return generate_error(req, pool);
}

static const char *
neon_request_get_location(
  neon_request_t * request,
  apr_pool_t * pool)
{
  const char * val = ne_get_response_header(request->ne_req, "Location");
  return val ? urlpath_canonicalize(val, pool) : NULL;
}

static error_t
neon_request_create(
  neon_request_t ** request,
  neon_session_t * sess,
  const char * method, const char * url,
  apr_pool_t * pool)
{
  apr_pool_t * reqpool = webdav_pool_create(pool);
  neon_request_t * req = NULL;
  const char * path = NULL;

  // We never want to send Neon an absolute URL, since that can cause
  // problems with some servers (for example, those that may be accessed
  // using different server names from different locations, or those that
  // want to rewrite the incoming URL).  If the URL passed in is absolute,
  // convert it to a path-absolute relative URL.
  path = path_from_url(url);

  req = static_cast<neon_request_t *>(apr_pcalloc(reqpool, sizeof(*req)));
  req->ne_sess = sess->ne_sess;
  req->ne_req = ne_request_create(req->ne_sess, method, path);
  req->sess = sess;
  req->pool = reqpool;
  req->iterpool = webdav_pool_create(req->pool);
  req->method = apr_pstrdup(req->pool, method);
  req->url = apr_pstrdup(req->pool, url);
  req->rv = -1;

  // Neon resources may be NULL on out-of-memory
  assert(req->ne_req != NULL);
  apr_pool_cleanup_register(sess->pool, req,
    dav_request_sess_cleanup,
    apr_pool_cleanup_null);
  apr_pool_cleanup_register(reqpool, req,
    dav_request_cleanup,
    apr_pool_cleanup_null);
  *request = req;
  return WEBDAV_NO_ERROR;
}

static error_t
get_path_relative_to_session(
  session_t * session,
  const char ** rel_path,
  const char * url,
  apr_pool_t * pool)
{
  const char * sess_url = NULL;
  WEBDAV_ERR(session->vtable->get_session_url(session, &sess_url, pool));
  if (strcmp(sess_url, url) == 0)
  {
    *rel_path = "";
  }
  else
  {
    *rel_path = uri_is_child(sess_url, url, pool);
    if (!*rel_path)
    {
      return error_createf(WEBDAV_ERR_ILLEGAL_URL, NULL,
        "'%s' isn't a child of session URL '%s'", url, sess_url);
    }
  }
  return WEBDAV_NO_ERROR;
}

static error_t
client_path_relative_to_root(
  const char ** rel_path,
  const char * abspath_or_url,
  const char * webdav_root,
  bool include_leading_slash,
  session_t * ra_session,
  apr_pool_t * result_pool,
  apr_pool_t * scratch_pool)
{
  const char * webdav_relpath = NULL;

  if (!path_is_url(abspath_or_url))
  {
    error_createf(WEBDAV_ERR_DAV_NOT_IMPLEMENTED, NULL,
      "not implemented: 'client_path_relative_to_root'");
  }
  // Merge handling passes a root that is not WebDAV resource root
  else if (webdav_root != NULL)
  {
    /*if (!uri_is_ancestor(webdav_root, abspath_or_url))
      return error_createf(WEBDAV_ERR_CLIENT_UNRELATED_RESOURCES, NULL,
        "URL '%s' is not a child of "
        "root URL '%s'",
        abspath_or_url, webdav_root);*/

    webdav_relpath = uri_skip_ancestor(webdav_root, abspath_or_url,
      result_pool);
  }
  else
  {
    error_t err = 0;

    WEBDAV_ERR_ASSERT(ra_session != NULL);

    // Ask the RA layer to create a relative path for us
    err = get_path_relative_to_root(ra_session, &webdav_relpath,
      abspath_or_url, scratch_pool);

    if (err)
    {
      if (err == WEBDAV_ERR_ILLEGAL_URL)
      {
        return error_createf(WEBDAV_ERR_CLIENT_UNRELATED_RESOURCES, &err,
          "URL '%s' is not inside WebDAV resource root", abspath_or_url);
      }

      return error_trace(err);
    }
  }

  if (include_leading_slash)
    *rel_path = apr_pstrcat(result_pool, "/", webdav_relpath, NULL);
  else
    *rel_path = webdav_relpath;

  return WEBDAV_NO_ERROR;
}

static const char *
neon_uri_unparse(
  const ne_uri * uri,
  apr_pool_t * pool)
{
  char * unparsed_uri = NULL;
  const char * result = NULL;

  // Unparse uri.
  unparsed_uri = ne_uri_unparse(uri);

  result = uri_canonicalize(unparsed_uri, pool);

  // Free neon's allocated copy.
  ne_free(unparsed_uri);

  // Return string allocated in result pool.
  return result;
}

typedef struct body_reader_wrapper_baton_t
{
  neon_request_t * req;
  neon_block_reader real_reader;
  void * real_baton;
} body_reader_wrapper_baton_t;

static int
body_reader_wrapper(
  void * userdata,
  const char * data,
  size_t len)
{
  body_reader_wrapper_baton_t * b = static_cast<body_reader_wrapper_baton_t *>(userdata);

  if (b->req->err)
    // We already had an error? Bail out.
    return 1;

  NEON_REQ_ERR(b->req, b->real_reader(b->real_baton, data, len));

  if (b->req->err)
    return 1;

  return 0;
}

static void
neon_add_response_body_reader(
  neon_request_t * req,
  ne_accept_response accpt,
  neon_block_reader reader,
  void * userdata)
{
  body_reader_wrapper_baton_t * b = static_cast<body_reader_wrapper_baton_t *>(apr_pcalloc(req->pool, sizeof(*b)));

  b->req = req;
  b->real_baton = userdata;
  b->real_reader = reader;

  attach_ne_body_reader(req, accpt, body_reader_wrapper, b);
}

//------------------------------------------------------------------------------
// from fetch.c

typedef struct file_read_ctx_t
{
  apr_pool_t * pool;

  // these two are the handler that the editor gave us
  void * handler_baton;

  // if we're receiving an svndiff, this is a parser which places the
  // resulting windows into the above handler/baton.
  stream_t * stream;

} file_read_ctx_t;

typedef struct file_write_ctx_t
{
  stream_t * stream;      // stream to write file contents to
} file_write_ctx_t;

typedef struct custom_get_ctx_t
{
  neon_request_t * req; // Used to propagate errors out of the reader
  int checked_type;             // have we processed ctype yet?

  void * subctx;
  void * callback_baton;
} custom_get_ctx_t;

// Helper for neon_get_file.  This implements
// the neon_block_reader() callback interface.
static error_t
get_file_reader(
  void * userdata,
  const char * buf,
  size_t len)
{
  custom_get_ctx_t * cgc = static_cast<custom_get_ctx_t *>(userdata);

  if (cgc->req->sess->callbacks &&
      cgc->req->sess->callbacks->cancel_func)
  {
    NEON_REQ_ERR(cgc->req, (cgc->req->sess->callbacks->cancel_func)(
      cgc->req->sess->callback_baton));
  }

  assert(cgc->callback_baton);
  callback_baton_t * cb = static_cast<callback_baton_t *>(cgc->callback_baton);

  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(cb->ctx->auth_baton->parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);
  fs->AdjustToCPSLimit(len);

  // The stream we want to push data at.
  file_write_ctx_t * fwc = static_cast<file_write_ctx_t *>(cgc->subctx);
  stream_t * stream = fwc->stream;

  // Write however many bytes were passed in by neon.
  WEBDAV_ERR(stream_write(stream, buf, &len));
  return WEBDAV_NO_ERROR;
}

static error_t
custom_get_request(
  neon_session_t * ras,
  const char * url,
  const char * editor_relpath,
  neon_block_reader reader,
  void * subctx,
  void * cb_baton,
  apr_pool_t * pool)
{
  custom_get_ctx_t cgc = { 0 };
  neon_request_t * request = NULL;
  error_t err = 0;

  WEBDAV_ERR(neon_request_create(&request, ras, "GET", url, pool));

  neon_add_response_body_reader(request, ne_accept_2xx, reader, &cgc);

  // complete initialization of the body reading context
  cgc.req = request;
  cgc.subctx = subctx;
  cgc.callback_baton = cb_baton;

  // run the request
  err = neon_request_dispatch(NULL, request, NULL, NULL,
    200 /* OK */,
    226 /* IM Used */,
    false,
    pool);
  neon_request_destroy(request);

  // The request runner raises internal errors before Neon errors,
  // pass a returned error to our callers

  return err;
}

//------------------------------------------------------------------------------

static error_t
get_file(
  session_t * session,
  const char * path,
  stream_t * stream,
  apr_hash_t ** props,
  apr_pool_t * pool)
{
  WEBDAV_ERR_ASSERT(*path != '/');
  return session->vtable->get_file(session, path,
    stream,
    props, pool);
}

static error_t
get_dir2(
  session_t * session,
  apr_hash_t ** dirents,
  const char * path,
  apr_uint32_t dirent_fields,
  apr_pool_t * pool)
{
  WEBDAV_ERR_ASSERT(*path != '/');
  return session->vtable->get_dir(session, dirents,
    path,
    dirent_fields, pool);
}

static error_t
get_webdav_resource_root2(
  session_t * session,
  const char ** url,
  apr_pool_t * pool)
{
  WEBDAV_ERR(session->vtable->get_webdav_resource_root(session, url, pool));
  *url = *url ? apr_pstrdup(pool, *url) : NULL;
  return WEBDAV_NO_ERROR;
}

static error_t
stat(
  session_t * session,
  const char * path,
  dirent_t ** dirent,
  apr_pool_t * pool)
{
  WEBDAV_ERR_ASSERT(*path != '/');
  return session->vtable->stat(session, path,
    dirent, pool);
}

static error_t
get_path_relative_to_root(
  session_t * session,
  const char ** rel_path,
  const char * url,
  apr_pool_t * pool)
{
  const char * root_url = NULL;
  WEBDAV_ERR(session->vtable->get_webdav_resource_root(session, &root_url, pool));
  if (strcmp(root_url, url) == 0)
  {
    *rel_path = "";
  }
  else
  {
    *rel_path = uri_is_child(root_url, url, pool);
    if (!*rel_path)
    {
      return error_createf(WEBDAV_ERR_ILLEGAL_URL, NULL,
        "'%s' isn't a child of root URL '%s'", url, root_url);
    }
  }

  return WEBDAV_NO_ERROR;
}

static error_t
check_path(
  session_t * session,
  const char * path,
  node_kind_t * kind,
  apr_pool_t * pool)
{
  WEBDAV_ERR_ASSERT(*path != '/');
  return session->vtable->check_path(session, path,
    kind, pool);
}

static error_t
reparent(
  session_t * session,
  const char * url,
  apr_pool_t * pool)
{
  return session->vtable->reparent(session, url, pool);
}

static error_t
session_open(
  session_t ** session_p,
  const char ** corrected_url_p,
  const char * session_URL,
  const callbacks2_t * callbacks,
  void * callback_baton,
  apr_pool_t * pool)
{
  assert(callback_baton);
  // check options, url, prepare parameters, callbacks, auth etc
  apr_pool_t * sesspool = webdav_pool_create(pool);

  // Initialize the return variable.
  *session_p = NULL;

  ne_uri * webdav_URI = NULL;
  error_t err = parse_ne_uri(&webdav_URI, session_URL, sesspool);
  if ((err != WEBDAV_NO_ERROR) || (webdav_URI->host == NULL))
  {
    return error_createf(WEBDAV_ERR_ILLEGAL_URL, NULL,
      "Illegal URL '%s'", session_URL);
  }

  // Auth caching parameters.
  bool store_passwords = WEBDAV_CONFIG_DEFAULT_OPTION_STORE_PASSWORDS;
  bool store_auth_creds = WEBDAV_CONFIG_DEFAULT_OPTION_STORE_AUTH_CREDS;
  const char * store_plaintext_passwords = WEBDAV_CONFIG_DEFAULT_OPTION_STORE_PLAINTEXT_PASSWORDS;
  bool store_pp = WEBDAV_CONFIG_DEFAULT_OPTION_STORE_SSL_CLIENT_CERT_PP;
  const char * store_pp_plaintext = WEBDAV_CONFIG_DEFAULT_OPTION_STORE_SSL_CLIENT_CERT_PP_PLAINTEXT;

  if (callbacks->auth_baton)
  {
    if (auth_baton_get_parameter(callbacks->auth_baton,
      WEBDAV_AUTH_PARAM_DONT_STORE_PASSWORDS) != NULL)
    {
      store_passwords = FALSE;
    }

    if (auth_baton_get_parameter(callbacks->auth_baton,
      WEBDAV_AUTH_PARAM_NO_AUTH_CACHE) != NULL)
    {
      store_auth_creds = FALSE;
    }
  }
  if (callbacks->auth_baton)
  {
    // Save auth caching parameters in the auth parameter hash.
    if (!store_passwords)
      auth_baton_set_parameter(callbacks->auth_baton,
        WEBDAV_AUTH_PARAM_DONT_STORE_PASSWORDS, "");

    auth_baton_set_parameter(callbacks->auth_baton,
      WEBDAV_AUTH_PARAM_STORE_PLAINTEXT_PASSWORDS,
      store_plaintext_passwords);

    if (!store_pp)
      auth_baton_set_parameter(callbacks->auth_baton,
        WEBDAV_AUTH_PARAM_DONT_STORE_SSL_CLIENT_CERT_PP,
        "");

    auth_baton_set_parameter(callbacks->auth_baton,
      WEBDAV_AUTH_PARAM_STORE_SSL_CLIENT_CERT_PP_PLAINTEXT,
      store_pp_plaintext);

    if (!store_auth_creds)
      auth_baton_set_parameter(callbacks->auth_baton,
        WEBDAV_AUTH_PARAM_NO_AUTH_CACHE, "");
  }

  const vtable_t * vtable = NULL;
  WEBDAV_ERR(neon_init(&vtable, sesspool));

  // Create the session object.
  session_t * session = static_cast<session_t *>(apr_pcalloc(sesspool, sizeof(*session)));

  session->vtable = vtable;
  session->pool = sesspool;

  const char * corrected_url = NULL;
  // Ask the library to open the session.
  WEBDAV_ERR_W(vtable->open_session(
    session,
    &corrected_url,
    session_URL,
    callbacks, callback_baton, sesspool),
      apr_psprintf(pool, "Unable to connect to a WebDAV resource at URL '%s'",
        session_URL));

  if (corrected_url_p && corrected_url)
  {
    if (!path_is_url(corrected_url))
    {
      ne_uri * corrected_URI = NULL;
      WEBDAV_ERR(parse_ne_uri(&corrected_URI, session_URL, sesspool));
      if (corrected_URI->path) ne_free(corrected_URI->path);
      corrected_URI->path = ne_strdup(corrected_url);
      corrected_url = neon_uri_unparse(corrected_URI, pool);
    }
    *corrected_url_p = uri_canonicalize(corrected_url, pool);
    webdav_pool_destroy(sesspool);
    return WEBDAV_NO_ERROR;
  }
  *session_p = session;
  return WEBDAV_NO_ERROR;
}

//---------------------------------------------------------------------------

// This implements the client_list_func_t API
static error_t
list_func(
  void * baton,
  const char * path,
  const dirent_t * dirent,
  const char * abs_path,
  apr_pool_t * pool)
{
  list_func_baton_t * pb = static_cast<list_func_baton_t *>(baton);
  assert(pb);
  assert(pb->entries);
  const char * entryname = NULL;

  neon_session_t * ras = static_cast<neon_session_t *>(pb->session->priv);
  assert(ras);
  if (ras->callbacks->cancel_func)
    WEBDAV_ERR(ras->callbacks->cancel_func(ras->callback_baton));

  if (strcmp(path, "") == 0)
  {
    if (dirent->kind == node_file)
      entryname = dirent_basename(abs_path, pool);
    else if (pb->verbose)
      entryname = ".";
    else
      // Don't bother to list if no useful information will be shown.
      return WEBDAV_NO_ERROR;
  }
  else
    entryname = path;

  if (pb->verbose)
  {
    apr_time_t now = apr_time_now();
    apr_time_exp_t exp_time;
    apr_status_t apr_err;
    apr_size_t size;
    char timestr[20];
    const char * utf8_timestr;

    // time_to_human_cstring gives us something *way* too long
    // to use for this, so we have to roll our own.  We include
    // the year if the entry's time is not within half a year.
    apr_time_exp_lt(&exp_time, dirent->time);
    if (apr_time_sec(now - dirent->time) < (365 * 86400 / 2) &&
        apr_time_sec(dirent->time - now) < (365 * 86400 / 2))
    {
      apr_err = apr_strftime(timestr, &size, sizeof(timestr),
        "%b %d %H:%M", &exp_time);
    }
    else
    {
      apr_err = apr_strftime(timestr, &size, sizeof(timestr),
        "%b %d  %Y", &exp_time);
    }

    // if that failed, just zero out the string and print nothing
    if (apr_err)
      timestr[0] = '\0';

    // we need it in UTF-8.
    WEBDAV_ERR(utf_cstring_to_utf8(&utf8_timestr, timestr, pool));

    TListDataEntry entry = {NULL, NULL, NULL, 0, false, false, {0}, NULL};
    if (APR_SUCCESS != utf8_to_unicode(const_cast<wchar_t **>(&entry.Name), entryname, pb->pool))
    {
      return error_create(WEBDAV_ERR_DAV_MALFORMED_DATA, NULL, NULL);
    }
    entry.Permissions = L"";
    entry.OwnerGroup = L"";
    int dir = dirent->kind == node_dir;
    entry.Size = dir == 0 ? dirent->size : 0;
    entry.Dir = dir != 0;
    entry.Link = false;
    entry.Time.Year = exp_time.tm_year + 1900;
    entry.Time.Month = exp_time.tm_mon + 1;
    entry.Time.Day = exp_time.tm_mday;
    entry.Time.Hour = exp_time.tm_hour;
    entry.Time.Minute = exp_time.tm_min;
    entry.Time.Second = exp_time.tm_sec;
    entry.Time.HasTime = true;
    entry.Time.HasSeconds = true;
    entry.Time.HasDate = true;
    entry.LinkTarget = L"";
    pb->entries->push_back(entry);
  }
  return WEBDAV_NO_ERROR;
}

//---------------------------------------------------------------------------
// from url.c

static error_t
client_url_from_path2(
  const char ** url,
  const char * path_or_url,
  stringbuf_t * session_url,
  apr_pool_t * result_pool,
  apr_pool_t * scratch_pool)
{
  if (!path_is_url(path_or_url))
  {
    ne_uri * uri = NULL;
    WEBDAV_ERR(parse_ne_uri(&uri, session_url->data, result_pool));
    if (uri->path) ne_free(uri->path);
    uri->path = ne_strdup(path_or_url);
    const char * corrected_url = neon_uri_unparse(uri, result_pool);
    *url = uri_canonicalize(corrected_url, result_pool);
  }
  else
    *url = uri_canonicalize(path_or_url, result_pool);

  return WEBDAV_NO_ERROR;
}

//---------------------------------------------------------------------------
// from ctx.c

static error_t
client_create_context(
  client_ctx_t ** ctx,
  apr_pool_t * pool)
{
  *ctx = static_cast<client_ctx_t *>(apr_pcalloc(pool, sizeof(client_ctx_t)));
  return WEBDAV_NO_ERROR;
}

//------------------------------------------------------------------------------
// from auth.c

static void
auth_baton_create(
  auth_baton_t ** auth_baton,
  apr_pool_t * pool)
{
  auth_baton_t * ab = NULL;

  // Build the auth_baton.
  ab = static_cast<auth_baton_t *>(apr_pcalloc(pool, sizeof(*ab)));
  ab->tables = apr_hash_make(pool);
  ab->parameters = apr_hash_make(pool);
  ab->creds_cache = apr_hash_make(pool);
  ab->pool = pool;

  *auth_baton = ab;
}

static void
create_baton_open(
  auth_baton_t * auth_baton,
  const apr_array_header_t * providers,
  apr_pool_t * pool)
{
  auth_provider_object_t * provider = NULL;

  // Register each provider in order.  Providers of different
  // credentials will be automatically sorted into different tables by
  // register_provider().
  if (providers)
  {
    for (int i = 0; i < providers->nelts; i++)
    {
      provider_set_t * table = NULL;
      provider = APR_ARRAY_IDX(providers, i, auth_provider_object_t *);

      // Add it to the appropriate table in the auth_baton
      table = static_cast<provider_set_t *>(apr_hash_get(auth_baton->tables,
        provider->vtable->cred_kind, APR_HASH_KEY_STRING));
      if (!table)
      {
        table = static_cast<provider_set_t *>(apr_pcalloc(pool, sizeof(*table)));
        table->providers = apr_array_make(pool, 1, sizeof(auth_provider_object_t *));

        apr_hash_set(auth_baton->tables,
          provider->vtable->cred_kind, APR_HASH_KEY_STRING,
          table);
      }
      APR_ARRAY_PUSH(table->providers, auth_provider_object_t *) = provider;
    }
  }
}

//---------------------------------------------------------------------------
// from ssl_client_cert_providers.c

// A function returning an SSL client certificate passphrase provider.
typedef void (*auth_ssl_client_cert_pw_provider_func_t)(
  auth_provider_object_t ** provider,
  apr_pool_t * pool);

// retrieve and load the ssl client certificate file from servers config
static error_t
ssl_client_cert_file_first_credentials(
  void ** credentials_p,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  const char * cert_file;

  cert_file = NULL;

  if (cert_file != NULL)
  {
    auth_cred_ssl_client_cert_t * cred =
      static_cast<auth_cred_ssl_client_cert_t *>(apr_pcalloc(pool, sizeof(*cred)));

    cred->cert_file = cert_file;
    cred->may_save = FALSE;
    *credentials_p = cred;
  }
  else
  {
    *credentials_p = NULL;
  }

  *iter_baton = NULL;
  return WEBDAV_NO_ERROR;
}

static const auth_provider_t ssl_client_cert_file_provider =
{
  AUTH_CRED_SSL_CLIENT_CERT,
  ssl_client_cert_file_first_credentials,
  NULL,
  NULL
};

// Public API to SSL file providers.
static void
auth_get_ssl_client_cert_file_provider(
  auth_provider_object_t ** provider,
  apr_pool_t * pool)
{
  auth_provider_object_t * po =
    static_cast<auth_provider_object_t *>(apr_pcalloc(pool, sizeof(*po)));
  po->vtable = &ssl_client_cert_file_provider;
  *provider = po;
}

/*-----------------------------------------------------------------------*/
// Prompt provider
/*-----------------------------------------------------------------------*/

// Baton type for prompting to send client ssl creds.
// There is no iteration baton type.
typedef struct ssl_client_cert_prompt_provider_baton_t
{
  auth_ssl_client_cert_prompt_func_t prompt_func;
  void * prompt_baton;

  // how many times to re-prompt after the first one fails
  int retry_limit;
} ssl_client_cert_prompt_provider_baton_t;

// Iteration baton.
typedef struct ssl_client_cert_prompt_iter_baton_t
{
  // The original provider baton
  ssl_client_cert_prompt_provider_baton_t * pb;

  // The original realmstring
  const char * realmstring;

  // how many times we've reprompted
  int retries;
} ssl_client_cert_prompt_iter_baton_t;

static error_t
ssl_client_cert_prompt_first_cred(
  void ** credentials_p,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  ssl_client_cert_prompt_provider_baton_t * pb =
    static_cast<ssl_client_cert_prompt_provider_baton_t *>(provider_baton);
  ssl_client_cert_prompt_iter_baton_t * ib =
    static_cast<ssl_client_cert_prompt_iter_baton_t *>(apr_pcalloc(pool, sizeof(*ib)));
  const char * no_auth_cache = static_cast<const char *>(apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_NO_AUTH_CACHE,
    APR_HASH_KEY_STRING));

  WEBDAV_ERR(pb->prompt_func((auth_cred_ssl_client_cert_t **) credentials_p,
    pb->prompt_baton, realmstring, !no_auth_cache,
    pool));

  ib->pb = pb;
  ib->realmstring = apr_pstrdup(pool, realmstring);
  ib->retries = 0;
  *iter_baton = ib;

  return WEBDAV_NO_ERROR;
}

static error_t
ssl_client_cert_prompt_next_cred(
  void ** credentials_p,
  void * iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  ssl_client_cert_prompt_iter_baton_t * ib =
    static_cast<ssl_client_cert_prompt_iter_baton_t *>(iter_baton);
  const char * no_auth_cache = static_cast<const char *>(apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_NO_AUTH_CACHE,
    APR_HASH_KEY_STRING));

  if ((ib->pb->retry_limit >= 0) && (ib->retries >= ib->pb->retry_limit))
  {
    // give up, go on to next provider.
    *credentials_p = NULL;
    return WEBDAV_NO_ERROR;
  }
  ib->retries++;

  return ib->pb->prompt_func((auth_cred_ssl_client_cert_t **)
    credentials_p, ib->pb->prompt_baton,
    ib->realmstring, !no_auth_cache, pool);
}

static const auth_provider_t ssl_client_cert_prompt_provider =
{
  AUTH_CRED_SSL_CLIENT_CERT,
  ssl_client_cert_prompt_first_cred,
  ssl_client_cert_prompt_next_cred,
  NULL
};

// Public API to SSL prompting providers.
static void
auth_get_ssl_client_cert_prompt_provider(
  auth_provider_object_t ** provider,
  auth_ssl_client_cert_prompt_func_t prompt_func,
  void * prompt_baton,
  int retry_limit,
  apr_pool_t * pool)
{
  auth_provider_object_t * po =
    static_cast<auth_provider_object_t *>(apr_pcalloc(pool, sizeof(*po)));
  ssl_client_cert_prompt_provider_baton_t * pb =
    static_cast<ssl_client_cert_prompt_provider_baton_t *>(apr_pcalloc(pool, sizeof(*pb)));

  pb->prompt_func = prompt_func;
  pb->prompt_baton = prompt_baton;
  pb->retry_limit = retry_limit;

  po->vtable = &ssl_client_cert_prompt_provider;
  po->provider_baton = pb;
  *provider = po;
}

//---------------------------------------------------------------------------
// from ssl_server_trust_providers.c

// retrieve ssl server CA failure overrides (if any) from servers config
static error_t
ssl_server_trust_file_first_credentials(
  void ** credentials,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  apr_uint32_t * failures = static_cast<apr_uint32_t *>(apr_hash_get(parameters,
    AUTH_PARAM_SSL_SERVER_FAILURES,
    APR_HASH_KEY_STRING));
  const auth_ssl_server_cert_info_t * cert_info =
    static_cast<const auth_ssl_server_cert_info_t *>(apr_hash_get(parameters,
        AUTH_PARAM_SSL_SERVER_CERT_INFO,
        APR_HASH_KEY_STRING));
  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);
  apr_hash_t * creds_hash = NULL;
  error_t error = WEBDAV_NO_ERROR;

  *credentials = NULL;
  *iter_baton = NULL;

  // Check if this is a permanently accepted certificate
  error = config_read_auth_data(&creds_hash, AUTH_CRED_SSL_SERVER_TRUST,
            realmstring, fs, pool);
  error_clear(&error);
  if (!error && creds_hash)
  {
    string_t * trusted_cert, *this_cert, *failstr;
    apr_uint32_t last_failures = 0;

    trusted_cert = static_cast<string_t *>(apr_hash_get(creds_hash, AUTHN_ASCII_CERT_KEY,
      APR_HASH_KEY_STRING));
    this_cert = string_create(cert_info->fingerprint, pool);
    failstr = static_cast<string_t *>(apr_hash_get(creds_hash, AUTHN_FAILURES_KEY,
      APR_HASH_KEY_STRING));

    if (failstr)
    {
      char * endptr;
      unsigned long tmp_ulong = strtoul(failstr->data, &endptr, 10);

      if (*endptr == '\0')
        last_failures = (apr_uint32_t) tmp_ulong;
    }

    // If the cert is trusted and there are no new failures, we
    // accept it by clearing all failures.
    if (trusted_cert &&
        string_compare(this_cert, trusted_cert) &&
        (*failures & ~last_failures) == 0)
    {
      *failures = 0;
    }
  }

  // If all failures are cleared now, we return the creds
  if (!*failures)
  {
    auth_cred_ssl_server_trust_t * creds =
      static_cast<auth_cred_ssl_server_trust_t *>(apr_pcalloc(pool, sizeof(*creds)));
    creds->may_save = FALSE; // No need to save it again...
    *credentials = creds;
  }

  return WEBDAV_NO_ERROR;
}

static error_t
ssl_server_trust_file_save_credentials(
  bool * saved,
  void * credentials,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  auth_cred_ssl_server_trust_t * creds =
    static_cast<auth_cred_ssl_server_trust_t *>(credentials);
  const auth_ssl_server_cert_info_t * cert_info;
  apr_hash_t * creds_hash = NULL;

  if (!creds->may_save)
    return WEBDAV_NO_ERROR;

  cert_info = static_cast<const auth_ssl_server_cert_info_t *>(apr_hash_get(parameters,
              AUTH_PARAM_SSL_SERVER_CERT_INFO,
              APR_HASH_KEY_STRING));
  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);

  creds_hash = apr_hash_make(pool);
  apr_hash_set(creds_hash, AUTHN_ASCII_CERT_KEY, APR_HASH_KEY_STRING,
               string_create(cert_info->fingerprint, pool));
  apr_hash_set(creds_hash, AUTHN_FAILURES_KEY, APR_HASH_KEY_STRING,
               string_createf(pool, "%lu", (unsigned long)
                              creds->accepted_failures));

  WEBDAV_ERR(config_write_auth_data(creds_hash,
    AUTH_CRED_SSL_SERVER_TRUST,
    realmstring,
    fs,
    pool));
  *saved = TRUE;
  return WEBDAV_NO_ERROR;
}

static const auth_provider_t ssl_server_trust_file_provider =
{
  AUTH_CRED_SSL_SERVER_TRUST,
  &ssl_server_trust_file_first_credentials,
  NULL,
  &ssl_server_trust_file_save_credentials,
};

// Public API to SSL file providers.
static void
auth_get_ssl_server_trust_file_provider(
  auth_provider_object_t ** provider,
  apr_pool_t * pool)
{
  auth_provider_object_t * po =
    static_cast<auth_provider_object_t *>(apr_pcalloc(pool, sizeof(*po)));

  po->vtable = &ssl_server_trust_file_provider;
  *provider = po;
}

/*-----------------------------------------------------------------------*/
// Prompt provider
/*-----------------------------------------------------------------------*/

// Baton type for prompting to verify server ssl creds.
// There is no iteration baton type.
typedef struct ssl_server_trust_prompt_provider_baton_t
{
  auth_ssl_server_trust_prompt_func_t prompt_func;
  void * prompt_baton;
} ssl_server_trust_prompt_provider_baton_t;

static error_t
ssl_server_trust_prompt_first_cred(
  void ** credentials_p,
  void ** iter_baton,
  void * provider_baton,
  apr_hash_t * parameters,
  const char * realmstring,
  apr_pool_t * pool)
{
  ssl_server_trust_prompt_provider_baton_t * pb =
    static_cast<ssl_server_trust_prompt_provider_baton_t *>(provider_baton);
  apr_uint32_t * failures = static_cast<apr_uint32_t *>(apr_hash_get(parameters,
    AUTH_PARAM_SSL_SERVER_FAILURES,
    APR_HASH_KEY_STRING));
  const char * no_auth_cache = static_cast<const char *>(apr_hash_get(parameters,
    WEBDAV_AUTH_PARAM_NO_AUTH_CACHE,
    APR_HASH_KEY_STRING));
  const auth_ssl_server_cert_info_t * cert_info =
    static_cast<const auth_ssl_server_cert_info_t *>(apr_hash_get(parameters,
      AUTH_PARAM_SSL_SERVER_CERT_INFO,
      APR_HASH_KEY_STRING));

  WEBDAV_ERR(pb->prompt_func((auth_cred_ssl_server_trust_t **)
    credentials_p, pb->prompt_baton, realmstring,
    *failures, cert_info, !no_auth_cache &&
    !(*failures & WEBDAV_AUTH_SSL_OTHER), pool));

  *iter_baton = NULL;
  return WEBDAV_NO_ERROR;
}

static const auth_provider_t ssl_server_trust_prompt_provider =
{
  AUTH_CRED_SSL_SERVER_TRUST,
  ssl_server_trust_prompt_first_cred,
  NULL,
  NULL
};

// Public API to SSL prompting providers.
static void
auth_get_ssl_server_trust_prompt_provider(
  auth_provider_object_t ** provider,
  auth_ssl_server_trust_prompt_func_t prompt_func,
  void * prompt_baton,
  apr_pool_t * pool)
{
  auth_provider_object_t * po =
    static_cast<auth_provider_object_t *>(apr_pcalloc(pool, sizeof(*po)));
  ssl_server_trust_prompt_provider_baton_t * pb =
    static_cast<ssl_server_trust_prompt_provider_baton_t *>(apr_pcalloc(pool, sizeof(*pb)));
  pb->prompt_func = prompt_func;
  pb->prompt_baton = prompt_baton;
  po->vtable = &ssl_server_trust_prompt_provider;
  po->provider_baton = pb;
  *provider = po;
}

//---------------------------------------------------------------------------
// from cmdline.h

typedef struct cmdline_prompt_baton2_t
{
  cancel_func_t cancel_func;
  void * cancel_baton;
} cmdline_prompt_baton2_t;

//---------------------------------------------------------------------------
// from prompt.c

// This is a helper for plaintext prompt functions.
static error_t
plaintext_prompt_helper(
  bool * may_save_plaintext,
  const char * realmstring,
  const char * prompt_string,
  const char * prompt_text,
  void * baton,
  apr_pool_t * pool)
{
  cmdline_prompt_baton2_t * pb = static_cast<cmdline_prompt_baton2_t *>(baton);

  auth_baton_t * ab = static_cast<auth_baton_t *>(pb->cancel_baton);
  assert(ab);
  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(ab->parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);

  unsigned int RequestResult = 0;
  error_t err = fs->SimplePrompt(prompt_text, prompt_string, RequestResult);
  if (err)
  {
    if (err == WEBDAV_ERR_CANCELLED)
    {
      error_clear(&err);
      *may_save_plaintext = FALSE;
      return WEBDAV_NO_ERROR;
    }
    else
      return err;
  }
  if (RequestResult == qaYes)
  {
    *may_save_plaintext = TRUE;
  }
  else if (RequestResult == qaNo)
  {
    *may_save_plaintext = FALSE;
  }

  return WEBDAV_NO_ERROR;
}

// This implements 'auth_plaintext_prompt_func_t'.
static error_t
cmdline_auth_plaintext_prompt(
  bool * may_save_plaintext,
  const char * realmstring,
  void * baton,
  apr_pool_t * pool)
{
  const char * prompt_string = "Store password unencrypted (yes/no)? ";
  const char * prompt_text =
    "\n-----------------------------------------------------------------------"
    "\nATTENTION!  Your password for authentication realm:\n"
    "\n"
    "   %s\n"
    "\n"
    "can only be stored to disk unencrypted!  You are advised to configure\n"
    "your system so that system can store passwords encrypted, if\n"
    "possible.  See the documentation for details.\n"
    "\n"
    "You can avoid future appearances of this warning by setting the value\n"
    "of the 'store-plaintext-passwords' option to either 'yes' or 'no' in\n"
    "'%s'.\n"
    "-----------------------------------------------------------------------\n"
    ;
  return plaintext_prompt_helper(may_save_plaintext, realmstring,
    prompt_string, prompt_text, baton,
    pool);
}

// This implements 'auth_plaintext_passphrase_prompt_func_t'.
static error_t
cmdline_auth_plaintext_passphrase_prompt(
  bool * may_save_plaintext,
  const char * realmstring,
  void * baton,
  apr_pool_t * pool)
{
  const char * prompt_string = "Store passphrase unencrypted (yes/no)? ";
  const char * prompt_text =
    "\n-----------------------------------------------------------------------\n"
    "ATTENTION!  Your passphrase for client certificate:\n"
    "\n"
    "   %s\n"
    "\n"
    "can only be stored to disk unencrypted!  You are advised to configure\n"
    "your system so that system can store passphrase encrypted, if\n"
    "possible.  See the documentation for details.\n"
    "\n"
    "You can avoid future appearances of this warning by setting the value\n"
    "of the 'store-ssl-client-cert-pp-plaintext' option to either 'yes' or\n"
    "'no' in '%s'.\n"
    "-----------------------------------------------------------------------\n"
    ;
  return plaintext_prompt_helper(may_save_plaintext, realmstring,
    prompt_string, prompt_text, baton,
    pool);
}

// This implements 'auth_ssl_server_trust_prompt_func_t'.
static error_t
cmdline_auth_ssl_server_trust_prompt(
  auth_cred_ssl_server_trust_t ** cred_p,
  void * baton,
  const char * realm,
  apr_uint32_t failures,
  const auth_ssl_server_cert_info_t * cert_info,
  bool may_save,
  apr_pool_t * pool)
{
  cmdline_prompt_baton2_t * pb =
    static_cast<cmdline_prompt_baton2_t *>(baton);
  stringbuf_t * buf = stringbuf_create("", pool);

  /*if (failures & WEBDAV_AUTH_SSL_UNKNOWNCA)
  {
    stringbuf_appendcstr(buf,
     " - The certificate is not issued by a trusted authority. Use the\n"
     "   fingerprint to validate the certificate manually!\n");
  }*/

  if (failures & WEBDAV_AUTH_SSL_CNMISMATCH)
  {
    stringbuf_appendcstr(buf, "- The certificate hostname does not match.\n");
  }

  if (failures & WEBDAV_AUTH_SSL_NOTYETVALID)
  {
    stringbuf_appendcstr(buf, "- The certificate is not yet valid.\n");
  }

  if (failures & WEBDAV_AUTH_SSL_EXPIRED)
  {
    stringbuf_appendcstr(buf, "- The certificate has expired.\n");
  }

  if (failures & WEBDAV_AUTH_SSL_OTHER)
  {
    stringbuf_appendcstr(buf, "- The certificate has an unknown error.\n");
  }

  stringbuf_t * msg = stringbuf_createf(pool,
    // "Certificate information:\n"
    " - Hostname: %s\n"
    " - Valid: from %s until %s\n"
    " - Issuer: %s\n"
    " - Fingerprint: %s",
    cert_info->hostname,
    cert_info->valid_from,
    cert_info->valid_until,
    cert_info->issuer_dname,
    cert_info->fingerprint);
  stringbuf_appendstr(buf, msg);

  auth_baton_t * ab = static_cast<auth_baton_t *>(pb->cancel_baton);
  assert(ab);
  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(ab->parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);

  unsigned int RequestResult = 0;
  WEBDAV_ERR(fs->VerifyCertificate(buf->data, cert_info->fingerprint, RequestResult));

  if (RequestResult == qaYes)
  {
    *cred_p = static_cast<auth_cred_ssl_server_trust_t *>(apr_pcalloc(pool, sizeof(**cred_p)));
    (*cred_p)->may_save = TRUE;
    (*cred_p)->accepted_failures = failures;
  }
  else if (RequestResult == qaNo)
  {
    *cred_p = static_cast<auth_cred_ssl_server_trust_t *>(apr_pcalloc(pool, sizeof(**cred_p)));
    (*cred_p)->may_save = FALSE;
    (*cred_p)->accepted_failures = failures;
  }
  else
  {
    *cred_p = NULL;
  }

  return WEBDAV_NO_ERROR;
}

// This implements 'auth_ssl_client_cert_prompt_func_t'.
static error_t
cmdline_auth_ssl_client_cert_prompt(
  auth_cred_ssl_client_cert_t ** cred_p,
  void * baton,
  const char * realm,
  bool may_save,
  apr_pool_t * pool)
{
  auth_cred_ssl_client_cert_t * cred = NULL;
  const char * cert_file = NULL;
  const char * abs_cert_file = NULL;
  cmdline_prompt_baton2_t * pb =
    static_cast<cmdline_prompt_baton2_t *>(baton);

  auth_baton_t * ab = static_cast<auth_baton_t *>(pb->cancel_baton);
  assert(ab);
  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(ab->parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);

  unsigned int RequestResult = 0;
  WEBDAV_ERR(fs->AskForClientCertificateFilename(&cert_file, RequestResult, pool));
  if (RequestResult != qaOK) return WEBDAV_NO_ERROR;

  WEBDAV_ERR(dirent_get_absolute(&abs_cert_file, cert_file, pool));

  cred = static_cast<auth_cred_ssl_client_cert_t *>(apr_pcalloc(pool, sizeof(*cred)));
  cred->cert_file = abs_cert_file;
  cred->may_save = may_save;
  *cred_p = cred;

  return WEBDAV_NO_ERROR;
}

// This implements 'auth_ssl_client_cert_pw_prompt_func_t'.
static error_t
cmdline_auth_ssl_client_cert_pw_prompt(
  auth_cred_ssl_client_cert_pw_t ** cred_p,
  void * baton,
  const char * realm,
  bool may_save,
  apr_pool_t * pool)
{
  auth_cred_ssl_client_cert_pw_t * cred = NULL;
  cmdline_prompt_baton2_t * pb =
    static_cast<cmdline_prompt_baton2_t *>(baton);

  auth_baton_t * ab = static_cast<auth_baton_t *>(pb->cancel_baton);
  assert(ab);
  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(ab->parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);

  unsigned int RequestResult = 0;
  const char * result = NULL;
  WEBDAV_ERR(fs->AskForPassphrase(&result, realm, RequestResult, pool));
  if (RequestResult != qaOK) return WEBDAV_NO_ERROR;

  cred = static_cast<auth_cred_ssl_client_cert_pw_t *>(apr_pcalloc(pool, sizeof(*cred)));
  cred->password = result;
  cred->may_save = may_save;
  *cred_p = cred;

  return WEBDAV_NO_ERROR;
}

// This implements 'auth_simple_prompt_func_t'.
static error_t
cmdline_auth_simple_prompt(
  auth_cred_simple_t ** cred_p,
  void * baton,
  const char * realm,
  const char * username,
  bool may_save,
  apr_pool_t * pool)
{
  auth_cred_simple_t * ret =
    static_cast<auth_cred_simple_t *>(apr_pcalloc(pool, sizeof(*ret)));
  cmdline_prompt_baton2_t * pb =
    static_cast<cmdline_prompt_baton2_t *>(baton);

  auth_baton_t * ab = static_cast<auth_baton_t *>(pb->cancel_baton);
  assert(ab);
  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(ab->parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);
  unsigned int RequestResult = 0;

  if (username)
    ret->username = apr_pstrdup(pool, username);
  else
  {
    WEBDAV_ERR(fs->AskForUsername(&ret->username, RequestResult, pool));
    if (RequestResult != qaOK) return WEBDAV_NO_ERROR;
  }

  WEBDAV_ERR(fs->AskForUserPassword(&ret->password, RequestResult, pool));
  if (RequestResult != qaOK) return WEBDAV_NO_ERROR;

  ret->may_save = may_save;
  *cred_p = ret;
  return WEBDAV_NO_ERROR;
}

// This implements 'auth_username_prompt_func_t'.
static error_t
cmdline_auth_username_prompt(
  auth_cred_username_t ** cred_p,
  void * baton,
  const char * realm,
  bool may_save,
  apr_pool_t * pool)
{
  auth_cred_username_t * ret =
    static_cast<auth_cred_username_t *>(apr_pcalloc(pool, sizeof(*ret)));
  cmdline_prompt_baton2_t * pb =
    static_cast<cmdline_prompt_baton2_t *>(baton);

  auth_baton_t * ab = static_cast<auth_baton_t *>(pb->cancel_baton);
  assert(ab);
  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(ab->parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);

  unsigned int RequestResult = 0;
  WEBDAV_ERR(fs->AskForUsername(&ret->username, RequestResult, pool));
  if (RequestResult != qaOK) return WEBDAV_NO_ERROR;

  ret->may_save = may_save;
  *cred_p = ret;
  return WEBDAV_NO_ERROR;
}

//---------------------------------------------------------------------------
// from cmdline.c

static error_t
ssl_trust_unknown_server_cert(
  auth_cred_ssl_server_trust_t ** cred_p,
  void * baton,
  const char * realm,
  apr_uint32_t failures,
  const auth_ssl_server_cert_info_t * cert_info,
  bool may_save,
  apr_pool_t * pool)
{
  *cred_p = NULL;

  if (failures == 0 || failures == WEBDAV_AUTH_SSL_UNKNOWNCA)
  {
    *cred_p = static_cast<auth_cred_ssl_server_trust_t *>(apr_pcalloc(pool, sizeof(**cred_p)));
    (*cred_p)->may_save = FALSE;
    (*cred_p)->accepted_failures = failures;
  }

  return WEBDAV_NO_ERROR;
}

static error_t
auth_baton_init(
  auth_baton_t * ab,
  bool non_interactive,
  const char * auth_username,
  const char * auth_password,
  bool no_auth_cache,
  bool trust_server_cert,
  TWebDAVFileSystem * fs,
  cancel_func_t cancel_func,
  void * cancel_baton,
  apr_pool_t * pool)
{
  bool store_password_val = TRUE;
  bool store_auth_creds_val = TRUE;
  auth_provider_object_t * provider = NULL;
  cmdline_prompt_baton2_t * pb = NULL;

  // The whole list of registered providers
  apr_array_header_t * providers = NULL;

  // Populate the registered providers with the platform-specific providers
  WEBDAV_ERR(auth_get_platform_specific_client_providers(&providers,
    pool));

  // If we have a cancellation function, cram it and the stuff it
  // needs into the prompt baton.
  if (cancel_func)
  {
    pb = static_cast<cmdline_prompt_baton2_t *>(apr_pcalloc(pool, sizeof(*pb)));
    pb->cancel_func = cancel_func;
    pb->cancel_baton = cancel_baton;
  }

  if (non_interactive == FALSE)
  {
    // This provider doesn't prompt the user in order to get creds;
    // it prompts the user regarding the caching of creds.
    auth_get_simple_provider2(&provider,
      cmdline_auth_plaintext_prompt,
      pb, pool);
  }
  else
  {
    auth_get_simple_provider2(&provider, NULL, NULL, pool);
  }
  APR_ARRAY_PUSH(providers, auth_provider_object_t *) = provider;

  auth_get_username_provider(&provider, pool);
  APR_ARRAY_PUSH(providers, auth_provider_object_t *) = provider;

  // The server-cert, client-cert, and client-cert-password providers.
  WEBDAV_ERR(auth_get_platform_specific_provider(&provider,
    "windows",
    "ssl_server_trust",
    pool));
  APR_ARRAY_PUSH(providers, auth_provider_object_t *) = provider;

  auth_get_ssl_server_trust_file_provider(&provider, pool);
  APR_ARRAY_PUSH(providers, auth_provider_object_t *) = provider;

  auth_get_ssl_client_cert_file_provider(&provider, pool);
  APR_ARRAY_PUSH(providers, auth_provider_object_t *) = provider;

  if (non_interactive == FALSE)
  {
    // This provider doesn't prompt the user in order to get creds;
    // it prompts the user regarding the caching of creds.
    auth_get_ssl_client_cert_pw_file_provider2(&provider, cmdline_auth_plaintext_passphrase_prompt,
     pb, pool);
  }
  else
  {
    auth_get_ssl_client_cert_pw_file_provider2(&provider, NULL, NULL,
        pool);
  }
  APR_ARRAY_PUSH(providers, auth_provider_object_t *) = provider;

  if (non_interactive == FALSE)
  {
    // Two basic prompt providers: username/password, and just username.
    auth_get_simple_prompt_provider(&provider,
      cmdline_auth_simple_prompt,
      pb,
      2, // retry limit
      pool);
    APR_ARRAY_PUSH(providers, auth_provider_object_t *) = provider;

    auth_get_username_prompt_provider(&provider, cmdline_auth_username_prompt, pb,
       2, /* retry limit */ pool);
    APR_ARRAY_PUSH(providers, auth_provider_object_t *) = provider;

    // Three ssl prompt providers, for server-certs, client-certs,
    // and client-cert-passphrases.
    auth_get_ssl_server_trust_prompt_provider(&provider, cmdline_auth_ssl_server_trust_prompt, pb, pool);
    APR_ARRAY_PUSH(providers, auth_provider_object_t *) = provider;

    auth_get_ssl_client_cert_prompt_provider(&provider, cmdline_auth_ssl_client_cert_prompt, pb, 2, pool);
    APR_ARRAY_PUSH(providers, auth_provider_object_t *) = provider;

    auth_get_ssl_client_cert_pw_prompt_provider(&provider, cmdline_auth_ssl_client_cert_pw_prompt, pb, 2, pool);
    APR_ARRAY_PUSH(providers, auth_provider_object_t *) = provider;
  }
  else if (trust_server_cert)
  {
    // Remember, only register this provider if non_interactive.
    auth_get_ssl_server_trust_prompt_provider(&provider, ssl_trust_unknown_server_cert, NULL, pool);
    APR_ARRAY_PUSH(providers, auth_provider_object_t *) = provider;
  }

  // Build an authentication baton to give to libclient.
  create_baton_open(ab, providers, pool);
  auth_baton_set_parameter(ab, CONST_FS_KEY, fs);

  // Place any default username or password credentials into the
  // auth_baton's run-time parameter hash.
  if (auth_username)
    auth_baton_set_parameter(ab, WEBDAV_AUTH_PARAM_DEFAULT_USERNAME,
      auth_username);
  if (auth_password)
    auth_baton_set_parameter(ab, WEBDAV_AUTH_PARAM_DEFAULT_PASSWORD,
      auth_password);

  // Same with the non-interactive option.
  if (non_interactive)
    auth_baton_set_parameter(ab, WEBDAV_AUTH_PARAM_NON_INTERACTIVE, "");

  if (!store_password_val)
    auth_baton_set_parameter(ab, WEBDAV_AUTH_PARAM_DONT_STORE_PASSWORDS, "");

  if (no_auth_cache || !store_auth_creds_val)
    auth_baton_set_parameter(ab, WEBDAV_AUTH_PARAM_NO_AUTH_CACHE, "");

  return WEBDAV_NO_ERROR;
}

//---------------------------------------------------------------------------
// from main.c

// A flag to see if we've been canceled by the client or not.
static volatile atomic_t cancelled = FALSE;

// Our cancellation callback.
static error_t
check_cancel(void * baton)
{
  if (cancelled)
    return error_create(WEBDAV_ERR_CANCELLED, NULL, "Cancelled");
  else
    return WEBDAV_NO_ERROR;
}

//---------------------------------------------------------------------------
// from ra.c

static error_t
cancel_callback(void * baton)
{
  callback_baton_t * cb = static_cast<callback_baton_t *>(baton);
  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(cb->ctx->auth_baton->parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);
  cancelled = static_cast<atomic_t>(fs->GetIsCancelled());
  return error_trace((cb->ctx->cancel_func)(cb->ctx->cancel_baton));
}

static error_t
get_client_string(
  void * baton,
  const char ** name,
  apr_pool_t * pool)
{
  callback_baton_t * b = static_cast<callback_baton_t *>(baton);
  *name = apr_pstrdup(pool, b->ctx->client_name);
  return WEBDAV_NO_ERROR;
}

// see ra.c::client_session_from_path
static error_t
init_session_from_path(
  session_t * session,
  const char ** url_p,
  const char * path_or_url,
  apr_pool_t * pool)
{
  const char * initial_url, *url;

  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  assert(ras);

  WEBDAV_ERR(client_url_from_path2(&initial_url, path_or_url,
    ras->url, pool, pool));
  if (!initial_url)
    return error_createf(WEBDAV_ERR_ENTRY_MISSING_URL, NULL,
      "'%s' has no URL", path_or_url);

  url = initial_url;
  // Make the session point to the real URL.
  WEBDAV_ERR(reparent(session, url, pool));

  *url_p = url;

  return WEBDAV_NO_ERROR;
}

static error_t
client_open_session_internal(
  session_t ** ra_session,
  const char ** corrected_url,
  const char * base_url,
  client_ctx_t * ctx,
  apr_pool_t * pool)
{
  // prepare callbacks, contexts
  callbacks2_t * cbtable = static_cast<callbacks2_t *>(apr_pcalloc(pool, sizeof(*cbtable)));
  callback_baton_t * cb = static_cast<callback_baton_t *>(apr_pcalloc(pool, sizeof(*cb)));

  cbtable->auth_baton = ctx->auth_baton;
  cbtable->progress_func = ctx->progress_func;
  cbtable->progress_baton = ctx->progress_baton;
  cbtable->cancel_func = ctx->cancel_func ? cancel_callback : NULL;
  cbtable->get_client_string = get_client_string;

  cb->pool = pool;
  cb->ctx = ctx;

  if (corrected_url)
  {
    apr_hash_t * attempted = apr_hash_make(pool);
    int attempts_left = MAX_REDIRECT_ATTEMPTS;
    while (attempts_left--)
    {
      const char * corrected = NULL;
      WEBDAV_ERR(session_open(
                   ra_session,
                   attempts_left == 0 ? NULL : &corrected,
                   base_url, cbtable, cb,
                   pool));
      // No error and no corrected URL?  We're done here.
      if (!corrected)
        break;
      // Our caller will want to know what our final corrected URL was.
      *corrected_url = corrected;
      // Make sure we've not attempted this URL before.
      if (apr_hash_get(attempted, corrected, APR_HASH_KEY_STRING))
        return WEBDAV_ERR_CLIENT_CYCLE_DETECTED;
      // Remember this CORRECTED_URL so we don't wind up in a loop.
      apr_hash_set(attempted, apr_pstrdup(pool, corrected), APR_HASH_KEY_STRING, (void *)1);
      base_url = corrected;
    }
  }
  else
  {
    WEBDAV_ERR(session_open(ra_session, NULL, base_url,
      cbtable, cb,
      pool));
  }
  return WEBDAV_NO_ERROR;
}

//---------------------------------------------------------------------------
// from list.c

static error_t
get_dir_contents(
  apr_uint32_t dirent_fields,
  const char * dir,
  session_t * ra_session,
  const char * fs_path,
  depth_t depth,
  client_list_func_t list_func,
  void * baton,
  apr_pool_t * pool)
{
  apr_hash_t * tmpdirents = NULL;
  apr_pool_t * iterpool = webdav_pool_create(pool);
  apr_array_header_t * array = NULL;
  error_t err = 0;

  if (depth == depth_empty)
    return WEBDAV_NO_ERROR;

  // Get the directory's entries, but not its props.  Ignore any
  // not-authorized errors.
  err = get_dir2(ra_session, &tmpdirents,
                 dir,
                 dirent_fields, pool);
  if (err && ((err == WEBDAV_ERR_NOT_AUTHORIZED) ||
             (err == WEBDAV_ERR_DAV_FORBIDDEN)))
  {
    error_clear(&err);
    return WEBDAV_NO_ERROR;
  }
  WEBDAV_ERR(err);

  neon_session_t * ras = static_cast<neon_session_t *>(ra_session->priv);
  assert(ras);
  if (ras->callbacks->cancel_func)
    WEBDAV_ERR(ras->callbacks->cancel_func(ras->callback_baton));

  // Sort the hash, so we can call the callback in a "deterministic" order.
  array = sort_hash(tmpdirents, sort_compare_items_lexically, pool);
  for (int i = 0; i < array->nelts; ++i)
  {
    sort_item_t * item = &APR_ARRAY_IDX(array, i, sort_item_t);
    dirent_t * the_ent = static_cast<dirent_t *>(apr_hash_get(tmpdirents, item->key, item->klen));

    webdav_pool_clear(iterpool);

    const char * path = relpath_join(dir, static_cast<const char *>(item->key), iterpool);

    if ((the_ent->kind == node_file) ||
        (depth == depth_immediates) ||
        (depth == depth_infinity))
      WEBDAV_ERR(list_func(baton, path,
        the_ent,
        fs_path, iterpool));

    if (depth == depth_infinity && the_ent->kind == node_dir)
      WEBDAV_ERR(get_dir_contents(dirent_fields, path,
        ra_session,
        fs_path, depth,
        list_func, baton, iterpool));
  }

  webdav_pool_destroy(iterpool);
  return WEBDAV_NO_ERROR;
}

//------------------------------------------------------------------------------
// from options.c

static const neon_xml_elm_t options_elements[] =
{
  { "DAV:", "href", ELEM_href, NEON_XML_CDATA },
  { "DAV:", "options-response", ELEM_options_response, 0 },

  { NULL }
};

typedef struct options_ctx_t
{
  // WARNING: WANT_CDATA should stay the first element in the baton:
  // neon_xml_collect_cdata() assumes the baton starts with a stringbuf.

  stringbuf_t * want_cdata;
  stringbuf_t * cdata;
  apr_pool_t * pool;
} options_ctx_t;

static int
options_validate_element(
  neon_xml_elmid parent,
  neon_xml_elmid child)
{
  switch (parent)
  {
    case ELEM_root:
      if (child == ELEM_options_response)
        return child;
      else
        return NEON_XML_INVALID;

    case ELEM_options_response:
      return NEON_XML_DECLINE; // not concerned with other response

    default:
      return NEON_XML_DECLINE;
  }

  // NOTREACHED
}

static error_t
options_start_element(
  int * elem,
  void * baton,
  int parent,
  const char * nspace,
  const char * name,
  const char ** atts)
{
  options_ctx_t * oc = static_cast<options_ctx_t *>(baton);
  const neon_xml_elm_t * elm = neon_lookup_xml_elem(options_elements, nspace, name);

  *elem = elm ? options_validate_element(parent, elm->id) : NEON_XML_DECLINE;
  if (*elem < 1)  // Not a valid element
    return WEBDAV_NO_ERROR;

  if (elm->id == ELEM_href)
    oc->want_cdata = oc->cdata;
  else
    oc->want_cdata = NULL;

  return WEBDAV_NO_ERROR;
}

static error_t
options_end_element(
  void * baton,
  int state,
  const char * nspace,
  const char * name)
{
  return WEBDAV_NO_ERROR;
}

static error_t
neon_exchange_capabilities(
  neon_session_t * ras,
  const char ** relocation_location,
  apr_pool_t * pool)
{
  neon_request_t * req = NULL;
  error_t err = WEBDAV_NO_ERROR;
  ne_xml_parser * parser = NULL;
  options_ctx_t oc = { 0 };
  int status_code = 0;

  oc.pool = pool;
  oc.cdata = stringbuf_create("", pool);

  if (relocation_location)
    *relocation_location = NULL;

  WEBDAV_ERR(neon_request_create(&req, ras,
    "OPTIONS", ras->url->data, pool));

  // Use a symbolic name somewhere for this MIME type?
  ne_add_request_header(req->ne_req, "Content-Type", "text/xml");
  apr_hash_t * extra_headers = apr_hash_make(pool);
  neon_add_depth_header(extra_headers, NEON_DEPTH_ZERO);

  // Create a parser to read the normal response body
  parser = neon_xml_parser_create(req, ne_accept_2xx, options_start_element,
    neon_xml_collect_cdata,
    options_end_element, &oc);

  // Run the request and get the resulting status code.
  if ((err = neon_request_dispatch(&status_code, req, extra_headers,
      "<?xml version=\"1.0\" "
      "encoding=\"utf-8\"?>"
      "<D:options xmlns:D=\"DAV:\">"
      "<D:resourcetype/>"
      "</D:options>",
      200,
      relocation_location ? 301 : 0,
      false,
      pool)) != WEBDAV_NO_ERROR)
    goto cleanup;
  if (req->code == 301)
  {
    *relocation_location = neon_request_get_location(req, pool);
    goto cleanup;
  }

  // Was there an XML parse error somewhere?
  err = neon_check_parse_error("OPTIONS", parser, ras->url->data);
  if (err)
    goto cleanup;

cleanup:
  neon_request_destroy(req);

  return err;
}

//------------------------------------------------------------------------------
// from props.c

typedef struct propfind_ctx_t
{
  // WARNING: WANT_CDATA should stay the first element in the baton:
  // neon_xml_collect_cdata() assumes the baton starts with a stringbuf.
  stringbuf_t * cdata;
  apr_hash_t * props;           // const char *URL-PATH -> neon_resource_t

  neon_resource_t * rsrc;       // the current resource.
  const char * encoding;        // property encoding (or NULL)
  int status;                   // status for the current <propstat> (or 0 if unknown).
  apr_hash_t * propbuffer;      // holds properties until their status is known.
  neon_xml_elmid last_open_id;  // the id of the last opened tag.
  ne_xml_parser * parser;       // xml parser handling the PROPSET request.

  apr_pool_t * pool;

} propfind_ctx_t;

// When we begin a checkout, we fetch these from the "public" resources.
// We fetch the resourcetype to
// verify that we're accessing a collection.
static const ne_propname starting_props[] =
{
  { "DAV:", "resourcetype" },
  { "DAV:", "creationdate" },
  { "DAV:", "getlastmodified" },
  { "DAV:", "getcontentlength" },
  { NULL }
};

static error_t
neon_get_starting_props(
  neon_resource_t ** rsrc,
  neon_session_t * sess,
  const char * url,
  bool check_errors,
  apr_pool_t * pool)
{
  WEBDAV_ERR(neon_get_props_resource(rsrc, sess, url,
    starting_props, check_errors, pool));

  // Cache some of the resource information.

  if (!sess->webdav_root)
  {
    string_t * propval = NULL;

    if (propval)
    {
      ne_uri uri = {0};
      stringbuf_t * urlbuf = stringbuf_create(url, pool);

      path_remove_components(urlbuf,
        path_component_count(propval->data));

      uri = sess->root;
      uri.path = urlbuf->data;

      sess->webdav_root = neon_uri_unparse(&uri, sess->pool);
    }
  }

  return WEBDAV_NO_ERROR;
}

// Propfind Implementation

typedef struct elem_defn
{
  neon_xml_elmid id;
  const char * name;
  int is_property;      // is it a property, or part of some structure?
} elem_defn;

static const elem_defn elem_definitions[] =
{
  // NOTE: Make sure that every item in here is also represented in
  // propfind_elements[]

  // DAV elements
  { ELEM_multistatus, "DAV:multistatus", 0 },
  { ELEM_response, "DAV:response", 0 },
  { ELEM_href, "DAV:href", NEON_XML_CDATA },
  { ELEM_propstat, "DAV:propstat", 0 },
  { ELEM_prop, "DAV:prop", 0 },
  { ELEM_status, "DAV:status", NEON_XML_CDATA },
  { ELEM_collection, "DAV:collection", NEON_XML_CDATA },
  { ELEM_resourcetype, "DAV:resourcetype", 0 },
  { ELEM_get_content_length, NEON_PROP_GETCONTENTLENGTH, 1 },
  { ELEM_creationdate, NEON_PROP_CREATIONDATE, 1 },

  { 0 }
};

static const neon_xml_elm_t propfind_elements[] =
{
  // NOTE: Make sure that every item in here is also represented in
  // elem_definitions[]

  // DAV elements
  { "DAV:", "multistatus", ELEM_multistatus, 0 },
  { "DAV:", "response", ELEM_response, 0 },
  { "DAV:", "href", ELEM_href, NEON_XML_CDATA },
  { "DAV:", "propstat", ELEM_propstat, 0 },
  { "DAV:", "prop", ELEM_prop, 0 },
  { "DAV:", "status", ELEM_status, NEON_XML_CDATA },
  { "DAV:", "collection", ELEM_collection, NEON_XML_CDATA },
  { "DAV:", "resourcetype", ELEM_resourcetype, 0 },
  { "DAV:", "getcontentlength", ELEM_get_content_length, NEON_XML_CDATA },
  { "DAV:", "getlastmodified", ELEM_get_last_modified, NEON_XML_CDATA },
  {
    "DAV:", "creator-displayname", ELEM_creator_displayname,
    NEON_XML_CDATA
  },

  // Unknowns
  { "", "", ELEM_unknown, NEON_XML_COLLECT },

  { NULL }
};

// Look up an element definition ID.  May return NULL if the elem is
// not recognized.
static const elem_defn *
defn_from_id(
  neon_xml_elmid id)
{
  for (const elem_defn * defn = elem_definitions; defn->name != NULL; ++defn)
  {
    if (id == defn->id)
      return defn;
  }

  return NULL;
}

// Assign URL to RSRC.  Use POOL for any allocations.
static error_t
assign_rsrc_url(
  neon_resource_t * rsrc,
  const char * url,
  apr_pool_t * pool)
{
  char * url_path = NULL;
  apr_size_t len = 0;
  ne_uri parsed_url = {0};

  // Parse the PATH element out of the URL.
  // NOTE: mod_dav does not (currently) use an absolute URL, but simply a
  // server-relative path (i.e. this uri_parse is effectively a no-op).

  if (ne_uri_parse(url, &parsed_url) != 0)
  {
    ne_uri_free(&parsed_url);
    return error_createf(WEBDAV_ERR_DAV_MALFORMED_DATA, NULL,
      "Unable to parse URL '%s'", url);
  }

  url_path = apr_pstrdup(pool, parsed_url.path);
  ne_uri_free(&parsed_url);

  // Clean up trailing slashes from the URL.
  len = strlen(url_path);
  if ((len > 1) && (url_path[len - 1] == '/'))
    url_path[len - 1] = '\0';
  rsrc->url = url_path;

  return WEBDAV_NO_ERROR;
}

// Determine whether we're receiving the expected XML response.
// Return CHILD when interested in receiving the child's contents
// or one of NEON_XML_INVALID and NEON_XML_DECLINE
// when respectively this is the incorrect response or
// the element (and its children) are uninteresting
static int
props_validate_element(
  neon_xml_elmid parent,
  neon_xml_elmid child)
{
  switch (parent)
  {
    case ELEM_root:
      if (child == ELEM_multistatus)
        return child;
      else
        return NEON_XML_INVALID;

    case ELEM_multistatus:
      if (child == ELEM_response)
        return child;
      else
        return NEON_XML_DECLINE;

    case ELEM_response:
      if ((child == ELEM_href) || (child == ELEM_propstat))
        return child;
      else
        return NEON_XML_DECLINE;

    case ELEM_propstat:
      if ((child == ELEM_prop) || (child == ELEM_status))
        return child;
      else
        return NEON_XML_DECLINE;

    case ELEM_prop:
      return child; // handle all children of <prop>

    case ELEM_resourcetype:
      if (child == ELEM_collection)
        return child;
      else
        return NEON_XML_DECLINE; // not concerned with other types (now)

    default:
      return NEON_XML_DECLINE;
  }

  // NOTREACHED
}

static error_t
props_start_element(
  int * elem,
  void * baton,
  int parent,
  const char * nspace,
  const char * name,
  const char ** atts)
{
  propfind_ctx_t * pc = static_cast<propfind_ctx_t *>(baton);
  const neon_xml_elm_t * elm = neon_lookup_xml_elem(propfind_elements, nspace, name);

  *elem = elm ? props_validate_element(parent, elm->id) : NEON_XML_DECLINE;
  if (*elem < 1)  // not a valid element
    return WEBDAV_NO_ERROR;

  stringbuf_setempty(pc->cdata);
  *elem = elm ? elm->id : ELEM_unknown;
  switch (*elem)
  {
    case ELEM_response:
      if (pc->rsrc)
        return error_create(WEBDAV_ERR_XML_MALFORMED, NULL, NULL);
      // Create a new resource.
      pc->rsrc = static_cast<neon_resource_t *>(apr_pcalloc(pc->pool, sizeof(*(pc->rsrc))));
      pc->rsrc->pool = pc->pool;
      pc->rsrc->propset = apr_hash_make(pc->pool);
      pc->status = 0;
      break;

    case ELEM_propstat:
      pc->status = 0;
      break;

    case ELEM_href:
      pc->rsrc->href_parent = pc->last_open_id;
      break;

    case ELEM_collection:
      pc->rsrc->is_collection = 1;
      break;

    case ELEM_unknown:
      // these are our user-visible properties, presumably.
      if (pc->encoding)
        pc->encoding = apr_pstrdup(pc->pool, pc->encoding);
      break;

    default:
      // nothing to do for these
      break;
  }

  // Remember the last tag we opened.
  pc->last_open_id = *elem;
  return WEBDAV_NO_ERROR;
}

static error_t
props_end_element(
  void * baton,
  int state,
  const char * nspace,
  const char * name)
{
  propfind_ctx_t * pc = static_cast<propfind_ctx_t *>(baton);
  neon_resource_t * rsrc = pc->rsrc;
  const string_t * value = NULL;
  const elem_defn * parent_defn = NULL;
  const elem_defn * defn = NULL;
  ne_status status = {0};
  const char * cdata = pc->cdata->data;

  switch (state)
  {
    case ELEM_response:
      // Verify that we've received a URL for this resource.
      if (!pc->rsrc->url)
        return error_create(WEBDAV_ERR_XML_MALFORMED, NULL, NULL);

      // Store the resource in the top-level hash table.
      apr_hash_set(pc->props, pc->rsrc->url, APR_HASH_KEY_STRING, pc->rsrc);
      pc->rsrc = NULL;
      return WEBDAV_NO_ERROR;

    case ELEM_propstat:
      // We're at the end of a set of properties.  Do the right thing status-wise.
      if (pc->status)
      {
        for (apr_hash_index_t * hi = apr_hash_first(pc->pool, pc->propbuffer); hi;
             hi = apr_hash_next(hi))
        {
          const void * key;
          apr_ssize_t klen;
          void * val;
          apr_hash_this(hi, &key, &klen, &val);
          if (pc->status == 200)
            apr_hash_set(rsrc->propset, key, klen, val);
          apr_hash_set(pc->propbuffer, key, klen, NULL);
        }
      }
      else if (!pc->status)
      {
        // No status at all? Bogosity.
        return error_create(WEBDAV_ERR_XML_MALFORMED, NULL, NULL);
      }
      return WEBDAV_NO_ERROR;

    case ELEM_status:
      // Parse the <status> tag's CDATA for a status code.
      if (ne_parse_statusline(cdata, &status))
        return error_create(WEBDAV_ERR_XML_MALFORMED, NULL, NULL);
      ne_free(status.reason_phrase);
      pc->status = status.code;
      return WEBDAV_NO_ERROR;

    case ELEM_href:
      // Special handling for <href> that belongs to the <response> tag.
      if (rsrc->href_parent == ELEM_response)
        return assign_rsrc_url(pc->rsrc,
          urlpath_canonicalize(cdata, pc->pool),
          pc->pool);

      // Use the parent element's name, not the href.
      parent_defn = defn_from_id(rsrc->href_parent);

      // No known parent?  Get outta here.
      if (!parent_defn)
        return WEBDAV_NO_ERROR;

      // All other href's we'll treat as property values.
      name = parent_defn->name;
      value = string_create(urlpath_canonicalize(cdata, pc->pool),
        pc->pool);
      break;

    default:
      if (state == ELEM_unknown)
      {
        name = apr_pstrcat(pc->pool, nspace, name, (char *)NULL);
      }
      else
      {
        defn = defn_from_id(state);
        if (!(defn && defn->is_property))
          return WEBDAV_NO_ERROR;
        name = defn->name;
      }

      // Check for encoding attribute.
      if (pc->encoding == NULL)
      {
        // Handle the property value by converting it to string.
        value = string_create(cdata, pc->pool);
        break;
      }

      // Check for known encoding type
      if (strcmp(pc->encoding, "base64") != 0)
        return error_create(WEBDAV_ERR_XML_MALFORMED, NULL, NULL);

      pc->encoding = NULL; // Reset encoding for future attribute(s).
  }

  // Handling resource properties from here out.

  // Add properties to the temporary propbuffer.  At the end of the
  // <propstat>, we'll either dump the props as invalid or move them
  // into the resource's property hash.
  apr_hash_set(pc->propbuffer, name, APR_HASH_KEY_STRING, value);
  return WEBDAV_NO_ERROR;
}

static void
props_set_parser(
  ne_xml_parser * parser,
  void * baton)
{
  propfind_ctx_t * pc = static_cast<propfind_ctx_t *>(baton);
  pc->parser = parser;
}

static error_t
neon_get_props(
  apr_hash_t ** results,
  neon_session_t * sess,
  const char * url,
  int depth,
  const ne_propname * which_props,
  bool check_errors,
  apr_pool_t * pool)
{
  apr_hash_t * extra_headers = apr_hash_make(pool);

  neon_add_depth_header(extra_headers, depth);

  // It's easier to roll our own PROPFIND here than use neon's current interfaces.
  // The start of the request body is fixed:
  stringbuf_t * body = stringbuf_create("<?xml version=\"1.0\" encoding=\"utf-8\"?>" DEBUG_CR
    "<propfind xmlns=\"DAV:\">" DEBUG_CR, pool);

  // Are we asking for specific propert(y/ies), or just all of them?
  if (which_props)
  {
    apr_pool_t * iterpool = webdav_pool_create(pool);

    stringbuf_appendcstr(body, "<prop>" DEBUG_CR);
    for (int n = 0; which_props[n].name != NULL; n++)
    {
      webdav_pool_clear(iterpool);
      stringbuf_appendcstr(body, apr_pstrcat(iterpool, "<", which_props[n].name,
        " xmlns=\"", which_props[n].nspace, "\"/>" DEBUG_CR,
        (char *)NULL));
    }
    stringbuf_appendcstr(body, "</prop></propfind>" DEBUG_CR);
    webdav_pool_destroy(iterpool);
  }
  else
  {
    stringbuf_appendcstr(body, "<allprop/></propfind>" DEBUG_CR);
  }

  // Initialize our baton.
  propfind_ctx_t pc;
  memset(&pc, 0, sizeof(pc));
  pc.pool = pool;
  pc.propbuffer = apr_hash_make(pool);
  pc.props = apr_hash_make(pool);
  pc.cdata = stringbuf_create("", pool);

  // Create and dispatch the request!
  WEBDAV_ERR(neon_parsed_request(sess, "PROPFIND", url,
    body->data, 0,
    props_set_parser,
    props_start_element,
    neon_xml_collect_cdata,
    props_end_element,
    &pc, extra_headers, NULL,
    check_errors,
    pool));

  *results = pc.props;
  return WEBDAV_NO_ERROR;
}

static error_t
neon_get_props_resource(
  neon_resource_t ** rsrc,
  neon_session_t * sess,
  const char * url,
  const ne_propname * which_props,
  bool check_errors,
  apr_pool_t * pool)
{
  apr_hash_t * props = NULL;
  char * url_path = apr_pstrdup(pool, url);
  apr_size_t len = strlen(url);
  // Clean up any trailing slashes.
  if ((len > 1) && (url[len - 1] == '/'))
    url_path[len - 1] = '\0';

  WEBDAV_ERR(neon_get_props(&props, sess, url_path, NEON_DEPTH_ZERO,
    which_props, check_errors, pool));

  // HACK.  We need to have the client canonicalize paths, get rid
  // of double slashes and such.  This check is just a check against
  // non-SVN servers;  in the long run we want to re-enable this.
  {
    // pick out the first response: the URL requested will not match
    // the response href.
    apr_hash_index_t * hi = apr_hash_first(pool, props);
    if (hi)
    {
      void * ent;
      apr_hash_this(hi, NULL, NULL, &ent);
      *rsrc = static_cast<neon_resource_t *>(ent);
    }
    else
      *rsrc = NULL;
  }

  if (*rsrc == NULL)
  {
    // hmmm, should have been in there...
    return error_createf(APR_EGENERAL, NULL,
      "Failed to find label '%s' for URL '%s'",
      "NULL", url_path);
  }

  return WEBDAV_NO_ERROR;
}

//------------------------------------------------------------------------------

static error_t
client_list2(
  session_t * session,
  const char * path_or_url,
  depth_t depth,
  apr_uint32_t dirent_fields,
  client_list_func_t list_func,
  void * baton,
  apr_pool_t * pool)
{
  dirent_t * dirent = NULL;
  const char * url = NULL;
  const char * webdav_root = NULL;
  const char * fs_path = NULL;
  error_t err = 0;

  assert(session);

  // We use the kind field to determine if we should recurse, so we
  // always need it.
  dirent_fields |= WEBDAV_DIRENT_KIND;

  WEBDAV_ERR(init_session_from_path(session,
    &url, path_or_url,
    pool));

  WEBDAV_ERR(get_webdav_resource_root2(session, &webdav_root, pool));

  WEBDAV_ERR(client_path_relative_to_root(&fs_path,
    url,
    webdav_root, TRUE, session,
    pool, pool));

  err = stat(session, "", &dirent, pool);

  if (err)
    return error_trace(err);

  if (!dirent)
    return error_createf(WEBDAV_ERR_FS_NOT_FOUND, NULL,
      "URL '%s' non-existent",
      url);

  // Report the dirent for the target.
  WEBDAV_ERR(list_func(baton, "", dirent, fs_path, pool));

  if (dirent->kind == node_dir && (depth == depth_files ||
      depth == depth_immediates ||
      depth == depth_infinity))
  {
    WEBDAV_ERR(get_dir_contents(dirent_fields, "",
      session,
      fs_path, depth,
      list_func, baton, pool));
  }
  return WEBDAV_NO_ERROR;
}

static error_t
client_get_file(
  session_t * session,
  const char * remote_path,
  apr_os_file_t * thefile,
  apr_pool_t * pool)
{
  const char * remote_url = NULL;
  WEBDAV_ERR(init_session_from_path(session,
    &remote_url, path_uri_encode(remote_path, pool),
    pool));

  stream_t * fstream = NULL;
  WEBDAV_ERR(stream_open_writable(&fstream, thefile,
    pool, pool));
  const char * src_rel = NULL;
  WEBDAV_ERR(get_path_relative_to_session(session, &src_rel,
    remote_url,
    pool));
  WEBDAV_ERR(get_file(session, src_rel, fstream, NULL, pool));
  WEBDAV_ERR(stream_close(fstream));
  return WEBDAV_NO_ERROR;
}

static error_t
client_put_file(
  session_t * session,
  const char * remote_path,
  const char * local_path,
  apr_pool_t * pool)
{
  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  assert(ras);

  error_t err = 0;
  int code = 0;
  apr_hash_t * extra_headers = NULL; // apr_hash_make(pool);

  neon_request_t * request = NULL;
  const char * put_target = path_uri_encode(remote_path, pool);

  apr_file_t * body_file = NULL;
  WEBDAV_ERR(io_file_open(&body_file, local_path, APR_READ | APR_BUFFERED | APR_BINARY,
    APR_OS_DEFAULT, pool));
  // create/prep the request
  WEBDAV_ERR(neon_request_create(&request, ras, "PUT",
    put_target, pool));
  // Give the file to neon. The provider will rewind the file.
  err = neon_set_neon_body_provider(request, body_file);
  if (err)
    goto cleanup;
  // run the request and get the resulting status code (and error_t)
  err = neon_request_dispatch(&code, request, extra_headers, NULL,
    201 /* Created */,
    204 /* No Content */,
    false,
    pool);
  if (err && (err == WEBDAV_ERR_DAV_REQUEST_FAILED))
  {
    err = error_createf(WEBDAV_ERR_CANNOT_PUT_FILE, NULL,
      "Cannot create '%s'"
      " (Status %d on PUT Request)",
      put_target, code);
  }

cleanup:
  neon_request_destroy(request);
  WEBDAV_ERR(err);
  return WEBDAV_NO_ERROR;
}

static error_t
client_move_file_or_directory(
  session_t * session,
  const char * remote_path_from,
  const char * remote_path_to,
  void * baton,
  apr_pool_t * pool)
{
  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  assert(ras);

  error_t err = 0;
  const char * target_from = path_uri_encode(remote_path_from, pool);
  const char * target_to = path_uri_encode(remote_path_to, pool);
  int code = 0;

  apr_hash_t * extra_headers = apr_hash_make(pool);
  apr_hash_set(extra_headers, "Destination", APR_HASH_KEY_STRING, target_to);
  neon_add_depth_header(extra_headers, NEON_DEPTH_INFINITE);
  err = neon_simple_request(&code, ras, "MOVE", target_from,
    extra_headers, NULL,
    201 /* Created */,
    204 /* No Content */,
    pool);
  if (err && (err == WEBDAV_ERR_DAV_REQUEST_FAILED))
  {
    err = error_createf(WEBDAV_ERR_CANNOT_MOVE, NULL,
      "Cannot move '%s' to '%s'"
      " (Status %d on MOVE Request)",
      target_from, target_to, code);
  }

  return err;
}

static error_t
client_delete_file(
  session_t * session,
  const char * remote_path,
  void * baton,
  apr_pool_t * pool)
{
  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  assert(ras);

  error_t err = 0;
  const char * target = path_uri_encode(remote_path, pool);
  int code = 0;

  err = neon_simple_request(&code, ras, "DELETE", target,
    NULL, NULL,
    200,
    204, // No Content
    pool);
  if (err && (err == WEBDAV_ERR_DAV_REQUEST_FAILED))
  {
    err = error_createf(WEBDAV_ERR_CANNOT_DELETE_FILE, NULL,
      "Cannot delete '%s'"
      " (Status %d on DELETE Request)",
      target, code);
  }

  return err;
}

static error_t
client_check_path(
  session_t * session,
  const char * remote_path,
  node_kind_t * kind,
  apr_pool_t * pool)
{
  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  assert(ras);

  *kind = node_none;

  error_t err = 0;
  char * target = apr_pstrdup(pool, path_uri_encode(remote_path, pool));

  const char * rel_path = NULL;
  apr_size_t len = strlen(target);
  if ((len > 1) && ((target)[len - 1] == '/'))
  {
    (target)[len - 1] = '\0';
  }
  if (*target == '/')
  {
    // check if root has trailing slash
    apr_size_t len = strlen(ras->webdav_root);
    if ((len > 1) && ((ras->webdav_root)[len - 1] == '/'))
    {
      target++;
    }
    const char * abs_path = apr_pstrcat(pool, ras->webdav_root, target, NULL);

    err = get_path_relative_to_root(
      session,
      &rel_path,
      abs_path,
      pool);
    WEBDAV_ERR(err);
  }
  else
  {
    rel_path = target;
  }
  err = check_path(
    session,
    rel_path,
    kind,
    pool
  );

  return err;
}

static error_t
client_make_directory(
  session_t * session,
  const char * remote_path,
  void * baton,
  apr_pool_t * pool)
{
  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  assert(ras);

  error_t err = 0;
  const char * target = path_uri_encode(remote_path, pool);
  int code = 0;

  err = neon_simple_request(
    &code, ras, "MKCOL", target,
    NULL, NULL,
    201, 207, pool);
  if (err && (err == WEBDAV_ERR_DAV_REQUEST_FAILED))
  {
    err = error_createf(WEBDAV_ERR_CANNOT_MKCOL, NULL,
      "Cannot create directory '%s'"
      " (Status %d on MKCOL Request)",
      target, code);
  }

  return err;
}

static error_t
client_send_propfind_request(
  session_t * session,
  const char * remote_path,
  int * response_code,
  apr_pool_t * pool)
{
  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  assert(ras);

  error_t err = 0;
  int code = 0;

  apr_hash_t * props = NULL;
  const char * target = path_uri_encode(remote_path, pool);
  char * url_path = apr_pstrdup(pool, target);

  WEBDAV_ERR(neon_get_props(&props, ras, url_path, NEON_DEPTH_ZERO,
    starting_props,
    false,
    pool));

  if (err && (err == WEBDAV_ERR_DAV_REQUEST_FAILED))
  {
    err = error_createf(WEBDAV_ERR_CANNOT_PROPFIND, NULL,
      "Cannot execute PROPFIND on '%s'"
      " (Status %d on PROPFIND Request)",
      target, code);
  }
  if (response_code)
    *response_code = code;
  return err;
}

//------------------------------------------------------------------------------
// from session.c

typedef struct neonprogress_baton_t
{
  neon_session_t * ras;
  apr_off_t last_progress;
  apr_time_t last_progress_time;
  apr_pool_t * pool;
} neonprogress_baton_t;

// Callback invoked to enter PKCS#11 PIN code.
static int
client_ssl_pkcs11_pin_entry(
  void * userdata,
  int attempt,
  const char * slot_descr,
  const char * token_label,
  unsigned int flags,
  char * pin)
{
  neon_session_t * ras = static_cast<neon_session_t *>(userdata);
  void * creds = NULL;
  auth_cred_ssl_client_cert_pw_t * pw_creds = NULL;

  // Always prevent PIN caching.
  auth_baton_set_parameter(ras->callbacks->auth_baton,
    AUTH_PARAM_NO_AUTH_CACHE, "");
  error_t err = 0;
  if (attempt == 0)
  {
    const char * realmstring;

    realmstring = apr_psprintf(ras->pool,
      "PIN for token \"%s\" in slot \"%s\"",
      token_label, slot_descr);

    err = auth_first_credentials(&creds,
      &(ras->auth_iterstate),
      AUTH_CRED_SSL_CLIENT_CERT_PW,
      realmstring,
      ras->callbacks->auth_baton,
      ras->pool);
  }
  else
  {
    err = auth_next_credentials(&creds, ras->auth_iterstate, ras->pool);
  }

  if (err || !creds)
  {
    error_clear(&err);
    return -1;
  }

  pw_creds = static_cast<auth_cred_ssl_client_cert_pw_t *>(creds);

  strncpy(pin, pw_creds->password, NE_SSL_P11PINLEN);

  return 0;
}

static bool
client_ssl_decrypt_cert(
  neon_session_t * ras,
  const char * cert_file,
  ne_ssl_client_cert * clicert)
{
  auth_iterstate_t * state = NULL;
  error_t error = 0;
  apr_pool_t * pool = NULL;
  bool ok = false;
  void * creds = NULL;
  int try_count = 0;

  apr_pool_create(&pool, ras->pool);
  for (try_count = 0; TRUE; ++try_count)
  {
    if (try_count == 0)
    {
      error = auth_first_credentials(&creds, &state,
        AUTH_CRED_SSL_CLIENT_CERT_PW,
        cert_file,
        ras->callbacks->auth_baton,
        pool);
    }
    else
    {
      error = auth_next_credentials(&creds, state, pool);
    }

    if (error || !creds)
    {
      // Failure or too many attempts
      error_clear(&error);
      break;
    }
    else
    {
      auth_cred_ssl_client_cert_pw_t * pw_creds = static_cast<auth_cred_ssl_client_cert_pw_t *>(creds);

      if (ne_ssl_clicert_decrypt(clicert, pw_creds->password) == 0)
      {
        error = auth_save_credentials(state, pool);
        if (error)
          error_clear(&error);

        // Success
        ok = TRUE;
        break;
      }
    }
  }
  webdav_pool_destroy(pool);

  return ok;
}

static void
client_ssl_callback(
  void * userdata,
  ne_session * sess,
  const ne_ssl_dname * const * dnames,
  int dncount)
{
  neon_session_t * ras = static_cast<neon_session_t *>(userdata);
  ne_ssl_client_cert * clicert = NULL;
  void * creds = NULL;
  auth_iterstate_t * state = NULL;
  const char * realmstring = NULL;
  apr_pool_t * pool = NULL;
  error_t error = 0;
  int try_count = 0;

  apr_pool_create(&pool, ras->pool);

  realmstring = apr_psprintf(pool, "%s://%s:%d", ras->root.scheme,
    ras->root.host, ras->root.port);

  for (try_count = 0; TRUE; ++try_count)
  {
    if (try_count == 0)
    {
      error = auth_first_credentials(&creds, &state,
        AUTH_CRED_SSL_CLIENT_CERT,
        realmstring,
        ras->callbacks->auth_baton,
        pool);
    }
    else
    {
      error = auth_next_credentials(&creds, state, pool);
    }

    if (error || !creds)
    {
      // Failure or too many attempts
      error_clear(&error);
      break;
    }
    else
    {
      auth_cred_ssl_client_cert_t * client_creds = static_cast<auth_cred_ssl_client_cert_t *>(creds);

      clicert = ne_ssl_clicert_read(client_creds->cert_file);
      if (clicert)
      {
        if (!ne_ssl_clicert_encrypted(clicert) ||
            client_ssl_decrypt_cert(ras, client_creds->cert_file,
              clicert))
        {
          ne_ssl_set_clicert(sess, clicert);
        }
        ne_ssl_clicert_free(clicert);
        clicert = NULL;
        break;
      }
    }
  }

  webdav_pool_destroy(pool);
}

static const apr_uint32_t neon_failure_map[][2] =
{
  { NE_SSL_NOTYETVALID,        WEBDAV_AUTH_SSL_NOTYETVALID },
  { NE_SSL_EXPIRED,            WEBDAV_AUTH_SSL_EXPIRED },
  { NE_SSL_IDMISMATCH,         WEBDAV_AUTH_SSL_CNMISMATCH },
  { NE_SSL_UNTRUSTED,          WEBDAV_AUTH_SSL_UNKNOWNCA }
};

// Convert neon's SSL failure mask to our own failure mask.
static apr_uint32_t
convert_neon_failures(
  int neon_failures)
{
  apr_uint32_t failures = 0;

  for (apr_size_t i = 0; i < sizeof(neon_failure_map) / (2 * sizeof(int)); ++i)
  {
    if (neon_failures & neon_failure_map[i][0])
    {
      failures |= neon_failure_map[i][1];
      neon_failures &= ~neon_failure_map[i][0];
    }
  }

  // Map any remaining neon failure bits to our OTHER bit.
  if (neon_failures)
  {
    failures |= WEBDAV_AUTH_SSL_OTHER;
  }

  return failures;
}

// A neon-session callback to validate the SSL certificate when the CA
// is unknown (e.g. a self-signed cert), or there are other SSL
// certificate problems.
static int
server_ssl_callback(
  void * userdata,
  int failures,
  const ne_ssl_certificate * cert)
{
  neon_session_t * ras = static_cast<neon_session_t *>(userdata);
  auth_cred_ssl_server_trust_t * server_creds = NULL;
  void * creds = NULL;
  auth_iterstate_t * state = NULL;
  apr_pool_t * pool = NULL;
  error_t error = 0;
  char * ascii_cert = ne_ssl_cert_export(cert);
  char * issuer_dname = ne_ssl_readable_dname(ne_ssl_cert_issuer(cert));
  auth_ssl_server_cert_info_t cert_info = {0};
  char fingerprint[NE_SSL_DIGESTLEN] = {0};
  char valid_from[NE_SSL_VDATELEN] = {0}, valid_until[NE_SSL_VDATELEN] = {0};
  apr_uint32_t * webdav_failures = static_cast<apr_uint32_t *>(apr_pcalloc(ras->pool, sizeof(*webdav_failures)));

  // Construct the realmstring, e.g. https://svn.collab.net:80
  const char * realmstring = apr_pstrdup(ras->pool, Format("%s://%s:%d", ras->root.scheme,
    ras->root.host, ras->root.port).c_str());

  *webdav_failures = convert_neon_failures(failures);
  auth_baton_set_parameter(ras->callbacks->auth_baton,
    AUTH_PARAM_SSL_SERVER_FAILURES,
    webdav_failures);

  // Extract the info from the certificate
  cert_info.hostname = ne_ssl_cert_identity(cert);
  if (ne_ssl_cert_digest(cert, fingerprint) != 0)
  {
    strcpy(fingerprint, "<unknown>");
  }
  cert_info.fingerprint = fingerprint;
  ne_ssl_cert_validity(cert, valid_from, valid_until);
  cert_info.valid_from = valid_from;
  cert_info.valid_until = valid_until;
  cert_info.issuer_dname = issuer_dname;
  cert_info.ascii_cert = ascii_cert;

  auth_baton_set_parameter(ras->callbacks->auth_baton,
    AUTH_PARAM_SSL_SERVER_CERT_INFO,
    &cert_info);

  apr_pool_create(&pool, ras->pool);
  error = auth_first_credentials(&creds, &state,
    AUTH_CRED_SSL_SERVER_TRUST,
    realmstring,
    ras->callbacks->auth_baton,
    pool);
  if (error || !creds)
  {
    error_clear(&error);
  }
  else
  {
    server_creds = static_cast<auth_cred_ssl_server_trust_t *>(creds);
    error = auth_save_credentials(state, pool);
    if (error)
    {
      // It would be nice to show the error to the user somehow...
      error_clear(&error);
    }
  }

  free(issuer_dname);
  free(ascii_cert);
  auth_baton_set_parameter(ras->callbacks->auth_baton,
    AUTH_PARAM_SSL_SERVER_CERT_INFO, NULL);

  webdav_pool_destroy(pool);
  return !server_creds;
}

// An `ne_request_auth' callback, see ne_auth.h. USERDATA is a struct proxy_auth_baton_t *

// If ATTEMPT < 10, copy USERDATA->username and USERDATA->password
// into USERNAME and PASSWORD respectively (but do not copy more than
// NE_ABUFSIZ bytes of either), and return zero to indicate to Neon
// that authentication should be attempted.

// If ATTEMPT >= 10, copy nothing into USERNAME and PASSWORD and
// return 1, to cancel further authentication attempts.

// Ignore REALM.

static int
proxy_auth(
  void * userdata,
  const char * realm,
  int attempt,
  char * username,
  char * password)
{
  proxy_auth_baton_t * pab = static_cast<proxy_auth_baton_t *>(userdata);

  if (attempt >= 10)
    return 1;

  // Else.

  strncpy(username, pab->username, NE_ABUFSIZ);
  strncpy(password, pab->password, NE_ABUFSIZ);

  return 0;
}

// A neon-session callback to 'pull' authentication data when
// challenged.  In turn, this routine 'pulls' the data from the client
// callbacks if needed.
static int
request_auth(
  void * userdata,
  const char * realm,
  int attempt,
  char * username,
  char * password)
{
  error_t err = 0;
  neon_session_t * ras = static_cast<neon_session_t *>(userdata);
  void * creds = NULL;
  auth_cred_simple_t * simple_creds = NULL;

  // Start by marking the current credentials invalid.
  ras->auth_used = false;

  // No auth_baton?  Give up.
  if (!ras->callbacks->auth_baton)
    return -1;

  // Neon automatically tries some auth protocols and bumps the attempt
  // count without using our callbacks, so we can't depend
  // on attempt == 0 the first time we are called -- we need to check
  // if the auth state has been initted as well.
  if (attempt == 0 || ras->auth_iterstate == NULL)
  {
    const char * realmstring = apr_psprintf(ras->pool, "<%s://%s:%d> %s",
      ras->root.scheme, ras->root.host,
      ras->root.port, realm);

    err = auth_first_credentials(&creds,
      &(ras->auth_iterstate),
      AUTH_CRED_SIMPLE,
      realmstring,
      ras->callbacks->auth_baton,
      ras->pool);
  }
  else // attempt > 0
    // TODO:  if the http realm changed this time around, we
    // should be calling first_creds(), not next_creds().
    err = auth_next_credentials(&creds,
      ras->auth_iterstate,
      ras->pool);
  if (err || !creds)
  {
    error_clear(&err);
    return -1;
  }
  simple_creds = static_cast<auth_cred_simple_t *>(creds);

  // Make neon_request_dispatch store the credentials after it
  // sees a successful response
  ras->auth_used = true;

  // silently truncates username/password to 256 chars.
  if (simple_creds->username) strncpy(username, simple_creds->username, NE_ABUFSIZ);
  if (simple_creds->password) strncpy(password, simple_creds->password, NE_ABUFSIZ);

  return 0;
}

// a cleanup routine attached to the pool that contains the RA session baton.
static apr_status_t
cleanup_session(
  void * sess)
{
  ne_session_destroy(static_cast<ne_session *>(sess));
  return APR_SUCCESS;
}

// a cleanup routine attached to the pool that contains the PKCS#11
// provider object.
static apr_status_t
cleanup_p11provider(
  void * provider)
{
  ne_ssl_pkcs11_provider * prov = static_cast<ne_ssl_pkcs11_provider *>(provider);
  ne_ssl_pkcs11_provider_destroy(prov);
  return APR_SUCCESS;
}

#ifdef NETBOX_DEBUG

typedef struct debug_file_baton_t
{
  FILE * file;
} debug_file_baton_t;

// a cleanup routine
static apr_status_t
cleanup_neon_debug_file(
  void * debug_file_baton)
{
  debug_file_baton_t * baton = static_cast<debug_file_baton_t *>(debug_file_baton);
  if (baton->file) fclose(baton->file);
  return APR_SUCCESS;
}
#endif

static void
progress_func(
  apr_off_t progress,
  apr_off_t total,
  void * baton,
  apr_pool_t * pool)
{
  client_ctx_t * ctx = static_cast<client_ctx_t *>(baton);
  TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(ctx->auth_baton->parameters,
    CONST_FS_KEY,
    APR_HASH_KEY_STRING));
  assert(fs);
  if (total == -1)
    fs->ReadDirectoryProgress(progress);
  else
    fs->FileTransferProgress(total, progress);
}

static void
ra_neon_neonprogress(
  void * baton,
  apr_off_t progress,
  apr_off_t total)
{
  neonprogress_baton_t * pb = static_cast<neonprogress_baton_t *>(baton);
  neon_session_t * ras = pb->ras;

  if (ras->progress_func)
  {
    apr_time_t now = apr_time_now();
    if (now - pb->last_progress_time > 200000) // 0.2 sec
    {
      if (total < 0)
      {
        // Neon sends the total number of bytes sent for this specific
        // session and there are two sessions active at once.
        // For this case we combine the totals to allow clients to provide
        // a better progress indicator.

        if (progress >= pb->last_progress)
          ras->total_progress += (progress - pb->last_progress);
        else
          // Session total has been reset. A new stream started
          ras->total_progress += pb->last_progress;

        pb->last_progress = progress;

        ras->progress_func(ras->total_progress, -1, ras->progress_baton, pb->pool);
      }
      else
      {
        // Neon provides total bytes to receive information. Pass literally
        // to allow providing a percentage.
        ras->progress_func(progress, total, ras->progress_baton, pb->pool);
      }
      pb->last_progress_time = now;
    }
  }
}

static atomic_t neon_initialized = 0;

static error_t
initialize_neon(
  void * baton,
  apr_pool_t * scratch_pool)
{
  if (ne_sock_init() != 0)
    return error_create(WEBDAV_ERR_DAV_SOCK_INIT, NULL,
      "Network socket initialization failed");

  return WEBDAV_NO_ERROR;
}

static error_t
ensure_neon_initialized()
{
  return atomic_init_once(&neon_initialized, initialize_neon, NULL, NULL);
}

static const char * const *
ra_neon_get_schemes(apr_pool_t * pool)
{
  static const char * schemes_no_ssl[] = { "http", NULL };
  static const char * schemes_ssl[] = { "http", "https", NULL };

  return ne_has_support(NE_FEATURE_SSL) ? schemes_ssl : schemes_no_ssl;
}

static error_t
neon_open(
  session_t * session,
  const char ** corrected_url,
  const char * session_URL,
  const callbacks2_t * callbacks,
  void * callback_baton,
  apr_pool_t * pool)
{
  assert(callback_baton);
  callback_baton_t * cb = static_cast<callback_baton_t *>(callback_baton);

  *corrected_url = NULL;

  ne_uri * uri = NULL;
  WEBDAV_ERR(parse_ne_uri(&uri, session_URL, pool));

  // Initialize neon if required
  WEBDAV_ERR(ensure_neon_initialized());

  int is_ssl_session = (strcmp(uri->scheme, "https") == 0);
  if (is_ssl_session)
  {
    if (ne_has_support(NE_FEATURE_SSL) == 0)
      return error_create(WEBDAV_ERR_DAV_SOCK_INIT, NULL,
        "TLS is not supported");
  }

  ne_session * sess = ne_session_create(uri->scheme, uri->host, uri->port);
  apr_pool_cleanup_register(pool, sess, cleanup_session, apr_pool_cleanup_null);
  bool compression = FALSE;
  unsigned int neon_auth_types = 0;
  const char * pkcs11_provider = NULL;
  const char * ssl_authority_file = NULL;
  {
    int proxy_method = 0;
    const char * proxy_host = NULL;
    unsigned int proxy_port = 0;
    const char * proxy_username = NULL;
    const char * proxy_password = NULL;
    int timeout = 0;
    int debug = 0;
    const char * neon_debug_file_name = NULL;

    TWebDAVFileSystem * fs = static_cast<TWebDAVFileSystem *>(apr_hash_get(cb->ctx->auth_baton->parameters,
      CONST_FS_KEY,
      APR_HASH_KEY_STRING));
    assert(fs);
    WEBDAV_ERR(fs->GetServerSettings(
      &proxy_method,
      &proxy_host,
      &proxy_port,
      &proxy_username,
      &proxy_password,
      &timeout,
      &debug,
      &neon_debug_file_name,
      &compression,
      &pkcs11_provider,
      &ssl_authority_file,
      pool));
    if (neon_auth_types == 0)
    {
      // If there were no auth types specified in the configuration
      // file, provide the appropriate defaults.
      neon_auth_types = NE_AUTH_BASIC | NE_AUTH_DIGEST;
      if (is_ssl_session)
        neon_auth_types |= NE_AUTH_NEGOTIATE;
    }

    if (debug && neon_debug_file_name)
    {
#ifdef NETBOX_DEBUG
      debug_file_baton_t * baton = static_cast<debug_file_baton_t *>(apr_pcalloc(pool, sizeof(*baton)));
      neon_debug_file_name = apr_pstrcat(pool, neon_debug_file_name, ".neondebug.log", NULL);
      baton->file = _fsopen(neon_debug_file_name, "w", SH_DENYWR);
      if (baton->file)
      {
        debug = NE_DBG_HTTP |
                // NE_DBG_XML | NE_DBG_HTTPAUTH |
                NE_DBG_HTTPPLAIN |
                // NE_DBG_XMLPARSE |
                NE_DBG_HTTPBODY |
                // NE_DBG_SSL |
                NE_DBG_FLUSH;
        ne_debug_init(baton->file, debug);
      }
      apr_pool_cleanup_register(pool, baton,
        cleanup_neon_debug_file,
        apr_pool_cleanup_null);
#else
      ne_debug_init(NULL, 0);
#endif // #ifdef NETBOX_DEBUG
    }

    TProxyMethod method = (TProxyMethod)proxy_method;
    if (method != ::pmNone)
    {
      if ((method == pmSocks4) || (method == pmSocks5))
      {
        enum ne_sock_sversion vers = method == pmSocks4 ? NE_SOCK_SOCKSV4A : NE_SOCK_SOCKSV5;
        ne_session_socks_proxy(sess, vers, proxy_host, proxy_port, proxy_username, proxy_password);
      }
      else if (proxy_host)
      {
        ne_session_proxy(sess, proxy_host, proxy_port);

        if (proxy_username)
        {
          proxy_auth_baton_t * pab = static_cast<proxy_auth_baton_t *>(apr_pcalloc(pool, sizeof(*pab)));
          pab->username = proxy_username;
          pab->password = proxy_password ? proxy_password : "";

          ne_set_proxy_auth(sess, proxy_auth, pab);
        }
        else
        {
          // Enable (only) the Negotiate scheme for proxy
          // authentication, if no username/password is
          // configured.
          ne_add_proxy_auth(sess, NE_AUTH_NEGOTIATE, NULL, NULL);
        }
      }
    }

    if (!timeout)
      timeout = DEFAULT_HTTP_TIMEOUT;
    ne_set_read_timeout(sess, timeout);

    ne_set_connect_timeout(sess, timeout);
  }

  {
    static std::string useragent = "WinSCP";
    ne_set_useragent(sess, useragent.c_str());
  }

  // Create and fill a session_baton.
  neon_session_t * ras = static_cast<neon_session_t *>(apr_pcalloc(pool, sizeof(*ras)));
  ras->pool = pool;
  {
    // canonicalize url
    const char * remote_url = urlpath_canonicalize(session_URL, pool);
    ras->url = stringbuf_create(remote_url, pool);
  }
  // copies uri pointer members, they get free'd in __close.
  ras->root = *uri;
  ras->ne_sess = sess;
  ras->callbacks = callbacks;
  ras->callback_baton = callback_baton;
  ras->compression = compression;
  ras->progress_baton = callbacks->progress_baton;
  ras->progress_func = callbacks->progress_func;
  ras->capabilities = apr_hash_make(ras->pool);

  // note that ras->username and ras->password are still NULL at this point.
  // Register an authentication 'pull' callback with the neon sessions
  ne_add_server_auth(sess, neon_auth_types, request_auth, ras);

  if (is_ssl_session)
  {
    bool trust_default_ca = false;
    // PEM-encoded Certificate Authority (CA) SSL certificate
    const char * authorities = ssl_authority_file;

    if (authorities != NULL && *authorities)
    {
      const char * file = authorities;
      ne_ssl_certificate * ca_cert = NULL;
      ca_cert = ne_ssl_cert_read(file);
      if (ca_cert == NULL)
      {
        return error_createf(
          WEBDAV_ERR_BAD_CONFIG_VALUE, NULL,
          "Invalid config: unable to load certificate file '%s'", file);
      }
      ne_ssl_trust_cert(sess, ca_cert);
      ne_ssl_cert_free(ca_cert);
      ca_cert = NULL;
    }

    // When the CA certificate or server certificate has
    // verification problems, neon will call our verify function before
    // outright rejection of the connection.*/
    ne_ssl_set_verify(sess, server_ssl_callback, ras);
    // For client connections, we register a callback for if the server
    // wants to authenticate the client via client certificate.

    if (pkcs11_provider && *pkcs11_provider)
    {
      ne_ssl_pkcs11_provider * provider;
      int rv;

      // Initialize the PKCS#11 provider.
      rv = ne_ssl_pkcs11_provider_init(&provider, pkcs11_provider);
      if (rv != NE_PK11_OK)
      {
        return error_createf(WEBDAV_ERR_BAD_CONFIG_VALUE, NULL,
          "Invalid config: unable to load PKCS#11 provider '%s'",
          pkcs11_provider);
      }

      // Share the provider between the two sessions.
      ne_ssl_set_pkcs11_provider(sess, provider);

      ne_ssl_pkcs11_provider_pin(provider, client_ssl_pkcs11_pin_entry, ras);

      apr_pool_cleanup_register(pool, provider, cleanup_p11provider,
        apr_pool_cleanup_null);
    }
    // Note the "else"; if a PKCS#11 provider is set up, a client
    // cert callback is already configured, so don't displace it
    // with the normal one here.
    else
    {
      ne_ssl_provide_clicert(sess, client_ssl_callback, ras);
    }

    // See if the user wants us to trust "default" openssl CAs.
    // TODO: option "trust default CA"
    trust_default_ca = true;

    if (trust_default_ca)
    {
      ne_ssl_trust_default_ca(sess);
    }
  }

  if (ras->progress_func)
  {
    neonprogress_baton_t * progress1 = static_cast<neonprogress_baton_t *>(apr_pcalloc(pool, sizeof(*progress1)));
    neonprogress_baton_t * progress2 = static_cast<neonprogress_baton_t *>(apr_pcalloc(pool, sizeof(*progress2)));

    progress1->pool = pool;
    progress1->ras = ras;
    progress1->last_progress = 0;

    progress2->pool = pool;
    progress2->ras = ras;
    progress2->last_progress = 0;

    ne_set_progress(sess, ra_neon_neonprogress, progress1);
  }

  session->priv = ras;

  const char * corrected = NULL;
  error_t err = neon_exchange_capabilities(ras, &corrected, pool);
  if (corrected)
    *corrected_url = corrected;
  return err;
}

static error_t
neon_reparent(
  session_t * session,
  const char * url,
  apr_pool_t * pool)
{
  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  ne_uri * uri = NULL;

  WEBDAV_ERR(parse_ne_uri(&uri, url, session->pool));

  ras->root = *uri;
  stringbuf_set(ras->url, url);
  ras->webdav_root = neon_uri_unparse(uri, session->pool);
  return WEBDAV_NO_ERROR;
}

static error_t
neon_get_session_url(
  session_t * session,
  const char ** url,
  apr_pool_t * pool)
{
  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  assert(ras);
  *url = apr_pstrmemdup(pool, ras->url->data, ras->url->len);
  return WEBDAV_NO_ERROR;
}

static const ne_propname restype_props[] =
{
  { "DAV:", "resourcetype" },
  { NULL }
};

static error_t
neon_get_file(
  session_t * session,
  const char * path,
  stream_t * stream,
  apr_hash_t ** props,
  apr_pool_t * pool)
{
  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  assert(ras);
  const char * url = path_url_add_component2(ras->url->data, path, pool);
  const ne_propname * which_props = NULL;

  const char * final_url = url;

  if (props)
  {
    // Request all properties if caller requested them.
    which_props = starting_props;
  }
  else
  {
    // Request only resource type on other cases.
    which_props = restype_props;
  }

  neon_resource_t * rsrc = NULL;
  WEBDAV_ERR(neon_get_props_resource(&rsrc, ras, final_url,
    which_props,
    false,
    pool));
  if (rsrc->is_collection)
  {
    return error_create(WEBDAV_ERR_FS_NOT_FILE, NULL,
      "Can't get text contents of a directory");
  }

  if (stream)
  {
    file_write_ctx_t fwc = {0};
    fwc.stream = stream;

    // Fetch the file, shoving it at the provided stream.
    WEBDAV_ERR(custom_get_request(ras, final_url, path,
      get_file_reader, &fwc,
      ras->callback_baton,
      pool));
  }
  return WEBDAV_NO_ERROR;
}

static error_t
neon_get_dir(
  session_t * session,
  apr_hash_t ** dirents,
  const char * path,
  apr_uint32_t dirent_fields,
  apr_pool_t * pool)
{
  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  const char * url = path_url_add_component2(ras->url->data, path, pool);

  const char * final_url = url;
  if (dirents)
  {
    // Just like Nautilus, Cadaver, or any other browser, we do a
    // PROPFIND on the directory of depth 1.
    apr_hash_t * resources = NULL;
    WEBDAV_ERR(neon_get_props(&resources, ras,
      final_url, NEON_DEPTH_ONE,
      starting_props,
      false,
      pool));

    // Count the number of path components in final_url.
    apr_size_t final_url_n_components = path_component_count(final_url);

    // Now we have a hash that maps a bunch of url children to resource
    // objects. Each resource object contains the properties of the
    // child. Parse these resources into dirent_t structs.
    *dirents = apr_hash_make(pool);
    for (apr_hash_index_t * hi = apr_hash_first(pool, resources); hi;
         hi = apr_hash_next(hi))
    {
      const void * key = NULL;
      void * val = NULL;
      const char * childname = NULL;
      neon_resource_t * resource = NULL;
      const string_t * propval = NULL;
      dirent_t * entry = NULL;

      apr_hash_this(hi, &key, NULL, &val);
      childname = relpath_canonicalize(static_cast<const char *>(key), pool);
      resource = static_cast<neon_resource_t *>(val);

      // Skip the effective '.' entry that comes back from
      // NEON_DEPTH_ONE. The children must have one more
      // component then final_url.
      // Note that we can't just strcmp the URLs because of URL encoding
      // differences (i.e. %3c vs. %3C etc.)
      if (path_component_count(childname) < final_url_n_components - 1)
        continue;

      entry = static_cast<dirent_t *>(apr_pcalloc(pool, sizeof(*entry)));

      if (dirent_fields & WEBDAV_DIRENT_KIND)
      {
        // node kind
        entry->kind = resource->is_collection ? node_dir : node_file;
      }

      if (dirent_fields & WEBDAV_DIRENT_SIZE)
      {
        // size
        propval = static_cast<const string_t *>(apr_hash_get(resource->propset,
          NEON_PROP_GETCONTENTLENGTH,
          APR_HASH_KEY_STRING));
        if (propval == NULL)
          entry->size = 0;
        else
          entry->size = atoui64(propval->data);
      }

      if (dirent_fields & WEBDAV_DIRENT_TIME)
      {
        propval = static_cast<const string_t *>(apr_hash_get(resource->propset,
          NEON_PROP_CREATIONDATE,
          APR_HASH_KEY_STRING));
        if (propval != NULL)
          WEBDAV_ERR(time_from_cstring(&(entry->time),
            propval->data, pool));
      }

      apr_hash_set(*dirents,
        path_uri_decode(relpath_basename(childname,
        pool),
        pool),
        APR_HASH_KEY_STRING, entry);
    }
  }

  return WEBDAV_NO_ERROR;
}

static error_t
neon_check_path(
  session_t * session,
  const char * path,
  node_kind_t * kind,
  apr_pool_t * pool)
{
  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  assert(ras);

  const char * url = ras->url->data;
  error_t err = 0;
  bool is_dir = FALSE;

  // If we were given a relative path to append, append it.
  if (path)
    url = path_url_add_component2(url, path, pool);

  if (!err)
  {
    neon_resource_t * rsrc;
    const char * full_bc_url = url;

    // query the DAV:resourcetype of the full, assembled URL.
    err = neon_get_starting_props(&rsrc, ras, full_bc_url, true, pool);
    if (!err)
      is_dir = rsrc->is_collection != 0;
  }

  if (err == WEBDAV_NO_ERROR)
  {
    if (is_dir)
      *kind = node_dir;
    else
      *kind = node_file;
  }
  else if (err == WEBDAV_ERR_FS_NOT_FOUND)
  {
    error_clear(&err);
    err = WEBDAV_NO_ERROR;
    *kind = node_none;
  }

  return err;
}

static error_t
neon_stat(
  session_t * session,
  const char * path,
  dirent_t ** dirent,
  apr_pool_t * pool)
{
  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  assert(ras);
  const char * url = ras->url->data;

  // If we were given a relative path to append, append it.
  if (path)
    url = path_url_add_component2(url, path, pool);

  const char * final_url = url;

  // Depth-zero PROPFIND is the One True DAV Way.
  apr_hash_t * resources = NULL;
  error_t err = neon_get_props(&resources, ras, final_url,
    NEON_DEPTH_ZERO,
    starting_props,
    false,
    pool);
  if (err)
  {
    if (err == WEBDAV_ERR_FS_NOT_FOUND)
    {
      // easy out:
      error_clear(&err);
      *dirent = NULL;
      return WEBDAV_NO_ERROR;
    }
    else
      return err;
  }

  // Copying parsing code from neon_get_dir() here. The hash
  // of resources only contains one item, but there's no other way to
  // get the item.
  for (apr_hash_index_t * hi = apr_hash_first(pool, resources); hi;
       hi = apr_hash_next(hi))
  {
    void * val = NULL;
    apr_hash_this(hi, NULL, NULL, &val);
    neon_resource_t * resource = static_cast<neon_resource_t *>(val);

    dirent_t * entry = static_cast<dirent_t *>(apr_pcalloc(pool, sizeof(*entry)));

    entry->kind = resource->is_collection ? node_dir : node_file;

    const string_t * propval = NULL;
    // entry->size is already 0 by virtue of pcalloc().
    if (entry->kind == node_file)
    {
      propval = static_cast<const string_t *>(apr_hash_get(resource->propset,
        NEON_PROP_GETCONTENTLENGTH,
        APR_HASH_KEY_STRING));
      if (propval)
        entry->size = atoui64(propval->data);
    }

    propval = static_cast<const string_t *>(apr_hash_get(resource->propset,
      NEON_PROP_CREATIONDATE,
      APR_HASH_KEY_STRING));
    if (propval != NULL)
      WEBDAV_ERR(time_from_cstring(&(entry->time),
        propval->data, pool));

    *dirent = entry;
  }

  return WEBDAV_NO_ERROR;
}

static error_t
neon_get_webdav_resource_root(
  session_t * session,
  const char ** url,
  apr_pool_t * pool)
{
  neon_session_t * ras = static_cast<neon_session_t *>(session->priv);
  assert(ras);

  *url = ras->webdav_root;
  return WEBDAV_NO_ERROR;
}

static const vtable_t neon_vtable =
{
  NULL, // get_description
  ra_neon_get_schemes, // get_schemes
  neon_open, // open_session
  neon_reparent, // reparent
  neon_get_session_url, // get_session_url
  neon_get_file, // get_file
  neon_get_dir, // get_dir
  neon_check_path, // check_path
  neon_stat, // stat
  neon_get_webdav_resource_root, // get_webdav_resource_root
};

static error_t
neon_init(
  const vtable_t ** vtable,
  apr_pool_t * pool)
{
  *vtable = &neon_vtable;

  return WEBDAV_NO_ERROR;
}

} // namespace webdav

//---------------------------------------------------------------------------
class TSessionData;
//---------------------------------------------------------------------------
class TWebDAVFileListHelper
{
public:
  explicit TWebDAVFileListHelper(TWebDAVFileSystem * FileSystem, TRemoteFileList * FileList,
    bool IgnoreFileList) :
    FFileSystem(FileSystem),
    FFileList(FFileSystem->FFileList),
    FIgnoreFileList(FFileSystem->FIgnoreFileList)
  {
    FFileSystem->FFileList = FileList;
    FFileSystem->FIgnoreFileList = IgnoreFileList;
  }

  ~TWebDAVFileListHelper()
  {
    FFileSystem->FFileList = FFileList;
    FFileSystem->FIgnoreFileList = FIgnoreFileList;
  }

private:
  TWebDAVFileSystem * FFileSystem;
  TRemoteFileList * FFileList;
  bool FIgnoreFileList;
};

//---------------------------------------------------------------------------
#undef FILE_OPERATION_LOOP_EX
#define FILE_OPERATION_LOOP_EX(ALLOW_SKIP, MESSAGE, OPERATION) \
  FILE_OPERATION_LOOP_CUSTOM(FTerminal, ALLOW_SKIP, MESSAGE, OPERATION, L"")
//---------------------------------------------------------------------------
static const UnicodeString CONST_WEBDAV_PROTOCOL_BASE_NAME = L"WebDAV";

//===========================================================================

TWebDAVFileSystem::TWebDAVFileSystem(TTerminal * ATerminal) :
  TCustomFileSystem(ATerminal),
  FFileList(NULL),
  FOnCaptureOutput(NULL),
  FPasswordFailed(false),
  FActive(false),
  FFileTransferAbort(ftaNone),
  FIgnoreFileList(false),
  FFileTransferCancelled(false),
  FFileTransferResumed(0),
  FFileTransferPreserveTime(false),
  FHasTrailingSlash(false),
  FFileTransferCPSLimit(0),
  FLastReadDirectoryProgress(0),
  FCurrentOperationProgress(NULL),
  FTransferStatusCriticalSection(new TCriticalSection()),
  webdav_pool(NULL),
  FSession(NULL)
{
  FFileSystemInfo.ProtocolBaseName = CONST_WEBDAV_PROTOCOL_BASE_NAME;
  FFileSystemInfo.ProtocolName = FFileSystemInfo.ProtocolBaseName;

  if (apr_initialize() != APR_SUCCESS)
    throw ExtException(UnicodeString(L"Cannot init APR"), NULL);
  apr_pool_create(&webdav_pool, NULL);
}
//---------------------------------------------------------------------------
__fastcall TWebDAVFileSystem::~TWebDAVFileSystem()
{
  delete FTransferStatusCriticalSection;
  FTransferStatusCriticalSection = NULL;

  webdav_pool_destroy(webdav_pool);
  apr_terminate();
  webdav_pool = NULL;
  ne_sock_exit();
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::Open()
{
  FCurrentDirectory = L"";
  FHasTrailingSlash = false;

  TSessionData * Data = FTerminal->SessionData;

  FSessionInfo.LoginTime = Now();

  bool Ssl = (FTerminal->SessionData->Ftps != ftpsNone);
  if (Ssl)
  {
    FSessionInfo.SecurityProtocolName = LoadStr(FTPS_IMPLICIT);
  }

  UnicodeString HostName = Data->HostNameExpanded;
  size_t Port = Data->PortNumber;
  UnicodeString ProtocolName = !Ssl ? L"http" : L"https";
  UnicodeString UserName = Data->UserNameExpanded;
  UnicodeString Path = Data->RemoteDirectory;
  UnicodeString Url = FORMAT(L"%s://%s:%d%s", (ProtocolName.c_str(), HostName.c_str(), Port, Path.c_str()));

  FPasswordFailed = false;

  FTerminal->Information(LoadStr(STATUS_CONNECT), true);
  for (int I = 0; I < 5; I++)
  {
    FActive = false;
    try
    {
      FActive = (WEBDAV_NO_ERROR == OpenURL(Url, webdav_pool));
      if (FActive)
      {
        break;
      }
    }
    catch (...)
    {
      if (FFileTransferCancelled)
        break;
      apr_sleep(200000); // 0.2 sec
    }
  }
  if (!FActive)
  {
    FTerminal->Closed();
    throw Exception(LoadStr(CONNECTION_FAILED));
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::Close()
{
  assert(FActive);
  FTerminal->Closed();
  FActive = false;
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::GetActive()
{
  return FActive;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CollectUsage()
{
}
//---------------------------------------------------------------------------
const TSessionInfo & __fastcall TWebDAVFileSystem::GetSessionInfo()
{
  return FSessionInfo;
}
//---------------------------------------------------------------------------
const TFileSystemInfo & __fastcall TWebDAVFileSystem::GetFileSystemInfo(bool Retrieve)
{
  return FFileSystemInfo;
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::TemporaryTransferFile(const UnicodeString & /*FileName*/)
{
  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::GetStoredCredentialsTried()
{
  return false;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWebDAVFileSystem::GetUserName()
{
  return FUserName;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::Idle()
{
  // TODO: Keep session alive
  return;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWebDAVFileSystem::AbsolutePath(const UnicodeString Path, bool /*Local*/)
{
  return ::AbsolutePath(GetCurrentDirectory(), Path);
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::IsCapable(int Capability) const
{
  assert(FTerminal);
  switch (Capability)
  {
    case fcUserGroupListing:
    case fcModeChanging:
    case fcModeChangingUpload:
    case fcPreservingTimestampUpload:
    case fcGroupChanging:
    case fcOwnerChanging:
    case fcAnyCommand:
    case fcShellAnyCommand:
    case fcHardLink:
    case fcSymbolicLink:
    case fcResolveSymlink:
      return false;
    case fcRename:
    case fcRemoteMove:
    case fcRemoteCopy:
      return true;

    case fcTextMode:
    case fcNativeTextMode:
    case fcNewerOnlyUpload:
    case fcTimestampChanging:
    case fcLoadingAdditionalProperties:
    case fcCheckingSpaceAvailable:
    case fcIgnorePermErrors:
    case fcCalculatingChecksum:
    case fcSecondaryShell: // has fcShellAnyCommand
    case fcGroupOwnerChangingByID: // by name
    case fcRemoveCtrlZUpload:
    case fcRemoveBOMUpload:
      return false;

    default:
      assert(false);
      return false;
  }
}

//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::EnsureLocation()
{
  if (!FCachedDirectoryChange.IsEmpty())
  {
    FTerminal->LogEvent(FORMAT(L"Locating to cached directory \"%s\".",
      (FCachedDirectoryChange.c_str())));
    UnicodeString Directory = FCachedDirectoryChange;
    FCachedDirectoryChange = L"";
    try
    {
      ChangeDirectory(Directory);
    }
    catch (...)
    {
      // when location to cached directory fails, pretend again
      // location in cached directory
      // here used to be check (CurrentDirectory != Directory), but it is
      // false always (currentdirectory is already set to cached directory),
      // making the condition below useless. check removed.
      if (FTerminal->GetActive())
      {
        FCachedDirectoryChange = Directory;
      }
      throw;
    }
  }
}

//---------------------------------------------------------------------------
UnicodeString __fastcall TWebDAVFileSystem::GetCurrentDirectory()
{
  return FCurrentDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::DoStartup()
{
  FTerminal->SetExceptionOnFail(true);
  // retrieve initialize working directory to save it as home directory
  ReadCurrentDirectory();
  FTerminal->SetExceptionOnFail(false);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::LookupUsersGroups()
{
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ReadCurrentDirectory()
{
  if (FCachedDirectoryChange.IsEmpty())
  {
    FCurrentDirectory = FCurrentDirectory.IsEmpty() ? UnicodeString(L"/") : FCurrentDirectory;
  }
  else
  {
    FCurrentDirectory = FCachedDirectoryChange;
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::HomeDirectory()
{
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::AnnounceFileListOperation()
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::DoChangeDirectory(const UnicodeString Directory)
{
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ChangeDirectory(const UnicodeString ADirectory)
{
  UnicodeString Directory = ADirectory;
  bool HasTrailingSlash = (Directory.Length() > 0) && (Directory[Directory.Length()] == L'/');
  try
  {
    // For changing directory, we do not make paths absolute, instead we
    // delegate this to the server, hence we synchronize current working
    // directory with the server and only then we ask for the change with
    // relative path.
    // But if synchronization fails, typically because current working directory
    // no longer exists, we fall back to out own resolution, to give
    // user chance to leave the non-existing directory.
    EnsureLocation();
  }
  catch (...)
  {
    if (FTerminal->GetActive())
    {
      Directory = AbsolutePath(Directory, false);
      if (HasTrailingSlash)
        Directory = ::UnixIncludeTrailingBackslash(Directory);
    }
    else
    {
      throw;
    }
  }

  FCurrentDirectory = AbsolutePath(Directory, false);
  if (HasTrailingSlash)
    FCurrentDirectory = ::UnixIncludeTrailingBackslash(FCurrentDirectory);

  // make next ReadCurrentDirectory retrieve actual server-side current directory
  FCachedDirectoryChange = L"";
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CachedChangeDirectory(const UnicodeString Directory)
{
  FCachedDirectoryChange = UnixExcludeTrailingBackslash(Directory);
}

void __fastcall TWebDAVFileSystem::DoReadDirectory(TRemoteFileList * FileList)
{
  FileList->Reset();
  // add parent directory
  FileList->AddFile(new TRemoteParentDirectory(FTerminal));

  FLastReadDirectoryProgress = 0;

  TWebDAVFileListHelper Helper(this, FileList, false);

  // always specify path to list, do not attempt to
  // list "current" dir as:
  // 1) List() lists again the last listed directory, not the current working directory
  // 2) we handle this way the cached directory change
  UnicodeString Directory = AbsolutePath(FileList->Directory, false);
  if (FHasTrailingSlash)
    Directory = ::UnixIncludeTrailingBackslash(Directory);
  WebDAVGetList(Directory);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ReadDirectory(TRemoteFileList * FileList)
{
  assert(FileList);
  bool Repeat = false;

  do
  {
    Repeat = false;
    try
    {
      DoReadDirectory(FileList);
    }
    catch (Exception &)
    {
      if (!FTerminal->GetActive())
      {
        FTerminal->Reopen(ropNoReadDirectory);
        Repeat = true;
      }
      else
      {
        throw;
      }
    }
  }
  while (Repeat);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ReadSymlink(TRemoteFile * SymlinkFile,
    TRemoteFile *& File)
{
  CustomReadFile(SymlinkFile->LinkTo, File, SymlinkFile);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ReadFile(const UnicodeString FileName,
    TRemoteFile *& File)
{
  CustomReadFile(FileName, File, NULL);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CustomReadFile(const UnicodeString FileName,
    TRemoteFile *& File, TRemoteFile * ALinkedByFile)
{
  File = NULL;
  bool isExist = false;
  int is_dir = 0;
  isExist = WebDAVCheckExisting(FileName.c_str(), is_dir);
  if (isExist)
  {
    File = new TRemoteFile();
    if (is_dir)
      File->Type = FILETYPE_DIRECTORY;
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::DeleteFile(const UnicodeString FileName,
  const TRemoteFile * File, int Params, TRmSessionAction & Action)
{
  USEDPARAM(File);
  USEDPARAM(Params);
  UnicodeString FullFileName = File->FullFileName;
  bool res = WebDAVDeleteFile(FullFileName.c_str());
  if (!res)
  {
    THROW_SKIP_FILE(NULL, L"");
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::RenameFile(const UnicodeString FileName,
    const UnicodeString NewName)
{
  UnicodeString FullFileName = ::UnixIncludeTrailingBackslash(FCurrentDirectory) + FileName;
  bool res = WebDAVRenameFile(FullFileName.c_str(), NewName.c_str());
  if (!res)
  {
    THROW_SKIP_FILE(NULL, L"");
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CopyFile(const UnicodeString FileName,
    const UnicodeString NewName)
{
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CreateDirectory(const UnicodeString DirName)
{
  UnicodeString FullDirName = AbsolutePath(DirName, true);
  bool res = WebDAVMakeDirectory(FullDirName.c_str());
  if (!res)
  {
    TStringList * Strings = new TStringList();
    Strings->Delimiter = L'/';
    Strings->DelimitedText = DirName;
    UnicodeString CurDir;
    for (int i = 0; i < Strings->Count; i++)
    {
      if (Strings->Strings[i].IsEmpty())
      {
        continue;
      }
      CurDir += L"/" + Strings->Strings[i];
      res = WebDAVMakeDirectory(CurDir.c_str());
    }
    if (!res)
    {
      THROW_SKIP_FILE(NULL, L"");
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CreateLink(const UnicodeString FileName,
    const UnicodeString PointTo, bool Symbolic)
{
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ChangeFileProperties(const UnicodeString FileName,
    const TRemoteFile * File, const TRemoteProperties * Properties,
    TChmodSessionAction & Action)
{
  assert(Properties);
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::LoadFilesProperties(TStrings * /*FileList*/)
{
  assert(false);
  return false;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CalculateFilesChecksum(const UnicodeString & /*Alg*/,
    TStrings * /*FileList*/, TStrings * /*Checksums*/,
    TCalculatedChecksumEvent /*OnCalculatedChecksum*/)
{
  assert(false);
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::ConfirmOverwrite(UnicodeString & FileName,
    TOverwriteMode & OverwriteMode, TFileOperationProgressType * OperationProgress,
    const TOverwriteFileParams * FileParams, const TCopyParamType * CopyParam,
    int Params, bool AutoResume, unsigned int &Answer)
{
  bool Result;
  bool CanAutoResume = FLAGSET(Params, cpNoConfirmation) && AutoResume;
  bool CanResume = false; // disable resume

  Answer = 0;
  if (CanAutoResume && CanResume)
  {
    Answer = qaRetry;
  }
  else
  {
    // retry = "resume"
    // all = "yes to newer"
    // ignore = "rename"
    int Answers = qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll | qaAll | qaIgnore;
    if (CanResume)
    {
      Answers |= qaRetry;
    }
    TQueryButtonAlias Aliases[3];
    Aliases[0].Button = qaRetry;
    Aliases[0].Alias = LoadStr(RESUME_BUTTON);
    Aliases[1].Button = qaAll;
    Aliases[1].Alias = LoadStr(YES_TO_NEWER_BUTTON);
    Aliases[2].Button = qaIgnore;
    Aliases[2].Alias = LoadStr(RENAME_BUTTON);
    TQueryParams QueryParams(qpNeverAskAgainCheck);
    QueryParams.Aliases = Aliases;
    QueryParams.AliasesCount = LENOF(Aliases);
    SUSPEND_OPERATION (
      Answer = FTerminal->ConfirmFileOverwrite(FileName, FileParams,
                 Answers, &QueryParams,
                 OperationProgress->Side == osLocal ? osRemote : osLocal,
                 CopyParam, Params, OperationProgress);
    )
  }

  Result = true;

  switch (Answer)
  {
      // resume
    case qaRetry:
      OverwriteMode = omResume;
      assert(FileParams != NULL);
      assert(CanResume);
      FFileTransferResumed = FileParams->DestSize;
      break;

      // rename
    case qaIgnore:
      if (FTerminal->PromptUser(FTerminal->SessionData, pkFileName,
            LoadStr(RENAME_TITLE), L"", LoadStr(RENAME_PROMPT2), true, 0, FileName))
      {
        OverwriteMode = omOverwrite;
      }
      else
      {
        if (!OperationProgress->Cancel)
        {
          OperationProgress->Cancel = csCancel;
        }
        FFileTransferAbort = ftaCancel;
        Result = false;
      }
      break;

    case qaYes:
      OverwriteMode = omOverwrite;
      break;

    case qaNo:
      FFileTransferAbort = ftaSkip;
      Result = false;
      break;

    case qaCancel:
      if (!OperationProgress->Cancel)
      {
        OperationProgress->Cancel = csCancel;
      }
      FFileTransferAbort = ftaCancel;
      Result = false;
      break;

    default:
      assert(false);
      Result = false;
      break;
  }
  return Result;
}

//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CustomCommandOnFile(const UnicodeString FileName,
    const TRemoteFile * File, UnicodeString Command, int Params, TCaptureOutputEvent OutputEvent)
{
  assert(File);
  bool Dir = File->IsDirectory && !File->IsSymLink;
  if (Dir && (Params & ccRecursive))
  {
    TCustomCommandParams AParams;
    AParams.Command = Command;
    AParams.Params = Params;
    AParams.OutputEvent = OutputEvent;
    FTerminal->ProcessDirectory(FileName, FTerminal->CustomCommandOnFile, &AParams);
  }

  if (!Dir || (Params & ccApplyToDirectories))
  {
    TCustomCommandData Data(FTerminal);
    UnicodeString Cmd = TRemoteCustomCommand(
      Data, FTerminal->GetCurrentDirectory(), FileName, L"").
      Complete(Command, true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::AnyCommand(const UnicodeString Command,
    TCaptureOutputEvent OutputEvent)
{
}

//---------------------------------------------------------------------------
UnicodeString __fastcall TWebDAVFileSystem::FileUrl(const UnicodeString FileName)
{
  return FTerminal->FileUrl(FTerminal->SessionData->Ftps == ftpsNone ?
    L"http" : L"https", FileName);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TWebDAVFileSystem::GetFixedPaths()
{
  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::SpaceAvailable(const UnicodeString Path,
    TSpaceAvailable & /*ASpaceAvailable*/)
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CopyToRemote(TStrings * FilesToCopy,
    const UnicodeString ATargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    TOnceDoneOperation & OnceDoneOperation)
{
  assert((FilesToCopy != NULL) && (OperationProgress != NULL));

  Params &= ~cpAppend;
  UnicodeString FileName, FileNameOnly;
  UnicodeString TargetDir = AbsolutePath(ATargetDir, false);
  UnicodeString FullTargetDir = ::UnixIncludeTrailingBackslash(TargetDir);
  intptr_t Index = 0;
  while ((Index < FilesToCopy->Count) && !OperationProgress->Cancel)
  {
    bool Success = false;
    FileName = FilesToCopy->Strings[Index];
    FileNameOnly = ExtractFileName(FileName, false);

    try
    {
      try
      {
        if (FTerminal->SessionData->CacheDirectories)
        {
          FTerminal->DirectoryModified(TargetDir, false);

          if (::DirectoryExists(::ExtractFilePath(FileName)))
          {
            FTerminal->DirectoryModified(FullTargetDir + FileNameOnly, true);
          }
        }
        WebDAVSourceRobust(FileName, FullTargetDir, CopyParam, Params, OperationProgress,
          tfFirstLevel);
        Success = true;
      }
      catch (EScpSkipFile & E)
      {
        SUSPEND_OPERATION (
          if (!FTerminal->HandleException(&E))
          {
            throw;
          }
        );
      }
    }
    __finally
    {
      OperationProgress->Finish(FileName, Success, OnceDoneOperation);
    }
    Index++;
  }
}

//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::WebDAVSourceRobust(const UnicodeString FileName,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  bool Retry = false;

  TUploadSessionAction Action(FTerminal->ActionLog);

  do
  {
    Retry = false;
    try
    {
      WebDAVSource(FileName, TargetDir, CopyParam, Params, OperationProgress,
        Flags, Action);
    }
    catch (Exception & E)
    {
      Retry = true;
      if (FTerminal->GetActive() ||
          !FTerminal->QueryReopen(&E, ropNoReadDirectory, OperationProgress))
      {
        FTerminal->RollbackAction(Action, OperationProgress, &E);
        throw;
      }
    }

    if (Retry)
    {
      OperationProgress->RollbackTransfer();
      Action.Restart();
      // prevent overwrite confirmations
      // (should not be set for directories!)
      Params |= cpNoConfirmation;
      Flags |= tfAutoResume;
    }
  }
  while (Retry);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::WebDAVSource(const UnicodeString FileName,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags,
  TUploadSessionAction & Action)
{
  bool CheckExistence = UnixComparePaths(TargetDir, FTerminal->GetCurrentDirectory()) &&
    (FTerminal->FFiles != NULL) && FTerminal->FFiles->Loaded;
  bool CanProceed = false;
  UnicodeString FileNameOnly =
    CopyParam->ChangeFileName(ExtractFileName(FileName, false), osLocal, true);
  if (CheckExistence)
  {
    TRemoteFile * File = FTerminal->FFiles->FindFile(FileNameOnly);
    if (File != NULL)
    {
      unsigned int Answer = 0;
      if (File->IsDirectory)
      {
        UnicodeString Message = FMTLOAD(DIRECTORY_OVERWRITE, (FileNameOnly.c_str()));
        TQueryParams QueryParams(qpNeverAskAgainCheck);
        SUSPEND_OPERATION (
          Answer = FTerminal->ConfirmFileOverwrite(
            FileNameOnly /*not used*/, NULL,
            qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll,
            &QueryParams, osRemote, CopyParam, Params, OperationProgress, Message);
        );
        switch (Answer)
        {
          case qaYes:
            CanProceed = true;
            break;

          case qaCancel:
            OperationProgress->Cancel = csCancel; // continue on next case
            // FALLTHROUGH
          case qaNo:
            CanProceed = false;
            break;

          default:
            break;
        }
      }
      else
      {
        __int64 Size;
        __int64 MTime;
        TOverwriteFileParams FileParams;
        FTerminal->OpenLocalFile(FileName, GENERIC_READ,
          NULL, NULL, NULL, &MTime, NULL,
          &Size);
        FileParams.SourceSize = Size;
        FileParams.SourceTimestamp = UnixToDateTime(MTime,
          FTerminal->SessionData->DSTMode);
        FileParams.DestSize = File->Size;
        FileParams.DestTimestamp = File->Modification;

        TOverwriteMode OverwriteMode = omOverwrite;
        bool AutoResume = false;
        ConfirmOverwrite(FileNameOnly, OverwriteMode, OperationProgress,
            &FileParams, CopyParam, Params, AutoResume, Answer);
        switch (Answer)
        {
          case qaYes:
            CanProceed = true;
            break;

          case qaCancel:
            OperationProgress->Cancel = csCancel; // continue on next case
            // FALLTHROUGH
          case qaNo:
            CanProceed = false;
            break;

          default:
            break;
        }
      }
    }
    else
    {
      CanProceed = true;
    }
  }
  else
  {
    CanProceed = true;
  }
  if (CanProceed)
  {
    Action.FileName(ExpandUNCFileName(FileName));

    OperationProgress->SetFile(FileName, false);

    if (!FTerminal->AllowLocalFileTransfer(FileName, CopyParam))
    {
      FTerminal->LogEvent(FORMAT(L"File \"%s\" excluded from transfer", (FileName.c_str())));
      THROW_SKIP_FILE_NULL;
    }

    __int64 Size;
    int Attrs;

    FTerminal->OpenLocalFile(FileName, GENERIC_READ, &Attrs,
      NULL, NULL, NULL, NULL, &Size);

    OperationProgress->SetFileInProgress();

    bool Dir = FLAGSET(Attrs, faDirectory);
    if (Dir)
    {
      Action.Cancel();
      WebDAVDirectorySource(IncludeTrailingBackslash(FileName), TargetDir,
        Attrs, CopyParam, Params, OperationProgress, Flags);
    }
    else
    {
      UnicodeString DestFileName = CopyParam->ChangeFileName(ExtractFileName(FileName, false),
        osLocal, FLAGSET(Flags, tfFirstLevel));

      FTerminal->LogEvent(FORMAT(L"Copying \"%s\" to remote directory started.", (FileName.c_str())));

      OperationProgress->SetLocalSize(Size);

      // Suppose same data size to transfer as to read
      // (not true with ASCII transfer)
      OperationProgress->SetTransferSize(OperationProgress->LocalSize);
      OperationProgress->TransferingFile = false;

      // Will we use ASCII of BINARY file transfer?
      TFileMasks::TParams MaskParams;
      MaskParams.Size = Size;

      ResetFileTransfer();

      TFileTransferData UserData;

      {
        unsigned int TransferType = 2;
        // ignore file list
        TWebDAVFileListHelper Helper(this, NULL, true);

        FFileTransferCPSLimit = OperationProgress->CPSLimit;
        // not used for uploads anyway
        FFileTransferPreserveTime = CopyParam->PreserveTime;
        // not used for uploads, but we get new name (if any) back in this field
        UserData.FileName = DestFileName;
        UserData.Params = Params;
        UserData.AutoResume = FLAGSET(Flags, tfAutoResume);
        UserData.CopyParam = CopyParam;
        FileTransfer(FileName, FileName, DestFileName,
          TargetDir, false, Size, TransferType, UserData, OperationProgress);
      }

      UnicodeString DestFullName = TargetDir + UserData.FileName;
      // only now, we know the final destination
      Action.Destination(DestFullName);
    }

    // TODO : Delete also read-only files.
    if (FLAGSET(Params, cpDelete))
    {
      if (!Dir)
      {
        FILE_OPERATION_LOOP (FMTLOAD(DELETE_LOCAL_FILE_ERROR, (FileName.c_str())),
          THROWOSIFFALSE(::DeleteFile(FileName.c_str()));
        )
      }
    }
    else if (CopyParam->ClearArchive && FLAGSET(Attrs, faArchive))
    {
      FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (FileName.c_str())),
        THROWOSIFFALSE(FileSetAttr(FileName, Attrs & ~faArchive) == 0);
      )
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::WebDAVDirectorySource(const UnicodeString DirectoryName,
    const UnicodeString TargetDir, int Attrs, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  UnicodeString DestDirectoryName = CopyParam->ChangeFileName(
    ExtractFileName(ExcludeTrailingBackslash(DirectoryName), false), osLocal,
    FLAGSET(Flags, tfFirstLevel));
  UnicodeString DestFullName = UnixIncludeTrailingBackslash(TargetDir + DestDirectoryName);
  // create DestFullName if it does not exist
  int IsDir = 0;
  bool Exists = WebDAVCheckExisting(DestFullName.c_str(), IsDir);
  if (!Exists)
  {
      CreateDirectory(DestFullName);
  }

  OperationProgress->SetFile(DirectoryName);

  WIN32_FIND_DATA SearchRec;
  bool FindOK = false;
  HANDLE findHandle = 0;

  FILE_OPERATION_LOOP (FMTLOAD(LIST_DIR_ERROR, (DirectoryName.c_str())),
    UnicodeString path = DirectoryName + L"*.*";
    findHandle = FindFirstFile(path.c_str(), &SearchRec);
    FindOK = (findHandle != 0);
    if (!FindOK)
    {
      FindCheck(GetLastError());
    }
  );

  bool CreateDir = true;

  try
  {
    while (FindOK && !OperationProgress->Cancel)
    {
      UnicodeString FileName = DirectoryName + SearchRec.cFileName;
      try
      {
        if ((wcscmp(SearchRec.cFileName, THISDIRECTORY) != 0) && (wcscmp(SearchRec.cFileName, PARENTDIRECTORY) != 0))
        {
          WebDAVSourceRobust(FileName, DestFullName, CopyParam, Params, OperationProgress,
            Flags & ~(tfFirstLevel | tfAutoResume));
          // if any file got uploaded (i.e. there were any file in the
          // directory and at least one was not skipped),
          // do not try to create the directory,
          // as it should be already created by FZAPI during upload
          CreateDir = false;
        }
      }
      catch (EScpSkipFile & E)
      {
        // If ESkipFile occurs, just log it and continue with next file
        SUSPEND_OPERATION (
          // here a message to user was displayed, which was not appropriate
          // when user refused to overwrite the file in subdirectory.
          // hopefully it won't be missing in other situations.
          if (!FTerminal->HandleException(&E))
          {
            throw;
          }
        );
      }

      FILE_OPERATION_LOOP (FMTLOAD(LIST_DIR_ERROR, (DirectoryName.c_str())),
        FindOK = (::FindNextFile(findHandle, &SearchRec) != 0);
        if (!FindOK)
        {
          FindCheck(GetLastError());
        }
      );
    }
  }
  __finally
  {
    ::FindClose(findHandle);
  }

  if (CreateDir)
  {
    TRemoteProperties Properties;
    if (CopyParam->PreserveRights)
    {
      Properties.Valid = TValidProperties() << vpRights;
      Properties.Rights = CopyParam->RemoteFileRights(Attrs);
    }

    try
    {
      FTerminal->SetExceptionOnFail(true);
      try
      {
        FTerminal->CreateDirectory(DestFullName, &Properties);
      }
      __finally
      {
        FTerminal->SetExceptionOnFail(false);
      }
    }
    catch (...)
    {
      TRemoteFile * File = NULL;
      // ignore non-fatal error when the directory already exists
      UnicodeString fn = UnixExcludeTrailingBackslash(DestFullName);
      if (fn.IsEmpty())
      {
        fn = L"/";
      }
      bool Rethrow =
        !FTerminal->GetActive() ||
        !FTerminal->FileExists(fn, &File) ||
        (File && !File->IsDirectory);
      delete File;
      if (Rethrow)
      {
        throw;
      }
    }
  }

  // TODO : Delete also read-only directories.
  // TODO : Show error message on failure.
  if (!OperationProgress->Cancel)
  {
    if (FLAGSET(Params, cpDelete))
    {
      RemoveDir(DirectoryName);
    }
    else if (CopyParam->ClearArchive && FLAGSET(Attrs, faArchive))
    {
      FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (DirectoryName.c_str())),
        THROWOSIFFALSE(FileSetAttr(DirectoryName, Attrs & ~faArchive) == 0);
      )
    }
  }
}

//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CopyToLocal(TStrings * FilesToCopy,
    const UnicodeString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    TOnceDoneOperation & OnceDoneOperation)
{
  Params &= ~cpAppend;
  UnicodeString FullTargetDir = ::IncludeTrailingBackslash(TargetDir);

  int Index = 0;
  while (Index < FilesToCopy->Count && !OperationProgress->Cancel)
  {
    UnicodeString FileName = FilesToCopy->Strings[Index];
    const TRemoteFile * File = dynamic_cast<const TRemoteFile *>(FilesToCopy->Objects[Index]);
    bool Success = false;
    FTerminal->SetExceptionOnFail(true);
    try
    {
      try
      {
        SinkRobust(AbsolutePath(FileName, false), File, FullTargetDir, CopyParam, Params,
          OperationProgress, tfFirstLevel);
        Success = true;
      }
      catch (EScpSkipFile & E)
      {
        SUSPEND_OPERATION (
          if (!FTerminal->HandleException(&E))
          {
            throw;
          }
        );
      }
    }
    __finally
    {
      OperationProgress->Finish(FileName, Success, OnceDoneOperation);
      FTerminal->SetExceptionOnFail(false);
    }
    Index++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::SinkRobust(const UnicodeString FileName,
    const TRemoteFile * File, const UnicodeString TargetDir,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  // the same in TSFTPFileSystem
  bool Retry;

  TDownloadSessionAction Action(FTerminal->ActionLog);

  do
  {
    Retry = false;
    try
    {
      Sink(FileName, File, TargetDir, CopyParam, Params, OperationProgress,
        Flags, Action);
    }
    catch (Exception & E)
    {
      Retry = true;
      if (FTerminal->GetActive() ||
          !FTerminal->QueryReopen(&E, ropNoReadDirectory, OperationProgress))
      {
        FTerminal->RollbackAction(Action, OperationProgress, &E);
        throw;
      }
    }

    if (Retry)
    {
      OperationProgress->RollbackTransfer();
      Action.Restart();
      assert(File != NULL);
      if (!File->IsDirectory)
      {
        // prevent overwrite confirmations
        Params |= cpNoConfirmation;
        Flags |= tfAutoResume;
      }
    }
  }
  while (Retry);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::Sink(const UnicodeString FileName,
  const TRemoteFile * File, const UnicodeString TargetDir,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags,
  TDownloadSessionAction & Action)
{
  UnicodeString FileNameOnly = UnixExtractFileName(FileName);

  Action.FileName(FileName);

  assert(File);
  TFileMasks::TParams MaskParams;
  MaskParams.Size = File->Size;

  if (!CopyParam->AllowTransfer(FileName, osRemote, File->IsDirectory, MaskParams))
  {
    FTerminal->LogEvent(FORMAT(L"File \"%s\" excluded from transfer", (FileName.c_str())));
    THROW_SKIP_FILE_NULL;
  }

  FTerminal->LogFileDetails(FileName, TDateTime(), File->Size);

  OperationProgress->SetFile(FileNameOnly);

  UnicodeString DestFileName = CopyParam->ChangeFileName(FileNameOnly,
    osRemote, FLAGSET(Flags, tfFirstLevel));
  UnicodeString DestFullName = TargetDir + DestFileName;

  if (File->IsDirectory)
  {
    bool CanProceed = true;
    if (::DirectoryExists(DestFullName))
    {
      unsigned int Answer = 0;
      UnicodeString Message = FMTLOAD(DIRECTORY_OVERWRITE, (FileNameOnly.c_str()));
      TQueryParams QueryParams(qpNeverAskAgainCheck);
      SUSPEND_OPERATION (
        Answer = FTerminal->ConfirmFileOverwrite(
          FileNameOnly /*not used*/, NULL,
          qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll,
          &QueryParams, osRemote, CopyParam, Params, OperationProgress, Message);
      );
      switch (Answer)
      {
        case qaCancel:
          OperationProgress->Cancel = csCancel; // continue on next case
          // FALLTHROUGH
        case qaNo:
          CanProceed = false;
        default:
          break;
      }
    }
    if (CanProceed)
    {
      Action.Cancel();
      if (!File->IsSymLink)
      {
        FILE_OPERATION_LOOP (FMTLOAD(NOT_DIRECTORY_ERROR, (DestFullName.c_str())),
          int Attrs = FileGetAttr(DestFullName);
          if (FLAGCLEAR(Attrs, faDirectory)) { EXCEPTION; }
        );

        FILE_OPERATION_LOOP (FMTLOAD(CREATE_DIR_ERROR, (DestFullName.c_str())),
          THROWOSIFFALSE(ForceDirectories(DestFullName));
        );

        TSinkFileParams SinkFileParams;
        SinkFileParams.TargetDir = ::IncludeTrailingBackslash(DestFullName);
        SinkFileParams.CopyParam = CopyParam;
        SinkFileParams.Params = Params;
        SinkFileParams.OperationProgress = OperationProgress;
        SinkFileParams.Skipped = false;
        SinkFileParams.Flags = Flags & ~(tfFirstLevel | tfAutoResume);

        FTerminal->ProcessDirectory(FileName, SinkFile, &SinkFileParams);

        // Do not delete directory if some of its files were skip.
        // Throw "skip file" for the directory to avoid attempt to deletion
        // of any parent directory
        if (FLAGSET(Params, cpDelete) && SinkFileParams.Skipped)
        {
          THROW_SKIP_FILE_NULL;
        }
      }
      else
      {
        // file is symlink to directory, currently do nothing, but it should be
        // reported to user
      }
    }
  }
  else
  {
    FTerminal->LogEvent(FORMAT(L"Copying \"%s\" to local directory started.", (FileName.c_str())));
    bool CanProceed = true;
    if (FileExists(DestFullName))
    {
      __int64 Size;
      __int64 MTime;
      FTerminal->OpenLocalFile(DestFullName, GENERIC_READ, NULL,
        NULL, NULL, &MTime, NULL, &Size);
      TOverwriteFileParams FileParams;

      FileParams.SourceSize = File->Size;
      FileParams.SourceTimestamp = File->Modification;
      FileParams.DestSize = Size;
      FileParams.DestTimestamp = UnixToDateTime(MTime,
        FTerminal->SessionData->DSTMode);

      unsigned int Answer = 0;
      TOverwriteMode OverwriteMode = omOverwrite;
      bool AutoResume = false;
      ConfirmOverwrite(DestFullName, OverwriteMode, OperationProgress,
          &FileParams, CopyParam, Params, AutoResume, Answer);
      switch (Answer)
      {
        case qaCancel:
          OperationProgress->Cancel = csCancel; // continue on next case
          // FALLTHROUGH
        case qaNo:
          CanProceed = false;
        default:
          break;
      }
    }
    if (CanProceed)
    {
      // Suppose same data size to transfer as to write
      OperationProgress->SetTransferSize(File->Size);
      OperationProgress->SetLocalSize(OperationProgress->TransferSize);

      int Attrs = -1;
      FILE_OPERATION_LOOP (FMTLOAD(NOT_FILE_ERROR, (DestFullName.c_str())),
        Attrs = FileGetAttr(DestFullName);
        if ((Attrs >= 0) && FLAGSET(Attrs, faDirectory)) { EXCEPTION; }
      );

      OperationProgress->TransferingFile = false; // not set with FTP protocol

      ResetFileTransfer();

      TFileTransferData UserData;

      UnicodeString FilePath = ::UnixExtractFilePath(FileName);
      if (FilePath.IsEmpty())
      {
        FilePath = L"/";
      }

      {
        unsigned int TransferType = 2;
        // ignore file list
        TWebDAVFileListHelper Helper(this, NULL, true);

        FFileTransferCPSLimit = OperationProgress->CPSLimit;
        FFileTransferPreserveTime = CopyParam->PreserveTime;
        UserData.FileName = DestFileName;
        UserData.Params = Params;
        UserData.AutoResume = FLAGSET(Flags, tfAutoResume);
        UserData.CopyParam = CopyParam;
        FileTransfer(FileName, DestFullName, FileNameOnly,
          FilePath, true, File->Size, TransferType, UserData, OperationProgress);
      }

      // in case dest filename is changed from overwrite dialog
      if (DestFileName != UserData.FileName)
      {
        DestFullName = TargetDir + UserData.FileName;
        Attrs = FileGetAttr(DestFullName);
      }

      Action.Destination(ExpandUNCFileName(DestFullName));

      if (Attrs == -1)
      {
        Attrs = faArchive;
      }
      int NewAttrs = CopyParam->LocalFileAttrs(*File->Rights);
      if ((NewAttrs & Attrs) != NewAttrs)
      {
        FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (DestFullName.c_str())),
          THROWOSIFFALSE(FileSetAttr(DestFullName, Attrs | NewAttrs) == 0);
        );
      }
      // set time
      {
        FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (DestFullName.c_str())),
          HANDLE Handle;
          Handle = CreateFile(DestFullName.c_str(), GENERIC_WRITE,
                              FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, 0);
          FILETIME WrTime = DateTimeToFileTime(File->Modification,
                            FTerminal->SessionData->DSTMode);
          bool Result = SetFileTime(Handle, &WrTime, &WrTime, &WrTime) > 0;
          CloseHandle(Handle);
          if (!Result)
          {
            Abort();
          }
        );
      }
    }
  }

  if (FLAGSET(Params, cpDelete))
  {
    // If file is directory, do not delete it recursively, because it should be
    // empty already. If not, it should not be deleted (some files were
    // skipped or some new files were copied to it, while we were downloading)
    int Params = dfNoRecursive;
    FTerminal->DeleteFile(FileName, File, &Params);
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::SinkFile(const UnicodeString FileName,
    const TRemoteFile * File, void * Param)
{
  TSinkFileParams * Params = static_cast<TSinkFileParams *>(Param);
  assert(Params->OperationProgress);
  try
  {
    SinkRobust(FileName, File, Params->TargetDir, Params->CopyParam,
      Params->Params, Params->OperationProgress, Params->Flags);
  }
  catch (EScpSkipFile & E)
  {
    TFileOperationProgressType * OperationProgress = Params->OperationProgress;

    Params->Skipped = true;
    SUSPEND_OPERATION (
      if (!FTerminal->HandleException(&E))
      {
        throw;
      }
    );

    if (OperationProgress->Cancel)
    {
      Abort();
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::HandleListData(const wchar_t * Path,
    const TListDataEntry * Entries, unsigned int Count)
{
  if (!FActive)
  {
    return false;
  }
  else if (FIgnoreFileList)
  {
    // directory listing provided implicitly by FZAPI during certain operations is ignored
    assert(FFileList == NULL);
    return false;
  }
  else
  {
    assert(FFileList != NULL);
    // this can actually fail in real life,
    // when connected to server with case insensitive paths
    assert(UnixComparePaths(AbsolutePath(FFileList->Directory, false), Path));
    USEDPARAM(Path);

    for (size_t Index = 0; Index < Count; Index++)
    {
      const TListDataEntry * Entry = &Entries[Index];
      TRemoteFile * File = new TRemoteFile();
      try
      {
        File->Terminal = FTerminal;

        File->FileName = UnicodeString(Entry->Name);
        if (wcslen(Entry->Permissions) >= 10)
        {
          try
          {
            File->Rights->Text = Entry->Permissions + 1;
          }
          catch (...)
          {
            // ignore permissions errors with WebDAV
          }
        }
        // FIXME
        UnicodeString own = Entry->OwnerGroup;
        const wchar_t * Space = wcschr(own.c_str(), ' ');
        if (Space != NULL)
        {
          File->Owner.Name = UnicodeString(own.c_str(), Space - own.c_str());
          File->Group.Name = Space + 1;
        }
        else
        {
          File->Owner.Name = Entry->OwnerGroup;
        }

        File->Size = Entry->Size;

        if (Entry->Link)
        {
          File->Type = FILETYPE_SYMLINK;
        }
        else if (Entry->Dir)
        {
          File->Type = FILETYPE_DIRECTORY;
        }
        else
        {
          File->Type = L'-';
        }

        // ModificationFmt must be set after Modification
        if (Entry->Time.HasDate)
        {
          // should be the same as ConvertRemoteTimestamp
          TDateTime Modification =
            EncodeDateVerbose(static_cast<unsigned short>(Entry->Time.Year), static_cast<unsigned short>(Entry->Time.Month),
              static_cast<unsigned short>(Entry->Time.Day));
          if (Entry->Time.HasTime)
          {
            unsigned short seconds = 0;
            if (Entry->Time.HasSeconds)
              seconds = static_cast<unsigned short>(Entry->Time.Second);
            File->Modification = Modification +
              EncodeTimeVerbose(static_cast<unsigned short>(Entry->Time.Hour),
                static_cast<unsigned short>(Entry->Time.Minute),
                seconds, 0);
            // not exact as we got year as well, but it is most probably
            // guessed by FZAPI anyway
            File->ModificationFmt = mfMDHM;
          }
          else
          {
            File->Modification = Modification;
            File->ModificationFmt = mfMDY;
          }
        }
        else
        {
          // We estimate date to be today, if we have at least time
          File->Modification = TDateTime(0.0);
          File->ModificationFmt = mfNone;
        }
        File->LastAccess = File->Modification;

        File->LinkTo = Entry->LinkTarget;

        File->Complete();
      }
      catch (Exception & E)
      {
        delete File;
        UnicodeString EntryData =
          FORMAT(L"%s/%s/%s/%lld/%d/%d/%d/%d/%d/%d/%d/%d/%d",
                 (Entry->Name,
                 Entry->Permissions,
                 Entry->OwnerGroup,
                 Entry->Size,
                 int(Entry->Dir), int(Entry->Link), Entry->Time.Year, Entry->Time.Month, Entry->Time.Day,
                 Entry->Time.Hour, Entry->Time.Minute, int(Entry->Time.HasTime), int(Entry->Time.HasDate)));
        throw ETerminal(&E, FMTLOAD(LIST_LINE_ERROR, (EntryData.c_str())), HELP_LIST_LINE_ERROR);
      }

      FFileList->AddFile(File);
    }
    return true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ResetFileTransfer()
{
  FFileTransferAbort = ftaNone;
  FFileTransferCancelled = false;
  FFileTransferResumed = 0;
  webdav::cancelled = FALSE;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ReadDirectoryProgress(__int64 Bytes)
{
  // with WebDAV we do not know exactly how many entries we have received,
  // instead we know number of bytes received only.
  // so we report approximation based on average size of entry.
  size_t Progress = static_cast<size_t>(Bytes / 80);
  if (Progress - FLastReadDirectoryProgress >= 10)
  {
    bool Cancel = false;
    FLastReadDirectoryProgress = Progress;
    FTerminal->DoReadDirectoryProgress(Progress, Cancel);
    if (Cancel)
    {
      FTerminal->DoReadDirectoryProgress(-2, Cancel);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::DoFileTransferProgress(__int64 TransferSize,
    __int64 Bytes)
{
  TFileOperationProgressType * OperationProgress = FTerminal->OperationProgress;
  if (!OperationProgress) return;

  OperationProgress->SetTransferSize(TransferSize);

  if (FFileTransferResumed > 0)
  {
    OperationProgress->AddResumed(FFileTransferResumed);
    FFileTransferResumed = 0;
  }

  __int64 Diff = Bytes - OperationProgress->TransferedSize;
  if (Diff >= 0)
  {
    OperationProgress->AddTransfered(Diff);
  }

  if (OperationProgress->Cancel == csCancel)
  {
    FFileTransferCancelled = true;
    FFileTransferAbort = ftaCancel;
  }

  if (FFileTransferCPSLimit != OperationProgress->CPSLimit)
  {
    FFileTransferCPSLimit = OperationProgress->CPSLimit;
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::FileTransferProgress(__int64 TransferSize,
    __int64 Bytes)
{
  TGuard Guard(FTransferStatusCriticalSection);

  DoFileTransferProgress(TransferSize, Bytes);
}

//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::FileTransfer(const UnicodeString FileName,
    const UnicodeString LocalFile, const UnicodeString RemoteFile,
    const UnicodeString RemotePath, bool Get, __int64 Size, int Type,
    TFileTransferData & UserData, TFileOperationProgressType * OperationProgress)
{
  FCurrentOperationProgress = OperationProgress;
  FILE_OPERATION_LOOP (FMTLOAD(TRANSFER_ERROR, (FileName.c_str())),
    UnicodeString FullRemoteFileName = RemotePath + RemoteFile;
    bool Result = false;
    if (Get)
    {
      HANDLE LocalFileHandle = 0;
      FTerminal->CreateLocalFile(LocalFile,
        OperationProgress, &LocalFileHandle, true);
      Result = WebDAVGetFile(FullRemoteFileName.c_str(), &LocalFileHandle);
      if (!Result)
      {
        ::CloseHandle(LocalFileHandle);
      }
    }
    else
    {
      Result = WebDAVPutFile(FullRemoteFileName.c_str(), LocalFile.c_str(), Size);
    }
    if (!Result)
      EXCEPTION;
  );

  switch (FFileTransferAbort)
  {
    case ftaSkip:
      THROW_SKIP_FILE(NULL, L"");

    case ftaCancel:
      Abort();
      break;
  }

  if (!FFileTransferCancelled)
  {
    // show completion of transfer
    // call non-guarded variant to avoid deadlock with keepalives
    // (we are not waiting for reply anymore so keepalives are free to proceed)
    DoFileTransferProgress(OperationProgress->TransferSize, OperationProgress->TransferSize);
  }
}

bool TWebDAVFileSystem::SendPropFindRequest(const wchar_t * path, int & responseCode)
{
  assert(path);

  assert(FSession);
  apr_pool_t * pool = webdav_pool_create(webdav_pool);
  webdav::error_t err = 0;
  const char * remote_path = NULL;
  err = webdav::path_cstring_to_utf8(&remote_path, AnsiString(path).c_str(), pool);
  if (err) return false;
  err = webdav::client_send_propfind_request(
    FSession,
    remote_path,
    &responseCode,
          pool
        );

  webdav_pool_destroy(pool);
  return err == WEBDAV_NO_ERROR;
}

bool TWebDAVFileSystem::WebDAVCheckExisting(const wchar_t * path, int & is_dir)
{
  assert(path);
  is_dir = 0;
  assert(FSession);
  apr_pool_t * pool = webdav_pool_create(webdav_pool);
  webdav::error_t err = 0;
  webdav::node_kind_t kind = webdav::node_none;
  const char * remote_path = NULL;
  err = webdav::path_cstring_to_utf8(&remote_path, AnsiString(path).c_str(), pool);
  if (err) return false;
  err = webdav::client_check_path(
    FSession,
    remote_path,
    &kind,
          pool
        );

  if (kind != webdav::node_none)
    is_dir = kind == webdav::node_dir;
  webdav_pool_destroy(pool);
  return (err == WEBDAV_NO_ERROR) && (kind != webdav::node_none);
}

bool TWebDAVFileSystem::WebDAVMakeDirectory(const wchar_t * path)
{
  assert(path);

  assert(FSession);
  apr_pool_t * pool = webdav_pool_create(webdav_pool);
  webdav::error_t err = 0;
  const char * remote_path = NULL;
  err = webdav::path_cstring_to_utf8(&remote_path, AnsiString(path).c_str(), pool);
  if (err) return false;
  err = webdav::client_make_directory(
    FSession,
    remote_path,
    NULL,
          pool
        );
  webdav_pool_destroy(pool);
  return err == WEBDAV_NO_ERROR;
}

bool TWebDAVFileSystem::WebDAVGetList(const UnicodeString Directory)
{
  webdav::listdataentry_vector_t Entries;

  assert(FSession);
  webdav::list_func_baton_t baton = {0};
  baton.verbose = true;
  baton.entries = &Entries;
  baton.session = FSession;
  baton.pool = webdav_pool_create(webdav_pool);
  webdav::error_t err = 0;
  const char * remote_path = NULL;
  err = webdav::path_cstring_to_utf8(&remote_path, AnsiString(Directory).c_str(), baton.pool);
  if (err) return false;
  err = webdav::client_list2(
    FSession,
    remote_path,
    webdav::depth_immediates,
    WEBDAV_DIRENT_ALL,
    webdav::list_func,
    &baton,
          baton.pool
        );

  TListDataEntry * pEntries = !Entries.empty() ? &Entries[0] : NULL;
  HandleListData(Directory.c_str(), pEntries, Entries.size());
  webdav_pool_destroy(baton.pool);
  return err == WEBDAV_NO_ERROR;
}

bool TWebDAVFileSystem::WebDAVGetFile(const wchar_t * remotePath,
  HANDLE * LocalFileHandle)
{
  assert(remotePath && *remotePath);
  assert(LocalFileHandle);

  assert(FSession);
  apr_pool_t * pool = webdav_pool_create(webdav_pool);
  webdav::error_t err = 0;
  const char * remote_path = NULL;
  err = webdav::path_cstring_to_utf8(&remote_path, AnsiString(remotePath).c_str(), pool);
  if (err) return false;
  err = webdav::client_get_file(
    FSession,
    remote_path,
    LocalFileHandle,
    pool);

  webdav_pool_destroy(pool);
  return err == WEBDAV_NO_ERROR;
}

bool TWebDAVFileSystem::WebDAVPutFile(const wchar_t * remotePath, const wchar_t * localPath, const unsigned __int64 /*fileSize*/)
{
  assert(remotePath && *remotePath);
  assert(localPath && *localPath);

  assert(FSession);
  apr_pool_t * pool = webdav_pool_create(webdav_pool);
  webdav::error_t err = 0;
  const char * remote_path = NULL;
  const char * local_path = NULL;
  err = webdav::path_cstring_to_utf8(&remote_path, AnsiString(remotePath).c_str(), pool);
  if (err) return false;
  err = webdav::path_cstring_to_utf8(&local_path, AnsiString(localPath).c_str(), pool);
  if (err) return false;
  err = webdav::client_put_file(
    FSession,
    remote_path,
    local_path,
          pool
        );

  webdav_pool_destroy(pool);
  return err == WEBDAV_NO_ERROR;
}

bool TWebDAVFileSystem::WebDAVRenameFile(const wchar_t * srcPath, const wchar_t * dstPath)
{
  assert(srcPath && *srcPath);
  assert(dstPath && *dstPath);

  assert(FSession);
  apr_pool_t * pool = webdav_pool_create(webdav_pool);
  webdav::error_t err = 0;
  const char * src_path = NULL;
  const char * dst_path = NULL;
  err = webdav::path_cstring_to_utf8(&src_path, AnsiString(srcPath).c_str(), pool);
  if (err) return false;
  err = webdav::path_cstring_to_utf8(&dst_path, AnsiString(dstPath).c_str(), pool);
  if (err) return false;
  err = webdav::client_move_file_or_directory(
    FSession,
    src_path,
    dst_path,
    NULL,
          pool
        );

  webdav_pool_destroy(pool);
  return err == WEBDAV_NO_ERROR;
}

bool TWebDAVFileSystem::WebDAVDeleteFile(const wchar_t * path)
{
  assert(path);

  assert(FSession);
  apr_pool_t * pool = webdav_pool_create(webdav_pool);
  webdav::error_t err = 0;
  const char * remote_path = NULL;
  err = webdav::path_cstring_to_utf8(&remote_path, AnsiString(path).c_str(), pool);
  if (err) return false;
  err = webdav::client_delete_file(
    FSession,
    remote_path,
    NULL,
          pool
        );

  webdav_pool_destroy(pool);
  return err == WEBDAV_NO_ERROR;
}

webdav::error_t TWebDAVFileSystem::OpenURL(const UnicodeString & session_URL,
  apr_pool_t * pool)
{
  webdav::client_ctx_t * ctx = NULL;
  WEBDAV_ERR(client_create_context(&ctx, pool));

  const char * auth_username = NULL;
  const char * auth_password = NULL;
  WEBDAV_ERR(webdav::utf_cstring_to_utf8(&auth_username,
    AnsiString(FTerminal->SessionData->UserNameExpanded).c_str(), pool));
  WEBDAV_ERR(webdav::utf_cstring_to_utf8(&auth_password,
    AnsiString(FTerminal->SessionData->Password).c_str(), pool));
  webdav::auth_baton_t * ab = NULL;
  webdav::auth_baton_create(&ab, pool);
  webdav::auth_baton_init(ab,
    FALSE, // non_interactive
    auth_username,
    auth_password,
    FALSE, // no_auth_cache
    TRUE, // trust_server_cert
    this,
    webdav::check_cancel, ab,
    pool);
  ctx->auth_baton = ab;

  // Set up our cancellation support.
  ctx->cancel_func = webdav::check_cancel;
  ctx->cancel_baton = ab;

  ctx->progress_func = webdav::progress_func;
  ctx->progress_baton = ctx;

  webdav::session_t * session_p = NULL;
  const char * corrected_url = NULL;
  AnsiString base_url = AnsiString(session_URL).c_str();
  const char * base_url_encoded = webdav::path_uri_encode(base_url.c_str(), pool);
  WEBDAV_ERR(webdav::client_open_session_internal(
    &session_p,
    &corrected_url,
    base_url_encoded,
    ctx,
    pool));

  const char * url = NULL;
  if (corrected_url)
  {
    url = apr_pstrdup(pool, corrected_url);
  }
  else
  {
    url = apr_pstrdup(pool, base_url_encoded);
  }
  ne_uri * uri = NULL;
  if (WEBDAV_NO_ERROR == webdav::parse_ne_uri(&uri, url, pool))
  {
    FCurrentDirectory = uri->path;
    FHasTrailingSlash = (FCurrentDirectory.Length() > 0) && (FCurrentDirectory[FCurrentDirectory.Length()] == L'/');
  }
  FSession = session_p;
  return WEBDAV_NO_ERROR;
}

//---------------------------------------------------------------------------

webdav::error_t TWebDAVFileSystem::GetServerSettings(
  int * proxy_method,
  const char ** proxy_host,
  unsigned int * proxy_port,
  const char ** proxy_username,
  const char ** proxy_password,
  int * timeout_seconds,
  int * neon_debug,
  const char ** neon_debug_file_name,
  bool * compression,
  const char ** pk11_provider,
  const char ** ssl_authority_file,
  apr_pool_t * pool)
{
  // If we find nothing, default to nulls.
  *proxy_method = 0;
  *proxy_host = NULL;
  *proxy_port = (unsigned int)-1;
  *proxy_username = NULL;
  *proxy_password = NULL;
  *pk11_provider = NULL;
  *ssl_authority_file = NULL;

  TSessionData * Data = FTerminal->SessionData;
  TConfiguration * Configuration = FTerminal->Configuration;
  {
    TProxyMethod ProxyMethod = Data->ProxyMethod;
    *proxy_method = (int)ProxyMethod;
    if (ProxyMethod != (TProxyMethod)::pmNone)
    {
      WEBDAV_ERR(webdav::path_cstring_to_utf8(proxy_host, AnsiString(Data->ProxyHost).c_str(), pool));
      WEBDAV_ERR(webdav::path_cstring_to_utf8(proxy_username, AnsiString(Data->ProxyUsername).c_str(), pool));
      WEBDAV_ERR(webdav::path_cstring_to_utf8(proxy_password, AnsiString(Data->ProxyPassword).c_str(), pool));
    }
  }

  // Apply non-proxy-specific settings regardless of exceptions:
  if (compression)
    *compression = Data->Compression;

  int l_debug = Configuration->ActualLogProtocol >= 1 ? 1 : 0;
  *pk11_provider = "";

  *ssl_authority_file = apr_pstrdup(pool, AnsiString(Data->PublicKeyFile).c_str());

  {
    int l_proxy_port = Data->ProxyPort;
    if (l_proxy_port < 0)
    {
      return webdav::error_create(WEBDAV_ERR_ILLEGAL_URL, NULL,
        "Invalid URL: negative proxy port number");
    }
    if (l_proxy_port > 65535)
    {
      return webdav::error_create(WEBDAV_ERR_ILLEGAL_URL, NULL,
        "Invalid URL: proxy port number greater "
        "than maximum TCP port number 65535");
    }
    *proxy_port = l_proxy_port;
  }

  {
    int l_timeout = Data->Timeout;
    if (l_timeout < 0)
      return webdav::error_create(WEBDAV_ERR_BAD_CONFIG_VALUE, NULL,
        "Invalid config: negative timeout value");
    *timeout_seconds = l_timeout;
  }

  if (l_debug)
  {
    *neon_debug = l_debug;
    if (Configuration->LogToFile)
    {
      WEBDAV_ERR(webdav::path_cstring_to_utf8(neon_debug_file_name,
        AnsiString(GetExpandedLogFileName(
          Configuration->LogFileName,
          Data)).c_str(), pool));
    }
    else
    {
      *neon_debug_file_name = NULL;
    }
  }
  else
  {
    *neon_debug = 0;
    *neon_debug_file_name = NULL;
  }

  return WEBDAV_NO_ERROR;
}

webdav::error_t TWebDAVFileSystem::VerifyCertificate(
  const char * Prompt, const char * fingerprint,
  unsigned int & RequestResult)
{
  RequestResult = 0;
  TClipboardHandler ClipboardHandler;
  ClipboardHandler.Text = fingerprint;

  TQueryButtonAlias Aliases[1];
  Aliases[0].Button = qaRetry;
  Aliases[0].Alias = LoadStr(COPY_KEY_BUTTON);
  Aliases[0].OnClick = &ClipboardHandler.Copy;

  TQueryParams Params;
  Params.HelpKeyword = HELP_VERIFY_CERTIFICATE;
  Params.NoBatchAnswers = qaYes | qaRetry;
  Params.Aliases = Aliases;
  Params.AliasesCount = LENOF(Aliases);
  unsigned int Answer = FTerminal->QueryUser(
    FMTLOAD(VERIFY_CERT_PROMPT3, (UnicodeString(Prompt).c_str())),
    NULL, qaYes | qaNo | qaCancel | qaRetry, &Params, qtWarning);
  RequestResult = Answer;
  switch (RequestResult)
  {
    case qaCancel:
      FFileTransferCancelled = true;
      FFileTransferAbort = ftaCancel;
      break;
  }
  return WEBDAV_NO_ERROR;
}

webdav::error_t TWebDAVFileSystem::AskForClientCertificateFilename(
  const char ** cert_file, unsigned int & RequestResult,
  apr_pool_t * pool)
{
  RequestResult = 0;
  TSessionData * Data = FTerminal->SessionData;
  UnicodeString FileName;
  if (!FTerminal->PromptUser(Data, pkFileName, LoadStr(CERT_FILENAME_PROMPT_TITLE), L"",
    LoadStr(CERT_FILENAME_PROMPT), true, 0, FileName))
  {
    FFileTransferCancelled = true;
    FFileTransferAbort = ftaCancel;
    return WEBDAV_ERR_CANCELLED;
  }
  WEBDAV_ERR(webdav::path_cstring_to_utf8(cert_file, AnsiString(FileName).c_str(), pool));
  RequestResult = qaOK;
  return WEBDAV_NO_ERROR;
}

webdav::error_t TWebDAVFileSystem::AskForUsername(
  const char ** user_name, unsigned int & RequestResult,
  apr_pool_t * pool)
{
  RequestResult = 0;
  TSessionData * Data = FTerminal->SessionData;
  UnicodeString UserName = Data->UserNameExpanded;
  if (!FTerminal->PromptUser(Data, pkUserName, LoadStr(USERNAME_TITLE), L"",
    LoadStr(USERNAME_PROMPT2), true, 0, UserName))
  {
    FFileTransferCancelled = true;
    FFileTransferAbort = ftaCancel;
    return WEBDAV_ERR_CANCELLED;
  }
  WEBDAV_ERR(webdav::path_cstring_to_utf8(user_name, AnsiString(UserName).c_str(), pool));
  RequestResult = qaOK;
  return WEBDAV_NO_ERROR;
}

webdav::error_t TWebDAVFileSystem::AskForUserPassword(
  const char ** password,
  unsigned int & RequestResult,
  apr_pool_t * pool)
{
  RequestResult = 0;
  TSessionData * Data = FTerminal->SessionData;
  UnicodeString Password = Data->Password;
  if (!FTerminal->PromptUser(Data, pkPassword, LoadStr(PASSWORD_TITLE), L"",
    LoadStr(PASSWORD_PROMPT), false, 0, Password))
  {
    FFileTransferCancelled = true;
    FFileTransferAbort = ftaCancel;
    return WEBDAV_ERR_CANCELLED;
  }
  WEBDAV_ERR(webdav::path_cstring_to_utf8(password, AnsiString(Password).c_str(), pool));
  RequestResult = qaOK;
  return WEBDAV_NO_ERROR;
}

//------------------------------------------------------------------------------
webdav::error_t TWebDAVFileSystem::AskForPassphrase(
  const char ** passphrase,
  const char * realm,
  unsigned int & RequestResult,
  apr_pool_t * pool)
{
  RequestResult = 0;
  TSessionData * Data = FTerminal->SessionData;
  UnicodeString Passphrase = Data->UserNameExpanded;
  UnicodeString Prompt = FORMAT(LoadStr(PROMPT_KEY_PASSPHRASE), (UnicodeString(realm)));
  if (!FTerminal->PromptUser(Data, pkPassphrase, LoadStr(PASSPHRASE_TITLE), L"",
    Prompt, false, 0, Passphrase))
  {
    FFileTransferCancelled = true;
    FFileTransferAbort = ftaCancel;
    return WEBDAV_ERR_CANCELLED;
  }
  WEBDAV_ERR(webdav::path_cstring_to_utf8(passphrase, AnsiString(Passphrase).c_str(), pool));
  RequestResult = qaOK;
  return WEBDAV_NO_ERROR;
}

webdav::error_t TWebDAVFileSystem::SimplePrompt(
  const char * prompt_text,
  const char * prompt_string,
  unsigned int & RequestResult)
{
  RequestResult = 0;
  TStrings * MoreMessages = new TStringList();
  try
  {
    MoreMessages->Add(UnicodeString(prompt_string));
    unsigned int Answer = FTerminal->QueryUser(
      UnicodeString(prompt_text),
      MoreMessages, qaYes | qaNo | qaCancel, NULL, qtConfirmation);
    RequestResult = Answer;
  }
  __finally
  {
    delete MoreMessages;
  }
  return RequestResult == qaCancel ? WEBDAV_ERR_CANCELLED : WEBDAV_NO_ERROR;
}

webdav::error_t TWebDAVFileSystem::CreateStorage(
  THierarchicalStorage *& Storage)
{
  Storage =
    FTerminal->Configuration->CreateScpStorage(false);
  return WEBDAV_NO_ERROR;
}

unsigned long TWebDAVFileSystem::AdjustToCPSLimit(unsigned long len)
{
  return FCurrentOperationProgress ? FCurrentOperationProgress->AdjustToCPSLimit(len) : len;
}

bool TWebDAVFileSystem::GetIsCancelled()
{
  TFileOperationProgressType * OperationProgress = FCurrentOperationProgress;
  return (OperationProgress && OperationProgress->Cancel == csCancel);
}
//------------------------------------------------------------------------------
