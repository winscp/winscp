/* 
   HTTP Authentication routines
   Copyright (C) 1999-2024, Joe Orton <joe@manyfish.co.uk>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA

*/

#include "config.h"

#include <sys/types.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for getpid() */
#endif

#ifdef WIN32
#include <windows.h> /* for GetCurrentThreadId() etc */
#endif

#ifdef HAVE_OPENSSL
#include <openssl/rand.h>
#elif defined(HAVE_GNUTLS)
#include <gnutls/gnutls.h>
#if LIBGNUTLS_VERSION_NUMBER < 0x020b00
#include <gcrypt.h>
#else
#include <gnutls/crypto.h>
#endif
#endif

#include <errno.h>
#include <time.h>

#include "ne_dates.h"
#include "ne_request.h"
#include "ne_auth.h"
#include "ne_string.h"
#include "ne_utils.h"
#include "ne_alloc.h"
#include "ne_uri.h"
#include "ne_internal.h"

#ifdef HAVE_GSSAPI
#ifdef HAVE_GSSAPI_GSSAPI_H
#include <gssapi/gssapi.h>
#ifdef HAVE_GSSAPI_GSSAPI_GENERIC_H
#include <gssapi/gssapi_generic.h>
#endif
#else
#include <gssapi.h>
#endif
#endif

#ifdef HAVE_SSPI
#include "ne_sspi.h"
#endif

#ifdef HAVE_NTLM
#include "ne_ntlm.h"
#endif
 
#define HOOK_SERVER_ID "http://webdav.org/neon/hooks/server-auth"
#define HOOK_PROXY_ID "http://webdav.org/neon/hooks/proxy-auth"

static const struct hashalg {
    const char *name;
    unsigned int hash;
    unsigned int sess; /* _session variant */
} hashalgs[] = {
    { "MD5", NE_HASH_MD5, 0 }, /* This must remain first in the array. */
    { "MD5-sess", NE_HASH_MD5, 1 },
    { "SHA-256", NE_HASH_SHA256, 0 },
    { "SHA-256-sess", NE_HASH_SHA256, 1 },
    { "SHA-512-256", NE_HASH_SHA512_256, 0 },
    { "SHA-512-256-sess", NE_HASH_SHA512_256, 1 }
};

#define HASHALG_MD5 (&hashalgs[0])
#define NUM_HASHALGS (sizeof(hashalgs)/sizeof(hashalgs[0]))

/* Selected method of qop which the client is using */
typedef enum {
    auth_qop_none,
    auth_qop_auth
} auth_qop;

/* A callback/userdata pair registered by the application for
 * a particular set of protocols. */
struct auth_handler {
    unsigned protomask; 

    ne_auth_creds old_creds;
    ne_auth_provide new_creds;
    void *userdata;
    int attempt; /* number of invocations of this callback for
                  * current request. */
    
    struct auth_handler *next;
};

/* A challenge */
struct auth_challenge {
    const struct auth_protocol *protocol;
    struct auth_handler *handler;
    const char *realm, *nonce, *opaque, *domain;
    unsigned int stale; /* if stale=true */
    unsigned int got_qop; /* we were given a qop directive */
    unsigned int qop_auth; /* "auth" token in qop attrib */
    enum { userhash_none=0, userhash_true=1, userhash_false=2} userhash;
    const struct hashalg *alg;
    struct auth_challenge *next;
};

static const struct auth_class {
    const char *id, *req_hdr, *resp_hdr, *resp_info_hdr;
    int status_code; /* Response status-code to trap. */
    int fail_code;   /* NE_* request to fail with. */
    unsigned protomask; /* protocol mask */
    const char *error_noauth; /* Error message template use when
                               * giving up authentication attempts. */
} ah_server_class = {
    HOOK_SERVER_ID,
    "Authorization", "WWW-Authenticate", "Authentication-Info",
    401, NE_AUTH, 0,
    N_("Could not authenticate to server: %s")
}, ah_proxy_class = {
    HOOK_PROXY_ID,
    "Proxy-Authorization", "Proxy-Authenticate", "Proxy-Authentication-Info",
    407, NE_PROXYAUTH, NE_AUTH_PROXY,
    N_("Could not authenticate to proxy server: %s")
};

/* Internal buffer size, which must be >= NE_ABUFSIZ. */
#define ABUFSIZE (NE_ABUFSIZ * 2)

#define zero_and_free(s) do { ne__strzero(s, strlen(s)); ne_free(s); } while (0)

/* Authentication session state. */
typedef struct {
    ne_session *sess;

    /* Which context will auth challenges be accepted? */
    enum {
        AUTH_ANY, /* ignore nothing. */
        AUTH_CONNECT, /* only in response to a CONNECT request. */
        AUTH_NOTCONNECT /* only in non-CONNECT responsees */
    } context;
    
    /* Protocol type for server/proxy auth. */
    const struct auth_class *spec;
    
    /* The protocol used for this authentication session */
    const struct auth_protocol *protocol;

    struct auth_handler *handlers;

    /*** Session details ***/

    /* The username and password we are using to authenticate with */
    char username[ABUFSIZE];

    /* This used for Basic auth */
    char *basic; 
#ifdef HAVE_GSSAPI
    /* for the GSSAPI/Negotiate scheme: */
    char *gssapi_token;
    gss_ctx_id_t gssctx;
    gss_name_t gssname;
    gss_OID gssmech;
#endif
#ifdef HAVE_SSPI
    /* This is used for SSPI (Negotiate/NTLM) auth */
    char *sspi_token;
    void *sspi_context;
    char *sspi_host;
#endif
#ifdef HAVE_NTLM
     /* This is used for NTLM auth */
     ne_ntlm_context *ntlm_context;
#endif
    /* These all used for Digest auth */
    char *realm;
    char *nonce;
    char *cnonce;
    char *opaque;
    char **domains; /* list of paths given as domain. */
    size_t ndomains; /* size of domains array */
    char *userhash;
    char *username_star;
    auth_qop qop;
    const struct hashalg *alg;
    unsigned int nonce_count;
    /* The hex representation of the H(A1) value */
    char *h_a1;
    /* Part of the RHS of the response digest. */
    char *response_rhs;
#ifdef WINSCP
    char * passport_auth_header;
    char * passport_cookies_header;
    /* In the current implementation, we actually possibly never reuse these two fields */
    ne_uri passport_login_uri;
    char * passport_cookies;
#endif
} auth_session;

struct auth_request {
    /*** Per-request details. ***/
    ne_request *request; /* the request object. */

    /* The request-target and method for the current request, */
    const char *target;
    const char *method;
    
    int attempt; /* number of times this request has been retries due
                  * to auth challenges. */
};

/* Used if this protocol takes an unquoted non-name/value-pair
 * parameter in the challenge. */
#define AUTH_FLAG_OPAQUE_PARAM (0x0001)
/* Used if this Authentication-Info may be sent for non-40[17]
 * response for this protocol. */
#define AUTH_FLAG_VERIFY_NON40x (0x0002)
/* Used for broken the connection-based auth schemes. */
#define AUTH_FLAG_CONN_AUTH (0x0004)
#ifdef WINSCP
#define AUTH_FLAG_FULL_HEADER (0x0008)
#endif

struct auth_protocol {
    unsigned id; /* public NE_AUTH_* id. */

    int strength; /* protocol strength for sort order. */

    const char *name; /* protocol name. */
    
    /* Parse the authentication challenge; returns zero on success, or
     * non-zero if this challenge be handled.  'attempt' is the number
     * of times the request has been resent due to auth challenges.
     * On failure, challenge_error() should be used to append an error
     * message to the error buffer 'errmsg'. */
    int (*challenge)(auth_session *sess, int attempt,
                     struct auth_challenge *chall,
                     const char *target, ne_buffer **errmsg
#ifdef WINSCP
                     , struct auth_request* areq
#endif
                    );

    /* Return the string to send in the -Authenticate request header:
     * (ne_malloc-allocated, NUL-terminated string) */
    char *(*response)(auth_session *sess, struct auth_request *req);
    
    /* Parse a Authentication-Info response; returns NE_* error code
     * on failure; on failure, the session error string must be
     * set. */
    int (*verify)(struct auth_request *req, auth_session *sess,
                  const char *value);
    
    int flags; /* AUTH_FLAG_* flags */
};

/* Helper function to append an error to the buffer during challenge
 * handling.  Pass printf-style string.  *errmsg may be NULL and is
 * allocated if necessary.  errmsg must be non-NULL. */
static void challenge_error(ne_buffer **errmsg, const char *fmt, ...)
    ne_attribute((format(printf, 2, 3)));

static int inside_domain(auth_session *sess, const char *req_uri);

/* Free the domains array, precondition sess->ndomains > 0. */
static void free_domains(auth_session *sess)
{
    do {
        ne_free(sess->domains[sess->ndomains - 1]);
    } while (--sess->ndomains);
    ne_free(sess->domains);
    sess->domains = NULL;
}

static void clean_session(auth_session *sess) 
{
    if (sess->basic) zero_and_free(sess->basic);
    if (sess->nonce) ne_free(sess->nonce);
    if (sess->cnonce) ne_free(sess->cnonce);
    if (sess->opaque) ne_free(sess->opaque);
    if (sess->realm) ne_free(sess->realm);
    if (sess->userhash) ne_free(sess->userhash);
    if (sess->username_star) ne_free(sess->username_star);
    if (sess->response_rhs) ne_free(sess->response_rhs);
    if (sess->h_a1) zero_and_free(sess->h_a1);
    sess->realm = sess->basic = sess->cnonce = sess->nonce =
        sess->opaque = sess->userhash = sess->response_rhs =
        sess->h_a1 = sess->username_star = NULL;
    if (sess->ndomains) free_domains(sess);
#ifdef HAVE_GSSAPI
    {
        unsigned int major;

        if (sess->gssctx != GSS_C_NO_CONTEXT)
            gss_delete_sec_context(&major, &sess->gssctx, GSS_C_NO_BUFFER);
        
    }
    if (sess->gssapi_token) ne_free(sess->gssapi_token);
    sess->gssapi_token = NULL;
#endif
#ifdef HAVE_SSPI
    if (sess->sspi_token) ne_free(sess->sspi_token);
    sess->sspi_token = NULL;
    ne_sspi_destroy_context(sess->sspi_context);
    sess->sspi_context = NULL;
#endif
#ifdef HAVE_NTLM
    if (sess->ntlm_context) {
        ne__ntlm_destroy_context(sess->ntlm_context);
        sess->ntlm_context = NULL;
    }
#endif
#ifdef WINSCP
    ne_uri_free(&sess->passport_login_uri);
    if (sess->passport_auth_header)
    {
        ne_free(sess->passport_auth_header);
        sess->passport_auth_header = NULL;
    }
    if (sess->passport_cookies_header)
    {
        ne_free(sess->passport_cookies_header);
        sess->passport_cookies_header = NULL;
    }
    if (sess->passport_cookies)
    {
        ne_free(sess->passport_cookies);
        sess->passport_cookies = NULL;
    }
#endif

    sess->protocol = NULL;
}

/* Returns client nonce string using given hash algorithm. Returns
 * NULL on error, in which case challenge_error(errmsg) is called. */
static char *get_cnonce(const struct hashalg *alg, ne_buffer **errmsg)
{
#ifdef NE_HAVE_SSL
    unsigned char data[32];

#ifdef HAVE_GNUTLS
#if LIBGNUTLS_VERSION_NUMBER < 0x020b00
    gcry_create_nonce(data, sizeof data);
#else
    gnutls_rnd(GNUTLS_RND_NONCE, data, sizeof data);
#endif
    return ne_base64(data, sizeof data);

#else /* !HAVE_GNUTLS */
    if (RAND_status() == 1 && RAND_bytes(data, sizeof data) >= 0) {
        return ne_base64(data, sizeof data);
    } 
    else {
        challenge_error(errmsg,
                        _("cannot create client nonce for Digest challenge, "
                          "OpenSSL PRNG not seeded"));
        return NULL;
    }
#endif /* HAVE_GNUTLS */

#else /* !NE_HAVE_SSL */
    /* Fallback sources of random data: all bad, but no good sources
     * are available. */
    ne_buffer *buf = ne_buffer_create();
    char *ret;

#ifdef HAVE_GETTIMEOFDAY
    struct timeval tv;
    if (gettimeofday(&tv, NULL) == 0)
        ne_buffer_snprintf(buf, 64, "%" NE_FMT_TIME_T ".%ld",
                           tv.tv_sec, (long)tv.tv_usec);
#else /* !HAVE_GETTIMEOFDAY */
    ne_buffer_snprintf(buf, 64, "%" NE_FMT_TIME_T, time(NULL));
#endif

    {
#ifdef WIN32
        DWORD pid = GetCurrentThreadId();
#else
        pid_t pid = getpid();
#endif
        ne_buffer_snprintf(buf, 32, "%lu", (unsigned long) pid);
    }

    ret = ne_strhash(alg->hash, buf->data, NULL);
    if (!ret)
        challenge_error(errmsg, _("%s hash failed for Digest challenge"),
                        alg->name);

    ne_buffer_destroy(buf);
    return ret;
#endif
}

/* Callback to retrieve user credentials for given session on given
 * attempt (pre request) for given challenge.  Password is written to
 * pwbuf (of size ABUFSIZE).  On error, challenge_error() is used
 * with errmsg. */
static int get_credentials(auth_session *sess, ne_buffer **errmsg, int attempt,
                           struct auth_challenge *chall, char *pwbuf)
{
    char *realm = sess->realm ? ne_strclean(ne_strdup(sess->realm)) : "";
    unsigned mask = chall->protocol->id | sess->spec->protomask;
    int rv;

    if (chall->handler->new_creds)
        rv = chall->handler->new_creds(chall->handler->userdata,
                                       attempt, mask, realm,
                                       sess->username, pwbuf,
                                       ABUFSIZE);
    else
        rv = chall->handler->old_creds(chall->handler->userdata, realm,
                                       chall->handler->attempt++, sess->username, pwbuf);

    if (sess->realm) ne_free(realm);

    if (rv == 0)
        return 0;

    challenge_error(errmsg, _("rejected %s challenge"),
                    chall->protocol->name);
    return -1;
}

/* Return the scope of the Basic authentication domain following rule
 * in RFC 7617.  Malloc-allocated path is returned. */
static char *get_scope_path(const char *uri)
{
    ne_uri base, udot, parent;
    char *s;

    memset(&udot, 0, sizeof udot);
    udot.path = ".";

    if (ne_uri_parse(uri, &base) != 0) {
        /* Assume scope is whole origin. */
        return ne_strdup("/");
    }

    ne_uri_resolve(&base, &udot, &parent);

    s = parent.path;
    parent.path = NULL;

    ne_uri_free(&parent);
    ne_uri_free(&base);

    return s;
}

/* Examine a Basic auth challenge.
 * Returns 0 if an valid challenge, else non-zero. */
static int basic_challenge(auth_session *sess, int attempt,
                           struct auth_challenge *parms,
                           const char *target, ne_buffer **errmsg
#ifdef WINSCP
                           , struct auth_request* areq
#endif
                           )
{
    char *tmp, password[ABUFSIZE];

    /* Verify challenge... must have a realm */
    if (parms->realm == NULL) {
        challenge_error(errmsg, _("missing realm in Basic challenge"));
	return -1;
    }

    clean_session(sess);
    
    sess->realm = ne_strdup(parms->realm);

    if (get_credentials(sess, errmsg, attempt, parms, password)) {
	/* Failed to get credentials */
	return -1;
    }

    if (strchr(sess->username, ':') != NULL) {
        challenge_error(errmsg, _("cannot handle Basic challenge "
                                  "for username containing colon"));
        return -1;
    }

    tmp = ne_concat(sess->username, ":", password, NULL);
    sess->basic = ne_base64((unsigned char *)tmp, strlen(tmp));
    zero_and_free(tmp);

    ne__strzero(password, sizeof password);

    if (strcmp(target, "*") == 0 || sess->context == AUTH_CONNECT) {
        /* For CONNECT, or if the request-target is "*", the auth
         * scope is implicitly the whole server. */
        return 0;
    }

    sess->domains = ne_malloc(sizeof *sess->domains);
    sess->domains[0] = get_scope_path(target);
    sess->ndomains = 1;

    NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Basic auth scope is: %s\n",
             sess->domains[0]);

    return 0;
}

/* Add Basic authentication credentials to a request */
static char *request_basic(auth_session *sess, struct auth_request *req) 
{
    if (sess->ndomains && !inside_domain(sess, req->target)) {
        return NULL;
    }

    return ne_concat("Basic ", sess->basic, "\r\n", NULL);
}

#ifdef HAVE_GSSAPI
/* Add GSSAPI authentication credentials to a request */
static char *request_negotiate(auth_session *sess, struct auth_request *req)
{
    if (sess->gssapi_token) 
        return ne_concat("Negotiate ", sess->gssapi_token, "\r\n", NULL);
    else
        return NULL;
}

/* Create an GSSAPI name for server HOSTNAME; returns non-zero on
 * error. */
static void get_gss_name(gss_name_t *server, const char *hostname)
{
    unsigned int major, minor;
    gss_buffer_desc token;

    token.value = ne_concat("HTTP@", hostname, NULL);
    token.length = strlen(token.value);

    major = gss_import_name(&minor, &token, GSS_C_NT_HOSTBASED_SERVICE,
                            server);
    ne_free(token.value);
    
    if (GSS_ERROR(major)) {
        NE_DEBUG(NE_DBG_HTTPAUTH, "gssapi: gss_import_name failed.\n");
        *server = GSS_C_NO_NAME;
    }
}

/* Append GSSAPI error(s) for STATUS of type TYPE to BUF; prepending
 * ": " to each error if *FLAG is non-zero, setting *FLAG after an
 * error has been appended. */
static void make_gss_error(ne_buffer *buf, int *flag,
                           unsigned int status, int type)
{
    unsigned int major, minor;
    unsigned int context = 0;
    
    do {
        gss_buffer_desc msg;
        major = gss_display_status(&minor, status, type,
                                   GSS_C_NO_OID, &context, &msg);
        if (major == GSS_S_COMPLETE && msg.length) {
            if ((*flag)++) ne_buffer_append(buf, ": ", 2);
            ne_buffer_append(buf, msg.value, msg.length);
        }
        if (msg.length) gss_release_buffer(&minor, &msg);
    } while (context);
}

/* Continue a GSS-API Negotiate exchange, using input TOKEN if
 * non-NULL.  Returns non-zero on error, in which case *errmsg is
 * guaranteed to be non-NULL (i.e. an error message is set). */
static int continue_negotiate(auth_session *sess, const char *token,
                              ne_buffer **errmsg)
{
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    unsigned int major, minor;
    gss_buffer_desc input = GSS_C_EMPTY_BUFFER;
    gss_buffer_desc output = GSS_C_EMPTY_BUFFER;
    unsigned char *bintoken = NULL;
    int ret;

    if (token) {
        input.length = ne_unbase64(token, &bintoken);
        if (input.length == 0) {
            challenge_error(errmsg, _("invalid Negotiate token"));
            return -1;
        }
        input.value = bintoken;
        NE_DEBUG(NE_DBG_HTTPAUTH, "gssapi: Continuation token [%s]\n", token);
    }
    else if (sess->gssctx != GSS_C_NO_CONTEXT) {
        NE_DEBUG(NE_DBG_HTTPAUTH, "gssapi: Reset incomplete context.\n");
        gss_delete_sec_context(&minor, &sess->gssctx, GSS_C_NO_BUFFER);
    }

    major = gss_init_sec_context(&minor, GSS_C_NO_CREDENTIAL, &sess->gssctx,
                                 sess->gssname, sess->gssmech, 
                                 GSS_C_MUTUAL_FLAG, GSS_C_INDEFINITE, 
                                 GSS_C_NO_CHANNEL_BINDINGS,
                                 &input, &sess->gssmech, &output, NULL, NULL);

    /* done with the input token. */
    if (bintoken) ne_free(bintoken);

    if (GSS_ERROR(major)) {
        int flag = 0;

        challenge_error(errmsg, _("GSSAPI authentication error: "));
        make_gss_error(*errmsg, &flag, major, GSS_C_GSS_CODE);
        make_gss_error(*errmsg, &flag, minor, GSS_C_MECH_CODE);

        return -1;
    }

    if (major == GSS_S_CONTINUE_NEEDED || major == GSS_S_COMPLETE) {
        NE_DEBUG(NE_DBG_HTTPAUTH, "gssapi: init_sec_context OK. (major=%d)\n",
                 major);
        ret = 0;
    } 
    else {
        challenge_error(errmsg, _("GSSAPI failure (code %u)"), major);
        ret = -1;
    }

    if (major != GSS_S_CONTINUE_NEEDED) {
        /* context no longer needed: destroy it */
        gss_delete_sec_context(&minor, &sess->gssctx, GSS_C_NO_BUFFER);
    }

    if (output.length) {
        sess->gssapi_token = ne_base64(output.value, output.length);
        NE_DEBUG(NE_DBG_HTTPAUTH, "gssapi: Output token: [%s]\n", 
                 sess->gssapi_token);
        gss_release_buffer(&minor, &output);
    } else {
        NE_DEBUG(NE_DBG_HTTPAUTH, "gssapi: No output token.\n");
    }

    return ret;
}

/* Process a Negotiate challenge CHALL in session SESS; returns zero
 * if challenge is accepted. */
static int negotiate_challenge(auth_session *sess, int attempt,
                               struct auth_challenge *chall,
                               const char *target, ne_buffer **errmsg
#ifdef WINSCP
                               , struct auth_request* areq
#endif
                               )
{
    const char *token = chall->opaque;

    /* Respect an initial challenge - which must have no input token,
     * or a continuation - which must have an input token. */
    if (attempt == 0 || token) {
        return continue_negotiate(sess, token, errmsg);
    }
    else {
        challenge_error(errmsg, _("ignoring empty Negotiate continuation"));
        return -1;
    }
}

/* Verify the header HDR in a Negotiate response. */
static int verify_negotiate_response(struct auth_request *req, auth_session *sess,
                                     const char *hdr)
{
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    char *duphdr = ne_strdup(hdr);
    char *sep, *ptr = strchr(duphdr, ' ');
    int ret;
    ne_buffer *errmsg = NULL;

    if (!ptr || strncmp(hdr, "Negotiate", ptr - duphdr) != 0) {
        ne_set_error(sess->sess, _("Negotiate response verification failed: "
                                   "invalid response header token"));
        ne_free(duphdr);
        return NE_ERROR;
    }
    
    ptr++;

    if (strlen(ptr) == 0) {
        NE_DEBUG(NE_DBG_HTTPAUTH, "gssapi: No token in Negotiate response!\n");
        ne_free(duphdr);
        return NE_OK;
    }

    if ((sep = strchr(ptr, ',')) != NULL)
        *sep = '\0';
    if ((sep = strchr(ptr, ' ')) != NULL)
        *sep = '\0';

    NE_DEBUG(NE_DBG_HTTPAUTH, "gssapi: Negotiate response token [%s]\n", ptr);
    ret = continue_negotiate(sess, ptr, &errmsg);
    if (ret) {
        ne_set_error(sess->sess, _("Negotiate response verification failure: %s"),
                     errmsg->data);
    }

    if (errmsg) ne_buffer_destroy(errmsg);
    ne_free(duphdr);

    return ret ? NE_ERROR : NE_OK;
}
#endif

#ifdef HAVE_SSPI
static char *request_sspi(auth_session *sess, struct auth_request *request) 
{
    if (sess->sspi_token)
        return ne_concat(sess->protocol->name, " ", sess->sspi_token, "\r\n", NULL);
    else
        return NULL;
}

static int continue_sspi(auth_session *sess, int ntlm, const char *hdr)
{
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    int status;
    char *response = NULL;
    
    NE_DEBUG(NE_DBG_HTTPAUTH, "auth: SSPI challenge.\n");
    
    if (!sess->sspi_context) {
        status = ne_sspi_create_context(&sess->sspi_context, sess->sspi_host, ntlm);
        if (status) {
            return status;
        }
    }
    
    status = ne_sspi_authenticate(sess->sspi_context, hdr, &response);
    if (status) {
        return status;
    }

    if (response && *response) {
        sess->sspi_token = response;
        
        NE_DEBUG(NE_DBG_HTTPAUTH, "auth: SSPI challenge [%s]\n", sess->sspi_token);
    }

    return 0;
}

static int sspi_challenge(auth_session *sess, int attempt,
                          struct auth_challenge *parms,
                          const char *target, ne_buffer **errmsg
#ifdef WINSCP
                           , struct auth_request* areq
#endif
                          )
{
    int ntlm = ne_strcasecmp(parms->protocol->name, "NTLM") == 0;

    return continue_sspi(sess, ntlm, parms->opaque);
}

static int verify_sspi(struct auth_request *req, auth_session *sess,
                       const char *hdr)
{
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    int ntlm = ne_strncasecmp(hdr, "NTLM ", 5) == 0;
    char *ptr = strchr(hdr, ' ');

    if (!ptr) {
        ne_set_error(sess->sess, _("SSPI response verification failed: "
                                   "invalid response header token"));
        return NE_ERROR;
    }

    while(*ptr == ' ')
        ptr++;

    if (*ptr == '\0') {
        NE_DEBUG(NE_DBG_HTTPAUTH, "auth: No token in SSPI response!\n");
        return NE_OK;
    }

    return continue_sspi(sess, ntlm, ptr);
}

#endif

/* Parse the "domain" challenge parameter and set the domains array up
 * in the session appropriately. */
static int parse_domain(auth_session *sess, const char *domain)
{
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    char *cp = ne_strdup(domain), *p = cp;
    ne_uri base;
    int invalid = 0;

    memset(&base, 0, sizeof base);
    ne_fill_server_uri(sess->sess, &base);

    do {
        char *token = ne_token(&p, ' ');
        ne_uri rel, absolute;

        if (ne_uri_parse(token, &rel) == 0) {
            /* Resolve relative to the Request-URI. */
            base.path = "/";
            ne_uri_resolve(&base, &rel, &absolute);

            /* Compare against the resolved path to check this URI has
             * the same (scheme, host, port) components; ignore it
             * otherwise: */
            base.path = absolute.path;
            if (absolute.path && ne_uri_cmp(&absolute, &base) == 0) {
                sess->domains = ne_realloc(sess->domains, 
                                           ++sess->ndomains *
                                           sizeof(*sess->domains));
                sess->domains[sess->ndomains - 1] = absolute.path;
                NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Using domain %s from %s\n",
                         absolute.path, token);
                absolute.path = NULL;
            }
            else {
                NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Ignoring domain %s\n",
                         token);
            }

            ne_uri_free(&absolute);
        }
        else {
            invalid = 1;
        }
        
        ne_uri_free(&rel);
        
    } while (p && !invalid);

    if (invalid && sess->ndomains) {
        free_domains(sess);
    }

    ne_free(cp);
    base.path = NULL;
    ne_uri_free(&base);

    return invalid;
}

#ifdef HAVE_NTLM

static char *request_ntlm(auth_session *sess, struct auth_request *request) 
{
    char *token = ne__ntlm_getRequestToken(sess->ntlm_context);
    if (token) {
        char *req = ne_concat(sess->protocol->name, " ", token, "\r\n", NULL);
        ne_free(token);
        return req;
    } else {
        return NULL;
    }
}

static int ntlm_challenge(auth_session *sess, int attempt,
                          struct auth_challenge *parms,
                          const char *target, ne_buffer **errmsg
#ifdef WINSCP
                          , struct auth_request* areq
#endif
                          )
{
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    int status;
    
    NE_DEBUG(NE_DBG_HTTPAUTH, "auth: NTLM challenge.\n");
    
    if (!parms->opaque && (!sess->ntlm_context || (attempt > 1))) {
        char password[ABUFSIZE];

        if (get_credentials(sess, errmsg, attempt, parms, password)) {
            /* Failed to get credentials */
            return -1;
        }

        if (sess->ntlm_context) {
            ne__ntlm_destroy_context(sess->ntlm_context);
            sess->ntlm_context = NULL;
        }

        sess->ntlm_context = ne__ntlm_create_context(sess->username, password);

        ne__strzero(password, sizeof password);
    }

    status = ne__ntlm_authenticate(sess->ntlm_context, parms->opaque);
    if (status) {
        return status;
    }

    return 0;
}
#endif /* HAVE_NTLM */

/* Generated with 'mktable safe_username', do not alter here -- */
static const unsigned char table_safe_username[256] = {
/* x00 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
/* x10 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
/* x20 */ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* x30 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* x40 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* x50 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
/* x60 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* x70 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
/* x80 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
/* x90 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
/* xA0 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
/* xB0 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
/* xC0 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
/* xD0 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
/* xE0 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
/* xF0 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
}; /* -- Generated code from 'mktable safe_username' ends. */

/* Returns non-zero if 'username' is unsafe to use without quoting. */
static int unsafe_username(const char *username)
{
    const char *p;
    int rv = 0;

    for (p = username; *p; p++)
        rv |= table_safe_username[(const unsigned char)*p];

    return rv;
}

/* Returns the H(username:realm:password) used in the Digest H(A1)
 * calculation. */
static char *get_digest_h_urp(auth_session *sess, ne_buffer **errmsg,
                              int attempt, struct auth_challenge *parms)
{
    char password[ABUFSIZE], *h_urp;

    if (get_credentials(sess, errmsg, attempt, parms, password)) {
        /* Failed to get credentials */
        return NULL;
    }

    /* Calculate userhash for this (realm, username) if required.
     * https://tools.ietf.org/html/rfc7616#section-3.4.4 */
    if (parms->userhash == userhash_true) {
        sess->userhash = ne_strhash(parms->alg->hash, sess->username, ":",
                                    sess->realm, NULL);
    }
    else {
        /* Without userhash, for usernames which need some kind of
         * escaping, either: a) username* must be supported, which
         * is known if the server sent userhash=false, *and* the
         * caller has indicated the username really is UTF-8; or
         * else b) the challenge is an error since the username
         * cannot be sent safely. */
        if (unsafe_username(sess->username)) {
            if (parms->userhash == userhash_none
                || parms->handler->new_creds == NULL) {
                challenge_error(errmsg, _("could not handle non-ASCII "
                                          "username in Digest challenge"));
                ne__strzero(password, sizeof password);
                return NULL;
            }
            sess->username_star = ne_strparam("UTF-8", NULL, (unsigned char *)sess->username);
            NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Using username* => %s\n", sess->username_star);
        }
    }

    /* H(A1) calculation identical for 2069 or 2617/7616:
     * https://tools.ietf.org/html/rfc7616#section-3.4.2 */
    h_urp = ne_strhash(parms->alg->hash, sess->username, ":", sess->realm, ":",
                       password, NULL);
    ne__strzero(password, sizeof password);

    return h_urp;
}

/* Examine a digest challenge: return 0 if it is a valid Digest challenge,
 * else non-zero. */
static int digest_challenge(auth_session *sess, int attempt,
                            struct auth_challenge *parms,
                            const char *target, ne_buffer **errmsg
#ifdef WINSCP
                            , struct auth_request* areq
#endif
                            )
{
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    char *p, *h_urp = NULL;

    if (parms->alg == NULL) {
        challenge_error(errmsg, _("unknown algorithm in Digest challenge"));
        return -1;
    }

    /* qop= is mandatory from 2617 onward, fail w/o LEGACY_DIGEST */
    if (!parms->got_qop
        && ((parms->handler->protomask & NE_AUTH_LEGACY_DIGEST) == 0)) {
        challenge_error(errmsg, _("legacy Digest challenge not supported"));
        return -1;
    }
    else if (parms->alg->sess && !parms->qop_auth) {
        challenge_error(errmsg, _("incompatible algorithm in Digest challenge"));
        return -1;
    }
    else if (parms->realm == NULL || parms->nonce == NULL) {
        challenge_error(errmsg, _("missing parameter in Digest challenge"));
	return -1;
    }
    else if (parms->stale && sess->realm == NULL) {
        challenge_error(errmsg, _("initial Digest challenge was stale"));
        return -1;
    }
    else if (parms->stale && (sess->alg != parms->alg
                              || strcmp(sess->realm, parms->realm))) {
        /* With stale=true the realm and algorithm cannot change since these
         * require re-hashing H(A1) which defeats the point. */
        challenge_error(errmsg, _("stale Digest challenge with new algorithm or realm"));
        return -1;
    }

    p = ne_strhash(parms->alg->hash, "", NULL);
    if (p == NULL) {
        challenge_error(errmsg,
                        _("%s algorithm in Digest challenge not supported"),
                        parms->alg->name);
        return -1;
    }
    ne_free(p);

    if (!parms->stale) {
        /* Non-stale challenge: clear session and request credentials. */
        clean_session(sess);

        /* The domain parameter must be parsed after the session is
         * cleaned; ignore domain for proxy auth. */
        if (parms->domain && sess->spec == &ah_server_class
            && parse_domain(sess, parms->domain)) {
            challenge_error(errmsg, _("could not parse domain in Digest challenge"));
            return -1;
        }

        /* The hash alg from parameters already tested to work above,
           so re-use it. */
        if ((sess->cnonce = get_cnonce(parms->alg, errmsg)) == NULL) {
            /* challenge_error() called by get_cnonce(). */
            return -1;
        }

        sess->realm = ne_strdup(parms->realm);
        sess->alg = parms->alg;

        h_urp = get_digest_h_urp(sess, errmsg, attempt, parms);
        if (h_urp == NULL) {
            return -1;
        }
    }
    else {
        /* Stale challenge: accept a new nonce or opaque. */
        if (sess->nonce) ne_free(sess->nonce);
        if (sess->opaque && parms->opaque) ne_free(sess->opaque);
    }
    
    sess->nonce = ne_strdup(parms->nonce);
    if (parms->opaque) {
	sess->opaque = ne_strdup(parms->opaque);
    }
    
    if (parms->got_qop) {
	/* What type of qop are we to apply to the message? */
	NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Got qop, using 2617-style.\n");
	sess->nonce_count = 0;
        sess->qop = auth_qop_auth;
    } else {
	/* No qop at all/ */
	sess->qop = auth_qop_none;
    }

    if (h_urp) {
        if (sess->alg->sess) {
            sess->h_a1 = ne_strhash(parms->alg->hash, h_urp, ":",
                                    sess->nonce, ":", sess->cnonce, NULL);
            zero_and_free(h_urp);
            NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Session H(A1) is [%s]\n", sess->h_a1);
        }
        else {
            sess->h_a1 = h_urp;
            NE_DEBUG(NE_DBG_HTTPAUTH, "auth: H(A1) is [%s]\n", sess->h_a1);
        }
    }
    
    NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Accepting digest challenge.\n");

    return 0;
}

/* Returns non-zero if given request-target is inside the
 * authentication domain defined for the session. */
static int inside_domain(auth_session *sess, const char *target)
{
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    int inside = 0;
    size_t n;
    ne_uri uri;
    
    /* Parse the Request-URI; it will be an absoluteURI if using a
     * proxy, and possibly '*'. */
    if (strcmp(target, "*") == 0 || ne_uri_parse(target, &uri) != 0) {
        /* Presume outside the authentication domain. */
        return 0;
    }

    for (n = 0; n < sess->ndomains && !inside; n++) {
        const char *d = sess->domains[n];
        
        inside = strncmp(uri.path, d, strlen(d)) == 0;
    }
    
    NE_DEBUG(NE_DBG_HTTPAUTH, "auth: '%s' is inside auth domain: %d.\n", 
             uri.path, inside);
    ne_uri_free(&uri);
    
    return inside;
}            

/* Return Digest authentication credentials header value for the given
 * session. */
static char *request_digest(auth_session *sess, struct auth_request *req) 
{
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    char *h_a2, *response;
    char nc_value[9] = {0};
    const char *qop_value = "auth"; /* qop-value */
    ne_buffer *ret;
    unsigned int hash = sess->alg->hash;

    /* Do not submit credentials if an auth domain is defined and this
     * request-uri fails outside it. */
    if (sess->ndomains && !inside_domain(sess, req->target)) {
        return NULL;
    }

    /* H(A2): https://tools.ietf.org/html/rfc7616#section-3.4.3 - Note
     * that the RFC specifies that "request-uri" is used in the A2
     * grammar, which matches the RFC 9112 'request-target', which is
     * what was passed through by ah_create. */
    h_a2 = ne_strhash(hash, req->method, ":", req->target, NULL);
    NE_DEBUG(NE_DBG_HTTPAUTH, "auth: H(A2): %s\n", h_a2);

    /* Calculate the 'response' to the Digest challenge to send the
     * server in the request. */
    if (sess->qop == auth_qop_none) {
        /* RFC 2069 case,
         * https://tools.ietf.org/html/rfc2069#section-2.1.2 */
        response = ne_strhash(hash, sess->h_a1, ":", sess->nonce,
                              ":", h_a2, NULL);
    } else {
        /* For RFC 2617/7616-style; part of this calculation will be
         * needed again when verifying the (Proxy-)Authentication-Info
         * response header; that part is cached in sess->response_rhs.
         * https://tools.ietf.org/html/rfc7616#section-3.4.1 */
        sess->nonce_count++;
        ne_snprintf(nc_value, 9, "%08x", sess->nonce_count);

        if (sess->response_rhs) ne_free(sess->response_rhs);
        sess->response_rhs = ne_concat(sess->nonce, ":",
                                       nc_value, ":", sess->cnonce, ":",
                                       qop_value, NULL);
        response = ne_strhash(hash, sess->h_a1, ":",
                              sess->response_rhs, ":", h_a2, NULL);
    }

    ret = ne_buffer_create();

    ne_buffer_concat(ret, 
                     "Digest realm=\"", sess->realm, "\", "
		     "nonce=\"", sess->nonce, "\", "
		     "uri=\"", req->target, "\", "
		     "response=\"", response, "\", "
		     "algorithm=\"", sess->alg->name, "\"",
		     NULL);
    if (sess->username_star) {
        ne_buffer_concat(ret, ", username*=", sess->username_star, NULL);
    }
    else {
        ne_buffer_concat(ret, ", username=\"",
                         sess->userhash ? sess->userhash : sess->username,
                         "\"", NULL);
    }

    ne_free(response);
    ne_free(h_a2);
    
    if (sess->opaque != NULL) {
	ne_buffer_concat(ret, ", opaque=\"", sess->opaque, "\"", NULL);
    }

    if (sess->qop != auth_qop_none) {
	/* Add in cnonce and nc-value fields */
	ne_buffer_concat(ret, ", cnonce=\"", sess->cnonce, "\", "
			 "nc=", nc_value, ", "
			 "qop=\"", qop_value, "\"", NULL);
    }
    if (sess->userhash) {
        ne_buffer_czappend(ret, ", userhash=true");
    }

    ne_buffer_zappend(ret, "\r\n");

    return ne_buffer_finish(ret);
}

/* Parse line of comma-separated key-value pairs.  If 'ischall' == 1,
 * then also return a leading space-separated token, as *value ==
 * NULL.  Otherwise, if return value is 0, *key and *value will be
 * non-NULL.  If return value is non-zero, parsing has ended.  If
 * 'sep' is non-NULL and ischall is 1, the separator character is
 * written to *sep when a challenge is parsed. */
static int tokenize(char **hdr, char **key, char **value, char *sep,
                    int ischall)
{
    char *pnt = *hdr;
    enum { BEFORE_EQ, AFTER_EQ, AFTER_EQ_QUOTED } state = BEFORE_EQ;
    
    if (**hdr == '\0')
	return 1;

    *key = NULL;

    do {
	switch (state) {
	case BEFORE_EQ:
	    if (*pnt == '=') {
		if (*key == NULL)
		    return -1;
		*pnt = '\0';
		*value = pnt + 1;
		state = AFTER_EQ;
	    } else if ((*pnt == ' ' || *pnt == ',') 
                       && ischall && *key != NULL) {
		*value = NULL;
                if (sep) *sep = *pnt;
		*pnt = '\0';
		*hdr = pnt + 1;
		return 0;
	    } else if (*key == NULL && strchr(" \r\n\t", *pnt) == NULL) {
		*key = pnt;
	    }
	    break;
	case AFTER_EQ:
	    if (*pnt == ',') {
		*pnt = '\0';
		*hdr = pnt + 1;
		return 0;
	    } else if (*pnt == '\"') {
		state = AFTER_EQ_QUOTED;
	    }
	    break;
	case AFTER_EQ_QUOTED:
	    if (*pnt == '\"') {
		state = AFTER_EQ;
                *pnt = '\0';
	    }
	    break;
	}
    } while (*++pnt != '\0');
    
    if (state == BEFORE_EQ && ischall && *key != NULL) {
	*value = NULL;
        if (sep) *sep = '\0';
    }

    *hdr = pnt;

    /* End of string: */
    return 0;
}

/* Pass this the value of the 'Authentication-Info:' header field, if
 * one is received.
 * Returns:
 *    0 if it gives a valid authentication for the server 
 *    non-zero otherwise (don't believe the response in this case!).
 */
static int verify_digest_response(struct auth_request *req, auth_session *sess,
                                  const char *value) 
{
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    char *hdr, *pnt, *key, *val;
    auth_qop qop = auth_qop_none;
    char *nextnonce, *rspauth, *cnonce, *nc, *qop_value;
    unsigned int nonce_count;
    int ret = NE_OK;

    nextnonce = rspauth = cnonce = nc = qop_value = NULL;

    pnt = hdr = ne_strdup(value);
    
    NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Got Auth-Info header: %s\n", value);

    while (tokenize(&pnt, &key, &val, NULL, 0) == 0) {
	val = ne_shave(val, "\"");

	if (ne_strcasecmp(key, "qop") == 0) {
            qop_value = val;
            if (ne_strcasecmp(val, "auth") == 0) {
		qop = auth_qop_auth;
	    } else {
		qop = auth_qop_none;
	    }
	} else if (ne_strcasecmp(key, "nextnonce") == 0) {
	    nextnonce = val;
	} else if (ne_strcasecmp(key, "rspauth") == 0) {
	    rspauth = val;
	} else if (ne_strcasecmp(key, "cnonce") == 0) {
	    cnonce = val;
	} else if (ne_strcasecmp(key, "nc") == 0) { 
	    nc = val;
        }
    }

    if (qop == auth_qop_none) {
        /* The 2069-style A-I header only has the entity and nextnonce
         * parameters. */
        NE_DEBUG(NE_DBG_HTTPAUTH, "auth: 2069-style A-I header.\n");
    }
    else if (!rspauth || !cnonce || !nc) {
        ret = NE_ERROR;
        ne_set_error(sess->sess, _("Digest mutual authentication failure: "
                                   "missing parameters"));
    }
    else if (strcmp(cnonce, sess->cnonce) != 0) {
        ret = NE_ERROR;
        ne_set_error(sess->sess, _("Digest mutual authentication failure: "
                                   "client nonce mismatch"));
    }
    else if (nc) {
        char *ptr;
        
        errno = 0;
        nonce_count = strtoul(nc, &ptr, 16);
        if (*ptr != '\0' || errno) {
            ret = NE_ERROR;
            ne_set_error(sess->sess, _("Digest mutual authentication failure: "
                                       "could not parse nonce count"));
        }
        else if (nonce_count != sess->nonce_count) {
            ret = NE_ERROR;
            ne_set_error(sess->sess, _("Digest mutual authentication failure: "
                                       "nonce count mismatch (%u not %u)"),
                         nonce_count, sess->nonce_count);
        }
    }

    /* Finally, for qop=auth cases, if everything else is OK, verify
     * the response-digest field. */    
    if (qop == auth_qop_auth && ret == NE_OK) {
        char *h_a2, *response;
        unsigned int hash = sess->alg->hash;

        h_a2 = ne_strhash(hash, ":", req->target, NULL);
        response = ne_strhash(hash, sess->h_a1, ":", sess->response_rhs,
                              ":", h_a2, NULL);
        ne_free(h_a2);
        ne_free(sess->response_rhs);
        sess->response_rhs = NULL;

        /* And... do they match? */
        ret = ne_strcasecmp(response, rspauth) == 0 ? NE_OK : NE_ERROR;
        
        NE_DEBUG(NE_DBG_HTTPAUTH, "auth: response-digest match: %s "
                 "(expected [%s] vs actual [%s])\n", 
                 ret == NE_OK ? "yes" : "no", response, rspauth);

        if (ret) {
            ne_set_error(sess->sess, _("Digest mutual authentication failure: "
                                       "request-digest mismatch"));
        }

        ne_free(response);
    }

    /* Check for a nextnonce */
    if (nextnonce != NULL) {
	NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Found nextnonce of [%s].\n", nextnonce);
        ne_free(sess->nonce);
	sess->nonce = ne_strdup(nextnonce);
        sess->nonce_count = 0;
    }

    ne_free(hdr);

    return ret;
}

#ifdef WINSCP

#define PASSPORT_REQ_ID "http://www.webdav.org/neon/hooks/http-passport-req"
#define PASSPORT_NAME "Passport1.4"

struct aux_request_init_data
{
    ne_aux_request_init aux_request_init;
    void * userdata;
};

static void free_aux_request_init(void * cookie)
{
    ne_free(cookie);
}

void ne_set_aux_request_init(ne_session * sess, ne_aux_request_init aux_request_init, void * userdata)
{
    struct aux_request_init_data * data = ne_malloc(sizeof(*data));
    data->aux_request_init = aux_request_init;
    data->userdata = userdata;
    ne_set_session_private(sess, PASSPORT_REQ_ID, data);
    ne_hook_destroy_session(sess, free_aux_request_init, data);
}

int is_passport_challenge(ne_request *req, const ne_status *status)
{
    const char *auth = ne_get_response_header(req, "WWW-Authenticate");
    return 
        (status->code == 302) &&
        (auth != NULL) &&
        (ne_strncasecmp(auth, PASSPORT_NAME, strlen(PASSPORT_NAME)) == 0);
}

static int passport_challenge(auth_session *sess, int attempt,
                              struct auth_challenge *parms,
                              const char *uri, ne_buffer **errmsg,
                              struct auth_request* areq)
{
    char *tmp, password[NE_ABUFSIZ];
    char *tmp_username;
    char *tmp_password;
    const char *auth_hdr;
    ne_session * sign_session;
    ne_request * sign_request;
    int status;
    ne_session * session = ne_get_session(areq->request);
    ne_uri orig_uri = {0};
    char * org_url;
    int result;

    if (sess->passport_auth_header == NULL)
    {
        struct aux_request_init_data * init_data = ne_get_session_private(session, PASSPORT_REQ_ID);

        if (init_data == NULL)
        {
            challenge_error(errmsg, _("No init ptr"));
            return -1;
        }

        clean_session(sess);

        if (sess->passport_login_uri.host == NULL)
        {
            ne_session * nexus_session;
            ne_request * nexus_request;
            int status;
            const char * host = "nexus.passport.com";
            const char * scheme = "https";
            int result = 0;

            NE_DEBUG(NE_DBG_HTTPAUTH, "Retrieving Passport Nexus URL");

            nexus_session = ne_session_create(scheme, host, ne_uri_defaultport(scheme));
            nexus_request = ne_request_create(nexus_session, "GET", "/rdr/pprdr.asp");
            init_data->aux_request_init(nexus_session, nexus_request, init_data->userdata);
            status = ne_request_dispatch(nexus_request);

            if (status != NE_OK)
            {
                challenge_error(errmsg, _("Error contacting %s: %s"), host, ne_get_error(nexus_session));
                result = -1;
            }
            else
            {
                const char * urls = ne_get_response_header(nexus_request, "PassportURLs");
                if (urls == NULL)
                {
                    challenge_error(errmsg, _("Missing PassportURLs header"));
                    result = -1;
                }
                else
                {
                    char * buf, * pnt, * key, * val;
                    pnt = buf = ne_strdup(urls);
                    while ((sess->passport_login_uri.host == NULL) &&
                           (tokenize(&pnt, &key, &val, NULL, 0) == 0))
                    {
                        if (ne_strcasecmp(key, "DALogin") == 0)
                        {
                            char * uri = ne_concat(scheme, "://", val, NULL);
                            NE_DEBUG(NE_DBG_HTTPAUTH, "Passport Nexus URL is %s", uri);
                            ne_uri_parse(uri, &sess->passport_login_uri);
                            if (sess->passport_login_uri.port == 0)
                            {
                                sess->passport_login_uri.port = ne_uri_defaultport(sess->passport_login_uri.scheme);
                            }
                            ne_free(uri);
                        }
                    }

                    ne_free(buf);

                    if (sess->passport_login_uri.host == NULL)
                    {
                        challenge_error(errmsg, _("Missing DALogin URL"));
                        result = -1;
                    }
                }
            }

            ne_request_destroy(nexus_request);
            ne_session_destroy(nexus_session);

            if (result != 0)
            {
                return result;
            }
        }

        auth_hdr = ne_get_response_header(areq->request, "WWW-Authenticate");
        if (auth_hdr == NULL)
        {
            challenge_error(errmsg, _("missing WWW-Authenticate header"));
            return -1;
        }

        if (get_credentials(sess, errmsg, attempt, parms, password)) {
            /* Failed to get credentials */
            return -1;
        }

        tmp_username = ne_path_escape(sess->username);
        tmp_password = ne_path_escape(password);

        /* There must be some, othewise we won't be here */
        auth_hdr = strchr(auth_hdr, ' ');
        if (auth_hdr == NULL) {
            challenge_error(errmsg, _("missing space in WWW-Authenticate header"));
            return -1;
        }
        while (*auth_hdr == ' ') auth_hdr++;

        ne_fill_server_uri(session, &orig_uri);
        orig_uri.path = ne_strdup(areq->target);
        org_url = ne_uri_unparse(&orig_uri);
        ne_uri_free(&orig_uri);

        tmp =
            ne_concat(
                PASSPORT_NAME, " sign-in=", tmp_username, ",pwd=", tmp_password, ",OrgVerb=", areq->method,
                ",OrgUrl=", org_url, ",", auth_hdr, NULL);
        ne_free(org_url);
        /* Paranoia. */
        memset(password, 0, sizeof password);
        memset(tmp_password, 0, strlen(tmp_password));
        ne_free(tmp_username);
        ne_free(tmp_password);

        NE_DEBUG(NE_DBG_HTTPAUTH, "Signing into Nexus");

        sign_session = ne_session_create(sess->passport_login_uri.scheme, sess->passport_login_uri.host, sess->passport_login_uri.port);
        sign_request = ne_request_create(sign_session, "GET", sess->passport_login_uri.path);
        ne_add_request_header(sign_request, "Authorization", tmp);
        init_data->aux_request_init(sign_session, sign_request, init_data->userdata);
        status = ne_request_dispatch(sign_request);

        memset(tmp, 0, strlen(tmp));
        ne_free(tmp);

        result = 0;
        if (status != NE_OK)
        {
            challenge_error(errmsg, _("Error signing into Nexus: %s"), ne_get_error(sign_session));
            result = -1;
        }
        else
        {
            const char * auth_info = ne_get_response_header(sign_request, "Authentication-Info");
            if (auth_info == NULL)
            {
                challenge_error(errmsg, _("Missing Authentication-Info header"));
                result = -1;
            }
            else
            {
                int success = -1;

                char * buf, * pnt, * key, * val;
                pnt = buf = ne_strdup(auth_info);
                while (((sess->passport_auth_header == NULL) || (success < 0)) &&
                       (tokenize(&pnt, &key, &val, NULL, 1) == 0))
                {
                    if (val == NULL)
                    {
                        if (ne_strcasecmp(key, PASSPORT_NAME) != 0)
                        {
                            challenge_error(errmsg, _("Wrong Authentication-Info header"));
                            result = -1;
                        }
                    }
                    else if (ne_strcasecmp(key, "da-status") == 0)
                    {
                        if (ne_strcasecmp(val, "success") == 0)
                        {
                            success = 1;
                        }
                        else
                        {
                            challenge_error(errmsg, _("Authentication failure"));
                            result = -1;
                            success = 0;
                        }
                    }
                    else if (ne_strcasecmp(key, "from-PP") == 0)
                    {
                        sess->passport_auth_header = ne_concat("Authorization: ", PASSPORT_NAME, " ", key, "=", val, "\r\n", NULL);
                    }
                }

                ne_free(buf);

                if ((sess->passport_auth_header == NULL) || (success < 0))
                {
                    challenge_error(errmsg, _("Cannot parse Nexus response"));
                    result = -1;
                }
                else
                {
                    NE_DEBUG(NE_DBG_HTTPAUTH, "Signed into Nexus");
                }
            }
        }

        ne_request_destroy(sign_request);
        ne_session_destroy(sign_session);
    }

    return result;
}

static char *request_passport(auth_session *sess, struct auth_request *req)
{
    return ne_concat(sess->passport_auth_header, sess->passport_cookies_header, NULL);
}

static void lower_case(char * s)
{
    int i;
    for (i = 0; i < strlen(s); i++)
    {
        s[i] = ne_tolower(s[i]);
    }
}

static int verify_passport_response(struct auth_request *req, auth_session *sess,
                                    const char *value)
{
    int result = 0;

    {
        ne_buffer * names = ne_buffer_create();
        char * buf, * pnt, * key, * val;
        /* We should really send all cookies, but for now, we send only those we know we have to.
           ClientCanary cookie is needed for the modify operations (PUT, DELETE, etc). */
        ne_buffer_zappend(names, "=clientcanary=");
        pnt = buf = ne_strdup(value);
        while (tokenize(&pnt, &key, &val, NULL, 1) == 0)
        {
            if (val == NULL)
            {
                if (ne_strcasecmp(key, PASSPORT_NAME) != 0)
                {
                    NE_DEBUG(NE_DBG_HTTPAUTH, "Wrong Authentication-Info header");
                    result = -1;
                }
            }
            else if (ne_strcasecmp(key, "tname") == 0)
            {
                lower_case(val);
                ne_buffer_concat(names, val, "=", NULL);
            }
        }

        ne_free(buf);

        /* We should update cookies based on future responses, for what we preserve list of cookies to cache,
           but we do not do that atm, so this field is not used actually */
        sess->passport_cookies = ne_buffer_finish(names);
    }

    {
        const char * set_cookies_header = ne_get_response_header(req->request, "Set-Cookie");
        if (set_cookies_header == NULL)
        {
            NE_DEBUG(NE_DBG_HTTPAUTH, "No Cookies");
            result = -1;
        }
        else
        {
            ne_buffer * cookie_header = NULL;
            char * cookies = ne_strdup(set_cookies_header);
            char * p = cookies;
            while (p != NULL)
            {
                /* Far from perfect, as it does not distringuish a comma from merged Set-Cookie header [seeadd_response_header],
                   and command e.g. in "expires=Thu, 30-Oct-1980 16:00:00 GMT".
                   But it is good enough for now, when we need only name and value */
                char * cookie = ne_shave(ne_token(&p, ','), " \t");
                char * cookie_name = ne_token(&cookie, '=');
                char * s = ne_concat("=", cookie_name, "=", NULL);
                lower_case(s);

                if (strstr(sess->passport_cookies, s) != NULL)
                {
                    char * val = ne_token(&cookie, ';');

                    if (cookie_header == NULL)
                    {
                        cookie_header = ne_buffer_create();
                        ne_buffer_zappend(cookie_header, "Cookie: ");
                    }
                    else
                    {
                        ne_buffer_zappend(cookie_header, "; ");
                    }

                    ne_buffer_concat(cookie_header, cookie_name, "=", val, NULL);
                }

                ne_free(s);
            }

            ne_free(cookies);

            if (cookie_header != NULL)
            {
                if (sess->passport_cookies_header != NULL)
                {
                    ne_free(sess->passport_cookies_header);
                }
                ne_buffer_zappend(cookie_header, "\r\n");
                sess->passport_cookies_header = ne_buffer_finish(cookie_header);
            }
        }
    }

    return result;
}

#endif

static const struct auth_protocol protocols[] = {
    { NE_AUTH_BASIC, 10, "Basic",
      basic_challenge, request_basic, NULL,
      0 },
    { NE_AUTH_DIGEST, 20, "Digest",
      digest_challenge, request_digest, verify_digest_response,
      0 },
#ifdef HAVE_GSSAPI
    { NE_AUTH_GSSAPI_ONLY, 30, "Negotiate",
      negotiate_challenge, request_negotiate, verify_negotiate_response,
      AUTH_FLAG_OPAQUE_PARAM|AUTH_FLAG_VERIFY_NON40x|AUTH_FLAG_CONN_AUTH },
#endif
#ifdef HAVE_SSPI
    { NE_AUTH_NTLM, 30, "NTLM",
      sspi_challenge, request_sspi, NULL,
      AUTH_FLAG_OPAQUE_PARAM|AUTH_FLAG_VERIFY_NON40x|AUTH_FLAG_CONN_AUTH },
    { NE_AUTH_SSPI, 30, "Negotiate",
      sspi_challenge, request_sspi, verify_sspi,
      AUTH_FLAG_OPAQUE_PARAM|AUTH_FLAG_VERIFY_NON40x|AUTH_FLAG_CONN_AUTH },
#endif
#ifdef HAVE_NTLM
    { NE_AUTH_NTLM, 30, "NTLM",
      ntlm_challenge, request_ntlm, NULL,
      AUTH_FLAG_OPAQUE_PARAM|AUTH_FLAG_VERIFY_NON40x|AUTH_FLAG_CONN_AUTH },
#endif
#ifdef WINSCP
    { NE_AUTH_PASSPORT, 30, PASSPORT_NAME,
      passport_challenge, request_passport, verify_passport_response,
      AUTH_FLAG_FULL_HEADER|AUTH_FLAG_CONN_AUTH /* just a guess */ },
#endif
    { 0 }
};

/* Insert a new auth challenge 'chall' into list of challenges 'list'.
 * The challenge list is kept in sorted order of strength, with
 * highest strength first. */
static void insert_challenge(struct auth_challenge **list,
                             struct auth_challenge *chall)
{
    struct auth_challenge **p;

    for (p = list; *p != NULL; p = &(*p)->next) {
        if (chall->protocol->strength > (*p)->protocol->strength
            || ((*p)->protocol->id == NE_AUTH_DIGEST
                && chall->protocol->id == NE_AUTH_DIGEST
                && chall->alg && (*p)->alg
                && chall->alg->hash > (*p)->alg->hash)) {
            break;
        }
    }

    chall->next = *p;
    *p = chall;
}

static void challenge_error(ne_buffer **errbuf, const char *fmt, ...)
{
    char err[128];
    va_list ap;
    size_t len;
    
    va_start(ap, fmt);
    len = ne_vsnprintf(err, sizeof err, fmt, ap);
    va_end(ap);
    
    if (*errbuf == NULL) {
        *errbuf = ne_buffer_create();
        ne_buffer_append(*errbuf, err, len);
    }
    else {
        ne_buffer_concat(*errbuf, ", ", err, NULL);
    }
}

/* Passed the value of a "(Proxy,WWW)-Authenticate: " header field.
 * Returns 0 if valid challenge was accepted; non-zero if no valid
 * challenge was found. */
static int auth_challenge(auth_session *sess, int attempt, const char *uri,
                          const char *value
#ifdef WINSCP
                          , struct auth_request* areq
#endif
                          )
{
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    char *pnt, *key, *val, *hdr, sep;
    struct auth_challenge *chall = NULL, *challenges = NULL;
    ne_buffer *errmsg = NULL;

    pnt = hdr = ne_strdup(value); 

    /* The header value may be made up of one or more challenges.  We
     * split it down into attribute-value pairs, then search for
     * schemes in the pair keys. */

    while (!tokenize(&pnt, &key, &val, &sep, 1)) {

	if (val == NULL) {
            /* Special case, challenge token, not key=value pair: */
            const struct auth_protocol *proto = NULL;
            struct auth_handler *hdl;
            size_t n;

            /* Accumulated challenge is now completed and can be
             * inserted into the list. */
            if (chall) {
                insert_challenge(&challenges, chall);
                chall = NULL;
            }

            for (hdl = sess->handlers; hdl; hdl = hdl->next) {
                for (n = 0; protocols[n].id; n++) {
                    if (protocols[n].id & hdl->protomask
                        && ne_strcasecmp(key, protocols[n].name) == 0) {
                        proto = &protocols[n];
                        break;
                    }
                }
                if (proto) break;
            }

            if (proto == NULL) {
                /* Ignore this challenge. */
                challenge_error(&errmsg, _("ignored %s challenge"), key);
                continue;
	    }
            
            NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Got '%s' challenge.\n", proto->name);
            chall = ne_calloc(sizeof *chall);
            chall->protocol = proto;
            chall->handler = hdl;
            chall->alg = HASHALG_MD5; /* RFC default is MD5 */

            if ((proto->flags & AUTH_FLAG_OPAQUE_PARAM) && sep == ' ') {
                /* Cope with the fact that the unquoted base64
                 * parameter token doesn't match the 2617 auth-param
                 * grammar: */
                chall->opaque = ne_shave(ne_token(&pnt, ','), " \t");
                NE_DEBUG(NE_DBG_HTTPAUTH, "auth: %s opaque parameter '%s'\n",
                         proto->name, chall->opaque);
                if (!pnt) break; /* stop parsing at end-of-string. */
            }
	    continue;
	} else if (chall == NULL) {
	    /* Ignore pairs for an unknown challenge. */
            NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Ignored parameter: %s = %s\n", key, val);
	    continue;
	}

	/* Strip quotes off value. */
	val = ne_shave(val, "\"'");

	if (ne_strcasecmp(key, "realm") == 0) {
	    chall->realm = val;
	} else if (ne_strcasecmp(key, "nonce") == 0) {
	    chall->nonce = val;
	} else if (ne_strcasecmp(key, "opaque") == 0) {
	    chall->opaque = val;
	} else if (ne_strcasecmp(key, "stale") == 0) {
	    /* Truth value */
	    chall->stale = (ne_strcasecmp(val, "true") == 0);
	} else if (ne_strcasecmp(key, "algorithm") == 0) {
            unsigned int n;

            chall->alg = NULL; /* left unset for unknown algorithm. */
            for (n = 0; n < NUM_HASHALGS; n++) {
                if (ne_strcasecmp(val, hashalgs[n].name) == 0) {
                    chall->alg = &hashalgs[n];
                    break;
                }
            }

            NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Mapped '%s' to algorithm %s\n", val,
                     chall->alg ? chall->alg->name : "[unknown]");
	} else if (ne_strcasecmp(key, "qop") == 0) {
            /* iterate over each token in the value */
            do {
                const char *tok = ne_shave(ne_token(&val, ','), " \t");
                
                if (ne_strcasecmp(tok, "auth") == 0) {
                    chall->qop_auth = 1;
                }
            } while (val);
            
            chall->got_qop = chall->qop_auth;
	}
        else if (ne_strcasecmp(key, "domain") == 0) {
            chall->domain = val;
        }
        else if (ne_strcasecmp(key, "userhash") == 0) {
            if (strcmp(val, "true") == 0)
                chall->userhash = userhash_true;
            else if (strcmp(val, "false") == 0)
                chall->userhash = userhash_false;
            else
                NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Ignored bogus userhash value '%s'\n", val);
        }
    }

    /* Insert the in-flight challenge (if any). */
    if (chall) insert_challenge(&challenges, chall);
    
    sess->protocol = NULL;

    /* Iterate through the challenge list (which is sorted from
     * strongest to weakest) attempting to accept each one. */
    for (chall = challenges; chall != NULL; chall = chall->next) {
        NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Trying %s challenge...\n",
                 chall->protocol->name);
        if (chall->protocol->challenge(sess, attempt, chall, uri, &errmsg
#ifdef WINSCP
            , areq
#endif
            ) == 0) {
            NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Accepted %s challenge.\n", 
                     chall->protocol->name);
            sess->protocol = chall->protocol;
            break;
        }
    }

    if (!sess->protocol) {
        NE_DEBUG(NE_DBG_HTTPAUTH, "auth: No challenges accepted.\n");
        ne_set_error(sess->sess, _(sess->spec->error_noauth),
                     errmsg ? errmsg->data : _("could not parse challenge"));
    }

    while (challenges != NULL) {
	chall = challenges->next;
	ne_free(challenges);
	challenges = chall;
    }

    ne_free(hdr);
    if (errmsg) ne_buffer_destroy(errmsg);

    return !(sess->protocol != NULL);
}

static void ah_create(ne_request *req, void *session, const char *method,
		      const char *target)
{
    auth_session *sess = session;
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    int is_connect = strcmp(method, "CONNECT") == 0;

    if (sess->context == AUTH_ANY ||
        (is_connect && sess->context == AUTH_CONNECT) ||
        (!is_connect && sess->context == AUTH_NOTCONNECT)) {
        struct auth_request *areq = ne_calloc(sizeof *areq);
        struct auth_handler *hdl;
        
        NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Create for %s\n", sess->spec->resp_hdr);
        
        areq->method = method;
        areq->target = target;
        areq->request = req;
        
        ne_set_request_private(req, sess->spec->id, areq);

        /* For each new request, reset the attempt counter in every
         * registered handler. */
        for (hdl = sess->handlers; hdl; hdl = hdl->next) {
            hdl->attempt = 0;
        }
    }
}


static void ah_pre_send(ne_request *r, void *cookie, ne_buffer *request)
{
    auth_session *sess = cookie;
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    struct auth_request *req = ne_get_request_private(r, sess->spec->id);

    if (sess->protocol && req) {
	char *value;

        NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Sending '%s' response.\n",
                 sess->protocol->name);

        value = sess->protocol->response(sess, req);

	if (value != NULL) {
#ifdef WINSCP
	    if (sess->protocol->flags & AUTH_FLAG_FULL_HEADER)
	    {
	    	    ne_buffer_zappend(request, value);
	    }
	    else
	    {
#endif
	    ne_buffer_concat(request, sess->spec->req_hdr, ": ", value, NULL);
#ifdef WINSCP
	    }
#endif
	    ne_free(value);
	}
    }

}

static int ah_post_send(ne_request *req, void *cookie, const ne_status *status)
{
    auth_session *sess = cookie;
    NE_DEBUG_WINSCP_CONTEXT(sess->sess);
    struct auth_request *areq = ne_get_request_private(req, sess->spec->id);
    const char *auth_hdr, *auth_info_hdr;
    int ret = NE_OK;

    if (!areq) return NE_OK;

    auth_hdr = ne_get_response_header(req, sess->spec->resp_hdr);
    auth_info_hdr = ne_get_response_header(req, sess->spec->resp_info_hdr);

    if (sess->context == AUTH_CONNECT && status->code == 401 && !auth_hdr) {
        /* Some broken proxies issue a 401 as a proxy auth challenge
         * to a CONNECT request; handle this here. */
        auth_hdr = ne_get_response_header(req, "WWW-Authenticate");
        auth_info_hdr = NULL;
    }

#ifdef HAVE_GSSAPI
    /* whatever happens: forget the GSSAPI token cached thus far */
    if (sess->gssapi_token) {
        ne_free(sess->gssapi_token);
        sess->gssapi_token = NULL;
    }
#endif

#ifdef HAVE_SSPI
    /* whatever happens: forget the SSPI token cached thus far */
    if (sess->sspi_token) {
        ne_free(sess->sspi_token);
        sess->sspi_token = NULL;
    }
#endif

    NE_DEBUG(NE_DBG_HTTPAUTH, 
	     "auth: Post-send (#%d), code is %d (want %d), %s is %s\n",
	     areq->attempt, status->code, sess->spec->status_code, 
	     sess->spec->resp_hdr, auth_hdr ? auth_hdr : "(none)");
    if (auth_info_hdr && sess->protocol && sess->protocol->verify 
        && (sess->protocol->flags & AUTH_FLAG_VERIFY_NON40x) == 0) {
        ret = sess->protocol->verify(areq, sess, auth_info_hdr);
    }
    else if (sess->protocol && sess->protocol->verify
             && (sess->protocol->flags & AUTH_FLAG_VERIFY_NON40x) 
             && (status->klass == 2 || status->klass == 3)
             && auth_hdr) {
        ret = sess->protocol->verify(areq, sess, auth_hdr);
    }
    else if ((status->code == sess->spec->status_code ||
#ifdef WINSCP
              is_passport_challenge(req, status) ||
#endif
              (status->code == 401 && sess->context == AUTH_CONNECT)) &&
	       auth_hdr) {
        /* note above: allow a 401 in response to a CONNECT request
         * from a proxy since some buggy proxies send that. */
	NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Got challenge (code %d).\n", status->code);
	if (!auth_challenge(sess, areq->attempt++, areq->target, auth_hdr
#ifdef WINSCP
                                                     , areq
#endif
                                                     )) {
	    ret = NE_RETRY;
	} else {
	    clean_session(sess);
	    ret = sess->spec->fail_code;
	}
        
        /* Set or clear the conn-auth flag according to whether this
         * was an accepted challenge for a borked protocol. */
        ne_set_session_flag(sess->sess, NE_SESSFLAG_CONNAUTH,
                            sess->protocol 
                            && (sess->protocol->flags & AUTH_FLAG_CONN_AUTH));
    }

#ifdef HAVE_SSPI
    /* Clear the SSPI context after successful authentication. */
    if (status->code != sess->spec->status_code && sess->sspi_context) {
        ne_sspi_clear_context(sess->sspi_context);
    }
#endif

    return ret;
}

static void ah_destroy(ne_request *req, void *session)
{
    auth_session *sess = session;
    struct auth_request *areq = ne_get_request_private(req, sess->spec->id);

    if (areq) {
        ne_free(areq);
    }
}

static void free_auth(void *cookie)
{
    auth_session *sess = cookie;
    struct auth_handler *hdl, *next;

#ifdef HAVE_GSSAPI
    if (sess->gssname != GSS_C_NO_NAME) {
        unsigned int major;
        gss_release_name(&major, &sess->gssname);
    }
#endif

    for (hdl = sess->handlers; hdl; hdl = next) {
        next = hdl->next;
        ne_free(hdl);
    }

    clean_session(sess);
#ifdef HAVE_SSPI
    if (sess->sspi_host) ne_free(sess->sspi_host);
    sess->sspi_host = NULL;
#endif
    ne_free(sess);
}

static void auth_register(ne_session *sess, int isproxy, unsigned protomask,
                          const struct auth_class *ahc, const char *id,
                          ne_auth_creds old_creds, ne_auth_provide new_creds,
                          void *userdata)
{
    auth_session *ahs;
    struct auth_handler **hdl;

    /* Handle the _ALL and _DEFAULT protocol masks: */
    if ((protomask & NE_AUTH_ALL) == NE_AUTH_ALL) {
        protomask |= NE_AUTH_BASIC | NE_AUTH_DIGEST | NE_AUTH_NEGOTIATE;
    }
    else if ((protomask & NE_AUTH_DEFAULT) == NE_AUTH_DEFAULT) {
        protomask |= NE_AUTH_BASIC | NE_AUTH_DIGEST;
        
        if (strcmp(ne_get_scheme(sess), "https") == 0 || isproxy) {
            protomask |= NE_AUTH_NEGOTIATE;
        }
    }

    /* For backwards-compatibility with older releases where DIGEST
     * used to be defined as WEAKEST, if only LEGACY_DIGEST is given,
     * that implies DIGEST|LEGACY_DIGEST. */
    if ((protomask & (NE_AUTH_LEGACY_DIGEST|NE_AUTH_DIGEST)) == NE_AUTH_LEGACY_DIGEST) {
        NE_DEBUG(NE_DBG_HTTPAUTH, "auth: Legacy Digest support compatibility mode.\n");
        protomask |= NE_AUTH_DIGEST;
    }

    if ((protomask & NE_AUTH_NEGOTIATE) == NE_AUTH_NEGOTIATE) {
        /* Map NEGOTIATE to NTLM | GSSAPI. */
        protomask |= NE_AUTH_GSSAPI | NE_AUTH_NTLM;
    }
    
    if ((protomask & NE_AUTH_GSSAPI) == NE_AUTH_GSSAPI) {
        /* Map GSSAPI to GSSAPI_ONLY | SSPI. */
        protomask |= NE_AUTH_GSSAPI_ONLY | NE_AUTH_SSPI;
    }

    ahs = ne_get_session_private(sess, id);
    if (ahs == NULL) {
        ahs = ne_calloc(sizeof *ahs);
        
        ahs->sess = sess;
        ahs->spec = ahc;
        
        if (strcmp(ne_get_scheme(sess), "https") == 0) {
            ahs->context = isproxy ? AUTH_CONNECT : AUTH_NOTCONNECT;
        } else {
            ahs->context = AUTH_ANY;
        }
        
        /* Register hooks */
        ne_hook_create_request(sess, ah_create, ahs);
        ne_hook_pre_send(sess, ah_pre_send, ahs);
        ne_hook_post_send(sess, ah_post_send, ahs);
        ne_hook_destroy_request(sess, ah_destroy, ahs);
        ne_hook_destroy_session(sess, free_auth, ahs);
        
        ne_set_session_private(sess, id, ahs);
    }

#ifdef HAVE_GSSAPI
    if ((protomask & NE_AUTH_GSSAPI_ONLY) && ahs->gssname == GSS_C_NO_NAME) {
        ne_uri uri = {0};
        
        if (isproxy)
            ne_fill_proxy_uri(sess, &uri);
        else
            ne_fill_server_uri(sess, &uri);

        get_gss_name(&ahs->gssname, uri.host);

        ne_uri_free(&uri);
    }
#endif
#ifdef HAVE_SSPI
    if ((protomask & (NE_AUTH_NTLM|NE_AUTH_SSPI)) && !ahs->sspi_host) {
        ne_uri uri = {0};
        
        if (isproxy)
        {
            ne_fill_proxy_uri(sess, &uri);
        }
        else
        {
            ne_fill_server_uri(sess, &uri);
        }

        ahs->sspi_host = uri.host;

        uri.host = NULL;

        ne_uri_free(&uri);
    }
#endif        

    /* Find the end of the handler list, and add a new one. */
    hdl = &ahs->handlers;
    while (*hdl)
        hdl = &(*hdl)->next;
        
    *hdl = ne_malloc(sizeof **hdl);
    (*hdl)->protomask = protomask;
    (*hdl)->old_creds = old_creds;
    (*hdl)->new_creds = new_creds;
    (*hdl)->userdata = userdata;
    (*hdl)->next = NULL;
    (*hdl)->attempt = 0;
}

void ne_set_server_auth(ne_session *sess, ne_auth_creds creds, void *userdata)
{
    auth_register(sess, 0, NE_AUTH_DEFAULT, &ah_server_class, HOOK_SERVER_ID,
                  creds, NULL, userdata);
}

void ne_set_proxy_auth(ne_session *sess, ne_auth_creds creds, void *userdata)
{
    auth_register(sess, 1, NE_AUTH_DEFAULT, &ah_proxy_class, HOOK_PROXY_ID,
                  creds, NULL, userdata);
}

void ne_add_server_auth(ne_session *sess, unsigned protocol, 
                        ne_auth_creds creds, void *userdata)
{
    auth_register(sess, 0, protocol, &ah_server_class, HOOK_SERVER_ID,
                  creds, NULL, userdata);
}

void ne_add_proxy_auth(ne_session *sess, unsigned protocol, 
                       ne_auth_creds creds, void *userdata)
{
    auth_register(sess, 1, protocol, &ah_proxy_class, HOOK_PROXY_ID,
                  creds, NULL, userdata);
}

void ne_add_auth(ne_session *sess, unsigned protocol,
                 ne_auth_provide new_creds, void *userdata)
{
    auth_register(sess, 0, protocol, &ah_proxy_class, HOOK_PROXY_ID,
                  NULL, new_creds, userdata);
    auth_register(sess, 0, protocol, &ah_server_class, HOOK_SERVER_ID,
                  NULL, new_creds, userdata);
}

#ifdef WINSCP
void ne_remove_server_auth(ne_session *sess)
{
    auth_session *as;
    if ((as = ne_get_session_private(sess, HOOK_SERVER_ID)) != NULL)
    {
        // copied from free_auth
        struct auth_handler *hdl, *next;
        for (hdl = as->handlers; hdl; hdl = next) {
            next = hdl->next;
            ne_free(hdl);
        }
        as->handlers = NULL;
    }
}
#endif

void ne_forget_auth(ne_session *sess)
{
    auth_session *as;
    if ((as = ne_get_session_private(sess, HOOK_SERVER_ID)) != NULL)
	clean_session(as);
    if ((as = ne_get_session_private(sess, HOOK_PROXY_ID)) != NULL)
	clean_session(as);
}

