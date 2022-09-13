/* 
   Authentication tests
   Copyright (C) 2001-2009, Joe Orton <joe@manyfish.co.uk>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
  
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
  
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#include "config.h"

#include <sys/types.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "ne_string.h"
#include "ne_request.h"
#include "ne_auth.h"
#include "ne_basic.h"

#include "tests.h"
#include "child.h"
#include "utils.h"

static const char username[] = "Aladdin", password[] = "open sesame";
static const char *alt_username, *alt_username_star;

static int auth_failed;

static int has_sha256 = 0, has_sha512_256 = 0;

#define BASIC_WALLY "Basic realm=WallyWorld"
#define CHAL_WALLY "WWW-Authenticate: " BASIC_WALLY

#define EOL "\r\n"

static int auth_cb(void *userdata, const char *realm, int tries, 
		   char *un, char *pw)
{
    if (strcmp(realm, "WallyWorld")) {
        NE_DEBUG(NE_DBG_HTTP, "Got wrong realm '%s'!\n", realm);
        return -1;
    }    
    strcpy(un, userdata ? userdata : username);
    strcpy(pw, password);
    return tries;
}		   

static int auth_provide_cb(void *userdata, int attempt,
                           unsigned protocol, const char *realm,
                           char *un, char *pw, size_t buflen)
{
    if (strcmp(realm, "WallyWorld")) {
        NE_DEBUG(NE_DBG_HTTP, "Got wrong realm '%s'!\n", realm);
        return -1;
    }
    strcpy(un, alt_username);
    strcpy(pw, password);
    return attempt;
}

static void auth_hdr(char *value)
{
#define B "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="
    auth_failed = strcmp(value, B);
    NE_DEBUG(NE_DBG_HTTP, "Got auth header: [%s]\nWanted header:   [%s]\n"
	     "Result: %d\n", value, B, auth_failed);
#undef B
}

/* Sends a response with given response-code. If hdr is not NULL,
 * sends that header string too (appending an EOL).  If eoc is
 * non-zero, request must be last sent down a connection; otherwise,
 * clength 0 is sent to maintain a persistent connection. */
static int send_response(ne_socket *sock, const char *hdr, int code, int eoc)
{
    char buffer[BUFSIZ];
    
    sprintf(buffer, "HTTP/1.1 %d Blah Blah" EOL, code);
    
    if (hdr) {
	strcat(buffer, hdr);
	strcat(buffer, EOL);
    }

    if (eoc) {
	strcat(buffer, "Connection: close" EOL EOL);
    } else {
	strcat(buffer, "Content-Length: 0" EOL EOL);
    }
	
    return SEND_STRING(sock, buffer);
}

/* Server function which sends two responses: first requires auth,
 * second doesn't. */
static int auth_serve(ne_socket *sock, void *userdata)
{
    char *hdr = userdata;

    auth_failed = 1;

    /* Register globals for discard_request. */
    got_header = auth_hdr;
    want_header = "Authorization";

    discard_request(sock);
    send_response(sock, hdr, 401, 0);

    discard_request(sock);
    send_response(sock, NULL, auth_failed?500:200, 1);

    return 0;
}

static int init(void)
{
    char *p;

    p = ne_strhash(NE_HASH_SHA256, "", NULL);
    has_sha256 = p != NULL;
    if (p) ne_free(p);

    p = ne_strhash(NE_HASH_SHA512_256, "", NULL);
    has_sha512_256 = p != NULL;
    if (p) ne_free(p);

    return lookup_localhost();
}

/* Test that various Basic auth challenges are correctly handled. */
static int basic(void)
{
    const char *hdrs[] = {
        /* simplest case */
        CHAL_WALLY,

        /* several challenges, one header */
        "WWW-Authenticate: BarFooScheme, " BASIC_WALLY,

        /* several challenges, one header */
        CHAL_WALLY ", BarFooScheme realm=\"PenguinWorld\"",

        /* whitespace tests. */
        "WWW-Authenticate:   Basic realm=WallyWorld   ",

        /* nego test. */
        "WWW-Authenticate: Negotiate fish, Basic realm=WallyWorld",

        /* nego test. */
        "WWW-Authenticate: Negotiate fish, bar=boo, Basic realm=WallyWorld",

        /* nego test. */
        "WWW-Authenticate: Negotiate, Basic realm=WallyWorld",

        /* multi-header case 1 */
        "WWW-Authenticate: BarFooScheme\r\n"
        CHAL_WALLY,
        
        /* multi-header cases 1 */
        CHAL_WALLY "\r\n"
        "WWW-Authenticate: BarFooScheme bar=\"foo\"",

        /* multi-header case 3 */
        "WWW-Authenticate: FooBarChall foo=\"bar\"\r\n"
        CHAL_WALLY "\r\n"
        "WWW-Authenticate: BarFooScheme bar=\"foo\"",

        /* quoting test; fails to handle scheme properly with <= 0.28.2. */
        "WWW-Authenticate: Basic realm=\"WallyWorld\" , BarFooScheme"
    };
    size_t n;
    
    for (n = 0; n < sizeof(hdrs)/sizeof(hdrs[0]); n++) {
        ne_session *sess;
        
        CALL(make_session(&sess, auth_serve, (void *)hdrs[n]));
        ne_set_server_auth(sess, auth_cb, NULL);
        
        CALL(any_2xx_request(sess, "/norman"));
        
        ne_session_destroy(sess);
        CALL(await_server());
    }

    return OK;
}

static int retry_serve(ne_socket *sock, void *ud)
{
    discard_request(sock);
    send_response(sock, CHAL_WALLY, 401, 0);

    discard_request(sock);
    send_response(sock, CHAL_WALLY, 401, 0);

    discard_request(sock);
    send_response(sock, NULL, 200, 0);
    
    discard_request(sock);
    send_response(sock, CHAL_WALLY, 401, 0);

    discard_request(sock);
    send_response(sock, NULL, 200, 0);

    discard_request(sock);
    send_response(sock, NULL, 200, 0);

    discard_request(sock);
    send_response(sock, NULL, 200, 0);

    discard_request(sock);
    send_response(sock, CHAL_WALLY, 401, 0);

    discard_request(sock);
    send_response(sock, NULL, 200, 0);

    discard_request(sock);
    send_response(sock, CHAL_WALLY, 401, 0);

    discard_request(sock);
    send_response(sock, CHAL_WALLY, 401, 0);

    discard_request(sock);
    send_response(sock, CHAL_WALLY, 401, 0);

    discard_request(sock);
    send_response(sock, NULL, 200, 0);
    
    return OK;
}

static int retry_cb(void *userdata, const char *realm, int tries, 
		    char *un, char *pw)
{
    int *count = userdata;

    /* dummy creds; server ignores them anyway. */
    strcpy(un, "a");
    strcpy(pw, "b");

    switch (*count) {
    case 0:
    case 1:
	if (tries == *count) {
	    *count += 1;
	    return 0;
	} else {
	    t_context("On request #%d, got attempt #%d", *count, tries);
	    *count = -1;
	    return 1;
	}
	break;
    case 2:
    case 3:
	/* server fails a subsequent request, check that tries has
	 * reset to zero. */
	if (tries == 0) {
	    *count += 1;
	    return 0;
	} else {
	    t_context("On retry after failure #%d, tries was %d", 
		      *count, tries);
	    *count = -1;
	    return 1;
	}
	break;
    case 4:
    case 5:
	if (tries > 1) {
	    t_context("Attempt counter reached #%d", tries);
	    *count = -1;
	    return 1;
	}
	return tries;
    default:
	t_context("Count reached %d!?", *count);
	*count = -1;
    }
    return 1;
}

/* Test that auth retries are working correctly. */
static int retries(void)
{
    ne_session *sess;
    int count = 0;
    
    CALL(make_session(&sess, retry_serve, NULL));

    ne_set_server_auth(sess, retry_cb, &count);

    /* This request will be 401'ed twice, then succeed. */
    ONREQ(any_request(sess, "/foo"));

    /* auth_cb will have set up context. */
    CALL(count != 2);

    /* this request will be 401'ed once, then succeed. */
    ONREQ(any_request(sess, "/foo"));

    /* auth_cb will have set up context. */
    CALL(count != 3);

    /* some 20x requests. */
    ONREQ(any_request(sess, "/foo"));
    ONREQ(any_request(sess, "/foo"));

    /* this request will be 401'ed once, then succeed. */
    ONREQ(any_request(sess, "/foo"));

    /* auth_cb will have set up context. */
    CALL(count != 4);

    /* First request is 401'ed by the server at both attempts. */
    ONV(any_request(sess, "/foo") != NE_AUTH,
	("auth succeeded, should have failed: %s", ne_get_error(sess)));

    count++;

    /* Second request is 401'ed first time, then will succeed if
     * retried.  0.18.0 didn't reset the attempt counter though so 
     * this didn't work. */
    ONV(any_request(sess, "/foo") == NE_AUTH,
	("auth failed on second try, should have succeeded: %s", ne_get_error(sess)));

    return destroy_and_wait(sess);
}

/* crashes with neon <0.22 */
static int forget_regress(void)
{
    ne_session *sess = ne_session_create("http", "localhost", 1234);
    ne_forget_auth(sess);
    ne_session_destroy(sess);
    return OK;    
}

static int fail_auth_cb(void *ud, const char *realm, int attempt, 
			char *un, char *pw)
{
    return 1;
}

/* this may trigger a segfault in neon 0.21.x and earlier. */
static int tunnel_regress(void)
{
    ne_session *sess;
    
    CALL(proxied_session_server(&sess, "http", "localhost", 443,
                                single_serve_string,
                                "HTTP/1.1 401 Auth failed.\r\n"
                                "WWW-Authenticate: Basic realm=asda\r\n"
                                "Content-Length: 0\r\n\r\n"));
    ne_set_server_auth(sess, fail_auth_cb, NULL);
    any_request(sess, "/foo");

    return destroy_and_wait(sess);
}

/* regression test for parsing a Negotiate challenge with on parameter
 * token. */
static int negotiate_regress(void)
{
    ne_session *sess;
    
    CALL(session_server(&sess, single_serve_string,
                        "HTTP/1.1 401 Auth failed.\r\n"
                        "WWW-Authenticate: Negotiate\r\n"
                        "Content-Length: 0\r\n\r\n"));
    ne_set_server_auth(sess, fail_auth_cb, NULL);
    any_request(sess, "/foo");

    return destroy_and_wait(sess);
}

static char *digest_hdr = NULL;

static void dup_header(char *header)
{
    if (digest_hdr) ne_free(digest_hdr);
    digest_hdr = ne_strdup(header);
}

#define PARM_PROXY     (0x0001)
#define PARM_NEXTNONCE (0x0002)
#define PARM_ALG       (0x0004) /* use algorithm= */
#define PARM_AINFO     (0x0008)
#define PARM_USERHASH  (0x0010) /* userhash=true */
#define PARM_UHFALSE   (0x0020) /* userhash=false */
#define PARM_ALTUSER   (0x0040)
#define PARM_LEGACY      (0x0080)
#define PARM_LEGACY_ONLY (0x0100)
#define PARM_QOP       (0x0200) /* use qop= */
#define PARM_RFC2617   (0x0204) /* use algorithm= and qop= */

struct digest_parms {
    const char *realm, *nonce, *opaque, *domain;
    enum { ALG_MD5 = 0, ALG_MD5_SESS, ALG_SHA256, ALG_SHA256_SESS, ALG_SHA512_256, ALG_SHA512_256_SESS } alg;
    unsigned int flags;
    int num_requests;
    int stale;
    enum digest_failure {
        fail_not,
        fail_bogus_alg,
        fail_req0_stale,
        fail_req0_2069_stale,
        fail_omit_qop,
        fail_omit_realm,
        fail_omit_nonce,
        fail_ai_bad_nc,
        fail_ai_bad_nc_syntax,
        fail_ai_bad_digest,
        fail_ai_bad_cnonce,
        fail_ai_omit_cnonce,
        fail_ai_omit_digest,
        fail_ai_omit_nc,
        fail_outside_domain,
        fail_2069_weak
    } failure;
};

struct digest_state {
    const char *realm, *nonce, *uri, *username, *username_star, *password, *algorithm, *qop,
        *method, *opaque;
    char userhash[64];
    char *cnonce, *digest, *ncval;
    long nc;
    int count;
    int uhash_bool;
};

static char *hash(struct digest_parms *p, ...)
    ne_attribute_sentinel;

static char *hash(struct digest_parms *p, ...)
{
    va_list ap;
    unsigned int flags;
    char *h;

    switch (p->alg) {
    case ALG_SHA512_256_SESS:
    case ALG_SHA512_256:
        flags = NE_HASH_SHA512_256;
        break;
    case ALG_SHA256_SESS:
    case ALG_SHA256:
        flags = NE_HASH_SHA256;
        break;
    default:
        flags = NE_HASH_MD5;
        break;
    }

    va_start(ap, p);
    h = ne_vstrhash(flags, ap);
    va_end(ap);

    if (h == NULL) abort();

    return h;
}

/* Write the request-digest into 'digest' (or response-digest if
 * auth_info is non-zero) for given digest auth state and
 * parameters.  */
static char *make_digest(struct digest_state *state, struct digest_parms *parms,
                         int auth_info)
{
    char *h_a1, *h_a2, *rv;

    h_a1 = hash(parms, state->username, ":", state->realm, ":", 
                state->password, NULL);

    if (parms->alg == ALG_MD5_SESS || parms->alg == ALG_SHA256_SESS || parms->alg == ALG_SHA512_256_SESS) {
        char *sess_h_a1;

        sess_h_a1 = hash(parms, h_a1, ":", state->nonce, ":", state->cnonce, NULL);
        ne_free(h_a1);
        h_a1 = sess_h_a1;
    }

    h_a2 = hash(parms, !auth_info ? state->method : "", ":", state->uri, NULL);

    if (parms->flags & PARM_QOP) {
        rv = hash(parms,
                  h_a1, ":", state->nonce, ":",
                  state->ncval, ":", state->cnonce, ":", state->qop, ":",
                  h_a2, NULL);
    }
    else {
        /* RFC2069-style */
        rv = hash(parms, h_a1, ":", state->nonce, ":", h_a2, NULL);
    }

    ne_free(h_a2);
    ne_free(h_a1);

    return rv;
}

/* Verify that the response-digest matches expected state. */
static int check_digest(struct digest_state *state, struct digest_parms *parms)
{
    char *digest;

    digest = make_digest(state, parms, 0);
    ONV(digest == NULL,
        ("failed to create digest for %s", state->algorithm));

    ONV(strcmp(digest, state->digest),
        ("bad digest; expected %s got %s", state->digest, digest));

    ne_free(digest);

    return OK;
}

#define DIGCMP(field)                                   \
    do {                                                \
        ONCMP(state->field, newstate.field,            \
              "Digest response header", #field);        \
    } while (0)

#define NPARAM(field, param)                                    \
    do {                                                        \
        if (ne_strcasecmp(name, param) == 0) {                  \
            ONV(newstate.field != NULL,                         \
                ("received multiple %s params: %s, %s", param,  \
                 newstate.field, val));                         \
            newstate.field = val;                               \
        }                                                       \
    } while (0)
#define PARAM(field) NPARAM(field, #field)

/* Verify that Digest auth request header, 'header', meets expected
 * state and parameters. */
static int verify_digest_header(struct digest_state *state, 
                                struct digest_parms *parms,
                                char *header)
{
    char *ptr;
    struct digest_state newstate = {0};

    ptr = ne_token(&header, ' ');

    ONCMP("Digest", ptr, "Digest response", "scheme name");

    while (header) {
        char *name, *val;

        ptr = ne_qtoken(&header, ',', "\"\'");
        ONN("quoting broken", ptr == NULL);

        name = ne_shave(ptr, " ");

        val = strchr(name, '=');
        ONV(val == NULL, ("bad name/value pair: %s", name));
        
        *val++ = '\0';

        val = ne_shave(val, "\"\' ");

        NE_DEBUG(NE_DBG_HTTP, "got field: [%s] = [%s]\n", name, val);

        PARAM(uri);
        PARAM(realm);
        PARAM(username);
        PARAM(nonce);
        PARAM(algorithm);
        PARAM(qop);
        PARAM(opaque);
        PARAM(cnonce);
        NPARAM(username_star, "username*");

        if (ne_strcasecmp(name, "nc") == 0) {
            long nc = strtol(val, NULL, 16);
            
            ONV(nc != state->nc, 
                ("got bad nonce count: %ld (%s) not %ld", 
                 nc, val, state->nc));

            state->ncval = ne_strdup(val);
        }
        else if (ne_strcasecmp(name, "response") == 0) {
            state->digest = ne_strdup(val);
        }
        else if (ne_strcasecmp(name, "userhash") == 0 ) {
            newstate.uhash_bool = strcmp(val, "true") == 0;
        }
    }

    ONN("cnonce param missing or short for 2617-style auth",
        (parms->flags & PARM_QOP)
        && (newstate.cnonce == NULL
            || strlen(newstate.cnonce) < 32));

    if (alt_username_star) {
        ONN("unexpected userhash=true sent", newstate.uhash_bool);
        ONN("username* missing", newstate.username_star == NULL);
        ONCMP(alt_username_star, newstate.username_star, "Digest field", "username*");
    }
    else if (parms->flags & PARM_USERHASH) {
        ONN("userhash missing", !newstate.uhash_bool);

        ONCMP(state->userhash, newstate.username,
              "Digest username (userhash) field", "userhash");
    }
    else {
        ONN("unexpected userhash=true sent", newstate.uhash_bool);
        DIGCMP(username);
    }

    DIGCMP(realm);
    if (!parms->domain)
        DIGCMP(uri);
    DIGCMP(nonce);
    DIGCMP(opaque);
    DIGCMP(algorithm);

    if (parms->flags & PARM_QOP) {
        DIGCMP(qop);
    }
        
    if (newstate.cnonce) {
        state->cnonce = ne_strdup(newstate.cnonce);
    }
    if (parms->domain) {
        state->uri = ne_strdup(newstate.uri);
    }

    ONN("no digest param given", !state->digest);

    CALL(check_digest(state, parms));

    state->nc++;
    
    return OK;
}

static char *make_authinfo_header(struct digest_state *state,
                                  struct digest_parms *parms)
{
    ne_buffer *buf = ne_buffer_create();
    char *digest, *ncval, *cnonce;

    if (parms->failure == fail_ai_bad_digest) {
        digest = ne_strdup("fish");
    } else {
        digest = make_digest(state, parms, 1);
    }

    if (parms->failure == fail_ai_bad_nc_syntax) {
        ncval = "zztop";
    } else if (parms->failure == fail_ai_bad_nc) {
        ncval = "999";
    } else {
        ncval = state->ncval;
    }

    if (parms->failure == fail_ai_bad_cnonce) {
        cnonce = "another-fish";
    } else {
        cnonce = state->cnonce;
    }

    if ((parms->flags & PARM_PROXY)) {
        ne_buffer_czappend(buf, "Proxy-");
    }

    ne_buffer_czappend(buf, "Authentication-Info: ");

    if ((parms->flags & PARM_QOP) == 0) {
        ne_buffer_concat(buf, "rspauth=\"", digest, "\"", NULL);
    } else {
        if (parms->failure != fail_ai_omit_nc) {
            ne_buffer_concat(buf, "nc=", ncval, ", ", NULL);
        }
        if (parms->failure != fail_ai_omit_cnonce) {
            ne_buffer_concat(buf, "cnonce=\"", cnonce, "\", ", NULL);
        } 
        if (parms->failure != fail_ai_omit_digest) {
            ne_buffer_concat(buf, "rspauth=\"", digest, "\", ", NULL);
        }
        if (parms->flags & PARM_NEXTNONCE) {
            state->nonce = ne_concat("next-", state->nonce, NULL);
            ne_buffer_concat(buf, "nextnonce=\"", state->nonce, "\", ", NULL);
            state->nc = 1;
        }
        ne_buffer_czappend(buf, "qop=\"auth\"");
    }

    ne_free(digest);
    
    return ne_buffer_finish(buf);
}

static char *make_digest_header(struct digest_state *state,
                                struct digest_parms *parms)
{
    ne_buffer *buf = ne_buffer_create();
    const char *algorithm;

    algorithm = parms->failure == fail_bogus_alg ? "fish" 
        : state->algorithm;

    ne_buffer_concat(buf, 
                     (parms->flags & PARM_PROXY) ? "Proxy-Authenticate"
                     : "WWW-Authenticate",
                     ": Digest "
                     "realm=\"", parms->realm, "\", ", NULL);
    
    if (parms->flags & PARM_ALG) {
        ne_buffer_concat(buf, "algorithm=\"", algorithm, "\", ", NULL);
    }

    if (parms->flags & PARM_QOP) {
        ne_buffer_concat(buf, "qop=\"", state->qop, "\", ", NULL);
    }

    if (parms->opaque) {
        ne_buffer_concat(buf, "opaque=\"", parms->opaque, "\", ", NULL);
    }

    if (parms->domain) {
        ne_buffer_concat(buf, "domain=\"", parms->domain, "\", ", NULL);
    }

    if (parms->flags & PARM_USERHASH) {
        ne_buffer_czappend(buf, "userhash=true, ");
    }
    else if (parms->flags & PARM_UHFALSE) {
        ne_buffer_czappend(buf, "userhash=false, ");
    }

    if (parms->failure == fail_req0_stale
        || parms->failure == fail_req0_2069_stale
        || parms->stale == parms->num_requests) {
        ne_buffer_concat(buf, "stale='true', ", NULL);
    }

    ne_buffer_concat(buf, "nonce=\"", state->nonce, "\"", NULL);

    return ne_buffer_finish(buf);
}

/* Server process for Digest auth handling. */
static int serve_digest(ne_socket *sock, void *userdata)
{
    struct digest_parms *parms = userdata;
    struct digest_state state;
    char resp[NE_BUFSIZ], *rspdigest;
    
    if ((parms->flags & PARM_PROXY))
        state.uri = "http://www.example.com/fish";
    else if (parms->domain)
        state.uri = "/fish/0";
    else
        state.uri = "/fish";
    state.method = "GET";
    state.realm = parms->realm;
    state.nonce = parms->nonce;
    state.opaque = parms->opaque;
    if (parms->flags & PARM_ALTUSER)
        state.username = alt_username;
    else
        state.username = username;
    state.password = password;
    state.nc = 1;
    switch (parms->alg) {
    case ALG_SHA512_256: state.algorithm = "SHA-512-256"; break;
    case ALG_SHA512_256_SESS: state.algorithm = "SHA-512-256-sess"; break;
    case ALG_SHA256: state.algorithm = "SHA-256"; break;
    case ALG_SHA256_SESS: state.algorithm = "SHA-256-sess"; break;
    case ALG_MD5_SESS: state.algorithm = "MD5-sess"; break;
    default:
    case ALG_MD5: state.algorithm = "MD5"; break;
    }
    state.qop = "auth";

    if (parms->flags & PARM_USERHASH) {
        char *uh = hash(parms, username, ":", parms->realm, NULL);

        ONN("userhash too long", strlen(uh) >= sizeof state.userhash);

        ne_strnzcpy(state.userhash, uh, sizeof state.userhash);
        ne_free(uh);
    }

    state.cnonce = state.digest = state.ncval = NULL;

    parms->num_requests += parms->stale ? 1 : 0;

    NE_DEBUG(NE_DBG_HTTP, ">>>> Response sequence begins, %d requests.\n",
             parms->num_requests);

    want_header = (parms->flags & PARM_PROXY) ? "Proxy-Authorization" : "Authorization";
    digest_hdr = NULL;
    got_header = dup_header;

    CALL(discard_request(sock));

    ONV(digest_hdr != NULL,
        ("got unwarranted WWW-Auth header: %s", digest_hdr));

    rspdigest = make_digest_header(&state, parms);
    ne_snprintf(resp, sizeof resp,
                "HTTP/1.1 %d Auth Denied\r\n"
                "%s\r\n"
                "Content-Length: 0\r\n" "\r\n",
                (parms->flags & PARM_PROXY) ? 407 : 401,
                rspdigest);
    ne_free(rspdigest);

    SEND_STRING(sock, resp);

    /* Give up now if we've sent a challenge which should force the
     * client to fail immediately: */
    if (parms->failure == fail_bogus_alg
        || parms->failure == fail_req0_stale
        || parms->failure == fail_req0_2069_stale) {
        return OK;
    }

    do {
        digest_hdr = NULL;
        CALL(discard_request(sock));

        if (digest_hdr && parms->domain && (parms->num_requests & 1) != 0) {
            SEND_STRING(sock, "HTTP/1.1 400 Used Auth Outside Domain\r\n\r\n");
            return OK;
        }
        else if (digest_hdr == NULL && parms->domain && (parms->num_requests & 1) != 0) {
            /* Do nothing. */
            NE_DEBUG(NE_DBG_HTTP, "No Authorization header sent, good.\n");
        }
        else {
            ONN("no Authorization header sent", digest_hdr == NULL);

            ONERR(sock, verify_digest_header(&state, parms, digest_hdr));
        }

        if (parms->num_requests == parms->stale) {
            char *dig;
            state.nonce = ne_concat("stale-", state.nonce, NULL);
            state.nc = 1;

            dig = make_digest_header(&state, parms);
            ne_snprintf(resp, sizeof resp,
                        "HTTP/1.1 %d Auth Denied\r\n"
                        "%s\r\n"
                        "Content-Length: 0\r\n" "\r\n",
                        (parms->flags & PARM_PROXY) ? 407 : 401,
                        dig);
            ne_free(dig);
        }
        else if (parms->flags & PARM_AINFO) {
            char *ai = make_authinfo_header(&state, parms);
            
            ne_snprintf(resp, sizeof resp,
                        "HTTP/1.1 200 Well, if you insist\r\n"
                        "Content-Length: 0\r\n"
                        "%s\r\n"
                        "\r\n", ai);
            
            ne_free(ai);
        } else {
            ne_snprintf(resp, sizeof resp,
                        "HTTP/1.1 200 You did good\r\n"
                        "Content-Length: 0\r\n" "\r\n");
        }

        SEND_STRING(sock, resp);

        NE_DEBUG(NE_DBG_HTTP, "Handled request; %d requests remain.\n",
                 parms->num_requests - 1);
    } while (--parms->num_requests);

    return OK;
}

static int test_digest(struct digest_parms *parms)
{
    ne_session *sess;
    unsigned proto = NE_AUTH_DIGEST;

    if ((parms->flags & PARM_LEGACY))
        proto |= NE_AUTH_LEGACY_DIGEST;
    else if ((parms->flags & PARM_LEGACY_ONLY))
        proto = NE_AUTH_LEGACY_DIGEST;

    NE_DEBUG(NE_DBG_HTTP, ">>>> Request sequence begins "
             "(reqs=%d, nonce=%s, rfc=%s, stale=%d, proxy=%d).\n",
             parms->num_requests,
             parms->nonce, (parms->flags & PARM_QOP) ? "2617" : "2069",
             parms->stale, !!(parms->flags & PARM_PROXY));

    if ((parms->flags & PARM_PROXY)) {
        CALL(proxied_session_server(&sess, "http", "www.example.com", 80,
                                    serve_digest, parms));
        ne_set_proxy_auth(sess, auth_cb, NULL);
    } 
    else {
        CALL(session_server(&sess, serve_digest, parms));
        if ((parms->flags & PARM_ALTUSER))
            ne_add_auth(sess, proto, auth_provide_cb, NULL);
        else
            ne_add_server_auth(sess, proto, auth_cb, NULL);
    }

    do {
        CALL(any_2xx_request(sess, "/fish"));
    } while (--parms->num_requests);
    
    return destroy_and_wait(sess);
}

/* Test for RFC2617-style Digest auth. */
static int digest(void)
{
    struct digest_parms parms[] = {
        /* RFC 2617-style */
        { "WallyWorld", "this-is-a-nonce", NULL, NULL, ALG_MD5, PARM_RFC2617, 1, 0, fail_not },
        /* Leaving algorithm= optional. */
        { "WallyWorld", "this-is-a-nonce", NULL, NULL, ALG_MD5, PARM_QOP, 1, 0, fail_not },
        { "WallyWorld", "this-is-also-a-nonce", "opaque-string", NULL, ALG_MD5, PARM_RFC2617, 1, 0, fail_not },
        /* ... with A-I */
        { "WallyWorld", "nonce-nonce-nonce", "opaque-string", NULL, ALG_MD5, PARM_RFC2617 | PARM_AINFO, 1, 0, fail_not },
        /* ... with md5-sess. */
        { "WallyWorld", "nonce-nonce-nonce", "opaque-string", NULL, ALG_MD5_SESS, PARM_RFC2617 | PARM_AINFO, 1, 0, fail_not },
        /* many requests, with changing nonces; tests for next-nonce handling bug. */
        { "WallyWorld", "this-is-a-nonce", "opaque-thingy", NULL, ALG_MD5, PARM_RFC2617 | PARM_AINFO | PARM_NEXTNONCE, 20, 0, fail_not },

        /* staleness. */
        { "WallyWorld", "this-is-a-nonce", "opaque-thingy", NULL, ALG_MD5, PARM_RFC2617 | PARM_AINFO, 3, 2, fail_not },
        /* 2069 + stale */
        { "WallyWorld", "this-is-a-nonce", NULL, NULL, ALG_MD5, PARM_LEGACY|PARM_AINFO, 3, 2, fail_not },

        /* RFC 7616-style */
        { "WallyWorld", "new-day-new-nonce", "new-opaque", NULL, ALG_MD5, PARM_RFC2617 | PARM_USERHASH, 1, 0, fail_not },
        /* ... userhash=false */
        { "WallyWorld", "just-another-nonce", "new-opaque", NULL, ALG_MD5, PARM_RFC2617 | PARM_UHFALSE, 1, 0, fail_not },

        /* RFC 2069-style */ 
        { "WallyWorld", "lah-di-da-di-dah", NULL, NULL, ALG_MD5, PARM_LEGACY, 1, 0, fail_not },
        { "WallyWorld", "lah-lah-lah-lah", NULL, NULL, ALG_MD5, PARM_LEGACY_ONLY, 1, 0, fail_not },
        { "WallyWorld", "fee-fi-fo-fum", "opaque-string", NULL, ALG_MD5, PARM_LEGACY, 1, 0, fail_not },
        { "WallyWorld", "fee-fi-fo-fum", "opaque-string", NULL, ALG_MD5, PARM_AINFO|PARM_LEGACY, 1, 0, fail_not },

        /* Proxy auth */
        { "WallyWorld", "this-is-also-a-nonce", "opaque-string", NULL, ALG_MD5, PARM_RFC2617|PARM_PROXY, 1, 0, fail_not },
        /* Proxy + nextnonce */
        { "WallyWorld", "this-is-also-a-nonce", "opaque-string", NULL, ALG_MD5, PARM_RFC2617|PARM_AINFO|PARM_PROXY, 1, 0, fail_not },

        { NULL }
    };
    size_t n;

    for (n = 0; parms[n].realm; n++) {
        CALL(test_digest(&parms[n]));

    }

    return OK;
}

static int digest_sha256(void)
{
    struct digest_parms parms[] = {
        { "WallyWorld", "nonce-sha-nonce", "opaque-string", NULL, ALG_SHA256, PARM_RFC2617, 1, 0, fail_not },
        { "WallyWorld", "nonce-sha-nonce", "opaque-string", NULL, ALG_SHA256, PARM_RFC2617|PARM_AINFO, 1, 0, fail_not },
        { "WallyWorld", "nonce-sha-session", "opaque-string", NULL, ALG_SHA256_SESS, PARM_RFC2617|PARM_AINFO, 1, 0, fail_not },
        { "WallyWorld", "nonce-sha-nonce", "opaque-string", NULL, ALG_SHA256, PARM_RFC2617|PARM_AINFO, 8, 0, fail_not },
        
        { NULL },
    };
    size_t n;

    if (!has_sha256) {
        t_context("SHA-256 not supported");
        return SKIP;
    }

    for (n = 0; parms[n].realm; n++) {
        CALL(test_digest(&parms[n]));

    }

    return OK;
}

static int digest_sha512_256(void)
{
    struct digest_parms parms[] = {
        { "WallyWorld", "nonce-sha5-nonce", "opaque-string", NULL, ALG_SHA512_256, PARM_RFC2617, 1, 0, fail_not },
        { "WallyWorld", "nonce-sha5-nonce", "opaque-string", NULL, ALG_SHA512_256, PARM_RFC2617|PARM_AINFO, 1, 0, fail_not },
        { "WallyWorld", "nonce-sha5-session", "opaque-string", NULL, ALG_SHA512_256_SESS, PARM_RFC2617|PARM_AINFO, 1, 0, fail_not },
        { "WallyWorld", "nonce-sha-nonce", "opaque-string", NULL, ALG_SHA512_256_SESS, PARM_RFC2617|PARM_AINFO, 20, 0, fail_not },
        { NULL },
    };
    size_t n;

    if (!has_sha512_256) {
        t_context("SHA-512/256 not supported");
        return SKIP;
    }

    for (n = 0; parms[n].realm; n++) {
        CALL(test_digest(&parms[n]));

    }

    return OK;
}

static int digest_username_star(void)
{
    static const struct {
        const char *username_raw, *username_star;
    } ts[] = {
        { "Aladdin", NULL },
        { "aladdin@cave.example.com", NULL },
        { "foo bar", NULL },
        { "Ałâddín", "UTF-8''A%c5%82%c3%a2dd%c3%adn" },
        { "Jäsøn Doe", "UTF-8''J%c3%a4s%c3%b8n%20Doe" },
        { "foo\"bar", "UTF-8''foo%22bar" },
        { NULL, NULL }
    };
    unsigned n;
    int ret = OK;

    for (n = 0; ret == OK && ts[n].username_raw; n++) {
        struct digest_parms parms = {
            "WallyWorld", "nonce-sha5-nonce", "opaque-string",
            NULL, ALG_MD5, PARM_RFC2617|PARM_UHFALSE|PARM_ALTUSER, 1, 0, fail_not };

        alt_username = ts[n].username_raw;
        alt_username_star = ts[n].username_star;

        ret = test_digest(&parms);
    }

    alt_username = NULL;
    alt_username_star = NULL;

    return ret;
}


static int digest_failures(void)
{
    struct digest_parms parms;
    static const struct {
        enum digest_failure mode;
        const char *message;
    } fails[] = {
        { fail_ai_bad_nc, "nonce count mismatch" },
        { fail_ai_bad_nc_syntax, "could not parse nonce count" },
        { fail_ai_bad_digest, "digest mismatch" },
        { fail_ai_bad_cnonce, "client nonce mismatch" },
        { fail_ai_omit_nc, "missing parameters" },
        { fail_ai_omit_digest, "missing parameters" },
        { fail_ai_omit_cnonce, "missing parameters" },
        { fail_bogus_alg, "unknown algorithm" },
        { fail_req0_stale, "initial Digest challenge was stale" },
        { fail_req0_2069_stale, "initial Digest challenge was stale" },
        { fail_2069_weak, "legacy Digest challenge not supported" },
        { fail_not, NULL }
    };
    unsigned n;

    memset(&parms, 0, sizeof parms);
    
    parms.realm = "WallyWorld";
    parms.nonce = "random-invented-string";
    parms.opaque = NULL;
    parms.num_requests = 1;

    for (n = 0; fails[n].message; n++) {
        ne_session *sess;
        int ret;
        unsigned protocol = NE_AUTH_DIGEST;

        parms.failure = fails[n].mode;
        parms.flags = PARM_AINFO;

        if (parms.failure == fail_req0_2069_stale) protocol |= NE_AUTH_LEGACY_DIGEST;

        if (parms.failure == fail_req0_2069_stale || parms.failure == fail_2069_weak)
            parms.flags &= ~PARM_RFC2617;
        else
            parms.flags |= PARM_RFC2617;

        NE_DEBUG(NE_DBG_HTTP, ">>> New Digest failure test %u, "
                 "expecting failure '%s', protocol %x\n", n,
                 fails[n].message, protocol);
        
        CALL(session_server(&sess, serve_digest, &parms));

        ne_add_server_auth(sess, protocol, auth_cb, NULL);
        
        ret = any_2xx_request(sess, "/fish");
        ONV(ret == NE_OK,
            ("request success (iter %u); expecting error '%s'",
             n, fails[n].message));

        ONV(strstr(ne_get_error(sess), fails[n].message) == NULL,
            ("request fails with error '%s'; expecting '%s'",
             ne_get_error(sess), fails[n].message));

        ne_session_destroy(sess);
        
        if (fails[n].mode == fail_bogus_alg
            || fails[n].mode == fail_req0_stale
            || fails[n].mode == fail_2069_weak) {
            reap_server();
        } else {
            CALL(await_server());
        }
    }

    return OK;
}

static int fail_cb(void *userdata, const char *realm, int tries, 
		   char *un, char *pw)
{
    ne_buffer *buf = userdata;
    char str[64];

    if (strcmp(realm, "colonic") == 0 && ne_buffer_size(buf) == 0) {
        ne_strnzcpy(un, "user:name", NE_ABUFSIZ);
        ne_strnzcpy(pw, "passwerd", NE_ABUFSIZ);
        return 0;
    }

    ne_snprintf(str, sizeof str, "<%s, %d>", realm, tries);
    ne_buffer_zappend(buf, str);

    return -1;
}

static int fail_challenge(void)
{
    static const struct {
        const char *resp, *error, *challs;
    } ts[] = {
        /* only possible Basic parse failure. */
        { "Basic", "missing realm in Basic challenge" },

        { "Basic realm=\"colonic\"", "username containing colon" },

        /* Digest parameter invalid/omitted failure cases: */
        { "Digest algorithm=MD5, qop=auth, nonce=\"foo\"",
          "missing parameter in Digest challenge" },
        { "Digest algorithm=MD5, qop=auth, realm=\"foo\"",
          "missing parameter in Digest challenge" },
        { "Digest algorithm=ZEBEDEE-GOES-BOING, qop=auth, realm=\"foo\"",
          "unknown algorithm in Digest challenge" },
        { "Digest algorithm=MD5-sess, realm=\"foo\"",
          "incompatible algorithm in Digest challenge" },
        { "Digest algorithm=MD5, qop=auth, nonce=\"foo\", realm=\"foo\", "
          "domain=\"http://[::1/\"", "could not parse domain" },

        /* Multiple challenge failure cases: */
        { "Basic, Digest realm=\"foo\", algorithm=MD5, qop=auth",
          "missing parameter in Digest challenge, missing realm in Basic challenge" },
        
        { "Digest realm=\"foo\", algorithm=MD5, qop=auth, nonce=\"foo\","
          " Basic realm=\"foo\"",
          "rejected Digest challenge, rejected Basic challenge" },

        { "WhizzBangAuth realm=\"foo\", " 
          "Basic realm='foo'",
          "ignored WhizzBangAuth challenge, rejected Basic challenge" },
        { "", "could not parse challenge" },

        /* neon 0.26.x regression in "attempt" handling. */
        { "Basic realm=\"foo\", " 
          "Digest realm=\"bar\", algorithm=MD5, qop=auth, nonce=\"foo\"",
          "rejected Digest challenge, rejected Basic challenge"
          , "<bar, 0><foo, 1>"  /* Digest challenge first, Basic second. */
        }
    };
    unsigned n;
    
    for (n = 0; n < sizeof(ts)/sizeof(ts[0]); n++) {
        char resp[512];
        ne_session *sess;
        int ret;
        ne_buffer *buf = ne_buffer_create();

        ne_snprintf(resp, sizeof resp,
                    "HTTP/1.1 401 Auth Denied\r\n"
                    "WWW-Authenticate: %s\r\n"
                    "Content-Length: 0\r\n" "\r\n",
                    ts[n].resp);
        
        CALL(multi_session_server(&sess, "http", "localhost",
                                  2, single_serve_string, resp));

        ne_add_server_auth(sess, NE_AUTH_ALL|NE_AUTH_LEGACY_DIGEST, fail_cb, buf);
        
        ret = any_2xx_request(sess, "/fish");
        ONV(ret == NE_OK,
            ("request success (iter %u); expecting error '%s'",
             n, ts[n].error));

        ONV(strstr(ne_get_error(sess), ts[n].error) == NULL,
            ("request fails with error '%s'; expecting '%s'",
             ne_get_error(sess), ts[n].error));
        
        if (ts[n].challs) {
            ONCMP(ts[n].challs, buf->data, "challenge callback", 
                  "invocation order");
        }

        ne_session_destroy(sess);
        ne_buffer_destroy(buf);
        reap_server();
    }

    return OK;
}

struct multi_context {
    int id;
    ne_buffer *buf;
};

static int multi_cb(void *userdata, const char *realm, int tries, 
                    char *un, char *pw)
{
    struct multi_context *ctx = userdata;

    ne_buffer_snprintf(ctx->buf, 128, "[id=%d, realm=%s, tries=%d]", 
                       ctx->id, realm, tries);

    return -1;
}

static int multi_handler(void)
{
    ne_session *sess;
    struct multi_context c[2];
    unsigned n;
    ne_buffer *buf = ne_buffer_create();

    CALL(make_session(&sess, single_serve_string,
                      "HTTP/1.1 401 Auth Denied\r\n"
                      "WWW-Authenticate: Basic realm='fish',"
                      " Digest realm='food', algorithm=MD5, qop=auth, nonce=gaga\r\n"
                      "Content-Length: 0\r\n" "\r\n"));
    
    for (n = 0; n < 2; n++) {
        c[n].buf = buf;
        c[n].id = n + 1;
    }

    ne_add_server_auth(sess, NE_AUTH_BASIC, multi_cb, &c[0]);
    ne_add_server_auth(sess, NE_AUTH_DIGEST, multi_cb, &c[1]);
    
    any_request(sess, "/fish");
    
    ONCMP("[id=2, realm=food, tries=0]"
          "[id=1, realm=fish, tries=0]", buf->data,
          "multiple callback", "invocation order");
    
    ne_buffer_destroy(buf);

    return destroy_and_wait(sess);
}

static int multi_rfc7616(void)
{
    ne_session *sess;
    struct multi_context c[2];
    unsigned n;
    ne_buffer *buf, *exp;

    buf = ne_buffer_create();
    CALL(make_session(&sess, single_serve_string,
                      "HTTP/1.1 401 Auth Denied\r\n"
                      "WWW-Authenticate: "
                      "Digest realm='sha512-realm', algorithm=SHA-512-256, qop=auth, nonce=gaga, "
                      "Basic realm='basic-realm', "
                      "Digest realm='md5-realm', algorithm=MD5, qop=auth, nonce=gaga, "
                      "Digest realm='sha256-realm', algorithm=SHA-256, qop=auth, nonce=gaga\r\n"
                      "Content-Length: 0\r\n" "\r\n"));

    for (n = 0; n < 2; n++) {
        c[n].buf = buf;
        c[n].id = n + 1;
    }

    ne_add_server_auth(sess, NE_AUTH_BASIC, multi_cb, &c[0]);
    ne_add_server_auth(sess, NE_AUTH_DIGEST, multi_cb, &c[1]);

    any_request(sess, "/fish");

    exp = ne_buffer_create();
    n = 0;
    if (has_sha512_256)
        ne_buffer_snprintf(exp, 100, "[id=2, realm=sha512-realm, tries=%u]", n++);
    if (has_sha256)
        ne_buffer_snprintf(exp, 100, "[id=2, realm=sha256-realm, tries=%u]", n++);
    ne_buffer_snprintf(exp, 100,
                       "[id=2, realm=md5-realm, tries=%u]"
                       "[id=1, realm=basic-realm, tries=0]", n);
    ONV(strcmp(exp->data, buf->data),
        ("unexpected callback ordering.\n"
         "expected: %s\n"
         "actual:   %s\n",
         exp->data, buf->data));

    ne_buffer_destroy(buf);
    ne_buffer_destroy(exp);

    return destroy_and_wait(sess);
}

static int multi_provider_cb(void *userdata, int attempt,
                             unsigned protocol, const char *realm,
                             char *un, char *pw, size_t buflen)
{
    ne_buffer *buf = userdata;
    const char *ctx;

    if (buflen == NE_ABUFSIZ) {
        NE_DEBUG(NE_DBG_HTTPAUTH, "auth: FAILED for short buffer length.\n");
        return -1;
    }

    if ((protocol & NE_AUTH_PROXY) == NE_AUTH_PROXY) {
        ctx = "proxy";
        protocol ^= NE_AUTH_PROXY;
    }
    else {
        ctx = "server";
    }

    ne_buffer_snprintf(buf, 128, "[%s: proto=%u, realm=%s, attempt=%d]",
                       ctx, protocol, realm, attempt);

    ne_strnzcpy(un, "foo", buflen);
    ne_strnzcpy(pw, "bar", buflen);

    return protocol == NE_AUTH_BASIC ? 0 : -1;
}

static int serve_provider(ne_socket *s, void *userdata)
{
    CALL(serve_response(s,
                        "HTTP/1.1 407 Proxy Auth Plz\r\n"
                        "Proxy-Authenticate: Basic realm='proxy-realm'\r\n"
                        "Content-Length: 0\r\n" "\r\n"));
    CALL(serve_response(s,
                        "HTTP/1.1 401 Auth Denied\r\n"
                        "WWW-Authenticate: "
                        "  Digest realm='sha512-realm', algorithm=SHA-512-256, qop=auth, nonce=gaga, "
                        "  Basic realm='basic-realm', "
                        "  Digest realm='md5-realm', algorithm=MD5, qop=auth, nonce=gaga, "
                        "  Digest realm='sha256-realm', algorithm=SHA-256, qop=auth, nonce=gaga\r\n"
                        "Content-Length: 0\r\n" "\r\n"));
    CALL(serve_response(s,
                        "HTTP/1.1 401 Auth Denied\r\n"
                        "WWW-Authenticate: "
                        "  Digest realm='sha512-realm', algorithm=SHA-512-256, qop=auth, nonce=gaga, "
                        "  Basic realm='basic-realm'\r\n"
                        "Content-Length: 0\r\n" "\r\n"));
    return serve_response(s,
                          "HTTP/1.1 200 OK\r\n"
                          "Content-Length: 0\r\n" "\r\n");
}

static int multi_provider(void)
{
    ne_session *sess;
    ne_buffer *buf = ne_buffer_create(), *exp;

    CALL(make_session(&sess, serve_provider, NULL));

    ne_add_auth(sess, NE_AUTH_DIGEST|NE_AUTH_BASIC, multi_provider_cb, buf);

    ONREQ(any_request(sess, "/fish"));

    exp = ne_buffer_create();
    ne_buffer_snprintf(exp, 100,
                       "[proxy: proto=%u, realm=proxy-realm, attempt=0]",
                       NE_AUTH_BASIC);
    if (has_sha512_256)
        ne_buffer_snprintf(exp, 100, "[server: proto=%u, realm=sha512-realm, attempt=0]",
                           NE_AUTH_DIGEST);
    if (has_sha256)
        ne_buffer_snprintf(exp, 100, "[server: proto=%u, realm=sha256-realm, attempt=0]",
                           NE_AUTH_DIGEST);
    ne_buffer_snprintf(exp, 100,
                       "[server: proto=%u, realm=md5-realm, attempt=0]"
                       "[server: proto=%u, realm=basic-realm, attempt=0]",
                       NE_AUTH_DIGEST, NE_AUTH_BASIC);

    if (has_sha512_256)
        ne_buffer_snprintf(exp, 100, "[server: proto=%u, realm=sha512-realm, attempt=1]",
                           NE_AUTH_DIGEST);
    ne_buffer_snprintf(exp, 100, "[server: proto=%u, realm=basic-realm, attempt=1]",
                       NE_AUTH_BASIC);

    ONV(strcmp(exp->data, buf->data),
        ("unexpected callback ordering.\n"
         "expected: %s\n"
         "actual:   %s\n",
         exp->data, buf->data));

    ne_buffer_destroy(buf);
    ne_buffer_destroy(exp);

    return destroy_and_wait(sess);
}


static int domains(void)
{
    ne_session *sess;
    struct digest_parms parms;

    memset(&parms, 0, sizeof parms);
    parms.realm = "WallyWorld";
    parms.flags = PARM_RFC2617;
    parms.nonce = "agoog";
    parms.domain = "http://localhost:4242/fish/ https://example.com /agaor /other";
    parms.num_requests = 6;

    CALL(proxied_session_server(&sess, "http", "localhost", 4242,
                                serve_digest, &parms));

    ne_set_server_auth(sess, auth_cb, NULL);

    CALL(any_2xx_request(sess, "/fish/0"));
    CALL(any_2xx_request(sess, "/outside"));
    CALL(any_2xx_request(sess, "/others"));
    CALL(any_2xx_request(sess, "/fish"));
    CALL(any_2xx_request(sess, "/fish/2"));
    CALL(any_2xx_request(sess, "*"));
    
    return destroy_and_wait(sess);
}

/* This segfaulted with 0.28.0 through 0.28.2 inclusive. */
static int CVE_2008_3746(void)
{
    ne_session *sess;
    struct digest_parms parms;

    memset(&parms, 0, sizeof parms);
    parms.realm = "WallyWorld";
    parms.flags = PARM_RFC2617;
    parms.nonce = "agoog";
    parms.domain = "foo";
    parms.num_requests = 1;

    CALL(proxied_session_server(&sess, "http", "www.example.com", 80,
                                serve_digest, &parms));

    ne_set_server_auth(sess, auth_cb, NULL);

    any_2xx_request(sess, "/fish/0");
    
    return destroy_and_wait(sess);
}

static int defaults(void)
{
    ne_session *sess;
    
    CALL(make_session(&sess, auth_serve, CHAL_WALLY));
    ne_add_server_auth(sess, NE_AUTH_DEFAULT, auth_cb, NULL);
    CALL(any_2xx_request(sess, "/norman"));
    ne_session_destroy(sess);
    CALL(await_server());

    CALL(make_session(&sess, auth_serve, CHAL_WALLY));
    ne_add_server_auth(sess, NE_AUTH_ALL, auth_cb, NULL);
    CALL(any_2xx_request(sess, "/norman"));

    return destroy_and_wait(sess);
}

static void fail_hdr(char *value)
{
    auth_failed = 1;
}

static int serve_forgotten(ne_socket *sock, void *userdata)
{
    auth_failed = 0;
    got_header = fail_hdr;
    want_header = "Authorization";
    
    CALL(discard_request(sock));
    if (auth_failed) {
        /* Should not get initial Auth header.  Eek. */
        send_response(sock, NULL, 403, 1);
        return 0;
    }
    send_response(sock, CHAL_WALLY, 401, 0);
    
    got_header = auth_hdr;
    CALL(discard_request(sock));
    if (auth_failed) {
        send_response(sock, NULL, 403, 1);
        return 0;
    }
    send_response(sock, NULL, 200, 0);
    
    ne_sock_read_timeout(sock, 5);

    /* Last time; should get no Auth header. */
    got_header = fail_hdr;
    CALL(discard_request(sock));
    send_response(sock, NULL, auth_failed ? 500 : 200, 1);
    
    return 0;                  
}

static int forget(void)
{
    ne_session *sess;

    CALL(make_session(&sess, serve_forgotten, NULL));

    ne_set_server_auth(sess, auth_cb, NULL);
    
    CALL(any_2xx_request(sess, "/norman"));
    
    ne_forget_auth(sess);

    CALL(any_2xx_request(sess, "/norman"));

    ne_session_destroy(sess);
    
    return await_server();
}

static int serve_basic_scope_checker(ne_socket *sock, void *userdata)
{
    /* --- GET /fish/0.txt -- first request */
    digest_hdr = NULL;
    got_header = dup_header;
    want_header = "Authorization";
    CALL(discard_request(sock));
    if (digest_hdr) {
        t_context("Got WWW-Auth header on initial request");
        return error_response(sock, FAIL);
    }

    send_response(sock, CHAL_WALLY, 401, 0);

    /* Retry of GET /fish/0 - expect Basic creds */
    auth_failed = 1;
    got_header = auth_hdr;
    CALL(discard_request(sock));
    if (auth_failed) {
        t_context("bad Basic Auth on first request");
        return error_response(sock, FAIL);
    }
    send_response(sock, CHAL_WALLY, 200, 0);
    
    /* --- GET /not/inside -- second request */
    got_header = dup_header;
    CALL(discard_request(sock));
    if (digest_hdr) {
        t_context("Basic auth sent outside of credentials scope");
        return error_response(sock, FAIL);
    }
    send_response(sock, CHAL_WALLY, 200, 0);

    /* --- GET /fish/1 -- third request */
    got_header = auth_hdr;
    CALL(discard_request(sock));
    send_response(sock, NULL, auth_failed?500:200, 1);
    
    return 0;    
}

/* Check that Basic auth follows the RFC7617 rules around scope. */
static int basic_scope(void)
{
    ne_session *sess;

    CALL(make_session(&sess, serve_basic_scope_checker, NULL));

    ne_set_server_auth(sess, auth_cb, NULL);
    
    CALL(any_2xx_request(sess, "/fish/0.txt")); /* must use auth */
    CALL(any_2xx_request(sess, "/not/inside")); /* must NOT use auth credentials */
    CALL(any_2xx_request(sess, "/fish/1")); /* must use auth credentials */

    return destroy_and_wait(sess);
}

/* Test for scope of "*" */
static int serve_star_scope_checker(ne_socket *sock, void *userdata)
{
    /* --- OPTIONS * -- first request */
    digest_hdr = NULL;
    got_header = dup_header;
    want_header = "Authorization";
    CALL(discard_request(sock));
    if (digest_hdr) {
        t_context("Got WWW-Auth header on initial request");
        return error_response(sock, FAIL);
    }

    send_response(sock, CHAL_WALLY, 401, 0);

    /* Retry of OPTIONS * - expect Basic creds */
    auth_failed = 1;
    got_header = auth_hdr;
    CALL(discard_request(sock));
    if (auth_failed) {
        t_context("No Basic Auth in OPTIONS request");
        return error_response(sock, FAIL);
    }
    send_response(sock, CHAL_WALLY, 200, 0);

    return 0;
}

/* Test for the scope of "*". */
static int star_scope(void)
{
    ne_session *sess;

    CALL(make_session(&sess, serve_star_scope_checker, NULL));

    ne_set_server_auth(sess, auth_cb, NULL);

    CALL(any_2xx_request_method(sess, "OPTIONS", "*")); /* must use auth */

    return destroy_and_wait(sess);
}

/* proxy auth, proxy AND origin */

ne_test tests[] = {
    T(init),
    T(basic),
    T(retries),
    T(forget_regress),
    T(tunnel_regress),
    T(negotiate_regress),
    T(digest),
    T(digest_sha256),
    T(digest_sha512_256),
    T(digest_failures),
    T(digest_username_star),
    T(fail_challenge),
    T(multi_handler),
    T(multi_rfc7616),
    T(multi_provider),
    T(domains),
    T(defaults),
    T(CVE_2008_3746),
    T(forget),
    T(basic_scope),
    T(star_scope),
    T(NULL)
};
