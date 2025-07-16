/* 
   neon SSL/TLS support using OpenSSL
   Copyright (C) 2002-2025, Joe Orton <joe@manyfish.co.uk>

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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <stdio.h>

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/pkcs12.h>
#include <openssl/x509v3.h>
#include <openssl/rand.h>
#include <openssl/opensslv.h>
#include <openssl/evp.h>

#if OPENSSL_VERSION_NUMBER >= 0x10100000L
#define HAVE_OPENSSL110
#endif

#if defined(NE_HAVE_TS_SSL) && !defined(HAVE_OPENSSL110)
/* From OpenSSL 1.1.0 locking callbacks are no longer needed. */
#define WITH_OPENSSL_LOCKING (1)
#include <stdlib.h> /* for abort() */
#ifndef _WIN32
#include <pthread.h>
#endif
#endif

#include "ne_ssl.h"
#include "ne_string.h"
#include "ne_session.h"
#include "ne_internal.h"
#include "ne_private.h"
#include "ne_privssl.h"

/* OpenSSL 0.9.6 compatibility */
#if OPENSSL_VERSION_NUMBER < 0x0090700fL
#define PKCS12_unpack_authsafes M_PKCS12_unpack_authsafes
#define PKCS12_unpack_p7data M_PKCS12_unpack_p7data
/* cast away lack of const-ness */
#define OBJ_cmp(a,b) OBJ_cmp((ASN1_OBJECT *)(a), (ASN1_OBJECT *)(b))
#endif

/* Second argument for d2i_X509() changed type in 0.9.8. */
#if OPENSSL_VERSION_NUMBER < 0x0090800fL
typedef unsigned char ne_d2i_uchar;
#else
typedef const unsigned char ne_d2i_uchar;
#endif

/* Convert a ASCII decimal pair into an integer. */
#define FROM_DEC(p_) (10*((p_)[0]-'0') + (p_)[1]-'0')

#ifndef HAVE_OPENSSL110
#define X509_get0_notBefore X509_get_notBefore
#define X509_get0_notAfter X509_get_notAfter
#define X509_up_ref(x) x->references++
#define EVP_PKEY_up_ref(x) x->references++
#define EVP_MD_CTX_new() ne_calloc(sizeof(EVP_MD_CTX))
#define EVP_MD_CTX_free(ctx) ne_free(ctx)
#define EVP_MD_CTX_reset EVP_MD_CTX_cleanup
#define EVP_PKEY_get0_RSA(evp) (evp->pkey.rsa)
#endif

#if OPENSSL_VERSION_NUMBER >= 0x10101000
/* OpenSSL 1.1.1 has STORE. */
#define HAVE_OPENSSL111
#include <openssl/store.h>
#include <openssl/ui.h>
#else
/* Backwards compatibility for <1.1.1. */
#define TLS_client_method SSLv23_client_method
#define TLS_server_method SSLv23_server_method
#endif

struct ne_ssl_dname_s {
    X509_NAME *dn;
};

struct ne_ssl_certificate_s {
    ne_ssl_dname subj_dn, issuer_dn;
    X509 *subject;
    ne_ssl_certificate *issuer;
    char *identity;
};

struct ne_ssl_client_cert_s {
    ne_ssl_certificate cert;
    /* .decrypt is non-NULL if the clicert is in the encrypted state,
     * and NULL in the decrypted state. */
    int (*decrypt)(ne_ssl_client_cert *cc, const char *password);
    PKCS12 *p12;
    char *uri;
    EVP_PKEY *pkey;
    char *friendly_name;
};

#define NE_SSL_UNHANDLED (0x20) /* failure bit for unhandled case. */

/* Append an ASN.1 DirectoryString STR to buffer BUF as UTF-8.
 * Returns zero on success or non-zero on error. */
static int append_dirstring(ne_buffer *buf, ASN1_STRING *str)
{
    unsigned char *tmp = (unsigned char *)""; /* initialize to workaround 0.9.6 bug */
    int len;

    switch (str->type) {
    case V_ASN1_IA5STRING: /* definitely ASCII */
    case V_ASN1_VISIBLESTRING: /* probably ASCII */
    case V_ASN1_PRINTABLESTRING: /* subset of ASCII */
        ne_buffer_qappend(buf, str->data, str->length);
        break;
    case V_ASN1_UTF8STRING:
        /* Fail for embedded NUL bytes. */
        if (strlen((char *)str->data) != (size_t)str->length) {
            return -1;
        }
        ne_buffer_append(buf, (char *)str->data, str->length);
        break;
    case V_ASN1_UNIVERSALSTRING:
    case V_ASN1_T61STRING: /* let OpenSSL convert it as ISO-8859-1 */
    case V_ASN1_BMPSTRING: 
        len = ASN1_STRING_to_UTF8(&tmp, str);
        if (len > 0) {
            /* Fail if there were embedded NUL bytes. */
            if (strlen((char *)tmp) != (size_t)len) {
                OPENSSL_free(tmp);
                return -1;
            } 
            else {
                ne_buffer_append(buf, (char *)tmp, len);
                OPENSSL_free(tmp);
            }
            break;
        } else {
            ERR_clear_error();
            return -1;
        }
        break;
    default:
        NE_DEBUG(NE_DBG_SSL, "Could not convert DirectoryString type %d\n",
                 str->type);
        return -1;
    }
    return 0;
}

/* Returns a malloc-allocated version of IA5 string AS, escaped for
 * safety. */
static char *dup_ia5string(const ASN1_IA5STRING *as)
{
    return ne_strnqdup(as->data, as->length);
}

char *ne_ssl_readable_dname(const ne_ssl_dname *name)
{
    int n, flag = 0;
    ne_buffer *dump = ne_buffer_create();
    const ASN1_OBJECT * const cname = OBJ_nid2obj(NID_commonName),
	* const email = OBJ_nid2obj(NID_pkcs9_emailAddress);

    for (n = X509_NAME_entry_count(name->dn); n > 0; n--) {
	X509_NAME_ENTRY *ent = X509_NAME_get_entry(name->dn, n-1);
	ASN1_OBJECT *obj = X509_NAME_ENTRY_get_object(ent);
	
        /* Skip commonName or emailAddress except if there is no other
         * attribute in dname. */
	if ((OBJ_cmp(obj, cname) && OBJ_cmp(obj, email)) ||
            (!flag && n == 1)) {
 	    if (flag++)
		ne_buffer_append(dump, ", ", 2);

            if (append_dirstring(dump, X509_NAME_ENTRY_get_data(ent)))
                ne_buffer_czappend(dump, "???");
	}
    }

    return ne_buffer_finish(dump);
}

int ne_ssl_dname_cmp(const ne_ssl_dname *dn1, const ne_ssl_dname *dn2)
{
    return X509_NAME_cmp(dn1->dn, dn2->dn);
}

static void clicert_free_cert(ne_ssl_client_cert *cc)
{
    if (cc->cert.identity) ne_free(cc->cert.identity);
    if (cc->cert.subject) X509_free(cc->cert.subject);
    cc->cert.identity = NULL;
    cc->cert.subject = NULL;
}

void ne_ssl_clicert_free(ne_ssl_client_cert *cc)
{
    if (cc->p12) PKCS12_free(cc->p12);
    if (cc->uri) ne_free(cc->uri);
    if (cc->pkey) EVP_PKEY_free(cc->pkey);
    if (cc->friendly_name) ne_free(cc->friendly_name);
    clicert_free_cert(cc);
    ne_free(cc);
}

/* Format an ASN1 time to a string. 'buf' must be at least of size
 * 'NE_SSL_VDATELEN'. */
static time_t asn1time_to_timet(const ASN1_TIME *atm)
{
    struct tm tm = {0};
    int i = atm->length;
    
    if (i < 10)
        return (time_t )-1;

    tm.tm_year = FROM_DEC(atm->data);

    /* Deal with Year 2000 */
    if (tm.tm_year < 70)
        tm.tm_year += 100;

    tm.tm_mon = FROM_DEC(atm->data + 2) - 1;
    tm.tm_mday = FROM_DEC(atm->data + 4);
    tm.tm_hour = FROM_DEC(atm->data + 6);
    tm.tm_min = FROM_DEC(atm->data + 8);
    tm.tm_sec = FROM_DEC(atm->data + 10);

#ifdef HAVE_TIMEZONE
    /* ANSI C time handling is... interesting. */
    return mktime(&tm) - timezone;
#else
    return mktime(&tm);
#endif
}

void ne_ssl_cert_validity_time(const ne_ssl_certificate *cert,
                               time_t *from, time_t *until)
{
    if (from) {
        *from = asn1time_to_timet(X509_get0_notBefore(cert->subject));
    }
    if (until) {
        *until = asn1time_to_timet(X509_get0_notAfter(cert->subject));
    }
}

/* Check certificate identity.  Returns zero if identity matches; 1 if
 * identity does not match, or <0 if the certificate had no identity.
 * If 'identity' is non-NULL, store the malloc-allocated identity in
 * *identity.  Logic specified by RFC 2818 and RFC 3280. */
static int check_identity(const struct host_info *server, X509 *cert,
                          char **identity)
{
    STACK_OF(GENERAL_NAME) *names;
    int match = 0, found = 0;
    const char *hostname;
    
    hostname = server ? server->hostname : "";

    names = X509_get_ext_d2i(cert, NID_subject_alt_name, NULL, NULL);
    if (names) {
	int n;

        /* subjectAltName contains a sequence of GeneralNames */
	for (n = 0; n < sk_GENERAL_NAME_num(names) && !match; n++) {
	    GENERAL_NAME *nm = sk_GENERAL_NAME_value(names, n);
	    
            /* handle dNSName and iPAddress name extensions only. */
	    if (nm->type == GEN_DNS) {
		char *name = dup_ia5string(nm->d.ia5);
                if (identity && !found) *identity = ne_strdup(name);

                /* Only match if the server was not identified by a
                 * literal IP address; avoiding wildcard matches. */
                if (server && !server->literal)
                    match = ne__ssl_match_hostname(name, strlen(name), hostname);
		ne_free(name);
		found = 1;
            } 
            else if (nm->type == GEN_IPADD && server && server->literal) {
                /* compare IP addfress with server literal IP address. */
                ne_inet_addr *ia;
                if (nm->d.ip->length == 4)
                    ia = ne_iaddr_make(ne_iaddr_ipv4, nm->d.ip->data);
                else if (nm->d.ip->length == 16)
                    ia = ne_iaddr_make(ne_iaddr_ipv6, nm->d.ip->data);
                else
                    ia = NULL;
                /* ne_iaddr_make returns NULL if address type is unsupported */
                if (ia != NULL) { /* address type was supported. */
                    match = ne_iaddr_cmp(ia, server->literal) == 0;
                    found = 1;
                    ne_iaddr_free(ia);
                } else {
                    NE_DEBUG(NE_DBG_SSL, "iPAddress name with unsupported "
                             "address type (length %d), skipped.\n",
                             nm->d.ip->length);
                }
            }
        }
        /* free the whole stack. */
        sk_GENERAL_NAME_pop_free(names, GENERAL_NAME_free);
    }
    
    /* Check against the commonName if no DNS alt. names were found,
     * as per RFC3280. */
    if (!found) {
	X509_NAME *subj = X509_get_subject_name(cert);
	X509_NAME_ENTRY *entry;
	ne_buffer *cname = ne_buffer_ncreate(30);
	int idx = -1, lastidx;

	/* find the most specific commonName attribute. */
	do {
	    lastidx = idx;
	    idx = X509_NAME_get_index_by_NID(subj, NID_commonName, lastidx);
	} while (idx >= 0);
	
	if (lastidx < 0) {
            /* no commonName attributes at all. */
            ne_buffer_destroy(cname);
	    return -1;
        }

	/* extract the string from the entry */
        entry = X509_NAME_get_entry(subj, lastidx);
        if (append_dirstring(cname, X509_NAME_ENTRY_get_data(entry))) {
            ne_buffer_destroy(cname);
            return -1;
        }
        if (identity) *identity = ne_strdup(cname->data);
        if (server && !server->literal)
            match = ne__ssl_match_hostname(cname->data, cname->used-1, hostname);
        ne_buffer_destroy(cname);
    }

    NE_DEBUG(NE_DBG_SSL, "Identity match for '%s': %s\n", hostname, 
             match ? "good" : "bad");
    return match ? 0 : 1;
}

/* Populate an ne_ssl_certificate structure from an X509 object. */
static ne_ssl_certificate *populate_cert(ne_ssl_certificate *cert, X509 *x5)
{
    cert->subj_dn.dn = X509_get_subject_name(x5);
    cert->issuer_dn.dn = X509_get_issuer_name(x5);
    cert->issuer = NULL;
    cert->subject = x5;
    /* Retrieve the cert identity; pass a dummy hostname to match. */
    cert->identity = NULL;
    check_identity(NULL, x5, &cert->identity);
    return cert;
}

/* OpenSSL cert verification callback.  This is invoked for *each*
 * error which is encountered whilst verifying the cert chain; multiple
 * invocations for any particular cert in the chain are possible. */
static int verify_callback(int ok, X509_STORE_CTX *ctx)
{
    /* OpenSSL, living in its own little happy world of global state,
     * where userdata was just a twinkle in the eye of an API designer
     * yet to be born.  Or... "Seriously, wtf?"  */
    SSL *ssl = X509_STORE_CTX_get_ex_data(ctx, 
                                          SSL_get_ex_data_X509_STORE_CTX_idx());
    ne_session *sess = SSL_get_app_data(ssl);
    int depth = X509_STORE_CTX_get_error_depth(ctx);
    int err = X509_STORE_CTX_get_error(ctx);
    int failures = 0;

    /* If there's no error, nothing to do here. */
    if (ok) return ok;

    NE_DEBUG(NE_DBG_SSL, "ssl: Verify callback @ %d => %d\n", depth, err);

    /* Map the error code onto any of the exported cert validation
     * errors, if possible. */
    switch (err) {
    case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
    case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY:
    case X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN:
    case X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT:
    case X509_V_ERR_CERT_UNTRUSTED:
    case X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE:
        failures |= NE_SSL_UNTRUSTED;
        break;
    case X509_V_ERR_CERT_NOT_YET_VALID:
        failures |= depth > 0 ? NE_SSL_BADCHAIN : NE_SSL_NOTYETVALID;
        break;
    case X509_V_ERR_CERT_HAS_EXPIRED:
        failures |= depth > 0 ? NE_SSL_BADCHAIN : NE_SSL_EXPIRED;
        break;
    case X509_V_OK:
        break;
    default:
        /* Clear the failures bitmask so check_certificate knows this
         * is a bailout. */
        sess->ssl_context->failures |= NE_SSL_UNHANDLED;
        NE_DEBUG(NE_DBG_SSL, "ssl: Unhandled verification error %d -> %s\n", 
                 err, X509_verify_cert_error_string(err));
        return 0;
    }

    sess->ssl_context->failures |= failures;

    NE_DEBUG(NE_DBG_SSL, "ssl: Verify failures |= %d => %d\n", failures,
             sess->ssl_context->failures);
    
    return 1;
}

/* Return a linked list of certificate objects from an OpenSSL chain. */
static ne_ssl_certificate *make_chain(STACK_OF(X509) *chain)
{
    int n, count = sk_X509_num(chain);
    ne_ssl_certificate *top = NULL, *current = NULL;
    
    NE_DEBUG(NE_DBG_SSL, "Chain depth: %d\n", count);

    for (n = 0; n < count; n++) {
        ne_ssl_certificate *cert = ne_malloc(sizeof *cert);
        populate_cert(cert, X509_dup(sk_X509_value(chain, n)));
#ifdef NE_DEBUGGING
        if (ne_debug_mask & NE_DBG_SSL) {
            fprintf(ne_debug_stream, "Cert #%d:\n", n);
            X509_print_fp(ne_debug_stream, cert->subject);
        }
#endif
        if (top == NULL) {
            current = top = cert;
        } else {
            current->issuer = cert;
            current = cert;
        }
    }

    return top;
}

/* Verifies an SSL server certificate. */
static int check_certificate(ne_session *sess, SSL *ssl, ne_ssl_certificate *chain)
{
    X509 *cert = chain->subject;
    int ret, failures = sess->ssl_context->failures;

    /* If the verification callback hit a case which can't be mapped
     * to one of the exported error bits, it's treated as a hard
     * failure rather than invoking the callback, which can't present
     * a useful error to the user.  "Um, something is wrong.  OK?" */
    if (failures & NE_SSL_UNHANDLED) {
        long result = SSL_get_verify_result(ssl);

        ne_set_error(sess, _("Certificate verification error: %s"),
                    X509_verify_cert_error_string(result));

        return NE_ERROR;
    }

    /* Check certificate was issued to this server; pass URI of
     * server. */
    ret = check_identity(&sess->server, cert, NULL);
    if (ret < 0) {
        ne_set_error(sess, _("Server certificate was missing commonName "
                             "attribute in subject name"));
        return NE_ERROR;
    } else if (ret > 0) failures |= NE_SSL_IDMISMATCH;

    if (failures == 0) {
        /* verified OK! */
        ret = NE_OK;
    } else {
        /* Set up the error string. */
        ne__ssl_set_verify_err(sess, failures);
        ret = NE_ERROR;
        /* Allow manual override */
        if (sess->ssl_verify_fn && 
            sess->ssl_verify_fn(sess->ssl_verify_ud, failures, chain) == 0)
            ret = NE_OK;
    }

    return ret;
}

/* Duplicate a client certificate, which must be in the decrypted state. */
static ne_ssl_client_cert *dup_client_cert(const ne_ssl_client_cert *cc)
{
    ne_ssl_client_cert *newcc = ne_calloc(sizeof *newcc);
    
    newcc->pkey = cc->pkey;
    if (cc->friendly_name)
        newcc->friendly_name = ne_strdup(cc->friendly_name);

    populate_cert(&newcc->cert, cc->cert.subject);

    X509_up_ref(cc->cert.subject);
    EVP_PKEY_up_ref(cc->pkey);
    return newcc;
}

/* Callback invoked when the SSL server requests a client certificate.  */
static int provide_client_cert(SSL *ssl, X509 **cert, EVP_PKEY **pkey)
{
    ne_session *const sess = SSL_get_app_data(ssl);

    if (!sess->client_cert && sess->ssl_provide_fn) {
	ne_ssl_dname **dnames = NULL, *dnarray = NULL;
        int n, count = 0;
	STACK_OF(X509_NAME) *ca_list = SSL_get_client_CA_list(ssl);

        count = ca_list ? sk_X509_NAME_num(ca_list) : 0;

        if (count > 0) {
            dnames = ne_malloc(count * sizeof(ne_ssl_dname *));
            dnarray = ne_malloc(count * sizeof(ne_ssl_dname));
            
            for (n = 0; n < count; n++) {
                dnames[n] = &dnarray[n];
                dnames[n]->dn = sk_X509_NAME_value(ca_list, n);
            }
        }

	NE_DEBUG(NE_DBG_SSL, "Calling client certificate provider...\n");
	sess->ssl_provide_fn(sess->ssl_provide_ud, sess, 
                             (const ne_ssl_dname *const *)dnames, count);
        if (count) {
            ne_free(dnarray);
            ne_free(dnames);
        }
    }

    if (sess->client_cert) {
        ne_ssl_client_cert *const cc = sess->client_cert;
	NE_DEBUG(NE_DBG_SSL, "Supplying client certificate.\n");
	EVP_PKEY_up_ref(cc->pkey);
	X509_up_ref(cc->cert.subject);
	*cert = cc->cert.subject;
	*pkey = cc->pkey;
	return 1;
    } else {
        sess->ssl_cc_requested = 1;
	NE_DEBUG(NE_DBG_SSL, "No client certificate supplied.\n");
	return 0;
    }
}

void ne_ssl_set_clicert(ne_session *sess, const ne_ssl_client_cert *cc)
{
    sess->client_cert = dup_client_cert(cc);
}

static int new_ssl_session(SSL *ssl, SSL_SESSION *sslsess)
{
    SSL_CTX *sslctx = SSL_get_SSL_CTX(ssl);
    ne_ssl_context *sctx = SSL_CTX_get_app_data(sslctx);

    if (sctx->sess)
        SSL_SESSION_free(sctx->sess);

    NE_DEBUG(NE_DBG_SSL, "sslsess: New session callback: %s.\n",
             SSL_SESSION_is_resumable(sslsess) ? "resumable" : "not resumable");

    sctx->sess = SSL_SESSION_dup(sslsess);
    return 1;
}

static void remove_ssl_session(SSL_CTX *sslctx, SSL_SESSION *sess)
{
    ne_ssl_context *ctx = SSL_CTX_get_app_data(sslctx);

    NE_DEBUG(NE_DBG_SSL, "sslsess: Remove session callback.\n");

    if (ctx->sess && sess == ctx->sess) {
        SSL_SESSION_free(ctx->sess);
        ctx->sess = NULL;
    }
}

ne_ssl_context *ne_ssl_context_create(int mode)
{
    ne_ssl_context *ctx = ne_calloc(sizeof *ctx);

    if (mode == NE_SSL_CTX_CLIENT) {
        ctx->ctx = SSL_CTX_new(TLS_client_method());
        ctx->sess = NULL;
        /* set client cert callback. */
        SSL_CTX_set_client_cert_cb(ctx->ctx, provide_client_cert);
        /* enable workarounds for buggy SSL server implementations */
        SSL_CTX_set_options(ctx->ctx, SSL_OP_ALL);
        SSL_CTX_set_verify(ctx->ctx, SSL_VERIFY_PEER, verify_callback);
#if defined(LIBRESSL_VERSION_NUMBER) && LIBRESSL_VERSION_NUMBER >= 0x3040000fL || (!defined(LIBRESSL_VERSION_NUMBER) && OPENSSL_VERSION_NUMBER >= 0x10101000L)
        SSL_CTX_set_post_handshake_auth(ctx->ctx, 1);
#endif
        SSL_CTX_set_app_data(ctx->ctx, ctx);
        SSL_CTX_sess_set_new_cb(ctx->ctx, new_ssl_session);
        SSL_CTX_sess_set_remove_cb(ctx->ctx, remove_ssl_session);
        SSL_CTX_set_session_cache_mode(ctx->ctx, SSL_SESS_CACHE_CLIENT|SSL_SESS_CACHE_NO_INTERNAL);
    }
    else /* mode == NE_SSL_CTX_SERVER */ {
        char sidctx[128];
        ctx->ctx = SSL_CTX_new(TLS_server_method());
#ifdef SSL_OP_NO_TICKET
        /* disable ticket support since it inhibits testing of session
         * caching. */
        SSL_CTX_set_options(ctx->ctx, SSL_OP_NO_TICKET);
#endif
        ne_snprintf(sidctx, sizeof sidctx, "%s-%p", PACKAGE_NAME, ctx);
        SSL_CTX_set_session_id_context(ctx->ctx, (unsigned char *)sidctx,
                                       strlen(sidctx));
    }
    return ctx;
}

void ne_ssl_context_set_flag(ne_ssl_context *ctx, int flag, int value)
{
}

int ne_ssl_context_get_flag(ne_ssl_context *ctx, int flag)
{
    return 0;
}

int ne_ssl_context_keypair(ne_ssl_context *ctx, const char *cert,
                           const char *key)
{
    int ret;

    ret = SSL_CTX_use_PrivateKey_file(ctx->ctx, key, SSL_FILETYPE_PEM);
    if (ret == 1) {
        ret = SSL_CTX_use_certificate_chain_file(ctx->ctx, cert);
    }

    return ret == 1 ? 0 : -1;
}

int ne_ssl_context_set_verify(ne_ssl_context *ctx, 
                              int required,
                              const char *ca_names,
                              const char *verify_cas)
{
    if (required) {
        SSL_CTX_set_verify(ctx->ctx, SSL_VERIFY_PEER | 
                           SSL_VERIFY_FAIL_IF_NO_PEER_CERT, NULL);
    }
    if (ca_names) {
        SSL_CTX_set_client_CA_list(ctx->ctx, 
                                   SSL_load_client_CA_file(ca_names));
    }
    if (verify_cas) {
        SSL_CTX_load_verify_locations(ctx->ctx, verify_cas, NULL);
    }
    return 0;
}

#ifdef HAVE_OPENSSL110
/* Map neon version constants to native OpenSSL constants, returns -1
 * on versions not supported. */
static int proto_to_native(enum ne_ssl_protocol proto)
{
    switch (proto) {
    case NE_SSL_PROTO_UNSPEC: return 0;
    case NE_SSL_PROTO_SSL_3: return SSL3_VERSION;
    case NE_SSL_PROTO_TLS_1_0: return TLS1_VERSION;
    case NE_SSL_PROTO_TLS_1_1: return TLS1_1_VERSION;
#ifdef TLS1_2_VERSION
    case NE_SSL_PROTO_TLS_1_2: return TLS1_2_VERSION;
#endif
#ifdef TLS1_3_VERSION
    case NE_SSL_PROTO_TLS_1_3: return TLS1_3_VERSION;
#endif
    default:
        return -1;
    }
}
#endif

int ne_ssl_context_set_versions(ne_ssl_context *ctx, enum ne_ssl_protocol min,
                                enum ne_ssl_protocol max)
{
#ifdef HAVE_OPENSSL110
    int omin = proto_to_native(min), omax = proto_to_native(max), ret;

    if (omin < 0 || omax < 0) {
        return NE_SOCK_ERROR;
    }

    if ((ret = SSL_CTX_set_min_proto_version(ctx->ctx, omin)) == 1)
        ret = SSL_CTX_set_max_proto_version(ctx->ctx, omax);

    ERR_clear_error();
    
    return ret == 1 ? 0 : NE_SOCK_ERROR;
#else
    return NE_SOCK_ERROR;
#endif
}

void ne_ssl_context_destroy(ne_ssl_context *ctx)
{
    SSL_CTX_free(ctx->ctx);
    if (ctx->sess)
        SSL_SESSION_free(ctx->sess);
    ne_free(ctx);
}

/* For internal use only. */
int ne__negotiate_ssl(ne_session *sess)
{
    ne_ssl_context *ctx = sess->ssl_context;
    SSL *ssl;
    STACK_OF(X509) *chain;
    int freechain = 0; /* non-zero if chain should be free'd. */

    NE_DEBUG(NE_DBG_SSL, "Doing SSL negotiation.\n");
    
    /* Pass through the hostname if SNI is enabled. */
    ctx->hostname = 
        sess->flags[NE_SESSFLAG_TLS_SNI] ? sess->server.hostname : NULL;

    sess->ssl_cc_requested = 0;
    ctx->failures = 0;

    if (ne_sock_connect_ssl(sess->socket, ctx, sess)) {
        if (sess->ssl_cc_requested) {
            ne_set_error(sess, _("SSL handshake failed, "
                                 "client certificate was requested: %s"),
                         ne_sock_error(sess->socket));
        }
        else {
            ne_set_error(sess, _("SSL handshake failed: %s"),
                         ne_sock_error(sess->socket));
        }
        return NE_ERROR;
    }	
    
    ssl = ne__sock_sslsock(sess->socket);

    chain = SSL_get_peer_cert_chain(ssl);
    /* For an SSLv2 connection, the cert chain will always be NULL. */
    if (chain == NULL) {
        X509 *cert = SSL_get_peer_certificate(ssl);
        if (cert) {
            chain = sk_X509_new_null();
            sk_X509_push(chain, cert);
            freechain = 1;
        }
    }

    if (chain == NULL || sk_X509_num(chain) == 0) {
	ne_set_error(sess, _("SSL server did not present certificate"));
	return NE_ERROR;
    }

    if (sess->server_cert 
        && X509_cmp(sk_X509_value(chain, 0), sess->server_cert->subject) == 0) {
        /* Same leaf cert used as last time - no need to reverify. */
        if (freechain) sk_X509_free(chain); /* no longer need the chain */
    } else {
	/* new connection: create the chain. */
        ne_ssl_certificate *cert = make_chain(chain);

        if (freechain) sk_X509_free(chain); /* no longer need the chain */

	if (check_certificate(sess, ssl, cert)) {
	    NE_DEBUG(NE_DBG_SSL, "SSL certificate checks failed: %s\n",
		     sess->error);
	    ne_ssl_cert_free(cert);
	    return NE_ERROR;
	}
	/* remember the chain. */
        sess->server_cert = cert;
    }
    
    if (sess->notify_cb) {
        const SSL_CIPHER *ciph = SSL_get_current_cipher(ssl);

        sess->status.hs.protocol = ne_sock_getproto(sess->socket);
        sess->status.hs.ciphersuite = SSL_CIPHER_standard_name(ciph);
        sess->notify_cb(sess->notify_ud, ne_status_handshake, &sess->status);
    }

    return NE_OK;
}

const ne_ssl_dname *ne_ssl_cert_issuer(const ne_ssl_certificate *cert)
{
    return &cert->issuer_dn;
}

const ne_ssl_dname *ne_ssl_cert_subject(const ne_ssl_certificate *cert)
{
    return &cert->subj_dn;
}

const ne_ssl_certificate *ne_ssl_cert_signedby(const ne_ssl_certificate *cert)
{
    return cert->issuer;
}

const char *ne_ssl_cert_identity(const ne_ssl_certificate *cert)
{
    return cert->identity;
}

void ne_ssl_context_trustcert(ne_ssl_context *ctx, const ne_ssl_certificate *cert)
{
    X509_STORE *store = SSL_CTX_get_cert_store(ctx->ctx);
    
    X509_STORE_add_cert(store, cert->subject);
}

void ne_ssl_trust_default_ca(ne_session *sess)
{
    if (sess->ssl_context) {
        X509_STORE *store = SSL_CTX_get_cert_store(sess->ssl_context->ctx);
    
#ifdef NE_SSL_CA_BUNDLE
        X509_STORE_load_locations(store, NE_SSL_CA_BUNDLE, NULL);
#else
        X509_STORE_set_default_paths(store);
#endif
    }
}

/* Find a friendly name in a PKCS12 structure the hard way, without
 * decrypting the parts which are encrypted.. */
static char *find_friendly_name(PKCS12 *p12)
{
    STACK_OF(PKCS7) *safes = PKCS12_unpack_authsafes(p12);
    int n, m;
    char *name = NULL;

    if (safes == NULL) return NULL;
    
    /* Iterate over the unpacked authsafes: */
    for (n = 0; n < sk_PKCS7_num(safes) && !name; n++) {
        PKCS7 *safe = sk_PKCS7_value(safes, n);
        STACK_OF(PKCS12_SAFEBAG) *bags;
    
        /* Only looking for unencrypted authsafes. */
        if (OBJ_obj2nid(safe->type) != NID_pkcs7_data) continue;

        bags = PKCS12_unpack_p7data(safe);
        if (!bags) continue;

        /* Iterate through the bags, picking out a friendly name */
        for (m = 0; m < sk_PKCS12_SAFEBAG_num(bags) && !name; m++) {
            PKCS12_SAFEBAG *bag = sk_PKCS12_SAFEBAG_value(bags, m);
            name = PKCS12_get_friendlyname(bag);
        }
    
        sk_PKCS12_SAFEBAG_pop_free(bags, PKCS12_SAFEBAG_free);
    }

    sk_PKCS7_pop_free(safes, PKCS7_free);
    return name;
}

static int pkcs12_decrypt(ne_ssl_client_cert *cc, const char *password);

static ne_ssl_client_cert *parse_client_cert(PKCS12 *p12)
{
    X509 *cert;
    EVP_PKEY *pkey;
    ne_ssl_client_cert *cc;

    if (p12 == NULL) {
        ERR_clear_error();
        return NULL;
    }

    /* Try parsing with no password. */
    if (PKCS12_parse(p12, NULL, &pkey, &cert, NULL) == 1) {
        /* Success - no password needed for decryption. */
        int len = 0;
        unsigned char *name;

        if (!cert || !pkey) {
            PKCS12_free(p12);
            return NULL;
        }

        name = X509_alias_get0(cert, &len);
        
        cc = ne_calloc(sizeof *cc);
        cc->pkey = pkey;
        if (name && len > 0)
            cc->friendly_name = ne_strndup((char *)name, len);
        populate_cert(&cc->cert, cert);
        PKCS12_free(p12);
        return cc;
    } else {
        /* Failed to parse the file */
        int err = ERR_get_error();
        ERR_clear_error();
        if (ERR_GET_LIB(err) == ERR_LIB_PKCS12 &&
            ERR_GET_REASON(err) == PKCS12_R_MAC_VERIFY_FAILURE) {
            /* Decryption error due to bad password. */
            cc = ne_calloc(sizeof *cc);
            cc->friendly_name = find_friendly_name(p12);
            cc->p12 = p12;
            cc->decrypt = pkcs12_decrypt;
            return cc;
        } else {
            /* Some parse error, give up. */
            PKCS12_free(p12);
            return NULL;
        }
    }
}

ne_ssl_client_cert *ne_ssl_clicert_import(const unsigned char *buffer, 
                                          size_t buflen)
{
    ne_d2i_uchar *p;
    PKCS12 *p12;

    p = buffer;
    p12 = d2i_PKCS12(NULL, &p, buflen);
    
    return parse_client_cert(p12);
}
    
ne_ssl_client_cert *ne_ssl_clicert_read(const char *filename)
{
    PKCS12 *p12;
    FILE *fp;

    fp = fopen(filename, "rb");
    if (fp == NULL)
        return NULL;

    p12 = d2i_PKCS12_fp(fp, NULL);

    fclose(fp);

    return parse_client_cert(p12);
}

#ifdef HAVE_OPENSSL111

static int ui_reader(UI *ui, UI_STRING *uis)
{
    const char *password = UI_get0_user_data(ui);
    enum UI_string_types uit = UI_get_string_type(uis);
    int ret = 0;

    if (uit != UIT_PROMPT) return -1;

    if (UI_set_result(ui, uis, password) != 0) {
        unsigned long err = ERR_get_error();
        NE_DEBUG(NE_DBG_SSL, "pk11: Result set failed: %s.\n",
                 ERR_reason_error_string(err));
    }
    else {
        NE_DEBUG(NE_DBG_SSL, "pk11: Result set successfully.\n");
        ret = 1;
    }

    return ret;
}

/* Iterate through an OSSL_STORE - possibly supplying decryption
 * password if non-NULL. */
static int store_iterate(ne_ssl_client_cert *cc, const char *password)
{
    X509 *cert = NULL;
    EVP_PKEY *pkey = NULL;
    OSSL_STORE_CTX *store;
    UI_METHOD *ui;

    if (password) {
        ui = UI_create_method("neon");
        UI_method_set_reader(ui, ui_reader);
    }
    else {
        ui = NULL;
    }

    /* Clear any existing cert data to ensure each iteration fetches a
     * matching cert/key pair. */
    clicert_free_cert(cc);

    NE_DEBUG(NE_DBG_SSL, "ssl: Opening store for %s...\n", cc->uri);
    store = OSSL_STORE_open(cc->uri, ui, (char *)password, NULL, NULL);
    if (!store) {
        NE_DEBUG(NE_DBG_SSL, "ssl: Failed to open store.\n");
        return ENODEV;
    }

    while (!OSSL_STORE_eof(store)) {
        OSSL_STORE_INFO *info = OSSL_STORE_load(store);

        if (!info) {
            /* OSSL_STORE_load() returns NULL once reaching EOF, or on
             * error - e.g. for an encrypted PKEY. Log but ignore any
             * errors. */
#ifdef NE_DEBUGGING
            unsigned long err = ERR_get_error();
            NE_DEBUG(NE_DBG_SSL, "ssl: Store load failed (eof: %s): %s\n",
                     OSSL_STORE_eof(store) ? "yes" : "no",
                     err ? ERR_reason_error_string(err) : "(no error)");
#endif
            ERR_clear_error();
            continue;
        }

        switch (OSSL_STORE_INFO_get_type(info)) {
        case OSSL_STORE_INFO_CERT:
            if (!cert) {
                NE_DEBUG(NE_DBG_SSL, "ssl: STORE got CERT.\n");
                cert = OSSL_STORE_INFO_get1_CERT(info);
            }
            break;
        case OSSL_STORE_INFO_PKEY:
            if (!pkey) {
                NE_DEBUG(NE_DBG_SSL, "ssl: STORE got PKEY.\n");
                pkey = OSSL_STORE_INFO_get1_PKEY(info);
            }
            break;
        }

        OSSL_STORE_INFO_free(info);
    }

    OSSL_STORE_close(store);
    NE_DEBUG(NE_DBG_SSL, "ssl: End of store.\n");
    if (ui) UI_destroy_method(ui);

    if (!cert) {
        if (pkey) EVP_PKEY_free(pkey);
        return ENOENT;
    }

    /* Fail early if both a cert&pkey are found but don't match. */
    if (pkey && X509_check_private_key(cert, pkey) != 1) {
        ERR_clear_error();
        X509_free(cert);
        EVP_PKEY_free(pkey);
        NE_DEBUG(NE_DBG_SSL, "ssl: Decrypted private key/cert are not matched.");
        return EINVAL;
    }

    /* Store/extract the X509 in the cc object. */
    populate_cert(&cc->cert, cert);

    if (pkey) {
        /* Successfully extracted both a cert & pkey. */
        cc->pkey = pkey;
        cc->decrypt = NULL;
        return 0;
    }

    /* If this was a _decrypt() call (password must be non-NULL),
     * return an error if a privkey wasn't available. Otherwise (NULL
     * password), this is the first iteration through the STORE for
     * the clicert object, succeed and set up for future decryption */
    if (password) {
        return EAGAIN;
    }
    else {
        cc->decrypt = store_iterate;
        return 0;
    }
}

ne_ssl_client_cert *ne_ssl_clicert_fromuri(const char *uri,
                                           unsigned int flags)
{
    ne_ssl_client_cert *cc = ne_calloc(sizeof *cc);
    int errnum;

    cc->uri = ne_strdup(uri);
    if ((errnum = store_iterate(cc, NULL)) != 0) {
        ne_free(cc->uri);
        ne_free(cc);
        errno = errnum;
        return NULL;
    }
    return cc;
}

#endif

#ifdef HAVE_PAKCHOIS
static ne_ssl_client_cert *clicert_from_keypair(X509 *cert, EVP_PKEY *key)
{
    ne_ssl_client_cert *cc = ne_calloc(sizeof *cc);

    cc->pkey = key;
    populate_cert(&cc->cert, cert);

    return cc;
}

ne_ssl_client_cert *ne__ssl_clicert_exkey_import(const unsigned char *der,
                                                 size_t der_len,
                                                 const RSA_METHOD *method)
{
    ne_d2i_uchar *p;
    X509 *x5;
    EVP_PKEY *pubkey, *privkey;
    RSA *rsa;

    p = der;
    x5 = d2i_X509(NULL, &p, der_len); /* p is incremented */
    if (x5 == NULL) {
        ERR_clear_error();
        return NULL;
    }

    pubkey = X509_get_pubkey(x5);
    if (EVP_PKEY_base_id(pubkey) != EVP_PKEY_RSA) {
        X509_free(x5);
        NE_DEBUG(NE_DBG_SSL, "ssl: Only RSA private keys are supported via PKCS#11.\n");
        return NULL;
    }

    /* Duplicate the public parameters of the RSA key. */
    rsa = RSAPublicKey_dup(EVP_PKEY_get0_RSA(pubkey));
    /* Done with the copied public key. */
    EVP_PKEY_free(pubkey);
    
    /* Switch to using customer RSA_METHOD for RSA object. */
    RSA_set_method(rsa, method);
    /* Set up new EVP_PKEY. */
    privkey = EVP_PKEY_new();
    EVP_PKEY_assign_RSA(privkey, rsa);

    return clicert_from_keypair(x5, privkey);
}
#endif

int ne_ssl_clicert_encrypted(const ne_ssl_client_cert *cc)
{
    return cc->decrypt != NULL;
}

static int pkcs12_decrypt(ne_ssl_client_cert *cc, const char *password)
{
    X509 *cert;
    EVP_PKEY *pkey;

    if (PKCS12_parse(cc->p12, password, &pkey, &cert, NULL) != 1) {
        ERR_clear_error();
        return EACCES;
    }
    
    if (X509_check_private_key(cert, pkey) != 1) {
        ERR_clear_error();
        X509_free(cert);
        EVP_PKEY_free(pkey);
        NE_DEBUG(NE_DBG_SSL, "ssl: Decrypted private key/cert are not matched.");
        return EINVAL;
    }

    PKCS12_free(cc->p12);
    populate_cert(&cc->cert, cert);
    cc->pkey = pkey;
    cc->decrypt = NULL;
    cc->p12 = NULL;
    return 0;
}

int ne_ssl_clicert_decrypt(ne_ssl_client_cert *cc, const char *password)
{
    int errnum = cc->decrypt ? cc->decrypt(cc, password) : EINVAL;
    return errnum != 0;
}

const ne_ssl_certificate *ne_ssl_clicert_owner(const ne_ssl_client_cert *cc)
{
    return &cc->cert;
}

const char *ne_ssl_clicert_name(const ne_ssl_client_cert *cc)
{
    return cc->friendly_name ? cc->friendly_name : cc->uri;
}

ne_ssl_certificate *ne_ssl_cert_read(const char *filename)
{
    FILE *fp = fopen(filename, "r");
    X509 *cert;

    if (fp == NULL)
        return NULL;

    cert = PEM_read_X509(fp, NULL, NULL, NULL);
    fclose(fp);

    if (cert == NULL) {
        NE_DEBUG(NE_DBG_SSL, "d2i_X509_fp failed: %s\n", 
                 ERR_reason_error_string(ERR_get_error()));
        ERR_clear_error();
        return NULL;
    }

    return populate_cert(ne_calloc(sizeof(struct ne_ssl_certificate_s)), cert);
}

int ne_ssl_cert_write(const ne_ssl_certificate *cert, const char *filename)
{
    FILE *fp = fopen(filename, "w");

    if (fp == NULL) return -1;

    if (PEM_write_X509(fp, cert->subject) != 1) {
        ERR_clear_error();
        fclose(fp);
        return -1;
    }
    
    if (fclose(fp) != 0)
        return -1;

    return 0;
}

void ne_ssl_cert_free(ne_ssl_certificate *cert)
{
    X509_free(cert->subject);
    if (cert->issuer)
        ne_ssl_cert_free(cert->issuer);
    if (cert->identity)
        ne_free(cert->identity);
    ne_free(cert);
}

int ne_ssl_cert_cmp(const ne_ssl_certificate *c1, const ne_ssl_certificate *c2)
{
    return X509_cmp(c1->subject, c2->subject);
}

/* The certificate import/export format is the base64 encoding of the
 * raw DER; PEM without the newlines and wrapping. */

ne_ssl_certificate *ne_ssl_cert_import(const char *data)
{
    unsigned char *der;
    ne_d2i_uchar *p;
    size_t len;
    X509 *x5;
    
    /* decode the base64 to get the raw DER representation */
    len = ne_unbase64(data, &der);
    if (len == 0) return NULL;

    p = der;
    x5 = d2i_X509(NULL, &p, len); /* p is incremented */
    ne_free(der);
    if (x5 == NULL) {
        ERR_clear_error();
        return NULL;
    }

    return populate_cert(ne_calloc(sizeof(struct ne_ssl_certificate_s)), x5);
}

char *ne_ssl_cert_export(const ne_ssl_certificate *cert)
{
    int len;
    unsigned char *der, *p;
    char *ret;
    
    /* find the length of the DER encoding. */
    len = i2d_X509(cert->subject, NULL);

    p = der = ne_malloc(len);
    i2d_X509(cert->subject, &p); /* p is incremented */

    ret = ne_base64(der, len);
    ne_free(der);
    return ret;
}

static const EVP_MD *hash_to_md(unsigned int flags)
{
    switch (flags & NE_HASH_ALGMASK) {
    case NE_HASH_MD5: return EVP_md5();
    case NE_HASH_SHA256: return EVP_sha256();
#ifdef HAVE_OPENSSL11
    case NE_HASH_SHA512: return EVP_sha512();
#if !defined(LIBRESSL_VERSION_NUMBER) || LIBRESSL_VERSION_NUMBER >= 0x3080000fL
    case NE_HASH_SHA512_256: return EVP_sha512_256();
#endif
#endif
    default: break;
    }
    return NULL;
}

#if SHA_DIGEST_LENGTH != 20
# error SHA digest length is not 20 bytes
#endif

char *ne_ssl_cert_hdigest(const ne_ssl_certificate *cert, unsigned int flags)
{
    const EVP_MD *md = hash_to_md(flags);
    unsigned char dig[EVP_MAX_MD_SIZE];
    unsigned int len;

    if (!md) return NULL;

    if (!X509_digest(cert->subject, md, dig, &len)) {
        ERR_clear_error();
        return NULL;
    }

    return ne__strhash2hex(dig, len, flags);
}

int ne_ssl_cert_digest(const ne_ssl_certificate *cert, char *digest)
{
    unsigned char sha1[EVP_MAX_MD_SIZE];
    unsigned int len, j;
    char *p;

    if (!X509_digest(cert->subject, EVP_sha1(), sha1, &len) || len != 20) {
        ERR_clear_error();
        return -1;
    }
    
    for (j = 0, p = digest; j < 20; j++) {
        *p++ = NE_HEX2ASC((sha1[j] >> 4) & 0x0f);
        *p++ = NE_HEX2ASC(sha1[j] & 0x0f);
        *p++ = ':';
    }

    p[-1] = '\0';
    return 0;
}

char *ne_vstrhash(unsigned int flags, va_list ap)
{
    EVP_MD_CTX *ctx;
    const EVP_MD *md = hash_to_md(flags);
    unsigned char v[EVP_MAX_MD_SIZE];
    unsigned int vlen;
    const char *arg;

    ctx = EVP_MD_CTX_new();
    if (!ctx) return NULL;

    if (EVP_DigestInit(ctx, md) != 1) {
        EVP_MD_CTX_free(ctx);
        return NULL;
    }

    while ((arg = va_arg(ap, const char *)) != NULL)
        EVP_DigestUpdate(ctx, arg, strlen(arg));

    EVP_DigestFinal_ex(ctx, v, &vlen);
    EVP_MD_CTX_free(ctx);

    return ne__strhash2hex(v, vlen, flags);
}

#ifdef WITH_OPENSSL_LOCKING
/* Implementation of locking callbacks to make OpenSSL thread-safe.
 * If the OpenSSL API was better designed, this wouldn't be necessary.
 * In OpenSSL releases without CRYPTO_set_idptr_callback, it's not
 * possible to implement the locking in a POSIX-compliant way, since
 * it's necessary to cast from a pthread_t to an unsigned long at some
 * point.  */

#ifndef _WIN32
static pthread_mutex_t *locks;
#else
static HANDLE *locks;
#endif
static size_t num_locks;

#ifndef HAVE_CRYPTO_SET_IDPTR_CALLBACK
/* Named to be obvious when it shows up in a backtrace. */
static unsigned long thread_id_neon(void)
{
#ifndef _WIN32
    /* This will break if pthread_t is a structure; upgrading OpenSSL
     * >= 0.9.9 (which does not require this callback) is the only
     * solution.  */
    return (unsigned long) pthread_self();
#else
    return (unsigned long) GetCurrentThreadId();
#endif
}
#endif

/* Another great API design win for OpenSSL: no return value!  So if
 * the lock/unlock fails, all that can be done is to abort. */
static void thread_lock_neon(int mode, int n, const char *file, int line)
{
    if (mode & CRYPTO_LOCK) {
#ifndef _WIN32
        if (pthread_mutex_lock(&locks[n])) {
#else
        if (WaitForSingleObject(locks[n], INFINITE)) {
#endif
            abort();
        }
    }
    else {
#ifndef _WIN32
        if (pthread_mutex_unlock(&locks[n])) {
#else
        if (!ReleaseMutex(locks[n])) {
#endif
            abort();
        }
    }
}

/* ID_CALLBACK_IS_{NEON,OTHER} evaluate as true if the currently
 * registered OpenSSL ID callback is the neon function (_NEON), or has
 * been overwritten by some other app (_OTHER). */
#ifdef HAVE_CRYPTO_SET_IDPTR_CALLBACK
#define ID_CALLBACK_IS_OTHER (0)
#define ID_CALLBACK_IS_NEON (1)
#else
#define ID_CALLBACK_IS_OTHER (CRYPTO_get_id_callback() != NULL)
#define ID_CALLBACK_IS_NEON (CRYPTO_get_id_callback() == thread_id_neon)
#endif
        
#endif /* NE_HAVE_TS_SSL && OPENSSL_VERSION_NUMBER < 1.1.1 */

int ne__ssl_init(void)
{
#if OPENSSL_VERSION_NUMBER < 0x10100000L
    CRYPTO_malloc_init();
    SSL_load_error_strings();
    SSL_library_init();
    OpenSSL_add_all_algorithms();

#ifdef WITH_OPENSSL_LOCKING
    /* If some other library has already come along and set up the
     * thread-safety callbacks, then it must be presumed that the
     * other library will have a longer lifetime in the process than
     * neon.  If the library which has installed the callbacks is
     * unloaded, then all bets are off. */
    if (ID_CALLBACK_IS_OTHER || CRYPTO_get_locking_callback() != NULL) {
        NE_DEBUG(NE_DBG_SOCKET, "ssl: OpenSSL thread-safety callbacks already installed.\n");
        NE_DEBUG(NE_DBG_SOCKET, "ssl: neon will not replace existing callbacks.\n");
    } else {
        size_t n;

        num_locks = CRYPTO_num_locks();

        /* For releases where CRYPTO_set_idptr_callback is present,
         * the default ID callback should be sufficient. */
#ifndef HAVE_CRYPTO_SET_IDPTR_CALLBACK
        CRYPTO_set_id_callback(thread_id_neon);
#endif
        CRYPTO_set_locking_callback(thread_lock_neon);

        locks = malloc(num_locks * sizeof *locks);
        for (n = 0; n < num_locks; n++) {
#ifndef _WIN32
            if (pthread_mutex_init(&locks[n], NULL)) {
#else
            if ((locks[n] = CreateMutex(NULL, FALSE, NULL)) == NULL) {
#endif
                NE_DEBUG(NE_DBG_SOCKET, "ssl: Failed to initialize pthread mutex.\n");
                return -1;
            }
        }
        
        NE_DEBUG(NE_DBG_SOCKET, "ssl: Initialized OpenSSL thread-safety callbacks "
                 "for %" NE_FMT_SIZE_T " locks.\n", num_locks);
    }
#endif
#endif /* OPENSSL_VERSION_NUMBER < 0x10100000L */

    return 0;
}

void ne__ssl_exit(void)
{
    /* Cannot call ERR_free_strings() etc here in case any other code
     * in the process using OpenSSL. */

#ifdef WITH_OPENSSL_LOCKING
    /* Only unregister the callbacks if some *other* library has not
     * come along in the mean-time and trampled over the callbacks
     * installed by neon. */
    if (CRYPTO_get_locking_callback() == thread_lock_neon
        && ID_CALLBACK_IS_NEON) {
        size_t n;

#ifndef HAVE_CRYPTO_SET_IDPTR_CALLBACK
        CRYPTO_set_id_callback(NULL);
#endif
        CRYPTO_set_locking_callback(NULL);

        for (n = 0; n < num_locks; n++) {
#ifndef _WIN32
            pthread_mutex_destroy(&locks[n]);
#else
            CloseHandle(locks[n]);
#endif
        }

        free(locks);
    }
#endif
}
