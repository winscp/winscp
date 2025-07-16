/*
   neon SSL/TLS support using GNU TLS
   Copyright (C) 2002-2021, Joe Orton <joe@manyfish.co.uk>
   Copyright (C) 2004, Aleix Conchillo Flaque <aleix@member.fsf.org>

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
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>

#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
#include <gnutls/pkcs12.h>

#ifdef NE_HAVE_TS_SSL
#include <errno.h>
#include <pthread.h>
#if LIBGNUTLS_VERSION_NUMBER < 0x020b01
#include <gcrypt.h>
GCRY_THREAD_OPTION_PTHREAD_IMPL;
#endif
#else
#if LIBGNUTLS_VERSION_NUMBER < 0x020b01
#include <gcrypt.h>
#endif
#endif

#ifdef HAVE_ICONV
#include <iconv.h>
#endif

#include "ne_ssl.h"
#include "ne_string.h"
#include "ne_session.h"
#include "ne_internal.h"

#include "ne_private.h"
#include "ne_privssl.h"

#if LIBGNUTLS_VERSION_NUMBER >= 0x020302
/* The GnuTLS DN functions in 2.3.2 and later allow a simpler DN
 * abstraction to be used. */
#define HAVE_NEW_DN_API
#endif

struct ne_ssl_dname_s {
#ifdef HAVE_NEW_DN_API
    gnutls_x509_dn_t dn;
#else
    int subject; /* non-zero if this is the subject DN object */
    gnutls_x509_crt_t cert;
#endif
};

struct ne_ssl_certificate_s {
    ne_ssl_dname subj_dn, issuer_dn;
    gnutls_x509_crt_t subject;
    ne_ssl_certificate *issuer;
    char *identity;
};

struct ne_ssl_client_cert_s {
    gnutls_pkcs12_t p12;
    int decrypted; /* non-zero if successfully decrypted. */
    int keyless;
    ne_ssl_certificate cert;
    gnutls_x509_privkey_t pkey;
    char *friendly_name;
#ifdef HAVE_GNUTLS_PRIVKEY_IMPORT_EXT
    /* Signing callback & userdata provided by ne_pkcs11.c.  It would
     * be better to rewrite the whole module to use gnutls_privkey_t
     * directly, but it seems impossible to dup such an object. */
    gnutls_privkey_sign_func sign_func;
    void *sign_ud;
#endif
};

/* Returns the highest used index in subject (or issuer) DN of
 * certificate CERT for OID, or -1 if no RDNs are present in the DN
 * using that OID. */
static int oid_find_highest_index(gnutls_x509_crt_t cert, int subject, const char *oid)
{
    int ret, idx = -1;

    do {
        size_t len = 0;

        if (subject)
            ret = gnutls_x509_crt_get_dn_by_oid(cert, oid, ++idx, 0, 
                                                NULL, &len);
        else
            ret = gnutls_x509_crt_get_issuer_dn_by_oid(cert, oid, ++idx, 0, 
                                                       NULL, &len);
    } while (ret == GNUTLS_E_SHORT_MEMORY_BUFFER);
    
    return idx - 1;
}

#ifdef HAVE_GNUTLS_X509_DN_GET_RDN_AVA
/* New-style RDN handling introduced in GnuTLS 1.7.x. */

#ifdef HAVE_ICONV
static void convert_dirstring(ne_buffer *buf, const char *charset, 
                              gnutls_datum_t *data)
{
    iconv_t id = iconv_open("UTF-8", charset);
    size_t inlen = data->size, outlen = buf->length - buf->used;
    char *inbuf = (char *)data->data;
    char *outbuf = buf->data + buf->used - 1;
    
    if (id == (iconv_t)-1) {
        char err[128];

        ne_buffer_snprintf(buf, 128, "[unprintable in %s: %s]",
                           charset,
                           ne_strerror(errno, err, sizeof err));
        return;
    }
    
    ne_buffer_grow(buf, buf->used + 64);
    
    while (inlen && outlen 
           && iconv(id, &inbuf, &inlen, &outbuf, &outlen) == 0)
        ;
    
    iconv_close(id);
    buf->used += buf->length - buf->used - outlen;
    buf->data[buf->used - 1] = '\0';
}
#endif

/* From section 11.13 of the Dubuisson ASN.1 bible: */
#define TAG_UTF8 (12)
#define TAG_PRINTABLE (19)
#define TAG_T61 (20)
#define TAG_IA5 (22)
#define TAG_VISIBLE (26)
#define TAG_UNIVERSAL (28)
#define TAG_BMP (30)

static void append_dirstring(ne_buffer *buf, gnutls_datum_t *data, unsigned long tag)
{
    switch (tag) {
    case TAG_UTF8:
    case TAG_IA5:
    case TAG_PRINTABLE:
    case TAG_VISIBLE:
        ne_buffer_append(buf, (char *)data->data, data->size);
        break;
#ifdef HAVE_ICONV
    case TAG_T61:
        convert_dirstring(buf, "ISO-8859-1", data);
        break;
    case TAG_BMP:
        convert_dirstring(buf, "UCS-2BE", data);
        break;
#endif
    default:
        ne_buffer_snprintf(buf, 128, _("[unprintable:#%lu]"), tag);
        break;
    }
}

/* OIDs to not include in readable DNs by default: */
#define OID_emailAddress "1.2.840.113549.1.9.1"
#define OID_commonName "2.5.4.3"

#define CMPOID(a,o) ((a)->oid.size == sizeof(o)                        \
                     && memcmp((a)->oid.data, o, strlen(o)) == 0)

char *ne_ssl_readable_dname(const ne_ssl_dname *name)
{
    gnutls_x509_dn_t dn;
    int ret, rdn = 0;
    ne_buffer *buf;
    gnutls_x509_ava_st val;

#ifdef HAVE_NEW_DN_API
    dn = name->dn;
#else
    if (name->subject)
        ret = gnutls_x509_crt_get_subject(name->cert, &dn);
    else
        ret = gnutls_x509_crt_get_issuer(name->cert, &dn);
    
    if (ret)
        return ne_strdup(_("[unprintable]"));
#endif /* HAVE_NEW_DN_API */

    buf = ne_buffer_create();
    
    /* Find the highest rdn... */
    while (gnutls_x509_dn_get_rdn_ava(dn, rdn++, 0, &val) == 0)
        ;        

    /* ..then iterate back to the first: */
    while (--rdn >= 0) {
        int ava = 0;

        /* Iterate through all AVAs for multivalued AVAs; better than
         * ne_openssl can do! */
        do {
            ret = gnutls_x509_dn_get_rdn_ava(dn, rdn, ava, &val);

            /* If the *only* attribute to append is the common name or
             * email address, use it; otherwise skip those
             * attributes. */
            if (ret == 0 && val.value.size > 0
                && ((!CMPOID(&val, OID_emailAddress)
                     && !CMPOID(&val, OID_commonName))
                    || (buf->used == 1 && rdn == 0))) {
                if (buf->used > 1) ne_buffer_append(buf, ", ", 2);

                append_dirstring(buf, &val.value, val.value_tag);
            }
            
            ava++;
        } while (ret == 0);
    }

    return ne_buffer_finish(buf);
}

#else /* !HAVE_GNUTLS_X509_DN_GET_RDN_AVA */

/* Appends the value of RDN with given oid from certitifcate x5
 * subject (if subject is non-zero), or issuer DN to buffer 'buf': */
static void append_rdn(ne_buffer *buf, gnutls_x509_crt_t x5, int subject, const char *oid)
{
    int idx, top, ret;
    char rdn[50];

    top = oid_find_highest_index(x5, subject, oid);
    
    for (idx = top; idx >= 0; idx--) {
        size_t rdnlen = sizeof rdn;

        if (subject)
            ret = gnutls_x509_crt_get_dn_by_oid(x5, oid, idx, 0, rdn, &rdnlen);
        else
            ret = gnutls_x509_crt_get_issuer_dn_by_oid(x5, oid, idx, 0, rdn, &rdnlen);
        
        if (ret < 0)
            return;

        if (buf->used > 1) {
            ne_buffer_append(buf, ", ", 2);
        }
        
        ne_buffer_append(buf, rdn, rdnlen);
    }
}

char *ne_ssl_readable_dname(const ne_ssl_dname *name)
{
    ne_buffer *buf = ne_buffer_create();
    int ret, idx = 0;

    do {
        char oid[32] = {0};
        size_t oidlen = sizeof oid;
        
        ret = name->subject 
            ? gnutls_x509_crt_get_dn_oid(name->cert, idx, oid, &oidlen)
            : gnutls_x509_crt_get_issuer_dn_oid(name->cert, idx, oid, &oidlen);
        
        if (ret == 0) {
            append_rdn(buf, name->cert, name->subject, oid);
            idx++;
        }
    } while (ret != GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE);

    return ne_buffer_finish(buf);
}
#endif /* HAVE_GNUTLS_X509_DN_GET_RDN_AVA */

int ne_ssl_dname_cmp(const ne_ssl_dname *dn1, const ne_ssl_dname *dn2)
{
    char c1[1024], c2[1024];
    size_t s1 = sizeof c1, s2 = sizeof c2;

#ifdef HAVE_NEW_DN_API
    if (gnutls_x509_dn_export(dn1->dn, GNUTLS_X509_FMT_DER, c1, &s1))
        return 1;
        
    if (gnutls_x509_dn_export(dn2->dn, GNUTLS_X509_FMT_DER, c2, &s2))
        return -1;
#else
    int ret;

    if (dn1->subject)
        ret = gnutls_x509_crt_get_dn(dn1->cert, c1, &s1);
    else
        ret = gnutls_x509_crt_get_issuer_dn(dn1->cert, c1, &s1);
    if (ret)
        return 1;

    if (dn2->subject)
        ret = gnutls_x509_crt_get_dn(dn2->cert, c2, &s2);
    else
        ret = gnutls_x509_crt_get_issuer_dn(dn2->cert, c2, &s2);
    if (ret)
        return -1;
#endif /* HAVE_NEW_DN_API */
    
    if (s1 != s2)
        return s2 - s1;

    return memcmp(c1, c2, s1);
}

void ne_ssl_clicert_free(ne_ssl_client_cert *cc)
{
    if (cc->p12)
        gnutls_pkcs12_deinit(cc->p12);
    if (cc->decrypted) {
        if (cc->cert.identity) ne_free(cc->cert.identity);
        if (cc->pkey) gnutls_x509_privkey_deinit(cc->pkey);
        if (cc->cert.subject) gnutls_x509_crt_deinit(cc->cert.subject);
    }
    if (cc->friendly_name) ne_free(cc->friendly_name);
    ne_free(cc);
}

void ne_ssl_cert_validity_time(const ne_ssl_certificate *cert,
                               time_t *from, time_t *until)
{
    if (from) {
        *from = gnutls_x509_crt_get_activation_time(cert->subject);
    }
    if (until) {
        *until = gnutls_x509_crt_get_expiration_time(cert->subject);
    }
}

/* Check certificate identity.  Returns zero if identity matches; 1 if
 * identity does not match, or <0 if the certificate had no identity.
 * If 'identity' is non-NULL, store the malloc-allocated identity in
 * *identity.  If 'server' is non-NULL, it must be the network address
 * of the server in use, and identity must be NULL. */
static int check_identity(const struct host_info *server, gnutls_x509_crt_t cert,
                          char **identity)
{
    char name[255];
    unsigned int critical;
    int ret, seq = 0;
    int match = 0, found = 0;
    size_t len;
    const char *hostname;
    
    hostname = server ? server->hostname : "";

    do {
        len = sizeof name - 1;
        ret = gnutls_x509_crt_get_subject_alt_name(cert, seq, name, &len,
                                                   &critical);
        switch (ret) {
        case GNUTLS_SAN_DNSNAME:
            name[len] = '\0';
            if (identity && !found) *identity = ne_strdup(name);
            /* Only match if the server was not identified by a
             * literal IP address; avoiding wildcard matches. */
            if (server && !server->literal)
                match = ne__ssl_match_hostname(name, len, hostname);
            found = 1;
            break;
        case GNUTLS_SAN_IPADDRESS: {
            ne_inet_addr *ia;
            if (len == 4)
                ia = ne_iaddr_make(ne_iaddr_ipv4, (unsigned char *)name);
            else if (len == 16)
                ia = ne_iaddr_make(ne_iaddr_ipv6, (unsigned char *)name);
            else 
                ia = NULL;
            if (ia) {
                if (server && server->literal)
                    match = ne_iaddr_cmp(ia, server->literal) == 0;
                found = 1;
                ne_iaddr_free(ia);
            }
            else {
                NE_DEBUG(NE_DBG_SSL, "iPAddress name with unsupported "
                         "address type (length %" NE_FMT_SIZE_T "), skipped.\n",
                         len);
            }
        } break;

        default:
            break;
        }
        seq++;
    } while (!match && ret != GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE);

    /* Check against the commonName if no DNS alt. names were found,
     * as per RFC3280. */
    if (!found) {
        seq = oid_find_highest_index(cert, 1, GNUTLS_OID_X520_COMMON_NAME);

        if (seq >= 0) {
            len = sizeof name;
            name[0] = '\0';
            ret = gnutls_x509_crt_get_dn_by_oid(cert, GNUTLS_OID_X520_COMMON_NAME,
                                                seq, 0, name, &len);
            if (ret == 0) {
                if (identity) *identity = ne_strdup(name);
                if (server && !server->literal)
                    match = ne__ssl_match_hostname(name, len, hostname);
            }
        } else {
            return -1;
        }
    }

    if (*hostname)
        NE_DEBUG(NE_DBG_SSL, "ssl: Identity match for '%s': %s\n", hostname, 
                 match ? "good" : "bad");

    return match ? 0 : 1;
}

/* Populate an ne_ssl_certificate structure from an X509 object.  Note
 * that x5 is owned by returned cert object and must not be otherwise
 * freed by the caller.  */
static ne_ssl_certificate *populate_cert(ne_ssl_certificate *cert,
                                         gnutls_x509_crt_t x5)
{
#ifdef HAVE_NEW_DN_API
    gnutls_x509_crt_get_subject(x5, &cert->subj_dn.dn);
    gnutls_x509_crt_get_issuer(x5, &cert->issuer_dn.dn);
#else
    cert->subj_dn.cert = x5;
    cert->subj_dn.subject = 1;
    cert->issuer_dn.cert = x5;
    cert->issuer_dn.subject = 0;
#endif
    cert->issuer = NULL;
    cert->subject = x5;
    cert->identity = NULL;
    check_identity(NULL, x5, &cert->identity);
    return cert;
}

/* Returns a copy certificate of certificate SRC. */
static gnutls_x509_crt_t x509_crt_copy(gnutls_x509_crt_t src)
{
    int ret;
    size_t size = 0;
    gnutls_datum_t tmp;
    gnutls_x509_crt_t dest;
    
    if (gnutls_x509_crt_init(&dest) != 0) {
        return NULL;
    }

    if (gnutls_x509_crt_export(src, GNUTLS_X509_FMT_DER, NULL, &size) 
        != GNUTLS_E_SHORT_MEMORY_BUFFER) {
        gnutls_x509_crt_deinit(dest);
        return NULL;
    }

    tmp.data = ne_malloc(size);
    ret = gnutls_x509_crt_export(src, GNUTLS_X509_FMT_DER, tmp.data, &size);
    if (ret == 0) {
        tmp.size = size;
        ret = gnutls_x509_crt_import(dest, &tmp, GNUTLS_X509_FMT_DER);
    }

    if (ret) {
        gnutls_x509_crt_deinit(dest);
        dest = NULL;
    }

    ne_free(tmp.data);
    return dest;
}

/* Duplicate a client certificate, which must be in the decrypted state. */
static ne_ssl_client_cert *dup_client_cert(const ne_ssl_client_cert *cc)
{
    int ret;
    ne_ssl_client_cert *newcc = ne_calloc(sizeof *newcc);

    newcc->decrypted = 1;
    
    if (cc->keyless) {
        newcc->keyless = 1;
#ifdef HAVE_GNUTLS_PRIVKEY_IMPORT_EXT
        newcc->sign_func = cc->sign_func;
        newcc->sign_ud = cc->sign_ud;
#endif
    }
    else {
        ret = gnutls_x509_privkey_init(&newcc->pkey);
        if (ret != 0) goto dup_error;
        
        ret = gnutls_x509_privkey_cpy(newcc->pkey, cc->pkey);
        if (ret != 0) goto dup_error;
    }    

    newcc->cert.subject = x509_crt_copy(cc->cert.subject);
    if (!newcc->cert.subject) goto dup_error;

    if (cc->friendly_name) newcc->friendly_name = ne_strdup(cc->friendly_name);

    populate_cert(&newcc->cert, newcc->cert.subject);
    return newcc;

dup_error:
    if (newcc->pkey) gnutls_x509_privkey_deinit(newcc->pkey);
    if (newcc->cert.subject) gnutls_x509_crt_deinit(newcc->cert.subject);
    ne_free(newcc);
    return NULL;
}    

/* Callback invoked when the SSL server requests a client certificate.  */
static int provide_client_cert(gnutls_session_t session,
                               const gnutls_datum_t *req_ca_rdn, int nreqs,
                               const gnutls_pk_algorithm_t *sign_algos,
                               int sign_algos_length, 
#ifdef HAVE_GNUTLS_CERTIFICATE_SET_RETRIEVE_FUNCTION2
                               gnutls_pcert_st **pcert, 
                               unsigned int *pcert_length, 
                               gnutls_privkey_t *pkey
#else
                               gnutls_retr2_st *st
#endif
    )
{
    ne_session *sess = gnutls_session_get_ptr(session);
    
    if (!sess) {
        return GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER;
    }

    NE_DEBUG(NE_DBG_SSL, "ssl: Client cert provider callback; %d CA names.\n",
             nreqs);

    if (!sess->client_cert && sess->ssl_provide_fn) {
#ifdef HAVE_NEW_DN_API
        const ne_ssl_dname **dns;
        ne_ssl_dname *dnarray;
        unsigned dncount = 0;
        int n;

        dns = ne_malloc(nreqs * sizeof(ne_ssl_dname *));
        dnarray = ne_calloc(nreqs * sizeof(ne_ssl_dname));

        for (n = 0; n < nreqs; n++) {
            gnutls_x509_dn_t dn;

            if (gnutls_x509_dn_init(&dn) == 0) {
                dnarray[n].dn = dn;
                if (gnutls_x509_dn_import(dn, &req_ca_rdn[n]) == 0) {
                    dns[dncount++] = &dnarray[n];
                }
                else {
                    gnutls_x509_dn_deinit(dn);
                }            
            }
        }
       
        NE_DEBUG(NE_DBG_SSL, "ssl: Mapped %d CA names to %u DN objects.\n",
                 nreqs, dncount);

        sess->ssl_provide_fn(sess->ssl_provide_ud, sess, dns, dncount);
        
        for (n = 0; n < nreqs; n++) {
            if (dnarray[n].dn) {
                gnutls_x509_dn_deinit(dnarray[n].dn);
            }
        }

        ne_free(dns);
        ne_free(dnarray);
#else /* HAVE_NEW_DN_API */
        /* Nothing to do here other than pretend no CA names were
         * given, and hope the caller can cope. */
        sess->ssl_provide_fn(sess->ssl_provide_ud, sess, NULL, 0);
#endif
    }

    if (sess->client_cert) {
        gnutls_certificate_type_t type = gnutls_certificate_type_get(session);
        if (type == GNUTLS_CRT_X509
            && (sess->client_cert->pkey || sess->client_cert->keyless)) {
            int ret;

#ifdef HAVE_GNUTLS_CERTIFICATE_SET_RETRIEVE_FUNCTION2
            *pkey = gnutls_malloc(sizeof *pkey);
            gnutls_privkey_init(pkey);

#ifdef HAVE_GNUTLS_PRIVKEY_IMPORT_EXT
            if (sess->client_cert->sign_func) {
                int algo = gnutls_x509_crt_get_pk_algorithm(sess->client_cert->cert.subject, NULL);
                NE_DEBUG(NE_DBG_SSL, "ssl: Signing for %s.\n", gnutls_pk_algorithm_get_name(algo));
                         
                ret = gnutls_privkey_import_ext(*pkey, algo, sess->client_cert->sign_ud,
                                                sess->client_cert->sign_func, NULL, 0);
            }
            else
#endif
            if (sess->client_cert->keyless) {
                ret = GNUTLS_E_UNSUPPORTED_CERTIFICATE_TYPE;
            }
            else {
                ret = gnutls_privkey_import_x509(*pkey, sess->client_cert->pkey, 0);
            }

            if (ret) {
                NE_DEBUG(NE_DBG_SSL, "ssl: Failed to import private key: %s.\n", gnutls_strerror(ret));
                ne_set_error(sess, _("Failed to import private key: %s"), gnutls_strerror(ret));
                return ret;
            }
            
            *pcert = gnutls_calloc(1, sizeof **pcert);
            gnutls_pcert_import_x509(*pcert, sess->client_cert->cert.subject, 0);
            *pcert_length = 1;
#else /* !HAVE_GNUTLS_CERTIFICATE_SET_RETRIEVE_FUNCTION2 */
            st->cert_type = type;
            st->ncerts = 1;
            st->cert.x509 = &sess->client_cert->cert.subject;
            st->key.x509 = sess->client_cert->pkey;
            
            /* tell GNU TLS not to deallocate the certs. */
            st->deinit_all = 0;
#endif
        } else {
            return GNUTLS_E_UNSUPPORTED_CERTIFICATE_TYPE;
        }
    } 
    else {
        NE_DEBUG(NE_DBG_SSL, "ssl: No client certificate supplied.\n");
#ifdef HAVE_GNUTLS_CERTIFICATE_SET_RETRIEVE_FUNCTION2
        *pcert_length = 0;
#else        
        st->ncerts = 0;
#endif
        sess->ssl_cc_requested = 1;
        return 0;
    }

    return 0;
}

void ne_ssl_set_clicert(ne_session *sess, const ne_ssl_client_cert *cc)
{
    sess->client_cert = dup_client_cert(cc);
}

ne_ssl_context *ne_ssl_context_create(int flags)
{
    ne_ssl_context *ctx = ne_calloc(sizeof *ctx);
    gnutls_certificate_allocate_credentials(&ctx->cred);
    if (flags == NE_SSL_CTX_CLIENT) {
#ifdef HAVE_GNUTLS_CERTIFICATE_SET_RETRIEVE_FUNCTION2
        gnutls_certificate_set_retrieve_function2(ctx->cred, provide_client_cert);
#else
        gnutls_certificate_client_set_retrieve_function(ctx->cred,
                                                        provide_client_cert);
#endif
    }
    gnutls_certificate_set_verify_flags(ctx->cred, 
                                        GNUTLS_VERIFY_ALLOW_X509_V1_CA_CRT);
    return ctx;
}

int ne_ssl_context_keypair(ne_ssl_context *ctx, 
                           const char *cert, const char *key)
{
    gnutls_certificate_set_x509_key_file(ctx->cred, cert, key,
                                         GNUTLS_X509_FMT_PEM);
    return 0;
}

int ne_ssl_context_set_verify(ne_ssl_context *ctx, int required,
                              const char *ca_names, const char *verify_cas)
{
    ctx->verify = required;
    if (verify_cas) {
        gnutls_certificate_set_x509_trust_file(ctx->cred, verify_cas,
                                               GNUTLS_X509_FMT_PEM);
    }
    /* gnutls_certificate_send_x509_rdn_sequence in gnutls >= 1.2 can
     * be used to *suppress* sending the CA names, but not control it,
     * it seems. */
    return 0;
}

void ne_ssl_context_set_flag(ne_ssl_context *ctx, int flag, int value)
{
}

int ne_ssl_context_get_flag(ne_ssl_context *ctx, int flag)
{
    return 0;
}

int ne_ssl_context_set_versions(ne_ssl_context *ctx, enum ne_ssl_protocol min,
                                enum ne_ssl_protocol max)
{
#ifdef HAVE_GNUTLS_SET_DEFAULT_PRIORITY_APPEND
    ne_buffer *prio;

    if (min > max) return NE_SOCK_ERROR;

    if (ctx->priority) {
        ne_free(ctx->priority);
        ctx->priority = NULL;
    }

    if (min == 0 && max == 0) {
        return 0;
    }

    prio = ne_buffer_create();
    ne_buffer_czappend(prio, "+VERS-ALL");

    if (min > NE_SSL_PROTO_SSL_3)
        ne_buffer_czappend(prio, ":-VERS-SSL3.0");
    if (min > NE_SSL_PROTO_TLS_1_0)
        ne_buffer_czappend(prio, ":-VERS-TLS1.0");
    if (min > NE_SSL_PROTO_TLS_1_1)
        ne_buffer_czappend(prio, ":-VERS-TLS1.1");
    if (min > NE_SSL_PROTO_TLS_1_2)
        ne_buffer_czappend(prio, ":-VERS-TLS1.2");

    if (max && max < NE_SSL_PROTO_TLS_1_3)
        ne_buffer_czappend(prio, ":-VERS-TLS1.3");
    if (max && max < NE_SSL_PROTO_TLS_1_2)
        ne_buffer_czappend(prio, ":-VERS-TLS1.2");
    if (max && max < NE_SSL_PROTO_TLS_1_1)
        ne_buffer_czappend(prio, ":-VERS-TLS1.1");
    if (max && max < NE_SSL_PROTO_TLS_1_0)
        ne_buffer_czappend(prio, ":-VERS-TLS1.0");

    ctx->priority = ne_buffer_finish(prio);

    return 0;
#else
    return NE_SOCK_ERROR;
#endif
}

void ne_ssl_context_destroy(ne_ssl_context *ctx)
{
    gnutls_certificate_free_credentials(ctx->cred);
    if (ctx->cache.client.data) {
#if defined(HAVE_GNUTLS_SESSION_GET_DATA2)
        gnutls_free(ctx->cache.client.data);
#else
        ne_free(ctx->cache.client.data);
#endif
    } else if (ctx->cache.server.key.data) {
        gnutls_free(ctx->cache.server.key.data);
        gnutls_free(ctx->cache.server.data.data);
    }
    if (ctx->priority) ne_free(ctx->priority);
    ne_free(ctx);
}

#if !defined(HAVE_GNUTLS_CERTIFICATE_GET_ISSUER) && defined(HAVE_GNUTLS_CERTIFICATE_GET_X509_CAS)
/* Return the issuer of the given certificate, or NULL if none can be
 * found. */
static gnutls_x509_crt_t find_issuer(gnutls_x509_crt_t *ca_list,
                                   unsigned int num_cas,
                                   gnutls_x509_crt_t cert)
{
    unsigned int n;

    for (n = 0; n < num_cas; n++) {
        if (gnutls_x509_crt_check_issuer(cert, ca_list[n]) == 1)
            return ca_list[n];
    }

    return NULL;
}
#endif

/* Return the certificate chain sent by the peer, or NULL on error. */
static ne_ssl_certificate *make_peers_chain(gnutls_session_t sock,
                                            gnutls_certificate_credentials_t crd)
{
    ne_ssl_certificate *current = NULL, *top = NULL;
    const gnutls_datum_t *certs;
    unsigned int n, count;
    ne_ssl_certificate *cert;

    certs = gnutls_certificate_get_peers(sock, &count);
    if (!certs) {
        return NULL;
    }

    NE_DEBUG(NE_DBG_SSL, "ssl: Got %u certs in peer chain.\n", count);
    
    for (n = 0; n < count; n++) {
        gnutls_x509_crt_t x5;

        if (gnutls_x509_crt_init(&x5) ||
            gnutls_x509_crt_import(x5, &certs[n], GNUTLS_X509_FMT_DER)) {
            if (top) {
                ne_ssl_cert_free(top);
            }
            return NULL;
        }

        cert = populate_cert(ne_calloc(sizeof *cert), x5);
        
        if (top == NULL) {
            current = top = cert;
        } else {
            current->issuer = cert;
            current = cert;
        }
    }

#if defined(HAVE_GNUTLS_CERTIFICATE_GET_ISSUER) || defined(HAVE_GNUTLS_CERTIFICATE_GET_X509_CAS)
    /* GnuTLS only returns the peers which were *sent* by the server
     * in the Certificate list during the handshake.  Fill in the
     * complete chain manually against the certs we trust: */
    if (current->issuer == NULL) {
        gnutls_x509_crt_t issuer;

#ifndef HAVE_GNUTLS_CERTIFICATE_GET_ISSUER
        gnutls_x509_crt_t *ca_list;
        unsigned int num_cas;
        
        gnutls_certificate_get_x509_cas(crd, &ca_list, &num_cas);
#endif

        do { 
            /* Look up the issuer. */
#ifndef HAVE_GNUTLS_CERTIFICATE_GET_ISSUER
            issuer = find_issuer(ca_list, num_cas, current->subject);
#else
            if (gnutls_certificate_get_issuer(crd, current->subject, &issuer, 0))
                issuer = NULL;
#endif

            if (issuer) {
                issuer = x509_crt_copy(issuer);
                if (issuer == NULL)
                    break;

                cert = populate_cert(ne_calloc(sizeof *cert), issuer);
                /* Check that the issuer does not match the current
                 * cert. */
                if (ne_ssl_cert_cmp(current, cert)) {
                    current = current->issuer = cert;
                }
                else {
                    ne_ssl_cert_free(cert);
                    issuer = NULL;
                }
            }
        } while (issuer);
    }
#endif
    
    return top;
}

/* Map from GnuTLS verify failure mask *status to NE_SSL_* failure
 * bitmask, which is returned.  *status is modified, removing all
 * mapped bits. */
static int map_verify_failures(unsigned int *status)
{
    static const struct {
        gnutls_certificate_status_t from;
        int to;
    } map[] = {
        { GNUTLS_CERT_REVOKED, NE_SSL_REVOKED },
#if LIBGNUTLS_VERSION_NUMBER >= 0x020800
        { GNUTLS_CERT_NOT_ACTIVATED, NE_SSL_NOTYETVALID },
        { GNUTLS_CERT_EXPIRED, NE_SSL_EXPIRED },
#endif
        { GNUTLS_CERT_INVALID|GNUTLS_CERT_SIGNER_NOT_FOUND, NE_SSL_UNTRUSTED },
        { GNUTLS_CERT_INVALID|GNUTLS_CERT_SIGNER_NOT_CA, NE_SSL_UNTRUSTED }
    };
    size_t n;
    int ret = 0;

    for (n = 0; n < sizeof(map)/sizeof(map[0]); n++) {
        if ((*status & map[n].from) == map[n].from) {
            *status &= ~map[n].from;
            ret |= map[n].to;
        }
    }

    return ret;
}

/* Return a malloc-allocated human-readable error string describing
 * GnuTLS verification error bitmask 'status'; return value must be
 * freed by the caller. */
static char *verify_error_string(unsigned int status)
{
    ne_buffer *buf = ne_buffer_create();

    /* sorry, i18n-ers */
    if (status & GNUTLS_CERT_INSECURE_ALGORITHM) {
        ne_buffer_zappend(buf, _("signed using insecure algorithm"));
    }
    else {
        ne_buffer_snprintf(buf, 64, _("unrecognized errors (%u)"),
                           status);
    }
    
    return ne_buffer_finish(buf);
}

/* Return NE_SSL_* failure bits after checking chain expiry. */
static int check_chain_expiry(ne_ssl_certificate *chain)
{
    time_t before, after, now = time(NULL);
    ne_ssl_certificate *cert;
    int failures = 0;
    
    /* Check that all certs within the chain are inside their defined
     * validity period.  Note that the errors flagged for the server
     * cert are different from the generic error for issues higher up
     * the chain. */
    for (cert = chain; cert; cert = cert->issuer) {
        before = gnutls_x509_crt_get_activation_time(cert->subject);
        after = gnutls_x509_crt_get_expiration_time(cert->subject);
        
        if (now < before)
            failures |= (cert == chain) ? NE_SSL_NOTYETVALID : NE_SSL_BADCHAIN;
        else if (now > after)
            failures |= (cert == chain) ? NE_SSL_EXPIRED : NE_SSL_BADCHAIN;
    }

    return failures;
}

/* Verifies an SSL server certificate. */
static int check_certificate(ne_session *sess, gnutls_session_t sock,
                             ne_ssl_certificate *chain)
{
    int ret, failures = 0;
    unsigned int status;

    ret = check_identity(&sess->server, chain->subject, NULL);

    if (ret < 0) {
        ne_set_error(sess, _("Server certificate was missing commonName "
                             "attribute in subject name"));
        return NE_ERROR;
    } 
    else if (ret > 0) {
        failures |= NE_SSL_IDMISMATCH;
    }
    
    failures |= check_chain_expiry(chain);

    ret = gnutls_certificate_verify_peers2(sock, &status);
    NE_DEBUG(NE_DBG_SSL, "ssl: Verify peers returned %d, status=%u\n", 
             ret, status);
    if (ret != GNUTLS_E_SUCCESS) {
        ne_set_error(sess, _("Could not verify server certificate: %s"),
                     gnutls_strerror(ret));
        return NE_ERROR;
    }

    ret = map_verify_failures(&status);
    /* For CA expiry/not-yet-valid, mask out the failure bits if
     * they've been caught and treated as chain errors already by check_chain_expiry(). */
    if ((ret & (NE_SSL_EXPIRED|NE_SSL_NOTYETVALID)) != 0
        && (failures & NE_SSL_BADCHAIN) == NE_SSL_BADCHAIN) {
        ret &= ~(NE_SSL_EXPIRED|NE_SSL_NOTYETVALID);
    }
    failures |= ret;

    NE_DEBUG(NE_DBG_SSL, "ssl: Verification failures %d => %d (status = %u).\n",
             ret, failures, status);
    
    if (status && status != GNUTLS_CERT_INVALID) {
        char *errstr = verify_error_string(status);
        ne_set_error(sess, _("Certificate verification error: %s"), errstr);
        ne_free(errstr);       
        return NE_ERROR;
    }

    if (failures == 0) {
        ret = NE_OK;
    } else {
        ne__ssl_set_verify_err(sess, failures);
        ret = NE_ERROR;
        if (sess->ssl_verify_fn
            && sess->ssl_verify_fn(sess->ssl_verify_ud, failures, chain) == 0)
            ret = NE_OK;
    }

    return ret;
}

/* Negotiate an SSL connection. */
int ne__negotiate_ssl(ne_session *sess)
{
    ne_ssl_context *const ctx = sess->ssl_context;
    ne_ssl_certificate *chain;
    gnutls_session_t sock;

    NE_DEBUG(NE_DBG_SSL, "Negotiating SSL connection.\n");

    /* Pass through the hostname if SNI is enabled. */
    ctx->hostname = 
        sess->flags[NE_SESSFLAG_TLS_SNI] ? sess->server.hostname : NULL;

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

    sock = ne__sock_sslsock(sess->socket);

    chain = make_peers_chain(sock, ctx->cred);
    if (chain == NULL) {
        ne_set_error(sess, _("Server did not send certificate chain"));
        return NE_ERROR;
    }

    if (sess->server_cert && ne_ssl_cert_cmp(sess->server_cert, chain) == 0) {
        /* Same cert as last time; presume OK.  This is not optimal as
         * make_peers_chain() has already gone through and done the
         * expensive DER parsing stuff for the whole chain by now. */
        ne_ssl_cert_free(chain);
        return NE_OK;
    }

    if (check_certificate(sess, sock, chain)) {
        ne_ssl_cert_free(chain);
        return NE_ERROR;
    }

    sess->server_cert = chain;

    if (sess->notify_cb) {
        memset(&sess->status, 0, sizeof sess->status);
        sess->status.hs.protocol = ne_sock_getproto(sess->socket);
#if LIBGNUTLS_VERSION_NUMBER >= 0x030704
        sess->status.hs.ciphersuite = gnutls_ciphersuite_get(sock);
#endif
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
    gnutls_x509_crt_t certs = cert->subject;
    gnutls_certificate_set_x509_trust(ctx->cred, &certs, 1);
}

void ne_ssl_trust_default_ca(ne_session *sess)
{
    if (sess->ssl_context) {
#ifdef NE_SSL_CA_BUNDLE
        gnutls_certificate_set_x509_trust_file(sess->ssl_context->cred,
                                               NE_SSL_CA_BUNDLE,
                                               GNUTLS_X509_FMT_PEM);
#elif defined(HAVE_GNUTLS_CERTIFICATE_SET_X509_SYSTEM_TRUST)
        int rv = gnutls_certificate_set_x509_system_trust(sess->ssl_context->cred);

        NE_DEBUG(NE_DBG_SSL, "ssl: System certificates trusted (%d)\n", rv);
#endif
    }
}

/* Read the contents of file FILENAME into *DATUM. */
static int read_to_datum(const char *filename, gnutls_datum_t *datum)
{
    FILE *f = fopen(filename, "r");
    ne_buffer *buf;
    char tmp[4192];
    size_t len;

    if (!f) {
        return -1;
    }

    buf = ne_buffer_ncreate(8192);
    while ((len = fread(tmp, 1, sizeof tmp, f)) > 0) {
        ne_buffer_append(buf, tmp, len);
    }

    if (!feof(f)) {
        fclose(f);
        ne_buffer_destroy(buf);
        return -1;
    }
    
    fclose(f);

    datum->size = ne_buffer_size(buf);
    datum->data = (unsigned char *)ne_buffer_finish(buf);
    return 0;
}

/* Parses a PKCS#12 structure and loads the certificate, private key
 * and friendly name if possible.  Returns zero on success, non-zero
 * on error. */
static int pkcs12_parse(gnutls_pkcs12_t p12, gnutls_x509_privkey_t *pkey,
                        gnutls_x509_crt_t *x5, char **friendly_name,
                        const char *password)
{
    gnutls_pkcs12_bag_t bag = NULL;
    int i, j, ret = 0;

    for (i = 0; ret == 0; ++i) {
        if (bag) gnutls_pkcs12_bag_deinit(bag);

        ret = gnutls_pkcs12_bag_init(&bag);
        if (ret < 0) continue;

        ret = gnutls_pkcs12_get_bag(p12, i, bag);
        if (ret < 0) continue;

        gnutls_pkcs12_bag_decrypt(bag, password);

        for (j = 0; ret == 0 && j < gnutls_pkcs12_bag_get_count(bag); ++j) {
            gnutls_pkcs12_bag_type_t type;
            gnutls_datum_t data;

            if (friendly_name && *friendly_name == NULL) {
                char *name = NULL;
                gnutls_pkcs12_bag_get_friendly_name(bag, j, &name);
                if (name) {
                    if (name[0] == '.') name++; /* weird GnuTLS bug? */
                    *friendly_name = ne_strdup(name);
                }
            }

            type = gnutls_pkcs12_bag_get_type(bag, j);
            switch (type) {
            case GNUTLS_BAG_PKCS8_KEY:
            case GNUTLS_BAG_PKCS8_ENCRYPTED_KEY:
                /* Ignore any but the first key encountered; really
                 * need to match up keyids. */
                if (*pkey) break;

                gnutls_x509_privkey_init(pkey);

                ret = gnutls_pkcs12_bag_get_data(bag, j, &data);
                if (ret < 0) continue;

                ret = gnutls_x509_privkey_import_pkcs8(*pkey, &data,
                                                       GNUTLS_X509_FMT_DER,
                                                       password,
                                                       0);
                if (ret < 0) continue;
                break;
            case GNUTLS_BAG_CERTIFICATE:
                /* Ignore any but the first cert encountered; again,
                 * really need to match up keyids. */
                if (*x5) break;

                ret = gnutls_x509_crt_init(x5);
                if (ret < 0) continue;

                ret = gnutls_pkcs12_bag_get_data(bag, j, &data);
                if (ret < 0) continue;

                ret = gnutls_x509_crt_import(*x5, &data, GNUTLS_X509_FMT_DER);
                if (ret < 0) continue;

                break;
            default:
                break;
            }
        }
    }

    /* Make sure last bag is freed */
    if (bag) gnutls_pkcs12_bag_deinit(bag);

    /* Free in case of error */
    if (ret < 0 && ret != GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE) {
        if (*x5) gnutls_x509_crt_deinit(*x5);
        if (*pkey) gnutls_x509_privkey_deinit(*pkey);
        if (friendly_name && *friendly_name) ne_free(*friendly_name);
    }

    if (ret == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE) ret = 0;
    return ret;
}

ne_ssl_client_cert *ne_ssl_clicert_read(const char *filename)
{
    gnutls_datum_t datum;
    ne_ssl_client_cert *cc;

    if (read_to_datum(filename, &datum))
        return NULL;

    cc = ne_ssl_clicert_import(datum.data, datum.size);

    ne_free(datum.data);

    return cc;
}

ne_ssl_client_cert *ne_ssl_clicert_import(const unsigned char *buffer, size_t buflen)
{
    int ret;
    gnutls_datum_t data;
    gnutls_pkcs12_t p12;
    ne_ssl_client_cert *cc;
    char *friendly_name = NULL;
    gnutls_x509_crt_t cert = NULL;
    gnutls_x509_privkey_t pkey = NULL;

    /* The datum structure is not modified by gnutls_pkcs12_import,
     * cast safely: */
    data.data = (unsigned char *)buffer;
    data.size = buflen;

    if (gnutls_pkcs12_init(&p12) != 0) {
        return NULL;
    }

    ret = gnutls_pkcs12_import(p12, &data, GNUTLS_X509_FMT_DER, 0);
    if (ret < 0) {
        gnutls_pkcs12_deinit(p12);
        return NULL;
    }

    if (gnutls_pkcs12_verify_mac(p12, "") == 0) {
        if (pkcs12_parse(p12, &pkey, &cert, &friendly_name, "") != 0
            || !cert || !pkey) {
            gnutls_pkcs12_deinit(p12);
            return NULL;
        }

        cc = ne_calloc(sizeof *cc);
        cc->pkey = pkey;
        cc->decrypted = 1;
        cc->friendly_name = friendly_name;
        populate_cert(&cc->cert, cert);
        gnutls_pkcs12_deinit(p12);
        cc->p12 = NULL;
        return cc;
    } else {
        /* TODO: calling pkcs12_parse() here to find the friendly_name
         * seems to break horribly.  */
        cc = ne_calloc(sizeof *cc);
        cc->p12 = p12;
        return cc;
    }
}

ne_ssl_client_cert *ne_ssl_clicert_fromuri(const char *uri,
                                           unsigned int flags)
{
    errno = ENOTSUP;
    return NULL;
}

#ifdef HAVE_GNUTLS_PRIVKEY_IMPORT_EXT
ne_ssl_client_cert *ne__ssl_clicert_exkey_import(const unsigned char *der, size_t der_len,
                                                 gnutls_privkey_sign_func sign_func,
                                                 void *userdata)
{
    ne_ssl_client_cert *cc;
    gnutls_x509_crt_t x5;
    gnutls_datum_t datum;

    datum.data = (unsigned char *)der;
    datum.size = der_len;    

    if (gnutls_x509_crt_init(&x5) 
        || gnutls_x509_crt_import(x5, &datum, GNUTLS_X509_FMT_DER)) {
        NE_DEBUG(NE_DBG_SSL, "ssl: crt_import failed.\n");
        return NULL;
    }
    
    cc = ne_calloc(sizeof *cc);
    cc->keyless = 1;
    cc->decrypted = 1;
    populate_cert(&cc->cert, x5);
    cc->sign_func = sign_func;
    cc->sign_ud = userdata;

    return cc;
}
#endif

int ne_ssl_clicert_encrypted(const ne_ssl_client_cert *cc)
{
    return !cc->decrypted;
}

int ne_ssl_clicert_decrypt(ne_ssl_client_cert *cc, const char *password)
{
    int ret;
    gnutls_x509_crt_t cert = NULL;
    gnutls_x509_privkey_t pkey = NULL;

    if (gnutls_pkcs12_verify_mac(cc->p12, password) != 0) {
        return -1;
    }        

    ret = pkcs12_parse(cc->p12, &pkey, &cert, NULL, password);
    if (ret < 0)
        return ret;
    
    if (!cert || (!pkey && !cc->keyless)) {
        if (cert) gnutls_x509_crt_deinit(cert);
        if (pkey) gnutls_x509_privkey_deinit(pkey);
        return -1;
    }

    gnutls_pkcs12_deinit(cc->p12);
    populate_cert(&cc->cert, cert);
    cc->pkey = pkey;
    cc->decrypted = 1;
    cc->p12 = NULL;
    return 0;
}

const ne_ssl_certificate *ne_ssl_clicert_owner(const ne_ssl_client_cert *cc)
{
    return &cc->cert;
}

const char *ne_ssl_clicert_name(const ne_ssl_client_cert *ccert)
{
    return ccert->friendly_name;
}

ne_ssl_certificate *ne_ssl_cert_read(const char *filename)
{
    int ret;
    gnutls_datum_t data;
    gnutls_x509_crt_t x5;

    if (read_to_datum(filename, &data))
        return NULL;

    if (gnutls_x509_crt_init(&x5) != 0)
        return NULL;

    ret = gnutls_x509_crt_import(x5, &data, GNUTLS_X509_FMT_PEM);
    ne_free(data.data);
    if (ret < 0) {
        gnutls_x509_crt_deinit(x5);
        return NULL;
    }
    
    return populate_cert(ne_calloc(sizeof(struct ne_ssl_certificate_s)), x5);
}

int ne_ssl_cert_write(const ne_ssl_certificate *cert, const char *filename)
{
    unsigned char buffer[10*1024];
    size_t len = sizeof buffer;

    FILE *fp = fopen(filename, "w");

    if (fp == NULL) return -1;

    if (gnutls_x509_crt_export(cert->subject, GNUTLS_X509_FMT_PEM, buffer,
                               &len) < 0) {
        fclose(fp);
        return -1;
    }

    if (fwrite(buffer, len, 1, fp) != 1) {
        fclose(fp);
        return -1;
    }

    if (fclose(fp) != 0)
        return -1;

    return 0;
}

void ne_ssl_cert_free(ne_ssl_certificate *cert)
{
    gnutls_x509_crt_deinit(cert->subject);
    if (cert->identity) ne_free(cert->identity);
    if (cert->issuer) ne_ssl_cert_free(cert->issuer);
    ne_free(cert);
}

int ne_ssl_cert_cmp(const ne_ssl_certificate *c1, const ne_ssl_certificate *c2)
{
#ifdef HAVE_GNUTLS_X509_CRT_EQUALS
    return !gnutls_x509_crt_equals(c1->subject, c2->subject);
#else
    char digest1[NE_SSL_DIGESTLEN], digest2[NE_SSL_DIGESTLEN];

    if (ne_ssl_cert_digest(c1, digest1) || ne_ssl_cert_digest(c2, digest2)) {
        return -1;
    }

    return strcmp(digest1, digest2);
#endif
}

/* The certificate import/export format is the base64 encoding of the
 * raw DER; PEM without the newlines and wrapping. */

ne_ssl_certificate *ne_ssl_cert_import(const char *data)
{
    int ret;
    size_t len;
    unsigned char *der;
    gnutls_datum_t buffer = { NULL, 0 };
    gnutls_x509_crt_t x5;

    if (gnutls_x509_crt_init(&x5) != 0)
        return NULL;

    /* decode the base64 to get the raw DER representation */
    len = ne_unbase64(data, &der);
    if (len == 0) return NULL;

    buffer.data = der;
    buffer.size = len;

    ret = gnutls_x509_crt_import(x5, &buffer, GNUTLS_X509_FMT_DER);
    ne_free(der);

    if (ret < 0) {
        gnutls_x509_crt_deinit(x5);
        return NULL;
    }

    return populate_cert(ne_calloc(sizeof(struct ne_ssl_certificate_s)), x5);
}

char *ne_ssl_cert_export(const ne_ssl_certificate *cert)
{
    unsigned char *der;
    size_t len = 0;
    char *ret;

    /* find the length of the DER encoding. */
    if (gnutls_x509_crt_export(cert->subject, GNUTLS_X509_FMT_DER, NULL, &len) != 
        GNUTLS_E_SHORT_MEMORY_BUFFER) {
        return NULL;
    }
    
    der = ne_malloc(len);
    if (gnutls_x509_crt_export(cert->subject, GNUTLS_X509_FMT_DER, der, &len)) {
        ne_free(der);
        return NULL;
    }
    
    ret = ne_base64(der, len);
    ne_free(der);
    return ret;
}

static gnutls_digest_algorithm_t hash_to_alg(unsigned int flags)
{
    switch (flags & NE_HASH_ALGMASK) {
    case NE_HASH_MD5: return GNUTLS_DIG_MD5; break;
    case NE_HASH_SHA256: return GNUTLS_DIG_SHA256; break;
    case NE_HASH_SHA512: return GNUTLS_DIG_SHA512; break;
    default: break;
    }
    return GNUTLS_DIG_UNKNOWN;
}

char *ne_ssl_cert_hdigest(const ne_ssl_certificate *cert, unsigned int flags)
{
    gnutls_digest_algorithm_t alg = hash_to_alg(flags);
    unsigned char *dig;
    size_t len;
    char *rv;

    if (alg == GNUTLS_DIG_UNKNOWN) return NULL;

    if (gnutls_x509_crt_get_fingerprint(cert->subject, alg, NULL, &len) != GNUTLS_E_SHORT_MEMORY_BUFFER) {
        return NULL;
    }

    dig = ne_malloc(len);
    if (gnutls_x509_crt_get_fingerprint(cert->subject, alg, dig, &len) < 0) {
        ne_free(dig);
        return NULL;
    }

    rv = ne__strhash2hex(dig, len, flags);
    ne_free(dig);
    return rv;
}

int ne_ssl_cert_digest(const ne_ssl_certificate *cert, char *digest)
{
    char sha1[20], *p;
    int j;
    size_t len = sizeof sha1;

    if (gnutls_x509_crt_get_fingerprint(cert->subject, GNUTLS_DIG_SHA,
                                        sha1, &len) < 0)
        return -1;

    for (j = 0, p = digest; j < 20; j++) {
        *p++ = NE_HEX2ASC((sha1[j] >> 4) & 0x0f);
        *p++ = NE_HEX2ASC(sha1[j] & 0x0f);
        *p++ = ':';
    }

    *--p = '\0';
    return 0;
}

int ne__ssl_init(void)
{
#if LIBGNUTLS_VERSION_NUMBER < 0x020b01
#ifdef NE_HAVE_TS_SSL
    gcry_control(GCRYCTL_SET_THREAD_CBS, &gcry_threads_pthread);
#endif
    gcry_control(GCRYCTL_ENABLE_QUICK_RANDOM, 0);
#endif
    return gnutls_global_init();
}

void ne__ssl_exit(void)
{
    /* No way to unregister the thread callbacks.  Doomed. */
#if LIBGNUTLS_VERSION_MAJOR > 1 || LIBGNUTLS_VERSION_MINOR > 3 \
    || (LIBGNUTLS_VERSION_MINOR == 3 && LIBGNUTLS_VERSION_PATCH >= 3)
    /* It's safe to call gnutls_global_deinit() here only with
     * gnutls >= 1.3., since older versions don't refcount and
     * doing so would prevent any other use of gnutls within
     * the process. */
    gnutls_global_deinit();
#endif
}

char *ne_vstrhash(unsigned int flags, va_list ap)
{
    gnutls_digest_algorithm_t alg = hash_to_alg(flags);
    gnutls_hash_hd_t hd;
    unsigned char *out;
    const char *arg;
    unsigned len;
    char *rv;

    if (alg == GNUTLS_DIG_UNKNOWN)
        return NULL;

    if (gnutls_hash_init(&hd, alg) < 0)
        return NULL;

    while ((arg = va_arg(ap, const char *)) != NULL)
        gnutls_hash(hd, arg, strlen(arg));

    len = gnutls_hash_get_len(alg);
    out = ne_malloc(len);
    gnutls_hash_deinit(hd, out);

    rv = ne__strhash2hex(out, len, flags);
    ne_free(out);
    return rv;
}
