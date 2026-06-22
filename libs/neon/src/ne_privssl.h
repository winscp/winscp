/* 
   SSL interface definitions internal to neon.
   Copyright (C) 2003-2021, Joe Orton <joe@manyfish.co.uk>
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

/* THIS IS NOT A PUBLIC INTERFACE. You CANNOT include this header file
 * from an application.  */
 
#ifndef NE_PRIVSSL_H
#define NE_PRIVSSL_H

/* This is the private interface between ne_socket, ne_gnutls and
 * ne_openssl. */

#include "ne_ssl.h"
#include "ne_socket.h"

#ifdef HAVE_OPENSSL

#include <openssl/ssl.h>

struct ne_ssl_context_s {
    SSL_CTX *ctx;
    SSL_SESSION *sess;
    const char *hostname; /* for SNI */
    ne_ssl_client_cert *client_cert;
    ne_ssl_ccprovide_fn provider;
    void *provider_ud;
};

typedef struct ne_ssl_socket_s {
    SSL *ssl;
    int failures;
    /* set to non-zero if a client cert was requested during initial
     * handshake, but none could be provided. */
    int cc_requested;
} ne_ssl_socket;

NE_PRIVATE ne_ssl_socket *ne__sock_sslsock(ne_socket *sock);

/* Create a clicert object from cert DER {der, der_len}, using given
 * RSA_METHOD for the RSA object. */
NE_PRIVATE ne_ssl_client_cert *
ne__ssl_clicert_exkey_import(const unsigned char *der,
                             size_t der_len,
                             const RSA_METHOD *method);

NE_PRIVATE ne_ssl_certificate *ne__ssl_make_chain(STACK_OF(X509) *chain);

#endif /* HAVE_OPENSSL */

#ifdef HAVE_GNUTLS

#include <gnutls/gnutls.h>

#ifdef HAVE_GNUTLS_PRIVKEY_IMPORT_EXT
#include <gnutls/abstract.h>
#endif

struct ne_ssl_context_s {
    gnutls_certificate_credentials_t cred;
    int verify; /* non-zero if client cert verification required */
    ne_ssl_client_cert *client_cert;
    const char *hostname; /* for SNI */
    char *priority;

    ne_ssl_ccprovide_fn provider;
    void *provider_ud;

    /* Session cache. */
    union ne_ssl_scache {
        struct {
            gnutls_datum_t key, data;
        } server;
#if defined(HAVE_GNUTLS_SESSION_GET_DATA2)
        gnutls_datum_t client;
#else
        struct {
            char *data;
            size_t len;
        } client;
#endif
    } cache;
};

typedef struct ne_ssl_socket_s {
    gnutls_session_t sess;
    ne_ssl_context *context;
    /* set to non-zero if a client cert was requested during initial
     * handshake, but none could be provided. */
    int cc_requested;
} ne_ssl_socket;

#ifdef HAVE_GNUTLS_PRIVKEY_IMPORT_EXT
NE_PRIVATE ne_ssl_client_cert *
ne__ssl_clicert_exkey_import(const unsigned char *der, size_t der_len,
                             gnutls_privkey_sign_func sign_func, void *userdata);
#endif

NE_PRIVATE ne_ssl_certificate *ne__ssl_make_chain(gnutls_session_t sock,
                                                  gnutls_certificate_credentials_t crd);

#endif /* HAVE_GNUTLS */

#ifdef NE_HAVE_SSL
/* Set the socket error appropriate for SSL verification failures. */
NE_PRIVATE void ne__sock_set_verify_err(ne_socket *sock, int failures);

/* Process-global initialization of the SSL library; returns non-zero
 * on error. */
NE_PRIVATE int ne__ssl_init(void);

/* Process-global de-initialization of the SSL library. */
NE_PRIVATE void ne__ssl_exit(void);

NE_PRIVATE ne_ssl_socket *ne__sock_sslsock(ne_socket *sock);

/* Return non-zero if hostname from certificate (cn) matches hostname
 * used for session (hostname); follows RFC2818 logic. */
NE_PRIVATE int ne__ssl_match_hostname(const char *cn, size_t cnlen, 
                                      const char *hostname);
#endif

#endif /* NE_PRIVSSL_H */
