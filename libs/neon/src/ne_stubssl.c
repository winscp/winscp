/* 
   Stubs for SSL support when no SSL library has been configured
   Copyright (C) 2002-2021, Joe Orton <joe@manyfish.co.uk>

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

#include <stdlib.h> /* for NULL */

#include "ne_ssl.h"
#include "ne_session.h"
#include "ne_socket.h"

char *ne_ssl_readable_dname(const ne_ssl_dname *dn)
{
    return NULL;
}

ne_ssl_certificate *ne_ssl_cert_read(const char *filename)
{
    return NULL;
}

int ne_ssl_cert_cmp(const ne_ssl_certificate *c1, const ne_ssl_certificate *c2)
{
    return 1;
}

const ne_ssl_certificate *ne_ssl_cert_signedby(const ne_ssl_certificate *cert)
{ 
    return NULL;
}

const ne_ssl_dname *ne_ssl_cert_issuer(const ne_ssl_certificate *cert)
{
    return NULL;
}

const ne_ssl_dname *ne_ssl_cert_subject(const ne_ssl_certificate *cert)
{
    return NULL;
}

void ne_ssl_cert_free(ne_ssl_certificate *cert) {}

ne_ssl_client_cert *ne_ssl_clicert_read(const char *filename)
{
    return NULL;
}

const ne_ssl_certificate *ne_ssl_clicert_owner(const ne_ssl_client_cert *ccert)
{
    return NULL;
}

ne_ssl_client_cert *ne_ssl_clicert_import(const unsigned char *buffer, 
                                          size_t buflen)
{
    return NULL;
}

int ne_ssl_clicert_encrypted(const ne_ssl_client_cert *ccert)
{
    return -1;
}

int ne_ssl_clicert_decrypt(ne_ssl_client_cert *ccert, const char *password)
{
    return -1;
}

void ne_ssl_clicert_free(ne_ssl_client_cert *ccert) {}

ne_ssl_context *ne_ssl_context_create(int mode)
{
    return NULL;
}

void ne_ssl_context_trustcert(ne_ssl_context *ctx, const ne_ssl_certificate *cert) {}
int ne_ssl_context_keypair(ne_ssl_context *ctx,
                           const char *cert, const char *key)
{
    return -1;
}

int ne_ssl_context_set_versions(ne_ssl_context *ctx, enum ne_ssl_protocol min,
                                enum ne_ssl_protocol max)
{
    return NE_SOCK_ERROR;
}
int ne_ssl_check_certificate(ne_ssl_context *ctx, ne_socket *sock,
                             const char *hostname,
                             const ne_inet_addr *address,
                             const ne_ssl_certificate *cert,
                             unsigned int flags, int *failures)
{
    return -1;
}

int ne_ssl_context_set_verify(ne_ssl_context *ctx, 
                              int required,
                              const char *ca_names,
                              const char *verify_cas)
{
    return -1;
}

void ne_ssl_context_set_clicert(ne_ssl_context *ctx, const ne_ssl_client_cert *cc)
{}

void ne_ssl_context_set_flag(ne_ssl_context *ctx, int flag, int value) {}
int ne_ssl_context_get_flag(ne_ssl_context *ctx, int flag) { return -1; }
void ne_ssl_context_trustdefca(ne_ssl_context *ctx) {}
void ne_ssl_context_destroy(ne_ssl_context *ctx) {}
void ne_ssl_context_set_ccprovide(ne_ssl_context *ctx,
                                  ne_ssl_ccprovide_fn provider,
                                  void *userdata) {}

int ne_ssl_cert_digest(const ne_ssl_certificate *cert, char *digest)
{
    return -1;
}

char *ne_ssl_cert_hdigest(const ne_ssl_certificate *cert, unsigned int flags)
{
    return NULL;
}

void ne_ssl_cert_validity_time(const ne_ssl_certificate *cert,
                               time_t *from, time_t *until) {}

const char *ne_ssl_cert_identity(const ne_ssl_certificate *cert)
{
    return NULL;
}


const char *ne_ssl_clicert_name(const ne_ssl_client_cert *ccert)
{
    return NULL;
}

int ne_ssl_dname_cmp(const ne_ssl_dname *dn1, const ne_ssl_dname *dn2)
{
    return -1;
}

int ne_ssl_cert_write(const ne_ssl_certificate *cert, const char *filename)
{
    return -1;
}

char *ne_ssl_cert_export(const ne_ssl_certificate *cert)
{
    return NULL;
}

ne_ssl_certificate *ne_ssl_cert_import(const char *data)
{
    return NULL;
}

ne_ssl_client_cert *ne_ssl_clicert_copy(const ne_ssl_client_cert *cc)

{
    return NULL;
}

ne_ssl_client_cert *ne_ssl_clicert_fromuri(const char *uri,
                                           unsigned int flags)
{
    return NULL;
}

int ne_sock_accept_ssl(ne_socket *sock, ne_ssl_context *ctx) {
    return NE_SOCK_ERROR;
}

int ne_sock_connect_ssl(ne_socket *sock, ne_ssl_context *ctx, void *userdata)
{
    return NE_SOCK_ERROR;
}

int ne_sock_handshake(ne_socket *sock, ne_ssl_context *ctx,
                      const char *hostname, unsigned int flags)

{
    return NE_SOCK_ERROR;
}
