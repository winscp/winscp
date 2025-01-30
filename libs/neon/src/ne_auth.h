/* 
   HTTP authentication routines
   Copyright (C) 1999-2021, Joe Orton <joe@manyfish.co.uk>

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

#ifndef NE_AUTH_H
#define NE_AUTH_H

#include "ne_session.h" /* for ne_session */

NE_BEGIN_DECLS

/* Size of username/password buffers passed to ne_auth_creds
 * callback. */
#define NE_ABUFSIZ (256)

/* The callback used to request the username and password in the given
 * realm. The username and password must be copied into the buffers
 * which are both of size NE_ABUFSIZ.  The 'attempt' parameter is zero
 * on the first call to the callback, and increases by one for each
 * invocation of the callback during an attempt to authenticate.  The
 * 'realm' parameter is provided by the server, the string is cleaned
 * of non-printable characters.
 *
 * The callback must return zero to indicate that authentication
 * should be attempted with the username/password, or non-zero to
 * cancel the request. (if non-zero, username and password are
 * ignored.)
 *
 * IMPORTANT NOTE: The callback will be invoked repeatedly until
 * either it returns non-zero, or authentication is successful.
 *
 * Hint: if you just wish to attempt authentication just once (even if
 * the user gets the username/password wrong), have the callback
 * function use 'attempt' value as the function return value. */
typedef int (*ne_auth_creds)(void *userdata, const char *realm, int attempt,
			     char *username, char *password);

/* Set callbacks to provide credentials for server and proxy
 * authentication, using the default set of authentication protocols.
 * userdata is passed as the first argument to the callback. */
void ne_set_server_auth(ne_session *sess, ne_auth_creds creds, void *userdata);
void ne_set_proxy_auth(ne_session *sess, ne_auth_creds creds, void *userdata);

/* As an alternative to using ne_set_server_auth and
 * ne_set_proxy_auth, the following interfaces may be used; these
 * allow control over which authentication protocol is used. */

/* NE_AUTH_BASIC: Basic authentication transmits the username and
 * password unprotected over the channel; this allows a passive attack
 * to steal the credentials if using an unsecured channel
 * (i.e. non-SSL). */
#define NE_AUTH_BASIC (0x0001)

/* NE_AUTH_DIGEST: Digest authentication uses a hash of the username,
 * password, and certain aspects of the request, so prevents passive
 * attackers from obtaining the credentials; active attackers can
 * still modify most of the request/response if using an unsecured
 * channel.  Supports algorithms from RFC 2617 and RFC 7616. */
#define NE_AUTH_DIGEST (0x0080)

/* NE_AUTH_LEGACY_DIGEST: Using this flag together with NE_AUTH_DIGEST
 * enables support for the weaker, legacy version of the Digest
 * algorithm specified in RFC 2069 (obsoleted by RFC 2617, which was
 * published in June 1999).  */
#define NE_AUTH_LEGACY_DIGEST (0x0002)

/* NE_AUTH_NEGOTIATE: Negotiate uses GSSAPI/SSPI, or NTLM, to
 * authenticate the user; an active attacker can modify any of the
 * request/response at will, so this must not be used over an
 * unsecured channel.  NE_AUTH_NEGOTIATE is currently equivalent to
 * use of (NE_AUTH_GSSAPI | NE_AUTH_NTLM). */
#define NE_AUTH_NEGOTIATE (0x0004)

/* NE_AUTH_GSSAPI: Use GSSAPI or SSPI to authenticate the user; an
 * active attacker can modify any of the request/response at will, so
 * this must not be used over an unsecured channel. NE_AUTH_GSSAPI
 * is currently equivalent to (NE_AUTH_GSSAPI_ONLY | NE_AUTH_SSPI). */
#define NE_AUTH_GSSAPI (0x0008)

/* NE_AUTH_NTLM: Use NTLM to authenticate the user; an active attacker
 * can modify any of the request/response at will, so this must not be
 * used over an unsecured channel. */
#define NE_AUTH_NTLM (0x0010)

/* NE_AUTH_SSPI: Use SSPI to authenticate the user; an
 * active attacker can modify any of the request/response at will, so
 * this must not be used over an unsecured channel. */
#define NE_AUTH_SSPI (0x0020)

/* NE_AUTH_GSSAPI_ONLY: Use GSSAPI to authenticate the user; an
 * active attacker can modify any of the request/response at will, so
 * this must not be used over an unsecured channel. */
#define NE_AUTH_GSSAPI_ONLY (0x0040)

/* 0x0080: legacy definition of NE_AUTH_DIGEST in 0.31 and earlier */

#ifdef WINSCP
#define NE_AUTH_PASSPORT (0x0080)
#endif

/* The default set of supported protocols, as deemed appropriate for
 * the given session scheme.  The interpretation of this flag may
 * change across versions, for example with older, less secure
 * protocols being removed from the default set. */
#define NE_AUTH_DEFAULT (0x1000)

/* All protocols supported by the library. The interpretation of this
 * flag may change across versions. */
#define NE_AUTH_ALL (0x2000)

/* If present in the protocol mask passed to ne_auth_provide,
 * indicates that proxy authentication is requested. */
#define NE_AUTH_PROXY (0x4000)

/* Add a callback to provide credentials for server and proxy
 * authentication using a particular auth protocol or set of
 * protocols.  The protocol is supplied as a bitmask of NE_AUTH_*
 * values.  For NE_AUTH_NEGOTIATE, the creds and userdata arguments
 * are ignored and may be NULL.
 *
 * These functions may be called multiple times per session to
 * register callbacks for different protocols.  If the server presents
 * more than one protocol in an auth challenge, the following
 * algorithm will be used to determine which callback is used:
 *
 * - iterate over the registered callbacks in the order registered
 * - for each each callback, iterate over the known set of protocols
 *   in order of algorithm strength (strongest first).
 * - if the protocol mask for that callback matches the protocol,
 *   attempt authentication using this protocol.
 *
 * Therefore, if multiple calls to ne_add_server_auth or
 * ne_add_proxy_auth are used for a given session, the caller must
 * ensure that the order in which those calls are made reflects the
 * precedence of protocols to be used. */
void ne_add_server_auth(ne_session *sess, unsigned protocol, 
                        ne_auth_creds creds, void *userdata);
void ne_add_proxy_auth(ne_session *sess, unsigned protocol, 
                       ne_auth_creds creds, void *userdata);

#ifdef WINSCP
void ne_remove_server_auth(ne_session *sess);

typedef void (*ne_aux_request_init)(ne_session * sess, ne_request * req, void * userdata);

void ne_set_aux_request_init(ne_session * sess, ne_aux_request_init aux_request_init, void * userdata);

int is_passport_challenge(ne_request *req, const ne_status *status);
#endif

/* Alternative credentials provider callback, invoked when credentials
 * are required to authenticate the client to either a server or
 * proxy.  'protocol' is the authentication protocol number
 * (NE_AUTH_*) of the challenge, bitwise-ORed with NE_AUTH_PROXY when
 * the auth challenge is made by an HTTP proxy.
 *
 * 'realm' is the realm name provided by the server, and is cleaned of
 * non-printable characters.  The 'attempt' counter reflects the
 * number of attempts to provide credentials to the server
 * (i.e. retried requests sent with a challenge response), NOT the
 * number of times the callback is invoked, unlike the ne_auth_creds
 * callback.
 *
 * The callback must return zero to indicate that authentication
 * should be attempted with the username/password, or non-zero to
 * cancel the request. (if non-zero, username and password are
 * ignored.)
 *
 * The username and password buffers have length 'buflen', which is
 * guaranteed to be >= NE_ABUFSIZ.  The username must be provided as a
 * NUL-terminated UTF-8 encoding only.  The password must be provided
 * as a NUL-terminated string.  Additional protocol-specific
 * restrictions apply, e.g. username cannot contain a colon for Basic
 * auth.
 *
 * IMPORTANT NOTE: The callback will be invoked repeatedly until
 * either it returns non-zero, or authentication is successful.
 *
 * Hint: if you just wish to attempt authentication just once (even if
 * the user gets the username/password wrong), have the callback
 * function use 'attempt' value as the function return value. */
typedef int (*ne_auth_provide)(void *userdata, int attempt,
                               unsigned protocol, const char *realm,
                               char *username, char *password, size_t buflen);

void ne_add_auth(ne_session *sess, unsigned protocol,
                 ne_auth_provide creds, void *userdata);

/* Clear any cached authentication credentials for the given
 * session. */
void ne_forget_auth(ne_session *sess);

NE_END_DECLS

#endif /* NE_AUTH_H */
