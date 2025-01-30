/* 
   Framework for testing with a server process
   Copyright (C) 2001-2004, 2009, Joe Orton <joe@manyfish.co.uk>

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

#ifndef CHILD_H
#define CHILD_H 1

#include "config.h"

#ifdef HAVE_STRING_H
#include <string.h> /* for strlen() */
#endif

#include "ne_socket.h"
#include "ne_session.h"

/* Test which does DNS lookup on "localhost": this must be the first
 * named test. */
int lookup_localhost(void);

int get_lh_family(void);
const char *get_lh_addr(void);
ne_inet_addr *get_lh_inet_addr(void);

/* Callback for spawn_server. */
typedef int (*server_fn)(ne_socket *sock, void *userdata);

/* Spawns server child process:
 * - forks child process.
 * - child process listens on localhost at given port.
 * - when you connect to it, 'fn' is run...
 * fn is passed the client/server socket as first argument,
 * and userdata as second.
 * - the socket is closed when 'fn' returns, so don't close in in 'fn'.
 */
int spawn_server(int port, server_fn fn, void *userdata);

/* Forks a server child process running 'fn(userdata)' on an
 * unspecified port.  Sets test suite error on failure; on success,
 * sets *port to bound port number. */
int new_spawn_server(int count, server_fn fn, void *userdata, 
                     unsigned int *port);

/* As per new_spawn_server, but also returns the bound address as *addr. */
int new_spawn_server2(int count, server_fn fn, void *userdata,
                      ne_inet_addr **addr, unsigned int *port);

/* Blocks until child process exits, and gives return code of 'fn'. */
int await_server(void);

/* Destroys session 'sess' and then is equivalent to await_server. */
int destroy_and_wait(ne_session *sess);

/* Kills child process. */
int reap_server(void);

/* Returns non-zero if server process has already died. */
int dead_server(void);

/* If discard_request comes across a header called 'want_header', it
 * will call got_header passing the header field value. */
extern const char *want_header;
typedef void (*got_header_fn)(char *value);
extern got_header_fn got_header;

/* Send string to child; ne_sock_fullwrite with debugging. */
ssize_t server_send(ne_socket *sock, const char *data, size_t len);

/* Utility macro: send given string down socket. */
#define SEND_STRING(sock, str) server_send((sock), (str), strlen((str)))

/* If test result 'err' is not OK, return an HTTP 500 error response
 * including the test context in a response header. */
#define ONERR(sock, err) do { int ret_ = (err); if (ret_) return error_response(sock, ret_); } while (0)

/* Send an HTTP error response including the test context in a
 * response header. */
int error_response(ne_socket *sock, int ret);

/* Tries to ensure that the socket will be closed using RST rather
 * than FIN. */
int reset_socket(ne_socket *sock);

/* Utility function: discard request.  Sets context on error. */
int discard_request(ne_socket *sock);

/* Utility function: discard request body. Sets context on error. */
int discard_body(ne_socket *sock);

struct serve_file_args {
    const char *fname;
    const char *headers;
    int chunks; 
};

/* Utility function: callback for spawn_server: pass pointer to
 * serve_file_args as userdata, and args->fname is served as a 200
 * request. If args->headers is non-NULL, it must be a set of
 * CRLF-terminated lines which is added in to the response headers.
 * If args->chunks is non-zero, the file is delivered using chunks of
 * that size. */
int serve_file(ne_socket *sock, void *ud);

/* set to value of C-L header by discard_request. */
extern int clength;

/* Sleep for a short time. */
void minisleep(void);

#endif /* CHILD_H */
