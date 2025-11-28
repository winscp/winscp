/* 
   Utility functions for HTTP client tests
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

#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for sleep() */
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <fcntl.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "ne_session.h"
#include "ne_string.h"

#include "child.h"
#include "tests.h"
#include "utils.h"

static char session_host[128];

int serve_response(ne_socket *s, const char *response)
{
    CALL(discard_request(s));
    CALL(discard_body(s));
    ONN("failed to send response", SEND_STRING(s, response));
    return OK;
}    

int single_serve_string(ne_socket *s, void *userdata)
{
    const char *str = userdata;
    return serve_response(s, str);
}

int double_serve_sstring(ne_socket *s, void *userdata)
{
    struct double_serve_args *args = userdata;
    struct string *str;

    CALL(discard_request(s));
    CALL(discard_body(s));
    
    str = &args->first;
    NE_DEBUG(NE_DBG_SOCKET, "Serving string: [[[%.*s]]]\n",
	     (int)str->len, str->data);
    ONN("write failed", ne_sock_fullwrite(s, str->data, str->len));

    CALL(discard_request(s));
    CALL(discard_body(s));

    str = &args->second;
    NE_DEBUG(NE_DBG_SOCKET, "Serving string: [[[%.*s]]]\n",
	     (int)str->len, str->data);
    ONN("write failed", ne_sock_fullwrite(s, str->data, str->len));

    return OK;
}

int serve_buffer(ne_socket *s, void *userdata)
{
    ne_buffer *buf = userdata;
    CALL(discard_request(s));
    CALL(discard_body(s));
    ONN("failed to send response", server_send(s, buf->data, buf->used-1));
    return OK;
}

int sleepy_server(ne_socket *sock, void *userdata)
{
    sleep(10);
    return 0;
}

int many_serve_string(ne_socket *s, void *userdata)
{
    int n;
    struct many_serve_args *args = userdata;
    
    for (n = 0; n < args->count; n++) {
	NE_DEBUG(NE_DBG_HTTP, "Serving response %d\n", n);
	CALL(serve_response(s, args->str));
    }

    return OK;
}

int any_request(ne_session *sess, const char *uri)
{
    ne_request *req = ne_request_create(sess, "GET", uri);
    int ret = ne_request_dispatch(req);
    ne_request_destroy(req);
    return ret;
}

int any_2xx_request_method(ne_session *sess, const char *method, const char *uri)
{
    ne_request *req = ne_request_create(sess, method, uri);
    int ret = ne_request_dispatch(req);
    int klass = ne_get_status(req)->klass;
    const char *context = ne_get_response_header(req, "X-Neon-Context");
    if (ret != NE_OK || klass != 2) {
        if (context)
            t_context("request failed, server error: %s", context);
        else
            t_context("request failed: %s", ne_get_error(sess));
        ret = FAIL;
    }
    else {
        ret = OK;
    }
    ne_request_destroy(req);
    return ret;
}

int any_2xx_request(ne_session *sess, const char *uri)
{
    return any_2xx_request_method(sess, "GET", uri);
}

int any_2xx_request_body(ne_session *sess, const char *uri)
{
    ne_request *req = ne_request_create(sess, "GET", uri);
#define BSIZE 5000
    char *body = memset(ne_malloc(BSIZE), 'A', BSIZE);
    int ret;
    ne_set_request_body_buffer(req, body, BSIZE);
    ret = ne_request_dispatch(req);
    ne_free(body);
    ONV(ret != NE_OK || ne_get_status(req)->klass != 2,
	("request failed: %s", ne_get_error(sess)));
    ne_request_destroy(req);
    return ret;
}

int serve_sstring(ne_socket *sock, void *ud)
{
    struct string *str = ud;

    NE_DEBUG(NE_DBG_SOCKET, "Serving string: [[[%.*s]]]\n",
	     (int)str->len, str->data);

    ONN("write failed", ne_sock_fullwrite(sock, str->data, str->len));
    
    return 0;
}

int serve_sstring_slowly(ne_socket *sock, void *ud)
{
    struct string *str = ud;
    size_t n;

    NE_DEBUG(NE_DBG_SOCKET, "Slowly serving string: [[[%.*s]]]\n",
	     (int)str->len, str->data);
    
    for (n = 0; n < str->len; n++) {
	ONN("write failed", ne_sock_fullwrite(sock, &str->data[n], 1));
	minisleep();
    }
    
    return 0;
}

int serve_infinite(ne_socket *sock, void *ud)
{
    struct infinite *i = ud;

    CALL(discard_request(sock));

    SEND_STRING(sock, i->header);

    while (server_send(sock, i->repeat, strlen(i->repeat)) == 0)
        /* nullop */;
    
    return OK;
}

int full_write(ne_socket *sock, const char *data, size_t len)
{
    int ret = ne_sock_fullwrite(sock, data, len);
    NE_DEBUG(NE_DBG_SOCKET, "wrote: [%.*s]\n", (int)len, data);
    ONV(ret, ("write failed (%d): %s", ret, ne_sock_error(sock)));
    return OK;
}

int multi_session_server(ne_session **sess,
                         const char *scheme, const char *hostname,
                         int count, server_fn fn, void *userdata)
{
    unsigned int port;
    
    CALL(new_spawn_server(count, fn, userdata, &port));
    
    *sess = ne_session_create(scheme, hostname, port);

    return OK;
}

const char *get_session_host(void)
{
    return session_host;
}

int session_server(ne_session **sess, server_fn fn, void *userdata)
{
    if (get_lh_family() == AF_INET6) {
        ne_snprintf(session_host, sizeof session_host, "[%s]", get_lh_addr());
    }
    else {
        ne_strnzcpy(session_host, get_lh_addr(), sizeof session_host);
    }

    return multi_session_server(sess, "http", session_host, 1, fn, userdata);
}

int proxied_multi_session_server(int count, ne_session **sess,
                                 const char *scheme, const char *host,
                                 unsigned int fakeport,
                                 server_fn fn, void *userdata)
{
    unsigned int port;
    
    CALL(new_spawn_server(count, fn, userdata, &port));
    
    *sess = ne_session_create(scheme, host, fakeport);

    NE_DEBUG(NE_DBG_HTTP, "test: Using proxied session to port %u.\n", port);

    ne_session_proxy(*sess, get_lh_addr(), port);

    return OK;
}


int proxied_session_server(ne_session **sess, const char *scheme,
                           const char *host, unsigned int fakeport,
                           server_fn fn, void *userdata)
{
    return proxied_multi_session_server(1, sess, scheme, host, fakeport,
                                        fn, userdata);
}

static void fakesess_destroy(void *userdata)
{
    ne_inet_addr *addr = userdata;

    ne_iaddr_free(addr);
}

int fakeproxied_session_server(ne_session **sess, const char *scheme,
                               const char *host, unsigned int fakeport,
                               server_fn fn, void *userdata)
{
    return fakeproxied_multi_session_server(1, sess, scheme, host, fakeport,
                                            fn, userdata);
}

int fakeproxied_multi_session_server(int count,
                                     ne_session **sess, const char *scheme,
                                     const char *host, unsigned int fakeport,
                                     server_fn fn, void *userdata)
{
    unsigned int port;
    ne_inet_addr *addr;
    const ne_inet_addr *alist[1];
    
    CALL(new_spawn_server2(count, fn, userdata, &addr, &port));

    NE_DEBUG(NE_DBG_HTTP, "test: Using fake proxied '%s' session for %s using port %u.\n",
             scheme, host, port);
    
    alist[0] = addr;

    *sess = ne_session_create(scheme, host, fakeport);

    ne_set_addrlist2(*sess, port, alist, 1);

    ne_hook_destroy_session(*sess, fakesess_destroy, addr);

    return OK;
}

int make_session(ne_session **sess, server_fn fn, void *ud)
{
    return session_server(sess, fn, ud);
}

int file_to_buffer(const char *filename, ne_buffer *buf)
{
    char buffer[BUFSIZ];
    int fd;
    ssize_t n;
    
    fd = open(filename, O_RDONLY);
    ONV(fd < 0, ("could not open file %s", filename));

    while ((n = read(fd, buffer, BUFSIZ)) > 0) {
	ne_buffer_append(buf, buffer, n);
    }

    close(fd);
    
    return 0;
}

void sess_notifier(void *userdata, ne_session_status status,
                   const ne_session_status_info *info)
{
    ne_buffer *buf = userdata;
    char scratch[512];

    switch (status) {
    case ne_status_lookup:
        ne_buffer_concat(buf, "lookup(", info->lu.hostname, ")-", NULL);
        break;
    case ne_status_connecting:
        ne_iaddr_print(info->ci.address, scratch, sizeof scratch);
        ne_buffer_concat(buf, "connecting(", info->lu.hostname,
                         ",", scratch, ")-", NULL);
        break;
    case ne_status_disconnected:
        ne_buffer_czappend(buf, "dis");
        /* fallthrough */
    case ne_status_connected:
        ne_buffer_concat(buf, "connected(", info->cd.hostname,
                         ")-", NULL);
        break;
    case ne_status_sending:
    case ne_status_recving:
        ne_snprintf(scratch, sizeof scratch,
                    "%" NE_FMT_NE_OFF_T ",%" NE_FMT_NE_OFF_T,
                    info->sr.progress, info->sr.total);
        ne_buffer_concat(buf,
                         status == ne_status_sending ? "send" : "recv",
                         "(", scratch, ")-", NULL);
        break;
    case ne_status_handshake:
        ne_buffer_snprintf(buf, 256,
                           "handshake(%s, %s)-",
                           ne_ssl_proto_name(info->hs.protocol),
                           info->hs.ciphersuite ?
                           info->hs.ciphersuite : "[none]");
        break;
    default:
        ne_buffer_czappend(buf, "bork!");
        break;
    }

    NE_DEBUG(NE_DBG_HTTP, "notifier %d => %s\n",
             status, buf->data);
}
