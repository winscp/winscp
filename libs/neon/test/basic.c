/* 
   Tests for high-level HTTP interface (ne_basic.h)
   Copyright (C) 2002-2008, 2012, Joe Orton <joe@manyfish.co.uk>

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

#include <fcntl.h>

#include "ne_basic.h"

#include "tests.h"
#include "child.h"
#include "utils.h"

static int content_type(void)
{
    int n;
    static const struct {
	const char *value, *type, *subtype, *charset;
    } ctypes[] = {
	{ "foo/bar", "foo", "bar", NULL },
	{ "foo/bar  ", "foo", "bar", NULL },
	{ "application/xml", "application", "xml", NULL },
        /* these rules previously applied RFC 3023, now updated for 7303. */
        { "text/lemon", "text", "lemon", NULL },
        { "text/xml", "text", "xml", NULL },
#undef TXU
#define TXU "text", "xml", "utf-8"
	/* 2616 doesn't *say* that charset can be quoted, but bets are
	 * that some servers do it anyway. */
	{ "text/xml; charset=utf-8", TXU },
	{ "text/xml; charset=utf-8; foo=bar", TXU },
	{ "text/xml;charset=utf-8", TXU },
	{ "text/xml ;charset=utf-8", TXU },
	{ "text/xml;charset=utf-8;foo=bar", TXU },
	{ "text/xml; foo=bar; charset=utf-8", TXU },
	{ "text/xml; foo=bar; charset=utf-8; bar=foo", TXU },
	{ "text/xml; charset=\"utf-8\"", TXU },
	{ "text/xml; charset='utf-8'", TXU },
	{ "text/xml; foo=bar; charset=\"utf-8\"; bar=foo", TXU },
#undef TXU
	/* badly quoted charset should come out as NULL */
	{ "foo/lemon; charset=\"utf-8", "foo", "lemon", NULL },
	{ NULL }
    };

    for (n = 0; ctypes[n].value != NULL; n++) {
        ne_content_type ct;
        ne_session *sess;
        ne_request *req;
        char resp[200];
        int rv;

        ct.type = ct.subtype = ct.charset = ct.value = "unset";

        ne_snprintf(resp, sizeof resp,
                    "HTTP/1.0 200 OK\r\n" "Content-Length: 0\r\n"
                    "Content-Type: %s\r\n" "\r\n", ctypes[n].value);
        
        CALL(make_session(&sess, single_serve_string, resp));

        req = ne_request_create(sess, "GET", "/anyfoo");
        ONREQ(ne_request_dispatch(req));
        rv = ne_get_content_type(req, &ct);
        
        ONV(rv == 0 && !ctypes[n].type,
            ("expected c-t parse failure for %s", ctypes[n].value));

        ONV(rv != 0 && ctypes[n].type,
            ("c-t parse failure %d for %s", rv, ctypes[n].value));

        ne_request_destroy(req);
        ne_session_destroy(sess);
        CALL(await_server());

        if (rv) continue;	

	ONV(strcmp(ct.type, ctypes[n].type),
	    ("for `%s': type was `%s'", ctypes[n].value, ct.type));

	ONV(strcmp(ct.subtype, ctypes[n].subtype),
	    ("for `%s': subtype was `%s'", ctypes[n].value, ct.subtype));

	ONV(ctypes[n].charset && ct.charset == NULL,
	    ("for `%s': charset unset", ctypes[n].value));
	
	ONV(ctypes[n].charset == NULL && ct.charset != NULL,
	    ("for `%s': unexpected charset `%s'", ctypes[n].value, 
	     ct.charset));

	ONV(ctypes[n].charset && ct.charset &&
	    strcmp(ctypes[n].charset, ct.charset),
	    ("for `%s': charset was `%s'", ctypes[n].value, ct.charset));

	ne_free(ct.value);
    }

    return OK;
}

/* Do ranged GET for range 'start' to 'end'; with 'resp' as response.
 * If 'fail' is non-NULL, expect ne_get_range to fail, and fail the
 * test with given message if it doesn't. */
static int do_range(off_t start, off_t end, const char *fail,
		    char *resp)
{
    ne_session *sess;
    ne_content_range range = {0};
    int fd, ret;

    CALL(make_session(&sess, single_serve_string, resp));
    
    range.start = start;
    range.end = end;
    
    fd = open("/dev/null", O_WRONLY);
    
    ret = ne_get_range(sess, "/foo", &range, fd);

    close(fd);
    ne_close_connection(sess);
    CALL(await_server());
    
    if (fail) {
#if 0
	t_warning("error was %s", ne_get_error(sess));
#endif
	ONV(ret == NE_OK, ("%s", fail));
    } else {
	ONREQ(ret);
    }
    
    ne_session_destroy(sess);
    return OK;
}

static int get_range(void)
{
    return do_range(1, 10, NULL,
		    "HTTP/1.1 206 Widgets\r\n" "Connection: close\r\n"
		    "Content-Range: bytes 1-10/10\r\n"
		    "Content-Length: 10\r\n\r\nabcdefghij");
}

static int get_eof_range(void)
{
    return do_range(1, -1, NULL,
		    "HTTP/1.1 206 Widgets\r\n" "Connection: close\r\n"
		    "Content-Range: bytes 1-10/10\r\n"
		    "Content-Length: 10\r\n\r\nabcdefghij");
}

static int fail_range_length(void)
{
    return do_range(1, 10, "range response length mismatch should fail",
		    "HTTP/1.1 206 Widgets\r\n" "Connection: close\r\n"
		    "Content-Range: bytes 1-2/2\r\n"
		    "Content-Length: 2\r\n\r\nab");
}

static int fail_range_units(void)
{
    return do_range(1, 2, "range response units check should fail",
		    "HTTP/1.1 206 Widgets\r\n" "Connection: close\r\n"
		    "Content-Range: fish 1-2/2\r\n"
		    "Content-Length: 2\r\n\r\nab");
}

static int fail_range_notrange(void)
{
    return do_range(1, 2, "non-ranged response should fail",
		    "HTTP/1.1 200 Widgets\r\n" "Connection: close\r\n"
		    "Content-Range: bytes 1-2/2\r\n"
		    "Content-Length: 2\r\n\r\nab");
}

static int fail_range_unsatify(void)
{
    return do_range(1, 2, "unsatisfiable range should fail",
		    "HTTP/1.1 416 No Go\r\n" "Connection: close\r\n"
		    "Content-Length: 2\r\n\r\nab");
}

static int dav_capabilities(void)
{
    static const struct {
	const char *hdrs;
	unsigned int class1, class2, exec;
    } caps[] = {
	{ "DAV: 1,2\r\n", 1, 1, 0 },
	{ "DAV: 1 2\r\n", 0, 0, 0 },
	/* these aren't strictly legal DAV: headers: */
	{ "DAV: 2,1\r\n", 1, 1, 0 },
	{ "DAV:  1, 2  \r\n", 1, 1, 0 },
	{ "DAV: 1\r\nDAV:2\r\n", 1, 1, 0 },
	{ NULL, 0, 0, 0 }
    };
    char resp[BUFSIZ];
    int n;

    for (n = 0; caps[n].hdrs != NULL; n++) {
	ne_server_capabilities c = {0};
	ne_session *sess;

	ne_snprintf(resp, BUFSIZ, "HTTP/1.0 200 OK\r\n"
		    "Connection: close\r\n"
		    "%s" "\r\n", caps[n].hdrs);

	CALL(make_session(&sess, single_serve_string, resp));

	ONREQ(ne_options(sess, "/foo", &c));

	ONV(c.dav_class1 != caps[n].class1,
	    ("class1 was %d not %d", c.dav_class1, caps[n].class1));
	ONV(c.dav_class2 != caps[n].class2,
	    ("class2 was %d not %d", c.dav_class2, caps[n].class2));
	ONV(c.dav_executable != caps[n].exec,
	    ("class2 was %d not %d", c.dav_executable, caps[n].exec));

	CALL(destroy_and_wait(sess));
    }

    return OK;	
}

static int get(void)
{
    ne_session *sess;
    int fd;
    
    CALL(make_session(&sess, single_serve_string, 
                      "HTTP/1.0 200 OK\r\n"
                      "Content-Length: 5\r\n"
                      "\r\n"
                      "abcde"));
    
    fd = open("/dev/null", O_WRONLY);
    ONREQ(ne_get(sess, "/getit", fd));
    close(fd);

    return destroy_and_wait(sess);
}


#define CHUNK(len, data) #len "\r\n" data "\r\n"

#define ABCDE_CHUNKS CHUNK(1, "a") CHUNK(1, "b") \
 CHUNK(1, "c") CHUNK(1, "d") \
 CHUNK(1, "e") CHUNK(0, "")

static int getbuf(void)
{
    ne_session *sess;
    char buffer[16];
    size_t buflen = sizeof buffer;

    CALL(make_session(&sess, single_serve_string,
                      "HTTP/1.1 200 OK\r\n" "Server: neon-test-server\r\n"
                      "Transfer-Encoding: chunked\r\n"
                      "\r\n"
                      ABCDE_CHUNKS));

    ONREQ(ne_getbuf(sess, "/", buffer, &buflen));

    ONV(buflen != 5, ("unexpected buffer length %" NE_FMT_SIZE_T, buflen));

    ONV(memcmp("abcde", buffer, 5) != 0,
        ("mismatched chunked response: [%5s]", buffer));

    return destroy_and_wait(sess);
}

static int getbuf_fail(void)
{
    ne_session *sess;
    char buffer[16];
    size_t buflen = sizeof buffer;
    int ret;

    CALL(make_session(&sess, single_serve_string,
                      "HTTP/1.1 302 OK\r\n" "Server: neon-test-server\r\n"
                      "Transfer-Encoding: chunked\r\n"
                      "\r\n"
                      ABCDE_CHUNKS));

    ret = ne_getbuf(sess, "/", buffer, &buflen);
    ONN("success for non-2xx response", ret != NE_ERROR);

    ONV(buflen != sizeof buffer,
        ("buffer length adjusted to %" NE_FMT_SIZE_T, buflen));

    return destroy_and_wait(sess);
}

static int getbuf_fill(void)
{
    ne_session *sess;
    char buffer[5];
    size_t buflen = sizeof buffer;

    CALL(make_session(&sess, single_serve_string,
                      "HTTP/1.1 200 OK\r\n" "Server: neon-test-server\r\n"
                      "Transfer-Encoding: chunked\r\n"
                      "\r\n"
                      ABCDE_CHUNKS));

    ONREQ(ne_getbuf(sess, "/", buffer, &buflen));

    ONV(buflen != 5, ("unexpected buffer length %" NE_FMT_SIZE_T, buflen));

    ONV(memcmp("abcde", buffer, 5) != 0,
        ("mismatched chunked response: [%5s]", buffer));

    return destroy_and_wait(sess);
}

static int getbuf2(void)
{
    ne_session *sess;
    char buf;
    size_t buflen = sizeof buf;
    int ret;

    CALL(make_session(&sess, single_serve_string,
                      "HTTP/1.1 200 OK\r\n" "Server: neon-test-server\r\n"
                      "Content-Length: 5\r\n"
                      "\r\n"
                      "abcde"
                      "HTTP/1.1 200 OK\r\n" "Server: neon-test-server\r\n"
                      "Content-Length: 5\r\n"
                      "\r\n"
                      "abcde"));

    ret = ne_getbuf(sess, "/", &buf, &buflen);
    ONV(ret != NE_FAILED, ("overflow case gave %d not FAILED", ret));
    ONV(buflen != 1, ("buffer length returned as %" NE_FMT_SIZE_T, buflen));

    ret = any_request(sess, "/closeme");
    ONN("failed to close connection", ret != NE_CONNECT);

    return destroy_and_wait(sess);
}

static int post_getbuf_retry(ne_request *req, void *userdata, 
                           const ne_status *status)
{
    return status->code == 401 ? NE_RETRY : NE_OK;    
}

static int getbuf_retry(void)
{
    ne_session *sess;
    char buf[6] = "00000";
    size_t buflen = sizeof buf;
    int ret;

    CALL(make_session(&sess, single_serve_string,
                      "HTTP/1.1 401 Retry Please\r\n" "Server: neon-test-server\r\n"
                      "Content-Length: 8\r\n"
                      "\r\n"
                      "zzzzzzzz"
                      "HTTP/1.1 200 OK\r\n" "Server: neon-test-server\r\n"
                      "Content-Length: 1\r\n"
                      "\r\n"
                      "a"));

    ne_hook_post_send(sess, post_getbuf_retry, NULL);
    
    ONREQ(ne_getbuf(sess, "/", buf, &buflen));
    ONV(ret != NE_OK, ("overflow case gave %d not FAILED", ret));
    ONV(buflen != 1, ("buffer length returned as %" NE_FMT_SIZE_T, buflen));

    ret = any_request(sess, "/closeme");
    ONN("failed to close connection", ret != NE_CONNECT);

    return destroy_and_wait(sess);
}

#define CLASS_12 (NE_CAP_DAV_CLASS1 | NE_CAP_DAV_CLASS2)

static int options2(void)
{
    static const struct {
        const char *hdrs;
        unsigned int caps;
    } ts[] = {
        { "1,2\r\n", CLASS_12 },
        { "1 2\r\n", 0 },
        /* these aren't strictly legal headers: */
        { "2,1\r\n", CLASS_12 },
        { " 1, 2  \r\n", CLASS_12 },
        { "1\r\nDAV:2\r\n", CLASS_12 },
        /* extended types */
        { "1, 2, extended-mkcol", CLASS_12 | NE_CAP_EXT_MKCOL },
        { NULL, 0 }
    };
    char resp[BUFSIZ];
    int n;

    for (n = 0; ts[n].hdrs != NULL; n++) {
        ne_session *sess;
        unsigned int caps;

        ne_snprintf(resp, BUFSIZ, "HTTP/1.0 200 OK\r\n"
                    "Connection: close\r\n"
                    "Content-Length: 0\r\n"
                    "DAV: %s" "\r\n\r\n", ts[n].hdrs);

        CALL(make_session(&sess, single_serve_string, resp));

        ONREQ(ne_options2(sess, "/foo", &caps));

        ONV(caps != ts[n].caps,
            ("capabilities for 'DAV: %s' were 0x%x, expected 0x%x", 
             ts[n].hdrs, caps, ts[n].caps));

        CALL(destroy_and_wait(sess));
    }

    return OK;  
}

static int put(void)
{
    ne_session *sess;
    
    CALL(make_session(&sess, single_serve_string, "HTTP/1.1 204 OK\r\n"
                      "Content-Length: 200\r\n"
                      "\r\n"));
    
    ONREQ(ne_putbuf(sess, "/foo", "foobar", 6));

    return destroy_and_wait(sess);
}

ne_test tests[] = {
    T(lookup_localhost),
    T(content_type),
    T(get_range),
    T(get_eof_range),
    T(fail_range_length),
    T(fail_range_units),
    T(fail_range_notrange),
    T(fail_range_unsatify),
    T(dav_capabilities),
    T(get),
    T(getbuf),
    T(getbuf_fail),
    T(getbuf_fill),
    T(getbuf2),
    T(getbuf_retry),
    T(options2),
    T(put),
    T(NULL) 
};

