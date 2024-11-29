/* 
   Tests for 3xx redirect interface (ne_redirect.h)
   Copyright (C) 2002-2024, Joe Orton <joe@manyfish.co.uk>

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

#include "ne_redirect.h"

#include "tests.h"
#include "child.h"
#include "utils.h"

/* Run a request to 'path' and retrieve the redirect destination to
 * *redir. */
static int process_redir(ne_session *sess, const char *path,
                         const ne_uri **redir)
{
    int ret = any_request(sess, path);
    ONV(ret != NE_REDIRECT,
        ("request got %d (%s) rather than NE_REDIRECT",
         ret, ne_get_error(sess)));
    *redir = ne_redirect_location(sess);
    return OK;
}

static int check_redir(int status_code, const char *location,
                       const char *target, const char *fragment,
                       const char *expect)
{
    char *unp, *full_expect = NULL, response[BUFSIZ];
    ne_session *sess;
    ne_uri *loc;
    ne_request *req;

    ne_snprintf(response, sizeof response,
		"HTTP/1.0 %d Get Ye Away\r\n"
		"Content-Length: 0\r\n"
		"Location: %s\r\n\n",
		status_code, location);

    CALL(multi_session_server(&sess, "http", "localhost", 2,
                              single_serve_string, response));

    if (expect[0] == '/') {
        ne_uri uri = {0};
        ne_fill_server_uri(sess, &uri);
        uri.path = (char *)expect;
        uri.fragment = (char *)fragment;
        full_expect = ne_uri_unparse(&uri);
        expect = full_expect;
        uri.path = NULL;
        uri.fragment = NULL;
        ne_uri_free(&uri);
    }

    /* First test the ne_get_response_location() API directly. */
    NE_DEBUG(NE_DBG_HTTP, "redirect: Target [%s ## %s]\n", target,
             fragment ? fragment : "(no fragment)");
    NE_DEBUG(NE_DBG_HTTP, "redirect: Location: [%s]\n", location);

    req = ne_request_create(sess, "GET", target);
    ONREQ(ne_request_dispatch(req));
    loc = ne_get_response_location(req, fragment);

    unp = ne_uri_unparse(loc);
    NE_DEBUG(NE_DBG_HTTP, "redirect: ne_get_response_location => [%s]\n", unp);
    ONV(strcmp(unp, expect), ("first redirect to `%s' not `%s'", unp, expect));
    ne_free(unp);
    ne_request_destroy(req);
    ne_uri_free(loc);
    ne_free(loc);

    if (fragment) {
        /* Can't handle fragments in the ne_redirect API, send a dummy request. */
        CALL(any_request(sess, "/dummy"));
    }
    else {
        const ne_uri *cloc;

        /* Second, test the ne_redirect* API. */
        ne_redirect_register(sess);

        CALL(process_redir(sess, target, &cloc));
        ONN("redirect location was NULL", cloc == NULL);

        unp = ne_uri_unparse(cloc);
        NE_DEBUG(NE_DBG_HTTP, "redirect: ne_redirect URI => [%s]\n", unp);
        ONV(strcmp(unp, expect), ("second redirect to `%s' not `%s'", unp, expect));
        ne_free(unp);
    }

    if (full_expect) ne_free(full_expect);

    return destroy_and_wait(sess);
}

#define DEST "http://foo.com/blah/blah/bar"
#define PATH "/redir/me"

static int redirects(void)
{
    const struct {
        const char *target;
        int code;
        const char *location;
        const char *expected;
        const char *fragment;
    } ts[] = {
        {PATH, 301, DEST, DEST, NULL},
        {PATH, 302, DEST, DEST, NULL},
        {PATH, 303, DEST, DEST, NULL},
        {PATH, 307, DEST, DEST, NULL},

        /* Simple relative URI cases: */
        {PATH, 302, "/foo/bar/blah", "/foo/bar/blah", NULL},
        {"/foo/bar", 302, "norman", "/foo/norman", NULL},
        {"/foo/bar/", 302, "wishbone", "/foo/bar/wishbone", NULL},

        /* all 3xx should get NE_REDIRECT. */
        {PATH, 399, DEST, DEST, NULL},

        /* Handling of various request-target cases. */
        {"*", 307, "/fish#food", "/fish#food", NULL},
        {"ftp://example.com/fish", 307, "/fish#food", "ftp://example.com/fish#food", NULL},

        /* More relative URIs: */
        {"/blah", 307, "//example.com:8080/fish#food", "http://example.com:8080/fish#food", NULL},
        {"/blah", 307, "#food", "/blah#food", NULL},

        /* Fragment handling. */
        {"/foo", 301, "http://example.com/redirect", "http://example.com/redirect#fragment",
         "fragment" },
        {"/foo", 301, "https://blah.example.com/redirect#override",
         "https://blah.example.com/redirect#override", "fragment" },
    };
    unsigned n;
    
    for (n = 0; n < sizeof(ts)/sizeof(ts[0]); n++) {
        CALL(check_redir(ts[n].code, ts[n].location, ts[n].target, ts[n].fragment,
                         ts[n].expected));
    }

    return OK;
}

#define RESP1 "HTTP/1.1 200 OK\r\n" "Content-Length: 0\r\n\r\n"
#define RESP2 "HTTP/1.0 302 Get Ye Away\r\n" "Location: /blah\r\n" "\r\n"

/* ensure that ne_redirect_location returns NULL when no redirect has
 * been encountered, or redirect hooks aren't registered. */
static int no_redirect(void)
{
    ne_session *sess;
    const ne_uri *loc;
    struct double_serve_args resp;

    resp.first.data = RESP1;
    resp.first.len = strlen(RESP1);
    resp.second.data = RESP2;
    resp.second.len = strlen(RESP2);

    CALL(session_server(&sess, double_serve_sstring, &resp));
    ONN("redirect non-NULL before register", ne_redirect_location(sess));
    ne_redirect_register(sess);
    ONN("initial redirect non-NULL", ne_redirect_location(sess));

    ONREQ(any_request(sess, "/noredir"));

    ONN("redirect non-NULL after non-redir req", ne_redirect_location(sess));

    CALL(process_redir(sess, "/foo", &loc));

    return destroy_and_wait(sess);
}

ne_test tests[] = {
    T(lookup_localhost),
    T(redirects),
    T(no_redirect),
    T(NULL) 
};

