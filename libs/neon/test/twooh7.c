/* 
   Test cases for the ne_207.h interface.
   Copyright (C) 2023, Joe Orton <joe@manyfish.co.uk>

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

#include "ne_207.h"

#include "tests.h"
#include "utils.h"

#define PFX "<?xml version='1.0' encoding='utf-8'?>\r\n"

#define RESP(status, rdesc) "<d:response>"                              \
    "<d:href>http://localhost/container/resource3</d:href>"       \
    status rdesc                                                        \
    "</d:response>"

#define MS_207_1(status, rdesc) PFX               \
    "<d:multistatus xmlns:d=\"DAV:\">"          \
    RESP(status, rdesc)                         \
    "</d:multistatus>"

#define MS_207_2(s1, rd1, s2, rd2) PFX          \
    "<d:multistatus xmlns:d=\"DAV:\">"          \
    RESP(s1, rd1)                               \
    RESP(s2, rd2)                             \
    "</d:multistatus>"

static int simples(void)
{
    static const struct {
        int status;
        const char *ctype;
        const char *body;
        const char *expect; /* NULL for success cases. */
    } ts[] = {
        { 207, "application/xml",
          MS_207_1("<d:status>HTTP/1.1 423 Locked</d:status>", ""),
          "423 Locked" },
        { 207, "application/xml",
          MS_207_1("<d:status>HTTP/1.1 423 Locked</d:status>",
                   "<d:responsedescription>The thing was locked</d:responsedescription>"),
          "The thing was" },
#if 0
         { 207, "application/xml",
          MS_207_1("<d:status>HTTP/1.1 423 Locked</d:status>",
                   "<d:error><d:lock-token-submitted/></d:error>"),
          "Resource locked" },
#endif
        { 207, "application/xml",
          MS_207_2("<d:status>HTTP/1.1 423 Locked</d:status>",
                   "<d:responsedescription>The thing was locked</d:responsedescription>",
                   "<d:status>HTTP/1.1 404 Gone</d:status>",
                 "<d:responsedescription>No such thingy</d:responsedescription>"),
          "such thingy" },
        /* Test that 201 responses are NOT parsed. */
        { 201, "application/xml", "<malformed-xml>", NULL },
        /* Test that non-2xx response is also an error. */
        { 404, "application/xml", "<malformed-xml>", "404" }
    };
    unsigned n;

    for (n = 0; n < sizeof(ts)/sizeof(ts[0]); n++) {
        char resp[1024];
        ne_session *sess;
        ne_request *req;
        char *err;
        int ret;

        ne_snprintf(resp, sizeof resp,
                    "HTTP/1.1 %d OK\r\n"
                    "Content-Type: %s\r\n"
                    "Connection: close\r\n" "\r\n"
                    "%s", ts[n].status, ts[n].ctype, ts[n].body);

        CALL(make_session(&sess, single_serve_string, resp));

        req = ne_request_create(sess, "SIMPLE", "/");

        ret = ne_simple_request(sess, req);

        if (ts[n].expect) {
            ONN("ne_simple_request didn't fail", ret == NE_OK);

            err = ne_strclean(ne_strdup(ne_get_error(sess)));
            ONV(strcmp(err, ne_get_error(sess)),
                ("error string wasn't cleaned: %s", ne_get_error(sess)));
            NE_DEBUG(NE_DBG_HTTP, "test: got error string: %s\n", err);
            ne_free(err);

            ONV(strstr(ne_get_error(sess), ts[n].expect) == NULL,
                ("error string didn't match: '%s' - expected '%s'",
                 ne_get_error(sess), ts[n].expect));
        }
        else {
            ONREQ(ret);
        }

        ne_session_destroy(sess);
        CALL(await_server());
    }

    return OK;
}

static int pstat_count;

/* tos_*: set of 207 callbacks which serialize the data back into a
 * text stream, which can be easily checked for correctness. */
static void *tos_startresp(void *buf, const ne_uri *uri)
{
    ne_buffer_concat(buf, "start-resp[", uri->path, "];", NULL);
    pstat_count = 0;
    return ne_strdup(uri->path);
}

static void tos_status_descr(ne_buffer *buf, const ne_status *status,
                             const char *description)
{
    if (status) {
        char s[50];
        ne_snprintf(s, sizeof s, "-status={%d %s}", status->code,
                    status->reason_phrase);
        ne_buffer_zappend(buf, s);
    }
    if (description)
        ne_buffer_concat(buf, "-descr={", description, "}", NULL);
}

static void tos_endresp(void *buf, void *response,
                        const ne_status *status, const char *description)
{
    char *href = response;
    ne_buffer_concat(buf, "end-resp[", href, "]", NULL);
    ne_free(href);
    tos_status_descr(buf, status, description);
    ne_buffer_zappend(buf, ";");
}

static void *tos_startpstat(void *buf, void *resphref)
{
    char num[20], *href;
    sprintf(num, "-%d", ++pstat_count);
    href = ne_concat(resphref, num, NULL);
    ne_buffer_concat(buf, "start-pstat[", href, "];", NULL);
    return href;
}

static void tos_endpstat(void *buf, void *href,
                         const ne_status *status, const char *description)
{
    ne_buffer_concat(buf, "end-pstat[", href, "]", NULL);
    tos_status_descr(buf, status, description);
    ne_buffer_zappend(buf, ";");
    ne_free(href);
}

struct propctx {
    ne_207_parser *p207;
    ne_buffer *buf;
};

#define STATE_myprop (NE_207_STATE_TOP)

static int tos_startprop(void *userdata, int parent,
                         const char *nspace, const char *name,
                         const char **atts)
{
    if (parent == NE_207_STATE_PROP
        && strcmp(nspace, "DAV:") == 0
        && (strcmp(name, "propone") == 0 || strcmp(name, "proptwo") == 0)) {
        /* Handle this! */
        struct propctx *ctx = userdata;
        char *resphref = ne_207_get_current_response(ctx->p207);
        char *pstathref = ne_207_get_current_propstat(ctx->p207);
        
        ne_buffer_concat(ctx->buf, "start-prop[", resphref, ",", pstathref,
                         ",", name, "];", NULL);

        return STATE_myprop;
    }
    else {
        return NE_XML_DECLINE;
    }
}

static int tos_cdata(void *userdata, int state,
                     const char *cdata, size_t len)
{
    struct propctx *ctx = userdata;

    ne_buffer_zappend(ctx->buf, "cdata-prop[");
    ne_buffer_append(ctx->buf, cdata, len);
    ne_buffer_zappend(ctx->buf, "];");
    return 0;
}

static int tos_endprop(void *userdata, int state,
                       const char *nspace, const char *name)
{
    struct propctx *ctx = userdata;

    ne_buffer_concat(ctx->buf, "end-prop[", name, "];", NULL);
    return 0;
}

static int run_207_response(const char *resp, const char *expected)
{
    ne_buffer *buf = ne_buffer_create();
    ne_session *sess;
    ne_xml_parser *p = ne_xml_create();
    ne_207_parser *p207;
    ne_request *req;
    ne_uri base = {0};
    struct propctx ctx;

    CALL(session_server(&sess, single_serve_string, (char *)resp));
    req = ne_request_create(sess, "PROPFIND", "/foo");
    ne_fill_server_uri(sess, &base);
    base.path = ne_strdup("/foo");
    p207 = ne_207_create(p, &base, buf);
    ne_uri_free(&base);

    ne_add_response_body_reader(req, ne_accept_207, ne_xml_parse_v, p);

    ne_207_set_response_handlers(p207, tos_startresp, tos_endresp);
    ne_207_set_propstat_handlers(p207, tos_startpstat, tos_endpstat);

    ctx.buf = buf;
    ctx.p207 = p207;
    ne_xml_push_handler(p, tos_startprop, tos_cdata, tos_endprop, &ctx);

    ONREQ(ne_request_dispatch(req));

    CALL(await_server());

    ONV(ne_xml_failed(p),
        ("parse error in response body: %s", ne_xml_get_error(p)));

    ONV(strcmp(buf->data, expected),
        ("comparison failed.\n"
         "expected string: `%s'\n"
         "got string:      `%s'", expected, buf->data));

    ne_buffer_destroy(buf);
    ne_207_destroy(p207);
    ne_xml_destroy(p);
    ne_request_destroy(req);
    ne_session_destroy(sess);
    return OK;
}

/* Tests for the 207 interface: send a 207 response body, compare the
 * re-serialized string returned with that expected. */
static int two_oh_seven(void)
{
    static const char *ts[][2] = {
        { MULTI_207(RESP_207("/foo", "")),
          "start-resp[/foo];end-resp[/foo];" },

        /* test for response status handling */
        { MULTI_207(RESP_207("/bar", STAT_207("200 OK"))), 
          "start-resp[/bar];end-resp[/bar]-status={200 OK};" },

        /* test that empty description == NULL description argument */
        { MULTI_207(RESP_207("/bar", STAT_207("200 OK") DESCR_207(""))), 
          "start-resp[/bar];end-resp[/bar]-status={200 OK};" },

        /* test multiple responses */
        { MULTI_207(RESP_207("/hello/world", STAT_207("200 OK"))
                    RESP_207("/foo/bar", STAT_207("599 French Fries"))),
          "start-resp[/hello/world];end-resp[/hello/world]-status={200 OK};"
          "start-resp[/foo/bar];end-resp[/foo/bar]"
          "-status={599 French Fries};"
        },

        /* test multiple propstats in multiple responses */
        { MULTI_207(RESP_207("/al/pha", 
                             PSTAT_207(STAT_207("321 Une"))
                             PSTAT_207(STAT_207("432 Deux"))
                             PSTAT_207(STAT_207("543 Trois")))
                    RESP_207("/be/ta",
                             PSTAT_207(STAT_207("587 Quatre"))
                             PSTAT_207(STAT_207("578 Cinq")))),
          "start-resp[/al/pha];"
          "start-pstat[/al/pha-1];end-pstat[/al/pha-1]-status={321 Une};"
          "start-pstat[/al/pha-2];end-pstat[/al/pha-2]-status={432 Deux};"
          "start-pstat[/al/pha-3];end-pstat[/al/pha-3]-status={543 Trois};"
          "end-resp[/al/pha];"
          "start-resp[/be/ta];"
          "start-pstat[/be/ta-1];end-pstat[/be/ta-1]-status={587 Quatre};"
          "start-pstat[/be/ta-2];end-pstat[/be/ta-2]-status={578 Cinq};"
          "end-resp[/be/ta];"
        },

        /* test that incomplete responses are completely ignored. */
        { MULTI_207("<D:response/>"
                    RESP_207("/", STAT_207("123 Hoorah"))
                    "<D:response/>"
                    "<D:response><D:propstat>hello</D:propstat></D:response>"
                    "<D:response><D:href/></D:response>"
                    RESP_207("/bar", STAT_207("200 OK"))),
          "start-resp[/];end-resp[/]-status={123 Hoorah};"
          "start-resp[/bar];end-resp[/bar]-status={200 OK};" },

        /* tests for propstat status */
        { MULTI_207(RESP_207("/pstat",
                            PSTAT_207("<D:prop/>" STAT_207("466 Doomed")))),
          "start-resp[/pstat];start-pstat[/pstat-1];"
          "end-pstat[/pstat-1]-status={466 Doomed};end-resp[/pstat];" },

        { MULTI_207(RESP_207("/pstat", PSTAT_207("<D:status/>"))),
          "start-resp[/pstat];start-pstat[/pstat-1];"
          "end-pstat[/pstat-1];end-resp[/pstat];" },

        /* tests for responsedescription handling */
        { MULTI_207(RESP_207("/bar", STAT_207("200 OK") DESCR_207(DESCR_REM))), 
          "start-resp[/bar];end-resp[/bar]-status={200 OK}"
          "-descr={" DESCR_REM "};" },

        { MULTI_207(RESP_207("/bar",
                             PSTAT_207(STAT_207("456 Too Hungry")
                                       DESCR_207("Not enough food available"))
                             STAT_207("200 OK") DESCR_207("Not " DESCR_REM))), 
          "start-resp[/bar];"
          "start-pstat[/bar-1];end-pstat[/bar-1]-status={456 Too Hungry}"
          "-descr={Not enough food available};"
          "end-resp[/bar]-status={200 OK}-descr={Not " DESCR_REM "};" },

        /* intermingle some random elements and cdata to make sure
         * they are ignored. */
        { MULTI_207("<D:fish-food/>blargl" 
                    RESP_207("/b<ping-pong/>ar", "<D:sausages/>"
                             PSTAT_207("<D:hello-mum/>blergl") 
                             STAT_207("200 OK") "<D:blah>foop</D:blah>"
                             DESCR_207(DESCR_REM) "carroon") 
                    "carapi"), 
          "start-resp[/bar];start-pstat[/bar-1];end-pstat[/bar-1];"
          "end-resp[/bar]-status={200 OK}-descr={" DESCR_REM "};" },

        /* test for properties within a 207. */
        { MULTI_207(RESP_207("/alpha",
                             PSTAT_207(PROPS_207(
                                           APROP_207("propone", "hello")
                                           APROP_207("proptwo", "foobar"))
                                       STAT_207("200 OK")))),
          "start-resp[/alpha];start-pstat[/alpha-1];"
          "start-prop[/alpha,/alpha-1,propone];cdata-prop[hello];"
          "end-prop[propone];"
          "start-prop[/alpha,/alpha-1,proptwo];cdata-prop[foobar];"
          "end-prop[proptwo];"
          "end-pstat[/alpha-1]-status={200 OK};end-resp[/alpha];" },

        /* test for whitespace around href, which should be
         * ignored. */
        { MULTI_207(RESP_207(" /spaces ", STAT_207("200 OK") DESCR_207(""))),
          "start-resp[/spaces];end-resp[/spaces]-status={200 OK};" },

        /* test for whitespace around href, which should be
         * ignored. */
        { MULTI_207(RESP_207(" /spaces ", STAT_207("200 OK\n") DESCR_207(""))),
          "start-resp[/spaces];end-resp[/spaces]-status={200 OK};" },

        /* test for omitted reason-phrase in status-line, which is
         * valid, per https://github.com/notroj/neon/issues/188 */
        { MULTI_207(RESP_207("/bar", STAT_207("200 ") DESCR_207(""))),
          "start-resp[/bar];end-resp[/bar]-status={200 };" }
    };
    unsigned n;

    for (n = 0; n < sizeof(ts)/sizeof(ts[0]); n++)
        CALL(run_207_response(ts[n][0], ts[n][1]));

    return OK;
}

ne_test tests[] = {
    T(simples),
    T(two_oh_seven),
    T(NULL)
};

