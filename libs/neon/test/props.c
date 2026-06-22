/* 
   Tests for property handling
   Copyright (C) 2002-2009, Joe Orton <joe@manyfish.co.uk>

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

#include "ne_props.h"

#include "tests.h"
#include "child.h"
#include "utils.h"

static const ne_propname p_alpha = {"DAV:", "alpha"},
    p_beta = {"http://webdav.org/random/namespace", "beta"};

/* Tests little except that ne_proppatch() doesn't segfault. */
static int patch_simple(void)
{
    ne_session *sess;
    ne_proppatch_operation ops[] = {
	{ &p_alpha, ne_propset, "fish" },
	{ &p_beta, ne_propremove, NULL },
	{ NULL, ne_propset, NULL }
    };
    
    CALL(make_session(&sess, single_serve_string, 
		      "HTTP/1.1 200 Goferit\r\n"
		      "Connection: close\r\n\r\n"));
    ONREQ(ne_proppatch(sess, "/fish", ops));

    return destroy_and_wait(sess);
}

#define RESP207 "HTTP/1.0 207 Stuff\r\n" "Server: foo\r\n\r\n"

static void dummy_results(void *ud, const ne_uri *uri,
			  const ne_prop_result_set *rset)
{
    NE_DEBUG(NE_DBG_HTTP, "dummy_results.\n");
}

/* Regression tests for propfind bodies which caused segfaults. */
static int regress(void)
{
    static const char *bodies[] = { 
	RESP207 "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
	"<multistatus xmlns=\"DAV:\">"
	"<response><propstat><prop><href>"
	"</href></prop></propstat></response>"
	"</multistatus>",
	
	/* segfaults with neon <= 0.23.5 */
	RESP207 "<?xml version=\"1.0\"?><D:multistatus xmlns:D=\"DAV:\">"
	"<D:response><D:href>/foo/</D:href>"
	"<D:propstat/>"
	"<D:status>HTTP/1.1 404 Not Found</D:status>"
	"</D:multistatus>",

	/* format string handling with neon <= 0.24.4 */
	RESP207 "<?xml version=\"1.0\"?><D:multistatus xmlns:D=\"DAV:\">"
	"<D:response><D:href>/foo/</D:href>"
	"<D:propstat/>"
	"<D:status>%s%s%s%s</D:status>"
	"</D:response></D:multistatus>",

	NULL,
    };
    ne_session *sess;
    int n;

    for (n = 0; bodies[n] != NULL; n++) {
	CALL(make_session(&sess, single_serve_string, (void *)bodies[n]));
	ne_simple_propfind(sess, "/", 0, NULL, dummy_results, NULL);
	ne_session_destroy(sess);
	CALL(await_server());
    }

    return OK;
}

static int patch_regress(void)
{
    static const char *bodies[] = { 
	/* format string handling bugs with neon <= 0.24.4 */
	RESP207 "<?xml version=\"1.0\"?><D:multistatus xmlns:D=\"DAV:\">"
	"<D:response><D:href>/foo/</D:href>"
	"<D:status>HTTP/1.1 500 Bad Voodoo</D:status>"
	"<D:responsedescription>%s%s%s%s</D:responsedescription>"
        "</D:response></D:multistatus>",

	RESP207 "<?xml version=\"1.0\"?><D:multistatus xmlns:D=\"DAV:\">"
	"<D:response><D:href>/foo/</D:href>"
	"<D:status>HTTP/1.1 %s%s%s%s</D:status>",

        NULL
    };
    ne_session *sess;
    int n;
    static const ne_propname pn = { "DAV:", "foobar" };
    ne_proppatch_operation pops[] = { 
        { &pn, ne_propset, "fish" },
        { NULL, ne_propset, NULL }
    };

    for (n = 0; bodies[n] != NULL; n++) {
	CALL(make_session(&sess, single_serve_string, (void *)bodies[n]));
	ne_proppatch(sess, "/", pops);
	ne_session_destroy(sess);
	CALL(await_server());
    }

    return OK;
}

/* Serialize propfind result callbacks into a string */
static int simple_iterator(void *vbuf, const ne_propname *name,
                           const char *value, const ne_status *st)
{
    char code[20];
    ne_buffer *buf = vbuf;

    ne_buffer_concat(buf, "prop:[{", name->nspace, ",", 
                     name->name, "}=", NULL);
    if (value)
        ne_buffer_concat(buf, "'", value, "'", NULL);
    else 
        ne_buffer_zappend(buf, "#novalue#");
    sprintf(code, ":{%d ", st->code);
    if (st->reason_phrase)
        ne_buffer_concat(buf, code, st->reason_phrase, "}];", NULL);
    else
        ne_buffer_concat(buf, code, "#noreason#}];", NULL);
    return 0;
}

static void simple_results(void *buf, const ne_uri *uri,
                           const ne_prop_result_set *rset)
{
    ne_buffer_concat(buf, "results(", uri->path, ",", NULL);
    ne_propset_iterate(rset, simple_iterator, buf);
    ne_buffer_czappend(buf, ")//");
}

/* Test function to compare two long strings and print a digestible
 * failure message. */
static int diffcmp(const char *expected, const char *actual)
{
    size_t n;
    
    if (!strcmp(expected, actual)) return OK;

    NE_DEBUG(NE_DBG_HTTP, 
             "diffcmp: Expect: [%s]\n"
             "diffcmp: Actual: [%s]\n",
             expected, actual);

    for (n = 0; expected[n] && actual[n]; n++) {
        if (expected[n] != actual[n]) {
            t_context("difference at byte %" NE_FMT_SIZE_T ": "
                      "`%.10s...' not `%.10s...'",
                      n, actual+n, expected+n);
            break;
        }
    }

    return FAIL;
}

/* PROPFIND creator callback. */
static void *pf_creator(void *userdata, const ne_uri *uri)
{
    ne_buffer *buf = userdata;

    NE_DEBUG(NE_DBG_HTTP, "pf: Creator at %s\n", uri->path);

    ne_buffer_concat(buf, "creator[", uri->path, "]//", NULL);

    return ne_strdup(uri->path);
}

/* PROPFIND destructor callback. */
static void pf_destructor(void *userdata, void *private)
{
    ne_buffer *buf = userdata;
    char *cookie = private;

    NE_DEBUG(NE_DBG_HTTP, "pf: Destructor at %s\n", cookie);

    ne_buffer_concat(buf, "destructor[", cookie, "]//", NULL);
    
    ne_free(cookie);
}

  
/* PROPFIND test type. */     
enum pftype { 
    PF_SIMPLE, /* using ne_simple_propfind */
    PF_NAMED,  /* using ne_propfind_named */
    PF_SP_NAMED, /* using ne_propfind_named w/SHAREPOINT hacks */
    PF_ALLPROP /* using ne_propfind_allprop */
};

static int run_propfind(const ne_propname *props, char *resp, 
                        int depth, const char *expected, enum pftype type)
{
    ne_session *sess;
    ne_buffer *buf = ne_buffer_create();

    CALL(make_session(&sess, single_serve_string, resp));

    if (type == PF_SIMPLE) {
        ONREQ(ne_simple_propfind(sess, "/propfind", depth, props,
                                 simple_results, buf));
    }
    else {
        ne_propfind_handler *hdl;

        if (type == PF_SP_NAMED) {
            ne_set_session_flag(sess, NE_SESSFLAG_SHAREPOINT, 1);
            type = PF_NAMED;
        }

        hdl = ne_propfind_create(sess, "/propfind", depth);

        ne_propfind_set_private(hdl, pf_creator, pf_destructor,
                                buf);
        
        if (type == PF_NAMED) {
            ONREQ(ne_propfind_named(hdl, props, simple_results, buf));
        }
        else {
            ONREQ(ne_propfind_allprop(hdl, simple_results, buf));
        }

        ne_propfind_destroy(hdl);
    }

    ne_session_destroy(sess);
    CALL(await_server());

    CALL(diffcmp(expected, buf->data));

    ne_buffer_destroy(buf);
    return OK;
}

/* a PROPFIND response body for the {DAV:}fishbone property, using
 * given property value and status. */
#define FISHBONE_RESP(value, status) MULTI_207(RESP_207("/foop", \
	PSTAT_207(PROPS_207(APROP_207("fishbone", value)) \
	STAT_207(status))))

static int propfind(void)
{
    static const struct {
        char *resp;
        const char *expected;
        int depth;
        enum pftype type;
    } ts[] = {
        /* simple single property. */
        { FISHBONE_RESP("hello, world", "212 Well OK"),
          "results(/foop,prop:[{DAV:,fishbone}='hello, world':{212 Well OK}];)//",
          0, PF_SIMPLE },
        /* property with some nested elements. */
        { FISHBONE_RESP("this is <foo/> a property <bar><lemon>fish</lemon></bar> value", 
                        "299 Just About OK"),
          "results(/foop,prop:[{DAV:,fishbone}="
          "'this is <foo></foo> a property "
          "<bar><lemon>fish</lemon></bar> value':"
          "{299 Just About OK}];)//",
          0, PF_SIMPLE },

        /* failed to fetch a property. */
        { FISHBONE_RESP("property value is ignored", 
                        "404 Il n'ya pas de property"),
          "results(/foop,prop:[{DAV:,fishbone}=#novalue#:"
          "{404 Il n'ya pas de property}];)//",
          0, PF_SIMPLE },

#if 0
        /* propstat missing status should be ignored; if a response contains no
         * valid propstats, it should also be ignored. */
        { MULTI_207(RESP_207("/alpha", PSTAT_207(APROP_207("fishbone", "unseen")))
                    RESP_207("/beta", PSTAT_207(APROP_207("fishbone", "hello, world")
                                                STAT_207("200 OK")))),
          "results(/beta,prop:[{DAV:,fishbone}='hello, world':{200 OK}];)//", 0, 
          PF_SIMPLE },
#endif

        /* props on several resources */
        { MULTI_207(RESP_207("/alpha",
                             PSTAT_207(PROPS_207(APROP_207("fishbone", "strike one"))
                                       STAT_207("234 First is OK")))
                    RESP_207("/beta",
                             PSTAT_207(PROPS_207(APROP_207("fishbone", "strike two"))
                                       STAT_207("256 Second is OK")))),
          "results(/alpha,prop:[{DAV:,fishbone}='strike one':{234 First is OK}];)//"
          "results(/beta,prop:[{DAV:,fishbone}='strike two':{256 Second is OK}];)//",
          0, PF_SIMPLE},

        /* whitespace handling. */
        { MULTI_207(RESP_207("\r\nhttp://localhost/alpha ",
                             PSTAT_207(PROPS_207(APROP_207("alpha", "beta"))
                                       "<D:status>\r\nHTTP/1.1 200 OK </D:status>"))),
          "results(/alpha,prop:[{DAV:,alpha}='beta':{200 OK}];)//",
          0, PF_SIMPLE},
        
        /* attribute handling. */
        { MULTI_207(RESP_207("\r\nhttp://localhost/alpha ",
                             PSTAT_207(PROPS_207("<D:alpha>"
                                                 "<D:foo D:fee='bar' bar='fee'>beta</D:foo></D:alpha>")
                                       "<D:status>\r\nHTTP/1.1 200 OK </D:status>"))),
          "results(/alpha,prop:[{DAV:,alpha}='<DAV:foo DAV:fee='bar' bar='fee'>beta</DAV:foo>':{200 OK}];)//",
          0, PF_SIMPLE},
        

        /* "complex" propfinds. */

        { FISHBONE_RESP("hello, world", "212 Well OK"),
          "creator[/foop]//"
          "results(/foop,prop:[{DAV:,fishbone}='hello, world':{212 Well OK}];)//"
          "destructor[/foop]//",
          0, PF_NAMED },

        /* 207 with badly encoded URI in href */
        { MULTI_207(RESP_207("http://example.com/foo€bar", \
                             PSTAT_207(PROPS_207(APROP_207("fishbone", "hello, world")) \
                                       STAT_207("209 Good News")))),
          "creator[/foo%e2%82%acbar]//"
          "results(/foo%e2%82%acbar,prop:[{DAV:,fishbone}='hello, world':{209 Good News}];)//"
          "destructor[/foo%e2%82%acbar]//",
          0, PF_SP_NAMED },

        { MULTI_207(RESP_207("/foo%20bar/€bar", \
                             PSTAT_207(PROPS_207(APROP_207("fishbone", "hello, world")) \
                                       STAT_207("209 Good News")))),
          "creator[/foo%20bar/%e2%82%acbar]//"
          "results(/foo%20bar/%e2%82%acbar,prop:[{DAV:,fishbone}='hello, world':{209 Good News}];)//"
          "destructor[/foo%20bar/%e2%82%acbar]//",
          0, PF_SP_NAMED }

    };
    const ne_propname pset1[] = {
        { "DAV:", "fishbone", },
        { NULL, NULL }
    };
    unsigned n;

    for (n = 0; n < sizeof(ts)/sizeof(ts[0]); n++) {
        const ne_propname *pset = pset1;

        NE_DEBUG(NE_DBG_HTTP, "--> running test %u for %s\n", n, ts[n].resp);
        CALL(run_propfind(pset, ts[n].resp, ts[n].depth,
                          ts[n].expected, ts[n].type));
    }


    return OK;
}

static int unbounded_response(const char *header, const char *repeats)
{
    ne_session *sess;
    struct infinite i = { header, repeats};

    CALL(make_session(&sess, serve_infinite, &i));

    ONN("unbounded PROPFIND response did not fail",
        ne_simple_propfind(sess, "/", 0, NULL, 
                           dummy_results, NULL) != NE_ERROR);

    CALL(reap_server());    
    ne_session_destroy(sess);
    return OK;
}

static int unbounded_propstats(void)
{
    return unbounded_response(
	RESP207 "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
	"<multistatus xmlns=\"DAV:\">"
	"<response><href>/</href>",
        "<propstat></propstat>");
}

static int unbounded_props(void)
{
    return unbounded_response(
	RESP207 "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
	"<multistatus xmlns=\"DAV:\">"
	"<response><href>/</href><propstat>",
        "<prop><jim>hello, world</jim></prop>");
}

ne_test tests[] = {
    T(patch_simple),
    T(propfind),
    T(regress),
    T(patch_regress),
    T(unbounded_props),
    T(unbounded_propstats),
    T(NULL) 
};

