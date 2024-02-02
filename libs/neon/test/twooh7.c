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
        const char *expect;
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
          "such thingy" }
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
        ONN("ne_simple_request didn't fail", ret == NE_OK);

        err = ne_strclean(ne_strdup(ne_get_error(sess)));
        ONV(strcmp(err, ne_get_error(sess)),
            ("error string wasn't cleaned: %s", ne_get_error(sess)));
        NE_DEBUG(NE_DBG_HTTP, "test: got error string: %s\n", err);
        ne_free(err);

        ONV(strstr(ne_get_error(sess), ts[n].expect) == NULL,
            ("error string didn't match: '%s' - expected '%s'",
             ne_get_error(sess), ts[n].expect));

        ne_session_destroy(sess);
        CALL(await_server());
    }

    return OK;
}

ne_test tests[] = {
    T(simples),
    T(NULL)
};

