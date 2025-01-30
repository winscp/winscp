/* 
   HTTP-redirect support
   Copyright (C) 1999-2024, Joe Orton <joe@manyfish.co.uk>

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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "ne_session.h"
#include "ne_request.h"
#include "ne_alloc.h"
#include "ne_uri.h"
#include "ne_redirect.h"
#include "ne_internal.h"
#include "ne_string.h"
#ifdef WINSCP
#include "ne_auth.h"
#endif

#define REDIRECT_ID "http://www.webdav.org/neon/hooks/http-redirect"

struct redirect {
    ne_uri *uri;
};

#define uri_free_clear(r_) do { if ((r_)->uri) { ne_uri_free((r_)->uri); ne_free((r_)->uri); (r_)->uri = NULL; }} while (0)

static void create(ne_request *req, void *userdata,
                   const char *method, const char *target)
{
    struct redirect *red = userdata;

    uri_free_clear(red);
}

static int post_send(ne_request *req, void *userdata, const ne_status *status)
{
    struct redirect *red = userdata;
    ne_uri *loc;

    uri_free_clear(red);

    if (status->klass == 3
#ifdef WINSCP
        && !is_passport_challenge(req, status)
#endif
        && (loc = ne_get_response_location(req, NULL)) != NULL
        ) {
        red->uri = loc;
        return NE_REDIRECT;
    }

    return NE_OK;
}

static void free_redirect(void *cookie)
{
    struct redirect *red = cookie;
    uri_free_clear(red);
    ne_free(red);
}

void ne_redirect_register(ne_session *sess)
{
    struct redirect *red = ne_calloc(sizeof *red);
    
    ne_hook_create_request(sess, create, red);
    ne_hook_post_send(sess, post_send, red);
    ne_hook_destroy_session(sess, free_redirect, red);

    ne_set_session_private(sess, REDIRECT_ID, red);
}

const ne_uri *ne_redirect_location(ne_session *sess)
{
    struct redirect *red = ne_get_session_private(sess, REDIRECT_ID);

    return red ? red->uri : NULL;
}

