/* $Id: testback.c,v 1.10 2004/06/20 17:07:32 jacob Exp $ */
/*
 * Copyright (c) 1999 Simon Tatham
 * Copyright (c) 1999 Ben Harris
 * All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/* PuTTY test backends */

#include <stdio.h>
#include <stdlib.h>

#include "putty.h"

static const char *null_init(void *, void **, Config *, char *, int, char **,
			     int, int);
static const char *loop_init(void *, void **, Config *, char *, int, char **,
			     int, int);
static void null_free(void *);
static void loop_free(void *);
static void null_reconfig(void *, Config *);
static int null_send(void *, char *, int);
static int loop_send(void *, char *, int);
static int null_sendbuffer(void *);
static void null_size(void *, int, int);
static void null_special(void *, Telnet_Special);
static const struct telnet_special *null_get_specials(void *handle);
static Socket null_socket(void *);
static int null_exitcode(void *);
static int null_sendok(void *);
static int null_ldisc(void *, int);
static void null_provide_ldisc(void *, void *);
static void null_provide_logctx(void *, void *);
static void null_unthrottle(void *, int);

Backend null_backend = {
    null_init, null_free, null_reconfig, null_send, null_sendbuffer, null_size,
    null_special, null_get_specials, null_socket, null_exitcode, null_sendok,
    null_ldisc, null_provide_ldisc, null_provide_logctx, null_unthrottle, 0
};

Backend loop_backend = {
    loop_init, loop_free, null_reconfig, loop_send, null_sendbuffer, null_size,
    null_special, null_get_specials, null_socket, null_exitcode, null_sendok,
    null_ldisc, null_provide_ldisc, null_provide_logctx, null_unthrottle, 0
};

struct loop_state {
    Terminal *term;
};

static const char *null_init(void *frontend_handle, void **backend_handle,
			     Config *cfg, char *host, int port,
			     char **realhost, int nodelay, int keepalive) {

    return NULL;
}

static const char *loop_init(void *frontend_handle, void **backend_handle,
			     Config *cfg, char *host, int port,
			     char **realhost, int nodelay, int keepalive) {
    struct loop_state *st = snew(struct loop_state);

    st->term = frontend_handle;
    *backend_handle = st;
    return NULL;
}

static void null_free(void *handle)
{

}

static void loop_free(void *handle)
{

    sfree(handle);
}

static void null_reconfig(void *handle, Config *cfg) {

}

static int null_send(void *handle, char *buf, int len) {

    return 0;
}

static int loop_send(void *handle, char *buf, int len) {
    struct loop_state *st = handle;

    return from_backend(st->term, 0, buf, len);
}

static int null_sendbuffer(void *handle) {

    return 0;
}

static void null_size(void *handle, int width, int height) {

}

static void null_special(void *handle, Telnet_Special code) {

}

static const struct telnet_special *null_get_specials (void *handle) {

    return NULL;
}

static Socket null_socket(void *handle) {

    return NULL;
}

static int null_exitcode(void *handle) {

    return 0;
}

static int null_sendok(void *handle) {

    return 1;
}

static void null_unthrottle(void *handle, int backlog) {

}

static int null_ldisc(void *handle, int option) {

    return 0;
}

static void null_provide_ldisc (void *handle, void *ldisc) {

}

static void null_provide_logctx(void *handle, void *logctx) {

}

/*
 * Emacs magic:
 * Local Variables:
 * c-file-style: "simon"
 * End:
 */
