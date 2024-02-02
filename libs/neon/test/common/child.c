/* 
   Framework for testing with a server process
   Copyright (C) 2001-2010, Joe Orton <joe@manyfish.co.uk>

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

#include <sys/wait.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <sys/stat.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <fcntl.h>
#include <netdb.h>
#include <signal.h>

#include "ne_socket.h"
#include "ne_utils.h"
#include "ne_string.h"

#include "tests.h"
#include "child.h"

#ifndef NEON_NO_TEST_CHILD

#ifndef NI_MAXHOST
#define NI_MAXHOST 1025
#endif

static pid_t child = 0;

int clength;

struct server_addr {
    int family;
    union {
	struct sockaddr_in in;
	struct sockaddr_in6 in6;
    } sockaddr;
    char name[NI_MAXHOST];
};

struct server_addr lh;

const char *want_header = NULL;
got_header_fn got_header = NULL;

/* If we have pipe(), then use a pipe between the parent and child to
 * know when the child is ready to accept incoming connections.
 * Otherwise use boring sleep()s trying to avoid the race condition
 * between listen() and connect() in the two processes. */
#ifdef HAVE_PIPE
#define USE_PIPE 1
#endif

int lookup_localhost(void)
{
#ifdef USE_GETADDRINFO
    struct addrinfo hints = {}, *results, *rp;
#endif

    if (lh.family != AF_UNSPEC)
	return OK;

#ifdef USE_GETADDRINFO

#ifdef USE_GAI_ADDRCONFIG
    hints.ai_flags = AI_ADDRCONFIG;
#endif
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    if (getaddrinfo("localhost", NULL, &hints, &results) != 0)
	goto err_use_ipv4;

    for (rp = results; rp != NULL; rp = rp->ai_next) {
	int sock, rv;

	if (rp->ai_family != AF_INET &&
	    rp->ai_family != AF_INET6)
	    continue;

	sock = socket(rp->ai_family, rp->ai_socktype, 0);
	if (sock < 0)
	    continue;
	rv = bind(sock, rp->ai_addr, rp->ai_addrlen);
	close(sock);
	if (rv < 0)
	    continue;

	if (getnameinfo(rp->ai_addr, rp->ai_addrlen,
			lh.name, sizeof lh.name, NULL, 0,
			NI_NUMERICHOST))
	    continue;

	lh.family = rp->ai_family;
	memcpy(&lh.sockaddr, rp->ai_addr, rp->ai_addrlen);
	break;
    }

    freeaddrinfo(results);

err_use_ipv4:

#endif

    if (lh.family == AF_UNSPEC) {
	/* this will break if a system is set up so that `localhost' does
	 * not resolve to 127.0.0.1, but... */
	lh.family = AF_INET;
	strcpy(lh.name, "127.0.0.1");
	lh.sockaddr.in.sin_family = lh.family;
	lh.sockaddr.in.sin_addr.s_addr = inet_addr(lh.name);
    }

    return OK;
}

int
get_lh_family(void)
{
    if (lh.family == AF_UNSPEC)
        lookup_localhost();

    return lh.family;
}

const char *
get_lh_addr(void)
{
    if (lh.family == AF_UNSPEC)
        lookup_localhost();

    return lh.name;
}

ne_inet_addr *
get_lh_inet_addr(void)
{
    ne_iaddr_type type;
    unsigned char *raw;

    if (lh.family == AF_UNSPEC)
        lookup_localhost();

    if (lh.family == AF_INET) {
	type = ne_iaddr_ipv4;
	raw = (unsigned char *) &lh.sockaddr.in.sin_addr.s_addr;
    } else {
	type = ne_iaddr_ipv6;
	raw = lh.sockaddr.in6.sin6_addr.s6_addr;
    }

    return ne_iaddr_make(type, raw);
}

static int do_listen(int port)
{
    int ls = socket(lh.family, SOCK_STREAM, 0);
    struct sockaddr *saddr;
    socklen_t saddrlen;
    int val = 1;

    setsockopt(ls, SOL_SOCKET, SO_REUSEADDR, (void *)&val, sizeof(int));

    if (lh.family == AF_INET) {
	lh.sockaddr.in.sin_port = htons(port);
	saddr = (struct sockaddr *) &lh.sockaddr.in;
	saddrlen = sizeof lh.sockaddr.in;
    } else {
	lh.sockaddr.in6.sin6_port = htons(port);
	saddr = (struct sockaddr *) &lh.sockaddr.in6;
	saddrlen = sizeof lh.sockaddr.in6;
    }

    if (bind(ls, saddr, saddrlen)) {
	printf("bind failed: %s\n", strerror(errno));
	return -1;
    }
    if (listen(ls, 5)) {
	printf("listen failed: %s\n", strerror(errno));
	return -1;
    }

    return ls;
}

void minisleep(void)
{
#ifdef HAVE_USLEEP
    usleep(500);
#else
    sleep(1);
#endif
}

int reset_socket(ne_socket *sock)
{
#ifdef SO_LINGER
    /* Stevens' magic trick to send an RST on close(). */
    struct linger l = {1, 0};
    return setsockopt(ne_sock_fd(sock), SOL_SOCKET, SO_LINGER, &l, sizeof l);
#else
    return 1;
#endif
}

/* close 'sock', performing lingering close to avoid premature RST. */
static int close_socket(ne_socket *sock)
{
    int ret;
    char buf[20];

    ret = ne_sock_shutdown(sock, NE_SOCK_SEND);
    if (ret == 0) {
	NE_DEBUG(NE_DBG_SOCKET, "ssl: Socket cleanly closed.\n");
    }
    else {
	NE_DEBUG(NE_DBG_SOCKET, "sock: Socket closed uncleanly: %s\n",
		 ne_sock_error(sock));
    }

    NE_DEBUG(NE_DBG_SSL, "sock: Lingering close...\n");
    ne_sock_read_timeout(sock, 5);
    while (ne_sock_read(sock, buf, sizeof buf) > 0);

    NE_DEBUG(NE_DBG_SSL, "sock: Closing socket.\n");
    ret = ne_sock_close(sock);
    NE_DEBUG(NE_DBG_SSL, "sock: Socket closed (%d).\n", ret);
    return ret;
}

/* This runs as the child process. */
static int server_child(int readyfd, int port,
			server_fn callback, void *userdata)
{
    ne_socket *s = ne_sock_create();
    int ret, listener;

    in_child();

    listener = do_listen(port);
    if (listener < 0)
	return FAIL;

#ifdef USE_PIPE
    /* Tell the parent we're ready for the request. */
    if (write(readyfd, "a", 1) != 1) abort();
#endif

    ONN("accept failed", ne_sock_accept(s, listener));

    ret = callback(s, userdata);

    close_socket(s);

    return ret;
}

int spawn_server(int port, server_fn fn, void *ud)
{
    int fds[2];

#ifdef USE_PIPE
    if (pipe(fds)) {
	perror("spawn_server: pipe");
	return FAIL;
    }
#else
    /* avoid using uninitialized variable. */
    fds[0] = fds[1] = 0;
#endif
    
    child = fork();

    ONN("fork server", child == -1);

    if (child == 0) {
	/* this is the child. */
	int ret;

	ret = server_child(fds[1], port, fn, ud);

#ifdef USE_PIPE
	close(fds[0]);
	close(fds[1]);
#endif

	/* print the error out otherwise it gets lost. */
	if (ret) {
	    printf("server child failed (%s): %s\n", 
                   tests[test_num].name, test_context);
	}
	
	/* and quit the child. */
        NE_DEBUG(NE_DBG_HTTP, "child exiting with %d\n", ret);
	exit(ret);
    } else {
	char ch;
	
#ifdef USE_PIPE
	if (read(fds[0], &ch, 1) < 0)
	    perror("parent read");

	close(fds[0]);
	close(fds[1]);
#else
	minisleep();
#endif

	return OK;
    }
}

int new_spawn_server(int count, server_fn fn, void *userdata,
                     unsigned int *port)
{
    ne_inet_addr *addr = NULL;
    int ret;

    ret = new_spawn_server2(count, fn, userdata, &addr, port);
    if (addr)
        ne_iaddr_free(addr);

    return ret;
}

int new_spawn_server2(int count, server_fn fn, void *userdata,
                      ne_inet_addr **addr, unsigned int *port)
{
    union {
	struct sockaddr_in  in;
	struct sockaddr_in6 in6;
    } sa;
    socklen_t salen = sizeof sa;
    int ls;

    if (lh.family == AF_UNSPEC)
        lookup_localhost();

    ls = do_listen(0);
    ONN("could not bind/listen fd for server", ls < 0);

    ONV(getsockname(ls, (struct sockaddr *) &sa, &salen) != 0,
        ("could not get socket name for listening fd: %s",
         strerror(errno)));

    if (salen == sizeof sa.in) {
	*port = ntohs(sa.in.sin_port);
	*addr = ne_iaddr_make(ne_iaddr_ipv4,
			      (unsigned char *) &sa.in.sin_addr.s_addr);
    } else {
	*port = ntohs(sa.in6.sin6_port);
	*addr = ne_iaddr_make(ne_iaddr_ipv6, sa.in6.sin6_addr.s6_addr);
    }

    NE_DEBUG(NE_DBG_SOCKET, "child using port %u\n", *port);

    NE_DEBUG(NE_DBG_SOCKET, "child forking now...\n");

    child = fork();
    ONN("failed to fork server", child == -1);

    if (child == 0) {
        int ret, iter = 1;
        
        in_child();

        NE_DEBUG(NE_DBG_SOCKET, ">>> child spawned, port %u, %d iterations.\n",
                 *port, count);

        do {
            ne_socket *sock = ne_sock_create();
            char errbuf[256];            
            int cret;

            NE_DEBUG(NE_DBG_HTTP, "child iteration #%d (of %d), "
                     "awaiting connection...\n", iter, count);

            if (ne_sock_accept(sock, ls)) {
                t_context("Server child could not accept connection: %s", 
                          ne_sock_error(sock));
                exit(FAIL);
            }

            NE_DEBUG(NE_DBG_HTTP, "child got connection, invoking server\n");
            ret = fn(sock, userdata);
            NE_DEBUG(NE_DBG_HTTP, "child iteration #%d returns %d\n",
                     iter, ret);

	    cret = close_socket(sock);
	    NE_DEBUG(NE_DBG_HTTP, "child closed connection, %d: %s.\n", cret,
                     cret ? ne_strerror(cret, errbuf, sizeof errbuf) 
                     : "no error");

        } while (ret == 0 && ++iter <= count);

        NE_DEBUG(NE_DBG_HTTP, "child terminating with %d\n", ret);
        exit(ret);
    }

    close(ls);

    return OK;
}

int dead_server(void)
{
    int status;

    if (waitpid(child, &status, WNOHANG)) {
	/* child quit already! */
	return FAIL;
    }

    NE_DEBUG(NE_DBG_HTTP, "child has not quit.\n");

    return OK;
}

int destroy_and_wait(ne_session *sess)
{
    ne_session_destroy(sess);
    return await_server();
}

int await_server(void)
{
    int status, code;

    (void) wait(&status);
    
    /* so that we aren't reaped by mistake. */
    child = 0;

    if (WIFEXITED(status)) {
        code = WEXITSTATUS(status);
        
        ONV(code,
            ("server process terminated abnormally: %s (%d)",
             code == FAIL ? "FAIL" : "error", code));
    }
    else {
        ONV(WIFSIGNALED(status),
            ("server process terminated by signal %d", WTERMSIG(status)));
    }

    return OK;
}

int reap_server(void)
{
    int status;
    
    if (child != 0) {
	(void) kill(child, SIGTERM);
	minisleep();
	(void) wait(&status);
	child = 0;
    }

    return OK;
}

ssize_t server_send(ne_socket *sock, const char *str, size_t len)
{
    NE_DEBUG(NE_DBG_HTTP, "Sending: %.*s\n", (int)len, str);
    return ne_sock_fullwrite(sock, str, len);
}

int discard_request(ne_socket *sock)
{
    char buffer[1024];
    size_t offset = want_header?strlen(want_header):0;

    clength = 0;

    NE_DEBUG(NE_DBG_HTTP, "Discarding request...\n");
    do {
	ONV(ne_sock_readline(sock, buffer, 1024) < 0,
	    ("error reading line: %s", ne_sock_error(sock)));
	NE_DEBUG(NE_DBG_HTTP, "[req] %s", buffer);
	if (strncasecmp(buffer, "content-length:", 15) == 0) {
	    clength = atoi(buffer + 16);
	}
	if (got_header != NULL && want_header != NULL && 
	    strncasecmp(buffer, want_header, offset) == 0 &&
	    buffer[offset] == ':') {
	    char *value = buffer + offset + 1;
	    if (*value == ' ') value++;
	    got_header(ne_shave(value, "\r\n"));
	}
    } while (strcmp(buffer, "\r\n") != 0);
    
    return OK;
}

int error_response(ne_socket *sock, int ret)
{
    char resp[1024];

    ne_snprintf(resp, sizeof resp,
                "HTTP/1.1 500 Server Test Failed\r\n"
                "X-Neon-Context: %s\r\n"
                "Content-Length: 0\r\n"
                "Connection: close\r\n"
                "\r\n",
                test_context);
    SEND_STRING(sock, resp);

    return ret;
}


int discard_body(ne_socket *sock)
{
    while (clength > 0) {
	char buf[BUFSIZ];
	size_t bytes = clength;
	ssize_t ret;
	if (bytes > sizeof(buf)) bytes = sizeof(buf);
	NE_DEBUG(NE_DBG_HTTP, "Discarding %" NE_FMT_SIZE_T " bytes.\n",
		 bytes);
	ret = ne_sock_read(sock, buf, bytes);
	ONV(ret < 0, ("socket read failed (%" NE_FMT_SSIZE_T "): %s", 
		      ret, ne_sock_error(sock)));
	clength -= ret;
	NE_DEBUG(NE_DBG_HTTP, "Got %" NE_FMT_SSIZE_T " bytes.\n", ret);
    }
    NE_DEBUG(NE_DBG_HTTP, "Discard successful.\n");
    return OK;
}

int serve_file(ne_socket *sock, void *ud)
{
    char buffer[BUFSIZ];
    struct stat st;
    struct serve_file_args *args = ud;
    ssize_t ret;
    int fd;

    CALL(discard_request(sock));

    ne_sock_fullread(sock, buffer, clength);

    fd = open(args->fname, O_RDONLY);
    if (fd < 0) {
	SEND_STRING(sock, 
		    "HTTP/1.0 404 File Not Found\r\n"
		    "Content-Length: 0\r\n\r\n");
	return 0;
    }

    ONN("fstat fd", fstat(fd, &st));

    SEND_STRING(sock, "HTTP/1.0 200 OK\r\n");
    if (args->chunks) {
	sprintf(buffer, "Transfer-Encoding: chunked\r\n");
    } else {
	sprintf(buffer, "Content-Length: %" NE_FMT_OFF_T "\r\n", 
		st.st_size);
    } 
    
    if (args->headers) {
	strcat(buffer, args->headers);
    }

    strcat(buffer, "\r\n");

    SEND_STRING(sock, buffer);

    NE_DEBUG(NE_DBG_HTTP, "Serving %s (%" NE_FMT_OFF_T " bytes).\n",
	     args->fname, st.st_size);

    if (args->chunks) {
	char buf[1024];
	
	while ((ret = read(fd, &buf, args->chunks)) > 0) {
	    /* this is a small integer, cast it explicitly to avoid
	     * warnings with printing an ssize_t. */
	    sprintf(buffer, "%x\r\n", (unsigned int)ret);
	    SEND_STRING(sock, buffer);
	    ONN("writing body", ne_sock_fullwrite(sock, buf, ret));
	    SEND_STRING(sock, "\r\n");
	}
	
	SEND_STRING(sock, "0\r\n\r\n");
	
    } else {
	while ((ret = read(fd, buffer, BUFSIZ)) > 0) {
	    ONN("writing body", ne_sock_fullwrite(sock, buffer, ret));
	}
    }

    ONN("error reading from file", ret < 0);

    (void) close(fd);

    return OK;
}

#endif /* NEON_NO_TEST_CHILD */
