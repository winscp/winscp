/*
 * Windows networking abstraction.
 *
 * Due to this clean abstraction it was possible
 * to easily implement IPv6 support :)
 *
 * IPv6 patch 1 (27 October 2000) Jeroen Massar <jeroen@unfix.org>
 *  - Preliminary hacked IPv6 support.
 *    - Connecting to IPv6 address (eg fec0:4242:4242:100:2d0:b7ff:fe8f:5d42) works.
 *    - Connecting to IPv6 hostname (eg heaven.ipv6.unfix.org) works.
 *  - Compiles as either IPv4 or IPv6.
 *
 * IPv6 patch 2 (29 October 2000) Jeroen Massar <jeroen@unfix.org>
 *  - When compiled as IPv6 it also allows connecting to IPv4 hosts.
 *  - Added some more documentation.
 *
 * IPv6 patch 3 (18 November 2000) Jeroen Massar <jeroen@unfix.org>
 *  - It now supports dynamically loading the IPv6 resolver dll's.
 *    This way we should be able to distribute one (1) binary
 *    which supports both IPv4 and IPv6.
 *  - getaddrinfo() and getnameinfo() are loaded dynamicaly if possible.
 *  - in6addr_any is defined in this file so we don't need to link to wship6.lib
 *  - The patch is now more unified so that we can still
 *    remove all IPv6 support by undef'ing IPV6.
 *    But where it fallsback to IPv4 it uses the IPv4 code which is already in place...
 *  - Canonical name resolving works.
 *
 * IPv6 patch 4 (07 January 2001) Jeroen Massar <jeroen@unfix.org>
 *  - patch against CVS of today, will be submitted to the bugs list
 *    as a 'cvs diff -u' on Simon's request...
 *
 */

/*
 * Define IPV6 to have IPv6 on-the-fly-loading support.
 * This means that one doesn't have to have an IPv6 stack to use it.
 * But if an IPv6 stack is found it is used with a fallback to IPv4.
 */
/* #define IPV6 1 */

#ifdef IPV6
#include <winsock2.h>
#include <ws2tcpip.h>
#include <tpipv6.h>
#else
#include <winsock.h>
#endif
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define DEFINE_PLUG_METHOD_MACROS
#include "putty.h"
#include "network.h"
#include "tree234.h"

struct Socket_tag {
    struct socket_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */
    char *error;
    SOCKET s;
    Plug plug;
    void *private_ptr;
    bufchain output_data;
    int connected;
    int writable;
    int frozen; /* this causes readability notifications to be ignored */
    int frozen_readable; /* this means we missed at least one readability
			  * notification while we were frozen */
    int localhost_only;		       /* for listening sockets */
    char oobdata[1];
    int sending_oob;
    int oobinline;
    int pending_error;		       /* in case send() returns error */
};

/*
 * We used to typedef struct Socket_tag *Socket.
 *
 * Since we have made the networking abstraction slightly more
 * abstract, Socket no longer means a tcp socket (it could mean
 * an ssl socket).  So now we must use Actual_Socket when we know
 * we are talking about a tcp socket.
 */
typedef struct Socket_tag *Actual_Socket;

struct SockAddr_tag {
    char *error;
    /* address family this belongs to, AF_INET for IPv4, AF_INET6 for IPv6. */
    int family;
    unsigned long address;	       /* Address IPv4 style. */
#ifdef IPV6
    struct addrinfo *ai;	       /* Address IPv6 style. */
#endif
};

static tree234 *sktree;

static int cmpfortree(void *av, void *bv)
{
    Actual_Socket a = (Actual_Socket) av, b = (Actual_Socket) bv;
    unsigned long as = (unsigned long) a->s, bs = (unsigned long) b->s;
    if (as < bs)
	return -1;
    if (as > bs)
	return +1;
    return 0;
}

static int cmpforsearch(void *av, void *bv)
{
    Actual_Socket b = (Actual_Socket) bv;
    unsigned long as = (unsigned long) av, bs = (unsigned long) b->s;
    if (as < bs)
	return -1;
    if (as > bs)
	return +1;
    return 0;
}

void sk_init(void)
{
    sktree = newtree234(cmpfortree);
}

void sk_cleanup(void)
{
    Actual_Socket s;
    int i;

    if (sktree) {
	for (i = 0; (s = index234(sktree, i)) != NULL; i++) {
	    closesocket(s->s);
	}
    }
}

char *winsock_error_string(int error)
{
    switch (error) {
      case WSAEACCES:
	return "Network error: Permission denied";
      case WSAEADDRINUSE:
	return "Network error: Address already in use";
      case WSAEADDRNOTAVAIL:
	return "Network error: Cannot assign requested address";
      case WSAEAFNOSUPPORT:
	return
	    "Network error: Address family not supported by protocol family";
      case WSAEALREADY:
	return "Network error: Operation already in progress";
      case WSAECONNABORTED:
	return "Network error: Software caused connection abort";
      case WSAECONNREFUSED:
	return "Network error: Connection refused";
      case WSAECONNRESET:
	return "Network error: Connection reset by peer";
      case WSAEDESTADDRREQ:
	return "Network error: Destination address required";
      case WSAEFAULT:
	return "Network error: Bad address";
      case WSAEHOSTDOWN:
	return "Network error: Host is down";
      case WSAEHOSTUNREACH:
	return "Network error: No route to host";
      case WSAEINPROGRESS:
	return "Network error: Operation now in progress";
      case WSAEINTR:
	return "Network error: Interrupted function call";
      case WSAEINVAL:
	return "Network error: Invalid argument";
      case WSAEISCONN:
	return "Network error: Socket is already connected";
      case WSAEMFILE:
	return "Network error: Too many open files";
      case WSAEMSGSIZE:
	return "Network error: Message too long";
      case WSAENETDOWN:
	return "Network error: Network is down";
      case WSAENETRESET:
	return "Network error: Network dropped connection on reset";
      case WSAENETUNREACH:
	return "Network error: Network is unreachable";
      case WSAENOBUFS:
	return "Network error: No buffer space available";
      case WSAENOPROTOOPT:
	return "Network error: Bad protocol option";
      case WSAENOTCONN:
	return "Network error: Socket is not connected";
      case WSAENOTSOCK:
	return "Network error: Socket operation on non-socket";
      case WSAEOPNOTSUPP:
	return "Network error: Operation not supported";
      case WSAEPFNOSUPPORT:
	return "Network error: Protocol family not supported";
      case WSAEPROCLIM:
	return "Network error: Too many processes";
      case WSAEPROTONOSUPPORT:
	return "Network error: Protocol not supported";
      case WSAEPROTOTYPE:
	return "Network error: Protocol wrong type for socket";
      case WSAESHUTDOWN:
	return "Network error: Cannot send after socket shutdown";
      case WSAESOCKTNOSUPPORT:
	return "Network error: Socket type not supported";
      case WSAETIMEDOUT:
	return "Network error: Connection timed out";
      case WSAEWOULDBLOCK:
	return "Network error: Resource temporarily unavailable";
      case WSAEDISCON:
	return "Network error: Graceful shutdown in progress";
      default:
	return "Unknown network error";
    }
}

SockAddr sk_namelookup(char *host, char **canonicalname)
{
    SockAddr ret = smalloc(sizeof(struct SockAddr_tag));
    unsigned long a;
    struct hostent *h = NULL;
    char realhost[8192];

    /* Clear the structure and default to IPv4. */
    memset(ret, 0, sizeof(struct SockAddr_tag));
    ret->family = 0;		       /* We set this one when we have resolved the host. */
    *realhost = '\0';

    if ((a = inet_addr(host)) == (unsigned long) INADDR_NONE) {
#ifdef IPV6

	/* Try to get the getaddrinfo() function from wship6.dll */
	/* This way one doesn't need to have IPv6 dll's to use PuTTY and
	 * it will fallback to IPv4. */
	typedef int (CALLBACK * FGETADDRINFO) (const char *nodename,
					       const char *servname,
					       const struct addrinfo *
					       hints,
					       struct addrinfo ** res);
	FGETADDRINFO fGetAddrInfo = NULL;

	HINSTANCE dllWSHIP6 = LoadLibrary("wship6.dll");
	if (dllWSHIP6)
	    fGetAddrInfo = (FGETADDRINFO) GetProcAddress(dllWSHIP6,
							 "getaddrinfo");

	/*
	 * Use fGetAddrInfo when it's available (which usually also
	 * means IPv6 is installed...)
	 */
	if (fGetAddrInfo) {
	    /*debug(("Resolving \"%s\" with getaddrinfo()  (IPv4+IPv6 capable)...\n", host)); */
	    if (fGetAddrInfo(host, NULL, NULL, &ret->ai) == 0)
		ret->family = ret->ai->ai_family;
	} else
#endif
	{
	    /*
	     * Otherwise use the IPv4-only gethostbyname...
	     * (NOTE: we don't use gethostbyname as a
	     * fallback!)
	     */
	    if (ret->family == 0) {
		/*debug(("Resolving \"%s\" with gethostbyname() (IPv4 only)...\n", host)); */
		if ( (h = gethostbyname(host)) )
		    ret->family = AF_INET;
	    }
	}
	/*debug(("Done resolving...(family is %d) AF_INET = %d, AF_INET6 = %d\n", ret->family, AF_INET, AF_INET6)); */

	if (ret->family == 0) {
	    DWORD err = WSAGetLastError();
	    ret->error = (err == WSAENETDOWN ? "Network is down" :
			  err ==
			  WSAHOST_NOT_FOUND ? "Host does not exist" : err
			  == WSATRY_AGAIN ? "Host not found" :
#ifdef IPV6
			  fGetAddrInfo ? "getaddrinfo: unknown error" :
#endif
			  "gethostbyname: unknown error");
#ifdef DEBUG
	    {
		LPVOID lpMsgBuf;
		FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
			      FORMAT_MESSAGE_FROM_SYSTEM |
			      FORMAT_MESSAGE_IGNORE_INSERTS, NULL, err,
			      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			      (LPTSTR) & lpMsgBuf, 0, NULL);
		/*debug(("Error %ld: %s (h=%lx)\n", err, lpMsgBuf, h)); */
		/* Free the buffer. */
		LocalFree(lpMsgBuf);
	    }
#endif
	} else {
	    ret->error = NULL;

#ifdef IPV6
	    /* If we got an address info use that... */
	    if (ret->ai) {
		typedef int (CALLBACK * FGETNAMEINFO)
		 (const struct sockaddr FAR * sa, socklen_t salen,
		  char FAR * host, size_t hostlen, char FAR * serv,
		  size_t servlen, int flags);
		FGETNAMEINFO fGetNameInfo = NULL;

		/* Are we in IPv4 fallback mode? */
		/* We put the IPv4 address into the a variable so we can further-on use the IPv4 code... */
		if (ret->family == AF_INET)
		    memcpy(&a,
			   (char *) &((SOCKADDR_IN *) ret->ai->
				      ai_addr)->sin_addr, sizeof(a));

		/* Now let's find that canonicalname... */
		if ((dllWSHIP6)
		    && (fGetNameInfo =
			(FGETNAMEINFO) GetProcAddress(dllWSHIP6,
						      "getnameinfo"))) {
		    if (fGetNameInfo
			((struct sockaddr *) ret->ai->ai_addr,
			 ret->family ==
			 AF_INET ? sizeof(SOCKADDR_IN) :
			 sizeof(SOCKADDR_IN6), realhost,
			 sizeof(realhost), NULL, 0, 0) != 0) {
			strncpy(realhost, host, sizeof(realhost));
		    }
		}
	    }
	    /* We used the IPv4-only gethostbyname()... */
	    else
#endif
	    {
		memcpy(&a, h->h_addr, sizeof(a));
		/* This way we are always sure the h->h_name is valid :) */
		strncpy(realhost, h->h_name, sizeof(realhost));
	    }
	}
#ifdef IPV6
	FreeLibrary(dllWSHIP6);
#endif
    } else {
	/*
	 * This must be a numeric IPv4 address because it caused a
	 * success return from inet_addr.
	 */
	ret->family = AF_INET;
	strncpy(realhost, host, sizeof(realhost));
    }
    ret->address = ntohl(a);
    realhost[lenof(realhost)-1] = '\0';
    *canonicalname = smalloc(1+strlen(realhost));
    strcpy(*canonicalname, realhost);
    return ret;
}

void sk_getaddr(SockAddr addr, char *buf, int buflen)
{
#ifdef IPV6
    if (addr->family == AF_INET) {
#endif
	struct in_addr a;
	a.s_addr = htonl(addr->address);
	strncpy(buf, inet_ntoa(a), buflen);
#ifdef IPV6
    } else {
	FIXME; /* I don't know how to get a text form of an IPv6 address. */
    }
#endif
}

int sk_addrtype(SockAddr addr)
{
    return addr->family;
}

void sk_addrcopy(SockAddr addr, char *buf)
{
#ifdef IPV6
    if (addr->family == AF_INET) {
#endif
	struct in_addr a;
	a.s_addr = htonl(addr->address);
	memcpy(buf, (char*) &a.s_addr, 4);
#ifdef IPV6
    } else {
	memcpy(buf, (char*) addr->ai, 16);
    }
#endif
}

void sk_addr_free(SockAddr addr)
{
    sfree(addr);
}

static Plug sk_tcp_plug(Socket sock, Plug p)
{
    Actual_Socket s = (Actual_Socket) sock;
    Plug ret = s->plug;
    if (p)
	s->plug = p;
    return ret;
}

static void sk_tcp_flush(Socket s)
{
    /*
     * We send data to the socket as soon as we can anyway,
     * so we don't need to do anything here.  :-)
     */
}

static void sk_tcp_close(Socket s);
static int sk_tcp_write(Socket s, char *data, int len);
static int sk_tcp_write_oob(Socket s, char *data, int len);
static void sk_tcp_set_private_ptr(Socket s, void *ptr);
static void *sk_tcp_get_private_ptr(Socket s);
static void sk_tcp_set_frozen(Socket s, int is_frozen);
static char *sk_tcp_socket_error(Socket s);

extern char *do_select(SOCKET skt, int startup);

Socket sk_register(void *sock, Plug plug)
{
    static struct socket_function_table fn_table = {
	sk_tcp_plug,
	sk_tcp_close,
	sk_tcp_write,
	sk_tcp_write_oob,
	sk_tcp_flush,
	sk_tcp_set_private_ptr,
	sk_tcp_get_private_ptr,
	sk_tcp_set_frozen,
	sk_tcp_socket_error
    };

    DWORD err;
    char *errstr;
    Actual_Socket ret;

    /*
     * Create Socket structure.
     */
    ret = smalloc(sizeof(struct Socket_tag));
    ret->fn = &fn_table;
    ret->error = NULL;
    ret->plug = plug;
    bufchain_init(&ret->output_data);
    ret->writable = 1;		       /* to start with */
    ret->sending_oob = 0;
    ret->frozen = 1;
    ret->frozen_readable = 0;
    ret->localhost_only = 0;	       /* unused, but best init anyway */
    ret->pending_error = 0;

    ret->s = (SOCKET)sock;

    if (ret->s == INVALID_SOCKET) {
	err = WSAGetLastError();
	ret->error = winsock_error_string(err);
	return (Socket) ret;
    }

    ret->oobinline = 0;

    /* Set up a select mechanism. This could be an AsyncSelect on a
     * window, or an EventSelect on an event object. */
    errstr = do_select(ret->s, 1);
    if (errstr) {
	ret->error = errstr;
	return (Socket) ret;
    }

    add234(sktree, ret);

    return (Socket) ret;
}

Socket sk_new(SockAddr addr, int port, int privport, int oobinline,
	      int nodelay, Plug plug)
{
    static struct socket_function_table fn_table = {
	sk_tcp_plug,
	sk_tcp_close,
	sk_tcp_write,
	sk_tcp_write_oob,
	sk_tcp_flush,
	sk_tcp_set_private_ptr,
	sk_tcp_get_private_ptr,
	sk_tcp_set_frozen,
	sk_tcp_socket_error
    };

    SOCKET s;
#ifdef IPV6
    SOCKADDR_IN6 a6;
#endif
    SOCKADDR_IN a;
    DWORD err;
    char *errstr;
    Actual_Socket ret;
    short localport;

    /*
     * Create Socket structure.
     */
    ret = smalloc(sizeof(struct Socket_tag));
    ret->fn = &fn_table;
    ret->error = NULL;
    ret->plug = plug;
    bufchain_init(&ret->output_data);
    ret->connected = 0;		       /* to start with */
    ret->writable = 0;		       /* to start with */
    ret->sending_oob = 0;
    ret->frozen = 0;
    ret->frozen_readable = 0;
    ret->localhost_only = 0;	       /* unused, but best init anyway */
    ret->pending_error = 0;

    /*
     * Open socket.
     */
    s = socket(addr->family, SOCK_STREAM, 0);
    ret->s = s;

    if (s == INVALID_SOCKET) {
	err = WSAGetLastError();
	ret->error = winsock_error_string(err);
	return (Socket) ret;
    }

    ret->oobinline = oobinline;
    if (oobinline) {
	BOOL b = TRUE;
	setsockopt(s, SOL_SOCKET, SO_OOBINLINE, (void *) &b, sizeof(b));
    }

    if (nodelay) {
	BOOL b = TRUE;
	setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (void *) &b, sizeof(b));
    }

    /*
     * Bind to local address.
     */
    if (privport)
	localport = 1023;	       /* count from 1023 downwards */
    else
	localport = 0;		       /* just use port 0 (ie winsock picks) */

    /* Loop round trying to bind */
    while (1) {
	int retcode;

#ifdef IPV6
	if (addr->family == AF_INET6) {
	    memset(&a6, 0, sizeof(a6));
	    a6.sin6_family = AF_INET6;
/*a6.sin6_addr      = in6addr_any; *//* == 0 */
	    a6.sin6_port = htons(localport);
	} else
#endif
	{
	    a.sin_family = AF_INET;
	    a.sin_addr.s_addr = htonl(INADDR_ANY);
	    a.sin_port = htons(localport);
	}
#ifdef IPV6
	retcode = bind(s, (addr->family == AF_INET6 ?
			   (struct sockaddr *) &a6 :
			   (struct sockaddr *) &a),
		       (addr->family ==
			AF_INET6 ? sizeof(a6) : sizeof(a)));
#else
	retcode = bind(s, (struct sockaddr *) &a, sizeof(a));
#endif
	if (retcode != SOCKET_ERROR) {
	    err = 0;
	    break;		       /* done */
	} else {
	    err = WSAGetLastError();
	    if (err != WSAEADDRINUSE)  /* failed, for a bad reason */
		break;
	}

	if (localport == 0)
	    break;		       /* we're only looping once */
	localport--;
	if (localport == 0)
	    break;		       /* we might have got to the end */
    }

    if (err) {
	ret->error = winsock_error_string(err);
	return (Socket) ret;
    }

    /*
     * Connect to remote address.
     */
#ifdef IPV6
    if (addr->family == AF_INET6) {
	memset(&a, 0, sizeof(a));
	a6.sin6_family = AF_INET6;
	a6.sin6_port = htons((short) port);
	a6.sin6_addr =
	    ((struct sockaddr_in6 *) addr->ai->ai_addr)->sin6_addr;
    } else
#endif
    {
	a.sin_family = AF_INET;
	a.sin_addr.s_addr = htonl(addr->address);
	a.sin_port = htons((short) port);
    }

    /* Set up a select mechanism. This could be an AsyncSelect on a
     * window, or an EventSelect on an event object. */
    errstr = do_select(s, 1);
    if (errstr) {
	ret->error = errstr;
	return (Socket) ret;
    }

    if ((
#ifdef IPV6
	    connect(s, ((addr->family == AF_INET6) ?
			(struct sockaddr *) &a6 : (struct sockaddr *) &a),
		    (addr->family == AF_INET6) ? sizeof(a6) : sizeof(a))
#else
	    connect(s, (struct sockaddr *) &a, sizeof(a))
#endif
	) == SOCKET_ERROR) {
	err = WSAGetLastError();
	/*
	 * We expect a potential EWOULDBLOCK here, because the
	 * chances are the front end has done a select for
	 * FD_CONNECT, so that connect() will complete
	 * asynchronously.
	 */
	if ( err != WSAEWOULDBLOCK ) {
	    ret->error = winsock_error_string(err);
	    return (Socket) ret;
	}
    } else {
	/*
	 * If we _don't_ get EWOULDBLOCK, the connect has completed
	 * and we should set the socket as writable.
	 */
	ret->writable = 1;
    }

    add234(sktree, ret);

    return (Socket) ret;
}

Socket sk_newlistener(int port, Plug plug, int local_host_only)
{
    static struct socket_function_table fn_table = {
	sk_tcp_plug,
	sk_tcp_close,
	sk_tcp_write,
	sk_tcp_write_oob,
	sk_tcp_flush,
	sk_tcp_set_private_ptr,
	sk_tcp_get_private_ptr,
	sk_tcp_set_frozen,
	sk_tcp_socket_error
    };

    SOCKET s;
#ifdef IPV6
    SOCKADDR_IN6 a6;
#endif
    SOCKADDR_IN a;
    DWORD err;
    char *errstr;
    Actual_Socket ret;
    int retcode;
    int on = 1;

    /*
     * Create Socket structure.
     */
    ret = smalloc(sizeof(struct Socket_tag));
    ret->fn = &fn_table;
    ret->error = NULL;
    ret->plug = plug;
    bufchain_init(&ret->output_data);
    ret->writable = 0;		       /* to start with */
    ret->sending_oob = 0;
    ret->frozen = 0;
    ret->frozen_readable = 0;
    ret->localhost_only = local_host_only;
    ret->pending_error = 0;

    /*
     * Open socket.
     */
    s = socket(AF_INET, SOCK_STREAM, 0);
    ret->s = s;

    if (s == INVALID_SOCKET) {
	err = WSAGetLastError();
	ret->error = winsock_error_string(err);
	return (Socket) ret;
    }

    ret->oobinline = 0;


    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (const char *)&on, sizeof(on));


#ifdef IPV6
	if (addr->family == AF_INET6) {
	    memset(&a6, 0, sizeof(a6));
	    a6.sin6_family = AF_INET6;
	    if (local_host_only)
		a6.sin6_addr = in6addr_loopback;
	    else
		a6.sin6_addr = in6addr_any;
	    a6.sin6_port = htons(port);
	} else
#endif
	{
	    a.sin_family = AF_INET;
	    if (local_host_only)
		a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	    else
		a.sin_addr.s_addr = htonl(INADDR_ANY);
	    a.sin_port = htons((short)port);
	}
#ifdef IPV6
	retcode = bind(s, (addr->family == AF_INET6 ?
			   (struct sockaddr *) &a6 :
			   (struct sockaddr *) &a),
		       (addr->family ==
			AF_INET6 ? sizeof(a6) : sizeof(a)));
#else
	retcode = bind(s, (struct sockaddr *) &a, sizeof(a));
#endif
	if (retcode != SOCKET_ERROR) {
	    err = 0;
	} else {
	    err = WSAGetLastError();
	}

    if (err) {
	ret->error = winsock_error_string(err);
	return (Socket) ret;
    }


    if (listen(s, SOMAXCONN) == SOCKET_ERROR) {
        closesocket(s);
	ret->error = winsock_error_string(err);
	return (Socket) ret;
    }

    /* Set up a select mechanism. This could be an AsyncSelect on a
     * window, or an EventSelect on an event object. */
    errstr = do_select(s, 1);
    if (errstr) {
	ret->error = errstr;
	return (Socket) ret;
    }

    add234(sktree, ret);

    return (Socket) ret;
}

static void sk_tcp_close(Socket sock)
{
    extern char *do_select(SOCKET skt, int startup);
    Actual_Socket s = (Actual_Socket) sock;

    del234(sktree, s);
    do_select(s->s, 0);
    closesocket(s->s);
    sfree(s);
}

/*
 * The function which tries to send on a socket once it's deemed
 * writable.
 */
void try_send(Actual_Socket s)
{
    while (s->sending_oob || bufchain_size(&s->output_data) > 0) {
	int nsent;
	DWORD err;
	void *data;
	int len, urgentflag;

	if (s->sending_oob) {
	    urgentflag = MSG_OOB;
	    len = s->sending_oob;
	    data = &s->oobdata;
	} else {
	    urgentflag = 0;
	    bufchain_prefix(&s->output_data, &data, &len);
	}
	nsent = send(s->s, data, len, urgentflag);
	noise_ultralight(nsent);
	if (nsent <= 0) {
	    err = (nsent < 0 ? WSAGetLastError() : 0);
	    if ((err < WSABASEERR && nsent < 0) || err == WSAEWOULDBLOCK) {
		/*
		 * Perfectly normal: we've sent all we can for the moment.
		 * 
		 * (Some WinSock send() implementations can return
		 * <0 but leave no sensible error indication -
		 * WSAGetLastError() is called but returns zero or
		 * a small number - so we check that case and treat
		 * it just like WSAEWOULDBLOCK.)
		 */
		s->writable = FALSE;
		return;
	    } else if (nsent == 0 ||
		       err == WSAECONNABORTED || err == WSAECONNRESET) {
		/*
		 * If send() returns CONNABORTED or CONNRESET, we
		 * unfortunately can't just call plug_closing(),
		 * because it's quite likely that we're currently
		 * _in_ a call from the code we'd be calling back
		 * to, so we'd have to make half the SSH code
		 * reentrant. Instead we flag a pending error on
		 * the socket, to be dealt with (by calling
		 * plug_closing()) at some suitable future moment.
		 */
		s->pending_error = err;
		return;
	    } else {
		logevent(winsock_error_string(err));
		fatalbox("%s", winsock_error_string(err));
	    }
	} else {
	    if (s->sending_oob) {
		if (nsent < len) {
		    memmove(s->oobdata, s->oobdata+nsent, len-nsent);
		    s->sending_oob = len - nsent;
		} else {
		    s->sending_oob = 0;
		}
	    } else {
		bufchain_consume(&s->output_data, nsent);
	    }
	}
    }
}

static int sk_tcp_write(Socket sock, char *buf, int len)
{
    Actual_Socket s = (Actual_Socket) sock;

    /*
     * Add the data to the buffer list on the socket.
     */
    bufchain_add(&s->output_data, buf, len);

    /*
     * Now try sending from the start of the buffer list.
     */
    if (s->writable)
	try_send(s);

    return bufchain_size(&s->output_data);
}

static int sk_tcp_write_oob(Socket sock, char *buf, int len)
{
    Actual_Socket s = (Actual_Socket) sock;

    /*
     * Replace the buffer list on the socket with the data.
     */
    bufchain_clear(&s->output_data);
    assert(len <= sizeof(s->oobdata));
    memcpy(s->oobdata, buf, len);
    s->sending_oob = len;

    /*
     * Now try sending from the start of the buffer list.
     */
    if (s->writable)
	try_send(s);

    return s->sending_oob;
}

int select_result(WPARAM wParam, LPARAM lParam)
{
    int ret, open;
    DWORD err;
    char buf[20480];		       /* nice big buffer for plenty of speed */
    Actual_Socket s;
    u_long atmark;

    /* wParam is the socket itself */
    s = find234(sktree, (void *) wParam, cmpforsearch);
    if (!s)
	return 1;		       /* boggle */

    if ((err = WSAGETSELECTERROR(lParam)) != 0) {
	/*
	 * An error has occurred on this socket. Pass it to the
	 * plug.
	 */
	return plug_closing(s->plug, winsock_error_string(err), err, 0);
    }

    noise_ultralight(lParam);

    switch (WSAGETSELECTEVENT(lParam)) {
      case FD_CONNECT:
	s->connected = s->writable = 1;
	break;
      case FD_READ:
	/* In the case the socket is still frozen, we don't even bother */
	if (s->frozen) {
	    s->frozen_readable = 1;
	    break;
	}

	/*
	 * We have received data on the socket. For an oobinline
	 * socket, this might be data _before_ an urgent pointer,
	 * in which case we send it to the back end with type==1
	 * (data prior to urgent).
	 */
	if (s->oobinline) {
	    atmark = 1;
	    ioctlsocket(s->s, SIOCATMARK, &atmark);
	    /*
	     * Avoid checking the return value from ioctlsocket(),
	     * on the grounds that some WinSock wrappers don't
	     * support it. If it does nothing, we get atmark==1,
	     * which is equivalent to `no OOB pending', so the
	     * effect will be to non-OOB-ify any OOB data.
	     */
	} else
	    atmark = 1;

	ret = recv(s->s, buf, sizeof(buf), 0);
	noise_ultralight(ret);
	if (ret < 0) {
	    err = WSAGetLastError();
	    if (err == WSAEWOULDBLOCK) {
		break;
	    }
	}
	if (ret < 0) {
	    return plug_closing(s->plug, winsock_error_string(err), err,
				0);
	} else if (0 == ret) {
	    return plug_closing(s->plug, NULL, 0, 0);
	} else {
	    return plug_receive(s->plug, atmark ? 0 : 1, buf, ret);
	}
	break;
      case FD_OOB:
	/*
	 * This will only happen on a non-oobinline socket. It
	 * indicates that we can immediately perform an OOB read
	 * and get back OOB data, which we will send to the back
	 * end with type==2 (urgent data).
	 */
	ret = recv(s->s, buf, sizeof(buf), MSG_OOB);
	noise_ultralight(ret);
	if (ret <= 0) {
	    char *str = (ret == 0 ? "Internal networking trouble" :
			 winsock_error_string(WSAGetLastError()));
	    logevent(str);
	    fatalbox("%s", str);
	} else {
	    return plug_receive(s->plug, 2, buf, ret);
	}
	break;
      case FD_WRITE:
	{
	    int bufsize_before, bufsize_after;
	    s->writable = 1;
	    bufsize_before = s->sending_oob + bufchain_size(&s->output_data);
	    try_send(s);
	    bufsize_after = s->sending_oob + bufchain_size(&s->output_data);
	    if (bufsize_after < bufsize_before)
		plug_sent(s->plug, bufsize_after);
	}
	break;
      case FD_CLOSE:
	/* Signal a close on the socket. First read any outstanding data. */
	open = 1;
	do {
	    ret = recv(s->s, buf, sizeof(buf), 0);
	    if (ret < 0) {
		err = WSAGetLastError();
		if (err == WSAEWOULDBLOCK)
		    break;
		return plug_closing(s->plug, winsock_error_string(err),
				    err, 0);
	    } else {
		if (ret)
		    open &= plug_receive(s->plug, 0, buf, ret);
		else
		    open &= plug_closing(s->plug, NULL, 0, 0);
	    }
	} while (ret > 0);
	return open;
       case FD_ACCEPT:
	{
	    struct sockaddr_in isa;
	    int addrlen = sizeof(struct sockaddr_in);
	    SOCKET t;  /* socket of connection */

	    memset(&isa, 0, sizeof(struct sockaddr_in));
	    err = 0;
	    t = accept(s->s,(struct sockaddr *)&isa,&addrlen);
	    if (t == INVALID_SOCKET)
	    {
		err = WSAGetLastError();
		if (err == WSATRY_AGAIN)
		    break;
	    }

	    if (s->localhost_only &&
		ntohl(isa.sin_addr.s_addr) != INADDR_LOOPBACK) {
		closesocket(t);	       /* dodgy WinSock let nonlocal through */
	    } else if (plug_accepting(s->plug, (void*)t)) {
		closesocket(t);	       /* denied or error */
	    }
	}
    }

    return 1;
}

/*
 * Deal with socket errors detected in try_send().
 */
void net_pending_errors(void)
{
    int i;
    Actual_Socket s;

    /*
     * This might be a fiddly business, because it's just possible
     * that handling a pending error on one socket might cause
     * others to be closed. (I can't think of any reason this might
     * happen in current SSH implementation, but to maintain
     * generality of this network layer I'll assume the worst.)
     * 
     * So what we'll do is search the socket list for _one_ socket
     * with a pending error, and then handle it, and then search
     * the list again _from the beginning_. Repeat until we make a
     * pass with no socket errors present. That way we are
     * protected against the socket list changing under our feet.
     */

    do {
	for (i = 0; (s = index234(sktree, i)) != NULL; i++) {
	    if (s->pending_error) {
		/*
		 * An error has occurred on this socket. Pass it to the
		 * plug.
		 */
		plug_closing(s->plug,
			     winsock_error_string(s->pending_error),
			     s->pending_error, 0);
		break;
	    }
	}
    } while (s);
}

/*
 * Each socket abstraction contains a `void *' private field in
 * which the client can keep state.
 */
static void sk_tcp_set_private_ptr(Socket sock, void *ptr)
{
    Actual_Socket s = (Actual_Socket) sock;
    s->private_ptr = ptr;
}

static void *sk_tcp_get_private_ptr(Socket sock)
{
    Actual_Socket s = (Actual_Socket) sock;
    return s->private_ptr;
}

/*
 * Special error values are returned from sk_namelookup and sk_new
 * if there's a problem. These functions extract an error message,
 * or return NULL if there's no problem.
 */
char *sk_addr_error(SockAddr addr)
{
    return addr->error;
}
static char *sk_tcp_socket_error(Socket sock)
{
    Actual_Socket s = (Actual_Socket) sock;
    return s->error;
}

static void sk_tcp_set_frozen(Socket sock, int is_frozen)
{
    Actual_Socket s = (Actual_Socket) sock;
    if (s->frozen == is_frozen)
	return;
    s->frozen = is_frozen;
    if (!is_frozen && s->frozen_readable) {
	char c;
	recv(s->s, &c, 1, MSG_PEEK);
    }
    s->frozen_readable = 0;
}

/*
 * For Plink: enumerate all sockets currently active.
 */
SOCKET first_socket(int *state)
{
    Actual_Socket s;
    *state = 0;
    s = index234(sktree, (*state)++);
    return s ? s->s : INVALID_SOCKET;
}

SOCKET next_socket(int *state)
{
    Actual_Socket s = index234(sktree, (*state)++);
    return s ? s->s : INVALID_SOCKET;
}
