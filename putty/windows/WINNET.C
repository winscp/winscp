/*
 * Windows networking abstraction.
 *
 * For the IPv6 code in here I am indebted to Jeroen Massar and
 * unfix.org.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define DEFINE_PLUG_METHOD_MACROS
#include "putty.h"
#include "network.h"
#include "tree234.h"

#include <ws2tcpip.h>

#ifndef NO_IPV6
const struct in6_addr in6addr_any = IN6ADDR_ANY_INIT;
const struct in6_addr in6addr_loopback = IN6ADDR_LOOPBACK_INIT;
#endif

#define ipv4_is_loopback(addr) \
	((p_ntohl(addr.s_addr) & 0xFF000000L) == 0x7F000000L)

/*
 * We used to typedef struct Socket_tag *Socket.
 *
 * Since we have made the networking abstraction slightly more
 * abstract, Socket no longer means a tcp socket (it could mean
 * an ssl socket).  So now we must use Actual_Socket when we know
 * we are talking about a tcp socket.
 */
typedef struct Socket_tag *Actual_Socket;

struct Socket_tag {
    const struct socket_function_table *fn;
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
    int oobinline, nodelay, keepalive, privport;
    SockAddr addr;
    int port;
    int pending_error;		       /* in case send() returns error */
    /*
     * We sometimes need pairs of Socket structures to be linked:
     * if we are listening on the same IPv6 and v4 port, for
     * example. So here we define `parent' and `child' pointers to
     * track this link.
     */
    Actual_Socket parent, child;
};

struct SockAddr_tag {
    char *error;
    /* 
     * Which address family this address belongs to. AF_INET for
     * IPv4; AF_INET6 for IPv6; AF_UNSPEC indicates that name
     * resolution has not been done and a simple host name is held
     * in this SockAddr structure.
     * The hostname field is also used when the hostname has both
     * an IPv6 and IPv4 address and the IPv6 connection attempt
     * fails. We then try the IPv4 address.
     * This 'family' should become an option in the GUI and
     * on the commandline for selecting a default protocol.
     */
    int family;
#ifndef NO_IPV6
    struct addrinfo *ais;	       /* Addresses IPv6 style. */
    struct addrinfo *ai;	       /* steps along the linked list */
#endif
    unsigned long *addresses;	       /* Addresses IPv4 style. */
    int naddresses, curraddr;
    char hostname[512];		       /* Store an unresolved host name. */
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

#define NOTHING
#define DECL_WINSOCK_FUNCTION(linkage, rettype, name, params) \
    typedef rettype (WINAPI *t_##name) params; \
    linkage t_##name p_##name
#define GET_WINSOCK_FUNCTION(module, name) \
    p_##name = (t_##name) GetProcAddress(module, #name)

DECL_WINSOCK_FUNCTION(NOTHING, int, WSAAsyncSelect,
		      (SOCKET, HWND, u_int, long));
DECL_WINSOCK_FUNCTION(NOTHING, int, WSAEventSelect, (SOCKET, WSAEVENT, long));
DECL_WINSOCK_FUNCTION(NOTHING, int, select,
		      (int, fd_set FAR *, fd_set FAR *,
		       fd_set FAR *, const struct timeval FAR *));
DECL_WINSOCK_FUNCTION(NOTHING, int, WSAGetLastError, (void));
DECL_WINSOCK_FUNCTION(NOTHING, int, WSAEnumNetworkEvents,
		      (SOCKET, WSAEVENT, LPWSANETWORKEVENTS));
DECL_WINSOCK_FUNCTION(static, int, WSAStartup, (WORD, LPWSADATA));
DECL_WINSOCK_FUNCTION(static, int, WSACleanup, (void));
DECL_WINSOCK_FUNCTION(static, int, closesocket, (SOCKET));
DECL_WINSOCK_FUNCTION(static, u_long, ntohl, (u_long));
DECL_WINSOCK_FUNCTION(static, u_long, htonl, (u_long));
DECL_WINSOCK_FUNCTION(static, u_short, htons, (u_short));
DECL_WINSOCK_FUNCTION(static, u_short, ntohs, (u_short));
DECL_WINSOCK_FUNCTION(static, struct hostent FAR *, gethostbyname,
		      (const char FAR *));
DECL_WINSOCK_FUNCTION(static, struct servent FAR *, getservbyname,
		      (const char FAR *, const char FAR *));
DECL_WINSOCK_FUNCTION(static, unsigned long, inet_addr, (const char FAR *));
DECL_WINSOCK_FUNCTION(static, char FAR *, inet_ntoa, (struct in_addr));
DECL_WINSOCK_FUNCTION(static, int, connect,
		      (SOCKET, const struct sockaddr FAR *, int));
DECL_WINSOCK_FUNCTION(static, int, bind,
		      (SOCKET, const struct sockaddr FAR *, int));
DECL_WINSOCK_FUNCTION(static, int, setsockopt,
		      (SOCKET, int, int, const char FAR *, int));
DECL_WINSOCK_FUNCTION(static, SOCKET, socket, (int, int, int));
DECL_WINSOCK_FUNCTION(static, int, listen, (SOCKET, int));
DECL_WINSOCK_FUNCTION(static, int, send, (SOCKET, const char FAR *, int, int));
DECL_WINSOCK_FUNCTION(static, int, ioctlsocket,
		      (SOCKET, long, u_long FAR *));
DECL_WINSOCK_FUNCTION(static, SOCKET, accept,
		      (SOCKET, struct sockaddr FAR *, int FAR *));
DECL_WINSOCK_FUNCTION(static, int, recv, (SOCKET, char FAR *, int, int));
DECL_WINSOCK_FUNCTION(static, int, WSAIoctl,
		      (SOCKET, DWORD, LPVOID, DWORD, LPVOID, DWORD,
		       LPDWORD, LPWSAOVERLAPPED,
		       LPWSAOVERLAPPED_COMPLETION_ROUTINE));
#ifndef NO_IPV6
DECL_WINSOCK_FUNCTION(static, int, getaddrinfo,
		      (const char *nodename, const char *servname,
		       const struct addrinfo *hints, struct addrinfo **res));
DECL_WINSOCK_FUNCTION(static, void, freeaddrinfo, (struct addrinfo *res));
DECL_WINSOCK_FUNCTION(static, int, getnameinfo,
		      (const struct sockaddr FAR * sa, socklen_t salen,
		       char FAR * host, size_t hostlen, char FAR * serv,
		       size_t servlen, int flags));
#endif

static HMODULE winsock_module;
#ifndef NO_IPV6
static HMODULE wship6_module;
#endif

void sk_init(void)
{
    WORD winsock_ver;
    WSADATA wsadata;

    winsock_ver = MAKEWORD(2, 0);
    winsock_module = LoadLibrary("WS2_32.DLL");
    if (!winsock_module) {
	winsock_module = LoadLibrary("WSOCK32.DLL");
	winsock_ver = MAKEWORD(1, 1);
    }
    if (!winsock_module)
	fatalbox("Unable to load any WinSock library");

#ifndef NO_IPV6
    wship6_module = LoadLibrary("wship6.dll");
    if (wship6_module) {
	GET_WINSOCK_FUNCTION(wship6_module, getaddrinfo);
	GET_WINSOCK_FUNCTION(wship6_module, freeaddrinfo);
	GET_WINSOCK_FUNCTION(wship6_module, getnameinfo);
    }
#endif

    GET_WINSOCK_FUNCTION(winsock_module, WSAAsyncSelect);
    GET_WINSOCK_FUNCTION(winsock_module, WSAEventSelect);
    GET_WINSOCK_FUNCTION(winsock_module, select);
    GET_WINSOCK_FUNCTION(winsock_module, WSAGetLastError);
    GET_WINSOCK_FUNCTION(winsock_module, WSAEnumNetworkEvents);
    GET_WINSOCK_FUNCTION(winsock_module, WSAStartup);
    GET_WINSOCK_FUNCTION(winsock_module, WSACleanup);
    GET_WINSOCK_FUNCTION(winsock_module, closesocket);
    GET_WINSOCK_FUNCTION(winsock_module, ntohl);
    GET_WINSOCK_FUNCTION(winsock_module, htonl);
    GET_WINSOCK_FUNCTION(winsock_module, htons);
    GET_WINSOCK_FUNCTION(winsock_module, ntohs);
    GET_WINSOCK_FUNCTION(winsock_module, gethostbyname);
    GET_WINSOCK_FUNCTION(winsock_module, getservbyname);
    GET_WINSOCK_FUNCTION(winsock_module, inet_addr);
    GET_WINSOCK_FUNCTION(winsock_module, inet_ntoa);
    GET_WINSOCK_FUNCTION(winsock_module, connect);
    GET_WINSOCK_FUNCTION(winsock_module, bind);
    GET_WINSOCK_FUNCTION(winsock_module, setsockopt);
    GET_WINSOCK_FUNCTION(winsock_module, socket);
    GET_WINSOCK_FUNCTION(winsock_module, listen);
    GET_WINSOCK_FUNCTION(winsock_module, send);
    GET_WINSOCK_FUNCTION(winsock_module, ioctlsocket);
    GET_WINSOCK_FUNCTION(winsock_module, accept);
    GET_WINSOCK_FUNCTION(winsock_module, recv);
    GET_WINSOCK_FUNCTION(winsock_module, WSAIoctl);

    if (p_WSAStartup(winsock_ver, &wsadata)) {
	fatalbox("Unable to initialise WinSock");
    }
    if (LOBYTE(wsadata.wVersion) != LOBYTE(winsock_ver)) {
        p_WSACleanup();
	fatalbox("WinSock version is incompatible with %d.%d",
		 LOBYTE(winsock_ver), HIBYTE(winsock_ver));
    }

    sktree = newtree234(cmpfortree);
}

void sk_cleanup(void)
{
    Actual_Socket s;
    int i;

    if (sktree) {
	for (i = 0; (s = index234(sktree, i)) != NULL; i++) {
	    p_closesocket(s->s);
	}
	freetree234(sktree);
	sktree = NULL;
    }

    p_WSACleanup();
    if (winsock_module)
	FreeLibrary(winsock_module);
#ifndef NO_IPV6
    if (wship6_module)
	FreeLibrary(wship6_module);
#endif
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

SockAddr sk_namelookup(const char *host, char **canonicalname,
		       int address_family)
{
    SockAddr ret = snew(struct SockAddr_tag);
    unsigned long a;
    struct hostent *h = NULL;
    char realhost[8192];
    int ret_family;
    int err;

    /* Clear the structure and default to IPv4. */
    memset(ret, 0, sizeof(struct SockAddr_tag));
    ret->family = (address_family == ADDRTYPE_IPV4 ? AF_INET :
#ifndef NO_IPV6
		   address_family == ADDRTYPE_IPV6 ? AF_INET6 :
#endif
		   AF_UNSPEC);
#ifndef NO_IPV6
    ret->ai = ret->ais = NULL;
#endif
    ret_family = AF_UNSPEC;
    *realhost = '\0';

    if ((a = p_inet_addr(host)) == (unsigned long) INADDR_NONE) {
#ifndef NO_IPV6
	/*
	 * Use getaddrinfo when it's available
	 */
	if (p_getaddrinfo) {
	    struct addrinfo hints;
	    memset(&hints, 0, sizeof(hints));
	    hints.ai_family = ret->family;
	    if ((err = p_getaddrinfo(host, NULL, &hints, &ret->ais)) == 0)
		ret_family = ret->ais->ai_family;
	    ret->ai = ret->ais;
	} else
#endif
	{
	    /*
	     * Otherwise use the IPv4-only gethostbyname...
	     * (NOTE: we don't use gethostbyname as a fallback!)
	     */
	    if ( (h = p_gethostbyname(host)) )
		ret_family = AF_INET;
	    else
		err = p_WSAGetLastError();
	}

	if (ret_family == AF_UNSPEC) {
	    ret->error = (err == WSAENETDOWN ? "Network is down" :
			  err == WSAHOST_NOT_FOUND ? "Host does not exist" :
			  err == WSATRY_AGAIN ? "Host not found" :
#ifndef NO_IPV6
			  p_getaddrinfo ? "getaddrinfo: unknown error" :
#endif
			  "gethostbyname: unknown error");
	} else {
	    ret->error = NULL;
	    ret->family = ret_family;

#ifndef NO_IPV6
	    /* If we got an address info use that... */
	    if (ret->ai) {
		/* Are we in IPv4 fallback mode? */
		/* We put the IPv4 address into the a variable so we can further-on use the IPv4 code... */
		if (ret->family == AF_INET)
		    memcpy(&a,
			   (char *) &((SOCKADDR_IN *) ret->ai->
				      ai_addr)->sin_addr, sizeof(a));

		/* Now let's find that canonicalname... */
		if (p_getnameinfo) {
		    if (p_getnameinfo
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
		int n;
		for (n = 0; h->h_addr_list[n]; n++);
		ret->addresses = snewn(n, unsigned long);
		ret->naddresses = n;
		for (n = 0; n < ret->naddresses; n++) {
		    memcpy(&a, h->h_addr_list[n], sizeof(a));
		    ret->addresses[n] = p_ntohl(a);
		}
		ret->curraddr = 0;
		memcpy(&a, h->h_addr, sizeof(a));
		/* This way we are always sure the h->h_name is valid :) */
		strncpy(realhost, h->h_name, sizeof(realhost));
	    }
	}
    } else {
	/*
	 * This must be a numeric IPv4 address because it caused a
	 * success return from inet_addr.
	 */
	ret->addresses = snewn(1, unsigned long);
	ret->naddresses = 1;
	ret->curraddr = 0;
	ret->addresses[0] = p_ntohl(a);
	ret->family = AF_INET;
	strncpy(realhost, host, sizeof(realhost));
    }
    realhost[lenof(realhost)-1] = '\0';
    *canonicalname = snewn(1+strlen(realhost), char);
    strcpy(*canonicalname, realhost);
    return ret;
}

SockAddr sk_nonamelookup(const char *host)
{
    SockAddr ret = snew(struct SockAddr_tag);
    // MP
    memset(ret, 0, sizeof(struct SockAddr_tag));
    ret->error = NULL;
    ret->family = AF_UNSPEC;
#ifndef NO_IPV6
    ret->ai = ret->ais = NULL;
#endif
    ret->naddresses = 0;
    strncpy(ret->hostname, host, lenof(ret->hostname));
    ret->hostname[lenof(ret->hostname)-1] = '\0';
    return ret;
}

int sk_nextaddr(SockAddr addr)
{
#ifndef NO_IPV6
    if (addr->ai) {
	if (addr->ai->ai_next) {
	    addr->ai = addr->ai->ai_next;
	    addr->family = addr->ai->ai_family;
	    return TRUE;
	} else
	    return FALSE;
    }
#endif
    if (addr->curraddr+1 < addr->naddresses) {
	addr->curraddr++;
	return TRUE;
    } else {
	return FALSE;
    }
}

void sk_getaddr(SockAddr addr, char *buf, int buflen)
{
#ifndef NO_IPV6
    if (addr->ai) {
	/* Try to get the WSAAddressToStringA() function from wship6.dll */
	/* This way one doesn't need to have IPv6 dll's to use PuTTY and
	 * it will fallback to IPv4. */
	typedef int (CALLBACK * FADDRTOSTR) (LPSOCKADDR lpsaAddress,
		DWORD dwAddressLength,
		LPWSAPROTOCOL_INFO lpProtocolInfo,
		OUT LPTSTR lpszAddressString,
		IN OUT LPDWORD lpdwAddressStringLength
	);
	FADDRTOSTR fAddrToStr = NULL;

	HINSTANCE dllWS2 = LoadLibrary("ws2_32.dll");
	if (dllWS2) {
	    fAddrToStr = (FADDRTOSTR)GetProcAddress(dllWS2,
						    "WSAAddressToStringA");
	    if (fAddrToStr) {
		fAddrToStr(addr->ai->ai_addr, addr->ai->ai_addrlen,
			   NULL, buf, &buflen);
	    }
	    else strncpy(buf, "IPv6", buflen);
	    FreeLibrary(dllWS2);
	}
    } else
#endif
    if (addr->family == AF_INET) {
	struct in_addr a;
	assert(addr->addresses && addr->curraddr < addr->naddresses);
	a.s_addr = p_htonl(addr->addresses[addr->curraddr]);
	strncpy(buf, p_inet_ntoa(a), buflen);
	buf[buflen-1] = '\0';
    } else {
	strncpy(buf, addr->hostname, buflen);
	buf[buflen-1] = '\0';
    }
}

int sk_hostname_is_local(char *name)
{
    return !strcmp(name, "localhost");
}

static INTERFACE_INFO local_interfaces[16];
static int n_local_interfaces;       /* 0=not yet, -1=failed, >0=number */

static int ipv4_is_local_addr(struct in_addr addr)
{
    if (ipv4_is_loopback(addr))
	return 1;		       /* loopback addresses are local */
    if (!n_local_interfaces) {
	SOCKET s = p_socket(AF_INET, SOCK_DGRAM, 0);
	DWORD retbytes;

	if (p_WSAIoctl &&
	    p_WSAIoctl(s, SIO_GET_INTERFACE_LIST, NULL, 0,
		       local_interfaces, sizeof(local_interfaces),
		       &retbytes, NULL, NULL) == 0)
	    n_local_interfaces = retbytes / sizeof(INTERFACE_INFO);
	else
	    logevent(NULL, "Unable to get list of local IP addresses");
    }
    if (n_local_interfaces > 0) {
	int i;
	for (i = 0; i < n_local_interfaces; i++) {
	    SOCKADDR_IN *address =
		(SOCKADDR_IN *)&local_interfaces[i].iiAddress;
	    if (address->sin_addr.s_addr == addr.s_addr)
		return 1;	       /* this address is local */
	}
    }
    return 0;		       /* this address is not local */
}

int sk_address_is_local(SockAddr addr)
{
#ifndef NO_IPV6
    if (addr->family == AF_INET6) {
    	return IN6_IS_ADDR_LOOPBACK((const struct in6_addr *)addr->ai->ai_addr);
    } else
#endif
    if (addr->family == AF_INET) {
#ifndef NO_IPV6
	if (addr->ai) {
	    return ipv4_is_local_addr(((struct sockaddr_in *)addr->ai->ai_addr)
				      ->sin_addr);
	} else
#endif
	{
	    struct in_addr a;
	    assert(addr->addresses && addr->curraddr < addr->naddresses);
	    a.s_addr = p_htonl(addr->addresses[addr->curraddr]);
	    return ipv4_is_local_addr(a);
	}
    } else {
	assert(addr->family == AF_UNSPEC);
	return 0;		       /* we don't know; assume not */
    }
}

int sk_addrtype(SockAddr addr)
{
    return (addr->family == AF_INET ? ADDRTYPE_IPV4 :
#ifndef NO_IPV6
	    addr->family == AF_INET6 ? ADDRTYPE_IPV6 :
#endif
	    ADDRTYPE_NAME);
}

void sk_addrcopy(SockAddr addr, char *buf)
{
    assert(addr->family != AF_UNSPEC);
#ifndef NO_IPV6
    if (addr->ai) {
	if (addr->family == AF_INET)
	    memcpy(buf, &((struct sockaddr_in *)addr->ai->ai_addr)->sin_addr,
		   sizeof(struct in_addr));
	else if (addr->family == AF_INET6)
	    memcpy(buf, &((struct sockaddr_in6 *)addr->ai->ai_addr)->sin6_addr,
		   sizeof(struct in6_addr));
	else
	    assert(FALSE);
    } else
#endif
    if (addr->family == AF_INET) {
	struct in_addr a;
	assert(addr->addresses && addr->curraddr < addr->naddresses);
	a.s_addr = p_htonl(addr->addresses[addr->curraddr]);
	memcpy(buf, (char*) &a.s_addr, 4);
    }
}

void sk_addr_free(SockAddr addr)
{
#ifndef NO_IPV6
    if (addr->ais && p_freeaddrinfo)
	p_freeaddrinfo(addr->ais);
#endif
    if (addr->addresses)
	sfree(addr->addresses);
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
static int sk_tcp_write(Socket s, const char *data, int len);
static int sk_tcp_write_oob(Socket s, const char *data, int len);
static void sk_tcp_set_private_ptr(Socket s, void *ptr);
static void *sk_tcp_get_private_ptr(Socket s);
static void sk_tcp_set_frozen(Socket s, int is_frozen);
static const char *sk_tcp_socket_error(Socket s);

#ifdef MPEXT
extern char *do_select(Plug plug, SOCKET skt, int startup);
#else
extern char *do_select(SOCKET skt, int startup);
#endif

Socket sk_register(void *sock, Plug plug)
{
    static const struct socket_function_table fn_table = {
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
    ret = snew(struct Socket_tag);
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
    ret->parent = ret->child = NULL;
    ret->addr = NULL;

    ret->s = (SOCKET)sock;

    if (ret->s == INVALID_SOCKET) {
	err = p_WSAGetLastError();
	ret->error = winsock_error_string(err);
	return (Socket) ret;
    }

    ret->oobinline = 0;

    /* Set up a select mechanism. This could be an AsyncSelect on a
     * window, or an EventSelect on an event object. */
#ifdef MPEXT
    errstr = do_select(plug, ret->s, 1);
#else
    errstr = do_select(ret->s, 1);
#endif
    if (errstr) {
	ret->error = errstr;
	return (Socket) ret;
    }

    add234(sktree, ret);

    return (Socket) ret;
}

static DWORD try_connect(Actual_Socket sock)
{
    SOCKET s;
#ifndef NO_IPV6
    SOCKADDR_IN6 a6;
#endif
    SOCKADDR_IN a;
    DWORD err;
    char *errstr;
    short localport;
    int family;

    if (sock->s != INVALID_SOCKET) {
#ifdef MPEXT
	do_select(sock->plug, sock->s, 0);
#else
	do_select(sock->s, 0);
#endif
        p_closesocket(sock->s);
    }

    plug_log(sock->plug, 0, sock->addr, sock->port, NULL, 0);

    /*
     * Open socket.
     */
#ifndef NO_IPV6
    /* Let's default to IPv6, this shouldn't hurt anybody
     * If the stack supports IPv6 it will also allow IPv4 connections. */
    if (sock->addr->ai) {
	family = sock->addr->ai->ai_family;
    } else
#endif
    {
	/* Default to IPv4 */
	family = AF_INET;
    }

    s = p_socket(family, SOCK_STREAM, 0);
    sock->s = s;

    if (s == INVALID_SOCKET) {
	err = p_WSAGetLastError();
	sock->error = winsock_error_string(err);
	goto ret;
    }

    if (sock->oobinline) {
	BOOL b = TRUE;
	p_setsockopt(s, SOL_SOCKET, SO_OOBINLINE, (void *) &b, sizeof(b));
    }

    if (sock->nodelay) {
	BOOL b = TRUE;
	p_setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (void *) &b, sizeof(b));
    }

    if (sock->keepalive) {
	BOOL b = TRUE;
	p_setsockopt(s, SOL_SOCKET, SO_KEEPALIVE, (void *) &b, sizeof(b));
    }

    /*
     * Bind to local address.
     */
    if (sock->privport)
	localport = 1023;	       /* count from 1023 downwards */
    else
	localport = 0;		       /* just use port 0 (ie winsock picks) */

    /* Loop round trying to bind */
    while (1) {
	int sockcode;

#ifndef NO_IPV6
	if (family == AF_INET6) {
	    memset(&a6, 0, sizeof(a6));
	    a6.sin6_family = AF_INET6;
          /*a6.sin6_addr = in6addr_any; */ /* == 0 done by memset() */
	    a6.sin6_port = p_htons(localport);
	} else
#endif
	{
	    a.sin_family = AF_INET;
	    a.sin_addr.s_addr = p_htonl(INADDR_ANY);
	    a.sin_port = p_htons(localport);
	}
#ifndef NO_IPV6
	sockcode = p_bind(s, (sock->addr->family == AF_INET6 ?
			   (struct sockaddr *) &a6 :
			   (struct sockaddr *) &a),
		       (sock->addr->family ==
			AF_INET6 ? sizeof(a6) : sizeof(a)));
#else
	sockcode = p_bind(s, (struct sockaddr *) &a, sizeof(a));
#endif
	if (sockcode != SOCKET_ERROR) {
	    err = 0;
	    break;		       /* done */
	} else {
	    err = p_WSAGetLastError();
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
	sock->error = winsock_error_string(err);
	goto ret;
    }

    /*
     * Connect to remote address.
     */
#ifndef NO_IPV6
    if (sock->addr->ai) {
	if (family == AF_INET6) {
	    a6.sin6_family = AF_INET6;
	    a6.sin6_port = p_htons((short) sock->port);
	    a6.sin6_addr =
		((struct sockaddr_in6 *) sock->addr->ai->ai_addr)->sin6_addr;
	} else {
	    a.sin_family = AF_INET;
	    a.sin_addr =
		((struct sockaddr_in *) sock->addr->ai->ai_addr)->sin_addr;
	    a.sin_port = p_htons((short) sock->port);
	}
    } else
#endif
    {
	assert(sock->addr->addresses && sock->addr->curraddr < sock->addr->naddresses);
	a.sin_family = AF_INET;
	a.sin_addr.s_addr = p_htonl(sock->addr->addresses[sock->addr->curraddr]);
	a.sin_port = p_htons((short) sock->port);
    }

    /* Set up a select mechanism. This could be an AsyncSelect on a
     * window, or an EventSelect on an event object. */
#ifdef MPEXT
    errstr = do_select(sock->plug, s, 1);
#else
    errstr = do_select(s, 1);
#endif
    if (errstr) {
	sock->error = errstr;
	err = 1;
	goto ret;
    }

    if ((
#ifndef NO_IPV6
	    p_connect(s,
		      ((family == AF_INET6) ? (struct sockaddr *) &a6 :
		       (struct sockaddr *) &a),
		      (family == AF_INET6) ? sizeof(a6) : sizeof(a))
#else
	    p_connect(s, (struct sockaddr *) &a, sizeof(a))
#endif
	) == SOCKET_ERROR) {
	err = p_WSAGetLastError();
	/*
	 * We expect a potential EWOULDBLOCK here, because the
	 * chances are the front end has done a select for
	 * FD_CONNECT, so that connect() will complete
	 * asynchronously.
	 */
	if ( err != WSAEWOULDBLOCK ) {
	    sock->error = winsock_error_string(err);
	    goto ret;
	}
    } else {
	/*
	 * If we _don't_ get EWOULDBLOCK, the connect has completed
	 * and we should set the socket as writable.
	 */
	sock->writable = 1;
    }

    add234(sktree, sock);

    err = 0;

    ret:
    if (err)
	plug_log(sock->plug, 1, sock->addr, sock->port, sock->error, err);
    return err;
}

Socket sk_new(SockAddr addr, int port, int privport, int oobinline,
	      int nodelay, int keepalive, Plug plug)
{
    static const struct socket_function_table fn_table = {
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

    Actual_Socket ret;
    DWORD err;

    /*
     * Create Socket structure.
     */
    ret = snew(struct Socket_tag);
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
    ret->parent = ret->child = NULL;
    ret->oobinline = oobinline;
    ret->nodelay = nodelay;
    ret->keepalive = keepalive;
    ret->privport = privport;
    ret->port = port;
    ret->addr = addr;
    ret->s = INVALID_SOCKET;

    err = 0;
    do {
        err = try_connect(ret);
    } while (err && sk_nextaddr(ret->addr));

    return (Socket) ret;
}

Socket sk_newlistener(char *srcaddr, int port, Plug plug, int local_host_only,
		      int orig_address_family)
{
    static const struct socket_function_table fn_table = {
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
#ifndef NO_IPV6
    SOCKADDR_IN6 a6;
#endif
    SOCKADDR_IN a;

    DWORD err;
    char *errstr;
    Actual_Socket ret;
    int retcode;
    int on = 1;

    int address_family;

    /*
     * Create Socket structure.
     */
    ret = snew(struct Socket_tag);
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
    ret->parent = ret->child = NULL;
    ret->addr = NULL;

    /*
     * Translate address_family from platform-independent constants
     * into local reality.
     */
    address_family = (orig_address_family == ADDRTYPE_IPV4 ? AF_INET :
#ifndef NO_IPV6
		      orig_address_family == ADDRTYPE_IPV6 ? AF_INET6 :
#endif
		      AF_UNSPEC);

    /*
     * Our default, if passed the `don't care' value
     * ADDRTYPE_UNSPEC, is to listen on IPv4. If IPv6 is supported,
     * we will also set up a second socket listening on IPv6, but
     * the v4 one is primary since that ought to work even on
     * non-v6-supporting systems.
     */
    if (address_family == AF_UNSPEC) address_family = AF_INET;

    /*
     * Open socket.
     */
    s = p_socket(address_family, SOCK_STREAM, 0);
    ret->s = s;

    if (s == INVALID_SOCKET) {
	err = p_WSAGetLastError();
	ret->error = winsock_error_string(err);
	return (Socket) ret;
    }

    ret->oobinline = 0;

    p_setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (const char *)&on, sizeof(on));

#ifndef NO_IPV6
	if (address_family == AF_INET6) {
	    memset(&a6, 0, sizeof(a6));
	    a6.sin6_family = AF_INET6;
	    /* FIXME: srcaddr is ignored for IPv6, because I (SGT) don't
	     * know how to do it. :-)
	     * (jeroen:) saddr is specified as an address.. eg 2001:db8::1
	     * Thus we need either a parser that understands [2001:db8::1]:80
	     * style addresses and/or enhance this to understand hostnames too. */
	    if (local_host_only)
		a6.sin6_addr = in6addr_loopback;
	    else
		a6.sin6_addr = in6addr_any;
	    a6.sin6_port = p_htons(port);
	} else
#endif
	{
	    int got_addr = 0;
	    a.sin_family = AF_INET;

	    /*
	     * Bind to source address. First try an explicitly
	     * specified one...
	     */
	    if (srcaddr) {
		a.sin_addr.s_addr = p_inet_addr(srcaddr);
		if (a.sin_addr.s_addr != INADDR_NONE) {
		    /* Override localhost_only with specified listen addr. */
		    ret->localhost_only = ipv4_is_loopback(a.sin_addr);
		    got_addr = 1;
		}
	    }

	    /*
	     * ... and failing that, go with one of the standard ones.
	     */
	    if (!got_addr) {
		if (local_host_only)
		    a.sin_addr.s_addr = p_htonl(INADDR_LOOPBACK);
		else
		    a.sin_addr.s_addr = p_htonl(INADDR_ANY);
	    }

	    a.sin_port = p_htons((short)port);
	}
#ifndef NO_IPV6
	retcode = p_bind(s, (address_family == AF_INET6 ?
			   (struct sockaddr *) &a6 :
			   (struct sockaddr *) &a),
		       (address_family ==
			AF_INET6 ? sizeof(a6) : sizeof(a)));
#else
	retcode = p_bind(s, (struct sockaddr *) &a, sizeof(a));
#endif
	if (retcode != SOCKET_ERROR) {
	    err = 0;
	} else {
	    err = p_WSAGetLastError();
	}

    if (err) {
	p_closesocket(s);
	ret->error = winsock_error_string(err);
	return (Socket) ret;
    }


    if (p_listen(s, SOMAXCONN) == SOCKET_ERROR) {
        p_closesocket(s);
	ret->error = winsock_error_string(err);
	return (Socket) ret;
    }

    /* Set up a select mechanism. This could be an AsyncSelect on a
     * window, or an EventSelect on an event object. */
#ifdef MPEXT
    errstr = do_select(plug, s, 1);
#else
    errstr = do_select(s, 1);
#endif
    if (errstr) {
	p_closesocket(s);
	ret->error = errstr;
	return (Socket) ret;
    }

    add234(sktree, ret);

#ifndef NO_IPV6
    /*
     * If we were given ADDRTYPE_UNSPEC, we must also create an
     * IPv6 listening socket and link it to this one.
     */
    if (address_family == AF_INET && orig_address_family == ADDRTYPE_UNSPEC) {
	Actual_Socket other;

	other = (Actual_Socket) sk_newlistener(srcaddr, port, plug,
					       local_host_only, ADDRTYPE_IPV6);

	if (other) {
	    if (!other->error) {
		other->parent = ret;
		ret->child = other;
	    } else {
		sfree(other);
	    }
	}
    }
#endif

    return (Socket) ret;
}

static void sk_tcp_close(Socket sock)
{
#ifdef MPEXT
    extern char *do_select(Plug plug, SOCKET skt, int startup);
#else
    extern char *do_select(SOCKET skt, int startup);
#endif
    Actual_Socket s = (Actual_Socket) sock;

    if (s->child)
	sk_tcp_close((Socket)s->child);

    del234(sktree, s);
#ifdef MPEXT
    do_select(s->plug, s->s, 0);
#else
    do_select(s->s, 0);
#endif
    p_closesocket(s->s);
    if (s->addr)
	sk_addr_free(s->addr);
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
	nsent = p_send(s->s, data, len, urgentflag);
	noise_ultralight(nsent);
	if (nsent <= 0) {
	    err = (nsent < 0 ? p_WSAGetLastError() : 0);
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
		/* We're inside the Windows frontend here, so we know
		 * that the frontend handle is unnecessary. */
		logevent(NULL, winsock_error_string(err));
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

static int sk_tcp_write(Socket sock, const char *buf, int len)
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

static int sk_tcp_write_oob(Socket sock, const char *buf, int len)
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

    if (wParam == 0)
	return 1;		       /* boggle */

    s = find234(sktree, (void *) wParam, cmpforsearch);
    if (!s)
	return 1;		       /* boggle */

    if ((err = WSAGETSELECTERROR(lParam)) != 0) {
	/*
	 * An error has occurred on this socket. Pass it to the
	 * plug.
	 */
	if (s->addr) {
	    plug_log(s->plug, 1, s->addr, s->port,
		     winsock_error_string(err), err);
	    while (s->addr && sk_nextaddr(s->addr)) {
		err = try_connect(s);
	    }
	}
	if (err != 0)
	    return plug_closing(s->plug, winsock_error_string(err), err, 0);
	else
	    return 1;
    }

    noise_ultralight(lParam);

    switch (WSAGETSELECTEVENT(lParam)) {
      case FD_CONNECT:
	s->connected = s->writable = 1;
	/*
	 * Once a socket is connected, we can stop falling
	 * back through the candidate addresses to connect
	 * to.
	 */
	if (s->addr) {
	    sk_addr_free(s->addr);
	    s->addr = NULL;
	}
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
	    p_ioctlsocket(s->s, SIOCATMARK, &atmark);
	    /*
	     * Avoid checking the return value from ioctlsocket(),
	     * on the grounds that some WinSock wrappers don't
	     * support it. If it does nothing, we get atmark==1,
	     * which is equivalent to `no OOB pending', so the
	     * effect will be to non-OOB-ify any OOB data.
	     */
	} else
	    atmark = 1;

	ret = p_recv(s->s, buf, sizeof(buf), 0);
	noise_ultralight(ret);
	if (ret < 0) {
	    err = p_WSAGetLastError();
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
	ret = p_recv(s->s, buf, sizeof(buf), MSG_OOB);
	noise_ultralight(ret);
	if (ret <= 0) {
	    char *str = (ret == 0 ? "Internal networking trouble" :
			 winsock_error_string(p_WSAGetLastError()));
	    /* We're inside the Windows frontend here, so we know
	     * that the frontend handle is unnecessary. */
	    logevent(NULL, str);
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
	    ret = p_recv(s->s, buf, sizeof(buf), 0);
	    if (ret < 0) {
		err = p_WSAGetLastError();
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
#ifdef NO_IPV6
	    struct sockaddr_in isa;
#else
            struct sockaddr_storage isa;
#endif
	    int addrlen = sizeof(isa);
	    SOCKET t;  /* socket of connection */

	    memset(&isa, 0, sizeof(isa));
	    err = 0;
	    t = p_accept(s->s,(struct sockaddr *)&isa,&addrlen);
	    if (t == INVALID_SOCKET)
	    {
		err = p_WSAGetLastError();
		if (err == WSATRY_AGAIN)
		    break;
	    }
#ifndef NO_IPV6
            if (isa.ss_family == AF_INET &&
                s->localhost_only &&
                !ipv4_is_local_addr(((struct sockaddr_in *)&isa)->sin_addr)) {
#else
	    if (s->localhost_only && !ipv4_is_local_addr(isa.sin_addr)) {
#endif
		p_closesocket(t);      /* dodgy WinSock let nonlocal through */
	    } else if (plug_accepting(s->plug, (void*)t)) {
		p_closesocket(t);      /* denied or error */
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
const char *sk_addr_error(SockAddr addr)
{
    return addr->error;
}
static const char *sk_tcp_socket_error(Socket sock)
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
    if (!is_frozen) {
#ifdef MPEXT
	do_select(s->plug, s->s, 1);
#else
	do_select(s->s, 1);
#endif
	if (s->frozen_readable) {
	    char c;
	    p_recv(s->s, &c, 1, MSG_PEEK);
	}
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

extern int socket_writable(SOCKET skt)
{
    Actual_Socket s = find234(sktree, (void *)skt, cmpforsearch);

    if (s)
	return bufchain_size(&s->output_data) > 0;
    else
	return 0;
}

int net_service_lookup(char *service)
{
    struct servent *se;
    se = p_getservbyname(service, NULL);
    if (se != NULL)
	return p_ntohs(se->s_port);
    else
	return 0;
}

SockAddr platform_get_x11_unix_address(int displaynum, char **canonicalname)
{
    SockAddr ret = snew(struct SockAddr_tag);
    memset(ret, 0, sizeof(struct SockAddr_tag));
    ret->error = "unix sockets not supported on this platform";
    return ret;
}
