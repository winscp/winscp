/*
 * Networking abstraction in PuTTY.
 *
 * The way this works is: a back end can choose to open any number
 * of sockets - including zero, which might be necessary in some.
 * It can register a bunch of callbacks (most notably for when 
 * data is received) for each socket, and it can call the networking
 * abstraction to send data without having to worry about blocking.
 * The stuff behind the abstraction takes care of selects and
 * nonblocking writes and all that sort of painful gubbins.
 */

#ifndef PUTTY_NETWORK_H
#define PUTTY_NETWORK_H

#ifndef DONE_TYPEDEFS
#define DONE_TYPEDEFS
typedef struct config_tag Config;
typedef struct backend_tag Backend;
typedef struct terminal_tag Terminal;
#endif

typedef struct SockAddr_tag *SockAddr;
/* pay attention to levels of indirection */
typedef struct socket_function_table **Socket;
typedef struct plug_function_table **Plug;

#ifndef OSSOCKET_DEFINED
typedef void *OSSocket;
#endif

struct socket_function_table {
    Plug(*plug) (Socket s, Plug p);
    /* use a different plug (return the old one) */
    /* if p is NULL, it doesn't change the plug */
    /* but it does return the one it's using */
    void (*close) (Socket s);
    int (*write) (Socket s, const char *data, int len);
    int (*write_oob) (Socket s, const char *data, int len);
    void (*flush) (Socket s);
    void (*set_private_ptr) (Socket s, void *ptr);
    void *(*get_private_ptr) (Socket s);
    void (*set_frozen) (Socket s, int is_frozen);
    /* ignored by tcp, but vital for ssl */
    const char *(*socket_error) (Socket s);
};

struct plug_function_table {
    int (*closing)
     (Plug p, const char *error_msg, int error_code, int calling_back);
    /* error_msg is NULL iff it is not an error (ie it closed normally) */
    /* calling_back != 0 iff there is a Plug function */
    /* currently running (would cure the fixme in try_send()) */
    int (*receive) (Plug p, int urgent, char *data, int len);
    /*
     *  - urgent==0. `data' points to `len' bytes of perfectly
     *    ordinary data.
     * 
     *  - urgent==1. `data' points to `len' bytes of data,
     *    which were read from before an Urgent pointer.
     * 
     *  - urgent==2. `data' points to `len' bytes of data,
     *    the first of which was the one at the Urgent mark.
     */
    void (*sent) (Plug p, int bufsize);
    /*
     * The `sent' function is called when the pending send backlog
     * on a socket is cleared or partially cleared. The new backlog
     * size is passed in the `bufsize' parameter.
     */
    int (*accepting)(Plug p, OSSocket sock);
    /*
     * returns 0 if the host at address addr is a valid host for connecting or error
     */
};

/* proxy indirection layer */
/* NB, control of 'addr' is passed via new_connection, which takes
 * responsibility for freeing it */
Socket new_connection(SockAddr addr, char *hostname,
		      int port, int privport,
		      int oobinline, int nodelay, int keepalive,
		      Plug plug, const Config *cfg);
Socket new_listener(char *srcaddr, int port, Plug plug, int local_host_only,
		    const Config *cfg);
SockAddr name_lookup(char *host, int port, char **canonicalname,
		     const Config *cfg);

/* platform-dependent callback from new_connection() */
/* (same caveat about addr as new_connection()) */
Socket platform_new_connection(SockAddr addr, char *hostname,
			       int port, int privport,
			       int oobinline, int nodelay, int keepalive,
			       Plug plug, const Config *cfg);

/* socket functions */

void sk_init(void);		       /* called once at program startup */
void sk_cleanup(void);		       /* called just before program exit */

SockAddr sk_namelookup(const char *host, char **canonicalname);
SockAddr sk_nonamelookup(const char *host);
void sk_getaddr(SockAddr addr, char *buf, int buflen);
int sk_hostname_is_local(char *name);
int sk_address_is_local(SockAddr addr);
enum { ADDRTYPE_IPV4, ADDRTYPE_IPV6, ADDRTYPE_NAME };
int sk_addrtype(SockAddr addr);
void sk_addrcopy(SockAddr addr, char *buf);
void sk_addr_free(SockAddr addr);

/* NB, control of 'addr' is passed via sk_new, which takes responsibility
 * for freeing it, as for new_connection() */
Socket sk_new(SockAddr addr, int port, int privport, int oobinline,
	      int nodelay, int keepalive, Plug p);

Socket sk_newlistener(char *srcaddr, int port, Plug plug, int local_host_only);

Socket sk_register(OSSocket sock, Plug plug);

#define sk_plug(s,p) (((*s)->plug) (s, p))
#define sk_close(s) (((*s)->close) (s))
#define sk_write(s,buf,len) (((*s)->write) (s, buf, len))
#define sk_write_oob(s,buf,len) (((*s)->write_oob) (s, buf, len))
#define sk_flush(s) (((*s)->flush) (s))

#ifdef DEFINE_PLUG_METHOD_MACROS
#define plug_closing(p,msg,code,callback) (((*p)->closing) (p, msg, code, callback))
#define plug_receive(p,urgent,buf,len) (((*p)->receive) (p, urgent, buf, len))
#define plug_sent(p,bufsize) (((*p)->sent) (p, bufsize))
#define plug_accepting(p, sock) (((*p)->accepting)(p, sock))
#endif

/*
 * Each socket abstraction contains a `void *' private field in
 * which the client can keep state.
 *
 * This is perhaps unnecessary now that we have the notion of a plug,
 * but there is some existing code that uses it, so it stays.
 */
#define sk_set_private_ptr(s, ptr) (((*s)->set_private_ptr) (s, ptr))
#define sk_get_private_ptr(s) (((*s)->get_private_ptr) (s))

/*
 * Special error values are returned from sk_namelookup and sk_new
 * if there's a problem. These functions extract an error message,
 * or return NULL if there's no problem.
 */
const char *sk_addr_error(SockAddr addr);
#define sk_socket_error(s) (((*s)->socket_error) (s))

/*
 * Set the `frozen' flag on a socket. A frozen socket is one in
 * which all READABLE notifications are ignored, so that data is
 * not accepted from the peer until the socket is unfrozen. This
 * exists for two purposes:
 * 
 *  - Port forwarding: when a local listening port receives a
 *    connection, we do not want to receive data from the new
 *    socket until we have somewhere to send it. Hence, we freeze
 *    the socket until its associated SSH channel is ready; then we
 *    unfreeze it and pending data is delivered.
 * 
 *  - Socket buffering: if an SSH channel (or the whole connection)
 *    backs up or presents a zero window, we must freeze the
 *    associated local socket in order to avoid unbounded buffer
 *    growth.
 */
#define sk_set_frozen(s, is_frozen) (((*s)->set_frozen) (s, is_frozen))

/*
 * Call this after an operation that might have tried to send on a
 * socket, to clean up any pending network errors.
 */
void net_pending_errors(void);

/*
 * Simple wrapper on getservbyname(), needed by ssh.c. Returns the
 * port number, in host byte order (suitable for printf and so on).
 * Returns 0 on failure. Any platform not supporting getservbyname
 * can just return 0 - this function is not required to handle
 * numeric port specifications.
 */
int net_service_lookup(char *service);

/********** SSL stuff **********/

/*
 * This section is subject to change, but you get the general idea
 * of what it will eventually look like.
 */

typedef struct certificate *Certificate;
typedef struct our_certificate *Our_Certificate;
    /* to be defined somewhere else, somehow */

typedef struct ssl_client_socket_function_table **SSL_Client_Socket;
typedef struct ssl_client_plug_function_table **SSL_Client_Plug;

struct ssl_client_socket_function_table {
    struct socket_function_table base;
    void (*renegotiate) (SSL_Client_Socket s);
    /* renegotiate the cipher spec */
};

struct ssl_client_plug_function_table {
    struct plug_function_table base;
    int (*refuse_cert) (SSL_Client_Plug p, Certificate cert[]);
    /* do we accept this certificate chain?  If not, why not? */
    /* cert[0] is the server's certificate, cert[] is NULL-terminated */
    /* the last certificate may or may not be the root certificate */
     Our_Certificate(*client_cert) (SSL_Client_Plug p);
    /* the server wants us to identify ourselves */
    /* may return NULL if we want anonymity */
};

SSL_Client_Socket sk_ssl_client_over(Socket s,	/* pre-existing (tcp) connection */
				     SSL_Client_Plug p);

#define sk_renegotiate(s) (((*s)->renegotiate) (s))

#endif
