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
typedef struct conf_tag Conf;
typedef struct backend_tag Backend;
typedef struct terminal_tag Terminal;
#endif

typedef struct SockAddr_tag *SockAddr;
/* pay attention to levels of indirection */
typedef struct socket_function_table **Socket;
typedef struct plug_function_table **Plug;

struct socket_function_table {
    Plug(*plug) (Socket s, Plug p);
    /* use a different plug (return the old one) */
    /* if p is NULL, it doesn't change the plug */
    /* but it does return the one it's using */
    void (*close) (Socket s);
    int (*write) (Socket s, const char *data, int len);
    int (*write_oob) (Socket s, const char *data, int len);
    void (*write_eof) (Socket s);
    void (*flush) (Socket s);
    void (*set_frozen) (Socket s, int is_frozen);
    /* ignored by tcp, but vital for ssl */
    const char *(*socket_error) (Socket s);
    char *(*peer_info) (Socket s);
};

typedef union { void *p; int i; } accept_ctx_t;
typedef Socket (*accept_fn_t)(accept_ctx_t ctx, Plug plug);

struct plug_function_table {
    void (*log)(Plug p, int type, SockAddr addr, int port,
		const char *error_msg, int error_code);
    /*
     * Passes the client progress reports on the process of setting
     * up the connection.
     * 
     * 	- type==0 means we are about to try to connect to address
     * 	  `addr' (error_msg and error_code are ignored)
     * 	- type==1 means we have failed to connect to address `addr'
     * 	  (error_msg and error_code are supplied). This is not a
     * 	  fatal error - we may well have other candidate addresses
     * 	  to fall back to. When it _is_ fatal, the closing()
     * 	  function will be called.
     */
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
    int (*accepting)(Plug p, accept_fn_t constructor, accept_ctx_t ctx);
    /*
     * `accepting' is called only on listener-type sockets, and is
     * passed a constructor function+context that will create a fresh
     * Socket describing the connection. It returns nonzero if it
     * doesn't want the connection for some reason, or 0 on success.
     */
};

/* proxy indirection layer */
/* NB, control of 'addr' is passed via new_connection, which takes
 * responsibility for freeing it */
Socket new_connection(SockAddr addr, char *hostname,
		      int port, int privport,
		      int oobinline, int nodelay, int keepalive,
		      Plug plug, Conf *conf);
Socket new_listener(char *srcaddr, int port, Plug plug, int local_host_only,
		    Conf *conf, int addressfamily);
SockAddr name_lookup(char *host, int port, char **canonicalname,
		     Conf *conf, int addressfamily);
int proxy_for_destination (SockAddr addr, const char *hostname, int port,
                           Conf *conf);

/* platform-dependent callback from new_connection() */
/* (same caveat about addr as new_connection()) */
Socket platform_new_connection(SockAddr addr, char *hostname,
			       int port, int privport,
			       int oobinline, int nodelay, int keepalive,
			       Plug plug, Conf *conf);

/* socket functions */

void sk_init(void);		       /* called once at program startup */
void sk_cleanup(void);		       /* called just before program exit */

SockAddr sk_namelookup(const char *host, char **canonicalname, int address_family);
SockAddr sk_nonamelookup(const char *host);
void sk_getaddr(SockAddr addr, char *buf, int buflen);
int sk_addr_needs_port(SockAddr addr);
int sk_hostname_is_local(const char *name);
int sk_address_is_local(SockAddr addr);
int sk_address_is_special_local(SockAddr addr);
int sk_addrtype(SockAddr addr);
void sk_addrcopy(SockAddr addr, char *buf);
void sk_addr_free(SockAddr addr);
/* sk_addr_dup generates another SockAddr which contains the same data
 * as the original one and can be freed independently. May not actually
 * physically _duplicate_ it: incrementing a reference count so that
 * one more free is required before it disappears is an acceptable
 * implementation. */
SockAddr sk_addr_dup(SockAddr addr);

/* NB, control of 'addr' is passed via sk_new, which takes responsibility
 * for freeing it, as for new_connection() */
Socket sk_new(SockAddr addr, int port, int privport, int oobinline,
	      int nodelay, int keepalive, Plug p);

Socket sk_newlistener(char *srcaddr, int port, Plug plug, int local_host_only, int address_family);

#define sk_plug(s,p) (((*s)->plug) (s, p))
#define sk_close(s) (((*s)->close) (s))
#define sk_write(s,buf,len) (((*s)->write) (s, buf, len))
#define sk_write_oob(s,buf,len) (((*s)->write_oob) (s, buf, len))
#define sk_write_eof(s) (((*s)->write_eof) (s))
#define sk_flush(s) (((*s)->flush) (s))

#ifdef DEFINE_PLUG_METHOD_MACROS
#define plug_log(p,type,addr,port,msg,code) (((*p)->log) (p, type, addr, port, msg, code))
#define plug_closing(p,msg,code,callback) (((*p)->closing) (p, msg, code, callback))
#define plug_receive(p,urgent,buf,len) (((*p)->receive) (p, urgent, buf, len))
#define plug_sent(p,bufsize) (((*p)->sent) (p, bufsize))
#define plug_accepting(p, constructor, ctx) (((*p)->accepting)(p, constructor, ctx))
#endif

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
 * Return a (dynamically allocated) string giving some information
 * about the other end of the socket, suitable for putting in log
 * files. May be NULL if nothing is available at all.
 */
#define sk_peer_info(s) (((*s)->peer_info) (s))

/*
 * Simple wrapper on getservbyname(), needed by ssh.c. Returns the
 * port number, in host byte order (suitable for printf and so on).
 * Returns 0 on failure. Any platform not supporting getservbyname
 * can just return 0 - this function is not required to handle
 * numeric port specifications.
 */
int net_service_lookup(char *service);

/*
 * Look up the local hostname; return value needs freeing.
 * May return NULL.
 */
char *get_hostname(void);

/*
 * Trivial socket implementation which just stores an error. Found in
 * errsock.c.
 */
Socket new_error_socket(const char *errmsg, Plug plug);

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
