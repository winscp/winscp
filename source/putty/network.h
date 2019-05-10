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

#include "defs.h"

typedef struct SocketVtable SocketVtable;
typedef struct PlugVtable PlugVtable;

struct Socket {
    const struct SocketVtable *vt;
};

struct SocketVtable {
    Plug *(*plug) (Socket *s, Plug *p);
    /* use a different plug (return the old one) */
    /* if p is NULL, it doesn't change the plug */
    /* but it does return the one it's using */
    void (*close) (Socket *s);
    size_t (*write) (Socket *s, const void *data, size_t len);
    size_t (*write_oob) (Socket *s, const void *data, size_t len);
    void (*write_eof) (Socket *s);
    void (*flush) (Socket *s);
    void (*set_frozen) (Socket *s, bool is_frozen);
    /* ignored by tcp, but vital for ssl */
    const char *(*socket_error) (Socket *s);
    SocketPeerInfo *(*peer_info) (Socket *s);
};

typedef union { void *p; int i; } accept_ctx_t;
typedef Socket *(*accept_fn_t)(accept_ctx_t ctx, Plug *plug);

struct Plug {
    const struct PlugVtable *vt;
};

struct PlugVtable {
    void (*log)(Plug *p, int type, SockAddr *addr, int port,
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
     *  - type==2 means that error_msg contains a line of generic
     *    logging information about setting up the connection. This
     *    will typically be a wodge of standard-error output from a
     *    proxy command, so the receiver should probably prefix it to
     *    indicate this.
     */
    void (*closing)
     (Plug *p, const char *error_msg, int error_code, bool calling_back);
    /* error_msg is NULL iff it is not an error (ie it closed normally) */
    /* calling_back != 0 iff there is a Plug function */
    /* currently running (would cure the fixme in try_send()) */
    void (*receive) (Plug *p, int urgent, const char *data, size_t len);
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
    void (*sent) (Plug *p, size_t bufsize);
    /*
     * The `sent' function is called when the pending send backlog
     * on a socket is cleared or partially cleared. The new backlog
     * size is passed in the `bufsize' parameter.
     */
    int (*accepting)(Plug *p, accept_fn_t constructor, accept_ctx_t ctx);
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
Socket *new_connection(SockAddr *addr, const char *hostname,
                       int port, bool privport,
                       bool oobinline, bool nodelay, bool keepalive,
                       Plug *plug, Conf *conf);
Socket *new_listener(const char *srcaddr, int port, Plug *plug,
                     bool local_host_only, Conf *conf, int addressfamily);
SockAddr *name_lookup(const char *host, int port, char **canonicalname,
                      Conf *conf, int addressfamily, LogContext *logctx,
                      const char *lookup_reason_for_logging);
bool proxy_for_destination (SockAddr *addr, const char *hostname, int port,
                            Conf *conf);

/* platform-dependent callback from new_connection() */
/* (same caveat about addr as new_connection()) */
Socket *platform_new_connection(SockAddr *addr, const char *hostname,
                                int port, bool privport,
                                bool oobinline, bool nodelay, bool keepalive,
                                Plug *plug, Conf *conf);

/* socket functions */

void sk_init(void);		       /* called once at program startup */
void sk_cleanup(void);		       /* called just before program exit */

SockAddr *sk_namelookup(const char *host, char **canonicalname, int address_family);
SockAddr *sk_nonamelookup(const char *host);
void sk_getaddr(SockAddr *addr, char *buf, int buflen);
bool sk_addr_needs_port(SockAddr *addr);
bool sk_hostname_is_local(const char *name);
bool sk_address_is_local(SockAddr *addr);
bool sk_address_is_special_local(SockAddr *addr);
int sk_addrtype(SockAddr *addr);
void sk_addrcopy(SockAddr *addr, char *buf);
void sk_addr_free(SockAddr *addr);
/* sk_addr_dup generates another SockAddr which contains the same data
 * as the original one and can be freed independently. May not actually
 * physically _duplicate_ it: incrementing a reference count so that
 * one more free is required before it disappears is an acceptable
 * implementation. */
SockAddr *sk_addr_dup(SockAddr *addr);

/* NB, control of 'addr' is passed via sk_new, which takes responsibility
 * for freeing it, as for new_connection() */
Socket *sk_new(SockAddr *addr, int port, bool privport, bool oobinline,
               bool nodelay, bool keepalive, Plug *p);

Socket *sk_newlistener(const char *srcaddr, int port, Plug *plug,
                       bool local_host_only, int address_family);

static inline Plug *sk_plug(Socket *s, Plug *p)
{ return s->vt->plug(s, p); }
static inline void sk_close(Socket *s)
{ s->vt->close(s); }
static inline size_t sk_write(Socket *s, const void *data, size_t len)
{ return s->vt->write(s, data, len); }
static inline size_t sk_write_oob(Socket *s, const void *data, size_t len)
{ return s->vt->write_oob(s, data, len); }
static inline void sk_write_eof(Socket *s)
{ s->vt->write_eof(s); }
static inline void sk_flush(Socket *s)
{ s->vt->flush(s); }

static inline void plug_log(
    Plug *p, int type, SockAddr *addr, int port, const char *msg, int code)
{ return p->vt->log(p, type, addr, port, msg, code); }
static inline void plug_closing(
    Plug *p, const char *msg, int code, bool calling_back)
{ return p->vt->closing(p, msg, code, calling_back); }
static inline void plug_receive(Plug *p, int urg, const char *data, size_t len)
{ return p->vt->receive(p, urg, data, len); }
static inline void plug_sent (Plug *p, size_t bufsize)
{ return p->vt->sent(p, bufsize); }
static inline int plug_accepting(Plug *p, accept_fn_t cons, accept_ctx_t ctx)
{ return p->vt->accepting(p, cons, ctx); }

/*
 * Special error values are returned from sk_namelookup and sk_new
 * if there's a problem. These functions extract an error message,
 * or return NULL if there's no problem.
 */
const char *sk_addr_error(SockAddr *addr);
static inline const char *sk_socket_error(Socket *s)
{ return s->vt->socket_error(s); }

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
static inline void sk_set_frozen(Socket *s, bool is_frozen)
{ s->vt->set_frozen(s, is_frozen); }

/*
 * Return a structure giving some information about the other end of
 * the socket. May be NULL, if nothing is available at all. If it is
 * not NULL, then it is dynamically allocated, and should be freed by
 * a call to sk_free_peer_info(). See below for the definition.
 */
static inline SocketPeerInfo *sk_peer_info(Socket *s)
{ return s->vt->peer_info(s); }

/*
 * The structure returned from sk_peer_info, and a function to free
 * one (in misc.c).
 */
struct SocketPeerInfo {
    int addressfamily;

    /*
     * Text form of the IPv4 or IPv6 address of the other end of the
     * socket, if available, in the standard text representation.
     */
    const char *addr_text;

    /*
     * Binary form of the same address. Filled in if and only if
     * addr_text is not NULL. You can tell which branch of the union
     * is used by examining 'addressfamily'.
     */
    union {
        unsigned char ipv6[16];
        unsigned char ipv4[4];
    } addr_bin;

    /*
     * Remote port number, or -1 if not available.
     */
    int port;

    /*
     * Free-form text suitable for putting in log messages. For IP
     * sockets, repeats the address and port information from above.
     * But it can be completely different, e.g. for Unix-domain
     * sockets it gives information about the uid, gid and pid of the
     * connecting process.
     */
    const char *log_text;
};
void sk_free_peer_info(SocketPeerInfo *pi);

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
Socket *new_error_socket_fmt(Plug *plug, const char *fmt, ...);

/*
 * Trivial plug that does absolutely nothing. Found in nullplug.c.
 */
extern Plug *const nullplug;

/* ----------------------------------------------------------------------
 * Functions defined outside the network code, which have to be
 * declared in this header file rather than the main putty.h because
 * they use types defined here.
 */

/*
 * Exports from be_misc.c.
 */
void backend_socket_log(Seat *seat, LogContext *logctx,
                        int type, SockAddr *addr, int port,
                        const char *error_msg, int error_code, Conf *conf,
                        bool session_started);

typedef struct ProxyStderrBuf {
    char buf[8192];
    size_t size;
} ProxyStderrBuf;
void psb_init(ProxyStderrBuf *psb);
void log_proxy_stderr(
    Plug *plug, ProxyStderrBuf *psb, const void *vdata, size_t len);

#endif
