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
    void (*set_frozen) (Socket *s, bool is_frozen);
    /* ignored by tcp, but vital for ssl */
    const char *(*socket_error) (Socket *s);
    SocketEndpointInfo *(*endpoint_info) (Socket *s, bool peer);
};

typedef union { void *p; int i; } accept_ctx_t;
typedef Socket *(*accept_fn_t)(accept_ctx_t ctx, Plug *plug);

struct Plug {
    const struct PlugVtable *vt;
};

typedef enum PlugLogType {
    PLUGLOG_CONNECT_TRYING,
    PLUGLOG_CONNECT_FAILED,
    PLUGLOG_CONNECT_SUCCESS,
    PLUGLOG_PROXY_MSG,
} PlugLogType;

typedef enum PlugCloseType {
    PLUGCLOSE_NORMAL,
    PLUGCLOSE_ERROR,
    PLUGCLOSE_BROKEN_PIPE,
    PLUGCLOSE_USER_ABORT,
} PlugCloseType;

struct PlugVtable {
    /*
     * Passes the client progress reports on the process of setting
     * up the connection.
     *
     *  - PLUGLOG_CONNECT_TRYING means we are about to try to connect
     *    to address `addr' (error_msg and error_code are ignored)
     *
     *  - PLUGLOG_CONNECT_FAILED means we have failed to connect to
     *    address `addr' (error_msg and error_code are supplied). This
     *    is not a fatal error - we may well have other candidate
     *    addresses to fall back to. When it _is_ fatal, the closing()
     *    function will be called.
     *
     *  - PLUGLOG_CONNECT_SUCCESS means we have succeeded in making a
     *    connection. `addr' gives the address we connected to, if
     *    available. (But sometimes, in cases of complicated proxy
     *    setups, it might not be available, so receivers of this log
     *    event should be prepared to deal with addr==NULL.)
     *
     *  - PLUGLOG_PROXY_MSG means that error_msg contains a line of
     *    logging information from whatever the connection is being
     *    proxied through. This will typically be a wodge of
     *    standard-error output from a local proxy command, so the
     *    receiver should probably prefix it to indicate this.
     *
     * Note that sometimes log messages may be sent even to Socket
     * types that don't involve making an outgoing connection, e.g.
     * because the same core implementation (such as Windows handle
     * sockets) is shared between listening and connecting sockets. So
     * all Plugs must implement this method, even if only to ignore
     * the logged events.
     */
    void (*log)(Plug *p, Socket *s, PlugLogType type, SockAddr *addr, int port,
                const char *error_msg, int error_code);

    /*
     * Notifies the Plug that the socket is closing, and something
     * about why.
     *
     *  - PLUGCLOSE_NORMAL means an ordinary non-error closure. In
     *    this case, error_msg should be ignored (and hopefully
     *    callers will have passed NULL).
     *
     *  - PLUGCLOSE_ERROR indicates that an OS error occurred, and
     *    'error_msg' contains a string describing it, for use in
     *    diagnostics. (Ownership of the string is not transferred.)
     *    This error class covers anything other than the special
     *    case below:
     *
     *  - PLUGCLOSE_BROKEN_PIPE behaves like PLUGCLOSE_ERROR (in
     *    particular, there's still an error message provided), but
     *    distinguishes the particular error condition signalled by
     *    EPIPE / ERROR_BROKEN_PIPE, which ssh/sharing.c needs to
     *    recognise and handle specially in one situation.
     *
     *  - PLUGCLOSE_USER_ABORT means that the close has happened as a
     *    result of some kind of deliberate user action (e.g. hitting
     *    ^C at a password prompt presented by a proxy socket setup
     *    phase). This can be used to suppress interactive error
     *    messages sent to the user (such as dialog boxes), on the
     *    grounds that the user already knows. However, 'error_msg'
     *    will still contain some appropriate text, so that
     *    non-interactive error reporting (e.g. event logs) can still
     *    record why the connection terminated.
     */
    void (*closing)(Plug *p, PlugCloseType type, const char *error_msg);

    /*
     * Provides incoming socket data to the Plug. Three cases:
     *
     *  - urgent==0. `data' points to `len' bytes of perfectly
     *    ordinary data.
     *
     *  - urgent==1. `data' points to `len' bytes of data,
     *    which were read from before an Urgent pointer.
     *
     *  - urgent==2. `data' points to `len' bytes of data,
     *    the first of which was the one at the Urgent mark.
     */
    void (*receive) (Plug *p, int urgent, const char *data, size_t len);

    /*
     * Called when the pending send backlog on a socket is cleared or
     * partially cleared. The new backlog size is passed in the
     * `bufsize' parameter.
     */
    void (*sent) (Plug *p, size_t bufsize);

    /*
     * Only called on listener-type sockets, and is passed a
     * constructor function+context that will create a fresh Socket
     * describing the connection. It returns nonzero if it doesn't
     * want the connection for some reason, or 0 on success.
     */
    int (*accepting)(Plug *p, accept_fn_t constructor, accept_ctx_t ctx);
};

/* Proxy indirection layer.
 *
 * Calling new_connection transfers ownership of 'addr': the proxy
 * layer is now responsible for freeing it, and the caller shouldn't
 * assume it exists any more.
 *
 * If calling this from a backend with a Seat, you can also give it a
 * pointer to the backend's Interactor trait. In that situation, it
 * might replace the backend's seat with a temporary seat of its own,
 * and give the real Seat to an Interactor somewhere in the proxy
 * system so that it can ask for passwords (and, in the case of SSH
 * proxying, other prompts like host key checks). If that happens,
 * then the resulting 'temp seat' is the backend's property, and it
 * will have to remember to free it when cleaning up, or after
 * flushing it back into the real seat when the network connection
 * attempt completes.
 *
 * You can free your TempSeat and resume using the real Seat when one
 * of two things happens: either your Plug's closing() method is
 * called (indicating failure to connect), or its log() method is
 * called with PLUGLOG_CONNECT_SUCCESS. In the latter case, you'll
 * probably want to flush the TempSeat's contents into the real Seat,
 * of course.
 */
Socket *new_connection(SockAddr *addr, const char *hostname,
                       int port, bool privport,
                       bool oobinline, bool nodelay, bool keepalive,
                       Plug *plug, Conf *conf, Interactor *interactor);
Socket *new_listener(const char *srcaddr, int port, Plug *plug,
                     bool local_host_only, Conf *conf, int addressfamily);
SockAddr *name_lookup(const char *host, int port, char **canonicalname,
                      Conf *conf, int addressfamily, LogContext *logctx,
                      const char *lookup_reason_for_logging);

/* platform-dependent callback from new_connection() */
/* (same caveat about addr as new_connection()) */
Socket *platform_new_connection(SockAddr *addr, const char *hostname,
                                int port, bool privport,
                                bool oobinline, bool nodelay, bool keepalive,
                                Plug *plug, Conf *conf, Interactor *itr);

/* callback for SSH jump-host proxying */
Socket *sshproxy_new_connection(SockAddr *addr, const char *hostname,
                                int port, bool privport,
                                bool oobinline, bool nodelay, bool keepalive,
                                Plug *plug, Conf *conf, Interactor *itr);

/* socket functions */

void sk_init(void);                    /* called once at program startup */
void sk_cleanup(void);                 /* called just before program exit */

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
               bool nodelay, bool keepalive, Plug *p,
#ifdef MPEXT
              int timeout,
              int sndbuf,
              const char *srcaddr
#endif
              );

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

#ifdef __cplusplus
#define WINSCP_ENUM_CAST(TYPE, EXPR) static_cast<TYPE>(EXPR)
#else
#define WINSCP_ENUM_CAST(TYPE, EXPR) (EXPR)
#endif
static inline void plug_log(
    Plug *p, Socket *s, int type, SockAddr *addr, int port,
    const char *msg, int code)
{ p->vt->log(p, s, WINSCP_ENUM_CAST(PlugLogType, type), addr, port, msg, code); }
static inline void plug_closing(Plug *p, PlugCloseType type, const char *msg)
{ p->vt->closing(p, type, msg); }
static inline void plug_closing_normal(Plug *p)
{ p->vt->closing(p, PLUGCLOSE_NORMAL, NULL); }
static inline void plug_closing_error(Plug *p, const char *msg)
{ p->vt->closing(p, PLUGCLOSE_ERROR, msg); }
static inline void plug_closing_user_abort(Plug *p)
{ p->vt->closing(p, PLUGCLOSE_USER_ABORT, "User aborted connection setup"); }
static inline void plug_receive(Plug *p, int urg, const char *data, size_t len)
{ p->vt->receive(p, urg, data, len); }
static inline void plug_sent (Plug *p, size_t bufsize)
{ p->vt->sent(p, bufsize); }
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
 * Return a structure giving some information about one end of
 * the socket. May be NULL, if nothing is available at all. If it is
 * not NULL, then it is dynamically allocated, and should be freed by
 * a call to sk_free_endpoint_info(). See below for the definition.
 */
static inline SocketEndpointInfo *sk_endpoint_info(Socket *s, bool peer)
{ return s->vt->endpoint_info(s, peer); }
static inline SocketEndpointInfo *sk_peer_info(Socket *s)
{ return sk_endpoint_info(s, true); }

/*
 * The structure returned from sk_endpoint_info, and a function to free
 * one (in utils).
 */
struct SocketEndpointInfo {
    int addressfamily;

    /*
     * Text form of the IPv4 or IPv6 address of the specified end of the
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
void sk_free_endpoint_info(SocketEndpointInfo *ei);

/*
 * Simple wrapper on getservbyname(), needed by portfwd.c. Returns the
 * port number, in host byte order (suitable for printf and so on).
 * Returns 0 on failure. Any platform not supporting getservbyname
 * can just return 0 - this function is not required to handle
 * numeric port specifications.
 */
int net_service_lookup(const char *service);

/*
 * Look up the local hostname; return value needs freeing.
 * May return NULL.
 */
char *get_hostname(void);

/*
 * Trivial socket implementation which just stores an error. Found in
 * errsock.c.
 *
 * The consume_string variant takes an already-formatted dynamically
 * allocated string, and takes over ownership of that string.
 */
Socket *new_error_socket_fmt(Plug *plug, const char *fmt, ...)
    PRINTF_LIKE(2, 3);
Socket *new_error_socket_consume_string(Plug *plug, char *errmsg);

/*
 * Trivial plug that does absolutely nothing. Found in nullplug.c.
 */
extern Plug *const nullplug;

/*
 * Some trivial no-op plug functions, also in nullplug.c; exposed here
 * so that other Plug implementations can use them too.
 *
 * In particular, nullplug_log is useful to Plugs that don't need to
 * worry about logging.
 */
void nullplug_log(Plug *plug, Socket *s, PlugLogType type, SockAddr *addr,
                  int port, const char *err_msg, int err_code);
void nullplug_closing(Plug *plug, PlugCloseType type, const char *error_msg);
void nullplug_receive(Plug *plug, int urgent, const char *data, size_t len);
void nullplug_sent(Plug *plug, size_t bufsize);

/*
 * Similar no-op socket function.
 */
SocketEndpointInfo *nullsock_endpoint_info(Socket *s, bool peer);

/* ----------------------------------------------------------------------
 * Functions defined outside the network code, which have to be
 * declared in this header file rather than the main putty.h because
 * they use types defined here.
 */

void backend_socket_log(Seat *seat, LogContext *logctx, Socket *sock,
                        PlugLogType type, SockAddr *addr, int port,
                        const char *error_msg, int error_code, Conf *conf,
                        bool session_started);

typedef struct ProxyStderrBuf {
    char buf[8192];
    size_t size;
    const char *prefix;                /* must be statically allocated */
} ProxyStderrBuf;
void psb_init(ProxyStderrBuf *psb);
void psb_set_prefix(ProxyStderrBuf *psb, const char *prefix);
void log_proxy_stderr(Plug *plug, Socket *sock, ProxyStderrBuf *psb,
                      const void *vdata, size_t len);

/* ----------------------------------------------------------------------
 * The DeferredSocketOpener trait. This is a thing that some Socket
 * implementations may choose to own if they need to delay actually
 * setting up the underlying connection. For example, sockets used in
 * local-proxy handling (Unix FdSocket / Windows HandleSocket) might
 * need to do this if they have to prompt the user interactively for
 * parts of the command they'll run.
 *
 * Mostly, a DeferredSocketOpener implementation will keep to itself,
 * arrange its own callbacks in order to do whatever setup it needs,
 * and when it's ready, call back to its parent Socket via some
 * implementation-specific API of its own. So the shared API here
 * requires almost nothing: the only thing we need is a free function,
 * so that if the owner of a Socket of this kind needs to close it
 * before the deferred connection process is finished, the Socket can
 * also clean up the DeferredSocketOpener dangling off it.
 */

struct DeferredSocketOpener {
    const DeferredSocketOpenerVtable *vt;
};
struct DeferredSocketOpenerVtable {
    void (*free)(DeferredSocketOpener *);
};
static inline void deferred_socket_opener_free(DeferredSocketOpener *dso)
{ dso->vt->free(dso); }

DeferredSocketOpener *null_deferred_socket_opener(void);

#endif
