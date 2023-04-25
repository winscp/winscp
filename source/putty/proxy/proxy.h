/*
 * Network proxy abstraction in PuTTY
 *
 * A proxy layer, if necessary, wedges itself between the
 * network code and the higher level backend.
 *
 * Supported proxies: HTTP CONNECT, generic telnet, SOCKS 4 & 5
 */

#ifndef PUTTY_PROXY_H
#define PUTTY_PROXY_H

typedef struct ProxySocket ProxySocket;
typedef struct ProxyNegotiator ProxyNegotiator;
typedef struct ProxyNegotiatorVT ProxyNegotiatorVT;

struct ProxySocket {
    const char *error;

    Socket *sub_socket;
    Plug *plug;
    SockAddr *remote_addr;
    int remote_port;

    /* Parameters needed to make further connections to the proxy */
    SockAddr *proxy_addr;
    int proxy_port;
    bool proxy_privport, proxy_oobinline, proxy_nodelay, proxy_keepalive;

    bufchain pending_output_data;
    bufchain pending_oob_output_data;
    bufchain pending_input_data;
    bool pending_eof;

    bool freeze; /* should we freeze the underlying socket when
                  * we are done with the proxy negotiation? this
                  * simply caches the value of sk_set_frozen calls.
                  */

    ProxyNegotiator *pn; /* non-NULL if still negotiating */
    bufchain output_from_negotiator;

    /* configuration, used to look up proxy settings */
    Conf *conf;

    /* for interaction with the Seat */
    Interactor *clientitr;
    LogPolicy *clientlp;
    Seat *clientseat;

    Socket sock;
    Plug plugimpl;
    Interactor interactor;
};

struct ProxyNegotiator {
    const ProxyNegotiatorVT *vt;

    /* Standard fields for any ProxyNegotiator. new() and free() don't
     * have to set these up; that's done centrally, to save duplication. */
    ProxySocket *ps;
    bufchain *input;
    bufchain_sink output[1];
    Interactor *itr; /* NULL if we are not able to interact with the user */

    /* Set to report success during proxy negotiation.  */
    bool done;

    /* Set to report an error during proxy negotiation. The main
     * ProxySocket will free it, and will then guarantee never to call
     * process_queue again. */
    char *error;

    /* Set to report user abort during proxy negotiation.  */
    bool aborted;

    /* Set to request the centralised code to make a fresh connection
     * to the proxy server, e.g. because an HTTP proxy slammed the
     * connection shut after sending 407 Proxy Auth Required. */
    bool reconnect;
};

struct ProxyNegotiatorVT {
    ProxyNegotiator *(*new)(const ProxyNegotiatorVT *);
    void (*process_queue)(ProxyNegotiator *);
    void (*free)(ProxyNegotiator *);
    const char *type;
};

static inline ProxyNegotiator *proxy_negotiator_new(
    const ProxyNegotiatorVT *vt)
{ return vt->new(vt); }
static inline void proxy_negotiator_process_queue(ProxyNegotiator *pn)
{ pn->vt->process_queue(pn); }
static inline void proxy_negotiator_free(ProxyNegotiator *pn)
{ pn->vt->free(pn); }

extern const ProxyNegotiatorVT http_proxy_negotiator_vt;
extern const ProxyNegotiatorVT socks4_proxy_negotiator_vt;
extern const ProxyNegotiatorVT socks5_proxy_negotiator_vt;
extern const ProxyNegotiatorVT telnet_proxy_negotiator_vt;

/*
 * Centralised functions to allow ProxyNegotiators to get hold of a
 * prompts_t, and to deal with SeatPromptResults coming back.
 */
prompts_t *proxy_new_prompts(ProxySocket *ps);
void proxy_spr_abort(ProxyNegotiator *pn, SeatPromptResult spr);

/*
 * This may be reused by local-command proxies on individual
 * platforms.
 */
#define TELNET_CMD_MISSING_USERNAME 0x0001
#define TELNET_CMD_MISSING_PASSWORD 0x0002
char *format_telnet_command(SockAddr *addr, int port, Conf *conf,
                            unsigned *flags_out);

DeferredSocketOpener *local_proxy_opener(
    SockAddr *addr, int port, Plug *plug, Conf *conf, Interactor *itr);
void local_proxy_opener_set_socket(DeferredSocketOpener *opener,
                                   Socket *socket);
char *platform_setup_local_proxy(Socket *socket, const char *cmd);

#include "cproxy.h"

#endif
