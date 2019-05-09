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

#define PROXY_ERROR_GENERAL 8000
#define PROXY_ERROR_UNEXPECTED 8001

typedef struct ProxySocket ProxySocket;

struct ProxySocket {
    const char *error;

    Socket *sub_socket;
    Plug *plug;
    SockAddr *remote_addr;
    int remote_port;

    bufchain pending_output_data;
    bufchain pending_oob_output_data;
    bool pending_flush;
    bufchain pending_input_data;
    bool pending_eof;

#define PROXY_STATE_NEW    -1
#define PROXY_STATE_ACTIVE  0

    int state; /* proxy states greater than 0 are implementation
		* dependent, but represent various stages/states
		* of the initialization/setup/negotiation with the
		* proxy server.
		*/
    bool freeze; /* should we freeze the underlying socket when
                  * we are done with the proxy negotiation? this
                  * simply caches the value of sk_set_frozen calls.
                  */

#define PROXY_CHANGE_NEW      -1
#define PROXY_CHANGE_CLOSING   0
#define PROXY_CHANGE_SENT      1
#define PROXY_CHANGE_RECEIVE   2
#define PROXY_CHANGE_ACCEPTING 3

    /* something has changed (a call from the sub socket
     * layer into our Proxy Plug layer, or we were just
     * created, etc), so the proxy layer needs to handle
     * this change (the type of which is the second argument)
     * and further the proxy negotiation process.
     */

    int (*negotiate) (ProxySocket * /* this */, int /* change type */);

    /* current arguments of plug handlers
     * (for use by proxy's negotiate function)
     */

    /* closing */
    const char *closing_error_msg;
    int closing_error_code;
    bool closing_calling_back;

    /* receive */
    bool receive_urgent;
    const char *receive_data;
    int receive_len;

    /* accepting */
    accept_fn_t accepting_constructor;
    accept_ctx_t accepting_ctx;

    /* configuration, used to look up proxy settings */
    Conf *conf;

    /* CHAP transient data */
    int chap_num_attributes;
    int chap_num_attributes_processed;
    int chap_current_attribute;
    int chap_current_datalen;

    Socket sock;
    Plug plugimpl;
};

extern void proxy_activate (ProxySocket *);

extern int proxy_http_negotiate (ProxySocket *, int);
extern int proxy_telnet_negotiate (ProxySocket *, int);
extern int proxy_socks4_negotiate (ProxySocket *, int);
extern int proxy_socks5_negotiate (ProxySocket *, int);

/*
 * This may be reused by local-command proxies on individual
 * platforms.
 */
char *format_telnet_command(SockAddr *addr, int port, Conf *conf);

/*
 * These are implemented in cproxy.c or nocproxy.c, depending on
 * whether encrypted proxy authentication is available.
 */
extern void proxy_socks5_offerencryptedauth(BinarySink *);
extern int proxy_socks5_handlechap (ProxySocket *);
extern int proxy_socks5_selectchap(ProxySocket *);

#endif
