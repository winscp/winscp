/*
 * Network proxy abstraction in PuTTY
 *
 * A proxy layer, if necessary, wedges itself between the network
 * code and the higher level backend.
 */

#include <assert.h>
#include <ctype.h>
#include <string.h>

#include "putty.h"
#include "network.h"
#include "proxy.h"

#define do_proxy_dns(conf) \
    (conf_get_int(conf, CONF_proxy_dns) == FORCE_ON || \
         (conf_get_int(conf, CONF_proxy_dns) == AUTO && \
              conf_get_int(conf, CONF_proxy_type) != PROXY_SOCKS4))

static void proxy_negotiator_cleanup(ProxySocket *ps)
{
    if (ps->pn) {
        proxy_negotiator_free(ps->pn);
        ps->pn = NULL;
    }
    if (ps->clientseat) {
        interactor_return_seat(ps->clientitr);
        ps->clientitr = NULL;
        ps->clientseat = NULL;
    }
}

/*
 * Call this when proxy negotiation is complete, so that this
 * socket can begin working normally.
 */
static void proxy_activate(ProxySocket *ps)
{
    size_t output_before, output_after;

    proxy_negotiator_cleanup(ps);

    plug_log(ps->plug, &ps->sock, PLUGLOG_CONNECT_SUCCESS, NULL, 0, NULL, 0);

    /* we want to ignore new receive events until we have sent
     * all of our buffered receive data.
     */
    sk_set_frozen(ps->sub_socket, true);

    /* how many bytes of output have we buffered? */
    output_before = bufchain_size(&ps->pending_oob_output_data) +
        bufchain_size(&ps->pending_output_data);
    /* and keep track of how many bytes do not get sent. */
    output_after = 0;

    /* send buffered OOB writes */
    while (bufchain_size(&ps->pending_oob_output_data) > 0) {
        ptrlen data = bufchain_prefix(&ps->pending_oob_output_data);
        output_after += sk_write_oob(ps->sub_socket, data.ptr, data.len);
        bufchain_consume(&ps->pending_oob_output_data, data.len);
    }

    /* send buffered normal writes */
    while (bufchain_size(&ps->pending_output_data) > 0) {
        ptrlen data = bufchain_prefix(&ps->pending_output_data);
        output_after += sk_write(ps->sub_socket, data.ptr, data.len);
        bufchain_consume(&ps->pending_output_data, data.len);
    }

    /* if we managed to send any data, let the higher levels know. */
    if (output_after < output_before)
        plug_sent(ps->plug, output_after);

    /* if we have a pending EOF to send, send it */
    if (ps->pending_eof) sk_write_eof(ps->sub_socket);

    /* if the backend wanted the socket unfrozen, try to unfreeze.
     * our set_frozen handler will flush buffered receive data before
     * unfreezing the actual underlying socket.
     */
    if (!ps->freeze)
        sk_set_frozen(&ps->sock, false);
}

/* basic proxy socket functions */

static Plug *sk_proxy_plug (Socket *s, Plug *p)
{
    ProxySocket *ps = container_of(s, ProxySocket, sock);
    Plug *ret = ps->plug;
    if (p)
        ps->plug = p;
    return ret;
}

static void sk_proxy_close (Socket *s)
{
    ProxySocket *ps = container_of(s, ProxySocket, sock);

    #ifdef WINSCP
    if (ps->sub_socket != NULL)
    #endif
    sk_close(ps->sub_socket);
    #ifdef WINSCP
    if (ps->proxy_addr != NULL)
    #endif
    sk_addr_free(ps->proxy_addr);
    sk_addr_free(ps->remote_addr);
    proxy_negotiator_cleanup(ps);
    bufchain_clear(&ps->output_from_negotiator);
    sfree(ps);
}

static size_t sk_proxy_write (Socket *s, const void *data, size_t len)
{
    ProxySocket *ps = container_of(s, ProxySocket, sock);

    if (ps->pn) {
        bufchain_add(&ps->pending_output_data, data, len);
        return bufchain_size(&ps->pending_output_data);
    }
    return sk_write(ps->sub_socket, data, len);
}

static size_t sk_proxy_write_oob (Socket *s, const void *data, size_t len)
{
    ProxySocket *ps = container_of(s, ProxySocket, sock);

    if (ps->pn) {
        bufchain_clear(&ps->pending_output_data);
        bufchain_clear(&ps->pending_oob_output_data);
        bufchain_add(&ps->pending_oob_output_data, data, len);
        return len;
    }
    return sk_write_oob(ps->sub_socket, data, len);
}

static void sk_proxy_write_eof (Socket *s)
{
    ProxySocket *ps = container_of(s, ProxySocket, sock);

    if (ps->pn) {
        ps->pending_eof = true;
        return;
    }
    sk_write_eof(ps->sub_socket);
}

static void sk_proxy_set_frozen (Socket *s, bool is_frozen)
{
    ProxySocket *ps = container_of(s, ProxySocket, sock);

    if (ps->pn) {
        ps->freeze = is_frozen;
        return;
    }

    /* handle any remaining buffered recv data first */
    if (bufchain_size(&ps->pending_input_data) > 0) {
        ps->freeze = is_frozen;

        /* loop while we still have buffered data, and while we are
         * unfrozen. the plug_receive call in the loop could result
         * in a call back into this function refreezing the socket,
         * so we have to check each time.
         */
        while (!ps->freeze && bufchain_size(&ps->pending_input_data) > 0) {
            char databuf[512];
            ptrlen data = bufchain_prefix(&ps->pending_input_data);
            if (data.len > lenof(databuf))
                data.len = lenof(databuf);
            memcpy(databuf, data.ptr, data.len);
            bufchain_consume(&ps->pending_input_data, data.len);
            plug_receive(ps->plug, 0, databuf, data.len);
        }

        /* if we're still frozen, we'll have to wait for another
         * call from the backend to finish unbuffering the data.
         */
        if (ps->freeze) return;
    }

    sk_set_frozen(ps->sub_socket, is_frozen);
}

static const char *sk_proxy_socket_error (Socket *s)
{
    ProxySocket *ps = container_of(s, ProxySocket, sock);
    if (ps->error != NULL || ps->sub_socket == NULL) {
        return ps->error;
    }
    return sk_socket_error(ps->sub_socket);
}

/* basic proxy plug functions */

static void plug_proxy_log(Plug *plug, Socket *s, PlugLogType type,
                           SockAddr *addr, int port,
                           const char *error_msg, int error_code)
{
    ProxySocket *ps = container_of(plug, ProxySocket, plugimpl);

    plug_log(ps->plug, &ps->sock, type, addr, port, error_msg, error_code);
}

static void plug_proxy_closing(Plug *p, PlugCloseType type,
                               const char *error_msg)
{
    ProxySocket *ps = container_of(p, ProxySocket, plugimpl);

    proxy_negotiator_cleanup(ps);
    plug_closing(ps->plug, type, error_msg);
}

static void proxy_negotiate(ProxySocket *ps)
{
    assert(ps->pn);
    proxy_negotiator_process_queue(ps->pn);

    if (ps->pn->error) {
        char *err = dupprintf("Proxy error: %s", ps->pn->error);
        sfree(ps->pn->error);
        proxy_negotiator_cleanup(ps);
        plug_closing_error(ps->plug, err);
        sfree(err);
        return;
    } else if (ps->pn->aborted) {
        proxy_negotiator_cleanup(ps);
        plug_closing_user_abort(ps->plug);
        return;
    }

    if (ps->pn->reconnect) {
        sk_close(ps->sub_socket);
        { // WINSCP
        SockAddr *proxy_addr = sk_addr_dup(ps->proxy_addr);
        ps->sub_socket = sk_new(proxy_addr, ps->proxy_port,
                                ps->proxy_privport, ps->proxy_oobinline,
                                ps->proxy_nodelay, ps->proxy_keepalive,
                                &ps->plugimpl,
                                #ifdef WINSCP
                                conf_get_int(ps->conf, CONF_connect_timeout), conf_get_int(ps->conf, CONF_sndbuf),
                                conf_get_str(ps->conf, CONF_srcaddr)
                                #endif
                                );
        ps->pn->reconnect = false;
        /* If the negotiator has asked us to reconnect, they are
         * expecting that on the next call their input queue will
         * consist entirely of data from the _new_ connection, without
         * any remaining data buffered from the old one. (If they'd
         * wanted the latter, they could have read it out of the input
         * queue before asking us to close the connection.) */
        bufchain_clear(&ps->pending_input_data);
        } // WINSCP
    }

    while (bufchain_size(&ps->output_from_negotiator)) {
        ptrlen data = bufchain_prefix(&ps->output_from_negotiator);
        sk_write(ps->sub_socket, data.ptr, data.len);
        bufchain_consume(&ps->output_from_negotiator, data.len);
    }
    if (ps->pn->done)
        proxy_activate(ps);
}

static void plug_proxy_receive(
    Plug *p, int urgent, const char *data, size_t len)
{
    ProxySocket *ps = container_of(p, ProxySocket, plugimpl);

    if (ps->pn) {
        /* we will lose the urgentness of this data, but since most,
         * if not all, of this data will be consumed by the negotiation
         * process, hopefully it won't affect the protocol above us
         */
        bufchain_add(&ps->pending_input_data, data, len);
        proxy_negotiate(ps);
    } else {
        plug_receive(ps->plug, urgent, data, len);
    }
}

static void plug_proxy_sent (Plug *p, size_t bufsize)
{
    ProxySocket *ps = container_of(p, ProxySocket, plugimpl);

    if (ps->pn)
        return;
    plug_sent(ps->plug, bufsize);
}

static int plug_proxy_accepting(Plug *p,
                                accept_fn_t constructor, accept_ctx_t ctx)
{
    unreachable("ProxySockets never create listening Sockets");
}

/*
 * This function can accept a NULL pointer as `addr', in which case
 * it will only check the host name.
 */
static bool proxy_for_destination(SockAddr *addr, const char *hostname,
                                  int port, Conf *conf)
{
    int s = 0, e = 0;
    char hostip[64];
    int hostip_len, hostname_len;
    const char *exclude_list;

    /*
     * Special local connections such as Unix-domain sockets
     * unconditionally cannot be proxied, even in proxy-localhost
     * mode. There just isn't any way to ask any known proxy type for
     * them.
     */
    if (addr && sk_address_is_special_local(addr))
        return false;                  /* do not proxy */

    /*
     * Check the host name and IP against the hard-coded
     * representations of `localhost'.
     */
    if (!conf_get_bool(conf, CONF_even_proxy_localhost) &&
        (sk_hostname_is_local(hostname) ||
         (addr && sk_address_is_local(addr))))
        return false;                  /* do not proxy */

    /* we want a string representation of the IP address for comparisons */
    if (addr) {
        sk_getaddr(addr, hostip, 64);
        hostip_len = strlen(hostip);
    } else
        hostip_len = 0;                /* placate gcc; shouldn't be required */

    hostname_len = strlen(hostname);

    exclude_list = conf_get_str(conf, CONF_proxy_exclude_list);

    /* now parse the exclude list, and see if either our IP
     * or hostname matches anything in it.
     */

    while (exclude_list[s]) {
        while (exclude_list[s] &&
               (isspace((unsigned char)exclude_list[s]) ||
                exclude_list[s] == ',')) s++;

        if (!exclude_list[s]) break;

        e = s;

        while (exclude_list[e] &&
               (isalnum((unsigned char)exclude_list[e]) ||
                exclude_list[e] == '-' ||
                exclude_list[e] == '.' ||
                exclude_list[e] == '*')) e++;

        if (exclude_list[s] == '*') {
            /* wildcard at beginning of entry */

            if ((addr && strnicmp(hostip + hostip_len - (e - s - 1),
                                  exclude_list + s + 1, e - s - 1) == 0) ||
                strnicmp(hostname + hostname_len - (e - s - 1),
                         exclude_list + s + 1, e - s - 1) == 0) {
                /* IP/hostname range excluded. do not use proxy. */
                return false;
            }
        } else if (exclude_list[e-1] == '*') {
            /* wildcard at end of entry */

            if ((addr && strnicmp(hostip, exclude_list + s, e - s - 1) == 0) ||
                strnicmp(hostname, exclude_list + s, e - s - 1) == 0) {
                /* IP/hostname range excluded. do not use proxy. */
                return false;
            }
        } else {
            /* no wildcard at either end, so let's try an absolute
             * match (ie. a specific IP)
             */

            if (addr && strnicmp(hostip, exclude_list + s, e - s) == 0)
                return false; /* IP/hostname excluded. do not use proxy. */
            if (strnicmp(hostname, exclude_list + s, e - s) == 0)
                return false; /* IP/hostname excluded. do not use proxy. */
        }

        s = e;

        /* Make sure we really have reached the next comma or end-of-string */
        while (exclude_list[s] &&
               !isspace((unsigned char)exclude_list[s]) &&
               exclude_list[s] != ',') s++;
    }

    /* no matches in the exclude list, so use the proxy */
    return true;
}

static char *dns_log_msg(const char *host, int addressfamily,
                         const char *reason)
{
    return dupprintf("Looking up host \"%s\"%s for %s", host,
                     (addressfamily == ADDRTYPE_IPV4 ? " (IPv4)" :
                      addressfamily == ADDRTYPE_IPV6 ? " (IPv6)" :
                      ""), reason);
}

SockAddr *name_lookup(const char *host, int port, char **canonicalname,
                      Conf *conf, int addressfamily, LogContext *logctx,
                      const char *reason)
{
    if (conf_get_int(conf, CONF_proxy_type) != PROXY_NONE &&
        do_proxy_dns(conf) &&
        proxy_for_destination(NULL, host, port, conf)) {

        if (logctx)
            logeventf(logctx, "Leaving host lookup to proxy of \"%s\""
                      " (for %s)", host, reason);

        *canonicalname = dupstr(host);
        return sk_nonamelookup(host);
    } else {
        if (logctx)
            logevent_and_free(
                logctx, dns_log_msg(host, addressfamily, reason));

        return sk_namelookup(host, canonicalname, addressfamily);
    }
}

static SocketEndpointInfo *sk_proxy_endpoint_info(Socket *s, bool peer)
{
    ProxySocket *ps = container_of(s, ProxySocket, sock);

    /* We can't reliably find out where we ended up connecting _to_:
     * that's at the far end of the proxy, and might be anything. */
    if (peer)
        return NULL;

    #ifdef WINSCP
    // if proxy is connected synchronously, we get here from sk_new even before ps->sub_socket is assigned
    if (ps->sub_socket == NULL) return NULL;
    #endif

    /* But we can at least tell where we're coming _from_. */
    return sk_endpoint_info(ps->sub_socket, false);
}

static const SocketVtable ProxySocket_sockvt = {
    // WINSCP
    /*.plug =*/ sk_proxy_plug,
    /*.close =*/ sk_proxy_close,
    /*.write =*/ sk_proxy_write,
    /*.write_oob =*/ sk_proxy_write_oob,
    /*.write_eof =*/ sk_proxy_write_eof,
    /*.set_frozen =*/ sk_proxy_set_frozen,
    /*.socket_error =*/ sk_proxy_socket_error,
    /*.endpoint_info =*/ sk_proxy_endpoint_info,
};

static const PlugVtable ProxySocket_plugvt = {
    // WINSCP
    /*.log =*/ plug_proxy_log,
    /*.closing =*/ plug_proxy_closing,
    /*.receive =*/ plug_proxy_receive,
    /*.sent =*/ plug_proxy_sent,
    /*.accepting =*/ plug_proxy_accepting
};

static char *proxy_description(Interactor *itr)
{
    ProxySocket *ps = container_of(itr, ProxySocket, interactor);
    assert(ps->pn);
    return dupprintf("%s connection to %s port %d", ps->pn->vt->type,
                     conf_get_str(ps->conf, CONF_proxy_host),
                     conf_get_int(ps->conf, CONF_proxy_port));
}

static LogPolicy *proxy_logpolicy(Interactor *itr)
{
    ProxySocket *ps = container_of(itr, ProxySocket, interactor);
    return ps->clientlp;
}

static Seat *proxy_get_seat(Interactor *itr)
{
    ProxySocket *ps = container_of(itr, ProxySocket, interactor);
    return ps->clientseat;
}

static void proxy_set_seat(Interactor *itr, Seat *seat)
{
    ProxySocket *ps = container_of(itr, ProxySocket, interactor);
    ps->clientseat = seat;
}

static const InteractorVtable ProxySocket_interactorvt = {
    // WINSCP
    /*.description =*/ proxy_description,
    /*.logpolicy =*/ proxy_logpolicy,
    /*.get_seat =*/ proxy_get_seat,
    /*.set_seat =*/ proxy_set_seat,
};

static void proxy_prompts_callback(void *ctx)
{
    proxy_negotiate((ProxySocket *)ctx);
}

prompts_t *proxy_new_prompts(ProxySocket *ps)
{
    prompts_t *prs = new_prompts();
    prs->callback = proxy_prompts_callback;
    prs->callback_ctx = ps;
    return prs;
}

void proxy_spr_abort(ProxyNegotiator *pn, SeatPromptResult spr)
{
    if (spr.kind == SPRK_SW_ABORT) {
        pn->error = spr_get_error_message(spr);
    } else {
        assert(spr.kind == SPRK_USER_ABORT);
        pn->aborted = true;
    }
}

Socket *new_connection(SockAddr *addr, const char *hostname,
                       int port, bool privport,
                       bool oobinline, bool nodelay, bool keepalive,
                       Plug *plug, Conf *conf, Interactor *itr)
{
    int type = conf_get_int(conf, CONF_proxy_type);

    if (type != PROXY_NONE &&
        proxy_for_destination(addr, hostname, port, conf)) {
        ProxySocket *ps;
        SockAddr *proxy_addr;
        char *proxy_canonical_name;
        Socket *sret;

        if ((type == PROXY_SSH_TCPIP ||
             type == PROXY_SSH_EXEC ||
             type == PROXY_SSH_SUBSYSTEM) &&
            (sret = sshproxy_new_connection(addr, hostname, port, privport,
                                            oobinline, nodelay, keepalive,
                                            plug, conf, itr)) != NULL)
            return sret;

        if ((sret = platform_new_connection(addr, hostname, port, privport,
                                            oobinline, nodelay, keepalive,
                                            plug, conf, itr)) != NULL)
            return sret;

        ps = snew(ProxySocket);
        ps->sock.vt = &ProxySocket_sockvt;
        ps->plugimpl.vt = &ProxySocket_plugvt;
        ps->interactor.vt = &ProxySocket_interactorvt;
        ps->conf = conf_copy(conf);
        ps->plug = plug;
        ps->remote_addr = addr;       /* will need to be freed on close */
        ps->remote_port = port;

        ps->error = NULL;
        ps->pending_eof = false;
        ps->freeze = false;

        bufchain_init(&ps->pending_input_data);
        bufchain_init(&ps->pending_output_data);
        bufchain_init(&ps->pending_oob_output_data);
        bufchain_init(&ps->output_from_negotiator);

        ps->sub_socket = NULL;

        #ifdef WINSCP
        ps->proxy_addr = NULL;
        #endif

        /*
         * If we've been given an Interactor by the caller, set ourselves
         * up to work with it.
         */
        if (itr) {
            ps->clientitr = itr;
            interactor_set_child(ps->clientitr, &ps->interactor);
            ps->clientlp = interactor_logpolicy(ps->clientitr);
            ps->clientseat = interactor_borrow_seat(ps->clientitr);
        }

        { // WINSCP
        const ProxyNegotiatorVT *vt;
        switch (type) {
          case PROXY_HTTP:
            vt = &http_proxy_negotiator_vt;
            break;
          case PROXY_SOCKS4:
            vt = &socks4_proxy_negotiator_vt;
            break;
          case PROXY_SOCKS5:
            vt = &socks5_proxy_negotiator_vt;
            break;
          case PROXY_TELNET:
            vt = &telnet_proxy_negotiator_vt;
            break;
          default:
            ps->error = "Proxy error: Unknown proxy method";
            return &ps->sock;
        }
        ps->pn = proxy_negotiator_new(vt);
        ps->pn->ps = ps;
        ps->pn->done = false;
        ps->pn->error = NULL;
        ps->pn->aborted = false;
        ps->pn->input = &ps->pending_input_data;
        /* Provide an Interactor to the negotiator if and only if we
         * are usefully able to ask interactive questions of the user */
        ps->pn->itr = ps->clientseat ? &ps->interactor : NULL;
        bufchain_sink_init(ps->pn->output, &ps->output_from_negotiator);

        {
            char *logmsg = dupprintf("Will use %s proxy at %s:%d to connect"
                                     " to %s:%d", vt->type,
                                     conf_get_str(conf, CONF_proxy_host),
                                     conf_get_int(conf, CONF_proxy_port),
                                     hostname, port);
            plug_log(plug, &ps->sock, PLUGLOG_PROXY_MSG, NULL, 0, logmsg, 0);
            sfree(logmsg);
        }

        {
            char *logmsg = dns_log_msg(conf_get_str(conf, CONF_proxy_host),
                                       conf_get_int(conf, CONF_addressfamily),
                                       "proxy");
            plug_log(plug, &ps->sock, PLUGLOG_PROXY_MSG, NULL, 0, logmsg, 0);
            sfree(logmsg);
        }

        /* look-up proxy */
        proxy_addr = sk_namelookup(conf_get_str(conf, CONF_proxy_host),
                                   &proxy_canonical_name,
                                   conf_get_int(conf, CONF_addressfamily));
        if (sk_addr_error(proxy_addr) != NULL) {
            ps->error = "Proxy error: Unable to resolve proxy host name";
            sk_addr_free(proxy_addr);
            return &ps->sock;
        }
        sfree(proxy_canonical_name);

        {
            char addrbuf[256], *logmsg;
            sk_getaddr(proxy_addr, addrbuf, lenof(addrbuf));
            logmsg = dupprintf("Connecting to %s proxy at %s port %d",
                               vt->type, addrbuf,
                               conf_get_int(conf, CONF_proxy_port));
            plug_log(plug, &ps->sock, PLUGLOG_PROXY_MSG, NULL, 0, logmsg, 0);
            sfree(logmsg);
        }

        /* create the actual socket we will be using,
         * connected to our proxy server and port.
         */
        ps->proxy_addr = sk_addr_dup(proxy_addr);
        ps->proxy_port = conf_get_int(conf, CONF_proxy_port);
        ps->proxy_privport = privport;
        ps->proxy_oobinline = oobinline;
        ps->proxy_nodelay = nodelay;
        ps->proxy_keepalive = keepalive;
        ps->sub_socket = sk_new(proxy_addr, ps->proxy_port,
                                ps->proxy_privport, ps->proxy_oobinline,
                                ps->proxy_nodelay, ps->proxy_keepalive,
                                &ps->plugimpl,
                                #ifdef WINSCP
                                conf_get_int(conf, CONF_connect_timeout), conf_get_int(conf, CONF_sndbuf),
                                conf_get_str(conf, CONF_srcaddr)
                                #endif
                                );
        if (sk_socket_error(ps->sub_socket) != NULL)
            return &ps->sock;

        /* start the proxy negotiation process... */
        sk_set_frozen(ps->sub_socket, false);
        proxy_negotiate(ps);

        return &ps->sock;
        } // WINSCP
    }

    /* no proxy, so just return the direct socket */
    return sk_new(addr, port, privport, oobinline, nodelay, keepalive, plug,
        #ifdef WINSCP
        conf_get_int(conf, CONF_connect_timeout), conf_get_int(conf, CONF_sndbuf),
        conf_get_str(conf, CONF_srcaddr)
        #endif
    );
}

Socket *new_listener(const char *srcaddr, int port, Plug *plug,
                     bool local_host_only, Conf *conf, int addressfamily)
{
    /* TODO: SOCKS (and potentially others) support inbound
     * TODO: connections via the proxy. support them.
     */

    return sk_newlistener(srcaddr, port, plug, local_host_only, addressfamily);
}

#ifdef WINSCP
ProxySocket * get_proxy_plug_socket(Plug * p)
{
    return container_of(p, ProxySocket, plugimpl);
}
#endif
