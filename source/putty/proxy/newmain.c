#include "putty.h"

typedef struct PreConnWrapper PreConnWrapper;
struct PreConnWrapper {
    /* The Plug from the client who instantiated us */
    Plug *plug;

    /* Initially non-NULL, until the pre-connect command finishes */
    Socket *preconn;
    SubprocessWaiter *waiter;

    /* Initially NULL, until we've run the pre-connect command */
    Socket *mainsocket;

    /* Used to collect stdout from the subprocess. (fd-socket collects
     * stderr) */
    ProxyStderrBuf psb;

    /* Pending stuff to pass on to the main socket once we open it */
    bufchain writebuf, oobbuf;
    bool frozen;
    bool pending_eof;

    /* Parameters for calling new_connection */
    SockAddr *addr;
    char *hostname;
    int port;
    bool privport, oobinline, nodelay, keepalive;
    Conf *conf;
    Interactor *itr;

    /* For logging the pre-connection command progress */
    LogContext *logctx;

    /* Our implementations of Plug and Socket */
    Socket sockimpl;
    Plug cmdplugimpl, mainplugimpl;
};

static void preconn_open_mainsocket(void *vctx)
{
    PreConnWrapper *wr = (PreConnWrapper *)vctx;
    assert(!wr->mainsocket);

    wr->mainsocket = new_connection(
        wr->addr, wr->hostname, wr->port, wr->privport, wr->oobinline,
        wr->nodelay, wr->keepalive, &wr->mainplugimpl, wr->conf, wr->itr);
    wr->addr = NULL; /* we've transferred ownership */

    sk_set_frozen(wr->mainsocket, wr->frozen);

    while (bufchain_size(&wr->oobbuf) > 0) {
        ptrlen data = bufchain_prefix(&wr->oobbuf);
        sk_write_oob(wr->mainsocket, data.ptr, data.len);
        bufchain_consume(&wr->oobbuf, data.len);
    }

    while (bufchain_size(&wr->writebuf) > 0) {
        ptrlen data = bufchain_prefix(&wr->writebuf);
        sk_write(wr->mainsocket, data.ptr, data.len);
        bufchain_consume(&wr->writebuf, data.len);
    }

    if (wr->pending_eof)
        sk_write_eof(wr->mainsocket);

    sfree(wr->hostname);
    wr->hostname = NULL;
    conf_free(wr->conf);
    wr->conf = NULL;
}

static void preconn_cmdplug_log(
    Plug *p, Socket *s, PlugLogType type, SockAddr *addr,
    int port, const char *error_msg, int error_code)
{
    PreConnWrapper *wr = container_of(p, PreConnWrapper, cmdplugimpl);

    switch (type) {
      case PLUGLOG_CONNECT_TRYING:
      case PLUGLOG_CONNECT_FAILED:
      case PLUGLOG_CONNECT_SUCCESS:
        /* Ignore these messages from the subcommand. We don't expect
         * to receive them anyway, and if we did, they'd be boring. */
        break;
      case PLUGLOG_PROXY_MSG:
        /* But this contains the stderr from the subcommand, so pass
         * it on. */
        plug_log(wr->plug, &wr->sockimpl, type, addr, port,
                 error_msg, error_code);
        break;

    }
}

static void preconn_cmdplug_closing(
    Plug *p, PlugCloseType type, const char *error_msg)
{
    PreConnWrapper *wr = container_of(p, PreConnWrapper, cmdplugimpl);

    switch (type) {
      case PLUGCLOSE_NORMAL:
        /* We don't really care when stdout from the subprocess
         * closes. Our trigger for moving on to the main connection is
         * the subprocess itself exiting. */
        break;

      case PLUGCLOSE_ERROR:
      case PLUGCLOSE_BROKEN_PIPE:
        /* But if it terminates _confusedly_, at least log that. */
        logeventf(wr->logctx, "Error reading output from "
                  "pre-connect command: %s", error_msg);
        break;

      case PLUGCLOSE_USER_ABORT:
        /* Hard to imagine how a subprocess socket might produce this,
         * but if it ever does grow a means of doing so, we should
         * interpret it as a request to abort the whole connection. */
        plug_closing(wr->plug, type, error_msg);
        break;
    }
}

static void preconn_subproc_terminated(
    void *vctx, int exittype, uint32_t exitdata)
{
    PreConnWrapper *wr = (PreConnWrapper *)vctx;
    switch (exittype) {
      case EXITTYPE_NORMAL:
        logeventf(wr->logctx, "Pre-connect command exited with status %"PRIu32,
                  exitdata);
        break;
      case EXITTYPE_SIGNAL:
        logeventf(wr->logctx, "Pre-connect command killed by signal %"PRIu32,
                  exitdata);
        break;
    }

    queue_toplevel_callback(preconn_open_mainsocket, wr);
}

static void preconn_cmdplug_receive(
    Plug *p, int urgent, const char *data, size_t len)
{
    PreConnWrapper *wr = container_of(p, PreConnWrapper, cmdplugimpl);
    log_proxy_stderr(wr->plug, &wr->sockimpl, &wr->psb, data, len);
}

static void preconn_cmdplug_sent(Plug *p, size_t bufsize)
{
}

static void preconn_mainplug_log(
    Plug *p, Socket *s, PlugLogType type, SockAddr *addr, int port,
    const char *error_msg, int error_code)
{
    PreConnWrapper *wr = container_of(p, PreConnWrapper, mainplugimpl);
    plug_log(wr->plug, &wr->sockimpl, type, addr, port,
             error_msg, error_code);
}

static void preconn_mainplug_closing(
    Plug *p, PlugCloseType type, const char *error_msg)
{
    PreConnWrapper *wr = container_of(p, PreConnWrapper, mainplugimpl);
    plug_closing(wr->plug, type, error_msg);
}

static void preconn_mainplug_receive(
    Plug *p, int urgent, const char *data, size_t len)
{
    PreConnWrapper *wr = container_of(p, PreConnWrapper, mainplugimpl);
    plug_receive(wr->plug, urgent, data, len);
}

static void preconn_mainplug_sent(Plug *p, size_t bufsize)
{
    PreConnWrapper *wr = container_of(p, PreConnWrapper, mainplugimpl);
    plug_sent(wr->plug, bufsize);
}

static int preconn_never_accepting(Plug *p, accept_fn_t constructor,
                                   accept_ctx_t ctx)
{
    unreachable("new_main_connection never creates a listening Socket");
}

static const PlugVtable preconn_cmd_plugvt = {
    .log = preconn_cmdplug_log,
    .closing = preconn_cmdplug_closing,
    .receive = preconn_cmdplug_receive,
    .sent = preconn_cmdplug_sent,
    .accepting = preconn_never_accepting,
};

static const PlugVtable preconn_main_plugvt = {
    .log = preconn_mainplug_log,
    .closing = preconn_mainplug_closing,
    .receive = preconn_mainplug_receive,
    .sent = preconn_mainplug_sent,
    .accepting = preconn_never_accepting,
};

static Plug *preconn_plug(Socket *s, Plug *p)
{
    PreConnWrapper *wr = container_of(s, PreConnWrapper, sockimpl);
    Plug *prev = wr->plug;
    if (p)
        wr->plug = p;
    return prev;
}

static void preconn_close(Socket *s)
{
    PreConnWrapper *wr = container_of(s, PreConnWrapper, sockimpl);
    if (wr->preconn)
        sk_close(wr->preconn);
    if (wr->waiter)
        subproc_waiter_free(wr->waiter);
    if (wr->mainsocket)
        sk_close(wr->mainsocket);
    if (wr->addr)
        sk_addr_free(wr->addr);
    if (wr->hostname)
        sfree(wr->hostname);
    if (wr->conf)
        conf_free(wr->conf);
    bufchain_clear(&wr->writebuf);
    bufchain_clear(&wr->oobbuf);
    delete_callbacks_for_context(wr);
    sfree(wr);
}

static size_t preconn_write(Socket *s, const void *data, size_t len)
{
    PreConnWrapper *wr = container_of(s, PreConnWrapper, sockimpl);
    if (wr->mainsocket)
        return sk_write(wr->mainsocket, data, len);

    bufchain_add(&wr->writebuf, data, len);
    return bufchain_size(&wr->writebuf);
}

static size_t preconn_write_oob(Socket *s, const void *data, size_t len)
{
    PreConnWrapper *wr = container_of(s, PreConnWrapper, sockimpl);
    if (wr->mainsocket)
        return sk_write_oob(wr->mainsocket, data, len);

    bufchain_clear(&wr->writebuf);
    bufchain_clear(&wr->oobbuf);
    bufchain_add(&wr->oobbuf, data, len);
    return bufchain_size(&wr->oobbuf);
}

static void preconn_write_eof(Socket *s)
{
    PreConnWrapper *wr = container_of(s, PreConnWrapper, sockimpl);
    if (wr->mainsocket) {
        sk_write_eof(wr->mainsocket);
    } else {
        wr->pending_eof = true;
    }
}

static void preconn_set_frozen(Socket *s, bool is_frozen)
{
    PreConnWrapper *wr = container_of(s, PreConnWrapper, sockimpl);
    if (wr->mainsocket) {
        sk_set_frozen(wr->mainsocket, is_frozen);
    } else {
        wr->frozen = is_frozen;
    }
}

static const char *preconn_socket_error(Socket *s)
{
    PreConnWrapper *wr = container_of(s, PreConnWrapper, sockimpl);
    if (wr->mainsocket)
        return sk_socket_error(wr->mainsocket);

    return NULL;
}

static SocketEndpointInfo *preconn_endpoint_info(Socket *s, bool peer)
{
    PreConnWrapper *wr = container_of(s, PreConnWrapper, sockimpl);
    if (wr->mainsocket)
        return sk_endpoint_info(wr->mainsocket, peer);

    return NULL;
}

static const SocketVtable preconn_sockvt = {
    .plug = preconn_plug,
    .close = preconn_close,
    .write = preconn_write,
    .write_oob = preconn_write_oob,
    .write_eof = preconn_write_eof,
    .set_frozen = preconn_set_frozen,
    .socket_error = preconn_socket_error,
    .endpoint_info = preconn_endpoint_info,
};

Socket *new_main_connection(
    SockAddr *addr, const char *hostname, int port, bool privport,
    bool oobinline, bool nodelay, bool keepalive, Plug *plug, Conf *conf,
    Interactor *itr, LogContext *logctx)
{
    char *command = NULL;

    const char *template = conf_get_str(conf, CONF_pre_connect_command);
    assert(template);
    if (*template) {
        Conf *tmpconf = conf_copy(conf);
        conf_set_str(conf, CONF_proxy_username, "");
        conf_set_str(conf, CONF_proxy_password, "");
        unsigned flags;
        command = format_connection_setup_command(
            template, addr, port, tmpconf, &flags);
        if (flags & (TELNET_CMD_MISSING_USERNAME |
                     TELNET_CMD_MISSING_PASSWORD)) {
            logeventf(logctx, "Pre-connect command: references to %%user or "
                      "%%pass not supported");
            sfree(command);
            command = NULL;
        }
        conf_free(tmpconf);
    }

    if (!command) {
        /* No pre-connection command: just open the main socket immediately */
        return new_connection(addr, hostname, port, privport, oobinline,
                              nodelay, keepalive, plug, conf, itr);
    }

    PreConnWrapper *wr = snew(PreConnWrapper);
    memset(wr, 0, sizeof(PreConnWrapper));
    wr->plug = plug;
    wr->addr = addr;                   /* we now own this */
    wr->hostname = dupstr(hostname);
    wr->port = port;
    wr->privport = privport;
    wr->oobinline = oobinline;
    wr->nodelay = nodelay;
    wr->keepalive = keepalive;
    wr->conf = conf_copy(conf);
    wr->itr = itr;
    wr->logctx = logctx;
    psb_init(&wr->psb);
    psb_set_prefix(&wr->psb, "pre-connect command stdout");
    bufchain_init(&wr->writebuf);
    bufchain_init(&wr->oobbuf);

    wr->cmdplugimpl.vt = &preconn_cmd_plugvt;
    wr->mainplugimpl.vt = &preconn_main_plugvt;
    wr->sockimpl.vt = &preconn_sockvt;

    logeventf(logctx, "Running pre-connect command: %s", command);
    wr->preconn = platform_start_subprocess(
        command, &wr->cmdplugimpl, "pre-connect command stderr", &wr->waiter);
    subproc_waiter_set_callback(wr->waiter, preconn_subproc_terminated, wr);
    sk_write_eof(wr->preconn);

    sfree(command);

    return &wr->sockimpl;
}
