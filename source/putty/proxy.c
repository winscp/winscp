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

/*
 * Call this when proxy negotiation is complete, so that this
 * socket can begin working normally.
 */
void proxy_activate (ProxySocket *p)
{
    size_t output_before, output_after;
    
    p->state = PROXY_STATE_ACTIVE;

    /* we want to ignore new receive events until we have sent
     * all of our buffered receive data.
     */
    sk_set_frozen(p->sub_socket, true);

    /* how many bytes of output have we buffered? */
    output_before = bufchain_size(&p->pending_oob_output_data) +
	bufchain_size(&p->pending_output_data);
    /* and keep track of how many bytes do not get sent. */
    output_after = 0;
    
    /* send buffered OOB writes */
    while (bufchain_size(&p->pending_oob_output_data) > 0) {
        ptrlen data = bufchain_prefix(&p->pending_oob_output_data);
	output_after += sk_write_oob(p->sub_socket, data.ptr, data.len);
	bufchain_consume(&p->pending_oob_output_data, data.len);
    }

    /* send buffered normal writes */
    while (bufchain_size(&p->pending_output_data) > 0) {
	ptrlen data = bufchain_prefix(&p->pending_output_data);
	output_after += sk_write(p->sub_socket, data.ptr, data.len);
	bufchain_consume(&p->pending_output_data, data.len);
    }

    /* if we managed to send any data, let the higher levels know. */
    if (output_after < output_before)
        plug_sent(p->plug, output_after);

    /* if we were asked to flush the output during
     * the proxy negotiation process, do so now.
     */
    if (p->pending_flush) sk_flush(p->sub_socket);

    /* if we have a pending EOF to send, send it */
    if (p->pending_eof) sk_write_eof(p->sub_socket);

    /* if the backend wanted the socket unfrozen, try to unfreeze.
     * our set_frozen handler will flush buffered receive data before
     * unfreezing the actual underlying socket.
     */
    if (!p->freeze)
	sk_set_frozen(&p->sock, 0);
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

    sk_close(ps->sub_socket);
    sk_addr_free(ps->remote_addr);
    sfree(ps);
}

static size_t sk_proxy_write (Socket *s, const void *data, size_t len)
{
    ProxySocket *ps = container_of(s, ProxySocket, sock);

    if (ps->state != PROXY_STATE_ACTIVE) {
	bufchain_add(&ps->pending_output_data, data, len);
	return bufchain_size(&ps->pending_output_data);
    }
    return sk_write(ps->sub_socket, data, len);
}

static size_t sk_proxy_write_oob (Socket *s, const void *data, size_t len)
{
    ProxySocket *ps = container_of(s, ProxySocket, sock);

    if (ps->state != PROXY_STATE_ACTIVE) {
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

    if (ps->state != PROXY_STATE_ACTIVE) {
        ps->pending_eof = true;
	return;
    }
    sk_write_eof(ps->sub_socket);
}

static void sk_proxy_flush (Socket *s)
{
    ProxySocket *ps = container_of(s, ProxySocket, sock);

    if (ps->state != PROXY_STATE_ACTIVE) {
	ps->pending_flush = true;
	return;
    }
    sk_flush(ps->sub_socket);
}

static void sk_proxy_set_frozen (Socket *s, bool is_frozen)
{
    ProxySocket *ps = container_of(s, ProxySocket, sock);

    if (ps->state != PROXY_STATE_ACTIVE) {
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

static const char * sk_proxy_socket_error (Socket *s)
{
    ProxySocket *ps = container_of(s, ProxySocket, sock);
    if (ps->error != NULL || ps->sub_socket == NULL) {
	return ps->error;
    }
    return sk_socket_error(ps->sub_socket);
}

/* basic proxy plug functions */

static void plug_proxy_log(Plug *plug, int type, SockAddr *addr, int port,
			   const char *error_msg, int error_code)
{
    ProxySocket *ps = container_of(plug, ProxySocket, plugimpl);

    plug_log(ps->plug, type, addr, port, error_msg, error_code);
}

static void plug_proxy_closing (Plug *p, const char *error_msg,
				int error_code, bool calling_back)
{
    ProxySocket *ps = container_of(p, ProxySocket, plugimpl);

    if (ps->state != PROXY_STATE_ACTIVE) {
	ps->closing_error_msg = error_msg;
	ps->closing_error_code = error_code;
	ps->closing_calling_back = calling_back;
	ps->negotiate(ps, PROXY_CHANGE_CLOSING);
    } else {
        plug_closing(ps->plug, error_msg, error_code, calling_back);
    }
}

static void plug_proxy_receive(
    Plug *p, int urgent, const char *data, size_t len)
{
    ProxySocket *ps = container_of(p, ProxySocket, plugimpl);

    if (ps->state != PROXY_STATE_ACTIVE) {
	/* we will lose the urgentness of this data, but since most,
	 * if not all, of this data will be consumed by the negotiation
	 * process, hopefully it won't affect the protocol above us
	 */
	bufchain_add(&ps->pending_input_data, data, len);
	ps->receive_urgent = (urgent != 0);
	ps->receive_data = data;
	ps->receive_len = len;
	ps->negotiate(ps, PROXY_CHANGE_RECEIVE);
    } else {
        plug_receive(ps->plug, urgent, data, len);
    }
}

static void plug_proxy_sent (Plug *p, size_t bufsize)
{
    ProxySocket *ps = container_of(p, ProxySocket, plugimpl);

    if (ps->state != PROXY_STATE_ACTIVE) {
	ps->negotiate(ps, PROXY_CHANGE_SENT);
	return;
    }
    plug_sent(ps->plug, bufsize);
}

static int plug_proxy_accepting(Plug *p,
                                accept_fn_t constructor, accept_ctx_t ctx)
{
    ProxySocket *ps = container_of(p, ProxySocket, plugimpl);

    if (ps->state != PROXY_STATE_ACTIVE) {
	ps->accepting_constructor = constructor;
	ps->accepting_ctx = ctx;
	return ps->negotiate(ps, PROXY_CHANGE_ACCEPTING);
    }
    return plug_accepting(ps->plug, constructor, ctx);
}

/*
 * This function can accept a NULL pointer as `addr', in which case
 * it will only check the host name.
 */
bool proxy_for_destination (SockAddr *addr, const char *hostname,
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
	hostip_len = 0;		       /* placate gcc; shouldn't be required */

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

static const struct SocketVtable ProxySocket_sockvt = {
    sk_proxy_plug,
    sk_proxy_close,
    sk_proxy_write,
    sk_proxy_write_oob,
    sk_proxy_write_eof,
    sk_proxy_flush,
    sk_proxy_set_frozen,
    sk_proxy_socket_error,
    NULL, /* peer_info */
};

static const struct PlugVtable ProxySocket_plugvt = {
    plug_proxy_log,
    plug_proxy_closing,
    plug_proxy_receive,
    plug_proxy_sent,
    plug_proxy_accepting
};

Socket *new_connection(SockAddr *addr, const char *hostname,
                       int port, bool privport,
                       bool oobinline, bool nodelay, bool keepalive,
                       Plug *plug, Conf *conf)
{
    if (conf_get_int(conf, CONF_proxy_type) != PROXY_NONE &&
	proxy_for_destination(addr, hostname, port, conf))
    {
	ProxySocket *ret;
	SockAddr *proxy_addr;
	char *proxy_canonical_name;
        const char *proxy_type;
	Socket *sret;
	int type;

	if ((sret = platform_new_connection(addr, hostname, port, privport,
					    oobinline, nodelay, keepalive,
					    plug, conf)) !=
	    NULL)
	    return sret;

	ret = snew(ProxySocket);
	ret->sock.vt = &ProxySocket_sockvt;
	ret->plugimpl.vt = &ProxySocket_plugvt;
	ret->conf = conf_copy(conf);
	ret->plug = plug;
	ret->remote_addr = addr;       /* will need to be freed on close */
	ret->remote_port = port;

	ret->error = NULL;
	ret->pending_flush = false;
	ret->pending_eof = false;
	ret->freeze = false;

	bufchain_init(&ret->pending_input_data);
	bufchain_init(&ret->pending_output_data);
	bufchain_init(&ret->pending_oob_output_data);

	ret->sub_socket = NULL;
	ret->state = PROXY_STATE_NEW;
	ret->negotiate = NULL;

	type = conf_get_int(conf, CONF_proxy_type);
	if (type == PROXY_HTTP) {
	    ret->negotiate = proxy_http_negotiate;
            proxy_type = "HTTP";
	} else if (type == PROXY_SOCKS4) {
            ret->negotiate = proxy_socks4_negotiate;
            proxy_type = "SOCKS 4";
	} else if (type == PROXY_SOCKS5) {
            ret->negotiate = proxy_socks5_negotiate;
            proxy_type = "SOCKS 5";
	} else if (type == PROXY_TELNET) {
	    ret->negotiate = proxy_telnet_negotiate;
            proxy_type = "Telnet";
	} else {
	    ret->error = "Proxy error: Unknown proxy method";
	    return &ret->sock;
	}

        {
            char *logmsg = dupprintf("Will use %s proxy at %s:%d to connect"
                                      " to %s:%d", proxy_type,
                                      conf_get_str(conf, CONF_proxy_host),
                                      conf_get_int(conf, CONF_proxy_port),
                                      hostname, port);
            plug_log(plug, 2, NULL, 0, logmsg, 0);
            sfree(logmsg);
        }

        {
            char *logmsg = dns_log_msg(conf_get_str(conf, CONF_proxy_host),
                                       conf_get_int(conf, CONF_addressfamily),
                                       "proxy");
            plug_log(plug, 2, NULL, 0, logmsg, 0);
            sfree(logmsg);
        }

	/* look-up proxy */
	proxy_addr = sk_namelookup(conf_get_str(conf, CONF_proxy_host),
				   &proxy_canonical_name,
				   conf_get_int(conf, CONF_addressfamily));
	if (sk_addr_error(proxy_addr) != NULL) {
	    ret->error = "Proxy error: Unable to resolve proxy host name";
            sk_addr_free(proxy_addr);
	    return &ret->sock;
	}
	sfree(proxy_canonical_name);

        {
            char addrbuf[256], *logmsg;
            sk_getaddr(proxy_addr, addrbuf, lenof(addrbuf));
            logmsg = dupprintf("Connecting to %s proxy at %s port %d",
                               proxy_type, addrbuf,
                               conf_get_int(conf, CONF_proxy_port));
            plug_log(plug, 2, NULL, 0, logmsg, 0);
            sfree(logmsg);
        }

	/* create the actual socket we will be using,
	 * connected to our proxy server and port.
	 */
	ret->sub_socket = sk_new(proxy_addr,
				 conf_get_int(conf, CONF_proxy_port),
				 privport, oobinline,
				 nodelay, keepalive, &ret->plugimpl);
	if (sk_socket_error(ret->sub_socket) != NULL)
	    return &ret->sock;

	/* start the proxy negotiation process... */
	sk_set_frozen(ret->sub_socket, 0);
	ret->negotiate(ret, PROXY_CHANGE_NEW);

	return &ret->sock;
    }

    /* no proxy, so just return the direct socket */
    return sk_new(addr, port, privport, oobinline, nodelay, keepalive, plug);
}

Socket *new_listener(const char *srcaddr, int port, Plug *plug,
                     bool local_host_only, Conf *conf, int addressfamily)
{
    /* TODO: SOCKS (and potentially others) support inbound
     * TODO: connections via the proxy. support them.
     */

    return sk_newlistener(srcaddr, port, plug, local_host_only, addressfamily);
}

/* ----------------------------------------------------------------------
 * HTTP CONNECT proxy type.
 */

static bool get_line_end(char *data, size_t len, size_t *out)
{
    size_t off = 0;

    while (off < len)
    {
	if (data[off] == '\n') {
	    /* we have a newline */
	    off++;

	    /* is that the only thing on this line? */
            if (off <= 2) {
                *out = off;
                return true;
            }

	    /* if not, then there is the possibility that this header
	     * continues onto the next line, if it starts with a space
	     * or a tab.
	     */

            if (off + 1 < len && data[off+1] != ' ' && data[off+1] != '\t') {
                *out = off;
                return true;
            }

	    /* the line does continue, so we have to keep going
	     * until we see an the header's "real" end of line.
	     */
	    off++;
	}

	off++;
    }

    return false;
}

int proxy_http_negotiate (ProxySocket *p, int change)
{
    if (p->state == PROXY_STATE_NEW) {
	/* we are just beginning the proxy negotiate process,
	 * so we'll send off the initial bits of the request.
	 * for this proxy method, it's just a simple HTTP
	 * request
	 */
	char *buf, dest[512];
	char *username, *password;

	sk_getaddr(p->remote_addr, dest, lenof(dest));

	buf = dupprintf("CONNECT %s:%i HTTP/1.1\r\nHost: %s:%i\r\n",
			dest, p->remote_port, dest, p->remote_port);
	sk_write(p->sub_socket, buf, strlen(buf));
	sfree(buf);

	username = conf_get_str(p->conf, CONF_proxy_username);
	password = conf_get_str(p->conf, CONF_proxy_password);
	if (username[0] || password[0]) {
	    char *buf, *buf2;
	    int i, j, len;
	    buf = dupprintf("%s:%s", username, password);
	    len = strlen(buf);
	    buf2 = snewn(len * 4 / 3 + 100, char);
	    sprintf(buf2, "Proxy-Authorization: Basic ");
	    for (i = 0, j = strlen(buf2); i < len; i += 3, j += 4)
		base64_encode_atom((unsigned char *)(buf+i),
				   (len-i > 3 ? 3 : len-i), buf2+j);
	    strcpy(buf2+j, "\r\n");
	    sk_write(p->sub_socket, buf2, strlen(buf2));
	    sfree(buf);
	    sfree(buf2);
	}

	sk_write(p->sub_socket, "\r\n", 2);

	p->state = 1;
	return 0;
    }

    if (change == PROXY_CHANGE_CLOSING) {
	/* if our proxy negotiation process involves closing and opening
	 * new sockets, then we would want to intercept this closing
	 * callback when we were expecting it. if we aren't anticipating
	 * a socket close, then some error must have occurred. we'll
	 * just pass those errors up to the backend.
	 */
	plug_closing(p->plug, p->closing_error_msg, p->closing_error_code,
		     p->closing_calling_back);
	return 0; /* ignored */
    }

    if (change == PROXY_CHANGE_SENT) {
	/* some (or all) of what we wrote to the proxy was sent.
	 * we don't do anything new, however, until we receive the
	 * proxy's response. we might want to set a timer so we can
	 * timeout the proxy negotiation after a while...
	 */
	return 0;
    }

    if (change == PROXY_CHANGE_ACCEPTING) {
	/* we should _never_ see this, as we are using our socket to
	 * connect to a proxy, not accepting inbound connections.
	 * what should we do? close the socket with an appropriate
	 * error message?
	 */
	return plug_accepting(p->plug,
                              p->accepting_constructor, p->accepting_ctx);
    }

    if (change == PROXY_CHANGE_RECEIVE) {
	/* we have received data from the underlying socket, which
	 * we'll need to parse, process, and respond to appropriately.
	 */

	char *data, *datap;
	size_t len, eol;

	if (p->state == 1) {

	    int min_ver, maj_ver, status;

	    /* get the status line */
	    len = bufchain_size(&p->pending_input_data);
	    assert(len > 0);	       /* or we wouldn't be here */
	    data = snewn(len+1, char);
	    bufchain_fetch(&p->pending_input_data, data, len);
	    /*
	     * We must NUL-terminate this data, because Windows
	     * sscanf appears to require a NUL at the end of the
	     * string because it strlens it _first_. Sigh.
	     */
	    data[len] = '\0';

            if (!get_line_end(data, len, &eol)) {
		sfree(data);
		return 1;
	    }

	    status = -1;
	    /* We can't rely on whether the %n incremented the sscanf return */
	    if (sscanf((char *)data, "HTTP/%i.%i %n",
		       &maj_ver, &min_ver, &status) < 2 || status == -1) {
		plug_closing(p->plug, "Proxy error: HTTP response was absent",
			     PROXY_ERROR_GENERAL, 0);
		sfree(data);
		return 1;
	    }

	    /* remove the status line from the input buffer. */
	    bufchain_consume(&p->pending_input_data, eol);
	    if (data[status] != '2') {
		/* error */
		char *buf;
		data[eol] = '\0';
		while (eol > status &&
		       (data[eol-1] == '\r' || data[eol-1] == '\n'))
		    data[--eol] = '\0';
		buf = dupprintf("Proxy error: %s", data+status);
		plug_closing(p->plug, buf, PROXY_ERROR_GENERAL, 0);
		sfree(buf);
		sfree(data);
		return 1;
	    }

	    sfree(data);

	    p->state = 2;
	}

	if (p->state == 2) {

	    /* get headers. we're done when we get a
	     * header of length 2, (ie. just "\r\n")
	     */

	    len = bufchain_size(&p->pending_input_data);
	    assert(len > 0);	       /* or we wouldn't be here */
	    data = snewn(len, char);
	    datap = data;
	    bufchain_fetch(&p->pending_input_data, data, len);

            if (!get_line_end(datap, len, &eol)) {
		sfree(data);
		return 1;
	    }
	    while (eol > 2) {
		bufchain_consume(&p->pending_input_data, eol);
		datap += eol;
		len   -= eol;
                if (!get_line_end(datap, len, &eol))
                    eol = 0;           /* terminate the loop */
	    }

	    if (eol == 2) {
		/* we're done */
		bufchain_consume(&p->pending_input_data, 2);
		proxy_activate(p);
		/* proxy activate will have dealt with
		 * whatever is left of the buffer */
		sfree(data);
		return 1;
	    }

	    sfree(data);
	    return 1;
	}
    }

    plug_closing(p->plug, "Proxy error: unexpected proxy error",
		 PROXY_ERROR_UNEXPECTED, 0);
    return 1;
}

/* ----------------------------------------------------------------------
 * SOCKS proxy type.
 */

/* SOCKS version 4 */
int proxy_socks4_negotiate (ProxySocket *p, int change)
{
    if (p->state == PROXY_CHANGE_NEW) {

	/* request format:
	 *  version number (1 byte) = 4
	 *  command code (1 byte)
	 *    1 = CONNECT
	 *    2 = BIND
	 *  dest. port (2 bytes) [network order]
	 *  dest. address (4 bytes)
	 *  user ID (variable length, null terminated string)
	 */

        strbuf *command = strbuf_new();
        char hostname[512];
        bool write_hostname = false;

        put_byte(command, 4);          /* SOCKS version 4 */
        put_byte(command, 1);          /* CONNECT command */
        put_uint16(command, p->remote_port);

	switch (sk_addrtype(p->remote_addr)) {
          case ADDRTYPE_IPV4:
            {
                char addr[4];
                sk_addrcopy(p->remote_addr, addr);
                put_data(command, addr, 4);
                break;
            }
          case ADDRTYPE_NAME:
            sk_getaddr(p->remote_addr, hostname, lenof(hostname));
            put_uint32(command, 1);
            write_hostname = true;
            break;
          case ADDRTYPE_IPV6:
            p->error = "Proxy error: SOCKS version 4 does not support IPv6";
            strbuf_free(command);
            return 1;
	}

        put_asciz(command, conf_get_str(p->conf, CONF_proxy_username));
        if (write_hostname)
            put_asciz(command, hostname);
	sk_write(p->sub_socket, command->s, command->len);
	strbuf_free(command);

	p->state = 1;
	return 0;
    }

    if (change == PROXY_CHANGE_CLOSING) {
	/* if our proxy negotiation process involves closing and opening
	 * new sockets, then we would want to intercept this closing
	 * callback when we were expecting it. if we aren't anticipating
	 * a socket close, then some error must have occurred. we'll
	 * just pass those errors up to the backend.
	 */
	plug_closing(p->plug, p->closing_error_msg, p->closing_error_code,
		     p->closing_calling_back);
	return 0; /* ignored */
    }

    if (change == PROXY_CHANGE_SENT) {
	/* some (or all) of what we wrote to the proxy was sent.
	 * we don't do anything new, however, until we receive the
	 * proxy's response. we might want to set a timer so we can
	 * timeout the proxy negotiation after a while...
	 */
	return 0;
    }

    if (change == PROXY_CHANGE_ACCEPTING) {
	/* we should _never_ see this, as we are using our socket to
	 * connect to a proxy, not accepting inbound connections.
	 * what should we do? close the socket with an appropriate
	 * error message?
	 */
	return plug_accepting(p->plug,
                              p->accepting_constructor, p->accepting_ctx);
    }

    if (change == PROXY_CHANGE_RECEIVE) {
	/* we have received data from the underlying socket, which
	 * we'll need to parse, process, and respond to appropriately.
	 */

	if (p->state == 1) {
	    /* response format:
	     *  version number (1 byte) = 4
	     *  reply code (1 byte)
	     *    90 = request granted
	     *    91 = request rejected or failed
	     *    92 = request rejected due to lack of IDENTD on client
	     *    93 = request rejected due to difference in user ID 
	     *         (what we sent vs. what IDENTD said)
	     *  dest. port (2 bytes)
	     *  dest. address (4 bytes)
	     */

	    char data[8];

	    if (bufchain_size(&p->pending_input_data) < 8)
		return 1;	       /* not got anything yet */
	    
	    /* get the response */
	    bufchain_fetch(&p->pending_input_data, data, 8);

	    if (data[0] != 0) {
		plug_closing(p->plug, "Proxy error: SOCKS proxy responded with "
				      "unexpected reply code version",
			     PROXY_ERROR_GENERAL, 0);
		return 1;
	    }

	    if (data[1] != 90) {

		switch (data[1]) {
		  case 92:
		    plug_closing(p->plug, "Proxy error: SOCKS server wanted IDENTD on client",
				 PROXY_ERROR_GENERAL, 0);
		    break;
		  case 93:
		    plug_closing(p->plug, "Proxy error: Username and IDENTD on client don't agree",
				 PROXY_ERROR_GENERAL, 0);
		    break;
		  case 91:
		  default:
		    plug_closing(p->plug, "Proxy error: Error while communicating with proxy",
				 PROXY_ERROR_GENERAL, 0);
		    break;
		}

		return 1;
	    }
	    bufchain_consume(&p->pending_input_data, 8);

	    /* we're done */
	    proxy_activate(p);
	    /* proxy activate will have dealt with
	     * whatever is left of the buffer */
	    return 1;
	}
    }

    plug_closing(p->plug, "Proxy error: unexpected proxy error",
		 PROXY_ERROR_UNEXPECTED, 0);
    return 1;
}

/* SOCKS version 5 */
int proxy_socks5_negotiate (ProxySocket *p, int change)
{
    if (p->state == PROXY_CHANGE_NEW) {

	/* initial command:
	 *  version number (1 byte) = 5
	 *  number of available authentication methods (1 byte)
	 *  available authentication methods (1 byte * previous value)
	 *    authentication methods:
	 *     0x00 = no authentication
	 *     0x01 = GSSAPI
	 *     0x02 = username/password
	 *     0x03 = CHAP
	 */

	strbuf *command;
	char *username, *password;
        int method_count_offset, methods_start;

        command = strbuf_new();
	put_byte(command, 5);          /* SOCKS version 5 */
	username = conf_get_str(p->conf, CONF_proxy_username);
	password = conf_get_str(p->conf, CONF_proxy_password);

        method_count_offset = command->len;
        put_byte(command, 0);
        methods_start = command->len;

        put_byte(command, 0x00);       /* no authentication */

	if (username[0] || password[0]) {
	    proxy_socks5_offerencryptedauth(BinarySink_UPCAST(command));
            put_byte(command, 0x02);    /* username/password */
	}

        command->u[method_count_offset] = command->len - methods_start;

	sk_write(p->sub_socket, command->s, command->len);
        strbuf_free(command);

	p->state = 1;
	return 0;
    }

    if (change == PROXY_CHANGE_CLOSING) {
	/* if our proxy negotiation process involves closing and opening
	 * new sockets, then we would want to intercept this closing
	 * callback when we were expecting it. if we aren't anticipating
	 * a socket close, then some error must have occurred. we'll
	 * just pass those errors up to the backend.
	 */
        plug_closing(p->plug, p->closing_error_msg, p->closing_error_code,
		     p->closing_calling_back);
	return 0; /* ignored */
    }

    if (change == PROXY_CHANGE_SENT) {
	/* some (or all) of what we wrote to the proxy was sent.
	 * we don't do anything new, however, until we receive the
	 * proxy's response. we might want to set a timer so we can
	 * timeout the proxy negotiation after a while...
	 */
	return 0;
    }

    if (change == PROXY_CHANGE_ACCEPTING) {
	/* we should _never_ see this, as we are using our socket to
	 * connect to a proxy, not accepting inbound connections.
	 * what should we do? close the socket with an appropriate
	 * error message?
	 */
	return plug_accepting(p->plug,
                              p->accepting_constructor, p->accepting_ctx);
    }

    if (change == PROXY_CHANGE_RECEIVE) {
	/* we have received data from the underlying socket, which
	 * we'll need to parse, process, and respond to appropriately.
	 */

	if (p->state == 1) {

	    /* initial response:
	     *  version number (1 byte) = 5
	     *  authentication method (1 byte)
	     *    authentication methods:
	     *     0x00 = no authentication
	     *     0x01 = GSSAPI
	     *     0x02 = username/password
	     *     0x03 = CHAP
	     *     0xff = no acceptable methods
	     */
	    char data[2];

	    if (bufchain_size(&p->pending_input_data) < 2)
		return 1;	       /* not got anything yet */

	    /* get the response */
	    bufchain_fetch(&p->pending_input_data, data, 2);

	    if (data[0] != 5) {
		plug_closing(p->plug, "Proxy error: SOCKS proxy returned unexpected version",
			     PROXY_ERROR_GENERAL, 0);
		return 1;
	    }

	    if (data[1] == 0x00) p->state = 2; /* no authentication needed */
	    else if (data[1] == 0x01) p->state = 4; /* GSSAPI authentication */
	    else if (data[1] == 0x02) p->state = 5; /* username/password authentication */
	    else if (data[1] == 0x03) p->state = 6; /* CHAP authentication */
	    else {
		plug_closing(p->plug, "Proxy error: SOCKS proxy did not accept our authentication",
			     PROXY_ERROR_GENERAL, 0);
		return 1;
	    }
	    bufchain_consume(&p->pending_input_data, 2);
	}

	if (p->state == 7) {

	    /* password authentication reply format:
	     *  version number (1 bytes) = 1
	     *  reply code (1 byte)
	     *    0 = succeeded
	     *    >0 = failed
	     */
	    char data[2];

	    if (bufchain_size(&p->pending_input_data) < 2)
		return 1;	       /* not got anything yet */

	    /* get the response */
	    bufchain_fetch(&p->pending_input_data, data, 2);

	    if (data[0] != 1) {
		plug_closing(p->plug, "Proxy error: SOCKS password "
			     "subnegotiation contained wrong version number",
			     PROXY_ERROR_GENERAL, 0);
		return 1;
	    }

	    if (data[1] != 0) {

		plug_closing(p->plug, "Proxy error: SOCKS proxy refused"
			     " password authentication",
			     PROXY_ERROR_GENERAL, 0);
		return 1;
	    }

	    bufchain_consume(&p->pending_input_data, 2);
	    p->state = 2;	       /* now proceed as authenticated */
	}

	if (p->state == 8) {
	    int ret;
	    ret = proxy_socks5_handlechap(p);
	    if (ret) return ret;
	}

	if (p->state == 2) {

	    /* request format:
	     *  version number (1 byte) = 5
	     *  command code (1 byte)
	     *    1 = CONNECT
	     *    2 = BIND
	     *    3 = UDP ASSOCIATE
	     *  reserved (1 byte) = 0x00
	     *  address type (1 byte)
	     *    1 = IPv4
	     *    3 = domainname (first byte has length, no terminating null)
	     *    4 = IPv6
	     *  dest. address (variable)
	     *  dest. port (2 bytes) [network order]
	     */

	    strbuf *command = strbuf_new();
	    put_byte(command, 5);      /* SOCKS version 5 */
	    put_byte(command, 1);      /* CONNECT command */
	    put_byte(command, 0x00);   /* reserved byte */

	    switch (sk_addrtype(p->remote_addr)) {
              case ADDRTYPE_IPV4:
		put_byte(command, 1);  /* IPv4 */
		sk_addrcopy(p->remote_addr, strbuf_append(command, 4));
                break;
              case ADDRTYPE_IPV6:
		put_byte(command, 4);  /* IPv6 */
		sk_addrcopy(p->remote_addr, strbuf_append(command, 16));
                break;
              case ADDRTYPE_NAME:
                {
                    char hostname[512];
                    put_byte(command, 3);  /* domain name */
                    sk_getaddr(p->remote_addr, hostname, lenof(hostname));
                    if (!put_pstring(command, hostname)) {
                        p->error = "Proxy error: SOCKS 5 cannot "
                            "support host names longer than 255 chars";
                        strbuf_free(command);
                        return 1;
                    }
                }
                break;
	    }

            put_uint16(command, p->remote_port);

	    sk_write(p->sub_socket, command->s, command->len);

            strbuf_free(command);

	    p->state = 3;
	    return 1;
	}

	if (p->state == 3) {

	    /* reply format:
	     *  version number (1 bytes) = 5
	     *  reply code (1 byte)
	     *    0 = succeeded
	     *    1 = general SOCKS server failure
	     *    2 = connection not allowed by ruleset
	     *    3 = network unreachable
	     *    4 = host unreachable
	     *    5 = connection refused
	     *    6 = TTL expired
	     *    7 = command not supported
	     *    8 = address type not supported
	     * reserved (1 byte) = x00
	     * address type (1 byte)
	     *    1 = IPv4
	     *    3 = domainname (first byte has length, no terminating null)
	     *    4 = IPv6
	     * server bound address (variable)
	     * server bound port (2 bytes) [network order]
	     */
	    char data[5];
	    int len;

	    /* First 5 bytes of packet are enough to tell its length. */ 
	    if (bufchain_size(&p->pending_input_data) < 5)
		return 1;	       /* not got anything yet */

	    /* get the response */
	    bufchain_fetch(&p->pending_input_data, data, 5);

	    if (data[0] != 5) {
		plug_closing(p->plug, "Proxy error: SOCKS proxy returned wrong version number",
			     PROXY_ERROR_GENERAL, 0);
		return 1;
	    }

	    if (data[1] != 0) {
		char buf[256];

		strcpy(buf, "Proxy error: ");

		switch (data[1]) {
		  case 1: strcat(buf, "General SOCKS server failure"); break;
		  case 2: strcat(buf, "Connection not allowed by ruleset"); break;
		  case 3: strcat(buf, "Network unreachable"); break;
		  case 4: strcat(buf, "Host unreachable"); break;
		  case 5: strcat(buf, "Connection refused"); break;
		  case 6: strcat(buf, "TTL expired"); break;
		  case 7: strcat(buf, "Command not supported"); break;
		  case 8: strcat(buf, "Address type not supported"); break;
		  default: sprintf(buf+strlen(buf),
				   "Unrecognised SOCKS error code %d",
				   data[1]);
		    break;
		}
		plug_closing(p->plug, buf, PROXY_ERROR_GENERAL, 0);

		return 1;
	    }

	    /*
	     * Eat the rest of the reply packet.
	     */
	    len = 6;		       /* first 4 bytes, last 2 */
	    switch (data[3]) {
	      case 1: len += 4; break; /* IPv4 address */
	      case 4: len += 16; break;/* IPv6 address */
	      case 3: len += (unsigned char)data[4]; break; /* domain name */
	      default:
		plug_closing(p->plug, "Proxy error: SOCKS proxy returned "
			     "unrecognised address format",
			     PROXY_ERROR_GENERAL, 0);
		return 1;
	    }
	    if (bufchain_size(&p->pending_input_data) < len)
		return 1;	       /* not got whole reply yet */
	    bufchain_consume(&p->pending_input_data, len);

	    /* we're done */
	    proxy_activate(p);
	    return 1;
	}

	if (p->state == 4) {
	    /* TODO: Handle GSSAPI authentication */
	    plug_closing(p->plug, "Proxy error: We don't support GSSAPI authentication",
			 PROXY_ERROR_GENERAL, 0);
	    return 1;
	}

	if (p->state == 5) {
            const char *username = conf_get_str(p->conf, CONF_proxy_username);
            const char *password = conf_get_str(p->conf, CONF_proxy_password);
	    if (username[0] || password[0]) {
                strbuf *auth = strbuf_new_nm();
		put_byte(auth, 1); /* version number of subnegotiation */
                if (!put_pstring(auth, username)) {
                    p->error = "Proxy error: SOCKS 5 authentication cannot "
                        "support usernames longer than 255 chars";
                    strbuf_free(auth);
                    return 1;
                }
                if (!put_pstring(auth, password)) {
                    p->error = "Proxy error: SOCKS 5 authentication cannot "
                        "support passwords longer than 255 chars";
                    strbuf_free(auth);
                    return 1;
                }
		sk_write(p->sub_socket, auth->s, auth->len);
                strbuf_free(auth);
		p->state = 7;
	    } else 
		plug_closing(p->plug, "Proxy error: Server chose "
			     "username/password authentication but we "
			     "didn't offer it!",
			 PROXY_ERROR_GENERAL, 0);
	    return 1;
	}

	if (p->state == 6) {
	    int ret;
	    ret = proxy_socks5_selectchap(p);
	    if (ret) return ret;
	}

    }

    plug_closing(p->plug, "Proxy error: Unexpected proxy error",
		 PROXY_ERROR_UNEXPECTED, 0);
    return 1;
}

/* ----------------------------------------------------------------------
 * `Telnet' proxy type.
 *
 * (This is for ad-hoc proxies where you connect to the proxy's
 * telnet port and send a command such as `connect host port'. The
 * command is configurable, since this proxy type is typically not
 * standardised or at all well-defined.)
 */

char *format_telnet_command(SockAddr *addr, int port, Conf *conf)
{
    char *fmt = conf_get_str(conf, CONF_proxy_telnet_command);
    int so = 0, eo = 0;
    strbuf *buf = strbuf_new();

    /* we need to escape \\, \%, \r, \n, \t, \x??, \0???, 
     * %%, %host, %port, %user, and %pass
     */

    while (fmt[eo] != 0) {

	/* scan forward until we hit end-of-line,
	 * or an escape character (\ or %) */
	while (fmt[eo] != 0 && fmt[eo] != '%' && fmt[eo] != '\\')
	    eo++;

	/* if we hit eol, break out of our escaping loop */
	if (fmt[eo] == 0) break;

	/* if there was any unescaped text before the escape
	 * character, send that now */
	if (eo != so)
            put_data(buf, fmt + so, eo - so);

	so = eo++;

	/* if the escape character was the last character of
	 * the line, we'll just stop and send it. */
	if (fmt[eo] == 0) break;

	if (fmt[so] == '\\') {

	    /* we recognize \\, \%, \r, \n, \t, \x??.
	     * anything else, we just send unescaped (including the \).
	     */

	    switch (fmt[eo]) {

	      case '\\':
		put_byte(buf, '\\');
		eo++;
		break;

	      case '%':
                put_byte(buf, '%');
		eo++;
		break;

	      case 'r':
                put_byte(buf, '\r');
		eo++;
		break;

	      case 'n':
                put_byte(buf, '\n');
		eo++;
		break;

	      case 't':
                put_byte(buf, '\t');
		eo++;
		break;

	      case 'x':
	      case 'X':
		{
		    /* escaped hexadecimal value (ie. \xff) */
		    unsigned char v = 0;
		    int i = 0;

		    for (;;) {
			eo++;
			if (fmt[eo] >= '0' && fmt[eo] <= '9')
			    v += fmt[eo] - '0';
			else if (fmt[eo] >= 'a' && fmt[eo] <= 'f')
			    v += fmt[eo] - 'a' + 10;
			else if (fmt[eo] >= 'A' && fmt[eo] <= 'F')
			    v += fmt[eo] - 'A' + 10;
			else {
			    /* non hex character, so we abort and just
			     * send the whole thing unescaped (including \x)
			     */
                            put_byte(buf, '\\');
			    eo = so + 1;
			    break;
			}

			/* we only extract two hex characters */
			if (i == 1) {
                            put_byte(buf, v);
			    eo++;
			    break;
			}

			i++;
			v <<= 4;
		    }
		}
		break;

	      default:
                put_data(buf, fmt + so, 2);
		eo++;
		break;
	    }
	} else {

	    /* % escape. we recognize %%, %host, %port, %user, %pass.
	     * %proxyhost, %proxyport. Anything else we just send
	     * unescaped (including the %).
	     */

	    if (fmt[eo] == '%') {
                put_byte(buf, '%');
		eo++;
	    }
	    else if (strnicmp(fmt + eo, "host", 4) == 0) {
		char dest[512];
		sk_getaddr(addr, dest, lenof(dest));
		put_data(buf, dest, strlen(dest));
		eo += 4;
	    }
	    else if (strnicmp(fmt + eo, "port", 4) == 0) {
                strbuf_catf(buf, "%d", port);
		eo += 4;
	    }
	    else if (strnicmp(fmt + eo, "user", 4) == 0) {
		const char *username = conf_get_str(conf, CONF_proxy_username);
		put_data(buf, username, strlen(username));
		eo += 4;
	    }
	    else if (strnicmp(fmt + eo, "pass", 4) == 0) {
		const char *password = conf_get_str(conf, CONF_proxy_password);
		put_data(buf, password, strlen(password));
		eo += 4;
	    }
	    else if (strnicmp(fmt + eo, "proxyhost", 9) == 0) {
		const char *host = conf_get_str(conf, CONF_proxy_host);
		put_data(buf, host, strlen(host));
		eo += 9;
	    }
	    else if (strnicmp(fmt + eo, "proxyport", 9) == 0) {
		int port = conf_get_int(conf, CONF_proxy_port);
                strbuf_catf(buf, "%d", port);
		eo += 9;
	    }
	    else {
		/* we don't escape this, so send the % now, and
		 * don't advance eo, so that we'll consider the
		 * text immediately following the % as unescaped.
		 */
                put_byte(buf, '%');
	    }
	}

	/* resume scanning for additional escapes after this one. */
	so = eo;
    }

    /* if there is any unescaped text at the end of the line, send it */
    if (eo != so) {
	put_data(buf, fmt + so, eo - so);
    }

    return strbuf_to_str(buf);
}

int proxy_telnet_negotiate (ProxySocket *p, int change)
{
    if (p->state == PROXY_CHANGE_NEW) {
	char *formatted_cmd;

	formatted_cmd = format_telnet_command(p->remote_addr, p->remote_port,
					      p->conf);

        {
            /*
             * Re-escape control chars in the command, for logging.
             */
            char *reescaped = snewn(4*strlen(formatted_cmd) + 1, char);
            const char *in;
            char *out;
            char *logmsg;

            for (in = formatted_cmd, out = reescaped; *in; in++) {
                if (*in == '\n') {
                    *out++ = '\\'; *out++ = 'n';
                } else if (*in == '\r') {
                    *out++ = '\\'; *out++ = 'r';
                } else if (*in == '\t') {
                    *out++ = '\\'; *out++ = 't';
                } else if (*in == '\\') {
                    *out++ = '\\'; *out++ = '\\';
                } else if ((unsigned)(((unsigned char)*in) - 0x20) <
                           (0x7F-0x20)) {
                    *out++ = *in;
                } else {
                    out += sprintf(out, "\\x%02X", (unsigned)*in & 0xFF);
                }
            }
            *out = '\0';

            logmsg = dupprintf("Sending Telnet proxy command: %s", reescaped);
            plug_log(p->plug, 2, NULL, 0, logmsg, 0);
            sfree(logmsg);
            sfree(reescaped);
        }

	sk_write(p->sub_socket, formatted_cmd, strlen(formatted_cmd));
	sfree(formatted_cmd);

	p->state = 1;
	return 0;
    }

    if (change == PROXY_CHANGE_CLOSING) {
	/* if our proxy negotiation process involves closing and opening
	 * new sockets, then we would want to intercept this closing
	 * callback when we were expecting it. if we aren't anticipating
	 * a socket close, then some error must have occurred. we'll
	 * just pass those errors up to the backend.
	 */
	plug_closing(p->plug, p->closing_error_msg, p->closing_error_code,
		     p->closing_calling_back);
	return 0; /* ignored */
    }

    if (change == PROXY_CHANGE_SENT) {
	/* some (or all) of what we wrote to the proxy was sent.
	 * we don't do anything new, however, until we receive the
	 * proxy's response. we might want to set a timer so we can
	 * timeout the proxy negotiation after a while...
	 */
	return 0;
    }

    if (change == PROXY_CHANGE_ACCEPTING) {
	/* we should _never_ see this, as we are using our socket to
	 * connect to a proxy, not accepting inbound connections.
	 * what should we do? close the socket with an appropriate
	 * error message?
	 */
	return plug_accepting(p->plug,
                              p->accepting_constructor, p->accepting_ctx);
    }

    if (change == PROXY_CHANGE_RECEIVE) {
	/* we have received data from the underlying socket, which
	 * we'll need to parse, process, and respond to appropriately.
	 */

	/* we're done */
	proxy_activate(p);
	/* proxy activate will have dealt with
	 * whatever is left of the buffer */
	return 1;
    }

    plug_closing(p->plug, "Proxy error: Unexpected proxy error",
		 PROXY_ERROR_UNEXPECTED, 0);
    return 1;
}
