/*
 * Network proxy abstraction in PuTTY
 *
 * A proxy layer, if necessary, wedges itself between the network
 * code and the higher level backend.
 */

#include <windows.h>

#include <assert.h>

#define DEFINE_PLUG_METHOD_MACROS
#include "putty.h"
#include "network.h"
#include "proxy.h"

/*
 * Call this when proxy negotiation is complete, so that this
 * socket can begin working normally.
 */
void proxy_activate (Proxy_Socket p)
{
    void *data;
    int len;

    p->state = PROXY_STATE_ACTIVE;

    /* let's try to keep extra receive events from coming through */
    sk_set_frozen(p->sub_socket, 1);

    /* send buffered OOB writes */
    while (bufchain_size(&p->pending_oob_output_data) > 0) {
	bufchain_prefix(&p->pending_oob_output_data, &data, &len);
	sk_write_oob(p->sub_socket, data, len);
	bufchain_consume(&p->pending_oob_output_data, len);
    }
    bufchain_clear(&p->pending_oob_output_data);

    /* send buffered normal writes */
    while (bufchain_size(&p->pending_output_data) > 0) {
	bufchain_prefix(&p->pending_output_data, &data, &len);
	sk_write(p->sub_socket, data, len);
	bufchain_consume(&p->pending_output_data, len);
    }
    bufchain_clear(&p->pending_output_data);

    /* if we were asked to flush the output during
     * the proxy negotiation process, do so now.
     */
    if (p->pending_flush) sk_flush(p->sub_socket);

    /* forward buffered recv data to the backend */
    while (bufchain_size(&p->pending_input_data) > 0) {
	bufchain_prefix(&p->pending_input_data, &data, &len);
	plug_receive(p->plug, 0, data, len);
	bufchain_consume(&p->pending_input_data, len);
    }
    bufchain_clear(&p->pending_input_data);

    /* now set the underlying socket to whatever freeze state they wanted */
    sk_set_frozen(p->sub_socket, p->freeze);
}

/* basic proxy socket functions */

static Plug sk_proxy_plug (Socket s, Plug p)
{
    Proxy_Socket ps = (Proxy_Socket) s;
    Plug ret = ps->plug;
    if (p)
	ps->plug = p;
    return ret;
}

static void sk_proxy_close (Socket s)
{
    Proxy_Socket ps = (Proxy_Socket) s;

    sk_close(ps->sub_socket);
    sfree(ps);
}

static int sk_proxy_write (Socket s, char *data, int len)
{
    Proxy_Socket ps = (Proxy_Socket) s;

    if (ps->state != PROXY_STATE_ACTIVE) {
	bufchain_add(&ps->pending_output_data, data, len);
	return bufchain_size(&ps->pending_output_data);
    }
    return sk_write(ps->sub_socket, data, len);
}

static int sk_proxy_write_oob (Socket s, char *data, int len)
{
    Proxy_Socket ps = (Proxy_Socket) s;

    if (ps->state != PROXY_STATE_ACTIVE) {
	bufchain_clear(&ps->pending_output_data);
	bufchain_clear(&ps->pending_oob_output_data);
	bufchain_add(&ps->pending_oob_output_data, data, len);
	return len;
    }
    return sk_write_oob(ps->sub_socket, data, len);
}

static void sk_proxy_flush (Socket s)
{
    Proxy_Socket ps = (Proxy_Socket) s;

    if (ps->state != PROXY_STATE_ACTIVE) {
	ps->pending_flush = 1;
	return;
    }
    sk_flush(ps->sub_socket);
}

static void sk_proxy_set_private_ptr (Socket s, void *ptr)
{
    Proxy_Socket ps = (Proxy_Socket) s;
    sk_set_private_ptr(ps->sub_socket, ptr);
}

static void * sk_proxy_get_private_ptr (Socket s)
{
    Proxy_Socket ps = (Proxy_Socket) s;
    return sk_get_private_ptr(ps->sub_socket);
}

static void sk_proxy_set_frozen (Socket s, int is_frozen)
{
    Proxy_Socket ps = (Proxy_Socket) s;

    if (ps->state != PROXY_STATE_ACTIVE) {
	ps->freeze = is_frozen;
	return;
    }
    sk_set_frozen(ps->sub_socket, is_frozen);
}

static char * sk_proxy_socket_error (Socket s)
{
    Proxy_Socket ps = (Proxy_Socket) s;
    if (ps->error != NULL || ps->sub_socket == NULL) {
	return ps->error;
    }
    return sk_socket_error(ps->sub_socket);
}

/* basic proxy plug functions */

static int plug_proxy_closing (Plug p, char *error_msg,
			       int error_code, int calling_back)
{
    Proxy_Plug pp = (Proxy_Plug) p;
    Proxy_Socket ps = pp->proxy_socket;

    if (ps->state != PROXY_STATE_ACTIVE) {
	ps->closing_error_msg = error_msg;
	ps->closing_error_code = error_code;
	ps->closing_calling_back = calling_back;
	return ps->negotiate(ps, PROXY_CHANGE_CLOSING);
    }
    return plug_closing(ps->plug, error_msg,
			error_code, calling_back);
}

static int plug_proxy_receive (Plug p, int urgent, char *data, int len)
{
    Proxy_Plug pp = (Proxy_Plug) p;
    Proxy_Socket ps = pp->proxy_socket;

    if (ps->state != PROXY_STATE_ACTIVE) {
	/* we will lose the urgentness of this data, but since most,
	 * if not all, of this data will be consumed by the negotiation
	 * process, hopefully it won't affect the protocol above us
	 */
	bufchain_add(&ps->pending_input_data, data, len);
	ps->receive_urgent = urgent;
	ps->receive_data = data;
	ps->receive_len = len;
	return ps->negotiate(ps, PROXY_CHANGE_RECEIVE);
    }
    return plug_receive(ps->plug, urgent, data, len);
}

static void plug_proxy_sent (Plug p, int bufsize)
{
    Proxy_Plug pp = (Proxy_Plug) p;
    Proxy_Socket ps = pp->proxy_socket;

    if (ps->state != PROXY_STATE_ACTIVE) {
	ps->sent_bufsize = bufsize;
	ps->negotiate(ps, PROXY_CHANGE_SENT);
	return;
    }
    plug_sent(ps->plug, bufsize);
}

static int plug_proxy_accepting (Plug p, void *sock)
{
    Proxy_Plug pp = (Proxy_Plug) p;
    Proxy_Socket ps = pp->proxy_socket;

    if (ps->state != PROXY_STATE_ACTIVE) {
	ps->accepting_sock = sock;
	return ps->negotiate(ps, PROXY_CHANGE_ACCEPTING);
    }
    return plug_accepting(ps->plug, sock);
}

static int proxy_for_destination (SockAddr addr, char * hostname, int port)
{
    int s = 0, e = 0;
    char hostip[64];
    int hostip_len, hostname_len;
    char * exclude_list;

    /* we want a string representation of the IP address for comparisons */
    sk_getaddr(addr, hostip, 64);

    hostip_len = strlen(hostip);
    hostname_len = strlen(hostname);

    exclude_list = cfg.proxy_exclude_list;

    /* now parse the exclude list, and see if either our IP
     * or hostname matches anything in it.
     */

    while (exclude_list[s]) {
	while (exclude_list[s] &&
	       (isspace(exclude_list[s]) ||
		exclude_list[s] == ',')) s++;

	if (!exclude_list[s]) break;

	e = s;

	while (exclude_list[e] &&
	       (isalnum(exclude_list[e]) ||
		exclude_list[e] == '-' ||
		exclude_list[e] == '.' ||
		exclude_list[e] == '*')) e++;

	if (exclude_list[s] == '*') {
	    /* wildcard at beginning of entry */

	    if (strnicmp(hostip + hostip_len - (e - s - 1),
			 exclude_list + s + 1, e - s - 1) == 0 ||
		strnicmp(hostname + hostname_len - (e - s - 1),
			 exclude_list + s + 1, e - s - 1) == 0)
		return 0; /* IP/hostname range excluded. do not use proxy. */

	} else if (exclude_list[e-1] == '*') {
	    /* wildcard at end of entry */

	    if (strnicmp(hostip, exclude_list + s, e - s - 1) == 0 ||
		strnicmp(hostname, exclude_list + s, e - s - 1) == 0)
		return 0; /* IP/hostname range excluded. do not use proxy. */

	} else {
	    /* no wildcard at either end, so let's try an absolute
	     * match (ie. a specific IP)
	     */

	    if (stricmp(hostip, exclude_list + s) == 0)
		return 0; /* IP/hostname excluded. do not use proxy. */
	    if (stricmp(hostname, exclude_list + s) == 0)
		return 0; /* IP/hostname excluded. do not use proxy. */
	}

	s = e;
    }

    /* no matches in the exclude list, so use the proxy */
    return 1;
}

Socket new_connection(SockAddr addr, char *hostname,
		      int port, int privport,
		      int oobinline, int nodelay, Plug plug)
{
    static struct socket_function_table socket_fn_table = {
	sk_proxy_plug,
	sk_proxy_close,
	sk_proxy_write,
	sk_proxy_write_oob,
	sk_proxy_flush,
	sk_proxy_set_private_ptr,
	sk_proxy_get_private_ptr,
	sk_proxy_set_frozen,
	sk_proxy_socket_error
    };

    static struct plug_function_table plug_fn_table = {
	plug_proxy_closing,
	plug_proxy_receive,
	plug_proxy_sent,
	plug_proxy_accepting
    };

    if (cfg.proxy_type != PROXY_NONE &&
	proxy_for_destination(addr, hostname, port))
    {
	Proxy_Socket ret;
	Proxy_Plug pplug;
	SockAddr proxy_addr;
	char * proxy_canonical_name;

	ret = smalloc(sizeof(struct Socket_proxy_tag));
	ret->fn = &socket_fn_table;
	ret->plug = plug;
	ret->remote_addr = addr;
	ret->remote_port = port;
	ret->error = NULL;  // !!! added by M.Prikryl

	bufchain_init(&ret->pending_input_data);
	bufchain_init(&ret->pending_output_data);
	bufchain_init(&ret->pending_oob_output_data);

	ret->sub_socket = NULL;
	ret->state = PROXY_STATE_NEW;

	if (cfg.proxy_type == PROXY_HTTP) {
	    ret->negotiate = proxy_http_negotiate;
	} else if (cfg.proxy_type == PROXY_SOCKS) {
	    if (cfg.proxy_socks_version == 4)
		ret->negotiate = proxy_socks4_negotiate;
	    else
		ret->negotiate = proxy_socks5_negotiate;
	} else if (cfg.proxy_type == PROXY_TELNET) {
	    ret->negotiate = proxy_telnet_negotiate;
	} else {
	    ret->error = "Proxy error: Unknown proxy method";
	    return (Socket) ret;
	}

	/* create the proxy plug to map calls from the actual
	 * socket into our proxy socket layer */
	pplug = smalloc(sizeof(struct Plug_proxy_tag));
	pplug->fn = &plug_fn_table;
	pplug->proxy_socket = ret;

	/* look-up proxy */
	proxy_addr = sk_namelookup(cfg.proxy_host,
				   &proxy_canonical_name);
	sfree(proxy_canonical_name);

	/* create the actual socket we will be using,
	 * connected to our proxy server and port.
	 */
	ret->sub_socket = sk_new(proxy_addr, cfg.proxy_port,
				 privport, oobinline,
				 nodelay, (Plug) pplug);
	if (sk_socket_error(ret->sub_socket) != NULL)
	    return (Socket) ret;

	sk_addr_free(proxy_addr);

	/* start the proxy negotiation process... */
	sk_set_frozen(ret->sub_socket, 0);
	ret->negotiate(ret, PROXY_CHANGE_NEW);

	return (Socket) ret;
    }

    /* no proxy, so just return the direct socket */
    return sk_new(addr, port, privport, oobinline, nodelay, plug);
}

Socket new_listener(int port, Plug plug, int local_host_only)
{
    /* TODO: SOCKS (and potentially others) support inbound
     * TODO: connections via the proxy. support them.
     */

    return sk_newlistener(port, plug, local_host_only);
}

/* ----------------------------------------------------------------------
 * HTTP CONNECT proxy type.
 */

static int get_line_end (char * data, int len)
{
    int off = 0;

    while (off < len)
    {
	if (data[off] == '\n') {
	    /* we have a newline */
	    off++;

	    /* is that the only thing on this line? */
	    if (off <= 2) return off;

	    /* if not, then there is the possibility that this header
	     * continues onto the next line, if it starts with a space
	     * or a tab.
	     */

	    if (off + 1 < len &&
		data[off+1] != ' ' &&
		data[off+1] != '\t') return off;

	    /* the line does continue, so we have to keep going
	     * until we see an the header's "real" end of line.
	     */
	    off++;
	}

	off++;
    }

    return -1;
}

int proxy_http_negotiate (Proxy_Socket p, int change)
{
    if (p->state == PROXY_STATE_NEW) {
	/* we are just beginning the proxy negotiate process,
	 * so we'll send off the initial bits of the request.
	 * for this proxy method, it's just a simple HTTP
	 * request
	 */
	char buf[256], dest[64];

	sk_getaddr(p->remote_addr, dest, 64);

	sprintf(buf, "CONNECT %s:%i HTTP/1.1\r\nHost: %s:%i\r\n",
		dest, p->remote_port, dest, p->remote_port);
	sk_write(p->sub_socket, buf, strlen(buf));

	if (cfg.proxy_username[0] || cfg.proxy_password[0]) {
	    char buf[sizeof(cfg.proxy_username)+sizeof(cfg.proxy_password)];
	    char buf2[sizeof(buf)*4/3 + 100];
	    int i, j, len;
	    sprintf(buf, "%s:%s", cfg.proxy_username, cfg.proxy_password);
	    len = strlen(buf);
	    sprintf(buf2, "Proxy-Authorization: basic ");
	    for (i = 0, j = strlen(buf2); i < len; i += 3, j += 4)
		base64_encode_atom(buf+i, (len-i > 3 ? 3 : len-i), buf2+j);
	    strcpy(buf2+j, "\r\n");
	    sk_write(p->sub_socket, buf2, strlen(buf2));
	}

	sprintf(buf, "\r\n");
	sk_write(p->sub_socket, buf, strlen(buf));

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
	return plug_closing(p->plug, p->closing_error_msg,
			    p->closing_error_code,
			    p->closing_calling_back);
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
	return plug_accepting(p->plug, p->accepting_sock);
    }

    if (change == PROXY_CHANGE_RECEIVE) {
	/* we have received data from the underlying socket, which
	 * we'll need to parse, process, and respond to appropriately.
	 */

	char *data, *datap;
	int len;
	int eol;

	if (p->state == 1) {

	    int min_ver, maj_ver, status;

	    /* get the status line */
	    len = bufchain_size(&p->pending_input_data);
	    assert(len > 0);	       /* or we wouldn't be here */
	    data = smalloc(len);
	    bufchain_fetch(&p->pending_input_data, data, len);

	    eol = get_line_end(data, len);
	    if (eol < 0) {
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
		char buf[1024];
		data[eol] = '\0';
		while (eol > status &&
		       (data[eol-1] == '\r' || data[eol-1] == '\n'))
		    data[--eol] = '\0';
		sprintf(buf, "Proxy error: %.900s",
			data+status);
		plug_closing(p->plug, buf, PROXY_ERROR_GENERAL, 0);
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
	    data = smalloc(len);
	    datap = data;
	    bufchain_fetch(&p->pending_input_data, data, len);

	    eol = get_line_end(datap, len);
	    if (eol < 0) {
		sfree(data);
		return 1;
	    }
	    while (eol > 2)
	    {
		bufchain_consume(&p->pending_input_data, eol);
		datap += eol;
		eol = get_line_end(datap, len);
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
int proxy_socks4_negotiate (Proxy_Socket p, int change)
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

	int length;
	char * command;

	if (sk_addrtype(p->remote_addr) != AF_INET) {
	    plug_closing(p->plug, "Proxy error: SOCKS version 4 does"
			 " not support IPv6", PROXY_ERROR_GENERAL, 0);
	    return 1;
	}

	length = strlen(cfg.proxy_username) + 9;
	command = (char*) malloc(length);
	strcpy(command + 8, cfg.proxy_username);

	command[0] = 4; /* version 4 */
	command[1] = 1; /* CONNECT command */

	/* port */
	command[2] = (char) (p->remote_port >> 8) & 0xff;
	command[3] = (char) p->remote_port & 0xff;

	/* address */
	sk_addrcopy(p->remote_addr, command + 4);

	sk_write(p->sub_socket, command, length);
	free(command);

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
	return plug_closing(p->plug, p->closing_error_msg,
			    p->closing_error_code,
			    p->closing_calling_back);
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
	return plug_accepting(p->plug, p->accepting_sock);
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
int proxy_socks5_negotiate (Proxy_Socket p, int change)
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

	char command[4];
	int len;

	command[0] = 5; /* version 5 */
	if (cfg.proxy_username[0] || cfg.proxy_password[0]) {
	    command[1] = 2;	       /* two methods supported: */
	    command[2] = 0x00;	       /* no authentication */
	    command[3] = 0x02;	       /* username/password */
	    len = 4;
	} else {
	    command[1] = 1;	       /* one methods supported: */
	    command[2] = 0x00;	       /* no authentication */
	    len = 3;
	}

	sk_write(p->sub_socket, command, len);

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
	return plug_closing(p->plug, p->closing_error_msg,
			    p->closing_error_code,
			    p->closing_calling_back);
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
	return plug_accepting(p->plug, p->accepting_sock);
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

	    char command[22];
	    int len;

	    if (sk_addrtype(p->remote_addr) == AF_INET) {
		len = 10;
		command[3] = 1; /* IPv4 */
	    } else {
		len = 22;
		command[3] = 4; /* IPv6 */
	    }

	    command[0] = 5; /* version 5 */
	    command[1] = 1; /* CONNECT command */
	    command[2] = 0x00;

	    /* address */
	    sk_addrcopy(p->remote_addr, command+4);

	    /* port */
	    command[len-2] = (char) (p->remote_port >> 8) & 0xff;
	    command[len-1] = (char) p->remote_port & 0xff;

	    sk_write(p->sub_socket, command, len);

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
	    if (cfg.proxy_username[0] || cfg.proxy_password[0]) {
		char userpwbuf[514];
		int ulen, plen;
		ulen = strlen(cfg.proxy_username);
		if (ulen > 255) ulen = 255; if (ulen < 1) ulen = 1;
		plen = strlen(cfg.proxy_password);
		if (plen > 255) plen = 255; if (plen < 1) plen = 1;
		userpwbuf[0] = 1;      /* version number of subnegotiation */
		userpwbuf[1] = ulen;
		memcpy(userpwbuf+2, cfg.proxy_username, ulen);
		userpwbuf[ulen+2] = plen;
		memcpy(userpwbuf+ulen+3, cfg.proxy_password, plen);
		sk_write(p->sub_socket, userpwbuf, ulen + plen + 3);
		p->state = 7;
	    } else 
		plug_closing(p->plug, "Proxy error: Server chose "
			     "username/password authentication but we "
			     "didn't offer it!",
			 PROXY_ERROR_GENERAL, 0);
	    return 1;
	}

	if (p->state == 6) {
	    /* TODO: Handle CHAP authentication */
	    plug_closing(p->plug, "Proxy error: We don't support CHAP authentication",
			 PROXY_ERROR_GENERAL, 0);
	    return 1;
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

int proxy_telnet_negotiate (Proxy_Socket p, int change)
{
    if (p->state == PROXY_CHANGE_NEW) {

	int so = 0, eo = 0;

	/* we need to escape \\, \%, \r, \n, \t, \x??, \0???, 
	 * %%, %host, and %port 
	 */

	while (cfg.proxy_telnet_command[eo] != 0) {

	    /* scan forward until we hit end-of-line, 
	     * or an escape character (\ or %) */
	    while (cfg.proxy_telnet_command[eo] != 0 &&
		   cfg.proxy_telnet_command[eo] != '%' &&
		   cfg.proxy_telnet_command[eo] != '\\') eo++;

	    /* if we hit eol, break out of our escaping loop */
	    if (cfg.proxy_telnet_command[eo] == 0) break;

	    /* if there was any unescaped text before the escape
	     * character, send that now */
	    if (eo != so) {
		sk_write(p->sub_socket, 
			 cfg.proxy_telnet_command + so, eo - so);
	    }

	    so = eo++;

	    /* if the escape character was the last character of
	     * the line, we'll just stop and send it. */
	    if (cfg.proxy_telnet_command[eo] == 0) break;

	    if (cfg.proxy_telnet_command[so] == '\\') {

		/* we recognize \\, \%, \r, \n, \t, \x??. 
		 * anything else, we just send unescaped (including the \). 
		 */

		switch (cfg.proxy_telnet_command[eo]) {

		  case '\\':
		    sk_write(p->sub_socket, "\\", 1);
		    eo++;
		    break;

		  case '%':
		    sk_write(p->sub_socket, "%%", 1);
		    eo++;
		    break;

		  case 'r':
		    sk_write(p->sub_socket, "\r", 1);
		    eo++;
		    break;

		  case 'n':
		    sk_write(p->sub_socket, "\n", 1);
		    eo++;
		    break;

		  case 't':
		    sk_write(p->sub_socket, "\t", 1);
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
			if (cfg.proxy_telnet_command[eo] >= '0' &&
			    cfg.proxy_telnet_command[eo] <= '9')
			    v += cfg.proxy_telnet_command[eo] - '0';
			else if (cfg.proxy_telnet_command[eo] >= 'a' &&
				 cfg.proxy_telnet_command[eo] <= 'f')
			    v += cfg.proxy_telnet_command[eo] - 'a' + 10;
			else if (cfg.proxy_telnet_command[eo] >= 'A' &&
				 cfg.proxy_telnet_command[eo] <= 'F')
			    v += cfg.proxy_telnet_command[eo] - 'A' + 10;
			else {
			    /* non hex character, so we abort and just
			     * send the whole thing unescaped (including \x)
			     */
			    sk_write(p->sub_socket, "\\", 1);
			    eo = so + 1;
			    break;
			}

			/* we only extract two hex characters */
			if (i == 1) {
			    sk_write(p->sub_socket, &v, 1);
			    eo++;
			    break;
			}

			i++;
			v <<= 4;
		    }
		    }
		    break;

		  default:
		    sk_write(p->sub_socket, 
			     cfg.proxy_telnet_command + so, 2);
		    eo++;
		    break;
		}
	    } else {

		/* % escape. we recognize %%, %host, %port. anything else,
		 * we just send unescaped (including the %). */

		if (cfg.proxy_telnet_command[eo] == '%') {
		    sk_write(p->sub_socket, "%", 1);
		    eo++;
		} 
		else if (strnicmp(cfg.proxy_telnet_command + eo,
				  "host", 4) == 0) {
		    char dest[64];
		    sk_getaddr(p->remote_addr, dest, 64);
		    sk_write(p->sub_socket, dest, strlen(dest));
		    eo += 4;
		} 
		else if (strnicmp(cfg.proxy_telnet_command + eo,
				  "port", 4) == 0) {
		    char port[8];
		    sprintf(port, "%i", p->remote_port);
		    sk_write(p->sub_socket, port, strlen(port));
		    eo += 4;
		} 
		else {
		    /* we don't escape this, so send the % now, and
		     * don't advance eo, so that we'll consider the
		     * text immediately following the % as unescaped.
		     */
		    sk_write(p->sub_socket, "%", 1);
		}
	    }

	    /* resume scanning for additional escapes after this one. */
	    so = eo;
	}

	/* if there is any unescaped text at the end of the line, send it */
	if (eo != so) {
	    sk_write(p->sub_socket, cfg.proxy_telnet_command + so, eo - so);
	}

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
	return plug_closing(p->plug, p->closing_error_msg,
			    p->closing_error_code,
			    p->closing_calling_back);
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
	return plug_accepting(p->plug, p->accepting_sock);
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
