/*
 * winproxy.c: Windows implementation of platform_new_connection(),
 * supporting an OpenSSH-like proxy command via the winhandl.c
 * mechanism.
 */

#include <stdio.h>
#include <assert.h>

#define DEFINE_PLUG_METHOD_MACROS
#include "tree234.h"
#include "putty.h"
#include "network.h"
#include "proxy.h"

typedef struct Socket_localproxy_tag *Local_Proxy_Socket;

struct Socket_localproxy_tag {
    const struct socket_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */

    HANDLE to_cmd_H, from_cmd_H;
    struct handle *to_cmd_h, *from_cmd_h;

    char *error;

    Plug plug;

    void *privptr;
};

int localproxy_gotdata(struct handle *h, void *data, int len)
{
    Local_Proxy_Socket ps = (Local_Proxy_Socket) handle_get_privdata(h);

    if (len < 0) {
	return plug_closing(ps->plug, "Read error from local proxy command",
			    0, 0);
    } else if (len == 0) {
	return plug_closing(ps->plug, NULL, 0, 0);
    } else {
	return plug_receive(ps->plug, 1, data, len);
    }
}

void localproxy_sentdata(struct handle *h, int new_backlog)
{
    Local_Proxy_Socket ps = (Local_Proxy_Socket) handle_get_privdata(h);
    
    plug_sent(ps->plug, new_backlog);
}

static Plug sk_localproxy_plug (Socket s, Plug p)
{
    Local_Proxy_Socket ps = (Local_Proxy_Socket) s;
    Plug ret = ps->plug;
    if (p)
	ps->plug = p;
    return ret;
}

static void sk_localproxy_close (Socket s)
{
    Local_Proxy_Socket ps = (Local_Proxy_Socket) s;

    handle_free(ps->to_cmd_h);
    handle_free(ps->from_cmd_h);
    CloseHandle(ps->to_cmd_H);
    CloseHandle(ps->from_cmd_H);

    sfree(ps);
}

static int sk_localproxy_write (Socket s, const char *data, int len)
{
    Local_Proxy_Socket ps = (Local_Proxy_Socket) s;

    return handle_write(ps->to_cmd_h, data, len);
}

static int sk_localproxy_write_oob(Socket s, const char *data, int len)
{
    /*
     * oob data is treated as inband; nasty, but nothing really
     * better we can do
     */
    return sk_localproxy_write(s, data, len);
}

static void sk_localproxy_flush(Socket s)
{
    /* Local_Proxy_Socket ps = (Local_Proxy_Socket) s; */
    /* do nothing */
}

static void sk_localproxy_set_private_ptr(Socket s, void *ptr)
{
    Local_Proxy_Socket ps = (Local_Proxy_Socket) s;
    ps->privptr = ptr;
}

static void *sk_localproxy_get_private_ptr(Socket s)
{
    Local_Proxy_Socket ps = (Local_Proxy_Socket) s;
    return ps->privptr;
}

static void sk_localproxy_set_frozen(Socket s, int is_frozen)
{
    Local_Proxy_Socket ps = (Local_Proxy_Socket) s;

    /*
     * FIXME
     */
}

static const char *sk_localproxy_socket_error(Socket s)
{
    Local_Proxy_Socket ps = (Local_Proxy_Socket) s;
    return ps->error;
}

Socket platform_new_connection(SockAddr addr, char *hostname,
			       int port, int privport,
			       int oobinline, int nodelay, int keepalive,
			       Plug plug, const Config *cfg)
{
    char *cmd;

    static const struct socket_function_table socket_fn_table = {
	sk_localproxy_plug,
	sk_localproxy_close,
	sk_localproxy_write,
	sk_localproxy_write_oob,
	sk_localproxy_flush,
	sk_localproxy_set_private_ptr,
	sk_localproxy_get_private_ptr,
	sk_localproxy_set_frozen,
	sk_localproxy_socket_error
    };

    Local_Proxy_Socket ret;
    HANDLE us_to_cmd, us_from_cmd, cmd_to_us, cmd_from_us;
    SECURITY_ATTRIBUTES sa;
    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    if (cfg->proxy_type != PROXY_CMD)
	return NULL;

    cmd = format_telnet_command(addr, port, cfg);

    {
	char *msg = dupprintf("Starting local proxy command: %s", cmd);
	/* We're allowed to pass NULL here, because we're part of the Windows
	 * front end so we know logevent doesn't expect any data. */
	logevent(NULL, msg);
	sfree(msg);
    }

    ret = snew(struct Socket_localproxy_tag);
    ret->fn = &socket_fn_table;
    ret->plug = plug;
    ret->error = NULL;

    /*
     * Create the pipes to the proxy command, and spawn the proxy
     * command process.
     */
    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = NULL;    /* default */
    sa.bInheritHandle = TRUE;
    if (!CreatePipe(&us_from_cmd, &cmd_to_us, &sa, 0)) {
	ret->error = dupprintf("Unable to create pipes for proxy command");
	return (Socket)ret;
    }

    if (!CreatePipe(&cmd_from_us, &us_to_cmd, &sa, 0)) {
	CloseHandle(us_from_cmd);
	CloseHandle(cmd_to_us);
	ret->error = dupprintf("Unable to create pipes for proxy command");
	return (Socket)ret;
    }

    SetHandleInformation(us_to_cmd, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(us_from_cmd, HANDLE_FLAG_INHERIT, 0);

    si.cb = sizeof(si);
    si.lpReserved = NULL;
    si.lpDesktop = NULL;
    si.lpTitle = NULL;
    si.dwFlags = STARTF_USESTDHANDLES;
    si.cbReserved2 = 0;
    si.lpReserved2 = NULL;
    si.hStdInput = cmd_from_us;
    si.hStdOutput = cmd_to_us;
    si.hStdError = NULL;
    CreateProcess(NULL, cmd, NULL, NULL, TRUE,
		  CREATE_NO_WINDOW | NORMAL_PRIORITY_CLASS,
		  NULL, NULL, &si, &pi);

    CloseHandle(cmd_from_us);
    CloseHandle(cmd_to_us);

    ret->to_cmd_H = us_to_cmd;
    ret->from_cmd_H = us_from_cmd;

    ret->from_cmd_h = handle_input_new(ret->from_cmd_H, localproxy_gotdata,
				       ret, 0);
    ret->to_cmd_h = handle_output_new(ret->to_cmd_H, localproxy_sentdata,
				      ret, 0);

    /* We are responsible for this and don't need it any more */
    sk_addr_free(addr);

    return (Socket) ret;
}
