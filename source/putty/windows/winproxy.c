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

Socket make_handle_socket(HANDLE send_H, HANDLE recv_H, Plug plug,
                          int overlapped);

Socket platform_new_connection(SockAddr addr, char *hostname,
			       int port, int privport,
			       int oobinline, int nodelay, int keepalive,
			       Plug plug, Conf *conf)
{
    char *cmd;
    HANDLE us_to_cmd, us_from_cmd, cmd_to_us, cmd_from_us;
    SECURITY_ATTRIBUTES sa;
    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    if (conf_get_int(conf, CONF_proxy_type) != PROXY_CMD)
	return NULL;

    cmd = format_telnet_command(addr, port, conf);

    /* We are responsible for this and don't need it any more */
    sk_addr_free(addr);

    {
	char *msg = dupprintf("Starting local proxy command: %s", cmd);
	/* We're allowed to pass NULL here, because we're part of the Windows
	 * front end so we know logevent doesn't expect any data. */
	logevent(NULL, msg);
	sfree(msg);
    }

    /*
     * Create the pipes to the proxy command, and spawn the proxy
     * command process.
     */
    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = NULL;    /* default */
    sa.bInheritHandle = TRUE;
    if (!CreatePipe(&us_from_cmd, &cmd_to_us, &sa, 0)) {
	Socket ret =
            new_error_socket("Unable to create pipes for proxy command", plug);
        sfree(cmd);
	return ret;
    }

    if (!CreatePipe(&cmd_from_us, &us_to_cmd, &sa, 0)) {
	Socket ret =
            new_error_socket("Unable to create pipes for proxy command", plug);
        sfree(cmd);
	CloseHandle(us_from_cmd);
	CloseHandle(cmd_to_us);
	return ret;
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
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    sfree(cmd);

    CloseHandle(cmd_from_us);
    CloseHandle(cmd_to_us);

    return make_handle_socket(us_to_cmd, us_from_cmd, plug, FALSE);
}
