/*
 * local-proxy.c: Windows implementation of platform_new_connection(),
 * supporting an OpenSSH-like proxy command via the handle-io.c
 * mechanism.
 */

#include <stdio.h>
#include <assert.h>

#include "tree234.h"
#include "putty.h"
#include "network.h"
#include "proxy/proxy.h"

char *platform_setup_local_proxy(Socket *socket, const char *cmd)
{
    HANDLE us_to_cmd, cmd_from_us;
    HANDLE us_from_cmd, cmd_to_us;
    HANDLE us_from_cmd_err, cmd_err_to_us;
    SECURITY_ATTRIBUTES sa;
    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    /*
     * Create the pipes to the proxy command, and spawn the proxy
     * command process.
     */
    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = NULL;    /* default */
    sa.bInheritHandle = true;
    if (!CreatePipe(&us_from_cmd, &cmd_to_us, &sa, 0)) {
        return dupprintf("Unable to create pipes for proxy command: %s",
                         win_strerror(GetLastError()));
    }

    if (!CreatePipe(&cmd_from_us, &us_to_cmd, &sa, 0)) {
        CloseHandle(us_from_cmd);
        CloseHandle(cmd_to_us);
        return dupprintf("Unable to create pipes for proxy command: %s",
                         win_strerror(GetLastError()));
    }

    if (!CreatePipe(&us_from_cmd_err, &cmd_err_to_us, &sa, 0)) {
        CloseHandle(us_from_cmd);
        CloseHandle(cmd_to_us);
        CloseHandle(us_to_cmd);
        CloseHandle(cmd_from_us);
        return dupprintf("Unable to create pipes for proxy command: %s",
                         win_strerror(GetLastError()));
    }

    SetHandleInformation(us_to_cmd, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(us_from_cmd, HANDLE_FLAG_INHERIT, 0);
    if (us_from_cmd_err != NULL)
        SetHandleInformation(us_from_cmd_err, HANDLE_FLAG_INHERIT, 0);

    si.cb = sizeof(si);
    si.lpReserved = NULL;
    si.lpDesktop = NULL;
    si.lpTitle = NULL;
    si.dwFlags = STARTF_USESTDHANDLES;
    si.cbReserved2 = 0;
    si.lpReserved2 = NULL;
    si.hStdInput = cmd_from_us;
    si.hStdOutput = cmd_to_us;
    si.hStdError = cmd_err_to_us;
    char *cmd_mutable = dupstr(cmd); /* CreateProcess needs non-const char * */
    CreateProcess(NULL, cmd_mutable, NULL, NULL, true,
                  CREATE_NO_WINDOW | NORMAL_PRIORITY_CLASS,
                  NULL, NULL, &si, &pi);
    sfree(cmd_mutable);
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    CloseHandle(cmd_from_us);
    CloseHandle(cmd_to_us);

    if (cmd_err_to_us != NULL)
        CloseHandle(cmd_err_to_us);

    setup_handle_socket(socket, us_to_cmd, us_from_cmd, us_from_cmd_err,
                        false);

    return NULL;
}

Socket *platform_new_connection(SockAddr *addr, const char *hostname,
                                int port, bool privport,
                                bool oobinline, bool nodelay, bool keepalive,
                                Plug *plug, Conf *conf, Interactor *itr)
{
    if (conf_get_int(conf, CONF_proxy_type) != PROXY_CMD)
        return NULL;

    DeferredSocketOpener *opener = local_proxy_opener(
        addr, port, plug, conf, itr);
    Socket *socket = make_deferred_handle_socket(opener, addr, port, plug);
    local_proxy_opener_set_socket(opener, socket);
    return socket;
}
