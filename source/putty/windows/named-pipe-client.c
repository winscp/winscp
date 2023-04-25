/*
 * Windows support module which deals with being a named-pipe client.
 */

#include <stdio.h>
#include <assert.h>

#include "tree234.h"
#include "putty.h"
#include "network.h"
#include "proxy/proxy.h"
#include "ssh.h"

#include "security-api.h"

HANDLE connect_to_named_pipe(const char *pipename, char **err)
{
    HANDLE pipehandle;
    PSID usersid, pipeowner;
    PSECURITY_DESCRIPTOR psd;

    assert(strncmp(pipename, "\\\\.\\pipe\\", 9) == 0);
    assert(strchr(pipename + 9, '\\') == NULL);

    while (1) {
        pipehandle = CreateFile(pipename, GENERIC_READ | GENERIC_WRITE,
                                0, NULL, OPEN_EXISTING,
                                FILE_FLAG_OVERLAPPED, NULL);

        if (pipehandle != INVALID_HANDLE_VALUE)
            break;

        if (GetLastError() != ERROR_PIPE_BUSY) {
            *err = dupprintf(
                "Unable to open named pipe '%s': %s",
                pipename, win_strerror(GetLastError()));
            return INVALID_HANDLE_VALUE;
        }

        /*
         * If we got ERROR_PIPE_BUSY, wait for the server to create a
         * new pipe instance. (Since the server is expected to be
         * named-pipe-server.c, which will do that immediately after a
         * previous connection is accepted, that shouldn't take
         * excessively long.)
         */
        if (!WaitNamedPipe(pipename, NMPWAIT_USE_DEFAULT_WAIT)) {
            *err = dupprintf(
                "Error waiting for named pipe '%s': %s",
                pipename, win_strerror(GetLastError()));
            return INVALID_HANDLE_VALUE;
        }
    }

    if ((usersid = get_user_sid()) == NULL) {
        CloseHandle(pipehandle);
        *err = dupprintf(
            "Unable to get user SID: %s", win_strerror(GetLastError()));
        return INVALID_HANDLE_VALUE;
    }

    if (p_GetSecurityInfo(pipehandle, SE_KERNEL_OBJECT,
                          OWNER_SECURITY_INFORMATION,
                          &pipeowner, NULL, NULL, NULL,
                          &psd) != ERROR_SUCCESS) {
        CloseHandle(pipehandle);
        *err = dupprintf(
            "Unable to get named pipe security information: %s",
            win_strerror(GetLastError()));
        return INVALID_HANDLE_VALUE;
    }

    if (!EqualSid(pipeowner, usersid)) {
        CloseHandle(pipehandle);
        LocalFree(psd);
        *err = dupprintf(
            "Owner of named pipe '%s' is not us", pipename);
        return INVALID_HANDLE_VALUE;
    }

    LocalFree(psd);

    return pipehandle;
}

Socket *new_named_pipe_client(const char *pipename, Plug *plug)
{
    char *err = NULL;
    HANDLE pipehandle = connect_to_named_pipe(pipename, &err);
    if (pipehandle == INVALID_HANDLE_VALUE)
        return new_error_socket_consume_string(plug, err);
    else
        return make_handle_socket(pipehandle, pipehandle, NULL, NULL, 0,
                                  plug, true);
}
