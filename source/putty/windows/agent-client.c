/*
 * Pageant client code.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "putty.h"
#include "pageant.h" /* for AGENT_MAX_MSGLEN */

#include "security-api.h"
#include "cryptoapi.h"

static bool wm_copydata_agent_exists(void)
{
    HWND hwnd;
    hwnd = FindWindow("Pageant", "Pageant");
    if (!hwnd)
        return false;
    else
        return true;
}

static void wm_copydata_agent_query(strbuf *query, void **out, int *outlen)
{
    HWND hwnd;
    char *mapname;
    HANDLE filemap;
    unsigned char *p, *ret;
    int id, retlen;
    COPYDATASTRUCT cds;
    SECURITY_ATTRIBUTES sa, *psa;
    PSECURITY_DESCRIPTOR psd = NULL;
    PSID usersid = NULL;

    *out = NULL;
    *outlen = 0;

    if (query->len > AGENT_MAX_MSGLEN)
        return;                        /* query too large */

    hwnd = FindWindow("Pageant", "Pageant");
    if (!hwnd)
        return;                        /* *out == NULL, so failure */
    mapname = dupprintf("PageantRequest%08x", (unsigned)GetCurrentThreadId());

    psa = NULL;
    if (got_advapi()) {
        /*
         * Make the file mapping we create for communication with
         * Pageant owned by the user SID rather than the default. This
         * should make communication between processes with slightly
         * different contexts more reliable: in particular, command
         * prompts launched as administrator should still be able to
         * run PSFTPs which refer back to the owning user's
         * unprivileged Pageant.
         */
        usersid = get_user_sid();

        if (usersid) {
            psd = (PSECURITY_DESCRIPTOR)
                LocalAlloc(LPTR, SECURITY_DESCRIPTOR_MIN_LENGTH);
            if (psd) {
                if (p_InitializeSecurityDescriptor
                    (psd, SECURITY_DESCRIPTOR_REVISION) &&
                    p_SetSecurityDescriptorOwner(psd, usersid, false)) {
                    sa.nLength = sizeof(sa);
                    sa.bInheritHandle = true;
                    sa.lpSecurityDescriptor = psd;
                    psa = &sa;
                } else {
                    LocalFree(psd);
                    psd = NULL;
                }
            }
        }
    }

    filemap = CreateFileMapping(INVALID_HANDLE_VALUE, psa, PAGE_READWRITE,
                                0, AGENT_MAX_MSGLEN, mapname);
    if (filemap == NULL || filemap == INVALID_HANDLE_VALUE) {
        sfree(mapname);
        return;                        /* *out == NULL, so failure */
    }
    p = MapViewOfFile(filemap, FILE_MAP_WRITE, 0, 0, 0);
    strbuf_finalise_agent_query(query);
    memcpy(p, query->s, query->len);
    cds.dwData = AGENT_COPYDATA_ID;
    cds.cbData = 1 + strlen(mapname);
    cds.lpData = mapname;

    /*
     * The user either passed a null callback (indicating that the
     * query is required to be synchronous) or CreateThread failed.
     * Either way, we need a synchronous request.
     */
    id = SendMessage(hwnd, WM_COPYDATA, (WPARAM) NULL, (LPARAM) &cds);
    if (id > 0) {
        uint32_t length_field = GET_32BIT_MSB_FIRST(p);
        if (length_field > 0 && length_field <= AGENT_MAX_MSGLEN - 4) {
            retlen = length_field + 4;
            ret = snewn(retlen, unsigned char);
            memcpy(ret, p, retlen);
            *out = ret;
            *outlen = retlen;
        } else {
            /*
             * If we get here, we received an out-of-range length
             * field, either without space for a message type code or
             * overflowing the FileMapping.
             *
             * Treat this as if Pageant didn't answer at all - which
             * actually means we do nothing, and just don't fill in
             * out and outlen.
             */
        }
    }
    UnmapViewOfFile(p);
    CloseHandle(filemap);
    sfree(mapname);
    if (psd)
        LocalFree(psd);
}

Socket *agent_connect(Plug *plug)
{
    char *pipename = agent_named_pipe_name();
    Socket *s = new_named_pipe_client(pipename, plug);
    sfree(pipename);
    return s;
}

static bool named_pipe_agent_exists(void)
{
    char *pipename = agent_named_pipe_name();
    WIN32_FIND_DATA data;
    HANDLE ffh = FindFirstFile(pipename, &data);
    sfree(pipename);
    if (ffh == INVALID_HANDLE_VALUE)
        return false;
    FindClose(ffh);
    return true;
}

bool agent_exists(void)
{
    return named_pipe_agent_exists() || wm_copydata_agent_exists();
}

struct agent_pending_query {
    struct handle *handle;
    HANDLE os_handle;
    strbuf *response;
    void (*callback)(void *, void *, int);
    void *callback_ctx;
};

static int named_pipe_agent_accumulate_response(
    strbuf *sb, const void *data, size_t len)
{
    put_data(sb, data, len);
    if (sb->len >= 4) {
        uint32_t length_field = GET_32BIT_MSB_FIRST(sb->u);
        if (length_field > AGENT_MAX_MSGLEN)
            return -1; /* badly formatted message */

        int overall_length = length_field + 4;
        if (sb->len >= overall_length)
            return overall_length;
    }

    return 0; /* not done yet */
}

static size_t named_pipe_agent_gotdata(
    struct handle *h, const void *data, size_t len, int err)
{
    agent_pending_query *pq = handle_get_privdata(h);

    if (err || len == 0) {
        pq->callback(pq->callback_ctx, NULL, 0);
        agent_cancel_query(pq);
        return 0;
    }

    int status = named_pipe_agent_accumulate_response(pq->response, data, len);
    if (status == -1) {
        pq->callback(pq->callback_ctx, NULL, 0);
        agent_cancel_query(pq);
    } else if (status > 0) {
        void *response_buf = strbuf_to_str(pq->response);
        pq->response = NULL;
        pq->callback(pq->callback_ctx, response_buf, status);
        agent_cancel_query(pq);
    }
    return 0;
}

static agent_pending_query *named_pipe_agent_query(
    strbuf *query, void **out, int *outlen,
    void (*callback)(void *, void *, int), void *callback_ctx)
{
    agent_pending_query *pq = NULL;
    char *err = NULL, *pipename = NULL;
    strbuf *sb = NULL;
    HANDLE pipehandle;

    pipename = agent_named_pipe_name();
    pipehandle = connect_to_named_pipe(pipename, &err);
    if (pipehandle == INVALID_HANDLE_VALUE)
        goto failure;

    strbuf_finalise_agent_query(query);

    for (DWORD done = 0; done < query->len ;) {
        DWORD nwritten;
        bool ret = WriteFile(pipehandle, query->s + done, query->len - done,
                             &nwritten, NULL);
        if (!ret)
            goto failure;

        done += nwritten;
    }

    if (!callback) {
        int status;

        sb = strbuf_new_nm();
        do {
            char buf[1024];
            DWORD nread;
            bool ret = ReadFile(pipehandle, buf, sizeof(buf), &nread, NULL);
            if (!ret)
                goto failure;
            status = named_pipe_agent_accumulate_response(sb, buf, nread);
        } while (status == 0);

        if (status == -1)
            goto failure;

        *out = strbuf_to_str(sb);
        *outlen = status;
        sb = NULL;
        pq = NULL;
        goto out;
    }

    pq = snew(agent_pending_query);
    pq->handle = handle_input_new(pipehandle, named_pipe_agent_gotdata, pq, 0);
    pq->os_handle = pipehandle;
    pipehandle = INVALID_HANDLE_VALUE;  /* prevent it being closed below */
    pq->response = strbuf_new_nm();
    pq->callback = callback;
    pq->callback_ctx = callback_ctx;
    goto out;

  failure:
    *out = NULL;
    *outlen = 0;
    pq = NULL;

  out:
    sfree(err);
    sfree(pipename);
    if (pipehandle != INVALID_HANDLE_VALUE)
        CloseHandle(pipehandle);
    if (sb)
        strbuf_free(sb);
    return pq;
}

void agent_cancel_query(agent_pending_query *pq)
{
    handle_free(pq->handle);
    CloseHandle(pq->os_handle);
    if (pq->response)
        strbuf_free(pq->response);
    sfree(pq);
}

agent_pending_query *agent_query(
    strbuf *query, void **out, int *outlen,
    void (*callback)(void *, void *, int), void *callback_ctx)
{
    agent_pending_query *pq = named_pipe_agent_query(
        query, out, outlen, callback, callback_ctx);
    if (pq || *out)
        return pq;

    wm_copydata_agent_query(query, out, outlen);
    return NULL;
}
