/*
 * Pageant client code.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "putty.h"
#include "pageant.h" /* for AGENT_MAX_MSGLEN */

#ifndef NO_SECURITY
#include "winsecur.h"
#endif

#define AGENT_COPYDATA_ID 0x804e50ba   /* random goop */

int agent_exists(void)
{
    HWND hwnd;
    hwnd = FindWindow("Pageant", "Pageant");
    if (!hwnd)
	return FALSE;
    else
	return TRUE;
}

void agent_cancel_query(agent_pending_query *q)
{
    assert(0 && "Windows agent queries are never asynchronous!");
}

agent_pending_query *agent_query(
    void *in, int inlen, void **out, int *outlen,
    void (*callback)(void *, void *, int), void *callback_ctx)
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

    hwnd = FindWindow("Pageant", "Pageant");
    if (!hwnd)
	return NULL;		       /* *out == NULL, so failure */
    mapname = dupprintf("PageantRequest%08x", (unsigned)GetCurrentThreadId());

    psa = NULL;
#ifndef NO_SECURITY
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
                    p_SetSecurityDescriptorOwner(psd, usersid, FALSE)) {
                    sa.nLength = sizeof(sa);
                    sa.bInheritHandle = TRUE;
                    sa.lpSecurityDescriptor = psd;
                    psa = &sa;
                } else {
                    LocalFree(psd);
                    psd = NULL;
                }
            }
        }
    }
#endif /* NO_SECURITY */

    filemap = CreateFileMapping(INVALID_HANDLE_VALUE, psa, PAGE_READWRITE,
				0, AGENT_MAX_MSGLEN, mapname);
    if (filemap == NULL || filemap == INVALID_HANDLE_VALUE) {
        sfree(mapname);
	return NULL;		       /* *out == NULL, so failure */
    }
    p = MapViewOfFile(filemap, FILE_MAP_WRITE, 0, 0, 0);
    memcpy(p, in, inlen);
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
	retlen = 4 + GET_32BIT(p);
	ret = snewn(retlen, unsigned char);
	if (ret) {
	    memcpy(ret, p, retlen);
	    *out = ret;
	    *outlen = retlen;
	}
    }
    UnmapViewOfFile(p);
    CloseHandle(filemap);
    sfree(mapname);
    if (psd)
        LocalFree(psd);
    return NULL;
}
