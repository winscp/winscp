/*
 * Pageant client code.
 */

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

#include "puttymem.h"

#define AGENT_COPYDATA_ID 0x804e50ba   /* random goop */
#define AGENT_MAX_MSGLEN  8192

#ifdef TESTMODE
#define debug(x) (printf x)
#else
#define debug(x)
#endif

#define GET_32BIT(cp) \
    (((unsigned long)(unsigned char)(cp)[0] << 24) | \
    ((unsigned long)(unsigned char)(cp)[1] << 16) | \
    ((unsigned long)(unsigned char)(cp)[2] << 8) | \
    ((unsigned long)(unsigned char)(cp)[3]))

int agent_exists(void)
{
    HWND hwnd;
    hwnd = FindWindow("Pageant", "Pageant");
    if (!hwnd)
	return FALSE;
    else
	return TRUE;
}

void agent_query(void *in, int inlen, void **out, int *outlen)
{
    HWND hwnd;
    char mapname[64];
    HANDLE filemap;
    unsigned char *p, *ret;
    int id, retlen;
    COPYDATASTRUCT cds;

    *out = NULL;
    *outlen = 0;

    hwnd = FindWindow("Pageant", "Pageant");
    debug(("hwnd is %p\n", hwnd));
    if (!hwnd)
	return;
    sprintf(mapname, "PageantRequest%08x", (unsigned)GetCurrentThreadId());
    filemap = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE,
				0, AGENT_MAX_MSGLEN, mapname);
    if (!filemap)
	return;
    p = MapViewOfFile(filemap, FILE_MAP_WRITE, 0, 0, 0);
    memcpy(p, in, inlen);
    cds.dwData = AGENT_COPYDATA_ID;
    cds.cbData = 1 + strlen(mapname);
    cds.lpData = mapname;
    id = SendMessage(hwnd, WM_COPYDATA, (WPARAM) NULL, (LPARAM) & cds);
    debug(("return is %d\n", id));
    if (id > 0) {
	retlen = 4 + GET_32BIT(p);
	debug(("len is %d\n", retlen));
	ret = smalloc(retlen);
	if (ret) {
	    memcpy(ret, p, retlen);
	    *out = ret;
	    *outlen = retlen;
	}
    }
    UnmapViewOfFile(p);
    CloseHandle(filemap);
}

#ifdef TESTMODE

int main(void)
{
    void *msg;
    int len;
    int i;

    agent_query("\0\0\0\1\1", 5, &msg, &len);
    debug(("%d:", len));
    for (i = 0; i < len; i++)
	debug((" %02x", ((unsigned char *) msg)[i]));
    debug(("\n"));
    return 0;
}

#endif
