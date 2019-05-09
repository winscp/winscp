/*
 * Noise generation for PuTTY's cryptographic random number
 * generator.
 */

#include <stdio.h>

#include "putty.h"
#include "ssh.h"
#include "storage.h"

#include <wincrypt.h>

DECL_WINDOWS_FUNCTION(static, BOOL, CryptAcquireContextA,
                      (HCRYPTPROV *, LPCTSTR, LPCTSTR, DWORD, DWORD));
DECL_WINDOWS_FUNCTION(static, BOOL, CryptGenRandom,
                      (HCRYPTPROV, DWORD, BYTE *));
DECL_WINDOWS_FUNCTION(static, BOOL, CryptReleaseContext,
                      (HCRYPTPROV, DWORD));
static HMODULE wincrypt_module = NULL;

bool win_read_random(void *buf, unsigned wanted)
{
    bool toret = false;
    HCRYPTPROV crypt_provider;

    if (!wincrypt_module) {
        wincrypt_module = load_system32_dll("advapi32.dll");
        GET_WINDOWS_FUNCTION(wincrypt_module, CryptAcquireContextA);
        GET_WINDOWS_FUNCTION(wincrypt_module, CryptGenRandom);
        GET_WINDOWS_FUNCTION(wincrypt_module, CryptReleaseContext);
    }

    if (wincrypt_module && p_CryptAcquireContextA &&
        p_CryptGenRandom && p_CryptReleaseContext &&
        p_CryptAcquireContextA(&crypt_provider, NULL, NULL, PROV_RSA_FULL,
                               CRYPT_VERIFYCONTEXT)) {
        toret = p_CryptGenRandom(crypt_provider, wanted, buf);
        p_CryptReleaseContext(crypt_provider, 0);
    }

    return toret;
}

/*
 * This function is called once, at PuTTY startup.
 */

void noise_get_heavy(void (*func) (void *, int))
{
    HANDLE srch;
    WIN32_FIND_DATA finddata;
    DWORD pid;
    char winpath[MAX_PATH + 3];
    BYTE buf[32];

    GetWindowsDirectory(winpath, sizeof(winpath));
    strcat(winpath, "\\*");
    srch = FindFirstFile(winpath, &finddata);
    if (srch != INVALID_HANDLE_VALUE) {
	do {
	    func(&finddata, sizeof(finddata));
	} while (FindNextFile(srch, &finddata));
	FindClose(srch);
    }

    pid = GetCurrentProcessId();
    func(&pid, sizeof(pid));

    if (win_read_random(buf, sizeof(buf))) {
        func(buf, sizeof(buf));
        smemclr(buf, sizeof(buf));
    }

    read_random_seed(func);
}

void random_save_seed(void)
{
    int len;
    void *data;

    if (random_active) {
	random_get_savedata(&data, &len);
	write_random_seed(data, len);
	sfree(data);
    }
}

/*
 * This function is called on a timer, and it will monitor
 * frequently changing quantities such as the state of physical and
 * virtual memory, the state of the process's message queue, which
 * window is in the foreground, which owns the clipboard, etc.
 */
void noise_regular(void)
{
    HWND w;
    DWORD z;
    POINT pt;
    MEMORYSTATUS memstat;
    FILETIME times[4];

    w = GetForegroundWindow();
    random_add_noise(NOISE_SOURCE_FGWINDOW, &w, sizeof(w));
    w = GetCapture();
    random_add_noise(NOISE_SOURCE_CAPTURE, &w, sizeof(w));
    w = GetClipboardOwner();
    random_add_noise(NOISE_SOURCE_CLIPBOARD, &w, sizeof(w));
    z = GetQueueStatus(QS_ALLEVENTS);
    random_add_noise(NOISE_SOURCE_QUEUE, &z, sizeof(z));

    GetCursorPos(&pt);
    random_add_noise(NOISE_SOURCE_CURSORPOS, &pt, sizeof(pt));

    GlobalMemoryStatus(&memstat);
    random_add_noise(NOISE_SOURCE_MEMINFO, &memstat, sizeof(memstat));

    GetThreadTimes(GetCurrentThread(), times, times + 1, times + 2,
		   times + 3);
    random_add_noise(NOISE_SOURCE_THREADTIME, &times, sizeof(times));
    GetProcessTimes(GetCurrentProcess(), times, times + 1, times + 2,
		    times + 3);
    random_add_noise(NOISE_SOURCE_PROCTIME, &times, sizeof(times));
}

/*
 * This function is called on every keypress or mouse move, and
 * will add the current Windows time and performance monitor
 * counter to the noise pool. It gets the scan code or mouse
 * position passed in.
 */
void noise_ultralight(NoiseSourceId id, unsigned long data)
{
    DWORD wintime;
    LARGE_INTEGER perftime;

    random_add_noise(id, &data, sizeof(DWORD));

    wintime = GetTickCount();
    random_add_noise(NOISE_SOURCE_TIME, &wintime, sizeof(DWORD));

    if (QueryPerformanceCounter(&perftime))
	random_add_noise(NOISE_SOURCE_PERFCOUNT, &perftime, sizeof(perftime));
}

uint64_t prng_reseed_time_ms(void)
{
    FILETIME ft;
    GetSystemTimeAsFileTime(&ft);
    uint64_t value = ft.dwHighDateTime;
    value = (value << 32) + ft.dwLowDateTime;
    return value / 10000;              /* 1 millisecond / 100ns */
}
