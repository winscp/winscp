/*
 * Noise generation for PuTTY's cryptographic random number
 * generator.
 */

#include <stdio.h>

#include "putty.h"
#include "ssh.h"
#include "storage.h"

/*
 * This function is called once, at PuTTY startup, and will do some
 * seriously silly things like listing directories and getting disk
 * free space and a process snapshot.
 */

void noise_get_heavy(void (*func) (void *, int))
{
    HANDLE srch;
    WIN32_FIND_DATA finddata;
    char winpath[MAX_PATH + 3];

    GetWindowsDirectory(winpath, sizeof(winpath));
    strcat(winpath, "\\*");
    srch = FindFirstFile(winpath, &finddata);
    if (srch != INVALID_HANDLE_VALUE) {
	do {
	    func(&finddata, sizeof(finddata));
	} while (FindNextFile(srch, &finddata));
	FindClose(srch);
    }

    read_random_seed(func);
    /* Update the seed immediately, in case another instance uses it. */
    random_save_seed();
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
 * This function is called every time the random pool needs
 * stirring, and will acquire the system time in all available
 * forms and the battery status.
 */
void noise_get_light(void (*func) (void *, int))
{
    SYSTEMTIME systime;
    DWORD adjust[2];
    BOOL rubbish;

    GetSystemTime(&systime);
    func(&systime, sizeof(systime));

    GetSystemTimeAdjustment(&adjust[0], &adjust[1], &rubbish);
    func(&adjust, sizeof(adjust));
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
    random_add_noise(&w, sizeof(w));
    w = GetCapture();
    random_add_noise(&w, sizeof(w));
    w = GetClipboardOwner();
    random_add_noise(&w, sizeof(w));
    z = GetQueueStatus(QS_ALLEVENTS);
    random_add_noise(&z, sizeof(z));

    GetCursorPos(&pt);
    random_add_noise(&pt, sizeof(pt));

    GlobalMemoryStatus(&memstat);
    random_add_noise(&memstat, sizeof(memstat));

    GetThreadTimes(GetCurrentThread(), times, times + 1, times + 2,
		   times + 3);
    random_add_noise(&times, sizeof(times));
    GetProcessTimes(GetCurrentProcess(), times, times + 1, times + 2,
		    times + 3);
    random_add_noise(&times, sizeof(times));
}

/*
 * This function is called on every keypress or mouse move, and
 * will add the current Windows time and performance monitor
 * counter to the noise pool. It gets the scan code or mouse
 * position passed in.
 */
void noise_ultralight(unsigned long data)
{
    DWORD wintime;
    LARGE_INTEGER perftime;

    random_add_noise(&data, sizeof(DWORD));

    wintime = GetTickCount();
    random_add_noise(&wintime, sizeof(DWORD));

    if (QueryPerformanceCounter(&perftime))
	random_add_noise(&perftime, sizeof(perftime));
}
