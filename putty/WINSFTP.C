/*
 * winsftp.c: the Windows-specific parts of PSFTP and PSCP.
 */

#include "putty.h"
#include "psftp.h"

/* ----------------------------------------------------------------------
 * Interface to GUI driver program.
 */

/* This is just a base value from which the main message numbers are
 * derived. */
#define   WM_APP_BASE		0x8000

/* These two pass a single character value in wParam. They represent
 * the visible output from PSCP. */
#define   WM_STD_OUT_CHAR	( WM_APP_BASE+400 )
#define   WM_STD_ERR_CHAR	( WM_APP_BASE+401 )

/* These pass a transfer status update. WM_STATS_CHAR passes a single
 * character in wParam, and is called repeatedly to pass the name of
 * the file, terminated with "\n". WM_STATS_SIZE passes the size of
 * the file being transferred in wParam. WM_STATS_ELAPSED is called
 * to pass the elapsed time (in seconds) in wParam, and
 * WM_STATS_PERCENT passes the percentage of the transfer which is
 * complete, also in wParam. */
#define   WM_STATS_CHAR		( WM_APP_BASE+402 )
#define   WM_STATS_SIZE 	( WM_APP_BASE+403 )
#define   WM_STATS_PERCENT	( WM_APP_BASE+404 )
#define   WM_STATS_ELAPSED	( WM_APP_BASE+405 )

/* These are used at the end of a run to pass an error code in
 * wParam: zero means success, nonzero means failure. WM_RET_ERR_CNT
 * is used after a copy, and WM_LS_RET_ERR_CNT is used after a file
 * list operation. */
#define   WM_RET_ERR_CNT	( WM_APP_BASE+406 )
#define   WM_LS_RET_ERR_CNT	( WM_APP_BASE+407 )

/* More transfer status update messages. WM_STATS_DONE passes the
 * number of bytes sent so far in wParam. WM_STATS_ETA passes the
 * estimated time to completion (in seconds). WM_STATS_RATEBS passes
 * the average transfer rate (in bytes per second). */
#define   WM_STATS_DONE		( WM_APP_BASE+408 )
#define   WM_STATS_ETA		( WM_APP_BASE+409 )
#define   WM_STATS_RATEBS	( WM_APP_BASE+410 )

#define NAME_STR_MAX 2048
static char statname[NAME_STR_MAX + 1];
static unsigned long statsize = 0;
static unsigned long statdone = 0;
static unsigned long stateta = 0;
static unsigned long statratebs = 0;
static int statperct = 0;
static unsigned long statelapsed = 0;

static HWND gui_hwnd = NULL;

static void send_msg(HWND h, UINT message, WPARAM wParam)
{
    while (!PostMessage(h, message, wParam, 0))
	SleepEx(1000, TRUE);
}

void gui_send_char(int is_stderr, int c)
{
    unsigned int msg_id = WM_STD_OUT_CHAR;
    if (is_stderr)
	msg_id = WM_STD_ERR_CHAR;
    send_msg(gui_hwnd, msg_id, (WPARAM) c);
}

void gui_send_errcount(int list, int errs)
{
    unsigned int msg_id = WM_RET_ERR_CNT;
    if (list)
	msg_id = WM_LS_RET_ERR_CNT;
    while (!PostMessage(gui_hwnd, msg_id, (WPARAM) errs, 0))
	SleepEx(1000, TRUE);
}

void gui_update_stats(char *name, unsigned long size,
		      int percentage, unsigned long elapsed,
		      unsigned long done, unsigned long eta,
		      unsigned long ratebs)
{
    unsigned int i;

    if (strcmp(name, statname) != 0) {
	for (i = 0; i < strlen(name); ++i)
	    send_msg(gui_hwnd, WM_STATS_CHAR, (WPARAM) name[i]);
	send_msg(gui_hwnd, WM_STATS_CHAR, (WPARAM) '\n');
	strcpy(statname, name);
    }
    if (statsize != size) {
	send_msg(gui_hwnd, WM_STATS_SIZE, (WPARAM) size);
	statsize = size;
    }
    if (statdone != done) {
	send_msg(gui_hwnd, WM_STATS_DONE, (WPARAM) done);
	statdone = done;
    }
    if (stateta != eta) {
	send_msg(gui_hwnd, WM_STATS_ETA, (WPARAM) eta);
	stateta = eta;
    }
    if (statratebs != ratebs) {
	send_msg(gui_hwnd, WM_STATS_RATEBS, (WPARAM) ratebs);
	statratebs = ratebs;
    }
    if (statelapsed != elapsed) {
	send_msg(gui_hwnd, WM_STATS_ELAPSED, (WPARAM) elapsed);
	statelapsed = elapsed;
    }
    if (statperct != percentage) {
	send_msg(gui_hwnd, WM_STATS_PERCENT, (WPARAM) percentage);
	statperct = percentage;
    }
}

void gui_enable(char *arg)
{
    gui_hwnd = (HWND) atoi(arg);
}

/* ----------------------------------------------------------------------
 * File access abstraction.
 */

/*
 * Set local current directory. Returns NULL on success, or else an
 * error message which must be freed after printing.
 */
char *psftp_lcd(char *dir)
{
    char *ret = NULL;

    if (!SetCurrentDirectory(dir)) {
	LPVOID message;
	int i;
	FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
		      FORMAT_MESSAGE_FROM_SYSTEM |
		      FORMAT_MESSAGE_IGNORE_INSERTS,
		      NULL, GetLastError(),
		      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		      (LPTSTR)&message, 0, NULL);
	i = strcspn((char *)message, "\n");
	ret = dupprintf("%.*s", i, (LPCTSTR)message);
	LocalFree(message);
    }

    return ret;
}

/*
 * Get local current directory. Returns a string which must be
 * freed.
 */
char *psftp_getcwd(void)
{
    char *ret = snewn(256, char);
    int len = GetCurrentDirectory(256, ret);
    if (len > 256)
	ret = sresize(ret, len, char);
    GetCurrentDirectory(len, ret);
    return ret;
}

#define TIME_POSIX_TO_WIN(t, ft) (*(LONGLONG*)&(ft) = \
	((LONGLONG) (t) + (LONGLONG) 11644473600) * (LONGLONG) 10000000)
#define TIME_WIN_TO_POSIX(ft, t) ((t) = (unsigned long) \
	((*(LONGLONG*)&(ft)) / (LONGLONG) 10000000 - (LONGLONG) 11644473600))

struct RFile {
    HANDLE h;
};

RFile *open_existing_file(char *name, unsigned long *size,
			  unsigned long *mtime, unsigned long *atime)
{
    HANDLE h;
    RFile *ret;

    h = CreateFile(name, GENERIC_READ, FILE_SHARE_READ, NULL,
		   OPEN_EXISTING, 0, 0);
    if (h == INVALID_HANDLE_VALUE)
	return NULL;

    ret = snew(RFile);
    ret->h = h;

    if (size)
	*size = GetFileSize(h, NULL);

    if (mtime || atime) {
	FILETIME actime, wrtime;
	GetFileTime(h, NULL, &actime, &wrtime);
	if (atime)
	    TIME_WIN_TO_POSIX(actime, *atime);
	if (mtime)
	    TIME_WIN_TO_POSIX(wrtime, *mtime);
    }

    return ret;
}

int read_from_file(RFile *f, void *buffer, int length)
{
    int ret, read;
    ret = ReadFile(f->h, buffer, length, &read, NULL);
    if (!ret)
	return -1;		       /* error */
    else
	return read;
}

void close_rfile(RFile *f)
{
    CloseHandle(f->h);
    sfree(f);
}

struct WFile {
    HANDLE h;
};

WFile *open_new_file(char *name)
{
    HANDLE h;
    WFile *ret;

    h = CreateFile(name, GENERIC_WRITE, 0, NULL,
		   CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if (h == INVALID_HANDLE_VALUE)
	return NULL;

    ret = snew(WFile);
    ret->h = h;

    return ret;
}

int write_to_file(WFile *f, void *buffer, int length)
{
    int ret, written;
    ret = WriteFile(f->h, buffer, length, &written, NULL);
    if (!ret)
	return -1;		       /* error */
    else
	return written;
}

void set_file_times(WFile *f, unsigned long mtime, unsigned long atime)
{
    FILETIME actime, wrtime;
    TIME_POSIX_TO_WIN(atime, actime);
    TIME_POSIX_TO_WIN(mtime, wrtime);
    SetFileTime(f->h, NULL, &actime, &wrtime);
}

void close_wfile(WFile *f)
{
    CloseHandle(f->h);
    sfree(f);
}

int file_type(char *name)
{
    DWORD attr;
    attr = GetFileAttributes(name);
    /* We know of no `weird' files under Windows. */
    if (attr == (DWORD)-1)
	return FILE_TYPE_NONEXISTENT;
    else if (attr & FILE_ATTRIBUTE_DIRECTORY)
	return FILE_TYPE_DIRECTORY;
    else
	return FILE_TYPE_FILE;
}

struct DirHandle {
    HANDLE h;
    char *name;
};

DirHandle *open_directory(char *name)
{
    HANDLE h;
    WIN32_FIND_DATA fdat;
    char *findfile;
    DirHandle *ret;

    /* To enumerate files in dir `foo', we search for `foo/*'. */
    findfile = dupcat(name, "/*", NULL);
    h = FindFirstFile(findfile, &fdat);
    if (h == INVALID_HANDLE_VALUE)
	return NULL;
    sfree(findfile);

    ret = snew(DirHandle);
    ret->h = h;
    ret->name = dupstr(fdat.cFileName);
    return ret;
}

char *read_filename(DirHandle *dir)
{
    while (!dir->name) {
	WIN32_FIND_DATA fdat;
	int ok = FindNextFile(dir->h, &fdat);

	if (!ok)
	    return NULL;

	if (fdat.cFileName[0] == '.' &&
	    (fdat.cFileName[1] == '\0' ||
	     (fdat.cFileName[1] == '.' && fdat.cFileName[2] == '\0')))
	    dir->name = NULL;
	else
	    dir->name = dupstr(fdat.cFileName);
    }

    if (dir->name) {
	char *ret = dir->name;
	dir->name = NULL;
	return ret;
    } else
	return NULL;
}

void close_directory(DirHandle *dir)
{
    FindClose(dir->h);
    if (dir->name)
	sfree(dir->name);
    sfree(dir);
}

int test_wildcard(char *name, int cmdline)
{
    HANDLE fh;
    WIN32_FIND_DATA fdat;

    /* First see if the exact name exists. */
    if (GetFileAttributes(name) != (DWORD)-1)
	return WCTYPE_FILENAME;

    /* Otherwise see if a wildcard match finds anything. */
    fh = FindFirstFile(name, &fdat);
    if (fh == INVALID_HANDLE_VALUE)
	return WCTYPE_NONEXISTENT;

    FindClose(fh);
    return WCTYPE_WILDCARD;
}

struct WildcardMatcher {
    HANDLE h;
    char *name;
    char *srcpath;
};

/*
 * Return a pointer to the portion of str that comes after the last
 * slash (or backslash or colon, if `local' is TRUE).
 */
static char *stripslashes(char *str, int local)
{
    char *p;

    if (local) {
        p = strchr(str, ':');
        if (p) str = p+1;
    }

    p = strrchr(str, '/');
    if (p) str = p+1;

    if (local) {
	p = strrchr(str, '\\');
	if (p) str = p+1;
    }

    return str;
}

WildcardMatcher *begin_wildcard_matching(char *name)
{
    HANDLE h;
    WIN32_FIND_DATA fdat;
    WildcardMatcher *ret;
    char *last;

    h = FindFirstFile(name, &fdat);
    if (h == INVALID_HANDLE_VALUE)
	return NULL;

    ret = snew(WildcardMatcher);
    ret->h = h;
    ret->srcpath = dupstr(name);
    last = stripslashes(ret->srcpath, 1);
    *last = '\0';
    if (fdat.cFileName[0] == '.' &&
	(fdat.cFileName[1] == '\0' ||
	 (fdat.cFileName[1] == '.' && fdat.cFileName[2] == '\0')))
	ret->name = NULL;
    else
	ret->name = dupcat(ret->srcpath, fdat.cFileName, NULL);

    return ret;
}

char *wildcard_get_filename(WildcardMatcher *dir)
{
    while (!dir->name) {
	WIN32_FIND_DATA fdat;
	int ok = FindNextFile(dir->h, &fdat);

	if (!ok)
	    return NULL;

	if (fdat.cFileName[0] == '.' &&
	    (fdat.cFileName[1] == '\0' ||
	     (fdat.cFileName[1] == '.' && fdat.cFileName[2] == '\0')))
	    dir->name = NULL;
	else
	    dir->name = dupcat(dir->srcpath, fdat.cFileName, NULL);
    }

    if (dir->name) {
	char *ret = dir->name;
	dir->name = NULL;
	return ret;
    } else
	return NULL;
}

void finish_wildcard_matching(WildcardMatcher *dir)
{
    FindClose(dir->h);
    if (dir->name)
	sfree(dir->name);
    sfree(dir->srcpath);
    sfree(dir);
}

int create_directory(char *name)
{
    return CreateDirectory(name, NULL) != 0;
}

char *dir_file_cat(char *dir, char *file)
{
    return dupcat(dir, "\\", file, NULL);
}

/* ----------------------------------------------------------------------
 * Platform-specific network handling.
 */

/*
 * Be told what socket we're supposed to be using.
 */
static SOCKET sftp_ssh_socket;
char *do_select(SOCKET skt, int startup)
{
    if (startup)
	sftp_ssh_socket = skt;
    else
	sftp_ssh_socket = INVALID_SOCKET;
    return NULL;
}
extern int select_result(WPARAM, LPARAM);

/*
 * Wait for some network data and process it.
 */
int ssh_sftp_loop_iteration(void)
{
    fd_set readfds;

    if (sftp_ssh_socket == INVALID_SOCKET)
	return -1;		       /* doom */

    FD_ZERO(&readfds);
    FD_SET(sftp_ssh_socket, &readfds);
    if (p_select(1, &readfds, NULL, NULL, NULL) < 0)
	return -1;		       /* doom */

    select_result((WPARAM) sftp_ssh_socket, (LPARAM) FD_READ);
    return 0;
}

/* ----------------------------------------------------------------------
 * Main program. Parse arguments etc.
 */
int main(int argc, char *argv[])
{
    int ret;

    ret = psftp_main(argc, argv);

    return ret;
}
