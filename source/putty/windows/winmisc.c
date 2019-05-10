/*
 * winmisc.c: miscellaneous Windows-specific things
 */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include "putty.h"
#ifndef SECURITY_WIN32
#define SECURITY_WIN32
#endif
#include <security.h>

DWORD osMajorVersion, osMinorVersion, osPlatformId;

char *platform_get_x_display(void) {
    /* We may as well check for DISPLAY in case it's useful. */
    return dupstr(getenv("DISPLAY"));
}

Filename *filename_from_str(const char *str)
{
    Filename *ret = snew(Filename);
    ret->path = dupstr(str);
    return ret;
}

Filename *filename_copy(const Filename *fn)
{
    return filename_from_str(fn->path);
}

const char *filename_to_str(const Filename *fn)
{
    return fn->path;
}

bool filename_equal(const Filename *f1, const Filename *f2)
{
    return !strcmp(f1->path, f2->path);
}

bool filename_is_null(const Filename *fn)
{
    return !*fn->path;
}

void filename_free(Filename *fn)
{
    sfree(fn->path);
    sfree(fn);
}

void filename_serialise(BinarySink *bs, const Filename *f)
{
    put_asciz(bs, f->path);
}
Filename *filename_deserialise(BinarySource *src)
{
    return filename_from_str(get_asciz(src));
}

char filename_char_sanitise(char c)
{
    if (strchr("<>:\"/\\|?*", c))
        return '.';
    return c;
}

char *get_username(void)
{
    DWORD namelen;
    char *user;
    bool got_username = false;
    DECL_WINDOWS_FUNCTION(static, BOOLEAN, GetUserNameExA,
			  (EXTENDED_NAME_FORMAT, LPSTR, PULONG));

    {
	static bool tried_usernameex = false;
	if (!tried_usernameex) {
	    /* Not available on Win9x, so load dynamically */
	    HMODULE secur32 = load_system32_dll("secur32.dll");
	    /* If MIT Kerberos is installed, the following call to
	       GET_WINDOWS_FUNCTION makes Windows implicitly load
	       sspicli.dll WITHOUT proper path sanitizing, so better
	       load it properly before */
	    HMODULE sspicli = load_system32_dll("sspicli.dll");
            (void)sspicli; /* squash compiler warning about unused variable */
	    GET_WINDOWS_FUNCTION(secur32, GetUserNameExA);
	    tried_usernameex = true;
	}
    }

    if (p_GetUserNameExA) {
	/*
	 * If available, use the principal -- this avoids the problem
	 * that the local username is case-insensitive but Kerberos
	 * usernames are case-sensitive.
	 */

	/* Get the length */
	namelen = 0;
	(void) p_GetUserNameExA(NameUserPrincipal, NULL, &namelen);

	user = snewn(namelen, char);
	got_username = p_GetUserNameExA(NameUserPrincipal, user, &namelen);
	if (got_username) {
	    char *p = strchr(user, '@');
	    if (p) *p = 0;
	} else {
	    sfree(user);
	}
    }

    if (!got_username) {
	/* Fall back to local user name */
	namelen = 0;
	if (!GetUserName(NULL, &namelen)) {
	    /*
	     * Apparently this doesn't work at least on Windows XP SP2.
	     * Thus assume a maximum of 256. It will fail again if it
	     * doesn't fit.
	     */
	    namelen = 256;
	}

	user = snewn(namelen, char);
	got_username = GetUserName(user, &namelen);
	if (!got_username) {
	    sfree(user);
	}
    }

    return got_username ? user : NULL;
}

void dll_hijacking_protection(void)
{
    /*
     * If the OS provides it, call SetDefaultDllDirectories() to
     * prevent DLLs from being loaded from the directory containing
     * our own binary, and instead only load from system32.
     *
     * This is a protection against hijacking attacks, if someone runs
     * PuTTY directly from their web browser's download directory
     * having previously been enticed into clicking on an unwise link
     * that downloaded a malicious DLL to the same directory under one
     * of various magic names that seem to be things that standard
     * Windows DLLs delegate to.
     *
     * It shouldn't break deliberate loading of user-provided DLLs
     * such as GSSAPI providers, because those are specified by their
     * full pathname by the user-provided configuration.
     */
    static HMODULE kernel32_module;
    DECL_WINDOWS_FUNCTION(static, BOOL, SetDefaultDllDirectories, (DWORD));

    if (!kernel32_module) {
        kernel32_module = load_system32_dll("kernel32.dll");
#if (defined _MSC_VER && _MSC_VER < 1900) || defined COVERITY
        /* For older Visual Studio, and also for the system I
         * currently use for Coveritying the Windows code, this
         * function isn't available in the header files to
         * type-check */
        GET_WINDOWS_FUNCTION_NO_TYPECHECK(
            kernel32_module, SetDefaultDllDirectories);
#else
        GET_WINDOWS_FUNCTION(kernel32_module, SetDefaultDllDirectories);
#endif
    }

    if (p_SetDefaultDllDirectories) {
        /* LOAD_LIBRARY_SEARCH_SYSTEM32 and explicitly specified
         * directories only */
        p_SetDefaultDllDirectories(LOAD_LIBRARY_SEARCH_SYSTEM32 |
                                   LOAD_LIBRARY_SEARCH_USER_DIRS);
    }
}

void init_winver(void)
{
    OSVERSIONINFO osVersion;
    static HMODULE kernel32_module;
    DECL_WINDOWS_FUNCTION(static, BOOL, GetVersionExA, (LPOSVERSIONINFO));

    if (!kernel32_module) {
        kernel32_module = load_system32_dll("kernel32.dll");
        /* Deliberately don't type-check this function, because that
         * would involve using its declaration in a header file which
         * triggers a deprecation warning. I know it's deprecated (see
         * below) and don't need telling. */
        GET_WINDOWS_FUNCTION_NO_TYPECHECK(kernel32_module, GetVersionExA);
    }

    ZeroMemory(&osVersion, sizeof(osVersion));
    osVersion.dwOSVersionInfoSize = sizeof (OSVERSIONINFO);
    if (p_GetVersionExA && p_GetVersionExA(&osVersion)) {
        osMajorVersion = osVersion.dwMajorVersion;
        osMinorVersion = osVersion.dwMinorVersion;
        osPlatformId = osVersion.dwPlatformId;
    } else {
        /*
         * GetVersionEx is deprecated, so allow for it perhaps going
         * away in future API versions. If it's not there, simply
         * assume that's because Windows is too _new_, so fill in the
         * variables we care about to a value that will always compare
         * higher than any given test threshold.
         *
         * Normally we should be checking against the presence of a
         * specific function if possible in any case.
         */
        osMajorVersion = osMinorVersion = UINT_MAX; /* a very high number */
        osPlatformId = VER_PLATFORM_WIN32_NT; /* not Win32s or Win95-like */
    }
}

HMODULE load_system32_dll(const char *libname)
{
    /*
     * Wrapper function to load a DLL out of c:\windows\system32
     * without going through the full DLL search path. (Hence no
     * attack is possible by placing a substitute DLL earlier on that
     * path.)
     */
    static char *sysdir = NULL;
    static size_t sysdirsize = 0;
    char *fullpath;
    HMODULE ret;

    if (!sysdir) {
        size_t len;
        while ((len = GetSystemDirectory(sysdir, sysdirsize)) >= sysdirsize)
            sgrowarray(sysdir, sysdirsize, len);
    }

    fullpath = dupcat(sysdir, "\\", libname, NULL);
    ret = LoadLibrary(fullpath);
    sfree(fullpath);
    return ret;
}

/*
 * A tree234 containing mappings from system error codes to strings.
 */

struct errstring {
    int error;
    char *text;
};

static int errstring_find(void *av, void *bv)
{
    int *a = (int *)av;
    struct errstring *b = (struct errstring *)bv;
    if (*a < b->error)
        return -1;
    if (*a > b->error)
        return +1;
    return 0;
}
static int errstring_compare(void *av, void *bv)
{
    struct errstring *a = (struct errstring *)av;
    return errstring_find(&a->error, bv);
}

static tree234 *errstrings = NULL;

const char *win_strerror(int error)
{
    struct errstring *es;

    if (!errstrings)
        errstrings = newtree234(errstring_compare);

    es = find234(errstrings, &error, errstring_find);

    if (!es) {
        char msgtext[65536]; /* maximum size for FormatMessage is 64K */

        es = snew(struct errstring);
        es->error = error;
        if (!FormatMessage((FORMAT_MESSAGE_FROM_SYSTEM |
                            FORMAT_MESSAGE_IGNORE_INSERTS), NULL, error,
                           MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                           msgtext, lenof(msgtext)-1, NULL)) {
            sprintf(msgtext,
                    "(unable to format: FormatMessage returned %u)",
                    (unsigned int)GetLastError());
        } else {
            int len = strlen(msgtext);
            if (len > 0 && msgtext[len-1] == '\n')
                msgtext[len-1] = '\0';
        }
        es->text = dupprintf("Error %d: %s", error, msgtext);
        add234(errstrings, es);
    }

    return es->text;
}

FontSpec *fontspec_new(const char *name, bool bold, int height, int charset)
{
    FontSpec *f = snew(FontSpec);
    f->name = dupstr(name);
    f->isbold = bold;
    f->height = height;
    f->charset = charset;
    return f;
}
FontSpec *fontspec_copy(const FontSpec *f)
{
    return fontspec_new(f->name, f->isbold, f->height, f->charset);
}
void fontspec_free(FontSpec *f)
{
    sfree(f->name);
    sfree(f);
}
void fontspec_serialise(BinarySink *bs, FontSpec *f)
{
    put_asciz(bs, f->name);
    put_uint32(bs, f->isbold);
    put_uint32(bs, f->height);
    put_uint32(bs, f->charset);
}
FontSpec *fontspec_deserialise(BinarySource *src)
{
    const char *name = get_asciz(src);
    unsigned isbold = get_uint32(src);
    unsigned height = get_uint32(src);
    unsigned charset = get_uint32(src);
    return fontspec_new(name, isbold, height, charset);
}

bool open_for_write_would_lose_data(const Filename *fn)
{
    WIN32_FILE_ATTRIBUTE_DATA attrs;
    if (!GetFileAttributesEx(fn->path, GetFileExInfoStandard, &attrs)) {
        /*
         * Generally, if we don't identify a specific reason why we
         * should return true from this function, we return false, and
         * let the subsequent attempt to open the file for real give a
         * more useful error message.
         */
        return false;
    }
    if (attrs.dwFileAttributes & (FILE_ATTRIBUTE_DEVICE |
                                  FILE_ATTRIBUTE_DIRECTORY)) {
        /*
         * File is something other than an ordinary disk file, so
         * opening it for writing will not cause truncation. (It may
         * not _succeed_ either, but that's not our problem here!)
         */
        return false;
    }
    if (attrs.nFileSizeHigh == 0 && attrs.nFileSizeLow == 0) {
        /*
         * File is zero-length (or may be a named pipe, which
         * dwFileAttributes can't tell apart from a regular file), so
         * opening it for writing won't truncate any data away because
         * there's nothing to truncate anyway.
         */
        return false;
    }
    return true;
}

void escape_registry_key(const char *in, strbuf *out)
{
    bool candot = false;
    static const char hex[16] = "0123456789ABCDEF";

    while (*in) {
	if (*in == ' ' || *in == '\\' || *in == '*' || *in == '?' ||
	    *in == '%' || *in < ' ' || *in > '~' || (*in == '.'
						     && !candot)) {
            put_byte(out, '%');
	    put_byte(out, hex[((unsigned char) *in) >> 4]);
	    put_byte(out, hex[((unsigned char) *in) & 15]);
	} else
	    put_byte(out, *in);
	in++;
	candot = true;
    }
}

void unescape_registry_key(const char *in, strbuf *out)
{
    while (*in) {
	if (*in == '%' && in[1] && in[2]) {
	    int i, j;

	    i = in[1] - '0';
	    i -= (i > 9 ? 7 : 0);
	    j = in[2] - '0';
	    j -= (j > 9 ? 7 : 0);

	    put_byte(out, (i << 4) + j);
	    in += 3;
	} else {
            put_byte(out, *in++);
	}
    }
}

#ifdef DEBUG
static FILE *debug_fp = NULL;
static HANDLE debug_hdl = INVALID_HANDLE_VALUE;
static int debug_got_console = 0;

void dputs(const char *buf)
{
    DWORD dw;

    if (!debug_got_console) {
	if (AllocConsole()) {
	    debug_got_console = 1;
	    debug_hdl = GetStdHandle(STD_OUTPUT_HANDLE);
	}
    }
    if (!debug_fp) {
	debug_fp = fopen("debug.log", "w");
    }

    if (debug_hdl != INVALID_HANDLE_VALUE) {
	WriteFile(debug_hdl, buf, strlen(buf), &dw, NULL);
    }
    fputs(buf, debug_fp);
    fflush(debug_fp);
}
#endif

char *registry_get_string(HKEY root, const char *path, const char *leaf)
{
    HKEY key = root;
    bool need_close_key = false;
    char *toret = NULL, *str = NULL;

    if (path) {
        if (RegCreateKey(key, path, &key) != ERROR_SUCCESS)
            goto out;
        need_close_key = true;
    }

    DWORD type, size;
    if (RegQueryValueEx(key, leaf, 0, &type, NULL, &size) != ERROR_SUCCESS)
        goto out;
    if (type != REG_SZ)
        goto out;

    str = snewn(size + 1, char);
    DWORD size_got = size;
    if (RegQueryValueEx(key, leaf, 0, &type, (LPBYTE)str,
                        &size_got) != ERROR_SUCCESS)
        goto out;
    if (type != REG_SZ || size_got > size)
        goto out;
    str[size_got] = '\0';

    toret = str;
    str = NULL;

  out:
    if (need_close_key)
        RegCloseKey(key);
    sfree(str);
    return toret;
}
