/*
 * Implementation of Filename for Windows.
 */

#include <wchar.h>

#include "putty.h"
#ifdef MPEXT
#include <assert.h>
#endif

Filename *filename_from_str(const char *str)
{
    Filename *fn = snew(Filename);
    fn->cpath = dupstr(str);
    fn->wpath = dup_mb_to_wc(DEFAULT_CODEPAGE, fn->cpath);
    fn->utf8path = encode_wide_string_as_utf8(fn->wpath);
    return fn;
}

Filename *filename_from_wstr(const wchar_t *str)
{
    Filename *fn = snew(Filename);
    fn->wpath = dupwcs(str);
    fn->cpath = dup_wc_to_mb(DEFAULT_CODEPAGE, fn->wpath, "?");
    fn->utf8path = encode_wide_string_as_utf8(fn->wpath);
    return fn;
}

Filename *filename_from_utf8(const char *ustr)
{
    Filename *fn = snew(Filename);
    fn->utf8path = dupstr(ustr);
    fn->wpath = decode_utf8_to_wide_string(fn->utf8path);
    fn->cpath = dup_wc_to_mb(DEFAULT_CODEPAGE, fn->wpath, "?");
    return fn;
}

Filename *filename_copy(const Filename *fn)
{
    Filename *newfn = snew(Filename);
    newfn->cpath = dupstr(fn->cpath);
    newfn->wpath = dupwcs(fn->wpath);
    newfn->utf8path = dupstr(fn->utf8path);
    return newfn;
}

#ifdef WINSCP
const char* in_memory_key_data(const Filename *fn)
{
    const char* result = fn->cpath;
    if (result[0] != '@')
    {
        result = NULL;
    }
    else
    {
        int len;
        result++;
        len = strlen(result);
        if (((len % 2) != 0) ||
            ((len / 2) < MAX_PATH))
        {
            result = NULL;
        }
        else
        {
            int i;
            for (i = 0; (result != NULL) && (i < len); i++)
            {
                 if (!isxdigit(result[i]))
                 {
                     result = NULL;
                 }
            }
        }
    }
    return result;
}
#endif

const char *filename_to_str(const Filename *fn)
{
    #ifdef WINSCP
    if (in_memory_key_data(fn) != NULL) return "in-memory";
    #endif
    // WINSCP
    return fn->utf8path;                  /* FIXME */
}

const wchar_t *filename_to_wstr(const Filename *fn)
{
    return fn->wpath;
}

bool filename_equal(const Filename *f1, const Filename *f2)
{
    /* wpath is primary: two filenames refer to the same file if they
     * have the same wpath */
    return !wcscmp(f1->wpath, f2->wpath);
}

bool filename_is_null(const Filename *fn)
{
    return !*fn->wpath;
}

void filename_free(Filename *fn)
{
    sfree(fn->wpath);
    sfree(fn->cpath);
    sfree(fn->utf8path);
    sfree(fn);
}

void filename_serialise(BinarySink *bs, const Filename *f)
{
    put_asciz(bs, f->utf8path);
}
Filename *filename_deserialise(BinarySource *src)
{
    const char *utf8 = get_asciz(src);
    return filename_from_utf8(utf8);
}

char filename_char_sanitise(char c)
{
    if (strchr("<>:\"/\\|?*", c))
        return '.';
    return c;
}

FILE *f_open(const Filename *fn, const char *mode, bool isprivate)
{
#ifdef LEGACY_WINDOWS
    /* Fallback for legacy pre-NT windows, where as far as I can see
     * _wfopen just doesn't work at all */
    init_winver();
    if (osPlatformId == VER_PLATFORM_WIN32_WINDOWS ||
        osPlatformId == VER_PLATFORM_WIN32s)
        return fopen(fn->cpath, mode);
#endif

    wchar_t *wmode = dup_mb_to_wc(DEFAULT_CODEPAGE, mode);
    FILE *fp = _wfopen(fn->wpath, wmode);
    sfree(wmode);
    return fp;
}
