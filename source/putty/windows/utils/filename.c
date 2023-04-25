/*
 * Implementation of Filename for Windows.
 */

#include "putty.h"
#ifdef MPEXT
#include <assert.h>
#endif

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

#ifdef WINSCP
const char* in_memory_key_data(const Filename *fn)
{
    const char* result = fn->path;
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

#ifdef WINSCP

FILE * mp_wfopen(const char *filename, const char *mode)
{
    size_t len = strlen(filename);
    wchar_t * wfilename = snewn(len * 10, wchar_t);
    size_t wlen = MultiByteToWideChar(CP_UTF8, 0, filename, -1, wfilename, len * 10);
    FILE * file;
    if (wlen <= 0)
    {
        file = NULL;
    }
    else
    {
        wchar_t wmode[3];
        memset(wmode, 0, sizeof(wmode));
        wmode[0] = (wchar_t)mode[0];
        if (mode[0] != '\0')
        {
            wmode[1] = (wchar_t)mode[1];
            if (mode[1] != '\0')
            {
                assert(mode[2] == '\0');
            }
        }

        file = _wfopen(wfilename, wmode);
    }
    sfree(wfilename);
    return file;
}

#endif
