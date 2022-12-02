/*
 * Implement convenience wrappers on the awkward low-level functions
 * for accessing the Windows registry.
 */

#include "putty.h"

HKEY open_regkey_fn(bool create, HKEY hk, const char *path, ...)
{
    HKEY toret = NULL;
    bool hk_needs_close = false;
    va_list ap;
    va_start(ap, path);
    assert(!reg_override_winscp());

    for (; path; path = va_arg(ap, const char *)) {
        HKEY hk_sub = NULL;

        LONG status;
        if (create)
            status = RegCreateKeyEx(
                hk, path, 0, NULL, REG_OPTION_NON_VOLATILE,
                KEY_READ | KEY_WRITE, NULL, &hk_sub, NULL);
        else
            status = RegOpenKeyEx(
                hk, path, 0, KEY_READ | KEY_WRITE, &hk_sub);

        if (status != ERROR_SUCCESS)
            goto out;

        if (hk_needs_close)
            RegCloseKey(hk);
        hk = hk_sub;
        hk_needs_close = true;
    }

    toret = hk;
    hk = NULL;
    hk_needs_close = false;

  out:
    va_end(ap);
    if (hk_needs_close)
        RegCloseKey(hk);
    return toret;
}

void close_regkey(HKEY key)
{
    if (reg_override_winscp())
    {
        close_regkey_winscp(key);
        return;
    }
    RegCloseKey(key);
}

#ifndef WINSCP
void del_regkey(HKEY key, const char *name)
{
    RegDeleteKey(key, name);
}
#endif

char *enum_regkey(HKEY key, int index)
{
    size_t regbuf_size = MAX_PATH + 1;
    char *regbuf = snewn(regbuf_size, char);
    assert(!reg_override_winscp());

    while (1) {
        LONG status = RegEnumKey(key, index, regbuf, regbuf_size);
        if (status == ERROR_SUCCESS)
            return regbuf;
        if (status != ERROR_MORE_DATA) {
            sfree(regbuf);
            return NULL;
        }
        sgrowarray(regbuf, regbuf_size, regbuf_size);
    }
}

bool get_reg_dword(HKEY key, const char *name, DWORD *out)
{
    DWORD type, size;
    if (reg_override_winscp())
    {
        return get_reg_dword_winscp(key, name, out);
    }
    size = sizeof(*out);

    if (RegQueryValueEx(key, name, 0, &type,
                        (BYTE *)out, &size) != ERROR_SUCCESS ||
        size != sizeof(*out) || type != REG_DWORD)
        return false;
    else
        return true;
}

bool put_reg_dword(HKEY key, const char *name, DWORD value)
{
    if (reg_override_winscp())
    {
        return put_reg_dword_winscp(key, name, value);
    }

    return RegSetValueEx(key, name, 0, REG_DWORD, (CONST BYTE *) &value,
                         sizeof(value)) == ERROR_SUCCESS;
}

char *get_reg_sz(HKEY key, const char *name)
{
    DWORD type, size;

    if (reg_override_winscp())
    {
        return get_reg_sz_winscp(key, name);
    }

    if (RegQueryValueEx(key, name, 0, &type, NULL,
                        &size) != ERROR_SUCCESS || type != REG_SZ)
        return NULL;                   /* not a string */

    { // WINSCP
    size_t allocsize = size+1;         /* allow for an extra NUL if needed */
    char *toret = snewn(allocsize, char);
    if (RegQueryValueEx(key, name, 0, &type, (BYTE *)toret,
                        &size) != ERROR_SUCCESS || type != REG_SZ) {
        sfree(toret);
        return NULL;
    }
    assert(size < allocsize);
    toret[size] = '\0'; /* add an extra NUL in case RegQueryValueEx
                         * didn't supply one */

    return toret;
    } // WINSCP
}

bool put_reg_sz(HKEY key, const char *name, const char *str)
{
    if (reg_override_winscp())
    {
        return put_reg_sz_winscp(key, name, str);
    }

    /* You have to store the trailing NUL as well */
    return RegSetValueEx(key, name, 0, REG_SZ, (CONST BYTE *)str,
                         1 + strlen(str)) == ERROR_SUCCESS;
}

#ifndef WINSCP
/*
 * REG_MULTI_SZ items are stored as a concatenation of NUL-terminated
 * strings, terminated in turn with an empty string, i.e. a second
 * consecutive NUL.
 *
 * We represent these in their storage format, as a strbuf - but
 * *without* the second consecutive NUL.
 *
 * So you can build up a new MULTI_SZ value in a strbuf by calling
 * put_asciz once per output string and then put_reg_multi_sz; and you
 * can consume one by initialising a BinarySource to the result of
 * get_reg_multi_sz, and then calling get_asciz on it and assuming
 * that !get_err(src) means you have a real output string.
 *
 * Also, calling strbuf_to_str on one of these will give you back a
 * bare 'char *' with the same double-NUL termination, to pass back to
 * a caller.
 */
strbuf *get_reg_multi_sz(HKEY key, const char *name)
{
    DWORD type, size;

    if (RegQueryValueEx(key, name, 0, &type, NULL,
                        &size) != ERROR_SUCCESS || type != REG_MULTI_SZ)
        return NULL;                   /* not a string */

    strbuf *toret = strbuf_new();
    void *ptr = strbuf_append(toret, (size_t)size + 2);
    if (RegQueryValueEx(key, name, 0, &type, (BYTE *)ptr,
                        &size) != ERROR_SUCCESS || type != REG_MULTI_SZ) {
        strbuf_free(toret);
        return NULL;
    }
    strbuf_shrink_to(toret, size);
    /* Ensure we end with exactly one \0 */
    while (strbuf_chomp(toret, '\0'));
    put_byte(toret, '\0');
    return toret;
}

bool put_reg_multi_sz(HKEY key, const char *name, strbuf *str)
{
    /*
     * Of course, to write our string list into the registry, we _do_
     * have to include both trailing NULs. But this is easy, because a
     * strbuf is also designed to hold a single string and make it
     * conveniently accessible in NUL-terminated form, so it stores a
     * NUL in its buffer just beyond its formal length. So we just
     * include that extra byte in the data we write.
     */
    return RegSetValueEx(key, name, 0, REG_MULTI_SZ, (CONST BYTE *)str->s,
                         str->len + 1) == ERROR_SUCCESS;
}

char *get_reg_sz_simple(HKEY key, const char *name, const char *leaf)
{
    HKEY subkey = open_regkey(false, key, name);
    if (!subkey)
        return NULL;
    char *toret = get_reg_sz(subkey, leaf);
    RegCloseKey(subkey);
    return toret;
}
#endif // WINSCP
