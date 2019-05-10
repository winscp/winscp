/*
 * winstore.c: Windows-specific implementation of the interface
 * defined in storage.h.
 */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>
#include "putty.h"
#include "storage.h"

#include <shlobj.h>
#ifndef CSIDL_APPDATA
#define CSIDL_APPDATA 0x001a
#endif
#ifndef CSIDL_LOCAL_APPDATA
#define CSIDL_LOCAL_APPDATA 0x001c
#endif

static const char *const reg_jumplist_key = PUTTY_REG_POS "\\Jumplist";
static const char *const reg_jumplist_value = "Recent sessions";
static const char *const puttystr = PUTTY_REG_POS "\\Sessions";

static bool tried_shgetfolderpath = false;
static HMODULE shell32_module = NULL;
DECL_WINDOWS_FUNCTION(static, HRESULT, SHGetFolderPathA, 
		      (HWND, int, HANDLE, DWORD, LPSTR));

struct settings_w {
    HKEY sesskey;
};

settings_w *open_settings_w(const char *sessionname, char **errmsg)
{
    HKEY subkey1, sesskey;
    int ret;
    strbuf *sb;

    *errmsg = NULL;

    if (!sessionname || !*sessionname)
	sessionname = "Default Settings";

    sb = strbuf_new();
    escape_registry_key(sessionname, sb);

    ret = RegCreateKey(HKEY_CURRENT_USER, puttystr, &subkey1);
    if (ret != ERROR_SUCCESS) {
	strbuf_free(sb);
        *errmsg = dupprintf("Unable to create registry key\n"
                            "HKEY_CURRENT_USER\\%s", puttystr);
	return NULL;
    }
    ret = RegCreateKey(subkey1, sb->s, &sesskey);
    RegCloseKey(subkey1);
    if (ret != ERROR_SUCCESS) {
        *errmsg = dupprintf("Unable to create registry key\n"
                            "HKEY_CURRENT_USER\\%s\\%s", puttystr, sb->s);
	strbuf_free(sb);
	return NULL;
    }
    strbuf_free(sb);

    settings_w *toret = snew(settings_w);
    toret->sesskey = sesskey;
    return toret;
}

void write_setting_s(settings_w *handle, const char *key, const char *value)
{
    if (handle)
        RegSetValueEx(handle->sesskey, key, 0, REG_SZ, (CONST BYTE *)value,
		      1 + strlen(value));
}

void write_setting_i(settings_w *handle, const char *key, int value)
{
    if (handle)
        RegSetValueEx(handle->sesskey, key, 0, REG_DWORD,
		      (CONST BYTE *) &value, sizeof(value));
}

void close_settings_w(settings_w *handle)
{
    RegCloseKey(handle->sesskey);
    sfree(handle);
}

struct settings_r {
    HKEY sesskey;
};

settings_r *open_settings_r(const char *sessionname)
{
    HKEY subkey1, sesskey;
    strbuf *sb;

    if (!sessionname || !*sessionname)
	sessionname = "Default Settings";

    sb = strbuf_new();
    escape_registry_key(sessionname, sb);

    if (RegOpenKey(HKEY_CURRENT_USER, puttystr, &subkey1) != ERROR_SUCCESS) {
	sesskey = NULL;
    } else {
	if (RegOpenKey(subkey1, sb->s, &sesskey) != ERROR_SUCCESS) {
	    sesskey = NULL;
	}
	RegCloseKey(subkey1);
    }

    strbuf_free(sb);

    if (!sesskey)
        return NULL;

    settings_r *toret = snew(settings_r);
    toret->sesskey = sesskey;
    return toret;
}

char *read_setting_s(settings_r *handle, const char *key)
{
    DWORD type, allocsize, size;
    char *ret;

    if (!handle)
	return NULL;

    /* Find out the type and size of the data. */
    if (RegQueryValueEx(handle->sesskey, key, 0,
			&type, NULL, &size) != ERROR_SUCCESS ||
	type != REG_SZ)
	return NULL;

    allocsize = size+1;         /* allow for an extra NUL if needed */
    ret = snewn(allocsize, char);
    if (RegQueryValueEx(handle->sesskey, key, 0,
			&type, (BYTE *)ret, &size) != ERROR_SUCCESS ||
	type != REG_SZ) {
        sfree(ret);
        return NULL;
    }
    assert(size < allocsize);
    ret[size] = '\0'; /* add an extra NUL in case RegQueryValueEx
                       * didn't supply one */

    return ret;
}

int read_setting_i(settings_r *handle, const char *key, int defvalue)
{
    DWORD type, val, size;
    size = sizeof(val);

    if (!handle ||
        RegQueryValueEx(handle->sesskey, key, 0, &type,
			(BYTE *) &val, &size) != ERROR_SUCCESS ||
	size != sizeof(val) || type != REG_DWORD)
	return defvalue;
    else
	return val;
}

FontSpec *read_setting_fontspec(settings_r *handle, const char *name)
{
    char *settingname;
    char *fontname;
    FontSpec *ret;
    int isbold, height, charset;

    fontname = read_setting_s(handle, name);
    if (!fontname)
	return NULL;

    settingname = dupcat(name, "IsBold", NULL);
    isbold = read_setting_i(handle, settingname, -1);
    sfree(settingname);
    if (isbold == -1) {
        sfree(fontname);
        return NULL;
    }

    settingname = dupcat(name, "CharSet", NULL);
    charset = read_setting_i(handle, settingname, -1);
    sfree(settingname);
    if (charset == -1) {
        sfree(fontname);
        return NULL;
    }

    settingname = dupcat(name, "Height", NULL);
    height = read_setting_i(handle, settingname, INT_MIN);
    sfree(settingname);
    if (height == INT_MIN) {
        sfree(fontname);
        return NULL;
    }

    ret = fontspec_new(fontname, isbold, height, charset);
    sfree(fontname);
    return ret;
}

void write_setting_fontspec(settings_w *handle,
                            const char *name, FontSpec *font)
{
    char *settingname;

    write_setting_s(handle, name, font->name);
    settingname = dupcat(name, "IsBold", NULL);
    write_setting_i(handle, settingname, font->isbold);
    sfree(settingname);
    settingname = dupcat(name, "CharSet", NULL);
    write_setting_i(handle, settingname, font->charset);
    sfree(settingname);
    settingname = dupcat(name, "Height", NULL);
    write_setting_i(handle, settingname, font->height);
    sfree(settingname);
}

Filename *read_setting_filename(settings_r *handle, const char *name)
{
    char *tmp = read_setting_s(handle, name);
    if (tmp) {
        Filename *ret = filename_from_str(tmp);
	sfree(tmp);
	return ret;
    } else
	return NULL;
}

void write_setting_filename(settings_w *handle,
                            const char *name, Filename *result)
{
    write_setting_s(handle, name, result->path);
}

void close_settings_r(settings_r *handle)
{
    if (handle) {
        RegCloseKey(handle->sesskey);
        sfree(handle);
    }
}

void del_settings(const char *sessionname)
{
    HKEY subkey1;
    strbuf *sb;

    if (RegOpenKey(HKEY_CURRENT_USER, puttystr, &subkey1) != ERROR_SUCCESS)
	return;

    sb = strbuf_new();
    escape_registry_key(sessionname, sb);
    RegDeleteKey(subkey1, sb->s);
    strbuf_free(sb);

    RegCloseKey(subkey1);

    remove_session_from_jumplist(sessionname);
}

struct settings_e {
    HKEY key;
    int i;
};

settings_e *enum_settings_start(void)
{
    settings_e *ret;
    HKEY key;

    if (RegOpenKey(HKEY_CURRENT_USER, puttystr, &key) != ERROR_SUCCESS)
	return NULL;

    ret = snew(settings_e);
    if (ret) {
	ret->key = key;
	ret->i = 0;
    }

    return ret;
}

bool enum_settings_next(settings_e *e, strbuf *sb)
{
    size_t regbuf_size = 256;
    char *regbuf = snewn(regbuf_size, char);
    bool success;

    while (1) {
        DWORD retd = RegEnumKey(e->key, e->i++, regbuf, regbuf_size);
        if (retd != ERROR_MORE_DATA) {
            success = (retd == ERROR_SUCCESS);
            break;
        }
        sgrowarray(regbuf, regbuf_size, regbuf_size);
    }

    if (success)
        unescape_registry_key(regbuf, sb);

    sfree(regbuf);
    return success;
}

void enum_settings_finish(settings_e *e)
{
    RegCloseKey(e->key);
    sfree(e);
}

static void hostkey_regname(strbuf *sb, const char *hostname,
			    int port, const char *keytype)
{
    strbuf_catf(sb, "%s@%d:", keytype, port);
    escape_registry_key(hostname, sb);
}

int verify_host_key(const char *hostname, int port,
		    const char *keytype, const char *key)
{
    char *otherstr;
    strbuf *regname;
    int len;
    HKEY rkey;
    DWORD readlen;
    DWORD type;
    int ret, compare;

    len = 1 + strlen(key);

    /*
     * Now read a saved key in from the registry and see what it
     * says.
     */
    regname = strbuf_new();
    hostkey_regname(regname, hostname, port, keytype);

    if (RegOpenKey(HKEY_CURRENT_USER, PUTTY_REG_POS "\\SshHostKeys",
		   &rkey) != ERROR_SUCCESS) {
        strbuf_free(regname);
	return 1;		       /* key does not exist in registry */
    }

    readlen = len;
    otherstr = snewn(len, char);
    ret = RegQueryValueEx(rkey, regname->s, NULL,
                          &type, (BYTE *)otherstr, &readlen);

    if (ret != ERROR_SUCCESS && ret != ERROR_MORE_DATA &&
	!strcmp(keytype, "rsa")) {
	/*
	 * Key didn't exist. If the key type is RSA, we'll try
	 * another trick, which is to look up the _old_ key format
	 * under just the hostname and translate that.
	 */
	char *justhost = regname->s + 1 + strcspn(regname->s, ":");
	char *oldstyle = snewn(len + 10, char);	/* safety margin */
	readlen = len;
	ret = RegQueryValueEx(rkey, justhost, NULL, &type,
			      (BYTE *)oldstyle, &readlen);

	if (ret == ERROR_SUCCESS && type == REG_SZ) {
	    /*
	     * The old format is two old-style bignums separated by
	     * a slash. An old-style bignum is made of groups of
	     * four hex digits: digits are ordered in sensible
	     * (most to least significant) order within each group,
	     * but groups are ordered in silly (least to most)
	     * order within the bignum. The new format is two
	     * ordinary C-format hex numbers (0xABCDEFG...XYZ, with
	     * A nonzero except in the special case 0x0, which
	     * doesn't appear anyway in RSA keys) separated by a
	     * comma. All hex digits are lowercase in both formats.
	     */
	    char *p = otherstr;
	    char *q = oldstyle;
	    int i, j;

	    for (i = 0; i < 2; i++) {
		int ndigits, nwords;
		*p++ = '0';
		*p++ = 'x';
		ndigits = strcspn(q, "/");	/* find / or end of string */
		nwords = ndigits / 4;
		/* now trim ndigits to remove leading zeros */
		while (q[(ndigits - 1) ^ 3] == '0' && ndigits > 1)
		    ndigits--;
		/* now move digits over to new string */
		for (j = 0; j < ndigits; j++)
		    p[ndigits - 1 - j] = q[j ^ 3];
		p += ndigits;
		q += nwords * 4;
		if (*q) {
		    q++;	       /* eat the slash */
		    *p++ = ',';	       /* add a comma */
		}
		*p = '\0';	       /* terminate the string */
	    }

	    /*
	     * Now _if_ this key matches, we'll enter it in the new
	     * format. If not, we'll assume something odd went
	     * wrong, and hyper-cautiously do nothing.
	     */
	    if (!strcmp(otherstr, key))
		RegSetValueEx(rkey, regname->s, 0, REG_SZ, (BYTE *)otherstr,
			      strlen(otherstr) + 1);
	}

        sfree(oldstyle);
    }

    RegCloseKey(rkey);

    compare = strcmp(otherstr, key);

    sfree(otherstr);
    strbuf_free(regname);

    if (ret == ERROR_MORE_DATA ||
	(ret == ERROR_SUCCESS && type == REG_SZ && compare))
	return 2;		       /* key is different in registry */
    else if (ret != ERROR_SUCCESS || type != REG_SZ)
	return 1;		       /* key does not exist in registry */
    else
	return 0;		       /* key matched OK in registry */
}

bool have_ssh_host_key(const char *hostname, int port,
		      const char *keytype)
{
    /*
     * If we have a host key, verify_host_key will return 0 or 2.
     * If we don't have one, it'll return 1.
     */
    return verify_host_key(hostname, port, keytype, "") != 1;
}

void store_host_key(const char *hostname, int port,
		    const char *keytype, const char *key)
{
    strbuf *regname;
    HKEY rkey;

    regname = strbuf_new();
    hostkey_regname(regname, hostname, port, keytype);

    if (RegCreateKey(HKEY_CURRENT_USER, PUTTY_REG_POS "\\SshHostKeys",
		     &rkey) == ERROR_SUCCESS) {
	RegSetValueEx(rkey, regname->s, 0, REG_SZ,
                      (BYTE *)key, strlen(key) + 1);
	RegCloseKey(rkey);
    } /* else key does not exist in registry */

    strbuf_free(regname);
}

/*
 * Open (or delete) the random seed file.
 */
enum { DEL, OPEN_R, OPEN_W };
static bool try_random_seed(char const *path, int action, HANDLE *ret)
{
    if (action == DEL) {
        if (!DeleteFile(path) && GetLastError() != ERROR_FILE_NOT_FOUND) {
            nonfatal("Unable to delete '%s': %s", path,
                     win_strerror(GetLastError()));
        }
	*ret = INVALID_HANDLE_VALUE;
	return false;		       /* so we'll do the next ones too */
    }

    *ret = CreateFile(path,
		      action == OPEN_W ? GENERIC_WRITE : GENERIC_READ,
		      action == OPEN_W ? 0 : (FILE_SHARE_READ |
					      FILE_SHARE_WRITE),
		      NULL,
		      action == OPEN_W ? CREATE_ALWAYS : OPEN_EXISTING,
		      action == OPEN_W ? FILE_ATTRIBUTE_NORMAL : 0,
		      NULL);

    return (*ret != INVALID_HANDLE_VALUE);
}

static HANDLE access_random_seed(int action)
{
    HKEY rkey;
    DWORD type, size;
    HANDLE rethandle;
    char seedpath[2 * MAX_PATH + 10] = "\0";

    /*
     * Iterate over a selection of possible random seed paths until
     * we find one that works.
     * 
     * We do this iteration separately for reading and writing,
     * meaning that we will automatically migrate random seed files
     * if a better location becomes available (by reading from the
     * best location in which we actually find one, and then
     * writing to the best location in which we can _create_ one).
     */

    /*
     * First, try the location specified by the user in the
     * Registry, if any.
     */
    size = sizeof(seedpath);
    if (RegOpenKey(HKEY_CURRENT_USER, PUTTY_REG_POS, &rkey) ==
	ERROR_SUCCESS) {
	int ret = RegQueryValueEx(rkey, "RandSeedFile",
				  0, &type, (BYTE *)seedpath, &size);
	if (ret != ERROR_SUCCESS || type != REG_SZ)
	    seedpath[0] = '\0';
	RegCloseKey(rkey);

	if (*seedpath && try_random_seed(seedpath, action, &rethandle))
	    return rethandle;
    }

    /*
     * Next, try the user's local Application Data directory,
     * followed by their non-local one. This is found using the
     * SHGetFolderPath function, which won't be present on all
     * versions of Windows.
     */
    if (!tried_shgetfolderpath) {
	/* This is likely only to bear fruit on systems with IE5+
	 * installed, or WinMe/2K+. There is some faffing with
	 * SHFOLDER.DLL we could do to try to find an equivalent
	 * on older versions of Windows if we cared enough.
	 * However, the invocation below requires IE5+ anyway,
	 * so stuff that. */
	shell32_module = load_system32_dll("shell32.dll");
	GET_WINDOWS_FUNCTION(shell32_module, SHGetFolderPathA);
	tried_shgetfolderpath = true;
    }
    if (p_SHGetFolderPathA) {
	if (SUCCEEDED(p_SHGetFolderPathA(NULL, CSIDL_LOCAL_APPDATA,
					 NULL, SHGFP_TYPE_CURRENT, seedpath))) {
	    strcat(seedpath, "\\PUTTY.RND");
	    if (try_random_seed(seedpath, action, &rethandle))
		return rethandle;
	}

	if (SUCCEEDED(p_SHGetFolderPathA(NULL, CSIDL_APPDATA,
					 NULL, SHGFP_TYPE_CURRENT, seedpath))) {
	    strcat(seedpath, "\\PUTTY.RND");
	    if (try_random_seed(seedpath, action, &rethandle))
		return rethandle;
	}
    }

    /*
     * Failing that, try %HOMEDRIVE%%HOMEPATH% as a guess at the
     * user's home directory.
     */
    {
	int len, ret;

	len =
	    GetEnvironmentVariable("HOMEDRIVE", seedpath,
				   sizeof(seedpath));
	ret =
	    GetEnvironmentVariable("HOMEPATH", seedpath + len,
				   sizeof(seedpath) - len);
	if (ret != 0) {
	    strcat(seedpath, "\\PUTTY.RND");
	    if (try_random_seed(seedpath, action, &rethandle))
		return rethandle;
	}
    }

    /*
     * And finally, fall back to C:\WINDOWS.
     */
    GetWindowsDirectory(seedpath, sizeof(seedpath));
    strcat(seedpath, "\\PUTTY.RND");
    if (try_random_seed(seedpath, action, &rethandle))
	return rethandle;

    /*
     * If even that failed, give up.
     */
    return INVALID_HANDLE_VALUE;
}

void read_random_seed(noise_consumer_t consumer)
{
    HANDLE seedf = access_random_seed(OPEN_R);

    if (seedf != INVALID_HANDLE_VALUE) {
	while (1) {
	    char buf[1024];
	    DWORD len;

	    if (ReadFile(seedf, buf, sizeof(buf), &len, NULL) && len)
		consumer(buf, len);
	    else
		break;
	}
	CloseHandle(seedf);
    }
}

void write_random_seed(void *data, int len)
{
    HANDLE seedf = access_random_seed(OPEN_W);

    if (seedf != INVALID_HANDLE_VALUE) {
	DWORD lenwritten;

	WriteFile(seedf, data, len, &lenwritten, NULL);
	CloseHandle(seedf);
    }
}

/*
 * Internal function supporting the jump list registry code. All the
 * functions to add, remove and read the list have substantially
 * similar content, so this is a generalisation of all of them which
 * transforms the list in the registry by prepending 'add' (if
 * non-null), removing 'rem' from what's left (if non-null), and
 * returning the resulting concatenated list of strings in 'out' (if
 * non-null).
 */
static int transform_jumplist_registry
    (const char *add, const char *rem, char **out)
{
    int ret;
    HKEY pjumplist_key;
    DWORD type;
    DWORD value_length;
    char *old_value, *new_value;
    char *piterator_old, *piterator_new, *piterator_tmp;

    ret = RegCreateKeyEx(HKEY_CURRENT_USER, reg_jumplist_key, 0, NULL,
                         REG_OPTION_NON_VOLATILE, (KEY_READ | KEY_WRITE), NULL,
                         &pjumplist_key, NULL);
    if (ret != ERROR_SUCCESS) {
	return JUMPLISTREG_ERROR_KEYOPENCREATE_FAILURE;
    }

    /* Get current list of saved sessions in the registry. */
    value_length = 200;
    old_value = snewn(value_length, char);
    ret = RegQueryValueEx(pjumplist_key, reg_jumplist_value, NULL, &type,
                          (BYTE *)old_value, &value_length);
    /* When the passed buffer is too small, ERROR_MORE_DATA is
     * returned and the required size is returned in the length
     * argument. */
    if (ret == ERROR_MORE_DATA) {
        sfree(old_value);
        old_value = snewn(value_length, char);
        ret = RegQueryValueEx(pjumplist_key, reg_jumplist_value, NULL, &type,
                              (BYTE *)old_value, &value_length);
    }

    if (ret == ERROR_FILE_NOT_FOUND) {
        /* Value doesn't exist yet. Start from an empty value. */
        *old_value = '\0';
        *(old_value + 1) = '\0';
    } else if (ret != ERROR_SUCCESS) {
        /* Some non-recoverable error occurred. */
        sfree(old_value);
        RegCloseKey(pjumplist_key);
        return JUMPLISTREG_ERROR_VALUEREAD_FAILURE;
    } else if (type != REG_MULTI_SZ) {
        /* The value present in the registry has the wrong type: we
         * try to delete it and start from an empty value. */
        ret = RegDeleteValue(pjumplist_key, reg_jumplist_value);
        if (ret != ERROR_SUCCESS) {
            sfree(old_value);
            RegCloseKey(pjumplist_key);
            return JUMPLISTREG_ERROR_VALUEREAD_FAILURE;
        }

        *old_value = '\0';
        *(old_value + 1) = '\0';
    }

    /* Check validity of registry data: REG_MULTI_SZ value must end
     * with \0\0. */
    piterator_tmp = old_value;
    while (((piterator_tmp - old_value) < (value_length - 1)) &&
           !(*piterator_tmp == '\0' && *(piterator_tmp+1) == '\0')) {
        ++piterator_tmp;
    }

    if ((piterator_tmp - old_value) >= (value_length-1)) {
        /* Invalid value. Start from an empty value. */
        *old_value = '\0';
        *(old_value + 1) = '\0';
    }

    /*
     * Modify the list, if we're modifying.
     */
    if (add || rem) {
        /* Walk through the existing list and construct the new list of
         * saved sessions. */
        new_value = snewn(value_length + (add ? strlen(add) + 1 : 0), char);
        piterator_new = new_value;
        piterator_old = old_value;

        /* First add the new item to the beginning of the list. */
        if (add) {
            strcpy(piterator_new, add);
            piterator_new += strlen(piterator_new) + 1;
        }
        /* Now add the existing list, taking care to leave out the removed
         * item, if it was already in the existing list. */
        while (*piterator_old != '\0') {
            if (!rem || strcmp(piterator_old, rem) != 0) {
                /* Check if this is a valid session, otherwise don't add. */
                settings_r *psettings_tmp = open_settings_r(piterator_old);
                if (psettings_tmp != NULL) {
                    close_settings_r(psettings_tmp);
                    strcpy(piterator_new, piterator_old);
                    piterator_new += strlen(piterator_new) + 1;
                }
            }
            piterator_old += strlen(piterator_old) + 1;
        }
        *piterator_new = '\0';
        ++piterator_new;

        /* Save the new list to the registry. */
        ret = RegSetValueEx(pjumplist_key, reg_jumplist_value, 0, REG_MULTI_SZ,
                            (BYTE *)new_value, piterator_new - new_value);

        sfree(old_value);
        old_value = new_value;
    } else
        ret = ERROR_SUCCESS;

    /*
     * Either return or free the result.
     */
    if (out && ret == ERROR_SUCCESS)
        *out = old_value;
    else
        sfree(old_value);

    /* Clean up and return. */
    RegCloseKey(pjumplist_key);

    if (ret != ERROR_SUCCESS) {
        return JUMPLISTREG_ERROR_VALUEWRITE_FAILURE;
    } else {
        return JUMPLISTREG_OK;
    }
}

/* Adds a new entry to the jumplist entries in the registry. */
int add_to_jumplist_registry(const char *item)
{
    return transform_jumplist_registry(item, item, NULL);
}

/* Removes an item from the jumplist entries in the registry. */
int remove_from_jumplist_registry(const char *item)
{
    return transform_jumplist_registry(NULL, item, NULL);
}

/* Returns the jumplist entries from the registry. Caller must free
 * the returned pointer. */
char *get_jumplist_registry_entries (void)
{
    char *list_value;

    if (transform_jumplist_registry(NULL,NULL,&list_value) != JUMPLISTREG_OK) {
	list_value = snewn(2, char);
        *list_value = '\0';
        *(list_value + 1) = '\0';
    }
    return list_value;
}

/*
 * Recursively delete a registry key and everything under it.
 */
static void registry_recursive_remove(HKEY key)
{
    DWORD i;
    char name[MAX_PATH + 1];
    HKEY subkey;

    i = 0;
    while (RegEnumKey(key, i, name, sizeof(name)) == ERROR_SUCCESS) {
	if (RegOpenKey(key, name, &subkey) == ERROR_SUCCESS) {
	    registry_recursive_remove(subkey);
	    RegCloseKey(subkey);
	}
	RegDeleteKey(key, name);
    }
}

void cleanup_all(void)
{
    HKEY key;
    int ret;
    char name[MAX_PATH + 1];

    /* ------------------------------------------------------------
     * Wipe out the random seed file, in all of its possible
     * locations.
     */
    access_random_seed(DEL);

    /* ------------------------------------------------------------
     * Ask Windows to delete any jump list information associated
     * with this installation of PuTTY.
     */
    clear_jumplist();

    /* ------------------------------------------------------------
     * Destroy all registry information associated with PuTTY.
     */

    /*
     * Open the main PuTTY registry key and remove everything in it.
     */
    if (RegOpenKey(HKEY_CURRENT_USER, PUTTY_REG_POS, &key) ==
	ERROR_SUCCESS) {
	registry_recursive_remove(key);
	RegCloseKey(key);
    }
    /*
     * Now open the parent key and remove the PuTTY main key. Once
     * we've done that, see if the parent key has any other
     * children.
     */
    if (RegOpenKey(HKEY_CURRENT_USER, PUTTY_REG_PARENT,
		   &key) == ERROR_SUCCESS) {
	RegDeleteKey(key, PUTTY_REG_PARENT_CHILD);
	ret = RegEnumKey(key, 0, name, sizeof(name));
	RegCloseKey(key);
	/*
	 * If the parent key had no other children, we must delete
	 * it in its turn. That means opening the _grandparent_
	 * key.
	 */
	if (ret != ERROR_SUCCESS) {
	    if (RegOpenKey(HKEY_CURRENT_USER, PUTTY_REG_GPARENT,
			   &key) == ERROR_SUCCESS) {
		RegDeleteKey(key, PUTTY_REG_GPARENT_CHILD);
		RegCloseKey(key);
	    }
	}
    }
    /*
     * Now we're done.
     */
}
