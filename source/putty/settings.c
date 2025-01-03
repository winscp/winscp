/*
 * settings.c: read and write saved sessions. (platform-independent)
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "putty.h"
#include "storage.h"
#ifndef NO_GSSAPI
#include "ssh/gssc.h"
#include "ssh/gss.h"
#endif


/* The cipher order given here is the default order. */
static const struct keyvalwhere ciphernames[] = {
    { "aes",        CIPHER_AES,             -1, -1 },
    { "chacha20",   CIPHER_CHACHA20,        CIPHER_AES, +1 },
    { "aesgcm",     CIPHER_AESGCM,          CIPHER_CHACHA20, +1 },
    { "3des",       CIPHER_3DES,            -1, -1 },
    { "WARN",       CIPHER_WARN,            -1, -1 },
    { "des",        CIPHER_DES,             -1, -1 },
    { "blowfish",   CIPHER_BLOWFISH,        -1, -1 },
    { "arcfour",    CIPHER_ARCFOUR,         -1, -1 },
};

/* The default order here is sometimes overridden by the backward-
 * compatibility warts in load_open_settings(), and should be kept
 * in sync with those. */
static const struct keyvalwhere kexnames[] = {
    { "ntru-curve25519",    KEX_NTRU_HYBRID, -1, +1 },
    { "mlkem-curve25519",   KEX_MLKEM_25519_HYBRID, KEX_NTRU_HYBRID, +1 },
    { "mlkem-nist",         KEX_MLKEM_NIST_HYBRID, KEX_MLKEM_25519_HYBRID, +1 },
    { "ecdh",               KEX_ECDH,       -1, +1 },
    /* This name is misleading: it covers both SHA-256 and SHA-1 variants */
    { "dh-gex-sha1",        KEX_DHGEX,      -1, -1 },
    /* Again, this covers both SHA-256 and SHA-1, despite the name: */
    { "dh-group14-sha1",    KEX_DHGROUP14,  -1, -1 },
    /* This one really is only SHA-1, though: */
    { "dh-group1-sha1",     KEX_DHGROUP1,   KEX_WARN, +1 },
    { "rsa",                KEX_RSA,        KEX_WARN, -1 },
    /* Larger fixed DH groups: prefer the larger 15 and 16 over 14,
     * but by default the even larger 17 and 18 go below 16.
     * Rationale: diminishing returns of improving the DH strength are
     * outweighed by increased CPU cost. Group 18 is painful on a slow
     * machine. Users can override if they need to. */
    { "dh-group15-sha512",  KEX_DHGROUP15,  KEX_DHGROUP14, -1 },
    { "dh-group16-sha512",  KEX_DHGROUP16,  KEX_DHGROUP15, -1 },
    { "dh-group17-sha512",  KEX_DHGROUP17,  KEX_DHGROUP16, +1 },
    { "dh-group18-sha512",  KEX_DHGROUP18,  KEX_DHGROUP17, +1 },
    { "WARN",               KEX_WARN,       -1, -1 }
};

static const struct keyvalwhere hknames[] = {
    { "ed25519",    HK_ED25519,             -1, +1 },
    { "ed448",      HK_ED448,               -1, +1 },
    { "ecdsa",      HK_ECDSA,               -1, -1 },
    { "dsa",        HK_DSA,                 -1, -1 },
    { "rsa",        HK_RSA,                 -1, -1 },
    { "WARN",       HK_WARN,                -1, -1 },
};

/*
 * All the terminal modes that we know about for the "TerminalModes"
 * setting. (Also used by config.c for the drop-down list.)
 * This is currently precisely the same as the set in
 * ssh/ttymode-list.h, but could in principle differ if other backends
 * started to support tty modes (e.g., the pty backend).
 * The set of modes in this array is currently significant for
 * settings migration from old versions; if they change, review the
 * gppmap() invocation for "TerminalModes".
 */
const char *const ttymodes[] = {
    "INTR",     "QUIT",     "ERASE",    "KILL",     "EOF",
    "EOL",      "EOL2",     "START",    "STOP",     "SUSP",
    "DSUSP",    "REPRINT",  "WERASE",   "LNEXT",    "FLUSH",
    "SWTCH",    "STATUS",   "DISCARD",  "IGNPAR",   "PARMRK",
    "INPCK",    "ISTRIP",   "INLCR",    "IGNCR",    "ICRNL",
    "IUCLC",    "IXON",     "IXANY",    "IXOFF",    "IMAXBEL",
    "IUTF8",    "ISIG",     "ICANON",   "XCASE",    "ECHO",
    "ECHOE",    "ECHOK",    "ECHONL",   "NOFLSH",   "TOSTOP",
    "IEXTEN",   "ECHOCTL",  "ECHOKE",   "PENDIN",   "OPOST",
    "OLCUC",    "ONLCR",    "OCRNL",    "ONOCR",    "ONLRET",
    "CS7",      "CS8",      "PARENB",   "PARODD",   NULL
};

static int default_protocol, default_port;
void settings_set_default_protocol(int newval) { default_protocol = newval; }
void settings_set_default_port(int newval) { default_port = newval; }

/*
 * Convenience functions to access the backends[] array
 * (which is only present in tools that manage settings).
 */

const struct BackendVtable *backend_vt_from_name(const char *name)
{
    const struct BackendVtable *const *p;
    for (p = backends; *p != NULL; p++)
        if (!strcmp((*p)->id, name))
            return *p;
    return NULL;
}

const struct BackendVtable *backend_vt_from_proto(int proto)
{
    const struct BackendVtable *const *p;
    for (p = backends; *p != NULL; p++)
        if ((*p)->protocol == proto)
            return *p;
    return NULL;
}

char *get_remote_username(Conf *conf)
{
    /* We don't worry about whether the username is stored as UTF-8,
     * because SSH wants it as UTF-8 */
    char *username = conf_get_str_ambi(conf, CONF_username, NULL);
    if (*username) {
        return dupstr(username);
    } else if (conf_get_bool(conf, CONF_username_from_env)) {
        /* Use local username. */
        return get_username();     /* might still be NULL */
    } else {
        return NULL;
    }
}

static char *gpps_raw(settings_r *sesskey, const char *name, const char *def)
{
    char *ret = read_setting_s(sesskey, name);
    if (!ret)
        ret = platform_default_s(name);
    if (!ret)
        ret = def ? dupstr(def) : NULL;   /* permit NULL as final fallback */
    return ret;
}

static void gpps(settings_r *sesskey, const char *name, const char *def,
                 Conf *conf, int primary)
{
    char *val = gpps_raw(sesskey, name, def);
    conf_set_str(conf, primary, val);
    sfree(val);
}

/*
 * gppfont and gppfile cannot have local defaults, since the very
 * format of a Filename or FontSpec is platform-dependent. So the
 * platform-dependent functions MUST return some sort of value.
 */
static void gppfont(settings_r *sesskey, const char *name,
                    Conf *conf, int primary)
{
    FontSpec *result = read_setting_fontspec(sesskey, name);
    if (!result)
        result = platform_default_fontspec(name);
    conf_set_fontspec(conf, primary, result);
    fontspec_free(result);
}
static void gppfile(settings_r *sesskey, const char *name,
                    Conf *conf, int primary)
{
    Filename *result = read_setting_filename(sesskey, name);
    if (!result)
        result = platform_default_filename(name);
    conf_set_filename(conf, primary, result);
    filename_free(result);
}

static bool gppb_raw(settings_r *sesskey, const char *name, bool def)
{
    def = platform_default_b(name, def);
    return read_setting_i(sesskey, name, def) != 0;
}

static void gppb(settings_r *sesskey, const char *name, bool def,
                 Conf *conf, int primary)
{
    conf_set_bool(conf, primary, gppb_raw(sesskey, name, def));
}

static int gppi_raw(settings_r *sesskey, const char *name, int def)
{
    def = platform_default_i(name, def);
    return read_setting_i(sesskey, name, def);
}

static void gppi(settings_r *sesskey, const char *name, int def,
                 Conf *conf, int primary)
{
    conf_set_int(conf, primary, gppi_raw(sesskey, name, def));
}

/*
 * Read a set of name-value pairs in the format we occasionally use:
 *   NAME\tVALUE\0NAME\tVALUE\0\0 in memory
 *   NAME=VALUE,NAME=VALUE, in storage
 * If there's no "=VALUE" (e.g. just NAME,NAME,NAME) then those keys
 * are mapped to the empty string.
 */
static bool gppmap(settings_r *sesskey, const char *name,
                   Conf *conf, int primary)
{
    char *buf, *p, *q, *key, *val;

    /*
     * Start by clearing any existing subkeys of this key from conf.
     */
    while ((key = conf_get_str_nthstrkey(conf, primary, 0)) != NULL)
        conf_del_str_str(conf, primary, key);

    /*
     * Now read a serialised list from the settings and unmarshal it
     * into its components.
     */
    buf = gpps_raw(sesskey, name, NULL);
    if (!buf)
        return false;

    p = buf;
    while (*p) {
        q = buf;
        val = NULL;
        while (*p && *p != ',') {
            int c = *p++;
            if (c == '=')
                c = '\0';
            if (c == '\\')
                c = *p++;
            *q++ = c;
            if (!c)
                val = q;
        }
        if (*p == ',')
            p++;
        if (!val)
            val = q;
        *q = '\0';

        if (primary == CONF_portfwd && strchr(buf, 'D') != NULL) {
            /*
             * Backwards-compatibility hack: dynamic forwardings are
             * indexed in the data store as a third type letter in the
             * key, 'D' alongside 'L' and 'R' - but really, they
             * should be filed under 'L' with a special _value_,
             * because local and dynamic forwardings both involve
             * _listening_ on a local port, and are hence mutually
             * exclusive on the same port number. So here we translate
             * the legacy storage format into the sensible internal
             * form, by finding the D and turning it into a L.
             */
            char *newkey = dupstr(buf);
            *strchr(newkey, 'D') = 'L';
            conf_set_str_str(conf, primary, newkey, "D");
            sfree(newkey);
        } else {
            conf_set_str_str(conf, primary, buf, val);
        }
    }
    sfree(buf);

    return true;
}

/*
 * Write a set of name/value pairs in the above format, or just the
 * names if include_values is false.
 */
static void wmap(settings_w *sesskey, char const *outkey, Conf *conf,
                 int primary, bool include_values)
{
    char *key, *realkey;
    const char *val, *q;

    strbuf *sb = strbuf_new();

    for (val = conf_get_str_strs(conf, primary, NULL, &key);
         val != NULL;
         val = conf_get_str_strs(conf, primary, key, &key)) {

        if (primary == CONF_portfwd && !strcmp(val, "D")) {
            /*
             * Backwards-compatibility hack, as above: translate from
             * the sensible internal representation of dynamic
             * forwardings (key "L<port>", value "D") to the
             * conceptually incoherent legacy storage format (key
             * "D<port>", value empty).
             */
            char *L;

            realkey = key;             /* restore it at end of loop */
            val = "";
            key = dupstr(key);
            L = strchr(key, 'L');
            if (L) *L = 'D';
        } else {
            realkey = NULL;
        }

        if (sb->len)
            put_byte(sb, ',');
        for (q = key; *q; q++) {
            if (*q == '=' || *q == ',' || *q == '\\')
                put_byte(sb, '\\');
            put_byte(sb, *q);
        }
        if (include_values) {
            put_byte(sb, '=');
            for (q = val; *q; q++) {
                if (*q == '=' || *q == ',' || *q == '\\')
                    put_byte(sb, '\\');
                put_byte(sb, *q);
            }
        }

        if (realkey) {
            free(key);
            key = realkey;
        }
    }
    write_setting_s(sesskey, outkey, sb->s);
    strbuf_free(sb);
}

static int key2val(const struct keyvalwhere *mapping,
                   int nmaps, char *key)
{
    int i;
    for (i = 0; i < nmaps; i++)
        if (!strcmp(mapping[i].s, key)) return mapping[i].v;
    return -1;
}

static const char *val2key(const struct keyvalwhere *mapping,
                           int nmaps, int val)
{
    int i;
    for (i = 0; i < nmaps; i++)
        if (mapping[i].v == val) return mapping[i].s;
    return NULL;
}

/*
 * Helper function to parse a comma-separated list of strings into
 * a preference list array of values. Any missing values are added
 * to the end and duplicates are weeded.
 * XXX: assumes vals in 'mapping' are small +ve integers
 */
static void gprefs_from_str(const char *str,
                            const struct keyvalwhere *mapping, int nvals,
                            Conf *conf, int primary)
{
    char *commalist = dupstr(str);
    char *p, *q;
    int i, j, n, v, pos;
    unsigned long seen = 0;            /* bitmap for weeding dups etc */

    /*
     * Go through that list and convert it into values.
     */
    n = 0;
    p = commalist;
    while (1) {
        while (*p && *p == ',') p++;
        if (!*p)
            break;                     /* no more words */

        q = p;
        while (*p && *p != ',') p++;
        if (*p) *p++ = '\0';

        v = key2val(mapping, nvals, q);
        if (v != -1 && !(seen & (1 << v))) {
            seen |= (1 << v);
            conf_set_int_int(conf, primary, n, v);
            n++;
        }
    }

    sfree(commalist);

    /*
     * Now go through 'mapping' and add values that weren't mentioned
     * in the list we fetched. We may have to loop over it multiple
     * times so that we add values before other values whose default
     * positions depend on them.
     */
    while (n < nvals) {
        for (i = 0; i < nvals; i++) {
            assert(mapping[i].v >= 0);
            assert(mapping[i].v < 32);

            if (!(seen & (1 << mapping[i].v))) {
                /*
                 * This element needs adding. But can we add it yet?
                 */
                if (mapping[i].vrel != -1 && !(seen & (1 << mapping[i].vrel)))
                    continue;          /* nope */

                /*
                 * OK, we can work out where to add this element, so
                 * do so.
                 */
                if (mapping[i].vrel == -1) {
                    pos = (mapping[i].where < 0 ? n : 0);
                } else {
                    for (j = 0; j < n; j++)
                        if (conf_get_int_int(conf, primary, j) ==
                            mapping[i].vrel)
                            break;
                    assert(j < n);     /* implied by (seen & (1<<vrel)) */
                    pos = (mapping[i].where < 0 ? j : j+1);
                }

                /*
                 * And add it.
                 */
                for (j = n-1; j >= pos; j--)
                    conf_set_int_int(conf, primary, j+1,
                                     conf_get_int_int(conf, primary, j));
                conf_set_int_int(conf, primary, pos, mapping[i].v);
                seen |= (1 << mapping[i].v);
                n++;
            }
        }
    }
}

/*
 * Read a preference list.
 */
static void gprefs(settings_r *sesskey, const char *name, const char *def,
                   const struct keyvalwhere *mapping, int nvals,
                   Conf *conf, int primary)
{
    /*
     * Fetch the string which we'll parse as a comma-separated list.
     */
    char *value = gpps_raw(sesskey, name, def);
    gprefs_from_str(value, mapping, nvals, conf, primary);
    sfree(value);
}

/*
 * Write out a preference list.
 */
static void wprefs(settings_w *sesskey, const char *name,
                   const struct keyvalwhere *mapping, int nvals,
                   Conf *conf, int primary)
{
    strbuf *sb = strbuf_new();

    int i; // WINSCP
    for (i = 0; i < nvals; i++) {
        const char *s = val2key(mapping, nvals,
                                conf_get_int_int(conf, primary, i));
        if (s)
            put_fmt(sb, "%s%s", (sb->len ? "," : ""), s);
    }

    write_setting_s(sesskey, name, sb->s);

    strbuf_free(sb);
}

static void write_setting_b(settings_w *handle, const char *key, bool value)
{
    write_setting_i(handle, key, value ? 1 : 0);
}

static void write_clip_setting(settings_w *sesskey, const char *savekey,
                               Conf *conf, int confkey, int strconfkey)
{
    int val = conf_get_int(conf, confkey);
    switch (val) {
      case CLIPUI_NONE:
      default:
        write_setting_s(sesskey, savekey, "none");
        break;
      case CLIPUI_IMPLICIT:
        write_setting_s(sesskey, savekey, "implicit");
        break;
      case CLIPUI_EXPLICIT:
        write_setting_s(sesskey, savekey, "explicit");
        break;
      case CLIPUI_CUSTOM: {
        char *sval = dupcat("custom:", conf_get_str(conf, strconfkey));
        write_setting_s(sesskey, savekey, sval);
        sfree(sval);
        break;
      }
    }
}

static void read_clip_setting(settings_r *sesskey, char *savekey,
                              int def, Conf *conf, int confkey, int strconfkey)
{
    char *setting = read_setting_s(sesskey, savekey);
    int val;

    conf_set_str(conf, strconfkey, "");
    if (!setting) {
        val = def;
    } else if (!strcmp(setting, "implicit")) {
        val = CLIPUI_IMPLICIT;
    } else if (!strcmp(setting, "explicit")) {
        val = CLIPUI_EXPLICIT;
    } else if (!strncmp(setting, "custom:", 7)) {
        val = CLIPUI_CUSTOM;
        conf_set_str(conf, strconfkey, setting + 7);
    } else {
        val = CLIPUI_NONE;
    }
    conf_set_int(conf, confkey, val);
    sfree(setting);
}

char *save_settings(const char *section, Conf *conf)
{
    struct settings_w *sesskey;
    char *errmsg;

    sesskey = open_settings_w(section, &errmsg);
    if (!sesskey)
        return errmsg;
    save_open_settings(sesskey, conf);
    close_settings_w(sesskey);
    return NULL;
}

/* Declare extern references to conf_enum_* types */
#define CONF_ENUM(name, ...) extern const ConfSaveEnumType conf_enum_##name;
#include "conf-enums.h"
#undef CONF_ENUM

void save_open_settings(settings_w *sesskey, Conf *conf)
{
    int i;
    const char *p;

    /* Save the settings simple enough to handle automatically */
    size_t key; // WINSCP
    for (key = 0; key < N_CONFIG_OPTIONS; key++) {
        const ConfKeyInfo *info = &conf_key_info[key];
        if (!info->save_custom && !info->not_saved) {
            /* Mappings are handled individually below */
            assert(info->subkey_type == CONF_TYPE_NONE);
            switch (info->value_type) {
              case CONF_TYPE_STR:
                write_setting_s(sesskey, info->save_keyword,
                                conf_get_str(conf, key));
                break;
              case CONF_TYPE_STR_AMBI: {
                bool orig_is_utf8;
                const char *orig = conf_get_str_ambi(conf, key, &orig_is_utf8);

                int cp_from, cp_to;
                if (orig_is_utf8) {
                    cp_from = CP_UTF8;
                    cp_to = DEFAULT_CODEPAGE;
                } else {
                    cp_from = DEFAULT_CODEPAGE;
                    cp_to = CP_UTF8;
                }

                { // WINSCP
                size_t wlen;
                wchar_t *wide = dup_mb_to_wc_c(
                    cp_from, orig, strlen(orig), &wlen);

                size_t clen;
                char *converted = dup_wc_to_mb_c(
                    cp_to, wide, wlen, "", &clen);

                const char *native, *utf8;
                if (orig_is_utf8) {
                    utf8 = orig;
                    native = converted;
                } else {
                    native = orig;
                    utf8 = converted;
                }
                write_setting_s(sesskey, info->save_keyword, native);
                (void)utf8; /* FIXME: also save the UTF-8 version */

                burnwcs(wide);
                burnstr(converted);
                } // WINSCP
                break;
              }
              case CONF_TYPE_INT: {
                int ival = conf_get_int(conf, key);
                if (info->storage_enum) {
                    bool success = conf_enum_map_to_storage(
                        info->storage_enum, ival, &ival);
                    assert(success && "unmapped integer value");
                }
                write_setting_i(sesskey, info->save_keyword, ival);
                break;
              }
              case CONF_TYPE_BOOL:
                write_setting_b(sesskey, info->save_keyword,
                                conf_get_bool(conf, key));
                break;
              case CONF_TYPE_FILENAME:
                write_setting_filename(sesskey, info->save_keyword,
                                       conf_get_filename(conf, key));
                break;
              case CONF_TYPE_FONT:
                write_setting_fontspec(sesskey, info->save_keyword,
                                       conf_get_fontspec(conf, key));
                break;
              default:
                unreachable("bad key type in save_open_settings");
            }
        }
    }

    write_setting_i(sesskey, "Present", 1);
    p = "raw";
    {
        const struct BackendVtable *vt =
            backend_vt_from_proto(conf_get_int(conf, CONF_protocol));
        if (vt)
            p = vt->id;
    }
    write_setting_s(sesskey, "Protocol", p);
    write_setting_i(sesskey, "PingInterval", conf_get_int(conf, CONF_ping_interval) / 60);      /* minutes */
    write_setting_i(sesskey, "PingIntervalSecs", conf_get_int(conf, CONF_ping_interval) % 60);  /* seconds */
    wmap(sesskey, "TerminalModes", conf, CONF_ttymodes, true);

    /* proxy settings */
    wmap(sesskey, "Environment", conf, CONF_environmt, true);
#ifndef NO_GSSAPI
    write_setting_b(sesskey, "GssapiFwd", conf_get_bool(conf, CONF_gssapifwd));
#endif
    wprefs(sesskey, "Cipher", ciphernames, CIPHER_MAX, conf, CONF_ssh_cipherlist);
    wprefs(sesskey, "KEX", kexnames, KEX_MAX, conf, CONF_ssh_kexlist);
    wprefs(sesskey, "HostKey", hknames, HK_MAX, conf, CONF_ssh_hklist);
#ifndef NO_GSSAPI
    write_setting_i(sesskey, "GssapiRekey", conf_get_int(conf, CONF_gssapirekey));
#endif
#ifndef NO_GSSAPI
    write_setting_b(sesskey, "AuthGSSAPI", conf_get_bool(conf, CONF_try_gssapi_auth));
    write_setting_b(sesskey, "AuthGSSAPIKEX", conf_get_bool(conf, CONF_try_gssapi_kex));
    wprefs(sesskey, "GSSLibs", gsslibkeywords, ngsslibs, conf, CONF_ssh_gsslist);
    write_setting_filename(sesskey, "GSSCustom", conf_get_filename(conf, CONF_ssh_gss_custom));
#endif
#ifdef OSX_META_KEY_CONFIG
    write_setting_b(sesskey, "OSXOptionMeta", conf_get_bool(conf, CONF_osx_option_meta));
    write_setting_b(sesskey, "OSXCommandMeta", conf_get_bool(conf, CONF_osx_command_meta));
#endif
    write_setting_i(sesskey, "BellOverloadT", conf_get_int(conf, CONF_bellovl_t)
#ifdef PUTTY_UNIX_PLATFORM_H
                    * 1000
#endif
                    );
    write_setting_i(sesskey, "BellOverloadS", conf_get_int(conf, CONF_bellovl_s)
#ifdef PUTTY_UNIX_PLATFORM_H
                    * 1000
#endif
                    );

    for (i = 0; i < 22; i++) {
        char buf[20], buf2[30];
        sprintf(buf, "Colour%d", i);
        sprintf(buf2, "%d,%d,%d",
                conf_get_int_int(conf, CONF_colours, i*3+0),
                conf_get_int_int(conf, CONF_colours, i*3+1),
                conf_get_int_int(conf, CONF_colours, i*3+2));
        write_setting_s(sesskey, buf, buf2);
    }
    for (i = 0; i < 256; i += 32) {
        char buf[20], buf2[256];
        int j;
        sprintf(buf, "Wordness%d", i);
        *buf2 = '\0';
        for (j = i; j < i + 32; j++) {
            sprintf(buf2 + strlen(buf2), "%s%d",
                    (*buf2 ? "," : ""),
                    conf_get_int_int(conf, CONF_wordness, j));
        }
        write_setting_s(sesskey, buf, buf2);
    }
    write_clip_setting(sesskey, "MousePaste", conf,
                       CONF_mousepaste, CONF_mousepaste_custom);
    write_clip_setting(sesskey, "CtrlShiftIns", conf,
                       CONF_ctrlshiftins, CONF_ctrlshiftins_custom);
    write_clip_setting(sesskey, "CtrlShiftCV", conf,
                       CONF_ctrlshiftcv, CONF_ctrlshiftcv_custom);
    wmap(sesskey, "PortForwardings", conf, CONF_portfwd, true);
    wmap(sesskey, "SSHManualHostKeys", conf, CONF_ssh_manual_hostkeys, false);
}

bool load_settings(const char *section, Conf *conf)
{
    settings_r *sesskey;

    sesskey = open_settings_r(section);
    { // WINSCP
    bool exists = (sesskey != NULL);
    load_open_settings(sesskey, conf);
    close_settings_r(sesskey);

    if (exists && conf_launchable(conf))
        add_session_to_jumplist(section);

    return exists;
    } // WINSCP
}

void load_open_settings(settings_r *sesskey, Conf *conf)
{
    int i;
    char *prot;

    // WINSCP BEGIN Testing that our conf.winscp.h projection is valid
    assert(conf_key_info[CONF_host].subkey_type == CONF_TYPE_NONE);
    assert(conf_key_info[CONF_host].value_type == CONF_TYPE_STR);
    assert(strcmp(conf_key_info[CONF_host].default_value.sval, "") == 0);
    assert(!conf_key_info[CONF_host].save_custom);
    assert(!conf_key_info[CONF_host].load_custom);
    assert(!conf_key_info[CONF_host].not_saved);
    assert(strcmp(conf_key_info[CONF_host].save_keyword, "HostName") == 0);
    assert(conf_key_info[CONF_host].storage_enum == NULL);

    assert(conf_key_info[CONF_close_on_exit].default_value.ival == AUTO);

    assert(conf_key_info[CONF_proxy_type].storage_enum == &conf_enum_proxy_type);

    assert(conf_key_info[CONF_remote_cmd2].not_saved);

    assert(strcmp(conf_key_info[CONF_proxy_host].default_value.sval, "proxy") == 0);

    assert(conf_key_info[CONF_ssh_kexlist].subkey_type == CONF_TYPE_INT);
    assert(conf_key_info[CONF_ssh_kexlist].save_custom);
    assert(conf_key_info[CONF_ssh_kexlist].load_custom);
    assert(conf_key_info[CONF_ssh_kexlist].save_keyword == NULL);

    assert(conf_key_info[CONF_ssh_prefer_known_hostkeys].default_value.bval == true);
    // WINSCP END

    { // WINSCP
    /* Load the settings simple enough to handle automatically */
    size_t key; // WINSCP
    for (key = 0; key < N_CONFIG_OPTIONS; key++) {
        const ConfKeyInfo *info = &conf_key_info[key];
        if (info->not_saved) {
            /* Mappings are assumed to default to empty */
            if (info->subkey_type == CONF_TYPE_NONE) {
                switch (info->value_type) {
                  case CONF_TYPE_STR:
                  case CONF_TYPE_STR_AMBI:
                    conf_set_str(conf, key, info->default_value.sval);
                    break;
                  case CONF_TYPE_INT:
                    conf_set_int(conf, key, info->default_value.ival);
                    break;
                  case CONF_TYPE_BOOL:
                    conf_set_bool(conf, key, info->default_value.bval);
                    break;
                  default:
                    unreachable("bad key type in load_open_settings");
                }
            }
        } else if (!info->load_custom) {
            /* Mappings are handled individually below */
            assert(info->subkey_type == CONF_TYPE_NONE);
            switch (info->value_type) {
              case CONF_TYPE_STR:
              case CONF_TYPE_STR_AMBI:
                gpps(sesskey, info->save_keyword, info->default_value.sval,
                     conf, key);
                break;
              case CONF_TYPE_INT:
                if (!info->storage_enum) {
                    gppi(sesskey, info->save_keyword,
                         info->default_value.ival, conf, key);
                } else {
                    /*
                     * Because our internal defaults are stored as the
                     * value we want in Conf, but our API for
                     * retrieving integers from storage requires a
                     * default value to fill in if no record is found,
                     * we must first figure out the default _storage_
                     * value, ugh.
                     */
                    int defstorage;
                    bool success = conf_enum_map_to_storage(
                        info->storage_enum, info->default_value.ival,
                        &defstorage);
                    assert(success && "unmapped default");

                    /* Now retrieve the stored value */
                    { // WINSCP
                    int storageval = gppi_raw(sesskey, info->save_keyword,
                                              defstorage);

                    /* And translate it back to Conf representation,
                     * replacing it with our Conf-rep default on failure */
                    int confval;
                    if (!conf_enum_map_from_storage(
                            info->storage_enum, storageval, &confval))
                        confval = info->default_value.ival;
                    conf_set_int(conf, key, confval);
                    } // WINSCP
                }
                break;
              case CONF_TYPE_BOOL:
                gppb(sesskey, info->save_keyword, info->default_value.bval,
                     conf, key);
                break;
              case CONF_TYPE_FILENAME:
                gppfile(sesskey, info->save_keyword, conf, key);
                break;
              case CONF_TYPE_FONT:
                gppfont(sesskey, info->save_keyword, conf, key);
                break;
              default:
                unreachable("bad key type in load_open_settings");
            }
        }
    }

    prot = gpps_raw(sesskey, "Protocol", "default");
    conf_set_int(conf, CONF_protocol, default_protocol);
    conf_set_int(conf, CONF_port, default_port);
    {
        const struct BackendVtable *vt = backend_vt_from_name(prot);
        if (vt) {
            conf_set_int(conf, CONF_protocol, vt->protocol);
            gppi(sesskey, "PortNumber", default_port, conf, CONF_port);
        }
    }
    sfree(prot);

    {
        /* This is two values for backward compatibility with 0.50/0.51 */
        int pingmin, pingsec;
        pingmin = gppi_raw(sesskey, "PingInterval", 0);
        pingsec = gppi_raw(sesskey, "PingIntervalSecs", 0);
        conf_set_int(conf, CONF_ping_interval, pingmin * 60 + pingsec);
    }
    if (gppmap(sesskey, "TerminalModes", conf, CONF_ttymodes)) {
        /*
         * Backwards compatibility with old saved settings.
         *
         * From the invention of this setting through 0.67, the set of
         * terminal modes was fixed, and absence of a mode from this
         * setting meant the user had explicitly removed it from the
         * UI and we shouldn't send it.
         *
         * In 0.68, the IUTF8 mode was added, and in handling old
         * settings we inadvertently removed the ability to not send
         * a mode. Any mode not mentioned was treated as if it was
         * set to 'auto' (A).
         *
         * After 0.68, we added explicit notation to the setting format
         * when the user removes a known terminal mode from the list.
         *
         * So: if any of the modes from the original set is missing, we
         * assume this was an intentional removal by the user and add
         * an explicit removal ('N'); but if IUTF8 (or any other mode
         * added after 0.67) is missing, we assume that its absence is
         * due to the setting being old rather than intentional, and
         * add it with its default setting.
         *
         * (This does mean that if a 0.68 user explicitly removed IUTF8,
         * we add it back; but removing IUTF8 had no effect in 0.68, so
         * we're preserving behaviour, which is the best we can do.)
         */
        for (i = 0; ttymodes[i]; i++) {
            if (!conf_get_str_str_opt(conf, CONF_ttymodes, ttymodes[i])) {
                /* Mode not mentioned in setting. */
                const char *def;
                if (!strcmp(ttymodes[i], "IUTF8")) {
                    /* Any new modes we add in future should be treated
                     * this way too. */
                    def = "A";  /* same as new-setting default below */
                } else {
                    /* One of the original modes. Absence is probably
                     * deliberate. */
                    def = "N";  /* don't send */
                }
                conf_set_str_str(conf, CONF_ttymodes, ttymodes[i], def);
            }
        }
    } else {
        /* This hardcodes a big set of defaults in any new saved
         * sessions. Let's hope we don't change our mind. */
        for (i = 0; ttymodes[i]; i++)
            conf_set_str_str(conf, CONF_ttymodes, ttymodes[i], "A");
    }

    /* proxy settings */
    {
        int storageval = gppi_raw(sesskey, "ProxyMethod", -1);
        int confval;
        if (!conf_enum_map_from_storage(&conf_enum_proxy_type,
                                        storageval, &confval)) {
            /*
             * Fall back to older ProxyType and ProxySOCKSVersion format
             */
            storageval = gppi_raw(sesskey, "ProxyType", 0);
            if (conf_enum_map_from_storage(&conf_enum_old_proxy_type,
                                           storageval, &confval)) {
                if (confval == PROXY_SOCKS5 &&
                    gppi_raw(sesskey, "ProxySOCKSVersion", 5) == 4)
                    confval = PROXY_SOCKS4;
            }
        }
        conf_set_int(conf, CONF_proxy_type, confval);
    }

    gppmap(sesskey, "Environment", conf, CONF_environmt);
#ifndef NO_GSSAPI
    gppb(sesskey, "GssapiFwd", false, conf, CONF_gssapifwd);
#endif
    gprefs(sesskey, "Cipher", "\0",
           ciphernames, CIPHER_MAX, conf, CONF_ssh_cipherlist);
    {
        /* Backward-compatibility: before 0.58 (when the "KEX"
         * preference was first added), we had an option to
         * disable gex under the "bugs" panel after one report of
         * a server which offered it then choked, but we never got
         * a server version string or any other reports. */
        const char *default_kexes,
                   *normal_default = "ecdh,dh-gex-sha1,dh-group18-sha512,dh-group17-sha512,dh-group16-sha512,dh-group15-sha512,dh-group14-sha1,rsa,"
                       "WARN,dh-group1-sha1",
                   *bugdhgex2_default = "ecdh,dh-group18-sha512,dh-group17-sha512,dh-group16-sha512,dh-group15-sha512,dh-group14-sha1,rsa,"
                       "WARN,dh-group1-sha1,dh-gex-sha1";
        char *raw;
        i = 2 - gppi_raw(sesskey, "BugDHGEx2", 0);
        if (i == FORCE_ON)
            default_kexes = bugdhgex2_default;
        else
            default_kexes = normal_default;
        /* Migration: after 0.67 we decided we didn't like
         * dh-group1-sha1. If it looks like the user never changed
         * the defaults, quietly upgrade their settings to demote it.
         * (If they did, they're on their own.) */
        raw = gpps_raw(sesskey, "KEX", default_kexes);
        assert(raw != NULL);
        /* Lack of 'ecdh' tells us this was saved by 0.58-0.67
         * inclusive. If it was saved by a later version, we need
         * to leave it alone. */
        if (strcmp(raw, "dh-group14-sha1,dh-group1-sha1,rsa,"
                   "WARN,dh-gex-sha1") == 0) {
            /* Previously migrated from BugDHGEx2. */
            sfree(raw);
            raw = dupstr(bugdhgex2_default);
        } else if (strcmp(raw, "dh-gex-sha1,dh-group14-sha1,"
                          "dh-group1-sha1,rsa,WARN") == 0) {
            /* Untouched old default setting. */
            sfree(raw);
            raw = dupstr(normal_default);
        }
        /* (For the record: after 0.70, the default algorithm list
         * very briefly contained the string 'gss-sha1-krb5'; this was
         * never used in any committed version of code, but was left
         * over from a pre-commit version of GSS key exchange.
         * Mentioned here as it is remotely possible that it will turn
         * up in someone's saved settings in future.) */

        gprefs_from_str(raw, kexnames, KEX_MAX, conf, CONF_ssh_kexlist);
        sfree(raw);
    }
    gprefs(sesskey, "HostKey", "ed25519,ecdsa,rsa,dsa,WARN",
           hknames, HK_MAX, conf, CONF_ssh_hklist);
#ifndef NO_GSSAPI
    gppi(sesskey, "GssapiRekey", GSS_DEF_REKEY_MINS, conf, CONF_gssapirekey);
    gppb(sesskey, "AuthGSSAPI", true, conf, CONF_try_gssapi_auth);
    gppb(sesskey, "AuthGSSAPIKEX", true, conf, CONF_try_gssapi_kex);
    gprefs(sesskey, "GSSLibs", "\0",
           gsslibkeywords, ngsslibs, conf, CONF_ssh_gsslist);
    gppfile(sesskey, "GSSCustom", conf, CONF_ssh_gss_custom);
#endif
    {
        int storageval = gppi_raw(sesskey, "RemoteQTitleAction", -1);
        int confval;
        if (!conf_enum_map_from_storage(&conf_enum_remote_qtitle_action,
                                        storageval, &confval)) {
            /*
             * Fall back to older NoRemoteQTitle format
             */
            storageval = gppi_raw(sesskey, "NoRemoteQTitle", 1);
            /* We deliberately interpret the old setting of "no response" as
             * "empty string". This changes the behaviour, but hopefully for
             * the better; the user can always recover the old behaviour. */
            confval = storageval ? TITLE_EMPTY : TITLE_REAL;
        }
        conf_set_int(conf, CONF_remote_qtitle_action, confval);
    }
#ifdef OSX_META_KEY_CONFIG
    gppb(sesskey, "OSXOptionMeta", true, conf, CONF_osx_option_meta);
    gppb(sesskey, "OSXCommandMeta", false, conf, CONF_osx_command_meta);
#endif
    i = gppi_raw(sesskey, "BellOverloadT", 2*TICKSPERSEC
#ifdef PUTTY_UNIX_PLATFORM_H
                                   *1000
#endif
                                   );
    conf_set_int(conf, CONF_bellovl_t, i
#ifdef PUTTY_UNIX_PLATFORM_H
                 / 1000
#endif
                 );
    i = gppi_raw(sesskey, "BellOverloadS", 5*TICKSPERSEC
#ifdef PUTTY_UNIX_PLATFORM_H
                                   *1000
#endif
                                   );
    conf_set_int(conf, CONF_bellovl_s, i
#ifdef PUTTY_UNIX_PLATFORM_H
                 / 1000
#endif
                 );

    for (i = 0; i < 22; i++) {
        static const char *const defaults[] = {
            "187,187,187", "255,255,255", "0,0,0", "85,85,85", "0,0,0",
            "0,255,0", "0,0,0", "85,85,85", "187,0,0", "255,85,85",
            "0,187,0", "85,255,85", "187,187,0", "255,255,85", "0,0,187",
            "85,85,255", "187,0,187", "255,85,255", "0,187,187",
            "85,255,255", "187,187,187", "255,255,255"
        };
        char buf[20], *buf2;
        int c0, c1, c2;
        sprintf(buf, "Colour%d", i);
        buf2 = gpps_raw(sesskey, buf, defaults[i]);
        if (sscanf(buf2, "%d,%d,%d", &c0, &c1, &c2) == 3) {
            conf_set_int_int(conf, CONF_colours, i*3+0, c0);
            conf_set_int_int(conf, CONF_colours, i*3+1, c1);
            conf_set_int_int(conf, CONF_colours, i*3+2, c2);
        }
        sfree(buf2);
    }
    for (i = 0; i < 256; i += 32) {
        static const char *const defaults[] = {
            "0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0",
            "0,1,2,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1",
            "1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,2",
            "1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1",
            "1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1",
            "1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1",
            "2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2",
            "2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2"
        };
        char buf[20], *buf2, *p;
        int j;
        sprintf(buf, "Wordness%d", i);
        buf2 = gpps_raw(sesskey, buf, defaults[i / 32]);
        p = buf2;
        for (j = i; j < i + 32; j++) {
            char *q = p;
            while (*p && *p != ',')
                p++;
            if (*p == ',')
                *p++ = '\0';
            conf_set_int_int(conf, CONF_wordness, j, atoi(q));
        }
        sfree(buf2);
    }
    read_clip_setting(sesskey, "MousePaste", CLIPUI_DEFAULT_MOUSE,
                      conf, CONF_mousepaste, CONF_mousepaste_custom);
    read_clip_setting(sesskey, "CtrlShiftIns", CLIPUI_DEFAULT_INS,
                      conf, CONF_ctrlshiftins, CONF_ctrlshiftins_custom);
    read_clip_setting(sesskey, "CtrlShiftCV", CLIPUI_NONE,
                      conf, CONF_ctrlshiftcv, CONF_ctrlshiftcv_custom);
    /*
     * The empty default for LineCodePage will be converted later
     * into a plausible default for the locale.
     */

    gppmap(sesskey, "PortForwardings", conf, CONF_portfwd);
    {
        int i;
        i = gppi_raw(sesskey, "BugHMAC2", 0); conf_set_int(conf, CONF_sshbug_hmac2, 2-i);
        if (2-i == AUTO) {
            i = gppi_raw(sesskey, "BuggyMAC", 0);
            if (i == 1)
                conf_set_int(conf, CONF_sshbug_hmac2, FORCE_ON);
        }
    }
    gppmap(sesskey, "SSHManualHostKeys", conf, CONF_ssh_manual_hostkeys);
    } // WINSCP
}

bool do_defaults(const char *session, Conf *conf)
{
    return load_settings(session, conf);
}

static int sessioncmp(const void *av, const void *bv)
{
    const char *a = *(const char *const *) av;
    const char *b = *(const char *const *) bv;

    /*
     * Alphabetical order, except that "Default Settings" is a
     * special case and comes first.
     */
    if (!strcmp(a, "Default Settings"))
        return -1;                     /* a comes first */
    if (!strcmp(b, "Default Settings"))
        return +1;                     /* b comes first */
    /*
     * FIXME: perhaps we should ignore the first & in determining
     * sort order.
     */
    return strcmp(a, b);               /* otherwise, compare normally */
}

bool sesslist_demo_mode = false;

#ifndef WINSCP
void get_sesslist(struct sesslist *list, bool allocate)
{
    int i;
    char *p;
    settings_e *handle;

    if (allocate) {
        strbuf *sb = strbuf_new();

        if (sesslist_demo_mode) {
            put_asciz(sb, "demo-server");
            put_asciz(sb, "demo-server-2");
        } else {
            if ((handle = enum_settings_start()) != NULL) {
                while (enum_settings_next(handle, sb))
                    put_byte(sb, '\0');
                enum_settings_finish(handle);
            }
            put_byte(sb, '\0');
        }

        list->buffer = strbuf_to_str(sb);

        /*
         * Now set up the list of sessions. Note that "Default
         * Settings" must always be claimed to exist, even if it
         * doesn't really.
         */

        p = list->buffer;
        list->nsessions = 1;           /* "Default Settings" counts as one */
        while (*p) {
            if (strcmp(p, "Default Settings"))
                list->nsessions++;
            while (*p)
                p++;
            p++;
        }

        list->sessions = snewn(list->nsessions + 1, const char *);
        list->sessions[0] = "Default Settings";
        p = list->buffer;
        i = 1;
        while (*p) {
            if (strcmp(p, "Default Settings"))
                list->sessions[i++] = p;
            while (*p)
                p++;
            p++;
        }

        qsort(list->sessions, i, sizeof(const char *), sessioncmp);
    } else {
        sfree(list->buffer);
        sfree(list->sessions);
        list->buffer = NULL;
        list->sessions = NULL;
    }
}
#endif
