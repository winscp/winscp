/*
 * Platform-independent routines shared between all PuTTY programs.
 *
 * This file contains functions that use the kind of infrastructure
 * like conf.c that tends to only live in the main applications, or
 * that do things that only something like a main PuTTY application
 * would need. So standalone test programs should generally be able to
 * avoid linking against it.
 *
 * More standalone functions that depend on nothing but the C library
 * live in utils.c.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <limits.h>
#include <ctype.h>
#include <assert.h>

#include "defs.h"
#include "putty.h"
#include "misc.h"

#define BASE64_CHARS_NOEQ \
    "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+/"
#define BASE64_CHARS_ALL BASE64_CHARS_NOEQ "="

void seat_connection_fatal(Seat *seat, const char *fmt, ...)
{
    va_list ap;
    char *msg;

    va_start(ap, fmt);
    msg = dupvprintf(fmt, ap);
    va_end(ap);

    seat->vt->connection_fatal(seat, msg);
    sfree(msg);                        /* if we return */
}

prompts_t *new_prompts(void)
{
    prompts_t *p = snew(prompts_t);
    p->prompts = NULL;
    p->n_prompts = p->prompts_size = 0;
    p->data = NULL;
    p->to_server = true; /* to be on the safe side */
    p->name = p->instruction = NULL;
    p->name_reqd = p->instr_reqd = false;
    return p;
}
void add_prompt(prompts_t *p, char *promptstr, bool echo)
{
    prompt_t *pr = snew(prompt_t);
    pr->prompt = promptstr;
    pr->echo = echo;
    pr->result = strbuf_new_nm();
    sgrowarray(p->prompts, p->prompts_size, p->n_prompts);
    p->prompts[p->n_prompts++] = pr;
}
void prompt_set_result(prompt_t *pr, const char *newstr)
{
    strbuf_clear(pr->result);
    put_datapl(pr->result, ptrlen_from_asciz(newstr));
}
const char *prompt_get_result_ref(prompt_t *pr)
{
    return pr->result->s;
}
char *prompt_get_result(prompt_t *pr)
{
    return dupstr(pr->result->s);
}
void free_prompts(prompts_t *p)
{
    size_t i;
    for (i=0; i < p->n_prompts; i++) {
        prompt_t *pr = p->prompts[i];
        strbuf_free(pr->result);
        sfree(pr->prompt);
        sfree(pr);
    }
    sfree(p->prompts);
    sfree(p->name);
    sfree(p->instruction);
    sfree(p);
}

/*
 * Determine whether or not a Conf represents a session which can
 * sensibly be launched right now.
 */
bool conf_launchable(Conf *conf)
{
    if (conf_get_int(conf, CONF_protocol) == PROT_SERIAL)
        return conf_get_str(conf, CONF_serline)[0] != 0;
    else
        return conf_get_str(conf, CONF_host)[0] != 0;
}

char const *conf_dest(Conf *conf)
{
    if (conf_get_int(conf, CONF_protocol) == PROT_SERIAL)
        return conf_get_str(conf, CONF_serline);
    else
        return conf_get_str(conf, CONF_host);
}

/*
 * Validate a manual host key specification (either entered in the
 * GUI, or via -hostkey). If valid, we return true, and update 'key'
 * to contain a canonicalised version of the key string in 'key'
 * (which is guaranteed to take up at most as much space as the
 * original version), suitable for putting into the Conf. If not
 * valid, we return false.
 */
bool validate_manual_hostkey(char *key)
{
    char *p, *q, *r, *s;

    /*
     * Step through the string word by word, looking for a word that's
     * in one of the formats we like.
     */
    p = key;
    while ((p += strspn(p, " \t"))[0]) {
        q = p;
        p += strcspn(p, " \t");
        if (*p) *p++ = '\0';

        /*
         * Now q is our word.
         */

        if (strstartswith(q, "SHA256:")) {
            /* Test for a valid SHA256 key fingerprint. */
            r = q + 7;
            if (strlen(r) == 43 && r[strspn(r, BASE64_CHARS_NOEQ)] == 0)
                return true;
        }

        r = q;
        if (strstartswith(r, "MD5:"))
            r += 4;
        if (strlen(r) == 16*3 - 1 &&
            r[strspn(r, "0123456789abcdefABCDEF:")] == 0) {
            /*
             * Test for a valid MD5 key fingerprint. Check the colons
             * are in the right places, and if so, return the same
             * fingerprint canonicalised into lowercase.
             */
            int i;
            for (i = 0; i < 16; i++)
                if (r[3*i] == ':' || r[3*i+1] == ':')
                    goto not_fingerprint; /* sorry */
            for (i = 0; i < 15; i++)
                if (r[3*i+2] != ':')
                    goto not_fingerprint; /* sorry */
            for (i = 0; i < 16*3 - 1; i++)
                key[i] = tolower(r[i]);
            key[16*3 - 1] = '\0';
            return true;
        }
      not_fingerprint:;

        /*
         * Before we check for a public-key blob, trim newlines out of
         * the middle of the word, in case someone's managed to paste
         * in a public-key blob _with_ them.
         */
        for (r = s = q; *r; r++)
            if (*r != '\n' && *r != '\r')
                *s++ = *r;
        *s = '\0';

        if (strlen(q) % 4 == 0 && strlen(q) > 2*4 &&
            q[strspn(q, BASE64_CHARS_ALL)] == 0) {
            /*
             * Might be a base64-encoded SSH-2 public key blob. Check
             * that it starts with a sensible algorithm string. No
             * canonicalisation is necessary for this string type.
             *
             * The algorithm string must be at most 64 characters long
             * (RFC 4251 section 6).
             */
            unsigned char decoded[6];
            unsigned alglen;
            int minlen;
            int len = 0;

            len += base64_decode_atom(q, decoded+len);
            if (len < 3)
                goto not_ssh2_blob;    /* sorry */
            len += base64_decode_atom(q+4, decoded+len);
            if (len < 4)
                goto not_ssh2_blob;    /* sorry */

            alglen = GET_32BIT_MSB_FIRST(decoded);
            if (alglen > 64)
                goto not_ssh2_blob;    /* sorry */

            minlen = ((alglen + 4) + 2) / 3;
            if (strlen(q) < minlen)
                goto not_ssh2_blob;    /* sorry */

            strcpy(key, q);
            return true;
        }
      not_ssh2_blob:;
    }

    return false;
}

char *buildinfo(const char *newline)
{
    strbuf *buf = strbuf_new();

    strbuf_catf(buf, "Build platform: %d-bit %s",
                (int)(CHAR_BIT * sizeof(void *)),
                BUILDINFO_PLATFORM);

#ifdef __clang_version__
#define FOUND_COMPILER
    strbuf_catf(buf, "%sCompiler: clang %s", newline, __clang_version__);
#elif defined __GNUC__ && defined __VERSION__
#define FOUND_COMPILER
    strbuf_catf(buf, "%sCompiler: gcc %s", newline, __VERSION__);
#endif

#if defined _MSC_VER
#ifndef FOUND_COMPILER
#define FOUND_COMPILER
    strbuf_catf(buf, "%sCompiler: ", newline);
#else
    strbuf_catf(buf, ", emulating ");
#endif
    strbuf_catf(buf, "Visual Studio");

#if 0
    /*
     * List of _MSC_VER values and their translations taken from
     * https://docs.microsoft.com/en-us/cpp/preprocessor/predefined-macros
     *
     * The pointless #if 0 branch containing this comment is there so
     * that every real clause can start with #elif and there's no
     * anomalous first clause. That way the patch looks nicer when you
     * add extra ones.
     */
#elif _MSC_VER == 1928 && _MSC_FULL_VER >= 192829500
    /*
     * 16.9 and 16.8 have the same _MSC_VER value, and have to be
     * distinguished by _MSC_FULL_VER. As of 2021-03-04 that is not
     * mentioned on the above page, but see e.g.
     * https://developercommunity.visualstudio.com/t/the-169-cc-compiler-still-uses-the-same-version-nu/1335194#T-N1337120
     * which says that 16.9 builds will have versions starting at
     * 19.28.29500.* and going up. Hence, 19 28 29500 is what we
     * compare _MSC_FULL_VER against above.
     */
    strbuf_catf(buf, " 2019 (16.9)");
#elif _MSC_VER == 1928
    strbuf_catf(buf, " 2019 (16.8)");
#elif _MSC_VER == 1927
    strbuf_catf(buf, " 2019 (16.7)");
#elif _MSC_VER == 1926
    strbuf_catf(buf, " 2019 (16.6)");
#elif _MSC_VER == 1925
    strbuf_catf(buf, " 2019 (16.5)");
#elif _MSC_VER == 1924
    strbuf_catf(buf, " 2019 (16.4)");
#elif _MSC_VER == 1923
    strbuf_catf(buf, " 2019 (16.3)");
#elif _MSC_VER == 1922
    strbuf_catf(buf, " 2019 (16.2)");
#elif _MSC_VER == 1921
    strbuf_catf(buf, " 2019 (16.1)");
#elif _MSC_VER == 1920
    strbuf_catf(buf, " 2019 (16.0)");
#elif _MSC_VER == 1916
    strbuf_catf(buf, " 2017 version 15.9");
#elif _MSC_VER == 1915
    strbuf_catf(buf, " 2017 version 15.8");
#elif _MSC_VER == 1914
    strbuf_catf(buf, " 2017 version 15.7");
#elif _MSC_VER == 1913
    strbuf_catf(buf, " 2017 version 15.6");
#elif _MSC_VER == 1912
    strbuf_catf(buf, " 2017 version 15.5");
#elif _MSC_VER == 1911
    strbuf_catf(buf, " 2017 version 15.3");
#elif _MSC_VER == 1910
    strbuf_catf(buf, " 2017 RTW (15.0)");
#elif _MSC_VER == 1900
    strbuf_catf(buf, " 2015 (14.0)");
#elif _MSC_VER == 1800
    strbuf_catf(buf, " 2013 (12.0)");
#elif _MSC_VER == 1700
    strbuf_catf(buf, " 2012 (11.0)");
#elif _MSC_VER == 1600
    strbuf_catf(buf, " 2010 (10.0)");
#elif _MSC_VER == 1500
    strbuf_catf(buf, " 2008 (9.0)");
#elif _MSC_VER == 1400
    strbuf_catf(buf, " 2005 (8.0)");
#elif _MSC_VER == 1310
    strbuf_catf(buf, " .NET 2003 (7.1)");
#elif _MSC_VER == 1300
    strbuf_catf(buf, " .NET 2002 (7.0)");
#elif _MSC_VER == 1200
    strbuf_catf(buf, " 6.0");
#else
    strbuf_catf(buf, ", unrecognised version");
#endif
    strbuf_catf(buf, ", _MSC_VER=%d", (int)_MSC_VER);
#endif

#ifdef BUILDINFO_GTK
    {
        char *gtk_buildinfo = buildinfo_gtk_version();
        if (gtk_buildinfo) {
            strbuf_catf(buf, "%sCompiled against GTK version %s",
                        newline, gtk_buildinfo);
            sfree(gtk_buildinfo);
        }
    }
#endif
#if defined _WINDOWS
    {
        int echm = has_embedded_chm();
        if (echm >= 0)
            strbuf_catf(buf, "%sEmbedded HTML Help file: %s", newline,
                        echm ? "yes" : "no");
    }
#endif

#if defined _WINDOWS && defined MINEFIELD
    strbuf_catf(buf, "%sBuild option: MINEFIELD", newline);
#endif
#ifdef NO_SECURITY
    strbuf_catf(buf, "%sBuild option: NO_SECURITY", newline);
#endif
#ifdef NO_SECUREZEROMEMORY
    strbuf_catf(buf, "%sBuild option: NO_SECUREZEROMEMORY", newline);
#endif
#ifdef NO_IPV6
    strbuf_catf(buf, "%sBuild option: NO_IPV6", newline);
#endif
#ifdef NO_GSSAPI
    strbuf_catf(buf, "%sBuild option: NO_GSSAPI", newline);
#endif
#ifdef STATIC_GSSAPI
    strbuf_catf(buf, "%sBuild option: STATIC_GSSAPI", newline);
#endif
#ifdef UNPROTECT
    strbuf_catf(buf, "%sBuild option: UNPROTECT", newline);
#endif
#ifdef FUZZING
    strbuf_catf(buf, "%sBuild option: FUZZING", newline);
#endif
#ifdef DEBUG
    strbuf_catf(buf, "%sBuild option: DEBUG", newline);
#endif

    strbuf_catf(buf, "%sSource commit: %s", newline, commitid);

    return strbuf_to_str(buf);
}

size_t nullseat_output(
    Seat *seat, bool is_stderr, const void *data, size_t len) { return 0; }
bool nullseat_eof(Seat *seat) { return true; }
int nullseat_get_userpass_input(
    Seat *seat, prompts_t *p, bufchain *input) { return 0; }
void nullseat_notify_remote_exit(Seat *seat) {}
void nullseat_connection_fatal(Seat *seat, const char *message) {}
void nullseat_update_specials_menu(Seat *seat) {}
char *nullseat_get_ttymode(Seat *seat, const char *mode) { return NULL; }
void nullseat_set_busy_status(Seat *seat, BusyStatus status) {}
int nullseat_verify_ssh_host_key(
    Seat *seat, const char *host, int port, const char *keytype,
    char *keystr, const char *keydisp, char **key_fingerprints,
    void (*callback)(void *ctx, int result), void *ctx) { return 0; }
int nullseat_confirm_weak_crypto_primitive(
    Seat *seat, const char *algtype, const char *algname,
    void (*callback)(void *ctx, int result), void *ctx) { return 0; }
int nullseat_confirm_weak_cached_hostkey(
    Seat *seat, const char *algname, const char *betteralgs,
    void (*callback)(void *ctx, int result), void *ctx) { return 0; }
bool nullseat_is_never_utf8(Seat *seat) { return false; }
bool nullseat_is_always_utf8(Seat *seat) { return true; }
void nullseat_echoedit_update(Seat *seat, bool echoing, bool editing) {}
const char *nullseat_get_x_display(Seat *seat) { return NULL; }
bool nullseat_get_windowid(Seat *seat, long *id_out) { return false; }
bool nullseat_get_window_pixel_size(
    Seat *seat, int *width, int *height) { return false; }
StripCtrlChars *nullseat_stripctrl_new(
    Seat *seat, BinarySink *bs_out, SeatInteractionContext sic) {return NULL;}
bool nullseat_set_trust_status(Seat *seat, bool tr) { return false; }
bool nullseat_set_trust_status_vacuously(Seat *seat, bool tr) { return true; }
bool nullseat_verbose_no(Seat *seat) { return false; }
bool nullseat_verbose_yes(Seat *seat) { return true; }
bool nullseat_interactive_no(Seat *seat) { return false; }
bool nullseat_interactive_yes(Seat *seat) { return true; }
bool nullseat_get_cursor_position(Seat *seat, int *x, int *y) { return false; }

bool null_lp_verbose_no(LogPolicy *lp) { return false; }
bool null_lp_verbose_yes(LogPolicy *lp) { return true; }

void sk_free_peer_info(SocketPeerInfo *pi)
{
    if (pi) {
        sfree((char *)pi->addr_text);
        sfree((char *)pi->log_text);
        sfree(pi);
    }
}

void out_of_memory(void)
{
    modalfatalbox("Out of memory");
}
