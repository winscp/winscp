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
    pr->result = NULL;
    pr->resultsize = 0;
    sgrowarray(p->prompts, p->prompts_size, p->n_prompts);
    p->prompts[p->n_prompts++] = pr;
}
void prompt_ensure_result_size(prompt_t *pr, int newlen)
{
    if ((int)pr->resultsize < newlen) {
        char *newbuf;
        newlen = newlen * 5 / 4 + 512; /* avoid too many small allocs */

        /*
         * We don't use sresize / realloc here, because we will be
         * storing sensitive stuff like passwords in here, and we want
         * to make sure that the data doesn't get copied around in
         * memory without the old copy being destroyed.
         */
        newbuf = snewn(newlen, char);
        memcpy(newbuf, pr->result, pr->resultsize);
        smemclr(pr->result, pr->resultsize);
        sfree(pr->result);
        pr->result = newbuf;
        pr->resultsize = newlen;
    }
}
void prompt_set_result(prompt_t *pr, const char *newstr)
{
    prompt_ensure_result_size(pr, strlen(newstr) + 1);
    strcpy(pr->result, newstr);
}
void free_prompts(prompts_t *p)
{
    size_t i;
    for (i=0; i < p->n_prompts; i++) {
	prompt_t *pr = p->prompts[i];
	smemclr(pr->result, pr->resultsize); /* burn the evidence */
	sfree(pr->result);
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

        if (strlen(q) == 16*3 - 1 &&
            q[strspn(q, "0123456789abcdefABCDEF:")] == 0) {
            /*
             * Might be a key fingerprint. Check the colons are in the
             * right places, and if so, return the same fingerprint
             * canonicalised into lowercase.
             */
            int i;
            for (i = 0; i < 16; i++)
                if (q[3*i] == ':' || q[3*i+1] == ':')
                    goto not_fingerprint; /* sorry */
            for (i = 0; i < 15; i++)
                if (q[3*i+2] != ':')
                    goto not_fingerprint; /* sorry */
            for (i = 0; i < 16*3 - 1; i++)
                key[i] = tolower(q[i]);
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
            q[strspn(q, "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                     "abcdefghijklmnopqrstuvwxyz+/=")] == 0) {
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
    strbuf_catf(buf, "Visual Studio", newline);
#if _MSC_VER == 1900
    strbuf_catf(buf, " 2015 / MSVC++ 14.0");
#elif _MSC_VER == 1912
    strbuf_catf(buf, " 2017 / MSVC++ 14.12");
#elif _MSC_VER == 1800
    strbuf_catf(buf, " 2013 / MSVC++ 12.0");
#elif _MSC_VER == 1700
    strbuf_catf(buf, " 2012 / MSVC++ 11.0");
#elif _MSC_VER == 1600
    strbuf_catf(buf, " 2010 / MSVC++ 10.0");
#elif _MSC_VER == 1500
    strbuf_catf(buf, " 2008 / MSVC++ 9.0");
#elif _MSC_VER == 1400
    strbuf_catf(buf, " 2005 / MSVC++ 8.0");
#elif _MSC_VER == 1310
    strbuf_catf(buf, " 2003 / MSVC++ 7.1");
#elif _MSC_VER == 1300
    strbuf_catf(buf, " 2003 / MSVC++ 7.0");
#else
    strbuf_catf(buf, ", unrecognised version");
#endif
    strbuf_catf(buf, " (_MSC_VER=%d)", (int)_MSC_VER);
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
    Seat *seat, const char *host, int port,
    const char *keytype, char *keystr, char *key_fingerprint,
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
