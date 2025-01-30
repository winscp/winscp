/*
 * Header for miscellaneous helper functions, mostly defined in the
 * utils subdirectory.
 */

#ifndef PUTTY_MISC_H
#define PUTTY_MISC_H

#include "defs.h"
#include "puttymem.h"
#include "marshal.h"

#include <stdio.h>                     /* for FILE * */
#include <stdarg.h>                    /* for va_list */
#include <stdlib.h>                    /* for abort */
#include <time.h>                      /* for struct tm */
#include <limits.h>                    /* for INT_MAX/MIN */
#include <assert.h>                    /* for assert (obviously) */

unsigned long parse_blocksize(const char *bs);
char ctrlparse(char *s, char **next);

size_t host_strcspn(const char *s, const char *set);
char *host_strchr(const char *s, int c);
char *host_strrchr(const char *s, int c);
char *host_strduptrim(const char *s);

char *dupstr(const char *s);
wchar_t *dupwcs(const wchar_t *s);
char *dupcat_fn(const char *s1, ...);
#define dupcat(...) dupcat_fn(__VA_ARGS__, (const char *)NULL)
wchar_t *dupwcscat_fn(const wchar_t *s1, ...);
#define dupwcscat(...) dupwcscat_fn(__VA_ARGS__, (const wchar_t *)NULL)
char *dupprintf(const char *fmt, ...) PRINTF_LIKE(1, 2);
char *dupvprintf(const char *fmt, va_list ap);
void burnstr(char *string);
void burnwcs(wchar_t *string);

/*
 * The visible part of a strbuf structure. There's a surrounding
 * implementation struct in strbuf.c, which isn't exposed to client
 * code.
 */
struct strbuf {
    char *s;
    unsigned char *u;
    size_t len;
    BinarySink_IMPLEMENTATION;
};

/* strbuf constructors: strbuf_new_nm and strbuf_new differ in that a
 * strbuf constructed using the _nm version will resize itself by
 * alloc/copy/smemclr/free instead of realloc. Use that version for
 * data sensitive enough that it's worth costing performance to
 * avoid copies of it lingering in process memory. */
strbuf *strbuf_new(void);
strbuf *strbuf_new_nm(void);

/* Helpers to allocate a strbuf containing an existing string */
strbuf *strbuf_dup(ptrlen string);
strbuf *strbuf_dup_nm(ptrlen string);

void strbuf_free(strbuf *buf);
void *strbuf_append(strbuf *buf, size_t len);
void strbuf_shrink_to(strbuf *buf, size_t new_len);
void strbuf_shrink_by(strbuf *buf, size_t amount_to_remove);
char *strbuf_to_str(strbuf *buf); /* does free buf, but you must free result */
static inline void strbuf_clear(strbuf *buf) { strbuf_shrink_to(buf, 0); }
bool strbuf_chomp(strbuf *buf, char char_to_remove);

strbuf *strbuf_new_for_agent_query(void);
void strbuf_finalise_agent_query(strbuf *buf);

/* String-to-Unicode converters that auto-allocate the destination and
 * work around the rather deficient interface of mb_to_wc. */
wchar_t *dup_mb_to_wc_c(int codepage, const char *string,
                        size_t len, size_t *outlen_p);
wchar_t *dup_mb_to_wc(int codepage, const char *string);
char *dup_wc_to_mb_c(int codepage, const wchar_t *string,
                     size_t len, const char *defchr, size_t *outlen_p);
char *dup_wc_to_mb(int codepage, const wchar_t *string,
                   const char *defchr);

static inline int toint(unsigned u)
{
    /*
     * Convert an unsigned to an int, without running into the
     * undefined behaviour which happens by the strict C standard if
     * the value overflows. You'd hope that sensible compilers would
     * do the sensible thing in response to a cast, but actually I
     * don't trust modern compilers not to do silly things like
     * assuming that _obviously_ you wouldn't have caused an overflow
     * and so they can elide an 'if (i < 0)' test immediately after
     * the cast.
     *
     * Sensible compilers ought of course to optimise this entire
     * function into 'just return the input value', and since it's
     * also declared inline, elide it completely in their output.
     */
    if (u <= (unsigned)INT_MAX)
        return (int)u;
    else if (u >= (unsigned)INT_MIN)   /* wrap in cast _to_ unsigned is OK */
        return INT_MIN + (int)(u - (unsigned)INT_MIN);
    else
        return INT_MIN; /* fallback; should never occur on binary machines */
}

char *fgetline(FILE *fp);
bool read_file_into(BinarySink *bs, FILE *fp);
char *chomp(char *str);
bool strstartswith(const char *s, const char *t);
bool strendswith(const char *s, const char *t);

void base64_encode_atom(const unsigned char *data, int n, char *out);
int base64_decode_atom(const char *atom, unsigned char *out);
void base64_decode_bs(BinarySink *bs, ptrlen data);
void base64_decode_fp(FILE *fp, ptrlen data);
strbuf *base64_decode_sb(ptrlen data);
void base64_encode_bs(BinarySink *bs, ptrlen data, int cpl);
void base64_encode_fp(FILE *fp, ptrlen data, int cpl);
strbuf *base64_encode_sb(ptrlen data, int cpl);
bool base64_valid(ptrlen data);

void percent_encode_bs(BinarySink *bs, ptrlen data, const char *badchars);
void percent_encode_fp(FILE *fp, ptrlen data, const char *badchars);
strbuf *percent_encode_sb(ptrlen data, const char *badchars);
void percent_decode_bs(BinarySink *bs, ptrlen data);
void percent_decode_fp(FILE *fp, ptrlen data);
strbuf *percent_decode_sb(ptrlen data);

struct bufchain_granule;
struct bufchain_tag {
    struct bufchain_granule *head, *tail;
    size_t buffersize;           /* current amount of buffered data */

    void (*queue_idempotent_callback)(IdempotentCallback *ic);
    IdempotentCallback *ic;
};

void bufchain_init(bufchain *ch);
void bufchain_clear(bufchain *ch);
size_t bufchain_size(bufchain *ch);
void bufchain_add(bufchain *ch, const void *data, size_t len);
ptrlen bufchain_prefix(bufchain *ch);
void bufchain_consume(bufchain *ch, size_t len);
void bufchain_fetch(bufchain *ch, void *data, size_t len);
void bufchain_fetch_consume(bufchain *ch, void *data, size_t len);
bool bufchain_try_consume(bufchain *ch, size_t len);
bool bufchain_try_fetch(bufchain *ch, void *data, size_t len);
bool bufchain_try_fetch_consume(bufchain *ch, void *data, size_t len);
size_t bufchain_fetch_consume_up_to(bufchain *ch, void *data, size_t len);
void bufchain_set_callback_inner(
    bufchain *ch, IdempotentCallback *ic,
    void (*queue_idempotent_callback)(IdempotentCallback *ic));
static inline void bufchain_set_callback(bufchain *ch, IdempotentCallback *ic)
{
    extern void queue_idempotent_callback(struct IdempotentCallback *ic);
    /* Wrapper that puts in the standard queue_idempotent_callback
     * function. Lives here rather than in bufchain.c so that
     * standalone programs can use the bufchain facility without this
     * optional callback feature and not need to provide a stub of
     * queue_idempotent_callback. */
    bufchain_set_callback_inner(ch, ic, queue_idempotent_callback);
}

bool validate_manual_hostkey(char *key);

struct tm ltime(void);

/*
 * Special form of strcmp which can cope with NULL inputs. NULL is
 * defined to sort before even the empty string.
 */
int nullstrcmp(const char *a, const char *b);

static inline ptrlen make_ptrlen(const void *ptr, size_t len)
{
    ptrlen pl;
    pl.ptr = ptr;
    pl.len = len;
    return pl;
}

static inline const void *ptrlen_end(ptrlen pl)
{
    return (const char *)pl.ptr + pl.len;
}

static inline ptrlen make_ptrlen_startend(const void *startv, const void *endv)
{
    const char *start = (const char *)startv, *end = (const char *)endv;
    assert(end >= start);
    { // WINSCP
    ptrlen pl;
    pl.ptr = start;
    pl.len = end - start;
    return pl;
    } // WINSCP
}

static inline ptrlen ptrlen_from_asciz(const char *str)
{
    return make_ptrlen(str, strlen(str));
}

static inline ptrlen ptrlen_from_strbuf(strbuf *sb)
{
    return make_ptrlen(sb->u, sb->len);
}

bool ptrlen_eq_string(ptrlen pl, const char *str);
bool ptrlen_eq_ptrlen(ptrlen pl1, ptrlen pl2);
int ptrlen_strcmp(ptrlen pl1, ptrlen pl2);
/* ptrlen_startswith and ptrlen_endswith write through their 'tail'
 * argument if and only if it is non-NULL and they return true. Hence
 * you can write ptrlen_startswith(thing, prefix, &thing), writing
 * back to the same ptrlen it read from, to remove a prefix if present
 * and say whether it did so. */
bool ptrlen_startswith(ptrlen whole, ptrlen prefix, ptrlen *tail);
bool ptrlen_endswith(ptrlen whole, ptrlen suffix, ptrlen *tail);
ptrlen ptrlen_get_word(ptrlen *input, const char *separators);
bool ptrlen_contains(ptrlen input, const char *characters);
bool ptrlen_contains_only(ptrlen input, const char *characters);
char *mkstr(ptrlen pl);
int string_length_for_printf(size_t);
/* Derive two printf arguments from a ptrlen, suitable for "%.*s" */
#define PTRLEN_PRINTF(pl) \
    string_length_for_printf((pl).len), (const char *)(pl).ptr
/* Make a ptrlen out of a compile-time string literal. We try to
 * enforce that it _is_ a string literal by token-pasting "" on to it,
 * which should provoke a compile error if it's any other kind of
 * string. */
#define PTRLEN_LITERAL(stringlit) \
    TYPECHECK("" stringlit "", make_ptrlen(stringlit, sizeof(stringlit)-1))
/* Make a ptrlen out of a compile-time string literal in a way that
 * allows you to declare the ptrlen itself as a compile-time initialiser. */
#define PTRLEN_DECL_LITERAL(stringlit) \
    { TYPECHECK("" stringlit "", stringlit), sizeof(stringlit)-1 }
/* Make a ptrlen out of a constant byte array. */
#define PTRLEN_FROM_CONST_BYTES(a) make_ptrlen(a, sizeof(a))

void wordwrap(BinarySink *bs, ptrlen input, size_t maxwid);

/* Wipe sensitive data out of memory that's about to be freed. Simpler
 * than memset because we don't need the fill char parameter; also
 * attempts (by fiddly use of volatile) to inhibit the compiler from
 * over-cleverly trying to optimise the memset away because it knows
 * the variable is going out of scope. */
void smemclr(void *b, size_t len);

/* Compare two fixed-length chunks of memory for equality, without
 * data-dependent control flow (so an attacker with a very accurate
 * stopwatch can't try to guess where the first mismatching byte was).
 * Returns 0 for mismatch or 1 for equality (unlike memcmp), hinted at
 * by the 'eq' in the name. */
unsigned smemeq(const void *av, const void *bv, size_t len);

/* Encode a wide-character string into UTF-8. Tolerates surrogates if
 * sizeof(wchar_t) == 2, assuming that in that case the wide string is
 * encoded in UTF-16. */
char *encode_wide_string_as_utf8(const wchar_t *wstr);

/* Decode UTF-8 to a wide-character string, emitting UTF-16 surrogates
 * if sizeof(wchar_t) == 2. */
wchar_t *decode_utf8_to_wide_string(const char *ustr);

/* Decode a single UTF-8 character. Returns U+FFFD for any of the
 * illegal cases. If the source is empty, returns L'\0' (and sets the
 * error indicator on the source, of course). */
#define DECODE_UTF8_FAILURE_LIST(X) \
    X(DUTF8_SUCCESS, "success")                                      \
    X(DUTF8_SPURIOUS_CONTINUATION, "spurious continuation byte")     \
    X(DUTF8_ILLEGAL_BYTE, "illegal UTF-8 byte value")                \
    X(DUTF8_E_OUT_OF_DATA, "unfinished multibyte encoding at end of string") \
    X(DUTF8_TRUNCATED_SEQUENCE, "multibyte encoding interrupted by " \
      "non-continuation byte")                                       \
    X(DUTF8_OVERLONG_ENCODING, "overlong encoding")                  \
    X(DUTF8_ENCODED_SURROGATE, "Unicode surrogate character encoded in " \
      "UTF-8")                                                       \
    X(DUTF8_CODE_POINT_TOO_BIG, "code point outside the Unicode range") \
    /* end of list */
typedef enum DecodeUTF8Failure {
    #define ENUM_DECL(sym, string) sym,
    DECODE_UTF8_FAILURE_LIST(ENUM_DECL)
    #undef ENUM_DECL
    DUTF8_N_FAILURE_CODES
} DecodeUTF8Failure;
unsigned decode_utf8(BinarySource *src, DecodeUTF8Failure *err);
extern const char *const decode_utf8_error_strings[DUTF8_N_FAILURE_CODES];

/* Decode a single UTF-8 character to an output buffer of the
 * platform's wchar_t. May write a pair of surrogates if
 * sizeof(wchar_t) == 2, assuming that in that case the wide string is
 * encoded in UTF-16. Otherwise, writes one character. Returns the
 * number written. */
size_t decode_utf8_to_wchar(BinarySource *src, wchar_t *out,
                            DecodeUTF8Failure *err);

/* Normalise a UTF-8 string into Normalisation Form C. */
strbuf *utf8_to_nfc(ptrlen input);

/* Determine if a UTF-8 string contains any characters unknown to our
 * supported version of Unicode. */
char *utf8_unknown_char(ptrlen input);

/* Write a string out in C string-literal format. */
void write_c_string_literal(FILE *fp, ptrlen str);

char *buildinfo(const char *newline);

/*
 * A function you can put at points in the code where execution should
 * never reach in the first place. Better than assert(false), or even
 * assert(false && "some explanatory message"), because some compilers
 * don't interpret assert(false) as a declaration of unreachability,
 * so they may still warn about pointless things like some variable
 * not being initialised on the unreachable code path.
 *
 * I follow the assertion with a call to abort() just in case someone
 * compiles with -DNDEBUG, and I wrap that abort inside my own
 * function labelled NORETURN just in case some unusual kind of system
 * header wasn't foresighted enough to label abort() itself that way.
 */
static inline NORETURN void unreachable_internal(void) { 
  #ifndef WINSCP_VS
  // Not to try to link to VS abort
  abort();
  #endif
}
#define unreachable(msg) (assert(false && msg), unreachable_internal())

/*
 * Debugging functions.
 *
 * Output goes to debug.log
 *
 * debug() is like printf().
 *
 * dmemdump() and dmemdumpl() both do memory dumps.  The difference
 * is that dmemdumpl() is more suited for when the memory address is
 * important (say because you'll be recording pointer values later
 * on).  dmemdump() is more concise.
 */

#ifdef DEBUG
void debug_printf(const char *fmt, ...) PRINTF_LIKE(1, 2);
void debug_memdump(const void *buf, int len, bool L);
#define debug(...) (debug_printf(__VA_ARGS__))
#define dmemdump(buf,len) (debug_memdump(buf, len, false))
#define dmemdumpl(buf,len) (debug_memdump(buf, len, true))

/* Functions used only for debugging, not declared unless
 * defined(DEBUG) to avoid accidentally linking them in production */
const char *conf_id(int key);

#else
#define debug(...) ((void)0)
#define dmemdump(buf,len) ((void)0)
#define dmemdumpl(buf,len) ((void)0)
#endif

#ifndef lenof
#define lenof(x) ( (sizeof((x))) / (sizeof(*(x))))
#endif

#ifndef min
#define min(x,y) ( (x) < (y) ? (x) : (y) )
#endif
#ifndef max
#define max(x,y) ( (x) > (y) ? (x) : (y) )
#endif

static inline uint64_t GET_64BIT_LSB_FIRST(const void *vp)
{
    const uint8_t *p = (const uint8_t *)vp;
    return (((uint64_t)p[0]      ) | ((uint64_t)p[1] <<  8) |
            ((uint64_t)p[2] << 16) | ((uint64_t)p[3] << 24) |
            ((uint64_t)p[4] << 32) | ((uint64_t)p[5] << 40) |
            ((uint64_t)p[6] << 48) | ((uint64_t)p[7] << 56));
}

static inline void PUT_64BIT_LSB_FIRST(void *vp, uint64_t value)
{
    uint8_t *p = (uint8_t *)vp;
    p[0] = (uint8_t)(value);
    p[1] = (uint8_t)(value >> 8);
    p[2] = (uint8_t)(value >> 16);
    p[3] = (uint8_t)(value >> 24);
    p[4] = (uint8_t)(value >> 32);
    p[5] = (uint8_t)(value >> 40);
    p[6] = (uint8_t)(value >> 48);
    p[7] = (uint8_t)(value >> 56);
}

static inline uint32_t GET_32BIT_LSB_FIRST(const void *vp)
{
    const uint8_t *p = (const uint8_t *)vp;
    return (((uint32_t)p[0]      ) | ((uint32_t)p[1] <<  8) |
            ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24));
}

static inline void PUT_32BIT_LSB_FIRST(void *vp, uint32_t value)
{
    uint8_t *p = (uint8_t *)vp;
    p[0] = (uint8_t)(value);
    p[1] = (uint8_t)(value >> 8);
    p[2] = (uint8_t)(value >> 16);
    p[3] = (uint8_t)(value >> 24);
}

static inline uint16_t GET_16BIT_LSB_FIRST(const void *vp)
{
    const uint8_t *p = (const uint8_t *)vp;
    return (((uint16_t)p[0]      ) | ((uint16_t)p[1] <<  8));
}

static inline void PUT_16BIT_LSB_FIRST(void *vp, uint16_t value)
{
    uint8_t *p = (uint8_t *)vp;
    p[0] = (uint8_t)(value);
    p[1] = (uint8_t)(value >> 8);
}

static inline uint64_t GET_64BIT_MSB_FIRST(const void *vp)
{
    const uint8_t *p = (const uint8_t *)vp;
    return (((uint64_t)p[7]      ) | ((uint64_t)p[6] <<  8) |
            ((uint64_t)p[5] << 16) | ((uint64_t)p[4] << 24) |
            ((uint64_t)p[3] << 32) | ((uint64_t)p[2] << 40) |
            ((uint64_t)p[1] << 48) | ((uint64_t)p[0] << 56));
}

static inline void PUT_64BIT_MSB_FIRST(void *vp, uint64_t value)
{
    uint8_t *p = (uint8_t *)vp;
    p[7] = (uint8_t)(value);
    p[6] = (uint8_t)(value >> 8);
    p[5] = (uint8_t)(value >> 16);
    p[4] = (uint8_t)(value >> 24);
    p[3] = (uint8_t)(value >> 32);
    p[2] = (uint8_t)(value >> 40);
    p[1] = (uint8_t)(value >> 48);
    p[0] = (uint8_t)(value >> 56);
}

static inline uint32_t GET_32BIT_MSB_FIRST(const void *vp)
{
    const uint8_t *p = (const uint8_t *)vp;
    return (((uint32_t)p[3]      ) | ((uint32_t)p[2] <<  8) |
            ((uint32_t)p[1] << 16) | ((uint32_t)p[0] << 24));
}

static inline void PUT_32BIT_MSB_FIRST(void *vp, uint32_t value)
{
    uint8_t *p = (uint8_t *)vp;
    p[3] = (uint8_t)(value);
    p[2] = (uint8_t)(value >> 8);
    p[1] = (uint8_t)(value >> 16);
    p[0] = (uint8_t)(value >> 24);
}

static inline uint16_t GET_16BIT_MSB_FIRST(const void *vp)
{
    const uint8_t *p = (const uint8_t *)vp;
    return (((uint16_t)p[1]      ) | ((uint16_t)p[0] <<  8));
}

static inline void PUT_16BIT_MSB_FIRST(void *vp, uint16_t value)
{
    uint8_t *p = (uint8_t *)vp;
    p[1] = (uint8_t)(value);
    p[0] = (uint8_t)(value >> 8);
}

/* For use in X11-related applications, an endianness-variable form of
 * {GET,PUT}_16BIT which expects 'endian' to be either 'B' or 'l' */

static inline uint16_t GET_16BIT_X11(char endian, const void *p)
{
    return endian == 'B' ? GET_16BIT_MSB_FIRST(p) : GET_16BIT_LSB_FIRST(p);
}

static inline void PUT_16BIT_X11(char endian, void *p, uint16_t value)
{
    if (endian == 'B')
        PUT_16BIT_MSB_FIRST(p, value);
    else
        PUT_16BIT_LSB_FIRST(p, value);
}

/* Replace NULL with the empty string, permitting an idiom in which we
 * get a string (pointer,length) pair that might be NULL,0 and can
 * then safely say things like printf("%.*s", length, NULLTOEMPTY(ptr)) */
static inline const char *NULLTOEMPTY(const char *s)
{
    return s ? s : "";
}

/* StripCtrlChars, defined in stripctrl.c: an adapter you can put on
 * the front of one BinarySink and which functions as one in turn.
 * Interprets its input as a stream of multibyte characters in the
 * system locale, and removes any that are not either printable
 * characters or newlines. */
struct StripCtrlChars {
    BinarySink_IMPLEMENTATION;
    /* and this is contained in a larger structure */
};
StripCtrlChars *stripctrl_new(
    BinarySink *bs_out, bool permit_cr, wchar_t substitution);
StripCtrlChars *stripctrl_new_term_fn(
    BinarySink *bs_out, bool permit_cr, wchar_t substitution,
    Terminal *term, unsigned long (*translate)(
        Terminal *, term_utf8_decode *, unsigned char));
#define stripctrl_new_term(bs, cr, sub, term) \
    stripctrl_new_term_fn(bs, cr, sub, term, term_translate)
void stripctrl_retarget(StripCtrlChars *sccpub, BinarySink *new_bs_out);
void stripctrl_reset(StripCtrlChars *sccpub);
void stripctrl_free(StripCtrlChars *sanpub);
void stripctrl_enable_line_limiting(StripCtrlChars *sccpub);
#ifndef WINSCP
char *stripctrl_string_ptrlen(StripCtrlChars *sccpub, ptrlen str);
static inline char *stripctrl_string(StripCtrlChars *sccpub, const char *str)
{
    return stripctrl_string_ptrlen(sccpub, ptrlen_from_asciz(str));
}
#endif

#ifdef MPEXT
// Recent PuTTY code uses C99 standard that allows code before initialization.
// Frequently that code are assertions. This assert implementation allows being used before code.
#define pinitassert(P) const int __assert_dummy = 1/((int)(P))
#endif

/*
 * A mechanism for loading a file from disk into a memory buffer where
 * it can be picked apart as a BinarySource.
 */
struct LoadedFile {
    char *data;
    size_t len, max_size;
    BinarySource_IMPLEMENTATION;
};
typedef enum {
    LF_OK,      /* file loaded successfully */
    LF_TOO_BIG, /* file didn't fit in buffer */
    LF_ERROR,   /* error from stdio layer */
} LoadFileStatus;
LoadedFile *lf_new(size_t max_size);
void lf_free(LoadedFile *lf);
LoadFileStatus lf_load_fp(LoadedFile *lf, FILE *fp);
LoadFileStatus lf_load(LoadedFile *lf, const Filename *filename);
static inline ptrlen ptrlen_from_lf(LoadedFile *lf)
{ return make_ptrlen(lf->data, lf->len); }

/* Set the memory block of 'size' bytes at 'out' to the bitwise XOR of
 * the two blocks of the same size at 'in1' and 'in2'.
 *
 * 'out' may point to exactly the same address as one of the inputs,
 * but if the input and output blocks overlap in any other way, the
 * result of this function is not guaranteed. No memmove-style effort
 * is made to handle difficult overlap cases. */
void memxor(uint8_t *out, const uint8_t *in1, const uint8_t *in2, size_t size);

/* Boolean expressions used in OpenSSH certificate configuration */
bool cert_expr_valid(const char *expression,
                     char **error_msg, ptrlen *error_loc);
bool cert_expr_match_str(const char *expression,
                         const char *hostname, unsigned port);
/* Build a certificate expression out of hostname wildcards. Required
 * to handle legacy configuration from early in development, when
 * multiple wildcards were stored separately in config, implicitly
 * ORed together. */
CertExprBuilder *cert_expr_builder_new(void);
void cert_expr_builder_free(CertExprBuilder *eb);
void cert_expr_builder_add(CertExprBuilder *eb, const char *wildcard);
char *cert_expr_expression(CertExprBuilder *eb);

#endif
