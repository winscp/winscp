/*
 * Header for misc.c.
 */

#ifndef PUTTY_MISC_H
#define PUTTY_MISC_H

#include "defs.h"
#include "puttymem.h"
#include "marshal.h"

#include <stdio.h>		       /* for FILE * */
#include <stdarg.h>		       /* for va_list */
#include <time.h>                      /* for struct tm */
#include <limits.h>                    /* for INT_MAX/MIN */

unsigned long parse_blocksize(const char *bs);
char ctrlparse(char *s, char **next);

size_t host_strcspn(const char *s, const char *set);
char *host_strchr(const char *s, int c);
char *host_strrchr(const char *s, int c);
char *host_strduptrim(const char *s);

char *dupstr(const char *s);
char *dupcat(const char *s1, ...);
char *dupprintf(const char *fmt, ...)
#ifdef __GNUC__
    __attribute__ ((format (printf, 1, 2)))
#endif
    ;
char *dupvprintf(const char *fmt, va_list ap);
void burnstr(char *string);

struct strbuf {
    char *s;
    unsigned char *u;
    int len;
    BinarySink_IMPLEMENTATION;
    /* (also there's a surrounding implementation struct in misc.c) */
};
strbuf *strbuf_new(void);
void strbuf_free(strbuf *buf);
void *strbuf_append(strbuf *buf, size_t len);
char *strbuf_to_str(strbuf *buf); /* does free buf, but you must free result */
void strbuf_catf(strbuf *buf, const char *fmt, ...);
void strbuf_catfv(strbuf *buf, const char *fmt, va_list ap);

strbuf *strbuf_new_for_agent_query(void);
void strbuf_finalise_agent_query(strbuf *buf);

/* String-to-Unicode converters that auto-allocate the destination and
 * work around the rather deficient interface of mb_to_wc.
 *
 * These actually live in miscucs.c, not misc.c (the distinction being
 * that the former is only linked into tools that also have the main
 * Unicode support). */
wchar_t *dup_mb_to_wc_c(int codepage, int flags, const char *string, int len);
wchar_t *dup_mb_to_wc(int codepage, int flags, const char *string);

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
char *chomp(char *str);
bool strstartswith(const char *s, const char *t);
bool strendswith(const char *s, const char *t);

void base64_encode_atom(const unsigned char *data, int n, char *out);
int base64_decode_atom(const char *atom, unsigned char *out);

struct bufchain_granule;
struct bufchain_tag {
    struct bufchain_granule *head, *tail;
    int buffersize;		       /* current amount of buffered data */
    IdempotentCallback *ic;
};

void bufchain_init(bufchain *ch);
void bufchain_clear(bufchain *ch);
int bufchain_size(bufchain *ch);
void bufchain_add(bufchain *ch, const void *data, int len);
void bufchain_prefix(bufchain *ch, void **data, int *len);
void bufchain_consume(bufchain *ch, int len);
void bufchain_fetch(bufchain *ch, void *data, int len);
void bufchain_fetch_consume(bufchain *ch, void *data, int len);
bool bufchain_try_fetch_consume(bufchain *ch, void *data, int len);
int bufchain_fetch_consume_up_to(bufchain *ch, void *data, int len);

void sanitise_term_data(bufchain *out, const void *vdata, int len);

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
bool ptrlen_startswith(ptrlen whole, ptrlen prefix, ptrlen *tail);
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

/* Wipe sensitive data out of memory that's about to be freed. Simpler
 * than memset because we don't need the fill char parameter; also
 * attempts (by fiddly use of volatile) to inhibit the compiler from
 * over-cleverly trying to optimise the memset away because it knows
 * the variable is going out of scope. */
void smemclr(void *b, size_t len);

/* Compare two fixed-length chunks of memory for equality, without
 * data-dependent control flow (so an attacker with a very accurate
 * stopwatch can't try to guess where the first mismatching byte was).
 * Returns false for mismatch or true for equality (unlike memcmp),
 * hinted at by the 'eq' in the name. */
bool smemeq(const void *av, const void *bv, size_t len);

char *buildinfo(const char *newline);

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
void debug_printf(const char *fmt, ...);
void debug_memdump(const void *buf, int len, bool L);
#define debug(...) (debug_printf(__VA_ARGS__))
#define dmemdump(buf,len) debug_memdump (buf, len, false);
#define dmemdumpl(buf,len) debug_memdump (buf, len, true);
#else
#define debug(...)
#define dmemdump(buf,len)
#define dmemdumpl(buf,len)
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

#define GET_64BIT_LSB_FIRST(cp) \
  (((uint64_t)(unsigned char)(cp)[0]) | \
  ((uint64_t)(unsigned char)(cp)[1] << 8) | \
  ((uint64_t)(unsigned char)(cp)[2] << 16) | \
  ((uint64_t)(unsigned char)(cp)[3] << 24) | \
  ((uint64_t)(unsigned char)(cp)[4] << 32) | \
  ((uint64_t)(unsigned char)(cp)[5] << 40) | \
  ((uint64_t)(unsigned char)(cp)[6] << 48) | \
  ((uint64_t)(unsigned char)(cp)[7] << 56))

#define PUT_64BIT_LSB_FIRST(cp, value) ( \
  (cp)[0] = (unsigned char)(value), \
  (cp)[1] = (unsigned char)((value) >> 8), \
  (cp)[2] = (unsigned char)((value) >> 16), \
  (cp)[3] = (unsigned char)((value) >> 24), \
  (cp)[4] = (unsigned char)((value) >> 32), \
  (cp)[5] = (unsigned char)((value) >> 40), \
  (cp)[6] = (unsigned char)((value) >> 48), \
  (cp)[7] = (unsigned char)((value) >> 56) )

#define GET_32BIT_LSB_FIRST(cp) \
  (((uint32_t)(unsigned char)(cp)[0]) | \
  ((uint32_t)(unsigned char)(cp)[1] << 8) | \
  ((uint32_t)(unsigned char)(cp)[2] << 16) | \
  ((uint32_t)(unsigned char)(cp)[3] << 24))

#define PUT_32BIT_LSB_FIRST(cp, value) ( \
  (cp)[0] = (unsigned char)(value), \
  (cp)[1] = (unsigned char)((value) >> 8), \
  (cp)[2] = (unsigned char)((value) >> 16), \
  (cp)[3] = (unsigned char)((value) >> 24) )

#define GET_16BIT_LSB_FIRST(cp) \
  (((unsigned long)(unsigned char)(cp)[0]) | \
  ((unsigned long)(unsigned char)(cp)[1] << 8))

#define PUT_16BIT_LSB_FIRST(cp, value) ( \
  (cp)[0] = (unsigned char)(value), \
  (cp)[1] = (unsigned char)((value) >> 8) )

#define GET_32BIT_MSB_FIRST(cp) \
  (((uint32_t)(unsigned char)(cp)[0] << 24) | \
  ((uint32_t)(unsigned char)(cp)[1] << 16) | \
  ((uint32_t)(unsigned char)(cp)[2] << 8) | \
  ((uint32_t)(unsigned char)(cp)[3]))

#define GET_32BIT(cp) GET_32BIT_MSB_FIRST(cp)

#define PUT_32BIT_MSB_FIRST(cp, value) ( \
  (cp)[0] = (unsigned char)((value) >> 24), \
  (cp)[1] = (unsigned char)((value) >> 16), \
  (cp)[2] = (unsigned char)((value) >> 8), \
  (cp)[3] = (unsigned char)(value) )

#define PUT_32BIT(cp, value) PUT_32BIT_MSB_FIRST(cp, value)

#define GET_64BIT_MSB_FIRST(cp) \
  (((uint64_t)(unsigned char)(cp)[0] << 56) | \
  ((uint64_t)(unsigned char)(cp)[1] << 48) | \
  ((uint64_t)(unsigned char)(cp)[2] << 40) | \
  ((uint64_t)(unsigned char)(cp)[3] << 32) | \
  ((uint64_t)(unsigned char)(cp)[4] << 24) | \
  ((uint64_t)(unsigned char)(cp)[5] << 16) | \
  ((uint64_t)(unsigned char)(cp)[6] << 8) | \
  ((uint64_t)(unsigned char)(cp)[7]))

#define PUT_64BIT_MSB_FIRST(cp, value) ( \
  (cp)[0] = (unsigned char)((value) >> 56), \
  (cp)[1] = (unsigned char)((value) >> 48), \
  (cp)[2] = (unsigned char)((value) >> 40), \
  (cp)[3] = (unsigned char)((value) >> 32), \
  (cp)[4] = (unsigned char)((value) >> 24), \
  (cp)[5] = (unsigned char)((value) >> 16), \
  (cp)[6] = (unsigned char)((value) >> 8), \
  (cp)[7] = (unsigned char)(value) )

#define GET_16BIT_MSB_FIRST(cp) \
  (((unsigned long)(unsigned char)(cp)[0] << 8) | \
  ((unsigned long)(unsigned char)(cp)[1]))

#define PUT_16BIT_MSB_FIRST(cp, value) ( \
  (cp)[0] = (unsigned char)((value) >> 8), \
  (cp)[1] = (unsigned char)(value) )

/* Replace NULL with the empty string, permitting an idiom in which we
 * get a string (pointer,length) pair that might be NULL,0 and can
 * then safely say things like printf("%.*s", length, NULLTOEMPTY(ptr)) */
#define NULLTOEMPTY(s) ((s)?(s):"")

#ifdef MPEXT
// Recent PuTTY code uses C99 standard that allows code before initialization.
// Mostly that code are assertions. This assert implementation allows being used before code.
#define pinitassert(P) const int __assert_dummy = 1/(P)
#endif

#endif
