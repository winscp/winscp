/*
 * Header for misc.c.
 */

#ifndef PUTTY_MISC_H
#define PUTTY_MISC_H

#include "puttymem.h"

#include <stdio.h>		       /* for FILE * */
#include <stdarg.h>		       /* for va_list */
#include <time.h>                      /* for struct tm */

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

typedef struct Filename Filename;
typedef struct FontSpec FontSpec;

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
typedef struct strbuf strbuf;
strbuf *strbuf_new(void);
void strbuf_free(strbuf *buf);
char *strbuf_str(strbuf *buf);         /* does not free buf */
char *strbuf_to_str(strbuf *buf); /* does free buf, but you must free result */
void strbuf_catf(strbuf *buf, const char *fmt, ...);
void strbuf_catfv(strbuf *buf, const char *fmt, va_list ap);

/* String-to-Unicode converters that auto-allocate the destination and
 * work around the rather deficient interface of mb_to_wc.
 *
 * These actually live in miscucs.c, not misc.c (the distinction being
 * that the former is only linked into tools that also have the main
 * Unicode support). */
wchar_t *dup_mb_to_wc_c(int codepage, int flags, const char *string, int len);
wchar_t *dup_mb_to_wc(int codepage, int flags, const char *string);

int toint(unsigned);

char *fgetline(FILE *fp);
char *chomp(char *str);
int strstartswith(const char *s, const char *t);
int strendswith(const char *s, const char *t);

void base64_encode_atom(const unsigned char *data, int n, char *out);
int base64_decode_atom(const char *atom, unsigned char *out);

struct bufchain_granule;
struct bufchain_tag {
    struct bufchain_granule *head, *tail;
    int buffersize;		       /* current amount of buffered data */
};
#ifndef BUFCHAIN_TYPEDEF
typedef struct bufchain_tag bufchain;  /* rest of declaration in misc.c */
#define BUFCHAIN_TYPEDEF
#endif

void bufchain_init(bufchain *ch);
void bufchain_clear(bufchain *ch);
int bufchain_size(bufchain *ch);
void bufchain_add(bufchain *ch, const void *data, int len);
void bufchain_prefix(bufchain *ch, void **data, int *len);
void bufchain_consume(bufchain *ch, int len);
void bufchain_fetch(bufchain *ch, void *data, int len);

int validate_manual_hostkey(char *key);

struct tm ltime(void);

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
int smemeq(const void *av, const void *bv, size_t len);

/* Extracts an SSH-marshalled string from the start of *data. If
 * successful (*datalen is not too small), advances data/datalen past
 * the string and returns a pointer to the string itself and its
 * length in *stringlen. Otherwise does nothing and returns NULL.
 *
 * Like strchr, this function can discard const from its parameter.
 * Treat it as if it was a family of two functions, one returning a
 * non-const string given a non-const pointer, and one taking and
 * returning const. */
void *get_ssh_string(int *datalen, const void **data, int *stringlen);
/* Extracts an SSH uint32, similarly. Returns TRUE on success, and
 * leaves the extracted value in *ret. */
int get_ssh_uint32(int *datalen, const void **data, unsigned *ret);
/* Given a not-necessarily-zero-terminated string in (length,data)
 * form, check if it equals an ordinary C zero-terminated string. */
int match_ssh_id(int stringlen, const void *string, const char *id);

char *buildinfo(const char *newline);

/*
 * Debugging functions.
 *
 * Output goes to debug.log
 *
 * debug(()) (note the double brackets) is like printf().
 *
 * dmemdump() and dmemdumpl() both do memory dumps.  The difference
 * is that dmemdumpl() is more suited for when the memory address is
 * important (say because you'll be recording pointer values later
 * on).  dmemdump() is more concise.
 */

#ifdef DEBUG
void debug_printf(const char *fmt, ...);
void debug_memdump(const void *buf, int len, int L);
#define debug(x) (debug_printf x)
#define dmemdump(buf,len) debug_memdump (buf, len, 0);
#define dmemdumpl(buf,len) debug_memdump (buf, len, 1);
#else
#define debug(x)
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

#define GET_32BIT_LSB_FIRST(cp) \
  (((unsigned long)(unsigned char)(cp)[0]) | \
  ((unsigned long)(unsigned char)(cp)[1] << 8) | \
  ((unsigned long)(unsigned char)(cp)[2] << 16) | \
  ((unsigned long)(unsigned char)(cp)[3] << 24))

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
  (((unsigned long)(unsigned char)(cp)[0] << 24) | \
  ((unsigned long)(unsigned char)(cp)[1] << 16) | \
  ((unsigned long)(unsigned char)(cp)[2] << 8) | \
  ((unsigned long)(unsigned char)(cp)[3]))

#define GET_32BIT(cp) GET_32BIT_MSB_FIRST(cp)

#define PUT_32BIT_MSB_FIRST(cp, value) ( \
  (cp)[0] = (unsigned char)((value) >> 24), \
  (cp)[1] = (unsigned char)((value) >> 16), \
  (cp)[2] = (unsigned char)((value) >> 8), \
  (cp)[3] = (unsigned char)(value) )

#define PUT_32BIT(cp, value) PUT_32BIT_MSB_FIRST(cp, value)

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

#endif
