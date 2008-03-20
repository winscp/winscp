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

char *dupstr(const char *s);
char *dupcat(const char *s1, ...);
char *dupprintf(const char *fmt, ...);
char *dupvprintf(const char *fmt, va_list ap);

char *fgetline(FILE *fp);

void base64_encode_atom(unsigned char *data, int n, char *out);

struct bufchain_granule;
typedef struct bufchain_tag {
    struct bufchain_granule *head, *tail;
    int buffersize;		       /* current amount of buffered data */
} bufchain;

void bufchain_init(bufchain *ch);
void bufchain_clear(bufchain *ch);
int bufchain_size(bufchain *ch);
void bufchain_add(bufchain *ch, const void *data, int len);
void bufchain_prefix(bufchain *ch, void **data, int *len);
void bufchain_consume(bufchain *ch, int len);
void bufchain_fetch(bufchain *ch, void *data, int len);

struct tm ltime(void);

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
void debug_printf(char *fmt, ...);
void debug_memdump(void *buf, int len, int L);
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

#endif
