#ifndef PUTTY_MISC_H
#define PUTTY_MISC_H

#include "puttymem.h"

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

char *dupstr(char *s);
char *dupcat(char *s1, ...);

void base64_encode_atom(unsigned char *data, int n, char *out);

struct bufchain_granule;
typedef struct bufchain_tag {
    struct bufchain_granule *head, *tail;
    int buffersize;		       /* current amount of buffered data */
} bufchain;

void bufchain_init(bufchain *ch);
void bufchain_clear(bufchain *ch);
int bufchain_size(bufchain *ch);
void bufchain_add(bufchain *ch, void *data, int len);
void bufchain_prefix(bufchain *ch, void **data, int *len);
void bufchain_consume(bufchain *ch, int len);
void bufchain_fetch(bufchain *ch, void *data, int len);

/*
 * Debugging functions.
 *
 * Output goes to debug.log
 *
 * debug(()) (note the double brackets) is like printf().
 *
 * dmemdump() and dmemdumpl() both do memory dumps.  The difference
 * is that dmemdumpl() is more suited for when where the memory is is
 * important (say because you'll be recording pointer values later
 * on).  dmemdump() is more concise.
 */

#ifdef DEBUG
void dprintf(char *fmt, ...);
void debug_memdump(void *buf, int len, int L);
#define debug(x) (dprintf x)
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


#endif
