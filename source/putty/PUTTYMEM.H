/*
 * PuTTY memory-handling header.
 */

#ifndef PUTTY_PUTTYMEM_H
#define PUTTY_PUTTYMEM_H

#include <stddef.h>		       /* for size_t */
#include <string.h>		       /* for memcpy() */


/* #define MALLOC_LOG  do this if you suspect putty of leaking memory */
#ifdef MALLOC_LOG
#define smalloc(z) (mlog(__FILE__,__LINE__), safemalloc(z,1))
#define snmalloc(z,s) (mlog(__FILE__,__LINE__), safemalloc(z,s))
#define srealloc(y,z) (mlog(__FILE__,__LINE__), saferealloc(y,z,1))
#define snrealloc(y,z,s) (mlog(__FILE__,__LINE__), saferealloc(y,z,s))
#define sfree(z) (mlog(__FILE__,__LINE__), safefree(z))
void mlog(char *, int);
#else
#define smalloc(z) safemalloc(z,1)
#define snmalloc safemalloc
#define srealloc(y,z) saferealloc(y,z,1)
#define snrealloc saferealloc
#define sfree safefree
#endif

void *safemalloc(size_t, size_t);
void *saferealloc(void *, size_t, size_t);
void safefree(void *);

/*
 * Direct use of smalloc within the code should be avoided where
 * possible, in favour of these type-casting macros which ensure
 * you don't mistakenly allocate enough space for one sort of
 * structure and assign it to a different sort of pointer.
 *
 * The nasty trick in sresize with sizeof arranges for the compiler,
 * in passing, to type-check the expression ((type *)0 == (ptr)), i.e.
 * to type-check that the input pointer is a pointer to the correct
 * type. The construction sizeof(stuff) ? (b) : (b) looks like a
 * violation of the first principle of safe macros, but in fact it's
 * OK - although it _expands_ the macro parameter more than once, it
 * only _evaluates_ it once, so it's still side-effect safe.
 */
#define snew(type) ((type *)snmalloc(1, sizeof(type)))
#define snewn(n, type) ((type *)snmalloc((n), sizeof(type)))
#define sresize(ptr, n, type)                                           \
    ((type *)snrealloc(sizeof((type *)0 == (ptr)) ? (ptr) : (ptr),      \
                       (n), sizeof(type)))

#endif
