/*
 * PuTTY memory-handling header.
 */

#ifndef PUTTY_PUTTYMEM_H
#define PUTTY_PUTTYMEM_H

#include <stddef.h>		       /* for size_t */
#include <string.h>		       /* for memcpy() */

#include "defs.h"

#define smalloc(z) safemalloc(z,1)
#define snmalloc safemalloc
#define srealloc(y,z) saferealloc(y,z,1)
#define snrealloc saferealloc
#define sfree safefree

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

/*
 * For cases where you want to allocate a struct plus a subsidiary
 * data buffer in one step, this macro lets you add a constant to the
 * amount malloced.
 *
 * Since the return value is already cast to the struct type, a
 * pointer to that many bytes of extra data can be conveniently
 * obtained by simply adding 1 to the returned pointer!
 * snew_plus_get_aux is a handy macro that does that and casts the
 * result to void *, so you can assign it straight to wherever you
 * wanted it.
 */
#define snew_plus(type, extra) ((type *)snmalloc(1, sizeof(type) + (extra)))
#define snew_plus_get_aux(ptr) ((void *)((ptr) + 1))

/*
 * This function is called by the innermost safemalloc/saferealloc
 * functions when allocation fails. Usually it's provided by misc.c
 * which ties it into an application's existing modalfatalbox()
 * system, but standalone test applications can reimplement it some
 * other way if they prefer.
 */
NORETURN void out_of_memory(void);

#endif
