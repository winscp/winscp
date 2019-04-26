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
 * possible, in favour of these type-casting macros which ensure you
 * don't mistakenly allocate enough space for one sort of structure
 * and assign it to a different sort of pointer. sresize also uses
 * TYPECHECK to verify that the _input_ pointer is a pointer to the
 * correct type.
 */
#define snew(type) ((type *)snmalloc(1, sizeof(type)))
#define snewn(n, type) ((type *)snmalloc((n), sizeof(type)))
#define sresize(ptr, n, type) TYPECHECK((type *)0 == (ptr), \
    ((type *)snrealloc((ptr), (n), sizeof(type))))

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
