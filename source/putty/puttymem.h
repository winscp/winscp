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
 * Helper macros to deal with the common use case of growing an array.
 *
 * The common setup is that 'array' is a pointer to the first element
 * of a dynamic array of some type, and 'size' represents the current
 * allocated size of that array (in elements). Both of those macro
 * parameters are implicitly written back to.
 *
 * Then sgrowarray(array, size, n) means: make sure the nth element of
 * the array exists (i.e. the size is at least n+1). You call that
 * before writing to the nth element, if you're looping round
 * appending to the array.
 *
 * If you need to grow the array by more than one element, you can
 * instead call sgrowarrayn(array, size, n, m), which will ensure the
 * size of the array is at least n+m. (So sgrowarray is just the
 * special case of that in which m == 1.)
 *
 * It's common to call sgrowarrayn with one of n,m equal to the
 * previous logical length of the array, and the other equal to the
 * new number of logical entries you want to add, so that n <= size on
 * entry. But that's not actually a mandatory precondition: the two
 * length parameters are just arbitrary integers that get added
 * together with an initial check for overflow, and the semantics are
 * simply 'make sure the array is big enough to take their sum, no
 * matter how big it was to start with'.)
 *
 * Another occasionally useful idiom is to call sgrowarray with n ==
 * size, i.e. sgrowarray(array, size, size). That just means: make
 * array bigger by _some_ amount, I don't particularly mind how much.
 * You might use that style if you were repeatedly calling an API
 * function outside your control, which would either fill your buffer
 * and return success, or else return a 'too big' error without
 * telling you how much bigger it needed to be.
 *
 * The _nm variants of the macro set the 'private' flag in the
 * underlying function, which forces array resizes to be done by a
 * manual allocate/copy/free instead of realloc, with careful clearing
 * of the previous memory block before we free it. This costs
 * performance, but if the block contains important secrets such as
 * private keys or passwords, it avoids the risk that a realloc that
 * moves the memory block might leave a copy of the data visible in
 * the freed memory at the previous location.
 */
void *safegrowarray(void *array, size_t *size, size_t eltsize,
                    size_t oldlen, size_t extralen, bool private);

/* The master macro wrapper, of which all others are special cases */
#define sgrowarray_general(array, size, n, m, priv)                     \
    ((array) = safegrowarray(array, &(size), sizeof(*array), n, m, priv))

/* The special-case macros that are easier to use in most situations */
#define sgrowarrayn(   a, s, n, m) sgrowarray_general(a, s, n, m, false)
#define sgrowarray(    a, s, n   ) sgrowarray_general(a, s, n, 1, false)
#define sgrowarrayn_nm(a, s, n, m) sgrowarray_general(a, s, n, m, true )
#define sgrowarray_nm( a, s, n   ) sgrowarray_general(a, s, n, 1, true )

/*
 * This function is called by the innermost safemalloc/saferealloc
 * functions when allocation fails. Usually it's provided by misc.c
 * which ties it into an application's existing modalfatalbox()
 * system, but standalone test applications can reimplement it some
 * other way if they prefer.
 */
NORETURN void out_of_memory(void);

#ifdef MINEFIELD
/*
 * Definitions for Minefield, PuTTY's own Windows-specific malloc
 * debugger in the style of Electric Fence. Implemented in winmisc.c,
 * and referred to by the main malloc wrappers in memory.c.
 */
void *minefield_c_malloc(size_t size);
void minefield_c_free(void *p);
void *minefield_c_realloc(void *p, size_t size);
#endif

#endif
