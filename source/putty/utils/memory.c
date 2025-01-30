/*
 * PuTTY's memory allocation wrappers.
 */

#ifdef ALLOCATION_ALIGNMENT
/* Before we include standard headers, define _ISOC11_SOURCE so that
 * we get the declaration of aligned_alloc(). */
#define _ISOC11_SOURCE
#endif

#include <assert.h>
#include <stdlib.h>
#include <limits.h>

#include "defs.h"
#include "puttymem.h"
#include "misc.h"

void *safemalloc(size_t factor1, size_t factor2, size_t addend)
{
    if (factor1 > SIZE_MAX / factor2)
        goto fail;
    { // WINSCP
    size_t product = factor1 * factor2;

    if (addend > SIZE_MAX)
        goto fail;
    if (product > SIZE_MAX - addend)
        goto fail;
    { // WINSCP
    size_t size = product + addend;

    if (size == 0)
        size = 1;

    { // WINSCP
    void *p;
#ifdef MINEFIELD
    p = minefield_c_malloc(size);
#elif defined ALLOCATION_ALIGNMENT
    /* aligned_alloc requires the allocation size to be rounded up */
    p = aligned_alloc(
        ALLOCATION_ALIGNMENT,
        (size + ALLOCATION_ALIGNMENT - 1) & ~(ALLOCATION_ALIGNMENT-1));
#else
    p = malloc(size);
#endif

    if (!p)
        goto fail;

    return p;

  fail:
    out_of_memory();
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

void *saferealloc(void *ptr, size_t n, size_t size)
{
    void *p;

    if (n > INT_MAX / size) {
        p = NULL;
    } else {
        size *= n;
        if (!ptr) {
#ifdef MINEFIELD
            p = minefield_c_malloc(size);
#elif defined ALLOCATION_ALIGNMENT
            p = aligned_alloc(ALLOCATION_ALIGNMENT, size);
#else
            p = malloc(size);
#endif
        } else {
#ifdef MINEFIELD
            p = minefield_c_realloc(ptr, size);
#else
            p = realloc(ptr, size);
#endif
        }
    }

    if (!p)
        out_of_memory();

    return p;
}

void safefree(void *ptr)
{
    if (ptr) {
#ifdef MINEFIELD
        minefield_c_free(ptr);
#else
        free(ptr);
#endif
    }
}

void *safegrowarray(void *ptr, size_t *allocated, size_t eltsize,
                    size_t oldlen, size_t extralen, bool secret)
{
    /* The largest value we can safely multiply by eltsize */
    pinitassert(eltsize > 0);
    size_t maxsize = (~(size_t)0) / eltsize;

    size_t oldsize = *allocated;

    /* Range-check the input values */
    assert(oldsize <= maxsize);
    assert(oldlen <= maxsize);
    assert(extralen <= maxsize - oldlen);

    /* If the size is already enough, don't bother doing anything! */
    if (oldsize > oldlen + extralen)
        return ptr;

    /* Find out how much we need to grow the array by. */
    { // WINSCP
    size_t increment = (oldlen + extralen) - oldsize;

    /* Invent a new size. We want to grow the array by at least
     * 'increment' elements; by at least a fixed number of bytes (to
     * get things started when sizes are small); and by some constant
     * factor of its old size (to avoid repeated calls to this
     * function taking quadratic time overall). */
    if (increment < 256 / eltsize)
        increment = 256 / eltsize;
    if (increment < oldsize / 16)
        increment = oldsize / 16;

    /* But we also can't grow beyond maxsize. */
    { // WINSCP
    size_t maxincr = maxsize - oldsize;
    if (increment > maxincr)
        increment = maxincr;

    { // WINSCP
    size_t newsize = oldsize + increment;
    void *toret;
    if (secret) {
        toret = safemalloc(newsize, eltsize, 0);
        if (oldsize) {
            memcpy(toret, ptr, oldsize * eltsize);
            smemclr(ptr, oldsize * eltsize);
            sfree(ptr);
        }
    } else {
        toret = saferealloc(ptr, newsize, eltsize);
    }
    *allocated = newsize;
    return toret;
    } // WINSCP
    } // WINSCP
    } // WINSCP
}
