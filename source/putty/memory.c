/*
 * PuTTY's memory allocation wrappers.
 */

#include <assert.h>
#include <stdlib.h>
#include <limits.h>

#include "defs.h"
#include "puttymem.h"
#include "misc.h"

void *safemalloc(size_t n, size_t size)
{
    void *p;

    if (n > INT_MAX / size) {
	p = NULL;
    } else {
	size *= n;
	if (size == 0) size = 1;
#ifdef MINEFIELD
	p = minefield_c_malloc(size);
#else
	p = malloc(size);
#endif
    }

    if (!p)
        out_of_memory();

    return p;
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
    assert(eltsize > 0);
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
    size_t maxincr = maxsize - oldsize;
    if (increment > maxincr)
        increment = maxincr;

    size_t newsize = oldsize + increment;
    void *toret;
    if (secret) {
        toret = safemalloc(newsize, eltsize);
        memcpy(toret, ptr, oldsize * eltsize);
        smemclr(ptr, oldsize * eltsize);
        sfree(ptr);
    } else {
        toret = saferealloc(ptr, newsize, eltsize);
    }
    *allocated = newsize;
    return toret;
}
