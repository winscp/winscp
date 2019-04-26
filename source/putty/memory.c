/*
 * PuTTY's memory allocation wrappers.
 */

#include <stdlib.h>
#include <limits.h>

#include "defs.h"
#include "puttymem.h"

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
