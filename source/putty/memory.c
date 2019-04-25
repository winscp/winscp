/*
 * PuTTY's memory allocation wrappers.
 */

#include <stdlib.h>

#include "putty.h"

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

    if (!p) {
	char str[200];
#ifdef MALLOC_LOG
	sprintf(str, "Out of memory! (%s:%d, size=%d)",
		mlog_file, mlog_line, size);
	fprintf(fp, "*** %s\n", str);
	fclose(fp);
#else
	strcpy(str, "Out of memory!");
#endif
	modalfatalbox("%s", str);
    }
#ifdef MALLOC_LOG
    if (fp)
	fprintf(fp, "malloc(%d) returns %p\n", size, p);
#endif
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

    if (!p) {
	char str[200];
#ifdef MALLOC_LOG
	sprintf(str, "Out of memory! (%s:%d, size=%d)",
		mlog_file, mlog_line, size);
	fprintf(fp, "*** %s\n", str);
	fclose(fp);
#else
	strcpy(str, "Out of memory!");
#endif
	modalfatalbox("%s", str);
    }
#ifdef MALLOC_LOG
    if (fp)
	fprintf(fp, "realloc(%p,%d) returns %p\n", ptr, size, p);
#endif
    return p;
}

void safefree(void *ptr)
{
    if (ptr) {
#ifdef MALLOC_LOG
	if (fp)
	    fprintf(fp, "free(%p)\n", ptr);
#endif
#ifdef MINEFIELD
	minefield_c_free(ptr);
#else
	free(ptr);
#endif
    }
#ifdef MALLOC_LOG
    else if (fp)
	fprintf(fp, "freeing null pointer - no action taken\n");
#endif
}

