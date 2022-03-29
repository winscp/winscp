/*
 * Do an sprintf(), but into a custom-allocated buffer.
 *
 * Currently I'm doing this via vsnprintf. This has worked so far,
 * but it's not good, because vsnprintf is not available on all
 * platforms. There's an ifdef to use `_vsnprintf', which seems
 * to be the local name for it on Windows. Other platforms may
 * lack it completely, in which case it'll be time to rewrite
 * this function in a totally different way.
 *
 * The only `properly' portable solution I can think of is to
 * implement my own format string scanner, which figures out an
 * upper bound for the length of each formatting directive,
 * allocates the buffer as it goes along, and calls sprintf() to
 * actually process each directive. If I ever need to actually do
 * this, some caveats:
 *
 *  - It's very hard to find a reliable upper bound for
 *    floating-point values. %f, in particular, when supplied with
 *    a number near to the upper or lower limit of representable
 *    numbers, could easily take several hundred characters. It's
 *    probably feasible to predict this statically using the
 *    constants in <float.h>, or even to predict it dynamically by
 *    looking at the exponent of the specific float provided, but
 *    it won't be fun.
 *
 *  - Don't forget to _check_, after calling sprintf, that it's
 *    used at most the amount of space we had available.
 *
 *  - Fault any formatting directive we don't fully understand. The
 *    aim here is to _guarantee_ that we never overflow the buffer,
 *    because this is a security-critical function. If we see a
 *    directive we don't know about, we should panic and die rather
 *    than run any risk.
 */

#include <stdio.h>

#include "defs.h"
#include "misc.h"
#include "utils/utils.h"

/* Work around lack of va_copy in old MSC */
#if defined _MSC_VER && !defined va_copy
#define va_copy(a, b) TYPECHECK(                        \
        (va_list *)0 == &(a) && (va_list *)0 == &(b),   \
        memcpy(&a, &b, sizeof(va_list)))
#endif

/* Also lack of vsnprintf before VS2015 */
#if defined _WINDOWS && \
    !defined __MINGW32__ && \
    !defined __WINE__ && \
    _MSC_VER < 1900
#define vsnprintf _vsnprintf
#endif

char *dupvprintf_inner(char *buf, size_t oldlen, size_t *sizeptr,
                       const char *fmt, va_list ap)
{
    size_t size = *sizeptr;
    sgrowarrayn_nm(buf, size, oldlen, 512);

    while (1) {
        va_list aq;
        va_copy(aq, ap);
        int len = vsnprintf(buf + oldlen, size - oldlen, fmt, aq);
        va_end(aq);

        if (len >= 0 && len < size) {
            /* This is the C99-specified criterion for snprintf to have
             * been completely successful. */
            *sizeptr = size;
            return buf;
        } else if (len > 0) {
            /* This is the C99 error condition: the returned length is
             * the required buffer size not counting the NUL. */
            sgrowarrayn_nm(buf, size, oldlen + 1, len);
        } else {
            /* This is the pre-C99 glibc error condition: <0 means the
             * buffer wasn't big enough, so we enlarge it a bit and hope. */
            sgrowarray_nm(buf, size, size);
        }
    }
}

char *dupvprintf(const char *fmt, va_list ap)
{
    size_t size = 0;
    return dupvprintf_inner(NULL, 0, &size, fmt, ap);
}
char *dupprintf(const char *fmt, ...)
{
    char *ret;
    va_list ap;
    va_start(ap, fmt);
    ret = dupvprintf(fmt, ap);
    va_end(ap);
    return ret;
}
