/*
 * Handling of the int64 and uint64 types. Done in 32-bit integers,
 * for (pre-C99) portability. Hopefully once C99 becomes widespread
 * we can kiss this lot goodbye...
 */

#include <assert.h>
#include <string.h>

#include "int64.h"

uint64 uint64_div10(uint64 x, int *remainder)
{
    uint64 y;
    int rem, r2;
    y.hi = x.hi / 10;
    y.lo = x.lo / 10;
    rem = x.lo % 10;
    /*
     * Now we have to add in the remainder left over from x.hi.
     */
    r2 = x.hi % 10;
    y.lo += r2 * 2 * (0x80000000 / 10);
    rem += r2 * 2 * (0x80000000 % 10);
    y.lo += rem / 10;
    rem %= 10;

    if (remainder)
	*remainder = rem;
    return y;
}

void uint64_decimal(uint64 x, char *buffer)
{
    char buf[20];
    int start = 20;
    int d;

    while (x.hi || x.lo) {
	x = uint64_div10(x, &d);
	assert(start > 0);
	buf[--start] = d + '0';
    }

    memcpy(buffer, buf + start, sizeof(buf) - start);
    buffer[sizeof(buf) - start] = '\0';
}

uint64 uint64_make(unsigned long hi, unsigned long lo)
{
    uint64 y;
    y.hi = hi;
    y.lo = lo;
    return y;
}

uint64 uint64_add(uint64 x, uint64 y)
{
    x.lo += y.lo;
    x.hi += y.hi + (x.lo < y.lo ? 1 : 0);
    return x;
}

uint64 uint64_add32(uint64 x, unsigned long y)
{
    uint64 yy;
    yy.hi = 0;
    yy.lo = y;
    return uint64_add(x, yy);
}

int uint64_compare(uint64 x, uint64 y)
{
    if (x.hi != y.hi)
	return x.hi < y.hi ? -1 : +1;
    if (x.lo != y.lo)
	return x.lo < y.lo ? -1 : +1;
    return 0;
}
