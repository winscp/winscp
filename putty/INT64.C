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

    do {
	x = uint64_div10(x, &d);
	assert(start > 0);
	buf[--start] = d + '0';
    } while (x.hi || x.lo);

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

uint64 uint64_subtract(uint64 x, uint64 y)
{
    x.lo -= y.lo;
    x.hi -= y.hi + (x.lo > ~y.lo ? 1 : 0);
    return x;
}

double uint64_to_double(uint64 x)
{
    return (4294967296.0 * x.hi) + (double)x.lo;
}

uint64 uint64_shift_right(uint64 x, int shift)
{
    if (shift < 32) {
	x.lo >>= shift;
	x.lo |= (x.hi << (32-shift));
	x.hi >>= shift;
    } else {
	x.lo = x.hi >> (shift-32);
	x.hi = 0;
    }
    return x;
}

uint64 uint64_shift_left(uint64 x, int shift)
{
    if (shift < 32) {
	x.hi <<= shift;
	x.hi |= (x.lo >> (32-shift));
	x.lo <<= shift;
    } else {
	x.hi = x.lo << (shift-32);
	x.lo = 0;
    }
    return x;
}

uint64 uint64_from_decimal(char *str)
{
    uint64 ret;
    ret.hi = ret.lo = 0;
    while (*str >= '0' && *str <= '9') {
	ret = uint64_add(uint64_shift_left(ret, 3),
			 uint64_shift_left(ret, 1));
	ret = uint64_add32(ret, *str - '0');
	str++;
    }
    return ret;
}
