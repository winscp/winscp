/*
 * CRC32 implementation, as used in SSH-1.
 *
 * This particular form of the CRC uses the polynomial
 * P(x) = x^32+x^26+x^23+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x^1+1
 * and represents polynomials in bit-reversed form, so that the x^0
 * coefficient (constant term) appears in the bit with place value
 * 2^31, and the x^31 coefficient in the bit with place value 2^0. In
 * this representation, (x^32 mod P) = 0xEDB88320, so multiplying the
 * current state by x is done by shifting right by one bit, and XORing
 * that constant into the result if the bit shifted out was 1.
 *
 * There's a bewildering array of subtly different variants of CRC out
 * there, using different polynomials, both bit orders, and varying
 * the start and end conditions. There are catalogue websites such as
 * http://reveng.sourceforge.net/crc-catalogue/ , which generally seem
 * to have the convention of indexing CRCs by their 'check value',
 * defined as whatever you get if you hash the 9-byte test string
 * "123456789".
 *
 * The crc32_rfc1662() function below, which starts off the CRC state
 * at 0xFFFFFFFF and complements it after feeding all the data, gives
 * the check value 0xCBF43926, and matches the hash function that the
 * above catalogue refers to as "CRC-32/ISO-HDLC"; among other things,
 * it's also the "FCS-32" checksum described in RFC 1662 section C.3
 * (hence the name I've given it here).
 *
 * The crc32_ssh1() function implements the variant form used by
 * SSH-1, which uses the same update function, but starts the state at
 * zero and doesn't complement it at the end of the computation. The
 * check value for that version is 0x2DFD2D88, which that CRC
 * catalogue doesn't list at all.
 */

#include <stdint.h>
#include <stdlib.h>

#include "ssh.h"

/*
 * Multiply a CRC value by x^4. This implementation strategy avoids
 * using a lookup table (which would be a side-channel hazard, since
 * SSH-1 applies this CRC to decrypted session data).
 *
 * The basic idea is that you'd like to "multiply" the shifted-out 4
 * bits by the CRC polynomial value 0xEDB88320, or rather by that
 * value shifted right 3 bits (since you want the _last_ bit shifted
 * out, i.e. the one originally at the 2^3 position, to generate
 * 0xEDB88320 itself). But the scare-quoted "multiply" would have to
 * be a multiplication of polynomials over GF(2), which differs from
 * integer multiplication in that you don't have any carries. In other
 * words, you make a copy of one input shifted left by the index of
 * each set bit in the other, so that adding them all together would
 * give you the ordinary integer product, and then you XOR them
 * together instead.
 *
 * With a 4-bit multiplier, the two kinds of multiplication coincide
 * provided the multiplicand has no two set bits at positions
 * differing by less than 4, because then no two copies of the
 * multiplier can overlap to generate a carry. So I break up the
 * intended multiplicand K = 0xEDB88320 >> 3 into three sub-constants
 * a,b,c with that property, such that a^b^c = K. Then I can multiply
 * m by each of them separately, and XOR together the results.
 */
static inline uint32_t crc32_shift_4(uint32_t v)
{
    const uint32_t a = 0x11111044, b = 0x08840020, c = 0x04220000;
    uint32_t m = v & 0xF;
    return (v >> 4) ^ (a*m) ^ (b*m) ^ (c*m);
}

/*
 * The 8-bit shift you need every time you absorb an input byte,
 * implemented simply by iterating the 4-bit shift twice.
 */
static inline uint32_t crc32_shift_8(uint32_t v)
{
    return crc32_shift_4(crc32_shift_4(v));
}

/*
 * Update an existing hash value with extra bytes of data.
 */
uint32_t crc32_update(uint32_t crc, ptrlen data)
{
    const uint8_t *p = (const uint8_t *)data.ptr;
    for (size_t len = data.len; len-- > 0 ;)
        crc = crc32_shift_8(crc ^ *p++);
    return crc;
}

/*
 * The SSH-1 variant of CRC-32.
 */
uint32_t crc32_ssh1(ptrlen data)
{
    return crc32_update(0, data);
}

/*
 * The official version of CRC-32. Nothing in PuTTY proper uses this,
 * but it's useful to expose it to testcrypt so that we can implement
 * standard test vectors.
 */
uint32_t crc32_rfc1662(ptrlen data)
{
    return crc32_update(0xFFFFFFFF, data) ^ 0xFFFFFFFF;
}
