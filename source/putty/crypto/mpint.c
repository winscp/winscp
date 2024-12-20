/*
 * Multiprecision integer arithmetic, implementing mpint.h.
 */

#include <assert.h>
#include <limits.h>
#include <stdio.h>

#include "defs.h"
#include "misc.h"
#include "puttymem.h"

#include "mpint.h"
#include "mpint_i.h"

// for WINSCP_PUTTY_SECTION_*
#include "putty.h"

#pragma warn -ngu // WINSCP

#define SIZE_T_BITS (CHAR_BIT * sizeof(size_t))

/*
 * Inline helpers to take min and max of size_t values, used
 * throughout this code.
 */
static inline size_t size_t_min(size_t a, size_t b)
{
    return a < b ? a : b;
}
static inline size_t size_t_max(size_t a, size_t b)
{
    return a > b ? a : b;
}

/*
 * Helper to fetch a word of data from x with array overflow checking.
 * If x is too short to have that word, 0 is returned.
 */
static inline BignumInt mp_word(mp_int *x, size_t i)
{
    return i < x->nw ? x->w[i] : 0;
}

/*
 * Shift an ordinary C integer by BIGNUM_INT_BITS, in a way that
 * avoids writing a shift operator whose RHS is greater or equal to
 * the size of the type, because that's undefined behaviour in C.
 *
 * In fact we must avoid even writing it in a definitely-untaken
 * branch of an if, because compilers will sometimes warn about
 * that. So you can't just write 'shift too big ? 0 : n >> shift',
 * because even if 'shift too big' is a constant-expression
 * evaluating to false, you can still get complaints about the
 * else clause of the ?:.
 *
 * So we have to re-check _inside_ that clause, so that the shift
 * count is reset to something nonsensical but safe in the case
 * where the clause wasn't going to be taken anyway.
 */
static uintmax_t shift_right_by_one_word(uintmax_t n)
{
    bool shift_too_big = BIGNUM_INT_BYTES >= sizeof(n);
    return shift_too_big ? 0 :
        n >> (shift_too_big ? 0 : BIGNUM_INT_BITS);
}
static uintmax_t shift_left_by_one_word(uintmax_t n)
{
    bool shift_too_big = BIGNUM_INT_BYTES >= sizeof(n);
    return shift_too_big ? 0 :
        n << (shift_too_big ? 0 : BIGNUM_INT_BITS);
}

mp_int *mp_make_sized(size_t nw)
{
    mp_int *x = snew_plus(mp_int, nw * sizeof(BignumInt));
    assert(nw);                   /* we outlaw the zero-word mp_int */
    x->nw = nw;
    x->w = snew_plus_get_aux(x);
    mp_clear(x);
    return x;
}

mp_int *mp_new(size_t maxbits)
{
    size_t words = (maxbits + BIGNUM_INT_BITS - 1) / BIGNUM_INT_BITS;
    return mp_make_sized(words);
}

mp_int *mp_resize(mp_int *mp, size_t newmaxbits)
{
    mp_int *copy = mp_new(newmaxbits);
    mp_copy_into(copy, mp);
    mp_free(mp);
    return copy;
}

mp_int *mp_from_integer(uintmax_t n)
{
    mp_int *x = mp_make_sized(
        (sizeof(n) + BIGNUM_INT_BYTES - 1) / BIGNUM_INT_BYTES);
    size_t i; // WINSCP
    for (i = 0; i < x->nw; i++)
        x->w[i] = n >> (i * BIGNUM_INT_BITS);
    return x;
}

size_t mp_max_bytes(mp_int *x)
{
    return x->nw * BIGNUM_INT_BYTES;
}

size_t mp_max_bits(mp_int *x)
{
    return x->nw * BIGNUM_INT_BITS;
}

void mp_free(mp_int *x)
{
    mp_clear(x);
    smemclr(x, sizeof(*x));
    sfree(x);
}

void mp_dump(FILE *fp, const char *prefix, mp_int *x, const char *suffix)
{
    size_t i; // WINSCP
    fprintf(fp, "%s0x", prefix);
    for (i = mp_max_bytes(x); i-- > 0 ;)
        fprintf(fp, "%02X", mp_get_byte(x, i));
    fputs(suffix, fp);
}

void mp_copy_into(mp_int *dest, mp_int *src)
{
    size_t copy_nw = size_t_min(dest->nw, src->nw);
    memmove(dest->w, src->w, copy_nw * sizeof(BignumInt));
    smemclr(dest->w + copy_nw, (dest->nw - copy_nw) * sizeof(BignumInt));
}

void mp_copy_integer_into(mp_int *r, uintmax_t n)
{
    size_t i; // WINSCP
    for (i = 0; i < r->nw; i++) {
        r->w[i] = n;
        n = shift_right_by_one_word(n);
    }
}

/*
 * Conditional selection is done by negating 'which', to give a mask
 * word which is all 1s if which==1 and all 0s if which==0. Then you
 * can select between two inputs a,b without data-dependent control
 * flow by XORing them to get their difference; ANDing with the mask
 * word to replace that difference with 0 if which==0; and XORing that
 * into a, which will either turn it into b or leave it alone.
 *
 * This trick will be used throughout this code and taken as read the
 * rest of the time (or else I'd be here all week typing comments),
 * but I felt I ought to explain it in words _once_.
 */
void mp_select_into(mp_int *dest, mp_int *src0, mp_int *src1,
                    unsigned which)
{
    BignumInt mask = -(BignumInt)(1 & which);
    size_t i; // WINSCP
    for (i = 0; i < dest->nw; i++) {
        BignumInt srcword0 = mp_word(src0, i), srcword1 = mp_word(src1, i);
        dest->w[i] = srcword0 ^ ((srcword1 ^ srcword0) & mask);
    }
}

void mp_cond_swap(mp_int *x0, mp_int *x1, unsigned swap)
{
    pinitassert(x0->nw == x1->nw);
    volatile BignumInt mask = -(BignumInt)(1 & swap);
    size_t i; // WINSCP
    for (i = 0; i < x0->nw; i++) {
        BignumInt diff = (x0->w[i] ^ x1->w[i]) & mask;
        x0->w[i] ^= diff;
        x1->w[i] ^= diff;
    }
}

void mp_clear(mp_int *x)
{
    smemclr(x->w, x->nw * sizeof(BignumInt));
}

void mp_cond_clear(mp_int *x, unsigned clear)
{
    BignumInt mask = ~-(BignumInt)(1 & clear);
    size_t i; // WINSCP
    for (i = 0; i < x->nw; i++)
        x->w[i] &= mask;
}

/*
 * Common code between mp_from_bytes_{le,be} which reads bytes in an
 * arbitrary arithmetic progression.
 */
static mp_int *mp_from_bytes_int(ptrlen bytes, size_t m, size_t c)
{
    size_t nw = (bytes.len + BIGNUM_INT_BYTES - 1) / BIGNUM_INT_BYTES;
    nw = size_t_max(nw, 1);
    { // WINSCP
    mp_int *n = mp_make_sized(nw);
    size_t i; // WINSCP
    for (i = 0; i < bytes.len; i++)
        n->w[i / BIGNUM_INT_BYTES] |=
            (BignumInt)(((const unsigned char *)bytes.ptr)[m*i+c]) <<
            (8 * (i % BIGNUM_INT_BYTES));
    return n;
    } // WINSCP
}

mp_int *mp_from_bytes_le(ptrlen bytes)
{
    return mp_from_bytes_int(bytes, 1, 0);
}

mp_int *mp_from_bytes_be(ptrlen bytes)
{
    return mp_from_bytes_int(bytes, -1, bytes.len - 1);
}

static mp_int *mp_from_words(size_t nw, const BignumInt *w)
{
    mp_int *x = mp_make_sized(nw);
    memcpy(x->w, w, x->nw * sizeof(BignumInt));
    return x;
}

/*
 * Decimal-to-binary conversion: just go through the input string
 * adding on the decimal value of each digit, and then multiplying the
 * number so far by 10.
 */
mp_int *mp_from_decimal_pl(ptrlen decimal)
{
    /* 196/59 is an upper bound (and also a continued-fraction
     * convergent) for log2(10), so this conservatively estimates the
     * number of bits that will be needed to store any number that can
     * be written in this many decimal digits. */
    pinitassert(decimal.len < (~(size_t)0) / 196);
    size_t bits = 196 * decimal.len / 59;

    /* Now round that up to words. */
    size_t words = bits / BIGNUM_INT_BITS + 1;

    mp_int *x = mp_make_sized(words);
    size_t i; // WINSCP
    for (i = 0; i < decimal.len; i++) {
        mp_add_integer_into(x, x, ((const char *)decimal.ptr)[i] - '0');

        if (i+1 == decimal.len)
            break;

        mp_mul_integer_into(x, x, 10);
    }
    return x;
}

mp_int *mp_from_decimal(const char *decimal)
{
    return mp_from_decimal_pl(ptrlen_from_asciz(decimal));
}

/*
 * Hex-to-binary conversion: _algorithmically_ simpler than decimal
 * (none of those multiplications by 10), but there's some fiddly
 * bit-twiddling needed to process each hex digit without diverging
 * control flow depending on whether it's a letter or a number.
 */
mp_int *mp_from_hex_pl(ptrlen hex)
{
    pinitassert(hex.len <= (~(size_t)0) / 4);
    size_t bits = hex.len * 4;
    size_t words = (bits + BIGNUM_INT_BITS - 1) / BIGNUM_INT_BITS;
    words = size_t_max(words, 1);
    { // WINSCP
    mp_int *x = mp_make_sized(words);
    size_t nibble; // WINSCP
    for (nibble = 0; nibble < hex.len; nibble++) {
        BignumInt digit = ((const char *)hex.ptr)[hex.len-1 - nibble];

        BignumInt lmask = ~-((BignumInt)((digit-'a')|('f'-digit))
                             >> (BIGNUM_INT_BITS-1));
        BignumInt umask = ~-((BignumInt)((digit-'A')|('F'-digit))
                             >> (BIGNUM_INT_BITS-1));

        BignumInt digitval = digit - '0';
        digitval ^= (digitval ^ (digit - 'a' + 10)) & lmask;
        digitval ^= (digitval ^ (digit - 'A' + 10)) & umask;
        digitval &= 0xF; /* at least be _slightly_ nice about weird input */

        { // WINSCP
        size_t word_idx = nibble / (BIGNUM_INT_BYTES*2);
        size_t nibble_within_word = nibble % (BIGNUM_INT_BYTES*2);
        x->w[word_idx] |= digitval << (nibble_within_word * 4);
        } // WINSCP
    }
    return x;
    } // WINSCP
}

mp_int *mp_from_hex(const char *hex)
{
    return mp_from_hex_pl(ptrlen_from_asciz(hex));
}

mp_int *mp_copy(mp_int *x)
{
    return mp_from_words(x->nw, x->w);
}

uint8_t mp_get_byte(mp_int *x, size_t byte)
{
    return 0xFF & (mp_word(x, byte / BIGNUM_INT_BYTES) >>
                   (8 * (byte % BIGNUM_INT_BYTES)));
}

unsigned mp_get_bit(mp_int *x, size_t bit)
{
    return 1 & (mp_word(x, bit / BIGNUM_INT_BITS) >>
                (bit % BIGNUM_INT_BITS));
}

uintmax_t mp_get_integer(mp_int *x)
{
    uintmax_t toret = 0;
    size_t i; // WINSCP 
    for (i = x->nw; i-- > 0 ;)
        toret = shift_left_by_one_word(toret) | x->w[i];
    return toret;
}

void mp_set_bit(mp_int *x, size_t bit, unsigned val)
{
    size_t word = bit / BIGNUM_INT_BITS;
    pinitassert(word < x->nw);

    unsigned shift = (bit % BIGNUM_INT_BITS);

    x->w[word] &= ~((BignumInt)1 << shift);
    x->w[word] |= (BignumInt)(val & 1) << shift;
}

/*
 * Helper function used here and there to normalise any nonzero input
 * value to 1.
 */
static inline unsigned normalise_to_1(BignumInt n)
{
    n = (n >> 1) | (n & 1);            /* ensure top bit is clear */
    n = (BignumInt)(-n) >> (BIGNUM_INT_BITS - 1); /* normalise to 0 or 1 */
    return n;
}
static inline unsigned normalise_to_1_u64(uint64_t n)
{
    n = (n >> 1) | (n & 1);            /* ensure top bit is clear */
    n = (-n) >> 63;                    /* normalise to 0 or 1 */
    return n;
}

/*
 * Find the highest nonzero word in a number. Returns the index of the
 * word in x->w, and also a pair of output uint64_t in which that word
 * appears in the high one shifted left by 'shift_wanted' bits, the
 * words immediately below it occupy the space to the right, and the
 * words below _that_ fill up the low one.
 *
 * If there is no nonzero word at all, the passed-by-reference output
 * variables retain their original values.
 */
static inline void mp_find_highest_nonzero_word_pair(
    mp_int *x, size_t shift_wanted, size_t *index,
    uint64_t *hi, uint64_t *lo)
{
    uint64_t curr_hi = 0, curr_lo = 0;

    size_t curr_index; // WINSCP
    for (curr_index = 0; curr_index < x->nw; curr_index++) {
        BignumInt curr_word = x->w[curr_index];
        unsigned indicator = normalise_to_1(curr_word);

        curr_lo = (BIGNUM_INT_BITS < 64 ? (curr_lo >> BIGNUM_INT_BITS) : 0) |
            (curr_hi << (64 - BIGNUM_INT_BITS));
        curr_hi = (BIGNUM_INT_BITS < 64 ? (curr_hi >> BIGNUM_INT_BITS) : 0) |
            ((uint64_t)curr_word << shift_wanted);

        if (hi)    *hi    ^= (curr_hi    ^ *hi   ) & -(uint64_t)indicator;
        if (lo)    *lo    ^= (curr_lo    ^ *lo   ) & -(uint64_t)indicator;
        if (index) *index ^= (curr_index ^ *index) & -(size_t)  indicator;
    }
}

size_t mp_get_nbits(mp_int *x)
{
    /* Sentinel values in case there are no bits set at all: we
     * imagine that there's a word at position -1 (i.e. the topmost
     * fraction word) which is all 1s, because that way, we handle a
     * zero input by considering its highest set bit to be the top one
     * of that word, i.e. just below the units digit, i.e. at bit
     * index -1, i.e. so we'll return 0 on output. */
    size_t hiword_index = -(size_t)1;
    uint64_t hiword64 = ~(BignumInt)0;

    /*
     * Find the highest nonzero word and its index.
     */
    mp_find_highest_nonzero_word_pair(x, 0, &hiword_index, &hiword64, NULL);
    { // WINSCP
    BignumInt hiword = hiword64; /* in case BignumInt is a narrower type */

    /*
     * Find the index of the highest set bit within hiword.
     */
    BignumInt hibit_index = 0;
    size_t i; // WINSCP
    for (i = (1 << (BIGNUM_INT_BITS_BITS-1)); i != 0; i >>= 1) {
        BignumInt shifted_word = hiword >> i;
        BignumInt indicator =
            (BignumInt)(-shifted_word) >> (BIGNUM_INT_BITS-1);
        hiword ^= (shifted_word ^ hiword ) & -indicator;
        hibit_index += i & -(size_t)indicator;
    }

    /*
     * Put together the result.
     */
    return (hiword_index << BIGNUM_INT_BITS_BITS) + hibit_index + 1;
    } // WINSCP
}

/*
 * Shared code between the hex and decimal output functions to get rid
 * of leading zeroes on the output string. The idea is that we wrote
 * out a fixed number of digits and a trailing \0 byte into 'buf', and
 * now we want to shift it all left so that the first nonzero digit
 * moves to buf[0] (or, if there are no nonzero digits at all, we move
 * up by 'maxtrim', so that we return 0 as "0" instead of "").
 */
static void trim_leading_zeroes(char *buf, size_t bufsize, size_t maxtrim)
{
    size_t trim = maxtrim;

    /*
     * Look for the first character not equal to '0', to find the
     * shift count.
     */
    if (trim > 0) {
        size_t pos; // WINSCP
        for (pos = trim; pos-- > 0 ;) {
            uint8_t diff = buf[pos] ^ '0';
            size_t mask = -((((size_t)diff) - 1) >> (SIZE_T_BITS - 1));
            trim ^= (trim ^ pos) & ~mask;
        }
    }

    /*
     * Now do the shift, in log n passes each of which does a
     * conditional shift by 2^i bytes if bit i is set in the shift
     * count.
     */
    { // WINSCP
    uint8_t *ubuf = (uint8_t *)buf;
    size_t logd; // WINSCP
    for (logd = 0; bufsize >> logd; logd++) {
        uint8_t mask = -(uint8_t)((trim >> logd) & 1);
        size_t d = (size_t)1 << logd;
        size_t i; // WINSCP
        for (i = 0; i+d < bufsize; i++) {
            uint8_t diff = mask & (ubuf[i] ^ ubuf[i+d]);
            ubuf[i] ^= diff;
            ubuf[i+d] ^= diff;
        }
    }
    } // WINSCP
}

/*
 * Binary to decimal conversion. Our strategy here is to extract each
 * decimal digit by finding the input number's residue mod 10, then
 * subtract that off to give an exact multiple of 10, which then means
 * you can safely divide by 10 by means of shifting right one bit and
 * then multiplying by the inverse of 5 mod 2^n.
 */
char *mp_get_decimal(mp_int *x_orig)
{
    mp_int *x = mp_copy(x_orig), *y = mp_make_sized(x->nw);

    /*
     * The inverse of 5 mod 2^lots is 0xccccccccccccccccccccd, for an
     * appropriate number of 'c's. Manually construct an integer the
     * right size.
     */
    mp_int *inv5 = mp_make_sized(x->nw);
    pinitassert(BIGNUM_INT_BITS % 8 == 0);
    size_t i; // WINSCP
    for (i = 0; i < inv5->nw; i++)
        inv5->w[i] = BIGNUM_INT_MASK / 5 * 4;
    inv5->w[0]++;

    /*
     * 146/485 is an upper bound (and also a continued-fraction
     * convergent) of log10(2), so this is a conservative estimate of
     * the number of decimal digits needed to store a value that fits
     * in this many binary bits.
     */
    assert(x->nw < (~(size_t)1) / (146 * BIGNUM_INT_BITS));
    { // WINSCP
    size_t bufsize = size_t_max(x->nw * (146 * BIGNUM_INT_BITS) / 485, 1) + 2;
    char *outbuf = snewn(bufsize, char);
    outbuf[bufsize - 1] = '\0';

    /*
     * Loop over the number generating digits from the least
     * significant upwards, so that we write to outbuf in reverse
     * order.
     */
    { // WINSCP
    size_t pos; // WINSCP
    for (pos = bufsize - 1; pos-- > 0 ;) {
        /*
         * Find the current residue mod 10. We do this by first
         * summing the bytes of the number, with all but the lowest
         * one multiplied by 6 (because 256^i == 6 mod 10 for all
         * i>0). That gives us a single word congruent mod 10 to the
         * input number, and then we reduce it further by manual
         * multiplication and shifting, just in case the compiler
         * target implements the C division operator in a way that has
         * input-dependent timing.
         */
        uint32_t low_digit = 0, maxval = 0, mult = 1;
        size_t i; // WINSCP
        for (i = 0; i < x->nw; i++) {
            unsigned j; // WINSCP
            for (j = 0; j < BIGNUM_INT_BYTES; j++) {
                low_digit += mult * (0xFF & (x->w[i] >> (8*j)));
                maxval += mult * 0xFF;
                mult = 6;
            }
            /*
             * For _really_ big numbers, prevent overflow of t by
             * periodically folding the top half of the accumulator
             * into the bottom half, using the same rule 'multiply by
             * 6 when shifting down by one or more whole bytes'.
             */
            if (maxval > UINT32_MAX - (6 * 0xFF * BIGNUM_INT_BYTES)) {
                low_digit = (low_digit & 0xFFFF) + 6 * (low_digit >> 16);
                maxval = (maxval & 0xFFFF) + 6 * (maxval >> 16);
            }
        }

        /*
         * Final reduction of low_digit. We multiply by 2^32 / 10
         * (that's the constant 0x19999999) to get a 64-bit value
         * whose top 32 bits are the approximate quotient
         * low_digit/10; then we subtract off 10 times that; and
         * finally we do one last trial subtraction of 10 by adding 6
         * (which sets bit 4 if the number was just over 10) and then
         * testing bit 4.
         */
        low_digit -= 10 * ((0x19999999ULL * low_digit) >> 32);
        low_digit -= 10 * ((low_digit + 6) >> 4);

        assert(low_digit < 10);        /* make sure we did reduce fully */
        outbuf[pos] = '0' + low_digit;

        /*
         * Now subtract off that digit, divide by 2 (using a right
         * shift) and by 5 (using the modular inverse), to get the
         * next output digit into the units position.
         */
        mp_sub_integer_into(x, x, low_digit);
        mp_rshift_fixed_into(y, x, 1);
        mp_mul_into(x, y, inv5);
    }

    mp_free(x);
    mp_free(y);
    mp_free(inv5);

    trim_leading_zeroes(outbuf, bufsize, bufsize - 2);
    return outbuf;
    } // WINSCP
    } // WINSCP
}

/*
 * Binary to hex conversion. Reasonably simple (only a spot of bit
 * twiddling to choose whether to output a digit or a letter for each
 * nibble).
 */
static char *mp_get_hex_internal(mp_int *x, uint8_t letter_offset)
{
    size_t nibbles = x->nw * BIGNUM_INT_BYTES * 2;
    size_t bufsize = nibbles + 1;
    char *outbuf = snewn(bufsize, char);
    size_t nibble; // WINSCP
    outbuf[nibbles] = '\0';

    for (nibble = 0; nibble < nibbles; nibble++) {
        size_t word_idx = nibble / (BIGNUM_INT_BYTES*2);
        size_t nibble_within_word = nibble % (BIGNUM_INT_BYTES*2);
        uint8_t digitval = 0xF & (x->w[word_idx] >> (nibble_within_word * 4));

        uint8_t mask = -((digitval + 6) >> 4);
        char digit = digitval + '0' + (letter_offset & mask);
        outbuf[nibbles-1 - nibble] = digit;
    }

    trim_leading_zeroes(outbuf, bufsize, nibbles - 1);
    return outbuf;
}

char *mp_get_hex(mp_int *x)
{
    return mp_get_hex_internal(x, 'a' - ('0'+10));
}

char *mp_get_hex_uppercase(mp_int *x)
{
    return mp_get_hex_internal(x, 'A' - ('0'+10));
}

/*
 * Routines for reading and writing the SSH-1 and SSH-2 wire formats
 * for multiprecision integers, declared in marshal.h.
 *
 * These can't avoid having control flow dependent on the true bit
 * size of the number, because the wire format requires the number of
 * output bytes to depend on that.
 */
void BinarySink_put_mp_ssh1(BinarySink *bs, mp_int *x)
{
    size_t bits = mp_get_nbits(x);
    size_t bytes = (bits + 7) / 8;

    size_t i; // WINSCP
    assert(bits < 0x10000);
    put_uint16(bs, bits);
    for (i = bytes; i-- > 0 ;)
        put_byte(bs, mp_get_byte(x, i));
}

void BinarySink_put_mp_ssh2(BinarySink *bs, mp_int *x)
{
    size_t bytes = (mp_get_nbits(x) + 8) / 8;

    size_t i; // WINSCP
    put_uint32(bs, bytes);
    for (i = bytes; i-- > 0 ;)
        put_byte(bs, mp_get_byte(x, i));
}

mp_int *BinarySource_get_mp_ssh1(BinarySource *src)
{
    unsigned bitc = get_uint16(src);
    ptrlen bytes = get_data(src, (bitc + 7) / 8);
    if (get_err(src)) {
        return mp_from_integer(0);
    } else {
        mp_int *toret = mp_from_bytes_be(bytes);
        /* SSH-1.5 spec says that it's OK for the prefix uint16 to be
         * _greater_ than the actual number of bits */
        if (mp_get_nbits(toret) > bitc) {
            src->err = BSE_INVALID;
            mp_free(toret);
            toret = mp_from_integer(0);
        }
        return toret;
    }
}

mp_int *BinarySource_get_mp_ssh2(BinarySource *src)
{
    ptrlen bytes = get_string(src);
    if (get_err(src)) {
        return mp_from_integer(0);
    } else {
        const unsigned char *p = bytes.ptr;
        if ((bytes.len > 0 &&
             ((p[0] & 0x80) ||
              (p[0] == 0 && (bytes.len <= 1 || !(p[1] & 0x80)))))) {
            src->err = BSE_INVALID;
            return mp_from_integer(0);
        }
        return mp_from_bytes_be(bytes);
    }
}

/*
 * Make an mp_int structure whose words array aliases a subinterval of
 * some other mp_int. This makes it easy to read or write just the low
 * or high words of a number, e.g. to add a number starting from a
 * high bit position, or to reduce mod 2^{n*BIGNUM_INT_BITS}.
 *
 * The convention throughout this code is that when we store an mp_int
 * directly by value, we always expect it to be an alias of some kind,
 * so its words array won't ever need freeing. Whereas an 'mp_int *'
 * has an owner, who knows whether it needs freeing or whether it was
 * created by address-taking an alias.
 */
static mp_int mp_make_alias(mp_int *in, size_t offset, size_t len)
{
    /*
     * Bounds-check the offset and length so that we always return
     * something valid, even if it's not necessarily the length the
     * caller asked for.
     */
    if (offset > in->nw)
        offset = in->nw;
    if (len > in->nw - offset)
        len = in->nw - offset;

    { // WINSCP
    mp_int toret;
    toret.nw = len;
    toret.w = in->w + offset;
    return toret;
    } // WINSCP
}

/*
 * A special case of mp_make_alias: in some cases we preallocate a
 * large mp_int to use as scratch space (to avoid pointless
 * malloc/free churn in recursive or iterative work).
 *
 * mp_alloc_from_scratch creates an alias of size 'len' to part of
 * 'pool', and adjusts 'pool' itself so that further allocations won't
 * overwrite that space.
 *
 * There's no free function to go with this. Typically you just copy
 * the pool mp_int by value, allocate from the copy, and when you're
 * done with those allocations, throw the copy away and go back to the
 * original value of pool. (A mark/release system.)
 */
static mp_int mp_alloc_from_scratch(mp_int *pool, size_t len)
{
    pinitassert(len <= pool->nw);
    mp_int toret = mp_make_alias(pool, 0, len);
    *pool = mp_make_alias(pool, len, pool->nw);
    return toret;
}

/*
 * Internal component common to lots of assorted add/subtract code.
 * Reads words from a,b; writes into w_out (which might be NULL if the
 * output isn't even needed). Takes an input carry flag in 'carry',
 * and returns the output carry. Each word read from b is ANDed with
 * b_and and then XORed with b_xor.
 *
 * So you can implement addition by setting b_and to all 1s and b_xor
 * to 0; you can subtract by making b_xor all 1s too (effectively
 * bit-flipping b) and also passing 1 as the input carry (to turn
 * one's complement into two's complement). And you can do conditional
 * add/subtract by choosing b_and to be all 1s or all 0s based on a
 * condition, because the value of b will be totally ignored if b_and
 * == 0.
 */
static BignumCarry mp_add_masked_into(
    BignumInt *w_out, size_t rw, mp_int *a, mp_int *b,
    BignumInt b_and, BignumInt b_xor, BignumCarry carry)
{
    size_t i; // WINSCP
    for (i = 0; i < rw; i++) {
        BignumInt aword = mp_word(a, i), bword = mp_word(b, i), out;
        bword = (bword & b_and) ^ b_xor;
        BignumADC(out, carry, aword, bword, carry);
        if (w_out)
            w_out[i] = out;
    }
    return carry;
}

/*
 * Like the public mp_add_into except that it returns the output carry.
 */
static inline BignumCarry mp_add_into_internal(mp_int *r, mp_int *a, mp_int *b)
{
    return mp_add_masked_into(r->w, r->nw, a, b, ~(BignumInt)0, 0, 0);
}

void mp_add_into(mp_int *r, mp_int *a, mp_int *b)
{
    mp_add_into_internal(r, a, b);
}

void mp_sub_into(mp_int *r, mp_int *a, mp_int *b)
{
    mp_add_masked_into(r->w, r->nw, a, b, ~(BignumInt)0, ~(BignumInt)0, 1);
}

void mp_and_into(mp_int *r, mp_int *a, mp_int *b)
{
    size_t i; // WINSCP
    for (i = 0; i < r->nw; i++) {
        BignumInt aword = mp_word(a, i), bword = mp_word(b, i);
        r->w[i] = aword & bword;
    }
}

void mp_or_into(mp_int *r, mp_int *a, mp_int *b)
{
    size_t i; // WINSCP
    for (i = 0; i < r->nw; i++) {
        BignumInt aword = mp_word(a, i), bword = mp_word(b, i);
        r->w[i] = aword | bword;
    }
}

void mp_xor_into(mp_int *r, mp_int *a, mp_int *b)
{
    size_t i; // WINSCP
    for (i = 0; i < r->nw; i++) {
        BignumInt aword = mp_word(a, i), bword = mp_word(b, i);
        r->w[i] = aword ^ bword;
    }
}

void mp_bic_into(mp_int *r, mp_int *a, mp_int *b)
{
    size_t i; // WINSCP
    for (i = 0; i < r->nw; i++) {
        BignumInt aword = mp_word(a, i), bword = mp_word(b, i);
        r->w[i] = aword & ~bword;
    }
}

static void mp_cond_negate(mp_int *r, mp_int *x, unsigned yes)
{
    BignumCarry carry = yes;
    BignumInt flip = -(BignumInt)yes;
    size_t i; // WINSCP
    for (i = 0; i < r->nw; i++) {
        BignumInt xword = mp_word(x, i);
        xword ^= flip;
        BignumADC(r->w[i], carry, 0, xword, carry);
    }
}

/*
 * Similar to mp_add_masked_into, but takes a C integer instead of an
 * mp_int as the masked operand.
 */
static BignumCarry mp_add_masked_integer_into(
    BignumInt *w_out, size_t rw, mp_int *a, uintmax_t b,
    BignumInt b_and, BignumInt b_xor, BignumCarry carry)
{
    size_t i; // WINSCP
    for (i = 0; i < rw; i++) {
        BignumInt aword = mp_word(a, i);
        BignumInt bword = b;
        b = shift_right_by_one_word(b);
        { // WINSCP
        BignumInt out;
        bword = (bword ^ b_xor) & b_and;
        BignumADC(out, carry, aword, bword, carry);
        if (w_out)
            w_out[i] = out;
        } // WINSCP
    }
    return carry;
}

void mp_add_integer_into(mp_int *r, mp_int *a, uintmax_t n)
{
    mp_add_masked_integer_into(r->w, r->nw, a, n, ~(BignumInt)0, 0, 0);
}

void mp_sub_integer_into(mp_int *r, mp_int *a, uintmax_t n)
{
    mp_add_masked_integer_into(r->w, r->nw, a, n,
                               ~(BignumInt)0, ~(BignumInt)0, 1);
}

/*
 * Sets r to a + n << (word_index * BIGNUM_INT_BITS), treating
 * word_index as secret data.
 */
static void mp_add_integer_into_shifted_by_words(
    mp_int *r, mp_int *a, uintmax_t n, size_t word_index)
{
    unsigned indicator = 0;
    BignumCarry carry = 0;
    size_t i; // WINSCP

    for (i = 0; i < r->nw; i++) {
        /* indicator becomes 1 when we reach the index that the least
         * significant bits of n want to be placed at, and it stays 1
         * thereafter. */
        indicator |= 1 ^ normalise_to_1(i ^ word_index);

        /* If indicator is 1, we add the low bits of n into r, and
         * shift n down. If it's 0, we add zero bits into r, and
         * leave n alone. */
        { // WINSCP
        BignumInt bword = n & -(BignumInt)indicator;
        uintmax_t new_n = shift_right_by_one_word(n);
        n ^= (n ^ new_n) & -(uintmax_t)indicator;

        { // WINSCP
        BignumInt aword = mp_word(a, i);
        BignumInt out;
        BignumADC(out, carry, aword, bword, carry);
        r->w[i] = out;
        } // WINSCP
        } // WINSCP
    }
}

void mp_mul_integer_into(mp_int *r, mp_int *a, uint16_t n)
{
    BignumInt carry = 0, mult = n;
    size_t i; // WINSCP
    for (i = 0; i < r->nw; i++) {
        BignumInt aword = mp_word(a, i);
        BignumMULADD(carry, r->w[i], aword, mult, carry);
    }
    assert(!carry);
}

void mp_cond_add_into(mp_int *r, mp_int *a, mp_int *b, unsigned yes)
{
    BignumInt mask = -(BignumInt)(yes & 1);
    mp_add_masked_into(r->w, r->nw, a, b, mask, 0, 0);
}

void mp_cond_sub_into(mp_int *r, mp_int *a, mp_int *b, unsigned yes)
{
    BignumInt mask = -(BignumInt)(yes & 1);
    mp_add_masked_into(r->w, r->nw, a, b, mask, mask, 1 & mask);
}

/*
 * Ordered comparison between unsigned numbers is done by subtracting
 * one from the other and looking at the output carry.
 */
unsigned mp_cmp_hs(mp_int *a, mp_int *b)
{
    size_t rw = size_t_max(a->nw, b->nw);
    return mp_add_masked_into(NULL, rw, a, b, ~(BignumInt)0, ~(BignumInt)0, 1);
}

unsigned mp_hs_integer(mp_int *x, uintmax_t n)
{
    BignumInt carry = 1;
    size_t nwords = sizeof(n)/BIGNUM_INT_BYTES;
    size_t i, e; // WINSCP
    for (i = 0, e = size_t_max(x->nw, nwords); i < e; i++) {
        BignumInt nword = n;
        n = shift_right_by_one_word(n);
        { // WINSCP
        BignumInt dummy_out;
        BignumADC(dummy_out, carry, mp_word(x, i), ~nword, carry);
        (void)dummy_out;
        } // WINSCP
    }
    return carry;
}

/*
 * Equality comparison is done by bitwise XOR of the input numbers,
 * ORing together all the output words, and normalising the result
 * using our careful normalise_to_1 helper function.
 */
unsigned mp_cmp_eq(mp_int *a, mp_int *b)
{
    BignumInt diff = 0;
    size_t i, limit; // WINSCP
    for (i = 0, limit = size_t_max(a->nw, b->nw); i < limit; i++)
        diff |= mp_word(a, i) ^ mp_word(b, i);
    return 1 ^ normalise_to_1(diff);   /* return 1 if diff _is_ zero */
}

unsigned mp_eq_integer(mp_int *x, uintmax_t n)
{
    BignumInt diff = 0;
    size_t nwords = sizeof(n)/BIGNUM_INT_BYTES;
    size_t i, e; // WINSCP
    for (i = 0, e = size_t_max(x->nw, nwords); i < e; i++) {
        BignumInt nword = n;
        n = shift_right_by_one_word(n);
        diff |= mp_word(x, i) ^ nword;
    }
    return 1 ^ normalise_to_1(diff);   /* return 1 if diff _is_ zero */
}

static void mp_neg_into(mp_int *r, mp_int *a)
{
    mp_int zero;
    zero.nw = 0;
    mp_sub_into(r, &zero, a);
}

mp_int *mp_add(mp_int *x, mp_int *y)
{
    mp_int *r = mp_make_sized(size_t_max(x->nw, y->nw) + 1);
    mp_add_into(r, x, y);
    return r;
}

mp_int *mp_sub(mp_int *x, mp_int *y)
{
    mp_int *r = mp_make_sized(size_t_max(x->nw, y->nw));
    mp_sub_into(r, x, y);
    return r;
}

/*
 * Internal routine: multiply and accumulate in the trivial O(N^2)
 * way. Sets r <- r + a*b.
 */
static void mp_mul_add_simple(mp_int *r, mp_int *a, mp_int *b)
{
    BignumInt *aend = a->w + a->nw, *bend = b->w + b->nw, *rend = r->w + r->nw;

    BignumInt *ap, *rp; // WINSCP
    for (ap = a->w, rp = r->w;
         ap < aend && rp < rend; ap++, rp++) {

        BignumInt adata = *ap, carry = 0, *rq = rp;

        { // WINSCP
        BignumInt *bp; // WINSCP
        for (bp = b->w; bp < bend && rq < rend; bp++, rq++) {
            BignumInt bdata = bp < bend ? *bp : 0;
            BignumMULADD2(carry, *rq, adata, bdata, *rq, carry);
        }
        } // WINSCP

        for (; rq < rend; rq++)
            BignumADC(*rq, carry, carry, *rq, 0);
    }
}

#ifndef KARATSUBA_THRESHOLD      /* allow redefinition via -D for testing */
#define KARATSUBA_THRESHOLD 24
#endif

static inline size_t mp_mul_scratchspace_unary(size_t n)
{
    /*
     * Simplistic and overcautious bound on the amount of scratch
     * space that the recursive multiply function will need.
     *
     * The rationale is: on the main Karatsuba branch of
     * mp_mul_internal, which is the most space-intensive one, we
     * allocate space for (a0+a1) and (b0+b1) (each just over half the
     * input length n) and their product (the sum of those sizes, i.e.
     * just over n itself). Then in order to actually compute the
     * product, we do a recursive multiplication of size just over n.
     *
     * If all those 'just over' weren't there, and everything was
     * _exactly_ half the length, you'd get the amount of space for a
     * size-n multiply defined by the recurrence M(n) = 2n + M(n/2),
     * which is satisfied by M(n) = 4n. But instead it's (2n plus a
     * word or two) and M(n/2 plus a word or two). On the assumption
     * that there's still some constant k such that M(n) <= kn, this
     * gives us kn = 2n + w + k(n/2 + w), where w is a small constant
     * (one or two words). That simplifies to kn/2 = 2n + (k+1)w, and
     * since we don't even _start_ needing scratch space until n is at
     * least 50, we can bound 2n + (k+1)w above by 3n, giving k=6.
     *
     * So I claim that 6n words of scratch space will suffice, and I
     * check that by assertion at every stage of the recursion.
     */
    return n * 6;
}

static size_t mp_mul_scratchspace(size_t rw, size_t aw, size_t bw)
{
    size_t inlen = size_t_min(rw, size_t_max(aw, bw));
    return mp_mul_scratchspace_unary(inlen);
}

static void mp_mul_internal(mp_int *r, mp_int *a, mp_int *b, mp_int scratch)
{
    size_t inlen = size_t_min(r->nw, size_t_max(a->nw, b->nw));
    assert(scratch.nw >= mp_mul_scratchspace_unary(inlen));

    mp_clear(r);

    if (inlen < KARATSUBA_THRESHOLD || a->nw == 0 || b->nw == 0) {
        /*
         * The input numbers are too small to bother optimising. Go
         * straight to the simple primitive approach.
         */
        mp_mul_add_simple(r, a, b);
        return;
    }

    /*
     * Karatsuba divide-and-conquer algorithm. We cut each input in
     * half, so that it's expressed as two big 'digits' in a giant
     * base D:
     *
     *   a = a_1 D + a_0
     *   b = b_1 D + b_0
     *
     * Then the product is of course
     *
     *   ab = a_1 b_1 D^2 + (a_1 b_0 + a_0 b_1) D + a_0 b_0
     *
     * and we compute the three coefficients by recursively calling
     * ourself to do half-length multiplications.
     *
     * The clever bit that makes this worth doing is that we only need
     * _one_ half-length multiplication for the central coefficient
     * rather than the two that it obviouly looks like, because we can
     * use a single multiplication to compute
     *
     *   (a_1 + a_0) (b_1 + b_0) = a_1 b_1 + a_1 b_0 + a_0 b_1 + a_0 b_0
     *
     * and then we subtract the other two coefficients (a_1 b_1 and
     * a_0 b_0) which we were computing anyway.
     *
     * Hence we get to multiply two numbers of length N in about three
     * times as much work as it takes to multiply numbers of length
     * N/2, which is obviously better than the four times as much work
     * it would take if we just did a long conventional multiply.
     */

    { // WINSCP
    /* Break up the input as botlen + toplen, with botlen >= toplen.
     * The 'base' D is equal to 2^{botlen * BIGNUM_INT_BITS}. */
    size_t toplen = inlen / 2;
    size_t botlen = inlen - toplen;

    /* Alias bignums that address the two halves of a,b, and useful
     * pieces of r. */
    mp_int a0 = mp_make_alias(a, 0, botlen);
    mp_int b0 = mp_make_alias(b, 0, botlen);
    mp_int a1 = mp_make_alias(a, botlen, toplen);
    mp_int b1 = mp_make_alias(b, botlen, toplen);
    mp_int r0 = mp_make_alias(r, 0, botlen*2);
    mp_int r1 = mp_make_alias(r, botlen, r->nw);
    mp_int r2 = mp_make_alias(r, botlen*2, r->nw);

    /* Recurse to compute a0*b0 and a1*b1, in their correct positions
     * in the output bignum. They can't overlap. */
    mp_mul_internal(&r0, &a0, &b0, scratch);
    mp_mul_internal(&r2, &a1, &b1, scratch);

    if (r->nw < inlen*2) {
        /*
         * The output buffer isn't large enough to require the whole
         * product, so some of a1*b1 won't have been stored. In that
         * case we won't try to do the full Karatsuba optimisation;
         * we'll just recurse again to compute a0*b1 and a1*b0 - or at
         * least as much of them as the output buffer size requires -
         * and add each one in.
         */
        mp_int s = mp_alloc_from_scratch(
            &scratch, size_t_min(botlen+toplen, r1.nw));

        mp_mul_internal(&s, &a0, &b1, scratch);
        mp_add_into(&r1, &r1, &s);
        mp_mul_internal(&s, &a1, &b0, scratch);
        mp_add_into(&r1, &r1, &s);
        return;
    }

    { // WINSCP
    /* a0+a1 and b0+b1 */
    mp_int asum = mp_alloc_from_scratch(&scratch, botlen+1);
    mp_int bsum = mp_alloc_from_scratch(&scratch, botlen+1);
    mp_add_into(&asum, &a0, &a1);
    mp_add_into(&bsum, &b0, &b1);

    { // WINSCP
    /* Their product */
    mp_int product = mp_alloc_from_scratch(&scratch, botlen*2+1);
    mp_mul_internal(&product, &asum, &bsum, scratch);

    /* Subtract off the outer terms we already have */
    mp_sub_into(&product, &product, &r0);
    mp_sub_into(&product, &product, &r2);

    /* And add it in with the right offset. */
    mp_add_into(&r1, &r1, &product);
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

void mp_mul_into(mp_int *r, mp_int *a, mp_int *b)
{
    mp_int *scratch = mp_make_sized(mp_mul_scratchspace(r->nw, a->nw, b->nw));
    mp_mul_internal(r, a, b, *scratch);
    mp_free(scratch);
}

mp_int *mp_mul(mp_int *x, mp_int *y)
{
    mp_int *r = mp_make_sized(x->nw + y->nw);
    mp_mul_into(r, x, y);
    return r;
}

void mp_lshift_fixed_into(mp_int *r, mp_int *a, size_t bits)
{
    size_t words = bits / BIGNUM_INT_BITS;
    size_t bitoff = bits % BIGNUM_INT_BITS;

    size_t i; // WINSCP
    for (i = r->nw; i-- > 0 ;) {
        if (i < words) {
            r->w[i] = 0;
        } else {
            r->w[i] = mp_word(a, i - words);
            if (bitoff != 0) {
                r->w[i] <<= bitoff;
                if (i > words)
                    r->w[i] |= mp_word(a, i - words - 1) >>
                        (BIGNUM_INT_BITS - bitoff);
            }
        }
    }
}

void mp_rshift_fixed_into(mp_int *r, mp_int *a, size_t bits)
{
    size_t words = bits / BIGNUM_INT_BITS;
    size_t bitoff = bits % BIGNUM_INT_BITS;

    size_t i; // WINSCP
    for (i = 0; i < r->nw; i++) {
        r->w[i] = mp_word(a, i + words);
        if (bitoff != 0) {
            r->w[i] >>= bitoff;
            r->w[i] |= mp_word(a, i + words + 1) << (BIGNUM_INT_BITS - bitoff);
        }
    }
}

mp_int *mp_lshift_fixed(mp_int *x, size_t bits)
{
    size_t words = (bits + BIGNUM_INT_BITS - 1) / BIGNUM_INT_BITS;
    mp_int *r = mp_make_sized(x->nw + words);
    mp_lshift_fixed_into(r, x, bits);
    return r;
}

mp_int *mp_rshift_fixed(mp_int *x, size_t bits)
{
    size_t words = bits / BIGNUM_INT_BITS;
    size_t nw = x->nw - size_t_min(x->nw, words);
    mp_int *r = mp_make_sized(size_t_max(nw, 1));
    mp_rshift_fixed_into(r, x, bits);
    return r;
}

/*
 * Safe right shift is done using the same technique as
 * trim_leading_zeroes above: you make an n-word left shift by
 * composing an appropriate subset of power-of-2-sized shifts, so it
 * takes log_2(n) loop iterations each of which does a different shift
 * by a power of 2 words, using the usual bit twiddling to make the
 * whole shift conditional on the appropriate bit of n.
 */
static void mp_rshift_safe_in_place(mp_int *r, size_t bits)
{
    size_t wordshift = bits / BIGNUM_INT_BITS;
    size_t bitshift = bits % BIGNUM_INT_BITS;

    unsigned bit; // WINSCP
    unsigned clear = (r->nw - wordshift) >> (CHAR_BIT * sizeof(size_t) - 1);
    mp_cond_clear(r, clear);

    for (bit = 0; r->nw >> bit; bit++) {
        size_t word_offset = (size_t)1 << bit;
        BignumInt mask = -(BignumInt)((wordshift >> bit) & 1);
        size_t i; // WINSCP
        for (i = 0; i < r->nw; i++) {
            BignumInt w = mp_word(r, i + word_offset);
            r->w[i] ^= (r->w[i] ^ w) & mask;
        }
    }

    /*
     * That's done the shifting by words; now we do the shifting by
     * bits.
     */
    for (bit = 0; bit < BIGNUM_INT_BITS_BITS; bit++) { // WINSCP
        unsigned shift = 1 << bit, upshift = BIGNUM_INT_BITS - shift;
        BignumInt mask = -(BignumInt)((bitshift >> bit) & 1);
        size_t i; // WINSCP
        for (i = 0; i < r->nw; i++) {
            BignumInt w = ((r->w[i] >> shift) | (mp_word(r, i+1) << upshift));
            r->w[i] ^= (r->w[i] ^ w) & mask;
        }
    }
}

mp_int *mp_rshift_safe(mp_int *x, size_t bits)
{
    mp_int *r = mp_copy(x);
    mp_rshift_safe_in_place(r, bits);
    return r;
}

void mp_rshift_safe_into(mp_int *r, mp_int *x, size_t bits)
{
    mp_copy_into(r, x);
    mp_rshift_safe_in_place(r, bits);
}

static void mp_lshift_safe_in_place(mp_int *r, size_t bits)
{
    size_t wordshift = bits / BIGNUM_INT_BITS;
    size_t bitshift = bits % BIGNUM_INT_BITS;

    /*
     * Same strategy as mp_rshift_safe_in_place, but of course the
     * other way up.
     */

    unsigned clear = (r->nw - wordshift) >> (CHAR_BIT * sizeof(size_t) - 1);
    mp_cond_clear(r, clear);

    { // WINSCP
    unsigned bit; // WINSCP
    for (bit = 0; r->nw >> bit; bit++) {
        size_t word_offset = (size_t)1 << bit;
        BignumInt mask = -(BignumInt)((wordshift >> bit) & 1);
        size_t i; // WINSCP
        for (i = r->nw; i-- > 0 ;) {
            BignumInt w = mp_word(r, i - word_offset);
            r->w[i] ^= (r->w[i] ^ w) & mask;
        }
    }

    { // WINSCP
    size_t downshift = BIGNUM_INT_BITS - bitshift;
    size_t no_shift = (downshift >> BIGNUM_INT_BITS_BITS);
    downshift &= ~-(size_t)no_shift;
    { // WINSCP
    BignumInt downshifted_mask = ~-(BignumInt)no_shift;

    size_t i; // WINSCP
    for (i = r->nw; i-- > 0 ;) {
        r->w[i] = (r->w[i] << bitshift) |
            ((mp_word(r, i-1) >> downshift) & downshifted_mask);
    }
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

void mp_lshift_safe_into(mp_int *r, mp_int *x, size_t bits)
{
    mp_copy_into(r, x);
    mp_lshift_safe_in_place(r, bits);
}

void mp_reduce_mod_2to(mp_int *x, size_t p)
{
    size_t word = p / BIGNUM_INT_BITS;
    size_t mask = ((size_t)1 << (p % BIGNUM_INT_BITS)) - 1;
    for (; word < x->nw; word++) {
        x->w[word] &= mask;
        mask = 0;
    }
}

/*
 * Inverse mod 2^n is computed by an iterative technique which doubles
 * the number of bits at each step.
 */
mp_int *mp_invert_mod_2to(mp_int *x, size_t p)
{
    /* Input checks: x must be coprime to the modulus, i.e. odd, and p
     * can't be zero */
    assert(x->nw > 0);
    assert(x->w[0] & 1);
    assert(p > 0);

    { // WINSCP
    size_t rw = (p + BIGNUM_INT_BITS - 1) / BIGNUM_INT_BITS;
    rw = size_t_max(rw, 1);
    { // WINSCP
    mp_int *r = mp_make_sized(rw);

    size_t mul_scratchsize = mp_mul_scratchspace(2*rw, rw, rw);
    mp_int *scratch_orig = mp_make_sized(6 * rw + mul_scratchsize);
    mp_int scratch_per_iter = *scratch_orig;
    mp_int mul_scratch = mp_alloc_from_scratch(
        &scratch_per_iter, mul_scratchsize);
    size_t b; // WINSCP

    r->w[0] = 1;

    for (b = 1; b < p; b <<= 1) {
        /*
         * In each step of this iteration, we have the inverse of x
         * mod 2^b, and we want the inverse of x mod 2^{2b}.
         *
         * Write B = 2^b for convenience, so we want x^{-1} mod B^2.
         * Let x = x_0 + B x_1 + k B^2, with 0 <= x_0,x_1 < B.
         *
         * We want to find r_0 and r_1 such that
         *    (r_1 B + r_0) (x_1 B + x_0) == 1 (mod B^2)
         *
         * To begin with, we know r_0 must be the inverse mod B of
         * x_0, i.e. of x, i.e. it is the inverse we computed in the
         * previous iteration. So now all we need is r_1.
         *
         * Multiplying out, neglecting multiples of B^2, and writing
         * x_0 r_0 = K B + 1, we have
         *
         *    r_1 x_0 B + r_0 x_1 B + K B == 0                    (mod B^2)
         * =>                   r_1 x_0 B == - r_0 x_1 B - K B    (mod B^2)
         * =>                     r_1 x_0 == - r_0 x_1 - K        (mod B)
         * =>                         r_1 == r_0 (- r_0 x_1 - K)  (mod B)
         *
         * (the last step because we multiply through by the inverse
         * of x_0, which we already know is r_0).
         */

        mp_int scratch_this_iter = scratch_per_iter;
        size_t Bw = (b + BIGNUM_INT_BITS - 1) / BIGNUM_INT_BITS;
        size_t B2w = (2*b + BIGNUM_INT_BITS - 1) / BIGNUM_INT_BITS;

        /* Start by finding K: multiply x_0 by r_0, and shift down. */
        mp_int x0 = mp_alloc_from_scratch(&scratch_this_iter, Bw);
        mp_copy_into(&x0, x);
        mp_reduce_mod_2to(&x0, b);
        { // WINSCP
        mp_int r0 = mp_make_alias(r, 0, Bw);
        mp_int Kshift = mp_alloc_from_scratch(&scratch_this_iter, B2w);
        mp_mul_internal(&Kshift, &x0, &r0, mul_scratch);
        { // WINSCP
        mp_int K = mp_alloc_from_scratch(&scratch_this_iter, Bw);
        mp_rshift_fixed_into(&K, &Kshift, b);

        /* Now compute the product r_0 x_1, reusing the space of Kshift. */
        { // WINSCP
        mp_int x1 = mp_alloc_from_scratch(&scratch_this_iter, Bw);
        mp_rshift_fixed_into(&x1, x, b);
        mp_reduce_mod_2to(&x1, b);
        { // WINSCP
        mp_int r0x1 = mp_make_alias(&Kshift, 0, Bw);
        mp_mul_internal(&r0x1, &r0, &x1, mul_scratch);

        /* Add K to that. */
        mp_add_into(&r0x1, &r0x1, &K);

        /* Negate it. */
        mp_neg_into(&r0x1, &r0x1);

        /* Multiply by r_0. */
        { // WINSCP
        mp_int r1 = mp_alloc_from_scratch(&scratch_this_iter, Bw);
        mp_mul_internal(&r1, &r0, &r0x1, mul_scratch);
        mp_reduce_mod_2to(&r1, b);

        /* That's our r_1, so add it on to r_0 to get the full inverse
         * output from this iteration. */
        mp_lshift_fixed_into(&K, &r1, (b % BIGNUM_INT_BITS));
        { // WINSCP
        size_t Bpos = b / BIGNUM_INT_BITS;
        mp_int r1_position = mp_make_alias(r, Bpos, B2w-Bpos);
        mp_add_into(&r1_position, &r1_position, &K);
        } // WINSCP
        } // WINSCP
        } // WINSCP
        } // WINSCP
        } // WINSCP
        } // WINSCP
    }

    /* Finally, reduce mod the precise desired number of bits. */
    mp_reduce_mod_2to(r, p);

    mp_free(scratch_orig);
    return r;
    } // WINSCP
    } // WINSCP
}

static size_t monty_scratch_size(MontyContext *mc)
{
    return 3*mc->rw + mc->pw + mp_mul_scratchspace(mc->pw, mc->rw, mc->rw);
}

MontyContext *monty_new(mp_int *modulus)
{
    MontyContext *mc = snew(MontyContext);

    mc->rw = modulus->nw;
    mc->rbits = mc->rw * BIGNUM_INT_BITS;
    mc->pw = mc->rw * 2 + 1;

    mc->m = mp_copy(modulus);

    mc->minus_minv_mod_r = mp_invert_mod_2to(mc->m, mc->rbits);
    mp_neg_into(mc->minus_minv_mod_r, mc->minus_minv_mod_r);

    { // WINSCP
    size_t j; // WINSCP
    mp_int *r = mp_make_sized(mc->rw + 1);
    r->w[mc->rw] = 1;
    mc->powers_of_r_mod_m[0] = mp_mod(r, mc->m);
    mp_free(r);

    for (j = 1; j < lenof(mc->powers_of_r_mod_m); j++)
        mc->powers_of_r_mod_m[j] = mp_modmul(
            mc->powers_of_r_mod_m[0], mc->powers_of_r_mod_m[j-1], mc->m);

    mc->winscp_guarded_scratch = mp_make_sized(monty_scratch_size(mc));

    return mc;
    } // WINSCP
}

void monty_free(MontyContext *mc)
{
    size_t j; // WINSCP
    mp_free(mc->m);
    for (j = 0; j < 3; j++)
        mp_free(mc->powers_of_r_mod_m[j]);
    mp_free(mc->minus_minv_mod_r);
    mp_free(mc->winscp_guarded_scratch);
    smemclr(mc, sizeof(*mc));
    sfree(mc);
}

/*
 * The main Montgomery reduction step.
 */
static mp_int monty_reduce_internal(MontyContext *mc, mp_int *x, mp_int scratch)
{
    /*
     * The trick with Montgomery reduction is that on the one hand we
     * want to reduce the size of the input by a factor of about r,
     * and on the other hand, the two numbers we just multiplied were
     * both stored with an extra factor of r multiplied in. So we
     * computed ar*br = ab r^2, but we want to return abr, so we need
     * to divide by r - and if we can do that by _actually dividing_
     * by r then this also reduces the size of the number.
     *
     * But we can only do that if the number we're dividing by r is a
     * multiple of r. So first we must add an adjustment to it which
     * clears its bottom 'rbits' bits. That adjustment must be a
     * multiple of m in order to leave the residue mod n unchanged, so
     * the question is, what multiple of m can we add to x to make it
     * congruent to 0 mod r? And the answer is, x * (-m)^{-1} mod r.
     */

    /* x mod r */
    mp_int x_lo = mp_make_alias(x, 0, mc->rbits);

    /* x * (-m)^{-1}, i.e. the number we want to multiply by m */
    mp_int k = mp_alloc_from_scratch(&scratch, mc->rw);
    mp_mul_internal(&k, &x_lo, mc->minus_minv_mod_r, scratch);

    /* m times that, i.e. the number we want to add to x */
    { // WINSCP
    mp_int mk = mp_alloc_from_scratch(&scratch, mc->pw);
    mp_mul_internal(&mk, mc->m, &k, scratch);

    /* Add it to x */
    mp_add_into(&mk, x, &mk);

    /* Reduce mod r, by simply making an alias to the upper words of x */
    { // WINSCP
    mp_int toret = mp_make_alias(&mk, mc->rw, mk.nw - mc->rw);

    /*
     * We'll generally be doing this after a multiplication of two
     * fully reduced values. So our input could be anything up to m^2,
     * and then we added up to rm to it. Hence, the maximum value is
     * rm+m^2, and after dividing by r, that becomes r + m(m/r) < 2r.
     * So a single trial-subtraction will finish reducing to the
     * interval [0,m).
     */
    mp_cond_sub_into(&toret, &toret, mc->m, mp_cmp_hs(&toret, mc->m));
    return toret;
    } // WINSCP
    } // WINSCP
}

void monty_mul_into(MontyContext *mc, mp_int *r, mp_int *x, mp_int *y)
{
    assert(x->nw <= mc->rw);
    assert(y->nw <= mc->rw);

    WINSCP_PUTTY_SECTION_ENTER;
    { // WINSCP
    mp_int scratch = *mc->winscp_guarded_scratch;
    mp_int tmp = mp_alloc_from_scratch(&scratch, 2*mc->rw);
    mp_mul_into(&tmp, x, y);
    { // WINSCP
    mp_int reduced = monty_reduce_internal(mc, &tmp, scratch);
    mp_copy_into(r, &reduced);
    mp_clear(mc->winscp_guarded_scratch);
    } // WINSCP
    } // WINSCP
    WINSCP_PUTTY_SECTION_LEAVE;
}

mp_int *monty_mul(MontyContext *mc, mp_int *x, mp_int *y)
{
    mp_int *toret = mp_make_sized(mc->rw);
    monty_mul_into(mc, toret, x, y);
    return toret;
}

mp_int *monty_modulus(MontyContext *mc)
{
    return mc->m;
}

mp_int *monty_identity(MontyContext *mc)
{
    return mc->powers_of_r_mod_m[0];
}

mp_int *monty_invert(MontyContext *mc, mp_int *x)
{
    /* Given xr, we want to return x^{-1}r = (xr)^{-1} r^2 =
     * monty_reduce((xr)^{-1} r^3) */
    mp_int *tmp = mp_invert(x, mc->m);
    mp_int *toret = monty_mul(mc, tmp, mc->powers_of_r_mod_m[2]);
    mp_free(tmp);
    return toret;
}

/*
 * Importing a number into Montgomery representation involves
 * multiplying it by r and reducing mod m. We use the general-purpose
 * mp_modmul for this, in case the input number is out of range.
 */
mp_int *monty_import(MontyContext *mc, mp_int *x)
{
    return mp_modmul(x, mc->powers_of_r_mod_m[0], mc->m);
}

void monty_import_into(MontyContext *mc, mp_int *r, mp_int *x)
{
    mp_int *imported = monty_import(mc, x);
    mp_copy_into(r, imported);
    mp_free(imported);
}

/*
 * Exporting a number means multiplying it by r^{-1}, which is exactly
 * what monty_reduce does anyway, so we just do that.
 */
void monty_export_into(MontyContext *mc, mp_int *r, mp_int *x)
{
    assert(x->nw <= 2*mc->rw);
    WINSCP_PUTTY_SECTION_ENTER;
    { // WINSCP
    mp_int reduced = monty_reduce_internal(mc, x, *mc->winscp_guarded_scratch);
    mp_copy_into(r, &reduced);
    mp_clear(mc->winscp_guarded_scratch);
    WINSCP_PUTTY_SECTION_LEAVE;
    } // WINSCP
}

mp_int *monty_export(MontyContext *mc, mp_int *x)
{
    mp_int *toret = mp_make_sized(mc->rw);
    monty_export_into(mc, toret, x);
    return toret;
}

#define MODPOW_LOG2_WINDOW_SIZE 5
#define MODPOW_WINDOW_SIZE (1 << MODPOW_LOG2_WINDOW_SIZE)
mp_int *monty_pow(MontyContext *mc, mp_int *base, mp_int *exponent)
{
    /*
     * Modular exponentiation is done from the top down, using a
     * fixed-window technique.
     *
     * We have a table storing every power of the base from base^0 up
     * to base^{w-1}, where w is a small power of 2, say 2^k. (k is
     * defined above as MODPOW_LOG2_WINDOW_SIZE, and w = 2^k is
     * defined as MODPOW_WINDOW_SIZE.)
     *
     * We break the exponent up into k-bit chunks, from the bottom up,
     * that is
     *
     *   exponent = c_0 + 2^k c_1 + 2^{2k} c_2 + ... + 2^{nk} c_n
     *
     * and we compute base^exponent by computing in turn
     *
     *   base^{c_n}
     *   base^{2^k c_n + c_{n-1}}
     *   base^{2^{2k} c_n + 2^k c_{n-1} + c_{n-2}}
     *   ...
     *
     * where each line is obtained by raising the previous line to the
     * power 2^k (i.e. squaring it k times) and then multiplying in
     * a value base^{c_i}, which we can look up in our table.
     *
     * Side-channel considerations: the exponent is secret, so
     * actually doing a single table lookup by using a chunk of
     * exponent bits as an array index would be an obvious leak of
     * secret information into the cache. So instead, in each
     * iteration, we read _all_ the table entries, and do a sequence
     * of mp_select operations to leave just the one we wanted in the
     * variable that will go into the multiplication. In other
     * contexts (like software AES) that technique is so prohibitively
     * slow that it makes you choose a strategy that doesn't use table
     * lookups at all (we do bitslicing in preference); but here, this
     * iteration through 2^k table elements is replacing k-1 bignum
     * _multiplications_ that you'd have to use instead if you did
     * simple square-and-multiply, and that makes it still a win.
     */

    /* Table that holds base^0, ..., base^{w-1} */
    mp_int *table[MODPOW_WINDOW_SIZE];
    table[0] = mp_copy(monty_identity(mc));
    { // WINSCP
    size_t i;
    for (i = 1; i < MODPOW_WINDOW_SIZE; i++)
        table[i] = monty_mul(mc, table[i-1], base);

    /* out accumulates the output value */
    { // WINSCP
    mp_int *out = mp_make_sized(mc->rw);
    mp_copy_into(out, monty_identity(mc));

    /* table_entry will hold each value we get out of the table */
    { // WINSCP
    mp_int *table_entry = mp_make_sized(mc->rw);

    /* Bit index of the chunk of bits we're working on. Start with the
     * highest multiple of k strictly less than the size of our
     * bignum, i.e. the highest-index chunk of bits that might
     * conceivably contain any nonzero bit. */
    { // WINSCP
    size_t i = (exponent->nw * BIGNUM_INT_BITS) - 1;
    i -= i % MODPOW_LOG2_WINDOW_SIZE;

    { // WINSCP
    bool first_iteration = true;

    while (true) {
        /* Construct the table index */
        unsigned table_index = 0;
        { // WINSCP
        size_t j;
        for (j = 0; j < MODPOW_LOG2_WINDOW_SIZE; j++)
            table_index |= mp_get_bit(exponent, i+j) << j;

        /* Iterate through the table to do a side-channel-safe lookup,
         * ending up with table_entry = table[table_index] */
        mp_copy_into(table_entry, table[0]);
        { // WINSCP
        size_t j;
        for (j = 1; j < MODPOW_WINDOW_SIZE; j++) {
            unsigned not_this_one =
                ((table_index ^ j) + MODPOW_WINDOW_SIZE - 1)
                >> MODPOW_LOG2_WINDOW_SIZE;
            mp_select_into(table_entry, table[j], table_entry, not_this_one);
        }

        if (!first_iteration) {
            /* Multiply into the output */
            monty_mul_into(mc, out, out, table_entry);
        } else {
            /* On the first iteration, we can save one multiplication
             * by just copying */
            mp_copy_into(out, table_entry);
            first_iteration = false;
        }

        /* If that was the bottommost chunk of bits, we're done */
        if (i == 0)
            break;

        /* Otherwise, square k times and go round again. */
        { // WINSCP
        size_t j;
        for (j = 0; j < MODPOW_LOG2_WINDOW_SIZE; j++)
            monty_mul_into(mc, out, out, out);

        i-= MODPOW_LOG2_WINDOW_SIZE;
        } // WINSCP
        } // WINSCP
        } // WINSCP
    }

    { // WINSCP
    size_t i;
    for (i = 0; i < MODPOW_WINDOW_SIZE; i++)
        mp_free(table[i]);
    mp_free(table_entry);
    WINSCP_PUTTY_SECTION_ENTER;
    mp_clear(mc->winscp_guarded_scratch);
    WINSCP_PUTTY_SECTION_LEAVE;
    return out;
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

mp_int *mp_modpow(mp_int *base, mp_int *exponent, mp_int *modulus)
{
    assert(modulus->nw > 0);
    assert(modulus->w[0] & 1);

    { // WINSCP
    MontyContext *mc = monty_new(modulus);
    mp_int *m_base = monty_import(mc, base);
    mp_int *m_out = monty_pow(mc, m_base, exponent);
    mp_int *out = monty_export(mc, m_out);
    mp_free(m_base);
    mp_free(m_out);
    monty_free(mc);
    return out;
    } // WINSCP
}

/*
 * Given two input integers a,b which are not both even, computes d =
 * gcd(a,b) and also two integers A,B such that A*a - B*b = d. A,B
 * will be the minimal non-negative pair satisfying that criterion,
 * which is equivalent to saying that 0 <= A < b/d and 0 <= B < a/d.
 *
 * This algorithm is an adapted form of Stein's algorithm, which
 * computes gcd(a,b) using only addition and bit shifts (i.e. without
 * needing general division), using the following rules:
 *
 *  - if both of a,b are even, divide off a common factor of 2
 *  - if one of a,b (WLOG a) is even, then gcd(a,b) = gcd(a/2,b), so
 *    just divide a by 2
 *  - if both of a,b are odd, then WLOG a>b, and gcd(a,b) =
 *    gcd(b,(a-b)/2).
 *
 * Sometimes this function is used for modular inversion, in which
 * case we already know we expect the two inputs to be coprime, so to
 * save time the 'both even' initial case is assumed not to arise (or
 * to have been handled already by the caller). So this function just
 * performs a sequence of reductions in the following form:
 *
 *  - if a,b are both odd, sort them so that a > b, and replace a with
 *    b-a; otherwise sort them so that a is the even one
 *  - either way, now a is even and b is odd, so divide a by 2.
 *
 * The big change to Stein's algorithm is that we need the Bezout
 * coefficients as output, not just the gcd. So we need to know how to
 * generate those in each case, based on the coefficients from the
 * reduced pair of numbers:
 *
 *  - If a is even, and u,v are such that u*(a/2) + v*b = d:
 *     + if u is also even, then this is just (u/2)*a + v*b = d
 *     + otherwise, (u+b)*(a/2) + (v-a/2)*b is also equal to d, and
 *       since u and b are both odd, (u+b)/2 is an integer, so we have
 *       ((u+b)/2)*a + (v-a/2)*b = d.
 *
 *  - If a,b are both odd, and u,v are such that u*b + v*(a-b) = d,
 *    then v*a + (u-v)*b = d.
 *
 * In the case where we passed from (a,b) to (b,(a-b)/2), we regard it
 * as having first subtracted b from a and then halved a, so both of
 * these transformations must be done in sequence.
 *
 * The code below transforms this from a recursive to an iterative
 * algorithm. We first reduce a,b to 0,1, recording at each stage
 * whether we did the initial subtraction, and whether we had to swap
 * the two values; then we iterate backwards over that record of what
 * we did, applying the above rules for building up the Bezout
 * coefficients as we go. Of course, all the case analysis is done by
 * the usual bit-twiddling conditionalisation to avoid data-dependent
 * control flow.
 *
 * Also, since these mp_ints are generally treated as unsigned, we
 * store the coefficients by absolute value, with the semantics that
 * they always have opposite sign, and in the unwinding loop we keep a
 * bit indicating whether Aa-Bb is currently expected to be +d or -d,
 * so that we can do one final conditional adjustment if it's -d.
 *
 * Once the reduction rules have managed to reduce the input numbers
 * to (0,d), then they are stable (the next reduction will always
 * divide the even one by 2, which maps 0 to 0). So it doesn't matter
 * if we do more steps of the algorithm than necessary; hence, for
 * constant time, we just need to find the maximum number we could
 * _possibly_ require, and do that many.
 *
 * If a,b < 2^n, at most 2n iterations are required. Proof: consider
 * the quantity Q = log_2(a) + log_2(b). Every step halves one of the
 * numbers (and may also reduce one of them further by doing a
 * subtraction beforehand, but in the worst case, not by much or not
 * at all). So Q reduces by at least 1 per iteration, and it starts
 * off with a value at most 2n.
 *
 * The worst case inputs (I think) are where x=2^{n-1} and y=2^n-1
 * (i.e. x is a power of 2 and y is all 1s). In that situation, the
 * first n-1 steps repeatedly halve x until it's 1, and then there are
 * n further steps each of which subtracts 1 from y and halves it.
 */
static void mp_bezout_into(mp_int *a_coeff_out, mp_int *b_coeff_out,
                           mp_int *gcd_out, mp_int *a_in, mp_int *b_in)
{
    size_t nw = size_t_max(1, size_t_max(a_in->nw, b_in->nw));

    /* Make mutable copies of the input numbers */
    mp_int *a = mp_make_sized(nw), *b = mp_make_sized(nw);
    mp_copy_into(a, a_in);
    mp_copy_into(b, b_in);

    /* Space to build up the output coefficients, with an extra word
     * so that intermediate values can overflow off the top and still
     * right-shift back down to the correct value */
    { // WINSCP
    mp_int *ac = mp_make_sized(nw + 1), *bc = mp_make_sized(nw + 1);

    /* And a general-purpose temp register */
    mp_int *tmp = mp_make_sized(nw);

    /* Space to record the sequence of reduction steps to unwind. We
     * make it a BignumInt for no particular reason except that (a)
     * mp_make_sized conveniently zeroes the allocation and mp_free
     * wipes it, and (b) this way I can use mp_dump() if I have to
     * debug this code. */
    size_t steps = 2 * nw * BIGNUM_INT_BITS;
    mp_int *record = mp_make_sized(
        (steps*2 + BIGNUM_INT_BITS - 1) / BIGNUM_INT_BITS);
    size_t step; // WINSCP

    for (step = 0; step < steps; step++) {
        /*
         * If a and b are both odd, we want to sort them so that a is
         * larger. But if one is even, we want to sort them so that a
         * is the even one.
         */
        unsigned swap_if_both_odd = mp_cmp_hs(b, a);
        unsigned swap_if_one_even = a->w[0] & 1;
        unsigned both_odd = a->w[0] & b->w[0] & 1;
        unsigned swap = swap_if_one_even ^ (
            (swap_if_both_odd ^ swap_if_one_even) & both_odd);

        mp_cond_swap(a, b, swap);

        /*
         * If a,b are both odd, then a is the larger number, so
         * subtract the smaller one from it.
         */
        mp_cond_sub_into(a, a, b, both_odd);

        /*
         * Now a is even, so divide it by two.
         */
        mp_rshift_fixed_into(a, a, 1);

        /*
         * Record the two 1-bit values both_odd and swap.
         */
        mp_set_bit(record, step*2, both_odd);
        mp_set_bit(record, step*2+1, swap);
    }

    /*
     * Now we expect to have reduced the two numbers to 0 and d,
     * although we don't know which way round. (But we avoid checking
     * this by assertion; sometimes we'll need to do this computation
     * without giving away that we already know the inputs were bogus.
     * So we'd prefer to just press on and return nonsense.)
     */

    if (gcd_out) {
        /*
         * At this point we can return the actual gcd. Since one of
         * a,b is it and the other is zero, the easiest way to get it
         * is to add them together.
         */
        mp_add_into(gcd_out, a, b);
    }

    /*
     * If the caller _only_ wanted the gcd, and neither Bezout
     * coefficient is even required, we can skip the entire unwind
     * stage.
     */
    if (a_coeff_out || b_coeff_out) {

        /*
         * The Bezout coefficients of a,b at this point are simply 0
         * for whichever of a,b is zero, and 1 for whichever is
         * nonzero. The nonzero number equals gcd(a,b), which by
         * assumption is odd, so we can do this by just taking the low
         * bit of each one.
         */
        ac->w[0] = mp_get_bit(a, 0);
        bc->w[0] = mp_get_bit(b, 0);

        /*
         * Overwrite a,b themselves with those same numbers. This has
         * the effect of dividing both of them by d, which will
         * arrange that during the unwind stage we generate the
         * minimal coefficients instead of a larger pair.
         */
        mp_copy_into(a, ac);
        mp_copy_into(b, bc);

        /*
         * We'll maintain the invariant as we unwind that ac * a - bc
         * * b is either +d or -d (or rather, +1/-1 after scaling by
         * d), and we'll remember which. (We _could_ keep it at +d the
         * whole time, but it would cost more work every time round
         * the loop, so it's cheaper to fix that up once at the end.)
         *
         * Initially, the result is +d if a was the nonzero value after
         * reduction, and -d if b was.
         */
        { // WINSCP
        unsigned minus_d = b->w[0];

        size_t step; // WINSCP
        for (step = steps; step-- > 0 ;) {
            /*
             * Recover the data from the step we're unwinding.
             */
            unsigned both_odd = mp_get_bit(record, step*2);
            unsigned swap = mp_get_bit(record, step*2+1);

            /*
             * Unwind the division: if our coefficient of a is odd, we
             * adjust the coefficients by +b and +a respectively.
             */
            unsigned adjust = ac->w[0] & 1;
            mp_cond_add_into(ac, ac, b, adjust);
            mp_cond_add_into(bc, bc, a, adjust);

            /*
             * Now ac is definitely even, so we divide it by two.
             */
            mp_rshift_fixed_into(ac, ac, 1);

            /*
             * Now unwind the subtraction, if there was one, by adding
             * ac to bc.
             */
            mp_cond_add_into(bc, bc, ac, both_odd);

            /*
             * Undo the transformation of the input numbers, by
             * multiplying a by 2 and then adding b to a (the latter
             * only if both_odd).
             */
            mp_lshift_fixed_into(a, a, 1);
            mp_cond_add_into(a, a, b, both_odd);

            /*
             * Finally, undo the swap. If we do swap, this also
             * reverses the sign of the current result ac*a+bc*b.
             */
            mp_cond_swap(a, b, swap);
            mp_cond_swap(ac, bc, swap);
            minus_d ^= swap;
        }

        /*
         * Now we expect to have recovered the input a,b (or rather,
         * the versions of them divided by d). But we might find that
         * our current result is -d instead of +d, that is, we have
         * A',B' such that A'a - B'b = -d.
         *
         * In that situation, we set A = b-A' and B = a-B', giving us
         * Aa-Bb = ab - A'a - ab + B'b = +1.
         */
        mp_sub_into(tmp, b, ac);
        mp_select_into(ac, ac, tmp, minus_d);
        mp_sub_into(tmp, a, bc);
        mp_select_into(bc, bc, tmp, minus_d);

        /*
         * Now we really are done. Return the outputs.
         */
        if (a_coeff_out)
            mp_copy_into(a_coeff_out, ac);
        if (b_coeff_out)
            mp_copy_into(b_coeff_out, bc);

        } // WINSCP
    }

    mp_free(a);
    mp_free(b);
    mp_free(ac);
    mp_free(bc);
    mp_free(tmp);
    mp_free(record);
    } // WINSCP
}

mp_int *mp_invert(mp_int *x, mp_int *m)
{
    mp_int *result = mp_make_sized(m->nw);
    mp_bezout_into(result, NULL, NULL, x, m);
    return result;
}

void mp_gcd_into(mp_int *a, mp_int *b, mp_int *gcd, mp_int *A, mp_int *B)
{
    /*
     * Identify shared factors of 2. To do this we OR the two numbers
     * to get something whose lowest set bit is in the right place,
     * remove all higher bits by ANDing it with its own negation, and
     * use mp_get_nbits to find the location of the single remaining
     * set bit.
     */
    mp_int *tmp = mp_make_sized(size_t_max(a->nw, b->nw));
    size_t i; // WINSCP
    for (i = 0; i < tmp->nw; i++)
        tmp->w[i] = mp_word(a, i) | mp_word(b, i);
    { // WINSCP
    BignumCarry carry = 1;
    size_t i;
    for (i = 0; i < tmp->nw; i++) {
        BignumInt negw;
        BignumADC(negw, carry, 0, ~tmp->w[i], carry);
        tmp->w[i] &= negw;
    }
    { // WINSCP
    size_t shift = mp_get_nbits(tmp) - 1;
    mp_free(tmp);

    /*
     * Make copies of a,b with those shared factors of 2 divided off,
     * so that at least one is odd (which is the precondition for
     * mp_bezout_into). Compute the gcd of those.
     */
    { // WINSCP
    mp_int *as = mp_rshift_safe(a, shift);
    mp_int *bs = mp_rshift_safe(b, shift);
    mp_bezout_into(A, B, gcd, as, bs);
    mp_free(as);
    mp_free(bs);

    /*
     * And finally shift the gcd back up (unless the caller didn't
     * even ask for it), to put the shared factors of 2 back in.
     */
    if (gcd)
        mp_lshift_safe_in_place(gcd, shift);
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

mp_int *mp_gcd(mp_int *a, mp_int *b)
{
    mp_int *gcd = mp_make_sized(size_t_min(a->nw, b->nw));
    mp_gcd_into(a, b, gcd, NULL, NULL);
    return gcd;
}

unsigned mp_coprime(mp_int *a, mp_int *b)
{
    mp_int *gcd = mp_gcd(a, b);
    unsigned toret = mp_eq_integer(gcd, 1);
    mp_free(gcd);
    return toret;
}

static uint32_t recip_approx_32(uint32_t x)
{
    /*
     * Given an input x in [2^31,2^32), i.e. a uint32_t with its high
     * bit set, this function returns an approximation to 2^63/x,
     * computed using only multiplications and bit shifts just in case
     * the C divide operator has non-constant time (either because the
     * underlying machine instruction does, or because the operator
     * expands to a library function on a CPU without hardware
     * division).
     *
     * The coefficients are derived from those of the degree-9
     * polynomial which is the minimax-optimal approximation to that
     * function on the given interval (generated using the Remez
     * algorithm), converted into integer arithmetic with shifts used
     * to maximise the number of significant bits at every state. (A
     * sort of 'static floating point' - the exponent is statically
     * known at every point in the code, so it never needs to be
     * stored at run time or to influence runtime decisions.)
     *
     * Exhaustive iteration over the whole input space shows the
     * largest possible error to be 1686.54. (The input value
     * attaining that bound is 4226800006 == 0xfbefd986, whose true
     * reciprocal is 2182116973.540... == 0x8210766d.8a6..., whereas
     * this function returns 2182115287 == 0x82106fd7.)
     */
    uint64_t r = 0x92db03d6ULL;
    r = 0xf63e71eaULL - ((r*x) >> 34);
    r = 0xb63721e8ULL - ((r*x) >> 34);
    r = 0x9c2da00eULL - ((r*x) >> 33);
    r = 0xaada0bb8ULL - ((r*x) >> 32);
    r = 0xf75cd403ULL - ((r*x) >> 31);
    r = 0xecf97a41ULL - ((r*x) >> 31);
    r = 0x90d876cdULL - ((r*x) >> 31);
    r = 0x6682799a0ULL - ((r*x) >> 26);
    return r;
}

void mp_divmod_into(mp_int *n, mp_int *d, mp_int *q_out, mp_int *r_out)
{
    pinitassert(!mp_eq_integer(d, 0));

    /*
     * We do division by using Newton-Raphson iteration to converge to
     * the reciprocal of d (or rather, R/d for R a sufficiently large
     * power of 2); then we multiply that reciprocal by n; and we
     * finish up with conditional subtraction.
     *
     * But we have to do it in a fixed number of N-R iterations, so we
     * need some error analysis to know how many we might need.
     *
     * The iteration is derived by defining f(r) = d - R/r.
     * Differentiating gives f'(r) = R/r^2, and the Newton-Raphson
     * formula applied to those functions gives
     *
     *      r_{i+1} = r_i - f(r_i) / f'(r_i)
     *              = r_i - (d - R/r_i) r_i^2 / R
     *              = r_i (2 R - d r_i) / R
     *
     * Now let e_i be the error in a given iteration, in the sense
     * that
     *
     *        d r_i = R + e_i
     *  i.e.  e_i/R = (r_i - r_true) / r_true
     *
     * so e_i is the _relative_ error in r_i.
     *
     * We must also introduce a rounding-error term, because the
     * division by R always gives an integer. This might make the
     * output off by up to 1 (in the negative direction, because
     * right-shifting gives floor of the true quotient). So when we
     * divide by R, we must imagine adding some f in [0,1). Then we
     * have
     *
     *    d r_{i+1} = d r_i (2 R - d r_i) / R - d f
     *              = (R + e_i) (R - e_i) / R - d f
     *              = (R^2 - e_i^2) / R - d f
     *              = R - (e_i^2 / R + d f)
     * =>   e_{i+1} = - (e_i^2 / R + d f)
     *
     * The sum of two positive quantities is bounded above by twice
     * their max, and max |f| = 1, so we can bound this as follows:
     *
     *               |e_{i+1}| <= 2 max (e_i^2/R, d)
     *             |e_{i+1}/R| <= 2 max ((e_i/R)^2, d/R)
     *        log2 |R/e_{i+1}| <= min (2 log2 |R/e_i|, log2 |R/d|) - 1
     *
     * which tells us that the number of 'good' bits - i.e.
     * log2(R/e_i) - very nearly doubles at every iteration (apart
     * from that subtraction of 1), until it gets to the same size as
     * log2(R/d). In other words, the size of R in bits has to be the
     * size of denominator we're putting in, _plus_ the amount of
     * precision we want to get back out.
     *
     * So when we multiply n (the input numerator) by our final
     * reciprocal approximation r, but actually r differs from R/d by
     * up to 2, then it follows that
     *
     *   n/d - nr/R = n/d - [ n (R/d + e) ] / R
     *              = n/d - [ (n/d) R + n e ] / R
     *              = -ne/R
     *      =>   0 <= n/d - nr/R < 2n/R
     *
     * so our computed quotient can differ from the true n/d by up to
     * 2n/R. Hence, as long as we also choose R large enough that 2n/R
     * is bounded above by a constant, we can guarantee a bounded
     * number of final conditional-subtraction steps.
     */

    /*
     * Get at least 32 of the most significant bits of the input
     * number.
     */
    size_t hiword_index = 0;
    uint64_t hibits = 0, lobits = 0;
    mp_find_highest_nonzero_word_pair(d, 64 - BIGNUM_INT_BITS,
                                      &hiword_index, &hibits, &lobits);

    /*
     * Make a shifted combination of those two words which puts the
     * topmost bit of the number at bit 63.
     */
    { // WINSCP
    size_t shift_up = 0;
    size_t i; // WINSCP
    for (i = BIGNUM_INT_BITS_BITS; i-- > 0;) {
        size_t sl = (size_t)1 << i;       /* left shift count */
        size_t sr = 64 - sl;     /* complementary right-shift count */

        /* Should we shift up? */
        unsigned indicator = 1 ^ normalise_to_1_u64(hibits >> sr);

        /* If we do, what will we get? */
        uint64_t new_hibits = (hibits << sl) | (lobits >> sr);
        uint64_t new_lobits = lobits << sl;
        size_t new_shift_up = shift_up + sl;

        /* Conditionally swap those values in. */
        hibits    ^= (hibits    ^ new_hibits   ) & -(uint64_t)indicator;
        lobits    ^= (lobits    ^ new_lobits   ) & -(uint64_t)indicator;
        shift_up  ^= (shift_up  ^ new_shift_up ) & -(size_t)  indicator;
    }

    /*
     * So now we know the most significant 32 bits of d are at the top
     * of hibits. Approximate the reciprocal of those bits.
     */
    lobits = (uint64_t)recip_approx_32(hibits >> 32) << 32;
    hibits = 0;

    /*
     * And shift that up by as many bits as the input was shifted up
     * just now, so that the product of this approximation and the
     * actual input will be close to a fixed power of two regardless
     * of where the MSB was.
     *
     * I do this in another log n individual passes, partly in case
     * the CPU's register-controlled shift operation isn't
     * time-constant, and also in case the compiler code-generates
     * uint64_t shifts out of a variable number of smaller-word shift
     * instructions, e.g. by splitting up into cases.
     */
    for (i = BIGNUM_INT_BITS_BITS; i-- > 0;) {
        size_t sl = (size_t)1 << i;       /* left shift count */
        size_t sr = 64 - sl;     /* complementary right-shift count */

        /* Should we shift up? */
        unsigned indicator = 1 & (shift_up >> i);

        /* If we do, what will we get? */
        uint64_t new_hibits = (hibits << sl) | (lobits >> sr);
        uint64_t new_lobits = lobits << sl;

        /* Conditionally swap those values in. */
        hibits    ^= (hibits    ^ new_hibits   ) & -(uint64_t)indicator;
        lobits    ^= (lobits    ^ new_lobits   ) & -(uint64_t)indicator;
    }

    /*
     * The product of the 128-bit value now in hibits:lobits with the
     * 128-bit value we originally retrieved in the same variables
     * will be in the vicinity of 2^191. So we'll take log2(R) to be
     * 191, plus a multiple of BIGNUM_INT_BITS large enough to allow R
     * to hold the combined sizes of n and d.
     */
    { // WINSCP
    size_t log2_R;
    {
        size_t max_log2_n = (n->nw + d->nw) * BIGNUM_INT_BITS;
        log2_R = max_log2_n + 3;
        log2_R -= size_t_min(191, log2_R);
        log2_R = (log2_R + BIGNUM_INT_BITS - 1) & ~(BIGNUM_INT_BITS - 1);
        log2_R += 191;
    }

    /* Number of words in a bignum capable of holding numbers the size
     * of twice R. */
    { // WINSCP
    size_t rw = ((log2_R+2) + BIGNUM_INT_BITS - 1) / BIGNUM_INT_BITS;

    /*
     * Now construct our full-sized starting reciprocal approximation.
     */
    mp_int *r_approx = mp_make_sized(rw);
    size_t output_bit_index;
    {
        /* Where in the input number did the input 128-bit value come from? */
        size_t input_bit_index =
            (hiword_index * BIGNUM_INT_BITS) - (128 - BIGNUM_INT_BITS);

        /* So how far do we need to shift our 64-bit output, if the
         * product of those two fixed-size values is 2^191 and we want
         * to make it 2^log2_R instead? */
        output_bit_index = log2_R - 191 - input_bit_index;

        /* If we've done all that right, it should be a whole number
         * of words. */
        assert(output_bit_index % BIGNUM_INT_BITS == 0);
        { // WINSCP
        size_t output_word_index = output_bit_index / BIGNUM_INT_BITS;

        mp_add_integer_into_shifted_by_words(
            r_approx, r_approx, lobits, output_word_index);
        mp_add_integer_into_shifted_by_words(
            r_approx, r_approx, hibits,
            output_word_index + 64 / BIGNUM_INT_BITS);
        } // WINSCP
    }

    /*
     * Make the constant 2*R, which we'll need in the iteration.
     */
    { // WINSCP
    mp_int *two_R = mp_make_sized(rw);
    BignumInt top_word = (BignumInt)1 << ((log2_R+1) % BIGNUM_INT_BITS);
    mp_add_integer_into_shifted_by_words(
        two_R, two_R, top_word, (log2_R+1) / BIGNUM_INT_BITS);

    /*
     * Scratch space.
     */
    { // WINSCP
    mp_int *dr = mp_make_sized(rw + d->nw);
    mp_int *diff = mp_make_sized(size_t_max(rw, dr->nw));
    mp_int *product = mp_make_sized(rw + diff->nw);
    size_t scratchsize = size_t_max(
        mp_mul_scratchspace(dr->nw, r_approx->nw, d->nw),
        mp_mul_scratchspace(product->nw, r_approx->nw, diff->nw));
    mp_int *scratch = mp_make_sized(scratchsize);
    mp_int product_shifted = mp_make_alias(
        product, log2_R / BIGNUM_INT_BITS, product->nw);

    /*
     * Initial error estimate: the 32-bit output of recip_approx_32
     * differs by less than 2048 (== 2^11) from the true top 32 bits
     * of the reciprocal, so the relative error is at most 2^11
     * divided by the 32-bit reciprocal, which at worst is 2^11/2^31 =
     * 2^-20. So even in the worst case, we have 20 good bits of
     * reciprocal to start with.
     */
    size_t good_bits = 31 - 11;
    size_t good_bits_needed = BIGNUM_INT_BITS * n->nw + 4; /* add a few */

    /*
     * Now do Newton-Raphson iterations until we have reason to think
     * they're not converging any more.
     */
    while (good_bits < good_bits_needed) {
        /*
         * Compute the next iterate.
         */
        mp_mul_internal(dr, r_approx, d, *scratch);
        mp_sub_into(diff, two_R, dr);
        mp_mul_internal(product, r_approx, diff, *scratch);
        mp_rshift_fixed_into(r_approx, &product_shifted,
                             log2_R % BIGNUM_INT_BITS);

        /*
         * Adjust the error estimate.
         */
        good_bits = good_bits * 2 - 1;
    }

    mp_free(dr);
    mp_free(diff);
    mp_free(product);
    mp_free(scratch);

    /*
     * Now we've got our reciprocal, we can compute the quotient, by
     * multiplying in n and then shifting down by log2_R bits.
     */
    { // WINSCP
    mp_int *quotient_full = mp_mul(r_approx, n);
    mp_int quotient_alias = mp_make_alias(
        quotient_full, log2_R / BIGNUM_INT_BITS, quotient_full->nw);
    mp_int *quotient = mp_make_sized(n->nw);
    mp_rshift_fixed_into(quotient, &quotient_alias, log2_R % BIGNUM_INT_BITS);

    /*
     * Next, compute the remainder.
     */
    { // WINSCP
    mp_int *remainder = mp_make_sized(d->nw);
    mp_mul_into(remainder, quotient, d);
    mp_sub_into(remainder, n, remainder);

    /*
     * Finally, two conditional subtractions to fix up any remaining
     * rounding error. (I _think_ one should be enough, but this
     * routine isn't time-critical enough to take chances.)
     */
    { // WINSCP
    unsigned q_correction = 0;
    unsigned iter; // WINSCP
    for (iter = 0; iter < 2; iter++) {
        unsigned need_correction = mp_cmp_hs(remainder, d);
        mp_cond_sub_into(remainder, remainder, d, need_correction);
        q_correction += need_correction;
    }
    mp_add_integer_into(quotient, quotient, q_correction);

    /*
     * Now we should have a perfect answer, i.e. 0 <= r < d.
     */
    assert(!mp_cmp_hs(remainder, d));

    if (q_out)
        mp_copy_into(q_out, quotient);
    if (r_out)
        mp_copy_into(r_out, remainder);

    mp_free(r_approx);
    mp_free(two_R);
    mp_free(quotient_full);
    mp_free(quotient);
    mp_free(remainder);
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

mp_int *mp_div(mp_int *n, mp_int *d)
{
    mp_int *q = mp_make_sized(n->nw);
    mp_divmod_into(n, d, q, NULL);
    return q;
}

mp_int *mp_mod(mp_int *n, mp_int *d)
{
    mp_int *r = mp_make_sized(d->nw);
    mp_divmod_into(n, d, NULL, r);
    return r;
}

uint32_t mp_mod_known_integer(mp_int *x, uint32_t m)
{
    uint64_t reciprocal = ((uint64_t)1 << 48) / m;
    uint64_t accumulator = 0;
    { // WINSCP
    size_t i;
    for (i = mp_max_bytes(x); i-- > 0 ;) {
        accumulator = 0x100 * accumulator + mp_get_byte(x, i);
        /*
         * Let A be the value in 'accumulator' at this point, and let
         * R be the value it will have after we subtract quot*m below.
         *
         * Lemma 1: if A < 2^48, then R < 2m.
         *
         * Proof:
         *
         * By construction, we have 2^48/m - 1 < reciprocal <= 2^48/m.
         * Multiplying that by the accumulator gives
         *
         *      A/m * 2^48 - A < unshifted_quot <= A/m * 2^48
         * i.e. 0 <= (A/m * 2^48) - unshifted_quot < A
         * i.e. 0 <= A/m - unshifted_quot/2^48 < A/2^48
         *
         * So when we shift this quotient right by 48 bits, i.e. take
         * the floor of (unshifted_quot/2^48), the value we take the
         * floor of is at most A/2^48 less than the true rational
         * value A/m that we _wanted_ to take the floor of.
         *
         * Provided A < 2^48, this is less than 1. So the quotient
         * 'quot' that we've just produced is either the true quotient
         * floor(A/m), or one less than it. Hence, the output value R
         * is less than 2m. []
         *
         * Lemma 2: if A < 2^16 m, then the multiplication of
         * accumulator*reciprocal does not overflow.
         *
         * Proof: as above, we have reciprocal <= 2^48/m. Multiplying
         * by A gives unshifted_quot <= 2^48 * A / m < 2^48 * 2^16 =
         * 2^64. []
         */
        { // WINSCP
        uint64_t unshifted_quot = accumulator * reciprocal;
        uint64_t quot = unshifted_quot >> 48;
        accumulator -= quot * m;
        } // WINSCP
    }

    /*
     * Theorem 1: accumulator < 2m at the end of every iteration of
     * this loop.
     *
     * Proof: induction on the above loop.
     *
     * Base case: at the start of the first loop iteration, the
     * accumulator is 0, which is certainly < 2m.
     *
     * Inductive step: in each loop iteration, we take a value at most
     * 2m-1, multiply it by 2^8, and add another byte less than 2^8 to
     * generate the input value A to the reduction process above. So
     * we have A < 2m * 2^8 - 1. We know m < 2^32 (because it was
     * passed in as a uint32_t), so A < 2^41, which is enough to allow
     * us to apply Lemma 1, showing that the value of 'accumulator' at
     * the end of the loop is still < 2m. []
     *
     * Corollary: we need at most one final subtraction of m to
     * produce the canonical residue of x mod m, i.e. in the range
     * [0,m).
     *
     * Theorem 2: no multiplication in the inner loop overflows.
     *
     * Proof: in Theorem 1 we established A < 2m * 2^8 - 1 in every
     * iteration. That is less than m * 2^16, so Lemma 2 applies.
     *
     * The other multiplication, of quot * m, cannot overflow because
     * quot is at most A/m, so quot*m <= A < 2^64. []
     */

    { // WINSCP
    uint32_t result = accumulator;
    uint32_t reduced = result - m;
    uint32_t select = -(reduced >> 31);
    result = reduced ^ ((result ^ reduced) & select);
    assert(result < m);
    return result;
    } // WINSCP
    } // WINSCP
}

mp_int *mp_nthroot(mp_int *y, unsigned n, mp_int *remainder_out)
{
    /*
     * Allocate scratch space.
     */
    mp_int **alloc, **powers, **newpowers, *scratch;
    size_t nalloc = 2*(n+1)+1;
    alloc = snewn(nalloc, mp_int *);
    { // WINSCP
    size_t i; // WINSCP
    for (i = 0; i < nalloc; i++)
        alloc[i] = mp_make_sized(y->nw + 1);
    powers = alloc;
    newpowers = alloc + (n+1);
    scratch = alloc[2*n+2];

    /*
     * We're computing the rounded-down nth root of y, i.e. the
     * maximal x such that x^n <= y. We try to add 2^i to it for each
     * possible value of i, starting from the largest one that might
     * fit (i.e. such that 2^{n*i} fits in the size of y) downwards to
     * i=0.
     *
     * We track all the smaller powers of x in the array 'powers'. In
     * each iteration, if we update x, we update all of those values
     * to match.
     */
    mp_copy_integer_into(powers[0], 1);
    { // WINSCP
    size_t s; // WINSCP
    for (s = mp_max_bits(y) / n + 1; s-- > 0 ;) {
        /*
         * Let b = 2^s. We need to compute the powers (x+b)^i for each
         * i, starting from our recorded values of x^i.
         */
        size_t i; // WINSCP
        for (i = 0; i < n+1; i++) {
            /*
             * (x+b)^i = x^i
             *         + (i choose 1) x^{i-1} b
             *         + (i choose 2) x^{i-2} b^2
             *         + ...
             *         + b^i
             */
            uint16_t binom = 1;       /* coefficient of b^i */
            mp_copy_into(newpowers[i], powers[i]);
            { // WINSCP
            size_t j; // WINSCP
            for (j = 0; j < i; j++) {
                /* newpowers[i] += binom * powers[j] * 2^{(i-j)*s} */
                mp_mul_integer_into(scratch, powers[j], binom);
                mp_lshift_fixed_into(scratch, scratch, (i-j) * s);
                mp_add_into(newpowers[i], newpowers[i], scratch);

                { // WINSCP
                uint32_t binom_mul = binom;
                binom_mul *= (i-j);
                binom_mul /= (j+1);
                assert(binom_mul < 0x10000);
                binom = binom_mul;
                } // WINSCP
            }
            } // WINSCP
        }

        /*
         * Now, is the new value of x^n still <= y? If so, update.
         */
        { // WINSCP
        unsigned newbit = mp_cmp_hs(y, newpowers[n]);
        size_t i; // WINSCP
        for (i = 0; i < n+1; i++)
            mp_select_into(powers[i], powers[i], newpowers[i], newbit);
        } // WINSCP
    }

    if (remainder_out)
        mp_sub_into(remainder_out, y, powers[n]);

    { // WINSCP
    mp_int *root = mp_new(mp_max_bits(y) / n);
    mp_copy_into(root, powers[1]);

    { // WINSCP
    size_t i;
    for (i = 0; i < nalloc; i++)
        mp_free(alloc[i]);
    sfree(alloc);

    return root;
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

mp_int *mp_modmul(mp_int *x, mp_int *y, mp_int *modulus)
{
    mp_int *product = mp_mul(x, y);
    mp_int *reduced = mp_mod(product, modulus);
    mp_free(product);
    return reduced;
}

mp_int *mp_modadd(mp_int *x, mp_int *y, mp_int *modulus)
{
    mp_int *sum = mp_add(x, y);
    mp_int *reduced = mp_mod(sum, modulus);
    mp_free(sum);
    return reduced;
}

mp_int *mp_modsub(mp_int *x, mp_int *y, mp_int *modulus)
{
    mp_int *diff = mp_make_sized(size_t_max(x->nw, y->nw));
    mp_sub_into(diff, x, y);
    { // WINSCP
    unsigned negate = mp_cmp_hs(y, x);
    mp_cond_negate(diff, diff, negate);
    { // WINSCP
    mp_int *residue = mp_mod(diff, modulus);
    mp_cond_negate(residue, residue, negate);
    /* If we've just negated the residue, then it will be < 0 and need
     * the modulus adding to it to make it positive - *except* if the
     * residue was zero when we negated it. */
    { // WINSCP
    unsigned make_positive = negate & ~mp_eq_integer(residue, 0);
    mp_cond_add_into(residue, residue, modulus, make_positive);
    mp_free(diff);
    return residue;
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

static mp_int *mp_modadd_in_range(mp_int *x, mp_int *y, mp_int *modulus)
{
    mp_int *sum = mp_make_sized(modulus->nw);
    unsigned carry = mp_add_into_internal(sum, x, y);
    mp_cond_sub_into(sum, sum, modulus, carry | mp_cmp_hs(sum, modulus));
    return sum;
}

static mp_int *mp_modsub_in_range(mp_int *x, mp_int *y, mp_int *modulus)
{
    mp_int *diff = mp_make_sized(modulus->nw);
    mp_sub_into(diff, x, y);
    mp_cond_add_into(diff, diff, modulus, 1 ^ mp_cmp_hs(x, y));
    return diff;
}

mp_int *monty_add(MontyContext *mc, mp_int *x, mp_int *y)
{
    return mp_modadd_in_range(x, y, mc->m);
}

mp_int *monty_sub(MontyContext *mc, mp_int *x, mp_int *y)
{
    return mp_modsub_in_range(x, y, mc->m);
}

void mp_min_into(mp_int *r, mp_int *x, mp_int *y)
{
    mp_select_into(r, x, y, mp_cmp_hs(x, y));
}

void mp_max_into(mp_int *r, mp_int *x, mp_int *y)
{
    mp_select_into(r, y, x, mp_cmp_hs(x, y));
}

mp_int *mp_min(mp_int *x, mp_int *y)
{
    mp_int *r = mp_make_sized(size_t_min(x->nw, y->nw));
    mp_min_into(r, x, y);
    return r;
}

mp_int *mp_max(mp_int *x, mp_int *y)
{
    mp_int *r = mp_make_sized(size_t_max(x->nw, y->nw));
    mp_max_into(r, x, y);
    return r;
}

mp_int *mp_power_2(size_t power)
{
    mp_int *x = mp_new(power + 1);
    mp_set_bit(x, power, 1);
    return x;
}

struct ModsqrtContext {
    mp_int *p;                      /* the prime */
    MontyContext *mc;                  /* for doing arithmetic mod p */

    /* Decompose p-1 as 2^e k, for positive integer e and odd k */
    size_t e;
    mp_int *k;
    mp_int *km1o2;                  /* (k-1)/2 */

    /* The user-provided value z which is not a quadratic residue mod
     * p, and its kth power. Both in Montgomery form. */
    mp_int *z, *zk;
};

ModsqrtContext *modsqrt_new(mp_int *p, mp_int *any_nonsquare_mod_p)
{
    ModsqrtContext *sc = snew(ModsqrtContext);
    memset(sc, 0, sizeof(ModsqrtContext));

    sc->p = mp_copy(p);
    sc->mc = monty_new(sc->p);
    sc->z = monty_import(sc->mc, any_nonsquare_mod_p);

    /* Find the lowest set bit in p-1. Since this routine expects p to
     * be non-secret (typically a well-known standard elliptic curve
     * parameter), for once we don't need clever bit tricks. */
    for (sc->e = 1; sc->e < BIGNUM_INT_BITS * p->nw; sc->e++)
        if (mp_get_bit(p, sc->e))
            break;

    sc->k = mp_rshift_fixed(p, sc->e);
    sc->km1o2 = mp_rshift_fixed(sc->k, 1);

    /* Leave zk to be filled in lazily, since it's more expensive to
     * compute. If this context turns out never to be needed, we can
     * save the bulk of the setup time this way. */

    return sc;
}

static void modsqrt_lazy_setup(ModsqrtContext *sc)
{
    if (!sc->zk)
        sc->zk = monty_pow(sc->mc, sc->z, sc->k);
}

void modsqrt_free(ModsqrtContext *sc)
{
    monty_free(sc->mc);
    mp_free(sc->p);
    mp_free(sc->z);
    mp_free(sc->k);
    mp_free(sc->km1o2);

    if (sc->zk)
        mp_free(sc->zk);

    sfree(sc);
}

mp_int *mp_modsqrt(ModsqrtContext *sc, mp_int *x, unsigned *success)
{
    mp_int *mx = monty_import(sc->mc, x);
    mp_int *mroot = monty_modsqrt(sc, mx, success);
    mp_free(mx);
    { // WINSCP
    mp_int *root = monty_export(sc->mc, mroot);
    mp_free(mroot);
    return root;
    } // WINSCP
}

/*
 * Modular square root, using an algorithm more or less similar to
 * Tonelli-Shanks but adapted for constant time.
 *
 * The basic idea is to write p-1 = k 2^e, where k is odd and e > 0.
 * Then the multiplicative group mod p (call it G) has a sequence of
 * e+1 nested subgroups G = G_0 > G_1 > G_2 > ... > G_e, where each
 * G_i is exactly half the size of G_{i-1} and consists of all the
 * squares of elements in G_{i-1}. So the innermost group G_e has
 * order k, which is odd, and hence within that group you can take a
 * square root by raising to the power (k+1)/2.
 *
 * Our strategy is to iterate over these groups one by one and make
 * sure the number x we're trying to take the square root of is inside
 * each one, by adjusting it if it isn't.
 *
 * Suppose g is a primitive root of p, i.e. a generator of G_0. (We
 * don't actually need to know what g _is_; we just imagine it for the
 * sake of understanding.) Then G_i consists of precisely the (2^i)th
 * powers of g, and hence, you can tell if a number is in G_i if
 * raising it to the power k 2^{e-i} gives 1. So the conceptual
 * algorithm goes: for each i, test whether x is in G_i by that
 * method. If it isn't, then the previous iteration ensured it's in
 * G_{i-1}, so it will be an odd power of g^{2^{i-1}}, and hence
 * multiplying by any other odd power of g^{2^{i-1}} will give x' in
 * G_i. And we have one of those, because our non-square z is an odd
 * power of g, so z^{2^{i-1}} is an odd power of g^{2^{i-1}}.
 *
 * (There's a special case in the very first iteration, where we don't
 * have a G_{i-1}. If it turns out that x is not even in G_1, that
 * means it's not a square, so we set *success to 0. We still run the
 * rest of the algorithm anyway, for the sake of constant time, but we
 * don't give a hoot what it returns.)
 *
 * When we get to the end and have x in G_e, then we can take its
 * square root by raising to (k+1)/2. But of course that's not the
 * square root of the original input - it's only the square root of
 * the adjusted version we produced during the algorithm. To get the
 * true output answer we also have to multiply by a power of z,
 * namely, z to the power of _half_ whatever we've been multiplying in
 * as we go along. (The power of z we multiplied in must have been
 * even, because the case in which we would have multiplied in an odd
 * power of z is the i=0 case, in which we instead set the failure
 * flag.)
 *
 * The code below is an optimised version of that basic idea, in which
 * we _start_ by computing x^k so as to be able to test membership in
 * G_i by only a few squarings rather than a full from-scratch modpow
 * every time; we also start by computing our candidate output value
 * x^{(k+1)/2}. So when the above description says 'adjust x by z^i'
 * for some i, we have to adjust our running values of x^k and
 * x^{(k+1)/2} by z^{ik} and z^{ik/2} respectively (the latter is safe
 * because, as above, i is always even). And it turns out that we
 * don't actually have to store the adjusted version of x itself at
 * all - we _only_ keep those two powers of it.
 */
mp_int *monty_modsqrt(ModsqrtContext *sc, mp_int *x, unsigned *success)
{
    modsqrt_lazy_setup(sc);

    { // WINSCP
    mp_int *scratch_to_free = mp_make_sized(3 * sc->mc->rw);
    mp_int scratch = *scratch_to_free;

    /*
     * Compute toret = x^{(k+1)/2}, our starting point for the output
     * square root, and also xk = x^k which we'll use as we go along
     * for knowing when to apply correction factors. We do this by
     * first computing x^{(k-1)/2}, then multiplying it by x, then
     * multiplying the two together.
     */
    mp_int *toret = monty_pow(sc->mc, x, sc->km1o2);
    mp_int xk = mp_alloc_from_scratch(&scratch, sc->mc->rw);
    mp_copy_into(&xk, toret);
    monty_mul_into(sc->mc, toret, toret, x);
    monty_mul_into(sc->mc, &xk, toret, &xk);

    { // WINSCP
    mp_int tmp = mp_alloc_from_scratch(&scratch, sc->mc->rw);

    mp_int power_of_zk = mp_alloc_from_scratch(&scratch, sc->mc->rw);
    size_t i; // WINSCP
    mp_copy_into(&power_of_zk, sc->zk);

    for (i = 0; i < sc->e; i++) {
        size_t j; // WINSCP
        mp_copy_into(&tmp, &xk);
        for (j = i+1; j < sc->e; j++)
            monty_mul_into(sc->mc, &tmp, &tmp, &tmp);
        { // WINSCP
        unsigned eq1 = mp_cmp_eq(&tmp, monty_identity(sc->mc));

        if (i == 0) {
            /* One special case: if x=0, then no power of x will ever
             * equal 1, but we should still report success on the
             * grounds that 0 does have a square root mod p. */
            *success = eq1 | mp_eq_integer(x, 0);
        } else {
            monty_mul_into(sc->mc, &tmp, toret, &power_of_zk);
            mp_select_into(toret, &tmp, toret, eq1);

            monty_mul_into(sc->mc, &power_of_zk,
                           &power_of_zk, &power_of_zk);

            monty_mul_into(sc->mc, &tmp, &xk, &power_of_zk);
            mp_select_into(&xk, &tmp, &xk, eq1);
        }
        } // WINSCP
    }

    mp_free(scratch_to_free);

    return toret;
    } // WINSCP
    } // WINSCP
}

mp_int *mp_random_bits_fn(size_t bits, random_read_fn_t random_read)
{
    size_t bytes = (bits + 7) / 8;
    uint8_t *randbuf = snewn(bytes, uint8_t);
    random_read(randbuf, bytes);
    if (bytes)
        randbuf[0] &= (2 << ((bits-1) & 7)) - 1;
    { // WINSCP
    mp_int *toret = mp_from_bytes_be(make_ptrlen(randbuf, bytes));
    smemclr(randbuf, bytes);
    sfree(randbuf);
    return toret;
    } // WINSCP
}

mp_int *mp_random_upto_fn(mp_int *limit, random_read_fn_t rf)
{
    /*
     * It would be nice to generate our random numbers in such a way
     * as to make every possible outcome literally equiprobable. But
     * we can't do that in constant time, so we have to go for a very
     * close approximation instead. I'm going to take the view that a
     * factor of (1+2^-128) between the probabilities of two outcomes
     * is acceptable on the grounds that you'd have to examine so many
     * outputs to even detect it.
     */
    mp_int *unreduced = mp_random_bits_fn(mp_max_bits(limit) + 128, rf);
    mp_int *reduced = mp_mod(unreduced, limit);
    mp_free(unreduced);
    return reduced;
}

mp_int *mp_random_in_range_fn(mp_int *lo, mp_int *hi, random_read_fn_t rf)
{
    mp_int *n_outcomes = mp_sub(hi, lo);
    mp_int *addend = mp_random_upto_fn(n_outcomes, rf);
    mp_int *result = mp_make_sized(hi->nw);
    mp_add_into(result, addend, lo);
    mp_free(addend);
    mp_free(n_outcomes);
    return result;
}
