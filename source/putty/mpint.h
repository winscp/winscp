#ifndef PUTTY_MPINT_H
#define PUTTY_MPINT_H

/*
 * PuTTY's multiprecision integer library.
 *
 * This library is written with the aim of avoiding leaking the input
 * numbers via timing and cache side channels. This means avoiding
 * making any control flow change, or deciding the address of any
 * memory access, based on the value of potentially secret input data.
 *
 * But in a library that has to handle numbers of arbitrary size, you
 * can't avoid your control flow depending on the _size_ of the input!
 * So the rule is that an mp_int has a nominal size that need not be
 * its mathematical size: i.e. if you call (say) mp_from_bytes_be to
 * turn an array of 256 bytes into an integer, and all but the last of
 * those bytes is zero, then you get an mp_int which has space for 256
 * bytes of data but just happens to store the value 1. So the
 * _nominal_ sizes of input data - e.g. the size in bits of some
 * public-key modulus - are not considered secret, and control flow is
 * allowed to do what it likes based on those sizes. But the same
 * function, called with the same _nominally sized_ arguments
 * containing different values, should run in the same length of time.
 *
 * When a function returns an 'mp_int *', it is newly allocated to an
 * appropriate nominal size (which, again, depends only on the nominal
 * sizes of the inputs). Other functions have 'into' in their name,
 * and they instead overwrite the contents of an existing mp_int.
 *
 * Functions in this API which return values that are logically
 * boolean return them as 'unsigned' rather than the C99 bool type.
 * That's because C99 bool does an implicit test for non-zero-ness
 * when converting any other integer type to it, which compilers might
 * well implement using data-dependent control flow.
 */

/*
 * Create and destroy mp_ints. A newly created one is initialised to
 * zero. mp_clear also resets an existing number to zero.
 */
mp_int *mp_new(size_t maxbits);
void mp_free(mp_int *);
void mp_clear(mp_int *x);

/*
 * Create mp_ints from various sources: little- and big-endian binary
 * data, an ordinary C unsigned integer type, a decimal or hex string
 * (given either as a ptrlen or a C NUL-terminated string), and
 * another mp_int.
 *
 * The decimal and hex conversion functions have running time
 * dependent on the length of the input data, of course.
 */
mp_int *mp_from_bytes_le(ptrlen bytes);
mp_int *mp_from_bytes_be(ptrlen bytes);
mp_int *mp_from_integer(uintmax_t n);
mp_int *mp_from_decimal_pl(ptrlen decimal);
mp_int *mp_from_decimal(const char *decimal);
mp_int *mp_from_hex_pl(ptrlen hex);
mp_int *mp_from_hex(const char *hex);
mp_int *mp_copy(mp_int *x);

/*
 * A macro for declaring large fixed numbers in source code (such as
 * elliptic curve parameters, or standard Diffie-Hellman moduli). The
 * idea is that you just write something like
 *
 *   mp_int *value = MP_LITERAL(0x19284376283754638745693467245);
 *
 * and it newly allocates you an mp_int containing that number.
 *
 * Internally, the macro argument is stringified and passed to
 * mp_from_hex. That's not as fast as it could be if I had instead set
 * up some kind of mp_from_array_of_uint64_t() function, but I think
 * this system is valuable for the fact that the literal integers
 * appear in a very natural syntax that can be pasted directly out
 * into, say, Python if you want to cross-check a calculation.
 */
static inline mp_int *mp__from_string_literal(const char *lit)
{
    /* Don't call this directly; it's not equipped to deal with
     * hostile data. Use only via the MP_LITERAL macro. */
    if (lit[0] && (lit[1] == 'x' || lit[1] == 'X'))
        return mp_from_hex(lit+2);
    else
        return mp_from_decimal(lit);
}
#define MP_LITERAL(number) mp__from_string_literal(#number)

/*
 * Create an mp_int with the value 2^power.
 */
mp_int *mp_power_2(size_t power);

/*
 * Retrieve the value of a particular bit or byte of an mp_int. The
 * byte / bit index is not considered to be secret data. Out-of-range
 * byte/bit indices are handled cleanly and return zero.
 */
uint8_t mp_get_byte(mp_int *x, size_t byte);
unsigned mp_get_bit(mp_int *x, size_t bit);

/*
 * Retrieve the value of an mp_int as a uintmax_t, assuming it's small
 * enough to fit.
 */
uintmax_t mp_get_integer(mp_int *x);

/*
 * Set an mp_int bit. Again, the bit index is not considered secret.
 * Do not pass an out-of-range index, on pain of assertion failure.
 */
void mp_set_bit(mp_int *x, size_t bit, unsigned val);

/*
 * Return the nominal size of an mp_int, in terms of the maximum
 * number of bytes or bits that can fit in it.
 */
size_t mp_max_bytes(mp_int *x);
size_t mp_max_bits(mp_int *x);

/*
 * Return the _mathematical_ bit count of an mp_int (not its nominal
 * size), i.e. a value n such that 2^{n-1} <= x < 2^n.
 *
 * This function is supposed to run in constant time for a given
 * nominal input size. Of course it's likely that clients of this
 * function will promptly need to use the result as the limit of some
 * loop (e.g. marshalling an mp_int into an SSH packet, which doesn't
 * permit extra prefix zero bytes). But that's up to the caller to
 * decide the safety of.
 */
size_t mp_get_nbits(mp_int *x);

/*
 * Return the value of an mp_int as a decimal or hex string. The
 * result is dynamically allocated, and the caller is responsible for
 * freeing it.
 *
 * These functions should run in constant time for a given nominal
 * input size, even though the exact number of digits returned is
 * variable. They always allocate enough space for the largest output
 * that might be needed, but they don't always fill it.
 */
char *mp_get_decimal(mp_int *x);
char *mp_get_hex(mp_int *x);
char *mp_get_hex_uppercase(mp_int *x);

/*
 * Compare two mp_ints, or compare one mp_int against a C integer. The
 * 'eq' functions return 1 if the two inputs are equal, or 0
 * otherwise; the 'hs' functions return 1 if the first input is >= the
 * second, and 0 otherwise.
 */
unsigned mp_cmp_hs(mp_int *a, mp_int *b);
unsigned mp_cmp_eq(mp_int *a, mp_int *b);
unsigned mp_hs_integer(mp_int *x, uintmax_t n);
unsigned mp_eq_integer(mp_int *x, uintmax_t n);

/*
 * Take the minimum or maximum of two mp_ints, without using a
 * conditional branch.
 */
void mp_min_into(mp_int *r, mp_int *x, mp_int *y);
void mp_max_into(mp_int *r, mp_int *x, mp_int *y);
mp_int *mp_min(mp_int *x, mp_int *y);
mp_int *mp_max(mp_int *x, mp_int *y);

/*
 * Diagnostic function. Writes out x in hex to the supplied stdio
 * stream, preceded by the string 'prefix' and followed by 'suffix'.
 *
 * This is useful to put temporarily into code, but it's also
 * potentially useful to call from a debugger.
 */
void mp_dump(FILE *fp, const char *prefix, mp_int *x, const char *suffix);

/*
 * Overwrite one mp_int with another.
 */
void mp_copy_into(mp_int *dest, mp_int *src);

/*
 * Conditional selection. Overwrites dest with either src0 or src1,
 * according to the value of 'choose_src1'. choose_src1 should be 0 or
 * 1; if it's 1, then dest is set to src1, otherwise src0.
 *
 * The value of choose_src1 is considered to be secret data, so
 * control flow and memory access should not depend on it.
 */
void mp_select_into(mp_int *dest, mp_int *src0, mp_int *src1,
                    unsigned choose_src1);

/*
 * Addition, subtraction and multiplication, either targeting an
 * existing mp_int or making a new one large enough to hold whatever
 * the output might be..
 */
void mp_add_into(mp_int *r, mp_int *a, mp_int *b);
void mp_sub_into(mp_int *r, mp_int *a, mp_int *b);
void mp_mul_into(mp_int *r, mp_int *a, mp_int *b);
mp_int *mp_add(mp_int *x, mp_int *y);
mp_int *mp_sub(mp_int *x, mp_int *y);
mp_int *mp_mul(mp_int *x, mp_int *y);

/*
 * Bitwise operations.
 */
void mp_and_into(mp_int *r, mp_int *a, mp_int *b);
void mp_or_into(mp_int *r, mp_int *a, mp_int *b);
void mp_xor_into(mp_int *r, mp_int *a, mp_int *b);
void mp_bic_into(mp_int *r, mp_int *a, mp_int *b);

/*
 * Addition, subtraction and multiplication with one argument small
 * enough to fit in a C integer. For mp_mul_integer_into, it has to be
 * even smaller than that.
 */
void mp_add_integer_into(mp_int *r, mp_int *a, uintmax_t n);
void mp_sub_integer_into(mp_int *r, mp_int *a, uintmax_t n);
void mp_mul_integer_into(mp_int *r, mp_int *a, uint16_t n);

/*
 * Conditional addition/subtraction. If yes == 1, sets r to a+b or a-b
 * (respectively). If yes == 0, sets r to just a. 'yes' is considered
 * secret data.
 */
void mp_cond_add_into(mp_int *r, mp_int *a, mp_int *b, unsigned yes);
void mp_cond_sub_into(mp_int *r, mp_int *a, mp_int *b, unsigned yes);

/*
 * Swap x0 and x1 if swap == 1, and not if swap == 0. 'swap' is
 * considered secret.
 */
void mp_cond_swap(mp_int *x0, mp_int *x1, unsigned swap);

/*
 * Set x to 0 if clear == 1, and otherwise leave it unchanged. 'clear'
 * is considered secret.
 */
void mp_cond_clear(mp_int *x, unsigned clear);

/*
 * Division. mp_divmod_into divides n by d, and writes the quotient
 * into q and the remainder into r. You can pass either of q and r as
 * NULL if you don't need one of the outputs.
 *
 * mp_div and mp_mod are wrappers that return one or other of those
 * outputs as a freshly allocated mp_int of the appropriate size.
 *
 * Division by zero gives no error, and returns a quotient of 0 and a
 * remainder of n (so as to still satisfy the division identity that
 * n=qd+r).
 */
void mp_divmod_into(mp_int *n, mp_int *d, mp_int *q, mp_int *r);
mp_int *mp_div(mp_int *n, mp_int *d);
mp_int *mp_mod(mp_int *x, mp_int *modulus);

/*
 * Trivially easy special case of mp_mod: reduce a number mod a power
 * of two.
 */
void mp_reduce_mod_2to(mp_int *x, size_t p);

/*
 * Modular inverses. mp_invert computes the inverse of x mod modulus
 * (and will expect the two to be coprime). mp_invert_mod_2to computes
 * the inverse of x mod 2^p, and is a great deal faster.
 */
mp_int *mp_invert_mod_2to(mp_int *x, size_t p);
mp_int *mp_invert(mp_int *x, mp_int *modulus);

/*
 * System for taking square roots modulo an odd prime.
 *
 * In order to do this efficiently, you need to provide an extra piece
 * of information at setup time, namely a number which is not
 * congruent mod p to any square. Given p and that non-square, you can
 * use modsqrt_new to make a context containing all the necessary
 * equipment for actually calculating the square roots, and then you
 * can call mp_modsqrt as many times as you like on that context
 * before freeing it.
 *
 * The output parameter '*success' will be filled in with 1 if the
 * operation was successful, or 0 if the input number doesn't have a
 * square root mod p at all. In the latter case, the returned mp_int
 * will be nonsense and you shouldn't depend on it.
 *
 * ==== WARNING ====
 *
 * This function DOES NOT TREAT THE PRIME MODULUS AS SECRET DATA! It
 * will protect the number you're taking the square root _of_, but not
 * the number you're taking the root of it _mod_.
 *
 * (This is because the algorithm requires a number of loop iterations
 * equal to the number of factors of 2 in p-1. And the expected use of
 * this function is for elliptic-curve point decompression, in which
 * the modulus is always a well-known one written down in standards
 * documents.)
 */
typedef struct ModsqrtContext ModsqrtContext;
ModsqrtContext *modsqrt_new(mp_int *p, mp_int *any_nonsquare_mod_p);
void modsqrt_free(ModsqrtContext *);
mp_int *mp_modsqrt(ModsqrtContext *sc, mp_int *x, unsigned *success);

/*
 * Functions for Montgomery multiplication, a fast technique for doing
 * a long series of modular multiplications all with the same modulus
 * (which has to be odd).
 *
 * You start by calling monty_new to set up a context structure
 * containing all the precomputed bits and pieces needed by the
 * algorithm. Then, any numbers you want to work with must first be
 * transformed into the internal Montgomery representation using
 * monty_import; having done that, you can use monty_mul and monty_pow
 * to operate on them efficiently; and finally, monty_export will
 * convert numbers back out of Montgomery representation to give their
 * ordinary values.
 *
 * Addition and subtraction are not optimised by the Montgomery trick,
 * but monty_add and monty_sub are provided anyway for convenience.
 *
 * There are also monty_invert and monty_modsqrt, which are analogues
 * of mp_invert and mp_modsqrt which take their inputs in Montgomery
 * representation. For mp_modsqrt, the prime modulus of the
 * ModsqrtContext must be the same as the modulus of the MontyContext.
 *
 * The query functions monty_modulus and monty_identity return numbers
 * stored inside the MontyContext, without copying them. The returned
 * pointers are still owned by the MontyContext, so don't free them!
 */
MontyContext *monty_new(mp_int *modulus);
void monty_free(MontyContext *mc);
mp_int *monty_modulus(MontyContext *mc); /* doesn't transfer ownership */
mp_int *monty_identity(MontyContext *mc); /* doesn't transfer ownership */
void monty_import_into(MontyContext *mc, mp_int *r, mp_int *x);
mp_int *monty_import(MontyContext *mc, mp_int *x);
void monty_export_into(MontyContext *mc, mp_int *r, mp_int *x);
mp_int *monty_export(MontyContext *mc, mp_int *x);
void monty_mul_into(MontyContext *, mp_int *r, mp_int *, mp_int *);
mp_int *monty_add(MontyContext *, mp_int *, mp_int *);
mp_int *monty_sub(MontyContext *, mp_int *, mp_int *);
mp_int *monty_mul(MontyContext *, mp_int *, mp_int *);
mp_int *monty_pow(MontyContext *, mp_int *base, mp_int *exponent);
mp_int *monty_invert(MontyContext *, mp_int *);
mp_int *monty_modsqrt(ModsqrtContext *sc, mp_int *mx, unsigned *success);

/*
 * Modular arithmetic functions which don't use an explicit
 * MontyContext. mp_modpow will use one internally (on the assumption
 * that the exponent is likely to be large enough to make it
 * worthwhile); the other three will just do ordinary non-Montgomery-
 * optimised modular reduction. Use mp_modmul if you only have one
 * product to compute; if you have a lot, consider using a
 * MontyContext in the client code.
 */
mp_int *mp_modpow(mp_int *base, mp_int *exponent, mp_int *modulus);
mp_int *mp_modmul(mp_int *x, mp_int *y, mp_int *modulus);
mp_int *mp_modadd(mp_int *x, mp_int *y, mp_int *modulus);
mp_int *mp_modsub(mp_int *x, mp_int *y, mp_int *modulus);

/*
 * Shift an mp_int right by a given number of bits. The shift count is
 * considered to be secret data, and as a result, the algorithm takes
 * O(n log n) time instead of the obvious O(n).
 */
mp_int *mp_rshift_safe(mp_int *x, size_t shift);

/*
 * Shift an mp_int left or right by a fixed number of bits. The shift
 * count is NOT considered to be secret data! Use this if you're
 * always dividing by 2, for example, but don't use it to shift by a
 * variable amount derived from another secret number.
 *
 * The upside is that these functions run in sensible linear time.
 */
void mp_lshift_fixed_into(mp_int *r, mp_int *a, size_t shift);
void mp_rshift_fixed_into(mp_int *r, mp_int *x, size_t shift);
mp_int *mp_rshift_fixed(mp_int *x, size_t shift);

/*
 * Generate a random mp_int.
 *
 * The _function_ definitions here will expect to be given a gen_data
 * function that provides random data. Normally you'd use this using
 * random_read() from random.c, and the macro wrappers automate that.
 *
 * (This is a bit of a dodge to avoid mpint.c having a link-time
 * dependency on random.c, so that programs can link against one but
 * not the other: if a client of this header uses one of these macros
 * then _they_ have link-time dependencies on both modules.)
 *
 * mp_random_bits[_fn] returns an integer 0 <= n < 2^bits.
 * mp_random_in_range[_fn](lo,hi) returns an integer lo <= n < hi.
 */
typedef void (*random_read_fn_t)(void *, size_t);
mp_int *mp_random_bits_fn(size_t bits, random_read_fn_t randfn);
mp_int *mp_random_in_range_fn(
    mp_int *lo_inclusive, mp_int *hi_exclusive, random_read_fn_t randfn);
#define mp_random_bits(bits) mp_random_bits_fn(bits, random_read)
#define mp_random_in_range(lo, hi) mp_random_in_range_fn(lo, hi, random_read)

#endif /* PUTTY_MPINT_H */
