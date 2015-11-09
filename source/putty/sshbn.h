/*
 * sshbn.h: the assorted conditional definitions of BignumInt and
 * multiply/divide macros used throughout the bignum code to treat
 * numbers as arrays of the most conveniently sized word for the
 * target machine. Exported so that other code (e.g. poly1305) can use
 * it too.
 */

/*
 * Usage notes:
 *  * Do not call the DIVMOD_WORD macro with expressions such as array
 *    subscripts, as some implementations object to this (see below).
 *  * Note that none of the division methods below will cope if the
 *    quotient won't fit into BIGNUM_INT_BITS. Callers should be careful
 *    to avoid this case.
 *    If this condition occurs, in the case of the x86 DIV instruction,
 *    an overflow exception will occur, which (according to a correspondent)
 *    will manifest on Windows as something like
 *      0xC0000095: Integer overflow
 *    The C variant won't give the right answer, either.
 */

#if defined __SIZEOF_INT128__
/* gcc and clang both provide a __uint128_t type on 64-bit targets
 * (and, when they do, indicate its presence by the above macro),
 * using the same 'two machine registers' kind of code generation that
 * 32-bit targets use for 64-bit ints. If we have one of these, we can
 * use a 64-bit BignumInt and a 128-bit BignumDblInt. */
typedef __uint64_t BignumInt;
typedef __uint128_t BignumDblInt;
#define BIGNUM_INT_MASK  0xFFFFFFFFFFFFFFFFULL
#define BIGNUM_TOP_BIT   0x8000000000000000ULL
#define BIGNUM_INT_BITS  64
#define MUL_WORD(w1, w2) ((BignumDblInt)w1 * w2)
#define DIVMOD_WORD(q, r, hi, lo, w) do { \
    BignumDblInt n = (((BignumDblInt)hi) << BIGNUM_INT_BITS) | lo; \
    q = n / w; \
    r = n % w; \
} while (0)
#elif defined __GNUC__ && defined __i386__
typedef unsigned long BignumInt;
typedef unsigned long long BignumDblInt;
#define BIGNUM_INT_MASK  0xFFFFFFFFUL
#define BIGNUM_TOP_BIT   0x80000000UL
#define BIGNUM_INT_BITS  32
#define MUL_WORD(w1, w2) ((BignumDblInt)w1 * w2)
#define DIVMOD_WORD(q, r, hi, lo, w) \
    __asm__("div %2" : \
	    "=d" (r), "=a" (q) : \
	    "r" (w), "d" (hi), "a" (lo))
#elif defined _MSC_VER && defined _M_IX86
typedef unsigned __int32 BignumInt;
typedef unsigned __int64 BignumDblInt;
#define BIGNUM_INT_MASK  0xFFFFFFFFUL
#define BIGNUM_TOP_BIT   0x80000000UL
#define BIGNUM_INT_BITS  32
#define MUL_WORD(w1, w2) ((BignumDblInt)w1 * w2)
/* Note: MASM interprets array subscripts in the macro arguments as
 * assembler syntax, which gives the wrong answer. Don't supply them.
 * <http://msdn2.microsoft.com/en-us/library/bf1dw62z.aspx> */
#define DIVMOD_WORD(q, r, hi, lo, w) do { \
    __asm mov edx, hi \
    __asm mov eax, lo \
    __asm div w \
    __asm mov r, edx \
    __asm mov q, eax \
} while(0)
#elif defined _LP64
/* 64-bit architectures can do 32x32->64 chunks at a time */
typedef unsigned int BignumInt;
typedef unsigned long BignumDblInt;
#define BIGNUM_INT_MASK  0xFFFFFFFFU
#define BIGNUM_TOP_BIT   0x80000000U
#define BIGNUM_INT_BITS  32
#define MUL_WORD(w1, w2) ((BignumDblInt)w1 * w2)
#define DIVMOD_WORD(q, r, hi, lo, w) do { \
    BignumDblInt n = (((BignumDblInt)hi) << BIGNUM_INT_BITS) | lo; \
    q = n / w; \
    r = n % w; \
} while (0)
#elif defined _LLP64
/* 64-bit architectures in which unsigned long is 32 bits, not 64 */
typedef unsigned long BignumInt;
typedef unsigned long long BignumDblInt;
#define BIGNUM_INT_MASK  0xFFFFFFFFUL
#define BIGNUM_TOP_BIT   0x80000000UL
#define BIGNUM_INT_BITS  32
#define MUL_WORD(w1, w2) ((BignumDblInt)w1 * w2)
#define DIVMOD_WORD(q, r, hi, lo, w) do { \
    BignumDblInt n = (((BignumDblInt)hi) << BIGNUM_INT_BITS) | lo; \
    q = n / w; \
    r = n % w; \
} while (0)
#else
/* Fallback for all other cases */
typedef unsigned short BignumInt;
typedef unsigned long BignumDblInt;
#define BIGNUM_INT_MASK  0xFFFFU
#define BIGNUM_TOP_BIT   0x8000U
#define BIGNUM_INT_BITS  16
#define MUL_WORD(w1, w2) ((BignumDblInt)w1 * w2)
#define DIVMOD_WORD(q, r, hi, lo, w) do { \
    BignumDblInt n = (((BignumDblInt)hi) << BIGNUM_INT_BITS) | lo; \
    q = n / w; \
    r = n % w; \
} while (0)
#endif

#define BIGNUM_INT_BYTES (BIGNUM_INT_BITS / 8)
