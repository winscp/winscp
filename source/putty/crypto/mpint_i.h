/*
 * mpint_i.h: definitions used internally by the bignum code, and
 * also a few other vaguely-bignum-like places.
 */

/* ----------------------------------------------------------------------
 * The assorted conditional definitions of BignumInt and multiply
 * macros used throughout the bignum code to treat numbers as arrays
 * of the most conveniently sized word for the target machine.
 * Exported so that other code (e.g. poly1305) can use it too.
 *
 * This code must export, in whatever ifdef branch it ends up in:
 *
 *  - two types: 'BignumInt' and 'BignumCarry'. BignumInt is an
 *    unsigned integer type which will be used as the base word size
 *    for all bignum operations. BignumCarry is an unsigned integer
 *    type used to hold the carry flag taken as input and output by
 *    the BignumADC macro (see below).
 *
 *  - five constant macros:
 *     + BIGNUM_INT_BITS, the number of bits in BignumInt,
 *     + BIGNUM_INT_BYTES, the number of bytes that works out to
 *     + BIGNUM_TOP_BIT, the BignumInt value consisting of only the top bit
 *     + BIGNUM_INT_MASK, the BignumInt value with all bits set
 *     + BIGNUM_INT_BITS_BITS, log to the base 2 of BIGNUM_INT_BITS.
 *
 *  - four statement macros: BignumADC, BignumMUL, BignumMULADD,
 *    BignumMULADD2. These do various kinds of multi-word arithmetic,
 *    and all produce two output values.
 *     * BignumADC(ret,retc,a,b,c) takes input BignumInt values a,b
 *       and a BignumCarry c, and outputs a BignumInt ret = a+b+c and
 *       a BignumCarry retc which is the carry off the top of that
 *       addition.
 *     * BignumMUL(rh,rl,a,b) returns the two halves of the
 *       double-width product a*b.
 *     * BignumMULADD(rh,rl,a,b,addend) returns the two halves of the
 *       double-width value a*b + addend.
 *     * BignumMULADD2(rh,rl,a,b,addend1,addend2) returns the two
 *       halves of the double-width value a*b + addend1 + addend2.
 *
 * Every branch of the main ifdef below defines the type BignumInt and
 * the value BIGNUM_INT_BITS_BITS. The other constant macros are
 * filled in by common code further down.
 *
 * Most branches also define a macro DEFINE_BIGNUMDBLINT containing a
 * typedef statement which declares a type _twice_ the length of a
 * BignumInt. This causes the common code further down to produce a
 * default implementation of the four statement macros in terms of
 * that double-width type, and also to defined BignumCarry to be
 * BignumInt.
 *
 * However, if a particular compile target does not have a type twice
 * the length of the BignumInt you want to use but it does provide
 * some alternative means of doing add-with-carry and double-word
 * multiply, then the ifdef branch in question can just define
 * BignumCarry and the four statement macros itself, and that's fine
 * too.
 */

/* You can lower the BignumInt size by defining BIGNUM_OVERRIDE on the
 * command line to be your chosen max value of BIGNUM_INT_BITS_BITS */
#if defined BIGNUM_OVERRIDE
#define BB_OK(b) ((b) <= BIGNUM_OVERRIDE)
#else
#define BB_OK(b) (1)
#endif

#if defined __SIZEOF_INT128__ && BB_OK(6)

  /*
   * 64-bit BignumInt using gcc/clang style 128-bit BignumDblInt.
   *
   * gcc and clang both provide a __uint128_t type on 64-bit targets
   * (and, when they do, indicate its presence by the above macro),
   * using the same 'two machine registers' kind of code generation
   * that 32-bit targets use for 64-bit ints.
   */

  typedef unsigned long long BignumInt;
  #define BIGNUM_INT_BITS_BITS 6
  #define DEFINE_BIGNUMDBLINT typedef __uint128_t BignumDblInt

#elif defined _MSC_VER && defined _M_AMD64 && BB_OK(6)

  /*
   * 64-bit BignumInt, using Visual Studio x86-64 compiler intrinsics.
   *
   * 64-bit Visual Studio doesn't provide very much in the way of help
   * here: there's no int128 type, and also no inline assembler giving
   * us direct access to the x86-64 MUL or ADC instructions. However,
   * there are compiler intrinsics giving us that access, so we can
   * use those - though it turns out we have to be a little careful,
   * since they seem to generate wrong code if their pointer-typed
   * output parameters alias their inputs. Hence all the internal temp
   * variables inside the macros.
   */

  #include <intrin.h>
  typedef unsigned char BignumCarry; /* the type _addcarry_u64 likes to use */
  typedef unsigned __int64 BignumInt;
  #define BIGNUM_INT_BITS_BITS 6
  #define BignumADC(ret, retc, a, b, c) do                \
      {                                                   \
          BignumInt ADC_tmp;                              \
          (retc) = _addcarry_u64(c, a, b, &ADC_tmp);      \
          (ret) = ADC_tmp;                                \
      } while (0)
  #define BignumMUL(rh, rl, a, b) do              \
      {                                           \
          BignumInt MULADD_hi;                    \
          (rl) = _umul128(a, b, &MULADD_hi);      \
          (rh) = MULADD_hi;                       \
      } while (0)
  #define BignumMULADD(rh, rl, a, b, addend) do                           \
      {                                                                   \
          BignumInt MULADD_lo, MULADD_hi;                                 \
          MULADD_lo = _umul128(a, b, &MULADD_hi);                         \
          MULADD_hi += _addcarry_u64(0, MULADD_lo, (addend), &(rl));     \
          (rh) = MULADD_hi;                                               \
      } while (0)
  #define BignumMULADD2(rh, rl, a, b, addend1, addend2) do                \
      {                                                                   \
          BignumInt MULADD_lo1, MULADD_lo2, MULADD_hi;                    \
          MULADD_lo1 = _umul128(a, b, &MULADD_hi);                        \
          MULADD_hi += _addcarry_u64(0, MULADD_lo1, (addend1), &MULADD_lo2); \
          MULADD_hi += _addcarry_u64(0, MULADD_lo2, (addend2), &(rl));    \
          (rh) = MULADD_hi;                                               \
      } while (0)

#elif (defined __GNUC__ || defined _LLP64 || __STDC__ >= 199901L) && BB_OK(5)

  /* 32-bit BignumInt, using C99 unsigned long long as BignumDblInt */

  typedef unsigned int BignumInt;
  #define BIGNUM_INT_BITS_BITS 5
  #define DEFINE_BIGNUMDBLINT typedef unsigned long long BignumDblInt

#elif defined _MSC_VER && BB_OK(5) || defined(MPEXT)

  /* 32-bit BignumInt, using Visual Studio __int64 as BignumDblInt */

  typedef unsigned int BignumInt;
  #define BIGNUM_INT_BITS_BITS 5
  #define DEFINE_BIGNUMDBLINT typedef unsigned __int64 BignumDblInt

#elif defined _LP64 && BB_OK(5)

  /*
   * 32-bit BignumInt, using unsigned long itself as BignumDblInt.
   *
   * Only for platforms where long is 64 bits, of course.
   */

  typedef unsigned int BignumInt;
  #define BIGNUM_INT_BITS_BITS 5
  #define DEFINE_BIGNUMDBLINT typedef unsigned long BignumDblInt

#elif BB_OK(4)

  /*
   * 16-bit BignumInt, using unsigned long as BignumDblInt.
   *
   * This is the final fallback for real emergencies: C89 guarantees
   * unsigned short/long to be at least the required sizes, so this
   * should work on any C implementation at all. But it'll be
   * noticeably slow, so if you find yourself in this case you
   * probably want to move heaven and earth to find an alternative!
   */

  typedef unsigned short BignumInt;
  #define BIGNUM_INT_BITS_BITS 4
  #define DEFINE_BIGNUMDBLINT typedef unsigned long BignumDblInt

#else

  /* Should only get here if BB_OK(4) evaluated false, i.e. the
   * command line defined BIGNUM_OVERRIDE to an absurdly small
   * value. */
  #error Must define BIGNUM_OVERRIDE to at least 4

#endif

#undef BB_OK

/*
 * Common code across all branches of that ifdef: define all the
 * easy constant macros in terms of BIGNUM_INT_BITS_BITS.
 */
#define BIGNUM_INT_BITS (1 << BIGNUM_INT_BITS_BITS)
#define BIGNUM_INT_BYTES (BIGNUM_INT_BITS / 8)
#define BIGNUM_TOP_BIT (((BignumInt)1) << (BIGNUM_INT_BITS-1))
#define BIGNUM_INT_MASK (BIGNUM_TOP_BIT | (BIGNUM_TOP_BIT-1))

/*
 * Just occasionally, we might need a GET_nnBIT_xSB_FIRST macro to
 * operate on whatever BignumInt is.
 */
#if BIGNUM_INT_BITS_BITS == 4
#define GET_BIGNUMINT_MSB_FIRST GET_16BIT_MSB_FIRST
#define GET_BIGNUMINT_LSB_FIRST GET_16BIT_LSB_FIRST
#define PUT_BIGNUMINT_MSB_FIRST PUT_16BIT_MSB_FIRST
#define PUT_BIGNUMINT_LSB_FIRST PUT_16BIT_LSB_FIRST
#elif BIGNUM_INT_BITS_BITS == 5
#define GET_BIGNUMINT_MSB_FIRST GET_32BIT_MSB_FIRST
#define GET_BIGNUMINT_LSB_FIRST GET_32BIT_LSB_FIRST
#define PUT_BIGNUMINT_MSB_FIRST PUT_32BIT_MSB_FIRST
#define PUT_BIGNUMINT_LSB_FIRST PUT_32BIT_LSB_FIRST
#elif BIGNUM_INT_BITS_BITS == 6
#define GET_BIGNUMINT_MSB_FIRST GET_64BIT_MSB_FIRST
#define GET_BIGNUMINT_LSB_FIRST GET_64BIT_LSB_FIRST
#define PUT_BIGNUMINT_MSB_FIRST PUT_64BIT_MSB_FIRST
#define PUT_BIGNUMINT_LSB_FIRST PUT_64BIT_LSB_FIRST
#else
  #error Ran out of options for GET_BIGNUMINT_xSB_FIRST
#endif

/*
 * Common code across _most_ branches of the ifdef: define a set of
 * statement macros in terms of the BignumDblInt type provided. In
 * this case, we also define BignumCarry to be the same thing as
 * BignumInt, for simplicity.
 */
#ifdef DEFINE_BIGNUMDBLINT

  typedef BignumInt BignumCarry;
  #define BignumADC(ret, retc, a, b, c) do                        \
      {                                                           \
          DEFINE_BIGNUMDBLINT;                                    \
          BignumDblInt ADC_temp = (BignumInt)(a);                 \
          ADC_temp += (BignumInt)(b);                             \
          ADC_temp += (c);                                        \
          (ret) = (BignumInt)ADC_temp;                            \
          (retc) = (BignumCarry)(ADC_temp >> BIGNUM_INT_BITS);    \
      } while (0)

  #define BignumMUL(rh, rl, a, b) do                              \
      {                                                           \
          DEFINE_BIGNUMDBLINT;                                    \
          BignumDblInt MUL_temp = (BignumInt)(a);                 \
          MUL_temp *= (BignumInt)(b);                             \
          (rh) = (BignumInt)(MUL_temp >> BIGNUM_INT_BITS);        \
          (rl) = (BignumInt)(MUL_temp);                           \
      } while (0)

  #define BignumMULADD(rh, rl, a, b, addend) do                   \
      {                                                           \
          DEFINE_BIGNUMDBLINT;                                    \
          BignumDblInt MUL_temp = (BignumInt)(a);                 \
          MUL_temp *= (BignumInt)(b);                             \
          MUL_temp += (BignumInt)(addend);                        \
          (rh) = (BignumInt)(MUL_temp >> BIGNUM_INT_BITS);        \
          (rl) = (BignumInt)(MUL_temp);                           \
      } while (0)

  #define BignumMULADD2(rh, rl, a, b, addend1, addend2) do        \
      {                                                           \
          DEFINE_BIGNUMDBLINT;                                    \
          BignumDblInt MUL_temp = (BignumInt)(a);                 \
          MUL_temp *= (BignumInt)(b);                             \
          MUL_temp += (BignumInt)(addend1);                       \
          MUL_temp += (BignumInt)(addend2);                       \
          (rh) = (BignumInt)(MUL_temp >> BIGNUM_INT_BITS);        \
          (rl) = (BignumInt)(MUL_temp);                           \
      } while (0)

#endif /* DEFINE_BIGNUMDBLINT */

/* ----------------------------------------------------------------------
 * Data structures used inside mpint.c.
 */

struct mp_int {
    size_t nw;
    BignumInt *w;
};

struct MontyContext {
    /*
     * The actual modulus.
     */
    mp_int *m;

    /*
     * Montgomery multiplication works by selecting a value r > m,
     * coprime to m, which is really easy to divide by. In binary
     * arithmetic, that means making it a power of 2; in fact we make
     * it a whole number of BignumInt.
     *
     * We don't store r directly as an mp_int (there's no need). But
     * its value is 2^rbits; we also store rw = rbits/BIGNUM_INT_BITS
     * (the corresponding word offset within an mp_int).
     *
     * pw is the number of words needed to store an mp_int you're
     * doing reduction on: it has to be big enough to hold the sum of
     * an input value up to m^2 plus an extra addend up to m*r.
     */
    size_t rbits, rw, pw;

    /*
     * The key step in Montgomery reduction requires the inverse of -m
     * mod r.
     */
    mp_int *minus_minv_mod_r;

    /*
     * r^1, r^2 and r^3 mod m, which are used for various purposes.
     *
     * (Annoyingly, this is one of the rare cases where it would have
     * been nicer to have a Pascal-style 1-indexed array. I couldn't
     * _quite_ bring myself to put a gratuitous zero element in here.
     * So you just have to live with getting r^k by taking the [k-1]th
     * element of this array.)
     */
    mp_int *powers_of_r_mod_m[3];

    /*
     * Persistent scratch space from which monty_* functions can
     * allocate storage for intermediate values.
     */
    // WINSCP: this is volatile member that needs to be guarded for thread-safety
    // We want to know if it starts being used elsewhre/otherwise.
    mp_int *winscp_guarded_scratch;
};

/* Functions shared between mpint.c and mpunsafe.c */
mp_int *mp_make_sized(size_t nw);
