/*
 * sshbn.h: the assorted conditional definitions of BignumInt and
 * multiply macros used throughout the bignum code to treat numbers as
 * arrays of the most conveniently sized word for the target machine.
 * Exported so that other code (e.g. poly1305) can use it too.
 *
 * This file must export, in whatever ifdef branch it ends up in:
 *
 *  - two types: 'BignumInt' and 'BignumCarry'. BignumInt is an
 *    unsigned integer type which will be used as the base word size
 *    for all bignum operations. BignumCarry is an unsigned integer
 *    type used to hold the carry flag taken as input and output by
 *    the BignumADC macro (see below).
 *
 *  - four constant macros: BIGNUM_INT_BITS, BIGNUM_INT_BYTES,
 *    BIGNUM_TOP_BIT, BIGNUM_INT_MASK. These should be more or less
 *    self-explanatory, but just in case, they give the number of bits
 *    in BignumInt, the number of bytes that works out to, the
 *    BignumInt value consisting of only the top bit, and the
 *    BignumInt value with all bits set.
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
 * the value BIGNUM_INT_BITS. The other three constant macros are
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

#if defined __SIZEOF_INT128__

  /*
   * 64-bit BignumInt using gcc/clang style 128-bit BignumDblInt.
   *
   * gcc and clang both provide a __uint128_t type on 64-bit targets
   * (and, when they do, indicate its presence by the above macro),
   * using the same 'two machine registers' kind of code generation
   * that 32-bit targets use for 64-bit ints.
   */

  typedef unsigned long long BignumInt;
  #define BIGNUM_INT_BITS 64
  #define DEFINE_BIGNUMDBLINT typedef __uint128_t BignumDblInt

#elif defined _MSC_VER && defined _M_AMD64

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
  #define BIGNUM_INT_BITS 64
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

#elif defined __GNUC__ || defined _LLP64 || __STDC__ >= 199901L

  /* 32-bit BignumInt, using C99 unsigned long long as BignumDblInt */

  typedef unsigned int BignumInt;
  #define BIGNUM_INT_BITS 32
  #define DEFINE_BIGNUMDBLINT typedef unsigned long long BignumDblInt

#elif defined _MSC_VER && defined _M_IX86

  /* 32-bit BignumInt, using Visual Studio __int64 as BignumDblInt */

  typedef unsigned int BignumInt;
  #define BIGNUM_INT_BITS  32
  #define DEFINE_BIGNUMDBLINT typedef unsigned __int64 BignumDblInt

#elif defined _LP64

  /*
   * 32-bit BignumInt, using unsigned long itself as BignumDblInt.
   *
   * Only for platforms where long is 64 bits, of course.
   */

  typedef unsigned int BignumInt;
  #define BIGNUM_INT_BITS  32
  #define DEFINE_BIGNUMDBLINT typedef unsigned long BignumDblInt

#else

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
  #define BIGNUM_INT_BITS  16
  #define DEFINE_BIGNUMDBLINT typedef unsigned long BignumDblInt

#endif

/*
 * Common code across all branches of that ifdef: define the three
 * easy constant macros in terms of BIGNUM_INT_BITS.
 */
#define BIGNUM_INT_BYTES (BIGNUM_INT_BITS / 8)
#define BIGNUM_TOP_BIT (((BignumInt)1) << (BIGNUM_INT_BITS-1))
#define BIGNUM_INT_MASK (BIGNUM_TOP_BIT | (BIGNUM_TOP_BIT-1))

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
