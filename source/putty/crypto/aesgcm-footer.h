/*
 * Common footer included by every implementation of the AES-GCM MAC.
 *
 * The difficult part of AES-GCM, which is done differently depending
 * on what hardware acceleration is available, is the actual
 * evaluation of a polynomial over GF(2^128) whose coefficients are
 * 128-bit chunks of data. But preparing those chunks in the first
 * place (out of the ciphertext, associated data, and an
 * administrative block containing the lengths of both) is done in the
 * same way no matter what technique is used for the evaluation, so
 * that's centralised into this file, along with as much of the other
 * functionality as possible.
 *
 * This footer file is #included by each implementation, but each one
 * will define its own struct type for the state, so that each alloc
 * function will test sizeof() a different structure, and similarly
 * for free when it zeroes out the state on cleanup.
 *
 * The functions in the source file may be defined as 'inline' so that
 * the functions in here can inline them. The 'coeff' function in
 * particular probably should be, because that's called once per
 * 16-byte block, so eliminating function call overheads is especially
 * useful there.
 *
 * This footer has the following expectations from the source file
 * that #includes it:
 *
 *  - define AESGCM_FLAVOUR to be a fragment of a C identifier that
 *    will be included in all the function names (both the ones
 *    defined in the implementation source file and those in here).
 *    For example purposes below I'll suppose that this is 'foo'.
 *
 *  - define AESGCM_NAME to be a string literal that will be included
 *    in the display name of the implementation.
 *
 *  - define a typedef 'aesgcm_foo' to be the state structure for the
 *    implementation, and inside that structure, expand the macro
 *    AESGCM_COMMON_FIELDS defined in aesgcm.h
 *
 *  - define the following functions:
 *
 *    // Determine whether this implementation is available at run time
 *    static bool aesgcm_foo_available(void);
 *
 *    // Set up the 'key' of the polynomial part of the MAC, that is,
 *    // the value at which the polynomial will be evaluated. 'var' is
 *    // a 16-byte data block in the byte order it comes out of AES.
 *    static void aesgcm_foo_setkey_impl(aesgcm_foo *ctx,
 *                                       const unsigned char *var);
 *
 *    // Set up at the start of evaluating an individual polynomial.
 *    // 'mask' is the 16-byte data block that will be XORed into the
 *    // output value of the polynomial, also in AES byte order. This
 *    // function should store 'mask' in whatever form is most
 *    // convenient, and initialise an accumulator to zero.
 *    static void aesgcm_foo_setup(aesgcm_foo *ctx,
 *                                 const unsigned char *mask);
 *
 *    // Fold in a coefficient of the polynomial, by means of XORing
 *    // it into the accumulator and then multiplying the accumulator
 *    // by the variable passed to setkey_impl() above.
 *    //
 *    // 'coeff' points to the 16-byte block of data that the
 *    // polynomial coefficient will be made out of.
 *    //
 *    // You probably want to mark this function 'inline'.
 *    static void aesgcm_foo_coeff(aesgcm_foo *ctx,
 *                                 const unsigned char *coeff);
 *
 *    // Generate the output MAC, by XORing the accumulator's final
 *    // value with the mask passed to setup() above.
 *    //
 *    // 'output' points to a 16-byte region of memory to write the
 *    // result to.
 *    static void aesgcm_foo_output(aesgcm_foo *ctx,
 *                                  unsigned char *output);
 *
 *  - if allocation of the state structure must be done in a
 *    non-standard way (e.g. x86 needs this to force greater alignment
 *    than standard malloc provides), then #define SPECIAL_ALLOC and
 *    define this additional function:
 *
 *    // Allocate a state structure, zero out its contents, and return it.
 *    static aesgcm_foo *aesgcm_foo_alloc(void);
 *
 *  - if freeing must also be done in an unusual way, #define
 *    SPECIAL_FREE and define this function:
 *
 *    // Zero out the state structure to avoid information leaks if the
 *    // memory is reused, and then free it.
 *    static void aesgcm_foo_free(aesgcm_foo *ctx);
 */

#ifndef AESGCM_FLAVOUR
#error AESGCM_FLAVOUR must be defined by any module including this footer
#endif
#ifndef AESGCM_NAME
#error AESGCM_NAME must be defined by any module including this footer
#endif

#define CONTEXT CAT(aesgcm_, AESGCM_FLAVOUR)
#define PREFIX(name) CAT(CAT(aesgcm_, AESGCM_FLAVOUR), CAT(_, name))

#include "aes.h" // for aes_encrypt_ecb_block

static const char *PREFIX(mac_text_name)(ssh2_mac *mac)
{
    return "AES-GCM (" AESGCM_NAME ")";
}

static void PREFIX(mac_next_message)(ssh2_mac *mac)
{
    CONTEXT *ctx = container_of(mac, CONTEXT, mac);

    /*
     * Make the mask value for a single MAC instance, by encrypting
     * the all-zeroes word using the associated AES instance in its
     * ordinary GCM fashion. This consumes the first block of
     * keystream (with per-block counter equal to 1), leaving the
     * second block of keystream ready to be used on the first block
     * of plaintext.
     */
    unsigned char buf[16];
    memset(buf, 0, 16);
    ssh_cipher_encrypt(ctx->cipher, buf, 16);
    PREFIX(setup)(ctx, buf); /* give it to the implementation to store */
    smemclr(buf, sizeof(buf));
}

static void PREFIX(mac_setkey)(ssh2_mac *mac, ptrlen key)
{
    CONTEXT *ctx = container_of(mac, CONTEXT, mac);

    /*
     * Make the value of the polynomial variable, by encrypting the
     * all-zeroes word using the associated AES instance in the
     * special ECB mode. This is done via the special AES-specific API
     * function encrypt_ecb_block, which doesn't touch the counter
     * state at all.
     */
    unsigned char var[16];
    memset(var, 0, 16);
    aes_encrypt_ecb_block(ctx->cipher, var);
    PREFIX(setkey_impl)(ctx, var);
    smemclr(var, sizeof(var));

    PREFIX(mac_next_message)(mac);     /* set up mask */
}

static void PREFIX(mac_start)(ssh2_mac *mac)
{
    CONTEXT *ctx = container_of(mac, CONTEXT, mac);

    ctx->skipgot = ctx->aadgot = ctx->ciphertextlen = ctx->partlen = 0;
}

/*
 * Handle receiving data via the BinarySink API and turning it into a
 * collection of 16-byte blocks to use as polynomial coefficients.
 *
 * This code is written in a fully general way, which is able to
 * handle an arbitrary number of bytes at the start of the data to
 * ignore completely (necessary for PuTTY integration), and an
 * arbitrary number to treat as associated data, and the rest will be
 * regarded as ciphertext. The stream can be interrupted at any byte
 * position and resumed later; a partial block will be stored as
 * necessary.
 *
 * At the time of writing this comment, in live use most of that
 * generality isn't required: the full data is passed to this function
 * in just one call. But there's no guarantee of that staying true in
 * future, so we do the full deal here just in case, and the test
 * vectors in cryptsuite.py will test it. (And they'll use
 * set_prefix_lengths to set up different configurations from the SSH
 * usage.)
 */
static void PREFIX(mac_BinarySink_write)(
    BinarySink *bs, const void *blkv, size_t len)
{
    CONTEXT *ctx = BinarySink_DOWNCAST(bs, CONTEXT);
    const unsigned char *blk = (const unsigned char *)blkv;

    /*
     * Skip the prefix sequence number used as implicit extra data in
     * SSH MACs. This is not included in the associated data field for
     * GCM, because the IV incrementation policy provides its own
     * sequence numbering.
     */
    if (ctx->skipgot < ctx->skiplen) {
        size_t n = ctx->skiplen - ctx->skipgot;
        if (n > len)
            n = len;
        blk += n;
        len -= n;
        ctx->skipgot += n;

        if (len == 0)
            return;
    }

    /*
     * Read additional authenticated data and fold it in to the MAC.
     */
    while (ctx->aadgot < ctx->aadlen) {
        size_t n = ctx->aadlen - ctx->aadgot;
        if (n > len)
            n = len;

        if (ctx->partlen || n < 16) {
            /*
             * Fold data into the partial block.
             */
            if (n > 16 - ctx->partlen)
                n = 16 - ctx->partlen;
            memcpy(ctx->partblk + ctx->partlen, blk, n);
            ctx->partlen += n;
        } else if (n >= 16) {
            /*
             * Consume a whole block of AAD.
             */
            PREFIX(coeff)(ctx, blk);
            n = 16;
        }
        blk += n;
        len -= n;
        ctx->aadgot += n;

        if (ctx->partlen == 16) {
            PREFIX(coeff)(ctx, ctx->partblk);
            ctx->partlen = 0;
        }

        if (ctx->aadgot == ctx->aadlen && ctx->partlen) {
            memset(ctx->partblk + ctx->partlen, 0, 16 - ctx->partlen);
            PREFIX(coeff)(ctx, ctx->partblk);
            ctx->partlen = 0;
        }

        if (len == 0)
            return;
    }

    /*
     * Read the main ciphertext and fold it in to the MAC.
     */
    while (len > 0) {
        size_t n = len;

        if (ctx->partlen || n < 16) {
            /*
             * Fold data into the partial block.
             */
            if (n > 16 - ctx->partlen)
                n = 16 - ctx->partlen;
            memcpy(ctx->partblk + ctx->partlen, blk, n);
            ctx->partlen += n;
        } else if (n >= 16) {
            /*
             * Consume a whole block of ciphertext.
             */
            PREFIX(coeff)(ctx, blk);
            n = 16;
        }
        blk += n;
        len -= n;
        ctx->ciphertextlen += n;

        if (ctx->partlen == 16) {
            PREFIX(coeff)(ctx, ctx->partblk);
            ctx->partlen = 0;
        }
    }
}

static void PREFIX(mac_genresult)(ssh2_mac *mac, unsigned char *output)
{
    CONTEXT *ctx = container_of(mac, CONTEXT, mac);

    /*
     * Consume any partial block of ciphertext remaining.
     */
    if (ctx->partlen) {
        memset(ctx->partblk + ctx->partlen, 0, 16 - ctx->partlen);
        PREFIX(coeff)(ctx, ctx->partblk);
    }

    /*
     * Consume the final block giving the lengths of the AAD and ciphertext.
     */
    { // WINSCP
    unsigned char blk[16];
    memset(blk, 0, 16);
    PUT_64BIT_MSB_FIRST(blk, ctx->aadlen * 8);
    PUT_64BIT_MSB_FIRST(blk + 8, ctx->ciphertextlen * 8);
    PREFIX(coeff)(ctx, blk);

    /*
     * And call the implementation's output function.
     */
    PREFIX(output)(ctx, output);

    smemclr(blk, sizeof(blk));
    smemclr(ctx->partblk, 16);
    } // WINSCP
}

static ssh2_mac *PREFIX(mac_new)(const ssh2_macalg *alg, ssh_cipher *cipher)
{
    const struct aesgcm_extra *extra = alg->extra;
    if (!check_aesgcm_availability(extra))
        return NULL;

    { // WINSCP
#ifdef SPECIAL_ALLOC
    CONTEXT *ctx = PREFIX(alloc)();
#else
    CONTEXT *ctx = snew(CONTEXT);
    memset(ctx, 0, sizeof(CONTEXT));
#endif

    ctx->mac.vt = alg;
    ctx->cipher = cipher;
    /* Default values for SSH-2, overridable by set_prefix_lengths for
     * testcrypt purposes */
    ctx->skiplen = 4;
    ctx->aadlen = 4;
    BinarySink_INIT(ctx, PREFIX(mac_BinarySink_write));
    BinarySink_DELEGATE_INIT(&ctx->mac, ctx);
    return &ctx->mac;
    } // WINSCP
}

static void PREFIX(set_prefix_lengths)(ssh2_mac *mac, size_t skip, size_t aad)
{
    CONTEXT *ctx = container_of(mac, CONTEXT, mac);
    ctx->skiplen = skip;
    ctx->aadlen = aad;
}

static void PREFIX(mac_free)(ssh2_mac *mac)
{
    CONTEXT *ctx = container_of(mac, CONTEXT, mac);
#ifdef SPECIAL_FREE
    PREFIX(free)(ctx);
#else
    smemclr(ctx, sizeof(*ctx));
    sfree(ctx);
#endif
}

static struct aesgcm_extra_mutable PREFIX(extra_mut);

static const struct aesgcm_extra PREFIX(extra) = {
    /*.check_available =*/ PREFIX(available),
    /*.mut =*/ &PREFIX(extra_mut),
    /*.set_prefix_lengths =*/ PREFIX(set_prefix_lengths),
};

const ssh2_macalg CAT(ssh2_aesgcm_mac_, AESGCM_FLAVOUR) = {
    /*.new =*/ PREFIX(mac_new),
    /*.free =*/ PREFIX(mac_free),
    /*.setkey =*/ PREFIX(mac_setkey),
    /*.start =*/ PREFIX(mac_start),
    /*.genresult =*/ PREFIX(mac_genresult),
    /*.next_message =*/ PREFIX(mac_next_message),
    /*.text_name =*/ PREFIX(mac_text_name),
    /*.name =*/ "",
    /*.etm_name =*/ "", /* Not selectable independently */
    /*.len =*/ 16,
    /*.keylen =*/ 0,
    /*.extra =*/ &PREFIX(extra),
};
