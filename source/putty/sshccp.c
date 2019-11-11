/*
 * ChaCha20-Poly1305 Implementation for SSH-2
 *
 * Protocol spec:
 *  http://cvsweb.openbsd.org/cgi-bin/cvsweb/src/usr.bin/ssh/PROTOCOL.chacha20poly1305?rev=1.2&content-type=text/x-cvsweb-markup
 *
 * ChaCha20 spec:
 *  http://cr.yp.to/chacha/chacha-20080128.pdf
 *
 * Salsa20 spec:
 *  http://cr.yp.to/snuffle/spec.pdf
 *
 * Poly1305-AES spec:
 *  http://cr.yp.to/mac/poly1305-20050329.pdf
 *
 * The nonce for the Poly1305 is the second part of the key output
 * from the first round of ChaCha20. This removes the AES requirement.
 * This is undocumented!
 *
 * This has an intricate link between the cipher and the MAC. The
 * keying of both is done in by the cipher and setting of the IV is
 * done by the MAC. One cannot operate without the other. The
 * configuration of the ssh_cipheralg structure ensures that the MAC is
 * set (and others ignored) if this cipher is chosen.
 *
 * This cipher also encrypts the length using a different
 * instantiation of the cipher using a different key and IV made from
 * the sequence number which is passed in addition when calling
 * encrypt/decrypt on it.
 */

#include "ssh.h"
#include "mpint_i.h"

#ifndef INLINE
#define INLINE
#endif

/* ChaCha20 implementation, only supporting 256-bit keys */

/* State for each ChaCha20 instance */
struct chacha20 {
    /* Current context, usually with the count incremented
     * 0-3 are the static constant
     * 4-11 are the key
     * 12-13 are the counter
     * 14-15 are the IV */
    uint32_t state[16];
    /* The output of the state above ready to xor */
    unsigned char current[64];
    /* The index of the above currently used to allow a true streaming cipher */
    int currentIndex;
};

static INLINE void chacha20_round(struct chacha20 *ctx)
{
    int i;
    uint32_t copy[16];

    /* Take a copy */
    memcpy(copy, ctx->state, sizeof(copy));

    /* A circular rotation for a 32bit number */
#define rotl(x, shift) x = ((x << shift) | (x >> (32 - shift)))

    /* What to do for each quarter round operation */
#define qrop(a, b, c, d)                        \
    copy[a] += copy[b];                         \
    copy[c] ^= copy[a];                         \
    rotl(copy[c], d)

    /* A quarter round */
#define quarter(a, b, c, d)                     \
    qrop(a, b, d, 16);                          \
    qrop(c, d, b, 12);                          \
    qrop(a, b, d, 8);                           \
    qrop(c, d, b, 7)

    /* Do 20 rounds, in pairs because every other is different */
    for (i = 0; i < 20; i += 2) {
        /* A round */
        quarter(0, 4, 8, 12);
        quarter(1, 5, 9, 13);
        quarter(2, 6, 10, 14);
        quarter(3, 7, 11, 15);
        /* Another slightly different round */
        quarter(0, 5, 10, 15);
        quarter(1, 6, 11, 12);
        quarter(2, 7, 8, 13);
        quarter(3, 4, 9, 14);
    }

    /* Dump the macros, don't need them littering */
#undef rotl
#undef qrop
#undef quarter

    /* Add the initial state */
    for (i = 0; i < 16; ++i) {
        copy[i] += ctx->state[i];
    }

    /* Update the content of the xor buffer */
    for (i = 0; i < 16; ++i) {
        ctx->current[i * 4 + 0] = copy[i] >> 0;
        ctx->current[i * 4 + 1] = copy[i] >> 8;
        ctx->current[i * 4 + 2] = copy[i] >> 16;
        ctx->current[i * 4 + 3] = copy[i] >> 24;
    }
    /* State full, reset pointer to beginning */
    ctx->currentIndex = 0;
    smemclr(copy, sizeof(copy));

    /* Increment round counter */
    ++ctx->state[12];
    /* Check for overflow, not done in one line so the 32 bits are chopped by the type */
    if (!(uint32_t)(ctx->state[12])) {
        ++ctx->state[13];
    }
}

/* Initialise context with 256bit key */
static void chacha20_key(struct chacha20 *ctx, const unsigned char *key)
{
    static const char constant[16] = "expand 32-byte k";

    /* Add the fixed string to the start of the state */
    ctx->state[0] = GET_32BIT_LSB_FIRST(constant + 0);
    ctx->state[1] = GET_32BIT_LSB_FIRST(constant + 4);
    ctx->state[2] = GET_32BIT_LSB_FIRST(constant + 8);
    ctx->state[3] = GET_32BIT_LSB_FIRST(constant + 12);

    /* Add the key */
    ctx->state[4]  = GET_32BIT_LSB_FIRST(key + 0);
    ctx->state[5]  = GET_32BIT_LSB_FIRST(key + 4);
    ctx->state[6]  = GET_32BIT_LSB_FIRST(key + 8);
    ctx->state[7]  = GET_32BIT_LSB_FIRST(key + 12);
    ctx->state[8]  = GET_32BIT_LSB_FIRST(key + 16);
    ctx->state[9]  = GET_32BIT_LSB_FIRST(key + 20);
    ctx->state[10] = GET_32BIT_LSB_FIRST(key + 24);
    ctx->state[11] = GET_32BIT_LSB_FIRST(key + 28);

    /* New key, dump context */
    ctx->currentIndex = 64;
}

static void chacha20_iv(struct chacha20 *ctx, const unsigned char *iv)
{
    ctx->state[12] = 0;
    ctx->state[13] = 0;
    ctx->state[14] = GET_32BIT_MSB_FIRST(iv);
    ctx->state[15] = GET_32BIT_MSB_FIRST(iv + 4);

    /* New IV, dump context */
    ctx->currentIndex = 64;
}

static void chacha20_encrypt(struct chacha20 *ctx, unsigned char *blk, int len)
{
    while (len) {
        /* If we don't have any state left, then cycle to the next */
        if (ctx->currentIndex >= 64) {
            chacha20_round(ctx);
        }

        /* Do the xor while there's some state left and some plaintext left */
        while (ctx->currentIndex < 64 && len) {
            *blk++ ^= ctx->current[ctx->currentIndex++];
            --len;
        }
    }
}

/* Decrypt is encrypt... It's xor against a PRNG... */
static INLINE void chacha20_decrypt(struct chacha20 *ctx,
                                    unsigned char *blk, int len)
{
    chacha20_encrypt(ctx, blk, len);
}

/* Poly1305 implementation (no AES, nonce is not encrypted) */

#define NWORDS ((130 + BIGNUM_INT_BITS-1) / BIGNUM_INT_BITS)
typedef struct bigval {
    BignumInt w[NWORDS];
} bigval;

static void bigval_clear(bigval *r)
{
    int i;
    for (i = 0; i < NWORDS; i++)
        r->w[i] = 0;
}

static void bigval_import_le(bigval *r, const void *vdata, int len)
{
    const unsigned char *data = (const unsigned char *)vdata;
    int i;
    bigval_clear(r);
    for (i = 0; i < len; i++)
        r->w[i / BIGNUM_INT_BYTES] |=
            (BignumInt)data[i] << (8 * (i % BIGNUM_INT_BYTES));
}

static void bigval_export_le(const bigval *r, void *vdata, int len)
{
    unsigned char *data = (unsigned char *)vdata;
    int i;
    for (i = 0; i < len; i++)
        data[i] = r->w[i / BIGNUM_INT_BYTES] >> (8 * (i % BIGNUM_INT_BYTES));
}

/*
 * Core functions to do arithmetic mod p = 2^130-5. The whole
 * collection of these, up to and including the surrounding #if, are
 * generated automatically for various sizes of BignumInt by
 * contrib/make1305.py.
 */

#if BIGNUM_INT_BITS == 16

static void bigval_add(bigval *r, const bigval *a, const bigval *b)
{
    BignumInt v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14;
    BignumInt v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26;
    BignumCarry carry;

    v0 = a->w[0];
    v1 = a->w[1];
    v2 = a->w[2];
    v3 = a->w[3];
    v4 = a->w[4];
    v5 = a->w[5];
    v6 = a->w[6];
    v7 = a->w[7];
    v8 = a->w[8];
    v9 = b->w[0];
    v10 = b->w[1];
    v11 = b->w[2];
    v12 = b->w[3];
    v13 = b->w[4];
    v14 = b->w[5];
    v15 = b->w[6];
    v16 = b->w[7];
    v17 = b->w[8];
    BignumADC(v18, carry, v0, v9, 0);
    BignumADC(v19, carry, v1, v10, carry);
    BignumADC(v20, carry, v2, v11, carry);
    BignumADC(v21, carry, v3, v12, carry);
    BignumADC(v22, carry, v4, v13, carry);
    BignumADC(v23, carry, v5, v14, carry);
    BignumADC(v24, carry, v6, v15, carry);
    BignumADC(v25, carry, v7, v16, carry);
    v26 = v8 + v17 + carry;
    r->w[0] = v18;
    r->w[1] = v19;
    r->w[2] = v20;
    r->w[3] = v21;
    r->w[4] = v22;
    r->w[5] = v23;
    r->w[6] = v24;
    r->w[7] = v25;
    r->w[8] = v26;
}

static void bigval_mul_mod_p(bigval *r, const bigval *a, const bigval *b)
{
    BignumInt v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14;
    BignumInt v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27;
    BignumInt v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40;
    BignumInt v41, v42, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53;
    BignumInt v54, v55, v56, v57, v58, v59, v60, v61, v62, v63, v64, v65, v66;
    BignumInt v67, v68, v69, v70, v71, v72, v73, v74, v75, v76, v77, v78, v79;
    BignumInt v80, v81, v82, v83, v84, v85, v86, v87, v88, v89, v90, v91, v92;
    BignumInt v93, v94, v95, v96, v97, v98, v99, v100, v101, v102, v103, v104;
    BignumInt v105, v106, v107, v108, v109, v110, v111, v112, v113, v114;
    BignumInt v115, v116, v117, v118, v119, v120, v121, v122, v123, v124;
    BignumInt v125, v126, v127, v128, v129, v130, v131, v132, v133, v134;
    BignumInt v135, v136, v137, v138, v139, v140, v141, v142, v143, v144;
    BignumInt v145, v146, v147, v148, v149, v150, v151, v152, v153, v154;
    BignumInt v155, v156, v157, v158, v159, v160, v161, v162, v163, v164;
    BignumInt v165, v166, v167, v168, v169, v170, v171, v172, v173, v174;
    BignumInt v175, v176, v177, v178, v180, v181, v182, v183, v184, v185;
    BignumInt v186, v187, v188, v189, v190, v191, v192, v193, v194, v195;
    BignumInt v196, v197, v198, v199, v200, v201, v202, v203, v204, v205;
    BignumInt v206, v207, v208, v210, v212, v213, v214, v215, v216, v217;
    BignumInt v218, v219, v220, v221, v222, v223, v224, v225, v226, v227;
    BignumInt v228, v229;
    BignumCarry carry;

    v0 = a->w[0];
    v1 = a->w[1];
    v2 = a->w[2];
    v3 = a->w[3];
    v4 = a->w[4];
    v5 = a->w[5];
    v6 = a->w[6];
    v7 = a->w[7];
    v8 = a->w[8];
    v9 = b->w[0];
    v10 = b->w[1];
    v11 = b->w[2];
    v12 = b->w[3];
    v13 = b->w[4];
    v14 = b->w[5];
    v15 = b->w[6];
    v16 = b->w[7];
    v17 = b->w[8];
    BignumMUL(v19, v18, v0, v9);
    BignumMULADD(v21, v20, v0, v10, v19);
    BignumMULADD(v23, v22, v0, v11, v21);
    BignumMULADD(v25, v24, v0, v12, v23);
    BignumMULADD(v27, v26, v0, v13, v25);
    BignumMULADD(v29, v28, v0, v14, v27);
    BignumMULADD(v31, v30, v0, v15, v29);
    BignumMULADD(v33, v32, v0, v16, v31);
    BignumMULADD(v35, v34, v0, v17, v33);
    BignumMULADD(v37, v36, v1, v9, v20);
    BignumMULADD2(v39, v38, v1, v10, v22, v37);
    BignumMULADD2(v41, v40, v1, v11, v24, v39);
    BignumMULADD2(v43, v42, v1, v12, v26, v41);
    BignumMULADD2(v45, v44, v1, v13, v28, v43);
    BignumMULADD2(v47, v46, v1, v14, v30, v45);
    BignumMULADD2(v49, v48, v1, v15, v32, v47);
    BignumMULADD2(v51, v50, v1, v16, v34, v49);
    BignumMULADD2(v53, v52, v1, v17, v35, v51);
    BignumMULADD(v55, v54, v2, v9, v38);
    BignumMULADD2(v57, v56, v2, v10, v40, v55);
    BignumMULADD2(v59, v58, v2, v11, v42, v57);
    BignumMULADD2(v61, v60, v2, v12, v44, v59);
    BignumMULADD2(v63, v62, v2, v13, v46, v61);
    BignumMULADD2(v65, v64, v2, v14, v48, v63);
    BignumMULADD2(v67, v66, v2, v15, v50, v65);
    BignumMULADD2(v69, v68, v2, v16, v52, v67);
    BignumMULADD2(v71, v70, v2, v17, v53, v69);
    BignumMULADD(v73, v72, v3, v9, v56);
    BignumMULADD2(v75, v74, v3, v10, v58, v73);
    BignumMULADD2(v77, v76, v3, v11, v60, v75);
    BignumMULADD2(v79, v78, v3, v12, v62, v77);
    BignumMULADD2(v81, v80, v3, v13, v64, v79);
    BignumMULADD2(v83, v82, v3, v14, v66, v81);
    BignumMULADD2(v85, v84, v3, v15, v68, v83);
    BignumMULADD2(v87, v86, v3, v16, v70, v85);
    BignumMULADD2(v89, v88, v3, v17, v71, v87);
    BignumMULADD(v91, v90, v4, v9, v74);
    BignumMULADD2(v93, v92, v4, v10, v76, v91);
    BignumMULADD2(v95, v94, v4, v11, v78, v93);
    BignumMULADD2(v97, v96, v4, v12, v80, v95);
    BignumMULADD2(v99, v98, v4, v13, v82, v97);
    BignumMULADD2(v101, v100, v4, v14, v84, v99);
    BignumMULADD2(v103, v102, v4, v15, v86, v101);
    BignumMULADD2(v105, v104, v4, v16, v88, v103);
    BignumMULADD2(v107, v106, v4, v17, v89, v105);
    BignumMULADD(v109, v108, v5, v9, v92);
    BignumMULADD2(v111, v110, v5, v10, v94, v109);
    BignumMULADD2(v113, v112, v5, v11, v96, v111);
    BignumMULADD2(v115, v114, v5, v12, v98, v113);
    BignumMULADD2(v117, v116, v5, v13, v100, v115);
    BignumMULADD2(v119, v118, v5, v14, v102, v117);
    BignumMULADD2(v121, v120, v5, v15, v104, v119);
    BignumMULADD2(v123, v122, v5, v16, v106, v121);
    BignumMULADD2(v125, v124, v5, v17, v107, v123);
    BignumMULADD(v127, v126, v6, v9, v110);
    BignumMULADD2(v129, v128, v6, v10, v112, v127);
    BignumMULADD2(v131, v130, v6, v11, v114, v129);
    BignumMULADD2(v133, v132, v6, v12, v116, v131);
    BignumMULADD2(v135, v134, v6, v13, v118, v133);
    BignumMULADD2(v137, v136, v6, v14, v120, v135);
    BignumMULADD2(v139, v138, v6, v15, v122, v137);
    BignumMULADD2(v141, v140, v6, v16, v124, v139);
    BignumMULADD2(v143, v142, v6, v17, v125, v141);
    BignumMULADD(v145, v144, v7, v9, v128);
    BignumMULADD2(v147, v146, v7, v10, v130, v145);
    BignumMULADD2(v149, v148, v7, v11, v132, v147);
    BignumMULADD2(v151, v150, v7, v12, v134, v149);
    BignumMULADD2(v153, v152, v7, v13, v136, v151);
    BignumMULADD2(v155, v154, v7, v14, v138, v153);
    BignumMULADD2(v157, v156, v7, v15, v140, v155);
    BignumMULADD2(v159, v158, v7, v16, v142, v157);
    BignumMULADD2(v161, v160, v7, v17, v143, v159);
    BignumMULADD(v163, v162, v8, v9, v146);
    BignumMULADD2(v165, v164, v8, v10, v148, v163);
    BignumMULADD2(v167, v166, v8, v11, v150, v165);
    BignumMULADD2(v169, v168, v8, v12, v152, v167);
    BignumMULADD2(v171, v170, v8, v13, v154, v169);
    BignumMULADD2(v173, v172, v8, v14, v156, v171);
    BignumMULADD2(v175, v174, v8, v15, v158, v173);
    BignumMULADD2(v177, v176, v8, v16, v160, v175);
    v178 = v8 * v17 + v161 + v177;
    v180 = (v162) & ((((BignumInt)1) << 2)-1);
    v181 = ((v162) >> 2) | ((v164) << 14);
    v182 = ((v164) >> 2) | ((v166) << 14);
    v183 = ((v166) >> 2) | ((v168) << 14);
    v184 = ((v168) >> 2) | ((v170) << 14);
    v185 = ((v170) >> 2) | ((v172) << 14);
    v186 = ((v172) >> 2) | ((v174) << 14);
    v187 = ((v174) >> 2) | ((v176) << 14);
    v188 = ((v176) >> 2) | ((v178) << 14);
    v189 = (v178) >> 2;
    v190 = (v189) & ((((BignumInt)1) << 2)-1);
    v191 = (v178) >> 4;
    BignumMUL(v193, v192, 5, v181);
    BignumMULADD(v195, v194, 5, v182, v193);
    BignumMULADD(v197, v196, 5, v183, v195);
    BignumMULADD(v199, v198, 5, v184, v197);
    BignumMULADD(v201, v200, 5, v185, v199);
    BignumMULADD(v203, v202, 5, v186, v201);
    BignumMULADD(v205, v204, 5, v187, v203);
    BignumMULADD(v207, v206, 5, v188, v205);
    v208 = 5 * v190 + v207;
    v210 = 25 * v191;
    BignumADC(v212, carry, v18, v192, 0);
    BignumADC(v213, carry, v36, v194, carry);
    BignumADC(v214, carry, v54, v196, carry);
    BignumADC(v215, carry, v72, v198, carry);
    BignumADC(v216, carry, v90, v200, carry);
    BignumADC(v217, carry, v108, v202, carry);
    BignumADC(v218, carry, v126, v204, carry);
    BignumADC(v219, carry, v144, v206, carry);
    v220 = v180 + v208 + carry;
    BignumADC(v221, carry, v212, v210, 0);
    BignumADC(v222, carry, v213, 0, carry);
    BignumADC(v223, carry, v214, 0, carry);
    BignumADC(v224, carry, v215, 0, carry);
    BignumADC(v225, carry, v216, 0, carry);
    BignumADC(v226, carry, v217, 0, carry);
    BignumADC(v227, carry, v218, 0, carry);
    BignumADC(v228, carry, v219, 0, carry);
    v229 = v220 + 0 + carry;
    r->w[0] = v221;
    r->w[1] = v222;
    r->w[2] = v223;
    r->w[3] = v224;
    r->w[4] = v225;
    r->w[5] = v226;
    r->w[6] = v227;
    r->w[7] = v228;
    r->w[8] = v229;
}

static void bigval_final_reduce(bigval *n)
{
    BignumInt v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v13, v14, v15;
    BignumInt v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28;
    BignumInt v29, v30, v31, v32, v34, v35, v36, v37, v38, v39, v40, v41, v42;
    BignumInt v43;
    BignumCarry carry;

    v0 = n->w[0];
    v1 = n->w[1];
    v2 = n->w[2];
    v3 = n->w[3];
    v4 = n->w[4];
    v5 = n->w[5];
    v6 = n->w[6];
    v7 = n->w[7];
    v8 = n->w[8];
    v9 = (v8) >> 2;
    v10 = (v8) & ((((BignumInt)1) << 2)-1);
    v11 = 5 * v9;
    BignumADC(v13, carry, v0, v11, 0);
    BignumADC(v14, carry, v1, 0, carry);
    BignumADC(v15, carry, v2, 0, carry);
    BignumADC(v16, carry, v3, 0, carry);
    BignumADC(v17, carry, v4, 0, carry);
    BignumADC(v18, carry, v5, 0, carry);
    BignumADC(v19, carry, v6, 0, carry);
    BignumADC(v20, carry, v7, 0, carry);
    v21 = v10 + 0 + carry;
    BignumADC(v22, carry, v13, 5, 0);
    (void)v22;
    BignumADC(v23, carry, v14, 0, carry);
    (void)v23;
    BignumADC(v24, carry, v15, 0, carry);
    (void)v24;
    BignumADC(v25, carry, v16, 0, carry);
    (void)v25;
    BignumADC(v26, carry, v17, 0, carry);
    (void)v26;
    BignumADC(v27, carry, v18, 0, carry);
    (void)v27;
    BignumADC(v28, carry, v19, 0, carry);
    (void)v28;
    BignumADC(v29, carry, v20, 0, carry);
    (void)v29;
    v30 = v21 + 0 + carry;
    v31 = (v30) >> 2;
    v32 = 5 * v31;
    BignumADC(v34, carry, v13, v32, 0);
    BignumADC(v35, carry, v14, 0, carry);
    BignumADC(v36, carry, v15, 0, carry);
    BignumADC(v37, carry, v16, 0, carry);
    BignumADC(v38, carry, v17, 0, carry);
    BignumADC(v39, carry, v18, 0, carry);
    BignumADC(v40, carry, v19, 0, carry);
    BignumADC(v41, carry, v20, 0, carry);
    v42 = v21 + 0 + carry;
    v43 = (v42) & ((((BignumInt)1) << 2)-1);
    n->w[0] = v34;
    n->w[1] = v35;
    n->w[2] = v36;
    n->w[3] = v37;
    n->w[4] = v38;
    n->w[5] = v39;
    n->w[6] = v40;
    n->w[7] = v41;
    n->w[8] = v43;
}

#elif BIGNUM_INT_BITS == 32

static void bigval_add(bigval *r, const bigval *a, const bigval *b)
{
    BignumInt v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14;
    BignumCarry carry;

    v0 = a->w[0];
    v1 = a->w[1];
    v2 = a->w[2];
    v3 = a->w[3];
    v4 = a->w[4];
    v5 = b->w[0];
    v6 = b->w[1];
    v7 = b->w[2];
    v8 = b->w[3];
    v9 = b->w[4];
    BignumADC(v10, carry, v0, v5, 0);
    BignumADC(v11, carry, v1, v6, carry);
    BignumADC(v12, carry, v2, v7, carry);
    BignumADC(v13, carry, v3, v8, carry);
    v14 = v4 + v9 + carry;
    r->w[0] = v10;
    r->w[1] = v11;
    r->w[2] = v12;
    r->w[3] = v13;
    r->w[4] = v14;
}

static void bigval_mul_mod_p(bigval *r, const bigval *a, const bigval *b)
{
    BignumInt v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14;
    BignumInt v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27;
    BignumInt v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40;
    BignumInt v41, v42, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53;
    BignumInt v54, v55, v56, v57, v58, v60, v61, v62, v63, v64, v65, v66, v67;
    BignumInt v68, v69, v70, v71, v72, v73, v74, v75, v76, v78, v80, v81, v82;
    BignumInt v83, v84, v85, v86, v87, v88, v89;
    BignumCarry carry;

    v0 = a->w[0];
    v1 = a->w[1];
    v2 = a->w[2];
    v3 = a->w[3];
    v4 = a->w[4];
    v5 = b->w[0];
    v6 = b->w[1];
    v7 = b->w[2];
    v8 = b->w[3];
    v9 = b->w[4];
    BignumMUL(v11, v10, v0, v5);
    BignumMULADD(v13, v12, v0, v6, v11);
    BignumMULADD(v15, v14, v0, v7, v13);
    BignumMULADD(v17, v16, v0, v8, v15);
    BignumMULADD(v19, v18, v0, v9, v17);
    BignumMULADD(v21, v20, v1, v5, v12);
    BignumMULADD2(v23, v22, v1, v6, v14, v21);
    BignumMULADD2(v25, v24, v1, v7, v16, v23);
    BignumMULADD2(v27, v26, v1, v8, v18, v25);
    BignumMULADD2(v29, v28, v1, v9, v19, v27);
    BignumMULADD(v31, v30, v2, v5, v22);
    BignumMULADD2(v33, v32, v2, v6, v24, v31);
    BignumMULADD2(v35, v34, v2, v7, v26, v33);
    BignumMULADD2(v37, v36, v2, v8, v28, v35);
    BignumMULADD2(v39, v38, v2, v9, v29, v37);
    BignumMULADD(v41, v40, v3, v5, v32);
    BignumMULADD2(v43, v42, v3, v6, v34, v41);
    BignumMULADD2(v45, v44, v3, v7, v36, v43);
    BignumMULADD2(v47, v46, v3, v8, v38, v45);
    BignumMULADD2(v49, v48, v3, v9, v39, v47);
    BignumMULADD(v51, v50, v4, v5, v42);
    BignumMULADD2(v53, v52, v4, v6, v44, v51);
    BignumMULADD2(v55, v54, v4, v7, v46, v53);
    BignumMULADD2(v57, v56, v4, v8, v48, v55);
    v58 = v4 * v9 + v49 + v57;
    v60 = (v50) & ((((BignumInt)1) << 2)-1);
    v61 = ((v50) >> 2) | ((v52) << 30);
    v62 = ((v52) >> 2) | ((v54) << 30);
    v63 = ((v54) >> 2) | ((v56) << 30);
    v64 = ((v56) >> 2) | ((v58) << 30);
    v65 = (v58) >> 2;
    v66 = (v65) & ((((BignumInt)1) << 2)-1);
    v67 = (v58) >> 4;
    BignumMUL(v69, v68, 5, v61);
    BignumMULADD(v71, v70, 5, v62, v69);
    BignumMULADD(v73, v72, 5, v63, v71);
    BignumMULADD(v75, v74, 5, v64, v73);
    v76 = 5 * v66 + v75;
    v78 = 25 * v67;
    BignumADC(v80, carry, v10, v68, 0);
    BignumADC(v81, carry, v20, v70, carry);
    BignumADC(v82, carry, v30, v72, carry);
    BignumADC(v83, carry, v40, v74, carry);
    v84 = v60 + v76 + carry;
    BignumADC(v85, carry, v80, v78, 0);
    BignumADC(v86, carry, v81, 0, carry);
    BignumADC(v87, carry, v82, 0, carry);
    BignumADC(v88, carry, v83, 0, carry);
    v89 = v84 + 0 + carry;
    r->w[0] = v85;
    r->w[1] = v86;
    r->w[2] = v87;
    r->w[3] = v88;
    r->w[4] = v89;
}

static void bigval_final_reduce(bigval *n)
{
    BignumInt v0, v1, v2, v3, v4, v5, v6, v7, v9, v10, v11, v12, v13, v14;
    BignumInt v15, v16, v17, v18, v19, v20, v22, v23, v24, v25, v26, v27;
    BignumCarry carry;

    v0 = n->w[0];
    v1 = n->w[1];
    v2 = n->w[2];
    v3 = n->w[3];
    v4 = n->w[4];
    v5 = (v4) >> 2;
    v6 = (v4) & ((((BignumInt)1) << 2)-1);
    v7 = 5 * v5;
    BignumADC(v9, carry, v0, v7, 0);
    BignumADC(v10, carry, v1, 0, carry);
    BignumADC(v11, carry, v2, 0, carry);
    BignumADC(v12, carry, v3, 0, carry);
    v13 = v6 + 0 + carry;
    BignumADC(v14, carry, v9, 5, 0);
    (void)v14;
    BignumADC(v15, carry, v10, 0, carry);
    (void)v15;
    BignumADC(v16, carry, v11, 0, carry);
    (void)v16;
    BignumADC(v17, carry, v12, 0, carry);
    (void)v17;
    v18 = v13 + 0 + carry;
    v19 = (v18) >> 2;
    v20 = 5 * v19;
    BignumADC(v22, carry, v9, v20, 0);
    BignumADC(v23, carry, v10, 0, carry);
    BignumADC(v24, carry, v11, 0, carry);
    BignumADC(v25, carry, v12, 0, carry);
    v26 = v13 + 0 + carry;
    v27 = (v26) & ((((BignumInt)1) << 2)-1);
    n->w[0] = v22;
    n->w[1] = v23;
    n->w[2] = v24;
    n->w[3] = v25;
    n->w[4] = v27;
}

#elif BIGNUM_INT_BITS == 64

static void bigval_add(bigval *r, const bigval *a, const bigval *b)
{
    BignumInt v0, v1, v2, v3, v4, v5, v6, v7, v8;
    BignumCarry carry;

    v0 = a->w[0];
    v1 = a->w[1];
    v2 = a->w[2];
    v3 = b->w[0];
    v4 = b->w[1];
    v5 = b->w[2];
    BignumADC(v6, carry, v0, v3, 0);
    BignumADC(v7, carry, v1, v4, carry);
    v8 = v2 + v5 + carry;
    r->w[0] = v6;
    r->w[1] = v7;
    r->w[2] = v8;
}

static void bigval_mul_mod_p(bigval *r, const bigval *a, const bigval *b)
{
    BignumInt v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14;
    BignumInt v15, v16, v17, v18, v19, v20, v21, v22, v24, v25, v26, v27, v28;
    BignumInt v29, v30, v31, v32, v33, v34, v36, v38, v39, v40, v41, v42, v43;
    BignumCarry carry;

    v0 = a->w[0];
    v1 = a->w[1];
    v2 = a->w[2];
    v3 = b->w[0];
    v4 = b->w[1];
    v5 = b->w[2];
    BignumMUL(v7, v6, v0, v3);
    BignumMULADD(v9, v8, v0, v4, v7);
    BignumMULADD(v11, v10, v0, v5, v9);
    BignumMULADD(v13, v12, v1, v3, v8);
    BignumMULADD2(v15, v14, v1, v4, v10, v13);
    BignumMULADD2(v17, v16, v1, v5, v11, v15);
    BignumMULADD(v19, v18, v2, v3, v14);
    BignumMULADD2(v21, v20, v2, v4, v16, v19);
    v22 = v2 * v5 + v17 + v21;
    v24 = (v18) & ((((BignumInt)1) << 2)-1);
    v25 = ((v18) >> 2) | ((v20) << 62);
    v26 = ((v20) >> 2) | ((v22) << 62);
    v27 = (v22) >> 2;
    v28 = (v27) & ((((BignumInt)1) << 2)-1);
    v29 = (v22) >> 4;
    BignumMUL(v31, v30, 5, v25);
    BignumMULADD(v33, v32, 5, v26, v31);
    v34 = 5 * v28 + v33;
    v36 = 25 * v29;
    BignumADC(v38, carry, v6, v30, 0);
    BignumADC(v39, carry, v12, v32, carry);
    v40 = v24 + v34 + carry;
    BignumADC(v41, carry, v38, v36, 0);
    BignumADC(v42, carry, v39, 0, carry);
    v43 = v40 + 0 + carry;
    r->w[0] = v41;
    r->w[1] = v42;
    r->w[2] = v43;
}

static void bigval_final_reduce(bigval *n)
{
    BignumInt v0, v1, v2, v3, v4, v5, v7, v8, v9, v10, v11, v12, v13, v14;
    BignumInt v16, v17, v18, v19;
    BignumCarry carry;

    v0 = n->w[0];
    v1 = n->w[1];
    v2 = n->w[2];
    v3 = (v2) >> 2;
    v4 = (v2) & ((((BignumInt)1) << 2)-1);
    v5 = 5 * v3;
    BignumADC(v7, carry, v0, v5, 0);
    BignumADC(v8, carry, v1, 0, carry);
    v9 = v4 + 0 + carry;
    BignumADC(v10, carry, v7, 5, 0);
    (void)v10;
    BignumADC(v11, carry, v8, 0, carry);
    (void)v11;
    v12 = v9 + 0 + carry;
    v13 = (v12) >> 2;
    v14 = 5 * v13;
    BignumADC(v16, carry, v7, v14, 0);
    BignumADC(v17, carry, v8, 0, carry);
    v18 = v9 + 0 + carry;
    v19 = (v18) & ((((BignumInt)1) << 2)-1);
    n->w[0] = v16;
    n->w[1] = v17;
    n->w[2] = v19;
}

#else
#error Add another bit count to contrib/make1305.py and rerun it
#endif

struct poly1305 {
    unsigned char nonce[16];
    bigval r;
    bigval h;

    /* Buffer in case we get less that a multiple of 16 bytes */
    unsigned char buffer[16];
    int bufferIndex;
};

static void poly1305_init(struct poly1305 *ctx)
{
    memset(ctx->nonce, 0, 16);
    ctx->bufferIndex = 0;
    bigval_clear(&ctx->h);
}

static void poly1305_key(struct poly1305 *ctx, ptrlen key)
{
    assert(key.len == 32);             /* Takes a 256 bit key */

    unsigned char key_copy[16];
    memcpy(key_copy, key.ptr, 16);

    /* Key the MAC itself
     * bytes 4, 8, 12 and 16 are required to have their top four bits clear */
    key_copy[3] &= 0x0f;
    key_copy[7] &= 0x0f;
    key_copy[11] &= 0x0f;
    key_copy[15] &= 0x0f;
    /* bytes 5, 9 and 13 are required to have their bottom two bits clear */
    key_copy[4] &= 0xfc;
    key_copy[8] &= 0xfc;
    key_copy[12] &= 0xfc;
    bigval_import_le(&ctx->r, key_copy, 16);
    smemclr(key_copy, sizeof(key_copy));

    /* Use second 128 bits as the nonce */
    memcpy(ctx->nonce, (const char *)key.ptr + 16, 16);
}

/* Feed up to 16 bytes (should only be less for the last chunk) */
static void poly1305_feed_chunk(struct poly1305 *ctx,
                                const unsigned char *chunk, int len)
{
    bigval c;
    bigval_import_le(&c, chunk, len);
    c.w[len / BIGNUM_INT_BYTES] |=
        (BignumInt)1 << (8 * (len % BIGNUM_INT_BYTES));
    bigval_add(&c, &c, &ctx->h);
    bigval_mul_mod_p(&ctx->h, &c, &ctx->r);
}

static void poly1305_feed(struct poly1305 *ctx,
                          const unsigned char *buf, int len)
{
    /* Check for stuff left in the buffer from last time */
    if (ctx->bufferIndex) {
        /* Try to fill up to 16 */
        while (ctx->bufferIndex < 16 && len) {
            ctx->buffer[ctx->bufferIndex++] = *buf++;
            --len;
        }
        if (ctx->bufferIndex == 16) {
            poly1305_feed_chunk(ctx, ctx->buffer, 16);
            ctx->bufferIndex = 0;
        }
    }

    /* Process 16 byte whole chunks */
    while (len >= 16) {
        poly1305_feed_chunk(ctx, buf, 16);
        len -= 16;
        buf += 16;
    }

    /* Cache stuff that's left over */
    if (len) {
        memcpy(ctx->buffer, buf, len);
        ctx->bufferIndex = len;
    }
}

/* Finalise and populate buffer with 16 byte with MAC */
static void poly1305_finalise(struct poly1305 *ctx, unsigned char *mac)
{
    bigval tmp;

    if (ctx->bufferIndex) {
        poly1305_feed_chunk(ctx, ctx->buffer, ctx->bufferIndex);
    }

    bigval_import_le(&tmp, ctx->nonce, 16);
    bigval_final_reduce(&ctx->h);
    bigval_add(&tmp, &tmp, &ctx->h);
    bigval_export_le(&tmp, mac, 16);
}

/* SSH-2 wrapper */

struct ccp_context {
    struct chacha20 a_cipher; /* Used for length */
    struct chacha20 b_cipher; /* Used for content */

    /* Cache of the first 4 bytes because they are the sequence number */
    /* Kept in 8 bytes with the top as zero to allow easy passing to setiv */
    int mac_initialised; /* Where we have got to in filling mac_iv */
    unsigned char mac_iv[8];

    struct poly1305 mac;

    BinarySink_IMPLEMENTATION;
    ssh_cipher ciph;
    ssh2_mac mac_if;
};

static ssh2_mac *poly_ssh2_new(
    const ssh2_macalg *alg, ssh_cipher *cipher)
{
    struct ccp_context *ctx = container_of(cipher, struct ccp_context, ciph);
    ctx->mac_if.vt = alg;
    BinarySink_DELEGATE_INIT(&ctx->mac_if, ctx);
    return &ctx->mac_if;
}

static void poly_ssh2_free(ssh2_mac *mac)
{
    /* Not allocated, just forwarded, no need to free */
}

static void poly_setkey(ssh2_mac *mac, ptrlen key)
{
    /* Uses the same context as ChaCha20, so ignore */
}

static void poly_start(ssh2_mac *mac)
{
    struct ccp_context *ctx = container_of(mac, struct ccp_context, mac_if);

    ctx->mac_initialised = 0;
    memset(ctx->mac_iv, 0, 8);
    poly1305_init(&ctx->mac);
}

static void poly_BinarySink_write(BinarySink *bs, const void *blkv, size_t len)
{
    struct ccp_context *ctx = BinarySink_DOWNCAST(bs, struct ccp_context);
    const unsigned char *blk = (const unsigned char *)blkv;

    /* First 4 bytes are the IV */
    while (ctx->mac_initialised < 4 && len) {
        ctx->mac_iv[7 - ctx->mac_initialised] = *blk++;
        ++ctx->mac_initialised;
        --len;
    }

    /* Initialise the IV if needed */
    if (ctx->mac_initialised == 4) {
        chacha20_iv(&ctx->b_cipher, ctx->mac_iv);
        ++ctx->mac_initialised;  /* Don't do it again */

        /* Do first rotation */
        chacha20_round(&ctx->b_cipher);

        /* Set the poly key */
        poly1305_key(&ctx->mac, make_ptrlen(ctx->b_cipher.current, 32));

        /* Set the first round as used */
        ctx->b_cipher.currentIndex = 64;
    }

    /* Update the MAC with anything left */
    if (len) {
        poly1305_feed(&ctx->mac, blk, len);
    }
}

static void poly_genresult(ssh2_mac *mac, unsigned char *blk)
{
    struct ccp_context *ctx = container_of(mac, struct ccp_context, mac_if);
    poly1305_finalise(&ctx->mac, blk);
}

static const char *poly_text_name(ssh2_mac *mac)
{
    return "Poly1305";
}

const ssh2_macalg ssh2_poly1305 = {
    poly_ssh2_new, poly_ssh2_free, poly_setkey,
    poly_start, poly_genresult, poly_text_name,

    "", "", /* Not selectable individually, just part of ChaCha20-Poly1305 */
    16, 0,
};

static ssh_cipher *ccp_new(const ssh_cipheralg *alg)
{
    struct ccp_context *ctx = snew(struct ccp_context);
    BinarySink_INIT(ctx, poly_BinarySink_write);
    poly1305_init(&ctx->mac);
    ctx->ciph.vt = alg;
    return &ctx->ciph;
}

static void ccp_free(ssh_cipher *cipher)
{
    struct ccp_context *ctx = container_of(cipher, struct ccp_context, ciph);
    smemclr(&ctx->a_cipher, sizeof(ctx->a_cipher));
    smemclr(&ctx->b_cipher, sizeof(ctx->b_cipher));
    smemclr(&ctx->mac, sizeof(ctx->mac));
    sfree(ctx);
}

static void ccp_iv(ssh_cipher *cipher, const void *iv)
{
    /* struct ccp_context *ctx =
           container_of(cipher, struct ccp_context, ciph); */
    /* IV is set based on the sequence number */
}

static void ccp_key(ssh_cipher *cipher, const void *vkey)
{
    const unsigned char *key = (const unsigned char *)vkey;
    struct ccp_context *ctx = container_of(cipher, struct ccp_context, ciph);
    /* Initialise the a_cipher (for decrypting lengths) with the first 256 bits */
    chacha20_key(&ctx->a_cipher, key + 32);
    /* Initialise the b_cipher (for content and MAC) with the second 256 bits */
    chacha20_key(&ctx->b_cipher, key);
}

static void ccp_encrypt(ssh_cipher *cipher, void *blk, int len)
{
    struct ccp_context *ctx = container_of(cipher, struct ccp_context, ciph);
    chacha20_encrypt(&ctx->b_cipher, blk, len);
}

static void ccp_decrypt(ssh_cipher *cipher, void *blk, int len)
{
    struct ccp_context *ctx = container_of(cipher, struct ccp_context, ciph);
    chacha20_decrypt(&ctx->b_cipher, blk, len);
}

static void ccp_length_op(struct ccp_context *ctx, void *blk, int len,
                          unsigned long seq)
{
    unsigned char iv[8];
    /*
     * According to RFC 4253 (section 6.4), the packet sequence number wraps
     * at 2^32, so its 32 high-order bits will always be zero.
     */
    PUT_32BIT_LSB_FIRST(iv, 0);
    PUT_32BIT_LSB_FIRST(iv + 4, seq);
    chacha20_iv(&ctx->a_cipher, iv);
    chacha20_iv(&ctx->b_cipher, iv);
    /* Reset content block count to 1, as the first is the key for Poly1305 */
    ++ctx->b_cipher.state[12];
    smemclr(iv, sizeof(iv));
}

static void ccp_encrypt_length(ssh_cipher *cipher, void *blk, int len,
                               unsigned long seq)
{
    struct ccp_context *ctx = container_of(cipher, struct ccp_context, ciph);
    ccp_length_op(ctx, blk, len, seq);
    chacha20_encrypt(&ctx->a_cipher, blk, len);
}

static void ccp_decrypt_length(ssh_cipher *cipher, void *blk, int len,
                               unsigned long seq)
{
    struct ccp_context *ctx = container_of(cipher, struct ccp_context, ciph);
    ccp_length_op(ctx, blk, len, seq);
    chacha20_decrypt(&ctx->a_cipher, blk, len);
}

const ssh_cipheralg ssh2_chacha20_poly1305 = {

    ccp_new,
    ccp_free,
    ccp_iv,
    ccp_key,
    ccp_encrypt,
    ccp_decrypt,
    ccp_encrypt_length,
    ccp_decrypt_length,

    "chacha20-poly1305@openssh.com",
    1, 512, 64, SSH_CIPHER_SEPARATE_LENGTH, "ChaCha20",

    &ssh2_poly1305
};

static const ssh_cipheralg *const ccp_list[] = {
    &ssh2_chacha20_poly1305
};

const ssh2_ciphers ssh2_ccp = { lenof(ccp_list), ccp_list };
