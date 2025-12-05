/*
 * Definitions likely to be helpful to multiple SHA-512 implementations.
 */

/*
 * The 'extra' structure used by SHA-512 implementations is used to
 * include information about how to check if a given implementation is
 * available at run time, and whether we've already checked.
 */
struct sha512_extra_mutable;
struct sha512_extra {
    /* Pointer to the initial state (distinguishes SHA-384 from -512) */
    const uint64_t *initial_state;

    /* Function to check availability. Might be expensive, so we don't
     * want to call it more than once. */
    bool (*check_available)(void);

    /* Point to a writable substructure. */
    struct sha512_extra_mutable *mut;
};
struct sha512_extra_mutable {
    bool checked_availability;
    bool is_available;
};
static inline bool check_availability(const struct sha512_extra *extra)
{
    if (!extra->mut->checked_availability) {
        extra->mut->is_available = extra->check_available();
        extra->mut->checked_availability = true;
    }

    return extra->mut->is_available;
}

/*
 * Macro to define a pair of SHA-{384,512} vtables together with their
 * 'extra' structure.
 */
#define SHA512_VTABLES(impl_c, impl_display)                            \
    static struct sha512_extra_mutable sha512_ ## impl_c ## _extra_mut; \
    static const struct sha512_extra sha384_ ## impl_c ## _extra = {    \
        .initial_state = sha384_initial_state,                          \
        .check_available = sha512_ ## impl_c ## _available,             \
        .mut = &sha512_ ## impl_c ## _extra_mut,                        \
    };                                                                  \
    static const struct sha512_extra sha512_ ## impl_c ## _extra = {    \
        .initial_state = sha512_initial_state,                          \
        .check_available = sha512_ ## impl_c ## _available,             \
        .mut = &sha512_ ## impl_c ## _extra_mut,                        \
    };                                                                  \
    const ssh_hashalg ssh_sha384_ ## impl_c = {                         \
        .new = sha512_ ## impl_c ## _new,                               \
        .reset = sha512_ ## impl_c ## _reset,                           \
        .copyfrom = sha512_ ## impl_c ## _copyfrom,                     \
        .digest = sha384_ ## impl_c ## _digest,                         \
        .free = sha512_ ## impl_c ## _free,                             \
        .hlen = 48,                                                     \
        .blocklen = 128,                                                \
        HASHALG_NAMES_ANNOTATED("SHA-384", impl_display),               \
        .extra = &sha384_ ## impl_c ## _extra,                          \
    };                                                                  \
    const ssh_hashalg ssh_sha512_ ## impl_c = {                         \
        .new = sha512_ ## impl_c ## _new,                               \
        .reset = sha512_ ## impl_c ## _reset,                           \
        .copyfrom = sha512_ ## impl_c ## _copyfrom,                     \
        .digest = sha512_ ## impl_c ## _digest,                         \
        .free = sha512_ ## impl_c ## _free,                             \
        .hlen = 64,                                                     \
        .blocklen = 128,                                                \
        HASHALG_NAMES_ANNOTATED("SHA-512", impl_display),               \
        .extra = &sha512_ ## impl_c ## _extra,                          \
    }

extern const uint64_t sha512_initial_state[8];
extern const uint64_t sha384_initial_state[8];
extern const uint64_t sha512_round_constants[80];

#define SHA512_ROUNDS 80

typedef struct sha512_block sha512_block;
struct sha512_block {
    uint8_t block[128];
    size_t used;
    uint64_t lenhi, lenlo;
};

static inline void sha512_block_setup(sha512_block *blk)
{
    blk->used = 0;
    blk->lenhi = blk->lenlo = 0;
}

static inline bool sha512_block_write(
    sha512_block *blk, const void **vdata, size_t *len)
{
    size_t blkleft = sizeof(blk->block) - blk->used;
    size_t chunk = *len < blkleft ? *len : blkleft;

    const uint8_t *p = *vdata;
    memcpy(blk->block + blk->used, p, chunk);
    *vdata = p + chunk;
    *len -= chunk;
    blk->used += chunk;

    size_t chunkbits = chunk << 3;

    blk->lenlo += chunkbits;
    blk->lenhi += (blk->lenlo < chunkbits);

    if (blk->used == sizeof(blk->block)) {
        blk->used = 0;
        return true;
    }

    return false;
}

static inline void sha512_block_pad(sha512_block *blk, BinarySink *bs)
{
    uint64_t final_lenhi = blk->lenhi;
    uint64_t final_lenlo = blk->lenlo;
    size_t pad = 127 & (111 - blk->used);

    put_byte(bs, 0x80);
    put_padding(bs, pad, 0);
    put_uint64(bs, final_lenhi);
    put_uint64(bs, final_lenlo);

    assert(blk->used == 0 && "Should have exactly hit a block boundary");
}
