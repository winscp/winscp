/*
 * Definitions likely to be helpful to multiple SHA-1 implementations.
 */

/*
 * The 'extra' structure used by SHA-1 implementations is used to
 * include information about how to check if a given implementation is
 * available at run time, and whether we've already checked.
 */
struct sha1_extra_mutable;
struct sha1_extra {
    /* Function to check availability. Might be expensive, so we don't
     * want to call it more than once. */
    bool (*check_available)(void);

    /* Point to a writable substructure. */
    struct sha1_extra_mutable *mut;
};
struct sha1_extra_mutable {
    bool checked_availability;
    bool is_available;
};
static inline bool check_availability(const struct sha1_extra *extra)
{
    if (!extra->mut->checked_availability) {
        extra->mut->is_available = extra->check_available();
        extra->mut->checked_availability = true;
    }

    return extra->mut->is_available;
}

/*
 * Macro to define a SHA-1 vtable together with its 'extra'
 * structure.
 */
#define SHA1_VTABLE(impl_c, impl_display)                               \
    static struct sha1_extra_mutable sha1_ ## impl_c ## _extra_mut;     \
    static const struct sha1_extra sha1_ ## impl_c ## _extra = {        \
        .check_available = sha1_ ## impl_c ## _available,               \
        .mut = &sha1_ ## impl_c ## _extra_mut,                          \
    };                                                                  \
    const ssh_hashalg ssh_sha1_ ## impl_c = {                           \
        .new = sha1_ ## impl_c ## _new,                                 \
        .reset = sha1_ ## impl_c ## _reset,                             \
        .copyfrom = sha1_ ## impl_c ## _copyfrom,                       \
        .digest = sha1_ ## impl_c ## _digest,                           \
        .free = sha1_ ## impl_c ## _free,                               \
        .hlen = 20,                                                     \
        .blocklen = 64,                                                 \
        HASHALG_NAMES_ANNOTATED("SHA-1", impl_display),                 \
        .extra = &sha1_ ## impl_c ## _extra,                            \
    }

extern const uint32_t sha1_initial_state[5];

#define SHA1_ROUNDS_PER_STAGE 20
#define SHA1_STAGE0_CONSTANT 0x5a827999
#define SHA1_STAGE1_CONSTANT 0x6ed9eba1
#define SHA1_STAGE2_CONSTANT 0x8f1bbcdc
#define SHA1_STAGE3_CONSTANT 0xca62c1d6
#define SHA1_ROUNDS (4 * SHA1_ROUNDS_PER_STAGE)

typedef struct sha1_block sha1_block;
struct sha1_block {
    uint8_t block[64];
    size_t used;
    uint64_t len;
};

static inline void sha1_block_setup(sha1_block *blk)
{
    blk->used = 0;
    blk->len = 0;
}

static inline bool sha1_block_write(
    sha1_block *blk, const void **vdata, size_t *len)
{
    size_t blkleft = sizeof(blk->block) - blk->used;
    size_t chunk = *len < blkleft ? *len : blkleft;

    const uint8_t *p = *vdata;
    memcpy(blk->block + blk->used, p, chunk);
    *vdata = p + chunk;
    *len -= chunk;
    blk->used += chunk;
    blk->len += chunk;

    if (blk->used == sizeof(blk->block)) {
        blk->used = 0;
        return true;
    }

    return false;
}

static inline void sha1_block_pad(sha1_block *blk, BinarySink *bs)
{
    uint64_t final_len = blk->len << 3;
    size_t pad = 1 + (63 & (55 - blk->used));

    put_byte(bs, 0x80);
    for (size_t i = 1; i < pad; i++)
        put_byte(bs, 0);
    put_uint64(bs, final_len);

    assert(blk->used == 0 && "Should have exactly hit a block boundary");
}
