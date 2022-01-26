/*
 * Definitions likely to be helpful to multiple SHA-256 implementations.
 */

/*
 * The 'extra' structure used by SHA-256 implementations is used to
 * include information about how to check if a given implementation is
 * available at run time, and whether we've already checked.
 */
struct sha256_extra_mutable;
struct sha256_extra {
    /* Function to check availability. Might be expensive, so we don't
     * want to call it more than once. */
    bool (*check_available)(void);

    /* Point to a writable substructure. */
    struct sha256_extra_mutable *mut;
};
struct sha256_extra_mutable {
    bool checked_availability;
    bool is_available;
};
static inline bool check_availability(const struct sha256_extra *extra)
{
    if (!extra->mut->checked_availability) {
        extra->mut->is_available = extra->check_available();
        extra->mut->checked_availability = true;
    }

    return extra->mut->is_available;
}

/*
 * Macro to define a SHA-256 vtable together with its 'extra'
 * structure.
 */
#define SHA256_VTABLE(impl_c, impl_display)                             \
    static struct sha256_extra_mutable sha256_ ## impl_c ## _extra_mut; \
    static const struct sha256_extra sha256_ ## impl_c ## _extra = {    \
        .check_available = sha256_ ## impl_c ## _available,             \
        .mut = &sha256_ ## impl_c ## _extra_mut,                        \
    };                                                                  \
    const ssh_hashalg ssh_sha256_ ## impl_c = {                         \
        .new = sha256_ ## impl_c ## _new,                               \
        .reset = sha256_ ## impl_c ## _reset,                           \
        .copyfrom = sha256_ ## impl_c ## _copyfrom,                     \
        .digest = sha256_ ## impl_c ## _digest,                         \
        .free = sha256_ ## impl_c ## _free,                             \
        .hlen = 32,                                                     \
        .blocklen = 64,                                                 \
        HASHALG_NAMES_ANNOTATED("SHA-256", impl_display),               \
        .extra = &sha256_ ## impl_c ## _extra,                          \
    }

extern const uint32_t sha256_initial_state[8];
extern const uint32_t sha256_round_constants[64];

#define SHA256_ROUNDS 64

typedef struct sha256_block sha256_block;
struct sha256_block {
    uint8_t block[64];
    size_t used;
    uint64_t len;
};

static inline void sha256_block_setup(sha256_block *blk)
{
    blk->used = 0;
    blk->len = 0;
}

static inline bool sha256_block_write(
    sha256_block *blk, const void **vdata, size_t *len)
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

static inline void sha256_block_pad(sha256_block *blk, BinarySink *bs)
{
    uint64_t final_len = blk->len << 3;
    size_t pad = 1 + (63 & (55 - blk->used));

    put_byte(bs, 0x80);
    for (size_t i = 1; i < pad; i++)
        put_byte(bs, 0);
    put_uint64(bs, final_len);

    assert(blk->used == 0 && "Should have exactly hit a block boundary");
}
