/*
 * Common parts of the state structure for AESGCM MAC implementations.
 */
#define AESGCM_COMMON_FIELDS                    \
    ssh_cipher *cipher;                         \
    unsigned char partblk[16];                  \
    size_t skiplen, aadlen, ciphertextlen;      \
    size_t skipgot, aadgot, partlen;            \
    BinarySink_IMPLEMENTATION;                  \
    ssh2_mac mac

/*
 * The 'extra' structure is used to include information about how to
 * check if a given implementation is available at run time, and
 * whether we've already checked.
 */
struct aesgcm_extra_mutable;
struct aesgcm_extra {
    /* Function to check availability. Might be expensive, so we don't
     * want to call it more than once. */
    bool (*check_available)(void);

    /* Point to a writable substructure. */
    struct aesgcm_extra_mutable *mut;

    /*
     * Extra API function specific to this MAC type that allows
     * testcrypt to set more general lengths for skiplen and aadlen.
     */
    void (*set_prefix_lengths)(ssh2_mac *mac, size_t skip, size_t aad);
};
struct aesgcm_extra_mutable {
    bool checked_availability;
    bool is_available;
};
static inline bool check_aesgcm_availability(const struct aesgcm_extra *extra)
{
    if (!extra->mut->checked_availability) {
        extra->mut->is_available = extra->check_available();
        extra->mut->checked_availability = true;
    }

    return extra->mut->is_available;
}
