/*
 * Top-level vtables to select a SHA-1 implementation.
 */

#include <assert.h>
#include <stdlib.h>

#include "putty.h"
#include "ssh.h"
#include "sha1.h"

static ssh_hash *sha1_select(const ssh_hashalg *alg)
{
    static const ssh_hashalg *const real_algs[] = {
#if HAVE_SHA_NI
        &ssh_sha1_ni,
#endif
#if HAVE_NEON_CRYPTO
        &ssh_sha1_neon,
#endif
        &ssh_sha1_sw,
        NULL,
    };

    for (size_t i = 0; real_algs[i]; i++) {
        const ssh_hashalg *alg = real_algs[i];
        const struct sha1_extra *alg_extra =
            (const struct sha1_extra *)alg->extra;
        if (check_availability(alg_extra))
            return ssh_hash_new(alg);
    }

    /* We should never reach the NULL at the end of the list, because
     * the last non-NULL entry should be software-only SHA-1, which
     * is always available. */
    unreachable("sha1_select ran off the end of its list");
}

const ssh_hashalg ssh_sha1 = {
    .new = sha1_select,
    .hlen = 20,
    .blocklen = 64,
    HASHALG_NAMES_ANNOTATED("SHA-1", "dummy selector vtable"),
};
