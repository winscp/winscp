#include "ssh.h"
#include "aesgcm.h"

static ssh2_mac *aesgcm_mac_selector_new(const ssh2_macalg *alg,
                                         ssh_cipher *cipher)
{
    static const ssh2_macalg *const real_algs[] = {
#if HAVE_CLMUL
        &ssh2_aesgcm_mac_clmul,
#endif
#if HAVE_NEON_PMULL
        &ssh2_aesgcm_mac_neon,
#endif
        &ssh2_aesgcm_mac_sw,
        NULL,
    };

    size_t i; // WINSCP
    for (i = 0; real_algs[i]; i++) {
        const ssh2_macalg *alg = real_algs[i];
        const struct aesgcm_extra *alg_extra =
            (const struct aesgcm_extra *)alg->extra;
        if (check_aesgcm_availability(alg_extra))
            return ssh2_mac_new(alg, cipher);
    }

    /* We should never reach the NULL at the end of the list, because
     * the last non-NULL entry should be software-only GCM, which is
     * always available. */
    unreachable("aesgcm_select ran off the end of its list");
}

const ssh2_macalg ssh2_aesgcm_mac = {
    /*.new =*/ aesgcm_mac_selector_new,
    NULL, NULL, NULL, NULL, NULL, NULL, // WINSCP
    /*.name =*/ "",
    /*.etm_name =*/ "", /* Not selectable independently */
    /*.len =*/ 16,
    /*.keylen =*/ 0,
    NULL, // WINSCP
};
