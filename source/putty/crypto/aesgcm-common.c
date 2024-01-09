#include "ssh.h"
#include "aesgcm.h"

void aesgcm_set_prefix_lengths(ssh2_mac *mac, size_t skip, size_t aad)
{
    const struct aesgcm_extra *extra = mac->vt->extra;
    extra->set_prefix_lengths(mac, skip, aad);
}
