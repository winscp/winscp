/*
 * Centralised parts of the SSH-2 MAC API, which don't need to vary
 * with the MAC implementation.
 */

#include <assert.h>

#include "ssh.h"

bool ssh2_mac_verresult(ssh2_mac *mac, const void *candidate)
{
    unsigned char correct[64]; /* at least as big as all known MACs */
    bool toret;

    assert(mac->vt->len <= sizeof(correct));
    ssh2_mac_genresult(mac, correct);
    toret = smemeq(correct, candidate, mac->vt->len);

    smemclr(correct, sizeof(correct));

    return toret;
}

static void ssh2_mac_prepare(ssh2_mac *mac, const void *blk, int len,
                             unsigned long seq)
{
    ssh2_mac_start(mac);
    put_uint32(mac, seq);
    put_data(mac, blk, len);
}

void ssh2_mac_generate(ssh2_mac *mac, void *blk, int len, unsigned long seq)
{
    ssh2_mac_prepare(mac, blk, len, seq);
    ssh2_mac_genresult(mac, (unsigned char *)blk + len);
}

bool ssh2_mac_verify(
    ssh2_mac *mac, const void *blk, int len, unsigned long seq)
{
    ssh2_mac_prepare(mac, blk, len, seq);
    return ssh2_mac_verresult(mac, (const unsigned char *)blk + len);
}
