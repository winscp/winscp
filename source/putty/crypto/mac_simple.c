/*
 * Convenience function to MAC a single piece of data, wrapping up
 * the faff of making and freeing an ssh_mac.
 */

#include "ssh.h"

void mac_simple(const ssh2_macalg *alg, ptrlen key, ptrlen data, void *output)
{
    ssh2_mac *mac = ssh2_mac_new(alg, NULL);
    ssh2_mac_setkey(mac, key);
    ssh2_mac_start(mac);
    put_datapl(mac, data);
    ssh2_mac_genresult(mac, output);
    ssh2_mac_free(mac);
}
