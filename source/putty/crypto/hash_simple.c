/*
 * Convenience function to hash a single piece of data, wrapping up
 * the faff of making and freeing an ssh_hash.
 */

#include "ssh.h"

void hash_simple(const ssh_hashalg *alg, ptrlen data, void *output)
{
    ssh_hash *hash = ssh_hash_new(alg);
    put_datapl(hash, data);
    ssh_hash_final(hash, output);
}
