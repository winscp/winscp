/*
 * Common variable definitions across all the AES implementations.
 */

#include "ssh.h"
#include "aes.h"

const uint8_t aes_key_setup_round_constants[10] = {
    /* The first few powers of X in GF(2^8), used during key setup.
     * This can safely be a lookup table without side channel risks,
     * because key setup iterates through it once in a standard way
     * regardless of the key. */
    0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36,
};
