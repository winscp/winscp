/*
 * Common variable definitions across all the SHA-1 implementations.
 */

#include "ssh.h"
#include "sha1.h"

const uint32_t sha1_initial_state[5] = {
    0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0,
};
