/*
 * Implementation of shared trivial routines that ssh_cipher
 * implementations might use.
 */

#include "ssh.h"

void nullcipher_next_message(ssh_cipher *cipher)
{
    /* Most ciphers don't do anything at all with this */
}
