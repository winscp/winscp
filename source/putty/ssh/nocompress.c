/*
 * SSH compression vtable for not using compression at all.
 */

#include "putty.h"
#include "ssh.h"

static ssh_compressor *ssh_comp_none_init(void)
{
    return NULL;
}
static void ssh_comp_none_cleanup(ssh_compressor *handle)
{
}
static ssh_decompressor *ssh_decomp_none_init(void)
{
    return NULL;
}
static void ssh_decomp_none_cleanup(ssh_decompressor *handle)
{
}
static void ssh_comp_none_block(ssh_compressor *handle,
                                const unsigned char *block, int len,
                                unsigned char **outblock, int *outlen,
                                int minlen)
{
}
static bool ssh_decomp_none_block(ssh_decompressor *handle,
                                  const unsigned char *block, int len,
                                  unsigned char **outblock, int *outlen)
{
    return false;
}
const ssh_compression_alg ssh_comp_none = {
    .name = "none",
    .delayed_name = NULL,
    .compress_new = ssh_comp_none_init,
    .compress_free = ssh_comp_none_cleanup,
    .compress = ssh_comp_none_block,
    .decompress_new = ssh_decomp_none_init,
    .decompress_free = ssh_decomp_none_cleanup,
    .decompress = ssh_decomp_none_block,
    .text_name = NULL,
};
