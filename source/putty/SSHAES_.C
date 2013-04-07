#include "sshaes.c"

#include "puttyexp.h"

void * call_aes_make_context()
{
  return aes_make_context();
}

void call_aes_free_context(void * handle)
{
  aes_free_context(handle);
}

void call_aes_setup(void * ctx, int blocklen, unsigned char * key, int keylen)
{
  aes_setup((AESContext *)ctx, blocklen, key, keylen);
}

void call_aes_encrypt(void * ctx, unsigned int * block)
{
  aes_encrypt((AESContext *)ctx, block);
}

void call_aes_decrypt(void * ctx, unsigned int * block)
{
  aes_decrypt((AESContext *)ctx, block);
}

