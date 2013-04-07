#include "sshsha.c"

#include "puttyexp.h"

void call_sha1_key_internal(void * handle, unsigned char * key, int len)
{
  sha1_key_internal(handle, key, len);
}
