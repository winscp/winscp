#include "winstore.c"

void putty_mungestr(const char *in, char *out)
{
  mungestr(in, out);
}

void putty_unmungestr(const char *in, char *out, int outlen)
{
  unmungestr(in, out, outlen);
}

void putty_get_seedpath(void)
{
  get_seedpath();
}

char * seedpath_ptr()
{
  return seedpath;
}

int seedpath_size()
{
  return sizeof(seedpath);
}

