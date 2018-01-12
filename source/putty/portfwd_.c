#include "portfwd.c"

#include "puttyexp.h"

int is_pfwd(void * handle)
{
  Plug fn = (Plug)handle;
  return ((*fn)->closing == pfd_closing);
}

int is_pfwd_listener(void * handle)
{
  Plug fn = (Plug)handle;
  return ((*fn)->accepting == pfd_accepting);
}

void * get_pfwd_backend(void * handle)
{
  return ((struct PFwdPrivate*)handle)->backhandle;
}
