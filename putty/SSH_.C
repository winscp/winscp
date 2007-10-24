#include "ssh.c"

#include "puttyexp.h"

void ssh_close(void * handle)
{
  ssh_do_close((Ssh)handle, FALSE);
}

int is_ssh(void * handle)
{
  Plug fn = (Plug)handle;
  return (*fn)->closing == ssh_closing;
}

void call_ssh_timer(void * handle)
{
  if (((Ssh)handle)->version == 2)
  {
    ssh2_timer(handle, GETTICKCOUNT());
  }
}

int get_ssh_version(void * handle)
{
  return ((Ssh)handle)->version;
}

void * get_ssh_frontend(void * handle)
{
  return ((Ssh)handle)->frontend;
}

int get_ssh1_compressing(void * handle)
{
  return ((Ssh)handle)->v1_compressing;
}

const struct ssh_cipher * get_cipher(void * handle)
{
  return ((Ssh)handle)->cipher;
}

const struct ssh2_cipher * get_cscipher(void * handle)
{
  return ((Ssh)handle)->cscipher;
}

const struct ssh2_cipher * get_sccipher(void * handle)
{
  return ((Ssh)handle)->sccipher;
}

const struct ssh_compress * get_cscomp(void * handle)
{
  return ((Ssh)handle)->cscomp;
}

const struct ssh_compress * get_sccomp(void * handle)
{
  return ((Ssh)handle)->sccomp;
}

int get_ssh_state(void * handle)
{
  return ((Ssh)handle)->state;
}

int get_ssh_state_closed(void * handle)
{
  return ((Ssh)handle)->state == SSH_STATE_CLOSED;
}

int get_ssh_state_session(void * handle)
{
  return ((Ssh)handle)->state == SSH_STATE_SESSION;
}

int get_ssh_exitcode(void * handle)
{
  return ssh_return_exitcode(handle);
}

const unsigned int * ssh2_remmaxpkt(void * handle)
{
  return &((Ssh)handle)->mainchan->v.v2.remmaxpkt;
}

const unsigned int * ssh2_remwindow(void * handle)
{
  return &((Ssh)handle)->mainchan->v.v2.remwindow;
}

void md5checksum(const char * buffer, int len, unsigned char output[16])
{
  struct MD5Context md5c;
  MD5Init(&md5c);
  MD5Update(&md5c, buffer, len);
  MD5Final(output, &md5c);
}

int has_gssapi_ssh()
{
  struct ssh_tag ssh;
  memset(&ssh, 0, sizeof(ssh));
  ssh.frontend = NULL;
  return ssh_gssapi_init(&ssh);
}
