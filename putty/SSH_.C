#ifdef SSPI_MECH

#define SECURITY_WIN32
#include "putty.h"
#include <security.h>

static SecurityFunctionTable SecurityFunctions;
static HMODULE SecurityLibrary = NULL;

static SECURITY_STATUS SEC_ENTRY DummyAcquireCredentialsHandleA(
  SEC_CHAR SEC_FAR * pszPrincipal, SEC_CHAR SEC_FAR * pszPackage,
  unsigned long fCredentialUse, void SEC_FAR * pvLogonId, void SEC_FAR * pAuthData,
  SEC_GET_KEY_FN pGetKeyFn, void SEC_FAR * pvGetKeyArgument,
  PCredHandle phCredential, PTimeStamp ptsExpiry)
{
  return ERROR_NOT_SUPPORTED;
}

#undef AcquireCredentialsHandle
#define AcquireCredentialsHandle \
  ((SecurityFunctions.AcquireCredentialsHandleA != NULL) ? \
    SecurityFunctions.AcquireCredentialsHandleA : \
    DummyAcquireCredentialsHandleA)
#undef QueryCredentialsAttributes
#define QueryCredentialsAttributes SecurityFunctions.QueryCredentialsAttributesA
#undef FreeCredentialsHandle
#define FreeCredentialsHandle SecurityFunctions.FreeCredentialsHandle
#undef QueryContextAttributes
#define QueryContextAttributes SecurityFunctions.QueryContextAttributesA
#undef MakeSignature
#define MakeSignature SecurityFunctions.MakeSignature
#undef VerifySignature
#define VerifySignature SecurityFunctions.VerifySignature
#undef InitializeSecurityContext
#define InitializeSecurityContext SecurityFunctions.InitializeSecurityContextA
#undef DeleteSecurityContext
#define DeleteSecurityContext SecurityFunctions.DeleteSecurityContext
#undef FreeContextBuffer
#define FreeContextBuffer SecurityFunctions.FreeContextBuffer

#endif

#include "ssh.c"

#include "puttyexp.h"

void sspi_init()
{
  memset(&SecurityFunctions, 0, sizeof(SecurityFunctions));
  SecurityLibrary = LoadLibrary("secur32.dll");
  if (SecurityLibrary != NULL)
  {
    INIT_SECURITY_INTERFACE_A AInitSecurityInterfaceA =
      (INIT_SECURITY_INTERFACE_A)GetProcAddress(SecurityLibrary, "InitSecurityInterfaceA");
    if (AInitSecurityInterfaceA != NULL)
    {
      PSecurityFunctionTableA ASecurityFunctions =  AInitSecurityInterfaceA();
      if (ASecurityFunctions != NULL)
      {
        memcpy(&SecurityFunctions, ASecurityFunctions, sizeof(SecurityFunctions));
      }
    }
  }
}

void sspi_cleanup()
{
  if (SecurityLibrary != NULL)
  {
    FreeLibrary(SecurityLibrary);
    SecurityLibrary = NULL;
    memset(&SecurityFunctions, 0, sizeof(SecurityFunctions));
  }
}

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
  int Result;
  if (SecurityFunctions.AcquireCredentialsHandleA == NULL)
  {
    Result = FALSE;
  }
  else
  {
    CredHandle Credentials;
    TimeStamp Expiry;
    if (AcquireCredentialsHandle(NULL, "Kerberos",
          SECPKG_CRED_OUTBOUND, NULL, NULL, NULL, NULL, &Credentials,
          &Expiry) == SEC_E_OK)
    {
      FreeCredentialsHandle(&Credentials);
      Result = TRUE;
    }
    else
    {
      Result = FALSE;
    }
  }
  return Result;
}
