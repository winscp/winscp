#ifndef NO_GSSAPI

#include "putty.h"

#define SECURITY_WIN32
#include <security.h>

#include "sshgss.h"
#include "misc.h"

#define NOTHING
#define DECL_SSPI_FUNCTION(linkage, rettype, name, params)	\
  typedef rettype (WINAPI *t_##name) params;			\
  linkage t_##name p_##name
#define GET_SSPI_FUNCTION(module, name)					\
  p_##name = module ? (t_##name) GetProcAddress(module, #name) : NULL

DECL_SSPI_FUNCTION(static, SECURITY_STATUS,
		   AcquireCredentialsHandleA,
		   (SEC_CHAR *, SEC_CHAR *, ULONG, PLUID,
		    PVOID, SEC_GET_KEY_FN, PVOID, PCredHandle, PTimeStamp));
DECL_SSPI_FUNCTION(static, SECURITY_STATUS,
		   InitializeSecurityContextA,
		   (PCredHandle, PCtxtHandle, SEC_CHAR *, ULONG, ULONG,
		   ULONG, PSecBufferDesc, ULONG, PCtxtHandle,
		    PSecBufferDesc, PULONG, PTimeStamp));
DECL_SSPI_FUNCTION(static, SECURITY_STATUS,
		   FreeContextBuffer,
		   (PVOID));
DECL_SSPI_FUNCTION(static, SECURITY_STATUS,
		   FreeCredentialsHandle,
		   (PCredHandle));
DECL_SSPI_FUNCTION(static, SECURITY_STATUS,
		   DeleteSecurityContext,
		   (PCtxtHandle));
DECL_SSPI_FUNCTION(static, SECURITY_STATUS,
		   QueryContextAttributesA,
		   (PCtxtHandle, ULONG, PVOID));
DECL_SSPI_FUNCTION(static, SECURITY_STATUS,
		   MakeSignature,
		   (PCtxtHandle, ULONG, PSecBufferDesc, ULONG));

static HMODULE security_module = NULL;

typedef struct winSsh_gss_ctx {
    unsigned long maj_stat;
    unsigned long min_stat;
    CredHandle cred_handle;
    CtxtHandle context;
    PCtxtHandle context_handle;
    TimeStamp expiry;
} winSsh_gss_ctx;


const Ssh_gss_buf gss_mech_krb5={9,"\x2A\x86\x48\x86\xF7\x12\x01\x02\x02"};

int ssh_gss_init(void)
{
    if (security_module)
	return 1;		       /* already initialised */

    security_module = LoadLibrary("secur32.dll");
    if (security_module) {
	GET_SSPI_FUNCTION(security_module, AcquireCredentialsHandleA);
	GET_SSPI_FUNCTION(security_module, InitializeSecurityContextA);
	GET_SSPI_FUNCTION(security_module, FreeContextBuffer);
	GET_SSPI_FUNCTION(security_module, FreeCredentialsHandle);
	GET_SSPI_FUNCTION(security_module, DeleteSecurityContext);
	GET_SSPI_FUNCTION(security_module, QueryContextAttributesA);
	GET_SSPI_FUNCTION(security_module, MakeSignature);
	return 1;
    }
    return 0;
}

Ssh_gss_stat ssh_gss_indicate_mech(Ssh_gss_buf *mech)
{
    *mech = gss_mech_krb5;
    return SSH_GSS_OK;
}


Ssh_gss_stat ssh_gss_import_name(char *host, Ssh_gss_name *srv_name)
{
    char *pStr;

    /* Check hostname */
    if (host == NULL) return SSH_GSS_FAILURE;
    
    /* copy it into form host/FQDN */
    pStr = dupcat("host/", host, NULL);

    *srv_name = (Ssh_gss_name) pStr;

    return SSH_GSS_OK;
}

Ssh_gss_stat ssh_gss_acquire_cred(Ssh_gss_ctx *ctx)
{
    winSsh_gss_ctx *winctx = snew(winSsh_gss_ctx);
    memset(winctx, 0, sizeof(winSsh_gss_ctx));

    /* prepare our "wrapper" structure */
    winctx->maj_stat =  winctx->min_stat = SEC_E_OK;
    winctx->context_handle = NULL;

    /* Specifying no principal name here means use the credentials of
       the current logged-in user */

    winctx->maj_stat = p_AcquireCredentialsHandleA(NULL,
						   "Kerberos",
						   SECPKG_CRED_OUTBOUND,
						   NULL,
						   NULL,
						   NULL,
						   NULL,
						   &winctx->cred_handle,
						   &winctx->expiry);

    if (winctx->maj_stat != SEC_E_OK) return SSH_GSS_FAILURE;
    
    *ctx = (Ssh_gss_ctx) winctx;
    return SSH_GSS_OK;
}

#ifdef MPEXT
static SecBuffer ssh_gss_init_sec_buffer(unsigned long buffersize,
  unsigned long buffertype, void * buffer)
{
  SecBuffer result;
  result.cbBuffer = buffersize;
  result.BufferType = buffertype;
  result.pvBuffer = buffer;
  return result;
}

static SecBufferDesc ssh_gss_init_sec_buffer_desc(unsigned long version,
  unsigned long bufferscount, PSecBuffer buffers)
{
  SecBufferDesc result;
  result.ulVersion = version;
  result.cBuffers = bufferscount;
  result.pBuffers = buffers;
  return result;
}

#define MPEXT_INIT_SEC_BUFFER(BUFFERSIZE, BUFFERTYPE, BUFFER) \
  ssh_gss_init_sec_buffer(BUFFERSIZE, BUFFERTYPE, BUFFER)
#define MPEXT_INIT_SEC_BUFFERDESC(VERSION, BUFFERSCOUNT, BUFFERS) \
  ssh_gss_init_sec_buffer_desc(VERSION, BUFFERSCOUNT, BUFFERS)

#else
#define MPEXT_INIT_SEC_BUFFER(BUFFERSIZE, BUFFERTYPE, BUFFER) \
  {BUFFERSIZE, BUFFERTYPE, BUFFER}
#define MPEXT_INIT_SEC_BUFFERDESC(VERSION, BUFFERSCOUNT, BUFFERS) \
  {VERSION, BUFFERSCOUNT, BUFFERS}
#endif

Ssh_gss_stat ssh_gss_init_sec_context(Ssh_gss_ctx *ctx,
				      Ssh_gss_name srv_name,
				      int to_deleg,
				      Ssh_gss_buf *recv_tok,
				      Ssh_gss_buf *send_tok)
{
    winSsh_gss_ctx *winctx = (winSsh_gss_ctx *) *ctx;
    SecBuffer wsend_tok = MPEXT_INIT_SEC_BUFFER(send_tok->length,SECBUFFER_TOKEN,send_tok->value);
    SecBuffer wrecv_tok = MPEXT_INIT_SEC_BUFFER(recv_tok->length,SECBUFFER_TOKEN,recv_tok->value);
    SecBufferDesc output_desc= MPEXT_INIT_SEC_BUFFERDESC(SECBUFFER_VERSION,1,&wsend_tok);
    SecBufferDesc input_desc = MPEXT_INIT_SEC_BUFFERDESC(SECBUFFER_VERSION,1,&wrecv_tok);
    unsigned long flags=ISC_REQ_MUTUAL_AUTH|ISC_REQ_REPLAY_DETECT|
	ISC_REQ_CONFIDENTIALITY|ISC_REQ_ALLOCATE_MEMORY;
    unsigned long ret_flags=0;
    
    /* check if we have to delegate ... */
    if (to_deleg) flags |= ISC_REQ_DELEGATE;
    winctx->maj_stat = p_InitializeSecurityContextA(&winctx->cred_handle,
						    winctx->context_handle,
						    (char*) srv_name,
						    flags,
						    0,          /* reserved */
						    SECURITY_NATIVE_DREP,
						    &input_desc,
						    0,          /* reserved */
						    &winctx->context,
						    &output_desc,
						    &ret_flags,
						    &winctx->expiry);
  
    /* prepare for the next round */
    winctx->context_handle = &winctx->context;
    send_tok->value = wsend_tok.pvBuffer;
    send_tok->length = wsend_tok.cbBuffer;
  
    /* check & return our status */
    if (winctx->maj_stat==SEC_E_OK) return SSH_GSS_S_COMPLETE;
    if (winctx->maj_stat==SEC_I_CONTINUE_NEEDED) return SSH_GSS_S_CONTINUE_NEEDED;
    
    return SSH_GSS_FAILURE;
}

Ssh_gss_stat ssh_gss_free_tok(Ssh_gss_buf *send_tok)
{
    /* check input */
    if (send_tok == NULL) return SSH_GSS_FAILURE;

    /* free Windows buffer */
    p_FreeContextBuffer(send_tok->value);
    SSH_GSS_CLEAR_BUF(send_tok);
    
    return SSH_GSS_OK;
}

Ssh_gss_stat ssh_gss_release_cred(Ssh_gss_ctx *ctx)
{
    winSsh_gss_ctx *winctx= (winSsh_gss_ctx *) *ctx;

    /* check input */
    if (winctx == NULL) return SSH_GSS_FAILURE;

    /* free Windows data */
    p_FreeCredentialsHandle(&winctx->cred_handle);
    #ifdef MPEXT
    if (winctx->context_handle != NULL)
    #endif
    p_DeleteSecurityContext(&winctx->context);

    /* delete our "wrapper" structure */
    sfree(winctx);
    *ctx = (Ssh_gss_ctx) NULL;

    return SSH_GSS_OK;
}


Ssh_gss_stat ssh_gss_release_name(Ssh_gss_name *srv_name)
{
    char *pStr= (char *) *srv_name;

    if (pStr == NULL) return SSH_GSS_FAILURE;
    sfree(pStr);
    *srv_name = (Ssh_gss_name) NULL;

    return SSH_GSS_OK;
}

Ssh_gss_stat ssh_gss_display_status(Ssh_gss_ctx ctx, Ssh_gss_buf *buf)
{
    winSsh_gss_ctx *winctx = (winSsh_gss_ctx *) ctx;
    char *msg;

    if (winctx == NULL) return SSH_GSS_FAILURE;

    /* decode the error code */
    switch (winctx->maj_stat) {
      case SEC_E_OK: msg="SSPI status OK"; break;
      case SEC_E_INVALID_HANDLE: msg="The handle passed to the function"
	    " is invalid.";
	break;
      case SEC_E_TARGET_UNKNOWN: msg="The target was not recognized."; break;
      case SEC_E_LOGON_DENIED: msg="The logon failed."; break;
      case SEC_E_INTERNAL_ERROR: msg="The Local Security Authority cannot"
	    " be contacted.";
	break;
      case SEC_E_NO_CREDENTIALS: msg="No credentials are available in the"
	    " security package.";
	break;
      case SEC_E_NO_AUTHENTICATING_AUTHORITY:
	msg="No authority could be contacted for authentication."
	    "The domain name of the authenticating party could be wrong,"
	    " the domain could be unreachable, or there might have been"
	    " a trust relationship failure.";
	break;
      case SEC_E_INSUFFICIENT_MEMORY:
	msg="One or more of the SecBufferDesc structures passed as"
	    " an OUT parameter has a buffer that is too small.";
	break;
      case SEC_E_INVALID_TOKEN:
	msg="The error is due to a malformed input token, such as a"
	    " token corrupted in transit, a token"
	    " of incorrect size, or a token passed into the wrong"
	    " security package. Passing a token to"
	    " the wrong package can happen if client and server did not"
	    " negotiate the proper security package.";
	break;
      default:
	msg = "Internal SSPI error";
	break;
    }

    buf->value = dupstr(msg);
    buf->length = strlen(buf->value);
    
    return SSH_GSS_OK;
}

Ssh_gss_stat ssh_gss_get_mic(Ssh_gss_ctx ctx, Ssh_gss_buf *buf,
			     Ssh_gss_buf *hash)
{
    winSsh_gss_ctx *winctx= (winSsh_gss_ctx *) ctx;
    SecPkgContext_Sizes ContextSizes;
    SecBufferDesc InputBufferDescriptor;
    SecBuffer InputSecurityToken[2];

    if (winctx == NULL) return SSH_GSS_FAILURE;
  
    winctx->maj_stat = 0;

    memset(&ContextSizes, 0, sizeof(ContextSizes));

    winctx->maj_stat = p_QueryContextAttributesA(&winctx->context,
						 SECPKG_ATTR_SIZES,
						 &ContextSizes);
    
    if (winctx->maj_stat != SEC_E_OK ||
	ContextSizes.cbMaxSignature == 0)
	return winctx->maj_stat;

    InputBufferDescriptor.cBuffers = 2;
    InputBufferDescriptor.pBuffers = InputSecurityToken;
    InputBufferDescriptor.ulVersion = SECBUFFER_VERSION;
    InputSecurityToken[0].BufferType = SECBUFFER_DATA;
    InputSecurityToken[0].cbBuffer = buf->length;
    InputSecurityToken[0].pvBuffer = buf->value;
    InputSecurityToken[1].BufferType = SECBUFFER_TOKEN;
    InputSecurityToken[1].cbBuffer = ContextSizes.cbMaxSignature;
    InputSecurityToken[1].pvBuffer = snewn(ContextSizes.cbMaxSignature, char);

    winctx->maj_stat = p_MakeSignature(&winctx->context,
				       0,
				       &InputBufferDescriptor,
				       0);

    if (winctx->maj_stat == SEC_E_OK) {
	hash->length = InputSecurityToken[1].cbBuffer;
	hash->value = InputSecurityToken[1].pvBuffer;
    }

    return winctx->maj_stat;
}

Ssh_gss_stat ssh_gss_free_mic(Ssh_gss_buf *hash)
{
    sfree(hash->value);
    return SSH_GSS_OK;
}

#else

/* Dummy function so this source file defines something if NO_GSSAPI
   is defined. */

int ssh_gss_init(void)
{
    return 0;
}

#endif
