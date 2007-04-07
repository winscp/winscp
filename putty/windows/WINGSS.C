/*
 * wingss.c: Windows-specific GSSAPI stuff. Specifically, supplies
 * the interface to the SSPI library, which apparently implements
 * much the same functionality but with a different API.
 */ 

#include "putty.h"
#include "sshint.h"
#include "sshgss.h"

#define SECURITY_WIN32
#include <security.h>
#include <ntsecapi.h>


/*********************************************************************************/

ACQUIRE_CREDENTIALS_HANDLE_FN   p_AcquireCredentialsHandle   = NULL;
INITIALIZE_SECURITY_CONTEXT_FN  p_InitializeSecurityContext  = NULL;
QUERY_CONTEXT_ATTRIBUTES_FN     p_QueryContextAttributes     = NULL;
MAKE_SIGNATURE_FN               p_MakeSignature              = NULL;
DELETE_SECURITY_CONTEXT_FN      p_DeleteSecurityContext      = NULL;
FREE_CONTEXT_BUFFER_FN          p_FreeContextBuffer          = NULL;
FREE_CREDENTIALS_HANDLE_FN      p_FreeCredentialsHandle      = NULL;

/*********************************************************************************/

static void ssh_sspi_display_status(Ssh ssh, Gssctxt *ctxt)
{
    logeventf(ssh, "SSPI error: major: %8.8x minor: %8.8x",
	      ctxt->major_status, ctxt->minor_status);

    if (ctxt->major_status) {
        LPVOID lpMsgBuf;

        FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
		      FORMAT_MESSAGE_FROM_SYSTEM |
		      FORMAT_MESSAGE_IGNORE_INSERTS,
                      NULL,
                      ctxt->major_status,
                      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                      (LPTSTR) &lpMsgBuf,
                      0,
                      NULL);

        logeventf(ssh, "SSPI error: %s", lpMsgBuf);
        if (flags & FLAG_VERBOSE) {
            char *msgbuf;
            msgbuf = dupprintf("SSPI error: %s\r\n", (LPCTSTR)lpMsgBuf);
            c_write_str(ssh, msgbuf);
            sfree(msgbuf);
        }

        LocalFree(lpMsgBuf);
    }
}

/*********************************************************************************/

static OM_uint32 ssh_sspi_acquire_cred(Ssh ssh, Gssctxt **ctxt)
{
    PCredHandle  pcred_handle = NULL;
    CredHandle cred_handle = {0,0};

    logevent("GSSAPI: ssh_sspi_acquire_cred");

    (*ctxt)->major_status =
        p_AcquireCredentialsHandle(
				   NULL,                       // no principal name
				   MICROSOFT_KERBEROS_NAME_A,  // package name
				   SECPKG_CRED_OUTBOUND,
				   NULL,                       // no logon id
				   NULL,                       // auth data
				   NULL,                       // no get key fn
				   NULL,                       // noget key arg
				   &cred_handle,
				   NULL
				   );

    if ((*ctxt)->major_status == SEC_E_OK) {

        pcred_handle = snew(CredHandle);
        pcred_handle->dwLower = cred_handle.dwLower;
        pcred_handle->dwUpper = cred_handle.dwUpper;

        (*ctxt)->cred_handle = pcred_handle;
        ssh_gssapi_set_oid(*ctxt, gss_mech_krb5);

    } else {
        ssh_sspi_display_status(ssh, *ctxt);
    }

    return (*ctxt)->major_status;
}

/*********************************************************************************/

static OM_uint32
    ssh_sspi_init_sec_context(Ssh ssh,
			      Gssctxt *ctxt,
			      int deleg_creds,
			      gss_buffer_desc *recv_tok,
			      gss_buffer_desc *send_tok,
			      OM_uint32 *flags)
{
    SecBuffer       lrecv_tok, lsend_tok;
    SecBufferDesc   input_desc, output_desc;

    CtxtHandle newcontext_handle = {0,0};
    PCtxtHandle pctxt_handle = NULL;

    const char *host = ssh->savedhost;
    char *service_princ_name;
    unsigned long deleg_flag =  ISC_REQ_MUTUAL_AUTH |
								ISC_REQ_ALLOCATE_MEMORY |
								ISC_REQ_INTEGRITY;

    logevent("GSSAPI: ssh_sspi_init_sec_context");

	if (ssh->cfg.gssapi_server_realm[0])
	    service_princ_name = dupprintf("host/%s@%s", host, ssh->cfg.gssapi_server_realm);
	else
	    service_princ_name = dupprintf("host/%s", host);

    if (deleg_creds) {
        deleg_flag |= ISC_REQ_DELEGATE;
        logevent("GSSAPI: delegating credentials");
    }

    if (recv_tok && recv_tok->value) {
        lrecv_tok.cbBuffer = recv_tok->length;
        lrecv_tok.pvBuffer = recv_tok->value;
    } else {
        lrecv_tok.cbBuffer = 0;
        lrecv_tok.pvBuffer = NULL;
    }
    lrecv_tok.BufferType = SECBUFFER_TOKEN;

    input_desc.cBuffers = 1;
    input_desc.pBuffers = &lrecv_tok;
    input_desc.ulVersion = SECBUFFER_VERSION;

    lsend_tok.cbBuffer = send_tok->length;
    lsend_tok.pvBuffer = send_tok->value;
    lsend_tok.BufferType = SECBUFFER_TOKEN;

    output_desc.cBuffers = 1;
    output_desc.pBuffers = &lsend_tok;
    output_desc.ulVersion = SECBUFFER_VERSION;


    ctxt->major_status =
        p_InitializeSecurityContext(
				    (PCredHandle) ctxt->cred_handle,
				    (PCtxtHandle) ctxt->context_handle,
				    service_princ_name,
				    deleg_flag,
				    0,        // reserved
				    SECURITY_NATIVE_DREP,
				    &input_desc,
				    0,        // reserved
				    &newcontext_handle,
				    &output_desc,
				    flags,
				    NULL
				    );

    pctxt_handle = snew(CtxtHandle);
    pctxt_handle->dwLower = newcontext_handle.dwLower;
    pctxt_handle->dwUpper = newcontext_handle.dwUpper;
    ctxt->context_handle = pctxt_handle;

    sfree(service_princ_name);

    if (lsend_tok.pvBuffer) {
        send_tok->length = lsend_tok.cbBuffer;
        send_tok->value = lsend_tok.pvBuffer;
    }

    if (ctxt->major_status != SEC_E_OK &&
        ctxt->major_status != SEC_I_CONTINUE_NEEDED)
    {
        p_DeleteSecurityContext(&newcontext_handle);
        ssh_sspi_display_status(ssh, ctxt);
    }

    if (ctxt->major_status == SEC_I_CONTINUE_NEEDED)
        ctxt->major_status = GSS_S_CONTINUE_NEEDED;

    return ctxt->major_status;
}

/*********************************************************************************/

static int ssh_sspi_integ_flag_set(OM_uint32 gss_flags)
{
    return gss_flags & ISC_REQ_INTEGRITY;
}

/*********************************************************************************/

static OM_uint32 ssh_sspi_get_mic(Gssctxt *ctxt,
				  gss_buffer_t buffer, gss_buffer_t hash)
{
    OM_uint32 major_status = 0;
    SecPkgContext_Sizes ContextSizes;
    SecBufferDesc InputBufferDescriptor;
    SecBuffer InputSecurityToken[2];

    memset(&ContextSizes, 0, sizeof(ContextSizes));

    major_status =
        p_QueryContextAttributes(
				 (PCtxtHandle) ctxt->context_handle,
				 SECPKG_ATTR_SIZES,
				 &ContextSizes
				 );

    if (major_status != SEC_E_OK ||
        ContextSizes.cbMaxSignature == 0)
        return major_status;

    //--------------------------------------------------------------------
    // Build the buffer descriptor and the buffers
    // to pass to the MakeSignature call.
    InputBufferDescriptor.cBuffers = 2;
    InputBufferDescriptor.pBuffers = InputSecurityToken;
    InputBufferDescriptor.ulVersion = SECBUFFER_VERSION;

    //--------------------------------------------------------------------
    // Build a security buffer for the message itself. If
    // the SECBUFFER_READONLY attribute is set, the buffer will not be
    // signed.
    InputSecurityToken[0].BufferType = SECBUFFER_DATA;
    InputSecurityToken[0].cbBuffer = buffer->length;
    InputSecurityToken[0].pvBuffer = buffer->value;

    //--------------------------------------------------------------------
    // Allocate and build a security buffer for the message
    // signature.
    InputSecurityToken[1].BufferType = SECBUFFER_TOKEN;
    InputSecurityToken[1].cbBuffer = ContextSizes.cbMaxSignature;
    InputSecurityToken[1].pvBuffer = snewn(ContextSizes.cbMaxSignature, char);

    major_status =
        p_MakeSignature(
			(PCtxtHandle) ctxt->context_handle,
			0,                          // no quality of service
			&InputBufferDescriptor,     // input message descriptor
			0                           // no sequence number
			);

    if (major_status == SEC_E_OK) {
        hash->length = InputSecurityToken[1].cbBuffer;
        hash->value = InputSecurityToken[1].pvBuffer;
    }

    return major_status;
}

/*********************************************************************************/

static OM_uint32 ssh_sspi_delete_sec_context(Gssctxt **ctxt)
{
    OM_uint32 major_status = 0;

    if ((*ctxt) != NULL && (*ctxt)->context_handle) {
        major_status =
            p_DeleteSecurityContext((PCtxtHandle) (*ctxt)->context_handle);
        (*ctxt)->context_handle = GSS_C_NO_CONTEXT;
    }

    return major_status;
}

/*********************************************************************************/

static OM_uint32 ssh_sspi_release_buffer(OM_uint32 * minor_status,
					 gss_buffer_t buffer)
{
	p_FreeContextBuffer(buffer->value);

    buffer->length = 0;
    *minor_status = 0;

    return 0;
}

/*********************************************************************************/

static OM_uint32 ssh_sspi_release_cred(Gssctxt **ctxt)
{
    OM_uint32 major_status = 0;

    if ((*ctxt) != NULL && (*ctxt)->cred_handle) {
        major_status =
            p_FreeCredentialsHandle((PCredHandle) (*ctxt)->cred_handle);
        (*ctxt)->cred_handle = GSS_C_NO_CREDENTIAL;
    }

    return major_status;
}

/*********************************************************************************/

/*
 * Set correct SSPI functions
 */
static void ssh_sspi_init(void)
{
    ssh_gssapi_display_status = ssh_sspi_display_status;
    ssh_gssapi_acquire_cred = ssh_sspi_acquire_cred;
    ssh_gssapi_init_sec_context = ssh_sspi_init_sec_context;
    ssh_gssapi_integ_flag_set = ssh_sspi_integ_flag_set;
    ssh_gssapi_get_mic = ssh_sspi_get_mic;
    ssh_gssapi_delete_sec_context = ssh_sspi_delete_sec_context;
    ssh_gssapi_release_buffer = ssh_sspi_release_buffer;
    ssh_gssapi_release_cred = ssh_sspi_release_cred;
}

/*********************************************************************************/

#define LOAD_SSPI_FUNCTION(ret, func, origin) do { \
    if (retval) { \
        func = (ret) dyn_lib_symbol(lib, origin); \
	    if (!func) \
            retval = FALSE; \
    } \
} while (0)

/*********************************************************************************/

static int ssh_sspi_loadlib(Ssh ssh) {
    Library *lib;
    int retval = TRUE;
    char *error;
    Filename libfname = {"secur32.dll"};


    if ((lib = load_dyn_lib(libfname, &error))) {

        LOAD_SSPI_FUNCTION(ACQUIRE_CREDENTIALS_HANDLE_FN,
			   p_AcquireCredentialsHandle,
			   "AcquireCredentialsHandleA");

        LOAD_SSPI_FUNCTION(INITIALIZE_SECURITY_CONTEXT_FN,
			   p_InitializeSecurityContext,
			   "InitializeSecurityContextA");

        LOAD_SSPI_FUNCTION(MAKE_SIGNATURE_FN,
			   p_MakeSignature,
			   "MakeSignature");

        LOAD_SSPI_FUNCTION(QUERY_CONTEXT_ATTRIBUTES_FN,
			   p_QueryContextAttributes,
			   "QueryContextAttributesA");

        LOAD_SSPI_FUNCTION(DELETE_SECURITY_CONTEXT_FN,
			   p_DeleteSecurityContext,
			   "DeleteSecurityContext");

        LOAD_SSPI_FUNCTION(FREE_CONTEXT_BUFFER_FN,
			   p_FreeContextBuffer,
			   "FreeContextBuffer");

        LOAD_SSPI_FUNCTION(FREE_CREDENTIALS_HANDLE_FN,
			   p_FreeCredentialsHandle,
			   "FreeCredentialsHandle");
    } else {
        retval = FALSE;
        logeventf(ssh, "GSSAPI: error while loading library %s: %s", libfname.path, error);
		sfree(error);
    }

    return retval;
}

/*********************************************************************************/

int ssh_gssapi_init(Ssh ssh)
{
    int retval = FALSE;

    if (ssh_gss_loadlib(ssh)) {
        ssh_gss_init();
        retval = TRUE;
    } else {
        /*
         * check for acceptable OS - Windows 2000 and later
         * only beginning with this version, Kerberos is available in SSPI
         */
        OSVERSIONINFO osvi;
        memset(&osvi, 0, sizeof(OSVERSIONINFO));
        osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);

        if (GetVersionEx(&osvi)) {
            if (osvi.dwMajorVersion >= 5 && ssh_sspi_loadlib(ssh)) {
                ssh_sspi_init();
                retval = TRUE;
            }
        }
    }

    return retval;
}

/*********************************************************************************/
