/*
 * sshgssc.c: common GSSAPI code which should work on any platform
 * that permits dynamic loading of a library supporting the
 * standard GSSAPI interface.
 */

#include "putty.h"
#include "sshint.h"
#include "sshgss.h"

#define DECLARE_LIB_FUNCTION(ret, func, params) \
    ret (GSS_CALLBACK *func)params

#define LOAD_LIB_FUNCTION(ret, func, params, origin) do { \
    if (retval) { \
        func = (ret (GSS_CALLBACK *)params) dyn_lib_symbol(lib, origin); \
        if (!func) \
            retval = FALSE; \
    } \
} while (0)

/*********************************************************************************/

static const gss_OID_desc gss_mech_krb5_desc = 
    {9, (void*)"\052\206\110\206\367\022\001\002\002"};
const_gss_OID gss_mech_krb5 = &gss_mech_krb5_desc;

static const gss_OID_set_desc gss_mech_krb5_set_desc =
    {1, (gss_OID) &gss_mech_krb5_desc};

static const_gss_OID_set gss_mech_krb5_set = &gss_mech_krb5_set_desc;

DECLARE_LIB_FUNCTION(OM_uint32,
		     gss_indicate_mechs,
		     (OM_uint32 *,
		      const_gss_OID_set *));

DECLARE_LIB_FUNCTION(OM_uint32,
		     gss_display_status,
		     (OM_uint32 *,
		      OM_uint32,
		      int,
		      gss_OID,
		      OM_uint32 *,
		      gss_buffer_t));

DECLARE_LIB_FUNCTION(OM_uint32,
		     gss_release_buffer,
		     (OM_uint32 *,
		      gss_buffer_t));

DECLARE_LIB_FUNCTION(OM_uint32,
		     gss_delete_sec_context,
		     (OM_uint32 *,
		      gss_ctx_id_t *,
		      gss_buffer_t));

DECLARE_LIB_FUNCTION(OM_uint32,
		     gss_release_name,
		     (OM_uint32 *,
		      gss_name_t *));

DECLARE_LIB_FUNCTION(OM_uint32,
		     gss_init_sec_context,
		     (OM_uint32 *,
		      gss_cred_id_t,
		      gss_ctx_id_t *,
		      gss_name_t,
		      gss_OID,
		      OM_uint32,
		      OM_uint32,
		      gss_channel_bindings_t,
		      gss_buffer_t,
		      gss_OID *,
		      gss_buffer_t,
		      OM_uint32 *,
		      OM_uint32 *));

DECLARE_LIB_FUNCTION(OM_uint32,
		     gss_import_name,
		     (OM_uint32 *,
		      gss_buffer_t,
		      const_gss_OID,
		      gss_name_t *));

DECLARE_LIB_FUNCTION(OM_uint32,
		     gss_get_mic,
		     (OM_uint32 *,
		      gss_ctx_id_t,
		      gss_qop_t,
		      gss_buffer_t,
		      gss_buffer_t));

DECLARE_LIB_FUNCTION(OM_uint32,
		     gss_test_oid_set_member,
		     (OM_uint32 *,             /* minor_status */
		      const_gss_OID,           /* member */
		      const_gss_OID_set,       /* set */
		      int *                    /* present */
		      ));

DECLARE_LIB_FUNCTION(OM_uint32,
		     gss_acquire_cred,
		     (OM_uint32 *,             /*  minor_status */
		      const gss_name_t,        /* desired_name */
		      OM_uint32,               /* time_req */
		      const_gss_OID_set,       /* desired_mechs */
		      gss_cred_usage_t,        /* cred_usage */
		      gss_cred_id_t *,         /* output_cred_handle */
		      gss_OID_set *,           /* actual_mechs */
		      OM_uint32 *              /* time_rec */
		      ));


DECLARE_LIB_FUNCTION(OM_uint32,
		     gss_release_oid_set,
		     (OM_uint32 *,             /* minor_status */
		      const_gss_OID_set *));            /* set */

DECLARE_LIB_FUNCTION(OM_uint32,
		     gss_release_cred,
		     (OM_uint32 *,             /* minor_status */
		      gss_cred_id_t *));          /* cred_handle */


/*********************************************************************************/

/* All this effort to report an error ... */
static void ssh_gss_display_status(Ssh ssh, Gssctxt *ctxt)
{
    OM_uint32 lmin;
    gss_buffer_desc msg = GSS_C_EMPTY_BUFFER;
    OM_uint32 ctx;
    char *msgbuf;

    ctx = 0;
    /* The GSSAPI error */
    do {
        gss_display_status(&lmin, ctxt->major_status,
			   GSS_C_GSS_CODE, GSS_C_NULL_OID, &ctx, &msg);

        logeventf(ssh, "GSSAPI error: %s", msg.value);
        if (flags & FLAG_VERBOSE) {
            msgbuf = dupprintf("GSSAPI error: %s\r\n", msg.value);
            c_write_str(ssh, msgbuf);
            sfree(msgbuf);
        }

        gss_release_buffer(&lmin, &msg);
    } while (ctx != 0);

    /* The mechanism specific error */
    do {
        gss_display_status(&lmin, ctxt->minor_status,
			   GSS_C_MECH_CODE, GSS_C_NULL_OID, &ctx, &msg);

        logeventf(ssh, "GSSAPI mech specific error: %s", msg.value);
        if (flags & FLAG_VERBOSE) {
            msgbuf = dupprintf("GSSAPI mech specific error: %s\r\n", msg.value);
            c_write_str(ssh, msgbuf);
            sfree(msgbuf);
        }

        gss_release_buffer(&lmin, &msg);
    } while (ctx != 0);
}

/*********************************************************************************/

static OM_uint32 ssh_gss_acquire_cred(Ssh ssh, Gssctxt **ctxt)
{
    const_gss_OID_set     mech_set_use = GSS_C_NO_OID_SET;
    const_gss_OID_set     mech_set_available = GSS_C_NO_OID_SET;
    int                   present = 0;

    logevent("GSSAPI: ssh_gss_acquire_cred");

    /*
     * we may have multiple mechs, so we need to find
     * the one that has creds. we will prefer the Kerberos
     * if it present. If it fails will take any
     * With seam at least, we need to pass the mech_used to
     * gss_init_sec_context too.
     */
    (*ctxt)->major_status = gss_indicate_mechs(&(*ctxt)->minor_status,
					       &mech_set_available);

    if (GSS_ERROR((*ctxt)->major_status))
        ssh_gss_display_status(ssh, *ctxt);

    /* will skip any errors here, as we are trying to
     * work with more advancced GSSAPI with multipl mechs
     * but will use basic ones too
     */
    if ((*ctxt)->major_status == 0 && mech_set_available)  {
        gss_test_oid_set_member(&(*ctxt)->minor_status,
				gss_mech_krb5,
				mech_set_available,
				&present);
        if (present) {
            mech_set_use = gss_mech_krb5_set;
        }
    }

    (*ctxt)->major_status =
        gss_acquire_cred(&(*ctxt)->minor_status,
			 GSS_C_NO_NAME,
			 0,
			 mech_set_use,
			 GSS_C_INITIATE,
			 &(*ctxt)->cred_handle,
			 NULL,
			 NULL);

    if (GSS_ERROR((*ctxt)->major_status))
        ssh_gss_display_status(ssh, *ctxt);

    /* if failed and there are other mechs, try again */

    if ((*ctxt)->major_status && mech_set_use) {
        (*ctxt)->major_status =
            gss_acquire_cred(&(*ctxt)->minor_status,
			     GSS_C_NO_NAME,
			     0,
			     GSS_C_NO_OID_SET,
			     GSS_C_INITIATE,
			     &(*ctxt)->cred_handle,
			     NULL,
			     NULL);

        if (GSS_ERROR((*ctxt)->major_status))
            ssh_gss_display_status(ssh, *ctxt);
    }

    if (mech_set_use) {
        ssh_gssapi_set_oid(*ctxt, gss_mech_krb5);
    }

    if (mech_set_available)
        gss_release_oid_set(&(*ctxt)->minor_status, &mech_set_available);

    return (*ctxt)->major_status;
}

/*********************************************************************************/

static OM_uint32 ssh_gss_init_sec_context(Ssh ssh, Gssctxt *ctxt,
					  int deleg_creds,
					  gss_buffer_desc *recv_tok,
					  gss_buffer_desc *send_tok,
					  OM_uint32 *flags)
{
    gss_name_t name = GSS_C_NO_NAME;
    gss_buffer_desc gssbuf;
    const char *host = ssh->savedhost;
    int deleg_flag = GSS_C_MUTUAL_FLAG | GSS_C_INTEG_FLAG;

    logevent("GSSAPI: ssh_gss_init_sec_context");

    gssbuf.value = dupprintf("host@%s", host);
    gssbuf.length = strlen(gssbuf.value);

    ctxt->major_status =
        gss_import_name(&ctxt->minor_status,
			&gssbuf,
			GSS_C_NT_HOSTBASED_SERVICE,
			&name);

    if (GSS_ERROR(ctxt->major_status))
        ssh_gss_display_status(ssh, ctxt);

	sfree(gssbuf.value);

    if (deleg_creds) {
        deleg_flag |= GSS_C_DELEG_FLAG;
        logevent("GSSAPI: delegating credentials");
    }

    ctxt->major_status =
        gss_init_sec_context(&ctxt->minor_status,
			     ctxt->cred_handle,
			     &ctxt->context_handle,
			     name,
			     ctxt->oid,
			     deleg_flag,
			     0,
			     NULL,
			     recv_tok,
			     NULL,
			     send_tok,
			     flags,
			     NULL);

    if (GSS_ERROR(ctxt->major_status))
        ssh_gss_display_status(ssh, ctxt);

    gss_release_name(&ctxt->minor_status, &name);

    return ctxt->major_status;
}

/*********************************************************************************/

static int ssh_gss_integ_flag_set(OM_uint32 gss_flags)
{
    return gss_flags & GSS_C_INTEG_FLAG;
}

/*********************************************************************************/

static OM_uint32 ssh_gss_get_mic(Gssctxt *ctxt,
				 gss_buffer_t buffer, gss_buffer_t hash)
{
    return gss_get_mic(&ctxt->minor_status,
		       ctxt->context_handle,
		       GSS_C_QOP_DEFAULT,
		       buffer,
		       hash);
}

/*********************************************************************************/

static OM_uint32 ssh_gss_delete_sec_context(Gssctxt **ctxt)
{
    OM_uint32 major_status = 0;

    if ((*ctxt) != NULL && (*ctxt)->context_handle) {
        major_status =
            gss_delete_sec_context(&(*ctxt)->minor_status,
				   (*ctxt)->context_handle,
				   GSS_C_NO_BUFFER);
        (*ctxt)->context_handle = GSS_C_NO_CONTEXT;
    }

    return major_status;
}

/*********************************************************************************/

static OM_uint32 ssh_gss_release_buffer(OM_uint32 *minor_status,
					gss_buffer_t buffer)
{
    return gss_release_buffer(minor_status, buffer);
}

/*********************************************************************************/

static OM_uint32 ssh_gss_release_cred(Gssctxt **ctxt)
{
    OM_uint32 major_status = 0;

    if ((*ctxt) != NULL && (*ctxt)->cred_handle) {
        major_status =
            gss_release_cred(&(*ctxt)->minor_status,
			     (*ctxt)->cred_handle);
        (*ctxt)->cred_handle = GSS_C_NO_CREDENTIAL;
    }

    return major_status;
}

/*********************************************************************************/

/*
 * Set correct GSS functions
 */
void ssh_gss_init(void)
{
    ssh_gssapi_display_status = ssh_gss_display_status;
    ssh_gssapi_acquire_cred = ssh_gss_acquire_cred;
    ssh_gssapi_init_sec_context = ssh_gss_init_sec_context;
    ssh_gssapi_integ_flag_set = ssh_gss_integ_flag_set;
    ssh_gssapi_get_mic = ssh_gss_get_mic;
    ssh_gssapi_delete_sec_context = ssh_gss_delete_sec_context;
    ssh_gssapi_release_buffer = ssh_gss_release_buffer;
    ssh_gssapi_release_cred = ssh_gss_release_cred;
}

/*********************************************************************************/

int ssh_gss_loadlib(Ssh ssh)
{
    Library *lib;
    int retval = TRUE;
    char *error;
    Filename libfname = GSSAPI_SHLIB;

    if ((lib = load_dyn_lib(libfname, &error))) {

        LOAD_LIB_FUNCTION(OM_uint32,
			  gss_indicate_mechs, (OM_uint32 *, const_gss_OID_set *),
			  "gss_indicate_mechs");

        LOAD_LIB_FUNCTION(OM_uint32,
			  gss_display_status, (OM_uint32 *, OM_uint32, int, gss_OID,
					       OM_uint32 *, gss_buffer_t),
			  "gss_display_status");

        LOAD_LIB_FUNCTION(OM_uint32,
			  gss_release_buffer, (OM_uint32 *, gss_buffer_t),
			  "gss_release_buffer");

        LOAD_LIB_FUNCTION(OM_uint32,
			  gss_delete_sec_context, (OM_uint32 *, gss_ctx_id_t *, gss_buffer_t),
			  "gss_delete_sec_context");

        LOAD_LIB_FUNCTION(OM_uint32,
			  gss_release_name, (OM_uint32 *, gss_name_t *),
			  "gss_release_name");

        LOAD_LIB_FUNCTION(OM_uint32,
			  gss_init_sec_context,
			  (OM_uint32 *,
			   gss_cred_id_t,
			   gss_ctx_id_t *,
			   gss_name_t,
			   gss_OID,
			   OM_uint32,
			   OM_uint32,
			   gss_channel_bindings_t,
			   gss_buffer_t,
			   gss_OID *,
			   gss_buffer_t,
			   OM_uint32 *,
			   OM_uint32 *),
			  "gss_init_sec_context");

        LOAD_LIB_FUNCTION(OM_uint32,
			  gss_import_name, (OM_uint32 *, gss_buffer_t,
					    const_gss_OID, gss_name_t *),
			  "gss_import_name");

        LOAD_LIB_FUNCTION(OM_uint32,
			  gss_get_mic,
			  (OM_uint32 *,
			   gss_ctx_id_t,
			   gss_qop_t,
			   gss_buffer_t,
			   gss_buffer_t),
			  "gss_get_mic");

        LOAD_LIB_FUNCTION(OM_uint32,
			  gss_test_oid_set_member,
			  (OM_uint32 *,             /* minor_status */
			   const_gss_OID,           /* member */
			   const_gss_OID_set,       /* set */
			   int *),                    /* present */
			  "gss_test_oid_set_member");

        LOAD_LIB_FUNCTION(OM_uint32,
			  gss_acquire_cred,
			  (OM_uint32 *,             /*  minor_status */
			   const gss_name_t,        /* desired_name */
			   OM_uint32,               /* time_req */
			   const_gss_OID_set,       /* desired_mechs */
			   gss_cred_usage_t,        /* cred_usage */
			   gss_cred_id_t *,         /* output_cred_handle */
			   gss_OID_set *,           /* actual_mechs */
			   OM_uint32 *),              /* time_rec */
			  "gss_acquire_cred");

        LOAD_LIB_FUNCTION(OM_uint32,
			  gss_release_oid_set,
			  (OM_uint32 *,             /* minor_status */
			   const_gss_OID_set *),            /* set */
			  "gss_release_oid_set");

        LOAD_LIB_FUNCTION(OM_uint32,
			  gss_release_cred,
			  (OM_uint32 *,             /* minor_status */
			   gss_cred_id_t *),          /* cred_handle */
			  "gss_release_cred");
    } else {
        retval = FALSE;
        logeventf(ssh, "GSSAPI: error while loading library %s: %s", libfname.path, error);
        sfree(error);
    }

    return retval;
}

/*********************************************************************************/
