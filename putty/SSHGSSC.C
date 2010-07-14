#include "putty.h"

#include <string.h>
#include "sshgssc.h"
#include "misc.h"

#ifndef NO_GSSAPI

static Ssh_gss_stat ssh_gssapi_indicate_mech(struct ssh_gss_library *lib,
					     Ssh_gss_buf *mech)
{
    /* Copy constant into mech */
    mech->length  = GSS_MECH_KRB5->length;
    mech->value = GSS_MECH_KRB5->elements;
    return SSH_GSS_OK;
}

static Ssh_gss_stat ssh_gssapi_import_name(struct ssh_gss_library *lib,
					   char *host,
					   Ssh_gss_name *srv_name)
{
    struct gssapi_functions *gss = &lib->u.gssapi;
    OM_uint32 min_stat,maj_stat;
    gss_buffer_desc host_buf;
    char *pStr;

    pStr = dupcat("host@", host, NULL);

    host_buf.value = pStr;
    host_buf.length = strlen(pStr);

    maj_stat = gss->import_name(&min_stat, &host_buf,
				GSS_C_NT_HOSTBASED_SERVICE, srv_name);
    /* Release buffer */
    sfree(pStr);
    if (maj_stat == GSS_S_COMPLETE) return SSH_GSS_OK;
    return SSH_GSS_FAILURE;
}

static Ssh_gss_stat ssh_gssapi_acquire_cred(struct ssh_gss_library *lib,
					    Ssh_gss_ctx *ctx)
{
    gssapi_ssh_gss_ctx *gssctx = snew(gssapi_ssh_gss_ctx);

    gssctx->maj_stat =  gssctx->min_stat = GSS_S_COMPLETE;
    gssctx->ctx = GSS_C_NO_CONTEXT;
    *ctx = (Ssh_gss_ctx) gssctx;

    return SSH_GSS_OK;
}

static Ssh_gss_stat ssh_gssapi_init_sec_context(struct ssh_gss_library *lib,
						Ssh_gss_ctx *ctx,
						Ssh_gss_name srv_name,
						int to_deleg,
						Ssh_gss_buf *recv_tok,
						Ssh_gss_buf *send_tok)
{
    struct gssapi_functions *gss = &lib->u.gssapi;
    gssapi_ssh_gss_ctx *gssctx = (gssapi_ssh_gss_ctx*) *ctx;
    OM_uint32 ret_flags;

    if (to_deleg) to_deleg = GSS_C_DELEG_FLAG;
    gssctx->maj_stat = gss->init_sec_context(&gssctx->min_stat,
					     GSS_C_NO_CREDENTIAL,
					     &gssctx->ctx,
					     srv_name,
					     (gss_OID) GSS_MECH_KRB5,
					     GSS_C_MUTUAL_FLAG |
					     GSS_C_INTEG_FLAG | to_deleg,
					     0,
					     GSS_C_NO_CHANNEL_BINDINGS,
					     recv_tok,
					     NULL,   /* ignore mech type */
					     send_tok,
					     &ret_flags,
					     NULL);  /* ignore time_rec */

    if (gssctx->maj_stat == GSS_S_COMPLETE) return SSH_GSS_S_COMPLETE;
    if (gssctx->maj_stat == GSS_S_CONTINUE_NEEDED) return SSH_GSS_S_CONTINUE_NEEDED;
    return SSH_GSS_FAILURE;
}

static Ssh_gss_stat ssh_gssapi_display_status(struct ssh_gss_library *lib,
					      Ssh_gss_ctx ctx,
					      Ssh_gss_buf *buf)
{
    struct gssapi_functions *gss = &lib->u.gssapi;
    gssapi_ssh_gss_ctx *gssctx = (gssapi_ssh_gss_ctx *) ctx;
    OM_uint32 lmin,lmax;
    OM_uint32 ccc;
    gss_buffer_desc msg_maj=GSS_C_EMPTY_BUFFER;
    gss_buffer_desc msg_min=GSS_C_EMPTY_BUFFER;

    /* Return empty buffer in case of failure */
    SSH_GSS_CLEAR_BUF(buf);

    /* get first mesg from GSS */
    ccc=0;
    lmax=gss->display_status(&lmin,gssctx->maj_stat,GSS_C_GSS_CODE,(gss_OID) GSS_MECH_KRB5,&ccc,&msg_maj);

    if (lmax != GSS_S_COMPLETE) return SSH_GSS_FAILURE;

    /* get first mesg from Kerberos */
    ccc=0;
    lmax=gss->display_status(&lmin,gssctx->min_stat,GSS_C_MECH_CODE,(gss_OID) GSS_MECH_KRB5,&ccc,&msg_min);

    if (lmax != GSS_S_COMPLETE) {
        gss->release_buffer(&lmin, &msg_maj);
        return SSH_GSS_FAILURE;
    }

    /* copy data into buffer */
    buf->length = msg_maj.length + msg_min.length + 1;
    buf->value = snewn(buf->length + 1, char);

    /* copy mem */
    memcpy((char *)buf->value, msg_maj.value, msg_maj.length);
    ((char *)buf->value)[msg_maj.length] = ' ';
    memcpy((char *)buf->value + msg_maj.length + 1, msg_min.value, msg_min.length);
    ((char *)buf->value)[buf->length] = 0;
    /* free mem & exit */
    gss->release_buffer(&lmin, &msg_maj);
    gss->release_buffer(&lmin, &msg_min);
    return SSH_GSS_OK;
}

static Ssh_gss_stat ssh_gssapi_free_tok(struct ssh_gss_library *lib,
					Ssh_gss_buf *send_tok)
{
    struct gssapi_functions *gss = &lib->u.gssapi;
    OM_uint32 min_stat,maj_stat;
    maj_stat = gss->release_buffer(&min_stat, send_tok);

    if (maj_stat == GSS_S_COMPLETE) return SSH_GSS_OK;
    return SSH_GSS_FAILURE;
}

static Ssh_gss_stat ssh_gssapi_release_cred(struct ssh_gss_library *lib,
					    Ssh_gss_ctx *ctx)
{
    struct gssapi_functions *gss = &lib->u.gssapi;
    gssapi_ssh_gss_ctx *gssctx = (gssapi_ssh_gss_ctx *) *ctx;
    OM_uint32 min_stat;
    OM_uint32 maj_stat=GSS_S_COMPLETE;

    if (gssctx == NULL) return SSH_GSS_FAILURE;
    if (gssctx->ctx != GSS_C_NO_CONTEXT)
        maj_stat = gss->delete_sec_context(&min_stat,&gssctx->ctx,GSS_C_NO_BUFFER);
    sfree(gssctx);

    if (maj_stat == GSS_S_COMPLETE) return SSH_GSS_OK;
    return SSH_GSS_FAILURE;
}


static Ssh_gss_stat ssh_gssapi_release_name(struct ssh_gss_library *lib,
					    Ssh_gss_name *srv_name)
{
    struct gssapi_functions *gss = &lib->u.gssapi;
    OM_uint32 min_stat,maj_stat;
    maj_stat = gss->release_name(&min_stat, srv_name);

    if (maj_stat == GSS_S_COMPLETE) return SSH_GSS_OK;
    return SSH_GSS_FAILURE;
}

static Ssh_gss_stat ssh_gssapi_get_mic(struct ssh_gss_library *lib,
				       Ssh_gss_ctx ctx, Ssh_gss_buf *buf,
				       Ssh_gss_buf *hash)
{
    struct gssapi_functions *gss = &lib->u.gssapi;
    gssapi_ssh_gss_ctx *gssctx = (gssapi_ssh_gss_ctx *) ctx;
    if (gssctx == NULL) return SSH_GSS_FAILURE;
    return gss->get_mic(&(gssctx->min_stat), gssctx->ctx, 0, buf, hash);
}

static Ssh_gss_stat ssh_gssapi_free_mic(struct ssh_gss_library *lib,
					Ssh_gss_buf *hash)
{
    /* On Unix this is the same freeing process as ssh_gssapi_free_tok. */
    return ssh_gssapi_free_tok(lib, hash);
}

void ssh_gssapi_bind_fns(struct ssh_gss_library *lib)
{
    lib->indicate_mech = ssh_gssapi_indicate_mech;
    lib->import_name = ssh_gssapi_import_name;
    lib->release_name = ssh_gssapi_release_name;
    lib->init_sec_context = ssh_gssapi_init_sec_context;
    lib->free_tok = ssh_gssapi_free_tok;
    lib->acquire_cred = ssh_gssapi_acquire_cred;
    lib->release_cred = ssh_gssapi_release_cred;
    lib->get_mic = ssh_gssapi_get_mic;
    lib->free_mic = ssh_gssapi_free_mic;
    lib->display_status = ssh_gssapi_display_status;
}

#else

/* Dummy function so this source file defines something if NO_GSSAPI
   is defined. */

int ssh_gssapi_init(void)
{
    return 0;
}

#endif
