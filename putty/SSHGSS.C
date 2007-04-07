/*
 * sshgss.c: common GSSAPI stuff to all platforms.
 */

#include "putty.h"
#include "ssh.h"
#include "sshgss.h"

const int has_gssapi = TRUE;


/* Reserved static storage for GSS_oids.  Comments are quotes from RFC 2744. */

static const gss_OID_desc oids[] = {
    /*
     * The implementation must reserve static storage for a
     * gss_OID_desc object containing the value */
    {10, (void *)"\x2a\x86\x48\x86\xf7\x12\x01\x02\x01\x01"},
    /* corresponding to an object-identifier value of
     * {iso(1) member-body(2) United States(840) mit(113554)
     * infosys(1) gssapi(2) generic(1) user_name(1)}.  The constant
     * GSS_C_NT_USER_NAME should be initialized to point
     * to that gss_OID_desc.
     */

    /*
     * The implementation must reserve static storage for a
     * gss_OID_desc object containing the value */
    {10, (void *)"\x2a\x86\x48\x86\xf7\x12\x01\x02\x01\x02"},
    /* corresponding to an object-identifier value of
     * {iso(1) member-body(2) United States(840) mit(113554)
     * infosys(1) gssapi(2) generic(1) machine_uid_name(2)}.
     * The constant GSS_C_NT_MACHINE_UID_NAME should be
     * initialized to point to that gss_OID_desc.
     */

    /*
    * The implementation must reserve static storage for a
    * gss_OID_desc object containing the value */
    {10, (void *)"\x2a\x86\x48\x86\xf7\x12\x01\x02\x01\x03"},
    /* corresponding to an object-identifier value of
    * {iso(1) member-body(2) United States(840) mit(113554)
    * infosys(1) gssapi(2) generic(1) string_uid_name(3)}.
    * The constant GSS_C_NT_STRING_UID_NAME should be
    * initialized to point to that gss_OID_desc.
    */

    /*
     * The implementation must reserve static storage for a
     * gss_OID_desc object containing the value */
    {6, (void *)"\x2b\x06\x01\x05\x06\x02"},
    /* corresponding to an object-identifier value of
     * {iso(1) org(3) dod(6) internet(1) security(5)
     * nametypes(6) gss-host-based-services(2)).  The constant
     * GSS_C_NT_HOSTBASED_SERVICE_X should be initialized to point
     * to that gss_OID_desc.  This is a deprecated OID value, and
     * implementations wishing to support hostbased-service names
     * should instead use the GSS_C_NT_HOSTBASED_SERVICE OID,
     * defined below, to identify such names;
     * GSS_C_NT_HOSTBASED_SERVICE_X should be accepted a synonym
     * for GSS_C_NT_HOSTBASED_SERVICE when presented as an input
     * parameter, but should not be emitted by GSS-API
     * implementations
     */

    /*
     * The implementation must reserve static storage for a
     * gss_OID_desc object containing the value */
    {10, (void *)"\x2a\x86\x48\x86\xf7\x12\x01\x02\x01\x04"},
    /* corresponding to an object-identifier value of
     * {iso(1) member-body(2) Unites States(840) mit(113554)
     * infosys(1) gssapi(2) generic(1) service_name(4)}.
     * The constant GSS_C_NT_HOSTBASED_SERVICE should be
     * initialized to point to that gss_OID_desc.
     */

    /*
     * The implementation must reserve static storage for a
     * gss_OID_desc object containing the value */
    {6, (void *)"\x2b\x06\01\x05\x06\x03"},
    /* corresponding to an object identifier value of
     * {1(iso), 3(org), 6(dod), 1(internet), 5(security),
     * 6(nametypes), 3(gss-anonymous-name)}.  The constant
     * and GSS_C_NT_ANONYMOUS should be initialized to point
     * to that gss_OID_desc.
     */

    /*
     * The implementation must reserve static storage for a
     * gss_OID_desc object containing the value */
    {6, (void *)"\x2b\x06\x01\x05\x06\x04"},
    /* corresponding to an object-identifier value of
     * {1(iso), 3(org), 6(dod), 1(internet), 5(security),
     * 6(nametypes), 4(gss-api-exported-name)}.  The constant
     * GSS_C_NT_EXPORT_NAME should be initialized to point
     * to that gss_OID_desc.
     */
};

/* Here are the constants which point to the static structure above.
 *
 * Constants of the form GSS_C_NT_* are specified by rfc 2744.
 *
 * Constants of the form gss_nt_* are the original MIT krb5 names
 * found in gssapi_generic.h.  They are provided for compatibility. */

const_gss_OID GSS_C_NT_USER_NAME           = oids+0;
const_gss_OID gss_nt_user_name             = oids+0;

const_gss_OID GSS_C_NT_MACHINE_UID_NAME    = oids+1;
const_gss_OID gss_nt_machine_uid_name      = oids+1;

const_gss_OID GSS_C_NT_STRING_UID_NAME     = oids+2;
const_gss_OID gss_nt_string_uid_name       = oids+2;

const_gss_OID GSS_C_NT_HOSTBASED_SERVICE_X = oids+3;
const_gss_OID gss_nt_service_name_v2       = oids+3;

const_gss_OID GSS_C_NT_HOSTBASED_SERVICE   = oids+4;
const_gss_OID gss_nt_service_name          = oids+4;

const_gss_OID GSS_C_NT_ANONYMOUS           = oids+5;

const_gss_OID GSS_C_NT_EXPORT_NAME         = oids+6;
const_gss_OID gss_nt_exported_name         = oids+6;

/*********************************************************************************/

void (*ssh_gssapi_display_status)(Ssh, Gssctxt *) = NULL;
OM_uint32 (*ssh_gssapi_acquire_cred)(Ssh, Gssctxt **) = NULL;
OM_uint32 (*ssh_gssapi_init_sec_context)(Ssh, Gssctxt *, int,
					 gss_buffer_desc *, gss_buffer_desc *,
					 OM_uint32 *) = NULL;
int (*ssh_gssapi_integ_flag_set)(OM_uint32) = NULL;
OM_uint32 (*ssh_gssapi_get_mic)(Gssctxt *, gss_buffer_t, gss_buffer_t) = NULL;
OM_uint32 (*ssh_gssapi_delete_sec_context)(Gssctxt **) = NULL;
OM_uint32 (*ssh_gssapi_release_buffer)(OM_uint32 *, gss_buffer_t) = NULL;
OM_uint32 (*ssh_gssapi_release_cred)(Gssctxt **) = NULL;

/*********************************************************************************/

/* Check that the OID in a data stream matches that in the context */
static int ssh_gssapi_check_oid(Gssctxt *ctx, void *data, size_t len)
{
    return (ctx != NULL && ctx->oid != GSS_C_NO_OID &&
        ctx->oid->length == len &&
        memcmp(ctx->oid->elements, data, len) == 0);
}

/*********************************************************************************/

/* Set the contexts OID */
void ssh_gssapi_set_oid(Gssctxt *ctx, const_gss_OID oid)
{
    void *data = oid->elements;
    size_t len = oid->length;

    if (ctx->oid != GSS_C_NO_OID) {
        sfree(ctx->oid->elements);
        sfree(ctx->oid);
    }
    ctx->oid = snew(gss_OID_desc);
    ctx->oid->length = len;
    ctx->oid->elements = snewn(len, char);
    memcpy(ctx->oid->elements, data, len);
}

/*********************************************************************************/

/*
 * Initialise our GSSAPI context. We use this opaque structure to contain all
 * of the data which both the client and server need to persist across
 * {accept,init}_sec_context calls, so that when we do it from the userauth
 * stuff life is a little easier
 */
void ssh_gssapi_build_ctx(Gssctxt **ctx)
{
    *ctx = snew(Gssctxt);
    (*ctx)->major_status = 0;
    (*ctx)->minor_status = 0;
    (*ctx)->context_handle = GSS_C_NO_CONTEXT;
    (*ctx)->cred_handle = GSS_C_NO_CREDENTIAL;
    (*ctx)->oid = GSS_C_NO_OID;
}

/*********************************************************************************/

/* Delete our context, providing it has been built correctly */
void ssh_gssapi_delete_ctx(Gssctxt **ctxt)
{
    if ((*ctxt) == NULL)
        return;

    if ((*ctxt)->context_handle != GSS_C_NO_CONTEXT)
        ssh_gssapi_delete_sec_context(ctxt);

    if ((*ctxt)->cred_handle != GSS_C_NO_CREDENTIAL)
        ssh_gssapi_release_cred(ctxt);

    if ((*ctxt)->oid != GSS_C_NO_OID) {
        sfree((*ctxt)->oid->elements);
        sfree((*ctxt)->oid);
        (*ctxt)->oid = GSS_C_NO_OID;
    }

    sfree(*ctxt);
    *ctxt = NULL;
}

/*********************************************************************************/

static OM_uint32 ssh_gssapi_sign(Ssh ssh, Gssctxt *ctxt,
				 gss_buffer_t buffer, gss_buffer_t hash)
{
    ctxt->major_status =
        ssh_gssapi_get_mic(ctxt, buffer, hash);

    if (GSS_ERROR(ctxt->major_status)) {
        ssh_gssapi_display_status(ssh, ctxt);
    }

    return ctxt->major_status;
}

/*********************************************************************************/

void input_gssapi_errtok(Ssh ssh, struct Packet *pktin, Gssctxt *ctxt)
{
    gss_buffer_desc send_tok = GSS_C_EMPTY_BUFFER;
    gss_buffer_desc recv_tok;
    OM_uint32 status, ms;
    char *val;
    unsigned len;

    ssh_pkt_getstring(pktin, &val, &len);
    recv_tok.value = val;
    recv_tok.length = len;

    /* Stick it into GSSAPI and see what it says */
    status = ssh_gssapi_init_sec_context(ssh,
                    ctxt,
                    ssh->cfg.gssapi_fwd_tgt,
                    &recv_tok,
                    &send_tok,
                    NULL);

    ssh_gssapi_release_buffer(&ms, &send_tok);

    /* Server will be returning a failed packet after this one */
}

/*********************************************************************************/

static OM_uint32 process_gssapi_token(Ssh ssh, Gssctxt *gssctxt,
				      gss_buffer_t recv_tok, char *username)
{
    gss_buffer_desc send_tok = GSS_C_EMPTY_BUFFER;
    gss_buffer_desc gssbuf, mic;
    OM_uint32 status, ms, gss_flags;
    int micoffset;
    struct Packet *pktout;

    status = ssh_gssapi_init_sec_context(ssh,
                    gssctxt,
                    ssh->cfg.gssapi_fwd_tgt,
                    recv_tok,
                    &send_tok,
                    &gss_flags);

    if (send_tok.length > 0) {
        if (GSS_ERROR(status))
            pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_GSSAPI_ERRTOK);
        else
            pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_GSSAPI_TOKEN);

        ssh2_pkt_addstring_start(pktout);
        ssh2_pkt_addstring_data(pktout, send_tok.value, send_tok.length);
        ssh2_pkt_send(ssh, pktout);
        ssh_gssapi_release_buffer(&ms, &send_tok);
    }

    if (status == GSS_S_COMPLETE) {
        /* send either complete or MIC, depending on mechanism */
        if (!ssh_gssapi_integ_flag_set(gss_flags)) {
            pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_GSSAPI_EXCHANGE_COMPLETE);
            ssh2_pkt_send(ssh, pktout);
        } else {
            /* build the mic using the pktout */
            pktout = ssh2_pkt_init(0); /* no type will not be sent */
            micoffset = pktout->length; /* save offset of actual start */
            ssh2_pkt_addstring_start(pktout);
            ssh2_pkt_addstring_data(pktout, ssh->v2_session_id, 20);
            ssh2_pkt_addbyte(pktout, SSH2_MSG_USERAUTH_REQUEST);
            ssh2_pkt_addstring(pktout, username);
            ssh2_pkt_addstring(pktout, "ssh-connection");
            ssh2_pkt_addstring(pktout, "gssapi-with-mic");

            gssbuf.value = pktout->data + micoffset;
            gssbuf.length = pktout->length - micoffset;

            status = ssh_gssapi_sign(ssh, gssctxt, &gssbuf, &mic);

            if (!GSS_ERROR(status)) {
                pktout = ssh2_pkt_init(SSH2_MSG_USERAUTH_GSSAPI_MIC);
                ssh2_pkt_addstring_start(pktout);
                ssh2_pkt_addstring_data(pktout, mic.value, mic.length);
                ssh2_pkt_send(ssh, pktout);
            }

            ssh_gssapi_release_buffer(&ms, &mic);
        }
    }

    return status;
}

/*********************************************************************************/

int input_gssapi_response(Ssh ssh, struct Packet *pktin,
			  Gssctxt *gssctxt, char *username)
{
    int oidlen;
    char *oidv;

    /* Setup our OID */
    ssh_pkt_getstring(pktin, &oidv, &oidlen);

    if (oidlen <= 2 ||
        oidv[0] != SSH_GSS_OIDTYPE ||
        oidv[1] != oidlen - 2) {
        logevent("GSSAPI: Badly encoded mechanism OID received");
        return TRUE;
    }

    if (!ssh_gssapi_check_oid(gssctxt, oidv + 2, oidlen - 2))
        bombout(("GSSAPI: Server returned different OID than expected"));

    if (GSS_ERROR(process_gssapi_token(ssh, gssctxt, GSS_C_NO_BUFFER, username))) {
        /* Start again with next method on list */
        return TRUE;
    }

    return FALSE;
}

/*********************************************************************************/

int input_gssapi_token(Ssh ssh, struct Packet *pktin,
		       Gssctxt *gssctxt, char *username)
{
    gss_buffer_desc recv_tok;
    char *sval;
    unsigned slen;

    ssh_pkt_getstring(pktin, &sval, &slen);
    recv_tok.value = sval;
    recv_tok.length = slen;

    if (GSS_ERROR(process_gssapi_token(ssh, gssctxt, &recv_tok, username))) {
        /* Start again with the next method in the list */
        return TRUE;
    }

    return FALSE;
}

/*********************************************************************************/

void input_gssapi_error(Ssh ssh, struct Packet *pktin)
{
    OM_uint32 maj, min;
    char *msg;
    char *lang;

    maj = ssh_pkt_getuint32(pktin);
    min = ssh_pkt_getuint32(pktin);
    ssh_pkt_getstring(pktin, &msg, NULL);
    ssh_pkt_getstring(pktin, &lang, NULL);

    logeventf(ssh, "GSSAPI Server Error: %s", msg);
}

/*********************************************************************************/
