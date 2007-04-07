/*
 * sshgss.h: GSSAPI-related definitions.
 */

#ifndef PUTTY_SSHGSS_H
#define PUTTY_SSHGSS_H

#include <stddef.h>
#include "sshint.h"

/* draft-ietf-secsh-gsskeyex-06 */
#define SSH2_MSG_USERAUTH_GSSAPI_RESPONSE           60
#define SSH2_MSG_USERAUTH_GSSAPI_TOKEN              61
#define SSH2_MSG_USERAUTH_GSSAPI_EXCHANGE_COMPLETE  63
#define SSH2_MSG_USERAUTH_GSSAPI_ERROR              64
#define SSH2_MSG_USERAUTH_GSSAPI_ERRTOK             65
#define SSH2_MSG_USERAUTH_GSSAPI_MIC                66

#define SSH_GSS_OIDTYPE     0x06

#define SSH2_PKTCTX_GSSAPI  0x0080


typedef unsigned int gss_uint32;
typedef gss_uint32  OM_uint32;

typedef struct gss_OID_desc_struct {
      OM_uint32 length;
      void *elements;
} gss_OID_desc;
typedef const gss_OID_desc *const_gss_OID;
typedef gss_OID_desc *gss_OID;

typedef struct gss_OID_set_desc_struct  {
      size_t  count;
      gss_OID elements;
} gss_OID_set_desc;
typedef const gss_OID_set_desc *const_gss_OID_set;
typedef gss_OID_set_desc *gss_OID_set;

typedef struct gss_buffer_desc_struct {
      size_t length;
      void *value;
} gss_buffer_desc, *gss_buffer_t;

typedef void * gss_ctx_id_t;

typedef void * gss_name_t;

typedef void * gss_cred_id_t;

typedef struct gss_channel_bindings_struct {
      OM_uint32 initiator_addrtype;
      gss_buffer_desc initiator_address;
      OM_uint32 acceptor_addrtype;
      gss_buffer_desc acceptor_address;
      gss_buffer_desc application_data;
} *gss_channel_bindings_t;

typedef OM_uint32   gss_qop_t;

typedef int gss_cred_usage_t;

typedef struct {
    OM_uint32       major_status;      /* both */
    OM_uint32       minor_status;      /* both */
    gss_ctx_id_t    context_handle;    /* both */
    gss_cred_id_t   cred_handle;
    gss_OID         oid;               /* client */
} Gssctxt;

/*********************************************************************************/


/*
 * Various Null values.
 */
#define GSS_C_NO_NAME ((gss_name_t) 0)
#define GSS_C_NO_BUFFER ((gss_buffer_t) 0)
#define GSS_C_NO_OID ((gss_OID) 0)
#define GSS_C_NO_OID_SET ((gss_OID_set) 0)
#define GSS_C_NO_CONTEXT ((gss_ctx_id_t) 0)
#define GSS_C_NO_CREDENTIAL ((gss_cred_id_t) 0)
#define GSS_C_NO_CHANNEL_BINDINGS ((gss_channel_bindings_t) 0)
#define GSS_C_EMPTY_BUFFER {0, NULL}

/*
 * Status code types for gss_display_status
 */
#define GSS_C_GSS_CODE 1
#define GSS_C_MECH_CODE 2

/*
 * Credential usage options
 */
#define GSS_C_BOTH 0
#define GSS_C_INITIATE 1
#define GSS_C_ACCEPT 2

/*
 * Flag bits for context-level services.
 */
#define GSS_C_DELEG_FLAG 1
#define GSS_C_MUTUAL_FLAG 2
#define GSS_C_REPLAY_FLAG 4
#define GSS_C_SEQUENCE_FLAG 8
#define GSS_C_CONF_FLAG 16
#define GSS_C_INTEG_FLAG 32
#define GSS_C_ANON_FLAG 64
#define GSS_C_PROT_READY_FLAG 128
#define GSS_C_TRANS_FLAG 256

#define GSS_C_QOP_DEFAULT 0

/*
 * Some "helper" definitions to make the status code macros obvious.
 */
#define GSS_C_CALLING_ERROR_OFFSET 24
#define GSS_C_ROUTINE_ERROR_OFFSET 16
#define GSS_C_SUPPLEMENTARY_OFFSET 0
#define GSS_C_CALLING_ERROR_MASK ((OM_uint32) 0377ul)
#define GSS_C_ROUTINE_ERROR_MASK ((OM_uint32) 0377ul)
#define GSS_C_SUPPLEMENTARY_MASK ((OM_uint32) 0177777ul)

#define GSS_ERROR(x) \
  ((x) & ((GSS_C_CALLING_ERROR_MASK << GSS_C_CALLING_ERROR_OFFSET) | \
      (GSS_C_ROUTINE_ERROR_MASK << GSS_C_ROUTINE_ERROR_OFFSET)))

/*
 * Various Null values.
 */
#define GSS_C_NO_NAME ((gss_name_t) 0)
#define GSS_C_NO_BUFFER ((gss_buffer_t) 0)
#define GSS_C_NO_OID ((gss_OID) 0)
#define GSS_C_NO_OID_SET ((gss_OID_set) 0)
#define GSS_C_NO_CONTEXT ((gss_ctx_id_t) 0)
#define GSS_C_NO_CREDENTIAL ((gss_cred_id_t) 0)
#define GSS_C_NO_CHANNEL_BINDINGS ((gss_channel_bindings_t) 0)
#define GSS_C_EMPTY_BUFFER {0, NULL}

/*
 * Some alternate names for a couple of the above values.  These are defined
 * for V1 compatibility.
 */
#define GSS_C_NULL_OID      GSS_C_NO_OID
#define GSS_C_NULL_OID_SET  GSS_C_NO_OID_SET

/*
 * Supplementary info bits:
 */
#define GSS_S_CONTINUE_NEEDED (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 0))
#define GSS_S_DUPLICATE_TOKEN (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 1))
#define GSS_S_OLD_TOKEN (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 2))
#define GSS_S_UNSEQ_TOKEN (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 3))
#define GSS_S_GAP_TOKEN (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 4))


#define GSS_S_COMPLETE 0

/* Constants of the form GSS_C_NT_* are specified by rfc 2744.
 *
 * Constants of the form gss_nt_* are the original MIT krb5 names
 * found in gssapi_generic.h.  They are provided for compatibility. */

extern const_gss_OID GSS_C_NT_USER_NAME;
extern const_gss_OID gss_nt_user_name;

extern const_gss_OID GSS_C_NT_MACHINE_UID_NAME;
extern const_gss_OID gss_nt_machine_uid_name;

extern const_gss_OID GSS_C_NT_STRING_UID_NAME;
extern const_gss_OID gss_nt_string_uid_name;

extern const_gss_OID GSS_C_NT_HOSTBASED_SERVICE_X;
extern const_gss_OID gss_nt_service_name_v2;

extern const_gss_OID GSS_C_NT_HOSTBASED_SERVICE;
extern const_gss_OID gss_nt_service_name;

extern const_gss_OID GSS_C_NT_ANONYMOUS;

extern const_gss_OID GSS_C_NT_EXPORT_NAME;
extern const_gss_OID gss_nt_exported_name;

extern const_gss_OID gss_mech_krb5;

/*
 * Function definitions.
 */

int ssh_gssapi_init(Ssh ssh);
void ssh_gssapi_build_ctx(Gssctxt **ctx);
void ssh_gssapi_delete_ctx(Gssctxt **ctxt);

void input_gssapi_errtok(Ssh ssh, struct Packet *pktin, Gssctxt *ctxt);
int input_gssapi_response(Ssh ssh, struct Packet *pktin,
			  Gssctxt *gssctxt, char *username);
int input_gssapi_token(Ssh ssh, struct Packet *pktin,
		       Gssctxt *gssctxt, char *username);
void input_gssapi_error(Ssh ssh, struct Packet *pktin);
void ssh_gssapi_set_oid(Gssctxt *ctx, const_gss_OID oid);

extern void (*ssh_gssapi_display_status)(Ssh, Gssctxt *);
extern OM_uint32 (*ssh_gssapi_acquire_cred)(Ssh, Gssctxt **);
extern OM_uint32 (*ssh_gssapi_init_sec_context)(Ssh, Gssctxt *, int,
						gss_buffer_desc *,
						gss_buffer_desc *,
						OM_uint32 *);
extern int (*ssh_gssapi_integ_flag_set)(OM_uint32);
extern OM_uint32 (*ssh_gssapi_get_mic)(Gssctxt *, gss_buffer_t, gss_buffer_t);
extern OM_uint32 (*ssh_gssapi_delete_sec_context)(Gssctxt **);
extern OM_uint32 (*ssh_gssapi_release_buffer)(OM_uint32 *, gss_buffer_t);
extern OM_uint32 (*ssh_gssapi_release_cred)(Gssctxt **);

void ssh_gss_init(void);
int ssh_gss_loadlib(Ssh ssh);

#endif /* PUTTY_SSHGSS_H */
