#ifndef PUTTY_PGSSAPI_H
#define PUTTY_PGSSAPI_H

#include "putty.h"

#ifndef NO_GSSAPI

/*
 * On Unix, if we're statically linking against GSSAPI, we leave the
 * declaration of all this lot to the official header. If we're
 * dynamically linking, we declare it ourselves, because that avoids
 * us needing the official header at compile time.
 *
 * However, we still need the function pointer types, because even
 * with statically linked GSSAPI we use the ssh_gss_library wrapper.
 */
#ifdef STATIC_GSSAPI
#include <gssapi/gssapi.h>
typedef gss_OID const_gss_OID;	       /* for our prototypes below */
#else /* STATIC_GSSAPI */

/*******************************************************************************
 *  GSSAPI Definitions, taken from RFC 2744
 ******************************************************************************/

/* GSSAPI Type Definitions */
typedef uint32_t OM_uint32;

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

typedef struct gss_channel_bindings_struct {
    OM_uint32 initiator_addrtype;
    gss_buffer_desc initiator_address;
    OM_uint32 acceptor_addrtype;
    gss_buffer_desc acceptor_address;
    gss_buffer_desc application_data;
} *gss_channel_bindings_t;

typedef void * gss_ctx_id_t;
typedef void * gss_name_t;
typedef void * gss_cred_id_t;

typedef OM_uint32 gss_qop_t;
typedef int gss_cred_usage_t;

/* Flag bits for context-level services. */

#define GSS_C_DELEG_FLAG      1
#define GSS_C_MUTUAL_FLAG     2
#define GSS_C_REPLAY_FLAG     4
#define GSS_C_SEQUENCE_FLAG   8
#define GSS_C_CONF_FLAG       16
#define GSS_C_INTEG_FLAG      32
#define GSS_C_ANON_FLAG       64
#define GSS_C_PROT_READY_FLAG 128
#define GSS_C_TRANS_FLAG      256

/* Credential usage options */
#define GSS_C_BOTH     0
#define GSS_C_INITIATE 1
#define GSS_C_ACCEPT   2

/*-
 * RFC 2744 Page 86
 * Expiration time of 2^32-1 seconds means infinite lifetime for a
 * credential or security context
 */
#define GSS_C_INDEFINITE 0xfffffffful

/* Status code types for gss_display_status */
#define GSS_C_GSS_CODE  1
#define GSS_C_MECH_CODE 2

/* The constant definitions for channel-bindings address families */
#define GSS_C_AF_UNSPEC     0
#define GSS_C_AF_LOCAL      1
#define GSS_C_AF_INET       2
#define GSS_C_AF_IMPLINK    3
#define GSS_C_AF_PUP        4
#define GSS_C_AF_CHAOS      5
#define GSS_C_AF_NS         6
#define GSS_C_AF_NBS        7
#define GSS_C_AF_ECMA       8
#define GSS_C_AF_DATAKIT    9
#define GSS_C_AF_CCITT      10
#define GSS_C_AF_SNA        11
#define GSS_C_AF_DECnet     12
#define GSS_C_AF_DLI        13
#define GSS_C_AF_LAT        14
#define GSS_C_AF_HYLINK     15
#define GSS_C_AF_APPLETALK  16
#define GSS_C_AF_BSC        17
#define GSS_C_AF_DSS        18
#define GSS_C_AF_OSI        19
#define GSS_C_AF_X25        21

#define GSS_C_AF_NULLADDR   255

/* Various Null values */
#define GSS_C_NO_NAME ((gss_name_t) 0)
#define GSS_C_NO_BUFFER ((gss_buffer_t) 0)
#define GSS_C_NO_OID ((gss_OID) 0)
#define GSS_C_NO_OID_SET ((gss_OID_set) 0)
#define GSS_C_NO_CONTEXT ((gss_ctx_id_t) 0)
#define GSS_C_NO_CREDENTIAL ((gss_cred_id_t) 0)
#define GSS_C_NO_CHANNEL_BINDINGS ((gss_channel_bindings_t) 0)
#define GSS_C_EMPTY_BUFFER {0, NULL}

/* Major status codes */
#define GSS_S_COMPLETE 0

/* Some "helper" definitions to make the status code macros obvious. */
#define GSS_C_CALLING_ERROR_OFFSET 24
#define GSS_C_ROUTINE_ERROR_OFFSET 16

#define GSS_C_SUPPLEMENTARY_OFFSET 0
#define GSS_C_CALLING_ERROR_MASK 0377ul
#define GSS_C_ROUTINE_ERROR_MASK 0377ul
#define GSS_C_SUPPLEMENTARY_MASK 0177777ul

/*
 * The macros that test status codes for error conditions.
 * Note that the GSS_ERROR() macro has changed slightly from
 * the V1 GSS-API so that it now evaluates its argument
 * only once.
 */
#define GSS_CALLING_ERROR(x)                                            \
    (x & (GSS_C_CALLING_ERROR_MASK << GSS_C_CALLING_ERROR_OFFSET))
#define GSS_ROUTINE_ERROR(x)                                            \
    (x & (GSS_C_ROUTINE_ERROR_MASK << GSS_C_ROUTINE_ERROR_OFFSET))
#define GSS_SUPPLEMENTARY_INFO(x)                                       \
    (x & (GSS_C_SUPPLEMENTARY_MASK << GSS_C_SUPPLEMENTARY_OFFSET))
#define GSS_ERROR(x)                                                    \
    (x & ((GSS_C_CALLING_ERROR_MASK << GSS_C_CALLING_ERROR_OFFSET) |    \
          (GSS_C_ROUTINE_ERROR_MASK << GSS_C_ROUTINE_ERROR_OFFSET)))

/* Now the actual status code definitions */

/* Calling errors: */
#define GSS_S_CALL_INACCESSIBLE_READ            \
    (1ul << GSS_C_CALLING_ERROR_OFFSET)
#define GSS_S_CALL_INACCESSIBLE_WRITE           \
    (2ul << GSS_C_CALLING_ERROR_OFFSET)
#define GSS_S_CALL_BAD_STRUCTURE                \
    (3ul << GSS_C_CALLING_ERROR_OFFSET)

/* Routine errors: */
#define GSS_S_BAD_MECH             (1ul <<                      \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_BAD_NAME             (2ul <<                      \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_BAD_NAMETYPE         (3ul <<                      \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_BAD_BINDINGS         (4ul <<                      \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_BAD_STATUS           (5ul <<                      \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_BAD_SIG              (6ul <<                      \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_BAD_MIC GSS_S_BAD_SIG
#define GSS_S_NO_CRED              (7ul <<                      \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_NO_CONTEXT           (8ul <<                      \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_DEFECTIVE_TOKEN      (9ul <<                      \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_DEFECTIVE_CREDENTIAL (10ul <<                     \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_CREDENTIALS_EXPIRED  (11ul <<                     \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_CONTEXT_EXPIRED      (12ul <<                     \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_FAILURE              (13ul <<                     \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_BAD_QOP              (14ul <<                     \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_UNAUTHORIZED         (15ul <<                     \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_UNAVAILABLE          (16ul <<                     \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_DUPLICATE_ELEMENT    (17ul <<                     \
                                    GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_NAME_NOT_MN          (18ul <<                     \
                                    GSS_C_ROUTINE_ERROR_OFFSET)

/* Supplementary info bits: */
#define GSS_S_CONTINUE_NEEDED                                           \
                           (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 0))
#define GSS_S_DUPLICATE_TOKEN                                           \
                           (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 1))
#define GSS_S_OLD_TOKEN                                                 \
                           (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 2))
#define GSS_S_UNSEQ_TOKEN                                               \
                           (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 3))
#define GSS_S_GAP_TOKEN                                                 \
                           (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 4))

extern const_gss_OID GSS_C_NT_USER_NAME;
extern const_gss_OID GSS_C_NT_MACHINE_UID_NAME;
extern const_gss_OID GSS_C_NT_STRING_UID_NAME;
extern const_gss_OID GSS_C_NT_HOSTBASED_SERVICE_X;
extern const_gss_OID GSS_C_NT_HOSTBASED_SERVICE;
extern const_gss_OID GSS_C_NT_ANONYMOUS;
extern const_gss_OID GSS_C_NT_EXPORT_NAME;

#endif /* STATIC_GSSAPI */

extern const gss_OID GSS_MECH_KRB5;

/* GSSAPI functions we use.
 * TODO: Replace with all GSSAPI functions from RFC?
 */

/* Calling convention, just in case we need one. */
#ifndef GSS_CC
#define GSS_CC
#endif /*GSS_CC*/

typedef OM_uint32 (GSS_CC *t_gss_release_cred)
            (OM_uint32                    * /*minor_status*/,
             gss_cred_id_t                * /*cred_handle*/);

typedef OM_uint32 (GSS_CC *t_gss_init_sec_context)
            (OM_uint32                    * /*minor_status*/,
             const gss_cred_id_t            /*initiator_cred_handle*/,
             gss_ctx_id_t                 * /*context_handle*/,
             const gss_name_t               /*target_name*/,
             const gss_OID                  /*mech_type*/,
             OM_uint32                      /*req_flags*/,
             OM_uint32                      /*time_req*/,
             const gss_channel_bindings_t   /*input_chan_bindings*/,
             const gss_buffer_t             /*input_token*/,
             gss_OID                      * /*actual_mech_type*/,
             gss_buffer_t                   /*output_token*/,
             OM_uint32                    * /*ret_flags*/,
             OM_uint32                    * /*time_rec*/);

typedef OM_uint32 (GSS_CC *t_gss_delete_sec_context)
            (OM_uint32                    * /*minor_status*/,
             gss_ctx_id_t                 * /*context_handle*/,
             gss_buffer_t                   /*output_token*/);

typedef OM_uint32 (GSS_CC *t_gss_get_mic)
            (OM_uint32                    * /*minor_status*/,
             const gss_ctx_id_t             /*context_handle*/,
             gss_qop_t                      /*qop_req*/,
             const gss_buffer_t             /*message_buffer*/,
             gss_buffer_t                   /*msg_token*/);

typedef OM_uint32 (GSS_CC *t_gss_verify_mic)
            (OM_uint32                    * /*minor_status*/,
             const gss_ctx_id_t             /*context_handle*/,
             const gss_buffer_t             /*message_buffer*/,
             const gss_buffer_t             /*msg_token*/,
             gss_qop_t                    * /*qop_state*/);

typedef OM_uint32 (GSS_CC *t_gss_display_status)
            (OM_uint32                   * /*minor_status*/,
             OM_uint32                     /*status_value*/,
             int                           /*status_type*/,
             const gss_OID                 /*mech_type*/,
             OM_uint32                   * /*message_context*/,
             gss_buffer_t                  /*status_string*/);


typedef OM_uint32 (GSS_CC *t_gss_import_name)
            (OM_uint32                   * /*minor_status*/,
             const gss_buffer_t            /*input_name_buffer*/,
             const_gss_OID                 /*input_name_type*/,
             gss_name_t                  * /*output_name*/);


typedef OM_uint32 (GSS_CC *t_gss_release_name)
            (OM_uint32                   * /*minor_status*/,
             gss_name_t                  * /*name*/);

typedef OM_uint32 (GSS_CC *t_gss_release_buffer)
            (OM_uint32                   * /*minor_status*/,
             gss_buffer_t                  /*buffer*/);

typedef OM_uint32 (GSS_CC *t_gss_acquire_cred)
            (OM_uint32                    * /*minor_status*/,
             const gss_name_t               /*desired_name*/,
             OM_uint32                      /*time_req*/,
             const gss_OID_set              /*desired_mechs*/,
             gss_cred_usage_t               /*cred_usage*/,
             gss_cred_id_t                * /*output_cred_handle*/,
             gss_OID_set                  * /*actual_mechs*/,
             OM_uint32                    * /*time_rec*/);

typedef OM_uint32 (GSS_CC *t_gss_inquire_cred_by_mech)
            (OM_uint32                    * /*minor_status*/,
             const gss_cred_id_t            /*cred_handle*/,
             const gss_OID                  /*mech_type*/,
             gss_name_t                   * /*name*/,
             OM_uint32                    * /*initiator_lifetime*/,
             OM_uint32                    * /*acceptor_lifetime*/,
             gss_cred_usage_t             * /*cred_usage*/);

struct gssapi_functions {
    t_gss_delete_sec_context delete_sec_context;
    t_gss_display_status display_status;
    t_gss_get_mic get_mic;
    t_gss_verify_mic verify_mic;
    t_gss_import_name import_name;
    t_gss_init_sec_context init_sec_context;
    t_gss_release_buffer release_buffer;
    t_gss_release_cred release_cred;
    t_gss_release_name release_name;
    t_gss_acquire_cred acquire_cred;
    t_gss_inquire_cred_by_mech inquire_cred_by_mech;
};

#endif /* NO_GSSAPI */

#endif /* PUTTY_PGSSAPI_H */
