/*
 * Copyright 1993 by OpenVision Technologies, Inc.
 *
 * Copyright 2004 Vaclav Tomec <v_t_m@seznam.cz>
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appears in all copies and
 * that both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of OpenVision not be used
 * in advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. OpenVision makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * OPENVISION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL OPENVISION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

/*
 *  wrapper for GSSAPI shared library
 *  gssapi32.dll
 *  or
 *  libgssapi_krb5.so
 */

#ifndef GSSAPIW_H
#define GSSAPIW_H


typedef unsigned int gss_uint32;
typedef gss_uint32	OM_uint32;

typedef struct gss_OID_desc_struct {
      OM_uint32 length;
      void *elements;
} gss_OID_desc, *gss_OID;

typedef struct gss_OID_set_desc_struct  {
      size_t  count;
      gss_OID elements;
} gss_OID_set_desc, *gss_OID_set;

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

typedef	OM_uint32	gss_qop_t;

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
 * Flag bits for context-level services.
 */
#define GSS_C_DELEG_FLAG 1
#define GSS_C_MUTUAL_FLAG 2
#define GSS_C_REPLAY_FLAG 4
#define GSS_C_SEQUENCE_FLAG 8
#define GSS_C_CONF_FLAG 16
#define GSS_C_INTEG_FLAG 32
#define	GSS_C_ANON_FLAG 64
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
#define	GSS_C_NULL_OID		GSS_C_NO_OID
#define	GSS_C_NULL_OID_SET	GSS_C_NO_OID_SET


/* Reserved static storage for GSS_oids.  Comments are quotes from RFC 2744. */

static gss_OID_desc oids[] = {
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

gss_OID GSS_C_NT_USER_NAME           = oids+0;
gss_OID gss_nt_user_name             = oids+0;

gss_OID GSS_C_NT_MACHINE_UID_NAME    = oids+1;
gss_OID gss_nt_machine_uid_name      = oids+1;

gss_OID GSS_C_NT_STRING_UID_NAME     = oids+2;
gss_OID gss_nt_string_uid_name       = oids+2;

gss_OID GSS_C_NT_HOSTBASED_SERVICE_X = oids+3;
gss_OID gss_nt_service_name_v2       = oids+3;

gss_OID GSS_C_NT_HOSTBASED_SERVICE   = oids+4;
gss_OID gss_nt_service_name          = oids+4;

gss_OID GSS_C_NT_ANONYMOUS           = oids+5;

gss_OID GSS_C_NT_EXPORT_NAME         = oids+6;
gss_OID gss_nt_exported_name         = oids+6;


#define GSS_S_COMPLETE 0



#ifdef _WINDOWS

#define GSSAPISHLIB "gssapi32"

#define DECLARE_LIB_FUNCTION(ret, func, params) \
	ret	(CALLBACK* func)params;

#define LOAD_LIB_FUNCTION(ret, func, params, origin) \
	if (retval) \
	{ \
		func = (ret (CALLBACK*) params) GetProcAddress (lib, origin); \
	} \
	if (!func) \
		retval = 0;

#else

#include <dlfcn.h>

#define GSSAPISHLIB "libgssapi_krb5.so"

#define DECLARE_LIB_FUNCTION(ret, func, params) \
	ret	(*func)params;

#define LOAD_LIB_FUNCTION(ret, func, params, origin) \
	if (retval) \
	{ \
		func = (ret (*)params) dlsym (lib, origin); \
	} \
	if (!func) \
		retval = 0;

#endif



DECLARE_LIB_FUNCTION(OM_uint32,
						gss_indicate_mechs, (OM_uint32 *, gss_OID_set *))

DECLARE_LIB_FUNCTION(OM_uint32,
						gss_display_status, (OM_uint32 *, OM_uint32, int, gss_OID, OM_uint32 *, gss_buffer_t))

DECLARE_LIB_FUNCTION(OM_uint32,
						gss_release_buffer, (OM_uint32 *, gss_buffer_t))

DECLARE_LIB_FUNCTION(OM_uint32,
						gss_delete_sec_context, (OM_uint32 *, gss_ctx_id_t *, gss_buffer_t))

DECLARE_LIB_FUNCTION(OM_uint32,
						gss_release_name, (OM_uint32 *, gss_name_t *))

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
						 OM_uint32 *))

DECLARE_LIB_FUNCTION(OM_uint32,
						gss_import_name, (OM_uint32 *, gss_buffer_t, gss_OID, gss_name_t *))

DECLARE_LIB_FUNCTION(OM_uint32,
						gss_get_mic,
						(OM_uint32 *,
					    gss_ctx_id_t,
					    gss_qop_t,
					    gss_buffer_t,
					    gss_buffer_t))



int
shlibloadfcs(void *lib) {
	int retval = 1;

	if (lib == NULL)
		retval = 0;

	LOAD_LIB_FUNCTION(OM_uint32,
						gss_indicate_mechs, (OM_uint32 *, gss_OID_set *),
						"gss_indicate_mechs")

	LOAD_LIB_FUNCTION(OM_uint32,
						gss_display_status, (OM_uint32 *, OM_uint32, int, gss_OID, OM_uint32 *, gss_buffer_t),
						"gss_display_status")

	LOAD_LIB_FUNCTION(OM_uint32,
						gss_release_buffer, (OM_uint32 *, gss_buffer_t),
						"gss_release_buffer")

	LOAD_LIB_FUNCTION(OM_uint32,
						gss_delete_sec_context, (OM_uint32 *, gss_ctx_id_t *, gss_buffer_t),
						"gss_delete_sec_context")

	LOAD_LIB_FUNCTION(OM_uint32,
						gss_release_name, (OM_uint32 *, gss_name_t *),
						"gss_release_name")

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
						"gss_init_sec_context")

	LOAD_LIB_FUNCTION(OM_uint32,
						gss_import_name, (OM_uint32 *, gss_buffer_t, gss_OID, gss_name_t *),
						"gss_import_name")

	LOAD_LIB_FUNCTION(OM_uint32,
						gss_get_mic,
						(OM_uint32 *,
					    gss_ctx_id_t,
					    gss_qop_t,
					    gss_buffer_t,
					    gss_buffer_t),
						"gss_get_mic")


	return retval;
}

#endif /* GSSAPIW_H */
