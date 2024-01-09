/* This file actually defines the GSSAPI function pointers for
 * functions we plan to import from a GSSAPI library.
 */
#include "putty.h"

#ifndef NO_GSSAPI

#include "pgssapi.h"

#ifndef NO_LIBDL

/* Reserved static storage for GSS_oids.
 * Constants of the form GSS_C_NT_* are specified by rfc 2744.
 * Comments are quotes from RFC 2744 itself.
 *
 * These may be #defined to complex expressions by the local header
 * file, if we're including one in static-GSSAPI mode. (For example,
 * Heimdal defines them to things like
 * (&__gss_c_nt_user_name_oid_desc).) So we only define them if
 * needed. */

#ifndef GSS_C_NT_USER_NAME
static gss_OID_desc oid_GSS_C_NT_USER_NAME = {
    /* The implementation must reserve static storage for a
     * gss_OID_desc object containing the value */
    10, "\x2a\x86\x48\x86\xf7\x12\x01\x02\x01\x01",
    /* corresponding to an object-identifier value of
     * {iso(1) member-body(2) United States(840) mit(113554)
     * infosys(1) gssapi(2) generic(1) user_name(1)}.  The constant
     * GSS_C_NT_USER_NAME should be initialized to point
     * to that gss_OID_desc. */
};
const_gss_OID GSS_C_NT_USER_NAME = &oid_GSS_C_NT_USER_NAME;
#endif

#ifndef GSS_C_NT_MACHINE_UID_NAME
static gss_OID_desc oid_GSS_C_NT_MACHINE_UID_NAME = {
    /* The implementation must reserve static storage for a
     * gss_OID_desc object containing the value */
    10, "\x2a\x86\x48\x86\xf7\x12\x01\x02\x01\x02",
    /* corresponding to an object-identifier value of
     * {iso(1) member-body(2) United States(840) mit(113554)
     * infosys(1) gssapi(2) generic(1) machine_uid_name(2)}.
     * The constant GSS_C_NT_MACHINE_UID_NAME should be
     * initialized to point to that gss_OID_desc. */
};
const_gss_OID GSS_C_NT_MACHINE_UID_NAME = &oid_GSS_C_NT_MACHINE_UID_NAME;
#endif

#ifndef GSS_C_NT_STRING_UID_NAME
static gss_OID_desc oid_GSS_C_NT_STRING_UID_NAME = {
    /* The implementation must reserve static storage for a
     * gss_OID_desc object containing the value */
    10, "\x2a\x86\x48\x86\xf7\x12\x01\x02\x01\x03",
    /* corresponding to an object-identifier value of
     * {iso(1) member-body(2) United States(840) mit(113554)
     * infosys(1) gssapi(2) generic(1) string_uid_name(3)}.
     * The constant GSS_C_NT_STRING_UID_NAME should be
     * initialized to point to that gss_OID_desc. */
};
const_gss_OID GSS_C_NT_STRING_UID_NAME = &oid_GSS_C_NT_STRING_UID_NAME;
#endif

#ifndef GSS_C_NT_HOSTBASED_SERVICE_X
static gss_OID_desc oid_GSS_C_NT_HOSTBASED_SERVICE_X = {
    /* The implementation must reserve static storage for a
     * gss_OID_desc object containing the value */
    6, "\x2b\x06\x01\x05\x06\x02",
    /* corresponding to an object-identifier value of
     * {iso(1) org(3) dod(6) internet(1) security(5)
     * nametypes(6) gss-host-based-services(2))}.  The constant
     * GSS_C_NT_HOSTBASED_SERVICE_X should be initialized to point
     * to that gss_OID_desc.  This is a deprecated OID value, and
     * implementations wishing to support hostbased-service names
     * should instead use the GSS_C_NT_HOSTBASED_SERVICE OID,
     * defined below, to identify such names;
     * GSS_C_NT_HOSTBASED_SERVICE_X should be accepted a synonym
     * for GSS_C_NT_HOSTBASED_SERVICE when presented as an input
     * parameter, but should not be emitted by GSS-API
     * implementations */
};
const_gss_OID GSS_C_NT_HOSTBASED_SERVICE_X = &oid_GSS_C_NT_HOSTBASED_SERVICE_X;
#endif

#ifndef GSS_C_NT_HOSTBASED_SERVICE
static gss_OID_desc oid_GSS_C_NT_HOSTBASED_SERVICE = {
    /* The implementation must reserve static storage for a
     * gss_OID_desc object containing the value */
    10, "\x2a\x86\x48\x86\xf7\x12\x01\x02\x01\x04",
    /* corresponding to an object-identifier value of {iso(1)
     * member-body(2) Unites States(840) mit(113554) infosys(1)
     * gssapi(2) generic(1) service_name(4)}.  The constant
     * GSS_C_NT_HOSTBASED_SERVICE should be initialized
     * to point to that gss_OID_desc. */
};
const_gss_OID GSS_C_NT_HOSTBASED_SERVICE = &oid_GSS_C_NT_HOSTBASED_SERVICE;
#endif

#ifndef GSS_C_NT_ANONYMOUS
static gss_OID_desc oid_GSS_C_NT_ANONYMOUS = {
    /* The implementation must reserve static storage for a
     * gss_OID_desc object containing the value */
    6, "\x2b\x06\01\x05\x06\x03",
    /* corresponding to an object identifier value of
     * {1(iso), 3(org), 6(dod), 1(internet), 5(security),
     * 6(nametypes), 3(gss-anonymous-name)}.  The constant
     * and GSS_C_NT_ANONYMOUS should be initialized to point
     * to that gss_OID_desc. */
};
const_gss_OID GSS_C_NT_ANONYMOUS = &oid_GSS_C_NT_ANONYMOUS;
#endif

#ifndef GSS_C_NT_EXPORT_NAME
static gss_OID_desc oid_GSS_C_NT_EXPORT_NAME = {
    /* The implementation must reserve static storage for a
     * gss_OID_desc object containing the value */
    6, "\x2b\x06\x01\x05\x06\x04",
    /* corresponding to an object-identifier value of
     * {1(iso), 3(org), 6(dod), 1(internet), 5(security),
     * 6(nametypes), 4(gss-api-exported-name)}.  The constant
     * GSS_C_NT_EXPORT_NAME should be initialized to point
     * to that gss_OID_desc.
     */
};
const_gss_OID GSS_C_NT_EXPORT_NAME = &oid_GSS_C_NT_EXPORT_NAME;
#endif

#endif /* NO_LIBDL */

static gss_OID_desc gss_mech_krb5_desc =
{ 9, "\x2a\x86\x48\x86\xf7\x12\x01\x02\x02" };
/* iso(1) member-body(2) United States(840) mit(113554) infosys(1) gssapi(2) krb5(2)*/
const gss_OID GSS_MECH_KRB5 = &gss_mech_krb5_desc;

#endif /* NO_GSSAPI */
