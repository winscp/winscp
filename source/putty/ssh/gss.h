#ifndef PUTTY_SSHGSS_H
#define PUTTY_SSHGSS_H
#include "putty.h"
#include "pgssapi.h"

/* This struct is defined even in NO_GSSAPI mode, so that stubs/no-gss.c can
 * return an instance of it containing no libraries */
struct ssh_gss_liblist {
    struct ssh_gss_library *libraries;
    int nlibraries;
};

#ifndef NO_GSSAPI

#define SSH2_GSS_OIDTYPE 0x06
typedef void *Ssh_gss_ctx;

typedef enum Ssh_gss_stat {
    SSH_GSS_OK = 0,
    SSH_GSS_S_CONTINUE_NEEDED,
    SSH_GSS_NO_MEM,
    SSH_GSS_BAD_HOST_NAME,
    SSH_GSS_BAD_MIC,
    SSH_GSS_NO_CREDS,
    SSH_GSS_FAILURE
} Ssh_gss_stat;

#define SSH_GSS_S_COMPLETE SSH_GSS_OK

#define SSH_GSS_CLEAR_BUF(buf) do {             \
    (*buf).length = 0;                          \
    (*buf).value = NULL;                                \
} while (0)

typedef gss_buffer_desc Ssh_gss_buf;
typedef gss_name_t Ssh_gss_name;

#define GSS_NO_EXPIRATION ((time_t)-1)

#define GSS_DEF_REKEY_MINS 2    /* Default minutes between GSS cache checks */

/* Functions, provided by either {windows,unix}/gss.c or gssc.c */

struct ssh_gss_library;

/*
 * Prepare a collection of GSSAPI libraries for use in a single SSH
 * connection. Returns a structure containing a list of libraries,
 * with their ids (see struct ssh_gss_library below) filled in so
 * that the client can go through them in the SSH user's preferred
 * order.
 *
 * Must always return non-NULL. (Even if no libraries are available,
 * it must return an empty structure.)
 *
 * The free function cleans up the structure, and its associated
 * libraries (if any).
 */
struct ssh_gss_liblist *ssh_gss_setup(Conf *conf, LogContext *logctx);
void ssh_gss_cleanup(struct ssh_gss_liblist *list);

/*
 * Fills in buf with a string describing the GSSAPI mechanism in
 * use. buf->data is not dynamically allocated.
 */
typedef Ssh_gss_stat (*t_ssh_gss_indicate_mech)(struct ssh_gss_library *lib,
                                                Ssh_gss_buf *buf);

/*
 * Converts a name such as a hostname into a GSSAPI internal form,
 * which is placed in "out". The result should be freed by
 * ssh_gss_release_name().
 */
typedef Ssh_gss_stat (*t_ssh_gss_import_name)(struct ssh_gss_library *lib,
                                              char *in, Ssh_gss_name *out);

/*
 * Frees the contents of an Ssh_gss_name structure filled in by
 * ssh_gss_import_name().
 */
typedef Ssh_gss_stat (*t_ssh_gss_release_name)(struct ssh_gss_library *lib,
                                               Ssh_gss_name *name);

/*
 * The main GSSAPI security context setup function. The "out"
 * parameter will need to be freed by ssh_gss_free_tok.
 */
typedef Ssh_gss_stat (*t_ssh_gss_init_sec_context)
    (struct ssh_gss_library *lib,
     Ssh_gss_ctx *ctx, Ssh_gss_name name, int delegate,
     Ssh_gss_buf *in, Ssh_gss_buf *out, time_t *expiry,
     unsigned long *lifetime);

/*
 * Frees the contents of an Ssh_gss_buf filled in by
 * ssh_gss_init_sec_context(). Do not accidentally call this on
 * something filled in by ssh_gss_get_mic() (which requires a
 * different free function) or something filled in by any other
 * way.
 */
typedef Ssh_gss_stat (*t_ssh_gss_free_tok)(struct ssh_gss_library *lib,
                                           Ssh_gss_buf *);

/*
 * Acquires the credentials to perform authentication in the first
 * place. Needs to be freed by ssh_gss_release_cred().
 */
typedef Ssh_gss_stat (*t_ssh_gss_acquire_cred)(struct ssh_gss_library *lib,
                                               Ssh_gss_ctx *,
                                               time_t *expiry);

/*
 * Frees the contents of an Ssh_gss_ctx filled in by
 * ssh_gss_acquire_cred().
 */
typedef Ssh_gss_stat (*t_ssh_gss_release_cred)(struct ssh_gss_library *lib,
                                               Ssh_gss_ctx *);

/*
 * Gets a MIC for some input data. "out" needs to be freed by
 * ssh_gss_free_mic().
 */
typedef Ssh_gss_stat (*t_ssh_gss_get_mic)(struct ssh_gss_library *lib,
                                          Ssh_gss_ctx ctx, Ssh_gss_buf *in,
                                          Ssh_gss_buf *out);

/*
 * Validates an input MIC for some input data.
 */
typedef Ssh_gss_stat (*t_ssh_gss_verify_mic)(struct ssh_gss_library *lib,
                                             Ssh_gss_ctx ctx,
                                             Ssh_gss_buf *in_data,
                                             Ssh_gss_buf *in_mic);

/*
 * Frees the contents of an Ssh_gss_buf filled in by
 * ssh_gss_get_mic(). Do not accidentally call this on something
 * filled in by ssh_gss_init_sec_context() (which requires a
 * different free function) or something filled in by any other
 * way.
 */
typedef Ssh_gss_stat (*t_ssh_gss_free_mic)(struct ssh_gss_library *lib,
                                           Ssh_gss_buf *);

/*
 * Return an error message after authentication failed. The
 * message string is returned in "buf", with buf->len giving the
 * number of characters of printable message text and buf->data
 * containing one more character which is a trailing NUL.
 * buf->data should be manually freed by the caller.
 */
typedef Ssh_gss_stat (*t_ssh_gss_display_status)(struct ssh_gss_library *lib,
                                                 Ssh_gss_ctx, Ssh_gss_buf *buf);

struct ssh_gss_library {
    /*
     * Identifying number in the enumeration used by the
     * configuration code to specify a preference order.
     */
    int id;

    /*
     * Filled in at initialisation time, if there's anything
     * interesting to say about how GSSAPI was initialised (e.g.
     * which of a number of alternative libraries was used).
     */
    const char *gsslogmsg;

    /*
     * Function pointers implementing the SSH wrapper layer on top
     * of GSSAPI. (Defined in sshgssc, typically, though Windows
     * provides an alternative layer to sit on top of the annoyingly
     * different SSPI.)
     */
    t_ssh_gss_indicate_mech indicate_mech;
    t_ssh_gss_import_name import_name;
    t_ssh_gss_release_name release_name;
    t_ssh_gss_init_sec_context init_sec_context;
    t_ssh_gss_free_tok free_tok;
    t_ssh_gss_acquire_cred acquire_cred;
    t_ssh_gss_release_cred release_cred;
    t_ssh_gss_get_mic get_mic;
    t_ssh_gss_verify_mic verify_mic;
    t_ssh_gss_free_mic free_mic;
    t_ssh_gss_display_status display_status;

    /*
     * Additional data for the wrapper layers.
     */
    union {
        struct gssapi_functions gssapi;
        /*
         * The SSPI wrappers don't need to store their Windows API
         * function pointers in this structure, because there can't
         * be more than one set of them available.
         */
    } u;

    /*
     * Wrapper layers will often also need to store a library handle
     * of some sort for cleanup time.
     */
    void *handle;
};

/*
 * State that has to be shared between all GSSAPI-using parts of the
 * same SSH connection, in particular between GSS key exchange and the
 * subsequent trivial userauth method that reuses its output.
 */
struct ssh_connection_shared_gss_state {
    struct ssh_gss_liblist *libs;
    struct ssh_gss_library *lib;
    Ssh_gss_name srv_name;
    Ssh_gss_ctx ctx;
};

#endif /* NO_GSSAPI */

#endif /*PUTTY_SSHGSS_H*/
