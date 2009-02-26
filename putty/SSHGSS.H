#include "puttyps.h"

#define SSH2_GSS_OIDTYPE 0x06
typedef void *Ssh_gss_ctx;

typedef enum Ssh_gss_stat {
    SSH_GSS_OK = 0,
    SSH_GSS_S_CONTINUE_NEEDED,
    SSH_GSS_NO_MEM,
    SSH_GSS_BAD_HOST_NAME,
    SSH_GSS_FAILURE
} Ssh_gss_stat;

#define SSH_GSS_S_COMPLETE SSH_GSS_OK

#define SSH_GSS_CLEAR_BUF(buf) do {		\
    (*buf).length = 0;				\
    (*buf).value = NULL;				\
} while (0)

/* Functions, provided by either wingss.c or uxgss.c */

/*
 * Do startup-time initialisation for using GSSAPI. (On Windows,
 * for instance, this dynamically loads the GSSAPI DLL and
 * retrieves some function pointers.)
 *
 * Return value is 1 on success, or 0 if initialisation failed.
 *
 * May be called multiple times (since the most convenient place
 * to call it _from_ is the ssh.c setup code), and will harmlessly
 * return success if already initialised.
 */
int ssh_gss_init(void);

/*
 * Fills in buf with a string describing the GSSAPI mechanism in
 * use. buf->data is not dynamically allocated.
 */
Ssh_gss_stat ssh_gss_indicate_mech(Ssh_gss_buf *buf);

/*
 * Converts a name such as a hostname into a GSSAPI internal form,
 * which is placed in "out". The result should be freed by
 * ssh_gss_release_name().
 */
Ssh_gss_stat ssh_gss_import_name(char *in, Ssh_gss_name *out);

/*
 * Frees the contents of an Ssh_gss_name structure filled in by
 * ssh_gss_import_name().
 */
Ssh_gss_stat ssh_gss_release_name(Ssh_gss_name *name);

/*
 * The main GSSAPI security context setup function. The "out"
 * parameter will need to be freed by ssh_gss_free_tok.
 */
Ssh_gss_stat ssh_gss_init_sec_context(Ssh_gss_ctx *ctx, Ssh_gss_name name, int delegate,
				      Ssh_gss_buf *in, Ssh_gss_buf *out);

/*
 * Frees the contents of an Ssh_gss_buf filled in by
 * ssh_gss_init_sec_context(). Do not accidentally call this on
 * something filled in by ssh_gss_get_mic() (which requires a
 * different free function) or something filled in by any other
 * way.
 */
Ssh_gss_stat ssh_gss_free_tok(Ssh_gss_buf *);

/*
 * Acquires the credentials to perform authentication in the first
 * place. Needs to be freed by ssh_gss_release_cred().
 */
Ssh_gss_stat ssh_gss_acquire_cred(Ssh_gss_ctx *);

/*
 * Frees the contents of an Ssh_gss_ctx filled in by
 * ssh_gss_acquire_cred().
 */
Ssh_gss_stat ssh_gss_release_cred(Ssh_gss_ctx *);

/*
 * Gets a MIC for some input data. "out" needs to be freed by
 * ssh_gss_free_mic().
 */
Ssh_gss_stat ssh_gss_get_mic(Ssh_gss_ctx ctx, Ssh_gss_buf *in,
			     Ssh_gss_buf *out);

/*
 * Frees the contents of an Ssh_gss_buf filled in by
 * ssh_gss_get_mic(). Do not accidentally call this on something
 * filled in by ssh_gss_init_sec_context() (which requires a
 * different free function) or something filled in by any other
 * way.
 */
Ssh_gss_stat ssh_gss_free_mic(Ssh_gss_buf *);

/*
 * Return an error message after authentication failed. The
 * message string is returned in "buf", with buf->len giving the
 * number of characters of printable message text and buf->data
 * containing one more character which is a trailing NUL.
 * buf->data should be manually freed by the caller. 
 */
Ssh_gss_stat ssh_gss_display_status(Ssh_gss_ctx, Ssh_gss_buf *buf);
