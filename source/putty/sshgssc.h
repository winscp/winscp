#ifndef PUTTY_SSHGSSC_H
#define PUTTY_SSHGSSC_H
#include "putty.h"
#ifndef NO_GSSAPI

#include "pgssapi.h"
#include "sshgss.h"

typedef struct gssapi_ssh_gss_ctx {
    OM_uint32 maj_stat;
    OM_uint32 min_stat;
    gss_ctx_id_t ctx;
    time_t expiry;
} gssapi_ssh_gss_ctx;

void ssh_gssapi_bind_fns(struct ssh_gss_library *lib);

#else

int ssh_gssapi_init(void);

#endif /*NO_GSSAPI*/

#endif /*PUTTY_SSHGSSC_H*/
