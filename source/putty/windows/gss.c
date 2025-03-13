#ifndef NO_GSSAPI

#include <limits.h>
#include "putty.h"

#ifndef WINSCP // already defined globally
#define SECURITY_WIN32
#endif
#include <security.h>

#include "ssh/pgssapi.h"
#include "ssh/gss.h"
#include "ssh/gssc.h"

#include "misc.h"

#define UNIX_EPOCH      11644473600ULL  /* Seconds from Windows epoch */
#define CNS_PERSEC      10000000ULL     /* # 100ns per second */

/*
 * Note, as a special case, 0 relative to the Windows epoch (unspecified) maps
 * to 0 relative to the POSIX epoch (unspecified)!
 */
#define TIME_WIN_TO_POSIX(ft, t) do { \
    ULARGE_INTEGER uli; \
    uli.LowPart  = (ft).dwLowDateTime; \
    uli.HighPart = (ft).dwHighDateTime; \
    if (uli.QuadPart != 0) \
        uli.QuadPart = uli.QuadPart / CNS_PERSEC - UNIX_EPOCH; \
    (t) = (time_t) uli.QuadPart; \
} while (0)

/* Windows code to set up the GSSAPI library list. */

#ifdef _WIN64
#define MIT_KERB_SUFFIX "64"
#else
#define MIT_KERB_SUFFIX "32"
#endif

const int ngsslibs = 3;
const char *const gsslibnames[3] = {
    "MIT Kerberos GSSAPI"MIT_KERB_SUFFIX".DLL",
    "Microsoft SSPI SECUR32.DLL",
    "User-specified GSSAPI DLL",
};
const struct keyvalwhere gsslibkeywords[] = {
    { "gssapi32", 0, -1, -1 },
    { "sspi", 1, -1, -1 },
    { "custom", 2, -1, -1 },
};

DECL_WINDOWS_FUNCTION(static, SECURITY_STATUS,
                      AcquireCredentialsHandleA,
                      (SEC_CHAR *, SEC_CHAR *, ULONG, PVOID,
                       PVOID, SEC_GET_KEY_FN, PVOID, PCredHandle, PTimeStamp));
DECL_WINDOWS_FUNCTION(static, SECURITY_STATUS,
                      InitializeSecurityContextA,
                      (PCredHandle, PCtxtHandle, SEC_CHAR *, ULONG, ULONG,
                       ULONG, PSecBufferDesc, ULONG, PCtxtHandle,
                       PSecBufferDesc, PULONG, PTimeStamp));
DECL_WINDOWS_FUNCTION(static, SECURITY_STATUS,
                      FreeContextBuffer,
                      (PVOID));
DECL_WINDOWS_FUNCTION(static, SECURITY_STATUS,
                      FreeCredentialsHandle,
                      (PCredHandle));
DECL_WINDOWS_FUNCTION(static, SECURITY_STATUS,
                      DeleteSecurityContext,
                      (PCtxtHandle));
DECL_WINDOWS_FUNCTION(static, SECURITY_STATUS,
                      QueryContextAttributesA,
                      (PCtxtHandle, ULONG, PVOID));
DECL_WINDOWS_FUNCTION(static, SECURITY_STATUS,
                      MakeSignature,
                      (PCtxtHandle, ULONG, PSecBufferDesc, ULONG));
DECL_WINDOWS_FUNCTION(static, SECURITY_STATUS,
                      VerifySignature,
                      (PCtxtHandle, PSecBufferDesc, ULONG, PULONG));
DECL_WINDOWS_FUNCTION(static, DLL_DIRECTORY_COOKIE,
                      AddDllDirectory,
                      (PCWSTR));

typedef struct winSsh_gss_ctx {
    unsigned long maj_stat;
    unsigned long min_stat;
    CredHandle cred_handle;
    CtxtHandle context;
    PCtxtHandle context_handle;
    TimeStamp expiry;
} winSsh_gss_ctx;


#ifdef MPEXT
static
#endif
const Ssh_gss_buf gss_mech_krb5={9,"\x2A\x86\x48\x86\xF7\x12\x01\x02\x02"};

static void ssh_sspi_bind_fns(struct ssh_gss_library *lib);

static tree234 *libraries_to_never_unload;
static int library_to_never_unload_cmp(void *av, void *bv)
{
    uintptr_t a = (uintptr_t)av, b = (uintptr_t)bv;
    return a < b ? -1 : a > b ? +1 : 0;
}
static void ensure_library_tree_exists(void)
{
    if (!libraries_to_never_unload)
        libraries_to_never_unload = newtree234(library_to_never_unload_cmp);
}
static bool library_is_in_never_unload_tree(HMODULE module)
{
    ensure_library_tree_exists();
    return find234(libraries_to_never_unload, module, NULL);
}
static void add_library_to_never_unload_tree(HMODULE module)
{
    ensure_library_tree_exists();
    add234(libraries_to_never_unload, module);
}

struct ssh_gss_liblist *ssh_gss_setup(Conf *conf, LogContext *logctx) // MPEXT
{
    HMODULE module;
    struct ssh_gss_liblist *list = snew(struct ssh_gss_liblist);
    static HMODULE kernel32_module;
    if (!kernel32_module) {
        kernel32_module = load_system32_dll("kernel32.dll");
    }
#if !HAVE_ADDDLLDIRECTORY
    /* Omit the type-check because older MSVCs don't have this function */
    GET_WINDOWS_FUNCTION_NO_TYPECHECK(kernel32_module, AddDllDirectory);
#else
    GET_WINDOWS_FUNCTION(kernel32_module, AddDllDirectory);
#endif

    list->libraries = snewn(3, struct ssh_gss_library);
    list->nlibraries = 0;

    /* MIT Kerberos GSSAPI implementation */
    module = NULL;
    putty_registry_pass(true);
    { // WINSCP
    HKEY regkey = open_regkey_ro(HKEY_LOCAL_MACHINE,
                                 "SOFTWARE\\MIT\\Kerberos");
    if (regkey) {
        char *installdir = get_reg_sz(regkey, "InstallDir");
        if (installdir) {
            char *bindir = dupcat(installdir, "\\bin");
            if (p_AddDllDirectory) {
                /* Add MIT Kerberos' path to the DLL search path,
                 * it loads its own DLLs further down the road */
                wchar_t *dllPath = dup_mb_to_wc(DEFAULT_CODEPAGE, bindir);
                p_AddDllDirectory(dllPath);
                sfree(dllPath);
            }

            { // WINSCP
            char *dllfile = dupcat(bindir, "\\gssapi"MIT_KERB_SUFFIX".dll");
            module = LoadLibraryEx(dllfile, NULL,
                                   LOAD_LIBRARY_SEARCH_SYSTEM32 |
                                   LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR |
                                   LOAD_LIBRARY_SEARCH_USER_DIRS);

            /*
             * The MIT Kerberos DLL suffers an internal segfault for
             * some reason if you unload and reload one within the
             * same process. So, make sure that after we load this
             * library, we never free it.
             *
             * Or rather: after we've loaded it once, if any _further_
             * load returns the same module handle, we immediately
             * free it again (to prevent the Windows API's internal
             * reference count growing without bound). But on the
             * other hand we never free it in ssh_gss_cleanup.
             */
            if (library_is_in_never_unload_tree(module))
                FreeLibrary(module);
            add_library_to_never_unload_tree(module);

            sfree(dllfile);
            sfree(bindir);
            sfree(installdir);
            } // WINSCP
        }
        close_regkey(regkey);
    }
    putty_registry_pass(false);
    if (module) {
        struct ssh_gss_library *lib =
            &list->libraries[list->nlibraries++];

        lib->id = 0;
        lib->gsslogmsg = "Using GSSAPI from GSSAPI"MIT_KERB_SUFFIX".DLL";
        lib->handle = (void *)module;

#define BIND_GSS_FN(name) \
    lib->u.gssapi.name = (t_gss_##name) GetProcAddress(module, "gss_" #name)

        BIND_GSS_FN(delete_sec_context);
        BIND_GSS_FN(display_status);
        BIND_GSS_FN(get_mic);
        BIND_GSS_FN(verify_mic);
        BIND_GSS_FN(import_name);
        BIND_GSS_FN(init_sec_context);
        BIND_GSS_FN(release_buffer);
        BIND_GSS_FN(release_cred);
        BIND_GSS_FN(release_name);
        BIND_GSS_FN(acquire_cred);
        BIND_GSS_FN(inquire_cred_by_mech);

#undef BIND_GSS_FN

        ssh_gssapi_bind_fns(lib);
    }

    /* Microsoft SSPI Implementation */
    module = load_system32_dll("secur32.dll");
    if (module) {
        struct ssh_gss_library *lib =
            &list->libraries[list->nlibraries++];

        lib->id = 1;
        lib->gsslogmsg = "Using SSPI from SECUR32.DLL";
        lib->handle = (void *)module;

        /* No typecheck because Winelib thinks one PVOID is a PLUID */
        GET_WINDOWS_FUNCTION_NO_TYPECHECK(module, AcquireCredentialsHandleA);
        GET_WINDOWS_FUNCTION(module, InitializeSecurityContextA);
        GET_WINDOWS_FUNCTION(module, FreeContextBuffer);
        GET_WINDOWS_FUNCTION(module, FreeCredentialsHandle);
        GET_WINDOWS_FUNCTION(module, DeleteSecurityContext);
        GET_WINDOWS_FUNCTION(module, QueryContextAttributesA);
        GET_WINDOWS_FUNCTION(module, MakeSignature);
        GET_WINDOWS_FUNCTION(module, VerifySignature);

        ssh_sspi_bind_fns(lib);
    }

    /*
     * Custom GSSAPI DLL.
     */
    module = NULL;
    { // WINSCP
    Filename *customlib = conf_get_filename(conf, CONF_ssh_gss_custom);
    if (!filename_is_null(customlib)) {
        const wchar_t *path = customlib->wpath;
        if (p_AddDllDirectory) {

            /* Add the custom directory as well in case it chainloads
             * some other DLLs (e.g a non-installed MIT Kerberos
             * instance) */
            int pathlen = wcslen(path);

            while (pathlen > 0 && path[pathlen-1] != L':' &&
                   path[pathlen-1] != L'\\')
                pathlen--;

            if (pathlen > 0 && path[pathlen-1] != L'\\')
                pathlen--;

            if (pathlen > 0) {
                wchar_t *dirpath = snewn(pathlen + 1, wchar_t);
                memcpy(dirpath, path, pathlen * sizeof(wchar_t));
                dirpath[pathlen] = L'\0';
                p_AddDllDirectory(dirpath);
                sfree(dirpath);
            }
        }

        module = LoadLibraryExW(path, NULL,
                                LOAD_LIBRARY_SEARCH_SYSTEM32 |
                                LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR |
                                LOAD_LIBRARY_SEARCH_USER_DIRS);
        // MPEXT
        if (!module && logctx) {
            char *buf = dupprintf(WINSCP_BOM "Cannot load GSSAPI from user-specified library '%s': %s", filename_to_str(customlib), win_strerror(GetLastError()));
            logevent(logctx, buf);
            sfree(buf);
        }
    }
    if (module) {
        struct ssh_gss_library *lib =
            &list->libraries[list->nlibraries++];

        lib->id = 2;
        lib->gsslogmsg = dupprintf("Using GSSAPI from user-specified"
                                   " library '%s'", customlib->cpath);
        lib->handle = (void *)module;

#define BIND_GSS_FN(name) \
    lib->u.gssapi.name = (t_gss_##name) GetProcAddress(module, "gss_" #name)

        BIND_GSS_FN(delete_sec_context);
        BIND_GSS_FN(display_status);
        BIND_GSS_FN(get_mic);
        BIND_GSS_FN(verify_mic);
        BIND_GSS_FN(import_name);
        BIND_GSS_FN(init_sec_context);
        BIND_GSS_FN(release_buffer);
        BIND_GSS_FN(release_cred);
        BIND_GSS_FN(release_name);
        BIND_GSS_FN(acquire_cred);
        BIND_GSS_FN(inquire_cred_by_mech);

#undef BIND_GSS_FN

        ssh_gssapi_bind_fns(lib);
    }


    return list;
    } // WINSCP
    } // WINSCP
}

void ssh_gss_cleanup(struct ssh_gss_liblist *list)
{
    int i;

    /*
     * LoadLibrary and FreeLibrary are defined to employ reference
     * counting in the case where the same library is repeatedly
     * loaded, so even in a multiple-sessions-per-process context
     * (not that we currently expect ever to have such a thing on
     * Windows) it's safe to naively FreeLibrary everything here
     * without worrying about destroying it under the feet of
     * another SSH instance still using it.
     */
    for (i = 0; i < list->nlibraries; i++) {
        if (list->libraries[i].id != 0) {
            HMODULE module = (HMODULE)list->libraries[i].handle;
            if (!library_is_in_never_unload_tree(module))
                FreeLibrary(module);
        }
        if (list->libraries[i].id == 2) {
            /* The 'custom' id involves a dynamically allocated message.
             * Note that we must cast away the 'const' to free it. */
            sfree((char *)list->libraries[i].gsslogmsg);
        }
    }
    sfree(list->libraries);
    sfree(list);
}

static Ssh_gss_stat ssh_sspi_indicate_mech(struct ssh_gss_library *lib,
                                           Ssh_gss_buf *mech)
{
    *mech = gss_mech_krb5;
    return SSH_GSS_OK;
}


static Ssh_gss_stat ssh_sspi_import_name(struct ssh_gss_library *lib,
                                         char *host, Ssh_gss_name *srv_name)
{
    char *pStr;

    /* Check hostname */
    if (host == NULL) return SSH_GSS_FAILURE;

    /* copy it into form host/FQDN */
    pStr = dupcat("host/", host);

    *srv_name = (Ssh_gss_name) pStr;

    return SSH_GSS_OK;
}

static Ssh_gss_stat ssh_sspi_acquire_cred(struct ssh_gss_library *lib,
                                          Ssh_gss_ctx *ctx,
                                          time_t *expiry)
{
    winSsh_gss_ctx *winctx = snew(winSsh_gss_ctx);
    memset(winctx, 0, sizeof(winSsh_gss_ctx));

    /* prepare our "wrapper" structure */
    winctx->maj_stat =  winctx->min_stat = SEC_E_OK;
    winctx->context_handle = NULL;

    /* Specifying no principal name here means use the credentials of
       the current logged-in user */

    winctx->maj_stat = p_AcquireCredentialsHandleA(NULL,
                                                   "Kerberos",
                                                   SECPKG_CRED_OUTBOUND,
                                                   NULL,
                                                   NULL,
                                                   NULL,
                                                   NULL,
                                                   &winctx->cred_handle,
                                                   NULL);

    if (winctx->maj_stat != SEC_E_OK) {
        p_FreeCredentialsHandle(&winctx->cred_handle);
        sfree(winctx);
        return SSH_GSS_FAILURE;
    }

    /* Windows does not return a valid expiration from AcquireCredentials */
    if (expiry)
        *expiry = GSS_NO_EXPIRATION;

    *ctx = (Ssh_gss_ctx) winctx;
    return SSH_GSS_OK;
}

#ifdef MPEXT
static SecBuffer ssh_gss_init_sec_buffer(unsigned long buffersize,
  unsigned long buffertype, void * buffer)
{
  SecBuffer result;
  result.cbBuffer = buffersize;
  result.BufferType = buffertype;
  result.pvBuffer = buffer;
  return result;
}

static SecBufferDesc ssh_gss_init_sec_buffer_desc(unsigned long version,
  unsigned long bufferscount, PSecBuffer buffers)
{
  SecBufferDesc result;
  result.ulVersion = version;
  result.cBuffers = bufferscount;
  result.pBuffers = buffers;
  return result;
}

#define MPEXT_INIT_SEC_BUFFER(BUFFERSIZE, BUFFERTYPE, BUFFER) \
  ssh_gss_init_sec_buffer(BUFFERSIZE, BUFFERTYPE, BUFFER)
#define MPEXT_INIT_SEC_BUFFERDESC(VERSION, BUFFERSCOUNT, BUFFERS) \
  ssh_gss_init_sec_buffer_desc(VERSION, BUFFERSCOUNT, BUFFERS)

#else
#define MPEXT_INIT_SEC_BUFFER(BUFFERSIZE, BUFFERTYPE, BUFFER) \
  {BUFFERSIZE, BUFFERTYPE, BUFFER}
#define MPEXT_INIT_SEC_BUFFERDESC(VERSION, BUFFERSCOUNT, BUFFERS) \
  {VERSION, BUFFERSCOUNT, BUFFERS}
#endif

static void localexp_to_exp_lifetime(TimeStamp *localexp,
                                     time_t *expiry, unsigned long *lifetime)
{
    FILETIME nowUTC;
    FILETIME expUTC;
    time_t now;
    time_t exp;
    time_t delta;

    if (!lifetime && !expiry)
        return;

    GetSystemTimeAsFileTime(&nowUTC);
    TIME_WIN_TO_POSIX(nowUTC, now);

    if (lifetime)
        *lifetime = 0;
    if (expiry)
        *expiry = GSS_NO_EXPIRATION;

    /*
     * Type oddity: localexp is a pointer to 'TimeStamp', whereas
     * LocalFileTimeToFileTime expects a pointer to FILETIME. However,
     * despite having different formal type names from the compiler's
     * point of view, these two structures are specified to be
     * isomorphic in the MS documentation, so it's legitimate to copy
     * between them:
     *
     * https://msdn.microsoft.com/en-us/library/windows/desktop/aa380511(v=vs.85).aspx
     */
    {
        FILETIME localexp_ft;
        enum { vorpal_sword = 1 / (sizeof(*localexp) == sizeof(localexp_ft)) };
        memcpy(&localexp_ft, localexp, sizeof(localexp_ft));
        if (!LocalFileTimeToFileTime(&localexp_ft, &expUTC))
            return;
    }

    TIME_WIN_TO_POSIX(expUTC, exp);
    delta = exp - now;
    if (exp == 0 || delta <= 0)
        return;

    if (expiry)
        *expiry = exp;
    if (lifetime) {
        if (delta <= ULONG_MAX)
            *lifetime = (unsigned long)delta;
        else
            *lifetime = ULONG_MAX;
    }
}

static Ssh_gss_stat ssh_sspi_init_sec_context(struct ssh_gss_library *lib,
                                              Ssh_gss_ctx *ctx,
                                              Ssh_gss_name srv_name,
                                              int to_deleg,
                                              Ssh_gss_buf *recv_tok,
                                              Ssh_gss_buf *send_tok,
                                              time_t *expiry,
                                              unsigned long *lifetime)
{
    winSsh_gss_ctx *winctx = (winSsh_gss_ctx *) *ctx;
    SecBuffer wsend_tok = MPEXT_INIT_SEC_BUFFER(send_tok->length,SECBUFFER_TOKEN,send_tok->value);
    SecBuffer wrecv_tok = MPEXT_INIT_SEC_BUFFER(recv_tok->length,SECBUFFER_TOKEN,recv_tok->value);
    SecBufferDesc output_desc = MPEXT_INIT_SEC_BUFFERDESC(SECBUFFER_VERSION,1,&wsend_tok);
    SecBufferDesc input_desc  = MPEXT_INIT_SEC_BUFFERDESC(SECBUFFER_VERSION,1,&wrecv_tok);
    unsigned long flags=ISC_REQ_MUTUAL_AUTH|ISC_REQ_REPLAY_DETECT|
        ISC_REQ_CONFIDENTIALITY|ISC_REQ_ALLOCATE_MEMORY;
    ULONG ret_flags=0;
    TimeStamp localexp;

    /* check if we have to delegate ... */
    if (to_deleg) flags |= ISC_REQ_DELEGATE;
    winctx->maj_stat = p_InitializeSecurityContextA(&winctx->cred_handle,
                                                    winctx->context_handle,
                                                    (char*) srv_name,
                                                    flags,
                                                    0,          /* reserved */
                                                    SECURITY_NATIVE_DREP,
                                                    &input_desc,
                                                    0,          /* reserved */
                                                    &winctx->context,
                                                    &output_desc,
                                                    &ret_flags,
                                                    &localexp);

    localexp_to_exp_lifetime(&localexp, expiry, lifetime);

    /* prepare for the next round */
    winctx->context_handle = &winctx->context;
    send_tok->value = wsend_tok.pvBuffer;
    send_tok->length = wsend_tok.cbBuffer;

    /* check & return our status */
    if (winctx->maj_stat==SEC_E_OK) return SSH_GSS_S_COMPLETE;
    if (winctx->maj_stat==SEC_I_CONTINUE_NEEDED) return SSH_GSS_S_CONTINUE_NEEDED;

    return SSH_GSS_FAILURE;
}

static Ssh_gss_stat ssh_sspi_free_tok(struct ssh_gss_library *lib,
                                      Ssh_gss_buf *send_tok)
{
    /* check input */
    if (send_tok == NULL) return SSH_GSS_FAILURE;

    /* free Windows buffer */
    p_FreeContextBuffer(send_tok->value);
    SSH_GSS_CLEAR_BUF(send_tok);

    return SSH_GSS_OK;
}

static Ssh_gss_stat ssh_sspi_release_cred(struct ssh_gss_library *lib,
                                          Ssh_gss_ctx *ctx)
{
    winSsh_gss_ctx *winctx = (winSsh_gss_ctx *) *ctx;

    /* check input */
    if (winctx == NULL) return SSH_GSS_FAILURE;

    /* free Windows data */
    p_FreeCredentialsHandle(&winctx->cred_handle);
    #ifdef MPEXT
    if (winctx->context_handle != NULL)
    #endif
    p_DeleteSecurityContext(&winctx->context);

    /* delete our "wrapper" structure */
    sfree(winctx);
    *ctx = (Ssh_gss_ctx) NULL;

    return SSH_GSS_OK;
}


static Ssh_gss_stat ssh_sspi_release_name(struct ssh_gss_library *lib,
                                          Ssh_gss_name *srv_name)
{
    char *pStr = (char *) *srv_name;

    if (pStr == NULL) return SSH_GSS_FAILURE;
    sfree(pStr);
    *srv_name = (Ssh_gss_name) NULL;

    return SSH_GSS_OK;
}

static Ssh_gss_stat ssh_sspi_display_status(struct ssh_gss_library *lib,
                                            Ssh_gss_ctx ctx, Ssh_gss_buf *buf)
{
    winSsh_gss_ctx *winctx = (winSsh_gss_ctx *) ctx;
    const char *msg;

    if (winctx == NULL) return SSH_GSS_FAILURE;

    /* decode the error code */
    switch (winctx->maj_stat) {
      case SEC_E_OK: msg="SSPI status OK"; break;
      case SEC_E_INVALID_HANDLE: msg="The handle passed to the function"
            " is invalid.";
        break;
      case SEC_E_TARGET_UNKNOWN: msg="The target was not recognized."; break;
      case SEC_E_LOGON_DENIED: msg="The logon failed."; break;
      case SEC_E_INTERNAL_ERROR: msg="The Local Security Authority cannot"
            " be contacted.";
        break;
      case SEC_E_NO_CREDENTIALS: msg="No credentials are available in the"
            " security package.";
        break;
      case SEC_E_NO_AUTHENTICATING_AUTHORITY:
        msg="No authority could be contacted for authentication."
            "The domain name of the authenticating party could be wrong,"
            " the domain could be unreachable, or there might have been"
            " a trust relationship failure.";
        break;
      case SEC_E_INSUFFICIENT_MEMORY:
        msg="One or more of the SecBufferDesc structures passed as"
            " an OUT parameter has a buffer that is too small.";
        break;
      case SEC_E_INVALID_TOKEN:
        msg="The error is due to a malformed input token, such as a"
            " token corrupted in transit, a token"
            " of incorrect size, or a token passed into the wrong"
            " security package. Passing a token to"
            " the wrong package can happen if client and server did not"
            " negotiate the proper security package.";
        break;
      default:
        msg = "Internal SSPI error";
        break;
    }

    buf->value = dupstr(msg);
    buf->length = strlen(buf->value);

    return SSH_GSS_OK;
}

static Ssh_gss_stat ssh_sspi_get_mic(struct ssh_gss_library *lib,
                                     Ssh_gss_ctx ctx, Ssh_gss_buf *buf,
                                     Ssh_gss_buf *hash)
{
    winSsh_gss_ctx *winctx = (winSsh_gss_ctx *) ctx;
    SecPkgContext_Sizes ContextSizes;
    SecBufferDesc InputBufferDescriptor;
    SecBuffer InputSecurityToken[2];

    if (winctx == NULL) return SSH_GSS_FAILURE;

    winctx->maj_stat = 0;

    memset(&ContextSizes, 0, sizeof(ContextSizes));

    winctx->maj_stat = p_QueryContextAttributesA(&winctx->context,
                                                 SECPKG_ATTR_SIZES,
                                                 &ContextSizes);

    if (winctx->maj_stat != SEC_E_OK ||
        ContextSizes.cbMaxSignature == 0)
        return winctx->maj_stat;

    InputBufferDescriptor.cBuffers = 2;
    InputBufferDescriptor.pBuffers = InputSecurityToken;
    InputBufferDescriptor.ulVersion = SECBUFFER_VERSION;
    InputSecurityToken[0].BufferType = SECBUFFER_DATA;
    InputSecurityToken[0].cbBuffer = buf->length;
    InputSecurityToken[0].pvBuffer = buf->value;
    InputSecurityToken[1].BufferType = SECBUFFER_TOKEN;
    InputSecurityToken[1].cbBuffer = ContextSizes.cbMaxSignature;
    InputSecurityToken[1].pvBuffer = snewn(ContextSizes.cbMaxSignature, char);

    winctx->maj_stat = p_MakeSignature(&winctx->context,
                                       0,
                                       &InputBufferDescriptor,
                                       0);

    if (winctx->maj_stat == SEC_E_OK) {
        hash->length = InputSecurityToken[1].cbBuffer;
        hash->value = InputSecurityToken[1].pvBuffer;
    }

    return winctx->maj_stat;
}

static Ssh_gss_stat ssh_sspi_verify_mic(struct ssh_gss_library *lib,
                                        Ssh_gss_ctx ctx,
                                        Ssh_gss_buf *buf,
                                        Ssh_gss_buf *mic)
{
    winSsh_gss_ctx *winctx = (winSsh_gss_ctx *) ctx;
    SecBufferDesc InputBufferDescriptor;
    SecBuffer InputSecurityToken[2];
    ULONG qop;

    if (winctx == NULL) return SSH_GSS_FAILURE;

    winctx->maj_stat = 0;

    InputBufferDescriptor.cBuffers = 2;
    InputBufferDescriptor.pBuffers = InputSecurityToken;
    InputBufferDescriptor.ulVersion = SECBUFFER_VERSION;
    InputSecurityToken[0].BufferType = SECBUFFER_DATA;
    InputSecurityToken[0].cbBuffer = buf->length;
    InputSecurityToken[0].pvBuffer = buf->value;
    InputSecurityToken[1].BufferType = SECBUFFER_TOKEN;
    InputSecurityToken[1].cbBuffer = mic->length;
    InputSecurityToken[1].pvBuffer = mic->value;

    winctx->maj_stat = p_VerifySignature(&winctx->context,
                                         &InputBufferDescriptor,
                                         0, &qop);
    return winctx->maj_stat;
}

static Ssh_gss_stat ssh_sspi_free_mic(struct ssh_gss_library *lib,
                                      Ssh_gss_buf *hash)
{
    sfree(hash->value);
    return SSH_GSS_OK;
}

static void ssh_sspi_bind_fns(struct ssh_gss_library *lib)
{
    lib->indicate_mech = ssh_sspi_indicate_mech;
    lib->import_name = ssh_sspi_import_name;
    lib->release_name = ssh_sspi_release_name;
    lib->init_sec_context = ssh_sspi_init_sec_context;
    lib->free_tok = ssh_sspi_free_tok;
    lib->acquire_cred = ssh_sspi_acquire_cred;
    lib->release_cred = ssh_sspi_release_cred;
    lib->get_mic = ssh_sspi_get_mic;
    lib->verify_mic = ssh_sspi_verify_mic;
    lib->free_mic = ssh_sspi_free_mic;
    lib->display_status = ssh_sspi_display_status;
}

#else

/* Dummy function so this source file defines something if NO_GSSAPI
   is defined. */

void ssh_gss_init(void)
{
}

#endif

#ifdef WINSCP

void wingss_cleanup(void)
{
    if (libraries_to_never_unload != NULL)
    {
        freetree234(libraries_to_never_unload);
        libraries_to_never_unload = NULL;
    }
}

#endif
