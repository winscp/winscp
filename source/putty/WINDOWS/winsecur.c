/*
 * winsecur.c: implementation of winsecur.h.
 */

#include <stdio.h>
#include <stdlib.h>

#include "putty.h"

#if !defined NO_SECURITY

#define WINSECUR_GLOBAL
#include "winsecur.h"

/* Initialised once, then kept around to reuse forever */
static PSID worldsid, networksid, usersid;


int got_advapi(void)
{
    static int attempted = FALSE;
    static int successful;
    static HMODULE advapi;

    if (!attempted) {
        attempted = TRUE;
        advapi = load_system32_dll("advapi32.dll");
        successful = advapi &&
            GET_WINDOWS_FUNCTION(advapi, GetSecurityInfo) &&
            GET_WINDOWS_FUNCTION(advapi, SetSecurityInfo) &&
            GET_WINDOWS_FUNCTION(advapi, OpenProcessToken) &&
            GET_WINDOWS_FUNCTION(advapi, GetTokenInformation) &&
            GET_WINDOWS_FUNCTION(advapi, InitializeSecurityDescriptor) &&
            GET_WINDOWS_FUNCTION(advapi, SetSecurityDescriptorOwner) &&
            GET_WINDOWS_FUNCTION(advapi, SetEntriesInAclA);
    }
    return successful;
}

PSID get_user_sid(void)
{
    HANDLE proc = NULL, tok = NULL;
    TOKEN_USER *user = NULL;
    DWORD toklen, sidlen;
    PSID sid = NULL, ret = NULL;

    if (usersid)
        return usersid;

    if (!got_advapi())
        goto cleanup;

    if ((proc = OpenProcess(MAXIMUM_ALLOWED, FALSE,
                            GetCurrentProcessId())) == NULL)
        goto cleanup;

    if (!p_OpenProcessToken(proc, TOKEN_QUERY, &tok))
        goto cleanup;

    if (!p_GetTokenInformation(tok, TokenUser, NULL, 0, &toklen) &&
        GetLastError() != ERROR_INSUFFICIENT_BUFFER)
        goto cleanup;

    if ((user = (TOKEN_USER *)LocalAlloc(LPTR, toklen)) == NULL)
        goto cleanup;

    if (!p_GetTokenInformation(tok, TokenUser, user, toklen, &toklen))
        goto cleanup;

    sidlen = GetLengthSid(user->User.Sid);

    sid = (PSID)smalloc(sidlen);

    if (!CopySid(sidlen, sid, user->User.Sid))
        goto cleanup;

    /* Success. Move sid into the return value slot, and null it out
     * to stop the cleanup code freeing it. */
    ret = usersid = sid;
    sid = NULL;

  cleanup:
    if (proc != NULL)
        CloseHandle(proc);
    if (tok != NULL)
        CloseHandle(tok);
    if (user != NULL)
        LocalFree(user);
    if (sid != NULL)
        sfree(sid);

    return ret;
}

int getsids(char *error)
{
    SID_IDENTIFIER_AUTHORITY world_auth = SECURITY_WORLD_SID_AUTHORITY;
    SID_IDENTIFIER_AUTHORITY nt_auth = SECURITY_NT_AUTHORITY;
    int ret;

    error=NULL;

    if (!usersid) {
        if ((usersid = get_user_sid()) == NULL) {
            error = dupprintf("unable to construct SID for current user: %s",
                               win_strerror(GetLastError()));
            goto cleanup;
        }
    }

    if (!worldsid) {
        if (!AllocateAndInitializeSid(&world_auth, 1, SECURITY_WORLD_RID,
                                      0, 0, 0, 0, 0, 0, 0, &worldsid)) {
            error = dupprintf("unable to construct SID for world: %s",
                               win_strerror(GetLastError()));
            goto cleanup;
        }
    }

    if (!networksid) {
        if (!AllocateAndInitializeSid(&nt_auth, 1, SECURITY_NETWORK_RID,
                                      0, 0, 0, 0, 0, 0, 0, &networksid)) {
            error = dupprintf("unable to construct SID for "
                               "local same-user access only: %s",
                               win_strerror(GetLastError()));
            goto cleanup;
        }
    }

    ret=TRUE;

 cleanup:
    if (ret) {
      sfree(error);
      error = NULL;
    }
    return ret;
}
  

int make_private_security_descriptor(DWORD permissions,
                                     PSECURITY_DESCRIPTOR *psd,
                                     PACL *acl,
                                     char **error)
{
    EXPLICIT_ACCESS ea[3];
    int acl_err;
    int ret = FALSE;


    *psd = NULL;
    *acl = NULL;
    *error = NULL;

    if (!getsids(*error))
      goto cleanup;

    memset(ea, 0, sizeof(ea));
    ea[0].grfAccessPermissions = permissions;
    ea[0].grfAccessMode = REVOKE_ACCESS;
    ea[0].grfInheritance = NO_INHERITANCE;
    ea[0].Trustee.TrusteeForm = TRUSTEE_IS_SID;
    ea[0].Trustee.ptstrName = (LPTSTR)worldsid;
    ea[1].grfAccessPermissions = permissions;
    ea[1].grfAccessMode = GRANT_ACCESS;
    ea[1].grfInheritance = NO_INHERITANCE;
    ea[1].Trustee.TrusteeForm = TRUSTEE_IS_SID;
    ea[1].Trustee.ptstrName = (LPTSTR)usersid;
    ea[2].grfAccessPermissions = permissions;
    ea[2].grfAccessMode = REVOKE_ACCESS;
    ea[2].grfInheritance = NO_INHERITANCE;
    ea[2].Trustee.TrusteeForm = TRUSTEE_IS_SID;
    ea[2].Trustee.ptstrName = (LPTSTR)networksid;

    acl_err = p_SetEntriesInAclA(3, ea, NULL, acl);
    if (acl_err != ERROR_SUCCESS || *acl == NULL) {
        *error = dupprintf("unable to construct ACL: %s",
                           win_strerror(acl_err));
        goto cleanup;
    }

    *psd = (PSECURITY_DESCRIPTOR)
        LocalAlloc(LPTR, SECURITY_DESCRIPTOR_MIN_LENGTH);
    if (!*psd) {
        *error = dupprintf("unable to allocate security descriptor: %s",
                           win_strerror(GetLastError()));
        goto cleanup;
    }

    if (!InitializeSecurityDescriptor(*psd, SECURITY_DESCRIPTOR_REVISION)) {
        *error = dupprintf("unable to initialise security descriptor: %s",
                           win_strerror(GetLastError()));
        goto cleanup;
    }

    if (!SetSecurityDescriptorOwner(*psd, usersid, FALSE)) {
        *error = dupprintf("unable to set owner in security descriptor: %s",
                           win_strerror(GetLastError()));
        goto cleanup;
    }

    if (!SetSecurityDescriptorDacl(*psd, TRUE, *acl, FALSE)) {
        *error = dupprintf("unable to set DACL in security descriptor: %s",
                           win_strerror(GetLastError()));
        goto cleanup;
    }

    ret = TRUE;

  cleanup:
    if (!ret) {
        if (*psd) {
            LocalFree(*psd);
            *psd = NULL;
        }
        if (*acl) {
            LocalFree(*acl);
            *acl = NULL;
        }
    } else {
        sfree(*error);
        *error = NULL;
    }
    return ret;
}

int setprocessacl(char *error)
{
    EXPLICIT_ACCESS ea[2];
    int acl_err;
    int ret=FALSE;
    PACL acl = NULL;

    static const nastyace=WRITE_DAC | WRITE_OWNER |
	PROCESS_CREATE_PROCESS | PROCESS_CREATE_THREAD |
	PROCESS_DUP_HANDLE | PROCESS_QUERY_INFORMATION |
	PROCESS_SET_QUOTA | PROCESS_SET_INFORMATION |
	PROCESS_VM_OPERATION | PROCESS_VM_READ | PROCESS_VM_WRITE |
	PROCESS_SUSPEND_RESUME;

    if (!getsids(error))
	goto cleanup;

    memset(ea, 0, sizeof(ea));

    /* Everyone: deny */
    ea[0].grfAccessPermissions = nastyace;
    ea[0].grfAccessMode = DENY_ACCESS;
    ea[0].grfInheritance = SUB_CONTAINERS_AND_OBJECTS_INHERIT;
    ea[0].Trustee.TrusteeForm = TRUSTEE_IS_SID;
    ea[0].Trustee.ptstrName = (LPTSTR)worldsid;

    /* User: user ace */
    ea[1].grfAccessPermissions = ~nastyace & 0x1fff;
    ea[1].grfAccessMode = GRANT_ACCESS;
    ea[1].grfInheritance = SUB_CONTAINERS_AND_OBJECTS_INHERIT;
    ea[1].Trustee.TrusteeForm = TRUSTEE_IS_SID;
    ea[1].Trustee.ptstrName = (LPTSTR)usersid;

    acl_err = p_SetEntriesInAclA(2, ea, NULL, &acl);

    if (acl_err != ERROR_SUCCESS || acl == NULL) {
	error = dupprintf("unable to construct ACL: %s",
			  win_strerror(acl_err));
        goto cleanup;
    }

    if (ERROR_SUCCESS != p_SetSecurityInfo
        (GetCurrentProcess(), SE_KERNEL_OBJECT,
         OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION,
         usersid, NULL, acl, NULL)) {
	error=dupprintf("Unable to set process ACL: %s",
			win_strerror(GetLastError()));
	goto cleanup;
    }
		      

    ret=TRUE;
    
  cleanup:
    if (!ret) {
        if (acl) {
            LocalFree(acl);
            acl = NULL;
        }
    }
    return ret;
}  
#endif /* !defined NO_SECURITY */
