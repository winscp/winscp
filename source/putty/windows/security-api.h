/*
 * security-api.h: some miscellaneous security-related helper functions,
 * defined in utils/security.c, that use the advapi32 library. Also
 * centralises the machinery for dynamically loading that library.
 */

#include <aclapi.h>

/*
 * Functions loaded from advapi32.dll.
 */
DECL_WINDOWS_FUNCTION(extern, BOOL, OpenProcessToken,
                      (HANDLE, DWORD, PHANDLE));
DECL_WINDOWS_FUNCTION(extern, BOOL, GetTokenInformation,
                      (HANDLE, TOKEN_INFORMATION_CLASS,
                       LPVOID, DWORD, PDWORD));
DECL_WINDOWS_FUNCTION(extern, BOOL, InitializeSecurityDescriptor,
                      (PSECURITY_DESCRIPTOR, DWORD));
DECL_WINDOWS_FUNCTION(extern, BOOL, SetSecurityDescriptorOwner,
                      (PSECURITY_DESCRIPTOR, PSID, BOOL));
DECL_WINDOWS_FUNCTION(extern, DWORD, GetSecurityInfo,
                      (HANDLE, SE_OBJECT_TYPE, SECURITY_INFORMATION,
                       PSID *, PSID *, PACL *, PACL *,
                       PSECURITY_DESCRIPTOR *));
DECL_WINDOWS_FUNCTION(extern, DWORD, SetSecurityInfo,
                      (HANDLE, SE_OBJECT_TYPE, SECURITY_INFORMATION,
                       PSID, PSID, PACL, PACL));
DECL_WINDOWS_FUNCTION(extern, DWORD, SetEntriesInAclA,
                      (ULONG, PEXPLICIT_ACCESS, PACL, PACL *));
bool got_advapi(void);

/*
 * Find the SID describing the current user. The return value (if not
 * NULL for some error-related reason) is smalloced.
 */
PSID get_user_sid(void);

/*
 * Construct a PSECURITY_DESCRIPTOR of the type used for named pipe
 * servers, i.e. allowing access only to the current user id and also
 * only local (i.e. not over SMB) connections.
 *
 * If this function returns true, then 'psd' and 'acl' will have been
 * filled in with memory allocated using LocalAlloc (and hence must be
 * freed later using LocalFree). If it returns false, then instead
 * 'error' has been filled with a dynamically allocated error message.
 */
bool make_private_security_descriptor(
    DWORD permissions, PSECURITY_DESCRIPTOR *psd, PACL *acl, char **error);
