/*
 * cryptoapi.h: Windows Crypto API functions defined in PuTTY that
 * use the crypt32 library. Also centralises the machinery for
 * dynamically loading that library, and our own functions using that
 * in turn.
 */

DECL_WINDOWS_FUNCTION(extern, BOOL, CryptProtectMemory, (LPVOID,DWORD,DWORD));

bool got_crypt(void);

/*
 * Function to obfuscate an input string into something usable as a
 * pathname for a Windows named pipe. Uses CryptProtectMemory to make
 * the obfuscation depend on a key Windows stores for the owning user,
 * and then hashes the string as well to make it have a manageable
 * length and be composed of filename-legal characters.
 *
 * Rationale: Windows's named pipes all live in the same namespace, so
 * one user can see what pipes another user has open. This is an
 * undesirable privacy leak: in particular, if we used unobfuscated
 * names for the connection-sharing pipe names, it would permit one
 * user to know what username@host another user is SSHing to.
 *
 * The returned string is dynamically allocated.
 */
char *capi_obfuscate_string(const char *realname);
