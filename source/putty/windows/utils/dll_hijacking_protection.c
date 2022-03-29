/*
 * If the OS provides it, call SetDefaultDllDirectories() to prevent
 * DLLs from being loaded from the directory containing our own
 * binary, and instead only load from system32.
 *
 * This is a protection against hijacking attacks, if someone runs
 * PuTTY directly from their web browser's download directory having
 * previously been enticed into clicking on an unwise link that
 * downloaded a malicious DLL to the same directory under one of
 * various magic names that seem to be things that standard Windows
 * DLLs delegate to.
 *
 * It shouldn't break deliberate loading of user-provided DLLs such as
 * GSSAPI providers, because those are specified by their full
 * pathname by the user-provided configuration.
 */

#include "putty.h"

void dll_hijacking_protection(void)
{
    static HMODULE kernel32_module;
    DECL_WINDOWS_FUNCTION(static, BOOL, SetDefaultDllDirectories, (DWORD));

    if (!kernel32_module) {
        kernel32_module = load_system32_dll("kernel32.dll");
#if !HAVE_SETDEFAULTDLLDIRECTORIES
        /* For older Visual Studio, this function isn't available in
         * the header files to type-check */
        GET_WINDOWS_FUNCTION_NO_TYPECHECK(
            kernel32_module, SetDefaultDllDirectories);
#else
        GET_WINDOWS_FUNCTION(kernel32_module, SetDefaultDllDirectories);
#endif
    }

    if (p_SetDefaultDllDirectories) {
        /* LOAD_LIBRARY_SEARCH_SYSTEM32 and explicitly specified
         * directories only */
        p_SetDefaultDllDirectories(LOAD_LIBRARY_SEARCH_SYSTEM32 |
                                   LOAD_LIBRARY_SEARCH_USER_DIRS);
    }
}
