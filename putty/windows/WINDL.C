/*
 * windl.c: Windows-specific functions for loading dynamic
 * libraries.
 */

#include "putty.h"

/*********************************************************************************/

Library *load_dyn_lib(Filename libname, char **error)
{
    HMODULE lib = NULL;

    lib = LoadLibrary(libname.path);

    if (lib == NULL && error) {
        LPVOID message;

        FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
              FORMAT_MESSAGE_FROM_SYSTEM |
              FORMAT_MESSAGE_IGNORE_INSERTS,
              NULL, GetLastError(),
              MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
              (LPTSTR)&message, 0, NULL);

		*error = dupstr(message);

        LocalFree(message);
    }

    return (Library *)lib;
}

/*********************************************************************************/

generic_fn_t dyn_lib_symbol(Library *lib, char *name)
{
    return (generic_fn_t)GetProcAddress((HMODULE)lib, name);
}

/*********************************************************************************/
