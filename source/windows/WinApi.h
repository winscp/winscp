//---------------------------------------------------------------------------
#ifndef WinApiH
#define WinApiH
//---------------------------------------------------------------------------
#include <shlobj.h>
//---------------------------------------------------------------------------
typedef BOOL WINAPI (* ChangeWindowMessageFilterExProc)(
    HWND hwnd, UINT message, DWORD action, PCHANGEFILTERSTRUCT pChangeFilterStruct);
//---------------------------------------------------------------------------
#endif  // WinApiH
