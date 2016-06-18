//---------------------------------------------------------------------------
#ifndef WinApiH
#define WinApiH
//---------------------------------------------------------------------------
#include <shlobj.h>
//---------------------------------------------------------------------------
typedef BOOL WINAPI (* ChangeWindowMessageFilterExProc)(
  HWND hwnd, UINT message, DWORD action, PCHANGEFILTERSTRUCT pChangeFilterStruct);
//---------------------------------------------------------------------------
typedef enum _Monitor_DPI_Type {
  MDT_Effective_DPI  = 0,
  MDT_Angular_DPI    = 1,
  MDT_Raw_DPI        = 2,
  MDT_Default        = MDT_Effective_DPI
} MONITOR_DPI_TYPE;
//---------------------------------------------------------------------------
typedef HRESULT WINAPI (* GetDpiForMonitorProc)(
  HMONITOR hmonitor, MONITOR_DPI_TYPE dpiType, UINT * dpiX, UINT * dpiY);
//---------------------------------------------------------------------------
#endif  // WinApiH
