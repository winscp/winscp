//---------------------------------------------------------------------------
#ifndef ToolsH
#define ToolsH

#include <comctrls.hpp>
//---------------------------------------------------------------------------
// from shlobj.h
#define CSIDL_SENDTO                    0x0009        // <user name>\SendTo
#define CSIDL_DESKTOPDIRECTORY          0x0010        // <user name>\Desktop
#define CSIDL_COMMON_DESKTOPDIRECTORY   0x0019        // All Users\Desktop
#define CSIDL_APPDATA                   0x001a        // <user name>\Application Data
//---------------------------------------------------------------------------
int __fastcall FileOperatorDelete(const AnsiString FileName, bool ToRecycleBin);
void __fastcall CenterFormOn(TForm * Form, TControl * CenterOn);
AnsiString __fastcall GetCoolbarLayoutStr(TCoolBar * CoolBar);
void __fastcall LoadCoolbarLayoutStr(TCoolBar * CoolBar, AnsiString LayoutStr);
void __fastcall SetCoolBandsMinWidth(TCoolBar * CoolBar);
bool __fastcall ExecuteShell(const AnsiString Path, const AnsiString Params);
bool __fastcall ExecuteShellAndWait(const AnsiString Path, const AnsiString Params);
void __fastcall CreateDesktopShortCut(const AnsiString &Name,
  const AnsiString &File, const AnsiString & Params, const AnsiString & Description,
  int SpecialFolder = -1);
TFontStyles IntToFontStyles(int value);
int FontStylesToInt(const TFontStyles value);
//---------------------------------------------------------------------------
#endif
