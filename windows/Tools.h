//---------------------------------------------------------------------------
#ifndef ToolsH
#define ToolsH

#include <comctrls.hpp>
//---------------------------------------------------------------------------
int __fastcall FileOperatorDelete(const AnsiString FileName, bool ToRecycleBin);
void __fastcall CenterFormOn(TForm * Form, TControl * CenterOn);
AnsiString __fastcall GetCoolbarLayoutStr(TCoolBar * CoolBar);
void __fastcall LoadCoolbarLayoutStr(TCoolBar * CoolBar, AnsiString LayoutStr);
void __fastcall SetCoolBandsMinWidth(TCoolBar * CoolBar);
bool __fastcall ExecuteShell(const AnsiString Path, const AnsiString Params);
bool __fastcall ExecuteShellAndWait(const AnsiString Path, const AnsiString Params);
void __fastcall CreateDesktopShortCut(const AnsiString &Name,
  const AnsiString &File, const AnsiString & Params, const AnsiString & Description);
TFontStyles IntToFontStyles(int value);
int FontStylesToInt(const TFontStyles value);
//---------------------------------------------------------------------------
#endif
