//---------------------------------------------------------------------------
#ifndef ToolsH
#define ToolsH

#include <comctrls.hpp>
#include <WinInterface.h>
//---------------------------------------------------------------------------
void __fastcall CenterFormOn(TForm * Form, TControl * CenterOn);
AnsiString __fastcall GetCoolbarLayoutStr(TCoolBar * CoolBar);
void __fastcall LoadCoolbarLayoutStr(TCoolBar * CoolBar, AnsiString LayoutStr);
void __fastcall SetCoolBandsMinWidth(TCoolBar * CoolBar);
void __fastcall CreateDesktopShortCut(const AnsiString &Name,
  const AnsiString &File, const AnsiString & Params, const AnsiString & Description,
  int SpecialFolder = -1);
AnsiString __fastcall GetListViewStr(TListView * ListView);
void __fastcall LoadListViewStr(TListView * ListView, AnsiString LayoutStr);
TFontStyles __fastcall IntToFontStyles(int value);
int __fastcall FontStylesToInt(const TFontStyles value);
void __fastcall ValidateMaskEdit(TComboBox * Edit);
//---------------------------------------------------------------------------
#endif
