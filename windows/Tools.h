//---------------------------------------------------------------------------
#ifndef ToolsH
#define ToolsH

#include <comctrls.hpp>
#include <WinInterface.h>
//---------------------------------------------------------------------------
void __fastcall CenterFormOn(TForm * Form, TControl * CenterOn);
bool __fastcall ExecuteShellAndWait(const AnsiString Path, const AnsiString Params);
bool __fastcall ExecuteShellAndWait(const AnsiString Command);
void __fastcall CreateDesktopShortCut(const AnsiString &Name,
  const AnsiString &File, const AnsiString & Params, const AnsiString & Description,
  int SpecialFolder = -1);
AnsiString __fastcall GetListViewStr(TListView * ListView);
void __fastcall LoadListViewStr(TListView * ListView, AnsiString LayoutStr);
void __fastcall RestoreForm(AnsiString Data, TForm * Form);
AnsiString __fastcall StoreForm(TCustomForm * Form);
TFontStyles __fastcall IntToFontStyles(int value);
int __fastcall FontStylesToInt(const TFontStyles value);
void __fastcall ValidateMaskEdit(TComboBox * Edit);
void __fastcall ValidateMaskEdit(TEdit * Edit);
void __fastcall OpenBrowser(AnsiString URL);
bool __fastcall IsFormatInClipboard(unsigned int Format);
bool __fastcall TextFromClipboard(AnsiString & Text);
void __fastcall ExitActiveControl(TForm * Form);
AnsiString __fastcall ReadResource(const AnsiString ResName);
bool __fastcall DumpResourceToFile(const AnsiString ResName,
  const AnsiString FileName);
void __fastcall BrowseForExecutable(TEdit * Control, AnsiString Title,
  AnsiString Filter, bool FileNameCommand);
void __fastcall BrowseForExecutable(TComboBox * Control, AnsiString Title,
  AnsiString Filter, bool FileNameCommand);
//---------------------------------------------------------------------------
#endif
