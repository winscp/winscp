//---------------------------------------------------------------------------
#ifndef VCLCommonH
#define VCLCommonH
//---------------------------------------------------------------------------
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
void __fastcall AdjustListColumnsWidth(TListView* ListView);
void __fastcall EnableControl(TControl* Control, bool Enable);
void __fastcall UseSystemSettingsPre(TCustomForm * Control, void ** Settings = NULL);
void __fastcall UseSystemSettingsPost(TCustomForm * Control, void * Settings = NULL);
void __fastcall UseSystemSettings(TCustomForm * Control, void ** Settings = NULL);
void __fastcall ResetSystemSettings(TCustomForm * Control);
void __fastcall RevokeSystemSettings(TCustomForm * Control, void * Settings);
void __fastcall LinkLabel(TLabel * Label);
void __fastcall ShowAsModal(TForm * Form, void *& Storage);
void __fastcall HideAsModal(TForm * Form, void *& Storage);
void __fastcall ReleaseAsModal(TForm * Form, void *& Storage);
bool __fastcall SelectDirectory(AnsiString & Path, const AnsiString Prompt,
  bool PreserveFileName);
enum TListViewCheckAll { caCheck, caUncheck, caToggle };
bool __fastcall ListViewAnyChecked(TListView * ListView, bool Checked = true);
void __fastcall ListViewCheckAll(TListView * ListView,
  TListViewCheckAll CheckAll);
void __fastcall InstallPathWordBreakProc(TWinControl * Control);
void __fastcall SetVerticalControlsOrder(TControl ** ControlsOrder, int Count);
void __fastcall SetHorizontalControlsOrder(TControl ** ControlsOrder, int Count);
TPoint __fastcall GetAveCharSize(TCanvas * Canvas);
void __fastcall MakeNextInTabOrder(TWinControl * Control, TWinControl * After);
AnsiString __fastcall GetToolbarsLayoutStr(const TComponent * OwnerComponent);
void __fastcall LoadToolbarsLayoutStr(const TComponent * OwnerComponent, AnsiString LayoutStr);
void __fastcall CutFormToDesktop(TForm * Form);
void __fastcall SetCorrectFormParent(TForm * Form);
//---------------------------------------------------------------------------
#endif  // VCLCommonH
