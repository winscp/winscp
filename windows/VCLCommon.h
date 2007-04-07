//---------------------------------------------------------------------------
#ifndef VCLCommonH
#define VCLCommonH
//---------------------------------------------------------------------------
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
void __fastcall AdjustListColumnsWidth(TListView* ListView, int RowCount = -1,
  int RightPad = 0);
void __fastcall EnableControl(TControl* Control, bool Enable);
void __fastcall ReadOnlyControl(TControl * Control, bool ReadOnly = true);
void __fastcall InitSystemSettings(TComponent * Control);
void __fastcall UseSystemSettingsPre(TCustomForm * Control, void ** Settings = NULL);
void __fastcall UseSystemSettingsPost(TCustomForm * Control, void * Settings = NULL);
void __fastcall UseSystemSettings(TCustomForm * Control, void ** Settings = NULL);
void __fastcall ResetSystemSettings(TCustomForm * Control);
void __fastcall RevokeSystemSettings(TCustomForm * Control, void * Settings);
void __fastcall DeleteSystemSettings(TCustomForm * Control, void * Settings);
void __fastcall LinkLabel(TStaticText * StaticText, AnsiString Url = "",
  TNotifyEvent OnEnter = NULL);
void __fastcall HintLabel(TStaticText * StaticText, AnsiString Hint = "");
void __fastcall HintLabelRestore(TStaticText * StaticText);
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
void __fastcall RepaintStatusBar(TCustomStatusBar * StatusBar);
void __fastcall SetVerticalControlsOrder(TControl ** ControlsOrder, int Count);
void __fastcall SetHorizontalControlsOrder(TControl ** ControlsOrder, int Count);
TPoint __fastcall GetAveCharSize(TCanvas * Canvas);
void __fastcall MakeNextInTabOrder(TWinControl * Control, TWinControl * After);
void __fastcall CutFormToDesktop(TForm * Form);
void __fastcall UpdateFormPosition(TForm * Form, TPosition Position);
void __fastcall ResizeForm(TForm * Form, int Width, int Height);
void __fastcall SetCorrectFormParent(TForm * Form);
void __fastcall InvokeHelp(TWinControl * Control);
//---------------------------------------------------------------------------
#endif  // VCLCommonH
