//---------------------------------------------------------------------------
#ifndef VCLCommonH
#define VCLCommonH
//---------------------------------------------------------------------------
#include "Common.h"
#include "Configuration.h"
#include "Exceptions.h"
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
const TColor LinkColor = clBlue;
//---------------------------------------------------------------------------
void __fastcall FixListColumnWidth(TListView * TListView, int Index);
void __fastcall AutoSizeListColumnsWidth(TListView * ListView, int ColumnToShrinkIndex = -1);
void __fastcall EnableControl(TControl* Control, bool Enable);
void __fastcall ReadOnlyControl(TControl * Control, bool ReadOnly = true);
void __fastcall InitializeSystemSettings();
void __fastcall FinalizeSystemSettings();
void __fastcall LocalSystemSettings(TCustomForm * Control);
void __fastcall UseSystemSettingsPre(TCustomForm * Control, void ** Settings = NULL);
void __fastcall UseSystemSettingsPost(TCustomForm * Control, void * Settings = NULL);
void __fastcall UseSystemSettings(TCustomForm * Control, void ** Settings = NULL);
void __fastcall ResetSystemSettings(TCustomForm * Control);
void __fastcall RevokeSystemSettings(TCustomForm * Control, void * Settings);
void __fastcall DeleteSystemSettings(TCustomForm * Control, void * Settings);
void __fastcall LinkLabel(TStaticText * StaticText, UnicodeString Url = L"",
  TNotifyEvent OnEnter = NULL);
void __fastcall HintLabel(TStaticText * StaticText, UnicodeString Hint = L"");
void __fastcall HintLabelRestore(TStaticText * StaticText);
void __fastcall HotTrackLabel(TLabel * Label);
void __fastcall FixComboBoxResizeBug(TCustomComboBox * ComboBox);
void __fastcall ShowAsModal(TForm * Form, void *& Storage);
void __fastcall HideAsModal(TForm * Form, void *& Storage);
void __fastcall ReleaseAsModal(TForm * Form, void *& Storage);
TImageList * __fastcall SharedSystemImageList(bool Large);
bool __fastcall SelectDirectory(UnicodeString & Path, const UnicodeString Prompt,
  bool PreserveFileName);
enum TListViewCheckAll { caCheck, caUncheck, caToggle };
bool __fastcall ListViewAnyChecked(TListView * ListView, bool Checked = true);
void __fastcall ListViewCheckAll(TListView * ListView,
  TListViewCheckAll CheckAll);
void __fastcall ComboAutoSwitchInitialize(TComboBox * ComboBox);
void __fastcall ComboAutoSwitchLoad(TComboBox * ComboBox, TAutoSwitch Value);
TAutoSwitch __fastcall ComboAutoSwitchSave(TComboBox * ComboBox);
void __fastcall CheckBoxAutoSwitchLoad(TCheckBox * CheckBox, TAutoSwitch Value);
TAutoSwitch __fastcall CheckBoxAutoSwitchSave(TCheckBox * CheckBox);
void __fastcall InstallPathWordBreakProc(TWinControl * Control);
void __fastcall SetVerticalControlsOrder(TControl ** ControlsOrder, int Count);
void __fastcall SetHorizontalControlsOrder(TControl ** ControlsOrder, int Count);
void __fastcall MakeNextInTabOrder(TWinControl * Control, TWinControl * After);
void __fastcall CutFormToDesktop(TForm * Form);
void __fastcall UpdateFormPosition(TCustomForm * Form, TPosition Position);
void __fastcall ResizeForm(TCustomForm * Form, int Width, int Height);
TComponent * __fastcall GetFormOwner();
TForm * __fastcall GetMainForm();
void __fastcall SetCorrectFormParent(TForm * Form);
void __fastcall InvokeHelp(TWinControl * Control);
void __fastcall SetFormIcons(TForm * Form, const UnicodeString & BigIconName,
  const UnicodeString & SmallIconName);
Forms::TMonitor *  __fastcall FormMonitor(TCustomForm * Form);
int __fastcall GetLastMonitor();
void __fastcall SetLastMonitor(int MonitorNum);
TForm * __fastcall _SafeFormCreate(TMetaClass * FormClass, TComponent * Owner);
template<class FormType>
FormType * __fastcall SafeFormCreate(TComponent * Owner = NULL)
{
  return dynamic_cast<FormType *>(_SafeFormCreate(__classid(FormType), Owner));
}
bool __fastcall SupportsSplitButton();
TModalResult __fastcall DefaultResult(TCustomForm * Form, TButton * DefaultButton = NULL);
void __fastcall DefaultButton(TButton * Button, bool Default);
void __fastcall MemoKeyDown(TObject * Sender, WORD & Key, TShiftState Shift);
void __fastcall UseDesktopFont(TControl * Control);
void __fastcall LoadResourceImage(TImage * Image, const UnicodeString & ImageName);
UnicodeString __fastcall FormatFormCaption(TCustomForm * Form, const UnicodeString & Caption);
UnicodeString __fastcall FormatMainFormCaption(const UnicodeString & Caption);
//---------------------------------------------------------------------------
#endif  // VCLCommonH
