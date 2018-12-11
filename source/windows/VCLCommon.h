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
void __fastcall ReadOnlyAndEnabledControl(TControl * Control, bool ReadOnly, bool Enabled);
void __fastcall InitializeSystemSettings();
void __fastcall FinalizeSystemSettings();
void __fastcall LocalSystemSettings(TCustomForm * Control);
void __fastcall UseSystemSettingsPre(TCustomForm * Control);
void __fastcall UseSystemSettingsPost(TCustomForm * Control);
void __fastcall UseSystemSettings(TCustomForm * Control);
void __fastcall ResetSystemSettings(TCustomForm * Control);
void __fastcall LinkLabel(TStaticText * StaticText, UnicodeString Url = L"",
  TNotifyEvent OnEnter = NULL);
void __fastcall LinkActionLabel(TStaticText * StaticText);
void __fastcall LinkAppLabel(TStaticText * StaticText);
void __fastcall HintLabel(TStaticText * StaticText, UnicodeString Hint = L"");
void __fastcall HotTrackLabel(TLabel * Label);
void __fastcall SetLabelHintPopup(TLabel * Label, const UnicodeString & Hint);
bool __fastcall HasLabelHintPopup(TControl * Control, const UnicodeString & HintStr);
void __fastcall FixComboBoxResizeBug(TCustomComboBox * ComboBox);
void __fastcall ShowAsModal(TForm * Form, void *& Storage, bool BringToFront = true);
void __fastcall HideAsModal(TForm * Form, void *& Storage);
bool __fastcall ReleaseAsModal(TForm * Form, void *& Storage);
bool __fastcall IsMainFormLike(TCustomForm * Form);
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
void __fastcall FixFormIcons(TForm * Form);
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
void __fastcall UpdateDesktopFont();
UnicodeString __fastcall FormatFormCaption(TCustomForm * Form, const UnicodeString & Caption);
UnicodeString __fastcall FormatMainFormCaption(const UnicodeString & Caption);
TShiftState __fastcall AllKeyShiftStates();
void __fastcall RealignControl(TControl * Control);
void __fastcall HookFormActivation(TCustomForm * Form);
void __fastcall UnhookFormActivation(TCustomForm * Form);
void __fastcall ShowFormNoActivate(TForm * Form);
TPanel * __fastcall CreateBlankPanel(TComponent * Owner);
typedef void __fastcall (*TRescaleEvent)(TComponent * Sender, TObject * Token);
void __fastcall SetRescaleFunction(
  TComponent * Component, TRescaleEvent OnRescale, TObject * Token = NULL, bool OwnsToken = false);
void __fastcall RecordFormImplicitRescale(TForm * Form);
//---------------------------------------------------------------------------
#endif  // VCLCommonH
