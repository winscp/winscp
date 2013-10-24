//---------------------------------------------------------------------------
#ifndef VCLCommonH
#define VCLCommonH
//---------------------------------------------------------------------------
#include "Common.h"
#include "Configuration.h"
#include "Exceptions.h"
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
void __fastcall AdjustListColumnsWidth(TListView* ListView, int RowCount = -1,
  int RightPad = 0);
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
TModalResult __fastcall DefaultResult(TCustomForm * Form);
void __fastcall UseDesktopFont(TControl * Control);
//---------------------------------------------------------------------------
// When exception is left to be handled by Application->OnException
// memory error occurs when clearing the exception for unknown reason.
// (possibly only when exception handling takes too long,
// such as when reconnect is retried)
// deferring call after the catch clause gets rid of the problem.
// on the other hand, not sure if there's some code that actually
// relies on exception leaving the method when we use APPLICATION_EXCEPTION_HACK*
// (such as then timer is called from withing event loop run
// by our code, which we want to break too with the exception).
// Steps to reproduce the problem:
// 1) Connect
// 2) Terminate the server
// 3) Let is try to reconnect once
// 4) Start the server and let it reconnect
// 5) Upon clearing the original terminattion exception from 2)
//    the EAccessViolation is thrown
class TApplicationExceptionHackHelper
{
public:
  void __fastcall Catch(Exception & E)
  {
    FSavedException.reset(CloneException(&E));
  }

  void __fastcall Handle()
  {
    if (FSavedException.get() != NULL)
    {
      Application->OnException(Application, FSavedException.get());
      Abort();
    }
  }

private:
  std::unique_ptr<Exception> FSavedException;
};
//---------------------------------------------------------------------------
#define APPLICATION_EXCEPTION_HACK_BEGIN \
  TApplicationExceptionHackHelper _ExceptionHackHelper; \
  try
#define APPLICATION_EXCEPTION_HACK_END \
  catch (Exception & E) \
  { \
    _ExceptionHackHelper.Catch(E); \
  } \
  _ExceptionHackHelper.Handle();
//---------------------------------------------------------------------------
#endif  // VCLCommonH
