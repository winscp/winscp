//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "WinInterface.h"
#include "VCLCommon.h"

#include <Common.h>
#include <TextsWin.h>
#include <RemoteFiles.h>
#include <GUITools.h>
#include <Tools.h>

#include <FileCtrl.hpp>
#include <ThemeMgr.hpp>
#include <PathLabel.hpp>
#include <PasTools.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
static TThemeManager * ThemeManager = NULL;
//---------------------------------------------------------------------------
void __fastcall AdjustListColumnsWidth(TListView* ListView, int RowCount, int RightPad)
{
  int OriginalWidth, NewWidth, i, CWidth, LastResizible;

  OriginalWidth = 0;
  LastResizible = -1;
  for (i = 0; i < ListView->Columns->Count; i++)
  {
    OriginalWidth += ListView->Columns->Items[i]->Width;
    if (ListView->Columns->Items[i]->Tag == 0)
    {
      LastResizible = i;
    }
  }
  assert(LastResizible >= 0);

  // when listview is virtual, ListView->Items->Count seems to return invalid
  // value, thus provide a method to pass actual count explicitly
  if (RowCount < 0)
  {
    RowCount = ListView->Items->Count;
  }

  NewWidth = 0;
  CWidth = ListView->ClientWidth - RightPad;
  if ((ListView->VisibleRowCount < RowCount) &&
      (ListView->Width - ListView->ClientWidth < GetSystemMetrics(SM_CXVSCROLL)))
  {
    CWidth -= GetSystemMetrics(SM_CXVSCROLL);
  }
  for (i = 0; i < ListView->Columns->Count; i++)
  {
    if (i != LastResizible)
    {
      if (ListView->Columns->Items[i]->Tag == 0)
      {
        ListView->Columns->Items[i]->Width =
          (CWidth * ListView->Columns->Items[i]->Width) / OriginalWidth;
      }
      NewWidth += ListView->Columns->Items[i]->Width;
    }
  }
  ListView->Columns->Items[LastResizible]->Width = CWidth-NewWidth;
}
//---------------------------------------------------------------------------
static void __fastcall SetParentColor(TControl * Control)
{
  TColor Color;
  assert(ThemeManager != NULL);
  if (ThemeManager->ThemesEnabled)
  {
    bool OnTabSheet = false;
    TWinControl * Parent = Control->Parent;
    while ((Parent != NULL) && !OnTabSheet)
    {
      TTabSheet * TabSheet = dynamic_cast<TTabSheet *>(Parent);
      OnTabSheet = (TabSheet != NULL) && TabSheet->TabVisible;
      Parent = Parent->Parent;
    }

    if (OnTabSheet)
    {
      Color = ThemeManager->GetColor(teTab, ::TABP_BODY, 0, ::TMT_FILLCOLORHINT);
    }
    else
    {
      Color = ThemeManager->GetColor(teWindow, ::WP_DIALOG, 0, ::TMT_FILLCOLOR);
    }
  }
  else
  {
    Color = clBtnFace;
  }

  ((TEdit*)Control)->Color = Color;
}
//---------------------------------------------------------------------------
void __fastcall EnableControl(TControl * Control, bool Enable)
{
  if (Control->Enabled != Enable)
  {
    if (Control->InheritsFrom(__classid(TWinControl)) &&
        (((TWinControl*)Control)->ControlCount > 0))
    {
      for (Integer Index = 0; Index < ((TWinControl*)Control)->ControlCount; Index++)
        EnableControl(((TWinControl*)Control)->Controls[Index], Enable);
    }
    Control->Enabled = Enable;
  }
  if (Control->InheritsFrom(__classid(TCustomEdit)) ||
      Control->InheritsFrom(__classid(TCustomComboBox)) ||
      Control->InheritsFrom(__classid(TCustomListView)))
  {
    if (Enable)
    {
      ((TEdit*)Control)->Color = clWindow;
    }
    else
    {
      ((TEdit*)Control)->Color = clBtnFace;
    }
  }
};
//---------------------------------------------------------------------------
void __fastcall ReadOnlyControl(TControl * Control, bool ReadOnly)
{
  if (Control->InheritsFrom(__classid(TCustomEdit)))
  {
    ((TEdit*)Control)->ReadOnly = ReadOnly;
    if (ReadOnly)
    {
      SetParentColor(Control);
    }
    else
    {
      ((TEdit*)Control)->Color = clWindow;
    }
  }
  else
  {
    assert(false);
  }
}
//---------------------------------------------------------------------------
struct TSavedSystemSettings
{
  TCustomForm * Form;
  AnsiString FontName;
  bool Flipped;
  TWndMethod OldWndProc;
};
//---------------------------------------------------------------------------
static void __fastcall ThemeManagerAllowSubclassing(void * /*Data*/,
  TThemeManager * /*Sender*/, TControl * Control, bool & Allow)
{
  TPathLabel * PathLabel = dynamic_cast<TPathLabel *>(Control);
  // intent is to only exclude path labels on the main window
  if ((PathLabel != NULL) && (PathLabel->FocusControl != NULL))
  {
    Allow = false;
  }
  // tree view on location profiles dialog occasionally does not show,
  // it is unique in that is is placed on group box (however I'm not sure if
  // it is the property that makes it not work)
  if ((dynamic_cast<TTreeView *>(Control) != NULL) &&
      (dynamic_cast<TCustomGroupBox *>(Control->Parent) != NULL))
  {
    Allow = false;
  }
}
//---------------------------------------------------------------------------
class TPublicControl : public TWinControl
{
friend TWndMethod __fastcall ControlWndProc(TWinControl * Control);
};
//---------------------------------------------------------------------------
TWndMethod __fastcall ControlWndProc(TWinControl * Control)
{
  TPublicControl * PublicControl = static_cast<TPublicControl *>(Control);
  return &PublicControl->WndProc;
}
//---------------------------------------------------------------------------
static TMonitor * LastMonitor = NULL;
//---------------------------------------------------------------------------
inline void __fastcall DoFormWindowProc(TCustomForm * Form, TWndMethod WndProc,
  TMessage & Message)
{
  if ((Message.Msg == WM_SYSCOMMAND) &&
      (Message.WParam == SC_CONTEXTHELP))
  {
    InvokeHelp(Form->ActiveControl);
    Message.Result = 1;
  }
  else if (Message.Msg == CM_SHOWINGCHANGED)
  {
    if ((Application->MainForm == Form) ||
        // this particularly happens if error occurs while main
        // window is being shown (e.g. non existent local directory when opening
        // explorer)
        ((Application->MainForm != NULL) && !Application->MainForm->Visible))
    {
      if (!Form->Showing)
      {
        // when closing main form, remember its monitor,
        // so that the next form is shown on the same one
        LastMonitor = Form->Monitor;
      }
      else if ((LastMonitor != NULL) && (LastMonitor != Form->Monitor) &&
                Form->Showing)
      {
        TForm * AForm = dynamic_cast<TForm *>(Form);
        assert(AForm != NULL);
        // would actually always be poScreenCenter, see _SafeFormCreate
        if ((AForm->Position == poMainFormCenter) ||
            (AForm->Position == poScreenCenter))
        {
          // this would typically be an authentication dialog,
          // but it may as well be an message box

          // taken from TCustomForm::SetWindowToMonitor
          AForm->SetBounds(LastMonitor->Left + ((LastMonitor->Width - AForm->Width) / 2),
            LastMonitor->Top + ((LastMonitor->Height - AForm->Height) / 2),
             AForm->Width, AForm->Height);
          AForm->Position = poDesigned;
        }
        else if ((AForm->Position != poDesigned) &&
                 (AForm->Position != poDefaultPosOnly))
        {
          // we do not expect any other positioning
          assert(false);
        }
      }
      else
      {
        TForm * AForm = dynamic_cast<TForm *>(Form);
        assert(AForm != NULL);
        // otherwise it would not get centered
        if (AForm->Position == poMainFormCenter)
        {
          AForm->Position = poScreenCenter;
        }
      }
    }
    WndProc(Message);
  }
  else
  {
    WndProc(Message);
  }
}
//---------------------------------------------------------------------------
static void __fastcall FormWindowProc(void * Data, TMessage & Message)
{
  TCustomForm * Form = static_cast<TCustomForm *>(Data);
  DoFormWindowProc(Form, ControlWndProc(Form), Message);
}
//---------------------------------------------------------------------------
static void __fastcall FormWindowProcEx(void * Data, TMessage & Message)
{
  TSavedSystemSettings * SSettings = static_cast<TSavedSystemSettings *>(Data);
  DoFormWindowProc(SSettings->Form, SSettings->OldWndProc, Message);
}
//---------------------------------------------------------------------------
void __fastcall InitializeSystemSettings()
{
  if (ThemeManager == NULL)
  {
    ThemeManager = new TThemeManager(Application);
    ThemeManager->Name = "ThemeManager";
    // ListView subclassing breaks TDirView
    ThemeManager->Options = (ThemeManager->Options >> toSubclassListView);
    // Speed Button subclassing on rights frame does not work somehow
    // and they are not used elsewhere
    ThemeManager->Options = (ThemeManager->Options >> toSubclassSpeedButtons);
    TAllowSubclassingEvent OnAllowSubclassing;
    ((TMethod*)&OnAllowSubclassing)->Code = ThemeManagerAllowSubclassing;
    ThemeManager->OnAllowSubclassing = OnAllowSubclassing;
  }
}
//---------------------------------------------------------------------------
void __fastcall FinalizeSystemSettings()
{
  if (ThemeManager != NULL)
  {
    SAFE_DESTROY(ThemeManager);
  }
}
//---------------------------------------------------------------------------
// Settings that must be set as soon as possible.
void __fastcall UseSystemSettingsPre(TCustomForm * Control, void ** Settings)
{
  LocalSystemSettings(Control);

  TWndMethod WindowProc;

  if (Settings)
  {
    TSavedSystemSettings * SSettings;
    SSettings = new TSavedSystemSettings();
    *Settings = static_cast<void*>(SSettings);
    SSettings->Form = Control;
    SSettings->FontName = Control->Font->Name;
    SSettings->OldWndProc = Control->WindowProc;

    ((TMethod*)&WindowProc)->Data = SSettings;
    ((TMethod*)&WindowProc)->Code = FormWindowProcEx;
  }
  else
  {
    ((TMethod*)&WindowProc)->Data = Control;
    ((TMethod*)&WindowProc)->Code = FormWindowProc;
  }

  Control->WindowProc = WindowProc;

  assert(Control && Control->Font);
  Control->Font->Name = "MS Shell Dlg";

  if (Control->HelpKeyword.IsEmpty())
  {
    // temporary help keyword to enable F1 key in all forms
    Control->HelpKeyword = "start";
  }

  // especially on login dialog, we need to reapply themes with language change
  if (ThemeManager != NULL)
  {
    ThemeManager->CollectForms(Control);
  }
};
//---------------------------------------------------------------------------
// Settings that must be set only after whole form is constructed
void __fastcall UseSystemSettingsPost(TCustomForm * Control, void * Settings)
{
  bool Flip;
  AnsiString FlipStr = LoadStr(FLIP_CHILDREN);
  Flip = !FlipStr.IsEmpty() && static_cast<bool>(StrToInt(FlipStr));

  if (Settings != NULL)
  {
    static_cast<TSavedSystemSettings*>(Settings)->Flipped = Flip;
  }

  if (Flip)
  {
    Control->FlipChildren(true);
  }

  ResetSystemSettings(Control);
};
//---------------------------------------------------------------------------
void __fastcall UseSystemSettings(TCustomForm * Control, void ** Settings)
{
  UseSystemSettingsPre(Control, Settings);
  UseSystemSettingsPost(Control, (Settings != NULL) ? *Settings : NULL);
};
//---------------------------------------------------------------------------
void __fastcall ResetSystemSettings(TCustomForm * /*Control*/)
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall DeleteSystemSettings(TCustomForm * Control, void * Settings)
{
  assert(Settings);
  TSavedSystemSettings * SSettings = static_cast<TSavedSystemSettings *>(Settings);

  Control->WindowProc = SSettings->OldWndProc;
  delete SSettings;
}
//---------------------------------------------------------------------------
void __fastcall RevokeSystemSettings(TCustomForm * Control, void * Settings)
{
  assert(Settings);
  TSavedSystemSettings* SSettings = static_cast<TSavedSystemSettings*>(Settings);
  if (SSettings->Flipped)
  {
    Control->FlipChildren(true);
  }
  DeleteSystemSettings(Control, Settings);
};
//---------------------------------------------------------------------------
class TPublicForm : public TForm
{
friend void __fastcall ShowAsModal(TForm * Form, void *& Storage);
friend void __fastcall HideAsModal(TForm * Form, void *& Storage);
};
//---------------------------------------------------------------------------
struct TShowAsModalStorage
{
  void * FocusWindowList;
  void * FocusActiveWindow;
  TFocusState FocusState;
};
//---------------------------------------------------------------------------
static TCustomForm ** __fastcall FocusedForm()
{
  return reinterpret_cast<TCustomForm **>(reinterpret_cast<char *>(Screen) + 0x78);
}
//---------------------------------------------------------------------------
static TList * __fastcall SaveFocusedList()
{
  return *reinterpret_cast<TList **>(reinterpret_cast<char *>(Screen) + 0x7C);
}
//---------------------------------------------------------------------------
void __fastcall ShowAsModal(TForm * Form, void *& Storage)
{
  SetCorrectFormParent(Form);
  CancelDrag();
  if (GetCapture() != 0) SendMessage(GetCapture(), WM_CANCELMODE, 0, 0);
  ReleaseCapture();
  (static_cast<TPublicForm*>(Form))->FFormState << fsModal;

  TShowAsModalStorage * AStorage = new TShowAsModalStorage;

  AStorage->FocusActiveWindow = GetActiveWindow();
  AStorage->FocusState = SaveFocusState();
  SaveFocusedList()->Insert(0, *FocusedForm());
  *FocusedForm() = Form;
  AStorage->FocusWindowList = DisableTaskWindows(0);
  Form->Show();
  SendMessage(Form->Handle, CM_ACTIVATE, 0, 0);

  Storage = AStorage;
}
//---------------------------------------------------------------------------
void __fastcall HideAsModal(TForm * Form, void *& Storage)
{
  assert((static_cast<TPublicForm*>(Form))->FFormState.Contains(fsModal));
  TShowAsModalStorage * AStorage = static_cast<TShowAsModalStorage *>(Storage);
  Storage = NULL;

  SendMessage(Form->Handle, CM_DEACTIVATE, 0, 0);
  if (GetActiveWindow() != Form->Handle)
  {
    AStorage->FocusActiveWindow = 0;
  }
  Form->Hide();

  EnableTaskWindows(AStorage->FocusWindowList);

  TList * ASaveFocusedList = SaveFocusedList();
  TCustomForm ** AFocusedForm = FocusedForm();
  if (ASaveFocusedList->Count > 0)
  {
    *AFocusedForm = static_cast<TCustomForm *>(ASaveFocusedList->First());
    ASaveFocusedList->Remove(*AFocusedForm);
  }
  else
  {
    *AFocusedForm = NULL;
  }

  if (AStorage->FocusActiveWindow != 0)
  {
    SetActiveWindow(AStorage->FocusActiveWindow);
  }

  RestoreFocusState(AStorage->FocusState);

  (static_cast<TPublicForm*>(Form))->FFormState >> fsModal;

  delete AStorage;
}
//---------------------------------------------------------------------------
void __fastcall ReleaseAsModal(TForm * Form, void *& Storage)
{
  if (Storage != NULL)
  {
    HideAsModal(Form, Storage);
  }
}
//---------------------------------------------------------------------------
bool __fastcall SelectDirectory(AnsiString & Path, const AnsiString Prompt,
  bool PreserveFileName)
{
  bool Result;
  unsigned int ErrorMode;
  ErrorMode = SetErrorMode(SEM_NOOPENFILEERRORBOX | SEM_FAILCRITICALERRORS);

  try
  {
    AnsiString Directory;
    AnsiString FileName;
    if (!PreserveFileName || DirectoryExists(Path))
    {
      Directory = Path;
    }
    else
    {
      Directory = ExtractFilePath(Path);
      FileName = ExtractFileName(Path);
    }
    Result = SelectDirectory(Prompt, "", Directory);
    if (Result)
    {
      Path = Directory;
      if (!FileName.IsEmpty())
      {
        Path = IncludeTrailingBackslash(Path) + FileName;
      }
    }
  }
  __finally
  {
    SetErrorMode(ErrorMode);
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall ListViewAnyChecked(TListView * ListView, bool Checked)
{
  bool AnyChecked = false;
  for (int Index = 0; Index < ListView->Items->Count; Index++)
  {
    if (ListView->Items->Item[Index]->Checked == Checked)
    {
      AnyChecked = true;
      break;
    }
  }
  return AnyChecked;
}
//---------------------------------------------------------------------------
void __fastcall ListViewCheckAll(TListView * ListView,
  TListViewCheckAll CheckAll)
{
  bool Check;

  if (CheckAll == caToggle)
  {
    Check = ListViewAnyChecked(ListView, false);
  }
  else
  {
    Check = (CheckAll == caCheck);
  }

  for (int Index = 0; Index < ListView->Items->Count; Index++)
  {
    ListView->Items->Item[Index]->Checked = Check;
  }
}
//---------------------------------------------------------------------------
// Windows algorithm is as follows (tested on W2k):
// right:
//   is_delimiter(current)
//     false:
//       right(left(current) + 1)
//     true:
//       right(right(current) + 1)
// left:
//   right(left(current) + 1)
int CALLBACK PathWordBreakProc(char * Ch, int Current, int Len, int Code)
{
  char Delimiters[] = "\\/ ;,.";
  int Result;
  AnsiString ACh;
  // stupid unicode autodetection
  // (on WinXP (or rather for RichEdit 2.0) we get unicode on input)
  if ((Len > 1) && (Ch[1] == '\0'))
  {
    // this convertes the unicode to ansi
    ACh = (wchar_t*)Ch;
  }
  else
  {
    ACh = Ch;
  }
  // it may not be NULL terminated
  ACh.SetLength(Len);
  if (Code == WB_ISDELIMITER)
  {
    // we return negacy of what WinAPI docs says
    Result = (strchr(Delimiters, ACh[Current + 1]) == NULL);
  }
  else if (Code == WB_LEFT)
  {
    Result = ACh.SubString(1, Current - 1).LastDelimiter(Delimiters);
  }
  else if (Code == WB_RIGHT)
  {
    if (Current == 0)
    {
      // will be called again with Current == 1
      Result = 0;
    }
    else
    {
      const char * P = strpbrk(ACh.c_str() + Current - 1, Delimiters);
      if (P == NULL)
      {
        Result = Len;
      }
      else
      {
        Result = P - ACh.c_str() + 1;
      }
    }
  }
  else
  {
    assert(false);
    Result = 0;
  }
  return Result;
}
//---------------------------------------------------------------------------
class TPublicCustomCombo : public TCustomCombo
{
friend void __fastcall InstallPathWordBreakProc(TWinControl * Control);
};
//---------------------------------------------------------------------------
void __fastcall InstallPathWordBreakProc(TWinControl * Control)
{
  HWND Wnd;
  if (dynamic_cast<TCustomCombo*>(Control) != NULL)
  {
    TPublicCustomCombo * Combo =
      static_cast<TPublicCustomCombo *>(dynamic_cast<TCustomCombo *>(Control));
    Combo->HandleNeeded();
    Wnd = Combo->EditHandle;
  }
  else
  {
    Wnd = Control->Handle;
  }
  SendMessage(Wnd, EM_SETWORDBREAKPROC, 0, (LPARAM)(EDITWORDBREAKPROC)PathWordBreakProc);
}
//---------------------------------------------------------------------------
static void __fastcall RemoveHiddenControlsFromOrder(TControl ** ControlsOrder, int & Count)
{
  int Shift = 0;
  for (int Index = 0; Index < Count; Index++)
  {
    if (ControlsOrder[Index]->Visible)
    {
      ControlsOrder[Index - Shift] = ControlsOrder[Index];
    }
    else
    {
      Shift++;
    }
  }
  Count -= Shift;
}
//---------------------------------------------------------------------------
void __fastcall RepaintStatusBar(TCustomStatusBar * StatusBar)
{
  StatusBar->SimplePanel = !StatusBar->SimplePanel;
  StatusBar->SimplePanel = !StatusBar->SimplePanel;
}
//---------------------------------------------------------------------------
void __fastcall SetVerticalControlsOrder(TControl ** ControlsOrder, int Count)
{
  RemoveHiddenControlsFromOrder(ControlsOrder, Count);

  if (Count > 0)
  {
    TWinControl * CommonParent = ControlsOrder[0]->Parent;
    CommonParent->DisableAlign();
    try
    {
      int Top = 0;
      for (int Index = 0; Index < Count; Index++)
      {
        assert(ControlsOrder[Index]->Parent == CommonParent);
        if ((Index == 0) || (Top > ControlsOrder[Index]->Top))
        {
          Top = ControlsOrder[Index]->Top;
        }
      }

      for (int Index = 0; Index < Count; Index++)
      {
        ControlsOrder[Index]->Top = Top;
        Top += ControlsOrder[Index]->Height;
      }
    }
    __finally
    {
      CommonParent->EnableAlign();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall SetHorizontalControlsOrder(TControl ** ControlsOrder, int Count)
{
  RemoveHiddenControlsFromOrder(ControlsOrder, Count);

  if (Count > 0)
  {
    TWinControl * CommonParent = ControlsOrder[0]->Parent;
    CommonParent->DisableAlign();
    try
    {
      int Left = 0;
      for (int Index = 0; Index < Count; Index++)
      {
        assert(ControlsOrder[Index]->Parent == CommonParent);
        if ((Index == 0) || (Left > ControlsOrder[Index]->Left))
        {
          Left = ControlsOrder[Index]->Left;
        }
      }

      for (int Index = 0; Index < Count; Index++)
      {
        ControlsOrder[Index]->Left = Left;
        Left += ControlsOrder[Index]->Width;
      }
    }
    __finally
    {
      CommonParent->EnableAlign();
    }
  }
}
//---------------------------------------------------------------------------
TPoint __fastcall GetAveCharSize(TCanvas* Canvas)
{
  Integer I;
  Char Buffer[52];
  TSize Result;
  for (I = 0; I <= 25; I++) Buffer[I] = (Char)('A' + I);
  for (I = 0; I <= 25; I++) Buffer[I+26] = (Char)('a' + I);
  GetTextExtentPoint(Canvas->Handle, Buffer, 52, &Result);
  return TPoint(Result.cx / 52, Result.cy);
}
//---------------------------------------------------------------------------
void __fastcall MakeNextInTabOrder(TWinControl * Control, TWinControl * After)
{
  if (After->TabOrder > Control->TabOrder)
  {
    After->TabOrder = Control->TabOrder;
  }
  else if (After->TabOrder < Control->TabOrder - 1)
  {
    After->TabOrder = static_cast<TTabOrder>(Control->TabOrder - 1);
  }
}
//---------------------------------------------------------------------------
void __fastcall CutFormToDesktop(TForm * Form)
{
  assert(Form->Monitor != NULL);
  TRect Workarea = Form->Monitor->WorkareaRect;
  if (Form->Top + Form->Height > Workarea.Bottom)
  {
    Form->Height = Workarea.Bottom - Form->Top;
  }
  if (Form->Left + Form->Width >= Workarea.Right)
  {
    Form->Width = Workarea.Right - Form->Left;
  }
}
//---------------------------------------------------------------------------
void __fastcall UpdateFormPosition(TCustomForm * Form, TPosition Position)
{
  if ((Position == poScreenCenter) ||
      (Position == poOwnerFormCenter) ||
      (Position == poMainFormCenter))
  {
    TCustomForm * CenterForm = NULL;
    if ((Position == poOwnerFormCenter) ||
        (Position == poMainFormCenter))
    {
      CenterForm = Application->MainForm;
      if ((Position == poOwnerFormCenter) &&
          (dynamic_cast<TCustomForm*>(Form->Owner) != NULL))
      {
        CenterForm = dynamic_cast<TCustomForm*>(Form->Owner);
      }
    }

    TRect Bounds = Form->BoundsRect;
    int X, Y;
    if (CenterForm != NULL)
    {
      X = ((((TForm *)CenterForm)->Width - Bounds.Width()) / 2) +
        ((TForm *)CenterForm)->Left;
      Y = ((((TForm *)CenterForm)->Height - Bounds.Height()) / 2) +
        ((TForm *)CenterForm)->Top;
    }
    else
    {
      X = (Screen->Width - Bounds.Width()) / 2;
      Y = (Screen->Height - Bounds.Height()) / 2;
    }

    if (X < 0)
    {
      X = 0;
    }
    if (Y < 0)
    {
      Y = 0;
    }

    Form->SetBounds(X, Y, Bounds.Width(), Bounds.Height());
  }
}
//---------------------------------------------------------------------------
void __fastcall ResizeForm(TCustomForm * Form, int Width, int Height)
{
  if (Height > Screen->WorkAreaHeight)
  {
    Height = Screen->WorkAreaHeight;
  }
  if (Width > Screen->WorkAreaWidth)
  {
    Width = Screen->WorkAreaWidth;
  }
  if (Height < Form->Constraints->MinHeight)
  {
    Height = Form->Constraints->MinHeight;
  }
  if (Width < Form->Constraints->MinWidth)
  {
    Width = Form->Constraints->MinWidth;
  }
  TRect Bounds = Form->BoundsRect;
  int Top = Bounds.Top + ((Bounds.Height() - Height) / 2);
  int Left = Bounds.Left + ((Bounds.Width() - Width) / 2);
  if (Top + Height > Screen->WorkAreaTop + Screen->WorkAreaHeight)
  {
    Top = Screen->WorkAreaTop + Screen->WorkAreaHeight - Height;
  }
  if (Left + Width >= Screen->WorkAreaLeft + Screen->WorkAreaWidth)
  {
    Left = Screen->WorkAreaLeft + Screen->WorkAreaWidth - Width;
  }
  if (Top < 0)
  {
    Top = 0;
  }
  if (Left < 0)
  {
    Left = 0;
  }
  Form->SetBounds(Left, Top, Width, Height);
  Bounds = Form->BoundsRect;
  // due to constraints, form can remain larger, make sure it is centered although
  Left = Bounds.Left + ((Width - Bounds.Width()) / 2);
  Top = Bounds.Top + ((Height - Bounds.Height()) / 2);
  Form->SetBounds(Left, Top, Width, Height);
}
//---------------------------------------------------------------------------
void __fastcall SetCorrectFormParent(TForm * Form)
{
  try
  {
    // Kind of hack (i do not understand this much).
    // Rationale: for example when the preferences window is opened from login dialog
    // settings Parent to Screen->ActiveForm leads to "cannot focus disabled control",
    // so we set Parent only when absolutelly necessary
    // (dialog opened from log window or editor)
    // TODO: does not work for dialogs opened from preferences dialog
    if ((Application->MainForm != NULL) &&
        (Application->MainForm != Screen->ActiveForm))
    {
      // this should better be check for modal form
      AnsiString C = Screen->ActiveForm->Caption;
      if ((Screen->ActiveForm != NULL) &&
          (Screen->ActiveForm->BorderStyle != bsDialog))
      {
        Form->ParentWindow = Screen->ActiveForm->Handle;
      }
    }
  }
  catch(...)
  {
    // avoid any errors, however we want to know about this in debug version.
    #ifdef _DEBUG
    throw;
    #endif
  }
}
//---------------------------------------------------------------------------
void __fastcall InvokeHelp(TWinControl * Control)
{
  assert(Control != NULL);

  HELPINFO HelpInfo;
  HelpInfo.cbSize = sizeof(HelpInfo);
  HelpInfo.iContextType = HELPINFO_WINDOW;
  HelpInfo.iCtrlId = 0;
  HelpInfo.hItemHandle = Control->Handle;
  HelpInfo.dwContextId = 0;
  HelpInfo.MousePos.x = 0;
  HelpInfo.MousePos.y = 0;
  SendMessage(Control->Handle, WM_HELP, NULL, reinterpret_cast<long>(&HelpInfo));
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
static void __fastcall FocusableLabelCanvas(TStaticText * StaticText,
  TControlCanvas ** ACanvas, TRect & R)
{
  TControlCanvas * Canvas = new TControlCanvas();
  try
  {
    Canvas->Control = StaticText;

    R = StaticText->ClientRect;

    AnsiString Caption = StaticText->Caption;
    bool AccelChar = false;
    if (StaticText->ShowAccelChar)
    {
      Caption = StripHotkey(Caption);
      AccelChar = (Caption != StaticText->Caption);
    }

    TSize TextSize = Canvas->TextExtent(Caption);
    assert(StaticText->BorderStyle == sbsNone); // not taken into account
    if (AccelChar)
    {
      TextSize.cy += 2;
    }

    R.Bottom = R.Top + TextSize.cy;
    if (StaticText->Alignment == taRightJustify)
    {
      R.Left = R.Right - TextSize.cx;
    }
    else
    {
      R.Right = R.Left + TextSize.cx;
    }
  }
  __finally
  {
    if (ACanvas == NULL)
    {
      delete Canvas;
    }
  }

  if (ACanvas != NULL)
  {
    *ACanvas = Canvas;
  }
}
//---------------------------------------------------------------------------
static void __fastcall FocusableLabelWindowProc(void * Data, TMessage & Message,
  bool & Clicked)
{
  Clicked = false;
  TStaticText * StaticText = static_cast<TStaticText *>(Data);
  if (Message.Msg == WM_LBUTTONDOWN)
  {
    StaticText->SetFocus();
    // in case the action takes long, make sure focus is shown immediatelly
    UpdateWindow(StaticText->Handle);
    Clicked = true;
    Message.Result = 1;
  }
  else if (Message.Msg == WM_RBUTTONDOWN)
  {
    StaticText->SetFocus();
    Message.Result = 1;
  }
  else if (Message.Msg == WM_CHAR)
  {
    if (reinterpret_cast<TWMChar &>(Message).CharCode == ' ')
    {
      Clicked = true;
      Message.Result = 1;
    }
    else
    {
      ControlWndProc(StaticText)(Message);
    }
  }
  else if (Message.Msg == CM_DIALOGCHAR)
  {
    if (StaticText->CanFocus() && StaticText->ShowAccelChar &&
        IsAccel(reinterpret_cast<TCMDialogChar &>(Message).CharCode, StaticText->Caption))
    {
      StaticText->SetFocus();
      // in case the action takes long, make sure focus is shown immediatelly
      UpdateWindow(StaticText->Handle);
      Clicked = true;
      Message.Result = 1;
    }
    else
    {
      ControlWndProc(StaticText)(Message);
    }
  }
  else
  {
    ControlWndProc(StaticText)(Message);
  }

  if (Message.Msg == WM_PAINT)
  {
    TRect R;
    TControlCanvas * Canvas;
    FocusableLabelCanvas(StaticText, &Canvas, R);
    try
    {
      if (StaticText->Focused())
      {
        Canvas->DrawFocusRect(R);
      }
      else if (!StaticText->Font->Style.Contains(fsUnderline))
      {
        Canvas->Pen->Style = psDot;
        Canvas->Brush->Style = bsClear;
        if (!StaticText->Enabled)
        {
          Canvas->Pen->Color = clBtnHighlight;
          Canvas->MoveTo(R.Left + 1 + 1, R.Bottom);
          Canvas->LineTo(R.Right + 1, R.Bottom);
          Canvas->Pen->Color = clGrayText;
        }
        Canvas->MoveTo(R.Left + 1, R.Bottom - 1);
        Canvas->LineTo(R.Right, R.Bottom - 1);
      }
    }
    __finally
    {
      delete Canvas;
    }
  }
  else if ((Message.Msg == WM_SETFOCUS) || (Message.Msg == WM_KILLFOCUS) ||
    (Message.Msg == CM_ENABLEDCHANGED))
  {
    StaticText->Invalidate();
  }
}
//---------------------------------------------------------------------------
static THintWindow * PersistentHintWindow = NULL;
static TControl * PersistentHintControl = NULL;
//---------------------------------------------------------------------------
void __fastcall CancelPersistentHint()
{
  if (PersistentHintWindow != NULL)
  {
    PersistentHintControl = NULL;
    SAFE_DESTROY(PersistentHintWindow);
  }
}
//---------------------------------------------------------------------------
void __fastcall ShowPersistentHint(TControl * Control, TPoint HintPos)
{
  CancelPersistentHint();

  THintInfo HintInfo;
  HintInfo.HintControl = Control;
  HintInfo.HintPos = HintPos;
  HintInfo.HintMaxWidth = Screen->Width;
  HintInfo.HintColor = Application->HintColor;
  HintInfo.HintStr = GetShortHint(Control->Hint);
  HintInfo.HintData = NULL;

  bool CanShow = true;
  if (Application->OnShowHint != NULL)
  {
    Application->OnShowHint(HintInfo.HintStr, CanShow, HintInfo);
  }

  if (CanShow)
  {
    PersistentHintControl = Control;

    PersistentHintWindow = new THintWindow(Application);
    PersistentHintWindow->BiDiMode = Control->BiDiMode;
    PersistentHintWindow->Color = HintInfo.HintColor;

    TRect HintWinRect;
    if (HintInfo.HintMaxWidth < Control->Width)
    {
      HintInfo.HintMaxWidth = Control->Width;
    }
    HintWinRect = PersistentHintWindow->CalcHintRect(
      HintInfo.HintMaxWidth, HintInfo.HintStr, HintInfo.HintData);
    OffsetRect(HintWinRect, HintInfo.HintPos.x, HintInfo.HintPos.y);
    // TODO: right align window placement for UseRightToLeftAlignment, see Forms.pas

    PersistentHintWindow->ActivateHintData(HintWinRect, HintInfo.HintStr, HintInfo.HintData);
  }
}
//---------------------------------------------------------------------------
static void __fastcall HintLabelWindowProc(void * Data, TMessage & Message)
{
  bool Clicked = false;
  bool Cancel = false;

  TStaticText * StaticText = static_cast<TStaticText *>(Data);
  if (Message.Msg == CM_HINTSHOW)
  {
    TCMHintShow & HintShow = reinterpret_cast<TCMHintShow &>(Message);
    if (PersistentHintControl == StaticText)
    {
      // do not allow standard hint when persistent is already shown
      HintShow.Result = 1;
    }
    else
    {
      HintShow.HintInfo->HideTimeout = 100000; // never
    }
  }
  else if (Message.Msg == CN_KEYDOWN)
  {
    if ((reinterpret_cast<TWMKey &>(Message).CharCode == VK_ESCAPE) &&
        (PersistentHintControl == StaticText))
    {
      CancelPersistentHint();
      StaticText->Invalidate();
      Message.Result = 1;
    }
    else
    {
      FocusableLabelWindowProc(Data, Message, Clicked);
    }
  }
  else
  {
    FocusableLabelWindowProc(Data, Message, Clicked);
  }

  if (Message.Msg == CM_CANCELMODE)
  {
    TCMCancelMode & CancelMessage = (TCMCancelMode&)Message;
    if ((CancelMessage.Sender != StaticText) &&
        (CancelMessage.Sender != PersistentHintWindow))
    {
      Cancel = true;
    }
  }

  if ((Message.Msg == WM_DESTROY) || (Message.Msg == WM_KILLFOCUS))
  {
    Cancel = true;
  }

  if (Cancel && (PersistentHintControl == StaticText))
  {
    CancelPersistentHint();
  }

  if (Clicked && (PersistentHintControl != StaticText))
  {
    TRect R;
    TPoint HintPos;

    FocusableLabelCanvas(StaticText, NULL, R);
    HintPos.y = R.Bottom - R.Top;
    HintPos.x = R.Left;

    ShowPersistentHint(StaticText, StaticText->ClientToScreen(HintPos));
  }
}
//---------------------------------------------------------------------------
void __fastcall HintLabel(TStaticText * StaticText, AnsiString Hint)
{
  StaticText->ParentFont = true;
  if (!Hint.IsEmpty())
  {
    StaticText->Hint = Hint;
  }
  StaticText->ShowHint = true;
  StaticText->Cursor = crHandPoint;

  TWndMethod WindowProc;
  ((TMethod*)&WindowProc)->Data = StaticText;
  ((TMethod*)&WindowProc)->Code = HintLabelWindowProc;
  StaticText->WindowProc = WindowProc;
}
//---------------------------------------------------------------------------
void __fastcall HintLabelRestore(TStaticText * StaticText)
{
  StaticText->WindowProc = ControlWndProc(StaticText);
  StaticText->ShowHint = false;
  StaticText->Cursor = crDefault;
}
//---------------------------------------------------------------------------
static void __fastcall LinkLabelClick(TStaticText * StaticText)
{
  if (StaticText->OnClick != NULL)
  {
    StaticText->OnClick(StaticText);
  }
  else
  {
    AnsiString Url = StaticText->Caption;
    if (!SameText(Url.SubString(1, 4), "http") && (Url.Pos("@") > 0))
    {
      Url = "mailto:" + Url;
    }
    OpenBrowser(Url);
  }
}
//---------------------------------------------------------------------------
static void __fastcall LinkLabelWindowProc(void * Data, TMessage & Message)
{
  bool Clicked = false;

  TStaticText * StaticText = static_cast<TStaticText *>(Data);
  if (Message.Msg == WM_CONTEXTMENU)
  {
    TWMContextMenu & ContextMenu = reinterpret_cast<TWMContextMenu &>(Message);

    if ((ContextMenu.Pos.x < 0) && (ContextMenu.Pos.y < 0))
    {
      TRect R;
      FocusableLabelCanvas(StaticText, NULL, R);
      TPoint P = StaticText->ClientToScreen(TPoint(R.Left, R.Bottom));
      ContextMenu.Pos.x = static_cast<short>(P.x);
      ContextMenu.Pos.y = static_cast<short>(P.y);
    }
  }
  else if (Message.Msg == WM_KEYDOWN)
  {
    TWMKey & Key = reinterpret_cast<TWMKey &>(Message);
    if ((GetKeyState(VK_CONTROL) < 0) && (Key.CharCode == 'C'))
    {
      CopyToClipboard(StaticText->Caption);
      Message.Result = 1;
    }
    else
    {
      FocusableLabelWindowProc(Data, Message, Clicked);
    }
  }

  FocusableLabelWindowProc(Data, Message, Clicked);

  if (Message.Msg == WM_DESTROY)
  {
    delete StaticText->PopupMenu;
    assert(StaticText->PopupMenu == NULL);
  }

  if (Clicked)
  {
    LinkLabelClick(StaticText);
  }
}
//---------------------------------------------------------------------------
static void __fastcall LinkLabelContextMenuClick(void * Data, TObject * Sender)
{
  TStaticText * StaticText = static_cast<TStaticText *>(Data);
  TMenuItem * MenuItem = dynamic_cast<TMenuItem *>(Sender);
  assert(MenuItem != NULL);

  if (MenuItem->Tag == 0)
  {
    LinkLabelClick(StaticText);
  }
  else
  {
    CopyToClipboard(StaticText->Caption);
  }
}
//---------------------------------------------------------------------------
void __fastcall LinkLabel(TStaticText * StaticText, AnsiString Url,
  TNotifyEvent OnEnter)
{
  StaticText->ParentFont = true;
  StaticText->Font->Style = StaticText->Font->Style << fsUnderline;
  StaticText->Font->Color = clBlue;
  StaticText->Cursor = crHandPoint;
  reinterpret_cast<TButton*>(StaticText)->OnEnter = OnEnter;
  if (!Url.IsEmpty())
  {
    StaticText->Caption = Url;
  }

  if (StaticText->OnClick == NULL)
  {
    assert(StaticText->PopupMenu == NULL);
    StaticText->PopupMenu = new TPopupMenu(StaticText);
    try
    {
      TNotifyEvent ContextMenuOnClick;
      ((TMethod*)&ContextMenuOnClick)->Data = StaticText;
      ((TMethod*)&ContextMenuOnClick)->Code = LinkLabelContextMenuClick;

      TMenuItem * Item;

      Item = new TMenuItem(StaticText->PopupMenu);
      Item->Caption = LoadStr(URL_LINK_OPEN);
      Item->Tag = 0;
      Item->ShortCut = ShortCut(' ', TShiftState());
      Item->OnClick = ContextMenuOnClick;
      StaticText->PopupMenu->Items->Add(Item);

      Item = new TMenuItem(StaticText->PopupMenu);
      Item->Caption = LoadStr(URL_LINK_COPY);
      Item->Tag = 1;
      Item->ShortCut = ShortCut('C', TShiftState() << ssCtrl);
      Item->OnClick = ContextMenuOnClick;
      StaticText->PopupMenu->Items->Add(Item);
    }
    catch(...)
    {
      delete StaticText->PopupMenu;
      assert(StaticText->PopupMenu == NULL);
      throw;
    }
  }

  TWndMethod WindowProc;
  ((TMethod*)&WindowProc)->Data = StaticText;
  ((TMethod*)&WindowProc)->Code = LinkLabelWindowProc;
  StaticText->WindowProc = WindowProc;
}
//---------------------------------------------------------------------------
TMonitor *  __fastcall FormMonitor(TCustomForm * Form)
{
  TMonitor * Result;
  if ((Application->MainForm != NULL) && (Application->MainForm != Form))
  {
    Result = Application->MainForm->Monitor;
  }
  else if (LastMonitor != NULL)
  {
    Result = LastMonitor;
  }
  else
  {
    int i = 0;
    while ((i < Screen->MonitorCount) && !Screen->Monitors[i]->Primary)
    {
      i++;
    }
    assert(Screen->Monitors[i]->Primary);
    Result = Screen->Monitors[i];
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall GetLastMonitor()
{
  if (LastMonitor != NULL)
  {
    return LastMonitor->MonitorNum;
  }
  else
  {
    return -1;
  }
}
//---------------------------------------------------------------------------
void __fastcall SetLastMonitor(int MonitorNum)
{
  if ((MonitorNum >= 0) && (MonitorNum < Screen->MonitorCount))
  {
    LastMonitor = Screen->Monitors[MonitorNum];
  }
  else
  {
    LastMonitor = NULL;
  }
}
//---------------------------------------------------------------------------
int __fastcall SafeShowModal(TForm * Form)
{
  // FIX: Due to some bug in Theme Manager, certain forms randomly
  // fails in call to ShowModal(), hence repeat the call until it succeeds.

  int Result = -1;
  int Retry = 0;

  do
  {
    try
    {
      Result = Form->ShowModal();
    }
    catch (EOSError & E)
    {
      if (E.Message == Sysconst_SUnkOSError)
      {
        ++Retry;
        if (Retry >= 10)
        {
          throw;
        }
        else
        {
          Form->Visible = false;
        }
      }
      else
      {
        throw;
      }
    }
  }
  while (Result < 0);

  return Result;
}
//---------------------------------------------------------------------------
static void __fastcall CreateHandles(TWinControl * Control)
{
  Control->HandleNeeded();

  for (int Index = 0; Index < Control->ControlCount; Index++)
  {
    TWinControl * ChildControl = dynamic_cast<TWinControl *>(Control->Controls[Index]);
    if (ChildControl != NULL)
    {
      CreateHandles(ChildControl);
    }
  }
}
//---------------------------------------------------------------------------
// FIX: Due to some bug in Theme Manager, certain forms randomly
// fails in call to ShowModal(), hence repeat the call until it succeeds.
TForm * __fastcall _SafeFormCreate(TMetaClass * FormClass, TComponent * Owner)
{
  // we do ignore owner atm, as we do not know how to set it,
  // but it should not cause any problems as we always manage memory
  // destruction ourselves
  assert(Owner == Application);
  USEDPARAM(Owner);

  int Retry = 0;
  TForm * Form;
  do
  {
    // if there is no main form yet, make this one main.
    // this, among other, makes other forms (dialogs invoked from this one),
    // be placed on the same monitor (otherwise all new forms get placed
    // on primary monitor)
    if (Application->MainForm == NULL)
    {
      Application->CreateForm(FormClass, &Form);
      assert(Application->MainForm == Form);
    }
    else
    {
      Form = dynamic_cast<TForm *>(Construct(FormClass, Application));
      assert(Form != NULL);
    }

    try
    {
      CreateHandles(Form);
    }
    catch (EOSError & E)
    {
      delete Form;
      Form = NULL;

      ++Retry;
      if ((E.Message != Sysconst_SUnkOSError) ||
          (Retry >= 10))
      {
        throw;
      }
    }
    catch(...)
    {
      delete Form;
    }
  }
  while (Form == NULL);

  return Form;
}
