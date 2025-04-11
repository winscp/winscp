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
#include <CustomWinConfiguration.h>
#include <CoreMain.h>

#include <Vcl.StdActns.hpp>
#include <PasswordEdit.hpp>
#include <FileCtrl.hpp>
#include <PathLabel.hpp>
#include <PasTools.hpp>
#include <StrUtils.hpp>
#include <Vcl.Imaging.pngimage.hpp>
#include <Math.hpp>
#include <TB2ExtItems.hpp>
#include <TBXExtItems.hpp>
#include <IEListView.hpp>
#include <WinApi.h>
#include <vssym32.h>
//---------------------------------------------------------------------------
const UnicodeString ContextSeparator(TraceInitStr(L"\x00BB"));
const UnicodeString LinkAppLabelMark(TraceInitStr(UnicodeString(L" ") + ContextSeparator));
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
void __fastcall FixListColumnWidth(TListView * TListView, int Index)
{
  if (Index < 0)
  {
    Index = TListView->Columns->Count + Index;
  }
  TListView->Column[Index]->Tag = 1;
}
//---------------------------------------------------------------------------
static int __fastcall GetColumnTextWidth(TListView * ListView, int ColumnPadding, const UnicodeString & Text)
{
  return
    ColumnPadding +
    ListView->Canvas->TextExtent(Text).Width;
}
//---------------------------------------------------------------------------
void __fastcall AutoSizeListColumnsWidth(TListView * ListView, int ColumnToShrinkIndex)
{
  // Preallocate handle to precreate columns, otherwise our changes may get
  // overwritten once the handle is created.
  // This should actually get called only once the handle are allocated.
  // Otherwise we end up recreating the handles,
  // what may cause a flicker of the currently focused window title.
  ListView->HandleNeeded();

  ListView->LockDrawing();

  try
  {
    int ColumnPadding = 2 * ScaleByTextHeightRunTime(ListView, 6);
    int ColumnShrinkMinWidth = ScaleByTextHeightRunTime(ListView, 67);

    int ResizableWidth = 0;
    int NonResizableWidth = 0;
    int LastResizable = -1;
    for (int Index = 0; Index < ListView->Columns->Count; Index++)
    {
      TListColumn * Column = ListView->Columns->Items[Index];

      int CaptionWidth = GetColumnTextWidth(ListView, ColumnPadding, Column->Caption);

      int OrigWidth = -1;
      bool NonResizable = (Column->Tag != 0);
      if (NonResizable) // optimization
      {
        OrigWidth = ListView_GetColumnWidth(ListView->Handle, Index);
      }

      int Width;
      // LVSCW_AUTOSIZE does not work for OwnerData list views
      if (!ListView->OwnerData)
      {
        ListView_SetColumnWidth(ListView->Handle, Index, LVSCW_AUTOSIZE);
        Width = ListView_GetColumnWidth(ListView->Handle, Index);
      }
      else
      {
        Width = 0;
        for (int ItemIndex = 0; ItemIndex < ListView->Items->Count; ItemIndex++)
        {
          TListItem * Item = ListView->Items->Item[ItemIndex];

          UnicodeString Text;
          if (Index == 0)
          {
            Text = Item->Caption;
          }
          // Particularly EditorListView3 on Preferences dialog does not have all subitems filled for internal editor
          else if (Index <= Item->SubItems->Count)
          {
            Text = Item->SubItems->Strings[Index - 1];
          }

          int TextWidth = GetColumnTextWidth(ListView, ColumnPadding, Text);
          if (TextWidth > Width)
          {
            Width = TextWidth;
          }
        }
      }
      Width = Max(Width, CaptionWidth);
      // Never shrink the non-resizable columns
      if (NonResizable && (Width < OrigWidth))
      {
        Width = OrigWidth;
      }
      Column->Width = Width;

      if (NonResizable)
      {
        NonResizableWidth += Width;
      }
      else
      {
        LastResizable = Index;
        ResizableWidth += Width;
      }
    }

    DebugAssert(LastResizable >= 0);

    int ClientWidth = ListView->ClientWidth;
    int RowCount = ListView->Items->Count;
    if ((ListView->VisibleRowCount < RowCount) &&
        (ListView->Width - ListView->ClientWidth < GetSystemMetricsForControl(ListView, SM_CXVSCROLL)))
    {
      ClientWidth -= GetSystemMetricsForControl(ListView, SM_CXVSCROLL);
    }

    if (DebugAlwaysTrue(NonResizableWidth < ClientWidth))
    {
      int Remaining = ClientWidth - NonResizableWidth;

      bool ProportionalResize = true;

      bool Shrinking = (Remaining < ResizableWidth);
      // If columns are too wide to fit and we have dedicated shrink column, shrink it
      if (Shrinking &&
          (ColumnToShrinkIndex >= 0))
      {
        TListColumn * ColumnToShrink = ListView->Columns->Items[ColumnToShrinkIndex];
        int ColumnToShrinkCaptionWidth = GetColumnTextWidth(ListView, ColumnPadding, ColumnToShrink->Caption);
        int ColumnToShrinkMinWidth = Max(ColumnShrinkMinWidth, ColumnToShrinkCaptionWidth);
        // This falls back to proprotional shrinking when the shrink column would fall below min width.
        // Question is whether we should not shrink to min width instead.
        if ((ResizableWidth - ColumnToShrink->Width) < (Remaining - ColumnToShrinkMinWidth))
        {
          int ColumnToShrinkWidth = Remaining - (ResizableWidth - ColumnToShrink->Width);
          ColumnToShrink->Width = ColumnToShrinkWidth;
          ProportionalResize = false;
        }
      }

      if (ProportionalResize)
      {
        for (int Index = 0; Index <= LastResizable; Index++)
        {
          TListColumn * Column = ListView->Columns->Items[Index];

          if (Column->Tag == 0)
          {
            int Width = ListView_GetColumnWidth(ListView->Handle, Index);
            int AutoWidth = Width;
            if (Index < LastResizable)
            {
              Width = (Remaining * Width) / ResizableWidth;
            }
            else
            {
              Width = Remaining;
            }

            int CaptionWidth = GetColumnTextWidth(ListView, ColumnPadding, Column->Caption);
            Width = Max(Width, CaptionWidth);
            Width = Max(Width, Min(ColumnShrinkMinWidth, AutoWidth));
            Column->Width = Width;
            Remaining -= Min(Width, Remaining);
          }
        }

        DebugAssert(Remaining == 0);
      }
    }
  }
  __finally
  {
    ListView->UnlockDrawing();
  }
}
//---------------------------------------------------------------------------
static void __fastcall SetParentColor(TControl * Control)
{
  TColor Color = clBtnFace;
  if (UseThemes)
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
      HTHEME Theme = OpenThemeData(NULL, L"tab");
      if (Theme != NULL)
      {
        COLORREF RGB;
        // XP with classic theme: Does not get past OpenThemeData, clBtnFace is exact color
        // XP with XP theme: not the exact color (probably same as clBtnFace), but close
        // Vista - ok
        // 2016 without desktop - ok
        // 7 with classic and high contrast themes: Do not get past OpenThemeData, clBtnFace is exact color
        // 7 with 7 and basic themes - ok
        // 10 with high contrast themes - ok (note the difference to 7 with high contract themes)
        // 10 - ok
        if (GetThemeColor(Theme, TABP_AEROWIZARDBODY, TIS_NORMAL, TMT_FILLCOLOR, &RGB) == S_OK)
        {
          Color = static_cast<TColor>(RGB);
        }
        CloseThemeData(Theme);
      }
    }
  }

  ((TEdit*)Control)->Color = Color;
}
//---------------------------------------------------------------------------
void __fastcall EnableControl(TControl * Control, bool Enable)
{
  if (Control->Enabled != Enable)
  {
    TWinControl * WinControl = dynamic_cast<TWinControl *>(Control);
    if ((WinControl != NULL) &&
        (WinControl->ControlCount > 0))
    {
      for (int Index = 0; Index < WinControl->ControlCount; Index++)
      {
        EnableControl(WinControl->Controls[Index], Enable);
      }
    }
    Control->Enabled = Enable;
  }

  if ((dynamic_cast<TCustomEdit *>(Control) != NULL) ||
      (dynamic_cast<TCustomComboBox *>(Control) != NULL) ||
      (dynamic_cast<TCustomListView *>(Control) != NULL) ||
      (dynamic_cast<TCustomTreeView *>(Control) != NULL))
  {
    if (Enable)
    {
      ((TEdit*)Control)->Color = clWindow;
    }
    else
    {
      // This does not work for list view with
      // LVS_EX_DOUBLEBUFFER (TCustomDirView).
      // It automatically gets gray background.
      // Though on Windows 7, the control has to be disabled
      // only after it is showing already (see TCustomScpExplorerForm::UpdateControls())
      ((TEdit*)Control)->Color = clBtnFace;
    }
  }
};
//---------------------------------------------------------------------------
void __fastcall DoReadOnlyControl(TControl * Control, bool ReadOnly, bool Color);
//---------------------------------------------------------------------------
void __fastcall ReadOnlyAndEnabledControl(TControl * Control, bool ReadOnly, bool Enabled)
{
  // Change color only in only one of EnableControl and DoReadOnlyControl to prevent flicker
  if (ReadOnly)
  {
    DebugAssert(dynamic_cast<TWinControl *>(Control)->ControlCount == 0);
    // As EnableControl, but with no color change
    Control->Enabled = Enabled;
    DoReadOnlyControl(Control, ReadOnly, true);
  }
  else
  {
    DoReadOnlyControl(Control, ReadOnly, false);
    EnableControl(Control, Enabled);
  }
}
//---------------------------------------------------------------------------
static void __fastcall ReadOnlyEditContextPopup(void * /*Data*/, TObject * Sender, const TPoint & MousePos, bool & Handled)
{
  TEdit * Edit = static_cast<TEdit *>(Sender);
  if (Edit->ReadOnly)
  {
    MenuPopup(Sender, MousePos, Handled);
  }
}
//---------------------------------------------------------------------------
class TPublicWinControl : public TWinControl
{
friend TWndMethod __fastcall ControlWndProc(TWinControl * Control);
friend void __fastcall InstallPathWordBreakProc(TWinControl * Control);
};
//---------------------------------------------------------------------------
TWndMethod __fastcall ControlWndProc(TWinControl * Control)
{
  TPublicWinControl * PublicWinControl = static_cast<TPublicWinControl *>(Control);
  return &PublicWinControl->WndProc;
}
//---------------------------------------------------------------------------
static void __fastcall ReadOnlyEditWindowProc(void * Data, TMessage & Message)
{
  TCustomEdit * Edit = static_cast<TCustomEdit *>(Data);
  TCustomStyleServices * AStyleServices;
  if ((Message.Msg == CN_CTLCOLORSTATIC) && Edit->ReadOnly && (AStyleServices = StyleServices(Edit))->Enabled)
  {
    // VCL_COPY Based on TCustomStaticText.CNCtlColorStatic

    // Pure Win32 alternative can be seen at:
    // https://stackoverflow.com/q/75759034/850848#75764544
    // (see my comment to the answer)

    HDC ControlDC = reinterpret_cast<HDC>(Message.WParam);
    HWND ControlHandle = reinterpret_cast<HWND>(Message.LParam);
    DebugAssert(ControlHandle == Edit->Handle);

    SetBkMode(ControlDC, TRANSPARENT);
    AStyleServices->DrawParentBackground(ControlHandle, ControlDC, NULL, false);

    Message.Result = reinterpret_cast<LRESULT>(GetStockObject(NULL_BRUSH));
  }
  else
  {
    ControlWndProc(Edit)(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall DoReadOnlyControl(TControl * Control, bool ReadOnly, bool Color)
{
  if (dynamic_cast<TCustomEdit *>(Control) != NULL)
  {
    TEdit * Edit = static_cast<TEdit *>(Control);
    Edit->ReadOnly = ReadOnly;
    TMemo * Memo = dynamic_cast<TMemo *>(Control);

    TWinControl * Parent = Edit->Parent;
    while (Parent != NULL)
    {
      // Not necessary, just to limit impact and conflicts
      if (dynamic_cast<TTabSheet *>(Parent) != NULL)
      {
        TWndMethod WindowProc = MakeMethod<TWndMethod>(Edit, ReadOnlyEditWindowProc);
        if ((Edit->WindowProc != WindowProc) && DebugAlwaysTrue(Edit->WindowProc == ControlWndProc(Edit)))
        {
          Edit->WindowProc = WindowProc;
        }
        break;
      }
      Parent = Parent->Parent;
    }

    if (ReadOnly)
    {
      if (Color)
      {
        SetParentColor(Control);
      }
      if (Memo != NULL)
      {
        // Is true by default and makes the control swallow not only
        // returns but also escapes.
        // See also MemoKeyDown
        Memo->WantReturns = false;
      }

      if ((Edit->PopupMenu == NULL) && (dynamic_cast<TPasswordEdit *>(Control) == NULL))
      {
        std::unique_ptr<TPopupMenu> PopupMenu(new TPopupMenu(Edit));

        TMenuItem * Item;
        Item = new TMenuItem(PopupMenu.get());
        PopupMenu->Items->Add(Item);
        Item->Action = new TEditCopy(Item);
        Item->Caption = LoadStr(EDIT_COPY);
        Item->ShortCut = ShortCut(L'C', TShiftState() << ssCtrl);
        Item = new TMenuItem(PopupMenu.get());
        PopupMenu->Items->Add(Item);
        Item->Action = new TEditSelectAll(Item);
        Item->Caption = LoadStr(EDIT_SELECT_ALL);
        Item->ShortCut = ShortCut(L'A', TShiftState() << ssCtrl);

        Edit->PopupMenu = PopupMenu.release();
        Edit->OnContextPopup = MakeMethod<TContextPopupEvent>(NULL, ReadOnlyEditContextPopup);
      }
    }
    else
    {
      if (Color)
      {
        Edit->Color = clWindow;
      }
      // not supported atm, we need to persist previous value of WantReturns
      DebugAssert(Memo == NULL);

      if ((Edit->PopupMenu != NULL) && (Edit->PopupMenu->Owner == Edit))
      {
        delete Edit->PopupMenu;
      }
    }
  }
  else if ((dynamic_cast<TCustomComboBox *>(Control) != NULL) ||
           (dynamic_cast<TCustomTreeView *>(Control) != NULL))
  {
    EnableControl(Control, !ReadOnly);
  }
  else
  {
    DebugFail();
  }
}
//---------------------------------------------------------------------------
void __fastcall ReadOnlyControl(TControl * Control, bool ReadOnly)
{
  DoReadOnlyControl(Control, ReadOnly, true);
}
//---------------------------------------------------------------------------
int CalculateCheckBoxWidth(TControl * Control, const UnicodeString & Caption)
{
  return
    ScaleByTextHeight(Control, 13 + 3 + 8) + // checkbox, padding and buffer
    GetParentForm(Control)->Canvas->TextWidth(StripHotkey(Caption));
}
//---------------------------------------------------------------------------
void AutoSizeCheckBox(TCheckBox * CheckBox)
{
  CheckBox->Width = CalculateCheckBoxWidth(CheckBox, CheckBox->Caption);
}
//---------------------------------------------------------------------------
// Some of MainFormLike code can now be obsolete, thanks to Application->OnGetMainFormHandle.
static TForm * MainLikeForm = NULL;
//---------------------------------------------------------------------------
TForm * __fastcall GetMainForm()
{
  TForm * Result;
  if (MainLikeForm != NULL)
  {
    Result = MainLikeForm;
  }
  else
  {
    Result = Application->MainForm;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall IsMainFormHidden()
{
  bool Result = false;
  TForm * MainForm = GetMainForm();
  if (MainForm != NULL)
  {
    Result =
      !MainForm->Visible ||
      (MainForm->Perform(WM_IS_HIDDEN, 0, 0) == 1);
  }
  // we do not expect this to return true when MainLikeForm is set
  DebugAssert(!Result || (MainLikeForm == NULL));
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall IsMainFormLike(TCustomForm * Form)
{
  return
    (GetMainForm() == Form) ||
    // this particularly happens if error occurs while main
    // window is being shown (e.g. non existent local directory when opening
    // explorer)
    IsMainFormHidden();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FormatMainFormCaption(const UnicodeString & Caption, const UnicodeString & SessionName)
{
  UnicodeString Suffix = AppName;
  if (!SessionName.IsEmpty())
  {
    Suffix = SessionName + TitleSeparator + Suffix;
  }
  UnicodeString Result = Caption;
  if (Result.IsEmpty())
  {
    Result = Suffix;
  }
  else
  {
    Suffix = TitleSeparator + Suffix;
    if (!EndsStr(Suffix, Result))
    {
      Result += Suffix;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FormatFormCaption(
  TCustomForm * Form, const UnicodeString & Caption, const UnicodeString & SessionName)
{
  UnicodeString Result = Caption;
  if (IsMainFormLike(Form))
  {
    Result = FormatMainFormCaption(Result, SessionName);
  }
  return Result;
}
//---------------------------------------------------------------------
class TPublicControl : public TControl
{
friend void __fastcall RealignControl(TControl * Control);
friend void __fastcall DoFormWindowProc(TCustomForm * Form, TWndMethod WndProc, TMessage & Message);
friend TCanvas * CreateControlCanvas(TControl * Control);
};
//---------------------------------------------------------------------
class TPublicForm : public TForm
{
friend void __fastcall ShowAsModal(TForm * Form, void *& Storage, bool BringToFront, bool TriggerModalStarted);
friend void __fastcall HideAsModal(TForm * Form, void *& Storage);
friend void __fastcall ShowFormNoActivate(TForm * Form);
};
//---------------------------------------------------------------------------
void __fastcall RealignControl(TControl * Control)
{
  TPublicControl * PublicControl = static_cast<TPublicControl *>(Control);
  PublicControl->RequestAlign();
}
//---------------------------------------------------------------------------
static Forms::TMonitor * LastMonitor = NULL;
//---------------------------------------------------------------------------
class TRescaleComponent : public TComponent
{
public:
  __fastcall TRescaleComponent(TRescaleEvent AOnRescale, TObject * AToken, bool AOwnsToken) :
    TComponent(NULL)
  {
    OnRescale = AOnRescale;
    Token = AToken;
    OwnsToken = AOwnsToken;
  }

  virtual __fastcall ~TRescaleComponent()
  {
    if (OwnsToken)
    {
      delete Token;
    }
  }

  TRescaleEvent OnRescale;
  TObject * Token;
  bool OwnsToken;
};
//---------------------------------------------------------------------------
void __fastcall SetRescaleFunction(
  TComponent * Component, TRescaleEvent OnRescale, TObject * Token, bool OwnsToken)
{
  TRescaleComponent * RescaleComponent = new TRescaleComponent(OnRescale, Token, OwnsToken);
  RescaleComponent->Name = TRescaleComponent::QualifiedClassName();
  Component->InsertComponent(RescaleComponent);
}
//---------------------------------------------------------------------------
static void __fastcall ChangeControlScale(TControl * Control)
{
  TCustomCombo * CustomCombo = dynamic_cast<TCustomCombo *>(Control);
  if (CustomCombo != NULL)
  {
    // WORKAROUND
    // Some combo boxes (e.g. path box on Copy dialog) visualize selection after rescaling, even when not focused
    if (!CustomCombo->Focused())
    {
      CustomCombo->SelLength = 0;
    }
  }

  for (int Index = 0; Index < Control->ComponentCount; Index++)
  {
    TComponent * Component = Control->Components[Index];

    TControl * ChildControl = dynamic_cast<TControl *>(Component);
    if (ChildControl != NULL)
    {
      ChangeControlScale(ChildControl);
    }

    TRescaleComponent * RescaleComponent =
      dynamic_cast<TRescaleComponent *>(Component->FindComponent(TRescaleComponent::QualifiedClassName()));
    if (RescaleComponent != NULL)
    {
      RescaleComponent->OnRescale(Component, RescaleComponent->Token);
    }
  }

  Control->Perform(CM_DPICHANGED, 0, 0);
}
//---------------------------------------------------------------------------
typedef std::pair<int, int> TRatio;
typedef std::map<TRatio, TRatio > TRatioMap;
//---------------------------------------------------------------------------
class TFormCustomizationComponent : public TComponent
{
public:
  __fastcall TFormCustomizationComponent() :
    TComponent(NULL)
  {
    WindowStateBeforeMimimize = wsNormal;
  }

  TWindowState WindowStateBeforeMimimize;
};
//---------------------------------------------------------------------------
static TFormCustomizationComponent * GetFormCustomizationComponent(TForm * Form)
{
  TFormCustomizationComponent * FormCustomizationComponent =
    dynamic_cast<TFormCustomizationComponent *>(Form->FindComponent(TFormCustomizationComponent::QualifiedClassName()));
  if (FormCustomizationComponent == NULL)
  {
    FormCustomizationComponent = new TFormCustomizationComponent();
    FormCustomizationComponent->Name = TFormCustomizationComponent::QualifiedClassName();
    Form->InsertComponent(FormCustomizationComponent);
  }
  return FormCustomizationComponent;
}
//---------------------------------------------------------------------------
static void __fastcall ChangeFormPixelsPerInch(TForm * Form)
{

  AppLogFmt(L"Scaling window %s", (Form->Caption));
  ChangeControlScale(Form);
}
//---------------------------------------------------------------------------
static void __fastcall FormShowingChanged(TForm * Form, TWndMethod WndProc, TMessage & Message)
{
  if (IsMainFormLike(Form))
  {
    if (Form->Showing)
    {
      if (Application->MainForm != Form)
      {
        MainLikeForm = Form;

        // When main form is hidden, no taskbar button for it is shown and
        // this window does not become main window, so there won't be no taskbar
        // icon created automatically (by VCL). So we force it manually here.
        // This particularly happen for all windows/dialogs of command-line
        // operations (e.g. /synchronize) after CreateScpExplorer() happens.

        // Also CM_SHOWINGCHANGED happens twice, and the WS_EX_APPWINDOW flag
        // from the first call is not preserved, re-applying on the second call too.

        // TODO: What about minimize to tray?

        int Style = GetWindowLong(Form->Handle, GWL_EXSTYLE);
        if (FLAGCLEAR(Style, WS_EX_APPWINDOW))
        {
          Style |= WS_EX_APPWINDOW;
          SetWindowLong(Form->Handle, GWL_EXSTYLE, Style);
        }
      }

      if (Form->Perform(WM_MANAGES_CAPTION, 0, 0) == 0)
      {
        Form->Caption = FormatFormCaption(Form, Form->Caption);
      }
      SendMessage(Form->Handle, WM_SETICON, ICON_BIG, reinterpret_cast<long>(Application->Icon->Handle));
    }
    else
    {
      if (MainLikeForm == Form)
      {
        // Do not bother with hiding the WS_EX_APPWINDOW flag
        // as the window is closing anyway.
        MainLikeForm = NULL;
      }
    }
  }

  // Part of following code (but actually not all, TODO), has to happen
  // for all windows when VCL main window is hidden (particularly the last branch).
  // This is different from above brach, that should happen only for top-level visible window.
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
      // would actually always be poScreenCenter, see _SafeFormCreate
      if ((Form->Position == poMainFormCenter) ||
          (Form->Position == poOwnerFormCenter) ||
          (Form->Position == poScreenCenter))
      {
        // this would typically be a standalone message box (e.g. /UninstallCleanup or /Update)

        // If DPI changes (as the form moves to a monitor with a non-system DPI),
        // we have to re-center as the form size changed too.
        int PixelsPerInch;
        do
        {
          PixelsPerInch = Form->PixelsPerInch;
          // taken from TCustomForm::SetWindowToMonitor
          Form->SetBounds(LastMonitor->Left + ((LastMonitor->Width - Form->Width) / 2),
            LastMonitor->Top + ((LastMonitor->Height - Form->Height) / 2),
             Form->Width, Form->Height);
          Form->Position = poDesigned;
        }
        while (PixelsPerInch != Form->PixelsPerInch);
      }
      else if ((Form->Position != poDesigned) &&
               (Form->Position != poDefaultPosOnly))
      {
        // we do not expect any other positioning
        DebugFail();
      }
    }
    // otherwise it would not get centered
    else if ((Form->Position == poMainFormCenter) ||
             (Form->Position == poOwnerFormCenter))
    {
      Form->Position = poScreenCenter;
    }
  }

  bool WasFormCenter =
    (Form->Position == poMainFormCenter) ||
    (Form->Position == poOwnerFormCenter);
  WndProc(Message);
  // Make sure dialogs are shown on-screen even if center of the main window
  // is off-screen. Occurs e.g. if you move the main window so that
  // only window title is visible above taksbar.
  if (Form->Showing && WasFormCenter && (Form->Position == poDesigned))
  {
    TRect Rect;
    // Reading Form.Left/Form.Top instead here does not work, likely due to some
    // bug, when querying TProgressForm opened from TEditorForm (reloading remote file)
    GetWindowRect(Form->Handle, &Rect);

    int Left = Rect.Left;
    int Top = Rect.Top;
    TRect WorkArea = Form->Monitor->WorkareaRect;

    if (Left + Rect.Width() > WorkArea.Right)
    {
      Left = WorkArea.Right - Rect.Width();
    }
    if (Left < WorkArea.Left)
    {
      Left = WorkArea.Left;
    }
    if (Top + Rect.Height() > WorkArea.Bottom)
    {
      Top = WorkArea.Bottom - Rect.Height();
    }
    if (Top < WorkArea.Top)
    {
      Top = WorkArea.Top;
    }
    if ((Left != Rect.Left) ||
        (Top != Rect.Top))
    {
      SetWindowPos(Form->Handle, 0, Left, Top, Rect.Width(), Rect.Height(),
        SWP_NOZORDER + SWP_NOACTIVATE);
    }
  }
}
//---------------------------------------------------------------------------
TWindowState GetWindowStateBeforeMimimize(TForm * Form)
{
  return GetFormCustomizationComponent(Form)->WindowStateBeforeMimimize;
}
//---------------------------------------------------------------------------
static TCustomForm * WindowPrintForm = NULL;
static DWORD WindowPrintPrevClick = 0;
static unsigned int WindowPrintClickCount = 0;
//---------------------------------------------------------------------------
void __fastcall CountClicksForWindowPrint(TForm * Form)
{
  if ((WinConfiguration != NULL) && WinConfiguration->AllowWindowPrint)
  {
    DWORD Tick = GetTickCount();
    if (WindowPrintForm != Form)
    {
      WindowPrintForm = Form;
      WindowPrintClickCount = 0;
    }
    if (WindowPrintPrevClick < Tick - 500)
    {
      WindowPrintClickCount = 0;
    }
    WindowPrintClickCount++;
    WindowPrintPrevClick = Tick;
    if (WindowPrintClickCount == 3)
    {
      WindowPrintClickCount = 0;

      TInstantOperationVisualizer Visualizer;

      // get the device context of the screen
      HDC ScreenDC = CreateDC(L"DISPLAY", NULL, NULL, NULL);
      // and a device context to put it in
      HDC MemoryDC = CreateCompatibleDC(ScreenDC);

      try
      {
        bool Sizable = (Form->BorderStyle == bsSizeable);
        int Frame = GetSystemMetrics(Sizable ? SM_CXSIZEFRAME : SM_CXFIXEDFRAME) - 1;
        int Width = Form->Width - 2*Frame;
        int Height = Form->Height - Frame;

        // maybe worth checking these are positive values
        HBITMAP Bitmap = CreateCompatibleBitmap(ScreenDC, Width, Height);
        try
        {
          // get a new bitmap
          HBITMAP OldBitmap = static_cast<HBITMAP>(SelectObject(MemoryDC, Bitmap));

          BitBlt(MemoryDC, 0, 0, Width, Height, ScreenDC, Form->Left + Frame, Form->Top, SRCCOPY);
          Bitmap = static_cast<HBITMAP>(SelectObject(MemoryDC, OldBitmap));

          OpenClipboard(NULL);
          try
          {
            EmptyClipboard();
            SetClipboardData(CF_BITMAP, Bitmap);
          }
          __finally
          {
            CloseClipboard();
          }
        }
        __finally
        {
          DeleteObject(Bitmap);
        }
      }
      __finally
      {
        DeleteDC(MemoryDC);
        DeleteDC(ScreenDC);
      }
    }
  }
}
//---------------------------------------------------------------------------
struct TWMDpiChangedData
{
  TMonitorDpiChangedEvent OnAfterMonitorDpiChanged;
};
//---------------------------------------------------------------------------
static void __fastcall AfterMonitorDpiChanged(void * AData, TObject * Sender, int OldDPI, int NewDPI)
{
  TWMDpiChangedData * Data = static_cast<TWMDpiChangedData *>(AData);
  ChangeFormPixelsPerInch(DebugNotNull(dynamic_cast<TForm *>(Sender)));
  if (Data->OnAfterMonitorDpiChanged != NULL)
  {
    Data->OnAfterMonitorDpiChanged(Sender, OldDPI, NewDPI);
  }
}
//---------------------------------------------------------------------------
static void __fastcall FormWindowProc(void * Data, TMessage & Message)
{
  TForm * AForm = static_cast<TForm *>(Data);
  DebugAssert(AForm != NULL);
  TWndMethod WndProc = ControlWndProc(AForm);
  if (Message.Msg == WM_SYSCOMMAND)
  {
    if (Message.WParam == SC_CONTEXTHELP)
    {
      FormHelp(AForm);
      Message.Result = 1;
    }
    else if (Message.WParam == SC_MINIMIZE)
    {
      GetFormCustomizationComponent(AForm)->WindowStateBeforeMimimize = AForm->WindowState;
      WndProc(Message);
    }
    else
    {
      WndProc(Message);
    }
  }
  else if (Message.Msg == CM_SHOWINGCHANGED)
  {
    FormShowingChanged(AForm, WndProc, Message);
  }
  else if (Message.Msg == WM_SETICON)
  {
    // WORKAROUND: Swallow VCL attempt to clear the icon from TCustomForm.WMDestroy.
    // The clearing still happens to be visualised before the form is hidden.
    // On resizable forms, the icon gets replaced by the default application icon,
    // on the other forms, the icon disappears and window caption is shifted left.
    if (Message.LParam != 0)
    {
      WndProc(Message);
    }
  }
  else if (Message.Msg == WM_DPICHANGED)
  {
    TWMDpiChangedData WMDpiChangedData;
    WMDpiChangedData.OnAfterMonitorDpiChanged = AForm->OnAfterMonitorDpiChanged;
    AForm->OnAfterMonitorDpiChanged = MakeMethod<TMonitorDpiChangedEvent>(&WMDpiChangedData, AfterMonitorDpiChanged);
    try
    {
      WndProc(Message);
    }
    __finally
    {
      AForm->OnAfterMonitorDpiChanged = WMDpiChangedData.OnAfterMonitorDpiChanged;
    }
  }
  else if ((Message.Msg == WM_LBUTTONDOWN) || (Message.Msg == WM_LBUTTONDBLCLK))
  {
    CountClicksForWindowPrint(AForm);
    WndProc(Message);
  }
  else
  {
    WndProc(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall InitializeSystemSettings()
{
}
//---------------------------------------------------------------------------
void __fastcall FinalizeSystemSettings()
{
}
//---------------------------------------------------------------------------
#ifdef _DEBUG
void __fastcall VerifyControl(TControl * Control)
{
  // If at this time the control has allocated persistence data that are used
  // for delayed handle recreation, we are at potential risk, as the future
  // de-persistence may overwrite meanwhile changed data.
  // For instance it may happen with list view that DPI scaling gets lost.
  // This for example happens when the list view has both design time
  // ReadOnly = true and some items set. We cannot usually explicitly
  // check for the presence of items as while the listview does not have
  // a handle allocated, item count querying does not work
  // (see also a check below)
  DebugAssert(!ControlHasRecreationPersistenceData(Control));

  TCustomListView * ListView = dynamic_cast<TCustomListView *>(Control);
  if (ListView != NULL)
  {
    // As of now the HandleAllocated check is pointless as
    // ListView->Items->Count returns 0 when the handle is not allocated yet.
    // But we want to know if the implementation ever changes to allocate the handle
    // on the call. Because we do not want to allocate a handle here as
    // that would change the debug mode behavior from release behavior,
    // possibly hiding away some problems.
    DebugAssert(!ListView->HandleAllocated() || (ListView->Items->Count == 0));
  }
}
#endif
//---------------------------------------------------------------------------
void __fastcall ApplySystemSettingsOnControl(TControl * Control)
{
  #ifdef _DEBUG
  VerifyControl(Control);
  #endif

  TCustomListView * ListView = dynamic_cast<TCustomListView *>(Control);
  TCustomNortonLikeListView * NortonLikeListView = dynamic_cast<TCustomNortonLikeListView *>(Control);
  // For NortonLikeListView, this is (somewhat) handled in the TCustomListViewColProperties
  if ((ListView != NULL) && (NortonLikeListView == NULL))
  {
    TListView * PublicListView = reinterpret_cast<TListView *>(ListView);

    for (int Index = 0; Index < PublicListView->Columns->Count; Index++)
    {
      TListColumn * Column = ListView->Column[Index];
      Column->MaxWidth = ScaleByTextHeight(ListView, Column->MaxWidth);
      Column->MinWidth = ScaleByTextHeight(ListView, Column->MinWidth);
    }
  }

  // WORKAROUND for lack of public API for mimicking Explorer-style mouse selection
  // See https://stackoverflow.com/q/15750842/850848
  if (NortonLikeListView != NULL)
  {
    // It should not be a problem to call the LVM_QUERYINTERFACE
    // on earlier versions of Windows. It should be noop.
    if (IsWin7())
    {
      IListView_Win7 * ListViewIntf = NULL;
      SendMessage(NortonLikeListView->Handle, LVM_QUERYINTERFACE, reinterpret_cast<WPARAM>(&IID_IListView_Win7), reinterpret_cast<LPARAM>(&ListViewIntf));
      if (ListViewIntf != NULL)
      {
        ListViewIntf->SetSelectionFlags(1, 1);
        ListViewIntf->Release();
      }
      else
      {
        DebugAssert(IsWine());
      }
    }
  }

  TWinControl * WinControl = dynamic_cast<TWinControl *>(Control);
  if (WinControl != NULL)
  {
    for (int Index = 0; Index < WinControl->ControlCount; Index++)
    {
      ApplySystemSettingsOnControl(WinControl->Controls[Index]);
    }
  }
}
//---------------------------------------------------------------------------
// Settings that must be set as soon as possible.
void __fastcall UseSystemSettingsPre(TForm * Control)
{
  LocalSystemSettings(Control);

  TWndMethod WindowProc;
  ((TMethod*)&WindowProc)->Data = Control;
  ((TMethod*)&WindowProc)->Code = FormWindowProc;
  Control->WindowProc = WindowProc;

  if (Control->HelpKeyword.IsEmpty())
  {
    // temporary help keyword to enable F1 key in all forms
    Control->HelpKeyword = L"start";
  }

  ApplySystemSettingsOnControl(Control);
};
//---------------------------------------------------------------------------
static void FlipAnchors(TControl * Control)
{
  // WORKAROUND VCL flips the Align, but not the Anchors
  TAnchors Anchors = Control->Anchors;
  if (Anchors.Contains(akLeft) != Anchors.Contains(akRight))
  {
    if (Anchors.Contains(akLeft))
    {
      Anchors << akRight;
      Anchors >> akLeft;
    }
    else
    {
      Anchors << akLeft;
      Anchors >> akRight;
    }
  }
  Control->Anchors = Anchors;

  TWinControl * WinControl = dynamic_cast<TWinControl *>(Control);
  if (WinControl != NULL)
  {
    for (int Index = 0; Index < WinControl->ControlCount; Index++)
    {
      FlipAnchors(WinControl->Controls[Index]);
    }
  }
}
//---------------------------------------------------------------------------
// Settings that must be set only after whole form is constructed
void __fastcall UseSystemSettingsPost(TForm * Control)
{
  // When showing an early error message
  if (WinConfiguration != NULL)
  {
    UnicodeString FlipStr = LoadStr(FLIP_CHILDREN);
    int FlipChildrenFlag =
      AdjustLocaleFlag(FlipStr, WinConfiguration->FlipChildrenOverride, false, true, false);
    if (static_cast<bool>(FlipChildrenFlag))
    {
      Control->FlipChildren(true);

      FlipAnchors(Control);
    }
  }
  ResetSystemSettings(Control);
};
//---------------------------------------------------------------------------
void __fastcall UseSystemSettings(TForm * Control)
{
  UseSystemSettingsPre(Control);
  UseSystemSettingsPost(Control);
};
//---------------------------------------------------------------------------
void __fastcall ResetSystemSettings(TForm * /*Control*/)
{
  // noop
}
//---------------------------------------------------------------------------
struct TShowAsModalStorage
{
  bool TriggerModalFinished;
  void * FocusWindowList;
  HWND FocusActiveWindow;
  TFocusState FocusState;
  TCursor SaveCursor;
  int SaveCount;
};
//---------------------------------------------------------------------------
void __fastcall ShowAsModal(TForm * Form, void *& Storage, bool BringToFront, bool TriggerModalStarted)
{
  SetCorrectFormParent(Form);
  CancelDrag();
  if (GetCapture() != 0) SendMessage(GetCapture(), WM_CANCELMODE, 0, 0);
  ReleaseCapture();
  if (TriggerModalStarted)
  {
    Application->ModalStarted();
  }
  (static_cast<TPublicForm*>(Form))->FFormState << fsModal;

  TShowAsModalStorage * AStorage = new TShowAsModalStorage;

  AStorage->TriggerModalFinished = TriggerModalStarted;
  AStorage->FocusActiveWindow = GetActiveWindow();
  AStorage->FocusState = SaveFocusState();
  Screen->SaveFocusedList->Insert(0, Screen->FocusedForm);
  Screen->FocusedForm = Form;
  AStorage->SaveCursor = Screen->Cursor;
  // This is particularly used when displaing progress window
  // while downloading via temporary folder
  // (the mouse cursor is yet hidden at this point).
  // While the code was added to workaround the above problem,
  // it is taken from TCustomForm.ShowModal and
  // should have been here ever since.
  Screen->Cursor = crDefault;
  AStorage->SaveCount = Screen->CursorCount;
  AStorage->FocusWindowList = DisableTaskWindows(0);

  // VCLCOPY (TCustomForm::Show)
  Form->Visible = true;
  if (BringToFront)
  {
    Form->BringToFront();
  }

  SendMessage(Form->Handle, CM_ACTIVATE, 0, 0);

  Storage = AStorage;
}
//---------------------------------------------------------------------------
void __fastcall HideAsModal(TForm * Form, void *& Storage)
{
  DebugAssert((static_cast<TPublicForm*>(Form))->FFormState.Contains(fsModal));
  TShowAsModalStorage * AStorage = static_cast<TShowAsModalStorage *>(Storage);
  Storage = NULL;

  SendMessage(Form->Handle, CM_DEACTIVATE, 0, 0);
  if (GetActiveWindow() != Form->Handle)
  {
    AStorage->FocusActiveWindow = 0;
  }
  Form->Hide();

  if (Screen->CursorCount == AStorage->SaveCount)
  {
    Screen->Cursor = AStorage->SaveCursor;
  }
  else
  {
    Screen->Cursor = crDefault;
  }

  EnableTaskWindows(AStorage->FocusWindowList);

  if (Screen->SaveFocusedList->Count > 0)
  {
    Screen->FocusedForm = static_cast<TCustomForm *>(Screen->SaveFocusedList->First());
    Screen->SaveFocusedList->Remove(Screen->FocusedForm);
  }
  else
  {
    Screen->FocusedForm = NULL;
  }

  if (AStorage->FocusActiveWindow != 0)
  {
    SetActiveWindow(AStorage->FocusActiveWindow);
  }

  RestoreFocusState(AStorage->FocusState);

  (static_cast<TPublicForm*>(Form))->FFormState >> fsModal;

  if (AStorage->TriggerModalFinished)
  {
    Application->ModalFinished();
  }

  delete AStorage;
}
//---------------------------------------------------------------------------
bool __fastcall ReleaseAsModal(TForm * Form, void *& Storage)
{
  bool Result = (Storage != NULL);
  if (Result)
  {
    HideAsModal(Form, Storage);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall SelectDirectory(UnicodeString & Path, const UnicodeString Prompt,
  bool PreserveFileName)
{
  bool Result;
  unsigned int ErrorMode;
  ErrorMode = SetErrorMode(SEM_NOOPENFILEERRORBOX | SEM_FAILCRITICALERRORS);

  try
  {
    UnicodeString Directory;
    UnicodeString FileName;
    // We do not have any real use for the PreserveFileName
    if (!PreserveFileName || DirectoryExists(ApiPath(Path)))
    {
      Directory = Path;
    }
    else
    {
      Directory = ExtractFilePath(Path);
      FileName = ExtractFileName(Path);
    }
    TSelectDirExtOpts Opts = TSelectDirExtOpts() << sdNewUI;
    Result = SelectDirectory(Prompt, EmptyStr, Directory, Opts);
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
void SelectDirectoryForEdit(THistoryComboBox * Edit)
{
  UnicodeString OriginalDirectory = ExpandEnvironmentVariables(Edit->Text);
  UnicodeString Directory = OriginalDirectory;
  if (SelectDirectory(Directory, LoadStr(SELECT_LOCAL_DIRECTORY), true) &&
      !SamePaths(OriginalDirectory, Directory))
  {
    Edit->Text = Directory;
    if (Edit->OnChange != NULL)
    {
      Edit->OnChange(Edit);
    }
  }
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
void __fastcall ComboAutoSwitchInitialize(TComboBox * ComboBox)
{
  int PrevIndex = ComboBox->ItemIndex;
  ComboBox->Items->BeginUpdate();
  try
  {
    ComboBox->Clear();
    ComboBox->Items->Add(LoadStr(AUTO_SWITCH_AUTO));
    ComboBox->Items->Add(LoadStr(AUTO_SWITCH_OFF));
    ComboBox->Items->Add(LoadStr(AUTO_SWITCH_ON));
  }
  __finally
  {
    ComboBox->Items->EndUpdate();
  }
  DebugAssert(PrevIndex < ComboBox->Items->Count);
  ComboBox->ItemIndex = PrevIndex;
}
//---------------------------------------------------------------------------
void __fastcall ComboAutoSwitchLoad(TComboBox * ComboBox, TAutoSwitch Value)
{
  ComboBox->ItemIndex = 2 - Value;
  if (ComboBox->ItemIndex < 0)
  {
    ComboBox->ItemIndex = 0;
  }
}
//---------------------------------------------------------------------------
TAutoSwitch __fastcall ComboAutoSwitchSave(TComboBox * ComboBox)
{
  return (TAutoSwitch)(2 - ComboBox->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall CheckBoxAutoSwitchLoad(TCheckBox * CheckBox, TAutoSwitch Value)
{
  switch (Value)
  {
    case asOn:
      CheckBox->State = cbChecked;
      break;
    case asOff:
      CheckBox->State = cbUnchecked;
      break;
    default:
      CheckBox->State = cbGrayed;
      break;
  }
}
//---------------------------------------------------------------------------
TAutoSwitch __fastcall CheckBoxAutoSwitchSave(TCheckBox * CheckBox)
{
  switch (CheckBox->State)
  {
    case cbChecked:
      return asOn;
    case cbUnchecked:
      return asOff;
    default:
      return asAuto;
  }
}
//---------------------------------------------------------------------------
static const wchar_t PathWordDelimiters[] = L"\\/ ;,.\r\n=";
//---------------------------------------------------------------------------
static bool IsPathWordDelimiter(wchar_t Ch)
{
  return (wcschr(PathWordDelimiters, Ch) != NULL);
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
int CALLBACK PathWordBreakProc(wchar_t * Ch, int Current, int Len, int Code)
{
  int Result;
  UnicodeString ACh(Ch, Len);
  if (Code == WB_ISDELIMITER)
  {
    // we return negacy of what WinAPI docs says
    Result = !IsPathWordDelimiter(ACh[Current + 1]);
  }
  else if (Code == WB_LEFT)
  {
    // skip consecutive delimiters
    while ((Current > 0) &&
           IsPathWordDelimiter(ACh[Current]))
    {
      Current--;
    }
    Result = ACh.SubString(1, Current - 1).LastDelimiter(PathWordDelimiters);
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
      const wchar_t * P = wcspbrk(ACh.c_str() + Current - 1, PathWordDelimiters);
      if (P == NULL)
      {
        Result = Len;
      }
      else
      {
        Result = P - ACh.c_str() + 1;
        // skip consecutive delimiters
        while ((Result < Len) &&
               IsPathWordDelimiter(ACh[Result + 1]))
        {
          Result++;
        }
      }
    }
  }
  else
  {
    DebugFail();
    Result = 0;
  }
  return Result;
}
//---------------------------------------------------------------------------
class TPathWordBreakProcComponent : public TComponent
{
public:
  __fastcall TPathWordBreakProcComponent() :
    TComponent(NULL)
  {
  }

  void __fastcall PathWordBreakEditWindowProc(TMessage & Message);

  TWinControl * WinControl;
  TWndMethod PrevWindowProc;
};
//---------------------------------------------------------------------------
void __fastcall TPathWordBreakProcComponent::PathWordBreakEditWindowProc(TMessage & Message)
{
  bool Handled = false;
  if (Message.Msg == WM_CHAR)
  {
    // Ctrl+Backspace
    // Ctrl+Backspace is handled in WM_CHAR as any other char,
    // so we have to swallow it here to prevent it getting inserted to the text
    TWMChar & CharMessage = *reinterpret_cast<TWMChar *>(&Message);
    if (CharMessage.CharCode == '\x7F')
    {
      TCustomEdit * Edit = dynamic_cast<TCustomEdit *>(WinControl);
      TCustomComboBox * ComboBox = dynamic_cast<TCustomComboBox *>(WinControl);

      if (((Edit != NULL) && (Edit->SelLength == 0)) ||
          ((ComboBox != NULL) && (ComboBox->SelLength == 0)))
      {
        // See TCustomMaskEdit.SetCursor
        TKeyboardState KeyState;
        GetKeyboardState(KeyState);
        TKeyboardState NewKeyState;
        memset(NewKeyState, 0, sizeof(NewKeyState));
        NewKeyState[VK_CONTROL] = 0x81;
        NewKeyState[VK_SHIFT] = 0x81;
        SetKeyboardState(NewKeyState);

        SendMessage(WinControl->Handle, WM_KEYDOWN, VK_LEFT, 1);
        NewKeyState[VK_SHIFT] = 0;
        NewKeyState[VK_CONTROL] = 0;
        SetKeyboardState(NewKeyState);

        SendMessage(WinControl->Handle, WM_KEYDOWN, VK_DELETE, 1);
        SetKeyboardState(KeyState);
      }
      Message.Result = 1;
      Handled = true;
    }
  }

  if (!Handled)
  {
    if (PrevWindowProc != NULL)
    {
      PrevWindowProc(Message);
    }
    else
    {
      ControlWndProc(WinControl)(Message);
    }
  }
}
//---------------------------------------------------------------------------
class TPublicCustomCombo : public TCustomCombo
{
friend void __fastcall InstallPathWordBreakProc(TWinControl * Control);
};
//---------------------------------------------------------------------------
void __fastcall InstallPathWordBreakProc(TWinControl * Control)
{
  // Since we are setting Application->ModalPopupMode = pmAuto,
  // this has to be called from OnShow, not from constructor anymore,
  // to have any effect

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

  TPathWordBreakProcComponent * PathWordBreakProcComponent = new TPathWordBreakProcComponent();
  PathWordBreakProcComponent->Name = TPathWordBreakProcComponent::QualifiedClassName();
  Control->InsertComponent(PathWordBreakProcComponent);
  PathWordBreakProcComponent->WinControl = Control;
  // Have to remember the proc because of TTBEditItemViewer.EditWndProc
  PathWordBreakProcComponent->PrevWindowProc =
    // Test is probably redundant, it's there to limit impact of the change.
    ((Control->WindowProc != ControlWndProc(Control)) ? Control->WindowProc : TWndMethod());

  Control->WindowProc = PathWordBreakProcComponent->PathWordBreakEditWindowProc;
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
        DebugAssert(ControlsOrder[Index]->Parent == CommonParent);
        if ((Index == 0) || (Top > ControlsOrder[Index]->Top))
        {
          Top = ControlsOrder[Index]->Top;
        }
      }

      for (int Index = 0; Index < Count; Index++)
      {
        TControl * Control = ControlsOrder[Index];
        Control->Top = Top;
        if (((Control->Align == alTop) || (Control->Align == alBottom)) ||
            ((Index == Count - 1) || (ControlsOrder[Index + 1]->Align == alBottom)))
        {
          Top += Control->Height;
        }
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
        DebugAssert(ControlsOrder[Index]->Parent == CommonParent);
        if ((Index == 0) || (Left > ControlsOrder[Index]->Left))
        {
          Left = ControlsOrder[Index]->Left;
        }
      }

      for (int Index = 0; Index < Count; Index++)
      {
        TControl * Control = ControlsOrder[Index];
        Control->Left = Left;
        if (((Control->Align == alLeft) || (Control->Align == alRight)) ||
            ((Index == Count - 1) || (ControlsOrder[Index + 1]->Align == alRight)))
        {
          Left += Control->Width;
        }
        // vertical alignment has priority, so alBottom-aligned controls start
        // at the very left, even if there are any alLeft/alRight controls.
        // for the reason this code is not necessary in SetVerticalControlsOrder.
        // we could exit the loop as well here.
        if ((Index == Count - 1) || (ControlsOrder[Index + 1]->Align == alBottom))
        {
          Left = 0;
        }
      }
    }
    __finally
    {
      CommonParent->EnableAlign();
    }
  }
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
  DebugAssert(Form->Monitor != NULL);
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
      X = (Form->Monitor->Width - Bounds.Width()) / 2;
      Y = (Form->Monitor->Height - Bounds.Height()) / 2;
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
  // This has to be called only after DoFormWindowProc(CM_SHOWINGCHANGED),
  // so that a correct monitor is considered.
  // Note that we cannot use LastMonitor(), as ResizeForm is also called from
  // TConsoleDialog::DoAdjustWindow, where we need to use the actual monitor
  // (in case user moves the console window to a different monitor,
  // than where a main window is [no matter how unlikely that is])
  TRect WorkareaRect = Form->Monitor->WorkareaRect;
  if (Height > WorkareaRect.Height())
  {
    Height = WorkareaRect.Height();
  }
  if (Width > WorkareaRect.Width())
  {
    Width = WorkareaRect.Width();
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
  if (Top + Height > WorkareaRect.Bottom)
  {
    Top = WorkareaRect.Bottom - Height;
  }
  if (Left + Width >= WorkareaRect.Right)
  {
    Left = WorkareaRect.Right - Width;
  }
  // WorkareaRect.Left is not 0, when secondary monitor is placed left of primary one.
  // Similarly for WorkareaRect.Top.
  if (Top < WorkareaRect.Top)
  {
    Top = WorkareaRect.Top;
  }
  if (Left < WorkareaRect.Left)
  {
    Left = WorkareaRect.Left;
  }
  Form->SetBounds(Left, Top, Width, Height);
  Bounds = Form->BoundsRect;
  // due to constraints, form can remain larger, make sure it is centered although
  Left = Bounds.Left + ((Width - Bounds.Width()) / 2);
  Top = Bounds.Top + ((Height - Bounds.Height()) / 2);
  Form->SetBounds(Left, Top, Width, Height);
}
//---------------------------------------------------------------------------
TComponent * __fastcall GetFormOwner()
{
  if (Screen->ActiveForm != NULL)
  {
    return Screen->ActiveForm;
  }
  else
  {
    return Application;
  }
}
//---------------------------------------------------------------------------
void __fastcall SetCorrectFormParent(TForm * /*Form*/)
{
  // noop
  // remove
}
//---------------------------------------------------------------------------
void __fastcall InvokeHelp(TWinControl * Control)
{
  DebugAssert(Control != NULL);

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
  TCanvas ** ACanvas, TRect & R)
{
  TCanvas * Canvas = CreateControlCanvas(StaticText);
  try
  {
    R = StaticText->ClientRect;

    TSize TextSize;
    if (StaticText->AutoSize)
    {
      // We possibly could use the same code as in !AutoSize branch,
      // keeping this to avoid problems in existing code
      UnicodeString Caption = StaticText->Caption;
      bool AccelChar = false;
      if (StaticText->ShowAccelChar)
      {
        Caption = StripHotkey(Caption);
        AccelChar = (Caption != StaticText->Caption);
      }
      TextSize = Canvas->TextExtent(Caption);

      DebugAssert(StaticText->BorderStyle == sbsNone); // not taken into account
      if (AccelChar)
      {
        TextSize.cy += 2;
      }
    }
    else
    {
      TRect TextRect;
      SetRect(&TextRect, 0, 0, StaticText->Width, 0);
      DrawText(Canvas->Handle, StaticText->Caption.c_str(), -1, &TextRect,
        DT_CALCRECT | DT_WORDBREAK |
        StaticText->DrawTextBiDiModeFlagsReadingOnly());
      TextSize = TextRect.GetSize();
    }

    R.Bottom = R.Top + TextSize.cy;
    // Should call ChangeBiDiModeAlignment when UseRightToLeftAlignment(),
    // but the label seems to draw the text wrongly aligned, even though
    // the alignment is correctly flipped in TCustomStaticText.CreateParams
    switch (StaticText->Alignment)
    {
      case taLeftJustify:
        R.Right = R.Left + TextSize.cx;
        break;

      case taRightJustify:
        R.Left = Max(0, R.Right - TextSize.cx);
        break;

      case taCenter:
        {
          DebugFail(); // not used branch, possibly untested
          int Diff = R.Width() - TextSize.cx;
          R.Left += Diff / 2;
          R.Right -= Diff - (Diff / 2);
        }
        break;
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
    // in case the action takes long, make sure focus is shown immediately
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
    if (reinterpret_cast<TWMChar &>(Message).CharCode == L' ')
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
      // in case the action takes long, make sure focus is shown immediately
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
    TCanvas * Canvas;
    FocusableLabelCanvas(StaticText, &Canvas, R);
    try
    {
      if (StaticText->Focused())
      {
        Canvas->DrawFocusRect(R);
      }
      else if ((StaticText->Font->Color != LinkColor) && // LinkActionLabel and LinkLabel
               !EndsStr(LinkAppLabelMark, StaticText->Caption)) // LinkAppLabel
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
        Canvas->MoveTo(R.Left, R.Bottom - 1);
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
  HintInfo.HintMaxWidth = GetParentForm(Control)->Monitor->Width;
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

    PersistentHintWindow = new TScreenTipHintWindow(Application);
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
void __fastcall HintLabel(TStaticText * StaticText, UnicodeString Hint)
{
  // Currently all are right-justified, when other alignment is used,
  // test respective branches in FocusableLabelCanvas.
  DebugAssert(StaticText->Alignment == taRightJustify);
  // With right-justify, it has to be off. We may not notice on original
  // English version, results will differ with translations only
  DebugAssert(!StaticText->AutoSize);
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
static void __fastcall ComboBoxFixWindowProc(void * Data, TMessage & Message)
{
  // it is TCustomComboxBox, but the properties are published only by TComboBox
  TComboBox * ComboBox = static_cast<TComboBox *>(Data);
  if (Message.Msg == WM_SIZE)
  {
    UnicodeString Text = ComboBox->Text;
    try
    {
      ControlWndProc(ComboBox)(Message);
    }
    __finally
    {
      // workaround for bug in combo box, that causes it to change text to any
      // item from drop down list which starts with current text,
      // after control is resized (unless the text is in drop down list as well)
      ComboBox->Text = Text;
      // hide selection, which is wrongly shown when form is resized, even when the box has not focus
      if (!ComboBox->Focused())
      {
        ComboBox->SelLength = 0;
      }
    }
  }
  else
  {
    ControlWndProc(ComboBox)(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall FixComboBoxResizeBug(TCustomComboBox * ComboBox)
{
  TWndMethod WindowProc;
  ((TMethod*)&WindowProc)->Data = ComboBox;
  ((TMethod*)&WindowProc)->Code = ComboBoxFixWindowProc;
  ComboBox->WindowProc = WindowProc;
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
    OpenBrowser(StaticText->Caption);
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
    if ((GetKeyState(VK_CONTROL) < 0) && (Key.CharCode == L'C'))
    {
      TInstantOperationVisualizer Visualizer;
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
    DebugAssert(StaticText->PopupMenu == NULL);
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
  DebugAssert(MenuItem != NULL);

  if (MenuItem->Tag == 0)
  {
    LinkLabelClick(StaticText);
  }
  else
  {
    TInstantOperationVisualizer Visualizer;
    CopyToClipboard(StaticText->Caption);
  }
}
//---------------------------------------------------------------------------
static void __fastcall DoLinkLabel(TStaticText * StaticText)
{
  StaticText->Transparent = false;
  StaticText->ParentFont = true;
  StaticText->Cursor = crHandPoint;

  TWndMethod WindowProc;
  ((TMethod*)&WindowProc)->Data = StaticText;
  ((TMethod*)&WindowProc)->Code = LinkLabelWindowProc;
  StaticText->WindowProc = WindowProc;
}
//---------------------------------------------------------------------------
void __fastcall LinkLabel(TStaticText * StaticText, UnicodeString Url,
  TNotifyEvent OnEnter)
{
  DoLinkLabel(StaticText);

  StaticText->Font->Style = StaticText->Font->Style << fsUnderline;

  reinterpret_cast<TButton*>(StaticText)->OnEnter = OnEnter;

  if (!Url.IsEmpty())
  {
    StaticText->Caption = Url;
  }

  bool IsUrl = IsHttpOrHttpsUrl(StaticText->Caption);
  if (IsUrl)
  {
    DebugAssert(StaticText->PopupMenu == NULL);
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
      Item->ShortCut = ShortCut(L' ', TShiftState());
      Item->OnClick = ContextMenuOnClick;
      StaticText->PopupMenu->Items->Add(Item);

      Item = new TMenuItem(StaticText->PopupMenu);
      Item->Caption = LoadStr(EDIT_COPY);
      Item->Tag = 1;
      Item->ShortCut = ShortCut(L'C', TShiftState() << ssCtrl);
      Item->OnClick = ContextMenuOnClick;
      StaticText->PopupMenu->Items->Add(Item);
    }
    catch(...)
    {
      delete StaticText->PopupMenu;
      DebugAssert(StaticText->PopupMenu == NULL);
      throw;
    }
  }

  StaticText->Font->Color = LinkColor;
}
//---------------------------------------------------------------------------
void __fastcall LinkActionLabel(TStaticText * StaticText)
{
  // Must be called only after setting Parent, as it modifies Font and hence clears ParentFont
  DebugAssert(StaticText->Parent != NULL);
  DoLinkLabel(StaticText);

  StaticText->Font->Color = LinkColor;
}
//---------------------------------------------------------------------------
void __fastcall LinkAppLabel(TStaticText * StaticText)
{
  DoLinkLabel(StaticText);

  StaticText->Caption = StaticText->Caption + LinkAppLabelMark;
}
//---------------------------------------------------------------------------
static void __fastcall HotTrackLabelMouseEnter(void * /*Data*/, TObject * Sender)
{
  reinterpret_cast<TLabel *>(Sender)->Font->Color = clBlue;
}
//---------------------------------------------------------------------------
static void __fastcall HotTrackLabelMouseLeave(void * /*Data*/, TObject * Sender)
{
  reinterpret_cast<TLabel *>(Sender)->ParentFont = true;
}
//---------------------------------------------------------------------------
void __fastcall HotTrackLabel(TLabel * Label)
{
  DebugAssert(Label->OnMouseEnter == NULL);
  DebugAssert(Label->OnMouseLeave == NULL);

  Label->OnMouseEnter = MakeMethod<TNotifyEvent>(NULL, HotTrackLabelMouseEnter);
  Label->OnMouseLeave = MakeMethod<TNotifyEvent>(NULL, HotTrackLabelMouseLeave);
}
//---------------------------------------------------------------------------
void __fastcall SetLabelHintPopup(TLabel * Label, const UnicodeString & Hint)
{
  Label->Caption = Hint;
  Label->Hint = Hint;
  TRect Rect(0, 0, Label->Width, 0);
  TScreenTipHintWindow::CalcHintTextRect(Label, Label->Canvas, Rect, Label->Caption);
  Label->ShowHint = (Rect.Bottom > Label->Height);
}
//---------------------------------------------------------------------------
bool __fastcall HasLabelHintPopup(TControl * Control, const UnicodeString & HintStr)
{
  TLabel * HintLabel = dynamic_cast<TLabel *>(Control);
  return (HintLabel != NULL) && (GetShortHint(HintLabel->Caption) == HintStr);
}
//---------------------------------------------------------------------------
Forms::TMonitor *  __fastcall FormMonitor(TCustomForm * Form)
{
  Forms::TMonitor * Result;
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
    DebugAssert(Screen->Monitors[i]->Primary);
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
TForm * __fastcall _SafeFormCreate(TMetaClass * FormClass, TComponent * Owner)
{
  TForm * Form;

  if (Owner == NULL)
  {
    Owner = GetFormOwner();
  }

  // If there is no main form yet, make this one main.
  // This:
  // - Makes other forms (dialogs invoked from this one),
  // be placed on the same monitor (otherwise all new forms get placed
  // on primary monitor)
  // - Triggers MainForm-specific code in DoFormWindowProc.
  // - Shows button on taskbar
  if (Application->MainForm == NULL)
  {
    Application->CreateForm(FormClass, &Form);
    DebugAssert(Application->MainForm == Form);
  }
  else
  {
    Form = dynamic_cast<TForm *>(Construct(FormClass, Owner));
    DebugAssert(Form != NULL);
  }

  return Form;
}
//---------------------------------------------------------------------------
static TButton * __fastcall FindStandardButton(TWinControl * Control, bool Default)
{
  TButton * Result = NULL;
  int Index = 0;
  while ((Result == NULL) && (Index < Control->ControlCount))
  {
    TControl * ChildControl = Control->Controls[Index];
    TButton * Button = dynamic_cast<TButton *>(ChildControl);
    if ((Button != NULL) && (Default ? Button->Default : Button->Cancel))
    {
      Result = Button;
    }
    else
    {
      TWinControl * WinControl = dynamic_cast<TWinControl *>(ChildControl);
      if (WinControl != NULL)
      {
        Result = FindStandardButton(WinControl, Default);
      }
    }
    Index++;
  }
  return Result;
}
//---------------------------------------------------------------------------
TModalResult __fastcall DefaultResult(TCustomForm * Form, TButton * DefaultButton)
{
  // The point of this is to avoid hardcoding mrOk when checking dialog results.
  // Previously we used != mrCancel instead, as mrCancel is more reliable,
  // being automatically used for Esc/X buttons (and hence kind of forced to be used
  // for Cancel buttons). But that failed to be reliable in the end, for
  // ModalResult being mrNone, when Windows session is being logged off.
  // We interpreted mrNone as OK, causing lots of troubles.
  TModalResult Result = mrNone;
  TButton * Button = FindStandardButton(Form, true);
  if (DebugAlwaysTrue(Button != NULL))
  {
    Result = Button->ModalResult;
  }
  if (Result == mrNone)
  {
    DebugAssert((DefaultButton != NULL) && (DefaultButton->ModalResult != mrNone));
    Result = DefaultButton->ModalResult;
  }
  else
  {
    // If default button fallback was provided,
    // make sure it is the default button we actually detected
    DebugAssert((DefaultButton == NULL) || (Button == DefaultButton));
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall DefaultButton(TButton * Button, bool Default)
{
  // default property setter does not have guard for "the same value"
  if (Button->Default != Default)
  {
    Button->Default = Default;
  }
}
//---------------------------------------------------------------------------
void __fastcall MemoKeyDown(TObject * Sender, WORD & Key, TShiftState Shift)
{
  // Sender can be Form or Memo itself
  TControl * Control = dynamic_cast<TControl *>(Sender);
  if (DebugAlwaysTrue(Control != NULL))
  {
    TCustomForm * Form = GetParentForm(Control);
    // Particularly when WantReturns is true,
    // memo swallows also Esc, so we have to handle it ourselves.
    // See also ReadOnlyControl.
    if ((Key == VK_ESCAPE) && Shift.Empty())
    {
      Form->ModalResult = mrCancel;
      Key = 0;
    }
    else if ((Key == VK_RETURN) && Shift.Contains(ssCtrl))
    {
      Form->ModalResult = DefaultResult(Form);
      Key = 0;
    }
  }
}
//---------------------------------------------------------------------------
class TIconOwnerComponent : public TComponent
{
public:
  __fastcall TIconOwnerComponent(TIcon * Icon) :
    TComponent(NULL),
    FIcon(Icon)
  {
  }

private:
  std::unique_ptr<TIcon> FIcon;
};
//---------------------------------------------------------------------------
static void __fastcall FixFormIcon(TForm * Form, int Size, int WidthMetric, int HeightMetric)
{
  // Whole this code is to call ReadIcon from Vcl.Graphics.pas with correct size

  // Clone the icon data (whole .ico file content, that is originally loaded from .dfm)
  // to a new TIcon that does not have a size fixed yet (size cannot be changed after handle is allocated)
  std::unique_ptr<TMemoryStream> Stream(new TMemoryStream());
  Form->Icon->SaveToStream(Stream.get());
  std::unique_ptr<TIcon> Icon(new TIcon());
  Stream->Position = 0;
  Icon->LoadFromStream(Stream.get());

  // Set desired size
  int Width = GetSystemMetricsForControl(Form, WidthMetric);
  int Height = GetSystemMetricsForControl(Form, HeightMetric);
  Icon->SetSize(Width, Height);

  // This calls TIcon::RequireHandle that retrieves the best icon for given size
  LPARAM LParam = reinterpret_cast<LPARAM>(Icon->Handle);
  SendMessage(Form->Handle, WM_SETICON, Size, LParam);

  // Make sure the icon is released
  TIconOwnerComponent * IconOwnerComponent = new TIconOwnerComponent(Icon.release());
  IconOwnerComponent->Name = TIconOwnerComponent::QualifiedClassName() + IntToStr(Size);
  Form->InsertComponent(IconOwnerComponent);
}
//---------------------------------------------------------------------------
void __fastcall FixFormIcons(TForm * Form)
{
  // VCL sets only ICON_BIG (so small icon is scaled down by OS from big icon),
  // and it uses a random (first?) size from the resource,
  // not the best size.
  FixFormIcon(Form, ICON_SMALL, SM_CXSMICON, SM_CYSMICON);
  FixFormIcon(Form, ICON_BIG, SM_CXICON, SM_CYICON);
  // We rely on VCL not calling WM_SETICON ever after
  // (what it would do, if CreateWnd is called again).
  // That would overwrite the ICON_BIG.
  // We might be able to make sure it uses a correct size by calling
  // TIcon.ReleaseHandle and setting a correct size.
}
//---------------------------------------------------------------------------
class TDesktopFontManager : public TComponent
{
public:
  TDesktopFontManager();
  virtual __fastcall ~TDesktopFontManager();

  void AddControl(TControl * Control);
  void Update();

protected:
  virtual void __fastcall Notification(TComponent * AComponent, TOperation Operation);

private:
  HWND FWindowHandle;
  typedef std::set<TControl *> TControlSet;
  TControlSet FControls;

  void __fastcall WndProc(TMessage & Message);
  void UpdateControl(TControl * Control);
};
//---------------------------------------------------------------------------
TDesktopFontManager::TDesktopFontManager() : TComponent(NULL)
{
  // Alternative is using Application->HookMainWindow
  FWindowHandle = AllocateHWnd(WndProc);
}
//---------------------------------------------------------------------------
__fastcall TDesktopFontManager::~TDesktopFontManager()
{
  DeallocateHWnd(FWindowHandle);
}
//---------------------------------------------------------------------------
void __fastcall TDesktopFontManager::Notification(TComponent * AComponent, TOperation Operation)
{
  if (DebugAlwaysTrue(Operation == opRemove))
  {
    TControl * Control = DebugNotNull(dynamic_cast<TControl *>(AComponent));
    FControls.erase(Control);
  }

  TComponent::Notification(AComponent, Operation);
}
//---------------------------------------------------------------------------
void TDesktopFontManager::UpdateControl(TControl * Control)
{
  class TPublicControl : public TControl
  {
  public:
    __property Font;
  };

  TPublicControl * PublicControl = reinterpret_cast<TPublicControl *>(Control);
  std::unique_ptr<TFont> DesktopFont(new TFont());
  if (WinConfiguration->PanelFont.FontName.IsEmpty())
  {
    int PixelsPerInch = GetControlPixelsPerInch(Control);
    TLogFont LogFont;
    if (DebugAlwaysTrue(SystemParametersInfoForPixelsPerInch(SPI_GETICONTITLELOGFONT, sizeof(LogFont), &LogFont, 0, PixelsPerInch)))
    {
      DesktopFont->Handle = CreateFontIndirect(&LogFont);
    }
  }
  else
  {
    TWinConfiguration::RestoreFont(WinConfiguration->PanelFont, DesktopFont.get());
    DesktopFont->Height = ScaleByPixelsPerInchFromSystem(DesktopFont->Height, Control);
    DesktopFont->PixelsPerInch = PublicControl->Font->PixelsPerInch;
  }

  // Neither CreateFontIndirect nor RestoreFont set color, so we should have the default set by TFont constructor here.
  DebugAssert(DesktopFont->Color == clWindowText);
  if (!SameFont(DesktopFont.get(), PublicControl->Font) ||
      (DesktopFont->PixelsPerInch != PublicControl->Font->PixelsPerInch))
  {
    // Preserve color (particularly white color of file panel font in dark mode)
    DesktopFont->Color = PublicControl->Font->Color;

    PublicControl->Font->Assign(DesktopFont.get());
  }
}
//---------------------------------------------------------------------------
void TDesktopFontManager::AddControl(TControl * Control)
{
  FControls.insert(Control);
  Control->FreeNotification(this);
  UpdateControl(Control);
}
//---------------------------------------------------------------------------
void TDesktopFontManager::Update()
{
  TControlSet::iterator I = FControls.begin();
  while (I != FControls.end())
  {
    UpdateControl(*I);
    I++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TDesktopFontManager::WndProc(TMessage & Message)
{
  if (Message.Msg == WM_WININICHANGE)
  {
    Update();
  }
  Message.Result = DefWindowProc(FWindowHandle, Message.Msg, Message.WParam, Message.LParam);
}
//---------------------------------------------------------------------------
std::unique_ptr<TDesktopFontManager> DesktopFontManager(new TDesktopFontManager());
//---------------------------------------------------------------------------
// This might be somewhat redundant now that at least the default Desktop font is actually the default VCL font
void __fastcall UseDesktopFont(TControl * Control)
{
  TCustomStatusBar * StatusBar = dynamic_cast<TCustomStatusBar *>(Control);
  if (StatusBar != NULL)
  {
    // prevent syncing to system font
    StatusBar->UseSystemFont = false;
  }

  DesktopFontManager->AddControl(Control);
}
//---------------------------------------------------------------------------
void __fastcall UpdateDesktopFont()
{
  DesktopFontManager->Update();
}
//---------------------------------------------------------------------------
TShiftState __fastcall AllKeyShiftStates()
{
  return TShiftState() << ssShift << ssAlt << ssCtrl;
}
//---------------------------------------------------------------------------
static bool __fastcall FormActivationHook(void * Data, TMessage & Message)
{
  bool Result = false;
  // Some dialogs, when application is restored from minimization,
  // do not get activated. So we do it explicitly here.
  // We cannot do this from TApplication::OnActivate because
  // TApplication.WndProc resets focus to the last active window afterwards.
  // So we override CM_ACTIVATE implementation here completely.
  if (Message.Msg == CM_ACTIVATE)
  {
    TCustomForm * Form = static_cast<TCustomForm *>(Data);
    if (Screen->FocusedForm == Form)
    {
      ::SetFocus(Form->Handle);
      // VCLCOPY
      if (Application->OnActivate != NULL)
      {
        Application->OnActivate(Application);
      }
      Result = true;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall HookFormActivation(TCustomForm * Form)
{
  Application->HookMainWindow(MakeMethod<TWindowHook>(Form, FormActivationHook));
}
//---------------------------------------------------------------------------
void __fastcall UnhookFormActivation(TCustomForm * Form)
{
  Application->UnhookMainWindow(MakeMethod<TWindowHook>(Form, FormActivationHook));
}
//---------------------------------------------------------------------------
void __fastcall ShowFormNoActivate(TForm * Form)
{
  TPublicForm * PublicForm = static_cast<TPublicForm *>(Form);

  // This is same as SendToBack, except for added SWP_NOACTIVATE (VCLCOPY)
  SetWindowPos(PublicForm->WindowHandle, HWND_BOTTOM, 0, 0, 0, 0,
    SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE);

  // This replaces TCustomForm::CMShowingChanged()
  // which calls ShowWindow(Handle, SW_SHOWNORMAL).

  ShowWindow(Form->Handle, SW_SHOWNOACTIVATE);

  // - so we have to call DoShow explicitly.
  PublicForm->DoShow();

  // - also we skip applying TForm::Position (VCLCOPY)
  if (DebugAlwaysTrue(Form->Position == poOwnerFormCenter))
  {
    TCustomForm * CenterForm = Application->MainForm;
    TCustomForm * OwnerForm = dynamic_cast<TCustomForm *>(Form->Owner);
    if (OwnerForm != NULL)
    {
      CenterForm = OwnerForm;
    }
    int X, Y;
    if ((CenterForm != NULL) && (CenterForm != Form))
    {
      TRect Bounds = CenterForm->BoundsRect;
      X = ((Bounds.Width() - Form->Width) / 2) + CenterForm->Left;
      Y = ((Bounds.Height() - Form->Height) / 2) + CenterForm->Top;
    }
    else
    {
      X = (Screen->Width - Form->Width) / 2;
      Y = (Screen->Height - Form->Height) / 2;
    }
    if (X < Screen->DesktopLeft)
    {
      X = Screen->DesktopLeft;
    }
    if (Y < Screen->DesktopTop)
    {
      Y = Screen->DesktopTop;
    }
    Form->SetBounds(X, Y, Form->Width, Form->Height);
    // We cannot call SetWindowToMonitor().
    // We cannot set FPosition = poDesigned, so workarea-checking code
    // in DoFormWindowProc is not triggered

    // If application is restored, dialog is not activated, do it manually.
    // Wait for application to be activated to activate ourself.
    HookFormActivation(Form);
  }
}
//---------------------------------------------------------------------------
TPanel * __fastcall CreateBlankPanel(TComponent * Owner)
{
  TPanel * Panel = new TPanel(Owner);
  Panel->BevelOuter = bvNone;
  Panel->BevelInner = bvNone; // default
  Panel->BevelKind = bkNone;
  return Panel;
}
//---------------------------------------------------------------------------
bool IsButtonBeingClicked(TButtonControl * Button)
{
  class TPublicButtonControl : public TButtonControl
  {
  public:
    __property ClicksDisabled;
  };
  TPublicButtonControl * PublicButton = reinterpret_cast<TPublicButtonControl *>(Button);
  // HACK ClicksDisabled is set in TButtonControl.WndProc while changing focus as response to WM_LBUTTONDOWN.
  return PublicButton->ClicksDisabled;
}
//---------------------------------------------------------------------------
// When using this in OnExit handers, it's still possible that the user does not actually click the
// CancelButton (for example, when the button is released out of the button).
// Then the validation is bypassed. Consequently, all dialogs that uses this must still
// gracefully handle submission with non-validated data.
bool IsCancelButtonBeingClicked(TControl * Control)
{
  TCustomForm * Form = GetParentForm(Control);
  TButtonControl * CancelButton = FindStandardButton(Form, false);
  // Find dialog has no Cancel button
  return (CancelButton != NULL) && IsButtonBeingClicked(CancelButton);
}
//---------------------------------------------------------------------------
TCanvas * CreateControlCanvas(TControl * Control)
{
  std::unique_ptr<TControlCanvas> Canvas(new TControlCanvas());
  Canvas->Control = Control;
  TPublicControl * PublicControl = static_cast<TPublicControl *>(Control);
  Canvas->Font = PublicControl->Font;
  return Canvas.release();
}
//---------------------------------------------------------------------------
void AutoSizeButton(TButton * Button)
{
  std::unique_ptr<TCanvas> Canvas(CreateControlCanvas(Button));
  int MinWidth = Canvas->TextWidth(Button->Caption) + ScaleByTextHeight(Button, (2 * 8));
  if (Button->Width < MinWidth)
  {
    if (Button->Anchors.Contains(akRight))
    {
      Button->Left = Button->Left - (MinWidth - Button->Width);
    }
    Button->Width = MinWidth;
  }
}
//---------------------------------------------------------------------------
template<class T>
bool DoAutoSizeLabel(T * Label, TCanvas * Canvas)
{
  TRect TextRect;
  SetRect(&TextRect, 0, 0, Label->Width, 0);
  DrawText(Canvas->Handle, Label->Caption.c_str(), Label->Caption.Length() + 1, &TextRect,
    DT_EXPANDTABS | DT_CALCRECT | DT_WORDBREAK | DT_NOPREFIX |
    Label->DrawTextBiDiModeFlagsReadingOnly());
  bool Result = (TextRect.Height() > Label->Height);
  if (Result)
  {
    Label->Height = TextRect.Height();
    Label->AutoSize = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
void AutoSizeLabel(TLabel * Label)
{
  if (!DoAutoSizeLabel(Label, Label->Canvas))
  {
    Label->WordWrap = false;
  }
}
//---------------------------------------------------------------------------
void AutoSizeLabel(TStaticText * Label)
{
  std::unique_ptr<TCanvas> Canvas(CreateControlCanvas(Label));
  DoAutoSizeLabel(Label, Canvas.get());
}//---------------------------------------------------------------------------
void GiveTBItemPriority(TTBCustomItem * Item)
{
  DebugAssert(Item->GetTopComponent() != NULL);
  TTBCustomToolbar * ToolbarComponent = dynamic_cast<TTBCustomToolbar *>(Item->GetTopComponent());
  if ((ToolbarComponent != NULL) &&
      // Only for top-level buttons on custom command toolbar, not for submenus of the custom commands menu in the main menu
      (Item->Parent == ToolbarComponent->Items))
  {
    TTBItemViewer * Viewer = ToolbarComponent->View->Find(Item);
    ToolbarComponent->View->GivePriority(Viewer);
  }
}
//---------------------------------------------------------------------------
void DeleteChildren(TWinControl * Control)
{
  while (Control->ControlCount > 0)
  {
    delete Control->Controls[0];
  }
}
