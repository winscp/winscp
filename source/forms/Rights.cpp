//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "Rights.h"

#include <VCLCommon.h>
#include <Tools.h>
#include <TextsWin.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GrayedCheckBox"
#pragma link "PngImageList"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TRightsFrame::TRightsFrame(TComponent* Owner)
        : TFrame(Owner)
{
  FAddXToDirectoriesSuffix = L"+x";
  FOnChange = NULL;
  FAllowAddXToDirectories = true;
  FPopup = false;
  FPopupParent = NULL;
  FDefaultButton = NULL;
  FCancelButton = NULL;
  // to avoid duplication of reference in forms that uses the frame
  PopupMenu = RightsPopup;
  FPopingContextMenu = false;
  FInitialized = false;
  FAcl = false;

  #define COPY_HINT(R) \
    Checks[TRights::rrGroup ## R]->Hint = Checks[TRights::rrUser ## R]->Hint; \
    Checks[TRights::rrOther ## R]->Hint = Checks[TRights::rrUser ## R]->Hint;
  COPY_HINT(Read);
  COPY_HINT(Write);
  COPY_HINT(Exec);
  #undef COPY_HINT
}
//---------------------------------------------------------------------------
__fastcall TRightsFrame::~TRightsFrame()
{
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetStates(TRights::TRight Right, TRights::TState value)
{
  DebugAssert(((value == TRights::rsNo) || (value == TRights::rsYes) || (value == TRights::rsUndef)));
  TCheckBox * CheckBox = Checks[Right];
  if (CheckBox != NULL)
  {
    switch (value) {
      case TRights::rsNo: CheckBox->State = cbUnchecked; break;
      case TRights::rsYes: CheckBox->State = cbChecked; break;
      case TRights::rsUndef: CheckBox->State = cbGrayed; break;
    }
  }
}
//---------------------------------------------------------------------------
TRights::TState __fastcall TRightsFrame::GetStates(TRights::TRight Right)
{
  TCheckBox * CheckBox = Checks[Right];
  if (CheckBox != NULL)
  {
    switch (CheckBox->State) {
      case cbUnchecked: return TRights::rsNo;
      case cbChecked: return TRights::rsYes;
      case cbGrayed:
      default: return TRights::rsUndef;
    }
  }
  else
  {
    return TRights::rsNo;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetRights(const TRights & value)
{
  bool Changed = (AllowUndef != value.AllowUndef);
  AllowUndef = true; // temporarily
  for (int Right = TRights::rrFirst; Right <= TRights::rrLast; Right++)
  {
    TRights::TRight R = static_cast<TRights::TRight>(Right);
    if (States[R] != value.RightUndef[R])
    {
      States[R] = value.RightUndef[R];
      Changed = true;
    }
  }
  AllowUndef = value.AllowUndef;
  if (Changed || !FInitialized)
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
TRights __fastcall TRightsFrame::GetRights()
{
  TRights Result;
  Result.AllowUndef = AllowUndef;
  for (int Right = TRights::rrFirst; Right <= TRights::rrLast; Right++)
  {
    Result.RightUndef[static_cast<TRights::TRight>(Right)] =
      States[static_cast<TRights::TRight>(Right)];
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetAllowUndef(bool value)
{
  for (int Right = TRights::rrFirst; Right <= TRights::rrLast; Right++)
  {
    TCheckBox * CheckBox = Checks[static_cast<TRights::TRight>(Right)];
    if (CheckBox != NULL)
    {
      CheckBox->AllowGrayed = value;
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TRightsFrame::GetAllowUndef()
{
  bool Result = false, First = true;
  for (int Right = TRights::rrFirst; Right <= TRights::rrLast; Right++)
  {
    TCheckBox * Check = Checks[static_cast<TRights::TRight>(Right)];
    if (Check != NULL)
    {
      if (First)
      {
        Result = Check->AllowGrayed;
        First = false;
      }
      else if (Result != Check->AllowGrayed)
      {
        DebugFail();
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
TCheckBox * __fastcall TRightsFrame::GetChecks(TRights::TRight Right)
{
  for (int Index = 0; Index < ControlCount; Index++)
  {
    if (Controls[Index]->InheritsFrom(__classid(TCheckBox)) &&
        (Controls[Index]->Tag == TRights::RightToFlag(Right)))
    {
      return ((TCheckBox *)Controls[Index]);
    }
  }
  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetAddXToDirectories(bool value)
{
  DirectoriesXCheck->Checked = value;
}
//---------------------------------------------------------------------------
bool __fastcall TRightsFrame::GetAddXToDirectories()
{
  return DirectoriesXCheck->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TRightsFrame::DirectoriesXEffective()
{
  return !((Rights.NumberSet & TRights::rfExec) == TRights::rfExec);
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::UpdateControls()
{
  if (!OctalEdit->Focused())
  {
    UpdateOctalEdit();
  }
  Color = (FPopup ? clWindow : clBtnFace);
  DirectoriesXCheck->Visible = AllowAddXToDirectories;
  EnableControl(DirectoriesXCheck,
    Enabled && DirectoriesXEffective());
  FInitialized = true;
  DoChange();
}
//---------------------------------------------------------------------------
void TRightsFrame::CycleRights(TRights::TRightGroup RightGroup)
{
  int Last = FAcl ? TRights::rlLastAcl : TRights::rlLastNormal;
  TRights::TState State = TRights::TState(); // shut up
  bool Any = false;
  bool Same = true;
  for (int RightLevelI = TRights::rlFirst; RightLevelI <= Last; RightLevelI++)
  {
    TRights::TRightLevel RightLevel = static_cast<TRights::TRightLevel>(RightLevelI);
    TRights::TRight Right = TRights::CalculateRight(RightGroup, RightLevel);

    if (Checks[Right]->Visible)
    {
      TRights::TState RightState = States[Right];
      if (!Any)
      {
        State = RightState;
        Any = true;
      }
      else if (State != RightState)
      {
        Same = false;
        break;
      }
    }
  }

  if (DebugAlwaysTrue(Any))
  {
    if (!Same)
    {
      State = TRights::rsYes;
    }
    else
    {
      switch (State)
      {
        case TRights::rsYes:
          State = TRights::rsNo;
          break;

        case TRights::rsNo:
          State = AllowUndef ? TRights::rsUndef : TRights::rsYes;
          break;

        case TRights::rsUndef:
          State = TRights::rsYes;
          break;
      }
    }

    for (int RightLevelI = TRights::rlFirst; RightLevelI <= Last; RightLevelI++)
    {
      TRights::TRightLevel RightLevel = static_cast<TRights::TRightLevel>(RightLevelI);
      TRights::TRight Right = TRights::CalculateRight(RightGroup, RightLevel);
      States[Right] = State;
    }
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::RightsButtonsClick(TObject * Sender)
{
  CycleRights(static_cast<TRights::TRightGroup>(dynamic_cast<TComponent *>(Sender)->Tag - 1));
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetAllowAddXToDirectories(bool value)
{
  if ((FAllowAddXToDirectories != value) || !FInitialized)
  {
    FAllowAddXToDirectories = value;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::DoChange()
{
  if (FOnChange)
  {
    FOnChange(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetEnabled(bool Value)
{
  TFrame::SetEnabled(Value);
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::ForceUpdate()
{
  UpdateOctalEdit();
}
//---------------------------------------------------------------------------
bool __fastcall TRightsFrame::HasFocus()
{
  return
    (Focused() ||
    ((Screen->ActiveControl != NULL) && (Screen->ActiveControl->Parent == this))) ||
    ((PopupParent != NULL) && PopupParent->Focused());
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::RightsActionsExecute(TBasicAction * Action,
  bool & Handled)
{
  // prevent shortcuts to be evaluated when frame does not have a focus
  if (HasFocus())
  {
    bool Changed = true;
    TRights R = Rights;
    R.Number = TRights::rfNo;

    Handled = true;
    if (Action == NoRightsAction)
    {
      R = TRights::rfNo;
    }
    else if (Action == DefaultRightsAction)
    {
      R = TRights::rfDefault;
    }
    else if (Action == AllRightsAction)
    {
      R = TRights::rfAll;
    }
    else if (Action == LeaveRightsAsIsAction)
    {
      R.AllUndef();
    }
    else if (Action == CopyTextAction)
    {
      TInstantOperationVisualizer Visualizer;
      CopyToClipboard(Text);
      Changed = false;
    }
    else if (Action == CopyOctalAction)
    {
      TRights R = Rights;
      DebugAssert(!R.IsUndef);
      if (!R.IsUndef)
      {
        TInstantOperationVisualizer Visualizer;
        CopyToClipboard(R.Octal);
      }
      Changed = false;
    }
    else if (Action == PasteAction)
    {
      UnicodeString S;
      if (TextFromClipboard(S, true))
      {
        Text = S;
      }
      // trigger on change event, even if no change actually occurred to
      // allow parent form to visualize feedback of an action
      DoChange();
      // Update octal edit in Rights frame even if it has focus
      ForceUpdate();
      Changed = false;
    }
    else
    {
      Handled = false;
    }

    if (Changed)
    {
      Rights = R;
      ForceUpdate();
      DoChange();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::RightsActionsUpdate(TBasicAction *Action,
      bool &Handled)
{
  TRights R = Rights;

  Handled = true;
  if (Action == NoRightsAction)
  {
    // explicitly enable as action gets disabled, if its shortcut is pressed,
    // while the form does not have a focus and OnExecute handler denies the execution
    NoRightsAction->Enabled = !FAcl;
    NoRightsAction->Checked = !R.IsUndef && (R.NumberSet == TRights::rfNo);
  }
  else if (Action == DefaultRightsAction)
  {
    DefaultRightsAction->Enabled = !FAcl;
    DefaultRightsAction->Checked = !R.IsUndef && (R.NumberSet == TRights::rfDefault);
  }
  else if (Action == AllRightsAction)
  {
    AllRightsAction->Enabled = !FAcl;
    AllRightsAction->Checked = !R.IsUndef && (R.NumberSet == TRights::rfAll);
  }
  else if (Action == LeaveRightsAsIsAction)
  {
    LeaveRightsAsIsAction->Enabled = !FAcl;
    LeaveRightsAsIsAction->Visible = R.AllowUndef;
    LeaveRightsAsIsAction->Checked = (R.NumberSet == TRights::rfNo) &&
      (R.NumberUnset == TRights::rfNo);
  }
  else if (Action == CopyTextAction)
  {
    CopyTextAction->Enabled = !FAcl && !R.IsUndef;
  }
  else if (Action == CopyOctalAction)
  {
    CopyOctalAction->Enabled = !FAcl && !R.IsUndef;
  }
  else if (Action == PasteAction)
  {
    PasteAction->Enabled = !FAcl && IsFormatInClipboard(CF_TEXT);
  }
  else
  {
    Handled = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::CreateParams(TCreateParams & Params)
{
  TFrame::CreateParams(Params);
  if (FPopup)
  {
    Params.Style |= WS_BORDER;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::CreateWnd()
{
  if (FPopup)
  {
    Width += 2 * GetSystemMetricsForControl(Parent, SM_CXBORDER);
    Height += 2 * GetSystemMetricsForControl(Parent, SM_CYBORDER);
  }
  TFrame::CreateWnd();
  UpdateButtons();
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetPopup(bool value)
{
  if (Popup != value)
  {
    FPopup = value;
    Visible = !FPopup;

    CloseButton->Visible = value;
    CloseButton->Cancel = value;
    CloseButton->Default = value;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::DoCloseUp()
{
  Hide();

  if (FDefaultButton != NULL)
  {
    FDefaultButton->Default = true;
    FDefaultButton = NULL;
  }

  if (FCancelButton != NULL)
  {
    FCancelButton->Cancel = true;
    FCancelButton = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::DoExit()
{
  // this is bit hack to make frame hide when ComboEdit button is pressed while
  // from is poped-up already.
  if (FPopup && !IsAncestor(Screen->ActiveControl, FPopupParent))
  {
    DoCloseUp();
  }
  TFrame::DoExit();
}
//---------------------------------------------------------------------------
bool __fastcall TRightsFrame::IsAncestor(TControl * Control, TControl * Ancestor)
{
  while ((Control != NULL) &&
         (Control->Parent != Ancestor))
  {
    Control = Control->Parent;
  }
  return (Control != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::WMContextMenu(TWMContextMenu & Message)
{
  FPopingContextMenu = true;
  try
  {
    TFrame::Dispatch(&Message);
  }
  __finally
  {
    FPopingContextMenu = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::CMDialogKey(TCMDialogKey & Message)
{
  if (FPopup && Visible &&
      ((Message.CharCode == VK_RETURN) ||
       (Message.CharCode == VK_ESCAPE)) &&
      KeyDataToShiftState(Message.KeyData).Empty())
  {
    CloseUp();
    Message.Result = 1;
  }
  else
  {
    TFrame::Dispatch(&Message);
  }

  if (Message.CharCode == VK_MENU)
  {
    UpdateButtons();
  }
}
//---------------------------------------------------------------------------
bool TRightsFrame::IsButtonAccel(TCMDialogChar & Message, TSpeedButton * Button, TWinControl * FocusControl)
{
  bool Result = IsAccel(Message.CharCode, Button->Caption) && Button->Visible && Button->Enabled;
  if (Result)
  {
    Perform(WM_CHANGEUISTATE, MAKELONG(UIS_CLEAR, UISF_HIDEFOCUS), 0);
    FocusControl->SetFocus();
  }
  return Result;
}
//---------------------------------------------------------------------------
void TRightsFrame::CMDialogChar(TCMDialogChar & Message)
{
  if (IsButtonAccel(Message, OwnerButton, OwnerReadCheck) ||
      IsButtonAccel(Message, GroupButton, GroupReadCheck) ||
      IsButtonAccel(Message, OthersButton, OthersReadCheck))
  {
    Message.Result = 1;
  }
  else
  {
    TFrame::Dispatch(&Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::CMCancelMode(TCMCancelMode & Message)
{
  if (FPopup && Visible && !FPopingContextMenu &&
      ((Message.Sender == NULL) ||
       (!IsAncestor(Message.Sender, this) &&
        !IsAncestor(Message.Sender, FPopupParent) &&
        (Message.Sender != this))))
  {
    CloseUp();
  }
  TFrame::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void TRightsFrame::CMDPIChanged(TMessage & Message)
{
  TFrame::Dispatch(&Message);
  UpdateButtons();
}
//---------------------------------------------------------------------------
void TRightsFrame::WMUpdateUIState(TMessage & Message)
{
  TFrame::Dispatch(&Message);
  UpdateButtons();
}
//---------------------------------------------------------------------------
void TRightsFrame::UpdateButton(TSpeedButton * Button, UnicodeString & Caption)
{
  if (Caption.IsEmpty())
  {
    Caption = Button->Caption;
    DebugAssert(Caption.Pos(L"&") >= 0);
  }
  int Padding = ScaleByTextHeight(Button, 4);
  std::unique_ptr<TCanvas> Canvas(CreateControlCanvas(Button));
  UnicodeString ACaption = Caption;
  UnicodeString StrippedCaption = StripHotkey(ACaption);
  if ((SendMessage(Handle, WM_QUERYUISTATE, 0, 0) & UISF_HIDEACCEL) != 0)
  {
    ACaption = StrippedCaption;
  }
  Button->Caption = ACaption;
  Button->Width = Padding + Canvas->TextWidth(StrippedCaption) + Padding;
  Button->Left = OctalLabel->Left - Padding;
}
//---------------------------------------------------------------------------
void TRightsFrame::UpdateButtons()
{
  UpdateButton(OwnerButton, FOwnerCaption);
  UpdateButton(GroupButton, FGroupCaption);
  UpdateButton(OthersButton, FOthersCaption);
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::Dispatch(void * Message)
{
  TMessage & AMessage = *static_cast<TMessage *>(Message);

  switch (AMessage.Msg)
  {
    case CM_CANCELMODE:
      CMCancelMode(*(TCMCancelMode *)Message);
      break;

    case CM_DIALOGKEY:
      CMDialogKey(*(TCMDialogKey *)Message);
      break;

    case CM_DIALOGCHAR:
      CMDialogChar(*(TCMDialogChar *)Message);
      break;

    case WM_CONTEXTMENU:
      WMContextMenu(*(TWMContextMenu *)Message);
      break;

    case CM_DPICHANGED:
      CMDPIChanged(AMessage);
      break;

    case WM_UPDATEUISTATE:
      WMUpdateUIState(AMessage);
      break;

    default:
      TFrame::Dispatch(Message);
      break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::DropDown()
{
  TCustomForm * Form = GetParentForm(this);
  // Due to lack of better idea, we clear "default" and "cancel" flags of respective
  // form buttons to prevent them to handle ESC/ENTER keys.
  // Could use FindStandardButton.
  for (int Index = 0; Index < Form->ControlCount; Index++)
  {
    TButton * Button = dynamic_cast<TButton *>(Form->Controls[Index]);
    if (Button != NULL)
    {
      if (Button->Default)
      {
        DebugAssert(FDefaultButton == NULL);
        FDefaultButton = Button;
        Button->Default = false;
      }

      if (Button->Cancel)
      {
        DebugAssert(FCancelButton == NULL);
        FCancelButton = Button;
        Button->Cancel = false;
      }
    }
  }

  TPoint Origin(PopupParent->Left, PopupParent->Top + PopupParent->Height);
  Origin = Parent->ScreenToClient(PopupParent->Parent->ClientToScreen(Origin));
  if (Origin.x + Width > Parent->ClientWidth)
  {
    Origin.x += PopupParent->Width - Width;
  }
  Left = Origin.x;
  Top = Origin.y;
  Show();
  SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::CloseUp()
{
  DebugAssert(FPopup);
  DebugAssert(Visible);
  if (!Focused())
  {
    // this can happen only if called from on-click handler of drop down button
    DoCloseUp();
  }
  FPopupParent->SetFocus();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TRightsFrame::GetText()
{
  UnicodeString Result = Rights.Text;
  if (AllowAddXToDirectories && DirectoriesXEffective() && AddXToDirectories)
  {
    Result = FORMAT(L"%s (%s)", (Result, FAddXToDirectoriesSuffix));
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetText(UnicodeString value)
{
  if (Text != value)
  {
    UnicodeString RightsStr = value;

    int P = RightsStr.LowerCase().Pos(FAddXToDirectoriesSuffix);
    bool AAddXToDirectories = (P > 0);
    if (AAddXToDirectories)
    {
      RightsStr.Delete(P, FAddXToDirectoriesSuffix.Length());
    }
    RightsStr = DeleteChar(DeleteChar(RightsStr, L'('), L')').Trim();
    TRights R = Rights;
    if (((RightsStr.Length() == 3) || (RightsStr.Length() == 4)) &&
        IsNumber(RightsStr))
    {
      R.Octal = RightsStr;
    }
    else
    {
      R.Text = RightsStr;
    }

    Rights = R;
    AddXToDirectories = AAddXToDirectories;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::RightsPopupPopup(TObject * /*Sender*/)
{
  if (!HasFocus())
  {
    SetFocus();
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::FrameContextPopup(TObject * Sender,
  TPoint & MousePos, bool & Handled)
{
  if (!FAcl)
  {
    SelectScaledImageList(RightsImages);
    MenuPopup(Sender, MousePos, Handled);
  }
  else
  {
    Handled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::UpdateByOctal()
{
  if (!OctalEdit->Text.IsEmpty())
  {
    TRights R = Rights;
    R.Octal = OctalEdit->Text;
    Rights = R;
  }
  UpdateControls();
  OctalEdit->Modified = false;
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::UpdateOctalEdit()
{
  TRights R = Rights;
  OctalEdit->Text = R.IsUndef ? UnicodeString() : R.Octal;
  OctalEdit->Modified = false;
  OctalEdit->SelectAll();
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::OctalEditChange(TObject *)
{
  if (OctalEdit->Modified && OctalEdit->Text.Length() >= 3)
  {
    try
    {
      UpdateByOctal();
    }
    catch(...)
    {
      OctalEdit->Modified = true;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::OctalEditExit(TObject *)
{
  if ((!Visible && DebugAlwaysTrue(Popup)) || // Popup assert: should happen only if popup is closed by esc key
      IsCancelButtonBeingClicked(this) || // CloseButton
      ((FCancelButton != NULL) && IsButtonBeingClicked(FCancelButton)))
  {
    // cancel changes
    ForceUpdate();
  }
  else if (OctalEdit->Modified)
  {
    // Now the text in OctalEdit is almost necessarily invalid, otherwise
    // OctalEditChange would have already cleared Modified flag
    try
    {
      UpdateByOctal();
    }
    catch(...)
    {
      OctalEdit->SelectAll();
      OctalEdit->SetFocus();
      throw;
    }
  }
  else
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::CloseButtonClick(TObject *)
{
  CloseUp();
}
//---------------------------------------------------------------------------
void TRightsFrame::DisplayAsAcl(TRights::TRight ReadRight, TRights::TRight WriteRight, TRights::TRight ExecRight, TRights::TRight SpecialRight)
{
  TCheckBox * ReadCheck = Checks[ReadRight];
  TCheckBox * WriteCheck = Checks[WriteRight];
  TCheckBox * ReadAclCheck = Checks[ExecRight];
  TCheckBox * WriteAclCheck = Checks[SpecialRight];

  WriteCheck->Visible = false;

  int ReadAclLeft = (3*ReadCheck->Left + 2*WriteAclCheck->Left) / 5;
  int Shift = ReadAclLeft - ReadAclCheck->Left;
  ReadAclCheck->Left = ReadAclLeft;
  ReadAclCheck->Width = ReadAclCheck->Width - Shift;
  ReadAclCheck->Caption = L"R ACL";
  ReadAclCheck->Hint = LoadStr(PROPERTIES_S3_R_ACL_HINT);

  WriteAclCheck->Caption = L"W ACL";
  WriteAclCheck->Hint = LoadStr(PROPERTIES_S3_W_ACL_HINT);
  WriteAclCheck->ShowHint = true;

  FGroupCaption = LoadStr(PROPERTIES_S3_USERS);
  FOthersCaption = LoadStr(PROPERTIES_S3_EVERYONE);
  UpdateButtons();
}
//---------------------------------------------------------------------------
void TRightsFrame::DisplayAsAcl()
{
  AllowAddXToDirectories = false;
  OctalLabel->Visible = false;
  OctalEdit->Visible = false;
  DisplayAsAcl(TRights::rrUserRead, TRights::rrUserWrite, TRights::rrUserExec, TRights::rrUserIDExec);
  DisplayAsAcl(TRights::rrGroupRead, TRights::rrGroupWrite, TRights::rrGroupExec, TRights::rrGroupIDExec);
  DisplayAsAcl(TRights::rrOtherRead, TRights::rrOtherWrite, TRights::rrOtherExec, TRights::rrStickyBit);
  FAcl = true;
}
