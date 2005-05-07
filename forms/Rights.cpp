//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Rights.h"

#include <Common.h>

#include <VCLCommon.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GrayedCheckBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TRightsFrame::TRightsFrame(TComponent* Owner)
        : TFrame(Owner)
{
  FOnChange = NULL;
  FAllowAddXToDirectories = true;
  // to avoid duplication of reference in forms that uses the frame
  PopupMenu = RightsPopup;
}
//---------------------------------------------------------------------------
__fastcall TRightsFrame::~TRightsFrame()
{
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetStates(TRights::TRight Right, TRights::TState value)
{
  assert(((value == TRights::rsNo) || (value == TRights::rsYes) || (value == TRights::rsUndef)));
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
  AllowUndef = true; // temporarily
  for (int Right = TRights::rrFirst; Right <= TRights::rrLast; Right++)
  {
    States[static_cast<TRights::TRight>(Right)] =
      value.RightUndef[static_cast<TRights::TRight>(Right)];
  }
  AllowUndef = value.AllowUndef;
  UpdateControls();
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
        assert(false);
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
void __fastcall TRightsFrame::UpdateControls()
{
  DirectoriesXCheck->Visible = AllowAddXToDirectories;
  EnableControl(DirectoriesXCheck,
    Enabled && !((Rights.NumberSet & TRights::rfExec) == TRights::rfExec));
  DoChange();
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::CycleRights(int Group)
{
  TRights::TState State;
  bool Same = true;
  for (int Right = 0; Right < 3; Right++)
  {
    TRights::TState CState = States[static_cast<TRights::TRight>(
      TRights::rrUserRead + Right + ((Group - 1) * 3))];

    if (Right == 0) State = CState;
      else
    if (State != CState) Same = False;
  }

  if (!Same)
  {
    State = TRights::rsYes;
  }
  else
  {
    switch (State) {
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

  for (int Right = 0; Right < 3; Right++)
  {
    States[static_cast<TRights::TRight>(
      TRights::rrUserRead + Right + ((Group - 1) * 3))] = State;
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::RightsButtonsClick(TObject *Sender)
{
  CycleRights(((TComponent*)Sender)->Tag);
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetAllowAddXToDirectories(bool value)
{
  if (FAllowAddXToDirectories != value)
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
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::RightsActionsExecute(TBasicAction * Action,
  bool & Handled)
{
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
  else
  {
    Handled = false;
  }
  Rights = R;
  ForceUpdate();
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::RightsActionsUpdate(TBasicAction *Action,
      bool &Handled)
{
  TRights R = Rights;

  Handled = true;
  if (Action == NoRightsAction)
  {
    NoRightsAction->Checked = !R.IsUndef && (R.NumberSet == TRights::rfNo);
  }
  else if (Action == DefaultRightsAction)
  {
    DefaultRightsAction->Checked = !R.IsUndef && (R.NumberSet == TRights::rfDefault);
  }
  else if (Action == AllRightsAction)
  {
    AllRightsAction->Checked = !R.IsUndef && (R.NumberSet == TRights::rfAll);
  }
  else if (Action == LeaveRightsAsIsAction)
  {
    LeaveRightsAsIsAction->Enabled = R.AllowUndef;
    LeaveRightsAsIsAction->Checked = (R.NumberSet == TRights::rfNo) &&
      (R.NumberUnset == TRights::rfNo);
  }
  else
  {
    Handled = false;
  }
}
//---------------------------------------------------------------------------

