//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Rights.h"
#include "NonVisual.h"

#include <Common.h>

#include <VCLCommon.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TRightsFrame::TRightsFrame(TComponent* Owner)
        : TFrame(Owner)
{
  FOnChange = NULL;
  FAllowAddXToDirectories = True;
}
//---------------------------------------------------------------------------
__fastcall TRightsFrame::~TRightsFrame()
{
  if (NonVisualDataModule->RightsFrame == this)
    NonVisualDataModule->RightsFrame = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetStates(TRightsFlag Flag, TRightState value)
{
  assert(((value == rsNo) || (value == rsYes) || (value == rsUndef)));
  TCheckBox *CheckBox = Checks[(TRightsFlag)Flag];
  switch (value) {
    case rsNo: CheckBox->State = cbUnchecked; break;
    case rsYes: CheckBox->State = cbChecked; break;
    case rsUndef: CheckBox->State = cbGrayed; break;
  }
}
//---------------------------------------------------------------------------
TRightState __fastcall TRightsFrame::GetStates(TRightsFlag Flag)
{
  switch (Checks[(TRightsFlag)Flag]->State) {
    case cbUnchecked: return rsNo;
    case cbChecked: return rsYes;
    case cbGrayed:
    default: return rsUndef;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetRights(TRights value)
{
  AllowUndef = true; // temporarily
  for (Integer Flag = 0; Flag < RightsFlagCount; Flag++)
    States[(TRightsFlag)Flag] = value.RightUndef[(TRightsFlag)Flag];
  AllowUndef = value.AllowUndef;
  UpdateControls();
}
//---------------------------------------------------------------------------
TRights __fastcall TRightsFrame::GetRights()
{
  TRights Result;
  Result.AllowUndef = AllowUndef;
  for (Integer Flag = 0; Flag < RightsFlagCount; Flag++)
    Result.RightUndef[(TRightsFlag)Flag] = States[(TRightsFlag)Flag];
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetAllowUndef(Boolean value)
{
  for (Integer Index = 0; Index < CheckCount; Index++)
    Checks[(TRightsFlag)Index]->AllowGrayed = value;
}
//---------------------------------------------------------------------------
Boolean __fastcall TRightsFrame::GetAllowUndef()
{
  Boolean Result = False, First = True;
  for (Integer Index = 0; Index < CheckCount; Index++)
  {
    TCheckBox *Check = Checks[(TRightsFlag)Index];
    if (First)
    {
      Result = Check->AllowGrayed;
      First = False;
    }
      else
    if (Result != Check->AllowGrayed)
      throw Exception("AllowGrayed property of all checkboxes of TRightsFrame don't have same value");
  }
  return Result;
}
//---------------------------------------------------------------------------
TCheckBox * __fastcall TRightsFrame::GetChecks(TRightsFlag Flag)
{
  assert((Flag >= 0) && (Flag < RightsFlagCount));
  for (Integer Index = 0; Index < ControlCount; Index++)
    if (Controls[Index]->InheritsFrom(__classid(TCheckBox)) &&
        ((Controls[Index]->Tag - 1) == Flag)) return ((TCheckBox *)Controls[Index]);
  assert(False);
  return NULL;
}
//---------------------------------------------------------------------------
Integer __fastcall TRightsFrame::GetCheckCount()
{
  return RightsFlagCount;
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetAddXToDirectories(Boolean value)
{
  DirectoriesXCheck->Checked = value;
}
//---------------------------------------------------------------------------
Boolean __fastcall TRightsFrame::GetAddXToDirectories()
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
    Enabled && !((Rights.NumberSet & raExecute) == raExecute));
  DoChange();
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::FrameContextPopup(TObject * /*Sender*/,
      TPoint & /*MousePos*/, bool & /*Handled*/)
{
  NonVisualDataModule->RightsFrame = this;
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::FrameEnter(TObject * /*Sender*/)
{
  // allow keyboard shortcuts
  NonVisualDataModule->RightsFrame = this;
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::CycleRights(Integer Group)
{
  TRightState State;
  Boolean Same = True;
  for (Integer Flag = 0; Flag < 3; Flag++)
  {
    TRightState CState = States[(TRightsFlag)(Flag + ((Group - 1) * 3))];
    if (Flag == 0) State = CState;
      else
    if (State != CState) Same = False;
  }
  if (!Same) State = rsYes;
    else
  switch (State) {
    case rsYes: State = rsNo; break;
    case rsNo: if (AllowUndef) State = rsUndef; else State = rsYes; break;
    case rsUndef: State = rsYes; break;
  }
  for (Integer Flag = 0; Flag < 3; Flag++)
    States[(TRightsFlag)(Flag + ((Group - 1) * 3))] = State;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::RightsButtonsClick(TObject *Sender)
{
  CycleRights(((TComponent*)Sender)->Tag);
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetAllowAddXToDirectories(Boolean value)
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
  if (FOnChange) FOnChange(this);
}
//---------------------------------------------------------------------------
void __fastcall TRightsFrame::SetEnabled(Boolean Value)
{
  TFrame::SetEnabled(Value);
  UpdateControls();
}




