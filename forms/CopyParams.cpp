//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "CopyParams.h"

#include <Common.h>
#include <VCLCommon.h>
#include <ScpMain.h>
#include "CustomWinConfiguration.h"
#include "TextsWin.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Rights"
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TCopyParamsFrame::TCopyParamsFrame(TComponent* Owner)
        : TFrame(Owner)
{
  // on start set different value than we want to allow property-setter to proceed
  FDirection = pdToLocal;
  Direction = pdToRemote;

  FForcePreserveTime = false;
  FAllowTransferMode = True;
  RightsFrame->AllowAddXToDirectories = True;
  FParams = new TCopyParamType();
  TCopyParamType DefParams;
  Params = DefParams;
}
//---------------------------------------------------------------------------
__fastcall TCopyParamsFrame::~TCopyParamsFrame()
{
  delete FParams;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::SetParams(TCopyParamType value)
{
  assert((value.TransferMode == tmAscii) ||
    (value.TransferMode == tmBinary) || (value.TransferMode == tmAutomatic));
  switch (value.TransferMode) {
    case tmAscii: TMTextButton->Checked = True; break;
    case tmBinary: TMBinaryButton->Checked = True; break;
    default: TMAutomaticButton->Checked = True; break;
  }
  FOrigMasks = value.AsciiFileMask.Masks;
  AsciiFileMaskCombo->Text = value.AsciiFileMask.Masks;

  switch (value.FileNameCase) {
    case ncNoChange: CCNoChangeButton->Checked = True; break;
    case ncLowerCase: CCLowerCaseButton->Checked = True; break;
    case ncUpperCase: CCUpperCaseButton->Checked = True; break;
    case ncFirstUpperCase: CCFirstUpperCaseButton->Checked = True; break;
  }

  ReplaceInvalidCharsCheck->Checked = value.ReplaceInvalidChars;

  RightsFrame->AddXToDirectories = value.AddXToDirectories;
  RightsFrame->Rights = value.Rights;
  PreserveRightsCheck->Checked = value.PreserveRights;
  PreserveReadOnlyCheck->Checked = value.PreserveReadOnly;

  assert(PreserveTimeCheck);
  PreserveTimeCheck->Checked = value.PreserveTime;

  CommonCalculateSizeCheck->Checked = value.CalculateSize;

  *FParams = value;

  UpdateControls();
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyParamsFrame::GetAsciiFileMask()
{
  return AsciiFileMaskCombo->Text;
}
//---------------------------------------------------------------------------
TCopyParamType __fastcall TCopyParamsFrame::GetParams()
{
  TCopyParamType Result = *FParams;

  assert(TMTextButton->Checked || TMBinaryButton->Checked || TMAutomaticButton->Checked);
  if (TMTextButton->Checked) Result.TransferMode = tmAscii;
    else
  if (TMBinaryButton->Checked) Result.TransferMode = tmBinary;
    else Result.TransferMode = tmAutomatic;

  Result.AsciiFileMask.Masks = AsciiFileMaskCombo->Text;
  if (!Result.AsciiFileMask.IsValid()) Result.AsciiFileMask.Masks = FOrigMasks;

  if (CCLowerCaseButton->Checked) Result.FileNameCase = ncLowerCase;
    else
  if (CCUpperCaseButton->Checked) Result.FileNameCase = ncUpperCase;
    else
  if (CCFirstUpperCaseButton->Checked) Result.FileNameCase = ncFirstUpperCase;
    else Result.FileNameCase = ncNoChange;

  Result.ReplaceInvalidChars = ReplaceInvalidCharsCheck->Checked;

  Result.AddXToDirectories = RightsFrame->AddXToDirectories;
  Result.Rights = RightsFrame->Rights;
  Result.PreserveRights = PreserveRightsCheck->Checked;
  Result.PreserveReadOnly = PreserveReadOnlyCheck->Checked;

  assert(PreserveTimeCheck);
  Result.PreserveTime = PreserveTimeCheck->Checked;

  Result.CalculateSize = CommonCalculateSizeCheck->Checked;

  return Result;
}
//---------------------------------------------------------------------------
TCheckBox * __fastcall TCopyParamsFrame::GetPreserveTimeCheck()
{
  switch (Direction) {
    case pdToRemote: return RemotePreserveTimeCheck;
    case pdToLocal: return LocalPreserveTimeCheck;
    case pdBoth:
    default: return CommonPreserveTimestampCheck;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::UpdateControls()
{
  EnableControl(TransferModeGroup, FAllowTransferMode && Enabled);
  EnableControl(AsciiFileMaskLabel,
    FAllowTransferMode && TMAutomaticButton->Checked && Enabled);
  EnableControl(AsciiFileMaskCombo,
    FAllowTransferMode && TMAutomaticButton->Checked && Enabled);
  EnableControl(RightsFrame, PreserveRightsCheck->Checked && Enabled);
  EnableControl(ReplaceInvalidCharsCheck,
    Direction == pdToLocal || Direction == pdBoth || Direction == pdAll);
  EnableControl(PreserveTimeCheck, !ForcePreserveTime);
  if (ForcePreserveTime)
  {
    PreserveTimeCheck->Checked = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::SetDirection(TParamsForDirection value)
{
  if (Direction != value)
  {
    Boolean APreserveTime = PreserveTimeCheck->Checked;
    FDirection = value;
    PreserveTimeCheck->Checked = APreserveTime;

    LocalPropertiesGroup->Visible = (Direction == pdToLocal || Direction == pdAll );
    RemotePropertiesGroup->Visible = (Direction == pdToRemote || Direction == pdAll );
    CommonPropertiesGroup->Visible = (Direction == pdBoth || Direction == pdAll );
    LocalPreserveTimeCheck->Visible = (Direction != pdAll);
    RemotePreserveTimeCheck->Visible = (Direction != pdAll);
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::BeforeExecute()
{
  assert(CustomWinConfiguration);
  AsciiFileMaskCombo->Items = CustomWinConfiguration->History["Mask"];
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::AfterExecute()
{
  assert(CustomWinConfiguration);
  AsciiFileMaskCombo->SaveToHistory();
  CustomWinConfiguration->History["Mask"] = AsciiFileMaskCombo->Items;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::SelectMask(Integer Start, Integer Length)
{
  AsciiFileMaskCombo->SetFocus();
  AsciiFileMaskCombo->SelStart = Start;
  AsciiFileMaskCombo->SelLength = Length;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::SetAllowTransferMode(Boolean value)
{
  FAllowTransferMode = value;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::SetForcePreserveTime(bool value)
{
  FForcePreserveTime = value;
  UpdateControls();
}
//---------------------------------------------------------------------------
Boolean __fastcall TCopyParamsFrame::GetAllowTransferMode()
{
  return FAllowTransferMode;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::SetEnabled(Boolean Value)
{
  TFrame::SetEnabled(Value);
  UpdateControls();
}

