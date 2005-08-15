//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "CopyParams.h"

#include <VCLCommon.h>
#include <ScpMain.h>
#include "CustomWinConfiguration.h"
#include "TextsWin.h"
#include "Tools.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Rights"
#pragma link "HistoryComboBox"
#pragma link "XPThemes"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TCopyParamsFrame::TCopyParamsFrame(TComponent* Owner)
        : TFrame(Owner)
{
  // on start set different value than we want to allow property-setter to proceed
  FDirection = pdToLocal;
  Direction = pdToRemote;

  FOptions = cfAllowTransferMode | cfAllowExcludeMask | cfAllowClearArchive;
  RightsFrame->AllowAddXToDirectories = True;
  FParams = new TCopyParamType();
  TCopyParamType DefParams;
  Params = DefParams;

  InstallPathWordBreakProc(AsciiFileMaskCombo);
  InstallPathWordBreakProc(ExcludeFileMaskCombo);
  HintLabel(ExcludeFileMaskHintText,
    FORMAT("%s\n \n%s",(LoadStr(MASK_HINT), LoadStr(PATH_MASK_HINT))));
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
    case ncLowerCaseShort: CCLowerCaseShortButton->Checked = True; break;
  }

  ReplaceInvalidCharsCheck->Checked =
    (value.InvalidCharsReplacement != TCopyParamType::NoReplacement);

  RightsFrame->AddXToDirectories = value.AddXToDirectories;
  RightsFrame->Rights = value.Rights;
  PreserveRightsCheck->Checked = value.PreserveRights;
  PreserveReadOnlyCheck->Checked = value.PreserveReadOnly;

  assert(PreserveTimeCheck);
  PreserveTimeCheck->Checked = value.PreserveTime;

  CommonCalculateSizeCheck->Checked = value.CalculateSize;

  NegativeExcludeCombo->ItemIndex = (value.NegativeExclude ? 1 : 0);
  ExcludeFileMaskCombo->Text = value.ExcludeFileMask.Masks;
  ClearArchiveCheck->Checked = value.ClearArchive;

  *FParams = value;

  UpdateControls();
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
    else
  if (CCLowerCaseShortButton->Checked) Result.FileNameCase = ncLowerCaseShort;
    else Result.FileNameCase = ncNoChange;

  Result.ReplaceInvalidChars = ReplaceInvalidCharsCheck->Checked;

  Result.AddXToDirectories = RightsFrame->AddXToDirectories;
  Result.Rights = RightsFrame->Rights;
  Result.PreserveRights = PreserveRightsCheck->Checked;
  Result.PreserveReadOnly = PreserveReadOnlyCheck->Checked;

  assert(PreserveTimeCheck);
  Result.PreserveTime = PreserveTimeCheck->Checked;

  Result.CalculateSize = CommonCalculateSizeCheck->Checked;

  Result.ExcludeFileMask.Masks = ExcludeFileMaskCombo->Text;
  Result.NegativeExclude = (NegativeExcludeCombo->ItemIndex == 1);

  Result.ClearArchive = ClearArchiveCheck->Checked;

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
  EnableControl(CommonPropertiesGroup, FLAGCLEAR(Options, cfAllowExcludeMaskOnly) && Enabled);
  EnableControl(LocalPropertiesGroup, FLAGCLEAR(Options, cfAllowExcludeMaskOnly) && Enabled);
  EnableControl(RemotePropertiesGroup, FLAGCLEAR(Options, cfAllowExcludeMaskOnly) && Enabled);
  EnableControl(TransferModeGroup,
    FLAGSET(Options, cfAllowTransferMode) &&
    FLAGCLEAR(Options, cfAllowExcludeMaskOnly) && Enabled);
  EnableControl(AsciiFileMaskLabel,
    TransferModeGroup->Enabled && TMAutomaticButton->Checked);
  EnableControl(AsciiFileMaskCombo,
    TransferModeGroup->Enabled && TMAutomaticButton->Checked);
  EnableControl(PreserveRightsCheck, FLAGCLEAR(Options, cfAllowExcludeMaskOnly) && Enabled);
  EnableControl(RightsFrame, PreserveRightsCheck->Checked &&
    PreserveRightsCheck->Enabled);
  EnableControl(ExcludeFileMaskCombo,
    (FLAGSET(Options, cfAllowExcludeMask) || FLAGSET(Options, cfAllowExcludeMaskOnly)) &&
    Enabled);
  EnableControl(ExclusionFileMaskLabel, ExcludeFileMaskCombo->Enabled);
  EnableControl(NegativeExcludeCombo, ExcludeFileMaskCombo->Enabled);
  EnableControl(ClearArchiveCheck, FLAGSET(Options, cfAllowClearArchive) &&
    FLAGCLEAR(Options, cfAllowExcludeMaskOnly) && Enabled);
  EnableControl(PreserveTimeCheck, FLAGCLEAR(Options, cfDisablePreserveTime) &&
    FLAGCLEAR(Options, cfAllowExcludeMaskOnly) && Enabled);
  EnableControl(ChangeCaseGroup, FLAGCLEAR(Options, cfAllowExcludeMaskOnly) && Enabled);
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
    ReplaceInvalidCharsCheck->Visible =
      (Direction == pdToLocal || Direction == pdBoth || Direction == pdAll);
    CCFirstUpperCaseButton->Visible =
      (Direction == pdToLocal || Direction == pdToRemote);
    CCLowerCaseShortButton->Visible =
      (Direction == pdToRemote || Direction == pdBoth || Direction == pdAll);
    if (Direction == pdBoth || Direction == pdAll)
    {
      CCLowerCaseShortButton->Top = CCFirstUpperCaseButton->Top;
    }
    else
    {
      CCLowerCaseShortButton->Top = ReplaceInvalidCharsCheck->Top;
    }
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
  ExcludeFileMaskCombo->Items = CustomWinConfiguration->History["ExcludeMask"];
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::AfterExecute()
{
  assert(CustomWinConfiguration);
  AsciiFileMaskCombo->SaveToHistory();
  CustomWinConfiguration->History["Mask"] = AsciiFileMaskCombo->Items;
  ExcludeFileMaskCombo->SaveToHistory();
  CustomWinConfiguration->History["ExcludeMask"] = ExcludeFileMaskCombo->Items;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::Validate()
{
  if (AsciiFileMaskCombo->Focused())
  {
    ValidateMaskEdit(AsciiFileMaskCombo);
  }
  if (ExcludeFileMaskCombo->Focused())
  {
    ValidateMaskEdit(ExcludeFileMaskCombo);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::SetOptions(int value)
{
  FOptions = value;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::SetEnabled(Boolean Value)
{
  TFrame::SetEnabled(Value);
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::ValidateMaskComboExit(TObject * Sender)
{
  ValidateMaskEdit(dynamic_cast<TComboBox*>(Sender));
}
//---------------------------------------------------------------------------

