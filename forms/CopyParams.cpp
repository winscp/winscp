//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "CopyParams.h"

#include <VCLCommon.h>
#include <CoreMain.h>
#include "CustomWinConfiguration.h"
#include "TextsWin.h"
#include "Tools.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Rights"
#pragma link "HistoryComboBox"
#pragma link "ComboEdit"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
__fastcall TCopyParamsFrame::TCopyParamsFrame(TComponent* Owner)
        : TFrame(Owner)
{
  FRightsFrame = new TRightsExtFrame(this);
  FRightsFrame->TabStop = false;
  FRightsFrame->Parent = this;
  FRightsFrame->TabOrder = 1000;
  FRightsFrame->AllowAddXToDirectories = True;
  FRightsFrame->Popup = true;
  FRightsFrame->PopupParent = RightsEdit;
  FRightsFrame->OnChange = RightsFrameChange;
  RightsEdit->PopupMenu = FRightsFrame->RightsPopup;

  // on start set different value than we want to allow property-setter to proceed
  FDirection = pdToLocal;
  Direction = pdToRemote;

  FCopyParamAttrs = 0;
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

  FRightsFrame->AddXToDirectories = value.AddXToDirectories;
  FRightsFrame->Rights = value.Rights;
  PreserveRightsCheck->Checked = value.PreserveRights;
  IgnorePermErrorsCheck->Checked = value.IgnorePermErrors;
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

  Result.AddXToDirectories = FRightsFrame->AddXToDirectories;
  Result.Rights = FRightsFrame->Rights;
  Result.PreserveRights = PreserveRightsCheck->Checked;
  Result.IgnorePermErrors = IgnorePermErrorsCheck->Checked;
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
  EnableControl(CommonPropertiesGroup, FLAGCLEAR(CopyParamAttrs, cpaExcludeMaskOnly) && Enabled);
  EnableControl(LocalPropertiesGroup, FLAGCLEAR(CopyParamAttrs, cpaExcludeMaskOnly) && Enabled);
  EnableControl(RemotePropertiesGroup, FLAGCLEAR(CopyParamAttrs, cpaExcludeMaskOnly) && Enabled);
  EnableControl(TransferModeGroup,
    FLAGCLEAR(CopyParamAttrs, cpaNoTransferMode) &&
    FLAGCLEAR(CopyParamAttrs, cpaExcludeMaskOnly) && Enabled);
  EnableControl(AsciiFileMaskLabel,
    TransferModeGroup->Enabled && TMAutomaticButton->Checked);
  EnableControl(AsciiFileMaskCombo,
    TransferModeGroup->Enabled && TMAutomaticButton->Checked);
  EnableControl(PreserveRightsCheck, FLAGCLEAR(CopyParamAttrs, cpaExcludeMaskOnly) &&
    FLAGCLEAR(CopyParamAttrs, cpaNoRights) && Enabled);
  EnableControl(RightsEdit, PreserveRightsCheck->Checked &&
    PreserveRightsCheck->Enabled);
  EnableControl(FRightsFrame, RightsEdit->Enabled);
  EnableControl(PreserveReadOnlyCheck, FLAGCLEAR(CopyParamAttrs, cpaExcludeMaskOnly) &&
    FLAGCLEAR(CopyParamAttrs, cpaNoPreserveReadOnly) && Enabled);
  EnableControl(ExcludeFileMaskCombo,
    (FLAGCLEAR(CopyParamAttrs, cpaNoExcludeMask) ||
     FLAGSET(CopyParamAttrs, cpaExcludeMaskOnly)) &&
    Enabled);
  EnableControl(ExclusionFileMaskLabel, ExcludeFileMaskCombo->Enabled);
  EnableControl(NegativeExcludeCombo, ExcludeFileMaskCombo->Enabled);
  EnableControl(ClearArchiveCheck, FLAGCLEAR(CopyParamAttrs, cpaNoClearArchive) &&
    FLAGCLEAR(CopyParamAttrs, cpaExcludeMaskOnly) && Enabled);
  EnableControl(PreserveTimeCheck, FLAGCLEAR(CopyParamAttrs, cpaNoPreserveTime) &&
    FLAGCLEAR(CopyParamAttrs, cpaExcludeMaskOnly) && Enabled);
  EnableControl(ChangeCaseGroup, FLAGCLEAR(CopyParamAttrs, cpaExcludeMaskOnly) && Enabled);
  EnableControl(IgnorePermErrorsCheck,
    ((PreserveRightsCheck->Enabled && PreserveRightsCheck->Checked) ||
     (PreserveTimeCheck->Enabled && PreserveTimeCheck->Checked)) &&
    FLAGCLEAR(CopyParamAttrs, cpaNoIgnorePermErrors) &&
    FLAGCLEAR(CopyParamAttrs, cpaExcludeMaskOnly));
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
      IgnorePermErrorsCheck->Top = RemotePreserveTimeCheck->Top;
    }
    else
    {
      CCLowerCaseShortButton->Top = ReplaceInvalidCharsCheck->Top;
      IgnorePermErrorsCheck->Top = CCFirstUpperCaseButton->Top;
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
  // adding TRightsFrame on run-time corrupts the tab order, fix it
  TransferModeGroup->TabOrder = 0;
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
void __fastcall TCopyParamsFrame::SetCopyParamAttrs(int value)
{
  FCopyParamAttrs = value;
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
void __fastcall TCopyParamsFrame::RightsEditButtonClick(TObject * Sender)
{
  if (!FRightsFrame->Visible)
  {
    // validate input before showing the popup
    RightsEditExit(Sender);
    FRightsFrame->DropDown();
  }
  else
  {
    FRightsFrame->CloseUp();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::RightsFrameChange(TObject * /*Sender*/)
{
  RightsEdit->Text = FRightsFrame->Text;
  RightsEdit->Modified = false;
  RightsEdit->SelectAll();
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::UpdateRightsByStr()
{
  if (!RightsEdit->Text.IsEmpty())
  {
    FRightsFrame->Text = RightsEdit->Text;
    // change handler may not be called if the rights were not actually changed,
    // but we want to normalize the user-entered information anyway
    RightsFrameChange(NULL);
  }
  UpdateControls();
  RightsEdit->Modified = false;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::RightsEditExit(TObject * /*Sender*/)
{
  if (RightsEdit->Modified)
  {
    try
    {
      UpdateRightsByStr();
    }
    catch(...)
    {
      RightsEdit->SelectAll();
      RightsEdit->SetFocus();
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::RightsEditContextPopup(TObject * Sender,
  TPoint & MousePos, bool & Handled)
{
  MenuPopup(Sender, MousePos, Handled);
}
//---------------------------------------------------------------------------
