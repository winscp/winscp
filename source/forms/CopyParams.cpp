//---------------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include "CopyParams.h"
//---------------------------------------------------------------------------
#pragma link "Rights"
#pragma link "HistoryComboBox"
#pragma link "ComboEdit"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TCopyParamsFrame::TCopyParamsFrame(TComponent* Owner)
        : TFrame(Owner)
{
  FRightsFrame = new TRightsFrame(this);
  FRightsFrame->TabStop = false;
  FRightsFrame->Parent = this;
  FRightsFrame->TabOrder = 1000;
  FRightsFrame->AllowAddXToDirectories = True;
  FRightsFrame->Popup = true;
  FRightsFrame->PopupParent = RightsEdit;
  FRightsFrame->OnChange = RightsFrameChange;
  RightsEdit->PopupMenu = FRightsFrame->RightsPopup;

  FCopyParamAttrs = 0;
  FParams = new TCopyParamType();
  TCopyParamType DefParams;
  Params = DefParams;

  HintLabel(IncludeFileMaskHintText,
    FORMAT(L"%s\n \n%s\n \n%s\n \n%s\n \n%s\n \n%s", (LoadStr(MASK_HINT2),
      LoadStr(FILE_MASK_EX_HINT), LoadStr(COMBINING_MASKS_HINT),
      LoadStr(PATH_MASK_HINT2),
      LoadStr(DIRECTORY_MASK_HINT), LoadStr(MASK_HELP))));
}
//---------------------------------------------------------------------------
__fastcall TCopyParamsFrame::~TCopyParamsFrame()
{
  delete FParams;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::SetParams(TCopyParamType value)
{
  DebugAssert((value.TransferMode == tmAscii) ||
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
    case ncFirstUpperCase: CCNoChangeButton->Checked = True; break; // unsupported
    case ncLowerCaseShort: CCLowerCaseShortButton->Checked = True; break;
  }

  ReplaceInvalidCharsCheck->Checked = value.ReplaceInvalidChars;

  FRightsFrame->AddXToDirectories = value.AddXToDirectories;
  FRightsFrame->Rights = value.Rights;
  PreserveRightsCheck->Checked = value.PreserveRights;
  IgnorePermErrorsCheck->Checked = value.IgnorePermErrors;
  PreserveReadOnlyCheck->Checked = value.PreserveReadOnly;

  PreserveTimeCheck->Checked = value.PreserveTime;
  PreserveTimeDirsCheck->Checked = value.PreserveTimeDirs;

  CommonCalculateSizeCheck->Checked = value.CalculateSize;

  IncludeFileMaskCombo->Text = value.IncludeFileMask.Masks;
  ClearArchiveCheck->Checked = value.ClearArchive;
  if (FLAGSET(CopyParamAttrs, cpaNoRemoveCtrlZ))
  {
    FRemoveCtrlZ = value.RemoveCtrlZ;
    RemoveCtrlZAndBOMCheck->Checked = value.RemoveBOM;
  }
  else
  {
    DebugAssert(FLAGCLEAR(CopyParamAttrs, cpaNoRemoveBOM));
    RemoveCtrlZAndBOMCheck->Checked = value.RemoveCtrlZ && value.RemoveBOM;
  }

  SpeedCombo->Text = SetSpeedLimit(value.CPSLimit);

  NewerOnlyCheck->Checked = value.NewerOnly;
  EncryptNewFilesCheck->Checked = value.EncryptNewFiles;
  ExcludeHiddenFilesCheck->Checked = value.ExcludeHiddenFiles;
  ExcludeEmptyDirectoriesCheck->Checked = value.ExcludeEmptyDirectories;

  *FParams = value;

  UpdateControls();
}
//---------------------------------------------------------------------------
TCopyParamType __fastcall TCopyParamsFrame::GetParams()
{
  TCopyParamType Result = *FParams;

  DebugAssert(TMTextButton->Checked || TMBinaryButton->Checked || TMAutomaticButton->Checked);
  if (TMTextButton->Checked) Result.TransferMode = tmAscii;
    else
  if (TMBinaryButton->Checked) Result.TransferMode = tmBinary;
    else Result.TransferMode = tmAutomatic;

  Result.AsciiFileMask.Masks = AsciiFileMaskCombo->Text;

  if (CCLowerCaseButton->Checked) Result.FileNameCase = ncLowerCase;
    else
  if (CCUpperCaseButton->Checked) Result.FileNameCase = ncUpperCase;
    else
  if (CCLowerCaseShortButton->Checked) Result.FileNameCase = ncLowerCaseShort;
    else Result.FileNameCase = ncNoChange;

  Result.ReplaceInvalidChars = ReplaceInvalidCharsCheck->Checked;

  Result.AddXToDirectories = FRightsFrame->AddXToDirectories;
  Result.Rights = FRightsFrame->Rights;
  Result.PreserveRights = PreserveRightsCheck->Checked;
  Result.IgnorePermErrors = IgnorePermErrorsCheck->Checked;
  Result.PreserveReadOnly = PreserveReadOnlyCheck->Checked;

  Result.PreserveTime = PreserveTimeCheck->Checked;
  Result.PreserveTimeDirs = PreserveTimeDirsCheck->Checked;

  Result.CalculateSize = CommonCalculateSizeCheck->Checked;

  Result.IncludeFileMask.Masks = IncludeFileMaskCombo->Text;

  Result.ClearArchive = ClearArchiveCheck->Checked;
  if (FLAGSET(CopyParamAttrs, cpaNoRemoveCtrlZ))
  {
    Result.RemoveCtrlZ = FRemoveCtrlZ;
  }
  else
  {
    Result.RemoveCtrlZ = RemoveCtrlZAndBOMCheck->Checked;
  }
  Result.RemoveBOM = RemoveCtrlZAndBOMCheck->Checked;

  Result.CPSLimit = GetSpeedLimit(SpeedCombo->Text);

  Result.NewerOnly = NewerOnlyCheck->Checked;
  Result.EncryptNewFiles = EncryptNewFilesCheck->Checked;
  Result.ExcludeHiddenFiles = ExcludeHiddenFilesCheck->Checked;
  Result.ExcludeEmptyDirectories = ExcludeEmptyDirectoriesCheck->Checked;

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::UpdateControls()
{
  EnableControl(CommonPropertiesGroup, FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) && Enabled);
  EnableControl(LocalPropertiesGroup, FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) && Enabled);
  EnableControl(RemotePropertiesGroup, FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) && Enabled);
  EnableControl(TransferModeGroup,
    FLAGCLEAR(CopyParamAttrs, cpaNoTransferMode) &&
    FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) && Enabled);
  EnableControl(AsciiFileMaskLabel,
    TransferModeGroup->Enabled && TMAutomaticButton->Checked);
  EnableControl(AsciiFileMaskCombo,
    TransferModeGroup->Enabled && TMAutomaticButton->Checked);
  EnableControl(PreserveRightsCheck, FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) &&
    FLAGCLEAR(CopyParamAttrs, cpaNoRights) && Enabled);
  EnableControl(RightsEdit, PreserveRightsCheck->Checked &&
    PreserveRightsCheck->Enabled);
  EnableControl(FRightsFrame, RightsEdit->Enabled);
  EnableControl(PreserveReadOnlyCheck, FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) &&
    FLAGCLEAR(CopyParamAttrs, cpaNoPreserveReadOnly) && Enabled);
  EnableControl(IncludeFileMaskCombo,
    (FLAGCLEAR(CopyParamAttrs, cpaNoIncludeMask) ||
     FLAGSET(CopyParamAttrs, cpaIncludeMaskOnly)) &&
    Enabled);
  EnableControl(IncludeFileMaskLabel, IncludeFileMaskCombo->Enabled);
  EnableControl(ClearArchiveCheck, FLAGCLEAR(CopyParamAttrs, cpaNoClearArchive) &&
    FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) && Enabled);
  // Do not mention EOF when not supported, but only when at least BOM is supported.
  // This is not ideal as it shows both BOM and EOF for FTP downloads
  // (when EOF is never supported for FTP).
  // But as the option is disabled for FTP download, it's not that bad.
  // We do not have enough information here to implement this correctly.
  RemoveCtrlZAndBOMCheck->Caption =
    LoadStr(
      FLAGSET(CopyParamAttrs, cpaNoRemoveCtrlZ) && FLAGCLEAR(CopyParamAttrs, cpaNoRemoveBOM) ?
        COPY_PARAM_REMOVE_BOM : COPY_PARAM_REMOVE_BOM_EOF);
  EnableControl(RemoveCtrlZAndBOMCheck,
    (FLAGCLEAR(CopyParamAttrs, cpaNoRemoveCtrlZ) || FLAGCLEAR(CopyParamAttrs, cpaNoRemoveBOM)) &&
    FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) &&
    (TMTextButton->Checked || TMAutomaticButton->Checked) &&
    Enabled);
  EnableControl(PreserveTimeCheck, FLAGCLEAR(CopyParamAttrs, cpaNoPreserveTime) &&
    FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) && Enabled);
  // The checkbox can be enabled even then "preserve time" checkbox is not (such as on the Synchronize dialog);
  EnableControl(PreserveTimeDirsCheck,
    FLAGCLEAR(CopyParamAttrs, cpaNoPreserveTimeDirs) &&
    FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) && Enabled &&
    PreserveTimeCheck->Checked);
  EnableControl(CommonCalculateSizeCheck,
    FLAGCLEAR(CopyParamAttrs, cpaNoCalculateSize) && FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) && Enabled);
  EnableControl(ChangeCaseGroup, FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) && Enabled);
  EnableControl(IgnorePermErrorsCheck,
    ((PreserveRightsCheck->Enabled && PreserveRightsCheck->Checked) ||
     (PreserveTimeCheck->Enabled && PreserveTimeCheck->Checked)) &&
    FLAGCLEAR(CopyParamAttrs, cpaNoIgnorePermErrors) &&
    FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly));
  EnableControl(NewerOnlyCheck, FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) &&
    FLAGCLEAR(CopyParamAttrs, cpaNoNewerOnly) && Enabled);
  EnableControl(EncryptNewFilesCheck, FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) &&
    FLAGCLEAR(CopyParamAttrs, cpaNoEncryptNewFiles) && Enabled);
  EnableControl(ExcludeHiddenFilesCheck, IncludeFileMaskCombo->Enabled);
  EnableControl(ExcludeEmptyDirectoriesCheck, FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly));
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::BeforeExecute()
{
  // only now is IncludeFileMaskHintText->Width updated to match actual caption
  // (localized one)
  TPoint P(IncludeFileMaskCombo->Width, 0);
  P = IncludeFileMaskHintText->ScreenToClient(IncludeFileMaskCombo->ClientToScreen(P));
  IncludeFileMaskHintText->Left = IncludeFileMaskHintText->Left + P.x - IncludeFileMaskHintText->Width;

  // adding TRightsFrame on run-time corrupts the tab order, fix it
  TransferModeGroup->TabOrder = 0;
  DebugAssert(CustomWinConfiguration);
  CopySpeedLimits(CustomWinConfiguration->History[L"SpeedLimit"], SpeedCombo->Items);
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::AfterExecute()
{
  DebugAssert(CustomWinConfiguration);
  AsciiFileMaskCombo->SaveToHistory();
  IncludeFileMaskCombo->SaveToHistory();
  SpeedCombo->SaveToHistory();
  CustomWinConfiguration->History[L"SpeedLimit"] = SpeedCombo->Items;
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
  if (RightsEdit->Modified && !IsCancelButtonBeingClicked(this))
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
void __fastcall TCopyParamsFrame::SpeedComboExit(TObject * /*Sender*/)
{
  if (!IsCancelButtonBeingClicked(this))
  {
    try
    {
      SpeedCombo->Text = SetSpeedLimit(GetSpeedLimit(SpeedCombo->Text));
    }
    catch (Exception & E)
    {
      ShowExtendedException(&E);
      SpeedCombo->SetFocus();
      SpeedCombo->SelectAll();
      Abort();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::IncludeFileMaskButtonClick(TObject * /*Sender*/)
{
  TFileMasks Masks = IncludeFileMaskCombo->Text;
  if (DoEditMaskDialog(Masks))
  {
    IncludeFileMaskCombo->Text = Masks.Masks;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamsFrame::CreateWnd()
{
  TFrame::CreateWnd();

  if (AsciiFileMaskCombo != NULL)
  {
    InstallPathWordBreakProc(AsciiFileMaskCombo);
  }
  if (IncludeFileMaskCombo != NULL)
  {
    InstallPathWordBreakProc(IncludeFileMaskCombo);
  }
}
//---------------------------------------------------------------------------
