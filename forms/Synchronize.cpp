//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Interface.h>
#include <ScpMain.h>
#include <TextsWin.h>
#include <VCLCommon.h>

#include "Synchronize.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------
#pragma link "CopyParams"
#pragma link "MoreButton"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
void __fastcall DoSynchronizeDialog(TSynchronizeParamType Params,
  TSynchronizeStartStopEvent OnStartStop)
{
  TSynchronizeDialog * Dialog = new TSynchronizeDialog(Application);
  try
  {
    Dialog->OnStartStop = OnStartStop;
    Dialog->Params = Params;
    Dialog->Execute();
  }
  __finally
  {
    delete Dialog;
  }
}
//---------------------------------------------------------------------
TSynchronizeParamType __fastcall TSynchronizeParamType::operator =(TSynchronizeParamType rhp)
{
  Assign(rhp);
  return *this;
}
//---------------------------------------------------------------------
void __fastcall TSynchronizeParamType::Assign(TSynchronizeParamType Source)
{
  CopyParams.Assign(Source.CopyParams);
  AllowTransferMode = Source.AllowTransferMode;
  LocalDirectory = Source.LocalDirectory;
  RemoteDirectory = Source.RemoteDirectory;
}
//---------------------------------------------------------------------
__fastcall TSynchronizeDialog::TSynchronizeDialog(TComponent* AOwner)
	: TForm(AOwner)
{
  FParams.AllowTransferMode = True;
  FSynchronizing = False;
  FMinimizedByMe = False;
  UseSystemFont(this);
  CopyParamsFrame->Direction = pdToRemote;
}
//---------------------------------------------------------------------------
bool __fastcall TSynchronizeDialog::Execute()
{
  SaveSettingsCheck->Checked = False;
  MoreButton->Expanded = WinConfiguration->CopyParamDialogExpanded;
  CopyParamsFrame->BeforeExecute();
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    CopyParamsFrame->AfterExecute();
    Configuration->BeginUpdate();
    try
    {
      WinConfiguration->CopyParamDialogExpanded = MoreButton->Expanded;
      if (SaveSettingsCheck->Checked)
      {
        Configuration->CopyParam = Params.CopyParams;
      }
    }
    __finally
    {
      Configuration->EndUpdate();
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::SetParams(TSynchronizeParamType value)
{
  FParams = value;
  CopyParamsFrame->Params = FParams.CopyParams;
}
//---------------------------------------------------------------------------
TSynchronizeParamType __fastcall TSynchronizeDialog::GetParams()
{
  FParams.CopyParams = CopyParamsFrame->Params;
  return FParams;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::UpdateControls()
{
  EnableControl(StartButton, !FSynchronizing);
  StartButton->Default = !FSynchronizing;
  EnableControl(StopButton, FSynchronizing);
  StopButton->Default = FSynchronizing;
  EnableControl(MinimizeButton, FSynchronizing);
  EnableControl(CloseButton, !FSynchronizing);
  EnableControl(MorePanel, !FSynchronizing);
  CopyParamsFrame->AllowTransferMode = FParams.AllowTransferMode;

  int Msg = (FSynchronizing ? SYCHRONIZE_WAITING : SYCHRONIZE_DESCRIPTION);
  StatusLabel->Caption = FMTLOAD(Msg, (FParams.LocalDirectory, FParams.RemoteDirectory));
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::FormShow(TObject * /*Sender*/)
{
  if (MoreButton->Expanded) MorePanel->SetFocus();
    else StartButton->SetFocus();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::Validate()
{
  if (Params.CopyParams.TransferMode == tmAutomatic)
  {
    TFileMasks Masks = CopyParamsFrame->AsciiFileMask;
    int Start, Length;
    if (!Masks.IsValid(Start, Length))
    {
      MoreButton->Expanded = True;
      // After closing dialog whole text is selected, we want to select only invalid mask
      CopyParamsFrame->SelectMask(Start, Length);
      throw Exception(FMTLOAD(MASK_ERROR, (Masks.Masks.SubString(Start+1, Length))));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::DoStartStop(Boolean Start,
  TSynchronizeParamType Params)
{
  if (FOnStartStop) FOnStartStop(this, Start, Params);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::FormCloseQuery(TObject * /*Sender*/,
      bool &CanClose)
{
  CanClose = !FSynchronizing;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::StartButtonClick(TObject * /*Sender*/)
{
  try
  {
    FSynchronizing = true;
    Validate();
    UpdateControls();
    DoStartStop(True, Params);
  }
  catch(...)
  {
    FSynchronizing = false;
    UpdateControls();
    throw;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::StopButtonClick(TObject * /*Sender*/)
{
  Stop();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::Stop()
{
  FSynchronizing = false;
  DoStartStop(false, Params);
  UpdateControls();
  if (IsIconic(Application->Handle) && FMinimizedByMe)
  {
    FMinimizedByMe = false;
    Application->Restore();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::MinimizeApp()
{
  Application->Minimize();
  FMinimizedByMe = true;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::MinimizeButtonClick(TObject * /*Sender*/)
{
  MinimizeApp();
}
//---------------------------------------------------------------------------
