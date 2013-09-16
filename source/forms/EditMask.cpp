//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <FileMasks.h>
#include <CoreMain.h>
#include <TextsWin.h>
#include <Tools.h>
#include <VCLCommon.h>

#include "EditMask.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
bool __fastcall DoEditMaskDialog(TFileMasks & Mask)
{
  bool Result;
  TEditMaskDialog * Dialog = new TEditMaskDialog(GetFormOwner());
  try
  {
    Result = Dialog->Execute(Mask);
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TEditMaskDialog::TEditMaskDialog(TComponent* Owner)
        : TForm(Owner)
{
  UseSystemSettings(this);
  HintLabel(MaskHintText,
    FORMAT(L"%s\n \n%s\n \n%s\n \n%s", (LoadStr(MASK_HINT2), LoadStr(FILE_MASK_EX_HINT), LoadStr(PATH_MASK_HINT2), LoadStr(MASK_HELP))));
  ReadOnlyControl(MaskMemo);
}
//---------------------------------------------------------------------------
void __fastcall TEditMaskDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult == DefaultResult(this))
  {
    ExitActiveControl(this);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TEditMaskDialog::Execute(TFileMasks & Mask)
{
  LoadFileMasks(Mask);
  bool Result = (ShowModal() == DefaultResult(this));
  if (Result)
  {
    SaveFileMasks(Mask);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TEditMaskDialog::LoadFileMasks(const TFileMasks & Mask)
{
  LoadFileMasks(IncludeFileMasksMemo, Mask.IncludeFileMasksStr);
  LoadFileMasks(ExcludeFileMasksMemo, Mask.ExcludeFileMasksStr);
  LoadFileMasks(IncludeDirectoryMasksMemo, Mask.IncludeDirectoryMasksStr);
  LoadFileMasks(ExcludeDirectoryMasksMemo, Mask.ExcludeDirectoryMasksStr);
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TEditMaskDialog::LoadFileMasks(TMemo * Memo, TStrings * MasksStr)
{
  Memo->Lines = MasksStr;
}
//---------------------------------------------------------------------------
void __fastcall TEditMaskDialog::SaveFileMasks(TFileMasks & Mask)
{
  TStrings * IncludeFileMasks = NULL;
  TStrings * ExcludeFileMasks = NULL;
  TStrings * IncludeDirectoryMasks = NULL;
  TStrings * ExcludeDirectoryMasks = NULL;
  try
  {
    IncludeFileMasks = GetUnwrappedMemoLines(IncludeFileMasksMemo);
    ExcludeFileMasks = GetUnwrappedMemoLines(ExcludeFileMasksMemo);
    IncludeDirectoryMasks = GetUnwrappedMemoLines(IncludeDirectoryMasksMemo);
    ExcludeDirectoryMasks = GetUnwrappedMemoLines(ExcludeDirectoryMasksMemo);

    Mask =
      TFileMasks::ComposeMaskStr(
        IncludeFileMasks, ExcludeFileMasks,
        IncludeDirectoryMasks, ExcludeDirectoryMasks);
  }
  __finally
  {
    delete IncludeFileMasks;
    delete ExcludeFileMasks;
    delete IncludeDirectoryMasks;
    delete ExcludeDirectoryMasks;
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditMaskDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TEditMaskDialog::ClearButtonClick(TObject * /*Sender*/)
{
  IncludeFileMasksMemo->Clear();
  ExcludeFileMasksMemo->Clear();
  IncludeDirectoryMasksMemo->Clear();
  ExcludeDirectoryMasksMemo->Clear();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TEditMaskDialog::FileMasksMemoExit(TObject *Sender)
{
  ValidateMaskEdit(dynamic_cast<TMemo *>(Sender), false);
}
//---------------------------------------------------------------------------
void __fastcall TEditMaskDialog::DirectoryMasksMemoExit(TObject *Sender)
{
  ValidateMaskEdit(dynamic_cast<TMemo *>(Sender), true);
}
//---------------------------------------------------------------------------
void __fastcall TEditMaskDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TEditMaskDialog::UpdateControls()
{
  try
  {
    TFileMasks Mask;
    SaveFileMasks(Mask);
    MaskMemo->Lines->Text = Mask.Masks;
  }
  catch(EFileMasksException & E)
  {
    MaskMemo->Lines->Text = E.Message;
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditMaskDialog::FormKeyDown(
  TObject * /*Sender*/, WORD & Key, TShiftState Shift)
{
  if ((Key == VK_ESCAPE) && Shift.Empty())
  {
    ModalResult = mrCancel;
    Key = 0;
  }
  else if ((Key == VK_RETURN) && Shift.Contains(ssCtrl))
  {
    ModalResult = DefaultResult(this);
    Key = 0;
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditMaskDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(IncludeFileMasksMemo);
  InstallPathWordBreakProc(ExcludeFileMasksMemo);
  InstallPathWordBreakProc(IncludeDirectoryMasksMemo);
  InstallPathWordBreakProc(ExcludeDirectoryMasksMemo);
}
//---------------------------------------------------------------------------
