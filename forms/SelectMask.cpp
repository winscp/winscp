//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <FileMasks.h>
#include <CoreMain.h>
#include <TextsWin.h>
#include <Tools.h>
#include <VCLCommon.h>

#include "SelectMask.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HistoryComboBox"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
bool __fastcall DoSelectMaskDialog(TCustomDirView * Parent, bool Select,
  TFileFilter * Filter, TConfiguration * Configuration)
{
  bool Result;
  TSelectMaskDialog * Dialog = new TSelectMaskDialog(Application);
  try
  {
    CenterFormOn(Dialog, Parent);
    Dialog->Init(Select ? TSelectMaskDialog::smSelect : TSelectMaskDialog::smDeselect);
    DefaultFileFilter(*Filter);
    Filter->Masks = WinConfiguration->SelectMask;
    Filter->Directories = WinConfiguration->SelectDirectories;
    Dialog->FileFilter = *Filter;
    Result = Dialog->Execute();
    if (Result)
    {
      *Filter = Dialog->FileFilter;
      WinConfiguration->SelectMask = Filter->Masks;
      WinConfiguration->SelectDirectories = Filter->Directories;
    }
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall DoFilterMaskDialog(TCustomDirView * Parent,
  TFileFilter * Filter)
{
  bool Result;
  TSelectMaskDialog * Dialog = new TSelectMaskDialog(Application);
  try
  {
    CenterFormOn(Dialog, Parent);
    Dialog->Init(TSelectMaskDialog::smFilter);
    Dialog->FileFilter = *Filter;
    Result = Dialog->Execute();
    if (Result)
    {
      *Filter = Dialog->FileFilter;
    }
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TSelectMaskDialog::TSelectMaskDialog(TComponent* Owner)
        : TForm(Owner)
{
  DefaultFileFilter(FFileFilter);
  SetFileFilter(FFileFilter);
  UseSystemSettings(this);
  InstallPathWordBreakProc(MaskEdit);
  HintLabel(HintText, LoadStr(MASK_HINT2));
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::Init(TMode Mode)
{
  int CaptionStr;
  switch (Mode)
  {
    case smSelect:
      CaptionStr = SELECT_MASK_SELECT_CAPTION;
      ClearButton->Hide();
      break;

    case smDeselect:
      CaptionStr = SELECT_MASK_DESELECT_CAPTION;
      ClearButton->Hide();
      break;

    case smFilter:
      CaptionStr = FILTER_MASK_CAPTION;
      IncludingDirectoriesCheck->Hide();
      HelpKeyword = "ui_filter";
      break;
  }
  Caption = LoadStr(CaptionStr);
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult != mrCancel)
  {
    if (MaskEdit->Focused())
    {
      MaskEditExit(NULL);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TSelectMaskDialog::Execute()
{
  MaskEdit->Items = WinConfiguration->History["Mask"];
  ActiveControl = MaskEdit;
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    MaskEdit->SaveToHistory();
    WinConfiguration->History["Mask"] = MaskEdit->Items;
  }
  return Result;
} /* TSelectMaskDialog::Execute */
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::SetFileFilter(TFileFilter value)
{
  FFileFilter = value;
  IncludingDirectoriesCheck->Checked = FFileFilter.Directories;
  MaskEdit->Text = FFileFilter.Masks;
} /* TSelectMaskDialog::SetFileFilter */
//---------------------------------------------------------------------------
TFileFilter __fastcall TSelectMaskDialog::GetFileFilter()
{
  TFileFilter Result = FFileFilter;
  Result.Directories = IncludingDirectoriesCheck->Checked;
  Result.Masks = MaskEdit->Text;
  return Result;
} /* TSelectMaskDialog::GetFileFilter */
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::MaskEditExit(TObject * /*Sender*/)
{
  ValidateMaskEdit(MaskEdit);
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::ClearButtonClick(TObject * /*Sender*/)
{
  MaskEdit->Text = "";
}
//---------------------------------------------------------------------------
