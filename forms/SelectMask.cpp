//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <FileMasks.h>
#include <ScpMain.h>
#include <TextsWin.h>
#include <Tools.h>
#include <VCLCommon.h>

#include "SelectMask.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoSelectMaskDialog(TCustomDirView * Parent, bool Select,
	TFileFilter * Filter, TConfiguration * Configuration)
{
	bool Result;
  TSelectMaskDialog * Dialog = new TSelectMaskDialog(Application);
  try
  {
    CenterFormOn(Dialog, Parent);
    Dialog->Select = Select;
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
__fastcall TSelectMaskDialog::TSelectMaskDialog(TComponent* Owner)
        : TForm(Owner)
{
  DefaultFileFilter(FFileFilter);
  SetFileFilter(FFileFilter);
  UseSystemFont(this);
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::FormCloseQuery(TObject * /*Sender*/,
      bool &CanClose)
{
  if (ModalResult != mrCancel)
  {
    TFileMasks Masks = MaskEdit->Text;
    int Start, Length;
    if (!Masks.IsValid(Start, Length))
    {
      CanClose = false;
      SimpleErrorDialog(FMTLOAD(MASK_ERROR, (Masks.Masks.SubString(Start+1, Length))));
      // After closing dialog whole text is selected, we want to select only invalid mask
      MaskEdit->SetFocus();
      MaskEdit->SelStart = Start;
      MaskEdit->SelLength = Length;
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TSelectMaskDialog::Execute()
{
  MaskEdit->Items->Text = WinConfiguration->MaskHistory;
  ActiveControl = MaskEdit;
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    MaskEdit->SaveToHistory();
    WinConfiguration->MaskHistory = MaskEdit->Items->Text;
  }
  return Result;
} /* TSelectMaskDialog::Execute */
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::SetSelect(Boolean value)
{
  if (FSelect != value)
  {
    FSelect = value;
    Caption = LoadStr(Select ? SELECT_MASK_SELECT_CAPTION : SELECT_MASK_DESELECT_CAPTION);
  }
} /* TSelectMaskDialog::SetSelect */
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
