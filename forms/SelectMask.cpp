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
#pragma link "XPThemes"
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
  UseSystemSettings(this);
  InstallPathWordBreakProc(MaskEdit);
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

