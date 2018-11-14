//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <FileMasks.h>
#include <CoreMain.h>
#include <TextsWin.h>
#include <HelpWin.h>
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
bool __fastcall DoSelectMaskDialog(TControl * Parent, bool Select, TFileFilter & Filter)
{
  std::unique_ptr<TSelectMaskDialog> Dialog(new TSelectMaskDialog(Application));
  Dialog->Init((Select ? TSelectMaskDialog::smSelect : TSelectMaskDialog::smDeselect), Parent);
  DefaultFileFilter(Filter);
  Filter.Masks = WinConfiguration->SelectMask;
  Filter.Directories = WinConfiguration->SelectDirectories;
  bool Result = Dialog->Execute(Filter);
  if (Result)
  {
    WinConfiguration->SelectMask = Filter.Masks;
    WinConfiguration->SelectDirectories = Filter.Directories;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall DoFilterMaskDialog(TControl * Parent, UnicodeString & Mask)
{
  TFileFilter Filter;
  DefaultFileFilter(Filter);
  Filter.Masks = Mask;
  std::unique_ptr<TSelectMaskDialog> Dialog(new TSelectMaskDialog(Application));
  Dialog->Init(TSelectMaskDialog::smFilter, Parent);
  bool Result = Dialog->Execute(Filter);
  if (Result)
  {
    Mask = Filter.Masks;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TSelectMaskDialog::TSelectMaskDialog(TComponent * Owner) :
  TForm(Owner)
{
  UseSystemSettings(this);
  HintLabel(HintText,
    FORMAT(L"%s\n \n%s\n \n%s\n \n%s", (LoadStr(MASK_HINT2), LoadStr(FILE_MASK_EX_HINT),
      LoadStr(COMBINING_MASKS_HINT), LoadStr(MASK_HELP))));
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::Init(TMode Mode, TControl * Parent)
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
      ApplyToDirectoriesCheck->Hide();
      HelpKeyword = HELP_FILTER;
      break;
  }
  Caption = LoadStr(CaptionStr);
  FParent = Parent;
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::FormCloseQuery(TObject *, bool & DebugUsedArg(CanClose))
{
  if (ModalResult == DefaultResult(this))
  {
    if (MaskEdit->Focused())
    {
      MaskEditExit(NULL);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TSelectMaskDialog::Execute(TFileFilter & FileFilter)
{
  ApplyToDirectoriesCheck->Checked = FileFilter.Directories;
  MaskEdit->Text = FileFilter.Masks;
  MaskEdit->Items = WinConfiguration->History[L"Mask"];
  ActiveControl = MaskEdit;
  bool Result = (ShowModal() == DefaultResult(this));
  if (Result)
  {
    MaskEdit->SaveToHistory();
    WinConfiguration->History[L"Mask"] = MaskEdit->Items;
    FileFilter.Directories = ApplyToDirectoriesCheck->Checked;
    FileFilter.Masks = MaskEdit->Text;
  }
  return Result;
}
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
  MaskEdit->Text = L"";
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(MaskEdit);
  // Only now it is scaled
  CenterFormOn(this, FParent);
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::MaskButtonClick(TObject * /*Sender*/)
{
  TFileMasks Masks = MaskEdit->Text;
  if (DoEditMaskDialog(Masks))
  {
    MaskEdit->Text = Masks.Masks;
  }
}
//---------------------------------------------------------------------------
