//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include "CopyLocal.h"
#include "VCLCommon.h"
#include "TextsWin.h"
#include "GUITools.h"
#include "Tools.h"
#include "WinInterface.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool DoCopyLocalDialog(bool Move, int Options, UnicodeString & TargetDirectory, UnicodeString & FileMask, int & OutputOptions)
{
  std::unique_ptr<TCopyLocalDialog> Dialog(new TCopyLocalDialog(GetFormOwner(), Move, Options));
  return Dialog->Execute(TargetDirectory, FileMask, OutputOptions);
}
//---------------------------------------------------------------------------
TCopyLocalDialog::TCopyLocalDialog(TComponent * Owner, bool Move, int Options)
  : TForm(Owner)
{

  FOptions = Options;
  UnicodeString ACaption;
  UnicodeString ImageName;
  if (!Move)
  {
    ImageName = L"Duplicate L to R";
    ACaption = LoadStr(COPY_LOCAL_COPY_CAPTION);
  }
  else
  {
    ImageName = L"Move L to R";
    ACaption = LoadStr(COPY_LOCAL_MOVE_CAPTION);
  }
  Caption = ACaption;
  LoadDialogImage(Image, ImageName);

  HotTrackLabel(ShortCutHintLabel);
  if (FLAGCLEAR(FOptions, cloShortCutHint) || CustomWinConfiguration->CopyShortCutHintShown)
  {
    ShortCutHintPanel->Visible = false;
    ClientHeight = ClientHeight - ShortCutHintPanel->Height;
  }

  AutoSizeCheckBox(NeverShowAgainCheck);

  UseSystemSettings(this);
}
//---------------------------------------------------------------------------
bool TCopyLocalDialog::Execute(UnicodeString & TargetDirectory, UnicodeString & FileMask, int & OutputOptions)
{
  DirectoryEdit->Items = CustomWinConfiguration->History[L"LocalTarget"];
  SetDirectoryAndFileMask(TargetDirectory, FileMask);
  NeverShowAgainCheck->Checked = FLAGSET(OutputOptions, clooDoNotShowAgain);
  DebugAssert((OutputOptions & ~clooDoNotShowAgain) == 0);
  bool Result = (ShowModal() == DefaultResult(this));
  if (Result)
  {
    ValidateDirectoryEdit();

    DirectoryEdit->SaveToHistory();
    CustomWinConfiguration->History[L"LocalTarget"] = DirectoryEdit->Items;

    FileMask = GetFileMask();
    TargetDirectory = GetDirectory();
    OutputOptions = FLAGMASK(NeverShowAgainCheck->Checked, clooDoNotShowAgain);

    if (ShortCutHintPanel->Visible)
    {
      CustomWinConfiguration->CopyShortCutHintShown = true;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCopyLocalDialog::ShortCutHintLabelClick(TObject *)
{
  DoPreferencesDialog(pmCommander);
}
//---------------------------------------------------------------------------
void __fastcall TCopyLocalDialog::FormShow(TObject *)
{
  InstallPathWordBreakProc(DirectoryEdit);
  // Does not work when set from a constructor
  ShortCutHintPanel->Color = Application->HintColor;
  UpdateControls();
}
//---------------------------------------------------------------------------
void TCopyLocalDialog::UpdateControls()
{
}
//---------------------------------------------------------------------------
void TCopyLocalDialog::ValidateDirectoryEdit()
{
  if (DirectoryExistsFix(DirectoryEdit->Text))
  {
    DirectoryEdit->Text = IncludeTrailingBackslash(DirectoryEdit->Text) + AnyMask;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyLocalDialog::DirectoryEditExit(TObject *)
{
  ValidateDirectoryEdit();
}
//---------------------------------------------------------------------------
void __fastcall TCopyLocalDialog::FormCloseQuery(TObject *, bool & CanClose)
{
  if (ModalResult == DefaultResult(this))
  {
    ExitActiveControl(this);

    CanClose =
      CopyDialogValidateLocalDirectory(GetDirectory(), DirectoryEdit) &&
      CopyDialogValidateFileMask(GetFileMask(), DirectoryEdit, FLAGSET(FOptions, cloMultipleFiles), false);
  }
}
//---------------------------------------------------------------------------
void TCopyLocalDialog::SetDirectoryAndFileMask(const UnicodeString & Directory, const UnicodeString & FileMask)
{
  DirectoryEdit->Text = IncludeTrailingBackslash(Directory) + FileMask;
}
//---------------------------------------------------------------------------
UnicodeString TCopyLocalDialog::GetDirectory()
{
  UnicodeString Result = DirectoryEdit->Text;
  Result = ExtractFilePath(Result);
  if (!Result.IsEmpty())
  {
    Result = IncludeTrailingBackslash(Result);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString TCopyLocalDialog::GetFileMask()
{
  return ExtractFileName(DirectoryEdit->Text);
}
//---------------------------------------------------------------------------
void __fastcall TCopyLocalDialog::HelpButtonClick(TObject *)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TCopyLocalDialog::LocalDirectoryBrowseButtonClick(TObject *)
{
  UnicodeString ADirectory = GetDirectory();
  if (SelectDirectory(ADirectory, LoadStr(SELECT_LOCAL_DIRECTORY), false))
  {
    SetDirectoryAndFileMask(ADirectory, GetFileMask());
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyLocalDialog::FormAfterMonitorDpiChanged(TObject *, int OldDPI, int NewDPI)
{
  DebugUsedParam2(OldDPI, NewDPI);
  AutoSizeCheckBox(NeverShowAgainCheck);
}
//---------------------------------------------------------------------------
