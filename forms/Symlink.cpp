//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Configuration.h>
#include <TextsWin.h>
#include "Symlink.h"
#include "VCLCommon.h"
#include <WinInterface.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
bool __fastcall DoSymlinkDialog(UnicodeString & FileName, UnicodeString & PointTo,
  TOperationSide Side, bool & SymbolicLink, bool Edit, bool AllowSymbolic)
{
  bool Result;
  TSymlinkDialog * Dialog = new TSymlinkDialog(Application);
  try
  {
    Dialog->FileName = FileName;
    Dialog->PointTo = PointTo;
    Dialog->Side = Side;
    Dialog->SymbolicLink = SymbolicLink;
    Dialog->Edit = Edit;
    Dialog->AllowSymbolic = AllowSymbolic;
    Result = Dialog->Execute();
    if (Result)
    {
      FileName = Dialog->FileName;
      PointTo = Dialog->PointTo;
      SymbolicLink = Dialog->SymbolicLink;
    }
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TSymlinkDialog::TSymlinkDialog(TComponent* Owner)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FSide = osLocal;

  InstallPathWordBreakProc(PointToEdit);
  InstallPathWordBreakProc(FileNameEdit);
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::UpdateControls()
{
  assert(Side == osLocal || Side == osRemote);
  FileNameEdit->Color = !Edit ? clWindow : clBtnFace;
  FileNameEdit->ReadOnly = Edit;
  FileNameEdit->TabStop = !Edit;
  EnableControl(SymbolicCheck, Side == osRemote && !Edit && AllowSymbolic);
  EnableControl(OkButton, !FileName.IsEmpty() && !PointTo.IsEmpty());
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::SetFileName(UnicodeString value)
{
  FileNameEdit->Text = value;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSymlinkDialog::GetFileName()
{
  return FileNameEdit->Text;
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::SetPointTo(UnicodeString value)
{
  PointToEdit->Text = value;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSymlinkDialog::GetPointTo()
{
  return PointToEdit->Text;
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::SetSymbolicLink(bool value)
{
  SymbolicCheck->Checked = value;
}
//---------------------------------------------------------------------------
bool __fastcall TSymlinkDialog::GetSymbolicLink()
{
  return SymbolicCheck->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::SetSide(TOperationSide value)
{
  FSide = value;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::SetEdit(bool value)
{
  FEdit = value;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::SetAllowSymbolic(bool value)
{
  FAllowSymbolic = value;
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TSymlinkDialog::Execute()
{
  return ShowModal() == mrOk;
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::FormShow(TObject * /*Sender*/)
{
  Caption = LoadStr(Edit ? LINK_EDIT_CAPTION : LINK_ADD_CAPTION);
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
