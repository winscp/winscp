//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Configuration.h>
#include <TextsWin.h>
#include "Symlink.h"
#include "VCLCommon.h"
#include <assert.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XPGroupBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoSymlinkDialog(AnsiString & FileName, AnsiString & PointTo,
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
  UseSystemFont(this);
  FSide = osLocal;
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::UpdateControls()
{
  assert(Side == osLocal || Side == osRemote);
  FileNameEdit->Color = !Edit ? clWindow : clBtnFace;
  FileNameEdit->ReadOnly = Edit;
  EnableControl(SymbolicCheck, Side == osRemote && !Edit && AllowSymbolic);
  EnableControl(OkButton, !FileName.IsEmpty() && !PointTo.IsEmpty());
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::SetFileName(AnsiString value)
{
  FileNameEdit->Text = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSymlinkDialog::GetFileName()
{
  return FileNameEdit->Text;
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::SetPointTo(AnsiString value)
{
  PointToEdit->Text = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSymlinkDialog::GetPointTo()
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
  if (Edit)
  {
    PointToEdit->SetFocus();
  }
  else
  {
    FileNameEdit->SetFocus();
  }
  UpdateControls();
}
//---------------------------------------------------------------------------

