//---------------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include "Symlink.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoSymlinkDialog(UnicodeString & FileName, UnicodeString & PointTo,
  TOperationSide Side, bool & SymbolicLink, bool Edit, bool AllowHardLink)
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
    Dialog->AllowHardLink = AllowHardLink;
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
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::UpdateControls()
{
  DebugAssert(Side == osLocal || Side == osRemote);
  FileNameEdit->Color = !Edit ? clWindow : clBtnFace;
  FileNameEdit->ReadOnly = Edit;
  FileNameEdit->TabStop = !Edit;
  EnableControl(HardLinkCheck, (Side == osRemote) && !Edit && AllowHardLink);
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
  HardLinkCheck->Checked = !value;
}
//---------------------------------------------------------------------------
bool __fastcall TSymlinkDialog::GetSymbolicLink()
{
  return !HardLinkCheck->Checked;
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
void __fastcall TSymlinkDialog::SetAllowHardLink(bool value)
{
  FAllowHardLink = value;
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TSymlinkDialog::Execute()
{
  return (ShowModal() == DefaultResult(this));
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(PointToEdit);
  InstallPathWordBreakProc(FileNameEdit);

  Caption = LoadStr(Edit ? LINK_EDIT_CAPTION : LINK_ADD_CAPTION);
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSymlinkDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
