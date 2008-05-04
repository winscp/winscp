//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Dialogs.hpp>
//---------------------------------------------------------------------
#include <Common.h>
#include <WinInterface.h>
#include <VCLCommon.h>

#include "ComboInput.h"
//---------------------------------------------------------------------
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------
bool __fastcall DoComboInputDialog(
  const AnsiString Caption, const AnsiString Prompt,
  AnsiString & Text, TStrings * Items,
  bool AllowEmpty, const AnsiString HelpKeyword)
{
  bool Result;
  TComboInputDialog * ComboInputDialog = new TComboInputDialog(Application);
  try
  {
    ComboInputDialog->Caption = Caption;
    ComboInputDialog->Prompt = Prompt;
    ComboInputDialog->Text = Text;
    ComboInputDialog->Items = Items;
    ComboInputDialog->AllowEmpty = AllowEmpty;
    ComboInputDialog->HelpKeyword = HelpKeyword;
    Result = (ComboInputDialog->ShowModal() == mrOk);
    if (Result)
    {
      Text = ComboInputDialog->Text;
    }
  }
  __finally
  {
    delete ComboInputDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TComboInputDialog::TComboInputDialog(TComponent* AOwner)
  : TForm(AOwner)
{
  FAllowEmpty = false;
  UseSystemSettings(this);
}
//---------------------------------------------------------------------
void __fastcall TComboInputDialog::SetItems(TStrings * value)
{
  InputCombo->Items = value;
  UpdateControls();
}
//---------------------------------------------------------------------
TStrings * __fastcall TComboInputDialog::GetItems()
{
  return InputCombo->Items;
}
//---------------------------------------------------------------------
void __fastcall TComboInputDialog::SetText(AnsiString value)
{
  InputCombo->Text = value;
  UpdateControls();
}
//---------------------------------------------------------------------
AnsiString __fastcall TComboInputDialog::GetText()
{
  return InputCombo->Text;
}
//---------------------------------------------------------------------------
void __fastcall TComboInputDialog::SetPrompt(AnsiString value)
{
  InputLabel->Caption = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TComboInputDialog::GetPrompt()
{
  return InputLabel->Caption;
}
//---------------------------------------------------------------------------
void __fastcall TComboInputDialog::InputComboChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TComboInputDialog::UpdateControls()
{
  EnableControl(OKButton, !Text.IsEmpty() || FAllowEmpty);
}
//---------------------------------------------------------------------------
void __fastcall TComboInputDialog::FormShow(TObject * /*Sender*/)
{
  TBorderIcons BI = BorderIcons;
  if (HelpKeyword.IsEmpty())
  {
    BI >> biHelp;

    OKButton->Left = CancelButton->Left;
    CancelButton->Left = HelpButton->Left;
    HelpButton->Visible = false;
  }
  else
  {
    BI << biHelp;
  }
  BorderIcons = BI;
}
//---------------------------------------------------------------------------
void __fastcall TComboInputDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
