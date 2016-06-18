//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "WinInterface.h"
#include <VCLCommon.h>
#include <Windows.hpp>
#include <Consts.hpp>
#include <HistoryComboBox.hpp>
#include <PasTools.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
struct TInputDialogToken
{
  TInputDialogInitialize OnInitialize;
  TInputDialogData Data;
  TWinControl * EditControl;
  bool PathInput;
};
//---------------------------------------------------------------------------
void __fastcall InputDialogHelp(void * /*Data*/, TObject * Sender)
{
  TControl * Control = dynamic_cast<TControl *>(Sender);
  Application->HelpKeyword(Control->Parent->HelpKeyword);
}
//---------------------------------------------------------------------------
void __fastcall InputDialogShow(void * Data, TObject * Sender)
{
  TInputDialogToken & Token = *static_cast<TInputDialogToken *>(Data);

  if (Token.OnInitialize != NULL)
  {
    Token.OnInitialize(Sender, &Token.Data);
  }

  if (Token.PathInput)
  {
    InstallPathWordBreakProc(Token.EditControl);
  }
}
//---------------------------------------------------------------------------
bool __fastcall InputDialog(const UnicodeString ACaption,
  const UnicodeString APrompt, UnicodeString & Value, UnicodeString HelpKeyword,
  TStrings * History, bool PathInput, TInputDialogInitialize OnInitialize)
{
  bool Result = False;
  TInputDialogToken Token;
  TForm * Form = new TForm(GetFormOwner(), 0); // bypass the VCL streaming (for Salamander)
  try
  {
    // salam needs to override this in UseSystemSettings
    Form->Position = poOwnerFormCenter;
    SetCorrectFormParent(Form);
    UseSystemSettingsPre(Form);

    // this is what TCustomForm.Loaded does
    // Note that this is not needed as due to use of an alternative constructor above
    // we are already set to default font
    // See TMessageForm::Create for contrary
    Form->Font->Assign(Application->DefaultFont);
    Form->ParentFont = true;

    Token.OnInitialize = OnInitialize;
    Token.PathInput = PathInput;

    TNotifyEvent OnShow;
    ((TMethod *)&OnShow)->Data = &Token;
    ((TMethod *)&OnShow)->Code = InputDialogShow;
    Form->OnShow = OnShow;

    Form->Canvas->Font = Form->Font;
    Form->BorderStyle = bsDialog;
    Form->Caption = ACaption;
    Form->ClientWidth = ScaleByTextHeightRunTime(Form, 275);
    Form->ClientHeight = ScaleByTextHeightRunTime(Form, 102);
    if (!HelpKeyword.IsEmpty())
    {
      Form->HelpKeyword = HelpKeyword;

      Form->BorderIcons = TBorderIcons(Form->BorderIcons) << biHelp;
    }

    TLabel * Prompt = new TLabel(Form);
    Prompt->Parent = Form;
    Prompt->AutoSize = True;
    Prompt->Left = ScaleByTextHeightRunTime(Form, 10);
    Prompt->Top = ScaleByTextHeightRunTime(Form, 13);
    Prompt->Caption = APrompt;

    TEdit * Edit;
    THistoryComboBox * HistoryCombo;
    if (History == NULL)
    {
      Edit = new TEdit(Form);
      Edit->Parent = Form;
      Edit->Text = Value;
      Edit->SelectAll();
      Edit->MaxLength = 255;
      Token.Data.Edit = Edit;
      Token.EditControl = Edit;
    }
    else
    {
      HistoryCombo = new THistoryComboBox(Form);
      HistoryCombo->Parent = Form;
      HistoryCombo->Text = Value;
      HistoryCombo->SelectAll();
      HistoryCombo->Items = History;
      HistoryCombo->MaxLength = 255;
      HistoryCombo->AutoComplete = false;
      Token.EditControl = HistoryCombo;
    }
    Token.EditControl->Left = Prompt->Left;
    Token.EditControl->Top = ScaleByTextHeightRunTime(Form, 30);
    Token.EditControl->Width = ScaleByTextHeightRunTime(Form, 255);

    Prompt->FocusControl = Token.EditControl;

    int ButtonTop = ScaleByTextHeightRunTime(Form, 66);
    int ButtonSpace = ScaleByTextHeightRunTime(Form, 6);

    TButton * Button;
    Button = new TButton(Form);
    Button->Parent = Form;
    Button->Caption = Vcl_Consts_SMsgDlgOK;
    Button->ModalResult = mrOk;
    Button->Default = True;

    int ButtonHeight = ScaleByTextHeightRunTime(Button, Button->Height);
    int ButtonWidth = ScaleByTextHeightRunTime(Button, Button->Width);
    int ButtonsStart;
    if (HelpKeyword.IsEmpty())
    {
      ButtonsStart = (Form->ClientWidth / 2) - ButtonWidth - (ButtonSpace / 2);
    }
    else
    {
      ButtonsStart = (Form->ClientWidth / 2) - (3 * ButtonWidth / 2) - ButtonSpace;
    }

    Button->SetBounds(ButtonsStart, ButtonTop, ButtonWidth, ButtonHeight);

    Button = new TButton(Form);
    Button->Parent = Form;
    Button->Caption = Vcl_Consts_SMsgDlgCancel;
    Button->ModalResult = mrCancel;
    Button->Cancel = True;
    Button->SetBounds(ButtonsStart + ButtonWidth + ButtonSpace, ButtonTop,
      ButtonWidth, ButtonHeight);

    if (!HelpKeyword.IsEmpty())
    {
      Button = new TButton(Form);
      Button->Parent = Form;
      Button->Caption = Vcl_Consts_SMsgDlgHelp;
      Button->ModalResult = mrNone;
      Button->SetBounds(ButtonsStart + 2 * (ButtonWidth + ButtonSpace), ButtonTop,
        ButtonWidth, ButtonHeight);
      TNotifyEvent OnClick;
      ((TMethod*)&OnClick)->Code = InputDialogHelp;
      Button->OnClick = OnClick;
    }

    UseSystemSettingsPost(Form);

    if (Form->ShowModal() == DefaultResult(Form))
    {
      if (History != NULL)
      {
        HistoryCombo->SaveToHistory();
        History->Assign(HistoryCombo->Items);
        Value = HistoryCombo->Text;
      }
      else
      {
        Value = Edit->Text;
      }
      Result = true;
    }
  }
  __finally
  {
    delete Form;
  }
  return Result;
}
//---------------------------------------------------------------------------
