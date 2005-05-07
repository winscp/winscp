//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <VCLCommon.h>
#include <Windows.hpp>
#include <Consts.hpp>
#include <HistoryComboBox.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
void __fastcall InputDialogHelp(void * /*Data*/, TObject * Sender)
{
  TControl * Control = dynamic_cast<TControl *>(Sender);
  Application->HelpKeyword(Control->Parent->HelpKeyword);
}
//---------------------------------------------------------------------------
bool __fastcall InputDialog(const AnsiString ACaption,
  const AnsiString APrompt, AnsiString & Value, AnsiString HelpKeyword,
  TStrings * History, bool PathInput)
{
  TForm * Form;
  TLabel * Prompt;
  TEdit * Edit;
  THistoryComboBox * HistoryCombo;
  TPoint DialogUnits;
  int ButtonTop, ButtonWidth, ButtonHeight;
  bool Result = False;
  Form = new TForm(Application);
  try
  {
    SetCorrectFormParent(Form);
    UseSystemSettingsPre(Form);
    Form->Canvas->Font = Form->Font;
    DialogUnits = GetAveCharSize(Form->Canvas);
    Form->BorderStyle = bsDialog;
    Form->Caption = ACaption;
    Form->ClientWidth = MulDiv(220, DialogUnits.x, 4);
    Form->ClientHeight = MulDiv(63, DialogUnits.y, 8);
    Form->Position = poMainFormCenter;
    if (!HelpKeyword.IsEmpty())
    {
      Form->HelpKeyword = HelpKeyword;

      Form->BorderIcons = TBorderIcons(Form->BorderIcons) << biHelp;
    }

    Prompt = new TLabel(Form);
    Prompt->Parent = Form;
    Prompt->AutoSize = True;
    Prompt->Left = MulDiv(8, DialogUnits.x, 4);
    Prompt->Top = MulDiv(8, DialogUnits.y, 8);
    Prompt->Caption = APrompt;

    TWinControl * EditControl;
    if (History == NULL)
    {
      Edit = new TEdit(Form);
      Edit->Parent = Form;
      Edit->Text = Value;
      Edit->SelectAll();
      Edit->MaxLength = 255;
      EditControl = Edit;
    }
    else
    {
      HistoryCombo = new THistoryComboBox(Form);
      HistoryCombo->Parent = Form;
      HistoryCombo->Text = Value;
      HistoryCombo->SelectAll();
      HistoryCombo->Items = History;
      HistoryCombo->MaxLength = 255;
      EditControl = HistoryCombo;
    }
    EditControl->Left = Prompt->Left;
    EditControl->Top = MulDiv(19, DialogUnits.y, 8);
    EditControl->Width = MulDiv(204, DialogUnits.x, 4);
    if (PathInput)
    {
      InstallPathWordBreakProc(EditControl);
    }

    Prompt->FocusControl = EditControl;

    ButtonTop = MulDiv(41, DialogUnits.y, 8);
    ButtonWidth = MulDiv(50, DialogUnits.x, 4);
    ButtonHeight = MulDiv(14, DialogUnits.y, 8);
    int ButtonSpace = MulDiv(5, DialogUnits.x, 4);
    int ButtonsStart;
    if (HelpKeyword.IsEmpty())
    {
      ButtonsStart = (Form->ClientWidth / 2) - ButtonWidth - (ButtonSpace / 2);
    }
    else
    {
      ButtonsStart = (Form->ClientWidth / 2) - (3 * ButtonWidth / 2) - ButtonSpace;
    }

    TButton * Button;
    Button = new TButton(Form);
    Button->Parent = Form;
    Button->Caption = Consts_SMsgDlgOK;
    Button->ModalResult = mrOk;
    Button->Default = True;
    Button->SetBounds(ButtonsStart, ButtonTop, ButtonWidth, ButtonHeight);

    Button = new TButton(Form);
    Button->Parent = Form;
    Button->Caption = Consts_SMsgDlgCancel;
    Button->ModalResult = mrCancel;
    Button->Cancel = True;
    Button->SetBounds(ButtonsStart + ButtonWidth + ButtonSpace, ButtonTop,
      ButtonWidth, ButtonHeight);

    if (!HelpKeyword.IsEmpty())
    {
      Button = new TButton(Form);
      Button->Parent = Form;
      Button->Caption = Consts_SMsgDlgHelp;
      Button->ModalResult = mrNone;
      Button->SetBounds(ButtonsStart + 2 * (ButtonWidth + ButtonSpace), ButtonTop,
        ButtonWidth, ButtonHeight);
      TNotifyEvent OnClick;  
      ((TMethod*)&OnClick)->Code = InputDialogHelp;
      Button->OnClick = OnClick;
    }

    UseSystemSettingsPost(Form);

    if (Form->ShowModal() == mrOk)
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
