//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <VCLCommon.h>
#include <Windows.hpp>
#include <Consts.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TPoint __fastcall GetAveCharSize(TCanvas* Canvas)
{
	Integer I;
	Char Buffer[52];
	TSize Result;
	for (I = 0; I <= 25; I++) Buffer[I] = (Char)('A' + I);
	for (I = 0; I <= 25; I++) Buffer[I+26] = (Char)('a' + I);
  GetTextExtentPoint(Canvas->Handle, Buffer, 52, &Result);
  return TPoint(Result.cx / 52, Result.cy);
}
//---------------------------------------------------------------------------
bool __fastcall InputDialog(const AnsiString ACaption,
	const AnsiString APrompt, AnsiString &Value)
{
  TForm * Form;
  TLabel * Prompt;
  TEdit * Edit;
  TPoint DialogUnits;
  int ButtonTop, ButtonWidth, ButtonHeight;
  bool Result = False;
  Form = new TForm(Application);
  try
  {
    UseSystemSettings(Form);
    Form->Canvas->Font = Form->Font;
    DialogUnits = GetAveCharSize(Form->Canvas);
    Form->BorderStyle = bsDialog;
    Form->Caption = ACaption;
    Form->ClientWidth = MulDiv(220, DialogUnits.x, 4);
    Form->ClientHeight = MulDiv(63, DialogUnits.y, 8);
    Form->Position = poMainFormCenter;

    Prompt = new TLabel(Form);
    Prompt->Parent = Form;
    Prompt->AutoSize = True;
    Prompt->Left = MulDiv(8, DialogUnits.x, 4);
    Prompt->Top = MulDiv(8, DialogUnits.y, 8);
    Prompt->Caption = APrompt;

    Edit = new TEdit(Form);
    Edit->Parent = Form;
    Edit->Left = Prompt->Left;
    Edit->Top = MulDiv(19, DialogUnits.y, 8);
    Edit->Width = MulDiv(204, DialogUnits.x, 4);
    Edit->MaxLength = 255;
    Edit->Text = Value;
    Edit->SelectAll();

    ButtonTop = MulDiv(41, DialogUnits.y, 8);
    ButtonWidth = MulDiv(50, DialogUnits.x, 4);
    ButtonHeight = MulDiv(14, DialogUnits.y, 8);

    TButton * Button;
    Button = new TButton(Form);
    Button->Parent = Form;
    Button->Caption = Consts_SMsgDlgOK;
    Button->ModalResult = mrOk;
    Button->Default = True;
    Button->SetBounds(MulDiv(58, DialogUnits.x, 4), ButtonTop, ButtonWidth,
      ButtonHeight);

    Button = new TButton(Form);
    Button->Parent = Form;
    Button->Caption = Consts_SMsgDlgCancel;
    Button->ModalResult = mrCancel;
    Button->Cancel = True;
    Button->SetBounds(MulDiv(112, DialogUnits.x, 4), ButtonTop, ButtonWidth,
      ButtonHeight);

    if (Form->ShowModal() == mrOk)
    {
      Value = Edit->Text;
      Result = True;
    }
  }
  __finally
  {
    delete Form;
  }
  return Result;
}
//---------------------------------------------------------------------------
