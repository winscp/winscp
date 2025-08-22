//---------------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include <Custom.h>
//---------------------------------------------------------------------------
class TInputDialog : public TCustomDialog
{
public:
  __fastcall TInputDialog(
    const UnicodeString & ACaption, const UnicodeString & Prompt, const UnicodeString & HelpKeyword,
    TStrings * History, bool PathInput, TInputDialogInitialize OnInitialize, bool Echo, int AWidth);

  bool __fastcall Execute(UnicodeString & Value);

protected:
  DYNAMIC void __fastcall DoShow();

private:
  bool FPathInput;
  TInputDialogInitialize FOnInitialize;
  TStrings * FHistory;
  TEdit * Edit;
  THistoryComboBox * HistoryCombo;
};
//---------------------------------------------------------------------------
__fastcall TInputDialog::TInputDialog(
  const UnicodeString & ACaption, const UnicodeString & Prompt, const UnicodeString & HelpKeyword,
  TStrings * History, bool PathInput, TInputDialogInitialize OnInitialize, bool Echo, int AWidth) :
  TCustomDialog(HelpKeyword)
{
  Caption = ACaption;
  FPathInput = PathInput;
  FOnInitialize = OnInitialize;
  FHistory = History;

  ClientWidth = ScaleByTextHeight(this, AWidth);

  TLabel * Label = CreateLabel(Prompt);
  int MaxLength = FPathInput ? 0 : 255;
  if (History == NULL)
  {
    Edit = new TEdit(this);
    SetEditPasswordMode(Edit, !Echo);
    HistoryCombo = NULL;
    AddEditLikeControl(Edit, Label);
    reinterpret_cast<TEdit *>(Edit)->MaxLength = MaxLength;
  }
  else
  {
    DebugAssert(Echo);
    HistoryCombo = new THistoryComboBox(this);
    AddEditLikeControl(HistoryCombo, Label);
    HistoryCombo->MaxLength = MaxLength;
    HistoryCombo->AutoComplete = false;
    Edit = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TInputDialog::DoShow()
{
  TCustomDialog::DoShow();

  if (FOnInitialize != NULL)
  {
    TInputDialogData Data;
    Data.Edit = Edit;
    FOnInitialize(this, &Data);
  }

  if (FPathInput)
  {
    if (FHistory == NULL)
    {
      InstallPathWordBreakProc(Edit);
    }
    else
    {
      InstallPathWordBreakProc(HistoryCombo);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TInputDialog::Execute(UnicodeString & Value)
{
  if (FHistory == NULL)
  {
    Edit->Text = Value;
    Edit->SelectAll();
  }
  else
  {
    HistoryCombo->Items = FHistory;
    HistoryCombo->Text = Value;
    HistoryCombo->SelectAll();
  }

  bool Result = TCustomDialog::Execute();

  if (Result)
  {
    if (FHistory != NULL)
    {
      HistoryCombo->SaveToHistory();
      FHistory->Assign(HistoryCombo->Items);
      Value = HistoryCombo->Text;
    }
    else
    {
      Value = Edit->Text;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall InputDialog(const UnicodeString ACaption,
  const UnicodeString APrompt, UnicodeString & Value, UnicodeString HelpKeyword,
  TStrings * History, bool PathInput, TInputDialogInitialize OnInitialize, bool Echo, int Width)
{
  std::unique_ptr<TInputDialog> Dialog(new TInputDialog(ACaption, APrompt, HelpKeyword, History, PathInput, OnInitialize, Echo, Width));
  return Dialog->Execute(Value);
}
//---------------------------------------------------------------------------
