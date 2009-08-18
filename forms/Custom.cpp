//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Dialogs.hpp>
//---------------------------------------------------------------------
#include <Common.h>
#include <WinInterface.h>
#include <VCLCommon.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <CoreMain.h>

#include "Custom.h"
//---------------------------------------------------------------------
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------
bool __fastcall DoCustomDialog(const TDialogParams & Params, TDialogData & Data)
{
  bool Result;
  TCustomDialog * CustomDialog = new TCustomDialog(Application, Params);
  try
  {
    Result = CustomDialog->Execute(Data);
  }
  __finally
  {
    delete CustomDialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall SaveSessionDialogShow(void *, const TDialogControls & Controls)
{
  InstallPathWordBreakProc(Controls.Combo);
  int P = Controls.Combo->Text.LastDelimiter("/");
  if (P > 0)
  {
    Controls.Combo->SetFocus();
    Controls.Combo->SelStart = P;
    Controls.Combo->SelLength = Controls.Combo->Text.Length() - P;
  }

  EnableControl(Controls.Check, bool(Controls.Token));
}
//---------------------------------------------------------------------------
void __fastcall SaveSessionDialogValidate(void * OriginalSession, const TDialogData & Data)
{
  SessionNameValidate(Data.Combo, static_cast<TSessionData *>(OriginalSession));
}
//---------------------------------------------------------------------------
bool __fastcall DoSaveSessionDialog(AnsiString & SessionName,
  bool * SavePassword, TSessionData * OriginalSession)
{
  bool Result;
  TDialogParams Params;
  try
  {
    Params.Caption = LoadStr(SAVE_SESSION_CAPTION);
    Params.HelpKeyword = HELP_SESSION_SAVE;
    Params.ComboLabel = LoadStr(SAVE_SESSION_PROMPT);
    Params.ComboEmptyValid = false;
    Params.CheckLabel = LoadStr(SAVE_SESSION_PASSWORD);
    bool CanSavePassword = (SavePassword != NULL);
    Params.Token = (void *)(CanSavePassword);
    MakeMethod(NULL, SaveSessionDialogShow, Params.OnShow);
    MakeMethod(OriginalSession, SaveSessionDialogValidate, Params.OnValidate);

    Params.ComboItems = new TStringList();
    for (int Index = 0; Index < StoredSessions->Count; Index++)
    {
      TSessionData * Data = StoredSessions->Sessions[Index];
      if (!Data->Special)
      {
        Params.ComboItems->Add(Data->Name);
      }
    }

    TDialogData Data;
    Data.Combo = SessionName;
    Data.Check = CanSavePassword ? *SavePassword : false;

    Result = DoCustomDialog(Params, Data);

    if (Result)
    {
      SessionName = Data.Combo;
      if (CanSavePassword)
      {
        *SavePassword = Data.Check;
      }
    }
  }
  __finally
  {
    delete Params.ComboItems;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall SessionNameValidate(const AnsiString & Text,
  TSessionData * RenamingSession)
{
  TSessionData::ValidatePath(Text);

  assert(StoredSessions);
  TSessionData * Data = (TSessionData *)StoredSessions->FindByName(Text);
  if (Data && Data->Special)
  {
    MessageDialog(FMTLOAD(CANNOT_OVERWRITE_SPECIAL_SESSION, (Text)),
      qtError, qaOK, HELP_NONE);
    Abort();
  }
  else if (Data && (Data != RenamingSession) &&
    MessageDialog(FMTLOAD(CONFIRM_OVERWRITE_SESSION, (Text)),
      qtConfirmation, qaYes | qaNo, HELP_SESSION_SAVE_OVERWRITE) != qaYes)
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
struct TShortCutData
{
  TShortCut ShortCut;
  const TShortCuts * ShortCuts;
};
//---------------------------------------------------------------------------
TShortCutData & __fastcall ShortCutData(const TDialogControls & Controls)
{
  return *reinterpret_cast<TShortCutData *>(Controls.Token);
}
//---------------------------------------------------------------------------
void __fastcall ShortCutDialogInit(void *, const TDialogControls & Controls)
{
  InitializeShortCutCombo(Controls.Combo, *ShortCutData(Controls).ShortCuts);
}
//---------------------------------------------------------------------------
void __fastcall ShortCutDialogLoad(void *, const TDialogData & /*Data*/,
  TDialogControls & Controls)
{
  SetShortCutCombo(Controls.Combo, ShortCutData(Controls).ShortCut);
}
//---------------------------------------------------------------------------
void __fastcall ShortCutDialogSave(void *, const TDialogControls & Controls,
  TDialogData & /*Data*/)
{
  ShortCutData(Controls).ShortCut = GetShortCutCombo(Controls.Combo);
}
//---------------------------------------------------------------------------
bool __fastcall DoShortCutDialog(TShortCut & ShortCut,
  const TShortCuts & ShortCuts, AnsiString HelpKeyword)
{
  TShortCutData Token;
  Token.ShortCut = ShortCut;
  Token.ShortCuts = &ShortCuts;

  TDialogParams Params;
  Params.Token = &Token;
  Params.Caption = LoadStr(SHORTCUT_CAPTION);
  Params.HelpKeyword = HelpKeyword;
  Params.ComboLabel = LoadStr(SHORTCUT_LABEL);
  MakeMethod(NULL, ShortCutDialogInit, Params.OnInit);
  MakeMethod(NULL, ShortCutDialogLoad, Params.OnLoad);
  MakeMethod(NULL, ShortCutDialogSave, Params.OnSave);

  TDialogData Data;

  bool Result = DoCustomDialog(Params, Data);

  if (Result)
  {
    ShortCut = Token.ShortCut;
  }

  return Result;
}
//---------------------------------------------------------------------
__fastcall TCustomDialog::TCustomDialog(TComponent * AOwner, const TDialogParams & Params)
  : TForm(AOwner),
  FParams(Params)
{
  UseSystemSettings(this);

  FControls.Token = Params.Token;
  FControls.Form = this;
  FControls.Combo = Combo;
  FControls.Check = Check;

  Caption = Params.Caption;
  HelpKeyword = Params.HelpKeyword;
  ComboLabel->Caption = Params.ComboLabel;
  if (Params.ComboItems != NULL)
  {
    Combo->Items = Params.ComboItems;
  }
  Check->Caption = Params.CheckLabel;
  if (Params.CheckLabel.IsEmpty())
  {
    ClientHeight -= (Check->Top + Check->Height) - (Combo->Top + Combo->Height);
    Check->Visible = false;
  }

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

  if (FParams.OnInit != NULL)
  {
    FParams.OnInit(FControls);
  }

  DoChange();
}
//---------------------------------------------------------------------
bool __fastcall TCustomDialog::Execute(TDialogData & Data)
{
  Combo->Text = Data.Combo;
  Check->Checked = Data.Check;
  if (FParams.OnLoad != NULL)
  {
    FParams.OnLoad(Data, FControls);
  }
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    SaveData(Data);
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TCustomDialog::SaveData(TDialogData & Data)
{
  Data.Combo = Combo->Text;
  Data.Check = Check->Checked;
  if (FParams.OnSave != NULL)
  {
    FParams.OnSave(FControls, Data);
  }
}
//---------------------------------------------------------------------
void __fastcall TCustomDialog::DoChange()
{
  bool Valid = !Combo->Text.IsEmpty() || FParams.ComboEmptyValid;
  if (FParams.OnChange != NULL)
  {
    FParams.OnChange(FControls, Valid);
  }
  EnableControl(OKButton, Valid);
}
//---------------------------------------------------------------------
void __fastcall TCustomDialog::Change(TObject * /*Sender*/)
{
  DoChange();
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::FormShow(TObject * /*Sender*/)
{
  if (FParams.OnShow != NULL)
  {
    FParams.OnShow(FControls);
  }
  DoChange();
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult == mrOk)
  {
    if (FParams.OnValidate != NULL)
    {
      TDialogData Data;
      SaveData(Data);
      FParams.OnValidate(Data);
    }
  }
}
//---------------------------------------------------------------------------
