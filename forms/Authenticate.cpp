//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "Authenticate.h"

#include <VCLCommon.h>
#include <TextsWin.h>
#include <SecureShell.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "PasswordEdit"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TAuthenticateForm::TAuthenticateForm(TComponent * Owner,
  AnsiString SessionName)
  : TForm(Owner), FSessionName(SessionName)
{
  UseSystemSettings(this);
  FShowAsModalStorage = NULL;
  FFocusControl = NULL;
}
//---------------------------------------------------------------------------
__fastcall TAuthenticateForm::~TAuthenticateForm()
{
  ReleaseAsModal(this, FShowAsModalStorage);
}
//---------------------------------------------------------------------------
void __fastcall TAuthenticateForm::ShowAsModal()
{
  ::ShowAsModal(this, FShowAsModalStorage);
}
//---------------------------------------------------------------------------
void __fastcall TAuthenticateForm::HideAsModal()
{
  ::HideAsModal(this, FShowAsModalStorage);
}
//---------------------------------------------------------------------------
void __fastcall TAuthenticateForm::FormShow(TObject * /*Sender*/)
{
  ClearLog();
  AdjustControls();

  if (FFocusControl != NULL)
  {
    FFocusControl->SetFocus();
  }
}
//---------------------------------------------------------------------------
void __fastcall TAuthenticateForm::ClearLog()
{
  // TListItems::Clear() does nothing without allocated handle
  LogView->HandleNeeded();
  LogView->Items->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TAuthenticateForm::Log(const AnsiString Message)
{
  TListItem * Item = LogView->Items->Add();
  Item->Caption = Message;
  Item->MakeVisible(false);
  LogView->Repaint();
}
//---------------------------------------------------------------------------
void __fastcall TAuthenticateForm::ChangeStatus(const AnsiString Status)
{
  Log(Status);
}
//---------------------------------------------------------------------------
void __fastcall TAuthenticateForm::UpdateControls()
{
}
//---------------------------------------------------------------------------
void __fastcall TAuthenticateForm::AdjustControls()
{
  AnsiString PasswordCaption = PasswordLabel->Hint;
  bool MultiLine = false;
  int P = PasswordCaption.Pos("\n");
  if (P > 0)
  {
    MultiLine = true;
    PasswordCaption.SetLength(P - 1);
  }
  P = PasswordCaption.Pos("\r");
  if (P > 0)
  {
    MultiLine = true;
    PasswordCaption.SetLength(P - 1);
  }

  bool NeedTrim;
  TControlCanvas * PasswordLabelCanvas = new TControlCanvas();
  try
  {
    PasswordLabelCanvas->Control = PasswordLabel;

    NeedTrim = MultiLine ||
      (PasswordLabelCanvas->TextWidth(PasswordCaption) > PasswordLabel->Width);
    if (NeedTrim)
    {
      static AnsiString Ellipsis(" ...");
      while (PasswordLabelCanvas->TextWidth(PasswordCaption + Ellipsis) >
          PasswordLabel->Width)
      {
        PasswordCaption.SetLength(PasswordCaption.Length() - 1);
      }
      PasswordCaption = PasswordCaption + Ellipsis;
    }
  }
  __finally
  {
    delete PasswordLabelCanvas;
  }

  PasswordLabel->Caption = PasswordCaption;
  if (NeedTrim)
  {
    HintLabel(PasswordLabel, Hint);
    PasswordLabel->TabStop = true;
  }
  else
  {
    HintLabelRestore(PasswordLabel);
    PasswordLabel->TabStop = false;
  }

  if (FStatus.IsEmpty())
  {
    Caption = FSessionName;
  }
  else
  {
    Caption = FORMAT("%s - %s", (FStatus, FSessionName));
  }
}
//---------------------------------------------------------------------------
bool __fastcall TAuthenticateForm::PromptUser(AnsiString Caption,
  TPromptKind Kind, AnsiString &Password)
{
  PasswordLabel->Hint = Caption;

  int Title;
  switch (Kind)
  {
    case pkPassword: Title = PASSWORD_TITLE; break;
    case pkPassphrase: Title = PASSPHRASE_TITLE; break;
    case pkServerPrompt: Title = SERVER_PASSWORD_TITLE; break;
    default: assert(false);
  }

  bool ShowServerPanel = (Kind == pkServerPrompt);
  if (ShowServerPanel != ServerPromptPanel->Visible)
  {
    ServerPromptPanel->Visible = ShowServerPanel;
    PasswordPanel->Height += (ShowServerPanel ? 1 : -1) * ServerPromptPanel->Height;
  }

  PasswordEdit->Text = Password;
  bool Result = Execute(LoadStr(Title), PasswordPanel, PasswordEdit,
    PasswordOKButton, PasswordCancelButton, true, false);
  if (Result)
  {
    Password = PasswordEdit->Text;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TAuthenticateForm::Banner(const AnsiString & Banner,
  bool & NeverShowAgain, int Options)
{
  BannerMemo->Lines->Text = Banner;
  NeverShowAgainCheck->Visible = FLAGCLEAR(Options, boDisableNeverShowAgain);
  NeverShowAgainCheck->Checked = NeverShowAgain;
  bool Result = Execute(LoadStr(AUTHENTICATION_BANNER), BannerPanel, BannerCloseButton,
    BannerCloseButton, BannerCloseButton, false, true);
  if (Result)
  {
    NeverShowAgain = NeverShowAgainCheck->Checked;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TAuthenticateForm::Execute(AnsiString Status, TControl * Control,
  TWinControl * FocusControl, TButton * DefaultButton, TButton * CancelButton,
  bool FixHeight, bool Zoom)
{
  TAlign Align = Control->Align;
  try
  {
    assert(FStatus.IsEmpty());
    FStatus = Status;
    DefaultButton->Default = true;
    CancelButton->Cancel = true;

    if (Zoom)
    {
      Control->Align = alClient;
    }

    if (Visible)
    {
      Control->Show();
      TCursor PrevCursor = Screen->Cursor;
      try
      {
        if (Zoom)
        {
          LogView->Hide();
        }
        else
        {
          if (LogView->Items->Count > 0)
          {
            TListItem * Item = LogView->ItemFocused;
            if (Item == NULL)
            {
              Item = LogView->Items->Item[LogView->Items->Count - 1];
            }
            Item->MakeVisible(false);
          }
        }
        Screen->Cursor = crDefault;
        FocusControl->SetFocus();
        ModalResult = mrNone;
        AdjustControls();
        do
        {
          Application->ProcessMessages();
        }
        while (!Application->Terminated && (ModalResult == mrNone));
      }
      __finally
      {
        Control->Hide();
        Screen->Cursor = PrevCursor;
        if (Zoom)
        {
          LogView->Show();
        }
        Repaint();
      }
    }
    else
    {
      int PrevHeight = ClientHeight;
      int PrevMinHeight = Constraints->MinHeight;
      int PrevMaxHeight = Constraints->MaxHeight;
      try
      {
        Constraints->MinHeight = 0;
        ClientHeight = Control->Height;
        if (FixHeight)
        {
          Constraints->MinHeight = Height;
          Constraints->MaxHeight = Height;
        }
        LogView->Hide();
        Control->Show();
        FFocusControl = FocusControl;

        ShowModal();
      }
      __finally
      {
        FFocusControl = NULL;
        ClientHeight = PrevHeight;
        Constraints->MinHeight = PrevMinHeight;
        Constraints->MaxHeight = PrevMaxHeight;
        Control->Hide();
        LogView->Show();
      }
    }
  }
  __finally
  {
    Control->Align = Align;
    DefaultButton->Default = false;
    CancelButton->Cancel = false;
    FStatus = "";
    AdjustControls();
  }

  return (ModalResult != mrCancel);
}
//---------------------------------------------------------------------------
void __fastcall TAuthenticateForm::FormResize(TObject * /*Sender*/)
{
  AdjustControls();
}
//---------------------------------------------------------------------------
void __fastcall TAuthenticateForm::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TAuthenticateForm::HideTypingCheckClick(TObject * /*Sender*/)
{
  PasswordEdit->Password = HideTypingCheck->Checked;
}
//---------------------------------------------------------------------------
