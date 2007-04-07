//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "Authenticate.h"

#include <VCLCommon.h>
#include <TextsWin.h>
#include <Terminal.h>
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
  ClearLog();
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
void __fastcall TAuthenticateForm::UpdateControls()
{
  PasswordEdit->Password = HideTypingCheck->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TAuthenticateForm::AdjustControls()
{
  if (PasswordLabel->Caption != FPasswordCaption)
  {
    int LabelWidth = PasswordLabel->Width;
    int LabelHeight = PasswordLabel->Height;
    PasswordLabel->AutoSize = false;
    PasswordLabel->Caption = FPasswordCaption;
    PasswordLabel->AutoSize = true;
    PasswordLabel->Width = LabelWidth;

    int HeightDiff = (PasswordLabel->Height - LabelHeight);
    PasswordEditPanel->Height = PasswordEditPanel->Height + HeightDiff;
    PasswordPanel->Height = PasswordPanel->Height + HeightDiff;
  }

  if (FStatus.IsEmpty())
  {
    Caption = FSessionName;
  }
  else
  {
    Caption = FORMAT("%s - %s", (FStatus, FSessionName));
  }

  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TAuthenticateForm::PromptUser(AnsiString Caption,
  TPromptKind Kind, AnsiString & Response, bool ForceLog)
{
  bool ShowServerPanel;
  AnsiString Title;

  switch (Kind)
  {
    case pkPassword:
      Title = LoadStr(PASSWORD_TITLE);
      HideTypingCheck->Checked = true;
      ShowServerPanel = false;
      break;

    case pkPassphrase:
      Title = LoadStr(PASSPHRASE_TITLE);
      HideTypingCheck->Checked = true;
      ShowServerPanel = false;
      break;

    case pkServerPrompt:
      Title = LoadStr(SERVER_PASSWORD_TITLE);
      ShowServerPanel = true;
      break;

    case pkPrompt:
      Title = CutToChar(Caption, '|', true);
      if (Caption.IsEmpty())
      {
        Caption = Title;
      }
      HideTypingCheck->Checked = false;
      ShowServerPanel = false;
      break;

    default:
      assert(false);
  }

  FPasswordCaption = Caption;

  if (ShowServerPanel != ServerPromptPanel->Visible)
  {
    ServerPromptPanel->Visible = ShowServerPanel;
    PasswordPanel->Height += (ShowServerPanel ? 1 : -1) * ServerPromptPanel->Height;
  }

  PasswordEdit->Text = Response;
  bool Result = Execute(Title, PasswordPanel, PasswordEdit,
    PasswordOKButton, PasswordCancelButton, true, false, ForceLog);
  if (Result)
  {
    Response = PasswordEdit->Text;
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
    BannerCloseButton, BannerCloseButton, false, true, false);
  if (Result)
  {
    NeverShowAgain = NeverShowAgainCheck->Checked;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TAuthenticateForm::Execute(AnsiString Status, TControl * Control,
  TWinControl * FocusControl, TButton * DefaultButton, TButton * CancelButton,
  bool FixHeight, bool Zoom, bool ForceLog)
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

    if (ForceLog || Visible)
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

        if (!Visible)
        {
          assert(ForceLog);
          ShowAsModal();
        }

        FocusControl->SetFocus();
        ModalResult = mrNone;
        AdjustControls();
        do
        {
          Application->HandleMessage();
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
  UpdateControls();
}
//---------------------------------------------------------------------------
