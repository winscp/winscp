//---------------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include "RemoteTransfer.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoRemoteCopyDialog(
  TStrings * Sessions, TStrings * Directories,
  TDirectRemoteCopy AllowDirectCopy, bool Multi, void *& Session, UnicodeString & Target, UnicodeString & FileMask,
  bool & DirectCopy, void * CurrentSession, TDirectoryExistsEvent OnDirectoryExists, bool TargetConfirmed)
{
  bool Result;
  TRemoteTransferDialog * Dialog = SafeFormCreate<TRemoteTransferDialog>();
  try
  {
    Dialog->Init(Multi, Sessions, Directories, AllowDirectCopy, CurrentSession, OnDirectoryExists, TargetConfirmed);
    Result = Dialog->Execute(Session, Target, FileMask, DirectCopy);
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TRemoteTransferDialog::TRemoteTransferDialog(TComponent * Owner)
  : TForm(Owner)
{
  UseSystemSettings(this);

  Caption = LoadStr(REMOTE_COPY_TITLE);
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::Init(
  bool Multi, TStrings * Sessions, TStrings * Directories, TDirectRemoteCopy AllowDirectCopy,
  void * CurrentSession, TDirectoryExistsEvent OnDirectoryExists, bool TargetConfirmed)
{
  FMulti = Multi;
  SessionCombo->Items = Sessions;
  FDirectories = Directories;
  DebugAssert(SessionCombo->Items->Count > 0);
  DebugAssert(SessionCombo->Items->Count == FDirectories->Count);
  FAllowDirectCopy = AllowDirectCopy;
  FCurrentSession = CurrentSession;
  FOnDirectoryExists = OnDirectoryExists;
  FTargetConfirmed = TargetConfirmed;
  LoadDialogImage(Image, L"Duplicate L to R");
}
//---------------------------------------------------------------------------
bool __fastcall TRemoteTransferDialog::Execute(void *& Session, UnicodeString & Target,
  UnicodeString & FileMask, bool & DirectCopy)
{
  for (int Index = 0; Index < SessionCombo->Items->Count; Index++)
  {
    if (SessionCombo->Items->Objects[Index] == Session)
    {
      SessionCombo->ItemIndex = Index;
      break;
    }
  }
  DirectoryEdit->Items = CustomWinConfiguration->History[L"RemoteTarget"];
  DirectoryEdit->Text = UnixIncludeTrailingBackslash(Target) + FileMask;
  FDirectCopy = DirectCopy;
  FOriginalTarget = Target;
  UpdateNotDirectCopyCheck();
  bool Result = (ShowModal() == DefaultResult(this));
  if (Result)
  {
    Session = GetSelectedSession();
    CustomWinConfiguration->History[L"RemoteTarget"] = DirectoryEdit->Items;
    Target = GetTarget();
    FileMask = GetFileMask();
    DirectCopy = !NotDirectCopyCheck->Checked;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString TRemoteTransferDialog::GetTarget()
{
  return UnixExtractFilePath(DirectoryEdit->Text);
}
//---------------------------------------------------------------------------
UnicodeString TRemoteTransferDialog::GetFileMask()
{
  return UnixExtractFileName(DirectoryEdit->Text);
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::UpdateControls()
{
  EnableControl(NotDirectCopyCheck,
    IsCurrentSessionSelected() &&
    (FAllowDirectCopy != drcDisallow));
  EnableControl(OkButton, !DirectoryEdit->Text.IsEmpty());
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(DirectoryEdit);

  UpdateControls();
  DirectoryEdit->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::SessionComboChange(TObject * /*Sender*/)
{
  DirectoryEdit->Text =
    UnixIncludeTrailingBackslash(FDirectories->Strings[SessionCombo->ItemIndex]) +
    UnixExtractFileName(DirectoryEdit->Text);
  UpdateNotDirectCopyCheck();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::UpdateNotDirectCopyCheck()
{
  if (IsCurrentSessionSelected())
  {
    NotDirectCopyCheck->Checked = !FDirectCopy;
  }
  else
  {
    NotDirectCopyCheck->Checked = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult == DefaultResult(this))
  {
    bool TargetConfirmed =
      FTargetConfirmed &&
      UnixSamePath(GetTarget(), FOriginalTarget);

    if (!TargetConfirmed &&
        !IsFileNameMask(GetFileMask()) &&
        FOnDirectoryExists(GetSelectedSession(), DirectoryEdit->Text))
    {
      DirectoryEdit->Text = UnixCombinePaths(DirectoryEdit->Text, AnyMask);
    }

    if (!IsFileNameMask(GetFileMask()) && FMulti)
    {
      UnicodeString Message =
        FormatMultiFilesToOneConfirmation(DirectoryEdit->Text, true);
      if (MessageDialog(Message, qtConfirmation, qaOK | qaCancel, HELP_NONE) == qaCancel)
      {
        Abort();
      }
    }

    if (IsCurrentSessionSelected() &&
        ((FAllowDirectCopy == drcConfirmCommandSession) || (FAllowDirectCopy == drcConfirmCommandSessionDirs)) &&
        !NotDirectCopyCheck->Checked &&
        GUIConfiguration->ConfirmCommandSession)
    {
      TMessageParams Params(mpNeverAskAgainCheck);
      int ObjectNamePart = (FAllowDirectCopy == drcConfirmCommandSession) ? 1 : 2;
      UnicodeString ObjectName = LoadStrPart(REMOTE_COPY_COMMAND_SESSION_FILES_DIRECTORIES, ObjectNamePart);
      UnicodeString Message = FMTLOAD(REMOTE_COPY_COMMAND_SESSION3, (ObjectName, ObjectName, ObjectName));
      unsigned int Answer = MessageDialog(Message, qtConfirmation, qaOK | qaCancel, HelpKeyword, &Params);
      if (Answer == qaNeverAskAgain)
      {
        GUIConfiguration->ConfirmCommandSession = false;
      }
      else if (Answer != qaOK)
      {
        Abort();
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::NotDirectCopyCheckClick(
  TObject * /*Sender*/)
{
  if (IsCurrentSessionSelected())
  {
    FDirectCopy = !NotDirectCopyCheck->Checked;
  }
}
//---------------------------------------------------------------------------
void * TRemoteTransferDialog::GetSelectedSession()
{
  return SessionCombo->Items->Objects[SessionCombo->ItemIndex];
}
//---------------------------------------------------------------------------
bool __fastcall TRemoteTransferDialog::IsCurrentSessionSelected()
{
  return (GetSelectedSession() == FCurrentSession);
}
//---------------------------------------------------------------------------
