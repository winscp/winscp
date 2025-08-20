//---------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include <Terminal.h>
#include "FileSystemInfo.h"
//---------------------------------------------------------------------
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
void __fastcall DoFileSystemInfoDialog(
  const TSessionInfo & SessionInfo, const TFileSystemInfo & FileSystemInfo,
  UnicodeString SpaceAvailablePath, TGetSpaceAvailable OnGetSpaceAvailable)
{
  TFileSystemInfoDialog * Dialog = new TFileSystemInfoDialog(Application,
    OnGetSpaceAvailable);
  try
  {
    Dialog->Execute(SessionInfo, FileSystemInfo, SpaceAvailablePath);
  }
  __finally
  {
    delete Dialog;
  }
}
//---------------------------------------------------------------------
__fastcall TFileSystemInfoDialog::TFileSystemInfoDialog(TComponent * AOwner,
  TGetSpaceAvailable OnGetSpaceAvailable)
  : TForm(AOwner)
{
  UseSystemSettings(this);
  FOnGetSpaceAvailable = OnGetSpaceAvailable;
  FSpaceAvailableLoaded = false;
  FLastListItem = 0;

  CertificateGroup->Top = HostKeyGroup->Top;

  ReadOnlyControl(HostKeyAlgorithmEdit);
  ReadOnlyControl(HostKeyFingerprintSHA256Edit);
  ReadOnlyControl(HostKeyFingerprintMD5Edit);
  ReadOnlyControl(CertificateFingerprintSha256Edit);
  ReadOnlyControl(CertificateFingerprintSha1Edit);
  ReadOnlyControl(InfoMemo);
}
//---------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::Execute(const TSessionInfo & SessionInfo,
  const TFileSystemInfo & FileSystemInfo, UnicodeString SpaceAvailablePath)
{
  FSessionInfo = SessionInfo;
  FFileSystemInfo = FileSystemInfo;
  SpaceAvailablePathEdit->Text = SpaceAvailablePath;
  UpdateControls();
  ShowModal();
}
//---------------------------------------------------------------------
UnicodeString __fastcall TFileSystemInfoDialog::CapabilityStr(TFSCapability Capability)
{
  return BooleanToStr(FFileSystemInfo.IsCapable[Capability]);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TFileSystemInfoDialog::CapabilityStr(TFSCapability Capability1,
  TFSCapability Capability2)
{
  return FORMAT(L"%s/%s", (CapabilityStr(Capability1), CapabilityStr(Capability2)));
}
//---------------------------------------------------------------------
UnicodeString __fastcall TFileSystemInfoDialog::SpaceStr(__int64 Bytes)
{
  UnicodeString Result;
  if (Bytes == 0)
  {
    Result = LoadStr(FSINFO_BYTES_UNKNOWN);
  }
  else
  {
    Result = FormatBytes(Bytes);
    UnicodeString SizeUnorderedStr = FormatBytes(Bytes, fbNone);
    if (Result != SizeUnorderedStr)
    {
      Result = FORMAT(L"%s (%s)", (Result, SizeUnorderedStr));
    }
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::Feed(TFeedFileSystemData AddItem)
{
  AddItem(ServerView, FSINFO_REMOTE_SYSTEM, FFileSystemInfo.RemoteSystem);
  AddItem(ServerView, FSINFO_FS_PROTOCOL, FFileSystemInfo.ProtocolName);
  AddItem(ServerView, FSINFO_CRYPTOGRAPHIC_PROTOCOL, FSessionInfo.SecurityProtocolName);
  AddItem(ServerView, FSINFO_SSH_IMPLEMENTATION, FSessionInfo.SshImplementation);

  UnicodeString Str = FSessionInfo.CSCipher;
  if (FSessionInfo.CSCipher != FSessionInfo.SCCipher)
  {
    Str += FORMAT(L"/%s", (FSessionInfo.SCCipher));
  }
  AddItem(ServerView, FSINFO_CIPHER, Str);

  Str = DefaultStr(FSessionInfo.CSCompression, LoadStr(NO_STR));
  if (FSessionInfo.CSCompression != FSessionInfo.SCCompression)
  {
    Str += FORMAT(L"/%s", (DefaultStr(FSessionInfo.SCCompression, LoadStr(NO_STR))));
  }
  AddItem(ServerView, FSINFO_COMPRESSION, Str);

  AddItem(HostKeyFingerprintSHA256Edit, 0, FSessionInfo.HostKeyFingerprintSHA256);
  AddItem(HostKeyFingerprintMD5Edit, 0, FSessionInfo.HostKeyFingerprintMD5);
  AddItem(CertificateFingerprintSha256Edit, 0, FSessionInfo.CertificateFingerprintSHA256);
  AddItem(CertificateFingerprintSha1Edit, 0, FSessionInfo.CertificateFingerprintSHA1);

  AddItem(ProtocolView, FSINFO_MODE_CHANGING, CapabilityStr(fcModeChanging));
  AddItem(ProtocolView, FSINFO_ACL_CHANGING, CapabilityStr(fcAclChangingFiles));
  AddItem(ProtocolView, FSINFO_OWNER_GROUP_CHANGING, CapabilityStr(fcGroupChanging));
  UnicodeString AnyCommand;
  if (!FFileSystemInfo.IsCapable[fcShellAnyCommand] &&
      FFileSystemInfo.IsCapable[fcAnyCommand])
  {
    AnyCommand = LoadStr(FSINFO_PROTOCOL_ANY_COMMAND);
  }
  else
  {
    AnyCommand = CapabilityStr(fcAnyCommand);
  }
  AddItem(ProtocolView, FSINFO_ANY_COMMAND, AnyCommand);
  AddItem(ProtocolView, FSINFO_SYMBOLIC_HARD_LINK, CapabilityStr(fcSymbolicLink, fcHardLink));
  AddItem(ProtocolView, FSINFO_USER_GROUP_LISTING, CapabilityStr(fcUserGroupListing));
  AddItem(ProtocolView, FSINFO_REMOTE_COPY, CapabilityStr(fcRemoteCopy));
  AddItem(ProtocolView, FSINFO_CHECKING_SPACE_AVAILABLE, CapabilityStr(fcCheckingSpaceAvailable));
  AddItem(ProtocolView, FSINFO_CALCULATING_CHECKSUM, CapabilityStr(fcCalculatingChecksum));
  AddItem(ProtocolView, FSINFO_NATIVE_TEXT_MODE, CapabilityStr(fcNativeTextMode));

  AddItem(InfoMemo, 0, FFileSystemInfo.AdditionalInfo);

  AddItem(SpaceAvailableView, FSINFO_BYTES_ON_DEVICE, SpaceStr(FSpaceAvailable.BytesOnDevice));
  AddItem(SpaceAvailableView, FSINFO_UNUSED_BYTES_ON_DEVICE, SpaceStr(FSpaceAvailable.UnusedBytesOnDevice));
  AddItem(SpaceAvailableView, FSINFO_BYTES_AVAILABLE_TO_USER, SpaceStr(FSpaceAvailable.BytesAvailableToUser));
  AddItem(SpaceAvailableView, FSINFO_UNUSED_BYTES_AVAILABLE_TO_USER, SpaceStr(FSpaceAvailable.UnusedBytesAvailableToUser));
  AddItem(SpaceAvailableView, FSINFO_BYTES_PER_ALLOCATION_UNIT, SpaceStr(FSpaceAvailable.BytesPerAllocationUnit));
}
//---------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::ControlsAddItem(TControl * Control,
  int Label, UnicodeString Value)
{
  if (FLastFeededControl != Control)
  {
    // TODO, we should clear excess list view items here, but it
    // actually should not happen as of now
    FLastFeededControl = Control;
    FLastListItem = 0;
  }

  if ((Control == HostKeyFingerprintSHA256Edit) || (Control == HostKeyFingerprintMD5Edit))
  {
    EnableControl(HostKeyGroup, !Value.IsEmpty());
    HostKeyGroup->Visible = !Value.IsEmpty();
    UnicodeString Alg1 = CutToChar(Value, L' ', true);
    UnicodeString Alg2 = CutToChar(Value, L' ', true);
    HostKeyAlgorithmEdit->Text = FORMAT(L"%s %s", (Alg1, Alg2));
    DebugNotNull(dynamic_cast<TEdit *>(Control))->Text = Value;
  }
  else if ((Control == CertificateFingerprintSha256Edit) || (Control == CertificateFingerprintSha1Edit))
  {
    EnableControl(CertificateGroup, !Value.IsEmpty());
    CertificateGroup->Visible = !Value.IsEmpty();
    DebugNotNull(dynamic_cast<TEdit *>(Control))->Text = Value;
  }
  else if (Control == InfoMemo)
  {
    EnableControl(InfoGroup, !Value.IsEmpty());
    InfoGroup->Visible = !Value.IsEmpty();
    InfoMemo->Lines->Text = Value;
  }
  else
  {
    TListView * ListView = dynamic_cast<TListView *>(Control);
    DebugAssert(ListView != NULL);

    if (!Value.IsEmpty())
    {
      TListItem * Item;
      if (ListView->Items->Count > FLastListItem)
      {
        Item = ListView->Items->Item[FLastListItem];
      }
      else
      {
        Item = ListView->Items->Add();
      }
      FLastListItem++;

      Item->Caption = LoadStr(Label);
      if (Item->SubItems->Count > 0)
      {
        Item->SubItems->Strings[0] = Value;
      }
      else
      {
        Item->SubItems->Add(Value);
      }
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::FeedControls()
{
  FLastFeededControl = NULL;
  Feed(ControlsAddItem);
  AutoSizeListColumnsWidth(ServerView, 1);
  AutoSizeListColumnsWidth(ProtocolView, 1);
  AutoSizeListColumnsWidth(SpaceAvailableView, 1);
}
//---------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::UpdateControls()
{
  EnableControl(SpaceAvailableSheet, SpaceAvailableSupported());
  EnableControl(SpaceAvailableButton, SpaceAvailableSheet->Enabled &&
    !SpaceAvailablePathEdit->Text.IsEmpty());
}
//---------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::ClipboardAddItem(TControl * Control,
  int Label, UnicodeString Value)
{
  if (Control->Enabled && !Value.IsEmpty())
  {
    TGroupBox * Group = dynamic_cast<TGroupBox *>(Control->Parent);
    TControl * ControlGroup = (Group != NULL) ? Group : Control;
    if (FLastFeededControl != ControlGroup)
    {
      if (FLastFeededControl != NULL)
      {
        FClipboard += UnicodeString::StringOfChar(L'-', 60) + L"\r\n";
      }

      if (Group != NULL)
      {
        FClipboard += FORMAT(L"%s\r\n", (Group->Caption));
      }

      FLastFeededControl = ControlGroup;
    }

    if (dynamic_cast<TListView *>(Control) == NULL)
    {
      for (int Index = 0; Index < Control->Parent->ControlCount; Index++)
      {
        TLabel * Label = dynamic_cast<TLabel *>(Control->Parent->Controls[Index]);
        if ((Label != NULL) && (Label->FocusControl == Control))
        {
          UnicodeString S = RemoveSuffix(StripHotkey(Label->Caption), L":");
          FClipboard += FORMAT(L"%s = ", (S));
        }
      }
      Value = RemoveSuffix(Value, L"\r\n");
      FClipboard += FORMAT(L"%s\r\n", (Value));
    }
    else
    {
      DebugAssert(dynamic_cast<TListView *>(Control) != NULL);
      FClipboard += FORMAT(L"%s = %s\r\n", (LoadStr(Label), Value));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::ClipboardButtonClick(
  TObject * /*Sender*/)
{
  TInstantOperationVisualizer Visualizer;

  NeedSpaceAvailable();
  FLastFeededControl = NULL;
  FClipboard = L"";
  Feed(ClipboardAddItem);
  CopyToClipboard(FClipboard);
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::CopyClick(TObject * Sender)
{
  TInstantOperationVisualizer Visualizer;

  TListView * ListView = dynamic_cast<TListView *>(GetPopupComponent(Sender));
  DebugAssert(ListView != NULL);

  UnicodeString Text;
  for (int Index = 0; Index < ListView->Items->Count; Index++)
  {
    TListItem * Item = ListView->Items->Item[Index];
    if (Item->Selected)
    {
      Text += FORMAT(L"%s = %s\r\n", (Item->Caption, Item->SubItems->Strings[0]));
    }
  }
  CopyToClipboard(Text);
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(SpaceAvailablePathEdit);

  PageControl->ActivePage = ProtocolSheet;
  FeedControls();
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::SpaceAvailableButtonClick(
  TObject * /*Sender*/)
{
  CheckSpaceAvailable();
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::CheckSpaceAvailable()
{
  DebugAssert(FOnGetSpaceAvailable != NULL);
  DebugAssert(!SpaceAvailablePathEdit->Text.IsEmpty());

  FSpaceAvailableLoaded = true;

  bool DoClose = false;
  try
  {
    FOnGetSpaceAvailable(SpaceAvailablePathEdit->Text, FSpaceAvailable, DoClose);
  }
  __finally
  {
    FeedControls();
    if (DoClose)
    {
      Close();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::NeedSpaceAvailable()
{
  if (!FSpaceAvailableLoaded && SpaceAvailableSupported())
  {
    CheckSpaceAvailable();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFileSystemInfoDialog::SpaceAvailableSupported()
{
  return (FOnGetSpaceAvailable != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::PageControlChange(TObject * /*Sender*/)
{
  if (PageControl->ActivePage == SpaceAvailableSheet)
  {
    NeedSpaceAvailable();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::SpaceAvailablePathEditEnter(
  TObject * /*Sender*/)
{
  SpaceAvailableButton->Default = true;
  CloseButton->Default = false;
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::SpaceAvailablePathEditExit(
  TObject * /*Sender*/)
{
  SpaceAvailableButton->Default = false;
  CloseButton->Default = true;
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::ControlContextPopup(
  TObject * Sender, TPoint & MousePos, bool & Handled)
{
  MenuPopup(Sender, MousePos, Handled);
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::CertificateViewButtonClick(
  TObject * /*Sender*/)
{
  MessageDialog(FSessionInfo.Certificate, qtInformation, qaOK);
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::SpaceAvailableViewCustomDrawItem(
  TCustomListView * Sender, TListItem * Item, TCustomDrawState /*State*/, bool & /*DefaultDraw*/)
{
  if ((Item->SubItems->Count >= 1) && (Item->SubItems->Strings[0] == LoadStr(FSINFO_BYTES_UNKNOWN)))
  {
    Sender->Canvas->Font->Color = clGrayText;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::EditCopyActionExecute(TObject * /*Sender*/)
{
  TEdit * Edit = HostKeyFingerprintSHA256Edit->Focused() ? HostKeyFingerprintSHA256Edit : HostKeyFingerprintMD5Edit;
  if ((Edit->SelLength == 0) || (Edit->SelLength == Edit->Text.Length()))
  {
    CopyToClipboard(FORMAT(L"%s %s", (HostKeyAlgorithmEdit->Text, Edit->Text)));
  }
  else
  {
    Edit->CopyToClipboard();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::HostKeyFingerprintSHA256EditContextPopup(
  TObject * Sender, TPoint & MousePos, bool & Handled)
{
  MenuPopup(Sender, MousePos, Handled);
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::EditCopyActionUpdate(TObject * /*Sender*/)
{
  // When noting is selected, we copy whole key, including algorithm, see EditCopyActionExecute
  EditCopyAction->Enabled = true;
}
//---------------------------------------------------------------------------
