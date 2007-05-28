//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Terminal.h>
#include <VCLCommon.h>
#include "WinInterface.h"
#include "FileSystemInfo.h"
#include "TextsCore.h"
#include "TextsWin.h"
#include "GUITools.h"
//---------------------------------------------------------------------
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
void __fastcall DoFileSystemInfoDialog(
  const TSessionInfo & SessionInfo, const TFileSystemInfo & FileSystemInfo,
  AnsiString SpaceAvailablePath, TGetSpaceAvailable OnGetSpaceAvailable)
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

  InstallPathWordBreakProc(SpaceAvailablePathEdit);
  ReadOnlyControl(HostKeyFingerprintEdit);
  ReadOnlyControl(InfoMemo);
}
//---------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::Execute(const TSessionInfo & SessionInfo,
  const TFileSystemInfo & FileSystemInfo, AnsiString SpaceAvailablePath)
{
  FSessionInfo = SessionInfo;
  FFileSystemInfo = FileSystemInfo;
  SpaceAvailablePathEdit->Text = SpaceAvailablePath;
  UpdateControls();
  ShowModal();
}
//---------------------------------------------------------------------
AnsiString __fastcall TFileSystemInfoDialog::CapabilityStr(TFSCapability Capability)
{
  return BooleanToStr(FFileSystemInfo.IsCapable[Capability]);
}
//---------------------------------------------------------------------
AnsiString __fastcall TFileSystemInfoDialog::CapabilityStr(TFSCapability Capability1,
  TFSCapability Capability2)
{
  return FORMAT("%s/%s", (CapabilityStr(Capability1), CapabilityStr(Capability2)));
}
//---------------------------------------------------------------------
AnsiString __fastcall TFileSystemInfoDialog::SpaceStr(__int64 Bytes)
{
  AnsiString Result;
  if (Bytes == 0)
  {
    Result = LoadStr(FSINFO_BYTES_UNKNOWN);
  }
  else
  {
    Result = FormatBytes(Bytes);
    AnsiString SizeUnorderedStr = FormatBytes(Bytes, false);
    if (Result != SizeUnorderedStr)
    {
      Result = FORMAT("%s (%s)", (Result, SizeUnorderedStr));
    }
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::Feed(TFeedFileSystemData AddItem)
{
  AddItem(ServerView, FSINFO_REMOTE_SYSTEM, FFileSystemInfo.RemoteSystem);
  AddItem(ServerView, FSINFO_SESSION_PROTOCOL, FSessionInfo.ProtocolName);
  AddItem(ServerView, FSINFO_SSH_IMPLEMENTATION, FSessionInfo.SshImplementation);

  AnsiString Str = FSessionInfo.CSCipher;
  if (FSessionInfo.CSCipher != FSessionInfo.SCCipher)
  {
    Str += FORMAT("/%s", (FSessionInfo.SCCipher));
  }
  AddItem(ServerView, FSINFO_CIPHER, Str);

  Str = DefaultStr(FSessionInfo.CSCompression, LoadStr(NO_STR));
  if (FSessionInfo.CSCompression != FSessionInfo.SCCompression)
  {
    Str += FORMAT("/%s", (DefaultStr(FSessionInfo.SCCompression, LoadStr(NO_STR))));
  }
  AddItem(ServerView, FSINFO_COMPRESSION, Str);
  if (FSessionInfo.ProtocolName != FFileSystemInfo.ProtocolName)
  {
    AddItem(ServerView, FSINFO_FS_PROTOCOL, FFileSystemInfo.ProtocolName);
  }

  AddItem(HostKeyFingerprintEdit, 0, FSessionInfo.HostKeyFingerprint);

  AddItem(ProtocolView, FSINFO_MODE_CHANGING, CapabilityStr(fcModeChanging));
  AddItem(ProtocolView, FSINFO_OWNER_GROUP_CHANGING, CapabilityStr(fcGroupChanging));
  AnsiString AnyCommand;
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
  int Label, AnsiString Value)
{
  if (FLastFeededControl != Control)
  {
    // TODO, we should clear excess list view items here, but it
    // actually should not happen as of now
    FLastFeededControl = Control;
    FLastListItem = 0;
  }

  if (Control == HostKeyFingerprintEdit)
  {
    EnableControl(HostKeyGroup, !Value.IsEmpty());
    HostKeyGroup->Visible = !Value.IsEmpty();
    HostKeyFingerprintEdit->Text = Value;
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
    assert(ListView != NULL);

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
  int Label, AnsiString Value)
{
  if (Control->Enabled && !Value.IsEmpty())
  {
    if (FLastFeededControl != Control)
    {
      if (FLastFeededControl != NULL)
      {
        FClipboard += AnsiString::StringOfChar('-', 60) + "\r\n";
      }
      FLastFeededControl = Control;
    }

    if (dynamic_cast<TListView *>(Control) == NULL)
    {
      TGroupBox * Group = dynamic_cast<TGroupBox *>(Control->Parent);
      assert(Group != NULL);
      if ((Value.Length() >= 2) && (Value.SubString(Value.Length() - 1, 2) == "\r\n"))
      {
        Value.SetLength(Value.Length() - 2);
      }
      FClipboard += FORMAT("%s\r\n%s\r\n", (Group->Caption, Value));
    }
    else
    {
      assert(dynamic_cast<TListView *>(Control) != NULL);
      FClipboard += FORMAT("%s = %s\r\n", (LoadStr(Label), Value));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::ClipboardButtonClick(
  TObject * /*Sender*/)
{
  NeedSpaceAvailable();
  FLastFeededControl = NULL;
  FClipboard = "";
  Feed(ClipboardAddItem);
  CopyToClipboard(FClipboard);
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::CopyClick(TObject * Sender)
{
  TComponent * Item = dynamic_cast<TComponent *>(Sender);
  assert(Item != NULL);
  TPopupMenu * PopupMenu = dynamic_cast<TPopupMenu *>(Item->Owner);
  assert(PopupMenu != NULL);
  TListView * ListView = dynamic_cast<TListView *>(PopupMenu->PopupComponent);
  assert(ListView != NULL);

  AnsiString Text;
  for (int Index = 0; Index < ListView->Items->Count; Index++)
  {
    TListItem * Item = ListView->Items->Item[Index];
    if (Item->Selected)
    {
      Text += FORMAT("%s = %s\r\n", (Item->Caption, Item->SubItems->Strings[0]));
    }
  }
  CopyToClipboard(Text);
}
//---------------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::FormShow(TObject * /*Sender*/)
{
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
  assert(FOnGetSpaceAvailable != NULL);
  assert(!SpaceAvailablePathEdit->Text.IsEmpty());

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
