//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Terminal.h>
#include <VCLCommon.h>
#include "FileSystemInfo.h"
//---------------------------------------------------------------------
#pragma link "XPThemes"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
void __fastcall DoFileSystemInfoDialog(TTerminal * Terminal)
{
  TFileSystemInfoDialog * Dialog = new TFileSystemInfoDialog(Application);
  try
  {
    Dialog->Terminal = Terminal;
    Dialog->ShowModal();
  }
  __finally
  {
    delete Dialog;
  }
} 
//---------------------------------------------------------------------
__fastcall TFileSystemInfoDialog::TFileSystemInfoDialog(TComponent* AOwner)
	: TForm(AOwner)
{
  UseSystemSettings(this);
}
//---------------------------------------------------------------------
AnsiString __fastcall TFileSystemInfoDialog::CapabilityStr(TFSCapability Capability)
{
  assert(FTerminal);
  return BooleanToStr(FTerminal->IsCapable[Capability]);
}
//---------------------------------------------------------------------
AnsiString __fastcall TFileSystemInfoDialog::CapabilityStr(TFSCapability Capability1,
  TFSCapability Capability2)
{
  return FORMAT("%s/%s", (CapabilityStr(Capability1), CapabilityStr(Capability2)));
}
//---------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::UpdateControls()
{
  assert(Terminal);

  SshVersionEdit->Text = IntToStr(Terminal->SshVersion);
  SshImplementationEdit->Text = Terminal->SshImplementation;

  AnsiString Str = CipherNames[Terminal->CSCipher];
  if (Terminal->CSCipher != Terminal->SCCipher)
  {
    Str += FORMAT("/%s", (CipherNames[Terminal->SCCipher]));
  }
  CipherEdit->Text = Str;

  Str = BooleanToStr(Terminal->CSCompression != ctNone);
  if (Terminal->CSCompression != Terminal->SCCompression)
  {
    Str += FORMAT("/%s", (BooleanToStr(Terminal->SCCompression != ctNone)));
  }
  CompressionEdit->Text = Str;

  HostKeyFingerprintEdit->Text = Terminal->HostKeyFingerprint;

  FSProtocolEdit->Text = Terminal->ProtocolName;

  ModeChangingEdit->Text = CapabilityStr(fcModeChanging);
  OwnerGroupChangingEdit->Text = CapabilityStr(fcOwnerChanging, fcGroupChanging);
  AnyCommandEdit->Text = CapabilityStr(fcAnyCommand);
  SymbolicHardLinkEdit->Text = CapabilityStr(fcSymbolicLink, fcHardLink);
  UserGroupListingEdit->Text = CapabilityStr(fcUserGroupListing);
  RemoteCopyEdit->Text = CapabilityStr(fcRemoteCopy);
  NativeTextModeEdit->Text = CapabilityStr(fcNativeTextMode);

  InfoMemo->Lines = Terminal->AdditionalInfo;
}
//---------------------------------------------------------------------
void __fastcall TFileSystemInfoDialog::SetTerminal(TTerminal * value)
{
  if (Terminal != value)
  {
    FTerminal = value;
    UpdateControls();
  }
}
//---------------------------------------------------------------------

