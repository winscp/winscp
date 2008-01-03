//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <StrUtils.hpp>
#include <CoreMain.h>
#include <Common.h>
#include <PuttyTools.h>
#include <TextsWin.h>
#include <TextsCore.h>
#include <HelpWin.h>
#include <VCLCommon.h>

#include "Login.h"
#include "WinInterface.h"
#include "GUITools.h"
#include "Tools.h"
#include "Setup.h"
#include "CustomWinConfiguration.h"
//---------------------------------------------------------------------
#pragma link "ComboEdit"
#pragma link "LogSettings"
#pragma link "GeneralSettings"
#pragma link "UpDownEdit"
#pragma link "PasswordEdit"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
// Sheet tag:
// 01 top, 02 indented
//---------------------------------------------------------------------------
bool __fastcall DoLoginDialog(TStoredSessionList *SessionList,
  TSessionData * Data, int Options)
{
  assert(Data);
  TLoginDialog *LoginDialog = new TLoginDialog(Application);
  bool Result;
  try
  {
    LoginDialog->StoredSessions = SessionList;
    LoginDialog->SessionData = Data;
    LoginDialog->Options = Options;
    Result = LoginDialog->Execute();
    if (Result)
    {
      Data->Assign(LoginDialog->SessionData);
    };
  }
  __finally
  {
    delete LoginDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
static const TFSProtocol FSOrder[] = { fsSFTPonly, fsSCPonly, fsFTP, fsExternalSSH, fsExternalSFTP };
//---------------------------------------------------------------------
__fastcall TLoginDialog::TLoginDialog(TComponent* AOwner)
        : TForm(AOwner)
{
  FSessionData = new TSessionData("");
  NoUpdate = 0;
  FLanguagesPopupMenu = NULL;
  FInitialized = false;
  FSavedTab = NULL;
  FSavedSession = -1;
  FOptions = loStartup;
  FLocaleChanging = false;
  FColor = (TColor)0;
  FEditingSessionData = NULL;
  FTreeLabels = new TStringList();
  InitControls();
}
//---------------------------------------------------------------------
__fastcall TLoginDialog::~TLoginDialog()
{
  // SelectItem event is called after destructor! Why?
  SessionListView->Selected = NULL;
  assert(FSystemSettings);
  DeleteSystemSettings(this, FSystemSettings);
  FSystemSettings = NULL;
  delete FTreeLabels;
  delete FSessionData;
  delete FLanguagesPopupMenu;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::ShowTabs(bool Show)
{
  for (int Index = 0; Index < PageControl->PageCount; Index++)
  {
    PageControl->Pages[Index]->TabVisible = Show;
  }
  // change form height by height of hidden tabs
  ClientHeight += (Show ? 1 : -1) * 45;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::InitControls()
{
  InitializeBugsCombo(BugIgnore1Combo);
  InitializeBugsCombo(BugPlainPW1Combo);
  InitializeBugsCombo(BugRSA1Combo);
  InitializeBugsCombo(BugHMAC2Combo);
  InitializeBugsCombo(BugDeriveKey2Combo);
  InitializeBugsCombo(BugRSAPad2Combo);
  InitializeBugsCombo(BugRekey2Combo);
  InitializeBugsCombo(BugPKSessID2Combo);

  InitializeBugsCombo(SFTPBugSymlinkCombo);
  InitializeBugsCombo(SFTPBugUtfCombo);
  InitializeBugsCombo(SFTPBugSignedTSCombo);

  InstallPathWordBreakProc(RemoteDirectoryEdit);
  InstallPathWordBreakProc(LocalDirectoryEdit);
  InstallPathWordBreakProc(PrivateKeyEdit);
  InstallPathWordBreakProc(RecycleBinPathEdit);

  TunnelLocalPortNumberEdit->Items->BeginUpdate();
  try
  {
    AnsiString TunnelLocalPortNumberAutoassign = TunnelLocalPortNumberEdit->Items->Strings[0];
    TunnelLocalPortNumberEdit->Items->Clear();
    TunnelLocalPortNumberEdit->Items->Add(TunnelLocalPortNumberAutoassign);
    for (int Index = Configuration->TunnelLocalPortNumberLow;
         Index <= Configuration->TunnelLocalPortNumberHigh; Index++)
    {
      TunnelLocalPortNumberEdit->Items->Add(IntToStr(Index));
    }
  }
  __finally
  {
    TunnelLocalPortNumberEdit->Items->EndUpdate();
  }
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::Init()
{
  UseSystemSettings(this, &FSystemSettings);
  Caption = FORMAT("%s %s", (AppName, Caption));

  InitControls();

  FTreeLabels->Clear();
  int Index = 0;
  while (Index < PageControl->PageCount)
  {
    FTreeLabels->Add(PageControl->Pages[Index]->Hint);
    PageControl->Pages[Index]->Hint = "";
    Index++;
  }

  UpdateNavigationTree();

  if ((Options & loLocalDirectory) == 0)
  {
    LocalDirectoryLabel->Visible = false;
    LocalDirectoryEdit->Visible = false;
    LocalDirectoryDescLabel->Visible = false;
    DirectoriesGroup->Height = RemoteDirectoryEdit->Top + RemoteDirectoryEdit->Height + 12;
    DirectoryOptionsGroup->Top = DirectoriesGroup->Top + DirectoriesGroup->Height + 8;
  }

  if (FLAGCLEAR(Options, loExternalProtocols))
  {
    assert(TransferProtocolCombo->Items->Count == FSPROTOCOL_COUNT - 1);
    TransferProtocolCombo->Items->Delete(TransferProtocolCombo->Items->Count - 2);
    TransferProtocolCombo->Items->Delete(TransferProtocolCombo->Items->Count - 1);
  }

  #ifdef NO_FILEZILLA
  assert(FLAGCLEAR(Options, loExternalProtocols));
  assert(TransferProtocolCombo->Items->Count == FSPROTOCOL_COUNT - 2 - 1);
  TransferProtocolCombo->Items->Delete(TransferProtocolCombo->Items->Count - 1);
  #endif

  ShowTabs(false);

  if (StoredSessions && StoredSessions->Count &&
      (FSessionData->Name == StoredSessions->DefaultSettings->Name))
  {
    ChangePage(SessionListSheet);
    SessionListView->SetFocus();
    assert(SessionListView->Items->Count > 0);
    if (SessionListView->Items->Count > 0)
    {
      SessionListView->ItemIndex = 0;
      SessionListView->ItemFocused = SessionListView->Selected;
    }
  }
  else
  {
    EditSession();
  }

  UpdateControls();
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::InitializeBugsCombo(TComboBox * BugsCombo)
{
  int PrevIndex = BugsCombo->ItemIndex;
  BugsCombo->Items->BeginUpdate();
  try
  {
    BugsCombo->Clear();
    BugsCombo->Items->Add(LoadStr(LOGIN_BUG_AUTO));
    BugsCombo->Items->Add(LoadStr(LOGIN_BUG_OFF));
    BugsCombo->Items->Add(LoadStr(LOGIN_BUG_ON));
  }
  __finally
  {
    BugsCombo->Items->EndUpdate();
  }
  assert(PrevIndex < BugsCombo->Items->Count);
  BugsCombo->ItemIndex = PrevIndex;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::LoadSessions()
{
  SessionListView->Items->BeginUpdate();
  try
  {
    SessionListView->Items->Clear();
    if (StoredSessions)
    {
      for (int Index = 0; Index < StoredSessions->Count; Index++)
      {
        TListItem *Item;
        Item = SessionListView->Items->Add();
        LoadSessionItem(Item);
      }
    }
  }
  __finally
  {
    SessionListView->Items->EndUpdate();
  }
  SelectedSession = StoredSessions->Count > 0 ?
    dynamic_cast<TSessionData*>(StoredSessions->AtObject(0)) : NULL;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::Default()
{
  if (StoredSessions)
  {
    FSessionData->Assign(StoredSessions->DefaultSettings);
  }
  else
  {
    FSessionData->Default();
  }
  FEditingSessionData = NULL;
  LoadSession(FSessionData);
  FCurrentSessionName = "";
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::LoadSession(TSessionData * aSessionData)
{
  // it was always true, and now must be true because of ping/keepalive settings
  assert(aSessionData == FSessionData);

  NoUpdate++;
  try
  {
    FFSProtocol = aSessionData->FSProtocol;

    // Basic tab
    UserNameEdit->Text = aSessionData->UserName;
    PortNumberEdit->AsInteger = aSessionData->PortNumber;
    HostNameEdit->Text = aSessionData->HostName;
    PasswordEdit->Text = aSessionData->Password;
    PrivateKeyEdit->Text = aSessionData->PublicKeyFile;
    FColor = (TColor)aSessionData->Color;

    bool AllowScpFallback;
    TransferProtocolCombo->ItemIndex =
      FSProtocolToIndex(aSessionData->FSProtocol, AllowScpFallback);
    AllowScpFallbackCheck->Checked = AllowScpFallback;

    // Directories tab
    LocalDirectoryEdit->Text = aSessionData->LocalDirectory;
    RemoteDirectoryEdit->Text = aSessionData->RemoteDirectory;
    UpdateDirectoriesCheck->Checked = aSessionData->UpdateDirectories;
    CacheDirectoriesCheck->Checked = aSessionData->CacheDirectories;
    CacheDirectoryChangesCheck->Checked = aSessionData->CacheDirectoryChanges;
    PreserveDirectoryChangesCheck->Checked = aSessionData->PreserveDirectoryChanges;
    ResolveSymlinksCheck->Checked = aSessionData->ResolveSymlinks;

    // Environment tab
    switch (aSessionData->DSTMode)
    {
      case dstmWin:
        DSTModeWinCheck->Checked = true;
        break;

      case dstmKeep:
        DSTModeKeepCheck->Checked = true;
        break;

      default:
      case dstmUnix:
        DSTModeUnixCheck->Checked = true;
        break;
    }
    if (aSessionData->EOLType == eolLF)
    {
      EOLTypeLFButton->Checked = true;
    }
    else
    {
      EOLTypeCRLFButton->Checked = true;
    }
    DeleteToRecycleBinCheck->Checked = aSessionData->DeleteToRecycleBin;
    OverwrittenToRecycleBinCheck->Checked = aSessionData->OverwrittenToRecycleBin;
    RecycleBinPathEdit->Text = aSessionData->RecycleBinPath;

    // SFTP tab
    #define LOAD_SFTP_BUG_COMBO(BUG) \
      SFTPBug ## BUG ## Combo->ItemIndex = 2 - aSessionData->SFTPBug[sb ## BUG]; \
      if (SFTPBug ## BUG ## Combo->ItemIndex < 0) SFTPBug ## BUG ## Combo->ItemIndex = 0
    LOAD_SFTP_BUG_COMBO(Symlink);
    LOAD_SFTP_BUG_COMBO(Utf);
    LOAD_SFTP_BUG_COMBO(SignedTS);
    #undef LOAD_SFTP_BUG_COMBO

    SFTPMaxVersionCombo->ItemIndex = aSessionData->SFTPMaxVersion;

    // Authentication tab
    AuthTISCheck->Checked = aSessionData->AuthTIS;
    AuthKICheck->Checked = aSessionData->AuthKI;
    AuthKIPasswordCheck->Checked = aSessionData->AuthKIPassword;
    AuthGSSAPICheck->Checked = aSessionData->AuthGSSAPI;
    AgentFwdCheck->Checked = aSessionData->AgentFwd;
    GSSAPIFwdTGTCheck->Checked = aSessionData->GSSAPIFwdTGT;
    GSSAPIServerRealmEdit->Text = aSessionData->GSSAPIServerRealm;

    // SSH tab
    Ssh2LegacyDESCheck->Checked = aSessionData->Ssh2DES;
    CompressionCheck->Checked = aSessionData->Compression;

    switch (aSessionData->SshProt) {
      case ssh1only:  SshProt1onlyButton->Checked = true; break;
      case ssh1:      SshProt1Button->Checked = true; break;
      case ssh2:      SshProt2Button->Checked = true; break;
      case ssh2only:  SshProt2onlyButton->Checked = true; break;
    }

    CipherListBox->Items->Clear();
    assert(CIPHER_NAME_WARN+CIPHER_COUNT-1 == CIPHER_NAME_DES);
    for (int Index = 0; Index < CIPHER_COUNT; Index++)
    {
      CipherListBox->Items->AddObject(
        LoadStr(CIPHER_NAME_WARN+aSessionData->Cipher[Index]),
        (TObject*)aSessionData->Cipher[Index]);
    }

    // KEX tab

    KexListBox->Items->Clear();
    assert(KEX_NAME_WARN+KEX_COUNT-1 == KEX_NAME_DHGEX);
    for (int Index = 0; Index < KEX_COUNT; Index++)
    {
      KexListBox->Items->AddObject(
        LoadStr(KEX_NAME_WARN+aSessionData->Kex[Index]),
        (TObject*)aSessionData->Kex[Index]);
    }

    RekeyTimeEdit->AsInteger = aSessionData->RekeyTime;
    RekeyDataEdit->Text = aSessionData->RekeyData;

    // Connection tab
    FtpPasvModeCheck->Checked = aSessionData->FtpPasvMode;

    LoadPing(aSessionData);
    TimeoutEdit->AsInteger = aSessionData->Timeout;

    switch (aSessionData->AddressFamily)
    {
      case afIPv4:
        IPv4Button->Checked = true;
        break;

      case afIPv6:
        IPv6Button->Checked = true;
        break;

      case afAuto:
      default:
        IPAutoButton->Checked = true;
        break;
    }

    // Shell tab
    if (aSessionData->DefaultShell)
    {
      ShellEdit->Text = ShellEdit->Items->Strings[0];
    }
    else
    {
      ShellEdit->Text = aSessionData->Shell;
    }
    // hide selection, which is wrongly shown initially even when the box has not focus
    ShellEdit->SelLength = 0;

    if (aSessionData->DetectReturnVar)
    {
      ReturnVarEdit->Text = ReturnVarEdit->Items->Strings[0];
    }
    else
    {
      ReturnVarEdit->Text = aSessionData->ReturnVar;
    }
    ReturnVarEdit->SelLength = 0;

    LookupUserGroupsCheck->Checked = aSessionData->LookupUserGroups;
    ClearAliasesCheck->Checked = aSessionData->ClearAliases;
    IgnoreLsWarningsCheck->Checked = aSessionData->IgnoreLsWarnings;
    Scp1CompatibilityCheck->Checked = aSessionData->Scp1Compatibility;
    UnsetNationalVarsCheck->Checked = aSessionData->UnsetNationalVars;
    AliasGroupListCheck->Checked = aSessionData->AliasGroupList;
    SCPLsFullTimeAutoCheck->Checked = (aSessionData->SCPLsFullTime != asOff);
    int TimeDifferenceMin = DateTimeToTimeStamp(aSessionData->TimeDifference).Time / 60000;
    if (double(aSessionData->TimeDifference) < 0)
    {
      TimeDifferenceMin = -TimeDifferenceMin;
    }
    TimeDifferenceEdit->AsInteger = TimeDifferenceMin / 60;
    TimeDifferenceMinutesEdit->AsInteger = TimeDifferenceMin % 60;

    // Proxy tab
    switch (aSessionData->ProxyMethod) {
      case pmHTTP: ProxyHTTPButton->Checked = true; break;
      case pmSocks4: ProxySocks4Button->Checked = true; break;
      case pmSocks5: ProxySocks5Button->Checked = true; break;
      case pmTelnet: ProxyTelnetButton->Checked = true; break;
      default: ProxyNoneButton->Checked = true; break;
    }
    ProxyHostEdit->Text = aSessionData->ProxyHost;
    ProxyPortEdit->AsInteger = aSessionData->ProxyPort;
    ProxyUsernameEdit->Text = aSessionData->ProxyUsername;
    ProxyPasswordEdit->Text = aSessionData->ProxyPassword;
    ProxyTelnetCommandEdit->Text = aSessionData->ProxyTelnetCommand;
    ProxyLocalhostCheck->Checked = aSessionData->ProxyLocalhost;
    switch (aSessionData->ProxyDNS) {
      case asOn: ProxyDNSOnButton->Checked = true; break;
      case asOff: ProxyDNSOffButton->Checked = true; break;
      default: ProxyDNSAutoButton->Checked = true; break;
    }

    // Bugs tab
    #define LOAD_BUG_COMBO(BUG) \
      Bug ## BUG ## Combo->ItemIndex = 2 - aSessionData->Bug[sb ## BUG]; \
      if (Bug ## BUG ## Combo->ItemIndex < 0) Bug ## BUG ## Combo->ItemIndex = 0
    LOAD_BUG_COMBO(Ignore1);
    LOAD_BUG_COMBO(PlainPW1);
    LOAD_BUG_COMBO(RSA1);
    LOAD_BUG_COMBO(HMAC2);
    LOAD_BUG_COMBO(DeriveKey2);
    LOAD_BUG_COMBO(RSAPad2);
    LOAD_BUG_COMBO(Rekey2);
    LOAD_BUG_COMBO(PKSessID2);
    #undef LOAD_BUG_COMBO

    // Tunnel tab
    TunnelCheck->Checked = aSessionData->Tunnel;
    TunnelUserNameEdit->Text = aSessionData->TunnelUserName;
    TunnelPortNumberEdit->AsInteger = aSessionData->TunnelPortNumber;
    TunnelHostNameEdit->Text = aSessionData->TunnelHostName;
    TunnelPasswordEdit->Text = aSessionData->TunnelPassword;
    TunnelPrivateKeyEdit->Text = aSessionData->TunnelPublicKeyFile;
    if (aSessionData->TunnelAutoassignLocalPortNumber)
    {
      TunnelLocalPortNumberEdit->Text = TunnelLocalPortNumberEdit->Items->Strings[0];
    }
    else
    {
      TunnelLocalPortNumberEdit->Text = IntToStr(aSessionData->TunnelLocalPortNumber);
    }
    // hide selection, which is wrongly shown initially even when the box has not focus
    TunnelLocalPortNumberEdit->SelLength = 0;
  }
  __finally
  {
    NoUpdate--;
    UpdateControls();
  }

  FCurrentSessionName = aSessionData->Name;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::LoadPing(TSessionData * aSessionData)
{
  switch (FFSProtocol == fsFTP ? aSessionData->FtpPingType : aSessionData->PingType)
  {
    case ptNullPacket:
      PingNullPacketButton->Checked = true;
      break;

    case ptDummyCommand:
      PingDummyCommandButton->Checked = true;
      break;

    default:
      PingOffButton->Checked = true;
      break;
  }
  PingIntervalSecEdit->AsInteger =
    (FFSProtocol == fsFTP ? aSessionData->FtpPingInterval : aSessionData->PingInterval);
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::SaveSession(TSessionData * aSessionData)
{
  // it was always true, and now must be true because of ping/keepalive settings
  assert(aSessionData == FSessionData);

  aSessionData->Name = FCurrentSessionName;
  // Basic tab
  aSessionData->UserName = UserNameEdit->Text.Trim();
  aSessionData->PortNumber = PortNumberEdit->AsInteger;
  // must be loaded after UserName, because HostName may be in format user@host
  aSessionData->HostName = HostNameEdit->Text.Trim();
  aSessionData->Password = PasswordEdit->Text;
  aSessionData->PublicKeyFile = PrivateKeyEdit->Text;
  aSessionData->Color = FColor;

  aSessionData->FSProtocol = IndexToFSProtocol(
    TransferProtocolCombo->ItemIndex, AllowScpFallbackCheck->Checked);

  // SSH tab
  aSessionData->Compression = CompressionCheck->Checked;
  aSessionData->Ssh2DES = Ssh2LegacyDESCheck->Checked;

  if (SshProt1onlyButton->Checked) aSessionData->SshProt = ssh1only;
    else
  if (SshProt1Button->Checked) aSessionData->SshProt = ssh1;
    else
  if (SshProt2Button->Checked) aSessionData->SshProt = ssh2;
    else aSessionData->SshProt = ssh2only;

  for (int Index = 0; Index < CIPHER_COUNT; Index++)
  {
    aSessionData->Cipher[Index] = (TCipher)CipherListBox->Items->Objects[Index];
  }

  // Kex tab

  for (int Index = 0; Index < KEX_COUNT; Index++)
  {
    aSessionData->Kex[Index] = (TKex)KexListBox->Items->Objects[Index];
  }

  aSessionData->RekeyTime = RekeyTimeEdit->AsInteger;
  aSessionData->RekeyData = RekeyDataEdit->Text;

  // Authentication tab
  aSessionData->AuthTIS = AuthTISCheck->Checked;
  aSessionData->AuthKI = AuthKICheck->Checked;
  aSessionData->AuthKIPassword = AuthKIPasswordCheck->Checked;
  aSessionData->AuthGSSAPI = AuthGSSAPICheck->Checked;
  aSessionData->AgentFwd = AgentFwdCheck->Checked;
  aSessionData->GSSAPIFwdTGT = GSSAPIFwdTGTCheck->Checked;
  aSessionData->GSSAPIServerRealm = GSSAPIServerRealmEdit->Text;

  // Connection tab
  aSessionData->FtpPasvMode = FtpPasvModeCheck->Checked;
  SavePing(aSessionData);
  aSessionData->Timeout = TimeoutEdit->AsInteger;

  if (IPv4Button->Checked)
  {
    aSessionData->AddressFamily = afIPv4;
  }
  else if (IPv6Button->Checked)
  {
    aSessionData->AddressFamily = afIPv6;
  }
  else
  {
    aSessionData->AddressFamily = afAuto;
  }

  // Directories tab
  aSessionData->LocalDirectory = LocalDirectoryEdit->Text;
  aSessionData->RemoteDirectory = RemoteDirectoryEdit->Text;
  aSessionData->UpdateDirectories = UpdateDirectoriesCheck->Checked;
  aSessionData->CacheDirectories = CacheDirectoriesCheck->Checked;
  aSessionData->CacheDirectoryChanges = CacheDirectoryChangesCheck->Checked;
  aSessionData->PreserveDirectoryChanges = PreserveDirectoryChangesCheck->Checked;
  aSessionData->ResolveSymlinks = ResolveSymlinksCheck->Checked;

  // Environment tab
  if (DSTModeUnixCheck->Checked) aSessionData->DSTMode = dstmUnix;
    else
  if (DSTModeKeepCheck->Checked) aSessionData->DSTMode = dstmKeep;
    else aSessionData->DSTMode = dstmWin;
  if (EOLTypeLFButton->Checked) aSessionData->EOLType = eolLF;
    else aSessionData->EOLType = eolCRLF;
  aSessionData->DeleteToRecycleBin = DeleteToRecycleBinCheck->Checked;
  aSessionData->OverwrittenToRecycleBin = OverwrittenToRecycleBinCheck->Checked;
  aSessionData->RecycleBinPath = RecycleBinPathEdit->Text;

  // SCP tab
  aSessionData->DefaultShell = (ShellEdit->Text == ShellEdit->Items->Strings[0]);
  aSessionData->Shell = (aSessionData->DefaultShell ? AnsiString() : ShellEdit->Text);
  aSessionData->DetectReturnVar = (ReturnVarEdit->Text == ReturnVarEdit->Items->Strings[0]);
  aSessionData->ReturnVar = (aSessionData->DetectReturnVar ? AnsiString() : ReturnVarEdit->Text);
  aSessionData->LookupUserGroups = LookupUserGroupsCheck->Checked;
  aSessionData->ClearAliases = ClearAliasesCheck->Checked;
  aSessionData->IgnoreLsWarnings = IgnoreLsWarningsCheck->Checked;
  aSessionData->Scp1Compatibility = Scp1CompatibilityCheck->Checked;
  aSessionData->UnsetNationalVars = UnsetNationalVarsCheck->Checked;
  aSessionData->AliasGroupList = AliasGroupListCheck->Checked;
  aSessionData->SCPLsFullTime = SCPLsFullTimeAutoCheck->Checked ? asAuto : asOff;
  aSessionData->TimeDifference =
    (double(TimeDifferenceEdit->AsInteger) / 24) +
    (double(TimeDifferenceMinutesEdit->AsInteger) / 24 / 60);

  // SFTP tab
  #define SAVE_SFTP_BUG_COMBO(BUG) aSessionData->SFTPBug[sb ## BUG] = (TAutoSwitch)(2 - SFTPBug ## BUG ## Combo->ItemIndex);
  SAVE_SFTP_BUG_COMBO(Symlink);
  SAVE_SFTP_BUG_COMBO(Utf);
  SAVE_SFTP_BUG_COMBO(SignedTS);
  #undef SAVE_SFTP_BUG_COMBO

  aSessionData->SFTPMaxVersion = SFTPMaxVersionCombo->ItemIndex;

  // Proxy tab
  if (ProxyHTTPButton->Checked) aSessionData->ProxyMethod = pmHTTP;
    else
  if (ProxySocks4Button->Checked) aSessionData->ProxyMethod = pmSocks4;
    else
  if (ProxySocks5Button->Checked) aSessionData->ProxyMethod = pmSocks5;
    else
  if (ProxyTelnetButton->Checked) aSessionData->ProxyMethod = pmTelnet;
    else aSessionData->ProxyMethod = pmNone;

  aSessionData->ProxyHost = ProxyHostEdit->Text;
  aSessionData->ProxyPort = ProxyPortEdit->AsInteger;
  aSessionData->ProxyUsername = ProxyUsernameEdit->Text;
  aSessionData->ProxyPassword = ProxyPasswordEdit->Text;
  aSessionData->ProxyTelnetCommand = ProxyTelnetCommandEdit->Text;
  aSessionData->ProxyLocalhost = ProxyLocalhostCheck->Checked;

  if (ProxyDNSOnButton->Checked) aSessionData->ProxyDNS = asOn;
    else
  if (ProxyDNSOffButton->Checked) aSessionData->ProxyDNS = asOff;
    else aSessionData->ProxyDNS = asAuto;

  // Bugs tab
  #define SAVE_BUG_COMBO(BUG) aSessionData->Bug[sb ## BUG] = (TAutoSwitch)(2 - Bug ## BUG ## Combo->ItemIndex);
  SAVE_BUG_COMBO(Ignore1);
  SAVE_BUG_COMBO(PlainPW1);
  SAVE_BUG_COMBO(RSA1);
  SAVE_BUG_COMBO(HMAC2);
  SAVE_BUG_COMBO(DeriveKey2);
  SAVE_BUG_COMBO(RSAPad2);
  SAVE_BUG_COMBO(Rekey2);
  SAVE_BUG_COMBO(PKSessID2);
  #undef SAVE_BUG_COMBO

  // Tunnel tab
  aSessionData->Tunnel = TunnelCheck->Checked;
  aSessionData->TunnelUserName = TunnelUserNameEdit->Text;
  aSessionData->TunnelPortNumber = TunnelPortNumberEdit->AsInteger;
  aSessionData->TunnelHostName = TunnelHostNameEdit->Text;
  aSessionData->TunnelPassword = TunnelPasswordEdit->Text;
  aSessionData->TunnelPublicKeyFile = TunnelPrivateKeyEdit->Text;
  if (TunnelLocalPortNumberEdit->Text == TunnelLocalPortNumberEdit->Items->Strings[0])
  {
    aSessionData->TunnelLocalPortNumber = 0;
  }
  else
  {
    aSessionData->TunnelLocalPortNumber = StrToIntDef(TunnelLocalPortNumberEdit->Text, 0);
  }
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::SavePing(TSessionData * aSessionData)
{
  TPingType PingType;
  if (PingNullPacketButton->Checked)
  {
    PingType = ptNullPacket;
  }
  else if (PingDummyCommandButton->Checked)
  {
    PingType = ptDummyCommand;
  }
  else
  {
    PingType = ptOff;
  }
  (FFSProtocol == fsFTP ? aSessionData->FtpPingType : aSessionData->PingType) =
    PingType;
  (FFSProtocol == fsFTP ? aSessionData->FtpPingInterval : aSessionData->PingInterval) =
    PingIntervalSecEdit->AsInteger;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::UpdateNavigationTree()
{
  TTreeNode * ActiveNode = NULL;

  NoUpdate++;
  try
  {
    int Index = 0;
    TTreeNode * PrevNode = NULL;
    while (Index < PageControl->PageCount)
    {
      TTabSheet * Tab = PageControl->Pages[Index];
      if (Tab->Enabled)
      {
        bool Indented = ((Tab->Tag % 100) == 2);
        AnsiString Label = FTreeLabels->Strings[Index];
        TTreeNode * Node;
        if (PrevNode == NULL)
        {
          assert(!Indented);
          Node = NavigationTree->Items->GetFirstNode();
        }
        else
        {
          if (Indented)
          {
            if (PrevNode->Level == 0)
            {
              Node = PrevNode->getFirstChild();
              if (Node == NULL)
              {
                Node = NavigationTree->Items->AddChild(PrevNode, Label);
              }
            }
            else
            {
              Node = PrevNode->getNextSibling();
              if (Node == NULL)
              {
                Node = NavigationTree->Items->Add(PrevNode, Label);
              }
            }
          }
          else
          {
            if (PrevNode->Level == 0)
            {
              // delete all excess children of previous top level node
              while ((Node = PrevNode->GetNext()) != PrevNode->getNextSibling())
              {
                Node->Delete();
              }

              Node = PrevNode->getNextSibling();
              if (Node == NULL)
              {
                Node = NavigationTree->Items->Add(PrevNode, Label);
              }
            }
            else
            {
              // delete all excess children of previous top level node
              while ((Node = PrevNode->getNextSibling()) != NULL)
              {
                Node->Delete();
              }

              Node = PrevNode->GetNext();
              if (Node == NULL)
              {
                Node = NavigationTree->Items->Add(NULL, Label);
              }
            }
          }
        }

        Node->Text = Label;
        Node->SelectedIndex = reinterpret_cast<int>(Tab);
        PrevNode = Node;
        if (PageControl->ActivePage == Tab)
        {
          Node->Selected = true;
          ActiveNode = Node;
        }
      }
      Index++;
    }

    TTreeNode * Node;
    while ((Node = PrevNode->GetNext()) != NULL)
    {
      Node->Delete();
    }

    NavigationTree->FullExpand();
  }
  __finally
  {
    NoUpdate--;
  }

  // node of active page was hidden
  if (ActiveNode == NULL)
  {
    ChangePage(BasicSheet);
  }
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::UpdateControls()
{
  if (Visible)
  {
    NoUpdate++;
    try
    {
      TFSProtocol FSProtocol = IndexToFSProtocol(
        TransferProtocolCombo->ItemIndex, AllowScpFallbackCheck->Checked);
      bool ExternalProtocol = (FSProtocol == fsExternalSSH) || (FSProtocol == fsExternalSFTP);
      bool InternalSshProtocol =
        (FSProtocol == fsSFTPonly) || (FSProtocol == fsSFTP) || (FSProtocol == fsSCPonly);
      bool SshProtocol = ExternalProtocol || InternalSshProtocol;
      bool FtpProtocol = (FSProtocol == fsFTP);
      bool Advanced = ShowAdvancedLoginOptionsCheck->Checked;

      // basic/session sheet
      AllowScpFallbackCheck->Visible =
        (IndexToFSProtocol(TransferProtocolCombo->ItemIndex, false) == fsSFTPonly);
      InsecureLabel->Visible = !SshProtocol;
      EnableControl(PrivateKeyEdit, SshProtocol);
      EnableControl(PrivateKeyLabel, PrivateKeyEdit->Enabled);

      // log sheet
      LogSheet->Enabled = Advanced;

      // connection sheet
      ConnSheet->Enabled = Advanced;
      EnableControl(FtpPasvModeCheck, FtpProtocol);
      if (FtpProtocol && !ProxyNoneButton->Checked && !FtpPasvModeCheck->Checked)
      {
        FtpPasvModeCheck->Checked = true;
        MessageDialog(LoadStr(FTP_PASV_MODE_REQUIRED), qtInformation, qaOK);
      }
      EnableControl(PingIntervalSecEdit, !PingOffButton->Checked);
      EnableControl(PingIntervalLabel, PingIntervalSecEdit->Enabled);
      EnableControl(PingNullPacketButton, SshProtocol);
      EnableControl(IPAutoButton, SshProtocol);

      // stored sessions sheet
      EnableControl(SessionListView, SessionListView->Items->Count);
      // keep 2 pixels on right free
      AdjustListColumnsWidth(SessionListView, -1, 2);

      // ssh/authentication sheet
      AuthSheet->Enabled = SshProtocol && Advanced;
      EnableControl(AuthTISCheck, !SshProt2onlyButton->Checked);
      EnableControl(AuthKICheck, !SshProt1onlyButton->Checked);
      EnableControl(AuthKIPasswordCheck,
        (AuthTISCheck->Enabled && AuthTISCheck->Checked) ||
        (AuthKICheck->Enabled && AuthKICheck->Checked));
      EnableControl(AuthGSSAPICheck, !SshProt1onlyButton->Checked);
      EnableControl(GSSAPIFwdTGTCheck,
        AuthGSSAPICheck->Enabled && AuthGSSAPICheck->Checked);
      EnableControl(GSSAPIServerRealmEdit,
        AuthGSSAPICheck->Enabled && AuthGSSAPICheck->Checked);
      EnableControl(GSSAPIServerRealmLabel, GSSAPIServerRealmEdit->Enabled);

      // ssh sheet
      AdvancedSheet->Enabled = SshProtocol;
      EnableControl(CipherUpButton, CipherListBox->ItemIndex > 0);
      EnableControl(CipherDownButton, CipherListBox->ItemIndex >= 0 &&
        CipherListBox->ItemIndex < CipherListBox->Items->Count-1);
      EnableControl(Ssh2LegacyDESCheck, !SshProt1onlyButton->Checked);

      // ssh/kex sheet
      KexSheet->Enabled = SshProtocol && Advanced && !SshProt1onlyButton->Checked &&
        (BugRekey2Combo->ItemIndex != 2);
      EnableControl(KexUpButton, KexListBox->ItemIndex > 0);
      EnableControl(KexDownButton, KexListBox->ItemIndex >= 0 &&
        KexListBox->ItemIndex < KexListBox->Items->Count-1);

      // ssh/bugs sheet
      BugsSheet->Enabled = SshProtocol && Advanced;
      EnableControl(BugIgnore1Combo, !SshProt2onlyButton->Checked);
      EnableControl(BugIgnore1Label, BugIgnore1Combo->Enabled);
      EnableControl(BugPlainPW1Combo, !SshProt2onlyButton->Checked);
      EnableControl(BugPlainPW1Label, BugPlainPW1Combo->Enabled);
      EnableControl(BugRSA1Combo, !SshProt2onlyButton->Checked);
      EnableControl(BugRSA1Label, BugRSA1Combo->Enabled);
      EnableControl(BugHMAC2Combo, !SshProt1onlyButton->Checked);
      EnableControl(BugHMAC2Label, BugHMAC2Combo->Enabled);
      EnableControl(BugDeriveKey2Combo, !SshProt1onlyButton->Checked);
      EnableControl(BugDeriveKey2Label, BugDeriveKey2Combo->Enabled);
      EnableControl(BugRSAPad2Combo, !SshProt1onlyButton->Checked);
      EnableControl(BugRSAPad2Label, BugRSAPad2Combo->Enabled);
      EnableControl(BugPKSessID2Combo, !SshProt1onlyButton->Checked);
      EnableControl(BugPKSessID2Label, BugPKSessID2Combo->Enabled);
      EnableControl(BugRekey2Combo, !SshProt1onlyButton->Checked);
      EnableControl(BugRekey2Label, BugRekey2Combo->Enabled);

      // environment/scp/shell sheet
      ScpSheet->Enabled = Advanced && InternalSshProtocol;

      // connection/proxy sheet
      ProxySheet->Enabled = Advanced;
      EnableControl(ProxyTelnetButton, SshProtocol);
      bool Proxy = !ProxyNoneButton->Checked &&
        (!ProxyTelnetButton->Checked || ProxyTelnetButton->Enabled);
      EnableControl(ProxyHostEdit, Proxy);
      EnableControl(ProxyHostLabel, ProxyHostEdit->Enabled);
      EnableControl(ProxyPortEdit, Proxy);
      EnableControl(ProxyPortLabel, ProxyPortEdit->Enabled);
      EnableControl(ProxyUsernameEdit, Proxy &&
        // FZAPI does not support username for SOCKS4
        ((ProxySocks4Button->Checked && SshProtocol) ||
         (ProxySocks5Button->Checked) ||
         (ProxyHTTPButton->Checked) ||
         (ProxyTelnetButton->Checked &&
          AnsiContainsText(ProxyTelnetCommandEdit->Text, "%user"))));
      EnableControl(ProxyUsernameLabel, ProxyUsernameEdit->Enabled);
      EnableControl(ProxyPasswordEdit, Proxy &&
        (!ProxySocks4Button->Checked &&
        (!ProxyTelnetButton->Checked ||
          AnsiContainsText(ProxyTelnetCommandEdit->Text, "%pass"))));
      EnableControl(ProxyPasswordLabel, ProxyPasswordEdit->Enabled);
      EnableControl(ProxySettingsGroup, Proxy && SshProtocol);
      EnableControl(ProxyTelnetCommandEdit, Proxy && ProxyTelnetButton->Checked);
      EnableControl(ProxyTelnetCommandLabel, ProxyTelnetCommandEdit->Enabled);

      // environment/directories sheet
      DirectoriesSheet->Enabled = !ExternalProtocol;
      DirectoryOptionsGroup->Visible = Advanced;
      EnableControl(CacheDirectoryChangesCheck,
        ((FSProtocol != fsSCPonly) || CacheDirectoriesCheck->Checked) && DirectoriesSheet->Enabled);
      EnableControl(PreserveDirectoryChangesCheck,
        CacheDirectoryChangesCheck->Enabled && CacheDirectoryChangesCheck->Checked &&
        DirectoriesSheet->Enabled);
      EnableControl(ResolveSymlinksCheck, (FSProtocol != fsFTP) && DirectoriesSheet->Enabled);

      // environment sheet
      EnvironmentSheet->Enabled = !ExternalProtocol;
      EnableControl(DSTModeGroup, (FSProtocol != fsFTP) && EnvironmentSheet->Enabled);
      UnixEnvironmentButton->Visible = !ShowAdvancedLoginOptionsCheck->Checked;
      WindowsEnvironmentButton->Visible = !ShowAdvancedLoginOptionsCheck->Checked;
      RecycleBinGroup->Visible = ShowAdvancedLoginOptionsCheck->Checked;
      EnableControl(OverwrittenToRecycleBinCheck, (FSProtocol != fsSCPonly) &&
        (FSProtocol != fsFTP) && EnvironmentSheet->Enabled);
      EnableControl(RecycleBinPathEdit,
        (DeleteToRecycleBinCheck->Enabled && DeleteToRecycleBinCheck->Checked) ||
        (OverwrittenToRecycleBinCheck->Enabled && OverwrittenToRecycleBinCheck->Checked) &&
        EnvironmentSheet->Enabled);
      EnableControl(RecycleBinPathLabel, RecycleBinPathEdit->Enabled &&
        EnvironmentSheet->Enabled);

      // environment/sftp sheet
      SftpSheet->Enabled = Advanced && ((FSProtocol == fsSFTPonly) || (FSProtocol == fsSFTP));

      // tunnel sheet
      TunnelSheet->Enabled = Advanced && InternalSshProtocol;
      EnableControl(TunnelSessionGroup, TunnelCheck->Enabled && TunnelCheck->Checked);
      EnableControl(TunnelOptionsGroup, TunnelCheck->Enabled && TunnelSessionGroup->Enabled);

      // preferences sheet
      GeneralSheet->Enabled = FLAGSET(Options, loPreferences);

      AboutButton->Visible = (Options & loAbout);
      LanguagesButton->Visible = (Options & loLanguage);
      ShellIconsButton->Visible = (Options & loTools);
      ToolsMenuButton->Visible = (Options & loTools);
      LoggingFrame->EnableLogWindow = (Options & loLogWindow);

      UpdateNavigationTree();
    }
    __finally
    {
      NoUpdate--;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::DataChange(TObject * /*Sender*/)
{
  if (!NoUpdate) UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::FormShow(TObject * /*Sender*/)
{
  if (!FInitialized)
  {
    FInitialized = true;
    Init();
    TSessionData * Data = GetSessionData();
    if (Data == FSessionData)
    {
      LoadSession(Data);
    }
    else
    {
      Default();
    }
  }
  if (FLocaleChanging)
  {
    Init();
    LoadSession(FSessionData);
    ChangePage(FSavedTab);
    SessionListView->ItemIndex = FSavedSession;
    LoadConfiguration();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionListViewSelectItem(TObject * /*Sender*/,
      TListItem * /*Item*/, bool /*Selected*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SetSessionData(TSessionData * value)
{
  FSessionData->Assign(value);
  FSessionData->Special = false;
  FEditingSessionData = value;
  LoadSession(FSessionData);
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TLoginDialog::GetSessionData()
{
  if (PageControl->ActivePage == SessionListSheet)
  {
    return SelectedSession;
  }
  else
  {
    SaveSession(FSessionData);
    return FSessionData;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SetStoredSessions(TStoredSessionList * value)
{
  if (FStoredSessions != value)
  {
    FStoredSessions = value;
    LoadSessions();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LoadSessionItem(TListItem * Item)
{
  Item->Data = StoredSessions->AtObject(Item->Index);
  Item->Caption = ((TSessionData*)Item->Data)->Name;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionListViewDblClick(TObject * /*Sender*/)
{
  if (SelectedSession)
  {
    if (SelectedSession->CanLogin) ModalResult = mrOk;
      else
    {
      SessionData = SelectedSession;
      EditSession();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SetSelectedSession(TSessionData * value)
{
  if (value)
  {
    int Index = StoredSessions->IndexOf(value);
    if (Index >= 0)
    {
      TListItem *Item = SessionListView->Items->Item[Index];
      Item->Focused = true;
      Item->Selected = true;
      Item->MakeVisible(false);
    }
  }
  else
  {
    SessionListView->Selected = NULL;
  }
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TLoginDialog::GetSelectedSession()
{
  if (SessionListView->Selected)
    return (TSessionData *)SessionListView->Selected->Data;
  else
    return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionListViewInfoTip(TObject * /*Sender*/,
      TListItem * Item, AnsiString & InfoTip)
{
  InfoTip = ((TSessionData*)Item->Data)->InfoTip;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionListViewKeyDown(TObject * /*Sender*/,
  WORD & Key, TShiftState /*Shift*/)
{
  if (!SessionListView->IsEditing())
  {
    if (Key == VK_DELETE)
    {
      DeleteSessionAction->Execute();
      Key = 0;
    }
    else if (Key == VK_F2)
    {
      RenameSessionAction->Execute();
      Key = 0;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::EditSession()
{
  ChangePage(BasicSheet);
  HostNameEdit->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::EditSessionActionExecute(TObject * /*Sender*/)
{
  if (SelectedSession)
  {
    SessionData = SelectedSession;
    EditSession();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveSessionActionExecute(TObject * /*Sender*/)
{
  AnsiString SessionName;
  SaveSession(FSessionData);
  if (FSessionData->Password.IsEmpty() ||
      Configuration->DisablePasswordStoring ||
      (MessageDialog(LoadStr(SAVE_PASSWORD), qtWarning, qaOK | qaCancel,
         HELP_SESSION_SAVE_PASSWORD) == qaOK))
  {
    SessionName = DoSaveSessionDialog(FSessionData->SessionName, FEditingSessionData);
    if (!SessionName.IsEmpty())
    {
      TListItem * Item;
      TSessionData *NewSession =
        StoredSessions->NewSession(SessionName, FSessionData);
      // modified only, explicit
      StoredSessions->Save(false, true);
      SaveConfiguration();
      // by now list must contais same number of items or one less
      assert(StoredSessions->Count == SessionListView->Items->Count ||
        StoredSessions->Count == SessionListView->Items->Count+1);
      if (StoredSessions->Count > SessionListView->Items->Count)
        Item = SessionListView->Items->Insert(StoredSessions->IndexOf(NewSession));
      else
        Item = SessionListView->Items->Item[StoredSessions->IndexOf(NewSession)];

      LoadSessionItem(Item);
      SelectedSession = NewSession;
      SessionData = NewSession;

      ChangePage(SessionListSheet);
      SessionListView->SetFocus();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::DeleteSessionActionExecute(TObject * /*Sender*/)
{
  if (SelectedSession &&
      (MessageDialog(FMTLOAD(CONFIRM_DELETE_SESSION, (SelectedSession->SessionName)),
         qtConfirmation, qaOK | qaCancel, HELP_DELETE_SESSION) == qaOK))
  {
    int PrevSelectedIndex = SessionListView->Selected->Index;
    if (FEditingSessionData == SelectedSession)
    {
      FEditingSessionData = NULL;
    }
    SelectedSession->Remove();
    StoredSessions->Remove(SelectedSession);
    SessionListView->Selected->Delete();
    if (SessionListView->Items->Count)
    {
      if (PrevSelectedIndex >= SessionListView->Items->Count)
        PrevSelectedIndex = SessionListView->Items->Count - 1;
      SelectedSession =
        (TSessionData *)StoredSessions->AtObject(PrevSelectedIndex);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ImportSessionsActionExecute(TObject * /*Sender*/)
{
  if (DoImportSessionsDialog(StoredSessions))
  {
    LoadSessions();
    if (SessionListView->Items->Count)
      SessionListView->Items->Item[0]->MakeVisible(False);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CleanUpActionExecute(TObject * /*Sender*/)
{
  if (DoCleanupDialog(StoredSessions, Configuration))
    LoadSessions();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::AboutActionExecute(TObject * /*Sender*/)
{
  DoAboutDialog(Configuration);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ActionListUpdate(TBasicAction *Action,
      bool &Handled)
{
  if (Action == EditSessionAction)
  {
    EditSessionAction->Enabled = SessionListView->Selected;
  }
  else if (Action == DeleteSessionAction)
  {
    TSessionData * Data = SessionData;
    DeleteSessionAction->Enabled =
      SessionListView->Selected && Data && !Data->Special;
  }
  else if (Action == RenameSessionAction)
  {
    TSessionData * Data = SessionData;
    RenameSessionAction->Enabled =
      SessionListView->Selected && Data && !Data->Special;
  }
  else if (Action == DesktopIconAction)
  {
    DesktopIconAction->Enabled = SessionListView->Selected;
  }
  else if (Action == SendToHookAction)
  {
    SendToHookAction->Enabled = SessionListView->Selected;
  }
  else if (Action == LoginAction)
  {
    TSessionData * Data = SessionData;
    LoginAction->Enabled = Data && Data->CanLogin;
  }
  else if (Action == SaveSessionAction)
  {
    SaveSessionAction->Enabled = (PageControl->ActivePage != SessionListSheet);
  }
  else if (Action == ShellIconSessionAction)
  {
    ShellIconSessionAction->Enabled = SessionListView->Selected;
  }
  Handled = true;

  if (!LoginButton->Default && !SessionListView->IsEditing())
  {
    LoginButton->Default = true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TLoginDialog::Execute()
{
  LoadConfiguration();
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    SaveConfiguration();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveConfiguration()
{
  assert(CustomWinConfiguration);
  CustomWinConfiguration->BeginUpdate();
  try
  {
    LoggingFrame->SaveConfiguration();
    GeneralSettingsFrame->SaveConfiguration();
    CustomWinConfiguration->ShowAdvancedLoginOptions = ShowAdvancedLoginOptionsCheck->Checked;
  }
  __finally
  {
    CustomWinConfiguration->EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LoadConfiguration()
{
  assert(CustomWinConfiguration);
  LoggingFrame->LoadConfiguration();
  GeneralSettingsFrame->LoadConfiguration();
  ShowAdvancedLoginOptionsCheck->Checked = CustomWinConfiguration->ShowAdvancedLoginOptions;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PreferencesButtonClick(TObject * /*Sender*/)
{
  ShowPreferencesDialog();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ShowPreferencesDialog()
{
  DoPreferencesDialog(pmLogin);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::NewSessionActionExecute(TObject * /*Sender*/)
{
  Default();
  EditSession();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::NavigationTreeChange(TObject * /*Sender*/,
      TTreeNode *Node)
{
  if (NoUpdate == 0)
  {
    TTabSheet * Tab = reinterpret_cast<TTabSheet *>(Node->SelectedIndex);
    // should happen only while loading language
    // (UpdateNavigationTree may not be called yet)
    if (Tab != NULL)
    {
      PageControl->ActivePage = Tab;
      // reshow the accelerators, etc
      ResetSystemSettings(this);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ChangePage(TTabSheet * Tab)
{
  PageControl->ActivePage = Tab;
  PageControlChange(PageControl);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PageControlChange(TObject *Sender)
{
  bool Found = false;
  if (PageControl->ActivePage)
  {
    for (int Index = 0; Index < NavigationTree->Items->Count; Index++)
    {
      if (NavigationTree->Items->Item[Index]->SelectedIndex ==
            reinterpret_cast<int>(PageControl->ActivePage))
      {
        NavigationTree->Items->Item[Index]->Selected = true;
        Found = true;
      }
    }
  }

  if (!Found)
  {
    assert(false);
    ChangePage(BasicSheet);
  }
  else
  {
    DataChange(Sender);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CMDialogKey(TWMKeyDown & Message)
{
  if (Message.CharCode == VK_TAB)
  {
    TShiftState Shift = KeyDataToShiftState(Message.KeyData);
    if (Shift.Contains(ssCtrl))
    {
      TTreeNode * Node = NavigationTree->Selected;
      if (!Shift.Contains(ssShift))
      {
        Node = Node->GetNext();
        if (!Node) Node = NavigationTree->Items->GetFirstNode();
      }
      else
      {
        if (Node->GetPrev()) Node = Node->GetPrev();
          else
        while (Node->GetNext()) Node = Node->GetNext();
      }
      Node->Selected = True;
      Message.Result = 1;
      return;
    }
  }
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::WMHelp(TWMHelp & Message)
{
  assert(Message.HelpInfo != NULL);

  if (Message.HelpInfo->iContextType == HELPINFO_WINDOW)
  {
    // invoke help for active page (not for whole form), regardless of focus
    // (e.g. even if focus is on control outside pagecontrol)
    Message.HelpInfo->hItemHandle = PageControl->ActivePage->Handle;
  }
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::Dispatch(void *Message)
{
  TMessage * M = reinterpret_cast<TMessage*>(Message);
  assert(M);
  if (M->Msg == CM_DIALOGKEY)
  {
    CMDialogKey(*((TWMKeyDown *)Message));
  }
  else if (M->Msg == WM_LOCALE_CHANGE)
  {
    if (M->WParam == 0)
    {
      SaveConfiguration();
      SaveSession(FSessionData);
      FSavedTab = PageControl->ActivePage;
      FSavedSession = SessionListView->ItemIndex;

      assert(FSystemSettings);
      RevokeSystemSettings(this, FSystemSettings);
      FSystemSettings = NULL;
      ShowTabs(true);

      Hide();
    }
    else
    {
      FLocaleChanging = true;
      try
      {
        Show();
      }
      __finally
      {
        FLocaleChanging = false;
      }
    }
  }
  else if (M->Msg == WM_HELP)
  {
    WMHelp(*((TWMHelp *)Message));
  }
  else
  {
    TForm::Dispatch(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::AlgListBoxStartDrag(TObject * Sender,
      TDragObject *& /*DragObject*/)
{
  FAlgDragSource = dynamic_cast<TListBox*>(Sender)->ItemIndex;
  FAlgDragDest = -1;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::AlgListBoxDragOver(TObject * Sender,
      TObject *Source, int X, int Y, TDragState /*State*/, bool &Accept)
{
  if (Source == Sender)
  {
    Accept = AllowAlgDrag(dynamic_cast<TListBox*>(Sender), X, Y);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::AlgListBoxDragDrop(TObject * Sender,
      TObject *Source, int X, int Y)
{
  if (Source == Sender)
  {
    TListBox * AlgListBox = dynamic_cast<TListBox*>(Sender);
    if (AllowAlgDrag(AlgListBox, X, Y))
    {
      AlgMove(AlgListBox, FAlgDragSource, FAlgDragDest);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CipherButtonClick(TObject *Sender)
{
  AlgMove(CipherListBox, CipherListBox->ItemIndex,
    CipherListBox->ItemIndex + (Sender == CipherUpButton ? -1 : 1));
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::KexButtonClick(TObject *Sender)
{
  AlgMove(KexListBox, KexListBox->ItemIndex,
    KexListBox->ItemIndex + (Sender == KexUpButton ? -1 : 1));
}
//---------------------------------------------------------------------------
bool __fastcall TLoginDialog::AllowAlgDrag(TListBox * AlgListBox, int X, int Y)
{
  FAlgDragDest = AlgListBox->ItemAtPos(TPoint(X, Y), true);
  return (FAlgDragDest >= 0) && (FAlgDragDest != FAlgDragSource);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::AlgMove(TListBox * AlgListBox, int Source, int Dest)
{
  if (Source >= 0 && Source < AlgListBox->Items->Count &&
      Dest >= 0 && Dest < AlgListBox->Items->Count)
  {
    AlgListBox->Items->Move(Source, Dest);
    AlgListBox->ItemIndex = Dest;
    AlgListBox->SetFocus();
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SetDefaultSessionActionExecute(
      TObject * /*Sender*/)
{
  if (MessageDialog(LoadStr(SET_DEFAULT_SESSION_SETTINGS), qtConfirmation,
        qaOK | qaCancel, HELP_SESSION_SAVE_DEFAULT) == qaOK)
  {
    SaveSession(FSessionData);
    StoredSessions->DefaultSettings = FSessionData;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ToolsMenuButtonClick(TObject * /*Sender*/)
{
  MenuPopup(ToolsPopupMenu, ToolsMenuButton);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ShellIconSessionActionExecute(
  TObject * /*Sender*/)
{
  MenuPopup(IconsPopupMenu, ShellIconsButton);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::DesktopIconActionExecute(TObject * /*Sender*/)
{
  if (MessageDialog(FMTLOAD(CONFIRM_CREATE_SHORTCUT, (SelectedSession->Name)),
        qtConfirmation, qaYes | qaNo, HELP_CREATE_SHORTCUT) == qaYes)
  {
    assert(SelectedSession);
    CreateDesktopShortCut(SelectedSession->Name, Application->ExeName,
      FORMAT("\"%s\" /UploadIfAny", (SelectedSession->Name)),
      FMTLOAD(SHORTCUT_INFO_TIP, (SelectedSession->Name, SelectedSession->InfoTip)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SendToHookActionExecute(TObject * /*Sender*/)
{
  if (MessageDialog(FMTLOAD(CONFIRM_CREATE_SENDTO, (SelectedSession->Name)),
        qtConfirmation, qaYes | qaNo, HELP_CREATE_SENDTO) == qaYes)
  {
    assert(SelectedSession);
    CreateDesktopShortCut(FMTLOAD(SESSION_SENDTO_HOOK_NAME, (SelectedSession->Name)),
      Application->ExeName,
      FORMAT("\"%s\" /Upload", (SelectedSession->Name)), "",
      CSIDL_SENDTO);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionListViewCustomDrawItem(
      TCustomListView *Sender, TListItem *Item, TCustomDrawState /*State*/,
      bool &DefaultDraw)
{
  TSessionData * Data = (TSessionData *)Item->Data;
  TFontStyles Styles = Sender->Canvas->Font->Style;
  if (Data->Special) Styles = Styles /*< fsItalic*/ << fsBold << fsUnderline;
    else Styles = Styles /*>> fsItalic*/ >> fsBold >> fsUnderline;
  Sender->Canvas->Font->Style = Styles;
  DefaultDraw = true;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CheckForUpdatesActionExecute(TObject * /*Sender*/)
{
  CheckForUpdates(false);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SetOptions(int value)
{
  if (Options != value)
  {
    FOptions = value;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LanguagesButtonClick(TObject * /*Sender*/)
{
  delete FLanguagesPopupMenu;
  FLanguagesPopupMenu = new TPopupMenu(this);

  TMenuItem * Item;
  TStrings * Locales = GUIConfiguration->Locales;
  for (int Index = 0; Index < Locales->Count; Index++)
  {
    Item = new TMenuItem(FLanguagesPopupMenu);
    FLanguagesPopupMenu->Items->Add(Item);
    Item->Caption = Locales->Strings[Index];
    Item->Tag = reinterpret_cast<int>(Locales->Objects[Index]);
    Item->OnClick = LocaleClick;
    Item->Checked = (reinterpret_cast<LCID>(Locales->Objects[Index]) ==
      GUIConfiguration->Locale);
  }
  Item = new TMenuItem(FLanguagesPopupMenu);
  FLanguagesPopupMenu->Items->Add(Item);
  Item->Caption = "-";

  Item = new TMenuItem(FLanguagesPopupMenu);
  FLanguagesPopupMenu->Items->Add(Item);
  Item->Caption = LoadStr(LOGIN_GET_LOCALES);
  Item->OnClick = LocaleGetClick;

  MenuPopup(FLanguagesPopupMenu, LanguagesButton);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LocaleClick(TObject * Sender)
{
  assert(Sender);
  GUIConfiguration->Locale =
    static_cast<LCID>(dynamic_cast<TComponent*>(Sender)->Tag);
  LanguagesButton->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LocaleGetClick(TObject * /*Sender*/)
{
  OpenBrowser(LoadStr(LOCALES_URL));
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::AuthGSSAPICheckClick(TObject * /*Sender*/)
{
  if (!NoUpdate)
  {
    UpdateControls();
    if (AuthGSSAPICheck->Checked && !Configuration->GSSAPIInstalled)
    {
      throw Exception(LoadStr(GSSAPI_NOT_INSTALLED));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::FormKeyDown(TObject * /*Sender*/, WORD &Key,
  TShiftState /*Shift*/)
{
  // we don't have "cancel" button, so we must handle "esc" ourselves
  if (Key == VK_ESCAPE)
  {
    ModalResult = mrCancel;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::VerifyKey(AnsiString FileName, bool TypeOnly)
{
  if (!FileName.Trim().IsEmpty())
  {
    FileName = ExpandEnvironmentVariables(FileName);
    TKeyType Type = KeyType(FileName);
    AnsiString Message;
    switch (Type)
    {
      case ktOpenSSH:
        Message = FMTLOAD(KEY_TYPE_UNSUPPORTED, (FileName, "OpenSSH SSH-2"));
        break;

      case ktSSHCom:
        Message = FMTLOAD(KEY_TYPE_UNSUPPORTED, (FileName, "ssh.com SSH-2"));
        break;

      case ktSSH1:
      case ktSSH2:
        // on file select do not check for SSH version as user may
        // intend to change it only after he/she selects key file
        if (!TypeOnly)
        {
          TSessionData * Data = SessionData;
          if ((Type == ktSSH1) !=
                ((Data->SshProt == ssh1only) || (Data->SshProt == ssh1)))
          {
            Message = FMTLOAD(KEY_TYPE_DIFFERENT_SSH,
              (FileName, (Type == ktSSH1 ? "SSH-1" : "PuTTY SSH-2")));
          }
        }
        break;

      default:
        assert(false);
        // fallthru
      case ktUnopenable:
      case ktUnknown:
        Message = FMTLOAD(KEY_TYPE_UNKNOWN, (FileName));
        break;
    }

    if (!Message.IsEmpty())
    {
      if (MessageDialog(Message, qtWarning, qaIgnore | qaAbort,
           HELP_LOGIN_KEY_TYPE) == qaAbort)
      {
        Abort();
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PrivateKeyEditAfterDialog(TObject * Sender,
  AnsiString & Name, bool & /*Action*/)
{
  if (CompareFileName(Name, ExpandEnvironmentVariables(FBeforeDialogPath)))
  {
    Name = FBeforeDialogPath;
  }

  TFilenameEdit * Edit = dynamic_cast<TFilenameEdit *>(Sender);
  if (Name != Edit->Text)
  {
    VerifyKey(Name, true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult != mrCancel)
  {
    VerifyKey(SessionData->PublicKeyFile, false);
    // for tunnel key do not check SSH version as it is not configurable
    VerifyKey(SessionData->TunnelPublicKeyFile, true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ColorButtonClick(TObject * /*Sender*/)
{
  ColorDefaultItem->Checked = (FColor == 0);
  PickColorItem->Checked = (FColor != 0);
  PickColorItem->ImageIndex = (FColor != 0 ? 0 : -1);
  ColorImageList->BkColor = FColor;

  MenuPopup(ColorPopupMenu, ColorButton);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ColorDefaultItemClick(TObject * /*Sender*/)
{
  FColor = (TColor)0;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PickColorItemClick(TObject * /*Sender*/)
{
  TColorDialog * Dialog = new TColorDialog(this);
  try
  {
    Dialog->Options = Dialog->Options << cdFullOpen;
    Dialog->Color = (FColor != 0 ? FColor : clSkyBlue);
    if (Dialog->Execute())
    {
      FColor = Dialog->Color;
    }
  }
  __finally
  {
    delete Dialog;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionListViewEditing(TObject * /*Sender*/,
  TListItem * Item, bool & AllowEdit)
{
  TSessionData * Data = static_cast<TSessionData *>(Item->Data);
  AllowEdit = !Data->Special;
  if (AllowEdit)
  {
    LoginButton->Default = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::RenameSessionActionExecute(TObject * /*Sender*/)
{
  if (SessionListView->Selected != NULL)
  {
    SessionListView->Selected->EditCaption();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionListViewEdited(TObject * /*Sender*/,
  TListItem * Item, AnsiString & S)
{
  if (Item->Caption != S)
  {
    TSessionData * Session = SelectedSession;

    SessionNameValidate(S, Session);

    // remove from storage
    Session->Remove();

    int PrevCount = StoredSessions->Count;
    TSessionData * NewSession = StoredSessions->NewSession(S, Session);
    // modified only explicit
    StoredSessions->Save(false, true);
    // the session may be the same, if only letter case has changed
    if (Session != NewSession)
    {
      SessionListView->Selected->Data = NewSession;
      // if we overwrite editing session, remove the original item
      // (we must preserve the one we are editing)
      if (PrevCount == StoredSessions->Count)
      {
        int Index = StoredSessions->IndexOf(NewSession);
        TListItem * OldItem = SessionListView->Items->Item[Index];
        assert(OldItem->Data == NewSession);
        OldItem->Delete();
      }
      StoredSessions->Remove(Session);
    }

    // sort items to the same order as in the container
    SessionListView->AlphaSort();

    SessionData = NewSession;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionListViewCompare(TObject * /*Sender*/,
  TListItem * Item1, TListItem * Item2, int /*Data*/, int & Compare)
{
  int Index1 = StoredSessions->IndexOf(static_cast<TSessionData*>(Item1->Data));
  int Index2 = StoredSessions->IndexOf(static_cast<TSessionData*>(Item2->Data));
  if (Index1 < Index2)
  {
    Compare = -1;
  }
  else if (Index1 > Index2)
  {
    Compare = 1;
  }
  else
  {
    Compare = 0;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::UnixEnvironmentButtonClick(TObject * /*Sender*/)
{
  EOLTypeLFButton->Checked = true;
  DSTModeUnixCheck->Checked = true;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::WindowsEnvironmentButtonClick(
  TObject * /*Sender*/)
{
  EOLTypeCRLFButton->Checked = true;
  DSTModeWinCheck->Checked = true;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PathEditBeforeDialog(TObject * /*Sender*/,
  AnsiString & Name, bool & /*Action*/)
{
  FBeforeDialogPath = Name;
  Name = ExpandEnvironmentVariables(Name);
}
//---------------------------------------------------------------------------
int __fastcall TLoginDialog::FSProtocolToIndex(TFSProtocol FSProtocol,
  bool & AllowScpFallback)
{
  if (FSProtocol == fsSFTP)
  {
    AllowScpFallback = true;
    bool Dummy;
    return FSProtocolToIndex(fsSFTPonly, Dummy);
  }
  else
  {
    AllowScpFallback = false;
    for (int Index = 0; Index < LENOF(FSOrder); Index++)
    {
      if ((FSOrder[Index] == FSProtocol) &&
          (Index < TransferProtocolCombo->Items->Count))
      {
        return Index;
      }
    }
    // SFTP is always present
    return FSProtocolToIndex(fsSFTP, AllowScpFallback);
  }
}
//---------------------------------------------------------------------------
TFSProtocol __fastcall TLoginDialog::IndexToFSProtocol(int Index, bool AllowScpFallback)
{
  bool InBounds = (Index >= 0) && (Index < LENOF(FSOrder));
  // can be temporary "unselected" while new language is being loaded
  assert(InBounds || (Index == -1));
  TFSProtocol Result = fsSFTP;
  if (InBounds)
  {
    Result = FSOrder[Index];
    if ((Result == fsSFTPonly) && AllowScpFallback)
    {
      Result = fsSFTP;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::TransferProtocolComboChange(TObject * Sender)
{
  if (!NoUpdate)
  {
    SavePing(FSessionData);

    FFSProtocol = IndexToFSProtocol(
      TransferProtocolCombo->ItemIndex, AllowScpFallbackCheck->Checked);

    LoadPing(FSessionData);
    if (FFSProtocol == fsFTP)
    {
      if (PortNumberEdit->AsInteger == 22)
      {
        PortNumberEdit->AsInteger = 21;
      }
    }
    else
    {
      if (PortNumberEdit->AsInteger == 21)
      {
        PortNumberEdit->AsInteger = 22;
      }
    }
  }
  DataChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::NavigationTreeCollapsing(
  TObject * /*Sender*/, TTreeNode * /*Node*/, bool & AllowCollapse)
{
  AllowCollapse = false;
}
//---------------------------------------------------------------------------
