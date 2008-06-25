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
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
// Sheet tag:
// 01 top, 02 indented
//---------------------------------------------------------------------------
bool __fastcall DoLoginDialog(TStoredSessionList *SessionList,
  TSessionData * Data, int Options)
{
  assert(Data);
  TLoginDialog * LoginDialog = SafeFormCreate<TLoginDialog>();
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
  FRecycleBinSheetVisible = false;
  FHintNode = NULL;
  FScrollOnDragOver = new TTreeViewScrollOnDragOver(SessionTree, true);

  // we need to make sure that window procedure is set asap
  // (so that CM_SHOWINGCHANGED handling is applied)
  UseSystemSettingsPre(this, &FSystemSettings);
  InitControls();
}
//---------------------------------------------------------------------
__fastcall TLoginDialog::~TLoginDialog()
{
  delete FScrollOnDragOver;
  assert(FSystemSettings);
  DeleteSystemSettings(this, FSystemSettings);
  FSystemSettings = NULL;
  delete FTreeLabels;
  delete FSessionData;
  delete FLanguagesPopupMenu;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::InitControls()
{
  LoggingFrame->Init();

  InitializeBugsCombo(UtfCombo);

  InitializeBugsCombo(BugIgnore1Combo);
  InitializeBugsCombo(BugPlainPW1Combo);
  InitializeBugsCombo(BugRSA1Combo);
  InitializeBugsCombo(BugHMAC2Combo);
  InitializeBugsCombo(BugDeriveKey2Combo);
  InitializeBugsCombo(BugRSAPad2Combo);
  InitializeBugsCombo(BugRekey2Combo);
  InitializeBugsCombo(BugPKSessID2Combo);

  InitializeBugsCombo(SFTPBugSymlinkCombo);
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

  HintLabel(ProxyTelnetCommandHintText, LoadStr(LOGIN_PROXY_COMMAND_PATTERNS_HINT));
  HintLabel(ProxyLocalCommandHintText, LoadStr(LOGIN_PROXY_COMMAND_PATTERNS_HINT));

  if (SessionTree->WindowProc != SessionTreeProc)
  {
    FOldSessionTreeProc = SessionTree->WindowProc;
    SessionTree->WindowProc = SessionTreeProc;
  }
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::Init()
{
  if (!FInitialized)
  {
    UseSystemSettingsPost(this, FSystemSettings);
  }
  else
  {
    UseSystemSettings(this, &FSystemSettings);
  }
  FInitialized = true;

  Caption = FORMAT("%s %s", (AppName, Caption));
  LinkLabel(RecycleBinLinkLabel);

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

  if (StoredSessions && StoredSessions->Count &&
      (FSessionData->Name == StoredSessions->DefaultSettings->Name))
  {
    ChangePage(SessionListSheet);
    ActiveControl = SessionTree;
    assert(SessionTree->Items->Count > 0);
    if (SessionTree->Items->Count > 0)
    {
      SessionTree->Selected = SessionTree->Items->GetFirstNode();
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
TTreeNode * __fastcall TLoginDialog::AddSessionPath(AnsiString Path)
{
  TTreeNode * Parent = NULL;
  while (!Path.IsEmpty())
  {
    AnsiString Folder = CutToChar(Path, '/', false);
    TTreeNode * Node =
      ((Parent == NULL) ? SessionTree->Items->GetFirstNode() : Parent->getFirstChild());
    // note that we allow folder with the same name as existing session
    // on the same level (see also SessionTreeEdited)
    while ((Node != NULL) && ((Node->Data != NULL) || !AnsiSameText(Node->Text, Folder)))
    {
      Node = Node->getNextSibling();
    }

    if (Node == NULL)
    {
      TTreeNode * AParent = Parent;
      Parent = SessionTree->Items->AddChild(Parent, Folder);
      UpdateFolderNode(Parent);
      // folders seem not to be sorted automatically (not having set the data property)
      if (AParent == NULL)
      {
        SessionTree->Items->AlphaSort();
      }
      else
      {
        AParent->AlphaSort();
      }
    }
    else
    {
      Parent = Node;
    }
  }
  return Parent;
}
//---------------------------------------------------------------------
TTreeNode * __fastcall TLoginDialog::AddSession(TSessionData * Data)
{
  TTreeNode * Parent = AddSessionPath(UnixExtractFilePath(Data->Name));
  TTreeNode * Node = SessionTree->Items->AddChild(Parent, UnixExtractFileName(Data->Name));
  Node->Data = Data;

  return Node;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::DestroySession(TSessionData * Data)
{
  if (FEditingSessionData == Data)
  {
    FEditingSessionData = NULL;
  }
  StoredSessions->Remove(Data);
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::LoadSessions()
{
  SessionTree->Items->BeginUpdate();
  try
  {
    SessionTree->Items->Clear();
    assert(StoredSessions != NULL);
    for (int Index = 0; Index < StoredSessions->Count; Index++)
    {
      AddSession(StoredSessions->Sessions[Index]);
    }
  }
  __finally
  {
    // folders seem not to be sorted automatically (not having set the data property)
    SessionTree->AlphaSort();
    SessionTree->Items->EndUpdate();
  }
  SessionTree->Selected = SessionTree->Items->GetFirstNode();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::UpdateFolderNode(TTreeNode * Node)
{
  Node->StateIndex = (Node->Expanded ? 2 : 3);
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
  // it was always true
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
      EOLTypeCombo->ItemIndex = 0;
    }
    else
    {
      EOLTypeCombo->ItemIndex = 1;
    }
    switch (aSessionData->Utf)
    {
      case asOn:
        UtfCombo->ItemIndex = 1;
        break;

      case asOff:
        UtfCombo->ItemIndex = 2;
        break;

      default:
        UtfCombo->ItemIndex = 0;
        break;
    }
    int TimeDifferenceMin = DateTimeToTimeStamp(aSessionData->TimeDifference).Time / 60000;
    if (double(aSessionData->TimeDifference) < 0)
    {
      TimeDifferenceMin = -TimeDifferenceMin;
    }
    TimeDifferenceEdit->AsInteger = TimeDifferenceMin / 60;
    TimeDifferenceMinutesEdit->AsInteger = TimeDifferenceMin % 60;

    // Environment/Recycle bin tab
    DeleteToRecycleBinCheck->Checked = aSessionData->DeleteToRecycleBin;
    OverwrittenToRecycleBinCheck->Checked = aSessionData->OverwrittenToRecycleBin;
    RecycleBinPathEdit->Text = aSessionData->RecycleBinPath;

    // SFTP tab
    if (aSessionData->SftpServer.IsEmpty())
    {
      SftpServerEdit->Text = SftpServerEdit->Items->Strings[0];
    }
    else
    {
      SftpServerEdit->Text = aSessionData->SftpServer;
    }
    // hide selection, which is wrongly shown initially even when the box has not focus
    SftpServerEdit->SelLength = 0;

    SFTPMaxVersionCombo->ItemIndex = aSessionData->SFTPMaxVersion;

    #define LOAD_SFTP_BUG_COMBO(BUG) \
      SFTPBug ## BUG ## Combo->ItemIndex = 2 - aSessionData->SFTPBug[sb ## BUG]; \
      if (SFTPBug ## BUG ## Combo->ItemIndex < 0) SFTPBug ## BUG ## Combo->ItemIndex = 0
    LOAD_SFTP_BUG_COMBO(Symlink);
    LOAD_SFTP_BUG_COMBO(SignedTS);
    #undef LOAD_SFTP_BUG_COMBO

    // FTP tab
    PostLoginCommandsMemo->Lines->Text = aSessionData->PostLoginCommands;

    // Authentication tab
    SshNoUserAuthCheck->Checked = aSessionData->SshNoUserAuth;
    TryAgentCheck->Checked = aSessionData->TryAgent;
    AuthTISCheck->Checked = aSessionData->AuthTIS;
    AuthKICheck->Checked = aSessionData->AuthKI;
    AuthKIPasswordCheck->Checked = aSessionData->AuthKIPassword;
    AuthGSSAPICheck2->Checked = aSessionData->AuthGSSAPI;
    AgentFwdCheck->Checked = aSessionData->AgentFwd;
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
    assert(CIPHER_NAME_WARN+CIPHER_COUNT-1 == CIPHER_NAME_ARCFOUR);
    for (int Index = 0; Index < CIPHER_COUNT; Index++)
    {
      CipherListBox->Items->AddObject(
        LoadStr(CIPHER_NAME_WARN+aSessionData->Cipher[Index]),
        (TObject*)aSessionData->Cipher[Index]);
    }

    // KEX tab

    KexListBox->Items->Clear();
    assert(KEX_NAME_WARN+KEX_COUNT-1 == KEX_NAME_GSSGEX);
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

    switch (aSessionData->PingType)
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
    PingIntervalSecEdit->AsInteger = aSessionData->PingInterval;
    switch (aSessionData->FtpPingType)
    {
      case ptDummyCommand:
        FtpPingDummyCommandButton->Checked = true;
        break;

      default:
        FtpPingOffButton->Checked = true;
        break;
    }
    FtpPingIntervalSecEdit->AsInteger = aSessionData->FtpPingInterval;
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

    ListingCommandEdit->Text = aSessionData->ListingCommand;
    ListingCommandEdit->SelLength = 0;

    LookupUserGroupsCheck->Checked = aSessionData->LookupUserGroups;
    ClearAliasesCheck->Checked = aSessionData->ClearAliases;
    IgnoreLsWarningsCheck->Checked = aSessionData->IgnoreLsWarnings;
    Scp1CompatibilityCheck->Checked = aSessionData->Scp1Compatibility;
    UnsetNationalVarsCheck->Checked = aSessionData->UnsetNationalVars;
    SCPLsFullTimeAutoCheck->Checked = (aSessionData->SCPLsFullTime != asOff);

    // Proxy tab
    SshProxyMethodCombo->ItemIndex = aSessionData->ProxyMethod;
    FtpProxyMethodCombo->ItemIndex = aSessionData->ProxyMethod;
    ProxyHostEdit->Text = aSessionData->ProxyHost;
    ProxyPortEdit->AsInteger = aSessionData->ProxyPort;
    ProxyUsernameEdit->Text = aSessionData->ProxyUsername;
    ProxyPasswordEdit->Text = aSessionData->ProxyPassword;
    ProxyTelnetCommandEdit->Text = aSessionData->ProxyTelnetCommand;
    ProxyLocalCommandEdit->Text = aSessionData->ProxyLocalCommand;
    ProxyLocalhostCheck->Checked = aSessionData->ProxyLocalhost;
    ProxyDNSCombo->ItemIndex = 2 - aSessionData->ProxyDNS;

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
void __fastcall TLoginDialog::SaveSession(TSessionData * aSessionData)
{
  // it was always true
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
  aSessionData->SshNoUserAuth = SshNoUserAuthCheck->Checked;
  aSessionData->TryAgent = TryAgentCheck->Checked;
  aSessionData->AuthTIS = AuthTISCheck->Checked;
  aSessionData->AuthKI = AuthKICheck->Checked;
  aSessionData->AuthKIPassword = AuthKIPasswordCheck->Checked;
  aSessionData->AuthGSSAPI = AuthGSSAPICheck2->Checked;
  aSessionData->AgentFwd = AgentFwdCheck->Checked;
  aSessionData->GSSAPIServerRealm = GSSAPIServerRealmEdit->Text;

  // Connection tab
  aSessionData->FtpPasvMode = FtpPasvModeCheck->Checked;
  if (PingNullPacketButton->Checked)
  {
    aSessionData->PingType = ptNullPacket;
  }
  else if (PingDummyCommandButton->Checked)
  {
    aSessionData->PingType = ptDummyCommand;
  }
  else
  {
    aSessionData->PingType = ptOff;
  }
  aSessionData->PingInterval = PingIntervalSecEdit->AsInteger;
  aSessionData->FtpPingType = (FtpPingDummyCommandButton->Checked ? ptDummyCommand : ptOff);
  aSessionData->FtpPingInterval = FtpPingIntervalSecEdit->AsInteger;
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
  if (EOLTypeCombo->ItemIndex == 0) aSessionData->EOLType = eolLF;
    else aSessionData->EOLType = eolCRLF;
  switch (UtfCombo->ItemIndex)
  {
    case 1:
      aSessionData->Utf = asOn;
      break;

    case 2:
      aSessionData->Utf = asOff;
      break;

    default:
      aSessionData->Utf = asAuto;
      break;
  }
  aSessionData->TimeDifference =
    (double(TimeDifferenceEdit->AsInteger) / 24) +
    (double(TimeDifferenceMinutesEdit->AsInteger) / 24 / 60);

  // Environment/Recycle bin tab
  aSessionData->DeleteToRecycleBin = DeleteToRecycleBinCheck->Checked;
  aSessionData->OverwrittenToRecycleBin = OverwrittenToRecycleBinCheck->Checked;
  aSessionData->RecycleBinPath = RecycleBinPathEdit->Text;

  // SCP tab
  aSessionData->DefaultShell = (ShellEdit->Text == ShellEdit->Items->Strings[0]);
  aSessionData->Shell = (aSessionData->DefaultShell ? AnsiString() : ShellEdit->Text);
  aSessionData->DetectReturnVar = (ReturnVarEdit->Text == ReturnVarEdit->Items->Strings[0]);
  aSessionData->ReturnVar = (aSessionData->DetectReturnVar ? AnsiString() : ReturnVarEdit->Text);
  aSessionData->ListingCommand = ListingCommandEdit->Text;
  aSessionData->LookupUserGroups = LookupUserGroupsCheck->Checked;
  aSessionData->ClearAliases = ClearAliasesCheck->Checked;
  aSessionData->IgnoreLsWarnings = IgnoreLsWarningsCheck->Checked;
  aSessionData->Scp1Compatibility = Scp1CompatibilityCheck->Checked;
  aSessionData->UnsetNationalVars = UnsetNationalVarsCheck->Checked;
  aSessionData->SCPLsFullTime = SCPLsFullTimeAutoCheck->Checked ? asAuto : asOff;

  // SFTP tab
  aSessionData->SftpServer =
    ((SftpServerEdit->Text == SftpServerEdit->Items->Strings[0]) ?
      AnsiString() : SftpServerEdit->Text);
  aSessionData->SFTPMaxVersion = SFTPMaxVersionCombo->ItemIndex;

  // FTP tab
  aSessionData->PostLoginCommands = PostLoginCommandsMemo->Lines->Text;

  #define SAVE_SFTP_BUG_COMBO(BUG) aSessionData->SFTPBug[sb ## BUG] = (TAutoSwitch)(2 - SFTPBug ## BUG ## Combo->ItemIndex);
  SAVE_SFTP_BUG_COMBO(Symlink);
  SAVE_SFTP_BUG_COMBO(SignedTS);
  #undef SAVE_SFTP_BUG_COMBO

  // Proxy tab
  aSessionData->ProxyMethod = (TProxyMethod)SshProxyMethodCombo->ItemIndex;
  aSessionData->ProxyHost = ProxyHostEdit->Text;
  aSessionData->ProxyPort = ProxyPortEdit->AsInteger;
  aSessionData->ProxyUsername = ProxyUsernameEdit->Text;
  aSessionData->ProxyPassword = ProxyPasswordEdit->Text;
  aSessionData->ProxyTelnetCommand = ProxyTelnetCommandEdit->Text;
  aSessionData->ProxyLocalCommand = ProxyLocalCommandEdit->Text;
  aSessionData->ProxyLocalhost = ProxyLocalhostCheck->Checked;
  aSessionData->ProxyDNS = (TAutoSwitch)(2 - ProxyDNSCombo->ItemIndex);

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
  if (Visible && FInitialized)
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
      assert(SshProtocol != FtpProtocol); // xor, some of the code below rely on this
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
      if (FtpProtocol && (FtpProxyMethodCombo->ItemIndex != pmNone) && !FtpPasvModeCheck->Checked)
      {
        FtpPasvModeCheck->Checked = true;
        MessageDialog(LoadStr(FTP_PASV_MODE_REQUIRED), qtInformation, qaOK);
      }
      PingGroup->Visible = SshProtocol;
      EnableControl(PingIntervalSecEdit, !PingOffButton->Checked);
      EnableControl(PingIntervalLabel, PingIntervalSecEdit->Enabled);
      FtpPingGroup->Visible = FtpProtocol;
      EnableControl(FtpPingIntervalSecEdit, !FtpPingOffButton->Checked);
      EnableControl(FtpPingIntervalLabel, FtpPingIntervalSecEdit->Enabled);
      EnableControl(IPAutoButton, SshProtocol);

      // stored sessions sheet
      EnableControl(SessionTree, SessionTree->Items->Count > 0);

      // ssh/authentication sheet
      AuthSheet->Enabled = SshProtocol && Advanced;
      EnableControl(SshNoUserAuthCheck, !SshProt1onlyButton->Checked);
      EnableControl(AuthenticationGroup,
        !SshNoUserAuthCheck->Enabled || !SshNoUserAuthCheck->Checked);
      EnableControl(AuthTISCheck, AuthenticationGroup->Enabled && !SshProt2onlyButton->Checked);
      EnableControl(AuthKICheck, AuthenticationGroup->Enabled && !SshProt1onlyButton->Checked);
      EnableControl(AuthKIPasswordCheck,
        AuthenticationGroup->Enabled &&
        ((AuthTISCheck->Enabled && AuthTISCheck->Checked) ||
         (AuthKICheck->Enabled && AuthKICheck->Checked)));
      EnableControl(AuthGSSAPICheck2,
        AuthenticationGroup->Enabled && !SshProt1onlyButton->Checked);
      EnableControl(GSSAPIServerRealmEdit,
        AuthGSSAPICheck2->Enabled && AuthGSSAPICheck2->Checked);
      EnableControl(GSSAPIServerRealmLabel2, GSSAPIServerRealmEdit->Enabled);

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

      // connection/proxy sheet
      TComboBox * ProxyMethodCombo = (SshProtocol ? SshProxyMethodCombo : FtpProxyMethodCombo);
      ProxyMethodCombo->Visible = true;
      ProxyMethodLabel->FocusControl = ProxyMethodCombo;
      TComboBox * OtherProxyMethodCombo = (!SshProtocol ? SshProxyMethodCombo : FtpProxyMethodCombo);
      OtherProxyMethodCombo->Visible = false;
      if (ProxyMethodCombo->ItemIndex >= OtherProxyMethodCombo->Items->Count)
      {
        OtherProxyMethodCombo->ItemIndex = pmNone;
      }
      else
      {
        OtherProxyMethodCombo->ItemIndex = ProxyMethodCombo->ItemIndex;
      }

      ProxySheet->Enabled = Advanced;
      bool Proxy = (ProxyMethodCombo->ItemIndex != pmNone);
      AnsiString ProxyCommand =
        ((ProxyMethodCombo->ItemIndex == pmCmd) ?
          ProxyLocalCommandEdit->Text : ProxyTelnetCommandEdit->Text);
      EnableControl(ProxyHostEdit, Proxy &&
        ((ProxyMethodCombo->ItemIndex != pmCmd) ||
         AnsiContainsText(ProxyCommand, "%proxyhost")));
      EnableControl(ProxyHostLabel, ProxyHostEdit->Enabled);
      EnableControl(ProxyPortEdit, Proxy &&
        ((ProxyMethodCombo->ItemIndex != pmCmd) ||
         AnsiContainsText(ProxyCommand, "%proxyport")));
      EnableControl(ProxyPortLabel, ProxyPortEdit->Enabled);
      EnableControl(ProxyUsernameEdit, Proxy &&
        // FZAPI does not support username for SOCKS4
        (((ProxyMethodCombo->ItemIndex == pmSocks4) && SshProtocol) ||
         (ProxyMethodCombo->ItemIndex == pmSocks5) ||
         (ProxyMethodCombo->ItemIndex == pmHTTP) ||
         (((ProxyMethodCombo->ItemIndex == pmTelnet) ||
           (ProxyMethodCombo->ItemIndex == pmCmd)) &&
          AnsiContainsText(ProxyCommand, "%user"))));
      EnableControl(ProxyUsernameLabel, ProxyUsernameEdit->Enabled);
      EnableControl(ProxyPasswordEdit, Proxy &&
        ((ProxyMethodCombo->ItemIndex == pmSocks5) ||
         (ProxyMethodCombo->ItemIndex == pmHTTP) ||
         (((ProxyMethodCombo->ItemIndex == pmTelnet) ||
           (ProxyMethodCombo->ItemIndex == pmCmd)) &&
          AnsiContainsText(ProxyCommand, "%pass"))));
      EnableControl(ProxyPasswordLabel, ProxyPasswordEdit->Enabled);
      bool ProxySettings = Proxy && SshProtocol;
      EnableControl(ProxySettingsGroup, ProxySettings);
      EnableControl(ProxyTelnetCommandEdit,
        ProxySettings && (ProxyMethodCombo->ItemIndex == pmTelnet));
      EnableControl(ProxyTelnetCommandLabel, ProxyTelnetCommandEdit->Enabled);
      EnableControl(ProxyTelnetCommandHintText, ProxyTelnetCommandEdit->Enabled);
      ProxyLocalCommandEdit->Visible = (ProxyMethodCombo->ItemIndex == pmCmd);
      ProxyLocalCommandLabel->Visible = ProxyLocalCommandEdit->Visible;
      ProxyLocalCommandBrowseButton->Visible = ProxyLocalCommandEdit->Visible;
      ProxyLocalCommandHintText->Visible = ProxyLocalCommandEdit->Visible;
      ProxyTelnetCommandEdit->Visible = !ProxyLocalCommandEdit->Visible;
      ProxyTelnetCommandLabel->Visible = ProxyTelnetCommandEdit->Visible;
      ProxyTelnetCommandHintText->Visible = ProxyTelnetCommandEdit->Visible;

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
      EnableControl(EOLTypeCombo, (FSProtocol != fsFTP) && EnvironmentSheet->Enabled);
      EnableControl(EOLTypeLabel, EOLTypeCombo->Enabled);
      EnableControl(DSTModeGroup, (FSProtocol != fsFTP) && EnvironmentSheet->Enabled);
      EnableControl(UtfCombo, (FSProtocol != fsSCPonly) && EnvironmentSheet->Enabled);
      EnableControl(UtfLabel, UtfCombo->Enabled);
      // should be enabled for fsSFTP (SCP fallback) too, but it would cause confusion
      EnableControl(TimeDifferenceEdit, (((FSProtocol == fsFTP) || (FSProtocol == fsSCPonly)) && EnvironmentSheet->Enabled));
      EnableControl(TimeDifferenceLabel, TimeDifferenceEdit->Enabled);
      EnableControl(TimeDifferenceHoursLabel, TimeDifferenceEdit->Enabled);
      EnableControl(TimeDifferenceMinutesEdit, TimeDifferenceEdit->Enabled);
      EnableControl(TimeDifferenceMinutesLabel, TimeDifferenceEdit->Enabled);
      RecycleBinLinkLabel->Visible = ShowAdvancedLoginOptionsCheck->Checked;
      EnvironmentOtherLabel->Visible = RecycleBinLinkLabel->Visible;

      // environment/recycle bin sheet
      RecycleBinSheet->Enabled = !ExternalProtocol &&
        ShowAdvancedLoginOptionsCheck->Checked && FRecycleBinSheetVisible;
      EnableControl(OverwrittenToRecycleBinCheck, (FSProtocol != fsSCPonly) &&
        (FSProtocol != fsFTP) && RecycleBinSheet->Enabled);
      EnableControl(RecycleBinPathEdit,
        (DeleteToRecycleBinCheck->Enabled && DeleteToRecycleBinCheck->Checked) ||
        (OverwrittenToRecycleBinCheck->Enabled && OverwrittenToRecycleBinCheck->Checked) &&
        RecycleBinSheet->Enabled);
      EnableControl(RecycleBinPathLabel, RecycleBinPathEdit->Enabled &&
        RecycleBinSheet->Enabled);

      // environment/sftp sheet
      SftpSheet->Enabled = Advanced && ((FSProtocol == fsSFTPonly) || (FSProtocol == fsSFTP));

      // environment/scp/shell
      ScpSheet->Enabled = Advanced && InternalSshProtocol;
      // disable also for SFTP with SCP fallback, as if someone wants to configure
      // these he/she probably intends to use SCP and should explicitly select it.
      // (note that these are not used for secondary shell session)
      EnableControl(ScpLsOptionsGroup, (FSProtocol == fsSCPonly));
      EnableControl(OtherShellOptionsGroup, (FSProtocol == fsSCPonly));

      // environment/ftp
      FtpSheet->Enabled = Advanced && FtpProtocol;

      // tunnel sheet
      TunnelSheet->Enabled = Advanced && InternalSshProtocol;
      // probably needless
      EnableControl(TunnelSessionGroup, TunnelCheck->Enabled && TunnelCheck->Checked);
      EnableControl(TunnelOptionsGroup, TunnelSessionGroup->Enabled);

      // preferences sheet
      GeneralSheet->Enabled = FLAGSET(Options, loPreferences);

      AboutButton->Visible = (Options & loAbout);
      LanguagesButton->Visible = (Options & loLanguage);
      ShellIconsButton->Visible = (Options & loTools);
      ToolsMenuButton->Visible = (Options & loTools);
      LoggingFrame->EnableLogWindow = (Options & loLogWindow);
      ColorButton->Visible = (Options & loColor);

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
    if (FSavedSession >= 0)
    {
      SessionTree->Selected = SessionTree->Items->Item[FSavedSession];
      SessionTree->Selected->MakeVisible();
    }
    LoadConfiguration();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeChange(TObject * /*Sender*/,
  TTreeNode * /*Node*/)
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
void __fastcall TLoginDialog::SessionTreeDblClick(TObject * /*Sender*/)
{
  TPoint P = SessionTree->ScreenToClient(Mouse->CursorPos);
  if (SelectedSession && (SessionTree->GetNodeAt(P.x, P.y) == SessionTree->Selected))
  {
    if (SelectedSession->CanLogin)
    {
      ModalResult = mrOk;
    }
    else
    {
      SessionData = SelectedSession;
      EditSession();
    }
  }
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TLoginDialog::GetSelectedSession()
{
  if (SessionTree->Selected != NULL)
  {
    return (TSessionData *)SessionTree->Selected->Data;
  }
  else
  {
    return NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeKeyDown(TObject * /*Sender*/,
  WORD & Key, TShiftState /*Shift*/)
{
  if (!SessionTree->IsEditing())
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
  SaveSession(FSessionData);

  bool SavePassword;
  bool * PSavePassword;

  if (Configuration->DisablePasswordStoring ||
      FSessionData->Password.IsEmpty())
  {
    PSavePassword = NULL;
  }
  else
  {
    PSavePassword = &SavePassword;
    SavePassword =
      (FEditingSessionData != NULL) &&
      (FEditingSessionData->Password == FSessionData->Password);
  }

  AnsiString SessionName = FSessionData->SessionName;
  // when saving new session, save by default to current session folder
  if (FCurrentSessionName.IsEmpty())
  {
    SessionName.Insert(
      UnixIncludeTrailingBackslash(SessionNodePath(CurrentSessionFolderNode())), 1);
  }
  if (DoSaveSessionDialog(SessionName, PSavePassword, FEditingSessionData))
  {
    if ((PSavePassword != NULL) && !*PSavePassword)
    {
      FSessionData->Password = "";
    }

    TSessionData * NewSession =
      StoredSessions->NewSession(SessionName, FSessionData);
    // modified only, explicit
    StoredSessions->Save(false, true);
    SaveConfiguration();

    TTreeNode * Node = SessionTree->Items->GetFirstNode();
    while ((Node != NULL) && (Node->Data != NewSession))
    {
      Node = Node->GetNext();
    }

    if (Node == NULL)
    {
      Node = AddSession(NewSession);
    }

    SessionTree->Selected = Node;
    SessionData = NewSession;

    ChangePage(SessionListSheet);
    SessionTree->SetFocus();
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TLoginDialog::SessionNodePath(TTreeNode * Node)
{
  AnsiString Path;
  if (Node != NULL)
  {
    Path = Node->Text;
    Node = Node->Parent;
    while (Node != NULL)
    {
      Path.Insert(UnixIncludeTrailingBackslash(Node->Text), 1);
      Node = Node->Parent;
    }
  }

  return Path;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::DeleteSessionActionExecute(TObject * /*Sender*/)
{
  assert(SessionTree->Selected != NULL);

  TSessionData * Session = SelectedSession;
  if (Session != NULL)
  {
    if (MessageDialog(FMTLOAD(CONFIRM_DELETE_SESSION, (Session->SessionName)),
          qtConfirmation, qaOK | qaCancel, HELP_DELETE_SESSION) == qaOK)
    {
      Session->Remove();
      DestroySession(Session);
      SessionTree->Selected->Delete();
    }
  }
  else
  {
    TTreeNode * Node = SessionTree->Selected;
    int Sessions = 0;
    TTreeNode * ANode = Node->GetNext();
    while ((ANode != NULL) && ANode->HasAsParent(Node))
    {
      if (ANode->Data != NULL)
      {
        TSessionData * Session = static_cast<TSessionData *>(ANode->Data);
        if (Session->Special)
        {
          SessionTree->Selected = ANode;
          ANode->MakeVisible();
          throw Exception(FMTLOAD(LOGIN_DELETE_SPECIAL_SESSION, (Session->SessionName)));
        }
        Sessions++;
      }
      ANode = ANode->GetNext();
    }

    AnsiString Path = SessionNodePath(Node);

    if ((Sessions == 0) ||
        (MessageDialog(FMTLOAD(LOGIN_DELETE_SESSION_FOLDER, (Path, Sessions)),
          qtConfirmation, qaOK | qaCancel, HELP_DELETE_SESSION_FOLDER) == qaOK))
    {
      Node = SessionTree->Selected;
      TTreeNode * ANode = Node->GetNext();
      while ((ANode != NULL) && ANode->HasAsParent(Node))
      {
        if (ANode->Data != NULL)
        {
          TSessionData * Session = static_cast<TSessionData *>(ANode->Data);
          Session->Remove();
          DestroySession(Session);
        }
        ANode = ANode->GetNext();
      }

      SessionTree->Selected->Delete();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ImportSessionsActionExecute(TObject * /*Sender*/)
{
  if (DoImportSessionsDialog(StoredSessions))
  {
    LoadSessions();
    if (SessionTree->Items->Count > 0)
    {
      SessionTree->Items->GetFirstNode()->MakeVisible();
    }
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
  bool SessionSelected =
    (SessionTree->Selected != NULL) && (SessionTree->Selected->Data != NULL);
  if (Action == EditSessionAction)
  {
    EditSessionAction->Enabled = SessionSelected;
  }
  else if (Action == DeleteSessionAction)
  {
    TSessionData * Data = SessionData;
    DeleteSessionAction->Enabled =
      (SessionSelected && !Data->Special) ||
      ((SessionTree->Selected != NULL) && (SessionTree->Selected->Data == NULL));
  }
  else if (Action == RenameSessionAction)
  {
    TSessionData * Data = SessionData;
    RenameSessionAction->Enabled =
      (SessionSelected && !Data->Special) ||
      ((SessionTree->Selected != NULL) && (SessionTree->Selected->Data == NULL));
  }
  else if (Action == DesktopIconAction)
  {
    DesktopIconAction->Enabled = SessionSelected;
  }
  else if (Action == SendToHookAction)
  {
    SendToHookAction->Enabled = SessionSelected;
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
    ShellIconSessionAction->Enabled = SessionSelected;
  }
  else if (Action == NewSessionFolderAction)
  {
    NewSessionFolderAction->Enabled = true;
  }
  Handled = true;

  if (!SessionTree->IsEditing())
  {
    // default property setter does not have guard for "the same value"
    if (!LoginButton->Default)
    {
      LoginButton->Default = true;
    }
    CloseButton->Cancel = true;
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
void __fastcall TLoginDialog::Dispatch(void * Message)
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
      FSavedSession = ((SessionTree->Selected != NULL) ?
        SessionTree->Selected->AbsoluteIndex : -1);

      assert(FSystemSettings);
      RevokeSystemSettings(this, FSystemSettings);
      FSystemSettings = NULL;

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
  else
  {
    Accept = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::AlgListBoxDragDrop(TObject * Sender,
  TObject * Source, int X, int Y)
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
void __fastcall TLoginDialog::SessionTreeCustomDrawItem(
  TCustomTreeView * Sender, TTreeNode * Node, TCustomDrawState State,
  bool & DefaultDraw)
{
  TSessionData * Data = (TSessionData *)Node->Data;
  TFontStyles Styles = Sender->Canvas->Font->Style;
  if ((Data != NULL) && Data->Special)
  {
    Styles = Styles << fsBold << fsUnderline;
  }
  else
  {
    if ((Data == NULL) && State.Empty() && !Node->DropTarget)
    {
      bool AnySession = false;
      TTreeNode * ANode = Node->GetNext();
      while (!AnySession && (ANode != NULL) && ANode->HasAsParent(Node))
      {
        AnySession = (ANode->Data != NULL);
        ANode = ANode->GetNext();
      }

      if (!AnySession)
      {
        Sender->Canvas->Font->Color = clGrayText;
      }
    }
    Styles = Styles >> fsBold >> fsUnderline;
  }
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
void __fastcall TLoginDialog::AuthGSSAPICheck2Click(TObject * /*Sender*/)
{
  if (!NoUpdate)
  {
    UpdateControls();
    if (AuthGSSAPICheck2->Checked && !Configuration->GSSAPIInstalled)
    {
      throw Exception(LoadStr(GSSAPI_NOT_INSTALLED2));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
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
void __fastcall TLoginDialog::SessionTreeEditing(TObject * /*Sender*/,
  TTreeNode * Node, bool & AllowEdit)
{
  TSessionData * Data = static_cast<TSessionData *>(Node->Data);
  AllowEdit = (Data == NULL) || !Data->Special;
  if (AllowEdit)
  {
    LoginButton->Default = false;
    CloseButton->Cancel = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::RenameSessionActionExecute(TObject * /*Sender*/)
{
  if (SessionTree->Selected != NULL)
  {
    SessionTree->Selected->EditText();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CheckDuplicateFolder(TTreeNode * Parent,
  AnsiString Text, TTreeNode * Node)
{
  TTreeNode * ANode =
    ((Parent == NULL) ? SessionTree->Items->GetFirstNode() :
     Parent->getFirstChild());
  // note that we allow folder with the same name as existing session
  // on the same level (see also AddSession)
  while ((ANode != NULL) &&
    ((ANode == Node) || (ANode->Data != NULL) || !AnsiSameText(ANode->Text, Text)))
  {
    ANode = ANode->getNextSibling();
  }

  if (ANode != NULL)
  {
    throw Exception(FMTLOAD(LOGIN_DUPLICATE_SESSION_FOLDER, (Text)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeEdited(TObject * /*Sender*/,
  TTreeNode * Node, AnsiString & S)
{
  if (Node->Text != S)
  {
    TSessionData * Session = SelectedSession;

    TSessionData::ValidateName(S);
    if (Session != NULL)
    {
      AnsiString Path = UnixExtractFilePath(Session->Name) + S;

      SessionNameValidate(Path, Session);

      // remove from storage
      Session->Remove();

      TSessionData * NewSession = StoredSessions->NewSession(Path, Session);
      // modified, only explicit
      StoredSessions->Save(false, true);
      // the session may be the same, if only letter case has changed
      if (Session != NewSession)
      {
        // if we overwrite existing session, remove the original item
        // (we must not delete the node we are editing)
        TTreeNode * ANode =
          ((Node->Parent == NULL) ? SessionTree->Items->GetFirstNode() :
           Node->Parent->getFirstChild());
        while ((ANode != NULL) && (ANode->Data != NewSession))
        {
          ANode = ANode->getNextSibling();
        }

        if (ANode != NULL)
        {
          ANode->Delete();
        }

        Node->Data = NewSession;

        DestroySession(Session);
      }

      SessionData = NewSession;
    }
    else
    {
      CheckDuplicateFolder(Node->Parent, S, Node);

      AnsiString ParentPath = UnixIncludeTrailingBackslash(SessionNodePath(Node->Parent));
      AnsiString OldRoot = ParentPath + Node->Text;
      AnsiString NewRoot = ParentPath + S;

      bool AnySession = false;

      TSortType PrevSortType = SessionTree->SortType;
      // temporarily disable automatic sorting, so that nodes are kept in order
      // while we traverse them. otherwise it may happen that we omit some.
      SessionTree->SortType = Comctrls::stNone;
      try
      {
        TTreeNode * ANode = Node->GetNext();
        while ((ANode != NULL) && ANode->HasAsParent(Node))
        {
          if (ANode->Data != NULL)
          {
            AnySession = true;
            TSessionData * Session = static_cast<TSessionData *>(ANode->Data);

            // remove from storage
            Session->Remove();

            AnsiString Path = Session->Name;
            assert(Path.SubString(1, OldRoot.Length()) == OldRoot);
            Path.Delete(1, OldRoot.Length());
            Path.Insert(NewRoot, 1);

            TSessionData * NewSession = StoredSessions->NewSession(Path, Session);

            // the session may be the same, if only letter case has changed
            if (NewSession != Session)
            {
              ANode->Data = NewSession;
              DestroySession(Session);
            }
          }

          ANode = ANode->GetNext();
        }
      }
      __finally
      {
        SessionTree->SortType = PrevSortType;
      }

      if (AnySession)
      {
        // modified, only explicit
        StoredSessions->Save(false, true);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::UnixEnvironmentButtonClick(TObject * /*Sender*/)
{
  EOLTypeCombo->ItemIndex = 0;
  DSTModeUnixCheck->Checked = true;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::WindowsEnvironmentButtonClick(
  TObject * /*Sender*/)
{
  EOLTypeCombo->ItemIndex = 1;
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
    for (int Index = 0; Index < TransferProtocolCombo->Items->Count; Index++)
    {
      if (FSOrder[Index] == FSProtocol)
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
    FFSProtocol = IndexToFSProtocol(
      TransferProtocolCombo->ItemIndex, AllowScpFallbackCheck->Checked);

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
void __fastcall TLoginDialog::RecycleBinLinkLabelClick(TObject * /*Sender*/)
{
  FRecycleBinSheetVisible = true;
  UpdateControls();
  ChangePage(RecycleBinSheet);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ProxyLocalCommandBrowseButtonClick(
  TObject * /*Sender*/)
{
  BrowseForExecutable(ProxyLocalCommandEdit,
    LoadStr(LOGIN_SELECT_LOCAL_PROXY),
    LoadStr(EXECUTABLE_FILTER), false, true);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeExpandedCollapsed(TObject * /*Sender*/,
  TTreeNode * Node)
{
  UpdateFolderNode(Node);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeCompare(TObject * /*Sender*/,
  TTreeNode * Node1, TTreeNode * Node2, int /*Data*/, int & Compare)
{
  if ((Node1->Data == NULL) && (Node2->Data != NULL))
  {
    Compare = -1;
  }
  else if ((Node1->Data != NULL) && (Node2->Data == NULL))
  {
    Compare = 1;
  }
  else if (Node1->Data == NULL)
  {
    Compare = AnsiCompareText(Node1->Text, Node2->Text);
  }
  else
  {
    Compare = NamedObjectSortProc(Node1->Data, Node2->Data);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::NewSessionFolderInputDialogInitialize(
  TObject * /*Sender*/, TInputDialogData * Data)
{
  TEdit * Edit = Data->Edit;
  int P = Edit->Text.LastDelimiter("/");
  if (P > 0)
  {
    Edit->SetFocus();
    Edit->SelStart = P;
    Edit->SelLength = Edit->Text.Length() - P;
  }
}
//---------------------------------------------------------------------------
TTreeNode * __fastcall TLoginDialog::SessionFolderNode(TTreeNode * Node)
{
  TTreeNode * Parent;
  if (Node == NULL)
  {
    Parent = NULL;
  }
  else if (Node->Data != NULL)
  {
    Parent = Node->Parent;
  }
  else
  {
    Parent = Node;
  }
  return Parent;
}
//---------------------------------------------------------------------------
TTreeNode * __fastcall TLoginDialog::CurrentSessionFolderNode()
{
  return SessionFolderNode(SessionTree->Selected);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::NewSessionFolderActionExecute(
  TObject * /*Sender*/)
{
  AnsiString Name =
    UnixIncludeTrailingBackslash(SessionNodePath(CurrentSessionFolderNode())) +
    LoadStr(NEW_FOLDER);
  if (InputDialog(LoadStr(LOGIN_NEW_SESSION_FOLDER_CAPTION),
        LoadStr(LOGIN_NEW_SESSION_FOLDER_PROMPT), Name, HELP_NEW_SESSION_FOLDER,
        NULL, true, NewSessionFolderInputDialogInitialize))
  {
    Name = UnixExcludeTrailingBackslash(Name);
    if (!Name.IsEmpty())
    {
      TTreeNode * Parent = AddSessionPath(UnixExtractFilePath(Name));
      CheckDuplicateFolder(Parent, UnixExtractFileName(Name), NULL);

      TTreeNode * Node = AddSessionPath(Name);
      SessionTree->Selected = Node;
      Node->MakeVisible();
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TLoginDialog::SessionAllowDrop(TTreeNode * DropTarget)
{
  assert(SessionTree->Selected != NULL);
  return (SessionTree->Selected->Parent != DropTarget);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeProc(TMessage & AMessage)
{
  if (AMessage.Msg == CM_DRAG)
  {
    TCMDrag & Message = reinterpret_cast<TCMDrag &>(AMessage);
    // reimplement dmDragMove to avoid TCustomTreeView.DoDragOver,
    // which resets DropTarget to pointed-to node
    // (note that this disables OnDragOver event handler)
    if ((Message.DragMessage == dmDragMove) ||
        (Message.DragMessage == dmDragEnter) ||
        (Message.DragMessage == dmDragLeave))
    {
      if (Message.DragMessage != dmDragMove)
      {
        // must call it at least for dmDragLeave, because it does some cleanup,
        // but we need to override result below, as it defaults to "not accepted"
        FOldSessionTreeProc(AMessage);
      }

      TDragControlObject * DragObject = dynamic_cast<TDragControlObject *>(Message.DragRec->Source);
      if ((DragObject != NULL) && (DragObject->Control == SessionTree))
      {
        TPoint P = SessionTree->ScreenToClient(Message.DragRec->Pos);
        TTreeNode * DropTarget = SessionFolderNode(SessionTree->GetNodeAt(P.x, P.y));
        if (!SessionAllowDrop(DropTarget))
        {
          DropTarget = NULL;
          Message.Result = 0;
        }
        else
        {
          Message.Result = 1;
        }

        if (Message.DragMessage == dmDragMove)
        {
          SessionTree->DropTarget = DropTarget;
        }
        FScrollOnDragOver->DragOver(P);
      }
      else
      {
        Message.Result = 0;
      }
    }
    else
    {
      FOldSessionTreeProc(AMessage);
    }
  }
  else
  {
    FOldSessionTreeProc(AMessage);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeStartDrag(TObject * /*Sender*/,
  TDragObject *& /*DragObject*/)
{
  assert(SessionTree->Selected != NULL);
  // neither session folders not special sessions can be dragged
  if ((SessionTree->Selected == NULL) ||
      (SessionTree->Selected->Data == NULL) ||
      ((SessionTree->Selected->Data != NULL) &&
       SelectedSession->Special))
  {
    Abort();
  }

  FScrollOnDragOver->StartDrag();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeDragDrop(TObject * Sender,
  TObject * Source, int /*X*/, int /*Y*/)
{
  if ((Sender == Source) &&
      (SessionTree->Selected != NULL) &&
      (SessionTree->Selected->Parent != SessionTree->DropTarget))
  {
    TTreeNode * DropTarget = SessionTree->DropTarget;
    TSessionData * Session = SelectedSession;
    AnsiString Path =
      UnixIncludeTrailingBackslash(SessionNodePath(DropTarget)) +
      UnixExtractFileName(Session->SessionName);

    SessionNameValidate(Path, Session);

    // remove from storage
    Session->Remove();

    TSessionData * NewSession = StoredSessions->NewSession(Path, Session);
    // modified, only explicit
    StoredSessions->Save(false, true);
    // this should aways be the case
    if (Session != NewSession)
    {
      TTreeNode * Node = SessionTree->Selected;

      // look for overwritten node (if any)
      TTreeNode * ANode = SessionTree->Items->GetFirstNode();
      while (ANode != NULL)
      {
        if (ANode->Data == NewSession)
        {
          ANode->Delete();
          break;
        }
        ANode = ANode->GetNext();
      }

      Node->MoveTo(DropTarget, naAddChild);
      Node->Data = NewSession;
      // try to make both visible
      if (DropTarget != NULL)
      {
        DropTarget->MakeVisible();
      }
      Node->MakeVisible();

      DestroySession(Session);
    }
    else
    {
      assert(false);
    }

    SessionData = NewSession;
  }
  else
  {
    assert(false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeMouseMove(TObject * /*Sender*/,
  TShiftState /*Shift*/, int X, int Y)
{
  TTreeNode * Node = SessionTree->GetNodeAt(X, Y);
  THitTests HitTest = SessionTree->GetHitTestInfoAt(X, Y);

  if (Node != FHintNode)
  {
    Application->CancelHint();

    AnsiString Hint;
    if (HitTest.Contains(htOnItem) || HitTest.Contains(htOnIcon) ||
        HitTest.Contains(htOnLabel) || HitTest.Contains(htOnStateIcon))
    {
      FHintNode = Node;
      if (Node->Data != NULL)
      {
        Hint = static_cast<TSessionData *>(Node->Data)->InfoTip;
      }
      else
      {
        Hint = "";
      }
    }
    else
    {
      FHintNode = NULL;
      Hint = "";
    }

    SessionTree->Hint = Hint;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeEndDrag(TObject * /*Sender*/,
  TObject * /*Target*/, int /*X*/, int /*Y*/)
{
  FScrollOnDragOver->EndDrag();
}
//---------------------------------------------------------------------------
