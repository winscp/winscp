//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <StrUtils.hpp>
#include <CoreMain.h>
#include <Common.h>
#include <TextsWin.h>
#include <TextsCore.h>
#include <HelpWin.h>
#include <VCLCommon.h>

#include "WinInterface.h"
#include "SiteAdvanced.h"
#include "GUITools.h"
#include "Tools.h"
#include "WinConfiguration.h"
#include "PuttyTools.h"
//---------------------------------------------------------------------
#pragma link "ComboEdit"
#pragma link "PasswordEdit"
#pragma link "UpDownEdit"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
// Sheet tag:
// 01 top, 02 indented
//---------------------------------------------------------------------------
bool __fastcall DoSiteAdvancedDialog(TSessionData * SessionData, int Options)
{
  TSiteAdvancedDialog * SiteAdvancedDialog = new TSiteAdvancedDialog(Application, Options);
  bool Result;
  try
  {
    Result = SiteAdvancedDialog->Execute(SessionData);
  }
  __finally
  {
    delete SiteAdvancedDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
static const UnicodeString DefaultRecycleBinPath = L"/tmp";
//---------------------------------------------------------------------
__fastcall TSiteAdvancedDialog::TSiteAdvancedDialog(
    TComponent * AOwner, int Options) :
  TForm(AOwner)
{
  NoUpdate = 0;
  FOptions = Options;

  // we need to make sure that window procedure is set asap
  // (so that CM_SHOWINGCHANGED handling is applied)
  UseSystemSettings(this);
  InitControls();
  PageControl->ActivePage = EnvironmentSheet;
}
//---------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::InitControls()
{
  ComboAutoSwitchInitialize(UtfCombo);

  ComboAutoSwitchInitialize(BugIgnore1Combo);
  ComboAutoSwitchInitialize(BugPlainPW1Combo);
  ComboAutoSwitchInitialize(BugRSA1Combo);
  ComboAutoSwitchInitialize(BugHMAC2Combo);
  ComboAutoSwitchInitialize(BugDeriveKey2Combo);
  ComboAutoSwitchInitialize(BugRSAPad2Combo);
  ComboAutoSwitchInitialize(BugPKSessID2Combo);
  ComboAutoSwitchInitialize(BugRekey2Combo);
  ComboAutoSwitchInitialize(BugMaxPkt2Combo);
  ComboAutoSwitchInitialize(BugIgnore2Combo);
  ComboAutoSwitchInitialize(BugWinAdjCombo);

  ComboAutoSwitchInitialize(SFTPBugSymlinkCombo);
  ComboAutoSwitchInitialize(SFTPBugSignedTSCombo);

  ComboAutoSwitchInitialize(FtpListAllCombo);
  ComboAutoSwitchInitialize(FtpUseMlsdCombo);
  ComboAutoSwitchInitialize(FtpForcePasvIpCombo);
  ComboAutoSwitchInitialize(FtpHostCombo);

  TunnelLocalPortNumberEdit->Items->BeginUpdate();
  try
  {
    UnicodeString TunnelLocalPortNumberAutoassign = TunnelLocalPortNumberEdit->Items->Strings[0];
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

  UpdateNavigationTree();

  if (FLAGCLEAR(FOptions, loLocalDirectory))
  {
    LocalDirectoryLabel->Visible = false;
    LocalDirectoryEdit->Visible = false;
    LocalDirectoryDescLabel->Visible = false;
    DirectoriesGroup->Height = RemoteDirectoryEdit->Top + RemoteDirectoryEdit->Height + ScaleByTextHeight(this, 12);
    DirectoryOptionsGroup->Top = DirectoriesGroup->Top + DirectoriesGroup->Height + ScaleByTextHeight(this, 8);
  }

  ColorButton->Visible = (FOptions & loColor);

  SetSessionColor((TColor)0);
}
//---------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::LoadSession()
{

  {
    TAutoNestingCounter NoUpdateCounter(NoUpdate);

    // Directories page
    SynchronizeBrowsingCheck->Checked = FSessionData->SynchronizeBrowsing;
    LocalDirectoryEdit->Text = FSessionData->LocalDirectory;
    RemoteDirectoryEdit->Text = FSessionData->RemoteDirectory;
    UpdateDirectoriesCheck->Checked = FSessionData->UpdateDirectories;
    CacheDirectoriesCheck->Checked = FSessionData->CacheDirectories;
    CacheDirectoryChangesCheck->Checked = FSessionData->CacheDirectoryChanges;
    PreserveDirectoryChangesCheck->Checked = FSessionData->PreserveDirectoryChanges;
    ResolveSymlinksCheck->Checked = FSessionData->ResolveSymlinks;

    // Environment page
    switch (FSessionData->DSTMode)
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

    if (FSessionData->EOLType == eolLF)
    {
      EOLTypeCombo->ItemIndex = 0;
    }
    else
    {
      EOLTypeCombo->ItemIndex = 1;
    }

    switch (FSessionData->NotUtf)
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

    int TimeDifferenceMin = TimeToMinutes(FSessionData->TimeDifference);
    TimeDifferenceEdit->AsInteger = TimeDifferenceMin / MinsPerHour;
    TimeDifferenceMinutesEdit->AsInteger = TimeDifferenceMin % MinsPerHour;
    TimeDifferenceAutoCheck->Checked = FSessionData->TimeDifferenceAuto;

    TrimVMSVersionsCheck->Checked = FSessionData->TrimVMSVersions;

    // Environment/Recycle bin page
    DeleteToRecycleBinCheck->Checked = FSessionData->DeleteToRecycleBin;
    OverwrittenToRecycleBinCheck->Checked = FSessionData->OverwrittenToRecycleBin;
    RecycleBinPathEdit->Text =
      !FSessionData->RecycleBinPath.IsEmpty() ?
        FSessionData->RecycleBinPath : DefaultRecycleBinPath;

    // SFTP page
    if (FSessionData->SftpServer.IsEmpty())
    {
      SftpServerEdit->Text = SftpServerEdit->Items->Strings[0];
    }
    else
    {
      SftpServerEdit->Text = FSessionData->SftpServer;
    }
    // hide selection, which is wrongly shown initially even when the box has not focus
    SftpServerEdit->SelLength = 0;
    AllowScpFallbackCheck->Checked = (FSessionData->FSProtocol == fsSFTP);

    SFTPMaxVersionCombo->ItemIndex = FSessionData->SFTPMaxVersion;

    #define LOAD_SFTP_BUG_COMBO(BUG) \
      ComboAutoSwitchLoad(SFTPBug ## BUG ## Combo, FSessionData->SFTPBug[sb ## BUG])
    LOAD_SFTP_BUG_COMBO(Symlink);
    LOAD_SFTP_BUG_COMBO(SignedTS);
    #undef LOAD_SFTP_BUG_COMBO

    // FTP page
    FtpAccountEdit->Text = FSessionData->FtpAccount;
    PostLoginCommandsMemo->Lines->Text = FSessionData->PostLoginCommands;
    ComboAutoSwitchLoad(FtpListAllCombo, FSessionData->FtpListAll);
    ComboAutoSwitchLoad(FtpUseMlsdCombo, FSessionData->FtpUseMlsd);
    ComboAutoSwitchLoad(FtpForcePasvIpCombo, FSessionData->FtpForcePasvIp);
    ComboAutoSwitchLoad(FtpHostCombo, FSessionData->FtpHost);

    // Authentication page
    SshNoUserAuthCheck->Checked = FSessionData->SshNoUserAuth;
    TryAgentCheck->Checked = FSessionData->TryAgent;
    AuthTISCheck->Checked = FSessionData->AuthTIS;
    AuthKICheck->Checked = FSessionData->AuthKI;
    AuthKIPasswordCheck->Checked = FSessionData->AuthKIPassword;
    AuthGSSAPICheck3->Checked = FSessionData->AuthGSSAPI;
    GSSAPIFwdTGTCheck->Checked = FSessionData->GSSAPIFwdTGT;
    AgentFwdCheck->Checked = FSessionData->AgentFwd;
    PrivateKeyEdit2->Text = FSessionData->PublicKeyFile;

    // SSH page
    Ssh2LegacyDESCheck->Checked = FSessionData->Ssh2DES;
    CompressionCheck->Checked = FSessionData->Compression;

    switch (FSessionData->SshProt)
    {
      case ssh1only:  SshProt1onlyButton->Checked = true; break;
      case ssh1:      SshProt1Button->Checked = true; break;
      case ssh2:      SshProt2Button->Checked = true; break;
      case ssh2only:  SshProt2onlyButton->Checked = true; break;
    }

    CipherListBox->Items->Clear();
    DebugAssert(CIPHER_NAME_WARN+CIPHER_COUNT-1 == CIPHER_NAME_ARCFOUR);
    for (int Index = 0; Index < CIPHER_COUNT; Index++)
    {
      CipherListBox->Items->AddObject(
        LoadStr(CIPHER_NAME_WARN+FSessionData->Cipher[Index]),
        (TObject*)FSessionData->Cipher[Index]);
    }

    // KEX page

    KexListBox->Items->Clear();
    DebugAssert(KEX_NAME_WARN+KEX_COUNT-1 == KEX_NAME_RSA);
    for (int Index = 0; Index < KEX_COUNT; Index++)
    {
      KexListBox->Items->AddObject(
        LoadStr(KEX_NAME_WARN+FSessionData->Kex[Index]),
        (TObject*)FSessionData->Kex[Index]);
    }

    RekeyTimeEdit->AsInteger = FSessionData->RekeyTime;
    RekeyDataEdit->Text = FSessionData->RekeyData;

    // Connection page
    FtpPasvModeCheck->Checked = FSessionData->FtpPasvMode;
    BufferSizeCheck->Checked = (FSessionData->SendBuf > 0) && FSessionData->SshSimple;

    switch (FSessionData->PingType)
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
    PingIntervalSecEdit->AsInteger = FSessionData->PingInterval;
    switch (FSessionData->FtpPingType)
    {
      case ptDummyCommand:
        FtpPingDummyCommandButton->Checked = true;
        break;

      default:
        FtpPingOffButton->Checked = true;
        break;
    }
    FtpPingIntervalSecEdit->AsInteger = FSessionData->FtpPingInterval;
    TimeoutEdit->AsInteger = FSessionData->Timeout;

    switch (FSessionData->AddressFamily)
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

    // Shell page
    if (FSessionData->DefaultShell)
    {
      ShellEdit->Text = ShellEdit->Items->Strings[0];
    }
    else
    {
      ShellEdit->Text = FSessionData->Shell;
    }
    // hide selection, which is wrongly shown initially even when the box has not focus
    ShellEdit->SelLength = 0;

    if (FSessionData->DetectReturnVar)
    {
      ReturnVarEdit->Text = ReturnVarEdit->Items->Strings[0];
    }
    else
    {
      ReturnVarEdit->Text = FSessionData->ReturnVar;
    }
    ReturnVarEdit->SelLength = 0;

    ListingCommandEdit->Text = FSessionData->ListingCommand;
    ListingCommandEdit->SelLength = 0;

    CheckBoxAutoSwitchLoad(LookupUserGroupsCheck, FSessionData->LookupUserGroups);
    ClearAliasesCheck->Checked = FSessionData->ClearAliases;
    IgnoreLsWarningsCheck->Checked = FSessionData->IgnoreLsWarnings;
    Scp1CompatibilityCheck->Checked = FSessionData->Scp1Compatibility;
    UnsetNationalVarsCheck->Checked = FSessionData->UnsetNationalVars;
    SCPLsFullTimeAutoCheck->Checked = (FSessionData->SCPLsFullTime != asOff);

    // Proxy page
    SshProxyMethodCombo->ItemIndex = FSessionData->ProxyMethod;
    FtpProxyMethodCombo->ItemIndex = GetSupportedFtpProxyMethod(FSessionData->ProxyMethod);
    if (FSessionData->FtpProxyLogonType != 0)
    {
      FtpProxyMethodCombo->ItemIndex = LastSupportedFtpProxyMethod() + FSessionData->FtpProxyLogonType;
    }
    WebDavProxyMethodCombo->ItemIndex = GetSupportedWebDavProxyMethod(FSessionData->ProxyMethod);
    ProxyHostEdit->Text = FSessionData->ProxyHost;
    ProxyPortEdit->AsInteger = FSessionData->ProxyPort;
    ProxyUsernameEdit->Text = FSessionData->ProxyUsername;
    ProxyPasswordEdit->Text = FSessionData->ProxyPassword;
    ProxyTelnetCommandEdit->Text = FSessionData->ProxyTelnetCommand;
    ProxyLocalCommandEdit->Text = FSessionData->ProxyLocalCommand;
    ProxyLocalhostCheck->Checked = FSessionData->ProxyLocalhost;
    ProxyDNSCombo->ItemIndex = 2 - FSessionData->ProxyDNS;

    // Bugs page
    #define LOAD_BUG_COMBO(BUG) \
      ComboAutoSwitchLoad(Bug ## BUG ## Combo, FSessionData->Bug[sb ## BUG])
    LOAD_BUG_COMBO(Ignore1);
    LOAD_BUG_COMBO(PlainPW1);
    LOAD_BUG_COMBO(RSA1);
    LOAD_BUG_COMBO(HMAC2);
    LOAD_BUG_COMBO(DeriveKey2);
    LOAD_BUG_COMBO(RSAPad2);
    LOAD_BUG_COMBO(PKSessID2);
    LOAD_BUG_COMBO(Rekey2);
    LOAD_BUG_COMBO(MaxPkt2);
    LOAD_BUG_COMBO(Ignore2);
    LOAD_BUG_COMBO(WinAdj);
    #undef LOAD_BUG_COMBO

    // Tunnel page
    TunnelCheck->Checked = FSessionData->Tunnel;
    TunnelUserNameEdit->Text = FSessionData->TunnelUserName;
    TunnelPortNumberEdit->AsInteger = FSessionData->TunnelPortNumber;
    TunnelHostNameEdit->Text = FSessionData->TunnelHostName;
    TunnelPasswordEdit->Text = FSessionData->TunnelPassword;
    TunnelPrivateKeyEdit2->Text = FSessionData->TunnelPublicKeyFile;
    if (FSessionData->TunnelAutoassignLocalPortNumber)
    {
      TunnelLocalPortNumberEdit->Text = TunnelLocalPortNumberEdit->Items->Strings[0];
    }
    else
    {
      TunnelLocalPortNumberEdit->Text = IntToStr(FSessionData->TunnelLocalPortNumber);
    }
    // hide selection, which is wrongly shown initially even when the box has not focus
    TunnelLocalPortNumberEdit->SelLength = 0;

    // connection/tls/ssl page
    MinTlsVersionCombo->ItemIndex = TlsVersionToIndex(FSessionData->MinTlsVersion);
    MaxTlsVersionCombo->ItemIndex = TlsVersionToIndex(FSessionData->MaxTlsVersion);
    SslSessionReuseCheck->Checked = FSessionData->SslSessionReuse;
    TlsCertificateFileEdit->Text = FSessionData->TlsCertificateFile;

    // Note page
    NoteMemo->Lines->Text = FSessionData->Note;

    // color
    SetSessionColor((TColor)FSessionData->Color);
  }

  UpdateControls();
}
//---------------------------------------------------------------------
TSshProt __fastcall TSiteAdvancedDialog::GetSshProt()
{
  if (SshProt1onlyButton->Checked)
  {
    return ssh1only;
  }
  else if (SshProt1Button->Checked)
  {
    return ssh1;
  }
  else if (SshProt2Button->Checked)
  {
    return ssh2;
  }
  else
  {
    return ssh2only;
  }
}
//---------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::SaveSession()
{
  // SSH page
  FSessionData->Compression = CompressionCheck->Checked;
  FSessionData->Ssh2DES = Ssh2LegacyDESCheck->Checked;

  FSessionData->SshProt = GetSshProt();

  for (int Index = 0; Index < CIPHER_COUNT; Index++)
  {
    FSessionData->Cipher[Index] = (TCipher)CipherListBox->Items->Objects[Index];
  }

  // Kex page

  for (int Index = 0; Index < KEX_COUNT; Index++)
  {
    FSessionData->Kex[Index] = (TKex)KexListBox->Items->Objects[Index];
  }

  FSessionData->RekeyTime = RekeyTimeEdit->AsInteger;
  FSessionData->RekeyData = RekeyDataEdit->Text;

  // Authentication page
  FSessionData->SshNoUserAuth = SshNoUserAuthCheck->Checked;
  FSessionData->TryAgent = TryAgentCheck->Checked;
  FSessionData->AuthTIS = AuthTISCheck->Checked;
  FSessionData->AuthKI = AuthKICheck->Checked;
  FSessionData->AuthKIPassword = AuthKIPasswordCheck->Checked;
  FSessionData->AuthGSSAPI = AuthGSSAPICheck3->Checked;
  FSessionData->GSSAPIFwdTGT = GSSAPIFwdTGTCheck->Checked;
  FSessionData->AgentFwd = AgentFwdCheck->Checked;
  FSessionData->PublicKeyFile = PrivateKeyEdit2->Text;

  // Connection page
  FSessionData->FtpPasvMode = FtpPasvModeCheck->Checked;
  FSessionData->SendBuf = BufferSizeCheck->Checked ? DefaultSendBuf : 0;
  FSessionData->SshSimple = BufferSizeCheck->Checked;

  if (PingNullPacketButton->Checked)
  {
    FSessionData->PingType = ptNullPacket;
  }
  else if (PingDummyCommandButton->Checked)
  {
    FSessionData->PingType = ptDummyCommand;
  }
  else
  {
    FSessionData->PingType = ptOff;
  }
  FSessionData->PingInterval = PingIntervalSecEdit->AsInteger;
  FSessionData->FtpPingType = (FtpPingDummyCommandButton->Checked ? ptDummyCommand : ptOff);
  FSessionData->FtpPingInterval = FtpPingIntervalSecEdit->AsInteger;
  FSessionData->Timeout = TimeoutEdit->AsInteger;

  if (IPv4Button->Checked)
  {
    FSessionData->AddressFamily = afIPv4;
  }
  else if (IPv6Button->Checked)
  {
    FSessionData->AddressFamily = afIPv6;
  }
  else
  {
    FSessionData->AddressFamily = afAuto;
  }

  // Directories page
  FSessionData->SynchronizeBrowsing = SynchronizeBrowsingCheck->Checked;
  FSessionData->LocalDirectory = LocalDirectoryEdit->Text;
  FSessionData->RemoteDirectory = RemoteDirectoryEdit->Text;
  FSessionData->UpdateDirectories = UpdateDirectoriesCheck->Checked;
  FSessionData->CacheDirectories = CacheDirectoriesCheck->Checked;
  FSessionData->CacheDirectoryChanges = CacheDirectoryChangesCheck->Checked;
  FSessionData->PreserveDirectoryChanges = PreserveDirectoryChangesCheck->Checked;
  FSessionData->ResolveSymlinks = ResolveSymlinksCheck->Checked;

  // Environment page
  if (DSTModeUnixCheck->Checked)
  {
    FSessionData->DSTMode = dstmUnix;
  }
  else if (DSTModeKeepCheck->Checked)
  {
    FSessionData->DSTMode = dstmKeep;
  }
  else
  {
    FSessionData->DSTMode = dstmWin;
  }

  if (EOLTypeCombo->ItemIndex == 0)
  {
    FSessionData->EOLType = eolLF;
  }
  else
  {
    FSessionData->EOLType = eolCRLF;
  }

  switch (UtfCombo->ItemIndex)
  {
    case 1:
      FSessionData->NotUtf = asOn;
      break;

    case 2:
      FSessionData->NotUtf = asOff;
      break;

    default:
      FSessionData->NotUtf = asAuto;
      break;
  }
  FSessionData->TimeDifference =
    (double(TimeDifferenceEdit->AsInteger) / HoursPerDay) +
    (double(TimeDifferenceMinutesEdit->AsInteger) / MinsPerDay);
  FSessionData->TimeDifferenceAuto = TimeDifferenceAutoCheck->Checked;

  FSessionData->TrimVMSVersions = TrimVMSVersionsCheck->Checked;

  // Environment/Recycle bin page
  FSessionData->DeleteToRecycleBin = DeleteToRecycleBinCheck->Checked;
  FSessionData->OverwrittenToRecycleBin = OverwrittenToRecycleBinCheck->Checked;
  FSessionData->RecycleBinPath =
    FSessionData->DeleteToRecycleBin || FSessionData->OverwrittenToRecycleBin ||
    (RecycleBinPathEdit->Text != DefaultRecycleBinPath) ?
      RecycleBinPathEdit->Text : UnicodeString();

  // SCP page
  FSessionData->DefaultShell = (ShellEdit->Text == ShellEdit->Items->Strings[0]);
  FSessionData->Shell = (FSessionData->DefaultShell ? UnicodeString() : ShellEdit->Text);
  FSessionData->DetectReturnVar = (ReturnVarEdit->Text == ReturnVarEdit->Items->Strings[0]);
  FSessionData->ReturnVar = (FSessionData->DetectReturnVar ? UnicodeString() : ReturnVarEdit->Text);
  FSessionData->ListingCommand = ListingCommandEdit->Text;
  FSessionData->LookupUserGroups = CheckBoxAutoSwitchSave(LookupUserGroupsCheck);
  FSessionData->ClearAliases = ClearAliasesCheck->Checked;
  FSessionData->IgnoreLsWarnings = IgnoreLsWarningsCheck->Checked;
  FSessionData->Scp1Compatibility = Scp1CompatibilityCheck->Checked;
  FSessionData->UnsetNationalVars = UnsetNationalVarsCheck->Checked;
  FSessionData->SCPLsFullTime = SCPLsFullTimeAutoCheck->Checked ? asAuto : asOff;

  // SFTP page
  FSessionData->SftpServer =
    ((SftpServerEdit->Text == SftpServerEdit->Items->Strings[0]) ?
      UnicodeString() : SftpServerEdit->Text);
  FSessionData->SFTPMaxVersion = SFTPMaxVersionCombo->ItemIndex;
  if (AllowScpFallbackCheck->Checked != (FSessionData->FSProtocol == fsSFTP))
  {
    if (AllowScpFallbackCheck->Checked)
    {
      DebugAssert(FSessionData->FSProtocol == fsSFTPonly);
      FSessionData->FSProtocol = fsSFTP;
    }
    else
    {
      DebugAssert(FSessionData->FSProtocol == fsSFTP);
      FSessionData->FSProtocol = fsSFTPonly;
    }
  }

  #define SAVE_SFTP_BUG_COMBO(BUG) FSessionData->SFTPBug[sb ## BUG] = ComboAutoSwitchSave(SFTPBug ## BUG ## Combo);
  SAVE_SFTP_BUG_COMBO(Symlink);
  SAVE_SFTP_BUG_COMBO(SignedTS);
  #undef SAVE_SFTP_BUG_COMBO

  // FTP page
  FSessionData->FtpAccount = FtpAccountEdit->Text;
  FSessionData->PostLoginCommands = PostLoginCommandsMemo->Lines->Text;
  FSessionData->FtpListAll = ComboAutoSwitchSave(FtpListAllCombo);
  FSessionData->FtpUseMlsd = ComboAutoSwitchSave(FtpUseMlsdCombo);
  FSessionData->FtpForcePasvIp = ComboAutoSwitchSave(FtpForcePasvIpCombo);
  FSessionData->FtpHost = ComboAutoSwitchSave(FtpHostCombo);

  // Proxy page
  FSessionData->ProxyMethod = GetProxyMethod();
  FSessionData->FtpProxyLogonType = GetFtpProxyLogonType();
  FSessionData->ProxyHost = ProxyHostEdit->Text;
  FSessionData->ProxyPort = ProxyPortEdit->AsInteger;
  FSessionData->ProxyUsername = ProxyUsernameEdit->Text;
  FSessionData->ProxyPassword = ProxyPasswordEdit->Text;
  FSessionData->ProxyTelnetCommand = ProxyTelnetCommandEdit->Text;
  FSessionData->ProxyLocalCommand = ProxyLocalCommandEdit->Text;
  FSessionData->ProxyLocalhost = ProxyLocalhostCheck->Checked;
  FSessionData->ProxyDNS = (TAutoSwitch)(2 - ProxyDNSCombo->ItemIndex);

  // Bugs page
  #define SAVE_BUG_COMBO(BUG) FSessionData->Bug[sb ## BUG] = ComboAutoSwitchSave(Bug ## BUG ## Combo)
  SAVE_BUG_COMBO(Ignore1);
  SAVE_BUG_COMBO(PlainPW1);
  SAVE_BUG_COMBO(RSA1);
  SAVE_BUG_COMBO(HMAC2);
  SAVE_BUG_COMBO(DeriveKey2);
  SAVE_BUG_COMBO(RSAPad2);
  SAVE_BUG_COMBO(PKSessID2);
  SAVE_BUG_COMBO(Rekey2);
  SAVE_BUG_COMBO(MaxPkt2);
  SAVE_BUG_COMBO(Ignore2);
  SAVE_BUG_COMBO(WinAdj);
  #undef SAVE_BUG_COMBO

  // Tunnel page
  FSessionData->Tunnel = TunnelCheck->Checked;
  FSessionData->TunnelUserName = TunnelUserNameEdit->Text;
  FSessionData->TunnelPortNumber = TunnelPortNumberEdit->AsInteger;
  FSessionData->TunnelHostName = TunnelHostNameEdit->Text;
  FSessionData->TunnelPassword = TunnelPasswordEdit->Text;
  FSessionData->TunnelPublicKeyFile = TunnelPrivateKeyEdit2->Text;
  if (TunnelLocalPortNumberEdit->Text == TunnelLocalPortNumberEdit->Items->Strings[0])
  {
    FSessionData->TunnelLocalPortNumber = 0;
  }
  else
  {
    FSessionData->TunnelLocalPortNumber = StrToIntDef(TunnelLocalPortNumberEdit->Text, 0);
  }

  // connection/tls/ssl page
  FSessionData->MinTlsVersion = IndexToTlsVersion(MinTlsVersionCombo->ItemIndex);
  FSessionData->MaxTlsVersion = IndexToTlsVersion(MaxTlsVersionCombo->ItemIndex);
  FSessionData->SslSessionReuse = SslSessionReuseCheck->Checked;
  FSessionData->TlsCertificateFile = TlsCertificateFileEdit->Text;

  // Note page
  FSessionData->Note = NoteMemo->Lines->Text;

  // color
  FSessionData->Color = FColor;
}
//---------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::UpdateNavigationTree()
{
  TTreeNode * ActiveNode = NULL;

  {
    TAutoNestingCounter NoUpdateCounter(NoUpdate);

    int Index = 0;
    TTreeNode * PrevNode = NULL;
    while (Index < PageControl->PageCount)
    {
      TTabSheet * Tab = PageControl->Pages[Index];
      if (Tab->Enabled)
      {
        bool Indented = ((Tab->Tag % 100) == 2);
        UnicodeString Label = Tab->Caption;
        TTreeNode * Node;
        if (PrevNode == NULL)
        {
          DebugAssert(!Indented);
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

  // node of active page was hidden
  if (ActiveNode == NULL)
  {
    ChangePage(EnvironmentSheet);
  }
}
//---------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::UpdateControls()
{
  if (Visible)
  {
    TAutoNestingCounter NoUpdateCounter(NoUpdate);

    bool SshProtocol = FSessionData->UsesSsh;
    bool SftpProtocol =
      (FSessionData->FSProtocol == fsSFTPonly) || (FSessionData->FSProtocol == fsSFTP);
    bool ScpProtocol = (FSessionData->FSProtocol == fsSCPonly);
    bool FtpProtocol = (FSessionData->FSProtocol == fsFTP);
    bool WebDavProtocol = (FSessionData->FSProtocol == fsWebDAV);
    bool Ssl = (FtpProtocol || WebDavProtocol) && (FSessionData->Ftps != ftpsNone);

    // connection sheet
    EnableControl(FtpPasvModeCheck, FtpProtocol);
    if (FtpProtocol &&
        (FtpProxyMethodCombo->ItemIndex != ::pmNone) &&
        SupportedFtpProxyMethod(FtpProxyMethodCombo->ItemIndex) &&
        !FtpPasvModeCheck->Checked)
    {
      FtpPasvModeCheck->Checked = true;
      MessageDialog(MainInstructions(LoadStr(FTP_PASV_MODE_REQUIRED)), qtInformation, qaOK);
    }
    EnableControl(BufferSizeCheck, SshProtocol);
    PingGroup->Visible = !FtpProtocol;
    EnableControl(PingGroup, SshProtocol);
    EnableControl(PingIntervalSecEdit, PingGroup->Enabled && !PingOffButton->Checked);
    EnableControl(PingIntervalLabel, PingGroup->Enabled && PingIntervalSecEdit->Enabled);
    FtpPingGroup->Visible = FtpProtocol;
    EnableControl(FtpPingIntervalSecEdit, !FtpPingOffButton->Checked);
    EnableControl(FtpPingIntervalLabel, FtpPingIntervalSecEdit->Enabled);
    EnableControl(IPvGroup, SshProtocol || FtpProtocol);
    EnableControl(IPAutoButton, IPvGroup->Enabled && SshProtocol);

    // ssh/authentication sheet
    AuthSheet->Enabled = SshProtocol;
    EnableControl(SshNoUserAuthCheck, !SshProt1onlyButton->Checked);
    EnableControl(AuthenticationGroup,
      !SshNoUserAuthCheck->Enabled || !SshNoUserAuthCheck->Checked);
    EnableControl(AuthTISCheck, AuthenticationGroup->Enabled && !SshProt2onlyButton->Checked);
    EnableControl(AuthKICheck, AuthenticationGroup->Enabled && !SshProt1onlyButton->Checked);
    EnableControl(AuthKIPasswordCheck,
      AuthenticationGroup->Enabled &&
      ((AuthTISCheck->Enabled && AuthTISCheck->Checked) ||
       (AuthKICheck->Enabled && AuthKICheck->Checked)));
    EnableControl(AuthGSSAPICheck3,
      AuthenticationGroup->Enabled && !SshProt1onlyButton->Checked);
    EnableControl(GSSAPIFwdTGTCheck,
      AuthGSSAPICheck3->Enabled && AuthGSSAPICheck3->Checked);

    // ssh sheet
    AdvancedSheet->Enabled = SshProtocol;
    EnableControl(CipherUpButton, CipherListBox->ItemIndex > 0);
    EnableControl(CipherDownButton, CipherListBox->ItemIndex >= 0 &&
      CipherListBox->ItemIndex < CipherListBox->Items->Count-1);
    EnableControl(Ssh2LegacyDESCheck, !SshProt1onlyButton->Checked);

    // ssh/kex sheet
    KexSheet->Enabled = SshProtocol && !SshProt1onlyButton->Checked &&
      (BugRekey2Combo->ItemIndex != 2);
    EnableControl(KexUpButton, KexListBox->ItemIndex > 0);
    EnableControl(KexDownButton, KexListBox->ItemIndex >= 0 &&
      KexListBox->ItemIndex < KexListBox->Items->Count-1);

    // ssh/bugs sheet
    BugsSheet->Enabled = SshProtocol;
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
    EnableControl(BugMaxPkt2Combo, !SshProt1onlyButton->Checked);
    EnableControl(BugMaxPkt2Label, BugMaxPkt2Combo->Enabled);
    EnableControl(BugIgnore2Combo, !SshProt1onlyButton->Checked);
    EnableControl(BugIgnore2Label, BugIgnore2Combo->Enabled);
    EnableControl(BugWinAdjCombo, !SshProt1onlyButton->Checked);
    EnableControl(BugWinAdjLabel, BugWinAdjCombo->Enabled);

    // connection/proxy sheet
    // this is probaqbly overkill, now we do not allow changing protocol on
    // the same window as changing proxy
    TComboBox * ProxyMethodCombo =
      (SshProtocol ? SshProxyMethodCombo : (FtpProtocol ? FtpProxyMethodCombo : WebDavProxyMethodCombo));
    TProxyMethod ProxyMethod = GetProxyMethod();
    ProxyMethodCombo->Visible = true;
    ProxyMethodLabel->FocusControl = ProxyMethodCombo;
    if (!SshProtocol)
    {
      SshProxyMethodCombo->Visible = false;
      SshProxyMethodCombo->ItemIndex = ProxyMethod;
    }
    if (!FtpProtocol)
    {
      FtpProxyMethodCombo->Visible = false;
      FtpProxyMethodCombo->ItemIndex = GetSupportedFtpProxyMethod(ProxyMethod);
    }
    if (!WebDavProtocol)
    {
      WebDavProxyMethodCombo->Visible = false;
      WebDavProxyMethodCombo->ItemIndex = GetSupportedWebDavProxyMethod(ProxyMethod);
    }

    int FtpProxyLogonType = GetFtpProxyLogonType();
    UnicodeString ProxyCommand =
      ((ProxyMethod == pmCmd) ?
        ProxyLocalCommandEdit->Text : ProxyTelnetCommandEdit->Text);
    EnableControl(ProxyHostEdit,
      (ProxyMethod == pmSocks4) ||
      (ProxyMethod == pmSocks5) ||
      (ProxyMethod == pmHTTP) ||
      (ProxyMethod == pmTelnet) ||
      ((ProxyMethod == pmCmd) && AnsiContainsText(ProxyCommand, L"%proxyhost")) ||
      (FtpProxyLogonType > 0));
    EnableControl(ProxyHostLabel, ProxyHostEdit->Enabled);
    EnableControl(ProxyPortEdit,
      (ProxyMethod == pmSocks4) ||
      (ProxyMethod == pmSocks5) ||
      (ProxyMethod == pmHTTP) ||
      (ProxyMethod == pmTelnet) ||
      ((ProxyMethod == pmCmd) && AnsiContainsText(ProxyCommand, L"%proxyport")) ||
      (FtpProxyLogonType > 0));
    EnableControl(ProxyPortLabel, ProxyPortEdit->Enabled);
    EnableControl(ProxyUsernameEdit,
      // FZAPI does not support username for SOCKS4
      ((ProxyMethod == pmSocks4) && (SshProtocol || WebDavProtocol)) ||
      (ProxyMethod == pmSocks5) ||
      (ProxyMethod == pmHTTP) ||
      (((ProxyMethod == pmTelnet) ||
        (ProxyMethod == pmCmd)) &&
       AnsiContainsText(ProxyCommand, L"%user")) ||
      ((FtpProxyLogonType > 0) && (FtpProxyLogonType != 3) && (FtpProxyLogonType != 5)));
    EnableControl(ProxyUsernameLabel, ProxyUsernameEdit->Enabled);
    EnableControl(ProxyPasswordEdit,
      (ProxyMethod == pmSocks5) ||
      (ProxyMethod == pmHTTP) ||
      (((ProxyMethod == pmTelnet) ||
        (ProxyMethod == pmCmd)) &&
       AnsiContainsText(ProxyCommand, L"%pass")) ||
      ((FtpProxyLogonType > 0) && (FtpProxyLogonType != 3) && (FtpProxyLogonType != 5)));
    EnableControl(ProxyPasswordLabel, ProxyPasswordEdit->Enabled);
    bool ProxySettings = (ProxyMethod != ::pmNone) && SshProtocol;
    ProxySettingsGroup->Visible = SshProtocol;
    EnableControl(ProxySettingsGroup, ProxySettings);
    EnableControl(ProxyTelnetCommandEdit,
      ProxySettings && (ProxyMethod == pmTelnet));
    EnableControl(ProxyTelnetCommandLabel, ProxyTelnetCommandEdit->Enabled);
    EnableControl(ProxyTelnetCommandHintText, ProxyTelnetCommandEdit->Enabled);
    ProxyLocalCommandEdit->Visible = (ProxyMethod == pmCmd);
    ProxyLocalCommandLabel->Visible = ProxyLocalCommandEdit->Visible;
    ProxyLocalCommandBrowseButton->Visible = ProxyLocalCommandEdit->Visible;
    ProxyLocalCommandHintText->Visible = ProxyLocalCommandEdit->Visible;
    ProxyTelnetCommandEdit->Visible = !ProxyLocalCommandEdit->Visible;
    ProxyTelnetCommandLabel->Visible = ProxyTelnetCommandEdit->Visible;
    ProxyTelnetCommandHintText->Visible = ProxyTelnetCommandEdit->Visible;

    // environment/directories sheet
    EnableControl(SynchronizeBrowsingCheck,
      (CustomWinConfiguration->Interface == ifCommander) &&
      WinConfiguration->PreservePanelState &&
      !WinConfiguration->ScpCommander.PreserveLocalDirectory);
    EnableControl(CacheDirectoryChangesCheck,
      (!ScpProtocol || CacheDirectoriesCheck->Checked) && DirectoriesSheet->Enabled);
    EnableControl(PreserveDirectoryChangesCheck,
      CacheDirectoryChangesCheck->Enabled && CacheDirectoryChangesCheck->Checked &&
      DirectoriesSheet->Enabled);
    EnableControl(ResolveSymlinksCheck, (SftpProtocol || ScpProtocol) && DirectoriesSheet->Enabled);

    // environment sheet
    EnableControl(EOLTypeCombo, (SftpProtocol || ScpProtocol) && EnvironmentSheet->Enabled);
    EnableControl(EOLTypeLabel, EOLTypeCombo->Enabled);
    EnableControl(DSTModeGroup, (SftpProtocol || ScpProtocol) && EnvironmentSheet->Enabled);
    EnableControl(DSTModeKeepCheck, UsesDaylightHack() && DSTModeGroup->Enabled);
    EnableControl(UtfCombo, (SftpProtocol || FtpProtocol || ScpProtocol) && EnvironmentSheet->Enabled);
    EnableControl(UtfLabel, UtfCombo->Enabled);
    // should be enabled for fsSFTP (SCP fallback) too, but it would cause confusion
    bool FtpProtocolWithMlsdOff = FtpProtocol && (ComboAutoSwitchSave(FtpUseMlsdCombo) == asOff);
    EnableControl(TimeDifferenceAutoCheck, FtpProtocolWithMlsdOff);
    EnableControl(TimeDifferenceEdit,
      ((FtpProtocolWithMlsdOff && !TimeDifferenceAutoCheck->Checked) ||
       ScpProtocol) &&
      EnvironmentSheet->Enabled);
    EnableControl(TimeDifferenceLabel,
      TimeDifferenceEdit->Enabled || TimeDifferenceAutoCheck->Enabled);
    EnableControl(TimeDifferenceHoursLabel, TimeDifferenceEdit->Enabled);
    EnableControl(TimeDifferenceMinutesEdit, TimeDifferenceEdit->Enabled);
    EnableControl(TimeDifferenceMinutesLabel, TimeDifferenceEdit->Enabled);

    // environment/recycle bin sheet
    EnableControl(OverwrittenToRecycleBinCheck, SftpProtocol && RecycleBinSheet->Enabled);
    EnableControl(RecycleBinPathEdit,
      (DeleteToRecycleBinCheck->Enabled && DeleteToRecycleBinCheck->Checked) ||
      (OverwrittenToRecycleBinCheck->Enabled && OverwrittenToRecycleBinCheck->Checked) &&
      RecycleBinSheet->Enabled);
    EnableControl(RecycleBinPathLabel, RecycleBinPathEdit->Enabled &&
      RecycleBinSheet->Enabled);

    // environment/sftp sheet
    SftpSheet->Enabled = SftpProtocol;

    // environment/scp/shell
    ScpSheet->Enabled = SshProtocol;
    ScpSheet->Caption = LoadStr(ScpProtocol ? LOGIN_SCP_SHELL_PAGE : LOGIN_SHELL_PAGE);
    // hide also for SFTP with SCP fallback, as if someone wants to configure
    // these he/she probably intends to use SCP and should explicitly select it.
    // (note that these are not used for separate shell session)
    ScpLsOptionsGroup->Visible = ScpProtocol;
    OtherShellOptionsGroup->Visible = ScpProtocol;

    // environment/ftp
    FtpSheet->Enabled = FtpProtocol;
    EnableControl(FtpListAllCombo,
      (ComboAutoSwitchSave(FtpUseMlsdCombo) == asOff));
    EnableControl(FtpListAllLabel, FtpListAllCombo->Enabled);
    EnableControl(FtpForcePasvIpCombo,
      FtpPasvModeCheck->Checked &&
      (IPAutoButton->Checked || IPv4Button->Checked));
    EnableControl(FtpForcePasvIpLabel, FtpForcePasvIpCombo->Enabled);

    // tunnel sheet
    TunnelSheet->Enabled = SshProtocol;
    // probably needless
    EnableControl(TunnelSessionGroup, TunnelCheck->Enabled && TunnelCheck->Checked);
    EnableControl(TunnelOptionsGroup, TunnelSessionGroup->Enabled);
    EnableControl(TunnelAuthenticationParamsGroup, TunnelSessionGroup->Enabled);

    // connection/ssl/tls
    SslSheet->Enabled = Ssl;
    // TLS/SSL session reuse is not configurable for WebDAV yet
    SslSessionReuseCheck->Enabled = SslSheet->Enabled && FtpProtocol;

    UpdateNavigationTree();

    // color
    if (FColor == 0)
    {
      MenuButton(ColorButton);
    }
    else
    {
      ColorButton->Images = ColorImageList;
      ColorButton->ImageIndex = 1;
      ColorButton->ImageAlignment = iaRight;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::DataChange(TObject * /*Sender*/)
{
  if (NoUpdate == 0)
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(PrivateKeyEdit2);
  InstallPathWordBreakProc(TunnelPrivateKeyEdit2);
  InstallPathWordBreakProc(RemoteDirectoryEdit);
  InstallPathWordBreakProc(LocalDirectoryEdit);
  InstallPathWordBreakProc(RecycleBinPathEdit);
  InstallPathWordBreakProc(TlsCertificateFileEdit);

  ChangePage(EnvironmentSheet);

  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TSiteAdvancedDialog::Execute(TSessionData * SessionData)
{
  FSessionData = SessionData;
  LoadSession();

  bool Result = (ShowModal() == DefaultResult(this));

  if (Result)
  {
    SaveSession();
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::NavigationTreeChange(TObject * /*Sender*/,
  TTreeNode * Node)
{
  if (NoUpdate == 0)
  {
    TAutoNestingCounter Guard(NoUpdate);

    TTabSheet * Tab = reinterpret_cast<TTabSheet *>(Node->SelectedIndex);
    // should happen only while loading language
    // (UpdateNavigationTree may not be called yet)
    if (Tab != NULL)
    {
      PageControl->ActivePage = Tab;
      // reshow the accelerators, etc
      ResetSystemSettings(this);
    }

    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::ChangePage(TTabSheet * Tab)
{
  PageControl->ActivePage = Tab;
  PageControlChange(PageControl);
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::PageControlChange(TObject *Sender)
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

  if (DebugAlwaysTrue(Found))
  {
    DataChange(Sender);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::CMDialogKey(TWMKeyDown & Message)
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
        if (Node == NULL)
        {
          Node = NavigationTree->Items->GetFirstNode();
        }
      }
      else
      {
        if (Node->GetPrev() != NULL)
        {
          Node = Node->GetPrev();
        }
        else
        {
          while (Node->GetNext() != NULL)
          {
            Node = Node->GetNext();
          }
        }
      }
      Node->Selected = True;
      Message.Result = 1;
      return;
    }
  }

  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::WMHelp(TWMHelp & Message)
{
  DebugAssert(Message.HelpInfo != NULL);

  if (Message.HelpInfo->iContextType == HELPINFO_WINDOW)
  {
    // invoke help for active page (not for whole form), regardless of focus
    // (e.g. even if focus is on control outside pagecontrol)
    Message.HelpInfo->hItemHandle = PageControl->ActivePage->Handle;
  }
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::Dispatch(void * Message)
{
  TMessage * M = reinterpret_cast<TMessage*>(Message);
  DebugAssert(M);
  if (M->Msg == CM_DIALOGKEY)
  {
    CMDialogKey(*((TWMKeyDown *)Message));
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
void __fastcall TSiteAdvancedDialog::AlgListBoxStartDrag(TObject * Sender,
  TDragObject *& /*DragObject*/)
{
  FAlgDragSource = dynamic_cast<TListBox*>(Sender)->ItemIndex;
  FAlgDragDest = -1;
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::AlgListBoxDragOver(TObject * Sender,
  TObject * Source, int X, int Y, TDragState /*State*/, bool & Accept)
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
void __fastcall TSiteAdvancedDialog::AlgListBoxDragDrop(TObject * Sender,
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
void __fastcall TSiteAdvancedDialog::CipherButtonClick(TObject *Sender)
{
  AlgMove(CipherListBox, CipherListBox->ItemIndex,
    CipherListBox->ItemIndex + (Sender == CipherUpButton ? -1 : 1));
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::KexButtonClick(TObject *Sender)
{
  AlgMove(KexListBox, KexListBox->ItemIndex,
    KexListBox->ItemIndex + (Sender == KexUpButton ? -1 : 1));
}
//---------------------------------------------------------------------------
bool __fastcall TSiteAdvancedDialog::AllowAlgDrag(TListBox * AlgListBox, int X, int Y)
{
  FAlgDragDest = AlgListBox->ItemAtPos(TPoint(X, Y), true);
  return (FAlgDragDest >= 0) && (FAlgDragDest != FAlgDragSource);
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::AlgMove(TListBox * AlgListBox, int Source, int Dest)
{
  if ((Source >= 0) && (Source < AlgListBox->Items->Count) &&
      (Dest >= 0) && (Dest < AlgListBox->Items->Count))
  {
    AlgListBox->Items->Move(Source, Dest);
    AlgListBox->ItemIndex = Dest;
    AlgListBox->SetFocus();
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::AuthGSSAPICheck3Click(TObject * /*Sender*/)
{
  if (NoUpdate == 0)
  {
    UpdateControls();
    if (AuthGSSAPICheck3->Checked && !HasGSSAPI(L""))
    {
      throw Exception(LoadStr(GSSAPI_NOT_INSTALLED2));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::PrivateKeyEdit2AfterDialog(TObject * Sender,
  UnicodeString & Name, bool & Action)
{
  PathEditAfterDialog(Sender, Name, Action);

  TFilenameEdit * Edit = dynamic_cast<TFilenameEdit *>(Sender);
  if (Name != Edit->Text)
  {
    VerifyAndConvertKey(Name);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult == DefaultResult(this))
  {
    VerifyKeyIncludingVersion(StripPathQuotes(PrivateKeyEdit2->Text), GetSshProt());
    // for tunnel key do not check SSH version as it is not configurable
    VerifyKey(StripPathQuotes(TunnelPrivateKeyEdit2->Text));
    VerifyCertificate(StripPathQuotes(TlsCertificateFileEdit->Text));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::PathEditBeforeDialog(TObject * /*Sender*/,
  UnicodeString & Name, bool & /*Action*/)
{
  FBeforeDialogPath = Name;
  Name = ExpandEnvironmentVariables(Name);
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::PathEditAfterDialog(TObject * /*Sender*/,
  UnicodeString & Name, bool & /*Action*/)
{
  if (CompareFileName(Name, ExpandEnvironmentVariables(FBeforeDialogPath)))
  {
    Name = FBeforeDialogPath;
  }
}
//---------------------------------------------------------------------------
int __fastcall TSiteAdvancedDialog::LastSupportedFtpProxyMethod()
{
  return pmHTTP;
}
//---------------------------------------------------------------------------
bool __fastcall TSiteAdvancedDialog::SupportedFtpProxyMethod(int Method)
{
  return (Method >= 0) && (Method <= LastSupportedFtpProxyMethod());
}
//---------------------------------------------------------------------------
int __fastcall TSiteAdvancedDialog::GetSupportedFtpProxyMethod(int Method)
{
  if (SupportedFtpProxyMethod(Method))
  {
    return Method;
  }
  else
  {
    return ::pmNone;
  }
}
//---------------------------------------------------------------------------
int __fastcall TSiteAdvancedDialog::GetSupportedWebDavProxyMethod(int Method)
{
  if ((Method >= 0) && (Method <= pmHTTP))
  {
    return Method;
  }
  else
  {
    return ::pmNone;
  }
}
//---------------------------------------------------------------------------
TProxyMethod __fastcall TSiteAdvancedDialog::GetProxyMethod()
{
  TProxyMethod Result;
  TFSProtocol FSProtocol = FSessionData->FSProtocol;
  if (FSessionData->UsesSsh)
  {
    Result = (TProxyMethod)SshProxyMethodCombo->ItemIndex;
  }
  else if (FSProtocol == fsFTP)
  {
    if (SupportedFtpProxyMethod(FtpProxyMethodCombo->ItemIndex))
    {
      Result = (TProxyMethod)FtpProxyMethodCombo->ItemIndex;
    }
    else
    {
      Result = ::pmNone;
    }
  }
  else if (FSProtocol == fsWebDAV)
  {
    Result = (TProxyMethod)WebDavProxyMethodCombo->ItemIndex;
  }
  else
  {
    DebugFail;
    Result = ::pmNone;
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TSiteAdvancedDialog::GetFtpProxyLogonType()
{
  int Result;
  if (FSessionData->FSProtocol != fsFTP)
  {
    Result = 0;
  }
  else
  {
    if (SupportedFtpProxyMethod(FtpProxyMethodCombo->ItemIndex))
    {
      Result = 0;
    }
    else
    {
      Result = FtpProxyMethodCombo->ItemIndex - LastSupportedFtpProxyMethod();
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::NavigationTreeCollapsing(
  TObject * /*Sender*/, TTreeNode * /*Node*/, bool & AllowCollapse)
{
  AllowCollapse = false;
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::ProxyLocalCommandBrowseButtonClick(
  TObject * /*Sender*/)
{
  BrowseForExecutable(ProxyLocalCommandEdit,
    LoadStr(LOGIN_SELECT_LOCAL_PROXY),
    LoadStr(EXECUTABLE_FILTER), false, true);
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::ColorButtonClick(TObject * /*Sender*/)
{
  // WORKAROUND: Compiler keeps crashing randomly (but frequently) with
  // "internal error" when passing menu directly to unique_ptr.
  // Splitting it to two statements seems to help.
  // The same hack exists in TPreferencesDialog::EditorFontColorButtonClick
  TPopupMenu * Menu = CreateSessionColorPopupMenu(FColor, SessionColorChange);
  // Popup menu has to survive the popup as TBX calls click handler asynchronously (post).
  FColorPopupMenu.reset(Menu);
  MenuPopup(Menu, ColorButton);
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::SessionColorChange(TColor Color)
{
  SetSessionColor(Color);
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::SetSessionColor(TColor Color)
{
  FColor = Color;

  while (ColorImageList->Count > 1)
  {
    ColorImageList->Delete(1);
  }

  if (Color != 0)
  {
    AddSessionColorImage(ColorImageList, Color, 0);
  }
}
//---------------------------------------------------------------------------
TTlsVersion __fastcall TSiteAdvancedDialog::IndexToTlsVersion(int Index)
{
  switch (Index)
  {
    default:
      DebugFail;
    case 0:
      return ssl3;
    case 1:
      return tls10;
    case 2:
      return tls11;
    case 3:
      return tls12;
  }
}
//---------------------------------------------------------------------------
int __fastcall TSiteAdvancedDialog::TlsVersionToIndex(TTlsVersion TlsVersion)
{
  switch (TlsVersion)
  {
    default:
      DebugFail;
    case ssl2:
    case ssl3:
      return 0;
    case tls10:
      return 1;
    case tls11:
      return 2;
    case tls12:
      return 3;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::MinTlsVersionComboChange(TObject */*Sender*/)
{
  TTlsVersion MinTlsVersion = IndexToTlsVersion(MinTlsVersionCombo->ItemIndex);
  TTlsVersion MaxTlsVersion = IndexToTlsVersion(MaxTlsVersionCombo->ItemIndex);
  if (MaxTlsVersion < MinTlsVersion)
  {
    MaxTlsVersionCombo->ItemIndex = MinTlsVersionCombo->ItemIndex;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::MaxTlsVersionComboChange(TObject * /*Sender*/)
{
  TTlsVersion MinTlsVersion = IndexToTlsVersion(MinTlsVersionCombo->ItemIndex);
  TTlsVersion MaxTlsVersion = IndexToTlsVersion(MaxTlsVersionCombo->ItemIndex);
  if (MinTlsVersion > MaxTlsVersion)
  {
    MinTlsVersionCombo->ItemIndex = MaxTlsVersionCombo->ItemIndex;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::ProxyAutodetectButtonClick(TObject * /*Sender*/)
{
  TInstantOperationVisualizer Visualizer;
  UnicodeString ProxyHost;
  int ProxyPort;
  if (AutodetectProxy(ProxyHost, ProxyPort))
  {
    ProxyHostEdit->Text = ProxyHost;
    ProxyPortEdit->AsInteger = ProxyPort;

    SshProxyMethodCombo->ItemIndex = pmHTTP;
    FtpProxyMethodCombo->ItemIndex = pmHTTP;
    WebDavProxyMethodCombo->ItemIndex = pmHTTP;

    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::NoteMemoKeyDown(
  TObject * Sender, WORD & Key, TShiftState Shift)
{
  MemoKeyDown(Sender, Key, Shift);
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::TlsCertificateFileEditAfterDialog(TObject *Sender,
  UnicodeString &Name, bool &Action)
{
  PathEditAfterDialog(Sender, Name, Action);

  TFilenameEdit * Edit = dynamic_cast<TFilenameEdit *>(Sender);
  if (Name != Edit->Text)
  {
    VerifyCertificate(Name);
  }
}
//---------------------------------------------------------------------------
