//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <StrUtils.hpp>
#include <CoreMain.h>
#include <Common.h>
#include <TextsWin.h>
#include <TextsCore.h>
#include <HelpCore.h>
#include <HelpWin.h>
#include <VCLCommon.h>
#include <Cryptography.h>

#include "WinInterface.h"
#include "SiteAdvanced.h"
#include "GUITools.h"
#include "Tools.h"
#include "WinConfiguration.h"
#include "PuttyTools.h"
#include "TerminalManager.h"
#include "Authenticate.h"
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
bool __fastcall DoSiteAdvancedDialog(TSessionData * SessionData)
{
  TSiteAdvancedDialog * SiteAdvancedDialog = new TSiteAdvancedDialog(Application);
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
__fastcall TSiteAdvancedDialog::TSiteAdvancedDialog(TComponent * AOwner) :
  TForm(AOwner)
{
  NoUpdate = 0;

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

  SelectScaledImageList(ColorImageList);
  FColor = TColor();

  MenuButton(PrivateKeyToolsButton);
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
    FollowDirectorySymlinksCheck->Checked = FSessionData->FollowDirectorySymlinks;

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

    // S3 page
    S3DefaultReqionCombo->Text = FSessionData->S3DefaultRegion;
    if (FSessionData->S3UrlStyle == s3usPath)
    {
      S3UrlStyleCombo->ItemIndex = 1;
    }
    else
    {
      S3UrlStyleCombo->ItemIndex = 0;
    }

    // Authentication page
    SshNoUserAuthCheck->Checked = FSessionData->SshNoUserAuth;
    TryAgentCheck->Checked = FSessionData->TryAgent;
    AuthTISCheck->Checked = FSessionData->AuthTIS;
    AuthKICheck->Checked = FSessionData->AuthKI;
    AuthKIPasswordCheck->Checked = FSessionData->AuthKIPassword;
    AuthGSSAPICheck3->Checked = FSessionData->AuthGSSAPI;
    GSSAPIFwdTGTCheck->Checked = FSessionData->GSSAPIFwdTGT;
    AgentFwdCheck->Checked = FSessionData->AgentFwd;
    PrivateKeyEdit3->Text = FSessionData->PublicKeyFile;

    // SSH page
    Ssh2LegacyDESCheck->Checked = FSessionData->Ssh2DES;
    CompressionCheck->Checked = FSessionData->Compression;
    if (FSessionData->SshProt == ssh1only)
    {
      SshProtCombo2->ItemIndex = 0;
    }
    else
    {
      SshProtCombo2->ItemIndex = 1;
    }

    CipherListBox->Items->Clear();
    DebugAssert(CIPHER_NAME_WARN+CIPHER_COUNT-1 == CIPHER_NAME_CHACHA20);
    for (int Index = 0; Index < CIPHER_COUNT; Index++)
    {
      CipherListBox->Items->AddObject(
        LoadStr(CIPHER_NAME_WARN+FSessionData->Cipher[Index]),
        (TObject*)FSessionData->Cipher[Index]);
    }

    // KEX page

    KexListBox->Items->Clear();
    DebugAssert(KEX_NAME_WARN+KEX_COUNT-1 == KEX_NAME_ECDH);
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
    NeonProxyMethodCombo->ItemIndex = GetSupportedNeonProxyMethod(FSessionData->ProxyMethod);
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
    TunnelPrivateKeyEdit3->Text = FSessionData->TunnelPublicKeyFile;
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

    // Encryption page
    EncryptFilesCheck->Checked = !FSessionData->EncryptKey.IsEmpty();
    GetEncryptKeyEdit()->Text = FSessionData->EncryptKey;

    // color
    FColor = (TColor)FSessionData->Color;
  }

  UpdateControls();
}
//---------------------------------------------------------------------
TSshProt __fastcall TSiteAdvancedDialog::GetSshProt()
{
  return (SshProtCombo2->ItemIndex == 0) ? ssh1only : ssh2only;
}
//---------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::SaveSession()
{
  // Last resort validation,
  // in case the test in EncryptKeyEditExit was previously bypassed due to IsCancelButtonBeingClicked
  EncryptKeyEditExit(GetEncryptKeyEdit());

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
  FSessionData->PublicKeyFile = PrivateKeyEdit3->Text;

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
  FSessionData->FollowDirectorySymlinks = FollowDirectorySymlinksCheck->Checked;

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

  // S3 page
  FSessionData->S3DefaultRegion = S3DefaultReqionCombo->Text;
  if (S3UrlStyleCombo->ItemIndex == 1)
  {
    FSessionData->S3UrlStyle = s3usPath;
  }
  else
  {
    FSessionData->S3UrlStyle = s3usVirtualHost;
  }

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
  FSessionData->TunnelPublicKeyFile = TunnelPrivateKeyEdit3->Text;
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

  // Encryption page
  FSessionData->EncryptKey = EncryptFilesCheck->Checked ? GetEncryptKeyEdit()->Text : UnicodeString();

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
    bool SftpProtocol = (NormalizeFSProtocol(FSessionData->FSProtocol) == fsSFTP);
    bool ScpProtocol = (FSessionData->FSProtocol == fsSCPonly);
    bool FtpProtocol = (FSessionData->FSProtocol == fsFTP);
    bool WebDavProtocol = (FSessionData->FSProtocol == fsWebDAV);
    bool S3Protocol = (FSessionData->FSProtocol == fsS3);
    bool Neon = IsNeon(FSessionData->FSProtocol);
    bool Ssl = (FtpProtocol || WebDavProtocol || S3Protocol) && (FSessionData->Ftps != ftpsNone);

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
    EnableControl(SshNoUserAuthCheck, (GetSshProt() == ssh2only));
    EnableControl(AuthenticationGroup,
      !SshNoUserAuthCheck->Enabled || !SshNoUserAuthCheck->Checked);
    EnableControl(AuthTISCheck, AuthenticationGroup->Enabled && (GetSshProt() == ssh1only));
    EnableControl(AuthKICheck, AuthenticationGroup->Enabled && (GetSshProt() == ssh2only));
    EnableControl(AuthKIPasswordCheck,
      AuthenticationGroup->Enabled &&
      ((AuthTISCheck->Enabled && AuthTISCheck->Checked) ||
       (AuthKICheck->Enabled && AuthKICheck->Checked)));
    EnableControl(AuthenticationParamsGroup, AuthenticationGroup->Enabled);
    EnableControl(AgentFwdCheck, AuthenticationParamsGroup->Enabled && FSessionData->TryAgent);
    EnableControl(PrivateKeyViewButton, PrivateKeyEdit3->Enabled && !PrivateKeyEdit3->Text.IsEmpty());
    EnableControl(AuthGSSAPICheck3,
      AuthenticationGroup->Enabled && (GetSshProt() == ssh2only));
    EnableControl(GSSAPIFwdTGTCheck,
      AuthGSSAPICheck3->Enabled && AuthGSSAPICheck3->Checked);

    // ssh sheet
    AdvancedSheet->Enabled = SshProtocol;
    EnableControl(CipherUpButton, CipherListBox->ItemIndex > 0);
    EnableControl(CipherDownButton, CipherListBox->ItemIndex >= 0 &&
      CipherListBox->ItemIndex < CipherListBox->Items->Count-1);
    EnableControl(Ssh2LegacyDESCheck, (GetSshProt() == ssh2only));

    // ssh/kex sheet
    KexSheet->Enabled = SshProtocol && (GetSshProt() == ssh2only) &&
      (BugRekey2Combo->ItemIndex != 2);
    EnableControl(KexUpButton, KexListBox->ItemIndex > 0);
    EnableControl(KexDownButton, KexListBox->ItemIndex >= 0 &&
      KexListBox->ItemIndex < KexListBox->Items->Count-1);

    // ssh/bugs sheet
    BugsSheet->Enabled = SshProtocol;
    EnableControl(BugIgnore1Combo, (GetSshProt() == ssh1only));
    EnableControl(BugIgnore1Label, BugIgnore1Combo->Enabled);
    EnableControl(BugPlainPW1Combo, (GetSshProt() == ssh1only));
    EnableControl(BugPlainPW1Label, BugPlainPW1Combo->Enabled);
    EnableControl(BugRSA1Combo, (GetSshProt() == ssh1only));
    EnableControl(BugRSA1Label, BugRSA1Combo->Enabled);
    EnableControl(BugHMAC2Combo, (GetSshProt() == ssh2only));
    EnableControl(BugHMAC2Label, BugHMAC2Combo->Enabled);
    EnableControl(BugDeriveKey2Combo, (GetSshProt() == ssh2only));
    EnableControl(BugDeriveKey2Label, BugDeriveKey2Combo->Enabled);
    EnableControl(BugRSAPad2Combo, (GetSshProt() == ssh2only));
    EnableControl(BugRSAPad2Label, BugRSAPad2Combo->Enabled);
    EnableControl(BugPKSessID2Combo, (GetSshProt() == ssh2only));
    EnableControl(BugPKSessID2Label, BugPKSessID2Combo->Enabled);
    EnableControl(BugRekey2Combo, (GetSshProt() == ssh2only));
    EnableControl(BugRekey2Label, BugRekey2Combo->Enabled);
    EnableControl(BugMaxPkt2Combo, (GetSshProt() == ssh2only));
    EnableControl(BugMaxPkt2Label, BugMaxPkt2Combo->Enabled);
    EnableControl(BugIgnore2Combo, (GetSshProt() == ssh2only));
    EnableControl(BugIgnore2Label, BugIgnore2Combo->Enabled);
    EnableControl(BugWinAdjCombo, (GetSshProt() == ssh2only));
    EnableControl(BugWinAdjLabel, BugWinAdjCombo->Enabled);

    // connection/proxy sheet
    // this is probaqbly overkill, now we do not allow changing protocol on
    // the same window as changing proxy
    TComboBox * ProxyMethodCombo =
      (SshProtocol ? SshProxyMethodCombo : (FtpProtocol ? FtpProxyMethodCombo : NeonProxyMethodCombo));
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
    if (!Neon)
    {
      NeonProxyMethodCombo->Visible = false;
      NeonProxyMethodCombo->ItemIndex = GetSupportedNeonProxyMethod(ProxyMethod);
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
      ((ProxyMethod == pmSocks4) && (SshProtocol || Neon)) ||
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
    EnableControl(FollowDirectorySymlinksCheck, (SftpProtocol || ScpProtocol || FtpProtocol));

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
    EnableControl(TrimVMSVersionsCheck, !S3Protocol);

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

    // environment/s3
    S3Sheet->Enabled = S3Protocol;

    // tunnel sheet
    TunnelSheet->Enabled = SshProtocol;
    // probably needless
    EnableControl(TunnelSessionGroup, TunnelCheck->Enabled && TunnelCheck->Checked);
    EnableControl(TunnelOptionsGroup, TunnelSessionGroup->Enabled);
    EnableControl(TunnelAuthenticationParamsGroup, TunnelSessionGroup->Enabled);

    // connection/ssl/tls
    SslSheet->Enabled = Ssl;
    // TLS/SSL session reuse is not configurable for WebDAV/S3 yet
    SslSessionReuseCheck->Enabled = SslSheet->Enabled && FtpProtocol;

    // encryption sheet
    EncryptionSheet->Enabled = SftpProtocol;
    EnableControl(EncryptFilesGroup, EncryptFilesCheck->Checked);

    UpdateNavigationTree();

    // color
    if (FColor == 0)
    {
      MenuButton(ColorButton);
    }
    else
    {
      ColorButton->Images = ColorImageList;
      ColorButton->ImageIndex = GetSessionColorImage(ColorImageList, FColor, 0);
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
  InstallPathWordBreakProc(PrivateKeyEdit3);
  InstallPathWordBreakProc(TunnelPrivateKeyEdit3);
  InstallPathWordBreakProc(RemoteDirectoryEdit);
  InstallPathWordBreakProc(LocalDirectoryEdit);
  InstallPathWordBreakProc(RecycleBinPathEdit);
  InstallPathWordBreakProc(TlsCertificateFileEdit);
  InstallPathWordBreakProc(SftpServerEdit);
  InstallPathWordBreakProc(ShellEdit);

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
  FPrivateKeyMonitors.reset(NULL);
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
void __fastcall TSiteAdvancedDialog::PrivateKeyEdit3AfterDialog(TObject * Sender,
  UnicodeString & Name, bool & Action)
{
  PathEditAfterDialog(Sender, Name, Action);

  TFilenameEdit * Edit = dynamic_cast<TFilenameEdit *>(Sender);
  if (Name != Edit->Text)
  {
    VerifyAndConvertKey(Name, GetSshProt(), true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult == DefaultResult(this))
  {
    // StripPathQuotes should not be needed as we do not feed quotes anymore
    VerifyKey(StripPathQuotes(PrivateKeyEdit3->Text), GetSshProt());
    // for tunnel SSH version is not configurable
    VerifyKey(StripPathQuotes(TunnelPrivateKeyEdit3->Text), ssh2only);
    VerifyCertificate(StripPathQuotes(TlsCertificateFileEdit->Text));
    // Particularly for EncryptKey*Edit's
    ExitActiveControl(this);
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
  if (IsPathToSameFile(Name, ExpandEnvironmentVariables(FBeforeDialogPath)))
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
int __fastcall TSiteAdvancedDialog::GetSupportedNeonProxyMethod(int Method)
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
bool __fastcall TSiteAdvancedDialog::IsNeon(TFSProtocol FSProtocol)
{
  return (FSProtocol == fsWebDAV) || (FSProtocol == fsS3);
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
  else if (IsNeon(FSProtocol))
  {
    Result = (TProxyMethod)NeonProxyMethodCombo->ItemIndex;
  }
  else
  {
    DebugFail();
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
  // Reason for separate Menu variable is given in TPreferencesDialog::EditorFontColorButtonClick
  TPopupMenu * Menu = CreateSessionColorPopupMenu(FColor, SessionColorChange);
  // Popup menu has to survive the popup as TBX calls click handler asynchronously (post).
  FColorPopupMenu.reset(Menu);
  MenuPopup(Menu, ColorButton);
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::SessionColorChange(TColor Color)
{
  FColor = Color;
  UpdateControls();
}
//---------------------------------------------------------------------------
TTlsVersion __fastcall TSiteAdvancedDialog::IndexToTlsVersion(int Index)
{
  switch (Index)
  {
    default:
      DebugFail();
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
      DebugFail();
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
    NeonProxyMethodCombo->ItemIndex = pmHTTP;

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
void __fastcall TSiteAdvancedDialog::PrivateKeyCreatedOrModified(TObject * /*Sender*/, const UnicodeString FileName)
{
  if (SameText(ExtractFileExt(FileName), FORMAT(L".%s", (PuttyKeyExt))))
  {
    PrivateKeyEdit3->Text = FileName;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::PrivateKeyToolsButtonClick(TObject * /*Sender*/)
{
  UnicodeString Dummy;
  PrivateKeyGenerateItem->Enabled = FindTool(PuttygenTool, Dummy);
  PrivateKeyUploadItem->Enabled = (GetSshProt() == ssh2only) && (NormalizeFSProtocol(FSessionData->FSProtocol) == fsSFTP);
  MenuPopup(PrivateKeyMenu, PrivateKeyToolsButton);
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::PrivateKeyGenerateItemClick(TObject * /*Sender*/)
{
  unsigned int Filters = FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_LAST_WRITE;
  TObjectList * PrivateKeyMonitors =
    StartCreationDirectoryMonitorsOnEachDrive(Filters, PrivateKeyCreatedOrModified);
  FPrivateKeyMonitors.reset(PrivateKeyMonitors);
  ExecuteTool(PuttygenTool);
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::PrivateKeyUploadItemClick(TObject * /*Sender*/)
{
  // This has an unwanted side effect of persisting the values, as if the dialog was submitted.
  SaveSession();
  FSessionData->FSProtocol = fsSFTPonly; // no SCP fallback, as SCP does not implement GetHomeDirectory
  FSessionData->RemoteDirectory = UnicodeString();

  UnicodeString FileName = PrivateKeyEdit3->Text;
  if (TTerminalManager::Instance()->UploadPublicKey(NULL, FSessionData, FileName))
  {
    PrivateKeyEdit3->Text = FileName;
    PrivateKeyEdit3->SetFocus();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::PrivateKeyViewButtonClick(TObject * /*Sender*/)
{
  UnicodeString FileName = PrivateKeyEdit3->Text;
  VerifyAndConvertKey(FileName, GetSshProt(), false);
  PrivateKeyEdit3->Text = FileName;
  UnicodeString CommentDummy;
  UnicodeString Line = GetPublicKeyLine(FileName, CommentDummy);
  std::unique_ptr<TStrings> Messages(TextToStringList(Line));

  TClipboardHandler ClipboardHandler;
  ClipboardHandler.Text = Line;

  TMessageParams Params;
  TQueryButtonAlias Aliases[1];
  Aliases[0].Button = qaRetry;
  Aliases[0].Alias = LoadStr(COPY_KEY_BUTTON);
  Aliases[0].OnSubmit = &ClipboardHandler.Copy;
  Params.Aliases = Aliases;
  Params.AliasesCount = LENOF(Aliases);

  UnicodeString Message = LoadStr(LOGIN_AUTHORIZED_KEYS);
  int Answers = qaOK | qaRetry;
  MoreMessageDialog(Message, Messages.get(), qtInformation, Answers, HELP_LOGIN_AUTHORIZED_KEYS, &Params);
}
//---------------------------------------------------------------------------
TCustomEdit * __fastcall TSiteAdvancedDialog::GetEncryptKeyEdit(bool AShow)
{
  bool Show = (ShowEncryptionKeyCheck->Checked == AShow);
  return (Show ? static_cast<TCustomEdit *>(EncryptKeyVisibleEdit) : static_cast<TCustomEdit *>(EncryptKeyPasswordEdit));
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::ShowEncryptionKeyCheckClick(TObject * /*Sender*/)
{
  TCustomEdit * ShowEdit = GetEncryptKeyEdit();
  TCustomEdit * HideEdit = GetEncryptKeyEdit(false);
  if (DebugAlwaysTrue(ShowEdit->Visible != HideEdit->Visible) &&
      DebugAlwaysTrue(!ShowEdit->Visible))
  {
    UnicodeString Key = HideEdit->Text;
    ShowEdit->Visible = true;
    ShowEdit->Text = Key;
    HideEdit->Visible = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::GenerateKeyButtonClick(TObject * /*Sender*/)
{
  UnicodeString Key = BytesToHex(GenerateEncryptKey());
  GetEncryptKeyEdit()->Text = Key;

  TClipboardHandler ClipboardHandler;
  ClipboardHandler.Text = Key;

  TMessageParams Params;
  TQueryButtonAlias Aliases[1];
  Aliases[0].Button = qaRetry;
  Aliases[0].Alias = LoadStr(COPY_KEY_BUTTON);
  Aliases[0].OnSubmit = &ClipboardHandler.Copy;
  Params.Aliases = Aliases;
  Params.AliasesCount = LENOF(Aliases);

  MessageDialog(LoadStr(ENCRYPT_KEY_GENERATED), qtInformation, qaOK | qaRetry, HELP_FILE_ENCRYPTION, &Params);
}
//---------------------------------------------------------------------------
void __fastcall TSiteAdvancedDialog::EncryptKeyEditExit(TObject * /*Sender*/)
{
  if (!IsCancelButtonBeingClicked(this))
  {
    UnicodeString HexKey = GetEncryptKeyEdit()->Text;
    if (!HexKey.IsEmpty())
    {
      ValidateEncryptKey(HexToBytes(HexKey));
    }
  }
}
//---------------------------------------------------------------------------
