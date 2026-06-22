//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SessionData.h"

#include "Common.h"
#include "Exceptions.h"
#include "FileBuffer.h"
#include "CoreMain.h"
#include "TextsCore.h"
#include "PuttyIntf.h"
#include "RemoteFiles.h"
#include "SftpFileSystem.h"
#include "S3FileSystem.h"
#include "FileMasks.h"
#include <Soap.EncdDecd.hpp>
#include <StrUtils.hpp>
#include <XMLDoc.hpp>
#include <System.IOUtils.hpp>
#include <algorithm>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define SET_SESSION_PROPERTY_FROM(PROPERTY, FROM) \
  if (F##PROPERTY != FROM) { F##PROPERTY = FROM; Modify(); }
//---------------------------------------------------------------------------
#define SET_SESSION_PROPERTY(PROPERTY) \
  SET_SESSION_PROPERTY_FROM(PROPERTY, value)
//---------------------------------------------------------------------------
const wchar_t * PingTypeNames = L"Off;Null;Dummy";
const wchar_t * FtpPingTypeNames = L"Off;Dummy;Dummy;List";
const wchar_t * ProxyMethodNames = L"None;SOCKS4;SOCKS5;HTTP;Telnet;Cmd";
static TIntMapping ProxyMethodMapping = CreateIntMappingFromEnumNames(LowerCase(ProxyMethodNames));
static const wchar_t * DefaultName = L"Default Settings";
const UnicodeString CipherNames[CIPHER_COUNT] = {L"WARN", L"3des", L"blowfish", L"aes", L"des", L"arcfour", L"chacha20", "aesgcm"};
const UnicodeString KexNames[KEX_COUNT] = {L"WARN", L"dh-group1-sha1", L"dh-group14-sha1", L"dh-group15-sha512", L"dh-group16-sha512", L"dh-group17-sha512", L"dh-group18-sha512", L"dh-gex-sha1", L"rsa", L"ecdh", L"ntru-curve25519", L"mlkem-curve25519", L"mlkem-nist"};
const UnicodeString HostKeyNames[HOSTKEY_COUNT] = {L"WARN", L"rsa", L"dsa", L"ecdsa", L"ed25519", L"ed448"};
const UnicodeString GssLibNames[GSSLIB_COUNT] = {L"gssapi32", L"sspi", L"custom"};
// Update also order in SshCipherList()
const TCipher DefaultCipherList[CIPHER_COUNT] =
  { cipAES, cipChaCha20, cipAESGCM, cip3DES, cipWarn, cipDES, cipBlowfish, cipArcfour };
// Update also order in SshKexList()
const TKex DefaultKexList[KEX_COUNT] =
  { kexNTRUHybrid, kexMLKEM25519Hybrid, kexMLKEMNISTHybrid, kexECDH, kexDHGEx, kexDHGroup18, kexDHGroup17, kexDHGroup16, kexDHGroup15, kexDHGroup14, kexRSA, kexWarn, kexDHGroup1 };
const THostKey DefaultHostKeyList[HOSTKEY_COUNT] =
  { hkED448, hkED25519, hkECDSA, hkRSA, hkDSA, hkWarn };
const TGssLib DefaultGssLibList[GSSLIB_COUNT] =
  { gssGssApi32, gssSspi, gssCustom };
const wchar_t FSProtocolNames[FSPROTOCOL_COUNT][16] = { L"SCP", L"SFTP (SCP)", L"SFTP", L"", L"", L"FTP", L"WebDAV", L"S3" };
const int SshPortNumber = 22;
const int FtpPortNumber = 21;
const int FtpsImplicitPortNumber = 990;
const int HTTPPortNumber = 80;
const int HTTPSPortNumber = 443;
const int TelnetPortNumber = 23;
const int DefaultSendBuf = 262144;
const int ProxyPortNumber = 80;
const UnicodeString AnonymousUserName(L"anonymous");
const UnicodeString AnonymousPassword(L"anonymous@example.com");
const UnicodeString PuttySshProtocol(L"ssh");
const UnicodeString PuttyTelnetProtocol(L"telnet");
const UnicodeString SftpProtocol(L"sftp");
const UnicodeString ScpProtocol(L"scp");
const UnicodeString FtpProtocol(L"ftp");
const UnicodeString FtpsProtocol(L"ftps");
const UnicodeString FtpesProtocol(L"ftpes");
const UnicodeString WebDAVProtocol(L"dav");
const UnicodeString WebDAVSProtocol(L"davs");
const UnicodeString S3Protocol(L"s3");
const UnicodeString S3PlainProtocol(L"s3plain");
const UnicodeString SshProtocol(L"ssh");
const UnicodeString WinSCPProtocolPrefix(L"winscp-");
const wchar_t UrlParamSeparator = L';';
const wchar_t UrlParamValueSeparator = L'=';
const UnicodeString UrlHostKeyParamName(L"fingerprint");
const UnicodeString UrlSaveParamName(L"save");
const UnicodeString UrlRawSettingsParamNamePrefix(L"x-");
const UnicodeString PassphraseOption(L"passphrase");
const UnicodeString RawSettingsOption(L"rawsettings");
const UnicodeString S3HostName(S3LibDefaultHostName());
const UnicodeString S3GoogleCloudHostName(L"storage.googleapis.com");
const UnicodeString OpensshHostDirective(L"Host");
//---------------------------------------------------------------------
TDateTime __fastcall SecToDateTime(int Sec)
{
  return TDateTime(double(Sec) / SecsPerDay);
}
//---------------------------------------------------------------------
static bool IsValidOpensshLine(const UnicodeString & Line)
{
  return !Line.IsEmpty() && (Line[1] != L'#');
}
//---------------------------------------------------------------------
bool ParseOpensshDirective(const UnicodeString & ALine, UnicodeString & Directive, UnicodeString & Value)
{
  bool Result = IsValidOpensshLine(ALine);
  if (Result)
  {
    UnicodeString Line = Trim(ALine);
    if (Line.SubString(1, 1) == "\"")
    {
      Line.Delete(1, 1);
      int P = Line.Pos("\"");
      Result = (P > 0);
      if (Result)
      {
        Directive = Line.SubString(1, P - 1);
        Line = MidStr(Line, P + 1).Trim();
      }
    }
    else
    {
      wchar_t Equal = L'=';
      UnicodeString Whitespaces(L" \t");
      UnicodeString Delimiters(Whitespaces + Equal);
      int P = FindDelimiter(Delimiters, Line);
      Result = (P > 0);
      if (Result)
      {
        Directive = Line.SubString(1, P - 1);
        Line.Delete(1, P - 1);
        UnicodeString TrimChars = Delimiters;
        while (!Line.IsEmpty() && Line.IsDelimiter(TrimChars, 1))
        {
          if (Line[1] == Equal)
          {
            TrimChars = Whitespaces;
          }
          Line.Delete(1, 1);
        }
      }
    }

    Value = Line;
    Result = !Value.IsEmpty();
  }
  return Result;
}
//--- TSessionData ----------------------------------------------------
__fastcall TSessionData::TSessionData(UnicodeString aName):
  TNamedObject(aName)
{
  Default();
  FModified = true;
}
//---------------------------------------------------------------------
_fastcall TSessionData::~TSessionData()
{
}
//---------------------------------------------------------------------
int __fastcall TSessionData::Compare(TNamedObject * Other)
{
  int Result;
  // To avoid using CompareLogicalText on hex names of sessions in workspace.
  // The session 000A would be sorted before 0001.
  if (IsWorkspace && DebugNotNull(dynamic_cast<TSessionData *>(Other))->IsWorkspace)
  {
    Result = CompareText(Name, Other->Name);
  }
  else
  {
    Result = TNamedObject::Compare(Other);
  }
  return Result;
}
//---------------------------------------------------------------------
TSessionData * __fastcall TSessionData::Clone()
{
  std::unique_ptr<TSessionData> Data(new TSessionData(L""));
  Data->Assign(this);
  return Data.release();
}
//---------------------------------------------------------------------
void __fastcall TSessionData::DefaultSettings()
{
  HostName = L"";
  PortNumber = SshPortNumber;
  UserName = L"";
  Password = L"";
  NewPassword = L"";
  ChangePassword = false;
  PingInterval = 30;
  PingType = ptOff;
  Timeout = 15;
  TryAgent = true;
  AgentFwd = false;
  AuthKI = true;
  AuthKIPassword = true;
  AuthGSSAPI = true;
  AuthGSSAPIKEX = false;
  GSSAPIFwdTGT = false;
  LogicalHostName = L"";
  ChangeUsername = false;
  Compression = false;
  Ssh2DES = false;
  SshNoUserAuth = false;
  for (int Index = 0; Index < CIPHER_COUNT; Index++)
  {
    Cipher[Index] = DefaultCipherList[Index];
  }
  for (int Index = 0; Index < KEX_COUNT; Index++)
  {
    Kex[Index] = DefaultKexList[Index];
  }
  for (int Index = 0; Index < HOSTKEY_COUNT; Index++)
  {
    HostKeys[Index] = DefaultHostKeyList[Index];
  }
  for (int Index = 0; Index < GSSLIB_COUNT; Index++)
  {
    GssLib[Index] = DefaultGssLibList[Index];
  }
  GssLibCustom = L"";
  PublicKeyFile = EmptyStr;
  DetachedCertificate = EmptyStr;
  Passphrase = L"";
  FPuttyProtocol = L"";
  TcpNoDelay = false;
  SendBuf = DefaultSendBuf;
  SourceAddress = L"";
  ProtocolFeatures = L"";
  SshSimple = true;
  HostKey = L"";
  FingerprintScan = false;
  FOverrideCachedHostKey = true;
  Note = L"";
  WinTitle = L"";
  InternalEditorEncoding = -1;

  EncryptKey = UnicodeString();

  WebDavLiberalEscaping = false;
  WebDavAuthLegacy = false;

  ProxyMethod = ::pmNone;
  ProxyHost = L"proxy";
  ProxyPort = ProxyPortNumber;
  ProxyUsername = L"";
  ProxyPassword = L"";
  ProxyTelnetCommand = L"connect %host %port\\n";
  ProxyLocalCommand = L"";
  ProxyDNS = asAuto;
  ProxyLocalhost = false;

  for (unsigned int Index = 0; Index < LENOF(FBugs); Index++)
  {
    Bug[(TSshBug)Index] = asAuto;
  }

  Special = false;
  FSProtocol = fsSFTP;
  AddressFamily = afAuto;
  RekeyData = L"1G";
  RekeyTime = MinsPerHour;

  // FS common
  LocalDirectory = L"";
  OtherLocalDirectory = L"";
  RemoteDirectory = L"";
  SynchronizeBrowsing = false;
  UpdateDirectories = true;
  RequireDirectories = false;
  CacheDirectories = true;
  CacheDirectoryChanges = true;
  PreserveDirectoryChanges = true;
  ResolveSymlinks = true;
  FollowDirectorySymlinks = false;
  DSTMode = dstmUnix;
  DeleteToRecycleBin = false;
  OverwrittenToRecycleBin = false;
  RecycleBinPath = L"";
  Color = 0;
  PostLoginCommands = L"";

  // SCP
  LookupUserGroups = asAuto;
  EOLType = eolLF;
  TrimVMSVersions = false;
  VMSAllRevisions = false;
  Shell = L""; //default shell
  ReturnVar = L"";
  ExitCode1IsError = false;
  ClearAliases = true;
  UnsetNationalVars = true;
  ListingCommand = L"ls -la";
  IgnoreLsWarnings = true;
  Scp1Compatibility = false;
  TimeDifference = 0;
  TimeDifferenceAuto = true;
  SCPLsFullTime = asAuto;
  NotUtf = asAuto;

  // S3
  S3DefaultRegion = EmptyStr;
  S3SessionToken = EmptyStr;
  S3RoleArn = EmptyStr;
  S3RoleSessionName = EmptyStr;
  S3Profile = EmptyStr;
  S3UrlStyle = s3usVirtualHost;
  S3MaxKeys = asAuto;
  S3CredentialsEnv = false;
  S3RequesterPays = false;

  // SFTP
  SftpServer = L"";
  SFTPDownloadQueue = 32;
  SFTPUploadQueue = 64;
  SFTPListingQueue = 2;
  SFTPMaxVersion = SFTPMaxVersionAuto;
  SFTPMaxPacketSize = 0;
  SFTPRealPath = asAuto;
  UsePosixRename = false;

  for (unsigned int Index = 0; Index < LENOF(FSFTPBugs); Index++)
  {
    SFTPBug[(TSftpBug)Index] = asAuto;
  }

  Tunnel = false;
  TunnelHostName = L"";
  TunnelPortNumber = SshPortNumber;
  TunnelUserName = L"";
  TunnelPassword = L"";
  TunnelPublicKeyFile = L"";
  TunnelPassphrase = L"";
  TunnelLocalPortNumber = 0;
  TunnelPortFwd = L"";
  TunnelHostKey = L"";

  // FTP
  FtpPasvMode = true;
  FtpForcePasvIp = asAuto;
  FtpUseMlsd = asAuto;
  FtpAccount = L"";
  FtpPingInterval = 30;
  FtpPingType = fptDummyCommand;
  FtpTransferActiveImmediately = asAuto;
  Ftps = ftpsNone;
  MinTlsVersion = tlsDefaultMin;
  MaxTlsVersion = tlsMax;
  CompleteTlsShutdown = asAuto;
  FtpListAll = asAuto;
  FtpHost = asAuto;
  FtpWorkFromCwd = asAuto;
  FtpAnyCodeForPwd = false;
  SslSessionReuse = true;
  TlsCertificateFile = L"";

  FtpProxyLogonType = 0; // none

  PuttySettings = UnicodeString();

  CustomParam1 = L"";
  CustomParam2 = L"";
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Default()
{
  DefaultSettings();

  IsWorkspace = false;
  Link = L"";
  NameOverride = L"";

  Selected = false;
  FModified = false;
  FSource = ::ssNone;
  FSaveOnly = false;

  // add also to TSessionLog::AddStartupInfo()
}
//---------------------------------------------------------------------
void __fastcall TSessionData::NonPersistent()
{
  UpdateDirectories = false;
  PreserveDirectoryChanges = false;
}
//---------------------------------------------------------------------
#define PROPERTY(P) PROPERTY_HANDLER(P, )
#define BASE_PROPERTIES \
  PROPERTY(HostName); \
  PROPERTY(PortNumber); \
  PROPERTY(UserName); \
  PROPERTY_HANDLER(Password, F); \
  PROPERTY(PublicKeyFile); \
  PROPERTY(DetachedCertificate); \
  PROPERTY_HANDLER(Passphrase, F); \
  PROPERTY(FSProtocol); \
  PROPERTY(Ftps); \
  PROPERTY(LocalDirectory); \
  PROPERTY(OtherLocalDirectory); \
  PROPERTY(RemoteDirectory); \
  PROPERTY(RequireDirectories); \
  PROPERTY(Color); \
  PROPERTY(SynchronizeBrowsing); \
  PROPERTY(Note);
//---------------------------------------------------------------------
#define ADVANCED_PROPERTIES \
  PROPERTY_HANDLER(NewPassword, F); \
  PROPERTY(ChangePassword); \
  PROPERTY(PingInterval); \
  PROPERTY(PingType); \
  PROPERTY(Timeout); \
  PROPERTY(TryAgent); \
  PROPERTY(AgentFwd); \
  PROPERTY(LogicalHostName); \
  PROPERTY(ChangeUsername); \
  PROPERTY(Compression); \
  PROPERTY(Ssh2DES); \
  PROPERTY(SshNoUserAuth); \
  PROPERTY(CipherList); \
  PROPERTY(KexList); \
  PROPERTY(HostKeyList); \
  PROPERTY(GssLibList); \
  PROPERTY(GssLibCustom); \
  PROPERTY(AddressFamily); \
  PROPERTY(RekeyData); \
  PROPERTY(RekeyTime); \
  PROPERTY(HostKey); \
  PROPERTY(FingerprintScan); \
  PROPERTY(InternalEditorEncoding); \
  \
  PROPERTY(UpdateDirectories); \
  PROPERTY(CacheDirectories); \
  PROPERTY(CacheDirectoryChanges); \
  PROPERTY(PreserveDirectoryChanges); \
  \
  PROPERTY(ResolveSymlinks); \
  PROPERTY(FollowDirectorySymlinks); \
  PROPERTY(DSTMode); \
  PROPERTY(Special); \
  PROPERTY(Selected); \
  PROPERTY(ReturnVar); \
  PROPERTY(ExitCode1IsError); \
  PROPERTY(LookupUserGroups); \
  PROPERTY(EOLType); \
  PROPERTY(TrimVMSVersions); \
  PROPERTY(VMSAllRevisions); \
  PROPERTY(Shell); \
  PROPERTY(ClearAliases); \
  PROPERTY(Scp1Compatibility); \
  PROPERTY(UnsetNationalVars); \
  PROPERTY(ListingCommand); \
  PROPERTY(IgnoreLsWarnings); \
  PROPERTY(SCPLsFullTime); \
  \
  PROPERTY(TimeDifference); \
  PROPERTY(TimeDifferenceAuto); \
  PROPERTY(TcpNoDelay); \
  PROPERTY(SendBuf); \
  PROPERTY(SourceAddress); \
  PROPERTY(ProtocolFeatures); \
  PROPERTY(SshSimple); \
  PROPERTY(AuthKI); \
  PROPERTY(AuthKIPassword); \
  PROPERTY(AuthGSSAPI); \
  PROPERTY(AuthGSSAPIKEX); \
  PROPERTY(GSSAPIFwdTGT); \
  PROPERTY(DeleteToRecycleBin); \
  PROPERTY(OverwrittenToRecycleBin); \
  PROPERTY(RecycleBinPath); \
  PROPERTY(NotUtf); \
  PROPERTY(PostLoginCommands); \
  \
  PROPERTY(S3DefaultRegion); \
  PROPERTY(S3SessionToken); \
  PROPERTY(S3RoleArn); \
  PROPERTY(S3RoleSessionName); \
  PROPERTY(S3Profile); \
  PROPERTY(S3UrlStyle); \
  PROPERTY(S3MaxKeys); \
  PROPERTY(S3CredentialsEnv); \
  PROPERTY(S3RequesterPays); \
  \
  PROPERTY(ProxyMethod); \
  PROPERTY(ProxyHost); \
  PROPERTY(ProxyPort); \
  PROPERTY(ProxyUsername); \
  PROPERTY_HANDLER(ProxyPassword, F); \
  PROPERTY(ProxyTelnetCommand); \
  PROPERTY(ProxyLocalCommand); \
  PROPERTY(ProxyDNS); \
  PROPERTY(ProxyLocalhost); \
  \
  for (unsigned int Index = 0; Index < LENOF(FBugs); Index++) \
  { \
    PROPERTY(Bug[(TSshBug)Index]); \
  } \
  \
  PROPERTY(SftpServer); \
  PROPERTY(SFTPDownloadQueue); \
  PROPERTY(SFTPUploadQueue); \
  PROPERTY(SFTPListingQueue); \
  PROPERTY(SFTPMaxVersion); \
  PROPERTY(SFTPMaxPacketSize); \
  PROPERTY(SFTPRealPath); \
  PROPERTY(UsePosixRename); \
  \
  for (unsigned int Index = 0; Index < LENOF(FSFTPBugs); Index++) \
  { \
    PROPERTY(SFTPBug[(TSftpBug)Index]); \
  } \
  \
  PROPERTY(Tunnel); \
  PROPERTY(TunnelHostName); \
  PROPERTY(TunnelPortNumber); \
  PROPERTY(TunnelUserName); \
  PROPERTY_HANDLER(TunnelPassword, F); \
  PROPERTY(TunnelPublicKeyFile); \
  PROPERTY_HANDLER(TunnelPassphrase, F); \
  PROPERTY(TunnelLocalPortNumber); \
  PROPERTY(TunnelPortFwd); \
  PROPERTY(TunnelHostKey); \
  \
  PROPERTY(FtpPasvMode); \
  PROPERTY(FtpForcePasvIp); \
  PROPERTY(FtpUseMlsd); \
  PROPERTY(FtpAccount); \
  PROPERTY(FtpPingInterval); \
  PROPERTY(FtpPingType); \
  PROPERTY(FtpTransferActiveImmediately); \
  PROPERTY(FtpListAll); \
  PROPERTY(FtpHost); \
  PROPERTY(FtpWorkFromCwd); \
  PROPERTY(FtpAnyCodeForPwd); \
  PROPERTY(SslSessionReuse); \
  PROPERTY(TlsCertificateFile); \
  \
  PROPERTY(FtpProxyLogonType); \
  \
  PROPERTY(MinTlsVersion); \
  PROPERTY(MaxTlsVersion); \
  PROPERTY(CompleteTlsShutdown); \
  \
  PROPERTY(WinTitle); \
  \
  PROPERTY_HANDLER(EncryptKey, F); \
  \
  PROPERTY(WebDavLiberalEscaping); \
  PROPERTY(WebDavAuthLegacy); \
  \
  PROPERTY(PuttySettings); \
  \
  PROPERTY(CustomParam1); \
  PROPERTY(CustomParam2);
#define META_PROPERTIES \
  PROPERTY(IsWorkspace); \
  PROPERTY(Link); \
  PROPERTY(NameOverride);
//---------------------------------------------------------------------
void __fastcall TSessionData::Assign(TPersistent * Source)
{
  if (Source && Source->InheritsFrom(__classid(TSessionData)))
  {
    TSessionData * SourceData = (TSessionData *)Source;
    // Master password prompt shows implicitly here, when cloning the session data for a new terminal
    CopyData(SourceData);
    FSource = SourceData->FSource;
  }
  else
  {
    TNamedObject::Assign(Source);
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::DoCopyData(TSessionData * SourceData, bool NoRecrypt)
{
  #define PROPERTY_HANDLER(P, F) \
    if (NoRecrypt) \
    { \
      F##P = SourceData->F##P; \
    } \
    else \
    { \
      P = SourceData->P; \
    }
  PROPERTY(Name);
  BASE_PROPERTIES;
  ADVANCED_PROPERTIES;
  META_PROPERTIES;
  #undef PROPERTY_HANDLER
  FOverrideCachedHostKey = SourceData->FOverrideCachedHostKey;
  FModified = SourceData->Modified;
  FSaveOnly = SourceData->FSaveOnly;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::CopyData(TSessionData * SourceData)
{
  DoCopyData(SourceData, false);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::CopyDataNoRecrypt(TSessionData * SourceData)
{
  DoCopyData(SourceData, true);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::CopyDirectoriesStateData(TSessionData * SourceData)
{
  RemoteDirectory = SourceData->RemoteDirectory;
  LocalDirectory = SourceData->LocalDirectory;
  OtherLocalDirectory = SourceData->OtherLocalDirectory;
  SynchronizeBrowsing = SourceData->SynchronizeBrowsing;
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::HasStateData()
{
  return
    !RemoteDirectory.IsEmpty() ||
    !LocalDirectory.IsEmpty() ||
    !OtherLocalDirectory.IsEmpty() ||
    (Color != 0);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::CopyStateData(TSessionData * SourceData)
{
  // Keep in sync with TCustomScpExplorerForm::UpdateSessionData.
  CopyDirectoriesStateData(SourceData);
  Color = SourceData->Color;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::CopyNonCoreData(TSessionData * SourceData)
{
  CopyStateData(SourceData);
  UpdateDirectories = SourceData->UpdateDirectories;
  Note = SourceData->Note;
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::IsSame(
  const TSessionData * Default, bool AdvancedOnly, TStrings * DifferentProperties, bool Decrypted)
{
  bool Result = true;
  #define PROPERTY_HANDLER(P, F) \
    if ((Decrypted && (P != Default->P)) || \
        (!Decrypted && (F##P != Default->F##P))) \
    { \
      Result = false; \
      if (DifferentProperties != NULL) \
      { \
        DifferentProperties->Add(#P); \
      } \
      else \
      { \
        return Result; \
      } \
    }

  if (!AdvancedOnly)
  {
    BASE_PROPERTIES;
    META_PROPERTIES;
  }
  ADVANCED_PROPERTIES;
  #undef PROPERTY_HANDLER
  return Result;
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::IsSame(const TSessionData * Default, bool AdvancedOnly)
{
  return IsSame(Default, AdvancedOnly, NULL, false);
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::IsSameDecrypted(const TSessionData * Default)
{
  return IsSame(Default, false, NULL, true);
}
//---------------------------------------------------------------------
TFSProtocol NormalizeFSProtocol(TFSProtocol FSProtocol)
{
  if (FSProtocol == fsSFTPonly)
  {
    FSProtocol = fsSFTP;
  }
  return FSProtocol;
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::IsSameSite(const TSessionData * Other)
{
  return
    // Particularly when handling /refresh,
    // fsSFTPonly sites when compared against sftp:// URLs (fsSFTP) have to match.
    // But similarly also falled back SCP sites.
    (NormalizeFSProtocol(FSProtocol) == NormalizeFSProtocol(Other->FSProtocol)) &&
    (HostName == Other->HostName) &&
    (PortNumber == Other->PortNumber) &&
    (UserName == Other->UserName);
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::IsInFolderOrWorkspace(const UnicodeString & AFolder)
{
  return StartsText(UnixIncludeTrailingBackslash(AFolder), Name);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::DoLoad(THierarchicalStorage * Storage, bool PuttyImport, bool & RewritePassword, bool Unsafe, bool RespectDisablePasswordStoring)
{
  // Make sure we only ever use methods supported by TOptionsStorage
  // (implemented by TOptionsIniFile)

  PortNumber = Storage->ReadInteger(L"PortNumber", PortNumber);
  UserName = Storage->ReadString(L"UserName", UserName);
  // must be loaded after UserName, because HostName may be in format user@host
  HostName = Storage->ReadString(L"HostName", HostName);

  #define LOAD_PASSWORD_EX(PROP, PLAIN_NAME, ENC_NAME, ONPLAIN) \
    if (Storage->ValueExists(PLAIN_NAME)) \
    { \
      PROP = Storage->ReadString(PLAIN_NAME, PROP); \
      ONPLAIN \
    } \
    else \
    { \
      RawByteString A##PROP = Storage->ReadStringAsBinaryData(ENC_NAME, F##PROP); \
      SET_SESSION_PROPERTY_FROM(PROP, A##PROP); \
    }
  #define LOAD_PASSWORD(PROP, PLAIN_NAME) LOAD_PASSWORD_EX(PROP, PLAIN_NAME, TEXT(#PROP), RewritePassword = true;)
  bool LoadPasswords = !Configuration->DisablePasswordStoring || !RespectDisablePasswordStoring;
  if (LoadPasswords)
  {
    LOAD_PASSWORD(Password, L"PasswordPlain");
  }
  HostKey = Storage->ReadString(L"SshHostKey", HostKey); // probably never used
  Note = Storage->ReadString(L"Note", Note);
  // Putty uses PingIntervalSecs
  int PingIntervalSecs = Storage->ReadInteger(L"PingIntervalSecs", -1);
  if (PingIntervalSecs < 0)
  {
    PingIntervalSecs = Storage->ReadInteger(L"PingIntervalSec", PingInterval%SecsPerMin);
  }
  PingInterval =
    Storage->ReadInteger(L"PingInterval", PingInterval/SecsPerMin)*SecsPerMin +
    PingIntervalSecs;
  if (PingInterval == 0)
  {
    PingInterval = 30;
  }
  PingType = static_cast<TPingType>(Storage->ReadInteger(L"PingType", PingType));
  Timeout = Storage->ReadInteger(L"Timeout", Timeout);
  TryAgent = Storage->ReadBool(L"TryAgent", TryAgent);
  AgentFwd = Storage->ReadBool(L"AgentFwd", AgentFwd);
  AuthKI = Storage->ReadBool(L"AuthKI", AuthKI);
  AuthKIPassword = Storage->ReadBool(L"AuthKIPassword", AuthKIPassword);
  // Continue to use setting keys of previous kerberos implementation (vaclav tomec),
  // but fallback to keys of other implementations (official putty and vintela quest putty),
  // to allow imports from all putty versions.
  // Both vaclav tomec and official putty use AuthGSSAPI
  AuthGSSAPI = Storage->ReadBool(L"AuthGSSAPI", Storage->ReadBool(L"AuthSSPI", AuthGSSAPI));
  AuthGSSAPIKEX = Storage->ReadBool(L"AuthGSSAPIKEX", AuthGSSAPIKEX);
  GSSAPIFwdTGT = Storage->ReadBool(L"GSSAPIFwdTGT", Storage->ReadBool(L"GssapiFwd", Storage->ReadBool(L"SSPIFwdTGT", GSSAPIFwdTGT)));
  // KerbPrincipal was used by Quest PuTTY
  // GSSAPIServerRealm was used by Vaclav Tomec
  LogicalHostName = Storage->ReadString(L"LogicalHostName", Storage->ReadString(L"GSSAPIServerRealm", Storage->ReadString(L"KerbPrincipal", LogicalHostName)));
  ChangeUsername = Storage->ReadBool(L"ChangeUsername", ChangeUsername);
  Compression = Storage->ReadBool(L"Compression", Compression);
  Ssh2DES = Storage->ReadBool(L"Ssh2DES", Ssh2DES);
  SshNoUserAuth = Storage->ReadBool(L"SshNoUserAuth", SshNoUserAuth);
  CipherList = Storage->ReadString(L"Cipher", CipherList);
  KexList = Storage->ReadString(L"KEX", KexList);
  HostKeyList = Storage->ReadString(L"HostKey", HostKeyList);
  if (!Unsafe)
  {
    GssLibList = Storage->ReadString(L"GSSLibs", GssLibList);
  }
  GssLibCustom = Storage->ReadString(L"GSSCustom", GssLibCustom);
  PublicKeyFile = Storage->ReadString(L"PublicKeyFile", PublicKeyFile);
  DetachedCertificate = Storage->ReadString(L"DetachedCertificate", DetachedCertificate);
  AddressFamily = static_cast<TAddressFamily>
    (Storage->ReadInteger(L"AddressFamily", AddressFamily));
  RekeyData = Storage->ReadString(L"RekeyBytes", RekeyData);
  RekeyTime = Storage->ReadInteger(L"RekeyTime", RekeyTime);

  FSProtocol = (TFSProtocol)Storage->ReadInteger(L"FSProtocol", FSProtocol);
  LocalDirectory = Storage->ReadString(L"LocalDirectory", LocalDirectory);
  OtherLocalDirectory = Storage->ReadString(L"OtherLocalDirectory", OtherLocalDirectory);
  RemoteDirectory = Storage->ReadString(L"RemoteDirectory", RemoteDirectory);
  SynchronizeBrowsing = Storage->ReadBool(L"SynchronizeBrowsing", SynchronizeBrowsing);
  UpdateDirectories = Storage->ReadBool(L"UpdateDirectories", UpdateDirectories);
  CacheDirectories = Storage->ReadBool(L"CacheDirectories", CacheDirectories);
  CacheDirectoryChanges = Storage->ReadBool(L"CacheDirectoryChanges", CacheDirectoryChanges);
  PreserveDirectoryChanges = Storage->ReadBool(L"PreserveDirectoryChanges", PreserveDirectoryChanges);

  ResolveSymlinks = Storage->ReadBool(L"ResolveSymlinks", ResolveSymlinks);
  FollowDirectorySymlinks = Storage->ReadBool(L"FollowDirectorySymlinks", FollowDirectorySymlinks);
  DSTMode = (TDSTMode)Storage->ReadInteger(L"ConsiderDST", DSTMode);
  Special = Storage->ReadBool(L"Special", Special);
  if (!Unsafe)
  {
    Shell = Storage->ReadString(L"Shell", Shell);
  }
  ClearAliases = Storage->ReadBool(L"ClearAliases", ClearAliases);
  UnsetNationalVars = Storage->ReadBool(L"UnsetNationalVars", UnsetNationalVars);
  if (!Unsafe)
  {
    ListingCommand = Storage->ReadString(L"ListingCommand",
      Storage->ReadBool(L"AliasGroupList", false) ? UnicodeString(L"ls -gla") : ListingCommand);
  }
  IgnoreLsWarnings = Storage->ReadBool(L"IgnoreLsWarnings", IgnoreLsWarnings);
  SCPLsFullTime = Storage->ReadEnum(L"SCPLsFullTime", SCPLsFullTime, AutoSwitchMapping);
  Scp1Compatibility = Storage->ReadBool(L"Scp1Compatibility", Scp1Compatibility);
  TimeDifference = Storage->ReadFloat(L"TimeDifference", TimeDifference);
  TimeDifferenceAuto = Storage->ReadBool(L"TimeDifferenceAuto", (TimeDifference == TDateTime()));
  if (!Unsafe)
  {
    DeleteToRecycleBin = Storage->ReadBool(L"DeleteToRecycleBin", DeleteToRecycleBin);
    OverwrittenToRecycleBin = Storage->ReadBool(L"OverwrittenToRecycleBin", OverwrittenToRecycleBin);
    RecycleBinPath = Storage->ReadString(L"RecycleBinPath", RecycleBinPath);
    PostLoginCommands = Storage->ReadString(L"PostLoginCommands", PostLoginCommands);
    ReturnVar = Storage->ReadString(L"ReturnVar", ReturnVar);
  }

  ExitCode1IsError = Storage->ReadBool(L"ExitCode1IsError", ExitCode1IsError);
  LookupUserGroups = Storage->ReadEnum(L"LookupUserGroups2", LookupUserGroups, AutoSwitchMapping);
  EOLType = (TEOLType)Storage->ReadInteger(L"EOLType", EOLType);
  TrimVMSVersions = Storage->ReadBool(L"TrimVMSVersions", TrimVMSVersions);
  VMSAllRevisions = Storage->ReadBool(L"VMSAllRevisions", VMSAllRevisions);
  NotUtf = Storage->ReadEnum(L"Utf", Storage->ReadEnum(L"SFTPUtfBug", NotUtf), AutoSwitchReversedMapping);
  InternalEditorEncoding = Storage->ReadInteger(L"InternalEditorEncoding", InternalEditorEncoding);

  S3DefaultRegion = Storage->ReadString(L"S3DefaultRegion", S3DefaultRegion);
  S3SessionToken = Storage->ReadString(L"S3SessionToken", S3SessionToken);
  S3RoleArn = Storage->ReadString(L"S3RoleArn", S3RoleArn);
  S3RoleSessionName = Storage->ReadString(L"S3RoleSessionName", S3RoleSessionName);
  S3Profile = Storage->ReadString(L"S3Profile", S3Profile);
  S3UrlStyle = (TS3UrlStyle)Storage->ReadInteger(L"S3UrlStyle", S3UrlStyle);
  S3MaxKeys = Storage->ReadEnum(L"S3MaxKeys", S3MaxKeys, AutoSwitchMapping);
  S3CredentialsEnv = Storage->ReadBool(L"S3CredentialsEnv", S3CredentialsEnv);
  S3RequesterPays = Storage->ReadBool(L"S3RequesterPays", S3RequesterPays);

  // PuTTY defaults to TcpNoDelay, but the psftp/pscp ignores this preference, and always set this to off (what is our default too)
  if (!PuttyImport)
  {
    TcpNoDelay = Storage->ReadBool(L"TcpNoDelay", TcpNoDelay);
  }
  SendBuf = Storage->ReadInteger(L"SendBuf", Storage->ReadInteger("SshSendBuf", SendBuf));
  SourceAddress = Storage->ReadString(L"SourceAddress", SourceAddress);
  ProtocolFeatures = Storage->ReadString(L"ProtocolFeatures", ProtocolFeatures);
  SshSimple = Storage->ReadBool(L"SshSimple", SshSimple);

  ProxyMethod = Storage->ReadEnum(L"ProxyMethod", ProxyMethod, ProxyMethodMapping);
  ProxyHost = Storage->ReadString(L"ProxyHost", ProxyHost);
  ProxyPort = Storage->ReadInteger(L"ProxyPort", ProxyPort);
  ProxyUsername = Storage->ReadString(L"ProxyUsername", ProxyUsername);
  // proxy password is not rewritten
  LOAD_PASSWORD_EX(ProxyPassword, L"ProxyPassword", L"ProxyPasswordEnc", );
  if (!Unsafe)
  {
    if (ProxyMethod == pmCmd)
    {
      ProxyLocalCommand = Storage->ReadStringRaw(L"ProxyTelnetCommand", ProxyLocalCommand);
    }
    else
    {
      ProxyTelnetCommand = Storage->ReadStringRaw(L"ProxyTelnetCommand", ProxyTelnetCommand);
    }
  }
  ProxyDNS = TAutoSwitch((Storage->ReadInteger(L"ProxyDNS", (ProxyDNS + 2) % 3) + 1) % 3);
  ProxyLocalhost = Storage->ReadBool(L"ProxyLocalhost", ProxyLocalhost);

  #define READ_BUG(BUG) \
    Bug[sb##BUG] = TAutoSwitch(2 - Storage->ReadInteger(L"Bug"#BUG, \
      2 - Bug[sb##BUG]));
  READ_BUG(HMAC2);
  READ_BUG(DeriveKey2);
  READ_BUG(RSAPad2);
  READ_BUG(PKSessID2);
  READ_BUG(Rekey2);
  READ_BUG(MaxPkt2);
  READ_BUG(Ignore2);
  READ_BUG(OldGex2);
  READ_BUG(WinAdj);
  READ_BUG(ChanReq);
  #undef READ_BUG

  if ((Bug[sbHMAC2] == asAuto) &&
      Storage->ReadBool(L"BuggyMAC", false))
  {
      Bug[sbHMAC2] = asOn;
  }

  if (!Unsafe)
  {
    SftpServer = Storage->ReadString(L"SftpServer", SftpServer);
  }
  #define READ_SFTP_BUG(BUG) \
    SFTPBug[sb##BUG] = Storage->ReadEnum(L"SFTP" #BUG "Bug", SFTPBug[sb##BUG], AutoSwitchMapping);
  READ_SFTP_BUG(Symlink);
  READ_SFTP_BUG(SignedTS);
  #undef READ_SFTP_BUG

  SFTPMaxVersion = Storage->ReadInteger(L"SFTPMaxVersion", SFTPMaxVersion);
  SFTPMaxPacketSize = Storage->ReadInteger(L"SFTPMaxPacketSize", SFTPMaxPacketSize);
  SFTPDownloadQueue = Storage->ReadInteger(L"SFTPDownloadQueue", SFTPDownloadQueue);
  SFTPUploadQueue = Storage->ReadInteger(L"SFTPUploadQueue", SFTPUploadQueue);
  SFTPListingQueue = Storage->ReadInteger(L"SFTPListingQueue", SFTPListingQueue);
  SFTPRealPath = Storage->ReadEnum(L"SFTPRealPath", SFTPRealPath, AutoSwitchMapping);
  UsePosixRename = Storage->ReadBool(L"UsePosixRename", UsePosixRename);

  Color = Storage->ReadInteger(L"Color", Color);

  PuttyProtocol = Storage->ReadString(L"Protocol", PuttyProtocol);

  Tunnel = Storage->ReadBool(L"Tunnel", Tunnel);
  TunnelPortNumber = Storage->ReadInteger(L"TunnelPortNumber", TunnelPortNumber);
  TunnelUserName = Storage->ReadString(L"TunnelUserName", TunnelUserName);
  // must be loaded after TunnelUserName,
  // because TunnelHostName may be in format user@host
  TunnelHostName = Storage->ReadString(L"TunnelHostName", TunnelHostName);
  if (LoadPasswords)
  {
    LOAD_PASSWORD(TunnelPassword, L"TunnelPasswordPlain");
  }
  TunnelPublicKeyFile = Storage->ReadString(L"TunnelPublicKeyFile", TunnelPublicKeyFile);
  // Contrary to main session passphrase (which has -passphrase switch in scripting),
  // we are loading tunnel passphrase, as there's no other way to provide it in scripting
  if (LoadPasswords)
  {
    LOAD_PASSWORD(TunnelPassphrase, L"TunnelPassphrasePlain");
  }
  TunnelLocalPortNumber = Storage->ReadInteger(L"TunnelLocalPortNumber", TunnelLocalPortNumber);
  TunnelHostKey = Storage->ReadString(L"TunnelHostKey", TunnelHostKey);

  // Ftp prefix
  FtpPasvMode = Storage->ReadBool(L"FtpPasvMode", FtpPasvMode);
  FtpForcePasvIp = Storage->ReadEnum(L"FtpForcePasvIp2", FtpForcePasvIp, AutoSwitchMapping);
  FtpUseMlsd = Storage->ReadEnum(L"FtpUseMlsd", FtpUseMlsd, AutoSwitchMapping);
  FtpAccount = Storage->ReadString(L"FtpAccount", FtpAccount);
  FtpPingInterval = Storage->ReadInteger(L"FtpPingInterval", FtpPingInterval);
  FtpPingType = static_cast<TFtpPingType>(Storage->ReadInteger(L"FtpPingType", FtpPingType));
  FtpTransferActiveImmediately = Storage->ReadEnum(L"FtpTransferActiveImmediately2", FtpTransferActiveImmediately, AutoSwitchMapping);
  Ftps = static_cast<TFtps>(Storage->ReadInteger(L"Ftps", Ftps));
  FtpListAll = Storage->ReadEnum(L"FtpListAll", FtpListAll, AutoSwitchMapping);
  FtpHost = Storage->ReadEnum(L"FtpHost", FtpHost, AutoSwitchMapping);
  FtpWorkFromCwd = Storage->ReadEnum(L"FtpWorkFromCwd", Storage->ReadEnum(L"FtpDeleteFromCwd", FtpWorkFromCwd), AutoSwitchMapping);
  FtpAnyCodeForPwd = Storage->ReadBool(L"FtpAnyCodeForPwd", FtpAnyCodeForPwd);
  SslSessionReuse = Storage->ReadBool(L"SslSessionReuse", SslSessionReuse);
  TlsCertificateFile = Storage->ReadString(L"TlsCertificateFile", TlsCertificateFile);

  FtpProxyLogonType = Storage->ReadInteger(L"FtpProxyLogonType", FtpProxyLogonType);

  MinTlsVersion = static_cast<TTlsVersion>(Storage->ReadInteger(L"MinTlsVersion", MinTlsVersion));
  MaxTlsVersion = static_cast<TTlsVersion>(Storage->ReadInteger(L"MaxTlsVersion", MaxTlsVersion));
  CompleteTlsShutdown = Storage->ReadEnum(L"CompleteTlsShutdown", CompleteTlsShutdown, AutoSwitchMapping);

  LOAD_PASSWORD(EncryptKey, L"EncryptKeyPlain");

  WebDavLiberalEscaping = Storage->ReadBool(L"WebDavLiberalEscaping", WebDavLiberalEscaping);
  WebDavAuthLegacy = Storage->ReadBool(L"WebDavAuthLegacy", WebDavAuthLegacy);

  IsWorkspace = Storage->ReadBool(L"IsWorkspace", IsWorkspace);
  Link = Storage->ReadString(L"Link", Link);
  NameOverride = Storage->ReadString(L"NameOverride", NameOverride);

  PuttySettings = Storage->ReadString(L"PuttySettings", PuttySettings);

  CustomParam1 = Storage->ReadString(L"CustomParam1", CustomParam1);
  CustomParam2 = Storage->ReadString(L"CustomParam2", CustomParam2);

  #undef LOAD_PASSWORD
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Load(THierarchicalStorage * Storage, bool PuttyImport)
{
  bool RewritePassword = false;
  if (Storage->OpenSubKey(InternalStorageKey, False))
  {
    // In case we are re-loading, reset passwords, to avoid pointless
    // re-cryption, while loading username/hostname. And moreover, when
    // the password is wrongly encrypted (using a different master password),
    // this breaks sites reload and consequently an overall operation,
    // such as opening Sites menu
    ClearSessionPasswords();
    FProxyPassword = L"";

    DoLoad(Storage, PuttyImport, RewritePassword, false, true);

    Storage->CloseSubKey();
  }

  if (RewritePassword)
  {
    TStorageAccessMode AccessMode = Storage->AccessMode;
    Storage->AccessMode = smReadWrite;

    try
    {
      if (Storage->OpenSubKey(InternalStorageKey, true))
      {
        #define REWRITE_PASSWORD(PROP, PLAIN_NAME) \
          Storage->DeleteValue(PLAIN_NAME); \
          if (!PROP.IsEmpty()) \
          { \
            Storage->WriteBinaryDataAsString(TEXT(#PROP), F##PROP); \
          }
        REWRITE_PASSWORD(Password, L"PasswordPlain");
        REWRITE_PASSWORD(TunnelPassword, L"TunnelPasswordPlain");
        REWRITE_PASSWORD(EncryptKey, L"EncryptKeyPlain");
        REWRITE_PASSWORD(TunnelPassphrase, L"TunnelPassphrasePlain");
        #undef REWRITE_PASSWORD
        Storage->CloseSubKey();
      }
    }
    catch(...)
    {
      // ignore errors (like read-only INI file)
    }

    Storage->AccessMode = AccessMode;
  }

  FModified = false;
  FSource = ssStored;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::DoSave(THierarchicalStorage * Storage,
  bool PuttyExport, const TSessionData * Default, bool DoNotEncryptPasswords)
{
  // Same as in TCopyParamType::Save
  #define WRITE_DATA_EX(TYPE, NAME, PROPERTY, CONV) \
    if ((Default != NULL) && (CONV(Default->PROPERTY) == CONV(PROPERTY))) \
    { \
      Storage->DeleteValue(NAME); \
    } \
    else \
    { \
      Storage->Write ## TYPE(NAME, CONV(PROPERTY)); \
    }
  #define WRITE_DATA_CONV(TYPE, NAME, PROPERTY) WRITE_DATA_EX(TYPE, NAME, PROPERTY, WRITE_DATA_CONV_FUNC)
  #define WRITE_DATA(TYPE, PROPERTY) WRITE_DATA_EX(TYPE, TEXT(#PROPERTY), PROPERTY, )

  WRITE_DATA(String, HostName);
  WRITE_DATA(Integer, PortNumber);
  if ((PingType == ptOff) && PuttyExport)
  {
    // Deleting would do too
    Storage->WriteInteger(L"PingInterval", 0);
    Storage->WriteInteger(L"PingIntervalSecs", 0);
  }
  else
  {
    WRITE_DATA_EX(Integer, L"PingInterval", PingInterval / SecsPerMin, );
    WRITE_DATA_EX(Integer, L"PingIntervalSecs", PingInterval % SecsPerMin, );
  }
  Storage->DeleteValue(L"PingIntervalSec"); // obsolete
  WRITE_DATA(Integer, PingType);
  WRITE_DATA(Integer, Timeout);
  WRITE_DATA(Bool, TryAgent);
  WRITE_DATA(Bool, AgentFwd);
  WRITE_DATA(Bool, AuthKI);
  WRITE_DATA(Bool, AuthKIPassword);
  WRITE_DATA_EX(String, L"SshHostKey", HostKey, );
  WRITE_DATA(String, Note);

  WRITE_DATA(Bool, AuthGSSAPI);
  WRITE_DATA(Bool, AuthGSSAPIKEX);
  WRITE_DATA(Bool, GSSAPIFwdTGT);
  Storage->DeleteValue(L"TryGSSKEX");
  Storage->DeleteValue(L"UserNameFromEnvironment");
  Storage->DeleteValue("GSSAPIServerChoosesUserName");
  Storage->DeleteValue(L"GSSAPITrustDNS");
  WRITE_DATA(String, LogicalHostName);
  if (PuttyExport)
  {
    // duplicate kerberos setting with keys of the vintela quest putty
    WRITE_DATA_EX(Bool, L"AuthSSPI", AuthGSSAPI, );
    WRITE_DATA_EX(Bool, L"SSPIFwdTGT", GSSAPIFwdTGT, );
    WRITE_DATA_EX(String, L"KerbPrincipal", LogicalHostName, );
    // duplicate kerberos setting with keys of the official putty
    WRITE_DATA_EX(Bool, L"GssapiFwd", GSSAPIFwdTGT, );
  }

  WRITE_DATA(Bool, ChangeUsername);
  WRITE_DATA(Bool, Compression);
  WRITE_DATA(Bool, Ssh2DES);
  WRITE_DATA(Bool, SshNoUserAuth);
  WRITE_DATA_EX(String, L"Cipher", CipherList, );
  WRITE_DATA_EX(String, L"KEX", KexList, );
  WRITE_DATA_EX(String, L"HostKey", HostKeyList, );
  WRITE_DATA_EX(String, L"GSSLibs", GssLibList, );
  WRITE_DATA_EX(String, L"GSSCustom", GssLibCustom, );
  WRITE_DATA(Integer, AddressFamily);
  WRITE_DATA_EX(String, L"RekeyBytes", RekeyData, );
  WRITE_DATA(Integer, RekeyTime);

  WRITE_DATA(Bool, TcpNoDelay);

  if (PuttyExport)
  {
    WRITE_DATA(StringRaw, UserName);
    // PuTTY is started in its binary directory to allow relative paths when opening PuTTY's own stored session.
    // To allow relative paths in our sessions, we have to expand them for PuTTY.
    WRITE_DATA_EX(StringRaw, L"PublicKeyFile", PublicKeyFile, ExpandFileName);
    WRITE_DATA_EX(StringRaw, L"DetachedCertificate", DetachedCertificate, ExpandFileName);
  }
  else
  {
    WRITE_DATA(String, UserName);
    WRITE_DATA(String, PublicKeyFile);
    WRITE_DATA(String, DetachedCertificate);
    WRITE_DATA(Integer, FSProtocol);
    WRITE_DATA(String, LocalDirectory);
    WRITE_DATA(String, OtherLocalDirectory);
    WRITE_DATA(String, RemoteDirectory);
    WRITE_DATA(Bool, SynchronizeBrowsing);
    WRITE_DATA(Bool, UpdateDirectories);
    WRITE_DATA(Bool, CacheDirectories);
    WRITE_DATA(Bool, CacheDirectoryChanges);
    WRITE_DATA(Bool, PreserveDirectoryChanges);

    WRITE_DATA(Bool, ResolveSymlinks);
    WRITE_DATA(Bool, FollowDirectorySymlinks);
    WRITE_DATA_EX(Integer, L"ConsiderDST", DSTMode, );
    // Special is never stored (if it would, login dialog must be modified not to
    // duplicate Special parameter when Special session is loaded and then stored
    // under different name)
    // WRITE_DATA(Bool, Special);
    WRITE_DATA(String, Shell);
    WRITE_DATA(Bool, ClearAliases);
    WRITE_DATA(Bool, UnsetNationalVars);
    WRITE_DATA(String, ListingCommand);
    WRITE_DATA(Bool, IgnoreLsWarnings);
    WRITE_DATA(Integer, SCPLsFullTime);
    WRITE_DATA(Bool, Scp1Compatibility);
    // TimeDifferenceAuto is valid for FTP protocol only.
    // For other protocols it's typically true (default value),
    // but ignored so TimeDifference is still taken into account (SCP only actually)
    if (TimeDifferenceAuto && (FSProtocol == fsFTP))
    {
      // Have to delete it as TimeDifferenceAuto is not saved when enabled,
      // but the default is derived from value of TimeDifference.
      Storage->DeleteValue(L"TimeDifference");
    }
    else
    {
      WRITE_DATA(Float, TimeDifference);
    }
    WRITE_DATA(Bool, TimeDifferenceAuto);
    WRITE_DATA(Bool, DeleteToRecycleBin);
    WRITE_DATA(Bool, OverwrittenToRecycleBin);
    WRITE_DATA(String, RecycleBinPath);
    WRITE_DATA(String, PostLoginCommands);

    WRITE_DATA(String, ReturnVar);
    WRITE_DATA(Bool, ExitCode1IsError);
    WRITE_DATA_EX(Integer, L"LookupUserGroups2", LookupUserGroups, );
    WRITE_DATA(Integer, EOLType);
    WRITE_DATA(Bool, TrimVMSVersions);
    WRITE_DATA(Bool, VMSAllRevisions);
    Storage->DeleteValue(L"SFTPUtfBug");
    WRITE_DATA_EX(Integer, L"Utf", NotUtf, );
    WRITE_DATA(Integer, InternalEditorEncoding);
    WRITE_DATA(String, S3DefaultRegion);
    WRITE_DATA(String, S3SessionToken);
    WRITE_DATA(String, S3RoleArn);
    WRITE_DATA(String, S3RoleSessionName);
    WRITE_DATA(String, S3Profile);
    WRITE_DATA(Integer, S3UrlStyle);
    WRITE_DATA(Integer, S3MaxKeys);
    WRITE_DATA(Bool, S3CredentialsEnv);
    WRITE_DATA(Bool, S3RequesterPays);
    WRITE_DATA(Integer, SendBuf);
    WRITE_DATA(String, SourceAddress);
    WRITE_DATA(String, ProtocolFeatures);
    WRITE_DATA(Bool, SshSimple);
  }

  WRITE_DATA(Integer, ProxyMethod);
  WRITE_DATA(String, ProxyHost);
  WRITE_DATA(Integer, ProxyPort);
  WRITE_DATA(String, ProxyUsername);
  if (ProxyMethod == pmCmd)
  {
    WRITE_DATA_EX(StringRaw, L"ProxyTelnetCommand", ProxyLocalCommand, );
  }
  else
  {
    WRITE_DATA(StringRaw, ProxyTelnetCommand);
  }
  #define WRITE_DATA_CONV_FUNC(X) (((X) + 2) % 3)
  WRITE_DATA_CONV(Integer, L"ProxyDNS", ProxyDNS);
  #undef WRITE_DATA_CONV_FUNC
  WRITE_DATA(Bool, ProxyLocalhost);

  #define WRITE_DATA_CONV_FUNC(X) (2 - (X))
  #define WRITE_BUG(BUG) WRITE_DATA_CONV(Integer, L"Bug" #BUG, Bug[sb##BUG]);
  WRITE_BUG(HMAC2);
  WRITE_BUG(DeriveKey2);
  WRITE_BUG(RSAPad2);
  WRITE_BUG(PKSessID2);
  WRITE_BUG(Rekey2);
  WRITE_BUG(MaxPkt2);
  WRITE_BUG(Ignore2);
  WRITE_BUG(OldGex2);
  WRITE_BUG(WinAdj);
  WRITE_BUG(ChanReq);
  #undef WRITE_BUG
  #undef WRITE_DATA_CONV_FUNC

  Storage->DeleteValue(L"BuggyMAC");
  Storage->DeleteValue(L"AliasGroupList");

  if (PuttyExport)
  {
    WRITE_DATA_EX(String, L"Protocol", GetNormalizedPuttyProtocol(), );
    WRITE_DATA(String, WinTitle);
  }

  if (!PuttyExport)
  {
    WRITE_DATA(String, SftpServer);

    #define WRITE_SFTP_BUG(BUG) WRITE_DATA_EX(Integer, L"SFTP" #BUG "Bug", SFTPBug[sb##BUG], );
    WRITE_SFTP_BUG(Symlink);
    WRITE_SFTP_BUG(SignedTS);
    #undef WRITE_SFTP_BUG

    WRITE_DATA(Integer, SFTPMaxVersion);
    WRITE_DATA(Integer, SFTPMaxPacketSize);
    WRITE_DATA(Integer, SFTPDownloadQueue);
    WRITE_DATA(Integer, SFTPUploadQueue);
    WRITE_DATA(Integer, SFTPListingQueue);
    WRITE_DATA(Integer, SFTPRealPath);
    WRITE_DATA(Bool, UsePosixRename);

    WRITE_DATA(Integer, Color);

    WRITE_DATA(Bool, Tunnel);
    WRITE_DATA(String, TunnelHostName);
    WRITE_DATA(Integer, TunnelPortNumber);
    WRITE_DATA(String, TunnelUserName);
    WRITE_DATA(String, TunnelPublicKeyFile);
    WRITE_DATA(Integer, TunnelLocalPortNumber);
    WRITE_DATA(String, TunnelHostKey);

    WRITE_DATA(Bool, FtpPasvMode);
    WRITE_DATA_EX(Integer, L"FtpForcePasvIp2", FtpForcePasvIp, );
    WRITE_DATA(Integer, FtpUseMlsd);
    WRITE_DATA(String, FtpAccount);
    WRITE_DATA(Integer, FtpPingInterval);
    WRITE_DATA(Integer, FtpPingType);
    WRITE_DATA_EX(Integer, L"FtpTransferActiveImmediately2", FtpTransferActiveImmediately, );
    WRITE_DATA(Integer, Ftps);
    WRITE_DATA(Integer, FtpListAll);
    WRITE_DATA(Integer, FtpHost);
    WRITE_DATA(Integer, FtpWorkFromCwd);
    WRITE_DATA(Bool, FtpAnyCodeForPwd);
    WRITE_DATA(Bool, SslSessionReuse);
    WRITE_DATA(String, TlsCertificateFile);

    WRITE_DATA(Integer, FtpProxyLogonType);

    WRITE_DATA(Integer, MinTlsVersion);
    WRITE_DATA(Integer, MaxTlsVersion);
    WRITE_DATA(Integer, CompleteTlsShutdown);

    WRITE_DATA(Bool, WebDavLiberalEscaping);
    WRITE_DATA(Bool, WebDavAuthLegacy);

    WRITE_DATA(Bool, IsWorkspace);
    WRITE_DATA(String, Link);
    WRITE_DATA(String, NameOverride);

    WRITE_DATA(String, PuttySettings);

    WRITE_DATA(String, CustomParam1);
    WRITE_DATA(String, CustomParam2);
  }

  // This is for collecting all keys for TSiteRawDialog::AddButtonClick.
  // It should be enough to test for (Default == NULL),
  // the DoNotEncryptPasswords and PuttyExport were added to limit a possible unintended impact.
  bool SaveAll = (Default == NULL) && DoNotEncryptPasswords && !PuttyExport;

  SavePasswords(Storage, PuttyExport, DoNotEncryptPasswords, SaveAll);

  if (PuttyExport)
  {
    WritePuttySettings(Storage, PuttySettings);
  }
}
//---------------------------------------------------------------------
TStrings * __fastcall TSessionData::SaveToOptions(const TSessionData * Default, bool SaveName, bool PuttyExport)
{
  std::unique_ptr<TStringList> Options(new TStringList());
  std::unique_ptr<TOptionsStorage> OptionsStorage(new TOptionsStorage(Options.get(), true));
  if (SaveName)
  {
    OptionsStorage->WriteString(L"Name", Name);
  }
  DoSave(OptionsStorage.get(), PuttyExport, Default, true);
  return Options.release();
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Save(THierarchicalStorage * Storage,
  bool PuttyExport, const TSessionData * Default)
{
  if (Storage->OpenSubKey(InternalStorageKey, true))
  {
    DoSave(Storage, PuttyExport, Default, false);

    Storage->CloseSubKey();
  }
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::ReadXmlNode(_di_IXMLNode Node, const UnicodeString & Name, const UnicodeString & Default)
{
  _di_IXMLNode TheNode = Node->ChildNodes->FindNode(Name);
  UnicodeString Result;
  if (TheNode != NULL)
  {
    Result = TheNode->Text.Trim();
  }

  if (Result.IsEmpty())
  {
    Result = Default;
  }

  return Result;
}
//---------------------------------------------------------------------
int __fastcall TSessionData::ReadXmlNode(_di_IXMLNode Node, const UnicodeString & Name, int Default)
{
  _di_IXMLNode TheNode = Node->ChildNodes->FindNode(Name);
  int Result;
  if (TheNode != NULL)
  {
    Result = StrToIntDef(TheNode->Text.Trim(), Default);
  }
  else
  {
    Result = Default;
  }

  return Result;
}
//---------------------------------------------------------------------
_di_IXMLNode __fastcall TSessionData::FindSettingsNode(_di_IXMLNode Node, const UnicodeString & Name)
{
  for (int Index = 0; Index < Node->ChildNodes->Count; Index++)
  {
    _di_IXMLNode ChildNode = Node->ChildNodes->Get(Index);
    if (ChildNode->NodeName == L"Setting")
    {
       OleVariant SettingName = ChildNode->GetAttribute(L"name");
       if (SettingName == Name)
       {
         return ChildNode;
       }
    }
  }

  return NULL;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::ReadSettingsNode(_di_IXMLNode Node, const UnicodeString & Name, const UnicodeString & Default)
{
  _di_IXMLNode TheNode = FindSettingsNode(Node, Name);
  UnicodeString Result;
  if (TheNode != NULL)
  {
    Result = TheNode->Text.Trim();
  }

  if (Result.IsEmpty())
  {
    Result = Default;
  }

  return Result;
}
//---------------------------------------------------------------------
int __fastcall TSessionData::ReadSettingsNode(_di_IXMLNode Node, const UnicodeString & Name, int Default)
{
  _di_IXMLNode TheNode = FindSettingsNode(Node, Name);
  int Result;
  if (TheNode != NULL)
  {
    Result = StrToIntDef(TheNode->Text.Trim(), Default);
  }
  else
  {
    Result = Default;
  }

  return Result;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::ImportFromFilezilla(
  _di_IXMLNode Node, const UnicodeString & Path, _di_IXMLNode SettingsNode)
{
  Name = UnixIncludeTrailingBackslash(Path) + MakeValidName(ReadXmlNode(Node, L"Name", Name));
  HostName = ReadXmlNode(Node, L"Host", HostName);
  PortNumber = ReadXmlNode(Node, L"Port", PortNumber);

  int AProtocol = ReadXmlNode(Node, L"Protocol", 0);
  // ServerProtocol enum
  switch (AProtocol)
  {
    case 0: // FTP
    default: // UNKNOWN, HTTP, HTTPS, INSECURE_FTP
      FSProtocol = fsFTP;
      break;

    case 1: // SFTP
      FSProtocol = fsSFTP;
      break;

    case 3: // FTPS
      FSProtocol = fsFTP;
      Ftps = ftpsImplicit;
      break;

    case 4: // FTPES
      FSProtocol = fsFTP;
      Ftps = ftpsExplicitTls;
      break;
  }

  // LogonType enum
  int LogonType = ReadXmlNode(Node, L"Logontype", 0);
  if (LogonType == 0) // ANONYMOUS
  {
    UserName = AnonymousUserName;
    Password = AnonymousPassword;
  }
  else
  {
    UserName = ReadXmlNode(Node, L"User", UserName);
    FtpAccount = ReadXmlNode(Node, L"Account", FtpAccount);

    _di_IXMLNode PassNode = Node->ChildNodes->FindNode(L"Pass");
    if (PassNode != NULL)
    {
      UnicodeString APassword = PassNode->Text.Trim();
      OleVariant EncodingValue = PassNode->GetAttribute(L"encoding");
      if (!EncodingValue.IsNull())
      {
        UnicodeString EncodingValueStr = EncodingValue;
        if (SameText(EncodingValueStr, L"base64"))
        {
          TBytes Bytes = DecodeBase64(APassword);
          APassword = TEncoding::UTF8->GetString(Bytes);
        }
      }
      Password = APassword;
    }
  }

  PublicKeyFile = ReadXmlNode(Node, L"Keyfile", PublicKeyFile);

  int DefaultTimeDifference = TimeToSeconds(TimeDifference);
  TimeDifference =
    (double(ReadXmlNode(Node, L"TimezoneOffset", DefaultTimeDifference) / SecsPerDay));
  TimeDifferenceAuto = (TimeDifference == TDateTime());

  UnicodeString PasvMode = ReadXmlNode(Node, L"PasvMode", L"");
  if (SameText(PasvMode, L"MODE_PASSIVE"))
  {
    FtpPasvMode = true;
  }
  else if (SameText(PasvMode, L"MODE_ACTIVE"))
  {
    FtpPasvMode = false;
  }
  else if (SettingsNode != NULL)
  {
    int PasvMode = ReadSettingsNode(SettingsNode, L"Use Pasv mode", 1);
    FtpPasvMode = (PasvMode != 0);
  }

  UnicodeString EncodingType = ReadXmlNode(Node, L"EncodingType", L"");
  if (SameText(EncodingType, L"Auto"))
  {
    NotUtf = asAuto;
  }
  else if (SameText(EncodingType, L"UTF-8"))
  {
    NotUtf = asOff;
  }

  // todo PostLoginCommands

  Note = ReadXmlNode(Node, L"Comments", Note);

  LocalDirectory = ReadXmlNode(Node, L"LocalDir", LocalDirectory);

  UnicodeString RemoteDir = ReadXmlNode(Node, L"RemoteDir", L"");
  if (!RemoteDir.IsEmpty())
  {
    CutToChar(RemoteDir, L' ', false); // type
    int PrefixSize = StrToIntDef(CutToChar(RemoteDir, L' ', false), 0); // prefix size
    if (PrefixSize > 0)
    {
      RemoteDir.Delete(1, PrefixSize);
    }
    RemoteDirectory = L"/";
    while (!RemoteDir.IsEmpty())
    {
      int SegmentSize = StrToIntDef(CutToChar(RemoteDir, L' ', false), 0);
      UnicodeString Segment = RemoteDir.SubString(1, SegmentSize);
      RemoteDirectory = UnixIncludeTrailingBackslash(RemoteDirectory) + Segment;
      RemoteDir.Delete(1, SegmentSize + 1);
    }
  }

  SynchronizeBrowsing = (ReadXmlNode(Node, L"SyncBrowsing", SynchronizeBrowsing ? 1 : 0) != 0);

  if (SettingsNode != NULL)
  {
    if (UsesSsh && PublicKeyFile.IsEmpty())
    {
      UnicodeString KeyFiles = ReadSettingsNode(SettingsNode, L"SFTP keyfiles", UnicodeString());
      UnicodeString KeyFile = CutToChar(KeyFiles, L'\n', true).Trim();
      KeyFiles = KeyFiles.Trim();
      // If there are more keys, ignore them, as we do not know which one to use
      if (!KeyFile.IsEmpty() && KeyFiles.IsEmpty())
      {
        PublicKeyFile = KeyFile;
      }
    }

    bool BypassProxy = (ReadXmlNode(Node, L"BypassProxy", 0) != 0);
    if (!BypassProxy)
    {
      int FtpProxyType = ReadSettingsNode(SettingsNode, L"FTP Proxy type", -1);
      if (FtpProxyType > 0)
      {
        switch (FtpProxyType)
        {
          case 1:
            FtpProxyLogonType = 2;
            break;
          case 2:
            FtpProxyLogonType = 1;
            break;
          case 3:
            FtpProxyLogonType = 3;
            break;
          case 4:
            // custom
            // TODO: map known sequences to our enumeration
            FtpProxyLogonType = 0;
            break;
          default:
            DebugFail();
            FtpProxyLogonType = 0;
            break;
        }

        ProxyHost = ReadSettingsNode(SettingsNode, L"FTP Proxy host", ProxyHost);
        ProxyUsername = ReadSettingsNode(SettingsNode, L"FTP Proxy user", ProxyUsername);
        ProxyPassword = ReadSettingsNode(SettingsNode, L"FTP Proxy password", ProxyPassword);
        // ProxyPort is not used with FtpProxyLogonType
      }
      else
      {
        int ProxyType = ReadSettingsNode(SettingsNode, L"Proxy type", -1);
        if (ProxyType >= 0)
        {
          switch (ProxyType)
          {
            case 0:
              ProxyMethod = ::pmNone;
              break;

            case 1:
              ProxyMethod = pmHTTP;
              break;

            case 2:
              ProxyMethod = pmSocks5;
              break;

            case 3:
              ProxyMethod = pmSocks4;
              break;

            default:
              DebugFail();
              ProxyMethod = ::pmNone;
              break;
          }

          ProxyHost = ReadSettingsNode(SettingsNode, L"Proxy host", ProxyHost);
          ProxyPort = ReadSettingsNode(SettingsNode, L"Proxy port", ProxyPort);
          ProxyUsername = ReadSettingsNode(SettingsNode, L"Proxy user", ProxyUsername);
          ProxyPassword = ReadSettingsNode(SettingsNode, L"Proxy password", ProxyPassword);
        }
      }
    }
  }

}
//---------------------------------------------------------------------
bool OpensshBoolValue(const UnicodeString & Value)
{
  return SameText(Value, L"yes");
}
//---------------------------------------------------------------------
UnicodeString CutOpensshToken(UnicodeString & S)
{
  const wchar_t NoQuote = L'\0';
  wchar_t Quote = NoQuote;
  UnicodeString Result;
  int P = 1;
  while (P <= S.Length())
  {
    wchar_t C = S[P];
    if ((C == L'\\') &&
        (P < S.Length()) &&
        ((S[P + 1] == L'\'') ||
         (S[P + 1] == L'\"') ||
         (S[P + 1] == L'\\') ||
         ((Quote == NoQuote) && S[P + 1] == L' ')))
    {
      Result += S[P + 1];
      P++;
    }
    else if ((Quote == NoQuote) && ((C == L' ') || (C == L'\t')))
    {
      break;
    }
    else if ((Quote == NoQuote) && ((C == L'\'') || (C == L'"')))
    {
      Quote = C;
    }
    else if ((Quote != NoQuote) && (Quote == C))
    {
      Quote = NoQuote;
    }
    else
    {
      Result += C;
    }
    P++;
  }

  S = MidStr(S, P).Trim();

  return Result;
}
//---------------------------------------------------------------------
UnicodeString ConvertPathFromOpenssh(const UnicodeString & Path)
{
  // It's likely there would be forward slashes in OpenSSH config file and our load/save dialogs
  // (e.g. when converting keys) work suboptimally when working with forward slashes.
  UnicodeString Result = GetNormalizedPath(Path);
  const UnicodeString HomePathPrefix = L"~";
  if (StartsStr(HomePathPrefix, Result))
  {
    Result =
      GetShellFolderPath(CSIDL_PROFILE) +
      Result.SubString(HomePathPrefix.Length() + 1, Result.Length() - HomePathPrefix.Length());
  }
  return Result;
}
//---------------------------------------------------------------------
void TSessionData::ImportFromOpenssh(TStrings * Lines)
{
  bool SkippingSection = false;
  std::unique_ptr<TStrings> UsedDirectives(CreateSortedStringList());
  for (int Index = 0; Index < Lines->Count; Index++)
  {
    UnicodeString Line = Lines->Strings[Index];
    UnicodeString Directive, Args;
    if (ParseOpensshDirective(Line, Directive, Args))
    {
      if (SameText(Directive, OpensshHostDirective))
      {
        SkippingSection = true;
        while (!Args.IsEmpty())
        {
          UnicodeString M = CutOpensshToken(Args);
          bool Negated = DebugAlwaysTrue(!M.IsEmpty()) && (M[1] == L'!');
          if (Negated)
          {
            M.Delete(1, 1);
          }
          TFileMasks Mask;
          Mask.SetMask(M);
          // This does way more than OpenSSH, but on the other hand, the special characters of our file masks,
          // should not be present in hostnames.
          if (Mask.MatchesFileName(Name))
          {
            if (Negated)
            {
              // Skip even if matched by other positive patterns
              SkippingSection = true;
              break;
            }
            else
            {
              // Keep looking, in case if negated
              SkippingSection = false;
            }
          }
        }
      }
      else if (SameText(Directive, L"Match"))
      {
        SkippingSection = true;
      }
      else if (!SkippingSection && (UsedDirectives->IndexOf(Directive) < 0))
      {
        UnicodeString Value = CutOpensshToken(Args);
        // All the directives we support allow only one token
        if (Args.IsEmpty())
        {
          if (SameText(Directive, L"AddressFamily"))
          {
            if (SameText(Value, L"inet"))
            {
              AddressFamily = afIPv4;
            }
            else if (SameText(Value, L"inet6"))
            {
              AddressFamily = afIPv6;
            }
            else
            {
              AddressFamily = afAuto;
            }
          }
          else if (SameText(Directive, L"BindAddress"))
          {
            SourceAddress = Value;
          }
          else if (SameText(Directive, L"Compression"))
          {
            Compression = OpensshBoolValue(Value);
          }
          else if (SameText(Directive, L"ForwardAgent"))
          {
            AgentFwd = OpensshBoolValue(Value);
          }
          else if (SameText(Directive, L"GSSAPIAuthentication"))
          {
            AuthGSSAPI = OpensshBoolValue(Value);
          }
          else if (SameText(Directive, L"GSSAPIDelegateCredentials"))
          {
            AuthGSSAPIKEX = OpensshBoolValue(Value);
          }
          else if (SameText(Directive, L"Hostname"))
          {
            HostName = Value;
          }
          else if (SameText(Directive, L"IdentityFile"))
          {
            PublicKeyFile = ConvertPathFromOpenssh(Value);
          }
          else if (SameText(Directive, L"CertificateFile"))
          {
            DetachedCertificate = ConvertPathFromOpenssh(Value);
          }
          else if (SameText(Directive, L"KbdInteractiveAuthentication"))
          {
            AuthKI = OpensshBoolValue(Value);
          }
          else if (SameText(Directive, L"Port"))
          {
            PortNumber = StrToInt(Value);
          }
          else if (SameText(Directive, L"User"))
          {
            UserName = Value;
          }
          else if (SameText(Directive, L"ProxyJump"))
          {
            UnicodeString Jump = Value;
            // multiple jumps are not supported
            if (Jump.Pos(L",") == 0)
            {
              std::unique_ptr<TSessionData> JumpData(new TSessionData(EmptyStr));
              bool DefaultsOnly;
              if ((JumpData->ParseUrl(Jump, NULL, NULL, DefaultsOnly, NULL, NULL, NULL, 0)) &&
                  !JumpData->HostName.IsEmpty())
              {
                JumpData->Name = JumpData->HostName;
                JumpData->ImportFromOpenssh(Lines);

                Tunnel = true;
                TunnelHostName = JumpData->HostName;
                TunnelPortNumber = JumpData->PortNumber;
                TunnelUserName = JumpData->UserName;
                TunnelPassword = JumpData->Password;
                TunnelPublicKeyFile = JumpData->PublicKeyFile;
              }
            }
          }
          UsedDirectives->Add(Directive);
        }
      }
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SavePasswords(THierarchicalStorage * Storage, bool PuttyExport, bool DoNotEncryptPasswords, bool SaveAll)
{
  // It's probably safe to replace this with if (!PuttyExport) { SAVE_PASSWORD(...) }
  if (!Configuration->DisablePasswordStoring && !PuttyExport && (!FPassword.IsEmpty() || SaveAll))
  {
    if (DoNotEncryptPasswords)
    {
      Storage->WriteString(L"PasswordPlain", Password);
      Storage->DeleteValue(L"Password");
    }
    else
    {
      Storage->WriteBinaryDataAsString(L"Password", StronglyRecryptPassword(FPassword, UserName+HostName));
      Storage->DeleteValue(L"PasswordPlain");
    }
  }
  else
  {
    Storage->DeleteValue(L"Password");
    Storage->DeleteValue(L"PasswordPlain");
  }

  if (PuttyExport)
  {
    // save password unencrypted
    Storage->WriteString(L"ProxyPassword", ProxyPassword);
  }
  else
  {
    #define SAVE_PASSWORD_EX(PROP, PLAIN_NAME, ENC_NAME, ENC_KEY, COND) \
      if (DoNotEncryptPasswords) \
      { \
        if (!F##PROP.IsEmpty() || SaveAll) \
        { \
          Storage->WriteString(PLAIN_NAME, PROP); \
        } \
        else \
        { \
          Storage->DeleteValue(PLAIN_NAME); \
        } \
        Storage->DeleteValue(ENC_NAME); \
      } \
      else \
      { \
        if (COND && (!F##PROP.IsEmpty() || SaveAll)) \
        { \
          Storage->WriteBinaryDataAsString(ENC_NAME, StronglyRecryptPassword(F##PROP, ENC_KEY)); \
        } \
        else \
        { \
          Storage->DeleteValue(ENC_NAME); \
        } \
        Storage->DeleteValue(PLAIN_NAME); \
      }
    #define SAVE_PASSWORD(PROP, PLAIN_NAME, ENC_KEY) SAVE_PASSWORD_EX(PROP, PLAIN_NAME, TEXT(#PROP), ENC_KEY, !Configuration->DisablePasswordStoring)

    SAVE_PASSWORD_EX(ProxyPassword, L"ProxyPassword", L"ProxyPasswordEnc", ProxyUsername + ProxyHost, true);
    SAVE_PASSWORD(TunnelPassword, L"TunnelPasswordPlain", TunnelUserName + TunnelHostName);
    SAVE_PASSWORD_EX(EncryptKey, L"EncryptKeyPlain", L"EncryptKey", UserName + HostName, true);
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::RecryptPasswords()
{
  Password = Password;
  NewPassword = NewPassword;
  ProxyPassword = ProxyPassword;
  TunnelPassword = TunnelPassword;
  TunnelPassphrase = TunnelPassphrase;
  Passphrase = Passphrase;
  EncryptKey = EncryptKey;
}
//---------------------------------------------------------------------
// Caching read password files, particularly when the file is actually a named pipe
// that does not support repeated reading.
static std::unique_ptr<TCriticalSection> PasswordFilesCacheSection(TraceInitPtr(new TCriticalSection()));
typedef std::map<UnicodeString, UnicodeString> TPasswordFilesCache;
static TPasswordFilesCache PasswordFilesCache;
//---------------------------------------------------------------------
static UnicodeString ReadPasswordFromFile(const UnicodeString & FileName)
{
  UnicodeString Result;
  if (!FileName.IsEmpty())
  {
    TGuard Guard(PasswordFilesCacheSection.get());
    TPasswordFilesCache::const_iterator I = PasswordFilesCache.find(FileName);
    if (I != PasswordFilesCache.end())
    {
      Result = I->second;
    }
    else
    {
      std::unique_ptr<TStrings> Lines(new TStringList());
      LoadScriptFromFile(FileName, Lines.get());
      if (Lines->Count > 0)
      {
        Result = Lines->Strings[0];
      }
      PasswordFilesCache[FileName] = Result;
    }
  }
  return Result;
}
//---------------------------------------------------------------------
void TSessionData::ReadPasswordsFromFiles()
{
  Password = ReadPasswordFromFile(Password);
  NewPassword = ReadPasswordFromFile(NewPassword);
  ProxyPassword = ReadPasswordFromFile(ProxyPassword);
  TunnelPassword = ReadPasswordFromFile(TunnelPassword);
  TunnelPassphrase = ReadPasswordFromFile(TunnelPassphrase);
  Passphrase = ReadPasswordFromFile(Passphrase);
  EncryptKey = ReadPasswordFromFile(EncryptKey);
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::HasPassword()
{
  return !FPassword.IsEmpty();
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::HasAnySessionPassword()
{
  // Keep in sync with ClearSessionPasswords
  return
    HasPassword() ||
    !FTunnelPassword.IsEmpty() ||
    // will probably be never used
    !FNewPassword.IsEmpty();
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::HasAnyPassword()
{
  // Keep in sync with MaskPasswords
  return
    HasAnySessionPassword() ||
    !FProxyPassword.IsEmpty() ||
    !FEncryptKey.IsEmpty() ||
    !FPassphrase.IsEmpty() ||
    !FTunnelPassphrase.IsEmpty();
}
//---------------------------------------------------------------------
void __fastcall TSessionData::ClearSessionPasswords()
{
  // Keep in sync with HasAnySessionPassword
  FPassword = L"";
  FNewPassword = L"";
  FTunnelPassword = L"";
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Modify()
{
  FModified = true;
  if (FSource == ssStored)
  {
    FSource = ssStoredModified;
  }
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetSourceName()
{
  switch (FSource)
  {
    case ::ssNone:
      return L"Ad-Hoc site";

    case ssStored:
      return L"Site";

    case ssStoredModified:
      return L"Modified site";

    default:
      DebugFail();
      return L"";
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SaveRecryptedPasswords(THierarchicalStorage * Storage)
{
  if (Storage->OpenSubKey(InternalStorageKey, true))
  {
    try
    {
      RecryptPasswords();

      SavePasswords(Storage, false, false, false);
    }
    __finally
    {
      Storage->CloseSubKey();
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Remove(THierarchicalStorage * Storage, const UnicodeString & Name)
{
  Storage->RecursiveDeleteSubKey(Name);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Remove()
{
  bool SessionList = true;
  THierarchicalStorage * Storage = Configuration->CreateScpStorage(SessionList);
  try
  {
    Storage->Explicit = true;
    if (Storage->OpenSubKey(Configuration->StoredSessionsSubKey, false))
    {
      Remove(Storage, InternalStorageKey);
    }
  }
  __finally
  {
    delete Storage;
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::CacheHostKeyIfNotCached()
{
  UnicodeString KeyType = KeyTypeFromFingerprint(HostKey);

  // Should allow importing to INI file as ImportHostKeys
  UnicodeString TargetKey = Configuration->RegistryStorageKey + L"\\" + Configuration->SshHostKeysSubKey;
  std::unique_ptr<TRegistryStorage> Storage(new TRegistryStorage(TargetKey));
  Storage->AccessMode = smReadWrite;
  if (Storage->OpenRootKey(true))
  {
    UnicodeString HostKeyName = PuttyMungeStr(FORMAT(L"%s@%d:%s", (KeyType, PortNumber, HostName)));
    if (!Storage->ValueExists(HostKeyName))
    {
      // fingerprint is a checksum of a host key, so it cannot be translated back to host key,
      // so we store fingerprint and TSecureShell::VerifyHostKey was
      // modified to accept also fingerprint
      Storage->WriteString(HostKeyName, HostKey);
    }
  }
}
//---------------------------------------------------------------------
inline void __fastcall MoveStr(UnicodeString & Source, UnicodeString * Dest, int Count)
{
  if (Dest != NULL)
  {
    (*Dest) += Source.SubString(1, Count);
  }

  Source.Delete(1, Count);
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::DoIsProtocolUrl(
  const UnicodeString & Url, const UnicodeString & Protocol, int & ProtocolLen)
{
  bool Result = SameText(Url.SubString(1, Protocol.Length() + 1), Protocol + L":");
  if (Result)
  {
    ProtocolLen = Protocol.Length() + 1;
  }
  return Result;
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::IsProtocolUrl(
  const UnicodeString & Url, const UnicodeString & Protocol, int & ProtocolLen)
{
  return
    DoIsProtocolUrl(Url, Protocol, ProtocolLen) ||
    DoIsProtocolUrl(Url, WinSCPProtocolPrefix + Protocol, ProtocolLen);
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::IsSensitiveOption(const UnicodeString & Option, const UnicodeString & Value)
{
  bool Result;
  if (SameText(Option, PassphraseOption) ||
      SameText(Option, PASSWORD_SWITCH) ||
      SameText(Option, NEWPASSWORD_SWITCH))
  {
    Result = true;
  }
  else if (SameText(Option, PRIVATEKEY_SWITCH))
  {
    Filename * AFilename = filename_from_utf8(UTF8String(Value).c_str());
    Result = (in_memory_key_data(AFilename) != NULL);
    filename_free(AFilename);
  }
  else
  {
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::IsOptionWithParameters(const UnicodeString & Option)
{
  return SameText(Option, RawSettingsOption);
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::MaskPasswordInOptionParameter(const UnicodeString & Option, UnicodeString & Param)
{
  bool Result = false;
  if (SameText(Option, RawSettingsOption))
  {
    int P = Param.Pos(L"=");
    if (P > 0)
    {
      // TStrings.IndexOfName does not trim
      UnicodeString Key = Param.SubString(1, P - 1);

      if (SameText(Key, L"ProxyPassword") ||
          SameText(Key, L"ProxyPasswordEnc") ||
          SameText(Key, L"TunnelPassword") ||
          SameText(Key, L"TunnelPasswordPlain") ||
          SameText(Key, L"TunnelPassphrase") ||
          SameText(Key, L"TunnelPassphrasePlain") ||
          SameText(Key, L"EncryptKey") ||
          SameText(Key, L"EncryptKeyPlain"))
      {
        Param = Key + L"=" + PasswordMask;
        Result = true;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::MaskPasswords()
{
  // Keep in sync with HasAnyPassword
  if (!Password.IsEmpty())
  {
    Password = PasswordMask;
  }
  if (!NewPassword.IsEmpty())
  {
    NewPassword = PasswordMask;
  }
  if (!ProxyPassword.IsEmpty())
  {
    ProxyPassword = PasswordMask;
  }
  if (!TunnelPassword.IsEmpty())
  {
    TunnelPassword = PasswordMask;
  }
  if (!TunnelPassphrase.IsEmpty())
  {
    TunnelPassphrase = PasswordMask;
  }
  if (!EncryptKey.IsEmpty())
  {
    EncryptKey = PasswordMask;
  }
  if (!Passphrase.IsEmpty())
  {
    Passphrase = PasswordMask;
  }
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::ParseUrl(UnicodeString Url, TOptions * Options,
  TStoredSessionList * StoredSessions, bool & DefaultsOnly, UnicodeString * FileName,
  bool * AProtocolDefined, UnicodeString * MaskedUrl, int Flags)
{
  bool ProtocolDefined = true;
  bool PortNumberDefined = false;
  TFSProtocol AFSProtocol = TFSProtocol(); // shut up
  int DefaultProtocolPortNumber = 0; // shut up
  TFtps AFtps = ftpsNone;
  int ProtocolLen = 0;
  bool HttpForWebdav = FLAGCLEAR(Flags, pufPreferProtocol) || (FSProtocol != fsS3);
  if (IsProtocolUrl(Url, ScpProtocol, ProtocolLen))
  {
    AFSProtocol = fsSCPonly;
    DefaultProtocolPortNumber = SshPortNumber;
  }
  else if (IsProtocolUrl(Url, SftpProtocol, ProtocolLen))
  {
    AFSProtocol = fsSFTPonly;
    DefaultProtocolPortNumber = SshPortNumber;
  }
  else if (IsProtocolUrl(Url, FtpProtocol, ProtocolLen))
  {
    AFSProtocol = fsFTP;
    Ftps = ftpsNone;
    DefaultProtocolPortNumber = FtpPortNumber;
  }
  else if (IsProtocolUrl(Url, FtpsProtocol, ProtocolLen))
  {
    AFSProtocol = fsFTP;
    AFtps = ftpsImplicit;
    DefaultProtocolPortNumber = FtpsImplicitPortNumber;
  }
  else if (IsProtocolUrl(Url, FtpesProtocol, ProtocolLen))
  {
    AFSProtocol = fsFTP;
    AFtps = ftpsExplicitTls;
    DefaultProtocolPortNumber = FtpPortNumber;
  }
  else if (IsProtocolUrl(Url, WebDAVProtocol, ProtocolLen) ||
           (HttpForWebdav && IsProtocolUrl(Url, HttpProtocol, ProtocolLen)))
  {
    AFSProtocol = fsWebDAV;
    AFtps = ftpsNone;
    DefaultProtocolPortNumber = HTTPPortNumber;
  }
  else if (IsProtocolUrl(Url, WebDAVSProtocol, ProtocolLen) ||
           (HttpForWebdav && IsProtocolUrl(Url, HttpsProtocol, ProtocolLen)))
  {
    AFSProtocol = fsWebDAV;
    AFtps = ftpsImplicit;
    DefaultProtocolPortNumber = HTTPSPortNumber;
  }
  else if (IsProtocolUrl(Url, S3PlainProtocol, ProtocolLen) ||
           IsProtocolUrl(Url, HttpProtocol, ProtocolLen))
  {
    AFSProtocol = fsS3;
    AFtps = ftpsNone;
    DefaultProtocolPortNumber = HTTPPortNumber;
  }
  else if (IsProtocolUrl(Url, S3Protocol, ProtocolLen) ||
           IsProtocolUrl(Url, HttpsProtocol, ProtocolLen))
  {
    AFSProtocol = fsS3;
    AFtps = ftpsImplicit;
    DefaultProtocolPortNumber = HTTPSPortNumber;
  }
  else if (IsProtocolUrl(Url, SshProtocol, ProtocolLen))
  {
    // For most uses, handling ssh:// the same way as sftp://
    // The only place where a difference is made is GetLoginData() in WinMain.cpp
    AFSProtocol = fsSFTPonly;
    PuttyProtocol = PuttySshProtocol;
    DefaultProtocolPortNumber = SshPortNumber;
  }
  else
  {
    ProtocolDefined = false;
  }

  if (ProtocolDefined)
  {
    MoveStr(Url, MaskedUrl, ProtocolLen);
  }

  if (ProtocolDefined && (Url.SubString(1, 2) == L"//"))
  {
    MoveStr(Url, MaskedUrl, 2);
  }

  if (AProtocolDefined != NULL)
  {
    *AProtocolDefined = ProtocolDefined;
  }

  bool Unsafe = FLAGSET(Flags, pufUnsafe);
  bool ParseOnly = FLAGSET(Flags, pufParseOnly);
  if (!Url.IsEmpty())
  {
    UnicodeString DecodedUrl = DecodeUrlChars(Url);
    // lookup stored session even if protocol was defined
    // (this allows setting for example default username for host
    // by creating stored session named by host)
    TSessionData * Data = NULL;
    // When using to paste URL on Login dialog, we do not want to lookup the stored sites
    if ((StoredSessions != NULL) &&
        (!ProtocolDefined || FLAGSET(Flags, pufAllowStoredSiteWithProtocol)))
    {
      // this can be optimized as the list is sorted
      for (Integer Index = 0; Index < StoredSessions->CountIncludingHidden; Index++)
      {
        TSessionData * AData = (TSessionData *)StoredSessions->Items[Index];
        if (!AData->IsWorkspace)
        {
          bool Match = false;
          // Comparison optimizations as this is called many times
          // e.g. when updating jumplist
          if ((AData->Name.Length() == DecodedUrl.Length()) &&
              SameText(AData->Name, DecodedUrl))
          {
            Match = true;
          }
          else if ((AData->Name.Length() < DecodedUrl.Length()) &&
                   (DecodedUrl[AData->Name.Length() + 1] == L'/') &&
                   // StrLIComp is an equivalent of SameText
                   (StrLIComp(AData->Name.c_str(), DecodedUrl.c_str(), AData->Name.Length()) == 0))
          {
            Match = true;
          }

          if (Match)
          {
            Data = AData;
            break;
          }
        }
      }
    }

    UnicodeString ARemoteDirectory;

    if (Data != NULL)
    {
      DoCopyData(Data, ParseOnly);
      FSource = Data->FSource;
      int P = 1;
      while (!AnsiSameText(DecodeUrlChars(Url.SubString(1, P)), Data->Name))
      {
        P++;
        DebugAssert(P <= Url.Length());
      }
      ARemoteDirectory = Url.SubString(P + 1, Url.Length() - P);

      if (Data->Hidden && !ParseOnly)
      {
        Data->Remove();
        StoredSessions->Remove(Data);
        // only modified, implicit
        StoredSessions->Save(false, false);
      }

      if (MaskedUrl != NULL)
      {
        (*MaskedUrl) += Url;
      }
    }
    else
    {
      // When ad-hoc URL is used, always display error when the directories are not valid,
      // no matter if they are part of the URL or raw settings.
      RequireDirectories = true;

      // This happens when pasting URL on Login dialog
      if (StoredSessions != NULL)
      {
        DoCopyData(StoredSessions->DefaultSettings, ParseOnly);
      }
      Name = L"";

      int PSlash = Url.Pos(L"/");
      if (PSlash == 0)
      {
        PSlash = Url.Length() + 1;
      }

      UnicodeString ConnectInfo = Url.SubString(1, PSlash - 1);

      int P = ConnectInfo.LastDelimiter(L"@");

      UnicodeString UserInfo;
      UnicodeString HostInfo;

      if (P > 0)
      {
        UserInfo = ConnectInfo.SubString(1, P - 1);
        HostInfo = ConnectInfo.SubString(P + 1, ConnectInfo.Length() - P);
      }
      else
      {
        HostInfo = ConnectInfo;
      }

      UnicodeString OrigHostInfo = HostInfo;
      if ((HostInfo.Length() >= 2) && (HostInfo[1] == L'[') && ((P = HostInfo.Pos(L"]")) > 0))
      {
        HostName = HostInfo.SubString(2, P - 2);
        HostInfo.Delete(1, P);
        if (!HostInfo.IsEmpty() && (HostInfo[1] == L':'))
        {
          HostInfo.Delete(1, 1);
        }
      }
      else
      {
        HostName = DecodeUrlChars(CutToChar(HostInfo, L':', true));
      }

      // expanded from ?: operator, as it caused strange "access violation" errors
      if (!HostInfo.IsEmpty())
      {
        int APortNumber = StrToIntDef(DecodeUrlChars(HostInfo), -1);
        if ((APortNumber > 0) && (APortNumber <= 65535))
        {
          PortNumber = APortNumber;
          PortNumberDefined = true;
        }
      }
      else if (ProtocolDefined)
      {
        if ((AFSProtocol == fsWebDAV) &&
            (IsDomainOrSubdomain(HostName, S3HostName) ||
             IsDomainOrSubdomain(HostName, L"digitaloceanspaces.com") ||
             IsDomainOrSubdomain(HostName, S3GoogleCloudHostName) ||
             IsDomainOrSubdomain(HostName, L"r2.cloudflarestorage.com") ||
             (IsDomainOrSubdomain(HostName, L"oraclecloud.com") && ContainsText(HostName, L".compat.objectstorage."))))
        {
          AFSProtocol = fsS3;
        }
        PortNumber = DefaultProtocolPortNumber;
      }

      if (ProtocolDefined)
      {
        Ftps = AFtps;
      }

      UnicodeString UserInfoWithoutConnectionParams = CutToChar(UserInfo, UrlParamSeparator, false);
      UnicodeString ConnectionParams = UserInfo;
      UserInfo = UserInfoWithoutConnectionParams;

      std::unique_ptr<TStrings> RawSettings(new TStringList());

      while (!ConnectionParams.IsEmpty())
      {
        UnicodeString ConnectionParam = CutToChar(ConnectionParams, UrlParamSeparator, false);
        UnicodeString ConnectionParamName = CutToChar(ConnectionParam, UrlParamValueSeparator, false);
        if (SameText(ConnectionParamName, UrlHostKeyParamName))
        {
          HostKey = DecodeUrlChars(ConnectionParam);
          FOverrideCachedHostKey = false;
        }
        else if (StartsText(UrlRawSettingsParamNamePrefix, ConnectionParamName))
        {
          UnicodeString AName = RightStr(ConnectionParamName, ConnectionParamName.Length() - UrlRawSettingsParamNamePrefix.Length());
          AName = DecodeUrlChars(AName);
          UnicodeString Value = DecodeUrlChars(ConnectionParam);
          if (SameText(AName, L"Name"))
          {
            Name = Value;
          }
          else
          {
            RawSettings->Values[AName] = Value;
          }
        }
      }

      if (RawSettings->Count > 0) // optimization
      {
        ApplyRawSettings(RawSettings.get(), Unsafe);
      }

      bool HasPassword = (UserInfo.Pos(L':') > 0);
      UnicodeString RawUserName = CutToChar(UserInfo, L':', false);
      UserName = DecodeUrlChars(RawUserName);

      Password = DecodeUrlChars(UserInfo);
      if (HasPassword)
      {
        Password = DenormalizeString(Password);
      }

      UnicodeString RemoteDirectoryWithSessionParams = Url.SubString(PSlash, Url.Length() - PSlash + 1);
      ARemoteDirectory = CutToChar(RemoteDirectoryWithSessionParams, UrlParamSeparator, false);
      UnicodeString SessionParams = RemoteDirectoryWithSessionParams;

      // We should handle session params in "stored session" branch too.
      // And particularly if there's a "save" param, we should actually not try to match the
      // URL against site names
      while (!SessionParams.IsEmpty())
      {
        UnicodeString SessionParam = CutToChar(SessionParams, UrlParamSeparator, false);
        UnicodeString SessionParamName = CutToChar(SessionParam, UrlParamValueSeparator, false);
        if (SameText(SessionParamName, UrlSaveParamName))
        {
          FSaveOnly = (StrToIntDef(SessionParam, 1) != 0);
        }
      }

      if (MaskedUrl != NULL)
      {
        (*MaskedUrl) += RawUserName;
        if (HasPassword)
        {
          (*MaskedUrl) += L":" + PasswordMask;
        }
        if (!RawUserName.IsEmpty() || HasPassword)
        {
          (*MaskedUrl) += L"@";
        }
        (*MaskedUrl) += OrigHostInfo + ARemoteDirectory;
      }
    }

    if (!ARemoteDirectory.IsEmpty() && (ARemoteDirectory != L"/"))
    {
      if ((ARemoteDirectory[ARemoteDirectory.Length()] != L'/') &&
          (FileName != NULL))
      {
        *FileName = DecodeUrlChars(UnixExtractFileName(ARemoteDirectory));
        ARemoteDirectory = UnixExtractFilePath(ARemoteDirectory);
      }
      RemoteDirectory = DecodeUrlChars(ARemoteDirectory);
      // Is already true for ad-hoc URL, but we want to error even for "storedsite/path/"-style URL.
      RequireDirectories = true;
    }

    DefaultsOnly = false;
  }
  else
  {
    // This happens when pasting URL on Login dialog
    if (StoredSessions != NULL)
    {
      CopyData(StoredSessions->DefaultSettings);
    }

    DefaultsOnly = true;
  }

  if (ProtocolDefined)
  {
    FSProtocol = AFSProtocol;
  }

  if (Options != NULL)
  {
    // we deliberately do keep defaultonly to false, in presence of any option,
    // as the option should not make session "connectable"

    UnicodeString Value;
    if (Options->FindSwitch(USERNAME_SWITCH, Value))
    {
      UserName = Value;
    }
    if (Options->FindSwitch(PASSWORD_SWITCH, Value))
    {
      Password = Value;
    }
    if (Options->FindSwitch(SESSIONNAME_SWICH, Value))
    {
      Name = Value;
    }
    if (Options->FindSwitch(NEWPASSWORD_SWITCH, Value))
    {
      ChangePassword = true;
      NewPassword = Value;
    }
    if (Options->FindSwitch(PRIVATEKEY_SWITCH, Value))
    {
      PublicKeyFile = Value;
    }
    if (Options->FindSwitch(L"clientcert", Value))
    {
      TlsCertificateFile = Value;
    }
    if (Options->FindSwitch(PassphraseOption, Value))
    {
      Passphrase = Value;
    }
    if (Options->FindSwitch(L"timeout", Value))
    {
      Timeout = StrToInt(Value);
    }
    if (Options->FindSwitch(L"hostkey", Value) ||
        Options->FindSwitch(L"certificate", Value))
    {
      HostKey = Value;
      FOverrideCachedHostKey = true;
    }
    FtpPasvMode = Options->SwitchValue(L"passive", FtpPasvMode);
    if (Options->FindSwitch(L"implicit"))
    {
      bool Enabled = Options->SwitchValue(L"implicit", true);
      Ftps = Enabled ? ftpsImplicit : ftpsNone;
      if (!PortNumberDefined && Enabled)
      {
        PortNumber = FtpsImplicitPortNumber;
      }
    }
    // BACKWARD COMPATIBILITY with 5.5.x
    if (Options->FindSwitch(L"explicitssl"))
    {
      bool Enabled = Options->SwitchValue(L"explicitssl", true);
      Ftps = Enabled ? ftpsExplicitSsl : ftpsNone;
      if (!PortNumberDefined && Enabled)
      {
        PortNumber = FtpPortNumber;
      }
    }
    if (Options->FindSwitch(L"explicit") ||
        // BACKWARD COMPATIBILITY with 5.5.x
        Options->FindSwitch(L"explicittls"))
    {
      UnicodeString SwitchName =
        Options->FindSwitch(L"explicit") ? L"explicit" : L"explicittls";
      bool Enabled = Options->SwitchValue(SwitchName, true);
      Ftps = Enabled ? ftpsExplicitTls : ftpsNone;
      if (!PortNumberDefined && Enabled)
      {
        PortNumber = FtpPortNumber;
      }
    }
    if (Options->FindSwitch(RawSettingsOption))
    {
      std::unique_ptr<TStrings> RawSettings(new TStringList());
      if (Options->FindSwitch(RawSettingsOption, RawSettings.get()))
      {
        ApplyRawSettings(RawSettings.get(), Unsafe);
      }
    }
    if (Options->FindSwitch(PASSWORDSFROMFILES_SWITCH) &&
        // Ad-hoc condition for the current only specific use of pufParseOnly
        // (when we do not want to consume the password pipe by the current instance,
        // in case the URL needs to be passed to another instance)
        !ParseOnly)
    {
      ReadPasswordsFromFiles();
    }
  }

  return true;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::ApplyRawSettings(TStrings * RawSettings, bool Unsafe)
{
  std::unique_ptr<TOptionsStorage> OptionsStorage(new TOptionsStorage(RawSettings, false));
  ApplyRawSettings(OptionsStorage.get(), Unsafe, false);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::ApplyRawSettings(THierarchicalStorage * Storage, bool Unsafe, bool RespectDisablePasswordStoring)
{
  bool Dummy;
  DoLoad(Storage, false, Dummy, Unsafe, RespectDisablePasswordStoring);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::ConfigureTunnel(int APortNumber)
{
  FOrigHostName = HostName;
  FOrigPortNumber = PortNumber;
  FOrigProxyMethod = ProxyMethod;

  HostName = L"127.0.0.1";
  PortNumber = APortNumber;
  // proxy settings is used for tunnel
  ProxyMethod = ::pmNone;
  FLogicalHostName = FOrigHostName;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::RollbackTunnel()
{
  HostName = FOrigHostName;
  PortNumber = FOrigPortNumber;
  ProxyMethod = FOrigProxyMethod;
  FLogicalHostName = L"";
}
//---------------------------------------------------------------------
TSessionData * TSessionData::CreateTunnelData(int TunnelLocalPortNumber)
{
  std::unique_ptr<TSessionData> TunnelData(new TSessionData(EmptyStr));
  TunnelData->Assign(StoredSessions->DefaultSettings);
  TunnelData->Name = FMTLOAD(TUNNEL_SESSION_NAME, (SessionName));
  TunnelData->Tunnel = false;
  TunnelData->HostName = TunnelHostName;
  TunnelData->PortNumber = TunnelPortNumber;
  TunnelData->UserName = TunnelUserName;
  TunnelData->Password = TunnelPassword;
  TunnelData->PublicKeyFile = TunnelPublicKeyFile;
  TunnelData->DetachedCertificate = EmptyStr;
  TunnelData->Passphrase = TunnelPassphrase;
  UnicodeString AHostName = HostNameExpanded;
  if (IsIPv6Literal(AHostName))
  {
    AHostName = EscapeIPv6Literal(AHostName);
  }
  TunnelData->TunnelPortFwd = FORMAT(L"L%d\t%s:%d",
    (TunnelLocalPortNumber, AHostName, PortNumber));
  TunnelData->HostKey = TunnelHostKey;

  // inherit proxy options on the main session
  TunnelData->ProxyMethod = ProxyMethod;
  TunnelData->ProxyHost = ProxyHost;
  TunnelData->ProxyPort = ProxyPort;
  TunnelData->ProxyUsername = ProxyUsername;
  TunnelData->ProxyPassword = ProxyPassword;
  TunnelData->ProxyTelnetCommand = ProxyTelnetCommand;
  TunnelData->ProxyLocalCommand = ProxyLocalCommand;
  TunnelData->ProxyDNS = ProxyDNS;
  TunnelData->ProxyLocalhost = ProxyLocalhost;

  // inherit most SSH options of the main session (except for private key and bugs)
  TunnelData->Compression = Compression;
  TunnelData->CipherList = CipherList;
  TunnelData->Ssh2DES = Ssh2DES;

  TunnelData->KexList = KexList;
  TunnelData->RekeyData = RekeyData;
  TunnelData->RekeyTime = RekeyTime;

  TunnelData->SshNoUserAuth = SshNoUserAuth;
  TunnelData->AuthGSSAPI = AuthGSSAPI;
  TunnelData->AuthGSSAPIKEX = AuthGSSAPIKEX;
  TunnelData->GSSAPIFwdTGT = GSSAPIFwdTGT;
  TunnelData->TryAgent = TryAgent;
  TunnelData->AgentFwd = AgentFwd;
  TunnelData->AuthKI = AuthKI;
  TunnelData->AuthKIPassword = AuthKIPassword;
  return TunnelData.release();
}
//---------------------------------------------------------------------
void __fastcall TSessionData::ExpandEnvironmentVariables()
{
  HostName = HostNameExpanded;
  UserName = UserNameExpanded;
  PublicKeyFile = ::ExpandEnvironmentVariables(PublicKeyFile);
  DetachedCertificate = ::ExpandEnvironmentVariables(DetachedCertificate);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::ValidatePath(const UnicodeString Path)
{
  DebugUsedParam(Path);
  // noop
}
//---------------------------------------------------------------------
void __fastcall TSessionData::ValidateName(const UnicodeString Name)
{
  // keep consistent with MakeValidName
  if (Name.LastDelimiter(L"/") > 0)
  {
    throw Exception(FMTLOAD(ITEM_NAME_INVALID, (Name, L"/")));
  }
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::MakeValidName(const UnicodeString & Name)
{
  // keep consistent with ValidateName
  return ReplaceStr(Name, L"/", L"\\");
}
//---------------------------------------------------------------------
RawByteString __fastcall TSessionData::EncryptPassword(const UnicodeString & Password, UnicodeString Key)
{
  return Configuration->EncryptPassword(Password, Key);
}
//---------------------------------------------------------------------
RawByteString __fastcall TSessionData::StronglyRecryptPassword(const RawByteString & Password, UnicodeString Key)
{
  return Configuration->StronglyRecryptPassword(Password, Key);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::DecryptPassword(const RawByteString & Password, UnicodeString Key)
{
  UnicodeString Result;
  try
  {
    Result = Configuration->DecryptPassword(Password, Key);
  }
  catch(EAbort &)
  {
    // silently ignore aborted prompts for master password and return empty password
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString TSessionData::GetSessionPasswordEncryptionKey() const
{
  return UserName + HostName;
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::GetCanLogin()
{
  return !FHostName.IsEmpty();
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::GetIsLocalBrowser()
{
  return !LocalDirectory.IsEmpty() && !OtherLocalDirectory.IsEmpty();
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::GetCanOpen()
{
  return CanLogin || IsLocalBrowser;
}
//---------------------------------------------------------------------------
int TSessionData::GetDefaultPort()
{
  return DefaultPort(FSProtocol, Ftps);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetSessionKey()
{
  UnicodeString Result = FORMAT(L"%s@%s", (UserName, HostName));
  if (PortNumber != GetDefaultPort())
  {
    Result += FORMAT(L":%d", (PortNumber));
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetInternalStorageKey()
{
  // This is probably useless remnant of previous use of this method from OpenSessionInPutty
  // that needs the method to return something even for ad-hoc sessions
  if (Name.IsEmpty())
  {
    return SessionKey;
  }
  else
  {
    return Name;
  }
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetStorageKey()
{
  return SessionName;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::FormatSiteKey(const UnicodeString & HostName, int PortNumber)
{
  return FORMAT(L"%s:%d", (HostName, PortNumber));
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetSiteKey()
{
  return FormatSiteKey(HostNameExpanded, PortNumber);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetHostName(UnicodeString value)
{
  if (FHostName != value)
  {
    // HostName is key for password encryption
    UnicodeString XPassword = Password;
    UnicodeString XNewPassword = NewPassword;
    UnicodeString XEncryptKey = EncryptKey;

    // This is now hardly used as hostname is parsed directly on login dialog.
    // But can be used when importing sites from PuTTY, as it allows same format too.
    int P = value.LastDelimiter(L"@");
    if (P > 0)
    {
      UserName = value.SubString(1, P - 1);
      value = value.SubString(P + 1, value.Length() - P);
    }
    FHostName = value;
    Modify();

    Password = XPassword;
    NewPassword = XNewPassword;
    EncryptKey = XEncryptKey;
    Shred(XPassword);
    Shred(XNewPassword);
    Shred(XEncryptKey);
  }
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetHostNameExpanded()
{
  return ::ExpandEnvironmentVariables(HostName);
}
//---------------------------------------------------------------------
UnicodeString TSessionData::GetHostNameSource()
{
  UnicodeString Result;
  if (HostName != HostNameExpanded)
  {
    Result = HostName;
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPortNumber(int value)
{
  SET_SESSION_PROPERTY(PortNumber);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetShell(UnicodeString value)
{
  SET_SESSION_PROPERTY(Shell);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetSftpServer(UnicodeString value)
{
  SET_SESSION_PROPERTY(SftpServer);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetClearAliases(bool value)
{
  SET_SESSION_PROPERTY(ClearAliases);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetListingCommand(UnicodeString value)
{
  SET_SESSION_PROPERTY(ListingCommand);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetIgnoreLsWarnings(bool value)
{
  SET_SESSION_PROPERTY(IgnoreLsWarnings);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetUnsetNationalVars(bool value)
{
  SET_SESSION_PROPERTY(UnsetNationalVars);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetUserName(UnicodeString value)
{
  // Avoid password recryption (what may popup master password prompt)
  if (FUserName != value)
  {
    // UserName is key for password encryption
    UnicodeString XPassword = Password;
    UnicodeString XNewPassword = NewPassword;
    UnicodeString XEncryptKey = EncryptKey;
    SET_SESSION_PROPERTY(UserName);
    Password = XPassword;
    NewPassword = XNewPassword;
    EncryptKey = XEncryptKey;
    Shred(XPassword);
    Shred(XNewPassword);
    Shred(XEncryptKey);
  }
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetUserNameExpanded()
{
  UnicodeString Result = ::ExpandEnvironmentVariables(UserName);
  if (Result.IsEmpty() && HasS3AutoCredentials())
  {
    Result = S3EnvUserName(S3Profile);
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString TSessionData::GetUserNameSource()
{
  UnicodeString Result;
  if (UserName.IsEmpty() && HasS3AutoCredentials())
  {
    S3EnvUserName(S3Profile, &Result);
  }
  if (Result.IsEmpty() && (UserName != UserNameExpanded))
  {
    Result = UserName;
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPassword(UnicodeString avalue)
{
  RawByteString value = EncryptPassword(avalue, GetSessionPasswordEncryptionKey());
  SET_SESSION_PROPERTY(Password);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetPassword() const
{
  return DecryptPassword(FPassword, GetSessionPasswordEncryptionKey());
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetNewPassword(UnicodeString avalue)
{
  RawByteString value = EncryptPassword(avalue, GetSessionPasswordEncryptionKey());
  SET_SESSION_PROPERTY(NewPassword);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetNewPassword() const
{
  return DecryptPassword(FNewPassword, GetSessionPasswordEncryptionKey());
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetChangePassword(bool value)
{
  SET_SESSION_PROPERTY(ChangePassword);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPingInterval(int value)
{
  SET_SESSION_PROPERTY(PingInterval);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTryAgent(bool value)
{
  SET_SESSION_PROPERTY(TryAgent);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetAgentFwd(bool value)
{
  SET_SESSION_PROPERTY(AgentFwd);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetAuthKI(bool value)
{
  SET_SESSION_PROPERTY(AuthKI);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetAuthKIPassword(bool value)
{
  SET_SESSION_PROPERTY(AuthKIPassword);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetAuthGSSAPI(bool value)
{
  SET_SESSION_PROPERTY(AuthGSSAPI);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetAuthGSSAPIKEX(bool value)
{
  SET_SESSION_PROPERTY(AuthGSSAPIKEX);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetGSSAPIFwdTGT(bool value)
{
  SET_SESSION_PROPERTY(GSSAPIFwdTGT);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetChangeUsername(bool value)
{
  SET_SESSION_PROPERTY(ChangeUsername);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetCompression(bool value)
{
  SET_SESSION_PROPERTY(Compression);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSsh2DES(bool value)
{
  SET_SESSION_PROPERTY(Ssh2DES);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSshNoUserAuth(bool value)
{
  SET_SESSION_PROPERTY(SshNoUserAuth);
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::GetUsesSsh()
{
  return IsSshProtocol(FSProtocol);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetCipher(int Index, TCipher value)
{
  DebugAssert(Index >= 0 && Index < CIPHER_COUNT);
  SET_SESSION_PROPERTY(Ciphers[Index]);
}
//---------------------------------------------------------------------
TCipher __fastcall TSessionData::GetCipher(int Index) const
{
  DebugAssert(Index >= 0 && Index < CIPHER_COUNT);
  return FCiphers[Index];
}
//---------------------------------------------------------------------
template<class AlgoT>
void __fastcall TSessionData::SetAlgoList(AlgoT * List, const AlgoT * DefaultList, const UnicodeString * Names,
  int Count, AlgoT WarnAlgo, UnicodeString value)
{
  std::vector<bool> Used(Count); // initialized to false
  std::vector<AlgoT> NewList(Count);

  bool HasWarnAlgo = (WarnAlgo >= AlgoT());
  const AlgoT * WarnPtr;
  int WarnDefaultIndex;
  if (!HasWarnAlgo)
  {
    WarnPtr = NULL;
    WarnDefaultIndex = -1;
  }
  else
  {
    WarnPtr = std::find(DefaultList, DefaultList + Count, WarnAlgo);
    DebugAssert(WarnPtr != NULL);
    WarnDefaultIndex = (WarnPtr - DefaultList);
  }

  int Index = 0;
  while (!value.IsEmpty())
  {
    UnicodeString AlgoStr = CutToChar(value, L',', true);
    for (int Algo = 0; Algo < Count; Algo++)
    {
      if (!AlgoStr.CompareIC(Names[Algo]) &&
          !Used[Algo] && DebugAlwaysTrue(Index < Count))
      {
        NewList[Index] = (AlgoT)Algo;
        Used[Algo] = true;
        Index++;
        break;
      }
    }
  }

  if (HasWarnAlgo && !Used[WarnAlgo] && DebugAlwaysTrue(Index < Count))
  {
    NewList[Index] = WarnAlgo;
    Used[WarnAlgo] = true;
    Index++;
  }

  int WarnIndex = -1;
  if (HasWarnAlgo)
  {
    WarnIndex = std::find(NewList.begin(), NewList.end(), WarnAlgo) - NewList.begin();
  }

  bool Priority = true;
  for (int DefaultIndex = 0; (DefaultIndex < Count); DefaultIndex++)
  {
    AlgoT DefaultAlgo = DefaultList[DefaultIndex];
    if (!Used[DefaultAlgo] && DebugAlwaysTrue(Index < Count))
    {
      int TargetIndex;
      // Unused algs that are prioritized in the default list,
      // should be merged before the existing custom list
      if (Priority)
      {
        TargetIndex = DefaultIndex;
      }
      else
      {
        if (HasWarnAlgo && (DefaultIndex < WarnDefaultIndex))
        {
          TargetIndex = WarnIndex;
        }
        else
        {
          TargetIndex = Index;
        }
      }

      NewList.insert(NewList.begin() + TargetIndex, DefaultAlgo);
      DebugAssert(NewList.back() == AlgoT());
      NewList.pop_back();

      if (HasWarnAlgo && (TargetIndex <= WarnIndex))
      {
        WarnIndex++;
      }

      Index++;
    }
    else
    {
      Priority = false;
    }
  }

  if (!std::equal(NewList.begin(), NewList.end(), List))
  {
    std::copy(NewList.begin(), NewList.end(), List);
    Modify();
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetCipherList(UnicodeString value)
{
  SetAlgoList(FCiphers, DefaultCipherList, CipherNames, CIPHER_COUNT, cipWarn, value);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetCipherList() const
{
  UnicodeString Result;
  for (int Index = 0; Index < CIPHER_COUNT; Index++)
  {
    Result += UnicodeString(Index ? L"," : L"") + CipherNames[Cipher[Index]];
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetKex(int Index, TKex value)
{
  DebugAssert(Index >= 0 && Index < KEX_COUNT);
  SET_SESSION_PROPERTY(Kex[Index]);
}
//---------------------------------------------------------------------
TKex __fastcall TSessionData::GetKex(int Index) const
{
  DebugAssert(Index >= 0 && Index < KEX_COUNT);
  return FKex[Index];
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetKexList(UnicodeString value)
{
  SetAlgoList(FKex, DefaultKexList, KexNames, KEX_COUNT, kexWarn, value);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetKexList() const
{
  UnicodeString Result;
  for (int Index = 0; Index < KEX_COUNT; Index++)
  {
    Result += UnicodeString(Index ? L"," : L"") + KexNames[Kex[Index]];
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetHostKeys(int Index, THostKey value)
{
  DebugAssert(Index >= 0 && Index < HOSTKEY_COUNT);
  SET_SESSION_PROPERTY(HostKeys[Index]);
}
//---------------------------------------------------------------------
THostKey __fastcall TSessionData::GetHostKeys(int Index) const
{
  DebugAssert(Index >= 0 && Index < HOSTKEY_COUNT);
  return FHostKeys[Index];
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetHostKeyList(UnicodeString value)
{
  SetAlgoList(FHostKeys, DefaultHostKeyList, HostKeyNames, HOSTKEY_COUNT, hkWarn, value);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetHostKeyList() const
{
  UnicodeString Result;
  for (int Index = 0; Index < HOSTKEY_COUNT; Index++)
  {
    Result += UnicodeString(Index ? L"," : L"") + HostKeyNames[HostKeys[Index]];
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetGssLib(int Index, TGssLib value)
{
  DebugAssert(Index >= 0 && Index < GSSLIB_COUNT);
  SET_SESSION_PROPERTY(GssLib[Index]);
}
//---------------------------------------------------------------------
TGssLib __fastcall TSessionData::GetGssLib(int Index) const
{
  DebugAssert(Index >= 0 && Index < GSSLIB_COUNT);
  return FGssLib[Index];
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetGssLibList(UnicodeString value)
{
  SetAlgoList(FGssLib, DefaultGssLibList, GssLibNames, GSSLIB_COUNT, TGssLib(-1), value);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetGssLibList() const
{
  UnicodeString Result;
  for (int Index = 0; Index < GSSLIB_COUNT; Index++)
  {
    Result += UnicodeString(Index ? L"," : L"") + GssLibNames[GssLib[Index]];
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetGssLibCustom(UnicodeString value)
{
  SET_SESSION_PROPERTY(GssLibCustom);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPublicKeyFile(UnicodeString value)
{
  if (FPublicKeyFile != value)
  {
    // PublicKeyFile is key for Passphrase encryption
    UnicodeString XPassphrase = Passphrase;

    // StripPathQuotes should not be needed as we do not feed quotes anymore
    FPublicKeyFile = StripPathQuotes(value);
    Modify();

    Passphrase = XPassphrase;
    Shred(XPassphrase);
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetDetachedCertificate(UnicodeString value)
{
  SET_SESSION_PROPERTY(DetachedCertificate);
}
//---------------------------------------------------------------------
UnicodeString TSessionData::ResolvePublicKeyFile()
{
  UnicodeString Result = PublicKeyFile;
  if (Result.IsEmpty())
  {
    Result = Configuration->DefaultKeyFile;
  }
  // StripPathQuotes should not be needed as we do not feed quotes anymore
  Result = StripPathQuotes(::ExpandEnvironmentVariables(Result));
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPassphrase(UnicodeString avalue)
{
  RawByteString value = EncryptPassword(avalue, PublicKeyFile);
  SET_SESSION_PROPERTY(Passphrase);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetPassphrase() const
{
  return DecryptPassword(FPassphrase, PublicKeyFile);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetReturnVar(UnicodeString value)
{
  SET_SESSION_PROPERTY(ReturnVar);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetExitCode1IsError(bool value)
{
  SET_SESSION_PROPERTY(ExitCode1IsError);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetLookupUserGroups(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(LookupUserGroups);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetEOLType(TEOLType value)
{
  SET_SESSION_PROPERTY(EOLType);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetTrimVMSVersions(bool value)
{
  SET_SESSION_PROPERTY(TrimVMSVersions);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetVMSAllRevisions(bool value)
{
  SET_SESSION_PROPERTY(VMSAllRevisions);
}
//---------------------------------------------------------------------------
TDateTime __fastcall TSessionData::GetTimeoutDT()
{
  return SecToDateTime(Timeout);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetTimeout(int value)
{
  SET_SESSION_PROPERTY(Timeout);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetFSProtocol(TFSProtocol value)
{
  SET_SESSION_PROPERTY(FSProtocol);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetFSProtocolStr()
{
  DebugAssert(FSProtocol >= 0 && static_cast<int>(FSProtocol) < FSPROTOCOL_COUNT);
  return FSProtocolNames[FSProtocol];
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetDetectReturnVar(bool value)
{
  if (value != DetectReturnVar)
  {
    ReturnVar = value ? L"" : L"$?";
  }
}
//---------------------------------------------------------------------------
bool __fastcall TSessionData::GetDetectReturnVar()
{
  return ReturnVar.IsEmpty();
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetDefaultShell(bool value)
{
  if (value != DefaultShell)
  {
    Shell = value ? L"" : L"/bin/bash";
  }
}
//---------------------------------------------------------------------------
bool __fastcall TSessionData::GetDefaultShell()
{
  return Shell.IsEmpty();
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetPuttyProtocol(UnicodeString value)
{
  SET_SESSION_PROPERTY(PuttyProtocol);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetNormalizedPuttyProtocol() const
{
  return DefaultStr(PuttyProtocol, PuttySshProtocol);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPingIntervalDT(TDateTime value)
{
  unsigned short hour, min, sec, msec;

  value.DecodeTime(&hour, &min, &sec, &msec);
  PingInterval = ((int)hour)*SecsPerHour + ((int)min)*SecsPerMin + sec;
}
//---------------------------------------------------------------------------
TDateTime __fastcall TSessionData::GetPingIntervalDT()
{
  return SecToDateTime(PingInterval);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetPingType(TPingType value)
{
  SET_SESSION_PROPERTY(PingType);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetAddressFamily(TAddressFamily value)
{
  SET_SESSION_PROPERTY(AddressFamily);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetRekeyData(UnicodeString value)
{
  SET_SESSION_PROPERTY(RekeyData);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetRekeyTime(unsigned int value)
{
  SET_SESSION_PROPERTY(RekeyTime);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetDefaultSessionName()
{
  UnicodeString Result;
  if (IsLocalBrowser)
  {
    // See also TScpCommanderForm::GetLocalBrowserSessionTitle
    UnicodeString Path1 = ExtractShortName(LocalDirectory, false);
    UnicodeString Path2 = ExtractShortName(OtherLocalDirectory, false);
    Result = Path1 + TitleSeparator + Path2;
  }
  else if (!HostName.IsEmpty() && !UserName.IsEmpty())
  {
    // If we ever choose to include port number,
    // we have to escape IPv6 literals in HostName
    Result = FORMAT(L"%s@%s", (UserName, HostName));
  }
  else if (!HostName.IsEmpty())
  {
    Result = HostName;
  }
  else
  {
    Result = L"session";
  }
  Result = MakeValidName(Result);
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetNameWithoutHiddenPrefix()
{
  UnicodeString Result = Name;
  if (Hidden)
  {
    Result = Result.SubString(TNamedObjectList::HiddenPrefix.Length() + 1, Result.Length() - TNamedObjectList::HiddenPrefix.Length());
  }
  return Result;
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::HasSessionName()
{
  return (!GetNameWithoutHiddenPrefix().IsEmpty() && (Name != DefaultName));
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetSessionName()
{
  UnicodeString Result;
  if (HasSessionName())
  {
    Result = GetNameWithoutHiddenPrefix();
  }
  else
  {
    Result = DefaultSessionName;
  }
  return Result;
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::IsSecure()
{
  bool Result;
  switch (FSProtocol)
  {
    case fsSCPonly:
    case fsSFTP:
    case fsSFTPonly:
      Result = true;
      break;

    case fsFTP:
    case fsWebDAV:
    case fsS3:
      Result = (Ftps != ftpsNone);
      break;

    default:
      DebugFail();
      break;
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetProtocolUrl(bool HttpForWebDAV)
{
  UnicodeString Url;
  switch (FSProtocol)
  {
    case fsSCPonly:
      Url = ScpProtocol;
      break;

    default:
      DebugFail();
      // fallback
    case fsSFTP:
    case fsSFTPonly:
      Url = SftpProtocol;
      break;

    case fsFTP:
      if (Ftps == ftpsImplicit)
      {
        Url = FtpsProtocol;
      }
      else if ((Ftps == ftpsExplicitTls) || (Ftps == ftpsExplicitSsl))
      {
        Url = FtpesProtocol;
      }
      else
      {
        Url = FtpProtocol;
      }
      break;

    case fsWebDAV:
      if (HttpForWebDAV)
      {
        if (Ftps == ftpsImplicit)
        {
          Url = HttpsProtocol;
        }
        else
        {
          Url = HttpProtocol;
        }
      }
      else
      {
        if (Ftps == ftpsImplicit)
        {
          Url = WebDAVSProtocol;
        }
        else
        {
          Url = WebDAVProtocol;
        }
      }
      break;

    case fsS3:
      if (Ftps == ftpsImplicit)
      {
        Url = S3Protocol;
      }
      else
      {
        Url = S3PlainProtocol;
      }
      break;
  }

  Url += ProtocolSeparator;

  return Url;
}
//---------------------------------------------------------------------
bool HasIP6LiteralBrackets(const UnicodeString & HostName)
{
  return
    (HostName.Length() >= 2) &&
    (HostName[1] == L'[') &&
    (HostName[HostName.Length()] == L']');
}
//---------------------------------------------------------------------
UnicodeString StripIP6LiteralBrackets(const UnicodeString & HostName)
{
  UnicodeString Result = HostName;
  if (DebugAlwaysTrue(HasIP6LiteralBrackets(Result)))
  {
    Result = Result.SubString(2, Result.Length() - 2);
  }
  return Result;
}
//---------------------------------------------------------------------
bool __fastcall IsIPv6Literal(const UnicodeString & HostName)
{
  UnicodeString Buf = HostName;
  if (HasIP6LiteralBrackets(Buf))
  {
    Buf = StripIP6LiteralBrackets(Buf);
  }
  int Colons = 0;
  bool Result = true;
  for (int Index = 1; Result && (Index <= Buf.Length()); Index++)
  {
    wchar_t C = Buf[Index];
    if (C == L'%')
    {
      break;
    }
    else if (C == L':')
    {
      Colons++;
    }
    else
    {
      Result = IsHex(C);
    }
  }
  Result = Result && (Colons >= 2);
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall EscapeIPv6Literal(const UnicodeString & IP)
{
  UnicodeString Result = IP;
  if (!HasIP6LiteralBrackets(Result))
  {
    Result = L"[" + IP + L"]";
  }
  return Result;
}
//---------------------------------------------------------------------
TStrings * __fastcall TSessionData::GetRawSettingsForUrl()
{
  std::unique_ptr<TSessionData> FactoryDefaults(new TSessionData(L""));
  std::unique_ptr<TSessionData> SessionData(Clone());
  SessionData->FSProtocol = FactoryDefaults->FSProtocol;
  SessionData->HostName = FactoryDefaults->HostName;
  SessionData->PortNumber = FactoryDefaults->PortNumber;
  SessionData->UserName = FactoryDefaults->UserName;
  SessionData->Password = FactoryDefaults->Password;
  SessionData->Ftps = FactoryDefaults->Ftps;
  SessionData->HostKey = FactoryDefaults->HostKey;
  SessionData->CopyNonCoreData(FactoryDefaults.get());
  // Cannot be decided in SaveToOptions as it does not have HostName and UserName, so it cannot calculate DefaultSessionName.
  bool SaveName = HasSessionName() && (Name != DefaultSessionName);
  return SessionData->SaveToOptions(FactoryDefaults.get(), SaveName, false);
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::HasRawSettingsForUrl()
{
  std::unique_ptr<TStrings> RawSettings(GetRawSettingsForUrl());
  return (RawSettings->Count > 0);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GenerateSessionUrl(unsigned int Flags)
{
  UnicodeString Url;

  if (FLAGSET(Flags, sufSpecific))
  {
    Url += WinSCPProtocolPrefix;
  }

  Url += GetProtocolUrl(FLAGSET(Flags, sufHttpForWebDAV));

  // Add username only if it was somehow explicitly specified (so not with S3CredentialsEnv), but if it was, add it in the expanded form.
  // For scripting, we might use unexpanded form (keeping the environment variables),
  // but for consistency with code generation (where explicit expansion code would need to be added), we do not.
  if (FLAGSET(Flags, sufUserName) && !UserName.IsEmpty())
  {
    Url += EncodeUrlString(UserNameExpanded);

    if (FLAGSET(Flags, sufPassword) && !Password.IsEmpty())
    {
      Url += L":" + EncodeUrlString(NormalizeString(Password));
    }

    if (FLAGSET(Flags, sufHostKey) && !HostKey.IsEmpty())
    {
      UnicodeString KeyName;
      UnicodeString Fingerprint = HostKey;
      NormalizeFingerprint(Fingerprint, KeyName);
      UnicodeString S = Fingerprint;
      if (!KeyName.IsEmpty())
      {
        S = KeyName + NormalizedFingerprintSeparator + S;
      }
      S = Base64ToUrlSafe(S); // Noop for MD5 (both in SSH host keys and TLS/SSL)
      S = MD5ToUrlSafe(S); // TLS/SSL fingerprints
      UnicodeString S2 = EncodeUrlString(S);
      DebugAssert(S2 == S2); // There should be nothing left for encoding

      Url +=
        UnicodeString(UrlParamSeparator) + UrlHostKeyParamName +
        UnicodeString(UrlParamValueSeparator) + S2;
    }

    if (FLAGSET(Flags, sufRawSettings))
    {
      std::unique_ptr<TStrings> RawSettings(GetRawSettingsForUrl());
      for (int Index = 0; Index < RawSettings->Count; Index++)
      {
        Url +=
          UnicodeString(UrlParamSeparator) +
          UrlRawSettingsParamNamePrefix + EncodeUrlString(LowerCase(RawSettings->Names[Index])) +
          UnicodeString(UrlParamValueSeparator) + EncodeUrlString(RawSettings->ValueFromIndex[Index]);
      }
    }

    Url += L"@";
  }

  DebugAssert(!HostNameExpanded.IsEmpty());
  if (IsIPv6Literal(HostNameExpanded))
  {
    Url += EscapeIPv6Literal(HostNameExpanded);
  }
  else
  {
    Url += EncodeUrlString(HostNameExpanded);
  }

  if (PortNumber != GetDefaultPort())
  {
    Url += L":" + IntToStr(PortNumber);
  }
  Url += L"/";

  return Url;
}
//---------------------------------------------------------------------
static UnicodeString ScriptCommandOpenLink(TraceInitStr(ScriptCommandLink(L"open")));
//---------------------------------------------------------------------
void __fastcall TSessionData::AddSwitch(
  UnicodeString & Result, const UnicodeString & Name, bool Rtf)
{
  Result += RtfSwitch(Name, ScriptCommandOpenLink, Rtf);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::AddSwitch(
  UnicodeString & Result, const UnicodeString & Name, const UnicodeString & Value, bool Rtf)
{
  Result += RtfSwitch(Name, ScriptCommandOpenLink, Value, Rtf);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::AddSwitch(
  UnicodeString & Result, const UnicodeString & Name, int Value, bool Rtf)
{
  Result += RtfSwitch(Name, ScriptCommandOpenLink, Value, Rtf);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::LookupLastFingerprint()
{
  UnicodeString FingerprintType;
  if (IsSshProtocol(FSProtocol))
  {
    FingerprintType = SshFingerprintType;
  }
  else if (Ftps != ftpsNone)
  {
    FingerprintType = TlsFingerprintType;
  }

  if (!FingerprintType.IsEmpty())
  {
    HostKey = Configuration->LastFingerprint(SiteKey, FingerprintType);
  }

  if (Tunnel)
  {
    // not used anyway
    int TunnelPortNumber = std::max(TunnelLocalPortNumber, Configuration->TunnelLocalPortNumberLow);
    std::unique_ptr<TSessionData> TunnelData(CreateTunnelData(TunnelPortNumber));
    TunnelHostKey = Configuration->LastFingerprint(TunnelData->SiteKey, SshFingerprintType);
  }
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GenerateOpenCommandArgs(bool Rtf)
{
  std::unique_ptr<TSessionData> FactoryDefaults(new TSessionData(L""));
  std::unique_ptr<TSessionData> SessionData(new TSessionData(L""));

  SessionData->Assign(this);

  UnicodeString Result = SessionData->GenerateSessionUrl(sufOpen);

  // Before we reset the FSProtocol
  bool AUsesSsh = SessionData->UsesSsh;
  // SFTP-only is not reflected by the protocol prefix, we have to use rawsettings for that
  if (SessionData->FSProtocol != fsSFTPonly)
  {
    SessionData->FSProtocol = FactoryDefaults->FSProtocol;
  }
  SessionData->HostName = FactoryDefaults->HostName;
  SessionData->PortNumber = FactoryDefaults->PortNumber;
  SessionData->UserName = FactoryDefaults->UserName;
  SessionData->Password = FactoryDefaults->Password;
  SessionData->CopyNonCoreData(FactoryDefaults.get());
  SessionData->Ftps = FactoryDefaults->Ftps;

  if (SessionData->HostKey != FactoryDefaults->HostKey)
  {
    UnicodeString SwitchName = AUsesSsh ? L"hostkey" : L"certificate";
    AddSwitch(Result, SwitchName, SessionData->HostKey, Rtf);
    SessionData->HostKey = FactoryDefaults->HostKey;
  }
  if (SessionData->PublicKeyFile != FactoryDefaults->PublicKeyFile)
  {
    AddSwitch(Result, PRIVATEKEY_SWITCH, SessionData->PublicKeyFile, Rtf);
    SessionData->PublicKeyFile = FactoryDefaults->PublicKeyFile;
  }
  if (SessionData->TlsCertificateFile != FactoryDefaults->TlsCertificateFile)
  {
    AddSwitch(Result, L"clientcert", SessionData->TlsCertificateFile, Rtf);
    SessionData->TlsCertificateFile = FactoryDefaults->TlsCertificateFile;
  }
  if (SessionData->Passphrase != FactoryDefaults->Passphrase)
  {
    AddSwitch(Result, PassphraseOption, SessionData->Passphrase, Rtf);
    SessionData->Passphrase = FactoryDefaults->Passphrase;
  }
  if (SessionData->FtpPasvMode != FactoryDefaults->FtpPasvMode)
  {
    AddSwitch(Result, L"passive", SessionData->FtpPasvMode ? 1 : 0, Rtf);
    SessionData->FtpPasvMode = FactoryDefaults->FtpPasvMode;
  }
  if (SessionData->Timeout != FactoryDefaults->Timeout)
  {
    AddSwitch(Result, L"timeout", SessionData->Timeout, Rtf);
    SessionData->Timeout = FactoryDefaults->Timeout;
  }

  std::unique_ptr<TStrings> RawSettings(SessionData->SaveToOptions(FactoryDefaults.get(), false, false));

  if (RawSettings->Count > 0)
  {
    AddSwitch(Result, RawSettingsOption, Rtf);

    Result += StringsToParams(RawSettings.get());
  }

  return Result;
}
//---------------------------------------------------------------------
static UnicodeString SessionOptionsClassName(L"SessionOptions");
//---------------------------------------------------------------------
void __fastcall TSessionData::AddAssemblyProperty(
  UnicodeString & Result, TAssemblyLanguage Language,
  const UnicodeString & Name, const UnicodeString & Type,
  const UnicodeString & Member)
{
  Result += AssemblyProperty(Language, SessionOptionsClassName, Name, Type, Member, false);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::AddAssemblyProperty(
  UnicodeString & Result, TAssemblyLanguage Language,
  const UnicodeString & Name, const UnicodeString & Value)
{
  Result += AssemblyProperty(Language, SessionOptionsClassName, Name, Value, false);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::AddAssemblyProperty(
  UnicodeString & Result, TAssemblyLanguage Language,
  const UnicodeString & Name, int Value)
{
  Result += AssemblyProperty(Language, SessionOptionsClassName, Name, Value, false);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::AddAssemblyProperty(
  UnicodeString & Result, TAssemblyLanguage Language,
  const UnicodeString & Name, bool Value)
{
  Result += AssemblyProperty(Language, SessionOptionsClassName, Name, Value, false);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::GenerateAssemblyCode(
  TAssemblyLanguage Language, UnicodeString & Head, UnicodeString & Tail, int & Indent)
{
  std::unique_ptr<TSessionData> FactoryDefaults(new TSessionData(L""));
  std::unique_ptr<TSessionData> SessionData(Clone());

  switch (Language)
  {
    case alCSharp:
    case alVBNET:
      // noop
      break;

    case alPowerShell:
      Head +=
        AssemblyCommentLine(Language, LoadStr(CODE_PS_ADD_TYPE)) +
        RtfKeyword(L"Add-Type") + RtfText(" -Path ") + AssemblyString(Language, "WinSCPnet.dll") + RtfPara +
        RtfPara;
      break;

    default:
      DebugFail();
      break;
  }

  Head +=
    AssemblyCommentLine(Language, LoadStr(CODE_SESSION_OPTIONS)) +
    AssemblyNewClassInstanceStart(Language, SessionOptionsClassName, false);

  UnicodeString ProtocolMember;
  switch (SessionData->FSProtocol)
  {
    case fsSCPonly:
      ProtocolMember = "Scp";
      break;

    default:
      DebugFail();
      // fallback
    case fsSFTP:
    case fsSFTPonly:
      ProtocolMember = "Sftp";
      break;

    case fsFTP:
      ProtocolMember = "Ftp";
      break;

    case fsWebDAV:
      ProtocolMember = "Webdav";
      break;

    case fsS3:
      ProtocolMember = "S3";
      break;
  }

  // Before we reset the FSProtocol
  bool AUsesSsh = SessionData->UsesSsh;

  // Protocol is set unconditionally, we want even the default SFTP
  AddAssemblyProperty(Head, Language, L"Protocol", L"Protocol", ProtocolMember);
  // SFTP-only is not reflected by the protocol prefix, we have to use rawsettings for that
  if (SessionData->FSProtocol != fsSFTPonly)
  {
    SessionData->FSProtocol = FactoryDefaults->FSProtocol;
  }
  if (SessionData->HostName != FactoryDefaults->HostName)
  {
    AddAssemblyProperty(Head, Language, L"HostName", HostName);
    SessionData->HostName = FactoryDefaults->HostName;
  }
  int ADefaultPort = GetDefaultPort();
  if (SessionData->PortNumber != ADefaultPort)
  {
    AddAssemblyProperty(Head, Language, L"PortNumber", PortNumber);
  }
  SessionData->PortNumber = FactoryDefaults->PortNumber;
  if (SessionData->UserName != FactoryDefaults->UserName)
  {
    AddAssemblyProperty(Head, Language, L"UserName", UserName);
    SessionData->UserName = FactoryDefaults->UserName;
  }
  if (SessionData->Password != FactoryDefaults->Password)
  {
    AddAssemblyProperty(Head, Language, L"Password", NormalizeString(Password));
    SessionData->Password = FactoryDefaults->Password;
  }

  SessionData->CopyNonCoreData(FactoryDefaults.get());

  TFtps DefaultFtps = FactoryDefaults->Ftps;
  if (FSProtocol == fsS3)
  {
    DefaultFtps = ftpsImplicit;
  }

  if (SessionData->Ftps != DefaultFtps)
  {
    // SessionData->FSProtocol is reset already
    switch (FSProtocol)
    {
      case fsFTP:
        {
          UnicodeString FtpSecureMember;
          switch (SessionData->Ftps)
          {
            case ftpsNone:
              DebugFail();
              FtpSecureMember = L"None";
              break;

            case ftpsImplicit:
              FtpSecureMember = L"Implicit";
              break;

            case ftpsExplicitTls:
            case ftpsExplicitSsl:
              FtpSecureMember = L"Explicit";
              break;

            default:
              DebugFail();
              break;
          }
          AddAssemblyProperty(Head, Language, L"FtpSecure", L"FtpSecure", FtpSecureMember);
        }
        break;

      case fsWebDAV:
      case fsS3:
        AddAssemblyProperty(Head, Language, L"Secure", (SessionData->Ftps != ftpsNone));
        break;

      default:
        DebugFail();
        break;
    }
  }
  SessionData->Ftps = FactoryDefaults->Ftps;

  if (SessionData->HostKey != FactoryDefaults->HostKey)
  {
    UnicodeString PropertyName = AUsesSsh ? L"SshHostKeyFingerprint" : L"TlsHostCertificateFingerprint";
    AddAssemblyProperty(Head, Language, PropertyName, SessionData->HostKey);
    SessionData->HostKey = FactoryDefaults->HostKey;
  }
  if (SessionData->PublicKeyFile != FactoryDefaults->PublicKeyFile)
  {
    AddAssemblyProperty(Head, Language, L"SshPrivateKeyPath", SessionData->PublicKeyFile);
    SessionData->PublicKeyFile = FactoryDefaults->PublicKeyFile;
  }
  if (SessionData->TlsCertificateFile != FactoryDefaults->TlsCertificateFile)
  {
    AddAssemblyProperty(Head, Language, L"TlsClientCertificatePath", SessionData->TlsCertificateFile);
    SessionData->TlsCertificateFile = FactoryDefaults->TlsCertificateFile;
  }
  if (SessionData->Passphrase != FactoryDefaults->Passphrase)
  {
    AddAssemblyProperty(Head, Language, L"PrivateKeyPassphrase", SessionData->Passphrase);
    SessionData->Passphrase = FactoryDefaults->Passphrase;
  }
  if (SessionData->FtpPasvMode != FactoryDefaults->FtpPasvMode)
  {
    AddAssemblyProperty(Head, Language, L"FtpMode", L"FtpMode", (SessionData->FtpPasvMode ? L"Passive" : L"Active"));
    SessionData->FtpPasvMode = FactoryDefaults->FtpPasvMode;
  }
  if (SessionData->Timeout != FactoryDefaults->Timeout)
  {
    AddAssemblyProperty(Head, Language, L"TimeoutInMilliseconds", SessionData->Timeout * 1000);
    SessionData->Timeout = FactoryDefaults->Timeout;
  }

  Head += AssemblyNewClassInstanceEnd(Language, false);

  std::unique_ptr<TStrings> RawSettings(SessionData->SaveToOptions(FactoryDefaults.get(), false, false));

  UnicodeString SessionOptionsVariableName = AssemblyVariableName(Language, SessionOptionsClassName);

  if (RawSettings->Count > 0)
  {
    Head +=
      RtfPara +
      AssemblyAddRawSettings(Language, RawSettings.get(), SessionOptionsClassName, L"AddRawSettings");
  }

  Head += RtfPara;

  UnicodeString Indentation = L"    ";
  UnicodeString SessionVariableName = AssemblyVariableName(Language, SessionClassName);
  UnicodeString RtfSessionClass = RtfLibraryClass(SessionClassName);
  UnicodeString RtfSessionOpenMethod = RtfLibraryMethod(SessionClassName, L"Open", false);

  UnicodeString NewSessionInstance = AssemblyNewClassInstance(Language, SessionClassName, false);
  UnicodeString OpenCall =
    Indentation + AssemblyCommentLine(Language, LoadStr(CODE_CONNECT)) +
    Indentation + RtfText(SessionVariableName + L".") + RtfSessionOpenMethod + RtfText(L"(" + SessionOptionsVariableName + L")") +
      AssemblyStatementSeparator(Language) + RtfPara;

  switch (Language)
  {
    case alCSharp:
      Head +=
        RtfKeyword(L"using") + RtfText(" (") + NewSessionInstance + RtfText(L"())") + RtfPara +
        RtfText(L"{") + RtfPara +
        OpenCall;

      Tail =
        RtfText(L"}") + RtfPara;
      break;

    case alVBNET:
      Head +=
        RtfKeyword(L"Using") + RtfText(L" ") + NewSessionInstance + RtfPara +
        OpenCall;

      Tail =
        RtfKeyword(L"End Using") + RtfPara;
      break;

    case alPowerShell:
      Head +=
        NewSessionInstance + RtfPara +
        RtfPara +
        RtfKeyword(L"try") + RtfPara +
        RtfText(L"{") + RtfPara +
        OpenCall;

      Tail =
        RtfText(L"}") + RtfPara +
        RtfKeyword(L"finally") + RtfPara +
        RtfText(L"{") + RtfPara +
        RtfText(Indentation + SessionVariableName + L".") +
          RtfLibraryMethod(SessionClassName, L"Dispose", false) + RtfText(L"()") + RtfPara +
        RtfText(L"}") + RtfPara;
      break;
  }

  Head += RtfPara;

  Indent = 4; // the same for all languages so far
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTimeDifference(TDateTime value)
{
  SET_SESSION_PROPERTY(TimeDifference);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTimeDifferenceAuto(bool value)
{
  SET_SESSION_PROPERTY(TimeDifferenceAuto);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetLocalDirectory(UnicodeString value)
{
  SET_SESSION_PROPERTY(LocalDirectory);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetOtherLocalDirectory(const UnicodeString & value)
{
  SET_SESSION_PROPERTY(OtherLocalDirectory);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetLocalDirectoryExpanded()
{
  return ExpandFileName(::ExpandEnvironmentVariables(LocalDirectory));
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetRemoteDirectory(UnicodeString value)
{
  SET_SESSION_PROPERTY(RemoteDirectory);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSynchronizeBrowsing(bool value)
{
  SET_SESSION_PROPERTY(SynchronizeBrowsing);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetUpdateDirectories(bool value)
{
  SET_SESSION_PROPERTY(UpdateDirectories);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetCacheDirectories(bool value)
{
  SET_SESSION_PROPERTY(CacheDirectories);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetCacheDirectoryChanges(bool value)
{
  SET_SESSION_PROPERTY(CacheDirectoryChanges);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPreserveDirectoryChanges(bool value)
{
  SET_SESSION_PROPERTY(PreserveDirectoryChanges);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetResolveSymlinks(bool value)
{
  SET_SESSION_PROPERTY(ResolveSymlinks);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFollowDirectorySymlinks(bool value)
{
  SET_SESSION_PROPERTY(FollowDirectorySymlinks);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetDSTMode(TDSTMode value)
{
  SET_SESSION_PROPERTY(DSTMode);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetDeleteToRecycleBin(bool value)
{
  SET_SESSION_PROPERTY(DeleteToRecycleBin);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetOverwrittenToRecycleBin(bool value)
{
  SET_SESSION_PROPERTY(OverwrittenToRecycleBin);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetRecycleBinPath(UnicodeString value)
{
  SET_SESSION_PROPERTY(RecycleBinPath);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetPostLoginCommands(UnicodeString value)
{
  SET_SESSION_PROPERTY(PostLoginCommands);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSpecial(bool value)
{
  SET_SESSION_PROPERTY(Special);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetScp1Compatibility(bool value)
{
  SET_SESSION_PROPERTY(Scp1Compatibility);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTcpNoDelay(bool value)
{
  SET_SESSION_PROPERTY(TcpNoDelay);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSendBuf(int value)
{
  SET_SESSION_PROPERTY(SendBuf);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSourceAddress(const UnicodeString & value)
{
  SET_SESSION_PROPERTY(SourceAddress);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProtocolFeatures(const UnicodeString & value)
{
  SET_SESSION_PROPERTY(ProtocolFeatures);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSshSimple(bool value)
{
  SET_SESSION_PROPERTY(SshSimple);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyMethod(TProxyMethod value)
{
  SET_SESSION_PROPERTY(ProxyMethod);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyHost(UnicodeString value)
{
  SET_SESSION_PROPERTY(ProxyHost);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyPort(int value)
{
  SET_SESSION_PROPERTY(ProxyPort);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyUsername(UnicodeString value)
{
  SET_SESSION_PROPERTY(ProxyUsername);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyPassword(UnicodeString avalue)
{
  RawByteString value = EncryptPassword(avalue, ProxyUsername+ProxyHost);
  SET_SESSION_PROPERTY(ProxyPassword);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetProxyPassword() const
{
  return DecryptPassword(FProxyPassword, ProxyUsername+ProxyHost);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyTelnetCommand(UnicodeString value)
{
  SET_SESSION_PROPERTY(ProxyTelnetCommand);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyLocalCommand(UnicodeString value)
{
  SET_SESSION_PROPERTY(ProxyLocalCommand);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyDNS(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(ProxyDNS);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyLocalhost(bool value)
{
  SET_SESSION_PROPERTY(ProxyLocalhost);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFtpProxyLogonType(int value)
{
  SET_SESSION_PROPERTY(FtpProxyLogonType);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetBug(TSshBug Bug, TAutoSwitch value)
{
  DebugAssert(Bug >= 0 && static_cast<unsigned int>(Bug) < LENOF(FBugs));
  SET_SESSION_PROPERTY(Bugs[Bug]);
}
//---------------------------------------------------------------------
TAutoSwitch __fastcall TSessionData::GetBug(TSshBug Bug) const
{
  DebugAssert(Bug >= 0 && static_cast<unsigned int>(Bug) < LENOF(FBugs));
  return FBugs[Bug];
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPuttySettings(UnicodeString value)
{
  SET_SESSION_PROPERTY(PuttySettings);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetCustomParam1(UnicodeString value)
{
  SET_SESSION_PROPERTY(CustomParam1);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetCustomParam2(UnicodeString value)
{
  SET_SESSION_PROPERTY(CustomParam2);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSFTPDownloadQueue(int value)
{
  SET_SESSION_PROPERTY(SFTPDownloadQueue);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSFTPUploadQueue(int value)
{
  SET_SESSION_PROPERTY(SFTPUploadQueue);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSFTPListingQueue(int value)
{
  SET_SESSION_PROPERTY(SFTPListingQueue);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSFTPMaxVersion(int value)
{
  SET_SESSION_PROPERTY(SFTPMaxVersion);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSFTPMaxPacketSize(unsigned long value)
{
  SET_SESSION_PROPERTY(SFTPMaxPacketSize);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSFTPRealPath(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(SFTPRealPath);
}
//---------------------------------------------------------------------
void TSessionData::SetUsePosixRename(bool value)
{
  SET_SESSION_PROPERTY(UsePosixRename);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSFTPBug(TSftpBug Bug, TAutoSwitch value)
{
  DebugAssert(Bug >= 0 && static_cast<unsigned int>(Bug) < LENOF(FSFTPBugs));
  SET_SESSION_PROPERTY(SFTPBugs[Bug]);
}
//---------------------------------------------------------------------
TAutoSwitch __fastcall TSessionData::GetSFTPBug(TSftpBug Bug) const
{
  DebugAssert(Bug >= 0 && static_cast<unsigned int>(Bug) < LENOF(FSFTPBugs));
  return FSFTPBugs[Bug];
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSCPLsFullTime(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(SCPLsFullTime);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetColor(int value)
{
  SET_SESSION_PROPERTY(Color);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetTunnel(bool value)
{
  SET_SESSION_PROPERTY(Tunnel);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTunnelHostName(UnicodeString value)
{
  if (FTunnelHostName != value)
  {
    // HostName is key for password encryption
    UnicodeString XTunnelPassword = TunnelPassword;

    int P = value.LastDelimiter(L"@");
    if (P > 0)
    {
      TunnelUserName = value.SubString(1, P - 1);
      value = value.SubString(P + 1, value.Length() - P);
    }
    FTunnelHostName = value;
    Modify();

    TunnelPassword = XTunnelPassword;
    Shred(XTunnelPassword);
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTunnelPortNumber(int value)
{
  SET_SESSION_PROPERTY(TunnelPortNumber);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTunnelUserName(UnicodeString value)
{
  // Avoid password recryption (what may popup master password prompt)
  if (FTunnelUserName != value)
  {
    // TunnelUserName is key for password encryption
    UnicodeString XTunnelPassword = TunnelPassword;
    SET_SESSION_PROPERTY(TunnelUserName);
    TunnelPassword = XTunnelPassword;
    Shred(XTunnelPassword);
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTunnelPassword(UnicodeString avalue)
{
  RawByteString value = EncryptPassword(avalue, TunnelUserName+TunnelHostName);
  SET_SESSION_PROPERTY(TunnelPassword);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetTunnelPassword() const
{
  return DecryptPassword(FTunnelPassword, TunnelUserName+TunnelHostName);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTunnelPassphrase(UnicodeString avalue)
{
  RawByteString value = EncryptPassword(avalue, TunnelPublicKeyFile);
  SET_SESSION_PROPERTY(TunnelPassphrase);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetTunnelPassphrase() const
{
  return DecryptPassword(FTunnelPassphrase, TunnelPublicKeyFile);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTunnelPublicKeyFile(UnicodeString value)
{
  if (FTunnelPublicKeyFile != value)
  {
    // TunnelPublicKeyFile is key for TunnelPassphrase encryption
    UnicodeString XTunnelPassphrase = TunnelPassphrase;

    // StripPathQuotes should not be needed as we do not feed quotes anymore
    FTunnelPublicKeyFile = StripPathQuotes(value);
    Modify();

    TunnelPassphrase = XTunnelPassphrase;
    Shred(XTunnelPassphrase);
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTunnelLocalPortNumber(int value)
{
  SET_SESSION_PROPERTY(TunnelLocalPortNumber);
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::GetTunnelAutoassignLocalPortNumber()
{
  return (FTunnelLocalPortNumber <= 0);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTunnelPortFwd(UnicodeString value)
{
  SET_SESSION_PROPERTY(TunnelPortFwd);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTunnelHostKey(UnicodeString value)
{
  SET_SESSION_PROPERTY(TunnelHostKey);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFtpPasvMode(bool value)
{
  SET_SESSION_PROPERTY(FtpPasvMode);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFtpForcePasvIp(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(FtpForcePasvIp);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFtpUseMlsd(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(FtpUseMlsd);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFtpAccount(UnicodeString value)
{
  SET_SESSION_PROPERTY(FtpAccount);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFtpPingInterval(int value)
{
  SET_SESSION_PROPERTY(FtpPingInterval);
}
//---------------------------------------------------------------------------
TDateTime __fastcall TSessionData::GetFtpPingIntervalDT()
{
  return SecToDateTime(FtpPingInterval);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetFtpPingType(TFtpPingType value)
{
  SET_SESSION_PROPERTY(FtpPingType);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetFtpTransferActiveImmediately(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(FtpTransferActiveImmediately);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetFtps(TFtps value)
{
  SET_SESSION_PROPERTY(Ftps);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetMinTlsVersion(TTlsVersion value)
{
  SET_SESSION_PROPERTY(MinTlsVersion);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetMaxTlsVersion(TTlsVersion value)
{
  SET_SESSION_PROPERTY(MaxTlsVersion);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetLogicalHostName(UnicodeString value)
{
  SET_SESSION_PROPERTY(LogicalHostName);
}
//---------------------------------------------------------------------------
void TSessionData::SetCompleteTlsShutdown(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(CompleteTlsShutdown);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFtpListAll(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(FtpListAll);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFtpHost(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(FtpHost);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFtpWorkFromCwd(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(FtpWorkFromCwd);
}
//---------------------------------------------------------------------
void TSessionData::SetFtpAnyCodeForPwd(bool value)
{
  SET_SESSION_PROPERTY(FtpAnyCodeForPwd);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSslSessionReuse(bool value)
{
  SET_SESSION_PROPERTY(SslSessionReuse);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTlsCertificateFile(UnicodeString value)
{
  SET_SESSION_PROPERTY(TlsCertificateFile);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetNotUtf(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(NotUtf);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetInternalEditorEncoding(int value)
{
  SET_SESSION_PROPERTY(InternalEditorEncoding);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetS3DefaultRegion(UnicodeString value)
{
  SET_SESSION_PROPERTY(S3DefaultRegion);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetS3SessionToken(UnicodeString value)
{
  SET_SESSION_PROPERTY(S3SessionToken);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetS3RoleArn(UnicodeString value)
{
  SET_SESSION_PROPERTY(S3RoleArn);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetS3RoleSessionName(UnicodeString value)
{
  SET_SESSION_PROPERTY(S3RoleSessionName);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetS3Profile(UnicodeString value)
{
  SET_SESSION_PROPERTY(S3Profile);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetS3UrlStyle(TS3UrlStyle value)
{
  SET_SESSION_PROPERTY(S3UrlStyle);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetS3MaxKeys(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(S3MaxKeys);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetS3CredentialsEnv(bool value)
{
  SET_SESSION_PROPERTY(S3CredentialsEnv);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetS3RequesterPays(bool value)
{
  SET_SESSION_PROPERTY(S3RequesterPays);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetIsWorkspace(bool value)
{
  SET_SESSION_PROPERTY(IsWorkspace);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetLink(UnicodeString value)
{
  SET_SESSION_PROPERTY(Link);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetNameOverride(UnicodeString value)
{
  SET_SESSION_PROPERTY(NameOverride);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetHostKey(UnicodeString value)
{
  SET_SESSION_PROPERTY(HostKey);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetNote(UnicodeString value)
{
  SET_SESSION_PROPERTY(Note);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetWinTitle(UnicodeString value)
{
  SET_SESSION_PROPERTY(WinTitle);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetEncryptKey() const
{
  return DecryptPassword(FEncryptKey, GetSessionPasswordEncryptionKey());
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetEncryptKey(UnicodeString avalue)
{
  RawByteString value = EncryptPassword(avalue, GetSessionPasswordEncryptionKey());
  SET_SESSION_PROPERTY(EncryptKey);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetWebDavLiberalEscaping(bool value)
{
  SET_SESSION_PROPERTY(WebDavLiberalEscaping);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetWebDavAuthLegacy(bool value)
{
  SET_SESSION_PROPERTY(WebDavAuthLegacy);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetInfoTip()
{
  if (UsesSsh)
  {
    return FMTLOAD(SESSION_INFO_TIP2,
        (HostName, UserName,
         (PublicKeyFile.IsEmpty() ? LoadStr(NO_STR) : LoadStr(YES_STR)),
         FSProtocolStr));
  }
  else
  {
    return FMTLOAD(SESSION_INFO_TIP_NO_SSH,
      (HostName, UserName, FSProtocolStr));
  }
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::ExtractLocalName(const UnicodeString & Name)
{
  UnicodeString Result = Name;
  int P = Result.LastDelimiter(L"/");
  if (P > 0)
  {
    Result.Delete(1, P);
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetLocalName()
{
  UnicodeString Result;
  if (HasSessionName())
  {
    Result = ExtractLocalName(Name);
  }
  else
  {
    Result = DefaultSessionName;
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::ExtractFolderName(const UnicodeString & Name)
{
  UnicodeString Result;
  int P = Name.LastDelimiter(L"/");
  if (P > 0)
  {
    Result = Name.SubString(1, P - 1);
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetFolderName()
{
  UnicodeString Result;
  if (HasSessionName() || IsWorkspace)
  {
    Result = ExtractFolderName(Name);
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::ComposePath(
  const UnicodeString & Path, const UnicodeString & Name)
{
  return UnixIncludeTrailingBackslash(Path) + Name;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::DisableAuthentationsExceptPassword()
{
  SshNoUserAuth = false;
  AuthKI = false;
  AuthKIPassword = false;
  AuthGSSAPI = false;
  AuthGSSAPIKEX = false;
  PublicKeyFile = EmptyStr;
  DetachedCertificate = EmptyStr;
  TlsCertificateFile = EmptyStr;
  Passphrase = EmptyStr;
  TryAgent = false;
}
//---------------------------------------------------------------------
TStrings * TSessionData::GetAllOptionNames(bool PuttyExport)
{
  std::unique_ptr<TSessionData> FactoryDefaults(new TSessionData(L""));
  return FactoryDefaults->SaveToOptions(NULL, false, PuttyExport);
}
//---------------------------------------------------------------------
bool TSessionData::HasS3AutoCredentials()
{
  return (FSProtocol == fsS3) && S3CredentialsEnv;
}
//---------------------------------------------------------------------
bool TSessionData::HasAutoCredentials()
{
  return HasS3AutoCredentials();
}
//=== TStoredSessionList ----------------------------------------------
__fastcall TStoredSessionList::TStoredSessionList(bool aReadOnly):
  TNamedObjectList(), FReadOnly(aReadOnly)
{
  DebugAssert(Configuration);
  FDefaultSettings = new TSessionData(DefaultName);
  FPendingRemovals.reset(new TStringList());
}
//---------------------------------------------------------------------
__fastcall TStoredSessionList::~TStoredSessionList()
{
  DebugAssert(Configuration);
  delete FDefaultSettings;
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Load(THierarchicalStorage * Storage,
  bool AsModified, bool UseDefaults, bool PuttyImport)
{
  TStringList *SubKeys = new TStringList();
  TList * Loaded = new TList;
  try
  {
    DebugAssert(AutoSort);
    AutoSort = false;
    bool WasEmpty = (Count == 0);

    Storage->GetSubKeyNames(SubKeys);

    for (int Index = 0; Index < SubKeys->Count; Index++)
    {
      UnicodeString SessionName = SubKeys->Strings[Index];

      bool ValidName = true;
      try
      {
        TSessionData::ValidatePath(SessionName);
      }
      catch(...)
      {
        ValidName = false;
      }

      if (ValidName)
      {
        TSessionData * SessionData;
        if (SessionName == FDefaultSettings->Name)
        {
          SessionData = FDefaultSettings;
        }
        else
        {
          // if the list was empty before loading, do not waste time trying to
          // find existing sites to overwrite (we rely on underlying storage
          // to secure uniqueness of the key names)
          if (WasEmpty)
          {
            SessionData = NULL;
          }
          else
          {
            SessionData = (TSessionData*)FindByName(SessionName);
          }
        }

        if ((SessionData != FDefaultSettings) || !UseDefaults)
        {
          if (SessionData == NULL)
          {
            SessionData = new TSessionData(L"");
            if (UseDefaults)
            {
              SessionData->CopyData(DefaultSettings);
            }
            SessionData->Name = SessionName;
            Add(SessionData);
          }
          Loaded->Add(SessionData);
          SessionData->Load(Storage, PuttyImport);
          if (AsModified)
          {
            SessionData->Modified = true;
          }
        }
      }
    }

    if (!AsModified)
    {
      for (int Index = 0; Index < TObjectList::Count; Index++)
      {
        if (Loaded->IndexOf(Items[Index]) < 0)
        {
          Delete(Index);
          Index--;
        }
      }
    }
  }
  __finally
  {
    AutoSort = true;
    AlphaSort();
    delete SubKeys;
    delete Loaded;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Reload()
{
  if (Count <= Configuration->DontReloadMoreThanSessions)
  {
    bool SessionList = true;
    std::unique_ptr<THierarchicalStorage> Storage(Configuration->CreateScpStorage(SessionList));
    if (Storage->OpenSubKey(Configuration->StoredSessionsSubKey, False))
    {
      Load(Storage.get());
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::DoSave(THierarchicalStorage * Storage,
  TSessionData * Data, bool All, bool RecryptPasswordOnly,
  TSessionData * FactoryDefaults)
{
  if (All || Data->Modified)
  {
    if (RecryptPasswordOnly)
    {
      Data->SaveRecryptedPasswords(Storage);
    }
    else
    {
      Data->Save(Storage, false, FactoryDefaults);
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::DoSave(THierarchicalStorage * Storage,
  bool All, bool RecryptPasswordOnly, TStrings * RecryptPasswordErrors)
{
  TSessionData * FactoryDefaults = new TSessionData(L"");
  try
  {
    while (FPendingRemovals->Count > 0)
    {
      TSessionData::Remove(Storage, FPendingRemovals->Strings[0]);
      FPendingRemovals->Delete(0);
    }

    DoSave(Storage, FDefaultSettings, All, RecryptPasswordOnly, FactoryDefaults);
    for (int Index = 0; Index < CountIncludingHidden; Index++)
    {
      TSessionData * SessionData = (TSessionData *)Items[Index];
      try
      {
        DoSave(Storage, SessionData, All, RecryptPasswordOnly, FactoryDefaults);
      }
      catch (Exception & E)
      {
        UnicodeString Message;
        if (RecryptPasswordOnly && DebugAlwaysTrue(RecryptPasswordErrors != NULL) &&
            ExceptionMessage(&E, Message))
        {
          RecryptPasswordErrors->Add(FORMAT("%s: %s", (SessionData->SessionName, Message)));
        }
        else
        {
          throw;
        }
      }
    }
  }
  __finally
  {
    delete FactoryDefaults;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Save(THierarchicalStorage * Storage, bool All)
{
  DoSave(Storage, All, false, NULL);
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::DoSave(bool All, bool Explicit,
  bool RecryptPasswordOnly, TStrings * RecryptPasswordErrors)
{
  bool SessionList = true;
  THierarchicalStorage * Storage = Configuration->CreateScpStorage(SessionList);
  try
  {
    Storage->AccessMode = smReadWrite;
    Storage->Explicit = Explicit;
    if (Storage->OpenSubKey(Configuration->StoredSessionsSubKey, true))
    {
      DoSave(Storage, All, RecryptPasswordOnly, RecryptPasswordErrors);
    }
  }
  __finally
  {
    delete Storage;
  }

  Saved();
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Save(bool All, bool Explicit)
{
  DoSave(All, Explicit, false, NULL);
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::RecryptPasswords(TStrings * RecryptPasswordErrors)
{
  DoSave(true, true, true, RecryptPasswordErrors);
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Saved()
{
  FDefaultSettings->Modified = false;
  for (int Index = 0; Index < CountIncludingHidden; Index++)
  {
    ((TSessionData *)Items[Index])->Modified = false;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::ImportLevelFromFilezilla(
  _di_IXMLNode Node, const UnicodeString & Path, _di_IXMLNode SettingsNode)
{
  for (int Index = 0; Index < Node->ChildNodes->Count; Index++)
  {
    _di_IXMLNode ChildNode = Node->ChildNodes->Get(Index);
    if (ChildNode->NodeName == L"Server")
    {
      std::unique_ptr<TSessionData> SessionData(new TSessionData(L""));
      SessionData->CopyData(DefaultSettings);
      SessionData->ImportFromFilezilla(ChildNode, Path, SettingsNode);
      Add(SessionData.release());
    }
    else if (ChildNode->NodeName == L"Folder")
    {
      UnicodeString Name;

      for (int Index = 0; Index < ChildNode->ChildNodes->Count; Index++)
      {
        _di_IXMLNode PossibleTextMode = ChildNode->ChildNodes->Get(Index);
        if (PossibleTextMode->NodeType == ntText)
        {
          UnicodeString NodeValue = PossibleTextMode->NodeValue;
          AddToList(Name, NodeValue.Trim(), L" ");
        }
      }

      Name = TSessionData::MakeValidName(Name).Trim();

      ImportLevelFromFilezilla(ChildNode, TSessionData::ComposePath(Path, Name), SettingsNode);
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::ImportFromFilezilla(
  const UnicodeString FileName, const UnicodeString ConfigurationFileName)
{

  // not sure if the document must exists if we want to use its node
  _di_IXMLDocument ConfigurationDocument;
  _di_IXMLNode SettingsNode;

  if (FileExists(ApiPath(ConfigurationFileName)))
  {
    ConfigurationDocument = interface_cast<Xmlintf::IXMLDocument>(new TXMLDocument(NULL));
    ConfigurationDocument->LoadFromFile(ConfigurationFileName);
    _di_IXMLNode FileZilla3Node = ConfigurationDocument->ChildNodes->FindNode(L"FileZilla3");
    if (FileZilla3Node != NULL)
    {
      SettingsNode = FileZilla3Node->ChildNodes->FindNode(L"Settings");
    }
  }

  const _di_IXMLDocument Document = interface_cast<Xmlintf::IXMLDocument>(new TXMLDocument(NULL));
  Document->LoadFromFile(FileName);
  _di_IXMLNode FileZilla3Node = Document->ChildNodes->FindNode(L"FileZilla3");
  if (FileZilla3Node != NULL)
  {
    _di_IXMLNode ServersNode = FileZilla3Node->ChildNodes->FindNode(L"Servers");
    if (ServersNode != NULL)
    {
      ImportLevelFromFilezilla(ServersNode, L"", SettingsNode);
    }
  }
}
//---------------------------------------------------------------------
UnicodeString FormatKnownHostName(const UnicodeString & HostName, int PortNumber)
{
  return FORMAT(L"%s:%d", (HostName, PortNumber));
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::ImportFromKnownHosts(TStrings * Lines)
{
  bool SessionList = false;
  std::unique_ptr<THierarchicalStorage> HostKeyStorage(Configuration->CreateScpStorage(SessionList));
  std::unique_ptr<TStrings> KeyList(new TStringList());
  if (OpenHostKeysSubKey(HostKeyStorage.get(), false))
  {
    HostKeyStorage->GetValueNames(KeyList.get());
  }
  HostKeyStorage.reset(NULL);

  UnicodeString FirstError;
  for (int Index = 0; Index < Lines->Count; Index++)
  {
    try
    {
      UnicodeString Line = Lines->Strings[Index];
      Line = Trim(Line);
      if (IsValidOpensshLine(Line))
      {
        int P = Pos(L' ', Line);
        if (P > 0)
        {
          UnicodeString HostNameStr = Line.SubString(1, P - 1);
          Line = Line.SubString(P + 1, Line.Length() - P);

          P = Pos(L',', HostNameStr);
          if (P > 0)
          {
            HostNameStr.SetLength(P - 1);
          }
          P = Pos(L':', HostNameStr);
          int PortNumber = -1;
          if (P > 0)
          {
            UnicodeString PortNumberStr = HostNameStr.SubString(P + 1, HostNameStr.Length() - P);
            PortNumber = StrToInt(PortNumberStr);
            HostNameStr.SetLength(P - 1);
          }
          if (HasIP6LiteralBrackets(HostNameStr))
          {
            HostNameStr = StripIP6LiteralBrackets(HostNameStr);
          }

          UnicodeString NameStr = HostNameStr;
          if (PortNumber >= 0)
          {
            NameStr = FormatKnownHostName(NameStr, PortNumber);
          }

          std::unique_ptr<TSessionData> SessionDataOwner;
          TSessionData * SessionData = dynamic_cast<TSessionData *>(FindByName(NameStr));
          if (SessionData == NULL)
          {
            SessionData = new TSessionData(L"");
            SessionDataOwner.reset(SessionData);
            SessionData->CopyData(DefaultSettings);
            SessionData->Name = NameStr;
            SessionData->HostName = HostNameStr;
            if (PortNumber >= 0)
            {
              SessionData->PortNumber = PortNumber;
            }
          }

          const struct ssh_keyalg * Algorithm;
          UnicodeString Key = ParseOpenSshPubLine(Line, Algorithm);
          UnicodeString KeyKey =
            FORMAT(L"%s@%d:%s", (Algorithm->cache_id, SessionData->PortNumber, HostNameStr));
          UnicodeString HostKey =
            FORMAT(L"%s:%s=%s", (Algorithm->ssh_id, KeyKey, Key));
          UnicodeString HostKeyList = SessionData->HostKey;
          AddToList(HostKeyList, HostKey, L";");
          SessionData->HostKey = HostKeyList;
          // If there's at least one unknown key type for this host, select it
          if (KeyList->IndexOf(KeyKey) < 0)
          {
            SessionData->Selected = true;
          }

          if (SessionDataOwner.get() != NULL)
          {
            Add(SessionDataOwner.release());
          }
        }
      }
    }
    catch (Exception & E)
    {
      if (FirstError.IsEmpty())
      {
        FirstError = E.Message;
      }
    }
  }

  if (Count == 0)
  {
    UnicodeString Message = LoadStr(KNOWN_HOSTS_NO_SITES);
    if (!FirstError.IsEmpty())
    {
      Message = FORMAT(L"%s\n(%s)", (Message, FirstError));
    }

    throw Exception(Message);
  }
}
//---------------------------------------------------------------------
void TStoredSessionList::ImportFromOpenssh(TStrings * Lines)
{
  std::unique_ptr<TStrings> Hosts(CreateSortedStringList());
  for (int Index = 0; Index < Lines->Count; Index++)
  {
    UnicodeString Line = Lines->Strings[Index];
    UnicodeString Directive, Value;
    if (ParseOpensshDirective(Line, Directive, Value))
    {
      if (SameText(Directive, OpensshHostDirective))
      {
        while (!Value.IsEmpty())
        {
          UnicodeString Name = CutOpensshToken(Value);
          if ((Hosts->IndexOf(Name) < 0) && (Name.LastDelimiter(L"*?") == 0))
          {
            std::unique_ptr<TSessionData> Data(new TSessionData(EmptyStr));
            Data->CopyData(DefaultSettings);
            Data->Name = Name;
            Data->HostName = Name;
            Data->ImportFromOpenssh(Lines);
            Add(Data.release());
            Hosts->Add(Name);
          }
        }
      }
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Export(const UnicodeString FileName)
{
  THierarchicalStorage * Storage = TIniFileStorage::CreateFromPath(FileName);
  try
  {
    Storage->AccessMode = smReadWrite;
    if (Storage->OpenSubKey(Configuration->StoredSessionsSubKey, true))
    {
      Save(Storage, true);
    }
  }
  __finally
  {
    delete Storage;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::SelectAll(bool Select)
{
  for (int Index = 0; Index < Count; Index++)
    Sessions[Index]->Selected = Select;
}
//---------------------------------------------------------------------
bool TStoredSessionList::Import(TStoredSessionList * From,
  bool OnlySelected, TList * Imported)
{
  bool Result = false;
  for (int Index = 0; Index < From->Count; Index++)
  {
    if (!OnlySelected || From->Sessions[Index]->Selected)
    {
      TSessionData *Session = new TSessionData(L"");
      Session->Assign(From->Sessions[Index]);
      Session->Modified = true;
      Session->MakeUniqueIn(this);
      Add(Session);
      Result = true;
      if (Imported != NULL)
      {
        Imported->Add(Session);
      }
    }
  }
  // only modified, explicit
  Save(false, true);
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::SelectSessionsToImport
  (TStoredSessionList * Dest, bool SSHOnly)
{
  for (int Index = 0; Index < Count; Index++)
  {
    Sessions[Index]->Selected =
      (!SSHOnly || (Sessions[Index]->GetNormalizedPuttyProtocol() == PuttySshProtocol)) &&
      !Dest->FindByName(Sessions[Index]->Name);
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Cleanup()
{
  try
  {
    if (Configuration->Storage == stRegistry) Clear();
    TRegistryStorage * Storage = new TRegistryStorage(Configuration->RegistryStorageKey);
    try
    {
      Storage->AccessMode = smReadWrite;
      if (Storage->OpenRootKey(False))
        Storage->RecursiveDeleteSubKey(Configuration->StoredSessionsSubKey);
    }
    __finally
    {
      delete Storage;
    }
  }
  catch (Exception &E)
  {
    throw ExtException(&E, LoadStr(CLEANUP_SESSIONS_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TStoredSessionList::UpdateStaticUsage()
{
  int SCP = 0;
  int SFTP = 0;
  int FTP = 0;
  int FTPS = 0;
  int WebDAV = 0;
  int WebDAVS = 0;
  int S3 = 0;
  int Password = 0;
  int Advanced = 0;
  int Color = 0;
  int Note = 0;
  int Tunnel = 0;
  bool Folders = false;
  bool Workspaces = false;
  std::unique_ptr<TSessionData> FactoryDefaults(new TSessionData(L""));
  std::unique_ptr<TStringList> DifferentAdvancedProperties(CreateSortedStringList());
  for (int Index = 0; Index < Count; Index++)
  {
    TSessionData * Data = Sessions[Index];
    if (Data->IsWorkspace)
    {
      Workspaces = true;
    }
    else
    {
      switch (Data->FSProtocol)
      {
        case fsSCPonly:
          SCP++;
          break;

        case fsSFTP:
        case fsSFTPonly:
          SFTP++;
          break;

        case fsFTP:
          if (Data->Ftps == ftpsNone)
          {
            FTP++;
          }
          else
          {
            FTPS++;
          }
          break;

        case fsWebDAV:
          if (Data->Ftps == ftpsNone)
          {
            WebDAV++;
          }
          else
          {
            WebDAVS++;
          }
          break;

        case fsS3:
          S3++;
          break;
      }

      if (Data->HasAnySessionPassword())
      {
        Password++;
      }

      if (Data->Color != 0)
      {
        Color++;
      }

      if (!Data->Note.IsEmpty())
      {
        Note++;
      }

      // This would not work for passwords, as they are compared in their encrypted form.
      // But there are no passwords set in factory defaults anyway.
      if (!Data->IsSame(FactoryDefaults.get(), true, DifferentAdvancedProperties.get(), false))
      {
        Advanced++;
      }

      if (Data->Tunnel)
      {
        Tunnel++;
      }

      if (!Data->FolderName.IsEmpty())
      {
        Folders = true;
      }
    }
  }

  Configuration->Usage->Set(L"StoredSessionsCountSCP", SCP);
  Configuration->Usage->Set(L"StoredSessionsCountSFTP", SFTP);
  Configuration->Usage->Set(L"StoredSessionsCountFTP", FTP);
  Configuration->Usage->Set(L"StoredSessionsCountFTPS", FTPS);
  Configuration->Usage->Set(L"StoredSessionsCountWebDAV", WebDAV);
  Configuration->Usage->Set(L"StoredSessionsCountWebDAVS", WebDAVS);
  Configuration->Usage->Set(L"StoredSessionsCountS3", S3);
  Configuration->Usage->Set(L"StoredSessionsCountPassword", Password);
  Configuration->Usage->Set(L"StoredSessionsCountColor", Color);
  Configuration->Usage->Set(L"StoredSessionsCountNote", Note);
  Configuration->Usage->Set(L"StoredSessionsCountAdvanced", Advanced);
  DifferentAdvancedProperties->Delimiter = L',';
  Configuration->Usage->Set(L"StoredSessionsAdvancedSettings", DifferentAdvancedProperties->DelimitedText);
  Configuration->Usage->Set(L"StoredSessionsCountTunnel", Tunnel);

  // actually default might be true, see below for when the default is actually used
  bool CustomDefaultStoredSession = false;
  try
  {
    // this can throw, when the default session settings have password set
    // (and no other basic property, like hostname/username),
    // and master password is enabled as we are called before master password
    // handler is set
    CustomDefaultStoredSession = !FDefaultSettings->IsSame(FactoryDefaults.get(), false);
  }
  catch (...)
  {
  }
  Configuration->Usage->Set(L"UsingDefaultStoredSession", CustomDefaultStoredSession);

  Configuration->Usage->Set(L"UsingStoredSessionsFolders", Folders);
  Configuration->Usage->Set(L"UsingWorkspaces", Workspaces);
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TStoredSessionList::FindSame(TSessionData * Data)
{
  TSessionData * Result;
  if (Data->Hidden || Data->Name.IsEmpty() || Data->IsWorkspace)
  {
    Result = NULL;
  }
  else
  {
    Result = dynamic_cast<TSessionData *>(FindByName(Data->Name));
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TStoredSessionList::IndexOf(TSessionData * Data)
{
  for (int Index = 0; Index < Count; Index++)
    if (Data == Sessions[Index]) return Index;
  return -1;
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TStoredSessionList::NewSession(
  UnicodeString SessionName, TSessionData * Session)
{
  TSessionData * DuplicateSession = (TSessionData*)FindByName(SessionName);
  if (!DuplicateSession)
  {
    DuplicateSession = new TSessionData(L"");
    DuplicateSession->Assign(Session);
    DuplicateSession->Name = SessionName;
    // make sure, that new stored session is saved to registry
    DuplicateSession->Modified = true;
    Add(DuplicateSession);
  }
  else
  {
    DuplicateSession->Assign(Session);
    DuplicateSession->Name = SessionName;
    DuplicateSession->Modified = true;
  }
  // list was saved here before to default storage, but it would not allow
  // to work with special lists (export/import) not using default storage
  return DuplicateSession;
}
//---------------------------------------------------------------------------
void __fastcall TStoredSessionList::SetDefaultSettings(TSessionData * value)
{
  DebugAssert(FDefaultSettings);
  if (FDefaultSettings != value)
  {
    FDefaultSettings->Assign(value);
    // make sure default settings are saved
    FDefaultSettings->Modified = true;
    FDefaultSettings->Name = DefaultName;
    if (!FReadOnly)
    {
      // only modified, explicit
      Save(false, true);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TStoredSessionList::OpenHostKeysSubKey(THierarchicalStorage * Storage, bool CanCreate)
{
  return Storage->OpenSubKey(Configuration->SshHostKeysSubKey, CanCreate);
}
//---------------------------------------------------------------------------
THierarchicalStorage * __fastcall TStoredSessionList::CreateHostKeysStorageForWriting()
{
  bool SessionList = false;
  std::unique_ptr<THierarchicalStorage> Storage(Configuration->CreateScpStorage(SessionList));
  Storage->Explicit = true;
  Storage->AccessMode = smReadWrite;
  return Storage.release();
}
//---------------------------------------------------------------------------
int TStoredSessionList::ImportHostKeys(
  THierarchicalStorage * SourceStorage, THierarchicalStorage * TargetStorage, TStoredSessionList * Sessions, bool OnlySelected)
{
  int Result = 0;
  if (OpenHostKeysSubKey(SourceStorage, false) &&
      OpenHostKeysSubKey(TargetStorage, true))
  {
    std::unique_ptr<TStringList> KeyList(new TStringList());
    SourceStorage->GetValueNames(KeyList.get());

    DebugAssert(Sessions != NULL);
    for (int Index = 0; Index < Sessions->Count; Index++)
    {
      TSessionData * Session = Sessions->Sessions[Index];
      if (!OnlySelected || Session->Selected)
      {
        UnicodeString HostKeyName = PuttyMungeStr(FORMAT(L"@%d:%s", (Session->PortNumber, Session->HostNameExpanded)));
        for (int KeyIndex = 0; KeyIndex < KeyList->Count; KeyIndex++)
        {
          UnicodeString KeyName = KeyList->Strings[KeyIndex];
          if (EndsText(HostKeyName, KeyName))
          {
            TargetStorage->WriteStringRaw(KeyName, SourceStorage->ReadStringRaw(KeyName, L""));
            Result++;
          }
        }
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void TStoredSessionList::ImportHostKeys(
  THierarchicalStorage * SourceStorage, TStoredSessionList * Sessions, bool OnlySelected)
{
  std::unique_ptr<THierarchicalStorage> TargetStorage(CreateHostKeysStorageForWriting());

  ImportHostKeys(SourceStorage, TargetStorage.get(), Sessions, OnlySelected);
}
//---------------------------------------------------------------------------
void TStoredSessionList::ImportHostKeys(
  const UnicodeString & SourceKey, TStoredSessionList * Sessions, bool OnlySelected)
{
  std::unique_ptr<THierarchicalStorage> SourceStorage(new TRegistryStorage(SourceKey));

  ImportHostKeys(SourceStorage.get(), Sessions, OnlySelected);
}
//---------------------------------------------------------------------------
void __fastcall TStoredSessionList::ImportSelectedKnownHosts(TStoredSessionList * Sessions)
{
  std::unique_ptr<THierarchicalStorage> Storage(CreateHostKeysStorageForWriting());
  if (OpenHostKeysSubKey(Storage.get(), true))
  {
    for (int Index = 0; Index < Sessions->Count; Index++)
    {
      TSessionData * Session = Sessions->Sessions[Index];
      if (Session->Selected)
      {
        UnicodeString Algs;
        UnicodeString HostKeys = Session->HostKey;
        while (!HostKeys.IsEmpty())
        {
          UnicodeString HostKey = CutToChar(HostKeys, L';', true);
          // skip alg
          CutToChar(HostKey, L':', true);
          UnicodeString Key = CutToChar(HostKey, L'=', true);
          Storage->WriteStringRaw(Key, HostKey);
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void TStoredSessionList::SelectKnownHostsForSelectedSessions(
  TStoredSessionList * KnownHosts, TStoredSessionList * Sessions)
{
  for (int SessionIndex = 0; SessionIndex < Sessions->Count; SessionIndex++)
  {
    TSessionData * Session = Sessions->Sessions[SessionIndex];
    if (Session->Selected)
    {
      UnicodeString Key = Session->HostName;
      if (Session->PortNumber != Session->GetDefaultPort())
      {
        Key = FormatKnownHostName(Key, Session->PortNumber);
      }
      TSessionData * KnownHost = dynamic_cast<TSessionData *>(KnownHosts->FindByName(Key));
      if (KnownHost != NULL)
      {
        KnownHost->Selected = true;
      }
    }
  }
}
//---------------------------------------------------------------------------
TSessionData * TStoredSessionList::GetFirstFolderOrWorkspaceSession(const UnicodeString & Name)
{
  TSessionData * Result = NULL;
  if (!Name.IsEmpty())
  {
    UnicodeString NameWithSlash = UnixIncludeTrailingBackslash(Name); // optimization
    for (int Index = 0; (Result == NULL) && (Index < Count); Index++)
    {
      if (Sessions[Index]->IsInFolderOrWorkspace(NameWithSlash))
      {
        Result = Sessions[Index];
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TStoredSessionList::IsFolderOrWorkspace(const UnicodeString & Name)
{
  return (GetFirstFolderOrWorkspaceSession(Name) != NULL);
}
//---------------------------------------------------------------------------
bool __fastcall TStoredSessionList::IsFolder(const UnicodeString & Name)
{
  TSessionData * SessionData = GetFirstFolderOrWorkspaceSession(Name);
  return (SessionData != NULL) && !SessionData->IsWorkspace;
}
//---------------------------------------------------------------------------
bool __fastcall TStoredSessionList::IsWorkspace(const UnicodeString & Name)
{
  TSessionData * SessionData = GetFirstFolderOrWorkspaceSession(Name);
  return (SessionData != NULL) && SessionData->IsWorkspace;
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TStoredSessionList::CheckIsInFolderOrWorkspaceAndResolve(
  TSessionData * Data, const UnicodeString & Name)
{
  if (Data->IsInFolderOrWorkspace(Name))
  {
    Data = ResolveWorkspaceData(Data);

    if ((Data != NULL) && Data->CanOpen &&
        DebugAlwaysTrue(Data->Link.IsEmpty()))
    {
      return Data;
    }
  }
  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TStoredSessionList::GetFolderOrWorkspace(const UnicodeString & Name, TList * List)
{
  DoGetFolderOrWorkspace(Name, List, false);
}
//---------------------------------------------------------------------------
void __fastcall TStoredSessionList::DoGetFolderOrWorkspace(const UnicodeString & Name, TList * List, bool NoRecrypt)
{
  for (int Index = 0; (Index < Count); Index++)
  {
    TSessionData * RawData = Sessions[Index];
    TSessionData * Data =
      CheckIsInFolderOrWorkspaceAndResolve(RawData, Name);

    if (Data != NULL)
    {
      TSessionData * Data2 = new TSessionData(L"");
      if (NoRecrypt)
      {
        Data2->CopyDataNoRecrypt(Data);
      }
      else
      {
        Data2->Assign(Data);
      }

      if (!RawData->Link.IsEmpty() && (DebugAlwaysTrue(Data != RawData)) &&
          // BACKWARD COMPATIBILITY
          // When loading pre-5.6.4 workspace, that does not have state saved,
          // do not overwrite the site "state" defaults
          // with (empty) workspace state
          RawData->HasStateData())
      {
        Data2->CopyStateData(RawData);
      }

      if (!RawData->NameOverride.IsEmpty())
      {
        Data2->Name = RawData->NameOverride;
      }
      else if (RawData->Link.IsEmpty() && RawData->IsWorkspace)
      {
        // Newly opened ad-hoc session has no name, so restore the workspace that way too.
        // Otherwise we would persist the generated internal workspace name as a real name.
        Data2->Name = UnicodeString();
      }

      List->Add(Data2);
    }
  }
}
//---------------------------------------------------------------------------
TStrings * __fastcall TStoredSessionList::GetFolderOrWorkspaceList(
  const UnicodeString & Name)
{
  std::unique_ptr<TObjectList> DataList(new TObjectList());
  DoGetFolderOrWorkspace(Name, DataList.get(), true);

  std::unique_ptr<TStringList> Result(new TStringList());
  for (int Index = 0; (Index < DataList->Count); Index++)
  {
    Result->Add(dynamic_cast<TSessionData *>(DataList->Items[Index])->SessionName);
  }

  return Result.release();
}
//---------------------------------------------------------------------------
TStrings * __fastcall TStoredSessionList::GetWorkspaces()
{
  std::unique_ptr<TStringList> Result(CreateSortedStringList());

  for (int Index = 0; (Index < Count); Index++)
  {
    TSessionData * Data = Sessions[Index];
    if (Data->IsWorkspace)
    {
      Result->Add(Data->FolderName);
    }
  }

  return Result.release();
}
//---------------------------------------------------------------------------
void __fastcall TStoredSessionList::NewWorkspace(
  UnicodeString Name, TList * DataList)
{
  for (int Index = 0; (Index < Count); Index++)
  {
    TSessionData * Data = Sessions[Index];
    if (Data->IsInFolderOrWorkspace(Name))
    {
      FPendingRemovals->Add(Data->Name);
      Remove(Data);
      Index--;
    }
  }

  for (int Index = 0; (Index < DataList->Count); Index++)
  {
    TSessionData * Data = static_cast<TSessionData *>(DataList->Items[Index]);

    TSessionData * Data2 = new TSessionData(L"");
    Data2->Assign(Data);
    Data2->Name = TSessionData::ComposePath(Name, Data->Name);
    // make sure, that new stored session is saved to registry
    Data2->Modified = true;
    Add(Data2);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TStoredSessionList::HasAnyWorkspace()
{
  bool Result = false;
  for (int Index = 0; !Result && (Index < Count); Index++)
  {
    TSessionData * Data = Sessions[Index];
    Result = Data->IsWorkspace;
  }
  return Result;
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TStoredSessionList::ParseUrl(UnicodeString Url,
  TOptions * Options, bool & DefaultsOnly, UnicodeString * FileName,
  bool * AProtocolDefined, UnicodeString * MaskedUrl, int Flags)
{
  TSessionData * Data = new TSessionData(L"");
  try
  {
    Data->ParseUrl(Url, Options, this, DefaultsOnly, FileName, AProtocolDefined, MaskedUrl, Flags);
  }
  catch(...)
  {
    delete Data;
    throw;
  }

  return Data;
}
//---------------------------------------------------------------------
bool __fastcall TStoredSessionList::IsUrl(UnicodeString Url)
{
  bool DefaultsOnly;
  bool ProtocolDefined = false;
  std::unique_ptr<TSessionData> ParsedData(ParseUrl(Url, NULL, DefaultsOnly, NULL, &ProtocolDefined));
  bool Result = ProtocolDefined;
  return Result;
}
//---------------------------------------------------------------------
TSessionData * __fastcall TStoredSessionList::ResolveWorkspaceData(TSessionData * Data)
{
  if (!Data->Link.IsEmpty())
  {
    Data = dynamic_cast<TSessionData *>(FindByName(Data->Link));
    if (Data != NULL)
    {
      Data = ResolveWorkspaceData(Data);
    }
  }
  return Data;
}
//---------------------------------------------------------------------
TSessionData * __fastcall TStoredSessionList::SaveWorkspaceData(TSessionData * Data, int Index)
{
  std::unique_ptr<TSessionData> Result(new TSessionData(L""));

  TSessionData * SameData = StoredSessions->FindSame(Data);
  if (SameData != NULL)
  {
    Result->CopyStateData(Data);
    Result->Link = Data->Name;
  }
  else
  {
    Result->Assign(Data);
    Result->NameOverride = Data->Name;
  }

  Result->IsWorkspace = true;
  Result->Name = IntToHex(Index, 4); // See HasSessionName()

  return Result.release();
}
//---------------------------------------------------------------------
bool __fastcall TStoredSessionList::CanOpen(TSessionData * Data)
{
  Data = ResolveWorkspaceData(Data);
  return (Data != NULL) && Data->CanOpen;
}
//---------------------------------------------------------------------
UnicodeString GetExpandedLogFileName(UnicodeString LogFileName, TDateTime Started, TSessionData * SessionData)
{
  // StripPathQuotes should not be needed as we do not feed quotes anymore
  UnicodeString ANewFileName = StripPathQuotes(ExpandEnvironmentVariables(LogFileName));
  for (int Index = 1; Index < ANewFileName.Length(); Index++)
  {
    if (ANewFileName[Index] == L'!')
    {
      UnicodeString Replacement;
      // keep consistent with TFileCustomCommand::PatternReplacement
      switch (towlower(ANewFileName[Index + 1]))
      {
        case L'y':
          Replacement = FormatDateTime(L"yyyy", Started);
          break;

        case L'm':
          Replacement = FormatDateTime(L"mm", Started);
          break;

        case L'd':
          Replacement = FormatDateTime(L"dd", Started);
          break;

        case L't':
          Replacement = FormatDateTime(L"hhnnss", Started);
          break;

        case 'p':
          Replacement = IntToStr(static_cast<int>(GetCurrentProcessId()));
          break;

        case L'@':
          if (SessionData != NULL)
          {
            Replacement = MakeValidFileName(SessionData->HostNameExpanded);
          }
          else
          {
            Replacement = L"nohost";
          }
          break;

        case L's':
          if (SessionData != NULL)
          {
            Replacement = MakeValidFileName(SessionData->SessionName);
          }
          else
          {
            Replacement = L"nosession";
          }
          break;

        case L'!':
          Replacement = L"!";
          break;

        default:
          Replacement = UnicodeString(L"!") + ANewFileName[Index + 1];
          break;
      }
      ANewFileName.Delete(Index, 2);
      ANewFileName.Insert(Replacement, Index);
      Index += Replacement.Length() - 1;
    }
  }
  return ANewFileName;
}
//---------------------------------------------------------------------
bool __fastcall IsSshProtocol(TFSProtocol FSProtocol)
{
  return
    (FSProtocol == fsSFTPonly) || (FSProtocol == fsSFTP) ||
    (FSProtocol == fsSCPonly);
}
//---------------------------------------------------------------------------
int __fastcall DefaultPort(TFSProtocol FSProtocol, TFtps Ftps)
{
  int Result;
  switch (FSProtocol)
  {
    case fsFTP:
      if (Ftps == ftpsImplicit)
      {
        Result = FtpsImplicitPortNumber;
      }
      else
      {
        Result = FtpPortNumber;
      }
      break;

    case fsWebDAV:
    case fsS3:
      if (Ftps == ftpsNone)
      {
        Result = HTTPPortNumber;
      }
      else
      {
        Result = HTTPSPortNumber;
      }
      break;

    default:
      if (IsSshProtocol(FSProtocol))
      {
        Result = SshPortNumber;
      }
      else
      {
        DebugFail();
        Result = -1;
      }
      break;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString GetTlsVersionName(TTlsVersion TlsVersion)
{
  switch (TlsVersion)
  {
    default:
      DebugFail();
    case ssl2:
    case ssl3:
    case tls10:
      return "TLSv1.0";
    case tls11:
      return "TLSv1.1";
    case tls12:
      return "TLSv1.2";
    case tls13:
      return "TLSv1.3";
  }
}
//---------------------------------------------------------------------
