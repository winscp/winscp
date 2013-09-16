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
#include <StrUtils.hpp>
#include <XMLDoc.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
enum TProxyType { pxNone, pxHTTP, pxSocks, pxTelnet }; // 0.53b and older
const wchar_t * DefaultName = L"Default Settings";
const wchar_t CipherNames[CIPHER_COUNT][10] = {L"WARN", L"3des", L"blowfish", L"aes", L"des", L"arcfour"};
const wchar_t KexNames[KEX_COUNT][20] = {L"WARN", L"dh-group1-sha1", L"dh-group14-sha1", L"dh-gex-sha1", L"rsa" };
const wchar_t SshProtList[][10] = {L"1 only", L"1", L"2", L"2 only"};
const wchar_t ProxyMethodList[][10] = {L"none", L"SOCKS4", L"SOCKS5", L"HTTP", L"Telnet", L"Cmd" };
const TCipher DefaultCipherList[CIPHER_COUNT] =
  { cipAES, cipBlowfish, cip3DES, cipWarn, cipArcfour, cipDES };
const TKex DefaultKexList[KEX_COUNT] =
  { kexDHGEx, kexDHGroup14, kexDHGroup1, kexRSA, kexWarn };
const wchar_t FSProtocolNames[FSPROTOCOL_COUNT][16] = { L"SCP", L"SFTP (SCP)", L"SFTP", L"", L"", L"FTP", L"WebDAV" };
const int SshPortNumber = 22;
const int FtpPortNumber = 21;
const int FtpsImplicitPortNumber = 990;
const int HTTPPortNumber = 80;
const int HTTPSPortNumber = 443;
const int TelnetPortNumber = 23;
const int DefaultSendBuf = 262144;
const UnicodeString AnonymousUserName(L"anonymous");
const UnicodeString AnonymousPassword(L"anonymous@example.com");
const UnicodeString PuttySshProtocol(L"ssh");
const UnicodeString PuttyTelnetProtocol(L"telnet");
//---------------------------------------------------------------------
TDateTime __fastcall SecToDateTime(int Sec)
{
  return TDateTime((unsigned short)(Sec/SecsPerHour),
    (unsigned short)(Sec/SecsPerMin%MinsPerHour), (unsigned short)(Sec%SecsPerMin), 0);
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
void __fastcall TSessionData::Default()
{
  HostName = L"";
  PortNumber = SshPortNumber;
  UserName = L"";
  Password = L"";
  PingInterval = 30;
  // when changing default, update load/save logic
  PingType = ptOff;
  Timeout = 15;
  TryAgent = true;
  AgentFwd = false;
  AuthTIS = false;
  AuthKI = true;
  AuthKIPassword = true;
  AuthGSSAPI = false;
  GSSAPIFwdTGT = false;
  GSSAPIServerRealm = L"";
  ChangeUsername = false;
  Compression = false;
  SshProt = ssh2;
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
  PublicKeyFile = L"";
  FPuttyProtocol = PuttySshProtocol;
  TcpNoDelay = true;
  SendBuf = DefaultSendBuf;
  SshSimple = true;
  HostKey = L"";

  ProxyMethod = ::pmNone;
  ProxyHost = L"proxy";
  ProxyPort = 80;
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
  RemoteDirectory = L"";
  SynchronizeBrowsing = false;
  UpdateDirectories = true;
  CacheDirectories = true;
  CacheDirectoryChanges = true;
  PreserveDirectoryChanges = true;
  LockInHome = false;
  ResolveSymlinks = true;
  DSTMode = dstmUnix;
  DeleteToRecycleBin = false;
  OverwrittenToRecycleBin = false;
  RecycleBinPath = L"";
  Color = 0;
  PostLoginCommands = L"";

  // SCP
  ReturnVar = L"";
  LookupUserGroups = asAuto;
  EOLType = eolLF;
  Shell = L""; //default shell
  ReturnVar = L"";
  ClearAliases = true;
  UnsetNationalVars = true;
  ListingCommand = L"ls -la";
  IgnoreLsWarnings = true;
  Scp1Compatibility = false;
  TimeDifference = 0;
  SCPLsFullTime = asAuto;
  NotUtf = asAuto;

  // SFTP
  SftpServer = L"";
  SFTPDownloadQueue = 4;
  SFTPUploadQueue = 4;
  SFTPListingQueue = 2;
  SFTPMaxVersion = 5;
  SFTPMaxPacketSize = 0;

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
  TunnelLocalPortNumber = 0;
  TunnelPortFwd = L"";
  TunnelHostKey = L"";

  // FTP
  FtpPasvMode = true;
  FtpForcePasvIp = asAuto;
  FtpUseMlsd = asAuto;
  FtpAccount = L"";
  FtpPingInterval = 30;
  FtpPingType = ptDummyCommand;
  Ftps = ftpsNone;
  MinTlsVersion = ssl2;
  MaxTlsVersion = tls12;
  FtpListAll = asAuto;
  SslSessionReuse = true;

  FtpProxyLogonType = 0; // none

  CustomParam1 = L"";
  CustomParam2 = L"";

  IsWorkspace = false;
  Link = L"";

  Selected = false;
  FModified = false;
  FSource = ::ssNone;

  // add also to TSessionLog::AddStartupInfo()
}
//---------------------------------------------------------------------
void __fastcall TSessionData::NonPersistant()
{
  UpdateDirectories = false;
  PreserveDirectoryChanges = false;
}
//---------------------------------------------------------------------
#define BASE_PROPERTIES \
  PROPERTY(HostName); \
  PROPERTY(PortNumber); \
  PROPERTY(UserName); \
  PROPERTY(Password); \
  PROPERTY(PublicKeyFile); \
  PROPERTY(FSProtocol); \
  PROPERTY(Ftps); \
  PROPERTY(LocalDirectory); \
  PROPERTY(RemoteDirectory); \
//---------------------------------------------------------------------
#define ADVANCED_PROPERTIES \
  PROPERTY(PingInterval); \
  PROPERTY(PingType); \
  PROPERTY(Timeout); \
  PROPERTY(TryAgent); \
  PROPERTY(AgentFwd); \
  PROPERTY(AuthTIS); \
  PROPERTY(ChangeUsername); \
  PROPERTY(Compression); \
  PROPERTY(SshProt); \
  PROPERTY(Ssh2DES); \
  PROPERTY(SshNoUserAuth); \
  PROPERTY(CipherList); \
  PROPERTY(KexList); \
  PROPERTY(AddressFamily); \
  PROPERTY(RekeyData); \
  PROPERTY(RekeyTime); \
  PROPERTY(HostKey); \
  \
  PROPERTY(SynchronizeBrowsing); \
  PROPERTY(UpdateDirectories); \
  PROPERTY(CacheDirectories); \
  PROPERTY(CacheDirectoryChanges); \
  PROPERTY(PreserveDirectoryChanges); \
  \
  PROPERTY(ResolveSymlinks); \
  PROPERTY(DSTMode); \
  PROPERTY(LockInHome); \
  PROPERTY(Special); \
  PROPERTY(Selected); \
  PROPERTY(ReturnVar); \
  PROPERTY(LookupUserGroups); \
  PROPERTY(EOLType); \
  PROPERTY(Shell); \
  PROPERTY(ClearAliases); \
  PROPERTY(Scp1Compatibility); \
  PROPERTY(UnsetNationalVars); \
  PROPERTY(ListingCommand); \
  PROPERTY(IgnoreLsWarnings); \
  PROPERTY(SCPLsFullTime); \
  \
  PROPERTY(TimeDifference); \
  PROPERTY(TcpNoDelay); \
  PROPERTY(SendBuf); \
  PROPERTY(SshSimple); \
  PROPERTY(AuthKI); \
  PROPERTY(AuthKIPassword); \
  PROPERTY(AuthGSSAPI); \
  PROPERTY(GSSAPIFwdTGT); \
  PROPERTY(GSSAPIServerRealm); \
  PROPERTY(DeleteToRecycleBin); \
  PROPERTY(OverwrittenToRecycleBin); \
  PROPERTY(RecycleBinPath); \
  PROPERTY(NotUtf); \
  PROPERTY(PostLoginCommands); \
  \
  PROPERTY(ProxyMethod); \
  PROPERTY(ProxyHost); \
  PROPERTY(ProxyPort); \
  PROPERTY(ProxyUsername); \
  PROPERTY(ProxyPassword); \
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
  \
  for (unsigned int Index = 0; Index < LENOF(FSFTPBugs); Index++) \
  { \
    PROPERTY(SFTPBug[(TSftpBug)Index]); \
  } \
  \
  PROPERTY(Color); \
  \
  PROPERTY(Tunnel); \
  PROPERTY(TunnelHostName); \
  PROPERTY(TunnelPortNumber); \
  PROPERTY(TunnelUserName); \
  PROPERTY(TunnelPassword); \
  PROPERTY(TunnelPublicKeyFile); \
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
  PROPERTY(FtpListAll); \
  PROPERTY(SslSessionReuse); \
  \
  PROPERTY(FtpProxyLogonType); \
  \
  PROPERTY(MinTlsVersion); \
  PROPERTY(MaxTlsVersion); \
  \
  PROPERTY(IsWorkspace); \
  PROPERTY(Link); \
  \
  PROPERTY(CustomParam1); \
  PROPERTY(CustomParam2);
//---------------------------------------------------------------------
void __fastcall TSessionData::Assign(TPersistent * Source)
{
  if (Source && Source->InheritsFrom(__classid(TSessionData)))
  {
    #define PROPERTY(P) P = ((TSessionData *)Source)->P
    PROPERTY(Name);
    BASE_PROPERTIES;
    ADVANCED_PROPERTIES;
    #undef PROPERTY
    FModified = ((TSessionData *)Source)->Modified;
    FSource = ((TSessionData *)Source)->FSource;
  }
  else
  {
    TNamedObject::Assign(Source);
  }
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::IsSame(const TSessionData * Default, bool AdvancedOnly)
{
  #define PROPERTY(P) if (P != Default->P) return false;
  if (!AdvancedOnly)
  {
    BASE_PROPERTIES;
  }
  ADVANCED_PROPERTIES;
  #undef PROPERTY
  return true;
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::IsInFolderOrWorkspace(UnicodeString AFolder)
{
  return StartsText(UnixIncludeTrailingBackslash(AFolder), Name);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::DoLoad(THierarchicalStorage * Storage, bool & RewritePassword)
{
  PortNumber = Storage->ReadInteger(L"PortNumber", PortNumber);
  UserName = Storage->ReadString(L"UserName", UserName);
  // must be loaded after UserName, because HostName may be in format user@host
  HostName = Storage->ReadString(L"HostName", HostName);

  if (!Configuration->DisablePasswordStoring)
  {
    if (Storage->ValueExists(L"PasswordPlain"))
    {
      Password = Storage->ReadString(L"PasswordPlain", Password);
      RewritePassword = true;
    }
    else
    {
      FPassword = Storage->ReadStringAsBinaryData(L"Password", FPassword);
    }
  }
  HostKey = Storage->ReadString(L"HostKey", HostKey);
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
  // PingType has not existed before 3.5, where PingInterval > 0 meant today's ptNullPacket
  // Since 3.5, until 4.1 PingType was stored unconditionally.
  // Since 4.1 PingType is stored when it is not ptOff (default) or
  // when PingInterval is stored.
  if (!Storage->ValueExists(L"PingType"))
  {
    if (Storage->ReadInteger(L"PingInterval", 0) > 0)
    {
      PingType = ptNullPacket;
    }
  }
  else
  {
    PingType = static_cast<TPingType>(Storage->ReadInteger(L"PingType", ptOff));
  }
  Timeout = Storage->ReadInteger(L"Timeout", Timeout);
  TryAgent = Storage->ReadBool(L"TryAgent", TryAgent);
  AgentFwd = Storage->ReadBool(L"AgentFwd", AgentFwd);
  AuthTIS = Storage->ReadBool(L"AuthTIS", AuthTIS);
  AuthKI = Storage->ReadBool(L"AuthKI", AuthKI);
  AuthKIPassword = Storage->ReadBool(L"AuthKIPassword", AuthKIPassword);
  // Continue to use setting keys of previous kerberos implementation (vaclav tomec),
  // but fallback to keys of other implementations (official putty and vintela quest putty),
  // to allow imports from all putty versions.
  // Both vaclav tomec and official putty use AuthGSSAPI
  AuthGSSAPI = Storage->ReadBool(L"AuthGSSAPI", Storage->ReadBool(L"AuthSSPI", AuthGSSAPI));
  GSSAPIFwdTGT = Storage->ReadBool(L"GSSAPIFwdTGT", Storage->ReadBool(L"GssapiFwd", Storage->ReadBool(L"SSPIFwdTGT", GSSAPIFwdTGT)));
  GSSAPIServerRealm = Storage->ReadString(L"GSSAPIServerRealm", Storage->ReadString(L"KerbPrincipal", GSSAPIServerRealm));
  ChangeUsername = Storage->ReadBool(L"ChangeUsername", ChangeUsername);
  Compression = Storage->ReadBool(L"Compression", Compression);
  SshProt = (TSshProt)Storage->ReadInteger(L"SshProt", SshProt);
  Ssh2DES = Storage->ReadBool(L"Ssh2DES", Ssh2DES);
  SshNoUserAuth = Storage->ReadBool(L"SshNoUserAuth", SshNoUserAuth);
  CipherList = Storage->ReadString(L"Cipher", CipherList);
  KexList = Storage->ReadString(L"KEX", KexList);
  PublicKeyFile = Storage->ReadString(L"PublicKeyFile", PublicKeyFile);
  AddressFamily = static_cast<TAddressFamily>
    (Storage->ReadInteger(L"AddressFamily", AddressFamily));
  RekeyData = Storage->ReadString(L"RekeyBytes", RekeyData);
  RekeyTime = Storage->ReadInteger(L"RekeyTime", RekeyTime);

  FSProtocol = (TFSProtocol)Storage->ReadInteger(L"FSProtocol", FSProtocol);
  LocalDirectory = Storage->ReadString(L"LocalDirectory", LocalDirectory);
  RemoteDirectory = Storage->ReadString(L"RemoteDirectory", RemoteDirectory);
  SynchronizeBrowsing = Storage->ReadBool(L"SynchronizeBrowsing", SynchronizeBrowsing);
  UpdateDirectories = Storage->ReadBool(L"UpdateDirectories", UpdateDirectories);
  CacheDirectories = Storage->ReadBool(L"CacheDirectories", CacheDirectories);
  CacheDirectoryChanges = Storage->ReadBool(L"CacheDirectoryChanges", CacheDirectoryChanges);
  PreserveDirectoryChanges = Storage->ReadBool(L"PreserveDirectoryChanges", PreserveDirectoryChanges);

  ResolveSymlinks = Storage->ReadBool(L"ResolveSymlinks", ResolveSymlinks);
  DSTMode = (TDSTMode)Storage->ReadInteger(L"ConsiderDST", DSTMode);
  LockInHome = Storage->ReadBool(L"LockInHome", LockInHome);
  Special = Storage->ReadBool(L"Special", Special);
  Shell = Storage->ReadString(L"Shell", Shell);
  ClearAliases = Storage->ReadBool(L"ClearAliases", ClearAliases);
  UnsetNationalVars = Storage->ReadBool(L"UnsetNationalVars", UnsetNationalVars);
  ListingCommand = Storage->ReadString(L"ListingCommand",
    Storage->ReadBool(L"AliasGroupList", false) ? UnicodeString(L"ls -gla") : ListingCommand);
  IgnoreLsWarnings = Storage->ReadBool(L"IgnoreLsWarnings", IgnoreLsWarnings);
  SCPLsFullTime = TAutoSwitch(Storage->ReadInteger(L"SCPLsFullTime", SCPLsFullTime));
  Scp1Compatibility = Storage->ReadBool(L"Scp1Compatibility", Scp1Compatibility);
  TimeDifference = Storage->ReadFloat(L"TimeDifference", TimeDifference);
  DeleteToRecycleBin = Storage->ReadBool(L"DeleteToRecycleBin", DeleteToRecycleBin);
  OverwrittenToRecycleBin = Storage->ReadBool(L"OverwrittenToRecycleBin", OverwrittenToRecycleBin);
  RecycleBinPath = Storage->ReadString(L"RecycleBinPath", RecycleBinPath);
  PostLoginCommands = Storage->ReadString(L"PostLoginCommands", PostLoginCommands);

  ReturnVar = Storage->ReadString(L"ReturnVar", ReturnVar);
  LookupUserGroups = TAutoSwitch(Storage->ReadInteger(L"LookupUserGroups2", LookupUserGroups));
  EOLType = (TEOLType)Storage->ReadInteger(L"EOLType", EOLType);
  NotUtf = TAutoSwitch(Storage->ReadInteger(L"Utf", Storage->ReadInteger(L"SFTPUtfBug", NotUtf)));

  TcpNoDelay = Storage->ReadBool(L"TcpNoDelay", TcpNoDelay);
  SendBuf = Storage->ReadInteger(L"SendBuf", Storage->ReadInteger("SshSendBuf", SendBuf));
  SshSimple = Storage->ReadBool(L"SshSimple", SshSimple);

  ProxyMethod = (TProxyMethod)Storage->ReadInteger(L"ProxyMethod", -1);
  if (ProxyMethod < 0)
  {
    int ProxyType = Storage->ReadInteger(L"ProxyType", pxNone);
    int ProxySOCKSVersion;
    switch (ProxyType) {
      case pxHTTP:
        ProxyMethod = pmHTTP;
        break;
      case pxTelnet:
        ProxyMethod = pmTelnet;
        break;
      case pxSocks:
        ProxySOCKSVersion = Storage->ReadInteger(L"ProxySOCKSVersion", 5);
        ProxyMethod = ProxySOCKSVersion == 5 ? pmSocks5 : pmSocks4;
        break;
      default:
      case pxNone:
        ProxyMethod = ::pmNone;
        break;
    }
  }
  ProxyHost = Storage->ReadString(L"ProxyHost", ProxyHost);
  ProxyPort = Storage->ReadInteger(L"ProxyPort", ProxyPort);
  ProxyUsername = Storage->ReadString(L"ProxyUsername", ProxyUsername);
  if (Storage->ValueExists(L"ProxyPassword"))
  {
    // encrypt unencrypted password
    ProxyPassword = Storage->ReadString(L"ProxyPassword", L"");
  }
  else
  {
    // load encrypted password
    FProxyPassword = Storage->ReadStringAsBinaryData(L"ProxyPasswordEnc", FProxyPassword);
  }
  if (ProxyMethod == pmCmd)
  {
    ProxyLocalCommand = Storage->ReadStringRaw(L"ProxyTelnetCommand", ProxyLocalCommand);
  }
  else
  {
    ProxyTelnetCommand = Storage->ReadStringRaw(L"ProxyTelnetCommand", ProxyTelnetCommand);
  }
  ProxyDNS = TAutoSwitch((Storage->ReadInteger(L"ProxyDNS", (ProxyDNS + 2) % 3) + 1) % 3);
  ProxyLocalhost = Storage->ReadBool(L"ProxyLocalhost", ProxyLocalhost);

  #define READ_BUG(BUG) \
    Bug[sb##BUG] = TAutoSwitch(2 - Storage->ReadInteger(L"Bug"#BUG, \
      2 - Bug[sb##BUG]));
  READ_BUG(Ignore1);
  READ_BUG(PlainPW1);
  READ_BUG(RSA1);
  READ_BUG(HMAC2);
  READ_BUG(DeriveKey2);
  READ_BUG(RSAPad2);
  READ_BUG(PKSessID2);
  READ_BUG(Rekey2);
  READ_BUG(MaxPkt2);
  READ_BUG(Ignore2);
  #undef READ_BUG

  if ((Bug[sbHMAC2] == asAuto) &&
      Storage->ReadBool(L"BuggyMAC", false))
  {
      Bug[sbHMAC2] = asOn;
  }

  SftpServer = Storage->ReadString(L"SftpServer", SftpServer);
  #define READ_SFTP_BUG(BUG) \
    SFTPBug[sb##BUG] = TAutoSwitch(Storage->ReadInteger(L"SFTP" #BUG "Bug", SFTPBug[sb##BUG]));
  READ_SFTP_BUG(Symlink);
  READ_SFTP_BUG(SignedTS);
  #undef READ_SFTP_BUG

  SFTPMaxVersion = Storage->ReadInteger(L"SFTPMaxVersion", SFTPMaxVersion);
  SFTPMaxPacketSize = Storage->ReadInteger(L"SFTPMaxPacketSize", SFTPMaxPacketSize);

  Color = Storage->ReadInteger(L"Color", Color);

  PuttyProtocol = Storage->ReadString(L"Protocol", PuttyProtocol);

  Tunnel = Storage->ReadBool(L"Tunnel", Tunnel);
  TunnelPortNumber = Storage->ReadInteger(L"TunnelPortNumber", TunnelPortNumber);
  TunnelUserName = Storage->ReadString(L"TunnelUserName", TunnelUserName);
  // must be loaded after TunnelUserName,
  // because TunnelHostName may be in format user@host
  TunnelHostName = Storage->ReadString(L"TunnelHostName", TunnelHostName);
  if (!Configuration->DisablePasswordStoring)
  {
    if (Storage->ValueExists(L"TunnelPasswordPlain"))
    {
      TunnelPassword = Storage->ReadString(L"TunnelPasswordPlain", TunnelPassword);
      RewritePassword = true;
    }
    else
    {
      FTunnelPassword = Storage->ReadStringAsBinaryData(L"TunnelPassword", FTunnelPassword);
    }
  }
  TunnelPublicKeyFile = Storage->ReadString(L"TunnelPublicKeyFile", TunnelPublicKeyFile);
  TunnelLocalPortNumber = Storage->ReadInteger(L"TunnelLocalPortNumber", TunnelLocalPortNumber);
  TunnelHostKey = Storage->ReadString(L"TunnelHostKey", TunnelHostKey);

  // Ftp prefix
  FtpPasvMode = Storage->ReadBool(L"FtpPasvMode", FtpPasvMode);
  FtpForcePasvIp = TAutoSwitch(Storage->ReadInteger(L"FtpForcePasvIp2", FtpForcePasvIp));
  FtpUseMlsd = TAutoSwitch(Storage->ReadInteger(L"FtpUseMlsd", FtpUseMlsd));
  FtpAccount = Storage->ReadString(L"FtpAccount", FtpAccount);
  FtpPingInterval = Storage->ReadInteger(L"FtpPingInterval", FtpPingInterval);
  FtpPingType = static_cast<TPingType>(Storage->ReadInteger(L"FtpPingType", FtpPingType));
  Ftps = static_cast<TFtps>(Storage->ReadInteger(L"Ftps", Ftps));
  FtpListAll = TAutoSwitch(Storage->ReadInteger(L"FtpListAll", FtpListAll));
  SslSessionReuse = Storage->ReadBool(L"SslSessionReuse", SslSessionReuse);

  FtpProxyLogonType = Storage->ReadInteger(L"FtpProxyLogonType", FtpProxyLogonType);

  MinTlsVersion = static_cast<TTlsVersion>(Storage->ReadInteger(L"MinTlsVersion", MinTlsVersion));
  MaxTlsVersion = static_cast<TTlsVersion>(Storage->ReadInteger(L"MaxTlsVersion", MaxTlsVersion));

  IsWorkspace = Storage->ReadBool(L"IsWorkspace", IsWorkspace);
  Link = Storage->ReadString(L"Link", Link);

  CustomParam1 = Storage->ReadString(L"CustomParam1", CustomParam1);
  CustomParam2 = Storage->ReadString(L"CustomParam2", CustomParam2);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Load(THierarchicalStorage * Storage)
{
  bool RewritePassword = false;
  if (Storage->OpenSubKey(InternalStorageKey, False))
  {
    DoLoad(Storage, RewritePassword);

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
        Storage->DeleteValue(L"PasswordPlain");
        if (!Password.IsEmpty())
        {
          Storage->WriteBinaryDataAsString(L"Password", FPassword);
        }
        Storage->DeleteValue(L"TunnelPasswordPlain");
        if (!TunnelPassword.IsEmpty())
        {
          Storage->WriteBinaryDataAsString(L"TunnelPassword", FTunnelPassword);
        }
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
void __fastcall TSessionData::Save(THierarchicalStorage * Storage,
  bool PuttyExport, const TSessionData * Default)
{
  if (Storage->OpenSubKey(InternalStorageKey, true))
  {
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
    WRITE_DATA_EX(Integer, L"PingInterval", PingInterval / SecsPerMin, );
    WRITE_DATA_EX(Integer, L"PingIntervalSecs", PingInterval % SecsPerMin, );
    Storage->DeleteValue(L"PingIntervalSec"); // obsolete
    // when PingInterval is stored always store PingType not to attempt to
    // deduce PingType from PingInterval (backward compatibility with pre 3.5)
    if (((Default != NULL) && (PingType != Default->PingType)) ||
        Storage->ValueExists(L"PingInterval"))
    {
      Storage->WriteInteger(L"PingType", PingType);
    }
    else
    {
      Storage->DeleteValue(L"PingType");
    }
    WRITE_DATA(Integer, Timeout);
    WRITE_DATA(Bool, TryAgent);
    WRITE_DATA(Bool, AgentFwd);
    WRITE_DATA(Bool, AuthTIS);
    WRITE_DATA(Bool, AuthKI);
    WRITE_DATA(Bool, AuthKIPassword);

    WRITE_DATA(Bool, AuthGSSAPI);
    WRITE_DATA(Bool, GSSAPIFwdTGT);
    WRITE_DATA(String, GSSAPIServerRealm);
    Storage->DeleteValue(L"TryGSSKEX");
    Storage->DeleteValue(L"UserNameFromEnvironment");
    Storage->DeleteValue("GSSAPIServerChoosesUserName");
    Storage->DeleteValue(L"GSSAPITrustDNS");
    if (PuttyExport)
    {
      // duplicate kerberos setting with keys of the vintela quest putty
      WRITE_DATA_EX(Bool, L"AuthSSPI", AuthGSSAPI, );
      WRITE_DATA_EX(Bool, L"SSPIFwdTGT", GSSAPIFwdTGT, );
      WRITE_DATA_EX(String, L"KerbPrincipal", GSSAPIServerRealm, );
      // duplicate kerberos setting with keys of the official putty
      WRITE_DATA_EX(Bool, L"GssapiFwd", GSSAPIFwdTGT, );
    }

    WRITE_DATA(Bool, ChangeUsername);
    WRITE_DATA(Bool, Compression);
    WRITE_DATA(Integer, SshProt);
    WRITE_DATA(Bool, Ssh2DES);
    WRITE_DATA(Bool, SshNoUserAuth);
    WRITE_DATA_EX(String, L"Cipher", CipherList, );
    WRITE_DATA_EX(String, L"KEX", KexList, );
    WRITE_DATA(Integer, AddressFamily);
    WRITE_DATA_EX(String, L"RekeyBytes", RekeyData, );
    WRITE_DATA(Integer, RekeyTime);

    WRITE_DATA(Bool, TcpNoDelay);

    if (PuttyExport)
    {
      WRITE_DATA(StringRaw, UserName);
      WRITE_DATA(StringRaw, PublicKeyFile);
    }
    else
    {
      WRITE_DATA(String, UserName);
      WRITE_DATA(String, PublicKeyFile);
      WRITE_DATA(Integer, FSProtocol);
      WRITE_DATA(String, LocalDirectory);
      WRITE_DATA(String, RemoteDirectory);
      WRITE_DATA(Bool, SynchronizeBrowsing);
      WRITE_DATA(Bool, UpdateDirectories);
      WRITE_DATA(Bool, CacheDirectories);
      WRITE_DATA(Bool, CacheDirectoryChanges);
      WRITE_DATA(Bool, PreserveDirectoryChanges);

      WRITE_DATA(Bool, ResolveSymlinks);
      WRITE_DATA_EX(Integer, L"ConsiderDST", DSTMode, );
      WRITE_DATA(Bool, LockInHome);
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
      WRITE_DATA(Float, TimeDifference);
      WRITE_DATA(Bool, DeleteToRecycleBin);
      WRITE_DATA(Bool, OverwrittenToRecycleBin);
      WRITE_DATA(String, RecycleBinPath);
      WRITE_DATA(String, PostLoginCommands);

      WRITE_DATA(String, ReturnVar);
      WRITE_DATA_EX(Integer, L"LookupUserGroups2", LookupUserGroups, );
      WRITE_DATA(Integer, EOLType);
      Storage->DeleteValue(L"SFTPUtfBug");
      WRITE_DATA_EX(Integer, L"Utf", NotUtf, );
      WRITE_DATA(Integer, SendBuf);
      WRITE_DATA(Bool, SshSimple);
    }

    WRITE_DATA(Integer, ProxyMethod);
    if (PuttyExport)
    {
      // support for Putty 0.53b and older
      int ProxyType;
      int ProxySOCKSVersion = 5;
      switch (ProxyMethod) {
        case pmHTTP:
          ProxyType = pxHTTP;
          break;
        case pmTelnet:
          ProxyType = pxTelnet;
          break;
        case pmSocks5:
          ProxyType = pxSocks;
          ProxySOCKSVersion = 5;
          break;
        case pmSocks4:
          ProxyType = pxSocks;
          ProxySOCKSVersion = 4;
          break;
        default:
        case ::pmNone:
          ProxyType = pxNone;
          break;
      }
      Storage->WriteInteger(L"ProxyType", ProxyType);
      Storage->WriteInteger(L"ProxySOCKSVersion", ProxySOCKSVersion);
    }
    else
    {
      Storage->DeleteValue(L"ProxyType");
      Storage->DeleteValue(L"ProxySOCKSVersion");
    }
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
    WRITE_BUG(Ignore1);
    WRITE_BUG(PlainPW1);
    WRITE_BUG(RSA1);
    WRITE_BUG(HMAC2);
    WRITE_BUG(DeriveKey2);
    WRITE_BUG(RSAPad2);
    WRITE_BUG(PKSessID2);
    WRITE_BUG(Rekey2);
    WRITE_BUG(MaxPkt2);
    WRITE_BUG(Ignore2);
    #undef WRITE_BUG
    #undef WRITE_DATA_CONV_FUNC

    Storage->DeleteValue(L"BuggyMAC");
    Storage->DeleteValue(L"AliasGroupList");

    if (PuttyExport)
    {
      WRITE_DATA_EX(String, L"Protocol", PuttyProtocol, );
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

      WRITE_DATA(Integer, Color);

      WRITE_DATA(Bool, Tunnel);
      WRITE_DATA(String, TunnelHostName);
      WRITE_DATA(Integer, TunnelPortNumber);
      WRITE_DATA(String, TunnelUserName);
      WRITE_DATA(String, TunnelPublicKeyFile);
      WRITE_DATA(Integer, TunnelLocalPortNumber);

      WRITE_DATA(Bool, FtpPasvMode);
      WRITE_DATA_EX(Integer, L"FtpForcePasvIp2", FtpForcePasvIp, );
      WRITE_DATA(Integer, FtpUseMlsd);
      WRITE_DATA(String, FtpAccount);
      WRITE_DATA(Integer, FtpPingInterval);
      WRITE_DATA(Integer, FtpPingType);
      WRITE_DATA(Integer, Ftps);
      WRITE_DATA(Integer, FtpListAll);
      WRITE_DATA(Bool, SslSessionReuse);

      WRITE_DATA(Integer, FtpProxyLogonType);

      WRITE_DATA(Integer, MinTlsVersion);
      WRITE_DATA(Integer, MaxTlsVersion);

      WRITE_DATA(Bool, IsWorkspace);
      WRITE_DATA(String, Link);

      WRITE_DATA(String, CustomParam1);
      WRITE_DATA(String, CustomParam2);
    }

    SavePasswords(Storage, PuttyExport);

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
void __fastcall TSessionData::ImportFromFilezilla(_di_IXMLNode Node, const UnicodeString & Path)
{
  Name = UnixIncludeTrailingBackslash(Path) + ReadXmlNode(Node, L"Name", Name);
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
      Ftps = ftpsExplicitSsl;
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
    Password = ReadXmlNode(Node, L"Pass", Password);
    FtpAccount = ReadXmlNode(Node, L"Account", FtpAccount);
  }

  int DefaultTimeDifference = TimeToMSec(TimeDifference) / MSecsPerSec;
  TimeDifference =
    (double(ReadXmlNode(Node, L"TimezoneOffset", DefaultTimeDifference) / SecsPerDay));

  UnicodeString PasvMode = ReadXmlNode(Node, L"PasvMode", L"");
  if (SameText(PasvMode, L"MODE_PASSIVE"))
  {
    FtpPasvMode = true;
  }
  else if (SameText(PasvMode, L"MODE_ACTIVE"))
  {
    FtpPasvMode = false;
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

}
//---------------------------------------------------------------------
void __fastcall TSessionData::SavePasswords(THierarchicalStorage * Storage, bool PuttyExport)
{
  if (!Configuration->DisablePasswordStoring && !PuttyExport && !FPassword.IsEmpty())
  {
    Storage->WriteBinaryDataAsString(L"Password", StronglyRecryptPassword(FPassword, UserName+HostName));
  }
  else
  {
    Storage->DeleteValue(L"Password");
  }
  Storage->DeleteValue(L"PasswordPlain");

  if (PuttyExport)
  {
    // save password unencrypted
    Storage->WriteString(L"ProxyPassword", ProxyPassword);
  }
  else
  {
    // save password encrypted
    if (!FProxyPassword.IsEmpty())
    {
      Storage->WriteBinaryDataAsString(L"ProxyPasswordEnc", StronglyRecryptPassword(FProxyPassword, ProxyUsername+ProxyHost));
    }
    else
    {
      Storage->DeleteValue(L"ProxyPasswordEnc");
    }
    Storage->DeleteValue(L"ProxyPassword");

    if (!Configuration->DisablePasswordStoring && !FTunnelPassword.IsEmpty())
    {
      Storage->WriteBinaryDataAsString(L"TunnelPassword", StronglyRecryptPassword(FTunnelPassword, TunnelUserName+TunnelHostName));
    }
    else
    {
      Storage->DeleteValue(L"TunnelPassword");
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::RecryptPasswords()
{
  Password = Password;
  ProxyPassword = ProxyPassword;
  TunnelPassword = TunnelPassword;
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::HasPassword()
{
  return !FPassword.IsEmpty();
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::HasAnyPassword()
{
  return HasPassword() || !FProxyPassword.IsEmpty() || !FTunnelPassword.IsEmpty();
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
UnicodeString __fastcall TSessionData::GetSource()
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
      assert(false);
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

      SavePasswords(Storage, false);
    }
    __finally
    {
      Storage->CloseSubKey();
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Remove()
{
  THierarchicalStorage * Storage = Configuration->CreateScpStorage(true);
  try
  {
    Storage->Explicit = true;
    if (Storage->OpenSubKey(Configuration->StoredSessionsSubKey, false))
    {
      Storage->RecursiveDeleteSubKey(InternalStorageKey);
    }
  }
  __finally
  {
    delete Storage;
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
bool __fastcall TSessionData::ParseUrl(UnicodeString Url, TOptions * Options,
  TStoredSessionList * StoredSessions, bool & DefaultsOnly, UnicodeString * FileName,
  bool * AProtocolDefined, UnicodeString * MaskedUrl)
{
  bool ProtocolDefined = false;
  bool PortNumberDefined = false;
  TFSProtocol AFSProtocol;
  int APortNumber;
  TFtps AFtps = ftpsNone;
  if (Url.SubString(1, 4).LowerCase() == L"scp:")
  {
    AFSProtocol = fsSCPonly;
    APortNumber = SshPortNumber;
    MoveStr(Url, MaskedUrl, 4);
    ProtocolDefined = true;
  }
  else if (Url.SubString(1, 5).LowerCase() == L"sftp:")
  {
    AFSProtocol = fsSFTPonly;
    APortNumber = SshPortNumber;
    MoveStr(Url, MaskedUrl, 5);
    ProtocolDefined = true;
  }
  else if (Url.SubString(1, 4).LowerCase() == L"ftp:")
  {
    AFSProtocol = fsFTP;
    Ftps = ftpsNone;
    APortNumber = FtpPortNumber;
    MoveStr(Url, MaskedUrl, 4);
    ProtocolDefined = true;
  }
  else if (Url.SubString(1, 5).LowerCase() == L"ftps:")
  {
    AFSProtocol = fsFTP;
    AFtps = ftpsImplicit;
    APortNumber = FtpsImplicitPortNumber;
    MoveStr(Url, MaskedUrl, 5);
    ProtocolDefined = true;
  }
  else if (Url.SubString(1, 5).LowerCase() == L"http:")
  {
    AFSProtocol = fsWebDAV;
    AFtps = ftpsNone;
    APortNumber = HTTPPortNumber;
    MoveStr(Url, MaskedUrl, 5);
    ProtocolDefined = true;
  }
  else if (Url.SubString(1, 6).LowerCase() == L"https:")
  {
    AFSProtocol = fsWebDAV;
    AFtps = ftpsImplicit;
    APortNumber = HTTPSPortNumber;
    MoveStr(Url, MaskedUrl, 6);
    ProtocolDefined = true;
  }

  if (ProtocolDefined && (Url.SubString(1, 2) == L"//"))
  {
    MoveStr(Url, MaskedUrl, 2);
  }

  if (AProtocolDefined != NULL)
  {
    *AProtocolDefined = ProtocolDefined;
  }

  if (!Url.IsEmpty())
  {
    UnicodeString DecodedUrl = DecodeUrlChars(Url);
    // lookup stored session even if protocol was defined
    // (this allows setting for example default username for host
    // by creating stored session named by host)
    TSessionData * Data = NULL;
    for (Integer Index = 0; Index < StoredSessions->Count + StoredSessions->HiddenCount; Index++)
    {
      TSessionData * AData = (TSessionData *)StoredSessions->Items[Index];
      if (!AData->IsWorkspace &&
          (AnsiSameText(AData->Name, DecodedUrl) ||
           AnsiSameText(AData->Name + L"/", DecodedUrl.SubString(1, AData->Name.Length() + 1))))
      {
        Data = AData;
        break;
      }
    }

    UnicodeString ARemoteDirectory;

    if (Data != NULL)
    {
      Assign(Data);
      int P = 1;
      while (!AnsiSameText(DecodeUrlChars(Url.SubString(1, P)), Data->Name))
      {
        P++;
        assert(P <= Url.Length());
      }
      ARemoteDirectory = Url.SubString(P + 1, Url.Length() - P);

      if (Data->Hidden)
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
      Assign(StoredSessions->DefaultSettings);
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
        PortNumber = StrToIntDef(DecodeUrlChars(HostInfo), -1);
        PortNumberDefined = true;
      }
      else if (ProtocolDefined)
      {
        PortNumber = APortNumber;
      }

      if (ProtocolDefined)
      {
        Ftps = AFtps;
      }

      UnicodeString RawUserName = CutToChar(UserInfo, L':', false);
      UserName = DecodeUrlChars(RawUserName);

      Password = DecodeUrlChars(UserInfo);

      ARemoteDirectory = Url.SubString(PSlash, Url.Length() - PSlash + 1);

      if (MaskedUrl != NULL)
      {
        (*MaskedUrl) += RawUserName;
        if (!UserInfo.IsEmpty())
        {
          (*MaskedUrl) += L":***";
        }
        (*MaskedUrl) += L"@" + OrigHostInfo + ARemoteDirectory;
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
    }

    DefaultsOnly = false;
  }
  else
  {
    Assign(StoredSessions->DefaultSettings);

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
    if (Options->FindSwitch(L"privatekey", Value))
    {
      PublicKeyFile = Value;
    }
    if (Options->FindSwitch(L"timeout", Value))
    {
      Timeout = StrToInt(Value);
    }
    if (Options->FindSwitch(L"hostkey", Value) ||
        Options->FindSwitch(L"certificate", Value))
    {
      HostKey = Value;
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
    if (Options->FindSwitch(L"explicitssl"))
    {
      bool Enabled = Options->SwitchValue(L"explicitssl", true);
      Ftps = Enabled ? ftpsExplicitSsl : ftpsNone;
      if (!PortNumberDefined && Enabled)
      {
        PortNumber = FtpPortNumber;
      }
    }
    if (Options->FindSwitch(L"explicittls", Value))
    {
      bool Enabled = Options->SwitchValue(L"explicittls", true);
      Ftps = Enabled ? ftpsExplicitTls : ftpsNone;
      if (!PortNumberDefined && Enabled)
      {
        PortNumber = FtpPortNumber;
      }
    }
    if (Options->FindSwitch(L"rawsettings"))
    {
      TStrings * RawSettings = NULL;
      TOptionsStorage * OptionsStorage = NULL;
      try
      {
        RawSettings = new TStringList();

        if (Options->FindSwitch(L"rawsettings", RawSettings))
        {
          OptionsStorage = new TOptionsStorage(RawSettings);

          bool Dummy;
          DoLoad(OptionsStorage, Dummy);
        }
      }
      __finally
      {
        delete RawSettings;
        delete OptionsStorage;
      }
    }
  }

  return true;
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
}
//---------------------------------------------------------------------
void __fastcall TSessionData::RollbackTunnel()
{
  HostName = FOrigHostName;
  PortNumber = FOrigPortNumber;
  ProxyMethod = FOrigProxyMethod;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::ExpandEnvironmentVariables()
{
  HostName = HostNameExpanded;
  UserName = UserNameExpanded;
  PublicKeyFile = ::ExpandEnvironmentVariables(PublicKeyFile);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::ValidatePath(const UnicodeString Path)
{
  // noop
}
//---------------------------------------------------------------------
void __fastcall TSessionData::ValidateName(const UnicodeString Name)
{
  if (Name.LastDelimiter(L"/") > 0)
  {
    throw Exception(FMTLOAD(ITEM_NAME_INVALID, (Name, L"/")));
  }
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
bool __fastcall TSessionData::GetCanLogin()
{
  return !FHostName.IsEmpty();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetSessionKey()
{
  return FORMAT(L"%s@%s", (UserName, HostName));
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetInternalStorageKey()
{
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
void __fastcall TSessionData::SetHostName(UnicodeString value)
{
  if (FHostName != value)
  {
    // HostName is key for password encryption
    UnicodeString XPassword = Password;

    int P = value.LastDelimiter(L"@");
    if (P > 0)
    {
      UserName = value.SubString(1, P - 1);
      value = value.SubString(P + 1, value.Length() - P);
    }
    FHostName = value;
    Modify();

    Password = XPassword;
    Shred(XPassword);
  }
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetHostNameExpanded()
{
  return ::ExpandEnvironmentVariables(HostName);
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
  // UserName is key for password encryption
  UnicodeString XPassword = Password;
  SET_SESSION_PROPERTY(UserName);
  Password = XPassword;
  Shred(XPassword);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetUserNameExpanded()
{
  return ::ExpandEnvironmentVariables(UserName);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPassword(UnicodeString avalue)
{
  RawByteString value = EncryptPassword(avalue, UserName+HostName);
  SET_SESSION_PROPERTY(Password);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetPassword() const
{
  return DecryptPassword(FPassword, UserName+HostName);
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
void __fastcall TSessionData::SetAuthTIS(bool value)
{
  SET_SESSION_PROPERTY(AuthTIS);
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
void __fastcall TSessionData::SetGSSAPIFwdTGT(bool value)
{
  SET_SESSION_PROPERTY(GSSAPIFwdTGT);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetGSSAPIServerRealm(UnicodeString value)
{
  SET_SESSION_PROPERTY(GSSAPIServerRealm);
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
void __fastcall TSessionData::SetSshProt(TSshProt value)
{
  SET_SESSION_PROPERTY(SshProt);
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
UnicodeString __fastcall TSessionData::GetSshProtStr()
{
  return SshProtList[FSshProt];
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::GetUsesSsh()
{
  return IsSshProtocol(FSProtocol);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetCipher(int Index, TCipher value)
{
  assert(Index >= 0 && Index < CIPHER_COUNT);
  SET_SESSION_PROPERTY(Ciphers[Index]);
}
//---------------------------------------------------------------------
TCipher __fastcall TSessionData::GetCipher(int Index) const
{
  assert(Index >= 0 && Index < CIPHER_COUNT);
  return FCiphers[Index];
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetCipherList(UnicodeString value)
{
  bool Used[CIPHER_COUNT];
  for (int C = 0; C < CIPHER_COUNT; C++) Used[C] = false;

  UnicodeString CipherStr;
  int Index = 0;
  while (!value.IsEmpty() && (Index < CIPHER_COUNT))
  {
    CipherStr = CutToChar(value, L',', true);
    for (int C = 0; C < CIPHER_COUNT; C++)
    {
      if (!CipherStr.CompareIC(CipherNames[C]))
      {
        Cipher[Index] = (TCipher)C;
        Used[C] = true;
        Index++;
        break;
      }
    }
  }

  for (int C = 0; C < CIPHER_COUNT && Index < CIPHER_COUNT; C++)
  {
    if (!Used[DefaultCipherList[C]]) Cipher[Index++] = DefaultCipherList[C];
  }
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
  assert(Index >= 0 && Index < KEX_COUNT);
  SET_SESSION_PROPERTY(Kex[Index]);
}
//---------------------------------------------------------------------
TKex __fastcall TSessionData::GetKex(int Index) const
{
  assert(Index >= 0 && Index < KEX_COUNT);
  return FKex[Index];
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetKexList(UnicodeString value)
{
  bool Used[KEX_COUNT];
  for (int K = 0; K < KEX_COUNT; K++) Used[K] = false;

  UnicodeString KexStr;
  int Index = 0;
  while (!value.IsEmpty() && (Index < KEX_COUNT))
  {
    KexStr = CutToChar(value, L',', true);
    for (int K = 0; K < KEX_COUNT; K++)
    {
      if (!KexStr.CompareIC(KexNames[K]))
      {
        Kex[Index] = (TKex)K;
        Used[K] = true;
        Index++;
        break;
      }
    }
  }

  for (int K = 0; K < KEX_COUNT && Index < KEX_COUNT; K++)
  {
    if (!Used[DefaultKexList[K]]) Kex[Index++] = DefaultKexList[K];
  }
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
void __fastcall TSessionData::SetPublicKeyFile(UnicodeString value)
{
  if (FPublicKeyFile != value)
  {
    FPublicKeyFile = StripPathQuotes(value);
    Modify();
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetReturnVar(UnicodeString value)
{
  SET_SESSION_PROPERTY(ReturnVar);
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
  assert(FSProtocol >= 0 && FSProtocol < FSPROTOCOL_COUNT);
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
  if (!HostName.IsEmpty() && !UserName.IsEmpty())
  {
    return FORMAT(L"%s@%s", (UserName, HostName));
  }
  else if (!HostName.IsEmpty())
  {
    return HostName;
  }
  else
  {
    return L"session";
  }
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::HasSessionName()
{
  return (!Name.IsEmpty() && (Name != DefaultName) && !IsWorkspace);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetSessionName()
{
  UnicodeString Result;
  if (HasSessionName())
  {
    Result = Name;
    if (Hidden)
    {
      Result = Result.SubString(TNamedObjectList::HiddenPrefix.Length() + 1, Result.Length() - TNamedObjectList::HiddenPrefix.Length());
    }
  }
  else
  {
    Result = DefaultSessionName;
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TSessionData::GetSessionUrl()
{
  UnicodeString Url;
  if (HasSessionName())
  {
    Url = Name;
  }
  else
  {
    switch (FSProtocol)
    {
      case fsSCPonly:
        Url = L"scp://";
        break;

      default:
        assert(false);
        // fallback
      case fsSFTP:
      case fsSFTPonly:
        Url = L"sftp://";
        break;

      case fsFTP:
        Url = L"ftp://";
        break;
      case fsWebDAV:
        if (Ftps == ftpsNone)
          Url = L"http://";
        else
          Url = L"https://";
        break;
    }

    if (!HostName.IsEmpty() && !UserName.IsEmpty())
    {
      Url += FORMAT(L"%s@%s", (UserName, HostName));
    }
    else if (!HostName.IsEmpty())
    {
      Url += HostName;
    }
    else
    {
      Url = L"";
    }
  }
  return Url;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTimeDifference(TDateTime value)
{
  SET_SESSION_PROPERTY(TimeDifference);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetLocalDirectory(UnicodeString value)
{
  SET_SESSION_PROPERTY(LocalDirectory);
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
void __fastcall TSessionData::SetLockInHome(bool value)
{
  SET_SESSION_PROPERTY(LockInHome);
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
  assert(Bug >= 0 && static_cast<unsigned int>(Bug) < LENOF(FBugs));
  SET_SESSION_PROPERTY(Bugs[Bug]);
}
//---------------------------------------------------------------------
TAutoSwitch __fastcall TSessionData::GetBug(TSshBug Bug) const
{
  assert(Bug >= 0 && static_cast<unsigned int>(Bug) < LENOF(FBugs));
  return FBugs[Bug];
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
void __fastcall TSessionData::SetSFTPBug(TSftpBug Bug, TAutoSwitch value)
{
  assert(Bug >= 0 && static_cast<unsigned int>(Bug) < LENOF(FSFTPBugs));
  SET_SESSION_PROPERTY(SFTPBugs[Bug]);
}
//---------------------------------------------------------------------
TAutoSwitch __fastcall TSessionData::GetSFTPBug(TSftpBug Bug) const
{
  assert(Bug >= 0 && static_cast<unsigned int>(Bug) < LENOF(FSFTPBugs));
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
  // TunnelUserName is key for password encryption
  UnicodeString XTunnelPassword = TunnelPassword;
  SET_SESSION_PROPERTY(TunnelUserName);
  TunnelPassword = XTunnelPassword;
  Shred(XTunnelPassword);
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
void __fastcall TSessionData::SetTunnelPublicKeyFile(UnicodeString value)
{
  if (FTunnelPublicKeyFile != value)
  {
    FTunnelPublicKeyFile = StripPathQuotes(value);
    Modify();
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
void __fastcall TSessionData::SetFtpPingType(TPingType value)
{
  SET_SESSION_PROPERTY(FtpPingType);
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
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFtpListAll(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(FtpListAll);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSslSessionReuse(bool value)
{
  SET_SESSION_PROPERTY(SslSessionReuse);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetNotUtf(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(NotUtf);
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
void __fastcall TSessionData::SetHostKey(UnicodeString value)
{
  SET_SESSION_PROPERTY(HostKey);
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
//=== TStoredSessionList ----------------------------------------------
__fastcall TStoredSessionList::TStoredSessionList(bool aReadOnly):
  TNamedObjectList(), FReadOnly(aReadOnly)
{
  assert(Configuration);
  FDefaultSettings = new TSessionData(DefaultName);
}
//---------------------------------------------------------------------
__fastcall TStoredSessionList::~TStoredSessionList()
{
  assert(Configuration);
  delete FDefaultSettings;
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Load(THierarchicalStorage * Storage,
  bool AsModified, bool UseDefaults)
{
  TStringList *SubKeys = new TStringList();
  TList * Loaded = new TList;
  try
  {
    Storage->GetSubKeyNames(SubKeys);
    for (int Index = 0; Index < SubKeys->Count; Index++)
    {
      TSessionData *SessionData;
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
        if (SessionName == FDefaultSettings->Name) SessionData = FDefaultSettings;
          else SessionData = (TSessionData*)FindByName(SessionName);

        if ((SessionData != FDefaultSettings) || !UseDefaults)
        {
          if (!SessionData)
          {
            SessionData = new TSessionData(L"");
            if (UseDefaults)
            {
              SessionData->Assign(DefaultSettings);
            }
            SessionData->Name = SessionName;
            Add(SessionData);
          }
          Loaded->Add(SessionData);
          SessionData->Load(Storage);
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
    delete SubKeys;
    delete Loaded;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Load()
{
  THierarchicalStorage * Storage = Configuration->CreateScpStorage(true);
  try
  {
    if (Storage->OpenSubKey(Configuration->StoredSessionsSubKey, False))
      Load(Storage);
  }
  __finally
  {
    delete Storage;
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
    DoSave(Storage, FDefaultSettings, All, RecryptPasswordOnly, FactoryDefaults);
    for (int Index = 0; Index < Count+HiddenCount; Index++)
    {
      TSessionData * SessionData = (TSessionData *)Items[Index];
      try
      {
        DoSave(Storage, SessionData, All, RecryptPasswordOnly, FactoryDefaults);
      }
      catch (Exception & E)
      {
        UnicodeString Message;
        if (RecryptPasswordOnly && ALWAYS_TRUE(RecryptPasswordErrors != NULL) &&
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
  THierarchicalStorage * Storage = Configuration->CreateScpStorage(true);
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
  for (int Index = 0; Index < Count + HiddenCount; Index++)
  {
    ((TSessionData *)Items[Index])->Modified = false;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::ImportLevelFromFilezilla(_di_IXMLNode Node, const UnicodeString & Path)
{
  for (int Index = 0; Index < Node->ChildNodes->Count; Index++)
  {
    _di_IXMLNode ChildNode = Node->ChildNodes->Get(Index);
    if (ChildNode->NodeName == L"Server")
    {
      std::auto_ptr<TSessionData> SessionData(new TSessionData(L""));
      SessionData->Assign(DefaultSettings);
      SessionData->ImportFromFilezilla(ChildNode, Path);
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

      ImportLevelFromFilezilla(ChildNode, UnixIncludeTrailingBackslash(Path) + Name.Trim());
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::ImportFromFilezilla(const UnicodeString FileName)
{
  const _di_IXMLDocument Document = interface_cast<Xmlintf::IXMLDocument>(new TXMLDocument(NULL));
  Document->LoadFromFile(FileName);
  _di_IXMLNode FileZilla3Node = Document->ChildNodes->FindNode(L"FileZilla3");
  if (FileZilla3Node != NULL)
  {
    _di_IXMLNode ServersNode = FileZilla3Node->ChildNodes->FindNode(L"Servers");
    if (ServersNode != NULL)
    {
      ImportLevelFromFilezilla(ServersNode, L"");
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Export(const UnicodeString FileName)
{
  THierarchicalStorage * Storage = new TIniFileStorage(FileName);
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
void __fastcall TStoredSessionList::Import(TStoredSessionList * From,
  bool OnlySelected)
{
  for (int Index = 0; Index < From->Count; Index++)
  {
    if (!OnlySelected || From->Sessions[Index]->Selected)
    {
      TSessionData *Session = new TSessionData(L"");
      Session->Assign(From->Sessions[Index]);
      Session->Modified = true;
      Session->MakeUniqueIn(this);
      Add(Session);
    }
  }
  // only modified, explicit
  Save(false, true);
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::SelectSessionsToImport
  (TStoredSessionList * Dest, bool SSHOnly)
{
  for (int Index = 0; Index < Count; Index++)
  {
    Sessions[Index]->Selected =
      (!SSHOnly || (Sessions[Index]->PuttyProtocol == PuttySshProtocol)) &&
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
  int Password = 0;
  int Advanced = 0;
  int Color = 0;
  bool Folders = false;
  bool Workspaces = false;
  std::auto_ptr<TSessionData> FactoryDefaults(new TSessionData(L""));
  for (int Index = 0; Index < Count; Index++)
  {
    TSessionData * Data = Sessions[Index];
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
    }

    if (Data->HasAnyPassword())
    {
      Password++;
    }

    if (Data->Color != 0)
    {
      Color++;
    }

    if (!Data->IsSame(FactoryDefaults.get(), true))
    {
      Advanced++;
    }

    if (Data->IsWorkspace)
    {
      Workspaces = true;
    }
    else if (!Data->FolderName.IsEmpty())
    {
      Folders = true;
    }
  }

  Configuration->Usage->Set(L"StoredSessionsCountSCP", SCP);
  Configuration->Usage->Set(L"StoredSessionsCountSFTP", SFTP);
  Configuration->Usage->Set(L"StoredSessionsCountFTP", FTP);
  Configuration->Usage->Set(L"StoredSessionsCountFTPS", FTPS);
  Configuration->Usage->Set(L"StoredSessionsCountWebDAV", WebDAV);
  Configuration->Usage->Set(L"StoredSessionsCountWebDAVS", WebDAVS);
  Configuration->Usage->Set(L"StoredSessionsCountPassword", Password);
  Configuration->Usage->Set(L"StoredSessionsCountColor", Color);
  Configuration->Usage->Set(L"StoredSessionsCountAdvanced", Advanced);

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
  assert(FDefaultSettings);
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
void __fastcall TStoredSessionList::ImportHostKeys(const UnicodeString TargetKey,
  const UnicodeString SourceKey, TStoredSessionList * Sessions,
  bool OnlySelected)
{
  TRegistryStorage * SourceStorage = NULL;
  TRegistryStorage * TargetStorage = NULL;
  TStringList * KeyList = NULL;
  try
  {
    SourceStorage = new TRegistryStorage(SourceKey);
    TargetStorage = new TRegistryStorage(TargetKey);
    TargetStorage->AccessMode = smReadWrite;
    KeyList = new TStringList();

    if (SourceStorage->OpenRootKey(false) &&
        TargetStorage->OpenRootKey(true))
    {
      SourceStorage->GetValueNames(KeyList);

      TSessionData * Session;
      UnicodeString HostKeyName;
      assert(Sessions != NULL);
      for (int Index = 0; Index < Sessions->Count; Index++)
      {
        Session = Sessions->Sessions[Index];
        if (!OnlySelected || Session->Selected)
        {
          HostKeyName = PuttyMungeStr(FORMAT(L"@%d:%s", (Session->PortNumber, Session->HostName)));
          UnicodeString KeyName;
          for (int KeyIndex = 0; KeyIndex < KeyList->Count; KeyIndex++)
          {
            KeyName = KeyList->Strings[KeyIndex];
            int P = KeyName.Pos(HostKeyName);
            if ((P > 0) && (P == KeyName.Length() - HostKeyName.Length() + 1))
            {
              TargetStorage->WriteStringRaw(KeyName,
                SourceStorage->ReadStringRaw(KeyName, L""));
            }
          }
        }
      }
    }
  }
  __finally
  {
    delete SourceStorage;
    delete TargetStorage;
    delete KeyList;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TStoredSessionList::IsFolderOrWorkspace(
  const UnicodeString & Name, bool Workspace)
{
  bool Result = false;
  TSessionData * FirstData = NULL;
  if (!Name.IsEmpty())
  {
    for (int Index = 0; !Result && (Index < Count); Index++)
    {
      Result = Sessions[Index]->IsInFolderOrWorkspace(Name);
      if (Result)
      {
        FirstData = Sessions[Index];
      }
    }
  }

  return
    Result &&
    ALWAYS_TRUE(FirstData != NULL) &&
    (FirstData->IsWorkspace == Workspace);
}
//---------------------------------------------------------------------------
bool __fastcall TStoredSessionList::IsFolder(const UnicodeString & Name)
{
  return IsFolderOrWorkspace(Name, false);
}
//---------------------------------------------------------------------------
bool __fastcall TStoredSessionList::IsWorkspace(const UnicodeString & Name)
{
  return IsFolderOrWorkspace(Name, true);
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TStoredSessionList::CheckIsInFolderOrWorkspaceAndResolve(
  TSessionData * Data, const UnicodeString & Name)
{
  if (Data->IsInFolderOrWorkspace(Name))
  {
    Data = ResolveSessionData(Data);

    if ((Data != NULL) && Data->CanLogin &&
        ALWAYS_TRUE(Data->Link.IsEmpty()))
    {
      return Data;
    }
  }
  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TStoredSessionList::GetFolderOrWorkspace(const UnicodeString & Name, TList * List)
{
  for (int Index = 0; (Index < Count); Index++)
  {
    TSessionData * Data =
      CheckIsInFolderOrWorkspaceAndResolve(Sessions[Index], Name);

    if (Data != NULL)
    {
      TSessionData * Data2 = new TSessionData(L"");
      Data2->Assign(Data);
      List->Add(Data2);
    }
  }
}
//---------------------------------------------------------------------------
TStrings * __fastcall TStoredSessionList::GetFolderOrWorkspaceList(
  const UnicodeString & Name)
{
  std::auto_ptr<TStringList> Result(new TStringList());

  for (int Index = 0; (Index < Count); Index++)
  {
    TSessionData * Data =
      CheckIsInFolderOrWorkspaceAndResolve(Sessions[Index], Name);

    if (Data != NULL)
    {
      Result->Add(Data->SessionName);
    }
  }

  return Result.release();
}
//---------------------------------------------------------------------------
TStrings * __fastcall TStoredSessionList::GetWorkspaces()
{
  std::auto_ptr<TStringList> Result(new TStringList());
  Result->Sorted = true;
  Result->Duplicates = dupIgnore;
  Result->CaseSensitive = false;

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
      Data->Remove();
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
  bool * AProtocolDefined, UnicodeString * MaskedUrl)
{
  TSessionData * Data = new TSessionData(L"");
  try
  {
    Data->ParseUrl(Url, Options, this, DefaultsOnly, FileName, AProtocolDefined, MaskedUrl);
  }
  catch(...)
  {
    delete Data;
    throw;
  }

  return Data;
}
//---------------------------------------------------------------------
TSessionData * __fastcall TStoredSessionList::ResolveSessionData(TSessionData * Data)
{
  if (!Data->Link.IsEmpty())
  {
    Data = dynamic_cast<TSessionData *>(FindByName(Data->Link));
    if (Data != NULL)
    {
      Data = ResolveSessionData(Data);
    }
  }
  return Data;
}
//---------------------------------------------------------------------
bool __fastcall TStoredSessionList::CanLogin(TSessionData * Data)
{
  Data = ResolveSessionData(Data);
  return (Data != NULL) && Data->CanLogin;
}
//---------------------------------------------------------------------
UnicodeString GetExpandedLogFileName(UnicodeString LogFileName, TSessionData * SessionData)
{
  UnicodeString ANewFileName = StripPathQuotes(ExpandEnvironmentVariables(LogFileName));
  TDateTime N = Now();
  for (int Index = 1; Index < ANewFileName.Length(); Index++)
  {
    if (ANewFileName[Index] == L'!')
    {
      UnicodeString Replacement;
      // keep consistent with TFileCustomCommand::PatternReplacement
      switch (tolower(ANewFileName[Index + 1]))
      {
        case L'y':
          Replacement = FormatDateTime(L"yyyy", N);
          break;

        case L'm':
          Replacement = FormatDateTime(L"mm", N);
          break;

        case L'd':
          Replacement = FormatDateTime(L"dd", N);
          break;

        case L't':
          Replacement = FormatDateTime(L"hhnnss", N);
          break;

        case 'p':
          Replacement = IntToStr(static_cast<int>(GetCurrentProcessId()));
          break;

        case L'@':
          Replacement = MakeValidFileName(SessionData->HostNameExpanded);
          break;

        case L's':
          Replacement = MakeValidFileName(SessionData->SessionName);
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
//---------------------------------------------------------------------
