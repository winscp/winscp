//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SessionData.h"

#include "Common.h"
#include "Exceptions.h"
#include "FileBuffer.h"
#include "CoreMain.h"
#include "Security.h"
#include "TextsCore.h"
#include "PuttyIntf.h"
#include "RemoteFiles.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
enum TProxyType { pxNone, pxHTTP, pxSocks, pxTelnet }; // 0.53b and older
const char * DefaultSessionName = "Default Settings";
const char CipherNames[CIPHER_COUNT][10] = {"WARN", "3des", "blowfish", "aes", "des", "arcfour"};
const char KexNames[KEX_COUNT][20] = {"WARN", "dh-group1-sha1", "dh-group14-sha1", "dh-gex-sha1", "rsa" };
const char ProtocolNames[PROTOCOL_COUNT][10] = { "raw", "telnet", "rlogin", "ssh" };
const char SshProtList[][10] = {"1 only", "1", "2", "2 only"};
const char ProxyMethodList[][10] = {"none", "SOCKS4", "SOCKS5", "HTTP", "Telnet", "Cmd" };
const TCipher DefaultCipherList[CIPHER_COUNT] =
  { cipAES, cipBlowfish, cip3DES, cipWarn, cipArcfour, cipDES };
const TKex DefaultKexList[KEX_COUNT] =
  { kexDHGEx, kexDHGroup14, kexDHGroup1, kexRSA, kexWarn };
const char FSProtocolNames[FSPROTOCOL_COUNT][11] = { "SCP", "SFTP (SCP)", "SFTP", "", "", "FTP" };
const int SshPortNumber = 22;
const int FtpPortNumber = 21;
//---------------------------------------------------------------------
TDateTime __fastcall SecToDateTime(int Sec)
{
  return TDateTime((unsigned short)(Sec/60/60),
    (unsigned short)(Sec/60%60), (unsigned short)(Sec%60), 0);
}
//--- TSessionData ----------------------------------------------------
__fastcall TSessionData::TSessionData(AnsiString aName):
  TNamedObject(aName)
{
  Default();
  FModified = true;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Default()
{
  HostName = "";
  PortNumber = SshPortNumber;
  UserName = "";
  Password = "";
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
  GSSAPIServerRealm = "";
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
  PublicKeyFile = "";
  FProtocol = ptSSH;
  TcpNoDelay = true;
  HostKey = "";

  ProxyMethod = pmNone;
  ProxyHost = "proxy";
  ProxyPort = 80;
  ProxyUsername = "";
  ProxyPassword = "";
  ProxyTelnetCommand = "connect %host %port\\n";
  ProxyLocalCommand = "";
  ProxyDNS = asAuto;
  ProxyLocalhost = false;

  for (int Index = 0; Index < LENOF(FBugs); Index++)
  {
    Bug[(TSshBug)Index] = asAuto;
  }

  Special = false;
  FSProtocol = fsSFTP;
  AddressFamily = afAuto;
  RekeyData = "1G";
  RekeyTime = 60;

  // FS common
  LocalDirectory = "";
  RemoteDirectory = "";
  UpdateDirectories = false;
  CacheDirectories = true;
  CacheDirectoryChanges = true;
  PreserveDirectoryChanges = true;
  LockInHome = false;
  ResolveSymlinks = true;
  DSTMode = dstmUnix;
  DeleteToRecycleBin = false;
  OverwrittenToRecycleBin = false;
  RecycleBinPath = "/tmp";
  Color = 0;
  PostLoginCommands = "";

  // SCP
  ReturnVar = "";
  LookupUserGroups = true;
  EOLType = eolLF;
  Shell = ""; //default shell
  ReturnVar = "";
  ClearAliases = true;
  UnsetNationalVars = true;
  ListingCommand = "ls -la";
  IgnoreLsWarnings = true;
  Scp1Compatibility = false;
  TimeDifference = 0;
  SCPLsFullTime = asAuto;
  Utf = asAuto;
  FtpListAll = asAuto;

  // SFTP
  SftpServer = "";
  SFTPDownloadQueue = 4;
  SFTPUploadQueue = 4;
  SFTPListingQueue = 2;
  SFTPMaxVersion = 5;
  SFTPMaxPacketSize = 0;

  for (int Index = 0; Index < LENOF(FSFTPBugs); Index++)
  {
    SFTPBug[(TSftpBug)Index] = asAuto;
  }

  Tunnel = false;
  TunnelHostName = "";
  TunnelPortNumber = SshPortNumber;
  TunnelUserName = "";
  TunnelPassword = "";
  TunnelPublicKeyFile = "";
  TunnelLocalPortNumber = 0;
  TunnelPortFwd = "";

  // FTP
  FtpPasvMode = false;
  FtpAccount = "";
  FtpPingInterval = 30;
  FtpPingType = ptDummyCommand;
  Ftps = ftpsNone;

  CustomParam1 = "";
  CustomParam2 = "";

  Selected = false;
  FModified = false;

  // add also to TSessionLog::AddStartupInfo()
}
//---------------------------------------------------------------------
void __fastcall TSessionData::NonPersistant()
{
  UpdateDirectories = false;
  PreserveDirectoryChanges = false;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Assign(TPersistent * Source)
{
  if (Source && Source->InheritsFrom(__classid(TSessionData)))
  {
    #define DUPL(P) P = ((TSessionData *)Source)->P
    DUPL(Name);
    DUPL(HostName);
    DUPL(PortNumber);
    DUPL(UserName);
    DUPL(Password);
    DUPL(PingInterval);
    DUPL(PingType);
    DUPL(Timeout);
    DUPL(TryAgent);
    DUPL(AgentFwd);
    DUPL(AuthTIS);
    DUPL(ChangeUsername);
    DUPL(Compression);
    DUPL(SshProt);
    DUPL(Ssh2DES);
    DUPL(SshNoUserAuth);
    DUPL(CipherList);
    DUPL(KexList);
    DUPL(PublicKeyFile);
    DUPL(AddressFamily);
    DUPL(RekeyData);
    DUPL(RekeyTime);
    DUPL(HostKey);

    DUPL(FSProtocol);
    DUPL(LocalDirectory);
    DUPL(RemoteDirectory);
    DUPL(UpdateDirectories);
    DUPL(CacheDirectories);
    DUPL(CacheDirectoryChanges);
    DUPL(PreserveDirectoryChanges);

    DUPL(ResolveSymlinks);
    DUPL(DSTMode);
    DUPL(LockInHome);
    DUPL(Special);
    DUPL(Selected);
    DUPL(ReturnVar);
    DUPL(LookupUserGroups);
    DUPL(EOLType);
    DUPL(Shell);
    DUPL(ClearAliases);
    DUPL(Scp1Compatibility);
    DUPL(UnsetNationalVars);
    DUPL(ListingCommand);
    DUPL(IgnoreLsWarnings);
    DUPL(SCPLsFullTime);
    DUPL(FtpListAll);

    DUPL(TimeDifference);
    // new in 53b
    DUPL(TcpNoDelay);
    DUPL(AuthKI);
    DUPL(AuthKIPassword);
    DUPL(AuthGSSAPI);
    DUPL(GSSAPIFwdTGT);
    DUPL(GSSAPIServerRealm);
    DUPL(DeleteToRecycleBin);
    DUPL(OverwrittenToRecycleBin);
    DUPL(RecycleBinPath);
    DUPL(Utf);
    DUPL(PostLoginCommands);

    DUPL(ProxyMethod);
    DUPL(ProxyHost);
    DUPL(ProxyPort);
    DUPL(ProxyUsername);
    DUPL(ProxyPassword);
    DUPL(ProxyTelnetCommand);
    DUPL(ProxyLocalCommand);
    DUPL(ProxyDNS);
    DUPL(ProxyLocalhost);

    for (int Index = 0; Index < LENOF(FBugs); Index++)
    {
      DUPL(Bug[(TSshBug)Index]);
    }

    // SFTP
    DUPL(SftpServer);
    DUPL(SFTPDownloadQueue);
    DUPL(SFTPUploadQueue);
    DUPL(SFTPListingQueue);
    DUPL(SFTPMaxVersion);
    DUPL(SFTPMaxPacketSize);

    for (int Index = 0; Index < LENOF(FSFTPBugs); Index++)
    {
      DUPL(SFTPBug[(TSftpBug)Index]);
    }

    DUPL(Color);

    DUPL(Tunnel);
    DUPL(TunnelHostName);
    DUPL(TunnelPortNumber);
    DUPL(TunnelUserName);
    DUPL(TunnelPassword);
    DUPL(TunnelPublicKeyFile);
    DUPL(TunnelLocalPortNumber);
    DUPL(TunnelPortFwd);

    DUPL(FtpPasvMode);
    DUPL(FtpAccount);
    DUPL(FtpPingInterval);
    DUPL(FtpPingType);
    DUPL(Ftps);

    DUPL(CustomParam1);
    DUPL(CustomParam2);

    #undef DUPL
    FModified = ((TSessionData *)Source)->Modified;
  }
  else
  {
    TNamedObject::Assign(Source);
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Load(THierarchicalStorage * Storage)
{
  bool RewritePassword = false;
  if (Storage->OpenSubKey(InternalStorageKey, False))
  {
    PortNumber = Storage->ReadInteger("PortNumber", PortNumber);
    UserName = Storage->ReadString("UserName", UserName);
    // must be loaded after UserName, because HostName may be in format user@host
    HostName = Storage->ReadString("HostName", HostName);

    if (!Configuration->DisablePasswordStoring)
    {
      if (Storage->ValueExists("PasswordPlain"))
      {
        Password = Storage->ReadString("PasswordPlain", Password);
        RewritePassword = true;
      }
      else
      {
        FPassword = Storage->ReadString("Password", FPassword);
      }
    }
    // Putty uses PingIntervalSecs
    int PingIntervalSecs = Storage->ReadInteger("PingIntervalSecs", -1);
    if (PingIntervalSecs < 0)
    {
      PingIntervalSecs = Storage->ReadInteger("PingIntervalSec", PingInterval%60);
    }
    PingInterval =
      Storage->ReadInteger("PingInterval", PingInterval/60)*60 +
      PingIntervalSecs;
    if (PingInterval == 0)
    {
      PingInterval = 30;
    }
    // PingType has not existed before 3.5, where PingInterval > 0 meant today's ptNullPacket
    // Since 3.5, until 4.1 PingType was stored unconditionally.
    // Since 4.1 PingType is stored when it is not ptOff (default) or
    // when PingInterval is stored.
    if (!Storage->ValueExists("PingType"))
    {
      if (Storage->ReadInteger("PingInterval", 0) > 0)
      {
        PingType = ptNullPacket;
      }
    }
    else
    {
      PingType = static_cast<TPingType>(Storage->ReadInteger("PingType", ptOff));
    }
    Timeout = Storage->ReadInteger("Timeout", Timeout);
    TryAgent = Storage->ReadBool("TryAgent", TryAgent);
    AgentFwd = Storage->ReadBool("AgentFwd", AgentFwd);
    AuthTIS = Storage->ReadBool("AuthTIS", AuthTIS);
    AuthKI = Storage->ReadBool("AuthKI", AuthKI);
    AuthKIPassword = Storage->ReadBool("AuthKIPassword", AuthKIPassword);
    // Continue to use setting keys of previous kerberos implementation (vaclav tomec),
    // but fallback to keys of other implementations (official putty and vintela quest putty),
    // to allow imports from all putty versions.
    // Both vaclav tomec and official putty use AuthGSSAPI
    AuthGSSAPI = Storage->ReadBool("AuthGSSAPI", Storage->ReadBool("AuthSSPI", AuthGSSAPI));
    GSSAPIFwdTGT = Storage->ReadBool("GSSAPIFwdTGT", Storage->ReadBool("GssapiFwd", Storage->ReadBool("SSPIFwdTGT", GSSAPIFwdTGT)));
    GSSAPIServerRealm = Storage->ReadString("GSSAPIServerRealm", Storage->ReadString("KerbPrincipal", GSSAPIServerRealm));
    ChangeUsername = Storage->ReadBool("ChangeUsername", ChangeUsername);
    Compression = Storage->ReadBool("Compression", Compression);
    SshProt = (TSshProt)Storage->ReadInteger("SshProt", SshProt);
    Ssh2DES = Storage->ReadBool("Ssh2DES", Ssh2DES);
    SshNoUserAuth = Storage->ReadBool("SshNoUserAuth", SshNoUserAuth);
    CipherList = Storage->ReadString("Cipher", CipherList);
    KexList = Storage->ReadString("KEX", KexList);
    PublicKeyFile = Storage->ReadString("PublicKeyFile", PublicKeyFile);
    AddressFamily = static_cast<TAddressFamily>
      (Storage->ReadInteger("AddressFamily", AddressFamily));
    RekeyData = Storage->ReadString("RekeyBytes", RekeyData);
    RekeyTime = Storage->ReadInteger("RekeyTime", RekeyTime);

    FSProtocol = (TFSProtocol)Storage->ReadInteger("FSProtocol", FSProtocol);
    LocalDirectory = Storage->ReadString("LocalDirectory", LocalDirectory);
    RemoteDirectory = Storage->ReadString("RemoteDirectory", RemoteDirectory);
    UpdateDirectories = Storage->ReadBool("UpdateDirectories", UpdateDirectories);
    CacheDirectories = Storage->ReadBool("CacheDirectories", CacheDirectories);
    CacheDirectoryChanges = Storage->ReadBool("CacheDirectoryChanges", CacheDirectoryChanges);
    PreserveDirectoryChanges = Storage->ReadBool("PreserveDirectoryChanges", PreserveDirectoryChanges);

    ResolveSymlinks = Storage->ReadBool("ResolveSymlinks", ResolveSymlinks);
    DSTMode = (TDSTMode)Storage->ReadInteger("ConsiderDST", DSTMode);
    LockInHome = Storage->ReadBool("LockInHome", LockInHome);
    Special = Storage->ReadBool("Special", Special);
    Shell = Storage->ReadString("Shell", Shell);
    ClearAliases = Storage->ReadBool("ClearAliases", ClearAliases);
    UnsetNationalVars = Storage->ReadBool("UnsetNationalVars", UnsetNationalVars);
    ListingCommand = Storage->ReadString("ListingCommand",
      Storage->ReadBool("AliasGroupList", false) ? AnsiString("ls -gla") : ListingCommand);
    IgnoreLsWarnings = Storage->ReadBool("IgnoreLsWarnings", IgnoreLsWarnings);
    SCPLsFullTime = TAutoSwitch(Storage->ReadInteger("SCPLsFullTime", SCPLsFullTime));
    FtpListAll = TAutoSwitch(Storage->ReadInteger("FtpListAll", FtpListAll));
    Scp1Compatibility = Storage->ReadBool("Scp1Compatibility", Scp1Compatibility);
    TimeDifference = Storage->ReadFloat("TimeDifference", TimeDifference);
    DeleteToRecycleBin = Storage->ReadBool("DeleteToRecycleBin", DeleteToRecycleBin);
    OverwrittenToRecycleBin = Storage->ReadBool("OverwrittenToRecycleBin", OverwrittenToRecycleBin);
    RecycleBinPath = Storage->ReadString("RecycleBinPath", RecycleBinPath);
    PostLoginCommands = Storage->ReadString("PostLoginCommands", PostLoginCommands);

    ReturnVar = Storage->ReadString("ReturnVar", ReturnVar);
    LookupUserGroups = Storage->ReadBool("LookupUserGroups", LookupUserGroups);
    EOLType = (TEOLType)Storage->ReadInteger("EOLType", EOLType);
    Utf = TAutoSwitch(Storage->ReadInteger("Utf", Storage->ReadInteger("SFTPUtfBug", Utf)));

    TcpNoDelay = Storage->ReadBool("TcpNoDelay", TcpNoDelay);

    ProxyMethod = (TProxyMethod)Storage->ReadInteger("ProxyMethod", -1);
    if (ProxyMethod < 0)
    {
      int ProxyType = Storage->ReadInteger("ProxyType", pxNone);
      int ProxySOCKSVersion;
      switch (ProxyType) {
        case pxHTTP:
          ProxyMethod = pmHTTP;
          break;
        case pxTelnet:
          ProxyMethod = pmTelnet;
          break;
        case pxSocks:
          ProxySOCKSVersion = Storage->ReadInteger("ProxySOCKSVersion", 5);
          ProxyMethod = ProxySOCKSVersion == 5 ? pmSocks5 : pmSocks4;
          break;
        default:
        case pxNone:
          ProxyMethod = pmNone;
          break;
      }
    }
    ProxyHost = Storage->ReadString("ProxyHost", ProxyHost);
    ProxyPort = Storage->ReadInteger("ProxyPort", ProxyPort);
    ProxyUsername = Storage->ReadString("ProxyUsername", ProxyUsername);
    if (Storage->ValueExists("ProxyPassword"))
    {
      // encrypt unencrypted password
      ProxyPassword = Storage->ReadString("ProxyPassword", "");
    }
    else
    {
      // load encrypted password
      FProxyPassword = Storage->ReadString("ProxyPasswordEnc", FProxyPassword);
    }
    if (ProxyMethod == pmCmd)
    {
      ProxyLocalCommand = Storage->ReadStringRaw("ProxyTelnetCommand", ProxyLocalCommand);
    }
    else
    {
      ProxyTelnetCommand = Storage->ReadStringRaw("ProxyTelnetCommand", ProxyTelnetCommand);
    }
    ProxyDNS = TAutoSwitch((Storage->ReadInteger("ProxyDNS", (ProxyDNS + 2) % 3) + 1) % 3);
    ProxyLocalhost = Storage->ReadBool("ProxyLocalhost", ProxyLocalhost);

    #define READ_BUG(BUG) \
      Bug[sb##BUG] = TAutoSwitch(2 - Storage->ReadInteger("Bug"#BUG, \
        2 - Bug[sb##BUG]));
    READ_BUG(Ignore1);
    READ_BUG(PlainPW1);
    READ_BUG(RSA1);
    READ_BUG(HMAC2);
    READ_BUG(DeriveKey2);
    READ_BUG(RSAPad2);
    READ_BUG(Rekey2);
    READ_BUG(PKSessID2);
    READ_BUG(MaxPkt2);
    #undef READ_BUG

    if ((Bug[sbHMAC2] == asAuto) &&
        Storage->ReadBool("BuggyMAC", false))
    {
        Bug[sbHMAC2] = asOn;
    }

    SftpServer = Storage->ReadString("SftpServer", SftpServer);
    #define READ_SFTP_BUG(BUG) \
      SFTPBug[sb##BUG] = TAutoSwitch(Storage->ReadInteger("SFTP" #BUG "Bug", SFTPBug[sb##BUG]));
    READ_SFTP_BUG(Symlink);
    READ_SFTP_BUG(SignedTS);
    #undef READ_SFTP_BUG

    SFTPMaxVersion = Storage->ReadInteger("SFTPMaxVersion", SFTPMaxVersion);
    SFTPMaxPacketSize = Storage->ReadInteger("SFTPMaxPacketSize", SFTPMaxPacketSize);

    Color = Storage->ReadInteger("Color", Color);

    ProtocolStr = Storage->ReadString("Protocol", ProtocolStr);

    Tunnel = Storage->ReadBool("Tunnel", Tunnel);
    TunnelPortNumber = Storage->ReadInteger("TunnelPortNumber", TunnelPortNumber);
    TunnelUserName = Storage->ReadString("TunnelUserName", TunnelUserName);
    // must be loaded after TunnelUserName,
    // because TunnelHostName may be in format user@host
    TunnelHostName = Storage->ReadString("TunnelHostName", TunnelHostName);
    if (!Configuration->DisablePasswordStoring)
    {
      FTunnelPassword = Storage->ReadString("TunnelPassword", FTunnelPassword);
    }
    TunnelPublicKeyFile = Storage->ReadString("TunnelPublicKeyFile", TunnelPublicKeyFile);
    TunnelLocalPortNumber = Storage->ReadInteger("TunnelLocalPortNumber", TunnelLocalPortNumber);

    // Ftp prefix
    FtpPasvMode = Storage->ReadBool("FtpPasvMode", FtpPasvMode);
    FtpAccount = Storage->ReadString("FtpAccount", FtpAccount);
    FtpPingInterval = Storage->ReadInteger("FtpPingInterval", FtpPingInterval);
    FtpPingType = static_cast<TPingType>(Storage->ReadInteger("FtpPingType", FtpPingType));
    Ftps = static_cast<TFtps>(Storage->ReadInteger("Ftps", Ftps));

    CustomParam1 = Storage->ReadString("CustomParam1", CustomParam1);
    CustomParam2 = Storage->ReadString("CustomParam2", CustomParam2);

    Storage->CloseSubKey();
  };

  if (RewritePassword)
  {
    TStorageAccessMode AccessMode = Storage->AccessMode;
    Storage->AccessMode = smReadWrite;

    try
    {
      if (Storage->OpenSubKey(InternalStorageKey, true))
      {
        Storage->DeleteValue("PasswordPlain");
        if (!Password.IsEmpty())
        {
          Storage->WriteString("Password", FPassword);
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
    #define WRITE_DATA(TYPE, PROPERTY) WRITE_DATA_EX(TYPE, #PROPERTY, PROPERTY, )

    WRITE_DATA(String, HostName);
    WRITE_DATA(Integer, PortNumber);
    if (!Configuration->DisablePasswordStoring && !PuttyExport && !Password.IsEmpty())
    {
      WRITE_DATA_EX(String, "Password", FPassword, );
    }
    else
    {
      Storage->DeleteValue("Password");
    }
    Storage->DeleteValue("PasswordPlain");
    WRITE_DATA_EX(Integer, "PingInterval", PingInterval / 60, );
    WRITE_DATA_EX(Integer, "PingIntervalSecs", PingInterval % 60, );
    Storage->DeleteValue("PingIntervalSec"); // obsolete
    // when PingInterval is stored always store PingType not to attempt to
    // deduce PingType from PingInterval (backward compatibility with pre 3.5)
    if (((Default != NULL) && (PingType != Default->PingType)) ||
        Storage->ValueExists("PingInterval"))
    {
      Storage->WriteInteger("PingType", PingType);
    }
    else
    {
      Storage->DeleteValue("PingType");
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
    Storage->DeleteValue("TryGSSKEX");
    Storage->DeleteValue("UserNameFromEnvironment");
    Storage->DeleteValue("GSSAPIServerChoosesUserName");
    Storage->DeleteValue("GSSAPITrustDNS");
    if (PuttyExport)
    {
      // duplicate kerberos setting with keys of the vintela quest putty
      WRITE_DATA_EX(Bool, "AuthSSPI", AuthGSSAPI, );
      WRITE_DATA_EX(Bool, "SSPIFwdTGT", GSSAPIFwdTGT, );
      WRITE_DATA_EX(String, "KerbPrincipal", GSSAPIServerRealm, );
      // duplicate kerberos setting with keys of the official putty
      WRITE_DATA_EX(Bool, "GssapiFwd", GSSAPIFwdTGT, );
    }

    WRITE_DATA(Bool, ChangeUsername);
    WRITE_DATA(Bool, Compression);
    WRITE_DATA(Integer, SshProt);
    WRITE_DATA(Bool, Ssh2DES);
    WRITE_DATA(Bool, SshNoUserAuth);
    WRITE_DATA_EX(String, "Cipher", CipherList, );
    WRITE_DATA_EX(String, "KEX", KexList, );
    WRITE_DATA(Integer, AddressFamily);
    WRITE_DATA_EX(String, "RekeyBytes", RekeyData, );
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
      WRITE_DATA(Bool, UpdateDirectories);
      WRITE_DATA(Bool, CacheDirectories);
      WRITE_DATA(Bool, CacheDirectoryChanges);
      WRITE_DATA(Bool, PreserveDirectoryChanges);

      WRITE_DATA(Bool, ResolveSymlinks);
      WRITE_DATA_EX(Integer, "ConsiderDST", DSTMode, );
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
      WRITE_DATA(Integer, FtpListAll);
      WRITE_DATA(Bool, Scp1Compatibility);
      WRITE_DATA(Float, TimeDifference);
      WRITE_DATA(Bool, DeleteToRecycleBin);
      WRITE_DATA(Bool, OverwrittenToRecycleBin);
      WRITE_DATA(String, RecycleBinPath);
      WRITE_DATA(String, PostLoginCommands);

      WRITE_DATA(String, ReturnVar);
      WRITE_DATA(Bool, LookupUserGroups);
      WRITE_DATA(Integer, EOLType);
      Storage->DeleteValue("SFTPUtfBug");
      WRITE_DATA(Integer, Utf);
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
        case pmNone:
          ProxyType = pxNone;
          break;
      }
      Storage->WriteInteger("ProxyType", ProxyType);
      Storage->WriteInteger("ProxySOCKSVersion", ProxySOCKSVersion);
    }
    else
    {
      Storage->DeleteValue("ProxyType");
      Storage->DeleteValue("ProxySOCKSVersion");
    }
    WRITE_DATA(String, ProxyHost);
    WRITE_DATA(Integer, ProxyPort);
    WRITE_DATA(String, ProxyUsername);
    if (PuttyExport)
    {
      // save password unencrypted
      WRITE_DATA(String, ProxyPassword);
    }
    else
    {
      // save password encrypted
      if (!ProxyPassword.IsEmpty())
      {
        WRITE_DATA_EX(String, "ProxyPasswordEnc", FProxyPassword, );
      }
      else
      {
        Storage->DeleteValue("ProxyPasswordEnc");
      }
      Storage->DeleteValue("ProxyPassword");
    }
    if (ProxyMethod == pmCmd)
    {
      WRITE_DATA_EX(StringRaw, "ProxyTelnetCommand", ProxyLocalCommand, );
    }
    else
    {
      WRITE_DATA(StringRaw, ProxyTelnetCommand);
    }
    #define WRITE_DATA_CONV_FUNC(X) (((X) + 2) % 3)
    WRITE_DATA_CONV(Integer, "ProxyDNS", ProxyDNS);
    #undef WRITE_DATA_CONV_FUNC
    WRITE_DATA(Bool, ProxyLocalhost);

    #define WRITE_DATA_CONV_FUNC(X) (2 - (X))
    #define WRITE_BUG(BUG) WRITE_DATA_CONV(Integer, "Bug" #BUG, Bug[sb##BUG]);
    WRITE_BUG(Ignore1);
    WRITE_BUG(PlainPW1);
    WRITE_BUG(RSA1);
    WRITE_BUG(HMAC2);
    WRITE_BUG(DeriveKey2);
    WRITE_BUG(RSAPad2);
    WRITE_BUG(Rekey2);
    WRITE_BUG(PKSessID2);
    WRITE_BUG(MaxPkt2);
    #undef WRITE_BUG
    #undef WRITE_DATA_CONV_FUNC

    Storage->DeleteValue("BuggyMAC");
    Storage->DeleteValue("AliasGroupList");

    if (PuttyExport)
    {
      WRITE_DATA_EX(String, "Protocol", ProtocolStr, );
    }

    if (!PuttyExport)
    {
      WRITE_DATA(String, SftpServer);

      #define WRITE_SFTP_BUG(BUG) WRITE_DATA_EX(Integer, "SFTP" #BUG "Bug", SFTPBug[sb##BUG], );
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
      if (!Configuration->DisablePasswordStoring && !TunnelPassword.IsEmpty())
      {
        WRITE_DATA_EX(String, "TunnelPassword", FTunnelPassword, );
      }
      else
      {
        Storage->DeleteValue("TunnelPassword");
      }
      WRITE_DATA(String, TunnelPublicKeyFile);
      WRITE_DATA(Integer, TunnelLocalPortNumber);

      WRITE_DATA(Bool, FtpPasvMode);
      WRITE_DATA(String, FtpAccount);
      WRITE_DATA(Integer, FtpPingInterval);
      WRITE_DATA(Integer, FtpPingType);
      WRITE_DATA(Integer, Ftps);

      WRITE_DATA(String, CustomParam1);
      WRITE_DATA(String, CustomParam2);
    }

    Storage->CloseSubKey();
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
bool __fastcall TSessionData::ParseUrl(AnsiString Url, TOptions * Options,
  TStoredSessionList * StoredSessions, bool & DefaultsOnly, AnsiString * FileName,
  bool * AProtocolDefined)
{
  bool ProtocolDefined = false;
  TFSProtocol AFSProtocol;
  int APortNumber;
  if (Url.SubString(1, 4).LowerCase() == "scp:")
  {
    AFSProtocol = fsSCPonly;
    APortNumber = SshPortNumber;
    Url.Delete(1, 4);
    ProtocolDefined = true;
  }
  else if (Url.SubString(1, 5).LowerCase() == "sftp:")
  {
    AFSProtocol = fsSFTPonly;
    APortNumber = SshPortNumber;
    Url.Delete(1, 5);
    ProtocolDefined = true;
  }
  else if (Url.SubString(1, 4).LowerCase() == "ftp:")
  {
    AFSProtocol = fsFTP;
    APortNumber = FtpPortNumber;
    Url.Delete(1, 4);
    ProtocolDefined = true;
  }

  if (ProtocolDefined && (Url.SubString(1, 2) == "//"))
  {
    Url.Delete(1, 2);
  }

  if (AProtocolDefined != NULL)
  {
    *AProtocolDefined = ProtocolDefined;
  }

  if (!Url.IsEmpty())
  {
    AnsiString DecodedUrl = DecodeUrlChars(Url);
    // lookup stored session even if protocol was defined
    // (this allows setting for example default username for host
    // by creating stored session named by host)
    TSessionData * Data = NULL;
    for (Integer Index = 0; Index < StoredSessions->Count + StoredSessions->HiddenCount; Index++)
    {
      TSessionData * AData = (TSessionData *)StoredSessions->Items[Index];
      if (AnsiSameText(AData->Name, DecodedUrl) ||
          AnsiSameText(AData->Name + "/", DecodedUrl.SubString(1, AData->Name.Length() + 1)))
      {
        Data = AData;
        break;
      }
    }

    AnsiString ARemoteDirectory;

    if (Data != NULL)
    {
      DefaultsOnly = false;
      Assign(Data);
      int P = 1;
      while (!AnsiSameText(DecodeUrlChars(Url.SubString(1, P)), Data->Name))
      {
        P++;
        assert(P <= Url.Length());
      }
      ARemoteDirectory = Url.SubString(P + 1, Url.Length() - P);

      if (StoredSessions->IsHidden(Data))
      {
        Data->Remove();
        StoredSessions->Remove(Data);
        // only modified, implicit
        StoredSessions->Save(false, false);
      }
    }
    else
    {
      Assign(StoredSessions->DefaultSettings);
      Name = "";

      int PSlash = Url.Pos("/");
      if (PSlash == 0)
      {
        PSlash = Url.Length() + 1;
      }

      AnsiString ConnectInfo = Url.SubString(1, PSlash - 1);

      int P = ConnectInfo.LastDelimiter("@");

      AnsiString UserInfo;
      AnsiString HostInfo;

      if (P > 0)
      {
        UserInfo = ConnectInfo.SubString(1, P - 1);
        HostInfo = ConnectInfo.SubString(P + 1, ConnectInfo.Length() - P);
      }
      else
      {
        HostInfo = ConnectInfo;
      }

      HostName = DecodeUrlChars(CutToChar(HostInfo, ':', true));

      // expanded from ?: operator, as it caused strange "access violation" errors
      if (!HostInfo.IsEmpty())
      {
        PortNumber = StrToIntDef(DecodeUrlChars(HostInfo), -1);
      }
      else if (ProtocolDefined)
      {
        PortNumber = APortNumber;
      }

      UserName = DecodeUrlChars(CutToChar(UserInfo, ':', false));
      Password = DecodeUrlChars(UserInfo);

      ARemoteDirectory = Url.SubString(PSlash, Url.Length() - PSlash + 1);
    }

    if (!ARemoteDirectory.IsEmpty() && (ARemoteDirectory != "/"))
    {
      if ((ARemoteDirectory[ARemoteDirectory.Length()] != '/') &&
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
    // we deliberatelly do keep defaultonly to false, in presence of any option,
    // as the option should not make session "connectable"

    AnsiString Value;
    if (Options->FindSwitch("privatekey", Value))
    {
      PublicKeyFile = Value;
    }
    if (Options->FindSwitch("timeout", Value))
    {
      Timeout = StrToInt(Value);
    }
    if (Options->FindSwitch("hostkey", Value))
    {
      HostKey = Value;
    }
    if (Options->FindSwitch("passive", Value))
    {
      FtpPasvMode = (StrToIntDef(Value, 1) != 0);
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

  HostName = "127.0.0.1";
  PortNumber = APortNumber;
  // proxy settings is used for tunnel
  ProxyMethod = pmNone;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::RollbackTunnel()
{
  HostName = FOrigHostName;
  PortNumber = FOrigPortNumber;
  ProxyMethod = FOrigProxyMethod;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::ValidatePath(const AnsiString Path)
{
  // noop
}
//---------------------------------------------------------------------
void __fastcall TSessionData::ValidateName(const AnsiString Name)
{
  if (Name.LastDelimiter("/") > 0)
  {
    throw Exception(FMTLOAD(ITEM_NAME_INVALID, (Name, "/")));
  }
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::GetCanLogin()
{
  return !FHostName.IsEmpty();
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetSessionKey()
{
  return FORMAT("%s@%s", (UserName, HostName));
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetInternalStorageKey()
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
AnsiString __fastcall TSessionData::GetStorageKey()
{
  return SessionName;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetHostName(AnsiString value)
{
  if (FHostName != value)
  {
    // HostName is key for password encryption
    AnsiString XPassword = Password;

    int P = value.LastDelimiter("@");
    if (P > 0)
    {
      UserName = value.SubString(1, P - 1);
      value = value.SubString(P + 1, value.Length() - P);
    }
    FHostName = value;
    FModified = true;

    Password = XPassword;
    if (!XPassword.IsEmpty())
    {
      XPassword.Unique();
      memset(XPassword.c_str(), 0, XPassword.Length());
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPortNumber(int value)
{
  SET_SESSION_PROPERTY(PortNumber);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetShell(AnsiString value)
{
  SET_SESSION_PROPERTY(Shell);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetSftpServer(AnsiString value)
{
  SET_SESSION_PROPERTY(SftpServer);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetClearAliases(bool value)
{
  SET_SESSION_PROPERTY(ClearAliases);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetListingCommand(AnsiString value)
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
void __fastcall TSessionData::SetUserName(AnsiString value)
{
  // UserName is key for password encryption
  AnsiString XPassword = Password;
  SET_SESSION_PROPERTY(UserName);
  Password = XPassword;
  if (!XPassword.IsEmpty())
  {
    XPassword.Unique();
    memset(XPassword.c_str(), 0, XPassword.Length());
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPassword(AnsiString value)
{
  value = EncryptPassword(value, UserName+HostName);
  SET_SESSION_PROPERTY(Password);
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetPassword()
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
void __fastcall TSessionData::SetGSSAPIServerRealm(AnsiString value)
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
AnsiString __fastcall TSessionData::GetSshProtStr()
{
  return SshProtList[FSshProt];
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::GetUsesSsh()
{
  return (FSProtocol != fsFTP);
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
void __fastcall TSessionData::SetCipherList(AnsiString value)
{
  bool Used[CIPHER_COUNT];
  for (int C = 0; C < CIPHER_COUNT; C++) Used[C] = false;

  AnsiString CipherStr;
  int Index = 0;
  while (!value.IsEmpty() && (Index < CIPHER_COUNT))
  {
    CipherStr = CutToChar(value, ',', true);
    for (int C = 0; C < CIPHER_COUNT; C++)
    {
      if (!CipherStr.AnsiCompareIC(CipherNames[C]))
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
AnsiString __fastcall TSessionData::GetCipherList() const
{
  AnsiString Result;
  for (int Index = 0; Index < CIPHER_COUNT; Index++)
  {
    Result += AnsiString(Index ? "," : "") + CipherNames[Cipher[Index]];
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
void __fastcall TSessionData::SetKexList(AnsiString value)
{
  bool Used[KEX_COUNT];
  for (int K = 0; K < KEX_COUNT; K++) Used[K] = false;

  AnsiString KexStr;
  int Index = 0;
  while (!value.IsEmpty() && (Index < KEX_COUNT))
  {
    KexStr = CutToChar(value, ',', true);
    for (int K = 0; K < KEX_COUNT; K++)
    {
      if (!KexStr.AnsiCompareIC(KexNames[K]))
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
AnsiString __fastcall TSessionData::GetKexList() const
{
  AnsiString Result;
  for (int Index = 0; Index < KEX_COUNT; Index++)
  {
    Result += AnsiString(Index ? "," : "") + KexNames[Kex[Index]];
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPublicKeyFile(AnsiString value)
{
  if (FPublicKeyFile != value)
  {
    FPublicKeyFile = StripPathQuotes(value);
    FModified = true;
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetReturnVar(AnsiString value)
{
  SET_SESSION_PROPERTY(ReturnVar);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetLookupUserGroups(bool value)
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
void __fastcall TSessionData::SetProtocol(TProtocol value)
{
  SET_SESSION_PROPERTY(Protocol);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetFSProtocol(TFSProtocol value)
{
  SET_SESSION_PROPERTY(FSProtocol);
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetFSProtocolStr()
{
  assert(FSProtocol >= 0 && FSProtocol < FSPROTOCOL_COUNT);
  return FSProtocolNames[FSProtocol];
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetDetectReturnVar(bool value)
{
  if (value != DetectReturnVar)
  {
    ReturnVar = value ? "" : "$?";
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
    Shell = value ? "" : "/bin/bash";
  }
}
//---------------------------------------------------------------------------
bool __fastcall TSessionData::GetDefaultShell()
{
  return Shell.IsEmpty();
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetProtocolStr(AnsiString value)
{
  FProtocol = ptRaw;
  for (int Index = 0; Index < PROTOCOL_COUNT; Index++)
  {
    if (value.AnsiCompareIC(ProtocolNames[Index]) == 0)
    {
      FProtocol = TProtocol(Index);
      break;
    }
  }
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetProtocolStr() const
{
  return ProtocolNames[Protocol];
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPingIntervalDT(TDateTime value)
{
  unsigned short hour, min, sec, msec;

  value.DecodeTime(&hour, &min, &sec, &msec);
  PingInterval = ((int)hour)*60*60 + ((int)min)*60 + sec;
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
void __fastcall TSessionData::SetRekeyData(AnsiString value)
{
  SET_SESSION_PROPERTY(RekeyData);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetRekeyTime(unsigned int value)
{
  SET_SESSION_PROPERTY(RekeyTime);
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetSessionName()
{
  if (!Name.IsEmpty() && !TNamedObjectList::IsHidden(this) &&
      (Name != DefaultSessionName))
  {
    return Name;
  }
  else if (!HostName.IsEmpty() && !UserName.IsEmpty())
  {
    return FORMAT("%s@%s", (UserName, HostName));
  }
  else if (!HostName.IsEmpty())
  {
    return HostName;
  }
  else
  {
    return "session";
  }
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetSessionUrl()
{
  AnsiString Url;
  if (!Name.IsEmpty() && !TNamedObjectList::IsHidden(this) &&
      (Name != DefaultSessionName))
  {
    Url = Name;
  }
  else
  {
    switch (FSProtocol)
    {
      case fsSCPonly:
        Url = "scp://";
        break;

      default:
        assert(false);
        // fallback
      case fsSFTP:
      case fsSFTPonly:
        Url = "sftp://";
        break;

      case fsFTP:
        Url = "ftp://";
        break;
    }

    if (!HostName.IsEmpty() && !UserName.IsEmpty())
    {
      Url += FORMAT("%s@%s", (UserName, HostName));
    }
    else if (!HostName.IsEmpty())
    {
      Url += HostName;
    }
    else
    {
      Url = "";
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
void __fastcall TSessionData::SetLocalDirectory(AnsiString value)
{
  SET_SESSION_PROPERTY(LocalDirectory);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetRemoteDirectory(AnsiString value)
{
  SET_SESSION_PROPERTY(RemoteDirectory);
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
void __fastcall TSessionData::SetRecycleBinPath(AnsiString value)
{
  SET_SESSION_PROPERTY(RecycleBinPath);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetPostLoginCommands(AnsiString value)
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
void __fastcall TSessionData::SetProxyMethod(TProxyMethod value)
{
  SET_SESSION_PROPERTY(ProxyMethod);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyHost(AnsiString value)
{
  SET_SESSION_PROPERTY(ProxyHost);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyPort(int value)
{
  SET_SESSION_PROPERTY(ProxyPort);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyUsername(AnsiString value)
{
  SET_SESSION_PROPERTY(ProxyUsername);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyPassword(AnsiString value)
{
  value = EncryptPassword(value, ProxyUsername+ProxyHost);
  SET_SESSION_PROPERTY(ProxyPassword);
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetProxyPassword() const
{
  return DecryptPassword(FProxyPassword, ProxyUsername+ProxyHost);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyTelnetCommand(AnsiString value)
{
  SET_SESSION_PROPERTY(ProxyTelnetCommand);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyLocalCommand(AnsiString value)
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
void __fastcall TSessionData::SetBug(TSshBug Bug, TAutoSwitch value)
{
  assert(Bug >= 0 && Bug < LENOF(FBugs));
  SET_SESSION_PROPERTY(Bugs[Bug]);
}
//---------------------------------------------------------------------
TAutoSwitch __fastcall TSessionData::GetBug(TSshBug Bug) const
{
  assert(Bug >= 0 && Bug < LENOF(FBugs));
  return FBugs[Bug];
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetCustomParam1(AnsiString value)
{
  SET_SESSION_PROPERTY(CustomParam1);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetCustomParam2(AnsiString value)
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
  assert(Bug >= 0 && Bug < LENOF(FSFTPBugs));
  SET_SESSION_PROPERTY(SFTPBugs[Bug]);
}
//---------------------------------------------------------------------
TAutoSwitch __fastcall TSessionData::GetSFTPBug(TSftpBug Bug) const
{
  assert(Bug >= 0 && Bug < LENOF(FSFTPBugs));
  return FSFTPBugs[Bug];
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetSCPLsFullTime(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(SCPLsFullTime);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFtpListAll(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(FtpListAll);
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
void __fastcall TSessionData::SetTunnelHostName(AnsiString value)
{
  if (FTunnelHostName != value)
  {
    // HostName is key for password encryption
    AnsiString XTunnelPassword = TunnelPassword;

    int P = value.LastDelimiter("@");
    if (P > 0)
    {
      TunnelUserName = value.SubString(1, P - 1);
      value = value.SubString(P + 1, value.Length() - P);
    }
    FTunnelHostName = value;
    FModified = true;

    TunnelPassword = XTunnelPassword;
    if (!XTunnelPassword.IsEmpty())
    {
      XTunnelPassword.Unique();
      memset(XTunnelPassword.c_str(), 0, XTunnelPassword.Length());
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTunnelPortNumber(int value)
{
  SET_SESSION_PROPERTY(TunnelPortNumber);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTunnelUserName(AnsiString value)
{
  // TunnelUserName is key for password encryption
  AnsiString XTunnelPassword = TunnelPassword;
  SET_SESSION_PROPERTY(TunnelUserName);
  TunnelPassword = XTunnelPassword;
  if (!XTunnelPassword.IsEmpty())
  {
    XTunnelPassword.Unique();
    memset(XTunnelPassword.c_str(), 0, XTunnelPassword.Length());
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTunnelPassword(AnsiString value)
{
  value = EncryptPassword(value, TunnelUserName+TunnelHostName);
  SET_SESSION_PROPERTY(TunnelPassword);
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetTunnelPassword()
{
  return DecryptPassword(FTunnelPassword, TunnelUserName+TunnelHostName);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetTunnelPublicKeyFile(AnsiString value)
{
  if (FTunnelPublicKeyFile != value)
  {
    FTunnelPublicKeyFile = StripPathQuotes(value);
    FModified = true;
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
void __fastcall TSessionData::SetTunnelPortFwd(AnsiString value)
{
  SET_SESSION_PROPERTY(TunnelPortFwd);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFtpPasvMode(bool value)
{
  SET_SESSION_PROPERTY(FtpPasvMode);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetFtpAccount(AnsiString value)
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
//---------------------------------------------------------------------
void __fastcall TSessionData::SetUtf(TAutoSwitch value)
{
  SET_SESSION_PROPERTY(Utf);
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetHostKey(AnsiString value)
{
  SET_SESSION_PROPERTY(HostKey);
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetInfoTip()
{
  if (UsesSsh)
  {
    return FMTLOAD(SESSION_INFO_TIP,
        (HostName, UserName,
         (PublicKeyFile.IsEmpty() ? LoadStr(NO_STR) : LoadStr(YES_STR)),
         SshProtStr, FSProtocolStr));
  }
  else
  {
    return FMTLOAD(SESSION_INFO_TIP_NO_SSH,
      (HostName, UserName, FSProtocolStr));
  }
}
//=== TStoredSessionList ----------------------------------------------
__fastcall TStoredSessionList::TStoredSessionList(bool aReadOnly):
  TNamedObjectList(), FReadOnly(aReadOnly)
{
  assert(Configuration);
  FDefaultSettings = new TSessionData(DefaultSessionName);
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
      AnsiString SessionName = SubKeys->Strings[Index];
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
            SessionData = new TSessionData("");
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
void __fastcall TStoredSessionList::Load(AnsiString aKey, bool UseDefaults)
{
  TRegistryStorage * Storage = new TRegistryStorage(aKey);
  try {
    if (Storage->OpenRootKey(False)) Load(Storage, false, UseDefaults);
  } __finally {
    delete Storage;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Load()
{
  THierarchicalStorage * Storage = Configuration->CreateScpStorage(true);
  try {
    if (Storage->OpenSubKey(Configuration->StoredSessionsSubKey, False))
      Load(Storage);
  } __finally {
    delete Storage;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Save(THierarchicalStorage * Storage, bool All)
{
  TSessionData * FactoryDefaults = new TSessionData("");
  try
  {
    if (All || FDefaultSettings->Modified)
    {
      FDefaultSettings->Save(Storage, false, FactoryDefaults);
    }
    for (int Index = 0; Index < Count+HiddenCount; Index++)
    {
      TSessionData *SessionData = (TSessionData *)Items[Index];
      if (All || SessionData->Modified)
      {
        SessionData->Save(Storage, false, FactoryDefaults);
      }
    }
  }
  __finally
  {
    delete FactoryDefaults;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Save(bool All, bool Explicit)
{
  THierarchicalStorage * Storage = Configuration->CreateScpStorage(true);
  try {
    Storage->AccessMode = smReadWrite;
    Storage->Explicit = Explicit;
    if (Storage->OpenSubKey(Configuration->StoredSessionsSubKey, True))
      Save(Storage, All);
  } __finally {
    delete Storage;
  }

  Saved();
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
void __fastcall TStoredSessionList::Export(const AnsiString FileName)
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
      TSessionData *Session = new TSessionData("");
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
      (!SSHOnly || (Sessions[Index]->Protocol == ptSSH)) &&
      !Dest->FindByName(Sessions[Index]->Name);
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Cleanup()
{
  try {
    if (Configuration->Storage == stRegistry) Clear();
    TRegistryStorage * Storage = new TRegistryStorage(Configuration->RegistryStorageKey);
    try {
      Storage->AccessMode = smReadWrite;
      if (Storage->OpenRootKey(False))
        Storage->RecursiveDeleteSubKey(Configuration->StoredSessionsSubKey);
    } __finally {
      delete Storage;
    }
  } catch (Exception &E) {
    throw ExtException(&E, CLEANUP_SESSIONS_ERROR);
  }
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
  AnsiString SessionName, TSessionData * Session)
{
  TSessionData * DuplicateSession = (TSessionData*)FindByName(SessionName);
  if (!DuplicateSession)
  {
    DuplicateSession = new TSessionData("");
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
    FDefaultSettings->Name = DefaultSessionName;
    if (!FReadOnly)
    {
      // only modified, explicit
      Save(false, true);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TStoredSessionList::ImportHostKeys(const AnsiString TargetKey,
  const AnsiString SourceKey, TStoredSessionList * Sessions,
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
      AnsiString HostKeyName;
      assert(Sessions != NULL);
      for (int Index = 0; Index < Sessions->Count; Index++)
      {
        Session = Sessions->Sessions[Index];
        if (!OnlySelected || Session->Selected)
        {
          HostKeyName = PuttyMungeStr(FORMAT("@%d:%s", (Session->PortNumber, Session->HostName)));
          AnsiString KeyName;
          for (int KeyIndex = 0; KeyIndex < KeyList->Count; KeyIndex++)
          {
            KeyName = KeyList->Strings[KeyIndex];
            int P = KeyName.Pos(HostKeyName);
            if ((P > 0) && (P == KeyName.Length() - HostKeyName.Length() + 1))
            {
              TargetStorage->WriteStringRaw(KeyName,
                SourceStorage->ReadStringRaw(KeyName, ""));
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
TSessionData * __fastcall TStoredSessionList::ParseUrl(AnsiString Url,
  TOptions * Options, bool & DefaultsOnly, AnsiString * FileName,
  bool * AProtocolDefined)
{
  TSessionData * Data = new TSessionData("");
  try
  {
    Data->ParseUrl(Url, Options, this, DefaultsOnly, FileName, AProtocolDefined);
  }
  catch(...)
  {
    delete Data;
    throw;
  }

  return Data;
}
