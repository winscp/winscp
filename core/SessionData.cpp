//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SessionData.h"

#include "Common.h"
#include "Configuration.h"
#include "Exceptions.h"
#include "FileBuffer.h"
#include "ScpMain.h"
#include "Security.h"
#include "TextsCore.h"
#include "PuttyIntf.h"
#include "RemoteFiles.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const char * DefaultSessionName = "Default Settings";
const char CipherNames[CIPHER_COUNT][10] = {"WARN", "3des", "blowfish", "aes", "des"};
const char KexNames[KEX_COUNT][20] = {"WARN", "dh-group1-sha1", "dh-group14-sha1", "dh-gex-sha1" };
const char SshProtList[][10] = {"1 only", "1", "2", "2 only"};
const char ProxyMethodList[][10] = {"none", "SOCKS4", "SOCKS5", "HTTP", "Telnet", "Cmd" };
const TCipher DefaultCipherList[CIPHER_COUNT] =
  { cipAES, cipBlowfish, cip3DES, cipWarn, cipDES };
const TKex DefaultKexList[KEX_COUNT] =
  { kexDHGEx, kexDHGroup14, kexDHGroup1, kexWarn };
const char FSProtocolNames[FSPROTOCOL_COUNT][11] = { "SCP", "SFTP (SCP)", "SFTP" };
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
  PortNumber = default_port;
  UserName = "";
  Password = "";
  PingInterval = 30;
  PingType = ptOff;
  Timeout = 15;
  AgentFwd = false;
  AuthTIS = false;
  AuthKI = true;
  AuthKIPassword = true;
  AuthGSSAPI = false;
  ChangeUsername = false;
  Compression = false;
  SshProt = ssh2;
  Ssh2DES = false;
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

  ProxyMethod = pmNone;
  ProxyHost = "proxy";
  ProxyPort = 80;
  ProxyUsername = "";
  ProxyPassword = "";
  ProxyTelnetCommand = "connect %host %port\\n";
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
  ConsiderDST = false;
  DeleteToRecycleBin = false;
  OverwrittenToRecycleBin = false;
  RecycleBinPath = "/tmp";

  // SCP
  ReturnVar = "";
  LookupUserGroups = true;
  EOLType = eolLF;
  Shell = ""; //default shell
  ReturnVar = "";
  ClearAliases = true;
  UnsetNationalVars = true;
  AliasGroupList = false;
  IgnoreLsWarnings = true;
  Scp1Compatibility = false;
  TimeDifference = 0;
  SCPLsFullTime = asAuto;

  // SFTP
  SFTPDownloadQueue = 4;
  SFTPUploadQueue = 4;
  SFTPListingQueue = 2;
  SFTPMaxVersion = 5;

  for (int Index = 0; Index < LENOF(FSFTPBugs); Index++)
  {
    SFTPBug[(TSftpBug)Index] = asAuto;
  }

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
    DUPL(AgentFwd);
    DUPL(AuthTIS);
    DUPL(ChangeUsername);
    DUPL(Compression);
    DUPL(SshProt);
    DUPL(Ssh2DES);
    DUPL(CipherList);
    DUPL(KexList);
    DUPL(PublicKeyFile);
    DUPL(AddressFamily);
    DUPL(RekeyData);
    DUPL(RekeyTime);

    DUPL(FSProtocol);
    DUPL(LocalDirectory);
    DUPL(RemoteDirectory);
    DUPL(UpdateDirectories);
    DUPL(CacheDirectories);
    DUPL(CacheDirectoryChanges);
    DUPL(PreserveDirectoryChanges);

    DUPL(ResolveSymlinks);
    DUPL(ConsiderDST);
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
    DUPL(AliasGroupList);
    DUPL(IgnoreLsWarnings);
    DUPL(SCPLsFullTime);

    DUPL(TimeDifference);
    // new in 53b
    DUPL(TcpNoDelay);
    DUPL(AuthKI);
    DUPL(AuthKIPassword);
    DUPL(AuthGSSAPI);
    DUPL(DeleteToRecycleBin);
    DUPL(OverwrittenToRecycleBin);
    DUPL(RecycleBinPath);

    DUPL(ProxyMethod);
    DUPL(ProxyHost);
    DUPL(ProxyPort);
    DUPL(ProxyUsername);
    DUPL(ProxyPassword);
    DUPL(ProxyTelnetCommand);
    DUPL(ProxyDNS);
    DUPL(ProxyLocalhost);

    for (int Index = 0; Index < LENOF(FBugs); Index++)
    {
      DUPL(Bug[(TSshBug)Index]);
    }

    // SFTP
    DUPL(SFTPDownloadQueue);
    DUPL(SFTPUploadQueue);
    DUPL(SFTPListingQueue);
    DUPL(SFTPMaxVersion);

    for (int Index = 0; Index < LENOF(FSFTPBugs); Index++)
    {
      DUPL(SFTPBug[(TSftpBug)Index]);
    }

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
void __fastcall TSessionData::StoreToConfig(void * config)
{
  Config * cfg = (Config *)config;
  // clear all (parameters not set below)
  memset(cfg, 0, sizeof(*cfg));

  // user-configurable settings
  ASCOPY(cfg->host, HostName);
  ASCOPY(cfg->username, UserName);
  cfg->port = PortNumber;
  cfg->protocol = PROT_SSH;
  // always set 0, as we will handle keepalives ourselves to avoid
  // multi-threaded issues in putty timer list
  cfg->ping_interval = 0;
  cfg->compression = Compression;
  cfg->agentfwd = AgentFwd;
  cfg->addressfamily = AddressFamily;
  ASCOPY(cfg->ssh_rekey_data, RekeyData);
  cfg->ssh_rekey_time = RekeyTime;

  for (int c = 0; c < CIPHER_COUNT; c++)
  {
    int pcipher;
    switch (Cipher[c]) {
      case cipWarn: pcipher = CIPHER_WARN; break;
      case cip3DES: pcipher = CIPHER_3DES; break;
      case cipBlowfish: pcipher = CIPHER_BLOWFISH; break;
      case cipAES: pcipher = CIPHER_AES; break;
      case cipDES: pcipher = CIPHER_DES; break;
      default: assert(false);
    }
    cfg->ssh_cipherlist[c] = pcipher;
  }

  for (int k = 0; k < KEX_COUNT; k++)
  {
    int pkex;
    switch (Kex[k]) {
      case kexWarn: pkex = KEX_WARN; break;
      case kexDHGroup1: pkex = KEX_DHGROUP1; break;
      case kexDHGroup14: pkex = KEX_DHGROUP14; break;
      case kexDHGEx: pkex = KEX_DHGEX; break;
      default: assert(false);
    }
    cfg->ssh_kexlist[k] = pkex;
  }

  AnsiString SPublicKeyFile = PublicKeyFile;
  if (SPublicKeyFile.IsEmpty()) SPublicKeyFile = Configuration->DefaultKeyFile;
  SPublicKeyFile = StripPathQuotes(SPublicKeyFile);
  ASCOPY(cfg->keyfile.path, SPublicKeyFile);
  cfg->sshprot = SshProt;
  cfg->ssh2_des_cbc = Ssh2DES;
  cfg->try_tis_auth = AuthTIS;
  cfg->try_ki_auth = AuthKI;
  cfg->try_gssapi_auth = AuthGSSAPI;
  cfg->change_username = ChangeUsername;

  cfg->proxy_type = ProxyMethod;
  ASCOPY(cfg->proxy_host, ProxyHost);
  cfg->proxy_port = ProxyPort;
  ASCOPY(cfg->proxy_username, ProxyUsername);
  ASCOPY(cfg->proxy_password, ProxyPassword);
  ASCOPY(cfg->proxy_telnet_command, ProxyTelnetCommand);
  cfg->proxy_dns = ProxyDNS;
  cfg->even_proxy_localhost = ProxyLocalhost;

  #pragma option push -w-eas
  // after 0.53b values were reversed, however putty still stores
  // settings to registry in save way as before
  cfg->sshbug_ignore1 = Bug[sbIgnore1];
  cfg->sshbug_plainpw1 = Bug[sbPlainPW1];
  cfg->sshbug_rsa1 = Bug[sbRSA1];
  cfg->sshbug_hmac2 = Bug[sbHMAC2];
  cfg->sshbug_derivekey2 = Bug[sbDeriveKey2];
  cfg->sshbug_rsapad2 = Bug[sbRSAPad2];
  cfg->sshbug_rekey2 = Bug[sbRekey2];
  // new after 0.53b
  cfg->sshbug_pksessid2 = Bug[sbPKSessID2];
  #pragma option pop

  if (FSProtocol == fsSCPonly)
  {
    cfg->ssh_subsys = FALSE;
    if (Shell.IsEmpty())
    {
      // Following forces Putty to open default shell
      // see ssh.c: do_ssh2_authconn() and ssh1_protocol()
      cfg->remote_cmd[0] = '\0';
    }
    else
    {
      ASCOPY(cfg->remote_cmd, Shell);
    }
    cfg->remote_cmd_ptr = &cfg->remote_cmd[0];
    cfg->remote_cmd_ptr2 = NULL; // no second attempt for SCPonly
  }
  else
  {
    cfg->ssh_subsys = TRUE;
    strcpy(cfg->remote_cmd, "sftp");
    cfg->remote_cmd_ptr = &cfg->remote_cmd[0];

    if (FSProtocol != fsSFTPonly)
    {
      cfg->ssh_subsys2 = FALSE;
      if (Shell.IsEmpty())
      {
        cfg->remote_cmd2[0] = '\0';
      }
      else
      {
        ASCOPY(cfg->remote_cmd2, Shell);
      }
      cfg->remote_cmd_ptr2 = &cfg->remote_cmd2[0];
    }
    else
    {
      // see psftp_connect() from psftp.c
      cfg->ssh_subsys2 = FALSE;
      cfg->remote_cmd_ptr2 =
        "test -x /usr/lib/sftp-server && exec /usr/lib/sftp-server\n"
        "test -x /usr/local/lib/sftp-server && exec /usr/local/lib/sftp-server\n"
        "exec sftp-server";
    }
  }

  // permanent settings
  cfg->nopty = TRUE;
  cfg->tcp_keepalives = 0;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Load(THierarchicalStorage * Storage)
{
  if (Storage->OpenSubKey(StorageKey, False))
  {
    PortNumber = Storage->ReadInteger("PortNumber", PortNumber);
    UserName = Storage->ReadString("UserName", UserName);
    // must be loaded after UserName, because HostName may be in format user@host
    HostName = Storage->ReadString("HostName", HostName);

    if (!Configuration->DisablePasswordStoring)
    {
      FPassword = Storage->ReadString("Password", FPassword);
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
    PingType = static_cast<TPingType>
      (Storage->ReadInteger("PingType", PingInterval > 0 ? ptNullPacket : ptOff));
    if (PingInterval == 0)
    {
      PingInterval = 30;
    }
    Timeout = Storage->ReadInteger("Timeout", Timeout);
    AgentFwd = Storage->ReadBool("AgentFwd", AgentFwd);
    AuthTIS = Storage->ReadBool("AuthTIS", AuthTIS);
    AuthKI = Storage->ReadBool("AuthKI", AuthKI);
    AuthKIPassword = Storage->ReadBool("AuthKIPassword", AuthKIPassword);
    AuthGSSAPI = Storage->ReadBool("AuthGSSAPI", AuthGSSAPI);
    ChangeUsername = Storage->ReadBool("ChangeUsername", ChangeUsername);
    Compression = Storage->ReadBool("Compression", Compression);
    SshProt = (TSshProt)Storage->ReadInteger("SshProt", SshProt);
    Ssh2DES = Storage->ReadBool("Ssh2DES", Ssh2DES);
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
    ConsiderDST = Storage->ReadBool("ConsiderDST", ConsiderDST);
    LockInHome = Storage->ReadBool("LockInHome", LockInHome);
    Special = Storage->ReadBool("Special", Special);
    Shell = Storage->ReadString("Shell", Shell);
    ClearAliases = Storage->ReadBool("ClearAliases", ClearAliases);
    UnsetNationalVars = Storage->ReadBool("UnsetNationalVars", UnsetNationalVars);
    AliasGroupList = Storage->ReadBool("AliasGroupList", AliasGroupList);
    IgnoreLsWarnings = Storage->ReadBool("IgnoreLsWarnings", IgnoreLsWarnings);
    SCPLsFullTime = TAutoSwitch(Storage->ReadInteger("SCPLsFullTime", SCPLsFullTime));
    Scp1Compatibility = Storage->ReadBool("Scp1Compatibility", Scp1Compatibility);
    TimeDifference = Storage->ReadFloat("TimeDifference", TimeDifference);
    DeleteToRecycleBin = Storage->ReadBool("DeleteToRecycleBin", DeleteToRecycleBin);
    OverwrittenToRecycleBin = Storage->ReadBool("OverwrittenToRecycleBin", OverwrittenToRecycleBin);
    RecycleBinPath = Storage->ReadString("RecycleBinPath", RecycleBinPath);

    ReturnVar = Storage->ReadString("ReturnVar", ReturnVar);
    LookupUserGroups = Storage->ReadBool("LookupUserGroups", LookupUserGroups);
    EOLType = (TEOLType)Storage->ReadInteger("EOLType", EOLType);

    // new in 53b
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
    FProxyPassword = Storage->ReadString("ProxyPassword", FProxyPassword);
    ProxyTelnetCommand = Storage->ReadStringRaw("ProxyTelnetCommand", ProxyTelnetCommand);
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
    #undef READ_BUG

    if ((Bug[sbHMAC2] == asAuto) &&
        Storage->ReadBool("BuggyMAC", false))
    {
        Bug[sbHMAC2] = asOn;
    }

    #define READ_SFTP_BUG(BUG) \
      SFTPBug[sb##BUG] = TAutoSwitch(Storage->ReadInteger("SFTP" #BUG "Bug", SFTPBug[sb##BUG]));
    READ_SFTP_BUG(Symlink);
    READ_SFTP_BUG(Utf);
    #undef READ_SFTP_BUG

    SFTPMaxVersion = Storage->ReadInteger("SFTPMaxVersion", SFTPMaxVersion);

    // read only (used only on Import from Putty dialog)
    ProtocolStr = Storage->ReadString("Protocol", ProtocolStr);

    CustomParam1 = Storage->ReadString("CustomParam1", CustomParam1);
    CustomParam2 = Storage->ReadString("CustomParam2", CustomParam2);

    Storage->CloseSubKey();
  };
  FModified = false;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Save(THierarchicalStorage * Storage,
  bool PuttyExport, const TSessionData * Default)
{
  if (Modified && Storage->OpenSubKey(StorageKey, true))
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
    WRITE_DATA(String, UserName);
    if (!Configuration->DisablePasswordStoring && !PuttyExport && !Password.IsEmpty())
    {
      WRITE_DATA_EX(String, "Password", FPassword, );
    }
    else
    {
      Storage->DeleteValue("Password");
    }
    WRITE_DATA_EX(Integer, "PingInterval", PingInterval / 60, );
    WRITE_DATA_EX(Integer, "PingIntervalSecs", PingInterval % 60, );
    Storage->DeleteValue("PingIntervalSec"); // obsolete
    // store PingType always as it does not have fixed default
    Storage->WriteInteger("PingType", PingType);
    WRITE_DATA(Integer, Timeout);
    WRITE_DATA(Bool, AgentFwd);
    WRITE_DATA(Bool, AuthTIS);
    WRITE_DATA(Bool, AuthKI);
    WRITE_DATA(Bool, AuthKIPassword);
    WRITE_DATA(Bool, ChangeUsername);
    WRITE_DATA(Bool, Compression);
    WRITE_DATA(Integer, SshProt);
    WRITE_DATA(Bool, Ssh2DES);
    WRITE_DATA_EX(String, "Cipher", CipherList, );
    WRITE_DATA_EX(String, "KEX", KexList, );
    WRITE_DATA(Integer, AddressFamily);
    WRITE_DATA_EX(String, "RekeyBytes", RekeyData, );
    WRITE_DATA(Integer, RekeyTime);

    WRITE_DATA(Bool, TcpNoDelay);

    if (PuttyExport)
    {
      WRITE_DATA(StringRaw, PublicKeyFile);
    }
    else
    {
      WRITE_DATA(Bool, AuthGSSAPI);
      WRITE_DATA(String, PublicKeyFile);
      WRITE_DATA(Integer, FSProtocol);
      WRITE_DATA(String, LocalDirectory);
      WRITE_DATA(String, RemoteDirectory);
      WRITE_DATA(Bool, UpdateDirectories);
      WRITE_DATA(Bool, CacheDirectories);
      WRITE_DATA(Bool, CacheDirectoryChanges);
      WRITE_DATA(Bool, PreserveDirectoryChanges);

      WRITE_DATA(Bool, ResolveSymlinks);
      WRITE_DATA(Bool, ConsiderDST);
      WRITE_DATA(Bool, LockInHome);
      // Special is never stored (if it would, login dialog must be modified not to
      // duplicate Special parameter when Special session is loaded and then stored
      // under different name)
      // WRITE_DATA(Bool, Special);
      WRITE_DATA(String, Shell);
      WRITE_DATA(Bool, ClearAliases);
      WRITE_DATA(Bool, UnsetNationalVars);
      WRITE_DATA(Bool, AliasGroupList);
      WRITE_DATA(Bool, IgnoreLsWarnings);
      WRITE_DATA(Integer, SCPLsFullTime);
      WRITE_DATA(Bool, Scp1Compatibility);
      WRITE_DATA(Float, TimeDifference);
      WRITE_DATA(Bool, DeleteToRecycleBin);
      WRITE_DATA(Bool, OverwrittenToRecycleBin);
      WRITE_DATA(String, RecycleBinPath);

      WRITE_DATA(String, ReturnVar);
      WRITE_DATA(Bool, LookupUserGroups);
      WRITE_DATA(Integer, EOLType);
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
    WRITE_DATA_EX(String, "ProxyPassword", FProxyPassword, );
    WRITE_DATA(StringRaw, ProxyTelnetCommand);
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
    #undef WRITE_BUG
    #undef WRITE_DATA_CONV_FUNC

    Storage->DeleteValue("BuggyMAC");

    if (PuttyExport)
    {
      WRITE_DATA_EX(String, "Protocol", ProtocolStr, );
    }

    if (!PuttyExport)
    {
      #define WRITE_SFTP_BUG(BUG) WRITE_DATA_EX(Integer, "SFTP" #BUG "Bug", SFTPBug[sb##BUG], );
      WRITE_SFTP_BUG(Symlink);
      WRITE_SFTP_BUG(Utf);
      #undef WRITE_SFTP_BUG

      WRITE_DATA(Integer, SFTPMaxVersion);

      WRITE_DATA(String, CustomParam1);
      WRITE_DATA(String, CustomParam2);
    }

    Storage->CloseSubKey();
    FModified = false;
  }
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Remove()
{
  THierarchicalStorage * Storage = Configuration->CreateScpStorage(true);
  try
  {
    if (Storage->OpenSubKey(Configuration->StoredSessionsSubKey, false))
    {
      Storage->RecursiveDeleteSubKey(StorageKey);
    }
  }
  __finally
  {
    delete Storage;
  }
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::ParseUrl(AnsiString Url, int Params,
  AnsiString * ConnectInfo, AnsiString * HostName, int * PortNumber,
  AnsiString * UserName, AnsiString * Password, AnsiString * Path,
  AnsiString * FileName)
{
  #define DECODE(S) (FLAGSET(Params, puDecodeUrlChars) ? DecodeUrlChars(S) : S)

  int PSlash = Url.Pos("/");
  if (PSlash == 0)
  {
    PSlash = Url.Length() + 1;
  }

  AnsiString AConnectInfo;
  AConnectInfo = Url.SubString(1, PSlash - 1);

  int P = AConnectInfo.Pos("@");
  bool Result = (P > 0) || ((Params & puRequireUsername) == 0);
  if (Result)
  {
    if ((Path != NULL) || (FileName != NULL))
    {
      bool ExcludeLeadingSlash = (Params & puExcludeLeadingSlash) != 0;
      int Delta = ExcludeLeadingSlash ? 1 : 0;
      AnsiString APath = Url.SubString(PSlash + Delta,
        Url.Length() - PSlash - Delta + 1);
      if (ExcludeLeadingSlash || (APath != "/"))
      {
        if ((APath.Length() > 0) && (APath[APath.Length()] != '/'))
        {
          if (FileName != NULL)
          {
            *FileName = DECODE(UnixExtractFileName(APath));
          }
          if (FLAGSET(Params, puExtractFileName))
          {
            APath = UnixExtractFilePath(APath);
          }
        }
        if (Path != NULL)
        {
          *Path = DECODE(APath);
        }
      }
    }

    if (ConnectInfo != NULL)
    {
      *ConnectInfo = AConnectInfo;
    }

    AnsiString UserInfo;
    AnsiString HostInfo;

    if (P > 0)
    {
      UserInfo = AConnectInfo.SubString(1, P - 1);
      HostInfo = AConnectInfo.SubString(P + 1, AConnectInfo.Length() - P);
    }
    else
    {
      HostInfo = AConnectInfo;
    }

    if (HostName != NULL)
    {
      *HostName = DECODE(CutToChar(HostInfo, ':', true));
    }
    else
    {
      CutToChar(HostInfo, ':', true);
    }

    if (PortNumber != NULL)
    {
      *PortNumber = HostInfo.IsEmpty() ? -1 : StrToIntDef(DECODE(HostInfo), -1);
    }

    if (UserName != NULL)
    {
      *UserName = DECODE(CutToChar(UserInfo, ':', false));
    }
    else
    {
      CutToChar(UserInfo, ':', false);
    }

    if (Password != NULL)
    {
      *Password = DECODE(UserInfo);
    }
  }
  return Result;
  #undef DECODE
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::ParseUrl(AnsiString Url, int Params,
  AnsiString * FileName)
{
  AnsiString AHostName = HostName;
  int APortNumber = PortNumber;
  AnsiString AUserName = UserName;
  AnsiString APassword = Password;
  AnsiString ARemoteDirectory = RemoteDirectory;

  bool Result = ParseUrl(Url, Params, NULL, &AHostName, &APortNumber,
    &AUserName, &APassword, &ARemoteDirectory, FileName);
  if (Result)
  {
    HostName = AHostName;
    if (APortNumber >= 0)
    {
      PortNumber = APortNumber;
    }
    UserName = AUserName;
    Password = APassword;
    RemoteDirectory = ARemoteDirectory;
  }
  return Result;
}
//---------------------------------------------------------------------
bool __fastcall TSessionData::GetCanLogin()
{
  return !FHostName.IsEmpty() && !FUserName.IsEmpty();
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetSessionKey()
{
  return FORMAT("%s@%s", (UserName, HostName));
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetStorageKey()
{
  return MungeStr(Name);
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
void __fastcall TSessionData::SetClearAliases(bool value)
{
  SET_SESSION_PROPERTY(ClearAliases);
}
//---------------------------------------------------------------------------
void __fastcall TSessionData::SetAliasGroupList(bool value)
{
  SET_SESSION_PROPERTY(AliasGroupList);
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
AnsiString __fastcall TSessionData::GetSshProtStr()
{
  return SshProtList[FSshProt];
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
AnsiString __fastcall TSessionData::GetDefaultLogFileName()
{
  return IncludeTrailingBackslash(SystemTemporaryDirectory()) +
    MakeValidFileName(SessionName) + ".log";
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
  for (int Index = 0; backends[Index].name != NULL; Index++)
  {
    if (!value.AnsiCompareIC(backends[Index].name))
    {
      FProtocol = (TProtocol)backends[Index].protocol;
      break;
    }
  }
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetProtocolStr() const
{
  for (int Index = 0; backends[Index].name != NULL; Index++)
  {
    if ((TProtocol)backends[Index].protocol == Protocol)
    {
      return backends[Index].name;
    }
  }
  return "raw";
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
  return TDateTime((unsigned short)(PingInterval/60/60),
    (unsigned short)(PingInterval/60%60), (unsigned short)(PingInterval%60), 0);
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
void __fastcall TSessionData::SetConsiderDST(bool value)
{
  SET_SESSION_PROPERTY(ConsiderDST);
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
  // proxy password unencrypted to maintain compatibility with Putty
  SET_SESSION_PROPERTY(ProxyPassword);
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetProxyPassword()
{
  return FProxyPassword;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetProxyTelnetCommand(AnsiString value)
{
  SET_SESSION_PROPERTY(ProxyTelnetCommand);
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
AnsiString __fastcall TSessionData::GetInfoTip()
{
  return FmtLoadStr(SESSION_INFO_TIP,
    ARRAYOFCONST((
      (HostName.IsEmpty() ? AnsiString() : HostName),
      (UserName.IsEmpty() ? AnsiString() : UserName),
      (PublicKeyFile.IsEmpty() ? LoadStr(NO_STR) : LoadStr(YES_STR)),
      SshProtStr,
      FSProtocolStr)));
}
//=== TStoredSessionList ----------------------------------------------
__fastcall TStoredSessionList::TStoredSessionList(bool aReadOnly):
  TNamedObjectList(), FReadOnly(aReadOnly)
{
  assert(Configuration);
  LastStorage = Configuration->Storage;
  FDefaultSettings = new TSessionData(DefaultSessionName);
}
//---------------------------------------------------------------------
__fastcall TStoredSessionList::~TStoredSessionList()
{
  assert(Configuration);
  if (!FReadOnly && (Configuration->Storage != LastStorage)) Save();
  delete FDefaultSettings;
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Load(THierarchicalStorage * Storage,
  bool AsModified, bool UseDefaults)
{
  TStringList *SubKeys = new TStringList();
  try {
    Storage->GetSubKeyNames(SubKeys);
    for (int Index = 0; Index < SubKeys->Count; Index++)
    {
      TSessionData *SessionData;
      AnsiString SessionName = UnMungeStr(SubKeys->Strings[Index]);
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
        SessionData->Load(Storage);
        if (AsModified)
        {
          SessionData->Modified = true;
        }
      }
    }
  } __finally {
    delete SubKeys;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Load(AnsiString aKey, bool UseDefaults)
{
  TRegistryStorage * Storage = new TRegistryStorage(aKey);
  try {
    LastStorage = stRegistry;
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
    LastStorage = Configuration->Storage;
    if (Storage->OpenSubKey(Configuration->StoredSessionsSubKey, False))
      Load(Storage);
  } __finally {
    delete Storage;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Save(THierarchicalStorage * Storage)
{
  TSessionData * FactoryDefaults = new TSessionData("");
  try
  {
    for (int Index = 0; Index < Count+HiddenCount; Index++)
    {
      TSessionData *SessionData = (TSessionData *)Items[Index];
      if (SessionData->Modified)
        SessionData->Save(Storage, false, FactoryDefaults);
    }
    if (FDefaultSettings->Modified)
      FDefaultSettings->Save(Storage, false, FactoryDefaults);
  }
  __finally
  {
    delete FactoryDefaults;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Save(AnsiString aKey)
{
  TRegistryStorage * Storage = new TRegistryStorage(aKey);
  try
  {
    LastStorage = stRegistry;
    Storage->AccessMode = smReadWrite;
    if (Storage->OpenRootKey(True)) Save(Storage);
  }
  __finally
  {
    delete Storage;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Save()
{
  if (LastStorage != Configuration->Storage)
  {
    // make sure, that all data will be stored, when saving to new storage
    for (int Index = 0; Index < Count+HiddenCount; Index++)
      ((TSessionData*)Items[Index])->Modified = true;
    FDefaultSettings->Modified = true;
    LastStorage = Configuration->Storage;
  }
  THierarchicalStorage * Storage = Configuration->CreateScpStorage(true);
  try {
    Storage->AccessMode = smReadWrite;
    if (Storage->OpenSubKey(Configuration->StoredSessionsSubKey, True))
      Save(Storage);
  } __finally {
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
  Save();
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
      Save();
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
          HostKeyName = MungeStr(FORMAT("@%d:%s", (Session->PortNumber, Session->HostName)));
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
  bool & DefaultsOnly, int Params, AnsiString * FileName)
{
  bool ProtocolDefined = false;
  TFSProtocol Protocol;
  if (Url.SubString(1, 6).LowerCase() == "scp://")
  {
    Protocol = fsSCPonly;
    Url.Delete(1, 6);
    ProtocolDefined = true;
  }
  else if (Url.SubString(1, 7).LowerCase() == "sftp://")
  {
    Protocol = fsSFTPonly;
    Url.Delete(1, 7);
    ProtocolDefined = true;
  }

  DefaultsOnly = true;
  TSessionData * Data = new TSessionData("");
  try
  {
    if (!Url.IsEmpty())
    {
      TSessionData * AData;
      // lookup stored session session even if protocol was defined
      // (this allows setting for example default username for host
      // by creating stored session named by host)
      AnsiString AUrl(Url);
      if (AUrl[AUrl.Length()] == '/')
      {
        AUrl.SetLength(AUrl.Length() - 1);
      }
      AData = dynamic_cast<TSessionData *>(FindByName(AUrl, false));
    
      if (AData == NULL)
      {
        Data->Assign(DefaultSettings);
        if (Data->ParseUrl(Url, Params, FileName))
        {
          Data->Name = "";
          DefaultsOnly = false;
        }
        else
        {
          throw Exception(FMTLOAD(SESSION_NOT_EXISTS_ERROR, (Url)));
        }
      }
      else
      {
        DefaultsOnly = false;
        Data->Assign(AData);
        if (IsHidden(AData))
        {
          AData->Remove();
          Remove(AData);
          Save();
        }
      }
    }
    else
    {
      Data->Assign(DefaultSettings);
    }

    if (ProtocolDefined)
    {
      Data->FSProtocol = Protocol;
    }
  }
  catch(...)
  {
    delete Data;
    throw;
  }

  return Data;
}

