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
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const char * DefaultSessionName = "Default Settings";
const char CipherNames[CIPHER_COUNT][10] = {"WARN", "3des", "blowfish", "aes", "des"};
const char SshProtList[][10] = {"1 only", "1", "2", "2 only"};
const char ProxyMethodList[][10] = {"none", "SOCKS4", "SOCKS5", "HTTP", "Telnet", "Cmd" };
const TCipher DefaultCipherList[CIPHER_COUNT] =
  { cipAES, cipBlowfish, cip3DES, cipWarn, cipDES };
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
  PingInterval = 0;
  Timeout = 15;
  AgentFwd = false;
  AuthTIS = false;
  AuthKI = true;
  Compression = false;
  SshProt = ssh2;
  Ssh2DES = false;
  for (int Index = 0; Index < CIPHER_COUNT; Index++)
  {
    Cipher[Index] = DefaultCipherList[Index];
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
  //ProxySOCKSVersion = 5;
  ProxyDNS = asAuto;
  ProxyLocalhost = false;

  for (int Index = 0; Index < LENOF(FBugs); Index++)
  {
    Bug[(TSshBug)Index] = asAuto;
  }

  Special = false;
  FSProtocol = fsSCPonly;

  // FS common
  LocalDirectory = "";
  RemoteDirectory = "";
  UpdateDirectories = false;
  CacheDirectories = true;
  LockInHome = false;
  ResolveSymlinks = true;

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

  CustomParam1 = "";
  CustomParam2 = "";

  Selected = false;
  FModified = false;

  // add also to TSessionLog::AddStartupInfo()
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
    DUPL(Timeout);
    DUPL(AgentFwd);
    DUPL(AuthTIS);
    DUPL(Compression);
    DUPL(SshProt);
    DUPL(Ssh2DES);
    DUPL(CipherList);
    DUPL(PublicKeyFile);

    DUPL(FSProtocol);
    DUPL(LocalDirectory);
    DUPL(RemoteDirectory);
    DUPL(UpdateDirectories);
    DUPL(CacheDirectories);
    DUPL(ResolveSymlinks);
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
    // new in 53b
    DUPL(TcpNoDelay);
    DUPL(AuthKI);

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
  cfg->ping_interval = PingInterval;
  cfg->compression = Compression;
  cfg->agentfwd = AgentFwd;

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

  AnsiString SPublicKeyFile = PublicKeyFile;
  if (SPublicKeyFile.IsEmpty()) SPublicKeyFile = Configuration->DefaultKeyFile;
  SPublicKeyFile = StripPathQuotes(SPublicKeyFile);
  ASCOPY(cfg->keyfile.path, SPublicKeyFile);
  cfg->sshprot = SshProt;
  cfg->ssh2_des_cbc = Ssh2DES;
  cfg->try_tis_auth = AuthTIS;
  cfg->try_ki_auth = AuthKI;

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
  cfg->sshbug_dhgex2 = Bug[sbDHGEx2];
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
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Load(THierarchicalStorage * Storage)
{
  if (Storage->OpenSubKey(MungeStr(Name), False))
  {
    HostName = Storage->ReadString("HostName", HostName);
    PortNumber = Storage->ReadInteger("PortNumber", PortNumber);
    UserName = Storage->ReadString("UserName", UserName);
    FPassword = Storage->ReadString("Password", FPassword);
    PingInterval =
      Storage->ReadInteger("PingInterval", PingInterval/60)*60 +
      Storage->ReadInteger("PingIntervalSec", PingInterval%60);
    Timeout = Storage->ReadInteger("Timeout", Timeout);
    AgentFwd = Storage->ReadBool("AgentFwd", AgentFwd);
    AuthTIS = Storage->ReadBool("AuthTIS", AuthTIS);
    AuthKI = Storage->ReadBool("AuthKI", AuthKI);
    Compression = Storage->ReadBool("Compression", Compression);
    SshProt = (TSshProt)Storage->ReadInteger("SshProt", SshProt);
    Ssh2DES = Storage->ReadBool("Ssh2DES", Ssh2DES);
    CipherList = Storage->ReadString("Cipher", CipherList);
    PublicKeyFile = Storage->ReadString("PublicKeyFile", PublicKeyFile);

    FSProtocol = (TFSProtocol)Storage->ReadInteger("FSProtocol", FSProtocol);
    LocalDirectory = Storage->ReadString("LocalDirectory", LocalDirectory);
    RemoteDirectory = Storage->ReadString("RemoteDirectory", RemoteDirectory);
    UpdateDirectories = Storage->ReadBool("UpdateDirectories", UpdateDirectories);
    CacheDirectories = Storage->ReadBool("CacheDirectories", CacheDirectories);
    ResolveSymlinks = Storage->ReadBool("ResolveSymlinks", ResolveSymlinks);
    LockInHome = Storage->ReadBool("LockInHome", LockInHome);
    Special = Storage->ReadBool("Special", Special);
    Shell = Storage->ReadString("Shell", Shell);
    ClearAliases = Storage->ReadBool("ClearAliases", ClearAliases);
    UnsetNationalVars = Storage->ReadBool("UnsetNationalVars", UnsetNationalVars);
    AliasGroupList = Storage->ReadBool("AliasGroupList", AliasGroupList);
    IgnoreLsWarnings = Storage->ReadBool("IgnoreLsWarnings", IgnoreLsWarnings);
    Scp1Compatibility = Storage->ReadBool("Scp1Compatibility", Scp1Compatibility);

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
    ProxyDNS = TAutoSwitch((Storage->ReadInteger("ProxyDNS", ProxyDNS) + 1) % 3);
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
    READ_BUG(DHGEx2);
    READ_BUG(PKSessID2);
    #undef READ_BUG

    if ((Bug[sbHMAC2] == asAuto) &&
        Storage->ReadBool("BuggyMAC", false))
    {
        Bug[sbHMAC2] = asOn;
    }

    // read only (used only on Import from Putty dialog)
    ProtocolStr = Storage->ReadString("Protocol", ProtocolStr);

    CustomParam1 = Storage->ReadString("CustomParam1", CustomParam1);
    CustomParam2 = Storage->ReadString("CustomParam2", CustomParam2);

    Storage->CloseSubKey();
  };
  FModified = false;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::Save(THierarchicalStorage * Storage)
{
  if (Modified && Storage->OpenSubKey(MungeStr(Name), true))
  {
    Storage->WriteString("HostName", HostName);
    Storage->WriteInteger("PortNumber", PortNumber);
    Storage->WriteString("UserName", UserName);
    Storage->WriteString("Password", FPassword);
    Storage->WriteInteger("PingInterval", PingInterval/60);
    Storage->WriteInteger("PingIntervalSec", PingInterval%60);
    Storage->WriteInteger("Timeout", Timeout);
    Storage->WriteBool("AgentFwd", AgentFwd);
    Storage->WriteBool("AuthTIS", AuthTIS);
    Storage->WriteBool("AuthKI", AuthKI);
    Storage->WriteBool("Compression", Compression);
    Storage->WriteInteger("SshProt", SshProt);
    Storage->WriteBool("Ssh2DES", Ssh2DES);
    Storage->WriteString("Cipher", CipherList);
    Storage->WriteString("PublicKeyFile", PublicKeyFile);

    Storage->WriteInteger("FSProtocol", FSProtocol);
    Storage->WriteString("LocalDirectory", LocalDirectory);
    Storage->WriteString("RemoteDirectory", RemoteDirectory);
    Storage->WriteBool("UpdateDirectories", UpdateDirectories);
    Storage->WriteBool("CacheDirectories", CacheDirectories);
    Storage->WriteBool("ResolveSymlinks", ResolveSymlinks);
    Storage->WriteBool("LockInHome", LockInHome);
    // Special is never stored (if it would, login dialog must be modified not to
    // duplicate Special parameter when Special session is loaded and then stored
    // under different name)
    // Storage->WriteBool("Special", Special);
    Storage->WriteString("Shell", Shell);
    Storage->WriteBool("ClearAliases", ClearAliases);
    Storage->WriteBool("UnsetNationalVars", UnsetNationalVars);
    Storage->WriteBool("AliasGroupList", AliasGroupList);
    Storage->WriteBool("IgnoreLsWarnings", IgnoreLsWarnings);
    Storage->WriteBool("Scp1Compatibility", Scp1Compatibility);

    Storage->WriteString("ReturnVar", ReturnVar);
    Storage->WriteBool("LookupUserGroups", LookupUserGroups);
    Storage->WriteInteger("EOLType", EOLType);

    Storage->WriteBool("TcpNoDelay", TcpNoDelay);

    Storage->WriteInteger("ProxyMethod", ProxyMethod);
    Storage->WriteString("ProxyHost", ProxyHost);
    Storage->WriteInteger("ProxyPort", ProxyPort);
    Storage->WriteString("ProxyUsername", ProxyUsername);
    Storage->WriteString("ProxyPassword", FProxyPassword);
    Storage->WriteStringRaw("ProxyTelnetCommand", ProxyTelnetCommand);
    Storage->WriteInteger("ProxyDNS", (ProxyDNS+2) % 3);
    Storage->WriteBool("ProxyLocalhost", ProxyLocalhost);

    #define WRITE_BUG(BUG) Storage->WriteInteger("Bug"#BUG, 2 - Bug[sb##BUG]);
    WRITE_BUG(Ignore1);
    WRITE_BUG(PlainPW1);
    WRITE_BUG(RSA1);
    WRITE_BUG(HMAC2);
    WRITE_BUG(DeriveKey2);
    WRITE_BUG(RSAPad2);
    WRITE_BUG(DHGEx2);
    WRITE_BUG(PKSessID2);
    #undef WRITE_BUG

    Storage->WriteString("CustomParam1", CustomParam1);
    Storage->WriteString("CustomParam2", CustomParam2);

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
      Storage->RecursiveDeleteSubKey(MungeStr(Name));
    }
  }
  __finally
  {
    delete Storage;
  }
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
void __fastcall TSessionData::SetHostName(AnsiString value)
{
  // HostName is key for password encryption
  AnsiString XPassword = Password;
  SET_SESSION_PROPERTY(HostName);
  Password = XPassword;
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
TCipher __fastcall TSessionData::GetCipher(int Index)
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
AnsiString __fastcall TSessionData::GetCipherList()
{
  AnsiString Result;
  for (int Index = 0; Index < CIPHER_COUNT; Index++)
  {
    Result += AnsiString(Index ? "," : "") + CipherNames[Cipher[Index]];
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TSessionData::SetPublicKeyFile(AnsiString value)
{
  SET_SESSION_PROPERTY(PublicKeyFile);
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetDefaultLogFileName()
{
  return GetTemporaryPath() + MakeValidFileName(SessionName) + ".log";
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
AnsiString __fastcall TSessionData::GetProtocolStr()
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
void __fastcall TSessionData::SetPingEnabled(bool value)
{
  if (value && !FPingInterval) FPingInterval = 60;
    else
  if (!value) FPingInterval = 0;
}
//---------------------------------------------------------------------------
bool __fastcall TSessionData::GetPingEnabled()
{
  return (bool)(FPingInterval > 0);
}
//---------------------------------------------------------------------
AnsiString __fastcall TSessionData::GetSessionName()
{
  if (!Name.IsEmpty() && !TNamedObjectList::IsHidden(this)) return Name;
    else
  if (!HostName.IsEmpty() && !UserName.IsEmpty())
      return Format("%s@%s", ARRAYOFCONST((UserName, HostName)));
    else
  if (!HostName.IsEmpty()) return HostName;
    else return "session";
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
void __fastcall TSessionData::SetResolveSymlinks(bool value)
{
  SET_SESSION_PROPERTY(ResolveSymlinks);
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
TAutoSwitch __fastcall TSessionData::GetBug(TSshBug Bug)
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
void __fastcall TStoredSessionList::Load(THierarchicalStorage * Storage)
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

      if (!SessionData)
      {
        SessionData = new TSessionData(SessionName);
        Add(SessionData);
      }
      SessionData->Load(Storage);
    }
  } __finally {
    delete SubKeys;
  }
}
//---------------------------------------------------------------------
void __fastcall TStoredSessionList::Load(AnsiString aKey)
{
  TRegistryStorage * Storage = new TRegistryStorage(aKey);
  try {
    LastStorage = stRegistry;
    if (Storage->OpenRootKey(False)) Load(Storage);
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
  for (int Index = 0; Index < Count+HiddenCount; Index++)
  {
    TSessionData *SessionData = (TSessionData *)Items[Index];
    if (SessionData->Modified)
      SessionData->Save(Storage);
  }
  if (FDefaultSettings->Modified)
    FDefaultSettings->Save(Storage);
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
    DuplicateSession->Modified = True;
    Add(DuplicateSession);
  }
    else
  {
    DuplicateSession->Assign(Session);
    DuplicateSession->Name = SessionName;
    DuplicateSession->Modified = true;
  }
  Save();
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
    Save();
  }
}
