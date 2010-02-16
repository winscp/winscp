//---------------------------------------------------------------------------
#ifndef SessionDataH
#define SessionDataH

#include "Common.h"
#include "Option.h"
#include "FileBuffer.h"
#include "NamedObjs.h"
#include "HierarchicalStorage.h"
#include "Configuration.h"
//---------------------------------------------------------------------------
#define SET_SESSION_PROPERTY(Property) \
  if (F##Property != value) { F##Property = value; FModified = true; }
//---------------------------------------------------------------------------
enum TCipher { cipWarn, cip3DES, cipBlowfish, cipAES, cipDES, cipArcfour };
#define CIPHER_COUNT (cipArcfour+1)
enum TProtocol { ptRaw, ptTelnet, ptRLogin, ptSSH };
#define PROTOCOL_COUNT (ptSSH+1)
// explicit values to skip obsoleted fsExternalSSH, fsExternalSFTP
enum TFSProtocol { fsSCPonly = 0, fsSFTP = 1, fsSFTPonly = 2, fsFTP = 5 };
#define FSPROTOCOL_COUNT (fsFTP+1)
enum TProxyMethod { pmNone, pmSocks4, pmSocks5, pmHTTP, pmTelnet, pmCmd };
enum TSshProt { ssh1only, ssh1, ssh2, ssh2only };
enum TKex { kexWarn, kexDHGroup1, kexDHGroup14, kexDHGEx, kexRSA };
#define KEX_COUNT (kexRSA+1)
enum TSshBug { sbIgnore1, sbPlainPW1, sbRSA1, sbHMAC2, sbDeriveKey2, sbRSAPad2,
  sbRekey2, sbPKSessID2, sbMaxPkt2 };
#define BUG_COUNT (sbMaxPkt2+1)
enum TSftpBug { sbSymlink, sbSignedTS };
#define SFTP_BUG_COUNT (sbSignedTS+1)
enum TPingType { ptOff, ptNullPacket, ptDummyCommand };
enum TAddressFamily { afAuto, afIPv4, afIPv6 };
enum TFtps { ftpsNone, ftpsImplicit, ftpsExplicitSsl, ftpsExplicitTls };
//---------------------------------------------------------------------------
extern const char CipherNames[CIPHER_COUNT][10];
extern const char KexNames[KEX_COUNT][20];
extern const char ProtocolNames[PROTOCOL_COUNT][10];
extern const char SshProtList[][10];
extern const char ProxyMethodList[][10];
extern const TCipher DefaultCipherList[CIPHER_COUNT];
extern const TKex DefaultKexList[KEX_COUNT];
extern const char FSProtocolNames[FSPROTOCOL_COUNT][11];
//---------------------------------------------------------------------------
class TStoredSessionList;
//---------------------------------------------------------------------------
class TSessionData : public TNamedObject
{
friend class TStoredSessionList;

private:
  AnsiString FHostName;
  int FPortNumber;
  AnsiString FUserName;
  AnsiString FPassword;
  bool FPasswordless;
  int FPingInterval;
  TPingType FPingType;
  bool FTryAgent;
  bool FAgentFwd;
  AnsiString FListingCommand;
  bool FAuthTIS;
  bool FAuthKI;
  bool FAuthKIPassword;
  bool FAuthGSSAPI;
  bool FGSSAPIFwdTGT; // not supported anymore
  AnsiString FGSSAPIServerRealm; // not supported anymore
  bool FChangeUsername;
  bool FCompression;
  TSshProt FSshProt;
  bool FSsh2DES;
  bool FSshNoUserAuth;
  TCipher FCiphers[CIPHER_COUNT];
  TKex FKex[KEX_COUNT];
  bool FClearAliases;
  TEOLType FEOLType;
  AnsiString FPublicKeyFile;
  TProtocol FProtocol;
  TFSProtocol FFSProtocol;
  bool FModified;
  AnsiString FLocalDirectory;
  AnsiString FRemoteDirectory;
  bool FLockInHome;
  bool FSpecial;
  bool FUpdateDirectories;
  bool FCacheDirectories;
  bool FCacheDirectoryChanges;
  bool FPreserveDirectoryChanges;
  bool FSelected;
  bool FLookupUserGroups;
  AnsiString FReturnVar;
  bool FScp1Compatibility;
  AnsiString FShell;
  AnsiString FSftpServer;
  int FTimeout;
  bool FUnsetNationalVars;
  bool FIgnoreLsWarnings;
  bool FTcpNoDelay;
  TProxyMethod FProxyMethod;
  AnsiString FProxyHost;
  int FProxyPort;
  AnsiString FProxyUsername;
  AnsiString FProxyPassword;
  AnsiString FProxyTelnetCommand;
  AnsiString FProxyLocalCommand;
  TAutoSwitch FProxyDNS;
  bool FProxyLocalhost;
  int FFtpProxyLogonType;
  TAutoSwitch FBugs[BUG_COUNT];
  AnsiString FCustomParam1;
  AnsiString FCustomParam2;
  bool FResolveSymlinks;
  TDateTime FTimeDifference;
  int FSFTPDownloadQueue;
  int FSFTPUploadQueue;
  int FSFTPListingQueue;
  int FSFTPMaxVersion;
  unsigned long FSFTPMaxPacketSize;
  TDSTMode FDSTMode;
  TAutoSwitch FSFTPBugs[SFTP_BUG_COUNT];
  bool FDeleteToRecycleBin;
  bool FOverwrittenToRecycleBin;
  AnsiString FRecycleBinPath;
  AnsiString FPostLoginCommands;
  TAutoSwitch FSCPLsFullTime;
  TAutoSwitch FFtpListAll;
  TAddressFamily FAddressFamily;
  AnsiString FRekeyData;
  unsigned int FRekeyTime;
  int FColor;
  bool FTunnel;
  AnsiString FTunnelHostName;
  int FTunnelPortNumber;
  AnsiString FTunnelUserName;
  AnsiString FTunnelPassword;
  AnsiString FTunnelPublicKeyFile;
  int FTunnelLocalPortNumber;
  AnsiString FTunnelPortFwd;
  bool FFtpPasvMode;
  bool FFtpForcePasvIp;
  AnsiString FFtpAccount;
  int FFtpPingInterval;
  TPingType FFtpPingType;
  TFtps FFtps;
  TAutoSwitch FNotUtf;
  AnsiString FHostKey;

  AnsiString FOrigHostName;
  int FOrigPortNumber;
  TProxyMethod FOrigProxyMethod;

  void __fastcall SetHostName(AnsiString value);
  void __fastcall SetPortNumber(int value);
  void __fastcall SetUserName(AnsiString value);
  void __fastcall SetPassword(AnsiString value);
  AnsiString __fastcall GetPassword();
  void __fastcall SetPasswordless(bool value);
  void __fastcall SetPingInterval(int value);
  void __fastcall SetTryAgent(bool value);
  void __fastcall SetAgentFwd(bool value);
  void __fastcall SetAuthTIS(bool value);
  void __fastcall SetAuthKI(bool value);
  void __fastcall SetAuthKIPassword(bool value);
  void __fastcall SetAuthGSSAPI(bool value);
  void __fastcall SetGSSAPIFwdTGT(bool value);
  void __fastcall SetGSSAPIServerRealm(AnsiString value);
  void __fastcall SetChangeUsername(bool value);
  void __fastcall SetCompression(bool value);
  void __fastcall SetSshProt(TSshProt value);
  void __fastcall SetSsh2DES(bool value);
  void __fastcall SetSshNoUserAuth(bool value);
  void __fastcall SetCipher(int Index, TCipher value);
  TCipher __fastcall GetCipher(int Index) const;
  void __fastcall SetKex(int Index, TKex value);
  TKex __fastcall GetKex(int Index) const;
  void __fastcall SetPublicKeyFile(AnsiString value);

  void __fastcall SetProtocolStr(AnsiString value);
  AnsiString __fastcall GetProtocolStr() const;
  bool __fastcall GetCanLogin();
  void __fastcall SetPingIntervalDT(TDateTime value);
  TDateTime __fastcall GetPingIntervalDT();
  TDateTime __fastcall GetFtpPingIntervalDT();
  void __fastcall SetTimeDifference(TDateTime value);
  void __fastcall SetPingType(TPingType value);
  AnsiString __fastcall GetSessionName();
  AnsiString __fastcall GetDefaultSessionName();
  AnsiString __fastcall GetSessionUrl();
  void __fastcall SetProtocol(TProtocol value);
  void __fastcall SetFSProtocol(TFSProtocol value);
  AnsiString __fastcall GetFSProtocolStr();
  void __fastcall SetLocalDirectory(AnsiString value);
  void __fastcall SetRemoteDirectory(AnsiString value);
  void __fastcall SetUpdateDirectories(bool value);
  void __fastcall SetCacheDirectories(bool value);
  void __fastcall SetCacheDirectoryChanges(bool value);
  void __fastcall SetPreserveDirectoryChanges(bool value);
  void __fastcall SetLockInHome(bool value);
  void __fastcall SetSpecial(bool value);
  AnsiString __fastcall GetInfoTip();
  bool __fastcall GetDefaultShell();
  void __fastcall SetDetectReturnVar(bool value);
  bool __fastcall GetDetectReturnVar();
  void __fastcall SetListingCommand(AnsiString value);
  void __fastcall SetClearAliases(bool value);
  void __fastcall SetDefaultShell(bool value);
  void __fastcall SetEOLType(TEOLType value);
  void __fastcall SetLookupUserGroups(bool value);
  void __fastcall SetReturnVar(AnsiString value);
  void __fastcall SetScp1Compatibility(bool value);
  void __fastcall SetShell(AnsiString value);
  void __fastcall SetSftpServer(AnsiString value);
  void __fastcall SetTimeout(int value);
  void __fastcall SetUnsetNationalVars(bool value);
  void __fastcall SetIgnoreLsWarnings(bool value);
  void __fastcall SetTcpNoDelay(bool value);
  AnsiString __fastcall GetSshProtStr();
  bool __fastcall GetUsesSsh();
  void __fastcall SetCipherList(AnsiString value);
  AnsiString __fastcall GetCipherList() const;
  void __fastcall SetKexList(AnsiString value);
  AnsiString __fastcall GetKexList() const;
  void __fastcall SetProxyMethod(TProxyMethod value);
  void __fastcall SetProxyHost(AnsiString value);
  void __fastcall SetProxyPort(int value);
  void __fastcall SetProxyUsername(AnsiString value);
  void __fastcall SetProxyPassword(AnsiString value);
  void __fastcall SetProxyTelnetCommand(AnsiString value);
  void __fastcall SetProxyLocalCommand(AnsiString value);
  void __fastcall SetProxyDNS(TAutoSwitch value);
  void __fastcall SetProxyLocalhost(bool value);
  AnsiString __fastcall GetProxyPassword() const;
  void __fastcall SetFtpProxyLogonType(int value);
  void __fastcall SetBug(TSshBug Bug, TAutoSwitch value);
  TAutoSwitch __fastcall GetBug(TSshBug Bug) const;
  AnsiString __fastcall GetSessionKey();
  void __fastcall SetCustomParam1(AnsiString value);
  void __fastcall SetCustomParam2(AnsiString value);
  void __fastcall SetResolveSymlinks(bool value);
  void __fastcall SetSFTPDownloadQueue(int value);
  void __fastcall SetSFTPUploadQueue(int value);
  void __fastcall SetSFTPListingQueue(int value);
  void __fastcall SetSFTPMaxVersion(int value);
  void __fastcall SetSFTPMaxPacketSize(unsigned long value);
  void __fastcall SetSFTPBug(TSftpBug Bug, TAutoSwitch value);
  TAutoSwitch __fastcall GetSFTPBug(TSftpBug Bug) const;
  void __fastcall SetSCPLsFullTime(TAutoSwitch value);
  void __fastcall SetFtpListAll(TAutoSwitch value);
  AnsiString __fastcall GetStorageKey();
  AnsiString __fastcall GetInternalStorageKey();
  void __fastcall SetDSTMode(TDSTMode value);
  void __fastcall SetDeleteToRecycleBin(bool value);
  void __fastcall SetOverwrittenToRecycleBin(bool value);
  void __fastcall SetRecycleBinPath(AnsiString value);
  void __fastcall SetPostLoginCommands(AnsiString value);
  void __fastcall SetAddressFamily(TAddressFamily value);
  void __fastcall SetRekeyData(AnsiString value);
  void __fastcall SetRekeyTime(unsigned int value);
  void __fastcall SetColor(int value);
  void __fastcall SetTunnel(bool value);
  void __fastcall SetTunnelHostName(AnsiString value);
  void __fastcall SetTunnelPortNumber(int value);
  void __fastcall SetTunnelUserName(AnsiString value);
  void __fastcall SetTunnelPassword(AnsiString value);
  AnsiString __fastcall GetTunnelPassword();
  void __fastcall SetTunnelPublicKeyFile(AnsiString value);
  void __fastcall SetTunnelPortFwd(AnsiString value);
  void __fastcall SetTunnelLocalPortNumber(int value);
  bool __fastcall GetTunnelAutoassignLocalPortNumber();
  void __fastcall SetFtpPasvMode(bool value);
  void __fastcall SetFtpForcePasvIp(bool value);
  void __fastcall SetFtpAccount(AnsiString value);
  void __fastcall SetFtpPingInterval(int value);
  void __fastcall SetFtpPingType(TPingType value);
  void __fastcall SetFtps(TFtps value);
  void __fastcall SetNotUtf(TAutoSwitch value);
  void __fastcall SetHostKey(AnsiString value);
  TDateTime __fastcall GetTimeoutDT();
  void __fastcall SavePasswords(THierarchicalStorage * Storage, bool PuttyExport);
  AnsiString __fastcall GetLocalName();
  static AnsiString __fastcall EncryptPassword(const AnsiString & Password, AnsiString Key);
  static AnsiString __fastcall DecryptPassword(const AnsiString & Password, AnsiString Key);
  static AnsiString __fastcall StronglyRecryptPassword(const AnsiString & Password, AnsiString Key);

  __property AnsiString InternalStorageKey = { read = GetInternalStorageKey };

public:
  __fastcall TSessionData(AnsiString aName);
  void __fastcall Default();
  void __fastcall NonPersistant();
  void __fastcall Load(THierarchicalStorage * Storage);
  void __fastcall Save(THierarchicalStorage * Storage, bool PuttyExport,
    const TSessionData * Default = NULL);
  void __fastcall SaveRecryptedPasswords(THierarchicalStorage * Storage);
  void __fastcall RecryptPasswords();
  bool __fastcall HasAnyPassword();
  void __fastcall Remove();
  virtual void __fastcall Assign(TPersistent * Source);
  bool __fastcall ParseUrl(AnsiString Url, TOptions * Options,
    TStoredSessionList * StoredSessions, bool & DefaultsOnly,
    AnsiString * FileName, bool * AProtocolDefined);
  bool __fastcall ParseOptions(TOptions * Options);
  void __fastcall ConfigureTunnel(int PortNumber);
  void __fastcall RollbackTunnel();
  void __fastcall ExpandEnvironmentVariables();
  static void __fastcall ValidatePath(const AnsiString Path);
  static void __fastcall ValidateName(const AnsiString Name);

  __property AnsiString HostName  = { read=FHostName, write=SetHostName };
  __property int PortNumber  = { read=FPortNumber, write=SetPortNumber };
  __property AnsiString UserName  = { read=FUserName, write=SetUserName };
  __property AnsiString Password  = { read=GetPassword, write=SetPassword };
  __property bool Passwordless = { read=FPasswordless, write=SetPasswordless };
  __property int PingInterval  = { read=FPingInterval, write=SetPingInterval };
  __property bool TryAgent  = { read=FTryAgent, write=SetTryAgent };
  __property bool AgentFwd  = { read=FAgentFwd, write=SetAgentFwd };
  __property AnsiString ListingCommand = { read = FListingCommand, write = SetListingCommand };
  __property bool AuthTIS  = { read=FAuthTIS, write=SetAuthTIS };
  __property bool AuthKI  = { read=FAuthKI, write=SetAuthKI };
  __property bool AuthKIPassword  = { read=FAuthKIPassword, write=SetAuthKIPassword };
  __property bool AuthGSSAPI  = { read=FAuthGSSAPI, write=SetAuthGSSAPI };
  __property bool GSSAPIFwdTGT = { read=FGSSAPIFwdTGT, write=SetGSSAPIFwdTGT };
  __property AnsiString GSSAPIServerRealm = { read=FGSSAPIServerRealm, write=SetGSSAPIServerRealm };
  __property bool ChangeUsername  = { read=FChangeUsername, write=SetChangeUsername };
  __property bool Compression  = { read=FCompression, write=SetCompression };
  __property TSshProt SshProt  = { read=FSshProt, write=SetSshProt };
  __property bool UsesSsh = { read = GetUsesSsh };
  __property bool Ssh2DES  = { read=FSsh2DES, write=SetSsh2DES };
  __property bool SshNoUserAuth  = { read=FSshNoUserAuth, write=SetSshNoUserAuth };
  __property TCipher Cipher[int Index] = { read=GetCipher, write=SetCipher };
  __property TKex Kex[int Index] = { read=GetKex, write=SetKex };
  __property AnsiString PublicKeyFile  = { read=FPublicKeyFile, write=SetPublicKeyFile };
  __property TProtocol Protocol  = { read=FProtocol, write=SetProtocol };
  __property AnsiString ProtocolStr  = { read=GetProtocolStr, write=SetProtocolStr };
  __property TFSProtocol FSProtocol  = { read=FFSProtocol, write=SetFSProtocol  };
  __property AnsiString FSProtocolStr  = { read=GetFSProtocolStr };
  __property bool Modified  = { read=FModified, write=FModified };
  __property bool CanLogin  = { read=GetCanLogin };
  __property bool ClearAliases = { read = FClearAliases, write = SetClearAliases };
  __property TDateTime PingIntervalDT = { read = GetPingIntervalDT, write = SetPingIntervalDT };
  __property TDateTime TimeDifference = { read = FTimeDifference, write = SetTimeDifference };
  __property TPingType PingType = { read = FPingType, write = SetPingType };
  __property AnsiString SessionName  = { read=GetSessionName };
  __property AnsiString DefaultSessionName  = { read=GetDefaultSessionName };
  __property AnsiString SessionUrl  = { read=GetSessionUrl };
  __property AnsiString LocalDirectory  = { read=FLocalDirectory, write=SetLocalDirectory };
  __property AnsiString RemoteDirectory  = { read=FRemoteDirectory, write=SetRemoteDirectory };
  __property bool UpdateDirectories = { read=FUpdateDirectories, write=SetUpdateDirectories };
  __property bool CacheDirectories = { read=FCacheDirectories, write=SetCacheDirectories };
  __property bool CacheDirectoryChanges = { read=FCacheDirectoryChanges, write=SetCacheDirectoryChanges };
  __property bool PreserveDirectoryChanges = { read=FPreserveDirectoryChanges, write=SetPreserveDirectoryChanges };
  __property bool LockInHome = { read=FLockInHome, write=SetLockInHome };
  __property bool Special = { read=FSpecial, write=SetSpecial };
  __property bool Selected  = { read=FSelected, write=FSelected };
  __property AnsiString InfoTip  = { read=GetInfoTip };
  __property bool DefaultShell = { read = GetDefaultShell, write = SetDefaultShell };
  __property bool DetectReturnVar = { read = GetDetectReturnVar, write = SetDetectReturnVar };
  __property TEOLType EOLType = { read = FEOLType, write = SetEOLType };
  __property bool LookupUserGroups = { read = FLookupUserGroups, write = SetLookupUserGroups };
  __property AnsiString ReturnVar = { read = FReturnVar, write = SetReturnVar };
  __property bool Scp1Compatibility = { read = FScp1Compatibility, write = SetScp1Compatibility };
  __property AnsiString Shell = { read = FShell, write = SetShell };
  __property AnsiString SftpServer = { read = FSftpServer, write = SetSftpServer };
  __property int Timeout = { read = FTimeout, write = SetTimeout };
  __property TDateTime TimeoutDT = { read = GetTimeoutDT };
  __property bool UnsetNationalVars = { read = FUnsetNationalVars, write = SetUnsetNationalVars };
  __property bool IgnoreLsWarnings  = { read=FIgnoreLsWarnings, write=SetIgnoreLsWarnings };
  __property bool TcpNoDelay  = { read=FTcpNoDelay, write=SetTcpNoDelay };
  __property AnsiString SshProtStr  = { read=GetSshProtStr };
  __property AnsiString CipherList  = { read=GetCipherList, write=SetCipherList };
  __property AnsiString KexList  = { read=GetKexList, write=SetKexList };
  __property TProxyMethod ProxyMethod  = { read=FProxyMethod, write=SetProxyMethod };
  __property AnsiString ProxyHost  = { read=FProxyHost, write=SetProxyHost };
  __property int ProxyPort  = { read=FProxyPort, write=SetProxyPort };
  __property AnsiString ProxyUsername  = { read=FProxyUsername, write=SetProxyUsername };
  __property AnsiString ProxyPassword  = { read=GetProxyPassword, write=SetProxyPassword };
  __property AnsiString ProxyTelnetCommand  = { read=FProxyTelnetCommand, write=SetProxyTelnetCommand };
  __property AnsiString ProxyLocalCommand  = { read=FProxyLocalCommand, write=SetProxyLocalCommand };
  __property TAutoSwitch ProxyDNS  = { read=FProxyDNS, write=SetProxyDNS };
  __property bool ProxyLocalhost  = { read=FProxyLocalhost, write=SetProxyLocalhost };
  __property int FtpProxyLogonType  = { read=FFtpProxyLogonType, write=SetFtpProxyLogonType };
  __property TAutoSwitch Bug[TSshBug Bug]  = { read=GetBug, write=SetBug };
  __property AnsiString CustomParam1 = { read = FCustomParam1, write = SetCustomParam1 };
  __property AnsiString CustomParam2 = { read = FCustomParam2, write = SetCustomParam2 };
  __property AnsiString SessionKey = { read = GetSessionKey };
  __property bool ResolveSymlinks = { read = FResolveSymlinks, write = SetResolveSymlinks };
  __property int SFTPDownloadQueue = { read = FSFTPDownloadQueue, write = SetSFTPDownloadQueue };
  __property int SFTPUploadQueue = { read = FSFTPUploadQueue, write = SetSFTPUploadQueue };
  __property int SFTPListingQueue = { read = FSFTPListingQueue, write = SetSFTPListingQueue };
  __property int SFTPMaxVersion = { read = FSFTPMaxVersion, write = SetSFTPMaxVersion };
  __property unsigned long SFTPMaxPacketSize = { read = FSFTPMaxPacketSize, write = SetSFTPMaxPacketSize };
  __property TAutoSwitch SFTPBug[TSftpBug Bug]  = { read=GetSFTPBug, write=SetSFTPBug };
  __property TAutoSwitch SCPLsFullTime = { read = FSCPLsFullTime, write = SetSCPLsFullTime };
  __property TAutoSwitch FtpListAll = { read = FFtpListAll, write = SetFtpListAll };
  __property TDSTMode DSTMode = { read = FDSTMode, write = SetDSTMode };
  __property bool DeleteToRecycleBin = { read = FDeleteToRecycleBin, write = SetDeleteToRecycleBin };
  __property bool OverwrittenToRecycleBin = { read = FOverwrittenToRecycleBin, write = SetOverwrittenToRecycleBin };
  __property AnsiString RecycleBinPath = { read = FRecycleBinPath, write = SetRecycleBinPath };
  __property AnsiString PostLoginCommands = { read = FPostLoginCommands, write = SetPostLoginCommands };
  __property TAddressFamily AddressFamily = { read = FAddressFamily, write = SetAddressFamily };
  __property AnsiString RekeyData = { read = FRekeyData, write = SetRekeyData };
  __property unsigned int RekeyTime = { read = FRekeyTime, write = SetRekeyTime };
  __property int Color = { read = FColor, write = SetColor };
  __property bool Tunnel = { read = FTunnel, write = SetTunnel };
  __property AnsiString TunnelHostName = { read = FTunnelHostName, write = SetTunnelHostName };
  __property int TunnelPortNumber = { read = FTunnelPortNumber, write = SetTunnelPortNumber };
  __property AnsiString TunnelUserName = { read = FTunnelUserName, write = SetTunnelUserName };
  __property AnsiString TunnelPassword = { read = GetTunnelPassword, write = SetTunnelPassword };
  __property AnsiString TunnelPublicKeyFile = { read = FTunnelPublicKeyFile, write = SetTunnelPublicKeyFile };
  __property bool TunnelAutoassignLocalPortNumber = { read = GetTunnelAutoassignLocalPortNumber };
  __property int TunnelLocalPortNumber = { read = FTunnelLocalPortNumber, write = SetTunnelLocalPortNumber };
  __property AnsiString TunnelPortFwd = { read = FTunnelPortFwd, write = SetTunnelPortFwd };
  __property bool FtpPasvMode = { read = FFtpPasvMode, write = SetFtpPasvMode };
  __property bool FtpForcePasvIp = { read = FFtpForcePasvIp, write = SetFtpForcePasvIp };
  __property AnsiString FtpAccount = { read = FFtpAccount, write = SetFtpAccount };
  __property int FtpPingInterval  = { read=FFtpPingInterval, write=SetFtpPingInterval };
  __property TDateTime FtpPingIntervalDT  = { read=GetFtpPingIntervalDT };
  __property TPingType FtpPingType = { read = FFtpPingType, write = SetFtpPingType };
  __property TFtps Ftps = { read = FFtps, write = SetFtps };
  __property TAutoSwitch NotUtf = { read = FNotUtf, write = SetNotUtf };
  __property AnsiString HostKey = { read = FHostKey, write = SetHostKey };
  __property AnsiString StorageKey = { read = GetStorageKey };
  __property AnsiString OrigHostName = { read = FOrigHostName };
  __property int OrigPortNumber = { read = FOrigPortNumber };
  __property AnsiString LocalName = { read = GetLocalName };
};
//---------------------------------------------------------------------------
class TStoredSessionList : public TNamedObjectList
{
public:
  __fastcall TStoredSessionList(bool aReadOnly = false);
  void __fastcall Load(AnsiString aKey, bool UseDefaults);
  void __fastcall Load();
  void __fastcall Save(bool All, bool Explicit);
  void __fastcall Saved();
  void __fastcall Export(const AnsiString FileName);
  void __fastcall Load(THierarchicalStorage * Storage, bool AsModified = false,
    bool UseDefaults = false);
  void __fastcall Save(THierarchicalStorage * Storage, bool All = false);
  void __fastcall SelectAll(bool Select);
  void __fastcall Import(TStoredSessionList * From, bool OnlySelected);
  void __fastcall RecryptPasswords();
  TSessionData * __fastcall AtSession(int Index)
    { return (TSessionData*)AtObject(Index); }
  void __fastcall SelectSessionsToImport(TStoredSessionList * Dest, bool SSHOnly);
  void __fastcall Cleanup();
  int __fastcall IndexOf(TSessionData * Data);
  TSessionData * __fastcall NewSession(AnsiString SessionName, TSessionData * Session);
  TSessionData * __fastcall ParseUrl(AnsiString Url, TOptions * Options, bool & DefaultsOnly,
    AnsiString * FileName = NULL, bool * ProtocolDefined = NULL);
  virtual __fastcall ~TStoredSessionList();
  __property TSessionData * Sessions[int Index]  = { read=AtSession };
  __property TSessionData * DefaultSettings  = { read=FDefaultSettings, write=SetDefaultSettings };

  static void __fastcall ImportHostKeys(const AnsiString TargetKey,
    const AnsiString SourceKey, TStoredSessionList * Sessions,
    bool OnlySelected);

private:
  TSessionData * FDefaultSettings;
  bool FReadOnly;
  void __fastcall SetDefaultSettings(TSessionData * value);
  void __fastcall DoSave(THierarchicalStorage * Storage, bool All, bool RecryptPasswordOnly);
  void __fastcall DoSave(bool All, bool Explicit, bool RecryptPasswordOnly);
  void __fastcall DoSave(THierarchicalStorage * Storage,
    TSessionData * Data, bool All, bool RecryptPasswordOnly,
    TSessionData * FactoryDefaults);
};
//---------------------------------------------------------------------------
#endif
