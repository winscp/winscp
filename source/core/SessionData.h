//---------------------------------------------------------------------------
#ifndef SessionDataH
#define SessionDataH

#include "Common.h"
#include "Option.h"
#include "FileBuffer.h"
#include "NamedObjs.h"
#include "HierarchicalStorage.h"
#include "Configuration.h"
#include <Xml.XMLIntf.hpp>
//---------------------------------------------------------------------------
enum TCipher { cipWarn, cip3DES, cipBlowfish, cipAES, cipDES, cipArcfour, cipChaCha20, cipAESGCM, cipCount };
#define CIPHER_COUNT (cipCount)
// explicit values to skip obsoleted fsExternalSSH, fsExternalSFTP
enum TFSProtocol { fsSCPonly = 0, fsSFTP = 1, fsSFTPonly = 2, fsFTP = 5, fsWebDAV = 6, fsS3 = 7 };
#define FSPROTOCOL_COUNT (fsS3+1)
extern const wchar_t * ProxyMethodNames;
enum TProxyMethod { pmNone, pmSocks4, pmSocks5, pmHTTP, pmTelnet, pmCmd };
enum TKex { kexWarn, kexDHGroup1, kexDHGroup14, kexDHGroup15, kexDHGroup16, kexDHGroup17, kexDHGroup18, kexDHGEx, kexRSA, kexECDH, kexNTRUHybrid, kexMLKEM25519Hybrid, kexMLKEMNISTHybrid, kexCount };
#define KEX_COUNT (kexCount)
enum THostKey { hkWarn, hkRSA, hkDSA, hkECDSA, hkED25519, hkED448, hkCount };
#define HOSTKEY_COUNT (hkCount)
enum TGssLib { gssGssApi32, gssSspi, gssCustom };
#define GSSLIB_COUNT (gssCustom+1)
// names have to match PuTTY registry entries (see settings.c)
enum TSshBug { sbHMAC2, sbDeriveKey2, sbRSAPad2,
  sbPKSessID2, sbRekey2, sbMaxPkt2, sbIgnore2, sbOldGex2, sbWinAdj, sbChanReq };
#define BUG_COUNT (sbChanReq+1)
enum TSftpBug { sbSymlink, sbSignedTS };
#define SFTP_BUG_COUNT (sbSignedTS+1)
extern const wchar_t * PingTypeNames;
enum TPingType { ptOff, ptNullPacket, ptDummyCommand };
extern const wchar_t * FtpPingTypeNames;
enum TFtpPingType { fptOff, fptDummyCommand0, fptDummyCommand, fptDirectoryListing };
enum TAddressFamily { afAuto, afIPv4, afIPv6 };
enum TFtps { ftpsNone, ftpsImplicit, ftpsExplicitSsl, ftpsExplicitTls };
// ssl2 and ssh3 are equivalent of tls10 now
enum TTlsVersion { ssl2 = 2, ssl3 = 3, tls10 = 10, tls11 = 11, tls12 = 12, tls13 = 13, tlsMin = tls10, tlsDefaultMin = tls12, tlsMax = tls13 };
// has to match libs3 S3UriStyle
enum TS3UrlStyle { s3usVirtualHost, s3usPath };
enum TSessionSource { ssNone, ssStored, ssStoredModified };
const int SFTPMaxVersionAuto = -1;
enum TSessionUrlFlags
{
  sufSpecific = 0x01,
  sufUserName = 0x02,
  sufPassword = 0x04,
  sufHostKey = 0x08,
  sufRawSettings = 0x10,
  sufHttpForWebDAV = 0x20,
  sufSession = sufUserName | sufPassword | sufHostKey,
  sufComplete = sufSession | sufRawSettings,
  sufOpen = sufUserName | sufPassword
};
enum TParseUrlFlags
{
  pufAllowStoredSiteWithProtocol = 0x01,
  pufUnsafe = 0x02,
  pufPreferProtocol = 0x04,
  pufParseOnly = 0x08,
};
//---------------------------------------------------------------------------
extern const UnicodeString CipherNames[CIPHER_COUNT];
extern const UnicodeString KexNames[KEX_COUNT];
extern const UnicodeString HostKeyNames[HOSTKEY_COUNT];
extern const UnicodeString GssLibNames[GSSLIB_COUNT];
extern const wchar_t SshProtList[][10];
extern const TCipher DefaultCipherList[CIPHER_COUNT];
extern const TKex DefaultKexList[KEX_COUNT];
extern const THostKey DefaultHostKeyList[HOSTKEY_COUNT];
extern const TGssLib DefaultGssLibList[GSSLIB_COUNT];
extern const wchar_t FSProtocolNames[FSPROTOCOL_COUNT][16];
extern const int DefaultSendBuf;
extern const UnicodeString AnonymousUserName;
extern const UnicodeString AnonymousPassword;
extern const int SshPortNumber;
extern const int FtpPortNumber;
extern const int FtpsImplicitPortNumber;
extern const int HTTPPortNumber;
extern const int HTTPSPortNumber;
extern const int TelnetPortNumber;
extern const int ProxyPortNumber;
extern const UnicodeString PuttySshProtocol;
extern const UnicodeString PuttyTelnetProtocol;
extern const UnicodeString SftpProtocol;
extern const UnicodeString ScpProtocol;
extern const UnicodeString FtpProtocol;
extern const UnicodeString FtpsProtocol;
extern const UnicodeString FtpesProtocol;
extern const UnicodeString WebDAVProtocol;
extern const UnicodeString WebDAVSProtocol;
extern const UnicodeString S3Protocol;
extern const UnicodeString SshProtocol;
extern const UnicodeString WinSCPProtocolPrefix;
extern const wchar_t UrlParamSeparator;
extern const wchar_t UrlParamValueSeparator;
extern const UnicodeString UrlHostKeyParamName;
extern const UnicodeString UrlSaveParamName;
extern const UnicodeString PassphraseOption;
extern const UnicodeString S3HostName;
extern const UnicodeString S3GoogleCloudHostName;
//---------------------------------------------------------------------------
class TStoredSessionList;
//---------------------------------------------------------------------------
class TSessionData : public TNamedObject
{
friend class TStoredSessionList;

private:
  UnicodeString FHostName;
  int FPortNumber;
  UnicodeString FUserName;
  RawByteString FPassword;
  RawByteString FNewPassword;
  bool FChangePassword;
  int FPingInterval;
  TPingType FPingType;
  bool FTryAgent;
  bool FAgentFwd;
  UnicodeString FListingCommand;
  bool FAuthKI;
  bool FAuthKIPassword;
  bool FAuthGSSAPI;
  bool FAuthGSSAPIKEX;
  bool FGSSAPIFwdTGT;
  bool FChangeUsername;
  bool FCompression;
  bool FSsh2DES;
  bool FSshNoUserAuth;
  TCipher FCiphers[CIPHER_COUNT];
  TKex FKex[KEX_COUNT];
  THostKey FHostKeys[HOSTKEY_COUNT];
  TGssLib FGssLib[GSSLIB_COUNT];
  UnicodeString FGssLibCustom;
  bool FClearAliases;
  TEOLType FEOLType;
  bool FTrimVMSVersions;
  bool FVMSAllRevisions;
  UnicodeString FPublicKeyFile;
  UnicodeString FPassphrase;
  UnicodeString FDetachedCertificate;
  UnicodeString FPuttyProtocol;
  TFSProtocol FFSProtocol;
  bool FModified;
  UnicodeString FLocalDirectory;
  UnicodeString FRemoteDirectory;
  UnicodeString FOtherLocalDirectory;
  bool FSpecial;
  bool FSynchronizeBrowsing;
  bool FUpdateDirectories;
  bool FRequireDirectories;
  bool FCacheDirectories;
  bool FCacheDirectoryChanges;
  bool FPreserveDirectoryChanges;
  bool FSelected;
  TAutoSwitch FLookupUserGroups;
  UnicodeString FReturnVar;
  bool FExitCode1IsError;
  bool FScp1Compatibility;
  UnicodeString FShell;
  UnicodeString FSftpServer;
  int FTimeout;
  bool FUnsetNationalVars;
  bool FIgnoreLsWarnings;
  bool FTcpNoDelay;
  int FSendBuf;
  UnicodeString FSourceAddress;
  UnicodeString FProtocolFeatures;
  bool FSshSimple;
  TProxyMethod FProxyMethod;
  UnicodeString FProxyHost;
  int FProxyPort;
  UnicodeString FProxyUsername;
  RawByteString FProxyPassword;
  UnicodeString FProxyTelnetCommand;
  UnicodeString FProxyLocalCommand;
  TAutoSwitch FProxyDNS;
  bool FProxyLocalhost;
  int FFtpProxyLogonType;
  TAutoSwitch FBugs[BUG_COUNT];
  UnicodeString FPuttySettings;
  UnicodeString FCustomParam1;
  UnicodeString FCustomParam2;
  bool FResolveSymlinks;
  bool FFollowDirectorySymlinks;
  TDateTime FTimeDifference;
  bool FTimeDifferenceAuto;
  int FSFTPDownloadQueue;
  int FSFTPUploadQueue;
  int FSFTPListingQueue;
  int FSFTPMaxVersion;
  unsigned long FSFTPMaxPacketSize;
  TAutoSwitch FSFTPRealPath;
  bool FUsePosixRename;
  TDSTMode FDSTMode;
  TAutoSwitch FSFTPBugs[SFTP_BUG_COUNT];
  bool FDeleteToRecycleBin;
  bool FOverwrittenToRecycleBin;
  UnicodeString FRecycleBinPath;
  UnicodeString FPostLoginCommands;
  TAutoSwitch FSCPLsFullTime;
  TAutoSwitch FFtpListAll;
  TAutoSwitch FFtpHost;
  TAutoSwitch FFtpWorkFromCwd;
  bool FFtpAnyCodeForPwd;
  bool FSslSessionReuse;
  UnicodeString FTlsCertificateFile;
  TAddressFamily FAddressFamily;
  UnicodeString FRekeyData;
  unsigned int FRekeyTime;
  int FColor;
  bool FTunnel;
  UnicodeString FTunnelHostName;
  int FTunnelPortNumber;
  UnicodeString FTunnelUserName;
  RawByteString FTunnelPassword;
  UnicodeString FTunnelPublicKeyFile;
  RawByteString FTunnelPassphrase;
  int FTunnelLocalPortNumber;
  UnicodeString FTunnelPortFwd;
  UnicodeString FTunnelHostKey;
  bool FFtpPasvMode;
  TAutoSwitch FFtpForcePasvIp;
  TAutoSwitch FFtpUseMlsd;
  UnicodeString FFtpAccount;
  int FFtpPingInterval;
  TFtpPingType FFtpPingType;
  TAutoSwitch FFtpTransferActiveImmediately;
  TFtps FFtps;
  TTlsVersion FMinTlsVersion;
  TTlsVersion FMaxTlsVersion;
  TAutoSwitch FCompleteTlsShutdown;
  TAutoSwitch FNotUtf;
  int FInternalEditorEncoding;
  UnicodeString FS3DefaultRegion;
  UnicodeString FS3SessionToken;
  UnicodeString FS3RoleArn;
  UnicodeString FS3RoleSessionName;
  UnicodeString FS3Profile;
  TS3UrlStyle FS3UrlStyle;
  TAutoSwitch FS3MaxKeys;
  bool FS3CredentialsEnv;
  bool FS3RequesterPays;
  bool FIsWorkspace;
  UnicodeString FLink;
  UnicodeString FNameOverride;
  UnicodeString FHostKey;
  bool FFingerprintScan;
  bool FOverrideCachedHostKey;
  UnicodeString FNote;
  UnicodeString FWinTitle;
  RawByteString FEncryptKey;
  bool FWebDavLiberalEscaping;
  bool FWebDavAuthLegacy;

  UnicodeString FOrigHostName;
  int FOrigPortNumber;
  TProxyMethod FOrigProxyMethod;
  TSessionSource FSource;
  bool FSaveOnly;
  UnicodeString FLogicalHostName;

  void __fastcall SetHostName(UnicodeString value);
  UnicodeString __fastcall GetHostNameExpanded();
  UnicodeString GetHostNameSource();
  void __fastcall SetPortNumber(int value);
  void __fastcall SetUserName(UnicodeString value);
  UnicodeString __fastcall GetUserNameExpanded();
  UnicodeString GetUserNameSource();
  void __fastcall SetPassword(UnicodeString value);
  UnicodeString __fastcall GetPassword() const;
  void __fastcall SetNewPassword(UnicodeString value);
  UnicodeString __fastcall GetNewPassword() const;
  void __fastcall SetChangePassword(bool value);
  void __fastcall SetPingInterval(int value);
  void __fastcall SetTryAgent(bool value);
  void __fastcall SetAgentFwd(bool value);
  void __fastcall SetAuthKI(bool value);
  void __fastcall SetAuthKIPassword(bool value);
  void __fastcall SetAuthGSSAPI(bool value);
  void __fastcall SetAuthGSSAPIKEX(bool value);
  void __fastcall SetGSSAPIFwdTGT(bool value);
  void __fastcall SetChangeUsername(bool value);
  void __fastcall SetCompression(bool value);
  void __fastcall SetSsh2DES(bool value);
  void __fastcall SetSshNoUserAuth(bool value);
  void __fastcall SetCipher(int Index, TCipher value);
  TCipher __fastcall GetCipher(int Index) const;
  void __fastcall SetKex(int Index, TKex value);
  TKex __fastcall GetKex(int Index) const;
  void __fastcall SetHostKeys(int Index, THostKey value);
  THostKey __fastcall GetHostKeys(int Index) const;
  void __fastcall SetGssLib(int Index, TGssLib value);
  TGssLib __fastcall GetGssLib(int Index) const;
  void __fastcall SetGssLibCustom(UnicodeString value);
  void __fastcall SetPublicKeyFile(UnicodeString value);
  UnicodeString __fastcall GetPassphrase() const;
  void __fastcall SetPassphrase(UnicodeString value);
  void __fastcall SetDetachedCertificate(UnicodeString value);

  void __fastcall SetPuttyProtocol(UnicodeString value);
  bool __fastcall GetCanLogin();
  bool __fastcall GetCanOpen();
  bool __fastcall GetIsLocalBrowser();
  void __fastcall SetPingIntervalDT(TDateTime value);
  TDateTime __fastcall GetPingIntervalDT();
  TDateTime __fastcall GetFtpPingIntervalDT();
  void __fastcall SetTimeDifference(TDateTime value);
  void __fastcall SetTimeDifferenceAuto(bool value);
  void __fastcall SetPingType(TPingType value);
  UnicodeString __fastcall GetSessionName();
  UnicodeString __fastcall GetDefaultSessionName();
  UnicodeString __fastcall GetProtocolUrl(bool HttpForWebDAV);
  void __fastcall SetFSProtocol(TFSProtocol value);
  UnicodeString __fastcall GetFSProtocolStr();
  void __fastcall SetLocalDirectory(UnicodeString value);
  void __fastcall SetOtherLocalDirectory(const UnicodeString & value);
  UnicodeString __fastcall GetLocalDirectoryExpanded();
  void __fastcall SetRemoteDirectory(UnicodeString value);
  void __fastcall SetSynchronizeBrowsing(bool value);
  void __fastcall SetUpdateDirectories(bool value);
  void __fastcall SetCacheDirectories(bool value);
  void __fastcall SetCacheDirectoryChanges(bool value);
  void __fastcall SetPreserveDirectoryChanges(bool value);
  void __fastcall SetSpecial(bool value);
  UnicodeString __fastcall GetInfoTip();
  bool __fastcall GetDefaultShell();
  void __fastcall SetDetectReturnVar(bool value);
  bool __fastcall GetDetectReturnVar();
  void __fastcall SetListingCommand(UnicodeString value);
  void __fastcall SetClearAliases(bool value);
  void __fastcall SetDefaultShell(bool value);
  void __fastcall SetEOLType(TEOLType value);
  void __fastcall SetTrimVMSVersions(bool value);
  void __fastcall SetVMSAllRevisions(bool value);
  void __fastcall SetLookupUserGroups(TAutoSwitch value);
  void __fastcall SetReturnVar(UnicodeString value);
  void __fastcall SetExitCode1IsError(bool value);
  void __fastcall SetScp1Compatibility(bool value);
  void __fastcall SetShell(UnicodeString value);
  void __fastcall SetSftpServer(UnicodeString value);
  void __fastcall SetTimeout(int value);
  void __fastcall SetUnsetNationalVars(bool value);
  void __fastcall SetIgnoreLsWarnings(bool value);
  void __fastcall SetTcpNoDelay(bool value);
  void __fastcall SetSendBuf(int value);
  void __fastcall SetSourceAddress(const UnicodeString & value);
  void __fastcall SetProtocolFeatures(const UnicodeString & value);
  void __fastcall SetSshSimple(bool value);
  bool __fastcall GetUsesSsh();
  void __fastcall SetCipherList(UnicodeString value);
  UnicodeString __fastcall GetCipherList() const;
  void __fastcall SetKexList(UnicodeString value);
  UnicodeString __fastcall GetKexList() const;
  void __fastcall SetHostKeyList(UnicodeString value);
  UnicodeString __fastcall GetHostKeyList() const;
  void __fastcall SetGssLibList(UnicodeString value);
  UnicodeString __fastcall GetGssLibList() const;
  void __fastcall SetProxyMethod(TProxyMethod value);
  void __fastcall SetProxyHost(UnicodeString value);
  void __fastcall SetProxyPort(int value);
  void __fastcall SetProxyUsername(UnicodeString value);
  void __fastcall SetProxyPassword(UnicodeString value);
  void __fastcall SetProxyTelnetCommand(UnicodeString value);
  void __fastcall SetProxyLocalCommand(UnicodeString value);
  void __fastcall SetProxyDNS(TAutoSwitch value);
  void __fastcall SetProxyLocalhost(bool value);
  UnicodeString __fastcall GetProxyPassword() const;
  void __fastcall SetFtpProxyLogonType(int value);
  void __fastcall SetBug(TSshBug Bug, TAutoSwitch value);
  TAutoSwitch __fastcall GetBug(TSshBug Bug) const;
  UnicodeString __fastcall GetSessionKey();
  void __fastcall SetPuttySettings(UnicodeString value);
  void __fastcall SetCustomParam1(UnicodeString value);
  void __fastcall SetCustomParam2(UnicodeString value);
  void __fastcall SetResolveSymlinks(bool value);
  void __fastcall SetFollowDirectorySymlinks(bool value);
  void __fastcall SetSFTPDownloadQueue(int value);
  void __fastcall SetSFTPUploadQueue(int value);
  void __fastcall SetSFTPListingQueue(int value);
  void __fastcall SetSFTPMaxVersion(int value);
  void __fastcall SetSFTPMaxPacketSize(unsigned long value);
  void __fastcall SetSFTPRealPath(TAutoSwitch value);
  void SetUsePosixRename(bool value);
  void __fastcall SetSFTPBug(TSftpBug Bug, TAutoSwitch value);
  TAutoSwitch __fastcall GetSFTPBug(TSftpBug Bug) const;
  void __fastcall SetSCPLsFullTime(TAutoSwitch value);
  void __fastcall SetFtpListAll(TAutoSwitch value);
  void __fastcall SetFtpHost(TAutoSwitch value);
  void __fastcall SetFtpWorkFromCwd(TAutoSwitch value);
  void SetFtpAnyCodeForPwd(bool value);
  void __fastcall SetSslSessionReuse(bool value);
  void __fastcall SetTlsCertificateFile(UnicodeString value);
  UnicodeString __fastcall GetStorageKey();
  UnicodeString __fastcall GetInternalStorageKey();
  UnicodeString __fastcall GetSiteKey();
  void __fastcall SetDSTMode(TDSTMode value);
  void __fastcall SetDeleteToRecycleBin(bool value);
  void __fastcall SetOverwrittenToRecycleBin(bool value);
  void __fastcall SetRecycleBinPath(UnicodeString value);
  void __fastcall SetPostLoginCommands(UnicodeString value);
  void __fastcall SetAddressFamily(TAddressFamily value);
  void __fastcall SetRekeyData(UnicodeString value);
  void __fastcall SetRekeyTime(unsigned int value);
  void __fastcall SetColor(int value);
  void __fastcall SetTunnel(bool value);
  void __fastcall SetTunnelHostName(UnicodeString value);
  void __fastcall SetTunnelPortNumber(int value);
  void __fastcall SetTunnelUserName(UnicodeString value);
  void __fastcall SetTunnelPassword(UnicodeString value);
  UnicodeString __fastcall GetTunnelPassword() const;
  void __fastcall SetTunnelPublicKeyFile(UnicodeString value);
  void __fastcall SetTunnelPassphrase(UnicodeString value);
  UnicodeString __fastcall GetTunnelPassphrase() const;
  void __fastcall SetTunnelPortFwd(UnicodeString value);
  void __fastcall SetTunnelLocalPortNumber(int value);
  bool __fastcall GetTunnelAutoassignLocalPortNumber();
  void __fastcall SetTunnelHostKey(UnicodeString value);
  void __fastcall SetFtpPasvMode(bool value);
  void __fastcall SetFtpForcePasvIp(TAutoSwitch value);
  void __fastcall SetFtpUseMlsd(TAutoSwitch value);
  void __fastcall SetFtpAccount(UnicodeString value);
  void __fastcall SetFtpPingInterval(int value);
  void __fastcall SetFtpPingType(TFtpPingType value);
  void __fastcall SetFtpTransferActiveImmediately(TAutoSwitch value);
  void __fastcall SetFtps(TFtps value);
  void __fastcall SetMinTlsVersion(TTlsVersion value);
  void __fastcall SetMaxTlsVersion(TTlsVersion value);
  void SetCompleteTlsShutdown(TAutoSwitch value);
  void __fastcall SetNotUtf(TAutoSwitch value);
  void __fastcall SetInternalEditorEncoding(int value);
  void __fastcall SetS3DefaultRegion(UnicodeString value);
  void __fastcall SetS3SessionToken(UnicodeString value);
  void __fastcall SetS3RoleArn(UnicodeString value);
  void __fastcall SetS3RoleSessionName(UnicodeString value);
  void __fastcall SetS3Profile(UnicodeString value);
  void __fastcall SetS3UrlStyle(TS3UrlStyle value);
  void __fastcall SetS3MaxKeys(TAutoSwitch value);
  void __fastcall SetS3CredentialsEnv(bool value);
  void __fastcall SetS3RequesterPays(bool value);
  void __fastcall SetLogicalHostName(UnicodeString value);
  void __fastcall SetIsWorkspace(bool value);
  void __fastcall SetLink(UnicodeString value);
  void __fastcall SetNameOverride(UnicodeString value);
  void __fastcall SetHostKey(UnicodeString value);
  void __fastcall SetNote(UnicodeString value);
  void __fastcall SetWinTitle(UnicodeString value);
  UnicodeString __fastcall GetEncryptKey() const;
  void __fastcall SetEncryptKey(UnicodeString value);
  void __fastcall SetWebDavLiberalEscaping(bool value);
  void __fastcall SetWebDavAuthLegacy(bool value);

  TDateTime __fastcall GetTimeoutDT();
  void __fastcall SavePasswords(THierarchicalStorage * Storage, bool PuttyExport, bool DoNotEncryptPasswords, bool SaveAll);
  UnicodeString __fastcall GetLocalName();
  UnicodeString __fastcall GetFolderName();
  void __fastcall Modify();
  UnicodeString __fastcall GetSourceName();
  void __fastcall DoLoad(THierarchicalStorage * Storage, bool PuttyImport, bool & RewritePassword, bool Unsafe, bool RespectDisablePasswordStoring);
  void __fastcall DoSave(THierarchicalStorage * Storage,
    bool PuttyExport, const TSessionData * Default, bool DoNotEncryptPasswords);
  UnicodeString __fastcall ReadXmlNode(_di_IXMLNode Node, const UnicodeString & Name, const UnicodeString & Default);
  int __fastcall ReadXmlNode(_di_IXMLNode Node, const UnicodeString & Name, int Default);
  _di_IXMLNode __fastcall FindSettingsNode(_di_IXMLNode Node, const UnicodeString & Name);
  UnicodeString __fastcall ReadSettingsNode(_di_IXMLNode Node, const UnicodeString & Name, const UnicodeString & Default);
  int __fastcall ReadSettingsNode(_di_IXMLNode Node, const UnicodeString & Name, int Default);
  bool __fastcall IsSame(const TSessionData * Default, bool AdvancedOnly, TStrings * DifferentProperties, bool Decrypted);
  UnicodeString __fastcall GetNameWithoutHiddenPrefix();
  bool __fastcall HasStateData();
  void __fastcall CopyStateData(TSessionData * SourceData);
  void __fastcall CopyNonCoreData(TSessionData * SourceData);
  UnicodeString __fastcall GetNormalizedPuttyProtocol() const;
  void ReadPasswordsFromFiles();
  static RawByteString __fastcall EncryptPassword(const UnicodeString & Password, UnicodeString Key);
  static UnicodeString __fastcall DecryptPassword(const RawByteString & Password, UnicodeString Key);
  static RawByteString __fastcall StronglyRecryptPassword(const RawByteString & Password, UnicodeString Key);
  static bool __fastcall DoIsProtocolUrl(const UnicodeString & Url, const UnicodeString & Protocol, int & ProtocolLen);
  static bool __fastcall IsProtocolUrl(const UnicodeString & Url, const UnicodeString & Protocol, int & ProtocolLen);
  static void __fastcall AddSwitch(UnicodeString & Result, const UnicodeString & Name, bool Rtf);
  static void __fastcall AddSwitch(
    UnicodeString & Result, const UnicodeString & Name, const UnicodeString & Value, bool Rtf);
  static void __fastcall AddSwitch(UnicodeString & Result, const UnicodeString & Name, int Value, bool Rtf);
  static void __fastcall AddAssemblyProperty(
    UnicodeString & Result, TAssemblyLanguage Language,
    const UnicodeString & Name, const UnicodeString & Value);
  static void __fastcall AddAssemblyProperty(
    UnicodeString & Result, TAssemblyLanguage Language,
    const UnicodeString & Name, const UnicodeString & Type,
    const UnicodeString & Member);
  static void __fastcall AddAssemblyProperty(
    UnicodeString & Result, TAssemblyLanguage Language,
    const UnicodeString & Name, int Value);
  void __fastcall AddAssemblyProperty(
    UnicodeString & Result, TAssemblyLanguage Language,
    const UnicodeString & Name, bool Value);
  TStrings * __fastcall GetRawSettingsForUrl();
  void __fastcall DoCopyData(TSessionData * SourceData, bool NoRecrypt);
  bool HasS3AutoCredentials();
  template<class AlgoT>
  void __fastcall SetAlgoList(AlgoT * List, const AlgoT * DefaultList, const UnicodeString * Names,
    int Count, AlgoT WarnAlgo, UnicodeString value);
  static void __fastcall Remove(THierarchicalStorage * Storage, const UnicodeString & Name);

  __property UnicodeString InternalStorageKey = { read = GetInternalStorageKey };

public:
  __fastcall TSessionData(UnicodeString aName);
  virtual __fastcall ~TSessionData();
  TSessionData * __fastcall Clone();
  void __fastcall Default();
  void __fastcall DefaultSettings();
  void __fastcall NonPersistent();
  void __fastcall Load(THierarchicalStorage * Storage, bool PuttyImport);
  void __fastcall ApplyRawSettings(TStrings * RawSettings, bool Unsafe);
  void __fastcall ApplyRawSettings(THierarchicalStorage * Storage, bool Unsafe, bool RespectDisablePasswordStoring);
  void __fastcall ImportFromFilezilla(_di_IXMLNode Node, const UnicodeString & Path, _di_IXMLNode SettingsNode);
  void ImportFromOpenssh(TStrings * Lines);
  void __fastcall Save(THierarchicalStorage * Storage, bool PuttyExport,
    const TSessionData * Default = NULL);
  void __fastcall SaveRecryptedPasswords(THierarchicalStorage * Storage);
  void __fastcall RecryptPasswords();
  bool __fastcall HasPassword();
  bool __fastcall HasAnySessionPassword();
  bool __fastcall HasAnyPassword();
  void __fastcall ClearSessionPasswords();
  void __fastcall MaskPasswords();
  void __fastcall Remove();
  void __fastcall CacheHostKeyIfNotCached();
  virtual void __fastcall Assign(TPersistent * Source);
  virtual int __fastcall Compare(TNamedObject * Other);
  void __fastcall CopyData(TSessionData * Source);
  void __fastcall CopyDataNoRecrypt(TSessionData * SourceData);
  void __fastcall CopyDirectoriesStateData(TSessionData * SourceData);
  bool __fastcall ParseUrl(UnicodeString Url, TOptions * Options,
    TStoredSessionList * StoredSessions, bool & DefaultsOnly,
    UnicodeString * FileName, bool * AProtocolDefined, UnicodeString * MaskedUrl, int Flags);
  TStrings * __fastcall SaveToOptions(const TSessionData * Default, bool SaveName, bool PuttyExport);
  void __fastcall ConfigureTunnel(int PortNumber);
  void __fastcall RollbackTunnel();
  TSessionData * CreateTunnelData(int TunnelLocalPortNumber);
  void __fastcall ExpandEnvironmentVariables();
  void __fastcall DisableAuthentationsExceptPassword();
  bool __fastcall IsSame(const TSessionData * Default, bool AdvancedOnly);
  bool __fastcall IsSameDecrypted(const TSessionData * Default);
  bool __fastcall IsSameSite(const TSessionData * Default);
  bool __fastcall IsInFolderOrWorkspace(const UnicodeString & Name);
  UnicodeString __fastcall GenerateSessionUrl(unsigned int Flags);
  bool __fastcall HasRawSettingsForUrl();
  bool __fastcall HasSessionName();
  bool HasAutoCredentials();
  int GetDefaultPort();
  UnicodeString ResolvePublicKeyFile();
  UnicodeString GetSessionPasswordEncryptionKey() const;

  UnicodeString __fastcall GenerateOpenCommandArgs(bool Rtf);
  void __fastcall GenerateAssemblyCode(TAssemblyLanguage Language, UnicodeString & Head, UnicodeString & Tail, int & Indent);
  void __fastcall LookupLastFingerprint();
  bool __fastcall IsSecure();
  static void __fastcall ValidatePath(const UnicodeString Path);
  static void __fastcall ValidateName(const UnicodeString Name);
  static UnicodeString __fastcall MakeValidName(const UnicodeString & Name);
  static UnicodeString __fastcall ExtractLocalName(const UnicodeString & Name);
  static UnicodeString __fastcall ExtractFolderName(const UnicodeString & Name);
  static UnicodeString __fastcall ComposePath(const UnicodeString & Path, const UnicodeString & Name);
  static bool __fastcall IsSensitiveOption(const UnicodeString & Option, const UnicodeString & Value);
  static bool __fastcall IsOptionWithParameters(const UnicodeString & Option);
  static bool __fastcall MaskPasswordInOptionParameter(const UnicodeString & Option, UnicodeString & Param);
  static UnicodeString __fastcall FormatSiteKey(const UnicodeString & HostName, int PortNumber);
  static TStrings * GetAllOptionNames(bool PuttyExport);

  __property UnicodeString HostName  = { read=FHostName, write=SetHostName };
  __property UnicodeString HostNameExpanded  = { read=GetHostNameExpanded };
  __property UnicodeString HostNameSource = { read=GetHostNameSource };
  __property int PortNumber  = { read=FPortNumber, write=SetPortNumber };
  __property UnicodeString UserName  = { read=FUserName, write=SetUserName };
  __property UnicodeString UserNameExpanded  = { read=GetUserNameExpanded };
  __property UnicodeString UserNameSource  = { read=GetUserNameSource };
  __property UnicodeString Password  = { read=GetPassword, write=SetPassword };
  __property UnicodeString NewPassword  = { read=GetNewPassword, write=SetNewPassword };
  __property bool ChangePassword  = { read=FChangePassword, write=SetChangePassword };
  __property int PingInterval  = { read=FPingInterval, write=SetPingInterval };
  __property bool TryAgent  = { read=FTryAgent, write=SetTryAgent };
  __property bool AgentFwd  = { read=FAgentFwd, write=SetAgentFwd };
  __property UnicodeString ListingCommand = { read = FListingCommand, write = SetListingCommand };
  __property bool AuthKI  = { read=FAuthKI, write=SetAuthKI };
  __property bool AuthKIPassword  = { read=FAuthKIPassword, write=SetAuthKIPassword };
  __property bool AuthGSSAPI  = { read=FAuthGSSAPI, write=SetAuthGSSAPI };
  __property bool AuthGSSAPIKEX  = { read=FAuthGSSAPIKEX, write=SetAuthGSSAPIKEX };
  __property bool GSSAPIFwdTGT = { read=FGSSAPIFwdTGT, write=SetGSSAPIFwdTGT };
  __property bool ChangeUsername  = { read=FChangeUsername, write=SetChangeUsername };
  __property bool Compression  = { read=FCompression, write=SetCompression };
  __property bool UsesSsh = { read = GetUsesSsh };
  __property bool Ssh2DES  = { read=FSsh2DES, write=SetSsh2DES };
  __property bool SshNoUserAuth  = { read=FSshNoUserAuth, write=SetSshNoUserAuth };
  __property TCipher Cipher[int Index] = { read=GetCipher, write=SetCipher };
  __property TKex Kex[int Index] = { read=GetKex, write=SetKex };
  __property THostKey HostKeys[int Index] = { read=GetHostKeys, write=SetHostKeys };
  __property TGssLib GssLib[int Index] = { read=GetGssLib, write=SetGssLib };
  __property UnicodeString GssLibCustom = { read=FGssLibCustom, write=SetGssLibCustom };
  __property UnicodeString PublicKeyFile  = { read=FPublicKeyFile, write=SetPublicKeyFile };
  __property UnicodeString Passphrase  = { read=GetPassphrase, write=SetPassphrase };
  __property UnicodeString DetachedCertificate  = { read=FDetachedCertificate, write=SetDetachedCertificate };
  __property UnicodeString PuttyProtocol  = { read=FPuttyProtocol, write=SetPuttyProtocol };
  __property TFSProtocol FSProtocol  = { read=FFSProtocol, write=SetFSProtocol  };
  __property UnicodeString FSProtocolStr  = { read=GetFSProtocolStr };
  __property bool Modified  = { read=FModified, write=FModified };
  __property bool CanLogin  = { read=GetCanLogin };
  __property bool CanOpen = { read=GetCanOpen };
  __property bool IsLocalBrowser = { read=GetIsLocalBrowser };
  __property bool ClearAliases = { read = FClearAliases, write = SetClearAliases };
  __property TDateTime PingIntervalDT = { read = GetPingIntervalDT, write = SetPingIntervalDT };
  __property TDateTime TimeDifference = { read = FTimeDifference, write = SetTimeDifference };
  __property bool TimeDifferenceAuto = { read = FTimeDifferenceAuto, write = SetTimeDifferenceAuto };
  __property TPingType PingType = { read = FPingType, write = SetPingType };
  __property UnicodeString SessionName  = { read=GetSessionName };
  __property UnicodeString DefaultSessionName  = { read=GetDefaultSessionName };
  __property UnicodeString LocalDirectory  = { read=FLocalDirectory, write=SetLocalDirectory };
  __property UnicodeString LocalDirectoryExpanded = { read = GetLocalDirectoryExpanded };
  __property UnicodeString OtherLocalDirectory = { read=FOtherLocalDirectory, write=SetOtherLocalDirectory };
  __property UnicodeString RemoteDirectory  = { read=FRemoteDirectory, write=SetRemoteDirectory };
  __property bool SynchronizeBrowsing = { read=FSynchronizeBrowsing, write=SetSynchronizeBrowsing };
  __property bool UpdateDirectories = { read=FUpdateDirectories, write=SetUpdateDirectories };
  __property bool RequireDirectories = { read=FRequireDirectories, write=FRequireDirectories };
  __property bool CacheDirectories = { read=FCacheDirectories, write=SetCacheDirectories };
  __property bool CacheDirectoryChanges = { read=FCacheDirectoryChanges, write=SetCacheDirectoryChanges };
  __property bool PreserveDirectoryChanges = { read=FPreserveDirectoryChanges, write=SetPreserveDirectoryChanges };
  __property bool Special = { read=FSpecial, write=SetSpecial };
  __property bool Selected  = { read=FSelected, write=FSelected };
  __property UnicodeString InfoTip  = { read=GetInfoTip };
  __property bool DefaultShell = { read = GetDefaultShell, write = SetDefaultShell };
  __property bool DetectReturnVar = { read = GetDetectReturnVar, write = SetDetectReturnVar };
  __property TEOLType EOLType = { read = FEOLType, write = SetEOLType };
  __property bool TrimVMSVersions = { read = FTrimVMSVersions, write = SetTrimVMSVersions };
  __property bool VMSAllRevisions = { read = FVMSAllRevisions, write = SetVMSAllRevisions };
  __property TAutoSwitch LookupUserGroups = { read = FLookupUserGroups, write = SetLookupUserGroups };
  __property UnicodeString ReturnVar = { read = FReturnVar, write = SetReturnVar };
  __property bool ExitCode1IsError = { read = FExitCode1IsError, write = SetExitCode1IsError };
  __property bool Scp1Compatibility = { read = FScp1Compatibility, write = SetScp1Compatibility };
  __property UnicodeString Shell = { read = FShell, write = SetShell };
  __property UnicodeString SftpServer = { read = FSftpServer, write = SetSftpServer };
  __property int Timeout = { read = FTimeout, write = SetTimeout };
  __property TDateTime TimeoutDT = { read = GetTimeoutDT };
  __property bool UnsetNationalVars = { read = FUnsetNationalVars, write = SetUnsetNationalVars };
  __property bool IgnoreLsWarnings  = { read=FIgnoreLsWarnings, write=SetIgnoreLsWarnings };
  __property bool TcpNoDelay  = { read=FTcpNoDelay, write=SetTcpNoDelay };
  __property int SendBuf  = { read=FSendBuf, write=SetSendBuf };
  __property UnicodeString SourceAddress = { read=FSourceAddress, write=SetSourceAddress };
  __property UnicodeString ProtocolFeatures = { read=FProtocolFeatures, write=SetProtocolFeatures };
  __property bool SshSimple  = { read=FSshSimple, write=SetSshSimple };
  __property UnicodeString CipherList  = { read=GetCipherList, write=SetCipherList };
  __property UnicodeString KexList  = { read=GetKexList, write=SetKexList };
  __property UnicodeString HostKeyList  = { read=GetHostKeyList, write=SetHostKeyList };
  __property UnicodeString GssLibList  = { read=GetGssLibList, write=SetGssLibList };
  __property TProxyMethod ProxyMethod  = { read=FProxyMethod, write=SetProxyMethod };
  __property UnicodeString ProxyHost  = { read=FProxyHost, write=SetProxyHost };
  __property int ProxyPort  = { read=FProxyPort, write=SetProxyPort };
  __property UnicodeString ProxyUsername  = { read=FProxyUsername, write=SetProxyUsername };
  __property UnicodeString ProxyPassword  = { read=GetProxyPassword, write=SetProxyPassword };
  __property UnicodeString ProxyTelnetCommand  = { read=FProxyTelnetCommand, write=SetProxyTelnetCommand };
  __property UnicodeString ProxyLocalCommand  = { read=FProxyLocalCommand, write=SetProxyLocalCommand };
  __property TAutoSwitch ProxyDNS  = { read=FProxyDNS, write=SetProxyDNS };
  __property bool ProxyLocalhost  = { read=FProxyLocalhost, write=SetProxyLocalhost };
  __property int FtpProxyLogonType  = { read=FFtpProxyLogonType, write=SetFtpProxyLogonType };
  __property TAutoSwitch Bug[TSshBug Bug]  = { read=GetBug, write=SetBug };
  __property UnicodeString PuttySettings = { read = FPuttySettings, write = SetPuttySettings };
  __property UnicodeString CustomParam1 = { read = FCustomParam1, write = SetCustomParam1 };
  __property UnicodeString CustomParam2 = { read = FCustomParam2, write = SetCustomParam2 };
  __property UnicodeString SessionKey = { read = GetSessionKey };
  __property bool ResolveSymlinks = { read = FResolveSymlinks, write = SetResolveSymlinks };
  __property bool FollowDirectorySymlinks = { read = FFollowDirectorySymlinks, write = SetFollowDirectorySymlinks };
  __property int SFTPDownloadQueue = { read = FSFTPDownloadQueue, write = SetSFTPDownloadQueue };
  __property int SFTPUploadQueue = { read = FSFTPUploadQueue, write = SetSFTPUploadQueue };
  __property int SFTPListingQueue = { read = FSFTPListingQueue, write = SetSFTPListingQueue };
  __property int SFTPMaxVersion = { read = FSFTPMaxVersion, write = SetSFTPMaxVersion };
  __property unsigned long SFTPMaxPacketSize = { read = FSFTPMaxPacketSize, write = SetSFTPMaxPacketSize };
  __property TAutoSwitch SFTPRealPath = { read = FSFTPRealPath, write = SetSFTPRealPath };
  __property bool UsePosixRename = { read = FUsePosixRename, write = SetUsePosixRename };
  __property TAutoSwitch SFTPBug[TSftpBug Bug]  = { read=GetSFTPBug, write=SetSFTPBug };
  __property TAutoSwitch SCPLsFullTime = { read = FSCPLsFullTime, write = SetSCPLsFullTime };
  __property TAutoSwitch FtpListAll = { read = FFtpListAll, write = SetFtpListAll };
  __property TAutoSwitch FtpHost = { read = FFtpHost, write = SetFtpHost };
  __property TAutoSwitch FtpWorkFromCwd = { read = FFtpWorkFromCwd, write = SetFtpWorkFromCwd };
  __property bool FtpAnyCodeForPwd = { read = FFtpAnyCodeForPwd, write = SetFtpAnyCodeForPwd };
  __property bool SslSessionReuse = { read = FSslSessionReuse, write = SetSslSessionReuse };
  __property UnicodeString TlsCertificateFile = { read=FTlsCertificateFile, write=SetTlsCertificateFile };
  __property TDSTMode DSTMode = { read = FDSTMode, write = SetDSTMode };
  __property bool DeleteToRecycleBin = { read = FDeleteToRecycleBin, write = SetDeleteToRecycleBin };
  __property bool OverwrittenToRecycleBin = { read = FOverwrittenToRecycleBin, write = SetOverwrittenToRecycleBin };
  __property UnicodeString RecycleBinPath = { read = FRecycleBinPath, write = SetRecycleBinPath };
  __property UnicodeString PostLoginCommands = { read = FPostLoginCommands, write = SetPostLoginCommands };
  __property TAddressFamily AddressFamily = { read = FAddressFamily, write = SetAddressFamily };
  __property UnicodeString RekeyData = { read = FRekeyData, write = SetRekeyData };
  __property unsigned int RekeyTime = { read = FRekeyTime, write = SetRekeyTime };
  __property int Color = { read = FColor, write = SetColor };
  __property bool Tunnel = { read = FTunnel, write = SetTunnel };
  __property UnicodeString TunnelHostName = { read = FTunnelHostName, write = SetTunnelHostName };
  __property int TunnelPortNumber = { read = FTunnelPortNumber, write = SetTunnelPortNumber };
  __property UnicodeString TunnelUserName = { read = FTunnelUserName, write = SetTunnelUserName };
  __property UnicodeString TunnelPassword = { read = GetTunnelPassword, write = SetTunnelPassword };
  __property UnicodeString TunnelPublicKeyFile = { read = FTunnelPublicKeyFile, write = SetTunnelPublicKeyFile };
  __property UnicodeString TunnelPassphrase = { read = GetTunnelPassphrase, write = SetTunnelPassphrase };
  __property bool TunnelAutoassignLocalPortNumber = { read = GetTunnelAutoassignLocalPortNumber };
  __property int TunnelLocalPortNumber = { read = FTunnelLocalPortNumber, write = SetTunnelLocalPortNumber };
  __property UnicodeString TunnelPortFwd = { read = FTunnelPortFwd, write = SetTunnelPortFwd };
  __property UnicodeString TunnelHostKey = { read = FTunnelHostKey, write = SetTunnelHostKey };
  __property bool FtpPasvMode = { read = FFtpPasvMode, write = SetFtpPasvMode };
  __property TAutoSwitch FtpForcePasvIp = { read = FFtpForcePasvIp, write = SetFtpForcePasvIp };
  __property TAutoSwitch FtpUseMlsd = { read = FFtpUseMlsd, write = SetFtpUseMlsd };
  __property UnicodeString FtpAccount = { read = FFtpAccount, write = SetFtpAccount };
  __property int FtpPingInterval  = { read=FFtpPingInterval, write=SetFtpPingInterval };
  __property TDateTime FtpPingIntervalDT  = { read=GetFtpPingIntervalDT };
  __property TFtpPingType FtpPingType = { read = FFtpPingType, write = SetFtpPingType };
  __property TAutoSwitch FtpTransferActiveImmediately = { read = FFtpTransferActiveImmediately, write = SetFtpTransferActiveImmediately };
  __property TFtps Ftps = { read = FFtps, write = SetFtps };
  __property TTlsVersion MinTlsVersion = { read = FMinTlsVersion, write = SetMinTlsVersion };
  __property TTlsVersion MaxTlsVersion = { read = FMaxTlsVersion, write = SetMaxTlsVersion };
  __property TAutoSwitch CompleteTlsShutdown = { read = FCompleteTlsShutdown, write = SetCompleteTlsShutdown };
  __property UnicodeString LogicalHostName = { read = FLogicalHostName, write = SetLogicalHostName };
  __property TAutoSwitch NotUtf = { read = FNotUtf, write = SetNotUtf };
  __property int InternalEditorEncoding = { read = FInternalEditorEncoding, write = SetInternalEditorEncoding };
  __property UnicodeString S3DefaultRegion = { read = FS3DefaultRegion, write = SetS3DefaultRegion };
  __property UnicodeString S3SessionToken = { read = FS3SessionToken, write = SetS3SessionToken };
  __property UnicodeString S3RoleArn = { read = FS3RoleArn, write = SetS3RoleArn };
  __property UnicodeString S3RoleSessionName = { read = FS3RoleSessionName, write = SetS3RoleSessionName };
  __property UnicodeString S3Profile = { read = FS3Profile, write = SetS3Profile };
  __property TS3UrlStyle S3UrlStyle = { read = FS3UrlStyle, write = SetS3UrlStyle };
  __property TAutoSwitch S3MaxKeys = { read = FS3MaxKeys, write = SetS3MaxKeys };
  __property bool S3CredentialsEnv = { read = FS3CredentialsEnv, write = SetS3CredentialsEnv };
  __property bool S3RequesterPays = { read = FS3RequesterPays, write = SetS3RequesterPays };
  __property bool IsWorkspace = { read = FIsWorkspace, write = SetIsWorkspace };
  __property UnicodeString Link = { read = FLink, write = SetLink };
  __property UnicodeString NameOverride = { read = FNameOverride, write = SetNameOverride };
  __property UnicodeString HostKey = { read = FHostKey, write = SetHostKey };
  __property bool FingerprintScan = { read = FFingerprintScan, write = FFingerprintScan };
  __property bool OverrideCachedHostKey = { read = FOverrideCachedHostKey };
  __property UnicodeString Note = { read = FNote, write = SetNote };
  __property UnicodeString WinTitle = { read = FWinTitle, write = SetWinTitle };
  __property UnicodeString EncryptKey = { read = GetEncryptKey, write = SetEncryptKey };
  __property bool WebDavLiberalEscaping = { read = FWebDavLiberalEscaping, write = SetWebDavLiberalEscaping };
  __property bool WebDavAuthLegacy = { read = FWebDavAuthLegacy, write = SetWebDavAuthLegacy };

  __property UnicodeString StorageKey = { read = GetStorageKey };
  __property UnicodeString SiteKey = { read = GetSiteKey };
  __property UnicodeString OrigHostName = { read = FOrigHostName };
  __property int OrigPortNumber = { read = FOrigPortNumber };
  __property UnicodeString LocalName = { read = GetLocalName };
  __property UnicodeString FolderName = { read = GetFolderName };
  __property TSessionSource Source = { read = FSource };
  __property UnicodeString SourceName = { read = GetSourceName };
  __property bool SaveOnly = { read = FSaveOnly };
};
//---------------------------------------------------------------------------
class TStoredSessionList : public TNamedObjectList
{
public:
  __fastcall TStoredSessionList(bool aReadOnly = false);
  void __fastcall Reload();
  void __fastcall Save(bool All, bool Explicit);
  void __fastcall Saved();
  void __fastcall ImportFromFilezilla(const UnicodeString FileName, const UnicodeString ConfigurationFileName);
  void __fastcall ImportFromKnownHosts(TStrings * Lines);
  void ImportFromOpenssh(TStrings * Lines);
  void __fastcall Export(const UnicodeString FileName);
  void __fastcall Load(THierarchicalStorage * Storage, bool AsModified = false,
    bool UseDefaults = false, bool PuttyImport = false);
  void __fastcall Save(THierarchicalStorage * Storage, bool All = false);
  void __fastcall SelectAll(bool Select);
  bool Import(TStoredSessionList * From, bool OnlySelected, TList * Imported);
  void __fastcall RecryptPasswords(TStrings * RecryptPasswordErrors);
  TSessionData * __fastcall AtSession(int Index)
    { return (TSessionData*)AtObject(Index); }
  void __fastcall SelectSessionsToImport(TStoredSessionList * Dest, bool SSHOnly);
  void __fastcall Cleanup();
  void __fastcall UpdateStaticUsage();
  int __fastcall IndexOf(TSessionData * Data);
  TSessionData * __fastcall FindSame(TSessionData * Data);
  TSessionData * __fastcall NewSession(UnicodeString SessionName, TSessionData * Session);
  void __fastcall NewWorkspace(UnicodeString Name, TList * DataList);
  bool __fastcall IsFolder(const UnicodeString & Name);
  bool __fastcall IsWorkspace(const UnicodeString & Name);
  bool __fastcall IsFolderOrWorkspace(const UnicodeString & Name);
  TSessionData * __fastcall ParseUrl(UnicodeString Url, TOptions * Options, bool & DefaultsOnly,
    UnicodeString * FileName = NULL, bool * ProtocolDefined = NULL, UnicodeString * MaskedUrl = NULL, int Flags = 0);
  bool __fastcall IsUrl(UnicodeString Url);
  bool __fastcall CanOpen(TSessionData * Data);
  void __fastcall GetFolderOrWorkspace(const UnicodeString & Name, TList * List);
  TStrings * __fastcall GetFolderOrWorkspaceList(const UnicodeString & Name);
  TStrings * __fastcall GetWorkspaces();
  bool __fastcall HasAnyWorkspace();
  TSessionData * __fastcall SaveWorkspaceData(TSessionData * Data, int Index);
  virtual __fastcall ~TStoredSessionList();
  __property TSessionData * Sessions[int Index]  = { read=AtSession };
  __property TSessionData * DefaultSettings  = { read=FDefaultSettings, write=SetDefaultSettings };

  static int ImportHostKeys(
    THierarchicalStorage * SourceStorage, THierarchicalStorage * TargetStorage, TStoredSessionList * Sessions, bool OnlySelected);
  static void ImportHostKeys(THierarchicalStorage * SourceStorage, TStoredSessionList * Sessions, bool OnlySelected);
  static void ImportHostKeys(const UnicodeString & SourceKey, TStoredSessionList * Sessions, bool OnlySelected);
  static void __fastcall ImportSelectedKnownHosts(TStoredSessionList * Sessions);
  static bool __fastcall OpenHostKeysSubKey(THierarchicalStorage * Storage, bool CanCreate);
  static void SelectKnownHostsForSelectedSessions(TStoredSessionList * KnownHosts, TStoredSessionList * Sessions);

private:
  TSessionData * FDefaultSettings;
  bool FReadOnly;
  std::unique_ptr<TStrings> FPendingRemovals;
  void __fastcall SetDefaultSettings(TSessionData * value);
  void __fastcall DoSave(THierarchicalStorage * Storage, bool All,
    bool RecryptPasswordOnly, TStrings * RecryptPasswordErrors);
  void __fastcall DoSave(bool All, bool Explicit, bool RecryptPasswordOnly,
    TStrings * RecryptPasswordErrors);
  void __fastcall DoSave(THierarchicalStorage * Storage,
    TSessionData * Data, bool All, bool RecryptPasswordOnly,
    TSessionData * FactoryDefaults);
  TSessionData * __fastcall ResolveWorkspaceData(TSessionData * Data);
  TSessionData * GetFirstFolderOrWorkspaceSession(const UnicodeString & Name);
  TSessionData * __fastcall CheckIsInFolderOrWorkspaceAndResolve(
    TSessionData * Data, const UnicodeString & Name);
  void __fastcall ImportLevelFromFilezilla(_di_IXMLNode Node, const UnicodeString & Path, _di_IXMLNode SettingsNode);
  void __fastcall DoGetFolderOrWorkspace(const UnicodeString & Name, TList * List, bool NoRecrypt);
  static THierarchicalStorage * __fastcall CreateHostKeysStorageForWriting();
};
//---------------------------------------------------------------------------
UnicodeString GetExpandedLogFileName(UnicodeString LogFileName, TDateTime Started, TSessionData * SessionData);
bool __fastcall IsSshProtocol(TFSProtocol FSProtocol);
int __fastcall DefaultPort(TFSProtocol FSProtocol, TFtps Ftps);
bool HasIP6LiteralBrackets(const UnicodeString & HostName);
UnicodeString StripIP6LiteralBrackets(const UnicodeString & HostName);
bool __fastcall IsIPv6Literal(const UnicodeString & HostName);
UnicodeString __fastcall EscapeIPv6Literal(const UnicodeString & IP);
TFSProtocol NormalizeFSProtocol(TFSProtocol FSProtocol);
bool ParseOpensshDirective(const UnicodeString & ALine, UnicodeString & Directive, UnicodeString & Value);
UnicodeString CutOpensshToken(UnicodeString & S);
UnicodeString ConvertPathFromOpenssh(const UnicodeString & Path);
UnicodeString GetTlsVersionName(TTlsVersion TlsVersion);
//---------------------------------------------------------------------------
#endif
