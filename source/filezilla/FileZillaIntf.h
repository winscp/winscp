//---------------------------------------------------------------------------
#ifndef FileZillaIntfH
#define FileZillaIntfH
//---------------------------------------------------------------------------
#include <map>

#include <time.h>
#include <FileZillaOpt.h>
#include <FileZillaTools.h>
//---------------------------------------------------------------------------
class CFileZillaApi;
class TFileZillaIntern;
//---------------------------------------------------------------------------
struct TRemoteFileTime
{
  int Year;
  int Month;
  int Day;
  int Hour;
  int Minute;
  int Second;
  bool HasTime;
  bool HasYear;
  bool HasSeconds;
  bool HasDate;
  bool Utc;
};
//---------------------------------------------------------------------------
struct TListDataEntry
{
  const wchar_t * Name;
  const wchar_t * Permissions;
  const wchar_t * HumanPerm;
  const wchar_t * OwnerGroup; // deprecated, to be replaced with Owner/Group
  const wchar_t * Owner;
  const wchar_t * Group;
  __int64 Size;
  bool Dir;
  bool Link;
  TRemoteFileTime Time;
  const wchar_t * LinkTarget;
};
//---------------------------------------------------------------------------
struct TFtpsCertificateData
{
  struct TContact
  {
    const wchar_t * Organization;
    const wchar_t * Unit;
    const wchar_t * CommonName;
    const wchar_t * Mail;
    const wchar_t * Country;
    const wchar_t * StateProvince;
    const wchar_t * Town;
    const wchar_t * Other;
  };

  TContact Subject;
  TContact Issuer;

  struct TValidityTime
  {
    int Year;
    int Month;
    int Day;
    int Hour;
    int Min;
    int Sec;
  };

  TValidityTime ValidFrom;
  TValidityTime ValidUntil;

  const wchar_t * SubjectAltName;

  const unsigned char * HashSha1;
  static const size_t HashSha1Len = 20;
  const unsigned char * HashSha256;
  static const size_t HashSha256Len = 32;

  const unsigned char * Certificate;
  size_t CertificateLen;

  int VerificationResult;
  int VerificationDepth;
};
//---------------------------------------------------------------------------
struct TNeedPassRequestData
{
    wchar_t * Password;
};
//---------------------------------------------------------------------------
class t_server;
class TFTPServerCapabilities;
typedef struct x509_st X509;
typedef struct evp_pkey_st EVP_PKEY;
//---------------------------------------------------------------------------
class TFileZillaIntf : public CFileZillaTools
{
friend class TFileZillaIntern;

public:
  enum TLogLevel
  {
    LOG_STATUS = 0,
    LOG_ERROR = 1,
    LOG_COMMAND = 2,
    LOG_REPLY = 3,
    LOG_APIERROR = 5,
    LOG_WARNING = 6,
    LOG_PROGRESS = 7,
    LOG_INFO = 8,
    LOG_DEBUG = 9
  };

  enum TMessageType
  {
    MSG_OTHER = 0,
    MSG_TRANSFERSTATUS = 1
  };

  enum
  {
    FILEEXISTS_OVERWRITE = 0,
    FILEEXISTS_RESUME = 1,
    FILEEXISTS_RENAME = 2,
    FILEEXISTS_SKIP = 3,
    FILEEXISTS_COMPLETE = 4,
  };

  enum
  {
    REPLY_OK = 0x0001,
    REPLY_WOULDBLOCK = 0x0002,
    REPLY_ERROR = 0x0004,
    REPLY_OWNERNOTSET = 0x0008,
    REPLY_INVALIDPARAM = 0x0010,
    REPLY_NOTCONNECTED = 0x0020,
    REPLY_ALREADYCONNECTED = 0x0040,
    REPLY_BUSY = 0x0080,
    REPLY_IDLE = 0x0100,
    REPLY_NOTINITIALIZED = 0x0200,
    REPLY_ALREADYINIZIALIZED = 0x0400,
    REPLY_CANCEL = 0x0800,
    REPLY_DISCONNECTED = 0x1000, // Always sent when disconnected from server
    REPLY_CRITICALERROR = 0x2000, // Used for FileTransfers only
    REPLY_ABORTED = 0x4000, // Used for FileTransfers only
    REPLY_NOTSUPPORTED = 0x8000 // Command is not supported for the current server
  };

  enum
  {
    SERVER_FTP =              0x1000,
    SERVER_FTP_SSL_IMPLICIT = 0x1100,
    SERVER_FTP_SSL_EXPLICIT = 0x1200,
    SERVER_FTP_TLS_EXPLICIT = 0x1400
  };

  static void __fastcall Initialize();
  static void __fastcall Finalize();

  __fastcall TFileZillaIntf();
  virtual __fastcall ~TFileZillaIntf();

  bool __fastcall Init();
  void __fastcall Destroying();

  bool __fastcall SetCurrentPath(const wchar_t * Path);
  bool __fastcall GetCurrentPath(wchar_t * Path, size_t MaxLen);

  bool __fastcall UsingMlsd();
  bool __fastcall UsingUtf8();
  std::string __fastcall GetTlsVersionStr();
  std::string __fastcall GetCipherName();

  bool __fastcall Cancel();

  bool __fastcall Connect(const wchar_t * Host, int Port, const wchar_t * User,
    const wchar_t * Pass, const wchar_t * Account,
    const wchar_t * Path, int ServerType, int Pasv, int TimeZoneOffset, int UTF8,
    int iForcePasvIp, int iUseMlsd,
    X509 * Certificate, EVP_PKEY * PrivateKey);
  bool __fastcall Close(bool AllowBusy);

  bool __fastcall List(const wchar_t * Path);
  bool __fastcall ListFile(const wchar_t * FileName, const wchar_t * APath);

  bool __fastcall CustomCommand(const wchar_t * Command);

  bool __fastcall MakeDir(const wchar_t* Path);
  bool __fastcall Chmod(int Value, const wchar_t* FileName, const wchar_t* Path);
  bool __fastcall Delete(const wchar_t* FileName, const wchar_t* Path, bool FileNameOnly);
  bool __fastcall RemoveDir(const wchar_t* FileName, const wchar_t* Path);
  bool __fastcall Rename(const wchar_t* OldName, const wchar_t* NewName,
    const wchar_t* Path, const wchar_t* NewPath);

  bool __fastcall FileTransfer(
    const wchar_t * LocalFile, const wchar_t * RemoteFile,
    const wchar_t * RemotePath, bool Get, __int64 Size, int Type, void * UserData,
    TTransferOutEvent OnTransferOut, TTransferInEvent OnTransferIn);

  virtual const wchar_t * __fastcall Option(int OptionID) const = 0;
  virtual int __fastcall OptionVal(int OptionID) const = 0;

  void __fastcall SetDebugLevel(TLogLevel Level);
  bool __fastcall HandleMessage(WPARAM wParam, LPARAM lParam);

protected:
  bool __fastcall PostMessage(WPARAM wParam, LPARAM lParam);
  virtual bool __fastcall DoPostMessage(TMessageType Type, WPARAM wParam, LPARAM lParam) = 0;

  virtual bool __fastcall HandleStatus(const wchar_t * Status, int Type) = 0;
  virtual bool __fastcall HandleAsynchRequestOverwrite(
    wchar_t * FileName1, size_t FileName1Len, const wchar_t * FileName2,
    const wchar_t * Path1, const wchar_t * Path2,
    __int64 Size1, __int64 Size2, time_t LocalTime,
    bool HasLocalTime1, const TRemoteFileTime & RemoteTime, void * UserData, int & RequestResult) = 0;
  virtual bool __fastcall HandleAsynchRequestVerifyCertificate(
    const TFtpsCertificateData & Data, int & RequestResult) = 0;
  virtual bool __fastcall HandleAsynchRequestNeedPass(
    struct TNeedPassRequestData & Data, int & RequestResult) = 0;
  virtual bool __fastcall HandleListData(const wchar_t * Path, const TListDataEntry * Entries,
    unsigned int Count) = 0;
  virtual bool __fastcall HandleTransferStatus(bool Valid, __int64 TransferSize,
    __int64 Bytes, bool FileTransfer) = 0;
  virtual bool __fastcall HandleReply(int Command, unsigned int Reply) = 0;
  virtual bool __fastcall HandleCapabilities(TFTPServerCapabilities * ServerCapabilities) = 0;
  virtual bool __fastcall CheckError(int ReturnCode, const wchar_t * Context);

  inline bool __fastcall Check(int ReturnCode, const wchar_t * Context, int Expected = -1);

private:
  CFileZillaApi * FFileZillaApi;
  TFileZillaIntern * FIntern;
  t_server * FServer;
};
//---------------------------------------------------------------------------
enum ftp_capabilities_t
{
  unknown,
  yes,
  no
};
//---------------------------------------------------------------------------
enum ftp_capability_names_t
{
  syst_command = 1, // reply of SYST command as option
  feat_command,
  clnt_command, // set to 'yes' if CLNT should be sent
  utf8_command, // set to 'yes' if OPTS UTF8 ON should be sent
  mlsd_command,
  opts_mlst_command, // Arguments for OPTS MLST command
  mfmt_command,
  pret_command,
  mdtm_command,
  mode_z_support,
  tvfs_support, // Trivial virtual file store (RFC 3659)
  list_hidden_support, // LIST -a command
  rest_stream, // supports REST+STOR in addition to APPE
};
//---------------------------------------------------------------------------
class TFTPServerCapabilities
{
public:
  ftp_capabilities_t GetCapability(ftp_capability_names_t Name);
  ftp_capabilities_t GetCapabilityString(ftp_capability_names_t Name, std::string * Option = NULL);
  void SetCapability(ftp_capability_names_t Name, ftp_capabilities_t Cap);
  void SetCapability(ftp_capability_names_t Name, ftp_capabilities_t Cap, const std::string & Option);
  void Clear() { FCapabilityMap.clear(); }
  void Assign(TFTPServerCapabilities * Source)
  {
    FCapabilityMap.clear();
    if (Source != NULL)
    {
      FCapabilityMap = Source->FCapabilityMap;
    }
  }
protected:
  struct t_cap
  {
    t_cap() :
      cap(unknown),
      option(),
      number(0)
    {}
    ftp_capabilities_t cap;
    std::string option;
    int number;
  };

  std::map<ftp_capability_names_t, t_cap> FCapabilityMap;
};
//---------------------------------------------------------------------------
#endif // FileZillaIntfH
