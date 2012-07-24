//---------------------------------------------------------------------------
#ifndef FileZillaIntfH
#define FileZillaIntfH
//---------------------------------------------------------------------------
#include <time.h>
#include <FileZillaOpt.h>
//---------------------------------------------------------------------------
class CFileZillaApi;
class TFileZillaIntern;
//---------------------------------------------------------------------------
struct TListDataEntry
{
  const char * Name;
  const char * Permissions;
  const char * OwnerGroup;
  __int64 Size;
  bool Dir;
  bool Link;
  int Year;
  int Month;
  int Day;
  int Hour;
  int Minute;
  bool HasTime;
  bool HasDate;
  const char * LinkTarget;
};
//---------------------------------------------------------------------------
struct TFtpsCertificateData
{
  struct TContact
  {
    const char * Organization;
    const char * Unit;
    const char * CommonName;
    const char * Mail;
    const char * Country;
    const char * StateProvince;
    const char * Town;
    const char * Other;
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

  const unsigned char * Hash;
  static const size_t HashLen = 20;

  int VerificationResult;
  int VerificationDepth;
};
//---------------------------------------------------------------------------
class t_server;
//---------------------------------------------------------------------------
class TFileZillaIntf
{
friend class TFileZillaIntern;

public:
  enum TLogLevel
  {
    LOG_STATUS = 0,
    LOG_ERROR = 1,
    LOG_COMMAND = 2,
    LOG_REPLY = 3,
    LOG_LIST = 4,
    LOG_APIERROR = 5,
    LOG_WARNING = 6,
    LOG_INFO = 7,
    LOG_DEBUG = 8
  };

  enum TMessageType
  {
    MSG_OTHER = 0,
    MSG_TRANSFERSTATUS = 1
  };

  enum
  {
    FILEEXISTS_OVERWRITE = 0,
    FILEEXISTS_OVERWRITEIFNEWER = 1,
    FILEEXISTS_RESUME = 2,
    FILEEXISTS_RENAME = 3,
    FILEEXISTS_SKIP = 4,
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
  static void __fastcall SetResourceModule(void * ResourceHandle);

  __fastcall TFileZillaIntf();
  __fastcall ~TFileZillaIntf();

  bool __fastcall Init();
  void __fastcall Destroying();

  bool __fastcall SetCurrentPath(const char * Path);
  bool __fastcall GetCurrentPath(char * Path, size_t MaxLen);

  bool __fastcall Cancel();

  bool __fastcall Connect(const char * Host, int Port, const char * User,
    const char * Pass, const char * Account, bool FwByPass,
    const char * Path, int ServerType, int Pasv, int TimeZoneOffset, int UTF8,
    int iForcePasvIp);
  bool __fastcall Close();

  bool __fastcall List();
  bool __fastcall List(const char * Path);

  bool __fastcall CustomCommand(const char * Command);

  bool __fastcall MakeDir(const char* Path);
  bool __fastcall Chmod(int Value, const char* FileName, const char* Path);
  bool __fastcall Delete(const char* FileName, const char* Path);
  bool __fastcall RemoveDir(const char* FileName, const char* Path);
  bool __fastcall Rename(const char* OldName, const char* NewName,
    const char* Path, const char* NewPath);

  bool __fastcall FileTransfer(const char * LocalFile, const char * RemoteFile,
    const char * RemotePath, bool Get, __int64 Size, int Type, void * UserData);

  virtual const char * __fastcall Option(int OptionID) const = 0;
  virtual int __fastcall OptionVal(int OptionID) const = 0;

  void __fastcall SetDebugLevel(TLogLevel Level);
  bool __fastcall HandleMessage(WPARAM wParam, LPARAM lParam);

protected:
  bool __fastcall PostMessage(WPARAM wParam, LPARAM lParam);
  virtual bool __fastcall DoPostMessage(TMessageType Type, WPARAM wParam, LPARAM lParam) = 0;

  virtual bool __fastcall HandleStatus(const char * Status, int Type) = 0;
  virtual bool __fastcall HandleAsynchRequestOverwrite(
    char * FileName1, size_t FileName1Len, const char * FileName2,
    const char * Path1, const char * Path2,
    __int64 Size1, __int64 Size2, time_t Time1, time_t Time2,
    bool HasTime1, bool HasTime2, void * UserData, int & RequestResult) = 0;
  virtual bool __fastcall HandleAsynchRequestVerifyCertificate(
    const TFtpsCertificateData & Data, int & RequestResult) = 0;
  virtual bool __fastcall HandleListData(const char * Path, const TListDataEntry * Entries,
    unsigned int Count) = 0;
  virtual bool __fastcall HandleTransferStatus(bool Valid, __int64 TransferSize,
    __int64 Bytes, int Percent, int TimeElapsed, int TimeLeft, int TransferRate,
    bool FileTransfer) = 0;
  virtual bool __fastcall HandleReply(int Command, unsigned int Reply) = 0;
  virtual bool __fastcall HandleCapabilities(bool Mfmt) = 0;
  virtual bool __fastcall CheckError(int ReturnCode, const char * Context);

  inline bool __fastcall Check(int ReturnCode, const char * Context, int Expected = -1);

private:
  CFileZillaApi * FFileZillaApi;
  TFileZillaIntern * FIntern;
  t_server * FServer;
};
//---------------------------------------------------------------------------
#endif // FileZillaIntfH
