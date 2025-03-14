//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

//---------------------------------------------------------------------------
#include <list>
#include "FtpFileSystem.h"
#include "FileZillaIntf.h"

#include "Common.h"
#include "Exceptions.h"
#include "Terminal.h"
#include "TextsCore.h"
#include "TextsFileZilla.h"
#include "HelpCore.h"
#include "Security.h"
#include "NeonIntf.h"
#include "SessionInfo.h"
#include "Cryptography.h"
#include <StrUtils.hpp>
#include <DateUtils.hpp>
#include <openssl/x509_vfy.h>
#include <openssl/err.h>
#include <limits>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_TERMINAL FTerminal
//---------------------------------------------------------------------------
const int DummyCodeClass = 8;
const int DummyTimeoutCode = 801;
const int DummyDisconnectCode = 803;
//---------------------------------------------------------------------------
class TFileZillaImpl : public TFileZillaIntf
{
public:
  __fastcall TFileZillaImpl(TFTPFileSystem * FileSystem);

  virtual const wchar_t * __fastcall Option(int OptionID) const;
  virtual int __fastcall OptionVal(int OptionID) const;

protected:
  virtual bool __fastcall DoPostMessage(TMessageType Type, WPARAM wParam, LPARAM lParam);

  virtual bool __fastcall HandleStatus(const wchar_t * Status, int Type);
  virtual bool __fastcall HandleAsynchRequestOverwrite(
    wchar_t * FileName1, size_t FileName1Len, const wchar_t * FileName2,
    const wchar_t * Path1, const wchar_t * Path2,
    __int64 Size1, __int64 Size2, time_t LocalTime,
    bool HasLocalTime, const TRemoteFileTime & RemoteTime, void * UserData, int & RequestResult);
  virtual bool __fastcall HandleAsynchRequestVerifyCertificate(
    const TFtpsCertificateData & Data, int & RequestResult);
  virtual bool __fastcall HandleAsynchRequestNeedPass(
    struct TNeedPassRequestData & Data, int & RequestResult);
  virtual bool __fastcall HandleListData(const wchar_t * Path, const TListDataEntry * Entries,
    unsigned int Count);
  virtual bool __fastcall HandleTransferStatus(bool Valid, __int64 TransferSize,
    __int64 Bytes, bool FileTransfer);
  virtual bool __fastcall HandleReply(int Command, unsigned int Reply);
  virtual bool __fastcall HandleCapabilities(TFTPServerCapabilities * ServerCapabilities);
  virtual bool __fastcall CheckError(int ReturnCode, const wchar_t * Context);

  virtual void PreserveDownloadFileTime(HANDLE Handle, void * UserData);
  virtual bool GetFileModificationTimeInUtc(const wchar_t * FileName, struct tm & Time);
  virtual wchar_t * LastSysErrorMessage();
  virtual std::wstring GetClientString();
  virtual void SetupSsl(ssl_st * Ssl);
  virtual std::wstring CustomReason(int Err);

private:
  TFTPFileSystem * FFileSystem;
};
//---------------------------------------------------------------------------
__fastcall TFileZillaImpl::TFileZillaImpl(TFTPFileSystem * FileSystem) :
  TFileZillaIntf(),
  FFileSystem(FileSystem)
{
}
//---------------------------------------------------------------------------
const wchar_t * __fastcall TFileZillaImpl::Option(int OptionID) const
{
  return FFileSystem->GetOption(OptionID);
}
//---------------------------------------------------------------------------
int __fastcall TFileZillaImpl::OptionVal(int OptionID) const
{
  return FFileSystem->GetOptionVal(OptionID);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::DoPostMessage(TMessageType Type, WPARAM wParam, LPARAM lParam)
{
  return FFileSystem->PostMessage(Type, wParam, lParam);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleStatus(const wchar_t * Status, int Type)
{
  return FFileSystem->HandleStatus(Status, Type);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleAsynchRequestOverwrite(
  wchar_t * FileName1, size_t FileName1Len, const wchar_t * FileName2,
  const wchar_t * Path1, const wchar_t * Path2,
  __int64 Size1, __int64 Size2, time_t LocalTime,
  bool HasLocalTime, const TRemoteFileTime & RemoteTime, void * UserData, int & RequestResult)
{
  return FFileSystem->HandleAsynchRequestOverwrite(
    FileName1, FileName1Len, FileName2, Path1, Path2, Size1, Size2, LocalTime,
    HasLocalTime, RemoteTime, UserData, RequestResult);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleAsynchRequestVerifyCertificate(
  const TFtpsCertificateData & Data, int & RequestResult)
{
  return FFileSystem->HandleAsynchRequestVerifyCertificate(Data, RequestResult);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleAsynchRequestNeedPass(
  struct TNeedPassRequestData & Data, int & RequestResult)
{
  return FFileSystem->HandleAsynchRequestNeedPass(Data, RequestResult);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleListData(const wchar_t * Path,
  const TListDataEntry * Entries, unsigned int Count)
{
  return FFileSystem->HandleListData(Path, Entries, Count);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleTransferStatus(bool Valid, __int64 TransferSize,
  __int64 Bytes, bool FileTransfer)
{
  return FFileSystem->HandleTransferStatus(Valid, TransferSize, Bytes, FileTransfer);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleReply(int Command, unsigned int Reply)
{
  return FFileSystem->HandleReply(Command, Reply);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleCapabilities(TFTPServerCapabilities * ServerCapabilities)
{
  return FFileSystem->HandleCapabilities(ServerCapabilities);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::CheckError(int ReturnCode, const wchar_t * Context)
{
  return FFileSystem->CheckError(ReturnCode, Context);
}
//---------------------------------------------------------------------------
void TFileZillaImpl::PreserveDownloadFileTime(HANDLE Handle, void * UserData)
{
  return FFileSystem->PreserveDownloadFileTime(Handle, UserData);
}
//---------------------------------------------------------------------------
bool TFileZillaImpl::GetFileModificationTimeInUtc(const wchar_t * FileName, struct tm & Time)
{
  return FFileSystem->GetFileModificationTimeInUtc(FileName, Time);
}
//---------------------------------------------------------------------------
wchar_t * TFileZillaImpl::LastSysErrorMessage()
{
  return _wcsdup(::LastSysErrorMessage().c_str());
}
//---------------------------------------------------------------------------
std::wstring TFileZillaImpl::GetClientString()
{
  return std::wstring(SshVersionString().c_str());
}
//---------------------------------------------------------------------------
void TFileZillaImpl::SetupSsl(ssl_st * Ssl)
{
  TSessionData * SessionData = FFileSystem->FTerminal->SessionData;
  ::SetupSsl(Ssl, SessionData->MinTlsVersion, SessionData->MaxTlsVersion);
}
//---------------------------------------------------------------------------
std::wstring TFileZillaImpl::CustomReason(int Err)
{
  std::wstring Result;
  int Lib = ERR_GET_LIB(Err);
  int Reason = ERR_GET_REASON(Err);
  if ((Lib == ERR_LIB_SSL) &&
      ((Reason == SSL_R_UNSUPPORTED_PROTOCOL) ||
       (Reason == SSL_R_TLSV1_ALERT_PROTOCOL_VERSION) ||
       (Reason == SSL_R_WRONG_SSL_VERSION) ||
       (Reason == SSL_R_WRONG_VERSION_NUMBER)))
  {
    TSessionData * SessionData = FFileSystem->FTerminal->SessionData;
    Result =
      FMTLOAD(
        TLS_UNSUPPORTED, (
          GetTlsVersionName(SessionData->MinTlsVersion), GetTlsVersionName(SessionData->MaxTlsVersion),
          GetTlsVersionName(tlsMin), GetTlsVersionName(tlsMax))).c_str();
  }
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TMessageQueue : public std::list<std::pair<WPARAM, LPARAM> >
{
};
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
struct TFileTransferData
{
  TFileTransferData()
  {
    Params = 0;
    AutoResume = false;
    OverwriteResult = -1;
    CopyParam = NULL;
  }

  UnicodeString FileName;
  int Params;
  bool AutoResume;
  int OverwriteResult;
  const TCopyParamType * CopyParam;
  TDateTime Modification;
};
//---------------------------------------------------------------------------
const UnicodeString SiteCommand(L"SITE");
const UnicodeString SymlinkSiteCommand(L"SYMLINK");
const UnicodeString CopySiteCommand(L"COPY");
const UnicodeString HashCommand(L"HASH"); // Cerberos + FileZilla servers
const UnicodeString AvblCommand(L"AVBL");
const UnicodeString XQuotaCommand(L"XQUOTA");
const UnicodeString MdtmCommand(L"MDTM");
const UnicodeString SizeCommand(L"SIZE");
const UnicodeString CsidCommand(L"CSID");
const UnicodeString DirectoryHasBytesPrefix(L"226-Directory has");
//---------------------------------------------------------------------------
class TFileListHelper
{
public:
  TFileListHelper(TFTPFileSystem * FileSystem, TRemoteFileList * FileList,
      bool IgnoreFileList) :
    FFileSystem(FileSystem),
    FFileList(FFileSystem->FFileList),
    FIgnoreFileList(FFileSystem->FIgnoreFileList)
  {
    FFileSystem->FFileList = FileList;
    FFileSystem->FIgnoreFileList = IgnoreFileList;
  }

  ~TFileListHelper()
  {
    FFileSystem->FFileList = FFileList;
    FFileSystem->FIgnoreFileList = FIgnoreFileList;
  }

private:
  TFTPFileSystem * FFileSystem;
  TRemoteFileList * FFileList;
  bool FIgnoreFileList;
};
//---------------------------------------------------------------------------
__fastcall TFTPFileSystem::TFTPFileSystem(TTerminal * ATerminal):
  TCustomFileSystem(ATerminal),
  FFileZillaIntf(NULL),
  FQueueCriticalSection(new TCriticalSection),
  FTransferStatusCriticalSection(new TCriticalSection),
  FQueue(new TMessageQueue),
  FQueueEvent(CreateEvent(NULL, true, false, NULL)),
  FFileSystemInfoValid(false),
  FReply(0),
  FCommandReply(0),
  FLastCommand(CMD_UNKNOWN),
  FLastResponse(new TStringList()),
  FLastErrorResponse(new TStringList()),
  FLastError(new TStringList()),
  FFeatures(new TStringList()),
  FReadCurrentDirectory(false),
  FFileList(NULL),
  FFileListCache(NULL),
  FActive(false),
  FWaitingForReply(false),
  FIgnoreFileList(false),
  FOnCaptureOutput(NULL),
  FDoListAll(false),
  FServerCapabilities(NULL)
{
  ResetReply();

  FListAll = FTerminal->SessionData->FtpListAll;
  FWorkFromCwd = FTerminal->SessionData->FtpWorkFromCwd;
  FFileSystemInfo.ProtocolBaseName = L"FTP";
  FFileSystemInfo.ProtocolName = FFileSystemInfo.ProtocolBaseName;
  FTimeoutStatus = LoadStr(IDS_ERRORMSG_TIMEOUT);
  FDisconnectStatus = LoadStr(IDS_STATUSMSG_DISCONNECTED);
  FServerCapabilities = new TFTPServerCapabilities();
  FHashAlgs.reset(new TStringList());
  FSupportedCommands.reset(CreateSortedStringList());
  FSupportedSiteCommands.reset(CreateSortedStringList());
  FCertificate = NULL;
  FPrivateKey = NULL;
  FBytesAvailable = -1;
  FBytesAvailableSupported = false;
  FLoggedIn = false;
  FAnyTransferSucceeded = false; // Do not reset on reconnect
  FForceReadSymlink = false;

  FChecksumAlgs.reset(new TStringList());
  FChecksumCommands.reset(new TStringList());
  RegisterChecksumAlgCommand(Sha1ChecksumAlg, L"XSHA1"); // e.g. Cerberos FTP
  RegisterChecksumAlgCommand(Sha256ChecksumAlg, L"XSHA256"); // e.g. Cerberos FTP
  RegisterChecksumAlgCommand(Sha512ChecksumAlg, L"XSHA512"); // e.g. Cerberos FTP
  RegisterChecksumAlgCommand(Md5ChecksumAlg, L"XMD5"); // e.g. Cerberos FTP
  RegisterChecksumAlgCommand(Md5ChecksumAlg, L"MD5"); // e.g. Apache FTP
  RegisterChecksumAlgCommand(Crc32ChecksumAlg, L"XCRC"); // e.g. Cerberos FTP
}
//---------------------------------------------------------------------------
__fastcall TFTPFileSystem::~TFTPFileSystem()
{
  DebugAssert(FFileList == NULL);

  FFileZillaIntf->Destroying();

  // to release memory associated with the messages
  DiscardMessages();

  delete FFileZillaIntf;
  FFileZillaIntf = NULL;

  delete FQueue;
  FQueue = NULL;

  CloseHandle(FQueueEvent);

  delete FQueueCriticalSection;
  FQueueCriticalSection = NULL;
  delete FTransferStatusCriticalSection;
  FTransferStatusCriticalSection = NULL;

  delete FLastResponse;
  FLastResponse = NULL;
  delete FLastErrorResponse;
  FLastErrorResponse = NULL;
  delete FLastError;
  FLastError = NULL;
  delete FFeatures;
  FFeatures = NULL;
  delete FServerCapabilities;
  FServerCapabilities = NULL;

  ResetCaches();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Open()
{
  // on reconnect, typically there may be pending status messages from previous session
  DiscardMessages();

  ResetCaches();
  FReadCurrentDirectory = true;
  FHomeDirectory = L"";
  FLoggedIn = false;

  FLastDataSent = Now();

  FMultiLineResponse = false;

  // initialize FZAPI on the first connect only
  if (FFileZillaIntf == NULL)
  {
    FFileZillaIntf = new TFileZillaImpl(this);

    try
    {
      TFileZillaIntf::TLogLevel LogLevel;
      switch (FTerminal->Configuration->ActualLogProtocol)
      {
        default:
        case -1:
          LogLevel = TFileZillaIntf::LOG_WARNING;
          break;

        case 0:
        case 1:
          LogLevel = TFileZillaIntf::LOG_PROGRESS;
          break;

        case 2:
          LogLevel = TFileZillaIntf::LOG_INFO;
          break;
      }
      FFileZillaIntf->SetDebugLevel(LogLevel);

      FFileZillaIntf->Init();
    }
    catch(...)
    {
      delete FFileZillaIntf;
      FFileZillaIntf = NULL;
      throw;
    }
  }

  TSessionData * Data = FTerminal->SessionData;

  FWindowsServer = false;
  FMVS = false;
  FVMS = false;
  FFileZilla = false;
  FIIS = false;
  FTransferActiveImmediately = (Data->FtpTransferActiveImmediately == asOn);
  FVMSAllRevisions = Data->VMSAllRevisions;

  FSessionInfo.LoginTime = Now();
  FSessionInfo.CertificateVerifiedManually = false;

  UnicodeString HostName = Data->HostNameExpanded;
  UnicodeString UserName = Data->UserNameExpanded;
  UnicodeString Password = Data->Password;
  UnicodeString Account = Data->FtpAccount;
  UnicodeString Path = Data->RemoteDirectory;
  int ServerType;
  if (Data->Ftps == ftpsNone)
  {
    ServerType = TFileZillaIntf::SERVER_FTP;
  }
  else
  {
    switch (Data->Ftps)
    {
      case ftpsImplicit:
        ServerType = TFileZillaIntf::SERVER_FTP_SSL_IMPLICIT;
        FSessionInfo.SecurityProtocolName = LoadStr(FTPS_IMPLICIT);
        break;

      case ftpsExplicitSsl:
        ServerType = TFileZillaIntf::SERVER_FTP_SSL_EXPLICIT;
        FSessionInfo.SecurityProtocolName = LoadStr(FTPS_EXPLICIT);
        break;

      case ftpsExplicitTls:
        ServerType = TFileZillaIntf::SERVER_FTP_TLS_EXPLICIT;
        FSessionInfo.SecurityProtocolName = LoadStr(FTPS_EXPLICIT);
        break;

      default:
        ServerType = int(); // shutup
        DebugFail();
        break;
    }

    RequireTls();
  }

  int Pasv = (Data->FtpPasvMode ? 1 : 2);

  int TimeZoneOffset = Data->TimeDifferenceAuto ? 0 : TimeToMinutes(Data->TimeDifference);

  int UTF8 = 0;
  switch (Data->NotUtf)
  {
    case asOn:
      UTF8 = 2;
      break;

    case asOff:
      UTF8 = 1;
      break;

    case asAuto:
      UTF8 = 0;
      break;
  }

  FPasswordFailed = false;
  FAnyPassword = !Password.IsEmpty();
  FStoredPasswordTried = false;
  bool PromptedForCredentials = false;

  do
  {
    FDetectTimeDifference = Data->TimeDifferenceAuto;
    FTimeDifference = 0;
    ResetFeatures();
    FSystem = EmptyStr;
    FServerID = EmptyStr;
    FWelcomeMessage = EmptyStr;
    FFileSystemInfoValid = false;

    // TODO: the same for account? it ever used?

    // ask for username if it was not specified in advance, even on retry,
    // but keep previous one as default,
    if (Data->UserNameExpanded.IsEmpty() && !FTerminal->SessionData->FingerprintScan)
    {
      FTerminal->LogEvent(L"Username prompt (no username provided)");

      if (!PromptedForCredentials)
      {
        FTerminal->Information(LoadStr(FTP_CREDENTIAL_PROMPT));
        PromptedForCredentials = true;
      }

      if (!FTerminal->PromptUser(Data, pkUserName, LoadStr(USERNAME_TITLE), L"",
            LoadStr(USERNAME_PROMPT2), true, 0, UserName))
      {
        FTerminal->FatalError(NULL, LoadStr(CREDENTIALS_NOT_SPECIFIED));
      }
      else
      {
        FUserName = UserName;
      }
    }

    // On retry ask for password.
    // This is particularly important, when stored password is no longer valid,
    // so we do not blindly try keep trying it in a loop (possibly causing account lockout)
    if (FPasswordFailed)
    {
      FTerminal->LogEvent(L"Password prompt (last login attempt failed)");

      Password = L"";
      if (!FTerminal->PromptUser(Data, pkPassword, LoadStr(PASSWORD_TITLE), L"",
            LoadStr(PASSWORD_PROMPT), false, 0, Password))
      {
        int Message = FAnyPassword ? AUTHENTICATION_FAILED : CREDENTIALS_NOT_SPECIFIED;
        FTerminal->FatalError(NULL, LoadStr(Message));
      }
      else if (!Password.IsEmpty())
      {
        FAnyPassword = true;
      }
    }

    if ((Data->Ftps != ftpsNone) && (FCertificate == NULL))
    {
      FTerminal->LoadTlsCertificate(FCertificate, FPrivateKey);
    }

    FPasswordFailed = false;

    FActive = FFileZillaIntf->Connect(
      HostName.c_str(), Data->PortNumber, UserName.c_str(),
      Password.c_str(), Account.c_str(), Path.c_str(),
      ServerType, Pasv, TimeZoneOffset, UTF8, Data->FtpForcePasvIp,
      Data->FtpUseMlsd, FCertificate, FPrivateKey);

    DebugAssert(FActive);

    try
    {
      // do not wait for FTP response code as Connect is complex operation
      GotReply(WaitForCommandReply(false), REPLY_CONNECT, LoadStr(CONNECTION_FAILED));

      Shred(Password);

      // we have passed, even if we got 530 on the way (if it is possible at all),
      // ignore it
      DebugAssert(!FPasswordFailed);
      FPasswordFailed = false;
    }
    catch(...)
    {
      if (FPasswordFailed)
      {
        FTerminal->Information(LoadStr(FTP_ACCESS_DENIED));
      }
      else
      {
        // see handling of REPLY_CONNECT in GotReply
        FTerminal->Closed();
        throw;
      }
    }
  }
  while (FPasswordFailed);

  ProcessFeatures();

  // see also TWebDAVFileSystem::CollectTLSSessionInfo()
  FSessionInfo.CSCipher = FFileZillaIntf->GetCipherName().c_str();
  FSessionInfo.SCCipher = FSessionInfo.CSCipher;
  UnicodeString TlsVersionStr = FFileZillaIntf->GetTlsVersionStr().c_str();
  AddToList(FSessionInfo.SecurityProtocolName, TlsVersionStr, L", ");
  FLoggedIn = true;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Close()
{
  DebugAssert(FActive);

  bool Result;
  bool Opening = (FTerminal->Status == ssOpening);
  if (FFileZillaIntf->Close(Opening))
  {
    DebugCheck(FLAGSET(WaitForCommandReply(false), TFileZillaIntf::REPLY_DISCONNECTED));
    Result = true;
  }
  else
  {
    // See TFileZillaIntf::Close
    Result = Opening;
  }

  if (DebugAlwaysTrue(Result))
  {
    DebugAssert(FActive);
    Disconnect();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::GetActive()
{
  return FActive;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CollectUsage()
{
  switch (FTerminal->SessionData->Ftps)
  {
    case ftpsNone:
      // noop
      break;

    case ftpsImplicit:
      FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPSImplicit");
      break;

    case ftpsExplicitSsl:
      FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPSExplicitSSL");
      break;

    case ftpsExplicitTls:
      FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPSExplicitTLS");
      break;

    default:
      DebugFail();
      break;
  }

  if (!FTerminal->SessionData->TlsCertificateFile.IsEmpty())
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPSCertificate");
  }

  if (FFileZillaIntf->UsingMlsd())
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPMLSD");
  }
  else
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPLIST");
  }

  if (FFileZillaIntf->UsingUtf8())
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPUTF8");
  }
  else
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPNonUTF8");
  }

  if (!CurrentDirectory.IsEmpty() && (CurrentDirectory[1] != L'/'))
  {
    if (IsUnixStyleWindowsPath(CurrentDirectory))
    {
      FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPWindowsPath");
    }
    else if ((CurrentDirectory.Length() >= 3) && IsLetter(CurrentDirectory[1]) && (CurrentDirectory[2] == L':') && (CurrentDirectory[3] == L'/'))
    {
      FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPRealWindowsPath");
    }
    else
    {
      FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPOtherPath");
    }
  }

  UnicodeString TlsVersionStr = FFileZillaIntf->GetTlsVersionStr().c_str();
  if (!TlsVersionStr.IsEmpty())
  {
    FTerminal->CollectTlsUsage(TlsVersionStr);
  }

  if (FFileZilla)
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPFileZilla");
  }
  // 220 ProFTPD 1.3.4a Server (Debian) [::ffff:192.168.179.137]
  // SYST
  // 215 UNIX Type: L8
  else if (ContainsText(FWelcomeMessage, L"ProFTPD"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPProFTPD");
  }
  // 220 Microsoft FTP Service
  // SYST
  // 215 Windows_NT
  else if (FIIS)
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPIIS");
  }
  // 220 (vsFTPd 3.0.2)
  // SYST
  // 215 UNIX Type: L8
  // (Welcome message is configurable)
  else if (ContainsText(FWelcomeMessage, L"vsFTPd"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPvsFTPd");
  }
  // 220 Welcome to Pure-FTPd.
  // ...
  // SYST
  // 215 UNIX Type: L8
  else if (ContainsText(FWelcomeMessage, L"Pure-FTPd"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPPureFTPd");
  }
  // 220 Titan FTP Server 10.47.1892 Ready.
  // ...
  // SYST
  // 215 UNIX Type: L8
  else if (ContainsText(FWelcomeMessage, L"Titan FTP Server"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPTitan");
  }
  // 220-Cerberus FTP Server - Home Edition
  // 220-This is the UNLICENSED Home Edition and may be used for home, personal use only
  // 220-Welcome to Cerberus FTP Server
  // 220 Created by Cerberus, LLC
  else if (ContainsText(FWelcomeMessage, L"Cerberus FTP Server"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPCerberus");
  }
  // 220 Serv-U FTP Server v15.1 ready...
  else if (ContainsText(FWelcomeMessage, L"Serv-U FTP Server"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPServU");
  }
  else if (ContainsText(FWelcomeMessage, L"WS_FTP"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPWSFTP");
  }
  // 220 Welcome to the most popular FTP hosting service! Save on hardware, software, hosting and admin. Share files/folders with read-write permission. Visit http://www.drivehq.com/ftp/
  // ...
  // SYST
  // 215 UNIX emulated by DriveHQ FTP Server.
  else if (ContainsText(FSystem, L"DriveHQ"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPDriveHQ");
  }
  // 220 GlobalSCAPE EFT Server (v. 6.0) * UNREGISTERED COPY *
  // ...
  // SYST
  // 215 UNIX Type: L8
  else if (ContainsText(FWelcomeMessage, L"GlobalSCAPE"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPGlobalScape");
  }
  // 220-<custom message>
  // 220 CompleteFTP v 8.1.3
  // ...
  // SYST
  // UNIX Type: L8
  else if (ContainsText(FWelcomeMessage, L"CompleteFTP"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPComplete");
  }
  // 220 Core FTP Server Version 1.2, build 567, 64-bit, installed 8 days ago Unregistered
  // ...
  // SYST
  // 215 UNIX Type: L8
  else if (ContainsText(FWelcomeMessage, L"Core FTP Server"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPCore");
  }
  // 220 Service ready for new user.
  // ..
  // SYST
  // 215 UNIX Type: Apache FtpServer
  // (e.g. brickftp.com)
  else if (ContainsText(FSystem, L"Apache FtpServer"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPApache");
  }
  // 220 pos1 FTP server (GNU inetutils 1.3b) ready.
  // ...
  // SYST
  // 215 UNIX Type: L8 Version: Linux 2.6.15.7-ELinOS-314pm3
  // Displaying "(GNU inetutils 1.3b)" in a welcome message can be turned off (-q switch):
  // 220 pos1 FTP server ready.
  // (the same for "Version: Linux 2.6.15.7-ELinOS-314pm3" in SYST response)
  else if (ContainsText(FWelcomeMessage, L"GNU inetutils"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPInetutils");
  }
  // 220 Syncplify.me Server! FTP(S) Service Ready
  // Message is configurable
  else if (ContainsText(FWelcomeMessage, L"Syncplify"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPSyncplify");
  }
  // 220-Idea FTP Server v0.80 (xxx.home.pl) [xxx.xxx.xxx.xxx]
  // 220 Ready
  // ...
  // SYST
  // UNIX Type: L8
  else if (ContainsText(FWelcomeMessage, L"Idea FTP Server"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPIdea");
  }
  // 220-FTPD1 IBM FTP CS V2R1 at name.test.com, 13:49:38 on 2016-01-28.
  // ...
  // SYST
  // 215 MVS is the operating system of this server. FTP Server is running on z/OS.
  else if (FMVS)
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPMVS");
  }
  // 220 xxx.xxx.xxx (xxx.xxx.xxx) FTP-OpenVMS FTPD V5.3-3 (c) 1998 Process Software Corporation
  // ...
  // SYST
  // 215 VMS system type. VMS V5.5-2.
  else if (FVMS)
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPVMS");
  }
  else
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPOther");
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DummyReadDirectory(const UnicodeString & Directory)
{
  std::unique_ptr<TRemoteDirectory> Files(new TRemoteDirectory(FTerminal));
  try
  {
    Files->Directory = Directory;
    DoReadDirectory(Files.get());
  }
  catch(...)
  {
    // ignore non-fatal errors
    // (i.e. current directory may not exist anymore)
    if (!FTerminal->Active)
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Idle()
{
  if (FActive && !FWaitingForReply)
  {
    PoolForFatalNonCommandReply();

    // Keep session alive
    if ((FTerminal->SessionData->FtpPingType == fptDirectoryListing) &&
        (double(Now() - FLastDataSent) > double(FTerminal->SessionData->FtpPingIntervalDT) * 4))
    {
      FTerminal->LogEvent(L"Dummy directory read to keep session alive.");
      FLastDataSent = Now(); // probably redundant to the same statement in DoReadDirectory

      DummyReadDirectory(CurrentDirectory);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Discard()
{
  // remove all pending messages, to get complete log
  // note that we need to retry discard on reconnect, as there still may be another
  // "disconnect/timeout/..." status messages coming
  DiscardMessages();
  DebugAssert(FActive);
  FActive = false;

  // See neon's ne_ssl_clicert_free
  if (FPrivateKey != NULL)
  {
    EVP_PKEY_free(FPrivateKey);
    FPrivateKey = NULL;
  }
  if (FCertificate != NULL)
  {
    X509_free(FCertificate);
    FCertificate = NULL;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::AbsolutePath(UnicodeString Path, bool /*Local*/)
{
  // TODO: improve (handle .. etc.)
  if (UnixIsAbsolutePath(Path))
  {
    return Path;
  }
  else
  {
    return ::AbsolutePath(FCurrentDirectory, Path);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::ActualCurrentDirectory()
{
  wchar_t CurrentPath[1024];
  FFileZillaIntf->GetCurrentPath(CurrentPath, LENOF(CurrentPath));
  return UnixExcludeTrailingBackslash(CurrentPath);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::EnsureLocation(const UnicodeString & Directory, bool Log)
{
  UnicodeString ADirectory = UnixExcludeTrailingBackslash(Directory);
  if (!UnixSamePath(ActualCurrentDirectory(), ADirectory))
  {
    if (Log)
    {
      FTerminal->LogEvent(FORMAT(L"Synchronizing current directory \"%s\".",
        (ADirectory)));
    }

    DoChangeDirectory(ADirectory);
    // make sure FZAPI is aware that we changed current working directory
    FFileZillaIntf->SetCurrentPath(ADirectory.c_str());
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::EnsureLocation()
{
  // if we do not know what's the current directory, do nothing
  if (!FCurrentDirectory.IsEmpty())
  {
    // Make sure that the FZAPI current working directory,
    // is actually our working directory.
    // It may not be because:
    // 1) We did cached directory change
    // 2) Listing was requested for non-current directory, which
    // makes FZAPI change its current directory (and not restoring it back afterwards)
    EnsureLocation(FCurrentDirectory, true);
  }
}
//---------------------------------------------------------------------------
bool TFTPFileSystem::EnsureLocationWhenWorkFromCwd(const UnicodeString & Directory)
{
  bool Result = (FWorkFromCwd == asOn);
  if (Result)
  {
    EnsureLocation(Directory, false);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::AnyCommand(const UnicodeString Command,
  TCaptureOutputEvent OutputEvent)
{
  // end-user has right to expect that client current directory is really
  // current directory for the server
  EnsureLocation();

  DebugAssert(FOnCaptureOutput == NULL);
  FOnCaptureOutput = OutputEvent;
  try
  {
    SendCommand(Command);

    GotReply(WaitForCommandReply(), REPLY_2XX_CODE | REPLY_3XX_CODE);
  }
  __finally
  {
    FOnCaptureOutput = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ResetCaches()
{
  delete FFileListCache;
  FFileListCache = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::AnnounceFileListOperation()
{
  ResetCaches();
}
//---------------------------------------------------------------------------
void TFTPFileSystem::SendCwd(const UnicodeString & Directory)
{
  UnicodeString Command = FORMAT(L"CWD %s", (Directory));
  SendCommand(Command);

  GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoChangeDirectory(const UnicodeString & Directory)
{
  if (FWorkFromCwd == asOn)
  {
    UnicodeString ADirectory = UnixIncludeTrailingBackslash(AbsolutePath(Directory, false));

    UnicodeString Actual = UnixIncludeTrailingBackslash(ActualCurrentDirectory());
    while (!UnixSamePath(Actual, ADirectory))
    {
      if (UnixIsChildPath(Actual, ADirectory))
      {
        UnicodeString SubDirectory = UnixExcludeTrailingBackslash(ADirectory);
        SubDirectory.Delete(1, Actual.Length());
        int P = SubDirectory.Pos(L'/');
        if (P > 0)
        {
          SubDirectory.SetLength(P - 1);
        }
        SendCwd(SubDirectory);
        Actual = UnixIncludeTrailingBackslash(Actual + SubDirectory);
      }
      else
      {
        SendCommand(L"CDUP");
        GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
        Actual = UnixExtractFilePath(UnixExcludeTrailingBackslash(Actual));
      }
    }
  }
  else
  {
    SendCwd(Directory);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ChangeDirectory(const UnicodeString ADirectory)
{
  UnicodeString Directory = ADirectory;
  try
  {
    // For changing directory, we do not make paths absolute, instead we
    // delegate this to the server, hence we synchronize current working
    // directory with the server and only then we ask for the change with
    // relative path.
    // But if synchronization fails, typically because current working directory
    // no longer exists, we fall back to out own resolution, to give
    // user chance to leave the non-existing directory.
    EnsureLocation();
  }
  catch(...)
  {
    if (FTerminal->Active)
    {
      Directory = AbsolutePath(Directory, false);
    }
    else
    {
      throw;
    }
  }

  DoChangeDirectory(Directory);

  // make next ReadCurrentDirectory retrieve actual server-side current directory
  FReadCurrentDirectory = true;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CachedChangeDirectory(const UnicodeString Directory)
{
  FCurrentDirectory = UnixExcludeTrailingBackslash(Directory);
  FReadCurrentDirectory = false;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ChangeFileProperties(const UnicodeString AFileName,
  const TRemoteFile * File, const TRemoteProperties * Properties,
  TChmodSessionAction & Action)
{
  DebugAssert(Properties);
  DebugAssert(!Properties->Valid.Contains(vpGroup));
  DebugAssert(!Properties->Valid.Contains(vpOwner));
  DebugAssert(!Properties->Valid.Contains(vpLastAccess));
  DebugAssert(!Properties->Valid.Contains(vpModification));

  if (Properties->Valid.Contains(vpRights))
  {
    TRemoteFile * OwnedFile = NULL;

    try
    {
      UnicodeString FileName = AbsolutePath(AFileName, false);

      if (File == NULL)
      {
        ReadFile(FileName, OwnedFile);
        File = OwnedFile;
      }

      if ((File != NULL) && File->IsDirectory && FTerminal->CanRecurseToDirectory(File) && Properties->Recursive)
      {
        try
        {
          FTerminal->ProcessDirectory(AFileName, FTerminal->ChangeFileProperties,
            (void*)Properties);
        }
        catch(...)
        {
          Action.Cancel();
          throw;
        }
      }

      TRights Rights;
      if (File != NULL)
      {
        Rights = *File->Rights;
      }
      Rights |= Properties->Rights.NumberSet;
      Rights &= (unsigned short)~Properties->Rights.NumberUnset;
      if ((File != NULL) && File->IsDirectory && Properties->AddXToDirectories)
      {
        Rights.AddExecute();
      }

      Action.Rights(Rights);

      UnicodeString FileNameOnly = UnixExtractFileName(FileName);
      UnicodeString FilePath = RemoteExtractFilePath(FileName);
      // FZAPI wants octal number represented as decadic
      FFileZillaIntf->Chmod(Rights.NumberDecadic, FileNameOnly.c_str(), FilePath.c_str());

      GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
    }
    __finally
    {
      delete OwnedFile;
    }
  }
  else
  {
    Action.Cancel();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::LoadFilesProperties(TStrings * /*FileList*/)
{
  DebugFail();
  return false;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::DoCalculateFileChecksum(const UnicodeString & Alg, TRemoteFile * File)
{
  // Overview of server supporting various hash commands is at:
  // https://datatracker.ietf.org/doc/html/draft-bryan-ftpext-hash-02#appendix-B

  UnicodeString CommandName;

  bool UsingHashCommand = UsingHashCommandChecksum(Alg);
  if (UsingHashCommand)
  {
    CommandName = HashCommand;
  }
  else
  {
    int Index = FChecksumAlgs->IndexOf(Alg);
    if (Index < 0)
    {
      DebugFail();
      EXCEPTION;
    }
    else
    {
      CommandName = FChecksumCommands->Strings[Index];
    }
  }

  UnicodeString FileName = File->FullFileName;
  // FTP way is not to quote.
  // But as Serv-U, GlobalSCAPE and possibly others allow
  // additional parameters (SP ER range), they need to quote file name.
  // Cerberus and FileZilla Server on the other hand can do without quotes
  // (but they can handle them, not sure about other servers)

  // Quoting:
  // FileZilla Server simply checks if argument starts and ends with double-quote
  // and strips them, no double-quote escaping is possible.
  // That's for all commands, not just HASH
  // ProFTPD: TODO: Check how "SITE SYMLINK target link" is parsed

  // We can possibly autodetect this from announced command format:
  // XCRC filename;start;end
  // XMD5 filename;start;end
  // XSHA1 filename;start;end
  // XSHA256 filename;start;end
  // XSHA512 filename;start;end
  if (FileName.Pos(L" ") > 0)
  {
    FileName = FORMAT(L"\"%s\"", (FileName));
  }

  UnicodeString Command = FORMAT(L"%s %s", (CommandName, FileName));
  SendCommand(Command);
  TStrings * Response;
  GotReply(WaitForCommandReply(), REPLY_2XX_CODE, EmptyStr, NULL, &Response);
  UnicodeString ResponseText;
  if (DebugAlwaysTrue(Response->Count > 0))
  {
    // ProFTPD response has this format:
    // 213-Computing MD5 digest
    // 213 MD5 0-687104 b359479cfda703b7fd473c7e09f39049 filename
    ResponseText = Response->Strings[Response->Count - 1];
  }
  delete Response;

  UnicodeString Hash;
  if (UsingHashCommand)
  {
    // Code should be 213, but let's be tolerant and accept any 2xx

    // ("213" SP) hashname SP start-point "-" end-point SP filehash SP <pathname> (CRLF)
    UnicodeString Buf = ResponseText;
    // skip alg
    CutToChar(Buf, L' ', true);
    // skip range
    UnicodeString Range = CutToChar(Buf, L' ', true);
    // This should be range (SP-EP), but if it does not conform to the format,
    // it's likely because the server uses version of the HASH spec
    // before draft-ietf-ftpext2-hash-01
    // (including draft-bryan-ftp-hash-06 implemented by FileZilla server; or Cerberus),
    // that did not have the "range" part.
    // The FileZilla Server even omits the file name.
    // The latest draft as of implementing this is draft-bryan-ftpext-hash-02.
    if (Range.Pos(L"-") > 0)
    {
      Hash = CutToChar(Buf, L' ', true);
    }
    else
    {
      Hash = Range;
    }
  }
  else // All hash-specific commands
  {
    // Accepting any 2xx response. Most servers use 213,
    // but for example WS_FTP uses non-sense code 220 (Service ready for new user)

    // MD5 response according to a draft-twine-ftpmd5-00 includes a file name
    // (implemented by Apache FtpServer).
    // Other commands (X<hash>) return the hash only.
    ResponseText = ResponseText.Trim();
    int P = ResponseText.LastDelimiter(L" ");
    if (P > 0)
    {
      ResponseText.Delete(1, P);
    }

    Hash = ResponseText;
  }

  if (Hash.IsEmpty())
  {
    throw Exception(FMTLOAD(FTP_RESPONSE_ERROR, (CommandName, ResponseText)));
  }

  return LowerCase(Hash);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CalculateFilesChecksum(
  const UnicodeString & Alg, TStrings * FileList, TCalculatedChecksumEvent OnCalculatedChecksum,
  TFileOperationProgressType * OperationProgress, bool FirstLevel)
{
  FTerminal->CalculateSubFoldersChecksum(Alg, FileList, OnCalculatedChecksum, OperationProgress, FirstLevel);

  int Index = 0;
  TOnceDoneOperation OnceDoneOperation; // not used
  while ((Index < FileList->Count) && !OperationProgress->Cancel)
  {
    TRemoteFile * File = (TRemoteFile *)FileList->Objects[Index];
    DebugAssert(File != NULL);

    if (!File->IsDirectory)
    {
      TChecksumSessionAction Action(FTerminal->ActionLog);
      try
      {
        OperationProgress->SetFile(File->FileName);
        Action.FileName(File->FullFileName);
        bool Success = false;
        try
        {
          UnicodeString Checksum = DoCalculateFileChecksum(Alg, File);

          if (OnCalculatedChecksum != NULL)
          {
            OnCalculatedChecksum(File->FileName, Alg, Checksum);
          }
          Action.Checksum(Alg, Checksum);
          Success = true;
        }
        __finally
        {
          if (FirstLevel)
          {
            OperationProgress->Finish(File->FileName, Success, OnceDoneOperation);
          }
        }
      }
      catch (Exception & E)
      {
        FTerminal->RollbackAction(Action, OperationProgress, &E);

        // Error formatting expanded from inline to avoid strange exceptions
        UnicodeString Error = FMTLOAD(CHECKSUM_ERROR, (File->FullFileName));
        FTerminal->CommandError(&E, Error);
        // Abort loop.
        // TODO: retries? resume?
        Index = FileList->Count;
      }
    }
    Index++;
  }
}
//---------------------------------------------------------------------------
UnicodeString TFTPFileSystem::CalculateFilesChecksumInitialize(const UnicodeString & Alg)
{
  UnicodeString NormalizedAlg = FindIdent(FindIdent(Alg, FHashAlgs.get()), FChecksumAlgs.get());
  if (UsingHashCommandChecksum(NormalizedAlg))
  {
    // The server should understand lowercase alg name by spec,
    // but we should use uppercase anyway
    SendCommand(FORMAT(L"OPTS %s %s", (HashCommand, UpperCase(NormalizedAlg))));
    GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
  }
  else if (FChecksumAlgs->IndexOf(NormalizedAlg) >= 0)
  {
    // will use algorithm-specific command
  }
  else
  {
    throw Exception(FMTLOAD(UNKNOWN_CHECKSUM, (Alg)));
  }
  return NormalizedAlg;
}
//---------------------------------------------------------------------------
bool TFTPFileSystem::UsingHashCommandChecksum(const UnicodeString & Alg)
{
  return (FHashAlgs->IndexOf(Alg) >= 0);
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::ConfirmOverwrite(
  const UnicodeString & SourceFullFileName, UnicodeString & TargetFileName,
  TOverwriteMode & OverwriteMode, TFileOperationProgressType * OperationProgress,
  const TOverwriteFileParams * FileParams, const TCopyParamType * CopyParam,
  int Params, bool AutoResume)
{
  bool Result;
  bool CanAutoResume = FLAGSET(Params, cpNoConfirmation) && AutoResume;
  bool DestIsSmaller = (FileParams != NULL) && (FileParams->DestSize < FileParams->SourceSize);
  bool DestIsSame = (FileParams != NULL) && (FileParams->DestSize == FileParams->SourceSize);
  bool CanResume =
    !OperationProgress->AsciiTransfer &&
    // when resuming transfer after interrupted connection,
    // do nothing (dummy resume) when the files have the same size.
    // this is workaround for servers that strangely fails just after successful
    // upload.
    (DestIsSmaller || (DestIsSame && CanAutoResume));

  unsigned int Answer;
  if (CanAutoResume && CanResume)
  {
    if (DestIsSame)
    {
      DebugAssert(CanAutoResume);
      Answer = qaSkip;
    }
    else
    {
      Answer = qaRetry;
    }
  }
  else
  {
    // retry = "resume"
    // all = "yes to newer"
    // ignore = "rename"
    int Answers = qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll | qaAll | qaIgnore;
    if (CanResume)
    {
      Answers |= qaRetry;
    }
    TQueryButtonAlias Aliases[5];
    Aliases[0].Button = qaRetry;
    Aliases[0].Alias = LoadStr(RESUME_BUTTON);
    Aliases[0].GroupWith = qaNo;
    Aliases[0].GrouppedShiftState = TShiftState() << ssAlt;
    Aliases[1] = TQueryButtonAlias::CreateAllAsYesToNewerGrouppedWithYes();
    Aliases[2] = TQueryButtonAlias::CreateIgnoreAsRenameGrouppedWithNo();
    Aliases[3] = TQueryButtonAlias::CreateYesToAllGrouppedWithYes();
    Aliases[4] = TQueryButtonAlias::CreateNoToAllGrouppedWithNo();
    TQueryParams QueryParams(qpNeverAskAgainCheck);
    QueryParams.Aliases = Aliases;
    QueryParams.AliasesCount = LENOF(Aliases);

    {
      TSuspendFileOperationProgress Suspend(OperationProgress);
      Answer = FTerminal->ConfirmFileOverwrite(
        SourceFullFileName, TargetFileName, FileParams, Answers, &QueryParams,
        ReverseOperationSide(OperationProgress->Side),
        CopyParam, Params, OperationProgress);
    }
  }

  Result = true;

  switch (Answer)
  {
    // resume
    case qaRetry:
      OverwriteMode = omResume;
      DebugAssert(FileParams != NULL);
      DebugAssert(CanResume);
      FFileTransferResumed = FileParams->DestSize;
      break;

    // rename
    case qaIgnore:
      if (FTerminal->PromptUser(FTerminal->SessionData, pkFileName,
            LoadStr(RENAME_TITLE), L"", LoadStr(RENAME_PROMPT2), true, 0, TargetFileName))
      {
        OverwriteMode = omOverwrite;
      }
      else
      {
        OperationProgress->SetCancelAtLeast(csCancel);
        FFileTransferAbort = ftaCancel;
        Result = false;
      }
      break;

    case qaYes:
      OverwriteMode = omOverwrite;
      break;

    case qaCancel:
      OperationProgress->SetCancelAtLeast(csCancel);
      FFileTransferAbort = ftaCancel;
      Result = false;
      break;

    case qaNo:
      FFileTransferAbort = ftaSkip;
      Result = false;
      break;

    case qaSkip:
      OverwriteMode = omComplete;
      break;

    default:
      DebugFail();
      Result = false;
      break;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ResetFileTransfer()
{
  FFileTransferAbort = ftaNone;
  FFileTransferCancelled = false;
  FFileTransferResumed = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadDirectoryProgress(__int64 Bytes)
{
  // with FTP we do not know exactly how many entries we have received,
  // instead we know number of bytes received only.
  // so we report approximation based on average size of entry.
  int Progress = int(Bytes / 80);
  DWORD Ticks = GetTickCount();
  if ((Ticks - FLastReadDirectoryProgress >= 100) &&
      // Cannot call OnReadDirectoryProgress with 0 as it would unmatch the "starting" and "ending" signals for disabling the window
      (Progress > 0))
  {
    FLastReadDirectoryProgress = Ticks;
    bool Cancel = false;
    FTerminal->DoReadDirectoryProgress(Progress, 0, Cancel);
    if (Cancel)
    {
      FTerminal->DoReadDirectoryProgress(-2, 0, Cancel);
      FFileZillaIntf->Cancel();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoFileTransferProgress(__int64 TransferSize,
  __int64 Bytes)
{
  TFileOperationProgressType * OperationProgress = FTerminal->OperationProgress;

  OperationProgress->SetTransferSize(TransferSize);

  if (FFileTransferResumed > 0)
  {
    // Bytes will be 0, if resume was not possible
    if (Bytes >= FFileTransferResumed)
    {
      OperationProgress->AddResumed(FFileTransferResumed);
    }
    FFileTransferResumed = 0;
  }

  __int64 Diff = Bytes - OperationProgress->TransferredSize;
  if (DebugAlwaysTrue(Diff >= 0))
  {
    OperationProgress->AddTransferred(Diff);
    FFileTransferAny = true;
  }
  if (OperationProgress->Cancel != csContinue)
  {
    if (OperationProgress->ClearCancelFile())
    {
      FFileTransferAbort = ftaSkip;
    }
    else
    {
      FFileTransferAbort = ftaCancel;
    }
    FFileTransferCancelled = true;
    FFileZillaIntf->Cancel();
  }

  if (FFileTransferCPSLimit != OperationProgress->CPSLimit)
  {
    SetCPSLimit(OperationProgress);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::SetCPSLimit(TFileOperationProgressType * OperationProgress)
{
  // Any reason we use separate field instead of directly using OperationProgress->CPSLimit?
  // Maybe thread-safety?
  FFileTransferCPSLimit = OperationProgress->CPSLimit;
  OperationProgress->SetSpeedCounters();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::FileTransferProgress(__int64 TransferSize,
  __int64 Bytes)
{
  TGuard Guard(FTransferStatusCriticalSection);

  DoFileTransferProgress(TransferSize, Bytes);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::FileTransfer(const UnicodeString & FileName,
  const UnicodeString & LocalFile, const UnicodeString & RemoteFile,
  const UnicodeString & RemotePath, bool Get, __int64 Size, int Type,
  TFileTransferData & UserData, TFileOperationProgressType * OperationProgress)
{
  FILE_OPERATION_LOOP_BEGIN
  {
    FFileZillaIntf->FileTransfer(
      ApiPath(LocalFile).c_str(), RemoteFile.c_str(), RemotePath.c_str(),
      Get, Size, Type, &UserData, UserData.CopyParam->OnTransferOut, UserData.CopyParam->OnTransferIn);
    // we may actually catch response code of the listing
    // command (when checking for existence of the remote file)
    unsigned int Reply = WaitForCommandReply();
    GotReply(Reply, FLAGMASK(FFileTransferCancelled, REPLY_ALLOW_CANCEL));
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(TRANSFER_ERROR, (FileName)));

  switch (FFileTransferAbort)
  {
    case ftaSkip:
      throw ESkipFile();

    case ftaCancel:
      Abort();
      break;
  }

  if (!FFileTransferCancelled)
  {
    // show completion of transfer
    // call non-guarded variant to avoid deadlock with keepalives
    // (we are not waiting for reply anymore so keepalives are free to proceed)
    DoFileTransferProgress(OperationProgress->TransferSize, OperationProgress->TransferSize);
    FAnyTransferSucceeded = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CopyToLocal(TStrings * FilesToCopy,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  Params &= ~cpAppend;

  FTerminal->DoCopyToLocal(
    FilesToCopy, TargetDir, CopyParam, Params, OperationProgress, tfUseFileTransferAny, OnceDoneOperation);
}
//---------------------------------------------------------------------------
UnicodeString TFTPFileSystem::RemoteExtractFilePath(const UnicodeString & Path)
{
  UnicodeString Result;
  // If the path ends with a slash, FZAPI CServerPath constructor does not identify the path as VMS.
  // It is probably ok to use UnixExtractFileDir for all paths passed to FZAPI,
  // but for now, we limit the impact of the change to VMS.
  if (FVMS)
  {
    Result = UnixExtractFileDir(Path);
  }
  else
  {
    Result = UnixExtractFilePath(Path);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Sink(
  const UnicodeString & FileName, const TRemoteFile * File,
  const UnicodeString & TargetDir, UnicodeString & DestFileName, int Attrs,
  const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress,
  unsigned int Flags, TDownloadSessionAction & Action)
{
  AutoDetectTimeDifference(UnixExtractFileDir(FileName), CopyParam, Params);

  ResetFileTransfer();

  TFileTransferData UserData;

  UnicodeString DestFullName = TargetDir + DestFileName;
  UnicodeString FilePath = RemoteExtractFilePath(FileName);
  unsigned int TransferType = (OperationProgress->AsciiTransfer ? 1 : 2);

  UnicodeString AFileName;
  UnicodeString OnlyFileName = UnixExtractFileName(FileName);
  if (EnsureLocationWhenWorkFromCwd(FilePath))
  {
    AFileName = OnlyFileName;
    FilePath = EmptyStr;
  }
  else
  {
    AFileName = FileName;
  }

  {
    // ignore file list
    TFileListHelper Helper(this, NULL, true);

    SetCPSLimit(OperationProgress);
    FFileTransferPreserveTime = CopyParam->PreserveTime;
    // not used for downloads anyway
    FFileTransferRemoveBOM = CopyParam->RemoveBOM;
    FFileTransferNoList = CanTransferSkipList(Params, Flags, CopyParam);
    UserData.FileName = DestFileName;
    UserData.Params = Params;
    UserData.AutoResume = FLAGSET(Flags, tfAutoResume);
    UserData.CopyParam = CopyParam;
    UserData.Modification = File->Modification;
    FileTransfer(AFileName, DestFullName, OnlyFileName,
      FilePath, true, File->Size, TransferType, UserData, OperationProgress);
  }

  // in case dest filename is changed from overwrite dialog
  if (DestFileName != UserData.FileName)
  {
    DestFullName = TargetDir + UserData.FileName;
    Attrs = FileGetAttrFix(ApiPath(DestFullName));
  }

  UnicodeString ExpandedDestFullName = ExpandUNCFileName(DestFullName);
  Action.Destination(ExpandedDestFullName);

  if (CopyParam->OnTransferOut == NULL)
  {
    FTerminal->UpdateTargetAttrs(DestFullName, File, CopyParam, Attrs);
  }

  FLastDataSent = Now();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::TransferOnDirectory(
  const UnicodeString & Directory, const TCopyParamType * CopyParam, int Params)
{
  AutoDetectTimeDifference(Directory, CopyParam, Params);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CopyToRemote(TStrings * FilesToCopy,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  Params &= ~cpAppend;

  FTerminal->DoCopyToRemote(FilesToCopy, TargetDir, CopyParam, Params, OperationProgress, tfUseFileTransferAny, OnceDoneOperation);
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::CanTransferSkipList(int Params, unsigned int Flags, const TCopyParamType * CopyParam)
{
  bool Result =
    (CopyParam->OnTransferIn != NULL) ||
    (FLAGSET(Params, cpNoConfirmation) &&
     // cpAppend is not supported with FTP
     DebugAlwaysTrue(FLAGCLEAR(Params, cpAppend)) &&
     FLAGCLEAR(Params, cpResume) &&
     FLAGCLEAR(Flags, tfAutoResume) &&
     !CopyParam->NewerOnly);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Source(
  TLocalFileHandle & Handle, const UnicodeString & TargetDir, UnicodeString & DestFileName,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags,
  TUploadSessionAction & Action, bool & /*ChildError*/)
{
  if (CopyParam->OnTransferIn == NULL)
  {
    Handle.Close();
  }

  ResetFileTransfer();

  TFileTransferData UserData;

  unsigned int TransferType = (OperationProgress->AsciiTransfer ? 1 : 2);

  EnsureLocationWhenWorkFromCwd(TargetDir);

  {
    // ignore file list
    TFileListHelper Helper(this, NULL, true);

    SetCPSLimit(OperationProgress);
    // not used for uploads anyway
    FFileTransferPreserveTime = CopyParam->PreserveTime && (CopyParam->OnTransferIn == NULL);
    FFileTransferRemoveBOM = CopyParam->RemoveBOM;
    FFileTransferNoList = CanTransferSkipList(Params, Flags, CopyParam);
    // not used for uploads, but we get new name (if any) back in this field
    UserData.FileName = DestFileName;
    UserData.Params = Params;
    UserData.AutoResume = FLAGSET(Flags, tfAutoResume);
    UserData.CopyParam = CopyParam;
    UserData.Modification = Handle.Modification;
    FileTransfer(Handle.FileName, Handle.FileName, DestFileName,
      TargetDir, false, Handle.Size, TransferType, UserData, OperationProgress);
  }

  UnicodeString DestFullName = TargetDir + UserData.FileName;
  // only now, we know the final destination
  Action.Destination(DestFullName);

  // We are not able to tell if setting timestamp succeeded,
  // so we log it always (if supported).
  // Support for MDTM does not necessarily mean that the server supports
  // non-standard hack of setting timestamp using
  // MFMT-like (two argument) call to MDTM.
  // IIS definitely does.
  if (FFileTransferPreserveTime &&
      ((FServerCapabilities->GetCapability(mfmt_command) == yes) ||
       ((FServerCapabilities->GetCapability(mdtm_command) == yes))))
  {
    TTouchSessionAction TouchAction(FTerminal->ActionLog, DestFullName, Handle.Modification);

    if (!FFileZillaIntf->UsingMlsd())
    {
      FUploadedTimes[DestFullName] = Handle.Modification;
      if ((FTerminal->Configuration->ActualLogProtocol >= 2))
      {
        FTerminal->LogEvent(
          FORMAT(L"Remembering modification time of \"%s\" as [%s]",
                 (DestFullName, StandardTimestamp(FUploadedTimes[DestFullName]))));
      }
    }
  }

  FLastDataSent = Now();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CreateDirectory(const UnicodeString & ADirName, bool /*Encrypt*/)
{
  UnicodeString DirName = AbsolutePath(ADirName, false);

  {
    // ignore file list
    TFileListHelper Helper(this, NULL, true);

    FFileZillaIntf->MakeDir(DirName.c_str());

    GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CreateLink(const UnicodeString FileName,
  const UnicodeString PointTo, bool Symbolic)
{
  DebugAssert(SupportsSiteCommand(SymlinkSiteCommand));
  if (DebugAlwaysTrue(Symbolic))
  {
    EnsureLocation();

    UnicodeString Command = FORMAT(L"%s %s %s %s", (SiteCommand, SymlinkSiteCommand, PointTo, FileName));
    SendCommand(Command);
    GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DeleteFile(const UnicodeString AFileName,
  const TRemoteFile * File, int Params, TRmSessionAction & Action)
{
  UnicodeString FileName = AbsolutePath(AFileName, false);
  UnicodeString FileNameOnly = UnixExtractFileName(FileName);
  UnicodeString FilePath = RemoteExtractFilePath(FileName);

  bool Dir = FTerminal->DeleteContentsIfDirectory(FileName, File, Params, Action);

  {
    // ignore file list
    TFileListHelper Helper(this, NULL, true);

    if (Dir)
    {
      // If current remote directory is in the directory being removed,
      // some servers may refuse to delete it
      // This is common as ProcessDirectory above would CWD to
      // the directory to LIST it.
      // EnsureLocation should reset actual current directory to user's working directory.
      // If user's working directory is still below deleted directory, it is
      // perfectly correct to report an error.
      if (UnixIsChildPath(ActualCurrentDirectory(), FileName))
      {
        EnsureLocation();
      }
      FFileZillaIntf->RemoveDir(FileNameOnly.c_str(), FilePath.c_str());
    }
    else
    {
      if (EnsureLocationWhenWorkFromCwd(FilePath))
      {
        FFileZillaIntf->Delete(FileNameOnly.c_str(), L"", true);
      }
      else
      {
        FFileZillaIntf->Delete(FileNameOnly.c_str(), FilePath.c_str(), false);
      }
    }
    GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CustomCommandOnFile(const UnicodeString /*FileName*/,
  const TRemoteFile * /*File*/, UnicodeString /*Command*/, int /*Params*/,
  TCaptureOutputEvent /*OutputEvent*/)
{
  // if ever implemented, do not forget to add EnsureLocation,
  // see AnyCommand for a reason why
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoStartup()
{
  TStrings * PostLoginCommands = new TStringList();
  try
  {
    PostLoginCommands->Text = FTerminal->SessionData->PostLoginCommands;
    for (int Index = 0; Index < PostLoginCommands->Count; Index++)
    {
      UnicodeString Command = PostLoginCommands->Strings[Index];
      if (!Command.IsEmpty())
      {
        SendCommand(Command);

        GotReply(WaitForCommandReply(), REPLY_2XX_CODE | REPLY_3XX_CODE);
      }
    }
  }
  __finally
  {
    delete PostLoginCommands;
  }

  if (SupportsCommand(CsidCommand))
  {
    UnicodeString NameFact = L"Name";
    UnicodeString VersionFact = L"Version";
    UnicodeString Command =
      FORMAT(L"%s %s=%s;%s=%s;", (CsidCommand, NameFact, AppNameString(), VersionFact, FTerminal->Configuration->Version));
    SendCommand(Command);
    TStrings * Response = NULL;
    std::unique_ptr<TStrings> ResponseOwner(Response);
    try
    {
      GotReply(WaitForCommandReply(), REPLY_2XX_CODE | REPLY_SINGLE_LINE, EmptyStr, NULL, &Response);
      ResponseOwner.reset(Response);
    }
    catch (...)
    {
      if (FTerminal->Active)
      {
        FTerminal->LogEvent(FORMAT(L"%s command failed", (CsidCommand)));
      }
      else
      {
        throw;
      }
    }
    if (ResponseOwner.get() != NULL)
    {
      UnicodeString ResponseText = Response->Strings[0];
      UnicodeString Name, Version;
      while (!ResponseText.IsEmpty())
      {
        UnicodeString Token = CutToChar(ResponseText, L';', true);
        UnicodeString Fact = CutToChar(Token, L'=', true);
        if (SameText(Fact, NameFact))
        {
          Name = Token;
        }
        else if (SameText(Fact, VersionFact))
        {
          Version = Token;
        }
      }

      if (!Name.IsEmpty())
      {
        FServerID = Name;
        AddToList(FServerID, Version, L" ");
        FTerminal->LogEvent(FORMAT("Server: %s", (FServerID)));
      }
    }
  }

  // retrieve initialize working directory to save it as home directory
  ReadCurrentDirectory();
  FHomeDirectory = FCurrentDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::HomeDirectory()
{
  // FHomeDirectory is an absolute path, so avoid unnecessary overhead
  // of ChangeDirectory, such as EnsureLocation
  DoChangeDirectory(FHomeDirectory);
  FCurrentDirectory = FHomeDirectory;
  FReadCurrentDirectory = false;
  // make sure FZAPI is aware that we changed current working directory
  FFileZillaIntf->SetCurrentPath(FCurrentDirectory.c_str());
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::IsCapable(int Capability) const
{
  DebugAssert(FTerminal);
  switch (Capability)
  {
    case fcResolveSymlink: // sic
    case fcTextMode:
    case fcModeChanging: // but not fcModeChangingUpload
    case fcNewerOnlyUpload:
    case fcAnyCommand: // but not fcShellAnyCommand
    case fcRename:
    case fcRemoteMove:
    case fcRemoveBOMUpload:
    case fcMoveToQueue:
    case fcSkipTransfer:
    case fcParallelTransfers:
    case fcTransferOut:
    case fcTransferIn:
      return true;

    case fcPreservingTimestampUpload:
      return (FServerCapabilities->GetCapability(mfmt_command) == yes);

    case fcRemoteCopy:
      return SupportsSiteCommand(CopySiteCommand);

    case fcSymbolicLink:
      return SupportsSiteCommand(SymlinkSiteCommand);

    case fcCalculatingChecksum:
      return FSupportsAnyChecksumFeature;

    case fcCheckingSpaceAvailable:
      return FBytesAvailableSupported || SupportsCommand(AvblCommand) || SupportsCommand(XQuotaCommand);

    case fcMoveOverExistingFile:
      return !FIIS;

    case fcAclChangingFiles:
    case fcModeChangingUpload:
    case fcLoadingAdditionalProperties:
    case fcShellAnyCommand:
    case fcHardLink:
    case fcUserGroupListing:
    case fcGroupChanging:
    case fcOwnerChanging:
    case fcGroupOwnerChangingByID:
    case fcSecondaryShell:
    case fcNativeTextMode:
    case fcTimestampChanging:
    case fcIgnorePermErrors:
    case fcRemoveCtrlZUpload:
    case fcLocking:
    case fcPreservingTimestampDirs:
    case fcResumeSupport:
    case fcChangePassword:
    case fcParallelFileTransfers:
    case fcTags:
      return false;

    default:
      DebugFail();
      return false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::LookupUsersGroups()
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadCurrentDirectory()
{
  // ask the server for current directory on startup only
  // and immediately after call to CWD,
  // later our current directory may be not synchronized with FZAPI current
  // directory anyway, see comments in EnsureLocation
  if (FReadCurrentDirectory || DebugAlwaysFalse(FCurrentDirectory.IsEmpty()))
  {
    UnicodeString Command = L"PWD";
    SendCommand(Command);

    unsigned int Code;
    TStrings * Response = NULL;
    GotReply(WaitForCommandReply(), REPLY_2XX_CODE, L"", &Code, &Response);

    try
    {
      DebugAssert(Response != NULL);
      bool Result = false;

      // The 257 is the only allowed 2XX code to "PWD"
      if (((Code == 257) || FTerminal->SessionData->FtpAnyCodeForPwd) &&
          (Response->Count == 1))
      {
        UnicodeString Path = Response->Text;

        int P = Path.Pos(L"\"");
        if (P == 0)
        {
          // some systems use single quotes, be tolerant
          P = Path.Pos(L"'");
        }
        if (P != 0)
        {
          Path.Delete(1, P - 1);

          if (Unquote(Path))
          {
            Result = true;
          }
        }
        else
        {
          P = Path.Pos(L" ");
          Path.Delete(P, Path.Length() - P + 1);
          Result = true;
        }

        if (Result)
        {
          if (Path.IsEmpty() || !UnixIsAbsolutePath(Path))
          {
            Path = L"/" + Path;
          }
          FCurrentDirectory = UnixExcludeTrailingBackslash(Path);
          FReadCurrentDirectory = false;
        }
      }

      if (Result)
      {
        FFileZillaIntf->SetCurrentPath(FCurrentDirectory.c_str());
      }
      else
      {
        throw Exception(FMTLOAD(FTP_RESPONSE_ERROR, (Command, Response->Text)));
      }
    }
    __finally
    {
      delete Response;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoReadDirectory(TRemoteFileList * FileList)
{
  UnicodeString Directory;
  if (!EnsureLocationWhenWorkFromCwd(FileList->Directory))
  {
    Directory = AbsolutePath(FileList->Directory, false);
  }

  FBytesAvailable = -1;
  FileList->Reset();
  // FZAPI does not list parent directory, add it
  FileList->AddFile(new TRemoteParentDirectory(FTerminal));

  FLastReadDirectoryProgress = 0;

  TFileListHelper Helper(this, FileList, false);

  // always specify path to list, do not attempt to list "current" dir as:
  // 1) List() lists again the last listed directory, not the current working directory
  // 2) we handle this way the cached directory change
  FFileZillaIntf->List(Directory.c_str());

  GotReply(WaitForCommandReply(), REPLY_2XX_CODE | REPLY_ALLOW_CANCEL);

  AutoDetectTimeDifference(FileList);

  if (!IsEmptyFileList(FileList))
  {
    CheckTimeDifference();

    if ((FTimeDifference != 0) || !FUploadedTimes.empty())// optimization
    {
      for (int Index = 0; Index < FileList->Count; Index++)
      {
        ApplyTimeDifference(FileList->Files[Index]);
      }
    }
  }

  FLastDataSent = Now();
  FAnyTransferSucceeded = true;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CheckTimeDifference()
{
  if (NeedAutoDetectTimeDifference())
  {
    FTerminal->LogEvent("Warning: Timezone difference was not detected yet, timestamps may be incorrect");
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ApplyTimeDifference(TRemoteFile * File)
{
  DebugAssert(File->Modification == File->LastAccess);
  File->ShiftTimeInSeconds(FTimeDifference);

  TDateTime Modification = File->Modification;
  if (LookupUploadModificationTime(File->FullFileName, Modification, File->ModificationFmt))
  {
    // implicitly sets ModificationFmt to mfFull
    File->Modification = Modification;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ApplyTimeDifference(
  const UnicodeString & FileName, TDateTime & Modification, TModificationFmt & ModificationFmt)
{
  CheckTimeDifference();
  TRemoteFile::ShiftTimeInSeconds(Modification, ModificationFmt, FTimeDifference);

  if (LookupUploadModificationTime(FileName, Modification, ModificationFmt))
  {
    ModificationFmt = mfFull;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::LookupUploadModificationTime(
  const UnicodeString & FileName, TDateTime & Modification, TModificationFmt ModificationFmt)
{
  bool Result = false;
  if (ModificationFmt != mfFull)
  {
    TUploadedTimes::iterator Iterator = FUploadedTimes.find(AbsolutePath(FileName, false));
    if (Iterator != FUploadedTimes.end())
    {
      TDateTime UploadModification = Iterator->second;
      TDateTime UploadModificationReduced = ReduceDateTimePrecision(UploadModification, ModificationFmt);
      if (UploadModificationReduced == Modification)
      {
        if ((FTerminal->Configuration->ActualLogProtocol >= 2))
        {
          FTerminal->LogEvent(
            FORMAT(L"Enriching modification time of \"%s\" from [%s] to [%s]",
                   (FileName, StandardTimestamp(Modification), StandardTimestamp(UploadModification))));
        }
        Modification = UploadModification;
        Result = true;
      }
      else
      {
        if ((FTerminal->Configuration->ActualLogProtocol >= 2))
        {
          FTerminal->LogEvent(
            FORMAT(L"Remembered modification time [%s]/[%s] of \"%s\" is obsolete, keeping [%s]",
                   (StandardTimestamp(UploadModification), StandardTimestamp(UploadModificationReduced), FileName, StandardTimestamp(Modification))));
        }
        FUploadedTimes.erase(Iterator);
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::NeedAutoDetectTimeDifference()
{
  return
    FDetectTimeDifference &&
    // Does not support MLST/MLSD, but supports MDTM at least
    !FFileZillaIntf->UsingMlsd() && SupportsReadingFile();
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::IsEmptyFileList(TRemoteFileList * FileList)
{
  return
    // (note that it's actually never empty here, there's always at least parent directory,
    // added explicitly by DoReadDirectory)
    (FileList->Count == 0) ||
    ((FileList->Count == 1) && FileList->Files[0]->IsParentDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::AutoDetectTimeDifference(TRemoteFileList * FileList)
{
  if (NeedAutoDetectTimeDifference())
  {
    FTerminal->LogEvent(L"Detecting timezone difference...");

    for (int Index = 0; Index < FileList->Count; Index++)
    {
      TRemoteFile * File = FileList->Files[Index];
      // For directories, we do not do MDTM in ReadFile
      // (it should not be problem to use them otherwise).
      // We are also not interested in files with day precision only.
      if (!File->IsDirectory && !File->IsSymLink &&
          File->IsTimeShiftingApplicable())
      {
        TRemoteFile * UtcFile = NULL;
        try
        {
          ReadFile(File->FullFileName, UtcFile);
        }
        catch (Exception &)
        {
          FDetectTimeDifference = false;
          if (!FTerminal->Active)
          {
            throw;
          }
          FTerminal->LogEvent(FORMAT(L"Failed to retrieve file %s attributes to detect timezone difference", (File->FullFileName)));
          break;
        }

        TDateTime UtcModification = UtcFile->Modification;
        delete UtcFile;

        if (UtcModification > Now())
        {
          FTerminal->LogEvent(
            FORMAT(L"Not using file %s to detect timezone difference as it has the timestamp in the future [%s]",
              (File->FullFileName, StandardTimestamp(UtcModification))));
        }
        // "SecureLink FTP Proxy" succeeds CWD for a file, so we never get a timestamp here
        else if (UtcModification == TDateTime())
        {
          FTerminal->LogEvent(
            FORMAT(L"Not using file %s to detect timezone difference as its timestamp was not resolved",
              (File->FullFileName)));
        }
        else
        {
          FDetectTimeDifference = false;

          // MDTM returns seconds, trim those
          UtcModification = ReduceDateTimePrecision(UtcModification, File->ModificationFmt);

          // Time difference between timestamp retrieved using MDTM (UTC converted to local timezone)
          // and using LIST (no conversion, expecting the server uses the same timezone as the client).
          // Note that FormatTimeZone reverses the value.
          FTimeDifference = static_cast<__int64>(SecsPerDay * static_cast<Variant>(UtcModification - File->Modification));
          double Hours = TTimeSpan::FromSeconds(FTimeDifference).TotalHours;

          UnicodeString FileLog =
            FORMAT(L"%s (Listing: %s, UTC: %s)", (File->FullFileName, StandardTimestamp(File->Modification), StandardTimestamp(UtcModification)));
          UnicodeString LogMessage;
          if (FTimeDifference == 0)
          {
            LogMessage = FORMAT(L"No timezone difference detected using file %s", (FileLog));
          }
          // Seen with "GamingDeluxe FTP Server", which returns "213 00010101000000"
          else if (fabs(Hours) >= 48)
          {
            FTimeDifference = 0;
            LogMessage = FORMAT(L"Ignoring suspicious timezone difference of %s hours, detected using file %s", (IntToStr(__int64(Hours)), FileLog));
          }
          else
          {
            LogMessage = FORMAT(L"Timezone difference of %s detected using file %s", (FormatTimeZone(static_cast<long>(FTimeDifference)), FileLog));
          }
          FTerminal->LogEvent(LogMessage);

          break;
        }
      }
    }

    if (FDetectTimeDifference)
    {
      FTerminal->LogEvent(L"Found no file to use for detecting timezone difference");
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::AutoDetectTimeDifference(
  const UnicodeString & Directory, const TCopyParamType * CopyParam, int Params)
{
  if (NeedAutoDetectTimeDifference() &&
      // do we need FTimeDifference for the operation?
      // (tmAutomatic - AsciiFileMask can theoretically include time constraints, while it is unlikely)
      (!FLAGSET(Params, cpNoConfirmation) ||
       CopyParam->NewerOnly || (CopyParam->TransferMode == tmAutomatic) || !CopyParam->IncludeFileMask.Masks.IsEmpty()))
  {
    FTerminal->LogEvent(L"Retrieving listing to detect timezone difference");
    DummyReadDirectory(Directory);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadDirectory(TRemoteFileList * FileList)
{
  // whole below "-a" logic is for LIST,
  // if we know we are going to use MLSD, skip it
  if (FFileZillaIntf->UsingMlsd())
  {
    DoReadDirectory(FileList);
  }
  else
  {
    bool GotNoFilesForAll = false;
    bool Repeat;

    do
    {
      Repeat = false;
      try
      {
        FDoListAll = (FListAll == asAuto) || (FListAll == asOn);
        DoReadDirectory(FileList);

        // We got no files with "-a", but again no files w/o "-a",
        // so it was not "-a"'s problem, revert to auto and let it decide the next time
        if (GotNoFilesForAll && (FileList->Count == 0))
        {
          DebugAssert(FListAll == asOff);
          FListAll = asAuto;
        }
        else if (FListAll == asAuto)
        {
          // some servers take "-a" as a mask and return empty directory listing
          if (IsEmptyFileList(FileList))
          {
            Repeat = true;
            FListAll = asOff;
            GotNoFilesForAll = true;
            FTerminal->LogEvent(L"LIST with -a switch returned empty directory listing, will try pure LIST");
          }
          else
          {
            // reading first directory has succeeded, always use "-a"
            FListAll = asOn;
          }
        }

        // use "-a" even for implicit directory reading by FZAPI?
        // (e.g. before file transfer)
        // Note that FZAPI ignores this for VMS/MVS.
        FDoListAll = (FListAll == asOn);
      }
      catch(Exception &)
      {
        FDoListAll = false;
        // reading the first directory has failed,
        // further try without "-a" only as the server may not support it
        if (FListAll == asAuto)
        {
          FTerminal->LogEvent(L"LIST with -a failed, will try pure LIST");
          if (!FTerminal->Active)
          {
            FTerminal->Reopen(ropNoReadDirectory);
          }

          FListAll = asOff;
          Repeat = true;
        }
        else
        {
          throw;
        }
      }
    }
    while (Repeat);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoReadFile(const UnicodeString & AFileName,
  TRemoteFile *& AFile)
{
  UnicodeString FileName = AbsolutePath(AFileName, false);
  UnicodeString FileNameOnly;
  UnicodeString FilePath;
  if (IsUnixRootPath(FileName))
  {
    FileNameOnly = FileName;
    FilePath = FileName;
  }
  else
  {
    FileNameOnly = UnixExtractFileName(FileName);
    FilePath = RemoteExtractFilePath(FileName);
  }

  TRemoteFileList * FileList = new TRemoteFileList();
  try
  {
    // Duplicate() call below would use this to compose FullFileName
    FileList->Directory = FilePath;
    TFileListHelper Helper(this, FileList, false);
    FFileZillaIntf->ListFile(FileNameOnly.c_str(), FilePath.c_str());

    GotReply(WaitForCommandReply(), REPLY_2XX_CODE | REPLY_ALLOW_CANCEL);
    TRemoteFile * File = FileList->FindFile(FileNameOnly);
    if (File != NULL)
    {
      AFile = File->Duplicate();
    }

    FLastDataSent = Now();
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::SupportsReadingFile()
{
  return
    FFileZillaIntf->UsingMlsd() ||
    (SupportsCommand(MdtmCommand) && SupportsCommand(SizeCommand));
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadFile(const UnicodeString FileName,
  TRemoteFile *& File)
{
  File = NULL;
  if (SupportsReadingFile())
  {
    DoReadFile(FileName, File);
  }
  else
  {
    if (IsUnixRootPath(FileName))
    {
      FTerminal->LogEvent(FORMAT(L"%s is a root path", (FileName)));
      File = new TRemoteDirectoryFile();
      File->FullFileName = FileName;
      File->FileName = L"";
    }
    else
    {
      UnicodeString Path = RemoteExtractFilePath(FileName);
      UnicodeString NameOnly;
      int P = 0; // shut up
      bool MVSPath =
        FMVS && Path.IsEmpty() &&
        (FileName.SubString(1, 1) == L"'") && (FileName.SubString(FileName.Length(), 1) == L"'") &&
        ((P = FileName.Pos(L".")) > 0);
      if (!MVSPath)
      {
        NameOnly = UnixExtractFileName(FileName);
      }
      else
      {
        NameOnly = FileName.SubString(P + 1, FileName.Length() - P - 1);
      }
      DebugAssert(!FVMSAllRevisions);
      TAutoFlag VMSAllRevisionsFlag(FVMSAllRevisions);
      if (FVMS && (NameOnly.Pos(L";") > 2))
      {
        FTerminal->LogEvent(L"VMS versioned file detected, asking for all revisions");
        FVMSAllRevisions = true;
      }
      TRemoteFile * AFile = NULL;
      // FZAPI does not have efficient way to read properties of one file.
      // In case we need properties of set of files from the same directory,
      // cache the file list for future
      if ((FFileListCache != NULL) &&
          UnixSamePath(Path, FFileListCache->Directory) &&
          (UnixIsAbsolutePath(FFileListCache->Directory) ||
          (FFileListCachePath == CurrentDirectory)))
      {
        AFile = FFileListCache->FindFile(NameOnly);
      }
      // if cache is invalid or file is not in cache, (re)read the directory
      if (AFile == NULL)
      {
        TRemoteFileList * FileListCache = new TRemoteFileList();
        FileListCache->Directory = Path;
        try
        {
          ReadDirectory(FileListCache);
        }
        catch(...)
        {
          delete FileListCache;
          throw;
        }
        // set only after we successfully read the directory,
        // otherwise, when we reconnect from ReadDirectory,
        // the FFileListCache is reset from ResetCache.
        delete FFileListCache;
        FFileListCache = FileListCache;
        FFileListCachePath = GetCurrentDirectory();

        AFile = FFileListCache->FindFile(NameOnly);
      }
      VMSAllRevisionsFlag.Release();

      if (AFile != NULL)
      {
        File = AFile->Duplicate();
        if (MVSPath)
        {
          File->FileName = FileName;
          File->FullFileName = FileName;
        }
        if (File->IsSymLink)
        {
          TAutoFlag AutoFlag(FForceReadSymlink);
          File->Complete();
        }
      }
    }
  }

  if (File == NULL)
  {
    throw Exception(FMTLOAD(FILE_NOT_EXISTS, (FileName)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadSymlink(TRemoteFile * SymlinkFile,
  TRemoteFile *& File)
{
  if (FForceReadSymlink && DebugAlwaysTrue(!SymlinkFile->LinkTo.IsEmpty()) && DebugAlwaysTrue(SymlinkFile->HaveFullFileName))
  {
    // When we get here from TFTPFileSystem::ReadFile, it's likely the second time ReadSymlink has been called for the link.
    // The first time getting to the later branch, so IsDirectory is true and hence FullFileName ends with a slash.
    UnicodeString SymlinkDir = UnixExtractFileDir(UnixExcludeTrailingBackslash(SymlinkFile->FullFileName));
    UnicodeString LinkTo = ::AbsolutePath(SymlinkDir, SymlinkFile->LinkTo);
    ReadFile(LinkTo, File);
  }
  else
  {
    // Resolving symlinks over FTP is big overhead
    // (involves opening TCPIP connection for retrieving "directory listing").
    // Moreover FZAPI does not support that anyway.
    // Though nowadays we could use MLST to read the symlink.
    std::unique_ptr<TRemoteFile> AFile(new TRemoteFile(SymlinkFile));
    AFile->Terminal = FTerminal;
    AFile->FileName = UnixExtractFileName(SymlinkFile->LinkTo);
    // FZAPI treats all symlink target as directories
    AFile->Type = FILETYPE_DIRECTORY;
    File = AFile.release();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::RenameFile(
  const UnicodeString & AFileName, const TRemoteFile *, const UnicodeString & ANewName, bool DebugUsedArg(Overwrite))
{
  UnicodeString FileName = AbsolutePath(AFileName, false);
  UnicodeString NewName = AbsolutePath(ANewName, false);

  UnicodeString FileNameOnly = UnixExtractFileName(FileName);
  UnicodeString FilePathOnly = RemoteExtractFilePath(FileName);
  UnicodeString NewNameOnly = UnixExtractFileName(NewName);
  UnicodeString NewPathOnly = RemoteExtractFilePath(NewName);

  {
    // ignore file list
    TFileListHelper Helper(this, NULL, true);

    FFileZillaIntf->Rename(FileNameOnly.c_str(), NewNameOnly.c_str(),
      FilePathOnly.c_str(), NewPathOnly.c_str());

    GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CopyFile(
  const UnicodeString & FileName, const TRemoteFile *, const UnicodeString & NewName, bool DebugUsedArg(Overwrite))
{
  DebugAssert(SupportsSiteCommand(CopySiteCommand));
  EnsureLocation();

  UnicodeString Command;

  Command = FORMAT(L"%s CPFR %s", (SiteCommand, FileName));
  SendCommand(Command);
  GotReply(WaitForCommandReply(), REPLY_3XX_CODE);

  Command = FORMAT(L"%s CPTO %s", (SiteCommand, NewName));
  SendCommand(Command);
  GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TFTPFileSystem::GetFixedPaths()
{
  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::SpaceAvailable(const UnicodeString Path,
  TSpaceAvailable & ASpaceAvailable)
{
  if (FBytesAvailableSupported)
  {
    std::unique_ptr<TRemoteFileList> DummyFileList(new TRemoteFileList());
    DummyFileList->Directory = Path;
    ReadDirectory(DummyFileList.get());
    ASpaceAvailable.UnusedBytesAvailableToUser = FBytesAvailable;
  }
  else if (SupportsCommand(XQuotaCommand))
  {
    // WS_FTP:
    // XQUOTA
    // 213-File and disk usage
    //     File count: 3
    //     File limit: 10000
    //     Disk usage: 1532791
    //     Disk limit: 2048000
    // 213 File and disk usage end

    // XQUOTA is global not path-specific
    UnicodeString Command = XQuotaCommand;
    SendCommand(Command);
    TStrings * Response = NULL;
    GotReply(WaitForCommandReply(), REPLY_2XX_CODE, L"", NULL, &Response);
    std::unique_ptr<TStrings> ResponseOwner(Response);

    __int64 UsedBytes = -1;
    for (int Index = 0; Index < Response->Count; Index++)
    {
      // trimming padding
      UnicodeString Line = Trim(Response->Strings[Index]);
      UnicodeString Label = CutToChar(Line, L':', true);
      if (SameText(Label, L"Disk usage"))
      {
        UsedBytes = StrToInt64(Line);
      }
      else if (SameText(Label, L"Disk limit") && !SameText(Line, L"unlimited"))
      {
        ASpaceAvailable.BytesAvailableToUser = StrToInt64(Line);
      }
    }

    if ((UsedBytes >= 0) && (ASpaceAvailable.BytesAvailableToUser > 0))
    {
      ASpaceAvailable.UnusedBytesAvailableToUser = ASpaceAvailable.BytesAvailableToUser - UsedBytes;
    }
  }
  else if (SupportsCommand(AvblCommand))
  {
    // draft-peterson-streamlined-ftp-command-extensions-10
    // Implemented by Serv-U.
    UnicodeString Command = FORMAT(L"%s %s", (AvblCommand, Path));
    SendCommand(Command);
    UnicodeString Response = GotReply(WaitForCommandReply(), REPLY_2XX_CODE | REPLY_SINGLE_LINE);
    ASpaceAvailable.UnusedBytesAvailableToUser = StrToInt64(Response);
  }
}
//---------------------------------------------------------------------------
const TSessionInfo & __fastcall TFTPFileSystem::GetSessionInfo()
{
  return FSessionInfo;
}
//---------------------------------------------------------------------------
const TFileSystemInfo & __fastcall TFTPFileSystem::GetFileSystemInfo(bool /*Retrieve*/)
{
  if (!FFileSystemInfoValid)
  {
    UnicodeString RemoteSystem = FSystem;
    AddToList(RemoteSystem, FServerID, L", ");
    RemoteSystem.Unique();
    FFileSystemInfo.RemoteSystem = RemoteSystem;

    if (FFeatures->Count == 0)
    {
      FFileSystemInfo.AdditionalInfo = LoadStr(FTP_NO_FEATURE_INFO);
    }
    else
    {
      FFileSystemInfo.AdditionalInfo =
        FORMAT(L"%s\r\n", (LoadStr(FTP_FEATURE_INFO)));
      for (int Index = 0; Index < FFeatures->Count; Index++)
      {
        FFileSystemInfo.AdditionalInfo += FORMAT(L"  %s\r\n", (FFeatures->Strings[Index]));
      }
    }

    FTerminal->SaveCapabilities(FFileSystemInfo);

    FFileSystemInfoValid = true;
  }
  return FFileSystemInfo;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::TemporaryTransferFile(const UnicodeString & /*FileName*/)
{
  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::GetStoredCredentialsTried()
{
  return FStoredPasswordTried;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::GetUserName()
{
  return FUserName;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::GetCurrentDirectory()
{
  return FCurrentDirectory;
}
//---------------------------------------------------------------------------
const wchar_t * __fastcall TFTPFileSystem::GetOption(int OptionID) const
{
  TSessionData * Data = FTerminal->SessionData;

  switch (OptionID)
  {
    case OPTION_PROXYHOST:
    case OPTION_FWHOST:
      FOptionScratch = Data->ProxyHost;
      break;

    case OPTION_PROXYUSER:
    case OPTION_FWUSER:
      FOptionScratch = Data->ProxyUsername;
      break;

    case OPTION_PROXYPASS:
    case OPTION_FWPASS:
      FOptionScratch = Data->ProxyPassword;
      break;

    case OPTION_TRANSFERIP:
      FOptionScratch = FTerminal->Configuration->ExternalIpAddress;
      break;

    case OPTION_ANONPWD:
    case OPTION_TRANSFERIP6:
      FOptionScratch = L"";
      break;

    case OPTION_MPEXT_CERT_STORAGE:
      FOptionScratch = FTerminal->Configuration->CertificateStorageExpanded;
      break;

    default:
      DebugFail();
      FOptionScratch = L"";
  }

  return FOptionScratch.c_str();
}
//---------------------------------------------------------------------------
int __fastcall TFTPFileSystem::GetOptionVal(int OptionID) const
{
  TSessionData * Data = FTerminal->SessionData;
  int Result;

  switch (OptionID)
  {
    case OPTION_PROXYTYPE:
      switch (Data->ProxyMethod)
      {
        case ::pmNone:
          Result = 0; // PROXYTYPE_NOPROXY;
          break;

        case pmSocks4:
          Result = 2; // PROXYTYPE_SOCKS4A
          break;

        case pmSocks5:
          Result = 3; // PROXYTYPE_SOCKS5
          break;

        case pmHTTP:
          Result = 4; // PROXYTYPE_HTTP11
          break;

        case pmTelnet:
        case pmCmd:
        default:
          DebugFail();
          Result = 0; // PROXYTYPE_NOPROXY;
          break;
      }
      break;

    case OPTION_PROXYPORT:
    case OPTION_FWPORT:
      Result = Data->ProxyPort;
      break;

    case OPTION_PROXYUSELOGON:
      Result = !Data->ProxyUsername.IsEmpty();
      break;

    case OPTION_LOGONTYPE:
      Result = Data->FtpProxyLogonType;
      break;

    case OPTION_TIMEOUTLENGTH:
      Result = Data->Timeout;
      break;

    case OPTION_DEBUGSHOWLISTING:
      Result = (FTerminal->Configuration->ActualLogProtocol >= 0);
      break;

    case OPTION_PASV:
      // should never get here t_server.nPasv being nonzero
      DebugFail();
      Result = FALSE;
      break;

    case OPTION_PRESERVEDOWNLOADFILETIME:
    case OPTION_MPEXT_PRESERVEUPLOADFILETIME:
      Result = FFileTransferPreserveTime ? TRUE : FALSE;
      break;

    case OPTION_LIMITPORTRANGE:
      Result = !FTerminal->SessionData->FtpPasvMode && FTerminal->Configuration->HasLocalPortNumberLimits();
      break;

    case OPTION_PORTRANGELOW:
      Result = FTerminal->Configuration->LocalPortNumberMin;
      break;

    case OPTION_PORTRANGEHIGH:
      Result = FTerminal->Configuration->LocalPortNumberMax;
      break;

    case OPTION_ENABLE_IPV6:
      Result = ((Data->AddressFamily != afIPv4) ? TRUE : FALSE);
      break;

    case OPTION_KEEPALIVE:
      Result = ((Data->FtpPingType != fptOff) ? TRUE : FALSE);
      break;

    case OPTION_INTERVALLOW:
    case OPTION_INTERVALHIGH:
      Result = Data->FtpPingInterval;
      break;

    case OPTION_VMSALLREVISIONS:
      Result = FVMSAllRevisions ? TRUE : FALSE;
      break;

    case OPTION_SPEEDLIMIT_DOWNLOAD_TYPE:
    case OPTION_SPEEDLIMIT_UPLOAD_TYPE:
      Result = (FFileTransferCPSLimit == 0 ? 0 : 1);
      break;

    case OPTION_SPEEDLIMIT_DOWNLOAD_VALUE:
    case OPTION_SPEEDLIMIT_UPLOAD_VALUE:
      Result = (FFileTransferCPSLimit / 1024); // FZAPI expects KB/s
      break;

    case OPTION_MPEXT_SHOWHIDDEN:
      Result = (FDoListAll ? TRUE : FALSE);
      break;

    case OPTION_MPEXT_SSLSESSIONREUSE:
      Result = (Data->SslSessionReuse ? TRUE : FALSE);
      break;

    case OPTION_MPEXT_SNDBUF:
      Result = Data->SendBuf;
      break;

    case OPTION_MPEXT_TRANSFER_ACTIVE_IMMEDIATELY:
      Result = FTransferActiveImmediately;
      break;

    case OPTION_MPEXT_REMOVE_BOM:
      Result = FFileTransferRemoveBOM ? TRUE : FALSE;
      break;

    case OPTION_MPEXT_LOG_SENSITIVE:
      Result = FTerminal->Configuration->LogSensitive ? TRUE : FALSE;
      break;

    case OPTION_MPEXT_HOST:
      Result = (Data->FtpHost == asOn);
      break;

    case OPTION_MPEXT_NODELAY:
      Result = Data->TcpNoDelay;
      break;

    case OPTION_MPEXT_NOLIST:
      Result = FFileTransferNoList ? TRUE : FALSE;
      break;

    case OPTION_MPEXT_COMPLETE_TLS_SHUTDOWN:
      if (Data->CompleteTlsShutdown == asAuto)
      {
        // As of FileZilla Server 1.6.1 this does not seem to be needed. It's still needed with 1.5.1.
        // It was possibly fixed by 1.6.0 (2022-12-06) change:
        // Fixed an issue in the networking code when dealing with TLS close_notify alerts
        Result = FFileZilla ? -1 : 0;
      }
      else
      {
        Result = (Data->CompleteTlsShutdown == asOn) ? 1 : -1;
      }
      break;

    case OPTION_MPEXT_WORK_FROM_CWD:
      Result = (FWorkFromCwd == asOn);
      break;

    case OPTION_MPEXT_TRANSFER_SIZE:
      {
        __int64 TransferSize = 0;
        if ((FTerminal->OperationProgress != NULL) &&
            (FTerminal->OperationProgress->Operation == foCopy) &&
            (FTerminal->OperationProgress->Side == osLocal))
        {
          TransferSize = FTerminal->OperationProgress->TransferSize - FTerminal->OperationProgress->TransferredSize;
        }
        Result = static_cast<int>(static_cast<unsigned int>(TransferSize & std::numeric_limits<unsigned int>::max()));
      }
      break;

    default:
      DebugFail();
      Result = FALSE;
      break;
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::PostMessage(unsigned int Type, WPARAM wParam, LPARAM lParam)
{
  if (Type == TFileZillaIntf::MSG_TRANSFERSTATUS)
  {
    // Stop here if FileTransferProgress is proceeding,
    // it makes "pause" in queue work.
    // Paused queue item stops in some of the TFileOperationProgressType
    // methods called from FileTransferProgress
    TGuard Guard(FTransferStatusCriticalSection);
  }

  TGuard Guard(FQueueCriticalSection);

  FQueue->push_back(TMessageQueue::value_type(wParam, lParam));
  SetEvent(FQueueEvent);

  return true;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::ProcessMessage()
{
  bool Result;
  TMessageQueue::value_type Message;

  {
    TGuard Guard(FQueueCriticalSection);

    Result = !FQueue->empty();
    if (Result)
    {
      Message = FQueue->front();
      FQueue->pop_front();
    }
    else
    {
      // now we are perfectly sure that the queue is empty as it is locked,
      // so reset the event
      ResetEvent(FQueueEvent);
    }
  }

  if (Result)
  {
    FFileZillaIntf->HandleMessage(Message.first, Message.second);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DiscardMessages()
{
  try
  {
    while (ProcessMessage());
  }
  __finally
  {
    FReply = 0;
    FCommandReply = 0;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::WaitForMessages()
{
  unsigned int Result;
  do
  {
    Result = WaitForSingleObject(FQueueEvent, GUIUpdateInterval);
    FTerminal->ProcessGUI();
  } while (Result == WAIT_TIMEOUT);

  if (Result != WAIT_OBJECT_0)
  {
    FTerminal->FatalError(NULL, FMTLOAD(INTERNAL_ERROR, (L"ftp#1", IntToStr(int(Result)))));
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::PoolForFatalNonCommandReply()
{
  DebugAssert(FReply == 0);
  DebugAssert(FCommandReply == 0);
  DebugAssert(!FWaitingForReply);

  FWaitingForReply = true;

  unsigned int Reply;

  try
  {
    // discard up to one reply
    // (it should not happen here that two replies are posted anyway)
    while (ProcessMessage() && (FReply == 0));
    Reply = FReply;
  }
  __finally
  {
    FReply = 0;
    DebugAssert(FCommandReply == 0);
    FCommandReply = 0;
    DebugAssert(FWaitingForReply);
    FWaitingForReply = false;
  }

  if (Reply != 0)
  {
    // throws
    GotNonCommandReply(Reply);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::NoFinalLastCode()
{
  return (FLastCodeClass == 0) || (FLastCodeClass == 1);
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::KeepWaitingForReply(unsigned int & ReplyToAwait, bool WantLastCode)
{
  // To keep waiting,
  // non-command reply must be unset,
  // the reply we wait for must be unset or
  // last code must be unset (if we wait for it).

  // Though make sure that disconnect makes it through always. As for example when connection is closed already,
  // when sending commands, we may get REPLY_DISCONNECTED as a command response and no other response after,
  // which would cause a hang.
  return
     (FReply == 0) &&
     ((ReplyToAwait == 0) ||
      (WantLastCode && NoFinalLastCode() && FLAGCLEAR(ReplyToAwait, TFileZillaIntf::REPLY_DISCONNECTED)));
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoWaitForReply(unsigned int & ReplyToAwait, bool WantLastCode)
{
  try
  {
    while (KeepWaitingForReply(ReplyToAwait, WantLastCode))
    {
      WaitForMessages();
      // wait for the first reply only,
      // i.e. in case two replies are posted get the first only.
      // e.g. when server closes the connection, but posts error message before,
      // sometime it happens that command (like download) fails because of the error
      // and does not catch the disconnection. then asynchronous "disconnect reply"
      // is posted immediately afterwards. leave detection of that to Idle()
      while (ProcessMessage() && KeepWaitingForReply(ReplyToAwait, WantLastCode));
    }

    if (FReply != 0)
    {
      // throws
      GotNonCommandReply(FReply);
    }
  }
  catch(...)
  {
    // even if non-fatal error happens, we must process pending message,
    // so that we "eat" the reply message, and it doesn't get mistakenly
    // associated with future connect
    if (FTerminal->Active)
    {
      DoWaitForReply(ReplyToAwait, WantLastCode);
    }
    throw;
  }
}
//---------------------------------------------------------------------------
unsigned int __fastcall TFTPFileSystem::WaitForReply(bool Command, bool WantLastCode)
{
  DebugAssert(FReply == 0);
  DebugAssert(FCommandReply == 0);
  DebugAssert(!FWaitingForReply);

  ResetReply();
  FWaitingForReply = true;

  unsigned int Reply;

  try
  {
    unsigned int & ReplyToAwait = (Command ? FCommandReply : FReply);
    DoWaitForReply(ReplyToAwait, WantLastCode);

    Reply = ReplyToAwait;
  }
  __finally
  {
    FReply = 0;
    FCommandReply = 0;
    DebugAssert(FWaitingForReply);
    FWaitingForReply = false;
  }

  return Reply;
}
//---------------------------------------------------------------------------
unsigned int __fastcall TFTPFileSystem::WaitForCommandReply(bool WantLastCode)
{
  return WaitForReply(true, WantLastCode);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::WaitForFatalNonCommandReply()
{
  WaitForReply(false, false);
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ResetReply()
{
  FLastCode = 0;
  FLastCodeClass = 0;
  FMultiLineResponse = false;
  DebugAssert(FLastResponse != NULL);
  FLastResponse->Clear();
  DebugAssert(FLastErrorResponse != NULL);
  FLastErrorResponse->Clear();
  DebugAssert(FLastError != NULL);
  FLastError->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::GotNonCommandReply(unsigned int Reply)
{
  DebugAssert(FLAGSET(Reply, TFileZillaIntf::REPLY_DISCONNECTED));
  GotReply(Reply);
  // should never get here as GotReply should raise fatal exception
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Disconnect()
{
  Discard();
  FTerminal->Closed();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::GotReply(unsigned int Reply, unsigned int Flags,
  UnicodeString Error, unsigned int * Code, TStrings ** Response)
{
  UnicodeString Result;
  try
  {
    if (FLAGSET(Reply, TFileZillaIntf::REPLY_OK))
    {
      DebugAssert(Reply == TFileZillaIntf::REPLY_OK);

      // With REPLY_2XX_CODE treat "OK" non-2xx code like an error.
      // REPLY_3XX_CODE has to be always used along with REPLY_2XX_CODE.
      if ((FLAGSET(Flags, REPLY_2XX_CODE) && (FLastCodeClass != 2)) &&
          ((FLAGCLEAR(Flags, REPLY_3XX_CODE) || (FLastCodeClass != 3))))
      {
        GotReply(TFileZillaIntf::REPLY_ERROR, Flags, Error);
      }
    }
    else if (FLAGSET(Reply, TFileZillaIntf::REPLY_CANCEL) &&
        FLAGSET(Flags, REPLY_ALLOW_CANCEL))
    {
      DebugAssert(
        (Reply == (TFileZillaIntf::REPLY_CANCEL | TFileZillaIntf::REPLY_ERROR)) ||
        (Reply == (TFileZillaIntf::REPLY_ABORTED | TFileZillaIntf::REPLY_CANCEL | TFileZillaIntf::REPLY_ERROR)));
      // noop
    }
    // we do not expect these with our usage of FZ
    else if (Reply &
          (TFileZillaIntf::REPLY_WOULDBLOCK | TFileZillaIntf::REPLY_OWNERNOTSET |
           TFileZillaIntf::REPLY_INVALIDPARAM | TFileZillaIntf::REPLY_ALREADYCONNECTED |
           TFileZillaIntf::REPLY_IDLE | TFileZillaIntf::REPLY_NOTINITIALIZED |
           TFileZillaIntf::REPLY_ALREADYINIZIALIZED))
    {
      FTerminal->FatalError(NULL, FMTLOAD(INTERNAL_ERROR, (L"ftp#2", FORMAT(L"0x%x", (int(Reply))))));
    }
    else
    {
      // everything else must be an error or disconnect notification
      DebugAssert(
        FLAGSET(Reply, TFileZillaIntf::REPLY_ERROR) ||
        FLAGSET(Reply, TFileZillaIntf::REPLY_DISCONNECTED));

      // TODO: REPLY_CRITICALERROR ignored

      // REPLY_NOTCONNECTED happens if connection is closed between moment
      // when FZAPI interface method dispatches the command to FZAPI thread
      // and moment when FZAPI thread receives the command
      bool Disconnected =
        FLAGSET(Reply, TFileZillaIntf::REPLY_DISCONNECTED) ||
        FLAGSET(Reply, TFileZillaIntf::REPLY_NOTCONNECTED);
      bool DoClose = false;

      UnicodeString HelpKeyword;
      TStrings * MoreMessages = new TStringList();
      try
      {
        if (Disconnected)
        {
          if (FLAGCLEAR(Flags, REPLY_CONNECT))
          {
            MoreMessages->Add(LoadStr(LOST_CONNECTION));
            Disconnect();
          }
          else
          {
            // For connection failure, do not report that connection was lost,
            // its obvious.
            // Also do not report to terminal that we are closed as
            // that turns terminal into closed mode, but we want to
            // pretend (at least with failed authentication) to retry
            // with the same connection (as with SSH), so we explicitly
            // close terminal in Open() only after we give up
            Discard();
          }
        }

        if (FLAGSET(Reply, TFileZillaIntf::REPLY_ABORTED))
        {
          MoreMessages->Add(LoadStr(USER_TERMINATED));
        }

        if (FLAGSET(Reply, TFileZillaIntf::REPLY_NOTSUPPORTED))
        {
          MoreMessages->Add(LoadStr(NOTSUPPORTED));
        }

        if (FLastCode == 530)
        {
          // Serv-U also uses this code in response to "SITE PSWD"
          MoreMessages->Add(LoadStr(AUTHENTICATION_FAILED));
        }

        bool RetryTransfer = false;
        if ((FLastCode == 425) || (FLastCode == 426))
        {
          if (FAnyTransferSucceeded)
          {
            FTerminal->LogEvent(FORMAT(L"Got %d after some previous data connections succeeded, retrying connection", (FLastCode)));
            RetryTransfer = true;
          }
          else if (!FTerminal->SessionData->FtpPasvMode)
          {
            MoreMessages->Add(LoadStr(FTP_CANNOT_OPEN_ACTIVE_CONNECTION2));
            HelpKeyword = HELP_FTP_CANNOT_OPEN_ACTIVE_CONNECTION;
          }
        }

        if (FLastCode == DummyTimeoutCode)
        {
          HelpKeyword = HELP_ERRORMSG_TIMEOUT;
        }

        if (FLastCode == DummyDisconnectCode)
        {
          HelpKeyword = HELP_STATUSMSG_DISCONNECTED;
        }

        if (FAnyTransferSucceeded && (FLastError->Count > 0))
        {
          UnicodeString CantOpenTransferChannelMessage = LoadStr(IDS_ERRORMSG_CANTOPENTRANSFERCHANNEL);
          int P = CantOpenTransferChannelMessage.Pos(L"%");
          if (DebugAlwaysTrue(P > 0))
          {
            CantOpenTransferChannelMessage.SetLength(P - 1);
          }
          if (ContainsText(FLastError->Strings[0], CantOpenTransferChannelMessage))
          {
            FTerminal->LogEvent(L"Failed to connection data connection after some previous data connections succeeded, retrying connection");
            RetryTransfer = true;
          }
        }

        if (RetryTransfer)
        {
          Disconnected = true;
          // Close only later, as we still need to use FLast* fields
          DoClose = true;
        }

        MoreMessages->AddStrings(FLastError);
        // already cleared from WaitForReply, but GotReply can be also called
        // from Closed. then make sure that error from previous command not
        // associated with session closure is not reused
        FLastError->Clear();

        MoreMessages->AddStrings(FLastErrorResponse);
        // see comment for FLastError
        FLastResponse->Clear();
        FLastErrorResponse->Clear();

        if (MoreMessages->Count == 0)
        {
          delete MoreMessages;
          MoreMessages = NULL;
        }
      }
      catch(...)
      {
        delete MoreMessages;
        throw;
      }

      if (Error.IsEmpty() && (MoreMessages != NULL))
      {
        DebugAssert(MoreMessages->Count > 0);
        // bit too generic assigning of main instructions, let's see how it works
        Error = MainInstructions(MoreMessages->Strings[0]);
        MoreMessages->Delete(0);
      }

      if (Disconnected)
      {
        if (DoClose)
        {
          Close();
        }
        // for fatal error, it is essential that there is some message
        DebugAssert(!Error.IsEmpty());
        ExtException * E = new ExtException(Error, MoreMessages, true, HelpKeyword);
        try
        {
          FTerminal->FatalError(E, L"");
        }
        __finally
        {
          delete E;
        }
      }
      else
      {
        throw ExtException(Error, MoreMessages, true, HelpKeyword);
      }
    }

    if ((Code != NULL) && (FLastCodeClass != DummyCodeClass))
    {
      *Code = FLastCode;
    }

    if (FLAGSET(Flags, REPLY_SINGLE_LINE))
    {
      if (FLastResponse->Count != 1)
      {
        throw Exception(FMTLOAD(FTP_RESPONSE_ERROR, (FLastCommandSent, FLastResponse->Text)));
      }
      Result = FLastResponse->Strings[0];
    }

    if (Response != NULL)
    {
      *Response = FLastResponse;
      FLastResponse = new TStringList();
      // just to be consistent
      delete FLastErrorResponse;
      FLastErrorResponse = new TStringList();
    }
  }
  __finally
  {
    ResetReply();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::SendCommand(const UnicodeString & Command)
{
  FFileZillaIntf->CustomCommand(Command.c_str());
  FLastCommandSent = CopyToChar(Command, L' ', false);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::SetLastCode(int Code)
{
  FLastCode = Code;
  FLastCodeClass = (Code / 100);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::StoreLastResponse(const UnicodeString & Text)
{
  FLastResponse->Add(Text);
  if (FLastCodeClass >= 4)
  {
    FLastErrorResponse->Add(Text);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::HandleReplyStatus(UnicodeString Response)
{
  if (FOnCaptureOutput != NULL)
  {
    FOnCaptureOutput(Response, cotOutput);
  }

  if (FWelcomeMessage.IsEmpty() && StartsStr(L"SSH", Response))
  {
    FLastErrorResponse->Add(LoadStr(SFTP_AS_FTP_ERROR));
  }

  // Two forms of multiline responses were observed
  // (the first is according to the RFC 959):

  // 211-Features:
  //  MDTM
  //  REST STREAM
  //  SIZE
  // 211 End

  // This format is according to RFC 2228.
  // Is used by ProFTPD when deprecated MultilineRFC2228 directive is enabled
  // http://www.proftpd.org/docs/modules/mod_core.html#FileZillaNonASCII

  // 211-Features:
  // 211-MDTM
  // 211-REST STREAM
  // 211-SIZE
  // 211-AUTH TLS
  // 211-PBSZ
  // 211-PROT
  // 211 End

  // IIS 2003:

  // 211-FEAT
  //     SIZE
  //     MDTM
  // 211 END

  // Partially duplicated in CFtpControlSocket::OnReceive

  int Code = 0; // shut up
  bool HasCodePrefix =
    (Response.Length() >= 3) &&
    TryStrToInt(Response.SubString(1, 3), Code) &&
    (Code >= 100) && (Code <= 599) &&
    ((Response.Length() == 3) || (Response[4] == L' ') || (Response[4] == L'-'));

  if (HasCodePrefix && !FMultiLineResponse)
  {
    FMultiLineResponse = (Response.Length() >= 4) && (Response[4] == L'-');
    FLastResponse->Clear();
    FLastErrorResponse->Clear();
    SetLastCode(Code);
    if (Response.Length() >= 5)
    {
      StoreLastResponse(Response.SubString(5, Response.Length() - 4));
    }
  }
  else
  {
    int Start;
    // response with code prefix
    if (HasCodePrefix && (FLastCode == Code))
    {
      // End of multiline response?
      if ((Response.Length() <= 3) || (Response[4] == L' '))
      {
        FMultiLineResponse = false;
      }
      Start = 5;
    }
    else
    {
      Start = (((Response.Length() >= 1) && (Response[1] == L' ')) ? 2 : 1);
    }

    // Intermediate empty lines are being added
    if (FMultiLineResponse || (Response.Length() >= Start))
    {
      StoreLastResponse(Response.SubString(Start, Response.Length() - Start + 1));
    }
  }

  if (StartsStr(DirectoryHasBytesPrefix, Response))
  {
    UnicodeString Buf = Response;
    Buf.Delete(1, DirectoryHasBytesPrefix.Length());
    Buf = Buf.TrimLeft();
    UnicodeString BytesStr = CutToChar(Buf, L' ', true);
    BytesStr = ReplaceStr(BytesStr, L",", L"");
    FBytesAvailable = StrToInt64Def(BytesStr, -1);
    if (FBytesAvailable >= 0)
    {
      FBytesAvailableSupported = true;
    }
  }

  if (!FMultiLineResponse)
  {
    if (FLastCode == 220)
    {
      // HOST command also uses 220 response.
      // Neither our use of welcome message is prepared for changing it
      // during the session, so we keep the initial message only.
      // Theoretically the welcome message can be host-specific,
      // but IIS uses "220 Host accepted", and we are not interested in that anyway.
      // Serv-U repeats the initial welcome message.
      // WS_FTP uses "200 Command HOST succeed"
      if (FWelcomeMessage.IsEmpty())
      {
        FWelcomeMessage = FLastResponse->Text;
        if (FTerminal->Configuration->ShowFtpWelcomeMessage)
        {
          FTerminal->DisplayBanner(FWelcomeMessage);
        }
        // Idea FTP Server v0.80
        if ((FTerminal->SessionData->FtpTransferActiveImmediately == asAuto) &&
            FWelcomeMessage.Pos(L"Idea FTP Server") > 0)
        {
          FTerminal->LogEvent(L"The server requires TLS/SSL handshake on transfer connection before responding 1yz to STOR/APPE");
          FTransferActiveImmediately = true;
        }
        if (ContainsText(FWelcomeMessage, L"Microsoft FTP Service") && !FIIS)
        {
          FTerminal->LogEvent(L"IIS detected.");
          FIIS = true;
        }
      }
    }
    else if (FLastCommand == PASS)
    {
      FStoredPasswordTried = true;
      // 530 = "Not logged in."
      // 501 = "Login incorrect." (ProFTPD empty password code)
      if ((FLastCode == 530) ||
          (FLastCode == 501))
      {
        FPasswordFailed = true;
      }
    }
    else if (FLastCommand == SYST)
    {
      DebugAssert(FSystem.IsEmpty());
      // Positive reply to "SYST" should be 215, see RFC 959.
      // But "VMS VAX/VMS V6.1 on node nsrp14" uses plain 200.
      if (FLastCodeClass == 2)
      {
        FSystem = FLastResponse->Text.TrimRight();
        // FZAPI has own detection of MVS/VMS

        // Full name is "MVS is the operating system of this server. FTP Server is running on ..."
        // (the ... can be "z/OS")
        // https://www.ibm.com/docs/en/zos/latest?topic=2rc-215-mvs-is-operating-system-this-server-ftp-server-is-running-name
        // FZPI has a different incompatible detection.
        // MVS FTP servers have two separate MVS and Unix file systems coexisting in the same session.
        FMVS = (FSystem.SubString(1, 3) == L"MVS");
        if (FMVS)
        {
          FTerminal->LogEvent(L"MVS system detected.");
        }

        // The FWelcomeMessage usually contains "Microsoft FTP Service" but can be empty
        if (ContainsText(FSystem, L"Windows_NT"))
        {
          FTerminal->LogEvent(L"The server is probably running Windows, assuming that directory listing timestamps are affected by DST.");
          FWindowsServer = true;
          if (!FIIS)
          {
            FTerminal->LogEvent(L"IIS detected.");
            FIIS = true;
          }
        }
        // VMS system type. VMS V5.5-2.
        // VMS VAX/VMS V6.1 on node nsrp14
        if (FSystem.SubString(1, 4) == L"VMS ")
        {
          FTerminal->LogEvent(L"VMS system detected.");
          FVMS = true;
        }
        if ((FListAll == asAuto) &&
             // full name is "Personal FTP Server PRO K6.0"
            ((FSystem.Pos(L"Personal FTP Server") > 0) ||
             FMVS || FVMS))
        {
          FTerminal->LogEvent(L"Server is known not to support LIST -a");
          FListAll = asOff;
        }
        if ((FWorkFromCwd == asAuto) && FVMS)
        {
          FTerminal->LogEvent(L"Server is known to require use of relative paths");
          FWorkFromCwd = asOn;
        }
        // 220-FileZilla Server 1.0.1
        // 220 Please visit https://filezilla-project.org/
        // SYST
        // 215 UNIX emulated by FileZilla
        // (Welcome message is configurable)
        if (ContainsText(FSystem, L"FileZilla"))
        {
          FTerminal->LogEvent(L"FileZilla server detected.");
          FFileZilla = true;
        }
      }
      else
      {
        FSystem = L"";
      }
    }
    else if (FLastCommand == FEAT)
    {
      HandleFeatReply();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ResetFeatures()
{
  FFeatures->Clear();
  FSupportedCommands->Clear();
  FSupportedSiteCommands->Clear();
  FHashAlgs->Clear();
  FSupportsAnyChecksumFeature = false;
}
//---------------------------------------------------------------------------
void TFTPFileSystem::ProcessFeatures()
{
  std::unique_ptr<TStrings> Features(FTerminal->ProcessFeatures(FFeatures));

  for (int Index = 0; Index < Features->Count; Index++)
  {
    UnicodeString Feature = Features->Strings[Index];

    UnicodeString Args = Feature;
    UnicodeString Command = CutToChar(Args, L' ', true);

    // Serv-U lists Xalg commands like:
    //  XSHA1 filename;start;end
    FSupportedCommands->Add(Command);

    if (SameText(Command, SiteCommand))
    {
      // Serv-U lists all SITE commands in one line like:
      //  SITE PSWD;SET;ZONE;CHMOD;MSG;EXEC;HELP
      // But ProFTPD lists them separately:
      //  SITE UTIME
      //  SITE RMDIR
      //  SITE COPY
      //  SITE MKDIR
      //  SITE SYMLINK
      while (!Args.IsEmpty())
      {
        UnicodeString Arg = CutToChar(Args, L';', true);
        FSupportedSiteCommands->Add(Arg);
      }
    }
    else if (SameText(Command, HashCommand))
    {
      while (!Args.IsEmpty())
      {
        UnicodeString Alg = CutToChar(Args, L';', true);
        if ((Alg.Length() > 0) && (Alg[Alg.Length()] == L'*'))
        {
          Alg.Delete(Alg.Length(), 1);
        }
        // FTP HASH alg names follow IANA as we do,
        // but using uppercase and we use lowercase
        FHashAlgs->Add(LowerCase(Alg));
        FSupportsAnyChecksumFeature = true;
      }
    }

    if (FChecksumCommands->IndexOf(Command) >= 0)
    {
      FSupportsAnyChecksumFeature = true;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::HandleFeatReply()
{
  ResetFeatures();
  // Response to FEAT must be multiline, where leading and trailing line
  // is "meaningless". See RFC 2389.
  if ((FLastCode == 211) && (FLastResponse->Count > 2))
  {
    FLastResponse->Delete(0);
    FLastResponse->Delete(FLastResponse->Count - 1);
    for (int Index = 0; Index < FLastResponse->Count; Index++)
    {
      FFeatures->Add(FLastResponse->Strings[Index].Trim());
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleStatus(const wchar_t * AStatus, int Type)
{
  TLogLineType LogType = (TLogLineType)-1;
  UnicodeString Status(AStatus);

  switch (Type)
  {
    case TFileZillaIntf::LOG_STATUS:
      FTerminal->Information(Status);
      LogType = llMessage;
      break;

    case TFileZillaIntf::LOG_COMMAND:
      if (Status == L"SYST")
      {
        // not to trigger the assert in HandleReplyStatus,
        // when SYST command is used by the user
        FSystem = "";
        FLastCommand = SYST;
      }
      else if (Status == L"FEAT")
      {
        FLastCommand = FEAT;
      }
      else if (Status.SubString(1, 5) == L"PASS ")
      {
        FLastCommand = PASS;
      }
      else
      {
        FLastCommand = CMD_UNKNOWN;
      }
      if (!FLoggedIn || (FTerminal->Configuration->ActualLogProtocol >= 0))
      {
        LogType = llInput;
      }
      break;

    case TFileZillaIntf::LOG_ERROR:
    case TFileZillaIntf::LOG_APIERROR:
    case TFileZillaIntf::LOG_WARNING:
      // when timeout message occurs, break loop waiting for response code
      // by setting dummy one
      if (Type == TFileZillaIntf::LOG_ERROR)
      {
        if (StartsStr(FTimeoutStatus, Status))
        {
          if (NoFinalLastCode())
          {
            SetLastCode(DummyTimeoutCode);
          }
        }
        else if (Status == FDisconnectStatus)
        {
          if (NoFinalLastCode())
          {
            SetLastCode(DummyDisconnectCode);
          }
        }
      }
      // there can be multiple error messages associated with single failure
      // (such as "cannot open local file..." followed by "download failed")
      FLastError->Add(Status);
      LogType = llMessage;
      break;

    case TFileZillaIntf::LOG_PROGRESS:
      LogType = llMessage;
      break;

    case TFileZillaIntf::LOG_REPLY:
      HandleReplyStatus(AStatus);
      if (!FLoggedIn || (FTerminal->Configuration->ActualLogProtocol >= 0))
      {
        LogType = llOutput;
      }
      break;

    case TFileZillaIntf::LOG_INFO:
      LogType = llMessage;
      break;

    case TFileZillaIntf::LOG_DEBUG:
      LogType = llMessage;
      break;

    default:
      DebugFail();
      break;
  }

  if (FTerminal->Log->Logging && (LogType != (TLogLineType)-1))
  {
    FTerminal->Log->Add(LogType, Status);
  }

  return true;
}
//---------------------------------------------------------------------------
TDateTime __fastcall TFTPFileSystem::ConvertLocalTimestamp(time_t Time)
{
  // This reverses how FZAPI converts FILETIME to time_t,
  // before passing it to FZ_ASYNCREQUEST_OVERWRITE.
  __int64 Timestamp;
  tm * Tm = localtime(&Time);
  if (Tm != NULL)
  {
    SYSTEMTIME SystemTime;
    SystemTime.wYear = static_cast<WORD>(Tm->tm_year + 1900);
    SystemTime.wMonth = static_cast<WORD>(Tm->tm_mon + 1);
    SystemTime.wDayOfWeek = 0;
    SystemTime.wDay = static_cast<WORD>(Tm->tm_mday);
    SystemTime.wHour = static_cast<WORD>(Tm->tm_hour);
    SystemTime.wMinute = static_cast<WORD>(Tm->tm_min);
    SystemTime.wSecond = static_cast<WORD>(Tm->tm_sec);
    SystemTime.wMilliseconds = 0;

    FILETIME LocalTime;
    SystemTimeToFileTime(&SystemTime, &LocalTime);
    FILETIME FileTime;
    LocalFileTimeToFileTime(&LocalTime, &FileTime);
    Timestamp = ConvertTimestampToUnixSafe(FileTime, dstmUnix);
  }
  else
  {
    // incorrect, but at least something
    Timestamp = Time;
  }

  return UnixToDateTime(Timestamp, dstmUnix);
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleAsynchRequestOverwrite(
  wchar_t * FileName1, size_t FileName1Len, const wchar_t * FileName2,
  const wchar_t * Path1, const wchar_t * Path2,
  __int64 Size1, __int64 Size2, time_t LocalTime,
  bool /*HasLocalTime*/, const TRemoteFileTime & RemoteTime, void * AUserData, int & RequestResult)
{
  if (!FActive)
  {
    return false;
  }
  else
  {
    TFileTransferData & UserData = *((TFileTransferData *)AUserData);
    if (UserData.OverwriteResult >= 0)
    {
      // on retry, use the same answer as on the first attempt
      RequestResult = UserData.OverwriteResult;
    }
    else if ((UserData.CopyParam->OnTransferOut != NULL) || (UserData.CopyParam->OnTransferIn != NULL))
    {
      DebugFail();
      RequestResult = TFileZillaIntf::FILEEXISTS_OVERWRITE;
    }
    else
    {
      TFileOperationProgressType * OperationProgress = FTerminal->OperationProgress;
      UnicodeString TargetFileName = FileName1;
      DebugAssert(UserData.FileName == TargetFileName);

      UnicodeString SourceFullFileName = Path2;
      UnicodeString TargetFullFileName = Path1;
      if (OperationProgress->Side == osLocal)
      {
        SourceFullFileName = IncludeTrailingBackslash(SourceFullFileName);
        TargetFullFileName = UnixIncludeTrailingBackslash(TargetFullFileName);
      }
      else
      {
        SourceFullFileName = UnixIncludeTrailingBackslash(SourceFullFileName);
        TargetFullFileName = IncludeTrailingBackslash(TargetFullFileName);
      }
      SourceFullFileName += FileName2;
      TargetFullFileName += FileName1;

      TOverwriteMode OverwriteMode = omOverwrite;
      TOverwriteFileParams FileParams;
      bool NoFileParams =
        (Size1 < 0) || (LocalTime == 0) ||
        (Size2 < 0) || !RemoteTime.HasDate;
      if (!NoFileParams)
      {
        FileParams.SourceSize = Size2;
        FileParams.DestSize = Size1;

        // Time is coming from LIST (not from MLSD or MDTM)
        bool NeedApplyTimeDifference = !RemoteTime.Utc && DebugAlwaysTrue(!FFileZillaIntf->UsingMlsd());

        if (OperationProgress->Side == osLocal)
        {
          FileParams.SourceTimestamp = ConvertLocalTimestamp(LocalTime);
          RemoteFileTimeToDateTimeAndPrecision(RemoteTime, FileParams.DestTimestamp, FileParams.DestPrecision);
          if (NeedApplyTimeDifference)
          {
            ApplyTimeDifference(TargetFullFileName, FileParams.DestTimestamp, FileParams.DestPrecision);
          }
        }
        else
        {
          FileParams.DestTimestamp = ConvertLocalTimestamp(LocalTime);
          RemoteFileTimeToDateTimeAndPrecision(RemoteTime, FileParams.SourceTimestamp, FileParams.SourcePrecision);
          if (NeedApplyTimeDifference)
          {
            ApplyTimeDifference(SourceFullFileName, FileParams.SourceTimestamp, FileParams.SourcePrecision);
          }
        }
      }

      bool AllowResume = UserData.CopyParam->AllowResume(FileParams.SourceSize, UnicodeString());
      bool AutoResume = UserData.AutoResume && AllowResume;
      if (ConfirmOverwrite(SourceFullFileName, TargetFileName, OverwriteMode, OperationProgress,
            (NoFileParams ? NULL : &FileParams), UserData.CopyParam, UserData.Params, AutoResume))
      {
        switch (OverwriteMode)
        {
          case omOverwrite:
            if (TargetFileName != FileName1)
            {
              wcsncpy(FileName1, TargetFileName.c_str(), FileName1Len);
              FileName1[FileName1Len - 1] = L'\0';
              UserData.FileName = FileName1;
              RequestResult = TFileZillaIntf::FILEEXISTS_RENAME;
            }
            else
            {
              RequestResult = TFileZillaIntf::FILEEXISTS_OVERWRITE;
            }
            break;

          case omResume:
            RequestResult = TFileZillaIntf::FILEEXISTS_RESUME;
            break;

          case omComplete:
            FTerminal->LogEvent(L"File transfer was completed before disconnect");
            RequestResult = TFileZillaIntf::FILEEXISTS_COMPLETE;
            break;

          default:
            DebugFail();
            RequestResult = TFileZillaIntf::FILEEXISTS_OVERWRITE;
            break;
        }
      }
      else
      {
        RequestResult = TFileZillaIntf::FILEEXISTS_SKIP;
      }
    }

    // remember the answer for the retries
    UserData.OverwriteResult = RequestResult;

    if (RequestResult == TFileZillaIntf::FILEEXISTS_SKIP)
    {
      // when user chooses not to overwrite, break loop waiting for response code
      // by setting dummy one, as FZAPI won't do anything then
      SetLastCode(DummyTimeoutCode);
    }

    return true;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FormatContactList(UnicodeString Entry1, UnicodeString Entry2)
{
  if (!Entry1.IsEmpty() && !Entry2.IsEmpty())
  {
    return FORMAT(L"%s, %s", (Entry1, Entry2));
  }
  else
  {
    return Entry1 + Entry2;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FormatContact(const TFtpsCertificateData::TContact & Contact)
{
  UnicodeString Result =
    FORMAT(LoadStrPart(VERIFY_CERT_CONTACT, 1),
      (FormatContactList(FormatContactList(FormatContactList(
        Contact.Organization, Contact.Unit), Contact.CommonName), Contact.Mail)));

  if ((wcslen(Contact.Country) > 0) ||
      (wcslen(Contact.StateProvince) > 0) ||
      (wcslen(Contact.Town) > 0))
  {
    Result +=
      FORMAT(LoadStrPart(VERIFY_CERT_CONTACT, 2),
        (FormatContactList(FormatContactList(
          Contact.Country, Contact.StateProvince), Contact.Town)));
  }

  if (wcslen(Contact.Other) > 0)
  {
    Result += FORMAT(LoadStrPart(VERIFY_CERT_CONTACT, 3), (Contact.Other));
  }

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FormatValidityTime(const TFtpsCertificateData::TValidityTime & ValidityTime)
{
  return FormatDateTime(L"ddddd tt",
    EncodeDateVerbose(
      (unsigned short)ValidityTime.Year, (unsigned short)ValidityTime.Month,
      (unsigned short)ValidityTime.Day) +
    EncodeTimeVerbose(
      (unsigned short)ValidityTime.Hour, (unsigned short)ValidityTime.Min,
      (unsigned short)ValidityTime.Sec, 0));
}
//---------------------------------------------------------------------------
bool __fastcall VerifyNameMask(UnicodeString Name, UnicodeString Mask)
{
  bool Result = true;
  int Pos CLANG_INITIALIZE(0);
  while (Result && (Pos = Mask.Pos(L"*")) > 0)
  {
    // Pos will typically be 1 here, so not actual comparison is done
    Result = SameText(Mask.SubString(1, Pos - 1), Name.SubString(1, Pos - 1));
    if (Result)
    {
      Mask.Delete(1, Pos); // including *
      Name.Delete(1, Pos - 1);
      // remove everything until the next dot
      Pos = Name.Pos(L".");
      if (Pos == 0)
      {
        Pos = Name.Length() + 1;
      }
      Name.Delete(1, Pos - 1);
    }
  }

  if (Result)
  {
    Result = SameText(Mask, Name);
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::VerifyCertificateHostName(const TFtpsCertificateData & Data)
{
  UnicodeString HostName = FTerminal->SessionData->HostNameExpanded;

  UnicodeString CommonName = Data.Subject.CommonName;
  bool NoMask = CommonName.IsEmpty();
  bool Result = !NoMask && VerifyNameMask(HostName, CommonName);
  if (Result)
  {
    FTerminal->LogEvent(FORMAT(L"Certificate common name \"%s\" matches hostname", (CommonName)));
  }
  else
  {
    if (!NoMask && (FTerminal->Configuration->ActualLogProtocol >= 1))
    {
      FTerminal->LogEvent(FORMAT(L"Certificate common name \"%s\" does not match hostname", (CommonName)));
    }
    UnicodeString SubjectAltName = Data.SubjectAltName;
    while (!Result && !SubjectAltName.IsEmpty())
    {
      UnicodeString Entry = CutToChar(SubjectAltName, L',', true);
      UnicodeString EntryName = CutToChar(Entry, L':', true);
      if (SameText(EntryName, L"DNS"))
      {
        NoMask = false;
        Result = VerifyNameMask(HostName, Entry);
        if (Result)
        {
          FTerminal->LogEvent(FORMAT(L"Certificate subject alternative name \"%s\" matches hostname", (Entry)));
        }
        else
        {
          if (FTerminal->Configuration->ActualLogProtocol >= 1)
          {
            FTerminal->LogEvent(FORMAT(L"Certificate subject alternative name \"%s\" does not match hostname", (Entry)));
          }
        }
      }
    }
  }
  if (!Result && NoMask)
  {
    FTerminal->LogEvent(L"Certificate has no common name nor subject alternative name, not verifying hostname");
    Result = true;
  }
  return Result;
}
//---------------------------------------------------------------------------
static bool __fastcall IsIPAddress(const UnicodeString & HostName)
{
  bool IPv4 = true;
  bool IPv6 = true;
  bool AnyColon = false;

  for (int Index = 1; Index <= HostName.Length(); Index++)
  {
    wchar_t C = HostName[Index];
    if (!IsDigit(C) && (C != L'.'))
    {
      IPv4 = false;
    }
    if (!IsHex(C) && (C != L':'))
    {
      IPv6 = false;
    }
    if (C == L':')
    {
      AnyColon = true;
    }
  }

  return IPv4 || (IPv6 && AnyColon);
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleAsynchRequestVerifyCertificate(
  const TFtpsCertificateData & Data, int & RequestResult)
{
  if (!FActive)
  {
    return false;
  }
  else
  {
    FSessionInfo.CertificateFingerprintSHA1 =
      BytesToHex(RawByteString((const char*)Data.HashSha1, Data.HashSha1Len), false, L':');
    FSessionInfo.CertificateFingerprintSHA256 =
      BytesToHex(RawByteString((const char*)Data.HashSha256, Data.HashSha256Len), false, L':');

    if (FTerminal->SessionData->FingerprintScan)
    {
      RequestResult = 0;
    }
    else
    {
      UnicodeString CertificateSubject = Data.Subject.Organization;
      FTerminal->LogEvent(FORMAT(L"Verifying certificate for \"%s\" with fingerprint %s and %d failures", (CertificateSubject, FSessionInfo.CertificateFingerprintSHA256, Data.VerificationResult)));

      bool Trusted = false;
      bool TryWindowsSystemCertificateStore = false;
      UnicodeString VerificationResultStr;
      switch (Data.VerificationResult)
      {
        case X509_V_OK:
          Trusted = true;
          break;
        case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
          VerificationResultStr = LoadStr(CERT_ERR_UNABLE_TO_GET_ISSUER_CERT);
          TryWindowsSystemCertificateStore = true;
          break;
        case X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE:
          VerificationResultStr = LoadStr(CERT_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE);
          break;
        case X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY:
          VerificationResultStr = LoadStr(CERT_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY);
          break;
        case X509_V_ERR_CERT_SIGNATURE_FAILURE:
          VerificationResultStr = LoadStr(CERT_ERR_CERT_SIGNATURE_FAILURE);
          break;
        case X509_V_ERR_CERT_NOT_YET_VALID:
          VerificationResultStr = LoadStr(CERT_ERR_CERT_NOT_YET_VALID);
          break;
        case X509_V_ERR_CERT_HAS_EXPIRED:
          VerificationResultStr = LoadStr(CERT_ERR_CERT_HAS_EXPIRED);
          break;
        case X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
          VerificationResultStr = LoadStr(CERT_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD);
          break;
        case X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
          VerificationResultStr = LoadStr(CERT_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD);
          break;
        case X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT:
          VerificationResultStr = LoadStr(CERT_ERR_DEPTH_ZERO_SELF_SIGNED_CERT);
          TryWindowsSystemCertificateStore = true;
          break;
        case X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN:
          VerificationResultStr = LoadStr(CERT_ERR_SELF_SIGNED_CERT_IN_CHAIN);
          TryWindowsSystemCertificateStore = true;
          break;
        case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY:
          VerificationResultStr = LoadStr(CERT_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY);
          TryWindowsSystemCertificateStore = true;
          break;
        case X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE:
          VerificationResultStr = LoadStr(CERT_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE);
          TryWindowsSystemCertificateStore = true;
          break;
        case X509_V_ERR_INVALID_CA:
          VerificationResultStr = LoadStr(CERT_ERR_INVALID_CA);
          break;
        case X509_V_ERR_PATH_LENGTH_EXCEEDED:
          VerificationResultStr = LoadStr(CERT_ERR_PATH_LENGTH_EXCEEDED);
          break;
        case X509_V_ERR_INVALID_PURPOSE:
          VerificationResultStr = LoadStr(CERT_ERR_INVALID_PURPOSE);
          break;
        case X509_V_ERR_CERT_UNTRUSTED:
          VerificationResultStr = LoadStr(CERT_ERR_CERT_UNTRUSTED);
          TryWindowsSystemCertificateStore = true;
          break;
        case X509_V_ERR_CERT_REJECTED:
          VerificationResultStr = LoadStr(CERT_ERR_CERT_REJECTED);
          break;
        case X509_V_ERR_KEYUSAGE_NO_CERTSIGN:
          VerificationResultStr = LoadStr(CERT_ERR_KEYUSAGE_NO_CERTSIGN);
          break;
        case X509_V_ERR_CERT_CHAIN_TOO_LONG:
          VerificationResultStr = LoadStr(CERT_ERR_CERT_CHAIN_TOO_LONG);
          break;
        default:
          VerificationResultStr =
            FORMAT(L"%s (%s)",
              (LoadStr(CERT_ERR_UNKNOWN), X509_verify_cert_error_string(Data.VerificationResult)));
          break;
      }

      bool IsHostNameIPAddress = IsIPAddress(FTerminal->SessionData->HostNameExpanded);
      bool CertificateHostNameVerified = !IsHostNameIPAddress && VerifyCertificateHostName(Data);

      bool VerificationResult = Trusted;

      if (IsHostNameIPAddress || !CertificateHostNameVerified)
      {
        VerificationResult = false;
        TryWindowsSystemCertificateStore = false;
      }

      if (!VerificationResult)
      {
        if (FTerminal->VerifyCertificate(FtpsCertificateStorageKey, FTerminal->SessionData->SiteKey,
              FSessionInfo.CertificateFingerprintSHA1, FSessionInfo.CertificateFingerprintSHA256,
              CertificateSubject, Data.VerificationResult))
        {
          // certificate is trusted, but for not purposes of info dialog
          VerificationResult = true;
          FSessionInfo.CertificateVerifiedManually = true;
        }
      }

      // TryWindowsSystemCertificateStore is set for the same set of failures
      // as trigger NE_SSL_UNTRUSTED flag in ne_openssl.c's verify_callback().
      // Use WindowsValidateCertificate only as a last resort (after checking the cached fingerprint)
      // as it can take a very long time (up to 1 minute).
      if (!VerificationResult && TryWindowsSystemCertificateStore)
      {
        UnicodeString WindowsCertificateError;
        if (WindowsValidateCertificate(Data.Certificate, Data.CertificateLen, WindowsCertificateError))
        {
          FTerminal->LogEvent(L"Certificate verified against Windows certificate store");
          VerificationResult = true;
          // certificate is trusted for all purposes
          Trusted = true;
        }
        else
        {
          FTerminal->LogEvent(
            FORMAT(L"Certificate failed to verify against Windows certificate store: %s", (DefaultStr(WindowsCertificateError, L"no details"))));
        }
      }

      const UnicodeString SummarySeparator = L"\n\n";
      UnicodeString Summary;
      // even if the fingerprint is cached, the certificate is still not trusted for a purposes of the info dialog.
      if (!Trusted)
      {
        AddToList(Summary, VerificationResultStr + L" " + FMTLOAD(CERT_ERRDEPTH, (Data.VerificationDepth + 1)), SummarySeparator);
      }

      if (IsHostNameIPAddress)
      {
        AddToList(Summary, FMTLOAD(CERT_IP_CANNOT_VERIFY, (FTerminal->SessionData->HostNameExpanded)), SummarySeparator);
      }
      else if (!CertificateHostNameVerified)
      {
        AddToList(Summary, FMTLOAD(CERT_NAME_MISMATCH, (FTerminal->SessionData->HostNameExpanded)), SummarySeparator);
      }

      if (Summary.IsEmpty())
      {
        Summary = LoadStr(CERT_OK);
      }

      FSessionInfo.Certificate =
        FMTLOAD(CERT_TEXT2, (
          FormatContact(Data.Issuer),
          FormatContact(Data.Subject),
          FormatValidityTime(Data.ValidFrom),
          FormatValidityTime(Data.ValidUntil),
          FSessionInfo.CertificateFingerprintSHA256,
          FSessionInfo.CertificateFingerprintSHA1,
          Summary));

      RequestResult = VerificationResult ? 1 : 0;

      if (RequestResult == 0)
      {
        if (FTerminal->ConfirmCertificate(FSessionInfo, Data.VerificationResult, FtpsCertificateStorageKey, true))
        {
          // FZ's VerifyCertDlg.cpp returns 2 for "cached", what we do nto distinguish here,
          // however FZAPI takes all non-zero values equally.
          RequestResult = 1;
          FSessionInfo.CertificateVerifiedManually = true;
        }
      }
    }

    return true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleAsynchRequestNeedPass(
  struct TNeedPassRequestData & Data, int & RequestResult)
{
  if (!FActive)
  {
    return false;
  }
  else
  {
    UnicodeString Password;
    if (FCertificate != NULL)
    {
      FTerminal->LogEvent(L"Server asked for password, but we are using certificate, and no password was specified upfront, using fake password");
      Password = L"USINGCERT";
      RequestResult = TFileZillaIntf::REPLY_OK;
    }
    else
    {
      if (FTerminal->PromptUser(FTerminal->SessionData, pkPassword, LoadStr(PASSWORD_TITLE), L"",
        LoadStr(PASSWORD_PROMPT), false, 0, Password))
      {
        RequestResult = TFileZillaIntf::REPLY_OK;
        if (!Password.IsEmpty())
        {
          FAnyPassword = true;
        }
      }
      else
      {
        RequestResult = TFileZillaIntf::REPLY_ABORTED;
      }
    }

    // When returning REPLY_OK, we need to return an allocated password,
    // even if we were returning and empty string we got on input.
    if (RequestResult == TFileZillaIntf::REPLY_OK)
    {
      Data.Password = _wcsdup(Password.c_str());
    }

    return true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::RemoteFileTimeToDateTimeAndPrecision(const TRemoteFileTime & Source, TDateTime & DateTime, TModificationFmt & ModificationFmt)
{
  // ModificationFmt must be set after Modification
  if (Source.HasDate)
  {
    DateTime =
      EncodeDateVerbose((unsigned short)Source.Year, (unsigned short)Source.Month,
        (unsigned short)Source.Day);
    if (Source.HasTime)
    {
      DateTime = DateTime +
        EncodeTimeVerbose((unsigned short)Source.Hour, (unsigned short)Source.Minute,
          (unsigned short)Source.Second, 0);
      ModificationFmt = Source.HasSeconds ? mfFull : (Source.HasYear ? mfYMDHM : mfMDHM);

      // With IIS, the Utc should be false only for MDTM
      if (FWindowsServer && !Source.Utc)
      {
        DateTime -= DSTDifferenceForTime(DateTime);
      }
    }
    else
    {
      ModificationFmt = mfMDY;
    }

    if (Source.Utc)
    {
      DateTime = ConvertTimestampFromUTC(DateTime);
    }

  }
  else
  {
    // With SCP we estimate date to be today, if we have at least time

    DateTime = double(0);
    ModificationFmt = mfNone;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleListData(const wchar_t * Path,
  const TListDataEntry * Entries, unsigned int Count)
{
  if (!FActive)
  {
    return false;
  }
  else if (FIgnoreFileList)
  {
    // directory listing provided implicitly by FZAPI during certain operations is ignored
    DebugAssert(FFileList == NULL);
    return false;
  }
  else
  {
    DebugAssert(FFileList != NULL);
    DebugUsedParam(Path);

    for (unsigned int Index = 0; Index < Count; Index++)
    {
      const TListDataEntry * Entry = &Entries[Index];
      std::unique_ptr<TRemoteFile> File(new TRemoteFile());
      try
      {
        File->Terminal = FTerminal;

        File->FileName = Entry->Name;
        try
        {
          int PermissionsLen = wcslen(Entry->Permissions);
          if (PermissionsLen >= 10)
          {
            File->Rights->Text = Entry->Permissions + 1;
          }
          else if ((PermissionsLen == 3) || (PermissionsLen == 4))
          {
            File->Rights->Octal = Entry->Permissions;
          }
        }
        catch(...)
        {
          // ignore permission errors with FTP
        }

        File->HumanRights = Entry->HumanPerm;

        // deprecated, to be replaced with Owner/Group
        if (wcslen(Entry->OwnerGroup) > 0)
        {
          const wchar_t * Space = wcschr(Entry->OwnerGroup, L' ');
          if (Space != NULL)
          {
            File->Owner.Name = UnicodeString(Entry->OwnerGroup, Space - Entry->OwnerGroup);
            File->Group.Name = Space + 1;
          }
          else
          {
            File->Owner.Name = Entry->OwnerGroup;
          }
        }
        else
        {
          File->Owner.Name = Entry->Owner;
          File->Group.Name = Entry->Group;
        }

        File->Size = Entry->Size;

        if (Entry->Link)
        {
          File->Type = FILETYPE_SYMLINK;
        }
        else if (Entry->Dir)
        {
          File->Type = FILETYPE_DIRECTORY;
        }
        else
        {
          File->Type = FILETYPE_DEFAULT;
        }

        TDateTime Modification;
        TModificationFmt ModificationFmt;
        RemoteFileTimeToDateTimeAndPrecision(Entry->Time, Modification, ModificationFmt);
        File->Modification = Modification;
        File->ModificationFmt = ModificationFmt;
        File->LastAccess = File->Modification;

        File->LinkTo = Entry->LinkTarget;

        File->Complete();
      }
      catch (Exception & E)
      {
        UnicodeString EntryData =
          FORMAT(L"%s/%s/%s/%s/%s/%s/%s/%d/%d/%d/%d/%d/%d/%d/%d/%d/%d/%d",
            (Entry->Name, Entry->Permissions, Entry->HumanPerm, Entry->Owner, Entry->Group, Entry->OwnerGroup, IntToStr(Entry->Size),
             int(Entry->Dir), int(Entry->Link), Entry->Time.Year, Entry->Time.Month, Entry->Time.Day,
             Entry->Time.Hour, Entry->Time.Minute, int(Entry->Time.HasTime),
             int(Entry->Time.HasYear), int(Entry->Time.HasSeconds), int(Entry->Time.HasDate)));
        throw ETerminal(&E, FMTLOAD(LIST_LINE_ERROR, (EntryData)), HELP_LIST_LINE_ERROR);
      }

      if (FTerminal->IsValidFile(File.get()))
      {
        FFileList->AddFile(File.release());
      }
    }
    return true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleTransferStatus(bool Valid, __int64 TransferSize,
  __int64 Bytes, bool FileTransfer)
{
  if (!FActive)
  {
    return false;
  }
  else if (!Valid)
  {
  }
  else if (FileTransfer)
  {
    FileTransferProgress(TransferSize, Bytes);
  }
  else
  {
    ReadDirectoryProgress(Bytes);
  }
  return true;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleReply(int Command, unsigned int Reply)
{
  if (!FActive)
  {
    return false;
  }
  else
  {
    if (FTerminal->Configuration->ActualLogProtocol >= 1)
    {
      FTerminal->LogEvent(FORMAT(L"Got reply %x to the command %d", (int(Reply), Command)));
    }

    // reply with Command 0 is not associated with current operation
    // so do not treat it as a reply
    // (it is typically used asynchronously to notify about disconnects)
    if (Command != 0)
    {
      DebugAssert(FCommandReply == 0);
      FCommandReply = Reply;
    }
    else
    {
      DebugAssert(FReply == 0);
      FReply = Reply;
    }
    return true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleCapabilities(
  TFTPServerCapabilities * ServerCapabilities)
{
  FServerCapabilities->Assign(ServerCapabilities);
  FFileSystemInfoValid = false;
  return true;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::CheckError(int ReturnCode, const wchar_t * Context)
{
  // we do not expect any FZAPI call to fail as it generally can fail only due to:
  // - invalid parameters
  // - busy FZAPI core
  // the only exception is REPLY_NOTCONNECTED that can happen if
  // connection is closed just between the last call to Idle()
  // and call to any FZAPI command
  // in such case reply without associated command is posted,
  // which we are going to wait for unless we are already waiting
  // on higher level (this typically happens if connection is lost while
  // waiting for user interaction and is detected within call to
  // SetAsyncRequestResult)
  if (FLAGSET(ReturnCode, TFileZillaIntf::REPLY_NOTCONNECTED))
  {
    if (!FWaitingForReply)
    {
      // throws
      WaitForFatalNonCommandReply();
    }
  }
  else
  {
    FTerminal->FatalError(NULL,
      FMTLOAD(INTERNAL_ERROR, (FORMAT(L"fz#%s", (Context)), IntToHex(ReturnCode, 4))));
    DebugFail();
  }

  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::Unquote(UnicodeString & Str)
{
  enum
  {
    INIT,
    QUOTE,
    QUOTED,
    DONE
  } State;

  State = INIT;
  DebugAssert((Str.Length() > 0) && ((Str[1] == L'"') || (Str[1] == L'\'')));

  int Index = 1;
  wchar_t Quote = wchar_t(); // shut up
  while (Index <= Str.Length())
  {
    switch (State)
    {
      case INIT:
        if ((Str[Index] == L'"') || (Str[Index] == L'\''))
        {
          Quote = Str[Index];
          State = QUOTED;
          Str.Delete(Index, 1);
        }
        else
        {
          DebugFail();
          // no quoted string
          Str.SetLength(0);
        }
        break;

      case QUOTED:
        if (Str[Index] == Quote)
        {
          State = QUOTE;
          Str.Delete(Index, 1);
        }
        else
        {
          Index++;
        }
        break;

      case QUOTE:
        if (Str[Index] == Quote)
        {
          Index++;
        }
        else
        {
          // end of quoted string, trim the rest
          Str.SetLength(Index - 1);
          State = DONE;
        }
        break;
    }
  }

  return (State == DONE);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::PreserveDownloadFileTime(HANDLE Handle, void * UserData)
{
  TFileTransferData * Data = static_cast<TFileTransferData *>(UserData);
  DebugAssert(Data->CopyParam->OnTransferOut == NULL);
  FTerminal->UpdateTargetTime(Handle, Data->Modification, mfFull, dstmUnix);
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::GetFileModificationTimeInUtc(const wchar_t * FileName, struct tm & Time)
{
  bool Result;
  try
  {
    // error-handling-free and DST-mode-unaware copy of TTerminal::OpenLocalFile
    HANDLE Handle = CreateFile(ApiPath(FileName).c_str(), GENERIC_READ,
      FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, 0);
    if (Handle == INVALID_HANDLE_VALUE)
    {
      Result = false;
    }
    else
    {
      FILETIME MTime;
      if (!GetFileTime(Handle, NULL, NULL, &MTime))
      {
        Result = false;
      }
      else
      {
        TDateTime Modification = ConvertTimestampToUTC(FileTimeToDateTime(MTime));

        unsigned short Year;
        unsigned short Month;
        unsigned short Day;
        Modification.DecodeDate(&Year, &Month, &Day);
        Time.tm_year = Year - 1900;
        Time.tm_mon = Month - 1;
        Time.tm_mday = Day;

        unsigned short Hour;
        unsigned short Min;
        unsigned short Sec;
        unsigned short MSec;
        Modification.DecodeTime(&Hour, &Min, &Sec, &MSec);
        Time.tm_hour = Hour;
        Time.tm_min = Min;
        Time.tm_sec = Sec;

        Result = true;
      }

      CloseHandle(Handle);
    }
  }
  catch (...)
  {
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::RegisterChecksumAlgCommand(const UnicodeString & Alg, const UnicodeString & Command)
{
  FChecksumAlgs->Add(Alg);
  FChecksumCommands->Add(Command);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::GetSupportedChecksumAlgs(TStrings * Algs)
{
  for (int Index = 0; Index < FHashAlgs->Count; Index++)
  {
    Algs->Add(FHashAlgs->Strings[Index]);
  }

  for (int Index = 0; Index < FChecksumAlgs->Count; Index++)
  {
    UnicodeString Alg = FChecksumAlgs->Strings[Index];
    UnicodeString Command = FChecksumCommands->Strings[Index];

    if (SupportsCommand(Command) && (Algs->IndexOf(Alg) < 0))
    {
      Algs->Add(Alg);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::SupportsSiteCommand(const UnicodeString & Command) const
{
  return (FSupportedSiteCommands->IndexOf(Command) >= 0);
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::SupportsCommand(const UnicodeString & Command) const
{
  return (FSupportedCommands->IndexOf(Command) >= 0);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::LockFile(const UnicodeString & /*FileName*/, const TRemoteFile * /*File*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::UnlockFile(const UnicodeString & /*FileName*/, const TRemoteFile * /*File*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::UpdateFromMain(TCustomFileSystem * /*MainFileSystem*/)
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ClearCaches()
{
  // noop
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetOpenSSLVersionText()
{
  return OPENSSL_VERSION_TEXT;
}
//---------------------------------------------------------------------------
