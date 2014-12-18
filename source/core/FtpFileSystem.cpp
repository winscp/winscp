//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#ifndef NO_FILEZILLA
//---------------------------------------------------------------------------
#include <list>
#define MPEXT
#include "FtpFileSystem.h"
#include "FileZillaIntf.h"

#include "Common.h"
#include "Exceptions.h"
#include "Terminal.h"
#include "TextsCore.h"
#include "TextsFileZilla.h"
#include "HelpCore.h"
#include "Security.h"
#include <StrUtils.hpp>
#include <DateUtils.hpp>
#include <openssl/x509_vfy.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_TERMINAL FTerminal
//---------------------------------------------------------------------------
const int DummyCodeClass = 8;
const int DummyTimeoutCode = 801;
const int DummyCancelCode = 802;
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
    __int64 Bytes, int Percent, int TimeElapsed, int TimeLeft, int TransferRate,
    bool FileTransfer);
  virtual bool __fastcall HandleReply(int Command, unsigned int Reply);
  virtual bool __fastcall HandleCapabilities(TFTPServerCapabilities * ServerCapabilities);
  virtual bool __fastcall CheckError(int ReturnCode, const wchar_t * Context);

  virtual void PreserveDownloadFileTime(HANDLE Handle, void * UserData);
  virtual bool GetFileModificationTimeInUtc(const wchar_t * FileName, struct tm & Time);
  virtual wchar_t * LastSysErrorMessage();

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
  __int64 Bytes, int Percent, int TimeElapsed, int TimeLeft, int TransferRate,
  bool FileTransfer)
{
  return FFileSystem->HandleTransferStatus(Valid, TransferSize, Bytes, Percent,
    TimeElapsed, TimeLeft, TransferRate, FileTransfer);
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
const int tfFirstLevel = 0x01;
const int tfAutoResume = 0x02;
const wchar_t CertificateStorageKey[] = L"FtpsCertificates";
const UnicodeString SiteCommand(L"SITE");
const UnicodeString SymlinkSiteCommand(L"SYMLINK");
const UnicodeString CopySiteCommand(L"COPY");
const UnicodeString HashCommand(L"HASH");
const UnicodeString AvblCommand(L"AVBL");
const UnicodeString XQuotaCommand(L"XQUOTA");
//---------------------------------------------------------------------------
struct TSinkFileParams
{
  UnicodeString TargetDir;
  const TCopyParamType * CopyParam;
  int Params;
  TFileOperationProgressType * OperationProgress;
  bool Skipped;
  unsigned int Flags;
};
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
  FQueueEvent(CreateEvent(NULL, true, false, NULL)),
  FQueue(new TMessageQueue),
  FReply(0),
  FCommandReply(0),
  FLastCommand(CMD_UNKNOWN),
  FLastResponse(new TStringList()),
  FLastErrorResponse(new TStringList()),
  FLastError(new TStringList()),
  FFeatures(new TStringList()),
  FFileList(NULL),
  FFileListCache(NULL),
  FActive(false),
  FOpening(false),
  FWaitingForReply(false),
  FIgnoreFileList(false),
  FOnCaptureOutput(NULL),
  FFileSystemInfoValid(false),
  FDoListAll(false),
  FServerCapabilities(NULL)
{
  ResetReply();

  FListAll = FTerminal->SessionData->FtpListAll;
  FFileSystemInfo.ProtocolBaseName = L"FTP";
  FFileSystemInfo.ProtocolName = FFileSystemInfo.ProtocolBaseName;
  FTimeoutStatus = LoadStr(IDS_ERRORMSG_TIMEOUT);
  FDisconnectStatus = LoadStr(IDS_STATUSMSG_DISCONNECTED);
  FServerCapabilities = new TFTPServerCapabilities();
  FHashAlgs.reset(new TStringList());
  FSupportedCommands.reset(CreateSortedStringList());
  FSupportedSiteCommands.reset(CreateSortedStringList());

  FChecksumAlgs.reset(new TStringList());
  FChecksumCommands.reset(new TStringList());
  RegisterChecksumAlgCommand(Sha1ChecksumAlg, L"XSHA1");
  RegisterChecksumAlgCommand(Sha256ChecksumAlg, L"XSHA256");
  RegisterChecksumAlgCommand(Sha512ChecksumAlg, L"XSHA512");
  RegisterChecksumAlgCommand(Md5ChecksumAlg, L"XMD5");
  RegisterChecksumAlgCommand(Crc32ChecksumAlg, L"XCRC");
}
//---------------------------------------------------------------------------
__fastcall TFTPFileSystem::~TFTPFileSystem()
{
  assert(FFileList == NULL);

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
  FCurrentDirectory = L"";
  FHomeDirectory = L"";

  TSessionData * Data = FTerminal->SessionData;

  FSessionInfo.LoginTime = Now();

  switch (Data->Ftps)
  {
    case ftpsNone:
      // noop;
      break;

    case ftpsImplicit:
      FSessionInfo.SecurityProtocolName = LoadStr(FTPS_IMPLICIT);
      break;

    case ftpsExplicitSsl:
    case ftpsExplicitTls:
      FSessionInfo.SecurityProtocolName = LoadStr(FTPS_EXPLICIT);
      break;

    default:
      FAIL;
      break;
  }

  FLastDataSent = Now();

  FMultineResponse = false;

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
        case 0:
        case 1:
          LogLevel = TFileZillaIntf::LOG_WARNING;
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

  UnicodeString HostName = Data->HostNameExpanded;
  UnicodeString UserName = Data->UserNameExpanded;
  UnicodeString Password = Data->Password;
  UnicodeString Account = Data->FtpAccount;
  UnicodeString Path = Data->RemoteDirectory;
  int ServerType;
  switch (Data->Ftps)
  {
    case ftpsImplicit:
      ServerType = TFileZillaIntf::SERVER_FTP_SSL_IMPLICIT;
      break;

    case ftpsExplicitSsl:
      ServerType = TFileZillaIntf::SERVER_FTP_SSL_EXPLICIT;
      break;

    case ftpsExplicitTls:
      ServerType = TFileZillaIntf::SERVER_FTP_TLS_EXPLICIT;
      break;

    default:
      assert(Data->Ftps == ftpsNone);
      ServerType = TFileZillaIntf::SERVER_FTP;
      break;
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
  bool PromptedForCredentials = false;

  do
  {
    FDetectTimeDifference = Data->TimeDifferenceAuto;
    FTimeDifference = 0;
    ResetFeatures();
    FSystem = L"";
    FWelcomeMessage = L"";
    FFileSystemInfoValid = false;

    // TODO: the same for account? it ever used?

    // ask for username if it was not specified in advance, even on retry,
    // but keep previous one as default,
    if (Data->UserNameExpanded.IsEmpty())
    {
      FTerminal->LogEvent(L"Username prompt (no username provided)");

      if (!PromptedForCredentials)
      {
        FTerminal->Information(LoadStr(FTP_CREDENTIAL_PROMPT), false);
        PromptedForCredentials = true;
      }

      if (!FTerminal->PromptUser(Data, pkUserName, LoadStr(USERNAME_TITLE), L"",
            LoadStr(USERNAME_PROMPT2), true, 0, UserName))
      {
        FTerminal->FatalError(NULL, LoadStr(AUTHENTICATION_FAILED));
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
        FTerminal->FatalError(NULL, LoadStr(AUTHENTICATION_FAILED));
      }
    }

    FPasswordFailed = false;
    TAutoFlag OpeningFlag(FOpening);

    FActive = FFileZillaIntf->Connect(
      HostName.c_str(), Data->PortNumber, UserName.c_str(),
      Password.c_str(), Account.c_str(), false, Path.c_str(),
      ServerType, Pasv, TimeZoneOffset, UTF8, Data->FtpForcePasvIp,
      Data->FtpUseMlsd);

    assert(FActive);

    try
    {
      // do not wait for FTP response code as Connect is complex operation
      GotReply(WaitForCommandReply(false), REPLY_CONNECT, LoadStr(CONNECTION_FAILED));

      Shred(Password);

      // we have passed, even if we got 530 on the way (if it is possible at all),
      // ignore it
      assert(!FPasswordFailed);
      FPasswordFailed = false;
    }
    catch(...)
    {
      if (FPasswordFailed)
      {
        FTerminal->Information(LoadStr(FTP_ACCESS_DENIED), false);
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

  // see also TWebDAVFileSystem::CollectTLSSessionInfo()
  FSessionInfo.CSCipher = FFileZillaIntf->GetCipherName().c_str();
  FSessionInfo.SCCipher = FSessionInfo.CSCipher;
  UnicodeString TlsVersionStr = FFileZillaIntf->GetTlsVersionStr().c_str();
  AddToList(FSessionInfo.SecurityProtocolName, TlsVersionStr, L", ");
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Close()
{
  assert(FActive);
  bool Result;
  if (FFileZillaIntf->Close(FOpening))
  {
    CHECK(FLAGSET(WaitForCommandReply(false), TFileZillaIntf::REPLY_DISCONNECTED));
    Result = true;
  }
  else
  {
    // See TFileZillaIntf::Close
    Result = FOpening;
  }

  if (ALWAYS_TRUE(Result))
  {
    assert(FActive);
    Discard();
    FTerminal->Closed();
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
      FAIL;
      break;
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

  // 220-FileZilla Server version 0.9.43 beta
  // 220-written by Tim Kosse (Tim.Kosse@gmx.de)
  // 220 Please visit http://sourceforge.net/projects/filezilla/
  // SYST
  // 215 UNIX emulated by FileZilla
  // (Welcome message is configurable)
  if (ContainsText(FSystem, L"FileZilla"))
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
  else if (ContainsText(FWelcomeMessage, L"Microsoft FTP Service"))
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
  // 220 Welcome to the most popular FTP hosting service! Save on hardware, software, hosting and admin. Share files/folders with read-write permission. Visit http://www.drivehq.com/ftp/;
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
  else
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsFTPOther");
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Idle()
{
  if (FActive && !FWaitingForReply)
  {
    PoolForFatalNonCommandReply();

    // Keep session alive
    if ((FTerminal->SessionData->FtpPingType != ptOff) &&
        (double(Now() - FLastDataSent) > double(FTerminal->SessionData->FtpPingIntervalDT) * 4))
    {
      FLastDataSent = Now();

      TRemoteDirectory * Files = new TRemoteDirectory(FTerminal);
      try
      {
        try
        {
          Files->Directory = CurrentDirectory;
          DoReadDirectory(Files);
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
      __finally
      {
        delete Files;
      }
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
  assert(FActive);
  FActive = false;
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
    if (!UnixSamePath(ActualCurrentDirectory(), FCurrentDirectory))
    {
      FTerminal->LogEvent(FORMAT(L"Synchronizing current directory \"%s\".",
        (FCurrentDirectory)));
      DoChangeDirectory(FCurrentDirectory);
      // make sure FZAPI is aware that we changed current working directory
      FFileZillaIntf->SetCurrentPath(FCurrentDirectory.c_str());
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::AnyCommand(const UnicodeString Command,
  TCaptureOutputEvent OutputEvent)
{
  // end-user has right to expect that client current directory is really
  // current directory for the server
  EnsureLocation();

  assert(FOnCaptureOutput == NULL);
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
void __fastcall TFTPFileSystem::DoChangeDirectory(const UnicodeString & Directory)
{
  UnicodeString Command = FORMAT(L"CWD %s", (Directory));
  SendCommand(Command);

  GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
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
  FCurrentDirectory = L"";
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CachedChangeDirectory(const UnicodeString Directory)
{
  FCurrentDirectory = UnixExcludeTrailingBackslash(Directory);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ChangeFileProperties(const UnicodeString AFileName,
  const TRemoteFile * File, const TRemoteProperties * Properties,
  TChmodSessionAction & Action)
{
  assert(Properties);
  assert(!Properties->Valid.Contains(vpGroup));
  assert(!Properties->Valid.Contains(vpOwner));
  assert(!Properties->Valid.Contains(vpLastAccess));
  assert(!Properties->Valid.Contains(vpModification));

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

      if ((File != NULL) && File->IsDirectory && !File->IsSymLink && Properties->Recursive)
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
      UnicodeString FilePath = UnixExtractFilePath(FileName);
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
  FAIL;
  return false;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::DoCalculateFileChecksum(
  bool UsingHashCommand, const UnicodeString & Alg, TRemoteFile * File)
{
  UnicodeString CommandName;

  if (UsingHashCommand)
  {
    CommandName = HashCommand;
  }
  else
  {
    int Index = FChecksumAlgs->IndexOf(Alg);
    if (Index < 0)
    {
      FAIL;
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
  UnicodeString ResponseText = GotReply(WaitForCommandReply(), REPLY_2XX_CODE | REPLY_SINGLE_LINE);

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
  else // All X<hash> commands
  {
    // Accepting any 2xx response. Most servers use 213,
    // but for example WS_FTP uses non-sense code 220 (Service ready for new user)
    Hash = ResponseText;
  }

  if (Hash.IsEmpty())
  {
    throw Exception(FMTLOAD(FTP_RESPONSE_ERROR, (CommandName, ResponseText)));
  }

  return LowerCase(Hash);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoCalculateFilesChecksum(bool UsingHashCommand,
  const UnicodeString & Alg, TStrings * FileList, TStrings * Checksums,
  TCalculatedChecksumEvent OnCalculatedChecksum,
  TFileOperationProgressType * OperationProgress, bool FirstLevel)
{
  TOnceDoneOperation OnceDoneOperation; // not used

  int Index = 0;
  while ((Index < FileList->Count) && !OperationProgress->Cancel)
  {
    TRemoteFile * File = (TRemoteFile *)FileList->Objects[Index];
    assert(File != NULL);

    if (File->IsDirectory)
    {
      if (!File->IsSymLink &&
          !File->IsParentDirectory && !File->IsThisDirectory &&
          // recurse into subdirectories only if we have callback function
          (OnCalculatedChecksum != NULL))
      {
        OperationProgress->SetFile(File->FileName);
        TRemoteFileList * SubFiles =
          FTerminal->CustomReadDirectoryListing(File->FullFileName, false);

        if (SubFiles != NULL)
        {
          TStrings * SubFileList = new TStringList();
          bool Success = false;
          try
          {
            OperationProgress->SetFile(File->FileName);

            for (int Index = 0; Index < SubFiles->Count; Index++)
            {
              TRemoteFile * SubFile = SubFiles->Files[Index];
              SubFileList->AddObject(SubFile->FullFileName, SubFile);
            }

            // do not collect checksums for files in subdirectories,
            // only send back checksums via callback
            DoCalculateFilesChecksum(UsingHashCommand, Alg, SubFileList, NULL,
              OnCalculatedChecksum, OperationProgress, false);

            Success = true;
          }
          __finally
          {
            delete SubFiles;
            delete SubFileList;

            if (FirstLevel)
            {
              OperationProgress->Finish(File->FileName, Success, OnceDoneOperation);
            }
          }
        }
      }
    }
    else
    {
      TChecksumSessionAction Action(FTerminal->ActionLog);
      try
      {
        OperationProgress->SetFile(File->FileName);
        Action.FileName(FTerminal->AbsolutePath(File->FullFileName, true));

        UnicodeString Checksum = DoCalculateFileChecksum(UsingHashCommand, Alg, File);

        if (OnCalculatedChecksum != NULL)
        {
          OnCalculatedChecksum(File->FileName, Alg, Checksum);
        }
        Action.Checksum(Alg, Checksum);
        if (Checksums != NULL)
        {
          Checksums->Add(Checksum);
        }
      }
      catch (Exception & E)
      {
        FTerminal->RollbackAction(Action, OperationProgress, &E);

        // Error formatting expanded from inline to avoid strange exceptions
        UnicodeString Error =
          FMTLOAD(CHECKSUM_ERROR,
            (File != NULL ? File->FullFileName : UnicodeString(L"")));
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
void __fastcall TFTPFileSystem::CalculateFilesChecksum(const UnicodeString & Alg,
  TStrings * FileList, TStrings * Checksums,
  TCalculatedChecksumEvent OnCalculatedChecksum)
{
  TFileOperationProgressType Progress(&FTerminal->DoProgress, &FTerminal->DoFinished);
  Progress.Start(foCalculateChecksum, osRemote, FileList->Count);

  FTerminal->FOperationProgress = &Progress;

  try
  {
    UnicodeString NormalizedAlg = FindIdent(FindIdent(Alg, FHashAlgs.get()), FChecksumAlgs.get());

    bool UsingHashCommand = (FHashAlgs->IndexOf(NormalizedAlg) >= 0);
    if (UsingHashCommand)
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

    DoCalculateFilesChecksum(UsingHashCommand, NormalizedAlg, FileList, Checksums, OnCalculatedChecksum,
      &Progress, true);
  }
  __finally
  {
    FTerminal->FOperationProgress = NULL;
    Progress.Stop();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::ConfirmOverwrite(
  const UnicodeString & FullFileName, UnicodeString & FileName,
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
    // do nothing (dummy resume) when the files has the same size.
    // this is workaround for servers that strangely fails just after successful
    // upload.
    (DestIsSmaller || (DestIsSame && CanAutoResume));

  unsigned int Answer;
  if (CanAutoResume && CanResume)
  {
    if (DestIsSame)
    {
      assert(CanAutoResume);
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
    Aliases[1].Button = qaAll;
    Aliases[1].Alias = LoadStr(YES_TO_NEWER_BUTTON);
    Aliases[1].GroupWith = qaYes;
    Aliases[1].GrouppedShiftState = TShiftState() << ssCtrl;
    Aliases[2].Button = qaIgnore;
    Aliases[2].Alias = LoadStr(RENAME_BUTTON);
    Aliases[2].GroupWith = qaNo;
    Aliases[2].GrouppedShiftState = TShiftState() << ssCtrl;
    Aliases[3].Button = qaYesToAll;
    Aliases[3].GroupWith = qaYes;
    Aliases[3].GrouppedShiftState = TShiftState() << ssShift;
    Aliases[4].Button = qaNoToAll;
    Aliases[4].GroupWith = qaNo;
    Aliases[4].GrouppedShiftState = TShiftState() << ssShift;
    TQueryParams QueryParams(qpNeverAskAgainCheck);
    QueryParams.Aliases = Aliases;
    QueryParams.AliasesCount = LENOF(Aliases);

    {
      TSuspendFileOperationProgress Suspend(OperationProgress);
      Answer = FTerminal->ConfirmFileOverwrite(FullFileName, FileParams,
        Answers, &QueryParams,
        OperationProgress->Side == osLocal ? osRemote : osLocal,
        CopyParam, Params, OperationProgress);
    }
  }

  Result = true;

  switch (Answer)
  {
    // resume
    case qaRetry:
      OverwriteMode = omResume;
      assert(FileParams != NULL);
      assert(CanResume);
      FFileTransferResumed = FileParams->DestSize;
      break;

    // rename
    case qaIgnore:
      if (FTerminal->PromptUser(FTerminal->SessionData, pkFileName,
            LoadStr(RENAME_TITLE), L"", LoadStr(RENAME_PROMPT2), true, 0, FileName))
      {
        OverwriteMode = omOverwrite;
      }
      else
      {
        if (!OperationProgress->Cancel)
        {
          OperationProgress->Cancel = csCancel;
        }
        FFileTransferAbort = ftaCancel;
        Result = false;
      }
      break;

    case qaYes:
      OverwriteMode = omOverwrite;
      break;

    case qaCancel:
      if (!OperationProgress->Cancel)
      {
        OperationProgress->Cancel = csCancel;
      }
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
      FAIL;
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
  if (Progress - FLastReadDirectoryProgress >= 10)
  {
    bool Cancel = false;
    FLastReadDirectoryProgress = Progress;
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
    OperationProgress->AddResumed(FFileTransferResumed);
    FFileTransferResumed = 0;
  }

  __int64 Diff = Bytes - OperationProgress->TransferedSize;
  if (ALWAYS_TRUE(Diff >= 0))
  {
    OperationProgress->AddTransfered(Diff);
  }

  if (OperationProgress->Cancel == csCancel)
  {
    FFileTransferCancelled = true;
    FFileTransferAbort = ftaCancel;
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
  // Any reason we use separate field intead of directly using OperationProgress->CPSLimit?
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
    FFileZillaIntf->FileTransfer(LocalFile.c_str(), RemoteFile.c_str(),
      RemotePath.c_str(), Get, Size, Type, &UserData);
    // we may actually catch response code of the listing
    // command (when checking for existence of the remote file)
    unsigned int Reply = WaitForCommandReply();
    GotReply(Reply, FLAGMASK(FFileTransferCancelled, REPLY_ALLOW_CANCEL));
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(TRANSFER_ERROR, (FileName)));

  switch (FFileTransferAbort)
  {
    case ftaSkip:
      THROW_SKIP_FILE_NULL;

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
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CopyToLocal(TStrings * FilesToCopy,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  Params &= ~cpAppend;
  UnicodeString FullTargetDir = IncludeTrailingBackslash(TargetDir);

  int Index = 0;
  while (Index < FilesToCopy->Count && !OperationProgress->Cancel)
  {
    UnicodeString FileName = FilesToCopy->Strings[Index];
    const TRemoteFile * File = dynamic_cast<const TRemoteFile *>(FilesToCopy->Objects[Index]);
    bool Success = false;

    try
    {
      try
      {
        SinkRobust(AbsolutePath(FileName, false), File, FullTargetDir, CopyParam, Params,
          OperationProgress, tfFirstLevel);
        Success = true;
        FLastDataSent = Now();
      }
      catch(EScpSkipFile & E)
      {
        TSuspendFileOperationProgress Suspend(OperationProgress);
        if (!FTerminal->HandleException(&E))
        {
          throw;
        }
      }
    }
    __finally
    {
      OperationProgress->Finish(FileName, Success, OnceDoneOperation);
    }
    Index++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::SinkRobust(const UnicodeString FileName,
  const TRemoteFile * File, const UnicodeString TargetDir,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  // the same in TSFTPFileSystem

  TDownloadSessionAction Action(FTerminal->ActionLog);
  TRobustOperationLoop RobustLoop(FTerminal, OperationProgress);

  do
  {
    try
    {
      Sink(FileName, File, TargetDir, CopyParam, Params, OperationProgress,
        Flags, Action);
    }
    catch(Exception & E)
    {
      if (!RobustLoop.TryReopen(E))
      {
        FTerminal->RollbackAction(Action, OperationProgress, &E);
        throw;
      }
    }

    if (RobustLoop.ShouldRetry())
    {
      OperationProgress->RollbackTransfer();
      Action.Restart();
      assert(File != NULL);
      if (!File->IsDirectory)
      {
        // prevent overwrite confirmations
        Params |= cpNoConfirmation;
        Flags |= tfAutoResume;
      }
    }
  }
  while (RobustLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Sink(const UnicodeString FileName,
  const TRemoteFile * File, const UnicodeString TargetDir,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags,
  TDownloadSessionAction & Action)
{
  UnicodeString OnlyFileName = UnixExtractFileName(FileName);

  Action.FileName(FileName);

  TFileMasks::TParams MaskParams;
  assert(File);
  MaskParams.Size = File->Size;
  MaskParams.Modification = File->Modification;

  if (!CopyParam->AllowTransfer(FileName, osRemote, File->IsDirectory, MaskParams))
  {
    FTerminal->LogEvent(FORMAT(L"File \"%s\" excluded from transfer", (FileName)));
    THROW_SKIP_FILE_NULL;
  }

  if (CopyParam->SkipTransfer(FileName, File->IsDirectory))
  {
    OperationProgress->AddSkippedFileSize(File->Size);
    THROW_SKIP_FILE_NULL;
  }

  FTerminal->LogFileDetails(FileName, File->Modification, File->Size);

  OperationProgress->SetFile(FileName);

  UnicodeString DestFileName = CopyParam->ChangeFileName(OnlyFileName,
    osRemote, FLAGSET(Flags, tfFirstLevel));
  UnicodeString DestFullName = TargetDir + DestFileName;

  if (File->IsDirectory)
  {
    Action.Cancel();
    if (!File->IsSymLink)
    {
      FILE_OPERATION_LOOP_BEGIN
      {
        int Attrs = FileGetAttr(ApiPath(DestFullName));
        if (FLAGCLEAR(Attrs, faDirectory))
        {
          EXCEPTION;
        }
      }
      FILE_OPERATION_LOOP_END(FMTLOAD(NOT_DIRECTORY_ERROR, (DestFullName)));

      FILE_OPERATION_LOOP_BEGIN
      {
        THROWOSIFFALSE(ForceDirectories(ApiPath(DestFullName)));
      }
      FILE_OPERATION_LOOP_END(FMTLOAD(CREATE_DIR_ERROR, (DestFullName)));

      TSinkFileParams SinkFileParams;
      SinkFileParams.TargetDir = IncludeTrailingBackslash(DestFullName);
      SinkFileParams.CopyParam = CopyParam;
      SinkFileParams.Params = Params;
      SinkFileParams.OperationProgress = OperationProgress;
      SinkFileParams.Skipped = false;
      SinkFileParams.Flags = Flags & ~(tfFirstLevel | tfAutoResume);

      FTerminal->ProcessDirectory(FileName, SinkFile, &SinkFileParams);

      // Do not delete directory if some of its files were skip.
      // Throw "skip file" for the directory to avoid attempt to deletion
      // of any parent directory
      if (FLAGSET(Params, cpDelete) && SinkFileParams.Skipped)
      {
        THROW_SKIP_FILE_NULL;
      }
    }
    else
    {
      FTerminal->LogEvent(FORMAT(L"Skipping symlink to directory \"%s\".", (FileName)));
    }
  }
  else
  {
    FTerminal->LogEvent(FORMAT(L"Copying \"%s\" to local directory started.", (FileName)));

    // Will we use ASCII of BINARY file transfer?
    OperationProgress->SetAsciiTransfer(
      CopyParam->UseAsciiTransfer(FileName, osRemote, MaskParams));
    FTerminal->LogEvent(UnicodeString((OperationProgress->AsciiTransfer ? L"Ascii" : L"Binary")) +
      L" transfer mode selected.");

    // Suppose same data size to transfer as to write
    OperationProgress->SetTransferSize(File->Size);
    OperationProgress->SetLocalSize(OperationProgress->TransferSize);

    int Attrs;
    FILE_OPERATION_LOOP_BEGIN
    {
      Attrs = FileGetAttr(ApiPath(DestFullName));
      if ((Attrs >= 0) && FLAGSET(Attrs, faDirectory))
      {
        EXCEPTION;
      }
    }
    FILE_OPERATION_LOOP_END(FMTLOAD(NOT_FILE_ERROR, (DestFullName)));

    OperationProgress->TransferingFile = false; // not set with FTP protocol

    ResetFileTransfer();

    TFileTransferData UserData;

    UnicodeString FilePath = UnixExtractFilePath(FileName);
    unsigned int TransferType = (OperationProgress->AsciiTransfer ? 1 : 2);

    {
      // ignore file list
      TFileListHelper Helper(this, NULL, true);

      SetCPSLimit(OperationProgress);
      FFileTransferPreserveTime = CopyParam->PreserveTime;
      // not used for downloads anyway
      FFileTransferRemoveBOM = CopyParam->RemoveBOM;
      UserData.FileName = DestFileName;
      UserData.Params = Params;
      UserData.AutoResume = FLAGSET(Flags, tfAutoResume);
      UserData.CopyParam = CopyParam;
      UserData.Modification = File->Modification;
      FileTransfer(FileName, DestFullName, OnlyFileName,
        FilePath, true, File->Size, TransferType, UserData, OperationProgress);
    }

    // in case dest filename is changed from overwrite dialog
    if (DestFileName != UserData.FileName)
    {
      DestFullName = TargetDir + UserData.FileName;
      Attrs = FileGetAttr(ApiPath(DestFullName));
    }

    Action.Destination(ExpandUNCFileName(DestFullName));

    if (Attrs == -1)
    {
      Attrs = faArchive;
    }
    int NewAttrs = CopyParam->LocalFileAttrs(*File->Rights);
    if ((NewAttrs & Attrs) != NewAttrs)
    {
      FILE_OPERATION_LOOP_BEGIN
      {
        THROWOSIFFALSE(FileSetAttr(ApiPath(DestFullName), Attrs | NewAttrs) == 0);
      }
      FILE_OPERATION_LOOP_END(FMTLOAD(CANT_SET_ATTRS, (DestFullName)));
    }

    FTerminal->LogFileDone(OperationProgress);
  }

  if (FLAGSET(Params, cpDelete))
  {
    // If file is directory, do not delete it recursively, because it should be
    // empty already. If not, it should not be deleted (some files were
    // skipped or some new files were copied to it, while we were downloading)
    int Params = dfNoRecursive;
    FTerminal->DeleteFile(FileName, File, &Params);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::SinkFile(UnicodeString FileName,
  const TRemoteFile * File, void * Param)
{
  TSinkFileParams * Params = (TSinkFileParams *)Param;
  assert(Params->OperationProgress);
  try
  {
    SinkRobust(FileName, File, Params->TargetDir, Params->CopyParam,
      Params->Params, Params->OperationProgress, Params->Flags);
  }
  catch(EScpSkipFile & E)
  {
    TFileOperationProgressType * OperationProgress = Params->OperationProgress;

    Params->Skipped = true;

    {
      TSuspendFileOperationProgress Suspend(OperationProgress);
      if (!FTerminal->HandleException(&E))
      {
        throw;
      }
    }

    if (OperationProgress->Cancel)
    {
      Abort();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CopyToRemote(TStrings * FilesToCopy,
  const UnicodeString ATargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  assert((FilesToCopy != NULL) && (OperationProgress != NULL));

  Params &= ~cpAppend;
  UnicodeString FileName, FileNameOnly;
  UnicodeString TargetDir = AbsolutePath(ATargetDir, false);
  UnicodeString FullTargetDir = UnixIncludeTrailingBackslash(TargetDir);
  int Index = 0;
  while ((Index < FilesToCopy->Count) && !OperationProgress->Cancel)
  {
    bool Success = false;
    FileName = FilesToCopy->Strings[Index];
    FileNameOnly = ExtractFileName(FileName);

    try
    {
      try
      {
        if (FTerminal->SessionData->CacheDirectories)
        {
          FTerminal->DirectoryModified(TargetDir, false);

          if (DirectoryExists(ApiPath(FileName)))
          {
            FTerminal->DirectoryModified(FullTargetDir + FileNameOnly, true);
          }
        }
        SourceRobust(FileName, FullTargetDir, CopyParam, Params, OperationProgress,
          tfFirstLevel);
        Success = true;
        FLastDataSent = Now();
      }
      catch(EScpSkipFile & E)
      {
        TSuspendFileOperationProgress Suspend(OperationProgress);
        if (!FTerminal->HandleException(&E))
        {
          throw;
        }
      }
    }
    __finally
    {
      OperationProgress->Finish(FileName, Success, OnceDoneOperation);
    }
    Index++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::SourceRobust(const UnicodeString FileName,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  // the same in TSFTPFileSystem

  TUploadSessionAction Action(FTerminal->ActionLog);
  TRobustOperationLoop RobustLoop(FTerminal, OperationProgress);

  do
  {
    try
    {
      Source(FileName, TargetDir, CopyParam, Params, OperationProgress,
        Flags, Action);
    }
    catch(Exception & E)
    {
      if (!RobustLoop.TryReopen(E))
      {
        FTerminal->RollbackAction(Action, OperationProgress, &E);
        throw;
      }
    }

    if (RobustLoop.ShouldRetry())
    {
      OperationProgress->RollbackTransfer();
      Action.Restart();
      // prevent overwrite confirmations
      // (should not be set for directories!)
      Params |= cpNoConfirmation;
      Flags |= tfAutoResume;
    }
  }
  while (RobustLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Source(const UnicodeString FileName,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags,
  TUploadSessionAction & Action)
{
  Action.FileName(ExpandUNCFileName(FileName));

  OperationProgress->SetFile(FileName, false);

  if (!FTerminal->AllowLocalFileTransfer(FileName, CopyParam, OperationProgress))
  {
    THROW_SKIP_FILE_NULL;
  }

  __int64 Size;
  int Attrs;

  FTerminal->OpenLocalFile(FileName, GENERIC_READ, &Attrs,
    NULL, NULL, NULL, NULL, &Size);

  OperationProgress->SetFileInProgress();

  bool Dir = FLAGSET(Attrs, faDirectory);
  if (Dir)
  {
    Action.Cancel();
    DirectorySource(IncludeTrailingBackslash(FileName), TargetDir,
      Attrs, CopyParam, Params, OperationProgress, Flags);
  }
  else
  {
    UnicodeString DestFileName = CopyParam->ChangeFileName(ExtractFileName(FileName),
      osLocal, FLAGSET(Flags, tfFirstLevel));

    FTerminal->LogEvent(FORMAT(L"Copying \"%s\" to remote directory started.", (FileName)));

    OperationProgress->SetLocalSize(Size);

    // Suppose same data size to transfer as to read
    // (not true with ASCII transfer)
    OperationProgress->SetTransferSize(OperationProgress->LocalSize);
    OperationProgress->TransferingFile = false;

    TDateTime Modification;
    // Inspired by SysUtils::FileAge
    WIN32_FIND_DATA FindData;
    HANDLE Handle = FindFirstFile(FileName.c_str(), &FindData);
    if (Handle != INVALID_HANDLE_VALUE)
    {
      Modification =
        UnixToDateTime(
          ConvertTimestampToUnixSafe(FindData.ftLastWriteTime, dstmUnix),
          dstmUnix);
      FindClose(Handle);
    }

    // Will we use ASCII of BINARY file transfer?
    TFileMasks::TParams MaskParams;
    MaskParams.Size = Size;
    MaskParams.Modification = Modification;
    OperationProgress->SetAsciiTransfer(
      CopyParam->UseAsciiTransfer(FileName, osLocal, MaskParams));
    FTerminal->LogEvent(
      UnicodeString(OperationProgress->AsciiTransfer ? L"Ascii" : L"Binary") +
        L" transfer mode selected.");

    ResetFileTransfer();

    TFileTransferData UserData;

    unsigned int TransferType = (OperationProgress->AsciiTransfer ? 1 : 2);

    {
      // ignore file list
      TFileListHelper Helper(this, NULL, true);

      SetCPSLimit(OperationProgress);
      // not used for uploads anyway
      FFileTransferPreserveTime = CopyParam->PreserveTime;
      FFileTransferRemoveBOM = CopyParam->RemoveBOM;
      // not used for uploads, but we get new name (if any) back in this field
      UserData.FileName = DestFileName;
      UserData.Params = Params;
      UserData.AutoResume = FLAGSET(Flags, tfAutoResume);
      UserData.CopyParam = CopyParam;
      UserData.Modification = Modification;
      FileTransfer(FileName, FileName, DestFileName,
        TargetDir, false, Size, TransferType, UserData, OperationProgress);
    }

    UnicodeString DestFullName = TargetDir + UserData.FileName;
    // only now, we know the final destination
    Action.Destination(DestFullName);

    // We are not able to tell if setting timestamp succeeded,
    // so we log it always (if supported).
    // Support for MDTM does not necessarily mean that the server supports
    // non-standard hack of setting timestamp using
    // MFMT-like (two argument) call to MDTM.
    // IIS definitelly does.
    if (FFileTransferPreserveTime &&
        ((FServerCapabilities->GetCapability(mfmt_command) == yes) ||
         ((FServerCapabilities->GetCapability(mdtm_command) == yes))))
    {
      TTouchSessionAction TouchAction(FTerminal->ActionLog, DestFullName, Modification);
    }

    FTerminal->LogFileDone(OperationProgress);
  }

  /* TODO : Delete also read-only files. */
  if (FLAGSET(Params, cpDelete))
  {
    if (!Dir)
    {
      FILE_OPERATION_LOOP_BEGIN
      {
        THROWOSIFFALSE(Sysutils::DeleteFile(ApiPath(FileName)));
      }
      FILE_OPERATION_LOOP_END(FMTLOAD(DELETE_LOCAL_FILE_ERROR, (FileName)));
    }
  }
  else if (CopyParam->ClearArchive && FLAGSET(Attrs, faArchive))
  {
    FILE_OPERATION_LOOP_BEGIN
    {
      THROWOSIFFALSE(FileSetAttr(ApiPath(FileName), Attrs & ~faArchive) == 0);
    }
    FILE_OPERATION_LOOP_END(FMTLOAD(CANT_SET_ATTRS, (FileName)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DirectorySource(const UnicodeString DirectoryName,
  const UnicodeString TargetDir, int Attrs, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  UnicodeString DestDirectoryName = CopyParam->ChangeFileName(
    ExtractFileName(ExcludeTrailingBackslash(DirectoryName)), osLocal,
    FLAGSET(Flags, tfFirstLevel));
  UnicodeString DestFullName = UnixIncludeTrailingBackslash(TargetDir + DestDirectoryName);

  OperationProgress->SetFile(DirectoryName);

  int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
  TSearchRecChecked SearchRec;
  bool FindOK;

  FILE_OPERATION_LOOP_BEGIN
  {
    FindOK =
      (FindFirstChecked(DirectoryName + L"*.*", FindAttrs, SearchRec) == 0);
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(LIST_DIR_ERROR, (DirectoryName)));

  bool CreateDir = true;

  try
  {
    while (FindOK && !OperationProgress->Cancel)
    {
      UnicodeString FileName = DirectoryName + SearchRec.Name;
      try
      {
        if ((SearchRec.Name != L".") && (SearchRec.Name != L".."))
        {
          SourceRobust(FileName, DestFullName, CopyParam, Params, OperationProgress,
            Flags & ~(tfFirstLevel | tfAutoResume));
          // if any file got uploaded (i.e. there were any file in the
          // directory and at least one was not skipped),
          // do not try to create the directory,
          // as it should be already created by FZAPI during upload
          CreateDir = false;
        }
      }
      catch (EScpSkipFile &E)
      {
        // If ESkipFile occurs, just log it and continue with next file
        TSuspendFileOperationProgress Suspend(OperationProgress);
        // here a message to user was displayed, which was not appropriate
        // when user refused to overwrite the file in subdirectory.
        // hopefully it won't be missing in other situations.
        if (!FTerminal->HandleException(&E))
        {
          throw;
        }
      }

      FILE_OPERATION_LOOP_BEGIN
      {
        FindOK = (FindNextChecked(SearchRec) == 0);
      }
      FILE_OPERATION_LOOP_END(FMTLOAD(LIST_DIR_ERROR, (DirectoryName)));
    }
  }
  __finally
  {
    FindClose(SearchRec);
  }

  if (CreateDir)
  {
    TRemoteProperties Properties;
    if (CopyParam->PreserveRights)
    {
      Properties.Valid = TValidProperties() << vpRights;
      Properties.Rights = CopyParam->RemoteFileRights(Attrs);
    }

    try
    {
      FTerminal->ExceptionOnFail = true;
      try
      {
        FTerminal->CreateDirectory(DestFullName, &Properties);
      }
      __finally
      {
        FTerminal->ExceptionOnFail = false;
      }
    }
    catch(...)
    {
      TRemoteFile * File = NULL;
      // ignore non-fatal error when the directory already exists
      bool Rethrow =
        !FTerminal->Active ||
        !FTerminal->FileExists(UnixExcludeTrailingBackslash(DestFullName), &File) ||
        !File->IsDirectory;
      delete File;
      if (Rethrow)
      {
        throw;
      }
    }
  }

  /* TODO : Delete also read-only directories. */
  /* TODO : Show error message on failure. */
  if (!OperationProgress->Cancel)
  {
    if (FLAGSET(Params, cpDelete))
    {
      RemoveDir(ApiPath(DirectoryName));
    }
    else if (CopyParam->ClearArchive && FLAGSET(Attrs, faArchive))
    {
      FILE_OPERATION_LOOP_BEGIN
      {
        THROWOSIFFALSE(FileSetAttr(ApiPath(DirectoryName), Attrs & ~faArchive) == 0);
      }
      FILE_OPERATION_LOOP_END(FMTLOAD(CANT_SET_ATTRS, (DirectoryName)));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CreateDirectory(const UnicodeString ADirName)
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
  assert(SupportsSiteCommand(SymlinkSiteCommand));
  if (ALWAYS_TRUE(Symbolic))
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
  UnicodeString FilePath = UnixExtractFilePath(FileName);

  bool Dir = (File != NULL) && File->IsDirectory && !File->IsSymLink;

  if (Dir && FLAGCLEAR(Params, dfNoRecursive))
  {
    try
    {
      FTerminal->ProcessDirectory(FileName, FTerminal->DeleteFile, &Params);
    }
    catch(...)
    {
      Action.Cancel();
      throw;
    }
  }

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
      FFileZillaIntf->Delete(FileNameOnly.c_str(), FilePath.c_str());
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
  FAIL;
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
  // make sure FZAPI is aware that we changed current working directory
  FFileZillaIntf->SetCurrentPath(FCurrentDirectory.c_str());
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::IsCapable(int Capability) const
{
  assert(FTerminal);
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
      return SupportsCommand(AvblCommand) || SupportsCommand(XQuotaCommand);

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
      return false;

    default:
      FAIL;
      return false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::LookupUsersGroups()
{
  FAIL;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadCurrentDirectory()
{
  // ask the server for current directory on startup only
  // and immediately after call to CWD,
  // later our current directory may be not synchronized with FZAPI current
  // directory anyway, see comments in EnsureLocation
  if (FCurrentDirectory.IsEmpty())
  {
    UnicodeString Command = L"PWD";
    SendCommand(Command);

    unsigned int Code;
    TStrings * Response = NULL;
    GotReply(WaitForCommandReply(), REPLY_2XX_CODE, L"", &Code, &Response);

    try
    {
      assert(Response != NULL);
      bool Result = false;

      // the only allowed 2XX code to "PWD"
      if ((Code == 257) &&
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
          FCurrentDirectory = UnixExcludeTrailingBackslash(Path);
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
  FileList->Reset();
  // FZAPI does not list parent directory, add it
  FileList->AddFile(new TRemoteParentDirectory(FTerminal));

  FLastReadDirectoryProgress = 0;

  TFileListHelper Helper(this, FileList, false);

  // always specify path to list, do not attempt to list "current" dir as:
  // 1) List() lists again the last listed directory, not the current working directory
  // 2) we handle this way the cached directory change
  UnicodeString Directory = AbsolutePath(FileList->Directory, false);
  FFileZillaIntf->List(Directory.c_str());

  GotReply(WaitForCommandReply(), REPLY_2XX_CODE | REPLY_ALLOW_CANCEL);

  AutoDetectTimeDifference(FileList);

  if (FTimeDifference != 0) // optimization
  {
    for (int Index = 0; Index < FileList->Count; Index++)
    {
      ApplyTimeDifference(FileList->Files[Index]);
    }
  }

  FLastDataSent = Now();
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::TimeZoneDifferenceApplicable(TModificationFmt ModificationFmt)
{
  // Full precision is available for MLST only, so we would not be here.
  return (ModificationFmt == mfMDHM) || ALWAYS_FALSE(ModificationFmt == mfFull);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ApplyTimeDifference(TRemoteFile * File)
{
  // FTimeDifference is not only optimization, but also prevents assertion failing
  // in TimeZoneDifferenceApplicable when the file has full precision
  if ((FTimeDifference != 0) &&
      TimeZoneDifferenceApplicable(File->ModificationFmt))
  {
    assert(File->Modification == File->LastAccess);
    File->Modification = IncSecond(File->Modification, FTimeDifference);
    File->LastAccess = IncSecond(File->LastAccess, FTimeDifference);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::AutoDetectTimeDifference(TRemoteFileList * FileList)
{
  if (FDetectTimeDifference &&
      // Does not support MLST/MLSD, but supports MDTM at least
      !FFileZillaIntf->UsingMlsd() && SupportsReadingFile())
  {
    for (int Index = 0; Index < FileList->Count; Index++)
    {
      TRemoteFile * File = FileList->Files[Index];
      // For directories, we do not do MDTM in ReadFile
      // (it should not be problem to use them otherwise).
      // We are also not interested in files with day precision only.
      if (!File->IsDirectory && !File->IsSymLink &&
          TimeZoneDifferenceApplicable(File->ModificationFmt))
      {
        FDetectTimeDifference = false;

        TRemoteFile * UtcFile = NULL;
        try
        {
          ReadFile(File->FullFileName, UtcFile);
        }
        catch (Exception & E)
        {
          if (!FTerminal->Active)
          {
            throw;
          }
          break;
        }

        TDateTime UtcModification = UtcFile->Modification;
        delete UtcFile;

        FTimeDifference = SecondsBetween(UtcModification, File->Modification);

        UnicodeString LogMessage;
        if (FTimeDifference == 0)
        {
          LogMessage = FORMAT(L"No timezone difference detected using file %s", (File->FullFileName));
        }
        else
        {
          LogMessage = FORMAT(L"Timezone difference of %s detected using file %s", (FormatTimeZone(FTimeDifference), File->FullFileName));
        }
        FTerminal->LogEvent(LogMessage);

        break;
      }
    }
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
          assert(FListAll == asOff);
          FListAll = asAuto;
        }
        else if (FListAll == asAuto)
        {
          // some servers take "-a" as a mask and return empty directory listing
          // (note that it's actually never empty here, there's always at least parent directory,
          // added explicitly by DoReadDirectory)
          if ((FileList->Count == 0) ||
              ((FileList->Count == 1) && FileList->Files[0]->IsParentDirectory))
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
        FDoListAll = (FListAll == asOn);
      }
      catch(Exception & E)
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
  UnicodeString FileNameOnly = UnixExtractFileName(FileName);
  UnicodeString FilePath = UnixExtractFilePath(FileName);

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

    ApplyTimeDifference(AFile);

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
    ((FServerCapabilities->GetCapability(mdtm_command) == yes) &&
     (FServerCapabilities->GetCapability(size_command) == yes));
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadFile(const UnicodeString FileName,
  TRemoteFile *& File)
{
  UnicodeString Path = UnixExtractFilePath(FileName);
  UnicodeString NameOnly = UnixExtractFileName(FileName);
  TRemoteFile *AFile = NULL;
  bool Own;
  if (SupportsReadingFile())
  {
    DoReadFile(FileName, AFile);
    Own = true;
  }
  else
  {
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

    Own = false;
  }

  if (AFile == NULL)
  {
    File = NULL;
    throw Exception(FMTLOAD(FILE_NOT_EXISTS, (FileName)));
  }

  assert(AFile != NULL);
  File = Own ? AFile : AFile->Duplicate();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadSymlink(TRemoteFile * SymlinkFile,
  TRemoteFile *& File)
{
  // Resolving symlinks over FTP is big overhead
  // (involves opening TCPIP connection for retrieving "directory listing").
  // Moreover FZAPI does not support that anyway.
  // Though nowadays we could use MLST to read the symlink.
  File = new TRemoteFile(SymlinkFile);
  try
  {
    File->Terminal = FTerminal;
    File->FileName = UnixExtractFileName(SymlinkFile->LinkTo);
    // FZAPI treats all symlink target as directories
    File->Type = FILETYPE_DIRECTORY;
  }
  catch(...)
  {
    delete File;
    File = NULL;
    throw;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::RenameFile(const UnicodeString AFileName,
  const UnicodeString ANewName)
{
  UnicodeString FileName = AbsolutePath(AFileName, false);
  UnicodeString NewName = AbsolutePath(ANewName, false);

  UnicodeString FileNameOnly = UnixExtractFileName(FileName);
  UnicodeString FilePathOnly = UnixExtractFilePath(FileName);
  UnicodeString NewNameOnly = UnixExtractFileName(NewName);
  UnicodeString NewPathOnly = UnixExtractFilePath(NewName);

  {
    // ignore file list
    TFileListHelper Helper(this, NULL, true);

    FFileZillaIntf->Rename(FileNameOnly.c_str(), NewNameOnly.c_str(),
      FilePathOnly.c_str(), NewPathOnly.c_str());

    GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CopyFile(const UnicodeString FileName,
  const UnicodeString NewName)
{
  assert(SupportsSiteCommand(CopySiteCommand));
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
  if (SupportsCommand(XQuotaCommand))
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
    FFileSystemInfo.RemoteSystem = FSystem;
    FFileSystemInfo.RemoteSystem.Unique();

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
        // For TrimLeft, refer to HandleFeatReply
        FFileSystemInfo.AdditionalInfo += FORMAT(L"  %s\r\n", (TrimLeft(FFeatures->Strings[Index])));
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
  return !FTerminal->SessionData->Password.IsEmpty();
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

    default:
      FAIL;
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
          FAIL;
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
      Result = true;
      break;

    case OPTION_PASV:
      // should never get here t_server.nPasv being nonzero
      FAIL;
      Result = FALSE;
      break;

    case OPTION_PRESERVEDOWNLOADFILETIME:
    case OPTION_MPEXT_PRESERVEUPLOADFILETIME:
      Result = FFileTransferPreserveTime ? TRUE : FALSE;
      break;

    case OPTION_LIMITPORTRANGE:
      Result = FALSE;
      break;

    case OPTION_PORTRANGELOW:
    case OPTION_PORTRANGEHIGH:
      // should never get here OPTION_LIMITPORTRANGE being zero
      FAIL;
      Result = 0;
      break;

    case OPTION_ENABLE_IPV6:
      Result = ((Data->AddressFamily == afIPv6) ? TRUE : FALSE);
      break;

    case OPTION_KEEPALIVE:
      Result = ((Data->FtpPingType != ptOff) ? TRUE : FALSE);
      break;

    case OPTION_INTERVALLOW:
    case OPTION_INTERVALHIGH:
      Result = Data->FtpPingInterval;
      break;

    case OPTION_VMSALLREVISIONS:
      Result = FALSE;
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

    case OPTION_MPEXT_MIN_TLS_VERSION:
      Result = Data->MinTlsVersion;
      break;

    case OPTION_MPEXT_MAX_TLS_VERSION:
      Result = Data->MaxTlsVersion;
      break;

    case OPTION_MPEXT_SNDBUF:
      Result = Data->SendBuf;
      break;

    case OPTION_MPEXT_TRANSFER_ACTIVE_IMMEDIATELY:
      Result = Data->FtpTransferActiveImmediately;
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

    default:
      FAIL;
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
  while (ProcessMessage());
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::WaitForMessages()
{
  unsigned int Result;
  do
  {
    Result = WaitForSingleObject(FQueueEvent, GUIUpdateInterval);
    ProcessGUI();
  } while (Result == WAIT_TIMEOUT);

  if (Result != WAIT_OBJECT_0)
  {
    FTerminal->FatalError(NULL, FMTLOAD(INTERNAL_ERROR, (L"ftp#1", IntToStr(int(Result)))));
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::PoolForFatalNonCommandReply()
{
  assert(FReply == 0);
  assert(FCommandReply == 0);
  assert(!FWaitingForReply);

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
    assert(FCommandReply == 0);
    FCommandReply = 0;
    assert(FWaitingForReply);
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
  // to keep waiting,
  // non-command reply must be unset,
  // the reply we wait for must be unset or
  // last code must be unset (if we wait for it)
  return
     (FReply == 0) &&
     ((ReplyToAwait == 0) ||
      (WantLastCode && NoFinalLastCode()));
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
    // so that we "eat" the reply message, so that it gets not mistakenly
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
  assert(FReply == 0);
  assert(FCommandReply == 0);
  assert(!FWaitingForReply);

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
    assert(FWaitingForReply);
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
  FAIL;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ResetReply()
{
  FLastCode = 0;
  FLastCodeClass = 0;
  assert(FLastResponse != NULL);
  FLastResponse->Clear();
  assert(FLastErrorResponse != NULL);
  FLastErrorResponse->Clear();
  assert(FLastError != NULL);
  FLastError->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::GotNonCommandReply(unsigned int Reply)
{
  assert(FLAGSET(Reply, TFileZillaIntf::REPLY_DISCONNECTED));
  GotReply(Reply);
  // should never get here as GotReply should raise fatal exception
  FAIL;
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
      assert(Reply == TFileZillaIntf::REPLY_OK);

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
      assert(
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
      assert(
        FLAGSET(Reply, TFileZillaIntf::REPLY_ERROR) ||
        FLAGSET(Reply, TFileZillaIntf::REPLY_DISCONNECTED));

      // TODO: REPLY_CRITICALERROR ignored

      // REPLY_NOTCONNECTED happens if connection is closed between moment
      // when FZAPI interface method dispatches the command to FZAPI thread
      // and moment when FZAPI thread receives the command
      bool Disconnected =
        FLAGSET(Reply, TFileZillaIntf::REPLY_DISCONNECTED) ||
        FLAGSET(Reply, TFileZillaIntf::REPLY_NOTCONNECTED);

      AnsiString HelpKeyword;
      TStrings * MoreMessages = new TStringList();
      try
      {
        if (Disconnected)
        {
          if (FLAGCLEAR(Flags, REPLY_CONNECT))
          {
            MoreMessages->Add(LoadStr(LOST_CONNECTION));
            Discard();
            FTerminal->Closed();
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

        if (FLastCode == 425)
        {
          if (!FTerminal->SessionData->FtpPasvMode)
          {
            MoreMessages->Add(LoadStr(FTP_CANNOT_OPEN_ACTIVE_CONNECTION2));
            HelpKeyword = HELP_FTP_CANNOT_OPEN_ACTIVE_CONNECTION;
          }
        }

        if (FLastCode == DummyTimeoutCode)
        {
          HelpKeyword = HELP_ERRORMSG_TIMEOUT;
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
        assert(MoreMessages->Count > 0);
        // bit too generic assigning of main instructions, let's see how it works
        Error = MainInstructions(MoreMessages->Strings[0]);
        MoreMessages->Delete(0);
      }

      if (Disconnected)
      {
        // for fatal error, it is essential that there is some message
        assert(!Error.IsEmpty());
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
  int From = 1;
  FLastCommandSent = CopyToChars(Command, From, L" ", false);
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
  int Code;

  if (FOnCaptureOutput != NULL)
  {
    FOnCaptureOutput(Response, cotOutput);
  }

  // Two forms of multiline responses were observed
  // (the first is according to the RFC 959):

  // 211-Features:
  //  MDTM
  //  REST STREAM
  //  SIZE
  // 211 End

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

  bool HasCodePrefix =
    (Response.Length() >= 3) &&
    TryStrToInt(Response.SubString(1, 3), Code) &&
    (Code >= 100) && (Code <= 599) &&
    ((Response.Length() == 3) || (Response[4] == L' ') || (Response[4] == L'-'));

  if (HasCodePrefix && !FMultineResponse)
  {
    FMultineResponse = (Response.Length() >= 4) && (Response[4] == L'-');
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
        FMultineResponse = false;
      }
      Start = 5;
    }
    else
    {
      Start = (((Response.Length() >= 1) && (Response[1] == L' ')) ? 2 : 1);
    }

    // Intermediate empty lines are being added
    if (FMultineResponse || (Response.Length() >= Start))
    {
      StoreLastResponse(Response.SubString(Start, Response.Length() - Start + 1));
    }
  }

  if (!FMultineResponse)
  {
    if (FLastCode == 220)
    {
      // HOST command also uses 220 response.
      // Neither our use of welcome messagfe is prepared for changing it
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
      }
    }
    else if (FLastCommand == PASS)
    {
      // 530 = "Not logged in."
      if (FLastCode == 530)
      {
        FPasswordFailed = true;
      }
    }
    else if (FLastCommand == SYST)
    {
      assert(FSystem.IsEmpty());
      // Positive reply to "SYST" must be 215, see RFC 959
      if (FLastCode == 215)
      {
        FSystem = FLastResponse->Text.TrimRight();
        // full name is "Personal FTP Server PRO K6.0"
        if ((FListAll == asAuto) &&
            (FSystem.Pos(L"Personal FTP Server") > 0))
        {
          FTerminal->LogEvent(L"Server is known not to support LIST -a");
          FListAll = asOff;
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
void __fastcall TFTPFileSystem::HandleFeatReply()
{
  ResetFeatures();
  // Response to FEAT must be multiline, where leading and trailing line
  // is "meaningless". See RFC 2389.
  if ((FLastCode == 211) && (FLastResponse->Count > 2))
  {
    FLastResponse->Delete(0);
    FLastResponse->Delete(FLastResponse->Count - 1);
    FFeatures->Assign(FLastResponse);
    for (int Index = 0; Index < FFeatures->Count; Index++)
    {
      // IIS 2003 indents response by 4 spaces, instead of one,
      // see example in HandleReplyStatus
      UnicodeString Feature = TrimLeft(FFeatures->Strings[Index]);

      UnicodeString Args = Feature;
      UnicodeString Command = CutToChar(Args, L' ', true);

      // Serv-U lists Xalg commands like:
      //  XSHA1 filename;start;end
      FSupportedCommands->Add(Command);

      if (SameText(Command, SiteCommand))
      {
        // Serv-U lists all SITE commands in one line like:
        //  SITE PSWD;SET;ZONE;CHMOD;MSG;EXEC;HELP
        // But ProFTPD lists them separatelly:
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
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::ExtractStatusMessage(UnicodeString Status)
{
  // CApiLog::LogMessage
  // (note that the formatting may not be present when LogMessageRaw is used)
  int P1 = Status.Pos(L"): ");
  if (P1 > 0)
  {
    int P2 = Status.Pos(L".cpp(");
    if ((P2 > 0) && (P2 < P1))
    {
      int P3 = Status.Pos(L"   caller=0x");
      if ((P3 > 0) && (P3 > P1))
      {
        Status = Status.SubString(P1 + 3, P3 - P1 - 3);
      }
    }
  }
  return Status;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleStatus(const wchar_t * AStatus, int Type)
{
  TLogLineType LogType = (TLogLineType)-1;
  UnicodeString Status(AStatus);

  switch (Type)
  {
    case TFileZillaIntf::LOG_STATUS:
      FTerminal->Information(Status, true);
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
      LogType = llInput;
      break;

    case TFileZillaIntf::LOG_ERROR:
    case TFileZillaIntf::LOG_APIERROR:
    case TFileZillaIntf::LOG_WARNING:
      // when timeout message occurs, break loop waiting for response code
      // by setting dummy one
      if (Type == TFileZillaIntf::LOG_ERROR)
      {
        if (Status == FTimeoutStatus)
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
      Status = ExtractStatusMessage(Status);
      FLastError->Add(Status);
      LogType = llMessage;
      break;

    case TFileZillaIntf::LOG_REPLY:
      HandleReplyStatus(AStatus);
      LogType = llOutput;
      break;

    case TFileZillaIntf::LOG_INFO:
      Status = ExtractStatusMessage(Status);
      LogType = llMessage;
      break;

    case TFileZillaIntf::LOG_DEBUG:
      // used for directory listing only
      LogType = llMessage;
      break;

    default:
      FAIL;
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
  wchar_t * FileName1, size_t FileName1Len, const wchar_t * /*FileName2*/,
  const wchar_t * /*Path1*/, const wchar_t * Path2,
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
    else
    {
      TFileOperationProgressType * OperationProgress = FTerminal->OperationProgress;
      UnicodeString FileName = FileName1;
      assert(UserData.FileName == FileName);
      TOverwriteMode OverwriteMode = omOverwrite;
      TOverwriteFileParams FileParams;
      bool NoFileParams =
        (Size1 < 0) || (LocalTime == 0) ||
        (Size2 < 0) || !RemoteTime.HasDate;
      if (!NoFileParams)
      {
        FileParams.SourceSize = Size2;
        FileParams.DestSize = Size1;

        if (OperationProgress->Side == osLocal)
        {
          FileParams.SourceTimestamp = ConvertLocalTimestamp(LocalTime);
          RemoteFileTimeToDateTimeAndPrecision(RemoteTime, FileParams.DestTimestamp, FileParams.DestPrecision);
        }
        else
        {
          FileParams.DestTimestamp = ConvertLocalTimestamp(LocalTime);
          RemoteFileTimeToDateTimeAndPrecision(RemoteTime, FileParams.SourceTimestamp, FileParams.SourcePrecision);
        }
      }

      UnicodeString FullFileName = Path2;
      if (OperationProgress->Side == osLocal)
      {
        FullFileName = IncludeTrailingBackslash(FullFileName);
      }
      else
      {
        FullFileName = UnixIncludeTrailingBackslash(FullFileName);
      }
      FullFileName += FileName;

      if (ConfirmOverwrite(FullFileName, FileName, OverwriteMode, OperationProgress,
            (NoFileParams ? NULL : &FileParams), UserData.CopyParam, UserData.Params,
            UserData.AutoResume && UserData.CopyParam->AllowResume(FileParams.SourceSize)))
      {
        switch (OverwriteMode)
        {
          case omOverwrite:
            if (FileName != FileName1)
            {
              wcsncpy(FileName1, FileName.c_str(), FileName1Len);
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
            FAIL;
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
  int Pos;
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
bool __fastcall TFTPFileSystem::HandleAsynchRequestVerifyCertificate(
  const TFtpsCertificateData & Data, int & RequestResult)
{
  if (!FActive)
  {
    return false;
  }
  else
  {
    FSessionInfo.CertificateFingerprint =
      BytesToHex(RawByteString((const char*)Data.Hash, Data.HashLen), false, L':');

    UnicodeString CertificateSubject = Data.Subject.Organization;
    FTerminal->LogEvent(FORMAT(L"Verifying certificate for \"%s\" with fingerprint %s and %d failures", (CertificateSubject, FSessionInfo.CertificateFingerprint, Data.VerificationResult)));

    bool VerificationResult = false;
    bool TryWindowsSystemCertificateStore = false;
    UnicodeString VerificationResultStr;
    switch (Data.VerificationResult)
    {
      case X509_V_OK:
        VerificationResult = true;
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

    // TryWindowsSystemCertificateStore is set for the same set of failures
    // as trigger NE_SSL_UNTRUSTED flag in ne_openssl.c's verify_callback()
    if (!VerificationResult && TryWindowsSystemCertificateStore)
    {
      if (WindowsValidateCertificate(Data.Certificate, Data.CertificateLen))
      {
        FTerminal->LogEvent(L"Certificate verified against Windows certificate store");
        VerificationResult = true;
      }
    }

    UnicodeString Summary;
    if (!VerificationResult)
    {
      Summary = VerificationResultStr + L" " + FMTLOAD(CERT_ERRDEPTH, (Data.VerificationDepth + 1));
    }

    if (!VerifyCertificateHostName(Data))
    {
      VerificationResult = false;
      AddToList(Summary, FMTLOAD(CERT_NAME_MISMATCH, (FTerminal->SessionData->HostNameExpanded)), L"\n\n");
    }

    if (VerificationResult)
    {
      Summary = LoadStr(CERT_OK);
    }

    FSessionInfo.Certificate =
      FMTLOAD(CERT_TEXT, (
        FormatContact(Data.Issuer),
        FormatContact(Data.Subject),
        FormatValidityTime(Data.ValidFrom),
        FormatValidityTime(Data.ValidUntil),
        FSessionInfo.CertificateFingerprint,
        Summary));

    RequestResult = 0;

    if (VerificationResult)
    {
      RequestResult = 1;
    }

    if (RequestResult == 0)
    {
      if (FTerminal->VerifyCertificate(CertificateStorageKey,
            FSessionInfo.CertificateFingerprint, CertificateSubject, Data.VerificationResult))
      {
        RequestResult = 1;
      }
    }

    if (RequestResult == 0)
    {
      TClipboardHandler ClipboardHandler;
      ClipboardHandler.Text = FSessionInfo.CertificateFingerprint;

      TQueryButtonAlias Aliases[1];
      Aliases[0].Button = qaRetry;
      Aliases[0].Alias = LoadStr(COPY_KEY_BUTTON);
      Aliases[0].OnClick = &ClipboardHandler.Copy;

      TQueryParams Params;
      Params.HelpKeyword = HELP_VERIFY_CERTIFICATE;
      Params.NoBatchAnswers = qaYes | qaRetry;
      Params.Aliases = Aliases;
      Params.AliasesCount = LENOF(Aliases);
      unsigned int Answer = FTerminal->QueryUser(
        FMTLOAD(VERIFY_CERT_PROMPT3, (FSessionInfo.Certificate)),
        NULL, qaYes | qaNo | qaCancel | qaRetry, &Params, qtWarning);

      switch (Answer)
      {
        case qaYes:
          // 2 = always, as used by FZ's VerifyCertDlg.cpp,
          // however FZAPI takes all non-zero values equally
          RequestResult = 2;
          break;

        case qaNo:
          RequestResult = 1;
          break;

        case qaCancel:
          FTerminal->Configuration->Usage->Inc(L"HostNotVerified");
          RequestResult = 0;
          break;

        default:
          FAIL;
          RequestResult = 0;
          break;
      }

      if (RequestResult == 2)
      {
        FTerminal->CacheCertificate(
          CertificateStorageKey, FSessionInfo.CertificateFingerprint, Data.VerificationResult);
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
    UnicodeString Password = L"";
    if (FTerminal->PromptUser(FTerminal->SessionData, pkPassword, LoadStr(PASSWORD_TITLE), L"",
      LoadStr(PASSWORD_PROMPT), false, 0, Password))
    {
      Data.Password = _wcsdup(Password.c_str());
      RequestResult = TFileZillaIntf::REPLY_OK;
    }
    else
    {
      RequestResult = TFileZillaIntf::REPLY_ABORTED;
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
      // not exact as we got year as well, but it is most probably
      // guessed by FZAPI anyway
      ModificationFmt = Source.HasSeconds ? mfFull : mfMDHM;
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
    assert(FFileList == NULL);
    return false;
  }
  else
  {
    assert(FFileList != NULL);
    // This can actually fail in real life,
    // when connected to server with case insensitive paths.
    // Is empty when called from DoReadFile
    assert(FFileList->Directory.IsEmpty() || UnixSamePath(AbsolutePath(FFileList->Directory, false), Path));
    USEDPARAM(Path);

    for (unsigned int Index = 0; Index < Count; Index++)
    {
      const TListDataEntry * Entry = &Entries[Index];
      TRemoteFile * File = new TRemoteFile();
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
          // ignore permissions errors with FTP
        }

        File->HumanRights = Entry->HumanPerm;

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
        delete File;
        UnicodeString EntryData =
          FORMAT(L"%s/%s/%s/%s/%s/%d/%d/%d/%d/%d/%d/%d/%d/%d/%d",
            (Entry->Name, Entry->Permissions, Entry->HumanPerm, Entry->OwnerGroup, IntToStr(Entry->Size),
             int(Entry->Dir), int(Entry->Link), Entry->Time.Year, Entry->Time.Month, Entry->Time.Day,
             Entry->Time.Hour, Entry->Time.Minute, int(Entry->Time.HasTime),
             int(Entry->Time.HasSeconds), int(Entry->Time.HasDate)));
        throw ETerminal(&E, FMTLOAD(LIST_LINE_ERROR, (EntryData)), HELP_LIST_LINE_ERROR);
      }

      FFileList->AddFile(File);
    }
    return true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleTransferStatus(bool Valid, __int64 TransferSize,
  __int64 Bytes, int /*Percent*/, int /*TimeElapsed*/, int /*TimeLeft*/, int /*TransferRate*/,
  bool FileTransfer)
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
    // so do not treat is as a reply
    // (it is typically used asynchronously to notify about disconnects)
    if (Command != 0)
    {
      assert(FCommandReply == 0);
      FCommandReply = Reply;
    }
    else
    {
      assert(FReply == 0);
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
    FAIL;
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
  assert((Str.Length() > 0) && ((Str[1] == L'"') || (Str[1] == L'\'')));

  int Index = 1;
  wchar_t Quote;
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
          FAIL;
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
  FILETIME WrTime = DateTimeToFileTime(Data->Modification, dstmUnix);
  SetFileTime(Handle, NULL, NULL, &WrTime);
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::GetFileModificationTimeInUtc(const wchar_t * FileName, struct tm & Time)
{
  bool Result;
  try
  {
    // error-handling-free and DST-mode-inaware copy of TTerminal::OpenLocalFile
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
UnicodeString __fastcall GetOpenSSLVersionText()
{
  return OPENSSL_VERSION_TEXT;
}
//---------------------------------------------------------------------------
#endif NO_FILEZILLA
