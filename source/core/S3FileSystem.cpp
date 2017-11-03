//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#define NE_LFS
#define WINSCP
#define NEED_LIBS3

#include "S3FileSystem.h"

#include "SessionData.h"
#include "Interface.h"
#include "Common.h"
#include "Exceptions.h"
#include "Terminal.h"
#include "TextsCore.h"
#include "HelpCore.h"
#include "NeonIntf.h"
#include <ne_request.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define StrFromS3(S) StrFromNeon(S)
#define StrToS3(S) StrToNeon(S)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_TERMINAL FTerminal
//---------------------------------------------------------------------------
const int tfFirstLevel = 0x01;
//---------------------------------------------------------------------------
static std::unique_ptr<TCriticalSection> LibS3Section(TraceInitPtr(new TCriticalSection()));
//---------------------------------------------------------------------------
UTF8String LibS3Delimiter(L"/");
//---------------------------------------------------------------------------
UnicodeString __fastcall S3LibVersion()
{
  return FORMAT(L"%s.%s", (LIBS3_VER_MAJOR, LIBS3_VER_MINOR));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall S3LibDefaultHostName()
{
  return UnicodeString(S3_DEFAULT_HOSTNAME);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TS3FileSystem::TS3FileSystem(TTerminal * ATerminal) :
  TCustomFileSystem(ATerminal),
  FActive(false)
{
  FFileSystemInfo.ProtocolBaseName = L"S3";
  FFileSystemInfo.ProtocolName = FFileSystemInfo.ProtocolBaseName;
  S3_create_request_context(&FRequestContext);
  S3_set_request_context_session_callback(FRequestContext, LibS3SessionCallback, this);
  S3_set_request_context_ssl_callback(FRequestContext, LibS3SslCallback, this);
  S3_set_request_context_response_data_callback(FRequestContext, LibS3ResponseDataCallback, this);
}
//---------------------------------------------------------------------------
__fastcall TS3FileSystem::~TS3FileSystem()
{
  S3_destroy_request_context(FRequestContext);
  FRequestContext = NULL;
  UnregisterFromNeonDebug(FTerminal);
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::Open()
{

  FTlsVersionStr = L"";
  FNeonSession = NULL;
  FCurrentDirectory = L"";

  RequireNeon(FTerminal);

  FTerminal->Information(LoadStr(STATUS_CONNECT), true);

  TSessionData * Data = FTerminal->SessionData;

  FSessionInfo.LoginTime = Now();

  FLibS3Protocol = (Data->Ftps != ftpsNone) ? S3ProtocolHTTPS : S3ProtocolHTTP;

  UnicodeString AccessKeyId = Data->UserNameExpanded;
  if (AccessKeyId.IsEmpty())
  {
    if (!FTerminal->PromptUser(Data, pkUserName, LoadStr(S3_ACCESS_KEY_ID_TITLE), L"",
          LoadStr(S3_ACCESS_KEY_ID_PROMPT), true, 0, AccessKeyId))
    {
      // note that we never get here actually
      throw Exception(L"");
    }
  }
  FAccessKeyId = UTF8String(AccessKeyId);

  UnicodeString SecretAccessKey = UTF8String(Data->Password);
  if (SecretAccessKey.IsEmpty())
  {
    if (!FTerminal->PromptUser(Data, pkPassword, LoadStr(S3_SECRET_ACCESS_KEY_TITLE), L"",
          LoadStr(S3_SECRET_ACCESS_KEY_PROMPT), false, 0, SecretAccessKey))
    {
      // note that we never get here actually
      throw Exception(L"");
    }
  }
  FSecretAccessKey = UTF8String(SecretAccessKey);

  FHostName = UTF8String(Data->HostNameExpanded);
  FTimeout = Data->Timeout;

  RegisterForNeonDebug(FTerminal);
  UpdateNeonDebugMask();

  {
    TGuard Guard(LibS3Section.get());
    S3_initialize(NULL, S3_INIT_ALL, NULL);
  }

  FActive = false;
  try
  {
    TryOpenDirectory(ROOTDIRECTORY);
  }
  catch (Exception & E)
  {
    LibS3Deinitialize();
    FTerminal->Closed();
    FTerminal->FatalError(&E, LoadStr(CONNECTION_FAILED));
  }
  FActive = true;
}
//---------------------------------------------------------------------------
struct TLibS3CallbackData
{
  TLibS3CallbackData()
  {
    Status = (S3Status)-1;
  }

  TS3FileSystem * FileSystem;
  S3Status Status;
  UnicodeString RegionDetail;
  UnicodeString EndpointDetail;
  UnicodeString ErrorMessage;
  UnicodeString ErrorDetails;
};
//---------------------------------------------------------------------------
TS3FileSystem * TS3FileSystem::GetFileSystem(void * CallbackData)
{
  return static_cast<TLibS3CallbackData *>(CallbackData)->FileSystem;
}
//---------------------------------------------------------------------------
void TS3FileSystem::LibS3SessionCallback(ne_session_s * Session, void * CallbackData)
{
  TS3FileSystem * FileSystem = static_cast<TS3FileSystem *>(CallbackData);
  TSessionData * Data = FileSystem->FTerminal->SessionData;

  InitNeonSession(
    Session, Data->ProxyMethod, Data->ProxyHost, Data->ProxyPort,
    Data->ProxyUsername, Data->ProxyPassword, FileSystem->FTerminal);

  SetNeonTlsInit(Session, FileSystem->InitSslSession);

  // Data->Timeout is propagated via timeoutMs parameter of functions like S3_list_service

  FileSystem->FNeonSession = Session;
}
//------------------------------------------------------------------------------
void TS3FileSystem::InitSslSession(ssl_st * Ssl, ne_session * /*Session*/)
{
  // See also CAsyncSslSocketLayer::InitSSLConnection
  SetupSsl(Ssl, FTerminal->SessionData->MinTlsVersion, FTerminal->SessionData->MaxTlsVersion);
}
//---------------------------------------------------------------------------
int TS3FileSystem::LibS3SslCallback(int Failures, const ne_ssl_certificate_s * Certificate, void * CallbackData)
{
  TNeonCertificateData Data;
  RetrieveNeonCertificateData(Failures, Certificate, Data);
  TS3FileSystem * FileSystem = static_cast<TS3FileSystem *>(CallbackData);
  return FileSystem->VerifyCertificate(Data) ? NE_OK : NE_ERROR;
}
//---------------------------------------------------------------------------
// Similar to TWebDAVFileSystem::VerifyCertificate
bool TS3FileSystem::VerifyCertificate(TNeonCertificateData Data)
{
  FSessionInfo.CertificateFingerprint = Data.Fingerprint;

  bool Result;
  if (FTerminal->SessionData->FingerprintScan)
  {
    Result = false;
  }
  else
  {
    FTerminal->LogEvent(CertificateVerificationMessage(Data));

    UnicodeString SiteKey = TSessionData::FormatSiteKey(FTerminal->SessionData->HostNameExpanded, FTerminal->SessionData->PortNumber);
    Result =
      FTerminal->VerifyCertificate(HttpsCertificateStorageKey, SiteKey, Data.Fingerprint, Data.Subject, Data.Failures);

    if (!Result)
    {
      UnicodeString Message;
      Result = NeonWindowsValidateCertificateWithMessage(Data, Message);
      FTerminal->LogEvent(Message);
    }

    FSessionInfo.Certificate = CertificateSummary(Data, FTerminal->SessionData->HostNameExpanded);

    if (!Result)
    {
      Result = FTerminal->ConfirmCertificate(FSessionInfo, Data.Failures, HttpsCertificateStorageKey, true);
    }

    if (Result)
    {
      CollectTLSSessionInfo();
    }
  }

  return Result;
}
//------------------------------------------------------------------------------
void TS3FileSystem::CollectTLSSessionInfo()
{
  // See also TFTPFileSystem::Open().
  // Have to cache the value as the connection (the neon HTTP session, not "our" session)
  // can be closed at the time we need it in CollectUsage().
  UnicodeString Message = NeonTlsSessionInfo(FNeonSession, FSessionInfo, FTlsVersionStr);
  FTerminal->LogEvent(Message);
}
//---------------------------------------------------------------------------
S3Status TS3FileSystem::LibS3ResponsePropertiesCallback(const S3ResponseProperties * /*Properties*/, void * /*CallbackData*/)
{

  // TODO
  return S3StatusOK;
}
//---------------------------------------------------------------------------
void TS3FileSystem::LibS3ResponseDataCallback(const char * Data, size_t Size, void * CallbackData)
{
  TS3FileSystem * FileSystem = static_cast<TS3FileSystem *>(CallbackData);
  if (FileSystem->FTerminal->Log->Logging)
  {
    UnicodeString Content = UnicodeString(UTF8String(Data, Size)).Trim();
    FileSystem->FResponse += Content;
  }
}
//---------------------------------------------------------------------------
void TS3FileSystem::LibS3ResponseCompleteCallback(S3Status Status, const S3ErrorDetails * Error, void * CallbackData)
{
  TLibS3CallbackData & Data = *static_cast<TLibS3CallbackData *>(CallbackData);

  TS3FileSystem * FileSystem = Data.FileSystem;
  Data.Status = Status;
  Data.RegionDetail = L"";
  Data.EndpointDetail = L"";
  Data.ErrorMessage = L"";
  Data.ErrorDetails = L"";

  if (Error != NULL)
  {
    if (Error->message != NULL)
    {
      Data.ErrorMessage = StrFromS3(Error->message);
      FileSystem->FTerminal->LogEvent(Data.ErrorMessage);
    }

    UnicodeString ErrorDetails;
    if (Error->resource != NULL)
    {
      AddToList(ErrorDetails, FMTLOAD(S3_ERROR_RESOURCE, (StrFromS3(Error->resource))), L"\n");
    }
    if (Error->furtherDetails != NULL)
    {
      AddToList(ErrorDetails, FMTLOAD(S3_ERROR_FURTHER_DETAILS, (StrFromS3(Error->furtherDetails))), L"\n");
    }
    if (Error->extraDetailsCount)
    {
      UnicodeString ExtraDetails;
      for (int I = 0; I < Error->extraDetailsCount; I++)
      {
        UnicodeString DetailName = StrFromS3(Error->extraDetails[I].name);
        UnicodeString DetailValue = StrFromS3(Error->extraDetails[I].value);
        if (SameText(DetailName, L"Region"))
        {
          Data.RegionDetail = DetailValue;
        }
        else if (SameText(DetailName, L"Endpoint"))
        {
          Data.EndpointDetail = DetailValue;
        }
        AddToList(ExtraDetails, FORMAT(L"%s: %s", (DetailName, DetailValue)), L", ");
      }
      AddToList(ErrorDetails, LoadStr(S3_ERROR_EXTRA_DETAILS) + ExtraDetails, L"\n");
    }

    if (!ErrorDetails.IsEmpty())
    {
      FileSystem->FTerminal->LogEvent(ErrorDetails);
      Data.ErrorDetails = ErrorDetails;
    }
  }

  if (!FileSystem->FResponse.IsEmpty())
  {
    FileSystem->FTerminal->Log->Add(llOutput, FileSystem->FResponse);
  }
}
//---------------------------------------------------------------------------
void TS3FileSystem::RequestInit()
{
  FResponse = L"";
}
//---------------------------------------------------------------------------
bool TS3FileSystem::RetryBucket(const TLibS3CallbackData & Data, const UnicodeString & BucketName)
{
  bool Result = false;
  UnicodeString EndpointDetail = Data.EndpointDetail;
  if ((Data.Status == S3StatusErrorAuthorizationHeaderMalformed) &&
      (GetBucketRegion(BucketName) != Data.RegionDetail))
  {
    FTerminal->LogEvent(FORMAT("Will use region \"%s\" for bucket \"%s\" from now on.", (Data.RegionDetail, BucketName)));
    FRegions.insert(std::make_pair(BucketName, Data.RegionDetail));
    Result = true;
  }
  // happens with newly created buckets
  else if ((Data.Status == S3StatusErrorTemporaryRedirect) && !Data.EndpointDetail.IsEmpty())
  {
    UnicodeString HostName = Data.EndpointDetail;
    if (SameText(HostName.SubString(1, BucketName.Length() + 1), BucketName + L"."))
    {
      HostName.Delete(1, BucketName.Length() + 1);
    }
    if (GetBucketHostName(BucketName) != HostName)
    {
      FTerminal->LogEvent(FORMAT("Will use endpoint \"%s\" for bucket \"%s\" from now on.", (HostName, BucketName)));
      FHostNames.insert(std::make_pair(BucketName, HostName));
      Result = true;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void TS3FileSystem::CheckLibS3Error(const TLibS3CallbackData & Data)
{
  if (Data.Status != S3StatusOK)
  {
    UnicodeString Error, Details;
    switch (Data.Status)
    {
      case S3StatusErrorAccessDenied:
        Error = LoadStr(S3_STATUS_ACCESS_DENIED);
        break;

      // While it can mean an implementation fault, it will typically mean a wrong secure key.
      case S3StatusErrorSignatureDoesNotMatch:
        Error = LoadStr(AUTHENTICATION_FAILED);
        break;
    }

    if (!Error.IsEmpty())
    {
      Details = Data.ErrorMessage;
      AddToList(Details, Data.ErrorDetails, L"\n");
    }
    else
    {
      if (!Data.ErrorMessage.IsEmpty())
      {
        Error = Data.ErrorMessage;
      }
      else
      {
        // only returns name of the S3 status code symbol, like S3StatusErrorAccountProblem,
        // not something we should really display to an user, but still better than an internal error code
        Error = S3_get_status_name(Data.Status);
      }
      Details = Data.ErrorDetails;
    }

    throw ExtException(MainInstructions(Error), Details);
  }
}
//---------------------------------------------------------------------------
void TS3FileSystem::LibS3Deinitialize()
{
  TGuard Guard(LibS3Section.get());
  S3_deinitialize();
}
//---------------------------------------------------------------------------
void TS3FileSystem::ParsePath(UnicodeString Path, UnicodeString & BucketName, UnicodeString & Prefix)
{
  if (DebugAlwaysTrue(Path.SubString(1, 1) == L"/"))
  {
    Path.Delete(1, 1);
  }
  int P = Path.Pos(L"/");
  UnicodeString Result;
  if (P == 0)
  {
    BucketName = Path;
    Prefix = L"";
  }
  else
  {
    BucketName = Path.SubString(0, P - 1);
    Prefix = Path.SubString(P + 1, Path.Length() - P) + L"/";
  }
}
//---------------------------------------------------------------------------
UnicodeString TS3FileSystem::GetBucketRegion(const UnicodeString & BucketName)
{
  TRegions::const_iterator I = FRegions.find(BucketName);
  UnicodeString Result;
  if (I != FRegions.end())
  {
    Result = I->second;
  }
  else
  {
    Result = StrFromS3(S3_DEFAULT_REGION);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString TS3FileSystem::GetBucketHostName(const UnicodeString & BucketName)
{
  TRegions::const_iterator I = FHostNames.find(BucketName);
  UnicodeString Result;
  if (I != FHostNames.end())
  {
    Result = I->second;
  }
  else
  {
    Result = UnicodeString(FHostName);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::Close()
{
  DebugAssert(FActive);
  LibS3Deinitialize();
  FTerminal->Closed();
  FActive = false;
  UnregisterFromNeonDebug(FTerminal);
}
//---------------------------------------------------------------------------
bool __fastcall TS3FileSystem::GetActive()
{
  return FActive;
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CollectUsage()
{
  // noop
}
//---------------------------------------------------------------------------
const TSessionInfo & __fastcall TS3FileSystem::GetSessionInfo()
{
  return FSessionInfo;
}
//---------------------------------------------------------------------------
const TFileSystemInfo & __fastcall TS3FileSystem::GetFileSystemInfo(bool /*Retrieve*/)
{
  return FFileSystemInfo;
}
//---------------------------------------------------------------------------
bool __fastcall TS3FileSystem::TemporaryTransferFile(const UnicodeString & /*FileName*/)
{
  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TS3FileSystem::GetStoredCredentialsTried()
{
  // if we have one, we always try it
  return !FTerminal->SessionData->Password.IsEmpty();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TS3FileSystem::GetUserName()
{
  return UnicodeString(FAccessKeyId);
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::Idle()
{
  // noop
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TS3FileSystem::AbsolutePath(const UnicodeString Path, bool /*Local*/)
{
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
bool __fastcall TS3FileSystem::IsCapable(int Capability) const
{
  DebugAssert(FTerminal);
  switch (Capability)
  {
    // Only to make double-click on file edit/open the file,
    // instead of trying to open it as directory
    case fcResolveSymlink:
      return true;

    case fcRename:
    case fcRemoteMove:
    case fcMoveToQueue:
    case fcPreservingTimestampUpload:
    case fcCheckingSpaceAvailable:
    case fsSkipTransfer:
    case fsParallelTransfers:
    case fcRemoteCopy:
    case fcUserGroupListing:
    case fcModeChanging:
    case fcModeChangingUpload:
    case fcGroupChanging:
    case fcOwnerChanging:
    case fcAnyCommand:
    case fcShellAnyCommand:
    case fcHardLink:
    case fcSymbolicLink:
    case fcTextMode:
    case fcNativeTextMode:
    case fcNewerOnlyUpload:
    case fcTimestampChanging:
    case fcLoadingAdditionalProperties:
    case fcIgnorePermErrors:
    case fcCalculatingChecksum:
    case fcSecondaryShell:
    case fcGroupOwnerChangingByID:
    case fcRemoveCtrlZUpload:
    case fcRemoveBOMUpload:
    case fcPreservingTimestampDirs:
    case fcResumeSupport:
    case fcChangePassword:
    case fcLocking:
      return false;

    default:
      DebugFail();
      return false;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TS3FileSystem::GetCurrentDirectory()
{
  return FCurrentDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::DoStartup()
{
  FTerminal->SetExceptionOnFail(true);
  // retrieve initialize working directory to save it as home directory
  ReadCurrentDirectory();
  FTerminal->SetExceptionOnFail(false);
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::LookupUsersGroups()
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::ReadCurrentDirectory()
{
  if (FCachedDirectoryChange.IsEmpty())
  {
    FCurrentDirectory = FCurrentDirectory.IsEmpty() ? UnicodeString(L"/") : FCurrentDirectory;
  }
  else
  {
    FCurrentDirectory = FCachedDirectoryChange;
    FCachedDirectoryChange = L"";
  }
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::HomeDirectory()
{
  ChangeDirectory(L"/");
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::AnnounceFileListOperation()
{
  // noop
}
//---------------------------------------------------------------------------
void TS3FileSystem::TryOpenDirectory(const UnicodeString & Directory)
{
  FTerminal->LogEvent(FORMAT(L"Trying to open directory \"%s\".", (Directory)));
  std::unique_ptr<TRemoteFileList> FileList(new TRemoteFileList());
  ReadDirectoryInternal(Directory, FileList.get(), 1, UnicodeString());
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::ChangeDirectory(const UnicodeString ADirectory)
{
  UnicodeString Path = AbsolutePath(ADirectory, false);

  // to verify existence of directory try to open it
  TryOpenDirectory(Path);

  // if open dir did not fail, directory exists -> success.
  FCachedDirectoryChange = Path;
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CachedChangeDirectory(const UnicodeString Directory)
{
  FCachedDirectoryChange = UnixExcludeTrailingBackslash(Directory);
}
//---------------------------------------------------------------------------
TRemoteToken TS3FileSystem::MakeRemoteToken(const char * OwnerId, const char * OwnerDisplayName)
{
  TRemoteToken Result;
  Result.Name = StrFromS3(OwnerDisplayName);
  if (Result.Name.IsEmpty())
  {
    Result.Name = StrFromS3(OwnerId);
  }
  return Result;
}
//---------------------------------------------------------------------------
struct TLibS3ListServiceCallbackData : TLibS3CallbackData
{
  TRemoteFileList * FileList;
};
//---------------------------------------------------------------------------
S3Status TS3FileSystem::LibS3ListServiceCallback(
  const char * OwnerId, const char * OwnerDisplayName, const char * BucketName,
  int64_t /*CreationDate*/, void * CallbackData)
{
  TLibS3ListServiceCallbackData & Data = *static_cast<TLibS3ListServiceCallbackData *>(CallbackData);

  std::unique_ptr<TRemoteFile> File(new TRemoteFile(NULL));
  File->Terminal = Data.FileSystem->FTerminal;
  File->FileName = StrFromS3(BucketName);
  File->Type = FILETYPE_DIRECTORY;
  File->Owner = Data.FileSystem->MakeRemoteToken(OwnerId, OwnerDisplayName);
  File->ModificationFmt = mfNone;
  Data.FileList->AddFile(File.release());
  return S3StatusOK;
}
//---------------------------------------------------------------------------
struct TLibS3ListBucketCallbackData : TLibS3CallbackData
{
  TRemoteFileList * FileList;
  int KeyCount;
  UTF8String NextMarker;
  bool IsTruncated;
};
//---------------------------------------------------------------------------
S3Status TS3FileSystem::LibS3ListBucketCallback(
  int IsTruncated, const char * NextMarker, int ContentsCount, const S3ListBucketContent * Contents,
  int CommonPrefixesCount, const char ** CommonPrefixes, void * CallbackData)
{
  TLibS3ListBucketCallbackData & Data = *static_cast<TLibS3ListBucketCallbackData *>(CallbackData);

  Data.IsTruncated = IsTruncated;
  // This is being called in chunks, not once for all data in a response.
  Data.KeyCount += ContentsCount;
  Data.NextMarker = StrFromS3(NextMarker);

  for (int Index = 0; Index < ContentsCount; Index++)
  {
    const S3ListBucketContent * Content = &Contents[Index];
    UnicodeString FileName = UnixExtractFileName(StrFromS3(Content->key));
    if (!FileName.IsEmpty())
    {
      std::unique_ptr<TRemoteFile> File(new TRemoteFile(NULL));
      File->Terminal = Data.FileSystem->FTerminal;
      File->FileName = FileName;
      File->Type = FILETYPE_DEFAULT;
      File->Modification = UnixToDateTime(Content->lastModified, dstmWin);
      File->Size = Content->size;
      File->Owner = Data.FileSystem->MakeRemoteToken(Content->ownerId, Content->ownerDisplayName);
      Data.FileList->AddFile(File.release());
    }
  }

  for (int Index = 0; Index < CommonPrefixesCount; Index++)
  {
    std::unique_ptr<TRemoteFile> File(new TRemoteFile(NULL));
    File->Terminal = Data.FileSystem->FTerminal;
    File->FileName = UnixExtractFileName(UnixExcludeTrailingBackslash(StrFromS3(CommonPrefixes[Index])));
    File->Type = FILETYPE_DIRECTORY;
    File->ModificationFmt = mfNone;
    Data.FileList->AddFile(File.release());
  }

  return S3StatusOK;
}
//---------------------------------------------------------------------------
void TS3FileSystem::ReadDirectoryInternal(
  const UnicodeString & APath, TRemoteFileList * FileList, int MaxKeys, const UnicodeString & FileName)
{
  UnicodeString Path = AbsolutePath(APath, false);
  if (IsUnixRootPath(Path))
  {
    DebugAssert(FileList != NULL);
    DebugAssert(FileName.IsEmpty());

    S3ListServiceHandler ListServiceHandler =
    {
      { &LibS3ResponsePropertiesCallback, &LibS3ResponseCompleteCallback },
      &LibS3ListServiceCallback
    };

    RequestInit();

    TLibS3ListServiceCallbackData Data;
    Data.FileSystem = this;
    Data.FileList = FileList;

    S3_list_service(
      FLibS3Protocol, FAccessKeyId.c_str(), FSecretAccessKey.c_str(), 0, FHostName.c_str(),
      NULL, MaxKeys, FRequestContext, FTimeout, &ListServiceHandler, &Data);

    CheckLibS3Error(Data);
  }
  else
  {
    bool Retry;
    do
    {
      FileList->Clear();

      UnicodeString BucketName, Prefix;
      ParsePath(Path, BucketName, Prefix);
      Prefix += FileName;
      UTF8String BucketNameUtf = UTF8String(BucketName);
      UTF8String Region = UTF8String(GetBucketRegion(BucketName));
      UTF8String HostName = UTF8String(GetBucketHostName(BucketName));
      S3BucketContext BucketContext =
      {
        HostName.c_str(),
        BucketNameUtf.c_str(),
        FLibS3Protocol,
        S3UriStyleVirtualHost,
        FAccessKeyId.c_str(),
        FSecretAccessKey.c_str(),
        0,
        Region.c_str()
      };

      S3ListBucketHandler ListBucketHandler =
      {
        { &LibS3ResponsePropertiesCallback, &LibS3ResponseCompleteCallback },
        &LibS3ListBucketCallback
      };

      TLibS3ListBucketCallbackData Data;
      bool Continue;

      do
      {
        RequestInit();

        Data.FileSystem = this;
        Data.KeyCount = 0;
        Data.FileList = FileList;
        Data.IsTruncated = false;

        S3_list_bucket(
          &BucketContext, StrToS3(Prefix), StrToS3(Data.NextMarker),
          LibS3Delimiter.c_str(), MaxKeys, FRequestContext, FTimeout, &ListBucketHandler, &Data);

        Retry = RetryBucket(Data, BucketName);
        Continue = false;

        if (!Retry)
        {
          CheckLibS3Error(Data);

          if (Data.IsTruncated && ((MaxKeys == 0) || (Data.KeyCount < MaxKeys)))
          {
            bool Cancel = false;
            FTerminal->DoReadDirectoryProgress(FileList->Count, false, Cancel);
            if (!Cancel)
            {
              Continue = true;
            }
          }
        }

      } while (Continue);
    }
    while (Retry);

    if (MaxKeys != 1)
    {
      FileList->AddFile(new TRemoteParentDirectory(FTerminal));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::ReadDirectory(TRemoteFileList * FileList)
{
  TOperationVisualizer Visualizer(FTerminal->UseBusyCursor);
  ReadDirectoryInternal(FileList->Directory, FileList, 0, UnicodeString());
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::ReadSymlink(TRemoteFile * /*SymlinkFile*/,
  TRemoteFile *& /*File*/)
{
  // we never set SymLink flag, so we should never get here
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::ReadFile(const UnicodeString FileName,
  TRemoteFile *& File)
{
  TOperationVisualizer Visualizer(FTerminal->UseBusyCursor);

  UnicodeString FileNameOnly = UnixExtractFileName(FileName);
  std::unique_ptr<TRemoteFileList> FileList(new TRemoteFileList());
  ReadDirectoryInternal(UnixExtractFileDir(FileName), FileList.get(), 1, FileNameOnly);
  TRemoteFile * AFile = FileList->FindFile(FileNameOnly);
  if (AFile == NULL)
  {
    throw Exception(FMTLOAD(FILE_NOT_EXISTS, (FileName)));
  }
  File = AFile->Duplicate();
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::DeleteFile(const UnicodeString FileName,
  const TRemoteFile * /*File*/, int /*Params*/, TRmSessionAction & /*Action*/)
{
  throw Exception(L"Not implemented");
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::RenameFile(const UnicodeString FileName,
  const UnicodeString NewName)
{
  throw Exception(L"Not implemented");
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CopyFile(const UnicodeString FileName,
    const UnicodeString NewName)
{
  throw Exception(L"Not implemented");
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CreateDirectory(const UnicodeString DirName)
{
  throw Exception(L"Not implemented");
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CreateLink(const UnicodeString FileName,
  const UnicodeString PointTo, bool /*Symbolic*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::ChangeFileProperties(const UnicodeString FileName,
  const TRemoteFile * /*File*/, const TRemoteProperties * /*Properties*/,
  TChmodSessionAction & /*Action*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
bool __fastcall TS3FileSystem::LoadFilesProperties(TStrings * /*FileList*/)
{
  DebugFail();
  return false;
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CalculateFilesChecksum(const UnicodeString & /*Alg*/,
    TStrings * /*FileList*/, TStrings * /*Checksums*/,
    TCalculatedChecksumEvent /*OnCalculatedChecksum*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CustomCommandOnFile(const UnicodeString FileName,
  const TRemoteFile * /*File*/, UnicodeString Command, int /*Params*/, TCaptureOutputEvent /*OutputEvent*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::AnyCommand(const UnicodeString Command,
  TCaptureOutputEvent /*OutputEvent*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
TStrings * __fastcall TS3FileSystem::GetFixedPaths()
{
  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::SpaceAvailable(const UnicodeString Path,
  TSpaceAvailable & /*ASpaceAvailable*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CopyToRemote(TStrings * /*FilesToCopy*/,
  const UnicodeString ATargetDir, const TCopyParamType * /*CopyParam*/,
  int /*Params*/, TFileOperationProgressType * /*OperationProgress*/,
  TOnceDoneOperation & /*OnceDoneOperation*/)
{
  throw Exception(L"Not implemented");
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CopyToLocal(TStrings * /*FilesToCopy*/,
  const UnicodeString TargetDir, const TCopyParamType * /*CopyParam*/,
  int /*Params*/, TFileOperationProgressType * /*OperationProgress*/,
  TOnceDoneOperation & /*OnceDoneOperation*/)
{
  throw Exception(L"Not implemented");
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::GetSupportedChecksumAlgs(TStrings * /*Algs*/)
{
  // NOOP
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::LockFile(const UnicodeString & /*FileName*/, const TRemoteFile * /*File*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::UnlockFile(const UnicodeString & /*FileName*/, const TRemoteFile * /*File*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::UpdateFromMain(TCustomFileSystem * /*AMainFileSystem*/)
{
  // noop
}
//------------------------------------------------------------------------------
void __fastcall TS3FileSystem::ClearCaches()
{
  FRegions.clear();
  FHostNames.clear();
}
//------------------------------------------------------------------------------
