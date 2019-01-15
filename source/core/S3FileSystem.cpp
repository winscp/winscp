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
#include <StrUtils.hpp>
#include <limits>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define StrFromS3(S) StrFromNeon(S)
#define StrToS3(S) StrToNeon(S)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_TERMINAL FTerminal
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
UnicodeString __fastcall S3LibDefaultRegion()
{
  return StrFromS3(S3_DEFAULT_REGION);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
const int TS3FileSystem::S3MultiPartChunkSize = 5 * 1024 * 1024;
//---------------------------------------------------------------------------
TS3FileSystem::TS3FileSystem(TTerminal * ATerminal) :
  TCustomFileSystem(ATerminal),
  FActive(false),
  FResponseIgnore(false)
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
  FAuthRegion = DefaultStr(FTerminal->SessionData->S3DefaultRegion, S3LibDefaultRegion());

  RequireNeon(FTerminal);

  FTerminal->Information(LoadStr(STATUS_CONNECT), true);

  TSessionData * Data = FTerminal->SessionData;

  FSessionInfo.LoginTime = Now();
  FSessionInfo.CertificateVerifiedManually = false;

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

  UnicodeString SecretAccessKey = UTF8String(NormalizeString(Data->Password));
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
    UnicodeString Path = Data->RemoteDirectory;
    if (IsUnixRootPath(Path))
    {
      Path = ROOTDIRECTORY;
    }
    TryOpenDirectory(Path);
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
    FTerminal->LogEvent(0, CertificateVerificationMessage(Data));

    UnicodeString SiteKey = TSessionData::FormatSiteKey(FTerminal->SessionData->HostNameExpanded, FTerminal->SessionData->PortNumber);
    Result =
      FTerminal->VerifyCertificate(HttpsCertificateStorageKey, SiteKey, Data.Fingerprint, Data.Subject, Data.Failures);

    if (Result)
    {
      FSessionInfo.CertificateVerifiedManually = true;
    }
    else
    {
      UnicodeString Message;
      Result = NeonWindowsValidateCertificateWithMessage(Data, Message);
      FTerminal->LogEvent(0, Message);
    }

    FSessionInfo.Certificate = CertificateSummary(Data, FTerminal->SessionData->HostNameExpanded);

    if (!Result)
    {
      if (FTerminal->ConfirmCertificate(FSessionInfo, Data.Failures, HttpsCertificateStorageKey, true))
      {
        Result = true;
        FSessionInfo.CertificateVerifiedManually = true;
      }
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
  FTerminal->LogEvent(0, Message);
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
  if (FileSystem->FTerminal->Log->Logging && !FileSystem->FResponseIgnore)
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

  if (!FileSystem->FResponse.IsEmpty() && (FileSystem->FTerminal->Configuration->ActualLogProtocol >= 0))
  {
    FileSystem->FTerminal->Log->Add(llOutput, FileSystem->FResponse);
  }
}
//---------------------------------------------------------------------------
void TS3FileSystem::RequestInit(TLibS3CallbackData & Data)
{
  Data.FileSystem = this;
  FResponse = L"";
}
//---------------------------------------------------------------------------
void TS3FileSystem::CheckLibS3Error(const TLibS3CallbackData & Data, bool FatalOnConnectError)
{
  if (Data.Status != S3StatusOK)
  {
    UnicodeString Error, Details;
    bool FatalCandidate = false;
    switch (Data.Status)
    {
      case S3StatusAbortedByCallback:
        Error = LoadStr(USER_TERMINATED);
        break;

      case S3StatusErrorAccessDenied:
        Error = LoadStr(S3_STATUS_ACCESS_DENIED);
        break;

      case S3StatusErrorSignatureDoesNotMatch: // While it can mean an implementation fault, it will typically mean a wrong secure key.
      case S3StatusErrorInvalidAccessKeyId:
        Error = LoadStr(AUTHENTICATION_FAILED);
        break;

      case S3StatusNameLookupError:
        Error = ReplaceStr(LoadStr(NET_TRANSL_HOST_NOT_EXIST2), L"%HOST%", FTerminal->SessionData->HostNameExpanded);
        FatalCandidate = true;
        break;

      case S3StatusFailedToConnect:
        Error = LoadStr(CONNECTION_FAILED);
        FatalCandidate = true;
        break;

      case S3StatusConnectionFailed:
        FatalCandidate = true;
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

    Error = MainInstructions(Error);

    if (FatalCandidate && FatalOnConnectError)
    {
      throw EFatal(NULL, Error, Details);
    }
    else
    {
      throw ExtException(Error, Details);
    }
  }
}
//---------------------------------------------------------------------------
void TS3FileSystem::LibS3Deinitialize()
{
  TGuard Guard(LibS3Section.get());
  S3_deinitialize();
}
//---------------------------------------------------------------------------
UnicodeString TS3FileSystem::GetFolderKey(const UnicodeString & Key)
{
  return Key + L"/";
}
//---------------------------------------------------------------------------
void TS3FileSystem::ParsePath(UnicodeString Path, UnicodeString & BucketName, UnicodeString & Key)
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
    Key = L"";
  }
  else
  {
    BucketName = Path.SubString(0, P - 1);
    Key = Path.SubString(P + 1, Path.Length() - P);
  }
}
//---------------------------------------------------------------------------
struct TLibS3BucketContext : S3BucketContext
{
  // These keep data that we point the native S3BucketContext fields to
  UTF8String HostNameBuf;
  UTF8String BucketNameBuf;
  UTF8String AuthRegionBuf;
};
//---------------------------------------------------------------------------
struct TLibS3ListBucketCallbackData : TLibS3CallbackData
{
  TRemoteFileList * FileList;
  int KeyCount;
  UTF8String NextMarker;
  bool IsTruncated;
};
//---------------------------------------------------------------------------
TLibS3BucketContext TS3FileSystem::GetBucketContext(const UnicodeString & BucketName, const UnicodeString & Prefix)
{
  TLibS3BucketContext Result;

  bool First = true;
  bool Retry = false;
  do
  {
    TRegions::const_iterator I;
    I = FRegions.find(BucketName);
    UnicodeString Region;
    if (I != FRegions.end())
    {
      Region = I->second;
    }
    else
    {
      Region = FAuthRegion;
      if (First)
      {
        FTerminal->LogEvent(FORMAT(L"Unknown bucket \"%s\", will detect its region (and service endpoint)", (BucketName)));
        First = false;
      }
      Retry = true;
    }

    I = FHostNames.find(BucketName);
    UnicodeString HostName;
    if (I != FHostNames.end())
    {
      HostName = I->second;
    }
    else
    {
      HostName = UnicodeString(FHostName);
    }

    Result.HostNameBuf = UTF8String(HostName);
    Result.hostName = Result.HostNameBuf.c_str();
    Result.BucketNameBuf = UTF8String(BucketName);
    Result.bucketName = Result.BucketNameBuf.c_str();
    Result.protocol = FLibS3Protocol;
    Result.uriStyle = S3UriStyleVirtualHost;
    Result.accessKeyId = FAccessKeyId.c_str();
    Result.secretAccessKey = FSecretAccessKey.c_str();
    Result.securityToken = NULL;
    Result.AuthRegionBuf = UTF8String(Region);
    Result.authRegion = Result.AuthRegionBuf.c_str();

    if (Retry)
    {
      std::unique_ptr<TRemoteFileList> FileList(new TRemoteFileList());
      TLibS3ListBucketCallbackData Data;
      // Using prefix for which we need the bucket, as the account may have access to that prefix only (using "Condition" in policy)
      DoListBucket(Prefix, FileList.get(), 1, Result, Data);

      Retry = false;
      UnicodeString EndpointDetail = Data.EndpointDetail;
      if ((Data.Status == S3StatusErrorAuthorizationHeaderMalformed) &&
          (Region != Data.RegionDetail))
      {
        FTerminal->LogEvent(FORMAT("Will use region \"%s\" for bucket \"%s\" from now on.", (Data.RegionDetail, BucketName)));
        FRegions.insert(std::make_pair(BucketName, Data.RegionDetail));

        Result.AuthRegionBuf = UTF8String(Data.RegionDetail);
        Result.authRegion = Result.AuthRegionBuf.c_str();
      }
      // happens with newly created buckets (and happens before the region redirect)
      else if (((Data.Status == S3StatusErrorTemporaryRedirect) || (Data.Status == S3StatusErrorPermanentRedirect)) &&
               !Data.EndpointDetail.IsEmpty())
      {
        UnicodeString Endpoint = Data.EndpointDetail;
        if (SameText(Endpoint.SubString(1, BucketName.Length() + 1), BucketName + L"."))
        {
          Endpoint.Delete(1, BucketName.Length() + 1);
        }
        if (HostName != Endpoint)
        {
          FTerminal->LogEvent(FORMAT("Will use endpoint \"%s\" for bucket \"%s\" from now on.", (Endpoint, BucketName)));
          FHostNames.insert(std::make_pair(BucketName, Endpoint));
          Retry = true;
        }
      }
    }
  }
  while (Retry);

  return Result;
}
//---------------------------------------------------------------------------
#define CreateResponseHandlerCustom(PropertiesCallback) { &PropertiesCallback, &LibS3ResponseCompleteCallback }
#define CreateResponseHandler() CreateResponseHandlerCustom(LibS3ResponsePropertiesCallback)
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
    case fcRemoteCopy:
    case fcRename:
    case fcRemoteMove:
    case fcMoveToQueue:
    case fsSkipTransfer:
    case fsParallelTransfers:
      return true;

    case fcPreservingTimestampUpload:
    case fcCheckingSpaceAvailable:
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
  UnicodeString FileName; // filter for buckets
};
//---------------------------------------------------------------------------
S3Status TS3FileSystem::LibS3ListServiceCallback(
  const char * OwnerId, const char * OwnerDisplayName, const char * BucketName,
  int64_t /*CreationDate*/, void * CallbackData)
{
  TLibS3ListServiceCallbackData & Data = *static_cast<TLibS3ListServiceCallbackData *>(CallbackData);

  UnicodeString FileName = StrFromS3(BucketName);
  if (Data.FileName.IsEmpty() || (Data.FileName == FileName))
  {
    std::unique_ptr<TRemoteFile> File(new TRemoteFile(NULL));
    File->Terminal = Data.FileSystem->FTerminal;
    File->FileName = StrFromS3(BucketName);
    File->Type = FILETYPE_DIRECTORY;
    File->Owner = Data.FileSystem->MakeRemoteToken(OwnerId, OwnerDisplayName);
    File->ModificationFmt = mfNone;
    Data.FileList->AddFile(File.release());
  }

  return S3StatusOK;
}
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
    else
    {
      // We needs this to distinguish empty and non-existing folders, see comments in ReadDirectoryInternal.
      Data.FileList->AddFile(new TRemoteParentDirectory(Data.FileSystem->FTerminal));
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
void TS3FileSystem::DoListBucket(
  const UnicodeString & Prefix, TRemoteFileList * FileList, int MaxKeys, const TLibS3BucketContext & BucketContext,
  TLibS3ListBucketCallbackData & Data)
{
  S3ListBucketHandler ListBucketHandler = { CreateResponseHandler(), &LibS3ListBucketCallback };
  RequestInit(Data);
  Data.KeyCount = 0;
  Data.FileList = FileList;
  Data.IsTruncated = false;

  S3_list_bucket(
    &BucketContext, StrToS3(Prefix), StrToS3(Data.NextMarker),
    LibS3Delimiter.c_str(), MaxKeys, FRequestContext, FTimeout, &ListBucketHandler, &Data);
}
//---------------------------------------------------------------------------
void TS3FileSystem::HandleNonBucketStatus(TLibS3CallbackData & Data, bool & Retry)
{
  if ((Data.Status == S3StatusErrorAuthorizationHeaderMalformed) &&
      (FAuthRegion != Data.RegionDetail))
  {
    FTerminal->LogEvent(FORMAT("Will use authentication region \"%s\" from now on.", (Data.RegionDetail)));
    FAuthRegion = Data.RegionDetail;
    Retry = true;
  }
}
//---------------------------------------------------------------------------
void TS3FileSystem::ReadDirectoryInternal(
  const UnicodeString & APath, TRemoteFileList * FileList, int MaxKeys, const UnicodeString & FileName)
{
  UnicodeString Path = UnixExcludeTrailingBackslash(AbsolutePath(APath, false));
  if (IsUnixRootPath(Path))
  {
    DebugAssert(FileList != NULL);

    TLibS3ListServiceCallbackData Data;
    Data.FileList = FileList;
    Data.FileName = FileName;

    bool Retry;
    do
    {
      RequestInit(Data);

      S3ListServiceHandler ListServiceHandler = { CreateResponseHandler(), &LibS3ListServiceCallback };

      Retry = false;

      S3_list_service(
        FLibS3Protocol, FAccessKeyId.c_str(), FSecretAccessKey.c_str(), 0, FHostName.c_str(),
        StrToS3(FAuthRegion), MaxKeys, FRequestContext, FTimeout, &ListServiceHandler, &Data);

      HandleNonBucketStatus(Data, Retry);
    }
    while (Retry);

    CheckLibS3Error(Data);
  }
  else
  {
    UnicodeString BucketName, Prefix;
    ParsePath(Path, BucketName, Prefix);
    if (!Prefix.IsEmpty())
    {
      Prefix = GetFolderKey(Prefix);
    }
    Prefix += FileName;
    TLibS3BucketContext BucketContext = GetBucketContext(BucketName, Prefix);

    TLibS3ListBucketCallbackData Data;
    bool Continue;

    do
    {
      DoListBucket(Prefix, FileList, MaxKeys, BucketContext, Data);
      CheckLibS3Error(Data);

      Continue = false;

      if (Data.IsTruncated && ((MaxKeys == 0) || (Data.KeyCount < MaxKeys)))
      {
        bool Cancel = false;
        FTerminal->DoReadDirectoryProgress(FileList->Count, false, Cancel);
        if (!Cancel)
        {
          Continue = true;
        }
      }
    } while (Continue);

    // Listing bucket root directory will report an error if the bucket does not exist.
    // But there won't be any prefix/ entry, so no ".." entry is created, so we have to add it explicitly
    if (Prefix.IsEmpty())
    {
      FileList->AddFile(new TRemoteParentDirectory(FTerminal));
    }
    else
    {
      // We do not get any error, when the "prefix" does not exist. But when prefix does exist, there's at least
      // prefix/ entry (translated to ..). If there's none, it means that the path does not exist.
      // When called from DoReadFile (FileName is set), leaving error handling to the caller.
      if ((FileList->Count == 0) && FileName.IsEmpty())
      {
        throw Exception(FMTLOAD(FILE_NOT_EXISTS, (APath)));
      }
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
void TS3FileSystem::DoReadFile(const UnicodeString & FileName, TRemoteFile *& File)
{
  UnicodeString FileNameOnly = UnixExtractFileName(FileName);
  std::unique_ptr<TRemoteFileList> FileList(new TRemoteFileList());
  ReadDirectoryInternal(UnixExtractFileDir(FileName), FileList.get(), 1, FileNameOnly);
  TRemoteFile * AFile = FileList->FindFile(FileNameOnly);
  if (AFile != NULL)
  {
    File = AFile->Duplicate();
  }
  else
  {
    File = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::ReadFile(const UnicodeString FileName,
  TRemoteFile *& File)
{
  TOperationVisualizer Visualizer(FTerminal->UseBusyCursor);
  DoReadFile(FileName, File);
  if (File == NULL)
  {
    throw Exception(FMTLOAD(FILE_NOT_EXISTS, (FileName)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::DeleteFile(const UnicodeString AFileName,
  const TRemoteFile * File, int Params, TRmSessionAction & Action)
{
  UnicodeString FileName = AbsolutePath(AFileName, false);

  bool Dir = FTerminal->DeleteContentsIfDirectory(FileName, File, Params, Action);

  UnicodeString BucketName, Key;
  ParsePath(FileName, BucketName, Key);

  if (!Key.IsEmpty() && Dir)
  {
    Key = GetFolderKey(Key);
  }

  TLibS3BucketContext BucketContext = GetBucketContext(BucketName, Key);

  S3ResponseHandler ResponseHandler = CreateResponseHandler();

  TLibS3CallbackData Data;
  RequestInit(Data);

  if (Key.IsEmpty())
  {
    S3_delete_bucket(
      BucketContext.protocol, BucketContext.uriStyle, BucketContext.accessKeyId, BucketContext.secretAccessKey,
      BucketContext.securityToken, BucketContext.hostName, BucketContext.bucketName, BucketContext.authRegion,
      FRequestContext, FTimeout, &ResponseHandler, &Data);
  }
  else
  {
    S3_delete_object(&BucketContext, StrToS3(Key), FRequestContext, FTimeout, &ResponseHandler, &Data);
  }

  CheckLibS3Error(Data);
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::RenameFile(const UnicodeString FileName, const TRemoteFile * File,
  const UnicodeString NewName)
{
  if (DebugAlwaysTrue(File != NULL) && File->IsDirectory)
  {
    throw Exception(LoadStr(NOTSUPPORTED));
  }
  CopyFile(FileName, File, NewName);
  TRmSessionAction DummyAction(FTerminal->ActionLog, FileName);
  DeleteFile(FileName, File, dfForceDelete, DummyAction);
  DummyAction.Cancel();
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CopyFile(const UnicodeString AFileName, const TRemoteFile * File,
  const UnicodeString ANewName)
{
  if (DebugAlwaysTrue(File != NULL) && File->IsDirectory)
  {
    throw Exception(LoadStr(DUPLICATE_FOLDER_NOT_SUPPORTED));
  }

  UnicodeString FileName = AbsolutePath(AFileName, false);
  UnicodeString NewName = AbsolutePath(ANewName, false);

  UnicodeString SourceBucketName, SourceKey;
  ParsePath(FileName, SourceBucketName, SourceKey);
  DebugAssert(!SourceKey.IsEmpty()); // it's not a folder, so it cannot be a bucket or root

  UnicodeString DestBucketName, DestKey;
  ParsePath(NewName, DestBucketName, DestKey);

  if (DestKey.IsEmpty())
  {
    throw Exception(LoadStr(MISSING_TARGET_BUCKET));
  }

  TLibS3BucketContext BucketContext = GetBucketContext(DestBucketName, DestKey);
  BucketContext.BucketNameBuf = SourceBucketName;
  BucketContext.bucketName = BucketContext.BucketNameBuf.c_str();

  S3ResponseHandler ResponseHandler = CreateResponseHandler();

  TLibS3CallbackData Data;
  RequestInit(Data);

  S3_copy_object(
    &BucketContext, StrToS3(SourceKey), StrToS3(DestBucketName), StrToS3(DestKey),
    NULL, NULL, 0, NULL, FRequestContext, FTimeout, &ResponseHandler, &Data);

  CheckLibS3Error(Data);
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CreateDirectory(const UnicodeString & ADirName, bool /*Encrypt*/)
{
  TOperationVisualizer Visualizer(FTerminal->UseBusyCursor);
  UnicodeString DirName = UnixExcludeTrailingBackslash(AbsolutePath(ADirName, false));

  UnicodeString BucketName, Key;
  ParsePath(DirName, BucketName, Key);

  if (Key.IsEmpty())
  {
    S3ResponseHandler ResponseHandler = CreateResponseHandler();

    // Not using GetBucketContext here, as the bucket does not exist

    UTF8String RegionBuf;
    char * Region = NULL;
    if (!FTerminal->SessionData->S3DefaultRegion.IsEmpty() &&
        (FTerminal->SessionData->S3DefaultRegion != S3LibDefaultRegion()))
    {
      RegionBuf = UTF8String(FTerminal->SessionData->S3DefaultRegion);
      Region = RegionBuf.c_str();
    }

    TLibS3CallbackData Data;

    bool Retry;
    do
    {
      RequestInit(Data);

      Retry = false;

      S3_create_bucket(
        FLibS3Protocol, FAccessKeyId.c_str(), FSecretAccessKey.c_str(), NULL, FHostName.c_str(), StrToS3(BucketName),
        StrToS3(FAuthRegion), S3CannedAclPrivate, Region, FRequestContext, FTimeout, &ResponseHandler, &Data);

      HandleNonBucketStatus(Data, Retry);
    }
    while (Retry);

    CheckLibS3Error(Data);
  }
  else
  {
    TLibS3CallbackData Data;
    RequestInit(Data);

    Key = GetFolderKey(Key);

    TLibS3BucketContext BucketContext = GetBucketContext(BucketName, Key);

    S3PutObjectHandler PutObjectHandler = { CreateResponseHandler(), NULL };

    S3_put_object(&BucketContext, StrToS3(Key), 0, NULL, FRequestContext, FTimeout, &PutObjectHandler, &Data);

    CheckLibS3Error(Data);
  }
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
void __fastcall TS3FileSystem::CopyToRemote(
  TStrings * FilesToCopy, const UnicodeString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress, TOnceDoneOperation & OnceDoneOperation)
{
  Params &= ~cpAppend;

  FTerminal->DoCopyToRemote(FilesToCopy, TargetDir, CopyParam, Params, OperationProgress, tfPreCreateDir, OnceDoneOperation);
}
//---------------------------------------------------------------------------
void TS3FileSystem::ConfirmOverwrite(
  const UnicodeString & SourceFullFileName, UnicodeString & TargetFileName,
  TFileOperationProgressType * OperationProgress, const TOverwriteFileParams * FileParams,
  const TCopyParamType * CopyParam, int Params)
{
  int Answers = qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll;
  std::vector<TQueryButtonAlias> Aliases;
  Aliases.push_back(TQueryButtonAlias::CreateYesToAllGrouppedWithYes());
  Aliases.push_back(TQueryButtonAlias::CreateNoToAllGrouppedWithNo());

  TQueryParams QueryParams(qpNeverAskAgainCheck);
  QueryParams.Aliases = &Aliases[0];
  QueryParams.AliasesCount = Aliases.size();

  unsigned int Answer;

  {
    TSuspendFileOperationProgress Suspend(OperationProgress);
    Answer =
      FTerminal->ConfirmFileOverwrite(
        SourceFullFileName, TargetFileName, FileParams, Answers, &QueryParams,
        ReverseOperationSide(OperationProgress->Side),
        CopyParam, Params, OperationProgress);
  }

  switch (Answer)
  {
    case qaYes:
      // noop
      break;

    case qaNo:
      throw ESkipFile();

    default:
      DebugFail();
    case qaCancel:
      OperationProgress->SetCancelAtLeast(csCancel);
      Abort();
      break;
  }
}
//---------------------------------------------------------------------------
struct TLibS3TransferObjectDataCallbackData : TLibS3CallbackData
{
  UnicodeString FileName;
  TStream * Stream;
  TFileOperationProgressType * OperationProgress;
  std::auto_ptr<Exception> Exception;
};
//---------------------------------------------------------------------------
struct TLibS3PutObjectDataCallbackData : TLibS3TransferObjectDataCallbackData
{
  RawByteString ETag;
};
//---------------------------------------------------------------------------
int TS3FileSystem::LibS3PutObjectDataCallback(int BufferSize, char * Buffer, void * CallbackData)
{
  TLibS3PutObjectDataCallbackData & Data = *static_cast<TLibS3PutObjectDataCallbackData *>(CallbackData);

  return Data.FileSystem->PutObjectData(BufferSize, Buffer, Data);
}
//---------------------------------------------------------------------------
bool TS3FileSystem::ShouldCancelTransfer(TLibS3TransferObjectDataCallbackData & Data)
{
  bool Result = (Data.OperationProgress->Cancel != csContinue);
  if (Result)
  {
    if (Data.OperationProgress->ClearCancelFile())
    {
      Data.Exception.reset(new ESkipFile());
    }
    else
    {
      Data.Exception.reset(new EAbort(L""));
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
int TS3FileSystem::PutObjectData(int BufferSize, char * Buffer, TLibS3PutObjectDataCallbackData & Data)
{
  int Result;

  if (ShouldCancelTransfer(Data))
  {
    Result = -1;
  }
  else
  {
    TFileOperationProgressType * OperationProgress = Data.OperationProgress;
    try
    {
      FILE_OPERATION_LOOP_BEGIN
      {
        Result = Data.Stream->Read(Buffer, BufferSize);
      }
      FILE_OPERATION_LOOP_END(FMTLOAD(READ_ERROR, (Data.FileName)));

      OperationProgress->AddTransferred(Result);
    }
    catch (Exception & E)
    {
      Data.Exception.reset(CloneException(&E));
      Result = -1;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
struct TLibS3MultipartInitialCallbackData : TLibS3CallbackData
{
  RawByteString UploadId;
};
//---------------------------------------------------------------------------
S3Status TS3FileSystem::LibS3MultipartInitialCallback(const char * UploadId, void * CallbackData)
{
  TLibS3MultipartInitialCallbackData & Data = *static_cast<TLibS3MultipartInitialCallbackData *>(CallbackData);

  Data.UploadId = UploadId;

  return S3StatusOK;
}
//---------------------------------------------------------------------------
struct TLibS3MultipartCommitPutObjectDataCallbackData : TLibS3CallbackData
{
  RawByteString Message;
  int Remaining;
};
//---------------------------------------------------------------------------
S3Status TS3FileSystem::LibS3MultipartResponsePropertiesCallback(
  const S3ResponseProperties * Properties, void * CallbackData)
{
  S3Status Result = LibS3ResponsePropertiesCallback(Properties, CallbackData);

  TLibS3PutObjectDataCallbackData & Data = *static_cast<TLibS3PutObjectDataCallbackData *>(CallbackData);

  Data.ETag = Properties->eTag;

  return Result;
}
//---------------------------------------------------------------------------
int TS3FileSystem::LibS3MultipartCommitPutObjectDataCallback(int BufferSize, char * Buffer, void * CallbackData)
{
  TLibS3MultipartCommitPutObjectDataCallbackData & Data =
    *static_cast<TLibS3MultipartCommitPutObjectDataCallbackData *>(CallbackData);
  int Result = 0;
  if (Data.Remaining > 0)
  {
    Result = std::min(BufferSize, Data.Remaining);
    memcpy(Buffer, Data.Message.c_str() + Data.Message.Length() - Data.Remaining, Result);
    Data.Remaining -= Result;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::Source(
  TLocalFileHandle & Handle, const UnicodeString & TargetDir, UnicodeString & DestFileName,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int /*Flags*/,
  TUploadSessionAction & Action, bool & /*ChildError*/)
{
  UnicodeString DestFullName = TargetDir + DestFileName;

  TRemoteFile * RemoteFile = NULL;
  try
  {
    // Should not throw on non-existing file by purpose (mainly not to get an exception while debugging)
    DoReadFile(DestFullName, RemoteFile);
  }
  catch (...)
  {
    // Pointless, as there's no persistent connection.
    if (!FTerminal->Active)
    {
      throw;
    }
  }

  if (RemoteFile != NULL)
  {
    TOverwriteFileParams FileParams;

    FileParams.SourceSize = Handle.Size;
    FileParams.SourceTimestamp = Handle.Modification;
    FileParams.DestSize = RemoteFile->Size;
    FileParams.DestTimestamp = TDateTime();
    FileParams.DestPrecision = mfNone;
    delete RemoteFile;

    ConfirmOverwrite(Handle.FileName, DestFileName, OperationProgress, &FileParams, CopyParam, Params);
  }

  DestFullName = TargetDir + DestFileName;
  // only now, we know the final destination
  // (not really true as we do not support changing file name on overwrite dialog)
  Action.Destination(DestFullName);

  UnicodeString BucketName, Key;
  ParsePath(DestFullName, BucketName, Key);

  if (Key.IsEmpty())
  {
    throw Exception(LoadStr(MISSING_TARGET_BUCKET));
  }

  TLibS3BucketContext BucketContext = GetBucketContext(BucketName, Key);

  UTF8String ContentType = UTF8String(FTerminal->Configuration->GetFileMimeType(Handle.FileName));
  S3PutProperties PutProperties =
    {
      (ContentType.IsEmpty() ? NULL : ContentType.c_str()),
      NULL,
      NULL,
      NULL,
      NULL,
      -1,
      S3CannedAclPrivate,
      0,
      NULL,
      0
    };

  int Parts = std::max(1, static_cast<int>((Handle.Size + S3MultiPartChunkSize - 1) / S3MultiPartChunkSize));
  bool Multipart = (Parts > 1);

  RawByteString MultipartUploadId;
  TLibS3MultipartCommitPutObjectDataCallbackData MultipartCommitPutObjectDataCallbackData;

  if (Multipart)
  {
    FTerminal->LogEvent(FORMAT(L"Initiating multipart upload (%d parts)", (Parts)));

    FILE_OPERATION_LOOP_BEGIN
    {
      TLibS3MultipartInitialCallbackData Data;
      RequestInit(Data);

      S3MultipartInitialHandler Handler = { CreateResponseHandler(), &LibS3MultipartInitialCallback };

      S3_initiate_multipart(&BucketContext, StrToS3(Key), &PutProperties, &Handler, FRequestContext, FTimeout, &Data);

      CheckLibS3Error(Data, true);

      MultipartUploadId = Data.UploadId;
    }
    FILE_OPERATION_LOOP_END_EX(FMTLOAD(TRANSFER_ERROR, (Handle.FileName)), (folAllowSkip | folRetryOnFatal));

    FTerminal->LogEvent(FORMAT(L"Initiated multipart upload (%s - %d parts)", (UnicodeString(MultipartUploadId), Parts)));

    MultipartCommitPutObjectDataCallbackData.Message += "<CompleteMultipartUpload>\n";
  }

  try
  {
    TLibS3PutObjectDataCallbackData Data;

    __int64 Position = 0;

    std::unique_ptr<TStream> Stream(new TSafeHandleStream(reinterpret_cast<THandle>(Handle.Handle)));

    for (int Part = 1; Part <= Parts; Part++)
    {
      FILE_OPERATION_LOOP_BEGIN
      {
        DebugAssert(Stream->Position == OperationProgress->TransferredSize);

        // If not, it's chunk retry and we have to undo the unsuccessful chunk upload
        if (Position < Stream->Position)
        {
          Stream->Position = Position;
          OperationProgress->AddTransferred(Position - OperationProgress->TransferredSize);
        }

        RequestInit(Data);
        Data.FileName = Handle.FileName;
        Data.Stream = Stream.get();
        Data.OperationProgress = OperationProgress;
        Data.Exception.reset(NULL);

        if (Multipart)
        {
          S3PutObjectHandler UploadPartHandler =
            { CreateResponseHandlerCustom(LibS3MultipartResponsePropertiesCallback), LibS3PutObjectDataCallback };
          __int64 Remaining = Stream->Size - Stream->Position;
          int RemainingInt = static_cast<int>(std::min(static_cast<__int64>(std::numeric_limits<int>::max()), Remaining));
          int PartLength = std::min(S3MultiPartChunkSize, RemainingInt);
          FTerminal->LogEvent(FORMAT(L"Uploading part %d [%s]", (Part, IntToStr(PartLength))));
          S3_upload_part(
            &BucketContext, StrToS3(Key), &PutProperties, &UploadPartHandler, Part, MultipartUploadId.c_str(),
            PartLength, FRequestContext, FTimeout, &Data);
        }
        else
        {
          S3PutObjectHandler PutObjectHandler = { CreateResponseHandler(), LibS3PutObjectDataCallback };
          S3_put_object(&BucketContext, StrToS3(Key), Handle.Size, &PutProperties, FRequestContext, FTimeout, &PutObjectHandler, &Data);
        }

        // The "exception" was already seen by the user, its presence mean an accepted abort of the operation.
        if (Data.Exception.get() == NULL)
        {
          CheckLibS3Error(Data, true);
        }

        Position = Stream->Position;

        if (Multipart)
        {
          RawByteString PartCommitTag =
            RawByteString::Format("  <Part><PartNumber>%d</PartNumber><ETag>%s</ETag></Part>\n", ARRAYOFCONST((Part, Data.ETag)));
          MultipartCommitPutObjectDataCallbackData.Message += PartCommitTag;
        }
      }
      FILE_OPERATION_LOOP_END_EX(FMTLOAD(TRANSFER_ERROR, (Handle.FileName)), (folAllowSkip | folRetryOnFatal));

      if (Data.Exception.get() != NULL)
      {
        RethrowException(Data.Exception.get());
      }
    }

    Stream.reset(NULL);

    if (Multipart)
    {
      MultipartCommitPutObjectDataCallbackData.Message += "</CompleteMultipartUpload>\n";

      FTerminal->LogEvent(FORMAT(L"Committing multipart upload (%s - %d parts)", (UnicodeString(MultipartUploadId), Parts)));
      FTerminal->LogEvent(UnicodeString(MultipartCommitPutObjectDataCallbackData.Message));

      FILE_OPERATION_LOOP_BEGIN
      {
        RequestInit(MultipartCommitPutObjectDataCallbackData);

        MultipartCommitPutObjectDataCallbackData.Remaining = MultipartCommitPutObjectDataCallbackData.Message.Length();

        S3MultipartCommitHandler MultipartCommitHandler =
          { CreateResponseHandler(), &LibS3MultipartCommitPutObjectDataCallback, NULL };

        S3_complete_multipart_upload(
          &BucketContext, StrToS3(Key), &MultipartCommitHandler, MultipartUploadId.c_str(),
          MultipartCommitPutObjectDataCallbackData.Remaining,
          FRequestContext, FTimeout, &MultipartCommitPutObjectDataCallbackData);

        CheckLibS3Error(MultipartCommitPutObjectDataCallbackData, true);
      }
      FILE_OPERATION_LOOP_END_EX(FMTLOAD(TRANSFER_ERROR, (Handle.FileName)), (folAllowSkip | folRetryOnFatal));

      // to skip abort, in case we ever add any code before the catch, that can throw
      MultipartUploadId = RawByteString();
    }
  }
  catch (Exception & E)
  {
    if (!MultipartUploadId.IsEmpty())
    {
      FTerminal->LogEvent(FORMAT(L"Aborting multipart upload (%s - %d parts)", (UnicodeString(MultipartUploadId), Parts)));

      try
      {
        TLibS3CallbackData Data;
        RequestInit(Data);

        S3AbortMultipartUploadHandler AbortMultipartUploadHandler = { CreateResponseHandler() };

        S3_abort_multipart_upload(
          &BucketContext, StrToS3(Key), MultipartUploadId.c_str(),
          FTimeout, &AbortMultipartUploadHandler, FRequestContext, &Data);
      }
      catch (...)
      {
        // swallow
      }
    }

    throw;
  }
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CopyToLocal(
  TStrings * FilesToCopy, const UnicodeString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress, TOnceDoneOperation & OnceDoneOperation)
{
  Params &= ~cpAppend;

  FTerminal->DoCopyToLocal(FilesToCopy, TargetDir, CopyParam, Params, OperationProgress, tfNone, OnceDoneOperation);
}
//---------------------------------------------------------------------------
struct TLibS3GetObjectDataCallbackData : TLibS3TransferObjectDataCallbackData
{
};
//---------------------------------------------------------------------------
S3Status TS3FileSystem::LibS3GetObjectDataCallback(int BufferSize, const char * Buffer, void * CallbackData)
{
  TLibS3GetObjectDataCallbackData & Data = *static_cast<TLibS3GetObjectDataCallbackData *>(CallbackData);

  return Data.FileSystem->GetObjectData(BufferSize, Buffer, Data);
}
//---------------------------------------------------------------------------
S3Status TS3FileSystem::GetObjectData(int BufferSize, const char * Buffer, TLibS3GetObjectDataCallbackData & Data)
{
  S3Status Result = S3StatusOK;

  if (ShouldCancelTransfer(Data))
  {
    Result = S3StatusAbortedByCallback;
  }
  else
  {
    TFileOperationProgressType * OperationProgress = Data.OperationProgress;
    try
    {
      FILE_OPERATION_LOOP_BEGIN
      {
        Data.Stream->Write(Buffer, BufferSize);
      }
      FILE_OPERATION_LOOP_END(FMTLOAD(WRITE_ERROR, (Data.FileName)));

      OperationProgress->AddTransferred(BufferSize);
    }
    catch (Exception & E)
    {
      Data.Exception.reset(CloneException(&E));
      Result = S3StatusAbortedByCallback;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::Sink(
  const UnicodeString & FileName, const TRemoteFile * File,
  const UnicodeString & TargetDir, UnicodeString & DestFileName, int Attrs,
  const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress,
  unsigned int /*Flags*/, TDownloadSessionAction & Action)
{
  UnicodeString DestFullName = TargetDir + DestFileName;
  if (FileExists(ApiPath(DestFullName)))
  {
    __int64 Size;
    __int64 MTime;
    FTerminal->OpenLocalFile(DestFullName, GENERIC_READ, NULL, NULL, NULL, &MTime, NULL, &Size);
    TOverwriteFileParams FileParams;

    FileParams.SourceSize = File->Size;
    FileParams.SourceTimestamp = File->Modification; // noop
    FileParams.DestSize = Size;
    FileParams.DestTimestamp = UnixToDateTime(MTime, FTerminal->SessionData->DSTMode);

    ConfirmOverwrite(FileName, DestFileName, OperationProgress, &FileParams, CopyParam, Params);
  }

  UnicodeString BucketName, Key;
  ParsePath(FileName, BucketName, Key);

  TLibS3BucketContext BucketContext = GetBucketContext(BucketName, Key);

  UnicodeString ExpandedDestFullName = ExpandUNCFileName(DestFullName);
  Action.Destination(ExpandedDestFullName);

  FILE_OPERATION_LOOP_BEGIN
  {
    HANDLE LocalHandle;
    if (!FTerminal->CreateLocalFile(DestFullName, OperationProgress, &LocalHandle, FLAGSET(Params, cpNoConfirmation)))
    {
      throw ESkipFile();
    }

    std::unique_ptr<TStream> Stream(new TSafeHandleStream(reinterpret_cast<THandle>(LocalHandle)));

    bool DeleteLocalFile = true;

    try
    {
      TLibS3GetObjectDataCallbackData Data;

      FILE_OPERATION_LOOP_BEGIN
      {
        RequestInit(Data);
        Data.FileName = FileName;
        Data.Stream = Stream.get();
        Data.OperationProgress = OperationProgress;
        Data.Exception.reset(NULL);

        TAutoFlag ResponseIgnoreSwitch(FResponseIgnore);
        S3GetObjectHandler GetObjectHandler = { CreateResponseHandler(), LibS3GetObjectDataCallback };
        S3_get_object(
          &BucketContext, StrToS3(Key), NULL, Stream->Position, 0, FRequestContext, FTimeout, &GetObjectHandler, &Data);

        // The "exception" was already seen by the user, its presence mean an accepted abort of the operation.
        if (Data.Exception.get() == NULL)
        {
          CheckLibS3Error(Data, true);
        }
      }
      FILE_OPERATION_LOOP_END_EX(FMTLOAD(TRANSFER_ERROR, (FileName)), (folAllowSkip | folRetryOnFatal));

      if (Data.Exception.get() != NULL)
      {
        RethrowException(Data.Exception.get());
      }

      DeleteLocalFile = false;

      if (CopyParam->PreserveTime)
      {
        FTerminal->UpdateTargetTime(LocalHandle, File->Modification, FTerminal->SessionData->DSTMode);
      }
    }
    __finally
    {
      CloseHandle(LocalHandle);

      if (DeleteLocalFile)
      {
        FILE_OPERATION_LOOP_BEGIN
        {
          THROWOSIFFALSE(Sysutils::DeleteFile(ApiPath(DestFullName)));
        }
        FILE_OPERATION_LOOP_END(FMTLOAD(DELETE_LOCAL_FILE_ERROR, (DestFullName)));
      }
    }
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(TRANSFER_ERROR, (FileName)));

  FTerminal->UpdateTargetAttrs(DestFullName, File, CopyParam, Attrs);
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
