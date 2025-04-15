//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <io.h>
#include <fcntl.h>
#include <wincrypt.h>

#define NE_LFS
#include <ne_basic.h>
#include <ne_auth.h>
#include <ne_props.h>
#include <ne_uri.h>
#include <ne_session.h>
#include <ne_request.h>
#include <ne_xml.h>
#include <ne_redirect.h>
#include <ne_xmlreq.h>
#include <ne_locks.h>
#include <expat.h>

#include "WebDAVFileSystem.h"

#include "Interface.h"
#include "Common.h"
#include "Exceptions.h"
#include "Terminal.h"
#include "TextsCore.h"
#include "SecureShell.h"
#include "HelpCore.h"
#include "CoreMain.h"
#include "Security.h"
#include <StrUtils.hpp>
#include <NeonIntf.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_TERMINAL FTerminal
//---------------------------------------------------------------------------
#define SESSION_CONTEXT_KEY "sessioncontext"
static const UnicodeString CONST_WEBDAV_PROTOCOL_BASE_NAME = L"WebDAV";
static const int HttpUnauthorized = 401;
//---------------------------------------------------------------------------
#define DAV_PROP_NAMESPACE "DAV:"
#define MODDAV_PROP_NAMESPACE "http://apache.org/dav/props/"
#define PROP_CONTENT_LENGTH "getcontentlength"
#define PROP_LAST_MODIFIED "getlastmodified"
#define PROP_RESOURCE_TYPE "resourcetype"
#define PROP_HIDDEN "ishidden"
#define PROP_QUOTA_AVAILABLE "quota-available-bytes"
#define PROP_QUOTA_USED "quota-used-bytes"
#define PROP_EXECUTABLE "executable"
#define PROP_OWNER "owner"
#define PROP_DISPLAY_NAME "displayname"
//------------------------------------------------------------------------------
//---------------------------------------------------------------------------
// ne_path_escape returns 7-bit string, so it does not really matter if we use
// AnsiString or UTF8String here, though UTF8String might be more safe
static AnsiString PathEscape(const char * Path)
{
  char * EscapedPath = ne_path_escape(Path);
  AnsiString Result = EscapedPath;
  ne_free(EscapedPath);
  return Result;
}
//---------------------------------------------------------------------------
static UnicodeString PathUnescape(const char * Path)
{
  char * UnescapedPath = ne_path_unescape(Path);
  UTF8String UtfResult;
  if (UnescapedPath != NULL)
  {
    UtfResult = UnescapedPath;
    ne_free(UnescapedPath);
  }
  else
  {
    // OneDrive, particularly in the response to *file* PROPFIND tend to return a malformed URL.
    // In such case, take the path as is and we will probably overwrite the name with "display name".
    UtfResult = Path;
  }
  UnicodeString Result = UnicodeString(UtfResult);
  return Result;
}
//---------------------------------------------------------------------------
#define AbsolutePathToNeon(P) PathEscape(StrToNeon(P)).c_str()
#define PathToNeonStatic(THIS, P) AbsolutePathToNeon((THIS)->AbsolutePath(P, false))
#define PathToNeon(P) PathToNeonStatic(this, P)
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
static bool NeonInitialized = false;
static bool NeonSspiInitialized = false;
//---------------------------------------------------------------------------
void __fastcall NeonInitialize()
{
  // Even if this fails, we do not want to interrupt WinSCP starting for that.
  // Anyway, it can hardly fail.
  // Though it fails on Wine on Debian VM, because of ne_sspi_init():
  // sspi: QuerySecurityPackageInfo [failed] [80090305].
  // sspi: Unable to get negotiate maximum packet size
  int NeonResult = ne_sock_init();
  if (NeonResult == 0)
  {
    NeonInitialized = true;
    NeonSspiInitialized = true;
  }
  else if (NeonResult == -2)
  {
    NeonInitialized = true;
    NeonSspiInitialized = false;
  }
  else
  {
    NeonInitialized = false;
    NeonSspiInitialized = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall NeonFinalize()
{
  if (NeonInitialized)
  {
    ne_sock_exit();
    NeonInitialized = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall RequireNeon(TTerminal * Terminal)
{
  if (!NeonInitialized)
  {
    throw Exception(LoadStr(NEON_INIT_FAILED2));
  }

  if (!NeonSspiInitialized)
  {
    Terminal->LogEvent(L"Warning: SSPI initialization failed.");
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall NeonVersion()
{
  UnicodeString Str = StrFromNeon(ne_version_string());
  CutToChar(Str, L' ', true); // "neon"
  UnicodeString Result = CutToChar(Str, L':', true);
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ExpatVersion()
{
  return FORMAT(L"%d.%d.%d", (XML_MAJOR_VERSION, XML_MINOR_VERSION, XML_MICRO_VERSION));
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TWebDAVFileSystem::TWebDAVFileSystem(TTerminal * ATerminal) :
  TCustomFileSystem(ATerminal),
  FActive(false),
  FHasTrailingSlash(false),
  FUploading(false),
  FDownloading(false),
  FNeonLockStore(NULL),
  FNeonLockStoreSection(new TCriticalSection()),
  FInitialHandshake(false),
  FSessionContext(NULL),
  FIgnoreAuthenticationFailure(iafNo)
{
  FFileSystemInfo.ProtocolBaseName = CONST_WEBDAV_PROTOCOL_BASE_NAME;
  FFileSystemInfo.ProtocolName = FFileSystemInfo.ProtocolBaseName;
}
//---------------------------------------------------------------------------
__fastcall TWebDAVFileSystem::~TWebDAVFileSystem()
{
  UnregisterFromNeonDebug(FTerminal);

  {
    TGuard Guard(FNeonLockStoreSection);
    if (FNeonLockStore != NULL)
    {
      ne_lockstore_destroy(FNeonLockStore);
      FNeonLockStore = NULL;
    }
  }

  delete FNeonLockStoreSection;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::Open()
{

  RequireNeon(FTerminal);

  RegisterForNeonDebug(FTerminal);

  FCurrentDirectory = L"";
  FHasTrailingSlash = true;
  FStoredPasswordTried = false;
  FTlsVersionStr = L"";
  FCapabilities = 0;

  TSessionData * Data = FTerminal->SessionData;

  FSessionInfo.LoginTime = Now();
  FSessionInfo.CertificateVerifiedManually = false;

  UnicodeString HostName = Data->HostNameExpanded;

  FOneDrive = SameText(HostName, L"d.docs.live.net");
  if (FOneDrive)
  {
    FTerminal->LogEvent(L"OneDrive host detected.");
    FOneDriveInterface = odiUnknown;
  }

  size_t Port = Data->PortNumber;
  UnicodeString ProtocolName = (Data->Ftps == ftpsNone) ? HttpProtocol : HttpsProtocol;
  UnicodeString Path = Data->RemoteDirectory;
  // PathToNeon is not used as we cannot call AbsolutePath here
  UnicodeString EscapedPath = StrFromNeon(PathEscape(StrToNeon(Path)).c_str());
  UnicodeString Url = FORMAT(L"%s://%s:%d%s", (ProtocolName, HostName, Port, EscapedPath));

  FTerminal->Information(LoadStr(STATUS_CONNECT));
  FActive = false;
  try
  {
    OpenUrl(Url);
  }
  catch (Exception & E)
  {
    CloseNeonSession();
    FTerminal->Closed();
    FTerminal->FatalError(&E, LoadStr(CONNECTION_FAILED));
  }
  FActive = true;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWebDAVFileSystem::ParsePathFromUrl(const UnicodeString & Url)
{
  UnicodeString Result;
  ne_uri ParsedUri;
  if (ne_uri_parse(StrToNeon(Url), &ParsedUri) == 0)
  {
    Result = PathUnescape(ParsedUri.path);
    ne_uri_free(&ParsedUri);
  }
  return Result;
}
//---------------------------------------------------------------------------
void TWebDAVFileSystem::OpenUrl(const UnicodeString & Url)
{
  UnicodeString CorrectedUrl;
  NeonClientOpenSessionInternal(CorrectedUrl, Url);

  if (CorrectedUrl.IsEmpty())
  {
    CorrectedUrl = Url;
  }
  UnicodeString ParsedPath = ParsePathFromUrl(CorrectedUrl);
  if (!ParsedPath.IsEmpty())
  {
    // this is most likely pointless as it get overwritten by
    // call to ChangeDirectory() from TTerminal::DoStartup
    FCurrentDirectory = ParsedPath;
  }
}
//---------------------------------------------------------------------------
void TWebDAVFileSystem::NeonClientOpenSessionInternal(UnicodeString & CorrectedUrl, UnicodeString Url)
{
  std::unique_ptr<TStringList> AttemptedUrls(CreateSortedStringList());
  AttemptedUrls->Add(Url);
  while (true)
  {

    FSessionInfo.CSCipher = EmptyStr;
    FSessionInfo.SCCipher = EmptyStr;

    UTF8String Path, DiscardQuery;
    DebugAssert(FSessionContext == NULL);
    FSessionContext = NeonOpen(Url, Path, DiscardQuery);

    bool Ssl = IsTlsSession(FSessionContext->NeonSession);
    FSessionInfo.SecurityProtocolName = Ssl ? LoadStr(FTPS_IMPLICIT) : EmptyStr;

    if (Ssl != (FTerminal->SessionData->Ftps != ftpsNone))
    {
      FTerminal->LogEvent(FORMAT(L"Warning: %s", (LoadStr(UNENCRYPTED_REDIRECT))));
    }

    {
      CorrectedUrl = EmptyStr;
      TAutoFlag Flag(FInitialHandshake);
      ExchangeCapabilities(Path.c_str(), CorrectedUrl);
    }
    // No error and no corrected URL?  We're done here.
    if (CorrectedUrl.IsEmpty())
    {
      break;
    }
    CloseNeonSession();
    CheckRedirectLoop(CorrectedUrl, AttemptedUrls.get());
    // Our caller will want to know what our final corrected URL was.
    Url = CorrectedUrl;
  }

  CorrectedUrl = Url;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::SetSessionTls(TSessionContext * SessionContext, ne_session_s * Session, bool Aux)
{
  ne_ssl_verify_fn Callback = Aux ? NeonServerSSLCallbackAux : NeonServerSSLCallbackMain;
  InitNeonTls(Session, InitSslSession, Callback, SessionContext, FTerminal);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::InitSession(TSessionContext * SessionContext, ne_session_s * Session)
{
  TSessionData * Data = FTerminal->SessionData;

  InitNeonSession(
    Session, Data->ProxyMethod, Data->ProxyHost, Data->ProxyPort,
    Data->ProxyUsername, Data->ProxyPassword, FTerminal);

  ne_set_read_timeout(Session, Data->Timeout);

  // The ne_set_connect_timeout was called here previously, but as neon does not support non-blocking
  // connection on Windows, it was noop.
  // Restore the call once ours non-blocking connection implementation proves working.

  ne_set_session_private(Session, SESSION_CONTEXT_KEY, SessionContext);

  // Allow ^-escaping in OneDrive
  ne_set_session_flag(Session, NE_SESSFLAG_LIBERAL_ESCAPING, Data->WebDavLiberalEscaping || FOneDrive);
}
//---------------------------------------------------------------------------
TWebDAVFileSystem::TSessionContext * TWebDAVFileSystem::NeonOpen(const UnicodeString & Url, UTF8String & Path, UTF8String & Query)
{
  ne_uri uri;
  NeonParseUrl(Url, uri);

  std::unique_ptr<TSessionContext> Result(new TSessionContext());
  Result->FileSystem = this;
  Result->HostName = StrFromNeon(uri.host);
  Result->PortNumber = uri.port;
  Result->NtlmAuthenticationFailed = false;

  Result->NeonSession = CreateNeonSession(uri);
  InitSession(Result.get(), Result->NeonSession);

  Path = uri.path;
  Query = uri.query;
  bool Ssl = IsTlsUri(uri);
  ne_uri_free(&uri);
  ne_set_aux_request_init(Result->NeonSession, NeonAuxRequestInit, Result.get());

  UpdateNeonDebugMask();

  NeonAddAuthentication(Result.get(), Ssl);

  if (Ssl)
  {
    SetSessionTls(Result.get(), Result->NeonSession, false);

    ne_ssl_provide_clicert(Result->NeonSession, NeonProvideClientCert, Result.get());
  }

  ne_set_notifier(Result->NeonSession, NeonNotifier, Result.get());
  ne_hook_create_request(Result->NeonSession, NeonCreateRequest, Result.get());
  ne_hook_pre_send(Result->NeonSession, NeonPreSend, Result.get());
  ne_hook_post_send(Result->NeonSession, NeonPostSend, Result.get());
  ne_hook_post_headers(Result->NeonSession, NeonPostHeaders, Result.get());
  return Result.release();
}
//---------------------------------------------------------------------------
bool TWebDAVFileSystem::IsTlsSession(ne_session * Session)
{
  ne_uri uri = ne_uri();
  ne_fill_server_uri(Session, &uri);
  bool Result = IsTlsUri(uri);
  ne_uri_free(&uri);

  return Result;
}
//---------------------------------------------------------------------------
void TWebDAVFileSystem::NeonAuxRequestInit(ne_session * Session, ne_request * /*Request*/, void * UserData)
{
  TSessionContext * SessionContext = static_cast<TSessionContext *>(UserData);
  TWebDAVFileSystem * FileSystem = SessionContext->FileSystem;
  FileSystem->InitSession(SessionContext, Session);

  if (FileSystem->IsTlsSession(Session))
  {
    FileSystem->SetSessionTls(SessionContext, Session, true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::NeonAddAuthentication(TSessionContext * SessionContext, bool UseNegotiate)
{
  unsigned int NeonAuthTypes = NE_AUTH_BASIC | NE_AUTH_DIGEST | NE_AUTH_PASSPORT;
  if (UseNegotiate)
  {
    NeonAuthTypes |= NE_AUTH_NEGOTIATE;
  }
  if (FTerminal->SessionData->WebDavAuthLegacy)
  {
    NeonAuthTypes |= NE_AUTH_LEGACY_DIGEST;
  }
  ne_add_server_auth(SessionContext->NeonSession, NeonAuthTypes, NeonRequestAuth, SessionContext);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWebDAVFileSystem::GetRedirectUrl()
{
  UnicodeString Result = GetNeonRedirectUrl(FSessionContext->NeonSession);
  FTerminal->LogEvent(FORMAT(L"Redirected to \"%s\".", (Result)));
  return Result;
}
//---------------------------------------------------------------------------
void TWebDAVFileSystem::ExchangeCapabilities(const char * Path, UnicodeString & CorrectedUrl)
{
  ClearNeonError();

  int NeonStatus;
  do
  {
    FAuthenticationRetry = false;
    NeonStatus = ne_options2(FSessionContext->NeonSession, Path, &FCapabilities);
  }
  while ((NeonStatus == NE_AUTH) && FAuthenticationRetry);

  if (NeonStatus == NE_REDIRECT)
  {
    CorrectedUrl = GetRedirectUrl();
  }
  else if (NeonStatus == NE_OK)
  {
    if (FCapabilities > 0)
    {
      UnicodeString Str;
      unsigned int Capability = 0x01;
      unsigned int Capabilities = FCapabilities;
      while (Capabilities > 0)
      {
        if (FLAGSET(Capabilities, Capability))
        {
          AddToList(Str, StrFromNeon(ne_capability_name(Capability)), L", ");
          Capabilities -= Capability;
        }
        Capability <<= 1;
      }
      FTerminal->LogEvent(FORMAT(L"Server capabilities: %s", (Str)));
      FFileSystemInfo.AdditionalInfo +=
        LoadStr(WEBDAV_EXTENSION_INFO) + sLineBreak +
        L"  " + Str + sLineBreak;
    }
  }
  else
  {
    CheckStatus(NeonStatus);
  }

  FTerminal->SaveCapabilities(FFileSystemInfo);
}
//---------------------------------------------------------------------------
TWebDAVFileSystem::TSessionContext::TSessionContext() :
  NeonSession(NULL)
{
}
//---------------------------------------------------------------------------
TWebDAVFileSystem::TSessionContext::~TSessionContext()
{
  if (NeonSession != NULL)
  {
    DestroyNeonSession(NeonSession);
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CloseNeonSession()
{
  if (FSessionContext != NULL)
  {
    delete FSessionContext;
    FSessionContext = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::Close()
{
  DebugAssert(FActive);
  CloseNeonSession();
  FTerminal->Closed();
  FActive = false;
  UnregisterFromNeonDebug(FTerminal);
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::GetActive()
{
  return FActive;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CollectUsage()
{
  if (!FTlsVersionStr.IsEmpty())
  {
    FTerminal->CollectTlsUsage(FTlsVersionStr);
  }

  if (!FTerminal->SessionData->TlsCertificateFile.IsEmpty())
  {
    Configuration->Usage->Inc(L"OpenedSessionsWebDAVSCertificate");
  }

  // The Authorization header for passport method is included only in the first request,
  // so we have to use FLastAuthorizationProtocol
  if (SameText(FLastAuthorizationProtocol, L"Passport1.4"))
  {
    Configuration->Usage->Inc(L"OpenedSessionsWebDAVSPassport");
  }
  else if (SameText(FLastAuthorizationProtocol, L"Basic"))
  {
    Configuration->Usage->Inc(L"OpenedSessionsWebDAVAuthBasic");
  }

  UnicodeString RemoteSystem = FFileSystemInfo.RemoteSystem;
  if (FOneDrive)
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsWebDAVOneDrive");
    if (FOneDriveInterface == odiUpperCase)
    {
      FTerminal->Configuration->Usage->Inc(L"OpenedSessionsWebDAVOneDriveUpperCase");
    }
    else if (FOneDriveInterface == odiLowerCase)
    {
      FTerminal->Configuration->Usage->Inc(L"OpenedSessionsWebDAVOneDriveLowerCase");
    }
  }
  if (ContainsText(RemoteSystem, L"Microsoft-IIS"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsWebDAVIIS");
  }
  else if (ContainsText(RemoteSystem, L"IT Hit WebDAV Server"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsWebDAVITHit");
  }
  // e.g. brickftp.com
  else if (ContainsText(RemoteSystem, L"nginx"))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsWebDAVNginx");
  }
  else
  {
    // We also know OpenDrive, Yandex, iFiles (iOS), Swapper (iOS), SafeSync
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsWebDAVOther");
  }
}
//---------------------------------------------------------------------------
const TSessionInfo & __fastcall TWebDAVFileSystem::GetSessionInfo()
{
  return FSessionInfo;
}
//---------------------------------------------------------------------------
const TFileSystemInfo & __fastcall TWebDAVFileSystem::GetFileSystemInfo(bool /*Retrieve*/)
{
  return FFileSystemInfo;
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::TemporaryTransferFile(const UnicodeString & /*FileName*/)
{
  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::GetStoredCredentialsTried()
{
  return FStoredPasswordTried;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWebDAVFileSystem::GetUserName()
{
  return FUserName;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::Idle()
{
  // noop
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWebDAVFileSystem::AbsolutePath(const UnicodeString Path, bool /*Local*/)
{
  bool AddTrailingBackslash;

  if (Path == L"/")
  {
    // does not really matter as path "/" is still "/" when absolute,
    // no slash needed
    AddTrailingBackslash = FHasTrailingSlash;
  }
  else
  {
    AddTrailingBackslash = (Path[Path.Length()] == L'/');
  }

  UnicodeString Result = ::AbsolutePath(GetCurrentDirectory(), Path);
  // We must preserve trailing slash, because particularly for mod_dav,
  // it really matters if the slash in there or not
  if (AddTrailingBackslash)
  {
    Result = UnixIncludeTrailingBackslash(Result);
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::IsCapable(int Capability) const
{
  DebugAssert(FTerminal);
  switch (Capability)
  {
    case fcRename:
    case fcRemoteMove:
    case fcMoveToQueue:
    case fcPreservingTimestampUpload:
    case fcCheckingSpaceAvailable:
    // Only to make double-click on file edit/open the file,
    // instead of trying to open it as directory
    case fcResolveSymlink:
    case fcSkipTransfer:
    case fcParallelTransfers:
    case fcRemoteCopy:
    case fcMoveOverExistingFile:
      return true;

    case fcUserGroupListing:
    case fcModeChanging:
    case fcModeChangingUpload:
    case fcAclChangingFiles:
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
    case fcTransferOut:
    case fcTransferIn:
    case fcParallelFileTransfers:
    case fcTags:
      return false;

    case fcLocking:
      return FLAGSET(FCapabilities, NE_CAP_DAV_CLASS2);

    default:
      DebugFail();
      return false;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWebDAVFileSystem::GetCurrentDirectory()
{
  return FCurrentDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::DoStartup()
{
  FTerminal->SetExceptionOnFail(true);
  // retrieve initialize working directory to save it as home directory
  ReadCurrentDirectory();
  FTerminal->SetExceptionOnFail(false);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ClearNeonError()
{
  FCancelled = false;
  FSkipped = false;
  FAuthenticationRequested = false;
  ne_set_error(FSessionContext->NeonSession, "");
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWebDAVFileSystem::GetNeonError()
{
  return ::GetNeonError(FSessionContext->NeonSession);
}
//---------------------------------------------------------------------------
void TWebDAVFileSystem::CheckStatus(int NeonStatus)
{
  CheckStatus(FSessionContext, NeonStatus);
}
//---------------------------------------------------------------------------
void TWebDAVFileSystem::CheckStatus(TSessionContext * SessionContext, int NeonStatus)
{
  if ((NeonStatus == NE_ERROR) && (FCancelled || FSkipped))
  {
    if (FCancelled)
    {
      FCancelled = false;
      FSkipped = false; // just in case
      Abort();
    }
    else
    {
      DebugAssert(FSkipped);
      FSkipped = false;
      throw ESkipFile();
    }
  }
  else
  {
    CheckNeonStatus(SessionContext->NeonSession, NeonStatus, SessionContext->HostName);
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::LookupUsersGroups()
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ReadCurrentDirectory()
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
void __fastcall TWebDAVFileSystem::HomeDirectory()
{
  ChangeDirectory(L"/");
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWebDAVFileSystem::DirectoryPath(UnicodeString Path)
{
  if (FHasTrailingSlash)
  {
    Path = ::UnixIncludeTrailingBackslash(Path);
  }
  return Path;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWebDAVFileSystem::FilePath(const TRemoteFile * File)
{
  UnicodeString Result = File->FullFileName;
  if (File->IsDirectory)
  {
    Result = DirectoryPath(Result);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::TryOpenDirectory(UnicodeString Directory)
{
  Directory = DirectoryPath(Directory);
  FTerminal->LogEvent(FORMAT(L"Trying to open directory \"%s\".", (Directory)));
  TRemoteFile * File;
  ReadFile(Directory, File);
  delete File;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::AnnounceFileListOperation()
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ChangeDirectory(const UnicodeString ADirectory)
{
  UnicodeString Path = AbsolutePath(ADirectory, false);

  // to verify existence of directory try to open it
  TryOpenDirectory(Path);

  // if open dir did not fail, directory exists -> success.
  FCachedDirectoryChange = Path;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CachedChangeDirectory(const UnicodeString Directory)
{
  FCachedDirectoryChange = UnixExcludeTrailingBackslash(Directory);
}
//---------------------------------------------------------------------------
struct TReadFileData
{
  TWebDAVFileSystem * FileSystem;
  TRemoteFile * File;
  TRemoteFileList * FileList;
};
//---------------------------------------------------------------------------
int __fastcall TWebDAVFileSystem::ReadDirectoryInternal(
  const UnicodeString & Path, TRemoteFileList * FileList)
{
  TReadFileData Data;
  Data.FileSystem = this;
  Data.File = NULL;
  Data.FileList = FileList;
  ClearNeonError();
  ne_propfind_handler * PropFindHandler = ne_propfind_create(FSessionContext->NeonSession, PathToNeon(Path), NE_DEPTH_ONE);
  void * DiscoveryContext = ne_lock_register_discovery(PropFindHandler);
  int Result;
  try
  {
    Result = ne_propfind_allprop(PropFindHandler, NeonPropsResult, &Data);
  }
  __finally
  {
    ne_lock_discovery_free(DiscoveryContext);
    ne_propfind_destroy(PropFindHandler);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool TWebDAVFileSystem::IsRedirect(int NeonStatus)
{
  return (NeonStatus == NE_REDIRECT);
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::IsValidRedirect(int NeonStatus, UnicodeString & Path)
{
  bool Result = IsRedirect(NeonStatus);
  if (Result)
  {
    // What PathToNeon does
    UnicodeString OriginalPath = AbsolutePath(Path, false);
    // Handle one-step redirect
    // (for more steps we would have to implement loop detection).
    // This is mainly to handle "folder" => "folder/" redirects of Apache/mod_dav.
    UnicodeString RedirectUrl = GetRedirectUrl();
    // We should test if the redirect is not for another server,
    // though not sure how to do this reliably (domain aliases, IP vs. domain, etc.)
    UnicodeString RedirectPath = ParsePathFromUrl(RedirectUrl);
    Result =
      !RedirectPath.IsEmpty() &&
      (RedirectPath != OriginalPath);

    if (Result)
    {
      Path = RedirectPath;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ReadDirectory(TRemoteFileList * FileList)
{
  UnicodeString Path = DirectoryPath(FileList->Directory);
  TOperationVisualizer Visualizer(FTerminal->UseBusyCursor);

  int NeonStatus = ReadDirectoryInternal(Path, FileList);
  if (IsValidRedirect(NeonStatus, Path))
  {
    NeonStatus = ReadDirectoryInternal(Path, FileList);
  }
  CheckStatus(NeonStatus);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ReadSymlink(TRemoteFile * /*SymlinkFile*/,
  TRemoteFile *& /*File*/)
{
  // we never set SymLink flag, so we should never get here
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ReadFile(const UnicodeString FileName,
  TRemoteFile *& File)
{
  CustomReadFile(FileName, File, NULL);
}
//---------------------------------------------------------------------------
void TWebDAVFileSystem::NeonPropsResult(
  void * UserData, const ne_uri * Uri, const ne_prop_result_set * Results)
{
  UnicodeString Path = PathUnescape(Uri->path);

  TReadFileData & Data = *static_cast<TReadFileData *>(UserData);
  TWebDAVFileSystem * FileSystem = Data.FileSystem;
  if (Data.FileList != NULL)
  {
    std::unique_ptr<TRemoteFile> File(new TRemoteFile(NULL));
    TTerminal * Terminal = FileSystem->FTerminal;
    File->Terminal = Terminal;
    FileSystem->ParsePropResultSet(File.get(), Path, Results);

    UnicodeString FileListPath = UnixIncludeTrailingBackslash(FileSystem->AbsolutePath(Data.FileList->Directory, false));
    if (FileSystem->FOneDrive)
    {
      UnicodeString FullFileName = UnixIncludeTrailingBackslash(File->FullFileName);
      if (Configuration->Usage->Collect && (FileSystem->FOneDriveInterface == odiUnknown) && !IsUnixRootPath(FullFileName))
      {
        UnicodeString Cid = FullFileName;
        if (DebugAlwaysTrue(StartsStr(L"/", Cid)))
        {
          Cid.Delete(1, 1);
          int P = Cid.Pos(L"/");
          if (P > 0)
          {
            Cid.SetLength(P - 1);
          }
          UnicodeString CidUpper = UpperCase(Cid);
          UnicodeString CidLower = LowerCase(Cid);
          if (CidUpper != CidLower)
          {
            if (Cid == CidUpper)
            {
              FileSystem->FOneDriveInterface = odiUpperCase;
              Terminal->LogEvent(L"Detected upper-case OneDrive interface");
            }
            else if (Cid == CidLower)
            {
              FileSystem->FOneDriveInterface = odiLowerCase;
              Terminal->LogEvent(L"Detected lower-case OneDrive interface");
            }
          }
        }
      }
      // OneDrive is case insensitive and when we enter the directory using a different case, it returns results with the actual case
      if (StartsText(FileListPath, FullFileName))
      {
        File->FullFileName = FileListPath + MidStr(FullFileName, FileListPath.Length() + 1);
      }
    }
    if (UnixSamePath(File->FullFileName, FileListPath))
    {
      File->FileName = PARENTDIRECTORY;
      File->FullFileName = UnixCombinePaths(Path, PARENTDIRECTORY);
    }
    else
    {
      // Quick and dirty hack. The following tests do not work on OneDrive, as the directory path may be escaped,
      // and we do not correctly unescape full `Path`, only its filename (so path in FullFileName may not be not correct).
      // Until it's real problem somewhere else, we skip this test, as we otherwise trust OneDrive not to return nonsense contents.
      if (!FileSystem->FOneDrive)
      {
        if (!StartsStr(FileListPath, File->FullFileName))
        {
          Terminal->LogEvent(FORMAT(L"Discarding entry \"%s\" with absolute path \"%s\" because it is not descendant of directory \"%s\".", (Path, File->FullFileName, FileListPath)));
          File.reset(NULL);
        }
        else
        {
          UnicodeString FileName = MidStr(File->FullFileName, FileListPath.Length() + 1);
          if (!UnixExtractFileDir(FileName).IsEmpty())
          {
            Terminal->LogEvent(FORMAT(L"Discarding entry \"%s\" with absolute path \"%s\" because it is not direct child of directory \"%s\".", (Path, File->FullFileName, FileListPath)));
            File.reset(NULL);
          }
        }
      }
    }

    if (File.get() != NULL)
    {
      Data.FileList->AddFile(File.release());
    }
  }
  else
  {
    FileSystem->ParsePropResultSet(Data.File, Path, Results);
  }
}
//---------------------------------------------------------------------------
const char * __fastcall TWebDAVFileSystem::GetProp(
  const ne_prop_result_set * Results, const char * Name, const char * NameSpace)
{
  ne_propname Prop;
  Prop.nspace = (NameSpace == NULL) ? DAV_PROP_NAMESPACE : NameSpace;
  Prop.name = Name;
  return ne_propset_value(Results, &Prop);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ParsePropResultSet(TRemoteFile * File,
  const UnicodeString & Path, const ne_prop_result_set * Results)
{
  File->FullFileName = UnixExcludeTrailingBackslash(Path);
  // Some servers do not use DAV:collection tag, but indicate the folder by trailing slash only.
  // But not all, for example OneDrive does not (it did in the past).
  bool Collection = (File->FullFileName != Path);
  File->FileName = UnixExtractFileName(File->FullFileName);
  const char * ContentLength = GetProp(Results, PROP_CONTENT_LENGTH);
  // some servers, for example iFiles, do not provide "getcontentlength" for folders
  if (ContentLength != NULL)
  {
    File->Size = StrToInt64Def(ContentLength, 0);
  }
  const char * LastModified = GetProp(Results, PROP_LAST_MODIFIED);
  // We've seen a server (t=24891) that does not set "getlastmodified" for the "this" folder entry.
  File->ModificationFmt = mfNone; // fallback
  if (LastModified != NULL)
  {
    char WeekDay[4] = { L'\0' };
    int Year = 0;
    char MonthStr[4] = { L'\0' };
    int Day = 0;
    int Hour = 0;
    int Min = 0;
    int Sec = 0;
    #define RFC1123_FORMAT "%3s, %02d %3s %4d %02d:%02d:%02d GMT"
    // Keep is sync with S3
    int Filled =
      sscanf(LastModified, RFC1123_FORMAT, WeekDay, &Day, MonthStr, &Year, &Hour, &Min, &Sec);
    // we need at least a complete date
    if (Filled >= 4)
    {
      int Month = ParseShortEngMonthName(MonthStr);
      if (Month >= 1)
      {
        TDateTime Modification =
          EncodeDateVerbose((unsigned short)Year, (unsigned short)Month, (unsigned short)Day) +
          EncodeTimeVerbose((unsigned short)Hour, (unsigned short)Min, (unsigned short)Sec, 0);
        File->Modification = ConvertTimestampFromUTC(Modification);
        // Should use mfYMDHM or mfMDY when appropriate according to Filled
        File->ModificationFmt = mfFull;
      }
    }
  }

  // optimization
  if (!Collection)
  {
    // See a comment at the Collection declaration.
    const char * ResourceType = GetProp(Results, PROP_RESOURCE_TYPE);
    if (ResourceType != NULL)
    {
      // property has XML value
      UnicodeString AResourceType = ResourceType;
      // this is very poor parsing
      if (ContainsText(ResourceType, L"<DAV:collection"))
      {
        Collection = true;
      }
    }
  }

  File->Type = Collection ? FILETYPE_DIRECTORY : FILETYPE_DEFAULT;
  // this is MS extension (draft-hopmann-collection-props-00)
  const char * IsHidden = GetProp(Results, PROP_HIDDEN);
  if (IsHidden != NULL)
  {
    File->IsHidden = (StrToIntDef(IsHidden, 0) != 0);
  }

  const char * Owner = GetProp(Results, PROP_OWNER);
  if (Owner != NULL)
  {
    File->Owner.Name = Owner;
  }

  const char * DisplayName = GetProp(Results, PROP_DISPLAY_NAME);
  if (DisplayName != NULL)
  {
    File->DisplayName = StrFromNeon(DisplayName);
    // OneDrive caret escaping (we could do this for all files, but let's limit the scope for now).
    // In *file* PROPFIND response, the # (and other symbols like comma or plus) is not escaped at all
    // (while in directory listing, they are caret-escaped),
    // so if we see one in the display name, take the name from there.
    // * and % won't help, as OneDrive seem to have bug with % at the end of the filename,
    // and the * (and others) is removed from file names.

    // Filenames with commas (,) get as many additional characters at the end of the filename as there are commas
    // (not true anymore in the new interface).
    if (FOneDrive &&
        (ContainsText(File->FileName, L"^") || ContainsText(File->FileName, L",") || (wcspbrk(File->DisplayName.c_str(), L"&,+#[]%*") != NULL)))
    {
      File->FileName = File->DisplayName;
      File->FullFileName = UnixCombinePaths(UnixExtractFileDir(File->FullFileName), File->FileName);
    }
  }

  const UnicodeString RightsDelimiter(L", ");
  UnicodeString HumanRights;

  // Proprietary property of mod_dav
  // http://www.webdav.org/mod_dav/#imp
  const char * Executable = GetProp(Results, PROP_EXECUTABLE, MODDAV_PROP_NAMESPACE);
  if (Executable != NULL)
  {
    if (strcmp(Executable, "T") == NULL)
    {
      UnicodeString ExecutableRights;
      // The "gear" character is supported since Windows 8
      if (IsWin8())
      {
        ExecutableRights = L"\u2699";
      }
      else
      {
        ExecutableRights = LoadStr(EXECUTABLE);
      }
      AddToList(HumanRights, ExecutableRights, RightsDelimiter);
    }
  }

  struct ne_lock * Lock = static_cast<struct ne_lock *>(ne_propset_private(Results));
  if ((Lock != NULL) && (Lock->token != NULL))
  {
    UnicodeString Owner;
    if (Lock->owner != NULL)
    {
      Owner = StrFromNeon(Lock->owner).Trim();
    }
    UnicodeString LockRights;
    if (IsWin8())
    {
      // The "lock" character is supported since Windows 8
      LockRights = U"\U0001F512" + Owner;
    }
    else
    {
      LockRights = LoadStr(LOCKED);
      if (!Owner.IsEmpty())
      {
        LockRights = FORMAT(L"%s (%s)", (LockRights, Owner));
      }
    }

    AddToList(HumanRights, LockRights, RightsDelimiter);
  }

  File->HumanRights = HumanRights;
}
//---------------------------------------------------------------------------
int __fastcall TWebDAVFileSystem::CustomReadFileInternal(const UnicodeString FileName,
  TRemoteFile *& File, TRemoteFile * ALinkedByFile)
{
  std::unique_ptr<TRemoteFile> AFile(new TRemoteFile(ALinkedByFile));
  TReadFileData Data;
  Data.FileSystem = this;
  Data.File = AFile.get();
  Data.FileList = NULL;
  ClearNeonError();
  int Result =
    ne_simple_propfind(FSessionContext->NeonSession, PathToNeon(FileName), NE_DEPTH_ZERO, NULL,
      NeonPropsResult, &Data);
  if (Result == NE_OK)
  {
    File = AFile.release();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CustomReadFile(UnicodeString FileName,
  TRemoteFile *& File, TRemoteFile * ALinkedByFile)
{
  TOperationVisualizer Visualizer(FTerminal->UseBusyCursor);

  int NeonStatus = CustomReadFileInternal(FileName, File, ALinkedByFile);
  if (IsValidRedirect(NeonStatus, FileName))
  {
    NeonStatus = CustomReadFileInternal(FileName, File, ALinkedByFile);
  }
  CheckStatus(NeonStatus);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::DeleteFile(const UnicodeString FileName,
  const TRemoteFile * File, int /*Params*/, TRmSessionAction & Action)
{
  DebugUsedParam(FileName);
  Action.Recursive();
  ClearNeonError();
  TOperationVisualizer Visualizer(FTerminal->UseBusyCursor);
  RawByteString Path = PathToNeon(FilePath(File));
  // WebDAV does not allow non-recursive delete:
  // RFC 4918, section 9.6.1:
  // "A client MUST NOT submit a Depth header with a DELETE on a collection with any value but infinity."
  // We should check that folder is empty when called with FLAGSET(Params, dfNoRecursive)
  CheckStatus(ne_delete(FSessionContext->NeonSession, Path.c_str()));
  // The lock is removed with the file, but if a file with the same name gets created,
  // we would try to use obsoleted lock token with it, what the server would reject
  // (mod_dav returns "412 Precondition Failed")
  DiscardLock(Path);
}
//---------------------------------------------------------------------------
int __fastcall TWebDAVFileSystem::RenameFileInternal(
  const UnicodeString & FileName, const UnicodeString & NewName, bool Overwrite)
{
  return ne_move(FSessionContext->NeonSession, Overwrite, PathToNeon(FileName), PathToNeon(NewName));
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::RenameFile(
  const UnicodeString & FileName, const TRemoteFile *, const UnicodeString & NewName, bool Overwrite)
{
  ClearNeonError();
  TOperationVisualizer Visualizer(FTerminal->UseBusyCursor);

  UnicodeString Path = FileName;
  int NeonStatus = RenameFileInternal(Path, NewName, Overwrite);
  if (IsValidRedirect(NeonStatus, Path))
  {
    NeonStatus = RenameFileInternal(Path, NewName, Overwrite);
  }
  CheckStatus(NeonStatus);
  // See a comment in DeleteFile
  DiscardLock(PathToNeon(Path));
}
//---------------------------------------------------------------------------
int __fastcall TWebDAVFileSystem::CopyFileInternal(
  const UnicodeString & FileName, const UnicodeString & NewName, bool Overwrite)
{
  return ne_copy(FSessionContext->NeonSession, Overwrite, NE_DEPTH_INFINITE, PathToNeon(FileName), PathToNeon(NewName));
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CopyFile(
  const UnicodeString & FileName, const TRemoteFile *, const UnicodeString & NewName, bool Overwrite)
{
  ClearNeonError();
  TOperationVisualizer Visualizer(FTerminal->UseBusyCursor);

  UnicodeString Path = FileName;
  int NeonStatus = CopyFileInternal(Path, NewName, Overwrite);
  if (IsValidRedirect(NeonStatus, Path))
  {
    NeonStatus = CopyFileInternal(Path, NewName, Overwrite);
  }
  CheckStatus(NeonStatus);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CreateDirectory(const UnicodeString & DirName, bool /*Encrypt*/)
{
  ClearNeonError();
  TOperationVisualizer Visualizer(FTerminal->UseBusyCursor);
  CheckStatus(ne_mkcol(FSessionContext->NeonSession, PathToNeon(DirName)));
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CreateLink(const UnicodeString FileName,
  const UnicodeString PointTo, bool /*Symbolic*/)
{
  DebugFail();
  DebugUsedParam2(FileName, PointTo);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ChangeFileProperties(const UnicodeString FileName,
  const TRemoteFile * /*File*/, const TRemoteProperties * /*Properties*/,
  TChmodSessionAction & /*Action*/)
{
  DebugFail();
  DebugUsedParam(FileName);
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::LoadFilesProperties(TStrings * /*FileList*/)
{
  DebugFail();
  return false;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CalculateFilesChecksum(
  const UnicodeString & DebugUsedArg(Alg), TStrings * DebugUsedArg(FileList), TCalculatedChecksumEvent,
  TFileOperationProgressType *, bool DebugUsedArg(FirstLevel))
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ConfirmOverwrite(
  const UnicodeString & SourceFullFileName, UnicodeString & TargetFileName,
  TFileOperationProgressType * OperationProgress,
  const TOverwriteFileParams * FileParams, const TCopyParamType * CopyParam,
  int Params)
{
  // all = "yes to newer"
  int Answers = qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll | qaAll;
  TQueryButtonAlias Aliases[3];
  Aliases[0] = TQueryButtonAlias::CreateAllAsYesToNewerGrouppedWithYes();
  Aliases[1] = TQueryButtonAlias::CreateYesToAllGrouppedWithYes();
  Aliases[2] = TQueryButtonAlias::CreateNoToAllGrouppedWithNo();
  TQueryParams QueryParams(qpNeverAskAgainCheck);
  QueryParams.Aliases = Aliases;
  QueryParams.AliasesCount = LENOF(Aliases);

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
    // Can happen when moving to background (and the server manages to commit the interrupted foreground transfer).
    // WebDAV does not support resumable uploads.
    // Resumable downloads are not implemented.
    case qaRetry:
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
void __fastcall TWebDAVFileSystem::CustomCommandOnFile(const UnicodeString FileName,
  const TRemoteFile * /*File*/, UnicodeString Command, int /*Params*/, TCaptureOutputEvent /*OutputEvent*/)
{
  DebugFail();
  DebugUsedParam2(FileName, Command);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::AnyCommand(const UnicodeString Command,
  TCaptureOutputEvent /*OutputEvent*/)
{
  DebugFail();
  DebugUsedParam(Command);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TWebDAVFileSystem::GetFixedPaths()
{
  return NULL;
}
//---------------------------------------------------------------------------
void TWebDAVFileSystem::NeonQuotaResult(
  void * UserData, const ne_uri * /*Uri*/, const ne_prop_result_set * Results)
{
  TSpaceAvailable & SpaceAvailable = *static_cast<TSpaceAvailable *>(UserData);

  const char * Value = GetProp(Results, PROP_QUOTA_AVAILABLE);
  if (Value != NULL)
  {
    SpaceAvailable.UnusedBytesAvailableToUser = StrToInt64(StrFromNeon(Value));

    const char * Value = GetProp(Results, PROP_QUOTA_USED);
    if (Value != NULL)
    {
      SpaceAvailable.BytesAvailableToUser =
        StrToInt64(StrFromNeon(Value)) + SpaceAvailable.UnusedBytesAvailableToUser;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::SpaceAvailable(const UnicodeString Path,
  TSpaceAvailable & ASpaceAvailable)
{
  // RFC4331: https://datatracker.ietf.org/doc/html/rfc4331

  // This is known to be supported by:

  // OpenDrive: for a root drive only (and contrary to the spec, it sends the properties
  // unconditionally, even when not explicitly requested)
  // Server: Apache/2.2.17 (Fedora)
  // X-Powered-By: PHP/5.5.7
  // X-DAV-Powered-By: OpenDrive
  // WWW-Authenticate: Basic realm="PHP WebDAV"

  // IT Hit WebDAV Server:
  // Server: Microsoft-HTTPAPI/1.0
  // X-Engine: IT Hit WebDAV Server .Net v3.8.1877.0 (Evaluation License)

  // Yandex disk:
  // WWW-Authenticate: Basic realm="Yandex.Disk"
  // Server: MochiWeb/1.0

  // OneDrive:
  // it sends the properties unconditionally, even when not explicitly requested

  UnicodeString APath = DirectoryPath(Path);

  ne_propname QuotaProps[3];
  memset(QuotaProps, 0, sizeof(QuotaProps));
  QuotaProps[0].nspace = DAV_PROP_NAMESPACE;
  QuotaProps[0].name = PROP_QUOTA_AVAILABLE;
  QuotaProps[1].nspace = DAV_PROP_NAMESPACE;
  QuotaProps[1].name = PROP_QUOTA_USED;
  QuotaProps[2].nspace = NULL;
  QuotaProps[2].name = NULL;

  TOperationVisualizer Visualizer(FTerminal->UseBusyCursor);

  CheckStatus(
    ne_simple_propfind(FSessionContext->NeonSession, PathToNeon(APath), NE_DEPTH_ZERO, QuotaProps,
      NeonQuotaResult, &ASpaceAvailable));
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CopyToRemote(TStrings * FilesToCopy,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  Params &= ~cpAppend;

  FTerminal->DoCopyToRemote(FilesToCopy, TargetDir, CopyParam, Params, OperationProgress, tfPreCreateDir, OnceDoneOperation);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::Source(
  TLocalFileHandle & Handle, const UnicodeString & TargetDir, UnicodeString & DestFileName,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int /*Flags*/,
  TUploadSessionAction & Action, bool & ChildError)
{
  int FD = -1;
  try
  {
    UnicodeString DestFullName = TargetDir + DestFileName;

    TRemoteFile * RemoteFile = NULL;
    try
    {
      TValueRestorer<TIgnoreAuthenticationFailure> IgnoreAuthenticationFailureRestorer(FIgnoreAuthenticationFailure);
      FIgnoreAuthenticationFailure = iafWaiting;

      // this should not throw
      CustomReadFileInternal(DestFullName, RemoteFile, NULL);
    }
    catch (...)
    {
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
      FileParams.DestTimestamp = RemoteFile->Modification;
      delete RemoteFile;

      ConfirmOverwrite(Handle.FileName, DestFileName, OperationProgress,
        &FileParams, CopyParam, Params);
    }

    DestFullName = TargetDir + DestFileName;
    // only now, we know the final destination
    // (not really true as we do not support changing file name on overwrite dialog)
    Action.Destination(DestFullName);

    FUploadMimeType = Configuration->GetFileMimeType(DestFileName);

    FILE_OPERATION_LOOP_BEGIN
    {
      SetFilePointer(Handle.Handle, 0, NULL, FILE_BEGIN);

      FD = _open_osfhandle((intptr_t)Handle.Handle, O_BINARY);
      if (FD < 0) // can happen only if there are no free slots in handle-FD table
      {
        throw ESkipFile();
      }

      TAutoFlag UploadingFlag(FUploading);

      ClearNeonError();
      CheckStatus(ne_put(FSessionContext->NeonSession, PathToNeon(DestFullName), FD));
    }
    FILE_OPERATION_LOOP_END(FMTLOAD(TRANSFER_ERROR, (Handle.FileName)));

    if (CopyParam->PreserveTime)
    {
      FTerminal->LogEvent(FORMAT(L"Preserving timestamp [%s]",
        (StandardTimestamp(Handle.Modification))));

      TTouchSessionAction TouchAction(FTerminal->ActionLog, DestFullName, Handle.Modification);
      try
      {
        TDateTime ModificationUTC = ConvertTimestampToUTC(Handle.Modification);
        TFormatSettings FormatSettings = GetEngFormatSettings();
        UnicodeString LastModified =
          FormatDateTime(L"ddd, d mmm yyyy hh:nn:ss 'GMT'", ModificationUTC, FormatSettings);
        UTF8String NeonLastModified(LastModified);
        // second element is "NULL-terminating"
        ne_proppatch_operation Operations[2];
        memset(Operations, 0, sizeof(Operations));
        ne_propname LastModifiedProp;
        LastModifiedProp.nspace = DAV_PROP_NAMESPACE;
        LastModifiedProp.name = PROP_LAST_MODIFIED;
        Operations[0].name = &LastModifiedProp;
        Operations[0].type = ne_propset;
        Operations[0].value = NeonLastModified.c_str();
        int Status = ne_proppatch(FSessionContext->NeonSession, PathToNeon(DestFullName), Operations);
        if (Status == NE_ERROR)
        {
          FTerminal->LogEvent(FORMAT(L"Preserving timestamp failed, ignoring: %s",
            (GetNeonError())));
          // Ignore errors as major WebDAV servers (like IIS), do not support
          // changing getlastmodified.
          // The only server we found that supports this is TradeMicro SafeSync.
          // But it announces itself as "Server: Apache",
          // so it's not reliable to autodetect the support.
          // Microsoft Office allegedly uses <Win32LastModifiedTime>
          // https://sabre.io/dav/clients/msoffice/
          // Carot DAV does that too. But we do not know what server does support this.
          TouchAction.Cancel();
        }
        else
        {
          CheckStatus(Status);
        }
      }
      catch (Exception & E)
      {
        TouchAction.Rollback(&E);
        ChildError = true;
        throw;
      }
    }
  }
  __finally
  {
    if (FD >= 0)
    {
      // _close calls CloseHandle internally (even doc states, we should not call CloseHandle),
      // but it crashes code guard
      _close(FD);
      Handle.Dismiss();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CopyToLocal(TStrings * FilesToCopy,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  Params &= ~cpAppend;

  FTerminal->DoCopyToLocal(FilesToCopy, TargetDir, CopyParam, Params, OperationProgress, tfNone, OnceDoneOperation);
}
//---------------------------------------------------------------------------
void TWebDAVFileSystem::NeonCreateRequest(
  ne_request * Request, void * UserData, const char * /*Method*/, const char * /*Uri*/)
{
  TSessionContext * SessionContext = static_cast<TSessionContext *>(UserData);
  ne_set_request_private(Request, SESSION_CONTEXT_KEY, SessionContext);
  ne_add_response_body_reader(Request, NeonBodyAccepter, NeonBodyReader, Request);
  SessionContext->NtlmAuthenticationFailed = false;
}
//---------------------------------------------------------------------------
void TWebDAVFileSystem::NeonPreSend(
  ne_request * Request, void * UserData, ne_buffer * Header)
{
  TSessionContext * SessionContext = static_cast<TSessionContext *>(UserData);
  TWebDAVFileSystem * FileSystem = SessionContext->FileSystem;

  SessionContext->AuthorizationProtocol = EmptyStr;
  UnicodeString HeaderBuf(UnicodeString(AnsiString(Header->data, Header->used)));
  const UnicodeString AuthorizationHeaderName(L"Authorization:");
  int P = HeaderBuf.Pos(AuthorizationHeaderName);
  if (P > 0)
  {
    P += AuthorizationHeaderName.Length();
    int P2 = PosEx(L"\n", HeaderBuf, P);
    if (DebugAlwaysTrue(P2 > 0))
    {
      UnicodeString AuthorizationHeader = HeaderBuf.SubString(P, P2 - P).Trim();
      SessionContext->AuthorizationProtocol = CutToChar(AuthorizationHeader, L' ', false);
      if (SessionContext == FileSystem->FSessionContext)
      {
        FileSystem->FLastAuthorizationProtocol = SessionContext->AuthorizationProtocol;
      }
    }
  }

  if (FileSystem->FDownloading)
  {
    // Needed by IIS server to make it download source code, not code output,
    // and mainly to even allow downloading file with unregistered extensions.
    // Without it files like .001 return 404 (Not found) HTTP code.
    // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-wdv/e37a9543-9290-4843-8c04-66457c60fa0a
    // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-wdvse/501879f9-3875-4d7a-ab88-3cecab440034
    // It's also supported by Oracle server:
    // https://docs.oracle.com/cd/E19146-01/821-1828/gczya/index.html
    // We do not know yet of any server that fails when the header is used,
    // so it's added unconditionally.
    ne_buffer_zappend(Header, "Translate: f\r\n");
  }

  const UnicodeString ContentTypeHeaderPrefix(L"Content-Type: ");
  if (FileSystem->FTerminal->Log->Logging)
  {
    const char * Buffer;
    size_t Size;
    if (ne_get_request_body_buffer(Request, &Buffer, &Size))
    {
      // all neon request types that use ne_add_request_header
      // use XML content-type, so it's text-based
      DebugAssert(ContainsStr(HeaderBuf, ContentTypeHeaderPrefix + NE_XML_MEDIA_TYPE));
      FileSystem->FTerminal->Log->Add(llInput, UnicodeString(UTF8String(Buffer, Size)));
    }
  }

  if (FileSystem->FUploading)
  {
    ne_set_request_body_provider_pre(Request, FileSystem->NeonUploadBodyProvider, SessionContext);
    if (!FileSystem->FUploadMimeType.IsEmpty())
    {
      UnicodeString ContentTypeHeader = ContentTypeHeaderPrefix + FileSystem->FUploadMimeType + L"\r\n";
      ne_buffer_zappend(Header, AnsiString(ContentTypeHeader).c_str());
    }
  }

  FileSystem->FResponse = L"";
}
//---------------------------------------------------------------------------
int TWebDAVFileSystem::NeonPostSend(ne_request * /*Req*/, void * UserData,
  const ne_status * /*Status*/)
{
  TWebDAVFileSystem * FileSystem = static_cast<TSessionContext *>(UserData)->FileSystem;
  if (!FileSystem->FResponse.IsEmpty())
  {
    FileSystem->FTerminal->Log->Add(llOutput, FileSystem->FResponse);
  }
  return NE_OK;
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::IsNtlmAuthentication(TSessionContext * SessionContext)
{
  return
    SameText(SessionContext->AuthorizationProtocol, L"NTLM") ||
    SameText(SessionContext->AuthorizationProtocol, L"Negotiate");
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::HttpAuthenticationFailed(TSessionContext * SessionContext)
{
  // NTLM/GSSAPI failed
  if (IsNtlmAuthentication(SessionContext))
  {
    if (SessionContext->NtlmAuthenticationFailed)
    {
      // Next time do not try Negotiate (NTLM/GSSAPI),
      // otherwise we end up in an endless loop.
      // If the server returns all other challenges in the response, removing the Negotiate
      // protocol will itself ensure that other protocols are tried (we haven't seen this behaviour).
      // IIS will return only Negotiate response if the request was Negotiate, so there's no fallback.
      // We have to retry with a fresh request. That's what FAuthenticationRetry does.
      FTerminal->LogEvent(FORMAT(L"%s challenge failed, will try different challenge", (SessionContext->AuthorizationProtocol)));
      ne_remove_server_auth(SessionContext->NeonSession);
      NeonAddAuthentication(SessionContext, false);
      FAuthenticationRetry = true;
    }
    else
    {
      // The first 401 is expected, the server is using it to send WWW-Authenticate header with data.
      SessionContext->NtlmAuthenticationFailed = true;
    }
  }
}
//---------------------------------------------------------------------------
void TWebDAVFileSystem::NeonPostHeaders(ne_request * /*Req*/, void * UserData, const ne_status * Status)
{
  TSessionContext * SessionContext = static_cast<TSessionContext *>(UserData);
  TWebDAVFileSystem * FileSystem = SessionContext->FileSystem;
  if (Status->code == HttpUnauthorized)
  {
    FileSystem->HttpAuthenticationFailed(SessionContext);
  }
}
//---------------------------------------------------------------------------
ssize_t TWebDAVFileSystem::NeonUploadBodyProvider(void * UserData, char * /*Buffer*/, size_t /*BufLen*/)
{
  TWebDAVFileSystem * FileSystem = static_cast<TSessionContext *>(UserData)->FileSystem;
  ssize_t Result;
  if (FileSystem->CancelTransfer())
  {
    Result = -1;
  }
  else
  {
    Result = 1;
  }
  return Result;
}
//---------------------------------------------------------------------------
static void __fastcall AddHeaderValueToList(UnicodeString & List, ne_request * Request, const char * Name)
{
  const char * Value = ne_get_response_header(Request, Name);
  if (Value != NULL)
  {
    AddToList(List, StrFromNeon(Value), L"; ");
  }
}
//---------------------------------------------------------------------------
int TWebDAVFileSystem::NeonBodyAccepter(void * UserData, ne_request * Request, const ne_status * Status)
{
  DebugAssert(UserData == Request);
  TSessionContext * SessionContext = static_cast<TSessionContext *>(ne_get_request_private(Request, SESSION_CONTEXT_KEY));
  TWebDAVFileSystem * FileSystem = SessionContext->FileSystem;

  bool AuthenticationFailureCode = (Status->code == HttpUnauthorized);
  bool PasswordAuthenticationFailed = AuthenticationFailureCode && FileSystem->FAuthenticationRequested;
  bool AuthenticationFailed = PasswordAuthenticationFailed || (AuthenticationFailureCode && FileSystem->IsNtlmAuthentication(SessionContext));
  bool AuthenticationNeeded = AuthenticationFailureCode && !AuthenticationFailed;

  if (FileSystem->FInitialHandshake)
  {
    UnicodeString Line;
    if (AuthenticationNeeded)
    {
      Line = LoadStr(STATUS_AUTHENTICATE);
    }
    else if (AuthenticationFailed)
    {
      Line = LoadStr(FTP_ACCESS_DENIED);
    }
    else if (Status->klass == 2)
    {
      Line = LoadStr(STATUS_AUTHENTICATED);
    }

    if (!Line.IsEmpty())
    {
      FileSystem->FTerminal->Information(Line);
    }

    UnicodeString RemoteSystem;
    // Used by IT Hit WebDAV Server:
    // Server: Microsoft-HTTPAPI/1.0
    // X-Engine: IT Hit WebDAV Server .Net v3.8.1877.0 (Evaluation License)
    AddHeaderValueToList(RemoteSystem, Request, "X-Engine");
    // Used by OpenDrive:
    // Server: Apache/2.2.17 (Fedora)
    // X-Powered-By: PHP/5.5.7
    // X-DAV-Powered-By: OpenDrive
    AddHeaderValueToList(RemoteSystem, Request, "X-DAV-Powered-By");
    // Used by IIS:
    // Server: Microsoft-IIS/8.5
    AddHeaderValueToList(RemoteSystem, Request, "Server");
    // Not really useful.
    // Can be e.g. "PleskLin"
    AddHeaderValueToList(RemoteSystem, Request, "X-Powered-By");
    FileSystem->FFileSystemInfo.RemoteSystem = RemoteSystem;
  }

  // When we explicitly fail authentication of request
  // with FIgnoreAuthenticationFailure flag (after it failed with password),
  // neon resets its internal password store and tries the next request
  // without calling our authentication hook first
  // (note AuthenticationFailed vs. AuthenticationNeeded)
  // what likely fails, but we do not want to reset out password
  // (as it was not even tried yet for this request).
  if (PasswordAuthenticationFailed)
  {
    if (FileSystem->FIgnoreAuthenticationFailure == iafNo)
    {
      FileSystem->FPassword = RawByteString();
    }
    else
    {
      FileSystem->FIgnoreAuthenticationFailure = iafPasswordFailed;
    }
  }

  return ne_accept_2xx(UserData, Request, Status);
}
//---------------------------------------------------------------------------
bool __fastcall TWebDAVFileSystem::CancelTransfer()
{
  bool Result = false;
  if ((FUploading || FDownloading) &&
      (FTerminal->OperationProgress != NULL) &&
      (FTerminal->OperationProgress->Cancel != csContinue))
  {
    if (FTerminal->OperationProgress->ClearCancelFile())
    {
      FSkipped = true;
    }
    else
    {
      FCancelled = true;
    }
    Result = true;
  }
  return Result;
}
//---------------------------------------------------------------------------
int TWebDAVFileSystem::NeonBodyReader(void * UserData, const char * Buf, size_t Len)
{
  ne_request * Request = static_cast<ne_request *>(UserData);
  TSessionContext * SessionContext = static_cast<TSessionContext *>(ne_get_request_private(Request, SESSION_CONTEXT_KEY));
  TWebDAVFileSystem * FileSystem = SessionContext->FileSystem;

  if (FileSystem->FTerminal->Log->Logging)
  {
    ne_content_type ContentType;
    if (ne_get_content_type(Request, &ContentType) == 0)
    {
      // The main point of the content-type check was to exclude
      // GET responses (with file contents).
      // But this won't work when downloading text files that have text
      // content type on their own, hence the additional not-downloading test.
      if (!FileSystem->FDownloading &&
          ((ne_strcasecmp(ContentType.type, "text") == 0) ||
           media_type_is_xml(&ContentType)))
      {
        UnicodeString Content = UnicodeString(UTF8String(Buf, Len)).Trim();
        FileSystem->FResponse += Content;
      }
      ne_free(ContentType.value);
    }
  }

  int Result = FileSystem->CancelTransfer() ? 1 : 0;
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::Sink(
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
    FileParams.SourceTimestamp = File->Modification;
    FileParams.DestSize = Size;
    FileParams.DestTimestamp = UnixToDateTime(MTime, FTerminal->SessionData->DSTMode);

    ConfirmOverwrite(FileName, DestFileName, OperationProgress, &FileParams, CopyParam, Params);
  }

  UnicodeString ExpandedDestFullName = ExpandUNCFileName(DestFullName);
  Action.Destination(ExpandedDestFullName);

  FILE_OPERATION_LOOP_BEGIN
  {
    HANDLE LocalHandle;
    if (!FTerminal->CreateLocalFile(DestFullName, OperationProgress, &LocalHandle, FLAGSET(Params, cpNoConfirmation)))
    {
      throw ESkipFile();
    }

    bool DeleteLocalFile = true;

    int FD = -1;
    try
    {
      FD = _open_osfhandle((intptr_t)LocalHandle, O_BINARY);
      if (FD < 0)
      {
        throw ESkipFile();
      }

      TAutoFlag DownloadingFlag(FDownloading);

      ClearNeonError();
      int NeonStatus = ne_get(FSessionContext->NeonSession, PathToNeon(FileName), FD);
      // Contrary to other actions, for "GET" we support any redirect
      if (IsRedirect(NeonStatus))
      {
        UnicodeString CorrectedUrl = GetRedirectUrl();
        UTF8String CorrectedFileName, Query;
        std::unique_ptr<TSessionContext> CorrectedSessionContext(NeonOpen(CorrectedUrl, CorrectedFileName, Query));
        UTF8String RedirectUrl = CorrectedFileName;
        if (!Query.IsEmpty())
        {
          RedirectUrl += L"?" + Query;
        }
        NeonStatus = ne_get(CorrectedSessionContext->NeonSession, RedirectUrl.c_str(), FD);
        CheckStatus(CorrectedSessionContext.get(), NeonStatus);
      }
      else
      {
        CheckStatus(NeonStatus);
      }

      DeleteLocalFile = false;

      if (CopyParam->PreserveTime)
      {
        FTerminal->UpdateTargetTime(
          LocalHandle, File->Modification, File->ModificationFmt, FTerminal->SessionData->DSTMode);
      }
    }
    __finally
    {
      if (FD >= 0)
      {
        // _close calls CloseHandle internally (even doc states, we should not call CloseHandle),
        // but it crashes code guard
        _close(FD);
      }
      else
      {
        CloseHandle(LocalHandle);
      }

      if (DeleteLocalFile)
      {
        FTerminal->DoDeleteLocalFile(DestFullName);
      }
    }
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(TRANSFER_ERROR, (FileName)));

  FTerminal->UpdateTargetAttrs(DestFullName, File, CopyParam, Attrs);
}
//---------------------------------------------------------------------------
bool TWebDAVFileSystem::VerifyCertificate(TSessionContext * SessionContext, TNeonCertificateData Data, bool Aux)
{
  bool Result =
    FTerminal->VerifyOrConfirmHttpCertificate(
      SessionContext->HostName, SessionContext->PortNumber, Data, !Aux, FSessionInfo);

  if (Result && !Aux && (SessionContext == FSessionContext))
  {
    CollectTLSSessionInfo();
  }

  return Result;
}
//------------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::CollectTLSSessionInfo()
{
  // See also TFTPFileSystem::Open().
  // Have to cache the value as the connection (the neon HTTP session, not "our" session)
  // can be closed at the time we need it in CollectUsage().
  UnicodeString Message = NeonTlsSessionInfo(FSessionContext->NeonSession, FSessionInfo, FTlsVersionStr);
  FTerminal->LogEvent(0, Message);
}
//------------------------------------------------------------------------------
// A neon-session callback to validate the SSL certificate when the CA
// is unknown (e.g. a self-signed cert), or there are other SSL
// certificate problems.
int TWebDAVFileSystem::DoNeonServerSSLCallback(void * UserData, int Failures, const ne_ssl_certificate * Certificate, bool Aux)
{
  TNeonCertificateData Data;
  RetrieveNeonCertificateData(Failures, Certificate, Data);
  TSessionContext * SessionContext = static_cast<TSessionContext *>(UserData);
  TWebDAVFileSystem * FileSystem = SessionContext->FileSystem;
  return FileSystem->VerifyCertificate(SessionContext, Data, Aux) ? NE_OK : NE_ERROR;
}
//------------------------------------------------------------------------------
int TWebDAVFileSystem::NeonServerSSLCallbackMain(void * UserData, int Failures, const ne_ssl_certificate * Certificate)
{
  return DoNeonServerSSLCallback(UserData, Failures, Certificate, false);
}
//------------------------------------------------------------------------------
int TWebDAVFileSystem::NeonServerSSLCallbackAux(void * UserData, int Failures, const ne_ssl_certificate * Certificate)
{
  return DoNeonServerSSLCallback(UserData, Failures, Certificate, true);
}
//------------------------------------------------------------------------------
void TWebDAVFileSystem::NeonProvideClientCert(void * UserData, ne_session * Sess,
  const ne_ssl_dname * const * /*DNames*/, int /*DNCount*/)
{
  TWebDAVFileSystem * FileSystem = static_cast<TSessionContext *>(UserData)->FileSystem;

  FileSystem->FTerminal->LogEvent(LoadStr(NEED_CLIENT_CERTIFICATE));

  X509 * Certificate;
  EVP_PKEY * PrivateKey;
  if (FileSystem->FTerminal->LoadTlsCertificate(Certificate, PrivateKey))
  {
    ne_ssl_client_cert * NeonCertificate = ne_ssl_clicert_create(Certificate, PrivateKey);
    ne_ssl_set_clicert(Sess, NeonCertificate);
    ne_ssl_clicert_free(NeonCertificate);
  }
}
//------------------------------------------------------------------------------
int TWebDAVFileSystem::NeonRequestAuth(
  void * UserData, const char * Realm, int Attempt, char * UserName, char * Password)
{
  DebugUsedParam(Realm);
  DebugUsedParam(Attempt);
  TWebDAVFileSystem * FileSystem = static_cast<TSessionContext *>(UserData)->FileSystem;

  TTerminal * Terminal = FileSystem->FTerminal;
  TSessionData * SessionData = Terminal->SessionData;

  bool Result = true;

  // will ask for username only once
  if (FileSystem->FUserName.IsEmpty())
  {
    if (!SessionData->UserName.IsEmpty())
    {
      FileSystem->FUserName = SessionData->UserNameExpanded;
    }
    else
    {
      Terminal->LogEvent(L"Username prompt");
      if (!Terminal->PromptUser(SessionData, pkUserName, LoadStr(USERNAME_TITLE), L"",
            LoadStr(USERNAME_PROMPT2), true, NE_ABUFSIZ, FileSystem->FUserName))
      {
        Result = false;
      }
    }
  }

  UnicodeString APassword;
  if (Result)
  {
    // Some servers (Gallery2 on discontinued g2.pixi.me)
    // return authentication error (401) on PROPFIND request for
    // non-existing files.
    // When we already tried password before, do not try anymore.
    // When we did not try password before (possible only when
    // server does not require authentication for any previous request,
    // such as when read access is not authenticated), try it now,
    // but use special flag for the try, because when it fails
    // we still want to try password for future requests (such as PUT).

    if (!FileSystem->FPassword.IsEmpty())
    {
      if (FileSystem->FIgnoreAuthenticationFailure == iafPasswordFailed)
      {
        // Fail PROPFIND /nonexisting request...
        Result = false;
      }
      else
      {
        APassword = Terminal->DecryptPassword(FileSystem->FPassword);
      }
    }
    else
    {
      if (!SessionData->Password.IsEmpty() && !FileSystem->FStoredPasswordTried)
      {
        APassword = NormalizeString(SessionData->Password);
        FileSystem->FStoredPasswordTried = true;
      }
      else
      {
        // Asking for password (or using configured password) the first time,
        // and asking for password.
        Terminal->LogEvent(L"Password prompt");
        Result =
          Terminal->PromptUser(
            SessionData, pkPassword, LoadStr(PASSWORD_TITLE), L"",
            LoadStr(PASSWORD_PROMPT), false, NE_ABUFSIZ, APassword);
      }

      if (Result)
      {
        // While neon remembers the password on its own,
        // we need to keep a copy in case neon store gets reset by
        // 401 response to PROPFIND /nonexisting on G2, see above.
        // Possibly we can do this for G2 servers only.
        FileSystem->FPassword = Terminal->EncryptPassword(APassword);
      }
    }
  }

  if (Result)
  {
    strncpy(UserName, StrToNeon(FileSystem->FUserName), NE_ABUFSIZ);
    strncpy(Password, StrToNeon(APassword), NE_ABUFSIZ);
  }

  FileSystem->FAuthenticationRequested = true;

  return Result ? 0 : -1;
}
//------------------------------------------------------------------------------
void TWebDAVFileSystem::NeonNotifier(void * UserData, ne_session_status Status, const ne_session_status_info * StatusInfo)
{
  TWebDAVFileSystem * FileSystem = static_cast<TSessionContext *>(UserData)->FileSystem;
  TFileOperationProgressType * OperationProgress = FileSystem->FTerminal->OperationProgress;

  // We particularly have to filter out response to "put" request,
  // handling that would reset the upload progress back to low number (response is small).
  if (((FileSystem->FUploading && (Status == ne_status_sending)) ||
       (FileSystem->FDownloading && (Status == ne_status_recving))) &&
      DebugAlwaysTrue(OperationProgress != NULL))
  {
    __int64 Progress = StatusInfo->sr.progress;
    __int64 Diff = Progress - OperationProgress->TransferredSize;

    if (Diff > 0)
    {
      OperationProgress->ThrottleToCPSLimit(static_cast<unsigned long>(Diff));
    }

    __int64 Total = StatusInfo->sr.total;

    // Total size unknown
    if (Total < 0)
    {
      if (Diff >= 0)
      {
        OperationProgress->AddTransferred(Diff);
      }
      else
      {
        // Session total has been reset. A new stream started
        OperationProgress->AddTransferred(Progress);
      }
    }
    else
    {
      OperationProgress->SetTransferSize(Total);
      OperationProgress->AddTransferred(Diff);
    }
  }
}
//------------------------------------------------------------------------------
void TWebDAVFileSystem::InitSslSession(ssl_st * Ssl, ne_session * /*Session*/)
{
  SetupSsl(Ssl, FTerminal->SessionData->MinTlsVersion, FTerminal->SessionData->MaxTlsVersion);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::GetSupportedChecksumAlgs(TStrings * /*Algs*/)
{
  // NOOP
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::LockFile(const UnicodeString & /*FileName*/, const TRemoteFile * File)
{
  ClearNeonError();
  struct ne_lock * Lock = ne_lock_create();
  try
  {
    Lock->uri.path = ne_strdup(PathToNeon(FilePath(File)));
    Lock->depth = NE_DEPTH_INFINITE;
    Lock->timeout = NE_TIMEOUT_INFINITE;
    Lock->owner = ne_strdup(StrToNeon(FTerminal->UserName));
    CheckStatus(ne_lock(FSessionContext->NeonSession, Lock));

    {
      TGuard Guard(FNeonLockStoreSection);

      RequireLockStore();

      ne_lockstore_add(FNeonLockStore, Lock);
    }
    // ownership passed
    Lock = NULL;
  }
  __finally
  {
    if (Lock != NULL)
    {
      ne_lock_destroy(Lock);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::RequireLockStore()
{
  // Create store only when needed,
  // to limit the use of cross-thread code in UpdateFromMain
  if (FNeonLockStore == NULL)
  {
    FNeonLockStore = ne_lockstore_create();
    ne_lockstore_register(FNeonLockStore, FSessionContext->NeonSession);
  }
}
//---------------------------------------------------------------------------
void TWebDAVFileSystem::LockResult(void * UserData, const struct ne_lock * Lock,
  const ne_uri * /*Uri*/, const ne_status * /*Status*/)
{
  // Is NULL on failure (Status is not NULL then)
  if (Lock != NULL)
  {
    RawByteString & LockToken = *static_cast<RawByteString *>(UserData);
    LockToken = Lock->token;
  }
}
//---------------------------------------------------------------------------
struct ne_lock * __fastcall TWebDAVFileSystem::FindLock(const RawByteString & Path)
{
  ne_uri Uri = ne_uri();
  Uri.path = Path.c_str();
  return ne_lockstore_findbyuri(FNeonLockStore, &Uri);
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::DiscardLock(const RawByteString & Path)
{
  TGuard Guard(FNeonLockStoreSection);
  if (FNeonLockStore != NULL)
  {
    struct ne_lock * Lock = FindLock(Path);
    if (Lock != NULL)
    {
      ne_lockstore_remove(FNeonLockStore, Lock);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::UnlockFile(const UnicodeString & FileName, const TRemoteFile * File)
{
  ClearNeonError();
  struct ne_lock * Lock = ne_lock_create();
  try
  {
    RawByteString Path = PathToNeon(FilePath(File));
    RawByteString LockToken;

    struct ne_lock * Lock = NULL;

    {
      TGuard Guard(FNeonLockStoreSection);
      if (FNeonLockStore != NULL)
      {
        Lock = FindLock(Path);
      }
    }

    // we are not aware of the file being locked,
    // though it can be locked from another (previous and already closed)
    // session, so query the server.
    if (Lock == NULL)
    {
      CheckStatus(ne_lock_discover(FSessionContext->NeonSession, Path.c_str(), LockResult, &LockToken));
    }

    if ((Lock == NULL) && (LockToken.IsEmpty()))
    {
      throw Exception(FMTLOAD(NOT_LOCKED, (FileName)));
    }
    else
    {
      struct ne_lock * Unlock;
      if (Lock == NULL)
      {
        DebugAssert(!LockToken.IsEmpty());
        Unlock = ne_lock_create();
        Unlock->uri.path = ne_strdup(Path.c_str());
        Unlock->token = ne_strdup(LockToken.c_str());
      }
      else
      {
        Unlock = Lock;
      }
      CheckStatus(ne_unlock(FSessionContext->NeonSession, Unlock));

      DiscardLock(Path);
    }
  }
  __finally
  {
    ne_lock_destroy(Lock);
  }
}
//---------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::UpdateFromMain(TCustomFileSystem * AMainFileSystem)
{
  TWebDAVFileSystem * MainFileSystem = dynamic_cast<TWebDAVFileSystem *>(AMainFileSystem);
  if (DebugAlwaysTrue(MainFileSystem != NULL))
  {
    TGuard Guard(FNeonLockStoreSection);
    TGuard MainGuard(MainFileSystem->FNeonLockStoreSection);

    if (FNeonLockStore != NULL)
    {
      struct ne_lock * Lock;
      while ((Lock = ne_lockstore_first(FNeonLockStore)) != NULL)
      {
        ne_lockstore_remove(FNeonLockStore, Lock);
      }
    }

    if (MainFileSystem->FNeonLockStore != NULL)
    {
      RequireLockStore();
      struct ne_lock * Lock = ne_lockstore_first(MainFileSystem->FNeonLockStore);
      while (Lock != NULL)
      {
        ne_lockstore_add(FNeonLockStore, ne_lock_copy(Lock));
        Lock = ne_lockstore_next(MainFileSystem->FNeonLockStore);
      }
    }
  }
}
//------------------------------------------------------------------------------
void __fastcall TWebDAVFileSystem::ClearCaches()
{
  // noop
}
//---------------------------------------------------------------------------
