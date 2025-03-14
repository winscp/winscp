//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#define NE_LFS
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
#include "CoreMain.h"
#include "Http.h"
#include "Cryptography.h"
#include <System.JSON.hpp>
#include <System.DateUtils.hpp>
#include "request.h"
#include <XMLDoc.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
// Should be used with character pointer only
#define StrFromS3(S) StrFromNeon(S)
#define StrToS3(S) StrToNeon(S)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_TERMINAL FTerminal
//---------------------------------------------------------------------------
#define AWS_ACCESS_KEY_ID L"AWS_ACCESS_KEY_ID"
#define AWS_SECRET_ACCESS_KEY L"AWS_SECRET_ACCESS_KEY"
#define AWS_SESSION_TOKEN L"AWS_SESSION_TOKEN"
#define AWS_SHARED_CREDENTIALS_FILE L"AWS_SHARED_CREDENTIALS_FILE"
#define AWS_CONFIG_FILE L"AWS_CONFIG_FILE"
#define AWS_PROFILE L"AWS_PROFILE"
#define AWS_PROFILE_DEFAULT L"default"
#define AWS_CONFIG_PROFILE_PREFIX L"profile "
#define AWS_SOURCE_PROFILE_KEY L"source_profile"
#define AWS_CREDENTIAL_SOURCE_KEY L"credential_source"
#define AWS_CREDENTIAL_SOURCE_ENVIRONMENT L"Environment"
#define AWS_CREDENTIAL_SOURCE_METADATA L"Ec2InstanceMetadata"
#define AWS_ROLE_ARN L"AWS_ROLE_ARN"
#define AWS_ROLE_ARN_KEY L"role_arn"
//---------------------------------------------------------------------------
static std::unique_ptr<TCriticalSection> LibS3Section(TraceInitPtr(new TCriticalSection()));
//---------------------------------------------------------------------------
static UTF8String LibS3Delimiter(L"/");
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
bool IsAmazonS3SessionData(TSessionData * Data)
{
  return IsDomainOrSubdomain(Data->HostNameExpanded, S3HostName);
}
//---------------------------------------------------------------------------
static void NeedS3Config(
  UnicodeString & FileName, TDateTime & TimeStamp, std::unique_ptr<TCustomIniFile> & File,
  const UnicodeString & DefaultName, const UnicodeString & EnvName)
{
  if (FileName.IsEmpty())
  {
    FileName = GetEnvironmentVariable(EnvName);
    if (FileName.IsEmpty())
    {
      UnicodeString ProfilePath = GetShellFolderPath(CSIDL_PROFILE);
      UnicodeString DefaultFileName = IncludeTrailingBackslash(ProfilePath) + L".aws\\" + DefaultName;
      if (FileExists(DefaultFileName))
      {
        FileName = DefaultFileName;
      }
    }
  }

  TDateTime CurrentTimestamp;
  FileAge(FileName, CurrentTimestamp);
  if (TimeStamp != CurrentTimestamp)
  {
    TimeStamp = CurrentTimestamp;
    // TMemIniFile silently ignores empty paths or non-existing files
    AppLogFmt(L"Reading AWS '%s' file", (FileName));
    File.reset(new TMemIniFile(FileName));
  }
}
//---------------------------------------------------------------------------
static UnicodeString S3CredentialsFileName;
static TDateTime S3CredentialsTimestamp;
static std::unique_ptr<TCustomIniFile> S3CredentialsFile;
static UnicodeString S3ConfigFileName;
static TDateTime S3ConfigTimestamp;
static std::unique_ptr<TCustomIniFile> S3ConfigFile;
static UnicodeString S3Profile;
static bool S3SecurityProfileChecked = false;
static TDateTime S3CredentialsExpiration;
static UnicodeString S3SessionToken;
static UnicodeString S3SecurityProfile;
typedef std::map<UnicodeString, UnicodeString> TS3Credentials;
static TS3Credentials S3Credentials;
//---------------------------------------------------------------------------
static void NeedS3Config()
{
  TGuard Guard(LibS3Section.get());
  if (S3Profile.IsEmpty())
  {
    S3Profile = GetEnvironmentVariable(AWS_PROFILE);
    if (S3Profile.IsEmpty())
    {
      S3Profile = AWS_PROFILE_DEFAULT;
    }
  }

  NeedS3Config(S3CredentialsFileName, S3CredentialsTimestamp, S3CredentialsFile, L"credentials", AWS_SHARED_CREDENTIALS_FILE);
  NeedS3Config(S3ConfigFileName, S3ConfigTimestamp, S3ConfigFile, L"config", AWS_CONFIG_FILE);
}
//---------------------------------------------------------------------------
void GetS3Profiles(TStrings * Profiles, TCustomIniFile * File, const UnicodeString & Prefix = EmptyStr)
{
  if (File != NULL)
  {
    std::unique_ptr<TStrings> Sections(new TStringList());
    File->ReadSections(Sections.get());
    for (int Index = 0; Index < Sections->Count; Index++)
    {
      UnicodeString Section = Sections->Strings[Index];
      if (Prefix.IsEmpty() || StartsText(Prefix, Section))
      {
        bool Supported = false;
        if (!File->ReadString(Section, AWS_ACCESS_KEY_ID, EmptyStr).IsEmpty() &&
            !File->ReadString(Section, AWS_SECRET_ACCESS_KEY, EmptyStr).IsEmpty())
        {
          Supported = true;
        }
        else if (!File->ReadString(Section, AWS_ROLE_ARN_KEY, EmptyStr).IsEmpty())
        {
          if (!File->ReadString(Section, AWS_SOURCE_PROFILE_KEY, EmptyStr).IsEmpty())
          {
            Supported = true;
          }
          else
          {
            UnicodeString CredentialSource = File->ReadString(Section, AWS_CREDENTIAL_SOURCE_KEY, EmptyStr);
            Supported =
              SameText(CredentialSource, AWS_CREDENTIAL_SOURCE_ENVIRONMENT) ||
              SameText(CredentialSource, AWS_CREDENTIAL_SOURCE_METADATA);
          }
        }

        if (Supported)
        {
          Profiles->Add(MidStr(Section, Prefix.Length() + 1));
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
TStrings * GetS3Profiles()
{
  NeedS3Config();
  // S3 allegedly treats the section case-sensitively, but our GetS3ConfigValue (ReadString) does not,
  // so consistently we return case-insensitive list.
  std::unique_ptr<TStrings> Result(CreateSortedStringList());
  GetS3Profiles(Result.get(), S3CredentialsFile.get());
  GetS3Profiles(Result.get(), S3ConfigFile.get(), AWS_CONFIG_PROFILE_PREFIX);
  return Result.release();
}
//---------------------------------------------------------------------------
static THttp * CreateHttp(const UnicodeString & Url, int ConnectTimeout)
{
  std::unique_ptr<THttp> Http(new THttp());
  Http->ResponseLimit = BasicHttpResponseLimit;
  Http->ConnectTimeout = ConnectTimeout;
  Http->URL = Url;
  return Http.release();
}
//---------------------------------------------------------------------------
static UnicodeString ReadSecurityUrl(const UnicodeString & Url, int ConnectTimeout = 0)
{
  std::unique_ptr<THttp> Http(CreateHttp(Url, ConnectTimeout));
  std::unique_ptr<TStrings> RequestHeaders(new TStringList());
  if (!S3SessionToken.IsEmpty())
  {
    RequestHeaders->Values[L"X-aws-ec2-metadata-token"] = S3SessionToken;
    Http->RequestHeaders = RequestHeaders.get();
  }
  Http->Get();
  return Http->Response.Trim();
}
//---------------------------------------------------------------------------
static TDateTime ParseExpiration(const UnicodeString & S)
{
  return ISO8601ToDate(S, false);
}
//---------------------------------------------------------------------------
static UnicodeString GetS3ConfigValue(
  TCustomIniFile * File, const UnicodeString & Profile, const UnicodeString & Name, const UnicodeString & Prefix, UnicodeString & Source)
{
  UnicodeString Result;
  if (File != NULL)
  {
    UnicodeString ConfigSection = Profile;
    if (!SameText(ConfigSection, AWS_PROFILE_DEFAULT))
    {
      ConfigSection = Prefix + ConfigSection;
    }
    // This is not consistent with AWS CLI.
    // AWS CLI fails if one of aws_access_key_id or aws_secret_access_key is set and other is missing:
    // "Partial credentials found in shared-credentials-file, missing: aws_secret_access_key"
    Result = File->ReadString(ConfigSection, Name, EmptyStr);
    if (!Result.IsEmpty())
    {
      Source = FORMAT(L"%s/%s", (ExtractFileName(File->FileName), Profile));
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
static UnicodeString GetS3ConfigValue(
  const UnicodeString & Profile, const UnicodeString & Name, UnicodeString & Source)
{
  UnicodeString Result = GetS3ConfigValue(S3CredentialsFile.get(), Profile, Name, EmptyStr, Source);
  if (Result.IsEmpty())
  {
    Result = GetS3ConfigValue(S3ConfigFile.get(), Profile, Name, AWS_CONFIG_PROFILE_PREFIX, Source);
  }
  return Result;
}
//---------------------------------------------------------------------------
static UnicodeString GetS3ConfigValue(
  const UnicodeString & Profile, const UnicodeString & EnvName, const UnicodeString & AConfigName,
  const UnicodeString & CredentialsName, UnicodeString * Source, bool OnlyCached = false)
{
  UnicodeString Result;
  UnicodeString ASource;
  TGuard Guard(LibS3Section.get());
  bool TryCredentials = true;

  try
  {
    if (Profile.IsEmpty())
    {
      Result = GetEnvironmentVariable(EnvName);
    }
    if (!Result.IsEmpty())
    {
      ASource = FORMAT(L"%%%s%%", (EnvName));
    }
    else if (!AConfigName.IsEmpty())
    {
      NeedS3Config();

      UnicodeString AProfile = DefaultStr(Profile, S3Profile);
      UnicodeString ConfigName = DefaultStr(AConfigName, EnvName);
      Result = GetS3ConfigValue(AProfile, ConfigName, ASource);
      if (Result.IsEmpty())
      {
        UnicodeString SourceSource;
        UnicodeString SourceProfile = GetS3ConfigValue(AProfile, AWS_SOURCE_PROFILE_KEY, SourceSource);
        if (!SourceProfile.IsEmpty())
        {
          TryCredentials = false;
          Result = GetS3ConfigValue(SourceProfile, ConfigName, ASource);
        }
        else
        {
          UnicodeString CredentialSource = GetS3ConfigValue(AProfile, AWS_CREDENTIAL_SOURCE_KEY, SourceSource);
          if (!CredentialSource.IsEmpty())
          {
            TryCredentials = false;
            if (SameText(CredentialSource, AWS_CREDENTIAL_SOURCE_ENVIRONMENT))
            {
              Result = GetS3ConfigValue(EmptyStr, EnvName, EmptyStr, EmptyStr, &ASource);
            }
            else if (SameText(CredentialSource, AWS_CREDENTIAL_SOURCE_METADATA))
            {
              Result = GetS3ConfigValue(EmptyStr, EmptyStr, EmptyStr, CredentialsName, &ASource);
            }
          }
        }

        if (!Result.IsEmpty())
        {
          ASource = FORMAT(L"%s=>%s", (SourceSource, ASource));
        }
      }
    }
  }
  catch (Exception & E)
  {
    throw ExtException(&E, MainInstructions(LoadStr(S3_CONFIG_ERROR)));
  }

  if (Result.IsEmpty() && TryCredentials && !CredentialsName.IsEmpty())
  {
    if (S3SecurityProfileChecked && (S3CredentialsExpiration != TDateTime()) && (IncHour(S3CredentialsExpiration, -1) < Now()))
    {
      AppLog(L"AWS session token or security credentials have expired or are close to expiration, will retrieve new");
      S3SecurityProfileChecked = false;
    }

    if (!S3SecurityProfileChecked && !OnlyCached)
    {
      S3Credentials.clear();
      S3SessionToken = EmptyStr;
      S3SecurityProfile = EmptyStr;
      S3SecurityProfileChecked = true;
      S3CredentialsExpiration = TDateTime();
      try
      {
        UnicodeString AWSAPI = DefaultStr(Configuration->AWSAPI, L"http://169.254.169.254/latest/");

        int ConnectTimeout = StrToIntDef(GetEnvironmentVariable(L"AWS_METADATA_SERVICE_TIMEOUT"), 1);
        UnicodeString TokenUrl = AWSAPI + L"api/token";

        AppLogFmt(L"Trying to create IMDSv2 session token via %s", (TokenUrl));
        try
        {
          std::unique_ptr<THttp> Http(CreateHttp(TokenUrl, ConnectTimeout));
          int TtlSeconds = 6 * 60 * 60; // max possible
          TDateTime TokenExpiration = IncSecond(Now(), TtlSeconds);
          std::unique_ptr<TStrings> RequestHeaders(new TStringList());
          RequestHeaders->Values[L"X-aws-ec2-metadata-token-ttl-seconds"] = IntToStr(TtlSeconds);
          Http->RequestHeaders = RequestHeaders.get();
          Http->Put(EmptyStr);
          S3SessionToken = Http->Response.Trim();
          S3CredentialsExpiration = TokenExpiration;
          AppLogFmt(L"Created IMDSv2 session token: %s, with expiration: %s", (S3SessionToken, StandardTimestamp(TokenExpiration)));
        }
        catch (Exception & E)
        {
          UnicodeString Message;
          ExceptionMessage(&E, Message);
          AppLogFmt(L"Error creating IMDSv2 session token: %s", (Message));
        }

        UnicodeString SecurityCredentialsUrl = AWSAPI + L"meta-data/iam/security-credentials/";
        AppLogFmt(L"Retrieving AWS security credentials from %s", (SecurityCredentialsUrl));
        S3SecurityProfile = ReadSecurityUrl(SecurityCredentialsUrl, ConnectTimeout);

        if (S3SecurityProfile.IsEmpty())
        {
          AppLog(L"No AWS security credentials role detected");
        }
        else
        {
          UnicodeString SecurityProfileUrl = SecurityCredentialsUrl + EncodeUrlString(S3SecurityProfile);
          AppLogFmt(L"AWS security credentials role detected: %s, retrieving %s", (S3SecurityProfile, SecurityProfileUrl));
          UnicodeString ProfileDataStr = ReadSecurityUrl(SecurityProfileUrl);

          std::unique_ptr<TJSONValue> ProfileDataValue(TJSONObject::ParseJSONValue(ProfileDataStr));
          TJSONObject * ProfileData = dynamic_cast<TJSONObject *>(ProfileDataValue.get());
          if (ProfileData == NULL)
          {
            throw new Exception(FORMAT(L"Unexpected response: %s", (ProfileDataStr.SubString(1, 1000))));
          }
          TJSONValue * CodeValue = ProfileData->Values[L"Code"];
          if (CodeValue == NULL)
          {
            throw new Exception(L"Missing \"Code\" value");
          }
          UnicodeString Code = CodeValue->Value();
          if (!SameText(Code, L"Success"))
          {
            throw new Exception(FORMAT(L"Received non-success code: %s", (Code)));
          }
          TJSONValue * ExpirationValue = ProfileData->Values[L"Expiration"];
          if (ExpirationValue == NULL)
          {
            throw new Exception(L"Missing \"Expiration\" value");
          }
          UnicodeString ExpirationStr = ExpirationValue->Value();
          TDateTime CredentialsExpiration = ParseExpiration(ExpirationStr);
          AppLogFmt(L"Credentials expiration: %s", (StandardTimestamp(CredentialsExpiration)));
          if ((S3CredentialsExpiration == TDateTime()) ||
              (CredentialsExpiration < S3CredentialsExpiration))
          {
            S3CredentialsExpiration = CredentialsExpiration;
          }

          std::unique_ptr<TJSONObject::TEnumerator> Enumerator(ProfileData->GetEnumerator());
          UnicodeString Names;
          while (Enumerator->MoveNext())
          {
            TJSONPair * Pair = Enumerator->Current;
            UnicodeString Name = Pair->JsonString->Value();
            S3Credentials.insert(std::make_pair(Name, Pair->JsonValue->Value()));
            AddToList(Names, Name, L", ");
          }
          AppLogFmt(L"Response contains following values: %s", (Names));
        }
      }
      catch (Exception & E)
      {
        UnicodeString Message;
        ExceptionMessage(&E, Message);
        AppLogFmt(L"Error retrieving AWS security credentials role: %s", (Message));
      }
    }

    TS3Credentials::const_iterator I = S3Credentials.find(CredentialsName);
    if (I != S3Credentials.end())
    {
      Result = I->second;
      ASource = FORMAT(L"meta-data/%s", (S3SecurityProfile));
    }
  }

  if (Source != NULL)
  {
    *Source = ASource;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString S3EnvUserName(const UnicodeString & Profile, UnicodeString * Source, bool OnlyCached)
{
  return GetS3ConfigValue(Profile, AWS_ACCESS_KEY_ID, AWS_ACCESS_KEY_ID, L"AccessKeyId", Source, OnlyCached);
}
//---------------------------------------------------------------------------
UnicodeString S3EnvPassword(const UnicodeString & Profile, UnicodeString * Source, bool OnlyCached)
{
  return GetS3ConfigValue(Profile, AWS_SECRET_ACCESS_KEY, AWS_SECRET_ACCESS_KEY, L"SecretAccessKey", Source, OnlyCached);
}
//---------------------------------------------------------------------------
UnicodeString S3EnvSessionToken(const UnicodeString & Profile, UnicodeString * Source, bool OnlyCached)
{
  return GetS3ConfigValue(Profile, AWS_SESSION_TOKEN, AWS_SESSION_TOKEN, L"Token", Source, OnlyCached);
}
//---------------------------------------------------------------------------
UnicodeString S3EnvRoleArn(const UnicodeString & Profile, UnicodeString * Source, bool OnlyCached)
{
  return GetS3ConfigValue(Profile, AWS_ROLE_ARN, AWS_ROLE_ARN_KEY, EmptyStr, Source, OnlyCached);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
const int TS3FileSystem::S3MinMultiPartChunkSize = 5 * 1024 * 1024;
const int TS3FileSystem::S3MaxMultiPartChunks = 10000;
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

  FTerminal->Information(LoadStr(STATUS_CONNECT));

  TSessionData * Data = FTerminal->SessionData;

  FSessionInfo.LoginTime = Now();
  FSessionInfo.CertificateVerifiedManually = false;

  if (Data->Ftps != ftpsNone)
  {
    FLibS3Protocol = S3ProtocolHTTPS;
    RequireTls();
  }
  else
  {
    FLibS3Protocol = S3ProtocolHTTP;
  }

  UnicodeString S3Profile;
  if (Data->S3CredentialsEnv)
  {
    S3Profile = Data->S3Profile;
  }
  if (!S3Profile.IsEmpty() && !Data->FingerprintScan)
  {
    std::unique_ptr<TStrings> S3Profiles(GetS3Profiles());
    if (S3Profiles->IndexOf(S3Profile) < 0)
    {
      throw Exception(MainInstructions(FMTLOAD(S3_PROFILE_NOT_EXIST, (S3Profile))));
    }
  }

  UnicodeString AccessKeyId = Data->UserNameExpanded;
  if (AccessKeyId.IsEmpty() && !Data->FingerprintScan)
  {
    if (!FTerminal->PromptUser(Data, pkUserName, LoadStr(S3_ACCESS_KEY_ID_TITLE), L"",
          LoadStr(S3_ACCESS_KEY_ID_PROMPT), true, 0, AccessKeyId))
    {
      FTerminal->FatalError(NULL, LoadStr(CREDENTIALS_NOT_SPECIFIED));
    }
  }

  UnicodeString Password = Data->Password;
  if (Password.IsEmpty() && Data->S3CredentialsEnv)
  {
    UnicodeString PasswordSource;
    Password = S3EnvPassword(S3Profile, &PasswordSource);
    if (!Password.IsEmpty())
    {
      FTerminal->LogEvent(FORMAT(L"Password (secret access key) read from %s", (PasswordSource)));
    }
  }
  UnicodeString SecretAccessKey = UTF8String(NormalizeString(Password));
  if (SecretAccessKey.IsEmpty() && !Data->FingerprintScan)
  {
    if (!FTerminal->PromptUser(Data, pkPassword, LoadStr(S3_SECRET_ACCESS_KEY_TITLE), L"",
          LoadStr(S3_SECRET_ACCESS_KEY_PROMPT), false, 0, SecretAccessKey))
    {
      FTerminal->FatalError(NULL, LoadStr(CREDENTIALS_NOT_SPECIFIED));
    }
  }

  UnicodeString SessionToken = Data->S3SessionToken;
  if (SessionToken.IsEmpty() && Data->S3CredentialsEnv)
  {
    UnicodeString SessionTokenSource;
    SessionToken = S3EnvSessionToken(S3Profile, &SessionTokenSource);
    if (!SessionToken.IsEmpty())
    {
      FTerminal->LogEvent(FORMAT(L"Session token read from %s", (SessionTokenSource)));
    }
  }

  SetCredentials(AccessKeyId, SecretAccessKey, SessionToken);

  FHostName = UTF8String(Data->HostNameExpanded);
  FPortSuffix = UTF8String();
  int ADefaultPort = Data->GetDefaultPort();
  DebugAssert((ADefaultPort == HTTPSPortNumber) || (ADefaultPort == HTTPPortNumber));
  if (Data->PortNumber != ADefaultPort)
  {
    FPortSuffix = UTF8String(FORMAT(L":%d", (Data->PortNumber)));
  }
  FTimeout = Data->Timeout;

  RegisterForNeonDebug(FTerminal);
  UpdateNeonDebugMask();

  {
    TGuard Guard(LibS3Section.get());
    S3_initialize(NULL, S3_INIT_ALL, NULL);
  }

  if (IsGoogleCloud())
  {
    FTerminal->LogEvent(L"Google Cloud detected.");
  }

  S3_set_request_context_requester_pays(FRequestContext, Data->S3RequesterPays);

  if (IsAmazonS3SessionData(Data))
  {
    UnicodeString RoleArn = Data->S3RoleArn;
    if (RoleArn.IsEmpty())
    {
      UnicodeString RoleArnSource;
      RoleArn = S3EnvRoleArn(S3Profile, &RoleArnSource);
      if (!RoleArn.IsEmpty())
      {
        FTerminal->LogEvent(FORMAT(L"Role ARN read from %s", (RoleArnSource)));
      }
    }

    if (!RoleArn.IsEmpty())
    {
      AssumeRole(RoleArn);
    }
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
void TS3FileSystem::SetCredentials(
  const UnicodeString & AccessKeyId, const UnicodeString & SecretAccessKey, const UnicodeString & SessionToken)
{
  FAccessKeyId = UTF8String(AccessKeyId);
  if (FAccessKeyId.Length() > S3_MAX_ACCESS_KEY_ID_LENGTH)
  {
    FAccessKeyId.SetLength(S3_MAX_ACCESS_KEY_ID_LENGTH);
  }
  FSecretAccessKey = UTF8String(SecretAccessKey);

  FSecurityTokenBuf = UTF8String(SessionToken);
  FSecurityToken = static_cast<const char *>(FSecurityTokenBuf.data());
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

  SetNeonTlsInit(Session, FileSystem->InitSslSession, FileSystem->FTerminal);

  ne_set_session_flag(Session, SE_SESSFLAG_SNDBUF, Data->SendBuf);

  // Data->Timeout is propagated via timeoutMs parameter of functions like S3_list_service

  FileSystem->FNeonSession = Session;
}
//------------------------------------------------------------------------------
void TS3FileSystem::InitSslSession(ssl_st * Ssl, ne_session * /*Session*/)
{
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
bool TS3FileSystem::VerifyCertificate(TNeonCertificateData Data)
{
  bool Result =
    FTerminal->VerifyOrConfirmHttpCertificate(
      FTerminal->SessionData->HostNameExpanded, FTerminal->SessionData->PortNumber, Data, true, FSessionInfo);

  if (Result)
  {
    CollectTLSSessionInfo();
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
      case S3StatusErrorSlowDown:
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
      std::unique_ptr<Exception> E(new Exception(Error));
      throw EFatal(E.get(), Details);
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
struct TLibS3XmlCallbackData : TLibS3CallbackData
{
  RawByteString Contents;
};
//---------------------------------------------------------------------------
const UnicodeString AssumeRoleVersion(TraceInitStr(L"2011-06-15"));
const UnicodeString AssumeRoleNamespace(TraceInitStr(FORMAT(L"https://sts.amazonaws.com/doc/%s/", (AssumeRoleVersion))));
//---------------------------------------------------------------------------
static _di_IXMLNode NeedNode(const _di_IXMLNodeList & NodeList, const UnicodeString & Name, const UnicodeString & Namespace)
{
  _di_IXMLNode Result = NodeList->FindNode(Name, Namespace);
  if (Result == NULL)
  {
    throw Exception(FMTLOAD(S3_RESPONSE_ERROR, (Name)));
  }
  return Result;
}
//---------------------------------------------------------------------------
static _di_IXMLNode AssumeRoleNeedNode(const _di_IXMLNodeList & NodeList, const UnicodeString & Name)
{
  return NeedNode(NodeList, Name, AssumeRoleNamespace);
}
//---------------------------------------------------------------------------
static const _di_IXMLDocument CreateDocumentFromXML(const TLibS3XmlCallbackData & Data, TParseOptions ParseOptions)
{
  const _di_IXMLDocument Result = interface_cast<Xmlintf::IXMLDocument>(new TXMLDocument(NULL));
  DebugAssert(Result->ParseOptions == TParseOptions());
  Result->ParseOptions = ParseOptions;
  Result->LoadFromXML(UTFToString(Data.Contents));
  return Result;
}
//---------------------------------------------------------------------------
void TS3FileSystem::AssumeRole(const UnicodeString & RoleArn)
{
  // According to AWS cli does, AWS_ROLE_SESSION_NAME does not apply here
  UnicodeString RoleSessionName = DefaultStr(FTerminal->SessionData->S3RoleSessionName, AppNameString());

  try
  {
    TLibS3XmlCallbackData Data;
    RequestInit(Data);

    UnicodeString QueryParams =
      FORMAT(L"Version=%s&Action=AssumeRole&RoleSessionName=%s&RoleArn=%s", (
        AssumeRoleVersion, EncodeUrlString(RoleSessionName), EncodeUrlString(RoleArn)));

    UTF8String AuthRegionBuf = UTF8String(FAuthRegion);
    UTF8String QueryParamsBuf = UTF8String(QueryParams);
    UTF8String StsService = L"sts";
    AnsiString StsHostName =
      AnsiString(ReplaceStr(S3LibDefaultHostName(), FORMAT(L"%s.", (UnicodeString(S3_SERVICE))), FORMAT(L"%s.", (UnicodeString(StsService)))));
    DebugAssert(StsHostName != S3LibDefaultHostName());

    RequestParams AssumeRoleRequestParams =
    {
      HttpRequestTypeGET,
      {
        StsHostName.c_str(),
        NULL,
        S3ProtocolHTTPS,
        S3UriStylePath, // Otherwise lib3s prefixes "(null)." (because of NULL bucketName)
        FAccessKeyId.c_str(),
        FSecretAccessKey.c_str(),
        FSecurityToken,
        AuthRegionBuf.c_str(),
        StsService.c_str(),
      },
      NULL,
      QueryParamsBuf.c_str(),
      NULL, NULL, NULL, NULL, 0, 0, NULL, NULL, NULL, 0,
      LibS3XmlDataCallback,
      LibS3ResponseCompleteCallback,
      &Data,
      FTimeout
    };

    request_perform(&AssumeRoleRequestParams, FRequestContext);

    CheckLibS3Error(Data);

    const _di_IXMLDocument Document = CreateDocumentFromXML(Data, TParseOptions());
    _di_IXMLNode ResponseNode = AssumeRoleNeedNode(Document->ChildNodes, L"AssumeRoleResponse");
    _di_IXMLNode ResultNode = AssumeRoleNeedNode(ResponseNode->ChildNodes, L"AssumeRoleResult");
    _di_IXMLNode CredentialsNode = AssumeRoleNeedNode(ResultNode->ChildNodes, L"Credentials");
    UnicodeString AccessKeyId = AssumeRoleNeedNode(CredentialsNode->ChildNodes, L"AccessKeyId")->Text;
    UnicodeString SecretAccessKey = AssumeRoleNeedNode(CredentialsNode->ChildNodes, L"SecretAccessKey")->Text;
    UnicodeString SessionToken = AssumeRoleNeedNode(CredentialsNode->ChildNodes, L"SessionToken")->Text;
    UnicodeString ExpirationStr = AssumeRoleNeedNode(CredentialsNode->ChildNodes, L"Expiration")->Text;

    FTerminal->LogEvent(FORMAT(L"Assumed role \"%s\".", (RoleArn)));
    FTerminal->LogEvent(FORMAT(L"New acess key is: %s", (AccessKeyId)));
    if (Configuration->LogSensitive)
    {
      FTerminal->LogEvent(FORMAT(L"Secret access key: %s", (SecretAccessKey)));
      FTerminal->LogEvent(FORMAT(L"Session token: %s", (SessionToken)));
    }

    // Only logged for now
    TDateTime Expiration = ParseExpiration(ExpirationStr);
    FTerminal->LogEvent(FORMAT(L"Credentials expiration: %s", (StandardTimestamp(Expiration))));

    SetCredentials(AccessKeyId, SecretAccessKey, SessionToken);
  }
  catch (Exception & E)
  {
    throw ExtException(MainInstructions(FMTLOAD(S3_ASSUME_ROLE_ERROR, (RoleArn))), &E);
  }
}
//---------------------------------------------------------------------------
S3Status TS3FileSystem::LibS3XmlDataCallback(int BufferSize, const char * Buffer, void * CallbackData)
{
  TLibS3XmlCallbackData & Data = *static_cast<TLibS3XmlCallbackData *>(CallbackData);
  if (Data.Contents.Length() + BufferSize > 1024 * 1024)
  {
    throw Exception(L"Too much data");
  }
  Data.Contents += RawByteString(Buffer, BufferSize);
  return S3StatusOK;
}
//---------------------------------------------------------------------------
int TS3FileSystem::LibS3XmlDataToCallback(int BufferSize, char * Buffer, void * CallbackData)
{
  TLibS3XmlCallbackData & Data = *static_cast<TLibS3XmlCallbackData *>(CallbackData);
  int Len = std::min(Data.Contents.Length(), BufferSize);
  memcpy(Buffer, Data.Contents.c_str(), Len);
  Data.Contents.Delete(1, Len);
  return Len;
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
  bool Any;
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

    S3UriStyle UriStyle = S3UriStyle(FTerminal->SessionData->S3UrlStyle);

    I = FHostNames.find(BucketName);
    UnicodeString HostName;
    if (I != FHostNames.end())
    {
      HostName = I->second;
      if (SameText(HostName.SubString(1, BucketName.Length() + 1), BucketName + L"."))
      {
        HostName.Delete(1, BucketName.Length() + 1);
        // Even when using path-style URL Amazon seems to redirect us to bucket hostname and
        // we need to switch to virtual host style URL (without bucket name in the path)
        UriStyle = S3UriStyleVirtualHost;
      }
    }
    else
    {
      HostName = UnicodeString(FHostName);
    }

    Result.HostNameBuf = UTF8String(HostName + UnicodeString(FPortSuffix));
    Result.hostName = Result.HostNameBuf.c_str();
    Result.BucketNameBuf = UTF8String(BucketName);
    Result.bucketName = Result.BucketNameBuf.c_str();
    Result.protocol = FLibS3Protocol;
    Result.uriStyle = UriStyle;
    Result.accessKeyId = FAccessKeyId.c_str();
    Result.secretAccessKey = FSecretAccessKey.c_str();
    Result.securityToken = FSecurityToken;
    Result.AuthRegionBuf = UTF8String(Region);
    if (Result.AuthRegionBuf.Length() > S3_MAX_REGION_LENGTH)
    {
      Result.AuthRegionBuf.SetLength(S3_MAX_REGION_LENGTH);
    }
    Result.authRegion = Result.AuthRegionBuf.c_str();
    Result.service = NULL;

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
        if (HostName != Endpoint)
        {
          FTerminal->LogEvent(FORMAT("Will use endpoint \"%s\" for bucket \"%s\" from now on.", (Endpoint, BucketName)));
          FHostNames.insert(std::make_pair(BucketName, Endpoint));
          Retry = true;
        }
      }
      // Minio
      else if (Data.Status == S3StatusOK)
      {
        FTerminal->LogEvent(FORMAT("Will keep using region \"%s\" for bucket \"%s\" from now on.", (FAuthRegion, BucketName)));
        FRegions.insert(std::make_pair(BucketName, FAuthRegion));
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
  if (IsAmazonS3SessionData(FTerminal->SessionData))
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsS3Amazon");
  }
  else
  {
    FTerminal->Configuration->Usage->Inc(L"OpenedSessionsS3Other");
  }
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
    case fcSkipTransfer:
    case fcParallelTransfers:
    case fcLoadingAdditionalProperties:
    case fcAclChangingFiles:
    case fcMoveOverExistingFile:
    case fcTags:
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
    case fcTransferOut:
    case fcTransferIn:
    case fcParallelFileTransfers:
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
  // Capabilities of S3 protocol are fixed
  FTerminal->SaveCapabilities(FFileSystemInfo);
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
  ReadDirectoryInternal(Directory, FileList.get(), -1, UnicodeString());
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
    TTerminal * Terminal = Data.FileSystem->FTerminal;
    File->Terminal = Terminal;
    File->FileName = StrFromS3(BucketName);
    File->Type = FILETYPE_DIRECTORY;
    File->Owner = Data.FileSystem->MakeRemoteToken(OwnerId, OwnerDisplayName);
    File->ModificationFmt = mfNone;
    if (Terminal->IsValidFile(File.get()))
    {
      Data.FileList->AddFile(File.release());
    }
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
  TTerminal * Terminal = Data.FileSystem->FTerminal;

  for (int Index = 0; Index < ContentsCount; Index++)
  {
    Data.Any = true;
    const S3ListBucketContent * Content = &Contents[Index];
    UnicodeString FileName = UnixExtractFileName(StrFromS3(Content->key));
    if (!FileName.IsEmpty())
    {
      std::unique_ptr<TRemoteFile> File(new TRemoteFile(NULL));
      File->Terminal = Terminal;
      File->FileName = FileName;
      File->Type = FILETYPE_DEFAULT;

      #define ISO8601_FORMAT "%04d-%02d-%02dT%02d:%02d:%02d"
      int Year = 0;
      int Month = 0;
      int Day = 0;
      int Hour = 0;
      int Min = 0;
      int Sec = 0;
      // The libs3's parseIso8601Time uses mktime, so returns a local time, which we would have to complicatedly restore,
      // Doing own parting instead as it's easier.
      // Might be replaced with ISO8601ToDate.
      // Keep is sync with WebDAV.
      int Filled =
        sscanf(Content->lastModifiedStr, ISO8601_FORMAT, &Year, &Month, &Day, &Hour, &Min, &Sec);
      if (Filled == 6)
      {
        TDateTime Modification =
          EncodeDateVerbose((unsigned short)Year, (unsigned short)Month, (unsigned short)Day) +
          EncodeTimeVerbose((unsigned short)Hour, (unsigned short)Min, (unsigned short)Sec, 0);
        File->Modification = ConvertTimestampFromUTC(Modification);
        File->ModificationFmt = mfFull;
      }
      else
      {
        File->ModificationFmt = mfNone;
      }

      File->Size = Content->size;
      File->Owner = Data.FileSystem->MakeRemoteToken(Content->ownerId, Content->ownerDisplayName);
      if (Terminal->IsValidFile(File.get()))
      {
        Data.FileList->AddFile(File.release());
      }
    }
  }

  for (int Index = 0; Index < CommonPrefixesCount; Index++)
  {
    Data.Any = true;
    UnicodeString CommonPrefix = StrFromS3(CommonPrefixes[Index]);
    UnicodeString FileName = UnixExtractFileName(UnixExcludeTrailingBackslash(CommonPrefix));
    // Have seen prefixes like "/" or "path/subpath//"
    if (!FileName.IsEmpty())
    {
      std::unique_ptr<TRemoteFile> File(new TRemoteFile(NULL));
      File->Terminal = Data.FileSystem->FTerminal;
      File->FileName = FileName;
      File->Type = FILETYPE_DIRECTORY;
      File->ModificationFmt = mfNone;
      if (Terminal->IsValidFile(File.get()))
      {
        Data.FileList->AddFile(File.release());
      }
    }
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
bool TS3FileSystem::IsGoogleCloud()
{
  return SameText(S3GoogleCloudHostName, FTerminal->SessionData->HostNameExpanded);
}
//---------------------------------------------------------------------------
void TS3FileSystem::ReadDirectoryInternal(
  const UnicodeString & APath, TRemoteFileList * FileList, int MaxKeys, const UnicodeString & FileName)
{
  UnicodeString Path = UnixExcludeTrailingBackslash(AbsolutePath(APath, false));
  int AMaxKeys = (MaxKeys == -1) ? 1 : MaxKeys;
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

      if ((FTerminal->SessionData->S3MaxKeys == asOff) ||
          ((FTerminal->SessionData->S3MaxKeys == asAuto) && IsGoogleCloud()))
      {
        if (AMaxKeys != 0)
        {
          FTerminal->LogEvent(1, L"Not limiting keys.");
          AMaxKeys = 0;
        }
      }

      S3_list_service(
        FLibS3Protocol, FAccessKeyId.c_str(), FSecretAccessKey.c_str(), FSecurityToken, (FHostName + FPortSuffix).c_str(),
        StrToS3(FAuthRegion), AMaxKeys, FRequestContext, FTimeout, &ListServiceHandler, &Data);

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
    Data.Any = false;

    bool Continue;

    do
    {
      DoListBucket(Prefix, FileList, AMaxKeys, BucketContext, Data);
      CheckLibS3Error(Data);

      Continue = false;

      if (Data.IsTruncated)
      {
        // We have report that with max-keys=1, server can return IsTruncated response with no keys,
        // so we would loop infinitely. For now, if we do GET request only to check for bucket/folder existence (MaxKeys == -1),
        // we are happy with a successful response and never loop, even if IsTruncated.
        if ((MaxKeys == 0) ||
            ((MaxKeys > 0) && (Data.KeyCount < MaxKeys)))
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

    // Listing bucket root directory will report an error if the bucket does not exist.
    // But there won't be any prefix/ entry, so if the bucket is empty, the Data.Any is false.
    // But when listing a prefix, we do not get any error, when the "prefix" does not exist.
    // But when the prefix does exist, there's at least the prefix/ entry. If there's none, it means that the path does not exist.
    // Even an empty-named entry/subprefix (which are ignored for other purposes) still indicate that the prefix exists.
    if (Prefix.IsEmpty() || Data.Any)
    {
      FileList->AddFile(new TRemoteParentDirectory(FTerminal));
    }
    else
    {
      // When called from DoReadFile (FileName is set), leaving error handling to the caller.
      if (FileName.IsEmpty())
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
    CheckLibS3Error(Data);
  }
  else
  {
    S3_delete_object(&BucketContext, StrToS3(Key), FRequestContext, FTimeout, &ResponseHandler, &Data);
    try
    {
      CheckLibS3Error(Data);
    }
    catch (...)
    {
      if (FTerminal->Active && Dir && !FTerminal->FileExists(AFileName))
      {
        // Amazon silently ignores attempts to delete non existing folders,
        // But Google Cloud fails that.
        FTerminal->LogEvent(L"Folder does not exist anymore, it was probably only virtual");
      }
      else
      {
        throw;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::RenameFile(
  const UnicodeString & FileName, const TRemoteFile * File, const UnicodeString & NewName, bool Overwrite)
{
  if (DebugAlwaysTrue(File != NULL) && File->IsDirectory)
  {
    NotSupported();
  }
  CopyFile(FileName, File, NewName, Overwrite);
  TRmSessionAction DummyAction(FTerminal->ActionLog, FileName);
  DeleteFile(FileName, File, dfForceDelete, DummyAction);
  DummyAction.Cancel();
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CopyFile(
  const UnicodeString & AFileName, const TRemoteFile * File, const UnicodeString & ANewName, bool DebugUsedArg(Overwrite))
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
        FLibS3Protocol, FAccessKeyId.c_str(), FSecretAccessKey.c_str(), FSecurityToken,
        (FHostName + FPortSuffix).c_str(), StrToS3(BucketName),
        StrToS3(FAuthRegion), S3CannedAclPrivate, Region, FRequestContext, FTimeout, &ResponseHandler, &Data);

      HandleNonBucketStatus(Data, Retry);
    }
    while (Retry);

    CheckLibS3Error(Data);
  }
  else
  {
    Key = GetFolderKey(Key);

    TLibS3BucketContext BucketContext = GetBucketContext(BucketName, Key);

    S3PutObjectHandler PutObjectHandler = { CreateResponseHandler(), NULL };

    TLibS3CallbackData Data;
    RequestInit(Data);

    S3_put_object(&BucketContext, StrToS3(Key), 0, NULL, FRequestContext, FTimeout, &PutObjectHandler, &Data);

    CheckLibS3Error(Data);
  }
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CreateLink(const UnicodeString FileName,
  const UnicodeString PointTo, bool /*Symbolic*/)
{
  DebugUsedParam2(FileName, PointTo);
  DebugFail();
}
//---------------------------------------------------------------------------
struct TS3FileProperties
{
  char OwnerId[S3_MAX_GRANTEE_USER_ID_SIZE];
  char OwnerDisplayName[S3_MAX_GRANTEE_DISPLAY_NAME_SIZE];
  int AclGrantCount;
  S3AclGrant AclGrants[S3_MAX_ACL_GRANT_COUNT];
  UnicodeString Tags;
};
//---------------------------------------------------------------------------
static TRights::TRightLevel S3PermissionToRightLevel(S3Permission Permission)
{
  TRights::TRightLevel Result;
  switch (Permission)
  {
    case S3PermissionRead: Result = TRights::rlS3Read; break;
    case S3PermissionWrite: Result = TRights::rlS3Write; break;
    case S3PermissionReadACP: Result = TRights::rlS3ReadACP; break;
    case S3PermissionWriteACP: Result = TRights::rlS3WriteACP; break;
    default: DebugFail(); Result = TRights::rlNone; break;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool TS3FileSystem::ParsePathForPropertiesRequests(
  const UnicodeString & Path, const TRemoteFile * File, UnicodeString & BucketName, UnicodeString & Key)
{
  UnicodeString FileName = AbsolutePath(Path, false);

  ParsePath(FileName, BucketName, Key);

  bool Result = !Key.IsEmpty();
  if (Result && File->IsDirectory)
  {
    Key = GetFolderKey(Key);
  }
  return Result;
}
//---------------------------------------------------------------------------
const UnicodeString S3Version(TraceInitStr(L"2006-03-01"));
const UnicodeString S3Namespace(TraceInitStr(FORMAT(L"http://s3.amazonaws.com/doc/%s/", (S3Version))));
//---------------------------------------------------------------------------
static _di_IXMLNode S3NeedNode(const _di_IXMLNodeList & NodeList, const UnicodeString & Name)
{
  return NeedNode(NodeList, Name, S3Namespace);
}
//---------------------------------------------------------------------------
#define COPY_BUCKET_CONTEXT(BucketContext) \
  { BucketContext.hostName, BucketContext.bucketName, BucketContext.protocol, BucketContext.uriStyle, \
    BucketContext.accessKeyId, BucketContext.secretAccessKey, BucketContext.securityToken, BucketContext.authRegion, \
    BucketContext.service }
//---------------------------------------------------------------------------
bool TS3FileSystem::DoLoadFileProperties(
  const UnicodeString & AFileName, const TRemoteFile * File, TS3FileProperties & Properties, bool LoadTags)
{
  UnicodeString BucketName, Key;
  bool Result = ParsePathForPropertiesRequests(AFileName, File, BucketName, Key);
  if (Result)
  {
    TLibS3BucketContext BucketContext = GetBucketContext(BucketName, Key);

    S3ResponseHandler ResponseHandler = CreateResponseHandler();

    TLibS3CallbackData AclData;
    RequestInit(AclData);

    S3_get_acl(
      &BucketContext, StrToS3(Key), Properties.OwnerId, Properties.OwnerDisplayName,
      &Properties.AclGrantCount, Properties.AclGrants,
      FRequestContext, FTimeout, &ResponseHandler, &AclData);

    CheckLibS3Error(AclData);

    if (LoadTags)
    {
      TLibS3XmlCallbackData TagsData;
      RequestInit(TagsData);

      UTF8String KeyBuf = UTF8String(Key);
      RequestParams TaggingRequestParams =
      {
        HttpRequestTypeGET,
        COPY_BUCKET_CONTEXT(BucketContext),
        KeyBuf.c_str(),
        NULL,
        "tagging",
        NULL, NULL, NULL, 0, 0, NULL,
        LibS3ResponsePropertiesCallback,
        NULL, 0,
        LibS3XmlDataCallback,
        LibS3ResponseCompleteCallback,
        &TagsData,
        FTimeout
      };

      request_perform(&TaggingRequestParams, FRequestContext);

      if (TagsData.Status != S3StatusErrorAccessDenied)
      {
        CheckLibS3Error(TagsData);

        const _di_IXMLDocument Document = CreateDocumentFromXML(TagsData, TParseOptions() << poPreserveWhiteSpace);
        _di_IXMLNode TaggingNode = S3NeedNode(Document->ChildNodes, L"Tagging");
        _di_IXMLNode TagSetNode = S3NeedNode(TaggingNode->ChildNodes, L"TagSet");
        _di_IXMLNodeList TagNodeList = TagSetNode->GetChildNodes();
        std::unique_ptr<TStrings> Tags(new TStringList());
        for (int Index = 0; Index < TagNodeList->Count; Index++)
        {
          _di_IXMLNode TagNode = TagNodeList->Get(Index);
          UnicodeString Key = S3NeedNode(TagNode->ChildNodes, L"Key")->Text;
          Tags->Add(Key);
          UnicodeString Value = S3NeedNode(TagNode->ChildNodes, L"Value")->Text;
          Tags->Add(Value);
        }

        Properties.Tags = Tags->Text;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
typedef std::vector<S3AclGrant> TAclGrantsVector;
static void AddAclGrant(
  TRights::TRightGroup Group, unsigned short & Permissions, TAclGrantsVector & AclGrants,
  const S3AclGrant & AclGrantTemplate, S3Permission Permission)
{
  TRights::TRightLevel Level = S3PermissionToRightLevel(Permission);
  TRights::TFlag Flag = TRights::CalculateFlag(Group, Level);
  if (FLAGSET(Permissions, Flag))
  {
    S3AclGrant AclGrant(AclGrantTemplate);
    AclGrant.permission = Permission;
    AclGrants.push_back(AclGrant);
    Permissions -= static_cast<short int>(Flag);
  }
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::ChangeFileProperties(const UnicodeString FileName,
  const TRemoteFile * File, const TRemoteProperties * Properties,
  TChmodSessionAction & /*Action*/)
{
  UnicodeString BucketName, Key;
  if (DebugAlwaysTrue(ParsePathForPropertiesRequests(FileName, File, BucketName, Key)))
  {
    TValidProperties ValidProperties = Properties->Valid;

    TLibS3BucketContext BucketContext = GetBucketContext(BucketName, Key);

    if (ValidProperties.Contains(vpRights))
    {
      ValidProperties >> vpRights;

      DebugAssert(!Properties->AddXToDirectories);

      TS3FileProperties FileProperties;
      if (DebugAlwaysTrue(!File->IsDirectory) &&
          DebugAlwaysTrue(DoLoadFileProperties(FileName, File, FileProperties, false)))
      {
        TAclGrantsVector NewAclGrants;

        unsigned short Permissions = File->Rights->Combine(Properties->Rights);
        for (int GroupI = TRights::rgFirst; GroupI <= TRights::rgLast; GroupI++)
        {
          TRights::TRightGroup Group = static_cast<TRights::TRightGroup>(GroupI);
          S3AclGrant NewAclGrant;
          memset(&NewAclGrant, 0, sizeof(NewAclGrant));
          if (Group == TRights::rgUser)
          {
            NewAclGrant.granteeType = S3GranteeTypeCanonicalUser;
            DebugAssert(sizeof(NewAclGrant.grantee.canonicalUser.id) == sizeof(FileProperties.OwnerId));
            strcpy(NewAclGrant.grantee.canonicalUser.id, FileProperties.OwnerId);
          }
          else if (Group == TRights::rgS3AllAwsUsers)
          {
            NewAclGrant.granteeType = S3GranteeTypeAllAwsUsers;
          }
          else if (DebugAlwaysTrue(Group == TRights::rgS3AllUsers))
          {
            NewAclGrant.granteeType = S3GranteeTypeAllUsers;
          }
          unsigned short AllGroupPermissions =
            TRights::CalculatePermissions(Group, TRights::rlS3Read, TRights::rlS3ReadACP, TRights::rlS3WriteACP);
          if (FLAGSET(Permissions, AllGroupPermissions))
          {
            NewAclGrant.permission = S3PermissionFullControl;
            NewAclGrants.push_back(NewAclGrant);
            Permissions -= AllGroupPermissions;
          }
          else
          {
            #define ADD_ACL_GRANT(PERM) AddAclGrant(Group, Permissions, NewAclGrants, NewAclGrant, PERM)
            ADD_ACL_GRANT(S3PermissionRead);
            ADD_ACL_GRANT(S3PermissionWrite);
            ADD_ACL_GRANT(S3PermissionReadACP);
            ADD_ACL_GRANT(S3PermissionWriteACP);
          }
        }

        DebugAssert(Permissions == 0);

        // Preserve unrecognized permissions
        for (int Index = 0; Index < FileProperties.AclGrantCount; Index++)
        {
          S3AclGrant & AclGrant = FileProperties.AclGrants[Index];
          unsigned short Permission = AclGrantToPermissions(AclGrant, FileProperties);
          if (Permission == 0)
          {
            NewAclGrants.push_back(AclGrant);
          }
        }

        S3ResponseHandler ResponseHandler = CreateResponseHandler();

        TLibS3CallbackData Data;
        RequestInit(Data);

        S3_set_acl(
          &BucketContext, StrToS3(Key), FileProperties.OwnerId, FileProperties.OwnerDisplayName,
          NewAclGrants.size(), &NewAclGrants[0],
          FRequestContext, FTimeout, &ResponseHandler, &Data);

        CheckLibS3Error(Data);
      }
    }

    if (ValidProperties.Contains(vpTags))
    {
      ValidProperties >> vpTags;

      UnicodeString NewLine = L"\n";
      UnicodeString Indent = L"  ";
      UnicodeString Xml =
        XmlDeclaration + NewLine +
        L"<Tagging>" + NewLine +
        Indent + L"<TagSet>" + NewLine;

      std::unique_ptr<TStrings> Tags(TextToStringList(Properties->Tags));
      for (int Index = 0; Index < Tags->Count; Index += 2)
      {
        UnicodeString Key = Tags->Strings[Index];
        UnicodeString Value = Tags->Strings[Index + 1];
        Xml += Indent + Indent + FORMAT(L"<Tag><Key>%s</Key><Value>%s</Value></Tag>", (XmlEscape(Key), XmlEscape(Value))) + NewLine;
      }

      Xml +=
        Indent + L"</TagSet>" + NewLine +
        "</Tagging>" + NewLine;

      FTerminal->Log->Add(llOutput, Xml);

      TLibS3XmlCallbackData Data;
      RequestInit(Data);

      Data.Contents = StrToS3(Xml);

      UTF8String KeyBuf = UTF8String(Key);
      RequestParams TaggingRequestParams =
      {
        HttpRequestTypePUT,
        COPY_BUCKET_CONTEXT(BucketContext),
        KeyBuf.c_str(),
        NULL,
        "tagging",
        NULL, NULL, NULL, 0, 0, NULL,
        LibS3ResponsePropertiesCallback,
        LibS3XmlDataToCallback,
        Data.Contents.Length(),
        NULL,
        LibS3ResponseCompleteCallback,
        &Data,
        FTimeout
      };

      request_perform(&TaggingRequestParams, FRequestContext);

      CheckLibS3Error(Data);
    }

    DebugAssert(ValidProperties.Empty());
  }
}
//---------------------------------------------------------------------------
unsigned short TS3FileSystem::AclGrantToPermissions(S3AclGrant & AclGrant, const TS3FileProperties & Properties)
{
  TRights::TRightGroup RightGroup = static_cast<TRights::TRightGroup>(-1);
  if (AclGrant.granteeType == S3GranteeTypeCanonicalUser)
  {
    if (strcmp(Properties.OwnerId, AclGrant.grantee.canonicalUser.id) == 0)
    {
      RightGroup = TRights::rgUser;
    }
    else
    {
      FTerminal->LogEvent(1, FORMAT(L"Unsupported permission for canonical user %s", (StrFromS3(Properties.OwnerId))));
    }
  }
  else if (AclGrant.granteeType == S3GranteeTypeAllAwsUsers)
  {
    RightGroup = TRights::rgS3AllAwsUsers;
  }
  else if (AclGrant.granteeType == S3GranteeTypeAllUsers)
  {
    RightGroup = TRights::rgS3AllUsers;
  }
  unsigned short Result;
  if (RightGroup < 0)
  {
    Result = 0;
  }
  else
  {
    if (AclGrant.permission == S3PermissionFullControl)
    {
      Result = TRights::CalculatePermissions(RightGroup, TRights::rlS3Read, TRights::rlS3ReadACP, TRights::rlS3WriteACP);
    }
    else
    {
      DebugAssert(AclGrant.permission != S3PermissionWrite);
      TRights::TRightLevel RightLevel = S3PermissionToRightLevel(AclGrant.permission);
      if (RightLevel == TRights::rlNone)
      {
        Result = 0;
      }
      else
      {
        Result = TRights::CalculateFlag(RightGroup, RightLevel);
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
struct TLoadFilePropertiesData
{
  bool Result;
  bool LoadTags;
};
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::LoadFileProperties(const UnicodeString AFileName, const TRemoteFile * AFile, void * Param)
{
  TRemoteFile * File = const_cast<TRemoteFile *>(AFile);
  TLoadFilePropertiesData & Data = *static_cast<TLoadFilePropertiesData *>(Param);
  TS3FileProperties Properties;
  Data.Result = DoLoadFileProperties(AFileName, File, Properties, Data.LoadTags);
  if (Data.Result)
  {
    bool AdditionalRights = false;
    unsigned short Permissions = 0;
    for (int Index = 0; Index < Properties.AclGrantCount; Index++)
    {
      S3AclGrant & AclGrant = Properties.AclGrants[Index];
      unsigned short Permission = AclGrantToPermissions(AclGrant, Properties);
      if (Permission == 0)
      {
        AdditionalRights = true;
      }
      else
      {
        Permissions |= Permission;
      }
    }

    UnicodeString Delimiter(L",");
    UnicodeString HumanRights;
    for (int GroupI = TRights::rgFirst; GroupI <= TRights::rgLast; GroupI++)
    {
      TRights::TRightGroup Group = static_cast<TRights::TRightGroup>(GroupI);
      #define RIGHT_LEVEL_SET(LEVEL) FLAGSET(Permissions, TRights::CalculateFlag(Group, TRights::LEVEL))
      bool ReadRight = RIGHT_LEVEL_SET(rlS3Read);
      bool WriteRight = DebugAlwaysFalse(RIGHT_LEVEL_SET(rlS3Write));
      bool ReadACPRight = RIGHT_LEVEL_SET(rlS3ReadACP);
      bool WriteACPRight = RIGHT_LEVEL_SET(rlS3WriteACP);
      UnicodeString Desc;
      if (ReadRight && ReadACPRight && WriteACPRight)
      {
        Desc = L"F";
      }
      else if (ReadRight)
      {
        Desc = L"R";
        if (ReadACPRight || WriteACPRight || WriteRight)
        {
          Desc += L"+";
        }
      }

      if (!Desc.IsEmpty())
      {
        UnicodeString GroupDesc;
        switch (Group)
        {
          case TRights::rgUser: GroupDesc = L"O"; break;
          case TRights::rgS3AllAwsUsers: GroupDesc = L"U"; break;
          case TRights::rgS3AllUsers: GroupDesc = L"E"; break;
          default: DebugFail(); break;
        }

        if (!GroupDesc.IsEmpty())
        {
          Desc = GroupDesc + L":" + Desc;
          AddToList(HumanRights, Desc, Delimiter);
        }
      }
    }

    if (AdditionalRights)
    {
      AddToList(HumanRights, L"+", Delimiter);
    }

    File->Rights->Number = Permissions;
    File->Rights->SetTextOverride(HumanRights);

    if (Data.LoadTags)
    {
      File->Tags = Properties.Tags;
    }

    Data.Result = true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TS3FileSystem::LoadFilesProperties(TStrings * FileList)
{
  TLoadFilePropertiesData Data;
  Data.Result = false;
  Data.LoadTags = (FileList->Count == 1);
  FTerminal->BeginTransaction();
  try
  {
    FTerminal->ProcessFiles(FileList, foGetProperties, LoadFileProperties, &Data);
  }
  __finally
  {
    FTerminal->EndTransaction();
  }
  return Data.Result;
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CalculateFilesChecksum(
  const UnicodeString & DebugUsedArg(Alg), TStrings * DebugUsedArg(FileList), TCalculatedChecksumEvent,
  TFileOperationProgressType *, bool DebugUsedArg(FirstLevel))
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::CustomCommandOnFile(const UnicodeString FileName,
  const TRemoteFile * /*File*/, UnicodeString Command, int /*Params*/, TCaptureOutputEvent /*OutputEvent*/)
{
  DebugUsedParam2(FileName, Command);
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TS3FileSystem::AnyCommand(const UnicodeString Command,
  TCaptureOutputEvent /*OutputEvent*/)
{
  DebugUsedParam(Command);
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
  DebugUsedParam(Path);
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
  std::unique_ptr<Exception> Exception;
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

      OperationProgress->ThrottleToCPSLimit(Result);
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

  int Parts = std::min(S3MaxMultiPartChunks, std::max(1, static_cast<int>((Handle.Size + S3MinMultiPartChunkSize - 1) / S3MinMultiPartChunkSize)));
  int ChunkSize = std::max(S3MinMultiPartChunkSize, static_cast<int>((Handle.Size + Parts - 1) / Parts));
  DebugAssert((ChunkSize == S3MinMultiPartChunkSize) || (Handle.Size > static_cast<__int64>(S3MaxMultiPartChunks) * S3MinMultiPartChunkSize));

  bool Multipart = (Parts > 1);

  RawByteString MultipartUploadId;
  TLibS3MultipartCommitPutObjectDataCallbackData MultipartCommitPutObjectDataCallbackData;

  if (Multipart)
  {
    FTerminal->LogEvent(FORMAT(L"Initiating multipart upload (%d parts - chunk size %s)", (Parts, IntToStr(ChunkSize))));

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
          int PartLength = std::min(ChunkSize, RemainingInt);
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
  catch (Exception &)
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

      OperationProgress->ThrottleToCPSLimit(BufferSize);
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
        FTerminal->UpdateTargetTime(
          LocalHandle, File->Modification, File->ModificationFmt, FTerminal->SessionData->DSTMode);
      }
    }
    __finally
    {
      CloseHandle(LocalHandle);

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
