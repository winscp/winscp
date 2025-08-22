//---------------------------------------------------------------------------
#include "FileZillaPCH.h"
//---------------------------------------------------------------------------
#include "FileZillaIntf.h"
#include "FileZillaIntern.h"
//---------------------------------------------------------------------------
#pragma comment(lib, "uafxcw.lib")
//---------------------------------------------------------------------------
void __fastcall TFileZillaIntf::Initialize()
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TFileZillaIntf::Finalize()
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TFileZillaIntf::SetResourceModule(void * ResourceHandle)
{
  afxCurrentResourceHandle = (HINSTANCE)ResourceHandle;
}
//---------------------------------------------------------------------------
__fastcall TFileZillaIntf::TFileZillaIntf() :
  FFileZillaApi(NULL),
  FIntern(new TFileZillaIntern(this)),
  FServer(new t_server)
{
}
//---------------------------------------------------------------------------
__fastcall TFileZillaIntf::~TFileZillaIntf()
{
  DebugAssert(FFileZillaApi == NULL);

  delete FIntern;
  FIntern = NULL;
  delete FServer;
  FServer = NULL;
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::Init()
{
  DebugAssert(FFileZillaApi == NULL);

  FFileZillaApi = new CFileZillaApi();

  bool Result = Check(FFileZillaApi->Init(FIntern, this), L"init");

  if (!Result)
  {
    delete FFileZillaApi;
    FFileZillaApi = NULL;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileZillaIntf::Destroying()
{
  // need to close FZAPI before calling destructor as it in turn post messages
  // back while being destroyed, what may result in calling virtual methods
  // of already destroyed descendants
  delete FFileZillaApi;
  FFileZillaApi = NULL;
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::SetCurrentPath(const wchar_t * APath)
{
  DebugAssert(FFileZillaApi != NULL);
  CServerPath Path(APath, false);
  return Check(FFileZillaApi->SetCurrentPath(Path), L"setcurrentpath");
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::GetCurrentPath(wchar_t * Path, size_t MaxLen)
{
  CServerPath APath;
  bool Result = Check(FFileZillaApi->GetCurrentPath(APath), L"getcurrentpath");
  if (Result)
  {
    wcsncpy(Path, APath.GetPath(), MaxLen);
    Path[MaxLen - 1] = L'\0';
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::Cancel()
{
  DebugAssert(FFileZillaApi != NULL);
  // tolerate even "idle" state, quite possible in MT environment
  return Check(FFileZillaApi->Cancel(), L"cancel", FZ_REPLY_WOULDBLOCK | FZ_REPLY_IDLE);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::Connect(const wchar_t * Host, int Port, const wchar_t * User,
  const wchar_t * Pass, const wchar_t * Account,
  const wchar_t * Path, int ServerType, int Pasv, int TimeZoneOffset, int UTF8,
  int iForcePasvIp, int iUseMlsd,
  X509 * Certificate, EVP_PKEY * PrivateKey)
{
  DebugAssert(FFileZillaApi != NULL);
  DebugAssert((ServerType & FZ_SERVERTYPE_HIGHMASK) == FZ_SERVERTYPE_FTP);

  t_server Server;

  Server.host = Host;
  Server.port = Port;
  Server.user = User;
  Server.pass = Pass;
  Server.account = Account;
  Server.path = Path;
  Server.nServerType = ServerType;
  Server.nPasv = Pasv;
  Server.nTimeZoneOffset = TimeZoneOffset;
  Server.nUTF8 = UTF8;
  Server.iForcePasvIp = iForcePasvIp;
  Server.iUseMlsd = iUseMlsd;
  Server.Certificate = Certificate;
  Server.PrivateKey = PrivateKey;

  *FServer = Server;

  return Check(FFileZillaApi->Connect(Server), L"connect");
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::Close(bool AllowBusy)
{
  bool Result;
  int ReturnCode = FFileZillaApi->Disconnect();

  switch (ReturnCode)
  {
    // If the connection terminated itself meanwhile,
    // do not try to wait for close response.
    case FZ_REPLY_NOTCONNECTED:
      // We might check AllowBusy here, as it's actually similar scenario,
      // as we expect this to happen during authentication only
      Result = false;
      break;

    // waiting for disconnect
    case FZ_REPLY_WOULDBLOCK:
      Result = true;
      break;

    // allowing busy while opening, not sure if it safe,
    // but we need it, when cancelling password prompt
    case FZ_REPLY_BUSY:
      if (AllowBusy)
      {
        Result = false;
        break;
      }

    case FZ_REPLY_NOTINITIALIZED:
    default:
      Result = Check(ReturnCode, L"disconnect");
      break;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::CustomCommand(const wchar_t * Command)
{
  DebugAssert(FFileZillaApi != NULL);
  return Check(FFileZillaApi->CustomCommand(Command), L"customcommand");
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::MakeDir(const wchar_t* APath)
{
  DebugAssert(FFileZillaApi != NULL);
  CServerPath Path(APath, false);
  return Check(FFileZillaApi->MakeDir(Path), L"makedir");
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::Chmod(int Value, const wchar_t* FileName,
  const wchar_t* APath)
{
  DebugAssert(FFileZillaApi != NULL);
  CServerPath Path(APath, false);
  return Check(FFileZillaApi->Chmod(Value, FileName, Path), L"chmod");
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::Delete(const wchar_t* FileName, const wchar_t* APath, bool FileNameOnly)
{
  DebugAssert(FFileZillaApi != NULL);
  CServerPath Path(APath, false);
  return Check(FFileZillaApi->Delete(FileName, Path, FileNameOnly), L"delete");
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::RemoveDir(const wchar_t* FileName, const wchar_t* APath)
{
  DebugAssert(FFileZillaApi != NULL);
  CServerPath Path(APath, false);
  return Check(FFileZillaApi->RemoveDir(FileName, Path), L"removedir");
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::Rename(const wchar_t* OldName,
  const wchar_t* NewName, const wchar_t* APath, const wchar_t* ANewPath)
{
  DebugAssert(FFileZillaApi != NULL);
  CServerPath Path(APath, false);
  CServerPath NewPath(ANewPath, false);
  return Check(FFileZillaApi->Rename(OldName, NewName, Path, NewPath), L"rename");
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::List(const wchar_t * APath)
{
  DebugAssert(FFileZillaApi != NULL);
  CServerPath Path(APath, false);
  return Check(FFileZillaApi->List(Path), L"list");
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::ListFile(const wchar_t * FileName, const wchar_t * APath)
{
  DebugAssert(FFileZillaApi != NULL);
  CServerPath Path(APath, false);
  return Check(FFileZillaApi->ListFile(FileName, Path), L"listfile");
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::FileTransfer(
  const wchar_t * LocalFile, const wchar_t * RemoteFile,
  const wchar_t * RemotePath, bool Get, __int64 Size, int Type, void * UserData,
  TTransferOutEvent OnTransferOut, TTransferInEvent OnTransferIn)
{
  t_transferfile Transfer;

  Transfer.localfile = LocalFile;
  Transfer.remotefile = RemoteFile;
  Transfer.remotepath = CServerPath(RemotePath, false);
  Transfer.get = Get;
  Transfer.size = Size;
  Transfer.server = *FServer;
  // 1 = ascii, 2 = binary
  Transfer.nType = Type;
  Transfer.nUserData = reinterpret_cast<int>(UserData);
  Transfer.OnTransferOut = OnTransferOut;
  Transfer.OnTransferIn = OnTransferIn;

  return Check(FFileZillaApi->FileTransfer(Transfer), L"filetransfer");
}
//---------------------------------------------------------------------------
void __fastcall TFileZillaIntf::SetDebugLevel(TLogLevel Level)
{
  FIntern->SetDebugLevel(Level - LOG_APIERROR + 1);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::PostMessage(WPARAM wParam, LPARAM lParam)
{
  unsigned int MessageID = FZ_MSG_ID(wParam);
  TMessageType Type;
  switch (MessageID)
  {
    case FZ_MSG_TRANSFERSTATUS:
      Type = MSG_TRANSFERSTATUS;
      break;

    default:
      Type = MSG_OTHER;
      break;
  }
  return DoPostMessage(Type, wParam, lParam);
}
//---------------------------------------------------------------------------
void __fastcall CopyContact(TFtpsCertificateData::TContact & Dest,
  const t_SslCertData::t_Contact& Source)
{
  Dest.Organization = Source.Organization;
  Dest.Unit = Source.Unit;
  Dest.CommonName = Source.CommonName;
  Dest.Mail = Source.Mail;
  Dest.Country = Source.Country;
  Dest.StateProvince = Source.StateProvince;
  Dest.Town = Source.Town;
  Dest.Other = Source.Other;
}
//---------------------------------------------------------------------------
void __fastcall CopyValidityTime(TFtpsCertificateData::TValidityTime & Dest,
  const t_SslCertData::t_validTime& Source)
{
  Dest.Year = Source.y;
  Dest.Month = Source.M;
  Dest.Day = Source.d;
  Dest.Hour = Source.h;
  Dest.Min = Source.m;
  Dest.Sec = Source.s;
}
//---------------------------------------------------------------------------
void __fastcall CopyFileTime(TRemoteFileTime & Dest, const t_directory::t_direntry::t_date & Source)
{
  Dest.Year = Source.year;
  Dest.Month = Source.month;
  Dest.Day = Source.day;
  Dest.Hour = Source.hour;
  Dest.Minute = Source.minute;
  Dest.Second = Source.second;
  Dest.HasTime = Source.hastime;
  Dest.HasDate = Source.hasdate;
  Dest.HasYear = Source.hasyear;
  Dest.HasSeconds = Source.hasseconds;
  Dest.Utc = Source.utc;
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::HandleMessage(WPARAM wParam, LPARAM lParam)
{
  bool Result;

  CString a;
  unsigned int MessageID = FZ_MSG_ID(wParam);

  switch (MessageID)
  {
    case FZ_MSG_STATUS:
      {
        DebugAssert(FZ_MSG_PARAM(wParam) == 0);
        t_ffam_statusmessage * Status = (t_ffam_statusmessage *)lParam;
        DebugAssert(Status->post);
        Result = HandleStatus(Status->status, Status->type);
        delete Status;
      }

      break;

    case FZ_MSG_ASYNCREQUEST:
      if (FZ_MSG_PARAM(wParam) == FZ_ASYNCREQUEST_OVERWRITE)
      {
        int RequestResult;
        wchar_t FileName1[MAX_PATH];
        COverwriteRequestData * Data = (COverwriteRequestData *)lParam;
        try
        {
          DebugAssert(Data != NULL);
          wcsncpy(FileName1, Data->FileName1, LENOF(FileName1));
          FileName1[LENOF(FileName1) - 1] = L'\0';
          TRemoteFileTime RemoteTime;
          CopyFileTime(RemoteTime, Data->remotetime);
          Result = HandleAsynchRequestOverwrite(
            FileName1, LENOF(FileName1), Data->FileName2, Data->path1, Data->path2,
            Data->size1, Data->size2,
            (Data->localtime != NULL) ? Data->localtime->GetTime() : 0,
            (Data->localtime != NULL) && ((Data->localtime->GetHour() != 0) || (Data->localtime->GetMinute() != 0)),
            RemoteTime,
            reinterpret_cast<void*>(Data->pTransferFile->nUserData), RequestResult);
        }
        catch(...)
        {
          FFileZillaApi->SetAsyncRequestResult(FILEEXISTS_SKIP, Data);
          throw;
        }

        if (Result)
        {
          Data->FileName1 = FileName1;
          Result = Check(FFileZillaApi->SetAsyncRequestResult(RequestResult, Data),
            L"setasyncrequestresult");
        }
      }
      else if (FZ_MSG_PARAM(wParam) == FZ_ASYNCREQUEST_VERIFYCERT)
      {
        int RequestResult;
        CVerifyCertRequestData * AData = (CVerifyCertRequestData *)lParam;
        try
        {
          DebugAssert(AData != NULL);
          TFtpsCertificateData Data;
          CopyContact(Data.Subject, AData->pCertData->subject);
          CopyContact(Data.Issuer, AData->pCertData->issuer);
          CopyValidityTime(Data.ValidFrom, AData->pCertData->validFrom);
          CopyValidityTime(Data.ValidUntil, AData->pCertData->validUntil);
          Data.SubjectAltName = AData->pCertData->subjectAltName;
          Data.HashSha1 = AData->pCertData->hashSha1;
          DebugAssert(Data.HashSha1Len == sizeof(AData->pCertData->hashSha1));
          Data.HashSha256 = AData->pCertData->hashSha256;
          DebugAssert(Data.HashSha256Len == sizeof(AData->pCertData->hashSha256));
          Data.Certificate = AData->pCertData->certificate;
          Data.CertificateLen = AData->pCertData->certificateLen;
          Data.VerificationResult = AData->pCertData->verificationResult;
          Data.VerificationDepth = AData->pCertData->verificationDepth;

          Result = HandleAsynchRequestVerifyCertificate(Data, RequestResult);
        }
        catch(...)
        {
          FFileZillaApi->SetAsyncRequestResult(0, AData);
          throw;
        }

        if (Result)
        {
          Result = Check(FFileZillaApi->SetAsyncRequestResult(RequestResult, AData),
            L"setasyncrequestresult");
        }
      }
      else if (FZ_MSG_PARAM(wParam) == FZ_ASYNCREQUEST_NEEDPASS)
      {
        int RequestResult = 0;
        CNeedPassRequestData * AData = (CNeedPassRequestData *)lParam;
        try
        {
            TNeedPassRequestData Data;
            Data.Password = NULL;
            Result = HandleAsynchRequestNeedPass(Data, RequestResult);
            if (Result && (RequestResult == TFileZillaIntf::REPLY_OK))
            {
              AData->Password = Data.Password;
              free(Data.Password);
            }
        }
        catch(...)
        {
          FFileZillaApi->SetAsyncRequestResult(0, AData);
          throw;
        }
        if (Result)
        {
          Result = Check(FFileZillaApi->SetAsyncRequestResult(RequestResult, AData),
            L"setasyncrequestresult");
        }
      }
      else
      {
        // FZ_ASYNCREQUEST_GSS_AUTHFAILED
        // FZ_ASYNCREQUEST_GSS_NEEDUSER
        // FZ_ASYNCREQUEST_GSS_NEEDPASS
        DebugFail();
        Result = false;
      }
      break;

    case FZ_MSG_LISTDATA:
      {
        DebugAssert(FZ_MSG_PARAM(wParam) == 0);
        t_directory * Directory = (t_directory *)lParam;
        CString Path = Directory->path.GetPath();
        std::vector<TListDataEntry> Entries(Directory->num);

        for (int Index = 0; Index < Directory->num; Index++)
        {
          t_directory::t_direntry & Source = Directory->direntry[Index];
          TListDataEntry & Dest = Entries[Index];

          Dest.Name = Source.name;
          Dest.Permissions = Source.permissionstr;
          Dest.HumanPerm = Source.humanpermstr;
          Dest.OwnerGroup = Source.ownergroup;
          Dest.Owner = Source.owner;
          Dest.Group = Source.group;
          Dest.Size = Source.size;
          Dest.Dir = Source.dir;
          Dest.Link = Source.bLink;
          CopyFileTime(Dest.Time, Source.date);
          Dest.LinkTarget = Source.linkTarget;
        }

        int Num = Directory->num;
        TListDataEntry * pEntries = Num > 0 ? &Entries[0] : NULL;
        Result = HandleListData(Path, pEntries, Num);

        delete Directory;
      }
      break;

    case FZ_MSG_TRANSFERSTATUS:
      {
        DebugAssert(FZ_MSG_PARAM(wParam) == 0);
        t_ffam_transferstatus * Status = (t_ffam_transferstatus *)lParam;
        if (Status != NULL)
        {
          Result = HandleTransferStatus(
            true, Status->transfersize, Status->bytes, Status->bFileTransfer);
          delete Status;
        }
        else
        {
          Result = HandleTransferStatus(false, -1, -1, false);
        }
      }
      break;

    case FZ_MSG_REPLY:
      Result = HandleReply(FZ_MSG_PARAM(wParam), lParam);
      break;

    case FZ_MSG_CAPABILITIES:
      Result = HandleCapabilities((TFTPServerCapabilities *)lParam);
      break;

    default:
      DebugFail();
      Result = false;
      break;
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::CheckError(int /*ReturnCode*/, const wchar_t * /*Context*/)
{
  return false;
}
//---------------------------------------------------------------------------
inline bool __fastcall TFileZillaIntf::Check(int ReturnCode,
  const wchar_t * Context, int Expected)
{
  if ((ReturnCode & (Expected == -1 ? FZ_REPLY_OK : Expected)) == ReturnCode)
  {
    return true;
  }
  else
  {
    return CheckError(ReturnCode, Context);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::UsingMlsd()
{
  return FFileZillaApi->UsingMlsd();
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaIntf::UsingUtf8()
{
  return FFileZillaApi->UsingUtf8();
}
//---------------------------------------------------------------------------
std::string __fastcall TFileZillaIntf::GetTlsVersionStr()
{
  return FFileZillaApi->GetTlsVersionStr();
}
//---------------------------------------------------------------------------
std::string __fastcall TFileZillaIntf::GetCipherName()
{
  return FFileZillaApi->GetCipherName();
}
