//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SftpFileSystem.h"

#include <sftp.h>

#include "PuttyIntf.h"
#include "Common.h"
#include "Interface.h"
#include "Terminal.h"
#include "TextsCore.h"

#include <memory>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
// additional constants for SFTP protocol not defined in Putty's sftp.h
#define SSH_FXP_READLINK           19
#define SSH_FXP_SYMLINK            20

#define SSH_FILEXFER_ATTR_ACCESSTIME        0x00000008
#define SSH_FILEXFER_ATTR_CREATETIME        0x00000010
#define SSH_FILEXFER_ATTR_MODIFYTIME        0x00000020
#define SSH_FILEXFER_ATTR_OWNERGROUP        0x00000080
#define SSH_FILEXFER_ATTR_SUBSECOND_TIMES   0x00000100

#define SSH_FILEXFER_TYPE_REGULAR          1
#define SSH_FILEXFER_TYPE_DIRECTORY        2
#define SSH_FILEXFER_TYPE_SYMLINK          3
#define SSH_FILEXFER_TYPE_SPECIAL          4
#define SSH_FILEXFER_TYPE_UNKNOWN          5

#define SSH_FXF_TEXT            0x00000040

#define SFTP_MAX_PACKET_LEN   102400
//---------------------------------------------------------------------------
const int SFTPMinVersion = 0;
const int SFTPMaxVersion = 4;
const int SFTPNoMessageNumber = -1;

const int asOK =            1 << SSH_FX_OK;
const int asEOF =           1 << SSH_FX_EOF;
const int asOpUnsupported = 1 << SSH_FX_OP_UNSUPPORTED;
const int asAll = 0xFFFF;
//---------------------------------------------------------------------------
#define GET_32BIT(cp) \
    (((unsigned long)(unsigned char)(cp)[0] << 24) | \
    ((unsigned long)(unsigned char)(cp)[1] << 16) | \
    ((unsigned long)(unsigned char)(cp)[2] << 8) | \
    ((unsigned long)(unsigned char)(cp)[3]))

#define PUT_32BIT(cp, value) { \
    (cp)[0] = (unsigned char)((value) >> 24); \
    (cp)[1] = (unsigned char)((value) >> 16); \
    (cp)[2] = (unsigned char)((value) >> 8); \
    (cp)[3] = (unsigned char)(value); }

#define THROW_SKIP_FILE_NULL THROW_SKIP_FILE(NULL, "")
//---------------------------------------------------------------------------
#define SFTP_PACKET_ALLOC_DELTA 256
//---------------------------------------------------------------------------
class TSFTPPacket
{
public:
  TSFTPPacket()
  {
    Init();
  }

  TSFTPPacket(const TSFTPPacket & Source)
  {
    Init();
    *this = Source;
  }

  TSFTPPacket(unsigned char AType)
  {
    Init();
    ChangeType(AType);
  }

  ~TSFTPPacket()
  {
    delete FData;
    if (FReservedBy) FReservedBy->UnreserveResponse(this);
  }

  void ChangeType(unsigned char AType)
  {
    FPosition = 0;
    FLength = 0;
    Capacity = 0;
    FType = AType;
    AddByte(FType);
    if (FType != SSH_FXP_INIT)
    {
      FMessageNumber = (FMessageCounter << 8) + FType;
      FMessageCounter++;
      AddCardinal(FMessageNumber);
    }
  }

  void AddByte(unsigned char Value)
  {
    Add(&Value, sizeof(Value));
  }

  void AddCardinal(unsigned long Value)
  {
    unsigned char Buf[4];
    PUT_32BIT(Buf, Value);
    Add(&Buf, sizeof(Buf));
  }

  void AddInt64(__int64 Value)
  {
    AddCardinal((unsigned long)(Value >> 32));
    AddCardinal((unsigned long)(Value & 0xFFFFFFFF));
  }

  void AddData(const void * Data, int ALength)
  {
    AddCardinal(ALength);
    Add(Data, ALength);
  }

  void AddString(const AnsiString Value)
  {
    AddCardinal(Value.Length());
    Add(Value.c_str(), Value.Length());
  }

  void AddProperties(const TRemoteProperties * Properties,
    unsigned short BaseRights, bool IsDirectory, int Version)
  {
    int Flags = 0;
    if (Properties)
    {
      if (Properties->Valid.Contains(vpOwner) || Properties->Valid.Contains(vpGroup))
      {
        Flags |= SSH_FILEXFER_ATTR_OWNERGROUP;
      }
      if (Properties->Valid.Contains(vpRights))
      {
        Flags |= SSH_FILEXFER_ATTR_PERMISSIONS;
      }
    }
    AddCardinal(Flags);
    if (Version >= 4)
    {
      AddByte((unsigned char)(IsDirectory ? SSH_FILEXFER_TYPE_DIRECTORY : SSH_FILEXFER_TYPE_REGULAR));
    }
    if (Properties)
    {
      if (Flags & SSH_FILEXFER_ATTR_OWNERGROUP)
      {
        assert(Version >= 4);
        AddString(Properties->Valid.Contains(vpOwner) ?
          Properties->Owner : AnsiString());
        AddString(Properties->Valid.Contains(vpGroup) ?
          Properties->Group : AnsiString());
      }
      if (Flags & SSH_FILEXFER_ATTR_PERMISSIONS)
      {
        TRights Rights = BaseRights;
        Rights |= Properties->Rights.NumberSet;
        Rights &= (unsigned short)~Properties->Rights.NumberUnset;
        if (IsDirectory && Properties->AddXToDirectories)
        {
          Rights.AddExecute();
        }
        AddCardinal(Rights);
      }
    }
  }

  char GetByte()
  {
    assert(FPosition <= FLength - sizeof(char));
    char Result = FData[FPosition];
    FPosition++;
    return Result;
  }

  unsigned long GetCardinal()
  {
    unsigned long Result;
    assert(FPosition <= FLength - sizeof(Result));
    Result = GET_32BIT(FData + FPosition);
    FPosition += sizeof(Result);
    return Result;
  }

  __int64 GetInt64()
  {
    __int64 Hi = GetCardinal();
    __int64 Lo = GetCardinal();
    return (Hi << 32) + Lo;
  }

  AnsiString GetString()
  {
    AnsiString Result;
    unsigned long Len = GetCardinal();
    Result.SetLength(Len);
    assert(FLength >= Len && FPosition <= FLength - Len);
    memcpy(Result.c_str(), FData + FPosition, Len);
    FPosition += Len;
    return Result;
  }

  void GetFile(TRemoteFile * File, int Version)
  {
    assert(File);
    unsigned int Flags;
    AnsiString ListingStr;
    unsigned long Permissions;
    bool ParsingFailed = false;
    if (Type != SSH_FXP_ATTRS)
    {
      File->FileName = GetString();
      if (Version < 4)
      {
        ListingStr = GetString();
      }
    }
    Flags = GetCardinal();
    if (Version >= 4)
    {
      char FXType = GetByte();
      char Types[] = "-DLSU";
      if (FXType < 1 || FXType > (char)strlen(Types))
      {
        throw Exception(FMTLOAD(SFTP_UNKNOWN_FILE_TYPE, (int(FXType))));
      }
      File->Type = Types[FXType-1];
    }
    if (Flags & SSH_FILEXFER_ATTR_SIZE)
    {
      File->Size = GetInt64();
    }
    if (Flags & SSH_FILEXFER_ATTR_UIDGID)
    {
      assert(Version < 4);
      GetCardinal(); // skip UID
      GetCardinal(); // skip GUID
    }
    if (Flags & SSH_FILEXFER_ATTR_OWNERGROUP)
    {
      assert(Version >= 4);
      File->Owner = GetString();
      File->Group = GetString();
    }
    if (Flags & SSH_FILEXFER_ATTR_PERMISSIONS)
    {
      Permissions = GetCardinal();
    }
    if (Version < 4)
    {
      if (Flags & SSH_FILEXFER_ATTR_ACMODTIME)
      {
        File->LastAccess = UnixToDateTime(GetCardinal());
        File->Modification = UnixToDateTime(GetCardinal());
      }
    }
    else
    {
      if (Flags & SSH_FILEXFER_ATTR_ACCESSTIME)
      {
        File->LastAccess = UnixToDateTime((unsigned long)GetInt64());
      }
      if (Flags & SSH_FILEXFER_ATTR_SUBSECOND_TIMES)
      {
        GetCardinal(); // skip access time subseconds
      }
      if (Flags & SSH_FILEXFER_ATTR_CREATETIME)
      {
        GetInt64(); // skip create time
      }
      if (Flags & SSH_FILEXFER_ATTR_SUBSECOND_TIMES)
      {
        GetCardinal(); // skip create time subseconds
      }
      if (Flags & SSH_FILEXFER_ATTR_MODIFYTIME)
      {
        File->Modification = UnixToDateTime((unsigned long)GetInt64());
      }
      if (Flags & SSH_FILEXFER_ATTR_SUBSECOND_TIMES)
      {
        GetCardinal(); // skip modification time subseconds
      }
    }

    if ((Version < 4) && (Type != SSH_FXP_ATTRS))
    {
      try
      {
        // update permissions and user/group name
        // modification time and filename is ignored
        File->ListingStr = ListingStr;
      }
      catch(...)
      {
        // ignore any error while parsing listing line,
        // SFTP specification do not recommend to parse it
        ParsingFailed = true;
      }
    }

    if (Type == SSH_FXP_ATTRS || Version >= 4 || ParsingFailed)
    {
      File->Rights->Number = (unsigned short)(Permissions & 0777);
      if (Version < 4)
      {
        File->Type = (Permissions & 0040000 ? FILETYPE_DIRECTORY : '-');
      }
    }

    // TODO: read extended attributes (Flags & SSH_FILEXFER_ATTR_EXTENDED)
    // Format: Count=Cardinal, Count*(Name=String, Value=String)
  }

  void DataUpdated(int ALength)
  {
    FPosition = 0;
    FLength = ALength;
    FType = GetByte();
    if (FType != SSH_FXP_VERSION)
    {
      FMessageNumber = GetCardinal();
    }
    else
    {
      FMessageNumber = SFTPNoMessageNumber;
    }
  }

  TSFTPPacket & operator = (const TSFTPPacket & Source)
  {
    Capacity = 0;
    Add(Source.Data, Source.Length);
    DataUpdated(Source.Length);
    FPosition = Source.FPosition;
    return *this;
  }

  __property unsigned int Length = { read = FLength };
  __property char * Data = { read = FData };
  __property char * NextData = { read = GetNextData };
  __property char * Content = { read = GetContent };
  __property unsigned int ContentLength = { read = GetContentLength };
  __property unsigned int Capacity = { read = FCapacity, write = SetCapacity };
  __property unsigned char Type = { read = FType };
  __property unsigned char RequestType = { read = GetRequestType };
  __property unsigned int MessageNumber = { read = FMessageNumber, write = FMessageNumber };
  __property TSFTPFileSystem * ReservedBy = { read = FReservedBy, write = FReservedBy };
  __property AnsiString TypeName = { read = GetTypeName };

private:
  char * FData;
  unsigned int FLength;
  unsigned int FCapacity;
  unsigned int FPosition;
  unsigned char FType;
  unsigned int FMessageNumber;
  TSFTPFileSystem * FReservedBy;

  static int FMessageCounter;

  void Init()
  {
    FData = NULL;
    FCapacity = 0;
    FLength = 0;
    FPosition = 0;
    FMessageNumber = SFTPNoMessageNumber;
    FType = -1;
    FReservedBy = NULL;
  }

  unsigned char GetRequestType()
  {
    if (FMessageNumber != SFTPNoMessageNumber)
    {
      return (unsigned char)(FMessageNumber & 0xFF);
    }
    else
    {
      assert(Type == SSH_FXP_VERSION);
      return SSH_FXP_INIT;
    }
  }

  void Add(const void * AData, int ALength)
  {
    if (Length + ALength > Capacity)
    {
      Capacity = Length + ALength + SFTP_PACKET_ALLOC_DELTA;
    }
    memcpy(FData + Length, AData, ALength);
    FLength += ALength;
  }

  void SetCapacity(unsigned int ACapacity)
  {
    if (ACapacity != Capacity)
    {
      FCapacity = ACapacity;
      if (FCapacity > 0)
      {
        char * NData = new char[FCapacity];
        if (FData)
        {
          memcpy(NData, FData, (FLength < FCapacity ? FLength : FCapacity));
          delete FData;
        }
        FData = NData;
      }
      else
      {
        if (FData) delete FData;
        FData = NULL;
      }
      if (FLength > FCapacity) FLength = FCapacity;
    }
  }

  AnsiString GetTypeName() const
  {
    #define TYPE_CASE(TYPE) case TYPE: return #TYPE
    switch (Type) {
      TYPE_CASE(SSH_FXP_INIT);
      TYPE_CASE(SSH_FXP_VERSION);
      TYPE_CASE(SSH_FXP_OPEN);
      TYPE_CASE(SSH_FXP_CLOSE);
      TYPE_CASE(SSH_FXP_READ);
      TYPE_CASE(SSH_FXP_WRITE);
      TYPE_CASE(SSH_FXP_LSTAT);
      TYPE_CASE(SSH_FXP_FSTAT);
      TYPE_CASE(SSH_FXP_SETSTAT);
      TYPE_CASE(SSH_FXP_FSETSTAT);
      TYPE_CASE(SSH_FXP_OPENDIR);
      TYPE_CASE(SSH_FXP_READDIR);
      TYPE_CASE(SSH_FXP_REMOVE);
      TYPE_CASE(SSH_FXP_MKDIR);
      TYPE_CASE(SSH_FXP_RMDIR);
      TYPE_CASE(SSH_FXP_REALPATH);
      TYPE_CASE(SSH_FXP_STAT);
      TYPE_CASE(SSH_FXP_RENAME);
      TYPE_CASE(SSH_FXP_READLINK);
      TYPE_CASE(SSH_FXP_SYMLINK);
      TYPE_CASE(SSH_FXP_STATUS);
      TYPE_CASE(SSH_FXP_HANDLE);
      TYPE_CASE(SSH_FXP_DATA);
      TYPE_CASE(SSH_FXP_NAME);
      TYPE_CASE(SSH_FXP_ATTRS);
      TYPE_CASE(SSH_FXP_EXTENDED);
      TYPE_CASE(SSH_FXP_EXTENDED_REPLY);
      default:
        return FORMAT("Unknown message (%d)", (int(Type)));
    }
  }

  char * GetContent()
  {
    return Data + sizeof(Byte) + (FType != SSH_FXP_VERSION ? sizeof(Cardinal) : 0);
  }

  unsigned int GetContentLength()
  {
    return Length - sizeof(Byte) - (FType != SSH_FXP_VERSION ? sizeof(Cardinal) : 0);
  }

  char * GetNextData()
  {
    return FPosition < FLength ? FData + FPosition : NULL;
  }
};
//---------------------------------------------------------------------------
class TSFTPQueue
{
public:
  __fastcall TSFTPQueue(TSFTPFileSystem * AFileSystem)
  {
    FFileSystem = AFileSystem;
    assert(FFileSystem);
    FRequests = new TList();
    FResponses = new TList();
  }

  __fastcall ~TSFTPQueue()
  {
    try
    {
      if (FFileSystem->FTerminal->Active)
      {
        Dispose();
      }
    }
    __finally
    {
      TSFTPPacket * Request;
      TSFTPPacket * Response;

      assert(FResponses->Count == FRequests->Count);
      for (int Index = 0; Index < FRequests->Count; Index++)
      {
        Request = static_cast<TSFTPPacket*>(FRequests->Items[Index]);
        assert(Request);
        delete Request;

        Response = static_cast<TSFTPPacket*>(FResponses->Items[Index]);
        assert(Response);
        delete Response;
      }
      delete FRequests;
      delete FResponses;
    }
  }

  bool __fastcall Init(int QueueLen)
  {
    bool Result = false;
    while ((QueueLen > 0) && SendRequest())
    {
      Result = true;
      QueueLen--;
    }
    return Result;
  }

  void __fastcall Dispose()
  {
    assert(FFileSystem->FTerminal->Active);

    TSFTPPacket * Request;
    TSFTPPacket * Response;

    while (FRequests->Count)
    {
      assert(FResponses->Count);

      Request = static_cast<TSFTPPacket*>(FRequests->Items[0]);
      assert(Request);

      Response = static_cast<TSFTPPacket*>(FResponses->Items[0]);
      assert(Response);

      try
      {
        FFileSystem->ReceiveResponse(Request, Response);
      }
      catch(Exception & E)
      {
        if (FFileSystem->FTerminal->Active)
        {
          FFileSystem->FTerminal->LogEvent("Error while disposing the SFTP queue.");
          HandleExtendedException(&E);
        }
        else
        {
          FFileSystem->FTerminal->LogEvent("Fatal error while disposing the SFTP queue.");
          throw;
        }
      }

      FRequests->Delete(0);
      delete Request;
      FResponses->Delete(0);
      delete Response;
    }
  }

  bool __fastcall ReceivePacket(TSFTPPacket * Packet,
    int ExpectedType = -1, int AllowStatus = -1)
  {
    assert(FRequests->Count);
    bool Result;
    TSFTPPacket * Request = NULL;
    TSFTPPacket * Response = NULL;
    try
    {
      Request = static_cast<TSFTPPacket*>(FRequests->Items[0]);
      FRequests->Delete(0);
      assert(Request);

      Response = static_cast<TSFTPPacket*>(FResponses->Items[0]);
      FResponses->Delete(0);
      assert(Response);

      FFileSystem->ReceiveResponse(Request, Response,
        ExpectedType, AllowStatus);

      if (Packet)
      {
        *Packet = *Response;
      }

      Result = SendNext(Response);
      if (Result)
      {
        Result = SendRequest();
      }
    }
    __finally
    {
      delete Request;
      delete Response;
    }

    return Result;
  }

  bool __fastcall Next(int ExpectedType = -1, int AllowStatus = -1)
  {
    return ReceivePacket(NULL, ExpectedType, AllowStatus);
  }

protected:
  virtual bool __fastcall InitRequest(TSFTPPacket * Request) = 0;

  virtual bool __fastcall SendNext(TSFTPPacket * Request) = 0;

  virtual void __fastcall SendPacket(TSFTPPacket * Packet)
  {
    FFileSystem->SendPacket(Packet);
  }

protected:
  TList * FRequests;
  TList * FResponses;
  TSFTPFileSystem * FFileSystem;

private:
  bool __fastcall SendRequest()
  {
    TSFTPPacket * Request = NULL;
    try
    {
      Request = new TSFTPPacket();
      if (!InitRequest(Request))
      {
        delete Request;
        Request = NULL;
      }
    }
    catch(...)
    {
      delete Request;
      throw;
    }

    if (Request != NULL)
    {
      TSFTPPacket * Response = new TSFTPPacket();
      FRequests->Add(Request);
      FResponses->Add(Response);

      SendPacket(Request);
      FFileSystem->ReserveResponse(Request, Response);
    }

    return (Request != NULL);
  }
};
//---------------------------------------------------------------------------
class TSFTPTransferQueue : public TSFTPQueue
{
public:
  TSFTPTransferQueue(TSFTPFileSystem * AFileSystem) : TSFTPQueue(AFileSystem)
  {
  }

protected:
  __int64 FTransfered;
  AnsiString FHandle;
  unsigned long FBlockSize;
};
//---------------------------------------------------------------------------
class TSFTPDownloadQueue : public TSFTPTransferQueue
{
public:
  TSFTPDownloadQueue(TSFTPFileSystem * AFileSystem) :
    TSFTPTransferQueue(AFileSystem)
  {
  }

  bool __fastcall Init(int QueueLen, const AnsiString AHandle,
    unsigned long ABlockSize, __int64 ATransfered)
  {
    FHandle = AHandle;
    FBlockSize = ABlockSize;
    FTransfered = ATransfered;

    return TSFTPTransferQueue::Init(QueueLen);
  }

protected:
  virtual bool __fastcall InitRequest(TSFTPPacket * Request)
  {
    Request->ChangeType(SSH_FXP_READ);
    Request->AddString(FHandle);
    Request->AddInt64(FTransfered);
    Request->AddCardinal(FBlockSize);
    FTransfered += FBlockSize;
    return true;
  }

  virtual bool __fastcall SendNext(TSFTPPacket * Request)
  {
    return (Request->Type == SSH_FXP_DATA);
  }
};
//---------------------------------------------------------------------------
class TSFTPUploadQueue : public TSFTPTransferQueue
{
public:
  TSFTPUploadQueue(TSFTPFileSystem * AFileSystem) :
    TSFTPTransferQueue(AFileSystem)
  {
    FFile = NULL;
    OperationProgress = NULL;
    FLastBlockSize = 0;
  }

  bool __fastcall Init(int QueueLen, const AnsiString AFileName,
    HANDLE AFile, TFileOperationProgressType * AOperationProgress,
    const AnsiString AHandle, unsigned long ABlockSize, __int64 ATransfered)
  {
    FFileName = AFileName;
    FFile = AFile;
    OperationProgress = AOperationProgress;
    FHandle = AHandle;
    FBlockSize = ABlockSize;
    FTransfered = ATransfered;

    return TSFTPTransferQueue::Init(QueueLen);
  }

protected:
  virtual bool __fastcall InitRequest(TSFTPPacket * Request)
  {
    TTerminal * FTerminal = FFileSystem->FTerminal;
    // Buffer for one block of data
    TFileBuffer BlockBuf;

    FILE_OPERATION_LOOP(FFileName,
      FMTLOAD(READ_ERROR, (FFileName)),
      BlockBuf.LoadFile(FFile, OperationProgress->StaticBlockSize(), false);
    );

    bool Result = (BlockBuf.Size != 0);
    if (Result)
    {
      OperationProgress->AddLocalyUsed(BlockBuf.Size);

      // We do ASCII transfer: convert EOL of current block
      if (OperationProgress->AsciiTransfer)
      {
        __int64 PrevBufSize = BlockBuf.Size;
        BlockBuf.Convert(FTerminal->Configuration->LocalEOLType,
          FFileSystem->GetEOL(), cpRemoveCtrlZ);
        // update transfer size with difference arised from EOL conversion
        OperationProgress->SetTransferSize(OperationProgress->TransferSize -
          PrevBufSize + BlockBuf.Size);
      }

      Request->ChangeType(SSH_FXP_WRITE);
      Request->AddString(FHandle);
      Request->AddInt64(FTransfered);
      Request->AddData(BlockBuf.Data, BlockBuf.Size);
      FLastBlockSize = BlockBuf.Size;

      FTransfered += FBlockSize;
    }

    return Result;
  }

  virtual void __fastcall SendPacket(TSFTPPacket * Packet)
  {
    TSFTPTransferQueue::SendPacket(Packet);
    OperationProgress->AddTransfered(FLastBlockSize);
  }

  virtual bool __fastcall SendNext(TSFTPPacket * Request)
  {
    return true;
  }

private:
  HANDLE FFile;
  TFileOperationProgressType * OperationProgress;
  AnsiString FFileName;
  unsigned long FLastBlockSize;
};
//---------------------------------------------------------------------------
int TSFTPPacket::FMessageCounter = 0;
//===========================================================================
struct TOpenRemoteFileParams
{
  int LocalFileAttrs;
  AnsiString RemoteFileName;
  TFileOperationProgressType * OperationProgress;
  const TCopyParamType * CopyParam;
  int Params;
  bool Resume;
  TSFTPOverwriteMode OverwriteMode;
  __int64 DestFileSize; // output
  AnsiString RemoteFileHandle; // output
};
//---------------------------------------------------------------------------
struct TSinkFileParams
{
  AnsiString TargetDir;
  const TCopyParamType * CopyParam;
  int Params;
  TFileOperationProgressType * OperationProgress;
  bool Skipped;
};
//===========================================================================
__fastcall TSFTPFileSystem::TSFTPFileSystem(TTerminal * ATerminal):
  TCustomFileSystem(ATerminal)
{
  FPacketReservations = new TList();
  FPacketNumbers = VarArrayCreate(OPENARRAY(int, (0, 1)), varInteger);
  FPreviousLoggedPacket = 0;
  FNotLoggedPackets = 0;
  FBusy = 0;
  FAvoidBusy = false;
}
//---------------------------------------------------------------------------
__fastcall TSFTPFileSystem::~TSFTPFileSystem()
{
  delete FPacketReservations;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSFTPFileSystem::GetProtocolName() const
{
  return FMTLOAD(SFTP_PROTOCOL_NAME, (FVersion));
}
//---------------------------------------------------------------------------
bool __fastcall TSFTPFileSystem::IsCapable(int Capability) const
{
  assert(FTerminal);
  switch (Capability) {
    case fcUserGroupListing:
    case fcAnyCommand:
    case fcHardLink:
      return false;

    case fcOwnerChanging:
    case fcGroupChanging:
      return (FVersion >= 4);

    case fcSymbolicLink:
    case fcResolveSymlink:
      return (FVersion >= 3);

    case fcModeChanging:
      return true;

    case fcTextMode:
      return (FVersion >= 4) ||
        strcmp(GetEOL(), EOLToStr(FTerminal->Configuration->LocalEOLType)) != 0;

    case fcRename:
      return (FVersion >= 2);

    default:
      assert(false);
      return false;
  }
}
//---------------------------------------------------------------------------
struct TUnicodeEmitParams
{
  WideString Buffer;
  int Pos;
  int Len;
};
//---------------------------------------------------------------------------
extern "C" void UnicodeEmit(void * AParams, long int Output)
{
  if (Output == 0xFFFFL) // see Putty's charset\internal.h
  {
    throw Exception(LoadStr(DECODE_UTF_ERROR));
  }
  TUnicodeEmitParams * Params = (TUnicodeEmitParams *)AParams;
  if (Params->Pos >= Params->Len)
  {
    Params->Len += 50;
    Params->Buffer.SetLength(Params->Len);
  }
  Params->Buffer[Params->Pos + 1] = (wchar_t)Output;
  Params->Pos++;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSFTPFileSystem::DecodeUTF(const AnsiString UTF)
{
  charset_state State;
  char * Str;
  TUnicodeEmitParams Params;
  AnsiString Result;

  State.s0 = 0;
  Str = UTF.c_str();
  Params.Pos = 0;
  Params.Len = UTF.Length();
  Params.Buffer.SetLength(Params.Len);

  while (*Str)
  {
    read_utf8(NULL, (unsigned char)*Str, &State, UnicodeEmit, &Params);
    Str++;
  }
  Params.Buffer.SetLength(Params.Pos);

  return Params.Buffer;
}          
//---------------------------------------------------------------------------
inline void __fastcall TSFTPFileSystem::BusyStart()
{
  if (FBusy == 0 && FTerminal->UseBusyCursor && !FAvoidBusy)
  {
    Busy(true);
  }
  FBusy++;
  assert(FBusy < 10);
}
//---------------------------------------------------------------------------
inline void __fastcall TSFTPFileSystem::BusyEnd()
{
  assert(FBusy > 0);
  FBusy--;
  if (FBusy == 0 && FTerminal->UseBusyCursor && !FAvoidBusy)
  {
    Busy(false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SendPacket(const TSFTPPacket * Packet)
{
  BusyStart();
  try
  {
    if (FTerminal->IsLogging())
    {
      if ((FPreviousLoggedPacket != SSH_FXP_READ &&
           FPreviousLoggedPacket != SSH_FXP_WRITE) ||
          (Packet->Type != FPreviousLoggedPacket))
      {
        if (FNotLoggedPackets)
        {
          FTerminal->LogEvent(FORMAT("%d skipped SSH_FXP_WRITE, SSH_FXP_READ, SSH_FXP_DATA and SSH_FXP_STATUS packets.",
            (FNotLoggedPackets)));
          FNotLoggedPackets = 0;
        }
        FTerminal->Log->Add(llInput, FORMAT("Type: %s, Size: %d, Number: %d",
          (Packet->TypeName, (int)Packet->Length, (int)Packet->MessageNumber)));
        FPreviousLoggedPacket = Packet->Type;
      }
      else
      {
        FNotLoggedPackets++;
      }
    }
    char LenBuf[4];
    PUT_32BIT(LenBuf, Packet->Length);
    FTerminal->Send(LenBuf, sizeof(LenBuf));
    FTerminal->Send(Packet->Data, Packet->Length);
  }
  __finally
  {
    BusyEnd();
  }
}
//---------------------------------------------------------------------------
unsigned long __fastcall TSFTPFileSystem::GotStatusPacket(TSFTPPacket * Packet,
  int AllowStatus)
{
  unsigned long Code = Packet->GetCardinal();

  static int Messages[] = {
    SFTP_STATUS_OK,
    SFTP_STATUS_EOF,
    SFTP_STATUS_NO_SUCH_FILE,
    SFTP_STATUS_PERMISSION_DENIED,
    SFTP_STATUS_FAILURE,
    SFTP_STATUS_BAD_MESSAGE,
    SFTP_STATUS_NO_CONNECTION,
    SFTP_STATUS_CONNECTION_LOST,
    SFTP_STATUS_OP_UNSUPPORTED,
    SFTP_STATUS_INVALID_HANDLE,
    SFTP_STATUS_NO_SUCH_PATH,
    SFTP_STATUS_FILE_ALREADY_EXISTS,
    SFTP_STATUS_WRITE_PROTECT,
    SFTP_STATUS_NO_MEDIA,
  };
  int Message;
  if ((AllowStatus & (0x01 << Code)) == 0)
  {
    if (Code >= sizeof(Messages) / sizeof(*Messages))
    {
      Message = SFTP_STATUS_UNKNOWN;
    }
    else
    {
      Message = Messages[Code];
    }
    AnsiString ServerMessage;
    AnsiString LanguageTag;
    if (FVersion >= 3)
    {
      ServerMessage = DecodeUTF(Packet->GetString());
      LanguageTag = Packet->GetString();
    }
    else
    {
      ServerMessage = LoadStr(SFTP_SERVER_MESSAGE_UNSUPPORTED);
    }
    if (LanguageTag.IsEmpty())
    {
      LanguageTag = "?";
    }
    if (FTerminal->IsLogging())
    {
      FTerminal->Log->Add(llOutput, FORMAT("Status/error code: %d, Message: %d, Server: %s, Language: %s ",
        (int(Code), (int)Packet->MessageNumber, ServerMessage, LanguageTag)));
    }
    FTerminal->TerminalError(NULL, FMTLOAD(SFTP_ERROR_FORMAT,
      (LoadStr(Message), int(Code), ServerMessage, LanguageTag, int(Packet->RequestType))));
    return 0;
  }
  else
  {
    if (!FNotLoggedPackets || Code)
    {
      FTerminal->Log->Add(llOutput, FORMAT("Status/error code: %d", ((int)Code)));
    }
    return Code;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::RemoveReservation(int Reservation)
{
  for (int Index = Reservation+1; Index < FPacketReservations->Count; Index++)
  {
    FPacketNumbers.PutElement(FPacketNumbers.GetElement(Index), Index-1);
  }
  TSFTPPacket * Packet = (TSFTPPacket *)FPacketReservations->Items[Reservation];
  if (Packet)
  {
    assert(Packet->ReservedBy == this);
    Packet->ReservedBy = NULL;
  }
  FPacketReservations->Delete(Reservation);
}
//---------------------------------------------------------------------------
int __fastcall TSFTPFileSystem::ReceivePacket(TSFTPPacket * Packet,
  int ExpectedType, int AllowStatus)
{
  int Result = SSH_FX_OK;
  BusyStart();
  try
  {
    int Reservation = FPacketReservations->IndexOf(Packet);
    if (Reservation < 0 || Packet->Capacity == 0)
    {
      bool IsReserved;
      do
      {
        IsReserved = false;

        assert(Packet);
        char LenBuf[4];
        FTerminal->Receive(LenBuf, sizeof(LenBuf));
        int Length = GET_32BIT(LenBuf);
        if (Length > SFTP_MAX_PACKET_LEN)
        {
          FTerminal->FatalError(FMTLOAD(SFTP_PACKET_TOO_BIG, (
            Length, SFTP_MAX_PACKET_LEN)));
        }
        Packet->Capacity = Length;
        FTerminal->Receive(Packet->Data, Length);
        Packet->DataUpdated(Length);

        if (FTerminal->IsLogging())
        {
          if ((FPreviousLoggedPacket != SSH_FXP_READ &&
               FPreviousLoggedPacket != SSH_FXP_WRITE) ||
              (Packet->Type != SSH_FXP_STATUS && Packet->Type != SSH_FXP_DATA))
          {
            if (FNotLoggedPackets)
            {
              FTerminal->LogEvent(FORMAT("%d skipped SSH_FXP_WRITE, SSH_FXP_READ, SSH_FXP_DATA and SSH_FXP_STATUS packets.",
                (FNotLoggedPackets)));
              FNotLoggedPackets = 0;
            }
            FTerminal->Log->Add(llOutput, FORMAT("Type: %s, Size: %d, Number: %d",
              (Packet->TypeName, (int)Packet->Length, (int)Packet->MessageNumber)));
          }
          else
          {
            FNotLoggedPackets++;
          }
        }

        if (Reservation < 0 ||
            Packet->MessageNumber != (unsigned long)FPacketNumbers.GetElement(Reservation))
        {
          TSFTPPacket * ReservedPacket;
          unsigned long MessageNumber;
          for (int Index = 0; Index < FPacketReservations->Count; Index++)
          {
            MessageNumber = FPacketNumbers.GetElement(Index);
            if (MessageNumber == Packet->MessageNumber)
            {
              ReservedPacket = (TSFTPPacket *)FPacketReservations->Items[Index];
              IsReserved = true;
              if (ReservedPacket)
              {
                *ReservedPacket = *Packet;
              }
              else
              {
                RemoveReservation(Index);
              }
              break;
            }
          }
        }
      }
      while (IsReserved);
    }

    if (ExpectedType >= 0)
    {
      if (Packet->Type == SSH_FXP_STATUS)
      {
        Result = GotStatusPacket(Packet, (AllowStatus >= 0 ? AllowStatus : asOK));
      }
      else if (ExpectedType != Packet->Type)
      {
        FTerminal->FatalError(FMTLOAD(SFTP_INVALID_TYPE, ((int)Packet->Type)));
      }
    }

    if (Reservation >= 0)
    {
      // order might have changed, when reserved, but not longer needed packet
      // was receive in above loop
      Reservation = FPacketReservations->IndexOf(Packet);
      assert(Reservation >= 0);
      assert(Packet->MessageNumber == (unsigned long)FPacketNumbers.GetElement(Reservation));
      RemoveReservation(Reservation);
    }
  }
  __finally
  {
    BusyEnd();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ReserveResponse(const TSFTPPacket * Packet,
  TSFTPPacket * Response)
{
  assert(FPacketReservations->IndexOf(Response) < 0);
  // mark response as not received yet
  Response->Capacity = 0;
  Response->ReservedBy = this;
  FPacketReservations->Add(Response);
  if (FPacketReservations->Count >= FPacketNumbers.ArrayHighBound())
  {
    FPacketNumbers.ArrayRedim(FPacketReservations->Count + 10);
  }
  FPacketNumbers.PutElement(Packet->MessageNumber, FPacketReservations->Count - 1);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::UnreserveResponse(TSFTPPacket * Response)
{
  int Reservation = FPacketReservations->IndexOf(Response);
  if (Reservation >= 0)
  {
    FPacketReservations->Items[Reservation] = NULL;
  }
}
//---------------------------------------------------------------------------
int __fastcall TSFTPFileSystem::ReceiveResponse(
  const TSFTPPacket * Packet, TSFTPPacket * Response, int ExpectedType,
  int AllowStatus)
{
  int Result;
  unsigned int MessageNumber = Packet->MessageNumber;
  TSFTPPacket * AResponse = (Response ? Response : new TSFTPPacket());
  try
  {
    Result = ReceivePacket(AResponse, ExpectedType, AllowStatus);
    if (MessageNumber != AResponse->MessageNumber)
    {
      FTerminal->FatalError(FMTLOAD(SFTP_MESSAGE_NUMBER,
        ((int)AResponse->MessageNumber, (int)MessageNumber)));
    }
  }
  __finally
  {
    if (!Response) delete AResponse;
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TSFTPFileSystem::SendPacketAndReceiveResponse(
  const TSFTPPacket * Packet, TSFTPPacket * Response, int ExpectedType,
  int AllowStatus)
{
  int Result;
  BusyStart();
  try
  {
    SendPacket(Packet);
    Result = ReceiveResponse(Packet, Response, ExpectedType, AllowStatus);
  }
  __finally
  {
    BusyEnd();
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall inline TSFTPFileSystem::IsAbsolutePath(const AnsiString Path)
{
  return !Path.IsEmpty() && Path[1] == '/';
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSFTPFileSystem::RealPath(const AnsiString Path)
{
  try
  {
    FTerminal->LogEvent(FORMAT("Getting real path for '%s'",
      (Path)));

    TSFTPPacket Packet(SSH_FXP_REALPATH);
    Packet.AddString(Path);
    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_NAME);
    if (Packet.GetCardinal() != 1)
    {
      FTerminal->FatalError(LoadStr(SFTP_NON_ONE_FXP_NAME_PACKET));
    }

    AnsiString RealDir = Packet.GetString();
    // ignore rest of SSH_FXP_NAME packet

    FTerminal->LogEvent(FORMAT("Real path is '%s'", (RealDir)));

    return RealDir;
  }
  catch(Exception & E)
  {
    if (FTerminal->Active)
    {
      throw ExtException(&E, FMTLOAD(SFTP_REALPATH_ERROR, (Path)));
    }
    else
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSFTPFileSystem::RealPath(const AnsiString Path,
  const AnsiString BaseDir)
{
  AnsiString APath;

  if (IsAbsolutePath(Path))
  {
    APath = Path;
  }
  else
  {
    if (!BaseDir.IsEmpty())
    {
      APath = UnixIncludeTrailingBackslash(BaseDir);
    }
    if (!Path.IsEmpty())
    {
      APath = APath + Path;
    }
    if (APath.IsEmpty()) APath = UnixIncludeTrailingBackslash(".");
  }
  return RealPath(APath);
}
//---------------------------------------------------------------------------
AnsiString __fastcall inline TSFTPFileSystem::LocalCanonify(const AnsiString Path)
{
  // TODO: improve (handle .. etc.)
  if (IsAbsolutePath(Path)) return Path;
    else
  {
    return UnixIncludeTrailingBackslash(FCurrentDirectory) + Path;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall inline TSFTPFileSystem::Canonify(AnsiString Path)
{
  // inspired by canonify() from PSFTP.C
  AnsiString Result;
  FTerminal->LogEvent(FORMAT("Canonifying: \"%s\"", (Path)));
  Path = LocalCanonify(Path);
  try
  {
    Result = RealPath(Path);
  }
  catch(...)
  {
    if (FTerminal->Active)
    {
      AnsiString APath = UnixExcludeTrailingBackslash(Path);
      AnsiString Name = UnixExtractFileName(APath);
      if (Name == "." || Name == "..")
      {
        Result = Path;
      }
      else
      {
        AnsiString FPath = UnixExtractFilePath(APath);
        try
        {
          Result = RealPath(FPath);
          Result = UnixIncludeTrailingBackslash(Result) + Name;
        }
        catch(...)
        {
          if (FTerminal->Active)
          {
            Result = Path;
          }
          else
          {
            throw;
          }
        }
      }
    }
    else
    {
      throw;
    }
  }

  FTerminal->LogEvent(FORMAT("Canonified: \"%s\"", (Result)));

  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSFTPFileSystem::GetHomeDirectory()
{
  if (FHomeDirectory.IsEmpty())
  {
    FHomeDirectory = RealPath(".");
  }
  return FHomeDirectory;
}
//---------------------------------------------------------------------------
TRemoteFile * __fastcall TSFTPFileSystem::LoadFile(TSFTPPacket * Packet,
  TRemoteFile * ALinkedByFile)
{
  TRemoteFile * File = new TRemoteFile(ALinkedByFile);
  try
  {
    File->Terminal = FTerminal;
    Packet->GetFile(File, FVersion);
  }
  catch(...)
  {
    delete File;
    throw;
  }
  return File;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SetCurrentDirectory(AnsiString value)
{
  assert(false);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSFTPFileSystem::GetCurrentDirectory()
{
  return FCurrentDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::DoStartup()
{
  TSFTPPacket Packet(SSH_FXP_INIT);
  Packet.AddCardinal(SFTPMaxVersion);

  try
  {
    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_VERSION);
  }
  catch(Exception &E)
  {
    FTerminal->FatalError(&E, LoadStr(SFTP_INITIALIZE_ERROR));
  }

  FVersion = Packet.GetCardinal();
  FTerminal->LogEvent(FORMAT("SFTP version %d negotiated.", (FVersion)));
  if (FVersion < SFTPMinVersion || FVersion > SFTPMaxVersion)
  {
    FTerminal->FatalError(FMTLOAD(SFTP_VERSION_NOT_SUPPORTED,
      (FVersion, SFTPMinVersion, SFTPMaxVersion)));
  }

  if (FVersion >= 3)
  {
    FEOL = "\r\n";
    AnsiString ExtensionName;
    AnsiString ExtensionData;
    while (Packet.NextData)
    {
      ExtensionName = Packet.GetString();
      ExtensionData = Packet.GetString();
      if (ExtensionName == "newline")
      {
        FEOL = ExtensionData;
        AnsiString EOLHex;
        for (int i = 0; i < FEOL.Length(); i++)
        {
          EOLHex += IntToHex(FEOL[i+1], 2);
        }
        FTerminal->LogEvent(FORMAT("Server requests EOL sequence %s.", (EOLHex)));
        if (FEOL.Length() < 1 || FEOL.Length() > 2)
        {
          FTerminal->FatalError(FMTLOAD(SFTP_INVALID_EOL, (EOLHex)));
        }
      }
      else
      {
        FTerminal->LogEvent(FORMAT("Unknown server extension %s=%s",
          (ExtensionName, ExtensionData)));
      }
    }

    Packet.ChangeType(SSH_FXP_EXTENDED);
    Packet.AddString("winscp@winscp.sourceforge.net");
    int Status = SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS,
      asOK | asOpUnsupported);
    if (Status == SSH_FX_OK)
    {
      FTerminal->LogEvent("Server recognises WinSCP.");
    }
    else
    {
      FTerminal->LogEvent("Server does not recognise WinSCP.");
    }
  }
}
//---------------------------------------------------------------------------
char * __fastcall TSFTPFileSystem::GetEOL() const
{
  if (FVersion >= 4)
  {
    assert(!FEOL.IsEmpty());
    return FEOL.c_str();
  }
  else
  {
    return EOLToStr(FTerminal->SessionData->EOLType);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::LookupUserGroups()
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ReadCurrentDirectory()
{
  if (!FDirectoryToChangeTo.IsEmpty())
  {
    FCurrentDirectory = FDirectoryToChangeTo;
    FDirectoryToChangeTo = "";
  }
  else if (FCurrentDirectory.IsEmpty())
  {
    // this happens only after startup when default remote directory is not specified
    FCurrentDirectory = GetHomeDirectory();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::HomeDirectory()
{
  ChangeDirectory(GetHomeDirectory());
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::TryOpenDirectory(const AnsiString Directory)
{
  FTerminal->LogEvent(FORMAT("Trying to open directory \"%s\".", (Directory)));
  TSFTPPacket Packet(SSH_FXP_OPENDIR);
  Packet.AddString(Directory);
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_HANDLE);
  AnsiString Handle = Packet.GetString();
  Packet.ChangeType(SSH_FXP_CLOSE);
  Packet.AddString(Handle);
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS, asAll);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ChangeDirectory(const AnsiString Directory)
{
  AnsiString Path, Current;

  Current = !FDirectoryToChangeTo.IsEmpty() ? FDirectoryToChangeTo : FCurrentDirectory;
  Path = RealPath(Directory, Current);

  // to verify existence of directory try to open it (SSH_FXP_REALPATH succeeds
  // for invalid paths on some systems, like CygWin)
  TryOpenDirectory(Path);

  // if open dir did not fail, directory exists -> success.
  FDirectoryToChangeTo = Path;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ReadDirectory(TRemoteFileList * FileList)
{
  assert(FileList && !FileList->Directory.IsEmpty());

  AnsiString Directory = LocalCanonify(FileList->Directory);
  FTerminal->LogEvent(FORMAT("Listing directory \"%s\".", (Directory)));

  TSFTPPacket Packet(SSH_FXP_OPENDIR);
  Packet.AddString(Directory);

  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_HANDLE);

  AnsiString Handle = Packet.GetString();

  FileList->Clear();

  TSFTPPacket Response;
  try
  {
    bool isEOF = false;
    int Total = 0;
    TRemoteFile * File;

    Packet.ChangeType(SSH_FXP_READDIR);
    Packet.AddString(Handle);
    SendPacket(&Packet);

    do
    {
      ReceiveResponse(&Packet, &Response);

      if (Response.Type == SSH_FXP_NAME)
      {
        TSFTPPacket ListingPacket = Response;

        Packet.ChangeType(SSH_FXP_READDIR);
        Packet.AddString(Handle);
        SendPacket(&Packet);
        ReserveResponse(&Packet, &Response);

        unsigned int Count = ListingPacket.GetCardinal();
        for (unsigned long Index = 0; Index < Count; Index++)
        {
          File = LoadFile(&ListingPacket, NULL);
          FileList->AddFile(File);
          Total++;
        }
      }
      else if (Response.Type == SSH_FXP_STATUS)
      {
        isEOF = (GotStatusPacket(&Response, asEOF) == SSH_FX_EOF);
      }
      else
      {
        FTerminal->FatalError(FMTLOAD(SFTP_INVALID_TYPE, ((int)Response.Type)));
      }
    }
    while (!isEOF);

    if (Total == 0)
    {
      // Empty file list -> probably "permision denied", we
      // at least get link to parent directory ("..")
      try
      {
        FTerminal->ExceptionOnFail = true;
        try
        {
          File = NULL;
          FTerminal->ReadFile(
            UnixIncludeTrailingBackslash(FileList->Directory) + PARENTDIRECTORY, File);
        }
        __finally
        {
          FTerminal->ExceptionOnFail = false;
        }
      }
      catch(Exception &E)
      {
        if (E.InheritsFrom(__classid(EFatal))) throw;
          else File = NULL;
      }

      // on some systems even getting ".." fails, we create dummy ".." instead
      bool Failure = (File == NULL);
      if (Failure)
      {
        File = new TRemoteFile();
        File->FileName = PARENTDIRECTORY;
        File->Modification = Now();
        File->Type = FILETYPE_DIRECTORY;
      }

      assert(File && File->IsParentDirectory);
      FileList->AddFile(File);

      if (Failure)
      {
        throw Exception(FMTLOAD(EMPTY_DIRECTORY, (FileList->Directory)));
      }
    }
  }
  __finally
  {
    if (FTerminal->Active)
    {
      Packet.ChangeType(SSH_FXP_CLOSE);
      Packet.AddString(Handle);
      SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS, asAll);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ReadSymlink(TRemoteFile * SymlinkFile,
  TRemoteFile *& File)
{
  assert(SymlinkFile && SymlinkFile->IsSymLink);
  assert(FVersion >= 3); // symlinks are supported with SFTP version 3 and later

  AnsiString FileName = LocalCanonify(SymlinkFile->FileName);

  TSFTPPacket ReadLinkPacket(SSH_FXP_READLINK);
  ReadLinkPacket.AddString(FileName);
  SendPacket(&ReadLinkPacket);
  ReserveResponse(&ReadLinkPacket, &ReadLinkPacket);

  // send second request before reading response to first one
  // (performance benefit)
  TSFTPPacket AttrsPacket(SSH_FXP_STAT);
  AttrsPacket.AddString(FileName);
  SendPacket(&AttrsPacket);
  ReserveResponse(&AttrsPacket, &AttrsPacket);

  ReceiveResponse(&ReadLinkPacket, &ReadLinkPacket, SSH_FXP_NAME);
  if (ReadLinkPacket.GetCardinal() != 1)
  {
    FTerminal->FatalError(LoadStr(SFTP_NON_ONE_FXP_NAME_PACKET));
  }
  SymlinkFile->LinkTo = ReadLinkPacket.GetString();

  ReceiveResponse(&AttrsPacket, &AttrsPacket, SSH_FXP_ATTRS);
  File = LoadFile(&AttrsPacket, SymlinkFile);
  File->FileName = UnixExtractFileName(SymlinkFile->FileName);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ReadFile(const AnsiString FileName,
  TRemoteFile *& File)
{
  CustomReadFile(FileName, File, SSH_FXP_LSTAT);
}
//---------------------------------------------------------------------------
bool __fastcall inline TSFTPFileSystem::RemoteFileExists(const AnsiString FullPath,
  TRemoteFile ** File)
{
  bool Result;
  try
  {
    TRemoteFile * AFile;
    ReadFile(FullPath, AFile);
    Result = true;
    if (File)
    {
      *File = AFile;
    }
    else
    {
      delete AFile;
    }
  }
  catch(...)
  {
    if (!FTerminal->Active)
    {
      throw;
    }
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CustomReadFile(const AnsiString FileName,
  TRemoteFile *& File, char Type, TRemoteFile * ALinkedByFile)
{
  TSFTPPacket Packet(Type);

  Packet.AddString(LocalCanonify(FileName));
  if (FVersion >= 4)
  {
    Packet.AddCardinal(SSH_FILEXFER_ATTR_SIZE | SSH_FILEXFER_ATTR_PERMISSIONS |
      SSH_FILEXFER_ATTR_ACCESSTIME | SSH_FILEXFER_ATTR_MODIFYTIME |
      SSH_FILEXFER_ATTR_OWNERGROUP);
  }
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_ATTRS);

  File = LoadFile(&Packet, ALinkedByFile);
  File->FileName = UnixExtractFileName(FileName);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::DeleteFile(const AnsiString FileName,
  const TRemoteFile * File, bool Recursive)
{
  char Type;
  AnsiString RealFileName = LocalCanonify(FileName);
  if (File && File->IsDirectory && !File->IsSymLink)
  {
    if (Recursive)
    {
      FTerminal->ProcessDirectory(FileName, FTerminal->DeleteFile, &Recursive);
    }
    Type = SSH_FXP_RMDIR;
  }
  else
  {
    Type = SSH_FXP_REMOVE;
  }

  TSFTPPacket Packet(Type);
  Packet.AddString(RealFileName);
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::RenameFile(const AnsiString FileName,
  const AnsiString NewName)
{
  TSFTPPacket Packet(SSH_FXP_RENAME);
  AnsiString RealName = LocalCanonify(FileName);
  Packet.AddString(RealName);
  Packet.AddString(UnixExtractFilePath(RealName) + NewName);
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CreateDirectory(const AnsiString DirName,
  const TRemoteProperties * Properties)
{
  TSFTPPacket Packet(SSH_FXP_MKDIR);
  AnsiString CanonifiedName = Canonify(DirName);
  Packet.AddString(CanonifiedName);
  Packet.AddProperties(Properties, 0, true, FVersion);
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CreateLink(const AnsiString FileName,
  const AnsiString PointTo, bool Symbolic)
{
  assert(Symbolic); // only symlinks are supported by SFTP
  assert(FVersion >= 3); // symlinks are supported with SFTP version 3 and later
  TSFTPPacket Packet(SSH_FXP_SYMLINK);
  Packet.AddString(PointTo);
  Packet.AddString(Canonify(FileName));
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ChangeFileProperties(const AnsiString FileName,
  const TRemoteFile * /*File*/, const TRemoteProperties * Properties)
{
  assert(Properties);

  TRemoteFile * File;

  AnsiString RealFileName = LocalCanonify(FileName);
  ReadFile(RealFileName, File);

  try
  {
    assert(File);

    if (File->IsDirectory && Properties->Recursive)
    {
      FTerminal->ProcessDirectory(FileName, FTerminal->ChangeFileProperties,
        (void*)Properties);
    }

    TSFTPPacket Packet(SSH_FXP_SETSTAT);
    Packet.AddString(RealFileName);
    Packet.AddProperties(Properties, *File->Rights, File->IsDirectory, FVersion);
    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
  }
  __finally
  {
    delete File;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CustomCommandOnFile(const AnsiString /*FileName*/,
    const TRemoteFile * /*File*/, AnsiString /*Command*/, int /*Params*/)
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::AnyCommand(const AnsiString /*Command*/)
{
  assert(false);
}
//---------------------------------------------------------------------------
// transfer protocol
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CopyToRemote(TStrings * FilesToCopy,
  const AnsiString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  bool & DisconnectWhenComplete)
{
  assert(FilesToCopy && OperationProgress);

  AnsiString FileName, FileNameOnly;
  AnsiString FullTargetDir = UnixIncludeTrailingBackslash(TargetDir);
  int Index = 0;
  while (Index < FilesToCopy->Count && !OperationProgress->Cancel)
  {
    bool Success = false;
    FileName = FilesToCopy->Strings[Index];
    FileNameOnly = ExtractFileName(FileName);
    assert(!FAvoidBusy);
    FAvoidBusy = true;

    try
    {
      try
      {
        if (FTerminal->SessionData->CacheDirectories)
        {
          FTerminal->DirectoryModified(TargetDir, false);

          if (DirectoryExists(FileName))
          {
            FTerminal->DirectoryModified(UnixIncludeTrailingBackslash(TargetDir)+
              FileNameOnly, true);
          }
        }
        SFTPSource(FileName, FullTargetDir, CopyParam, Params, OperationProgress);
        Success = true;
      }
      catch(EScpSkipFile & E)
      {
        SUSPEND_OPERATION (
          if (!FTerminal->HandleException(&E)) throw;
        );
      }
      catch(...)
      {
        throw;
      }
    }
    __finally
    {
      FAvoidBusy = false;
      OperationProgress->Finish(FileNameOnly, Success, DisconnectWhenComplete);
    }
    Index++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SFTPConfirmOverwrite(const AnsiString FileName,
  bool TargetBiggerThanSource, TFileOperationProgressType * OperationProgress,
  TSFTPOverwriteMode & OverwriteMode)
{
  if (OperationProgress->NoToAll)
  {
    THROW_SKIP_FILE_NULL;
  }
  else
  {
    int Answer;
    SUSPEND_OPERATION
    (
      int Answers = qaYes | qaNo | qaAbort | qaYesToAll | qaNoToAll;
      if ((FVersion < 4) || !OperationProgress->AsciiTransfer)
      {
        Answers |= qaAppend;
      }
      Answer = FTerminal->DoQueryUser(FORMAT(LoadStr(FILE_OVERWRITE), (FileName)),
        Answers, qpNeverAskAgainCheck);
    );

    if (Answer == qaAppend)
    {
      if (TargetBiggerThanSource || OperationProgress->AsciiTransfer)
      {
        OverwriteMode = omAppend;
      }
      else
      {
        SUSPEND_OPERATION
        (
          Answer = FTerminal->DoQueryUser(FORMAT(LoadStr(APPEND_OR_RESUME), (FileName)),
            qaYes | qaNo | qaCancel, 0);
        );
        
        if (Answer == qaCancel)
        {
          if (!OperationProgress->Cancel)
          {
            OperationProgress->Cancel = csCancel;
          }
          Abort();
        }
        else
        {
          OverwriteMode = ((Answer == qaYes) ? omAppend : omResume);
        }
      }
    }
    else
    {
      OverwriteMode = omOverwrite;
      switch (Answer) {
        case qaNeverAskAgain:
          FTerminal->Configuration->ConfirmOverwriting = false;
        case qaYesToAll:
          OperationProgress->YesToAll = true;
        case qaYes:
          // file overwriting was confirmed, try to open file
          // second time, now without exclusive flag (SSH_FXF_EXCL)
          break;

        case qaAbort:
          if (!OperationProgress->Cancel)
          {
            OperationProgress->Cancel = csCancel;
          }
          Abort();
          break;

        case qaNoToAll:
          OperationProgress->NoToAll = true;
        case qaNo:
          THROW_SKIP_FILE_NULL;
      }
    }
  }
}
//---------------------------------------------------------------------------
bool TSFTPFileSystem::SFTPConfirmResume(const AnsiString DestFileName,
  bool PartialBiggerThanSource, TFileOperationProgressType * OperationProgress)
{
  bool ResumeTransfer;
  assert(OperationProgress);
  if (PartialBiggerThanSource)
  {
    int Answer;
    SUSPEND_OPERATION
    (
      Answer = FTerminal->DoQueryUser(
        FMTLOAD(PARTIAL_BIGGER_THAN_SOURCE, (DestFileName)), NULL,
          qaOK | qaAbort, qpAllowContinueOnError, qtWarning);
    )

    if (Answer == qaAbort)
    {
      if (!OperationProgress->Cancel)
      {
        OperationProgress->Cancel = csCancel;
      }
      Abort();
    }
    ResumeTransfer = false;
  }
  else
  {
    int Answer;
    SUSPEND_OPERATION
    (
      Answer = FTerminal->DoQueryUser(
        FMTLOAD(RESUME_TRANSFER, (DestFileName)), qaYes | qaNo | qaAbort,
        qpAllowContinueOnError);
    );

    switch (Answer) {
      case qaYes:
        ResumeTransfer = true;
        break;

      case qaNo:
        ResumeTransfer = false;
        break;

      case qaAbort:
        if (!OperationProgress->Cancel)
        {
          OperationProgress->Cancel = csCancel;
        }
        Abort();
        break;
    }
  }
  return ResumeTransfer;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SFTPSource(const AnsiString FileName,
  const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress)
{
  FTerminal->LogEvent(FORMAT("File: \"%s\"", (FileName)));

  OperationProgress->SetFile(FileName);

  TOpenRemoteFileParams OpenParams;
  OpenParams.OverwriteMode = omOverwrite;

  HANDLE File;
  unsigned long MTime, ATime;
  __int64 Size;

  FTerminal->OpenLocalFile(FileName, GENERIC_READ, &OpenParams.LocalFileAttrs,
    &File, NULL, &MTime, &ATime, &Size);

  if (OpenParams.LocalFileAttrs & faDirectory)
  {
    SFTPDirectorySource(IncludeTrailingBackslash(FileName), TargetDir,
      OpenParams.LocalFileAttrs, CopyParam, Params, OperationProgress);
  }
    else
  try
  {
    // File is regular file (not directory)
    assert(File);

    AnsiString DestFileName = CopyParam->ChangeFileName(ExtractFileName(FileName), osLocal);
    AnsiString DestFullName = LocalCanonify(TargetDir + DestFileName);
    AnsiString DestPartinalFullName;
    bool ResumeAllowed;
    bool ResumeTransfer = false;
    bool DestFileExists = false;
    __int64 ResumeOffset;

    FTerminal->LogEvent(FORMAT("Copying \"%s\" to remote directory started.", (FileName)));

    OperationProgress->SetLocalSize(Size);

    // Suppose same data size to transfer as to read
    // (not true with ASCII transfer)
    OperationProgress->SetTransferSize(OperationProgress->LocalSize);
    OperationProgress->TransferingFile = false;

    // Will we use ASCII of BINARY file tranfer?
    OperationProgress->SetAsciiTransfer(CopyParam->UseAsciiTransfer(FileName));
    FTerminal->LogEvent(
      AnsiString((OperationProgress->AsciiTransfer ? "Ascii" : "Binary")) +
        " transfer mode selected.");

    ResumeAllowed = !OperationProgress->AsciiTransfer &&
      CopyParam->AllowResume(OperationProgress->LocalSize) &&
      IsCapable(fcRename);
    OperationProgress->SetResumeStatus(ResumeAllowed ? rsEnabled : rsDisabled);

    if (ResumeAllowed)
    {
      DestPartinalFullName = DestFullName + FTerminal->Configuration->PartialExt;

      FTerminal->LogEvent("Checking existence of file.");
      TRemoteFile * File = new TRemoteFile();
      DestFileExists = RemoteFileExists(DestFullName, &File);
      if (DestFileExists)
      {
        OpenParams.DestFileSize = File->Size;
        delete File;
        File = NULL;
      }

      FTerminal->LogEvent("Checking existence of partially transfered file.");
      if (RemoteFileExists(DestPartinalFullName, &File))
      {
        ResumeOffset = File->Size;
        delete File;
        File = NULL;

        ResumeTransfer = SFTPConfirmResume(DestFileName,
          ResumeOffset > OperationProgress->LocalSize, OperationProgress);

        if (!ResumeTransfer)
        {
          DeleteFile(DestPartinalFullName);
        }
        else
        {
          FTerminal->LogEvent("Resuming file transfer.");
        }
      }
      else
      {
        // partial upload file does not exists, check for full file
        if (DestFileExists && FTerminal->Configuration->ConfirmOverwriting &&
            !OperationProgress->YesToAll && !(Params & cpNoConfirmation))
        {
          SFTPConfirmOverwrite(DestFileName,
            OpenParams.DestFileSize >= OperationProgress->LocalSize,
            OperationProgress, OpenParams.OverwriteMode);
        }
      }
    }

    bool DoResume = (ResumeAllowed && (OpenParams.OverwriteMode == omOverwrite));
    OpenParams.RemoteFileName = DoResume ? DestPartinalFullName : DestFullName;
    OpenParams.Resume = DoResume;
    OpenParams.OperationProgress = OperationProgress;
    OpenParams.CopyParam = CopyParam;
    OpenParams.Params = Params;

    FTerminal->LogEvent("Opening remote file.");
    FTerminal->FileOperationLoop(SFTPOpenRemote, OperationProgress, true,
      FMTLOAD(SFTP_CREATE_FILE_ERROR, (OpenParams.RemoteFileName)),
      &OpenParams);

    bool DeleteFileAfter = true;
    __int64 DestWriteOffset = 0;

    try
    {
      if (OpenParams.OverwriteMode == omAppend)
      {
        FTerminal->LogEvent("Appending file.");
        DestWriteOffset = OpenParams.DestFileSize;
      }
      else if (ResumeTransfer || (OpenParams.OverwriteMode == omResume))
      {
        if (OpenParams.OverwriteMode == omResume)
        {
          FTerminal->LogEvent("Resuming file transfer (append style).");
          ResumeOffset = OpenParams.DestFileSize;
        }
        FileSeek((THandle)File, ResumeOffset, 0);
        OperationProgress->AddResumed(ResumeOffset);
      }

      // at end of this block queue is disposed
      {
        TSFTPUploadQueue Queue(this);
        unsigned long BlockSize = OperationProgress->StaticBlockSize();
        int QueueLen = int(OperationProgress->LocalSize / BlockSize);
        if (QueueLen > FTerminal->SessionData->SFTPUploadQueue)
        {
          QueueLen = FTerminal->SessionData->SFTPUploadQueue;
        }
        
        bool Initialized;
        Initialized = Queue.Init(QueueLen, FileName, File, OperationProgress,
          OpenParams.RemoteFileHandle, BlockSize,
          DestWriteOffset + OperationProgress->TransferedSize);

        if (Initialized)
        {
          while (Queue.Next(SSH_FXP_STATUS))
          {
            if (OperationProgress->Cancel == csCancel)
            {
              Abort();
            }
          }
        }
      }

      DeleteFileAfter = false;
    }
    __finally
    {
      if (FTerminal->Active)
      {
        FILE_OPERATION_LOOP(DestFileName,
          FMTLOAD(SFTP_CLOSE_FILE_ERROR, (DestFileName)),
          TSFTPPacket CloseRequest(SSH_FXP_CLOSE);
          CloseRequest.AddString(OpenParams.RemoteFileHandle);
          SendPacketAndReceiveResponse(&CloseRequest, NULL, SSH_FXP_STATUS);
        );
        // delete file if transfer was not completed and resuming is not allowed
        if (DeleteFileAfter && !DoResume)
        {
          DeleteFile(OpenParams.RemoteFileName);
        }
      }
    }

    // originally this was before CLOSE (last __finally statement),
    // on VShell it failed
    if (DoResume)
    {
      FILE_OPERATION_LOOP(DestFileName,
        FMTLOAD(RENAME_AFTER_RESUME_ERROR,
          (UnixExtractFileName(OpenParams.RemoteFileName), DestFileName)),

        if (DestFileExists)
        {
          DeleteFile(DestFullName);
          DestFileExists = false;
        }
        RenameFile(OpenParams.RemoteFileName, DestFileName);
      );
    }

    if (CopyParam->PreserveTime)
    {
      FILE_OPERATION_LOOP(DestFileName,
        FMTLOAD(CHANGE_PROPERTIES_ERROR, (DestFileName)),
        TSFTPPacket Packet(SSH_FXP_SETSTAT);
        Packet.AddString(DestFullName);
        if (FVersion >= 4)
        {
          Packet.AddCardinal(SSH_FILEXFER_ATTR_ACCESSTIME |
            SSH_FILEXFER_ATTR_MODIFYTIME);
          Packet.AddByte(SSH_FILEXFER_TYPE_REGULAR);
          Packet.AddInt64(ATime);
          Packet.AddInt64(MTime);
        }
        else
        {
          Packet.AddCardinal(SSH_FILEXFER_ATTR_ACMODTIME);
          Packet.AddCardinal(ATime);
          Packet.AddCardinal(MTime);
        }
        SendPacketAndReceiveResponse(&Packet, NULL, SSH_FXP_STATUS);
      );
    }
  }
  __finally
  {
    CloseHandle(File);
  }

  /* TODO : Delete also read-only files. */
  /* TODO : Show error message on failure. */
  if (Params & cpDelete) Sysutils::DeleteFile(FileName);
}
//---------------------------------------------------------------------------
int __fastcall TSFTPFileSystem::SFTPOpenRemote(void * AOpenParams, void * /*Param2*/)
{
  TOpenRemoteFileParams * OpenParams = (TOpenRemoteFileParams *)AOpenParams;
  assert(OpenParams);
  TFileOperationProgressType * OperationProgress = OpenParams->OperationProgress;

  TSFTPPacket OpenRequest;
  int OpenType;
  bool Confirmed = false;
  bool Success = false;

  do
  {
    try
    {
      OpenType = 0;
      OpenRequest.ChangeType(SSH_FXP_OPEN);
      if (FTerminal->Configuration->ConfirmOverwriting &&
          !Confirmed && !OperationProgress->YesToAll && !OpenParams->Resume &&
          !(OpenParams->Params & cpNoConfirmation) &&
          (OpenParams->OverwriteMode == omOverwrite))
      {
        OpenType |= SSH_FXF_EXCL;
      }
      if (!OpenParams->Resume && (OpenParams->OverwriteMode == omOverwrite))
      {
        OpenType |= SSH_FXF_TRUNC;
      }
      if ((FVersion >= 4) && OpenParams->OperationProgress->AsciiTransfer)
      {
        OpenType |= SSH_FXF_TEXT;
      }

      OpenRequest.AddString(OpenParams->RemoteFileName);
      OpenRequest.AddCardinal(SSH_FXF_WRITE | SSH_FXF_CREAT | OpenType );

      OpenRequest.AddCardinal(
        OpenParams->CopyParam->PreserveRights ? SSH_FILEXFER_ATTR_PERMISSIONS : 0);
      if (FVersion >= 4)
      {
        OpenRequest.AddByte(SSH_FILEXFER_TYPE_REGULAR);
      }
      if (OpenParams->CopyParam->PreserveRights)
      {
        OpenRequest.AddCardinal(
          OpenParams->CopyParam->RemoteFileRights(OpenParams->LocalFileAttrs));
      }
      SendPacketAndReceiveResponse(&OpenRequest, &OpenRequest, SSH_FXP_HANDLE);
      OpenParams->RemoteFileHandle = OpenRequest.GetString();
      Success = true;
    }
    catch(...)
    {
      if (!Confirmed && (OpenType & SSH_FXF_EXCL) && FTerminal->Active)
      {
        bool TargetBiggerThanSource;
        bool ThrowOriginal = false;

        // When exclusive opening of file fails, try to detect if file exists.
        // When file does not exist, failure was probably caused by 'permission denied'
        // or similar error. In this case throw original exception.
        try
        {
          TRemoteFile * File;
          AnsiString RealFileName = LocalCanonify(OpenParams->RemoteFileName);
          ReadFile(RealFileName, File);
          OpenParams->DestFileSize = File->Size;
          TargetBiggerThanSource = File->Size >= OperationProgress->LocalSize;
          // file exists (otherwise exception was thrown)
          assert(File);
          SAFE_DESTROY(File);
        }
        catch(...)
        {
          if (!FTerminal->Active)
          {
            throw;
          }
          else
          {
            ThrowOriginal = true;
          }
        }

        if (ThrowOriginal)
        {
          throw;
        }

        // confirmation duplicated in SFTPSource for resumable file transfers.
        SFTPConfirmOverwrite(UnixExtractFileName(OpenParams->RemoteFileName),
          TargetBiggerThanSource, OperationProgress, OpenParams->OverwriteMode);
        Confirmed = true;
      }
      else
      {
        throw;
      }
    }
  }
  while (!Success);

  return 0;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SFTPDirectorySource(const AnsiString DirectoryName,
  const AnsiString TargetDir, int Attrs, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress)
{
  AnsiString DestDirectoryName = CopyParam->ChangeFileName(
    ExtractFileName(ExcludeTrailingBackslash(DirectoryName)), osLocal);
  AnsiString DestFullName = UnixIncludeTrailingBackslash(TargetDir + DestDirectoryName);

  OperationProgress->SetFile(DirectoryName);

  try
  {
    TryOpenDirectory(DestFullName);
  }
  catch(...)
  {
    if (FTerminal->Active)
    {
      // opening directory failed, it probably does not exists, try to
      // create it
      TRemoteProperties Properties;
      if (CopyParam->PreserveRights)
      {
        Properties.Valid = TValidProperties() << vpRights;
        Properties.Rights = CopyParam->RemoteFileRights(Attrs);
      }
      FTerminal->CreateDirectory(DestFullName, &Properties);
    }
    else
    {
      throw;
    }
  }

  int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
  TSearchRec SearchRec;
  bool FindOK;

  FILE_OPERATION_LOOP (DirectoryName, FMTLOAD(LIST_DIR_ERROR, (DirectoryName)),
    FindOK = (bool)(FindFirst(DirectoryName + "*.*",
      FindAttrs, SearchRec) == 0);
  );

  while (FindOK && !OperationProgress->Cancel)
  {
    AnsiString FileName = DirectoryName + SearchRec.Name;
    try
    {
      if ((SearchRec.Name != ".") && (SearchRec.Name != ".."))
      {
        SFTPSource(FileName, DestFullName, CopyParam, Params, OperationProgress);
      }
    }
    catch (EScpSkipFile &E)
    {
      // If ESkipFile occurs, just log it and continue with next file
      SUSPEND_OPERATION (
        if (FTerminal->DoQueryUser(FMTLOAD(COPY_ERROR, (FileName)), E.Message,
              qaOK | qaAbort, qpAllowContinueOnError) == qaAbort)
        {
          OperationProgress->Cancel = csCancel;
        }
        if (!FTerminal->HandleException(&E)) throw;
      );
    }

    FILE_OPERATION_LOOP (DirectoryName, FMTLOAD(LIST_DIR_ERROR, (DirectoryName)),
      FindOK = (FindNext(SearchRec) == 0);
    );
  };

  FindClose(SearchRec);

  /* TODO : Delete also read-only directories. */
  /* TODO : Show error message on failure. */
  if ((Params & cpDelete) && !OperationProgress->Cancel) RemoveDir(DirectoryName);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CopyToLocal(TStrings * FilesToCopy,
  const AnsiString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  bool & DisconnectWhenComplete)
{
  assert(FilesToCopy && OperationProgress);

  AnsiString FileName, FileNameOnly;
  AnsiString FullTargetDir = IncludeTrailingBackslash(TargetDir);
  const TRemoteFile * File;
  bool Success;
  int Index = 0;
  while (Index < FilesToCopy->Count && !OperationProgress->Cancel)
  {
    Success = false;
    FileName = FilesToCopy->Strings[Index];
    FileNameOnly = UnixExtractFileName(FileName);
    File = (TRemoteFile *)FilesToCopy->Objects[Index];

    assert(!FAvoidBusy);
    FAvoidBusy = true;

    try
    {
      try
      {
        SFTPSink(LocalCanonify(FileName), File, FullTargetDir, CopyParam,
          Params, OperationProgress);
        Success = true;
      }
      catch(EScpSkipFile & E)
      {
        SUSPEND_OPERATION (
          if (!FTerminal->HandleException(&E)) throw;
        );
      }
      /*catch(...)
      {
        throw;
      } */
    }
    __finally
    {
      FAvoidBusy = false;
      OperationProgress->Finish(FileNameOnly, Success, DisconnectWhenComplete);
    }
    Index++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SFTPSink(const AnsiString FileName,
  const TRemoteFile * File, const AnsiString TargetDir,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress)
{
  assert(File);
  FTerminal->LogEvent(FORMAT("File: \"%s\"", (FileName)));

  AnsiString OnlyFileName = UnixExtractFileName(FileName);
  OperationProgress->SetFile(OnlyFileName);

  AnsiString DestFileName = CopyParam->ChangeFileName(OnlyFileName, osRemote);
  AnsiString DestFullName = TargetDir + DestFileName;

  if (File->IsDirectory)
  {
    FILE_OPERATION_LOOP (DestFullName, FMTLOAD(NOT_DIRECTORY_ERROR, (DestFullName)),
      int Attrs = FileGetAttr(DestFullName);
      if ((Attrs & faDirectory) == 0) EXCEPTION;
    );

    FILE_OPERATION_LOOP (DestFullName, FMTLOAD(CREATE_DIR_ERROR, (DestFullName)),
      if (!ForceDirectories(DestFullName)) EXCEPTION;
    );

    TSinkFileParams SinkFileParams;
    SinkFileParams.TargetDir = IncludeTrailingBackslash(DestFullName);
    SinkFileParams.CopyParam = CopyParam;
    SinkFileParams.Params = Params;
    SinkFileParams.OperationProgress = OperationProgress;
    SinkFileParams.Skipped = false;

    FTerminal->ProcessDirectory(FileName, SFTPSinkFile, &SinkFileParams);

    // Do not delete directory if some of its files were skip.
    // Throw "skip file" for the directory to avoid attempt to deletion
    // of any parent directory
    if ((Params & cpDelete) && SinkFileParams.Skipped)
    {
      THROW_SKIP_FILE_NULL;
    }
  }
  else
  {
    FTerminal->LogEvent(FORMAT("Copying \"%s\" to local directory started.", (FileName)));

    AnsiString DestPartinalFullName;
    bool ResumeAllowed;
    bool ResumeTransfer = false;
    __int64 ResumeOffset;

    // Will we use ASCII of BINARY file tranfer?
    OperationProgress->SetAsciiTransfer(CopyParam->UseAsciiTransfer(DestFileName));
    FTerminal->LogEvent(AnsiString((OperationProgress->AsciiTransfer ? "Ascii" : "Binary")) +
      " transfer mode selected.");

    // Suppose same data size to transfer as to write
    // (not true with ASCII transfer)
    OperationProgress->SetTransferSize(File->Size);
    OperationProgress->SetLocalSize(OperationProgress->TransferSize);

    // resume has no sense for drag&drop downloads, because files are downloaded
    // to temp directory, but anyway TTerminal does know this, so the condition
    // placement is currently wrong.
    ResumeAllowed = ((Params & cpDragDrop) == 0) &&
      !OperationProgress->AsciiTransfer &&
      CopyParam->AllowResume(OperationProgress->TransferSize);
    OperationProgress->SetResumeStatus(ResumeAllowed ? rsEnabled : rsDisabled);

    int Attrs;
    FILE_OPERATION_LOOP (DestFullName, FMTLOAD(NOT_FILE_ERROR, (DestFullName)),
      Attrs = FileGetAttr(DestFullName);
      if ((Attrs >= 0) && (Attrs & faDirectory)) EXCEPTION;
    );

    OperationProgress->TransferingFile = false; // not set with SFTP protocol

    HANDLE LocalHandle = NULL;
    TStream * FileStream = NULL;
    bool DeleteLocalFile = false;
    AnsiString RemoteHandle;
    AnsiString LocalFileName = DestFullName;

    try
    {
      if (ResumeAllowed)
      {
        DestPartinalFullName = DestFullName + FTerminal->Configuration->PartialExt;
        LocalFileName = DestPartinalFullName;

        FTerminal->LogEvent("Checking existence of partially transfered file.");
        if (FileExists(DestPartinalFullName))
        {
          FTerminal->OpenLocalFile(DestPartinalFullName, GENERIC_WRITE,
            NULL, &LocalHandle, NULL, NULL, NULL, &ResumeOffset);

          ResumeTransfer = SFTPConfirmResume(DestFileName,
            ResumeOffset > OperationProgress->TransferSize, OperationProgress);

          if (!ResumeTransfer)
          {
            CloseHandle(LocalHandle);
            LocalHandle = NULL;
            Sysutils::DeleteFile(DestPartinalFullName);
          }
          else
          {
            FTerminal->LogEvent("Resuming file transfer.");
            FileSeek((THandle)LocalHandle, ResumeOffset, 0);
            OperationProgress->AddResumed(ResumeOffset);
          }
        }
      }

      if ((Attrs >= 0) && FTerminal->Configuration->ConfirmOverwriting &&
          !OperationProgress->YesToAll && !ResumeTransfer &&
          !(Params & cpNoConfirmation))
      {
        __int64 DestFileSize;
        FTerminal->OpenLocalFile(DestFullName, GENERIC_WRITE,
          NULL, &LocalHandle, NULL, NULL, NULL, &DestFileSize);

        FTerminal->LogEvent("Checking existence of file.");
        TSFTPOverwriteMode OverwriteMode;
        SFTPConfirmOverwrite(DestFileName, DestFileSize >= OperationProgress->TransferSize,
          OperationProgress, OverwriteMode);

        if (OverwriteMode == omOverwrite)
        {
          CloseHandle(LocalHandle);
          LocalHandle = NULL;
        }
        else
        {
          ResumeAllowed = false;
          FileSeek((THandle)LocalHandle, DestFileSize, 0);
          if (OverwriteMode == omAppend)
          {
            FTerminal->LogEvent("Appending to file.");
          }
          else
          {
            FTerminal->LogEvent("Resuming file transfer (append style).");
            assert(OverwriteMode == omResume);
            OperationProgress->AddResumed(DestFileSize);
          }
        }
      }

      // if not already opened (resume, append...), create new empty file
      if (!LocalHandle)
      {
        // Create file
        FILE_OPERATION_LOOP (DestFullName, FMTLOAD(CREATE_FILE_ERROR, (DestFullName)),
          LocalHandle = CreateFile(LocalFileName.c_str(), GENERIC_WRITE, 0, NULL,
            CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
          if (LocalHandle == INVALID_HANDLE_VALUE) EXCEPTION;
        );
      }
      assert(LocalHandle);

      DeleteLocalFile = true;

      FTerminal->LogEvent("Opening remote file.");
      FILE_OPERATION_LOOP (FileName, FMTLOAD(SFTP_OPEN_FILE_ERROR, (FileName)),
        TSFTPPacket Packet;
        Packet.ChangeType(SSH_FXP_OPEN);
        Packet.AddString(FileName);
        int OpenType = SSH_FXF_READ;
        if ((FVersion >= 4) && OperationProgress->AsciiTransfer)
        {
          OpenType |= SSH_FXF_TEXT;
        }
        Packet.AddCardinal(OpenType);
        Packet.AddCardinal(0); // no attrs
        if (FVersion >= 4)
        {
          Packet.AddCardinal(SSH_FILEXFER_TYPE_REGULAR);
        }
        SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_HANDLE);
        RemoteHandle = Packet.GetString();
      );

      FileStream = new THandleStream((THandle)LocalHandle);

      unsigned long BlockSize = OperationProgress->StaticBlockSize();

      // at end of this block queue is disposed
      {
        TSFTPDownloadQueue Queue(this);
        TSFTPPacket DataPacket;
        int QueueLen = int(File->Size / BlockSize) + 1;
        if (QueueLen > FTerminal->SessionData->SFTPDownloadQueue)
        {
          QueueLen = FTerminal->SessionData->SFTPDownloadQueue;
        }
        Queue.Init(QueueLen, RemoteHandle, BlockSize,
          OperationProgress->TransferedSize);

        bool Eof = false;
        bool PrevIncomplete = false;
        unsigned long DataLen = 0;
        while (!Eof)
        {
          // Buffer for one block of data
          TFileBuffer BlockBuf;

          Queue.ReceivePacket(&DataPacket, SSH_FXP_DATA, asEOF);

          if (DataPacket.Type == SSH_FXP_STATUS)
          {
            // must be SSH_FX_EOF, any other status packet would raise exception
            Eof = true;
          }

          if (!Eof)
          {
            if (PrevIncomplete)
            {
              FTerminal->LogEvent(FORMAT(
                "Received incomplete data packet before end of file, "
                "offset: %s, size: %d, requested: %d",
                (IntToStr(OperationProgress->TransferedSize), int(DataLen),
                int(BlockSize))));
              FTerminal->TerminalError(NULL, LoadStr(SFTP_INCOMPLETE_BEFORE_EOF));
            }

            DataLen = DataPacket.GetCardinal();
            assert(DataLen <= BlockSize);
            PrevIncomplete = (DataLen < BlockSize);
            BlockBuf.Insert(0, DataPacket.NextData, DataLen);
            OperationProgress->AddTransfered(DataLen);

            if (OperationProgress->AsciiTransfer)
            {
              assert(!ResumeTransfer && !ResumeAllowed);

              unsigned int PrevBlockSize = BlockBuf.Size;
              BlockBuf.Convert(GetEOL(), FTerminal->Configuration->LocalEOLType, 0);
              OperationProgress->SetLocalSize(
                OperationProgress->LocalSize - PrevBlockSize + BlockBuf.Size);
            }

            FILE_OPERATION_LOOP (
              DestFullName, FMTLOAD(WRITE_ERROR, (LocalFileName)),
              BlockBuf.WriteToStream(FileStream, BlockBuf.Size);
            );

            OperationProgress->AddLocalyUsed(BlockBuf.Size);

            if (OperationProgress->Cancel == csCancel)
            {
              Abort();
            }
          }
        };
      }

      if (CopyParam->PreserveTime)
      {
        FILETIME AcTime = DateTimeToFileTime(File->LastAccess);
        FILETIME WrTime = DateTimeToFileTime(File->Modification);
        SetFileTime(LocalHandle, NULL, &AcTime, &WrTime);
      }

      CloseHandle(LocalHandle);
      LocalHandle = NULL;

      if (ResumeAllowed)
      {
        FILE_OPERATION_LOOP(DestFileName,
          FMTLOAD(RENAME_AFTER_RESUME_ERROR,
            (ExtractFileName(DestPartinalFullName), DestFileName)),

          if (FileExists(DestFullName))
          {
            if (!Sysutils::DeleteFile(DestFullName)) EXCEPTION;
          }
          if (!Sysutils::RenameFile(DestPartinalFullName, DestFullName))
          {
            EXCEPTION;
          }
        );
      }

      DeleteLocalFile = false;

      if (Attrs == -1)
      {
        Attrs = faArchive;
      }
      int NewAttrs = CopyParam->LocalFileAttrs(*File->Rights);
      if ((NewAttrs & Attrs) != NewAttrs)
      {
        FILE_OPERATION_LOOP (DestFullName, FMTLOAD(CANT_SET_ATTRS, (DestFullName)),
          FileSetAttr(DestFullName, Attrs | NewAttrs);
        );
      }

    }
    __finally
    {
      if (LocalHandle) CloseHandle(LocalHandle);
      if (FileStream) delete FileStream;
      if (DeleteLocalFile && (!ResumeAllowed || OperationProgress->LocalyUsed == 0))
      {
        Sysutils::DeleteFile(LocalFileName);
      }

      if (FTerminal->Active && !RemoteHandle.IsEmpty())
      {
        FILE_OPERATION_LOOP(FileName,
          FMTLOAD(SFTP_CLOSE_FILE_ERROR, (DestFileName)),
          TSFTPPacket CloseRequest(SSH_FXP_CLOSE);
          CloseRequest.AddString(RemoteHandle);
          SendPacketAndReceiveResponse(&CloseRequest, NULL, SSH_FXP_STATUS);
        );
      }
    }
  }

  if (Params & cpDelete)
  {
    // If file is directory, do not delete it recursively, because it should be
    // empty already. If not, it should not be deleted (some files were
    // skipped or some new files were copied to it, while we were downloading)  
    bool Recursive = false;
    FTerminal->DeleteFile(FileName, File, &Recursive);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SFTPSinkFile(AnsiString FileName,
  const TRemoteFile * File, void * Param)
{
  TSinkFileParams * Params = (TSinkFileParams *)Param;
  assert(Params->OperationProgress);
  try
  {
    SFTPSink(FileName, File, Params->TargetDir, Params->CopyParam,
      Params->Params, Params->OperationProgress);
  }
  catch(EScpSkipFile & E)
  {
    TFileOperationProgressType * OperationProgress = Params->OperationProgress;

    Params->Skipped = true;

    SUSPEND_OPERATION (
      if (!FTerminal->HandleException(&E)) throw;
    );

    if (OperationProgress->Cancel)
    {
      Abort();
    }
  }
}



