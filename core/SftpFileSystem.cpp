//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SftpFileSystem.h"

#include "PuttyIntf.h"
#include "Common.h"
#include "Interface.h"
#include "Terminal.h"
#include "TextsCore.h"

#include <memory>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_EX(ALLOW_SKIP, MESSAGE, OPERATION) \
  FILE_OPERATION_LOOP_CUSTOM(FTerminal, ALLOW_SKIP, MESSAGE, OPERATION)
//---------------------------------------------------------------------------
#define SSH_FX_OK                                 0
#define SSH_FX_EOF                                1
#define SSH_FX_NO_SUCH_FILE                       2
#define SSH_FX_OP_UNSUPPORTED                     8

#define SSH_FXP_INIT               1
#define SSH_FXP_VERSION            2
#define SSH_FXP_OPEN               3
#define SSH_FXP_CLOSE              4
#define SSH_FXP_READ               5
#define SSH_FXP_WRITE              6
#define SSH_FXP_LSTAT              7 
#define SSH_FXP_FSTAT              8 
#define SSH_FXP_SETSTAT            9 
#define SSH_FXP_FSETSTAT           10
#define SSH_FXP_OPENDIR            11
#define SSH_FXP_READDIR            12
#define SSH_FXP_REMOVE             13
#define SSH_FXP_MKDIR              14 
#define SSH_FXP_RMDIR              15 
#define SSH_FXP_REALPATH           16 
#define SSH_FXP_STAT               17 
#define SSH_FXP_RENAME             18
#define SSH_FXP_READLINK           19
#define SSH_FXP_SYMLINK            20
#define SSH_FXP_STATUS             101   
#define SSH_FXP_HANDLE             102   
#define SSH_FXP_DATA               103   
#define SSH_FXP_NAME               104   
#define SSH_FXP_ATTRS              105   
#define SSH_FXP_EXTENDED           200   
#define SSH_FXP_EXTENDED_REPLY     201
#define SSH_FXP_ATTRS              105

#define SSH_FILEXFER_ATTR_SIZE              0x00000001
#define SSH_FILEXFER_ATTR_UIDGID            0x00000002
#define SSH_FILEXFER_ATTR_PERMISSIONS       0x00000004
#define SSH_FILEXFER_ATTR_ACMODTIME         0x00000008
#define SSH_FILEXFER_ATTR_EXTENDED          0x80000000
#define SSH_FILEXFER_ATTR_ACCESSTIME        0x00000008
#define SSH_FILEXFER_ATTR_CREATETIME        0x00000010
#define SSH_FILEXFER_ATTR_MODIFYTIME        0x00000020
#define SSH_FILEXFER_ATTR_ACL               0x00000040
#define SSH_FILEXFER_ATTR_OWNERGROUP        0x00000080
#define SSH_FILEXFER_ATTR_SUBSECOND_TIMES   0x00000100
#define SSH_FILEXFER_ATTR_BITS              0x00000200
#define SSH_FILEXFER_ATTR_EXTENDED          0x80000000

#define SSH_FILEXFER_ATTR_COMMON \
  (SSH_FILEXFER_ATTR_SIZE | SSH_FILEXFER_ATTR_OWNERGROUP | \
   SSH_FILEXFER_ATTR_PERMISSIONS | SSH_FILEXFER_ATTR_ACCESSTIME | \
   SSH_FILEXFER_ATTR_MODIFYTIME)

#define SSH_FILEXFER_TYPE_REGULAR          1
#define SSH_FILEXFER_TYPE_DIRECTORY        2
#define SSH_FILEXFER_TYPE_SYMLINK          3
#define SSH_FILEXFER_TYPE_SPECIAL          4
#define SSH_FILEXFER_TYPE_UNKNOWN          5

#define SSH_FXF_READ            0x00000001
#define SSH_FXF_WRITE           0x00000002
#define SSH_FXF_APPEND          0x00000004
#define SSH_FXF_CREAT           0x00000008
#define SSH_FXF_TRUNC           0x00000010
#define SSH_FXF_EXCL            0x00000020
#define SSH_FXF_TEXT            0x00000040

#define SSH_FXF_ACCESS_DISPOSITION        0x00000007
#define     SSH_FXF_CREATE_NEW            0x00000000
#define     SSH_FXF_CREATE_TRUNCATE       0x00000001
#define     SSH_FXF_OPEN_EXISTING         0x00000002
#define     SSH_FXF_OPEN_OR_CREATE        0x00000003
#define     SSH_FXF_TRUNCATE_EXISTING     0x00000004
#define SSH_FXF_ACCESS_APPEND_DATA        0x00000008
#define SSH_FXF_ACCESS_APPEND_DATA_ATOMIC 0x00000010
#define SSH_FXF_ACCESS_TEXT_MODE          0x00000020
#define SSH_FXF_ACCESS_READ_LOCK          0x00000040
#define SSH_FXF_ACCESS_WRITE_LOCK         0x00000080
#define SSH_FXF_ACCESS_DELETE_LOCK        0x00000100

#define ACE4_READ_DATA         0x00000001
#define ACE4_LIST_DIRECTORY    0x00000001
#define ACE4_WRITE_DATA        0x00000002
#define ACE4_ADD_FILE          0x00000002
#define ACE4_APPEND_DATA       0x00000004
#define ACE4_ADD_SUBDIRECTORY  0x00000004
#define ACE4_READ_NAMED_ATTRS  0x00000008
#define ACE4_WRITE_NAMED_ATTRS 0x00000010
#define ACE4_EXECUTE           0x00000020
#define ACE4_DELETE_CHILD      0x00000040
#define ACE4_READ_ATTRIBUTES   0x00000080
#define ACE4_WRITE_ATTRIBUTES  0x00000100
#define ACE4_DELETE            0x00010000
#define ACE4_READ_ACL          0x00020000
#define ACE4_WRITE_ACL         0x00040000
#define ACE4_WRITE_OWNER       0x00080000
#define ACE4_SYNCHRONIZE       0x00100000

#define SSH_FILEXFER_ATTR_FLAGS_HIDDEN           0x00000004

#define SFTP_MAX_PACKET_LEN   102400
//---------------------------------------------------------------------------
#define SFTP_EXT_WINSCP "winscp@winscp.sourceforge.net"
#define SFTP_EXT_OWNER_GROUP "owner-group-query@generic-extensions"
#define SFTP_EXT_OWNER_GROUP_REPLY "owner-group-query-reply@generic-extensions"
#define SFTP_EXT_SOFTWARE "software@generic-extensions"
#define SFTP_EXT_SOFTWARE_OS "Microsoft Windows"
#define SFTP_EXT_NEWLINE "newline"
#define SFTP_EXT_SUPPORTED "supported"
//---------------------------------------------------------------------------
#define OGQ_LIST_OWNERS 0x01
#define OGQ_LIST_GROUPS 0x02
//---------------------------------------------------------------------------
const int SFTPMinVersion = 0;
const int SFTPMaxVersion = 5;
const int SFTPNoMessageNumber = -1;

const int asNo =            0;
const int asOK =            1 << SSH_FX_OK;
const int asEOF =           1 << SSH_FX_EOF;
const int asOpUnsupported = 1 << SSH_FX_OP_UNSUPPORTED;
const int asNoSuchFile =    1 << SSH_FX_NO_SUCH_FILE;
const int asAll = 0xFFFF;

const int tfFirstLevel =   0x01;
const int tfNewDirectory = 0x02;
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
//---------------------------------------------------------------------------
#define SFTP_PACKET_ALLOC_DELTA 256
//---------------------------------------------------------------------------
#pragma warn -inl
//---------------------------------------------------------------------------
struct TSFTPSupport
{
  unsigned int AttributeMask;
  unsigned int AttributeBits;
  unsigned int OpenFlags;
  unsigned int AccessMask;
  unsigned int MaxReadSize;
  TStrings * Extensions;
};
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

  TSFTPPacket(const char * Source, unsigned int Len)
  {
    Init();
    FLength = Len;
    Capacity = FLength;
    memcpy(Data, Source, Len);
  }

  TSFTPPacket(const AnsiString & Source)
  {
    Init();
    FLength = Source.Length();
    Capacity = FLength;
    memcpy(Data, Source.c_str(), Source.Length());
  }

  ~TSFTPPacket()
  {
    if (FData != NULL)
    {
      delete[] (FData - FSendPrefixLen);
    }
    if (FReservedBy) FReservedBy->UnreserveResponse(this);
  }

  void ChangeType(unsigned char AType)
  {
    FPosition = 0;
    FLength = 0;
    Capacity = 0;
    FType = AType;
    AddByte(FType);
    if ((FType != 1) && (FType != SSH_FXP_INIT))
    {
      AssignNumber();
      AddCardinal(FMessageNumber);
    }
  }

  void Reuse()
  {
    AssignNumber();
    
    assert(Length >= 5);

    // duplicated in AddCardinal()
    unsigned char Buf[4];
    PUT_32BIT(Buf, FMessageNumber);

    memcpy(FData + 1, Buf, sizeof(Buf));
  }

  void AddByte(unsigned char Value)
  {
    Add(&Value, sizeof(Value));
  }

  void AddCardinal(unsigned long Value)
  {
    // duplicated in Reuse()
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

  inline void AddUtfString(const AnsiString Value)
  {
    AddString(TSFTPFileSystem::EncodeUTF(Value));
  }

  inline void AddString(const AnsiString Value, bool Utf)
  {
    if (Utf)
    {
      AddUtfString(Value);
    }
    else
    {
      AddString(Value);
    }
  }

  inline void AddPathString(const AnsiString Value, int Version, bool Utf)
  {
    AddString(Value, (Version >= 4) && Utf);
  }

  void AddProperties(unsigned short * Rights, AnsiString * Owner,
    AnsiString * Group, unsigned long * MTime, unsigned long * ATime,
    __int64 * Size, bool IsDirectory, int Version, bool Utf)
  {
    int Flags = 0;
    if (Size != NULL)
    {
      Flags |= SSH_FILEXFER_ATTR_SIZE;
    }
    if ((Owner != NULL) || (Group != NULL))
    {
      Flags |= SSH_FILEXFER_ATTR_OWNERGROUP;
    }
    if (Rights != NULL)
    {
      Flags |= SSH_FILEXFER_ATTR_PERMISSIONS;
    }
    if ((Version < 4) && ((MTime != NULL) || (ATime != NULL)))
    {
      Flags |= SSH_FILEXFER_ATTR_ACMODTIME;
    }
    if ((Version >= 4) && (ATime != NULL))
    {
      Flags |= SSH_FILEXFER_ATTR_ACCESSTIME;
    }
    if ((Version >= 4) && (MTime != NULL))
    {
      Flags |= SSH_FILEXFER_ATTR_MODIFYTIME;
    }
    AddCardinal(Flags);

    if (Version >= 4)
    {
      AddByte(static_cast<unsigned char>(IsDirectory ?
        SSH_FILEXFER_TYPE_DIRECTORY : SSH_FILEXFER_TYPE_REGULAR));
    }

    if (Size != NULL)
    {
      AddInt64(*Size);
    }

    if ((Owner != NULL) || (Group != NULL))
    {
      assert(Version >= 4);
      AddString(Owner != NULL ? *Owner : AnsiString(), Utf);
      AddString(Group != NULL ? *Group : AnsiString(), Utf);
    }

    if (Rights != NULL)
    {
      AddCardinal(*Rights);
    }

    if ((Version < 4) && ((MTime != NULL) || (ATime != NULL)))
    {
      AddCardinal(ATime != NULL ? *ATime : *MTime);
      AddCardinal(MTime != NULL ? *MTime : *ATime);
    }
    if ((Version >= 4) && (ATime != NULL))
    {
      AddInt64(*ATime);
    }
    if ((Version >= 4) && (MTime != NULL))
    {
      AddInt64(*MTime);
    }
  }

  void AddProperties(const TRemoteProperties * Properties,
    unsigned short BaseRights, bool IsDirectory, int Version, bool Utf)
  {
    enum { valNone = 0, valRights = 0x01, valOwner = 0x02, valGroup = 0x04 }
      Valid = valNone;
    unsigned short RightsNum = 0;
    AnsiString Owner;
    AnsiString Group;

    if (Properties != NULL)
    {
      if (Properties->Valid.Contains(vpGroup))
      {
        Valid |= valGroup;
        Group = Properties->Group;
      }

      if (Properties->Valid.Contains(vpOwner))
      {
        Valid |= valOwner;
        Owner = Properties->Owner;
      }

      if (Properties->Valid.Contains(vpRights))
      {
        Valid |= valRights;
        TRights Rights = BaseRights;
        Rights |= Properties->Rights.NumberSet;
        Rights &= (unsigned short)~Properties->Rights.NumberUnset;
        if (IsDirectory && Properties->AddXToDirectories)
        {
          Rights.AddExecute();
        }
        RightsNum = Rights;
      }
    }

    AddProperties(
      Valid & valRights ? &RightsNum : NULL,
      Valid & valOwner ? &Owner : NULL,
      Valid & valGroup ? &Group : NULL,
      NULL, NULL, NULL, IsDirectory, Version, Utf);
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

  inline AnsiString GetUtfString()
  {
    return TSFTPFileSystem::DecodeUTF(GetString());
  }

  inline AnsiString GetString(bool Utf)
  {
    return (Utf ? GetUtfString() : GetString());
  }

  inline AnsiString GetPathString(int Version, bool Utf)
  {
    return GetString((Version >= 4) && Utf);
  }

  void GetFile(TRemoteFile * File, int Version, bool ConsiderDST, bool Utf)
  {
    assert(File);
    unsigned int Flags;
    AnsiString ListingStr;
    unsigned long Permissions;
    bool ParsingFailed = false;
    if (Type != SSH_FXP_ATTRS)
    {
      File->FileName = GetPathString(Version, Utf);
      if (Version < 4)
      {
        ListingStr = GetString();
      }
    }
    Flags = GetCardinal();
    if (Version >= 4)
    {
      char FXType = GetByte();
      // -:regular, D:directory, L:symlink, S:special, U:unknown
      // O:socket, C:char devide, B:block device, F:fifo 
      static char* Types = "-DLSUOCBF";
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
      File->Owner = GetString(Utf);
      File->Group = GetString(Utf);
    }
    if (Flags & SSH_FILEXFER_ATTR_PERMISSIONS)
    {
      Permissions = GetCardinal();
    }
    if (Version < 4)
    {
      if (Flags & SSH_FILEXFER_ATTR_ACMODTIME)
      {
        File->LastAccess = UnixToDateTime(GetCardinal(), ConsiderDST);
        File->Modification = UnixToDateTime(GetCardinal(), ConsiderDST);
      }
    }
    else
    {
      if (Flags & SSH_FILEXFER_ATTR_ACCESSTIME)
      {
        File->LastAccess = UnixToDateTime((unsigned long)GetInt64(), ConsiderDST);
        if (Flags & SSH_FILEXFER_ATTR_SUBSECOND_TIMES)
        {
          GetCardinal(); // skip access time subseconds
        }
      }
      else
      {
        File->LastAccess = Now();
      }
      if (Flags & SSH_FILEXFER_ATTR_CREATETIME)
      {
        GetInt64(); // skip create time
        if (Flags & SSH_FILEXFER_ATTR_SUBSECOND_TIMES)
        {
          GetCardinal(); // skip create time subseconds
        }
      }
      if (Flags & SSH_FILEXFER_ATTR_MODIFYTIME)
      {
        File->Modification = UnixToDateTime((unsigned long)GetInt64(), ConsiderDST);
        if (Flags & SSH_FILEXFER_ATTR_SUBSECOND_TIMES)
        {
          GetCardinal(); // skip modification time subseconds
        }
      }
      else
      {
        File->Modification = Now();
      }
    }

    if (Flags & SSH_FILEXFER_ATTR_ACL)
    {
      GetString();
    }

    if (Flags & SSH_FILEXFER_ATTR_BITS)
    {
      assert(Version >= 5);
      unsigned long Bits = GetCardinal();
      if (FLAGSET(Bits, SSH_FILEXFER_ATTR_FLAGS_HIDDEN))
      {
        File->IsHidden = true;
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
      File->Rights->Number = (unsigned short)(Permissions & TRights::rfAllSpecials);
      if (Version < 4)
      {
        File->Type = (Permissions & TRights::rfDirectory ? FILETYPE_DIRECTORY : '-');
      }
    }

    if (Flags & SSH_FILEXFER_ATTR_EXTENDED)
    {
      unsigned int ExtendedCount = GetCardinal();
      for (unsigned int Index = 0; Index < ExtendedCount; Index++)
      {
        GetString(); // skip extended_type
        GetString(); // skip extended_data
      }
    }

    File->Complete();
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

  void LoadFromFile(const AnsiString FileName)
  {
    TStringList * DumpLines = new TStringList();
    AnsiString Dump;
    try
    {
      DumpLines->LoadFromFile(FileName);
      Dump = DumpLines->Text;
    }
    __finally
    {
      delete DumpLines;
    }

    Capacity = 20480;
    char Byte[3];
    memset(Byte, '\0', sizeof(Byte));
    int Index = 1;
    unsigned int Length = 0;
    while (Index < Dump.Length())
    {
      char C = Dump[Index];
      if (((C >= '0') && (C <= '9')) || ((C >= 'A') && (C <= 'Z')))
      {
        if (Byte[0] == '\0')
        {
          Byte[0] = C;
        }
        else
        {
          Byte[1] = C;
          assert(Length < Capacity);
          Data[Length] = static_cast<unsigned char>(HexToInt(Byte));
          Length++;  
          memset(Byte, '\0', sizeof(Byte));
        }
      }
      Index++;
    }
    DataUpdated(Length);
  }

  AnsiString __fastcall Dump() const
  {
    AnsiString Result;
    for (unsigned int Index = 0; Index < Length; Index++)
    {
      Result += IntToHex(int((unsigned char)Data[Index]), 2) + ",";
      if (((Index + 1) % 25) == 0)
      {
        Result += "\n";
      }
    }
    return Result;
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
  __property char * SendData = { read = GetSendData };
  __property unsigned int SendLength = { read = GetSendLength };
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
  static const FSendPrefixLen = 4;

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

  void AssignNumber()
  {
    // this is not strictly thread-safe, but as it is accessed from multiple 
    // threads only for multiple connection, it is not problem if two threads get
    // the same number
    FMessageNumber = (FMessageCounter << 8) + FType;
    FMessageCounter++;
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
        char * NData = (new char[FCapacity + FSendPrefixLen]) + FSendPrefixLen;
        if (FData)
        {
          memcpy(NData - FSendPrefixLen, FData - FSendPrefixLen,
            (FLength < FCapacity ? FLength : FCapacity) + FSendPrefixLen);
          delete[] (FData - FSendPrefixLen);
        }
        FData = NData;
      }
      else
      {
        if (FData) delete[] (FData - FSendPrefixLen);
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

  char * GetNextData()
  {
    return FPosition < FLength ? FData + FPosition : NULL;
  }

  char * GetSendData() const
  {
    char * Result = FData - FSendPrefixLen;
    // this is not strictly const-object operation
    PUT_32BIT(Result, Length);
    return Result;
  }

  unsigned int GetSendLength() const
  {
    return FSendPrefixLen + Length;
  }
};
//---------------------------------------------------------------------------
int TSFTPPacket::FMessageCounter = 0;
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
    FMissedRequests = 0;
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
      TSFTPQueuePacket * Request;
      TSFTPPacket * Response;

      assert(FResponses->Count == FRequests->Count);
      for (int Index = 0; Index < FRequests->Count; Index++)
      {
        Request = static_cast<TSFTPQueuePacket*>(FRequests->Items[Index]);
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
    FMissedRequests = QueueLen;
    return Result;
  }

  void __fastcall Dispose()
  {
    assert(FFileSystem->FTerminal->Active);

    TSFTPQueuePacket * Request;
    TSFTPPacket * Response;

    while (FRequests->Count)
    {
      assert(FResponses->Count);

      Request = static_cast<TSFTPQueuePacket*>(FRequests->Items[0]);
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
          FFileSystem->FTerminal->DoHandleExtendedException(&E);
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
    int ExpectedType = -1, int AllowStatus = -1, void ** Token = NULL)
  {
    assert(FRequests->Count);
    bool Result;
    TSFTPQueuePacket * Request = NULL;
    TSFTPPacket * Response = NULL;
    try
    {
      Request = static_cast<TSFTPQueuePacket*>(FRequests->Items[0]);
      FRequests->Delete(0);
      assert(Request);
      if (Token != NULL)
      {
        *Token = Request->Token;
      }

      Response = static_cast<TSFTPPacket*>(FResponses->Items[0]);
      FResponses->Delete(0);
      assert(Response);

      FFileSystem->ReceiveResponse(Request, Response,
        ExpectedType, AllowStatus);

      if (Packet)
      {
        *Packet = *Response;
      }

      Result = !End(Response);
      if (Result)
      {
        FMissedRequests++;
        while ((FMissedRequests > 0) && SendRequest())
        {
          FMissedRequests--;
        }
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
  class TSFTPQueuePacket : public TSFTPPacket
  {
  public:
    TSFTPQueuePacket() :
      TSFTPPacket()
    {
      Token = NULL;
    }
  
    void * Token;
  };

  virtual bool __fastcall InitRequest(TSFTPQueuePacket * Request) = 0;

  virtual bool __fastcall End(TSFTPPacket * Response) = 0;

  virtual void __fastcall SendPacket(TSFTPQueuePacket * Packet)
  {
    FFileSystem->SendPacket(Packet);
  }

protected:
  TList * FRequests;
  TList * FResponses;
  TSFTPFileSystem * FFileSystem;
  int FMissedRequests;

private:
  bool __fastcall SendRequest()
  {
    TSFTPQueuePacket * Request = NULL;
    try
    {
      Request = new TSFTPQueuePacket();
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
};
//---------------------------------------------------------------------------
class TSFTPDownloadQueue : public TSFTPTransferQueue
{
public:
  TSFTPDownloadQueue(TSFTPFileSystem * AFileSystem) :
    TSFTPTransferQueue(AFileSystem)
  {
  }

  bool __fastcall Init(int QueueLen, const AnsiString AHandle,__int64 ATransfered,
    TFileOperationProgressType * AOperationProgress)
  {
    FHandle = AHandle;
    FTransfered = ATransfered;
    OperationProgress = AOperationProgress;

    return TSFTPTransferQueue::Init(QueueLen);
  }

  void __fastcall InitFillGapRequest(__int64 Offset, unsigned long Missing,
    TSFTPPacket * Packet)
  {
    InitRequest(Packet, Offset, Missing);
  }

  bool __fastcall ReceivePacket(TSFTPPacket * Packet, unsigned long & BlockSize)
  {
    void * Token;
    bool Result = TSFTPTransferQueue::ReceivePacket(Packet, SSH_FXP_DATA, asEOF, &Token);
    BlockSize = reinterpret_cast<unsigned long>(Token);
    return Result;
  }
    
protected:
  virtual bool __fastcall InitRequest(TSFTPQueuePacket * Request)
  {
    unsigned int BlockSize = FFileSystem->DownloadBlockSize(OperationProgress);
    InitRequest(Request, FTransfered, BlockSize);
    Request->Token = reinterpret_cast<void*>(BlockSize);
    FTransfered += BlockSize;
    return true;
  }

  void __fastcall InitRequest(TSFTPPacket * Request, __int64 Offset,
    unsigned long Size)
  {
    Request->ChangeType(SSH_FXP_READ);
    Request->AddString(FHandle);
    Request->AddInt64(Offset);
    Request->AddCardinal(Size);
  }

  virtual bool __fastcall End(TSFTPPacket * Response)
  {
    return (Response->Type != SSH_FXP_DATA);
  }

private:
  TFileOperationProgressType * OperationProgress;
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
    FEnd = false;
  }

  bool __fastcall Init(int QueueLen, const AnsiString AFileName,
    HANDLE AFile, TFileOperationProgressType * AOperationProgress,
    const AnsiString AHandle, __int64 ATransfered)
  {
    FFileName = AFileName;
    FFile = AFile;
    OperationProgress = AOperationProgress;
    FHandle = AHandle;
    FTransfered = ATransfered;

    return TSFTPTransferQueue::Init(QueueLen);
  }

protected:
  virtual bool __fastcall InitRequest(TSFTPQueuePacket * Request)
  {
    TTerminal * FTerminal = FFileSystem->FTerminal;
    // Buffer for one block of data
    TFileBuffer BlockBuf;

    unsigned long BlockSize = GetBlockSize();
    bool Result = (BlockSize > 0);
    
    if (Result)
    {
      FILE_OPERATION_LOOP(FMTLOAD(READ_ERROR, (FFileName)),
        BlockBuf.LoadFile(FFile, BlockSize, false);
      );

      FEnd = (BlockBuf.Size == 0);
      Result = !FEnd;
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
          OperationProgress->ChangeTransferSize(OperationProgress->TransferSize -
            PrevBufSize + BlockBuf.Size);
        }

        Request->ChangeType(SSH_FXP_WRITE);
        Request->AddString(FHandle);
        Request->AddInt64(FTransfered);
        Request->AddData(BlockBuf.Data, BlockBuf.Size);
        FLastBlockSize = BlockBuf.Size;

        FTransfered += BlockBuf.Size;
      }
    }

    return Result;
  }

  virtual void __fastcall SendPacket(TSFTPQueuePacket * Packet)
  {
    TSFTPTransferQueue::SendPacket(Packet);
    OperationProgress->AddTransfered(FLastBlockSize);
  }

  inline int __fastcall GetBlockSize()
  {
    return FFileSystem->UploadBlockSize(FHandle, OperationProgress);
  }
  
  virtual bool __fastcall End(TSFTPPacket * /*Response*/)
  {
    return FEnd;
  }

private:
  HANDLE FFile;
  TFileOperationProgressType * OperationProgress;
  AnsiString FFileName;
  unsigned long FLastBlockSize;
  bool FEnd;
};
//---------------------------------------------------------------------------
#pragma warn .inl
//---------------------------------------------------------------------------
class TSFTPBusy
{
public:
  __fastcall TSFTPBusy(TSFTPFileSystem * FileSystem)
  {
    FFileSystem = FileSystem;
    assert(FFileSystem != NULL);
    FFileSystem->BusyStart();
  }

  __fastcall ~TSFTPBusy()
  {
    FFileSystem->BusyEnd();
  }

private:
  TSFTPFileSystem * FFileSystem;
};
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
  TOverwriteFileParams * FileParams;
};
//---------------------------------------------------------------------------
struct TSinkFileParams
{
  AnsiString TargetDir;
  const TCopyParamType * CopyParam;
  int Params;
  TFileOperationProgressType * OperationProgress;
  bool Skipped;
  unsigned int Flags;
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
  FUtfStrings = false;
  FSupport = new TSFTPSupport();
  FSupport->Extensions = new TStringList();
  FExtensions = new TStringList();
}
//---------------------------------------------------------------------------
__fastcall TSFTPFileSystem::~TSFTPFileSystem()
{
  delete FSupport->Extensions;
  delete FSupport;
  // there must be no valid packet reservation at the end
  for (int i = 0; i < FPacketReservations->Count; i++)
  {
    assert(FPacketReservations->Items[i] == NULL);
  }
  delete FPacketReservations;
  delete FExtensions;
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
    case fcAnyCommand:
    case fcHardLink:
    case fcRemoteCopy:
      return false;

    case fcModeChanging:
    case fcNewerOnlyUpload:
      return true;

    case fcRename:
      return (FVersion >= 2);

    case fcSymbolicLink:
    case fcResolveSymlink:
      return (FVersion >= 3);

    case fcOwnerChanging:
    case fcGroupChanging:
    case fcNativeTextMode:
      return (FVersion >= 4);

    case fcTextMode:
      return (FVersion >= 4) ||
        strcmp(GetEOL(), EOLToStr(FTerminal->Configuration->LocalEOLType)) != 0;

    case fcUserGroupListing:
      return SupportsExtension(SFTP_EXT_OWNER_GROUP);

    default:
      assert(false);
      return false;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TSFTPFileSystem::SupportsExtension(const AnsiString & Extension) const
{
  return (FSupport->Extensions->IndexOf(Extension) >= 0);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::KeepAlive()
{
  TSFTPPacket Packet(SSH_FXP_REALPATH);
  Packet.AddPathString("/", FVersion, FUtfStrings);
  SendPacketAndReceiveResponse(&Packet, &Packet);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::AdditionalInfo(TStrings * AdditionalInfo,
  bool Initial)
{
  if (Initial)
  {
    if (!IsCapable(fcRename))
    {
      AdditionalInfo->Add(LoadStr(FS_RENAME_NOT_SUPPORTED));
      AdditionalInfo->Add("");
    }

    if (FExtensions->Count > 0)
    {
      AnsiString Name;
      AnsiString Value;
      AnsiString Line;
      AdditionalInfo->Add(LoadStr(SFTP_EXTENSION_INFO));
      for (int Index = 0; Index < FExtensions->Count; Index++)
      {
        AnsiString Name = FExtensions->Names[Index];
        AnsiString Value = FExtensions->Values[Name];
        AnsiString Line;
        if (Value.IsEmpty())
        {
          Line = Name;
        }
        else
        {
          if (!IsDisplayableStr(Value))
          {
            Value = "0x" + StrToHex(Value);
          }
          Line = FORMAT("%s=%s", (Name, Value));
        }
        AdditionalInfo->Add(FORMAT("  %s", (Line)));;
      }
    }
    else
    {
      AdditionalInfo->Add(LoadStr(SFTP_NO_EXTENSION_INFO));
    }
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
  Params->Pos++;
  Params->Buffer[Params->Pos] = (wchar_t)Output;
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
struct TUnicodeEmitParams2
{
  AnsiString Buffer;
  int Pos;
  int Len;
};
//---------------------------------------------------------------------------
extern "C" void UnicodeEmit2(void * AParams, long int Output)
{
  if (Output == 0xFFFFL) // see Putty's charset\internal.h
  {
    throw Exception(LoadStr(DECODE_UTF_ERROR));
  }
  TUnicodeEmitParams2 * Params = (TUnicodeEmitParams2 *)AParams;
  if (Params->Pos >= Params->Len)
  {
    Params->Len += 50;
    Params->Buffer.SetLength(Params->Len);
  }
  Params->Pos++;
  Params->Buffer[Params->Pos] = (unsigned char)Output;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSFTPFileSystem::EncodeUTF(const WideString Source)
{
  charset_state State;
  wchar_t * Str;
  TUnicodeEmitParams2 Params;
  AnsiString Result;

  State.s0 = 0;
  Str = Source.c_bstr();
  Params.Pos = 0;
  Params.Len = Source.Length();
  Params.Buffer.SetLength(Params.Len);

  while (*Str)
  {
    write_utf8(NULL, (wchar_t)*Str, &State, UnicodeEmit2, &Params);
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
unsigned long __fastcall TSFTPFileSystem::TransferBlockSize(unsigned long Overhead,
  TFileOperationProgressType * OperationProgress, unsigned long MaxPacketSize)
{
  const unsigned long MinPacketSize = 4096;
  // size + message number + type
  const unsigned long SFTPPacketOverhead = 4 + 4 + 1;
  unsigned long AMaxPacketSize = FTerminal->MaxPacketSize();
  unsigned long Result = OperationProgress->CPS();

  if ((MaxPacketSize > 0) && (MaxPacketSize < AMaxPacketSize))
  {
    AMaxPacketSize = MaxPacketSize;
  }
  
  if (Result == 0)
  {
    Result = OperationProgress->StaticBlockSize();
  }
  
  if (Result < MinPacketSize)
  {
    Result = MinPacketSize;
  }
  
  if (AMaxPacketSize > 0)
  {
    AMaxPacketSize -= SFTPPacketOverhead + Overhead;
    if (Result > AMaxPacketSize)
    {
      Result = AMaxPacketSize;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned long __fastcall TSFTPFileSystem::UploadBlockSize(const AnsiString & Handle,
  TFileOperationProgressType * OperationProgress)
{
  // handle length + offset + data size
  const unsigned long UploadPacketOverhead = 
    sizeof(unsigned long) + sizeof(__int64) + sizeof(unsigned long);
  int RemainingSendBuffer = FTerminal->RemainingSendBuffer();
  unsigned long Result;
  if (RemainingSendBuffer <= 0)
  {
    Result = 0;
  }
  else
  {
    Result = TransferBlockSize(UploadPacketOverhead + Handle.Length(), OperationProgress,
      RemainingSendBuffer);
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned long __fastcall TSFTPFileSystem::DownloadBlockSize(
  TFileOperationProgressType * OperationProgress)
{
  return TransferBlockSize(sizeof(unsigned long), OperationProgress);
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
          (Packet->Type != FPreviousLoggedPacket) ||
          FTerminal->Configuration->LogProtocol)
      {
        if (FNotLoggedPackets)
        {
          FTerminal->LogEvent(FORMAT("%d skipped SSH_FXP_WRITE, SSH_FXP_READ, SSH_FXP_DATA and SSH_FXP_STATUS packets.",
            (FNotLoggedPackets)));
          FNotLoggedPackets = 0;
        }
        FTerminal->Log->Add(llInput, FORMAT("Type: %s, Size: %d, Number: %d",
          (Packet->TypeName, (int)Packet->Length, (int)Packet->MessageNumber)));
        if (FTerminal->Configuration->LogProtocol)
        {
          FTerminal->Log->Add(llInput, Packet->Dump());
        }
        FPreviousLoggedPacket = Packet->Type;
      }
      else
      {
        FNotLoggedPackets++;
      }
    }
    FTerminal->Send(Packet->SendData, Packet->SendLength);
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
    SFTP_STATUS_NO_SPACE_ON_FILESYSTEM,
    SFTP_STATUS_QUOTA_EXCEEDED,
    SFTP_STATUS_UNKNOWN_PRINCIPLE
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
    AnsiString AdditionalInfo;
    if (FVersion >= 3)
    {
      // message is in UTF only since SFTP specification 01 (specification 00 
      // is also version 3)
      ServerMessage = Packet->GetString(FUtfStrings);
      LanguageTag = Packet->GetString();
      if ((FVersion >= 5) && (Message == SFTP_STATUS_UNKNOWN_PRINCIPLE))
      {
        while (Packet->NextData != NULL)
        {
          if (!AdditionalInfo.IsEmpty())
          {
            AdditionalInfo += ", ";
          }
          AdditionalInfo += Packet->GetString();
        }
      }
    }
    else
    {
      ServerMessage = LoadStr(SFTP_SERVER_MESSAGE_UNSUPPORTED);
    }
    if (FTerminal->IsLogging())
    {
      FTerminal->Log->Add(llOutput, FORMAT("Status/error code: %d, Message: %d, Server: %s, Language: %s ",
        (int(Code), (int)Packet->MessageNumber, ServerMessage, LanguageTag)));
    }
    if (!LanguageTag.IsEmpty())
    {
      LanguageTag = FORMAT("(%s)", (LanguageTag));
    }
    AnsiString Error = FMTLOAD(SFTP_ERROR_FORMAT, (LoadStr(Message) + AdditionalInfo,
      int(Code), ServerMessage, LanguageTag, int(Packet->RequestType)));
    FTerminal->TerminalError(NULL, Error);
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
  TSFTPBusy Busy(this);

  int Result = SSH_FX_OK;
  int Reservation = FPacketReservations->IndexOf(Packet);

  if (Reservation < 0 || Packet->Capacity == 0)
  {
    bool IsReserved;
    do
    {
      IsReserved = false;

      assert(Packet);
      char LenBuf[5];
      LenBuf[sizeof(LenBuf) - 1] = '\0';
      FTerminal->Receive(LenBuf, sizeof(LenBuf) - 1);
      int Length = GET_32BIT(LenBuf);
      if (Length > SFTP_MAX_PACKET_LEN)
      {
        AnsiString Message = FMTLOAD(SFTP_PACKET_TOO_BIG, (
          int(Length), SFTP_MAX_PACKET_LEN));
        if (ExpectedType == SSH_FXP_VERSION)
        {
          AnsiString LenString = LenBuf;
          if (!IsDisplayableStr(LenString))
          {
            LenString = "0x" + StrToHex(LenString);
          }
          Message = FMTLOAD(SFTP_PACKET_TOO_BIG_INIT_EXPLAIN,
            (Message, LenString));
        }
        FTerminal->FatalError(Message);
      }
      Packet->Capacity = Length;
      FTerminal->Receive(Packet->Data, Length);
      Packet->DataUpdated(Length);

      if (FTerminal->IsLogging())
      {
        if ((FPreviousLoggedPacket != SSH_FXP_READ &&
             FPreviousLoggedPacket != SSH_FXP_WRITE) ||
            (Packet->Type != SSH_FXP_STATUS && Packet->Type != SSH_FXP_DATA) ||
            FTerminal->Configuration->LogProtocol)
        {
          if (FNotLoggedPackets)
          {
            FTerminal->LogEvent(FORMAT("%d skipped SSH_FXP_WRITE, SSH_FXP_READ, SSH_FXP_DATA and SSH_FXP_STATUS packets.",
              (FNotLoggedPackets)));
            FNotLoggedPackets = 0;
          }
          FTerminal->Log->Add(llOutput, FORMAT("Type: %s, Size: %d, Number: %d",
            (Packet->TypeName, (int)Packet->Length, (int)Packet->MessageNumber)));
          if (FTerminal->Configuration->LogProtocol)
          {
            FTerminal->Log->Add(llOutput, Packet->Dump());
          }
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
              FTerminal->LogEvent("Storing reserved response");
              *ReservedPacket = *Packet;
            }
            else
            {
              FTerminal->LogEvent("Discarding reserved response");
              RemoveReservation(Index);
            }
            break;
          }
        }
      }
    }
    while (IsReserved);
  }

  // before we removed the reservation after check for packet type,
  // but if it raises exception, removal is unnecessarily
  // postponed until the packet is removed
  // (and it have not worked anyway until recent fix to UnreserveResponse)
  if (Reservation >= 0)
  {
    // order might have changed, when reserved, but no longer needed packet
    // was received in above loop
    Reservation = FPacketReservations->IndexOf(Packet);
    assert(Reservation >= 0);
    assert(Packet->MessageNumber == (unsigned long)FPacketNumbers.GetElement(Reservation));
    RemoveReservation(Reservation);
  }

  if (ExpectedType >= 0)
  {
    if (Packet->Type == SSH_FXP_STATUS)
    {
      if (AllowStatus < 0)
      {
        AllowStatus = (ExpectedType == SSH_FXP_STATUS ? asOK : asNo);
      }
      Result = GotStatusPacket(Packet, AllowStatus);
    }
    else if (ExpectedType != Packet->Type)
    {
      FTerminal->FatalError(FMTLOAD(SFTP_INVALID_TYPE, ((int)Packet->Type)));
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ReserveResponse(const TSFTPPacket * Packet,
  TSFTPPacket * Response)
{
  if (Response != NULL)
  {
    assert(FPacketReservations->IndexOf(Response) < 0);
    // mark response as not received yet
    Response->Capacity = 0;
    Response->ReservedBy = this;
  }
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
  if (Response->Capacity != 0)
  {
    // added check for already received packet
    // (it happens when the reserved response is received out of order,
    // unexpectedly soon, and then receivepacket() on the packet
    // is not actually called, due to exception)
    RemoveReservation(Reservation);
  }
  else
  {
    if (Reservation >= 0)
    {
      // we probably do not remove the item at all, because
      // we must remember that the respose was expected, so we skip it
      // in receivepacket()
      FPacketReservations->Items[Reservation] = NULL;
    }
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
  TSFTPBusy Busy(this);
  SendPacket(Packet);
  Result = ReceiveResponse(Packet, Response, ExpectedType, AllowStatus);
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSFTPFileSystem::RealPath(const AnsiString Path)
{
  try
  {
    FTerminal->LogEvent(FORMAT("Getting real path for '%s'",
      (Path)));

    TSFTPPacket Packet(SSH_FXP_REALPATH);
    Packet.AddPathString(Path, FVersion, FUtfStrings);
    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_NAME);
    if (Packet.GetCardinal() != 1)
    {
      FTerminal->FatalError(LoadStr(SFTP_NON_ONE_FXP_NAME_PACKET));
    }

    AnsiString RealDir = Packet.GetPathString(FVersion, FUtfStrings);
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

  if (TTerminal::IsAbsolutePath(Path))
  {
    APath = Path;
  }
  else
  {
    if (!Path.IsEmpty())
    {
      // this condition/block was outside (before) current block
      // but it dod not work when Path was empty
      if (!BaseDir.IsEmpty())
      {
        APath = UnixIncludeTrailingBackslash(BaseDir);
      }
      APath = APath + Path;
    }
    if (APath.IsEmpty()) APath = UnixIncludeTrailingBackslash(".");
  }
  return RealPath(APath);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSFTPFileSystem::LocalCanonify(const AnsiString & Path)
{
  // TODO: improve (handle .. etc.)
  if (TTerminal::IsAbsolutePath(Path)) return Path;
    else
  {
    return UnixIncludeTrailingBackslash(FCurrentDirectory) + Path;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSFTPFileSystem::Canonify(AnsiString Path)
{
  // inspired by canonify() from PSFTP.C
  AnsiString Result;
  FTerminal->LogEvent(FORMAT("Canonifying: \"%s\"", (Path)));
  Path = LocalCanonify(Path);
  bool TryParent = false;
  try
  {
    Result = RealPath(Path);
  }
  catch(...)
  {
    if (FTerminal->Active)
    {
      TryParent = true;
    }
    else
    {
      throw;
    }
  }

  if (TryParent)
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

  FTerminal->LogEvent(FORMAT("Canonified: \"%s\"", (Result)));

  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSFTPFileSystem::AbsolutePath(AnsiString Path)
{
  return RealPath(Path, CurrentDirectory);
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
  TRemoteFile * ALinkedByFile, const AnsiString FileName)
{
  TRemoteFile * File = new TRemoteFile(ALinkedByFile);
  try
  {
    File->Terminal = FTerminal;
    if (!FileName.IsEmpty())
    {
      File->FileName = FileName;
    }
    Packet->GetFile(File, FVersion, FTerminal->SessionData->ConsiderDST, FUtfStrings);
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
  int MaxVersion = FTerminal->SessionData->SFTPMaxVersion;
  if (MaxVersion > SFTPMaxVersion)
  {
    MaxVersion = SFTPMaxVersion;
  }
  Packet.AddCardinal(MaxVersion);

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
    AnsiString ExtensionDisplayData;
    while (Packet.NextData)
    {
      ExtensionName = Packet.GetString();
      ExtensionData = Packet.GetString();
      ExtensionDisplayData = IsDisplayableStr(ExtensionData) ? ExtensionData :
        "0x" + StrToHex(ExtensionData);
      if (ExtensionName == SFTP_EXT_NEWLINE)
      {
        FEOL = ExtensionData;
        FTerminal->LogEvent(FORMAT("Server requests EOL sequence %s.",
          (ExtensionDisplayData)));
        if (FEOL.Length() < 1 || FEOL.Length() > 2)
        {
          FTerminal->FatalError(FMTLOAD(SFTP_INVALID_EOL, (ExtensionDisplayData)));
        }
      }
      else if (ExtensionName == SFTP_EXT_SUPPORTED)
      {
        TSFTPPacket SupportedStruct(ExtensionData);
        FSupport->AttributeMask = SupportedStruct.GetCardinal();
        FSupport->AttributeBits = SupportedStruct.GetCardinal();
        FSupport->OpenFlags = SupportedStruct.GetCardinal();
        FSupport->AccessMask = SupportedStruct.GetCardinal();
        FSupport->MaxReadSize = SupportedStruct.GetCardinal();
        AnsiString Extension;
        FSupport->Extensions->Clear();
        while (SupportedStruct.NextData)
        {
          Extension = SupportedStruct.GetString();
          FSupport->Extensions->Add(Extension);
        }

        if (FTerminal->IsLogging())
        {
          FTerminal->LogEvent(FORMAT(
            "Server support information:\n"
            "  Attribute mask: %s, Attribute bits: %s, Open flags: %s\n"
            "  Access mask: %s, Max read size: %s\n"
            "  Extensions (%d)\n",
            (IntToHex(__int64(FSupport->AttributeMask), 4),
             IntToHex(__int64(FSupport->AttributeBits), 4),
             IntToHex(__int64(FSupport->OpenFlags), 4),
             IntToHex(__int64(FSupport->AccessMask), 4),
             IntToStr(__int64(FSupport->MaxReadSize)),
             FSupport->Extensions->Count)));
          for (int Index = 0; Index < FSupport->Extensions->Count; Index++)
          {
            FTerminal->LogEvent(
              FORMAT("    %s", (FSupport->Extensions->Strings[Index])));
          }
        }
      }
      else
      {
        FTerminal->LogEvent(FORMAT("Unknown server extension %s=%s",
          (ExtensionName, ExtensionDisplayData)));
      }
      FExtensions->Values[ExtensionName] = ExtensionData;
    }

    Packet.ChangeType(SSH_FXP_EXTENDED);
    Packet.AddString(SFTP_EXT_WINSCP);
    SendPacket(&Packet);
    ReserveResponse(&Packet, &Packet);

    TSFTPPacket PacketSoftware(SSH_FXP_EXTENDED);
    bool SoftwareExt = SupportsExtension(SFTP_EXT_SOFTWARE);
    if (SoftwareExt)
    {
      PacketSoftware.AddString(SFTP_EXT_SOFTWARE);
      PacketSoftware.AddString(FTerminal->Configuration->ProductName);
      PacketSoftware.AddString(FTerminal->Configuration->ProductVersion);
      PacketSoftware.AddString(FTerminal->Configuration->CompanyName);
      PacketSoftware.AddString(SFTP_EXT_SOFTWARE_OS);
      PacketSoftware.AddString(FTerminal->Configuration->OSVersionStr);
      SendPacket(&PacketSoftware);
      ReserveResponse(&PacketSoftware, &PacketSoftware);
    }

    int Status = ReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS,
      asOK | asOpUnsupported);
    if (Status == SSH_FX_OK)
    {
      FTerminal->LogEvent("Server recognises WinSCP.");
    }
    else
    {
      FTerminal->LogEvent("Server does not recognise WinSCP.");
    }

    if (SoftwareExt)
    {
      ReceiveResponse(&PacketSoftware, &PacketSoftware, SSH_FXP_EXTENDED_REPLY,
        asOpUnsupported);
      if ((PacketSoftware.Type != SSH_FXP_EXTENDED_REPLY) ||
          (PacketSoftware.GetString() != SFTP_EXT_SOFTWARE))
      {
        FTerminal->LogEvent(FORMAT("Invalid response to %s", (SFTP_EXT_SOFTWARE)));
      }
      else
      {
        AnsiString Software(PacketSoftware.GetString());
        AnsiString Vendor(PacketSoftware.GetString());
        AnsiString Version(PacketSoftware.GetString());
        AnsiString OS(PacketSoftware.GetString());
        AnsiString OSVersion(PacketSoftware.GetString());

        FTerminal->LogEvent(FORMAT("Server software: %s %s by %s",
          (Software, Version, Vendor)));
        FTerminal->LogEvent(FORMAT("Server OS: %s %s",
          (OS, OSVersion)));
      }
    }
  }

  if (FVersion >= 4)
  {
    FUtfStrings = (FTerminal->SessionData->SFTPBug[sbUtf] == asOff) ||
      ((FTerminal->SessionData->SFTPBug[sbUtf] == asAuto) &&
        (FTerminal->SshImplementation.Pos("Foxit-WAC-Server") != 1));
    if (FUtfStrings)
    {
      FTerminal->LogEvent("We will use UTF-8 strings when appropriate");
    }
    else
    {
      FTerminal->LogEvent("We believe the server has SFTP UTF-8 bug");
    }
  }
  else
  {
    FUtfStrings = false;
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
void __fastcall TSFTPFileSystem::LookupUsersGroups()
{
  assert(SupportsExtension(SFTP_EXT_OWNER_GROUP));

  TSFTPPacket PacketOwners(SSH_FXP_EXTENDED);
  TSFTPPacket PacketGroups(SSH_FXP_EXTENDED);
  TSFTPPacket * Packet;
  TUsersGroupsList * List;

  TSFTPPacket * Packets[] = { &PacketOwners, &PacketGroups };
  TUsersGroupsList * Lists[] = { FTerminal->FUsers, FTerminal->FGroups };
  char ListTypes[] = { OGQ_LIST_OWNERS, OGQ_LIST_GROUPS };

  for (int Index = 0; Index < LENOF(Packets); Index++)
  {
    Packet = Packets[Index];
    Packet->AddString(SFTP_EXT_OWNER_GROUP);
    Packet->AddByte(ListTypes[Index]);
    SendPacket(Packet);
    ReserveResponse(Packet, Packet);
  }

  for (int Index = 0; Index < LENOF(Packets); Index++)
  {
    Packet = Packets[Index];

    ReceiveResponse(Packet, Packet, SSH_FXP_EXTENDED_REPLY, asOpUnsupported);

    if ((Packet->Type != SSH_FXP_EXTENDED_REPLY) ||
        (Packet->GetString() != SFTP_EXT_OWNER_GROUP_REPLY))
    {
      FTerminal->LogEvent(FORMAT("Invalid response to %s", (SFTP_EXT_OWNER_GROUP)));
    }
    else
    {
      List = Lists[Index];
      unsigned long Count = Packet->GetCardinal();

      List->BeginUpdate();
      try
      {
        List->Clear();
        for (unsigned long Item = 0; Item < Count; Item++)
        {
          List->Add(Packet->GetUtfString());
        }
      }
      __finally
      {
        List->EndUpdate();
      }
    }
  }
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
  TRemoteFile * File;
  ReadFile(Directory, File);
  delete File;
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
void __fastcall TSFTPFileSystem::CachedChangeDirectory(const AnsiString Directory)
{
  FDirectoryToChangeTo = Directory;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ReadDirectory(TRemoteFileList * FileList)
{
  assert(FileList && !FileList->Directory.IsEmpty());

  AnsiString Directory;
  Directory = UnixExcludeTrailingBackslash(LocalCanonify(FileList->Directory));
  FTerminal->LogEvent(FORMAT("Listing directory \"%s\".", (Directory)));

  // moved before SSH_FXP_OPENDIR, so directory listing does not retain
  // old data (e.g. parent directory) when reading fails
  FileList->Clear();

  TSFTPPacket Packet(SSH_FXP_OPENDIR);
  AnsiString Handle;

  try
  {
    Packet.AddPathString(Directory, FVersion, FUtfStrings);

    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_HANDLE);

    Handle = Packet.GetString();
  }
  catch(...)
  {
    if (FTerminal->Active)
    {
      FileList->AddFile(new TRemoteParentDirectory());
    }
    throw;
  }

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
          File = LoadFile(&ListingPacket, NULL, "");
          // security fix
          if (((File->FileName.Length() > 2) && IsDots(File->FileName)) || 
              (File->FileName.Pos("/") > 0) || (File->FileName.Pos("\\") > 0))
          {
            FTerminal->LogEvent(FORMAT("Ignored suspicious file '%s'", (File->FileName))); 
            delete File;
          }
          else
          {
            FileList->AddFile(File);

            Total++;
          }

          if (Total % 10 == 0)
          {
            FTerminal->DoReadDirectoryProgress(Total);
          }
        }

        if (Count == 0)
        {
          FTerminal->LogEvent("Empty directory listing packet. Aborting directory reading."); 
          isEOF = true;
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
        File = new TRemoteParentDirectory();
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
      SendPacket(&Packet);
      // we are not interested in the response, do not wait for it
      ReserveResponse(&Packet, NULL);
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
  ReadLinkPacket.AddPathString(FileName, FVersion, FUtfStrings);
  SendPacket(&ReadLinkPacket);
  ReserveResponse(&ReadLinkPacket, &ReadLinkPacket);

  // send second request before reading response to first one
  // (performance benefit)
  TSFTPPacket AttrsPacket(SSH_FXP_STAT);
  AttrsPacket.AddPathString(FileName, FVersion, FUtfStrings);
  if (FVersion >= 4)
  {
    AttrsPacket.AddCardinal(SSH_FILEXFER_ATTR_COMMON);
  }
  SendPacket(&AttrsPacket);
  ReserveResponse(&AttrsPacket, &AttrsPacket);

  ReceiveResponse(&ReadLinkPacket, &ReadLinkPacket, SSH_FXP_NAME);
  if (ReadLinkPacket.GetCardinal() != 1)
  {
    FTerminal->FatalError(LoadStr(SFTP_NON_ONE_FXP_NAME_PACKET));
  }
  SymlinkFile->LinkTo = ReadLinkPacket.GetPathString(FVersion, FUtfStrings);

  ReceiveResponse(&AttrsPacket, &AttrsPacket, SSH_FXP_ATTRS);
  // SymlinkFile->FileName was used instead SymlinkFile->LinkTo before, why?
  File = LoadFile(&AttrsPacket, SymlinkFile,
    UnixExtractFileName(SymlinkFile->LinkTo));
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ReadFile(const AnsiString FileName,
  TRemoteFile *& File)
{
  CustomReadFile(FileName, File, SSH_FXP_LSTAT);
}
//---------------------------------------------------------------------------
bool __fastcall TSFTPFileSystem::RemoteFileExists(const AnsiString FullPath,
  TRemoteFile ** File)
{
  bool Result;
  try
  {
    TRemoteFile * AFile;
    CustomReadFile(FullPath, AFile, SSH_FXP_LSTAT, NULL, true);
    Result = (AFile != NULL);
    if (Result)
    {
      if (File)
      {
        *File = AFile;
      }
      else
      {
        delete AFile;
      }
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
  TRemoteFile *& File, char Type, TRemoteFile * ALinkedByFile,
  bool AllowNonexistence)
{
  TSFTPPacket Packet(Type);

  Packet.AddPathString(LocalCanonify(FileName), FVersion, FUtfStrings);
  if (FVersion >= 4)
  {
    Packet.AddCardinal(SSH_FILEXFER_ATTR_SIZE | SSH_FILEXFER_ATTR_PERMISSIONS |
      SSH_FILEXFER_ATTR_ACCESSTIME | SSH_FILEXFER_ATTR_MODIFYTIME |
      SSH_FILEXFER_ATTR_OWNERGROUP);
  }
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_ATTRS,
    AllowNonexistence ? asNoSuchFile : -1);

  if (Packet.Type == SSH_FXP_ATTRS)
  {
    File = LoadFile(&Packet, ALinkedByFile, UnixExtractFileName(FileName));
  }
  else
  {
    assert(AllowNonexistence);
    File = NULL;
  }
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
  Packet.AddPathString(RealFileName, FVersion, FUtfStrings);
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::RenameFile(const AnsiString FileName,
  const AnsiString NewName)
{
  TSFTPPacket Packet(SSH_FXP_RENAME);
  AnsiString RealName = LocalCanonify(FileName);
  Packet.AddPathString(RealName, FVersion, FUtfStrings);
  AnsiString TargetName;
  if (UnixExtractFilePath(NewName).IsEmpty())
  {
    // rename case (TTerminal::RenameFile)
    TargetName = UnixExtractFilePath(RealName) + NewName;
  }
  else
  {
    TargetName = LocalCanonify(NewName);
  }
  Packet.AddPathString(TargetName, FVersion, FUtfStrings);
  if (FVersion >= 5)
  {
    Packet.AddCardinal(0);
  }
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CopyFile(const AnsiString /*FileName*/,
  const AnsiString /*NewName*/)
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CreateDirectory(const AnsiString DirName,
  const TRemoteProperties * Properties)
{
  TSFTPPacket Packet(SSH_FXP_MKDIR);
  AnsiString CanonifiedName = Canonify(DirName);
  Packet.AddPathString(CanonifiedName, FVersion, FUtfStrings);
  Packet.AddProperties(NULL, 0, true, FVersion, FUtfStrings);
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);

  // explicitly set permissions after directory creation,
  // permissions specified in SSH_FXP_MKDIR are ignored at least by OpenSSH
  Packet.ChangeType(SSH_FXP_SETSTAT);
  Packet.AddPathString(CanonifiedName, FVersion, FUtfStrings);
  Packet.AddProperties(Properties, 0, true, FVersion, FUtfStrings);
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CreateLink(const AnsiString FileName,
  const AnsiString PointTo, bool Symbolic)
{
  USEDPARAM(Symbolic);
  assert(Symbolic); // only symlinks are supported by SFTP
  assert(FVersion >= 3); // symlinks are supported with SFTP version 3 and later
  TSFTPPacket Packet(SSH_FXP_SYMLINK);

  bool Buggy = (FTerminal->SessionData->SFTPBug[sbSymlink] == asOn) ||
    ((FTerminal->SessionData->SFTPBug[sbSymlink] == asAuto) &&
      (FTerminal->SshImplementation.Pos("OpenSSH") == 1));

  if (!Buggy)
  {
    Packet.AddPathString(Canonify(FileName), FVersion, FUtfStrings);
    Packet.AddPathString(PointTo, FVersion, FUtfStrings);
  }
  else
  {
    FTerminal->LogEvent("We believe the server has SFTP symlink bug");
    Packet.AddPathString(PointTo, FVersion, FUtfStrings);
    Packet.AddPathString(Canonify(FileName), FVersion, FUtfStrings);
  }
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

    if (File->IsDirectory && !File->IsSymLink && Properties->Recursive)
    {
      FTerminal->ProcessDirectory(FileName, FTerminal->ChangeFileProperties,
        (void*)Properties);
    }

    TSFTPPacket Packet(SSH_FXP_SETSTAT);
    Packet.AddPathString(RealFileName, FVersion, FUtfStrings);
    Packet.AddProperties(Properties, *File->Rights, File->IsDirectory, FVersion, FUtfStrings);
    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
  }
  __finally
  {
    delete File;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CustomCommandOnFile(const AnsiString /*FileName*/,
    const TRemoteFile * /*File*/, AnsiString /*Command*/, int /*Params*/, 
    TLogAddLineEvent /*OutputEvent*/)
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::AnyCommand(const AnsiString /*Command*/)
{
  assert(false);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSFTPFileSystem::FileUrl(const AnsiString FileName)
{
  assert(FileName.Length() > 0);
  return AnsiString("sftp://") + FTerminal->SessionData->SessionName +
    (FileName[1] == '/' ? "" : "/") + FileName;
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
        SFTPSource(FileName, FullTargetDir, CopyParam, Params, OperationProgress,
          tfFirstLevel);
        Success = true;
      }
      catch(EScpSkipFile & E)
      {
        SUSPEND_OPERATION (
          if (!FTerminal->HandleException(&E)) throw;
        );
      }
    }
    __finally
    {
      FAvoidBusy = false;
      OperationProgress->Finish(FileName, Success, DisconnectWhenComplete);
    }
    Index++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SFTPConfirmOverwrite(const AnsiString FileName,
  bool TargetBiggerThanSource, TFileOperationProgressType * OperationProgress,
  TSFTPOverwriteMode & OverwriteMode, const TOverwriteFileParams * FileParams)
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
      int Answers = qaYes | qaNo | qaAbort | qaYesToAll | qaNoToAll | qaAll;
      if ((FVersion < 4) || !OperationProgress->AsciiTransfer)
      {
        Answers |= qaRetry;
      }
      TQueryButtonAlias Aliases[2];
      Aliases[0].Button = qaRetry;
      Aliases[0].Alias = LoadStr(APPEND_BUTTON);
      Aliases[1].Button = qaAll;
      Aliases[1].Alias = LoadStr(YES_TO_NEWER_BUTTON);
      TQueryParams Params(qpNeverAskAgainCheck);
      Params.Aliases = Aliases;
      Params.AliasesCount = LENOF(Aliases);
      Answer = FTerminal->ConfirmFileOverwrite(FileName, FileParams,
        Answers, &Params,
        OperationProgress->Side == osLocal ? osRemote : osLocal,
        OperationProgress);
    );

    if (Answer == qaRetry)
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
      switch (Answer)
      {
        case qaAbort:
          if (!OperationProgress->Cancel)
          {
            OperationProgress->Cancel = csCancel;
          }
          Abort();
          break;

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
      TQueryParams Params(qpAllowContinueOnError);
      Answer = FTerminal->DoQueryUser(
        FMTLOAD(PARTIAL_BIGGER_THAN_SOURCE, (DestFileName)), NULL,
          qaOK | qaAbort, &Params, qtWarning);
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
  else if (FTerminal->Configuration->ConfirmResume)
  {
    int Answer;
    SUSPEND_OPERATION
    (
      TQueryParams Params(qpAllowContinueOnError | qpNeverAskAgainCheck);
      Answer = FTerminal->DoQueryUser(
        FMTLOAD(RESUME_TRANSFER, (DestFileName)), qaYes | qaNo | qaAbort,
        &Params);
    );

    switch (Answer) {
      case qaNeverAskAgain:
        FTerminal->Configuration->ConfirmResume = false;
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
  else
  {
    ResumeTransfer = true;
  }
  return ResumeTransfer;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SFTPSource(const AnsiString FileName,
  const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  AnsiString OnlyFileName = ExtractFileName(FileName);

  if (FLAGCLEAR(Params, cpDelete) &&
      !CopyParam->AllowTransfer(OnlyFileName))
  {
    FTerminal->LogEvent(FORMAT("File \"%s\" excluded from transfer", (FileName)));
    THROW_SKIP_FILE_NULL;
  }

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
      OpenParams.LocalFileAttrs, CopyParam, Params, OperationProgress, Flags);
  }
  else
  {
    try
    {
      // File is regular file (not directory)
      assert(File);

      AnsiString DestFileName = CopyParam->ChangeFileName(OnlyFileName,
        osLocal, FLAGSET(Flags, tfFirstLevel));
      AnsiString DestFullName = LocalCanonify(TargetDir + DestFileName);
      AnsiString DestPartinalFullName;
      bool ResumeAllowed;
      bool ResumeTransfer = false;
      bool DestFileExists = false;
      TRights DestRights;

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
        FLAGCLEAR(Flags, tfNewDirectory) &&
        CopyParam->AllowResume(OperationProgress->LocalSize) &&
        IsCapable(fcRename);
      OperationProgress->SetResumeStatus(ResumeAllowed ? rsEnabled : rsDisabled);

      TOverwriteFileParams FileParams;
      FileParams.SourceSize = OperationProgress->LocalSize;
      FileParams.SourceTimestamp = UnixToDateTime(MTime,
        FTerminal->SessionData->ConsiderDST);

      if (ResumeAllowed)
      {
        DestPartinalFullName = DestFullName + FTerminal->Configuration->PartialExt;

        FTerminal->LogEvent("Checking existence of file.");
        TRemoteFile * File = new TRemoteFile();
        DestFileExists = RemoteFileExists(DestFullName, &File);
        if (DestFileExists)
        {
          OpenParams.DestFileSize = File->Size;
          FileParams.DestSize = OpenParams.DestFileSize;
          FileParams.DestTimestamp = File->Modification;
          DestRights = *File->Rights;
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
          if (DestFileExists &&
              (FLAGSET(Params, cpNewerOnly) ||
               (FTerminal->Configuration->ConfirmOverwriting &&
                !OperationProgress->YesToAll && FLAGCLEAR(Params, cpNoConfirmation))))
          {
            SFTPConfirmOverwrite(DestFileName,
              OpenParams.DestFileSize >= OperationProgress->LocalSize,
              OperationProgress, OpenParams.OverwriteMode, &FileParams);
          }
        }
      }

      bool DoResume = (ResumeAllowed && (OpenParams.OverwriteMode == omOverwrite));
      OpenParams.RemoteFileName = DoResume ? DestPartinalFullName : DestFullName;
      OpenParams.Resume = DoResume;
      OpenParams.OperationProgress = OperationProgress;
      OpenParams.CopyParam = CopyParam;
      OpenParams.Params = Params;
      OpenParams.FileParams = &FileParams;

      FTerminal->LogEvent("Opening remote file.");
      FTerminal->FileOperationLoop(SFTPOpenRemote, OperationProgress, true,
        FMTLOAD(SFTP_CREATE_FILE_ERROR, (OpenParams.RemoteFileName)),
        &OpenParams);

      bool TransferFinished = false;
      __int64 DestWriteOffset = 0;
      TSFTPPacket CloseRequest;
      bool SetRights = ((DoResume && DestFileExists) || CopyParam->PreserveRights);
      bool SetProperties = (CopyParam->PreserveTime || SetRights);
      TSFTPPacket PropertiesRequest(SSH_FXP_SETSTAT);
      TSFTPPacket PropertiesResponse;
      if (SetProperties)
      {
        PropertiesRequest.AddPathString(DestFullName, FVersion, FUtfStrings);
        unsigned short Rights = 0;
        if (CopyParam->PreserveRights)
        {
          Rights = CopyParam->RemoteFileRights(OpenParams.LocalFileAttrs);
        }
        else if (DoResume && DestFileExists)
        {
          Rights = DestRights.NumberSet;
        }

        PropertiesRequest.AddProperties(
          SetRights ? &Rights : NULL, NULL, NULL,
          CopyParam->PreserveTime ? &MTime : NULL,
          CopyParam->PreserveTime ? &ATime : NULL,
          NULL, false, FVersion, FUtfStrings);
      }

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
          // no need to limit upload queue len by file size, the queue
          // will take care of it itself
          int QueueLen = FTerminal->SessionData->SFTPUploadQueue;

          bool Initialized;
          Initialized = Queue.Init(QueueLen, FileName, File, OperationProgress,
            OpenParams.RemoteFileHandle,
            DestWriteOffset + OperationProgress->TransferedSize);

          if (Initialized)
          {
            while (Queue.Next(SSH_FXP_STATUS))
            {
              if (OperationProgress->Cancel)
              {
                Abort();
              }
            }
            // send close request before waiting for pending read responses
            SFTPCloseRemote(OpenParams.RemoteFileHandle, DestFileName,
              OperationProgress, false, true, &CloseRequest);
            OpenParams.RemoteFileHandle = "";

            // when resuming is disabled, we can send "set properties"
            // request before waiting for pending read/close responses
            if (SetProperties && !DoResume)
            {
              SendPacket(&PropertiesRequest);
              ReserveResponse(&PropertiesRequest, &PropertiesResponse);
            }
          }
        }

        TransferFinished = true;
      }
      __finally
      {
        if (FTerminal->Active)
        {
          // if file transfer was finished, the close request was already sent
          if (!OpenParams.RemoteFileHandle.IsEmpty())
          {
            SFTPCloseRemote(OpenParams.RemoteFileHandle, DestFileName,
              OperationProgress, TransferFinished, true, &CloseRequest);
          }
          // wait for the response
          SFTPCloseRemote(OpenParams.RemoteFileHandle, DestFileName,
            OperationProgress, TransferFinished, false, &CloseRequest);

          // delete file if transfer was not completed and resuming is not allowed
          if (!TransferFinished && !DoResume)
          {
            DeleteFile(OpenParams.RemoteFileName);
          }
        }
      }

      // originally this was before CLOSE (last __finally statement),
      // on VShell it failed
      if (DoResume)
      {
        FILE_OPERATION_LOOP(FMTLOAD(RENAME_AFTER_RESUME_ERROR,
            (UnixExtractFileName(OpenParams.RemoteFileName), DestFileName)),

          if (DestFileExists)
          {
            DeleteFile(DestFullName);
            // DestFileExists used to be set to false here. Had it any reason?
          }
          RenameFile(OpenParams.RemoteFileName, DestFileName);
        );
      }

      if (SetProperties)
      {
        // when resuming is enabled, the set properties request was not sent yet
        if (DoResume)
        {
          SendPacket(&PropertiesRequest);
        }
        bool Resend = false;
        FILE_OPERATION_LOOP(FMTLOAD(PRESERVE_TIME_PERM_ERROR, (DestFileName)),
          TSFTPPacket DummyResponse;
          TSFTPPacket * Response = &PropertiesResponse;
          if (Resend)
          {
            PropertiesRequest.Reuse();
            SendPacket(&PropertiesRequest);
            // ReceiveResponse currently cannot receive twice into same packet,
            // so DummyResponse is temporary workaround
            Response = &DummyResponse;
          }
          Resend = true;
          ReceiveResponse(&PropertiesRequest, Response, SSH_FXP_STATUS);
        );
      }
    }
    __finally
    {
      CloseHandle(File);
    }
  }

  /* TODO : Delete also read-only files. */
  /* TODO : Show error message on failure. */
  if (FLAGSET(Params, cpDelete)) 
  {
    Sysutils::DeleteFile(FileName);
  }
  else if (CopyParam->ClearArchive && FLAGSET(OpenParams.LocalFileAttrs, faArchive))
  {
    FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (FileName)),
      THROWIFFALSE(FileSetAttr(FileName, OpenParams.LocalFileAttrs & ~faArchive) == 0);
    )
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSFTPFileSystem::SFTPOpenRemoteFile(
  const AnsiString & FileName, unsigned int OpenType, __int64 Size)
{
  TSFTPPacket Packet(SSH_FXP_OPEN);

  Packet.AddPathString(FileName, FVersion, FUtfStrings);
  if (FVersion < 5)
  {
    Packet.AddCardinal(OpenType);
  }
  else
  {
    unsigned long Access =
      FLAGMASK(FLAGSET(OpenType, SSH_FXF_READ), ACE4_READ_DATA) |
      FLAGMASK(FLAGSET(OpenType, SSH_FXF_WRITE), ACE4_WRITE_DATA | ACE4_APPEND_DATA);

    unsigned long Flags;

    if (FLAGSET(OpenType, SSH_FXF_CREAT | SSH_FXF_EXCL))
    {
      Flags = SSH_FXF_CREATE_NEW;
    }
    else if (FLAGSET(OpenType, SSH_FXF_CREAT | SSH_FXF_TRUNC))
    {
      Flags = SSH_FXF_CREATE_TRUNCATE;
    }
    else if (FLAGSET(OpenType, SSH_FXF_CREAT))
    {
      Flags = SSH_FXF_OPEN_OR_CREATE;
    }
    else
    {
      Flags = SSH_FXF_OPEN_EXISTING;
    }

    Flags |=
      FLAGMASK(FLAGSET(OpenType, SSH_FXF_APPEND), SSH_FXF_ACCESS_APPEND_DATA) |
      FLAGMASK(FLAGSET(OpenType, SSH_FXF_TEXT), SSH_FXF_ACCESS_TEXT_MODE);

    Packet.AddCardinal(Access);
    Packet.AddCardinal(Flags);
  }
  Packet.AddProperties(NULL, NULL, NULL, NULL, NULL,
    Size >= 0 ? &Size : NULL, false, FVersion, FUtfStrings);

  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_HANDLE);

  return Packet.GetString();
}
//---------------------------------------------------------------------------
int __fastcall TSFTPFileSystem::SFTPOpenRemote(void * AOpenParams, void * /*Param2*/)
{
  TOpenRemoteFileParams * OpenParams = (TOpenRemoteFileParams *)AOpenParams;
  assert(OpenParams);
  TFileOperationProgressType * OperationProgress = OpenParams->OperationProgress;

  int OpenType;
  bool Confirmed = false;
  bool Success = false;

  do
  {
    try
    {
      OpenType = SSH_FXF_WRITE | SSH_FXF_CREAT;
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

      OpenParams->RemoteFileHandle = SFTPOpenRemoteFile(
        OpenParams->RemoteFileName, OpenType, OperationProgress->LocalSize);

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
          if (OpenParams->FileParams != NULL)
          {
            OpenParams->FileParams->DestTimestamp = File->Modification;
            OpenParams->FileParams->DestSize = OpenParams->DestFileSize;
          }
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
          TargetBiggerThanSource, OperationProgress, OpenParams->OverwriteMode,
          OpenParams->FileParams);
        Confirmed = true;

        if (FTerminal->SessionData->OverwrittenToRecycleBin)
        {
          FTerminal->RecycleFile(OpenParams->RemoteFileName, NULL);
        }
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
void __fastcall TSFTPFileSystem::SFTPCloseRemote(const AnsiString Handle,
  const AnsiString FileName, TFileOperationProgressType * OperationProgress,
  bool TransferFinished, bool Request, TSFTPPacket * Packet)
{
  // Moving this out of SFTPSource() fixed external exception 0xC0000029 error
  FILE_OPERATION_LOOP(FMTLOAD(SFTP_CLOSE_FILE_ERROR, (FileName)),
    try
    {
      TSFTPPacket CloseRequest;
      TSFTPPacket * P = (Packet == NULL ? &CloseRequest : Packet);
      
      if (Request)
      {
        P->ChangeType(SSH_FXP_CLOSE);
        P->AddString(Handle);
        SendPacket(P);
        ReserveResponse(P, Packet);
      }
      else
      {
        assert(Packet != NULL);
        ReceiveResponse(P, Packet, SSH_FXP_STATUS);
      }
    }
    catch(...)
    {
      if (!FTerminal->Active || TransferFinished)
      {
        throw;
      }
    }
  );
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SFTPDirectorySource(const AnsiString DirectoryName,
  const AnsiString TargetDir, int Attrs, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  AnsiString DestDirectoryName = CopyParam->ChangeFileName(
    ExtractFileName(ExcludeTrailingBackslash(DirectoryName)), osLocal,
    FLAGSET(Flags, tfFirstLevel));
  AnsiString DestFullName = UnixIncludeTrailingBackslash(TargetDir + DestDirectoryName);

  OperationProgress->SetFile(DirectoryName);

  bool CreateDir = false;
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
      CreateDir = true;
    }
    else
    {
      throw;
    }
  }

  if (CreateDir)
  {
    TRemoteProperties Properties;
    if (CopyParam->PreserveRights)
    {
      Properties.Valid = TValidProperties() << vpRights;
      Properties.Rights = CopyParam->RemoteFileRights(Attrs);
    }
    FTerminal->CreateDirectory(DestFullName, &Properties);
    Flags |= tfNewDirectory;
  }

  int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
  TSearchRec SearchRec;
  bool FindOK;

  FILE_OPERATION_LOOP (FMTLOAD(LIST_DIR_ERROR, (DirectoryName)),
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
        SFTPSource(FileName, DestFullName, CopyParam, Params, OperationProgress,
          Flags & ~tfFirstLevel);
      }
    }
    catch (EScpSkipFile &E)
    {
      // If ESkipFile occurs, just log it and continue with next file
      SUSPEND_OPERATION (
        // here a message to user was displayed, which was not appropriate
        // when user refused to overwrite the file in subdirectory.
        // hopefuly it won't be missing in other situations.
        if (!FTerminal->HandleException(&E)) throw;
      );
    }

    FILE_OPERATION_LOOP (FMTLOAD(LIST_DIR_ERROR, (DirectoryName)),
      FindOK = (FindNext(SearchRec) == 0);
    );
  };

  FindClose(SearchRec);

  /* TODO : Delete also read-only directories. */
  /* TODO : Show error message on failure. */
  if (!OperationProgress->Cancel)
  {
    if (FLAGSET(Params, cpDelete)) 
    {
      RemoveDir(DirectoryName);
    }
    else if (CopyParam->ClearArchive && FLAGSET(Attrs, faArchive))
    {
      FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (DirectoryName)),
        THROWIFFALSE(FileSetAttr(DirectoryName, Attrs & ~faArchive) == 0);
      )
    }
  }
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
    File = (TRemoteFile *)FilesToCopy->Objects[Index];

    assert(!FAvoidBusy);
    FAvoidBusy = true;

    try
    {
      try
      {
        SFTPSink(LocalCanonify(FileName), File, FullTargetDir, CopyParam,
          Params, OperationProgress, tfFirstLevel);
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
      OperationProgress->Finish(FileName, Success, DisconnectWhenComplete);
    }
    Index++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SFTPSink(const AnsiString FileName,
  const TRemoteFile * File, const AnsiString TargetDir,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  AnsiString OnlyFileName = UnixExtractFileName(FileName);

  if (FLAGCLEAR(Params, cpDelete) &&
      !CopyParam->AllowTransfer(OnlyFileName))
  {
    FTerminal->LogEvent(FORMAT("File \"%s\" excluded from transfer", (FileName)));
    THROW_SKIP_FILE_NULL;  
  }

  assert(File);
  FTerminal->LogEvent(FORMAT("File: \"%s\"", (FileName)));

  OperationProgress->SetFile(OnlyFileName);

  AnsiString DestFileName = CopyParam->ChangeFileName(OnlyFileName,
    osRemote, FLAGSET(Flags, tfFirstLevel));
  AnsiString DestFullName = TargetDir + DestFileName;

  if (File->IsDirectory)
  {
    if (!File->IsSymLink)
    {
      FILE_OPERATION_LOOP (FMTLOAD(NOT_DIRECTORY_ERROR, (DestFullName)),
        int Attrs = FileGetAttr(DestFullName);
        if ((Attrs & faDirectory) == 0) EXCEPTION;
      );

      FILE_OPERATION_LOOP (FMTLOAD(CREATE_DIR_ERROR, (DestFullName)),
        if (!ForceDirectories(DestFullName)) EXCEPTION;
      );

      TSinkFileParams SinkFileParams;
      SinkFileParams.TargetDir = IncludeTrailingBackslash(DestFullName);
      SinkFileParams.CopyParam = CopyParam;
      SinkFileParams.Params = Params;
      SinkFileParams.OperationProgress = OperationProgress;
      SinkFileParams.Skipped = false;
      SinkFileParams.Flags = Flags & ~tfFirstLevel;

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
      // file is symlink to directory, currently do nothing, but it should be
      // reported to user
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
    FILE_OPERATION_LOOP (FMTLOAD(NOT_FILE_ERROR, (DestFullName)),
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

      if ((Attrs >= 0) && !ResumeTransfer &&
          (FLAGSET(Params, cpNewerOnly) ||
           (FTerminal->Configuration->ConfirmOverwriting &&
            !OperationProgress->YesToAll &&
            FLAGCLEAR(Params, cpNoConfirmation))))
      {
        __int64 DestFileSize;
        unsigned long MTime;
        FTerminal->OpenLocalFile(DestFullName, GENERIC_WRITE,
          NULL, &LocalHandle, NULL, &MTime, NULL, &DestFileSize, false);

        FTerminal->LogEvent("Checking existence of file.");
        TSFTPOverwriteMode OverwriteMode;
        TOverwriteFileParams FileParams;
        FileParams.SourceSize = OperationProgress->TransferSize;
        FileParams.SourceTimestamp = File->Modification;
        FileParams.DestTimestamp = UnixToDateTime(MTime,
          FTerminal->SessionData->ConsiderDST);
        FileParams.DestSize = DestFileSize;
        SFTPConfirmOverwrite(DestFileName, (DestFileSize >= OperationProgress->TransferSize),
          OperationProgress, OverwriteMode, &FileParams);

        if (OverwriteMode == omOverwrite)
        {
          // is NULL when overwritting read-only file
          if (LocalHandle)
          {
            CloseHandle(LocalHandle);
            LocalHandle = NULL;
          }
        }
        else
        {
          // is NULL when overwritting read-only file, so following will
          // probably fail anyway
          if (LocalHandle == NULL)
          {
            FTerminal->OpenLocalFile(DestFullName, GENERIC_WRITE,
              NULL, &LocalHandle, NULL, NULL, NULL, NULL);
          }
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
        if (!FTerminal->CreateLocalFile(LocalFileName, OperationProgress, &LocalHandle))
        {
          THROW_SKIP_FILE_NULL;
        }
      }
      assert(LocalHandle);

      DeleteLocalFile = true;

      FTerminal->LogEvent("Opening remote file.");
      FILE_OPERATION_LOOP (FMTLOAD(SFTP_OPEN_FILE_ERROR, (FileName)),
        int OpenType = SSH_FXF_READ;
        if ((FVersion >= 4) && OperationProgress->AsciiTransfer)
        {
          OpenType |= SSH_FXF_TEXT;
        }
        RemoteHandle = SFTPOpenRemoteFile(FileName, OpenType);
      );

      FileStream = new THandleStream((THandle)LocalHandle);

      // at end of this block queue is disposed
      {
        TSFTPDownloadQueue Queue(this);
        TSFTPPacket DataPacket;

        int QueueLen = int(File->Size / DownloadBlockSize(OperationProgress)) + 1;
        if ((QueueLen > FTerminal->SessionData->SFTPDownloadQueue) ||
            (QueueLen < 0))
        {
          QueueLen = FTerminal->SessionData->SFTPDownloadQueue;
        }
        if (QueueLen < 1)
        {
          QueueLen = 1;
        }
        Queue.Init(QueueLen, RemoteHandle, OperationProgress->TransferedSize,
          OperationProgress);

        bool Eof = false;
        bool PrevIncomplete = false;
        int GapFillCount = 0;
        int GapCount = 0;
        unsigned long Missing = 0;
        unsigned long DataLen = 0;
        unsigned long BlockSize;

        while (!Eof)
        {
          if (Missing > 0)
          {
            Queue.InitFillGapRequest(OperationProgress->TransferedSize, Missing,
              &DataPacket);
            GapFillCount++;
            SendPacketAndReceiveResponse(&DataPacket, &DataPacket,
              SSH_FXP_DATA, asEOF);
          }
          else
          {
            Queue.ReceivePacket(&DataPacket, BlockSize);
          }

          if (DataPacket.Type == SSH_FXP_STATUS)
          {
            // must be SSH_FX_EOF, any other status packet would raise exception
            Eof = true;
            // close file right away, before waiting for pending responses
            SFTPCloseRemote(RemoteHandle, DestFileName, OperationProgress,
              true, true, NULL);
            RemoteHandle = ""; // do not close file again in __finally block
          }

          if (!Eof)
          {
            if ((Missing == 0) && PrevIncomplete)
            {
              // This can happen only if last request returned less bytes
              // than expected, but exacly number of bytes missing to last
              // known file size, but actually EOF was not reached.
              // Can happen only when filesize has changed since directory
              // listing and server returns less bytes than requested and
              // file has some special file size.
              FTerminal->LogEvent(FORMAT(
                "Received incomplete data packet before end of file, "
                "offset: %s, size: %d, requested: %d",
                (IntToStr(OperationProgress->TransferedSize), int(DataLen),
                int(BlockSize))));
              FTerminal->TerminalError(NULL, LoadStr(SFTP_INCOMPLETE_BEFORE_EOF));
            }

            // Buffer for one block of data
            TFileBuffer BlockBuf;

            DataLen = DataPacket.GetCardinal();

            PrevIncomplete = false;
            if (Missing > 0)
            {
              assert(DataLen <= Missing);
              Missing -= DataLen;
            }
            else if (DataLen < BlockSize)
            {
              if (OperationProgress->TransferedSize + DataLen !=
                    OperationProgress->TransferSize)
              {
                // with native text transfer mode (SFTP>=4), do not bother about
                // getting less than requested, read offset is ignored anyway
                if ((FVersion < 4) || !OperationProgress->AsciiTransfer)
                {
                  GapCount++;
                  Missing = BlockSize - DataLen;
                }
              }
              else
              {
                PrevIncomplete = true;
              }
            }

            assert(DataLen <= BlockSize);
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

            FILE_OPERATION_LOOP (FMTLOAD(WRITE_ERROR, (LocalFileName)),
              try
              {
                BlockBuf.WriteToStream(FileStream, BlockBuf.Size);
              }
              catch(...)
              {
                RaiseLastOSError();
              }
            );

            OperationProgress->AddLocalyUsed(BlockBuf.Size);
          }
          
          if (OperationProgress->Cancel == csCancel)
          {
            Abort();
          }
        };

        if (GapCount > 0)
        {
          FTerminal->LogEvent(FORMAT(
            "%d requests to fill %d data gaps were issued.",
            (GapFillCount, GapCount)));
        }
      }

      if (CopyParam->PreserveTime)
      {
        FILETIME AcTime = DateTimeToFileTime(File->LastAccess,
          FTerminal->SessionData->ConsiderDST);
        FILETIME WrTime = DateTimeToFileTime(File->Modification,
          FTerminal->SessionData->ConsiderDST);
        SetFileTime(LocalHandle, NULL, &AcTime, &WrTime);
      }

      CloseHandle(LocalHandle);
      LocalHandle = NULL;

      if (ResumeAllowed)
      {
        FILE_OPERATION_LOOP(FMTLOAD(RENAME_AFTER_RESUME_ERROR,
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
        FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (DestFullName)),
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

      // if the transfer was finished, the file is closed already
      if (FTerminal->Active && !RemoteHandle.IsEmpty())
      {
        // do not wait for response
        SFTPCloseRemote(RemoteHandle, DestFileName, OperationProgress,
          true, true, NULL);
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
      Params->Params, Params->OperationProgress, Params->Flags);
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
