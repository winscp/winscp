//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SftpFileSystem.h"

#include "PuttyTools.h"
#include "Common.h"
#include "Exceptions.h"
#include "Interface.h"
#include "Terminal.h"
#include "TextsCore.h"
#include "HelpCore.h"
#include "SecureShell.h"
#include "Cryptography.h"
#include <WideStrUtils.hpp>
#include <limits>

#include <memory>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_TERMINAL FTerminal
//---------------------------------------------------------------------------
#define SSH_FX_OK                                 0
#define SSH_FX_EOF                                1
#define SSH_FX_NO_SUCH_FILE                       2
#define SSH_FX_PERMISSION_DENIED                  3
#define SSH_FX_FAILURE                            4
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
#define SSH_FXP_LINK               21
#define SSH_FXP_STATUS             101
#define SSH_FXP_HANDLE             102
#define SSH_FXP_DATA               103
#define SSH_FXP_NAME               104
#define SSH_FXP_ATTRS              105
#define SSH_FXP_EXTENDED           200
#define SSH_FXP_EXTENDED_REPLY     201

#define SSH_FILEXFER_ATTR_SIZE              0x00000001
#define SSH_FILEXFER_ATTR_UIDGID            0x00000002
#define SSH_FILEXFER_ATTR_PERMISSIONS       0x00000004
#define SSH_FILEXFER_ATTR_ACMODTIME         0x00000008
#define SSH_FILEXFER_ATTR_ACCESSTIME        0x00000008
#define SSH_FILEXFER_ATTR_CREATETIME        0x00000010
#define SSH_FILEXFER_ATTR_MODIFYTIME        0x00000020
#define SSH_FILEXFER_ATTR_ACL               0x00000040
#define SSH_FILEXFER_ATTR_OWNERGROUP        0x00000080
#define SSH_FILEXFER_ATTR_SUBSECOND_TIMES   0x00000100
#define SSH_FILEXFER_ATTR_BITS              0x00000200
#define SSH_FILEXFER_ATTR_ALLOCATION_SIZE   0x00000400
#define SSH_FILEXFER_ATTR_TEXT_HINT         0x00000800
#define SSH_FILEXFER_ATTR_MIME_TYPE         0x00001000
#define SSH_FILEXFER_ATTR_LINK_COUNT        0x00002000
#define SSH_FILEXFER_ATTR_UNTRANSLATED_NAME 0x00004000
#define SSH_FILEXFER_ATTR_CTIME             0x00008000
#define SSH_FILEXFER_ATTR_EXTENDED          0x80000000

#define SSH_FILEXFER_ATTR_COMMON \
  (SSH_FILEXFER_ATTR_SIZE | SSH_FILEXFER_ATTR_OWNERGROUP | \
   SSH_FILEXFER_ATTR_PERMISSIONS | SSH_FILEXFER_ATTR_ACCESSTIME | \
   SSH_FILEXFER_ATTR_MODIFYTIME)

#define SSH_FILEXFER_TYPE_REGULAR          1
#define SSH_FILEXFER_TYPE_DIRECTORY        2
//      SSH_FILEXFER_TYPE_SYMLINK          3
//      SSH_FILEXFER_TYPE_SPECIAL          4
//      SSH_FILEXFER_TYPE_UNKNOWN          5

#define SSH_FXF_READ            0x00000001
#define SSH_FXF_WRITE           0x00000002
#define SSH_FXF_APPEND          0x00000004
#define SSH_FXF_CREAT           0x00000008
#define SSH_FXF_TRUNC           0x00000010
#define SSH_FXF_EXCL            0x00000020
#define SSH_FXF_TEXT            0x00000040

//      SSH_FXF_ACCESS_DISPOSITION        0x00000007
#define     SSH_FXF_CREATE_NEW            0x00000000
#define     SSH_FXF_CREATE_TRUNCATE       0x00000001
#define     SSH_FXF_OPEN_EXISTING         0x00000002
#define     SSH_FXF_OPEN_OR_CREATE        0x00000003
//          SSH_FXF_TRUNCATE_EXISTING     0x00000004
#define SSH_FXF_ACCESS_APPEND_DATA        0x00000008
//      SSH_FXF_ACCESS_APPEND_DATA_ATOMIC 0x00000010
#define SSH_FXF_ACCESS_TEXT_MODE          0x00000020

#define ACE4_READ_DATA         0x00000001
//      ACE4_LIST_DIRECTORY    0x00000001
#define ACE4_WRITE_DATA        0x00000002
//      ACE4_ADD_FILE          0x00000002
#define ACE4_APPEND_DATA       0x00000004
//      ACE4_ADD_SUBDIRECTORY  0x00000004
//      ACE4_READ_NAMED_ATTRS  0x00000008
//      ACE4_WRITE_NAMED_ATTRS 0x00000010
//      ACE4_EXECUTE           0x00000020
//      ACE4_DELETE_CHILD      0x00000040
//      ACE4_READ_ATTRIBUTES   0x00000080
//      ACE4_WRITE_ATTRIBUTES  0x00000100
//      ACE4_DELETE            0x00010000
//      ACE4_READ_ACL          0x00020000
//      ACE4_WRITE_ACL         0x00040000
//      ACE4_WRITE_OWNER       0x00080000
//      ACE4_SYNCHRONIZE       0x00100000

#define SSH_FILEXFER_ATTR_FLAGS_HIDDEN           0x00000004

//      SSH_FXP_REALPATH_NO_CHECK    0x00000001
#define SSH_FXP_REALPATH_STAT_IF     0x00000002
#define SSH_FXP_REALPATH_STAT_ALWAYS 0x00000003

#define SFTP_MAX_PACKET_LEN   1024000
//---------------------------------------------------------------------------
#define SFTP_EXT_OWNER_GROUP L"owner-group-query@generic-extensions"
#define SFTP_EXT_OWNER_GROUP_REPLY L"owner-group-query-reply@generic-extensions"
#define SFTP_EXT_NEWLINE L"newline"
#define SFTP_EXT_SUPPORTED L"supported"
#define SFTP_EXT_SUPPORTED2 L"supported2"
#define SFTP_EXT_FSROOTS L"fs-roots@vandyke.com"
#define SFTP_EXT_VENDOR_ID L"vendor-id"
#define SFTP_EXT_VERSIONS L"versions"
#define SFTP_EXT_SPACE_AVAILABLE L"space-available"
#define SFTP_EXT_CHECK_FILE L"check-file"
#define SFTP_EXT_CHECK_FILE_NAME L"check-file-name"
#define SFTP_EXT_STATVFS L"statvfs@openssh.com"
#define SFTP_EXT_STATVFS_VALUE_V2 L"2"
#define SFTP_EXT_STATVFS_ST_RDONLY 0x1
#define SFTP_EXT_STATVFS_ST_NOSUID 0x2
#define SFTP_EXT_HARDLINK L"hardlink@openssh.com"
#define SFTP_EXT_HARDLINK_VALUE_V1 L"1"
#define SFTP_EXT_COPY_FILE L"copy-file"
#define SFTP_EXT_COPY_DATA L"copy-data"
#define SFTP_EXT_LIMITS L"limits@openssh.com"
#define SFTP_EXT_LIMITS_VALUE_V1 L"1"
#define SFTP_EXT_POSIX_RENAME L"posix-rename@openssh.com"
//---------------------------------------------------------------------------
#define OGQ_LIST_OWNERS 0x01
#define OGQ_LIST_GROUPS 0x02
//---------------------------------------------------------------------------
const int SFTPMinVersion = 0;
const int SFTPStandardVersion = 3;
const int SFTPMaxVersion = 6;
const unsigned int SFTPNoMessageNumber = static_cast<unsigned int>(-1);

const int asNo =            0;
const int asOK =            1 << SSH_FX_OK;
const int asEOF =           1 << SSH_FX_EOF;
const int asPermDenied =    1 << SSH_FX_PERMISSION_DENIED;
const int asOpUnsupported = 1 << SSH_FX_OP_UNSUPPORTED;
const int asNoSuchFile =    1 << SSH_FX_NO_SUCH_FILE;
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
//---------------------------------------------------------------------------
#define SFTP_PACKET_ALLOC_DELTA 256
//---------------------------------------------------------------------------
#pragma warn -inl
//---------------------------------------------------------------------------
struct TSFTPSupport
{
  TSFTPSupport() :
    AttribExtensions(new TStringList())
  {
    Reset();
  }

  ~TSFTPSupport()
  {
    delete AttribExtensions;
  }

  void Reset()
  {
    AttributeMask = 0;
    AttributeBits = 0;
    OpenFlags = 0;
    AccessMask = 0;
    MaxReadSize = 0;
    OpenBlockVector = 0;
    BlockVector = 0;
    AttribExtensions->Clear();
    Loaded = false;
  }

  unsigned int AttributeMask;
  unsigned int AttributeBits;
  unsigned int OpenFlags;
  unsigned int AccessMask;
  unsigned int MaxReadSize;
  unsigned int OpenBlockVector;
  unsigned int BlockVector;
  TStrings * AttribExtensions;
  bool Loaded;
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

  TSFTPPacket(const unsigned char * Source, unsigned int Len)
  {
    Init();
    FLength = Len;
    Capacity = FLength;
    memcpy(Data, Source, Len);
  }

  TSFTPPacket(const RawByteString & Source)
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

    DebugAssert(Length >= 5);

    // duplicated in AddCardinal()
    unsigned char Buf[4];
    PUT_32BIT(Buf, FMessageNumber);

    memcpy(FData + 1, Buf, sizeof(Buf));
  }

  void AddByte(unsigned char Value)
  {
    Add(&Value, sizeof(Value));
  }

  void AddBool(bool Value)
  {
    AddByte(Value ? 1 : 0);
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

  void AddString(const RawByteString & Value)
  {
    AddCardinal(Value.Length());
    Add(Value.c_str(), Value.Length());
  }

  inline void AddUtfString(const UTF8String & Value)
  {
    AddString(Value);
  }

  inline void AddUtfString(const UnicodeString & Value)
  {
    AddUtfString(UTF8String(Value));
  }

  inline void AddString(const UnicodeString Value, TAutoSwitch Utf)
  {
    // asAuto: Using UTF until we receive non-UTF string from the server
    if ((Utf == asOn) || (Utf == asAuto))
    {
      AddUtfString(Value);
    }
    else
    {
      AddString(RawByteString(AnsiString(Value)));
    }
  }

  // now purposeless alias to AddString
  inline void AddPathString(const UnicodeString & Value, TAutoSwitch Utf)
  {
    AddString(Value, Utf);
  }

  unsigned int AllocationSizeAttribute(int Version)
  {
    return (Version >= 6) ? SSH_FILEXFER_ATTR_ALLOCATION_SIZE : SSH_FILEXFER_ATTR_SIZE;
  }

  void AddProperties(unsigned short * Rights, TRemoteToken * Owner,
    TRemoteToken * Group, __int64 * MTime, __int64 * ATime,
    __int64 * Size, bool IsDirectory, int Version, TAutoSwitch Utf)
  {
    int Flags = 0;
    if (Size != NULL)
    {
      Flags |= AllocationSizeAttribute(Version);
    }
    // both or neither
    DebugAssert((Owner != NULL) == (Group != NULL));
    if ((Owner != NULL) && (Group != NULL))
    {
      if (Version < 4)
      {
        DebugAssert(Owner->IDValid && Group->IDValid);
        Flags |= SSH_FILEXFER_ATTR_UIDGID;
      }
      else
      {
        DebugAssert(Owner->NameValid && Group->NameValid);
        Flags |= SSH_FILEXFER_ATTR_OWNERGROUP;
      }
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
      // this is SSH_FILEXFER_ATTR_SIZE for version <= 5, but
      // SSH_FILEXFER_ATTR_ALLOCATION_SIZE for version >= 6
      AddInt64(*Size);
    }

    if ((Owner != NULL) && (Group != NULL))
    {
      if (Version < 4)
      {
        DebugAssert(Owner->IDValid && Group->IDValid);
        AddCardinal(Owner->ID);
        AddCardinal(Group->ID);
      }
      else
      {
        DebugAssert(Owner->NameValid && Group->NameValid);
        AddString(Owner->Name, Utf);
        AddString(Group->Name, Utf);
      }
    }

    if (Rights != NULL)
    {
      AddCardinal(*Rights);
    }

    if ((Version < 4) && ((MTime != NULL) || (ATime != NULL)))
    {
      // any way to reflect sbSignedTS here?
      // (note that casting __int64 > 2^31 < 2^32 to unsigned long is wrapped,
      // thus we never can set time after 2038, even if the server supports it)
      AddCardinal(static_cast<unsigned long>(ATime != NULL ? *ATime : *MTime));
      AddCardinal(static_cast<unsigned long>(MTime != NULL ? *MTime : *ATime));
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
    unsigned short BaseRights, bool IsDirectory, int Version, TAutoSwitch Utf,
    TChmodSessionAction * Action)
  {
    enum TValid { valNone = 0, valRights = 0x01, valOwner = 0x02, valGroup = 0x04,
      valMTime = 0x08, valATime = 0x10 } Valid = valNone;
    unsigned short RightsNum = 0;
    TRemoteToken Owner;
    TRemoteToken Group;
    __int64 MTime;
    __int64 ATime;

    if (Properties != NULL)
    {
      if (Properties->Valid.Contains(vpGroup))
      {
        Valid = (TValid)(Valid | valGroup);
        Group = Properties->Group;
      }

      if (Properties->Valid.Contains(vpOwner))
      {
        Valid = (TValid)(Valid | valOwner);
        Owner = Properties->Owner;
      }

      if (Properties->Valid.Contains(vpRights))
      {
        Valid = (TValid)(Valid | valRights);
        TRights Rights = TRights(BaseRights).Combine(Properties->Rights);
        if (IsDirectory && Properties->AddXToDirectories)
        {
          Rights.AddExecute();
        }
        RightsNum = Rights;

        if (Action != NULL)
        {
          Action->Rights(Rights);
        }
      }

      if (Properties->Valid.Contains(vpLastAccess))
      {
        Valid = (TValid)(Valid | valATime);
        ATime = Properties->LastAccess;
      }

      if (Properties->Valid.Contains(vpModification))
      {
        Valid = (TValid)(Valid | valMTime);
        MTime = Properties->Modification;
      }
    }

    AddProperties(
      Valid & valRights ? &RightsNum : NULL,
      Valid & valOwner ? &Owner : NULL,
      Valid & valGroup ? &Group : NULL,
      Valid & valMTime ? &MTime : NULL,
      Valid & valATime ? &ATime : NULL,
      NULL, IsDirectory, Version, Utf);
  }

  unsigned char GetByte()
  {
    Need(sizeof(unsigned char));
    unsigned char Result = FData[FPosition];
    DataConsumed(sizeof(unsigned char));
    return Result;
  }

  bool GetBool()
  {
    return (GetByte() != 0);
  }

  bool CanGetBool()
  {
    return (RemainingLength >= sizeof(unsigned char));
  }

  unsigned long GetCardinal()
  {
    unsigned long Result = PeekCardinal();
    DataConsumed(sizeof(Result));
    return Result;
  }

  bool CanGetCardinal()
  {
    return (RemainingLength >= sizeof(unsigned long));
  }

  unsigned long GetSmallCardinal()
  {
    unsigned long Result;
    Need(2);
    Result = (FData[FPosition] << 8) + FData[FPosition + 1];
    DataConsumed(2);
    return Result;
  }

  bool CanGetSmallCardinal()
  {
    return (RemainingLength >= 2);
  }

  __int64 GetInt64()
  {
    __int64 Hi = GetCardinal();
    __int64 Lo = GetCardinal();
    return (Hi << 32) + Lo;
  }

  RawByteString GetRawByteString()
  {
    RawByteString Result;
    unsigned long Len = GetCardinal();
    Need(Len);
    // cannot happen anyway as Need() would raise exception
    DebugAssert(Len < SFTP_MAX_PACKET_LEN);
    Result.SetLength(Len);
    memcpy(Result.c_str(), FData + FPosition, Len);
    DataConsumed(Len);
    return Result;
  }

  bool CanGetString(unsigned int & Size)
  {
    bool Result = CanGetCardinal();
    if (Result)
    {
      unsigned long Len = PeekCardinal();
      Size = (sizeof(Len) + Len);
      Result = (Size <= RemainingLength);
    }
    return Result;
  }

  // For reading strings that are character strings (not byte strings
  // as file handles), and SFTP spec does not say explicitly that they
  // are in UTF. For most of them it actually does not matter as
  // the content should be pure ASCII (e.g. extension names, etc.)
  inline UnicodeString GetAnsiString()
  {
    return AnsiToString(GetRawByteString());
  }

  inline RawByteString GetFileHandle()
  {
    return GetRawByteString();
  }

  inline UnicodeString GetString(TAutoSwitch & Utf)
  {
    if (Utf != asOff)
    {
      return GetUtfString(Utf);
    }
    else
    {
      return GetAnsiString();
    }
  }

  // now purposeless alias to GetString(bool)
  inline UnicodeString GetPathString(TAutoSwitch & Utf)
  {
    return GetString(Utf);
  }

  void GetFile(TRemoteFile * File, int Version, TDSTMode DSTMode, TAutoSwitch & Utf, bool SignedTS, bool Complete)
  {
    DebugAssert(File);
    unsigned int Flags;
    UnicodeString ListingStr;
    unsigned long Permissions = 0;
    bool ParsingFailed = false;
    if (Type != SSH_FXP_ATTRS)
    {
      File->FileName = GetPathString(Utf);
      if (Version < 4)
      {
        ListingStr = GetString(Utf);
      }
    }
    Flags = GetCardinal();
    if (Version >= 4)
    {
      unsigned char FXType = GetByte();
      // -:regular, D:directory, L:symlink, S:special, U:unknown
      // O:socket, C:char device, B:block device, F:fifo

      // SSH-2.0-cryptlib returns file type 0 in response to SSH_FXP_LSTAT,
      // handle this undefined value as "unknown"
      static const wchar_t * Types = L"U-DLSUOCBF";
      if (FXType > (unsigned char)wcslen(Types))
      {
        throw Exception(FMTLOAD(SFTP_UNKNOWN_FILE_TYPE, (int(FXType))));
      }
      File->Type = Types[FXType];
    }
    if (Flags & SSH_FILEXFER_ATTR_SIZE)
    {
      File->Size = GetInt64();
    }
    // SFTP-6 only
    if (Flags & SSH_FILEXFER_ATTR_ALLOCATION_SIZE)
    {
      GetInt64(); // skip
    }
    // SSH-2.0-3.2.0 F-SECURE SSH - Process Software MultiNet
    // sets SSH_FILEXFER_ATTR_UIDGID for v4, but does not include the UID/GUID
    if ((Flags & SSH_FILEXFER_ATTR_UIDGID) && (Version < 4))
    {
      File->Owner.ID = GetCardinal();
      File->Group.ID = GetCardinal();
    }
    if (Flags & SSH_FILEXFER_ATTR_OWNERGROUP)
    {
      DebugAssert(Version >= 4);
      File->Owner.Name = GetString(Utf);
      File->Group.Name = GetString(Utf);
    }
    if (Flags & SSH_FILEXFER_ATTR_PERMISSIONS)
    {
      Permissions = GetCardinal();
    }
    if (Version < 4)
    {
      if (Flags & SSH_FILEXFER_ATTR_ACMODTIME)
      {
        File->LastAccess = UnixToDateTime(
          SignedTS ?
            static_cast<__int64>(static_cast<signed long>(GetCardinal())) :
            static_cast<__int64>(GetCardinal()),
          DSTMode);
        File->Modification = UnixToDateTime(
          SignedTS ?
            static_cast<__int64>(static_cast<signed long>(GetCardinal())) :
            static_cast<__int64>(GetCardinal()),
          DSTMode);
      }
    }
    else
    {
      if (Flags & SSH_FILEXFER_ATTR_ACCESSTIME)
      {
        File->LastAccess = UnixToDateTime(GetInt64(), DSTMode);
        if (Flags & SSH_FILEXFER_ATTR_SUBSECOND_TIMES)
        {
          GetCardinal(); // skip access time subseconds
        }
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
        File->Modification = UnixToDateTime(GetInt64(), DSTMode);
        if (Flags & SSH_FILEXFER_ATTR_SUBSECOND_TIMES)
        {
          GetCardinal(); // skip modification time subseconds
        }
      }
      // SFTP-6
      if (Flags & SSH_FILEXFER_ATTR_CTIME)
      {
        GetInt64(); // skip attribute modification time
        if (Flags & SSH_FILEXFER_ATTR_SUBSECOND_TIMES)
        {
          GetCardinal(); // skip attribute modification time subseconds
        }
      }
    }

    if (Flags & SSH_FILEXFER_ATTR_ACL)
    {
      GetRawByteString();
    }

    if (Flags & SSH_FILEXFER_ATTR_BITS)
    {
      // while SSH_FILEXFER_ATTR_BITS is defined for SFTP5 only, vandyke 2.3.3 sets it
      // for SFTP4 as well
      unsigned long Bits = GetCardinal();
      if (Version >= 6)
      {
        unsigned long BitsValid = GetCardinal();
        Bits = Bits & BitsValid;
      }
      if (FLAGSET(Bits, SSH_FILEXFER_ATTR_FLAGS_HIDDEN))
      {
        File->IsHidden = true;
      }
    }

    // skip some SFTP-6 only fields
    if (Flags & SSH_FILEXFER_ATTR_TEXT_HINT)
    {
      GetByte();
    }
    if (Flags & SSH_FILEXFER_ATTR_MIME_TYPE)
    {
      GetAnsiString();
    }
    if (Flags & SSH_FILEXFER_ATTR_LINK_COUNT)
    {
      GetCardinal();
    }
    if (Flags & SSH_FILEXFER_ATTR_UNTRANSLATED_NAME)
    {
      GetPathString(Utf);
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
      wchar_t Type = FILETYPE_DEFAULT;
      if (FLAGSET(Flags, SSH_FILEXFER_ATTR_PERMISSIONS))
      {
        File->Rights->Number = (unsigned short)(Permissions & TRights::rfAllSpecials);
        if (FLAGSET(Permissions, TRights::rfDirectory))
        {
          Type = FILETYPE_DIRECTORY;
        }
      }

      if (Version < 4)
      {
        File->Type = Type;
      }
    }

    if (Flags & SSH_FILEXFER_ATTR_EXTENDED)
    {
      unsigned int ExtendedCount = GetCardinal();
      for (unsigned int Index = 0; Index < ExtendedCount; Index++)
      {
        GetRawByteString(); // skip extended_type
        GetRawByteString(); // skip extended_data
      }
    }

    if (Complete)
    {
      File->Complete();
    }
  }

  unsigned char * GetNextData(unsigned int Size = 0)
  {
    if (Size > 0)
    {
      Need(Size);
    }
    return FPosition < FLength ? FData + FPosition : NULL;
  }

  void DataConsumed(unsigned int Size)
  {
    FPosition += Size;
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

  void LoadFromFile(const UnicodeString FileName)
  {
    TStringList * DumpLines = new TStringList();
    RawByteString Dump;
    try
    {
      DumpLines->LoadFromFile(FileName);
      Dump = RawByteString(AnsiString(DumpLines->Text));
    }
    __finally
    {
      delete DumpLines;
    }

    Capacity = 20480;
    unsigned char Byte[3];
    memset(Byte, '\0', sizeof(Byte));
    int Index = 1;
    unsigned int Length = 0;
    while (Index < Dump.Length())
    {
      char C = Dump[Index];
      if (IsHex(C))
      {
        if (Byte[0] == '\0')
        {
          Byte[0] = C;
        }
        else
        {
          Byte[1] = C;
          DebugAssert(Length < Capacity);
          Data[Length] = HexToByte(UnicodeString(reinterpret_cast<char *>(Byte)));
          Length++;
          memset(Byte, '\0', sizeof(Byte));
        }
      }
      Index++;
    }
    DataUpdated(Length);
  }

  UnicodeString __fastcall Dump() const
  {
    UnicodeString Result;
    for (unsigned int Index = 0; Index < Length; Index++)
    {
      Result += ByteToHex(Data[Index]) + L",";
      if (((Index + 1) % 25) == 0)
      {
        Result += L"\n";
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
  __property unsigned int RemainingLength = { read = GetRemainingLength };
  __property unsigned char * Data = { read = FData };
  __property unsigned char * SendData = { read = GetSendData };
  __property unsigned int SendLength = { read = GetSendLength };
  __property unsigned int Capacity = { read = FCapacity, write = SetCapacity };
  __property unsigned char Type = { read = FType };
  __property unsigned char RequestType = { read = GetRequestType };
  __property unsigned int MessageNumber = { read = FMessageNumber, write = FMessageNumber };
  __property TSFTPFileSystem * ReservedBy = { read = FReservedBy, write = FReservedBy };
  __property UnicodeString TypeName = { read = GetTypeName };

private:
  unsigned char * FData;
  unsigned int FLength;
  unsigned int FCapacity;
  unsigned int FPosition;
  unsigned char FType;
  unsigned int FMessageNumber;
  TSFTPFileSystem * FReservedBy;

  static int FMessageCounter;
  static const int FSendPrefixLen = 4;

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
      DebugAssert(Type == SSH_FXP_VERSION);
      return SSH_FXP_INIT;
    }
  }

  inline void Add(const void * AData, int ALength)
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
        unsigned char * NData = (new unsigned char[FCapacity + FSendPrefixLen]) + FSendPrefixLen;
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

  UnicodeString GetTypeName() const
  {
    #define TYPE_CASE(TYPE) case TYPE: return TEXT(#TYPE)
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
      TYPE_CASE(SSH_FXP_LINK);
      TYPE_CASE(SSH_FXP_STATUS);
      TYPE_CASE(SSH_FXP_HANDLE);
      TYPE_CASE(SSH_FXP_DATA);
      TYPE_CASE(SSH_FXP_NAME);
      TYPE_CASE(SSH_FXP_ATTRS);
      TYPE_CASE(SSH_FXP_EXTENDED);
      TYPE_CASE(SSH_FXP_EXTENDED_REPLY);
      default:
        return FORMAT(L"Unknown message (%d)", (int(Type)));
    }
  }

  unsigned char * GetSendData() const
  {
    unsigned char * Result = FData - FSendPrefixLen;
    // this is not strictly const-object operation
    PUT_32BIT(Result, Length);
    return Result;
  }

  unsigned int GetSendLength() const
  {
    return FSendPrefixLen + Length;
  }

  unsigned int GetRemainingLength() const
  {
    return Length - FPosition;
  }

  inline void Need(unsigned int Size)
  {
    if (Size > RemainingLength)
    {
      throw Exception(FMTLOAD(SFTP_PACKET_ERROR, (int(FPosition), int(Size), int(FLength))));
    }
  }

  unsigned long PeekCardinal()
  {
    unsigned long Result;
    Need(sizeof(Result));
    Result = GET_32BIT(FData + FPosition);
    return Result;
  }

  inline UnicodeString GetUtfString(TAutoSwitch & Utf)
  {
    DebugAssert(Utf != asOff);
    UnicodeString Result;
    RawByteString S = GetRawByteString();

    if (Utf == asAuto)
    {
      TEncodeType EncodeType = DetectUTF8Encoding(S);
      if (EncodeType == etANSI)
      {
        Utf = asOff;
        Result = AnsiToString(S);
      }
    }

    if (Utf != asOff)
    {
      Result = UTF8ToString(S);
    }

    return Result;
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
    DebugAssert(FFileSystem);
    FRequests = new TList();
    FResponses = new TList();
  }

  virtual __fastcall ~TSFTPQueue()
  {
    TSFTPQueuePacket * Request;
    TSFTPPacket * Response;

    DebugAssert(FResponses->Count == FRequests->Count);
    for (int Index = 0; Index < FRequests->Count; Index++)
    {
      Request = static_cast<TSFTPQueuePacket*>(FRequests->Items[Index]);
      DebugAssert(Request);
      delete Request;

      Response = static_cast<TSFTPPacket*>(FResponses->Items[Index]);
      DebugAssert(Response);
      delete Response;
    }
    delete FRequests;
    delete FResponses;
  }

  bool __fastcall Init()
  {
    return SendRequests();
  }

  void DisposeUntil(int Count, int ExpectedType)
  {
    DebugAssert(FFileSystem->FTerminal->Active);

    while (FRequests->Count > Count)
    {
      DebugAssert(FResponses->Count);

      std::unique_ptr<TSFTPQueuePacket> Request(DebugNotNull(static_cast<TSFTPQueuePacket*>(FRequests->Items[0])));
      std::unique_ptr<TSFTPPacket> Response(DebugNotNull(static_cast<TSFTPPacket*>(FResponses->Items[0])));

      // Particularly when ExpectedType >= 0, the ReceiveResponse may throw, and we have to remove the packets from queue
      FRequests->Delete(0);
      FResponses->Delete(0);

      try
      {
        ReceiveResponse(Request.get(), Response.get(), ExpectedType, -1);
      }
      catch(Exception & E)
      {
        if ((ExpectedType < 0) && (Count == 0))
        {
          if (FFileSystem->FTerminal->Active)
          {
            FFileSystem->FTerminal->LogEvent(L"Error while disposing the SFTP queue.");
            FFileSystem->FTerminal->Log->AddException(&E);
          }
          else
          {
            FFileSystem->FTerminal->LogEvent(L"Fatal error while disposing the SFTP queue.");
            throw;
          }
        }
        else
        {
          throw;
        }
      }
    }
  }

  virtual void __fastcall Dispose(int ExpectedType)
  {
    DisposeUntil(0, ExpectedType);
  }

  void __fastcall DisposeSafe(int ExpectedType = -1)
  {
    if (FFileSystem->FTerminal->Active)
    {
      Dispose(ExpectedType);
    }
  }

  bool __fastcall ReceivePacket(TSFTPPacket * Packet,
    int ExpectedType = -1, int AllowStatus = -1, void ** Token = NULL, bool TryOnly = false)
  {
    DebugAssert(FRequests->Count);
    bool Result;
    TSFTPQueuePacket * Request = NULL;
    TSFTPPacket * Response = NULL;
    try
    {
      Request = static_cast<TSFTPQueuePacket*>(FRequests->Items[0]);
      FRequests->Delete(0);
      DebugAssert(Request);
      if (Token != NULL)
      {
        *Token = Request->Token;
      }

      Response = static_cast<TSFTPPacket*>(FResponses->Items[0]);
      FResponses->Delete(0);
      DebugAssert(Response);

      ReceiveResponse(Request, Response, ExpectedType, AllowStatus, TryOnly);

      if ((Response->Capacity == 0) && DebugAlwaysTrue(TryOnly))
      {
        FRequests->Insert(0, Request);
        Request = NULL;
        FResponses->Insert(0, Response);
        Response = NULL;
        Result = true;
      }
      else
      {
        if (Packet)
        {
          *Packet = *Response;
        }

        Result = !End(Response);
        if (Result)
        {
          SendRequests();
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
  TList * FRequests;
  TList * FResponses;
  TSFTPFileSystem * FFileSystem;

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

  virtual void __fastcall ReceiveResponse(
    const TSFTPPacket * Packet, TSFTPPacket * Response, int ExpectedType,
    int AllowStatus, bool TryOnly = false)
  {
    FFileSystem->ReceiveResponse(Packet, Response, ExpectedType, AllowStatus, TryOnly);
  }

  // sends as many requests as allowed by implementation
  virtual bool SendRequests() = 0;

  virtual bool __fastcall SendRequest()
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
      if (FFileSystem->FTerminal->Configuration->ActualLogProtocol >= 1)
      {
        FFileSystem->FTerminal->LogEvent(FORMAT(L"Queue len: %d", (FRequests->Count)));
      }

      // make sure the response is reserved before actually sending the message
      // as we may receive response asynchronously before SendPacket finishes
      FFileSystem->ReserveResponse(Request, Response);
      SendPacket(Request);
    }

    return (Request != NULL);
  }
};
//---------------------------------------------------------------------------
class TSFTPFixedLenQueue : public TSFTPQueue
{
public:
  __fastcall TSFTPFixedLenQueue(TSFTPFileSystem * AFileSystem) : TSFTPQueue(AFileSystem)
  {
    FMissedRequests = 0;
  }
  virtual __fastcall ~TSFTPFixedLenQueue(){}

  bool Init(int QueueLen)
  {
    FMissedRequests = QueueLen - 1;
    return TSFTPQueue::Init();
  }

protected:
  int FMissedRequests;

  // sends as many requests as allowed by implementation
  virtual bool SendRequests()
  {
    bool Result = false;
    FMissedRequests++;
    while ((FMissedRequests > 0) && SendRequest())
    {
      Result = true;
      FMissedRequests--;
    }
    return Result;
  }
};
//---------------------------------------------------------------------------
class TSFTPAsynchronousQueue : public TSFTPQueue
{
public:
  __fastcall TSFTPAsynchronousQueue(TSFTPFileSystem * AFileSystem) : TSFTPQueue(AFileSystem)
  {
    RegisterReceiveHandler();
  }

  virtual __fastcall ~TSFTPAsynchronousQueue()
  {
    UnregisterReceiveHandler();
  }

  virtual void __fastcall Dispose(int ExpectedType)
  {
    // we do not want to receive asynchronous notifications anymore,
    // while waiting synchronously for pending responses
    UnregisterReceiveHandler();
    TSFTPQueue::Dispose(ExpectedType);
  }

  bool __fastcall Continue()
  {
    return SendRequest();
  }

protected:

  // event handler for incoming data
  void __fastcall ReceiveHandler(TObject * /*Sender*/)
  {
    while (// optimization only as we call ReceivePacket with TryOnly anyway
           FFileSystem->PeekPacket() &&
           ReceivePacketAsynchronously())
    {
      // loop
    }
  }

  virtual bool __fastcall ReceivePacketAsynchronously() = 0;

  // sends as many requests as allowed by implementation
  virtual bool SendRequests()
  {
    // noop
    return true;
  }

  bool __fastcall UnregisterReceiveHandler()
  {
    bool Result = FReceiveHandlerRegistered;
    if (Result)
    {
      FReceiveHandlerRegistered = false;
      FFileSystem->FSecureShell->UnregisterReceiveHandler(ReceiveHandler);
    }
    return Result;
  }

  void __fastcall RegisterReceiveHandler()
  {
    FFileSystem->FSecureShell->RegisterReceiveHandler(ReceiveHandler);
    FReceiveHandlerRegistered = true;
  }

private:
  bool FReceiveHandlerRegistered;
};
//---------------------------------------------------------------------------
class TSFTPDownloadQueue : public TSFTPFixedLenQueue
{
public:
  TSFTPDownloadQueue(TSFTPFileSystem * AFileSystem) :
    TSFTPFixedLenQueue(AFileSystem)
  {
  }
  virtual __fastcall ~TSFTPDownloadQueue(){}

  bool __fastcall Init(
    int QueueLen, const RawByteString & AHandle, __int64 Offset, __int64 PartSize, TFileOperationProgressType * AOperationProgress)
  {
    FHandle = AHandle;
    FOffset = Offset;
    FTransferred = Offset;
    FPartSize = PartSize;
    OperationProgress = AOperationProgress;

    return TSFTPFixedLenQueue::Init(QueueLen);
  }

  void __fastcall InitFillGapRequest(__int64 Offset, unsigned long Missing,
    TSFTPPacket * Packet)
  {
    InitRequest(Packet, Offset, Missing);
  }

  bool __fastcall ReceivePacket(TSFTPPacket * Packet, unsigned long & BlockSize)
  {
    void * Token;
    bool Result = TSFTPFixedLenQueue::ReceivePacket(Packet, SSH_FXP_DATA, asEOF, &Token);
    BlockSize = reinterpret_cast<unsigned long>(Token);
    return Result;
  }

protected:
  virtual bool __fastcall InitRequest(TSFTPQueuePacket * Request)
  {
    unsigned int BlockSize = FFileSystem->DownloadBlockSize(OperationProgress);
    if (FPartSize >= 0)
    {
      __int64 Remaining = (FOffset + FPartSize) - FTransferred;
      if (Remaining < BlockSize)
      {
        // It's lower, so the cast is safe
        BlockSize = static_cast<unsigned int>(Remaining);
      }
    }
    bool Result = (BlockSize > 0);
    if (Result)
    {
      InitRequest(Request, FTransferred, BlockSize);
      Request->Token = reinterpret_cast<void*>(BlockSize);
      FTransferred += BlockSize;
    }
    return Result;
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
  __int64 FOffset;
  __int64 FTransferred;
  __int64 FPartSize;
  RawByteString FHandle;
};
//---------------------------------------------------------------------------
class TSFTPUploadQueue : public TSFTPAsynchronousQueue
{
public:
  TSFTPUploadQueue(TSFTPFileSystem * AFileSystem, TEncryption * Encryption, int QueueMaxLen) :
    TSFTPAsynchronousQueue(AFileSystem),
    FEncryption(Encryption)
  {
    FStream = NULL;
    FOnTransferIn = NULL;
    OperationProgress = NULL;
    FLastBlockSize = 0;
    FEnd = false;
    FConvertToken = false;
    FQueueMaxLen = QueueMaxLen;
  }

  virtual __fastcall ~TSFTPUploadQueue()
  {
    delete FStream;
  }

  bool __fastcall Init(const UnicodeString & AFileName,
    HANDLE AFile, TTransferInEvent OnTransferIn, TFileOperationProgressType * AOperationProgress,
    const RawByteString AHandle, __int64 ATransferred,
    int ConvertParams)
  {
    FFileName = AFileName;
    if (OnTransferIn == NULL)
    {
      FStream = new TSafeHandleStream((THandle)AFile);
    }
    OperationProgress = AOperationProgress;
    FHandle = AHandle;
    FOnTransferIn = OnTransferIn;
    FTransferred = ATransferred;
    FConvertParams = ConvertParams;

    return TSFTPAsynchronousQueue::Init();
  }

  void __fastcall DisposeSafeWithErrorHandling()
  {
    DisposeSafe(SSH_FXP_STATUS);
  }

protected:
  virtual bool __fastcall InitRequest(TSFTPQueuePacket * Request)
  {
    TTerminal * FTerminal = FFileSystem->FTerminal;
    // Buffer for one block of data
    TFileBuffer BlockBuf;

    unsigned long BlockSize = FFileSystem->UploadBlockSize(FHandle, OperationProgress);
    bool Result = (BlockSize > 0);

    if (Result)
    {
      bool Last;
      if (FOnTransferIn != NULL)
      {
        size_t Read = BlockBuf.LoadFromIn(FOnTransferIn, FTerminal, BlockSize);
        Last = (Read < BlockSize);
      }
      else
      {
        FILE_OPERATION_LOOP_BEGIN
        {
          BlockBuf.LoadStream(FStream, BlockSize, false);
        }
        FILE_OPERATION_LOOP_END(FMTLOAD(READ_ERROR, (FFileName)));
        Last = (FStream->Position >= FStream->Size);
      }

      FEnd = (BlockBuf.Size == 0);
      Result = !FEnd;
      if (Result)
      {
        OperationProgress->AddLocallyUsed(BlockBuf.Size);

        // We do ASCII transfer: convert EOL of current block
        if (OperationProgress->AsciiTransfer)
        {
          __int64 PrevBufSize = BlockBuf.Size;
          BlockBuf.Convert(FTerminal->Configuration->LocalEOLType,
            FFileSystem->GetEOL(), FConvertParams, FConvertToken);
          // update transfer size with difference arised from EOL conversion
          OperationProgress->ChangeTransferSize(OperationProgress->TransferSize -
            PrevBufSize + BlockBuf.Size);
        }

        if (FFileSystem->FTerminal->Configuration->ActualLogProtocol >= 1)
        {
          FFileSystem->FTerminal->LogEvent(FORMAT(L"Write request offset: %d, len: %d",
            (int(FTransferred), int(BlockBuf.Size))));
        }

        if (FEncryption != NULL)
        {
          FEncryption->Encrypt(BlockBuf, Last);
        }

        Request->ChangeType(SSH_FXP_WRITE);
        Request->AddString(FHandle);
        Request->AddInt64(FTransferred);
        Request->AddData(BlockBuf.Data, BlockBuf.Size);
        FLastBlockSize = BlockBuf.Size;

        FTransferred += BlockBuf.Size;
      }
    }

    return Result;
  }

  virtual void __fastcall SendPacket(TSFTPQueuePacket * Packet)
  {
    TSFTPAsynchronousQueue::SendPacket(Packet);
    OperationProgress->AddTransferred(FLastBlockSize);
  }

  virtual void __fastcall ReceiveResponse(
    const TSFTPPacket * Packet, TSFTPPacket * Response, int ExpectedType,
    int AllowStatus, bool TryOnly)
  {
    TSFTPAsynchronousQueue::ReceiveResponse(Packet, Response, ExpectedType, AllowStatus, TryOnly);

    if (Response->Capacity > 0)
    {
      // particularly when uploading a file that completely fits into send buffer
      // over slow line, we may end up seemingly completing the transfer immediately
      // but hanging the application for a long time waiting for responses
      // (common is that the progress window would not even manage to draw itself,
      // showing that upload finished, before the application "hangs")
      FFileSystem->Progress(OperationProgress);
    }
    else
    {
      DebugAssert(TryOnly);
    }
  }

  virtual bool __fastcall ReceivePacketAsynchronously()
  {
    // do not read response to close request
    bool Result = (FRequests->Count > 0);
    if (Result)
    {
      // Try only: We cannot read from the socket here as we are already called
      // from TSecureShell::HandleNetworkEvents as it would cause a recursion
      // that would potentially make PuTTY code process the SSH packets in wrong order.
      ReceivePacket(NULL, SSH_FXP_STATUS, -1, NULL, true);
    }
    return Result;
  }

  virtual bool __fastcall End(TSFTPPacket * /*Response*/)
  {
    return FEnd;
  }

  virtual bool __fastcall SendRequest()
  {
    bool Result = TSFTPAsynchronousQueue::SendRequest();
    if (FRequests->Count >= FQueueMaxLen)
    {
      FFileSystem->FTerminal->LogEvent(1, L"Too many outstanding requests, waiting for responses...");
      DebugCheck(UnregisterReceiveHandler());
      try
      {
        DisposeUntil(FQueueMaxLen - 1, SSH_FXP_STATUS);
      }
      __finally
      {
        RegisterReceiveHandler();
      }
    }
    return Result;
  }

private:
  TStream * FStream;
  TTransferInEvent FOnTransferIn;
  TFileOperationProgressType * OperationProgress;
  UnicodeString FFileName;
  unsigned long FLastBlockSize;
  bool FEnd;
  __int64 FTransferred;
  RawByteString FHandle;
  bool FConvertToken;
  int FConvertParams;
  TEncryption * FEncryption;
  int FQueueMaxLen;
};
//---------------------------------------------------------------------------
class TSFTPLoadFilesPropertiesQueue : public TSFTPFixedLenQueue
{
public:
  TSFTPLoadFilesPropertiesQueue(TSFTPFileSystem * AFileSystem) :
    TSFTPFixedLenQueue(AFileSystem)
  {
    FIndex = 0;
  }
  virtual __fastcall ~TSFTPLoadFilesPropertiesQueue(){}

  bool __fastcall Init(int QueueLen, TStrings * FileList)
  {
    FFileList = FileList;

    return TSFTPFixedLenQueue::Init(QueueLen);
  }

  bool __fastcall ReceivePacket(TSFTPPacket * Packet, TRemoteFile *& File)
  {
    void * Token;
    bool Result = TSFTPFixedLenQueue::ReceivePacket(Packet, SSH_FXP_ATTRS, asAll, &Token);
    File = static_cast<TRemoteFile *>(Token);
    return Result;
  }

protected:
  virtual bool __fastcall InitRequest(TSFTPQueuePacket * Request)
  {
    bool Result = false;
    while (!Result && (FIndex < FFileList->Count))
    {
      TRemoteFile * File = static_cast<TRemoteFile *>(FFileList->Objects[FIndex]);
      FIndex++;

      bool MissingRights =
        (FFileSystem->FSupport->Loaded &&
         FLAGSET(FFileSystem->FSupport->AttributeMask, SSH_FILEXFER_ATTR_PERMISSIONS) &&
         File->Rights->Unknown);
      bool MissingOwnerGroup =
        (FFileSystem->FSecureShell->SshImplementation == sshiBitvise) ||
        (FFileSystem->FSupport->Loaded &&
         FLAGSET(FFileSystem->FSupport->AttributeMask, SSH_FILEXFER_ATTR_OWNERGROUP) &&
         (!File->Owner.IsSet || !File->Group.IsSet));

      Result = (MissingRights || MissingOwnerGroup);
      if (Result)
      {
        Request->ChangeType(SSH_FXP_LSTAT);
        FFileSystem->AddPathString(*Request, FFileSystem->LocalCanonify(File->FileName));
        if (FFileSystem->FVersion >= 4)
        {
          Request->AddCardinal(
            FLAGMASK(MissingRights, SSH_FILEXFER_ATTR_PERMISSIONS) |
            FLAGMASK(MissingOwnerGroup, SSH_FILEXFER_ATTR_OWNERGROUP));
        }
        Request->Token = File;
      }
    }

    return Result;
  }

  virtual bool __fastcall SendRequest()
  {
    bool Result =
      (FIndex < FFileList->Count) &&
      TSFTPFixedLenQueue::SendRequest();
    return Result;
  }

  virtual bool __fastcall End(TSFTPPacket * /*Response*/)
  {
    return (FRequests->Count == 0);
  }

private:
  TStrings * FFileList;
  int FIndex;
};
//---------------------------------------------------------------------------
class TSFTPCalculateFilesChecksumQueue : public TSFTPFixedLenQueue
{
public:
  TSFTPCalculateFilesChecksumQueue(TSFTPFileSystem * AFileSystem) :
    TSFTPFixedLenQueue(AFileSystem)
  {
    FIndex = 0;
  }
  virtual __fastcall ~TSFTPCalculateFilesChecksumQueue(){}

  bool __fastcall Init(int QueueLen, const UnicodeString & Alg, TStrings * FileList)
  {
    FAlg = Alg;
    FFileList = FileList;

    return TSFTPFixedLenQueue::Init(QueueLen);
  }

  bool __fastcall ReceivePacket(TSFTPPacket * Packet, TRemoteFile *& File)
  {
    void * Token;
    bool Result;
    try
    {
      Result = TSFTPFixedLenQueue::ReceivePacket(Packet, SSH_FXP_EXTENDED_REPLY, asNo, &Token);
    }
    __finally
    {
      File = static_cast<TRemoteFile *>(Token);
    }
    return Result;
  }

protected:
  virtual bool __fastcall InitRequest(TSFTPQueuePacket * Request)
  {
    bool Result = false;
    while (!Result && (FIndex < FFileList->Count))
    {
      TRemoteFile * File = static_cast<TRemoteFile *>(FFileList->Objects[FIndex]);
      DebugAssert(File != NULL);
      FIndex++;

      Result = !File->IsDirectory;
      if (Result)
      {
        DebugAssert(IsRealFile(File->FileName));

        Request->ChangeType(SSH_FXP_EXTENDED);
        Request->AddString(SFTP_EXT_CHECK_FILE_NAME);
        FFileSystem->AddPathString(*Request, FFileSystem->LocalCanonify(File->FullFileName));
        Request->AddString(FAlg);
        Request->AddInt64(0); // offset
        Request->AddInt64(0); // length (0 = till end)
        Request->AddCardinal(0); // block size (0 = no blocks or "one block")

        Request->Token = File;
      }
    }

    return Result;
  }

  virtual bool __fastcall SendRequest()
  {
    bool Result =
      (FIndex < FFileList->Count) &&
      TSFTPFixedLenQueue::SendRequest();
    return Result;
  }

  virtual bool __fastcall End(TSFTPPacket * /*Response*/)
  {
    return (FRequests->Count == 0);
  }

private:
  UnicodeString FAlg;
  TStrings * FFileList;
  int FIndex;
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
    DebugAssert(FFileSystem != NULL);
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
  UnicodeString FileName;
  UnicodeString RemoteFileName;
  UnicodeString RemoteFullFileName;
  TFileOperationProgressType * OperationProgress;
  const TCopyParamType * CopyParam;
  int Params;
  bool Resume;
  bool Resuming;
  TSFTPOverwriteMode OverwriteMode;
  __int64 DestFileSize; // output
  RawByteString RemoteFileHandle; // output
  TOverwriteFileParams * FileParams;
  bool Confirmed;
  bool DontRecycle;
  bool Recycled;
  TRights RecycledRights;
};
//===========================================================================
__fastcall TSFTPFileSystem::TSFTPFileSystem(TTerminal * ATerminal,
  TSecureShell * SecureShell):
  TCustomFileSystem(ATerminal)
{
  FSecureShell = SecureShell;
  FPacketReservations = new TList();
  ResetConnection();
  FBusy = 0;
  FAvoidBusy = false;
  FUtfStrings = asOff;
  FUtfDisablingAnnounced = true;
  FSignedTS = false;
  FSupport = new TSFTPSupport();
  FFixedPaths = NULL;
  FFileSystemInfoValid = false;

  FChecksumAlgs.reset(new TStringList());
  FChecksumSftpAlgs.reset(new TStringList());
  // List as defined by draft-ietf-secsh-filexfer-extensions-00
  // MD5 moved to the back
  RegisterChecksumAlg(Sha1ChecksumAlg, L"sha1");
  RegisterChecksumAlg(Sha224ChecksumAlg, L"sha224");
  RegisterChecksumAlg(Sha256ChecksumAlg, L"sha256");
  RegisterChecksumAlg(Sha384ChecksumAlg, L"sha384");
  RegisterChecksumAlg(Sha512ChecksumAlg, L"sha512");
  RegisterChecksumAlg(Md5ChecksumAlg, L"md5");
  RegisterChecksumAlg(Crc32ChecksumAlg, L"crc32");
}
//---------------------------------------------------------------------------
__fastcall TSFTPFileSystem::~TSFTPFileSystem()
{
  delete FSupport;
  // After closing, we can only possibly have "discard" reservations of the not-read responses to the last requests
  // (typically to SSH_FXP_CLOSE)
  for (int i = 0; i < FPacketReservations->Count; i++)
  {
    DebugAssert(FPacketReservations->Items[i] == NULL);
  }
  delete FPacketReservations;
  delete FFixedPaths;
  delete FSecureShell;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::Open()
{
  // this is used for reconnects only
  ResetConnection();
  FSecureShell->Open();
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::Close()
{
  FSecureShell->Close();
}
//---------------------------------------------------------------------------
bool __fastcall TSFTPFileSystem::GetActive()
{
  return FSecureShell->Active;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CollectUsage()
{
  FSecureShell->CollectUsage();

  UnicodeString VersionCounter;
  switch (FVersion)
  {
    case 0:
      VersionCounter = L"OpenedSessionsSFTP0";
      break;
    case 1:
      VersionCounter = L"OpenedSessionsSFTP1";
      break;
    case 2:
      VersionCounter = L"OpenedSessionsSFTP2";
      break;
    case 3:
      VersionCounter = L"OpenedSessionsSFTP3";
      break;
    case 4:
      VersionCounter = L"OpenedSessionsSFTP4";
      break;
    case 5:
      VersionCounter = L"OpenedSessionsSFTP5";
      break;
    case 6:
      VersionCounter = L"OpenedSessionsSFTP6";
      break;
    default:
      DebugFail();
  }
  FTerminal->Configuration->Usage->Inc(VersionCounter);
}
//---------------------------------------------------------------------------
const TSessionInfo & __fastcall TSFTPFileSystem::GetSessionInfo()
{
  return FSecureShell->GetSessionInfo();
}
//---------------------------------------------------------------------------
const TFileSystemInfo & __fastcall TSFTPFileSystem::GetFileSystemInfo(bool /*Retrieve*/)
{
  if (!FFileSystemInfoValid)
  {
    FFileSystemInfo.AdditionalInfo = L"";

    if (!IsCapable(fcRename))
    {
      FFileSystemInfo.AdditionalInfo += LoadStr(FS_RENAME_NOT_SUPPORTED) + L"\r\n\r\n";
    }

    if (!FExtensions.IsEmpty())
    {
      FFileSystemInfo.AdditionalInfo +=
        LoadStr(SFTP_EXTENSION_INFO) + L"\r\n" +
        FExtensions;
    }
    else
    {
      FFileSystemInfo.AdditionalInfo += LoadStr(SFTP_NO_EXTENSION_INFO) + L"\r\n";
    }

    FFileSystemInfo.ProtocolBaseName = L"SFTP";
    FFileSystemInfo.ProtocolName = FMTLOAD(SFTP_PROTOCOL_NAME2, (FVersion));
    FTerminal->SaveCapabilities(FFileSystemInfo);

    FFileSystemInfoValid = true;
  }

  return FFileSystemInfo;
}
//---------------------------------------------------------------------------
bool __fastcall TSFTPFileSystem::TemporaryTransferFile(const UnicodeString & FileName)
{
  return (GetPartialFileExtLen(FileName) > 0);
}
//---------------------------------------------------------------------------
bool __fastcall TSFTPFileSystem::GetStoredCredentialsTried()
{
  return FSecureShell->GetStoredCredentialsTried();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSFTPFileSystem::GetUserName()
{
  return FSecureShell->UserName;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::Idle()
{
  // Keep session alive
  if ((FTerminal->SessionData->PingType != ptOff) &&
      (Now() - FSecureShell->LastDataSent > FTerminal->SessionData->PingIntervalDT))
  {
    if ((FTerminal->SessionData->PingType == ptDummyCommand) &&
        FSecureShell->Ready)
    {
      FTerminal->LogEvent(L"Sending dummy command to keep session alive.");
      TSFTPPacket Packet(SSH_FXP_REALPATH);
      AddPathString(Packet, L"/");
      SendPacketAndReceiveResponse(&Packet, &Packet);
    }
    else
    {
      FSecureShell->KeepAlive();
    }
  }

  FSecureShell->Idle();
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ResetConnection()
{
  FPacketReservations->Clear();
  FPacketNumbers = VarArrayCreate(OPENARRAY(int, (0, 1)), varLongWord);
  FNotLoggedRequests.clear();
  FPreviousLoggedPacket = 0;
  FNotLoggedWritePackets = FNotLoggedReadPackets = FNotLoggedStatusPackets = FNotLoggedDataPackets = 0;
}
//---------------------------------------------------------------------------
bool __fastcall TSFTPFileSystem::IsCapable(int Capability) const
{
  DebugAssert(FTerminal);
  switch (Capability) {
    case fcAnyCommand:
    case fcShellAnyCommand:
    case fcLocking:
    case fcAclChangingFiles: // pending implementation
    case fcMoveOverExistingFile:
    case fcTags:
      return false;

    case fcNewerOnlyUpload:
    case fcTimestampChanging:
    case fcIgnorePermErrors:
    case fcPreservingTimestampUpload:
    case fcSecondaryShell:
    case fcRemoveCtrlZUpload:
    case fcRemoveBOMUpload:
    case fcPreservingTimestampDirs:
    case fcTransferOut:
    case fcTransferIn:
      return true;

    case fcMoveToQueue:
    case fcResumeSupport:
    case fcSkipTransfer:
    case fcParallelTransfers:
    case fcParallelFileTransfers:
      return !FTerminal->IsEncryptingFiles();

    case fcRename:
    case fcRemoteMove:
      return (FVersion >= 2);

    case fcSymbolicLink:
    case fcResolveSymlink:
      return (FVersion >= 3) && !FTerminal->IsEncryptingFiles();

    case fcModeChanging:
    case fcModeChangingUpload:
      return !FSupport->Loaded ||
        FLAGSET(FSupport->AttributeMask, SSH_FILEXFER_ATTR_PERMISSIONS);

    case fcGroupOwnerChangingByID:
      return (FVersion <= 3);

    case fcOwnerChanging:
    case fcGroupChanging:
      return
        (FVersion <= 3) ||
        ((FVersion >= 4) &&
         (!FSupport->Loaded ||
          FLAGSET(FSupport->AttributeMask, SSH_FILEXFER_ATTR_OWNERGROUP)));

    case fcNativeTextMode:
      return !FTerminal->IsEncryptingFiles() && (FVersion >= 4);

    case fcTextMode:
      return
        !FTerminal->IsEncryptingFiles() &&
        ((FVersion >= 4) ||
         (strcmp(GetEOL(), EOLToStr(FTerminal->Configuration->LocalEOLType)) != 0));

    case fcUserGroupListing:
      return SupportsExtension(SFTP_EXT_OWNER_GROUP);

    case fcLoadingAdditionalProperties:
      // We allow loading properties only, if "supported" extension is supported and
      // the server supports "permissions" and/or "owner/group" attributes
      // (no other attributes are loaded).
      // This is here only because of VShell
      // (it supports owner/group, but does not include them into response to
      // SSH_FXP_READDIR)
      // and Bitvise (the same as VShell, but it does not even bother to provide "supported" extension until 6.21)
      // No other use is known.
      return
        (FSupport->Loaded &&
         ((FSupport->AttributeMask &
           (SSH_FILEXFER_ATTR_PERMISSIONS | SSH_FILEXFER_ATTR_OWNERGROUP)) != 0)) ||
        (FSecureShell->SshImplementation == sshiBitvise);

    case fcCheckingSpaceAvailable:
      return
        // extension announced in extension list of by
        // SFTP_EXT_SUPPORTED/SFTP_EXT_SUPPORTED2 extension
        // (SFTP version 5 and newer only)
        SupportsExtension(SFTP_EXT_SPACE_AVAILABLE) ||
        // extension announced by proprietary SFTP_EXT_STATVFS extension
        FSupportsStatVfsV2 ||
        // Bitvise (until 6.21) fails to report it's supported extensions.
        (FSecureShell->SshImplementation == sshiBitvise);

    case fcCalculatingChecksum:
      return
        !FTerminal->IsEncryptingFiles() &&
        (// Specification says that "check-file" should be announced,
         // yet Vandyke VShell (as of 4.0.3) announce "check-file-name"
         SupportsExtension(SFTP_EXT_CHECK_FILE) ||
         SupportsExtension(SFTP_EXT_CHECK_FILE_NAME) ||
         // see above
         (FSecureShell->SshImplementation == sshiBitvise));

    case fcRemoteCopy:
      return
        // Implemented by ProFTPD/mod_sftp, OpenSSH (since 9.0) and Bitvise WinSSHD (without announcing it)
        SupportsExtension(SFTP_EXT_COPY_FILE) ||
        SupportsExtension(SFTP_EXT_COPY_DATA) ||
        // see above
        (FSecureShell->SshImplementation == sshiBitvise);

    case fcHardLink:
      return
        (FVersion >= 6) ||
        FSupportsHardlink;

    case fcChangePassword:
      return FSecureShell->CanChangePassword();

    default:
      DebugFail();
      return false;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TSFTPFileSystem::SupportsExtension(const UnicodeString & Extension) const
{
  return (FSupportedExtensions->IndexOf(Extension) >= 0);
}
//---------------------------------------------------------------------------
inline void __fastcall TSFTPFileSystem::BusyStart()
{
  if (FBusy == 0 && FTerminal->UseBusyCursor && !FAvoidBusy)
  {
    FBusyToken = ::BusyStart();
  }
  FBusy++;
  DebugAssert(FBusy < 10);
}
//---------------------------------------------------------------------------
inline void __fastcall TSFTPFileSystem::BusyEnd()
{
  DebugAssert(FBusy > 0);
  FBusy--;
  if (FBusy == 0 && FTerminal->UseBusyCursor && !FAvoidBusy)
  {
    ::BusyEnd(FBusyToken);
    FBusyToken = NULL;
  }
}
//---------------------------------------------------------------------------
// size + message number + type
const unsigned long SFTPPacketOverhead = 4 + 4 + 1;
//---------------------------------------------------------------------------
unsigned long __fastcall TSFTPFileSystem::TransferBlockSize(
  unsigned long Overhead, TFileOperationProgressType * OperationProgress)
{
  const unsigned long MinPacketSize = 32768;
  unsigned long AMaxPacketSize = FSecureShell->MaxPacketSize();
  bool MaxPacketSizeValid = (AMaxPacketSize > 0);
  unsigned long CPSRounded = TEncryption::RoundToBlock(OperationProgress->CPS());
  unsigned long Result = CPSRounded;

  if ((FMaxPacketSize > 0) &&
      ((FMaxPacketSize < AMaxPacketSize) || !MaxPacketSizeValid))
  {
    AMaxPacketSize = FMaxPacketSize;
    MaxPacketSizeValid = true;
  }

  if (Result == 0)
  {
    Result = OperationProgress->StaticBlockSize();
  }

  if (Result < MinPacketSize)
  {
    Result = MinPacketSize;
  }

  if (MaxPacketSizeValid)
  {
    Overhead += SFTPPacketOverhead;
    if (AMaxPacketSize < Overhead)
    {
      // do not send another request
      // (generally should happen only if upload buffer is full)
      Result = 0;
    }
    else
    {
      AMaxPacketSize -= Overhead;
      if (Result > AMaxPacketSize)
      {
        unsigned int MaxPacketSizeRounded = TEncryption::RoundToBlockDown(AMaxPacketSize);
        if (MaxPacketSizeRounded > 0)
        {
          AMaxPacketSize = MaxPacketSizeRounded;
        }
        Result = AMaxPacketSize;
      }
    }
  }

  Result = OperationProgress->AdjustToCPSLimit(Result);

  return Result;
}
//---------------------------------------------------------------------------
unsigned long __fastcall TSFTPFileSystem::UploadBlockSize(const RawByteString & Handle,
  TFileOperationProgressType * OperationProgress)
{
  // handle length + offset + data size
  const unsigned long UploadPacketOverhead =
    sizeof(unsigned long) + sizeof(__int64) + sizeof(unsigned long);
  return TransferBlockSize(UploadPacketOverhead + Handle.Length(), OperationProgress);
}
//---------------------------------------------------------------------------
unsigned long __fastcall TSFTPFileSystem::DownloadBlockSize(
  TFileOperationProgressType * OperationProgress)
{
  unsigned long Result = TransferBlockSize(sizeof(unsigned long), OperationProgress);
  if (FSupport->Loaded && (FSupport->MaxReadSize > 0) &&
      (Result > FSupport->MaxReadSize))
  {
    Result = FSupport->MaxReadSize;
  }
  // Never ask for more than we can accept (overhead here should correctly not include the "size" field)
  if (Result + SFTPPacketOverhead > SFTP_MAX_PACKET_LEN)
  {
    Result = SFTP_MAX_PACKET_LEN - SFTPPacketOverhead;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::Progress(TFileOperationProgressType * OperationProgress)
{
  FTerminal->Progress(OperationProgress);
}
//---------------------------------------------------------------------------
void TSFTPFileSystem::LogPacket(const TSFTPPacket * Packet, TLogLineType Type)
{
  std::vector<UnicodeString> NotLogged;
  #define ADD_NOT_LOGGED(V, N) \
    if (FNotLogged##V##Packets > 0) \
    { \
      NotLogged.push_back(FORMAT(L"%d SSH_FXP_"#N, (FNotLogged##V##Packets))); \
      FNotLogged##V##Packets = 0; \
    }
  ADD_NOT_LOGGED(Write, WRITE);
  ADD_NOT_LOGGED(Read, READ);
  ADD_NOT_LOGGED(Data, DATA);
  ADD_NOT_LOGGED(Status, STATUS);
  if (!NotLogged.empty())
  {
    UnicodeString S;
    for (size_t Index = 0; Index < NotLogged.size(); Index++)
    {
      AddToList(S, NotLogged[Index], (Index < NotLogged.size() - 1 ? L", " : L" and "));
    }
    FTerminal->LogEvent(FORMAT(L"Skipped %s packets", (S)));
  }
  FTerminal->Log->Add(
    Type, FORMAT(L"Type: %s, Size: %d, Number: %d", (Packet->TypeName, (int)Packet->Length, (int)Packet->MessageNumber)));
  if (FTerminal->Configuration->ActualLogProtocol >= 2)
  {
    FTerminal->Log->Add(Type, Packet->Dump());
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SendPacket(const TSFTPPacket * Packet)
{
  // putting here for a lack of better place
  if (!FUtfDisablingAnnounced && (FUtfStrings == asOff))
  {
    FTerminal->LogEvent(L"Strings received in non-UTF-8 encoding in a previous packet, will not use UTF-8 anymore");
    FUtfDisablingAnnounced = true;
  }

  BusyStart();
  try
  {
    if (FTerminal->Log->Logging && (FTerminal->Configuration->ActualLogProtocol >= 0))
    {
      if ((FPreviousLoggedPacket != SSH_FXP_READ &&
           FPreviousLoggedPacket != SSH_FXP_WRITE) ||
          (Packet->Type != FPreviousLoggedPacket) ||
          (FTerminal->Configuration->ActualLogProtocol >= 1))
      {
        LogPacket(Packet, llInput);
        FPreviousLoggedPacket = Packet->Type;
      }
      else
      {
        if (Packet->Type == SSH_FXP_WRITE)
        {
          FNotLoggedWritePackets++;
        }
        else if (DebugAlwaysTrue(Packet->Type == SSH_FXP_READ))
        {
          FNotLoggedReadPackets++;
        }
        FNotLoggedRequests.insert(Packet->MessageNumber);
      }
    }
    FSecureShell->Send(Packet->SendData, Packet->SendLength);
  }
  __finally
  {
    BusyEnd();
  }
}
//---------------------------------------------------------------------------
unsigned long __fastcall TSFTPFileSystem::GotStatusPacket(
  TSFTPPacket * Packet, int AllowStatus, bool DoNotForceLog)
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
    SFTP_STATUS_UNKNOWN_PRINCIPAL,
    SFTP_STATUS_LOCK_CONFLICT,
    SFTP_STATUS_DIR_NOT_EMPTY,
    SFTP_STATUS_NOT_A_DIRECTORY,
    SFTP_STATUS_INVALID_FILENAME,
    SFTP_STATUS_LINK_LOOP,
    SFTP_STATUS_CANNOT_DELETE,
    SFTP_STATUS_INVALID_PARAMETER,
    SFTP_STATUS_FILE_IS_A_DIRECTORY,
    SFTP_STATUS_BYTE_RANGE_LOCK_CONFLICT,
    SFTP_STATUS_BYTE_RANGE_LOCK_REFUSED,
    SFTP_STATUS_DELETE_PENDING,
    SFTP_STATUS_FILE_CORRUPT,
    SFTP_STATUS_OWNER_INVALID,
    SFTP_STATUS_GROUP_INVALID,
    SFTP_STATUS_NO_MATCHING_BYTE_RANGE_LOCK
  };
  int Message;
  if ((AllowStatus & (0x01 << Code)) == 0)
  {
    if (Code >= LENOF(Messages))
    {
      Message = SFTP_STATUS_UNKNOWN;
    }
    else
    {
      Message = Messages[Code];
    }
    UnicodeString MessageStr = LoadStr(Message);
    UnicodeString ServerMessage;
    UnicodeString LanguageTag;
    // Message field is defined since version 3 only.
    // We also might get the packet even before the version is established.
    // And Cisco servers respond without message field even in version 3.
    if (Packet->RemainingLength > 0)
    {
      // message is in UTF only since SFTP specification 01 (specification 00
      // is also version 3)
      // (in other words, always use UTF unless server is known to be buggy)
      ServerMessage = Packet->GetString(FUtfStrings);
      // SSH-2.0-Maverick_SSHD and SSH-2.0-CIGNA SFTP Server Ready! omit the language tag
      // and I believe I've seen one more server doing the same.
      if (Packet->RemainingLength > 0)
      {
        LanguageTag = Packet->GetAnsiString();
        if ((FVersion >= 5) && (Message == SFTP_STATUS_UNKNOWN_PRINCIPAL))
        {
          UnicodeString Principals;
          while (Packet->GetNextData() != NULL)
          {
            if (!Principals.IsEmpty())
            {
              Principals += L", ";
            }
            Principals += Packet->GetAnsiString();
          }
          MessageStr = FORMAT(MessageStr, (Principals));
        }
      }
    }
    else
    {
      ServerMessage = LoadStr(SFTP_SERVER_MESSAGE_UNSUPPORTED);
    }
    if (FTerminal->Log->Logging &&
        (FTerminal->Configuration->ActualLogProtocol >= 0))
    {
      FTerminal->Log->Add(llOutput, FORMAT(L"Status code: %d, Message: %d, Server: %s, Language: %s ",
        (int(Code), (int)Packet->MessageNumber, ServerMessage, LanguageTag)));
    }
    if (!LanguageTag.IsEmpty())
    {
      LanguageTag = FORMAT(L" (%s)", (LanguageTag));
    }
    UnicodeString HelpKeyword;
    switch (Code)
    {
      case SSH_FX_FAILURE:
        HelpKeyword = HELP_SFTP_STATUS_FAILURE;
        break;

      case SSH_FX_PERMISSION_DENIED:
        HelpKeyword = HELP_SFTP_STATUS_PERMISSION_DENIED;
        break;
    }
    UnicodeString Error = FMTLOAD(SFTP_ERROR_FORMAT3, (MessageStr,
      int(Code), LanguageTag, ServerMessage));
    if (Code == SSH_FX_FAILURE)
    {
      FTerminal->Configuration->Usage->Inc(L"SftpFailureErrors");
      Error += L"\n\n" + LoadStr(SFTP_STATUS_4);
    }
    FTerminal->TerminalError(NULL, Error, HelpKeyword);
    UNREACHABLE_AFTER_NORETURN(return 0);
  }
  else
  {
    if (!DoNotForceLog || (Code != 0))
    {
      if (FTerminal->Configuration->ActualLogProtocol >= 0)
      {
        FTerminal->Log->Add(llOutput, FORMAT(L"Status code: %d", ((int)Code)));
      }
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
    DebugAssert(Packet->ReservedBy == this);
    Packet->ReservedBy = NULL;
  }
  FPacketReservations->Delete(Reservation);
}
//---------------------------------------------------------------------------
inline int __fastcall TSFTPFileSystem::PacketLength(unsigned char * LenBuf, int ExpectedType)
{
  int Length = GET_32BIT(LenBuf);
  if (Length > SFTP_MAX_PACKET_LEN)
  {
    UnicodeString Message = FMTLOAD(SFTP_PACKET_TOO_BIG, (
      int(Length), SFTP_MAX_PACKET_LEN));
    if (ExpectedType == SSH_FXP_VERSION)
    {
      RawByteString LenString(reinterpret_cast<char *>(LenBuf), 4);
      Message = FMTLOAD(SFTP_PACKET_TOO_BIG_INIT_EXPLAIN,
        (Message, DisplayableStr(LenString)));
    }
    FTerminal->FatalError(NULL, Message, HELP_SFTP_PACKET_TOO_BIG);
  }
  return Length;
}
//---------------------------------------------------------------------------
bool __fastcall TSFTPFileSystem::PeekPacket()
{
  bool Result;
  unsigned char * Buf;
  Result = FSecureShell->Peek(Buf, 4);
  if (Result)
  {
    int Length = PacketLength(Buf, -1);
    Result = FSecureShell->Peek(Buf, 4 + Length);
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TSFTPFileSystem::ReceivePacket(TSFTPPacket * Packet,
  int ExpectedType, int AllowStatus, bool TryOnly)
{
  TSFTPBusy Busy(this);

  int Result = SSH_FX_OK;
  int Reservation = FPacketReservations->IndexOf(Packet);
  bool NotLogged = false; // shut up

  if (Reservation < 0 || Packet->Capacity == 0)
  {
    bool IsReserved;
    do
    {
      IsReserved = false;
      NotLogged = false;

      DebugAssert(Packet);

      if (TryOnly && !PeekPacket())
      {
        // Reset packet in case it was filled by previous out-of-order
        // reserved packet
        *Packet = TSFTPPacket();
      }
      else
      {
        unsigned char LenBuf[4];
        FSecureShell->Receive(LenBuf, sizeof(LenBuf));
        int Length = PacketLength(LenBuf, ExpectedType);
        Packet->Capacity = Length;
        FSecureShell->Receive(Packet->Data, Length);
        Packet->DataUpdated(Length);

        bool ResponseToNotLoggedRequest = (FNotLoggedRequests.erase(Packet->MessageNumber) != 0);
        if (FTerminal->Log->Logging && (FTerminal->Configuration->ActualLogProtocol >= 0))
        {
          if (!ResponseToNotLoggedRequest ||
              (Packet->Type != SSH_FXP_STATUS && Packet->Type != SSH_FXP_DATA) ||
              (FTerminal->Configuration->ActualLogProtocol >= 1))
          {
            LogPacket(Packet, llOutput);
          }
          else
          {
            if (Packet->Type == SSH_FXP_STATUS)
            {
              FNotLoggedStatusPackets++;
            }
            else if (DebugAlwaysTrue(Packet->Type == SSH_FXP_DATA))
            {
              FNotLoggedDataPackets++;
            }
            NotLogged = true; // used only for the response we wait for
          }
        }

        if (Reservation < 0 ||
            Packet->MessageNumber != (unsigned int)FPacketNumbers.GetElement(Reservation))
        {
          TSFTPPacket * ReservedPacket;
          unsigned int MessageNumber;
          for (int Index = 0; Index < FPacketReservations->Count; Index++)
          {
            MessageNumber = (unsigned int)FPacketNumbers.GetElement(Index);
            if (MessageNumber == Packet->MessageNumber)
            {
              ReservedPacket = (TSFTPPacket *)FPacketReservations->Items[Index];
              IsReserved = true;
              if (ReservedPacket)
              {
                FTerminal->LogEvent(0, L"Storing reserved response");
                *ReservedPacket = *Packet;
              }
              else
              {
                FTerminal->LogEvent(0, L"Discarding reserved response");
                RemoveReservation(Index);
                if ((Reservation >= 0) && (Reservation > Index))
                {
                  Reservation--;
                  DebugAssert(Reservation == FPacketReservations->IndexOf(Packet));
                }
              }
              break;
            }
          }
        }
      }
    }
    while (IsReserved);
  }

  if ((Packet->Capacity == 0) && DebugAlwaysTrue(TryOnly))
  {
    // noop
  }
  else
  {
    // before we removed the reservation after check for packet type,
    // but if it raises exception, removal is unnecessarily
    // postponed until the packet is removed
    // (and it have not worked anyway until recent fix to UnreserveResponse)
    if (Reservation >= 0)
    {
      DebugAssert(Packet->MessageNumber == (unsigned int)FPacketNumbers.GetElement(Reservation));
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
        Result = GotStatusPacket(Packet, AllowStatus, NotLogged);
      }
      else if (ExpectedType != Packet->Type)
      {
        FTerminal->FatalError(NULL, FMTLOAD(SFTP_INVALID_TYPE, ((int)Packet->Type)));
      }
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
    DebugAssert(FPacketReservations->IndexOf(Response) < 0);
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
      // we must remember that the response was expected, so we skip it
      // in receivepacket()
      FPacketReservations->Items[Reservation] = NULL;
    }
  }
}
//---------------------------------------------------------------------------
int __fastcall TSFTPFileSystem::ReceiveResponse(
  const TSFTPPacket * Packet, TSFTPPacket * Response, int ExpectedType,
  int AllowStatus, bool TryOnly)
{
  int Result;
  unsigned int MessageNumber = Packet->MessageNumber;
  TSFTPPacket * AResponse = (Response ? Response : new TSFTPPacket());
  try
  {
    Result = ReceivePacket(AResponse, ExpectedType, AllowStatus, TryOnly);
    if (MessageNumber != AResponse->MessageNumber)
    {
      FTerminal->FatalError(NULL, FMTLOAD(SFTP_MESSAGE_NUMBER,
        ((int)AResponse->MessageNumber, (int)MessageNumber)));
    }
  }
  __finally
  {
    if (!Response)
    {
      delete AResponse;
    }
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
UnicodeString __fastcall TSFTPFileSystem::RealPath(const UnicodeString & Path)
{
  if (FTerminal->SessionData->SFTPRealPath == asOff)
  {
    return LocalCanonify(Path);
  }
  else
  {
    try
    {
      FTerminal->LogEvent(0, FORMAT(L"Getting real path for '%s'", (Path)));

      TSFTPPacket Packet(SSH_FXP_REALPATH);
      AddPathString(Packet, Path);

      // In SFTP-6 new optional field control-byte is added that defaults to
      // SSH_FXP_REALPATH_NO_CHECK=0x01, meaning it won't fail, if the path does not exist.
      // That differs from SFTP-5 recommendation that
      // "The server SHOULD fail the request if the path is not present on the server."
      // Earlier versions had no recommendation, though canonical SFTP-3 implementation
      // in OpenSSH fails.

      // While we really do not care much, we anyway set the flag to ~ & 0x01 to make the request fail.
      // First for consistency.
      // Second to workaround a bug in ProFTPD/mod_sftp version 1.3.5rc1 through 1.3.5-stable
      // that sends a completely malformed response for non-existing paths,
      // when SSH_FXP_REALPATH_NO_CHECK (even implicitly) is used.
      // See http://bugs.proftpd.org/show_bug.cgi?id=4160

      // Note that earlier drafts of SFTP-6 (filexfer-07 and -08) had optional compose-path field
      // before control-byte field. If we ever use this against a server conforming to those drafts,
      // it may cause trouble.
      if (FVersion >= 6)
      {
        if (FSecureShell->SshImplementation != sshiProFTPD)
        {
          Packet.AddByte(SSH_FXP_REALPATH_STAT_ALWAYS);
        }
        else
        {
          // Cannot use SSH_FXP_REALPATH_STAT_ALWAYS as ProFTPD does wrong bitwise test
          // so it incorrectly evaluates SSH_FXP_REALPATH_STAT_ALWAYS (0x03) as
          // SSH_FXP_REALPATH_NO_CHECK (0x01). The only value conforming to the
          // specification, yet working with ProFTPD is SSH_FXP_REALPATH_STAT_IF (0x02).
          Packet.AddByte(SSH_FXP_REALPATH_STAT_IF);
        }
      }
      SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_NAME);
      if (Packet.GetCardinal() != 1)
      {
        FTerminal->FatalError(NULL, LoadStr(SFTP_NON_ONE_FXP_NAME_PACKET));
      }

      UnicodeString RealDir = UnixExcludeTrailingBackslash(Packet.GetPathString(FUtfStrings));
      // do not cache, as particularly when called from CreateDirectory > Canonify,
      // we would cache an unencrypted path to a directory we want to create encrypted,
      // what would prevent the encryption later.
      RealDir = FTerminal->DecryptFileName(RealDir, true, true);
      // ignore rest of SSH_FXP_NAME packet

      FTerminal->LogEvent(0, FORMAT(L"Real path is '%s'", (RealDir)));

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
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSFTPFileSystem::RealPath(const UnicodeString & Path, const UnicodeString & BaseDir)
{
  UnicodeString APath;

  if (UnixIsAbsolutePath(Path))
  {
    APath = Path;
  }
  else
  {
    if (!Path.IsEmpty())
    {
      // this condition/block was outside (before) current block
      // but it did not work when Path was empty
      if (!BaseDir.IsEmpty())
      {
        APath = UnixIncludeTrailingBackslash(BaseDir);
      }
      APath = APath + Path;
    }
    if (APath.IsEmpty()) APath = UnixIncludeTrailingBackslash(L".");
  }
  return RealPath(APath);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSFTPFileSystem::LocalCanonify(const UnicodeString & Path)
{
  // TODO: improve (handle .. etc.)
  if (UnixIsAbsolutePath(Path) ||
      (!FCurrentDirectory.IsEmpty() && UnixSamePath(FCurrentDirectory, Path)))
  {
    return Path;
  }
  else
  {
    return ::AbsolutePath(FCurrentDirectory, Path);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSFTPFileSystem::Canonify(const UnicodeString & AAPath)
{
  // inspired by canonify() from PSFTP.C
  UnicodeString Result;
  FTerminal->LogEvent(FORMAT(L"Canonifying: \"%s\"", (AAPath)));
  UnicodeString Path = LocalCanonify(AAPath);
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
    UnicodeString APath = UnixExcludeTrailingBackslash(Path);
    UnicodeString Name = UnixExtractFileName(APath);
    if (!IsRealFile(Name))
    {
      Result = Path;
    }
    else
    {
      UnicodeString FPath = UnixExtractFilePath(APath);
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

  FTerminal->LogEvent(FORMAT(L"Canonified: \"%s\"", (Result)));

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSFTPFileSystem::AbsolutePath(UnicodeString Path, bool Local)
{
  if (Local)
  {
    return LocalCanonify(Path);
  }
  else
  {
    return RealPath(Path, CurrentDirectory);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSFTPFileSystem::GetHomeDirectory()
{
  if (FHomeDirectory.IsEmpty())
  {
    FHomeDirectory = RealPath(L".");
    // Prevent infinite recursion when the server is broken
    if (FHomeDirectory.IsEmpty())
    {
      FHomeDirectory = ROOTDIRECTORY;
    }
  }
  return FHomeDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::LoadFile(TRemoteFile * File, TSFTPPacket * Packet,
  bool Complete)
{
  Packet->GetFile(File, FVersion, FTerminal->SessionData->DSTMode, FUtfStrings, FSignedTS, Complete);
}
//---------------------------------------------------------------------------
TRemoteFile * __fastcall TSFTPFileSystem::LoadFile(TSFTPPacket * Packet,
  TRemoteFile * ALinkedByFile, const UnicodeString FileName,
  TRemoteFileList * TempFileList, bool Complete)
{
  TRemoteFile * File = new TRemoteFile(ALinkedByFile);
  try
  {
    File->Terminal = FTerminal;
    if (!FileName.IsEmpty())
    {
      File->FileName = FileName;
    }
    // to get full path for symlink completion
    File->Directory = TempFileList;
    LoadFile(File, Packet, Complete);
    File->Directory = NULL;
  }
  catch(...)
  {
    delete File;
    throw;
  }
  return File;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSFTPFileSystem::GetCurrentDirectory()
{
  return FCurrentDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::DoStartup()
{
  // do not know yet
  FVersion = -1;
  FFileSystemInfoValid = false;
  TSFTPPacket Packet(SSH_FXP_INIT);
  int MaxVersion = FTerminal->SessionData->SFTPMaxVersion;
  if (MaxVersion == SFTPMaxVersionAuto)
  {
    TSshImplementation SshImplementation = FSecureShell->SshImplementation;
    if ((SshImplementation == sshiOpenSSH) || (SshImplementation == sshiProFTPD) || (SshImplementation == sshiBitvise))
    {
      MaxVersion = SFTPMaxVersion;
      FTerminal->LogEvent(FORMAT(L"Well known server, allowing SFTP version %d.", (MaxVersion)));
    }
    else
    {
      MaxVersion = SFTPStandardVersion;
      FTerminal->LogEvent(FORMAT(L"Not well known server, limiting to safe SFTP version %d.", (MaxVersion)));
    }
  }
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
    FTerminal->FatalError(&E, LoadStr(SFTP_INITIALIZE_ERROR), HELP_SFTP_INITIALIZE_ERROR);
  }

  FVersion = Packet.GetCardinal();
  FTerminal->LogEvent(FORMAT(L"SFTP version %d negotiated.", (FVersion)));
  if (FVersion > MaxVersion)
  {
    // This happens with ProFTPD:
    // https://github.com/proftpd/proftpd/issues/1200
    FTerminal->LogEvent(L"Got higher version than asked for.");
  }
  if (FVersion < SFTPMinVersion || FVersion > SFTPMaxVersion)
  {
    FTerminal->FatalError(NULL, FMTLOAD(SFTP_VERSION_NOT_SUPPORTED,
      (FVersion, SFTPMinVersion, SFTPMaxVersion)));
  }

  FExtensions = EmptyStr;
  FEOL = "\r\n";
  FSupport->Loaded = false;
  FSupportsStatVfsV2 = false;
  FSupportsHardlink = false;
  bool SupportsLimits = false;
  SAFE_DESTROY(FFixedPaths);
  // OpenSSH announce extensions directly in the SSH_FXP_VERSION packet only.
  // Bitvise uses "supported2" extension for some (mostly the standard ones) and SSH_FXP_VERSION for other.
  // ProFTPD uses "supported2" extension for the standard extensions. And repeats them along with non-standard in the SSH_FXP_VERSION.
  std::unique_ptr<TStrings> SupportedExtensions(new TStringList());

  if (FVersion >= 3)
  {
    while (Packet.GetNextData() != NULL)
    {
      UnicodeString ExtensionName = Packet.GetAnsiString();
      RawByteString ExtensionData = Packet.GetRawByteString();
      UnicodeString ExtensionDisplayData = DisplayableStr(ExtensionData);

      if (ExtensionName == SFTP_EXT_NEWLINE)
      {
        FEOL = AnsiString(ExtensionData);
        FTerminal->LogEvent(FORMAT(L"Server requests EOL sequence %s.",
          (ExtensionDisplayData)));
        if (FEOL.Length() < 1 || FEOL.Length() > 2)
        {
          FTerminal->FatalError(NULL, FMTLOAD(SFTP_INVALID_EOL, (ExtensionDisplayData)));
        }
      }
      // do not allow "supported" to override "supported2" if both are received
      else if (((ExtensionName == SFTP_EXT_SUPPORTED) && !FSupport->Loaded) ||
               (ExtensionName == SFTP_EXT_SUPPORTED2))
      {
        FSupport->Reset();
        TSFTPPacket SupportedStruct(ExtensionData);
        FSupport->Loaded = true;
        FSupport->AttributeMask = SupportedStruct.GetCardinal();
        FSupport->AttributeBits = SupportedStruct.GetCardinal();
        FSupport->OpenFlags = SupportedStruct.GetCardinal();
        FSupport->AccessMask = SupportedStruct.GetCardinal();
        FSupport->MaxReadSize = SupportedStruct.GetCardinal();
        std::unique_ptr<TStrings> ExtensionsLog(new TStringList());
        if (ExtensionName == SFTP_EXT_SUPPORTED)
        {
          while (SupportedStruct.GetNextData() != NULL)
          {
            UnicodeString Extension = SupportedStruct.GetAnsiString();
            ExtensionsLog->Add(Extension);
            SupportedExtensions->Add(Extension);
          }
        }
        else
        {
          // note that supported-open-block-vector, supported-block-vector,
          // attrib-extension-count and attrib-extension-names fields
          // were added only in rev 08, while "supported2" was defined in rev 07
          FSupport->OpenBlockVector = SupportedStruct.GetSmallCardinal();
          FSupport->BlockVector = SupportedStruct.GetSmallCardinal();
          unsigned int ExtensionCount;
          ExtensionCount = SupportedStruct.GetCardinal();
          for (unsigned int i = 0; i < ExtensionCount; i++)
          {
            FSupport->AttribExtensions->Add(SupportedStruct.GetAnsiString());
          }
          ExtensionCount = SupportedStruct.GetCardinal();
          for (unsigned int i = 0; i < ExtensionCount; i++)
          {
            UnicodeString Extension = SupportedStruct.GetAnsiString();
            SupportedExtensions->Add(Extension);
            ExtensionsLog->Add(Extension);
          }
        }

        if (FTerminal->Log->Logging)
        {
          FTerminal->LogEvent(FORMAT(
            L"Server support information (%s):\n"
             "  Attribute mask: %x, Attribute bits: %x, Open flags: %x\n"
             "  Access mask: %x, Open block vector: %x, Block vector: %x, Max read size: %d\n",
            (ExtensionName,
             int(FSupport->AttributeMask),
             int(FSupport->AttributeBits),
             int(FSupport->OpenFlags),
             int(FSupport->AccessMask),
             int(FSupport->OpenBlockVector),
             int(FSupport->BlockVector),
             int(FSupport->MaxReadSize))));
          FTerminal->LogEvent(FORMAT(L"  Attribute extensions (%d)\n", (FSupport->AttribExtensions->Count)));
          for (int Index = 0; Index < FSupport->AttribExtensions->Count; Index++)
          {
            FTerminal->LogEvent(
              FORMAT(L"    %s", (FSupport->AttribExtensions->Strings[Index])));
          }
          FTerminal->LogEvent(FORMAT(L"  Extensions (%d)\n", (ExtensionsLog->Count)));
          for (int Index = 0; Index < ExtensionsLog->Count; Index++)
          {
            FTerminal->LogEvent(
              FORMAT(L"    %s", (ExtensionsLog->Strings[Index])));
          }
        }
      }
      else if (ExtensionName == SFTP_EXT_VENDOR_ID)
      {
        TSFTPPacket VendorIdStruct(ExtensionData);
        UnicodeString VendorName(VendorIdStruct.GetAnsiString());
        UnicodeString ProductName(VendorIdStruct.GetAnsiString());
        UnicodeString ProductVersion(VendorIdStruct.GetAnsiString());
        __int64 ProductBuildNumber = VendorIdStruct.GetInt64();
        FTerminal->LogEvent(FORMAT(L"Server software: %s %s (%d) by %s",
          (ProductName, ProductVersion, int(ProductBuildNumber), VendorName)));
      }
      else if (ExtensionName == SFTP_EXT_FSROOTS)
      {
        FTerminal->LogEvent(L"File system roots:\n");
        DebugAssert(FFixedPaths == NULL);
        FFixedPaths = new TStringList();
        try
        {
          TSFTPPacket RootsPacket(ExtensionData);
          while (RootsPacket.GetNextData() != NULL)
          {
            unsigned long Dummy = RootsPacket.GetCardinal();
            if (Dummy != 1)
            {
              break;
            }
            else
            {
              unsigned char Drive = RootsPacket.GetByte();
              unsigned char MaybeType = RootsPacket.GetByte();
              FTerminal->LogEvent(FORMAT(L"  %s: (type %d)", (static_cast<char>(Drive), (int)MaybeType)));
              FFixedPaths->Add(FORMAT(L"%s:", (static_cast<char>(Drive))));
            }
          }
        }
        catch(Exception & E)
        {
          FFixedPaths->Clear();
          FTerminal->LogEvent(FORMAT(L"Failed to decode %s extension",
            (ExtensionName)));
          FTerminal->HandleException(&E);
        }
      }
      else if (ExtensionName == SFTP_EXT_VERSIONS)
      {
        // first try legacy decoding according to incorrect encoding
        // (structure-like) as of VShell (bug no longer present as of 4.0.3).
        TSFTPPacket VersionsPacket(ExtensionData);
        unsigned int StringSize;
        if (VersionsPacket.CanGetString(StringSize) &&
            (StringSize == VersionsPacket.RemainingLength))
        {
          UnicodeString Versions = VersionsPacket.GetAnsiString();
          FTerminal->LogEvent(FORMAT(L"SFTP versions supported by the server (VShell format): %s",
            (Versions)));
        }
        else
        {
          // if that fails, fallback to proper decoding
          FTerminal->LogEvent(FORMAT(L"SFTP versions supported by the server: %s",
            (AnsiToString(ExtensionData))));
        }
      }
      else if (ExtensionName == SFTP_EXT_STATVFS)
      {
        UnicodeString StatVfsVersion = AnsiToString(ExtensionData);
        if (StatVfsVersion == SFTP_EXT_STATVFS_VALUE_V2)
        {
          FSupportsStatVfsV2 = true;
          FTerminal->LogEvent(FORMAT(L"Supports %s extension version %s", (ExtensionName, StatVfsVersion)));
        }
        else
        {
          FTerminal->LogEvent(FORMAT(L"Unsupported %s extension version %s", (ExtensionName, StatVfsVersion)));
        }
      }
      else if (ExtensionName == SFTP_EXT_HARDLINK)
      {
        UnicodeString HardlinkVersion = AnsiToString(ExtensionData);
        if (HardlinkVersion == SFTP_EXT_HARDLINK_VALUE_V1)
        {
          FSupportsHardlink = true;
          FTerminal->LogEvent(FORMAT(L"Supports %s extension version %s", (ExtensionName, HardlinkVersion)));
        }
        else
        {
          FTerminal->LogEvent(FORMAT(L"Unsupported %s extension version %s", (ExtensionName, HardlinkVersion)));
        }
      }
      else if (ExtensionName == SFTP_EXT_LIMITS)
      {
        UnicodeString LimitsVersion = AnsiToString(ExtensionData);
        if (LimitsVersion == SFTP_EXT_LIMITS_VALUE_V1)
        {
          SupportsLimits = true;
          FTerminal->LogEvent(FORMAT(L"Supports %s extension version %s", (ExtensionName, LimitsVersion)));
        }
        else
        {
          FTerminal->LogEvent(FORMAT(L"Unsupported %s extension version %s", (ExtensionName, LimitsVersion)));
        }
      }
      else if ((ExtensionName == SFTP_EXT_COPY_FILE) ||
               (ExtensionName == SFTP_EXT_COPY_DATA) ||
               (ExtensionName == SFTP_EXT_SPACE_AVAILABLE) ||
               (ExtensionName == SFTP_EXT_CHECK_FILE) ||
               (ExtensionName == SFTP_EXT_POSIX_RENAME))
      {
        FTerminal->LogEvent(FORMAT(L"Supports extension %s=%s", (ExtensionName, ExtensionDisplayData)));
      }
      else
      {
        FTerminal->LogEvent(0, FORMAT(L"Unknown server extension %s=%s", (ExtensionName, ExtensionDisplayData)));
      }

      UnicodeString Line = ExtensionName;
      if (!ExtensionDisplayData.IsEmpty())
      {
        Line += FORMAT(L"=%s", (ExtensionDisplayData));
      }
      FExtensions += FORMAT(L"  %s\r\n", (Line));

      SupportedExtensions->Add(ExtensionName);
    }
  }

  if (FVersion < 4)
  {
    // currently enable the bug for all servers (really known on OpenSSH)
    FSignedTS = (FTerminal->SessionData->SFTPBug[sbSignedTS] == asOn) ||
      (FTerminal->SessionData->SFTPBug[sbSignedTS] == asAuto);
    if (FSignedTS)
    {
      FTerminal->LogEvent(L"We believe the server has signed timestamps bug");
    }
  }
  else
  {
    FSignedTS = false;
  }

  switch (FTerminal->SessionData->NotUtf)
  {
    case asOff:
      FUtfStrings = asOn;
      FTerminal->LogEvent(L"We will use UTF-8 strings as configured");
      break;

    default:
      DebugFail();
    case asAuto:
      // Nb, Foxit server does not exist anymore
      if (GetSessionInfo().SshImplementation.Pos(L"Foxit-WAC-Server") == 1)
      {
        FUtfStrings = asOff;
        FTerminal->LogEvent(L"We will not use UTF-8 strings as the server is known not to use them");
      }
      else
      {
        if (FVersion >= 4)
        {
          FTerminal->LogEvent(L"We will use UTF-8 strings as it is mandatory with SFTP version 4 and newer");
          FUtfStrings = asOn;
        }
        else
        {
          FTerminal->LogEvent(L"We will use UTF-8 strings until server sends an invalid UTF-8 string as with SFTP version 3 and older UTF-8 strings are not mandatory");
          FUtfStrings = asAuto;
          FUtfDisablingAnnounced = false;
        }
      }
      break;

    case asOn:
      FTerminal->LogEvent(L"We will not use UTF-8 strings as configured");
      FUtfStrings = asOff;
      break;
  }

  FMaxPacketSize = FTerminal->SessionData->SFTPMaxPacketSize;
  if (FMaxPacketSize == 0)
  {
    unsigned int PacketPayload = 4;
    if (SupportsLimits)
    {
      TSFTPPacket Packet(SSH_FXP_EXTENDED);
      Packet.AddString(SFTP_EXT_LIMITS);
      SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_EXTENDED_REPLY);
      unsigned int MaxPacketSize = static_cast<unsigned int>(std::min(static_cast<__int64>(std::numeric_limits<unsigned int>::max()), Packet.GetInt64()));
      FTerminal->LogEvent(FORMAT(L"Limiting packet size to server's limit of %d + %d bytes",
        (static_cast<int>(MaxPacketSize), static_cast<int>(PacketPayload))));
      FMaxPacketSize = MaxPacketSize + PacketPayload;
    }
    else if ((FSecureShell->SshImplementation == sshiOpenSSH) && (FVersion == 3) && !FSupport->Loaded)
    {
      FMaxPacketSize = PacketPayload + (256 * 1024); // len + 256kB payload
      FTerminal->LogEvent(FORMAT(L"Limiting packet size to OpenSSH sftp-server limit of %d bytes",
        (int(FMaxPacketSize))));
    }
    // full string is "1.77 sshlib: Momentum SSH Server",
    // possibly it is sshlib-related
    else if (GetSessionInfo().SshImplementation.Pos(L"Momentum SSH Server") != 0)
    {
      FMaxPacketSize = PacketPayload + (32 * 1024);
      FTerminal->LogEvent(FORMAT(L"Limiting packet size to Momentum sftp-server limit of %d bytes",
        (int(FMaxPacketSize))));
    }
  }

  FSupportedExtensions.reset(FTerminal->ProcessFeatures(SupportedExtensions.get()));

  if (SupportsExtension(SFTP_EXT_VENDOR_ID))
  {
    TSFTPPacket Packet(SSH_FXP_EXTENDED);
    Packet.AddString(SFTP_EXT_VENDOR_ID);
    Packet.AddString(FTerminal->Configuration->CompanyName);
    Packet.AddString(FTerminal->Configuration->ProductName);
    Packet.AddString(FTerminal->Configuration->ProductVersion);
    Packet.AddInt64(LOWORD(FTerminal->Configuration->FixedApplicationInfo->dwFileVersionLS));
    SendPacket(&Packet);
    // we are not interested in the response, do not wait for it
    ReserveResponse(&Packet, NULL);
  }

}
//---------------------------------------------------------------------------
const char * __fastcall TSFTPFileSystem::GetEOL() const
{
  if (FVersion >= 4)
  {
    DebugAssert(!FEOL.IsEmpty());
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
  DebugAssert(SupportsExtension(SFTP_EXT_OWNER_GROUP));

  TSFTPPacket PacketOwners(SSH_FXP_EXTENDED);
  TSFTPPacket PacketGroups(SSH_FXP_EXTENDED);

  TSFTPPacket * Packets[] = { &PacketOwners, &PacketGroups };
  TRemoteTokenList * Lists[] = { &FTerminal->FUsers, &FTerminal->FGroups };
  unsigned char ListTypes[] = { OGQ_LIST_OWNERS, OGQ_LIST_GROUPS };

  for (size_t Index = 0; Index < LENOF(Packets); Index++)
  {
    TSFTPPacket * Packet = Packets[Index];
    Packet->AddString(SFTP_EXT_OWNER_GROUP);
    Packet->AddByte(ListTypes[Index]);
    SendPacket(Packet);
    ReserveResponse(Packet, Packet);
  }

  for (size_t Index = 0; Index < LENOF(Packets); Index++)
  {
    TSFTPPacket * Packet = Packets[Index];

    ReceiveResponse(Packet, Packet, SSH_FXP_EXTENDED_REPLY, asOpUnsupported);

    if ((Packet->Type != SSH_FXP_EXTENDED_REPLY) ||
        (Packet->GetAnsiString() != SFTP_EXT_OWNER_GROUP_REPLY))
    {
      FTerminal->LogEvent(FORMAT(L"Invalid response to %s", (SFTP_EXT_OWNER_GROUP)));
    }
    else
    {
      TRemoteTokenList & List = *Lists[Index];
      unsigned long Count = Packet->GetCardinal();

      List.Clear();
      for (unsigned long Item = 0; Item < Count; Item++)
      {
        TRemoteToken Token(Packet->GetString(FUtfStrings));
        List.Add(Token);
        if (&List == &FTerminal->FGroups)
        {
          FTerminal->FMembership.Add(Token);
        }
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
    FDirectoryToChangeTo = L"";
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
void __fastcall TSFTPFileSystem::TryOpenDirectory(const UnicodeString Directory)
{
  FTerminal->LogEvent(FORMAT(L"Trying to open directory \"%s\".", (Directory)));
  TRemoteFile * File;
  CustomReadFile(Directory, File, SSH_FXP_LSTAT, NULL, asOpUnsupported);
  if (File == NULL)
  {
    // File can be NULL only when server does not support SSH_FXP_LSTAT.
    // Fallback to legacy solution, which in turn does not allow entering
    // traverse-only (chmod 110) directories.
    // This is workaround for https://www.ftpshell.com/
    TSFTPPacket Packet(SSH_FXP_OPENDIR);
    AddPathString(Packet, UnixExcludeTrailingBackslash(Directory));
    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_HANDLE);
    RawByteString Handle = Packet.GetFileHandle();
    Packet.ChangeType(SSH_FXP_CLOSE);
    Packet.AddString(Handle);
    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS, asAll);
  }
  else
  {
    delete File;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::AnnounceFileListOperation()
{
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ChangeDirectory(const UnicodeString Directory)
{
  UnicodeString Path, Current;

  Current = !FDirectoryToChangeTo.IsEmpty() ? FDirectoryToChangeTo : FCurrentDirectory;
  Path = RealPath(Directory, Current);

  // to verify existence of directory try to open it (SSH_FXP_REALPATH succeeds
  // for invalid paths on some systems, like CygWin)
  TryOpenDirectory(Path);

  // if open dir did not fail, directory exists -> success.
  FDirectoryToChangeTo = Path;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CachedChangeDirectory(const UnicodeString Directory)
{
  FDirectoryToChangeTo = UnixExcludeTrailingBackslash(Directory);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ReadDirectory(TRemoteFileList * FileList)
{
  DebugAssert(FileList && !FileList->Directory.IsEmpty());

  UnicodeString Directory;
  Directory = UnixExcludeTrailingBackslash(LocalCanonify(FileList->Directory));
  FTerminal->LogEvent(FORMAT(L"Listing directory \"%s\".", (Directory)));

  // moved before SSH_FXP_OPENDIR, so directory listing does not retain
  // old data (e.g. parent directory) when reading fails
  FileList->Reset();

  TSFTPPacket Packet(SSH_FXP_OPENDIR);
  RawByteString Handle;

  try
  {
    AddPathString(Packet, Directory);

    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_HANDLE);

    Handle = Packet.GetFileHandle();
  }
  catch(...)
  {
    if (FTerminal->Active)
    {
      FileList->AddFile(new TRemoteParentDirectory(FTerminal));
    }
    throw;
  }

  TSFTPPacket Response;
  try
  {
    bool isEOF = false;
    int Total = 0;
    bool HasParentDirectory = false;
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

        int ResolvedLinks = 0;
        for (unsigned long Index = 0; !isEOF && (Index < Count); Index++)
        {
          std::unique_ptr<TRemoteFile> AFile(LoadFile(&ListingPacket, NULL, L"", FileList));
          TRemoteFile * File = AFile.get();
          if (FTerminal->IsValidFile(File))
          {
            FileList->AddFile(AFile.release());
            if (FTerminal->IsEncryptingFiles() && // optimization
                IsRealFile(File->FileName))
            {
              UnicodeString FullFileName = UnixExcludeTrailingBackslash(File->FullFileName);
              UnicodeString FileName = UnixExtractFileName(FTerminal->DecryptFileName(FullFileName, false, false));
              if (File->FileName != FileName)
              {
                File->SetEncrypted();
              }
              File->FileName = FileName;
            }
            if (FTerminal->Configuration->ActualLogProtocol >= 1)
            {
              FTerminal->LogEvent(FORMAT(L"Read file '%s' from listing", (File->FileName)));
            }
            if (File->LinkedFile != NULL)
            {
              ResolvedLinks++;
            }
            if (File->IsParentDirectory)
            {
              HasParentDirectory = true;
            }
            Total++;

            if (Total % 10 == 0)
            {
              FTerminal->DoReadDirectoryProgress(Total, ResolvedLinks, isEOF);
              if (isEOF)
              {
                FTerminal->LogEvent(L"Listing directory cancelled.");
                FTerminal->DoReadDirectoryProgress(-2, 0, isEOF);
              }
            }
          }
        }

        if (!isEOF &&
            (FVersion >= 6) &&
            // As of 7.0.9 the Cerberus SFTP server always sets the end-of-list to true.
            // Fixed in 7.0.10.
            (FSecureShell->SshImplementation != sshiCerberus) &&
            ListingPacket.CanGetBool())
        {
          isEOF = ListingPacket.GetBool();
        }

        if (Count == 0)
        {
          FTerminal->LogEvent(L"Empty directory listing packet. Aborting directory reading.");
          isEOF = true;
        }
      }
      else if (Response.Type == SSH_FXP_STATUS)
      {
        isEOF = (GotStatusPacket(&Response, asEOF, false) == SSH_FX_EOF);
      }
      else
      {
        FTerminal->FatalError(NULL, FMTLOAD(SFTP_INVALID_TYPE, ((int)Response.Type)));
      }
    }
    while (!isEOF);

    if (Total == 0)
    {
      bool Failure = false;
      // no point reading parent of root directory,
      // moreover CompleteFTP terminates session upon attempt to do so
      if (IsUnixRootPath(FileList->Directory))
      {
        File = NULL;
      }
      else
      {
        // Empty file list -> probably "permission denied", we
        // at least get link to parent directory ("..")
        try
        {
          FTerminal->ExceptionOnFail = true;
          try
          {
            File = FTerminal->ReadFile(UnixCombinePaths(FileList->Directory, PARENTDIRECTORY));
          }
          __finally
          {
            FTerminal->ExceptionOnFail = false;
          }
        }
        catch(Exception &E)
        {
          if (E.InheritsFrom(__classid(EFatal)))
          {
            throw;
          }
          else
          {
            File = NULL;
            Failure = true;
          }
        }
      }

      // on some systems even getting ".." fails, we create dummy ".." instead
      if (File == NULL)
      {
        File = new TRemoteParentDirectory(FTerminal);
      }

      DebugAssert(File && File->IsParentDirectory);
      FileList->AddFile(File);

      if (Failure)
      {
        throw ExtException(
          NULL, FMTLOAD(EMPTY_DIRECTORY, (FileList->Directory)),
          HELP_EMPTY_DIRECTORY);
      }
    }
    else
    {
      if (!HasParentDirectory)
      {
        FileList->AddFile(new TRemoteParentDirectory(FTerminal));
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
  DebugAssert(SymlinkFile != NULL);
  DebugAssert(FVersion >= 3); // symlinks are supported with SFTP version 3 and later

  // need to use full filename when resolving links within subdirectory
  // (i.e. for download)
  UnicodeString FileName = LocalCanonify(
    SymlinkFile->HaveFullFileName ? SymlinkFile->FullFileName : SymlinkFile->FileName);

  TSFTPPacket ReadLinkPacket(SSH_FXP_READLINK);
  AddPathString(ReadLinkPacket, FileName);
  SendPacket(&ReadLinkPacket);
  ReserveResponse(&ReadLinkPacket, &ReadLinkPacket);

  // send second request before reading response to first one
  // (performance benefit)
  TSFTPPacket AttrsPacket(SSH_FXP_STAT);
  AddPathString(AttrsPacket, FileName);
  if (FVersion >= 4)
  {
    AttrsPacket.AddCardinal(SSH_FILEXFER_ATTR_COMMON);
  }
  SendPacket(&AttrsPacket);
  ReserveResponse(&AttrsPacket, &AttrsPacket);

  ReceiveResponse(&ReadLinkPacket, &ReadLinkPacket, SSH_FXP_NAME);
  if (ReadLinkPacket.GetCardinal() != 1)
  {
    FTerminal->FatalError(NULL, LoadStr(SFTP_NON_ONE_FXP_NAME_PACKET));
  }
  // Not sure about the DontCache parameter here. Actually we should not get here for encrypted sessions.
  DebugAssert(!FTerminal->IsEncryptingFiles());
  SymlinkFile->LinkTo = FTerminal->DecryptFileName(ReadLinkPacket.GetPathString(FUtfStrings), true, true);
  FTerminal->LogEvent(FORMAT(L"Link resolved to \"%s\".", (SymlinkFile->LinkTo)));

  ReceiveResponse(&AttrsPacket, &AttrsPacket, SSH_FXP_ATTRS);
  // SymlinkFile->FileName was used instead SymlinkFile->LinkTo before, why?
  File = LoadFile(&AttrsPacket, SymlinkFile,
    UnixExtractFileName(SymlinkFile->LinkTo));
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ReadFile(const UnicodeString FileName,
  TRemoteFile *& File)
{
  CustomReadFile(FileName, File, SSH_FXP_LSTAT);
}
//---------------------------------------------------------------------------
bool __fastcall TSFTPFileSystem::RemoteFileExists(const UnicodeString FullPath,
  TRemoteFile ** File)
{
  bool Result;
  try
  {
    TRemoteFile * AFile;
    CustomReadFile(FullPath, AFile, SSH_FXP_LSTAT, NULL, asNoSuchFile);
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
void __fastcall TSFTPFileSystem::SendCustomReadFile(TSFTPPacket * Packet,
  TSFTPPacket * Response, unsigned long Flags)
{
  if (FVersion >= 4)
  {
    Packet->AddCardinal(Flags);
  }
  SendPacket(Packet);
  ReserveResponse(Packet, Response);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CustomReadFile(const UnicodeString FileName,
  TRemoteFile *& File, unsigned char Type, TRemoteFile * ALinkedByFile,
  int AllowStatus)
{
  unsigned long Flags = SSH_FILEXFER_ATTR_SIZE | SSH_FILEXFER_ATTR_PERMISSIONS |
    SSH_FILEXFER_ATTR_ACCESSTIME | SSH_FILEXFER_ATTR_MODIFYTIME |
    SSH_FILEXFER_ATTR_OWNERGROUP;
  TSFTPPacket Packet(Type);
  UnicodeString FullName = LocalCanonify(FileName);
  AddPathString(Packet, FullName);
  SendCustomReadFile(&Packet, &Packet, Flags);
  ReceiveResponse(&Packet, &Packet, SSH_FXP_ATTRS, AllowStatus);

  if (Packet.Type == SSH_FXP_ATTRS)
  {
    File = LoadFile(&Packet, ALinkedByFile, UnixExtractFileName(FileName));
    if (FTerminal->IsFileEncrypted(FullName))
    {
      File->SetEncrypted();
    }
  }
  else
  {
    DebugAssert(AllowStatus > 0);
    File = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::DoDeleteFile(const UnicodeString FileName, unsigned char Type)
{
  TSFTPPacket Packet(Type);
  UnicodeString RealFileName = LocalCanonify(FileName);
  AddPathString(Packet, RealFileName);
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::DeleteFile(const UnicodeString FileName,
  const TRemoteFile * File, int Params, TRmSessionAction & Action)
{
  unsigned char Type;
  if (FTerminal->DeleteContentsIfDirectory(FileName, File, Params, Action))
  {
    Type = SSH_FXP_RMDIR;
  }
  else
  {
    Type = SSH_FXP_REMOVE;
  }

  DoDeleteFile(FileName, Type);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::RenameFile(
  const UnicodeString & FileName, const TRemoteFile *, const UnicodeString & NewName, bool DebugUsedArg(Overwrite))
{
  bool UsePosixRename = FTerminal->SessionData->UsePosixRename;
  TSFTPPacket Packet(UsePosixRename ? SSH_FXP_EXTENDED : SSH_FXP_RENAME);
  if (UsePosixRename)
  {
    Packet.AddString(SFTP_EXT_POSIX_RENAME);
  }
  UnicodeString RealName = LocalCanonify(FileName);
  bool Encrypted = FTerminal->IsFileEncrypted(RealName);
  AddPathString(Packet, RealName);
  UnicodeString TargetName;
  if (UnixExtractFilePath(NewName).IsEmpty())
  {
    // rename case (TTerminal::RenameFile)
    TargetName = UnixExtractFilePath(RealName) + NewName;
  }
  else
  {
    TargetName = LocalCanonify(NewName);
  }
  AddPathString(Packet, TargetName, Encrypted);
  if (!UsePosixRename && (FVersion >= 5))
  {
    // Use SSH_FXP_RENAME + SSH_FXF_RENAME_ATOMIC when UsePosixRename?
    Packet.AddCardinal(0);
  }
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
}
//---------------------------------------------------------------------------
void TSFTPFileSystem::DoCloseRemoteIfOpened(const RawByteString & Handle)
{
  if (!Handle.IsEmpty())
  {
    TSFTPPacket Packet(SSH_FXP_CLOSE);
    Packet.AddString(Handle);
    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CopyFile(
  const UnicodeString & FileName, const TRemoteFile * File, const UnicodeString & NewName, bool DebugUsedArg(Overwrite))
{
  UnicodeString FileNameCanonical = Canonify(FileName);
  bool Encrypted = FTerminal->IsFileEncrypted(FileNameCanonical);
  UnicodeString NewNameCanonical = Canonify(NewName);

  if (SupportsExtension(SFTP_EXT_COPY_FILE) || (FSecureShell->SshImplementation == sshiBitvise))
  {
    TSFTPPacket Packet(SSH_FXP_EXTENDED);
    Packet.AddString(SFTP_EXT_COPY_FILE);
    AddPathString(Packet, FileNameCanonical);
    AddPathString(Packet, NewNameCanonical, Encrypted);
    Packet.AddBool(false);
    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
  }
  else
  {
    DebugAssert(SupportsExtension(SFTP_EXT_COPY_DATA));

    __int64 Size = DebugAlwaysTrue(File != NULL) ? File->Size : -1;
    RawByteString SourceRemoteHandle, DestRemoteHandle;
    try
    {
      SourceRemoteHandle = SFTPOpenRemoteFile(FileNameCanonical, SSH_FXF_READ, Encrypted, Size);
      // SFTP_EXT_COPY_FILE does not allow overwriting existing files
      // (the specification does not mandate it, but it is implemented like that both in ProFTPD and Bitvise).
      // So using SSH_FXF_EXCL for consistency.
      DestRemoteHandle = SFTPOpenRemoteFile(NewNameCanonical, SSH_FXF_WRITE | SSH_FXF_CREAT | SSH_FXF_EXCL, Encrypted, Size);

      TSFTPPacket Packet(SSH_FXP_EXTENDED);
      Packet.AddString(SFTP_EXT_COPY_DATA);
      Packet.AddString(SourceRemoteHandle);
      Packet.AddInt64(0);
      Packet.AddInt64(0); // until EOF
      Packet.AddString(DestRemoteHandle);
      Packet.AddInt64(0);
      SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);

      if (DebugAlwaysTrue(File != NULL))
      {
        TSFTPPacket PropertiesRequest(SSH_FXP_SETSTAT);
        AddPathString(PropertiesRequest, NewNameCanonical);

        unsigned short Rights = File->Rights->NumberSet;
        TDSTMode DSTMode = FTerminal->SessionData->DSTMode;
        __int64 MTime = ConvertTimestampToUnix(DateTimeToFileTime(File->Modification, DSTMode), DSTMode);
        PropertiesRequest.AddProperties(&Rights, NULL, NULL, &MTime, NULL, NULL, false, FVersion, FUtfStrings);
        SendPacketAndReceiveResponse(&PropertiesRequest, &Packet, SSH_FXP_STATUS);
      }
    }
    __finally
    {
      if (FTerminal->Active)
      {
        DoCloseRemoteIfOpened(SourceRemoteHandle);
        DoCloseRemoteIfOpened(DestRemoteHandle);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CreateDirectory(const UnicodeString & DirName, bool Encrypt)
{
  TSFTPPacket Packet(SSH_FXP_MKDIR);
  UnicodeString CanonifiedName = Canonify(DirName);
  AddPathString(Packet, CanonifiedName, Encrypt);
  Packet.AddProperties(NULL, 0, true, FVersion, FUtfStrings, NULL);
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CreateLink(const UnicodeString FileName,
  const UnicodeString PointTo, bool Symbolic)
{
  // Cerberus server does not even respond to LINK or SYMLINK,
  // Although its log says:
  // Unrecognized SFTP client command: (20)
  // Unknown SFTP packet - Sending Unsupported OP response

  DebugAssert(FVersion >= 3); // links are supported with SFTP version 3 and later
  bool UseLink = (FVersion >= 6);
  bool UseHardlink = !Symbolic && !UseLink && FSupportsHardlink;
  TSFTPPacket Packet(UseHardlink ? SSH_FXP_EXTENDED : (UseLink ? SSH_FXP_LINK : SSH_FXP_SYMLINK));
  if (UseHardlink)
  {
    Packet.AddString(SFTP_EXT_HARDLINK);
  }

  bool Buggy;
  // OpenSSH hardlink extension always uses the "wrong" order
  // as it's defined as such to mimic OpenSSH symlink bug
  if (UseHardlink)
  {
    Buggy = true; //sic
  }
  else
  {
    if (FTerminal->SessionData->SFTPBug[sbSymlink] == asOn)
    {
      Buggy = true;
      FTerminal->LogEvent(L"Forcing workaround for SFTP link bug");
    }
    else if (FTerminal->SessionData->SFTPBug[sbSymlink] == asOff)
    {
      Buggy = false;
    }
    else
    {
      if (UseLink)
      {
        if (FSecureShell->SshImplementation == sshiProFTPD)
        {
          // ProFTPD/mod_sftp followed OpenSSH symlink bug even for link implementation.
          // This will be fixed with the next release with
          // SSH version string bumped to "mod_sftp/1.0.0"
          // http://bugs.proftpd.org/show_bug.cgi?id=4080
          UnicodeString ProFTPDVerStr = GetSessionInfo().SshImplementation;
          CutToChar(ProFTPDVerStr, L'/', false);
          int ProFTPDMajorVer = StrToIntDef(CutToChar(ProFTPDVerStr, L'.', false), 0);
          Buggy = (ProFTPDMajorVer == 0);
          if (Buggy)
          {
            FTerminal->LogEvent(L"We believe the server has SFTP link bug");
          }
        }
        else
        {
          Buggy = false;
        }
      }
      else
      {
        // ProFTPD/mod_sftp deliberately follows OpenSSH bug.
        // Though we should get here with ProFTPD only when user forced
        // SFTP version < 6 or when connecting to an ancient version of ProFTPD.
        Buggy =
          (FSecureShell->SshImplementation == sshiOpenSSH) ||
          (FSecureShell->SshImplementation == sshiProFTPD);
        if (Buggy)
        {
          FTerminal->LogEvent(L"We believe the server has SFTP symlink bug");
        }
      }
    }
  }

  UnicodeString FinalPointTo = PointTo;
  UnicodeString FinalFileName = Canonify(FileName);

  if (!Symbolic)
  {
    FinalPointTo = Canonify(PointTo);
  }

  // creating symlinks is not allowed when encryption is enabled, so we are not considering encryption here
  if (!Buggy)
  {
    AddPathString(Packet, FinalFileName);
    AddPathString(Packet, FinalPointTo);
  }
  else
  {
    AddPathString(Packet, FinalPointTo);
    AddPathString(Packet, FinalFileName);
  }

  if (UseLink)
  {
    Packet.AddBool(Symbolic);
  }
  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ChangeFileProperties(const UnicodeString FileName,
  const TRemoteFile * /*File*/, const TRemoteProperties * AProperties,
  TChmodSessionAction & Action)
{
  DebugAssert(AProperties != NULL);

  TRemoteFile * File;

  UnicodeString RealFileName = LocalCanonify(FileName);
  ReadFile(RealFileName, File);

  try
  {
    DebugAssert(File);

    if (File->IsDirectory && FTerminal->CanRecurseToDirectory(File) && AProperties->Recursive)
    {
      try
      {
        FTerminal->ProcessDirectory(FileName, FTerminal->ChangeFileProperties,
          (void*)AProperties);
      }
      catch(...)
      {
        Action.Cancel();
        throw;
      }
    }

    // SFTP can change owner and group at the same time only, not individually.
    // Fortunately we know current owner/group, so if only one is present,
    // we can supplement the other.
    TRemoteProperties Properties(*AProperties);
    if (Properties.Valid.Contains(vpGroup) &&
        !Properties.Valid.Contains(vpOwner))
    {
      Properties.Owner = File->Owner;
      Properties.Valid << vpOwner;
    }
    else if (Properties.Valid.Contains(vpOwner) &&
             !Properties.Valid.Contains(vpGroup))
    {
      Properties.Group = File->Group;
      Properties.Valid << vpGroup;
    }

    TSFTPPacket Packet(SSH_FXP_SETSTAT);
    AddPathString(Packet, RealFileName);
    Packet.AddProperties(&Properties, *File->Rights, File->IsDirectory, FVersion, FUtfStrings, &Action);
    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_STATUS);
  }
  __finally
  {
    delete File;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TSFTPFileSystem::LoadFilesProperties(TStrings * FileList)
{
  bool Result = false;
  // without knowledge of server's capabilities, this all make no sense
  if (FSupport->Loaded || (FSecureShell->SshImplementation == sshiBitvise))
  {
    TFileOperationProgressType Progress(&FTerminal->DoProgress, &FTerminal->DoFinished);
    FTerminal->OperationStart(Progress, foGetProperties, osRemote, FileList->Count);

    static int LoadFilesPropertiesQueueLen = 5;
    TSFTPLoadFilesPropertiesQueue Queue(this);
    try
    {
      if (Queue.Init(LoadFilesPropertiesQueueLen, FileList))
      {
        TRemoteFile * File;
        TSFTPPacket Packet;
        bool Next;
        do
        {
          Next = Queue.ReceivePacket(&Packet, File);
          DebugAssert((Packet.Type == SSH_FXP_ATTRS) || (Packet.Type == SSH_FXP_STATUS));
          if (Packet.Type == SSH_FXP_ATTRS)
          {
            DebugAssert(File != NULL);
            Progress.SetFile(File->FileName);
            LoadFile(File, &Packet);
            Result = true;
            TOnceDoneOperation OnceDoneOperation;
            Progress.Finish(File->FileName, true, OnceDoneOperation);
          }

          if (Progress.Cancel != csContinue)
          {
            Next = false;
          }
        }
        while (Next);
      }
    }
    __finally
    {
      Queue.DisposeSafe();
      FTerminal->OperationStop(Progress);
    }
    // queue is discarded here
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CalculateFilesChecksum(
  const UnicodeString & Alg, TStrings * FileList, TCalculatedChecksumEvent OnCalculatedChecksum,
  TFileOperationProgressType * OperationProgress, bool FirstLevel)
{
  FTerminal->CalculateSubFoldersChecksum(Alg, FileList, OnCalculatedChecksum, OperationProgress, FirstLevel);

  static int CalculateFilesChecksumQueueLen = 5;
  TSFTPCalculateFilesChecksumQueue Queue(this);
  TOnceDoneOperation OnceDoneOperation; // not used
  try
  {
    UnicodeString SftpAlg;
    int Index = FChecksumAlgs->IndexOf(Alg);
    if (Index >= 0)
    {
      SftpAlg = FChecksumSftpAlgs->Strings[Index];
    }
    else
    {
      // try user-specified alg
      SftpAlg = Alg;
    }

    if (Queue.Init(CalculateFilesChecksumQueueLen, SftpAlg, FileList))
    {
      TSFTPPacket Packet;
      bool Next;
      do
      {
        bool Success = false;
        TRemoteFile * File = NULL;

        try
        {
          TChecksumSessionAction Action(FTerminal->ActionLog);
          try
          {
            Next = Queue.ReceivePacket(&Packet, File);
            DebugAssert(Packet.Type == SSH_FXP_EXTENDED_REPLY);

            OperationProgress->SetFile(File->FileName);
            Action.FileName(File->FullFileName);

            // skip alg
            Packet.GetAnsiString();
            UnicodeString Checksum = BytesToHex(reinterpret_cast<const unsigned char*>(Packet.GetNextData(Packet.RemainingLength)), Packet.RemainingLength, false);
            if (OnCalculatedChecksum != NULL)
            {
              OnCalculatedChecksum(File->FileName, Alg, Checksum);
            }
            Action.Checksum(Alg, Checksum);

            Success = true;
          }
          catch (Exception & E)
          {
            FTerminal->RollbackAction(Action, OperationProgress, &E);

            // Error formatting expanded from inline to avoid strange exceptions
            UnicodeString Error =
              FMTLOAD(CHECKSUM_ERROR,
                (File != NULL ? File->FullFileName : UnicodeString(L"")));
            FTerminal->CommandError(&E, Error);
            // TODO: retries? resume?
            Next = false;
          }
        }
        __finally
        {
          if (FirstLevel)
          {
            OperationProgress->Finish(File->FileName, Success, OnceDoneOperation);
          }
        }

        if (OperationProgress->Cancel != csContinue)
        {
          Next = false;
        }
      }
      while (Next);
    }
  }
  __finally
  {
    Queue.DisposeSafe();
  }
  // queue is discarded here
}
//---------------------------------------------------------------------------
UnicodeString TSFTPFileSystem::CalculateFilesChecksumInitialize(const UnicodeString & Alg)
{
  return FindIdent(Alg, FChecksumAlgs.get());
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CustomCommandOnFile(const UnicodeString /*FileName*/,
    const TRemoteFile * /*File*/, UnicodeString /*Command*/, int /*Params*/,
    TCaptureOutputEvent /*OutputEvent*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::AnyCommand(const UnicodeString /*Command*/,
  TCaptureOutputEvent /*OutputEvent*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
TStrings * __fastcall TSFTPFileSystem::GetFixedPaths()
{
  return FFixedPaths;
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SpaceAvailable(const UnicodeString Path,
  TSpaceAvailable & ASpaceAvailable)
{
  if (SupportsExtension(SFTP_EXT_SPACE_AVAILABLE) ||
      // See comment in IsCapable
      (FSecureShell->SshImplementation == sshiBitvise))
  {
    TSFTPPacket Packet(SSH_FXP_EXTENDED);
    Packet.AddString(SFTP_EXT_SPACE_AVAILABLE);
    AddPathString(Packet, LocalCanonify(Path));

    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_EXTENDED_REPLY);

    ASpaceAvailable.BytesOnDevice = Packet.GetInt64();
    ASpaceAvailable.UnusedBytesOnDevice = Packet.GetInt64();
    ASpaceAvailable.BytesAvailableToUser = Packet.GetInt64();
    ASpaceAvailable.UnusedBytesAvailableToUser = Packet.GetInt64();
    // bytes-per-allocation-unit was added later to the protocol
    // (revision 07, while the extension was defined already in rev 06),
    // be tolerant
    if (Packet.CanGetCardinal())
    {
      ASpaceAvailable.BytesPerAllocationUnit = Packet.GetCardinal();
    }
    else if (Packet.CanGetSmallCardinal())
    {
      // See http://bugs.proftpd.org/show_bug.cgi?id=4079
      FTerminal->LogEvent(L"Assuming ProFTPD/mod_sftp bug of 2-byte bytes-per-allocation-unit field");
      ASpaceAvailable.BytesPerAllocationUnit = Packet.GetSmallCardinal();
    }
    else
    {
      FTerminal->LogEvent(L"Missing bytes-per-allocation-unit field");
    }
  }
  else if (DebugAlwaysTrue(FSupportsStatVfsV2))
  {
    // https://github.com/openssh/openssh-portable/blob/master/PROTOCOL
    TSFTPPacket Packet(SSH_FXP_EXTENDED);
    Packet.AddString(SFTP_EXT_STATVFS);
    AddPathString(Packet, LocalCanonify(Path));

    SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_EXTENDED_REPLY);

    __int64 BlockSize = Packet.GetInt64(); // file system block size
    __int64 FundamentalBlockSize = Packet.GetInt64(); // fundamental fs block size
    __int64 Blocks = Packet.GetInt64(); // number of blocks (unit f_frsize)
    __int64 FreeBlocks = Packet.GetInt64(); // free blocks in file system
    __int64 AvailableBlocks = Packet.GetInt64(); // free blocks for non-root
    __int64 FileINodes = Packet.GetInt64(); // total file inodes
    __int64 FreeFileINodes = Packet.GetInt64(); // free file inodes
    __int64 AvailableFileINodes = Packet.GetInt64(); // free file inodes for to non-root
    __int64 SID = Packet.GetInt64(); // file system id
    __int64 Flags = Packet.GetInt64(); // bit mask of f_flag values
    __int64 NameMax = Packet.GetInt64(); // maximum filename length

    FTerminal->LogEvent(FORMAT(L"Block size: %s", (IntToStr(BlockSize))));
    FTerminal->LogEvent(FORMAT(L"Fundamental block size: %s", (IntToStr(FundamentalBlockSize))));
    FTerminal->LogEvent(FORMAT(L"Total blocks: %s", (IntToStr(Blocks))));
    FTerminal->LogEvent(FORMAT(L"Free blocks: %s", (IntToStr(FreeBlocks))));
    FTerminal->LogEvent(FORMAT(L"Free blocks for non-root: %s", (IntToStr(AvailableBlocks))));
    FTerminal->LogEvent(FORMAT(L"Total file inodes: %s", (IntToStr(FileINodes))));
    FTerminal->LogEvent(FORMAT(L"Free file inodes: %s", (IntToStr(FreeFileINodes))));
    FTerminal->LogEvent(FORMAT(L"Free file inodes for non-root: %s", (IntToStr(AvailableFileINodes))));
    FTerminal->LogEvent(FORMAT(L"File system ID: %s", (BytesToHex(reinterpret_cast<const unsigned char *>(&SID), sizeof(SID)))));
    UnicodeString FlagStr;
    if (FLAGSET(Flags, SFTP_EXT_STATVFS_ST_RDONLY))
    {
      AddToList(FlagStr, L"read-only", L",");
      Flags -= SFTP_EXT_STATVFS_ST_RDONLY;
    }
    if (FLAGSET(Flags, SFTP_EXT_STATVFS_ST_NOSUID))
    {
      AddToList(FlagStr, L"no-setuid", L",");
      Flags -= SFTP_EXT_STATVFS_ST_NOSUID;
    }
    if (Flags != 0)
    {
      AddToList(FlagStr, UnicodeString(L"0x") + IntToHex(Flags, 2), L",");
    }
    if (FlagStr.IsEmpty())
    {
      FlagStr = L"none";
    }
    FTerminal->LogEvent(FORMAT(L"Flags: %s", (FlagStr)));
    FTerminal->LogEvent(FORMAT(L"Max name length: %s", (IntToStr(NameMax))));

    ASpaceAvailable.BytesOnDevice = BlockSize * Blocks;
    ASpaceAvailable.UnusedBytesOnDevice = BlockSize * FreeBlocks;
    ASpaceAvailable.BytesAvailableToUser = 0;
    ASpaceAvailable.UnusedBytesAvailableToUser = BlockSize * AvailableBlocks;
    ASpaceAvailable.BytesPerAllocationUnit =
      (BlockSize > std::numeric_limits<unsigned long>::max()) ? 0 : static_cast<unsigned long>(BlockSize);
  }
}
//---------------------------------------------------------------------------
// transfer protocol
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CopyToRemote(TStrings * FilesToCopy,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  TAutoFlag AvoidBusyFlag(FAvoidBusy);
  FTerminal->DoCopyToRemote(FilesToCopy, TargetDir, CopyParam, Params, OperationProgress, tfPreCreateDir, OnceDoneOperation);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::SFTPConfirmOverwrite(
  const UnicodeString & SourceFullFileName, UnicodeString & TargetFileName,
  const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress,
  TSFTPOverwriteMode & OverwriteMode, const TOverwriteFileParams * FileParams)
{
  bool CanAppend = !FTerminal->IsEncryptingFiles() && ((FVersion < 4) || !OperationProgress->AsciiTransfer);
  unsigned int Answer;

  {
    TSuspendFileOperationProgress Suspend(OperationProgress);
    int Answers = qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll | qaAll | qaIgnore;

    // possibly we can allow alternate resume at least in some cases
    if (CanAppend)
    {
      Answers |= qaRetry;
    }
    TQueryButtonAlias Aliases[5];
    Aliases[0].Button = qaRetry;
    Aliases[0].Alias = LoadStr(APPEND_BUTTON);
    Aliases[0].GroupWith = qaNo;
    Aliases[0].GrouppedShiftState = TShiftState() << ssAlt;
    Aliases[1] = TQueryButtonAlias::CreateAllAsYesToNewerGrouppedWithYes();
    Aliases[2] = TQueryButtonAlias::CreateIgnoreAsRenameGrouppedWithNo();
    Aliases[3] = TQueryButtonAlias::CreateYesToAllGrouppedWithYes();
    Aliases[4] = TQueryButtonAlias::CreateNoToAllGrouppedWithNo();
    TQueryParams QueryParams(qpNeverAskAgainCheck);
    QueryParams.NoBatchAnswers = qaIgnore | qaRetry | qaAll;
    QueryParams.Aliases = Aliases;
    QueryParams.AliasesCount = LENOF(Aliases);
    Answer = FTerminal->ConfirmFileOverwrite(
      SourceFullFileName, TargetFileName, FileParams,
      Answers, &QueryParams,
      ReverseOperationSide(OperationProgress->Side),
      CopyParam, Params, OperationProgress);
  }

  if (CanAppend &&
      ((Answer == qaRetry) || (Answer == qaSkip)))
  {
    OperationProgress->LockUserSelections();
    try
    {
      // duplicated in TTerminal::ConfirmFileOverwrite
      bool CanAlternateResume =
        (FileParams->DestSize < FileParams->SourceSize) && !OperationProgress->AsciiTransfer;
      TBatchOverwrite BatchOverwrite =
        FTerminal->EffectiveBatchOverwrite(SourceFullFileName, CopyParam, Params, OperationProgress, true);
      // when mode is forced by batch, never query user
      if (BatchOverwrite == boAppend)
      {
        OverwriteMode = omAppend;
      }
      else if (CanAlternateResume &&
               ((BatchOverwrite == boResume) || (BatchOverwrite == boAlternateResume)))
      {
        OverwriteMode = omResume;
      }
      // no other option, but append
      else if (!CanAlternateResume)
      {
        OverwriteMode = omAppend;
      }
      else
      {
        TQueryParams Params(0, HELP_APPEND_OR_RESUME);

        {
          TSuspendFileOperationProgress Suspend(OperationProgress);
          Answer = FTerminal->QueryUser(FORMAT(LoadStr(APPEND_OR_RESUME2), (SourceFullFileName)),
            NULL, qaYes | qaNo | qaNoToAll | qaCancel, &Params);
        }

        switch (Answer)
        {
          case qaYes:
            OverwriteMode = omAppend;
            break;

          case qaNo:
            OverwriteMode = omResume;
            break;

          case qaNoToAll:
            OverwriteMode = omResume;
            OperationProgress->SetBatchOverwrite(boAlternateResume);
            break;

          default: DebugFail(); //fallthru
          case qaCancel:
            OperationProgress->SetCancelAtLeast(csCancel);
            Abort();
            break;
        }
      }
    }
    __finally
    {
      OperationProgress->UnlockUserSelections();
    }
  }
  else if (Answer == qaIgnore)
  {
    if (FTerminal->PromptUser(FTerminal->SessionData, pkFileName, LoadStr(RENAME_TITLE), L"",
          LoadStr(RENAME_PROMPT2), true, 0, TargetFileName))
    {
      OverwriteMode = omOverwrite;
    }
    else
    {
      OperationProgress->SetCancelAtLeast(csCancel);
      Abort();
    }
  }
  else
  {
    OverwriteMode = omOverwrite;
    switch (Answer)
    {
      case qaCancel:
        OperationProgress->SetCancelAtLeast(csCancel);
        Abort();
        break;

      case qaNo:
        throw ESkipFile();
    }
  }
}
//---------------------------------------------------------------------------
bool TSFTPFileSystem::SFTPConfirmResume(const UnicodeString DestFileName,
  bool PartialBiggerThanSource, TFileOperationProgressType * OperationProgress)
{
  bool ResumeTransfer;
  DebugAssert(OperationProgress);
  if (PartialBiggerThanSource)
  {
    unsigned int Answer;
    {
      TSuspendFileOperationProgress Suspend(OperationProgress);
      TQueryParams Params(qpAllowContinueOnError, HELP_PARTIAL_BIGGER_THAN_SOURCE);
      Answer = FTerminal->QueryUser(
        FMTLOAD(PARTIAL_BIGGER_THAN_SOURCE, (DestFileName)), NULL,
          qaOK | qaAbort, &Params, qtWarning);
    }

    if (Answer == qaAbort)
    {
      OperationProgress->SetCancelAtLeast(csCancel);
      Abort();
    }
    ResumeTransfer = false;
  }
  else if (FTerminal->Configuration->ConfirmResume)
  {
    unsigned int Answer;

    {
      TSuspendFileOperationProgress Suspend(OperationProgress);
      TQueryParams Params(qpAllowContinueOnError | qpNeverAskAgainCheck,
        HELP_RESUME_TRANSFER);
      // "abort" replaced with "cancel" to unify with "append/resume" query
      Answer = FTerminal->QueryUser(
        FMTLOAD(RESUME_TRANSFER2, (DestFileName)), NULL, qaYes | qaNo | qaCancel,
        &Params);
    }

    switch (Answer) {
      case qaNeverAskAgain:
        FTerminal->Configuration->ConfirmResume = false;
      case qaYes:
        ResumeTransfer = true;
        break;

      case qaNo:
        ResumeTransfer = false;
        break;

      case qaCancel:
      default:
        OperationProgress->SetCancelAtLeast(csCancel);
        ResumeTransfer = false; // shut up
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
bool __fastcall TSFTPFileSystem::DoesFileLookLikeSymLink(TRemoteFile * File)
{
  unsigned short AllInt = static_cast<unsigned short>(TRights::rfAll);
  return
    (FVersion < 4) &&
    ((*File->Rights & AllInt) == AllInt) &&
    (File->Size < 100);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::Source(
  TLocalFileHandle & Handle, const UnicodeString & TargetDir, UnicodeString & DestFileName,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags,
  TUploadSessionAction & Action, bool & ChildError)
{
  UnicodeString DestFullName = LocalCanonify(TargetDir + DestFileName);
  UnicodeString DestPartialFullName;
  bool ResumeAllowed;
  bool ResumeTransfer = false;
  bool DestFileExists = false;
  TRights DestRights;

  __int64 ResumeOffset = 0; // shut up

  // should we check for interrupted transfer?
  ResumeAllowed =
    !OperationProgress->AsciiTransfer &&
    CopyParam->AllowResume(OperationProgress->LocalSize, DestFileName) &&
    IsCapable(fcRename) &&
    !FTerminal->IsEncryptingFiles() &&
    (CopyParam->OnTransferIn == NULL);

  TOpenRemoteFileParams OpenParams;
  OpenParams.OverwriteMode = omOverwrite;

  TOverwriteFileParams FileParams;
  FileParams.SourceSize = OperationProgress->LocalSize;
  FileParams.SourceTimestamp = Handle.Modification;

  if (ResumeAllowed)
  {
    DestPartialFullName = DestFullName + PartialExt;

    if (FLAGCLEAR(Flags, tfNewDirectory))
    {
      FTerminal->LogEvent(L"Checking existence of file.");
      TRemoteFile * File = NULL;
      DestFileExists = RemoteFileExists(DestFullName, &File);

      OperationProgress->Progress();

      if (DestFileExists)
      {
        FTerminal->LogEvent(FORMAT(L"File exists: %s", (FTerminal->GetRemoteFileInfo(File))));
        OpenParams.DestFileSize = File->Resolve()->Size;
        FileParams.DestSize = OpenParams.DestFileSize;
        FileParams.DestTimestamp = File->Modification;
        DestRights = *File->Rights;
        // If destination file is symlink, never do resumable transfer,
        // as it would delete the symlink.
        if (File->IsSymLink)
        {
          ResumeAllowed = false;
          FTerminal->LogEvent(L"Existing file is symbolic link, not doing resumable transfer.");
        }
        // Also bit of heuristics to detect symlink on SFTP-3 and older
        // (which does not indicate symlink in SSH_FXP_ATTRS).
        // if file has all permissions and is small, then it is likely symlink.
        // also it is not likely that such a small file (if it is not symlink)
        // gets overwritten by large file (that would trigger resumable transfer).
        else if (DoesFileLookLikeSymLink(File))
        {
          ResumeAllowed = false;
          FTerminal->LogEvent(L"Existing file looks like a symbolic link, not doing resumable transfer.");
        }
        // Also never do resumable transfer for file owned by other user
        // as deleting and recreating the file would change ownership.
        // This won't for work for SFTP-3 (OpenSSH) as it does not provide
        // owner name (only UID) and we know only logged in user name (not UID)
        else if (!File->Owner.Name.IsEmpty() && !SameUserName(File->Owner.Name, FTerminal->UserName))
        {
          ResumeAllowed = false;
          FTerminal->LogEvent(
            FORMAT(L"Existing file is owned by another user [%s], not doing resumable transfer.", (File->Owner.Name)));
        }

        delete File;
        File = NULL;
      }

      if (ResumeAllowed)
      {
        FTerminal->LogEvent(L"Checking existence of partially transferred file.");
        if (RemoteFileExists(DestPartialFullName, &File))
        {
          ResumeOffset = File->Resolve()->Size; // Though partial file should not be symlink
          delete File;
          File = NULL;

          bool PartialBiggerThanSource = (ResumeOffset > OperationProgress->LocalSize);
          if (FLAGCLEAR(Params, cpNoConfirmation) &&
              FLAGCLEAR(Params, cpResume) &&
              !CopyParam->ResumeTransfer(Handle.FileName))
          {
            ResumeTransfer = SFTPConfirmResume(DestFileName,
              PartialBiggerThanSource, OperationProgress);
          }
          else
          {
            ResumeTransfer = !PartialBiggerThanSource;
          }

          if (!ResumeTransfer)
          {
            DoDeleteFile(DestPartialFullName, SSH_FXP_REMOVE);
            OperationProgress->Progress();
          }
          else
          {
            FTerminal->LogEvent(L"Resuming file transfer.");
          }
        }
        else
        {
          // partial upload file does not exist, check for full file
          if (DestFileExists)
          {
            UnicodeString PrevDestFileName = DestFileName;
            SFTPConfirmOverwrite(Handle.FileName, DestFileName,
              CopyParam, Params, OperationProgress, OpenParams.OverwriteMode, &FileParams);
            if (PrevDestFileName != DestFileName)
            {
              // update paths in case user changes the file name
              DestFullName = LocalCanonify(TargetDir + DestFileName);
              DestPartialFullName = DestFullName + PartialExt;
              FTerminal->LogEvent(L"Checking existence of new file.");
              DestFileExists = RemoteFileExists(DestFullName, NULL);
            }
          }
        }
      }
    }
  }

  // will the transfer be resumable?
  bool IntendedResume = (ResumeAllowed && (OpenParams.OverwriteMode == omOverwrite));
  bool DoResume = IntendedResume;

  UnicodeString RemoteFileName = DoResume ? DestPartialFullName : DestFullName;
  OpenParams.FileName = Handle.FileName;
  OpenParams.RemoteFileName = RemoteFileName;
  OpenParams.RemoteFullFileName = DestFullName;
  OpenParams.Resume = DoResume;
  OpenParams.Resuming = ResumeTransfer;
  OpenParams.OperationProgress = OperationProgress;
  OpenParams.CopyParam = CopyParam;
  OpenParams.Params = Params;
  OpenParams.FileParams = &FileParams;
  OpenParams.Confirmed = (CopyParam->OnTransferIn != NULL) && FLAGCLEAR(Params, cpAppend);
  OpenParams.DontRecycle = false;
  OpenParams.Recycled = false;

  FTerminal->LogEvent(0, L"Opening remote file.");
  FTerminal->FileOperationLoop(SFTPOpenRemote, OperationProgress, folAllowSkip,
    FMTLOAD(SFTP_CREATE_FILE_ERROR, (OpenParams.RemoteFileName)),
    &OpenParams);
  OperationProgress->Progress();

  DoResume = OpenParams.Resume;

  if (OpenParams.RemoteFileName != RemoteFileName)
  {
    DebugAssert(!DoResume);
    DebugAssert(UnixExtractFilePath(OpenParams.RemoteFileName) == UnixExtractFilePath(RemoteFileName));
    DestFullName = OpenParams.RemoteFileName;
    UnicodeString NewFileName = UnixExtractFileName(DestFullName);
    // We can get here either when user change target name or
    // when we intended to transfer via temporary file but we fails to create it in the end
    DebugAssert((DestFileName != NewFileName) || (IntendedResume && !DoResume));
    DestFileName = NewFileName;
  }

  Action.Destination(DestFullName);

  bool TransferFinished = false;
  __int64 DestWriteOffset = 0;
  TSFTPPacket CloseRequest;
  bool PreserveRights = CopyParam->PreserveRights && (CopyParam->OnTransferIn == NULL);
  bool PreserveExistingRights = (DoResume && DestFileExists) || OpenParams.Recycled;
  bool SetRights = (PreserveExistingRights || PreserveRights);
  bool PreserveTime = CopyParam->PreserveTime && (CopyParam->OnTransferIn == NULL);
  bool SetProperties = (PreserveTime || SetRights);
  TSFTPPacket PropertiesRequest(SSH_FXP_SETSTAT);
  TSFTPPacket PropertiesResponse;
  TRights Rights;
  if (SetProperties)
  {
    AddPathString(PropertiesRequest, DestFullName);
    if (PreserveRights)
    {
      Rights = CopyParam->RemoteFileRights(Handle.Attrs);
    }
    else if (PreserveExistingRights)
    {
      if (DestFileExists)
      {
        Rights = DestRights;
      }
      else
      {
        Rights = OpenParams.RecycledRights;
      }
    }
    else
    {
      DebugAssert(!SetRights);
    }

    unsigned short RightsNumber = Rights.NumberSet;
    PropertiesRequest.AddProperties(
      SetRights ? &RightsNumber : NULL, NULL, NULL,
      PreserveTime ? &Handle.MTime : NULL,
      NULL, NULL, false, FVersion, FUtfStrings);
  }

  try
  {
    if (OpenParams.OverwriteMode == omAppend)
    {
      FTerminal->LogEvent(L"Appending file.");
      DestWriteOffset = OpenParams.DestFileSize;
    }
    else if (ResumeTransfer || (OpenParams.OverwriteMode == omResume))
    {
      if (OpenParams.OverwriteMode == omResume)
      {
        FTerminal->LogEvent(L"Resuming file transfer (append style).");
        ResumeOffset = OpenParams.DestFileSize;
      }
      FileSeek((THandle)Handle.Handle, ResumeOffset, soBeginning);
      OperationProgress->AddResumed(ResumeOffset);
    }

    TEncryption Encryption(FTerminal->GetEncryptKey());
    bool Encrypt = FTerminal->IsFileEncrypted(DestFullName, CopyParam->EncryptNewFiles);
    TValueRestorer<TSecureShellMode> SecureShellModeRestorer(FSecureShell->Mode, ssmUploading);
    TSFTPUploadQueue Queue(this, (Encrypt ? &Encryption : NULL), FTerminal->SessionData->SFTPUploadQueue);
    try
    {
      int ConvertParams =
        FLAGMASK(CopyParam->RemoveCtrlZ, cpRemoveCtrlZ) |
        FLAGMASK(CopyParam->RemoveBOM, cpRemoveBOM);
      Queue.Init(Handle.FileName, Handle.Handle, CopyParam->OnTransferIn, OperationProgress,
        OpenParams.RemoteFileHandle,
        DestWriteOffset + OperationProgress->TransferredSize,
        ConvertParams);

      while (Queue.Continue())
      {
        if (OperationProgress->Cancel)
        {
          if (OperationProgress->ClearCancelFile())
          {
            throw ESkipFile();
          }
          else
          {
            Abort();
          }
        }
      }

      // send close request before waiting for pending read responses
      SFTPCloseRemote(OpenParams.RemoteFileHandle, DestFileName,
        OperationProgress, false, true, &CloseRequest);
      OpenParams.RemoteFileHandle = L"";

      // when resuming is disabled, we can send "set properties"
      // request before waiting for pending read/close responses
      if (SetProperties && !DoResume)
      {
        SendPacket(&PropertiesRequest);
        ReserveResponse(&PropertiesRequest, &PropertiesResponse);
      }
      // No error so far, processes pending responses and throw on first error
      Queue.DisposeSafeWithErrorHandling();
    }
    __finally
    {
      // Either queue is empty now (noop call then),
      // or some error occurred (in that case, process remaining responses, ignoring other errors)
      Queue.DisposeSafe();
    }

    TransferFinished = true;
    // queue is discarded here
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

      // delete file if transfer was not completed, resuming was not allowed and
      // we were not appending (incl. alternate resume),
      // shortly after plain transfer completes (eq. !ResumeAllowed)
      if (!TransferFinished && !DoResume && (OpenParams.OverwriteMode == omOverwrite))
      {
        DoDeleteFile(OpenParams.RemoteFileName, SSH_FXP_REMOVE);
      }
    }
  }

  OperationProgress->Progress();

  if (DoResume)
  {
    if (DestFileExists)
    {
      FILE_OPERATION_LOOP_BEGIN
      {
        if (FTerminal->SessionData->OverwrittenToRecycleBin &&
            !FTerminal->SessionData->RecycleBinPath.IsEmpty())
        {
          FTerminal->RecycleFile(DestFullName, NULL);
        }
        else
        {
          DoDeleteFile(DestFullName, SSH_FXP_REMOVE);
        }
      }
      FILE_OPERATION_LOOP_END(
        FMTLOAD(DELETE_ON_RESUME_ERROR,
          (UnixExtractFileName(DestFullName), DestFullName)));
    }

    // originally this was before CLOSE (last __finally statement),
    // on VShell it failed
    FILE_OPERATION_LOOP_BEGIN
    {
      RenameFile(OpenParams.RemoteFileName, NULL, DestFileName, false);
    }
    FILE_OPERATION_LOOP_END_CUSTOM(
      FMTLOAD(RENAME_AFTER_RESUME_ERROR,
        (UnixExtractFileName(OpenParams.RemoteFileName), DestFileName)),
      folAllowSkip, HELP_RENAME_AFTER_RESUME_ERROR);
  }

  if (SetProperties)
  {
    std::unique_ptr<TTouchSessionAction> TouchAction;
    if (PreserveTime)
    {
      TDateTime MDateTime = UnixToDateTime(Handle.MTime, FTerminal->SessionData->DSTMode);
      FTerminal->LogEvent(FORMAT(L"Preserving timestamp [%s]",
        (StandardTimestamp(MDateTime))));
      TouchAction.reset(new TTouchSessionAction(FTerminal->ActionLog, DestFullName,
        MDateTime));
    }
    std::unique_ptr<TChmodSessionAction> ChmodAction;
    // do record chmod only if it was explicitly requested,
    // not when it was implicitly performed to apply timestamp
    // of overwritten file to new file
    if (PreserveRights)
    {
      ChmodAction.reset(new TChmodSessionAction(FTerminal->ActionLog, DestFullName, Rights));
    }
    try
    {
      // when resuming is enabled, the set properties request was not sent yet
      if (DoResume)
      {
        SendPacket(&PropertiesRequest);
      }
      bool Resend = false;
      FILE_OPERATION_LOOP_BEGIN
      {
        try
        {
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
          ReceiveResponse(&PropertiesRequest, Response, SSH_FXP_STATUS,
            asOK | FLAGMASK(CopyParam->IgnorePermErrors, asPermDenied));
        }
        catch (...)
        {
          if (FTerminal->Active &&
              (!PreserveRights && !PreserveTime))
          {
            DebugAssert(DoResume);
            FTerminal->LogEvent(L"Ignoring error preserving permissions of overwritten file");
          }
          else
          {
            throw;
          }
        }
      }
      FILE_OPERATION_LOOP_END_CUSTOM(
        FMTLOAD(PRESERVE_TIME_PERM_ERROR3, (DestFileName)),
        folAllowSkip, HELP_PRESERVE_TIME_PERM_ERROR);
    }
    catch(Exception & E)
    {
      if (TouchAction.get() != NULL)
      {
        TouchAction->Rollback(&E);
      }
      if (ChmodAction.get() != NULL)
      {
        ChmodAction->Rollback(&E);
      }
      ChildError = true;
      throw;
    }
  }
}
//---------------------------------------------------------------------------
RawByteString __fastcall TSFTPFileSystem::SFTPOpenRemoteFile(
  const UnicodeString & FileName, unsigned int OpenType, bool EncryptNewFiles, __int64 Size)
{
  TSFTPPacket Packet(SSH_FXP_OPEN);

  AddPathString(Packet, FileName, EncryptNewFiles);
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

  bool SendSize =
    (Size >= 0) &&
    FLAGSET(OpenType, SSH_FXF_CREAT | SSH_FXF_TRUNC) &&
    // Particularly VanDyke VShell (4.0.3) does not support SSH_FILEXFER_ATTR_ALLOCATION_SIZE
    // (it fails open request when the attribute is included).
    // It's SFTP-6 attribute, so support structure should be available.
    // It's actually not with VShell. But VShell supports the SSH_FILEXFER_ATTR_ALLOCATION_SIZE.
    // All servers should support SSH_FILEXFER_ATTR_SIZE (SFTP < 6)
    (!FSupport->Loaded || FLAGSET(FSupport->AttributeMask, Packet.AllocationSizeAttribute(FVersion)));
  Packet.AddProperties(NULL, NULL, NULL, NULL, NULL,
    SendSize ? &Size : NULL, false, FVersion, FUtfStrings);

  SendPacketAndReceiveResponse(&Packet, &Packet, SSH_FXP_HANDLE);

  return Packet.GetFileHandle();
}
//---------------------------------------------------------------------------
int __fastcall TSFTPFileSystem::SFTPOpenRemote(void * AOpenParams, void * /*Param2*/)
{
  TOpenRemoteFileParams * OpenParams = (TOpenRemoteFileParams *)AOpenParams;
  DebugAssert(OpenParams);
  TFileOperationProgressType * OperationProgress = OpenParams->OperationProgress;

  int OpenType;
  bool Success = false;
  bool ConfirmOverwriting;

  do
  {
    try
    {
      ConfirmOverwriting =
        !OpenParams->Confirmed && !OpenParams->Resume &&
        FTerminal->CheckRemoteFile(OpenParams->FileName, OpenParams->CopyParam, OpenParams->Params, OperationProgress);
      OpenType = SSH_FXF_WRITE | SSH_FXF_CREAT;
      // when we want to preserve overwritten files, we need to find out that
      // they exist first... even if overwrite confirmation is disabled.
      // but not when we already know we are not going to overwrite (but e.g. to append)
      if ((ConfirmOverwriting || (FTerminal->SessionData->OverwrittenToRecycleBin && !OpenParams->DontRecycle)) &&
          (OpenParams->OverwriteMode == omOverwrite))
      {
        OpenType |= SSH_FXF_EXCL;
      }
      if (!OpenParams->Resuming && (OpenParams->OverwriteMode == omOverwrite))
      {
        OpenType |= SSH_FXF_TRUNC;
      }
      if ((FVersion >= 4) && OpenParams->OperationProgress->AsciiTransfer)
      {
        OpenType |= SSH_FXF_TEXT;
      }

      OpenParams->RemoteFileHandle =
        SFTPOpenRemoteFile(OpenParams->RemoteFileName, OpenType, OpenParams->CopyParam->EncryptNewFiles, OperationProgress->LocalSize);

      Success = true;
    }
    catch(Exception & E)
    {
      if (OpenParams->Resume && !OpenParams->Resuming &&
          (OpenParams->RemoteFileName != OpenParams->RemoteFullFileName) &&
          (OpenParams->CopyParam->ResumeSupport == rsSmart) && FTerminal->Active)
      {
        FTerminal->LogEvent(FORMAT(L"Cannot create new partial file \"%s\", trying to create target file \"%s\"", (OpenParams->RemoteFileName, OpenParams->RemoteFullFileName)));
        OpenParams->RemoteFileName = OpenParams->RemoteFullFileName;
        OpenParams->Resume = false;
      }
      else if (!OpenParams->Confirmed && (OpenType & SSH_FXF_EXCL) && FTerminal->Active)
      {
        FTerminal->LogEvent(FORMAT(L"Cannot create new file \"%s\", checking if it exists already", (OpenParams->RemoteFileName)));

        bool ThrowOriginal = false;
        std::unique_ptr<TRemoteFile> File;

        // When exclusive opening of file fails, try to detect if file exists.
        // When file does not exist, failure was probably caused by 'permission denied'
        // or similar error. In this case throw original exception.
        try
        {
          OperationProgress->Progress();
          TRemoteFile * AFile;
          UnicodeString RealFileName = LocalCanonify(OpenParams->RemoteFileName);
          ReadFile(RealFileName, AFile);
          File.reset(AFile);
          File->FullFileName = RealFileName;
          OpenParams->DestFileSize = File->Size; // Resolve symlinks?
          if (OpenParams->FileParams != NULL)
          {
            OpenParams->FileParams->DestTimestamp = File->Modification;
            OpenParams->FileParams->DestSize = OpenParams->DestFileSize;
          }
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

        // we may get here even if confirmation is disabled,
        // when we have preserving of overwritten files enabled
        if (ConfirmOverwriting)
        {
          OperationProgress->Progress();
          // confirmation duplicated in SFTPSource for resumable file transfers.
          UnicodeString RemoteFileNameOnly = UnixExtractFileName(OpenParams->RemoteFileName);
          SFTPConfirmOverwrite(OpenParams->FileName, RemoteFileNameOnly,
            OpenParams->CopyParam, OpenParams->Params, OperationProgress, OpenParams->OverwriteMode, OpenParams->FileParams);
          if (RemoteFileNameOnly != UnixExtractFileName(OpenParams->RemoteFileName))
          {
            OpenParams->RemoteFileName =
              UnixExtractFilePath(OpenParams->RemoteFileName) + RemoteFileNameOnly;
            // no longer points to a relevant file
            File.reset(NULL);
          }
          OpenParams->Confirmed = true;
        }
        else
        {
          DebugAssert(FTerminal->SessionData->OverwrittenToRecycleBin);
        }

        if ((OpenParams->OverwriteMode == omOverwrite) &&
            FTerminal->SessionData->OverwrittenToRecycleBin &&
            !FTerminal->SessionData->RecycleBinPath.IsEmpty())
        {
          bool IsSymLink = (File.get() != NULL) && File->IsSymLink;
          if (!IsSymLink && (File.get() != NULL) && DoesFileLookLikeSymLink(File.get()) && IsCapable(fcResolveSymlink))
          {
            FTerminal->LogEvent(L"Existing file looks like a symbolic link, checking if it really is.");
            try
            {
              OperationProgress->Progress();
              TRemoteFile * LinkedFile;
              ReadSymlink(File.get(), LinkedFile);
              delete LinkedFile;
              IsSymLink = true;
            }
            catch (...)
            {
              if (!FTerminal->Active)
              {
                throw;
              }
            }
          }

          if (IsSymLink)
          {
            FTerminal->LogEvent(L"Existing file is a symbolic link, it will not be moved to a recycle bin.");
            OpenParams->DontRecycle = true;
          }
          else
          {
            OperationProgress->Progress();
            if (!FTerminal->RecycleFile(OpenParams->RemoteFileName, NULL))
            {
              // Allow normal overwrite
              OpenParams->DontRecycle = true;
            }
            else
            {
              OpenParams->Recycled = true;
              OpenParams->RecycledRights = *File->Rights;
            }
          }
        }
      }
      else if (FTerminal->Active)
      {
        // if file overwriting was confirmed, it means that the file already exists,
        // if not, check now
        if (!OpenParams->Confirmed)
        {
          bool ThrowOriginal = false;

          // When file does not exist, failure was probably caused by 'permission denied'
          // or similar error. In this case throw original exception.
          try
          {
            TRemoteFile * File;
            UnicodeString RealFileName = LocalCanonify(OpenParams->RemoteFileName);
            ReadFile(RealFileName, File);
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
        }

        // now we know that the file exists

        if (FTerminal->FileOperationLoopQuery(E, OperationProgress,
              FMTLOAD(SFTP_OVERWRITE_FILE_ERROR2, (OpenParams->RemoteFileName)),
              folAllowSkip, LoadStr(SFTP_OVERWRITE_DELETE_BUTTON)))
        {
          OperationProgress->Progress();
          int Params = dfNoRecursive;
          FTerminal->DeleteFile(OpenParams->RemoteFileName, NULL, &Params);
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
void __fastcall TSFTPFileSystem::SFTPCloseRemote(const RawByteString Handle,
  const UnicodeString FileName, TFileOperationProgressType * OperationProgress,
  bool TransferFinished, bool Request, TSFTPPacket * Packet)
{
  // Moving this out of SFTPSource() fixed external exception 0xC0000029 error
  FILE_OPERATION_LOOP_BEGIN
  {
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
        DebugAssert(Packet != NULL);
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
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(SFTP_CLOSE_FILE_ERROR, (FileName)));
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::CopyToLocal(TStrings * FilesToCopy,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  TAutoFlag AvoidBusyFlag(FAvoidBusy);
  FTerminal->DoCopyToLocal(FilesToCopy, TargetDir, CopyParam, Params, OperationProgress, tfNone, OnceDoneOperation);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::DirectorySunk(
  const UnicodeString & DestFullName, const TRemoteFile * File, const TCopyParamType * CopyParam)
{
  if (CopyParam->PreserveTime && CopyParam->PreserveTimeDirs)
  {
    // FILE_FLAG_BACKUP_SEMANTICS is needed to "open" directory
    HANDLE LocalHandle =
      CreateFile(
        ApiPath(DestFullName).c_str(), GENERIC_WRITE, FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
        FILE_FLAG_BACKUP_SEMANTICS, 0);

    if (LocalHandle == INVALID_HANDLE_VALUE)
    {
      int SetFileTimeError = GetLastError();
      FTerminal->LogEvent(
        FORMAT(L"Preserving directory timestamp failed, ignoring: %s", (SysErrorMessageForError(SetFileTimeError))));
    }
    else
    {
      FTerminal->UpdateTargetTime(
        LocalHandle, File->Modification, File->ModificationFmt, FTerminal->SessionData->DSTMode);
      CloseHandle(LocalHandle);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::WriteLocalFile(
  const TCopyParamType * CopyParam, TStream * FileStream, TFileBuffer & BlockBuf, const UnicodeString & LocalFileName,
  TFileOperationProgressType * OperationProgress)
{
  if (CopyParam->OnTransferOut != NULL)
  {
    BlockBuf.WriteToOut(CopyParam->OnTransferOut, FTerminal, BlockBuf.Size);
  }
  else
  {
    FILE_OPERATION_LOOP_BEGIN
    {
      BlockBuf.WriteToStream(FileStream, BlockBuf.Size);
    }
    FILE_OPERATION_LOOP_END(FMTLOAD(WRITE_ERROR, (LocalFileName)));
  }

  OperationProgress->AddLocallyUsed(BlockBuf.Size);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::Sink(
  const UnicodeString & FileName, const TRemoteFile * File,
  const UnicodeString & TargetDir, UnicodeString & DestFileName, int Attrs,
  const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress,
  unsigned int /*Flags*/, TDownloadSessionAction & Action)
{
  // resume has no sense for temporary downloads
  bool ResumeAllowed =
    FLAGCLEAR(Params, cpTemporary) &&
    !OperationProgress->AsciiTransfer &&
    CopyParam->AllowResume(OperationProgress->TransferSize, DestFileName) &&
    !FTerminal->IsEncryptingFiles() &&
    (CopyParam->OnTransferOut == NULL) &&
    (CopyParam->PartOffset < 0);

  HANDLE LocalHandle = NULL;
  TStream * FileStream = NULL;
  bool DeleteLocalFile = false;
  RawByteString RemoteHandle;
  UnicodeString DestFullName = TargetDir + DestFileName;
  UnicodeString LocalFileName = DestFullName;
  TSFTPOverwriteMode OverwriteMode = omOverwrite;

  try
  {
    bool ResumeTransfer = false;
    UnicodeString DestPartialFullName;

    if (ResumeAllowed)
    {
      DestPartialFullName = DestFullName + PartialExt;
      LocalFileName = DestPartialFullName;

      FTerminal->LogEvent(L"Checking existence of partially transferred file.");
      if (FileExists(ApiPath(DestPartialFullName)))
      {
        FTerminal->LogEvent(L"Partially transferred file exists.");
        __int64 ResumeOffset;
        FTerminal->OpenLocalFile(DestPartialFullName, GENERIC_WRITE,
          NULL, &LocalHandle, NULL, NULL, NULL, &ResumeOffset);

        bool PartialBiggerThanSource = (ResumeOffset > OperationProgress->TransferSize);
        if (FLAGCLEAR(Params, cpNoConfirmation))
        {
          ResumeTransfer = SFTPConfirmResume(DestFileName, PartialBiggerThanSource, OperationProgress);
        }
        else
        {
          ResumeTransfer = !PartialBiggerThanSource;
          if (!ResumeTransfer)
          {
            FTerminal->LogEvent(L"Partially transferred file is bigger than original file.");
          }
        }

        if (!ResumeTransfer)
        {
          CloseHandle(LocalHandle);
          LocalHandle = NULL;
          FTerminal->DoDeleteLocalFile(DestPartialFullName);
        }
        else
        {
          FTerminal->LogEvent(L"Resuming file transfer.");
          FileSeek((THandle)LocalHandle, ResumeOffset, soBeginning);
          OperationProgress->AddResumed(ResumeOffset);
        }
      }

      OperationProgress->Progress();
    }

    // first open source file, not to loose the destination file,
    // if we cannot open the source one in the first place
    FTerminal->LogEvent(L"Opening remote file.");
    FILE_OPERATION_LOOP_BEGIN
    {
      int OpenType = SSH_FXF_READ;
      if ((FVersion >= 4) && OperationProgress->AsciiTransfer)
      {
        OpenType |= SSH_FXF_TEXT;
      }
      RemoteHandle = SFTPOpenRemoteFile(FileName, OpenType);
      OperationProgress->Progress();
    }
    FILE_OPERATION_LOOP_END(FMTLOAD(SFTP_OPEN_FILE_ERROR, (FileName)));

    TSFTPPacket RemoteFilePacket(SSH_FXP_FSTAT);
    RemoteFilePacket.AddString(RemoteHandle);
    SendCustomReadFile(&RemoteFilePacket, &RemoteFilePacket, SSH_FILEXFER_ATTR_MODIFYTIME);
    ReceiveResponse(&RemoteFilePacket, &RemoteFilePacket);
    OperationProgress->Progress();

    TDateTime Modification = File->Modification; // fallback
    TModificationFmt ModificationFmt = File->ModificationFmt;
    // ignore errors
    if (RemoteFilePacket.Type == SSH_FXP_ATTRS)
    {
      // load file, avoid completion (resolving symlinks) as we do not need that
      std::unique_ptr<TRemoteFile> AFile(
        LoadFile(&RemoteFilePacket, NULL, UnixExtractFileName(FileName), NULL, false));
      if (AFile->Modification != TDateTime())
      {
        Modification = File->Modification;
        ModificationFmt = File->ModificationFmt;
      }
    }

    if ((Attrs >= 0) && !ResumeTransfer)
    {
      __int64 DestFileSize;
      __int64 MTime;
      FTerminal->OpenLocalFile(
        DestFullName, GENERIC_WRITE, NULL, &LocalHandle, NULL, &MTime, NULL, &DestFileSize, false);

      FTerminal->LogEvent(L"Confirming overwriting of file.");
      TOverwriteFileParams FileParams;
      FileParams.SourceSize = OperationProgress->TransferSize;
      FileParams.SourceTimestamp = Modification;
      FileParams.DestTimestamp = UnixToDateTime(MTime,
        FTerminal->SessionData->DSTMode);
      FileParams.DestSize = DestFileSize;
      UnicodeString PrevDestFileName = DestFileName;
      SFTPConfirmOverwrite(FileName, DestFileName, CopyParam, Params, OperationProgress, OverwriteMode, &FileParams);
      if (PrevDestFileName != DestFileName)
      {
        DestFullName = TargetDir + DestFileName;
        DestPartialFullName = DestFullName + PartialExt;
        if (ResumeAllowed)
        {
          if (FileExists(ApiPath(DestPartialFullName)))
          {
            FTerminal->DoDeleteLocalFile(DestPartialFullName);
          }
          LocalFileName = DestPartialFullName;
        }
        else
        {
          LocalFileName = DestFullName;
        }
      }

      if (OverwriteMode == omOverwrite)
      {
        // is NULL when overwriting read-only file
        if (LocalHandle)
        {
          CloseHandle(LocalHandle);
          LocalHandle = NULL;
        }
      }
      else
      {
        // is NULL when overwriting read-only file, so following will
        // probably fail anyway
        if (LocalHandle == NULL)
        {
          FTerminal->OpenLocalFile(DestFullName, GENERIC_WRITE, NULL, &LocalHandle, NULL, NULL, NULL, NULL);
        }
        ResumeAllowed = false;
        FileSeek((THandle)LocalHandle, DestFileSize, soBeginning);
        if (OverwriteMode == omAppend)
        {
          FTerminal->LogEvent(L"Appending to file.");
        }
        else
        {
          FTerminal->LogEvent(L"Resuming file transfer (append style).");
          DebugAssert(OverwriteMode == omResume);
          OperationProgress->AddResumed(DestFileSize);
        }
      }
    }

    Action.Destination(ExpandUNCFileName(DestFullName));

    if (CopyParam->OnTransferOut == NULL)
    {
      // if not already opened (resume, append...), create new empty file
      if (!LocalHandle)
      {
        if (!FTerminal->CreateLocalFile(LocalFileName, OperationProgress,
               &LocalHandle, FLAGSET(Params, cpNoConfirmation)))
        {
          throw ESkipFile();
        }
      }
      DebugAssert(LocalHandle);

      DeleteLocalFile = true;

      FileStream = new TSafeHandleStream((THandle)LocalHandle);
    }

    // at end of this block queue is discarded
    {
      TValueRestorer<TSecureShellMode> SecureShellModeRestorer(FSecureShell->Mode, ssmDownloading);
      TSFTPDownloadQueue Queue(this);
      try
      {
        TSFTPPacket DataPacket;

        int QueueLen = int(OperationProgress->TransferSize / DownloadBlockSize(OperationProgress)) + 1;
        if ((QueueLen > FTerminal->SessionData->SFTPDownloadQueue) ||
            (QueueLen < 0))
        {
          QueueLen = FTerminal->SessionData->SFTPDownloadQueue;
        }
        if (QueueLen < 1)
        {
          QueueLen = 1;
        }
        __int64 Offset = OperationProgress->TransferredSize + std::max(CopyParam->PartOffset, 0LL);
        Queue.Init(QueueLen, RemoteHandle, Offset, CopyParam->PartSize, OperationProgress);

        bool Eof = false;
        bool PrevIncomplete = false;
        int GapFillCount = 0;
        int GapCount = 0;
        unsigned long Missing = 0;
        unsigned long DataLen = 0;
        unsigned long BlockSize = 0; // shut up
        bool ConvertToken = false;
        TEncryption Encryption(FTerminal->GetEncryptKey());
        bool Decrypt = FTerminal->IsFileEncrypted(FileName);

        while (!Eof)
        {
          if (Missing > 0)
          {
            Queue.InitFillGapRequest(Offset + OperationProgress->TransferredSize, Missing, &DataPacket);
            GapFillCount++;
            SendPacketAndReceiveResponse(&DataPacket, &DataPacket, SSH_FXP_DATA, asEOF);
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
            SFTPCloseRemote(RemoteHandle, DestFileName, OperationProgress, true, true, NULL);
            RemoteHandle = L""; // do not close file again in __finally block
          }

          if (!Eof)
          {
            if ((Missing == 0) && PrevIncomplete)
            {
              // This can happen only if last request returned less bytes
              // than expected, but exactly number of bytes missing to last
              // known file size, but actually EOF was not reached.
              // Can happen only when filesize has changed since directory
              // listing and server returns less bytes than requested and
              // file has some special file size.
              FTerminal->LogEvent(FORMAT(
                L"Received incomplete data packet before end of file, offset: %s, size: %d, requested: %d",
                (IntToStr(OperationProgress->TransferredSize), int(DataLen), int(BlockSize))));
              FTerminal->TerminalError(NULL, LoadStr(SFTP_INCOMPLETE_BEFORE_EOF));
            }

            // Buffer for one block of data
            TFileBuffer BlockBuf;

            DataLen = DataPacket.GetCardinal();

            PrevIncomplete = false;
            if (Missing > 0)
            {
              DebugAssert(DataLen <= Missing);
              Missing -= DataLen;
            }
            else if (DataLen < BlockSize)
            {
              if (OperationProgress->TransferredSize + DataLen !=
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

            DebugAssert(DataLen <= BlockSize);
            BlockBuf.Insert(0, reinterpret_cast<const char *>(DataPacket.GetNextData(DataLen)), DataLen);
            DataPacket.DataConsumed(DataLen);
            OperationProgress->AddTransferred(DataLen);

            if ((FVersion >= 6) && DataPacket.CanGetBool() && (Missing == 0))
            {
              Eof = DataPacket.GetBool();
            }

            if ((CopyParam->PartSize >= 0) &&
                (OperationProgress->TransferredSize >= CopyParam->PartSize))
            {
              Eof = true;
            }

            if (OperationProgress->AsciiTransfer)
            {
              DebugAssert(!ResumeTransfer && !ResumeAllowed);

              unsigned int PrevBlockSize = BlockBuf.Size;
              BlockBuf.Convert(GetEOL(), FTerminal->Configuration->LocalEOLType, 0, ConvertToken);
              OperationProgress->SetLocalSize(OperationProgress->LocalSize - PrevBlockSize + BlockBuf.Size);
            }

            if (Decrypt)
            {
              Encryption.Decrypt(BlockBuf);
            }

            WriteLocalFile(CopyParam, FileStream, BlockBuf, LocalFileName, OperationProgress);
          }

          if (OperationProgress->Cancel != csContinue)
          {
            if (OperationProgress->ClearCancelFile())
            {
              throw ESkipFile();
            }
            else
            {
              Abort();
            }
          }
        };

        if (GapCount > 0)
        {
          FTerminal->LogEvent(FORMAT(L"%d requests to fill %d data gaps were issued.", (GapFillCount, GapCount)));
        }

        if (Decrypt)
        {
          TFileBuffer BlockBuf;
          if (Encryption.DecryptEnd(BlockBuf))
          {
            WriteLocalFile(CopyParam, FileStream, BlockBuf, LocalFileName, OperationProgress);
          }
        }
      }
      __finally
      {
        Queue.DisposeSafe();
      }
      // queue is discarded here
    }

    if (CopyParam->OnTransferOut == NULL)
    {
      DebugAssert(LocalHandle);
      if (CopyParam->PreserveTime)
      {
        FTerminal->UpdateTargetTime(LocalHandle, Modification, ModificationFmt, FTerminal->SessionData->DSTMode);
      }

      CloseHandle(LocalHandle);
      LocalHandle = NULL;

      if (ResumeAllowed)
      {
        // See also DoRenameLocalFileForce
        FILE_OPERATION_LOOP_BEGIN
        {
          if (FileExists(ApiPath(DestFullName)))
          {
            DeleteFileChecked(DestFullName);
          }
          THROWOSIFFALSE(Sysutils::RenameFile(ApiPath(DestPartialFullName), ApiPath(DestFullName)));
        }
        FILE_OPERATION_LOOP_END(FMTLOAD(RENAME_AFTER_RESUME_ERROR, (ExtractFileName(DestPartialFullName), DestFileName)));
      }

      DeleteLocalFile = false;

      FTerminal->UpdateTargetAttrs(DestFullName, File, CopyParam, Attrs);
    }

  }
  __finally
  {
    if (LocalHandle)
    {
      CloseHandle(LocalHandle);
    }

    if (FileStream != NULL)
    {
      delete FileStream;
    }

    if (DeleteLocalFile && (!ResumeAllowed || OperationProgress->LocallyUsed == 0) &&
        (OverwriteMode == omOverwrite))
    {
      FTerminal->DoDeleteLocalFile(LocalFileName);
    }

    // if the transfer was finished, the file is usually closed already
    // (except for special cases like SFTPv6 EOF indication or partial file download)
    if (FTerminal->Active && !RemoteHandle.IsEmpty())
    {
      // do not wait for response
      SFTPCloseRemote(RemoteHandle, DestFileName, OperationProgress, true, true, NULL);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::RegisterChecksumAlg(const UnicodeString & Alg, const UnicodeString & SftpAlg)
{
  FChecksumAlgs->Add(Alg);
  FChecksumSftpAlgs->Add(SftpAlg);
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::GetSupportedChecksumAlgs(TStrings * Algs)
{
  Algs->AddStrings(FChecksumAlgs.get());
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::LockFile(const UnicodeString & /*FileName*/, const TRemoteFile * /*File*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::UnlockFile(const UnicodeString & /*FileName*/, const TRemoteFile * /*File*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::UpdateFromMain(TCustomFileSystem * /*MainFileSystem*/)
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TSFTPFileSystem::ClearCaches()
{
  // noop
}
//---------------------------------------------------------------------------
void TSFTPFileSystem::AddPathString(TSFTPPacket & Packet, const UnicodeString & Value, bool EncryptNewFiles)
{
  UnicodeString EncryptedPath = FTerminal->EncryptFileName(Value, EncryptNewFiles);
  Packet.AddPathString(EncryptedPath, FUtfStrings);
}
