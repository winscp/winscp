//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "Exceptions.h"
#include "PuttyIntf.h"
#include "HierarchicalStorage.h"
#include <Interface.h>
#include <TextsCore.h>
#include <StrUtils.hpp>
#include <vector>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValueExists test was probably added to avoid registry exceptions when debugging
#define READ_REGISTRY(Method) \
  if (FRegistry->ValueExists(Name)) \
  try { return FRegistry->Method(Name); } catch(...) { return Default; } \
  else return Default;
#define WRITE_REGISTRY(Method) \
  try { FRegistry->Method(Name, Value); } catch(...) { }
//---------------------------------------------------------------------------
UnicodeString PuttyEscape(const RawByteString & Source, bool Value)
{
  strbuf * sb = strbuf_new();
  escape_registry_key(Source.c_str(), sb);
  RawByteString Dest(sb->s);
  strbuf_free(sb);
  if (Value)
  {
    // We do not want to munge * in PasswordMask
    Dest = ReplaceStr(Dest, L"%2A", L"*");
  }
  return UnicodeString(Dest.c_str(), Dest.Length());
}
//---------------------------------------------------------------------------
bool ToUnicode(const UnicodeString & Str, RawByteString & Utf)
{
  Utf = RawByteString(UTF8String(Str));
  bool Result = (Utf.Length() > Str.Length());
  if (Result)
  {
    Utf.Insert(Bom, 1);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall MungeStr(const UnicodeString & Str, bool ForceAnsi, bool Value)
{
  RawByteString Source;
  if (ForceAnsi)
  {
    Source = RawByteString(PuttyStr(Str));
  }
  else
  {
    ToUnicode(Str, Source);
  }
  return PuttyEscape(Source, Value);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall UnMungeStr(const UnicodeString & Str)
{
  // Str should contain ASCII characters only
  RawByteString Source = AnsiString(Str);
  strbuf * sb = strbuf_new();
  unescape_registry_key(Source.c_str(), sb);
  RawByteString Dest(sb->s);
  strbuf_free(sb);
  UnicodeString Result;
  if (Dest.SubString(1, strlen(Bom)) == Bom)
  {
    Dest.Delete(1, strlen(Bom));
    Result = UTF8ToString(Dest);
  }
  else
  {
    Result = AnsiToString(Dest);
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString PuttyStr(const UnicodeString & Str)
{
  return AnsiString(Str);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall PuttyMungeStr(const UnicodeString & Str)
{
  return MungeStr(Str, true, false);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall MungeIniName(const UnicodeString & Str, bool ForceAnsi)
{
  bool NeedEscaping = false;
  for (int Index = 1; !NeedEscaping && (Index <= Str.Length()); Index++)
  {
    wchar_t Ch = Str[Index];
    NeedEscaping = IsWideChar(Ch) || (Ch == L'=');
  }
  UnicodeString Result;
  if (!ForceAnsi && NeedEscaping)
  {
    RawByteString Utf;
    ToUnicode(Str, Utf);
    Result = PuttyEscape(Utf, false);
  }
  else
  {
    Result = Str;
  }
  Result = ReplaceStr(Result, L"=", L"%3D");
  return Result;
}
//---------------------------------------------------------------------------
static UnicodeString EscapedBom(TraceInitStr(PuttyEscape(Bom, false)));
//---------------------------------------------------------------------------
UnicodeString __fastcall UnMungeIniName(const UnicodeString & Str)
{
  UnicodeString Result;
  if (StartsStr(EscapedBom, Str))
  {
    Result = UnMungeStr(Str);
  }
  else
  {
    // Backward compatibility only, with versions that did not Unicode-encoded strings with =.
    // Can be dropped eventually.
    int P = Str.Pos(L"%3D");
    if (P > 0)
    {
      Result = ReplaceStr(Str, L"%3D", L"=");
    }
    else
    {
      Result = Str;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
template<typename T>
void AddIntMapping(TIntMapping & Mapping, const wchar_t * Name, const T & Value)
{
  if (Name != NULL)
  {
    Mapping.insert(std::make_pair(UnicodeString(Name), Value));
  }
}
//---------------------------------------------------------------------------
template<typename T>
TIntMapping CreateIntMapping(
  const wchar_t * Name1, const T & Value1,
  const wchar_t * Name2 = NULL, const T & Value2 = T(0),
  const wchar_t * Name3 = NULL, const T & Value3 = T(0))
{
  TIntMapping Result;
  AddIntMapping(Result, Name1, Value1);
  AddIntMapping(Result, Name2, Value2);
  AddIntMapping(Result, Name3, Value3);
  return Result;
}
//---------------------------------------------------------------------------
TIntMapping CreateIntMappingFromEnumNames(const UnicodeString & ANames)
{
  UnicodeString Names(ANames);
  TIntMapping Result;
  int Index = 0;
  while (!Names.IsEmpty())
  {
    UnicodeString Name = CutToChar(Names, L';', true);
    Result.insert(std::make_pair(Name, Index));
    Index++;
  }
  return Result;
}
//---------------------------------------------------------------------------
TIntMapping AutoSwitchMapping = CreateIntMapping(L"on", asOn, L"off", asOff, L"auto", asAuto);
TIntMapping AutoSwitchReversedMapping = CreateIntMapping(L"on", asOff, L"off", asOn, L"auto", asAuto);
static TIntMapping BoolMapping = CreateIntMapping(L"on", true, L"off", false);
//===========================================================================
static UnicodeString AccessValueName(L"Access");
static UnicodeString DefaultAccessString(L"inherit");
//---------------------------------------------------------------------------
__fastcall THierarchicalStorage::THierarchicalStorage(const UnicodeString & AStorage)
{
  FStorage = AStorage;
  AccessMode = smRead;
  Explicit = false;
  ForceSave = false;
  // While this was implemented in 5.0 already, for some reason
  // it was disabled (by mistake?). So although enabled for 5.6.1 only,
  // data written in Unicode/UTF8 can be read by all versions back to 5.0.
  ForceAnsi = false;
  MungeStringValues = true;
  FFakeReadOnlyOpens = 0;
  FRootAccess = -1;
}
//---------------------------------------------------------------------------
__fastcall THierarchicalStorage::~THierarchicalStorage()
{
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::Flush()
{
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::SetAccessMode(TStorageAccessMode value)
{
  FAccessMode = value;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall THierarchicalStorage::GetCurrentSubKeyMunged()
{
  if (!FKeyHistory.empty())
  {
    return FKeyHistory.back().Key;
  }
  else
  {
    return UnicodeString();
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall THierarchicalStorage::GetCurrentSubKey()
{
  return UnMungeStr(GetCurrentSubKeyMunged());
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::ConfigureForPutty()
{
  MungeStringValues = false;
  ForceAnsi = true;
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::OpenRootKey(bool CanCreate)
{
  // This do not seem to be doing what it advertises.
  // It probably "works" only when used as a first "Open". So let's verify that by this assertion.
  DebugAssert(CurrentSubKey.IsEmpty());

  return OpenSubKey(UnicodeString(), CanCreate);
}
//---------------------------------------------------------------------------
bool THierarchicalStorage::MungingKeyName(const UnicodeString & Key)
{
  UnicodeString K = CurrentSubKey + Key + L"\\";
  return UnmungedRoot.IsEmpty() || !SameText(LeftStr(UnmungedRoot + L"\\", K.Length()), K);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall THierarchicalStorage::MungeKeyName(const UnicodeString & Key)
{
  UnicodeString Result;
  if (!MungingKeyName(Key))
  {
    Result = Key;
  }
  else
  {
    Result = MungeStr(Key, ForceAnsi, false);
    // if there's already ANSI-munged subkey, keep ANSI munging
    if ((Result != Key) && !ForceAnsi && CanRead() && DoKeyExists(Key, true))
    {
      Result = MungeStr(Key, true, false);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString THierarchicalStorage::MungeIniName(const UnicodeString & Str)
{
  UnicodeString Result = ::MungeIniName(Str, ForceAnsi);
  // This does not handle Ansi-encoded value with equal sign, but that's an edge case
  if ((Result != Str) && !ForceAnsi && CanRead() && DoValueExists(Str, true))
  {
    Result = ::MungeIniName(Str, true);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall THierarchicalStorage::DoReadRootAccessString()
{
  UnicodeString Result;
  if (OpenRootKey(false))
  {
    Result = ReadAccessString();
    CloseSubKey();
  }
  else
  {
    Result = DefaultAccessString;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall THierarchicalStorage::ReadAccessString()
{
  UnicodeString Result;
  if (!FKeyHistory.empty())
  {
    Result = ReadString(AccessValueName, DefaultAccessString);
  }
  else
  {
    Result = DoReadRootAccessString();
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned int __fastcall THierarchicalStorage::ReadAccess(unsigned int CurrentAccess)
{
  UnicodeString Access = ReadAccessString();
  unsigned int Result = 0;
  while (!Access.IsEmpty())
  {
    UnicodeString Token = CutToChar(Access, L',', true);
    if (SameText(Token, L"inherit"))
    {
      Result |= CurrentAccess;
    }
    else if (SameText(Token, "read"))
    {
      Result |= hsaRead;
    }
    else if (SameText(Token, "full"))
    {
      Result |= hsaRead | hsaWrite;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned int __fastcall THierarchicalStorage::GetCurrentAccess()
{
  unsigned int Result;
  if (!FKeyHistory.empty())
  {
    // We must have resolved root access when opening the sub key the latest.
    DebugAssert(FRootAccess >= 0);
    Result = FKeyHistory.back().Access;
  }
  else
  {
    if (FRootAccess < 0)
    {
      FRootAccess = hsaRead; // Prevent recursion to allow reading the access
      FRootAccess = ReadAccess(hsaRead | hsaWrite);
    }
    Result = FRootAccess;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::OpenSubKeyPath(const UnicodeString & KeyPath, bool CanCreate)
{
  DebugAssert(!KeyPath.IsEmpty() && (KeyPath[KeyPath.Length()] != L'\\'));
  bool Result = false; // shut up
  UnicodeString Buf(KeyPath);
  int Opens = 0;
  while (!Buf.IsEmpty())
  {
    UnicodeString SubKey = CutToChar(Buf, L'\\', false);
    Result = OpenSubKey(SubKey, CanCreate);
    if (Result)
    {
      Opens++;
    }
  }

  if (Result)
  {
    FKeyHistory.back().Levels = Opens;
  }
  else
  {
    while (Opens > 0)
    {
      CloseSubKey();
      Opens--;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::OpenSubKey(const UnicodeString & Key, bool CanCreate)
{
  UnicodeString MungedKey = MungeKeyName(Key);

  bool Result;
  unsigned int InheritAccess;
  unsigned int Access;
  // For the first open, CanWrite > GetCurrentAccess > ReadAccess has a (needed) side effect of caching root access.
  if (!CanWrite() && CanCreate && !KeyExists(MungedKey))
  {
    InheritAccess = Access = 0; // do not even try to read the access, as the key is actually not opened
    FFakeReadOnlyOpens++;
    Result = true;
  }
  else
  {
    Access = hsaRead; // allow reading the access
    InheritAccess = GetCurrentAccess();
    Result = DoOpenSubKey(MungedKey, CanCreate);
  }

  if (Result)
  {
    TKeyEntry Entry;
    Entry.Key = IncludeTrailingBackslash(CurrentSubKey + MungedKey);
    Entry.Levels = 1;
    Entry.Access = Access;
    FKeyHistory.push_back(Entry);
    // Read the real access only now, that the key is finally opened (it's in FKeyHistory)
    FKeyHistory.back().Access = ReadAccess(InheritAccess);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::CloseSubKeyPath()
{
  if (FKeyHistory.empty())
  {
    throw Exception(UnicodeString());
  }

  int Levels = FKeyHistory.back().Levels;
  FKeyHistory.back().Levels = 1; // to satisfy the assertion in CloseSubKey()
  while (Levels > 0)
  {
    CloseSubKey();
    Levels--;
    DebugAssert((Levels == 0) || (FKeyHistory.back().Levels == 1));
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::CloseSubKey()
{
  if (FKeyHistory.empty())
  {
    throw Exception(UnicodeString());
  }

  DebugAssert(FKeyHistory.back().Levels == 1);
  FKeyHistory.pop_back();
  if (FFakeReadOnlyOpens > 0)
  {
    FFakeReadOnlyOpens--;
  }
  else
  {
    DoCloseSubKey();
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::CloseAll()
{
  while (!CurrentSubKey.IsEmpty())
  {
    CloseSubKeyPath();
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::ClearSubKeys()
{
  std::unique_ptr<TStringList> SubKeys(new TStringList());
  GetSubKeyNames(SubKeys.get());
  for (int Index = 0; Index < SubKeys->Count; Index++)
  {
    RecursiveDeleteSubKey(SubKeys->Strings[Index]);
  }
}
//---------------------------------------------------------------------------
bool THierarchicalStorage::RecursiveDeleteSubKey(const UnicodeString & Key)
{
  return DeleteSubKey(Key, true);
}
//---------------------------------------------------------------------------
bool THierarchicalStorage::DeleteSubKey(const UnicodeString & Key, bool Recursive)
{
  bool CanWriteParent = CanWrite();
  bool Result = OpenSubKey(Key, false);
  if (Result)
  {
    if (Recursive)
    {
      ClearSubKeys();

      // Cannot delete the key itself, but can delete its contents, so at least delete the values
      // (which would otherwise be deleted implicitly by DoDeleteSubKey)
      if (!CanWriteParent && CanWrite())
      {
        ClearValues();
      }
    }

    std::unique_ptr<TStrings> ValueNames(new TStringList());
    if (!Recursive)
    {
      GetValueNames(ValueNames.get());
    }

    // Only if all subkeys were successfully deleted in ClearSubKeys
    Result = CanWriteParent && !HasSubKeys() && (ValueNames->Count == 0);

    CloseSubKey();

    if (Result)
    {
      Result = DoDeleteSubKey(Key);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::HasSubKeys()
{
  std::unique_ptr<TStrings> SubKeys(new TStringList());
  GetSubKeyNames(SubKeys.get());
  bool Result = (SubKeys->Count > 0);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::DeleteValue(const UnicodeString & Name)
{
  if (CanWrite())
  {
    return DoDeleteValue(Name);
  }
  else
  {
    return false;
  }
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::KeyExists(const UnicodeString & SubKey)
{
  if (CanRead())
  {
    return DoKeyExists(SubKey, ForceAnsi);
  }
  else
  {
    return false;
  }
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::ValueExists(const UnicodeString & Value)
{
  if (CanRead())
  {
    return DoValueExists(Value, ForceAnsi);
  }
  else
  {
    return false;
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::ReadValues(TStrings * Strings, bool MaintainKeys)
{
  std::unique_ptr<TStrings> Names(new TStringList());
  GetValueNames(Names.get());
  for (int Index = 0; Index < Names->Count; Index++)
  {
    if (MaintainKeys)
    {
      Strings->Add(FORMAT(L"%s=%s", (Names->Strings[Index], ReadString(Names->Strings[Index], UnicodeString()))));
    }
    else
    {
      Strings->Add(ReadString(Names->Strings[Index], UnicodeString()));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::ClearValues()
{
  std::unique_ptr<TStrings> Names(new TStringList());
  GetValueNames(Names.get());
  for (int Index = 0; Index < Names->Count; Index++)
  {
    DeleteValue(Names->Strings[Index]);
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteValues(TStrings * Strings, bool MaintainKeys)
{
  ClearValues();

  if (Strings != NULL)
  {
    for (int Index = 0; Index < Strings->Count; Index++)
    {
      if (MaintainKeys)
      {
        DebugAssert(Strings->Strings[Index].Pos(L"=") > 1);
        WriteString(Strings->Names[Index], Strings->Values[Strings->Names[Index]]);
      }
      else
      {
        WriteString(IntToStr(Index), Strings->Strings[Index]);
      }
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::HasAccess(unsigned int Access)
{
  return
    FLAGSET(GetCurrentAccess(), Access) &&
    // There should never be any kind of access to a non-existent key
    DebugAlwaysTrue(FFakeReadOnlyOpens == 0);
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::CanRead()
{
  return HasAccess(hsaRead);
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::GetSubKeyNames(TStrings * Strings)
{
  if (CanRead())
  {
    DoGetSubKeyNames(Strings);
  }
  else
  {
    Strings->Clear();
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::GetValueNames(TStrings * Strings)
{
  if (CanRead())
  {
    DoGetValueNames(Strings);

    int Index = 0;
    while (Index < Strings->Count)
    {
      if (SameText(Strings->Strings[Index], AccessValueName))
      {
        Strings->Delete(Index);
      }
      else
      {
        Index++;
      }
    }
  }
  else
  {
    Strings->Clear();
  }
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::ReadBool(const UnicodeString & Name, bool Default)
{
  if (CanRead())
  {
    return DoReadBool(Name, Default);
  }
  else
  {
    return Default;
  }
}
//---------------------------------------------------------------------------
int __fastcall THierarchicalStorage::ReadInteger(const UnicodeString & Name, int Default)
{
  return ReadIntegerWithMapping(Name, Default, NULL);
}
//---------------------------------------------------------------------------
int THierarchicalStorage::ReadIntegerWithMapping(const UnicodeString & Name, int Default, const TIntMapping * Mapping)
{
  if (CanRead())
  {
    return DoReadInteger(Name, Default, Mapping);
  }
  else
  {
    return Default;
  }
}
//---------------------------------------------------------------------------
__int64 __fastcall THierarchicalStorage::ReadInt64(const UnicodeString & Name, __int64 Default)
{
  if (CanRead())
  {
    return DoReadInt64(Name, Default);
  }
  else
  {
    return Default;
  }
}
//---------------------------------------------------------------------------
TDateTime __fastcall THierarchicalStorage::ReadDateTime(const UnicodeString & Name, TDateTime Default)
{
  if (CanRead())
  {
    return DoReadDateTime(Name, Default);
  }
  else
  {
    return Default;
  }
}
//---------------------------------------------------------------------------
double __fastcall THierarchicalStorage::ReadFloat(const UnicodeString & Name, double Default)
{
  if (CanRead())
  {
    return DoReadFloat(Name, Default);
  }
  else
  {
    return Default;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall THierarchicalStorage::ReadStringRaw(const UnicodeString & Name, const UnicodeString & Default)
{
  if (CanRead())
  {
    return DoReadStringRaw(Name, Default);
  }
  else
  {
    return Default;
  }
}
//---------------------------------------------------------------------------
size_t __fastcall THierarchicalStorage::ReadBinaryData(const UnicodeString & Name, void * Buffer, size_t Size)
{
  if (CanRead())
  {
    return DoReadBinaryData(Name, Buffer, Size);
  }
  else
  {
    return 0;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall THierarchicalStorage::ReadString(const UnicodeString & Name, const UnicodeString & Default)
{
  UnicodeString Result;
  if (CanRead())
  {
    if (MungeStringValues)
    {
      Result = UnMungeStr(ReadStringRaw(Name, MungeStr(Default, ForceAnsi, true)));
    }
    else
    {
      Result = ReadStringRaw(Name, Default);
    }
  }
  else
  {
    Result = Default;
  }
  return Result;
}
//---------------------------------------------------------------------------
size_t __fastcall THierarchicalStorage::BinaryDataSize(const UnicodeString & Name)
{
  if (CanRead())
  {
    return DoBinaryDataSize(Name);
  }
  else
  {
    return 0;
  }
}
//---------------------------------------------------------------------------
RawByteString __fastcall THierarchicalStorage::ReadBinaryData(const UnicodeString & Name)
{
  size_t Size = BinaryDataSize(Name);
  RawByteString Value;
  Value.SetLength(Size);
  ReadBinaryData(Name, Value.c_str(), Size);
  return Value;
}
//---------------------------------------------------------------------------
RawByteString __fastcall THierarchicalStorage::ReadStringAsBinaryData(const UnicodeString & Name, const RawByteString & Default)
{
  UnicodeString UnicodeDefault = AnsiToString(Default);
  // This should be exactly the same operation as calling ReadString in
  // C++Builder 6 (non-Unicode) on Unicode-based OS
  // (conversion is done by Ansi layer of the OS)
  UnicodeString String = ReadString(Name, UnicodeDefault);
  AnsiString Ansi = AnsiString(String);
  RawByteString Result = RawByteString(Ansi.c_str(), Ansi.Length());
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::CanWrite()
{
  return HasAccess(hsaWrite);
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteBool(const UnicodeString & Name, bool Value)
{
  if (CanWrite())
  {
    DoWriteBool(Name, Value);
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteStringRaw(const UnicodeString & Name, const UnicodeString & Value)
{
  if (CanWrite())
  {
    DoWriteStringRaw(Name, Value);
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteInteger(const UnicodeString & Name, int Value)
{
  if (CanWrite())
  {
    DoWriteInteger(Name, Value);
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteInt64(const UnicodeString & Name, __int64 Value)
{
  if (CanWrite())
  {
    DoWriteInt64(Name, Value);
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteDateTime(const UnicodeString & Name, TDateTime Value)
{
  if (CanWrite())
  {
    // TRegistry.WriteDateTime does this internally
    DoWriteBinaryData(Name, &Value, sizeof(Value));
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteFloat(const UnicodeString & Name, double Value)
{
  if (CanWrite())
  {
    // TRegistry.WriteFloat does this internally
    DoWriteBinaryData(Name, &Value, sizeof(Value));
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteString(const UnicodeString & Name, const UnicodeString & Value)
{
  if (MungeStringValues)
  {
    WriteStringRaw(Name, MungeStr(Value, ForceAnsi, true));
  }
  else
  {
    WriteStringRaw(Name, Value);
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteBinaryData(const UnicodeString & Name, const void * Buffer, int Size)
{
  if (CanWrite())
  {
    DoWriteBinaryData(Name, Buffer, Size);
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteBinaryData(const UnicodeString & Name, const RawByteString & Value)
{
  WriteBinaryData(Name, Value.c_str(), Value.Length());
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteBinaryDataAsString(const UnicodeString & Name, const RawByteString & Value)
{
  // This should be exactly the same operation as calling WriteString in
  // C++Builder 6 (non-Unicode) on Unicode-based OS
  // (conversion is done by Ansi layer of the OS)
  WriteString(Name, AnsiToString(Value));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall THierarchicalStorage::IncludeTrailingBackslash(const UnicodeString & S)
{
  // expanded from ?: as it caused memory leaks
  if (S.IsEmpty())
  {
    return S;
  }
  else
  {
    return ::IncludeTrailingBackslash(S);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall THierarchicalStorage::ExcludeTrailingBackslash(const UnicodeString & S)
{
  // expanded from ?: as it caused memory leaks
  if (S.IsEmpty())
  {
    return S;
  }
  else
  {
    return ::ExcludeTrailingBackslash(S);
  }
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::GetTemporary()
{
  return false;
}
//===========================================================================
__fastcall TRegistryStorage::TRegistryStorage(const UnicodeString & AStorage) :
  THierarchicalStorage(IncludeTrailingBackslash(AStorage))
{
  FWowMode = 0;
  Init();
};
//---------------------------------------------------------------------------
__fastcall TRegistryStorage::TRegistryStorage(const UnicodeString & AStorage, HKEY ARootKey, REGSAM WowMode):
  THierarchicalStorage(IncludeTrailingBackslash(AStorage))
{
  FWowMode = WowMode;
  Init();
  FRegistry->RootKey = ARootKey;
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::Init()
{
  FRegistry = new TRegistry();
  FRegistry->Access = KEY_READ | FWowMode;
}
//---------------------------------------------------------------------------
__fastcall TRegistryStorage::~TRegistryStorage()
{
  delete FRegistry;
};
//---------------------------------------------------------------------------
// Used only in OpenSessionInPutty
bool __fastcall TRegistryStorage::Copy(TRegistryStorage * Storage)
{
  TRegistry * Registry = Storage->FRegistry;
  bool Result = true;
  std::unique_ptr<TStrings> Names(new TStringList());
  Registry->GetValueNames(Names.get());
  std::vector<unsigned char> Buffer(1024, 0);
  int Index = 0;
  while ((Index < Names->Count) && Result)
  {
    UnicodeString Name = MungeStr(Names->Strings[Index], ForceAnsi, false);
    unsigned long Size = Buffer.size();
    unsigned long Type;
    int RegResult;
    do
    {
      RegResult = RegQueryValueEx(Registry->CurrentKey, Name.c_str(), NULL, &Type, &Buffer[0], &Size);
      if (RegResult == ERROR_MORE_DATA)
      {
        Buffer.resize(Size);
      }
    } while (RegResult == ERROR_MORE_DATA);

    Result = (RegResult == ERROR_SUCCESS);
    if (Result)
    {
      RegResult = RegSetValueEx(FRegistry->CurrentKey, Name.c_str(), NULL, Type, &Buffer[0], Size);
      Result = (RegResult == ERROR_SUCCESS);
    }

    ++Index;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TRegistryStorage::GetSource()
{
  return RootKeyToStr(FRegistry->RootKey) + L"\\" + Storage;
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::SetAccessMode(TStorageAccessMode value)
{
  THierarchicalStorage::SetAccessMode(value);
  if (FRegistry)
  {
    switch (AccessMode) {
      case smRead:
        FRegistry->Access = KEY_READ | FWowMode;
        break;

      case smReadWrite:
      default:
        FRegistry->Access = KEY_READ | KEY_WRITE | FWowMode;
        break;
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TRegistryStorage::DoOpenSubKey(const UnicodeString & SubKey, bool CanCreate)
{
  UnicodeString PrevPath;
  bool WasOpened = (FRegistry->CurrentKey != NULL);
  if (WasOpened)
  {
    PrevPath = FRegistry->CurrentPath;
    DebugAssert(SamePaths(PrevPath, Storage + GetCurrentSubKeyMunged()));
    FRegistry->CloseKey();
  }
  UnicodeString K = ExcludeTrailingBackslash(Storage + CurrentSubKey + SubKey);
  bool Result = FRegistry->OpenKey(K, CanCreate);
  if (!Result && WasOpened)
  {
    FRegistry->OpenKey(PrevPath, false);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::DoCloseSubKey()
{
  FRegistry->CloseKey();
  if (!FKeyHistory.empty())
  {
    FRegistry->OpenKey(Storage + GetCurrentSubKeyMunged(), True);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TRegistryStorage::DoDeleteSubKey(const UnicodeString & SubKey)
{
  UnicodeString K;
  if (FKeyHistory.empty())
  {
    K = Storage + CurrentSubKey;
  }
  K += MungeKeyName(SubKey);
  return FRegistry->DeleteKey(K);
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::DoGetSubKeyNames(TStrings * Strings)
{
  FRegistry->GetKeyNames(Strings);
  for (int Index = 0; Index < Strings->Count; Index++)
  {
    Strings->Strings[Index] = UnMungeStr(Strings->Strings[Index]);
  }
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::DoGetValueNames(TStrings * Strings)
{
  FRegistry->GetValueNames(Strings);
}
//---------------------------------------------------------------------------
bool __fastcall TRegistryStorage::DoDeleteValue(const UnicodeString & Name)
{
  return FRegistry->DeleteValue(Name);
}
//---------------------------------------------------------------------------
bool __fastcall TRegistryStorage::DoKeyExists(const UnicodeString & SubKey, bool AForceAnsi)
{
  UnicodeString K = SubKey;
  if (MungingKeyName(K))
  {
    K = MungeStr(K, AForceAnsi, false);
  }
  bool Result = FRegistry->KeyExists(K);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TRegistryStorage::DoValueExists(const UnicodeString & Value, bool DebugUsedArg(AForceAnsi))
{
  // TODO: use AForceAnsi
  bool Result = FRegistry->ValueExists(Value);
  return Result;
}
//---------------------------------------------------------------------------
size_t __fastcall TRegistryStorage::DoBinaryDataSize(const UnicodeString & Name)
{
  size_t Result = FRegistry->GetDataSize(Name);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TRegistryStorage::DoReadBool(const UnicodeString & Name, bool Default)
{
  READ_REGISTRY(ReadBool);
}
//---------------------------------------------------------------------------
TDateTime __fastcall TRegistryStorage::DoReadDateTime(const UnicodeString & Name, TDateTime Default)
{
  // Internally does what would DoReadBinaryData do (like in DoReadInt64)
  READ_REGISTRY(ReadDateTime);
}
//---------------------------------------------------------------------------
double __fastcall TRegistryStorage::DoReadFloat(const UnicodeString & Name, double Default)
{
  // Internally does what would DoReadBinaryData do (like in DoReadInt64)
  READ_REGISTRY(ReadFloat);
}
//---------------------------------------------------------------------------
int __fastcall TRegistryStorage::DoReadInteger(const UnicodeString & Name, int Default, const TIntMapping *)
{
  READ_REGISTRY(ReadInteger);
}
//---------------------------------------------------------------------------
__int64 __fastcall TRegistryStorage::DoReadInt64(const UnicodeString & Name, __int64 Default)
{
  __int64 Result;
  if (DoReadBinaryData(Name, &Result, sizeof(Result)) == 0)
  {
    Result = Default;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TRegistryStorage::DoReadStringRaw(const UnicodeString & Name, const UnicodeString & Default)
{
  READ_REGISTRY(ReadString);
}
//---------------------------------------------------------------------------
size_t __fastcall TRegistryStorage::DoReadBinaryData(const UnicodeString & Name, void * Buffer, size_t Size)
{
  size_t Result;
  if (FRegistry->ValueExists(Name))
  {
    try
    {
      Result = FRegistry->ReadBinaryData(Name, Buffer, Size);
    }
    catch(...)
    {
      Result = 0;
    }
  }
  else
  {
    Result = 0;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::DoWriteBool(const UnicodeString & Name, bool Value)
{
  WRITE_REGISTRY(WriteBool);
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::DoWriteStringRaw(const UnicodeString & Name, const UnicodeString & Value)
{
  WRITE_REGISTRY(WriteString);
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::DoWriteInteger(const UnicodeString & Name, int Value)
{
  WRITE_REGISTRY(WriteInteger);
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::DoWriteInt64(const UnicodeString & Name, __int64 Value)
{
  WriteBinaryData(Name, &Value, sizeof(Value));
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::DoWriteBinaryData(const UnicodeString & Name, const void * Buffer, int Size)
{
  try
  {
    FRegistry->WriteBinaryData(Name, const_cast<void *>(Buffer), Size);
  }
  catch(...)
  {
  }
}
//===========================================================================
__fastcall TCustomIniFileStorage::TCustomIniFileStorage(const UnicodeString & Storage, TCustomIniFile * IniFile) :
  THierarchicalStorage(Storage),
  FIniFile(IniFile),
  FMasterStorageOpenFailures(0),
  FOpeningSubKey(false)
{
}
//---------------------------------------------------------------------------
__fastcall TCustomIniFileStorage::~TCustomIniFileStorage()
{
  delete FIniFile;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomIniFileStorage::GetSource()
{
  return Storage;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomIniFileStorage::GetCurrentSection()
{
  return ExcludeTrailingBackslash(GetCurrentSubKeyMunged());
}
//---------------------------------------------------------------------------
void __fastcall TCustomIniFileStorage::CacheSections()
{
  if (FSections.get() == NULL)
  {
    FSections.reset(new TStringList());
    FIniFile->ReadSections(FSections.get());
    FSections->Sorted = true; // has to set only after reading as ReadSections reset it to false
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomIniFileStorage::ResetCache()
{
  FSections.reset(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomIniFileStorage::SetAccessMode(TStorageAccessMode value)
{
  if (FMasterStorage.get() != NULL)
  {
    FMasterStorage->AccessMode = value;
  }
  THierarchicalStorage::SetAccessMode(value);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomIniFileStorage::DoKeyExistsInternal(const UnicodeString & SubKey)
{
  CacheSections();
  bool Result = false;
  UnicodeString NewKey = ExcludeTrailingBackslash(CurrentSubKey + SubKey);
  if (FSections->Count > 0)
  {
    int Index = -1;
    Result = FSections->Find(NewKey, Index);
    if (!Result &&
        (Index < FSections->Count) &&
        (FSections->Strings[Index].SubString(1, NewKey.Length()+1) == NewKey + L"\\"))
    {
      Result = true;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomIniFileStorage::DoOpenSubKey(const UnicodeString & SubKey, bool CanCreate)
{
  bool Result =
    CanCreate ||
    DoKeyExistsInternal(SubKey);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomIniFileStorage::OpenSubKey(const UnicodeString & Key, bool CanCreate)
{
  bool Result;

  // To cache root access in advance, otherwise we end up calling ourselves, what TAutoFlag does not like
  GetCurrentAccess();

  {
    TAutoFlag Flag(FOpeningSubKey);
    Result = THierarchicalStorage::OpenSubKey(Key, CanCreate);
  }

  if (FMasterStorage.get() != NULL)
  {
    if (FMasterStorageOpenFailures > 0)
    {
      if (Result)
      {
        FMasterStorageOpenFailures++;
      }
    }
    else
    {
      bool MasterResult = FMasterStorage->OpenSubKey(Key, CanCreate);
      if (!Result && MasterResult)
      {
        Result = THierarchicalStorage::OpenSubKey(Key, true);
        DebugAssert(Result);
      }
      else if (Result && !MasterResult)
      {
        FMasterStorageOpenFailures++;
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomIniFileStorage::DoCloseSubKey()
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TCustomIniFileStorage::CloseSubKey()
{
  // What we are called to restore previous key from OpenSubKey,
  // when opening path component fails, the master storage was not involved yet
  if (!FOpeningSubKey && (FMasterStorage.get() != NULL))
  {
    if (FMasterStorageOpenFailures > 0)
    {
      FMasterStorageOpenFailures--;
    }
    else
    {
      FMasterStorage->CloseSubKey();
    }
  }

  THierarchicalStorage::CloseSubKey();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomIniFileStorage::DoDeleteSubKey(const UnicodeString & SubKey)
{
  bool Result = true;
  try
  {
    ResetCache();
    FIniFile->EraseSection(CurrentSubKey + MungeKeyName(SubKey));
  }
  catch (...)
  {
  }
  if (HandleByMasterStorage())
  {
    Result =
      FMasterStorage->CanWrite() &&
      FMasterStorage->DoDeleteSubKey(SubKey);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomIniFileStorage::DoGetSubKeyNames(TStrings * Strings)
{
  Strings->Clear();
  if (HandleByMasterStorage())
  {
    FMasterStorage->GetSubKeyNames(Strings);
  }
  CacheSections();
  for (int i = 0; i < FSections->Count; i++)
  {
    UnicodeString Section = FSections->Strings[i];
    if (AnsiCompareText(CurrentSubKey,
        Section.SubString(1, CurrentSubKey.Length())) == 0)
    {
      UnicodeString SubSection = Section.SubString(CurrentSubKey.Length() + 1,
        Section.Length() - CurrentSubKey.Length());
      int P = SubSection.Pos(L"\\");
      if (P)
      {
        SubSection.SetLength(P - 1);
      }
      if (Strings->IndexOf(SubSection) < 0)
      {
        Strings->Add(UnMungeStr(SubSection));
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomIniFileStorage::DoGetValueNames(TStrings * Strings)
{
  if (HandleByMasterStorage())
  {
    FMasterStorage->GetValueNames(Strings);
  }
  FIniFile->ReadSection(CurrentSection, Strings);
  for (int Index = 0; Index < Strings->Count; Index++)
  {
    UnicodeString S = Strings->Strings[Index];
    S = UnMungeIniName(S);
    Strings->Strings[Index] = S;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomIniFileStorage::DoKeyExists(const UnicodeString & SubKey, bool AForceAnsi)
{
  return
    (HandleByMasterStorage() && FMasterStorage->DoKeyExists(SubKey, AForceAnsi)) ||
    DoKeyExistsInternal(MungeStr(SubKey, AForceAnsi, false));
}
//---------------------------------------------------------------------------
bool __fastcall TCustomIniFileStorage::DoValueExistsInternal(const UnicodeString & Value, bool AForceAnsi)
{
  return FIniFile->ValueExists(CurrentSection, ::MungeIniName(Value, AForceAnsi));
}
//---------------------------------------------------------------------------
bool __fastcall TCustomIniFileStorage::DoValueExists(const UnicodeString & Value, bool AForceAnsi)
{
  return
    (HandleByMasterStorage() && FMasterStorage->DoValueExists(Value, AForceAnsi)) ||
    DoValueExistsInternal(Value, AForceAnsi);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomIniFileStorage::DoDeleteValue(const UnicodeString & Name)
{
  bool Result = true;
  if (HandleByMasterStorage())
  {
    Result = FMasterStorage->DeleteValue(Name);
  }
  ResetCache();
  FIniFile->DeleteKey(CurrentSection, MungeIniName(Name));
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomIniFileStorage::HandleByMasterStorage()
{
  return
    (FMasterStorage.get() != NULL) &&
    (FMasterStorageOpenFailures == 0);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomIniFileStorage::HandleReadByMasterStorage(const UnicodeString & Name)
{
  return HandleByMasterStorage() && !DoValueExistsInternal(Name, ForceAnsi);
}
//---------------------------------------------------------------------------
size_t __fastcall TCustomIniFileStorage::DoBinaryDataSize(const UnicodeString & Name)
{
  if (HandleReadByMasterStorage(Name))
  {
    return FMasterStorage->BinaryDataSize(Name);
  }
  else
  {
    return ReadStringRaw(Name, L"").Length() / 2;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomIniFileStorage::DoReadBool(const UnicodeString & Name, bool Default)
{
  if (HandleReadByMasterStorage(Name))
  {
    return FMasterStorage->ReadBool(Name, Default);
  }
  else
  {
    int IntDefault = int(Default);
    int Int = DoReadIntegerWithMapping(Name, IntDefault, &BoolMapping);
    return (Int != 0);
  }
}
//---------------------------------------------------------------------------
int __fastcall TCustomIniFileStorage::DoReadIntegerWithMapping(const UnicodeString & Name, int Default, const TIntMapping * Mapping)
{
  int Result = 0; // shut up
  bool ReadAsInteger = true;
  UnicodeString MungedName = MungeIniName(Name);
  if (Mapping != NULL) // optimization
  {
    UnicodeString S = FIniFile->ReadString(CurrentSection, MungedName, EmptyStr);
    if (!S.IsEmpty())
    {
      S = S.LowerCase();
      TIntMapping::const_iterator I = Mapping->find(S);
      if (I != Mapping->end())
      {
        Result = I->second;
        ReadAsInteger = false;
      }
    }
  }

  if (ReadAsInteger)
  {
    Result = FIniFile->ReadInteger(CurrentSection, MungedName, Default);
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TCustomIniFileStorage::DoReadInteger(const UnicodeString & Name, int Default, const TIntMapping * Mapping)
{
  int Result;
  if (HandleReadByMasterStorage(Name))
  {
    Result = FMasterStorage->ReadIntegerWithMapping(Name, Default, Mapping);
  }
  else
  {
    Result = DoReadIntegerWithMapping(Name, Default, Mapping);
  }
  return Result;
}
//---------------------------------------------------------------------------
__int64 __fastcall TCustomIniFileStorage::DoReadInt64(const UnicodeString & Name, __int64 Default)
{
  __int64 Result;
  if (HandleReadByMasterStorage(Name))
  {
    Result = FMasterStorage->ReadInt64(Name, Default);
  }
  else
  {
    Result = Default;
    UnicodeString Str;
    Str = ReadStringRaw(Name, L"");
    if (!Str.IsEmpty())
    {
      Result = StrToInt64Def(Str, Default);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
TDateTime __fastcall TCustomIniFileStorage::DoReadDateTime(const UnicodeString & Name, TDateTime Default)
{
  TDateTime Result;
  if (HandleReadByMasterStorage(Name))
  {
    Result = FMasterStorage->ReadDateTime(Name, Default);
  }
  else
  {
    UnicodeString Value = FIniFile->ReadString(CurrentSection, MungeIniName(Name), L"");
    if (Value.IsEmpty())
    {
      Result = Default;
    }
    else
    {
      try
      {
        RawByteString Raw = HexToBytes(Value);
        if (static_cast<size_t>(Raw.Length()) == sizeof(Result))
        {
          memcpy(&Result, Raw.c_str(), sizeof(Result));
        }
        else
        {
          Result = StrToDateTime(Value);
        }
      }
      catch(...)
      {
        Result = Default;
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
double __fastcall TCustomIniFileStorage::DoReadFloat(const UnicodeString & Name, double Default)
{
  double Result;
  if (HandleReadByMasterStorage(Name))
  {
    Result = FMasterStorage->ReadFloat(Name, Default);
  }
  else
  {
    UnicodeString Value = FIniFile->ReadString(CurrentSection, MungeIniName(Name), L"");
    if (Value.IsEmpty())
    {
      Result = Default;
    }
    else
    {
      try
      {
        RawByteString Raw = HexToBytes(Value);
        if (static_cast<size_t>(Raw.Length()) == sizeof(Result))
        {
          memcpy(&Result, Raw.c_str(), sizeof(Result));
        }
        else
        {
          Result = static_cast<double>(StrToFloat(Value));
        }
      }
      catch(...)
      {
        Result = Default;
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomIniFileStorage::DoReadStringRaw(const UnicodeString & Name, const UnicodeString & Default)
{
  UnicodeString Result;
  if (HandleReadByMasterStorage(Name))
  {
    Result = FMasterStorage->ReadStringRaw(Name, Default);
  }
  else
  {
    Result = FIniFile->ReadString(CurrentSection, MungeIniName(Name), Default);
  }
  return Result;
}
//---------------------------------------------------------------------------
size_t __fastcall TCustomIniFileStorage::DoReadBinaryData(const UnicodeString & Name, void * Buffer, size_t Size)
{
  size_t Len;
  if (HandleReadByMasterStorage(Name))
  {
    Size = FMasterStorage->ReadBinaryData(Name, Buffer, Size);
  }
  else
  {
    RawByteString Value = HexToBytes(ReadStringRaw(Name, L""));
    Len = Value.Length();
    if (Size > Len)
    {
      Size = Len;
    }
    DebugAssert(Buffer);
    memcpy(Buffer, Value.c_str(), Size);
  }
  return Size;
}
//---------------------------------------------------------------------------
void __fastcall TCustomIniFileStorage::DoWriteBool(const UnicodeString & Name, bool Value)
{
  if (HandleByMasterStorage())
  {
    FMasterStorage->WriteBool(Name, Value);
  }
  ResetCache();
  FIniFile->WriteBool(CurrentSection, MungeIniName(Name), Value);
}
//---------------------------------------------------------------------------
void __fastcall TCustomIniFileStorage::DoWriteInteger(const UnicodeString & Name, int Value)
{
  if (HandleByMasterStorage())
  {
    FMasterStorage->WriteInteger(Name, Value);
  }
  ResetCache();
  FIniFile->WriteInteger(CurrentSection, MungeIniName(Name), Value);
}
//---------------------------------------------------------------------------
void __fastcall TCustomIniFileStorage::DoWriteInt64(const UnicodeString & Name, __int64 Value)
{
  if (HandleByMasterStorage())
  {
    FMasterStorage->WriteInt64(Name, Value);
  }
  DoWriteStringRawInternal(Name, IntToStr(Value));
}
//---------------------------------------------------------------------------
void __fastcall TCustomIniFileStorage::DoWriteStringRawInternal(const UnicodeString & Name, const UnicodeString & Value)
{
  ResetCache();
  FIniFile->WriteString(CurrentSection, MungeIniName(Name), Value);
}
//---------------------------------------------------------------------------
void __fastcall TCustomIniFileStorage::DoWriteStringRaw(const UnicodeString & Name, const UnicodeString & Value)
{
  if (HandleByMasterStorage())
  {
    FMasterStorage->WriteStringRaw(Name, Value);
  }
  DoWriteStringRawInternal(Name, Value);
}
//---------------------------------------------------------------------------
void __fastcall TCustomIniFileStorage::DoWriteBinaryData(const UnicodeString & Name, const void * Buffer, int Size)
{
  if (HandleByMasterStorage())
  {
    FMasterStorage->WriteBinaryData(Name, Buffer, Size);
  }
  DoWriteStringRawInternal(Name, BytesToHex(RawByteString(static_cast<const char*>(Buffer), Size)));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomIniFileStorage::DoReadRootAccessString()
{
  UnicodeString Result;
  if (OpenSubKey(L"_", false))
  {
    Result = ReadAccessString();
    CloseSubKey();
  }
  else
  {
    Result = DefaultAccessString;
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned int __fastcall TCustomIniFileStorage::GetCurrentAccess()
{
  unsigned int Result;
  // The way THierarchicalStorage::OpenSubKey is implemented, the access will be zero for non-existing keys in
  // configuration overrides => delegating access handling to the master storage (which still should read overriden access keys)
  if (FMasterStorage.get() != NULL)
  {
    Result = FMasterStorage->GetCurrentAccess();
  }
  else
  {
    Result = THierarchicalStorage::GetCurrentAccess();
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomIniFileStorage::HasAccess(unsigned int Access)
{
  bool Result;
  // Avoid the FFakeReadOnlyOpens assertion in THierarchicalStorage::HasAccess, which is not valid for overriden configuration
  if (HandleByMasterStorage())
  {
    Result = FMasterStorage->HasAccess(Access);
  }
  else
  {
    Result = THierarchicalStorage::HasAccess(Access);
  }
  return Result;
}
//===========================================================================
TIniFileStorage * __fastcall TIniFileStorage::CreateFromPath(const UnicodeString & AStorage)
{
  // The code was originally inline in the parent constructor call in the TIniFileStorage::TIniFileStorage [public originally].
  // But if the TMemIniFile constructor throws (e.g. because the INI file is locked), the exception causes access violation.
  // Moving the code to a factory solves this.
  TMemIniFile * IniFile = new TMemIniFile(AStorage);
  return new TIniFileStorage(AStorage, IniFile);
}
//---------------------------------------------------------------------------
TIniFileStorage * __fastcall TIniFileStorage::CreateNul()
{
  // Before we passed "nul", but we have report of a system, where opening "nul" file fails.
  // Passing an empty string is even more efficient, as it does not even try to read the file.
  TMemIniFile * IniFile = new TMemIniFile(EmptyStr);
  return new TIniFileStorage(INI_NUL, IniFile);
}
//---------------------------------------------------------------------------
__fastcall TIniFileStorage::TIniFileStorage(const UnicodeString & AStorage, TCustomIniFile * IniFile):
  TCustomIniFileStorage(AStorage, IniFile)
{
  if (!FIniFile->FileName.IsEmpty())
  {
    FOriginal = new TStringList();
    dynamic_cast<TMemIniFile *>(FIniFile)->GetStrings(FOriginal);
  }
  else
  {
    FOriginal = NULL;
  }
  ApplyOverrides();
}
//---------------------------------------------------------------------------
void __fastcall TIniFileStorage::Flush()
{
  if (FMasterStorage.get() != NULL)
  {
    FMasterStorage->Flush();
  }
  // This does not seem correct, if storage is written after it is flushed (though we probably never do that currently)
  if ((FOriginal != NULL) && !FIniFile->FileName.IsEmpty())
  {
    std::unique_ptr<TStrings> Strings(new TStringList);
    std::unique_ptr<TStrings> Original(FOriginal);
    FOriginal = NULL;

    dynamic_cast<TMemIniFile *>(FIniFile)->GetStrings(Strings.get());
    if (!Strings->Equals(Original.get()))
    {
      int Attr;
      // preserve attributes (especially hidden)
      bool Exists = FileExists(ApiPath(Storage));
      if (Exists)
      {
        Attr = GetFileAttributes(ApiPath(Storage).c_str());
      }
      else
      {
        Attr = FILE_ATTRIBUTE_NORMAL;
      }

      if (FLAGSET(Attr, FILE_ATTRIBUTE_READONLY) && ForceSave)
      {
        SetFileAttributes(ApiPath(Storage).c_str(), Attr & ~FILE_ATTRIBUTE_READONLY);
      }

      HANDLE Handle;
      int Error;
      bool Retry;
      int Trying = 0;
      do
      {
        Error = 0;
        Handle =
          CreateFile(ApiPath(Storage).c_str(), GENERIC_READ | GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, Attr, 0);
        if (Handle == INVALID_HANDLE_VALUE)
        {
          Error = GetLastError();
        }
        Retry = (Error == ERROR_SHARING_VIOLATION) && (Trying < 2000);
        if (Retry)
        {
          const int Step = 100;
          Sleep(Step);
          Trying += Step;
        }
      }
      while (Retry);

      if (Handle == INVALID_HANDLE_VALUE)
      {
        // "access denied" errors upon implicit saves to existing file are ignored
        if (Explicit || !Exists || (Error != ERROR_ACCESS_DENIED))
        {
          throw EOSExtException(FMTLOAD((Exists ? WRITE_ERROR : CREATE_FILE_ERROR), (Storage)));
        }
      }
      else
      {
        try
        {
          std::unique_ptr<TStream> Stream(new TSafeHandleStream(int(Handle)));
          try
          {
            Strings->SaveToStream(Stream.get());
          }
          __finally
          {
            CloseHandle(Handle);
          }
        }
        catch (Exception & E)
        {
          throw ExtException(&E, FMTLOAD(WRITE_ERROR, (Storage)));
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
__fastcall TIniFileStorage::~TIniFileStorage()
{
  Flush();
}
//---------------------------------------------------------------------------
void __fastcall TIniFileStorage::ApplyOverrides()
{
  UnicodeString OverridesKey = IncludeTrailingBackslash(L"Override");

  CacheSections();
  for (int i = 0; i < FSections->Count; i++)
  {
    UnicodeString Section = FSections->Strings[i];

    if (SameText(OverridesKey,
          Section.SubString(1, OverridesKey.Length())))
    {
      UnicodeString SubKey = Section.SubString(OverridesKey.Length() + 1,
        Section.Length() - OverridesKey.Length());

      // this all uses raw names (munged)
      TStrings * Names = new TStringList;
      try
      {
        FIniFile->ReadSection(Section, Names);

        for (int ii = 0; ii < Names->Count; ii++)
        {
          UnicodeString Name = Names->Strings[ii];
          UnicodeString Value = FIniFile->ReadString(Section, Name, L"");
          FIniFile->WriteString(SubKey, Name, Value);
        }
      }
      __finally
      {
        delete Names;
      }

      FIniFile->EraseSection(Section);
      ResetCache();
    }
  }
}
//===========================================================================
enum TWriteMode { wmAllow, wmFail, wmIgnore };
//---------------------------------------------------------------------------
class TOptionsIniFile : public TCustomIniFile
{
public:
  __fastcall TOptionsIniFile(TStrings * Options, TWriteMode WriteMode, const UnicodeString & RootKey);

  virtual UnicodeString __fastcall ReadString(const UnicodeString Section, const UnicodeString Ident, const UnicodeString Default);
  virtual void __fastcall WriteString(const UnicodeString Section, const UnicodeString Ident, const UnicodeString Value);
  virtual void __fastcall ReadSection(const UnicodeString Section, TStrings * Strings);
  virtual void __fastcall ReadSections(TStrings* Strings);
  virtual void __fastcall ReadSectionValues(const UnicodeString Section, TStrings* Strings);
  virtual void __fastcall EraseSection(const UnicodeString Section);
  virtual void __fastcall DeleteKey(const UnicodeString Section, const UnicodeString Ident);
  virtual void __fastcall UpdateFile();
  // Hoisted overload
  void __fastcall ReadSections(const UnicodeString Section, TStrings* Strings);
  // Ntb, we can implement ValueExists more efficiently than the TCustomIniFile.ValueExists

private:
  TStrings * FOptions;
  TWriteMode FWriteMode;
  UnicodeString FRootKey;

  bool __fastcall AllowWrite();
  bool __fastcall AllowSection(const UnicodeString & Section);
  UnicodeString __fastcall FormatKey(const UnicodeString & Section, const UnicodeString & Ident);
};
//---------------------------------------------------------------------------
__fastcall TOptionsIniFile::TOptionsIniFile(TStrings * Options, TWriteMode WriteMode, const UnicodeString & RootKey) :
  TCustomIniFile(UnicodeString())
{
  FOptions = Options;
  FWriteMode = WriteMode;
  FRootKey = RootKey;
  if (!FRootKey.IsEmpty())
  {
    FRootKey += PathDelim;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TOptionsIniFile::AllowWrite()
{
  switch (FWriteMode)
  {
    case wmAllow:
      return true;

    case wmFail:
      NotImplemented();
      UNREACHABLE_AFTER_NORETURN(return false);

    case wmIgnore:
      return false;

    default:
      DebugFail();
      return false;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TOptionsIniFile::AllowSection(const UnicodeString & Section)
{
  UnicodeString Name = Section;
  if (!Name.IsEmpty())
  {
    Name += PathDelim;
  }
  bool Result = SameText(Name.SubString(1, FRootKey.Length()), FRootKey);
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TOptionsIniFile::FormatKey(const UnicodeString & Section, const UnicodeString & Ident)
{
  UnicodeString Result = Section;
  if (!Result.IsEmpty())
  {
    Result += PathDelim;
  }
  Result += Ident; // Can be empty, when called from a constructor, AllowSection or ReadSection
  if (DebugAlwaysTrue(AllowSection(Section)))
  {
    Result.Delete(1, FRootKey.Length());
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TOptionsIniFile::ReadString(const UnicodeString Section, const UnicodeString Ident, const UnicodeString Default)
{
  UnicodeString Value;
  if (!AllowSection(Section))
  {
    Value = Default;
  }
  else
  {
    UnicodeString Name = FormatKey(Section, Ident);

    int Index = FOptions->IndexOfName(Name);
    if (Index >= 0)
    {
      Value = FOptions->ValueFromIndex[Index];
    }
    else
    {
      Value = Default;
    }
  }
  return Value;
}
//---------------------------------------------------------------------------
void __fastcall TOptionsIniFile::WriteString(const UnicodeString Section, const UnicodeString Ident, const UnicodeString Value)
{
  if (AllowWrite() &&
      DebugAlwaysTrue(AllowSection(Section)))
  {
    UnicodeString Name = FormatKey(Section, Ident);
    SetStringValueEvenIfEmpty(FOptions, Name, Value);
  }
}
//---------------------------------------------------------------------------
void __fastcall TOptionsIniFile::ReadSection(const UnicodeString Section, TStrings * Strings)
{

  if (AllowSection(Section))
  {
    UnicodeString SectionPrefix = FormatKey(Section, UnicodeString());

    Strings->BeginUpdate();
    try
    {
      for (int Index = 0; Index < FOptions->Count; Index++)
      {
        UnicodeString Name = FOptions->Names[Index];
        if (SameText(Name.SubString(1, SectionPrefix.Length()), SectionPrefix) &&
            (LastDelimiter(PathDelim, Name) <= SectionPrefix.Length()))
        {
          Strings->Add(Name.SubString(SectionPrefix.Length() + 1, Name.Length() - SectionPrefix.Length()));
        }
      }
    }
    __finally
    {
      Strings->EndUpdate();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TOptionsIniFile::ReadSections(TStrings * Strings)
{
  std::unique_ptr<TStringList> Sections(CreateSortedStringList());

  for (int Index = 0; Index < FOptions->Count; Index++)
  {
    UnicodeString Name = FOptions->Names[Index];
    int P = LastDelimiter(PathDelim, Name);
    if (P > 0)
    {
      UnicodeString Section = Name.SubString(1, P - 1);
      if (Sections->IndexOf(Section) < 0)
      {
        Sections->Add(Section);
      }
    }
  }

  for (int Index = 0; Index < Sections->Count; Index++)
  {
    Strings->Add(FRootKey + Sections->Strings[Index]);
  }
}
//---------------------------------------------------------------------------
void __fastcall TOptionsIniFile::ReadSectionValues(const UnicodeString Section, TStrings * /*Strings*/)
{
  DebugUsedParam(Section);
  NotImplemented();
}
//---------------------------------------------------------------------------
void __fastcall TOptionsIniFile::EraseSection(const UnicodeString Section)
{
  DebugUsedParam(Section);
  if (AllowWrite())
  {
    NotImplemented();
  }
}
//---------------------------------------------------------------------------
void __fastcall TOptionsIniFile::DeleteKey(const UnicodeString Section, const UnicodeString Ident)
{
  if (AllowWrite() &&
      DebugAlwaysTrue(AllowSection(Section)))
  {
    UnicodeString Name = FormatKey(Section, Ident);

    int Index = FOptions->IndexOfName(Name);
    if (Index >= 0)
    {
      FOptions->Delete(Index);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TOptionsIniFile::UpdateFile()
{
  if (AllowWrite())
  {
    // noop
  }
}
//---------------------------------------------------------------------------
void __fastcall TOptionsIniFile::ReadSections(const UnicodeString /*Section*/, TStrings * /*Strings*/)
{
  NotImplemented();
}
//===========================================================================
__fastcall TOptionsStorage::TOptionsStorage(TStrings * Options, bool AllowWrite):
  TCustomIniFileStorage(
    UnicodeString(L"Command-line options"),
    new TOptionsIniFile(Options, (AllowWrite ? wmAllow : wmFail), UnicodeString()))
{
}
//---------------------------------------------------------------------------
__fastcall TOptionsStorage::TOptionsStorage(TStrings * Options, const UnicodeString & RootKey, THierarchicalStorage * MasterStorage) :
  TCustomIniFileStorage(
    UnicodeString(L"Command-line options overriding " + MasterStorage->Source),
    new TOptionsIniFile(Options, wmIgnore, RootKey))
{
  FMasterStorage.reset(MasterStorage);
}
//---------------------------------------------------------------------------
bool __fastcall TOptionsStorage::GetTemporary()
{
  return true;
}
