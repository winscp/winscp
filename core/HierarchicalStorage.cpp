//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "Exceptions.h"
#include "PuttyIntf.h"
#include "HierarchicalStorage.h"
#include <TextsCore.h>
#include <vector>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define READ_REGISTRY(Method) \
  if (FRegistry->ValueExists(Name)) \
  try { return FRegistry->Method(Name); } catch(...) { FFailed++; return Default; } \
  else return Default;
#define WRITE_REGISTRY(Method) \
  try { FRegistry->Method(Name, Value); } catch(...) { FFailed++; }
//---------------------------------------------------------------------------
AnsiString __fastcall MungeStr(const AnsiString Str)
{
  AnsiString Result;
  Result.SetLength(Str.Length() * 3 + 1);
  putty_mungestr(Str.c_str(), Result.c_str());
  PackStr(Result);
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall UnMungeStr(const AnsiString Str)
{
  AnsiString Result;
  Result.SetLength(Str.Length() * 3 + 1);
  putty_unmungestr(Str.c_str(), Result.c_str(), Result.Length());
  PackStr(Result);
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall PuttyMungeStr(const AnsiString Str)
{
  return MungeStr(Str);
}
//---------------------------------------------------------------------------
AnsiString __fastcall MungeIniName(const AnsiString Str)
{
  int P = Str.Pos("=");
  // make this fast for now
  if (P > 0)
  {
    return StringReplace(Str, "=", "%3D", TReplaceFlags() << rfReplaceAll);
  }
  else
  {
    return Str;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall UnMungeIniName(const AnsiString Str)
{
  int P = Str.Pos("%3D");
  // make this fast for now
  if (P > 0)
  {
    return StringReplace(Str, "%3D", "=", TReplaceFlags() << rfReplaceAll);
  }
  else
  {
    return Str;
  }
}
//===========================================================================
__fastcall THierarchicalStorage::THierarchicalStorage(const AnsiString AStorage)
{
  FStorage = AStorage;
  FKeyHistory = new TStringList();
  AccessMode = smRead;
  Explicit = false;
  MungeStringValues = true;
}
//---------------------------------------------------------------------------
__fastcall THierarchicalStorage::~THierarchicalStorage()
{
  delete FKeyHistory;
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::SetAccessMode(TStorageAccessMode value)
{
  FAccessMode = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall THierarchicalStorage::GetCurrentSubKeyMunged()
{
  if (FKeyHistory->Count) return FKeyHistory->Strings[FKeyHistory->Count-1];
    else return "";
}
//---------------------------------------------------------------------------
AnsiString __fastcall THierarchicalStorage::GetCurrentSubKey()
{
  return UnMungeStr(GetCurrentSubKeyMunged());
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::OpenRootKey(bool CanCreate)
{
  return OpenSubKey("", CanCreate);
}
//---------------------------------------------------------------------------
AnsiString __fastcall THierarchicalStorage::MungeSubKey(AnsiString Key, bool Path)
{
  AnsiString Result;
  if (Path)
  {
    assert(Key.IsEmpty() || (Key[Key.Length()] != '\\'));
    while (!Key.IsEmpty())
    {
      if (!Result.IsEmpty())
      {
        Result += '\\';
      }
      Result += MungeStr(CutToChar(Key, '\\', false));
    }
  }
  else
  {
    Result = MungeStr(Key);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::OpenSubKey(const AnsiString SubKey, bool /*CanCreate*/, bool Path)
{
  FKeyHistory->Add(IncludeTrailingBackslash(CurrentSubKey+MungeSubKey(SubKey, Path)));
  return true;
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::CloseSubKey()
{
  if (FKeyHistory->Count == 0) throw Exception("");
    else FKeyHistory->Delete(FKeyHistory->Count-1);
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::ClearSubKeys()
{
  TStringList *SubKeys = new TStringList();
  try
  {
    GetSubKeyNames(SubKeys);
    for (int Index = 0; Index < SubKeys->Count; Index++)
    {
      RecursiveDeleteSubKey(SubKeys->Strings[Index]);
    }
  }
  __finally
  {
    delete SubKeys;
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::RecursiveDeleteSubKey(const AnsiString Key)
{
  if (OpenSubKey(Key, false))
  {
    ClearSubKeys();
    CloseSubKey();
  }
  DeleteSubKey(Key);
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::HasSubKeys()
{
  bool Result;
  TStrings * SubKeys = new TStringList();
  try
  {
    GetSubKeyNames(SubKeys);
    Result = (SubKeys->Count > 0);
  }
  __finally
  {
    delete SubKeys;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall THierarchicalStorage::HasSubKey(const AnsiString SubKey)
{
  bool Result = OpenSubKey(SubKey, false);
  if (Result)
  {
    CloseSubKey();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::ReadValues(Classes::TStrings* Strings,
  bool MaintainKeys)
{
  TStrings * Names = new TStringList();
  try
  {
    GetValueNames(Names);
    for (int Index = 0; Index < Names->Count; Index++)
    {
      if (MaintainKeys)
      {
        Strings->Add(FORMAT("%s=%s", (Names->Strings[Index],
          ReadString(Names->Strings[Index], ""))));
      }
      else
      {
        Strings->Add(ReadString(Names->Strings[Index], ""));
      }
    }
  }
  __finally
  {
    delete Names;
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::ClearValues()
{
  TStrings * Names = new TStringList();
  try
  {
    GetValueNames(Names);
    for (int Index = 0; Index < Names->Count; Index++)
    {
      DeleteValue(Names->Strings[Index]);
    }
  }
  __finally
  {
    delete Names;
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteValues(Classes::TStrings * Strings,
  bool MaintainKeys)
{
  ClearValues();

  if (Strings)
  {
    for (int Index = 0; Index < Strings->Count; Index++)
    {
      if (MaintainKeys)
      {
        assert(Strings->Strings[Index].Pos("=") > 1);
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
AnsiString __fastcall THierarchicalStorage::ReadString(const AnsiString Name, const AnsiString Default)
{
  AnsiString Result;
  if (MungeStringValues)
  {
    Result = UnMungeStr(ReadStringRaw(Name, MungeStr(Default)));
  }
  else
  {
    Result = ReadStringRaw(Name, Default);
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall THierarchicalStorage::ReadBinaryData(const AnsiString Name)
{
  int Size = BinaryDataSize(Name);
  AnsiString Value;
  Value.SetLength(Size);
  ReadBinaryData(Name, Value.c_str(), Size);
  return Value;
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteString(const AnsiString Name, const AnsiString Value)
{
  if (MungeStringValues)
  {
    WriteStringRaw(Name, MungeStr(Value));
  }
  else
  {
    WriteStringRaw(Name, Value);
  }
}
//---------------------------------------------------------------------------
void __fastcall THierarchicalStorage::WriteBinaryData(const AnsiString Name,
  const AnsiString Value)
{
  WriteBinaryData(Name, Value.c_str(), Value.Length());
}
//---------------------------------------------------------------------------
AnsiString __fastcall THierarchicalStorage::IncludeTrailingBackslash(const AnsiString & S)
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
AnsiString __fastcall THierarchicalStorage::ExcludeTrailingBackslash(const AnsiString & S)
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
//===========================================================================
__fastcall TRegistryStorage::TRegistryStorage(const AnsiString AStorage):
  THierarchicalStorage(IncludeTrailingBackslash(AStorage))
{
  Init();
};
//---------------------------------------------------------------------------
__fastcall TRegistryStorage::TRegistryStorage(const AnsiString AStorage, HKEY ARootKey):
  THierarchicalStorage(IncludeTrailingBackslash(AStorage))
{
  Init();
  FRegistry->RootKey = ARootKey;
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::Init()
{
  FFailed = 0;
  FRegistry = new TRegistry();
  FRegistry->Access = KEY_READ;
}
//---------------------------------------------------------------------------
__fastcall TRegistryStorage::~TRegistryStorage()
{
  delete FRegistry;
};
//---------------------------------------------------------------------------
bool __fastcall TRegistryStorage::Copy(TRegistryStorage * Storage)
{
  TRegistry * Registry = Storage->FRegistry;
  bool Result = true;
  TStrings * Names = new TStringList();
  try
  {
    Registry->GetValueNames(Names);
    std::vector<unsigned char> Buffer(1024, 0);
    int Index = 0;
    while ((Index < Names->Count) && Result)
    {
      AnsiString Name = MungeStr(Names->Strings[Index]);
      unsigned long Size = Buffer.size();
      unsigned long Type;
      int RegResult;
      do
      {
        RegResult = RegQueryValueEx(Registry->CurrentKey, Name.c_str(), NULL,
          &Type, &Buffer[0], &Size);
        if (Result == ERROR_MORE_DATA)
        {
          Buffer.resize(Size);
        }
      } while (RegResult == ERROR_MORE_DATA);

      Result = (RegResult == ERROR_SUCCESS);
      if (Result)
      {
        RegResult = RegSetValueEx(FRegistry->CurrentKey, Name.c_str(), NULL, Type,
          &Buffer[0], Size);
        Result = (RegResult == ERROR_SUCCESS);
      }

      ++Index;
    }
  }
  __finally
  {
    delete Names;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::SetAccessMode(TStorageAccessMode value)
{
  THierarchicalStorage::SetAccessMode(value);
  if (FRegistry)
  {
    switch (AccessMode) {
      case smRead:
        FRegistry->Access = KEY_READ;
        break;

      case smReadWrite:
      default:
        FRegistry->Access = KEY_READ | KEY_WRITE;
        break;
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TRegistryStorage::OpenSubKey(const AnsiString SubKey, bool CanCreate, bool Path)
{
  bool Result;
  if (FKeyHistory->Count > 0) FRegistry->CloseKey();
  AnsiString K = ExcludeTrailingBackslash(Storage + CurrentSubKey + MungeSubKey(SubKey, Path));
  Result = FRegistry->OpenKey(K, CanCreate);
  if (Result) Result = THierarchicalStorage::OpenSubKey(SubKey, CanCreate, Path);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::CloseSubKey()
{
  FRegistry->CloseKey();
  THierarchicalStorage::CloseSubKey();
  if (FKeyHistory->Count)
  {
    FRegistry->OpenKey(Storage + CurrentSubKey, True);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TRegistryStorage::DeleteSubKey(const AnsiString SubKey)
{
  AnsiString K;
  if (FKeyHistory->Count == 0) K = Storage + CurrentSubKey;
  K += MungeStr(SubKey);
  return FRegistry->DeleteKey(K);
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::GetSubKeyNames(Classes::TStrings* Strings)
{
  FRegistry->GetKeyNames(Strings);
  for (int Index = 0; Index < Strings->Count; Index++)
  {
    Strings->Strings[Index] = UnMungeStr(Strings->Strings[Index]);
  }
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::GetValueNames(Classes::TStrings* Strings)
{
  FRegistry->GetValueNames(Strings);
}
//---------------------------------------------------------------------------
bool __fastcall TRegistryStorage::DeleteValue(const AnsiString Name)
{
  return FRegistry->DeleteValue(Name);
}
//---------------------------------------------------------------------------
bool __fastcall TRegistryStorage::KeyExists(const AnsiString SubKey)
{
  AnsiString K = MungeStr(SubKey);
  bool Result = FRegistry->KeyExists(K);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TRegistryStorage::ValueExists(const AnsiString Value)
{
  bool Result = FRegistry->ValueExists(Value);
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TRegistryStorage::BinaryDataSize(const AnsiString Name)
{
  int Result = FRegistry->GetDataSize(Name);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TRegistryStorage::ReadBool(const AnsiString Name, bool Default)
{
  READ_REGISTRY(ReadBool);
}
//---------------------------------------------------------------------------
TDateTime __fastcall TRegistryStorage::ReadDateTime(const AnsiString Name, TDateTime Default)
{
  READ_REGISTRY(ReadDateTime);
}
//---------------------------------------------------------------------------
double __fastcall TRegistryStorage::ReadFloat(const AnsiString Name, double Default)
{
  READ_REGISTRY(ReadFloat);
}
//---------------------------------------------------------------------------
int __fastcall TRegistryStorage::ReadInteger(const AnsiString Name, int Default)
{
  READ_REGISTRY(ReadInteger);
}
//---------------------------------------------------------------------------
__int64 __fastcall TRegistryStorage::ReadInt64(const AnsiString Name, __int64 Default)
{
  __int64 Result = Default;
  if (FRegistry->ValueExists(Name))
  {
    try
    {
      FRegistry->ReadBinaryData(Name, &Result, sizeof(Result));
    }
    catch(...)
    {
      FFailed++;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRegistryStorage::ReadStringRaw(const AnsiString Name, const AnsiString Default)
{
  READ_REGISTRY(ReadString);
}
//---------------------------------------------------------------------------
int __fastcall TRegistryStorage::ReadBinaryData(const AnsiString Name,
  void * Buffer, int Size)
{
  int Result;
  if (FRegistry->ValueExists(Name))
  {
    try
    {
      Result = FRegistry->ReadBinaryData(Name, Buffer, Size);
    }
    catch(...)
    {
      Result = 0;
      FFailed++;
    }
  }
  else
  {
    Result = 0;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::WriteBool(const AnsiString Name, bool Value)
{
  WRITE_REGISTRY(WriteBool);
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::WriteDateTime(const AnsiString Name, TDateTime Value)
{
  WRITE_REGISTRY(WriteDateTime);
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::WriteFloat(const AnsiString Name, double Value)
{
  WRITE_REGISTRY(WriteFloat);
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::WriteStringRaw(const AnsiString Name, const AnsiString Value)
{
  WRITE_REGISTRY(WriteString);
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::WriteInteger(const AnsiString Name, int Value)
{
  WRITE_REGISTRY(WriteInteger);
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::WriteInt64(const AnsiString Name, __int64 Value)
{
  try
  {
    FRegistry->WriteBinaryData(Name, &Value, sizeof(Value));
  }
  catch(...)
  {
    FFailed++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRegistryStorage::WriteBinaryData(const AnsiString Name,
  const void * Buffer, int Size)
{
  try
  {
    FRegistry->WriteBinaryData(Name, const_cast<void *>(Buffer), Size);
  }
  catch(...)
  {
    FFailed++;
  }
}
//---------------------------------------------------------------------------
int __fastcall TRegistryStorage::GetFailed()
{
  int Result = FFailed;
  FFailed = 0;
  return Result;
}
//===========================================================================
__fastcall TIniFileStorage::TIniFileStorage(const AnsiString AStorage):
  THierarchicalStorage(AStorage)
{
  FIniFile = new TMemIniFile(Storage);
  FOriginal = new TStringList();
  FIniFile->GetStrings(FOriginal);
  ApplyOverrides();
}
//---------------------------------------------------------------------------
__fastcall TIniFileStorage::~TIniFileStorage()
{
  TStrings * Strings = new TStringList;
  try
  {
    FIniFile->GetStrings(Strings);
    if (!Strings->Equals(FOriginal))
    {
      int Attr;
      // preserve attributes (especially hidden)
      if (FileExists(Storage))
      {
        Attr = GetFileAttributes(Storage.c_str());
      }
      else
      {
        Attr = FILE_ATTRIBUTE_NORMAL;
      }

      HANDLE Handle = CreateFile(Storage.c_str(), GENERIC_READ | GENERIC_WRITE,
        0, NULL, CREATE_ALWAYS, Attr, 0);

      if (Handle == INVALID_HANDLE_VALUE)
      {
        // "access denied" errors upon implicit saves are ignored
        if (Explicit || (GetLastError() != ERROR_ACCESS_DENIED))
        {
          try
          {
            RaiseLastOSError();
          }
          catch(Exception & E)
          {
            throw ExtException(&E, FMTLOAD(CREATE_FILE_ERROR, (Storage)));
          }
        }
      }
      else
      {
        TStream * Stream = new THandleStream(int(Handle));
        try
        {
          Strings->SaveToStream(Stream);
        }
        __finally
        {
          CloseHandle(Handle);
          delete Stream;
        }
      }
    }
  }
  __finally
  {
    delete FOriginal;
    delete Strings;
    delete FIniFile;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TIniFileStorage::GetCurrentSection()
{
  return ExcludeTrailingBackslash(GetCurrentSubKeyMunged());
}
//---------------------------------------------------------------------------
bool __fastcall TIniFileStorage::OpenSubKey(const AnsiString SubKey, bool CanCreate, bool Path)
{
  bool Result = CanCreate;

  if (!Result)
  {
    TStringList * Sections = new TStringList();
    try
    {
      Sections->Sorted = true;
      FIniFile->ReadSections(Sections);
      AnsiString NewKey = ExcludeTrailingBackslash(CurrentSubKey+MungeSubKey(SubKey, Path));
      int Index = -1;
      if (Sections->Count)
      {
        Result = Sections->Find(NewKey, Index);
        if (!Result && Index < Sections->Count &&
            Sections->Strings[Index].SubString(1, NewKey.Length()+1) == NewKey + "\\")
        {
          Result = true;
        }
      }
    }
    __finally
    {
      delete Sections;
    }
  }

  if (Result)
  {
    Result = THierarchicalStorage::OpenSubKey(SubKey, CanCreate, Path);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TIniFileStorage::DeleteSubKey(const AnsiString SubKey)
{
  bool Result;
  try
  {
    FIniFile->EraseSection(CurrentSubKey + MungeStr(SubKey));
    Result = true;
  }
  catch (...)
  {
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TIniFileStorage::GetSubKeyNames(Classes::TStrings* Strings)
{
  TStrings * Sections = new TStringList();
  try
  {
    Strings->Clear();
    FIniFile->ReadSections(Sections);
    for (int i = 0; i < Sections->Count; i++)
    {
      AnsiString Section = Sections->Strings[i];
      if (AnsiCompareText(CurrentSubKey,
          Section.SubString(1, CurrentSubKey.Length())) == 0)
      {
        AnsiString SubSection = Section.SubString(CurrentSubKey.Length() + 1,
          Section.Length() - CurrentSubKey.Length());
        int P = SubSection.Pos("\\");
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
  __finally
  {
    delete Sections;
  }
}
//---------------------------------------------------------------------------
void __fastcall TIniFileStorage::GetValueNames(Classes::TStrings* Strings)
{
  FIniFile->ReadSection(CurrentSection, Strings);
  for (int Index = 0; Index < Strings->Count; Index++)
  {
    Strings->Strings[Index] = UnMungeIniName(Strings->Strings[Index]);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TIniFileStorage::KeyExists(const AnsiString SubKey)
{
  return FIniFile->SectionExists(CurrentSubKey + MungeStr(SubKey));
}
//---------------------------------------------------------------------------
bool __fastcall TIniFileStorage::ValueExists(const AnsiString Value)
{
  return FIniFile->ValueExists(CurrentSection, MungeIniName(Value));
}
//---------------------------------------------------------------------------
bool __fastcall TIniFileStorage::DeleteValue(const AnsiString Name)
{
  FIniFile->DeleteKey(CurrentSection, MungeIniName(Name));
  return true;
}
//---------------------------------------------------------------------------
int __fastcall TIniFileStorage::BinaryDataSize(const AnsiString Name)
{
  return ReadStringRaw(Name, "").Length() / 2;
}
//---------------------------------------------------------------------------
void __fastcall TIniFileStorage::ApplyOverrides()
{
  AnsiString OverridesKey = IncludeTrailingBackslash("Override");

  TStrings * Sections = new TStringList();
  try
  {
    Sections->Clear();
    FIniFile->ReadSections(Sections);
    for (int i = 0; i < Sections->Count; i++)
    {
      AnsiString Section = Sections->Strings[i];

      if (AnsiSameText(OverridesKey,
            Section.SubString(1, OverridesKey.Length())))
      {
        AnsiString SubKey = Section.SubString(OverridesKey.Length() + 1,
          Section.Length() - OverridesKey.Length());

        // this all uses raw names (munged)
        TStrings * Names = new TStringList;
        try
        {
          FIniFile->ReadSection(Section, Names);

          for (int ii = 0; ii < Names->Count; ii++)
          {
            AnsiString Name = Names->Strings[ii];
            AnsiString Value = FIniFile->ReadString(Section, Name, "");
            FIniFile->WriteString(SubKey, Name, Value);
          }
        }
        __finally
        {
          delete Names;
        }

        FIniFile->EraseSection(Section);
      }
    }
  }
  __finally
  {
    delete Sections;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TIniFileStorage::ReadBool(const AnsiString Name, bool Default)
{
  return FIniFile->ReadBool(CurrentSection, MungeIniName(Name), Default);
}
//---------------------------------------------------------------------------
int __fastcall TIniFileStorage::ReadInteger(const AnsiString Name, int Default)
{
  int Result = FIniFile->ReadInteger(CurrentSection, MungeIniName(Name), Default);
  return Result;
}
//---------------------------------------------------------------------------
__int64 __fastcall TIniFileStorage::ReadInt64(const AnsiString Name, __int64 Default)
{
  __int64 Result = Default;
  AnsiString Str;
  Str = ReadStringRaw(Name, "");
  if (!Str.IsEmpty())
  {
    Result = StrToInt64Def(Str, Default);
  }
  return Result;
}
//---------------------------------------------------------------------------
TDateTime __fastcall TIniFileStorage::ReadDateTime(const AnsiString Name, TDateTime Default)
{
  TDateTime Result;
  AnsiString Value = FIniFile->ReadString(CurrentSection, MungeIniName(Name), "");
  if (Value.IsEmpty())
  {
    Result = Default;
  }
  else
  {
    try
    {
      AnsiString Raw = HexToStr(Value);
      if (Raw.Length() == sizeof(Result))
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

  return Result;
}
//---------------------------------------------------------------------------
double __fastcall TIniFileStorage::ReadFloat(const AnsiString Name, double Default)
{
  double Result;
  AnsiString Value = FIniFile->ReadString(CurrentSection, MungeIniName(Name), "");
  if (Value.IsEmpty())
  {
    Result = Default;
  }
  else
  {
    try
    {
      AnsiString Raw = HexToStr(Value);
      if (Raw.Length() == sizeof(Result))
      {
        memcpy(&Result, Raw.c_str(), sizeof(Result));
      }
      else
      {
        Result = StrToFloat(Value);
      }
    }
    catch(...)
    {
      Result = Default;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TIniFileStorage::ReadStringRaw(const AnsiString Name, AnsiString Default)
{
  AnsiString Result = FIniFile->ReadString(CurrentSection, MungeIniName(Name), Default);
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TIniFileStorage::ReadBinaryData(const AnsiString Name,
  void * Buffer, int Size)
{
  AnsiString Value = HexToStr(ReadStringRaw(Name, ""));
  int Len = Value.Length();
  if (Size > Len)
  {
    Size = Len;
  }
  assert(Buffer);
  memcpy(Buffer, Value.c_str(), Size);
  return Size;
}
//---------------------------------------------------------------------------
void __fastcall TIniFileStorage::WriteBool(const AnsiString Name, bool Value)
{
  FIniFile->WriteBool(CurrentSection, MungeIniName(Name), Value);
}
//---------------------------------------------------------------------------
void __fastcall TIniFileStorage::WriteInteger(const AnsiString Name, int Value)
{
  FIniFile->WriteInteger(CurrentSection, MungeIniName(Name), Value);
}
//---------------------------------------------------------------------------
void __fastcall TIniFileStorage::WriteInt64(const AnsiString Name, __int64 Value)
{
  WriteStringRaw(Name, IntToStr(Value));
}
//---------------------------------------------------------------------------
void __fastcall TIniFileStorage::WriteDateTime(const AnsiString Name, TDateTime Value)
{
  WriteBinaryData(Name, &Value, sizeof(Value));
}
//---------------------------------------------------------------------------
void __fastcall TIniFileStorage::WriteFloat(const AnsiString Name, double Value)
{
  WriteBinaryData(Name, &Value, sizeof(Value));
}
//---------------------------------------------------------------------------
void __fastcall TIniFileStorage::WriteStringRaw(const AnsiString Name, const AnsiString Value)
{
  FIniFile->WriteString(CurrentSection, MungeIniName(Name), Value);
}
//---------------------------------------------------------------------------
void __fastcall TIniFileStorage::WriteBinaryData(const AnsiString Name,
  const void * Buffer, int Size)
{
  WriteStringRaw(Name, StrToHex(AnsiString(static_cast<const char*>(Buffer), Size)));
}
