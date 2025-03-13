//---------------------------------------------------------------------------
#ifndef HierarchicalStorageH
#define HierarchicalStorageH

#include <Registry.hpp>
#include <memory>
#include <map>
//---------------------------------------------------------------------------
enum TStorage { stDetect, stRegistry, stIniFile, stNul };
enum TStorageAccessMode { smRead, smReadWrite };
typedef std::map<UnicodeString, int> TIntMapping;
//---------------------------------------------------------------------------
class THierarchicalStorage
{
friend class TCustomIniFileStorage;

public:
  __fastcall THierarchicalStorage(const UnicodeString & AStorage);
  virtual __fastcall ~THierarchicalStorage();
  void __fastcall ConfigureForPutty();
  bool __fastcall OpenRootKey(bool CanCreate);
  virtual bool __fastcall OpenSubKey(const UnicodeString & SubKey, bool CanCreate);
  virtual void __fastcall CloseSubKey();
  void __fastcall CloseAll();
  bool __fastcall OpenSubKeyPath(const UnicodeString & KeyPath, bool CanCreate);
  void __fastcall CloseSubKeyPath();
  void __fastcall GetSubKeyNames(TStrings * Strings);
  void __fastcall GetValueNames(TStrings * Strings);
  bool __fastcall HasSubKeys();
  bool __fastcall KeyExists(const UnicodeString & SubKey);
  bool __fastcall ValueExists(const UnicodeString & Value);
  bool DeleteSubKey(const UnicodeString & Key, bool Recursive = false);
  bool RecursiveDeleteSubKey(const UnicodeString & Key);
  virtual void __fastcall ClearSubKeys();
  virtual void __fastcall ReadValues(TStrings* Strings, bool MaintainKeys = false);
  virtual void __fastcall WriteValues(TStrings* Strings, bool MaintainKeys = false);
  virtual void __fastcall ClearValues();
  bool __fastcall DeleteValue(const UnicodeString & Name);

  bool __fastcall ReadBool(const UnicodeString & Name, bool Default);
  template<typename T>
  T __fastcall ReadEnum(
    const UnicodeString & Name, const T & Default, const TIntMapping & Mapping = TIntMapping());
  int __fastcall ReadInteger(const UnicodeString & Name, int Default);
  __int64 __fastcall ReadInt64(const UnicodeString & Name, __int64 Default);
  TDateTime __fastcall ReadDateTime(const UnicodeString & Name, TDateTime Default);
  double __fastcall ReadFloat(const UnicodeString & Name, double Default);
  UnicodeString __fastcall ReadStringRaw(const UnicodeString & Name, const UnicodeString & Default);
  size_t __fastcall ReadBinaryData(const UnicodeString & Name, void * Buffer, size_t Size);

  virtual UnicodeString __fastcall ReadString(const UnicodeString & Name, const UnicodeString & Default);
  RawByteString __fastcall ReadBinaryData(const UnicodeString & Name);
  RawByteString __fastcall ReadStringAsBinaryData(const UnicodeString & Name, const RawByteString & Default);

  void __fastcall WriteBool(const UnicodeString & Name, bool Value);
  void __fastcall WriteStringRaw(const UnicodeString & Name, const UnicodeString & Value);
  void __fastcall WriteInteger(const UnicodeString & Name, int Value);
  void __fastcall WriteInt64(const UnicodeString & Name, __int64 Value);
  void __fastcall WriteDateTime(const UnicodeString & Name, TDateTime Value);
  void __fastcall WriteFloat(const UnicodeString & Name, double Value);
  void __fastcall WriteBinaryData(const UnicodeString & Name, const void * Buffer, int Size);
  void __fastcall WriteString(const UnicodeString & Name, const UnicodeString & Value);
  void __fastcall WriteBinaryData(const UnicodeString & Name, const RawByteString & Value);
  void __fastcall WriteBinaryDataAsString(const UnicodeString & Name, const RawByteString & Value);

  virtual void __fastcall Flush();

  __property UnicodeString Storage = { read = FStorage };
  __property UnicodeString CurrentSubKey = { read = GetCurrentSubKey };
  __property TStorageAccessMode AccessMode = { read = FAccessMode, write = SetAccessMode };
  __property bool Explicit = { read = FExplicit, write = FExplicit };
  __property bool ForceSave = { read = FForceSave, write = FForceSave };
  __property bool MungeStringValues = { read = FMungeStringValues, write = FMungeStringValues };
  __property UnicodeString Source = { read = GetSource };
  __property bool Temporary = { read = GetTemporary };
  __property UnicodeString UnmungedRoot = { read = FUnmungedRoot, write = FUnmungedRoot };

protected:
  enum THierarchicalStorageAccess { hsaRead = 0x01, hsaWrite = 0x02 };
  struct TKeyEntry
  {
    UnicodeString Key;
    int Levels;
    unsigned int Access;
  };

  UnicodeString FStorage;
  std::vector<TKeyEntry> FKeyHistory;
  TStorageAccessMode FAccessMode;
  bool FExplicit;
  bool FForceSave;
  bool FMungeStringValues;
  bool FForceAnsi;
  int FFakeReadOnlyOpens;
  int FRootAccess;
  UnicodeString FUnmungedRoot;

  __property bool ForceAnsi = { read = FForceAnsi, write = FForceAnsi };

  UnicodeString __fastcall GetCurrentSubKey();
  UnicodeString __fastcall GetCurrentSubKeyMunged();
  virtual void __fastcall SetAccessMode(TStorageAccessMode value);
  virtual bool __fastcall DoKeyExists(const UnicodeString & SubKey, bool ForceAnsi) = 0;
  virtual bool __fastcall DoValueExists(const UnicodeString & Value, bool ForceAnsi) = 0;
  static UnicodeString __fastcall IncludeTrailingBackslash(const UnicodeString & S);
  static UnicodeString __fastcall ExcludeTrailingBackslash(const UnicodeString & S);
  virtual bool __fastcall DoOpenSubKey(const UnicodeString & SubKey, bool CanCreate) = 0;
  virtual void __fastcall DoCloseSubKey() = 0;
  bool MungingKeyName(const UnicodeString & Key);
  UnicodeString __fastcall MungeKeyName(const UnicodeString & Key);
  UnicodeString MungeIniName(const UnicodeString & Str);
  virtual UnicodeString __fastcall GetSource() = 0;
  virtual bool __fastcall GetTemporary();
  virtual bool __fastcall DoDeleteSubKey(const UnicodeString & SubKey) = 0;
  virtual bool __fastcall DoDeleteValue(const UnicodeString & Name) = 0;

  virtual void __fastcall DoGetSubKeyNames(TStrings * Strings) = 0;
  virtual void __fastcall DoGetValueNames(TStrings * Strings) = 0;

  virtual void __fastcall DoWriteBool(const UnicodeString & Name, bool Value) = 0;
  virtual void __fastcall DoWriteStringRaw(const UnicodeString & Name, const UnicodeString & Value) = 0;
  virtual void __fastcall DoWriteInteger(const UnicodeString & Name, int Value) = 0;
  virtual void __fastcall DoWriteInt64(const UnicodeString & Name, __int64 Value) = 0;
  virtual void __fastcall DoWriteBinaryData(const UnicodeString & Name, const void * Buffer, int Size) = 0;

  virtual bool __fastcall DoReadBool(const UnicodeString & Name, bool Default) = 0;
  virtual UnicodeString __fastcall DoReadStringRaw(const UnicodeString & Name, const UnicodeString & Default) = 0;
  int ReadIntegerWithMapping(const UnicodeString & Name, int Default, const TIntMapping * Mapping);
  virtual int __fastcall DoReadInteger(const UnicodeString & Name, int Default, const TIntMapping * Mapping) = 0;
  virtual __int64 __fastcall DoReadInt64(const UnicodeString & Name, __int64 Default) = 0;
  virtual TDateTime __fastcall DoReadDateTime(const UnicodeString & Name, TDateTime Default) = 0;
  virtual double __fastcall DoReadFloat(const UnicodeString & Name, double Default) = 0;
  virtual size_t __fastcall DoReadBinaryData(const UnicodeString & Name, void * Buffer, size_t Size) = 0;

  virtual size_t __fastcall DoBinaryDataSize(const UnicodeString & Name) = 0;

  virtual UnicodeString __fastcall DoReadRootAccessString();

  size_t __fastcall BinaryDataSize(const UnicodeString & Name);
  UnicodeString __fastcall ReadAccessString();
  unsigned int __fastcall ReadAccess(unsigned int CurrentAccess);
  virtual bool __fastcall HasAccess(unsigned int Access);
  inline bool __fastcall CanRead();
  inline bool __fastcall CanWrite();
  virtual unsigned int __fastcall GetCurrentAccess();
};
//---------------------------------------------------------------------------
extern TIntMapping AutoSwitchMapping;
extern TIntMapping AutoSwitchReversedMapping;
//---------------------------------------------------------------------------
template<typename T>
T __fastcall THierarchicalStorage::ReadEnum(const UnicodeString & Name, const T & Default, const TIntMapping & Mapping)
{
  const TIntMapping * AMapping = (Mapping == TIntMapping()) ? NULL : &Mapping;
  return T(ReadIntegerWithMapping(Name, int(Default), AMapping));
}
//---------------------------------------------------------------------------
class TRegistryStorage : public THierarchicalStorage
{
public:
  __fastcall TRegistryStorage(const UnicodeString & AStorage, HKEY ARootKey, REGSAM WowMode = 0);
  __fastcall TRegistryStorage(const UnicodeString & AStorage);
  virtual __fastcall ~TRegistryStorage();

  bool __fastcall Copy(TRegistryStorage * Storage);

protected:
  virtual void __fastcall SetAccessMode(TStorageAccessMode value);
  virtual bool __fastcall DoKeyExists(const UnicodeString & SubKey, bool ForceAnsi);
  virtual bool __fastcall DoValueExists(const UnicodeString & Value, bool ForceAnsi);
  virtual bool __fastcall DoOpenSubKey(const UnicodeString & SubKey, bool CanCreate);
  virtual void __fastcall DoCloseSubKey();
  virtual UnicodeString __fastcall GetSource();
  virtual size_t __fastcall DoBinaryDataSize(const UnicodeString & Name);
  virtual bool __fastcall DoDeleteSubKey(const UnicodeString & SubKey);
  virtual bool __fastcall DoDeleteValue(const UnicodeString & Name);

  virtual void __fastcall DoGetSubKeyNames(TStrings * Strings);
  virtual void __fastcall DoGetValueNames(TStrings* Strings);

  virtual void __fastcall DoWriteBool(const UnicodeString & Name, bool Value);
  virtual void __fastcall DoWriteStringRaw(const UnicodeString & Name, const UnicodeString & Value);
  virtual void __fastcall DoWriteInteger(const UnicodeString & Name, int Value);
  virtual void __fastcall DoWriteInt64(const UnicodeString & Name, __int64 Value);
  virtual void __fastcall DoWriteBinaryData(const UnicodeString & Name, const void * Buffer, int Size);

  virtual bool __fastcall DoReadBool(const UnicodeString & Name, bool Default);
  virtual int __fastcall DoReadInteger(const UnicodeString & Name, int Default, const TIntMapping * Mapping);
  virtual __int64 __fastcall DoReadInt64(const UnicodeString & Name, __int64 Default);
  virtual TDateTime __fastcall DoReadDateTime(const UnicodeString & Name, TDateTime Default);
  virtual double __fastcall DoReadFloat(const UnicodeString & Name, double Default);
  virtual UnicodeString __fastcall DoReadStringRaw(const UnicodeString & Name, const UnicodeString & Default);
  virtual size_t __fastcall DoReadBinaryData(const UnicodeString & Name, void * Buffer, size_t Size);

private:
  TRegistry * FRegistry;
  REGSAM FWowMode;

  void __fastcall Init();
};
//---------------------------------------------------------------------------
class TCustomIniFileStorage : public THierarchicalStorage
{
public:
  __fastcall TCustomIniFileStorage(const UnicodeString & Storage, TCustomIniFile * IniFile);
  virtual __fastcall ~TCustomIniFileStorage();

  virtual bool __fastcall OpenSubKey(const UnicodeString & SubKey, bool CanCreate);
  virtual void __fastcall CloseSubKey();

private:
  UnicodeString __fastcall GetCurrentSection();
  inline bool __fastcall HandleByMasterStorage();
  inline bool __fastcall HandleReadByMasterStorage(const UnicodeString & Name);
  inline bool __fastcall DoValueExistsInternal(const UnicodeString & Value, bool ForceAnsi);
  void __fastcall DoWriteStringRawInternal(const UnicodeString & Name, const UnicodeString & Value);
  int __fastcall DoReadIntegerWithMapping(const UnicodeString & Name, int Default, const TIntMapping * Mapping);

protected:
  TCustomIniFile * FIniFile;
  std::unique_ptr<TStringList> FSections;
  std::unique_ptr<THierarchicalStorage> FMasterStorage;
  int FMasterStorageOpenFailures;
  bool FOpeningSubKey;

  __property UnicodeString CurrentSection = { read = GetCurrentSection };
  virtual void __fastcall SetAccessMode(TStorageAccessMode value);
  virtual bool __fastcall DoKeyExists(const UnicodeString & SubKey, bool ForceAnsi);
  virtual bool __fastcall DoValueExists(const UnicodeString & Value, bool ForceAnsi);
  virtual bool __fastcall DoOpenSubKey(const UnicodeString & SubKey, bool CanCreate);
  virtual void __fastcall DoCloseSubKey();
  virtual UnicodeString __fastcall GetSource();
  virtual size_t __fastcall DoBinaryDataSize(const UnicodeString & Name);
  virtual bool __fastcall DoDeleteSubKey(const UnicodeString & SubKey);
  virtual bool __fastcall DoDeleteValue(const UnicodeString & Name);

  virtual void __fastcall DoGetSubKeyNames(TStrings * Strings);
  virtual void __fastcall DoGetValueNames(TStrings * Strings);

  virtual void __fastcall DoWriteBool(const UnicodeString & Name, bool Value);
  virtual void __fastcall DoWriteStringRaw(const UnicodeString & Name, const UnicodeString & Value);
  virtual void __fastcall DoWriteInteger(const UnicodeString & Name, int Value);
  virtual void __fastcall DoWriteInt64(const UnicodeString & Name, __int64 Value);
  virtual void __fastcall DoWriteBinaryData(const UnicodeString & Name, const void * Buffer, int Size);

  virtual bool __fastcall DoReadBool(const UnicodeString & Name, bool Default);
  virtual int __fastcall DoReadInteger(const UnicodeString & Name, int Default, const TIntMapping * Mapping);
  virtual __int64 __fastcall DoReadInt64(const UnicodeString & Name, __int64 Default);
  virtual TDateTime __fastcall DoReadDateTime(const UnicodeString & Name, TDateTime Default);
  virtual double __fastcall DoReadFloat(const UnicodeString & Name, double Default);
  virtual UnicodeString __fastcall DoReadStringRaw(const UnicodeString & Name, const UnicodeString & Default);
  virtual size_t __fastcall DoReadBinaryData(const UnicodeString & Name, void * Buffer, size_t Size);

  virtual UnicodeString __fastcall DoReadRootAccessString();
  virtual unsigned int __fastcall GetCurrentAccess();
  virtual bool __fastcall HasAccess(unsigned int Access);

  void __fastcall CacheSections();
  void __fastcall ResetCache();
  bool __fastcall DoKeyExistsInternal(const UnicodeString & SubKey);
};
//---------------------------------------------------------------------------
class TIniFileStorage : public TCustomIniFileStorage
{
public:
  static TIniFileStorage * __fastcall CreateFromPath(const UnicodeString & AStorage);
  static TIniFileStorage * __fastcall CreateNul();
  virtual __fastcall ~TIniFileStorage();

  virtual void __fastcall Flush();

private:
  __fastcall TIniFileStorage(const UnicodeString & FileName, TCustomIniFile * IniFile);
  TStrings * FOriginal;
  void __fastcall ApplyOverrides();
};
//---------------------------------------------------------------------------
class TOptionsStorage : public TCustomIniFileStorage
{
public:
  __fastcall TOptionsStorage(TStrings * Options, bool AllowWrite);
  __fastcall TOptionsStorage(TStrings * Options, const UnicodeString & RootKey, THierarchicalStorage * MasterStorage);

protected:
  virtual bool __fastcall GetTemporary();
};
//---------------------------------------------------------------------------
UnicodeString __fastcall PuttyMungeStr(const UnicodeString & Str);
AnsiString PuttyStr(const UnicodeString & Str);
TIntMapping CreateIntMappingFromEnumNames(const UnicodeString & Names);
//---------------------------------------------------------------------------
#endif
