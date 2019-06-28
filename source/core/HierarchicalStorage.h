//---------------------------------------------------------------------------
#ifndef HierarchicalStorageH
#define HierarchicalStorageH

#include <registry.hpp>
#include <memory>
//---------------------------------------------------------------------------
enum TStorage { stDetect, stRegistry, stIniFile, stNul };
enum TStorageAccessMode { smRead, smReadWrite };
//---------------------------------------------------------------------------
class THierarchicalStorage
{
friend class TCustomIniFileStorage;

public:
  __fastcall THierarchicalStorage(const UnicodeString & AStorage);
  virtual __fastcall ~THierarchicalStorage();
  virtual bool __fastcall OpenRootKey(bool CanCreate);
  virtual bool __fastcall OpenSubKey(const UnicodeString & SubKey, bool CanCreate, bool Path = false);
  virtual void __fastcall CloseSubKey();
  void __fastcall CloseAll();
  virtual bool __fastcall DeleteSubKey(const UnicodeString & SubKey) = 0;
  virtual void __fastcall GetSubKeyNames(TStrings* Strings) = 0;
  virtual void __fastcall GetValueNames(TStrings* Strings) = 0;
  bool __fastcall HasSubKeys();
  bool __fastcall HasSubKey(const UnicodeString & SubKey);
  bool __fastcall KeyExists(const UnicodeString & SubKey);
  virtual bool __fastcall ValueExists(const UnicodeString & Value) = 0;
  virtual void __fastcall RecursiveDeleteSubKey(const UnicodeString & Key);
  virtual void __fastcall ClearSubKeys();
  virtual void __fastcall ReadValues(TStrings* Strings, bool MaintainKeys = false);
  virtual void __fastcall WriteValues(TStrings* Strings, bool MaintainKeys = false);
  virtual void __fastcall ClearValues();
  virtual bool __fastcall DeleteValue(const UnicodeString & Name) = 0;

  virtual size_t __fastcall BinaryDataSize(const UnicodeString & Name) = 0;

  virtual bool __fastcall ReadBool(const UnicodeString & Name, bool Default) = 0;
  virtual int __fastcall ReadInteger(const UnicodeString & Name, int Default) = 0;
  virtual __int64 __fastcall ReadInt64(const UnicodeString & Name, __int64 Default) = 0;
  virtual TDateTime __fastcall ReadDateTime(const UnicodeString & Name, TDateTime Default) = 0;
  virtual double __fastcall ReadFloat(const UnicodeString & Name, double Default) = 0;
  virtual UnicodeString __fastcall ReadStringRaw(const UnicodeString & Name, const UnicodeString & Default) = 0;
  virtual size_t __fastcall ReadBinaryData(const UnicodeString & Name, void * Buffer, size_t Size) = 0;

  virtual UnicodeString __fastcall ReadString(const UnicodeString & Name, const UnicodeString & Default);
  RawByteString __fastcall ReadBinaryData(const UnicodeString & Name);
  RawByteString __fastcall ReadStringAsBinaryData(const UnicodeString & Name, const RawByteString & Default);

  virtual void __fastcall WriteBool(const UnicodeString & Name, bool Value) = 0;
  virtual void __fastcall WriteStringRaw(const UnicodeString & Name, const UnicodeString & Value) = 0;
  virtual void __fastcall WriteInteger(const UnicodeString & Name, int Value) = 0;
  virtual void __fastcall WriteInt64(const UnicodeString & Name, __int64 Value) = 0;
  virtual void __fastcall WriteDateTime(const UnicodeString & Name, TDateTime Value) = 0;
  virtual void __fastcall WriteFloat(const UnicodeString & Name, double Value) = 0;
  virtual void __fastcall WriteBinaryData(const UnicodeString & Name, const void * Buffer, int Size) = 0;

  virtual void __fastcall WriteString(const UnicodeString & Name, const UnicodeString & Value);
  void __fastcall WriteBinaryData(const UnicodeString & Name, const RawByteString & Value);
  void __fastcall WriteBinaryDataAsString(const UnicodeString & Name, const RawByteString & Value);

  virtual void __fastcall Flush();

  __property UnicodeString Storage = { read = FStorage };
  __property UnicodeString CurrentSubKey = { read = GetCurrentSubKey };
  __property TStorageAccessMode AccessMode = { read = FAccessMode, write = SetAccessMode };
  __property bool Explicit = { read = FExplicit, write = FExplicit };
  __property bool ForceSave = { read = FForceSave, write = FForceSave };
  __property bool ForceAnsi = { read = FForceAnsi, write = FForceAnsi };
  __property bool MungeStringValues = { read = FMungeStringValues, write = FMungeStringValues };
  __property UnicodeString Source = { read = GetSource };
  __property bool Temporary = { read = GetTemporary };

protected:
  UnicodeString FStorage;
  TStrings * FKeyHistory;
  TStorageAccessMode FAccessMode;
  bool FExplicit;
  bool FForceSave;
  bool FMungeStringValues;
  bool FForceAnsi;

  UnicodeString __fastcall GetCurrentSubKey();
  UnicodeString __fastcall GetCurrentSubKeyMunged();
  virtual void __fastcall SetAccessMode(TStorageAccessMode value);
  virtual bool __fastcall DoKeyExists(const UnicodeString & SubKey, bool ForceAnsi) = 0;
  static UnicodeString __fastcall IncludeTrailingBackslash(const UnicodeString & S);
  static UnicodeString __fastcall ExcludeTrailingBackslash(const UnicodeString & S);
  virtual bool __fastcall DoOpenSubKey(const UnicodeString & SubKey, bool CanCreate) = 0;
  UnicodeString __fastcall MungeKeyName(const UnicodeString & Key);
  virtual UnicodeString __fastcall GetSource() = 0;
  virtual bool __fastcall GetTemporary();
};
//---------------------------------------------------------------------------
class TRegistryStorage : public THierarchicalStorage
{
public:
  __fastcall TRegistryStorage(const UnicodeString & AStorage, HKEY ARootKey, REGSAM WowMode = 0);
  __fastcall TRegistryStorage(const UnicodeString & AStorage);
  virtual __fastcall ~TRegistryStorage();

  bool __fastcall Copy(TRegistryStorage * Storage);

  virtual void __fastcall CloseSubKey();
  virtual bool __fastcall DeleteSubKey(const UnicodeString & SubKey);
  virtual bool __fastcall DeleteValue(const UnicodeString & Name);
  virtual void __fastcall GetSubKeyNames(TStrings * Strings);
  virtual bool __fastcall ValueExists(const UnicodeString & Value);

  virtual size_t __fastcall BinaryDataSize(const UnicodeString & Name);

  virtual bool __fastcall ReadBool(const UnicodeString & Name, bool Default);
  virtual int __fastcall ReadInteger(const UnicodeString & Name, int Default);
  virtual __int64 __fastcall ReadInt64(const UnicodeString & Name, __int64 Default);
  virtual TDateTime __fastcall ReadDateTime(const UnicodeString & Name, TDateTime Default);
  virtual double __fastcall ReadFloat(const UnicodeString & Name, double Default);
  virtual UnicodeString __fastcall ReadStringRaw(const UnicodeString & Name, const UnicodeString & Default);
  virtual size_t __fastcall ReadBinaryData(const UnicodeString & Name, void * Buffer, size_t Size);

  virtual void __fastcall WriteBool(const UnicodeString & Name, bool Value);
  virtual void __fastcall WriteInteger(const UnicodeString & Name, int Value);
  virtual void __fastcall WriteInt64(const UnicodeString & Name, __int64 Value);
  virtual void __fastcall WriteDateTime(const UnicodeString & Name, TDateTime Value);
  virtual void __fastcall WriteFloat(const UnicodeString & Name, double Value);
  virtual void __fastcall WriteStringRaw(const UnicodeString & Name, const UnicodeString & Value);
  virtual void __fastcall WriteBinaryData(const UnicodeString & Name, const void * Buffer, int Size);

  virtual void __fastcall GetValueNames(TStrings* Strings);

protected:
  virtual void __fastcall SetAccessMode(TStorageAccessMode value);
  virtual bool __fastcall DoKeyExists(const UnicodeString & SubKey, bool ForceAnsi);
  virtual bool __fastcall DoOpenSubKey(const UnicodeString & SubKey, bool CanCreate);
  virtual UnicodeString __fastcall GetSource();

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

  virtual bool __fastcall OpenRootKey(bool CanCreate);
  virtual bool __fastcall OpenSubKey(const UnicodeString & SubKey, bool CanCreate, bool Path = false);
  virtual void __fastcall CloseSubKey();
  virtual bool __fastcall DeleteSubKey(const UnicodeString & SubKey);
  virtual bool __fastcall DeleteValue(const UnicodeString & Name);
  virtual void __fastcall GetSubKeyNames(TStrings * Strings);
  virtual bool __fastcall ValueExists(const UnicodeString & Value);

  virtual size_t __fastcall BinaryDataSize(const UnicodeString & Name);

  virtual bool __fastcall ReadBool(const UnicodeString & Name, bool Default);
  virtual int __fastcall ReadInteger(const UnicodeString & Name, int Default);
  virtual __int64 __fastcall ReadInt64(const UnicodeString & Name, __int64 Default);
  virtual TDateTime __fastcall ReadDateTime(const UnicodeString & Name, TDateTime Default);
  virtual double __fastcall ReadFloat(const UnicodeString & Name, double Default);
  virtual UnicodeString __fastcall ReadStringRaw(const UnicodeString & Name, const UnicodeString & Default);
  virtual size_t __fastcall ReadBinaryData(const UnicodeString & Name, void * Buffer, size_t Size);

  virtual void __fastcall WriteBool(const UnicodeString & Name, bool Value);
  virtual void __fastcall WriteInteger(const UnicodeString & Name, int Value);
  virtual void __fastcall WriteInt64(const UnicodeString & Name, __int64 Value);
  virtual void __fastcall WriteDateTime(const UnicodeString & Name, TDateTime Value);
  virtual void __fastcall WriteFloat(const UnicodeString & Name, double Value);
  virtual void __fastcall WriteStringRaw(const UnicodeString & Name, const UnicodeString & Value);
  virtual void __fastcall WriteBinaryData(const UnicodeString & Name, const void * Buffer, int Size);

  virtual void __fastcall GetValueNames(TStrings * Strings);

private:
  UnicodeString __fastcall GetCurrentSection();
  inline bool __fastcall HandleByMasterStorage();
  inline bool __fastcall HandleReadByMasterStorage(const UnicodeString & Name);
  inline bool __fastcall DoValueExists(const UnicodeString & Value);
  void __fastcall DoWriteStringRaw(const UnicodeString & Name, const UnicodeString & Value);
  void __fastcall DoWriteBinaryData(const UnicodeString & Name, const void * Buffer, int Size);

protected:
  TCustomIniFile * FIniFile;
  std::unique_ptr<TStringList> FSections;
  std::unique_ptr<THierarchicalStorage> FMasterStorage;
  int FMasterStorageOpenFailures;
  bool FOpeningSubKey;

  __property UnicodeString CurrentSection = { read = GetCurrentSection };
  virtual void __fastcall SetAccessMode(TStorageAccessMode value);
  virtual bool __fastcall DoKeyExists(const UnicodeString & SubKey, bool ForceAnsi);
  virtual bool __fastcall DoOpenSubKey(const UnicodeString & SubKey, bool CanCreate);
  virtual UnicodeString __fastcall GetSource();
  void __fastcall CacheSections();
  void __fastcall ResetCache();
};
//---------------------------------------------------------------------------
class TIniFileStorage : public TCustomIniFileStorage
{
public:
  static TIniFileStorage * __fastcall CreateFromPath(const UnicodeString & AStorage);
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
//---------------------------------------------------------------------------
#endif
