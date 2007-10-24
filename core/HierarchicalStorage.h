//---------------------------------------------------------------------------
#ifndef HierarchicalStorageH
#define HierarchicalStorageH

#include <registry.hpp>
//---------------------------------------------------------------------------
enum TStorage { stDetect, stRegistry, stIniFile };
enum TStorageAccessMode { smRead, smReadWrite };
//---------------------------------------------------------------------------
class THierarchicalStorage
{
public:
  __fastcall THierarchicalStorage(const AnsiString AStorage);
  virtual __fastcall ~THierarchicalStorage();
  bool __fastcall OpenRootKey(bool CanCreate);
  virtual bool __fastcall OpenSubKey(const AnsiString SubKey, bool CanCreate);
  virtual bool __fastcall CreateSubKey(const AnsiString SubKey);
  virtual void __fastcall CloseSubKey();
  virtual bool __fastcall DeleteSubKey(const AnsiString SubKey) = 0;
  virtual void __fastcall GetSubKeyNames(Classes::TStrings* Strings) = 0;
  virtual void __fastcall GetValueNames(Classes::TStrings* Strings) = 0;
  bool __fastcall HasSubKeys();
  virtual bool __fastcall KeyExists(const AnsiString SubKey) = 0;
  virtual bool __fastcall ValueExists(const AnsiString Value) = 0;
  virtual void __fastcall RecursiveDeleteSubKey(const AnsiString Key);
  virtual void __fastcall ClearSubKeys();
  virtual void __fastcall ReadValues(Classes::TStrings* Strings, bool MaintainKeys = false);
  virtual void __fastcall WriteValues(Classes::TStrings* Strings, bool MaintainKeys = false);
  virtual void __fastcall ClearValues();
  virtual bool __fastcall DeleteValue(const AnsiString Name) = 0;

  virtual int __fastcall BinaryDataSize(const AnsiString Name) = 0;

  virtual bool __fastcall ReadBool(const AnsiString Name, bool Default) = 0;
  virtual int __fastcall ReadInteger(const AnsiString Name, int Default) = 0;
  virtual __int64 __fastcall ReadInt64(const AnsiString Name, __int64 Default) = 0;
  virtual TDateTime __fastcall ReadDateTime(const AnsiString Name, TDateTime Default) = 0;
  virtual double __fastcall ReadFloat(const AnsiString Name, double Default) = 0;
  virtual AnsiString __fastcall ReadStringRaw(const AnsiString Name, const AnsiString Default) = 0;
  virtual int __fastcall ReadBinaryData(const AnsiString Name, void * Buffer, int Size) = 0;

  virtual AnsiString __fastcall ReadString(AnsiString Name, AnsiString Default);
  AnsiString __fastcall ReadBinaryData(const AnsiString Name);

  virtual void __fastcall WriteBool(const AnsiString Name, bool Value) = 0;
  virtual void __fastcall WriteStringRaw(const AnsiString Name, const AnsiString Value) = 0;
  virtual void __fastcall WriteInteger(const AnsiString Name, int Value) = 0;
  virtual void __fastcall WriteInt64(AnsiString Name, __int64 Value) = 0;
  virtual void __fastcall WriteDateTime(const AnsiString Name, TDateTime Value) = 0;
  virtual void __fastcall WriteFloat(const AnsiString Name, double Value) = 0;
  virtual void __fastcall WriteBinaryData(const AnsiString Name, const void * Buffer, int Size) = 0;

  virtual void __fastcall WriteString(const AnsiString Name, const AnsiString Value);
  void __fastcall WriteBinaryData(const AnsiString Name, const AnsiString Value);

  __property AnsiString Storage  = { read=FStorage };
  __property AnsiString CurrentSubKey  = { read=GetCurrentSubKey };
  __property TStorageAccessMode AccessMode  = { read=FAccessMode, write=SetAccessMode };

protected:
  AnsiString FStorage;
  TStrings * FKeyHistory;
  TStorageAccessMode FAccessMode;

  AnsiString __fastcall GetCurrentSubKey();
  virtual void __fastcall SetAccessMode(TStorageAccessMode value);
  static AnsiString __fastcall IncludeTrailingBackslash(const AnsiString & S);
  static AnsiString __fastcall ExcludeTrailingBackslash(const AnsiString & S);
};
//---------------------------------------------------------------------------
class TRegistryStorage : public THierarchicalStorage
{
public:
  __fastcall TRegistryStorage(const AnsiString AStorage, HKEY ARootKey);
  __fastcall TRegistryStorage(const AnsiString AStorage);
  virtual __fastcall ~TRegistryStorage();

  bool __fastcall Copy(TRegistryStorage * Storage);

  virtual bool __fastcall OpenSubKey(const AnsiString SubKey, bool CanCreate);
  virtual bool __fastcall CreateSubKey(const AnsiString SubKey);
  virtual void __fastcall CloseSubKey();
  virtual bool __fastcall DeleteSubKey(const AnsiString SubKey);
  virtual bool __fastcall DeleteValue(const AnsiString Name);
  virtual void __fastcall GetSubKeyNames(Classes::TStrings* Strings);
  virtual bool __fastcall KeyExists(const AnsiString SubKey);
  virtual bool __fastcall ValueExists(const AnsiString Value);

  virtual int __fastcall BinaryDataSize(const AnsiString Name);

  virtual bool __fastcall ReadBool(const AnsiString Name, bool Default);
  virtual int __fastcall ReadInteger(const AnsiString Name, int Default);
  virtual __int64 __fastcall ReadInt64(const AnsiString Name, __int64 Default);
  virtual TDateTime __fastcall ReadDateTime(const AnsiString Name, TDateTime Default);
  virtual double __fastcall ReadFloat(const AnsiString Name, double Default);
  virtual AnsiString __fastcall ReadStringRaw(const AnsiString Name, const AnsiString Default);
  virtual int __fastcall ReadBinaryData(const AnsiString Name, void * Buffer, int Size);

  virtual void __fastcall WriteBool(const AnsiString Name, bool Value);
  virtual void __fastcall WriteInteger(const AnsiString Name, int Value);
  virtual void __fastcall WriteInt64(const AnsiString Name, __int64 Value);
  virtual void __fastcall WriteDateTime(const AnsiString Name, TDateTime Value);
  virtual void __fastcall WriteFloat(const AnsiString Name, double Value);
  virtual void __fastcall WriteStringRaw(const AnsiString Name, const AnsiString Value);
  virtual void __fastcall WriteBinaryData(const AnsiString Name, const void * Buffer, int Size);

  virtual void __fastcall GetValueNames(Classes::TStrings* Strings);

protected:
  int __fastcall GetFailed();
  virtual void __fastcall SetAccessMode(TStorageAccessMode value);

  __property int Failed  = { read=GetFailed, write=FFailed };

private:
  TRegistry * FRegistry;
  int FFailed;

  void __fastcall Init();
};
//---------------------------------------------------------------------------
class TIniFileStorage : public THierarchicalStorage
{
public:
  __fastcall TIniFileStorage(const AnsiString FileName);
  virtual __fastcall ~TIniFileStorage();

  virtual bool __fastcall OpenSubKey(const AnsiString SubKey, bool CanCreate);
  virtual bool __fastcall DeleteSubKey(const AnsiString SubKey);
  virtual bool __fastcall DeleteValue(const AnsiString Name);
  virtual void __fastcall GetSubKeyNames(Classes::TStrings* Strings);
  virtual bool __fastcall KeyExists(const AnsiString SubKey);
  virtual bool __fastcall ValueExists(const AnsiString Value);

  virtual int __fastcall BinaryDataSize(const AnsiString Name);

  virtual bool __fastcall ReadBool(const AnsiString Name, bool Default);
  virtual int __fastcall ReadInteger(const AnsiString Name, int Default);
  virtual __int64 __fastcall ReadInt64(const AnsiString Name, __int64 Default);
  virtual TDateTime __fastcall ReadDateTime(const AnsiString Name, TDateTime Default);
  virtual double __fastcall ReadFloat(const AnsiString Name, double Default);
  virtual AnsiString __fastcall ReadStringRaw(const AnsiString Name, const AnsiString Default);
  virtual int __fastcall ReadBinaryData(const AnsiString Name, void * Buffer, int Size);

  virtual void __fastcall WriteBool(const AnsiString Name, bool Value);
  virtual void __fastcall WriteInteger(const AnsiString Name, int Value);
  virtual void __fastcall WriteInt64(AnsiString Name, __int64 Value);
  virtual void __fastcall WriteDateTime(const AnsiString Name, TDateTime Value);
  virtual void __fastcall WriteFloat(const AnsiString Name, double Value);
  virtual void __fastcall WriteStringRaw(const AnsiString Name, const AnsiString Value);
  virtual void __fastcall WriteBinaryData(const AnsiString Name, const void * Buffer, int Size);

  virtual void __fastcall GetValueNames(Classes::TStrings* Strings);

private:
  TCustomIniFile * FIniFile;
  AnsiString __fastcall GetCurrentSection();
protected:
  __property AnsiString CurrentSection  = { read=GetCurrentSection };
};
//---------------------------------------------------------------------------
AnsiString __fastcall MungeStr(const AnsiString Str);
AnsiString __fastcall UnMungeStr(const AnsiString Str);
AnsiString __fastcall SimpleMungeStr(const AnsiString Str);
//---------------------------------------------------------------------------
#endif
