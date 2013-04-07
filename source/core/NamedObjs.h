//---------------------------------------------------------------------------
#ifndef NamedObjsH
#define NamedObjsH

#include <system.hpp>
#include <contnrs.hpp>
//---------------------------------------------------------------------------
class TNamedObjectList;
class TNamedObject : public TPersistent
{
public:
  __property UnicodeString Name = { read = FName, write = SetName };
  __property bool Hidden = { read = FHidden };
  __fastcall TNamedObject() {};
  Integer __fastcall CompareName(UnicodeString aName, Boolean CaseSensitive = False);
  __fastcall TNamedObject(UnicodeString aName);
  void __fastcall MakeUniqueIn(TNamedObjectList * List);
private:
  UnicodeString FName;
  bool FHidden;

  void __fastcall SetName(UnicodeString value);
};
//---------------------------------------------------------------------------
class TNamedObjectList : public TObjectList
{
private:
  int FHiddenCount;
  int __fastcall GetCount();
  virtual void __fastcall Notify(void *Ptr, TListNotification Action);
  void __fastcall SetCount(int value);
protected:
  void __fastcall Recount();
public:
  static const UnicodeString HiddenPrefix;

  bool AutoSort;

  __fastcall TNamedObjectList();
  void __fastcall AlphaSort();
  virtual TNamedObject * __fastcall AtObject(Integer Index);
  TNamedObject * __fastcall FindByName(UnicodeString Name, Boolean CaseSensitive = False);
  __property int Count = { read = GetCount, write = SetCount };
  __property int HiddenCount = { read = FHiddenCount, write = FHiddenCount };
};
//---------------------------------------------------------------------------
int __fastcall NamedObjectSortProc(void * Item1, void * Item2);
//---------------------------------------------------------------------------
#endif
