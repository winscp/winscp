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
  AnsiString Name;
  __fastcall TNamedObject(): TPersistent() {};
  Integer __fastcall CompareName(AnsiString aName, Boolean CaseSensitive = False);
  __fastcall TNamedObject(AnsiString aName): TPersistent(), Name(aName) {}
  void __fastcall MakeUniqueIn(TNamedObjectList * List);
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
  static const AnsiString HiddenPrefix;
  static bool __fastcall IsHidden(TNamedObject * Object);

  bool AutoSort;

  __fastcall TNamedObjectList();
  void __fastcall AlphaSort();
  virtual TNamedObject * __fastcall AtObject(Integer Index);
  TNamedObject * __fastcall FindByName(AnsiString Name, Boolean CaseSensitive = False);
  __property int Count = { read = GetCount, write = SetCount };
  __property int HiddenCount = { read = FHiddenCount, write = FHiddenCount };
};
//---------------------------------------------------------------------------
int __fastcall NamedObjectSortProc(void * Item1, void * Item2);
//---------------------------------------------------------------------------
#endif
