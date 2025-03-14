//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "NamedObjs.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
int __fastcall NamedObjectSortProc(void * Item1, void * Item2)
{
  return static_cast<TNamedObject *>(Item1)->Compare(static_cast<TNamedObject *>(Item2));
}
//--- TNamedObject ----------------------------------------------------------
__fastcall TNamedObject::TNamedObject(UnicodeString AName)
{
  Name = AName;
}
//---------------------------------------------------------------------------
void __fastcall TNamedObject::SetName(UnicodeString value)
{
  FHidden = (value.SubString(1, TNamedObjectList::HiddenPrefix.Length()) == TNamedObjectList::HiddenPrefix);
  FName = value;
}
//---------------------------------------------------------------------------
int __fastcall TNamedObject::Compare(TNamedObject * Other)
{
  int Result;
  if (Hidden && !Other->Hidden)
  {
    Result = -1;
  }
  else if (!Hidden && Other->Hidden)
  {
    Result = 1;
  }
  else
  {
    Result = CompareLogicalText(Name, Other->Name, true);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TNamedObject::IsSameName(const UnicodeString & AName)
{
  return (Name.CompareIC(AName) == 0);
}
//---------------------------------------------------------------------------
void __fastcall TNamedObject::MakeUniqueIn(TNamedObjectList * List)
{
  // This object can't be item of list, it would create infinite loop
  if (List && (List->IndexOf(this) == -1))
    while (List->FindByName(Name))
    {
      Integer N = 0, P CLANG_INITIALIZE(0);
      // If name already contains number parenthesis remove it (and remember it)
      if ((Name[Name.Length()] == L')') && ((P = Name.LastDelimiter(L'(')) > 0))
        try
        {
          N = StrToInt(Name.SubString(P + 1, Name.Length() - P - 1));
          Name.Delete(P, Name.Length() - P + 1);
          Name = Name.TrimRight();
        }
        catch (Exception &)
        {
          N = 0;
        }
      Name += L" (" + IntToStr(N+1) + L")";
    }
}
//--- TNamedObjectList ------------------------------------------------------
const UnicodeString TNamedObjectList::HiddenPrefix = L"_!_";
//---------------------------------------------------------------------------
__fastcall TNamedObjectList::TNamedObjectList():
  TObjectList()
{
  AutoSort = True;
  FHiddenCount = 0;
  FControlledAdd = false;
}
//---------------------------------------------------------------------------
TNamedObject * __fastcall TNamedObjectList::AtObject(Integer Index)
{
  return (TNamedObject *)Items[Index+FHiddenCount];
}
//---------------------------------------------------------------------------
void __fastcall TNamedObjectList::Recount()
{
  int i = 0;
  while ((i < TObjectList::Count) && ((TNamedObject *)Items[i])->Hidden) i++;
  FHiddenCount = i;
}
//---------------------------------------------------------------------------
void __fastcall TNamedObjectList::AlphaSort()
{
  Sort(NamedObjectSortProc);
  Recount();
}
//---------------------------------------------------------------------------
int __fastcall TNamedObjectList::Add(TObject * AObject)
{
  int Result;
  TAutoFlag ControlledAddFlag(FControlledAdd);
  TNamedObject * NamedObject = static_cast<TNamedObject *>(AObject);
  // If temporarily not auto-sorting (when loading session list),
  // keep the hidden objects in front, so that HiddenCount is correct
  if (!AutoSort && NamedObject->Hidden)
  {
    Result = 0;
    Insert(Result, AObject);
    FHiddenCount++;
  }
  else
  {
    Result = TObjectList::Add(AObject);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TNamedObjectList::Notify(void *Ptr, TListNotification Action)
{
  if (Action == lnDeleted)
  {
    TNamedObject * NamedObject = static_cast<TNamedObject *>(Ptr);
    if (NamedObject->Hidden && (FHiddenCount >= 0))
    {
      FHiddenCount--;
    }
  }
  TObjectList::Notify(Ptr, Action);
  if (Action == lnAdded)
  {
    if (!FControlledAdd)
    {
      FHiddenCount = -1;
    }
    if (AutoSort)
    {
      AlphaSort();
    }
  }
}
//---------------------------------------------------------------------------
TNamedObject * __fastcall TNamedObjectList::FindByName(const UnicodeString & Name)
{
  // This should/can be optimized when list is sorted
  for (Integer Index = 0; Index < CountIncludingHidden; Index++)
  {
    // Not using AtObject as we iterate even hidden objects here
    TNamedObject * NamedObject = static_cast<TNamedObject *>(Items[Index]);
    if (NamedObject->IsSameName(Name))
    {
      return NamedObject;
    }
  }
  return NULL;
}
//---------------------------------------------------------------------------
int __fastcall TNamedObjectList::GetCount()
{
  DebugAssert(FHiddenCount >= 0);
  return TObjectList::Count - FHiddenCount;
}
//---------------------------------------------------------------------------
int __fastcall TNamedObjectList::GetCountIncludingHidden()
{
  DebugAssert(FHiddenCount >= 0);
  return TObjectList::Count;
}
