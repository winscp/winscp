//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <Common.h>
#include "Bookmarks.h"
#include "HierarchicalStorage.h"
#include "TextsWin.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define IS_NUMBER(STR) (StrToIntDef(STR, -123) != -123)
//---------------------------------------------------------------------------
__fastcall TBookmarks::TBookmarks(): TObject()
{
  FBookmarkLists = new TStringList();
  FBookmarkLists->Sorted = true;
  FBookmarkLists->CaseSensitive = false;
  FBookmarkLists->Duplicates = dupError;
}
//---------------------------------------------------------------------------
__fastcall TBookmarks::~TBookmarks()
{
  Clear();
  SAFE_DESTROY(FBookmarkLists);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarks::Clear()
{
  for (int i = 0; i < FBookmarkLists->Count; i++)
  {
    delete FBookmarkLists->Objects[i];
  }
  FBookmarkLists->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TBookmarks::Load(THierarchicalStorage * Storage)
{
  bool Local = false;
  do
  {
    Local = !Local;
    if (Storage->OpenSubKey(Local ? "Local" : "Remote", false))
    {
      TStrings * BookmarkKeys = new TStringList();
      try
      {
        Storage->GetSubKeyNames(BookmarkKeys);
        for (int Index = 0; Index < BookmarkKeys->Count; Index++)
        {
          AnsiString Key = BookmarkKeys->Strings[Index];
          if (Storage->OpenSubKey(Key, false))
          {
            TBookmarkList * BookmarkList = Bookmarks[Key];
            if (!BookmarkList)
            {
              BookmarkList = new TBookmarkList();
              FBookmarkLists->AddObject(Key, BookmarkList);
            }
            LoadLevel(Storage, "", Local, BookmarkList);
            Storage->CloseSubKey();
          }
        } 
      }
      __finally
      {
        delete BookmarkKeys;
      }
      Storage->CloseSubKey();
    }
  }
  while (Local);

  ModifyAll(false);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarks::LoadLevel(THierarchicalStorage * Storage, const AnsiString Key,
  bool Local, TBookmarkList * BookmarkList)
{
  TStrings * Names = new TStringList();
  try
  {
    Storage->GetValueNames(Names);
    AnsiString Name, Directory;
    for (int i = 0; i < Names->Count; i++)
    {
      Name = Names->Strings[i];
      Directory = Storage->ReadString(Name, "");
      TBookmark * Bookmark;
      if (IS_NUMBER(Name))
      {
        Name = Directory;
      }
      if (!Name.IsEmpty())
      {
        Bookmark = BookmarkList->FindByName(Key, Name);
        bool New;
        New = (Bookmark == NULL);
        if (New)
        {
          Bookmark = new TBookmark();
          Bookmark->Node = Key;
          Bookmark->Name = Name;
        }
        if (Local)
        {
          Bookmark->Local = Directory;
        }
        else
        {
          Bookmark->Remote = Directory;
        }
        if (New)
        {
          BookmarkList->Add(Bookmark);
        }
      }
    }

    Storage->GetSubKeyNames(Names);
    for (int i = 0; i < Names->Count; i++)
    {
      Name = Names->Strings[i];
      if (Storage->OpenSubKey(Name, false))
      {
        LoadLevel(Storage, Key + (Key.IsEmpty() ? "" : "/") + Name, Local, BookmarkList);
        Storage->CloseSubKey();
      }
    }
  }
  __finally
  {
    delete Names;
  }
}
//---------------------------------------------------------------------------
void __fastcall TBookmarks::Save(THierarchicalStorage * Storage)
{
  bool Local = false;
  do
  {
    Local = !Local;
    if (Storage->OpenSubKey(Local ? "Local" : "Remote", true))
    {
      for (int Index = 0; Index < FBookmarkLists->Count; Index++)
      {
        TBookmarkList * BookmarkList = dynamic_cast<TBookmarkList *>(FBookmarkLists->Objects[Index]);
        if (BookmarkList->Modified)
        {
          AnsiString Key;
          Key = FBookmarkLists->Strings[Index];
          Storage->RecursiveDeleteSubKey(Key);
          if (Storage->OpenSubKey(Key, true))
          {
            for (int IndexB = 0; IndexB < BookmarkList->Count; IndexB++)
            {
              TBookmark * Bookmark = BookmarkList->Bookmarks[IndexB];
              AnsiString Directory = Local ? Bookmark->Local : Bookmark->Remote;
              if (!Bookmark->Node.IsEmpty())
              {
                if (Storage->OpenSubKey(Bookmark->Node, true))
                {
                  Storage->WriteString(Bookmark->Name, Directory);
                  Storage->CloseSubKey();
                }
              }
              else
              {
                Storage->WriteString(Bookmark->Name, Directory);
              }
            }
            Storage->CloseSubKey();
          }
        }
      }
      Storage->CloseSubKey();
    }
  }
  while (Local);

  ModifyAll(false);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarks::ModifyAll(bool Modify)
{
  TBookmarkList * BookmarkList;
  for (int i = 0; i < FBookmarkLists->Count; i++)
  {
    BookmarkList = dynamic_cast<TBookmarkList *>(FBookmarkLists->Objects[i]);
    assert(BookmarkList);
    BookmarkList->Modified = Modify;
  }
}
//---------------------------------------------------------------------------
TBookmarkList * __fastcall TBookmarks::GetBookmarks(AnsiString Index)
{
  int I = FBookmarkLists->IndexOf(Index);
  if (I >= 0)
  {
    return dynamic_cast<TBookmarkList *>(FBookmarkLists->Objects[I]);
  }
  else
  {
    return NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TBookmarks::SetBookmarks(AnsiString Index, TBookmarkList * value)
{
  int I = FBookmarkLists->IndexOf(Index);
  if (I >= 0)
  {
    TBookmarkList * BookmarkList; 
    BookmarkList = dynamic_cast<TBookmarkList *>(FBookmarkLists->Objects[I]);
    BookmarkList->Assign(value);
  }
  else
  {
    TBookmarkList * BookmarkList = new TBookmarkList();
    BookmarkList->Assign(value); 
    FBookmarkLists->AddObject(Index, BookmarkList);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBookmarkList::TBookmarkList(): TPersistent()
{
  FModified = false;
  FBookmarks = new TStringList();
  FBookmarks->CaseSensitive = false;
}
//---------------------------------------------------------------------------
__fastcall TBookmarkList::~TBookmarkList()
{
  Clear();
  SAFE_DESTROY(FBookmarks);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::Clear()
{
  for (int i = 0; i < FBookmarks->Count; i++)
  {
    delete FBookmarks->Objects[i];
  }
  FBookmarks->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::Assign(TPersistent * Source)
{
  TBookmarkList * SourceList;
  SourceList = dynamic_cast<TBookmarkList *>(Source);
  if (SourceList)
  {
    Clear();
    for (int i = 0; i < SourceList->FBookmarks->Count; i++)
    {
      TBookmark * Bookmark = new TBookmark();
      Bookmark->Assign(dynamic_cast<TBookmark *>(SourceList->FBookmarks->Objects[i]));
      Add(Bookmark);
    }
    Modified = SourceList->Modified;
  }
  else
  {
    TPersistent::Assign(Source);
  }
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::Add(TBookmark * Bookmark)
{
  Insert(Count, Bookmark);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::InsertBefore(TBookmark * BeforeBookmark, TBookmark * Bookmark)
{
  assert(BeforeBookmark);
  int I = FBookmarks->IndexOf(BeforeBookmark->Key);
  assert(I >= 0);
  Insert(I, Bookmark);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::MoveBefore(TBookmark * BeforeBookmark, TBookmark * Bookmark)
{
  assert(BeforeBookmark);
  int NewIndex = FBookmarks->IndexOf(BeforeBookmark->Key);
  assert(Bookmark);
  int OldIndex = FBookmarks->IndexOf(Bookmark->Key);
  if (NewIndex > OldIndex)
  {
    NewIndex--;
  }
  FModified = true;
  FBookmarks->Move(OldIndex, NewIndex);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::MoveAtEnd(TBookmark * Bookmark)
{
  assert(Bookmark);
  int OldIndex = FBookmarks->IndexOf(Bookmark->Key);
  FModified = true;
  FBookmarks->Move(OldIndex, Count - 1);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::Insert(int Index, TBookmark * Bookmark)
{
  assert(Bookmark);
  assert(!Bookmark->FOwner);
  assert(!Bookmark->Name.IsEmpty());

  FModified = true;
  Bookmark->FOwner = this;
  if (FBookmarks->IndexOf(Bookmark->Key) >= 0)
  {
    throw Exception(FMTLOAD(DUPLICATE_BOOKMARK, (Bookmark->Name)));
  }
  FBookmarks->InsertObject(Index, Bookmark->Key, Bookmark);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::Delete(TBookmark * Bookmark)
{
  assert(Bookmark);
  assert(Bookmark->FOwner == this);
  int I = IndexOf(Bookmark);
  assert(I >= 0);
  FModified = true;
  Bookmark->FOwner = NULL;
  FBookmarks->Delete(I);
}
//---------------------------------------------------------------------------
int __fastcall TBookmarkList::IndexOf(TBookmark * Bookmark)
{
  return FBookmarks->IndexOf(Bookmark->Key);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::KeyChanged(int Index)
{
  assert(Index < Count);
  TBookmark * Bookmark = dynamic_cast<TBookmark *>(FBookmarks->Objects[Index]);
  assert(FBookmarks->Strings[Index] != Bookmark->Key);
  if (FBookmarks->IndexOf(Bookmark->Key) >= 0)
  {
    throw Exception(FMTLOAD(DUPLICATE_BOOKMARK, (Bookmark->Name)));
  }
  FBookmarks->Strings[Index] = Bookmark->Key;
}
//---------------------------------------------------------------------------
TBookmark * __fastcall TBookmarkList::FindByName(const AnsiString Node, const AnsiString Name)
{
  int I = FBookmarks->IndexOf(TBookmark::BookmarkKey(Node, Name));
  TBookmark * Bookmark = I >= 0 ? dynamic_cast<TBookmark *>(FBookmarks->Objects[I]) : NULL;
  assert(!Bookmark || (Bookmark->Node == Node && Bookmark->Name == Name));
  return Bookmark;
}
//---------------------------------------------------------------------------
int __fastcall TBookmarkList::GetCount()
{
  return FBookmarks->Count;
}
//---------------------------------------------------------------------------
TBookmark * __fastcall TBookmarkList::GetBookmarks(int Index)
{
  TBookmark * Bookmark = dynamic_cast<TBookmark *>(FBookmarks->Objects[Index]);
  assert(Bookmark);
  return Bookmark;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBookmark::TBookmark()
{
  FOwner = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TBookmark::Assign(TPersistent * Source)
{
  TBookmark * SourceBookmark;
  SourceBookmark = dynamic_cast<TBookmark *>(Source);
  if (SourceBookmark)
  {
    Name = SourceBookmark->Name;
    Local = SourceBookmark->Local;
    Remote = SourceBookmark->Remote;
    Node = SourceBookmark->Node;
  }
  else
  {
    TPersistent::Assign(Source);
  }
}
//---------------------------------------------------------------------------
void __fastcall TBookmark::SetName(const AnsiString value)
{
  if (Name != value)
  {
    int OldIndex = FOwner ? FOwner->IndexOf(this) : -1;
    AnsiString OldName = FName;
    FName = value;
    try
    {
      Modify(OldIndex);
    }
    catch(...)
    {
      FName = OldName;
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TBookmark::SetLocal(const AnsiString value)
{
  if (Local != value)
  {
    FLocal = value;
    Modify(-1);
  }
}
//---------------------------------------------------------------------------
void __fastcall TBookmark::SetRemote(const AnsiString value)
{
  if (Remote != value)
  {
    FRemote = value;
    Modify(-1);
  }
}
//---------------------------------------------------------------------------
void __fastcall TBookmark::SetNode(const AnsiString value)
{
  if (Node != value)
  {
    int OldIndex = FOwner ? FOwner->IndexOf(this) : -1;
    FNode = value;
    Modify(OldIndex);
  }
}
//---------------------------------------------------------------------------
void __fastcall TBookmark::Modify(int OldIndex)
{
  if (FOwner)
  {
    FOwner->Modified = true;
    if (OldIndex >= 0)
    {
      FOwner->KeyChanged(OldIndex);
    }
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TBookmark::BookmarkKey(const AnsiString Node, const AnsiString Name)
{
  return FORMAT("%s\1%s", (Node, Name));
}
//---------------------------------------------------------------------------
AnsiString __fastcall TBookmark::GetKey()
{
  return BookmarkKey(Node, Name);
}

