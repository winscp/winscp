//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <Common.h>
#include "NamedObjs.h"
#include "Bookmarks.h"
#include "Configuration.h"
#include "HierarchicalStorage.h"
#include "TextsCore.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TBookmarks::TBookmarks(): TObject()
{
  FSharedKey = TNamedObjectList::HiddenPrefix + "shared";
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
AnsiString TBookmarks::Keys[] = { "Local", "Remote", "ShortCuts", "Options" };
//---------------------------------------------------------------------------
void __fastcall TBookmarks::Load(THierarchicalStorage * Storage)
{
  for (int i = 0; i <= 3; i++)
  {
    if (Storage->OpenSubKey(Keys[i], false))
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
            if (i < 3)
            {
              LoadLevel(Storage, "", i, BookmarkList);
            }
            else
            {
              BookmarkList->LoadOptions(Storage);
            }
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

  ModifyAll(false);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarks::LoadLevel(THierarchicalStorage * Storage, const AnsiString Key,
  int Index, TBookmarkList * BookmarkList)
{
  TStrings * Names = new TStringList();
  try
  {
    Storage->GetValueNames(Names);
    AnsiString Name;
    AnsiString Directory;
    TShortCut ShortCut;
    for (int i = 0; i < Names->Count; i++)
    {
      Name = Names->Strings[i];
      bool IsDirectory = (Index == 0) || (Index == 1);
      if (IsDirectory)
      {
        Directory = Storage->ReadString(Name, "");
      }
      else
      {
        Directory = ""; // use only in cased of malformed config
        ShortCut = (TShortCut)Storage->ReadInteger(Name, 0);
      }
      TBookmark * Bookmark;
      if (IsNumber(Name))
      {
        assert(IsDirectory); // unless malformed
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
        switch (Index)
        {
          case 0:
            Bookmark->Local = Directory;
            break;

          case 1:
            Bookmark->Remote = Directory;
            break;

          case 2:
            Bookmark->ShortCut = ShortCut;
            break;
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
        LoadLevel(Storage, Key + (Key.IsEmpty() ? "" : "/") + Name, Index, BookmarkList);
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
void __fastcall TBookmarks::Save(THierarchicalStorage * Storage, bool All)
{
  for (int i = 0; i <= 3; i++)
  {
    if (Storage->OpenSubKey(Keys[i], true))
    {
      for (int Index = 0; Index < FBookmarkLists->Count; Index++)
      {
        TBookmarkList * BookmarkList = dynamic_cast<TBookmarkList *>(FBookmarkLists->Objects[Index]);
        if (All || BookmarkList->Modified)
        {
          AnsiString Key;
          Key = FBookmarkLists->Strings[Index];
          Storage->RecursiveDeleteSubKey(Key);
          if (Storage->OpenSubKey(Key, true))
          {
            if (i < 3)
            {
              for (int IndexB = 0; IndexB < BookmarkList->Count; IndexB++)
              {
                TBookmark * Bookmark = BookmarkList->Bookmarks[IndexB];
                // avoid creating empty subfolder if there's no shortcut
                if ((i == 0) || (i == 1) ||
                    ((i == 2) && (Bookmark->ShortCut != 0)))
                {
                  bool HasNode = !Bookmark->Node.IsEmpty();
                  if (!HasNode || Storage->OpenSubKey(Bookmark->Node, true))
                  {
                    switch (i)
                    {
                      case 0:
                        Storage->WriteString(Bookmark->Name, Bookmark->Local);
                        break;

                      case 1:
                        Storage->WriteString(Bookmark->Name, Bookmark->Remote);
                        break;

                      case 2:
                        assert(Bookmark->ShortCut != 0);
                        Storage->WriteInteger(Bookmark->Name, Bookmark->ShortCut);
                        break;
                    }

                    if (HasNode)
                    {
                      Storage->CloseSubKey();
                    }
                  }
                }
              }
            }
            else
            {
              BookmarkList->SaveOptions(Storage);
            }
            Storage->CloseSubKey();
          }
        }
      }
      Storage->CloseSubKey();
    }
  }

  if (!All)
  {
    ModifyAll(false);
  }
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
TBookmarkList * __fastcall TBookmarks::GetSharedBookmarks()
{
  return GetBookmarks(FSharedKey);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarks::SetSharedBookmarks(TBookmarkList * value)
{
  SetBookmarks(FSharedKey, value);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBookmarkList::TBookmarkList(): TPersistent()
{
  FModified = false;
  FBookmarks = new TStringList();
  FBookmarks->CaseSensitive = false;
  FOpenedNodes = new TStringList();
  FOpenedNodes->CaseSensitive = false;
  FOpenedNodes->Sorted = true;
}
//---------------------------------------------------------------------------
__fastcall TBookmarkList::~TBookmarkList()
{
  Clear();
  SAFE_DESTROY(FBookmarks);
  SAFE_DESTROY(FOpenedNodes);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::Clear()
{
  for (int i = 0; i < FBookmarks->Count; i++)
  {
    delete FBookmarks->Objects[i];
  }
  FBookmarks->Clear();
  FOpenedNodes->Clear();
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
    FOpenedNodes->Assign(SourceList->FOpenedNodes);
    Modified = SourceList->Modified;
  }
  else
  {
    TPersistent::Assign(Source);
  }
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::LoadOptions(THierarchicalStorage * Storage)
{
  FOpenedNodes->CommaText = Storage->ReadString("OpenedNodes", "");
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::SaveOptions(THierarchicalStorage * Storage)
{
  Storage->WriteString("OpenedNodes", FOpenedNodes->CommaText);
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
void __fastcall TBookmarkList::MoveTo(TBookmark * ToBookmark,
  TBookmark * Bookmark, bool Before)
{
  assert(ToBookmark != NULL);
  int NewIndex = FBookmarks->IndexOf(ToBookmark->Key);
  assert(Bookmark != NULL);
  int OldIndex = FBookmarks->IndexOf(Bookmark->Key);
  if (Before && (NewIndex > OldIndex))
  {
    // otherwise item is moved after the item in the target index
    NewIndex--;
  }
  else if (!Before && (NewIndex < OldIndex))
  {
    NewIndex++;
  }
  FModified = true;
  FBookmarks->Move(OldIndex, NewIndex);
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
  delete Bookmark;
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
TBookmark * __fastcall TBookmarkList::FindByShortCut(TShortCut ShortCut)
{
  for (int Index = 0; Index < FBookmarks->Count; Index++)
  {
    if (Bookmarks[Index]->ShortCut == ShortCut)
    {
      return Bookmarks[Index];
    }
  }
  return NULL;
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
bool __fastcall TBookmarkList::GetNodeOpened(AnsiString Index)
{
  return (FOpenedNodes->IndexOf(Index) >= 0);
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::SetNodeOpened(AnsiString Index, bool value)
{
  int I = FOpenedNodes->IndexOf(Index);
  if ((I >= 0) != value)
  {
    if (value)
    {
      FOpenedNodes->Add(Index);
    }
    else
    {
      FOpenedNodes->Delete(I);
    }
    FModified = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TBookmarkList::ShortCuts(TShortCuts & ShortCuts)
{
  for (int Index = 0; Index < Count; Index++)
  {
    TBookmark * Bookmark = Bookmarks[Index];
    if (Bookmark->ShortCut != 0)
    {
      ShortCuts.Add(Bookmark->ShortCut);
    }
  }
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
    ShortCut = SourceBookmark->ShortCut;
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
void __fastcall TBookmark::SetShortCut(TShortCut value)
{
  if (ShortCut != value)
  {
    FShortCut = value;
    Modify(-1);
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
