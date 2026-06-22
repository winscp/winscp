//---------------------------------------------------------------------------
#ifndef BookmarksH
#define BookmarksH
//---------------------------------------------------------------------------
#include <CopyParam.h>
//---------------------------------------------------------------------------
class THierarchicalStorage;
class TBookmarkList;
class TShortCuts;
//---------------------------------------------------------------------------
class TBookmarks : public TObject
{
public:
  __fastcall TBookmarks();
  virtual __fastcall ~TBookmarks();

  void __fastcall Load(THierarchicalStorage * Storage);
  void __fastcall Save(THierarchicalStorage * Storage, bool All);
  void __fastcall ModifyAll(bool Modify);
  void __fastcall Clear();

  __property TBookmarkList * Bookmarks[UnicodeString Index] = { read = GetBookmarks, write = SetBookmarks };
  __property TBookmarkList * SharedBookmarks = { read = GetSharedBookmarks, write = SetSharedBookmarks };

private:
  TStringList * FBookmarkLists;
  UnicodeString FSharedKey;
  static UnicodeString Keys[];

  TBookmarkList * __fastcall GetBookmarks(UnicodeString Index);
  void __fastcall SetBookmarks(UnicodeString Index, TBookmarkList * value);
  TBookmarkList * __fastcall GetSharedBookmarks();
  void __fastcall SetSharedBookmarks(TBookmarkList * value);
  void __fastcall LoadLevel(THierarchicalStorage * Storage, const UnicodeString Key,
    int Index, TBookmarkList * BookmarkList);
};
//---------------------------------------------------------------------------
class TBookmark;
//---------------------------------------------------------------------------
class TBookmarkList : public TPersistent
{
friend class TBookmarks;
friend class TBookmark;
public:
  __fastcall TBookmarkList();
  virtual __fastcall ~TBookmarkList();

  void __fastcall Clear();
  void __fastcall Add(TBookmark * Bookmark);
  void __fastcall Insert(int Index, TBookmark * Bookmark);
  void __fastcall InsertBefore(TBookmark * BeforeBookmark, TBookmark * Bookmark);
  void __fastcall MoveTo(TBookmark * ToBookmark, TBookmark * Bookmark, bool Before);
  void __fastcall Delete(TBookmark * Bookmark);
  TBookmark * __fastcall FindByName(const UnicodeString Node, const UnicodeString Name);
  TBookmark * __fastcall FindByShortCut(TShortCut ShortCut);
  virtual void __fastcall Assign(TPersistent * Source);
  void __fastcall LoadOptions(THierarchicalStorage * Storage);
  void __fastcall SaveOptions(THierarchicalStorage * Storage);
  void __fastcall ShortCuts(TShortCuts & ShortCuts);

  __property int Count = { read = GetCount };
  __property TBookmark * Bookmarks[int Index] = { read = GetBookmarks };
  __property bool NodeOpened[UnicodeString Index] = { read = GetNodeOpened, write = SetNodeOpened };

protected:
  int __fastcall IndexOf(TBookmark * Bookmark);
  void __fastcall KeyChanged(int Index);

  __property bool Modified = { read = FModified, write = FModified };

private:
  TStringList * FBookmarks;
  TStringList * FOpenedNodes;
  bool FModified;

  int __fastcall GetCount();
  TBookmark * __fastcall GetBookmarks(int Index);
  bool __fastcall GetNodeOpened(UnicodeString Index);
  void __fastcall SetNodeOpened(UnicodeString Index, bool value);
};
//---------------------------------------------------------------------------
class TBookmark : public TPersistent
{
friend class TBookmarkList;
public:
  __fastcall TBookmark();

  virtual void __fastcall Assign(TPersistent * Source);

  UnicodeString __fastcall GetSideDirectory(TOperationSide Side);

  __property UnicodeString Name = { read = FName, write = SetName };
  __property UnicodeString Local = { read = FLocal, write = SetLocal };
  __property UnicodeString Remote = { read = FRemote, write = SetRemote };
  __property UnicodeString Node = { read = FNode, write = SetNode };
  __property TShortCut ShortCut = { read = FShortCut, write = SetShortCut };

protected:
  TBookmarkList * FOwner;

  static UnicodeString __fastcall BookmarkKey(const UnicodeString Node, const UnicodeString Name);
  __property UnicodeString Key = { read = GetKey };

private:
  UnicodeString FName;
  UnicodeString FLocal;
  UnicodeString FRemote;
  UnicodeString FNode;
  TShortCut FShortCut;

  void __fastcall SetName(const UnicodeString value);
  void __fastcall SetLocal(const UnicodeString value);
  void __fastcall SetRemote(const UnicodeString value);
  void __fastcall SetNode(const UnicodeString value);
  void __fastcall SetShortCut(TShortCut value);
  UnicodeString __fastcall GetKey();
  void __fastcall Modify(int OldIndex);
};
//---------------------------------------------------------------------------
#endif
