//---------------------------------------------------------------------------
#ifndef BookmarksH
#define BookmarksH
//---------------------------------------------------------------------------
class THierarchicalStorage;
class TBookmarkList;
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

  __property TBookmarkList * Bookmarks[AnsiString Index] = { read = GetBookmarks, write = SetBookmarks };
  __property TBookmarkList * SharedBookmarks = { read = GetSharedBookmarks, write = SetSharedBookmarks };

private:
  TStringList * FBookmarkLists;
  AnsiString FSharedKey;
  static AnsiString Keys[];

  TBookmarkList * __fastcall GetBookmarks(AnsiString Index);
  void __fastcall SetBookmarks(AnsiString Index, TBookmarkList * value);
  TBookmarkList * __fastcall GetSharedBookmarks();
  void __fastcall SetSharedBookmarks(TBookmarkList * value);
  void __fastcall LoadLevel(THierarchicalStorage * Storage, const AnsiString Key,
    bool Local, TBookmarkList * BookmarkList);
};
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
  TBookmark * __fastcall FindByName(const AnsiString Node, const AnsiString Name);
  virtual void __fastcall Assign(TPersistent * Source);
  void __fastcall LoadOptions(THierarchicalStorage * Storage);
  void __fastcall SaveOptions(THierarchicalStorage * Storage);

  __property int Count = { read = GetCount };
  __property TBookmark * Bookmarks[int Index] = { read = GetBookmarks };
  __property bool NodeOpened[AnsiString Index] = { read = GetNodeOpened, write = SetNodeOpened };

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
  bool __fastcall GetNodeOpened(AnsiString Index);
  void __fastcall SetNodeOpened(AnsiString Index, bool value);
};
//---------------------------------------------------------------------------
class TBookmark : public TPersistent
{
friend class TBookmarkList;
public:
  __fastcall TBookmark();

  virtual void __fastcall Assign(TPersistent * Source);

  __property AnsiString Name = { read = FName, write = SetName };
  __property AnsiString Local = { read = FLocal, write = SetLocal };
  __property AnsiString Remote = { read = FRemote, write = SetRemote };
  __property AnsiString Node = { read = FNode, write = SetNode };

protected:
  TBookmarkList * FOwner;

  static AnsiString __fastcall BookmarkKey(const AnsiString Node, const AnsiString Name);
  __property AnsiString Key = { read = GetKey };

private:
  AnsiString FName;
  AnsiString FLocal;
  AnsiString FRemote;
  AnsiString FNode;

  void __fastcall SetName(const AnsiString value);
  void __fastcall SetLocal(const AnsiString value);
  void __fastcall SetRemote(const AnsiString value);
  void __fastcall SetNode(const AnsiString value);
  AnsiString __fastcall GetKey();
  void __fastcall Modify(int OldIndex);
};
//---------------------------------------------------------------------------
#endif
