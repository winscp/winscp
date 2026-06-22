//----------------------------------------------------------------------------
#ifndef OpenDirectoryH
#define OpenDirectoryH
//----------------------------------------------------------------------------
#include "HistoryComboBox.h"
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>

#include <FileOperationProgress.h>
#include <Terminal.h>
#include <WinInterface.h>
#include <Bookmarks.h>
#include <PasTools.hpp>
#include <ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
//----------------------------------------------------------------------------
class TOpenDirectoryDialog : public TForm
{
__published:
  TButton *OKBtn;
  TButton *CancelBtn;
  THistoryComboBox *RemoteDirectoryEdit;
  THistoryComboBox *LocalDirectoryEdit;
  TLabel *EditLabel;
  TPageControl *PageControl;
  TButton *LocalDirectoryBrowseButton;
  TButton *SwitchButton;
  TButton *HelpButton;
  TTabSheet *SessionBookmarksSheet;
  TListBox *SessionBookmarksList;
  TButton *AddSessionBookmarkButton;
  TButton *RemoveSessionBookmarkButton;
  TButton *UpSessionBookmarkButton;
  TButton *DownSessionBookmarkButton;
  TListBox *SharedBookmarksList;
  TButton *AddSharedBookmarkButton;
  TButton *RemoveSharedBookmarkButton;
  TButton *UpSharedBookmarkButton;
  TButton *DownSharedBookmarkButton;
  TTabSheet *SharedBookmarksSheet;
  TButton *ShortCutSharedBookmarkButton;
  TImage *Image;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall AddBookmarkButtonClick(TObject *Sender);
  void __fastcall RemoveBookmarkButtonClick(TObject *Sender);
  void __fastcall BookmarksListClick(TObject *Sender);
  void __fastcall BookmarkButtonClick(TObject *Sender);
  void __fastcall BookmarksListStartDrag(TObject *Sender,
          TDragObject *&DragObject);
  void __fastcall BookmarksListDragOver(TObject *Sender, TObject *Source,
          int X, int Y, TDragState State, bool &Accept);
  void __fastcall BookmarksListDragDrop(TObject *Sender, TObject *Source,
          int X, int Y);
  void __fastcall DirectoryEditChange(TObject *Sender);
  void __fastcall BookmarksListDblClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall BookmarksListKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall LocalDirectoryBrowseButtonClick(TObject *Sender);
  void __fastcall SwitchButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall BookmarksListEndDrag(TObject *Sender, TObject *Target,
          int X, int Y);
  void __fastcall ShortCutBookmarkButtonClick(TObject *Sender);
  void __fastcall PageControlChange(TObject *Sender);

public:
  __fastcall TOpenDirectoryDialog(TComponent* AOwner);
  virtual __fastcall ~TOpenDirectoryDialog();

  bool __fastcall Execute();

  __property UnicodeString Directory = { read = GetDirectory, write = SetDirectory };
  __property TOperationSide OperationSide = { read = FOperationSide, write = SetOperationSide };
  __property TStrings * Directories  = { read=GetDirectories, write=SetDirectories };
  __property TOpenDirectoryMode Mode = { read = FMode, write = SetMode };
  __property TTerminal * Terminal = { read = FTerminal, write = FTerminal };
  __property bool AllowSwitch = { read = FAllowSwitch, write = FAllowSwitch };

protected:
  bool __fastcall AllowBookmarkDrag(TObject * Sender, int X, int Y);
  void __fastcall BookmarkMove(TObject * Sender, int Source, int Dest);
  Integer __fastcall FindBookmark(TListBox * BookmarksList, const UnicodeString Bookmark);
  void __fastcall UpdateControls(bool ListBoxUpdate = false);
  void __fastcall AddAsBookmark(TObject * Sender);
  void __fastcall RemoveBookmark(TObject * Sender);
  __property TWinControl * CurrentEdit = { read = GetCurrentEdit };

private:
  TOperationSide FOperationSide;
  TTerminal * FTerminal;
  int FBookmarkDragSource, FBookmarkDragDest;
  TOpenDirectoryMode FMode;
  TBookmarkList * FSessionBookmarkList;
  TBookmarkList * FSharedBookmarkList;
  bool FAllowSwitch;
  TListBoxScrollOnDragOver * FSessionScrollOnDragOver;
  TListBoxScrollOnDragOver * FSharedScrollOnDragOver;
  bool FBookmarkSelected;

  void __fastcall SetDirectory(UnicodeString value);
  UnicodeString __fastcall GetDirectory();
  TWinControl * __fastcall GetCurrentEdit();
  void __fastcall SetOperationSide(TOperationSide value);
  void __fastcall SetDirectories(TStrings * value);
  TStrings * __fastcall GetDirectories();
  void __fastcall SetMode(TOpenDirectoryMode value);
  void __fastcall LoadBookmarks(TListBox * ListBox,
    TBookmarkList * BookmarkList, TBookmarkList * Source);
  TListBox * GetBookmarksList(TObject * Sender);
  TBookmarkList * GetBookmarkList(TObject * Sender);
  TListBoxScrollOnDragOver * GetScrollOnDragOver(TObject * Sender);
  void __fastcall SelectBookmark(TListBox * BookmarksList);
  void __fastcall UpdateBookmarkControls(
    TButton * AddBookmarkButton, TButton * RemoveBookmarkButton,
    TButton * ShortCutBookmarkButton,
    TButton * UpBookmarkButton, TButton * DownBookmarkButton,
    TListBox * BookmarksList, bool ListBoxUpdate);
  void __fastcall BookmarkSelected(TObject * Sender);
  UnicodeString __fastcall BookmarkDirectory(TBookmark * Bookmark);
  UnicodeString __fastcall BookmarkText(TBookmark * Bookmark);
  inline TBookmark * __fastcall GetBookmark(TListBox * BookmarksList, int Index);

  INTERFACE_HOOK
};
//----------------------------------------------------------------------------
#endif
