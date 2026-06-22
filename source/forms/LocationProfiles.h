//----------------------------------------------------------------------------
#ifndef LocationProfilesH
#define LocationProfilesH
//----------------------------------------------------------------------------
#include "HistoryComboBox.hpp"
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.StdCtrls.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include "PngImageList.hpp"
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.pngimage.hpp>
#include <Vcl.ExtCtrls.hpp>

#include <FileOperationProgress.h>
#include <Terminal.h>
#include <WinInterface.h>
#include <Bookmarks.h>
#include <PasTools.hpp>
#include <GUITools.h>
#include <System.ImageList.hpp>
//----------------------------------------------------------------------------
class TLocationProfilesDialog : public TForm
{
__published:
  TButton *OKBtn;
  TButton *CancelBtn;
  TPageControl *PageControl;
  TTreeView *SessionProfilesView;
  TButton *AddSessionBookmarkButton;
  TButton *RemoveSessionBookmarkButton;
  TButton *DownSessionBookmarkButton;
  TButton *UpSessionBookmarkButton;
  TLabel *LocalDirectoryLabel;
  THistoryComboBox *RemoteDirectoryEdit;
  TButton *RenameSessionBookmarkButton;
  TLabel *RemoteDirectoryLabel;
  TButton *SessionBookmarkMoveToButton;
  TPngImageList *BookmarkImageList;
  THistoryComboBox *LocalDirectoryEdit;
  TButton *LocalDirectoryBrowseButton;
  TButton *SwitchButton;
  TButton *HelpButton;
  TTabSheet *SessionProfilesSheet;
  TTabSheet *SharedProfilesSheet;
  TTreeView *SharedProfilesView;
  TButton *AddSharedBookmarkButton;
  TButton *RemoveSharedBookmarkButton;
  TButton *RenameSharedBookmarkButton;
  TButton *SharedBookmarkMoveToButton;
  TButton *UpSharedBookmarkButton;
  TButton *DownSharedBookmarkButton;
  TButton *ShortCutSharedBookmarkButton;
  TImage *Image;
  TPngImageList *BookmarkImageList120;
  TPngImageList *BookmarkImageList144;
  TPngImageList *BookmarkImageList192;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall AddBookmarkButtonClick(TObject *Sender);
  void __fastcall RemoveBookmarkButtonClick(TObject *Sender);
  void __fastcall BookmarkButtonClick(TObject *Sender);
  void __fastcall ProfilesViewStartDrag(TObject *Sender,
          TDragObject *&DragObject);
  void __fastcall ProfilesViewDragOver(TObject *Sender, TObject *Source,
          int X, int Y, TDragState State, bool &Accept);
  void __fastcall ProfilesViewDragDrop(TObject *Sender, TObject *Source,
          int X, int Y);
  void __fastcall ProfilesViewDblClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall ProfilesViewKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall DirectoryEditChange(TObject *Sender);
  void __fastcall ProfilesViewChange(TObject *Sender, TTreeNode *Node);
  void __fastcall BookmarkMoveToButtonClick(TObject *Sender);
  void __fastcall RenameBookmarkButtonClick(TObject *Sender);
  void __fastcall ProfilesViewGetImageIndex(TObject *Sender,
          TTreeNode *Node);
  void __fastcall ProfilesViewGetSelectedIndex(TObject *Sender,
          TTreeNode *Node);
  void __fastcall LocalDirectoryBrowseButtonClick(TObject *Sender);
  void __fastcall SwitchButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall ProfilesViewCollapsed(TObject *Sender, TTreeNode *Node);
  void __fastcall ProfilesViewExpanded(TObject *Sender, TTreeNode *Node);
  void __fastcall ProfilesViewEdited(TObject * Sender, TTreeNode * Node,
    UnicodeString & S);
  void __fastcall ProfilesViewEditing(TObject * Sender, TTreeNode * Node,
    bool & AllowEdit);
  void __fastcall ProfilesViewEndDrag(TObject *Sender, TObject *Target,
          int X, int Y);
  void __fastcall ShortCutBookmarkButtonClick(TObject *Sender);

public:
  __fastcall TLocationProfilesDialog(TComponent* AOwner);
  __fastcall ~TLocationProfilesDialog();

  bool __fastcall Execute();

  __property UnicodeString LocalDirectory = { read = GetLocalDirectory, write = SetLocalDirectory };
  __property UnicodeString RemoteDirectory = { read = GetRemoteDirectory, write = SetRemoteDirectory };
  __property TTerminal * Terminal = { read = FTerminal, write = FTerminal };
  __property TOperationSide OperationSide = { read = FOperationSide, write = FOperationSide };
  __property TStrings * RemoteDirectories  = { read=GetRemoteDirectories, write=SetRemoteDirectories };
  __property TStrings * LocalDirectories  = { read=GetLocalDirectories, write=SetLocalDirectories };
  __property TOpenDirectoryMode Mode = { read = FMode, write = FMode };

protected:
  void __fastcall BookmarkMove(TObject * Sender, TTreeNode * Source, TTreeNode * Dest);
  void __fastcall UpdateControls();
  bool __fastcall AddAsBookmark(TObject * Sender, bool Initial);
  virtual void __fastcall UpdateActions();

private:
  TOpenDirectoryMode FMode;
  TTerminal * FTerminal;
  TOperationSide FOperationSide;
  TStringList * FSessionFolders;
  TStringList * FSharedFolders;
  bool FChanging;
  TTreeNode * FBookmarkDragSource;
  TBookmarkList * FSessionBookmarkList;
  TBookmarkList * FSharedBookmarkList;
  UnicodeString FLocalDirectory;
  UnicodeString FRemoteDirectory;
  TTreeViewScrollOnDragOver * FSessionScrollOnDragOver;
  TTreeViewScrollOnDragOver * FSharedScrollOnDragOver;
  UnicodeString FSessionKey;
  bool FBookmarkSelected;
  #ifdef _DEBUG
  HWND FSessionProfilesViewHandle;
  HWND FSharedProfilesViewHandle;
  #endif

  void __fastcall SetLocalDirectory(UnicodeString value);
  UnicodeString __fastcall GetLocalDirectory();
  void __fastcall SetRemoteDirectory(UnicodeString value);
  UnicodeString __fastcall GetRemoteDirectory();
  void __fastcall SetOperationSide(TOperationSide value);
  void __fastcall FindProfile();
  void __fastcall SetRemoteDirectories(TStrings * value);
  TStrings * __fastcall GetRemoteDirectories();
  void __fastcall SetLocalDirectories(TStrings * value);
  TStrings * __fastcall GetLocalDirectories();
  void __fastcall FindProfile(TTreeView * ProfilesView);
  void __fastcall UpdateProfilesControls(
    TTreeView * ProfilesView,
    TButton * AddBookmarkButton, TButton * RemoveBookmarkButton,
    TButton * RenameBookmarkButton,  TButton * BookmarkMoveToButton,
    TButton * ShortCutBookmarkButton,
    TButton * UpBookmarkButton, TButton * DownBookmarkButton);
  TBookmarkList * GetBookmarkList(TObject * Sender);
  TStringList * GetFolders(TObject * Sender);
  TTreeView * GetProfilesView(TObject * Sender);
  TTreeViewScrollOnDragOver * GetScrollOnDragOver(TObject * Sender);
  void __fastcall RenameBookmark(TObject * Sender);
  void __fastcall RemoveBookmark(TObject * Sender);
  TTabSheet * GetProfilesSheet();
  void __fastcall LoadBookmarks(
    TTreeView * ProfilesView, TStringList * Folders, TBookmarkList * BookmarkList,
    TBookmarkList * Source);
  bool __fastcall ProfileMatch(TTreeNode * Node);
  UnicodeString __fastcall BookmarkText(TBookmark * Bookmark);

  INTERFACE_HOOK;
};
//----------------------------------------------------------------------------
#endif
