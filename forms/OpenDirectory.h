//----------------------------------------------------------------------------
#ifndef OpenDirectoryH
#define OpenDirectoryH
//----------------------------------------------------------------------------
#include <vcl\System.hpp>
#include <vcl\Windows.hpp>
#include <vcl\SysUtils.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\ExtCtrls.hpp>
#include <Mask.hpp>
#include <ComboEdit.hpp>

#include <FileOperationProgress.h>
#include <Terminal.h>
#include <WinInterface.h>
#include <Bookmarks.h>
#include "IEComboBox.hpp"
//----------------------------------------------------------------------------
class TOpenDirectoryDialog : public TForm
{
__published:
  TButton *OKBtn;
  TButton *CancelBtn;
  TIEComboBox *RemoteDirectoryEdit;
  TIEComboBox *LocalDirectoryEdit;
  TLabel *EditLabel;
  TGroupBox *BookmarksGroup;
  TListBox *BookmarksList;
  TButton *AddBookmarkButton;
  TButton *RemoveBookmarkButton;
  TButton *DownBookmarkButton;
  TButton *UpBookmarkButton;
  TLabel *Label1;
  TButton *LocalDirectoryBrowseButton;
  TButton *SwitchButton;
  TButton *HelpButton;
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

public:
  __fastcall TOpenDirectoryDialog(TComponent* AOwner);
  virtual __fastcall ~TOpenDirectoryDialog();

  bool __fastcall Execute();

  __property AnsiString Directory = { read = GetDirectory, write = SetDirectory };
  __property TOperationSide OperationSide = { read = FOperationSide, write = SetOperationSide };
  __property TStrings * Directories  = { read=GetDirectories, write=SetDirectories };
  __property TOpenDirectoryMode Mode = { read = FMode, write = SetMode };
  __property TTerminal * Terminal = { read = FTerminal, write = FTerminal };
  __property bool AllowSwitch = { read = FAllowSwitch, write = FAllowSwitch };

protected:
  bool __fastcall AllowBookmarkDrag(int X, int Y);
  void __fastcall BookmarkMove(int Source, int Dest);
  Integer __fastcall FindBookmark(const AnsiString Bookmark);
  void __fastcall UpdateControls(bool ListBoxUpdate = false);
  void __fastcall AddAsBookmark();
  __property TWinControl * CurrentEdit = { read = GetCurrentEdit };

private:
  TOperationSide FOperationSide;
  TTerminal * FTerminal;
  int FBookmarkDragSource, FBookmarkDragDest;
  TOpenDirectoryMode FMode;
  TBookmarkList * FBookmarkList;
  bool FAllowSwitch;

  void __fastcall SetDirectory(AnsiString value);
  AnsiString __fastcall GetDirectory();
  TWinControl * __fastcall GetCurrentEdit();
  void __fastcall SetOperationSide(TOperationSide value);
  void __fastcall SetDirectories(TStrings * value);
  TStrings * __fastcall GetDirectories();
  void __fastcall SetMode(TOpenDirectoryMode value);
  void __fastcall LoadBookmarks();
};
//----------------------------------------------------------------------------
#endif
