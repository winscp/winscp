//----------------------------------------------------------------------------
#ifndef SynchronizeChecklistH
#define SynchronizeChecklistH
//----------------------------------------------------------------------------
#include "IEListView.hpp"
#include "NortonLikeListView.hpp"
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.StdCtrls.hpp>
#include "PngImageList.hpp"
//----------------------------------------------------------------------------
#include <Terminal.h>
#include <System.Actions.hpp>
#include <Vcl.ActnList.hpp>
#include <System.ImageList.hpp>
//----------------------------------------------------------------------------
class TSynchronizeChecklistDialog : public TForm
{
__published:
  TPanel * Panel;
  TIEListView *ListView;
  TStatusBar *StatusBar;
  TPngImageList *ActionImages;
  TButton *OkButton;
  TButton *CancelButton;
  TButton *CheckAllButton;
  TButton *UncheckAllButton;
  TButton *CheckButton;
  TButton *UncheckButton;
  TPopupMenu *ListViewPopupMenu;
  TMenuItem *CheckItem;
  TMenuItem *UncheckItem;
  TMenuItem *N1;
  TMenuItem *SelectAllItem;
  TTimer *UpdateTimer;
  TButton *HelpButton;
  TButton *CustomCommandsButton2;
  TActionList *ActionList;
  TAction *CheckAction;
  TAction *UncheckAction;
  TAction *CheckAllAction;
  TAction *UncheckAllAction;
  TAction *SelectAllAction;
  TAction *CustomCommandsAction;
  TMenuItem *N2;
  TButton *ReverseButton;
  TAction *ReverseAction;
  TMenuItem *ReverseItem;
  TPngImageList *ActionImages120;
  TPngImageList *ActionImages144;
  TPngImageList *ActionImages192;
  TAction *CalculateSizeAction;
  TMenuItem *Calculate1;
  TButton *ToolsMenuButton;
  TAction *CalculateSizeAllAction;
  TAction *MoveAction;
  TButton *MoveButton;
  TMenuItem *MoveItem;
  TAction *CheckDirectoryAction;
  TAction *UncheckDirectoryAction;
  TMenuItem *N3;
  TMenuItem *CheckAllFilesinThisDirectory1;
  TMenuItem *UncheckAllActionsinThisDirectory1;
  TAction *BrowseLocalAction;
  TAction *BrowseRemoteAction;
  TMenuItem *BrowseLocalDirectory1;
  TMenuItem *BrowseLocalDirectory2;
  TAction *FindMoveCandidateAction;
  TMenuItem *Calculate3;
  TMenuItem *CalculateAll1;
  TPopupMenu *ToolsPopupMenu;
  TMenuItem *Calculate2;
  TMenuItem *CalculateAll2;
  TMenuItem *N4;
  TMenuItem *FindMoveCandidate1;
  TPopupMenu *OkPopupMenu;
  TMenuItem *StartItem;
  TMenuItem *StartQueueItem;
  void __fastcall HelpButtonClick(TObject * Sender);
  void __fastcall FormShow(TObject * Sender);
  void __fastcall StatusBarDrawPanel(TStatusBar *StatusBar,
          TStatusPanel *Panel, const TRect &Rect);
  void __fastcall ListViewChange(TObject *Sender, TListItem *Item,
          TItemChange Change);
  void __fastcall ListViewChanging(TObject *Sender, TListItem *Item,
          TItemChange Change, bool &AllowChange);
  void __fastcall CheckAllActionExecute(TObject *Sender);
  void __fastcall CheckActionExecute(TObject *Sender);
  void __fastcall ListViewSelectItem(TObject *Sender, TListItem *Item,
          bool Selected);
  void __fastcall UpdateTimerTimer(TObject *Sender);
  void __fastcall SelectAllActionExecute(TObject *Sender);
  void __fastcall StatusBarMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
  void __fastcall ListViewCompare(TObject *Sender, TListItem *Item1,
          TListItem *Item2, int Data, int &Compare);
  void __fastcall ListViewSecondaryColumnHeader(TCustomIEListView *Sender,
          int Index, int &SecondaryColumn);
  void __fastcall ListViewContextPopup(TObject *Sender, TPoint &MousePos,
          bool &Handled);
  void __fastcall CustomCommandsActionExecute(TObject *Sender);
  void __fastcall ListViewAdvancedCustomDrawSubItem(TCustomListView *Sender, TListItem *Item,
          int SubItem, TCustomDrawState State, TCustomDrawStage Stage, bool &DefaultDraw);
  void __fastcall StatusBarResize(TObject *Sender);
  void __fastcall UncheckActionExecute(TObject *Sender);
  void __fastcall UncheckAllActionExecute(TObject *Sender);
  void __fastcall ReverseActionExecute(TObject *Sender);
  void __fastcall ListViewClick(TObject *Sender);
  void __fastcall OkButtonClick(TObject *Sender);
  void __fastcall CalculateSizeActionExecute(TObject *Sender);
  void __fastcall CalculateSizeAllActionExecute(TObject *Sender);
  void __fastcall MoveActionExecute(TObject *Sender);
  void __fastcall CheckDirectoryActionExecute(TObject *Sender);
  void __fastcall UncheckDirectoryActionExecute(TObject *Sender);
  void __fastcall BrowseLocalActionExecute(TObject *Sender);
  void __fastcall BrowseRemoteActionExecute(TObject *Sender);
  void __fastcall ListViewRecreate(TObject *Sender);
  void __fastcall ToolsMenuButtonClick(TObject *Sender);
  void __fastcall FindMoveCandidateActionExecute(TObject *Sender);
  void __fastcall FormAfterMonitorDpiChanged(TObject *Sender, int OldDPI, int NewDPI);
  void __fastcall StartItemClick(TObject *Sender);
  void __fastcall OkButtonDropDownClick(TObject *Sender);
  void __fastcall StartQueueItemClick(TObject *Sender);

public:
  __fastcall TSynchronizeChecklistDialog(
    TComponent * AOwner, TSynchronizeMode Mode, int Params,
    const UnicodeString & LocalDirectory, const UnicodeString & RemoteDirectory,
    TCustomCommandMenuEvent OnCustomCommandMenu, TFullSynchronizeEvent OnSynchronize,
    TQueueSynchronizeEvent OnQueueSynchronize,
    TSynchronizeChecklistCalculateSize OnSynchronizeChecklistCalculateSize, TSynchronizeMoveEvent OnSynchronizeMove,
    TSynchronizeBrowseEvent OnSynchronizeBrowse, void * Token);
  virtual __fastcall ~TSynchronizeChecklistDialog();

  bool __fastcall Execute(TSynchronizeChecklist * Checklist);

protected:
  bool FFormRestored;
  TSynchronizeChecklist * FChecklist;
  TSynchronizeMode FMode;
  int FParams;
  UnicodeString FLocalDirectory;
  UnicodeString FRemoteDirectory;
  TWndMethod FOrigListViewWindowProc;
  TWndMethod FOrigStatusBarWindowProc;
  int FTotals[1 + TSynchronizeChecklist::ActionCount];
  int FChecked[1 + TSynchronizeChecklist::ActionCount];
  __int64 FCheckedSize[1 + TSynchronizeChecklist::ActionCount];
  TListItem * FChangingItem;
  bool FChangingItemChecked;
  bool FChangingItemIgnore;
  bool FChangingItemMass;
  TCustomCommandMenuEvent FOnCustomCommandMenu;
  TSynchronizeChecklistCalculateSize FOnSynchronizeChecklistCalculateSize;
  TSynchronizeMoveEvent FOnSynchronizeMove;
  TSynchronizeBrowseEvent FOnSynchronizeBrowse;
  typedef std::map<const TSynchronizeChecklist::TItem *, TSynchronizeChecklist::TAction> TActions;
  TActions FActions;
  TFullSynchronizeEvent FOnSynchronize;
  TQueueSynchronizeEvent FOnQueueSynchronize;
  void * FToken;
  bool FSynchronizing;
  std::unique_ptr<Exception> FException;
  std::map<const TSynchronizeChecklist::TItem *, TListItem *> FChecklistToListViewMap;
  int FDirectories;
  int FMoveCandidatesValidForSort;
  typedef std::vector<const TSynchronizeChecklist::TItem *> TChecklistItems;
  typedef std::map<UnicodeString, TChecklistItems> TMoveCandidatesFileNameMap;
  TMoveCandidatesFileNameMap FMoveCandidatesFileName;
  typedef std::map<__int64, TChecklistItems> TMoveCandidatesSizeMap;
  TMoveCandidatesSizeMap FMoveCandidatesSize;

  void __fastcall UpdateControls();
  void __fastcall UpdateCaption();
  virtual void __fastcall CreateParams(TCreateParams & Params);
  void __fastcall LoadItem(TListItem * Item);
  void __fastcall LoadList();
  void __fastcall ListViewWindowProc(TMessage & Message);
  void __fastcall StatusBarWindowProc(TMessage & Message);
  int __fastcall PanelAt(int X);
  void __fastcall CheckAll(bool Check);
  void __fastcall Check(bool Check);
  TListItem * __fastcall SelectAll(bool Select, int Action = 0,
    bool OnlyTheAction = true);
  void __fastcall UpdateStatusBarSize();
  int __fastcall PanelCount();
  inline const TSynchronizeChecklist::TItem * GetChecklistItem(TListItem * Item);
  TSynchronizeChecklist::TAction & GetChecklistItemAction(
    const TSynchronizeChecklist::TItem * ChecklistItem);
  void __fastcall AddSubItem(TListItem * Item, int & Index, const UnicodeString & S);
  TRect __fastcall GetColumnHeaderRect(int Index);
  virtual void __fastcall Dispatch(void * Message);
  void __fastcall UpdateImages();
  bool __fastcall GetWindowParams(UnicodeString & WindowParams);
  void __fastcall ProcessedItem(void * Token, const TSynchronizeChecklist::TItem * ChecklistItem);
  void __fastcall UpdatedSynchronizationChecklistItems(const TSynchronizeChecklist::TItemList & Items);
  void __fastcall CountItemSize(const TSynchronizeChecklist::TItem * ChecklistItem, int Factor);
  void __fastcall CountItem(const TSynchronizeChecklist::TItem * ChecklistItem, int Factor);
  void __fastcall CountItemTotal(const TSynchronizeChecklist::TItem * ChecklistItem, int Factor);
  void __fastcall DeleteItem(const TSynchronizeChecklist::TItem * ChecklistItem);
  void __fastcall CheckDirectory(bool Check);
  void __fastcall DoBrowse(TOperationSide Side);
  void __fastcall ListViewHintShow(TCMHintShow & HintShow);
  void __fastcall StatusBarHintShow(TCMHintShow & HintShow);
  DYNAMIC void __fastcall KeyDown(Word & Key, TShiftState Shift);
  void CalculateSize(bool All);
  TIEListViewColProperties * GetColProperties();
  bool IterateItems(TListItem *& Item, TItemStates States);
  bool IterateSelectedItems(TListItem *& Item);
  void DoSynchronize(bool Queue);
};
//----------------------------------------------------------------------------
#endif
