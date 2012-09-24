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
//----------------------------------------------------------------------------
#include <Terminal.h>
//----------------------------------------------------------------------------
class TSynchronizeChecklistDialog : public TForm
{
__published:
  TPanel * Panel;
  TIEListView *ListView;
  TStatusBar *StatusBar;
  TImageList *ActionImages;
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
  TImageList *ArrowImages;
  TButton *CustomCommandsButton;
  void __fastcall HelpButtonClick(TObject * Sender);
  void __fastcall FormShow(TObject * Sender);
  void __fastcall StatusBarDrawPanel(TStatusBar *StatusBar,
          TStatusPanel *Panel, const TRect &Rect);
  void __fastcall StatusBarMouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y);
  void __fastcall ListViewChange(TObject *Sender, TListItem *Item,
          TItemChange Change);
  void __fastcall ListViewChanging(TObject *Sender, TListItem *Item,
          TItemChange Change, bool &AllowChange);
  void __fastcall CheckAllButtonClick(TObject *Sender);
  void __fastcall CheckButtonClick(TObject *Sender);
  void __fastcall ListViewSelectItem(TObject *Sender, TListItem *Item,
          bool Selected);
  void __fastcall StatusBarResize(TObject *Sender);
  void __fastcall UpdateTimerTimer(TObject *Sender);
  void __fastcall SelectAllItemClick(TObject *Sender);
  void __fastcall StatusBarMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
  void __fastcall ListViewCompare(TObject *Sender, TListItem *Item1,
          TListItem *Item2, int Data, int &Compare);
  void __fastcall ListViewSecondaryColumnHeader(TCustomIEListView *Sender,
          int Index, int &SecondaryColumn);
  void __fastcall ListViewContextPopup(TObject *Sender, TPoint &MousePos,
          bool &Handled);
  void __fastcall CustomCommandsButtonClick(TObject *Sender);
  void __fastcall ListViewAdvancedCustomDrawSubItem(TCustomListView *Sender, TListItem *Item,
          int SubItem, TCustomDrawState State, TCustomDrawStage Stage, bool &DefaultDraw);


public:
  __fastcall TSynchronizeChecklistDialog(TComponent * AOwner,
    TSynchronizeMode Mode, int Params, const UnicodeString LocalDirectory,
    const UnicodeString RemoteDirectory, TCustomCommandMenuEvent OnCustomCommandMenu);
  virtual __fastcall ~TSynchronizeChecklistDialog();

  bool __fastcall Execute(TSynchronizeChecklist * Checklist);

protected:
  bool FFormRestored;
  TSynchronizeChecklist * FChecklist;
  TSynchronizeMode FMode;
  int FParams;
  UnicodeString FLocalDirectory;
  UnicodeString FRemoteDirectory;
  TImageList * FSystemImageList;
  TWndMethod FOrigListViewWindowProc;
  int FTotals[1 + TSynchronizeChecklist::ActionCount];
  int FChecked[1 + TSynchronizeChecklist::ActionCount];
  TListItem * FChangingItem;
  bool FChangingItemChecked;
  bool FChangingItemIgnore;
  bool FChangingItemMass;
  UnicodeString FGeneralHint;
  TCustomCommandMenuEvent FOnCustomCommandMenu;

  void __fastcall UpdateControls();
  virtual void __fastcall CreateParams(TCreateParams & Params);
  void __fastcall LoadItem(TListItem * Item);
  void __fastcall LoadList();
  void __fastcall ListViewWindowProc(TMessage & Message);
  int __fastcall PanelAt(int X);
  void __fastcall CheckAll(bool Check);
  TListItem * __fastcall SelectAll(bool Select, int Action = 0,
    bool OnlyTheAction = true);
  static int __fastcall CompareNumber(__int64 Value1, __int64 Value2);
};
//----------------------------------------------------------------------------
#endif
