//----------------------------------------------------------------------------
#ifndef SynchronizeChecklistH
#define SynchronizeChecklistH
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
#include <ComCtrls.hpp>
#include <HistoryComboBox.hpp>
#include <PathLabel.hpp>

#include <Terminal.h>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "IEListView.hpp"
#include "NortonLikeListView.hpp"
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
  void __fastcall HelpButtonClick(TObject * Sender);
  void __fastcall FormShow(TObject * Sender);
  void __fastcall ListViewAdvancedCustomDrawSubItem(
    TCustomListView * Sender, TListItem * Item, int SubItem,
    TCustomDrawState State, TCustomDrawStage Stage,
    bool & DefaultDraw);
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
  
public:
  __fastcall TSynchronizeChecklistDialog(TComponent * AOwner,
    TSynchronizeMode Mode, int Params, const AnsiString LocalDirectory,
    const AnsiString RemoteDirectory);
  virtual __fastcall ~TSynchronizeChecklistDialog();

  bool __fastcall Execute(TSynchronizeChecklist * Checklist);

protected:
  bool FFormRestored;
  TSynchronizeChecklist * FChecklist;
  TSynchronizeMode FMode;
  int FParams;
  AnsiString FLocalDirectory;
  AnsiString FRemoteDirectory;
  TImageList * FSystemImageList;
  TWndMethod FOrigListViewWindowProc;
  int FTotals[1 + TSynchronizeChecklist::ActionCount];
  int FChecked[1 + TSynchronizeChecklist::ActionCount];
  TListItem * FChangingItem;
  bool FChangingItemChecked;
  bool FChangingItemIgnore;
  bool FChangingItemMass;
  AnsiString FGeneralHint;

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
