//---------------------------------------------------------------------------
#ifndef QueueControllerH
#define QueueControllerH
//---------------------------------------------------------------------------
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
enum TQueueOperation { qoNone, qoGoTo, qoPreferences, qoItemUserAction,
  qoItemQuery, qoItemError, qoItemPrompt, qoItemDelete, qoItemExecute,
  qoItemUp, qoItemDown, qoItemPause, qoItemResume, qoItemSpeed, qoPauseAll, qoResumeAll,
  qoOnceEmpty, qoDeleteAllDone, qoDeleteAll };
class TQueueItemProxy;
class TTerminalQueueStatus;
//---------------------------------------------------------------------------
class TQueueController
{
public:
  __fastcall TQueueController(TListView * ListView);
  virtual __fastcall ~TQueueController();

  TQueueOperation __fastcall DefaultOperation();
  bool __fastcall AllowOperation(TQueueOperation Operation, void ** Param = NULL);
  void __fastcall ExecuteOperation(TQueueOperation Operation, void * Param = NULL);

  void __fastcall UpdateQueueStatus(TTerminalQueueStatus * QueueStatus);
  void __fastcall RefreshQueueItem(TQueueItemProxy * QueueItem);
  static bool __fastcall QueueItemNeedsFrequentRefresh(TQueueItemProxy * QueueItem);

  bool __fastcall NeedRefresh();

  TQueueItemProxy * __fastcall GetFocusedPrimaryItem();

  __property TNotifyEvent OnChange = { read = FOnChange, write = FOnChange };
  __property bool Empty = { read = GetEmpty };

private:
  TListView * FListView;
  TTerminalQueueStatus * FQueueStatus;
  TNotifyEvent FOnChange;
  TFormatBytesStyle FFormatSizeBytes;

  TQueueItemProxy * __fastcall QueueViewItemToQueueItem(TListItem * Item);
  void __fastcall QueueViewDblClick(TObject * Sender);
  void __fastcall QueueViewKeyDown(TObject * Sender, WORD & Key, TShiftState Shift);
  void __fastcall QueueViewCustomDrawItem(TCustomListView * Sender, TListItem * Item,
    TCustomDrawState State, bool & DefaultDraw);
  virtual void __fastcall DoChange();
  bool __fastcall GetEmpty();
  void __fastcall RememberConfiguration();

  static void __fastcall FillQueueViewItem(TListItem * Item,
    TQueueItemProxy * QueueItem, bool Detail, bool OnlyLine);
  TListItem * __fastcall InsertItemFor(TQueueItemProxy * QueueItem, int Index);
  bool __fastcall UseDetailsLine(int ItemIndex, TQueueItemProxy * QueueItem);
};
//---------------------------------------------------------------------------
#endif
