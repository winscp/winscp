//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <ScpMain.h>
#include <Queue.h>
#include <TextsWin.h>
#include <AssociatedStatusBar.hpp>
#include <GUITools.h>
#include "QueueController.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TQueueController::TQueueController(TListView * ListView)
{
  FListView = ListView;
  assert(FListView != NULL);
  assert(FListView->OnDblClick == NULL);
  FListView->OnDblClick = QueueViewDblClick;
  assert(FListView->OnKeyDown == NULL);
  FListView->OnKeyDown = QueueViewKeyDown;

  FQueueStatus = NULL;
  FOnChange = NULL;
}
//---------------------------------------------------------------------------
__fastcall TQueueController::~TQueueController()
{
  assert(FListView->OnDblClick == QueueViewDblClick);
  FListView->OnDblClick = NULL;
  assert(FListView->OnKeyDown == QueueViewKeyDown);
  FListView->OnKeyDown = NULL;
}
//---------------------------------------------------------------------------
TQueueItemProxy * __fastcall TQueueController::QueueViewItemToQueueItem(
  TListItem * Item, bool * Detail)
{
  assert(Item != NULL);
  bool ADetail = false;

  int Index = Item->Index;
  if (Index < FQueueStatus->ActiveCount * 2)
  {
    ADetail = ((Index % 2) > 0);
    Index /= 2;
  }
  else
  {
    Index -= FQueueStatus->ActiveCount;
  }

  if (Detail != NULL)
  {
    *Detail = ADetail;
  }

  return FQueueStatus->Items[Index];
}
//---------------------------------------------------------------------------
TQueueOperation __fastcall TQueueController::DefaultOperation()
{
  TQueueItemProxy * QueueItem;

  if (FListView->ItemFocused != NULL)
  {
    QueueItem = QueueViewItemToQueueItem(FListView->ItemFocused);

    switch (QueueItem->Status)
    {
      case TQueueItem::qsPending:
        return qoItemExecute;

      case TQueueItem::qsQuery:
        return qoItemQuery;

      case TQueueItem::qsError:
        return qoItemError;

      case TQueueItem::qsPrompt:
        return qoItemPrompt;
    }
  }

  return qoNone;
}
//---------------------------------------------------------------------------
bool __fastcall TQueueController::AllowOperation(
  TQueueOperation Operation)
{
  TQueueItemProxy * QueueItem = NULL;

  if (FListView->ItemFocused != NULL)
  {
    QueueItem = QueueViewItemToQueueItem(FListView->ItemFocused);
  }

  switch (Operation)
  {
    case qoItemUserAction:
      return (QueueItem != NULL) && TQueueItem::IsUserActionStatus(QueueItem->Status);

    case qoItemQuery:
      return (QueueItem != NULL) && (QueueItem->Status == TQueueItem::qsQuery);

    case qoItemError:
      return (QueueItem != NULL) && (QueueItem->Status == TQueueItem::qsError);

    case qoItemPrompt:
      return (QueueItem != NULL) && (QueueItem->Status == TQueueItem::qsPrompt);

    case qoItemDelete:
      return (QueueItem != NULL) && (QueueItem->Status != TQueueItem::qsDone) &&
        !TQueueItem::IsUserActionStatus(QueueItem->Status);

    case qoItemExecute:
      return (QueueItem != NULL) && (QueueItem->Status == TQueueItem::qsPending);

    case qoItemUp:
      return (QueueItem != NULL) &&
        (QueueItem->Status == TQueueItem::qsPending) &&
        (FListView->ItemFocused->Index > (FQueueStatus->ActiveCount * 2));

    case qoItemDown:
      return (QueueItem != NULL) &&
        (QueueItem->Status == TQueueItem::qsPending) &&
        (FListView->ItemFocused->Index < (FListView->Items->Count - 1));

    default:
      assert(false);
      return false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TQueueController::ExecuteOperation(TQueueOperation Operation)
{
  TQueueItemProxy * QueueItem = NULL;

  if (FListView->ItemFocused != NULL)
  {
    QueueItem = QueueViewItemToQueueItem(FListView->ItemFocused);
  }

  if (QueueItem != NULL)
  {
    switch (Operation)
    {
      case qoItemUserAction:
      case qoItemQuery:
      case qoItemError:
      case qoItemPrompt:
        QueueItem->ProcessUserAction();
        break;

      case qoItemExecute:
        QueueItem->ExecuteNow();
        break;

      case qoItemUp:
      case qoItemDown:
        QueueItem->Move(Operation == qoItemUp);
        break;

      case qoItemDelete:
        QueueItem->Delete();
        break;

      default:
        assert(false);
        break;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TQueueController::FillQueueViewItem(TListItem * Item,
  TQueueItemProxy * QueueItem, bool Detail)
{
  assert(!Detail || (QueueItem->Status != TQueueItem::qsPending));

  assert((Item->Data == NULL) || (Item->Data == QueueItem));
  Item->Data = QueueItem;

  AnsiString ProgressStr;
  int State = -1;

  switch (QueueItem->Status)
  {
    case TQueueItem::qsPending:
      ProgressStr = LoadStr(QUEUE_PENDING);
      break;

    case TQueueItem::qsConnecting:
      ProgressStr = LoadStr(QUEUE_CONNECTING);
      break;

    case TQueueItem::qsQuery:
      ProgressStr = LoadStr(QUEUE_QUERY);
      State = 4;
      break;

    case TQueueItem::qsError:
      ProgressStr = LoadStr(QUEUE_ERROR);
      State = 5;
      break;

    case TQueueItem::qsPrompt:
      ProgressStr = LoadStr(QUEUE_PROMPT);
      State = 6;
      break;
  }

  bool BlinkHide = TQueueItem::IsUserActionStatus(QueueItem->Status) &&
    !QueueItem->ProcessingUserAction &&
    ((GetTickCount() % 1000) >= 500);

  int Image = -1;
  AnsiString Values[5];
  TFileOperationProgressType * ProgressData = QueueItem->ProgressData;
  TQueueItem::TInfo * Info = QueueItem->Info;

  if (!Detail)
  {
    switch (Info->Operation)
    {
      case foCopy:
        Image = 2;
        break;

      case foMove:
        Image = 3;
        break;
    }
    State = ((Info->Side == osLocal) ? 1 : 0);

    Values[0] = Info->Source;
    Values[1] = Info->Destination;

    if (ProgressData != NULL)
    {
      Values[2] = FormatBytes(ProgressData->TotalTransfered);
      Values[3] = FormatDateTimeSpan(Configuration->TimeFormat, ProgressData->TimeElapsed());
      if (ProgressStr.IsEmpty())
      {
        ProgressStr = FORMAT("%d%%", (ProgressData->OverallProgress()));
      }
    }
    Values[4] = ProgressStr;
  }
  else
  {
    if (ProgressData != NULL)
    {
      Values[0] = ProgressData->FileName;
      Values[2] = FormatBytes(ProgressData->TransferedSize);
      Values[3] = FORMAT("%s/s", (FormatBytes(ProgressData->CPS())));
      Values[4] = FORMAT("%d%%", (ProgressData->TransferProgress()));
    }
    else
    {
      Values[0] = ProgressStr;
    }
  }

  Item->StateIndex = (!BlinkHide ? State : -1);
  Item->ImageIndex = (!BlinkHide ? Image : -1);
  for (int Index = 0; Index < LENOF(Values); Index++)
  {
    if (Index < Item->SubItems->Count)
    {
      Item->SubItems->Strings[Index] = Values[Index];
    }
    else
    {
      Item->SubItems->Add(Values[Index]);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TQueueController::UpdateQueueStatus(
  TTerminalQueueStatus * QueueStatus)
{
  FQueueStatus = QueueStatus;
   
  if (FQueueStatus != NULL)
  {
    TQueueItemProxy * QueueItem;
    TListItem * Item;
    int Index = 0;
    for (int ItemIndex = 0; ItemIndex < FQueueStatus->Count; ItemIndex++)
    {
      QueueItem = FQueueStatus->Items[ItemIndex];

      int Index2 = Index;
      while ((Index2 < FListView->Items->Count) &&
             (FListView->Items->Item[Index2]->Data != QueueItem))
      {
        Index2++;
      }

      if (Index2 < FListView->Items->Count)
      {
        while (Index < Index2)
        {
          FListView->Items->Delete(Index);
          Index2--;
        }
      }

      if (Index == FListView->Items->Count)
      {
        Item = FListView->Items->Add();
      }
      else if (FListView->Items->Item[Index]->Data != QueueItem)
      {
        Item = FListView->Items->Insert(Index);
      }
      else
      {
        Item = FListView->Items->Item[Index];
        assert(Item->Data == QueueItem);
      }
      FillQueueViewItem(Item, QueueItem, false);
      Index++;

      assert((QueueItem->Status != TQueueItem::qsPending) ==
        (ItemIndex < FQueueStatus->ActiveCount));

      if (ItemIndex < FQueueStatus->ActiveCount)
      {
        if (Index == FListView->Items->Count)
        {
          Item = FListView->Items->Add();
        }
        else if (FListView->Items->Item[Index]->Data != QueueItem)
        {
          Item = FListView->Items->Insert(Index);
        }
        else
        {
          Item = FListView->Items->Item[Index];
          assert(Item->Data == QueueItem);
        }
        FillQueueViewItem(Item, QueueItem, true);
        Index++;
      }
    }

    while (Index < FListView->Items->Count)
    {
      FListView->Items->Delete(Index);
    }
  }
  else
  {
    FListView->Items->Clear();
  }

  DoChange();
}
//---------------------------------------------------------------------------
void __fastcall TQueueController::RefreshQueueItem(TQueueItemProxy * QueueItem)
{
  TListItem * NextListItem = NULL;
  TListItem * ListItem;

  ListItem = FListView->FindData(0, QueueItem, true, false);
  assert(ListItem != NULL);

  int ItemIndex = ListItem->Index;
  if (ItemIndex + 1 < FListView->Items->Count)
  {
    NextListItem = FListView->Items->Item[ItemIndex + 1];
    if (NextListItem->Data != QueueItem)
    {
      NextListItem = NULL;
    }
  }

  FillQueueViewItem(ListItem, QueueItem, false);

  if (NextListItem == NULL)
  {
    NextListItem = FListView->Items->Insert(ItemIndex + 1);
  }
  FillQueueViewItem(NextListItem, QueueItem, true);

  DoChange();
}
//---------------------------------------------------------------------------
void __fastcall TQueueController::DoChange()
{
  if (FOnChange != NULL)
  {
    FOnChange(NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TQueueController::QueueViewDblClick(TObject * /*Sender*/)
{
  TQueueOperation Operation = DefaultOperation();

  if (Operation != qoNone)
  {
    ExecuteOperation(Operation);
  }
}
//---------------------------------------------------------------------------
void __fastcall TQueueController::QueueViewKeyDown(TObject * /*Sender*/,
  WORD & Key, TShiftState /*Shift*/)
{
  if (Key == VK_RETURN)
  {
    TQueueOperation Operation = DefaultOperation();

    if (Operation != qoNone)
    {
      ExecuteOperation(Operation);
    }
    Key = 0;
  }
  else if (Key == VK_DELETE)
  {
    ExecuteOperation(qoItemDelete);
    Key = 0;
  }
}

