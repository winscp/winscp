//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <CoreMain.h>
#include <Queue.h>
#include <TextsWin.h>
#include <GUITools.h>
#include <WinConfiguration.h>
#include "QueueController.h"
#include <BaseUtils.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TQueueController::TQueueController(TListView * ListView)
{
  FListView = ListView;
  DebugAssert(FListView != NULL);
  DebugAssert(FListView->OnDblClick == NULL);
  FListView->OnDblClick = QueueViewDblClick;
  DebugAssert(FListView->OnKeyDown == NULL);
  FListView->OnKeyDown = QueueViewKeyDown;
  DebugAssert(FListView->OnCustomDrawItem == NULL);
  FListView->OnCustomDrawItem = QueueViewCustomDrawItem;

  FQueueStatus = NULL;
  FOnChange = NULL;

  RememberConfiguration();
}
//---------------------------------------------------------------------------
__fastcall TQueueController::~TQueueController()
{
  DebugAssert(FListView->OnDblClick == QueueViewDblClick);
  FListView->OnDblClick = NULL;
  DebugAssert(FListView->OnKeyDown == QueueViewKeyDown);
  FListView->OnKeyDown = NULL;
  DebugAssert(FListView->OnCustomDrawItem == QueueViewCustomDrawItem);
  FListView->OnCustomDrawItem = NULL;
}
//---------------------------------------------------------------------------
TQueueItemProxy * __fastcall TQueueController::QueueViewItemToQueueItem(
  TListItem * Item, bool * Detail)
{
  // previously this method was based on ActiveCount and DoneCount,
  // as if we were inconfident about validity of Item->Data pointers,
  // not sure why
  TQueueItemProxy * QueueItem = static_cast<TQueueItemProxy *>(Item->Data);

  if (Detail != NULL)
  {
    (*Detail) = false;
    int Index = Item->Index;
    if (Index > 0)
    {
      TQueueItemProxy * PrevQueueItem = static_cast<TQueueItemProxy *>(Item->Data);
      if (PrevQueueItem == QueueItem)
      {
        (*Detail) = true;
      }
    }
  }

  return QueueItem;
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

      case TQueueItem::qsProcessing:
        return qoItemPause;

      case TQueueItem::qsPaused:
        return qoItemResume;
    }
  }

  return qoNone;
}
//---------------------------------------------------------------------------
bool __fastcall TQueueController::AllowOperation(
  TQueueOperation Operation, void ** Param)
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
      return (QueueItem != NULL);

    case qoItemExecute:
      return (QueueItem != NULL) && (QueueItem->Status == TQueueItem::qsPending);

    case qoItemUp:
      return (QueueItem != NULL) &&
        (QueueItem->Status == TQueueItem::qsPending) &&
        // it's not first pending item,
        // this is based on assumption that pending items occupy single line always
        (FListView->ItemFocused->Index > 0) &&
        (QueueViewItemToQueueItem(FListView->Items->Item[FListView->ItemFocused->Index - 1])->Status == TQueueItem::qsPending);

    case qoItemDown:
      return (QueueItem != NULL) &&
        (QueueItem->Status == TQueueItem::qsPending) &&
        (FListView->ItemFocused->Index < (FListView->Items->Count - 1));

    case qoItemPause:
      return (QueueItem != NULL) &&
        (QueueItem->Status == TQueueItem::qsProcessing);

    case qoItemResume:
      return (QueueItem != NULL) &&
        (QueueItem->Status == TQueueItem::qsPaused);

    case qoItemSpeed:
      {
        bool Result = (QueueItem != NULL) && (QueueItem->Status != TQueueItem::qsDone);
        if (Result && (Param != NULL))
        {
          Result = QueueItem->GetCPSLimit(*reinterpret_cast<unsigned long *>(Param));
        }
        return Result;
      }

    case qoPauseAll:
    case qoResumeAll:
      {
        TQueueItem::TStatus Status =
          (Operation == qoPauseAll) ? TQueueItem::qsProcessing : TQueueItem::qsPaused;
        bool Result = false;
        // can be NULL when action update is triggered while disconnecting
        if (FQueueStatus != NULL)
        {
          for (int i = FQueueStatus->DoneCount; !Result && (i < FQueueStatus->DoneAndActiveCount); i++)
          {
            QueueItem = FQueueStatus->Items[i];
            Result = (QueueItem->Status == Status);
          }
        }
        return Result;
      }

    case qoDeleteAllDone:
      return (FQueueStatus != NULL) && (FQueueStatus->DoneCount > 0);

    case qoDeleteAll:
      return (FQueueStatus != NULL) && (FQueueStatus->Count > 0);

    default:
      DebugFail();
      return false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TQueueController::ExecuteOperation(TQueueOperation Operation,
  void * Param)
{
  TQueueItemProxy * QueueItem = NULL;

  if (FListView->ItemFocused != NULL)
  {
    QueueItem = QueueViewItemToQueueItem(FListView->ItemFocused);
  }

  switch (Operation)
  {
    case qoItemUserAction:
    case qoItemQuery:
    case qoItemError:
    case qoItemPrompt:
      if (QueueItem != NULL)
      {
        QueueItem->ProcessUserAction();
      }
      break;

    case qoItemExecute:
      if (QueueItem != NULL)
      {
        QueueItem->ExecuteNow();
      }
      break;

    case qoItemUp:
    case qoItemDown:
      if (QueueItem != NULL)
      {
        QueueItem->Move(Operation == qoItemUp);
      }
      break;

    case qoItemDelete:
      if (QueueItem != NULL)
      {
        QueueItem->Delete();
      }
      break;

    case qoItemPause:
      if (QueueItem != NULL)
      {
        QueueItem->Pause();
      }
      break;

    case qoItemResume:
      if (QueueItem != NULL)
      {
        QueueItem->Resume();
      }
      break;

    case qoItemSpeed:
      if (QueueItem != NULL)
      {
        QueueItem->SetCPSLimit(reinterpret_cast<unsigned long>(Param));
      }
      break;

    case qoPauseAll:
    case qoResumeAll:
      {
        for (int i = FQueueStatus->DoneCount; i < FQueueStatus->DoneAndActiveCount; i++)
        {
          QueueItem = FQueueStatus->Items[i];
          if ((Operation == qoPauseAll) && (QueueItem->Status == TQueueItem::qsProcessing))
          {
            QueueItem->Pause();
          }
          else if ((Operation == qoResumeAll) && (QueueItem->Status == TQueueItem::qsPaused))
          {
            QueueItem->Resume();
          }
        }
      }
      break;

    case qoDeleteAllDone:
    case qoDeleteAll:
      {
        int Count = (Operation == qoDeleteAll) ? FQueueStatus->Count : FQueueStatus->DoneCount;
        for (int i = 0; i < Count; i++)
        {
          QueueItem = FQueueStatus->Items[i];
          QueueItem->Delete();
        }
      }
      break;

    default:
      DebugFail();
      break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TQueueController::FillQueueViewItem(TListItem * Item,
  TQueueItemProxy * QueueItem, bool Detail, bool OnlyLine)
{
  DebugAssert(!Detail || (QueueItem->Status != TQueueItem::qsPending));

  DebugAssert((Item->Data == NULL) || (Item->Data == QueueItem));
  Item->Data = QueueItem;

  UnicodeString ProgressStr;
  int Image = -1;

  switch (QueueItem->Status)
  {
    case TQueueItem::qsDone:
      ProgressStr = LoadStr(QUEUE_DONE);
      break;

    case TQueueItem::qsPending:
      ProgressStr = LoadStr(QUEUE_PENDING);
      break;

    case TQueueItem::qsConnecting:
      ProgressStr = LoadStr(QUEUE_CONNECTING);
      break;

    case TQueueItem::qsQuery:
      ProgressStr = LoadStr(QUEUE_QUERY);
      Image = 4;
      break;

    case TQueueItem::qsError:
      ProgressStr = LoadStr(QUEUE_ERROR);
      Image = 5;
      break;

    case TQueueItem::qsPrompt:
      ProgressStr = LoadStr(QUEUE_PROMPT);
      Image = 6;
      break;

    case TQueueItem::qsPaused:
      ProgressStr = LoadStr(QUEUE_PAUSED);
      Image = 7;
      break;
  }

  bool BlinkHide = QueueItemNeedsFrequentRefresh(QueueItem) &&
    !QueueItem->ProcessingUserAction &&
    ((GetTickCount() % MSecsPerSec) >= (MSecsPerSec/2));

  int State = -1;
  UnicodeString Values[6];
  TFileOperationProgressType * ProgressData = QueueItem->ProgressData;
  TQueueItem::TInfo * Info = QueueItem->Info;

  if (!Detail && Info->Primary)
  {
    switch (Info->Operation)
    {
      case foCopy:
        State = ((Info->Side == osLocal) ? 2 : 0);
        break;

      case foMove:
        State = ((Info->Side == osLocal) ? 3 : 1);
        break;
    }

    if (!OnlyLine)
    {
      Image = -1;
      ProgressStr = L"";
    }

    // If both are empty, it's bootstrap item => do not show anything
    if (!Info->Source.IsEmpty() || !Info->Destination.IsEmpty())
    {
      // cannot use ProgressData->Temp as it is set only after the transfer actually starts
      Values[0] = Info->Source.IsEmpty() ? LoadStr(PROGRESS_TEMP_DIR) : Info->Source;
      Values[1] = Info->Destination.IsEmpty() ? LoadStr(PROGRESS_TEMP_DIR) : Info->Destination;
    }

    __int64 TotalTransferred = QueueItem->TotalTransferred;
    if (TotalTransferred >= 0)
    {
      Values[2] =
        FormatPanelBytes(TotalTransferred, WinConfiguration->FormatSizeBytes);
    }

    if (ProgressData != NULL)
    {
      if (ProgressData->Operation == Info->Operation)
      {
        if (QueueItem->Status != TQueueItem::qsDone)
        {
          if (ProgressData->TotalSizeSet)
          {
            Values[3] = FormatDateTimeSpan(Configuration->TimeFormat, ProgressData->TotalTimeLeft());
          }
          else
          {
            Values[3] = FormatDateTimeSpan(Configuration->TimeFormat, ProgressData->TimeElapsed());
          }

          Values[4] = FORMAT(L"%s/s", (FormatBytes(ProgressData->CPS())));
        }

        if (ProgressStr.IsEmpty())
        {
          ProgressStr = FORMAT(L"%d%%", (ProgressData->OverallProgress()));
        }
      }
      else if (ProgressData->Operation == foCalculateSize)
      {
        ProgressStr = LoadStr(QUEUE_CALCULATING_SIZE);
      }
    }
    Values[5] = ProgressStr;
  }
  else
  {
    if (ProgressData != NULL)
    {
      if ((Info->Side == osRemote) || !ProgressData->Temp)
      {
        Values[0] = ProgressData->FileName;
      }
      else
      {
        Values[0] = ExtractFileName(ProgressData->FileName);
      }

      if (ProgressData->Operation == Info->Operation)
      {
        Values[2] =
          FormatPanelBytes(ProgressData->TransferredSize, WinConfiguration->FormatSizeBytes);

        if (ProgressStr.IsEmpty())
        {
          ProgressStr = FORMAT(L"%d%%", (ProgressData->TransferProgress()));
        }
      }
    }
    Values[5] = ProgressStr;
  }

  Item->StateIndex = (!BlinkHide ? State : -1);
  Item->ImageIndex = (!BlinkHide ? Image : -1);
  for (size_t Index = 0; Index < LENOF(Values); Index++)
  {
    if (Index < static_cast<size_t>(Item->SubItems->Count))
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
TListItem * __fastcall TQueueController::InsertItemFor(TQueueItemProxy * QueueItem, int Index)
{
  TListItem * Item;
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
    DebugAssert(Item->Data == QueueItem);
  }
  return Item;
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

      Item = InsertItemFor(QueueItem, Index);
      bool HasDetailsLine = UseDetailsLine(ItemIndex, QueueItem);
      FillQueueViewItem(Item, QueueItem, false, !HasDetailsLine);
      Index++;

      DebugAssert((QueueItem->Status != TQueueItem::qsPending) ==
        (ItemIndex < FQueueStatus->DoneAndActiveCount));

      if (HasDetailsLine)
      {
        Item = InsertItemFor(QueueItem, Index);
        FillQueueViewItem(Item, QueueItem, true, false);
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
bool __fastcall TQueueController::UseDetailsLine(int ItemIndex, TQueueItemProxy * QueueItem)
{
  return
    (ItemIndex >= FQueueStatus->DoneCount) &&
    (ItemIndex < FQueueStatus->DoneAndActiveCount) &&
    QueueItem->Info->Primary &&
    !QueueItem->Info->SingleFile &&
    ((QueueItem->ProgressData == NULL) || !QueueItem->ProgressData->Done);
}
//---------------------------------------------------------------------------
void __fastcall TQueueController::RefreshQueueItem(TQueueItemProxy * QueueItem)
{
  TListItem * NextListItem = NULL;
  TListItem * ListItem;

  ListItem = FListView->FindData(0, QueueItem, true, false);
  DebugAssert(ListItem != NULL);

  int Index = ListItem->Index;
  if (Index + 1 < FListView->Items->Count)
  {
    NextListItem = FListView->Items->Item[Index + 1];
    if (NextListItem->Data != QueueItem)
    {
      NextListItem = NULL;
    }
  }

  bool HasDetailsLine = UseDetailsLine(QueueItem->Index, QueueItem);
  FillQueueViewItem(ListItem, QueueItem, false, !HasDetailsLine);

  if (HasDetailsLine)
  {
    if (NextListItem == NULL)
    {
      NextListItem = FListView->Items->Insert(Index + 1);
    }
    FillQueueViewItem(NextListItem, QueueItem, true, false);
  }
  else
  {
    if (NextListItem != NULL)
    {
      NextListItem->Delete();
    }
  }

  DoChange();
}
//---------------------------------------------------------------------------
bool __fastcall TQueueController::QueueItemNeedsFrequentRefresh(
  TQueueItemProxy * QueueItem)
{
  return
    (TQueueItem::IsUserActionStatus(QueueItem->Status) ||
     (QueueItem->Status == TQueueItem::qsPaused));
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
//---------------------------------------------------------------------------
void __fastcall TQueueController::QueueViewCustomDrawItem(TCustomListView * Sender,
  TListItem * Item, TCustomDrawState /*State*/, bool & /*DefaultDraw*/)
{
  TQueueItemProxy * QueueItem = QueueViewItemToQueueItem(Item);
  if (QueueItem->Status == TQueueItem::qsDone)
  {
    Sender->Canvas->Font->Color = clGrayText;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TQueueController::GetEmpty()
{
  return (FQueueStatus == NULL) || (FQueueStatus->Count == 0);
}
//---------------------------------------------------------------------------
void __fastcall TQueueController::RememberConfiguration()
{
  FFormatSizeBytes = WinConfiguration->FormatSizeBytes;
}
//---------------------------------------------------------------------------
bool __fastcall TQueueController::NeedRefresh()
{
  bool Result = (WinConfiguration->FormatSizeBytes != FFormatSizeBytes);
  RememberConfiguration();
  return Result;
}
