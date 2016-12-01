//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "FileOperationProgress.h"
#include "CoreMain.h"
//---------------------------------------------------------------------------
#define TRANSFER_BUF_SIZE 32768
//---------------------------------------------------------------------------
__fastcall TFileOperationProgressType::TFileOperationProgressType()
{
  FOnProgress = NULL;
  FOnFinished = NULL;
  Clear();
}
//---------------------------------------------------------------------------
__fastcall TFileOperationProgressType::TFileOperationProgressType(
  TFileOperationProgressEvent AOnProgress, TFileOperationFinished AOnFinished)
{
  FOnProgress = AOnProgress;
  FOnFinished = AOnFinished;
  FReset = false;
  Clear();
}
//---------------------------------------------------------------------------
__fastcall TFileOperationProgressType::~TFileOperationProgressType()
{
  DebugAssert(!InProgress || FReset);
  DebugAssert(!Suspended || FReset);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AssignButKeepSuspendState(const TFileOperationProgressType & Other)
{
  TValueRestorer<unsigned int> SuspendTimeRestorer(FSuspendTime);
  TValueRestorer<bool> SuspendedRestorer(FSuspended);

  *this = Other;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Clear()
{
  FFileName = L"";
  FFullFileName = L"";
  FDirectory = L"";
  FAsciiTransfer = false;
  FCount = 0;
  FFilesFinished = 0;
  FStartTime = Now();
  FSuspended = false;
  FSuspendTime = 0;
  FInProgress = false;
  FFileInProgress = false;
  FTotalTransferred = 0;
  FTotalSkipped = 0;
  FTotalSize = 0;
  FSkippedSize = 0;
  FTotalSizeSet = false;
  FOperation = foNone;
  FTemp = false;
  FSkipToAll = false;
  FBatchOverwrite = boNo;
  // to bypass check in ClearTransfer()
  FTransferSize = 0;
  FCPSLimit = 0;
  FTicks.clear();
  FTotalTransferredThen.clear();
  FCounterSet = false;
  ClearTransfer();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::ClearTransfer()
{
  if ((TransferSize > 0) && (TransferredSize < TransferSize))
  {
    __int64 RemainingSize = (TransferSize - TransferredSize);
    FTotalSkipped += RemainingSize;
  }
  FLocalSize = 0;
  FTransferSize = 0;
  FLocallyUsed = 0;
  FSkippedSize = 0;
  FTransferredSize = 0;
  FTransferingFile = false;
  FLastSecond = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Start(TFileOperation AOperation,
  TOperationSide ASide, int ACount)
{
  Start(AOperation, ASide, ACount, false, L"", 0);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Start(TFileOperation AOperation,
  TOperationSide ASide, int ACount, bool ATemp,
  const UnicodeString ADirectory, unsigned long ACPSLimit)
{
  Clear();
  FOperation = AOperation;
  FSide = ASide;
  FCount = ACount;
  FInProgress = true;
  FCancel = csContinue;
  FDirectory = ADirectory;
  FTemp = ATemp;
  FCPSLimit = ACPSLimit;
  try
  {
    DoProgress();
  }
  catch (...)
  {
    // connection can be lost during progress callbacks
    ClearTransfer();
    FInProgress = false;
    throw;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Reset()
{
  FReset = true;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Stop()
{
  // added to include remaining bytes to TotalSkipped, in case
  // the progress happens to update before closing
  ClearTransfer();
  FInProgress = false;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Suspend()
{
  DebugAssert(!Suspended);
  FSuspended = true;
  FSuspendTime = GetTickCount();
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Resume()
{
  DebugAssert(Suspended);
  FSuspended = false;

  // shift timestamps for CPS calculation in advance
  // by the time the progress was suspended
  unsigned long Stopped = (GetTickCount() - FSuspendTime);
  size_t i = 0;
  while (i < FTicks.size())
  {
    FTicks[i] += Stopped;
    ++i;
  }

  DoProgress();
}
//---------------------------------------------------------------------------
int __fastcall TFileOperationProgressType::OperationProgress()
{
  DebugAssert(Count);
  int Result = (FFilesFinished * 100)/Count;
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TFileOperationProgressType::TransferProgress()
{
  int Result;
  if (TransferSize)
  {
    Result = (int)((TransferredSize * 100)/TransferSize);
  }
  else
  {
    Result = 0;
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TFileOperationProgressType::TotalTransferProgress()
{
  DebugAssert(TotalSizeSet);
  int Result = TotalSize > 0 ? (int)(((TotalTransferred + TotalSkipped) * 100)/TotalSize) : 0;
  return Result < 100 ? Result : 100;
}
//---------------------------------------------------------------------------
int __fastcall TFileOperationProgressType::OverallProgress()
{
  if (TotalSizeSet)
  {
    DebugAssert((Operation == foCopy) || (Operation == foMove));
    return TotalTransferProgress();
  }
  else
  {
    return OperationProgress();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Progress()
{
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::DoProgress()
{
  SetThreadExecutionState(ES_SYSTEM_REQUIRED);
  FOnProgress(*this);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Finish(UnicodeString FileName,
  bool Success, TOnceDoneOperation & OnceDoneOperation)
{
  DebugAssert(InProgress);

  FOnFinished(Operation, Side, Temp, FileName,
    /* TODO : There wasn't 'Success' condition, was it by mistake or by purpose? */
    Success && (Cancel == csContinue), OnceDoneOperation);
  FFilesFinished++;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetFile(UnicodeString AFileName, bool AFileInProgress)
{
  FFullFileName = AFileName;
  if (Side == osRemote)
  {
    // historically set were passing filename-only for remote site operations,
    // now we need to collect a full paths, so we pass in full path,
    // but still want to have filename-only in FileName
    AFileName = UnixExtractFileName(AFileName);
  }
  FFileName = AFileName;
  FFileInProgress = AFileInProgress;
  ClearTransfer();
  FFileStartTime = Now();
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetFileInProgress()
{
  DebugAssert(!FileInProgress);
  FFileInProgress = true;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetLocalSize(__int64 ASize)
{
  FLocalSize = ASize;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddLocallyUsed(__int64 ASize)
{
  FLocallyUsed += ASize;
  if (LocallyUsed > LocalSize)
  {
    FLocalSize = LocallyUsed;
  }
  DoProgress();
}
//---------------------------------------------------------------------------
bool __fastcall TFileOperationProgressType::IsLocallyDone()
{
  DebugAssert(LocallyUsed <= LocalSize);
  return (LocallyUsed == LocalSize);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetSpeedCounters()
{
  if ((CPSLimit > 0) && !FCounterSet)
  {
    FCounterSet = true;
    Configuration->Usage->Inc(L"SpeedLimitUses");
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::ThrottleToCPSLimit(
  unsigned long Size)
{
  unsigned long Remaining = Size;
  while (Remaining > 0)
  {
    Remaining -= AdjustToCPSLimit(Remaining);
  }
}
//---------------------------------------------------------------------------
unsigned long __fastcall TFileOperationProgressType::AdjustToCPSLimit(
  unsigned long Size)
{
  SetSpeedCounters();

  if (CPSLimit > 0)
  {
    // we must not return 0, hence, if we reach zero,
    // we wait until the next second
    do
    {
      unsigned int Second = (GetTickCount() / MSecsPerSec);

      if (Second != FLastSecond)
      {
        FRemainingCPS = CPSLimit;
        FLastSecond = Second;
      }

      if (FRemainingCPS == 0)
      {
        SleepEx(100, true);
        DoProgress();
      }
    }
    while ((CPSLimit > 0) && (FRemainingCPS == 0));

    // CPSLimit may have been dropped in DoProgress
    if (CPSLimit > 0)
    {
      if (FRemainingCPS < Size)
      {
        Size = FRemainingCPS;
      }

      FRemainingCPS -= Size;
    }
  }
  return Size;
}
//---------------------------------------------------------------------------
unsigned long __fastcall TFileOperationProgressType::LocalBlockSize()
{
  unsigned long Result = TRANSFER_BUF_SIZE;
  if (LocallyUsed + Result > LocalSize)
  {
    Result = (unsigned long)(LocalSize - LocallyUsed);
  }
  Result = AdjustToCPSLimit(Result);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetTotalSize(__int64 ASize)
{
  FTotalSize = ASize;
  FTotalSizeSet = true;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetTransferSize(__int64 ASize)
{
  FTransferSize = ASize;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetTransferingFile(bool ATransferingFile)
{
  FTransferingFile = ATransferingFile;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetCancel(TCancelStatus ACancel)
{
  FCancel = ACancel;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetCancelAtLeast(TCancelStatus ACancel)
{
  if (FCancel < ACancel)
  {
    FCancel = ACancel;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFileOperationProgressType::ClearCancelFile()
{
  bool Result = (Cancel == csCancelFile);
  if (Result)
  {
    SetCancel(csContinue);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetCPSLimit(unsigned long ACPSLimit)
{
  FCPSLimit = ACPSLimit;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetBatchOverwrite(TBatchOverwrite ABatchOverwrite)
{
  FBatchOverwrite = ABatchOverwrite;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetSkipToAll()
{
  FSkipToAll = true;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::ChangeTransferSize(__int64 ASize)
{
  // reflect change on file size (due to text transfer mode conversion particulary)
  // on total transfer size
  if (TotalSizeSet)
  {
    FTotalSize += (ASize - TransferSize);
  }
  FTransferSize = ASize;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::RollbackTransfer()
{
  FTransferredSize -= SkippedSize;
  DebugAssert(TransferredSize <= TotalTransferred);
  FTotalTransferred -= TransferredSize;
  DebugAssert(SkippedSize <= TotalSkipped);
  FTicks.clear();
  FTotalTransferredThen.clear();
  FTotalSkipped -= SkippedSize;
  FSkippedSize = 0;
  FTransferredSize = 0;
  FTransferSize = 0;
  FLocallyUsed = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddTransferred(__int64 ASize,
  bool AddToTotals)
{
  FTransferredSize += ASize;
  if (TransferredSize > TransferSize)
  {
    // this can happen with SFTP when downloading file that
    // grows while being downloaded
    if (TotalSizeSet)
    {
      // we should probably guard this with AddToTotals
      FTotalSize += (TransferredSize - TransferSize);
    }
    FTransferSize = TransferredSize;
  }
  if (AddToTotals)
  {
    FTotalTransferred += ASize;
    unsigned long Ticks = GetTickCount();
    if (FTicks.empty() ||
        (FTicks.back() > Ticks) || // ticks wrap after 49.7 days
        ((Ticks - FTicks.back()) >= MSecsPerSec))
    {
      FTicks.push_back(Ticks);
      FTotalTransferredThen.push_back(TotalTransferred);
    }

    if (FTicks.size() > 10)
    {
      FTicks.erase(FTicks.begin());
      FTotalTransferredThen.erase(FTotalTransferredThen.begin());
    }
  }
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddResumed(__int64 ASize)
{
  FTotalSkipped += ASize;
  FSkippedSize += ASize;
  AddTransferred(ASize, false);
  AddLocallyUsed(ASize);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddSkippedFileSize(__int64 ASize)
{
  FTotalSkipped += ASize;
  DoProgress();
}
//---------------------------------------------------------------------------
unsigned long __fastcall TFileOperationProgressType::TransferBlockSize()
{
  unsigned long Result = TRANSFER_BUF_SIZE;
  if (TransferredSize + Result > TransferSize)
  {
    Result = (unsigned long)(TransferSize - TransferredSize);
  }
  Result = AdjustToCPSLimit(Result);
  return Result;
}
//---------------------------------------------------------------------------
unsigned long __fastcall TFileOperationProgressType::StaticBlockSize()
{
  return TRANSFER_BUF_SIZE;
}
//---------------------------------------------------------------------------
bool __fastcall TFileOperationProgressType::IsTransferDone()
{
  DebugAssert(TransferredSize <= TransferSize);
  return (TransferredSize == TransferSize);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetAsciiTransfer(bool AAsciiTransfer)
{
  FAsciiTransfer = AAsciiTransfer;
  DoProgress();
}
//---------------------------------------------------------------------------
TDateTime __fastcall TFileOperationProgressType::TimeElapsed()
{
  return Now() - StartTime;
}
//---------------------------------------------------------------------------
unsigned int __fastcall TFileOperationProgressType::CPS()
{
  unsigned int Result;
  if (FTicks.empty())
  {
    Result = 0;
  }
  else
  {
    unsigned long Ticks = (Suspended ? FSuspendTime : GetTickCount());
    unsigned long TimeSpan;
    if (Ticks < FTicks.front())
    {
      // clocks has wrapped, guess 10 seconds difference
      TimeSpan = 10000;
    }
    else
    {
      TimeSpan = (Ticks - FTicks.front());
    }

    if (TimeSpan == 0)
    {
      Result = 0;
    }
    else
    {
      __int64 Transferred = (TotalTransferred - FTotalTransferredThen.front());
      Result = (unsigned int)(Transferred * MSecsPerSec / TimeSpan);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
TDateTime __fastcall TFileOperationProgressType::TimeExpected()
{
  unsigned int CurCps = CPS();
  if (CurCps)
  {
    return TDateTime((double)(((double)(TransferSize - TransferredSize)) / CurCps) / SecsPerDay);
  }
  else
  {
    return 0;
  }
}
//---------------------------------------------------------------------------
TDateTime __fastcall TFileOperationProgressType::TotalTimeExpected()
{
  DebugAssert(TotalSizeSet);
  unsigned int CurCps = CPS();
  // sanity check
  if ((CurCps > 0) && (TotalSize > TotalSkipped))
  {
    return TDateTime((double)((double)(TotalSize - TotalSkipped) / CurCps) /
      SecsPerDay);
  }
  else
  {
    return 0;
  }
}
//---------------------------------------------------------------------------
TDateTime __fastcall TFileOperationProgressType::TotalTimeLeft()
{
  DebugAssert(TotalSizeSet);
  unsigned int CurCps = CPS();
  // sanity check
  if ((CurCps > 0) && (TotalSize > TotalSkipped + TotalTransferred))
  {
    return TDateTime((double)((double)(TotalSize - TotalSkipped - TotalTransferred) / CurCps) /
      SecsPerDay);
  }
  else
  {
    return 0;
  }
}
