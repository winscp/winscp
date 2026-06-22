//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "FileOperationProgress.h"
#include "CoreMain.h"
#include "Interface.h"
//---------------------------------------------------------------------------
#define TRANSFER_BUF_SIZE 32768
//---------------------------------------------------------------------------
TFileOperationStatistics::TFileOperationStatistics()
{
  memset(this, 0, sizeof(*this));
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TFileOperationProgressType::TPersistence::TPersistence()
{
  FStatistics = NULL;
  Clear(true, true);
}
//---------------------------------------------------------------------------
bool TFileOperationProgressType::IsIndeterminateOperation(TFileOperation Operation)
{
  return (Operation == foCalculateSize);
}
//---------------------------------------------------------------------------
bool TFileOperationProgressType::IsTransferOperation(TFileOperation Operation)
{
  return (Operation == foCopy) || (Operation == foMove);
}
//---------------------------------------------------------------------------
void TFileOperationProgressType::TPersistence::Clear(bool Batch, bool Speed)
{
  if (Batch)
  {
    TotalTransferred = 0;
    StartTime = Now();
    SkipToAll = false;
    BatchOverwrite = boNo;
    CPSLimit = 0;
    CounterSet = false;
  }
  if (Speed)
  {
    Ticks.clear();
    TotalTransferredThen.clear();
  }
}
//---------------------------------------------------------------------------
__fastcall TFileOperationProgressType::TFileOperationProgressType()
{
  FOnProgress = NULL;
  FOnFinished = NULL;
  FParent = NULL;
  Init();
  Clear();
}
//---------------------------------------------------------------------------
__fastcall TFileOperationProgressType::TFileOperationProgressType(
  TFileOperationProgressEvent AOnProgress, TFileOperationFinished AOnFinished,
  TFileOperationProgressType * Parent)
{
  FOnProgress = AOnProgress;
  FOnFinished = AOnFinished;
  FParent = Parent;
  FReset = false;
  Init();
  Clear();
}
//---------------------------------------------------------------------------
__fastcall TFileOperationProgressType::~TFileOperationProgressType()
{
  DebugAssert(!InProgress || FReset);
  DebugAssert(!Suspended || FReset);
  SAFE_DESTROY(FSection);
  SAFE_DESTROY(FUserSelectionsSection);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Init()
{
  FSection = new TCriticalSection();
  FUserSelectionsSection = new TCriticalSection();
  FRestored = false;
  FPersistence.Side = osCurrent; // = undefined value
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Assign(const TFileOperationProgressType & Other)
{
  TValueRestorer<TCriticalSection *> SectionRestorer(FSection);
  TValueRestorer<TCriticalSection *> UserSelectionsSectionRestorer(FUserSelectionsSection);
  TGuard Guard(FSection);
  TGuard OtherGuard(Other.FSection);

  *this = Other;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AssignButKeepSuspendState(const TFileOperationProgressType & Other)
{
  TGuard Guard(FSection);
  TValueRestorer<unsigned int> SuspendTimeRestorer(FSuspendTime);
  TValueRestorer<bool> SuspendedRestorer(FSuspended);

  Assign(Other);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::DoClear(bool Batch, bool Speed)
{
  FFileName = L"";
  FFullFileName = L"";
  FDirectory = L"";
  FAsciiTransfer = false;
  FCount = -1;
  FFilesFinished = 0;
  FFilesFinishedSuccessfully = 0;
  FSuspended = false;
  FSuspendTime = 0;
  FInProgress = false;
  FDone = false;
  FFileInProgress = false;
  FTotalSkipped = 0;
  FTotalSize = 0;
  FSkippedSize = 0;
  FTotalSizeSet = false;
  FOperation = foNone;
  FTemp = false;
  FPersistence.Clear(Batch, Speed);
  // to bypass check in ClearTransfer()
  FTransferSize = 0;
  ClearTransfer();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Clear()
{
  DoClear(true, true);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::ClearTransfer()
{
  if ((TransferSize > 0) && (TransferredSize < TransferSize))
  {
    TGuard Guard(FSection);
    __int64 RemainingSize = (TransferSize - TransferredSize);
    AddSkipped(RemainingSize);
  }
  FLocalSize = 0;
  FTransferSize = 0;
  FLocallyUsed = 0;
  FSkippedSize = 0;
  FTransferredSize = 0;
  FTransferringFile = false;
  FLastSecond = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Start(TFileOperation AOperation,
  TOperationSide ASide, int ACount)
{
  Start(AOperation, ASide, ACount, false, L"", 0, odoIdle);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Start(TFileOperation AOperation,
  TOperationSide ASide, int ACount, bool ATemp,
  const UnicodeString ADirectory, unsigned long ACPSLimit, TOnceDoneOperation InitialOnceDoneOperation)
{

  {
    TGuard Guard(FSection); // not really needed, just for consistency
    DoClear(!FRestored, (FPersistence.Side != osCurrent) && (FPersistence.Side != ASide));
    FTotalTransferBase = FPersistence.TotalTransferred;
    FOperation = AOperation;
    FPersistence.Side = ASide;
    FCount = ACount;
    FInProgress = true;
    FCancel = csContinue;
    FDirectory = ADirectory;
    FTemp = ATemp;
    FInitialOnceDoneOperation = InitialOnceDoneOperation;
    FPersistence.CPSLimit = ACPSLimit;
  }

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
void __fastcall TFileOperationProgressType::SetDone()
{
  FDone = true;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Suspend()
{

  {
    TGuard Guard(FSection);
    DebugAssert(!Suspended);
    FSuspended = true;
    FSuspendTime = GetTickCount();
  }

  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Resume()
{

  {
    TGuard Guard(FSection);
    DebugAssert(Suspended);
    FSuspended = false;

    // shift timestamps for CPS calculation in advance
    // by the time the progress was suspended
    unsigned long Stopped = (GetTickCount() - FSuspendTime);
    size_t i = 0;
    while (i < FPersistence.Ticks.size())
    {
      FPersistence.Ticks[i] += Stopped;
      ++i;
    }
  }

  DoProgress();
}
//---------------------------------------------------------------------------
int __fastcall TFileOperationProgressType::OperationProgress() const
{
  int Result;
  if (FCount > 0)
  {
    Result = (FFilesFinished * 100)/FCount;
  }
  else
  {
    Result = 0;
  }
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
int __fastcall TFileOperationProgressType::TotalTransferProgress() const
{
  TGuard Guard(FSection);
  DebugAssert(TotalSizeSet);
  int Result;
  if (FTotalSize > 0)
  {
    Result = (int)(((FPersistence.TotalTransferred - FTotalTransferBase + FTotalSkipped) * 100)/FTotalSize);
  }
  else
  {
    Result = 0;
  }
  return Result < 100 ? Result : 100;
}
//---------------------------------------------------------------------------
int __fastcall TFileOperationProgressType::OverallProgress() const
{
  if (TotalSizeSet)
  {
    DebugAssert(IsTransfer());
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
  SystemRequired();
  FOnProgress(*this);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Finish(UnicodeString FileName,
  bool Success, TOnceDoneOperation & OnceDoneOperation)
{
  DebugAssert(InProgress);

  // Cancel reader is guarded
  bool NotCancelled = (Cancel == csContinue);
  FOnFinished(Operation, Side, Temp, FileName, Success, NotCancelled, OnceDoneOperation);
  FFilesFinished++;
  if (Success)
  {
    FFilesFinishedSuccessfully++;
  }
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Succeeded(int Count)
{
  if (FPersistence.Statistics != NULL)
  {
    if (IsTransfer())
    {
      __int64 Transferred = FTransferredSize - FSkippedSize;
      if (Side == osLocal)
      {
        FPersistence.Statistics->FilesUploaded += Count;
        FPersistence.Statistics->TotalUploaded += Transferred;
      }
      else
      {
        FPersistence.Statistics->FilesDownloaded += Count;
        FPersistence.Statistics->TotalDownloaded += Transferred;
      }
    }
    else if (Operation == foDelete)
    {
      if (Side == osLocal)
      {
        FPersistence.Statistics->FilesDeletedLocal += Count;
      }
      else
      {
        FPersistence.Statistics->FilesDeletedRemote += Count;
      }
    }
  }
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
  if ((CPSLimit > 0) && !FPersistence.CounterSet)
  {
    FPersistence.CounterSet = true;
    Configuration->Usage->Inc(L"SpeedLimitUses");
  }
}
//---------------------------------------------------------------------------
// Used in WebDAV and S3 protocols
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

  // CPSLimit reader is guarded, we cannot block whole method as it can last long.
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
// Use in SCP protocol only
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
  TGuard Guard(FSection); // not really needed, just for consistency

  FTotalSize = ASize;
  FTotalSizeSet = true;
  // parent has its own totals
  if (FParent != NULL)
  {
    DebugAssert(FParent->TotalSizeSet);
  }
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetTransferSize(__int64 ASize)
{
  FTransferSize = ASize;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetTransferringFile(bool ATransferringFile)
{
  FTransferringFile = ATransferringFile;
}
//---------------------------------------------------------------------------
bool __fastcall TFileOperationProgressType::PassCancelToParent(TCancelStatus ACancel)
{
  bool Result;
  if (ACancel < csCancel)
  {
    // do not propagate csCancelFile,
    // though it's not supported for queue atm, so we do not expect it here
    DebugFail();
    Result = false;
  }
  else if (ACancel == csCancel)
  {
    Result = true;
  }
  else
  {
    // csCancelTransfer and csRemoteAbort are used with SCP only, which does not use parallel transfers
    DebugFail();
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetCancel(TCancelStatus ACancel)
{
  TGuard Guard(FSection);
  FCancel = ACancel;

  if ((FParent != NULL) && PassCancelToParent(ACancel))
  {
    FParent->SetCancel(ACancel);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetCancelAtLeast(TCancelStatus ACancel)
{
  TGuard Guard(FSection);
  if (FCancel < ACancel)
  {
    FCancel = ACancel;
  }

  if ((FParent != NULL) && PassCancelToParent(ACancel))
  {
    FParent->SetCancelAtLeast(ACancel);
  }
}
//---------------------------------------------------------------------------
TCancelStatus __fastcall TFileOperationProgressType::GetCancel()
{
  TCancelStatus Result = FCancel;
  if (FParent != NULL)
  {
    TGuard Guard(FSection);
    TCancelStatus ParentCancel = FParent->Cancel;
    if (ParentCancel > Result)
    {
      Result = ParentCancel;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFileOperationProgressType::ClearCancelFile()
{
  TGuard Guard(FSection);
  // Not propagated to parent, as this is local flag, see also PassCancelToParent
  bool Result = (Cancel == csCancelFile);
  if (Result)
  {
    FCancel = csContinue;
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned long __fastcall TFileOperationProgressType::GetCPSLimit()
{
  unsigned int Result;
  if (FParent != NULL)
  {
    Result = FParent->CPSLimit;
  }
  else
  {
    TGuard Guard(FSection);
    Result = FPersistence.CPSLimit;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetCPSLimit(unsigned long ACPSLimit)
{
  if (FParent != NULL)
  {
    FParent->SetCPSLimit(ACPSLimit);
  }
  else
  {
    TGuard Guard(FSection);
    FPersistence.CPSLimit = ACPSLimit;
  }
}
//---------------------------------------------------------------------------
TBatchOverwrite __fastcall TFileOperationProgressType::GetBatchOverwrite()
{
  TBatchOverwrite Result;
  if (FParent != NULL)
  {
    Result = FParent->BatchOverwrite;
  }
  else
  {
    TGuard Guard(FSection); // not really needed
    Result = FPersistence.BatchOverwrite;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetBatchOverwrite(TBatchOverwrite ABatchOverwrite)
{
  if (FParent != NULL)
  {
    FParent->SetBatchOverwrite(ABatchOverwrite);
  }
  else
  {
    TGuard Guard(FSection); // not really needed
    FPersistence.BatchOverwrite = ABatchOverwrite;;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFileOperationProgressType::GetSkipToAll()
{
  bool Result;
  if (FParent != NULL)
  {
    Result = FParent->SkipToAll;
  }
  else
  {
    TGuard Guard(FSection); // not really needed
    Result = FPersistence.SkipToAll;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetSkipToAll()
{
  if (FParent != NULL)
  {
    FParent->SetSkipToAll();
  }
  else
  {
    TGuard Guard(FSection); // not really needed
    FPersistence.SkipToAll = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::ChangeTransferSize(__int64 ASize)
{
  // reflect change on file size (due to text transfer mode conversion particularly)
  // on total transfer size
  if (TotalSizeSet)
  {
    AddTotalSize(ASize - TransferSize);
  }
  FTransferSize = ASize;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::RollbackTransferFromTotals(__int64 ATransferredSize, __int64 ASkippedSize)
{
  TGuard Guard(FSection);

  DebugAssert(ATransferredSize <= FPersistence.TotalTransferred - FTotalTransferBase);
  DebugAssert(ASkippedSize <= FTotalSkipped);
  FPersistence.TotalTransferred -= ATransferredSize;
  FPersistence.Ticks.clear();
  FPersistence.TotalTransferredThen.clear();
  FTotalSkipped -= ASkippedSize;

  if (FParent != NULL)
  {
    FParent->RollbackTransferFromTotals(ATransferredSize, ASkippedSize);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::RollbackTransfer()
{
  FTransferredSize -= FSkippedSize;
  RollbackTransferFromTotals(FTransferredSize, FSkippedSize);
  FSkippedSize = 0;
  FTransferredSize = 0;
  FTransferSize = 0;
  FLocallyUsed = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddTransferredToTotals(__int64 ASize)
{
  TGuard Guard(FSection);

  FPersistence.TotalTransferred += ASize;
  if (ASize >= 0)
  {
    unsigned long Ticks = GetTickCount();
    if (FPersistence.Ticks.empty() ||
        (FPersistence.Ticks.back() > Ticks) || // ticks wrap after 49.7 days
        ((Ticks - FPersistence.Ticks.back()) >= MSecsPerSec))
    {
      FPersistence.Ticks.push_back(Ticks);
      FPersistence.TotalTransferredThen.push_back(FPersistence.TotalTransferred);
    }

    if (FPersistence.Ticks.size() > 10)
    {
      FPersistence.Ticks.erase(FPersistence.Ticks.begin());
      FPersistence.TotalTransferredThen.erase(FPersistence.TotalTransferredThen.begin());
    }
  }
  else
  {
    FPersistence.Ticks.clear();
  }

  if (FParent != NULL)
  {
    FParent->AddTransferredToTotals(ASize);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddTotalSize(__int64 ASize)
{
  if (ASize != 0)
  {
    TGuard Guard(FSection);
    FTotalSize += ASize;

    if (FParent != NULL)
    {
      FParent->AddTotalSize(ASize);
    }
  }
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
      AddTotalSize(TransferredSize - TransferSize);
    }
    FTransferSize = TransferredSize;
  }
  if (AddToTotals)
  {
    AddTransferredToTotals(ASize);
  }
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddSkipped(__int64 ASize)
{
  TGuard Guard(FSection);

  FTotalSkipped += ASize;

  if (FParent != NULL)
  {
    FParent->AddSkipped(ASize);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddResumed(__int64 ASize)
{
  AddSkipped(ASize);
  FSkippedSize += ASize;
  AddTransferred(ASize, false);
  AddLocallyUsed(ASize);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddSkippedFileSize(__int64 ASize)
{
  AddSkipped(ASize);
  DoProgress();
}
//---------------------------------------------------------------------------
// Use in SCP protocol only
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
bool TFileOperationProgressType::IsTransferDoneChecked()
{
  DebugAssert(TransferredSize <= TransferSize);
  return IsTransferDone();
}
//---------------------------------------------------------------------------
// Note that this does not work correctly, if the file is larger than expected (at least with the FTP protocol)
bool TFileOperationProgressType::IsTransferDone()
{
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
  TGuard Guard(FSection);
  return GetCPS();
}
//---------------------------------------------------------------------------
inline static unsigned int CalculateCPS(__int64 Transferred, unsigned int MSecElapsed)
{
  unsigned int Result;
  if (MSecElapsed == 0)
  {
    Result = 0;
  }
  else
  {
    Result = (unsigned int)(Transferred * MSecsPerSec / MSecElapsed);
  }
  return Result;
}
//---------------------------------------------------------------------------
// Has to be called from a guarded method
unsigned int __fastcall TFileOperationProgressType::GetCPS()
{
  unsigned int Result;
  if (FPersistence.Ticks.empty())
  {
    Result = 0;
  }
  else
  {
    unsigned long Ticks = (Suspended ? FSuspendTime : GetTickCount());
    unsigned long TimeSpan;
    if (Ticks < FPersistence.Ticks.front())
    {
      // clocks has wrapped, guess 10 seconds difference
      TimeSpan = 10000;
    }
    else
    {
      TimeSpan = (Ticks - FPersistence.Ticks.front());
    }

    __int64 Transferred = (FPersistence.TotalTransferred - FPersistence.TotalTransferredThen.front());
    Result = CalculateCPS(Transferred, TimeSpan);
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
TDateTime __fastcall TFileOperationProgressType::TotalTimeLeft()
{
  TGuard Guard(FSection);
  DebugAssert(FTotalSizeSet);
  unsigned int CurCps = GetCPS();
  // sanity check
  __int64 Processed = FTotalSkipped + FPersistence.TotalTransferred - FTotalTransferBase;
  if ((CurCps > 0) && (FTotalSize > Processed))
  {
    return TDateTime((double)((double)(FTotalSize - Processed) / CurCps) / SecsPerDay);
  }
  else
  {
    return 0;
  }
}
//---------------------------------------------------------------------------
__int64 __fastcall TFileOperationProgressType::GetTotalTransferred()
{
  TGuard Guard(FSection);
  return FPersistence.TotalTransferred;
}
//---------------------------------------------------------------------------
__int64 __fastcall TFileOperationProgressType::GetOperationTransferred() const
{
  TGuard Guard(FSection);
  return FPersistence.TotalTransferred - FTotalTransferBase + FTotalSkipped;
}
//---------------------------------------------------------------------------
__int64 __fastcall TFileOperationProgressType::GetTotalSize()
{
  TGuard Guard(FSection);
  return FTotalSize;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::LockUserSelections()
{
  if (FParent != NULL)
  {
    FParent->LockUserSelections();
  }
  else
  {
    FUserSelectionsSection->Enter();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::UnlockUserSelections()
{
  if (FParent != NULL)
  {
    FParent->UnlockUserSelections();
  }
  else
  {
    FUserSelectionsSection->Leave();
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFileOperationProgressType::GetLogStr(bool Done)
{
  UnicodeString Transferred = FormatSize(TotalTransferred);

  TDateTime Time;
  UnicodeString TimeLabel;
  if (!Done && TotalSizeSet)
  {
    Time = TotalTimeLeft();
    TimeLabel = L"Left";
  }
  else
  {
    Time = TimeElapsed();
    TimeLabel = L"Elapsed";
  }
  UnicodeString TimeStr = FormatDateTimeSpan(Time);

  unsigned int ACPS;
  if (!Done)
  {
    ACPS = CPS();
  }
  else
  {
    unsigned int Elapsed = TimeToMSec(TimeElapsed());
    ACPS = CalculateCPS(TotalTransferred, Elapsed);
  }
  UnicodeString CPSStr = FormatSize(ACPS);

  return FORMAT(L"Transferred: %s, %s: %s, CPS: %s/s", (Transferred, TimeLabel, TimeStr, CPSStr));
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Store(TPersistence & Persistence)
{
  // in this current use, we do not need this to be guarded actually, as it's used only for a single-threaded synchronization
  TGuard Guard(FSection);
  Persistence = FPersistence;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Restore(TPersistence & Persistence)
{
  TGuard Guard(FSection);
  FPersistence = Persistence;
  FRestored = true;
}
//---------------------------------------------------------------------------
bool TFileOperationProgressType::IsIndeterminate() const
{
  return
    IsIndeterminateOperation(FOperation) ||
    (!TotalSizeSet && (FCount == 1));
}
//---------------------------------------------------------------------------
bool TFileOperationProgressType::IsTransfer() const
{
  return IsTransferOperation(Operation);
}
