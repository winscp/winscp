//---------------------------------------------------------------------------
#ifndef FileOperationProgressH
#define FileOperationProgressH
//---------------------------------------------------------------------------
#include "Configuration.h"
#include "CopyParam.h"
#include "Exceptions.h"
#include <vector>
//---------------------------------------------------------------------------
class TFileOperationProgressType;
enum TFileOperation { foNone, foCopy, foMove, foDelete, foSetProperties,
  foRename, foCustomCommand, foCalculateSize, foRemoteMove, foRemoteCopy,
  foGetProperties, foCalculateChecksum, foLock, foUnlock };
// csCancelTransfer and csRemoteAbort are used with SCP only
enum TCancelStatus { csContinue = 0, csCancelFile, csCancel, csCancelTransfer, csRemoteAbort };
enum TBatchOverwrite { boNo, boAll, boNone, boOlder, boAlternateResume, boAppend, boResume };
typedef void __fastcall (__closure *TFileOperationProgressEvent)
  (TFileOperationProgressType & ProgressData);
typedef void __fastcall (__closure *TFileOperationFinished)
  (TFileOperation Operation, TOperationSide Side, bool Temp,
   const UnicodeString & FileName, bool Success, bool NotCancelled, TOnceDoneOperation & OnceDoneOperation);
//---------------------------------------------------------------------------
class TFileOperationStatistics
{
public:
  TFileOperationStatistics();

  int FilesUploaded;
  int FilesDownloaded;
  int FilesDeletedLocal;
  int FilesDeletedRemote;
  __int64 TotalUploaded;
  __int64 TotalDownloaded;
};
//---------------------------------------------------------------------------
class TFileOperationProgressType
{
public:
  class TPersistence
  {
  friend class TFileOperationProgressType;
  public:
    TPersistence();
    __property TFileOperationStatistics * Statistics = { read = FStatistics, write = FStatistics };

  private:
    void Clear(bool Batch, bool Speed);

    TDateTime StartTime;
    TBatchOverwrite BatchOverwrite;
    bool SkipToAll;
    unsigned long CPSLimit;
    bool CounterSet;
    std::vector<unsigned long> Ticks;
    std::vector<__int64> TotalTransferredThen;
    TOperationSide Side;
    __int64 TotalTransferred;
    TFileOperationStatistics * FStatistics;
  };

private:
  TFileOperation FOperation;
  UnicodeString FFileName;
  UnicodeString FFullFileName;
  UnicodeString FDirectory;
  bool FAsciiTransfer;
  bool FTransferringFile;
  bool FTemp;
  __int64 FLocalSize;
  __int64 FLocallyUsed;
  __int64 FTransferSize;
  __int64 FTransferredSize;
  __int64 FSkippedSize;
  bool FInProgress;
  bool FDone;
  bool FFileInProgress;
  TCancelStatus FCancel;
  int FCount;
  __int64 FTotalTransferBase;
  __int64 FTotalSkipped;
  __int64 FTotalSize;
  bool FTotalSizeSet;
  bool FSuspended;
  bool FRestored;
  TFileOperationProgressType * FParent;

  // when it was last time suspended (to calculate suspend time in Resume())
  unsigned int FSuspendTime;
  // when current file was started being transferred
  TDateTime FFileStartTime;
  int FFilesFinished;
  int FFilesFinishedSuccessfully;
  TFileOperationProgressEvent FOnProgress;
  TFileOperationFinished FOnFinished;
  bool FReset;
  unsigned int FLastSecond;
  unsigned long FRemainingCPS;
  TOnceDoneOperation FInitialOnceDoneOperation;
  TPersistence FPersistence;
  TCriticalSection * FSection;
  TCriticalSection * FUserSelectionsSection;

  __int64 __fastcall GetTotalTransferred();
  __int64 __fastcall GetOperationTransferred() const;
  __int64 __fastcall GetTotalSize();
  unsigned long __fastcall GetCPSLimit();
  TBatchOverwrite __fastcall GetBatchOverwrite();
  bool __fastcall GetSkipToAll();
  TDateTime __fastcall GetStartTime() const { return FPersistence.StartTime; }
  TOperationSide __fastcall GetSide() const { return FPersistence.Side; }

protected:
  void __fastcall ClearTransfer();
  inline void __fastcall DoProgress();
  int __fastcall OperationProgress() const;
  void __fastcall AddTransferredToTotals(__int64 ASize);
  void __fastcall AddSkipped(__int64 ASize);
  void __fastcall AddTotalSize(__int64 ASize);
  void __fastcall RollbackTransferFromTotals(__int64 ATransferredSize, __int64 ASkippedSize);
  unsigned int __fastcall GetCPS();
  void __fastcall Init();
  static bool __fastcall PassCancelToParent(TCancelStatus ACancel);
  void __fastcall DoClear(bool Batch, bool Speed);

public:
  // common data
  __property TFileOperation Operation = { read = FOperation };
  // on what side if operation being processed (local/remote), source of copy
  __property TOperationSide Side = { read = GetSide };
  __property int Count =  { read = FCount };
  __property UnicodeString FileName =  { read = FFileName };
  __property UnicodeString FullFileName = { read = FFullFileName };
  __property UnicodeString Directory = { read = FDirectory };
  __property bool AsciiTransfer = { read = FAsciiTransfer };
  // Can be true with SCP protocol only
  __property bool TransferringFile = { read = FTransferringFile };
  __property bool Temp = { read = FTemp };

  // file size to read/write
  __property __int64 LocalSize = { read = FLocalSize };
  __property __int64 LocallyUsed = { read = FLocallyUsed };
  __property __int64 TransferSize = { read = FTransferSize };
  __property __int64 TransferredSize = { read = FTransferredSize };
  __property __int64 SkippedSize = { read = FSkippedSize };
  __property bool InProgress = { read = FInProgress };
  __property bool Done = { read = FDone };
  __property bool FileInProgress = { read = FFileInProgress };
  __property TCancelStatus Cancel = { read = GetCancel };
  // when operation started
  __property TDateTime StartTime = { read = GetStartTime };
  // bytes transferred
  __property __int64 TotalTransferred = { read = GetTotalTransferred };
  __property __int64 OperationTransferred = { read = GetOperationTransferred };
  __property __int64 TotalSize = { read = GetTotalSize };
  __property int FilesFinishedSuccessfully = { read = FFilesFinishedSuccessfully };
  __property TOnceDoneOperation InitialOnceDoneOperation = { read = FInitialOnceDoneOperation };

  __property TBatchOverwrite BatchOverwrite = { read = GetBatchOverwrite };
  __property bool SkipToAll = { read = GetSkipToAll };
  __property unsigned long CPSLimit = { read = GetCPSLimit };

  __property bool TotalSizeSet = { read = FTotalSizeSet };

  __property bool Suspended = { read = FSuspended };

  __fastcall TFileOperationProgressType();
  __fastcall TFileOperationProgressType(
    TFileOperationProgressEvent AOnProgress, TFileOperationFinished AOnFinished,
    TFileOperationProgressType * Parent = NULL);
  __fastcall ~TFileOperationProgressType();
  void __fastcall Assign(const TFileOperationProgressType & Other);
  void __fastcall AssignButKeepSuspendState(const TFileOperationProgressType & Other);
  void __fastcall AddLocallyUsed(__int64 ASize);
  void __fastcall AddTransferred(__int64 ASize, bool AddToTotals = true);
  void __fastcall AddResumed(__int64 ASize);
  void __fastcall AddSkippedFileSize(__int64 ASize);
  void __fastcall Clear();
  unsigned int __fastcall CPS();
  void __fastcall Finish(UnicodeString FileName, bool Success,
    TOnceDoneOperation & OnceDoneOperation);
  void __fastcall Succeeded(int Count = 1);
  void __fastcall Progress();
  unsigned long __fastcall LocalBlockSize();
  bool __fastcall IsLocallyDone();
  bool IsTransferDone();
  bool IsTransferDoneChecked();
  void __fastcall SetFile(UnicodeString AFileName, bool AFileInProgress = true);
  void __fastcall SetFileInProgress();
  unsigned long __fastcall TransferBlockSize();
  unsigned long __fastcall AdjustToCPSLimit(unsigned long Size);
  void __fastcall ThrottleToCPSLimit(unsigned long Size);
  static unsigned long __fastcall StaticBlockSize();
  void __fastcall Reset();
  void __fastcall Resume();
  void __fastcall SetLocalSize(__int64 ASize);
  void __fastcall SetAsciiTransfer(bool AAsciiTransfer);
  void __fastcall SetTransferSize(__int64 ASize);
  void __fastcall ChangeTransferSize(__int64 ASize);
  void __fastcall RollbackTransfer();
  void __fastcall SetTotalSize(__int64 ASize);
  void __fastcall Start(TFileOperation AOperation, TOperationSide ASide, int ACount);
  void __fastcall Start(TFileOperation AOperation,
    TOperationSide ASide, int ACount, bool ATemp, const UnicodeString ADirectory,
    unsigned long ACPSLimit, TOnceDoneOperation InitialOnceDoneOperation);
  void __fastcall Stop();
  void __fastcall SetDone();
  void __fastcall Suspend();
  void __fastcall LockUserSelections();
  void __fastcall UnlockUserSelections();
  // whole operation
  TDateTime __fastcall TimeElapsed();
  // only current file
  TDateTime __fastcall TimeExpected();
  TDateTime __fastcall TotalTimeLeft();
  int __fastcall TransferProgress();
  int __fastcall OverallProgress() const;
  int __fastcall TotalTransferProgress() const;
  void __fastcall SetSpeedCounters();
  void __fastcall SetTransferringFile(bool ATransferringFile);
  TCancelStatus __fastcall GetCancel();
  void __fastcall SetCancel(TCancelStatus ACancel);
  void __fastcall SetCancelAtLeast(TCancelStatus ACancel);
  bool __fastcall ClearCancelFile();
  void __fastcall SetCPSLimit(unsigned long ACPSLimit);
  void __fastcall SetBatchOverwrite(TBatchOverwrite ABatchOverwrite);
  void __fastcall SetSkipToAll();
  UnicodeString __fastcall GetLogStr(bool Done);
  void __fastcall Store(TPersistence & Persistence);
  void __fastcall Restore(TPersistence & Persistence);
  bool IsIndeterminate() const;
  bool IsTransfer() const;

  static bool IsIndeterminateOperation(TFileOperation Operation);
  static bool IsTransferOperation(TFileOperation Operation);
};
//---------------------------------------------------------------------------
class TSuspendFileOperationProgress
{
public:
  __fastcall TSuspendFileOperationProgress(TFileOperationProgressType * OperationProgress)
  {
    FOperationProgress = OperationProgress;
    if (FOperationProgress != NULL)
    {
      FOperationProgress->Suspend();
    }
  }

  __fastcall ~TSuspendFileOperationProgress()
  {
    if (FOperationProgress != NULL)
    {
      FOperationProgress->Resume();
    }
  }

private:
  TFileOperationProgressType * FOperationProgress;
};
//---------------------------------------------------------------------------
#endif
