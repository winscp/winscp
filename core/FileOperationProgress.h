//---------------------------------------------------------------------------
#ifndef FileOperationProgressH
#define FileOperationProgressH
//---------------------------------------------------------------------------
#include "Configuration.h"
#include "CopyParam.h"
#include <vector>
//---------------------------------------------------------------------------
class TFileOperationProgressType;
enum TFileOperation { foNone, foCopy, foMove, foDelete, foSetProperties,
  foRename, foCustomCommand, foCalculateSize, foRemoteMove, foRemoteCopy,
  foGetProperties, foCalculateChecksum };
enum TCancelStatus { csContinue = 0, csCancel, csCancelTransfer, csRemoteAbort };
enum TResumeStatus { rsNotAvailable, rsEnabled, rsDisabled };
typedef void __fastcall (__closure *TFileOperationProgressEvent)
  (TFileOperationProgressType & ProgressData, TCancelStatus & Cancel);
typedef void __fastcall (__closure *TFileOperationFinished)
  (TFileOperation Operation, TOperationSide Side, bool Temp,
    const AnsiString FileName, bool Success, bool & DisconnectWhenComplete);
//---------------------------------------------------------------------------
class TFileOperationProgressType
{
private:
  // when it was last time suspended (to calculate suspend time in Resume())
  unsigned int FSuspendTime;
  // when current file was started being transfered
  TDateTime FFileStartTime;
  int FFilesFinished;
  TFileOperationProgressEvent FOnProgress;
  TFileOperationFinished FOnFinished;
  bool FReset;
  unsigned int FLastSecond;
  unsigned long FRemainingCPS;
  std::vector<unsigned long> FTicks;
  std::vector<__int64> FTotalTransferredThen;

protected:
  void __fastcall ClearTransfer();
  void __fastcall DoProgress();

public:
  // common data
  TFileOperation Operation;
  // on what side if operation being processed (local/remote), source of copy
  TOperationSide Side;
  AnsiString FileName;
  AnsiString Directory;
  bool AsciiTransfer;
  bool TransferingFile;
  bool Temp;

  // file size to read/write
  __int64 LocalSize;
  __int64 LocalyUsed;
  __int64 TransferSize;
  __int64 TransferedSize;
  __int64 SkippedSize;
  TResumeStatus ResumeStatus;
  bool InProgress;
  TCancelStatus Cancel;
  int Count;
  // when operation started
  TDateTime StartTime;
  // bytes transfered
  __int64 TotalTransfered;
  __int64 TotalSkipped;
  __int64 TotalSize;
  bool YesToAll;
  bool YesToNewer;
  bool NoToAll;
  bool SkipToAll;
  bool AlternateResumeAlways;
  unsigned long CPSLimit;

  bool TotalSizeSet;

  bool Suspended;

  __fastcall TFileOperationProgressType();
  __fastcall TFileOperationProgressType(
    TFileOperationProgressEvent AOnProgress, TFileOperationFinished AOnFinished);
  __fastcall ~TFileOperationProgressType();
  void __fastcall AddLocalyUsed(__int64 ASize);
  void __fastcall AddTransfered(__int64 ASize, bool AddToTotals = true);
  void __fastcall AddResumed(__int64 ASize);
  void __fastcall Clear();
  unsigned int __fastcall CPS();
  void __fastcall Finish(AnsiString FileName, bool Success,
    bool & DisconnectWhenComplete);
  unsigned long __fastcall LocalBlockSize();
  bool __fastcall IsLocalyDone();
  bool __fastcall IsTransferDone();
  void __fastcall SetFile(AnsiString AFileName);
  int __fastcall OperationProgress();
  unsigned long __fastcall TransferBlockSize();
  unsigned long __fastcall AdjustToCPSLimit(unsigned long Size);
  static unsigned long __fastcall StaticBlockSize();
  void __fastcall Reset();
  void __fastcall Resume();
  void __fastcall SetLocalSize(__int64 ASize);
  void __fastcall SetAsciiTransfer(bool AAsciiTransfer);
  void __fastcall SetResumeStatus(TResumeStatus AResumeStatus);
  void __fastcall SetTransferSize(__int64 ASize);
  void __fastcall ChangeTransferSize(__int64 ASize);
  void __fastcall RollbackTransfer();
  void __fastcall SetTotalSize(__int64 ASize);
  void __fastcall Start(TFileOperation AOperation, TOperationSide ASide, int ACount);
  void __fastcall Start(TFileOperation AOperation,
    TOperationSide ASide, int ACount, bool ATemp, const AnsiString ADirectory,
    unsigned long ACPSLimit);
  void __fastcall Stop();
  void __fastcall Suspend();
  // whole operation
  TDateTime __fastcall TimeElapsed();
  // only current file
  TDateTime __fastcall TimeExpected();
  TDateTime __fastcall TotalTimeExpected();
  TDateTime __fastcall TotalTimeLeft();
  int __fastcall TransferProgress();
  int __fastcall OverallProgress();
  int __fastcall TotalTransferProgress();
};
//---------------------------------------------------------------------------
class TSuspendFileOperationProgress
{
public:
  __fastcall TSuspendFileOperationProgress(TFileOperationProgressType * OperationProgress)
  {
    FOperationProgress = OperationProgress;
    FOperationProgress->Suspend();
  }

  __fastcall ~TSuspendFileOperationProgress()
  {
    FOperationProgress->Resume();
  }

private:
  TFileOperationProgressType * FOperationProgress;
};
//---------------------------------------------------------------------------
#endif
