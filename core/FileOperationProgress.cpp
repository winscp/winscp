//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "FileOperationProgress.h"
//---------------------------------------------------------------------------
#define TRANSFER_BUF_SIZE 4096
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
  Clear();
}
//---------------------------------------------------------------------------
__fastcall TFileOperationProgressType::~TFileOperationProgressType()
{
  assert(!InProgress && !Suspended);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Clear()
{
  FileName = "";
  AsciiTransfer = false;
  ResumeStatus = rsNotAvailable;
  Count = 0;
  FFilesFinished = 0;
  StartTime = Now();
  FStopped = 0;
  Suspended = false;
  FSuspendTime = 0;
  InProgress = false;
  TotalTransfered = 0;
  TotalSkipped = 0;
  TotalSize = 0;
  TotalSizeSet = false;
  Operation = foNone;
  DragDrop = false;
  YesToAll = false;
  YesToNewer = false;
  NoToAll = false;
  // to bypass check in ClearTransfer()
  TransferSize = 0;
  ClearTransfer();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::ClearTransfer()
{
  if ((TransferSize > 0) && (TransferedSize < TransferSize))
  {
    __int64 RemainingSide = (TransferSize - TransferedSize);
    TotalSkipped += RemainingSide;
  }
  LocalSize = 0;
  TransferSize = 0;
  LocalyUsed = 0;
  TransferedSize = 0;
  TransferingFile = false;
  FFileStopped = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Start(TFileOperation AOperation,
  TOperationSide ASide, int ACount, bool ADragDrop,
  const AnsiString ADirectory)
{
  Clear();
  Operation = AOperation;
  Side = ASide;
  Count = ACount;
  InProgress = true;
  Cancel = csContinue;
  Directory = ADirectory;
  DragDrop = ADragDrop;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Reset()
{
  InProgress = false;
  Suspended = false;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Stop()
{
  // added to include remaining bytes to TotalSkipped, in case
  // the progress happes to update before closing
  ClearTransfer();
  InProgress = false;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Suspend()
{
  assert(!Suspended);
  Suspended = true;
  FSuspendTime = Now();
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Resume()
{
  assert(Suspended);
  Suspended = false;
  TDateTime TimeSuspended = (Now() - FSuspendTime);
  // see CPS()
  FStopped += TimeSuspended;
  FFileStopped += TimeSuspended;
  DoProgress();
}
//---------------------------------------------------------------------------
int __fastcall TFileOperationProgressType::OperationProgress()
{
  assert(Count);
  return (FFilesFinished * 100)/Count;
}
//---------------------------------------------------------------------------
int __fastcall TFileOperationProgressType::TransferProgress()
{
  if (TransferSize) return (int)((TransferedSize * 100)/TransferSize);
    else return 0;
}
//---------------------------------------------------------------------------
int __fastcall TFileOperationProgressType::TotalTransferProgress()
{
  assert(TotalSizeSet);
  int Result = TotalSize > 0 ? (int)(((TotalTransfered + TotalSkipped) * 100)/TotalSize) : 0;
  return Result < 100 ? Result : 100;
}
//---------------------------------------------------------------------------
int __fastcall TFileOperationProgressType::OverallProgress()
{
  if (TotalSizeSet)
  {
    assert((Operation == foCopy) || (Operation == foMove));
    return TotalTransferProgress();
  }
  else
  {
    return OperationProgress();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::DoProgress()
{
  if (FOnProgress) FOnProgress(*this, Cancel);
} 
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::Finish(AnsiString FileName,
  bool Success, bool & DisconnectWhenComplete)
{
  assert(InProgress);

  if (FOnFinished)
  {
    FOnFinished(Operation, Side, DragDrop, FileName,
      /* TODO : There wasn't 'Success' condition, was it by mistake or by purpose? */
      Success && (Cancel == csContinue), DisconnectWhenComplete);
  }
  FFilesFinished++;
  DoProgress();
} 
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetFile(AnsiString AFileName)
{
  FileName = AFileName;
  ClearTransfer();
  FFileStartTime = Now();
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetLocalSize(__int64 ASize)
{
  LocalSize = ASize;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddLocalyUsed(__int64 ASize)
{
  LocalyUsed += ASize;
  if (LocalyUsed > LocalSize)
  {
    LocalSize = LocalyUsed;
  }
  DoProgress();
} 
//---------------------------------------------------------------------------
bool __fastcall TFileOperationProgressType::IsLocalyDone()
{
  assert(LocalyUsed <= LocalSize);
  return (LocalyUsed == LocalSize);
}
//---------------------------------------------------------------------------
unsigned long __fastcall TFileOperationProgressType::LocalBlockSize()
{
  unsigned long Result = TRANSFER_BUF_SIZE;
  if (LocalyUsed + Result > LocalSize) Result = (unsigned long)(LocalSize - LocalyUsed);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetTotalSize(__int64 ASize)
{
  TotalSize = ASize;
  TotalSizeSet = true;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetTransferSize(__int64 ASize)
{
  TransferSize = ASize;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::ChangeTransferSize(__int64 ASize)
{
  // reflect change on file size (due to text transfer mode conversion particulary)
  // on total transfer size
  if (TotalSizeSet)
  {
    TotalSize += (ASize - TransferSize); 
  }
  TransferSize = ASize;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddTransfered(__int64 ASize,
  bool AddToTotals)
{
  TransferedSize += ASize;
  if (TransferedSize > TransferSize)
  {
    // this can happen with SFTP when downloading file that
    // grows while being downloaded
    if (TotalSizeSet)
    {
      TotalSize += (TransferedSize - TransferSize); 
    }
    TransferSize = TransferedSize;
  }
  if (AddToTotals)
  {
    TotalTransfered += ASize;
  }
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddResumed(__int64 ASize)
{
  TotalSkipped += ASize;
  AddTransfered(ASize, false);
  AddLocalyUsed(ASize);
}
//---------------------------------------------------------------------------
unsigned long __fastcall TFileOperationProgressType::TransferBlockSize()
{
  unsigned long Result = TRANSFER_BUF_SIZE;
  if (TransferedSize + Result > TransferSize) Result = (unsigned long)(TransferSize - TransferedSize);
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
  assert(TransferedSize <= TransferSize);
  return (TransferedSize == TransferSize);
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetAsciiTransfer(bool AAsciiTransfer)
{
  AsciiTransfer = AAsciiTransfer;
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::SetResumeStatus(TResumeStatus AResumeStatus)
{
  ResumeStatus = AResumeStatus;
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
  TDateTime CurTime = Suspended ? FSuspendTime : Now();
  TDateTime RealTime = (CurTime - StartTime) - FStopped;
  
  if ((double)RealTime > 0)
  {
    return TotalTransfered / ((double)RealTime * (24 * 60 * 60));
  }
  else
  {
    return 0;
  }
} 
//---------------------------------------------------------------------------
TDateTime __fastcall TFileOperationProgressType::TimeExpected()
{
  unsigned int CurCps = CPS();
  if (CurCps) return TDateTime((double)(((double)(TransferSize - TransferedSize)) / CurCps) / (24 * 60 * 60));
    else return 0; 
}
//---------------------------------------------------------------------------
TDateTime __fastcall TFileOperationProgressType::TotalTimeExpected()
{
  assert(TotalSizeSet);
  unsigned int CurCps = CPS();
  // sanity check
  if ((CurCps > 0) && (TotalSize > TotalSkipped))
  {
    return TDateTime((double)((double)(TotalSize - TotalSkipped) / CurCps) /
      (24 * 60 * 60));
  }
  else
  {
    return 0;
  }
}

