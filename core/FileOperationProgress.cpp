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
  Clear();
} 
//---------------------------------------------------------------------------
__fastcall TFileOperationProgressType::TFileOperationProgressType(
  TFileOperationProgressEvent AOnProgress, TFileOperationFinished AOnFinished)
{
  Clear();
  FOnProgress = AOnProgress;
  FOnFinished = AOnFinished;
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
  TotalResumed = 0;
  Operation = foNone;
  DragDrop = false;
  YesToAll = false;
  NoToAll = false;
  ClearTransfer();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::ClearTransfer()
{
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
void __fastcall TFileOperationProgressType::Stop()
{
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
    FOnFinished(Side, DragDrop, FileName,
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
  assert(LocalyUsed <= LocalSize);
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
void __fastcall TFileOperationProgressType::SetTransferSize(__int64 ASize)
{
  TransferSize = ASize;
  DoProgress();
} 
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddTransfered(__int64 ASize)
{
  TransferedSize += ASize;
  TotalTransfered += ASize;
  assert(TransferedSize <= TransferSize);
  DoProgress();
}
//---------------------------------------------------------------------------
void __fastcall TFileOperationProgressType::AddResumed(__int64 ASize)
{
  TotalResumed += ASize;
  AddTransfered(ASize);
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
    return (TotalTransfered - TotalResumed) / ((double)RealTime * (24 * 60 * 60));
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
