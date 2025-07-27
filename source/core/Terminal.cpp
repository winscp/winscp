//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Terminal.h"

#include <SysUtils.hpp>
#include <FileCtrl.hpp>
#include <StrUtils.hpp>
#include <System.IOUtils.hpp>

#include "Common.h"
#include "PuttyTools.h"
#include "FileBuffer.h"
#include "Interface.h"
#include "RemoteFiles.h"
#include "SecureShell.h"
#include "ScpFileSystem.h"
#include "SftpFileSystem.h"
#include "FtpFileSystem.h"
#include "WebDAVFileSystem.h"
#include "S3FileSystem.h"
#include "TextsCore.h"
#include "HelpCore.h"
#include "CoreMain.h"
#include "Queue.h"
#include "Cryptography.h"
#include "NeonIntf.h"
#include <PuttyTools.h>
#include <openssl/pkcs12.h>
#include <openssl/err.h>
#include <algorithm>

#ifndef AUTO_WINSOCK
#include <winsock2.h>
#endif
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_TERMINAL this
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TLoopDetector
{
public:
  __fastcall TLoopDetector();
  void __fastcall RecordVisitedDirectory(const UnicodeString & Directory);
  bool __fastcall IsUnvisitedDirectory(const UnicodeString & Directory);

private:
  std::unique_ptr<TStringList> FVisitedDirectories;
};
//---------------------------------------------------------------------------
__fastcall TLoopDetector::TLoopDetector()
{
  FVisitedDirectories.reset(CreateSortedStringList());
}
//---------------------------------------------------------------------------
void __fastcall TLoopDetector::RecordVisitedDirectory(const UnicodeString & Directory)
{
  UnicodeString VisitedDirectory = ExcludeTrailingBackslash(Directory);
  FVisitedDirectories->Add(VisitedDirectory);
}
//---------------------------------------------------------------------------
bool __fastcall TLoopDetector::IsUnvisitedDirectory(const UnicodeString & Directory)
{
  bool Result = (FVisitedDirectories->IndexOf(Directory) < 0);

  if (Result)
  {
    RecordVisitedDirectory(Directory);
  }

  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
struct TMoveFileParams
{
  UnicodeString Target;
  UnicodeString FileMask;
  bool DontOverwrite;
};
//---------------------------------------------------------------------------
struct TFilesFindParams
{
  TFileMasks FileMask;
  TFileFoundEvent OnFileFound;
  TFindingFileEvent OnFindingFile;
  bool Cancel;
  TLoopDetector LoopDetector;
  UnicodeString RealDirectory;
};
//---------------------------------------------------------------------------
TCalculateSizeStats::TCalculateSizeStats()
{
  memset(this, 0, sizeof(*this));
}
//---------------------------------------------------------------------------
TCalculateSizeParams::TCalculateSizeParams()
{
  memset(this, 0, sizeof(*this));
  Result = true;
  AllowDirs = true;
}
//---------------------------------------------------------------------------
TSynchronizeOptions::TSynchronizeOptions()
{
  memset(this, 0, sizeof(*this));
}
//---------------------------------------------------------------------------
TSynchronizeOptions::~TSynchronizeOptions()
{
  delete Filter;
}
//---------------------------------------------------------------------------
bool __fastcall TSynchronizeOptions::MatchesFilter(const UnicodeString & FileName)
{
  int FoundIndex;
  bool Result;
  if (Filter == NULL)
  {
    Result = true;
  }
  else
  {
    Result = Filter->Find(FileName, FoundIndex);
  }
  return Result;
}
//---------------------------------------------------------------------------
TSpaceAvailable::TSpaceAvailable()
{
  memset(this, 0, sizeof(*this));
}
//---------------------------------------------------------------------------
TOverwriteFileParams::TOverwriteFileParams()
{
  SourceSize = 0;
  DestSize = 0;
  SourcePrecision = mfFull;
  DestPrecision = mfFull;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TTunnelThread : public TSimpleThread
{
public:
  __fastcall TTunnelThread(TSecureShell * SecureShell);
  virtual __fastcall ~TTunnelThread();

  virtual void __fastcall Terminate();

protected:
  virtual void __fastcall Execute();

private:
  TSecureShell * FSecureShell;
  bool FTerminated;
};
//---------------------------------------------------------------------------
__fastcall TTunnelThread::TTunnelThread(TSecureShell * SecureShell) :
  FSecureShell(SecureShell),
  FTerminated(false)
{
  Start();
}
//---------------------------------------------------------------------------
__fastcall TTunnelThread::~TTunnelThread()
{
  // close before the class's virtual functions (Terminate particularly) are lost
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TTunnelThread::Terminate()
{
  FTerminated = true;
}
//---------------------------------------------------------------------------
void __fastcall TTunnelThread::Execute()
{
  try
  {
    while (!FTerminated)
    {
      FSecureShell->Idle(250);
    }
  }
  catch(...)
  {
    if (FSecureShell->Active)
    {
      FSecureShell->Close();
    }
    // do not pass exception out of thread's proc
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TTunnelUI : public TSessionUI
{
public:
  __fastcall TTunnelUI(TTerminal * Terminal);
  virtual void __fastcall Information(const UnicodeString & Str);
  virtual unsigned int __fastcall QueryUser(const UnicodeString Query,
    TStrings * MoreMessages, unsigned int Answers, const TQueryParams * Params,
    TQueryType QueryType);
  virtual unsigned int __fastcall QueryUserException(const UnicodeString Query,
    Exception * E, unsigned int Answers, const TQueryParams * Params,
    TQueryType QueryType);
  virtual bool __fastcall PromptUser(TSessionData * Data, TPromptKind Kind,
    UnicodeString Name, UnicodeString Instructions, TStrings * Prompts,
    TStrings * Results);
  virtual void __fastcall DisplayBanner(const UnicodeString & Banner);
  virtual void __fastcall FatalError(Exception * E, UnicodeString Msg, UnicodeString HelpContext);
  virtual void __fastcall HandleExtendedException(Exception * E);
  virtual void __fastcall Closed();
  virtual void __fastcall ProcessGUI();

private:
  TTerminal * FTerminal;
  unsigned int FTerminalThread;
};
//---------------------------------------------------------------------------
__fastcall TTunnelUI::TTunnelUI(TTerminal * Terminal)
{
  FTerminal = Terminal;
  FTerminalThread = GetCurrentThreadId();
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::Information(const UnicodeString & Str)
{
  if (GetCurrentThreadId() == FTerminalThread)
  {
    FTerminal->Information(Str);
  }
}
//---------------------------------------------------------------------------
unsigned int __fastcall TTunnelUI::QueryUser(const UnicodeString Query,
  TStrings * MoreMessages, unsigned int Answers, const TQueryParams * Params,
  TQueryType QueryType)
{
  unsigned int Result;
  if (GetCurrentThreadId() == FTerminalThread)
  {
    Result = FTerminal->QueryUser(Query, MoreMessages, Answers, Params, QueryType);
  }
  else
  {
    Result = AbortAnswer(Answers);
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned int __fastcall TTunnelUI::QueryUserException(const UnicodeString Query,
  Exception * E, unsigned int Answers, const TQueryParams * Params,
  TQueryType QueryType)
{
  unsigned int Result;
  if (GetCurrentThreadId() == FTerminalThread)
  {
    Result = FTerminal->QueryUserException(Query, E, Answers, Params, QueryType);
  }
  else
  {
    Result = AbortAnswer(Answers);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTunnelUI::PromptUser(TSessionData * Data, TPromptKind Kind,
  UnicodeString Name, UnicodeString Instructions, TStrings * Prompts, TStrings * Results)
{
  bool Result;
  if (GetCurrentThreadId() == FTerminalThread)
  {
    if (IsAuthenticationPrompt(Kind))
    {
      Instructions =
        FMTLOAD(TUNNEL_INSTRUCTION2, (Data->HostName)) +
        (Instructions.IsEmpty() ? L"" : L"\n") +
        Instructions;
    }

    Result = FTerminal->PromptUser(Data, Kind, Name, Instructions, Prompts, Results);
  }
  else
  {
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::DisplayBanner(const UnicodeString & Banner)
{
  if (GetCurrentThreadId() == FTerminalThread)
  {
    FTerminal->DisplayBanner(Banner);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::FatalError(Exception * E, UnicodeString Msg, UnicodeString HelpKeyword)
{
  throw ESshFatal(E, Msg, HelpKeyword);
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::HandleExtendedException(Exception * E)
{
  if (GetCurrentThreadId() == FTerminalThread)
  {
    FTerminal->HandleExtendedException(E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::Closed()
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::ProcessGUI()
{
  // noop
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TCallbackGuard
{
public:
  inline __fastcall TCallbackGuard(TTerminal * FTerminal);
  inline __fastcall ~TCallbackGuard();

  NORETURN void __fastcall FatalError(Exception * E, const UnicodeString & Msg, const UnicodeString & HelpKeyword);
  inline void __fastcall Verify();
  inline bool __fastcall Verify(Exception * E);
  void __fastcall Dismiss();

private:
  ExtException * FFatalError;
  TTerminal * FTerminal;
  bool FGuarding;
};
//---------------------------------------------------------------------------
__fastcall TCallbackGuard::TCallbackGuard(TTerminal * Terminal) :
  FFatalError(NULL),
  FTerminal(Terminal),
  FGuarding(FTerminal->FCallbackGuard == NULL)
{
  if (FGuarding)
  {
    FTerminal->FCallbackGuard = this;
  }
}
//---------------------------------------------------------------------------
__fastcall TCallbackGuard::~TCallbackGuard()
{
  if (FGuarding)
  {
    DebugAssert((FTerminal->FCallbackGuard == this) || (FTerminal->FCallbackGuard == NULL));
    FTerminal->FCallbackGuard = NULL;
  }

  delete FFatalError;
}
//---------------------------------------------------------------------------
NORETURN void __fastcall TCallbackGuard::FatalError(Exception * E, const UnicodeString & Msg, const UnicodeString & HelpKeyword)
{
  DebugAssert(FGuarding);

  // make sure we do not bother about getting back the silent abort exception
  // we issued ourselves. this may happen when there is an exception handler
  // that converts any exception to fatal one (such as in TTerminal::Open).
  if (dynamic_cast<ECallbackGuardAbort *>(E) == NULL)
  {
    delete FFatalError;
    FFatalError = new ExtException(E, Msg, HelpKeyword);
  }

  // silently abort what we are doing.
  // non-silent exception would be caught probably by default application
  // exception handler, which may not do an appropriate action
  // (particularly it will not resume broken transfer).
  throw ECallbackGuardAbort();
}
//---------------------------------------------------------------------------
void __fastcall TCallbackGuard::Dismiss()
{
  DebugAssert(FFatalError == NULL);
  FGuarding = false;
}
//---------------------------------------------------------------------------
void __fastcall TCallbackGuard::Verify()
{
  if (FGuarding)
  {
    FGuarding = false;
    DebugAssert(FTerminal->FCallbackGuard == this);
    FTerminal->FCallbackGuard = NULL;

    if (FFatalError != NULL)
    {
      throw ESshFatal(FFatalError, L"");
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCallbackGuard::Verify(Exception * E)
{
  bool Result =
    (dynamic_cast<ECallbackGuardAbort *>(E) != NULL);
  if (Result)
  {
    DebugAssert(FGuarding);
    DebugAssert(FFatalError != NULL);
    Verify();
  }
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TRobustOperationLoop::TRobustOperationLoop(
  TTerminal * Terminal, TFileOperationProgressType * OperationProgress, bool * AnyTransfer, bool CanRetry) :
  FTerminal(Terminal),
  FOperationProgress(OperationProgress),
  FRetry(false),
  FAnyTransfer(AnyTransfer),
  FCanRetry(CanRetry)
{
  if (FAnyTransfer != NULL)
  {
    FPrevAnyTransfer = *FAnyTransfer;
    *FAnyTransfer = false;
    FStart = Now();
  }
}
//---------------------------------------------------------------------------
TRobustOperationLoop::~TRobustOperationLoop()
{
  if (FAnyTransfer != NULL)
  {
    *FAnyTransfer = FPrevAnyTransfer;
  }
}
//---------------------------------------------------------------------------
bool TRobustOperationLoop::TryReopen(Exception & E)
{
  if (!FCanRetry)
  {
    FRetry = false;
  }
  else if (dynamic_cast<ESkipFile *>(&E) != NULL)
  {
    FRetry = false;
  }
  else if (FTerminal->Active)
  {
    FTerminal->LogEvent(1, L"Session is open, will not retry transfer");
    FRetry = false;
  }
  else
  {
    FRetry = true;
    if (FAnyTransfer != NULL)
    {
      // if there was any transfer, reset the global timestamp
      if (*FAnyTransfer)
      {
        FStart = Now();
        FPrevAnyTransfer = true;
        *FAnyTransfer = false;
      }
      else
      {
        FRetry = FTerminal->ContinueReopen(FStart);
        if (!FRetry)
        {
          FTerminal->LogEvent(L"Retry interval expired, will not retry transfer");
        }
      }
    }

    if (FRetry)
    {
      FRetry =
        FTerminal->QueryReopen(&E, ropNoReadDirectory, FOperationProgress);
    }
  }
  return FRetry;
}
//---------------------------------------------------------------------------
bool TRobustOperationLoop::ShouldRetry()
{
  return FRetry;
}
//---------------------------------------------------------------------------
bool TRobustOperationLoop::Retry()
{
  bool Result = FRetry;
  FRetry = false;
  return Result;
}
//---------------------------------------------------------------------------
class TRetryOperationLoop
{
public:
  TRetryOperationLoop(TTerminal * Terminal);

  void Error(Exception & E);
  void Error(Exception & E, TSessionAction & Action);
  void Error(Exception & E, const UnicodeString & Message);
  void Error(Exception & E, TSessionAction & Action, const UnicodeString & Message);
  bool Retry();
  bool Succeeded();

private:
  TTerminal * FTerminal;
  bool FRetry;
  bool FSucceeded;

  void DoError(Exception & E, TSessionAction * Action, const UnicodeString & Message);
};
//---------------------------------------------------------------------------
TRetryOperationLoop::TRetryOperationLoop(TTerminal * Terminal)
{
  FTerminal = Terminal;
  FRetry = false;
  FSucceeded = true;
}
//---------------------------------------------------------------------------
void TRetryOperationLoop::DoError(Exception & E, TSessionAction * Action, const UnicodeString & Message)
{
  FSucceeded = false;
  // Note that the action may already be canceled when RollbackAction is called
  unsigned int Result;
  try
  {
    Result = FTerminal->CommandError(&E, Message, qaRetry | qaSkip | qaAbort);
  }
  catch (Exception & E2)
  {
    if (Action != NULL)
    {
      FTerminal->RollbackAction(*Action, NULL, &E2);
    }
    throw;
  }

  switch (Result)
  {
    case qaRetry:
      FRetry = true;
      if (Action != NULL)
      {
        Action->Cancel();
      }
      break;

    case qaAbort:
      if (Action != NULL)
      {
        FTerminal->RollbackAction(*Action, NULL, &E);
      }
      Abort();
      break;

    case qaSkip:
      if (Action != NULL)
      {
        Action->Cancel();
      }
      break;

    default:
      DebugFail();
      break;
  }
}
//---------------------------------------------------------------------------
void TRetryOperationLoop::Error(Exception & E)
{
  DoError(E, NULL, UnicodeString());
}
//---------------------------------------------------------------------------
void TRetryOperationLoop::Error(Exception & E, TSessionAction & Action)
{
  DoError(E, &Action, UnicodeString());
}
//---------------------------------------------------------------------------
void TRetryOperationLoop::Error(Exception & E, const UnicodeString & Message)
{
  DoError(E, NULL, Message);
}
//---------------------------------------------------------------------------
void TRetryOperationLoop::Error(Exception & E, TSessionAction & Action, const UnicodeString & Message)
{
  DoError(E, &Action, Message);
}
//---------------------------------------------------------------------------
bool TRetryOperationLoop::Retry()
{
  bool Result = FRetry;
  FRetry = false;
  if (Result)
  {
    FSucceeded = true;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool TRetryOperationLoop::Succeeded()
{
  return FSucceeded;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TCollectedFileList::TCollectedFileList()
{
}
//---------------------------------------------------------------------------
__fastcall TCollectedFileList::~TCollectedFileList()
{
  for (size_t Index = 0; Index < FList.size(); Index++)
  {
    Deleting(Index);
  }
}
//---------------------------------------------------------------------------
void TCollectedFileList::Deleting(int Index)
{
  delete FList[Index].Object;
}
//---------------------------------------------------------------------------
int TCollectedFileList::Add(const UnicodeString & FileName, TObject * Object, bool Dir)
{
  TFileData Data;
  Data.FileName = FileName;
  Data.Object = Object;
  Data.Dir = Dir;
  Data.Recursed = true;
  Data.State = 0;
  FList.push_back(Data);
  return Count() - 1;
}
//---------------------------------------------------------------------------
void TCollectedFileList::DidNotRecurse(int Index)
{
  FList[Index].Recursed = false;
}
//---------------------------------------------------------------------------
void TCollectedFileList::Delete(int Index)
{
  Deleting(Index);
  FList.erase(FList.begin() + Index);
}
//---------------------------------------------------------------------------
int TCollectedFileList::Count() const
{
  return FList.size();
}
//---------------------------------------------------------------------------
UnicodeString TCollectedFileList::GetFileName(int Index) const
{
  return FList[Index].FileName;
}
//---------------------------------------------------------------------------
TObject * TCollectedFileList::GetObject(int Index) const
{
  return FList[Index].Object;
}
//---------------------------------------------------------------------------
bool TCollectedFileList::IsDir(int Index) const
{
  return FList[Index].Dir;
}
//---------------------------------------------------------------------------
bool TCollectedFileList::IsRecursed(int Index) const
{
  return FList[Index].Recursed;
}
//---------------------------------------------------------------------------
int TCollectedFileList::GetState(int Index) const
{
  return FList[Index].State;
}
//---------------------------------------------------------------------------
void TCollectedFileList::SetState(int Index, int State)
{
  FList[Index].State = State;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TParallelOperation::TParallelOperation(TOperationSide Side)
{
  FCopyParam = NULL;
  FParams = 0;
  FProbablyEmpty = false;
  FClients = 0;
  FMainOperationProgress = NULL;
  DebugAssert((Side == osLocal) || (Side == osRemote));
  FSide = Side;
  FVersion = 0;
}
//---------------------------------------------------------------------------
void TParallelOperation::Init(
  TStrings * AFileList, const UnicodeString & TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * MainOperationProgress, const UnicodeString & MainName,
  __int64 ParallelFileSize)
{
  DebugAssert(FFileList.get() == NULL);
  // More lists should really happen in scripting only, which does not support parallel transfers atm.
  // But in general the code should work with more lists anyway, it just was not tested for it.
  DebugAssert(AFileList->Count == 1);
  FFileList.reset(AFileList);
  FSection.reset(new TCriticalSection());
  FTargetDir = TargetDir;
  FCopyParam = CopyParam;
  FParams = Params;
  FMainOperationProgress = MainOperationProgress;
  FMainName = MainName;
  FListIndex = 0;
  FIndex = 0;
  FIsParallelFileTransfer = (ParallelFileSize >= 0);
  FParallelFileSize = ParallelFileSize;
  FParallelFileOffset = 0;
  FParallelFileCount = 0;
  FParallelFileMerging = false;
  FParallelFileMerged = 0;
}
//---------------------------------------------------------------------------
TParallelOperation::~TParallelOperation()
{
  WaitFor();
}
//---------------------------------------------------------------------------
bool TParallelOperation::IsInitialized()
{
  return (FMainOperationProgress != NULL);
}
//---------------------------------------------------------------------------
bool TParallelOperation::ShouldAddClient()
{
  bool Result;
  // TTransferQueueItem::ProgressUpdated() already checks IsInitialized() before calling this
  if (!IsInitialized())
  {
    Result = false;
  }
  else
  {
    TGuard Guard(FSection.get());
    Result = !FProbablyEmpty && (FMainOperationProgress->Cancel < csCancel);
  }
  return Result;
}
//---------------------------------------------------------------------------
void TParallelOperation::AddClient()
{
  TGuard Guard(FSection.get());
  FClients++;
}
//---------------------------------------------------------------------------
void TParallelOperation::RemoveClient()
{
  TGuard Guard(FSection.get());
  FClients--;
}
//---------------------------------------------------------------------------
void TParallelOperation::WaitFor()
{
  // Even initialized?
  // Won't be, when parallel transfers were not possible (like when preserving of directory timestamps is enabled)
  if (FSection.get() != NULL)
  {
    bool Done;

    do
    {
      {
        TGuard Guard(FSection.get());
        Done = (FClients == 0);
      }

      if (!Done)
      {
        // propagate the total progress incremented by the parallel operations
        FMainOperationProgress->Progress();
        Sleep(200);
      }
    }
    while (!Done);
    FProbablyEmpty = true;
  }
  else
  {
    DebugAssert(FClients == 0);
  }

}
//---------------------------------------------------------------------------
void TParallelOperation::Done(
  const UnicodeString & FileName, bool Dir, bool Success, const UnicodeString & TargetDir,
  const TCopyParamType * CopyParam, TTerminal * Terminal)
{
  if (Dir)
  {
    TGuard Guard(FSection.get());

    TDirectories::iterator DirectoryIterator = FDirectories.find(FileName);
    if (DebugAlwaysTrue(DirectoryIterator != FDirectories.end()))
    {
      if (Success)
      {
        DebugAssert(!DirectoryIterator->second.Exists);
        DirectoryIterator->second.Exists = true;
      }
      else
      {
        // This is actually not useful at the moment, as when creating directory fails and "Skip" is pressed,
        // the current code in CopyToRemote/CreateDirectory will behave as, if it succeded, so Success will be true here.
        FDirectories.erase(DirectoryIterator);
        if (FFileList->Count > FListIndex)
        {
          UnicodeString FileNameWithSlash;
          if (FSide == osLocal)
          {
            FileNameWithSlash = IncludeTrailingBackslash(FileName);
          }
          else
          {
            FileNameWithSlash = UnixIncludeTrailingBackslash(FileName);
          }

          // It can actually be a different list than the one the directory was taken from,
          // but that does not matter that much. It should not happen anyway, as more lists should be in scripting only.
          TCollectedFileList * Files = GetFileList(FListIndex);
          int Index = 0;
          while (Index < Files->Count())
          {
            if (StartsText(FileNameWithSlash, Files->GetFileName(Index)))
            {
              // We should add the file to "skip" counters in the OperationProgress,
              // but an interactive foreground transfer is not doing that either yet.
              Files->Delete(Index);
              // Force re-update
              FVersion++;
              if (Index < FIndex)
              {
                FIndex--;
              }
            }
            else
            {
              Index++;
            }
          }
        }
      }
    }
  }
  else
  {
    if (IsParallelFileTransfer)
    {
      TGuard Guard(FSection.get());

      try
      {
        if (Success && DebugAlwaysTrue(FSide == osRemote))
        {
          TParallelFileOffsets::const_iterator I = std::find(FParallelFileOffsets.begin(), FParallelFileOffsets.end(), CopyParam->PartOffset);
          if (DebugAlwaysTrue(I != FParallelFileOffsets.end()))
          {
            int Index = I - FParallelFileOffsets.begin();
            DebugAssert(!FParallelFileDones[Index]);
            FParallelFileDones[Index] = true;

            if (!FParallelFileMerging)
            {
              // Once we obtain "merging" semaphore, we won't leave until everything is merged
              TAutoFlag MergingFlag(FParallelFileMerging);

              try
              {
                UnicodeString TargetName = CombinePaths(TargetDir, FParallelFileTargetName);
                UnicodeString TargetNamePartial = TargetName + PartialExt;
                UnicodeString TargetNamePartialOnly = ExtractFileName(TargetNamePartial);

                while (true)
                {
                  if ((FParallelFileMerged >= FParallelFileCount) || !FParallelFileDones[FParallelFileMerged])
                  {
                    break;
                  }
                  else
                  {
                    TUnguard Unguard(FSection.get());

                    UnicodeString FileNameOnly = UnixExtractFileName(FileName);
                    // Safe as write access to FParallelFileMerged is guarded by FParallelFileMerging
                    int Index = FParallelFileMerged;
                    UnicodeString TargetPartName = GetPartPrefix(CombinePaths(TargetDir, FileNameOnly)) + IntToStr(Index);

                    if ((CopyParam->PartSize >= 0) && (Terminal->OperationProgress->TransferredSize != CopyParam->PartSize))
                    {
                      UnicodeString TransferredSizeStr = IntToStr(Terminal->OperationProgress->TransferredSize);
                      UnicodeString PartSizeStr = IntToStr(CopyParam->PartSize);
                      UnicodeString Message =
                        FMTLOAD(INCONSISTENT_SIZE, (TargetPartName, TransferredSizeStr, PartSizeStr));
                      Terminal->TerminalError(NULL, Message);
                    }

                    if (Index == 0)
                    {
                      Terminal->LogEvent(FORMAT(L"Renaming part %d of \"%s\" to \"%s\"...", (Index, FileNameOnly, TargetNamePartialOnly)));
                      Terminal->DoRenameLocalFileForce(TargetPartName, TargetNamePartial);
                    }
                    else
                    {
                      {
                        Terminal->LogEvent(FORMAT(L"Appending part %d of \"%s\" to \"%s\"...", (Index, FileNameOnly, TargetNamePartialOnly)));
                        std::unique_ptr<THandleStream> SourceStream(TSafeHandleStream::CreateFromFile(TargetPartName, fmOpenRead | fmShareDenyWrite));
                        std::unique_ptr<THandleStream> DestStream(TSafeHandleStream::CreateFromFile(TargetNamePartial, fmOpenWrite | fmShareDenyWrite));
                        HANDLE DestHandle = reinterpret_cast<HANDLE>(DestStream->Handle);
                        FILETIME WrTime;
                        bool GotWrTime = GetFileTime(DestHandle, NULL, NULL, &WrTime);
                        DestStream->Seek(0L, soEnd);
                        DestStream->CopyFrom(SourceStream.get(), SourceStream->Size);
                        if (GotWrTime)
                        {
                          SetFileTime(DestHandle, NULL, NULL, &WrTime);
                        }
                      }

                      Terminal->DoDeleteLocalFile(TargetPartName);
                    }

                    FParallelFileMerged++;
                  }
                }

                if ((FParallelFileMerged == FParallelFileCount) && (FParallelFileOffset == FParallelFileSize))
                {
                  Terminal->LogEvent(FORMAT(L"Renaming completed \"%s\" to \"%s\"...", (TargetNamePartialOnly, FParallelFileTargetName)));
                  Terminal->DoRenameLocalFileForce(TargetNamePartial, TargetName);
                }
              }
              catch (...)
              {
                Success = false;
                throw;
              }
            }
          }
        }
      }
      __finally
      {
        if (!Success)
        {
          // If any part fails, cancel whole transfer.
          // We should get here only after the user sees/acknowledges the failure.
          FMainOperationProgress->SetCancel(csCancel);
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
bool TParallelOperation::CheckEnd(TCollectedFileList * Files)
{
  bool Result = (FIndex >= Files->Count());
  if (Result)
  {
    FListIndex++;
    FIndex = 0;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool TParallelOperation::GetOnlyFile(TStrings * FileList, UnicodeString & FileName, TObject *& Object)
{
  bool Result = (FileList->Count == 1);
  if (Result)
  {
    TCollectedFileList * OnlyFileList = GetFileList(FileList, 0);
    Result = (OnlyFileList->Count() == 1) && !OnlyFileList->IsDir(0);
    if (Result)
    {
      FileName = OnlyFileList->GetFileName(0);
      Object = OnlyFileList->GetObject(0);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
TCollectedFileList * TParallelOperation::GetFileList(TStrings * FileList, int Index)
{
  return DebugNotNull(dynamic_cast<TCollectedFileList *>(FileList->Objects[Index]));
}
//---------------------------------------------------------------------------
TCollectedFileList * TParallelOperation::GetFileList(int Index)
{
  return GetFileList(FFileList.get(), Index);
}
//---------------------------------------------------------------------------
UnicodeString TParallelOperation::GetPartPrefix(const UnicodeString & FileName)
{
  return FORMAT(L"%s%s.", (FileName, PartialExt));
}
//---------------------------------------------------------------------------
int TParallelOperation::GetNext(
  TTerminal * Terminal, UnicodeString & FileName, TObject *& Object, UnicodeString & TargetDir, bool & Dir,
  bool & Recursed, TCopyParamType *& CustomCopyParam)
{
  TGuard Guard(FSection.get());
  int Result = 1;
  TCollectedFileList * Files;
  do
  {
    if (FFileList->Count > FListIndex)
    {
      Files = GetFileList(FListIndex);
      // can happen if the file was excluded by file mask
      if (CheckEnd(Files))
      {
        Files = NULL;
      }
    }
    else
    {
      Files = NULL;
      Result = -1;
    }
  }
  while ((Result == 1) && (Files == NULL));

  if (Files != NULL)
  {
    UnicodeString RootPath = FFileList->Strings[FListIndex];

    FileName = Files->GetFileName(FIndex);
    Object = Files->GetObject(FIndex);
    Dir = Files->IsDir(FIndex);
    Recursed = Files->IsRecursed(FIndex);
    UnicodeString DirPath;
    bool FirstLevel;
    if (FSide == osLocal)
    {
      DirPath = ExtractFileDir(FileName);
      FirstLevel = SamePaths(DirPath, RootPath);
    }
    else
    {
      DirPath = UnixExtractFileDir(FileName);
      FirstLevel = UnixSamePath(DirPath, RootPath);
    }

    if (FirstLevel)
    {
      TargetDir = FTargetDir;
    }
    else
    {
      TDirectories::const_iterator DirectoryIterator = FDirectories.find(DirPath);
      if (DebugAlwaysFalse(DirectoryIterator == FDirectories.end()))
      {
        throw EInvalidOperation(L"Parent path not known");
      }
      const TDirectoryData & DirectoryData = DirectoryIterator->second;
      if (!DirectoryData.Exists)
      {
        Result = 0; // wait for parent directory to be created
      }
      else
      {
        TargetDir = DirectoryData.OppositePath;
      }
    }

    if (!TargetDir.IsEmpty())
    {
      UnicodeString OnlyFileName;
      if (Dir || IsParallelFileTransfer) // optimization
      {
        if (FSide == osLocal)
        {
          OnlyFileName = ExtractFileName(FileName);
        }
        else
        {
          OnlyFileName = UnixExtractFileName(FileName);
        }
        OnlyFileName = Terminal->ChangeFileName(FCopyParam, OnlyFileName, FSide, FirstLevel);
      }

      if (Dir)
      {
        DebugAssert(!OnlyFileName.IsEmpty());
        TDirectoryData DirectoryData;
        if (FSide == osLocal)
        {
          DirectoryData.OppositePath = UnixCombinePaths(TargetDir, OnlyFileName);
        }
        else
        {
          DirectoryData.OppositePath = CombinePaths(TargetDir, OnlyFileName);
        }

        DirectoryData.Exists = false;

        FDirectories.insert(std::make_pair(FileName, DirectoryData));
      }

      bool Processed = true;
      if (IsParallelFileTransfer)
      {
        CustomCopyParam = new TCopyParamType(*FCopyParam);
        CustomCopyParam->PartOffset = FParallelFileOffset;
        __int64 Remaining = FParallelFileSize - CustomCopyParam->PartOffset;
        CustomCopyParam->PartSize = FParallelFileSize / Configuration->QueueTransfersLimit;
        DebugAssert(!OnlyFileName.IsEmpty());
        if (FParallelFileTargetName.IsEmpty())
        {
          FParallelFileTargetName = OnlyFileName;
        }
        DebugAssert(FParallelFileTargetName == OnlyFileName);
        int Index = FParallelFileCount;
        UnicodeString PartFileName = GetPartPrefix(OnlyFileName) + IntToStr(Index);
        FParallelFileCount++;
        FParallelFileOffsets.push_back(CustomCopyParam->PartOffset);
        FParallelFileDones.push_back(false);
        DebugAssert(FParallelFileOffsets.size() == static_cast<size_t>(FParallelFileCount));
        CustomCopyParam->FileMask = DelimitFileNameMask(PartFileName);
        if ((CustomCopyParam->PartSize >= Remaining) ||
            (Remaining - CustomCopyParam->PartSize < CustomCopyParam->PartSize / 10))
        {
          CustomCopyParam->PartSize = -1; // Until the end
          FParallelFileOffset = FParallelFileSize;
          Terminal->LogEvent(FORMAT(L"Starting transfer of \"%s\" part %d from %s until the EOF", (OnlyFileName, Index, IntToStr(CustomCopyParam->PartOffset))));
        }
        else
        {
          Processed = false;
          FParallelFileOffset += CustomCopyParam->PartSize;
          Terminal->LogEvent(FORMAT(L"Starting transfer of \"%s\" part %d from %s, length %s", (OnlyFileName, Index, IntToStr(CustomCopyParam->PartOffset), IntToStr(CustomCopyParam->PartSize))));
        }
      }

      if (Processed)
      {
        // The current implementation of UpdateFileList relies on this specific way of changing state
        // (all files before FIndex one-by-one)
        Files->SetState(FIndex, qfsProcessed);
        FIndex++;
        CheckEnd(Files);
      }
    }
  }

  FProbablyEmpty = (FFileList->Count == FListIndex);

  return Result;
}
//---------------------------------------------------------------------------
bool TParallelOperation::UpdateFileList(TQueueFileList * UpdateFileList)
{
  TGuard Guard(FSection.get());

  bool Result =
    (UpdateFileList->FLastParallelOperation != this) ||
    (UpdateFileList->FLastParallelOperationVersion != FVersion);

  DebugAssert(FFileList->Count == 1);
  TCollectedFileList * Files = GetFileList(0);

  if (!Result && (UpdateFileList->GetCount() != Files->Count()))
  {
    DebugAssert(false);
    Result = true;
  }

  if (Result)
  {
    UpdateFileList->Clear();
    for (int Index = 0; Index < Files->Count(); Index++)
    {
      UpdateFileList->Add(Files->GetFileName(Index), Files->GetState(Index));
    }
  }
  else
  {
    int Index = ((FListIndex == 0) ? FIndex : Files->Count()) - 1;
    while ((Index >= 0) && (UpdateFileList->GetState(Index) != Files->GetState(Index)))
    {
      UpdateFileList->SetState(Index, Files->GetState(Index));
      Index--;
      Result = true;
    }
  }

  UpdateFileList->FLastParallelOperation = this;
  UpdateFileList->FLastParallelOperationVersion = FVersion;

  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TTerminal::TTerminal(TSessionData * SessionData, TConfiguration * Configuration, TActionLog * ActionLog)
{
  FConfiguration = Configuration;
  FSessionData = new TSessionData(L"");
  FSessionData->Assign(SessionData);
  // Cache it, in case it changes (particularly by ConfigureTunnel)
  FPasswordEncryptionKey = SessionData->GetSessionPasswordEncryptionKey();
  TDateTime Started = Now(); // use the same time for session and XML log
  FLog = new TSessionLog(this, Started, FSessionData, Configuration);
  if (ActionLog != NULL)
  {
    FActionLog = ActionLog;
    FActionLogOwned = false;
  }
  else
  {
    FActionLog = new TActionLog(this, Started, FSessionData, Configuration);
    FActionLogOwned = true;
  }
  FFiles = new TRemoteDirectory(this);
  FExceptionOnFail = 0;
  FInTransaction = 0;
  FReadCurrentDirectoryPending = false;
  FReadDirectoryPending = false;
  FUsersGroupsLookedup = False;
  FTunnelLocalPortNumber = 0;
  FFileSystem = NULL;
  FSecureShell = NULL;
  FOnProgress = NULL;
  FOnFinished = NULL;
  FOnDeleteLocalFile = NULL;
  FOnReadDirectoryProgress = NULL;
  FOnQueryUser = NULL;
  FOnPromptUser = NULL;
  FOnDisplayBanner = NULL;
  FOnShowExtendedException = NULL;
  FOnInformation = NULL;
  FOnCustomCommand = NULL;
  FOnClose = NULL;
  FOnFindingFile = NULL;
  FOperationProgressPersistence = NULL;
  FOperationProgressOnceDoneOperation = odoIdle;

  FUseBusyCursor = True;
  FDirectoryCache = new TRemoteDirectoryCache();
  FDirectoryChangesCache = NULL;
  FFSProtocol = cfsUnknown;
  FCommandSession = NULL;
  FAutoReadDirectory = true;
  FReadingCurrentDirectory = false;
  FStatus = ssClosed;
  FOpening = 0;
  FTunnelThread = NULL;
  FTunnel = NULL;
  FTunnelData = NULL;
  FTunnelLog = NULL;
  FTunnelUI = NULL;
  FTunnelOpening = false;
  FCallbackGuard = NULL;
  FNesting = 0;
  FRememberedPasswordKind = TPromptKind(-1);
  FSecondaryTerminals = 0;
}
//---------------------------------------------------------------------------
__fastcall TTerminal::~TTerminal()
{
  if (Active)
  {
    Close();
  }

  if (FCallbackGuard != NULL)
  {
    // see TTerminal::HandleExtendedException
    FCallbackGuard->Dismiss();
  }
  DebugAssert(FTunnel == NULL);

  SAFE_DESTROY(FCommandSession);

  if (SessionData->CacheDirectoryChanges && SessionData->PreserveDirectoryChanges &&
      (FDirectoryChangesCache != NULL))
  {
    Configuration->SaveDirectoryChangesCache(SessionData->SessionKey,
      FDirectoryChangesCache);
  }

  SAFE_DESTROY_EX(TCustomFileSystem, FFileSystem);
  SAFE_DESTROY_EX(TSessionLog, FLog);
  if (FActionLogOwned)
  {
    SAFE_DESTROY_EX(TActionLog, FActionLog);
  }
  else
  {
    FActionLog = NULL;
  }
  delete FFiles;
  delete FDirectoryCache;
  delete FDirectoryChangesCache;
  SAFE_DESTROY(FSessionData);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Idle()
{
  // Once we disconnect, do nothing, until reconnect handler
  // "receives the information".
  // Never go idle when called from within ::ProcessGUI() call
  // as we may recurse for good, timeouting eventually.
  if (Active && (FNesting == 0))
  {
    TAutoNestingCounter NestingCounter(FNesting);

    if (Configuration->ActualLogProtocol >= 1)
    {
      LogEvent(L"Session upkeep");
    }

    DebugAssert(FFileSystem != NULL);
    FFileSystem->Idle();

    if (CommandSessionOpened)
    {
      try
      {
        FCommandSession->Idle();
      }
      catch(Exception & E)
      {
        // If the secondary session is dropped, ignore the error and let
        // it be reconnected when needed.
        // BTW, non-fatal error can hardly happen here, that's why
        // it is displayed, because it can be useful to know.
        if (FCommandSession->Active)
        {
          FCommandSession->HandleExtendedException(&E);
        }
      }
    }
  }
}
//---------------------------------------------------------------------
RawByteString __fastcall TTerminal::EncryptPassword(const UnicodeString & Password)
{
  return Configuration->EncryptPassword(Password, FPasswordEncryptionKey);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TTerminal::DecryptPassword(const RawByteString & Password)
{
  UnicodeString Result;
  try
  {
    Result = Configuration->DecryptPassword(Password, FPasswordEncryptionKey);
  }
  catch(EAbort &)
  {
    // silently ignore aborted prompts for master password and return empty password
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::RecryptPasswords()
{
  FSessionData->RecryptPasswords();
  FRememberedPassword = EncryptPassword(GetRememberedPassword());
  FRememberedTunnelPassword = EncryptPassword(GetRememberedTunnelPassword());
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::ExpandFileName(UnicodeString Path,
  const UnicodeString BasePath)
{
  // replace this by AbsolutePath()
  Path = UnixExcludeTrailingBackslash(Path);
  if (!UnixIsAbsolutePath(Path) && !BasePath.IsEmpty())
  {
    // TODO: Handle more complicated cases like "../../xxx"
    if (Path == PARENTDIRECTORY)
    {
      Path = UnixExcludeTrailingBackslash(UnixExtractFilePath(
        UnixExcludeTrailingBackslash(BasePath)));
    }
    else
    {
      Path = UnixIncludeTrailingBackslash(BasePath) + Path;
    }
  }
  return Path;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetActive()
{
  return (FFileSystem != NULL) && FFileSystem->GetActive();
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Close()
{
  FFileSystem->Close();

  // Cannot rely on CommandSessionOpened here as Status is set to ssClosed too late
  if ((FCommandSession != NULL) && FCommandSession->Active)
  {
    // prevent recursion
    FCommandSession->OnClose = NULL;
    FCommandSession->Close();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ResetConnection()
{
  // used to be called from Reopen(), why?
  FTunnelError = L"";

  FRememberedPasswordTried = false;
  FRememberedTunnelPasswordTried = false;

  if (FDirectoryChangesCache != NULL)
  {
    delete FDirectoryChangesCache;
    FDirectoryChangesCache = NULL;
  }

  FFiles->Directory = L"";
  // note that we cannot clear contained files
  // as they can still be referenced in the GUI atm
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FingerprintScan(UnicodeString & SHA256, UnicodeString & SHA1, UnicodeString & MD5)
{
  SessionData->FingerprintScan = true;
  try
  {
    Open();
    // we should never get here
    DebugFail();
    Abort();
  }
  catch (...)
  {
    if (!FFingerprintScannedSHA256.IsEmpty() ||
        !FFingerprintScannedSHA1.IsEmpty() ||
        !FFingerprintScannedMD5.IsEmpty())
    {
      SHA256 = FFingerprintScannedSHA256;
      SHA1 = FFingerprintScannedSHA1;
      MD5 = FFingerprintScannedMD5;
    }
    else
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Open()
{
  AnySession = true;
  Configuration->Usage->Inc(L"SessionOpens");
  TAutoNestingCounter OpeningCounter(FOpening);
  ReflectSettings();
  try
  {
    FEncryptKey = HexToBytes(FSessionData->EncryptKey);
    if (IsEncryptingFiles())
    {
      ValidateEncryptKey(FEncryptKey);
    }

    DoInformation(L"", 1);
    try
    {
      FRememberedPasswordUsed = false;
      try
      {
        ResetConnection();
        FStatus = ssOpening;

        try
        {
          if (FFileSystem == NULL)
          {
            Log->AddSystemInfo();
            DoInitializeLog();
            Log->AddStartupInfo();
          }

          DebugAssert(FTunnel == NULL);
          if (FSessionData->Tunnel)
          {
            Information(LoadStr(OPEN_TUNNEL));
            LogEvent(L"Opening tunnel.");
            OpenTunnel();
            Log->AddSeparator();

            FSessionData->ConfigureTunnel(FTunnelLocalPortNumber);

            Information(LoadStr(USING_TUNNEL));
            LogEvent(FORMAT(L"Connecting via tunnel interface %s:%d.",
              (FSessionData->HostNameExpanded, FSessionData->PortNumber)));
          }
          else
          {
            DebugAssert(FTunnelLocalPortNumber == 0);
          }

          if (FFileSystem == NULL)
          {
            if (SessionData->FSProtocol == fsFTP)
            {
              FFSProtocol = cfsFTP;
              FFileSystem = new TFTPFileSystem(this);
              FFileSystem->Open();
              Log->AddSeparator();
              LogEvent(L"Using FTP protocol.");
            }
            else if (SessionData->FSProtocol == fsWebDAV)
            {
              FFSProtocol = cfsWebDAV;
              FFileSystem = new TWebDAVFileSystem(this);
              FFileSystem->Open();
              Log->AddSeparator();
              LogEvent(L"Using WebDAV protocol.");
            }
            else if (SessionData->FSProtocol == fsS3)
            {
              FFSProtocol = cfsS3;
              FFileSystem = new TS3FileSystem(this);
              FFileSystem->Open();
              Log->AddSeparator();
              LogEvent(L"Using S3 protocol.");
            }
            else
            {
              DebugAssert(FSecureShell == NULL);
              try
              {
                FSecureShell = new TSecureShell(this, FSessionData, Log, Configuration);
                try
                {
                  // there will be only one channel in this session
                  FSecureShell->Simple = true;
                  FSecureShell->Open();
                }
                catch(Exception & E)
                {
                  DebugAssert(!FSecureShell->Active);
                  if (SessionData->FingerprintScan)
                  {
                    FSecureShell->GetHostKeyFingerprint(FFingerprintScannedSHA256, FFingerprintScannedMD5);
                    FFingerprintScannedSHA1 = UnicodeString();
                  }
                  // Tunnel errors that happens only once we connect to the local port (server-side forwarding problems).
                  // Contrary to local side problems handled already in OpenTunnel.
                  if (!FSecureShell->Active && !FTunnelError.IsEmpty())
                  {
                    // the only case where we expect this to happen (first in GUI, the latter in scripting)
                    DebugAssert((E.Message == LoadStr(UNEXPECTED_CLOSE_ERROR)) || (E.Message == LoadStr(NET_TRANSL_CONN_ABORTED)) || (E.Message.Pos(LoadStr(NOT_CONNECTED)) > 0));
                    FatalError(&E, FMTLOAD(TUNNEL_ERROR, (FTunnelError)));
                  }
                  else
                  {
                    throw;
                  }
                }

                Log->AddSeparator();

                if ((SessionData->FSProtocol == fsSCPonly) ||
                    (SessionData->FSProtocol == fsSFTP && FSecureShell->SshFallbackCmd()))
                {
                  FFSProtocol = cfsSCP;
                  FFileSystem = new TSCPFileSystem(this, FSecureShell);
                  FSecureShell = NULL; // ownership passed
                  LogEvent(L"Using SCP protocol.");
                }
                else
                {
                  FFSProtocol = cfsSFTP;
                  FFileSystem = new TSFTPFileSystem(this, FSecureShell);
                  FSecureShell = NULL; // ownership passed
                  LogEvent(L"Using SFTP protocol.");
                }
              }
              __finally
              {
                delete FSecureShell;
                FSecureShell = NULL;
              }
            }
          }
          else
          {
            FFileSystem->Open();
          }
        }
        __finally
        {
          if (FSessionData->Tunnel)
          {
            FSessionData->RollbackTunnel();
          }
        }

        if (SessionData->CacheDirectoryChanges)
        {
          DebugAssert(FDirectoryChangesCache == NULL);
          FDirectoryChangesCache = new TRemoteDirectoryChangesCache(
            Configuration->CacheDirectoryChangesMaxSize);
          if (SessionData->PreserveDirectoryChanges)
          {
            Configuration->LoadDirectoryChangesCache(SessionData->SessionKey,
              FDirectoryChangesCache);
          }
        }

        DoStartup();

        if (FCollectFileSystemUsage)
        {
          FFileSystem->CollectUsage();
          FCollectFileSystemUsage = false;
        }

        Information(LoadStr(STATUS_READY));
        FStatus = ssOpened;
      }
      catch(...)
      {
        // rollback
        if (FDirectoryChangesCache != NULL)
        {
          delete FDirectoryChangesCache;
          FDirectoryChangesCache = NULL;
        }
        if (SessionData->FingerprintScan && (FFileSystem != NULL) &&
            DebugAlwaysTrue(SessionData->Ftps != ftpsNone))
        {
          const TSessionInfo & SessionInfo = FFileSystem->GetSessionInfo();
          FFingerprintScannedSHA256 = SessionInfo.CertificateFingerprintSHA256;
          FFingerprintScannedSHA1 = SessionInfo.CertificateFingerprintSHA1;
          FFingerprintScannedMD5 = UnicodeString();
        }
        if (FRememberedPasswordUsed)
        {
          // Particularly to prevent reusing a wrong client certificate passphrase
          // in the next login attempt
          FRememberedPassword = UnicodeString();
          FRememberedPasswordKind = TPromptKind(-1);
          FRememberedTunnelPassword = UnicodeString();
        }
        throw;
      }
    }
    __finally
    {
      // This does not make it through, if terminal thread is abandoned,
      // see also TTerminalManager::DoConnectTerminal
      DoInformation(L"", 0);
    }
  }
  catch(EFatal &)
  {
    throw;
  }
  catch(Exception & E)
  {
    // any exception while opening session is fatal
    FatalError(&E, L"");
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::IsListenerFree(unsigned int PortNumber)
{
  SOCKET Socket = socket(AF_INET, SOCK_STREAM, 0);
  bool Result = (Socket != INVALID_SOCKET);
  if (Result)
  {
    SOCKADDR_IN Address;

    memset(&Address, 0, sizeof(Address));
    Address.sin_family = AF_INET;
    Address.sin_port = htons(static_cast<short>(PortNumber));
    Address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    Result = (bind(Socket, reinterpret_cast<sockaddr *>(&Address), sizeof(Address)) == 0);
    closesocket(Socket);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::OpenTunnel()
{
  DebugAssert(FTunnelData == NULL);

  FTunnelLocalPortNumber = FSessionData->TunnelLocalPortNumber;
  if (FTunnelLocalPortNumber == 0)
  {
    // Randomizing the port selection to reduce a chance of conflicts.
    std::vector<int> Ports;
    for (int Port = Configuration->TunnelLocalPortNumberLow; Port <= Configuration->TunnelLocalPortNumberHigh; Port++)
    {
      Ports.push_back(Port);
    }

    do
    {
      if (Ports.empty())
      {
        FatalError(NULL, FMTLOAD(TUNNEL_NO_FREE_PORT,
          (Configuration->TunnelLocalPortNumberLow, Configuration->TunnelLocalPortNumberHigh)));
      }
      int Index = Random(Ports.size());
      int Port = Ports[Index];
      Ports.erase(Ports.begin() + Index);
      if (IsListenerFree(Port))
      {
        FTunnelLocalPortNumber = Port;
        LogEvent(FORMAT(L"Autoselected tunnel local port number %d", (FTunnelLocalPortNumber)));
      }
    }
    while (FTunnelLocalPortNumber == 0);
  }

  try
  {
    FTunnelData = FSessionData->CreateTunnelData(FTunnelLocalPortNumber);

    // The Started argument is not used with Parent being set
    FTunnelLog = new TSessionLog(this, TDateTime(), FTunnelData, Configuration);
    FTunnelLog->SetParent(FLog, L"Tunnel");
    FTunnelLog->ReflectSettings();
    FTunnelUI = new TTunnelUI(this);
    FTunnel = new TSecureShell(FTunnelUI, FTunnelData, FTunnelLog, Configuration);

    FTunnelOpening = true;
    try
    {
      FTunnel->Open();
    }
    __finally
    {
      FTunnelOpening = false;
    }

    // When forwarding fails due to a local problem (e.g. port in use), we get the error already here, so abort asap.
    // If we won't abort, particularly in case of "port in use", we may not even detect the problem,
    // as the connection to the port may succeed.
    // Remote-side tunnel problems are handled in TTerminal::Open().
    if (!FTunnel->LastTunnelError.IsEmpty())
    {
      // Throw and handle with any other theoretical forwarding errors that may cause tunnel connection close
      // (probably cannot happen)
      EXCEPTION;
    }

    FTunnelThread = new TTunnelThread(FTunnel);
  }
  catch (Exception & E)
  {
    LogEvent(L"Error opening tunnel.");
    CloseTunnel();
    if (!FTunnelError.IsEmpty())
    {
      FatalError(&E, FMTLOAD(TUNNEL_ERROR, (FTunnelError)));
    }
    else
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CloseTunnel()
{
  SAFE_DESTROY_EX(TTunnelThread, FTunnelThread);
  FTunnelError = FTunnel->LastTunnelError;
  SAFE_DESTROY_EX(TSecureShell, FTunnel);
  SAFE_DESTROY_EX(TTunnelUI, FTunnelUI);
  SAFE_DESTROY_EX(TSessionLog, FTunnelLog);
  SAFE_DESTROY(FTunnelData);

  FTunnelLocalPortNumber = 0;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Closed()
{
  if (FTunnel != NULL)
  {
    CloseTunnel();
  }

  if (OnClose)
  {
    TCallbackGuard Guard(this);
    try
    {
      OnClose(this);
      Guard.Verify();
    }
    catch (Exception & E)
    {
      if (!Guard.Verify(&E))
      {
        throw;
      }
    }
  }

  FStatus = ssClosed;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ProcessGUI()
{
  // Do not process GUI here, as we are called directly from a GUI loop and may
  // recurse for good.
  // Alternatively we may check for (FOperationProgress == NULL)
  if (FNesting == 0)
  {
    TAutoNestingCounter NestingCounter(FNesting);
    ::ProcessGUI();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Progress(TFileOperationProgressType * OperationProgress)
{
  if (FNesting == 0)
  {
    TAutoNestingCounter NestingCounter(FNesting);
    OperationProgress->Progress();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Reopen(int Params)
{
  TFSProtocol OrigFSProtocol = SessionData->FSProtocol;
  UnicodeString PrevRemoteDirectory = SessionData->RemoteDirectory;
  bool PrevReadCurrentDirectoryPending = FReadCurrentDirectoryPending;
  bool PrevReadDirectoryPending = FReadDirectoryPending;
  DebugAssert(!FSuspendTransaction);
  bool PrevAutoReadDirectory = FAutoReadDirectory;
  // here used to be a check for FExceptionOnFail being 0
  // but it can happen, e.g. when we are downloading file to execute it.
  // however I'm not sure why we mind having exception-on-fail enabled here
  int PrevExceptionOnFail = FExceptionOnFail;
  try
  {
    FReadCurrentDirectoryPending = false;
    FReadDirectoryPending = false;
    // Not sure why we are suspending the transaction in the first place,
    // but definitely when set while connecting auto loaded workspace session, it causes loading the directory twice.
    // (when reconnecting lost connection, it's usually prevented by cached directory)
    // Preventing that by suspending transaction only when there is one.
    FSuspendTransaction = (FInTransaction > 0);
    FExceptionOnFail = 0;
    // typically, we avoid reading directory, when there is operation ongoing,
    // for file list which may reference files from current directory
    if (FLAGSET(Params, ropNoReadDirectory))
    {
      AutoReadDirectory = false;
    }

    // only peek, we may not be connected at all atm,
    // so make sure we do not try retrieving current directory from the server
    // (particularly with FTP)
    UnicodeString ACurrentDirectory = PeekCurrentDirectory();
    if (!ACurrentDirectory.IsEmpty())
    {
      SessionData->RemoteDirectory = ACurrentDirectory;
    }
    if (SessionData->FSProtocol == fsSFTP)
    {
      SessionData->FSProtocol = (FFSProtocol == cfsSCP ? fsSCPonly : fsSFTPonly);
    }

    Open();
  }
  __finally
  {
    SessionData->RemoteDirectory = PrevRemoteDirectory;
    SessionData->FSProtocol = OrigFSProtocol;
    FAutoReadDirectory = PrevAutoReadDirectory;
    FReadCurrentDirectoryPending = PrevReadCurrentDirectoryPending;
    FReadDirectoryPending = PrevReadDirectoryPending;
    FSuspendTransaction = false;
    FExceptionOnFail = PrevExceptionOnFail;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::PromptUser(TSessionData * Data, TPromptKind Kind,
  UnicodeString Name, UnicodeString Instructions, UnicodeString Prompt, bool Echo, int MaxLen, UnicodeString & Result)
{
  bool AResult;
  TStrings * Prompts = new TStringList;
  TStrings * Results = new TStringList;
  try
  {
    Prompts->AddObject(Prompt, (TObject *)(FLAGMASK(Echo, pupEcho)));
    Results->AddObject(Result, (TObject *)MaxLen);

    AResult = PromptUser(Data, Kind, Name, Instructions, Prompts, Results);

    Result = Results->Strings[0];
  }
  __finally
  {
    delete Prompts;
    delete Results;
  }

  return AResult;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::PromptUser(TSessionData * Data, TPromptKind Kind,
  UnicodeString Name, UnicodeString Instructions, TStrings * Prompts, TStrings * Results)
{
  // If PromptUser is overridden in descendant class, the overridden version
  // is not called when accessed via TSessionIU interface.
  // So this is workaround.
  // Actually no longer needed as we do not override DoPromptUser
  // anymore in TSecondaryTerminal.
  return DoPromptUser(Data, Kind, Name, Instructions, Prompts, Results);
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminal::GetPrimaryTerminal()
{
  return this;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoPromptUser(TSessionData * /*Data*/, TPromptKind Kind,
  UnicodeString Name, UnicodeString Instructions, TStrings * Prompts, TStrings * Results)
{
  bool AResult = false;


  bool PasswordOrPassphrasePrompt = IsPasswordOrPassphrasePrompt(Kind, Prompts);
  if (PasswordOrPassphrasePrompt)
  {
    bool & PasswordTried =
      FTunnelOpening ? FRememberedTunnelPasswordTried : FRememberedPasswordTried;
    if (!PasswordTried)
    {
      // let's expect that the main session is already authenticated and its password
      // is not written after, so no locking is necessary
      // (no longer true, once the main session can be reconnected)
      UnicodeString APassword;
      if (FTunnelOpening)
      {
        APassword = PrimaryTerminal->GetRememberedTunnelPassword();
      }
      else
      {
        APassword = PrimaryTerminal->GetRememberedPassword();
      }
      FRememberedPasswordUsed = true;
      Results->Strings[0] = APassword;
      if (!Results->Strings[0].IsEmpty())
      {
        LogEvent(L"Using remembered password.");
        AResult = true;
      }
      PasswordTried = true;
    }
  }

  if (!AResult)
  {
    if (PasswordOrPassphrasePrompt && !Configuration->RememberPassword)
    {
      Prompts->Objects[0] = (TObject*)(int(Prompts->Objects[0]) | pupRemember);
    }

    if (OnPromptUser != NULL)
    {
      TCallbackGuard Guard(this);
      try
      {
        OnPromptUser(this, Kind, Name, Instructions, Prompts, Results, AResult, NULL);
        Guard.Verify();
      }
      catch (Exception & E)
      {
        if (!Guard.Verify(&E))
        {
          throw;
        }
      }
    }

    if (AResult && PasswordOrPassphrasePrompt &&
        (Configuration->RememberPassword || FLAGSET(int(Prompts->Objects[0]), pupRemember)))
    {
      UnicodeString Password = DenormalizeString(Results->Strings[0]);
      RawByteString EncryptedPassword = EncryptPassword(Password);
      Shred(Password);
      if (FTunnelOpening)
      {
        PrimaryTerminal->FRememberedTunnelPassword = EncryptedPassword;
      }
      else
      {
        PrimaryTerminal->FRememberedPassword = EncryptedPassword;
        PrimaryTerminal->FRememberedPasswordKind = Kind;
      }
    }
  }

  return AResult;
}
//---------------------------------------------------------------------------
unsigned int __fastcall TTerminal::QueryUser(const UnicodeString Query,
  TStrings * MoreMessages, unsigned int Answers, const TQueryParams * Params,
  TQueryType QueryType)
{
  LogEvent(FORMAT(L"Asking user:\n%s (%s)", (Query, (MoreMessages ? MoreMessages->CommaText : UnicodeString() ))));
  unsigned int Answer = AbortAnswer(Answers);
  if (FOnQueryUser)
  {
    TCallbackGuard Guard(this);
    try
    {
      FOnQueryUser(this, Query, MoreMessages, Answers, Params, Answer, QueryType, NULL);
      Guard.Verify();
    }
    catch (Exception & E)
    {
      if (!Guard.Verify(&E))
      {
        throw;
      }
    }
  }
  UnicodeString Name;
  if (Answer == qaNeverAskAgain)
  {
    Name = L"NeverAskAgain";
  }
  else
  {
    UnicodeString Caption;
    AnswerNameAndCaption(Answer, Name, Caption);
  }
  LogEvent(FORMAT(L"Answer: %s", (Name)));
  return Answer;
}
//---------------------------------------------------------------------------
unsigned int __fastcall TTerminal::QueryUserException(const UnicodeString Query,
  Exception * E, unsigned int Answers, const TQueryParams * Params,
  TQueryType QueryType)
{
  unsigned int Result;
  UnicodeString ExMessage;
  if (DebugAlwaysTrue(ExceptionMessageFormatted(E, ExMessage) || !Query.IsEmpty()))
  {
    TStrings * MoreMessages = new TStringList();
    try
    {
      if (!ExMessage.IsEmpty() && !Query.IsEmpty())
      {
        MoreMessages->Add(UnformatMessage(ExMessage));
      }

      ExtException * EE = dynamic_cast<ExtException*>(E);
      if ((EE != NULL) && (EE->MoreMessages != NULL))
      {
        MoreMessages->AddStrings(EE->MoreMessages);
      }

      // We know MoreMessages not to be NULL here,
      // AppendExceptionStackTraceAndForget should never return true
      // (indicating it had to create the string list)
      DebugAlwaysFalse(AppendExceptionStackTraceAndForget(MoreMessages));

      TQueryParams HelpKeywordOverrideParams;
      if (Params != NULL)
      {
        HelpKeywordOverrideParams.Assign(*Params);
      }
      HelpKeywordOverrideParams.HelpKeyword =
        MergeHelpKeyword(HelpKeywordOverrideParams.HelpKeyword, GetExceptionHelpKeyword(E));

      Result = QueryUser(!Query.IsEmpty() ? Query : ExMessage,
        MoreMessages->Count ? MoreMessages : NULL,
        Answers, &HelpKeywordOverrideParams, QueryType);
    }
    __finally
    {
      delete MoreMessages;
    }
  }
  else
  {
    Result = AbortAnswer(Answers);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DisplayBanner(const UnicodeString & Banner)
{
  if (OnDisplayBanner != NULL)
  {
    unsigned int Params = 0; // shut up
    if (Configuration->ShowBanner(SessionData->SessionKey, Banner, Params) ||
        Configuration->ForceBanners)
    {
      bool NeverShowAgain = false;
      int Options =
        FLAGMASK(Configuration->ForceBanners, boDisableNeverShowAgain);
      unsigned int OrigParams = Params;

      TCallbackGuard Guard(this);
      try
      {
        OnDisplayBanner(this, SessionData->SessionName, Banner, NeverShowAgain, Options, Params);
        Guard.Verify();
      }
      catch (Exception & E)
      {
        if (!Guard.Verify(&E))
        {
          throw;
        }
      }

      if (!Configuration->ForceBanners && NeverShowAgain)
      {
        Configuration->NeverShowBanner(SessionData->SessionKey, Banner);
      }
      if (OrigParams != Params)
      {
        Configuration->SetBannerParams(SessionData->SessionKey, Params);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::HandleExtendedException(Exception * E)
{
  Log->AddException(E);
  if (OnShowExtendedException != NULL)
  {
    TCallbackGuard Guard(this);
    try
    {
      // the event handler may destroy 'this' ...
      OnShowExtendedException(this, E, NULL);
      // .. hence guard is dismissed from destructor, to make following call no-op
      Guard.Verify();
    }
    catch (Exception & E)
    {
      if (!Guard.Verify(&E))
      {
        throw;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ShowExtendedException(Exception * E)
{
  Log->AddException(E);
  if (OnShowExtendedException != NULL)
  {
    OnShowExtendedException(this, E, NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoInformation(
  const UnicodeString & Str, int Phase, const UnicodeString & Additional)
{
  if (OnInformation)
  {
    TCallbackGuard Guard(this);
    try
    {
      OnInformation(this, Str, Phase, Additional);
      Guard.Verify();
    }
    catch (Exception & E)
    {
      if (!Guard.Verify(&E))
      {
        throw;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Information(const UnicodeString & Str)
{
  DoInformation(Str);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoProgress(TFileOperationProgressType & ProgressData)
{

  if ((Configuration->ActualLogProtocol >= 1) &&
      ProgressData.IsTransfer())
  {
    DWORD Now = GetTickCount();
    if (Now - FLastProgressLogged >= 1000)
    {
      LogEvent(L"Transfer progress: " + ProgressData.GetLogStr(false));
      FLastProgressLogged = Now;
    }
  }

  if (ProgressData.TransferredSize > 0)
  {
    FFileTransferAny = true;
  }

  if (OnProgress != NULL)
  {
    TCallbackGuard Guard(this);
    try
    {
      OnProgress(ProgressData);
      Guard.Verify();
    }
    catch (Exception & E)
    {
      if (!Guard.Verify(&E))
      {
        throw;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoFinished(TFileOperation Operation, TOperationSide Side, bool Temp,
  const UnicodeString & FileName, bool Success, bool NotCancelled, TOnceDoneOperation & OnceDoneOperation)
{
  if (OnFinished != NULL)
  {
    TCallbackGuard Guard(this);
    try
    {
      OnFinished(Operation, Side, Temp, FileName, Success, NotCancelled, OnceDoneOperation);
      Guard.Verify();
    }
    catch (Exception & E)
    {
      if (!Guard.Verify(&E))
      {
        throw;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SaveCapabilities(TFileSystemInfo & FileSystemInfo)
{
  for (int Index = 0; Index < fcCount; Index++)
  {
    FileSystemInfo.IsCapable[Index] = IsCapable[(TFSCapability)Index];
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetIsCapable(TFSCapability Capability) const
{
  if (DebugAlwaysTrue(FFileSystem != NULL))
  {
    switch (Capability)
    {
      case fcBackgroundTransfers:
        return !IsEncryptingFiles();

      default:
        return FFileSystem->IsCapable(Capability);
    }
  }
  else
  {
    return false;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::AbsolutePath(UnicodeString Path, bool Local)
{
  return FFileSystem->AbsolutePath(Path, Local);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReactOnCommand(int /*TFSCommand*/ Cmd)
{
  bool ChangesDirectory = false;
  bool ModifiesFiles = false;

  switch ((TFSCommand)Cmd) {
    case fsChangeDirectory:
    case fsHomeDirectory:
      ChangesDirectory = true;
      break;

    case fsCopyToRemote:
    case fsDeleteFile:
    case fsRenameFile:
    case fsMoveFile:
    case fsCopyFile:
    case fsCreateDirectory:
    case fsChangeMode:
    case fsChangeGroup:
    case fsChangeOwner:
    case fsChangeProperties:
    case fsLock:
      ModifiesFiles = true;
      break;

    case fsAnyCommand:
      ChangesDirectory = true;
      ModifiesFiles = true;
      break;
  }

  if (ChangesDirectory)
  {
    if (!InTransaction())
    {
      ReadCurrentDirectory();
      if (AutoReadDirectory)
      {
        ReadDirectory(false);
      }
    }
    else
    {
      FReadCurrentDirectoryPending = true;
      if (AutoReadDirectory)
      {
        FReadDirectoryPending = true;
      }
    }
  }
  else if (ModifiesFiles && AutoReadDirectory && Configuration->AutoReadDirectoryAfterOp)
  {
    if (!InTransaction())
    {
      ReadDirectory(true);
    }
    else
    {
      FReadDirectoryPending = true;
    }
  }
}
//---------------------------------------------------------------------------
NORETURN void __fastcall TTerminal::TerminalError(UnicodeString Msg)
{
  TerminalError(NULL, Msg);
}
//---------------------------------------------------------------------------
NORETURN void __fastcall TTerminal::TerminalError(
  Exception * E, UnicodeString Msg, UnicodeString HelpKeyword)
{
  throw ETerminal(E, Msg, HelpKeyword);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoQueryReopen(Exception * E)
{
  EFatal * Fatal = dynamic_cast<EFatal *>(E);
  DebugAssert(Fatal != NULL);
  bool Result;
  if ((Fatal != NULL) && Fatal->ReopenQueried)
  {
    Result = false;
  }
  else
  {
    LogEvent(L"Connection was lost, asking what to do.");

    TQueryParams Params(qpAllowContinueOnError);
    Params.Timeout = Configuration->SessionReopenAuto;
    Params.TimeoutAnswer = qaRetry;
    Params.TimeoutResponse = Params.TimeoutAnswer;
    TQueryButtonAlias Aliases[1];
    Aliases[0].Button = qaRetry;
    Aliases[0].Alias = LoadStr(RECONNECT_BUTTON);
    Params.Aliases = Aliases;
    Params.AliasesCount = LENOF(Aliases);
    Result = (QueryUserException(L"", E, qaRetry | qaAbort, &Params, qtError) == qaRetry);

    if (Fatal != NULL)
    {
      Fatal->ReopenQueried = true;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::ContinueReopen(TDateTime Start)
{
  return
    (Configuration->SessionReopenTimeout == 0) ||
    (int(double(Now() - Start) * MSecsPerDay) < Configuration->SessionReopenTimeout);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::QueryReopen(Exception * E, int Params,
  TFileOperationProgressType * OperationProgress)
{
  TSuspendFileOperationProgress Suspend(OperationProgress);

  bool Result = DoQueryReopen(E);

  if (Result)
  {
    TDateTime Start = Now();
    do
    {
      try
      {
        Reopen(Params);
      }
      catch(Exception & E)
      {
        if (!Active)
        {
          Result =
            ContinueReopen(Start) &&
            DoQueryReopen(&E);
        }
        else
        {
          throw;
        }
      }
    }
    while (!Active && Result);
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::FileOperationLoopQuery(Exception & E,
  TFileOperationProgressType * OperationProgress, const UnicodeString Message,
  unsigned int Flags, UnicodeString SpecialRetry, UnicodeString HelpKeyword)
{
  bool Result = false;
  Log->AddException(&E);
  unsigned int Answer;
  bool AllowSkip = FLAGSET(Flags, folAllowSkip);
  bool SkipToAllPossible = AllowSkip && (OperationProgress != NULL);

  if (SkipToAllPossible && OperationProgress->SkipToAll)
  {
    Answer = qaSkip;
  }
  else
  {
    int Answers = qaRetry | qaAbort |
      FLAGMASK(AllowSkip, qaSkip) |
      FLAGMASK(SkipToAllPossible, qaAll) |
      FLAGMASK(!SpecialRetry.IsEmpty(), qaYes);
    TQueryParams Params(qpAllowContinueOnError | FLAGMASK(!AllowSkip, qpFatalAbort));
    Params.HelpKeyword = HelpKeyword;
    TQueryButtonAlias Aliases[2];
    int AliasCount = 0;

    if (FLAGSET(Answers, qaAll))
    {
      Aliases[AliasCount].Button = qaAll;
      Aliases[AliasCount].Alias = LoadStr(SKIP_ALL_BUTTON);
      AliasCount++;
    }
    if (FLAGSET(Answers, qaYes))
    {
      Aliases[AliasCount].Button = qaYes;
      Aliases[AliasCount].Alias = SpecialRetry;
      AliasCount++;
    }

    if (AliasCount > 0)
    {
      Params.Aliases = Aliases;
      Params.AliasesCount = AliasCount;
    }

    {
      TSuspendFileOperationProgress Suspend(OperationProgress);
      Answer = QueryUserException(Message, &E, Answers, &Params, qtError);
    }

    if (Answer == qaAll)
    {
      DebugAssert(OperationProgress != NULL);
      OperationProgress->SetSkipToAll();
      Answer = qaSkip;
    }
    if (Answer == qaYes)
    {
      Result = true;
      Answer = qaRetry;
    }
  }

  if (Answer != qaRetry)
  {
    if ((Answer == qaAbort) && (OperationProgress != NULL))
    {
      OperationProgress->SetCancel(csCancel);
    }

    if (AllowSkip)
    {
      throw ESkipFile(&E, Message);
    }
    else
    {
      // this can happen only during file transfer with SCP
      throw ExtException(&E, Message);
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FileOperationLoopEnd(Exception & E,
  TFileOperationProgressType * OperationProgress, const UnicodeString & Message,
  unsigned int Flags, const UnicodeString & SpecialRetry, const UnicodeString & HelpKeyword)
{
  if ((dynamic_cast<EAbort *>(&E) != NULL) ||
      (dynamic_cast<ESkipFile *>(&E) != NULL) ||
      (dynamic_cast<ESkipFile *>(&E) != NULL))
  {
    RethrowException(&E);
  }
  else if (dynamic_cast<EFatal *>(&E) != NULL)
  {
    if (FLAGCLEAR(Flags, folRetryOnFatal))
    {
      RethrowException(&E);
    }
    else
    {
      DebugAssert(SpecialRetry.IsEmpty());
      std::unique_ptr<Exception> E2(new EFatal(&E, Message, HelpKeyword));
      if (!DoQueryReopen(E2.get()))
      {
        RethrowException(E2.get());
      }
    }
  }
  else
  {
    FileOperationLoopQuery(E, OperationProgress, Message, Flags, SpecialRetry, HelpKeyword);
  }
}
//---------------------------------------------------------------------------
int __fastcall TTerminal::FileOperationLoop(TFileOperationEvent CallBackFunc,
  TFileOperationProgressType * OperationProgress, unsigned int Flags,
  const UnicodeString Message, void * Param1, void * Param2)
{
  DebugAssert(CallBackFunc);
  int Result;
  FILE_OPERATION_LOOP_BEGIN
  {
    Result = CallBackFunc(Param1, Param2);
  }
  FILE_OPERATION_LOOP_END_EX(Message, Flags);

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ClearCaches()
{
  FDirectoryCache->Clear();
  if (FDirectoryChangesCache != NULL)
  {
    FDirectoryChangesCache->Clear();
  }
  if (FCommandSession != NULL)
  {
    FCommandSession->ClearCaches();
  }
  FFileSystem->ClearCaches();
  FEncryptedFileNames.clear();
  FFoldersScannedForEncryptedFiles.clear();
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ClearCachedFileList(const UnicodeString Path,
  bool SubDirs)
{
  FDirectoryCache->ClearFileList(Path, SubDirs);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::AddCachedFileList(TRemoteFileList * FileList)
{
  FDirectoryCache->AddFileList(FileList);
}
//---------------------------------------------------------------------------
TRemoteFileList * __fastcall TTerminal::DirectoryFileList(const UnicodeString Path, TDateTime Timestamp, bool CanLoad)
{
  TRemoteFileList * Result = NULL;
  if (UnixSamePath(FFiles->Directory, Path))
  {
    if (Timestamp < FFiles->Timestamp)
    {
      Result = new TRemoteFileList();
      FFiles->DuplicateTo(Result);
    }
  }
  else
  {
    if (FDirectoryCache->HasNewerFileList(Path, Timestamp))
    {
      Result = new TRemoteFileList();
      DebugAlwaysTrue(FDirectoryCache->GetFileList(Path, Result));
    }
    // do not attempt to load file list if there is cached version,
    // only absence of cached version indicates that we consider
    // the directory content obsolete
    else if (CanLoad && !FDirectoryCache->HasFileList(Path))
    {
      Result = new TRemoteFileList();
      Result->Directory = Path;

      try
      {
        ReadDirectory(Result);
      }
      catch(...)
      {
        SAFE_DESTROY(Result);
        throw;
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SetCurrentDirectory(UnicodeString value)
{
  DebugAssert(FFileSystem);
  if (value != FFileSystem->CurrentDirectory)
  {
    ChangeDirectory(value);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetCurrentDirectory()
{
  if (FFileSystem != NULL)
  {
    // there's occasional crash when assigning FFileSystem->CurrentDirectory
    // to FCurrentDirectory, splitting the assignment to two statements
    // to locate the crash more closely
    UnicodeString CurrentDirectory = FFileSystem->CurrentDirectory;
    FCurrentDirectory = CurrentDirectory;
    if (FCurrentDirectory.IsEmpty())
    {
      ReadCurrentDirectory();
    }
  }

  return FCurrentDirectory;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::PeekCurrentDirectory()
{
  if (FFileSystem)
  {
    FCurrentDirectory = FFileSystem->CurrentDirectory;
  }

  return FCurrentDirectory;
}
//---------------------------------------------------------------------------
const TRemoteTokenList * __fastcall TTerminal::GetGroups()
{
  DebugAssert(FFileSystem);
  LookupUsersGroups();
  return &FGroups;
}
//---------------------------------------------------------------------------
const TRemoteTokenList * __fastcall TTerminal::GetUsers()
{
  DebugAssert(FFileSystem);
  LookupUsersGroups();
  return &FUsers;
}
//---------------------------------------------------------------------------
const TRemoteTokenList * __fastcall TTerminal::GetMembership()
{
  DebugAssert(FFileSystem);
  LookupUsersGroups();
  return &FMembership;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetUserName() const
{
  // in future might also be implemented to detect username similar to GetUserGroups
  DebugAssert(FFileSystem != NULL);
  UnicodeString Result = FFileSystem->GetUserName();
  // Is empty also when stored username was used
  if (Result.IsEmpty())
  {
    Result = SessionData->UserNameExpanded;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetAreCachesEmpty() const
{
  return FDirectoryCache->IsEmpty &&
    ((FDirectoryChangesCache == NULL) || FDirectoryChangesCache->IsEmpty);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoInitializeLog()
{
  if (FOnInitializeLog)
  {
    TCallbackGuard Guard(this);
    try
    {
      FOnInitializeLog(this);
      Guard.Verify();
    }
    catch (Exception & E)
    {
      if (!Guard.Verify(&E))
      {
        throw;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoChangeDirectory()
{
  if (FOnChangeDirectory)
  {
    TCallbackGuard Guard(this);
    try
    {
      FOnChangeDirectory(this);
      Guard.Verify();
    }
    catch (Exception & E)
    {
      if (!Guard.Verify(&E))
      {
        throw;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoReadDirectory(bool ReloadOnly)
{
  if (FOnReadDirectory)
  {
    TCallbackGuard Guard(this);
    try
    {
      FOnReadDirectory(this, ReloadOnly);
      Guard.Verify();
    }
    catch (Exception & E)
    {
      if (!Guard.Verify(&E))
      {
        throw;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoStartReadDirectory()
{
  if (FOnStartReadDirectory)
  {
    TCallbackGuard Guard(this);
    try
    {
      FOnStartReadDirectory(this);
      Guard.Verify();
    }
    catch (Exception & E)
    {
      if (!Guard.Verify(&E))
      {
        throw;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoReadDirectoryProgress(int Progress, int ResolvedLinks, bool & Cancel)
{
  if (FReadingCurrentDirectory && (FOnReadDirectoryProgress != NULL))
  {
    TCallbackGuard Guard(this);
    try
    {
      FOnReadDirectoryProgress(this, Progress, ResolvedLinks, Cancel);
      Guard.Verify();
    }
    catch (Exception & E)
    {
      if (!Guard.Verify(&E))
      {
        throw;
      }
    }
  }
  if (FOnFindingFile != NULL)
  {
    TCallbackGuard Guard(this);
    try
    {
      FOnFindingFile(this, L"", Cancel);
      Guard.Verify();
    }
    catch (Exception & E)
    {
      if (!Guard.Verify(&E))
      {
        throw;
      }
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::InTransaction()
{
  return (FInTransaction > 0) && !FSuspendTransaction;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::BeginTransaction()
{
  if (FInTransaction == 0)
  {
    FReadCurrentDirectoryPending = false;
    FReadDirectoryPending = false;
  }
  FInTransaction++;

  if (FCommandSession != NULL)
  {
    FCommandSession->BeginTransaction();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::EndTransaction()
{
  DoEndTransaction(false);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoEndTransaction(bool Inform)
{
  if (FInTransaction == 0)
    TerminalError(L"Can't end transaction, not in transaction");
  DebugAssert(FInTransaction > 0);
  FInTransaction--;

  // it connection was closed due to fatal error during transaction, do nothing
  if (Active)
  {
    if (FInTransaction == 0)
    {
      try
      {
        if (FReadCurrentDirectoryPending)
        {
          ReadCurrentDirectory();
        }

        if (FReadDirectoryPending)
        {
          if (Inform)
          {
            DoInformation(LoadStr(STATUS_OPEN_DIRECTORY), -1, CurrentDirectory);
          }
          ReadDirectory(!FReadCurrentDirectoryPending);
        }
      }
      __finally
      {
        FReadCurrentDirectoryPending = false;
        FReadDirectoryPending = false;
      }
    }
  }

  if (FCommandSession != NULL)
  {
    FCommandSession->EndTransaction();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SetExceptionOnFail(bool value)
{
  if (value)
  {
    FExceptionOnFail++;
  }
  else
  {
    if (FExceptionOnFail == 0)
      throw Exception(L"ExceptionOnFail is already zero.");
    FExceptionOnFail--;
  }

  if (FCommandSession != NULL)
  {
    FCommandSession->FExceptionOnFail = FExceptionOnFail;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetExceptionOnFail() const
{
  return (bool)(FExceptionOnFail > 0);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FatalAbort()
{
  FatalError(NULL, "");
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FatalError(Exception * E, UnicodeString Msg, UnicodeString HelpKeyword)
{
  bool SecureShellActive = (FSecureShell != NULL) && FSecureShell->Active;
  if (Active || SecureShellActive)
  {
    // We log this instead of exception handler, because Close() would
    // probably cause exception handler to loose pointer to TShellLog()
    LogEvent(L"Attempt to close connection due to fatal exception:");
    Log->Add(llException, Msg);
    Log->AddException(E);

    if (Active)
    {
      Close();
    }

    // this may happen if failure of authentication of SSH, owned by terminal yet
    // (because the protocol was not decided yet), is detected by us (not by putty).
    // e.g. not verified host key
    if (SecureShellActive)
    {
      FSecureShell->Close();
    }
  }

  if (FCallbackGuard != NULL)
  {
    FCallbackGuard->FatalError(E, Msg, HelpKeyword);
  }
  else
  {
    throw ESshFatal(E, Msg, HelpKeyword);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CommandError(Exception * E, const UnicodeString Msg)
{
  CommandError(E, Msg, 0);
}
//---------------------------------------------------------------------------
unsigned int __fastcall TTerminal::CommandError(Exception * E, const UnicodeString Msg,
  unsigned int Answers, const UnicodeString HelpKeyword)
{
  // may not be, particularly when TTerminal::Reopen is being called
  // from within OnShowExtendedException handler
  DebugAssert(FCallbackGuard == NULL);
  unsigned int Result = 0;
  if (E && E->InheritsFrom(__classid(EFatal)))
  {
    FatalError(E, Msg, HelpKeyword);
  }
  else if (E && E->InheritsFrom(__classid(EAbort)))
  {
    // resent EAbort exception
    Abort();
  }
  else if (ExceptionOnFail)
  {
    throw ECommand(E, Msg, HelpKeyword);
  }
  else if (!Answers)
  {
    ECommand * ECmd = new ECommand(E, Msg, HelpKeyword);
    try
    {
      HandleExtendedException(ECmd);
    }
    __finally
    {
      delete ECmd;
    }
  }
  else
  {
    // small hack to enable "skip to all" for TRetryOperationLoop
    bool CanSkip = FLAGSET(Answers, qaSkip) && (OperationProgress != NULL);
    if (CanSkip && OperationProgress->SkipToAll)
    {
      Result = qaSkip;
    }
    else
    {
      TQueryParams Params(qpAllowContinueOnError, HelpKeyword);
      TQueryButtonAlias Aliases[1];
      if (CanSkip)
      {
        Aliases[0].Button = qaAll;
        Aliases[0].Alias = LoadStr(SKIP_ALL_BUTTON);
        Params.Aliases = Aliases;
        Params.AliasesCount = LENOF(Aliases);
        Answers |= qaAll;
      }
      Result = QueryUserException(Msg, E, Answers, &Params, qtError);
      if (Result == qaAll)
      {
        DebugAssert(OperationProgress != NULL);
        OperationProgress->SetSkipToAll();
        Result = qaSkip;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::HandleException(Exception * E)
{
  if (ExceptionOnFail)
  {
    return false;
  }
  else
  {
    Log->AddException(E);
    return true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CloseOnCompletion(
  TOnceDoneOperation Operation, const UnicodeString & Message,
  const UnicodeString & TargetLocalPath, const UnicodeString & DestLocalFileName)
{
  if (FOperationProgressPersistence != NULL)
  {
    DebugAssert(Message.IsEmpty());
    FOperationProgressOnceDoneOperation = Operation;
  }
  else
  {
    Configuration->Usage->Inc(L"ClosesOnCompletion");
    LogEvent(L"Closing session after completed operation (as requested by user)");
    Close();
    throw ESshTerminate(NULL,
      MainInstructions(Message.IsEmpty() ? LoadStr(CLOSED_ON_COMPLETION) : Message),
      Operation, TargetLocalPath, DestLocalFileName);
  }
}
//---------------------------------------------------------------------------
TBatchOverwrite __fastcall TTerminal::EffectiveBatchOverwrite(
  const UnicodeString & SourceFullFileName, const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress, bool Special)
{
  TBatchOverwrite Result;
  if (Special &&
      (FLAGSET(Params, cpResume) || CopyParam->ResumeTransfer(SourceFullFileName)) &&
      (CopyParam->PartOffset < 0))
  {
    Result = boResume;
  }
  else if (FLAGSET(Params, cpAppend))
  {
    Result = boAppend;
  }
  else if (CopyParam->NewerOnly &&
           (((OperationProgress->Side == osLocal) && IsCapable[fcNewerOnlyUpload]) ||
            (OperationProgress->Side != osLocal)) &&
           DebugAlwaysTrue(CopyParam->PartOffset < 0))
  {
    // no way to change batch overwrite mode when CopyParam->NewerOnly is on
    Result = boOlder;
  }
  else if (FLAGSET(Params, cpNoConfirmation) || !Configuration->ConfirmOverwriting)
  {
    // no way to change batch overwrite mode when overwrite confirmations are off
    DebugAssert(OperationProgress->BatchOverwrite == boNo);
    Result = boAll;
  }
  else
  {
    Result = OperationProgress->BatchOverwrite;
    if (!Special &&
        ((Result == boOlder) || (Result == boAlternateResume) || (Result == boResume)))
    {
      Result = boNo;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CheckRemoteFile(
  const UnicodeString & FileName, const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress)
{
  bool Result;
  OperationProgress->LockUserSelections();
  try
  {
    Result = (EffectiveBatchOverwrite(FileName, CopyParam, Params, OperationProgress, true) != boAll);
  }
  __finally
  {
    OperationProgress->UnlockUserSelections();
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned int __fastcall TTerminal::ConfirmFileOverwrite(
  const UnicodeString & SourceFullFileName, const UnicodeString & TargetFileName,
  const TOverwriteFileParams * FileParams, unsigned int Answers, TQueryParams * QueryParams,
  TOperationSide Side, const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress,
  UnicodeString Message)
{
  unsigned int Result = qaCancel; // shut up
  TBatchOverwrite BatchOverwrite;
  bool CanAlternateResume =
    (FileParams != NULL) &&
    (FileParams->DestSize < FileParams->SourceSize) &&
    !OperationProgress->AsciiTransfer;

  OperationProgress->LockUserSelections();
  try
  {
    // duplicated in TSFTPFileSystem::SFTPConfirmOverwrite
    BatchOverwrite = EffectiveBatchOverwrite(SourceFullFileName, CopyParam, Params, OperationProgress, true);
    bool Applicable = true;
    switch (BatchOverwrite)
    {
      case boOlder:
        Applicable = (FileParams != NULL);
        break;

      case boAlternateResume:
        Applicable = CanAlternateResume;
        break;

      case boResume:
        Applicable = CanAlternateResume;
        break;
    }

    if (!Applicable)
    {
      TBatchOverwrite ABatchOverwrite = EffectiveBatchOverwrite(SourceFullFileName, CopyParam, Params, OperationProgress, false);
      DebugAssert(BatchOverwrite != ABatchOverwrite);
      BatchOverwrite = ABatchOverwrite;
    }

    if (BatchOverwrite != boNo)
    {
      LogEvent(1, FORMAT(L"Batch operation mode [%d] is effective", (int(BatchOverwrite))));
    }
    else
    {
      // particularly with parallel transfers, the overall operation can be already cancelled by other parallel operation
      if (OperationProgress->Cancel > csContinue)
      {
        LogEvent(1, L"Transfer cancelled in parallel operation");
        Result = qaCancel;
      }
      else
      {
        if (Message.IsEmpty())
        {
          // Side refers to destination side here
          Message = FMTLOAD((Side == osLocal ? LOCAL_FILE_OVERWRITE2 :
            REMOTE_FILE_OVERWRITE2), (TargetFileName, TargetFileName));
        }
        if (FileParams != NULL)
        {
          Message = FMTLOAD(FILE_OVERWRITE_DETAILS, (Message,
            FormatSize(FileParams->SourceSize),
            DefaultStr(UserModificationStr(FileParams->SourceTimestamp, FileParams->SourcePrecision), LoadStr(TIME_UNKNOWN)),
            FormatSize(FileParams->DestSize),
            DefaultStr(UserModificationStr(FileParams->DestTimestamp, FileParams->DestPrecision), LoadStr(TIME_UNKNOWN))));
        }
        if (DebugAlwaysTrue(QueryParams->HelpKeyword.IsEmpty()))
        {
          QueryParams->HelpKeyword = HELP_OVERWRITE;
        }
        Result = QueryUser(Message, NULL, Answers, QueryParams);
        switch (Result)
        {
          case qaNeverAskAgain:
            Configuration->ConfirmOverwriting = false;
            Result = qaYes;
            break;

          case qaYesToAll:
            BatchOverwrite = boAll;
            break;

          case qaAll:
            BatchOverwrite = boOlder;
            break;

          case qaNoToAll:
            BatchOverwrite = boNone;
            break;
        }

        // if user has not selected another batch overwrite mode,
        // keep the current one. note that we may get here even
        // when batch overwrite was selected already, but it could not be applied
        // to current transfer (see condition above)
        if (BatchOverwrite != boNo)
        {
          OperationProgress->SetBatchOverwrite(BatchOverwrite);
        }
      }
    }
  }
  __finally
  {
    OperationProgress->UnlockUserSelections();
  }

  if (BatchOverwrite != boNo)
  {
    switch (BatchOverwrite)
    {
      case boAll:
        LogEvent(1, L"Overwriting all files");
        Result = qaYes;
        break;

      case boNone:
        LogEvent(1, L"Not overwriting any file");
        Result = qaNo;
        break;

      case boOlder:
        if (FileParams == NULL)
        {
          LogEvent(1, L"Not overwriting due to lack of file information");
          Result = qaNo;
        }
        else
        {
          TModificationFmt Precision = LessDateTimePrecision(FileParams->SourcePrecision, FileParams->DestPrecision);
          TDateTime ReducedSourceTimestamp =
            ReduceDateTimePrecision(FileParams->SourceTimestamp, Precision);
          TDateTime ReducedDestTimestamp =
            ReduceDateTimePrecision(FileParams->DestTimestamp, Precision);

          Result =
            (CompareFileTime(ReducedSourceTimestamp, ReducedDestTimestamp) > 0) ?
            qaYes : qaNo;

          LogEvent(FORMAT(L"Source file timestamp is [%s], destination timestamp is [%s], will%s overwrite",
            (StandardTimestamp(ReducedSourceTimestamp),
             StandardTimestamp(ReducedDestTimestamp),
             ((Result == qaYes) ? L"" : L" not"))));
        }
        break;

      case boAlternateResume:
        DebugAssert(CanAlternateResume);
        LogEvent(1, L"Alternate resume mode");
        Result = qaSkip; // ugh
        break;

      case boAppend:
        LogEvent(1, L"Appending file");
        Result = qaRetry;
        break;

      case boResume:
        LogEvent(1, L"Resuming file transfer");
        Result = qaRetry;
        break;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FileModified(const TRemoteFile * File,
  const UnicodeString FileName, bool ClearDirectoryChange)
{
  UnicodeString ParentDirectory;
  UnicodeString Directory;

  if (SessionData->CacheDirectories || SessionData->CacheDirectoryChanges)
  {
    if ((File != NULL) && (File->Directory != NULL))
    {
      if (File->IsDirectory)
      {
        Directory = File->Directory->FullDirectory + File->FileName;
      }
      ParentDirectory = File->Directory->Directory;
    }
    else if (!FileName.IsEmpty())
    {
      ParentDirectory = UnixExtractFilePath(FileName);
      if (ParentDirectory.IsEmpty())
      {
        ParentDirectory = CurrentDirectory;
      }

      // this case for scripting
      if ((File != NULL) && File->IsDirectory)
      {
        Directory = UnixIncludeTrailingBackslash(ParentDirectory) +
          UnixExtractFileName(File->FileName);
      }
    }
  }

  if (SessionData->CacheDirectories)
  {
    if (!Directory.IsEmpty())
    {
      DirectoryModified(Directory, true);
    }
    if (!ParentDirectory.IsEmpty())
    {
      DirectoryModified(ParentDirectory, false);
    }
  }

  if (SessionData->CacheDirectoryChanges && ClearDirectoryChange)
  {
    if (!Directory.IsEmpty())
    {
      FDirectoryChangesCache->ClearDirectoryChange(Directory);
      FDirectoryChangesCache->ClearDirectoryChangeTarget(Directory);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DirectoryModified(const UnicodeString Path, bool SubDirs)
{
  if (Path.IsEmpty())
  {
    ClearCachedFileList(CurrentDirectory, SubDirs);
  }
  else
  {
    ClearCachedFileList(Path, SubDirs);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DirectoryLoaded(TRemoteFileList * FileList)
{
  AddCachedFileList(FileList);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReloadDirectory()
{
  if (SessionData->CacheDirectories)
  {
    DirectoryModified(CurrentDirectory, false);
  }
  if (SessionData->CacheDirectoryChanges)
  {
    DebugAssert(FDirectoryChangesCache != NULL);
    FDirectoryChangesCache->ClearDirectoryChange(CurrentDirectory);
  }

  ReadCurrentDirectory();
  FReadCurrentDirectoryPending = false;
  ReadDirectory(true);
  FReadDirectoryPending = false;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::RefreshDirectory()
{
  if (!SessionData->CacheDirectories)
  {
    LogEvent(L"Not refreshing directory, caching is off.");
  }
  else if (FDirectoryCache->HasNewerFileList(CurrentDirectory, FFiles->Timestamp))
  {
    // Second parameter was added to allow (rather force) using the cache.
    // Before, the directory was reloaded always, it seems useless,
    // has it any reason?
    ReadDirectory(true, true);
    FReadDirectoryPending = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::EnsureNonExistence(const UnicodeString FileName)
{
  // if filename doesn't contain path, we check for existence of file
  if ((UnixExtractFileDir(FileName).IsEmpty()) &&
      UnixSamePath(CurrentDirectory, FFiles->Directory))
  {
    TRemoteFile *File = FFiles->FindFile(FileName);
    if (File)
    {
      if (File->IsDirectory) throw ECommand(NULL, FMTLOAD(RENAME_CREATE_DIR_EXISTS, (FileName)));
        else throw ECommand(NULL, FMTLOAD(RENAME_CREATE_FILE_EXISTS, (FileName)));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall inline TTerminal::LogEvent(const UnicodeString & Str)
{
  if (Log->Logging)
  {
    Log->Add(llMessage, Str);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LogEvent(int Level, const UnicodeString & Str)
{
  if (Log->Logging && (Configuration->ActualLogProtocol >= Level))
  {
    Log->Add(llMessage, Str);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::RollbackAction(TSessionAction & Action,
  TFileOperationProgressType * OperationProgress, Exception * E)
{
  // ESkipFile without "cancel" is file skip,
  // and we do not want to record skipped actions.
  // But ESkipFile with "cancel" is abort and we want to record that.
  // Note that TScpFileSystem modifies the logic of RollbackAction little bit.
  if ((dynamic_cast<ESkipFile *>(E) != NULL) &&
      ((OperationProgress == NULL) ||
       (OperationProgress->Cancel == csContinue)))
  {
    Action.Cancel();
  }
  else
  {
    Action.Rollback(E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoStartup()
{
  LogEvent(L"Doing startup conversation with host.");
  BeginTransaction();
  try
  {
    Information(LoadStr(STATUS_STARTUP));

    // Make sure that directory would be loaded at last
    FReadCurrentDirectoryPending = true;
    FReadDirectoryPending = AutoReadDirectory;

    FFileSystem->DoStartup();

    LookupUsersGroups();

    if (!SessionData->RemoteDirectory.IsEmpty())
    {
      if (SessionData->UpdateDirectories)
      {
        ExceptionOnFail = true;
        try
        {
          ChangeDirectory(SessionData->RemoteDirectory);
        }
        catch (...)
        {
          if (!Active || SessionData->RequireDirectories)
          {
            ExceptionOnFail = false;
            throw;
          }
          else
          {
            LogEvent(L"Configured initial remote directory cannot be opened, staying in the home directory.");
          }
        }
        ExceptionOnFail = false;
      }
      else
      {
        ChangeDirectory(SessionData->RemoteDirectory);
      }
    }
  }
  __finally
  {
    DoEndTransaction(true);
  }
  LogEvent(L"Startup conversation with host finished.");
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReadCurrentDirectory()
{
  DebugAssert(FFileSystem);
  try
  {
    // reset flag in case we are called externally (like from console dialog)
    FReadCurrentDirectoryPending = false;

    LogEvent(L"Getting current directory name.");
    UnicodeString OldDirectory = FFileSystem->CurrentDirectory;

    FFileSystem->ReadCurrentDirectory();
    ReactOnCommand(fsCurrentDirectory);

    if (SessionData->CacheDirectoryChanges)
    {
      DebugAssert(FDirectoryChangesCache != NULL);
      if (!CurrentDirectory.IsEmpty() && !FLastDirectoryChange.IsEmpty() && (CurrentDirectory != OldDirectory))
      {
        FDirectoryChangesCache->AddDirectoryChange(OldDirectory,
          FLastDirectoryChange, CurrentDirectory);
      }
      // not to breake the cache, if the next directory change would not
      // be initialized by ChangeDirectory(), which sets it
      // (HomeDirectory() particularly)
      FLastDirectoryChange = L"";
    }

    if (OldDirectory != FFileSystem->CurrentDirectory) DoChangeDirectory();
  }
  catch (Exception &E)
  {
    CommandError(&E, LoadStr(READ_CURRENT_DIR_ERROR));
  }
}
//---------------------------------------------------------------------------
void TTerminal::DoReadDirectoryFinish(TRemoteDirectory * Files, bool ReloadOnly)
{
  // Factored out to solve Clang ICE
  TRemoteDirectory * OldFiles = FFiles;
  FFiles = Files;
  try
  {
    DoReadDirectory(ReloadOnly);
  }
  __finally
  {
    // delete only after loading new files to dir view,
    // not to destroy the file objects that the view holds
    // (can be issue in multi threaded environment, such as when the
    // terminal is reconnecting in the terminal thread)
    delete OldFiles;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReadDirectory(bool ReloadOnly, bool ForceCache)
{
  bool LoadedFromCache = false;

  if (SessionData->CacheDirectories && FDirectoryCache->HasFileList(CurrentDirectory))
  {
    if (ReloadOnly && !ForceCache)
    {
      LogEvent(L"Cached directory not reloaded.");
    }
    else
    {
      DoStartReadDirectory();
      try
      {
        LoadedFromCache = FDirectoryCache->GetFileList(CurrentDirectory, FFiles);
      }
      __finally
      {
        DoReadDirectory(ReloadOnly);
      }

      if (LoadedFromCache)
      {
        LogEvent(L"Directory content loaded from cache.");
      }
      else
      {
        LogEvent(L"Cached Directory content has been removed.");
      }
    }
  }

  if (!LoadedFromCache)
  {
    DoStartReadDirectory();
    FReadingCurrentDirectory = true;
    bool Cancel = false; // dummy
    DoReadDirectoryProgress(0, 0, Cancel);

    try
    {
      TRemoteDirectory * Files = new TRemoteDirectory(this, FFiles);
      try
      {
        Files->Directory = CurrentDirectory;
        CustomReadDirectory(Files);
      }
      __finally
      {
        DoReadDirectoryProgress(-1, 0, Cancel);
        FReadingCurrentDirectory = false;
        DoReadDirectoryFinish(Files, ReloadOnly);
        if (Active)
        {
          if (SessionData->CacheDirectories)
          {
            DirectoryLoaded(FFiles);
          }
        }
      }
    }
    catch (Exception &E)
    {
      CommandError(&E, FmtLoadStr(LIST_DIR_ERROR, ARRAYOFCONST((FFiles->Directory))));
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetRemoteFileInfo(TRemoteFile * File)
{
  return
    FORMAT(L"%s;%s;%d;%s;%d;%s;%s;%s;%d",
      (File->FileName, File->Type, File->Size, StandardTimestamp(File->Modification), int(File->ModificationFmt),
       File->Owner.LogText, File->Group.LogText, File->Rights->Text,
       File->Attr));
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LogRemoteFile(TRemoteFile * File)
{
  // optimization
  if (Log->Logging)
  {
    LogEvent(GetRemoteFileInfo(File));
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::FormatFileDetailsForLog(
  const UnicodeString & FileName, TDateTime Modification, __int64 Size, const TRemoteFile * LinkedFile)
{
  UnicodeString Result;
  // optimization
  if (Log->Logging)
  {
    Result = FORMAT(L"'%s' [%s] [%s]", (FileName, (Modification != TDateTime() ? StandardTimestamp(Modification) : UnicodeString(L"n/a")), IntToStr(Size)));
    if (LinkedFile != NULL)
    {
      LinkedFile = LinkedFile->Resolve(); // in case it's symlink to symlink
      Result += FORMAT(L" - Link to: %s", (FormatFileDetailsForLog(LinkedFile->FileName, LinkedFile->Modification, LinkedFile->Size, NULL)));
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LogFileDetails(const UnicodeString & FileName, TDateTime Modification, __int64 Size, const TRemoteFile * LinkedFile)
{
  // optimization
  if (Log->Logging)
  {
    LogEvent(FORMAT("File: %s", (FormatFileDetailsForLog(FileName, Modification, Size, LinkedFile))));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LogFileDone(
  TFileOperationProgressType * OperationProgress, const UnicodeString & DestFileName,
  TTransferSessionAction & Action)
{
  if (FDestFileName.IsEmpty())
  {
    FDestFileName = DestFileName;
  }
  else
  {
    FMultipleDestinationFiles = true;
  }

  __int64 Size = OperationProgress->TransferredSize;
  // optimization
  if (Log->Logging)
  {
    LogEvent(FORMAT("Transfer done: '%s' => '%s' [%s]", (OperationProgress->FullFileName, DestFileName, IntToStr(Size))));
  }

  Action.Size(Size);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CustomReadDirectory(TRemoteFileList * FileList)
{
  DebugAssert(FileList);
  DebugAssert(FFileSystem);

  AppLogFmt(L"Reading directory %s", (FileList->Directory));

  // To match FTP upload/download, we also limit directory listing.
  // For simplicity, we limit it unconditionally, for all protocols for any kind of errors.
  bool FileTransferAny = false;
  TRobustOperationLoop RobustLoop(this, OperationProgress, &FileTransferAny);

  do
  {
    try
    {
      // Record even an attempt, to avoid listing a directory over and over again, when it is not readable actually
      if (IsEncryptingFiles())
      {
        FFoldersScannedForEncryptedFiles.insert(FileList->Directory);
      }

      FFileSystem->ReadDirectory(FileList);
    }
    catch (Exception & E)
    {
      // Do not retry for initial listing of directory,
      // we instead retry whole connection attempt,
      // what would be done anyway (but Open is not ready for recursion).
      if ((FOpening > 0) ||
          !RobustLoop.TryReopen(E))
      {
        throw;
      }
    }
   }
  while (RobustLoop.Retry());


  if (Log->Logging && (Configuration->ActualLogProtocol >= 0))
  {
    for (int Index = 0; Index < FileList->Count; Index++)
    {
      LogRemoteFile(FileList->Files[Index]);
    }
  }

  ReactOnCommand(fsListDirectory);
}
//---------------------------------------------------------------------------
TRemoteFileList * __fastcall TTerminal::ReadDirectoryListing(UnicodeString Directory, const TFileMasks & Mask)
{
  TRemoteFileList * FileList;
  TRetryOperationLoop RetryLoop(this);
  do
  {
    FileList = NULL;
    TLsSessionAction Action(ActionLog, AbsolutePath(Directory, true));

    try
    {
      FileList = DoReadDirectoryListing(Directory, false);
      if (FileList != NULL)
      {
        int Index = 0;
        while (Index < FileList->Count)
        {
          TRemoteFile * File = FileList->Files[Index];
          TFileMasks::TParams Params;
          Params.Size = File->Resolve()->Size;
          Params.Modification = File->Modification;
          if (!Mask.MatchesFileName(File->FileName, false, &Params))
          {
            FileList->Delete(Index);
          }
          else
          {
            Index++;
          }
        }

        Action.FileList(FileList);
      }
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, Action);
    }
  }
  while (RetryLoop.Retry());
  return FileList;
}
//---------------------------------------------------------------------------
TRemoteFile * __fastcall TTerminal::ReadFileListing(UnicodeString Path)
{
  TRemoteFile * File;
  TRetryOperationLoop RetryLoop(this);
  do
  {
    File = NULL;
    TStatSessionAction Action(ActionLog, AbsolutePath(Path, true));
    try
    {
      // reset caches
      AnnounceFileListOperation();
      File = ReadFile(Path);
      Action.File(File);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, Action);
    }
  }
  while (RetryLoop.Retry());
  return File;
}
//---------------------------------------------------------------------------
TRemoteFileList * __fastcall TTerminal::CustomReadDirectoryListing(UnicodeString Directory, bool UseCache)
{
  TRemoteFileList * FileList = NULL;
  TRetryOperationLoop RetryLoop(this);
  do
  {
    try
    {
      FileList = DoReadDirectoryListing(Directory, UseCache);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E);
    }
  }
  while (RetryLoop.Retry());
  return FileList;
}
//---------------------------------------------------------------------------
TRemoteFileList * __fastcall TTerminal::DoReadDirectoryListing(UnicodeString Directory, bool UseCache)
{
  TRemoteFileList * FileList = new TRemoteFileList();
  try
  {
    bool Cache = UseCache && SessionData->CacheDirectories;
    bool LoadedFromCache = Cache && FDirectoryCache->HasFileList(Directory);
    if (LoadedFromCache)
    {
      LoadedFromCache = FDirectoryCache->GetFileList(Directory, FileList);
    }

    if (!LoadedFromCache)
    {
      FileList->Directory = Directory;

      ExceptionOnFail = true;
      try
      {
        ReadDirectory(FileList);
      }
      __finally
      {
        ExceptionOnFail = false;
      }

      if (Cache)
      {
        AddCachedFileList(FileList);
      }
    }
  }
  catch(...)
  {
    delete FileList;
    throw;
  }
  return FileList;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DeleteContentsIfDirectory(
  const UnicodeString & FileName, const TRemoteFile * File, int Params, TRmSessionAction & Action)
{
  bool Dir = (File != NULL) && File->IsDirectory && CanRecurseToDirectory(File);

  if (Dir && FLAGCLEAR(Params, dfNoRecursive))
  {
    try
    {
      ProcessDirectory(FileName, DeleteFile, &Params);
    }
    catch(...)
    {
      Action.Cancel();
      throw;
    }
  }
  return Dir && !File->IsSymLink;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ProcessDirectory(const UnicodeString DirName,
  TProcessFileEvent CallBackFunc, void * Param, bool UseCache, bool IgnoreErrors)
{
  TRemoteFileList * FileList = NULL;
  if (IgnoreErrors)
  {
    ExceptionOnFail = true;
    try
    {
      try
      {
        FileList = CustomReadDirectoryListing(DirName, UseCache);
      }
      catch(...)
      {
        if (!Active)
        {
          throw;
        }
      }
    }
    __finally
    {
      ExceptionOnFail = false;
    }
  }
  else
  {
    FileList = CustomReadDirectoryListing(DirName, UseCache);
  }

  // skip if directory listing fails and user selects "skip"
  if (FileList)
  {
    try
    {
      UnicodeString Directory = UnixIncludeTrailingBackslash(DirName);

      TRemoteFile * File;
      for (int Index = 0; Index < FileList->Count; Index++)
      {
        File = FileList->Files[Index];
        if (IsRealFile(File->FileName))
        {
          CallBackFunc(Directory + File->FileName, File, Param);
          // We should catch ESkipFile here as we do in ProcessFiles.
          // Now we have to handle ESkipFile in every callback implementation.
        }
      }
    }
    __finally
    {
      delete FileList;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReadDirectory(TRemoteFileList * FileList)
{
  try
  {
    CustomReadDirectory(FileList);
  }
  catch (Exception &E)
  {
    CommandError(&E, FmtLoadStr(LIST_DIR_ERROR, ARRAYOFCONST((FileList->Directory))));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReadSymlink(TRemoteFile * SymlinkFile,
  TRemoteFile *& File)
{
  DebugAssert(FFileSystem);
  try
  {
    LogEvent(FORMAT(L"Reading symlink \"%s\".", (SymlinkFile->FileName)));
    FFileSystem->ReadSymlink(SymlinkFile, File);
    ReactOnCommand(fsReadSymlink);
  }
  catch (Exception &E)
  {
    CommandError(&E, FMTLOAD(READ_SYMLINK_ERROR, (SymlinkFile->FileName)));
  }
}
//---------------------------------------------------------------------------
TRemoteFile * TTerminal::ReadFile(const UnicodeString & FileName)
{
  DebugAssert(FFileSystem);
  std::unique_ptr<TRemoteFile> File;
  try
  {
    LogEvent(FORMAT(L"Listing file \"%s\".", (FileName)));
    TRemoteFile * AFile = NULL;
    FFileSystem->ReadFile(FileName, AFile);
    File.reset(AFile);
    ReactOnCommand(fsListFile);
    LogRemoteFile(File.get());
  }
  catch (Exception &E)
  {
    File.reset(NULL);
    CommandError(&E, FMTLOAD(CANT_GET_ATTRS, (FileName)));
  }
  return File.release();
}
//---------------------------------------------------------------------------
TRemoteFile * TTerminal::TryReadFile(const UnicodeString & FileName)
{
  TRemoteFile * File;
  try
  {
    ExceptionOnFail = true;
    try
    {
      File = ReadFile(UnixExcludeTrailingBackslash(FileName));
    }
    __finally
    {
      ExceptionOnFail = false;
    }
  }
  catch(...)
  {
    if (Active)
    {
      File = NULL;
    }
    else
    {
      throw;
    }
  }
  return File;
}
//---------------------------------------------------------------------------
bool TTerminal::FileExists(const UnicodeString & FileName)
{
  std::unique_ptr<TRemoteFile> File(TryReadFile(FileName));
  return (File.get() != NULL);
}
//---------------------------------------------------------------------------
bool TTerminal::DirectoryExists(const UnicodeString & FileName)
{
  std::unique_ptr<TRemoteFile> File(TryReadFile(FileName));
  return
    (File.get() != NULL) &&
    File->IsDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::AnnounceFileListOperation()
{
  FFileSystem->AnnounceFileListOperation();
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::OperationFinish(
  TFileOperationProgressType * Progress, const void * /*Item*/, const UnicodeString & FileName,
  bool Success, TOnceDoneOperation & OnceDoneOperation)
{
  Progress->Finish(FileName, Success, OnceDoneOperation);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::OperationStart(
  TFileOperationProgressType & Progress, TFileOperation Operation, TOperationSide Side, int Count)
{
  OperationStart(Progress, Operation, Side, Count, false, UnicodeString(), 0, odoIdle);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::OperationStart(
  TFileOperationProgressType & Progress, TFileOperation Operation, TOperationSide Side, int Count,
  bool Temp, const UnicodeString & Directory, unsigned long CPSLimit, TOnceDoneOperation OnceDoneOperation)
{
  if (FOperationProgressPersistence != NULL)
  {
    Progress.Restore(*FOperationProgressPersistence);
  }
  Progress.Start(Operation, Side, Count, Temp, Directory, CPSLimit, OnceDoneOperation);
  DebugAssert(FOperationProgress == NULL);
  FOperationProgress = &Progress;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::OperationStop(TFileOperationProgressType & Progress)
{
  DebugAssert(FOperationProgress == &Progress);
  if (FOperationProgressPersistence != NULL)
  {
    Progress.Store(*FOperationProgressPersistence);
  }
  FOperationProgress = NULL;
  Progress.Stop();
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::ProcessFiles(TStrings * FileList,
  TFileOperation Operation, TProcessFileEvent ProcessFile, void * Param,
  TOperationSide Side, bool Ex)
{
  DebugAssert(FileList);

  bool Result = false;
  TOnceDoneOperation OnceDoneOperation = odoIdle;

  try
  {
    TFileOperationProgressType Progress(&DoProgress, &DoFinished);
    OperationStart(Progress, Operation, Side, FileList->Count);

    try
    {
      if (Side == osRemote)
      {
        DebugAssert(FFileSystem != NULL);
        BeginTransaction();
      }

      try
      {
        int Index = 0;
        UnicodeString FileName;
        bool Success;
        while ((Index < FileList->Count) && (Progress.Cancel == csContinue))
        {
          FileName = FileList->Strings[Index];
          TRemoteFile * File = reinterpret_cast<TRemoteFile *>(FileList->Objects[Index]);
          try
          {
            try
            {
              Success = false;
              if (!Ex)
              {
                ProcessFile(FileName, File, Param);
              }
              else
              {
                // not used anymore
                TProcessFileEventEx ProcessFileEx = (TProcessFileEventEx)ProcessFile;
                ProcessFileEx(FileName, File, Param, Index);
              }
              Success = true;
            }
            __finally
            {
              OperationFinish(&Progress, File, FileName, Success, OnceDoneOperation);
            }
          }
          catch(ESkipFile & E)
          {
            TSuspendFileOperationProgress Suspend(OperationProgress);
            if (!HandleException(&E))
            {
              throw;
            }
          }
          Index++;
        }
      }
      __finally
      {
        if (Side == osRemote)
        {
          EndTransaction();
        }
      }

      if (Progress.Cancel == csContinue)
      {
        Result = true;
      }
    }
    __finally
    {
      OperationStop(Progress);
    }
  }
  catch (...)
  {
    OnceDoneOperation = odoIdle;
    // this was missing here. was it by purpose?
    // without it any error message is lost
    throw;
  }

  if (OnceDoneOperation != odoIdle)
  {
    CloseOnCompletion(OnceDoneOperation);
  }

  return Result;
}
//---------------------------------------------------------------------------
// not used anymore
bool __fastcall TTerminal::ProcessFilesEx(TStrings * FileList, TFileOperation Operation,
  TProcessFileEventEx ProcessFile, void * Param, TOperationSide Side)
{
  return ProcessFiles(FileList, Operation, TProcessFileEvent(ProcessFile),
    Param, Side, true);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TTerminal::GetFixedPaths()
{
  DebugAssert(FFileSystem != NULL);
  return FFileSystem->GetFixedPaths();
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetResolvingSymlinks()
{
  return SessionData->ResolveSymlinks && IsCapable[fcResolveSymlink];
}
//---------------------------------------------------------------------------
TUsableCopyParamAttrs __fastcall TTerminal::UsableCopyParamAttrs(int Params)
{
  TUsableCopyParamAttrs Result;
  Result.General =
    FLAGMASK(!IsCapable[fcTextMode], cpaNoTransferMode) |
    FLAGMASK(!IsCapable[fcModeChanging], cpaNoRights) |
    FLAGMASK(!IsCapable[fcModeChanging], cpaNoPreserveReadOnly) |
    FLAGMASK(FLAGSET(Params, cpDelete), cpaNoClearArchive) |
    FLAGMASK(!IsCapable[fcIgnorePermErrors], cpaNoIgnorePermErrors) |
    // the following three are never supported for download,
    // so when they are not supported for upload too,
    // set them in General flags, so that they do not get enabled on
    // Synchronize dialog.
    FLAGMASK(!IsCapable[fcModeChangingUpload], cpaNoRights) |
    FLAGMASK(!IsCapable[fcRemoveCtrlZUpload], cpaNoRemoveCtrlZ) |
    FLAGMASK(!IsCapable[fcRemoveBOMUpload], cpaNoRemoveBOM) |
    FLAGMASK(!IsCapable[fcPreservingTimestampDirs], cpaNoPreserveTimeDirs) |
    FLAGMASK(!IsCapable[fcResumeSupport], cpaNoResumeSupport) |
    FLAGMASK(!IsEncryptingFiles(), cpaNoEncryptNewFiles);
  Result.Download = Result.General | cpaNoClearArchive |
    cpaNoIgnorePermErrors |
    // May be already set in General flags, but it's unconditional here
    cpaNoRights | cpaNoRemoveCtrlZ | cpaNoRemoveBOM | cpaNoEncryptNewFiles;
  Result.Upload = Result.General | cpaNoPreserveReadOnly |
    FLAGMASK(!IsCapable[fcPreservingTimestampUpload], cpaNoPreserveTime);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::IsRecycledFile(UnicodeString FileName)
{
  bool Result = !SessionData->RecycleBinPath.IsEmpty();
  if (Result)
  {
    UnicodeString Path = UnixExtractFilePath(FileName);
    if (Path.IsEmpty())
    {
      Path = CurrentDirectory;
    }
    Result = UnixSamePath(Path, SessionData->RecycleBinPath);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::RecycleFile(const UnicodeString & AFileName, const TRemoteFile * File)
{
  UnicodeString FileName = AFileName;
  if (FileName.IsEmpty())
  {
    DebugAssert(File != NULL);
    FileName = File->FileName;
  }

  bool Result;
  if (IsRecycledFile(FileName))
  {
    Result = true;
  }
  else
  {
    LogEvent(FORMAT(L"Moving file \"%s\" to remote recycle bin '%s'.",
      (FileName, SessionData->RecycleBinPath)));

    TMoveFileParams Params;
    Params.Target = SessionData->RecycleBinPath;
    Params.FileMask = FORMAT(L"*-%s.*", (FormatDateTime(L"yyyymmdd-hhnnss", Now())));
    Params.DontOverwrite = false;

    Result = DoMoveFile(FileName, File, &Params);

    if (Result && (OperationProgress != NULL) && (OperationProgress->Operation == foDelete))
    {
      OperationProgress->Succeeded();
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::TryStartOperationWithFile(
  const UnicodeString & FileName, TFileOperation Operation1, TFileOperation Operation2)
{
  bool Result = true;
  if ((OperationProgress != NULL) &&
      ((OperationProgress->Operation == Operation1) ||
       ((Operation2 != foNone) && (OperationProgress->Operation == Operation2))))
  {
    if (OperationProgress->Cancel != csContinue)
    {
      Result = false;
    }
    else
    {
      OperationProgress->SetFile(FileName);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::StartOperationWithFile(
  const UnicodeString & FileName, TFileOperation Operation1, TFileOperation Operation2)
{
  if (!TryStartOperationWithFile(FileName, Operation1, Operation2))
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DeleteFile(UnicodeString FileName,
  const TRemoteFile * File, void * AParams)
{
  if (FileName.IsEmpty() && File)
  {
    FileName = File->FileName;
  }
  StartOperationWithFile(FileName, foDelete);
  int Params = (AParams != NULL) ? *((int*)AParams) : 0;
  bool Recycle =
    FLAGCLEAR(Params, dfForceDelete) &&
    (SessionData->DeleteToRecycleBin != FLAGSET(Params, dfAlternative)) &&
    !SessionData->RecycleBinPath.IsEmpty();
  if (Recycle && !IsRecycledFile(FileName))
  {
    RecycleFile(FileName, File);
  }
  else
  {
    DoDeleteFile(FFileSystem, FileName, File, Params);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoDeleteFile(
  TCustomFileSystem * FileSystem, const UnicodeString & FileName, const TRemoteFile * File, int Params)
{
  LogEvent(FORMAT(L"Deleting file \"%s\".", (FileName)));
  FileModified(File, FileName, true);

  TRetryOperationLoop RetryLoop(this);
  do
  {
    TRmSessionAction Action(ActionLog, AbsolutePath(FileName, true));
    try
    {
      DebugAssert(FileSystem != NULL);
      // 'File' parameter: SFTPFileSystem needs to know if file is file or directory
      FileSystem->DeleteFile(FileName, File, Params, Action);
      if ((OperationProgress != NULL) && (OperationProgress->Operation == foDelete))
      {
        OperationProgress->Succeeded();
      }
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, Action, FMTLOAD(DELETE_FILE_ERROR, (FileName)));
    }
  }
  while (RetryLoop.Retry());

  // Forget if file was or was not encrypted and use user preferences, if we ever recreate it.
  FEncryptedFileNames.erase(AbsolutePath(FileName, true));
  ReactOnCommand(fsDeleteFile);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DeleteFiles(TStrings * FilesToDelete, int Params)
{
  TValueRestorer<bool> UseBusyCursorRestorer(FUseBusyCursor, false);

  // TODO: avoid resolving symlinks while reading subdirectories.
  // Resolving does not work anyway for relative symlinks in subdirectories
  // (at least for SFTP).
  return ProcessFiles(FilesToDelete, foDelete, DeleteFile, &Params);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DeleteLocalFile(UnicodeString FileName,
  const TRemoteFile * /*File*/, void * Params)
{
  StartOperationWithFile(FileName, foDelete);
  int Deleted;
  if (OnDeleteLocalFile == NULL)
  {
    Deleted = RecursiveDeleteFileChecked(FileName, false);
  }
  else
  {
    OnDeleteLocalFile(FileName, FLAGSET(*((int*)Params), dfAlternative), Deleted);
  }
  AppLogFmt(L"Deleted local file \"%s\"", (FileName));
  if (DebugAlwaysTrue((OperationProgress != NULL) && (OperationProgress->Operation == foDelete)))
  {
    OperationProgress->Succeeded(Deleted);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DeleteLocalFiles(TStrings * FileList, int Params)
{
  return ProcessFiles(FileList, foDelete, DeleteLocalFile, &Params, osLocal);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CustomCommandOnFile(UnicodeString FileName,
  const TRemoteFile * File, void * AParams)
{
  TCustomCommandParams * Params = ((TCustomCommandParams *)AParams);
  if (FileName.IsEmpty() && File)
  {
    FileName = File->FileName;
  }
  StartOperationWithFile(FileName, foCustomCommand);
  LogEvent(FORMAT(L"Executing custom command \"%s\" (%d) on file \"%s\".",
    (Params->Command, Params->Params, FileName)));
  FileModified(File, FileName);
  DoCustomCommandOnFile(FileName, File, Params->Command, Params->Params,
    Params->OutputEvent);
  ReactOnCommand(fsAnyCommand);
}
//---------------------------------------------------------------------------
void TTerminal::PrepareCommandSession(bool NeedCurrentDirectory)
{
  DebugAssert(CommandSessionOpened);
  DebugAssert(FCommandSession->FSProtocol == cfsSCP);
  LogEvent(L"Performing operation on command session.");

  if (FCommandSession->CurrentDirectory != CurrentDirectory)
  {
    FCommandSession->CurrentDirectory = CurrentDirectory;
    // We are likely in transaction, so ReadCurrentDirectory won't get called
    // until transaction ends. But we need to know CurrentDirectory to
    // expand !/ pattern when in CustomCommandOnFiles.
    // Doing this only, when current directory of the main and secondary shell differs,
    // what would be the case before the first file in transaction.
    // Otherwise we would be reading pwd before every time as the
    // CustomCommandOnFile on its own sets FReadCurrentDirectoryPending
    if (NeedCurrentDirectory && FCommandSession->FReadCurrentDirectoryPending)
    {
      FCommandSession->ReadCurrentDirectory();
    }
  }
}
//---------------------------------------------------------------------------
TCustomFileSystem * TTerminal::GetFileSystemForCapability(TFSCapability Capability, bool NeedCurrentDirectory)
{
  TCustomFileSystem * Result;
  if (IsCapable[Capability])
  {
    DebugAssert(FFileSystem != NULL);
    Result = FFileSystem;
  }
  else
  {
    PrepareCommandSession(NeedCurrentDirectory);

    Result = FCommandSession->FFileSystem;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCustomCommandOnFile(UnicodeString FileName,
  const TRemoteFile * File, UnicodeString Command, int Params,
  TCaptureOutputEvent OutputEvent)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    try
    {
      TCustomFileSystem * FileSystem = GetFileSystemForCapability(fcAnyCommand, true);
      DebugAssert((FileSystem != FFileSystem) || IsCapable[fcShellAnyCommand]);

      FileSystem->CustomCommandOnFile(FileName, File, Command, Params, OutputEvent);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, FMTLOAD(CUSTOM_COMMAND_ERROR, (Command, FileName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CustomCommandOnFiles(UnicodeString Command,
  int Params, TStrings * Files, TCaptureOutputEvent OutputEvent)
{
  if (!TRemoteCustomCommand().IsFileListCommand(Command))
  {
    TCustomCommandParams AParams;
    AParams.Command = Command;
    AParams.Params = Params;
    AParams.OutputEvent = OutputEvent;
    ProcessFiles(Files, foCustomCommand, CustomCommandOnFile, &AParams);
  }
  else
  {
    UnicodeString FileList;
    for (int i = 0; i < Files->Count; i++)
    {
      TRemoteFile * File = static_cast<TRemoteFile *>(Files->Objects[i]);
      bool Dir = File->IsDirectory && CanRecurseToDirectory(File);

      if (!Dir || FLAGSET(Params, ccApplyToDirectories))
      {
        AddToShellFileListCommandLine(FileList, Files->Strings[i]);
      }
    }

    TCustomCommandData Data(this);
    UnicodeString Cmd =
      TRemoteCustomCommand(Data, CurrentDirectory, L"", FileList).
        Complete(Command, true);
    if (!DoOnCustomCommand(Cmd))
    {
      DoAnyCommand(Cmd, OutputEvent, NULL);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoOnCustomCommand(const UnicodeString & Command)
{
  bool Result = false;
  if (FOnCustomCommand != NULL)
  {
    FOnCustomCommand(this, Command, Result);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ChangeFileProperties(UnicodeString FileName,
  const TRemoteFile * File, /*const TRemoteProperties*/ void * Properties)
{
  TRemoteProperties * RProperties = (TRemoteProperties *)Properties;
  DebugAssert(RProperties && !RProperties->Valid.Empty());

  if (FileName.IsEmpty() && File)
  {
    FileName = File->FileName;
  }
  StartOperationWithFile(FileName, foSetProperties);
  if (Log->Logging)
  {
    LogEvent(FORMAT(L"Changing properties of \"%s\" (%s)",
      (FileName, BooleanToEngStr(RProperties->Recursive))));
    if (RProperties->Valid.Contains(vpRights))
    {
      LogEvent(FORMAT(L" - mode: \"%s\"", (RProperties->Rights.ModeStr)));
    }
    if (RProperties->Valid.Contains(vpGroup))
    {
      LogEvent(FORMAT(L" - group: %s", (RProperties->Group.LogText)));
    }
    if (RProperties->Valid.Contains(vpOwner))
    {
      LogEvent(FORMAT(L" - owner: %s", (RProperties->Owner.LogText)));
    }
    if (RProperties->Valid.Contains(vpModification))
    {
      LogEvent(FORMAT(L" - modification: \"%s\"",
        (FormatDateTime(L"dddddd tt",
           UnixToDateTime(RProperties->Modification, SessionData->DSTMode)))));
    }
    if (RProperties->Valid.Contains(vpLastAccess))
    {
      LogEvent(FORMAT(L" - last access: \"%s\"",
        (FormatDateTime(L"dddddd tt",
           UnixToDateTime(RProperties->LastAccess, SessionData->DSTMode)))));
    }
  }
  FileModified(File, FileName);
  DoChangeFileProperties(FileName, File, RProperties);
  ReactOnCommand(fsChangeProperties);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoChangeFileProperties(const UnicodeString FileName,
  const TRemoteFile * File, const TRemoteProperties * Properties)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    TChmodSessionAction Action(ActionLog, AbsolutePath(FileName, true));
    try
    {
      DebugAssert(FFileSystem);
      FFileSystem->ChangeFileProperties(FileName, File, Properties, Action);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, Action, FMTLOAD(CHANGE_PROPERTIES_ERROR, (FileName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ChangeFilesProperties(TStrings * FileList,
  const TRemoteProperties * Properties)
{
  TValueRestorer<bool> UseBusyCursorRestorer(FUseBusyCursor, false);

  AnnounceFileListOperation();
  ProcessFiles(FileList, foSetProperties, ChangeFileProperties, (void *)Properties);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::LoadFilesProperties(TStrings * FileList)
{
  TValueRestorer<bool> UseBusyCursorRestorer(FUseBusyCursor, false);

  // see comment in TSFTPFileSystem::IsCapable
  bool Result =
    IsCapable[fcLoadingAdditionalProperties] &&
    FFileSystem->LoadFilesProperties(FileList);
  if (Result && SessionData->CacheDirectories &&
      (FileList->Count > 0) &&
      (dynamic_cast<TRemoteFile *>(FileList->Objects[0])->Directory == FFiles))
  {
    AddCachedFileList(FFiles);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCalculateFileSize(UnicodeString FileName,
  const TRemoteFile * File, void * Param)
{
  // This is called for top-level entries only
  TCalculateSizeParams * AParams = static_cast<TCalculateSizeParams*>(Param);

  if (AParams->Stats->FoundFiles != NULL)
  {
    UnicodeString DirPath = UnixExtractFilePath(UnixExcludeTrailingBackslash(File->FullFileName));
    if ((DirPath != AParams->LastDirPath) ||
        (AParams->Files == NULL))
    {
      AParams->Files = new TCollectedFileList();
      AParams->LastDirPath = DirPath;
      AParams->Stats->FoundFiles->AddObject(AParams->LastDirPath, AParams->Files);
    }
  }

  __int64 PrevSize = AParams->Size;
  CalculateFileSize(FileName, File, Param);

  if (AParams->Stats->CalculatedSizes != NULL)
  {
    __int64 Size = AParams->Size - PrevSize;
    AParams->Stats->CalculatedSizes->push_back(Size);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CalculateFileSize(UnicodeString FileName,
  const TRemoteFile * File, /*TCalculateSizeParams*/ void * Param)
{
  DebugAssert(Param);
  DebugAssert(File);
  TCalculateSizeParams * AParams = static_cast<TCalculateSizeParams*>(Param);

  if (FileName.IsEmpty())
  {
    FileName = File->FileName;
  }

  if (!TryStartOperationWithFile(FileName, foCalculateSize))
  {
    AParams->Result = false;
    // Combined with csIgnoreErrors, this will not abort completely, but we get called again
    // with next sibling of our parent directory (and we get again to this branch, throwing again)
    Abort();
  }

  if ((AParams->CopyParam == NULL) ||
      DoAllowRemoteFileTransfer(File, AParams->CopyParam, FLAGSET(AParams->Params, csDisallowTemporaryTransferFiles)))
  {
    int CollectionIndex = -1;
    if (AParams->Files != NULL)
    {
      UnicodeString FullFileName = UnixExcludeTrailingBackslash(File->FullFileName);
      CollectionIndex = AParams->Files->Add(FullFileName, File->Duplicate(), File->IsDirectory);
    }

    if (File->IsDirectory)
    {
      if (CanRecurseToDirectory(File))
      {
        if (!AParams->AllowDirs)
        {
          AParams->Result = false;
        }
        else if (FLAGSET(AParams->Params, csStopOnFirstFile) && (AParams->Stats->Files > 0))
        {
          // do not waste time recursing into a folder, if we already found some files
        }
        else
        {
          if (FLAGCLEAR(AParams->Params, csStopOnFirstFile))
          {
            LogEvent(FORMAT(L"Getting size of directory \"%s\"", (FileName)));
          }

          // pass in full path so we get it back in file list for AllowTransfer() exclusion
          if (!DoCalculateDirectorySize(File->FullFileName, AParams))
          {
            if (CollectionIndex >= 0)
            {
              AParams->Files->DidNotRecurse(CollectionIndex);
            }
          }
        }
      }

      AParams->Stats->Directories++;
    }
    else
    {
      AParams->Size += File->Resolve()->Size;

      AParams->Stats->Files++;
    }

    if (File->IsSymLink)
    {
      AParams->Stats->SymLinks++;
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoCalculateDirectorySize(const UnicodeString & FileName, TCalculateSizeParams * Params)
{
  bool Result = false;
  if (FLAGSET(Params->Params, csStopOnFirstFile) && (Configuration->ActualLogProtocol >= 1))
  {
    LogEvent(FORMAT(L"Checking if remote directory \"%s\" is empty", (FileName)));
  }

  TRetryOperationLoop RetryLoop(this);
  do
  {
    try
    {
      ProcessDirectory(FileName, CalculateFileSize, Params, Params->UseCache);
      Result = true;
    }
    catch(Exception & E)
    {
      // We can probably replace the csIgnoreErrors with IgnoreErrors argument of the ProcessDirectory
      if (!Active || ((Params->Params & csIgnoreErrors) == 0))
      {
        RetryLoop.Error(E, FMTLOAD(CALCULATE_SIZE_ERROR, (FileName)));
      }
    }
  }
  while (RetryLoop.Retry());

  if (Configuration->ActualLogProtocol >= 1)
  {
    if (Params->Stats->Files == 0)
    {
      LogEvent(FORMAT(L"Remote directory \"%s\" is empty", (FileName)));
    }
    else
    {
      LogEvent(FORMAT(L"Remote directory \"%s\" is not empty", (FileName)));
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool TTerminal::CalculateFilesSize(TStrings * FileList, __int64 & Size, TCalculateSizeParams & Params)
{
  // With FTP protocol, we may use DSIZ command from
  // draft-peterson-streamlined-ftp-command-extensions-10
  // Implemented by Serv-U FTP.

  TValueRestorer<bool> UseBusyCursorRestorer(FUseBusyCursor, false);

  ProcessFiles(FileList, foCalculateSize, DoCalculateFileSize, &Params);
  Size = Params.Size;
  if (Configuration->ActualLogProtocol >= 1)
  {
    LogEvent(FORMAT(L"Size of %d remote files/folders calculated as %s", (FileList->Count, IntToStr(Size))));
  }
  return Params.Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CalculateSubFoldersChecksum(
  const UnicodeString & Alg, TStrings * FileList, TCalculatedChecksumEvent OnCalculatedChecksum,
  TFileOperationProgressType * OperationProgress, bool FirstLevel)
{
  // recurse into subdirectories only if we have callback function
  if (OnCalculatedChecksum != NULL)
  {
    int Index = 0;
    TOnceDoneOperation OnceDoneOperation; // unused
    while ((Index < FileList->Count) && !OperationProgress->Cancel)
    {
      UnicodeString FileName = FileList->Strings[Index];
      TRemoteFile * File = DebugNotNull(dynamic_cast<TRemoteFile *>(FileList->Objects[Index]));

      if (File->IsDirectory &&
          CanRecurseToDirectory(File) &&
          IsRealFile(File->FileName))
      {
        OperationProgress->SetFile(File->FileName);
        std::unique_ptr<TRemoteFileList> SubFiles(CustomReadDirectoryListing(File->FullFileName, false));

        if (SubFiles.get() != NULL)
        {
          std::unique_ptr<TStrings> SubFileList(new TStringList());
          bool Success = false;
          try
          {
            OperationProgress->SetFile(File->FileName);

            for (int Index = 0; Index < SubFiles->Count; Index++)
            {
              TRemoteFile * SubFile = SubFiles->Files[Index];
              UnicodeString SubFileName = UnixCombinePaths(FileName, SubFile->FileName);
              SubFileList->AddObject(SubFileName, SubFile);
            }

            FFileSystem->CalculateFilesChecksum(Alg, SubFileList.get(), OnCalculatedChecksum, OperationProgress, false);

            Success = true;
          }
          __finally
          {
            if (FirstLevel)
            {
              OperationProgress->Finish(File->FileName, Success, OnceDoneOperation);
            }
          }
        }
      }
      Index++;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CalculateFilesChecksum(
  const UnicodeString & Alg, TStrings * FileList, TCalculatedChecksumEvent OnCalculatedChecksum)
{
  TFileOperationProgressType Progress(&DoProgress, &DoFinished);
  OperationStart(Progress, foCalculateChecksum, osRemote, FileList->Count);

  try
  {
    TCustomFileSystem * FileSystem = GetFileSystemForCapability(fcCalculatingChecksum);

    UnicodeString NormalizedAlg = FileSystem->CalculateFilesChecksumInitialize(Alg);

    FileSystem->CalculateFilesChecksum(NormalizedAlg, FileList, OnCalculatedChecksum, &Progress, true);
  }
  __finally
  {
    OperationStop(Progress);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::RenameFile(const TRemoteFile * File, const UnicodeString & NewName)
{
  // Already checked in TUnixDirView::InternalEdit
  if (DebugAlwaysTrue(File->FileName != NewName))
  {
    FileModified(File, File->FileName);
    LogEvent(FORMAT(L"Renaming file \"%s\" to \"%s\".", (File->FileName, NewName)));
    if (DoRenameFile(File->FileName, File, NewName, false, false))
    {
      ReactOnCommand(fsRenameFile);
    }
  }
}
//---------------------------------------------------------------------------
bool TTerminal::DoRenameOrCopyFile(
  bool Rename, const UnicodeString & FileName, const TRemoteFile * File, const UnicodeString & NewName,
  bool Move, bool DontOverwrite, bool IsBatchOperation)
{
  TBatchOverwrite BatchOverwrite = (IsBatchOperation ? OperationProgress->BatchOverwrite : boNo);
  UnicodeString AbsoluteFileName = AbsolutePath(FileName, true);
  UnicodeString AbsoluteNewName = AbsolutePath(NewName, true);
  bool Result = true;
  bool ExistenceKnown = false;
  std::unique_ptr<TRemoteFile> DuplicateFile;
  if (UnixSamePath(AbsoluteFileName, AbsoluteNewName))
  {
    LogEvent(FORMAT(L"Target \"%s\" is same as source \"%s\" - skipping.", (AbsoluteNewName, AbsoluteFileName)));
    Result = false;
  }
  else if (BatchOverwrite == boNone)
  {
    DebugAssert(!DontOverwrite); // unsupported combination
    Result = !FileExists(AbsoluteNewName);
    ExistenceKnown = true;
  }
  else if (BatchOverwrite == boAll)
  {
    // noop
  }
  else if (DebugAlwaysTrue(BatchOverwrite == boNo) &&
           Configuration->ConfirmOverwriting &&
           !DontOverwrite)
  {
    DuplicateFile.reset(TryReadFile(AbsoluteNewName));
    ExistenceKnown = true;

    if (DuplicateFile.get() != NULL)
    {
      UnicodeString QuestionFmt;
      TQueryType QueryType;
      if (DuplicateFile->IsDirectory)
      {
        QuestionFmt = MainInstructions(LoadStr(DIRECTORY_OVERWRITE)) + LoadStr(DIRECTORY_OVERWRITE_WARNING);
        QueryType = qtWarning;
      }
      else
      {
        QuestionFmt = MainInstructions(LoadStr(FILE_OVERWRITE));
        QueryType = qtConfirmation;
      }
      TQueryParams Params(qpNeverAskAgainCheck);
      UnicodeString Question = FORMAT(QuestionFmt, (NewName));
      unsigned int Answers = qaYes | qaNo | FLAGMASK(OperationProgress != NULL, qaCancel) | FLAGMASK(IsBatchOperation, qaYesToAll | qaNoToAll);
      unsigned int Answer = QueryUser(Question, NULL, Answers, &Params, QueryType);
      switch (Answer)
      {
        case qaNeverAskAgain:
          Configuration->ConfirmOverwriting = false;
          Result = true;
          break;

        case qaYes:
          Result = true;
          break;

        case qaNo:
          Result = false;
          break;

        case qaYesToAll:
          Result = true;
          if (DebugAlwaysTrue(IsBatchOperation))
          {
            OperationProgress->SetBatchOverwrite(boAll);
          }
          break;

        case qaNoToAll:
          Result = false;
          if (DebugAlwaysTrue(IsBatchOperation))
          {
            OperationProgress->SetBatchOverwrite(boNone);
          }
          break;

        case qaCancel:
          Result = false;
          OperationProgress->SetCancel(csCancel);
          break;

        default:
          Result = false;
          DebugFail();
          break;
      }
    }
  }

  if (Result)
  {
    // Prevent destroying TRemoteFile between delete and rename
    BeginTransaction();
    try
    {
      TCustomFileSystem * FileSystem;
      if (!Rename)
      {
        if (IsCapable[fcSecondaryShell] &&
            File->IsDirectory &&
            (FCommandSession != NULL)) // Won't be in scripting, there we let it fail later
        {
          PrepareCommandSession();
          FileSystem = FCommandSession->FFileSystem;
        }
        else
        {
          FileSystem = GetFileSystemForCapability(fcRemoteCopy);
        }
      }
      else
      {
        FileSystem = FFileSystem;
      }

      if (!IsCapable[fcMoveOverExistingFile] && !DontOverwrite)
      {
        if (!ExistenceKnown)
        {
          DuplicateFile.reset(TryReadFile(AbsoluteNewName));
        }

        if (DuplicateFile.get() != NULL)
        {
          DoDeleteFile(FileSystem, AbsoluteNewName, DuplicateFile.get(), 0);
        }
      }

      TRetryOperationLoop RetryLoop(this);
      do
      {
        DebugAssert(FileSystem != NULL);
        if (Rename)
        {
          TMvSessionAction Action(ActionLog, AbsoluteFileName, AbsoluteNewName);
          try
          {
            FileSystem->RenameFile(FileName, File, NewName, !DontOverwrite);
          }
          catch (Exception & E)
          {
            UnicodeString Message = FMTLOAD(Move ? MOVE_FILE_ERROR : RENAME_FILE_ERROR, (FileName, NewName));
            RetryLoop.Error(E, Action, Message);
          }
        }
        else
        {
          TCpSessionAction Action(ActionLog, AbsoluteFileName, AbsoluteNewName);
          try
          {
            FileSystem->CopyFile(FileName, File, NewName, !DontOverwrite);
          }
          catch (Exception & E)
          {
            RetryLoop.Error(E, Action, FMTLOAD(COPY_FILE_ERROR, (FileName, NewName)));
          }
        }
      }
      while (RetryLoop.Retry());
      Result = RetryLoop.Succeeded();
    }
    __finally
    {
      EndTransaction();
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoRenameFile(
  const UnicodeString & FileName, const TRemoteFile * File, const UnicodeString & NewName, bool Move, bool DontOverwrite)
{
  // Can be foDelete when recycling (and overwrite should not happen in this case)
  bool IsBatchOperation = (OperationProgress != NULL) && (OperationProgress->Operation == foRemoteMove);
  return DoRenameOrCopyFile(true, FileName, File, NewName, Move, DontOverwrite, IsBatchOperation);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoMoveFile(const UnicodeString & FileName, const TRemoteFile * File, /*const TMoveFileParams*/ void * Param)
{
  StartOperationWithFile(FileName, foRemoteMove, foDelete);
  DebugAssert(Param != NULL);
  const TMoveFileParams & Params = *static_cast<const TMoveFileParams*>(Param);
  UnicodeString NewName = UnixIncludeTrailingBackslash(Params.Target) +
    MaskFileName(UnixExtractFileName(FileName), Params.FileMask);
  LogEvent(FORMAT(L"Moving file \"%s\" to \"%s\".", (FileName, NewName)));
  FileModified(File, FileName);
  bool Result = DoRenameFile(FileName, File, NewName, true, Params.DontOverwrite);
  if (Result)
  {
    ReactOnCommand(fsMoveFile);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::MoveFile(const UnicodeString FileName, const TRemoteFile * File, /*const TMoveFileParams*/ void * Param)
{
  DoMoveFile(FileName, File, Param);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::MoveFiles(
  TStrings * FileList, const UnicodeString & Target, const UnicodeString & FileMask, bool DontOverwrite)
{
  TMoveFileParams Params;
  Params.Target = Target;
  Params.FileMask = FileMask;
  Params.DontOverwrite = DontOverwrite;
  bool Result;
  BeginTransaction();
  try
  {
    Result = ProcessFiles(FileList, foRemoteMove, MoveFile, &Params);
  }
  __finally
  {
    if (Active)
    {
      // Only after the move, as with encryption, the folders can be read and cached before the move
      // (when determining if the target exists and is encrypted)
      DirectoryModified(Target, true);

      UnicodeString WithTrailing = UnixIncludeTrailingBackslash(CurrentDirectory);
      bool PossiblyMoved = false;
      // check if we was moving current directory.
      // this is just optimization to avoid checking existence of current
      // directory after each move operation.
      for (int Index = 0; !PossiblyMoved && (Index < FileList->Count); Index++)
      {
        const TRemoteFile * File =
          dynamic_cast<const TRemoteFile *>(FileList->Objects[Index]);
        // File can be NULL, and filename may not be full path,
        // but currently this is the only way we can move (at least in GUI)
        // current directory
        if ((File != NULL) &&
            File->IsDirectory &&
            ((CurrentDirectory.SubString(1, FileList->Strings[Index].Length()) == FileList->Strings[Index]) &&
             ((FileList->Strings[Index].Length() == CurrentDirectory.Length()) ||
              (CurrentDirectory[FileList->Strings[Index].Length() + 1] == L'/'))))
        {
          PossiblyMoved = true;
        }
      }

      if (PossiblyMoved && !FileExists(CurrentDirectory))
      {
        UnicodeString NearestExisting = CurrentDirectory;
        do
        {
          NearestExisting = UnixExtractFileDir(NearestExisting);
        }
        while (!IsUnixRootPath(NearestExisting) && !FileExists(NearestExisting));

        ChangeDirectory(NearestExisting);
      }
    }
    EndTransaction();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCopyFile(
  const UnicodeString & FileName, const TRemoteFile * File, const UnicodeString & NewName, bool DontOverwrite)
{
  bool IsBatchOperation = DebugAlwaysTrue(OperationProgress != NULL);
  bool Move = false; // not used
  DoRenameOrCopyFile(false, FileName, File, NewName, Move, DontOverwrite, IsBatchOperation);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CopyFile(const UnicodeString FileName,
  const TRemoteFile * File, /*const TMoveFileParams*/ void * Param)
{
  StartOperationWithFile(FileName, foRemoteCopy);
  DebugAssert(Param != NULL);
  const TMoveFileParams & Params = *static_cast<const TMoveFileParams*>(Param);
  UnicodeString NewName = UnixIncludeTrailingBackslash(Params.Target) +
    MaskFileName(UnixExtractFileName(FileName), Params.FileMask);
  LogEvent(FORMAT(L"Copying file \"%s\" to \"%s\".", (FileName, NewName)));
  DoCopyFile(FileName, File, NewName, Params.DontOverwrite);
  ReactOnCommand(fsCopyFile);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CopyFiles(
  TStrings * FileList, const UnicodeString & Target, const UnicodeString & FileMask, bool DontOverwrite)
{
  TMoveFileParams Params;
  Params.Target = Target;
  Params.FileMask = FileMask;
  Params.DontOverwrite = DontOverwrite;
  DirectoryModified(Target, true);
  return ProcessFiles(FileList, foRemoteCopy, CopyFile, &Params);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CreateDirectory(const UnicodeString & DirName, const TRemoteProperties * Properties)
{
  DebugAssert(FFileSystem);
  DebugAssert(Properties != NULL);
  EnsureNonExistence(DirName);
  FileModified(NULL, DirName);

  LogEvent(FORMAT(L"Creating directory \"%s\".", (DirName)));
  bool Encrypt = Properties->Valid.Contains(vpEncrypt) && Properties->Encrypt;
  DoCreateDirectory(DirName, Encrypt);

  TValidProperties RemainingPropeties = Properties->Valid - (TValidProperties() << vpEncrypt);
  if (!RemainingPropeties.Empty() &&
      (IsCapable[fcModeChanging] || IsCapable[fcOwnerChanging] || IsCapable[fcGroupChanging]))
  {
    DoChangeFileProperties(DirName, NULL, Properties);
  }

  ReactOnCommand(fsCreateDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCreateDirectory(const UnicodeString & DirName, bool Encrypt)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    TMkdirSessionAction Action(ActionLog, AbsolutePath(DirName, true));
    try
    {
      DebugAssert(FFileSystem);
      FFileSystem->CreateDirectory(DirName, Encrypt);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, Action, FMTLOAD(CREATE_DIR_ERROR, (DirName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CreateLink(const UnicodeString FileName,
  const UnicodeString PointTo, bool Symbolic)
{
  DebugAssert(FFileSystem);
  EnsureNonExistence(FileName);
  if (SessionData->CacheDirectories)
  {
    DirectoryModified(CurrentDirectory, false);
  }

  LogEvent(FORMAT(L"Creating link \"%s\" to \"%s\" (symbolic: %s).",
    (FileName, PointTo, BooleanToEngStr(Symbolic))));
  DoCreateLink(FileName, PointTo, Symbolic);
  ReactOnCommand(fsCreateDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCreateLink(const UnicodeString FileName,
  const UnicodeString PointTo, bool Symbolic)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    try
    {
      DebugAssert(FFileSystem);
      FFileSystem->CreateLink(FileName, PointTo, Symbolic);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, FMTLOAD(CREATE_LINK_ERROR, (FileName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::HomeDirectory()
{
  DebugAssert(FFileSystem);
  try
  {
    LogEvent(L"Changing directory to home directory.");
    FFileSystem->HomeDirectory();
    ReactOnCommand(fsHomeDirectory);
  }
  catch (Exception &E)
  {
    CommandError(&E, LoadStr(CHANGE_HOMEDIR_ERROR));
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetHomeDirectory()
{
  return FFileSystem->GetHomeDirectory();
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ChangeDirectory(const UnicodeString Directory)
{
  DebugAssert(FFileSystem);
  try
  {
    UnicodeString CachedDirectory;
    DebugAssert(!SessionData->CacheDirectoryChanges || (FDirectoryChangesCache != NULL));
    // never use directory change cache during startup, this ensures, we never
    // end-up initially in non-existing directory
    if ((Status == ssOpened) &&
        SessionData->CacheDirectoryChanges &&
        FDirectoryChangesCache->GetDirectoryChange(PeekCurrentDirectory(),
          Directory, CachedDirectory))
    {
      LogEvent(FORMAT(L"Cached directory change via \"%s\" to \"%s\".",
        (Directory, CachedDirectory)));
      FFileSystem->CachedChangeDirectory(CachedDirectory);
    }
    else
    {
      LogEvent(FORMAT(L"Changing directory to \"%s\".", (Directory)));
      FFileSystem->ChangeDirectory(Directory);
    }
    FLastDirectoryChange = Directory;
    ReactOnCommand(fsChangeDirectory);
  }
  catch (Exception &E)
  {
    CommandError(&E, FMTLOAD(CHANGE_DIR_ERROR, (Directory)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LookupUsersGroups()
{
  if (!FUsersGroupsLookedup && (SessionData->LookupUserGroups != asOff) &&
      IsCapable[fcUserGroupListing])
  {
    DebugAssert(FFileSystem);

    try
    {
      FUsersGroupsLookedup = true;
      LogEvent(L"Looking up groups and users.");
      FFileSystem->LookupUsersGroups();
      ReactOnCommand(fsLookupUsersGroups);

      if (Log->Logging)
      {
        FGroups.Log(this, L"groups");
        FMembership.Log(this, L"membership");
        FUsers.Log(this, L"users");
      }
    }
    catch (Exception &E)
    {
      if (!Active || (SessionData->LookupUserGroups == asOn))
      {
        CommandError(&E, LoadStr(LOOKUP_GROUPS_ERROR));
      }
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::AllowedAnyCommand(const UnicodeString Command)
{
  return !Command.Trim().IsEmpty();
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetCommandSessionOpened()
{
  // consider secondary terminal open in "ready" state only
  // so we never do keepalives on it until it is completely initialized
  return (FCommandSession != NULL) &&
    (FCommandSession->Status == ssOpened);
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminal::CreateSecondarySession(const UnicodeString & Name, TSessionData * SessionData)
{
  std::unique_ptr<TTerminal> Result(new TSecondaryTerminal(this, SessionData, Configuration, Name, ActionLog));

  Result->AutoReadDirectory = false;

  Result->FExceptionOnFail = FExceptionOnFail;

  Result->OnQueryUser = OnQueryUser;
  Result->OnPromptUser = OnPromptUser;
  Result->OnShowExtendedException = OnShowExtendedException;
  Result->OnProgress = OnProgress;
  Result->OnFinished = OnFinished;
  Result->OnInformation = OnInformation;
  Result->OnCustomCommand = OnCustomCommand;
  // do not copy OnDisplayBanner to avoid it being displayed
  return Result.release();
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FillSessionDataForCode(TSessionData * Data)
{
  const TSessionInfo & SessionInfo = GetSessionInfo();
  if (!SessionInfo.HostKeyFingerprintSHA256.IsEmpty())
  {
    Data->HostKey = SessionInfo.HostKeyFingerprintSHA256;
  }
  else if (SessionInfo.CertificateVerifiedManually && DebugAlwaysTrue(!SessionInfo.CertificateFingerprintSHA256.IsEmpty()))
  {
    Data->HostKey = SessionInfo.CertificateFingerprintSHA256;
  }

  if (FTunnel != NULL)
  {
    const TSessionInfo & TunnelSessionInfo = FTunnel->GetSessionInfo();
    if (DebugAlwaysTrue(!TunnelSessionInfo.HostKeyFingerprintSHA256.IsEmpty()))
    {
      Data->TunnelHostKey = TunnelSessionInfo.HostKeyFingerprintSHA256;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::UpdateSessionCredentials(TSessionData * Data)
{
  // Only if it differs from the original session data?
  // Because this way, we persist the username redundantly in workspace session linked to a stored site.
  Data->UserName = UserName;
  Data->Password = Password;
  if (!FRememberedPassword.IsEmpty() && (FRememberedPasswordKind == pkPassphrase))
  {
    Data->Passphrase = GetRememberedPassword();
  }
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminal::GetCommandSession()
{
  if ((FCommandSession != NULL) && !FCommandSession->Active)
  {
    SAFE_DESTROY(FCommandSession);
  }

  if (FCommandSession == NULL)
  {
    // transaction cannot be started yet to allow proper matching transaction
    // levels between main and command session
    DebugAssert(FInTransaction == 0);

    std::unique_ptr<TSessionData> CommandSessionData(SessionData->Clone());
    CommandSessionData->RemoteDirectory = CurrentDirectory;
    CommandSessionData->FSProtocol = fsSCPonly;
    CommandSessionData->ClearAliases = false;
    CommandSessionData->UnsetNationalVars = false;
    CommandSessionData->LookupUserGroups = asOff;

    FCommandSession = CreateSecondarySession(L"Shell", CommandSessionData.get());

    FCommandSession->AutoReadDirectory = false;

    FCommandSession->FExceptionOnFail = FExceptionOnFail;

    FCommandSession->OnQueryUser = OnQueryUser;
    FCommandSession->OnPromptUser = OnPromptUser;
    FCommandSession->OnShowExtendedException = OnShowExtendedException;
    FCommandSession->OnProgress = OnProgress;
    FCommandSession->OnFinished = OnFinished;
    FCommandSession->OnInformation = OnInformation;
    FCommandSession->OnCustomCommand = OnCustomCommand;
    // do not copy OnDisplayBanner to avoid it being displayed
  }

  return FCommandSession;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::AnyCommand(const UnicodeString Command,
  TCaptureOutputEvent OutputEvent)
{

  #pragma warn -inl
  class TOutputProxy
  {
  public:
    __fastcall TOutputProxy(TCallSessionAction & Action, TCaptureOutputEvent OutputEvent) :
      FAction(Action),
      FOutputEvent(OutputEvent)
    {
    }

    void __fastcall Output(const UnicodeString & Str, TCaptureOutputType OutputType)
    {
      switch (OutputType)
      {
        case cotOutput:
          FAction.AddOutput(Str, false);
          break;
        case cotError:
          FAction.AddOutput(Str, true);
          break;
        case cotExitCode:
          FAction.ExitCode(StrToInt(Str));
          break;
      }

      if (FOutputEvent != NULL)
      {
        FOutputEvent(Str, OutputType);
      }
    }

  private:
    TCallSessionAction & FAction;
    TCaptureOutputEvent FOutputEvent;
  };
  #pragma warn .inl

  TCallSessionAction Action(ActionLog, Command, CurrentDirectory);
  TOutputProxy ProxyOutputEvent(Action, OutputEvent);
  DoAnyCommand(Command, ProxyOutputEvent.Output, &Action);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoAnyCommand(const UnicodeString Command,
  TCaptureOutputEvent OutputEvent, TCallSessionAction * Action)
{
  DebugAssert(FFileSystem);
  try
  {
    DirectoryModified(CurrentDirectory, false);
    LogEvent(L"Executing user defined command.");
    TCustomFileSystem * FileSystem = GetFileSystemForCapability(fcAnyCommand);
    FileSystem->AnyCommand(Command, OutputEvent);
    if (CommandSessionOpened && (FileSystem == FCommandSession->FFileSystem))
    {
      FCommandSession->FFileSystem->ReadCurrentDirectory();

      // synchronize pwd (by purpose we lose transaction optimization here)
      ChangeDirectory(FCommandSession->CurrentDirectory);
    }
    ReactOnCommand(fsAnyCommand);
  }
  catch (Exception &E)
  {
    if (Action != NULL)
    {
      RollbackAction(*Action, NULL, &E);
    }
    if (ExceptionOnFail || (E.InheritsFrom(__classid(EFatal)))) throw;
      else HandleExtendedException(&E);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoCreateLocalFile(const UnicodeString FileName,
  TFileOperationProgressType * OperationProgress, HANDLE * AHandle,
  bool NoConfirmation)
{
  bool Result = true;
  bool Done;
  unsigned int CreateAttr = FILE_ATTRIBUTE_NORMAL;
  do
  {
    *AHandle = CreateFile(ApiPath(FileName).c_str(), GENERIC_WRITE, FILE_SHARE_READ,
      NULL, CREATE_ALWAYS, CreateAttr, 0);
    Done = (*AHandle != INVALID_HANDLE_VALUE);
    if (!Done)
    {
      // save the error, otherwise it gets overwritten by call to FileExists
      int LastError = GetLastError();
      int FileAttr = 0; // shut up
      if (::FileExists(ApiPath(FileName)) &&
        (((FileAttr = FileGetAttrFix(ApiPath(FileName))) & (faReadOnly | faHidden)) != 0))
      {
        if (FLAGSET(FileAttr, faReadOnly))
        {
          OperationProgress->LockUserSelections();
          try
          {
            if (OperationProgress->BatchOverwrite == boNone)
            {
              Result = false;
            }
            else if ((OperationProgress->BatchOverwrite != boAll) && !NoConfirmation)
            {
              unsigned int Answer;

              {
                TSuspendFileOperationProgress Suspend(OperationProgress);
                Answer = QueryUser(
                  MainInstructions(FMTLOAD(READ_ONLY_OVERWRITE, (FileName))), NULL,
                  qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll, 0);
              }

              switch (Answer) {
                case qaYesToAll: OperationProgress->SetBatchOverwrite(boAll); break;
                case qaCancel: OperationProgress->SetCancel(csCancel); // continue on next case
                case qaNoToAll: OperationProgress->SetBatchOverwrite(boNone);
                case qaNo: Result = false; break;
              }
            }
          }
          __finally
          {
            OperationProgress->UnlockUserSelections();
          }
        }
        else
        {
          DebugAssert(FLAGSET(FileAttr, faHidden));
          Result = true;
        }

        if (Result)
        {
          CreateAttr |=
            FLAGMASK(FLAGSET(FileAttr, faHidden), FILE_ATTRIBUTE_HIDDEN) |
            FLAGMASK(FLAGSET(FileAttr, faReadOnly), FILE_ATTRIBUTE_READONLY);

          FILE_OPERATION_LOOP_BEGIN
          {
            if (FileSetAttr(ApiPath(FileName), FileAttr & ~(faReadOnly | faHidden)) != 0)
            {
              RaiseLastOSError();
            }
          }
          FILE_OPERATION_LOOP_END(FMTLOAD(CANT_SET_ATTRS, (FileName)));
        }
        else
        {
          Done = true;
        }
      }
      else
      {
        RaiseLastOSError(LastError);
      }
    }
  }
  while (!Done);

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CreateLocalFile(const UnicodeString FileName,
  TFileOperationProgressType * OperationProgress, HANDLE * AHandle,
  bool NoConfirmation)
{
  DebugAssert(AHandle);
  bool Result = true;

  FILE_OPERATION_LOOP_BEGIN
  {
    Result = DoCreateLocalFile(FileName, OperationProgress, AHandle, NoConfirmation);
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(CREATE_FILE_ERROR, (FileName)));

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::OpenLocalFile(const UnicodeString FileName,
  unsigned int Access, int * AAttrs, HANDLE * AHandle, __int64 * ACTime,
  __int64 * AMTime, __int64 * AATime, __int64 * ASize,
  bool TryWriteReadOnly)
{
  int Attrs = 0;
  HANDLE Handle = 0;

  FILE_OPERATION_LOOP_BEGIN
  {
    UnicodeString FileNameApi = ApiPath(FileName);
    Attrs = FileGetAttrFix(FileNameApi);
    if (Attrs < 0)
    {
      RaiseLastOSError();
    }
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(FILE_NOT_EXISTS, (FileName)));

  if (FLAGCLEAR(Attrs, faDirectory) || (AHandle == NULL))
  {
    bool NoHandle = false;
    if (!TryWriteReadOnly && (Access == GENERIC_WRITE) &&
        ((Attrs & faReadOnly) != 0))
    {
      Access = GENERIC_READ;
      NoHandle = true;
    }

    FILE_OPERATION_LOOP_BEGIN
    {
      DWORD Flags = FLAGMASK(FLAGSET(Attrs, faDirectory), FILE_FLAG_BACKUP_SEMANTICS);
      Handle = CreateFile(ApiPath(FileName).c_str(), Access,
        Access == GENERIC_READ ? FILE_SHARE_READ | FILE_SHARE_WRITE : FILE_SHARE_READ,
        NULL, OPEN_EXISTING, Flags, 0);
      if (Handle == INVALID_HANDLE_VALUE)
      {
        Handle = 0;
        RaiseLastOSError();
      }
    }
    FILE_OPERATION_LOOP_END(FMTLOAD(OPENFILE_ERROR, (FileName)));

    try
    {
      if (AATime || AMTime || ACTime)
      {
        FILETIME ATime;
        FILETIME MTime;
        FILETIME CTime;

        // Get last file access and modification time
        FILE_OPERATION_LOOP_BEGIN
        {
          THROWOSIFFALSE(GetFileTime(Handle, &CTime, &ATime, &MTime));
        }
        FILE_OPERATION_LOOP_END(FMTLOAD(CANT_GET_ATTRS, (FileName)));

        if (ACTime)
        {
          *ACTime = ConvertTimestampToUnixSafe(CTime, SessionData->DSTMode);
        }
        if (AATime)
        {
          *AATime = ConvertTimestampToUnixSafe(ATime, SessionData->DSTMode);
        }
        if (AMTime)
        {
          *AMTime = ConvertTimestampToUnix(MTime, SessionData->DSTMode);
        }
      }

      if (ASize)
      {
        // Get file size
        FILE_OPERATION_LOOP_BEGIN
        {
          unsigned long LSize;
          unsigned long HSize;
          LSize = GetFileSize(Handle, &HSize);
          if ((LSize == 0xFFFFFFFF) && (GetLastError() != NO_ERROR)) RaiseLastOSError();
          *ASize = (__int64(HSize) << 32) + LSize;
        }
        FILE_OPERATION_LOOP_END(FMTLOAD(CANT_GET_ATTRS, (FileName)));
      }

      if ((AHandle == NULL) || NoHandle)
      {
        CloseHandle(Handle);
        Handle = NULL;
      }
    }
    catch(...)
    {
      CloseHandle(Handle);
      throw;
    }
  }

  if (AAttrs) *AAttrs = Attrs;
  if (AHandle) *AHandle = Handle;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::OpenLocalFile(
  const UnicodeString & FileName, unsigned int Access, TLocalFileHandle & Handle, bool TryWriteReadOnly)
{
  Handle.FileName = FileName;
  OpenLocalFile(
    FileName, Access, &Handle.Attrs, &Handle.Handle, NULL, &Handle.MTime, &Handle.ATime, &Handle.Size,
    TryWriteReadOnly);
  Handle.Modification = UnixToDateTime(Handle.MTime, SessionData->DSTMode);
  Handle.Directory = FLAGSET(Handle.Attrs, faDirectory);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoAllowLocalFileTransfer(
  const UnicodeString & FileName, const TSearchRecSmart & SearchRec, const TCopyParamType * CopyParam, bool DisallowTemporaryTransferFiles)
{
  TFileMasks::TParams Params;
  Params.Size = SearchRec.Size;
  Params.Modification = SearchRec.GetLastWriteTime();
  UnicodeString BaseFileName = GetBaseFileName(FileName);
  // Note that for synchronization, we do not need to check "TSynchronizeOptions::MatchesFilter" here as
  // that is checked for top-level entries only and we are never top-level here.
  return
    CopyParam->AllowTransfer(BaseFileName, osLocal, SearchRec.IsDirectory(), Params, SearchRec.IsHidden()) &&
    (!DisallowTemporaryTransferFiles || !FFileSystem->TemporaryTransferFile(FileName)) &&
    (!SearchRec.IsDirectory() || !CopyParam->ExcludeEmptyDirectories ||
       !IsEmptyLocalDirectory(FileName, CopyParam, DisallowTemporaryTransferFiles));
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoAllowRemoteFileTransfer(
  const TRemoteFile * File, const TCopyParamType * CopyParam, bool DisallowTemporaryTransferFiles)
{
  TFileMasks::TParams MaskParams;
  MaskParams.Size = File->Resolve()->Size;
  MaskParams.Modification = File->Modification;
  UnicodeString FullRemoteFileName = UnixExcludeTrailingBackslash(File->FullFileName);
  UnicodeString BaseFileName = GetBaseFileName(FullRemoteFileName);
  return
    CopyParam->AllowTransfer(BaseFileName, osRemote, File->IsDirectory, MaskParams, File->IsHidden) &&
    (!DisallowTemporaryTransferFiles || !FFileSystem->TemporaryTransferFile(File->FileName)) &&
    (!File->IsDirectory || !CopyParam->ExcludeEmptyDirectories ||
       !IsEmptyRemoteDirectory(File, CopyParam, DisallowTemporaryTransferFiles));
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::AllowLocalFileTransfer(
  const UnicodeString & FileName, const TSearchRecSmart * SearchRec,
  const TCopyParamType * CopyParam, TFileOperationProgressType * OperationProgress)
{
  bool Result = true;
  // optimization (though in most uses of the method, the caller actually knows TSearchRec already, so it passes it here
  if (Log->Logging || !CopyParam->AllowAnyTransfer())
  {
    TSearchRecSmart ASearchRec;
    if (SearchRec == NULL)
    {
      if (CopyParam->OnTransferIn != NULL)
      {
        ASearchRec.Clear();
      }
      else
      {
        FILE_OPERATION_LOOP_BEGIN
        {
          if (!FileSearchRec(FileName, ASearchRec))
          {
            RaiseLastOSError();
          }
        }
        FILE_OPERATION_LOOP_END(FMTLOAD(FILE_NOT_EXISTS, (FileName)));
      }
      SearchRec = &ASearchRec;
    }

    if (!DoAllowLocalFileTransfer(FileName, *SearchRec, CopyParam, false))
    {
      LogEvent(FORMAT(L"File \"%s\" excluded from transfer", (FileName)));
      Result = false;
    }
    else if (CopyParam->SkipTransfer(FileName, SearchRec->IsDirectory()))
    {
      OperationProgress->AddSkippedFileSize(SearchRec->Size);
      Result = false;
    }

    if (Result)
    {
      LogFileDetails(FileName, SearchRec->GetLastWriteTime(), SearchRec->Size);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::MakeLocalFileList(
  const UnicodeString & FileName, const TSearchRecSmart & Rec, void * Param)
{
  TMakeLocalFileListParams & Params = *static_cast<TMakeLocalFileListParams*>(Param);

  if (Rec.IsDirectory() && Params.Recursive)
  {
    ProcessLocalDirectory(FileName, MakeLocalFileList, &Params);
  }

  if (!Rec.IsDirectory() || Params.IncludeDirs)
  {
    Params.FileList->Add(FileName);
    if (Params.FileTimes != NULL)
    {
      Params.FileTimes->push_back(const_cast<TSearchRecSmart &>(Rec).TimeStamp);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CalculateLocalFileSize(
  const UnicodeString & FileName, const TSearchRecSmart & Rec, /*TCalculateSizeParams*/ void * Params)
{
  TCalculateSizeParams * AParams = static_cast<TCalculateSizeParams*>(Params);

  if (!TryStartOperationWithFile(FileName, foCalculateSize))
  {
    AParams->Result = false;
  }
  else
  {
    try
    {
      if ((AParams->CopyParam == NULL) ||
          DoAllowLocalFileTransfer(FileName, Rec, AParams->CopyParam, false))
      {
        int CollectionIndex = -1;
        if (AParams->Files != NULL)
        {
          UnicodeString FullFileName = ::ExpandFileName(FileName);
          CollectionIndex = AParams->Files->Add(FullFileName, NULL, Rec.IsDirectory());
        }

        if (!Rec.IsDirectory())
        {
          AParams->Size += Rec.Size;
        }
        else
        {
          try
          {
            ProcessLocalDirectory(FileName, CalculateLocalFileSize, Params);
          }
          catch (...)
          {
            if (CollectionIndex >= 0)
            {
              AParams->Files->DidNotRecurse(CollectionIndex);
            }
          }
        }
      }
    }
    catch (...)
    {
      // ignore
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CalculateLocalFilesSize(TStrings * FileList,
  __int64 & Size, const TCopyParamType * CopyParam, bool AllowDirs, TStrings * Files,
  TCalculatedSizes * CalculatedSizes)
{
  bool Result = false;
  TFileOperationProgressType OperationProgress(&DoProgress, &DoFinished);
  TOnceDoneOperation OnceDoneOperation = odoIdle;
  OperationStart(OperationProgress, foCalculateSize, osLocal, FileList->Count);
  try
  {
    TCalculateSizeParams Params;
    Params.Size = 0;
    Params.Params = 0;
    Params.CopyParam = CopyParam;
    Params.Files = NULL;
    Params.Result = true;

    UnicodeString LastDirPath;
    for (int Index = 0; Params.Result && (Index < FileList->Count); Index++)
    {
      UnicodeString FileName = FileList->Strings[Index];
      TSearchRecSmart Rec;
      if (FileSearchRec(FileName, Rec))
      {
        if (Rec.IsDirectory() && !AllowDirs)
        {
          Params.Result = false;
        }
        else
        {
          if (Files != NULL)
          {
            UnicodeString FullFileName = ::ExpandFileName(FileName);
            UnicodeString DirPath = ExtractFilePath(FullFileName);
            if (DirPath != LastDirPath)
            {
              Params.Files = new TCollectedFileList();
              LastDirPath = DirPath;
              Files->AddObject(LastDirPath, Params.Files);
            }
          }

          __int64 PrevSize = Params.Size;

          CalculateLocalFileSize(FileName, Rec, &Params);

          if (CalculatedSizes != NULL)
          {
            __int64 Size = Params.Size - PrevSize;
            CalculatedSizes->push_back(Size);
          }

          OperationFinish(&OperationProgress, FileList->Objects[Index], FileName, true, OnceDoneOperation);
        }
      }
    }

    Size = Params.Size;
    Result = Params.Result;
  }
  __finally
  {
    OperationStop(OperationProgress);
  }

  if (Configuration->ActualLogProtocol >= 1)
  {
    LogEvent(FORMAT(L"Size of %d local files/folders calculated as %s", (FileList->Count, IntToStr(Size))));
  }

  if (OnceDoneOperation != odoIdle)
  {
    CloseOnCompletion(OnceDoneOperation);
  }
  return Result;
}
//---------------------------------------------------------------------------
struct TSynchronizeFileData
{
  bool Modified;
  bool New;
  bool IsDirectory;
  TSynchronizeChecklist::TItem::TFileInfo Info;
  TSynchronizeChecklist::TItem::TFileInfo MatchingRemoteFile;
  TRemoteFile * MatchingRemoteFileFile;
  int MatchingRemoteFileImageIndex;
  FILETIME LocalLastWriteTime;
};
//---------------------------------------------------------------------------
const int sfFirstLevel = 0x01;
struct TSynchronizeData
{
  UnicodeString LocalDirectory;
  UnicodeString RemoteDirectory;
  TTerminal::TSynchronizeMode Mode;
  int Params;
  TSynchronizeDirectory OnSynchronizeDirectory;
  TSynchronizeOptions * Options;
  int Flags;
  TStringList * LocalFileList;
  const TCopyParamType * CopyParam;
  TSynchronizeChecklist * Checklist;
};
//---------------------------------------------------------------------------
TSynchronizeChecklist * __fastcall TTerminal::SynchronizeCollect(const UnicodeString LocalDirectory,
  const UnicodeString RemoteDirectory, TSynchronizeMode Mode,
  const TCopyParamType * CopyParam, int Params,
  TSynchronizeDirectory OnSynchronizeDirectory,
  TSynchronizeOptions * Options)
{
  TValueRestorer<bool> UseBusyCursorRestorer(FUseBusyCursor, false);

  TSynchronizeChecklist * Checklist = new TSynchronizeChecklist();
  try
  {
    DoSynchronizeCollectDirectory(LocalDirectory, RemoteDirectory, Mode,
      CopyParam, Params, OnSynchronizeDirectory, Options, sfFirstLevel,
      Checklist);
    Checklist->Sort();
  }
  catch(...)
  {
    delete Checklist;
    throw;
  }
  return Checklist;
}
//---------------------------------------------------------------------------
static void __fastcall AddFlagName(UnicodeString & ParamsStr, int & Params, int Param, const UnicodeString & Name)
{
  if (FLAGSET(Params, Param))
  {
    AddToList(ParamsStr, Name, ", ");
  }
  Params &= ~Param;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::SynchronizeModeStr(TSynchronizeMode Mode)
{
  UnicodeString ModeStr;
  switch (Mode)
  {
    case smRemote:
      ModeStr = L"Remote";
      break;
    case smLocal:
      ModeStr = L"Local";
      break;
    case smBoth:
      ModeStr = L"Both";
      break;
    default:
      ModeStr = L"Unknown";
      break;
  }
  return ModeStr;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::SynchronizeParamsStr(int Params)
{
  UnicodeString ParamsStr;
  AddFlagName(ParamsStr, Params, spDelete, L"Delete");
  AddFlagName(ParamsStr, Params, spNoConfirmation, L"NoConfirmation");
  AddFlagName(ParamsStr, Params, spExistingOnly, L"ExistingOnly");
  AddFlagName(ParamsStr, Params, spNoRecurse, L"NoRecurse");
  AddFlagName(ParamsStr, Params, spUseCache, L"UseCache");
  AddFlagName(ParamsStr, Params, spDelayProgress, L"DelayProgress");
  AddFlagName(ParamsStr, Params, spPreviewChanges, L"*PreviewChanges"); // GUI only
  AddFlagName(ParamsStr, Params, spSubDirs, L"SubDirs");
  AddFlagName(ParamsStr, Params, spTimestamp, L"Timestamp");
  AddFlagName(ParamsStr, Params, spNotByTime, L"NotByTime");
  AddFlagName(ParamsStr, Params, spBySize, L"BySize");
  AddFlagName(ParamsStr, Params, spByChecksum, L"ByChecksum");
  AddFlagName(ParamsStr, Params, spCaseSensitive, L"CaseSensitive");
  AddFlagName(ParamsStr, Params, spSelectedOnly, L"*SelectedOnly"); // GUI only
  AddFlagName(ParamsStr, Params, spMirror, L"Mirror");
  if (Params > 0)
  {
    AddToList(ParamsStr, FORMAT(L"0x%x", (int(Params))), L", ");
  }
  return ParamsStr;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::LocalFindFirstLoop(const UnicodeString & Path, TSearchRecChecked & SearchRec)
{
  bool Result;
  FILE_OPERATION_LOOP_BEGIN
  {
    const int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
    Result = (FindFirstChecked(Path, FindAttrs, SearchRec) == 0);
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(LIST_DIR_ERROR, (Path)));
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::LocalFindNextLoop(TSearchRecChecked & SearchRec)
{
  bool Result;
  FILE_OPERATION_LOOP_BEGIN
  {
    Result = (FindNextChecked(SearchRec) == 0);
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(LIST_DIR_ERROR, (SearchRec.Path)));
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::IsEmptyLocalDirectory(
  const UnicodeString & Path, const TCopyParamType * CopyParam, bool DisallowTemporaryTransferFiles)
{
  UnicodeString Contents;
  if (Configuration->ActualLogProtocol >= 1)
  {
    LogEvent(FORMAT(L"Checking if local directory \"%s\" is empty", (Path)));
  }

  TSearchRecOwned SearchRec;
  if (LocalFindFirstLoop(IncludeTrailingBackslash(Path) + L"*.*", SearchRec))
  {
    do
    {
      UnicodeString FullLocalFileName = SearchRec.GetFilePath();
      if (SearchRec.IsRealFile() &&
          DoAllowLocalFileTransfer(FullLocalFileName, SearchRec, CopyParam, true))
      {
        if (!SearchRec.IsDirectory() ||
            !IsEmptyLocalDirectory(FullLocalFileName, CopyParam, DisallowTemporaryTransferFiles))
        {
          Contents = SearchRec.Name;
        }
      }
    }
    while (Contents.IsEmpty() && LocalFindNextLoop(SearchRec));
  }

  if (Configuration->ActualLogProtocol >= 1)
  {
    if (Contents.IsEmpty())
    {
      LogEvent(FORMAT(L"Local directory \"%s\" is empty", (Path)));
    }
    else
    {
      LogEvent(FORMAT(L"Local directory \"%s\" is not empty, it contains \"%s\"", (Path, Contents)));
    }
  }

  return Contents.IsEmpty();
}
//---------------------------------------------------------------------------
void DestroyLocalFileList(TStringList * LocalFileList)
{
  // Factored out to workaround Clang ICE
  if (LocalFileList != NULL)
  {
    for (int Index = 0; Index < LocalFileList->Count; Index++)
    {
      TSynchronizeFileData * FileData = reinterpret_cast<TSynchronizeFileData*>(LocalFileList->Objects[Index]);
      delete FileData;
    }
    delete LocalFileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoSynchronizeCollectDirectory(const UnicodeString LocalDirectory,
  const UnicodeString RemoteDirectory, TSynchronizeMode Mode,
  const TCopyParamType * CopyParam, int Params,
  TSynchronizeDirectory OnSynchronizeDirectory, TSynchronizeOptions * Options,
  int Flags, TSynchronizeChecklist * Checklist)
{
  TSynchronizeData Data;

  Data.LocalDirectory = IncludeTrailingBackslash(LocalDirectory);
  Data.RemoteDirectory = UnixIncludeTrailingBackslash(RemoteDirectory);
  Data.Mode = Mode;
  Data.Params = Params;
  Data.OnSynchronizeDirectory = OnSynchronizeDirectory;
  Data.LocalFileList = NULL;
  Data.CopyParam = CopyParam;
  Data.Options = Options;
  Data.Flags = Flags;
  Data.Checklist = Checklist;

  LogEvent(FORMAT(L"Collecting synchronization list for local directory '%s' and remote directory '%s', "
    "mode = %s, params = 0x%x (%s), file mask = '%s'", (LocalDirectory, RemoteDirectory,
    SynchronizeModeStr(Mode), int(Params), SynchronizeParamsStr(Params), CopyParam->IncludeFileMask.Masks)));

  if (FLAGCLEAR(Params, spDelayProgress))
  {
    DoSynchronizeProgress(Data, true);
  }

  try
  {
    Data.LocalFileList = CreateSortedStringList(FLAGSET(Params, spCaseSensitive));

    TSearchRecOwned SearchRec;
    if (LocalFindFirstLoop(Data.LocalDirectory + L"*.*", SearchRec))
    {
      do
      {
        UnicodeString FileName = SearchRec.Name;
        UnicodeString FullLocalFileName = SearchRec.GetFilePath();
        UnicodeString RemoteFileName = ChangeFileName(CopyParam, FileName, osLocal, false);
        if (SearchRec.IsRealFile() &&
            DoAllowLocalFileTransfer(FullLocalFileName, SearchRec, CopyParam, true) &&
            (FLAGCLEAR(Flags, sfFirstLevel) ||
             (Options == NULL) ||
             Options->MatchesFilter(FileName) ||
             Options->MatchesFilter(RemoteFileName)))
        {
          TSynchronizeFileData * FileData = new TSynchronizeFileData;

          FileData->IsDirectory = SearchRec.IsDirectory();
          FileData->Info.FileName = FileName;
          FileData->Info.Directory = Data.LocalDirectory;
          FileData->Info.Modification = SearchRec.GetLastWriteTime();
          FileData->Info.ModificationFmt = mfFull;
          FileData->Info.Size = SearchRec.Size;
          FileData->LocalLastWriteTime = SearchRec.FindData.ftLastWriteTime;
          FileData->New = true;
          FileData->Modified = false;
          Data.LocalFileList->AddObject(FileName, reinterpret_cast<TObject*>(FileData));
          LogEvent(0, FORMAT(L"Local file %s included to synchronization",
            (FormatFileDetailsForLog(FullLocalFileName, SearchRec.GetLastWriteTime(), SearchRec.Size))));
        }
        else
        {
          LogEvent(0, FORMAT(L"Local file %s excluded from synchronization",
            (FormatFileDetailsForLog(FullLocalFileName, SearchRec.GetLastWriteTime(), SearchRec.Size))));
        }

      }
      while (LocalFindNextLoop(SearchRec));

      SearchRec.Close();

      // can we expect that ProcessDirectory would take so little time
      // that we can postpone showing progress window until anything actually happens?
      bool Cached = FLAGSET(Params, spUseCache) && SessionData->CacheDirectories &&
        FDirectoryCache->HasFileList(RemoteDirectory);

      if (!Cached && FLAGSET(Params, spDelayProgress))
      {
        DoSynchronizeProgress(Data, true);
      }

      ProcessDirectory(RemoteDirectory, SynchronizeCollectFile, &Data,
        FLAGSET(Params, spUseCache));

      TSynchronizeFileData * FileData;
      for (int Index = 0; Index < Data.LocalFileList->Count; Index++)
      {
        FileData = reinterpret_cast<TSynchronizeFileData *>
          (Data.LocalFileList->Objects[Index]);
        // add local file either if we are going to upload it
        // (i.e. if it is updated or we want to upload even new files)
        // or if we are going to delete it (i.e. all "new"=obsolete files)
        bool Modified = (FileData->Modified && ((Mode == smBoth) || (Mode == smRemote)));
        bool New = (FileData->New &&
          ((Mode == smLocal) ||
           (((Mode == smBoth) || (Mode == smRemote)) && FLAGCLEAR(Params, spTimestamp))));

        if (New)
        {
          LogEvent(FORMAT(L"Local file %s is new",
            (FormatFileDetailsForLog(FileData->Info.Directory + FileData->Info.FileName,
             FileData->Info.Modification, FileData->Info.Size))));
        }

        if (Modified || New)
        {
          TSynchronizeChecklist::TItem * ChecklistItem = new TSynchronizeChecklist::TItem();
          try
          {
            ChecklistItem->IsDirectory = FileData->IsDirectory;

            ChecklistItem->Local = FileData->Info;
            ChecklistItem->FLocalLastWriteTime = FileData->LocalLastWriteTime;

            if (Modified)
            {
              DebugAssert(!FileData->MatchingRemoteFile.Directory.IsEmpty());
              ChecklistItem->Remote = FileData->MatchingRemoteFile;
              ChecklistItem->ImageIndex = FileData->MatchingRemoteFileImageIndex;
              ChecklistItem->RemoteFile = FileData->MatchingRemoteFileFile;
            }
            else
            {
              ChecklistItem->Remote.Directory = Data.RemoteDirectory;
            }

            if ((Mode == smBoth) || (Mode == smRemote))
            {
              ChecklistItem->Action =
                (Modified ? TSynchronizeChecklist::saUploadUpdate : TSynchronizeChecklist::saUploadNew);
              ChecklistItem->Checked =
                (Modified || FLAGCLEAR(Params, spExistingOnly)) &&
                (!ChecklistItem->IsDirectory || FLAGCLEAR(Params, spNoRecurse) ||
                 FLAGSET(Params, spSubDirs));
            }
            else if ((Mode == smLocal) && FLAGCLEAR(Params, spTimestamp))
            {
              ChecklistItem->Action = TSynchronizeChecklist::saDeleteLocal;
              ChecklistItem->Checked =
                FLAGSET(Params, spDelete) &&
                (!ChecklistItem->IsDirectory || FLAGCLEAR(Params, spNoRecurse) ||
                 FLAGSET(Params, spSubDirs));
            }

            if (ChecklistItem->Action != TSynchronizeChecklist::saNone)
            {
              Data.Checklist->Add(ChecklistItem);
              ChecklistItem = NULL;
            }
          }
          __finally
          {
            delete ChecklistItem;
          }
        }
        else
        {
          if (FileData->Modified)
          {
            delete FileData->MatchingRemoteFileFile;
          }
        }
      }
    }
  }
  __finally
  {
    DestroyLocalFileList(Data.LocalFileList);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SynchronizeCollectFile(const UnicodeString FileName,
  const TRemoteFile * File, /*TSynchronizeData*/ void * Param)
{
  try
  {
    DoSynchronizeCollectFile(FileName, File, Param);
  }
  catch(ESkipFile & E)
  {
    TSuspendFileOperationProgress Suspend(OperationProgress);
    if (!HandleException(&E))
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::IsEmptyRemoteDirectory(
  const TRemoteFile * File, const TCopyParamType * ACopyParam, bool DisallowTemporaryTransferFiles)
{
  TCalculateSizeStats Stats;

  TCopyParamType CopyParam(*ACopyParam);
  CopyParam.ExcludeEmptyDirectories = false; // to avoid endless recursion

  TCalculateSizeParams Params;
  Params.Params = csStopOnFirstFile | csIgnoreErrors | FLAGMASK(DisallowTemporaryTransferFiles, csDisallowTemporaryTransferFiles);
  Params.CopyParam = &CopyParam;
  Params.Stats = &Stats;

  DoCalculateDirectorySize(UnixExcludeTrailingBackslash(File->FullFileName), &Params);

  return Params.Result && (Stats.Files == 0);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CollectCalculatedChecksum(
  const UnicodeString & DebugUsedArg(FileName), const UnicodeString & DebugUsedArg(Alg), const UnicodeString & Hash)
{
  DebugAssert(FCollectedCalculatedChecksum.IsEmpty());
  FCollectedCalculatedChecksum = Hash;
}
//---------------------------------------------------------------------------
bool TTerminal::SameFileChecksum(const UnicodeString & LocalFileName, const TRemoteFile * File)
{
  UnicodeString DefaultAlg = Sha256ChecksumAlg;
  UnicodeString Algs =
    DefaultStr(Configuration->SynchronizationChecksumAlgs, DefaultAlg + L"," + Sha1ChecksumAlg);
  std::unique_ptr<TStrings> SupportedAlgs(CreateSortedStringList());
  GetSupportedChecksumAlgs(SupportedAlgs.get());
  UnicodeString Alg;
  while (Alg.IsEmpty() && !Algs.IsEmpty())
  {
    UnicodeString A = CutToChar(Algs, L',', true);

    if (SupportedAlgs->IndexOf(A) >= 0)
    {
      Alg = A;
    }
  }

  if (Alg.IsEmpty())
  {
    Alg = DefaultAlg;
  }

  std::unique_ptr<TStrings> FileList(new TStringList());
  FileList->AddObject(File->FullFileName, const_cast<TRemoteFile *>(File));
  DebugAssert(FCollectedCalculatedChecksum.IsEmpty());
  FCollectedCalculatedChecksum = EmptyStr;
  CalculateFilesChecksum(Alg, FileList.get(), CollectCalculatedChecksum);
  UnicodeString RemoteChecksum = FCollectedCalculatedChecksum;
  FCollectedCalculatedChecksum = EmptyStr;

  UnicodeString LocalChecksum;
  FILE_OPERATION_LOOP_BEGIN
  {
    std::unique_ptr<THandleStream> Stream(TSafeHandleStream::CreateFromFile(LocalFileName, fmOpenRead | fmShareDenyWrite));
    LocalChecksum = CalculateFileChecksum(Stream.get(), Alg);
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(CHECKSUM_ERROR, (LocalFileName)));

  return SameText(RemoteChecksum, LocalChecksum);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoSynchronizeCollectFile(const UnicodeString FileName,
  const TRemoteFile * File, /*TSynchronizeData*/ void * Param)
{
  DebugUsedParam(FileName);
  TSynchronizeData * Data = static_cast<TSynchronizeData *>(Param);

  // Can be NULL in scripting
  if (Data->Options != NULL)
  {
    Data->Options->Files++;
  }

  UnicodeString LocalFileName = ChangeFileName(Data->CopyParam, File->FileName, osRemote, false);
  UnicodeString FullRemoteFileName = UnixExcludeTrailingBackslash(File->FullFileName);
  if (DoAllowRemoteFileTransfer(File, Data->CopyParam, true) &&
      (FLAGCLEAR(Data->Flags, sfFirstLevel) ||
       (Data->Options == NULL) ||
        Data->Options->MatchesFilter(File->FileName) ||
        Data->Options->MatchesFilter(LocalFileName)))
  {
    TSynchronizeChecklist::TItem * ChecklistItem = new TSynchronizeChecklist::TItem();
    try
    {
      ChecklistItem->IsDirectory = File->IsDirectory;
      ChecklistItem->ImageIndex = File->IconIndex;

      ChecklistItem->Remote.FileName = File->FileName;
      ChecklistItem->Remote.Directory = Data->RemoteDirectory;
      ChecklistItem->Remote.Modification = File->Modification;
      ChecklistItem->Remote.ModificationFmt = File->ModificationFmt;
      ChecklistItem->Remote.Size = File->Resolve()->Size;

      bool Modified = false;
      bool New = false;
      if (File->IsDirectory && !CanRecurseToDirectory(File))
      {
        LogEvent(FORMAT(L"Skipping symlink to directory \"%s\".", (File->FileName)));
      }
      else
      {
        int LocalIndex = Data->LocalFileList->IndexOf(LocalFileName);
        New = (LocalIndex < 0);
        if (!New)
        {
          TSynchronizeFileData * LocalData =
            reinterpret_cast<TSynchronizeFileData *>(Data->LocalFileList->Objects[LocalIndex]);

          LocalData->New = false;
          UnicodeString FullLocalFileName = LocalData->Info.Directory + LocalData->Info.FileName;

          if (File->IsDirectory != LocalData->IsDirectory)
          {
            LogEvent(FORMAT(L"%s is directory on one side, but file on the another",
              (File->FileName)));
          }
          else if (!File->IsDirectory)
          {
            ChecklistItem->Local = LocalData->Info;

            ChecklistItem->Local.Modification =
              ReduceDateTimePrecision(ChecklistItem->Local.Modification, File->ModificationFmt);

            bool LocalModified = false;
            // for spTimestamp+spBySize require that the file sizes are the same
            // before comparing file time
            int TimeCompare;
            if (FLAGCLEAR(Data->Params, spNotByTime) &&
                (FLAGCLEAR(Data->Params, spTimestamp) ||
                 FLAGCLEAR(Data->Params, spBySize) ||
                 (ChecklistItem->Local.Size == ChecklistItem->Remote.Size)))
            {
              TimeCompare = CompareFileTime(ChecklistItem->Local.Modification,
                   ChecklistItem->Remote.Modification);
            }
            else
            {
              TimeCompare = 0;
            }
            if (TimeCompare < 0)
            {
              if ((FLAGCLEAR(Data->Params, spTimestamp) && FLAGCLEAR(Data->Params, spMirror)) ||
                  (Data->Mode == smBoth) || (Data->Mode == smLocal))
              {
                Modified = true;
              }
              else
              {
                LocalModified = true;
              }
            }
            else if (TimeCompare > 0)
            {
              if ((FLAGCLEAR(Data->Params, spTimestamp) && FLAGCLEAR(Data->Params, spMirror)) ||
                  (Data->Mode == smBoth) || (Data->Mode == smRemote))
              {
                LocalModified = true;
              }
              else
              {
                Modified = true;
              }
            }
            else if (FLAGSET(Data->Params, spBySize) &&
                     (ChecklistItem->Local.Size != ChecklistItem->Remote.Size) &&
                     FLAGCLEAR(Data->Params, spTimestamp))
            {
              Modified = true;
              LocalModified = true;
            }
            else if (FLAGSET(Data->Params, spByChecksum) &&
                     FLAGCLEAR(Data->Params, spTimestamp) &&
                     !SameFileChecksum(FullLocalFileName, File) &&
                     FLAGCLEAR(Data->Params, spTimestamp))
            {
              Modified = true;
              LocalModified = true;
            }

            if (LocalModified)
            {
              LocalData->Modified = true;
              LocalData->MatchingRemoteFile = ChecklistItem->Remote;
              LocalData->MatchingRemoteFileImageIndex = ChecklistItem->ImageIndex;
              // we need this for custom commands over checklist only,
              // not for sync itself
              LocalData->MatchingRemoteFileFile = File->Duplicate();
              LogEvent(FORMAT(L"Local file %s is modified comparing to remote file %s",
                (FormatFileDetailsForLog(FullLocalFileName, LocalData->Info.Modification, LocalData->Info.Size),
                 FormatFileDetailsForLog(FullRemoteFileName, File->Modification, File->Size, File->LinkedFile))));
            }

            if (Modified)
            {
              LogEvent(FORMAT(L"Remote file %s is modified comparing to local file %s",
                (FormatFileDetailsForLog(FullRemoteFileName, File->Modification, File->Size, File->LinkedFile),
                 FormatFileDetailsForLog(FullLocalFileName, LocalData->Info.Modification, LocalData->Info.Size))));
            }
          }
          else if (FLAGCLEAR(Data->Params, spNoRecurse))
          {
            DoSynchronizeCollectDirectory(
              FullLocalFileName, FullRemoteFileName,
              Data->Mode, Data->CopyParam, Data->Params, Data->OnSynchronizeDirectory,
              Data->Options, (Data->Flags & ~sfFirstLevel),
              Data->Checklist);
          }
        }
        else
        {
          ChecklistItem->Local.Directory = Data->LocalDirectory;
          LogEvent(FORMAT(L"Remote file %s is new",
            (FormatFileDetailsForLog(FullRemoteFileName, File->Modification, File->Size, File->LinkedFile))));
        }
      }

      if (New || Modified)
      {
        DebugAssert(!New || !Modified);

        // download the file if it changed or is new and we want to have it locally
        if ((Data->Mode == smBoth) || (Data->Mode == smLocal))
        {
          if (FLAGCLEAR(Data->Params, spTimestamp) || Modified)
          {
            ChecklistItem->Action =
              (Modified ? TSynchronizeChecklist::saDownloadUpdate : TSynchronizeChecklist::saDownloadNew);
            ChecklistItem->Checked =
              (Modified || FLAGCLEAR(Data->Params, spExistingOnly)) &&
              (!ChecklistItem->IsDirectory || FLAGCLEAR(Data->Params, spNoRecurse) ||
               FLAGSET(Data->Params, spSubDirs));
          }
        }
        else if ((Data->Mode == smRemote) && New)
        {
          if (FLAGCLEAR(Data->Params, spTimestamp))
          {
            ChecklistItem->Action = TSynchronizeChecklist::saDeleteRemote;
            ChecklistItem->Checked =
              FLAGSET(Data->Params, spDelete) &&
              (!ChecklistItem->IsDirectory || FLAGCLEAR(Data->Params, spNoRecurse) ||
               FLAGSET(Data->Params, spSubDirs));
          }
        }

        if (ChecklistItem->Action != TSynchronizeChecklist::saNone)
        {
          ChecklistItem->RemoteFile = File->Duplicate();
          Data->Checklist->Add(ChecklistItem);
          ChecklistItem = NULL;
        }
      }
    }
    __finally
    {
      delete ChecklistItem;
    }
  }
  else
  {
    LogEvent(0, FORMAT(L"Remote file %s excluded from synchronization",
      (FormatFileDetailsForLog(FullRemoteFileName, File->Modification, File->Size, File->LinkedFile))));
  }
}
//---------------------------------------------------------------------------
TCopyParamType TTerminal::GetSynchronizeCopyParam(const TCopyParamType * CopyParam, int Params)
{
  TCopyParamType SyncCopyParam = *CopyParam;
  // when synchronizing by time, we force preserving time,
  // otherwise it does not make any sense
  if (FLAGCLEAR(Params, spNotByTime))
  {
    SyncCopyParam.PreserveTime = true;
  }
  return SyncCopyParam;
}
//---------------------------------------------------------------------------
int TTerminal::GetSynchronizeCopyParams(int Params)
{
  return
    // The spNoConfirmation seems to always be present
    FLAGMASK(DebugAlwaysTrue(FLAGSET(Params, spNoConfirmation)), cpNoConfirmation);
}
//---------------------------------------------------------------------------
TQueueItem * TTerminal::SynchronizeToQueue(
  const TSynchronizeChecklist::TItem * ChecklistItem, const TCopyParamType * CopyParam, int Params, bool Parallel)
{
  TQueueItem * Result;
  if (DebugAlwaysFalse(FLAGSET(Params, spTimestamp)))
  {
    NotImplemented();
  }
  else
  {
    std::unique_ptr<TStrings> FileList(ChecklistItem->GetFileList());
    switch (ChecklistItem->Action)
    {
      case TSynchronizeChecklist::saDownloadNew:
      case TSynchronizeChecklist::saDownloadUpdate:
        Result = new TDownloadQueueItem(this, FileList.get(), ChecklistItem->GetLocalTarget(), CopyParam, Params, Parallel);
        break;

      case TSynchronizeChecklist::saDeleteRemote:
        Result = new TRemoteDeleteQueueItem(this, FileList.get(), 0);
        break;

      case TSynchronizeChecklist::saUploadNew:
      case TSynchronizeChecklist::saUploadUpdate:
        Result = new TUploadQueueItem(this, FileList.get(), ChecklistItem->GetRemoteTarget(), CopyParam, Params, Parallel);
        break;

      case TSynchronizeChecklist::saDeleteLocal:
        Result = new TLocalDeleteQueueItem(FileList.get(), 0);
        break;

      default:
        DebugFail();
        NotImplemented();
        UNREACHABLE_AFTER_NORETURN(break);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SynchronizeApply(
  TSynchronizeChecklist * Checklist,
  const TCopyParamType * CopyParam, int Params,
  TSynchronizeDirectory OnSynchronizeDirectory, TProcessedSynchronizationChecklistItem OnProcessedItem,
  TUpdatedSynchronizationChecklistItems OnUpdatedSynchronizationChecklistItems, void * Token,
  TFileOperationStatistics * Statistics)
{
  TSynchronizeData Data;

  Data.OnSynchronizeDirectory = OnSynchronizeDirectory;

  int CopyParams = GetSynchronizeCopyParams(Params);
  TCopyParamType SyncCopyParam = GetSynchronizeCopyParam(CopyParam, Params);

  if (SyncCopyParam.CalculateSize)
  {
    // If we fail to collect the sizes, do not try again during an actual transfer
    SyncCopyParam.CalculateSize = false;

    TSynchronizeChecklist::TItemList Items;
    int Index = 0;
    const TSynchronizeChecklist::TItem * ChecklistItem;
    while (Checklist->GetNextChecked(Index, ChecklistItem))
    {
      // TSynchronizeChecklistDialog relies on us not to update a size of an item that had size already
      // See TSynchronizeChecklistDialog::UpdatedSynchronizationChecklistItems
      if (!TSynchronizeChecklist::IsItemSizeIrrelevant(ChecklistItem->Action) &&
          !ChecklistItem->HasSize() && DebugAlwaysTrue(ChecklistItem->IsDirectory))
      {
        Items.push_back(ChecklistItem);
      }
    }

    SynchronizeChecklistCalculateSize(Checklist, Items, &SyncCopyParam);
    if (OnUpdatedSynchronizationChecklistItems != NULL)
    {
      OnUpdatedSynchronizationChecklistItems(Items);
    }
  }

  BeginTransaction();
  TValueRestorer<TFileOperationProgressType::TPersistence *> OperationProgressPersistenceRestorer(FOperationProgressPersistence);
  TValueRestorer<TOnceDoneOperation> OperationProgressOnceDoneOperationRestorer(FOperationProgressOnceDoneOperation);
  TFileOperationProgressType::TPersistence OperationProgressPersistence;
  OperationProgressPersistence.Statistics = Statistics;
  FOperationProgressPersistence = &OperationProgressPersistence;

  try
  {
    int Index = 0;
    const TSynchronizeChecklist::TItem * ChecklistItem;
    while (Checklist->GetNextChecked(Index, ChecklistItem))
    {
      UnicodeString LocalTarget = ChecklistItem->GetLocalTarget();
      UnicodeString RemoteTarget = ChecklistItem->GetRemoteTarget();
      if (!SamePaths(Data.LocalDirectory, LocalTarget) ||
          !UnixSamePath(Data.RemoteDirectory, RemoteTarget))
      {
        Data.LocalDirectory = LocalTarget;
        Data.RemoteDirectory = RemoteTarget;

        LogEvent(
          FORMAT(L"Synchronizing local directory '%s' with remote directory '%s', params = 0x%x (%s)",
          (Data.LocalDirectory, Data.RemoteDirectory, int(Params), SynchronizeParamsStr(Params))));

        DoSynchronizeProgress(Data, false);
      }

      bool Result = true;

      if (FLAGSET(Params, spTimestamp))
      {
        // used by SynchronizeLocalTimestamp and SynchronizeRemoteTimestamp
        TObject * ChecklistItemToken = const_cast<TObject *>(reinterpret_cast<const TObject *>(ChecklistItem));
        std::unique_ptr<TStringList> FileList(new TStringList());
        switch (ChecklistItem->Action)
        {
          case TSynchronizeChecklist::saDownloadUpdate:
            FileList->AddObject(ChecklistItem->GetRemotePath(), ChecklistItemToken);
            ProcessFiles(FileList.get(), foSetProperties, SynchronizeLocalTimestamp, NULL, osLocal);
            break;

          case TSynchronizeChecklist::saUploadUpdate:
            FileList->AddObject(ChecklistItem->GetLocalPath(), ChecklistItemToken);
            ProcessFiles(FileList.get(), foSetProperties, SynchronizeRemoteTimestamp);
            break;

          default:
            DebugFail();
            Result = false;
            break;
        }
      }
      else
      {
        std::unique_ptr<TStrings> FileList(ChecklistItem->GetFileList());
        TCopyParamType ItemCopyParam = SyncCopyParam;
        ItemCopyParam.Size = ChecklistItem->HasSize() ? ChecklistItem->GetSize() : -1;
        switch (ChecklistItem->Action)
        {
          case TSynchronizeChecklist::saDownloadNew:
          case TSynchronizeChecklist::saDownloadUpdate:
            Result = CopyToLocal(FileList.get(), LocalTarget, &ItemCopyParam, CopyParams, NULL);
            break;

          case TSynchronizeChecklist::saDeleteRemote:
            Result = DeleteFiles(FileList.get());
            break;

          case TSynchronizeChecklist::saUploadNew:
          case TSynchronizeChecklist::saUploadUpdate:
            Result = CopyToRemote(FileList.get(), RemoteTarget, &ItemCopyParam, CopyParams, NULL);
            break;

          case TSynchronizeChecklist::saDeleteLocal:
            Result = DeleteLocalFiles(FileList.get());
            break;

          default:
            DebugFail();
            Result = false;
            break;
        }
      }

      if (!Result)
      {
        Abort();
      }

      if (OnProcessedItem != NULL)
      {
        OnProcessedItem(Token, ChecklistItem);
      }
    }
  }
  __finally
  {
    EndTransaction();
  }

  if (FOperationProgressOnceDoneOperation != odoIdle)
  {
    // Otherwise CloseOnCompletion would have no effect
    OperationProgressPersistenceRestorer.Release();
    CloseOnCompletion(FOperationProgressOnceDoneOperation);
  }

}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SynchronizeChecklistCalculateSize(
  TSynchronizeChecklist * Checklist, const TSynchronizeChecklist::TItemList & Items,
  const TCopyParamType * CopyParam)
{
  std::unique_ptr<TStrings> RemoteFileList(new TStringList());
  std::unique_ptr<TStrings> LocalFileList(new TStringList());

  for (size_t Index = 0; Index < Items.size(); Index++)
  {
    const TSynchronizeChecklist::TItem * ChecklistItem = Items[Index];
    if (ChecklistItem->IsDirectory)
    {
      if (ChecklistItem->IsRemoteOnly())
      {
        DebugAssert(UnixSamePath(ChecklistItem->RemoteFile->FullFileName, ChecklistItem->GetRemotePath()));
        RemoteFileList->AddObject(ChecklistItem->GetRemotePath(), ChecklistItem->RemoteFile);
      }
      else if (ChecklistItem->IsLocalOnly())
      {
        LocalFileList->Add(ChecklistItem->GetLocalPath());
      }
      else
      {
        // "update" actions are not relevant for directories
        DebugFail();
      }
    }
  }

  TCalculatedSizes RemoteCalculatedSizes;
  TCalculatedSizes LocalCalculatedSizes;

  try
  {
    bool Result = true;
    if (LocalFileList->Count > 0)
    {
      __int64 LocalSize = 0;
      Result = CalculateLocalFilesSize(LocalFileList.get(), LocalSize, CopyParam, true, NULL, &LocalCalculatedSizes);
    }
    if (Result && (RemoteFileList->Count > 0))
    {
      __int64 RemoteSize = 0;
      TCalculateSizeStats RemoteStats;
      RemoteStats.CalculatedSizes = &RemoteCalculatedSizes;
      TCalculateSizeParams Params;
      Params.CopyParam = CopyParam;
      Params.Stats = &RemoteStats;
      CalculateFilesSize(RemoteFileList.get(), RemoteSize, Params);
    }
  }
  __finally
  {
    size_t LocalIndex = 0;
    size_t RemoteIndex = 0;

    for (size_t Index = 0; Index < Items.size(); Index++)
    {
      const TSynchronizeChecklist::TItem * ChecklistItem = Items[Index];
      if (ChecklistItem->IsDirectory)
      {
        __int64 Size = -1;
        if (ChecklistItem->IsRemoteOnly())
        {
          if (RemoteIndex < RemoteCalculatedSizes.size())
          {
            Size = RemoteCalculatedSizes[RemoteIndex];
          }
          RemoteIndex++;
        }
        else if (ChecklistItem->IsLocalOnly())
        {
          if (LocalIndex < LocalCalculatedSizes.size())
          {
            Size = LocalCalculatedSizes[LocalIndex];
          }
          LocalIndex++;
        }
        else
        {
          // "update" actions are not relevant for directories
          DebugFail();
        }

        if (Size >= 0)
        {
          Checklist->UpdateDirectorySize(ChecklistItem, Size);
        }
      }
    }

    DebugAssert(RemoteIndex >= RemoteCalculatedSizes.size());
    DebugAssert(LocalIndex >= LocalCalculatedSizes.size());
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoSynchronizeProgress(const TSynchronizeData & Data,
  bool Collect)
{
  if (Data.OnSynchronizeDirectory != NULL)
  {
    bool Continue = true;
    Data.OnSynchronizeDirectory(
      Data.LocalDirectory, Data.RemoteDirectory, Continue, Collect, Data.Options);

    if (!Continue)
    {
      Abort();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SynchronizeLocalTimestamp(const UnicodeString /*FileName*/,
  const TRemoteFile * File, void * /*Param*/)
{
  const TSynchronizeChecklist::TItem * ChecklistItem =
    reinterpret_cast<const TSynchronizeChecklist::TItem *>(File);

  UnicodeString LocalFile = ChecklistItem->GetLocalPath();

  FILE_OPERATION_LOOP_BEGIN
  {
    HANDLE Handle;
    OpenLocalFile(LocalFile, GENERIC_WRITE, NULL, &Handle,
      NULL, NULL, NULL, NULL);
    FILETIME WrTime = DateTimeToFileTime(ChecklistItem->Remote.Modification,
      SessionData->DSTMode);
    bool Result = SetFileTime(Handle, NULL, NULL, &WrTime);
    int Error = GetLastError();
    CloseHandle(Handle);
    if (!Result)
    {
      RaiseLastOSError(Error);
    }
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(CANT_SET_ATTRS, (LocalFile)));
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SynchronizeRemoteTimestamp(const UnicodeString /*FileName*/,
  const TRemoteFile * File, void * /*Param*/)
{
  const TSynchronizeChecklist::TItem * ChecklistItem =
    reinterpret_cast<const TSynchronizeChecklist::TItem *>(File);

  TRemoteProperties Properties;
  Properties.Valid << vpModification;
  Properties.Modification = ConvertTimestampToUnix(ChecklistItem->FLocalLastWriteTime,
    SessionData->DSTMode);

  ChangeFileProperties(ChecklistItem->GetRemotePath(), NULL, &Properties);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FileFind(UnicodeString FileName,
  const TRemoteFile * File, /*TFilesFindParams*/ void * Param)
{
  // see DoFilesFind
  FOnFindingFile = NULL;

  DebugAssert(Param);
  DebugAssert(File);
  TFilesFindParams * AParams = static_cast<TFilesFindParams*>(Param);

  if (!AParams->Cancel)
  {
    if (FileName.IsEmpty())
    {
      FileName = File->FileName;
    }

    TFileMasks::TParams MaskParams;
    MaskParams.Size = File->Resolve()->Size;
    MaskParams.Modification = File->Modification;

    UnicodeString FullFileName = UnixExcludeTrailingBackslash(File->FullFileName);
    bool ImplicitMatch;
    // Do not use recursive include match
    if (AParams->FileMask.Matches(FullFileName, false,
         File->IsDirectory, &MaskParams, false, ImplicitMatch))
    {
      if (!ImplicitMatch)
      {
        AParams->OnFileFound(this, FileName, File, AParams->Cancel);
      }

      if (File->IsDirectory)
      {
        UnicodeString RealDirectory;
        if (!File->IsSymLink || File->LinkTo.IsEmpty())
        {
          RealDirectory = UnixIncludeTrailingBackslash(AParams->RealDirectory) + File->FileName;
        }
        else
        {
          RealDirectory = ::AbsolutePath(AParams->RealDirectory, File->LinkTo);
        }

        if (!AParams->LoopDetector.IsUnvisitedDirectory(RealDirectory))
        {
          LogEvent(FORMAT(L"Already searched \"%s\" directory (real path \"%s\"), link loop detected", (FullFileName, RealDirectory)));
        }
        else
        {
          DoFilesFind(FullFileName, *AParams, RealDirectory);
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoFilesFind(UnicodeString Directory, TFilesFindParams & Params, UnicodeString RealDirectory)
{
  LogEvent(FORMAT(L"Searching directory \"%s\" (real path \"%s\")", (Directory, RealDirectory)));
  Params.OnFindingFile(this, Directory, Params.Cancel);
  if (!Params.Cancel)
  {
    DebugAssert(FOnFindingFile == NULL);
    // ideally we should set the handler only around actually reading
    // of the directory listing, so we at least reset the handler in
    // FileFind
    FOnFindingFile = Params.OnFindingFile;
    UnicodeString PrevRealDirectory = Params.RealDirectory;
    try
    {
      Params.RealDirectory = RealDirectory;
      ProcessDirectory(Directory, FileFind, &Params, false, true);
    }
    __finally
    {
      Params.RealDirectory = PrevRealDirectory;
      FOnFindingFile = NULL;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FilesFind(UnicodeString Directory, const TFileMasks & FileMask,
  TFileFoundEvent OnFileFound, TFindingFileEvent OnFindingFile)
{
  TFilesFindParams Params;
  Params.FileMask = FileMask;
  Params.FileMask.NoImplicitMatchWithDirExcludeMask = true;
  Params.FileMask.AllDirsAreImplicitlyIncluded = true;
  Params.OnFileFound = OnFileFound;
  Params.OnFindingFile = OnFindingFile;
  Params.Cancel = false;

  Params.LoopDetector.RecordVisitedDirectory(Directory);

  DoFilesFind(Directory, Params, Directory);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SpaceAvailable(const UnicodeString Path,
  TSpaceAvailable & ASpaceAvailable)
{
  DebugAssert(IsCapable[fcCheckingSpaceAvailable]);

  try
  {
    FFileSystem->SpaceAvailable(Path, ASpaceAvailable);
  }
  catch (Exception &E)
  {
    CommandError(&E, FMTLOAD(SPACE_AVAILABLE_ERROR, (Path)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LockFile(const UnicodeString FileName,
  const TRemoteFile * File, void * /*Param*/)
{
  StartOperationWithFile(FileName, foLock);

  LogEvent(FORMAT(L"Locking file \"%s\".", (FileName)));
  FileModified(File, FileName, true);

  DoLockFile(FileName, File);
  ReactOnCommand(fsLock);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoLockFile(const UnicodeString & FileName, const TRemoteFile * File)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    try
    {
      FFileSystem->LockFile(FileName, File);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, FMTLOAD(LOCK_FILE_ERROR, (FileName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::UnlockFile(const UnicodeString FileName,
  const TRemoteFile * File, void * /*Param*/)
{
  StartOperationWithFile(FileName, foUnlock);

  LogEvent(FORMAT(L"Unlocking file \"%s\".", (FileName)));
  FileModified(File, FileName, true);

  DoUnlockFile(FileName, File);
  ReactOnCommand(fsLock);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoUnlockFile(const UnicodeString & FileName, const TRemoteFile * File)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    try
    {
      FFileSystem->UnlockFile(FileName, File);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, FMTLOAD(UNLOCK_FILE_ERROR, (FileName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LockFiles(TStrings * FileList)
{
  BeginTransaction();
  try
  {
    ProcessFiles(FileList, foLock, LockFile, NULL);
  }
  __finally
  {
    EndTransaction();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::UnlockFiles(TStrings * FileList)
{
  BeginTransaction();
  try
  {
    ProcessFiles(FileList, foUnlock, UnlockFile, NULL);
  }
  __finally
  {
    EndTransaction();
  }
}
//---------------------------------------------------------------------------
const TSessionInfo & __fastcall TTerminal::GetSessionInfo()
{
  return FFileSystem->GetSessionInfo();
}
//---------------------------------------------------------------------------
const TFileSystemInfo & __fastcall TTerminal::GetFileSystemInfo(bool Retrieve)
{
  return FFileSystem->GetFileSystemInfo(Retrieve);
}
//---------------------------------------------------------------------
TStrings * TTerminal::GetShellChecksumAlgDefs()
{
  if (FShellChecksumAlgDefs.get() == NULL)
  {
    UnicodeString ChecksumCommandsDef = Configuration->ChecksumCommands;
    if (ChecksumCommandsDef.IsEmpty())
    {
      UnicodeString Delimiter(L",");
      AddToList(ChecksumCommandsDef, Sha512ChecksumAlg + L"=sha512sum", Delimiter);
      AddToList(ChecksumCommandsDef, Sha384ChecksumAlg + L"=sha384sum", Delimiter);
      AddToList(ChecksumCommandsDef, Sha256ChecksumAlg + L"=sha256sum", Delimiter);
      AddToList(ChecksumCommandsDef, Sha224ChecksumAlg + L"=sha224sum", Delimiter);
      AddToList(ChecksumCommandsDef, Sha1ChecksumAlg + L"=sha1sum", Delimiter);
      AddToList(ChecksumCommandsDef, Md5ChecksumAlg + L"=md5sum", Delimiter);
    }

    FShellChecksumAlgDefs.reset(CommaTextToStringList(ChecksumCommandsDef));
  }
  return FShellChecksumAlgDefs.get();
}
//---------------------------------------------------------------------
void TTerminal::GetShellChecksumAlgs(TStrings * Algs)
{
  TStrings * AlgDefs = GetShellChecksumAlgDefs();
  for (int Index = 0; Index < AlgDefs->Count; Index++)
  {
    Algs->Add(AlgDefs->Names[Index]);
  }
}
//---------------------------------------------------------------------
void __fastcall TTerminal::GetSupportedChecksumAlgs(TStrings * Algs)
{
  if (!IsCapable[fcCalculatingChecksum] && IsCapable[fcSecondaryShell])
  {
    // Both return the same anyway
    if (CommandSessionOpened)
    {
      FCommandSession->GetSupportedChecksumAlgs(Algs);
    }
    else
    {
      GetShellChecksumAlgs(Algs);
    }
  }
  else
  {
    FFileSystem->GetSupportedChecksumAlgs(Algs);
  }
}
//---------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetPassword()
{
  UnicodeString Result;
  // FRememberedPassword is empty also when stored password was used
  if (FRememberedPassword.IsEmpty())
  {
    Result = SessionData->Password;
  }
  else if (FRememberedPasswordKind != pkPassphrase)
  {
    Result = GetRememberedPassword();
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetRememberedPassword()
{
  return DecryptPassword(FRememberedPassword);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetRememberedTunnelPassword()
{
  return DecryptPassword(FRememberedTunnelPassword);
}
//---------------------------------------------------------------------
bool __fastcall TTerminal::GetStoredCredentialsTried()
{
  bool Result;
  if (FFileSystem != NULL)
  {
    Result = FFileSystem->GetStoredCredentialsTried();
  }
  else if (FSecureShell != NULL)
  {
    Result = FSecureShell->GetStoredCredentialsTried();
  }
  else
  {
    DebugAssert(FTunnelOpening);
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TTerminal::CopyToParallel(TParallelOperation * ParallelOperation, TFileOperationProgressType * OperationProgress)
{
  UnicodeString FileName;
  TObject * Object;
  UnicodeString TargetDir;
  bool Dir;
  bool Recursed;

  TCopyParamType * CustomCopyParam = NULL;
  int Result = ParallelOperation->GetNext(this, FileName, Object, TargetDir, Dir, Recursed, CustomCopyParam);
  std::unique_ptr<TCopyParamType> CustomCopyParamOwner(CustomCopyParam);

  if (Result > 0)
  {
    std::unique_ptr<TStrings> FilesToCopy(new TStringList());
    FilesToCopy->AddObject(FileName, Object);

    // OnceDoneOperation is not supported
    TOnceDoneOperation OnceDoneOperation = odoIdle;

    int Params = ParallelOperation->Params;
    // If we failed to recurse the directory when enumerating, recurse now (typically only to show the recursion error)
    if (Dir && Recursed)
    {
      Params = Params | cpNoRecurse;
    }

    int Prev = OperationProgress->FilesFinishedSuccessfully;
    DebugAssert((FOperationProgress == OperationProgress) || (FOperationProgress == NULL));
    TFileOperationProgressType * PrevOperationProgress = FOperationProgress;
    const TCopyParamType * CopyParam = (CustomCopyParam != NULL) ? CustomCopyParam : ParallelOperation->CopyParam;
    try
    {
      FOperationProgress = OperationProgress;
      if (ParallelOperation->Side == osLocal)
      {
        FFileSystem->CopyToRemote(
          FilesToCopy.get(), TargetDir, CopyParam, Params, OperationProgress, OnceDoneOperation);
      }
      else if (DebugAlwaysTrue(ParallelOperation->Side == osRemote))
      {
        FFileSystem->CopyToLocal(
          FilesToCopy.get(), TargetDir, CopyParam, Params, OperationProgress, OnceDoneOperation);
      }
    }
    __finally
    {
      bool Success = (Prev < OperationProgress->FilesFinishedSuccessfully);
      ParallelOperation->Done(FileName, Dir, Success, TargetDir, CopyParam, this);
      // Not to fail an assertion in OperationStop when called from CopyToRemote or CopyToLocal,
      // when FOperationProgress is already OperationProgress.
      FOperationProgress = PrevOperationProgress;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CanParallel(
  const TCopyParamType * CopyParam, int Params, TParallelOperation * ParallelOperation)
{
  return
    (ParallelOperation != NULL) &&
    FFileSystem->IsCapable(fcParallelTransfers) &&
    // parallel transfer is not implemented for operations needed to be done on a folder
    // after all its files are processed
    FLAGCLEAR(Params, cpDelete) &&
    (!CopyParam->PreserveTime || !CopyParam->PreserveTimeDirs);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CopyParallel(TParallelOperation * ParallelOperation, TFileOperationProgressType * OperationProgress)
{
  try
  {
    bool Continue = true;
    do
    {
      int GotNext = CopyToParallel(ParallelOperation, OperationProgress);
      if (GotNext < 0)
      {
        Continue = false;
      }
      else if (GotNext == 0)
      {
        Sleep(100);
      }
    }
    while (Continue && !OperationProgress->Cancel);
  }
  __finally
  {
    OperationProgress->SetDone();
    ParallelOperation->WaitFor();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LogParallelTransfer(TParallelOperation * ParallelOperation)
{
  LogEvent(
    FORMAT("Adding a parallel transfer to the transfer started on the connection \"%s\"",
    (ParallelOperation->MainName)));
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LogTotalTransferDetails(
  const UnicodeString TargetDir, const TCopyParamType * CopyParam,
  TFileOperationProgressType * OperationProgress, bool Parallel, TStrings * Files)
{
  if (Log->Logging)
  {
    UnicodeString TargetSide = ((OperationProgress->Side == osLocal) ? L"remote" : L"local");
    UnicodeString S =
      FORMAT(
        L"Copying %d files/directories to %s directory \"%s\"",
        (OperationProgress->Count, TargetSide, TargetDir));
    if (Parallel && DebugAlwaysTrue(Files != NULL))
    {
      int Count = 0;
      for (int Index = 0; Index < Files->Count; Index++)
      {
        TCollectedFileList * FileList = dynamic_cast<TCollectedFileList *>(Files->Objects[Index]);
        Count += FileList->Count();
      }
      S += FORMAT(L" - in parallel, with %d total files", (Count));
    }
    if (OperationProgress->TotalSizeSet)
    {
      S += FORMAT(L" - total size: %s", (FormatSize(OperationProgress->TotalSize)));
    }
    LogEvent(S);
    LogEvent(0, CopyParam->LogStr);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LogTotalTransferDone(TFileOperationProgressType * OperationProgress)
{
  LogEvent(L"Copying finished: " + OperationProgress->GetLogStr(true));
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CopyToRemote(
  TStrings * FilesToCopy, const UnicodeString & TargetDir, const TCopyParamType * CopyParam, int Params,
  TParallelOperation * ParallelOperation)
{
  DebugAssert(FFileSystem);
  DebugAssert(FilesToCopy);


  bool Result = false;
  TOnceDoneOperation OnceDoneOperation = odoIdle;

  if ((CopyParam->OnTransferIn != NULL) && !FFileSystem->IsCapable(fcTransferIn))
  {
    NotSupported();
  }

  try
  {
    __int64 Size;
    std::unique_ptr<TStringList> Files;
    bool ACanParallel =
      CanParallel(CopyParam, Params, ParallelOperation) &&
      !CopyParam->ClearArchive;
    if (ACanParallel)
    {
      Files.reset(new TStringList());
      Files->OwnsObjects = true;
    }
    bool CalculatedSize;
    if ((CopyParam->Size >= 0) &&
        DebugAlwaysTrue(!ACanParallel)) // Size is set for sync only and we never use parallel transfer for sync
    {
      Size = CopyParam->Size;
      CalculatedSize = true;
    }
    else
    {
      bool CalculateSize = ACanParallel || CopyParam->CalculateSize;
      CalculatedSize = CalculateLocalFilesSize(FilesToCopy, Size, CopyParam, CalculateSize, Files.get(), NULL);
    }

    FLastProgressLogged = GetTickCount();
    TFileOperationProgressType OperationProgress(&DoProgress, &DoFinished);
    OperationStart(
      OperationProgress, (Params & cpDelete ? foMove : foCopy), osLocal,
      FilesToCopy->Count, Params & cpTemporary, TargetDir, CopyParam->CPSLimit, CopyParam->OnceDoneOperation);

    bool CollectingUsage = false;
    try
    {
      if (CalculatedSize)
      {
        if (Configuration->Usage->Collect)
        {
          Configuration->Usage->Inc(L"Uploads");
          int CounterSize = TUsage::CalculateCounterSize(Size);
          Configuration->Usage->IncAndSetMax(L"UploadedBytes", L"MaxUploadSize", CounterSize);
          CollectingUsage = true;
        }

        OperationProgress.SetTotalSize(Size);
      }

      BeginTransaction();
      try
      {
        bool Parallel = ACanParallel && CalculatedSize;
        LogTotalTransferDetails(TargetDir, CopyParam, &OperationProgress, Parallel, Files.get());

        if (Parallel)
        {
          // OnceDoneOperation is not supported
          ParallelOperation->Init(Files.release(), TargetDir, CopyParam, Params, &OperationProgress, Log->Name, -1);
          CopyParallel(ParallelOperation, &OperationProgress);
        }
        else
        {
          FFileSystem->CopyToRemote(
            FilesToCopy, TargetDir, CopyParam, Params, &OperationProgress, OnceDoneOperation);
        }

        LogTotalTransferDone(&OperationProgress);
      }
      __finally
      {
        if (Active)
        {
          ReactOnCommand(fsCopyToRemote);
        }
        EndTransaction();
      }

      if (OperationProgress.Cancel == csContinue)
      {
        Result = true;
      }
    }
    __finally
    {
      if (CollectingUsage)
      {
        Configuration->Usage->IncAndSetMax(L"UploadTime", L"MaxUploadTime", TimeToSeconds(OperationProgress.TimeElapsed()));
      }
      OperationStop(OperationProgress);
    }
  }
  catch (Exception &E)
  {
    CommandError(&E, MainInstructions(LoadStr(TOREMOTE_COPY_ERROR)));
    OnceDoneOperation = odoIdle;
  }

  if (OnceDoneOperation != odoIdle)
  {
    CloseOnCompletion(OnceDoneOperation);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCopyToRemote(
  TStrings * FilesToCopy, const UnicodeString & ATargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags, TOnceDoneOperation & OnceDoneOperation)
{
  DebugAssert((FilesToCopy != NULL) && (OperationProgress != NULL));

  FFileSystem->TransferOnDirectory(ATargetDir, CopyParam, Params);

  // Must be local resolving, as this is outside of robust loop
  UnicodeString TargetDir = AbsolutePath(ATargetDir, true);
  UnicodeString FullTargetDir = UnixIncludeTrailingBackslash(TargetDir);
  int Index = 0;
  while ((Index < FilesToCopy->Count) && !OperationProgress->Cancel)
  {
    bool Success = false;
    UnicodeString FileName = FilesToCopy->Strings[Index];
    TSearchRecSmart * SearchRec = NULL;
    if (FilesToCopy->Objects[Index] != NULL)
    {
      TLocalFile * LocalFile = dynamic_cast<TLocalFile *>(FilesToCopy->Objects[Index]);
      SearchRec = &LocalFile->SearchRec;
    }

    try
    {
      try
      {
        if (SessionData->CacheDirectories)
        {
          DirectoryModified(TargetDir, false);

          if (::DirectoryExists(ApiPath(FileName)))
          {
            UnicodeString FileNameOnly = ExtractFileName(FileName);
            DirectoryModified(FullTargetDir + FileNameOnly, true);
          }
        }
        SourceRobust(FileName, SearchRec, FullTargetDir, CopyParam, Params, OperationProgress, Flags | tfFirstLevel);
        Success = true;
      }
      catch (ESkipFile & E)
      {
        TSuspendFileOperationProgress Suspend(OperationProgress);
        if (!HandleException(&E))
        {
          throw;
        }
      }
    }
    __finally
    {
      OperationFinish(OperationProgress, FilesToCopy->Objects[Index], FileName, Success, OnceDoneOperation);
    }
    Index++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SourceRobust(
  const UnicodeString & FileName, const TSearchRecSmart * SearchRec,
  const UnicodeString & TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  TUploadSessionAction Action(ActionLog);
  bool * AFileTransferAny = FLAGSET(Flags, tfUseFileTransferAny) ? &FFileTransferAny : NULL;
  bool CanRetry = (CopyParam->OnTransferIn == NULL);
  TRobustOperationLoop RobustLoop(this, OperationProgress, AFileTransferAny, CanRetry);

  do
  {
    bool ChildError = false;
    try
    {
      Source(FileName, SearchRec, TargetDir, CopyParam, Params, OperationProgress, Flags, Action, ChildError);
    }
    catch (Exception & E)
    {
      if (!RobustLoop.TryReopen(E))
      {
        if (!ChildError)
        {
          RollbackAction(Action, OperationProgress, &E);
        }
        throw;
      }
    }

    if (RobustLoop.ShouldRetry())
    {
      OperationProgress->RollbackTransfer();
      Action.Restart();
      // prevent overwrite and resume confirmations (should not be set for directories!)
      Params |= cpNoConfirmation;
      // enable resume even if we are uploading into new directory
      Flags &= ~tfNewDirectory;
      Flags |= tfAutoResume;
    }
  }
  while (RobustLoop.Retry());
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CreateTargetDirectory(
  const UnicodeString & DirectoryPath, int Attrs, const TCopyParamType * CopyParam)
{
  bool DoCreate = !DirectoryExists(DirectoryPath);
  if (DoCreate)
  {
    TRemoteProperties Properties;
    // Do not even try with FTP, as while theoretically it supports setting permissions of created folders and it will try,
    // it most likely fail as most FTP servers do not support SITE CHMOD.
    if (CopyParam->PreserveRights && IsCapable[fcModeChangingUpload])
    {
      Properties.Valid = Properties.Valid << vpRights;
      Properties.Rights = CopyParam->RemoteFileRights(Attrs);
    }
    Properties.Valid = Properties.Valid << vpEncrypt;
    Properties.Encrypt = CopyParam->EncryptNewFiles;
    CreateDirectory(DirectoryPath, &Properties);
  }
  return DoCreate;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DirectorySource(
  const UnicodeString & DirectoryName, const UnicodeString & TargetDir, const UnicodeString & DestDirectoryName,
  int Attrs, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  FFileSystem->TransferOnDirectory(TargetDir, CopyParam, Params);

  UnicodeString DestFullName = TargetDir + DestDirectoryName;

  OperationProgress->SetFile(DirectoryName);

  bool PostCreateDir = FLAGCLEAR(Flags, tfPreCreateDir);
  // WebDAV, SFTP and S3
  if (!PostCreateDir)
  {
    // This is originally a code for WebDAV, SFTP used a slightly different logic,
    // but functionally it should be very similar.
    if (CreateTargetDirectory(DestFullName, Attrs, CopyParam))
    {
      Flags |= tfNewDirectory;
    }
  }

  bool DoRecurse = FLAGCLEAR(Params, cpNoRecurse);
  if (DoRecurse)
  {
    TSearchRecOwned SearchRec;
    bool FindOK = LocalFindFirstLoop(DirectoryName + L"*.*", SearchRec);
    while (FindOK && !OperationProgress->Cancel)
    {
      UnicodeString FileName = SearchRec.GetFilePath();
      try
      {
        if (SearchRec.IsRealFile())
        {
          // Not sure if we need the trailing slash here, but we cannot use it in CreateTargetDirectory.
          // At least FTP cannot handle it, when setting the new directory permissions.
          UnicodeString ATargetDir = UnixIncludeTrailingBackslash(DestFullName);
          SourceRobust(FileName, &SearchRec, ATargetDir, CopyParam, Params, OperationProgress, (Flags & ~(tfFirstLevel | tfAutoResume)));
          // FTP: if any file got uploaded (i.e. there were any file in the directory and at least one was not skipped),
          // do not try to create the directory, as it should be already created by FZAPI during upload
          PostCreateDir = false;
        }
      }
      catch (ESkipFile &E)
      {
        // If ESkipFile occurs, just log it and continue with next file
        TSuspendFileOperationProgress Suspend(OperationProgress);
        // here a message to user was displayed, which was not appropriate
        // when user refused to overwrite the file in subdirectory.
        // hopefully it won't be missing in other situations.
        if (!HandleException(&E))
        {
          throw;
        }
      }

      FindOK = LocalFindNextLoop(SearchRec);
    }

    SearchRec.Close();
  }

  // FTP
  if (PostCreateDir)
  {
    CreateTargetDirectory(DestFullName, Attrs, CopyParam);
  }

  // Parallel transfers (cpNoRecurse) won't be allowed if any of these are set anyway (see CanParallel).
  // Exception is ClearArchive, which is does not prevent parallel transfer, but is silently ignored for directories.
  if (DoRecurse && !OperationProgress->Cancel)
  {
    // TODO : Delete also read-only directories.
    // TODO : Show error message on failure.
    if (IsCapable[fcPreservingTimestampDirs] && CopyParam->PreserveTime && CopyParam->PreserveTimeDirs)
    {
      TRemoteProperties Properties;
      Properties.Valid << vpModification;

      OpenLocalFile(
        ExcludeTrailingBackslash(DirectoryName), GENERIC_READ, NULL, NULL, NULL,
        &Properties.Modification, &Properties.LastAccess, NULL);

      ChangeFileProperties(DestFullName, NULL, &Properties);
    }

    if (FLAGSET(Params, cpDelete))
    {
      DebugAssert(FLAGCLEAR(Params, cpNoRecurse));
      RemoveDir(ApiPath(DirectoryName));
    }
    else if (CopyParam->ClearArchive && FLAGSET(Attrs, faArchive))
    {
      FILE_OPERATION_LOOP_BEGIN
      {
        THROWOSIFFALSE(FileSetAttr(ApiPath(DirectoryName), Attrs & ~faArchive) == 0);
      }
      FILE_OPERATION_LOOP_END(FMTLOAD(CANT_SET_ATTRS, (DirectoryName)));
    }
  }
}
//---------------------------------------------------------------------------
bool TTerminal::UseAsciiTransfer(
  const UnicodeString & BaseFileName, TOperationSide Side, const TCopyParamType * CopyParam,
  const TFileMasks::TParams & MaskParams)
{
  return
    IsCapable[fcTextMode] &&
    CopyParam->UseAsciiTransfer(BaseFileName, Side, MaskParams) &&
    // Used either before PartSize is set (to check that file is to be transferred in binary mode, hence allows parallel file transfer),
    // or later during parallel transfer (which never happens for ascii mode).
    DebugAlwaysTrue(CopyParam->PartSize < 0);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SelectTransferMode(
  const UnicodeString & BaseFileName, TOperationSide Side, const TCopyParamType * CopyParam,
  const TFileMasks::TParams & MaskParams)
{
  bool AsciiTransfer = UseAsciiTransfer(BaseFileName, Side, CopyParam, MaskParams);
  OperationProgress->SetAsciiTransfer(AsciiTransfer);
  UnicodeString ModeName = (OperationProgress->AsciiTransfer ? L"Ascii" : L"Binary");
  LogEvent(0, FORMAT(L"%s transfer mode selected.", (ModeName)));
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SelectSourceTransferMode(const TLocalFileHandle & Handle, const TCopyParamType * CopyParam)
{
  // Will we use ASCII or BINARY file transfer?
  TFileMasks::TParams MaskParams;
  MaskParams.Size = Handle.Size;
  MaskParams.Modification = Handle.Modification;
  UnicodeString BaseFileName = GetBaseFileName(Handle.FileName);
  SelectTransferMode(BaseFileName, osLocal, CopyParam, MaskParams);
}
//---------------------------------------------------------------------------
void TTerminal::DoDeleteLocalFile(const UnicodeString & FileName)
{
  FILE_OPERATION_LOOP_BEGIN
  {
    DeleteFileChecked(FileName);
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(DELETE_LOCAL_FILE_ERROR, (FileName)));
}
//---------------------------------------------------------------------------
void TTerminal::DoRenameLocalFileForce(const UnicodeString & OldName, const UnicodeString & NewName)
{
  if (::FileExists(ApiPath(NewName)))
  {
    DoDeleteLocalFile(NewName);
  }

  FILE_OPERATION_LOOP_BEGIN
  {
    THROWOSIFFALSE(Sysutils::RenameFile(ApiPath(OldName), ApiPath(NewName)));
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(RENAME_FILE_ERROR, (OldName, NewName)));
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::UpdateSource(const TLocalFileHandle & Handle, const TCopyParamType * CopyParam, int Params)
{
  // TODO: Delete also read-only files.
  if (FLAGSET(Params, cpDelete))
  {
    if (!Handle.Directory)
    {
      LogEvent(FORMAT(L"Deleting successfully uploaded source file \"%s\".", (Handle.FileName)));

      DoDeleteLocalFile(Handle.FileName);
    }
  }
  else if (CopyParam->ClearArchive && FLAGSET(Handle.Attrs, faArchive))
  {
    FILE_OPERATION_LOOP_BEGIN
    {
      THROWOSIFFALSE(FileSetAttr(ApiPath(Handle.FileName), (Handle.Attrs & ~faArchive)) == 0);
    }
    FILE_OPERATION_LOOP_END(FMTLOAD(CANT_SET_ATTRS, (Handle.FileName)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Source(
  const UnicodeString & FileName, const TSearchRecSmart * SearchRec,
  const UnicodeString & TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags, TUploadSessionAction & Action, bool & ChildError)
{
  UnicodeString ActionFileName = FileName;
  if (CopyParam->OnTransferIn == NULL)
  {
    ActionFileName = ExpandUNCFileName(ActionFileName);
  }
  Action.FileName(ActionFileName);

  OperationProgress->SetFile(FileName, false);

  if (!AllowLocalFileTransfer(FileName, SearchRec, CopyParam, OperationProgress))
  {
    throw ESkipFile();
  }

  TLocalFileHandle Handle;
  if (CopyParam->OnTransferIn == NULL)
  {
    OpenLocalFile(FileName, GENERIC_READ, Handle);
  }
  else
  {
    Handle.FileName = FileName;
  }

  OperationProgress->SetFileInProgress();

  UnicodeString DestFileName =
    ChangeFileName(CopyParam, ExtractFileName(FileName), osLocal, FLAGSET(Flags, tfFirstLevel));

  if (Handle.Directory)
  {
    Action.Cancel();
    DirectorySource(
      IncludeTrailingBackslash(FileName), TargetDir, DestFileName, Handle.Attrs,
      CopyParam, Params, OperationProgress, Flags);
  }
  else
  {
    if (CopyParam->OnTransferIn != NULL)
    {
      LogEvent(FORMAT(L"Streaming \"%s\" to remote directory started.", (FileName)));
    }
    else
    {
      LogEvent(FORMAT(L"Copying \"%s\" to remote directory started.", (FileName)));

      OperationProgress->SetLocalSize(Handle.Size);

      // Suppose same data size to transfer as to read
      // (not true with ASCII transfer)
      OperationProgress->SetTransferSize(OperationProgress->LocalSize);
    }

    if (IsCapable[fcTextMode])
    {
      SelectSourceTransferMode(Handle, CopyParam);
    }

    FFileSystem->Source(
      Handle, TargetDir, DestFileName, CopyParam, Params, OperationProgress, Flags, Action, ChildError);

    LogFileDone(OperationProgress, AbsolutePath(TargetDir + DestFileName, true), Action);
    OperationProgress->Succeeded();
  }

  Handle.Release();

  UpdateSource(Handle, CopyParam, Params);
}
//---------------------------------------------------------------------------
void TTerminal::CheckParallelFileTransfer(
  const UnicodeString & TargetDir, TStringList * Files, const TCopyParamType * CopyParam, int Params,
  UnicodeString & ParallelFileName, __int64 & ParallelFileSize, TFileOperationProgressType * OperationProgress)
{
  if ((Configuration->ParallelTransferThreshold > 0) &&
      FFileSystem->IsCapable(fcParallelFileTransfers))
  {
    TObject * ParallelObject = NULL;
    if (TParallelOperation::GetOnlyFile(Files, ParallelFileName, ParallelObject))
    {
      TRemoteFile * File = static_cast<TRemoteFile *>(ParallelObject);
      const TRemoteFile * UltimateFile = File->Resolve();
      if ((UltimateFile == File) && // not tested with symlinks
          (UltimateFile->Size >= static_cast<__int64>(Configuration->ParallelTransferThreshold) * 1024))
      {
        UnicodeString BaseFileName = GetBaseFileName(ParallelFileName);
        TFileMasks::TParams MaskParams;
        MaskParams.Size = UltimateFile->Size;
        MaskParams.Modification = File->Modification;
        if (!UseAsciiTransfer(BaseFileName, osRemote, CopyParam, MaskParams))
        {
          ParallelFileSize = UltimateFile->Size;
          UnicodeString TargetFileName = CopyParam->ChangeFileName(UnixExtractFileName(ParallelFileName), osRemote, true);
          UnicodeString DestFullName = CombinePaths(TargetDir, TargetFileName);

          if (::FileExists(ApiPath(DestFullName)))
          {
            TSuspendFileOperationProgress Suspend(OperationProgress);

            TOverwriteFileParams FileParams;
            __int64 MTime;
            OpenLocalFile(DestFullName, GENERIC_READ, NULL, NULL, NULL, &MTime, NULL, &FileParams.DestSize, false);
            FileParams.SourceSize = ParallelFileSize;
            FileParams.SourceTimestamp = UltimateFile->Modification;
            FileParams.SourcePrecision = UltimateFile->ModificationFmt;
            FileParams.DestTimestamp = UnixToDateTime(MTime, SessionData->DSTMode);
            int Answers = qaYes | qaNo | qaCancel;
            TQueryParams QueryParams(qpNeverAskAgainCheck);
            unsigned int Answer =
              ConfirmFileOverwrite(
                ParallelFileName, TargetFileName, &FileParams, Answers, &QueryParams, osRemote,
                CopyParam, Params, OperationProgress, EmptyStr);
            switch (Answer)
            {
              case qaCancel:
              case qaNo:
                OperationProgress->SetCancelAtLeast(csCancel);
                break;
            }
          }
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CopyToLocal(
  TStrings * FilesToCopy, const UnicodeString & TargetDir, const TCopyParamType * CopyParam, int Params,
  TParallelOperation * ParallelOperation)
{
  DebugAssert(FFileSystem);

  // see scp.c: sink(), tolocal()

  bool Result = false;
  DebugAssert(FilesToCopy != NULL);
  TOnceDoneOperation OnceDoneOperation = odoIdle;

  if ((CopyParam->OnTransferOut != NULL) && !FFileSystem->IsCapable(fcTransferOut))
  {
    NotSupported();
  }

  FDestFileName = L"";
  FMultipleDestinationFiles = false;

  BeginTransaction();
  try
  {
    __int64 TotalSize;
    bool TotalSizeKnown = false;
    FLastProgressLogged = GetTickCount();
    TFileOperationProgressType OperationProgress(&DoProgress, &DoFinished);

    std::unique_ptr<TStringList> Files;
    bool ACanParallel =
      CanParallel(CopyParam, Params, ParallelOperation) &&
      DebugAlwaysTrue(CopyParam->OnTransferOut == NULL);
    if (ACanParallel)
    {
      Files.reset(new TStringList());
      Files->OwnsObjects = true;
    }

    if ((CopyParam->Size >= 0) &&
        DebugAlwaysTrue(!ACanParallel)) // Size is set for sync only and we never use parallel transfer for sync
    {
      TotalSize = CopyParam->Size;
      TotalSizeKnown = true;
    }
    else
    {
      ExceptionOnFail = true;
      try
      {
        TCalculateSizeStats Stats;
        Stats.FoundFiles = Files.get();
        TCalculateSizeParams Params;
        Params.Params = csIgnoreErrors;
        Params.CopyParam = CopyParam;
        Params.AllowDirs = ACanParallel || CopyParam->CalculateSize;
        Params.Stats = &Stats;
        if (CalculateFilesSize(FilesToCopy, TotalSize, Params))
        {
          TotalSizeKnown = true;
        }
      }
      __finally
      {
        ExceptionOnFail = false;
      }
    }

    OperationStart(OperationProgress, (Params & cpDelete ? foMove : foCopy), osRemote,
      FilesToCopy->Count, Params & cpTemporary, TargetDir, CopyParam->CPSLimit, CopyParam->OnceDoneOperation);

    bool CollectingUsage = false;
    try
    {
      if (TotalSizeKnown)
      {
        if (Configuration->Usage->Collect)
        {
          int CounterTotalSize = TUsage::CalculateCounterSize(TotalSize);
          Configuration->Usage->Inc(L"Downloads");
          Configuration->Usage->IncAndSetMax(L"DownloadedBytes", L"MaxDownloadSize", CounterTotalSize);
          CollectingUsage = true;
        }

        OperationProgress.SetTotalSize(TotalSize);
      }

      try
      {
        try
        {
          bool Parallel = ACanParallel && TotalSizeKnown;
          LogTotalTransferDetails(TargetDir, CopyParam, &OperationProgress, Parallel, Files.get());

          if (Parallel)
          {
            UnicodeString ParallelFileName;
            __int64 ParallelFileSize = -1;
            CheckParallelFileTransfer(TargetDir, Files.get(), CopyParam, Params, ParallelFileName, ParallelFileSize, &OperationProgress);

            if (OperationProgress.Cancel == csContinue)
            {
              if (ParallelFileSize >= 0)
              {
                DebugAssert(ParallelFileSize == TotalSize);
                Params |= cpNoConfirmation;
              }

              // OnceDoneOperation is not supported
              ParallelOperation->Init(
                Files.release(), TargetDir, CopyParam, Params, &OperationProgress, Log->Name, ParallelFileSize);
              UnicodeString ParallelFilePrefix;
              CopyParallel(ParallelOperation, &OperationProgress);
            }
          }
          else
          {
            FFileSystem->CopyToLocal(FilesToCopy, TargetDir, CopyParam, Params,
              &OperationProgress, OnceDoneOperation);
          }

          LogTotalTransferDone(&OperationProgress);
        }
        __finally
        {
          if (Active)
          {
            ReactOnCommand(fsCopyToLocal);
          }
        }
      }
      catch (Exception &E)
      {
        CommandError(&E, MainInstructions(LoadStr(TOLOCAL_COPY_ERROR)));
        OnceDoneOperation = odoIdle;
      }

      if (OperationProgress.Cancel == csContinue)
      {
        Result = true;
      }
    }
    __finally
    {
      if (CollectingUsage)
      {
        Configuration->Usage->IncAndSetMax(L"DownloadTime", L"MaxDownloadTime", TimeToSeconds(OperationProgress.TimeElapsed()));
      }
      OperationStop(OperationProgress);
    }
    Files.reset(NULL);
  }
  __finally
  {
    // If session is still active (no fatal error) we reload directory
    // by calling EndTransaction
    EndTransaction();
  }

  if (OnceDoneOperation != odoIdle)
  {
    UnicodeString DestFileName;
    if (!FMultipleDestinationFiles)
    {
      DestFileName = FDestFileName;
    }
    CloseOnCompletion(OnceDoneOperation, UnicodeString(), TargetDir, DestFileName);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCopyToLocal(
  TStrings * FilesToCopy, const UnicodeString & TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags, TOnceDoneOperation & OnceDoneOperation)
{
  DebugAssert((FilesToCopy != NULL) && (OperationProgress != NULL));

  UnicodeString FullTargetDir = IncludeTrailingBackslash(TargetDir);
  int Index = 0;
  while ((Index < FilesToCopy->Count) && !OperationProgress->Cancel)
  {
    bool Success = false;
    UnicodeString FileName = FilesToCopy->Strings[Index];
    const TRemoteFile * File = dynamic_cast<const TRemoteFile *>(FilesToCopy->Objects[Index]);

    try
    {
      try
      {
        UnicodeString AbsoluteFileName = AbsolutePath(FileName, true);
        SinkRobust(AbsoluteFileName, File, FullTargetDir, CopyParam, Params, OperationProgress, Flags | tfFirstLevel);
        Success = true;
      }
      catch (ESkipFile & E)
      {
        TSuspendFileOperationProgress Suspend(OperationProgress);
        if (!HandleException(&E))
        {
          throw;
        }
      }
    }
    __finally
    {
      OperationFinish(OperationProgress, File, FileName, Success, OnceDoneOperation);
    }
    Index++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SinkRobust(
  const UnicodeString & FileName, const TRemoteFile * File, const UnicodeString & TargetDir,
  const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  TDownloadSessionAction Action(ActionLog);
  try
  {
    bool * AFileTransferAny = FLAGSET(Flags, tfUseFileTransferAny) ? &FFileTransferAny : NULL;
    bool CanRetry = (CopyParam->OnTransferOut == NULL);
    TRobustOperationLoop RobustLoop(this, OperationProgress, AFileTransferAny, CanRetry);
    bool Sunk = false;

    do
    {
      try
      {
        // If connection is lost while deleting the file, so not retry download on the next round.
        // The file may not exist anymore and the download attempt might overwrite the (only) local copy.
        if (!Sunk)
        {
          Sink(FileName, File, TargetDir, CopyParam, Params, OperationProgress, Flags, Action);
          Sunk = true;
        }

        if (FLAGSET(Params, cpDelete))
        {
          DebugAssert(FLAGCLEAR(Params, cpNoRecurse));
          // If file is directory, do not delete it recursively, because it should be
          // empty already. If not, it should not be deleted (some files were
          // skipped or some new files were copied to it, while we were downloading)
          int Params = dfNoRecursive;
          DeleteFile(FileName, File, &Params);
        }
      }
      catch (Exception & E)
      {
        if (!RobustLoop.TryReopen(E))
        {
          if (!Sunk)
          {
            RollbackAction(Action, OperationProgress, &E);
          }
          throw;
        }
      }

      if (RobustLoop.ShouldRetry())
      {
        OperationProgress->RollbackTransfer();
        Action.Restart();
        DebugAssert(File != NULL);
        if (!File->IsDirectory)
        {
          // prevent overwrite and resume confirmations
          Params |= cpNoConfirmation;
          Flags |= tfAutoResume;
        }
      }
    }
    while (RobustLoop.Retry());
  }
  __finally
  {
    // Once we issue <download> we must terminate the data stream
    if (Action.IsValid() && (CopyParam->OnTransferOut != NULL) && DebugAlwaysTrue(IsCapable[fcTransferOut]))
    {
      CopyParam->OnTransferOut(this, NULL, 0);
    }
  }
}
//---------------------------------------------------------------------------
struct TSinkFileParams
{
  UnicodeString TargetDir;
  const TCopyParamType * CopyParam;
  int Params;
  TFileOperationProgressType * OperationProgress;
  bool Skipped;
  unsigned int Flags;
};
//---------------------------------------------------------------------------
void __fastcall TTerminal::Sink(
  const UnicodeString & FileName, const TRemoteFile * File, const UnicodeString & TargetDir,
  const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress, unsigned int Flags,
  TDownloadSessionAction & Action)
{
  Action.FileName(FileName);
  DebugAssert(File);

  if (!DoAllowRemoteFileTransfer(File, CopyParam, false))
  {
    LogEvent(FORMAT(L"File \"%s\" excluded from transfer", (FileName)));
    throw ESkipFile();
  }

  if (CopyParam->SkipTransfer(FileName, File->IsDirectory))
  {
    OperationProgress->AddSkippedFileSize(File->Resolve()->Size);
    throw ESkipFile();
  }

  LogFileDetails(FileName, File->Modification, File->Size, File->LinkedFile);

  OperationProgress->SetFile(FileName);

  UnicodeString OnlyFileName = UnixExtractFileName(FileName);
  UnicodeString DestFileName = ChangeFileName(CopyParam, OnlyFileName, osRemote, FLAGSET(Flags, tfFirstLevel));
  UnicodeString DestFullName = TargetDir + DestFileName;

  if (File->IsDirectory)
  {
    Action.Cancel();
    if (CanRecurseToDirectory(File))
    {
      FILE_OPERATION_LOOP_BEGIN
      {
        int Attrs = FileGetAttrFix(ApiPath(DestFullName));
        if (FLAGCLEAR(Attrs, faDirectory))
        {
          EXCEPTION;
        }
      }
      FILE_OPERATION_LOOP_END(FMTLOAD(NOT_DIRECTORY_ERROR, (DestFullName)));

      FILE_OPERATION_LOOP_BEGIN
      {
        THROWOSIFFALSE(ForceDirectories(ApiPath(DestFullName)));
      }
      FILE_OPERATION_LOOP_END(FMTLOAD(CREATE_DIR_ERROR, (DestFullName)));

      if (FLAGCLEAR(Params, cpNoRecurse))
      {
        TSinkFileParams SinkFileParams;
        SinkFileParams.TargetDir = IncludeTrailingBackslash(DestFullName);
        SinkFileParams.CopyParam = CopyParam;
        SinkFileParams.Params = Params;
        SinkFileParams.OperationProgress = OperationProgress;
        SinkFileParams.Skipped = false;
        SinkFileParams.Flags = Flags & ~(tfFirstLevel | tfAutoResume);

        ProcessDirectory(FileName, SinkFile, &SinkFileParams);

        FFileSystem->DirectorySunk(DestFullName, File, CopyParam);

        // Do not delete directory if some of its files were skipped.
        // Throw "skip file" for the directory to avoid attempt to deletion
        // of any parent directory
        if (FLAGSET(Params, cpDelete) && SinkFileParams.Skipped)
        {
          throw ESkipFile();
        }
      }
    }
    else
    {
      LogEvent(FORMAT(L"Skipping symlink to directory \"%s\".", (FileName)));
    }
  }
  else
  {
    if (CopyParam->OnTransferOut != NULL)
    {
      LogEvent(FORMAT(L"Streaming \"%s\" to local machine started.", (FileName)));
    }
    else
    {
      LogEvent(FORMAT(L"Copying \"%s\" to local directory started.", (FileName)));
    }

    // Will we use ASCII or BINARY file transfer?
    UnicodeString BaseFileName = GetBaseFileName(FileName);
    const TRemoteFile * UltimateFile = File->Resolve();
    TFileMasks::TParams MaskParams;
    MaskParams.Size = UltimateFile->Size;
    MaskParams.Modification = File->Modification;
    SelectTransferMode(BaseFileName, osRemote, CopyParam, MaskParams);

    // Suppose same data size to transfer as to write
    // (not true with ASCII transfer)
    __int64 TransferSize = (CopyParam->PartSize >= 0) ? CopyParam->PartSize : (UltimateFile->Size - std::max(0LL, CopyParam->PartOffset));
    OperationProgress->SetLocalSize(TransferSize);
    if (IsFileEncrypted(FileName))
    {
      TransferSize += TEncryption::GetOverhead();
    }
    OperationProgress->SetTransferSize(TransferSize);

    int Attrs;
    UnicodeString LogFileName;
    if (CopyParam->OnTransferOut != NULL)
    {
      Attrs = -1;
      LogFileName = L"-";
    }
    else
    {
      FILE_OPERATION_LOOP_BEGIN
      {
        Attrs = FileGetAttrFix(ApiPath(DestFullName));
        if ((Attrs >= 0) && FLAGSET(Attrs, faDirectory))
        {
          EXCEPTION;
        }
      }
      FILE_OPERATION_LOOP_END(FMTLOAD(NOT_FILE_ERROR, (DestFullName)));
      LogFileName = ExpandUNCFileName(DestFullName);
    }

    FFileSystem->Sink(
      FileName, File, TargetDir, DestFileName, Attrs, CopyParam, Params, OperationProgress, Flags, Action);

    LogFileDone(OperationProgress, LogFileName, Action);
    OperationProgress->Succeeded();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::UpdateTargetAttrs(
  const UnicodeString & DestFullName, const TRemoteFile * File, const TCopyParamType * CopyParam, int Attrs)
{
  if (Attrs == -1)
  {
    Attrs = faArchive;
  }
  int NewAttrs = CopyParam->LocalFileAttrs(*File->Rights);
  if ((NewAttrs & Attrs) != NewAttrs)
  {
    FILE_OPERATION_LOOP_BEGIN
    {
      THROWOSIFFALSE(FileSetAttr(ApiPath(DestFullName), Attrs | NewAttrs) == 0);
    }
    FILE_OPERATION_LOOP_END(FMTLOAD(CANT_SET_ATTRS, (DestFullName)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::UpdateTargetTime(
  HANDLE Handle, TDateTime Modification, TModificationFmt ModificationFmt, TDSTMode DSTMode)
{
  if (ModificationFmt == mfNone)
  {
    LogEvent(L"Timestamp not known");
  }
  else
  {
    LogEvent(FORMAT(L"Preserving timestamp [%s]", (StandardTimestamp(Modification))));
    FILETIME WrTime = DateTimeToFileTime(Modification, DSTMode);
    if (!SetFileTime(Handle, NULL, NULL, &WrTime))
    {
      int Error = GetLastError();
      LogEvent(FORMAT(L"Preserving timestamp failed, ignoring: %s", (SysErrorMessageForError(Error))));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SinkFile(UnicodeString FileName, const TRemoteFile * File, void * Param)
{
  TSinkFileParams * Params = static_cast<TSinkFileParams *>(Param);
  DebugAssert(Params->OperationProgress != NULL);
  try
  {
    SinkRobust(FileName, File, Params->TargetDir, Params->CopyParam,
      Params->Params, Params->OperationProgress, Params->Flags);
  }
  catch (ESkipFile & E)
  {
    Params->Skipped = true;

    {
      TSuspendFileOperationProgress Suspend(Params->OperationProgress);
      if (!HandleException(&E))
      {
        throw;
      }
    }

    if (Params->OperationProgress->Cancel)
    {
      Abort();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReflectSettings()
{
  DebugAssert(FLog != NULL);
  FLog->ReflectSettings();
  if (FActionLogOwned)
  {
    DebugAssert(FActionLog != NULL);
    FActionLog->ReflectSettings();
  }
  // also FTunnelLog ?
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CollectUsage()
{
  switch (SessionData->FSProtocol)
  {
    case fsSCPonly:
      Configuration->Usage->Inc(L"OpenedSessionsSCP");
      break;

    case fsSFTP:
    case fsSFTPonly:
      Configuration->Usage->Inc(L"OpenedSessionsSFTP");
      break;

    case fsFTP:
      if (SessionData->Ftps == ftpsNone)
      {
        Configuration->Usage->Inc(L"OpenedSessionsFTP");
      }
      else
      {
        Configuration->Usage->Inc(L"OpenedSessionsFTPS");
      }
      break;

    case fsWebDAV:
      if (SessionData->Ftps == ftpsNone)
      {
        Configuration->Usage->Inc(L"OpenedSessionsWebDAV");
      }
      else
      {
        Configuration->Usage->Inc(L"OpenedSessionsWebDAVS");
      }
      break;

    case fsS3:
      Configuration->Usage->Inc(L"OpenedSessionsS3");
      break;
  }

  if (Configuration->Logging && Configuration->LogToFile)
  {
    Configuration->Usage->Inc(L"OpenedSessionsLogToFile2");
  }

  if (Configuration->LogActions)
  {
    Configuration->Usage->Inc(L"OpenedSessionsXmlLog");
  }

  std::unique_ptr<TSessionData> FactoryDefaults(new TSessionData(L""));
  if (!SessionData->IsSame(FactoryDefaults.get(), true))
  {
    Configuration->Usage->Inc(L"OpenedSessionsAdvanced");
  }

  if (SessionData->ProxyMethod != ::pmNone)
  {
    Configuration->Usage->Inc(L"OpenedSessionsProxy");
  }
  if (SessionData->FtpProxyLogonType > 0)
  {
    Configuration->Usage->Inc(L"OpenedSessionsFtpProxy");
  }
  if (IsEncryptingFiles())
  {
    Configuration->Usage->Inc(L"OpenedSessionsEncrypted");
  }

  if (SessionData->SendBuf == 0)
  {
    Configuration->Usage->Inc(L"OpenedSessionsNoSendBuf");
  }

  FCollectFileSystemUsage = true;
}
//---------------------------------------------------------------------------
static UnicodeString __fastcall FormatCertificateData(const UnicodeString & Fingerprint, int Failures)
{
  return FORMAT(L"%s;%2.2X", (Fingerprint, Failures));
}
//---------------------------------------------------------------------------
bool  __fastcall TTerminal::VerifyCertificate(
  const UnicodeString & CertificateStorageKey, const UnicodeString & SiteKey,
  const UnicodeString & FingerprintSHA1, const UnicodeString & FingerprintSHA256,
  const UnicodeString & CertificateSubject, int Failures)
{
  bool Result = false;

  UnicodeString CertificateDataSHA1 = FormatCertificateData(FingerprintSHA1, Failures);
  UnicodeString CertificateDataSHA256 = FormatCertificateData(FingerprintSHA256, Failures);

  std::unique_ptr<THierarchicalStorage> Storage(Configuration->CreateConfigStorage());
  Storage->AccessMode = smRead;

  if (Storage->OpenSubKey(CertificateStorageKey, false))
  {
    if (Storage->ValueExists(SiteKey))
    {
      UnicodeString CachedCertificateData = Storage->ReadString(SiteKey, L"");
      if (SameChecksum(CertificateDataSHA1, CachedCertificateData, false) ||
          SameChecksum(CertificateDataSHA256, CachedCertificateData, false))
      {
        LogEvent(FORMAT(L"Certificate for \"%s\" matches cached fingerprint and failures", (CertificateSubject)));
        Result = true;
      }
    }
    else if (Storage->ValueExists(FingerprintSHA1) || Storage->ValueExists(FingerprintSHA256))
    {
      LogEvent(FORMAT(L"Certificate for \"%s\" matches legacy cached fingerprint", (CertificateSubject)));
      Result = true;
    }
  }

  if (!Result)
  {
    UnicodeString Buf = SessionData->HostKey;
    while (!Result && !Buf.IsEmpty())
    {
      UnicodeString ExpectedKey = CutToChar(Buf, L';', false);
      if (ExpectedKey == L"*")
      {
        UnicodeString Message = LoadStr(ANY_CERTIFICATE);
        Information(Message);
        Log->Add(llException, Message);
        Result = true;
      }
      else if (SameChecksum(ExpectedKey, FingerprintSHA1, false) ||
               SameChecksum(ExpectedKey, FingerprintSHA256, false))
      {
        LogEvent(FORMAT(L"Certificate for \"%s\" matches configured fingerprint", (CertificateSubject)));
        Result = true;
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::ConfirmCertificate(
  TSessionInfo & SessionInfo, int Failures, const UnicodeString & CertificateStorageKey, bool CanRemember)
{
  TClipboardHandler ClipboardHandler;
  ClipboardHandler.Text =
    FORMAT(L"SHA-256: %s\nSHA-1: %s", (SessionInfo.CertificateFingerprintSHA256, SessionInfo.CertificateFingerprintSHA1));

  TQueryButtonAlias Aliases[1];
  Aliases[0].Button = qaRetry;
  Aliases[0].Alias = LoadStr(COPY_KEY_BUTTON);
  Aliases[0].ActionAlias = LoadStr(COPY_CERTIFICATE_ACTION);
  Aliases[0].OnSubmit = &ClipboardHandler.Copy;

  TQueryParams Params(qpWaitInBatch);
  Params.HelpKeyword = HELP_VERIFY_CERTIFICATE;
  Params.NoBatchAnswers = qaYes | qaRetry;
  Params.Aliases = Aliases;
  Params.AliasesCount = LENOF(Aliases);
  unsigned int Answer =
    QueryUser(
      FMTLOAD(VERIFY_CERT_PROMPT3, (SessionInfo.Certificate)),
      NULL, qaYes | qaNo | qaCancel | qaRetry, &Params, qtWarning);

  bool Result;
  switch (Answer)
  {
    case qaYes:
      CacheCertificate(
        CertificateStorageKey, SessionData->SiteKey,
        SessionInfo.CertificateFingerprintSHA1, SessionInfo.CertificateFingerprintSHA256, Failures);
      Result = true;
      break;

    case qaNo:
      Result = true;
      break;

    case qaCancel:
      Configuration->Usage->Inc(L"HostNotVerified");
      Result = false;
      break;

    default:
      DebugFail();
      Result = false;
      break;
  }

  // Cache only if the certificate was accepted manually
  if (Result && CanRemember)
  {
    Configuration->RememberLastFingerprint(
      SessionData->SiteKey, TlsFingerprintType, SessionInfo.CertificateFingerprintSHA256);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CacheCertificate(
  const UnicodeString & CertificateStorageKey, const UnicodeString & SiteKey,
  const UnicodeString & DebugUsedArg(FingerprintSHA1), const UnicodeString & FingerprintSHA256, int Failures)
{
  UnicodeString CertificateData = FormatCertificateData(FingerprintSHA256, Failures);

  std::unique_ptr<THierarchicalStorage> Storage(Configuration->CreateConfigStorage());
  Storage->AccessMode = smReadWrite;

  if (Storage->OpenSubKey(CertificateStorageKey, true))
  {
    Storage->WriteString(SiteKey, CertificateData);
  }
}
//---------------------------------------------------------------------------
// Shared implementation for WebDAV and S3
bool TTerminal::VerifyOrConfirmHttpCertificate(
  const UnicodeString & AHostName, int APortNumber, const TNeonCertificateData & AData, bool CanRemember,
  TSessionInfo & SessionInfo)
{
  TNeonCertificateData Data = AData;
  SessionInfo.CertificateFingerprintSHA1 = Data.FingerprintSHA1;
  SessionInfo.CertificateFingerprintSHA256 = Data.FingerprintSHA256;

  bool Result;
  if (SessionData->FingerprintScan)
  {
    Result = false;
  }
  else
  {
    LogEvent(0, CertificateVerificationMessage(Data));

    UnicodeString WindowsValidatedMessage;
    // Side effect is that NE_SSL_UNTRUSTED is removed from Failure, before we call VerifyCertificate,
    // as it compares failures against a cached failures that do not include NE_SSL_UNTRUSTED
    // (if the certificate is trusted by Windows certificate store).
    // But we will log that result only if we actually use it for the decision.
    bool WindowsValidated = NeonWindowsValidateCertificateWithMessage(Data, WindowsValidatedMessage);

    UnicodeString SiteKey = TSessionData::FormatSiteKey(AHostName, APortNumber);
    Result =
      VerifyCertificate(
        HttpsCertificateStorageKey, SiteKey, Data.FingerprintSHA1, Data.FingerprintSHA256, Data.Subject, Data.Failures);

    if (Result)
    {
      SessionInfo.CertificateVerifiedManually = true;
    }
    else
    {
      Result = WindowsValidated;
      LogEvent(0, WindowsValidatedMessage);
    }

    SessionInfo.Certificate = CertificateSummary(Data, AHostName);

    if (!Result)
    {
      if (ConfirmCertificate(SessionInfo, Data.Failures, HttpsCertificateStorageKey, CanRemember))
      {
        Result = true;
        SessionInfo.CertificateVerifiedManually = true;
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CollectTlsUsage(const UnicodeString & TlsVersionStr)
{
  // see SSL_get_version() in OpenSSL ssl_lib.c
  if (TlsVersionStr == L"TLSv1.3")
  {
    Configuration->Usage->Inc(L"OpenedSessionsTLS13");
  }
  else if (TlsVersionStr == L"TLSv1.2")
  {
    Configuration->Usage->Inc(L"OpenedSessionsTLS12");
  }
  else if (TlsVersionStr == L"TLSv1.1")
  {
    Configuration->Usage->Inc(L"OpenedSessionsTLS11");
  }
  else if (TlsVersionStr == L"TLSv1")
  {
    Configuration->Usage->Inc(L"OpenedSessionsTLS10");
  }
  else if (TlsVersionStr == L"SSLv3")
  {
    Configuration->Usage->Inc(L"OpenedSessionsSSL30");
  }
  else if (TlsVersionStr == L"SSLv2")
  {
    Configuration->Usage->Inc(L"OpenedSessionsSSL20");
  }
  else
  {
    DebugFail();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::LoadTlsCertificate(X509 *& Certificate, EVP_PKEY *& PrivateKey)
{
  bool Result = !SessionData->TlsCertificateFile.IsEmpty();
  if (Result)
  {
    UnicodeString Passphrase = SessionData->Passphrase;

    // Inspired by neon's ne_ssl_clicert_read
    bool Retry;

    do
    {
      Retry = false;

      bool WrongPassphrase;
      ParseCertificate(SessionData->TlsCertificateFile, Passphrase, Certificate, PrivateKey, WrongPassphrase);
      if (WrongPassphrase)
      {
        if (Passphrase.IsEmpty())
        {
          LogEvent(L"Certificate is encrypted, need passphrase");
          Information(LoadStr(CLIENT_CERTIFICATE_LOADING));
        }
        else
        {
          Information(LoadStr(CERTIFICATE_DECODE_ERROR_INFO));
        }

        Passphrase = L"";
        if (PromptUser(
              SessionData, pkPassphrase,
              LoadStr(CERTIFICATE_PASSPHRASE_TITLE), L"",
              LoadStr(CERTIFICATE_PASSPHRASE_PROMPT), false, 0, Passphrase))
        {
          Retry = true;
        }
        else
        {
          Result = false;
        }
      }
    }
    while (Retry);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetBaseFileName(UnicodeString FileName)
{
  if (FSessionData->TrimVMSVersions)
  {
    int P = FileName.LastDelimiter(L";");
    if (P > 0)
    {
      FileName.SetLength(P - 1);
    }
  }
  return FileName;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::ChangeFileName(const TCopyParamType * CopyParam,
  UnicodeString FileName, TOperationSide Side, bool FirstLevel)
{
  FileName = GetBaseFileName(FileName);
  FileName = CopyParam->ChangeFileName(FileName, Side, FirstLevel);
  return FileName;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CanRecurseToDirectory(const TRemoteFile * File)
{
  return !File->IsSymLink || FSessionData->FollowDirectorySymlinks;
}
//---------------------------------------------------------------------------
TTerminal::TEncryptedFileNames::const_iterator __fastcall TTerminal::GetEncryptedFileName(const UnicodeString & Path)
{
  UnicodeString FileDir = UnixExtractFileDir(Path);

  // If we haven't been in this folder yet, read it to collect mapping to encrypted file names.
  if (FFoldersScannedForEncryptedFiles.find(FileDir) == FFoldersScannedForEncryptedFiles.end())
  {
    try
    {
      delete DoReadDirectoryListing(FileDir, true);
    }
    catch (Exception &)
    {
      if (!Active)
      {
        throw;
      }

      if (FEncryptedFileNames.find(Path) == FEncryptedFileNames.end())
      {
        FEncryptedFileNames.insert(std::make_pair(Path, UnixExtractFileName(Path)));
        LogEvent(2, FORMAT(L"Name of file '%s' assumed not to be encrypted", (Path)));
      }
    }

    FFoldersScannedForEncryptedFiles.insert(FileDir);
  }

  TEncryptedFileNames::const_iterator Result = FEncryptedFileNames.find(Path);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::IsFileEncrypted(const UnicodeString & Path, bool EncryptNewFiles)
{
  // can be optimized
  bool Result = (EncryptFileName(Path, EncryptNewFiles) != Path);
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::EncryptFileName(const UnicodeString & Path, bool EncryptNewFiles)
{
  UnicodeString Result = Path;
  if (IsEncryptingFiles() && !IsUnixRootPath(Path))
  {
    UnicodeString FileName = UnixExtractFileName(Path);
    UnicodeString FileDir = UnixExtractFileDir(Path);

    if (!FileName.IsEmpty() && IsRealFile(FileName))
    {
      TEncryptedFileNames::const_iterator I = GetEncryptedFileName(Path);
      if (I != FEncryptedFileNames.end())
      {
        FileName = I->second;
      }
      else if (EncryptNewFiles)
      {
        TEncryption Encryption(FEncryptKey);
        FileName = Encryption.EncryptFileName(FileName);
        FEncryptedFileNames.insert(std::make_pair(Path, FileName));
        LogEvent(2, FORMAT(L"Name of file '%s' encrypted as '%s'", (Path, FileName)));
      }
    }

    FileDir = EncryptFileName(FileDir, EncryptNewFiles);
    Result = UnixCombinePaths(FileDir, FileName);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::DecryptFileName(const UnicodeString & Path, bool DecryptFullPath, bool DontCache)
{
  UnicodeString Result = Path;
  if (IsEncryptingFiles() && !IsUnixRootPath(Path))
  {
    UnicodeString FileName = UnixExtractFileName(Path);
    UnicodeString FileNameEncrypted = FileName;

    bool Encrypted = TEncryption::IsEncryptedFileName(FileName);
    if (Encrypted)
    {
      TEncryption Encryption(FEncryptKey);
      FileName = Encryption.DecryptFileName(FileName);
    }

    if (Encrypted || DecryptFullPath) // DecryptFullPath is just an optimization
    {
      UnicodeString FileDir = UnixExtractFileDir(Path);
      FileDir = DecryptFileName(FileDir, DecryptFullPath, DontCache);
      Result = UnixCombinePaths(FileDir, FileName);
    }

    TEncryptedFileNames::iterator Iter = FEncryptedFileNames.find(Result);
    bool NotCached = (Iter == FEncryptedFileNames.end());
    if (!DontCache && (Encrypted || NotCached))
    {
      if (Encrypted && (NotCached || (Iter->second != FileNameEncrypted)))
      {
        LogEvent(2, FORMAT(L"Name of file '%s' decrypted from '%s'", (Result, FileNameEncrypted)));
      }
      // This may overwrite another variant of encryption
      FEncryptedFileNames[Result] = FileNameEncrypted;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
TRemoteFile * TTerminal::CheckRights(const UnicodeString & EntryType, const UnicodeString & FileName, bool & WrongRights)
{
  std::unique_ptr<TRemoteFile> File;
  try
  {
    LogEvent(FORMAT(L"Checking %s \"%s\"...", (LowerCase(EntryType), FileName)));
    File.reset(ReadFile(FileName));
    int ForbiddenRights = TRights::rfGroupWrite | TRights::rfOtherWrite;
    if ((File->Rights->Number & ForbiddenRights) != 0)
    {
      LogEvent(FORMAT(L"%s \"%s\" exists, but has incorrect permissions %s.", (EntryType, FileName, File->Rights->Octal)));
      WrongRights = true;
    }
    else
    {
      LogEvent(FORMAT(L"%s \"%s\" exists and has correct permissions %s.", (EntryType, FileName, File->Rights->Octal)));
    }
  }
  catch (Exception &)
  {
  }
  return File.release();
}
//---------------------------------------------------------------------------
void TTerminal::LogAndInformation(const UnicodeString & S)
{
  LogEvent(S);
  Information(S);
}
//---------------------------------------------------------------------------
UnicodeString TTerminal::UploadPublicKey(const UnicodeString & FileName)
{
  if (FSProtocol != cfsSFTP)
  {
    NotSupported();
  }

  UnicodeString Result;

  UnicodeString TemporaryDir;
  bool PrevAutoReadDirectory = AutoReadDirectory;
  bool PrevExceptionOnFail = ExceptionOnFail;

  UnicodeString AuthorizedKeysFilePath = FORMAT(L"%s/%s", (OpensshFolderName, OpensshAuthorizedKeysFileName));

  try
  {
    AutoReadDirectory = false;
    ExceptionOnFail = true;

    Log->AddSeparator();

    UnicodeString Comment;
    bool UnusedHasCertificate;
    UnicodeString Line = GetPublicKeyLine(FileName, Comment, UnusedHasCertificate);

    LogAndInformation(FMTLOAD(PUBLIC_KEY_ADDING, (AuthorizedKeysFilePath)) + L"\n" + Line);

    UnicodeString SshFolderAbsolutePath = UnixIncludeTrailingBackslash(GetHomeDirectory()) + OpensshFolderName;
    bool WrongRights = false;
    std::unique_ptr<TRemoteFile> SshFolderFile(CheckRights(L"Folder", SshFolderAbsolutePath, WrongRights));
    if (SshFolderFile.get() == NULL)
    {
      TRights SshFolderRights;
      SshFolderRights.Number = TRights::rfUserRead | TRights::rfUserWrite | TRights::rfUserExec;
      TRemoteProperties SshFolderProperties;
      SshFolderProperties.Rights = SshFolderRights;
      SshFolderProperties.Valid = TValidProperties() << vpRights;

      LogEvent(FORMAT(L"Trying to create \"%s\" folder with permissions %s...", (OpensshFolderName, SshFolderRights.Octal)));
      CreateDirectory(SshFolderAbsolutePath, &SshFolderProperties);
    }

    TemporaryDir = ExcludeTrailingBackslash(Configuration->TemporaryDir());
    if (!ForceDirectories(ApiPath(TemporaryDir)))
    {
      throw EOSExtException(FMTLOAD(CREATE_TEMP_DIR_ERROR, (TemporaryDir)));
    }
    UnicodeString TemporaryAuthorizedKeysFile = IncludeTrailingBackslash(TemporaryDir) + OpensshAuthorizedKeysFileName;

    UnicodeString AuthorizedKeysFileAbsolutePath = UnixIncludeTrailingBackslash(SshFolderAbsolutePath) + OpensshAuthorizedKeysFileName;

    bool Updated = true;
    TCopyParamType CopyParam; // Use factory defaults
    CopyParam.ResumeSupport = rsOff; // not to break the permissions
    CopyParam.PreserveTime = false; // not needed

    UnicodeString AuthorizedKeys;
    std::unique_ptr<TRemoteFile> AuthorizedKeysFileFile(CheckRights(L"File", AuthorizedKeysFileAbsolutePath, WrongRights));
    if (AuthorizedKeysFileFile.get() != NULL)
    {
      AuthorizedKeysFileFile->FullFileName = AuthorizedKeysFileAbsolutePath;
      std::unique_ptr<TStrings> Files(new TStringList());
      Files->AddObject(AuthorizedKeysFileAbsolutePath, AuthorizedKeysFileFile.get());
      LogAndInformation(FMTLOAD(PUBLIC_KEY_DOWNLOADING, (OpensshAuthorizedKeysFileName)));
      CopyToLocal(Files.get(), TemporaryDir, &CopyParam, cpNoConfirmation, NULL);
      // Overload with Encoding parameter work incorrectly, when used on a file without BOM
      AuthorizedKeys = TFile::ReadAllText(TemporaryAuthorizedKeysFile);

      std::unique_ptr<TStrings> AuthorizedKeysLines(TextToStringList(AuthorizedKeys));
      int P = Line.Pos(L" ");
      if (DebugAlwaysTrue(P > 0))
      {
        P = PosEx(L" ", Line, P + 1);
      }
      UnicodeString Prefix = Line.SubString(1, P); // including the space
      for (int Index = 0; Index < AuthorizedKeysLines->Count; Index++)
      {
        if (StartsStr(Prefix, AuthorizedKeysLines->Strings[Index]))
        {
          LogAndInformation(FMTLOAD(PUBLIC_KEY_CONTAINS, (OpensshAuthorizedKeysFileName)) + L"\n" + AuthorizedKeysLines->Strings[Index]);
          Updated = false;
        }
      }

      if (Updated)
      {
        LogAndInformation(FMTLOAD(PUBLIC_KEY_NOT_CONTAINS, (OpensshAuthorizedKeysFileName)));
        if (!EndsStr(L"\n", AuthorizedKeys))
        {
          LogEvent(FORMAT(L"Adding missing trailing new line to \"%s\" file...", (OpensshAuthorizedKeysFileName)));
          AuthorizedKeys += L"\n";
        }
      }
    }
    else
    {
      LogAndInformation(FMTLOAD(PUBLIC_KEY_NEW, (OpensshAuthorizedKeysFileName)));
      CopyParam.PreserveRights = true;
      CopyParam.Rights.Number = TRights::rfUserRead | TRights::rfUserWrite;
    }

    if (Updated)
    {
      AuthorizedKeys += Line + L"\n";
      // Overload without Encoding parameter uses TEncoding::UTF8, but does not write BOM, what we want
      TFile::WriteAllText(TemporaryAuthorizedKeysFile, AuthorizedKeys);
      std::unique_ptr<TStrings> Files(new TStringList());
      Files->Add(TemporaryAuthorizedKeysFile);
      LogAndInformation(FMTLOAD(PUBLIC_KEY_UPLOADING, (OpensshAuthorizedKeysFileName)));
      CopyToRemote(Files.get(), SshFolderAbsolutePath, &CopyParam, cpNoConfirmation, NULL);
    }

    Result = FMTLOAD(PUBLIC_KEY_UPLOADED, (Comment));
    if (Updated)
    {
      LogAndInformation(ReplaceStr(RemoveMainInstructionsTag(Result), L"\n\n", L"\n"));
    }
    if (WrongRights)
    {
      UnicodeString PermissionsInfo = FMTLOAD(PUBLIC_KEY_PERMISSIONS, (AuthorizedKeysFilePath));
      LogAndInformation(PermissionsInfo);
      Result += L"\n\n" + PermissionsInfo;
    }
  }
  __finally
  {
    AutoReadDirectory = PrevAutoReadDirectory;
    ExceptionOnFail = PrevExceptionOnFail;
    if (!TemporaryDir.IsEmpty())
    {
      RecursiveDeleteFile(ExcludeTrailingBackslash(TemporaryDir));
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool TTerminal::IsValidFile(TRemoteFile * File)
{
  return
    !File->FileName.IsEmpty() &&
    (IsUnixRootPath(File->FileName) || UnixExtractFileDir(File->FileName).IsEmpty());
}
//---------------------------------------------------------------------------
TStrings * TTerminal::ProcessFeatures(TStrings * Features)
{
  return ::ProcessFeatures(Features, SessionData->ProtocolFeatures.Trim());
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TSecondaryTerminal::TSecondaryTerminal(
  TTerminal * MainTerminal, TSessionData * ASessionData, TConfiguration * Configuration,
  const UnicodeString & Name, TActionLog * ActionLog) :
  TTerminal(ASessionData, Configuration, ActionLog),
  FMainTerminal(MainTerminal)
{
  Log->SetParent(FMainTerminal->Log, Name);
  SessionData->NonPersistent();
  DebugAssert(FMainTerminal != NULL);
  FMainTerminal->FSecondaryTerminals++;
  if (SessionData->TunnelLocalPortNumber != 0)
  {
    SessionData->TunnelLocalPortNumber = SessionData->TunnelLocalPortNumber + FMainTerminal->FSecondaryTerminals;
  }
  if (!FMainTerminal->UserName.IsEmpty())
  {
    SessionData->UserName = FMainTerminal->UserName;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecondaryTerminal::UpdateFromMain()
{
  if ((FFileSystem != NULL) && (FMainTerminal->FFileSystem != NULL))
  {
    FFileSystem->UpdateFromMain(FMainTerminal->FFileSystem);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecondaryTerminal::DirectoryLoaded(TRemoteFileList * FileList)
{
  FMainTerminal->DirectoryLoaded(FileList);
  DebugAssert(FileList != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TSecondaryTerminal::DirectoryModified(const UnicodeString Path,
  bool SubDirs)
{
  // clear cache of main terminal
  FMainTerminal->DirectoryModified(Path, SubDirs);
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TSecondaryTerminal::GetPrimaryTerminal()
{
  return FMainTerminal;
}
//---------------------------------------------------------------------------
__fastcall TTerminalList::TTerminalList(TConfiguration * AConfiguration) :
  TObjectList()
{
  DebugAssert(AConfiguration);
  FConfiguration = AConfiguration;
}
//---------------------------------------------------------------------------
__fastcall TTerminalList::~TTerminalList()
{
  DebugAssert(Count == 0);
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminalList::CreateTerminal(TSessionData * Data)
{
  return new TTerminal(Data, FConfiguration);
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminalList::NewTerminal(TSessionData * Data)
{
  TTerminal * Terminal = CreateTerminal(Data);
  Add(Terminal);
  return Terminal;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalList::FreeTerminal(TTerminal * Terminal)
{
  DebugAssert(IndexOf(Terminal) >= 0);
  Remove(Terminal);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalList::FreeAndNullTerminal(TTerminal * & Terminal)
{
  TTerminal * T = Terminal;
  Terminal = NULL;
  FreeTerminal(T);
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminalList::GetTerminal(int Index)
{
  return dynamic_cast<TTerminal *>(Items[Index]);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalList::RecryptPasswords()
{
  for (int Index = 0; Index < Count; Index++)
  {
    Terminals[Index]->RecryptPasswords();
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TLocalFileHandle::TLocalFileHandle()
{
  Handle = 0;
  Attrs = 0;
  MTime = 0;
  ATime = 0;
  Size = 0;
  Directory = false;
}
//---------------------------------------------------------------------------
TLocalFileHandle::~TLocalFileHandle()
{
  Release();
}
//---------------------------------------------------------------------------
void TLocalFileHandle::Release()
{
  if (Handle != 0)
  {
    Close();
  }
}
//---------------------------------------------------------------------------
void TLocalFileHandle::Dismiss()
{
  if (DebugAlwaysTrue(Handle != 0))
  {
    Handle = 0;
  }
}
//---------------------------------------------------------------------------
void TLocalFileHandle::Close()
{
  if (DebugAlwaysTrue(Handle != 0))
  {
    CloseHandle(Handle);
    Dismiss();
  }
}
