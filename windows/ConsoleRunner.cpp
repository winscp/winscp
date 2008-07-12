//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Exceptions.h>
#include <Script.h>
#include <CoreMain.h>
#include <Terminal.h>
#include <PuttyTools.h>

#include <Consts.hpp>

#include "Console.h"
#include "WinInterface.h"
#include "ProgParams.h"
#include "TextsWin.h"
#include "TextsCore.h"
#include "CustomWinConfiguration.h"
#include "SynchronizeController.h"
#include "GUITools.h"
enum { RESULT_SUCCESS = 0, RESULT_ANY_ERROR = 1 };
//---------------------------------------------------------------------------
#define WM_INTERUPT_IDLE (WM_WINSCP_USER + 3)
#define BATCH_INPUT_TIMEOUT 10000
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
class TConsole
{
public:
  virtual __fastcall ~TConsole() {};
  virtual void __fastcall Print(AnsiString Str, bool FromBeginning = false) = 0;
  virtual bool __fastcall Input(AnsiString & Str, bool Echo, unsigned int Timer) = 0;
  virtual int __fastcall Choice(AnsiString Options, int Cancel, int Break,
    int Timeouted, unsigned int Timer) = 0;
  virtual bool __fastcall PendingAbort() = 0;
  virtual void __fastcall SetTitle(AnsiString Title) = 0;
  virtual bool __fastcall LimitedOutput() = 0;
  virtual void __fastcall WaitBeforeExit() = 0;
};
//---------------------------------------------------------------------------
class TOwnConsole : public TConsole
{
public:
  static TOwnConsole * __fastcall Instance();

  virtual void __fastcall Print(AnsiString Str, bool FromBeginning = false);
  virtual bool __fastcall Input(AnsiString & Str, bool Echo, unsigned int Timer);
  virtual int __fastcall Choice(AnsiString Options, int Cancel, int Break,
    int Timeouted, unsigned int Timer);
  virtual bool __fastcall PendingAbort();
  virtual void __fastcall SetTitle(AnsiString Title);
  virtual bool __fastcall LimitedOutput();
  virtual void __fastcall WaitBeforeExit();

protected:
  static TOwnConsole * FInstance;

  __fastcall TOwnConsole();
  virtual __fastcall ~TOwnConsole();

  void __fastcall BreakInput();
  static BOOL WINAPI HandlerRoutine(DWORD CtrlType);
  static int __fastcall InputTimerThreadProc(void * Parameter);

private:
  HANDLE FInput;
  HANDLE FOutput;
  HANDLE FInputTimerEvent;
  static TCriticalSection FSection;

  bool FPendingAbort;
};
//---------------------------------------------------------------------------
TOwnConsole * TOwnConsole::FInstance = NULL;
TCriticalSection TOwnConsole::FSection;
//---------------------------------------------------------------------------
__fastcall TOwnConsole::TOwnConsole()
{
  assert(FInstance == NULL);
  FInstance = this;

  AllocConsole();
  SetConsoleCtrlHandler(HandlerRoutine, true);

  FInput = GetStdHandle(STD_INPUT_HANDLE);
  FOutput = GetStdHandle(STD_OUTPUT_HANDLE);
  FInputTimerEvent = NULL;
  FPendingAbort = false;
}
//---------------------------------------------------------------------------
__fastcall TOwnConsole::~TOwnConsole()
{
  TGuard Guard(&FSection);

  // deliberatelly do not remove ConsoleCtrlHandler as it causes
  // failures while exiting

  FreeConsole();

  assert(FInstance == this);
  FInstance = NULL;
}
//---------------------------------------------------------------------------
TOwnConsole * __fastcall TOwnConsole::Instance()
{
  return new TOwnConsole();
}
//---------------------------------------------------------------------------
void __fastcall TOwnConsole::BreakInput()
{
  FlushConsoleInputBuffer(FInput);
  INPUT_RECORD InputRecord;
  memset(&InputRecord, 0, sizeof(InputRecord));
  InputRecord.EventType = KEY_EVENT;
  InputRecord.Event.KeyEvent.bKeyDown = true;
  InputRecord.Event.KeyEvent.wRepeatCount = 1;
  InputRecord.Event.KeyEvent.uChar.AsciiChar = '\r';

  unsigned long Written;
  // this assertion occasionally fails (when console is being exited)
  CHECK(WriteConsoleInput(FInput, &InputRecord, 1, &Written));
  assert(Written == 1);

  FPendingAbort = true;

  PostMessage(Application->Handle, WM_INTERUPT_IDLE, 0, 0);
}
//---------------------------------------------------------------------------
BOOL WINAPI TOwnConsole::HandlerRoutine(DWORD CtrlType)
{
  if ((CtrlType == CTRL_C_EVENT) || (CtrlType == CTRL_BREAK_EVENT))
  {
    {
      TGuard Guard(&FSection);

      // just to be real thread-safe
      if (FInstance != NULL)
      {
        FInstance->BreakInput();
      }
    }

    return true;
  }
  else
  {
    return false;
  }
}
//---------------------------------------------------------------------------
int __fastcall TOwnConsole::InputTimerThreadProc(void * Parameter)
{
  assert(FInstance != NULL);
  unsigned int Timer = reinterpret_cast<unsigned int>(Parameter);

  if (WaitForSingleObject(FInstance->FInputTimerEvent, Timer) == WAIT_TIMEOUT)
  {
    FInstance->BreakInput();
  }

  return 0;
}
//---------------------------------------------------------------------------
bool __fastcall TOwnConsole::PendingAbort()
{
  if (FPendingAbort)
  {
    FPendingAbort = false;
    return true;
  }
  else
  {
    return FPendingAbort;
  }
}
//---------------------------------------------------------------------------
void __fastcall TOwnConsole::Print(AnsiString Str, bool FromBeginning)
{
  if (FromBeginning)
  {
    CONSOLE_SCREEN_BUFFER_INFO BufferInfo;
    GetConsoleScreenBufferInfo(FOutput, &BufferInfo);
    BufferInfo.dwCursorPosition.X = 0;
    SetConsoleCursorPosition(FOutput, BufferInfo.dwCursorPosition);
  }
  unsigned long Written;
  AnsiToOem(Str);
  bool Result = WriteConsole(FOutput, Str.c_str(), Str.Length(), &Written, NULL);
  assert(Result);
  USEDPARAM(Result);
  assert(Str.Length() == static_cast<long>(Written));
}
//---------------------------------------------------------------------------
bool __fastcall TOwnConsole::Input(AnsiString & Str, bool Echo, unsigned int Timer)
{
  unsigned long PrevMode, NewMode;
  GetConsoleMode(FInput, &PrevMode);
  NewMode = PrevMode | ENABLE_PROCESSED_INPUT | ENABLE_LINE_INPUT;
  if (Echo)
  {
    NewMode |= ENABLE_ECHO_INPUT;
  }
  else
  {
    NewMode &= ~ENABLE_ECHO_INPUT;
  }
  SetConsoleMode(FInput, NewMode);

  HANDLE InputTimerThread = NULL;

  bool Result;

  try
  {
    if (Timer > 0)
    {
      unsigned int ThreadId;
      assert(FInputTimerEvent == NULL);
      FInputTimerEvent = CreateEvent(NULL, false, false, NULL);
      InputTimerThread = (HANDLE)BeginThread(NULL, 0, InputTimerThreadProc,
        reinterpret_cast<void *>(Timer), 0, ThreadId);
    }

    unsigned long Read;
    Str.SetLength(10240);
    Result = ReadConsole(FInput, Str.c_str(), Str.Length(), &Read, NULL);
    assert(Result);
    Str.SetLength(Read);
    OemToAnsi(Str);

    if (FPendingAbort || !Echo)
    {
      Print("\n");
    }

    if (FPendingAbort || (Read == 0))
    {
      Result = false;
      FPendingAbort = false;
    }
  }
  __finally
  {
    if (InputTimerThread != NULL)
    {
      SetEvent(FInputTimerEvent);
      WaitForSingleObject(InputTimerThread, 100);
      CloseHandle(FInputTimerEvent);
      FInputTimerEvent = NULL;
      CloseHandle(InputTimerThread);
    }
    SetConsoleMode(FInput, PrevMode);
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TOwnConsole::Choice(AnsiString Options, int Cancel, int Break,
  int Timeouted, unsigned int Timer)
{
  AnsiToOem(Options);

  unsigned int ATimer = Timer;
  int Result = 0;
  unsigned long PrevMode, NewMode;
  GetConsoleMode(FInput, &PrevMode);
  NewMode = (PrevMode | ENABLE_PROCESSED_INPUT) & ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT);
  SetConsoleMode(FInput, NewMode);

  try
  {
    do
    {
      unsigned long Read;
      INPUT_RECORD Record;
      if ((PeekConsoleInput(FInput, &Record, 1, &Read) != 0) &&
          (Read == 1))
      {
        if ((ReadConsoleInput(FInput, &Record, 1, &Read) != 0) &&
            (Read == 1))
        {
          if (PendingAbort())
          {
            Result = Break;
          }
          else if ((Record.EventType == KEY_EVENT) &&
                   Record.Event.KeyEvent.bKeyDown)
          {
            char C = AnsiUpperCase(Record.Event.KeyEvent.uChar.AsciiChar)[1];
            if (C == 27)
            {
              Result = Cancel;
            }
            else if ((Options.Pos(C) > 0) &&
                     FLAGCLEAR(Record.Event.KeyEvent.dwControlKeyState,
                       LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED | LEFT_ALT_PRESSED  |
                       RIGHT_CTRL_PRESSED))

            {
              Result = Options.Pos(C);
            }
          }
        }
      }

      if (Result == 0)
      {
        unsigned int TimerSlice = 50;
        Sleep(TimerSlice);
        if (Timer > 0)
        {
          if (ATimer > TimerSlice)
          {
            ATimer -= TimerSlice;
          }
          else
          {
            Result = Timeouted;
          }
        }
      }
    }
    while (Result == 0);
  }
  __finally
  {
    SetConsoleMode(FInput, PrevMode);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TOwnConsole::SetTitle(AnsiString Title)
{
  AnsiToOem(Title);
  SetConsoleTitle(Title.c_str());
}
//---------------------------------------------------------------------------
bool __fastcall TOwnConsole::LimitedOutput()
{
  return true;
}
//---------------------------------------------------------------------------
void __fastcall TOwnConsole::WaitBeforeExit()
{
  unsigned long Read;
  INPUT_RECORD Record;
  while (ReadConsoleInput(FInput, &Record, 1, &Read))
  {
    if ((Read == 1) && (Record.EventType == KEY_EVENT) &&
        (Record.Event.KeyEvent.uChar.AsciiChar != 0) &&
        Record.Event.KeyEvent.bKeyDown)
    {
      break;
    }
  }
}
//---------------------------------------------------------------------------
class TExternalConsole : public TConsole
{
public:
  __fastcall TExternalConsole(const AnsiString Instance);
  virtual __fastcall ~TExternalConsole();

  virtual void __fastcall Print(AnsiString Str, bool FromBeginning = false);
  virtual bool __fastcall Input(AnsiString & Str, bool Echo, unsigned int Timer);
  virtual int __fastcall Choice(AnsiString Options, int Cancel, int Break,
    int Timeouted, unsigned int Timer);
  virtual bool __fastcall PendingAbort();
  virtual void __fastcall SetTitle(AnsiString Title);
  virtual bool __fastcall LimitedOutput();
  virtual void __fastcall WaitBeforeExit();

private:
  bool FPendingAbort;
  HANDLE FRequestEvent;
  HANDLE FResponseEvent;
  HANDLE FCancelEvent;
  HANDLE FFileMapping;
  bool FLimitedOutput;
  static const int PrintTimeout = 5000;

  inline TConsoleCommStruct * __fastcall GetCommStruct();
  inline void __fastcall FreeCommStruct(TConsoleCommStruct * CommStruct);
  inline void __fastcall SendEvent(int Timeout);
  void __fastcall Init();
};
//---------------------------------------------------------------------------
__fastcall TExternalConsole::TExternalConsole(const AnsiString Instance)
{
  FRequestEvent = OpenEvent(EVENT_ALL_ACCESS, false,
    FORMAT("%s%s", (CONSOLE_EVENT_REQUEST, (Instance))).c_str());
  FResponseEvent = OpenEvent(EVENT_ALL_ACCESS, false,
    FORMAT("%s%s", (CONSOLE_EVENT_RESPONSE, (Instance))).c_str());
  FCancelEvent = OpenEvent(EVENT_ALL_ACCESS, false,
    FORMAT("%s%s", (CONSOLE_EVENT_CANCEL, (Instance))).c_str());
  FFileMapping = OpenFileMapping(FILE_MAP_ALL_ACCESS, false,
    FORMAT("%s%s", (CONSOLE_MAPPING, (Instance))).c_str());

  if ((FRequestEvent == NULL) || (FResponseEvent == NULL) || (FFileMapping == NULL))
  {
    throw Exception(LoadStr(EXTERNAL_CONSOLE_INIT_ERROR));
  }

  HANDLE Kernel32 = GetModuleHandle(kernel32);
  typedef HANDLE WINAPI (* TOpenJobObject)(DWORD DesiredAccess, BOOL InheritHandles, LPCTSTR Name);
  typedef HANDLE WINAPI (* TAssignProcessToJobObject)(HANDLE Job, HANDLE Process);
  TOpenJobObject OpenJobObject =
    (TOpenJobObject)GetProcAddress(Kernel32, "OpenJobObjectA");
  TAssignProcessToJobObject AssignProcessToJobObject =
    (TAssignProcessToJobObject)GetProcAddress(Kernel32, "AssignProcessToJobObject");
  if ((OpenJobObject != NULL) && (AssignProcessToJobObject != NULL))
  {
    HANDLE Job = OpenJobObject(JOB_OBJECT_ASSIGN_PROCESS, FALSE,
      FORMAT("%s%s", (CONSOLE_JOB, Instance)).c_str());
    if (Job != NULL)
    {
      AssignProcessToJobObject(Job, GetCurrentProcess());
      // winscp.com keeps the only reference to the job.
      // once it gets closed (because winscp.com if forcefully terminated),
      // we get terminated as well
      CloseHandle(Job);
    }
  }

  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    if (CommStruct->Version != TConsoleCommStruct::CurrentVersion)
    {
      throw Exception(FMTLOAD(EXTERNAL_CONSOLE_INCOMPATIBLE, (CommStruct->CurrentVersion)));
    }

    CommStruct->Version = TConsoleCommStruct::CurrentVersionConfirmed;
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }

  // to break application event loop regularly during "watching for changes"
  // to allow user to abort it
  SetTimer(Application->Handle, 1, 500, NULL);

  Init();
}
//---------------------------------------------------------------------------
__fastcall TExternalConsole::~TExternalConsole()
{
  CloseHandle(FRequestEvent);
  CloseHandle(FResponseEvent);
  CloseHandle(FCancelEvent);
  CloseHandle(FFileMapping);
  KillTimer(Application->Handle, 1);
}
//---------------------------------------------------------------------------
TConsoleCommStruct * __fastcall TExternalConsole::GetCommStruct()
{
  TConsoleCommStruct * Result;
  Result = static_cast<TConsoleCommStruct*>(MapViewOfFile(FFileMapping,
    FILE_MAP_ALL_ACCESS, 0, 0, 0));
  if (Result == NULL)
  {
    throw Exception(LoadStr(CONSOLE_COMM_ERROR));
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TExternalConsole::FreeCommStruct(TConsoleCommStruct * CommStruct)
{
  UnmapViewOfFile(CommStruct);
}
//---------------------------------------------------------------------------
void __fastcall TExternalConsole::SendEvent(int Timeout)
{
  SetEvent(FRequestEvent);
  if (WaitForSingleObject(FResponseEvent, Timeout) != WAIT_OBJECT_0)
  {
    throw Exception(LoadStr(CONSOLE_SEND_TIMEOUT));
  }
}
//---------------------------------------------------------------------------
void __fastcall TExternalConsole::Print(AnsiString Str, bool FromBeginning)
{
  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    if (Str.Length() >= sizeof(CommStruct->PrintEvent.Message))
    {
      throw Exception(FMTLOAD(CONSOLE_PRINT_TOO_LONG, (Str.Length())));
    }

    CommStruct->Event = TConsoleCommStruct::PRINT;
    CharToOem(Str.c_str(), CommStruct->PrintEvent.Message);
    CommStruct->PrintEvent.FromBeginning = FromBeginning;
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }

  SendEvent(PrintTimeout);
}
//---------------------------------------------------------------------------
bool __fastcall TExternalConsole::Input(AnsiString & Str, bool Echo, unsigned int Timer)
{
  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    CommStruct->Event = TConsoleCommStruct::INPUT;
    CommStruct->InputEvent.Echo = Echo;
    CommStruct->InputEvent.Result = false;
    CommStruct->InputEvent.Str[0] = '\0';
    CommStruct->InputEvent.Timer = Timer;
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }

  SendEvent(INFINITE);

  bool Result;
  CommStruct = GetCommStruct();
  try
  {
    Result = CommStruct->InputEvent.Result;
    Str.SetLength(strlen(CommStruct->InputEvent.Str));
    OemToChar(CommStruct->InputEvent.Str, Str.c_str());
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TExternalConsole::Choice(AnsiString Options, int Cancel, int Break,
  int Timeouted, unsigned int Timer)
{
  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    CommStruct->Event = TConsoleCommStruct::CHOICE;

    assert(Options.Length() < sizeof(CommStruct->ChoiceEvent.Options));
    CharToOem(Options.c_str(), CommStruct->ChoiceEvent.Options);
    CommStruct->ChoiceEvent.Cancel = Cancel;
    CommStruct->ChoiceEvent.Break = Break;
    CommStruct->ChoiceEvent.Result = Break;
    CommStruct->ChoiceEvent.Timeouted = Timeouted;
    CommStruct->ChoiceEvent.Timer = Timer;
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }

  SendEvent(INFINITE);

  int Result;
  CommStruct = GetCommStruct();
  try
  {
    Result = CommStruct->ChoiceEvent.Result;
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TExternalConsole::PendingAbort()
{
  return (WaitForSingleObject(FCancelEvent, 0) == WAIT_OBJECT_0);
}
//---------------------------------------------------------------------------
void __fastcall TExternalConsole::SetTitle(AnsiString Title)
{
  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    if (Title.Length() >= sizeof(CommStruct->TitleEvent.Title))
    {
      throw Exception(FMTLOAD(CONSOLE_PRINT_TOO_LONG, (Title.Length())));
    }

    CommStruct->Event = TConsoleCommStruct::TITLE;
    CharToOem(Title.c_str(), CommStruct->TitleEvent.Title);
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }

  SendEvent(INFINITE);
}
//---------------------------------------------------------------------------
void __fastcall TExternalConsole::Init()
{
  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    CommStruct->Event = TConsoleCommStruct::INIT;
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }
  SendEvent(INFINITE);

  CommStruct = GetCommStruct();
  try
  {
    FLimitedOutput = (CommStruct->InitEvent.OutputType == FILE_TYPE_CHAR);
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TExternalConsole::LimitedOutput()
{
  return FLimitedOutput;
}
//---------------------------------------------------------------------------
void __fastcall TExternalConsole::WaitBeforeExit()
{
  // noop
}
//---------------------------------------------------------------------------
class TNullConsole : public TConsole
{
public:
  __fastcall TNullConsole();

  virtual void __fastcall Print(AnsiString Str, bool FromBeginning = false);
  virtual bool __fastcall Input(AnsiString & Str, bool Echo, unsigned int Timer);
  virtual int __fastcall Choice(AnsiString Options, int Cancel, int Break,
    int Timeouted, unsigned int Timer);
  virtual bool __fastcall PendingAbort();
  virtual void __fastcall SetTitle(AnsiString Title);
  virtual bool __fastcall LimitedOutput();
  virtual void __fastcall WaitBeforeExit();
};
//---------------------------------------------------------------------------
__fastcall TNullConsole::TNullConsole()
{
}
//---------------------------------------------------------------------------
void __fastcall TNullConsole::Print(AnsiString /*Str*/, bool /*FromBeginning*/)
{
  // noop
}
//---------------------------------------------------------------------------
bool __fastcall TNullConsole::Input(AnsiString & /*Str*/, bool /*Echo*/,
  unsigned int /*Timer*/)
{
  return false;
}
//---------------------------------------------------------------------------
int __fastcall TNullConsole::Choice(AnsiString /*Options*/, int /*Cancel*/,
  int Break, int /*Timeouted*/, unsigned int /*Timer*/)
{
  return Break;
}
//---------------------------------------------------------------------------
bool __fastcall TNullConsole::PendingAbort()
{
  return false;
}
//---------------------------------------------------------------------------
void __fastcall TNullConsole::SetTitle(AnsiString /*Title*/)
{
  // noop
}
//---------------------------------------------------------------------------
bool __fastcall TNullConsole::LimitedOutput()
{
  return false;
}
//---------------------------------------------------------------------------
void __fastcall TNullConsole::WaitBeforeExit()
{
  assert(false);
  // noop
}
//---------------------------------------------------------------------------
class TConsoleRunner
{
public:
  TConsoleRunner(TConsole * Console);
  ~TConsoleRunner();

  int __fastcall Run(const AnsiString Session, TOptions * Options,
    TStrings * ScriptCommands);
  void __fastcall ShowException(Exception * E);

protected:
  bool __fastcall Input(AnsiString & Str, bool Echo, unsigned int Timer);
  inline void __fastcall Print(const AnsiString & Str, bool FromBeginning = false);
  inline void __fastcall PrintLine(const AnsiString & Str);
  inline void __fastcall PrintMessage(const AnsiString & Str);
  void __fastcall UpdateTitle();
  inline void __fastcall NotifyAbort();
  inline bool __fastcall Aborted(bool AllowCompleteAbort = true);

private:
  TManagementScript * FScript;
  TConsole * FConsole;
  TSynchronizeController FSynchronizeController;
  int FLastProgressLen;
  bool FSynchronizeAborted;
  bool FCommandError;
  bool FBatchScript;
  bool FAborted;
  TTimer * Timer;

  void __fastcall ScriptPrint(TScript * Script, const AnsiString Str);
  void __fastcall ScriptPrintProgress(TScript * Script, bool First, const AnsiString Str);
  void __fastcall ScriptInput(TScript * Script, const AnsiString Prompt, AnsiString & Str);
  void __fastcall ScriptTerminalPromptUser(TTerminal * Terminal,
    TPromptKind Kind, AnsiString Name, AnsiString Instructions, TStrings * Prompts,
    TStrings * Results, bool & Result, void * Arg);
  void __fastcall ScriptShowExtendedException(TTerminal * Terminal,
    Exception * E, void * Arg);
  void __fastcall ScriptTerminalQueryUser(TObject * Sender, const AnsiString Query,
    TStrings * MoreMessages, int Answers, const TQueryParams * Params, int & Answer,
    TQueryType QueryType, void * Arg);
  void __fastcall ScriptQueryCancel(TScript * Script, bool & Cancel);
  void __fastcall SynchronizeControllerAbort(TObject * Sender, bool Close);
  void __fastcall SynchronizeControllerLog(TSynchronizeController * Controller,
    TSynchronizeLogEntry Entry, const AnsiString Message);
  void __fastcall ScriptSynchronizeStartStop(TScript * Script,
    const AnsiString LocalDirectory, const AnsiString RemoteDirectory,
    const TCopyParamType & CopyParam, int SynchronizeParams);
  void __fastcall SynchronizeControllerSynchronize(TSynchronizeController * Sender,
    const AnsiString LocalDirectory, const AnsiString RemoteDirectory,
    const TCopyParamType & CopyParam, const TSynchronizeParamType & Params,
    TSynchronizeChecklist ** Checklist, TSynchronizeOptions * Options, bool Full);
  void __fastcall SynchronizeControllerSynchronizeInvalid(TSynchronizeController * Sender,
    const AnsiString Directory, const AnsiString ErrorStr);
  void __fastcall SynchronizeControllerTooManyDirectories(TSynchronizeController * Sender,
    int & MaxDirectories);
  unsigned int InputTimeout();
  void __fastcall TimerTimer(TObject * Sender);
};
//---------------------------------------------------------------------------
TConsoleRunner::TConsoleRunner(TConsole * Console) :
  FSynchronizeController(&SynchronizeControllerSynchronize,
    &SynchronizeControllerSynchronizeInvalid,
    &SynchronizeControllerTooManyDirectories)
{
  FConsole = Console;
  FLastProgressLen = 0;
  FScript = NULL;
  FAborted = false;
  FBatchScript = false;
  Timer = new TTimer(Application);
  Timer->OnTimer = TimerTimer;
  Timer->Interval = 1000;
  Timer->Enabled = true;
}
//---------------------------------------------------------------------------
TConsoleRunner::~TConsoleRunner()
{
  delete Timer;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::TimerTimer(TObject * /*Sender*/)
{
  // sole presence of timer causes message to be dispatched,
  // hence breaks the loops
}
//---------------------------------------------------------------------------
unsigned int TConsoleRunner::InputTimeout()
{
  return (FScript->Batch != TScript::BatchOff ? BATCH_INPUT_TIMEOUT : 0);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptInput(TScript * /*Script*/,
  const AnsiString Prompt, AnsiString & Str)
{
  Print(Prompt);

  if (!Input(Str, true, InputTimeout()))
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::Print(const AnsiString & Str, bool FromBeginning)
{
  if (FLastProgressLen > 0)
  {
    FConsole->Print("\n" + Str, FromBeginning);
    FLastProgressLen = 0;
  }
  else
  {
    FConsole->Print(Str, FromBeginning);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::PrintLine(const AnsiString & Str)
{
  Print(Str + "\n");
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::PrintMessage(const AnsiString & Str)
{
  PrintLine(
    StringReplace(StringReplace(Str.TrimRight(), "\n\n", "\n", TReplaceFlags() << rfReplaceAll),
      "\n \n", "\n", TReplaceFlags() << rfReplaceAll));
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::NotifyAbort()
{
  if (FBatchScript)
  {
    FAborted = true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TConsoleRunner::Aborted(bool AllowCompleteAbort)
{
  bool Result;
  if (FAborted)
  {
    Result = true;
  }
  else
  {
    Result = FConsole->PendingAbort();
    if (Result)
    {
      PrintLine(LoadStr(USER_TERMINATED));
      if (AllowCompleteAbort)
      {
        NotifyAbort();
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptPrint(TScript * /*Script*/,
  const AnsiString Str)
{
  Print(Str);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptPrintProgress(TScript * /*Script*/,
  bool First, const AnsiString Str)
{
  AnsiString S = Str;
  if (First && (FLastProgressLen > 0))
  {
    S = "\n" + S;
  }
  else if (S.Length() < FLastProgressLen)
  {
    int Padding = FLastProgressLen - S.Length();
    S += AnsiString::StringOfChar(' ', Padding) +
      AnsiString::StringOfChar('\b', Padding);
  }
  FConsole->Print(S, true);
  FLastProgressLen = Str.Length();
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptTerminalPromptUser(TTerminal * /*Terminal*/,
  TPromptKind /*Kind*/, AnsiString Name, AnsiString Instructions, TStrings * Prompts,
  TStrings * Results, bool & Result, void * /*Arg*/)
{
  if (!Instructions.IsEmpty())
  {
    PrintLine(Instructions);
  }

  for (int Index = 0; Index < Prompts->Count; Index++)
  {
    AnsiString Prompt = Prompts->Strings[Index];
    if (!Prompt.IsEmpty() && (Prompt[Prompt.Length()] != ' '))
    {
      Prompt += ' ';
    }
    int P = Prompt.Pos('&');
    if (P > 0)
    {
      Prompt.Delete(P, 1);
    }
    Print(Prompt);

    AnsiString AResult = Results->Strings[Index]; // useless
    Result = Input(AResult, bool(Prompts->Objects[Index]), InputTimeout());
    Results->Strings[Index] = AResult;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptShowExtendedException(
  TTerminal * /*Terminal*/, Exception * E, void * /*Arg*/)
{
  ShowException(E);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptTerminalQueryUser(TObject * /*Sender*/,
  const AnsiString Query, TStrings * MoreMessages, int Answers,
  const TQueryParams * Params, int & Answer, TQueryType /*QueryType*/,
  void * /*Arg*/)
{
  AnsiString AQuery = Query;
  unsigned int Timer = 0;
  unsigned int Timeout = 0;
  int TimeoutA = 0;

  if (Params != NULL)
  {
    if (Params->Timeout > 0)
    {
      assert(Params->Timer == 0);
      Timeout = Params->Timeout;
      TimeoutA = Params->TimeoutAnswer;
    }

    if (Params->Timer > 0)
    {
      assert(Params->Timeout == 0);
      Timer = Params->Timer;
      if (Params->TimerAnswers > 0)
      {
        Answers = Params->TimerAnswers;
      }
      if (!Params->TimerMessage.IsEmpty())
      {
        AQuery = Params->TimerMessage;
      }
    }

    if (FLAGSET(Params->Params, qpFatalAbort))
    {
      AQuery = FMTLOAD(WARN_FATAL_ERROR, (AQuery));
    }
  }

  int AAnswers = Answers;

  PrintMessage(AQuery);
  if ((MoreMessages != NULL) && (MoreMessages->Count > 0))
  {
    PrintMessage(MoreMessages->Text);
  }

  static const int MaxButtonCount = 15;
  int Buttons[MaxButtonCount];
  AnsiString Captions[MaxButtonCount];
  TNotifyEvent OnClicks[MaxButtonCount];
  int ButtonCount = 0;

  #define ADD_BUTTON(TYPE, CAPTION) \
    if (FLAGSET(AAnswers, qa ## TYPE)) \
    { \
      assert(ButtonCount < MaxButtonCount); \
      Captions[ButtonCount] = CAPTION; \
      Buttons[ButtonCount] = qa ## TYPE; \
      OnClicks[ButtonCount] = NULL; \
      ButtonCount++; \
      AAnswers -= qa ## TYPE; \
    }
  #define ADD_BUTTON_RES(TYPE) ADD_BUTTON(TYPE, LoadResourceString(&_SMsgDlg ## TYPE));
  ADD_BUTTON_RES(Yes);
  ADD_BUTTON_RES(No);
  ADD_BUTTON_RES(OK);
  ADD_BUTTON_RES(Cancel);
  ADD_BUTTON_RES(Abort);
  ADD_BUTTON_RES(Retry);
  ADD_BUTTON_RES(Ignore);
  // to keep the same order as for GUI message box
  ADD_BUTTON(Skip, LoadStr(SKIP_BUTTON));
  ADD_BUTTON_RES(All);
  ADD_BUTTON_RES(NoToAll);
  ADD_BUTTON_RES(YesToAll);
  ADD_BUTTON_RES(Help);
  #undef ADD_BUTTON_RES
  #undef ADD_BUTTON

  USEDPARAM(AAnswers);
  assert(AAnswers == 0);
  assert(ButtonCount > 0);

  if ((Params != NULL) && (Params->Aliases != NULL))
  {
    for (int bi = 0; bi < ButtonCount; bi++)
    {
      for (unsigned int ai = 0; ai < Params->AliasesCount; ai++)
      {
        if (static_cast<int>(Params->Aliases[ai].Button) == Buttons[bi])
        {
          Captions[bi] = Params->Aliases[ai].Alias;
          OnClicks[bi] = Params->Aliases[ai].OnClick;
          break;
        }
      }
    }
  }

  AnsiString Accels;
  for (int Index = 0; Index < ButtonCount; Index++)
  {
    AnsiString & Caption = Captions[Index];
    int P = Caption.Pos('&');
    if ((P > 0) && (P < Caption.Length()))
    {
      char Accel = AnsiUpperCase(Caption)[P + 1];
      if (Accels.Pos(Accel) > 0)
      {
        Caption.Delete(P, 1);
        Accels += ' ';
      }
      else
      {
        Accels += Accel;
      }
    }
    else
    {
      Accels += ' ';
    }
  }

  assert(Accels.Length() == ButtonCount);
  int NumberAccel = 0;
  int CancelA = CancelAnswer(Answers);
  int CancelIndex;
  int AbortA = AbortAnswer(Answers);
  int AbortIndex;
  int ContinueA = ContinueAnswer(Answers);
  int ContinueIndex;
  int TimeoutIndex = 0;

  for (int Index = 0; Index < ButtonCount; Index++)
  {
    AnsiString & Caption = Captions[Index];

    if (Accels[Index + 1] == ' ')
    {
      for (int Index2 = 1; Index2 <= Caption.Length(); Index2++)
      {
        char C = AnsiUpperCase(Caption)[Index2];
        if ((C >= 'A') && (C <= 'Z') && (Accels.Pos(C) == 0))
        {
          Caption.Insert("&", Index2);
          Accels[Index + 1] = C;
          break;
        }
      }
    }

    if (Accels[Index + 1] == ' ')
    {
      for (int Index2 = 1; Index2 <= Caption.Length(); Index2++)
      {
        char C = AnsiUpperCase(Caption)[Index2];
        if ((C != ' ') && (Accels.Pos(C) == 0))
        {
          Caption.Insert("&", Index2);
          Accels[Index + 1] = C;
          break;
        }
      }
    }

    if (Accels[Index + 1] == ' ')
    {
      NumberAccel++;
      assert(NumberAccel <= 9);
      Caption = FORMAT("&%d%s", (NumberAccel, Caption));
      Accels[Index + 1] = Caption[2];
    }

    if (Buttons[Index] == CancelA)
    {
      CancelIndex = Index + 1;
    }
    if (Buttons[Index] == AbortA)
    {
      AbortIndex = Index + 1;
    }
    if (Buttons[Index] == ContinueA)
    {
      ContinueIndex = Index + 1;
    }
    if (Buttons[Index] == ContinueA)
    {
      ContinueIndex = Index + 1;
    }
    if (Buttons[Index] == TimeoutA)
    {
      TimeoutIndex = Index + 1;
    }
  }

  assert(Accels.Pos(' ') == 0);

  bool Timeouting = (Timeout > 0);

  do
  {
    Answer = 0;
    int AnswerIndex;
    bool Retry;

    do
    {
      Retry = false;

      AnsiString Output;
      for (int i = 0; i < ButtonCount; i++)
      {
        if (i > 0)
        {
          Output += ", ";
        }

        AnsiString Caption = Captions[i];
        int P = Caption.Pos('&');
        assert(P >= 0);

        Caption[P] = '(';
        Caption.Insert(")", P + 2);

        if (i + 1 == TimeoutIndex)
        {
          assert(Timeouting);
          Caption = FMTLOAD(TIMEOUT_BUTTON, (Caption, int(Timeout / 1000)));
        }

        Output += Caption;
      }
      Output += ": ";

      // note that length of string may decrease over time due to number of
      // seconds length, but supposing it decreases by one character at time
      // at most, we do not mind as the prompt is terminated with space
      Print(Output, true);

      if (!Timeouting && (FScript->Batch == TScript::BatchContinue))
      {
        AnswerIndex = ContinueIndex;
      }
      else if (!Timeouting && (FScript->Batch != TScript::BatchOff))
      {
        AnswerIndex = AbortIndex;
      }
      else if (Timeouting && (Timeout < 1000))
      {
        AnswerIndex = TimeoutIndex;
      }
      else
      {
        AnswerIndex = FConsole->Choice(Accels, CancelIndex, -1, -2,
          (Timeouting ? 1000 : Timer));
        if (AnswerIndex == -1)
        {
          NotifyAbort();
          AnswerIndex = AbortIndex;
        }
        else if (AnswerIndex == -2)
        {
          if (Timeouting)
          {
            assert(Timeout >= 1000);
            Timeout -= 1000;
            Retry = true;
          }
          else
          {
            assert((Params != NULL) && (Params->TimerEvent != NULL));
            if ((Params != NULL) && (Params->TimerEvent != NULL))
            {
              unsigned int AAnswer = 0;
              Params->TimerEvent(AAnswer);
              if (AAnswer != 0)
              {
                Answer = AAnswer;
              }
              else
              {
                Retry = true;
              }
            }
          }
        }
      }
    }
    while (Retry);

    if (Answer == 0)
    {
      assert((AnswerIndex >= 1) && (AnswerIndex <= Accels.Length()));
      AnsiString AnswerCaption = Captions[AnswerIndex - 1];
      int P = AnswerCaption.Pos("&");
      assert(P >= 0);
      AnswerCaption.Delete(P, 1);
      PrintLine(AnswerCaption);

      if (OnClicks[AnswerIndex - 1] != NULL)
      {
        OnClicks[AnswerIndex - 1](NULL);
      }
      else
      {
        Answer = Buttons[AnswerIndex - 1];
      }
    }
    else
    {
      PrintLine("");
    }
  }
  while (Answer == 0);

  if (Answer == AbortA)
  {
    FCommandError = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptQueryCancel(TScript * /*Script*/, bool & Cancel)
{
  if (Aborted())
  {
    Cancel = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptSynchronizeStartStop(TScript * /*Script*/,
  const AnsiString LocalDirectory, const AnsiString RemoteDirectory,
  const TCopyParamType & CopyParam, int SynchronizeParams)
{
  TSynchronizeParamType Params;
  Params.LocalDirectory = LocalDirectory;
  Params.RemoteDirectory = RemoteDirectory;
  Params.Params = SynchronizeParams;
  Params.Options = soRecurse;

  FSynchronizeController.StartStop(Application, true, Params,
    CopyParam, NULL, SynchronizeControllerAbort, NULL,
    SynchronizeControllerLog);

  try
  {
    FSynchronizeAborted = false;

    while (!FSynchronizeAborted && !Aborted(false))
    {
      Application->HandleMessage();
      FScript->Terminal->Idle();
    }
  }
  __finally
  {
    FSynchronizeController.StartStop(Application, false, Params,
      CopyParam, NULL, SynchronizeControllerAbort, NULL,
      SynchronizeControllerLog);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::SynchronizeControllerLog(
  TSynchronizeController * /*Controller*/, TSynchronizeLogEntry /*Entry*/,
  const AnsiString Message)
{
  PrintLine(Message);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::SynchronizeControllerAbort(TObject * /*Sender*/,
  bool /*Close*/)
{
  FSynchronizeAborted = true;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::SynchronizeControllerSynchronize(
  TSynchronizeController * /*Sender*/, const AnsiString LocalDirectory,
  const AnsiString RemoteDirectory, const TCopyParamType & CopyParam,
  const TSynchronizeParamType & Params, TSynchronizeChecklist ** Checklist,
  TSynchronizeOptions * /*Options*/, bool Full)
{
  if (!Full)
  {
    FScript->Synchronize(LocalDirectory, RemoteDirectory, CopyParam,
      Params.Params, Checklist);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::SynchronizeControllerSynchronizeInvalid(
  TSynchronizeController * /*Sender*/, const AnsiString Directory, const AnsiString ErrorStr)
{
  if (!Directory.IsEmpty())
  {
    PrintLine(FMTLOAD(WATCH_ERROR_DIRECTORY, (Directory)));
  }
  else
  {
    PrintLine(LoadStr(WATCH_ERROR_GENERAL));
  }

  if (!ErrorStr.IsEmpty())
  {
    PrintLine(ErrorStr);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::SynchronizeControllerTooManyDirectories(
  TSynchronizeController * /*Sender*/, int & MaxDirectories)
{
  if (Aborted())
  {
    Abort();
  }

  if (MaxDirectories < GUIConfiguration->MaxWatchDirectories)
  {
    MaxDirectories = GUIConfiguration->MaxWatchDirectories;
  }
  else
  {
    MaxDirectories *= 2;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ShowException(Exception * E)
{
  if (!E->Message.IsEmpty() &&
      (dynamic_cast<EAbort *>(E) == NULL))
  {
    FCommandError = true;
    PrintMessage(TranslateExceptionMessage(E));
    ExtException * EE = dynamic_cast<ExtException *>(E);
    if ((EE != NULL) && (EE->MoreMessages != NULL))
    {
      PrintMessage(EE->MoreMessages->Text);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TConsoleRunner::Input(AnsiString & Str, bool Echo, unsigned int Timeout)
{
  bool Result = FConsole->Input(Str, Echo, Timeout);
  if (Result)
  {
    while (!Str.IsEmpty() &&
      ((Str[Str.Length()] == '\n') || (Str[Str.Length()] == '\r')))
    {
      Str.SetLength(Str.Length() - 1);
    }
  }
  else
  {
    NotifyAbort();
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TConsoleRunner::Run(const AnsiString Session, TOptions * Options,
  TStrings * ScriptCommands)
{
  bool AnyError = false;

  try
  {
    FScript = new TManagementScript(StoredSessions, FConsole->LimitedOutput());
    try
    {
      FScript->CopyParam = GUIConfiguration->DefaultCopyParam;
      FScript->SynchronizeParams = GUIConfiguration->SynchronizeParams;
      FScript->OnPrint = ScriptPrint;
      FScript->OnPrintProgress = ScriptPrintProgress;
      FScript->OnInput = ScriptInput;
      FScript->OnTerminalPromptUser = ScriptTerminalPromptUser;
      FScript->OnShowExtendedException = ScriptShowExtendedException;
      FScript->OnTerminalQueryUser = ScriptTerminalQueryUser;
      FScript->OnQueryCancel = ScriptQueryCancel;
      FScript->OnSynchronizeStartStop = ScriptSynchronizeStartStop;

      UpdateTitle();

      // everything until the first manually entered command is "batch"
      // (including opening session from command line and script file)
      FBatchScript = true;

      if (!Session.IsEmpty())
      {
        FScript->Connect(Session, Options, false);
      }

      int ScriptPos = 0;
      bool Result;
      do
      {
        UpdateTitle();

        AnsiString Command;
        if ((ScriptCommands != NULL) && (ScriptPos < ScriptCommands->Count))
        {
          Result = true;
          Command = ScriptCommands->Strings[ScriptPos];
          ScriptPos++;
        }
        else
        {
          // no longer batch
          FBatchScript = false;
          Print("winscp> ");
          Result = Input(Command, true, 0);
        }

        if (Result)
        {
          FCommandError = false;
          FScript->Command(ExpandEnvironmentVariables(Command));

          if (FCommandError)
          {
            AnyError = true;
            if (FScript->Batch == TScript::BatchAbort)
            {
              Result = false;
            }
          }

          if (FScript->Terminal != NULL)
          {
            FScript->Terminal->Idle();
          }
        }
      }
      while (Result && FScript->Continue && !Aborted());
    }
    __finally
    {
      delete FScript;
      FScript = NULL;
    }
  }
  catch(Exception & E)
  {
    ShowException(&E);
    AnyError = true;
  }

  return AnyError ? RESULT_ANY_ERROR : RESULT_SUCCESS;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::UpdateTitle()
{
  AnsiString NewTitle;
  if (FScript->Terminal != NULL)
  {
    NewTitle = FMTLOAD(APP_CAPTION, (FScript->Terminal->SessionData->SessionName, AppName));
  }
  else
  {
    NewTitle = AppName;
  }
  FConsole->SetTitle(NewTitle);
}
//---------------------------------------------------------------------------
void __fastcall LoadScriptFromFile(AnsiString FileName, TStrings * Lines)
{
  AnsiString UTFBOM = "\xEF\xBB\xBF";
  Lines->LoadFromFile(FileName);
  if ((Lines->Count > 0) &&
      (Lines->Strings[0].SubString(1, UTFBOM.Length()) == UTFBOM))
  {
    Lines->Strings[0] = Lines->Strings[0].SubString(
      UTFBOM.Length() + 1, Lines->Strings[0].Length() - UTFBOM.Length());
    for (int Index = 0; Index < Lines->Count; Index++)
    {
      Lines->Strings[Index] = DecodeUTF(Lines->Strings[Index]);
    }
  }
}
//---------------------------------------------------------------------------
int __fastcall Console(bool Help)
{
  TProgramParams * Params = TProgramParams::Instance();
  int Result = 0;
  TConsole * Console = NULL;
  TConsoleRunner * Runner = NULL;
  TStrings * ScriptCommands = new TStringList();
  try
  {
    AnsiString ConsoleInstance;
    if (Params->FindSwitch("consoleinstance", ConsoleInstance))
    {
      Console = new TExternalConsole(ConsoleInstance);
    }
    else if (Params->FindSwitch("Console") || Help)
    {
      Console = TOwnConsole::Instance();
    }
    else
    {
      Console = new TNullConsole();
    }

    if (Help)
    {
      AnsiString Usage = LoadStr(USAGE4, 10240);
      AnsiString ExeBaseName = ChangeFileExt(ExtractFileName(Application->ExeName), "");
      Usage = StringReplace(Usage, "%APP%", ExeBaseName,
        TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
      AnsiString Copyright = StringReplace(LoadStr(WINSCP_COPYRIGHT), "©", "(c)",
        TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
      Usage = FORMAT(Usage, (Configuration->VersionStr, Copyright));
      Console->Print(Usage);
      Console->WaitBeforeExit();
    }
    else
    {
      Runner = new TConsoleRunner(Console);

      try
      {
        AnsiString Value;
        if (Params->FindSwitch("script", Value) && !Value.IsEmpty())
        {
          LoadScriptFromFile(Value, ScriptCommands);
        }
        Params->FindSwitch("command", ScriptCommands);

        bool Url = false;
        AnsiString Session;
        if (Params->ParamCount >= 1)
        {
          Session = Params->Param[1];
        }

        bool DefaultsOnly;
        delete StoredSessions->ParseUrl(Session, Params, DefaultsOnly,
          puDecodeUrlChars, NULL, &Url);

        if (Url || Params->FindSwitch("Unsafe"))
        {
          // prevent any automatic action when URL is provided on
          // command-line (the check is duplicated in Execute())
          if ((ScriptCommands->Count > 0) || Params->FindSwitch("Log"))
          {
            Console->Print(LoadStr(UNSAFE_ACTIONS_DISABLED) + "\n");
          }
          ScriptCommands->Clear();
        }
        else
        {
          AnsiString LogFile;
          if (Params->FindSwitch("Log", LogFile))
          {
            Configuration->TemporaryLogging(LogFile);
          }
        }

        Result = Runner->Run(Session, Params,
          (ScriptCommands->Count > 0 ? ScriptCommands : NULL));
      }
      catch(Exception & E)
      {
        Runner->ShowException(&E);
        Result = RESULT_ANY_ERROR;
      }
    }
  }
  __finally
  {
    delete Runner;
    delete Console;
    delete ScriptCommands;
  }

  return Result;
}
