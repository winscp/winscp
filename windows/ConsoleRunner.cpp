//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Exceptions.h>
#include <Script.h>
#include <CoreMain.h>
#include <Terminal.h>
#include <PuttyTools.h>
#include <Queue.h>

#include <Consts.hpp>

#include "Console.h"
#include "WinInterface.h"
#include "ProgParams.h"
#include "TextsWin.h"
#include "TextsCore.h"
#include "WinConfiguration.h"
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
  virtual void __fastcall Print(UnicodeString Str, bool FromBeginning = false) = 0;
  virtual bool __fastcall Input(UnicodeString & Str, bool Echo, unsigned int Timer) = 0;
  virtual int __fastcall Choice(UnicodeString Options, int Cancel, int Break,
    int Timeouted, bool Timeouting, unsigned int Timer) = 0;
  virtual bool __fastcall PendingAbort() = 0;
  virtual void __fastcall SetTitle(UnicodeString Title) = 0;
  virtual bool __fastcall LimitedOutput() = 0;
  virtual bool __fastcall LiveOutput() = 0;
  virtual bool __fastcall NoInteractiveInput() = 0;
  virtual void __fastcall WaitBeforeExit() = 0;
  virtual bool __fastcall CommandLineOnly() = 0;
};
//---------------------------------------------------------------------------
class TOwnConsole : public TConsole
{
public:
  static TOwnConsole * __fastcall Instance();

  virtual void __fastcall Print(UnicodeString Str, bool FromBeginning = false);
  virtual bool __fastcall Input(UnicodeString & Str, bool Echo, unsigned int Timer);
  virtual int __fastcall Choice(UnicodeString Options, int Cancel, int Break,
    int Timeouted, bool Timeouting, unsigned int Timer);
  virtual bool __fastcall PendingAbort();
  virtual void __fastcall SetTitle(UnicodeString Title);
  virtual bool __fastcall LimitedOutput();
  virtual bool __fastcall LiveOutput();
  virtual bool __fastcall NoInteractiveInput();
  virtual void __fastcall WaitBeforeExit();
  virtual bool __fastcall CommandLineOnly();

protected:
  static TOwnConsole * FInstance;
  friend class TConsoleInputThread;

  __fastcall TOwnConsole();
  virtual __fastcall ~TOwnConsole();

  void __fastcall BreakInput();
  void __fastcall CancelInput();
  static BOOL WINAPI HandlerRoutine(DWORD CtrlType);
  void __fastcall WindowStateTimer(TObject * Sender);
  void __fastcall ProcessMessages();
  void __fastcall TrayIconClick(TObject * Sender);

private:
  HANDLE FInput;
  HANDLE FOutput;
  HWND FConsoleWindow;
  TTimer * FWindowStateTimer;
  bool FMinimized;
  ::TTrayIcon * FTrayIcon;
  static std::auto_ptr<TCriticalSection> FSection;

  bool FPendingAbort;
};
//---------------------------------------------------------------------------
TOwnConsole * TOwnConsole::FInstance = NULL;
std::auto_ptr<TCriticalSection> TOwnConsole::FSection(new TCriticalSection());
//---------------------------------------------------------------------------
__fastcall TOwnConsole::TOwnConsole()
{
  assert(FInstance == NULL);
  FInstance = this;

  AllocConsole();
  SetConsoleCtrlHandler(HandlerRoutine, true);

  FInput = GetStdHandle(STD_INPUT_HANDLE);
  FOutput = GetStdHandle(STD_OUTPUT_HANDLE);
  FPendingAbort = false;
  FConsoleWindow = NULL;
  FWindowStateTimer = NULL;
  FMinimized = false;
  FTrayIcon = new ::TTrayIcon(0);
  FTrayIcon->OnClick = TrayIconClick;

  if (WinConfiguration->MinimizeToTray)
  {
    HINSTANCE Kernel32 = GetModuleHandle(kernel32);

    typedef HWND WINAPI (* TGetConsoleWindow)();
    TGetConsoleWindow GetConsoleWindow =
      (TGetConsoleWindow)GetProcAddress(Kernel32, "GetConsoleWindow");
    if (GetConsoleWindow != NULL)
    {
      FConsoleWindow = GetConsoleWindow();
      if (FConsoleWindow != NULL)
      {
        FWindowStateTimer = new TTimer(Application);
        FWindowStateTimer->OnTimer = WindowStateTimer;
        FWindowStateTimer->Interval = 250;
        FWindowStateTimer->Enabled = true;
      }
      else
      {
        assert(false);
      }
    }
  }
}
//---------------------------------------------------------------------------
__fastcall TOwnConsole::~TOwnConsole()
{
  TGuard Guard(FSection.get());

  delete FTrayIcon;
  delete FWindowStateTimer;

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
void __fastcall TOwnConsole::WindowStateTimer(TObject * /*Sender*/)
{
  assert(FConsoleWindow != NULL);
  WINDOWPLACEMENT Placement;
  memset(&Placement, 0, sizeof(Placement));
  Placement.length = sizeof(Placement);
  if (GetWindowPlacement(FConsoleWindow, &Placement))
  {
    bool Minimized = (Placement.showCmd == SW_SHOWMINIMIZED);
    if (FMinimized != Minimized)
    {
      FMinimized = Minimized;

      if (FMinimized && WinConfiguration->MinimizeToTray)
      {
        FTrayIcon->Visible = true;
        ShowWindow(FConsoleWindow, SW_HIDE);
      }
      else
      {
        FTrayIcon->Visible = false;
        ShowWindow(FConsoleWindow, SW_SHOW);
      }
    }
  }
  else
  {
    assert(false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TOwnConsole::ProcessMessages()
{
  // as of now, there's no point doing this unless we have icon tray
  // (i.e. we need to monitor window state and eventually process tray icon messages)
  if (FWindowStateTimer != NULL)
  {
    assert(WinConfiguration->MinimizeToTray);

    Application->ProcessMessages();
  }
}
//---------------------------------------------------------------------------
void __fastcall TOwnConsole::TrayIconClick(TObject * /*Sender*/)
{
  assert(FConsoleWindow != NULL);
  SetForegroundWindow(FConsoleWindow);
  ShowWindow(FConsoleWindow, SW_RESTORE);
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
  InputRecord.Event.KeyEvent.uChar.UnicodeChar = L'\r';

  unsigned long Written;
  // this assertion occasionally fails (when console is being exited)
  CHECK(WriteConsoleInput(FInput, &InputRecord, 1, &Written));
  assert(Written == 1);

  CancelInput();
}
//---------------------------------------------------------------------------
void __fastcall TOwnConsole::CancelInput()
{
  FPendingAbort = true;

  PostMessage(Application->Handle, WM_INTERUPT_IDLE, 0, 0);
}
//---------------------------------------------------------------------------
BOOL WINAPI TOwnConsole::HandlerRoutine(DWORD CtrlType)
{
  if ((CtrlType == CTRL_C_EVENT) || (CtrlType == CTRL_BREAK_EVENT))
  {
    {
      TGuard Guard(FSection.get());

      // just to be real thread-safe
      if (FInstance != NULL)
      {
        FInstance->CancelInput();
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
void __fastcall TOwnConsole::Print(UnicodeString Str, bool FromBeginning)
{
  if (FromBeginning)
  {
    CONSOLE_SCREEN_BUFFER_INFO BufferInfo;
    GetConsoleScreenBufferInfo(FOutput, &BufferInfo);
    BufferInfo.dwCursorPosition.X = 0;
    SetConsoleCursorPosition(FOutput, BufferInfo.dwCursorPosition);
  }
  unsigned long Written;
  bool Result = WriteConsole(FOutput, Str.c_str(), Str.Length(), &Written, NULL);
  assert(Result);
  USEDPARAM(Result);
  assert(Str.Length() == static_cast<long>(Written));
  ProcessMessages();
}
//---------------------------------------------------------------------------
class TConsoleInputThread : public TSimpleThread
{
public:
  __fastcall TConsoleInputThread(HANDLE Input, UnicodeString & Str, bool & Result) :
    FInput(Input),
    FStr(Str),
    FResult(Result)
  {
  }

  virtual __fastcall ~TConsoleInputThread()
  {
    Close();
  }

protected:
  virtual void __fastcall Execute()
  {
    unsigned long Read;
    FStr.SetLength(10240);
    FResult = ReadConsole(FInput, FStr.c_str(), FStr.Length(), &Read, NULL);
    assert(FResult);
    FStr.SetLength(Read);
  }

  virtual void __fastcall Terminate()
  {
    TOwnConsole::FInstance->BreakInput();
  }

private:
  HANDLE FInput;
  UnicodeString & FStr;
  bool & FResult;
};
//---------------------------------------------------------------------------
bool __fastcall TOwnConsole::Input(UnicodeString & Str, bool Echo, unsigned int Timer)
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

  bool Result;

  try
  {
    {
      TConsoleInputThread InputThread(FInput, Str, Result);

      InputThread.Start();

      double Start = Now();
      double End = Start + double(Timer)/MSecsPerDay;
      while (!InputThread.IsFinished() &&
             ((Timer == 0) || (double(Now()) < End)))
      {
        ProcessMessages();
        InputThread.WaitFor(50);
      }
    }

    if (FPendingAbort || !Echo)
    {
      Print(L"\n");
    }

    if (FPendingAbort || (Str.Length() == 0))
    {
      Result = false;
      FPendingAbort = false;
    }
  }
  __finally
  {
    SetConsoleMode(FInput, PrevMode);
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TOwnConsole::Choice(UnicodeString Options, int Cancel, int Break,
  int Timeouted, bool /*Timeouting*/, unsigned int Timer)
{
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
            wchar_t C = AnsiUpperCase(Record.Event.KeyEvent.uChar.UnicodeChar)[1];
            if (C == 27)
            {
              Result = Cancel;
            }
            else if ((Options.Pos(C) > 0) &&
                     FLAGCLEAR(Record.Event.KeyEvent.dwControlKeyState,
                       LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED | LEFT_ALT_PRESSED  |
                       RIGHT_ALT_PRESSED))

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

      ProcessMessages();
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
void __fastcall TOwnConsole::SetTitle(UnicodeString Title)
{
  FTrayIcon->Hint = Title;
  SetConsoleTitle(Title.c_str());
}
//---------------------------------------------------------------------------
bool __fastcall TOwnConsole::LimitedOutput()
{
  return true;
}
//---------------------------------------------------------------------------
bool __fastcall TOwnConsole::LiveOutput()
{
  return true;
}
//---------------------------------------------------------------------------
bool __fastcall TOwnConsole::NoInteractiveInput()
{
  return false;
}
//---------------------------------------------------------------------------
void __fastcall TOwnConsole::WaitBeforeExit()
{
  unsigned long Read;
  INPUT_RECORD Record;
  while (true)
  {
    if (PeekConsoleInput(FInput, &Record, 1, &Read) && (Read == 1) &&
        ReadConsoleInput(FInput, &Record, 1, &Read) &&
        (Read == 1) && (Record.EventType == KEY_EVENT) &&
        (Record.Event.KeyEvent.uChar.UnicodeChar != 0) &&
        Record.Event.KeyEvent.bKeyDown)
    {
      break;
    }
    Sleep(50);
    ProcessMessages();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TOwnConsole::CommandLineOnly()
{
  return false;
}
//---------------------------------------------------------------------------
class TExternalConsole : public TConsole
{
public:
  __fastcall TExternalConsole(const UnicodeString Instance, bool NoInteractiveInput);
  virtual __fastcall ~TExternalConsole();

  virtual void __fastcall Print(UnicodeString Str, bool FromBeginning = false);
  virtual bool __fastcall Input(UnicodeString & Str, bool Echo, unsigned int Timer);
  virtual int __fastcall Choice(UnicodeString Options, int Cancel, int Break,
    int Timeouted, bool Timeouting, unsigned int Timer);
  virtual bool __fastcall PendingAbort();
  virtual void __fastcall SetTitle(UnicodeString Title);
  virtual bool __fastcall LimitedOutput();
  virtual bool __fastcall LiveOutput();
  virtual bool __fastcall NoInteractiveInput();
  virtual void __fastcall WaitBeforeExit();
  virtual bool __fastcall CommandLineOnly();

private:
  bool FPendingAbort;
  HANDLE FRequestEvent;
  HANDLE FResponseEvent;
  HANDLE FCancelEvent;
  HANDLE FFileMapping;
  bool FLimitedOutput;
  bool FLiveOutput;
  bool FPipeOutput;
  bool FNoInteractiveInput;
  static const int PrintTimeout = 30000;

  inline TConsoleCommStruct * __fastcall GetCommStruct();
  inline void __fastcall FreeCommStruct(TConsoleCommStruct * CommStruct);
  inline void __fastcall SendEvent(int Timeout);
  void __fastcall Init();
};
//---------------------------------------------------------------------------
__fastcall TExternalConsole::TExternalConsole(
  const UnicodeString Instance, bool NoInteractiveInput)
{
  FRequestEvent = OpenEvent(EVENT_ALL_ACCESS, false,
    FORMAT(L"%s%s", (CONSOLE_EVENT_REQUEST, (Instance))).c_str());
  FResponseEvent = OpenEvent(EVENT_ALL_ACCESS, false,
    FORMAT(L"%s%s", (CONSOLE_EVENT_RESPONSE, (Instance))).c_str());
  FCancelEvent = OpenEvent(EVENT_ALL_ACCESS, false,
    FORMAT(L"%s%s", (CONSOLE_EVENT_CANCEL, (Instance))).c_str());
  FFileMapping = OpenFileMapping(FILE_MAP_ALL_ACCESS, false,
    FORMAT(L"%s%s", (CONSOLE_MAPPING, (Instance))).c_str());

  if ((FRequestEvent == NULL) || (FResponseEvent == NULL) || (FFileMapping == NULL))
  {
    throw Exception(LoadStr(EXTERNAL_CONSOLE_INIT_ERROR));
  }

  HINSTANCE Kernel32 = GetModuleHandle(kernel32);
  typedef HANDLE WINAPI (* TOpenJobObject)(DWORD DesiredAccess, BOOL InheritHandles, LPCTSTR Name);
  typedef HANDLE WINAPI (* TAssignProcessToJobObject)(HANDLE Job, HANDLE Process);
  TOpenJobObject OpenJobObject =
    (TOpenJobObject)GetProcAddress(Kernel32, "OpenJobObjectW");
  TAssignProcessToJobObject AssignProcessToJobObject =
    (TAssignProcessToJobObject)GetProcAddress(Kernel32, "AssignProcessToJobObject");
  if ((OpenJobObject != NULL) && (AssignProcessToJobObject != NULL))
  {
    HANDLE Job = OpenJobObject(JOB_OBJECT_ASSIGN_PROCESS, FALSE,
      FORMAT(L"%s%s", (CONSOLE_JOB, Instance)).c_str());
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
      throw Exception(FMTLOAD(EXTERNAL_CONSOLE_INCOMPATIBLE, (CommStruct->Version)));
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

  FNoInteractiveInput = NoInteractiveInput;

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
  unsigned int Result = WaitForSingleObject(FResponseEvent, Timeout);
  if (Result != WAIT_OBJECT_0)
  {
    UnicodeString Message = LoadStr(CONSOLE_SEND_TIMEOUT);
    if (FPipeOutput)
    {
      Message = FORMAT("%s %s", (Message, LoadStr(CONSOLE_SEND_PIPE)));
    }
    throw Exception(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TExternalConsole::Print(UnicodeString Str, bool FromBeginning)
{
  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    if (Str.Length() >= static_cast<int>(LENOF(CommStruct->PrintEvent.Message)))
    {
      throw Exception(FMTLOAD(CONSOLE_PRINT_TOO_LONG, (Str.Length())));
    }

    CommStruct->Event = TConsoleCommStruct::PRINT;
    wcscpy(CommStruct->PrintEvent.Message, Str.c_str());
    CommStruct->PrintEvent.FromBeginning = FromBeginning;
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }

  SendEvent(PrintTimeout);
}
//---------------------------------------------------------------------------
bool __fastcall TExternalConsole::Input(UnicodeString & Str, bool Echo, unsigned int Timer)
{
  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    CommStruct->Event = TConsoleCommStruct::INPUT;
    CommStruct->InputEvent.Echo = Echo;
    CommStruct->InputEvent.Result = false;
    CommStruct->InputEvent.Str[0] = L'\0';
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
    Str = CommStruct->InputEvent.Str;
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TExternalConsole::Choice(UnicodeString Options, int Cancel, int Break,
  int Timeouted, bool Timeouting, unsigned int Timer)
{
  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    CommStruct->Event = TConsoleCommStruct::CHOICE;

    assert(Options.Length() < static_cast<int>(LENOF(CommStruct->ChoiceEvent.Options)));
    wcscpy(CommStruct->ChoiceEvent.Options, Options.c_str());
    CommStruct->ChoiceEvent.Cancel = Cancel;
    CommStruct->ChoiceEvent.Break = Break;
    CommStruct->ChoiceEvent.Result = Break;
    CommStruct->ChoiceEvent.Timeouted = Timeouted;
    CommStruct->ChoiceEvent.Timer = Timer;
    CommStruct->ChoiceEvent.Timeouting = Timeouting;
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
void __fastcall TExternalConsole::SetTitle(UnicodeString Title)
{
  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    if (Title.Length() >= static_cast<int>(LENOF(CommStruct->TitleEvent.Title)))
    {
      throw Exception(FMTLOAD(CONSOLE_PRINT_TOO_LONG, (Title.Length())));
    }

    CommStruct->Event = TConsoleCommStruct::TITLE;
    wcscpy(CommStruct->TitleEvent.Title, Title.c_str());
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
    FLiveOutput =
      (CommStruct->InitEvent.OutputType != FILE_TYPE_DISK) &&
      (CommStruct->InitEvent.OutputType != FILE_TYPE_PIPE);
    FPipeOutput = (CommStruct->InitEvent.OutputType != FILE_TYPE_PIPE);
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
bool __fastcall TExternalConsole::LiveOutput()
{
  return FLiveOutput;
}
//---------------------------------------------------------------------------
bool __fastcall TExternalConsole::NoInteractiveInput()
{
  return FNoInteractiveInput;
}
//---------------------------------------------------------------------------
void __fastcall TExternalConsole::WaitBeforeExit()
{
  // noop
}
//---------------------------------------------------------------------------
bool __fastcall TExternalConsole::CommandLineOnly()
{
  return true;
}
//---------------------------------------------------------------------------
class TNullConsole : public TConsole
{
public:
  __fastcall TNullConsole();

  virtual void __fastcall Print(UnicodeString Str, bool FromBeginning = false);
  virtual bool __fastcall Input(UnicodeString & Str, bool Echo, unsigned int Timer);
  virtual int __fastcall Choice(UnicodeString Options, int Cancel, int Break,
    int Timeouted, bool Timeouting, unsigned int Timer);
  virtual bool __fastcall PendingAbort();
  virtual void __fastcall SetTitle(UnicodeString Title);
  virtual bool __fastcall LimitedOutput();
  virtual bool __fastcall LiveOutput();
  virtual bool __fastcall NoInteractiveInput();
  virtual void __fastcall WaitBeforeExit();
  virtual bool __fastcall CommandLineOnly();
};
//---------------------------------------------------------------------------
__fastcall TNullConsole::TNullConsole()
{
}
//---------------------------------------------------------------------------
void __fastcall TNullConsole::Print(UnicodeString /*Str*/, bool /*FromBeginning*/)
{
  // noop
}
//---------------------------------------------------------------------------
bool __fastcall TNullConsole::Input(UnicodeString & /*Str*/, bool /*Echo*/,
  unsigned int /*Timer*/)
{
  return false;
}
//---------------------------------------------------------------------------
int __fastcall TNullConsole::Choice(UnicodeString /*Options*/, int /*Cancel*/,
  int Break, int Timeouted, bool Timeouting, unsigned int Timer)
{
  int Result;
  if (Timeouting)
  {
    Sleep(Timer);
    Result = Timeouted;
  }
  else
  {
    Result = Break;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TNullConsole::PendingAbort()
{
  return false;
}
//---------------------------------------------------------------------------
void __fastcall TNullConsole::SetTitle(UnicodeString /*Title*/)
{
  // noop
}
//---------------------------------------------------------------------------
bool __fastcall TNullConsole::LimitedOutput()
{
  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TNullConsole::LiveOutput()
{
  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TNullConsole::NoInteractiveInput()
{
  // do not matter, even if we return false,
  // it fails immediatelly afterwards in TNullConsole::Input
  return true;
}
//---------------------------------------------------------------------------
void __fastcall TNullConsole::WaitBeforeExit()
{
  assert(false);
  // noop
}
//---------------------------------------------------------------------------
bool __fastcall TNullConsole::CommandLineOnly()
{
  assert(false);
  return false;
}
//---------------------------------------------------------------------------
class TConsoleRunner
{
public:
  TConsoleRunner(TConsole * Console);
  ~TConsoleRunner();

  int __fastcall Run(const UnicodeString Session, TOptions * Options,
    TStrings * ScriptCommands, TStrings * ScriptParameters);
  void __fastcall ShowException(Exception * E);

protected:
  bool __fastcall DoInput(UnicodeString & Str, bool Echo, unsigned int Timer,
    bool Interactive);
  void __fastcall Input(const UnicodeString Prompt, UnicodeString & Str,
    bool Echo, bool Interactive);
  inline void __fastcall Print(const UnicodeString & Str, bool FromBeginning = false);
  inline void __fastcall PrintLine(const UnicodeString & Str);
  inline void __fastcall PrintMessage(const UnicodeString & Str);
  void __fastcall UpdateTitle();
  inline void __fastcall NotifyAbort();
  inline bool __fastcall Aborted(bool AllowCompleteAbort = true);
  void __fastcall MasterPasswordPrompt();
  void __fastcall DoShowException(TTerminal * Terminal, Exception * E);

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

  void __fastcall ScriptPrint(TScript * Script, const UnicodeString Str);
  void __fastcall ScriptPrintProgress(TScript * Script, bool First, const UnicodeString Str);
  void __fastcall ScriptInput(TScript * Script, const UnicodeString Prompt, UnicodeString & Str);
  void __fastcall ScriptTerminalPromptUser(TTerminal * Terminal,
    TPromptKind Kind, UnicodeString Name, UnicodeString Instructions, TStrings * Prompts,
    TStrings * Results, bool & Result, void * Arg);
  void __fastcall ScriptShowExtendedException(TTerminal * Terminal,
    Exception * E, void * Arg);
  void __fastcall ScriptTerminalQueryUser(TObject * Sender, const UnicodeString Query,
    TStrings * MoreMessages, unsigned int Answers, const TQueryParams * Params, unsigned int & Answer,
    TQueryType QueryType, void * Arg);
  void __fastcall ScriptQueryCancel(TScript * Script, bool & Cancel);
  void __fastcall SynchronizeControllerAbort(TObject * Sender, bool Close);
  void __fastcall SynchronizeControllerLog(TSynchronizeController * Controller,
    TSynchronizeLogEntry Entry, const UnicodeString Message);
  void __fastcall ScriptSynchronizeStartStop(TScript * Script,
    const UnicodeString LocalDirectory, const UnicodeString RemoteDirectory,
    const TCopyParamType & CopyParam, int SynchronizeParams);
  void __fastcall SynchronizeControllerSynchronize(TSynchronizeController * Sender,
    const UnicodeString LocalDirectory, const UnicodeString RemoteDirectory,
    const TCopyParamType & CopyParam, const TSynchronizeParamType & Params,
    TSynchronizeChecklist ** Checklist, TSynchronizeOptions * Options, bool Full);
  void __fastcall SynchronizeControllerSynchronizeInvalid(TSynchronizeController * Sender,
    const UnicodeString Directory, const UnicodeString ErrorStr);
  void __fastcall SynchronizeControllerTooManyDirectories(TSynchronizeController * Sender,
    int & MaxDirectories);
  unsigned int InputTimeout();
  void __fastcall TimerTimer(TObject * Sender);
  UnicodeString ExpandCommand(UnicodeString Command, TStrings * ScriptParameters);
  void __fastcall Failed(bool & AnyError);
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
  Timer->Interval = MSecsPerSec;
  Timer->Enabled = true;
  assert(WinConfiguration->OnMasterPasswordPrompt == NULL);
  WinConfiguration->OnMasterPasswordPrompt = MasterPasswordPrompt;
}
//---------------------------------------------------------------------------
TConsoleRunner::~TConsoleRunner()
{
  assert(WinConfiguration->OnMasterPasswordPrompt == MasterPasswordPrompt);
  WinConfiguration->OnMasterPasswordPrompt = NULL;
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
  return ((FScript != NULL) && (FScript->Batch != TScript::BatchOff) ? BATCH_INPUT_TIMEOUT : 0);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::Input(
  const UnicodeString Prompt, UnicodeString & Str, bool Echo, bool Interactive)
{
  Print(Prompt);

  if (!DoInput(Str, Echo, InputTimeout(), Interactive))
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptInput(TScript * /*Script*/,
  const UnicodeString Prompt, UnicodeString & Str)
{
  Input(Prompt, Str, true, true);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::Print(const UnicodeString & Str, bool FromBeginning)
{
  if (FLastProgressLen > 0)
  {
    FConsole->Print(L"\n" + Str, FromBeginning);
    FLastProgressLen = 0;
  }
  else
  {
    FConsole->Print(Str, FromBeginning);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::PrintLine(const UnicodeString & Str)
{
  Print(Str + L"\n");
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::PrintMessage(const UnicodeString & Str)
{
  UnicodeString Line =
    StringReplace(StringReplace(Str.TrimRight(), L"\n\n", L"\n", TReplaceFlags() << rfReplaceAll),
      L"\n \n", L"\n", TReplaceFlags() << rfReplaceAll);

  if (FScript != NULL)
  {
    // this also logs the message
    FScript->PrintLine(Line);
  }
  else
  {
    PrintLine(Line);
  }
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
      PrintMessage(LoadStr(USER_TERMINATED));
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
  const UnicodeString Str)
{
  Print(Str);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptPrintProgress(TScript * /*Script*/,
  bool First, const UnicodeString Str)
{
  UnicodeString S = Str;
  if (First && (FLastProgressLen > 0))
  {
    S = L"\n" + S;
  }
  else if (S.Length() < FLastProgressLen)
  {
    int Padding = FLastProgressLen - S.Length();
    S += UnicodeString::StringOfChar(L' ', Padding) +
      UnicodeString::StringOfChar(L'\b', Padding);
  }
  FConsole->Print(S, true);
  FLastProgressLen = Str.Length();
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptTerminalPromptUser(TTerminal * /*Terminal*/,
  TPromptKind /*Kind*/, UnicodeString Name, UnicodeString Instructions, TStrings * Prompts,
  TStrings * Results, bool & Result, void * /*Arg*/)
{
  if (!Instructions.IsEmpty())
  {
    PrintMessage(Instructions);
  }

  // if there are no prompts, success is default
  Result = true;

  for (int Index = 0; Index < Prompts->Count; Index++)
  {
    UnicodeString Prompt = Prompts->Strings[Index];
    if (!Prompt.IsEmpty() && (Prompt[Prompt.Length()] != L' '))
    {
      Prompt += L' ';
    }
    int P = Prompt.Pos(L'&');
    if (P > 0)
    {
      Prompt.Delete(P, 1);
    }
    Print(Prompt);

    UnicodeString AResult = Results->Strings[Index]; // useless
    Result = DoInput(AResult, bool(Prompts->Objects[Index]), InputTimeout(), true);
    Results->Strings[Index] = AResult;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptShowExtendedException(
  TTerminal * Terminal, Exception * E, void * /*Arg*/)
{
  DoShowException(Terminal, E);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptTerminalQueryUser(TObject * /*Sender*/,
  const UnicodeString Query, TStrings * MoreMessages, unsigned int Answers,
  const TQueryParams * Params, unsigned int & Answer, TQueryType /*QueryType*/,
  void * /*Arg*/)
{
  UnicodeString AQuery = Query;
  unsigned int Timer = 0;
  unsigned int Timeout = 0;
  unsigned int TimeoutA = 0;
  unsigned int NoBatchA = 0;

  if (Params != NULL)
  {
    if (Params->Timeout > 0)
    {
      Timeout = Params->Timeout;
      TimeoutA = Params->TimeoutAnswer;
    }

    if (Params->Timer > 0)
    {
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

    NoBatchA = Params->NoBatchAnswers;
  }

  unsigned int AAnswers = Answers;

  PrintMessage(AQuery);
  if ((MoreMessages != NULL) && (MoreMessages->Count > 0))
  {
    PrintMessage(MoreMessages->Text);
  }

  static const int MaxButtonCount = 15;
  unsigned int Buttons[MaxButtonCount];
  UnicodeString Captions[MaxButtonCount];
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
        if (Params->Aliases[ai].Button == Buttons[bi])
        {
          Captions[bi] = Params->Aliases[ai].Alias;
          OnClicks[bi] = Params->Aliases[ai].OnClick;
          break;
        }
      }
    }
  }

  UnicodeString Accels;
  for (int Index = 0; Index < ButtonCount; Index++)
  {
    UnicodeString & Caption = Captions[Index];
    int P = Caption.Pos(L'&');
    if ((P > 0) && (P < Caption.Length()))
    {
      wchar_t Accel = AnsiUpperCase(Caption)[P + 1];
      if (Accels.Pos(Accel) > 0)
      {
        Caption.Delete(P, 1);
        Accels += L' ';
      }
      else
      {
        Accels += Accel;
      }
    }
    else
    {
      Accels += L' ';
    }
  }

  assert(Accels.Length() == ButtonCount);
  int NumberAccel = 0;
  unsigned int CancelA = CancelAnswer(Answers);
  int CancelIndex;
  unsigned int AbortA = AbortAnswer(Answers & ~NoBatchA);
  int AbortIndex;
  unsigned int ContinueA = ContinueAnswer(Answers & ~NoBatchA);
  int ContinueIndex;
  int TimeoutIndex = 0;

  for (int Index = 0; Index < ButtonCount; Index++)
  {
    UnicodeString & Caption = Captions[Index];

    if (Accels[Index + 1] == L' ')
    {
      for (int Index2 = 1; Index2 <= Caption.Length(); Index2++)
      {
        wchar_t C = AnsiUpperCase(Caption)[Index2];
        if ((C >= L'A') && (C <= L'Z') && (Accels.Pos(C) == 0))
        {
          Caption.Insert(L"&", Index2);
          Accels[Index + 1] = C;
          break;
        }
      }
    }

    if (Accels[Index + 1] == L' ')
    {
      for (int Index2 = 1; Index2 <= Caption.Length(); Index2++)
      {
        wchar_t C = AnsiUpperCase(Caption)[Index2];
        if ((C != L' ') && (Accels.Pos(C) == 0))
        {
          Caption.Insert(L"&", Index2);
          Accels[Index + 1] = C;
          break;
        }
      }
    }

    if (Accels[Index + 1] == L' ')
    {
      NumberAccel++;
      assert(NumberAccel <= 9);
      Caption = FORMAT(L"&%d%s", (NumberAccel, Caption));
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
    if (Buttons[Index] == TimeoutA)
    {
      TimeoutIndex = Index + 1;
    }
  }

  assert(Accels.Pos(L' ') == 0);

  bool Timeouting = (Timeout > 0);
  bool FirstOutput = true;

  do
  {
    Answer = 0;
    int AnswerIndex;
    bool Retry;

    do
    {
      Retry = false;

      if (FirstOutput || FConsole->LiveOutput())
      {
        UnicodeString Output;
        for (int i = 0; i < ButtonCount; i++)
        {
          if (i > 0)
          {
            Output += L", ";
          }

          UnicodeString Caption = Captions[i];
          int P = Caption.Pos(L'&');
          assert(P >= 0);

          Caption[P] = L'(';
          Caption.Insert(L")", P + 2);

          if (i + 1 == TimeoutIndex)
          {
            assert(Timeouting);
            Caption = FMTLOAD(TIMEOUT_BUTTON, (Caption, int(Timeout / MSecsPerSec)));
          }

          Output += Caption;
        }
        Output += L": ";

        // note that length of string may decrease over time due to number of
        // seconds length, but supposing it decreases by one character at time
        // at most, we do not mind as the prompt is terminated with space

        // If output is not live (file or pipe), do not use 'from beginning'
        // as it means that the output is not actually stored until new line
        // is sent (and we will not [because we cannot] rewrite the output anyway)
        Print(Output, !FirstOutput);
        FirstOutput = false;
      }

      if (!Timeouting && (FScript->Batch == TScript::BatchContinue))
      {
        AnswerIndex = ContinueIndex;
      }
      else if (!Timeouting && (FScript->Batch != TScript::BatchOff))
      {
        AnswerIndex = AbortIndex;
      }
      else if (Timeouting && (Timeout < MSecsPerSec))
      {
        AnswerIndex = TimeoutIndex;
      }
      else
      {
        unsigned int ActualTimer =
          (Timeouting && ((Timer == 0) || (Timer > MSecsPerSec)) ? MSecsPerSec : Timer);
        AnswerIndex = FConsole->Choice(Accels, CancelIndex, -1, -2,
          Timeouting, ActualTimer);
        if (AnswerIndex == -1)
        {
          NotifyAbort();
          AnswerIndex = AbortIndex;
        }
        else if (AnswerIndex == -2)
        {
          if (Timeouting)
          {
            assert(Timeout >= MSecsPerSec);
            Timeout -= ActualTimer;
            Retry = true;
          }

          // this does not take Timer into account,
          // but as of now Timer is used for TSecureShell timeout prompt only,
          // where Timer is less than MSecsPerSec
          if (Timer > 0)
          {
            assert((Params != NULL) && (Params->TimerEvent != NULL));
            if ((Params != NULL) && (Params->TimerEvent != NULL))
            {
              unsigned int AAnswer = 0;
              Params->TimerEvent(AAnswer);
              if (AAnswer != 0)
              {
                Answer = AAnswer;
                Retry = false;
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
      UnicodeString AnswerCaption = Captions[AnswerIndex - 1];
      int P = AnswerCaption.Pos(L"&");
      assert(P >= 0);
      AnswerCaption.Delete(P, 1);
      PrintLine(AnswerCaption);
      FirstOutput = true;

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
      PrintLine(L"");
    }
  }
  while (Answer == 0);

  if ((Answer == AbortA) && FLAGCLEAR(Params->Params, qpIgnoreAbort))
  {
    if (FScript->Terminal != NULL)
    {
      TStrings * Messages = new TStringList();
      try
      {
        Messages->Add(Query);
        if (MoreMessages != NULL)
        {
          Messages->AddStrings(MoreMessages);
        }
        FScript->Terminal->ActionLog->AddFailure(Messages);
      }
      __finally
      {
        delete Messages;
      }
    }

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
  const UnicodeString LocalDirectory, const UnicodeString RemoteDirectory,
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
  const UnicodeString Message)
{
  PrintMessage(Message);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::SynchronizeControllerAbort(TObject * /*Sender*/,
  bool /*Close*/)
{
  FSynchronizeAborted = true;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::SynchronizeControllerSynchronize(
  TSynchronizeController * /*Sender*/, const UnicodeString LocalDirectory,
  const UnicodeString RemoteDirectory, const TCopyParamType & CopyParam,
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
  TSynchronizeController * /*Sender*/, const UnicodeString Directory, const UnicodeString ErrorStr)
{
  if (!Directory.IsEmpty())
  {
    PrintMessage(FMTLOAD(WATCH_ERROR_DIRECTORY, (Directory)));
  }
  else
  {
    PrintMessage(LoadStr(WATCH_ERROR_GENERAL));
  }

  if (!ErrorStr.IsEmpty())
  {
    PrintMessage(ErrorStr);
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
  DoShowException(NULL, E);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::DoShowException(TTerminal * Terminal, Exception * E)
{
  if ((Terminal == NULL) && (FScript != NULL))
  {
    Terminal = FScript->Terminal;
  }

  UnicodeString Message;
  if (ExceptionMessage(E, Message))
  {
    FCommandError = true;
    PrintMessage(Message);
    ExtException * EE = dynamic_cast<ExtException *>(E);
    if ((EE != NULL) && (EE->MoreMessages != NULL))
    {
      PrintMessage(EE->MoreMessages->Text);
    }
  }

  TTerminal * LoggingTerminal = Terminal;
  TSecondaryTerminal * SecondaryTerminal = dynamic_cast<TSecondaryTerminal *>(LoggingTerminal);
  if (SecondaryTerminal != NULL)
  {
    LoggingTerminal = SecondaryTerminal->MainTerminal;
  }

  if (LoggingTerminal != NULL)
  {
    LoggingTerminal->ActionLog->AddFailure(E);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TConsoleRunner::DoInput(UnicodeString & Str, bool Echo,
  unsigned int Timeout, bool Interactive)
{
  bool Result;
  if (Interactive && FConsole->NoInteractiveInput())
  {
    Result = false;
  }
  else
  {
    Result = FConsole->Input(Str, Echo, Timeout);
  }

  if (Result)
  {
    while (!Str.IsEmpty() &&
      ((Str[Str.Length()] == L'\n') || (Str[Str.Length()] == L'\r')))
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
void __fastcall TConsoleRunner::MasterPasswordPrompt()
{
  UnicodeString Password;
  Input(LoadStr(CONSOLE_MASTER_PASSWORD_PROMPT), Password, false, true);
  WinConfiguration->SetMasterPassword(Password);
}
//---------------------------------------------------------------------------
UnicodeString TConsoleRunner::ExpandCommand(UnicodeString Command, TStrings * ScriptParameters)
{
  assert(ScriptParameters != NULL);
  for (int Index = 0; Index < ScriptParameters->Count; Index++)
  {
    Command = StringReplace(Command, FORMAT(L"%%%d%%", (Index+1)),
      ScriptParameters->Strings[Index], TReplaceFlags() << rfReplaceAll);
  }
  Command = ExpandEnvironmentVariables(Command);
  return Command;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::Failed(bool & AnyError)
{
  if (FScript != NULL)
  {
    FScript->Log(llMessage, L"Failed");
  }
  AnyError = true;
}
//---------------------------------------------------------------------------
int __fastcall TConsoleRunner::Run(const UnicodeString Session, TOptions * Options,
  TStrings * ScriptCommands, TStrings * ScriptParameters)
{
  int ExitCode;
  try
  {
    bool AnyError = false;

    try
    {
      FScript = new TManagementScript(StoredSessions, FConsole->LimitedOutput());

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
        if (FCommandError)
        {
          Failed(AnyError);
        }
      }

      FScript->Groups = Options->SwitchValue(L"xmlgroups", true, false);

      int ScriptPos = 0;
      bool Result;
      do
      {
        UpdateTitle();

        UnicodeString Command;
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
          Print(L"winscp> ");
          Result = DoInput(Command, true, 0, false);
        }

        if (Result)
        {
          FCommandError = false;
          FScript->Command(ExpandCommand(Command, ScriptParameters));

          if (FCommandError)
          {
            Failed(AnyError);
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
    catch(Exception & E)
    {
      Failed(AnyError);
      ShowException(&E);
    }

    ExitCode = AnyError ? RESULT_ANY_ERROR : RESULT_SUCCESS;

    if (FScript != NULL)
    {
      FScript->Log(llMessage, FORMAT(L"Exit code: %d", (ExitCode)));
    }
  }
  __finally
  {
    delete FScript;
    FScript = NULL;
  }

  return ExitCode;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::UpdateTitle()
{
  UnicodeString NewTitle;
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
void __fastcall LoadScriptFromFile(UnicodeString FileName, TStrings * Lines)
{
  Lines->LoadFromFile(FileName);
}
//---------------------------------------------------------------------------
void __fastcall Usage(TConsole * Console)
{
  UnicodeString Usage = LoadStr(USAGE8, 10240);
  UnicodeString ExeBaseName = ChangeFileExt(ExtractFileName(Application->ExeName), L"");
  Usage = StringReplace(Usage, L"%APP%", ExeBaseName,
    TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
  UnicodeString Copyright =
    StringReplace(LoadStr(WINSCP_COPYRIGHT), "©", "(c)",
      TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
  Usage = FORMAT(Usage, (Configuration->VersionStr, Copyright));
  TStrings * Lines = new TStringList();
  try
  {
    Lines->Text = Usage;
    for (int Index = 0; Index < Lines->Count; Index++)
    {
      bool Print = true;
      UnicodeString Line = Lines->Strings[Index];
      if ((Line.Length() >= 2) && (Line[2] == L':'))
      {
        switch (Line[1])
        {
          case L'G':
            Print = !Console->CommandLineOnly();
            break;

          case L'C':
            Print = Console->CommandLineOnly();
            break;

          case L'B':
            Print = true;
            break;

          default:
            assert(false);
            break;
        }
        Line.Delete(1, 2);
      }

      if (Print)
      {
        Console->Print(Line + L"\n");
      }
    }
  }
  __finally
  {
    delete Lines;
  }
  Console->WaitBeforeExit();
}
//---------------------------------------------------------------------------
int __fastcall Console(bool Help)
{
  TProgramParams * Params = TProgramParams::Instance();
  int Result = 0;
  TConsole * Console = NULL;
  TConsoleRunner * Runner = NULL;
  TStrings * ScriptCommands = new TStringList();
  TStrings * ScriptParameters = new TStringList();
  try
  {
    UnicodeString ConsoleInstance;
    if (Params->FindSwitch(L"consoleinstance", ConsoleInstance))
    {
      Configuration->Usage->Inc(L"ConsoleExternal");
      Console = new TExternalConsole(ConsoleInstance, Params->FindSwitch(L"nointeractiveinput"));
    }
    else if (Params->FindSwitch(L"Console") || Help)
    {
      Configuration->Usage->Inc(L"ConsoleOwn");
      Console = TOwnConsole::Instance();
    }
    else
    {
      Configuration->Usage->Inc(L"ConsoleNull");
      Console = new TNullConsole();
    }

    if (Help)
    {
      Usage(Console);
    }
    else
    {
      Runner = new TConsoleRunner(Console);

      try
      {
        UnicodeString Value;
        if (Params->FindSwitch(L"script", Value) && !Value.IsEmpty())
        {
          Configuration->Usage->Inc(L"ScriptFile");
          LoadScriptFromFile(Value, ScriptCommands);
        }
        Params->FindSwitch(L"command", ScriptCommands);
        if (ScriptCommands->Count > 0)
        {
          Configuration->Usage->Inc(L"ScriptCommands");
        }
        Params->FindSwitch(L"parameter", ScriptParameters);
        if (ScriptParameters->Count > 0)
        {
          Configuration->Usage->Inc(L"ScriptParameters");
        }

        bool Url = false;
        UnicodeString Session;
        if (Params->ParamCount >= 1)
        {
          Session = Params->Param[1];
        }

        bool DefaultsOnly;
        delete StoredSessions->ParseUrl(Session, Params, DefaultsOnly,
          NULL, &Url);

        if (Url || Params->FindSwitch(L"Unsafe"))
        {
          // prevent any automatic action when URL is provided on
          // command-line (the check is duplicated in Execute())
          if ((ScriptCommands->Count > 0) || Params->FindSwitch(L"Log") || Params->FindSwitch(L"XmlLog"))
          {
            Console->Print(LoadStr(UNSAFE_ACTIONS_DISABLED) + L"\n");
          }
          ScriptCommands->Clear();
        }
        else
        {
          UnicodeString LogFile;
          if (Params->FindSwitch(L"Log", LogFile))
          {
            Configuration->Usage->Inc(L"ScriptLog");
            Configuration->TemporaryLogging(LogFile);
          }
          if (Params->FindSwitch(L"XmlLog", LogFile))
          {
            Configuration->Usage->Inc(L"ScriptXmlLog");
            Configuration->TemporaryActionsLogging(LogFile);
          }
        }

        Result = Runner->Run(Session, Params,
          (ScriptCommands->Count > 0 ? ScriptCommands : NULL),
          ScriptParameters);
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
    delete ScriptParameters;
  }

  return Result;
}
