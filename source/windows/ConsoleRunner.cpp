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
#include <HierarchicalStorage.h>
#include <Tools.h>

#include <Consts.hpp>
#include <StrUtils.hpp>

#include "Console.h"
#include "WinInterface.h"
#include "ProgParams.h"
#include "TextsWin.h"
#include "TextsCore.h"
#include "WinConfiguration.h"
#include "SynchronizeController.h"
#include "GUITools.h"
#include "VCLCommon.h"
#include "Setup.h"
#include "FtpFileSystem.h"
#include "SessionInfo.h"
//---------------------------------------------------------------------------
#define WM_INTERUPT_IDLE (WM_WINSCP_USER + 3)
#define BATCH_INPUT_TIMEOUT 10000
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
void TrimNewLine(UnicodeString & Str)
{
  while (!Str.IsEmpty() &&
    ((Str[Str.Length()] == L'\n') || (Str[Str.Length()] == L'\r')))
  {
    Str.SetLength(Str.Length() - 1);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsole::PrintLine(const UnicodeString & Str, bool Error)
{
  Print(Str + L"\n", false, Error);
};
//---------------------------------------------------------------------------
class TOwnConsole : public TConsole
{
public:
  static TOwnConsole * __fastcall Instance();

  virtual void __fastcall Print(UnicodeString Str, bool FromBeginning = false, bool Error = false);
  virtual bool __fastcall Input(UnicodeString & Str, bool Echo, unsigned int Timer);
  virtual int __fastcall Choice(
    UnicodeString Options, int Cancel, int Break, int Continue, int Timeouted, bool Timeouting, unsigned int Timer,
    UnicodeString Message);
  virtual bool __fastcall HasFlag(TConsoleFlag Flag) const;
  virtual bool __fastcall PendingAbort();
  virtual void __fastcall SetTitle(UnicodeString Title);
  virtual void __fastcall WaitBeforeExit();
  virtual void __fastcall Progress(TScriptProgress & Progress);
  virtual void __fastcall TransferOut(const unsigned char * Data, size_t Len);
  virtual size_t __fastcall TransferIn(unsigned char * Data, size_t Len);
  virtual UnicodeString __fastcall FinalLogMessage();

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
  static std::unique_ptr<TCriticalSection> FSection;

  bool FPendingAbort;
};
//---------------------------------------------------------------------------
TOwnConsole * TOwnConsole::FInstance = NULL;
std::unique_ptr<TCriticalSection> TOwnConsole::FSection(TraceInitPtr(new TCriticalSection()));
//---------------------------------------------------------------------------
__fastcall TOwnConsole::TOwnConsole()
{
  DebugAssert(FInstance == NULL);
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
    FConsoleWindow = GetConsoleWindow();
    if (DebugAlwaysTrue(FConsoleWindow != NULL))
    {
      FWindowStateTimer = new TTimer(Application);
      FWindowStateTimer->OnTimer = WindowStateTimer;
      FWindowStateTimer->Interval = 250;
      FWindowStateTimer->Enabled = true;
    }
  }
}
//---------------------------------------------------------------------------
__fastcall TOwnConsole::~TOwnConsole()
{
  TGuard Guard(FSection.get());

  delete FTrayIcon;
  delete FWindowStateTimer;

  // deliberately do not remove ConsoleCtrlHandler as it causes
  // failures while exiting

  FreeConsole();

  DebugAssert(FInstance == this);
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
  DebugAssert(FConsoleWindow != NULL);
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
    DebugFail();
  }
}
//---------------------------------------------------------------------------
void __fastcall TOwnConsole::ProcessMessages()
{
  // as of now, there's no point doing this unless we have icon tray
  // (i.e. we need to monitor window state and eventually process tray icon messages)
  if (FWindowStateTimer != NULL)
  {
    DebugAssert(WinConfiguration->MinimizeToTray);

    Application->ProcessMessages();
  }
}
//---------------------------------------------------------------------------
void __fastcall TOwnConsole::TrayIconClick(TObject * /*Sender*/)
{
  DebugAssert(FConsoleWindow != NULL);
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
  DebugCheck(WriteConsoleInput(FInput, &InputRecord, 1, &Written));
  DebugAssert(Written == 1);

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
void __fastcall TOwnConsole::Print(UnicodeString Str, bool FromBeginning, bool /*Error*/)
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
  DebugAssert(Result);
  DebugUsedParam(Result);
  DebugAssert(Str.Length() == static_cast<long>(Written));
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
    DebugAssert(FResult);
    FStr.SetLength(Read);
    TrimNewLine(FStr);
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

  bool Result = false;

  try
  {
    {
      const int FirstKey = VK_LBUTTON; // 0x01
      const int LastKey = VK_OEM_CLEAR; // 0xFE

      // reset key state
      for (int Key = FirstKey; Key <= LastKey; Key++)
      {
        GetAsyncKeyState(Key);
      }

      TConsoleInputThread InputThread(FInput, Str, Result);

      InputThread.Start();

      double TimerD = double(Timer)/MSecsPerDay;
      double End = Now() + TimerD;
      while (!InputThread.IsFinished() &&
             ((Timer == 0) || (double(Now()) < End)))
      {
        ProcessMessages();
        InputThread.WaitFor(50);

        for (int Key = FirstKey; Key <= LastKey; Key++)
        {
          if ((GetAsyncKeyState(Key) & 0x01) != 0)
          {
            End = Now() + TimerD;
            // Finishing the loop nevertheless to reset state of all keys
          }
        }
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
int __fastcall TOwnConsole::Choice(
  UnicodeString Options, int Cancel, int Break, int /*Continue*/, int Timeouted, bool /*Timeouting*/, unsigned int Timer,
  UnicodeString Message)
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
bool __fastcall TOwnConsole::HasFlag(TConsoleFlag Flag) const
{
  switch (Flag)
  {
    case cfLimitedOutput:
    case cfLiveOutput:
    case cfInteractive:
      return true;

    case cfNoInteractiveInput:
    case cfCommandLineOnly:
    case cfWantsProgress:
    case cfStdOut:
    case cfStdIn:
      return false;

    default:
      DebugFail();
      return false;
  }
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
void __fastcall TOwnConsole::Progress(TScriptProgress & /*Progress*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TOwnConsole::TransferOut(const unsigned char * DebugUsedArg(Data), size_t DebugUsedArg(Len))
{
  DebugFail();
}
//---------------------------------------------------------------------------
size_t __fastcall TOwnConsole::TransferIn(unsigned char * DebugUsedArg(Data), size_t DebugUsedArg(Len))
{
  DebugFail();
  return 0;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TOwnConsole::FinalLogMessage()
{
  return UnicodeString();
}
//---------------------------------------------------------------------------
typedef TConsoleCommStruct::TInitEvent::STDINOUT TStdInOutMode;
//---------------------------------------------------------------------------
class TExternalConsole : public TConsole
{
public:
  __fastcall TExternalConsole(const UnicodeString Instance, bool NoInteractiveInput, TStdInOutMode StdOut, TStdInOutMode StdIn);
  virtual __fastcall ~TExternalConsole();

  virtual void __fastcall Print(UnicodeString Str, bool FromBeginning = false, bool Error = false);
  virtual bool __fastcall Input(UnicodeString & Str, bool Echo, unsigned int Timer);
  virtual int __fastcall Choice(
    UnicodeString Options, int Cancel, int Break, int Continue, int Timeouted, bool Timeouting, unsigned int Timer,
    UnicodeString Message);
  virtual bool __fastcall HasFlag(TConsoleFlag Flag) const;
  virtual bool __fastcall PendingAbort();
  virtual void __fastcall SetTitle(UnicodeString Title);
  virtual void __fastcall WaitBeforeExit();
  virtual void __fastcall Progress(TScriptProgress & Progress);
  virtual void __fastcall TransferOut(const unsigned char * Data, size_t Len);
  virtual size_t __fastcall TransferIn(unsigned char * Data, size_t Len);
  virtual UnicodeString __fastcall FinalLogMessage();

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
  TStdInOutMode FStdOut;
  TStdInOutMode FStdIn;
  bool FWantsProgress;
  bool FInteractive;
  unsigned int FMaxSend;
  // Particularly FTP calls TransferOut/In from other thread
  std::unique_ptr<TCriticalSection> FSection;

  inline TConsoleCommStruct * __fastcall GetCommStruct();
  inline void __fastcall FreeCommStruct(TConsoleCommStruct * CommStruct);
  inline void __fastcall SendEvent(int Timeout);
  void __fastcall Init();
  void __fastcall CheckHandle(HANDLE Handle, const UnicodeString & Desc);
};
//---------------------------------------------------------------------------
__fastcall TExternalConsole::TExternalConsole(
  const UnicodeString Instance, bool NoInteractiveInput, TStdInOutMode StdOut, TStdInOutMode StdIn)
{
  UnicodeString Name;
  Name = FORMAT(L"%s%s", (CONSOLE_EVENT_REQUEST, (Instance)));
  CheckHandle(FRequestEvent = OpenEvent(EVENT_ALL_ACCESS, false, Name.c_str()), L"Request event");
  Name = FORMAT(L"%s%s", (CONSOLE_EVENT_RESPONSE, (Instance)));
  CheckHandle(FResponseEvent = OpenEvent(EVENT_ALL_ACCESS, false, Name.c_str()), L"Response event");
  Name = FORMAT(L"%s%s", (CONSOLE_EVENT_CANCEL, (Instance)));
  CheckHandle(FCancelEvent = OpenEvent(EVENT_ALL_ACCESS, false, Name.c_str()), L"Cancel event");
  Name = FORMAT(L"%s%s", (CONSOLE_MAPPING, (Instance)));
  CheckHandle(FFileMapping = OpenFileMapping(FILE_MAP_ALL_ACCESS, false, Name.c_str()), L"File mapping");

  HANDLE Job = OpenJobObject(JOB_OBJECT_ASSIGN_PROCESS, FALSE,
    FORMAT(L"%s%s", (CONSOLE_JOB, Instance)).c_str());
  if (DebugAlwaysTrue(Job != NULL))
  {
    AssignProcessToJobObject(Job, GetCurrentProcess());
    // winscp.com/winscp.dll keeps the only reference to the job.
    // once it gets closed (because winscp.com is forcefully terminated),
    // we get terminated as well
    CloseHandle(Job);
  }

  FSection.reset(new TCriticalSection());

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
  FStdOut = StdOut;
  FStdIn = StdIn;
  FMaxSend = 0;

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
void __fastcall TExternalConsole::CheckHandle(HANDLE Handle, const UnicodeString & Desc)
{
  if (Handle == NULL)
  {
    throw ExtException(LoadStr(EXTERNAL_CONSOLE_INIT_ERROR), FORMAT(L"%s\n%s", (Desc, LastSysErrorMessage())));
  }
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
  unsigned int Start = 0; // shut up
  if (Configuration->ActualLogProtocol >= 1)
  {
    Start = GetTickCount();
  }
  unsigned int Result = WaitForSingleObject(FResponseEvent, Timeout);
  if (Configuration->ActualLogProtocol >= 1)
  {
    unsigned int End = GetTickCount();
    unsigned int Duration = End - Start;
    FMaxSend = std::max(Duration, FMaxSend);
  }
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
UnicodeString __fastcall TExternalConsole::FinalLogMessage()
{
  return FORMAT(L"Max roundtrip: %d", (static_cast<int>(FMaxSend)));
}
//---------------------------------------------------------------------------
void __fastcall TExternalConsole::Print(UnicodeString Str, bool FromBeginning, bool Error)
{
  TGuard Guard(FSection.get());
  // need to do at least one iteration, even when Str is empty (new line)
  do
  {
    TConsoleCommStruct * CommStruct = GetCommStruct();
    try
    {
      size_t MaxLen = LENOF(CommStruct->PrintEvent.Message) - 1;
      UnicodeString Piece = Str.SubString(1, MaxLen);
      Str.Delete(1, MaxLen);

      CommStruct->Event = TConsoleCommStruct::PRINT;
      wcscpy(CommStruct->PrintEvent.Message, Piece.c_str());
      CommStruct->PrintEvent.FromBeginning = FromBeginning;
      CommStruct->PrintEvent.Error = Error;

      // In the next iteration we need to append never overwrite.
      // Note that this won't work properly for disk/pipe outputs,
      // when the next line is also FromBeginning,
      // as !FromBeginning print effectively commits previous FromBeginning print.
      // On the other hand, FromBeginning print is always initiated by us,
      // and it's not likely we ever issue print over 10 KB.
      FromBeginning = false;
    }
    __finally
    {
      FreeCommStruct(CommStruct);
    }

    SendEvent(INFINITE);
  }
  while (!Str.IsEmpty());
}
//---------------------------------------------------------------------------
bool __fastcall TExternalConsole::Input(UnicodeString & Str, bool Echo, unsigned int Timer)
{
  TGuard Guard(FSection.get());
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
    TrimNewLine(Str);
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TExternalConsole::Choice(
  UnicodeString Options, int Cancel, int Break, int Continue, int Timeouted, bool Timeouting, unsigned int Timer,
  UnicodeString Message)
{
  TGuard Guard(FSection.get());
  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    CommStruct->Event = TConsoleCommStruct::CHOICE;

    DebugAssert(Options.Length() < static_cast<int>(LENOF(CommStruct->ChoiceEvent.Options)));
    wcscpy(CommStruct->ChoiceEvent.Options, Options.c_str());
    CommStruct->ChoiceEvent.Cancel = Cancel;
    CommStruct->ChoiceEvent.Break = Break;
    CommStruct->ChoiceEvent.Result = Break;
    CommStruct->ChoiceEvent.Continue = Continue;
    CommStruct->ChoiceEvent.Timeouted = Timeouted;
    CommStruct->ChoiceEvent.Timer = Timer;
    CommStruct->ChoiceEvent.Timeouting = Timeouting;
    size_t MaxLen = LENOF(CommStruct->ChoiceEvent.Message) - 1;
    Message = Message.SubString(1, MaxLen);
    wcscpy(CommStruct->ChoiceEvent.Message, Message.c_str());
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
  TGuard Guard(FSection.get());
  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    // Truncate to maximum allowed. Title over 10 KB won't fit to screen anyway
    Title = Title.SubString(1, LENOF(CommStruct->TitleEvent.Title) - 1);

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
  TGuard Guard(FSection.get());
  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    CommStruct->Event = TConsoleCommStruct::INIT;
    CommStruct->InitEvent.WantsProgress = false;
    CommStruct->InitEvent.UseStdErr = (FStdOut != TConsoleCommStruct::TInitEvent::OFF);
    CommStruct->InitEvent.OutputFormat = FStdOut;
    CommStruct->InitEvent.InputFormat = FStdIn;
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
    FInteractive =
      (CommStruct->InitEvent.InputType != FILE_TYPE_DISK) &&
      (CommStruct->InitEvent.InputType != FILE_TYPE_PIPE);
    FWantsProgress = CommStruct->InitEvent.WantsProgress;
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TExternalConsole::HasFlag(TConsoleFlag Flag) const
{
  switch (Flag)
  {
    case cfLimitedOutput:
      return FLimitedOutput;

    case cfLiveOutput:
      return FLiveOutput;

    case cfNoInteractiveInput:
      return FNoInteractiveInput;

    case cfInteractive:
      return FInteractive;

    case cfCommandLineOnly:
      return true;

    case cfWantsProgress:
      return FWantsProgress;

    case cfStdOut:
      return (FStdOut != TConsoleCommStruct::TInitEvent::OFF);

    case cfStdIn:
      return (FStdIn != TConsoleCommStruct::TInitEvent::OFF);

    default:
      DebugFail();
      return false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TExternalConsole::WaitBeforeExit()
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TExternalConsole::Progress(TScriptProgress & Progress)
{
  TGuard Guard(FSection.get());
  TConsoleCommStruct * CommStruct = GetCommStruct();

  typedef TConsoleCommStruct::TProgressEvent TProgressEvent;

  try
  {
    TProgressEvent & ProgressEvent = CommStruct->ProgressEvent;

    CommStruct->Event = TConsoleCommStruct::PROGRESS;

    if (DebugAlwaysTrue(TFileOperationProgressType::IsTransferOperation(Progress.Operation)))
    {
      ProgressEvent.Operation = TProgressEvent::COPY;
    }

    switch (Progress.Side)
    {
      case osLocal:
        ProgressEvent.Side = TProgressEvent::LOCAL;
        break;

      case osRemote:
        ProgressEvent.Side = TProgressEvent::REMOTE;
        break;

      default:
        DebugFail();
    }

    wcsncpy(ProgressEvent.FileName, Progress.FileName.c_str(), LENOF(ProgressEvent.FileName));
    NULL_TERMINATE(ProgressEvent.FileName);

    wcsncpy(ProgressEvent.Directory, Progress.Directory.c_str(), LENOF(ProgressEvent.Directory));
    NULL_TERMINATE(ProgressEvent.Directory);

    ProgressEvent.OverallProgress = Progress.OverallProgress;
    ProgressEvent.FileProgress = Progress.FileProgress;
    ProgressEvent.CPS = Progress.CPS;
    ProgressEvent.Cancel = Progress.Cancel;
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }
  SendEvent(INFINITE);

  CommStruct = GetCommStruct();
  try
  {
    TProgressEvent & ProgressEvent = CommStruct->ProgressEvent;
    Progress.Cancel = ProgressEvent.Cancel;
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }
}
//---------------------------------------------------------------------------
void __fastcall TExternalConsole::TransferOut(const unsigned char * Data, size_t Len)
{
  TGuard Guard(FSection.get());
  DebugAssert((Data == NULL) == (Len == 0));
  size_t Offset = 0;
  do
  {
    TConsoleCommStruct * CommStruct = GetCommStruct();
    try
    {
      CommStruct->Event = TConsoleCommStruct::TRANSFEROUT;
      unsigned int BlockLen = std::min(Len - Offset, sizeof(CommStruct->TransferEvent.Data));
      memcpy(CommStruct->TransferEvent.Data, Data + Offset, BlockLen);
      CommStruct->TransferEvent.Len = BlockLen;
      Offset += BlockLen;
    }
    __finally
    {
      FreeCommStruct(CommStruct);
    }
    SendEvent(INFINITE);
  }
  while (Offset < Len);
}
//---------------------------------------------------------------------------
size_t __fastcall TExternalConsole::TransferIn(unsigned char * Data, size_t Len)
{
  TGuard Guard(FSection.get());
  size_t Offset = 0;
  size_t Result = 0;
  while ((Result == Offset) && (Offset < Len))
  {
    TConsoleCommStruct * CommStruct;
    size_t BlockLen = std::min(Len - Offset, sizeof(CommStruct->TransferEvent.Data));

    CommStruct = GetCommStruct();
    try
    {
      CommStruct->Event = TConsoleCommStruct::TRANSFERIN;
      CommStruct->TransferEvent.Len = BlockLen;
      CommStruct->TransferEvent.Error = false;
    }
    __finally
    {
      FreeCommStruct(CommStruct);
    }

    SendEvent(INFINITE);

    CommStruct = GetCommStruct();
    try
    {
      if (CommStruct->TransferEvent.Error)
      {
        throw Exception(LoadStr(STREAM_READ_ERROR));
      }
      DebugAssert(CommStruct->TransferEvent.Len <= BlockLen);
      Result += CommStruct->TransferEvent.Len;
      memcpy(Data + Offset, CommStruct->TransferEvent.Data, CommStruct->TransferEvent.Len);
      Offset += BlockLen;
    }
    __finally
    {
      FreeCommStruct(CommStruct);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
class TNullConsole : public TConsole
{
public:
  __fastcall TNullConsole();

  virtual void __fastcall Print(UnicodeString Str, bool FromBeginning = false, bool Error = false);
  virtual bool __fastcall Input(UnicodeString & Str, bool Echo, unsigned int Timer);
  virtual int __fastcall Choice(
    UnicodeString Options, int Cancel, int Break, int Continue, int Timeouted, bool Timeouting, unsigned int Timer,
    UnicodeString Message);
  virtual bool __fastcall HasFlag(TConsoleFlag Flag) const;
  virtual bool __fastcall PendingAbort();
  virtual void __fastcall SetTitle(UnicodeString Title);
  virtual void __fastcall WaitBeforeExit();

  virtual void __fastcall Progress(TScriptProgress & Progress);
  virtual void __fastcall TransferOut(const unsigned char * Data, size_t Len);
  virtual size_t __fastcall TransferIn(unsigned char * Data, size_t Len);
  virtual UnicodeString __fastcall FinalLogMessage();
};
//---------------------------------------------------------------------------
__fastcall TNullConsole::TNullConsole()
{
}
//---------------------------------------------------------------------------
void __fastcall TNullConsole::Print(UnicodeString /*Str*/, bool /*FromBeginning*/, bool /*Error*/)
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
int __fastcall TNullConsole::Choice(
  UnicodeString /*Options*/, int /*Cancel*/, int Break, int /*Continue*/, int Timeouted, bool Timeouting,
  unsigned int Timer, UnicodeString /*Message*/)
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
bool __fastcall TNullConsole::HasFlag(TConsoleFlag Flag) const
{
  switch (Flag)
  {
    // do not matter, even if we return false,
    // it fails immediately afterwards in TNullConsole::Input
    case cfNoInteractiveInput:
      return true;

    case cfLimitedOutput:
    case cfLiveOutput:
    case cfInteractive:
    case cfCommandLineOnly:
    case cfWantsProgress:
    case cfStdOut:
    case cfStdIn:
      return false;

    default:
      DebugFail();
      return false;
  }

}
//---------------------------------------------------------------------------
void __fastcall TNullConsole::WaitBeforeExit()
{
  DebugFail();
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TNullConsole::Progress(TScriptProgress & /*Progress*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TNullConsole::TransferOut(const unsigned char * DebugUsedArg(Data), size_t DebugUsedArg(Len))
{
  DebugFail();
}
//---------------------------------------------------------------------------
size_t __fastcall TNullConsole::TransferIn(unsigned char * DebugUsedArg(Data), size_t DebugUsedArg(Len))
{
  DebugFail();
  return 0;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TNullConsole::FinalLogMessage()
{
  return UnicodeString();
}
//---------------------------------------------------------------------------
static UnicodeString TimestampVarName(L"TIMESTAMP");
//---------------------------------------------------------------------------
class TConsoleRunner
{
public:
  TConsoleRunner(TConsole * Console);
  ~TConsoleRunner();

  int Run(
    const UnicodeString Session, TOptions * Options, TStrings * ScriptCommands, TStrings * ScriptParameters,
    bool UsageWarnings);
  void __fastcall ShowException(Exception * E);
  inline void __fastcall PrintMessage(const UnicodeString & Str, bool Error = false);

protected:
  bool __fastcall DoInput(UnicodeString & Str, bool Echo, unsigned int Timer,
    bool Interactive);
  void __fastcall Input(const UnicodeString Prompt, UnicodeString & Str,
    bool Echo, bool Interactive);
  inline void __fastcall Print(const UnicodeString & Str, bool FromBeginning = false, bool Error = false);
  void __fastcall UpdateTitle();
  inline bool __fastcall NotifyAbort();
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
  bool FExternalTimestampVar;

  void __fastcall ScriptPrint(TScript * Script, const UnicodeString Str, bool Error);
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
  void __fastcall ScriptProgress(TScript * Script, TScriptProgress & Progress);
  void __fastcall ScriptTransferOut(TObject *, const unsigned char * Data, size_t Len);
  size_t __fastcall ScriptTransferIn(TObject *, unsigned char * Data, size_t Len);
  void __fastcall ConfigurationChange(TObject * Sender);
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
  DebugAssert(WinConfiguration->OnMasterPasswordPrompt == NULL);
  WinConfiguration->OnMasterPasswordPrompt = MasterPasswordPrompt;
  DebugAssert(Configuration->OnChange == NULL);
  FExternalTimestampVar = !GetEnvironmentVariable(TimestampVarName).IsEmpty();
  Configuration->OnChange = ConfigurationChange;
  Configuration->Scripting = true;
}
//---------------------------------------------------------------------------
TConsoleRunner::~TConsoleRunner()
{
  DebugAssert(WinConfiguration->OnMasterPasswordPrompt == MasterPasswordPrompt);
  WinConfiguration->OnMasterPasswordPrompt = NULL;
  DebugAssert(Configuration->OnChange == ConfigurationChange);
  Configuration->OnChange = NULL;
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
void __fastcall TConsoleRunner::Print(const UnicodeString & Str, bool FromBeginning, bool Error)
{
  if (FLastProgressLen > 0)
  {
    FConsole->Print(L"\n" + Str, FromBeginning, Error);
    FLastProgressLen = 0;
  }
  else
  {
    FConsole->Print(Str, FromBeginning, Error);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::PrintMessage(const UnicodeString & Str, bool Error)
{
  UnicodeString Line = RemoveEmptyLines(Str);

  if (FScript != NULL)
  {
    // this also logs the message
    FScript->PrintLine(Line, Error);
  }
  else
  {
    FConsole->PrintLine(Line, Error);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TConsoleRunner::NotifyAbort()
{
  bool Result = FBatchScript;
  if (Result)
  {
    FAborted = true;
  }
  return Result;
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
      PrintMessage(LoadStr(USER_TERMINATED), true);

      if (AllowCompleteAbort && NotifyAbort())
      {
        if (FScript->Terminal != NULL)
        {
          std::unique_ptr<TStringList> Failure(TextToStringList(LoadStr(USER_TERMINATED)));
          FScript->Terminal->ActionLog->AddFailure(Failure.get());
        }
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptPrint(TScript * /*Script*/,
  const UnicodeString Str, bool Error)
{
  Print(Str, false, Error);
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
    bool Echo = FLAGSET(int(Prompts->Objects[Index]), pupEcho);
    Result = DoInput(AResult, Echo, InputTimeout(), true);
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
  unsigned int TimeoutR = 0;
  unsigned int NoBatchA = 0;

  if (Params != NULL)
  {
    if (Params->Timeout > 0)
    {
      Timeout = Params->Timeout;
      TimeoutA = Params->TimeoutAnswer;
      TimeoutR = Params->TimeoutResponse;
      DebugAssert((TimeoutA != 0) && (TimeoutR != 0));
    }

    if (Params->Timer > 0)
    {
      Timer = Params->Timer;
      if (Params->TimerAnswers > 0)
      {
        Answers = Params->TimerAnswers;
      }
      // not considering TimerQueryType as we do not use QueryType anyway
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

    if (FLAGSET(Params->Params, qpWaitInBatch))
    {
      DebugAssert(Timeout == 0);
      Timeout = InputTimeout();
      if (Timeout != 0)
      {
        DebugAssert((TimeoutA == 0) && (TimeoutR == 0));
        // See a duplicate AbortAnswer call below
        TimeoutA = AbortAnswer(Answers & ~NoBatchA);
        TimeoutR = TimeoutA;
      }
    }
  }

  AQuery = UnformatMessage(AQuery);
  AQuery = RemoveInteractiveMsgTag(AQuery);

  ApplyTabs(AQuery, L' ', NULL, NULL);

  unsigned int AAnswers = Answers;

  UnicodeString Message = AQuery;
  PrintMessage(AQuery);
  if ((MoreMessages != NULL) && (MoreMessages->Count > 0))
  {
    PrintMessage(MoreMessages->Text);
    Message += L"\n" + MoreMessages->Text;
  }

  std::vector<unsigned int> Buttons;
  std::vector<UnicodeString> Captions;
  std::vector<TButtonSubmitEvent> OnSubmits;

  for (unsigned int Answer = qaFirst; Answer <= qaLast; Answer = Answer << 1)
  {
    if (FLAGSET(Answers, Answer))
    {
      UnicodeString Name; // unused
      UnicodeString Caption;
      AnswerNameAndCaption(Answer, Name, Caption);
      Captions.push_back(Caption);
      Buttons.push_back(Answer);
      OnSubmits.push_back(NULL);
      AAnswers -= Answer;
    }
  }

  DebugUsedParam(AAnswers);
  DebugAssert(AAnswers == 0);
  DebugAssert(!Buttons.empty());

  if ((Params != NULL) && (Params->Aliases != NULL))
  {
    for (unsigned int bi = 0; bi < Buttons.size(); bi++)
    {
      for (unsigned int ai = 0; ai < Params->AliasesCount; ai++)
      {
        if (Params->Aliases[ai].Button == Buttons[bi])
        {
          if (!Params->Aliases[ai].Alias.IsEmpty())
          {
            Captions[bi] = Params->Aliases[ai].Alias;
          }
          OnSubmits[bi] = Params->Aliases[ai].OnSubmit;
          break;
        }
      }
    }
  }

  UnicodeString Accels;
  for (unsigned int Index = 0; Index < Buttons.size(); Index++)
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

  DebugAssert(Accels.Length() == static_cast<int>(Buttons.size()));
  int NumberAccel = 0;
  unsigned int CancelA = CancelAnswer(Answers);
  int CancelIndex;
  // AbortAnswer call duplicated in qpWaitInBatch branch above
  unsigned int AbortA = AbortAnswer(Answers & ~NoBatchA);
  int AbortIndex;
  unsigned int ContinueA = ContinueAnswer(Answers & ~NoBatchA);
  int ContinueIndex;
  int TimeoutIndex = 0;

  for (unsigned int Index = 0; Index < Buttons.size(); Index++)
  {
    UnicodeString & Caption = Captions[Index];

    if (Accels[Index + 1] == L' ')
    {
      for (int Index2 = 1; Index2 <= Caption.Length(); Index2++)
      {
        wchar_t C = AnsiUpperCase(Caption)[Index2];
        if (IsLetter(C) && (Accels.Pos(C) == 0))
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
      DebugAssert(NumberAccel <= 9);
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

  DebugAssert(Accels.Pos(L' ') == 0);

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

      if (FirstOutput || FConsole->HasFlag(cfLiveOutput))
      {
        UnicodeString Output;
        for (unsigned int i = 0; i < Buttons.size(); i++)
        {
          if (i > 0)
          {
            Output += L", ";
          }

          UnicodeString Caption = Captions[i];
          int P = Caption.Pos(L'&');
          if (DebugAlwaysTrue(P >= 0))
          {
            Caption[P] = L'(';
            Caption.Insert(L")", P + 2);
          }

          if (i + 1 == static_cast<unsigned int>(TimeoutIndex))
          {
            DebugAssert(Timeouting);
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
        Answer = TimeoutR;
      }
      else
      {
        unsigned int ActualTimer;
        if (Timeouting)
        {
          if (Timer == 0)
          {
            if (FConsole->HasFlag(cfNoInteractiveInput))
            {
              ActualTimer = Timeout;
            }
            else
            {
              ActualTimer = MSecsPerSec;
            }
          }
          else
          {
            if (Timer < MSecsPerSec)
            {
              ActualTimer = Timer;
            }
            else
            {
              ActualTimer = MSecsPerSec;
            }
          }
        }
        else
        {
          ActualTimer = Timer;
        }
        // Not to get preliminary "host is not responding" messages to .NET assembly
        if (FConsole->HasFlag(cfNoInteractiveInput) && (Timer > 0))
        {
          Sleep(Timer);
          AnswerIndex = -2;
        }
        else
        {
          AnswerIndex =
            FConsole->Choice(Accels, CancelIndex, -1, ContinueIndex, -2, Timeouting, ActualTimer, Message);
        }
        if (AnswerIndex == -1)
        {
          NotifyAbort();
          AnswerIndex = AbortIndex;
        }
        else if (AnswerIndex == -2)
        {
          if (Timeouting)
          {
            DebugAssert(Timeout >= MSecsPerSec);
            Timeout -= ActualTimer;
            Retry = true;
          }

          // this does not take Timer into account,
          // but as of now Timer is used for TSecureShell timeout prompt only,
          // where Timer is less than MSecsPerSec
          if (Timer > 0)
          {
            DebugAssert((Params != NULL) && (Params->TimerEvent != NULL));
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
      DebugAssert((AnswerIndex >= 1) && (AnswerIndex <= Accels.Length()));
      UnicodeString AnswerCaption = Captions[AnswerIndex - 1];
      int P = AnswerCaption.Pos(L"&");
      DebugAssert(P >= 0);
      AnswerCaption.Delete(P, 1);
      FConsole->PrintLine(AnswerCaption);
      FirstOutput = true;

      if (OnSubmits[AnswerIndex - 1] != NULL)
      {
        OnSubmits[AnswerIndex - 1](NULL, Answer);
      }
      else
      {
        Answer = Buttons[AnswerIndex - 1];
      }
    }
    else
    {
      FConsole->PrintLine();
    }
  }
  while (Answer == 0);

  if ((Answer == AbortA) &&
      ((Params == NULL) || FLAGCLEAR(Params->Params, qpIgnoreAbort)))
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
void __fastcall TConsoleRunner::ScriptProgress(TScript * /*Script*/, TScriptProgress & Progress)
{
  FConsole->Progress(Progress);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptTransferOut(TObject *, const unsigned char * Data, size_t Len)
{
  FConsole->TransferOut(Data, Len);
}
//---------------------------------------------------------------------------
size_t __fastcall TConsoleRunner::ScriptTransferIn(TObject *, unsigned char * Data, size_t Len)
{
  return FConsole->TransferIn(Data, Len);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::SynchronizeControllerLog(
  TSynchronizeController * /*Controller*/, TSynchronizeLogEntry /*Entry*/,
  const UnicodeString Message)
{
  PrintMessage(Message);
  LogSynchronizeEvent(FScript->Terminal, Message);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::SynchronizeControllerAbort(TObject * /*Sender*/,
  bool /*Close*/)
{
  FSynchronizeAborted = true;
  NotifyAbort();
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
    try
    {
      FScript->Synchronize(LocalDirectory, RemoteDirectory, CopyParam,
        Params.Params, Checklist);
    }
    catch (Exception & E)
    {
      if ((FScript->Batch == TScript::BatchContinue) &&
          FScript->Terminal->Active)
      {
        // noop
      }
      else
      {
        throw;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::SynchronizeControllerSynchronizeInvalid(
  TSynchronizeController * /*Sender*/, const UnicodeString Directory, const UnicodeString ErrorStr)
{
  if (!Directory.IsEmpty())
  {
    PrintMessage(FMTLOAD(WATCH_ERROR_DIRECTORY, (Directory)), true);
  }
  else
  {
    PrintMessage(LoadStr(WATCH_ERROR_GENERAL), true);
  }

  if (!ErrorStr.IsEmpty())
  {
    PrintMessage(ErrorStr, true);
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
  if (ExceptionFullMessage(E, Message))
  {
    FCommandError = true;
    PrintMessage(Message, true);
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
  if (Interactive && FConsole->HasFlag(cfNoInteractiveInput))
  {
    Result = false;
  }
  else
  {
    Result = FConsole->Input(Str, Echo, Timeout);
  }

  if (!Result)
  {
    NotifyAbort();
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::MasterPasswordPrompt()
{
  bool Retry;
  do
  {
    UnicodeString Password;
    Input(LoadStr(CONSOLE_MASTER_PASSWORD_PROMPT), Password, false, true);
    Retry = !WinConfiguration->ValidateMasterPassword(Password);
    if (Retry)
    {
      FConsole->PrintLine(LoadStr(MASTER_PASSWORD_INCORRECT));
    }
    else
    {
      WinConfiguration->SetMasterPassword(Password);
    }
  }
  while (Retry);

}
//---------------------------------------------------------------------------
UnicodeString TConsoleRunner::ExpandCommand(UnicodeString Command, TStrings * ScriptParameters)
{
  DebugAssert(ScriptParameters != NULL);
  for (int Index = 0; Index < ScriptParameters->Count; Index++)
  {
    Command = ReplaceStr(Command, FORMAT(L"%%%d%%", (Index+1)),
      ScriptParameters->Strings[Index]);
  }

  TDateTime N = Now();

  if (!FExternalTimestampVar)
  {
    Command =
      ReplaceStr(Command, FORMAT(L"%%%s%%", (TimestampVarName)), FormatDateTime(L"yyyymmddhhnnss", N));
  }

  int Offset = 1;
  int P2;
  do
  {
    int P = Pos(UpperCase(L"%" + TimestampVarName), UpperCase(Command), Offset);
    if (P > 0)
    {
      Offset = P + 1 + TimestampVarName.Length();
      P2 = Pos(L"%", Command, Offset);
      int P3 = Pos(L"#", Command, Offset);
      if ((P2 > 0) && (P3 > 0) && (P3 < P2) &&
          ((P3 == Offset) || (Command[Offset] == L'+') || (Command[Offset] == L'-')))
      {
        bool Valid = true;
        TDateTime T = N;
        if (P3 > Offset)
        {
          bool Add = (Command[Offset] == L'+');
          Offset++;
          Valid = TryRelativeStrToDateTime(Command.SubString(Offset, P3 - Offset), T, Add);
        }

        Offset = P3 + 1;
        if (Valid)
        {
          UnicodeString TimestampFormat = Command.SubString(Offset, P2 - Offset);
          UnicodeString TimestampValue = FormatDateTime(TimestampFormat, T);
          Command = Command.SubString(1, P - 1) + TimestampValue + Command.SubString(P2 + 1, Command.Length() - P2);
          Offset = P + TimestampValue.Length();
        }
      }
    }
    else
    {
      P2 = 0;
    }
  }
  while (P2 > 0);

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
int TConsoleRunner::Run(
  const UnicodeString Session, TOptions * Options, TStrings * ScriptCommands, TStrings * ScriptParameters,
  bool UsageWarnings)
{
  int ExitCode;
  try
  {
    bool AnyError = false;

    try
    {
      FScript = new TManagementScript(StoredSessions, FConsole->HasFlag(cfLimitedOutput));

      FScript->CopyParam = GUIConfiguration->DefaultCopyParam;
      FScript->SynchronizeParams = GUIConfiguration->SynchronizeParams;
      FScript->WantsProgress = FConsole->HasFlag(cfWantsProgress);
      FScript->OnPrint = ScriptPrint;
      FScript->OnPrintProgress = ScriptPrintProgress;
      FScript->OnInput = ScriptInput;
      FScript->OnTerminalPromptUser = ScriptTerminalPromptUser;
      FScript->OnShowExtendedException = ScriptShowExtendedException;
      FScript->OnTerminalQueryUser = ScriptTerminalQueryUser;
      FScript->OnQueryCancel = ScriptQueryCancel;
      FScript->OnSynchronizeStartStop = ScriptSynchronizeStartStop;
      FScript->OnProgress = ScriptProgress;
      FScript->UsageWarnings = UsageWarnings;
      if (FConsole->HasFlag(cfStdOut))
      {
        FScript->OnTransferOut = ScriptTransferOut;
      }
      if (FConsole->HasFlag(cfStdIn))
      {
        FScript->OnTransferIn = ScriptTransferIn;
      }

      UpdateTitle();

      // everything until the first manually entered command is "batch"
      // (including opening session from command line and script file)
      FBatchScript = true;

      if (!Session.IsEmpty())
      {
        if (UsageWarnings)
        {
          PrintMessage(LoadStr(SCRIPT_CMDLINE_SESSION));
        }
        FCommandError = false;
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
          if (FBatchScript)
          {
            // no longer batch
            FBatchScript = false;
            FScript->StartInteractive();
          }
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

    if (FLastProgressLen > 0)
    {
      FConsole->Print(L"\n");
      FLastProgressLen = 0;
    }

    ExitCode = (AnyError || FAborted) ? RESULT_ANY_ERROR : RESULT_SUCCESS;

    if (FScript != NULL)
    {
      UnicodeString ExitCodeMessage = FORMAT(L"Exit code: %d", (ExitCode));
      FScript->Log(llMessage, ExitCodeMessage);
      if (Configuration->ActualLogProtocol >= 1)
      {
        FConsole->Print(ExitCodeMessage + L"\n");
        UnicodeString LogMessage = FConsole->FinalLogMessage();
        if (!LogMessage.IsEmpty())
        {
          FScript->Log(llMessage, LogMessage);
          FConsole->Print(LogMessage + L"\n");
        }
      }
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
    NewTitle = FormatMainFormCaption(FScript->Terminal->SessionData->SessionName);
  }
  else
  {
    NewTitle = FormatMainFormCaption(L"");
  }
  FConsole->SetTitle(NewTitle);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ConfigurationChange(TObject * /*Sender*/)
{
  if (FScript != NULL)
  {
    FScript->ReflectSettings();
  }
}
//---------------------------------------------------------------------------
static UnicodeString __fastcall GetExeBaseName()
{
  return ExtractFileBaseName(Application->ExeName);
}
//---------------------------------------------------------------------------
static void __fastcall PrintUsageSyntax(TConsole * Console, const UnicodeString & Str)
{
  Console->PrintLine(GetExeBaseName() + L" " + Str);
}
//---------------------------------------------------------------------------
typedef std::vector<std::pair<UnicodeString, UnicodeString> > TSwitchesUsage;
//---------------------------------------------------------------------------
static void __fastcall RegisterSwitch(
  TSwitchesUsage & SwitchesUsage, const UnicodeString & Name, const UnicodeString & Desc)
{
  SwitchesUsage.push_back(std::make_pair(LowerCase(Name), Desc));
}
//---------------------------------------------------------------------------
static void __fastcall RegisterSwitch(
  TSwitchesUsage & SwitchesUsage, const UnicodeString & Name, int DescID)
{
  UnicodeString Desc = LoadStr(DescID);
  Desc = ReplaceText(Desc, L"%APP%", GetExeBaseName());
  RegisterSwitch(SwitchesUsage, Name, Desc);
}
//---------------------------------------------------------------------------
void __fastcall Usage(TConsole * Console)
{
  Console->PrintLine(FORMAT(L"WinSCP, %s", (Configuration->VersionStr)));
  UnicodeString Copyright =
    ReplaceText(LoadStr(WINSCP_COPYRIGHT), L"©", L"(c)");
  Console->PrintLine(Copyright);
  Console->PrintLine();
  Console->PrintLine(LoadStr(USAGE_SYNTAX_LABEL));

  bool CommandLineOnly = Console->HasFlag(cfCommandLineOnly);
  if (!CommandLineOnly)
  {
    PrintUsageSyntax(Console, L"site|workspace|folder");
    PrintUsageSyntax(Console, L"(sftp|scp|ftp[es]|dav[s]|s3)://[user[:password]@]host[:port][/path/[file]]");
    PrintUsageSyntax(Console, FORMAT(L"[mysession] /%s=<name>", (LowerCase(SESSIONNAME_SWICH))));
    PrintUsageSyntax(Console, L"[mysession] /newinstance");
    PrintUsageSyntax(Console, L"[mysession] /edit <path>");
    PrintUsageSyntax(Console, FORMAT(L"[mysession] /%s[=<file>]", (LowerCase(BROWSE_SWITCH))));
    PrintUsageSyntax(Console, FORMAT(L"[mysession] /%s [local_dir] [remote_dir] [/%s]", (LowerCase(SYNCHRONIZE_SWITCH), LowerCase(DEFAULTS_SWITCH))));
    PrintUsageSyntax(Console, FORMAT(L"[mysession] /%s [local_dir] [remote_dir] [/%s]", (LowerCase(KEEP_UP_TO_DATE_SWITCH), LowerCase(DEFAULTS_SWITCH))));
    PrintUsageSyntax(Console, FORMAT(L"[mysession] /%s [path]", (LowerCase(REFRESH_SWITCH))));
    PrintUsageSyntax(Console, FORMAT(L"[mysession] [/%s=<file> [/%s=<passphrase>]]", (LowerCase(PRIVATEKEY_SWITCH), PassphraseOption)));
    PrintUsageSyntax(Console, L"[mysession] [/hostkey=<fingerprint>]");
    PrintUsageSyntax(Console, FORMAT(L"[mysession] [/%s=<user> [/%s=<password>]]", (LowerCase(USERNAME_SWITCH), LowerCase(PASSWORD_SWITCH))));
    PrintUsageSyntax(Console, FORMAT(L"[mysession] [/%s]", (LowerCase(PASSWORDSFROMFILES_SWITCH))));
    PrintUsageSyntax(Console, FORMAT(L"[mysession] [/clientcert=<file> [/%s=<passphrase>]]", (PassphraseOption)));
    PrintUsageSyntax(Console, L"[mysession] [/certificate=<fingerprint>]");
    PrintUsageSyntax(Console, L"[mysession] [/passive[=on|off]] [/implicit|explicit]");
    PrintUsageSyntax(Console, L"[mysession] [/timeout=<sec>]");
    PrintUsageSyntax(Console, L"[mysession] [/rawsettings setting1=value1 setting2=value2 ...]");
  }
  PrintUsageSyntax(Console,
    UnicodeString(!CommandLineOnly ? L"[/console] " : L"") +
    FORMAT(L"[/script=<file>] [/%s cmd1...] [/parameter // param1...]", (LowerCase(COMMAND_SWITCH))));
  if (CommandLineOnly)
  {
    PrintUsageSyntax(
      Console, FORMAT(L"[/%s[=%s|%s]] [/%s]",
      (LowerCase(STDOUT_SWITCH), STDINOUT_BINARY_VALUE, STDINOUT_CHUNKED_VALUE, LowerCase(STDIN_SWITCH))));
  }
  PrintUsageSyntax(Console,
    FORMAT(L"[/%s=<logfile> [/loglevel=<level>]] [/%s=[<count>%s]<size>]", (LowerCase(LOG_SWITCH), LowerCase(LOGSIZE_SWITCH), LOGSIZE_SEPARATOR)));
  if (!CommandLineOnly)
  {
    PrintUsageSyntax(Console, L"[/xmllog=<logfile> [/xmlgroups]]");
  }
  else
  {
    PrintUsageSyntax(Console, FORMAT(L"[/xmllog=<logfile> [/xmlgroups]] [/%s]", (LowerCase(NOINTERACTIVEINPUT_SWITCH))));
  }
  PrintUsageSyntax(Console,
    FORMAT(L"[/%s=<inifile>]", (LowerCase(INI_SWITCH))));
  PrintUsageSyntax(Console, FORMAT(L"[/%s config1=value1 config2=value2 ...]", (LowerCase(RAW_CONFIG_SWITCH))));
  PrintUsageSyntax(Console, FORMAT(L"[/%s setting1=value1 setting2=value2 ...]", (LowerCase(RAWTRANSFERSETTINGS_SWITCH))));
  PrintUsageSyntax(Console, L"/batchsettings <site_mask> setting1=value1 setting2=value2 ...");
  PrintUsageSyntax(Console, FORMAT(L"/%s keyfile [/%s=<file>] [/%s]", (
    LowerCase(KEYGEN_SWITCH), LowerCase(KEYGEN_OUTPUT_SWITCH), LowerCase(KEYGEN_CHANGE_PASSPHRASE_SWITCH))));
  PrintUsageSyntax(Console, FORMAT(L"/%s keyfile [/%s=<text>] [/%s=<file>]",
    (LowerCase(KEYGEN_SWITCH), LowerCase(KEYGEN_COMMENT_SWITCH), LowerCase(KEYGEN_CERTIFICATE_SWITCH))));
  PrintUsageSyntax(Console, FORMAT(L"/%s /%s=<file> mysession", (
    LowerCase(COPYID_SWITCH), LowerCase(IDENTITY_SWITCH))));
  if (!CommandLineOnly)
  {
    PrintUsageSyntax(Console, L"/update");
  }
  PrintUsageSyntax(Console, TProgramParams::FormatSwitch(LowerCase(INFO_SWITCH)));
  PrintUsageSyntax(Console, L"/help");

  Console->PrintLine();

  TSwitchesUsage SwitchesUsage;
  if (!CommandLineOnly)
  {
    RegisterSwitch(SwitchesUsage, L"session", USAGE_SESSION);
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(SESSIONNAME_SWICH) + L"=", USAGE_SESSIONNAME);
    RegisterSwitch(SwitchesUsage, L"/newinstance", USAGE_NEWINSTANCE);
    RegisterSwitch(SwitchesUsage, L"/edit", USAGE_EDIT);
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(BROWSE_SWITCH), USAGE_BROWSE);
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(SYNCHRONIZE_SWITCH), USAGE_SYNCHRONIZE);
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(KEEP_UP_TO_DATE_SWITCH), USAGE_KEEPUPTODATE);
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(REFRESH_SWITCH), USAGE_REFRESH);
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(DEFAULTS_SWITCH), USAGE_DEFAULTS);
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(PRIVATEKEY_SWITCH) + L"=", USAGE_PRIVATEKEY);
    RegisterSwitch(SwitchesUsage, L"/hostkey=", USAGE_HOSTKEY);
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(USERNAME_SWITCH), USAGE_USERNAME);
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(PASSWORD_SWITCH), USAGE_PASSWORD);
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(PASSWORDSFROMFILES_SWITCH), USAGE_PASSWORDSFROMFILES);
    RegisterSwitch(SwitchesUsage, L"/clientcert=", USAGE_CLIENTCERT);
    RegisterSwitch(SwitchesUsage, L"/certificate=", USAGE_CERTIFICATE);
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(PassphraseOption) + L"=", USAGE_PASSPHRASE);
    RegisterSwitch(SwitchesUsage, L"/passive=", USAGE_PASSIVE);
    RegisterSwitch(SwitchesUsage, L"/implicit", USAGE_IMPLICIT);
    RegisterSwitch(SwitchesUsage, L"/explicit", USAGE_EXPLICIT);
    RegisterSwitch(SwitchesUsage, L"/timeout=", USAGE_TIMEOUT);
    RegisterSwitch(SwitchesUsage, L"/rawsettings", USAGE_RAWSETTINGS);
    RegisterSwitch(SwitchesUsage, L"/console", USAGE_CONSOLE);
  }
  RegisterSwitch(SwitchesUsage, L"/script=", USAGE_SCRIPT);
  RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(COMMAND_SWITCH), USAGE_COMMAND);
  RegisterSwitch(SwitchesUsage, L"/parameter", USAGE_PARAMETER);
  if (CommandLineOnly)
  {
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(STDOUT_SWITCH), USAGE_STDOUT);
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(STDIN_SWITCH), USAGE_STDIN);
  }
  RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(LOG_SWITCH) + L"=", USAGE_LOG);
  RegisterSwitch(SwitchesUsage, L"/loglevel=", USAGE_LOGLEVEL);
  RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(LOGSIZE_SWITCH) + L"=", USAGE_LOGSIZE);
  RegisterSwitch(SwitchesUsage, L"/xmllog=", USAGE_XMLLOG);
  RegisterSwitch(SwitchesUsage, L"/xmlgroups", USAGE_XMLGROUPS);
  if (CommandLineOnly)
  {
    RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(NOINTERACTIVEINPUT_SWITCH), USAGE_INTERACTIVEINPUT);
  }
  RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(INI_SWITCH) + L"=", USAGE_INI);
  RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(RAW_CONFIG_SWITCH), USAGE_RAWCONFIG);
  RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(RAWTRANSFERSETTINGS_SWITCH), USAGE_RAWTRANSFERSETTINGS);
  RegisterSwitch(SwitchesUsage, L"/batchsettings", USAGE_BATCHSETTINGS);
  UnicodeString KeyGenDesc =
    FMTLOAD(USAGE_KEYGEN2, (
      TProgramParams::FormatSwitch(LowerCase(KEYGEN_OUTPUT_SWITCH)) + L"=",
      TProgramParams::FormatSwitch(LowerCase(KEYGEN_CHANGE_PASSPHRASE_SWITCH)),
      TProgramParams::FormatSwitch(LowerCase(KEYGEN_COMMENT_SWITCH)) + L"=",
      TProgramParams::FormatSwitch(LowerCase(KEYGEN_CERTIFICATE_SWITCH)) + L"="));
  RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(KEYGEN_SWITCH), KeyGenDesc);
  RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(COPYID_SWITCH), USAGE_COPYID);
  if (!CommandLineOnly)
  {
    RegisterSwitch(SwitchesUsage, L"/update", USAGE_UPDATE);
  }
  RegisterSwitch(SwitchesUsage, TProgramParams::FormatSwitch(INFO_SWITCH), USAGE_INFO);
  RegisterSwitch(SwitchesUsage, L"/help", USAGE_HELP);

  int MaxSwitchLen = 0;
  TSwitchesUsage::const_iterator Index = SwitchesUsage.begin();
  while (Index != SwitchesUsage.end())
  {
    MaxSwitchLen = std::max(Index->first.Length(), MaxSwitchLen);
    ++Index;
  }

  Index = SwitchesUsage.begin();
  while (Index != SwitchesUsage.end())
  {
    UnicodeString Label =
      UnicodeString(L" ") +
      Index->first +
      UnicodeString::StringOfChar(L' ', MaxSwitchLen - Index->first.Length()) +
      L" ";
    Console->Print(Label);

    const int ConsoleWidth = 80;
    int DescWidth = ConsoleWidth - Label.Length() - 1;

    bool FirstLine = true;
    UnicodeString Desc = Index->second;
    while (!Desc.IsEmpty())
    {
      UnicodeString DescLine = CutToChar(Desc, L'\n', true);
      DescLine = WrapText(DescLine, L"\n", TSysCharSet() << L' ', DescWidth);
      while (!DescLine.IsEmpty())
      {
        UnicodeString DescLineLine = CutToChar(DescLine, L'\n', true);
        if (!FirstLine)
        {
          DescLineLine =
            UnicodeString::StringOfChar(L' ', Label.Length()) +
            DescLineLine;
        }
        FirstLine = false;
        Console->PrintLine(DescLineLine);
      }
    }
    ++Index;
  }

  Console->WaitBeforeExit();
}
//---------------------------------------------------------------------------
int __fastcall HandleException(TConsole * Console, Exception & E)
{
  UnicodeString Message;
  if (ExceptionFullMessage(&E, Message))
  {
    Console->Print(Message);
  }
  return RESULT_ANY_ERROR;
}
//---------------------------------------------------------------------------
int __fastcall BatchSettings(TConsole * Console, TProgramParams * Params)
{
  int Result = RESULT_SUCCESS;
  try
  {
    std::unique_ptr<TStrings> Arguments(new TStringList());
    if (!DebugAlwaysTrue(Params->FindSwitch(L"batchsettings", Arguments.get())))
    {
      Abort();
    }
    else
    {
      if (Arguments->Count < 1)
      {
        throw Exception(LoadStr(BATCH_SET_NO_MASK));
      }
      else if (Arguments->Count < 2)
      {
        throw Exception(LoadStr(BATCH_SET_NO_SETTINGS));
      }
      else
      {
        TFileMasks Mask(Arguments->Strings[0]);
        Arguments->Delete(0);

        std::unique_ptr<TOptionsStorage> OptionsStorage(new TOptionsStorage(Arguments.get(), false));

        int Matches = 0;
        int Changes = 0;

        for (int Index = 0; Index < StoredSessions->Count; Index++)
        {
          TSessionData * Data = StoredSessions->Sessions[Index];
          if (!Data->IsWorkspace &&
              Mask.Matches(Data->Name, false, false))
          {
            Matches++;
            std::unique_ptr<TSessionData> OriginalData(new TSessionData(L""));
            OriginalData->CopyDataNoRecrypt(Data);
            Data->ApplyRawSettings(OptionsStorage.get(), false, true);
            bool Changed = !OriginalData->IsSame(Data, false);
            if (Changed)
            {
              Changes++;
            }
            UnicodeString StateStr = LoadStr(Changed ? BATCH_SET_CHANGED : BATCH_SET_NOT_CHANGED);
            Console->PrintLine(Data->Name + TitleSeparator + StateStr);
          }
        }

        StoredSessions->Save(false, true); // explicit
        Console->PrintLine(FMTLOAD(BATCH_SET_SUMMARY, (Matches, Changes)));
      }
    }
  }
  catch (Exception & E)
  {
    Result = HandleException(Console, E);
  }

  Console->WaitBeforeExit();
  return Result;
}
//---------------------------------------------------------------------------
bool FindNixCompatibleSwitch(
  TProgramParams * Params, const UnicodeString & Name, const UnicodeString & NixName, UnicodeString & Value, bool & Set)
{
  bool Result =
    !Name.IsEmpty() &&
    Params->FindSwitch(Name, Value, Set);
  if (!Result)
  {
    std::unique_ptr<TStrings> Args(new TStringList());
    Result = Params->FindSwitchCaseSensitive(NixName, Args.get(), 1);
    if (Result && (Args->Count >= 1))
    {
      Value = Args->Strings[0];
      Set = true;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall KeyGen(TConsole * Console, TProgramParams * Params)
{
  int Result = RESULT_SUCCESS;
  UnicodeString Passphrase;
  UnicodeString NewPassphrase;
  try
  {
    UnicodeString InputFileName;
    std::unique_ptr<TStrings> Args(new TStringList());
    if (!Params->FindSwitch(KEYGEN_SWITCH, Args.get(), 1) ||
        (Args->Count < 1) ||
        Args->Strings[0].IsEmpty())
    {
      throw Exception(LoadStr(KEYGEN_NO_INPUT));
    }
    InputFileName = Args->Strings[0];

    bool ValueSet;
    UnicodeString OutputFileName;
    FindNixCompatibleSwitch(Params, KEYGEN_OUTPUT_SWITCH, L"o", OutputFileName, ValueSet);

    UnicodeString NewComment;
    FindNixCompatibleSwitch(Params, KEYGEN_COMMENT_SWITCH, L"C", NewComment, ValueSet);

    bool ChangePassphrase;
    bool NewPassphraseSet;
    if (Params->FindSwitchCaseSensitive(L"P"))
    {
      ChangePassphrase = true;
      FindNixCompatibleSwitch(Params, EmptyStr, L"-new-passphrase", NewPassphrase, NewPassphraseSet);
    }
    else
    {
      ChangePassphrase = Params->FindSwitch(KEYGEN_CHANGE_PASSPHRASE_SWITCH, NewPassphrase, NewPassphraseSet);
    }

    UnicodeString Certificate;
    // It's --certificate in puttygen
    FindNixCompatibleSwitch(Params, KEYGEN_CERTIFICATE_SWITCH, L"-certificate", Certificate, ValueSet);

    FindNixCompatibleSwitch(Params, PassphraseOption, L"-old-passphrase", Passphrase, ValueSet);

    if (Params->ParamCount > 0)
    {
      throw Exception(LoadStr(TOO_MANY_PARAMS_ERROR));
    }

    TKeyType Type = KeyType(InputFileName);
    int Error = errno;
    switch (Type)
    {
      case ktSSH1:
        throw Exception(LoadStr(KEYGEN_SSH1));

      case ktSSH2:
        if (NewComment.IsEmpty() && !ChangePassphrase && Certificate.IsEmpty() &&
            (Configuration->KeyVersion == 0)) // We should better check for version change
        {
          throw Exception(LoadStr(KEYGEN_NO_ACTION));
        }
        break;

      case ktOpenSSHPEM:
      case ktOpenSSHNew:
      case ktSSHCom:
        if (OutputFileName.IsEmpty())
        {
          throw Exception(LoadStr(KEYGEN_NO_OUTPUT));
        }
        break;

      case ktSSH1Public:
      case ktSSH2PublicRFC4716:
      case ktSSH2PublicOpenSSH:
        throw Exception(LoadStr(KEYGEN_PUBLIC));

      case ktUnopenable:
        throw EOSExtException(FMTLOAD(KEY_TYPE_UNOPENABLE, (InputFileName)), Error);

      case ktOpenSSHAuto:
      default:
        DebugFail();
        // fallthru
      case ktUnknown:
        throw Exception(FMTLOAD(KEY_TYPE_UNKNOWN2, (InputFileName)));
    }

    UnicodeString Comment;
    if (IsKeyEncrypted(Type, InputFileName, Comment) &&
        Passphrase.IsEmpty())
    {
      Console->Print(StripHotkey(FMTLOAD(PROMPT_KEY_PASSPHRASE, (Comment))) + L" ");
      if (!Console->Input(Passphrase, false, 0) ||
          Passphrase.IsEmpty())
      {
        Abort();
      }
    }

    TPrivateKey * PrivateKey = LoadKey(Type, InputFileName, Passphrase);

    try
    {
      if (!NewComment.IsEmpty())
      {
        ChangeKeyComment(PrivateKey, NewComment);
      }

      if (!Certificate.IsEmpty())
      {
        AddCertificateToKey(PrivateKey, Certificate);
      }

      if (ChangePassphrase)
      {
        if (!NewPassphraseSet)
        {
          Console->Print(LoadStr(KEYGEN_PASSPHRASE) + L" ");
          if (!Console->Input(NewPassphrase, false, 0))
          {
            Abort();
          }

          Console->Print(LoadStr(KEYGEN_PASSPHRASE2) + L" ");
          UnicodeString NewPassphrase2;
          if (!Console->Input(NewPassphrase2, false, 0))
          {
            Abort();
          }

          if (NewPassphrase != NewPassphrase2)
          {
            Shred(NewPassphrase2);
            throw Exception(LoadStr(KEYGEN_PASSPHRASES_MISMATCH));
          }
        }
      }
      else
      {
        NewPassphrase = Passphrase;
      }

      if (OutputFileName.IsEmpty())
      {
        OutputFileName = InputFileName;
      }

      SaveKey(ktSSH2, OutputFileName, NewPassphrase, PrivateKey);

      Console->PrintLine(FMTLOAD(KEYGEN_SAVED, (OutputFileName)));
    }
    __finally
    {
      FreeKey(PrivateKey);
    }
  }
  catch (Exception & E)
  {
    Result = HandleException(Console, E);
  }

  Shred(Passphrase);
  Shred(NewPassphrase);

  Console->WaitBeforeExit();
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall FingerprintScan(TConsole * Console, TProgramParams * Params)
{
  int Result = RESULT_SUCCESS;
  try
  {
    CheckLogParam(Params);

    std::unique_ptr<TSessionData> SessionData;

    if (Params->ParamCount > 0)
    {
      UnicodeString SessionUrl = Params->Param[1];
      bool DefaultsOnly;
      SessionData.reset(StoredSessions->ParseUrl(SessionUrl, Params, DefaultsOnly));
      if (DefaultsOnly || !SessionData->CanLogin ||
          (!SessionData->UsesSsh && (SessionData->Ftps == ftpsNone)))
      {
        SessionData.reset(NULL);
      }
    }

    if (!SessionData)
    {
      throw Exception(LoadStr(FINGERPRINTSCAN_NEED_SECURE_SESSION));
    }

    std::unique_ptr<TTerminal> Terminal(new TTerminal(SessionData.get(), Configuration));
    UnicodeString SHA256;
    UnicodeString SHA1;
    UnicodeString MD5;
    Terminal->FingerprintScan(SHA256, SHA1, MD5);
    if (!SHA256.IsEmpty())
    {
      Console->PrintLine(FORMAT(L"SHA-256: %s", (SHA256)));
    }
    if (!SHA1.IsEmpty())
    {
      Console->PrintLine(FORMAT(L"SHA-1: %s", (SHA1)));
    }
    if (!MD5.IsEmpty())
    {
      Console->PrintLine(FORMAT(L"MD5:     %s", (MD5)));
    }
  }
  catch (Exception & E)
  {
    Result = HandleException(Console, E);
  }

  Console->WaitBeforeExit();
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall DumpCallstack(TConsole * Console, TProgramParams * Params)
{
  int Result = RESULT_SUCCESS;
  try
  {
    int ProcessId = StrToInt(Params->SwitchValue(DUMPCALLSTACK_SWITCH));
    UnicodeString EventName = DumpCallstackEventName(ProcessId);
    UnicodeString FileName = DumpCallstackFileName(ProcessId);
    if (FileExists(FileName))
    {
      DeleteFileChecked(FileName);
    }

    HANDLE Event = OpenEvent(EVENT_ALL_ACCESS, false, EventName.c_str());
    if (Event == NULL)
    {
      throw ExtException(FORMAT(L"Error communicating with process %d.", (ProcessId)), LastSysErrorMessage());
    }

    SetEvent(Event);
    CloseHandle(Event);

    Console->PrintLine(FORMAT(L"Requested callstack dump for process %d...", (ProcessId)));

    int Timeout = 30;
    while (!FileExists(FileName))
    {
      Sleep(1000);
      Timeout--;
      if (Timeout == 0)
      {
        throw Exception(L"Timeout");
      }
    }

    Console->PrintLine(FORMAT(L"Callstack dumped to file \"%s\".", (FileName)));
  }
  catch (Exception & E)
  {
    Result = HandleException(Console, E);
  }

  Console->WaitBeforeExit();
  return Result;
}
//---------------------------------------------------------------------------
void static PrintList(TConsole * Console, const UnicodeString & Caption, TStrings * List)
{
  Console->PrintLine(Caption);
  for (int Index = 0; Index < List->Count; Index++)
  {
    Console->PrintLine(List->Strings[Index]);
  }
  Console->PrintLine();
}
//---------------------------------------------------------------------------
void static PrintListAndFree(TConsole * Console, const UnicodeString & Caption, TStrings * List)
{
  std::unique_ptr<TStrings> Owner(List);
  PrintList(Console, Caption, List);
}
//---------------------------------------------------------------------------
int Info(TConsole * Console)
{
  int Result = RESULT_SUCCESS;
  try
  {
    Console->PrintLine(FORMAT(L"SSH implementation: PuTTY %s", (GetPuTTYVersion())));
    Console->PrintLine();
    PrintListAndFree(Console, L"SSH encryption ciphers:", SshCipherList());
    PrintListAndFree(Console, L"SSH key exchange algorithms:", SshKexList());
    PrintListAndFree(Console, L"SSH host key algorithms:", SshHostKeyList());
    PrintListAndFree(Console, L"SSH MAC algorithms:", SshMacList());

    Console->PrintLine(TSessionLog::GetSeparator());

    Console->PrintLine(FORMAT(L"TLS/SSL implementation: %s", (GetOpenSSLVersionText())));
    Console->PrintLine();
    PrintListAndFree(Console, L"TLS/SSL cipher suites:", TlsCipherList());
  }
  catch (Exception & E)
  {
    Result = HandleException(Console, E);
  }

  Console->WaitBeforeExit();
  return Result;
}
//---------------------------------------------------------------------------
TStdInOutMode ParseStdInOutMode(TProgramParams * Params, const UnicodeString & Switch, bool AllowChunked)
{
  TStdInOutMode Result;
  UnicodeString Value;
  if (!Params->FindSwitch(Switch, Value))
  {
    Result = TConsoleCommStruct::TInitEvent::OFF;
  }
  else
  {
    if (Value.IsEmpty() || SameText(Value, STDINOUT_BINARY_VALUE))
    {
      Result = TConsoleCommStruct::TInitEvent::BINARY;
    }
    else if (SameText(Value, STDINOUT_CHUNKED_VALUE) && AllowChunked)
    {
      Result = TConsoleCommStruct::TInitEvent::CHUNKED;
    }
    else
    {
      throw Exception(FORMAT(SCRIPT_VALUE_UNKNOWN, (Value, Switch))); // abuse of the string
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall Console(TConsoleMode Mode)
{
  DebugAssert(Mode != cmNone);
  TProgramParams * Params = TProgramParams::Instance();
  int Result = RESULT_SUCCESS;
  TConsole * Console = NULL;
  TConsoleRunner * Runner = NULL;
  TStrings * ScriptCommands = new TStringList();
  TStrings * ScriptParameters = new TStringList();
  try
  {
    UnicodeString ConsoleInstance;
    // First check for /consoleinstance as /console is always used by winscp.com
    if (Params->FindSwitch(L"consoleinstance", ConsoleInstance))
    {
      Configuration->Usage->Inc(L"ConsoleExternal");
      TStdInOutMode StdOut = ParseStdInOutMode(Params, STDOUT_SWITCH, true);
      TStdInOutMode StdIn = ParseStdInOutMode(Params, STDIN_SWITCH, false);
      bool NoInteractiveInput = Params->FindSwitch(NOINTERACTIVEINPUT_SWITCH) || (StdIn != TConsoleCommStruct::TInitEvent::OFF);
      Console = new TExternalConsole(ConsoleInstance, NoInteractiveInput, StdOut, StdIn);
    }
    else if (Params->FindSwitch(L"Console") || (Mode != cmScripting))
    {
      Configuration->Usage->Inc(L"ConsoleOwn");
      Console = TOwnConsole::Instance();
    }
    else
    {
      Configuration->Usage->Inc(L"ConsoleNull");
      Console = new TNullConsole();
    }

    SetNoGUI();

    if (Mode == cmHelp)
    {
      Configuration->Usage->Inc(L"UsageShown");
      Usage(Console);
    }
    else if (Mode == cmBatchSettings)
    {
      if (CheckSafe(Params))
      {
        Configuration->Usage->Inc(L"BatchSettings");
        Result = BatchSettings(Console, Params);
      }
    }
    else if (Mode == cmKeyGen)
    {
      if (CheckSafe(Params))
      {
        Configuration->Usage->Inc(L"KeyGen");
        Result = KeyGen(Console, Params);
      }
    }
    else if (Mode == cmFingerprintScan)
    {
      if (CheckSafe(Params))
      {
        Configuration->Usage->Inc(L"FingerprintScan");
        Result = FingerprintScan(Console, Params);
      }
    }
    else if (Mode == cmDumpCallstack)
    {
      Result = DumpCallstack(Console, Params);
    }
    else if (Mode == cmInfo)
    {
      Result = Info(Console);
    }
    else if (Mode == cmComRegistration)
    {
      if (CheckSafe(Params))
      {
        Result = ComRegistration(Console);
      }
    }
    else
    {
      Runner = new TConsoleRunner(Console);

      try
      {
        if (CheckSafe(Params))
        {
          UnicodeString Value;
          if (Mode == cmCopyId)
          {
            Configuration->Usage->Inc(L"CopyId");
            UnicodeString IdentityFileName;
            bool ValueSet;
            FindNixCompatibleSwitch(Params, IDENTITY_SWITCH, L"i", IdentityFileName, ValueSet);
            if (IdentityFileName.IsEmpty())
            {
              throw Exception(LoadStr(COPYID_IDETITY_MISSING));
            }
            ScriptCommands->Add(FORMAT(L"%s \"%s\"", (COPYID_COMMAND, IdentityFileName)));
            ScriptCommands->Add(EXIT_COMMAND);
          }
          else if (Params->FindSwitch(SCRIPT_SWITCH, Value) && !Value.IsEmpty())
          {
            Configuration->Usage->Inc(L"ScriptFile");
            LoadScriptFromFile(Value, ScriptCommands);
          }
          Params->FindSwitch(COMMAND_SWITCH, ScriptCommands);
          if (ScriptCommands->Count > 0)
          {
            Configuration->Usage->Inc(L"ScriptCommands");
          }
          Params->FindSwitch(L"parameter", ScriptParameters);
          if (ScriptParameters->Count > 0)
          {
            Configuration->Usage->Inc(L"ScriptParameters");
          }
        }

        UnicodeString Session;
        if (Params->ParamCount >= 1)
        {
          Session = Params->Param[1];
          if (Params->ParamCount > 1)
          {
            // Check if the pending parameters will be consumed by ParseUrl (/rawsettings) in TManagementScript::Connect.
            // This way we parse the options twice, but we do not want to refactor the code just for nicer test for this minor warning.
            TOptions OptionsCopy(*Params);
            bool DefaultsOnly = false;
            StoredSessions->ParseUrl(Session, &OptionsCopy, DefaultsOnly);
            if (OptionsCopy.ParamCount > 1)
            {
              Runner->PrintMessage(LoadStr(SCRIPT_CMDLINE_PARAMETERS));
            }
          }
        }

        CheckLogParam(Params);
        CheckXmlLogParam(Params);

        bool UsageWarnings = (Mode != cmCopyId) && ((ScriptCommands->Count > 0) || !Console->HasFlag(cfInteractive));
        Result = Runner->Run(Session, Params, ScriptCommands, ScriptParameters, UsageWarnings);
      }
      catch(Exception & E)
      {
        Runner->ShowException(&E);
        Result = RESULT_ANY_ERROR;
      }

      if (Mode == cmCopyId)
      {
        Console->WaitBeforeExit();
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
