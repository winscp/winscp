//---------------------------------------------------------------------------
#include <stdexcept>
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

#include "Console.h"
#define MAX_ATTEMPTS 10
//---------------------------------------------------------------------------
#define LENOF(x) ( (sizeof((x))) / (sizeof(*(x))))
//---------------------------------------------------------------------------
using namespace std;
HANDLE ConsoleInput = NULL;
HANDLE ConsoleOutput = NULL;
HANDLE Child = NULL;
HANDLE CancelEvent = NULL;
HANDLE InputTimerEvent = NULL;
unsigned int OutputType = FILE_TYPE_UNKNOWN;
unsigned int InputType = FILE_TYPE_UNKNOWN;
enum { RESULT_GLOBAL_ERROR = 1, RESULT_INIT_ERROR = 2, RESULT_PROCESSING_ERROR = 3,
  RESULT_UNKNOWN_ERROR = 4 };
const wchar_t* CONSOLE_CHILD_PARAM = L"consolechild";
//---------------------------------------------------------------------------
inline TConsoleCommStruct* GetCommStruct(HANDLE FileMapping)
{
  TConsoleCommStruct* Result;
  Result = static_cast<TConsoleCommStruct*>(MapViewOfFile(FileMapping,
    FILE_MAP_ALL_ACCESS, 0, 0, 0));
  if (Result == NULL)
  {
    throw runtime_error("Cannot open mapping object.");
  }
  return Result;
}
//---------------------------------------------------------------------------
inline void FreeCommStruct(TConsoleCommStruct* CommStruct)
{
  UnmapViewOfFile(CommStruct);
}
//---------------------------------------------------------------------------
void InitializeConsole(wchar_t* InstanceName, HANDLE& RequestEvent, HANDLE& ResponseEvent,
  HANDLE& CancelEvent, HANDLE& FileMapping, HANDLE& Job)
{
  unsigned int Process = GetCurrentProcessId();

  int Attempts = 0;
  wchar_t Name[MAX_PATH];
  bool UniqEvent;

  do
  {
    if (Attempts > MAX_ATTEMPTS)
    {
      throw runtime_error("Cannot find unique name for event object.");
    }

    int InstanceNumber;
    #ifdef CONSOLE_TEST
    InstanceNumber = 1;
    #else
    InstanceNumber = random(1000);
    #endif
    swprintf(InstanceName, L"_%u_%d", Process, InstanceNumber);
    swprintf(Name, L"%s%s", CONSOLE_EVENT_REQUEST, InstanceName);
    HANDLE EventHandle = OpenEvent(EVENT_ALL_ACCESS, false, Name);
    UniqEvent = (EventHandle == NULL);
    if (!UniqEvent)
    {
      CloseHandle(EventHandle);
    }
    Attempts++;
  }
  while (!UniqEvent);

  RequestEvent = CreateEvent(NULL, false, false, Name);
  if (RequestEvent == NULL)
  {
    throw runtime_error("Cannot create request event object.");
  }

  swprintf(Name, L"%s%s", CONSOLE_EVENT_RESPONSE, InstanceName);
  ResponseEvent = CreateEvent(NULL, false, false, Name);
  if (ResponseEvent == NULL)
  {
    throw runtime_error("Cannot create response event object.");
  }

  swprintf(Name, L"%s%s", CONSOLE_EVENT_CANCEL, InstanceName);
  CancelEvent = CreateEvent(NULL, false, false, Name);
  if (CancelEvent == NULL)
  {
    throw runtime_error("Cannot create cancel event object.");
  }

  swprintf(Name, L"%s%s", CONSOLE_MAPPING, InstanceName);
  FileMapping = CreateFileMapping((HANDLE)0xFFFFFFFF, NULL, PAGE_READWRITE,
    0, sizeof(TConsoleCommStruct), Name);
  if (FileMapping == NULL)
  {
    throw runtime_error("Cannot create mapping object.");
  }

  typedef HANDLE WINAPI (*TCreateJobObject)(LPSECURITY_ATTRIBUTES JobAttributes, LPCTSTR Name);
  typedef HANDLE WINAPI (*TSetInformationJobObject)(HANDLE Job, JOBOBJECTINFOCLASS JobObjectInformationClass,
    LPVOID JobObjectInformation, DWORD JobObjectInformationLength);

  HANDLE Kernel32 = GetModuleHandle(L"kernel32");
  TCreateJobObject CreateJobObject =
    (TCreateJobObject)GetProcAddress(Kernel32, "CreateJobObjectW");
  TSetInformationJobObject SetInformationJobObject =
    (TSetInformationJobObject)GetProcAddress(Kernel32, "SetInformationJobObject");
  if ((CreateJobObject != NULL) && (SetInformationJobObject != NULL))
  {
    swprintf(Name, L"%s%s", CONSOLE_JOB, InstanceName);
    Job = CreateJobObject(NULL, Name);
    if (Job == NULL)
    {
      throw runtime_error("Cannot create job object.");
    }

    JOBOBJECT_EXTENDED_LIMIT_INFORMATION ExtendedLimitInformation;
    memset(&ExtendedLimitInformation, 0, sizeof(ExtendedLimitInformation));
    ExtendedLimitInformation.BasicLimitInformation.LimitFlags =
      JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if (SetInformationJobObject(Job, JobObjectExtendedLimitInformation,
          &ExtendedLimitInformation, sizeof(ExtendedLimitInformation)) == 0)
    {
      CloseHandle(Job);
      Job = NULL;
    }
  }
  else
  {
    Job = NULL;
  }

  TConsoleCommStruct* CommStruct = GetCommStruct(FileMapping);
  CommStruct->Size = sizeof(TConsoleCommStruct);
  CommStruct->Version = TConsoleCommStruct::CurrentVersion;
  CommStruct->Event = TConsoleCommStruct::NONE;
  FreeCommStruct(CommStruct);
}
//---------------------------------------------------------------------------
// duplicated in Common.cpp
bool __fastcall CutToken(const wchar_t*& Str, wchar_t* Token)
{
  bool Result;

  // inspired by Putty's sftp_getcmd() from PSFTP.C
  int Length = wcslen(Str);
  int Index = 0;
  while ((Index < Length) &&
    ((Str[Index] == L' ') || (Str[Index] == L'\t')))
  {
    Index++;
  }

  if (Index < Length)
  {
    bool Quoting = false;

    while (Index < Length)
    {
      if (!Quoting && ((Str[Index] == L' ') || (Str[Index] == L'\t')))
      {
        break;
      }
      else if ((Str[Index] == L'"') && (Index + 1 < Length) &&
        (Str[Index + 1] == L'"'))
      {
        Index += 2;
        *Token = L'"';
        Token++;
      }
      else if (Str[Index] == L'"')
      {
        Index++;
        Quoting = !Quoting;
      }
      else
      {
        *Token = Str[Index];
        Token++;
        Index++;
      }
    }

    if (Index < Length)
    {
      Index++;
    }

    Str += Index;

    Result = true;
  }
  else
  {
    Result = false;
    Str += Length;
  }

  *Token = L'\0';

  return Result;
}
//---------------------------------------------------------------------------
void GetProductVersion(wchar_t* ProductVersion)
{
  wchar_t Buffer[MAX_PATH];
  DWORD ModuleNameLen = GetModuleFileName(NULL, Buffer, MAX_PATH);
  if ((ModuleNameLen == 0) || (ModuleNameLen == MAX_PATH))
  {
    throw runtime_error("Error retrieving executable name.");
  }
  ProductVersion[0] = '\0';
  unsigned long Handle;
  unsigned int Size = GetFileVersionInfoSize(Buffer, &Handle);
  if (Size > 0)
  {
    void * VersionInfo = new char[Size];
    VS_FIXEDFILEINFO* FixedFileInfo;
    unsigned int Length;
    if (GetFileVersionInfo(Buffer, Handle, Size, VersionInfo))
    {
      if (VerQueryValue(VersionInfo, L"\\", (void**)&FixedFileInfo, &Length))
      {
        int ProductMajor = HIWORD(FixedFileInfo->dwProductVersionMS);
        int ProductMinor = LOWORD(FixedFileInfo->dwProductVersionMS);
        int ProductBuild = HIWORD(FixedFileInfo->dwProductVersionLS);
        if ((ProductMajor >= 0) && (ProductMajor <= 9) &&
            (ProductMinor >= 0) && (ProductMinor <= 9) &&
            (ProductBuild >= 0) && (ProductBuild <= 9))
        {
          ProductVersion[0] = static_cast<wchar_t>(L'0' + ProductMajor);
          ProductVersion[1] = static_cast<wchar_t>(L'0' + ProductMinor);
          ProductVersion[2] = static_cast<wchar_t>(L'0' + ProductBuild);
          ProductVersion[3] = L'\0';
        }
      }
    }
    delete[] VersionInfo;
  }

  if (ProductVersion[0] == L'\0')
  {
    throw runtime_error("Error retrieving product version.");
  }
}
//---------------------------------------------------------------------------
void InitializeChild(const wchar_t* CommandLine, const wchar_t* InstanceName, HANDLE& Child)
{
  int SkipParam = 0;
  wchar_t ChildPath[MAX_PATH] = L"";

  size_t CommandLineLen = wcslen(CommandLine);
  wchar_t* Buffer = new wchar_t[(CommandLineLen > MAX_PATH ? CommandLineLen : MAX_PATH) + 1];

  int Count = 0;
  const wchar_t* P = CommandLine;
  while (CutToken(P, Buffer))
  {
    if ((wcschr(L"-/", Buffer[0]) != NULL) &&
        (wcsncmpi(Buffer + 1, CONSOLE_CHILD_PARAM, wcslen(CONSOLE_CHILD_PARAM)) == 0) &&
        (Buffer[wcslen(CONSOLE_CHILD_PARAM) + 1] == L'='))
    {
      SkipParam = Count;
      wcscpy(ChildPath, Buffer + 1 + wcslen(CONSOLE_CHILD_PARAM) + 1);
    }
    ++Count;
  }

  if (wcslen(ChildPath) == 0)
  {
    DWORD ModuleNameLen = GetModuleFileName(NULL, Buffer, MAX_PATH);
    if ((ModuleNameLen == 0) || (ModuleNameLen == MAX_PATH))
    {
      throw runtime_error("Error retrieving executable name.");
    }
    const wchar_t* LastDelimiter = wcsrchr(Buffer, L'\\');
    const wchar_t* AppFileName;
    if (LastDelimiter != NULL)
    {
      wcsncpy(ChildPath, Buffer, LastDelimiter - Buffer + 1);
      ChildPath[LastDelimiter - Buffer + 1] = L'\0';
      AppFileName = LastDelimiter + 1;
    }
    else
    {
      ChildPath[0] = L'\0';
      AppFileName = Buffer;
    }

    const wchar_t* ExtensionStart = wcsrchr(AppFileName, L'.');
    if (ExtensionStart != NULL)
    {
      wchar_t* End = ChildPath + wcslen(ChildPath);
      wcsncpy(End, AppFileName, ExtensionStart - AppFileName);
      *(End + (ExtensionStart - AppFileName)) = L'\0';
    }
    else
    {
      wcscat(ChildPath, AppFileName);
    }
    wcscat(ChildPath, L".exe");
  }

  wchar_t ProductVersion[4];
  GetProductVersion(ProductVersion);

  wchar_t* Parameters = new wchar_t[(CommandLineLen * 2) + 100 + (Count * 3) + 1];
  wsprintf(Parameters, L"\"%s\" /console=%s /consoleinstance=%s ", ChildPath, ProductVersion, InstanceName);
  P = CommandLine;
  // skip executable path
  CutToken(P, Buffer);
  int i = 1;
  while (CutToken(P, Buffer))
  {
    if (i != SkipParam)
    {
      wcscat(Parameters, L"\"");
      wchar_t* P2 = Parameters + wcslen(Parameters);
      const wchar_t* P3 = Buffer;
      const wchar_t* BufferEnd = Buffer + wcslen(Buffer) + 1;
      while (P3 != BufferEnd)
      {
        *P2 = *P3;
        ++P2;
        if (*P3 == L'"')
        {
          *P2 = L'"';
          ++P2;
        }
        ++P3;
      }

      wcscat(Parameters, L"\" ");
    }
    ++i;
  }

  delete[] Buffer;

  STARTUPINFO StartupInfo = { sizeof(STARTUPINFO) };
  PROCESS_INFORMATION ProcessInfomation;

  BOOL Result =
    CreateProcess(ChildPath, Parameters, NULL, NULL, false, 0, NULL, NULL,
      &StartupInfo, &ProcessInfomation);

  delete[] Parameters;

  if (Result)
  {
    Child = ProcessInfomation.hProcess;
  }
  else
  {
    throw runtime_error("Cannot start WinSCP application.");
  }
}
//---------------------------------------------------------------------------
void FinalizeChild(HANDLE Child)
{
  if (Child != NULL)
  {
    TerminateProcess(Child, 0);
    CloseHandle(Child);
  }
}
//---------------------------------------------------------------------------
void FinalizeConsole(const wchar_t* /*InstanceName*/, HANDLE RequestEvent,
  HANDLE ResponseEvent, HANDLE CancelEvent, HANDLE FileMapping, HANDLE Job)
{
  CloseHandle(RequestEvent);
  CloseHandle(ResponseEvent);
  CloseHandle(CancelEvent);
  CloseHandle(FileMapping);
  CloseHandle(Job);
}
//---------------------------------------------------------------------------
static wchar_t LastFromBeginning[sizeof(TConsoleCommStruct::TPrintEvent)] = L""; //???
//---------------------------------------------------------------------------
inline void Flush()
{
  if ((OutputType == FILE_TYPE_DISK) || (OutputType == FILE_TYPE_PIPE))
  {
    fflush(stdout);
  }
}
//---------------------------------------------------------------------------
void Print(const wchar_t* Message)
{
  int Size = WideCharToMultiByte(CP_UTF8, 0, Message, -1, 0, 0, 0, 0);
  if (Size > 0)
  {
    char* Buffer = new char[(Size * 2) + 1];
    if (WideCharToMultiByte(CP_UTF8, 0, Message, -1, Buffer, Size, 0, 0) > 0)
    {
      Buffer[Size] = '\0';
      char* Ptr = Buffer;
      while ((Ptr = strchr(Ptr, '\n')) != NULL)
      {
        memmove(Ptr + 1, Ptr, strlen(Ptr) + 1);
        *Ptr = '\r';
        Ptr += 2;
      }
      unsigned long Written;
      WriteFile(ConsoleOutput, Buffer, strlen(Buffer), &Written, NULL);
    }
    delete[] Buffer;
  }
}
//---------------------------------------------------------------------------
void Print(bool FromBeginning, const wchar_t* Message)
{
  if ((OutputType == FILE_TYPE_DISK) || (OutputType == FILE_TYPE_PIPE))
  {
    if (FromBeginning && (Message[0] != L'\n'))
    {
      wcscpy(LastFromBeginning, Message);
    }
    else
    {
      if (LastFromBeginning[0] != L'\0')
      {
        Print(LastFromBeginning);
        LastFromBeginning[0] = L'\0';
      }

      if (FromBeginning && (Message[0] == L'\n'))
      {
        Print(L"\n");
        wcscpy(LastFromBeginning, Message + 1);
      }
      else
      {
        Print(Message);
      }
      Flush();
    }
  }
  else
  {
    unsigned long Written;
    if (FromBeginning)
    {
      WriteConsole(ConsoleOutput, L"\r", 1, &Written, NULL);
    }
    WriteConsole(ConsoleOutput, Message, wcslen(Message), &Written, NULL);
  }
}
//---------------------------------------------------------------------------
inline void ProcessPrintEvent(TConsoleCommStruct::TPrintEvent& Event)
{
  Print(Event.FromBeginning, Event.Message);
}
//---------------------------------------------------------------------------
void CancelInput()
{
  SetEvent(CancelEvent);
}
//---------------------------------------------------------------------------
void BreakInput()
{
  FlushConsoleInputBuffer(ConsoleInput);
  INPUT_RECORD InputRecord;
  memset(&InputRecord, 0, sizeof(InputRecord));
  InputRecord.EventType = KEY_EVENT;
  InputRecord.Event.KeyEvent.bKeyDown = true;
  InputRecord.Event.KeyEvent.wRepeatCount = 1;
  InputRecord.Event.KeyEvent.uChar.UnicodeChar = L'\r';

  unsigned long Written;
  WriteConsoleInput(ConsoleInput, &InputRecord, 1, &Written);

  CancelInput();
}
//---------------------------------------------------------------------------
DWORD WINAPI InputTimerThreadProc(void* Parameter)
{
  unsigned int Timer = reinterpret_cast<unsigned int>(Parameter);

  if (WaitForSingleObject(InputTimerEvent, Timer) == WAIT_TIMEOUT)
  {
    BreakInput();
  }

  return 0;
}
//---------------------------------------------------------------------------
void ProcessInputEvent(TConsoleCommStruct::TInputEvent& Event)
{
  if ((InputType == FILE_TYPE_DISK) || (InputType == FILE_TYPE_PIPE))
  {
    unsigned long Bytes = 0;
    unsigned long Read;
    bool Result;
    char Ch;
    char Buf[LENOF(Event.Str) * 3];

    while (((Result = (ReadFile(ConsoleInput, &Ch, 1, &Read, NULL) != 0)) != false) &&
           (Read > 0) && (Bytes < LENOF(Buf) - 1) && (Ch != '\n'))
    {
      if (Ch != '\r')
      {
        Buf[Bytes] = Ch;
        Bytes++;
      }
    }
    Buf[Bytes] = L'\0';

    MultiByteToWideChar(CP_UTF8, 0, Buf, -1, Event.Str, LENOF(Event.Str) - 1);
    Event.Str[LENOF(Event.Str) - 1] = L'\0';

    Print(false, Event.Str);
    Print(false, L"\n");

    Event.Result = ((Result && (Read > 0)) || (Bytes > 0));
  }
  else
  {
    unsigned long PrevMode, NewMode;
    GetConsoleMode(ConsoleInput, &PrevMode);
    NewMode = PrevMode | ENABLE_PROCESSED_INPUT | ENABLE_LINE_INPUT;
    if (Event.Echo)
    {
      NewMode |= ENABLE_ECHO_INPUT;
    }
    else
    {
      NewMode &= ~ENABLE_ECHO_INPUT;
    }
    SetConsoleMode(ConsoleInput, NewMode);

    HANDLE InputTimerThread = NULL;

    try
    {
      if (Event.Timer > 0)
      {
        unsigned long ThreadId;
        InputTimerEvent = CreateEvent(NULL, false, false, NULL);
        InputTimerThread = CreateThread(NULL, 0, InputTimerThreadProc,
          reinterpret_cast<void *>(Event.Timer), 0, &ThreadId);
      }

      unsigned long Read;
      Event.Result = ReadConsole(ConsoleInput, Event.Str, LENOF(Event.Str) - 1, &Read, NULL);
      Event.Str[Read] = L'\0';

      bool PendingCancel = (WaitForSingleObject(CancelEvent, 0) == WAIT_OBJECT_0);
      if (PendingCancel || !Event.Echo)
      {
        WriteFile(ConsoleOutput, "\n", 1, NULL, NULL);
        Flush();
      }

      if (PendingCancel || (Read == 0))
      {
        Event.Result = false;
      }
    }
    __finally
    {
      if (InputTimerThread != NULL)
      {
        SetEvent(InputTimerEvent);
        WaitForSingleObject(InputTimerThread, 100);
        CloseHandle(InputTimerEvent);
        InputTimerEvent = NULL;
        CloseHandle(InputTimerThread);
      }
      SetConsoleMode(ConsoleInput, PrevMode);
    }
  }
}
//---------------------------------------------------------------------------
void ProcessChoiceEvent(TConsoleCommStruct::TChoiceEvent& Event)
{
  // note that if output is redirected to file, input is still FILE_TYPE_CHAR
  if ((InputType == FILE_TYPE_DISK) || (InputType == FILE_TYPE_PIPE))
  {
    if (Event.Timeouting)
    {
      Sleep(Event.Timer);
      Event.Result = Event.Timeouted;
    }
    else
    {
      Event.Result = Event.Break;
    }
  }
  else
  {
    Event.Result = 0;

    unsigned long PrevMode, NewMode;
    GetConsoleMode(ConsoleInput, &PrevMode);
    NewMode = (PrevMode | ENABLE_PROCESSED_INPUT) & ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT);
    SetConsoleMode(ConsoleInput, NewMode);

    unsigned int ATimer = Event.Timer;

    try
    {
      do
      {
        unsigned long Read;
        INPUT_RECORD Record;
        if ((PeekConsoleInput(ConsoleInput, &Record, 1, &Read) != 0) &&
            (Read == 1))
        {
          if ((ReadConsoleInput(ConsoleInput, &Record, 1, &Read) != 0) &&
              (Read == 1))
          {
            bool PendingCancel = (WaitForSingleObject(CancelEvent, 0) == WAIT_OBJECT_0);
            if (PendingCancel)
            {
              Event.Result = Event.Break;
            }
            else if ((Record.EventType == KEY_EVENT) &&
                     Record.Event.KeyEvent.bKeyDown)
            {
              wchar_t CStr[2];
              CStr[0] = Record.Event.KeyEvent.uChar.AsciiChar;
              CStr[1] = L'\0';
              CharUpperBuff(CStr, 1);
              wchar_t C = CStr[0];
              if (C == 27)
              {
                Event.Result = Event.Cancel;
              }
              else if ((wcschr(Event.Options, C) != NULL) &&
                       ((Record.Event.KeyEvent.dwControlKeyState &
                         (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED | LEFT_ALT_PRESSED |
                         RIGHT_ALT_PRESSED)) == 0))

              {
                Event.Result = wcschr(Event.Options, C) - Event.Options + 1;
              }
            }
          }
        }

        if (Event.Result == 0)
        {
          unsigned int TimerSlice = 50;
          Sleep(TimerSlice);
          if (Event.Timer > 0)
          {
            if (ATimer > TimerSlice)
            {
              ATimer -= TimerSlice;
            }
            else
            {
              Event.Result = Event.Timeouted;
            }
          }
        }
      }
      while (Event.Result == 0);

      SetConsoleMode(ConsoleInput, PrevMode);
    }
    catch(...)
    {
      SetConsoleMode(ConsoleInput, PrevMode);
      throw;
    }
  }
}
//---------------------------------------------------------------------------
inline void ProcessTitleEvent(TConsoleCommStruct::TTitleEvent& Event)
{
  SetConsoleTitle(Event.Title);
}
//---------------------------------------------------------------------------
inline void ProcessInitEvent(TConsoleCommStruct::TInitEvent& Event)
{
  Event.InputType = InputType;
  Event.OutputType = OutputType;
}
//---------------------------------------------------------------------------
void ProcessEvent(HANDLE ResponseEvent, HANDLE FileMapping)
{
  TConsoleCommStruct* CommStruct = GetCommStruct(FileMapping);
  try
  {
    if (CommStruct->Version != TConsoleCommStruct::CurrentVersionConfirmed)
    {
      throw runtime_error("Incompatible console protocol version");
    }

    switch (CommStruct->Event)
    {
      case TConsoleCommStruct::PRINT:
        ProcessPrintEvent(CommStruct->PrintEvent);
        break;

      case TConsoleCommStruct::INPUT:
        ProcessInputEvent(CommStruct->InputEvent);
        break;

      case TConsoleCommStruct::CHOICE:
        ProcessChoiceEvent(CommStruct->ChoiceEvent);
        break;

      case TConsoleCommStruct::TITLE:
        ProcessTitleEvent(CommStruct->TitleEvent);
        break;

      case TConsoleCommStruct::INIT:
        ProcessInitEvent(CommStruct->InitEvent);
        break;

      default:
        throw runtime_error("Unknown event");
    }

    FreeCommStruct(CommStruct);

    SetEvent(ResponseEvent);
  }
  catch(...)
  {
    FreeCommStruct(CommStruct);
    throw;
  }
}
//---------------------------------------------------------------------------
BOOL WINAPI HandlerRoutine(DWORD CtrlType)
{
  if ((CtrlType == CTRL_C_EVENT) || (CtrlType == CTRL_BREAK_EVENT))
  {
    CancelInput();

    return true;
  }
  else
  {
    FinalizeChild(Child);
    return false;
  }
}
//---------------------------------------------------------------------------
#pragma argsused
int wmain(int /*argc*/, wchar_t* /*argv*/[])
{
  unsigned long Result = RESULT_UNKNOWN_ERROR;

  try
  {
    randomize();

    OSVERSIONINFO VersionInfo;
    VersionInfo.dwOSVersionInfoSize = sizeof(VersionInfo);
    GetVersionEx(&VersionInfo);

    ConsoleInput = GetStdHandle(STD_INPUT_HANDLE);
    InputType = GetFileType(ConsoleInput);
    SetConsoleCtrlHandler(HandlerRoutine, true);

    unsigned int SavedConsoleCP = GetConsoleCP();
    unsigned int SavedConsoleOutputCP = GetConsoleOutputCP();

    ConsoleOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    OutputType = GetFileType(ConsoleOutput);


    bool SupportsUtf8ConsoleOutput =
      (VersionInfo.dwMajorVersion == 6) &&
      (VersionInfo.dwMinorVersion == 1);

    if ((InputType == FILE_TYPE_DISK) || (InputType == FILE_TYPE_PIPE) ||
        SupportsUtf8ConsoleOutput)
    {
      SetConsoleCP(CP_UTF8);
    }
    else
    {
      SetConsoleCP(CP_ACP);
    }

    if ((OutputType == FILE_TYPE_DISK) || (OutputType == FILE_TYPE_PIPE) ||
        SupportsUtf8ConsoleOutput)
    {
      SetConsoleOutputCP(CP_UTF8);
    }
    else
    {
      SetConsoleOutputCP(CP_ACP);
    }


    wchar_t InstanceName[MAX_PATH];
    HANDLE RequestEvent, ResponseEvent, FileMapping, Job;
    InitializeConsole(InstanceName, RequestEvent, ResponseEvent,
      CancelEvent, FileMapping, Job);

    wchar_t SavedTitle[512];
    GetConsoleTitle(SavedTitle, LENOF(SavedTitle));

    try
    {
      #ifndef CONSOLE_TEST
      InitializeChild(GetCommandLine(), InstanceName, Child);
      #endif

      try
      {
        bool Continue = true;
        do
        {
          HANDLE Handles[2];
          Handles[0] = RequestEvent;
          Handles[1] = Child;
          unsigned int HandleCount;
          #ifndef CONSOLE_TEST
          HandleCount = 2;
          #else
          HandleCount = 1;
          #endif
          unsigned long WaitResult =
            WaitForMultipleObjects(HandleCount, Handles, false, INFINITE);

          switch (WaitResult)
          {
            case WAIT_OBJECT_0:
              ProcessEvent(ResponseEvent, FileMapping);
              break;

            case WAIT_OBJECT_0 + 1:
              GetExitCodeProcess(Child, &Result);
              CloseHandle(Child);
              Child = NULL;
              Continue = false;
              break;

            default:
              throw runtime_error("Error waiting for communication from child process.");
          }
        }
        while (Continue);

        // flush pending progress message
        Print(false, L"");
      }
      catch(const exception& e)
      {
        puts(e.what());
        Result = RESULT_PROCESSING_ERROR;
      }

      #ifndef CONSOLE_TEST
      FinalizeChild(Child);
      #endif

      SetConsoleTitle(SavedTitle);

      SetConsoleCP(SavedConsoleCP);
      SetConsoleOutputCP(SavedConsoleOutputCP);
    }
    catch(const exception& e)
    {
      puts(e.what());
      Result = RESULT_INIT_ERROR;
    }

    FinalizeConsole(InstanceName, RequestEvent, ResponseEvent,
      CancelEvent, FileMapping, Job);
  }
  catch(const exception& e)
  {
    puts(e.what());
    Result = RESULT_GLOBAL_ERROR;
  }

  return Result;
}
//---------------------------------------------------------------------------
