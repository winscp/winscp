//---------------------------------------------------------------------------
#include <stdexcept>
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

#include "Console.h"
#define MAX_ATTEMPTS 10
//---------------------------------------------------------------------------
using namespace std;
HANDLE ConsoleInput = NULL;
HANDLE Child = NULL;
HANDLE CancelEvent = NULL;
HANDLE InputTimerEvent = NULL;
unsigned int OutputType = FILE_TYPE_UNKNOWN;
unsigned int InputType = FILE_TYPE_UNKNOWN;
enum { RESULT_GLOBAL_ERROR = 1, RESULT_INIT_ERROR = 2, RESULT_PROCESSING_ERROR = 3,
  RESULT_UNKNOWN_ERROR = 4 };
const char * CONSOLE_CHILD_PARAM = "consolechild";
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
void InitializeConsole(char* InstanceName, HANDLE& RequestEvent, HANDLE& ResponseEvent,
  HANDLE& CancelEvent, HANDLE& FileMapping, HANDLE& Job)
{
  unsigned int Process = GetCurrentProcessId();

  int Attempts = 0;
  char Name[MAX_PATH];
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
    sprintf(InstanceName, "_%u_%d", Process, InstanceNumber);
    sprintf(Name, "%s%s", CONSOLE_EVENT_REQUEST, InstanceName);
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

  sprintf(Name, "%s%s", CONSOLE_EVENT_RESPONSE, InstanceName);
  ResponseEvent = CreateEvent(NULL, false, false, Name);
  if (ResponseEvent == NULL)
  {
    throw runtime_error("Cannot create response event object.");
  }

  sprintf(Name, "%s%s", CONSOLE_EVENT_CANCEL, InstanceName);
  CancelEvent = CreateEvent(NULL, false, false, Name);
  if (CancelEvent == NULL)
  {
    throw runtime_error("Cannot create cancel event object.");
  }

  sprintf(Name, "%s%s", CONSOLE_MAPPING, InstanceName);
  FileMapping = CreateFileMapping((HANDLE)0xFFFFFFFF, NULL, PAGE_READWRITE,
    0, sizeof(TConsoleCommStruct), Name);
  if (FileMapping == NULL)
  {
    throw runtime_error("Cannot create mapping object.");
  }

  typedef HANDLE WINAPI (*TCreateJobObject)(LPSECURITY_ATTRIBUTES JobAttributes, LPCTSTR Name);
  typedef HANDLE WINAPI (*TSetInformationJobObject)(HANDLE Job, JOBOBJECTINFOCLASS JobObjectInformationClass,
    LPVOID JobObjectInformation, DWORD JobObjectInformationLength);

  HANDLE Kernel32 = GetModuleHandle("kernel32");
  TCreateJobObject CreateJobObject =
    (TCreateJobObject)GetProcAddress(Kernel32, "CreateJobObjectA");
  TSetInformationJobObject SetInformationJobObject =
    (TSetInformationJobObject)GetProcAddress(Kernel32, "SetInformationJobObject");
  if ((CreateJobObject != NULL) && (SetInformationJobObject != NULL))
  {
    sprintf(Name, "%s%s", CONSOLE_JOB, InstanceName);
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
bool __fastcall CutToken(const char *& Str, char * Token)
{
  bool Result;

  // inspired by Putty's sftp_getcmd() from PSFTP.C
  int Length = strlen(Str);
  int Index = 0;
  while ((Index < Length) &&
    ((Str[Index] == ' ') || (Str[Index] == '\t')))
  {
    Index++;
  }

  if (Index < Length)
  {
    bool Quoting = false;

    while (Index < Length)
    {
      if (!Quoting && ((Str[Index] == ' ') || (Str[Index] == '\t')))
      {
        break;
      }
      else if ((Str[Index] == '"') && (Index + 1 < Length) &&
        (Str[Index + 1] == '"'))
      {
        Index += 2;
        *Token = '"';
        Token++;
      }
      else if (Str[Index] == '"')
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

  *Token = '\0';

  return Result;
}
//---------------------------------------------------------------------------
void InitializeChild(const char* CommandLine, const char* InstanceName, HANDLE& Child)
{
  int SkipParam = 0;
  char ChildPath[MAX_PATH] = "";

  size_t CommandLineLen = strlen(CommandLine);
  char* Buffer = new char[(CommandLineLen > MAX_PATH ? CommandLineLen : MAX_PATH) + 1];

  int Count = 0;
  const char * P = CommandLine;
  while (CutToken(P, Buffer))
  {
    if ((strchr("-/", Buffer[0]) != NULL) &&
        (strncmpi(Buffer + 1, CONSOLE_CHILD_PARAM, strlen(CONSOLE_CHILD_PARAM)) == 0) &&
        (Buffer[strlen(CONSOLE_CHILD_PARAM) + 1] == '='))
    {
      SkipParam = Count;
      strcpy(ChildPath, Buffer + 1 + strlen(CONSOLE_CHILD_PARAM) + 1);
    }
    ++Count;
  }

  if (strlen(ChildPath) == 0)
  {
    DWORD ModuleNameLen = GetModuleFileName(NULL, Buffer, MAX_PATH);
    if ((ModuleNameLen == 0) || (ModuleNameLen == MAX_PATH))
    {
      throw runtime_error("Error retrieving executable name.");
    }
    const char* LastDelimiter = strrchr(Buffer, '\\');
    const char* AppFileName;
    if (LastDelimiter != NULL)
    {
      strncpy(ChildPath, Buffer, LastDelimiter - Buffer + 1);
      ChildPath[LastDelimiter - Buffer + 1] = '\0';
      AppFileName = LastDelimiter + 1;
    }
    else
    {
      ChildPath[0] = '\0';
      AppFileName = Buffer;
    }

    const char* ExtensionStart = strrchr(AppFileName, '.');
    if (ExtensionStart != NULL)
    {
      char* End = ChildPath + strlen(ChildPath);
      strncpy(End, AppFileName, ExtensionStart - AppFileName);
      *(End + (ExtensionStart - AppFileName)) = '\0';
    }
    else
    {
      strcat(ChildPath, AppFileName);
    }
    strcat(ChildPath, ".exe");
  }

  char* Parameters = new char[(CommandLineLen * 2) + 100 + (Count * 3) + 1];
  sprintf(Parameters, "\"%s\" /console /consoleinstance=%s ", ChildPath, InstanceName);
  P = CommandLine;
  // skip executable path
  CutToken(P, Buffer);
  int i = 1;
  while (CutToken(P, Buffer))
  {
    if (i != SkipParam)
    {
      strcat(Parameters, "\"");
      char* P2 = Parameters + strlen(Parameters);
      const char* P3 = Buffer;
      const char* BufferEnd = Buffer + strlen(Buffer) + 1;
      while (P3 != BufferEnd)
      {
        *P2 = *P3;
        ++P2;
        if (*P3 == '"')
        {
          *P2 = '"';
          ++P2;
        }
        ++P3;
      }

      strcat(Parameters, "\" ");
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
void FinalizeConsole(const char* /*InstanceName*/, HANDLE RequestEvent,
  HANDLE ResponseEvent, HANDLE CancelEvent, HANDLE FileMapping, HANDLE Job)
{
  CloseHandle(RequestEvent);
  CloseHandle(ResponseEvent);
  CloseHandle(CancelEvent);
  CloseHandle(FileMapping);
  CloseHandle(Job);
}
//---------------------------------------------------------------------------
static char LastFromBeginning[sizeof(TConsoleCommStruct::TPrintEvent)] = "";
//---------------------------------------------------------------------------
inline void Flush()
{
  if ((OutputType == FILE_TYPE_DISK) || (OutputType == FILE_TYPE_PIPE))
  {
    fflush(stdout);
  }
}
//---------------------------------------------------------------------------
void Print(bool FromBeginning, const char * Message)
{
  if ((OutputType == FILE_TYPE_DISK) || (OutputType == FILE_TYPE_PIPE))
  {
    if (FromBeginning && (Message[0] != '\n'))
    {
      strcpy(LastFromBeginning, Message);
    }
    else
    {
      if (LastFromBeginning[0] != '\0')
      {
        printf("%s", LastFromBeginning);
        LastFromBeginning[0] = '\0';
      }

      if (FromBeginning && (Message[0] == '\n'))
      {
        printf("\n");
        strcpy(LastFromBeginning, Message + 1);
      }
      else
      {
        printf("%s", Message);
      }
      Flush();
    }
  }
  else
  {
    if (FromBeginning)
    {
      printf("\r");
    }
    printf("%s", Message);
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
  InputRecord.Event.KeyEvent.uChar.AsciiChar = '\r';

  unsigned long Written;
  WriteConsoleInput(ConsoleInput, &InputRecord, 1, &Written);

  CancelInput();
}
//---------------------------------------------------------------------------
DWORD WINAPI InputTimerThreadProc(void * Parameter)
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
    unsigned long Len = 0;
    unsigned long Read;
    char Ch;
    bool Result;

    while (((Result = (ReadFile(ConsoleInput, &Ch, 1, &Read, NULL) != 0)) != false) &&
           (Read > 0) && (Len < sizeof(Event.Str) - 1) && (Ch != '\n'))
    {
      if (Ch != '\r')
      {
        Event.Str[Len] = Ch;
        Len++;
      }
    }
    Event.Str[Len] = '\0';

    Print(false, Event.Str);
    Print(false, "\n");

    Event.Result = ((Result && (Read > 0)) || (Len > 0));
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
      Event.Result = ReadConsole(ConsoleInput, Event.Str, sizeof(Event.Str) - 1, &Read, NULL);
      Event.Str[Read] = '\0';

      bool PendingCancel = (WaitForSingleObject(CancelEvent, 0) == WAIT_OBJECT_0);
      if (PendingCancel || !Event.Echo)
      {
        printf("\n");
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
              char CStr[2];
              CStr[0] = Record.Event.KeyEvent.uChar.AsciiChar;
              CStr[1] = '\0';
              CharUpperBuff(CStr, 1);
              char C = CStr[0];
              if (C == 27)
              {
                Event.Result = Event.Cancel;
              }
              else if ((strchr(Event.Options, C) != NULL) &&
                       ((Record.Event.KeyEvent.dwControlKeyState &
                         (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED | LEFT_ALT_PRESSED |
                         RIGHT_CTRL_PRESSED)) == 0))

              {
                Event.Result = strchr(Event.Options, C) - Event.Options + 1;
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
      throw logic_error("Incompatible console protocol version");
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
int main(int /*argc*/, char* /*argv*/[])
{
  unsigned long Result = RESULT_UNKNOWN_ERROR;

  try
  {
    randomize();

    ConsoleInput = GetStdHandle(STD_INPUT_HANDLE);
    InputType = GetFileType(ConsoleInput);
    SetConsoleCtrlHandler(HandlerRoutine, true);

    HANDLE ConsoleOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    OutputType = GetFileType(ConsoleOutput);

    char InstanceName[MAX_PATH];
    HANDLE RequestEvent, ResponseEvent, FileMapping, Job;
    InitializeConsole(InstanceName, RequestEvent, ResponseEvent,
      CancelEvent, FileMapping, Job);

    char SavedTitle[512];
    GetConsoleTitle(SavedTitle, sizeof(SavedTitle));

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
        Print(false, "");
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
