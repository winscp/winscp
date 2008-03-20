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
    throw logic_error("Cannot open mapping object.");
  }
  return Result;
}
//---------------------------------------------------------------------------
inline void FreeCommStruct(TConsoleCommStruct* CommStruct)
{
  UnmapViewOfFile(CommStruct);
}
//---------------------------------------------------------------------------
void InitializeConsole(int& InstanceNumber, HANDLE& RequestEvent, HANDLE& ResponseEvent,
  HANDLE& CancelEvent, HANDLE& FileMapping)
{
  int Attempts = 0;
  char Name[MAX_PATH];
  bool UniqEvent;

  do
  {
    if (Attempts > MAX_ATTEMPTS)
    {
      throw logic_error("Cannot find unique name for event object.");
    }

    #ifdef CONSOLE_TEST
    InstanceNumber = 1;
    #else
    InstanceNumber = random(1000);
    #endif
    sprintf(Name, "%s%d", CONSOLE_EVENT_REQUEST, InstanceNumber);
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
    throw logic_error("Cannot create request event object.");
  }

  sprintf(Name, "%s%d", CONSOLE_EVENT_RESPONSE, InstanceNumber);
  ResponseEvent = CreateEvent(NULL, false, false, Name);
  if (ResponseEvent == NULL)
  {
    throw logic_error("Cannot create response event object.");
  }

  sprintf(Name, "%s%d", CONSOLE_EVENT_CANCEL, InstanceNumber);
  CancelEvent = CreateEvent(NULL, false, false, Name);
  if (CancelEvent == NULL)
  {
    throw logic_error("Cannot create cancel event object.");
  }

  sprintf(Name, "%s%d", CONSOLE_MAPPING, InstanceNumber);
  FileMapping = CreateFileMapping((HANDLE)0xFFFFFFFF, NULL, PAGE_READWRITE,
    0, sizeof(TConsoleCommStruct), Name);
  if (FileMapping == NULL)
  {
    throw logic_error("Cannot create mapping object.");
  }

  TConsoleCommStruct* CommStruct = GetCommStruct(FileMapping);
  CommStruct->Size = sizeof(TConsoleCommStruct);
  CommStruct->Version = TConsoleCommStruct::CurrentVersion;
  CommStruct->Event = TConsoleCommStruct::NONE;
  FreeCommStruct(CommStruct);
}
//---------------------------------------------------------------------------
void InitializeChild(int argc, char* argv[], int InstanceNumber, HANDLE& Child)
{
  int SkipParam = 0;
  char ChildPath[MAX_PATH] = "";

  for (int i = 1; i < argc; i++)
  {
    if ((strchr("-/", argv[i][0]) != NULL) &&
        (strncmpi(argv[i] + 1, CONSOLE_CHILD_PARAM, strlen(CONSOLE_CHILD_PARAM)) == 0) &&
        (argv[i][strlen(CONSOLE_CHILD_PARAM) + 1] == '='))
    {
      SkipParam = i;
      strcpy(ChildPath, argv[i] + 1 + strlen(CONSOLE_CHILD_PARAM) + 1);
      break;
    }
  }

  if (strlen(ChildPath) == 0)
  {
    const char* AppPath = argv[0];
    const char* LastDelimiter = strrchr(AppPath, '\\');
    const char* AppFileName;
    if (LastDelimiter != NULL)
    {
      strncpy(ChildPath, AppPath, LastDelimiter - AppPath + 1);
      ChildPath[LastDelimiter - AppPath + 1] = '\0';
      AppFileName = LastDelimiter + 1;
    }
    else
    {
      ChildPath[0] = '\0';
      AppFileName = AppPath;
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

  char Parameters[10240];
  sprintf(Parameters, "\"%s\" /console /consoleinstance=%d ", ChildPath, InstanceNumber);
  for (int i = 1; i < argc; i++)
  {
    if (i != SkipParam)
    {
      if (strlen(Parameters) + strlen(argv[i]) + 4 > sizeof(Parameters))
      {
        throw length_error("Too many parameters");
      }
      strcat(Parameters, "\"");
      strcat(Parameters, argv[i]);
      strcat(Parameters, "\" ");
    }
  }

  STARTUPINFO StartupInfo = { sizeof(STARTUPINFO) };
  PROCESS_INFORMATION ProcessInfomation;

  if (CreateProcess(ChildPath, Parameters, NULL, NULL, false, 0, NULL, NULL,
        &StartupInfo, &ProcessInfomation) != 0)
  {
    Child = ProcessInfomation.hProcess;
  }
  else
  {
    throw logic_error("Cannot start WinSCP application.");
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
void FinalizeConsole(int /*InstanceNumber*/, HANDLE RequestEvent,
  HANDLE ResponseEvent, HANDLE CancelEvent, HANDLE FileMapping)
{
  CloseHandle(RequestEvent);
  CloseHandle(ResponseEvent);
  CloseHandle(CancelEvent);
  CloseHandle(FileMapping);
}
//---------------------------------------------------------------------------
static char LastFromBeginning[sizeof(TConsoleCommStruct::TPrintEvent)] = "";
//---------------------------------------------------------------------------
inline void Print(bool FromBeginning, const char * Message)
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

  SetEvent(CancelEvent);
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
  if ((InputType == FILE_TYPE_DISK) || (InputType == FILE_TYPE_PIPE))
  {
    Event.Result = Event.Cancel;
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
        throw logic_error("Unknown event");
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
    BreakInput();

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
int main(int argc, char* argv[])
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

    int InstanceNumber;
    HANDLE RequestEvent, ResponseEvent, FileMapping;
    InitializeConsole(InstanceNumber, RequestEvent, ResponseEvent,
      CancelEvent, FileMapping);

    char SavedTitle[512];
    GetConsoleTitle(SavedTitle, sizeof(SavedTitle));

    try
    {
      #ifndef CONSOLE_TEST
      InitializeChild(argc, argv, InstanceNumber, Child);
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
              throw logic_error("Error waiting for communication from child process.");
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

    FinalizeConsole(InstanceNumber, RequestEvent, ResponseEvent,
      CancelEvent, FileMapping);
  }
  catch(const exception& e)
  {
    puts(e.what());
    Result = RESULT_GLOBAL_ERROR;
  }

  return Result;
}
//---------------------------------------------------------------------------
