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
unsigned int OutputType = FILE_TYPE_UNKNOWN;
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
  char ChildPath[MAX_PATH];
  const char* AppPath = argv[0];
  const char* LastDelimiter = strrchr(AppPath, '\\');
  const char* AppFileName;
  if (LastDelimiter != NULL)
  {
    strncpy(ChildPath, AppPath, LastDelimiter - AppPath + 1);
    ChildPath[LastDelimiter - AppPath + 1] = '\0';
    #ifndef TEST_CHILD
    AppFileName = LastDelimiter + 1;
    #endif
  }
  else
  {
    ChildPath[0] = '\0';
    #ifndef TEST_CHILD
    AppFileName = AppPath;
    #endif
  }

  #ifdef TEST_CHILD
  AppFileName = "WinSCP3.com";
  #endif

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

  char Parameters[10240];
  sprintf(Parameters, "\"%s\" /console /consoleinstance=%d ", ChildPath, InstanceNumber);
  for (int i = 1; i < argc; i++)
  {
    if (strlen(Parameters) + strlen(argv[i]) + 4 > sizeof(Parameters))
    {
      throw length_error("Too many parameters");
    }
    strcat(Parameters, "\"");
    strcat(Parameters, argv[i]);
    strcat(Parameters, "\" ");
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
  if (((OutputType == FILE_TYPE_DISK) || (OutputType == FILE_TYPE_PIPE)))
  {
    if (FromBeginning)
    {
      strcpy(LastFromBeginning, Message);
    }
    else
    {
      if (LastFromBeginning[0] != '\0')
      {
        printf(LastFromBeginning);
        LastFromBeginning[0] = '\0';
      }
      printf(Message);
    }
  }
  else
  {
    if (FromBeginning)
    {
      printf("\r");
    }
    printf(Message);
  }
}
//---------------------------------------------------------------------------
inline void ProcessPrintEvent(TConsoleCommStruct::TPrintEvent& Event)
{
  Print(Event.FromBeginning, Event.Message);
}
//---------------------------------------------------------------------------
void ProcessInputEvent(TConsoleCommStruct::TInputEvent& Event)
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

  try
  {
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

    SetConsoleMode(ConsoleInput, PrevMode);
  }
  catch(...)
  {
    SetConsoleMode(ConsoleInput, PrevMode);
    throw;
  }
}
//---------------------------------------------------------------------------
void ProcessChoiceEvent(TConsoleCommStruct::TChoiceEvent& Event)
{
  Event.Result = 0;

  unsigned long PrevMode, NewMode;
  GetConsoleMode(ConsoleInput, &PrevMode);
  NewMode = (PrevMode | ENABLE_PROCESSED_INPUT) & ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT);
  SetConsoleMode(ConsoleInput, NewMode);

  try
  {
    do
    {
      unsigned long Read;
      INPUT_RECORD Record;
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
    while (Event.Result == 0);

    SetConsoleMode(ConsoleInput, PrevMode);
  }
  catch(...)
  {
    SetConsoleMode(ConsoleInput, PrevMode);
    throw;
  }
}
//---------------------------------------------------------------------------
inline void ProcessTitleEvent(TConsoleCommStruct::TTitleEvent& Event)
{
  SetConsoleTitle(Event.Title);
}
//---------------------------------------------------------------------------
void ProcessEvent(HANDLE ResponseEvent, HANDLE FileMapping)
{
  TConsoleCommStruct* CommStruct = GetCommStruct(FileMapping);
  try
  {
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
    FlushConsoleInputBuffer(ConsoleInput);
    INPUT_RECORD InputRecord;
    memset(&InputRecord, 0, sizeof(InputRecord));
    InputRecord.EventType = KEY_EVENT;
    InputRecord.Event.KeyEvent.bKeyDown = true;
    InputRecord.Event.KeyEvent.wRepeatCount = 1;
    InputRecord.Event.KeyEvent.wVirtualKeyCode = VK_RETURN;
    InputRecord.Event.KeyEvent.uChar.AsciiChar = '\n';

    unsigned long Written;
    WriteConsoleInput(ConsoleInput, &InputRecord, 1, &Written);

    SetEvent(CancelEvent);

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
  int Result = 0;

  try
  {
    randomize();

    ConsoleInput = GetStdHandle(STD_INPUT_HANDLE);
    SetConsoleCtrlHandler(HandlerRoutine, true);

    HANDLE ConsoleOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    OutputType = GetFileType(ConsoleOutput);

    int InstanceNumber;
    HANDLE RequestEvent, ResponseEvent, FileMapping;
    InitializeConsole(InstanceNumber, RequestEvent, ResponseEvent,
      CancelEvent, FileMapping);

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
        Result = 3;
      }

      #ifndef CONSOLE_TEST
      FinalizeChild(Child);
      #endif
    }
    catch(const exception& e)
    {
      puts(e.what());
      Result = 2;
    }

    FinalizeConsole(InstanceNumber, RequestEvent, ResponseEvent,
      CancelEvent, FileMapping);
  }
  catch(const exception& e)
  {
    puts(e.what());
    Result = 1;
  }

  return Result;
}
//---------------------------------------------------------------------------
