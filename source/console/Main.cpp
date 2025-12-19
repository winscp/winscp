//---------------------------------------------------------------------------
#include <stdexcept>
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <io.h>
#include <fcntl.h>

#include "Console.h"
#define MAX_ATTEMPTS 10
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
using namespace std;
static HANDLE ConsoleInput = nullptr;
static HANDLE ConsoleOutput = nullptr;
static HANDLE ConsoleStandardOutput = nullptr;
static HANDLE ConsoleErrorOutput = nullptr;
static TConsoleCommStruct::TInitEvent::STDINOUT OutputFormat;
static TConsoleCommStruct::TInitEvent::STDINOUT InputFormat;
static HANDLE Child = nullptr;
static HANDLE CancelEvent = nullptr;
static HANDLE InputTimerEvent = nullptr;
static bool SupportsUtf8ConsoleOutput = false;
static unsigned int OutputType = FILE_TYPE_UNKNOWN;
static unsigned int InputType = FILE_TYPE_UNKNOWN;
enum { RESULT_GLOBAL_ERROR = 1, RESULT_INIT_ERROR = 2, RESULT_PROCESSING_ERROR = 3,
  RESULT_UNKNOWN_ERROR = 4 };
static const wchar_t* CONSOLE_CHILD_PARAM = L"consolechild";
//---------------------------------------------------------------------------
inline TConsoleCommStruct* GetCommStruct(HANDLE FileMapping)
{
  TConsoleCommStruct* Result;
  Result = static_cast<TConsoleCommStruct*>(MapViewOfFile(FileMapping,
    FILE_MAP_ALL_ACCESS, 0, 0, 0));
  if (Result == nullptr)
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
static void InitializeConsole(
  wchar_t* InstanceName, HANDLE& RequestEvent, HANDLE& ResponseEvent, HANDLE& ACancelEvent, HANDLE& FileMapping, HANDLE& Job)
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
    UniqEvent = (EventHandle == nullptr);
    if (!UniqEvent)
    {
      CloseHandle(EventHandle);
    }
    Attempts++;
  }
  while (!UniqEvent);

  RequestEvent = CreateEvent(nullptr, false, false, Name);
  if (RequestEvent == nullptr)
  {
    throw runtime_error("Cannot create request event object.");
  }

  swprintf(Name, L"%s%s", CONSOLE_EVENT_RESPONSE, InstanceName);
  ResponseEvent = CreateEvent(nullptr, false, false, Name);
  if (ResponseEvent == nullptr)
  {
    throw runtime_error("Cannot create response event object.");
  }

  swprintf(Name, L"%s%s", CONSOLE_EVENT_CANCEL, InstanceName);
  ACancelEvent = CreateEvent(nullptr, false, false, Name);
  if (ACancelEvent == nullptr)
  {
    throw runtime_error("Cannot create cancel event object.");
  }

  swprintf(Name, L"%s%s", CONSOLE_MAPPING, InstanceName);
  FileMapping =
    CreateFileMapping(INVALID_HANDLE_VALUE, nullptr, PAGE_READWRITE, 0, sizeof(TConsoleCommStruct), Name);
  if (FileMapping == nullptr)
  {
    throw runtime_error("Cannot create mapping object.");
  }

  swprintf(Name, L"%s%s", CONSOLE_JOB, InstanceName);
  Job = CreateJobObject(nullptr, Name);
  if (Job == nullptr)
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
    Job = nullptr;
  }

  TConsoleCommStruct* CommStruct = GetCommStruct(FileMapping);
  CommStruct->Size = sizeof(TConsoleCommStruct);
  CommStruct->Version = TConsoleCommStruct::CurrentVersion;
  CommStruct->Event = TConsoleCommStruct::NONE;
  FreeCommStruct(CommStruct);
}
//---------------------------------------------------------------------------
// duplicated in Common.cpp
static bool CutToken(const wchar_t*& Str, wchar_t* Token)
{
  bool Result;

  // inspired by Putty's sftp_getcmd() from PSFTP.C
  size_t Length = wcslen(Str);
  size_t Index = 0;
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
static char* WideStringToString(const wchar_t* Message)
{
  char* Buffer;
  int Size = WideCharToMultiByte(CP_UTF8, 0, Message, -1, nullptr, 0, nullptr, nullptr);
  if (Size > 0)
  {
    Buffer = new char[(static_cast<size_t>(Size) * 2) + 1];
    if (WideCharToMultiByte(CP_UTF8, 0, Message, -1, Buffer, Size, nullptr, nullptr) > 0)
    {
      Buffer[Size] = '\0';
    }
    else
    {
      delete[] Buffer;
      Buffer = nullptr;
    }
  }
  else
  {
    Buffer = nullptr;
  }
  return Buffer;
}
//---------------------------------------------------------------------------
static void GetProductVersion(wchar_t* ProductVersion)
{
  wchar_t Buffer[MAX_PATH];
  DWORD ModuleNameLen = GetModuleFileName(nullptr, Buffer, MAX_PATH);
  if ((ModuleNameLen == 0) || (ModuleNameLen == MAX_PATH))
  {
    throw runtime_error("Error retrieving executable name.");
  }
  ProductVersion[0] = '\0';
  unsigned long Handle;
  unsigned int Size = GetFileVersionInfoSize(Buffer, &Handle);
  if (Size > 0)
  {
    char * VersionInfo = new char[Size];
    VS_FIXEDFILEINFO* FixedFileInfo;
    unsigned int Length;
    if (GetFileVersionInfo(Buffer, Handle, Size, VersionInfo))
    {
      if (VerQueryValue(VersionInfo, L"\\", reinterpret_cast<void**>(&FixedFileInfo), &Length))
      {
        int ProductMajor = HIWORD(FixedFileInfo->dwProductVersionMS);
        int ProductMinor = LOWORD(FixedFileInfo->dwProductVersionMS);
        int ProductBuild = HIWORD(FixedFileInfo->dwProductVersionLS);
        if ((ProductMajor >= 1) && (ProductMajor <= 99) &&
            (ProductMinor >= 0) && (ProductMinor <= 99) &&
            (ProductBuild >= 0) && (ProductBuild <= 99))
        {
          wsprintf(ProductVersion, L"%d.%d.%d", ProductMajor, ProductMinor, ProductBuild);
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
static void InitializeChild(const wchar_t* CommandLine, const wchar_t* InstanceName, HANDLE& AChild)
{
  unsigned int SkipParam = 0;
  wchar_t ChildPath[MAX_PATH] = L"";

  size_t CommandLineLen = wcslen(CommandLine);
  wchar_t* Buffer = new wchar_t[(CommandLineLen > MAX_PATH ? CommandLineLen : MAX_PATH) + 1];

  unsigned int Count = 0;
  const wchar_t* P = CommandLine;
  while (CutToken(P, Buffer))
  {
    if ((wcschr(L"-/", Buffer[0]) != nullptr) &&
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
    DWORD ModuleNameLen = GetModuleFileName(nullptr, Buffer, MAX_PATH);
    if ((ModuleNameLen == 0) || (ModuleNameLen == MAX_PATH))
    {
      throw runtime_error("Error retrieving executable name.");
    }
    const wchar_t* LastDelimiter = wcsrchr(Buffer, L'\\');
    const wchar_t* AppFileName;
    if (LastDelimiter != nullptr)
    {
      wcsncpy(ChildPath, Buffer, static_cast<size_t>(LastDelimiter - Buffer + 1));
      ChildPath[LastDelimiter - Buffer + 1] = L'\0';
      AppFileName = LastDelimiter + 1;
    }
    else
    {
      ChildPath[0] = L'\0';
      AppFileName = Buffer;
    }

    const wchar_t* ExtensionStart = wcsrchr(AppFileName, L'.');
    if (ExtensionStart != nullptr)
    {
      wchar_t* End = ChildPath + wcslen(ChildPath);
      wcsncpy(End, AppFileName, static_cast<size_t>(ExtensionStart - AppFileName));
      *(End + (ExtensionStart - AppFileName)) = L'\0';
    }
    else
    {
      wcscat(ChildPath, AppFileName);
    }
    wcscat(ChildPath, L".exe");
  }

  wchar_t ProductVersion[32];
  GetProductVersion(ProductVersion);

  wchar_t* Parameters = new wchar_t[(CommandLineLen * 2) + 100 + (Count * 3) + 1];
  wsprintf(Parameters, L"\"%s\" /console=%s /consoleinstance=%s ", ChildPath, ProductVersion, InstanceName);
  P = CommandLine;
  // skip executable path
  CutToken(P, Buffer);
  unsigned int i = 1;
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

  STARTUPINFO StartupInfo = { .cb = sizeof(STARTUPINFO) };
  PROCESS_INFORMATION ProcessInfomation;

  BOOL Result =
    CreateProcess(ChildPath, Parameters, nullptr, nullptr, false, 0, nullptr, nullptr, &StartupInfo, &ProcessInfomation);

  delete[] Parameters;

  if (Result)
  {
    AChild = ProcessInfomation.hProcess;
  }
  else
  {
    size_t Len = MAX_PATH + 1024;
    DWORD Error = GetLastError();
    wchar_t * Buffer2 = nullptr;
    DWORD Flags = FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER;
    Len += FormatMessage(Flags, nullptr, Error, 0, reinterpret_cast<wchar_t *>(&Buffer2), 0, nullptr);

    wchar_t* Message = new wchar_t[Len];
    wsprintf(Message, L"Cannot start WinSCP application \"%s\".", ChildPath);
    if (Buffer2 != nullptr)
    {
      wcscat(Message, L"\n");
      wcscat(Message, Buffer2);
      LocalFree(Buffer2);
    }
    char* MessageString = WideStringToString(Message);
    delete[] Message;
    std::string ErrorString(MessageString);
    delete[] MessageString;
    throw runtime_error(ErrorString);
  }
}
//---------------------------------------------------------------------------
static void FinalizeChild(HANDLE AChild)
{
  if (AChild != nullptr)
  {
    TerminateProcess(AChild, 0);
    CloseHandle(AChild);
  }
}
//---------------------------------------------------------------------------
static void FinalizeConsole(
  const wchar_t* /*InstanceName*/, HANDLE RequestEvent, HANDLE ResponseEvent, HANDLE ACancelEvent, HANDLE FileMapping, HANDLE Job)
{
  CloseHandle(RequestEvent);
  CloseHandle(ResponseEvent);
  CloseHandle(ACancelEvent);
  CloseHandle(FileMapping);
  if (Job != nullptr)
  {
    CloseHandle(Job);
  }
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
static void PrintException(const exception& e)
{
  if (ConsoleOutput != nullptr)
  {
    unsigned long Written;
    WriteFile(ConsoleOutput, e.what(), strlen(e.what()), &Written, nullptr);
  }
  else
  {
    puts(e.what());
  }
}
//---------------------------------------------------------------------------
static void Print(const wchar_t* Message)
{
  char* Buffer = WideStringToString(Message);
  if (Buffer != nullptr)
  {
    char* Ptr = Buffer;
    while ((Ptr = strchr(Ptr, '\n')) != nullptr)
    {
      memmove(Ptr + 1, Ptr, strlen(Ptr) + 1);
      *Ptr = '\r';
      Ptr += 2;
    }
    unsigned long Written;
    WriteFile(ConsoleOutput, Buffer, strlen(Buffer), &Written, nullptr);
    delete[] Buffer;
  }
}
//---------------------------------------------------------------------------
static void Print(bool FromBeginning, const wchar_t* Message)
{
  size_t Len = wcslen(Message);
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
      WriteConsole(ConsoleOutput, L"\r", 1, &Written, nullptr);
    }
    bool WriteResult =
      WriteConsole(ConsoleOutput, Message, Len, &Written, nullptr);
    DWORD Error = GetLastError();
    // The current console font does not support some characters in the message,
    // fall back to ansi-writting
    if (!WriteResult && (Error == ERROR_GEN_FAILURE))
    {
      int Size = WideCharToMultiByte(CP_ACP, 0, Message, -1, nullptr, 0, nullptr, nullptr);
      if (Size > 0)
      {
        char* Buffer = new char[static_cast<size_t>(Size)];
        if (WideCharToMultiByte(CP_ACP, 0, Message, -1, Buffer, Size, nullptr, nullptr) > 0)
        {
          WriteConsoleA(ConsoleOutput, Buffer, strlen(Buffer), &Written, nullptr);
        }
        delete[] Buffer;
      }
    }
  }
}
//---------------------------------------------------------------------------
inline void ProcessPrintEvent(TConsoleCommStruct::TPrintEvent& Event)
{
  Print(Event.FromBeginning, Event.Message);
}
//---------------------------------------------------------------------------
static void CancelInput()
{
  SetEvent(CancelEvent);
}
//---------------------------------------------------------------------------
static void BreakInput()
{
  FlushConsoleInputBuffer(ConsoleInput);

  // Signal cancel first, otherwise the main thread can get Enter before the event is set
  CancelInput();

  INPUT_RECORD InputRecord;
  memset(&InputRecord, 0, sizeof(InputRecord));
  InputRecord.EventType = KEY_EVENT;
  InputRecord.Event.KeyEvent.bKeyDown = true;
  InputRecord.Event.KeyEvent.wRepeatCount = 1;
  InputRecord.Event.KeyEvent.uChar.UnicodeChar = L'\r';

  unsigned long Written;
  WriteConsoleInput(ConsoleInput, &InputRecord, 1, &Written);
}
//---------------------------------------------------------------------------
static DWORD WINAPI InputTimerThreadProc(void* Parameter)
{
  unsigned int Timer = reinterpret_cast<unsigned int>(Parameter);
  unsigned int Remaining = Timer;
  const unsigned int Step = 1000;
  const int FirstKey = VK_LBUTTON; // 0x01
  const int LastKey = VK_OEM_CLEAR; // 0xFE

  // reset key state
  for (int Key = FirstKey; Key <= LastKey; Key++)
  {
    GetAsyncKeyState(Key);
  }

  while (Remaining > 0)
  {
    unsigned long WaitResult = WaitForSingleObject(InputTimerEvent, Step);

    if (WaitResult == WAIT_OBJECT_0)
    {
      // input entered
      Remaining = 0;
    }
    else if (WaitResult == WAIT_TIMEOUT)
    {
      bool Input = false;

      for (int Key = FirstKey; Key <= LastKey; Key++)
      {
        if ((GetAsyncKeyState(Key) & 0x01) != 0)
        {
          Input = true;
          // Finishing the loop nevertheless to reset state of all keys
        }
      }

      if (Input)
      {
        // If we have new input, reset timer
        Remaining = Timer;
      }
      else if (Remaining > Step)
      {
        Remaining -= Step;
      }
      else
      {
        BreakInput();
        Remaining = 0;
      }
    }
    else
    {
      // abort input on (unlikely) error
      BreakInput();
      Remaining = 0;
    }
  }

  return 0;
}
//---------------------------------------------------------------------------
static void ProcessInputEvent(TConsoleCommStruct::TInputEvent& Event)
{
  if ((InputType == FILE_TYPE_DISK) || (InputType == FILE_TYPE_PIPE))
  {
    unsigned long Bytes = 0;
    unsigned long Read;
    bool Result;
    char Ch;
    size_t BufSize = std::size(Event.Str) * 3;
    char Buf[BufSize];

    while (((Result = (ReadFile(ConsoleInput, &Ch, 1, &Read, nullptr) != 0)) != false) &&
           (Read > 0) && (Bytes < BufSize - 1) && (Ch != '\n'))
    {
      if (Ch != '\r')
      {
        Buf[Bytes] = Ch;
        Bytes++;
      }
    }
    Buf[Bytes] = L'\0';

    MultiByteToWideChar(CP_UTF8, 0, Buf, -1, Event.Str, static_cast<int>(std::size(Event.Str) - 1));
    Event.Str[std::size(Event.Str) - 1] = L'\0';

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
      NewMode &= static_cast<unsigned long>(~ENABLE_ECHO_INPUT);
    }
    SetConsoleMode(ConsoleInput, NewMode);

    HANDLE InputTimerThread = nullptr;

    try
    {
      if (Event.Timer > 0)
      {
        unsigned long ThreadId;
        InputTimerEvent = CreateEvent(nullptr, false, false, nullptr);
        InputTimerThread =
          CreateThread(nullptr, 0, InputTimerThreadProc, reinterpret_cast<void *>(Event.Timer), 0, &ThreadId);
      }

      unsigned long Read;
      Event.Result = ReadConsole(ConsoleInput, Event.Str, std::size(Event.Str) - 1, &Read, nullptr);
      Event.Str[Read] = L'\0';

      bool PendingCancel = (WaitForSingleObject(CancelEvent, 0) == WAIT_OBJECT_0);
      if (PendingCancel || !Event.Echo)
      {
        WriteFile(ConsoleOutput, "\n", 1, nullptr, nullptr);
        Flush();
      }

      if (PendingCancel || (Read == 0))
      {
        Event.Result = false;
      }
    }
    __finally
    {
      if (InputTimerThread != nullptr)
      {
        SetEvent(InputTimerEvent);
        WaitForSingleObject(InputTimerThread, 100);
        CloseHandle(InputTimerEvent);
        InputTimerEvent = nullptr;
        CloseHandle(InputTimerThread);
      }
      SetConsoleMode(ConsoleInput, PrevMode);
    }
  }
}
//---------------------------------------------------------------------------
static void ProcessChoiceEvent(TConsoleCommStruct::TChoiceEvent& Event)
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
    NewMode = (PrevMode | ENABLE_PROCESSED_INPUT) & static_cast<unsigned long>(~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT));
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
              // This happens when Shift key is pressed
              if (Record.Event.KeyEvent.uChar.UnicodeChar != 0)
              {
                wchar_t CStr[2];
                CStr[0] = Record.Event.KeyEvent.uChar.UnicodeChar;
                CStr[1] = L'\0';
                CharUpperBuff(CStr, 1);
                wchar_t C = CStr[0];
                if (C == 27)
                {
                  Event.Result = Event.Cancel;
                }
                else if ((wcschr(Event.Options, C) != nullptr) &&
                         ((Record.Event.KeyEvent.dwControlKeyState &
                           (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED | LEFT_ALT_PRESSED |
                           RIGHT_ALT_PRESSED)) == 0))

                {
                  Event.Result = wcschr(Event.Options, C) - Event.Options + 1;
                }
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
  if (Event.UseStdErr)
  {
    ConsoleOutput = ConsoleErrorOutput;
  }

  OutputFormat = Event.OutputFormat;
  if (OutputFormat != TConsoleCommStruct::TInitEvent::OFF)
  {
    setmode(fileno(stdout), O_BINARY);
  }

  InputFormat = Event.InputFormat;
  if (InputFormat != TConsoleCommStruct::TInitEvent::OFF)
  {
    setmode(fileno(stdin), O_BINARY);
  }

  OutputType = GetFileType(ConsoleOutput);
  // Until now we should not have printed anything.
  // Only in case of a fatal failure, we might have printed a pure ASCII error messages (and never got here).
  if ((OutputType == FILE_TYPE_DISK) || (OutputType == FILE_TYPE_PIPE) ||
      SupportsUtf8ConsoleOutput)
  {
    SetConsoleOutputCP(CP_UTF8);
  }
  else
  {
    SetConsoleOutputCP(CP_ACP);
  }

  Event.InputType = InputType;
  Event.OutputType = OutputType;
  // default anyway
  Event.WantsProgress = false;
}
//---------------------------------------------------------------------------
inline void ProcessTransferOutEvent(TConsoleCommStruct::TTransferEvent& Event)
{
  if (OutputFormat == TConsoleCommStruct::TInitEvent::BINARY)
  {
    fwrite(Event.Data, 1, Event.Len, stdout);
  }
  else if (OutputFormat == TConsoleCommStruct::TInitEvent::CHUNKED)
  {
    fprintf(stdout, "%x\r\n", static_cast<int>(Event.Len));
    fwrite(Event.Data, 1, Event.Len, stdout);
    fputs("\r\n", stdout);
  }
}
//---------------------------------------------------------------------------
inline void ProcessTransferInEvent(TConsoleCommStruct::TTransferEvent& Event)
{
  size_t Read = fread(Event.Data, 1, Event.Len, stdin);
  if (Read != Event.Len)
  {
    if (ferror(stdin))
    {
      Event.Error = true;
    }
    else
    {
      Event.Len = Read;
    }
  }
}
//---------------------------------------------------------------------------
static void ProcessEvent(HANDLE ResponseEvent, HANDLE FileMapping)
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

      case TConsoleCommStruct::TRANSFEROUT:
        ProcessTransferOutEvent(CommStruct->TransferEvent);
        break;

      case TConsoleCommStruct::TRANSFERIN:
        ProcessTransferInEvent(CommStruct->TransferEvent);
        break;

      case TConsoleCommStruct::NONE:
      case TConsoleCommStruct::PROGRESS:
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
static BOOL WINAPI HandlerRoutine(DWORD CtrlType)
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
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wdeprecated-declarations"
    GetVersionEx(&VersionInfo);
    #pragma clang diagnostic pop

    ConsoleInput = GetStdHandle(STD_INPUT_HANDLE);
    InputType = GetFileType(ConsoleInput);
    SetConsoleCtrlHandler(HandlerRoutine, true);

    unsigned int SavedConsoleCP = GetConsoleCP();
    unsigned int SavedConsoleOutputCP = GetConsoleOutputCP();

    ConsoleStandardOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    ConsoleOutput = ConsoleStandardOutput;
    ConsoleErrorOutput = GetStdHandle(STD_ERROR_HANDLE);


    SupportsUtf8ConsoleOutput =
      ((VersionInfo.dwMajorVersion == 6) && (VersionInfo.dwMinorVersion >= 1)) ||
      (VersionInfo.dwMajorVersion > 6);

    if ((InputType == FILE_TYPE_DISK) || (InputType == FILE_TYPE_PIPE) ||
        SupportsUtf8ConsoleOutput)
    {
      SetConsoleCP(CP_UTF8);
    }
    else
    {
      SetConsoleCP(CP_ACP);
    }


    wchar_t InstanceName[MAX_PATH];
    HANDLE RequestEvent, ResponseEvent, FileMapping, Job;
    InitializeConsole(InstanceName, RequestEvent, ResponseEvent,
      CancelEvent, FileMapping, Job);

    wchar_t SavedTitle[512];
    GetConsoleTitle(SavedTitle, std::size(SavedTitle));

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
              Child = nullptr;
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
        PrintException(e);
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
      PrintException(e);
      Result = RESULT_INIT_ERROR;
    }

    FinalizeConsole(InstanceName, RequestEvent, ResponseEvent,
      CancelEvent, FileMapping, Job);
  }
  catch(const exception& e)
  {
    PrintException(e);
    Result = RESULT_GLOBAL_ERROR;
  }

  return static_cast<int>(Result);
}
//---------------------------------------------------------------------------
