//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Exceptions.h>
#include <Script.h>
#include <ScpMain.h>
#include <Terminal.h>

#include <Consts.hpp>

#include "Console.h"
#include "WinInterface.h"
#include "ProgParams.h"
#include "TextsWin.h"
#include "CustomWinConfiguration.h"
#include "SynchronizeController.h"
//---------------------------------------------------------------------------
#define WM_INTERUPT_IDLE (WM_WINSCP_USER + 3)
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
class TConsole
{
public:
  virtual void __fastcall Print(AnsiString Str, bool FromBeginning = false) = 0;
  virtual bool __fastcall Input(AnsiString & Str, bool Echo) = 0;
  virtual int __fastcall Choice(AnsiString Options, int Cancel, int Break) = 0;
  virtual bool __fastcall PendingAbort() = 0;
  virtual void __fastcall SetTitle(AnsiString Title) = 0;
  virtual void __fastcall WaitBeforeExit() = 0;
};
//---------------------------------------------------------------------------
class TOwnConsole : public TConsole
{
public:
  static TOwnConsole * __fastcall Instance();

  virtual void __fastcall Print(AnsiString Str, bool FromBeginning = false);
  virtual bool __fastcall Input(AnsiString & Str, bool Echo);
  virtual int __fastcall Choice(AnsiString Options, int Cancel, int Break);
  virtual bool __fastcall PendingAbort();
  virtual void __fastcall SetTitle(AnsiString Title);
  virtual void __fastcall WaitBeforeExit();

protected:
  static TOwnConsole * FInstance;

  __fastcall TOwnConsole();
  __fastcall ~TOwnConsole();

  static BOOL WINAPI HandlerRoutine(DWORD CtrlType);

private:
  HANDLE FInput;
  HANDLE FOutput;
  bool FPendingAbort;
};
//---------------------------------------------------------------------------
TOwnConsole * TOwnConsole::FInstance = NULL;
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
}
//---------------------------------------------------------------------------
__fastcall TOwnConsole::~TOwnConsole()
{
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
BOOL WINAPI TOwnConsole::HandlerRoutine(DWORD CtrlType)
{
  if ((CtrlType == CTRL_C_EVENT) || (CtrlType == CTRL_BREAK_EVENT))
  {
    assert(FInstance != NULL);
    FlushConsoleInputBuffer(FInstance->FInput);
    INPUT_RECORD InputRecord;
    memset(&InputRecord, 0, sizeof(InputRecord));
    InputRecord.EventType = KEY_EVENT;
    InputRecord.Event.KeyEvent.bKeyDown = true;
    InputRecord.Event.KeyEvent.wRepeatCount = 1;
    InputRecord.Event.KeyEvent.wVirtualKeyCode = VK_RETURN;
    InputRecord.Event.KeyEvent.uChar.AsciiChar = '\n';

    unsigned long Written;
    bool Result = WriteConsoleInput(FInstance->FInput, &InputRecord, 1, &Written);
    assert(Result);
    USEDPARAM(Result);
    assert(Written == 1);

    FInstance->FPendingAbort = true;

    SendMessage(Application->Handle, WM_INTERUPT_IDLE, 0, 0);
    
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
bool __fastcall TOwnConsole::Input(AnsiString & Str, bool Echo)
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
    SetConsoleMode(FInput, PrevMode);
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TOwnConsole::Choice(AnsiString Options, int Cancel, int Break)
{
  AnsiToOem(Options);

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
void __fastcall TOwnConsole::WaitBeforeExit()
{
  unsigned long Read;
  INPUT_RECORD Record;
  while (ReadConsoleInput(FInput, &Record, 1, &Read))
  {
    if ((Read == 1) && (Record.EventType == KEY_EVENT) &&
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
  __fastcall ~TExternalConsole();

  virtual void __fastcall Print(AnsiString Str, bool FromBeginning = false);
  virtual bool __fastcall Input(AnsiString & Str, bool Echo);
  virtual int __fastcall Choice(AnsiString Options, int Cancel, int Break);
  virtual bool __fastcall PendingAbort();
  virtual void __fastcall SetTitle(AnsiString Title);
  virtual void __fastcall WaitBeforeExit();

private:
  bool FPendingAbort;
  HANDLE FRequestEvent;
  HANDLE FResponseEvent;
  HANDLE FCancelEvent;
  HANDLE FFileMapping;
  static const int PrintTimeout = 5000;

  inline TConsoleCommStruct * __fastcall GetCommStruct();
  inline void __fastcall FreeCommStruct(TConsoleCommStruct * CommStruct);
  inline void __fastcall SendEvent(int Timeout);
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

  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    if (CommStruct->Version < TConsoleCommStruct::MinVersion)
    {
      throw Exception(FMTLOAD(EXTERNAL_CONSOLE_INCOMPATIBLE, (CommStruct->Version)));
    }
  }
  __finally
  {
    FreeCommStruct(CommStruct);
  }

  // to break application event loop regularly during "watching for changes"
  // to allow user to abort it
  SetTimer(Application->Handle, 1, 500, NULL);
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
bool __fastcall TExternalConsole::Input(AnsiString & Str, bool Echo)
{
  TConsoleCommStruct * CommStruct = GetCommStruct();
  try
  {
    CommStruct->Event = TConsoleCommStruct::INPUT;
    CommStruct->InputEvent.Echo = Echo;
    CommStruct->InputEvent.Result = false;
    CommStruct->InputEvent.Str[0] = '\0';
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
int __fastcall TExternalConsole::Choice(AnsiString Options, int Cancel, int Break)
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

  SendEvent(PrintTimeout);
}
//---------------------------------------------------------------------------
void __fastcall TExternalConsole::WaitBeforeExit()
{
  // noop
}
//---------------------------------------------------------------------------
class TConsoleRunner
{
public:
  TConsoleRunner(TConsole * Console);

  int __fastcall Run(const AnsiString Session, TStrings * ScriptCommands);

protected:
  bool __fastcall Input(AnsiString & Str, bool Echo);
  inline void __fastcall Print(const AnsiString & Str);
  inline void __fastcall PrintLine(const AnsiString & Str);
  inline void __fastcall PrintMessage(const AnsiString & Str);
  void __fastcall UpdateTitle();

private:
  TManagementScript * FScript;
  TConsole * FConsole;
  TSynchronizeController FSynchronizeController;
  int FLastProgressLen;
  bool FSynchronizeAborted;
  bool FAnyError;

  void __fastcall ScriptPrint(TScript * Script, const AnsiString Str);
  void __fastcall ScriptPrintProgress(TScript * Script, const AnsiString Str);
  void __fastcall ScriptInput(TScript * Script, const AnsiString Prompt, AnsiString & Str);
  void __fastcall ScriptTerminalUpdateStatus(TObject * Sender);
  void __fastcall ScriptTerminalPromptUser(TSecureShell * SecureShell,
    AnsiString Prompt, TPromptKind Kind, AnsiString & Response, bool & Result, void * Arg);
  void __fastcall ScriptShowExtendedException(TSecureShell * SecureShell,
    Exception * E, void * Arg);
  void __fastcall ScriptTerminalQueryUser(TObject * Sender, const AnsiString Query,
    TStrings * MoreMessages, int Answers, const TQueryParams * Params, int & Answer,
    TQueryType QueryType, void * Arg);
  void __fastcall ScriptQueryCancel(TScript * Script, bool & Cancel);
  void __fastcall SynchronizeControllerAbort(TObject * Sender, bool Close);
  void __fastcall ScriptSynchronizeStartStop(TScript * Script, 
    const AnsiString LocalDirectory, const AnsiString RemoteDirectory);
  void __fastcall SynchronizeControllerSynchronize(TSynchronizeController * Sender,
    const AnsiString LocalDirectory, const AnsiString RemoteDirectory,
    const TCopyParamType & CopyParam, const TSynchronizeParamType & Params,
    TSynchronizeStats * Stats, bool Full);
  void __fastcall SynchronizeControllerSynchronizeInvalid(TSynchronizeController * Sender,
    const AnsiString Directory, const AnsiString ErrorStr);
  void __fastcall ShowException(Exception * E);
};
//---------------------------------------------------------------------------
TConsoleRunner::TConsoleRunner(TConsole * Console) :
  FSynchronizeController(&SynchronizeControllerSynchronize,
    &SynchronizeControllerSynchronizeInvalid)
{
  FConsole = Console;
  FLastProgressLen = 0;
  FScript = NULL;
  FAnyError = false;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptInput(TScript * /*Script*/,
  const AnsiString Prompt, AnsiString & Str)
{
  Print(Prompt);

  if (!Input(Str, true))
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::Print(const AnsiString & Str)
{
  if (FLastProgressLen > 0)
  {
    FConsole->Print("\n" + Str);
    FLastProgressLen = 0;
  }
  else
  {
    FConsole->Print(Str);
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
void __fastcall TConsoleRunner::ScriptPrint(TScript * /*Script*/,
  const AnsiString Str)
{
  Print(Str);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptPrintProgress(TScript * /*Script*/,
  const AnsiString Str)
{
  AnsiString S = Str;
  if (S.Length() < FLastProgressLen)
  {
    S += AnsiString::StringOfChar(' ', FLastProgressLen - S.Length());
  }
  FConsole->Print(S, true);
  FLastProgressLen = Str.Length();
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptTerminalUpdateStatus(TObject * Sender)
{
  // duplicated in forms\OperationStatus.cpp
  static const int ConnectionStatusStrings[] =
    { STATUS_CLOSED, STATUS_INITWINSOCK, STATUS_LOOKUPHOST, STATUS_CONNECT,
      STATUS_AUTHENTICATE, STATUS_AUTHENTICATED, STATUS_STARTUP,
      STATUS_OPEN_DIRECTORY, STATUS_READY };

  TTerminal * Terminal = dynamic_cast<TTerminal *>(Sender);
  assert(Terminal != NULL);

  assert((Terminal->Status >= 0) && (Terminal->Status < LENOF(ConnectionStatusStrings)));
  AnsiString Status = LoadStr(ConnectionStatusStrings[Terminal->Status]);
  PrintLine(Status);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptTerminalPromptUser(TSecureShell * /*SecureShell*/,
  AnsiString Prompt, TPromptKind /*Kind*/, AnsiString & Response, bool & Result, 
  void * /*Arg*/)
{
  if (!Prompt.IsEmpty() && (Prompt[Prompt.Length()] != ' '))
  {
    Prompt += ' ';
  }
  Print(Prompt);

  Result = Input(Response, false);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptShowExtendedException(
  TSecureShell * /*SecureShell*/, Exception * E, void * /*Arg*/)
{
  ShowException(E);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptTerminalQueryUser(TObject * /*Sender*/,
  const AnsiString Query, TStrings * MoreMessages, int Answers,
  const TQueryParams * Params, int & Answer, TQueryType /*QueryType*/, 
  void * /*Arg*/)
{
  PrintMessage(Query);
  if ((MoreMessages != NULL) && (MoreMessages->Count > 0))
  {
    PrintMessage(MoreMessages->Text);
  }

  static const int MaxButtonCount = 15;
  int Buttons[MaxButtonCount];
  AnsiString Captions[MaxButtonCount];
  int ButtonCount = 0;

  int AAnswers = Answers;

  #define ADD_BUTTON(TYPE, CAPTION) \
    if (FLAGSET(AAnswers, qa ## TYPE)) \
    { \
      assert(ButtonCount < MaxButtonCount); \
      Captions[ButtonCount] = CAPTION; \
      Buttons[ButtonCount] = qa ## TYPE; \
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
  }

  assert(Accels.Pos(' ') == 0);

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
    Output += Caption;
  }
  Output += ": ";
  Print(Output);

  int AnswerIndex;
  if (FScript->Batch)
  {
    AnswerIndex = AbortIndex;
  }
  else
  {
    AnswerIndex = FConsole->Choice(Accels, CancelIndex, AbortIndex);
  }
  assert((AnswerIndex >= 1) && (AnswerIndex <= Accels.Length()));
  AnsiString AnswerCaption = Captions[AnswerIndex - 1];
  int P = AnswerCaption.Pos("&");
  assert(P >= 0);
  AnswerCaption.Delete(P, 1);
  PrintLine(AnswerCaption);

  Answer = Buttons[AnswerIndex - 1];
  if (Answer == qaAbort)
  {
    FAnyError = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptQueryCancel(TScript * /*Script*/, bool & Cancel)
{
  if (FConsole->PendingAbort())
  {
    Cancel = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleRunner::ScriptSynchronizeStartStop(TScript * /*Script*/,
  const AnsiString LocalDirectory, const AnsiString RemoteDirectory)
{
  TSynchronizeParamType Params;
  Params.LocalDirectory = LocalDirectory;
  Params.RemoteDirectory = RemoteDirectory;
  Params.Params = -1; // never used
  Params.Options = soRecurse;

  FSynchronizeController.StartStop(Application, true, Params,
    FScript->CopyParam, SynchronizeControllerAbort, NULL);

  try
  {
    FSynchronizeAborted = false;

    while (!FSynchronizeAborted && !FConsole->PendingAbort())
    {
      Application->HandleMessage();
    }
  }
  __finally
  {
    FSynchronizeController.StartStop(Application, false, Params,
      FScript->CopyParam, SynchronizeControllerAbort, NULL);
  }
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
  const TSynchronizeParamType & /*Params*/, TSynchronizeStats * Stats, bool Full)
{
  if (!Full)
  {
    FScript->Synchronize(LocalDirectory, RemoteDirectory, CopyParam, Stats);
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
void __fastcall TConsoleRunner::ShowException(Exception * E)
{
  if (!E->Message.IsEmpty() &&
      (dynamic_cast<EAbort *>(E) == NULL))
  {
    FAnyError = true;
    PrintMessage(E->Message);
    ExtException * EE = dynamic_cast<ExtException *>(E);
    if ((EE != NULL) && (EE->MoreMessages != NULL))
    {
      PrintMessage(EE->MoreMessages->Text);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TConsoleRunner::Input(AnsiString & Str, bool Echo)
{
  bool Result = FConsole->Input(Str, Echo);
  if (Result)
  {
    while (!Str.IsEmpty() &&
      ((Str[Str.Length()] == '\n') || (Str[Str.Length()] == '\r')))
    {
      Str.SetLength(Str.Length() - 1);
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TConsoleRunner::Run(const AnsiString Session, TStrings * ScriptCommands)
{
  try
  {
    FScript = new TManagementScript(StoredSessions);
    try
    {
      FScript->CopyParam = GUIConfiguration->DefaultCopyParam;
      FScript->OnPrint = ScriptPrint;
      FScript->OnPrintProgress = ScriptPrintProgress;
      FScript->OnInput = ScriptInput;
      FScript->OnTerminalUpdateStatus = ScriptTerminalUpdateStatus;
      FScript->OnTerminalPromptUser = ScriptTerminalPromptUser;
      FScript->OnShowExtendedException = ScriptShowExtendedException;
      FScript->OnTerminalQueryUser = ScriptTerminalQueryUser;
      FScript->OnQueryCancel = ScriptQueryCancel;
      FScript->OnSynchronizeStartStop = ScriptSynchronizeStartStop;

      UpdateTitle();
    
      if (!Session.IsEmpty())
      {
        FScript->Connect(Session);
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
          Print("winscp> ");
          Result = Input(Command, true);
        }
      
        if (Result)
        {
          FScript->Command(Command);
        }
      }
      while (Result && FScript->Continue);
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
    FAnyError = true;
  }

  return FAnyError ? 1 : 0;
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
int __fastcall Console(TProgramParams * Params, bool Help)
{
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
    else
    {
      Console = TOwnConsole::Instance();
    }

    if (Help)
    {
      AnsiString Usage = LoadStr(USAGE);
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
      AnsiString Value;
      if (Params->FindSwitch("script", Value))
      {
        ScriptCommands->LoadFromFile(Value);
      }
      Params->FindSwitch("command", ScriptCommands);

      Runner = new TConsoleRunner(Console);

      AnsiString Session;
      if (Params->ParamCount >= 1)
      {
        Session = Params->Param[1];
      }
      Result = Runner->Run(Session,
        (ScriptCommands->Count > 0 ? ScriptCommands : NULL));
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
