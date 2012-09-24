//---------------------------------------------------------------------------
#ifndef ConsoleH
#define ConsoleH
//---------------------------------------------------------------------------
#define CONSOLE_MAPPING L"WinSCPConsoleMapping"
#define CONSOLE_EVENT_REQUEST L"WinSCPConsoleEventRequest"
#define CONSOLE_EVENT_RESPONSE L"WinSCPConsoleEventResponse"
#define CONSOLE_EVENT_CANCEL L"WinSCPConsoleEventCancel"
#define CONSOLE_JOB L"WinSCPConsoleJob"
//---------------------------------------------------------------------------
struct TConsoleCommStruct
{
  enum TVersion
  {
    // Version 5 was actually mistake and there's no difference to version 4
    CurrentVersion =          0x0005,
    CurrentVersionConfirmed = 0x0105
  };

  struct TInitEvent
  {
    unsigned int InputType;
    unsigned int OutputType;
  };

  struct TPrintEvent
  {
    wchar_t Message[10240]; // wide since version 4
    bool FromBeginning;
  };

  struct TInputEvent
  {
    bool Echo;
    bool Result;
    wchar_t Str[10240]; // wide since version 4
    unsigned int Timer; // since version 2
  };

  struct TChoiceEvent
  {
    wchar_t Options[64]; // wide since version 4
    int Cancel;
    int Break;
    int Result;
    int Timeouted; // since version 2
    unsigned int Timer; // since version 2
    bool Timeouting; // since version 4
  };

  struct TTitleEvent
  {
    wchar_t Title[10240]; // wide since version 4
  };

  size_t Size;
  int Version;
  enum { NONE, PRINT, INPUT, CHOICE, TITLE, INIT } Event;

  union
  {
    TPrintEvent PrintEvent;
    TInputEvent InputEvent;
    TChoiceEvent ChoiceEvent;
    TTitleEvent TitleEvent;
    TInitEvent InitEvent;
  };
};
//---------------------------------------------------------------------------
#endif // ConsoleH
