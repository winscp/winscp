//---------------------------------------------------------------------------
#ifndef ConsoleH
#define ConsoleH
//---------------------------------------------------------------------------
#define CONSOLE_MAPPING "WinSCPConsoleMapping"
#define CONSOLE_EVENT_REQUEST "WinSCPConsoleEventRequest"
#define CONSOLE_EVENT_RESPONSE "WinSCPConsoleEventResponse"
#define CONSOLE_EVENT_CANCEL "WinSCPConsoleEventCancel"
//---------------------------------------------------------------------------
struct TConsoleCommStruct
{
  enum TVersion
  {
    Version0 = 0,
    CurrentVersion = Version0,
    MinVersion = Version0,
    MaxVersion = CurrentVersion
  };

  struct TPrintEvent
  {
    char Message[10240];
    bool FromBeginning;
  };

  struct TInputEvent
  {
    bool Echo;
    bool Result;
    char Str[10240];
  };

  struct TChoiceEvent
  {
    char Options[64];
    int Cancel;
    int Break;
    int Result;
  };

  struct TTitleEvent
  {
    char Title[10240];
  };

  size_t Size;
  int Version;
  enum { NONE, PRINT, INPUT, CHOICE, TITLE } Event;
  union
  {
    TPrintEvent PrintEvent;
    TInputEvent InputEvent;
    TChoiceEvent ChoiceEvent;
    TTitleEvent TitleEvent;
  };
};
//---------------------------------------------------------------------------
#endif // ConsoleH
