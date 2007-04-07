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
    Version1 = 1,
    Version2 = 2,
    CurrentVersion = Version2,
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
    unsigned int Timer; // since Version2
  };

  struct TChoiceEvent
  {
    char Options[64];
    int Cancel;
    int Break;
    int Result;
    int Timeouted; // since Version2
    unsigned int Timer; // since Version2
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
