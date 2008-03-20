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
    CurrentVersion =          0x0003,
    CurrentVersionConfirmed = 0x0103
  };

  struct TInitEvent
  {
    unsigned int InputType;
    unsigned int OutputType;
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
