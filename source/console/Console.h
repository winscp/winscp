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
    CurrentVersion =          0x0008,
    CurrentVersionConfirmed = 0x0108
  };

  struct TInitEvent
  {
    unsigned int InputType;
    unsigned int OutputType;
    bool WantsProgress; // since version 6
  };

  struct TPrintEvent
  {
    wchar_t Message[10240]; // wide since version 4
    bool FromBeginning;
    bool Error; // since vesion 7
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

  // Since version 6
  struct TProgressEvent
  {
    enum { COPY } Operation;
    enum { LOCAL, REMOTE } Side;
    wchar_t FileName[1024];
    wchar_t Directory[1024];
    unsigned int OverallProgress;
    unsigned int FileProgress;
    unsigned int CPS;
    bool Cancel; // since version 8
  };

  size_t Size;
  int Version;
  enum { NONE, PRINT, INPUT, CHOICE, TITLE, INIT, PROGRESS } Event;

  union
  {
    TPrintEvent PrintEvent;
    TInputEvent InputEvent;
    TChoiceEvent ChoiceEvent;
    TTitleEvent TitleEvent;
    TInitEvent InitEvent;
    TProgressEvent ProgressEvent;
  };
};
//---------------------------------------------------------------------------
#endif // ConsoleH
