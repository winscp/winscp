//---------------------------------------------------------------------------
#ifndef TerminalManagerH
#define TerminalManagerH
//---------------------------------------------------------------------------
#include <Terminal.h>
#include <FileOperationProgress.h>
//---------------------------------------------------------------------------
class TCustomScpExplorerForm;
class TLogMemo;
class TTerminalQueue;
class TAuthenticateForm;
//---------------------------------------------------------------------------
enum TTerminalPendingAction { tpNull, tpNone, tpReconnect, tpFree };
//---------------------------------------------------------------------------
class TTerminalManager : public TTerminalList
{
public:
  static TTerminalManager * __fastcall Instance(bool ForceCreation = true);
  static void __fastcall DestroyInstance();

  __fastcall TTerminalManager();
  __fastcall ~TTerminalManager();

  virtual TTerminal * __fastcall NewTerminal(TSessionData * Data);
  virtual void __fastcall FreeTerminal(TTerminal * Terminal);
  bool __fastcall ConnectActiveTerminal();
  void __fastcall DisconnectActiveTerminal();
  void __fastcall ReconnectActiveTerminal();
  void __fastcall FreeActiveTerminal();
  void __fastcall CycleTerminals(bool Forward);
  static void ConnectTerminal(TTerminal * Terminal, bool Reopen);
  AnsiString __fastcall UpdateAppTitle();
  bool __fastcall CanOpenInPutty();
  void __fastcall OpenInPutty();
  bool __fastcall NewSession();

  __property TCustomScpExplorerForm * ScpExplorer = { read = FScpExplorer, write = SetScpExplorer };
  __property TTerminal * ActiveTerminal = { read = FActiveTerminal, write = SetActiveTerminal };
  __property TTerminalQueue * ActiveQueue = { read = GetActiveQueue };
  __property int ActiveTerminalIndex = { read = GetActiveTerminalIndex, write = SetActiveTerminalIndex };
  __property AnsiString ActiveTerminalTitle = { read = GetActiveTerminalTitle };
  __property TStrings * TerminalList = { read = GetTerminalList };
  __property TLogMemo * LogMemo = { read = FLogMemo };
  __property TNotifyEvent OnLastTerminalClosed = { read = FOnLastTerminalClosed, write = FOnLastTerminalClosed };
  __property TNotifyEvent OnChangeTerminal = { read = FOnChangeTerminal, write = FOnChangeTerminal };
  __property TNotifyEvent OnTerminalListChanged = { read = FOnTerminalListChanged, write = FOnTerminalListChanged };
  __property TNotifyEvent OnTerminalClosed = { read = FOnTerminalClosed, write = FOnTerminalClosed };

private:
  static TTerminalManager * FInstance;
  TCustomScpExplorerForm * FScpExplorer;
  TTerminal * FActiveTerminal;
  TLogMemo * FLogMemo;
  bool FDestroying;
  TTerminalPendingAction FTerminalPendingAction;
  TNotifyEvent FOnLastTerminalClosed;
  TNotifyEvent FOnTerminalListChanged;
  TNotifyEvent FOnChangeTerminal;
  TNotifyEvent FOnTerminalClosed;
  TStrings * FTerminalList;
  TList * FQueues;
  AnsiString FProgressTitle;
  TDateTime FDirectoryReadingStart;
  TAuthenticateForm * FAuthenticateForm;

  bool __fastcall ConnectActiveTerminalImpl(bool Reopen);
  TTerminalQueue * __fastcall NewQueue(TTerminal * Terminal);
  void __fastcall CreateLogMemo();
  void __fastcall FreeLogMemo();
  void __fastcall SetScpExplorer(TCustomScpExplorerForm * value);
  void __fastcall SetActiveTerminal(TTerminal * value);
  void __fastcall SetLogMemo(TLogMemo * value);
  void __fastcall UpdateTerminal(TTerminal * Terminal);
  void __fastcall UpdateAll();
  void __fastcall ApplicationException(TObject * Sender, Exception * E);
  void __fastcall ApplicationShowHint(AnsiString & HintStr, bool & CanShow,
    THintInfo & HintInfo);
  void __fastcall ConfigurationChange(TObject * Sender);
  void __fastcall TerminalUpdateStatus(TSecureShell * SecureShell, bool Active);
  void __fastcall TerminalQueryUser(TObject * Sender,
    const AnsiString Query, TStrings * MoreMessages, int Answers,
    const TQueryParams * Params, int & Answer, TQueryType Type, void * Arg);
  void __fastcall TerminalPromptUser(TSecureShell * SecureShell,
    AnsiString Prompt, TPromptKind Kind, AnsiString & Response, bool & Result,
    void * Arg);
  void __fastcall TerminalDisplayBanner(TSecureShell * SecureShell,
    AnsiString SessionName, const AnsiString & Banner, bool & NeverShowAgain,
    int Options);
  void __fastcall TerminalShowExtendedException(TSecureShell * SecureShell,
    Exception * E, void * Arg);
  void __fastcall TerminalReadDirectoryProgress(TObject * Sender, int Progress,
    bool & Cancel);
  void __fastcall TerminalOnStdError(TObject * Sender, TLogLineType Type,
    const AnsiString AddedLine);
  void __fastcall FreeAll();
  void __fastcall TerminalReady();
  TStrings * __fastcall GetTerminalList();
  int __fastcall GetActiveTerminalIndex();
  AnsiString __fastcall GetActiveTerminalTitle();
  TTerminalQueue * __fastcall GetActiveQueue();
  void __fastcall SaveTerminal(TTerminal * Terminal);
  void __fastcall SetActiveTerminalIndex(int value);
  void __fastcall OperationFinished(::TFileOperation Operation, TOperationSide Side,
    bool Temp, const AnsiString FileName, bool Success,
    bool & DisconnectWhenFinished);
  void __fastcall OperationProgress(TFileOperationProgressType & ProgressData,
    TCancelStatus & Cancel);
  void __fastcall DeleteLocalFile(const AnsiString FileName);
};
//---------------------------------------------------------------------------
#endif
