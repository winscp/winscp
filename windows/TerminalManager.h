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
  void __fastcall ReconnectActiveTerminal();
  void __fastcall FreeActiveTerminal();
  void __fastcall CycleTerminals(bool Forward);

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
  TStrings * FTerminalList;
  int FProgress;
  ::TFileOperation FOperation;
  TList * FQueues;

  void __fastcall CreateLogMemo();
  void __fastcall FreeLogMemo();
  void __fastcall SetScpExplorer(TCustomScpExplorerForm * value);
  void __fastcall SetActiveTerminal(TTerminal * value);
  void __fastcall SetLogMemo(TLogMemo * value);
  void __fastcall UpdateTerminal(TTerminal * Terminal);
  void __fastcall UpdateAll();
  void __fastcall ApplicationException(TObject * Sender, Exception * E);
  void __fastcall ConfigurationChange(TObject * Sender);
  void __fastcall TerminalQueryUser(TObject * Sender,
    const AnsiString Query, TStrings * MoreMessages, int Answers,
    int Params, int & Answer, TQueryType Type);
  void __fastcall TerminalPromptUser(TSecureShell * SecureShell,
    AnsiString Prompt, TPromptKind Kind, AnsiString & Response, bool & Result);
  void __fastcall TerminalShowExtendedException(TSecureShell * SecureShell,
    Exception * E);
  void __fastcall FreeAll();
  void __fastcall TerminalReady();
  TStrings * __fastcall GetTerminalList();
  int __fastcall GetActiveTerminalIndex();
  AnsiString __fastcall GetActiveTerminalTitle();
  TTerminalQueue * __fastcall GetActiveQueue();
  void __fastcall SaveTerminal(TTerminal * Terminal);
  void __fastcall SetActiveTerminalIndex(int value);
  void __fastcall OperationFinished(::TFileOperation Operation, TOperationSide Side,
    bool DragDrop, const AnsiString FileName, bool Success,
    bool & DisconnectWhenFinished);
  void __fastcall OperationProgress(TFileOperationProgressType & ProgressData,
    TCancelStatus & Cancel);
  void __fastcall UpdateAppTitle();
  void __fastcall DeleteLocalFile(const AnsiString FileName);
};
//---------------------------------------------------------------------------
#endif
