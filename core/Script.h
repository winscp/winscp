//---------------------------------------------------------------------------
#ifndef ScriptH
#define ScriptH
//---------------------------------------------------------------------------
#include <time.h>
#include "Terminal.h"
#include "FileOperationProgress.h"
//---------------------------------------------------------------------------
class TTerminal;
class TScript;
class TScriptCommands;
class TStoredSessionList;
class TTerminalList;
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TScriptPrintEvent)(TScript * Script, const AnsiString Str);
typedef void __fastcall (__closure *TScriptSynchronizeStartStop)(TScript * Script,
  const AnsiString LocalDirectory, const AnsiString RemoteDirectory);
//---------------------------------------------------------------------------
class TScriptProcParams
{
public:
  __fastcall TScriptProcParams(TStrings * Params, const AnsiString & ParamsStr);
  void __fastcall SkipParam();

  __property AnsiString Param[int Index] = { read = GetParam };
  __property int ParamCount = { read = GetParamCount };
  __property void * Arg = { read = FArg, write = FArg };
  __property AnsiString ParamsStr = { read = FParamsStr };

private:
  TStrings * FParams;
  int FSkipParams;
  void * FArg;
  AnsiString FParamsStr;

  AnsiString __fastcall GetParam(int Index);
  int __fastcall GetParamCount();
};
//---------------------------------------------------------------------------
class TScript
{
public:
  enum TBatchMode { BatchOff, BatchOn, BatchAbort };

  __fastcall TScript();
  __fastcall TScript(TTerminal * Terminal);
  virtual __fastcall ~TScript();

  void __fastcall Command(const AnsiString Cmd);

  void __fastcall Synchronize(const AnsiString LocalDirectory,
    const AnsiString RemoteDirectory, const TCopyParamType & CopyParam,
    TSynchronizeStats * Stats);

  __property TScriptPrintEvent OnPrint = { read = FOnPrint, write = FOnPrint };
  __property TExtendedExceptionEvent OnShowExtendedException = { read = FOnShowExtendedException, write = FOnShowExtendedException };
  __property TSynchronizeDirectory OnTerminalSynchronizeDirectory = { read = FOnTerminalSynchronizeDirectory, write = FOnTerminalSynchronizeDirectory };
  __property TScriptSynchronizeStartStop OnSynchronizeStartStop = { read = FOnSynchronizeStartStop, write = FOnSynchronizeStartStop };
  __property TCopyParamType CopyParam = { read = FCopyParam, write = SetCopyParam };
  __property int SynchronizeParams = { read = FSynchronizeParams, write = SetSynchronizeParams };
  __property TBatchMode Batch = { read = FBatch };
  __property TTerminal * Terminal = { read = FTerminal };

protected:
  TTerminal * FTerminal;
  TScriptCommands * FCommands;
  TScriptPrintEvent FOnPrint;
  TExtendedExceptionEvent FOnShowExtendedException;
  TSynchronizeDirectory FOnTerminalSynchronizeDirectory;
  TScriptSynchronizeStartStop FOnSynchronizeStartStop;
  TCopyParamType FCopyParam;
  TBatchMode FBatch;
  bool FConfirm;
  int FSynchronizeParams;
  int FSynchronizeMode;
  bool FKeepingUpToDate;
  AnsiString FSynchronizeIntro;

  virtual void __fastcall ResetTransfer();
  virtual void __fastcall ConnectTerminal(TTerminal * Terminal);
  bool __fastcall EnsureCommandSessionFallback(TFSCapability Capability);
  void __fastcall Print(const AnsiString Str);
  void __fastcall PrintLine(const AnsiString Str);
  void __fastcall Tokenize(const AnsiString Str, TStrings * Tokens,
    AnsiString & AllButFirst);
  void __fastcall CheckSession();
  enum TFileListType
  {
    fltDefault =     0x00,
    fltDirectories = 0x01,
    fltQueryServer = 0x02,
    fltMask =        0x04
  };
  TStrings * __fastcall CreateFileList(TScriptProcParams * Parameters, int Start,
    int End, TFileListType ListType = fltDefault);
  TStrings * __fastcall CreateLocalFileList(TScriptProcParams * Parameters,
    int Start, int End, TFileListType ListType);
  void __fastcall FreeFileList(TStrings * FileList);

  void __fastcall SecondaryProc(TScriptProcParams * Parameters);
  void __fastcall DummyProc(TScriptProcParams * Parameters);
  void __fastcall HelpProc(TScriptProcParams * Parameters);
  void __fastcall CallProc(TScriptProcParams * Parameters);
  void __fastcall PwdProc(TScriptProcParams * Parameters);
  void __fastcall CdProc(TScriptProcParams * Parameters);
  void __fastcall LsProc(TScriptProcParams * Parameters);
  void __fastcall RmProc(TScriptProcParams * Parameters);
  void __fastcall RmDirProc(TScriptProcParams * Parameters);
  void __fastcall MvProc(TScriptProcParams * Parameters);
  void __fastcall ChModProc(TScriptProcParams * Parameters);
  void __fastcall LnProc(TScriptProcParams * Parameters);
  void __fastcall MkDirProc(TScriptProcParams * Parameters);
  void __fastcall GetProc(TScriptProcParams * Parameters);
  void __fastcall PutProc(TScriptProcParams * Parameters);
  void __fastcall OptionProc(TScriptProcParams * Parameters);
  void __fastcall AsciiProc(TScriptProcParams * Parameters);
  void __fastcall BinaryProc(TScriptProcParams * Parameters);
  void __fastcall SynchronizeProc(TScriptProcParams * Parameters);
  void __fastcall KeepUpToDateProc(TScriptProcParams * Parameters);

  void __fastcall OptionImpl(AnsiString OptionName, AnsiString ValueName);
  void __fastcall SynchronizeDirectories(TScriptProcParams * Parameters,
    AnsiString & LocalDirectory, AnsiString & RemoteDirectory, int FirstParam);
  virtual bool __fastcall HandleExtendedException(Exception * E,
    TTerminal * Terminal = NULL);
  void __fastcall TerminalCaptureLog(TObject * Sender, TLogLineType Type,
    const AnsiString AddedLine);

private:
  void __fastcall Init();
  void __fastcall SetCopyParam(const TCopyParamType & value);
  void __fastcall SetSynchronizeParams(int value);
};
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TScriptInputEvent)(TScript * Script, const AnsiString Prompt, AnsiString & Str);
typedef void __fastcall (__closure *TScriptQueryCancelEvent)(TScript * Script, bool & Cancel);
typedef void __fastcall (__closure *TScriptPrintProgressEvent)(TScript * Script, bool First, const AnsiString Str);
//---------------------------------------------------------------------------
class TManagementScript : public TScript
{
public:
  __fastcall TManagementScript(TStoredSessionList * StoredSessions);
  virtual __fastcall ~TManagementScript();

  void __fastcall Connect(const AnsiString Session);

  __property TScriptInputEvent OnInput = { read = FOnInput, write = FOnInput };
  __property TScriptQueryCancelEvent OnQueryCancel = { read = FOnQueryCancel, write = FOnQueryCancel };
  __property TNotifyEvent OnTerminalUpdateStatus = { read = FOnTerminalUpdateStatus, write = FOnTerminalUpdateStatus };
  __property TPromptUserEvent OnTerminalPromptUser = { read = FOnTerminalPromptUser, write = FOnTerminalPromptUser };
  __property TQueryUserEvent OnTerminalQueryUser = { read = FOnTerminalQueryUser, write = FOnTerminalQueryUser };
  __property TScriptPrintProgressEvent OnPrintProgress = { read = FOnPrintProgress, write = FOnPrintProgress };
  __property bool Continue = { read = FContinue };

protected:
  TScriptInputEvent FOnInput;
  TScriptQueryCancelEvent FOnQueryCancel;
  TNotifyEvent FOnTerminalUpdateStatus;
  TPromptUserEvent FOnTerminalPromptUser;
  TQueryUserEvent FOnTerminalQueryUser;
  TScriptPrintProgressEvent FOnPrintProgress;
  TStoredSessionList * FStoredSessions;
  TTerminalList * FTerminalList;
  AnsiString FLastProgressFile;
  AnsiString FLastProgressMessage;
  time_t FLastProgressTime;
  bool FContinue;

  virtual void __fastcall ResetTransfer();
  virtual void __fastcall ConnectTerminal(TTerminal * Terminal);
  void __fastcall Input(const AnsiString Prompt, AnsiString & Str, bool AllowEmpty);
  void __fastcall TerminalOnStdError(TObject * Sender, TLogLineType Type,
    const AnsiString AddedLine);
  void __fastcall TerminalOperationProgress(TFileOperationProgressType & ProgressData,
    TCancelStatus & Cancel);
  void __fastcall TerminalOperationFinished(TFileOperation Operation, TOperationSide Side,
    bool Temp, const AnsiString FileName, Boolean Success,
    bool & DisconnectWhenComplete);

  void __fastcall PrintActiveSession();
  TTerminal * __fastcall FindSession(const AnsiString Index);
  void __fastcall FreeTerminal(TTerminal * Terminal);
  void __fastcall PrintProgress(bool First, const AnsiString Str);
  bool __fastcall QueryCancel();
  void __fastcall TerminalSynchronizeDirectory(const AnsiString LocalDirectory,
    const AnsiString RemoteDirectory, bool & Continue);
  void __fastcall DoConnect(const AnsiString Session);
  void __fastcall DoClose(TTerminal * Terminal);
  virtual bool __fastcall HandleExtendedException(Exception * E,
    TTerminal * Terminal = NULL);
  void __fastcall TerminalPromptUser(TSecureShell * SecureShell,
    AnsiString Prompt, TPromptKind Kind, AnsiString & Response, bool & Result,
    void * Arg);
  inline bool __fastcall Synchronizing();
  inline void __fastcall ShowPendingProgress();

  void __fastcall ExitProc(TScriptProcParams * Parameters);
  void __fastcall OpenProc(TScriptProcParams * Parameters);
  void __fastcall CloseProc(TScriptProcParams * Parameters);
  void __fastcall SessionProc(TScriptProcParams * Parameters);
  void __fastcall LPwdProc(TScriptProcParams * Parameters);
  void __fastcall LCdProc(TScriptProcParams * Parameters);
  void __fastcall LLsProc(TScriptProcParams * Parameters);
};
//---------------------------------------------------------------------------
#endif
