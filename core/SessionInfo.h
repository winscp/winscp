//---------------------------------------------------------------------------
#ifndef SessionInfoH
#define SessionInfoH

#include "SessionData.h"
#include "Interface.h"
//---------------------------------------------------------------------------
enum TSessionStatus { ssClosed, ssOpening, ssOpened };
//---------------------------------------------------------------------------
struct TSessionInfo
{
  TSessionInfo();

  TDateTime LoginTime;
  AnsiString ProtocolBaseName;
  AnsiString ProtocolName;
  AnsiString SecurityProtocolName;

  AnsiString CSCipher;
  AnsiString CSCompression;
  AnsiString SCCipher;
  AnsiString SCCompression;

  AnsiString SshVersionString;
  AnsiString SshImplementation;
  AnsiString HostKeyFingerprint;
};
//---------------------------------------------------------------------------
enum TFSCapability { fcUserGroupListing, fcModeChanging, fcGroupChanging,
  fcOwnerChanging, fcGroupOwnerChangingByID, fcAnyCommand, fcHardLink,
  fcSymbolicLink, fcResolveSymlink,
  fcTextMode, fcRename, fcNativeTextMode, fcNewerOnlyUpload, fcRemoteCopy,
  fcTimestampChanging, fcRemoteMove, fcLoadingAdditionalProperties,
  fcCheckingSpaceAvailable, fcIgnorePermErrors, fcCalculatingChecksum,
  fcModeChangingUpload, fcPreservingTimestampUpload, fcShellAnyCommand,
  fcSecondaryShell, fcCount };
//---------------------------------------------------------------------------
struct TFileSystemInfo
{
  TFileSystemInfo();

  AnsiString ProtocolBaseName;
  AnsiString ProtocolName;
  AnsiString RemoteSystem;
  AnsiString AdditionalInfo;
  bool IsCapable[fcCount];
};
//---------------------------------------------------------------------------
class TSessionUI
{
public:
  virtual void __fastcall Information(const AnsiString & Str, bool Status) = 0;
  virtual int __fastcall QueryUser(const AnsiString Query,
    TStrings * MoreMessages, int Answers, const TQueryParams * Params,
    TQueryType QueryType = qtConfirmation) = 0;
  virtual int __fastcall QueryUserException(const AnsiString Query,
    Exception * E, int Answers, const TQueryParams * Params,
    TQueryType QueryType = qtConfirmation) = 0;
  virtual bool __fastcall PromptUser(TSessionData * Data, TPromptKind Kind,
    AnsiString Name, AnsiString Instructions, TStrings * Prompts,
    TStrings * Results) = 0;
  virtual void __fastcall DisplayBanner(const AnsiString & Banner) = 0;
  virtual void __fastcall FatalError(Exception * E, AnsiString Msg) = 0;
  virtual void __fastcall HandleExtendedException(Exception * E) = 0;
  virtual void __fastcall Closed() = 0;
};
//---------------------------------------------------------------------------
// Duplicated in LogMemo.h for design-time-only purposes
enum TLogLineType { llOutput, llInput, llStdError, llMessage, llException, llAction };
enum TLogAction { laUpload, laDownload, laTouch, laChmod, laMkdir, laRm, laMv, laCall, laLs };
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TCaptureOutputEvent)(
  const AnsiString & Str, bool StdError);
typedef void __fastcall (__closure *TCalculatedChecksumEvent)(
  const AnsiString & FileName, const AnsiString & Alg, const AnsiString & Hash);
//---------------------------------------------------------------------------
class TCriticalSection;
class TSessionActionRecord;
class TSessionLog;
//---------------------------------------------------------------------------
class TSessionAction
{
public:
  __fastcall TSessionAction(TSessionLog * Log, TLogAction Action);
  __fastcall ~TSessionAction();

  void __fastcall Restart();

  void __fastcall Commit();
  void __fastcall Rollback(Exception * E = NULL);
  void __fastcall Cancel();

protected:
  TSessionActionRecord * FRecord;
};
//---------------------------------------------------------------------------
class TFileSessionAction : public TSessionAction
{
public:
  __fastcall TFileSessionAction(TSessionLog * Log, TLogAction Action);
  __fastcall TFileSessionAction(TSessionLog * Log, TLogAction Action, const AnsiString & FileName);

  void __fastcall FileName(const AnsiString & FileName);
};
//---------------------------------------------------------------------------
class TFileLocationSessionAction : public TFileSessionAction
{
public:
  __fastcall TFileLocationSessionAction(TSessionLog * Log, TLogAction Action);
  __fastcall TFileLocationSessionAction(TSessionLog * Log, TLogAction Action, const AnsiString & FileName);

  void __fastcall Destination(const AnsiString & Destination);
};
//---------------------------------------------------------------------------
class TUploadSessionAction : public TFileLocationSessionAction
{
public:
  __fastcall TUploadSessionAction(TSessionLog * Log);
};
//---------------------------------------------------------------------------
class TDownloadSessionAction : public TFileLocationSessionAction
{
public:
  __fastcall TDownloadSessionAction(TSessionLog * Log);
};
//---------------------------------------------------------------------------
class TRights;
//---------------------------------------------------------------------------
class TChmodSessionAction : public TFileSessionAction
{
public:
  __fastcall TChmodSessionAction(TSessionLog * Log, const AnsiString & FileName);
  __fastcall TChmodSessionAction(TSessionLog * Log, const AnsiString & FileName,
    const TRights & Rights);

  void __fastcall Rights(const TRights & Rights);
  void __fastcall Recursive();
};
//---------------------------------------------------------------------------
class TTouchSessionAction : public TFileSessionAction
{
public:
  __fastcall TTouchSessionAction(TSessionLog * Log, const AnsiString & FileName,
    const TDateTime & Modification);
};
//---------------------------------------------------------------------------
class TMkdirSessionAction : public TFileSessionAction
{
public:
  __fastcall TMkdirSessionAction(TSessionLog * Log, const AnsiString & FileName);
};
//---------------------------------------------------------------------------
class TRmSessionAction : public TFileSessionAction
{
public:
  __fastcall TRmSessionAction(TSessionLog * Log, const AnsiString & FileName);

  void __fastcall Recursive();
};
//---------------------------------------------------------------------------
class TMvSessionAction : public TFileLocationSessionAction
{
public:
  __fastcall TMvSessionAction(TSessionLog * Log, const AnsiString & FileName,
    const AnsiString & Destination);
};
//---------------------------------------------------------------------------
class TCallSessionAction : public TSessionAction
{
public:
  __fastcall TCallSessionAction(TSessionLog * Log, const AnsiString & Command,
    const AnsiString & Destination);

  void __fastcall AddOutput(const AnsiString & Output, bool StdError);
};
//---------------------------------------------------------------------------
class TLsSessionAction : public TSessionAction
{
public:
  __fastcall TLsSessionAction(TSessionLog * Log, const AnsiString & Destination);

  void __fastcall FileList(TRemoteFileList * FileList);
};
//---------------------------------------------------------------------------
class TSessionLog : protected TStringList
{
friend class TSessionAction;
friend class TSessionActionRecord;
public:
  __fastcall TSessionLog(TSessionUI* UI, TSessionData * SessionData,
    TConfiguration * Configuration);
  __fastcall ~TSessionLog();
  HIDESBASE void __fastcall Add(TLogLineType Type, const AnsiString & Line);
  void __fastcall AddStartupInfo();
  void __fastcall AddException(Exception * E);
  void __fastcall AddSeparator();

  virtual void __fastcall Clear();
  void __fastcall ReflectSettings();
  void __fastcall Lock();
  void __fastcall Unlock();

  __property TSessionLog * Parent = { read = FParent, write = FParent };
  __property bool Logging = { read = FLogging };
  __property int BottomIndex = { read = GetBottomIndex };
  __property AnsiString Line[int Index]  = { read=GetLine };
  __property TLogLineType Type[int Index]  = { read=GetType };
  __property OnChange;
  __property TNotifyEvent OnStateChange = { read = FOnStateChange, write = FOnStateChange };
  __property AnsiString CurrentFileName = { read = FCurrentFileName };
  __property bool LoggingToFile = { read = GetLoggingToFile };
  __property int TopIndex = { read = FTopIndex };
  __property AnsiString SessionName = { read = GetSessionName };
  __property AnsiString Name = { read = FName, write = FName };
  __property Count;

protected:
  void __fastcall CloseLogFile();
  bool __fastcall LogToFile();
  inline void __fastcall AddPendingAction(TSessionActionRecord * Action);
  void __fastcall RecordPendingActions();

private:
  TConfiguration * FConfiguration;
  TSessionLog * FParent;
  TCriticalSection * FCriticalSection;
  bool FLogging;
  void * FFile;
  AnsiString FCurrentLogFileName;
  AnsiString FCurrentFileName;
  int FLoggedLines;
  int FTopIndex;
  TSessionUI * FUI;
  TSessionData * FSessionData;
  AnsiString FName;
  bool FLoggingActions;
  bool FClosed;
  TList * FPendingActions;
  TNotifyEvent FOnStateChange;

  AnsiString __fastcall GetLine(int Index);
  TLogLineType __fastcall GetType(int Index);
  void DeleteUnnecessary();
  void StateChange();
  void OpenLogFile();
  int __fastcall GetBottomIndex();
  AnsiString __fastcall GetLogFileName();
  bool __fastcall GetLoggingToFile();
  AnsiString __fastcall GetSessionName();
  void __fastcall DoAdd(TLogLineType Type, AnsiString Line,
    void __fastcall (__closure *f)(TLogLineType Type, const AnsiString & Line));
  void __fastcall DoAddToParent(TLogLineType aType, const AnsiString & aLine);
  void __fastcall DoAddToSelf(TLogLineType aType, const AnsiString & aLine);
  void __fastcall DoAddStartupInfo(TSessionData * Data);
};
//---------------------------------------------------------------------------
#endif
