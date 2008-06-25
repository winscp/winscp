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
  fcOwnerChanging, fcAnyCommand, fcHardLink, fcSymbolicLink, fcResolveSymlink,
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
  virtual void __fastcall ShowExtendedException(Exception * E) = 0;
  virtual void __fastcall Closed() = 0;
};
//---------------------------------------------------------------------------
// Duplicated in LogMemo.h for design-time-only purposes
enum TLogLineType { llOutput, llInput, llStdError, llMessage, llException };
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TCaptureOutputEvent)(
  const AnsiString & Str, bool StdError);
typedef void __fastcall (__closure *TCalculatedChecksumEvent)(
  const AnsiString & FileName, const AnsiString & Alg, const AnsiString & Hash);
//---------------------------------------------------------------------------
class TCriticalSection;
//---------------------------------------------------------------------------
class TSessionLog : protected TStringList
{
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

  __property TSessionLog * Parent = { read = FParent, write = SetParent };
  __property bool Logging = { read = FLogging };
  __property int BottomIndex = { read = GetBottomIndex };
  __property AnsiString Line[int Index]  = { read=GetLine };
  __property TLogLineType Type[int Index]  = { read=GetType };
  __property OnChange;
  __property AnsiString CurrentFileName = { read = FCurrentFileName };
  __property bool LoggingToFile = { read = GetLoggingToFile };
  __property int TopIndex = { read = FTopIndex };
  __property AnsiString SessionName = { read = GetSessionName };
  __property AnsiString Name = { read = FName, write = FName };
  __property Count;

protected:
  void __fastcall CloseLogFile();
  bool __fastcall LogToFile();

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

  AnsiString __fastcall GetLine(int Index);
  TLogLineType __fastcall GetType(int Index);
  void DeleteUnnecessary();
  void OpenLogFile();
  int __fastcall GetBottomIndex();
  AnsiString __fastcall GetLogFileName();
  bool __fastcall GetLoggingToFile();
  void __fastcall SetParent(TSessionLog * value);
  AnsiString __fastcall GetSessionName();
  void __fastcall DoAdd(TLogLineType Type, AnsiString Line,
    void __fastcall (__closure *f)(TLogLineType Type, const AnsiString & Line));
  void __fastcall DoAddToParent(TLogLineType aType, const AnsiString & aLine);
  void __fastcall DoAddToSelf(TLogLineType aType, const AnsiString & aLine);
  void __fastcall DoAddStartupInfo(TSessionData * Data);
};
//---------------------------------------------------------------------------
#endif
