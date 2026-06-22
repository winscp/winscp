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
  UnicodeString ProtocolBaseName;
  UnicodeString ProtocolName;
  UnicodeString SecurityProtocolName;

  UnicodeString CSCipher;
  UnicodeString CSCompression;
  UnicodeString SCCipher;
  UnicodeString SCCompression;

  UnicodeString SshVersionString;
  UnicodeString SshImplementation;
  UnicodeString HostKeyFingerprintSHA256;
  UnicodeString HostKeyFingerprintMD5;

  UnicodeString CertificateFingerprintSHA1;
  UnicodeString CertificateFingerprintSHA256;
  UnicodeString Certificate;
  bool CertificateVerifiedManually;
};
//---------------------------------------------------------------------------
enum TFSCapability { fcUserGroupListing, fcModeChanging, fcAclChangingFiles, fcGroupChanging,
  fcOwnerChanging, fcGroupOwnerChangingByID, fcAnyCommand, fcHardLink,
  fcSymbolicLink,
  // With WebDAV this is always true, to avoid double-click on
  // file try to open the file as directory. It does no harm atm as
  // WebDAV never produce a symlink in listing.
  fcResolveSymlink,
  fcTextMode, fcRename, fcNativeTextMode, fcNewerOnlyUpload, fcRemoteCopy,
  fcTimestampChanging, fcRemoteMove, fcLoadingAdditionalProperties,
  fcCheckingSpaceAvailable, fcIgnorePermErrors, fcCalculatingChecksum,
  fcModeChangingUpload, fcPreservingTimestampUpload, fcShellAnyCommand,
  fcSecondaryShell, fcRemoveCtrlZUpload, fcRemoveBOMUpload, fcMoveToQueue,
  fcLocking, fcPreservingTimestampDirs, fcResumeSupport,
  fcChangePassword, fcSkipTransfer,
  fcParallelTransfers, fcParallelFileTransfers,
  fcBackgroundTransfers,
  fcTransferOut, fcTransferIn,
  fcMoveOverExistingFile,
  fcTags,
  fcCount };
//---------------------------------------------------------------------------
struct TFileSystemInfo
{
  TFileSystemInfo();

  UnicodeString ProtocolBaseName;
  UnicodeString ProtocolName;
  UnicodeString RemoteSystem;
  UnicodeString AdditionalInfo;
  bool IsCapable[fcCount];
};
//---------------------------------------------------------------------------
class TSessionUI
{
public:
  virtual void __fastcall Information(const UnicodeString & Str) = 0;
  virtual unsigned int __fastcall QueryUser(const UnicodeString Query,
    TStrings * MoreMessages, unsigned int Answers, const TQueryParams * Params,
    TQueryType QueryType = qtConfirmation) = 0;
  virtual unsigned int __fastcall QueryUserException(const UnicodeString Query,
    Exception * E, unsigned int Answers, const TQueryParams * Params,
    TQueryType QueryType = qtConfirmation) = 0;
  virtual bool __fastcall PromptUser(TSessionData * Data, TPromptKind Kind,
    UnicodeString Name, UnicodeString Instructions, TStrings * Prompts,
    TStrings * Results) = 0;
  virtual void __fastcall DisplayBanner(const UnicodeString & Banner) = 0;
  virtual void __fastcall FatalError(Exception * E, UnicodeString Msg, UnicodeString HelpKeyword = L"") = 0;
  virtual void __fastcall HandleExtendedException(Exception * E) = 0;
  virtual void __fastcall Closed() = 0;
  virtual void __fastcall ProcessGUI() = 0;
};
//---------------------------------------------------------------------------
enum TLogLineType { llOutput, llInput, llStdError, llMessage, llException };
enum TLogAction
{
  laUpload, laDownload, laTouch, laChmod, laMkdir, laRm, laMv, laCp, laCall, laLs,
  laStat, laChecksum, laCwd, laDifference
};
//---------------------------------------------------------------------------
enum TCaptureOutputType { cotOutput, cotError, cotExitCode };
typedef void __fastcall (__closure *TCaptureOutputEvent)(
  const UnicodeString & Str, TCaptureOutputType OutputType);
typedef void __fastcall (__closure *TCalculatedChecksumEvent)(
  const UnicodeString & FileName, const UnicodeString & Alg, const UnicodeString & Hash);
//---------------------------------------------------------------------------
class TSessionActionRecord;
class TActionLog;
//---------------------------------------------------------------------------
class TSessionAction
{
public:
  __fastcall TSessionAction(TActionLog * Log, TLogAction Action);
  __fastcall ~TSessionAction();

  void __fastcall Restart();

  void __fastcall Rollback(Exception * E = NULL);
  void __fastcall Cancel();

  bool __fastcall IsValid();

protected:
  TSessionActionRecord * FRecord;
  bool FCancelled;
};
//---------------------------------------------------------------------------
class TFileSessionAction : public TSessionAction
{
public:
  __fastcall TFileSessionAction(TActionLog * Log, TLogAction Action);
  __fastcall TFileSessionAction(TActionLog * Log, TLogAction Action, const UnicodeString & FileName);

  void __fastcall FileName(const UnicodeString & FileName);
};
//---------------------------------------------------------------------------
class TFileLocationSessionAction : public TFileSessionAction
{
public:
  __fastcall TFileLocationSessionAction(TActionLog * Log, TLogAction Action);
  __fastcall TFileLocationSessionAction(TActionLog * Log, TLogAction Action, const UnicodeString & FileName);

  void __fastcall Destination(const UnicodeString & Destination);
};
//---------------------------------------------------------------------------
class TTransferSessionAction : public TFileLocationSessionAction
{
public:
  TTransferSessionAction(TActionLog * Log, TLogAction Action);

  void Size(__int64 Size);
};
//---------------------------------------------------------------------------
class TUploadSessionAction : public TTransferSessionAction
{
public:
  __fastcall TUploadSessionAction(TActionLog * Log);
};
//---------------------------------------------------------------------------
class TDownloadSessionAction : public TTransferSessionAction
{
public:
  __fastcall TDownloadSessionAction(TActionLog * Log);
};
//---------------------------------------------------------------------------
class TRights;
//---------------------------------------------------------------------------
class TChmodSessionAction : public TFileSessionAction
{
public:
  __fastcall TChmodSessionAction(TActionLog * Log, const UnicodeString & FileName);
  __fastcall TChmodSessionAction(TActionLog * Log, const UnicodeString & FileName,
    const TRights & Rights);

  void __fastcall Rights(const TRights & Rights);
  void __fastcall Recursive();
};
//---------------------------------------------------------------------------
class TTouchSessionAction : public TFileSessionAction
{
public:
  __fastcall TTouchSessionAction(TActionLog * Log, const UnicodeString & FileName,
    const TDateTime & Modification);
};
//---------------------------------------------------------------------------
class TMkdirSessionAction : public TFileSessionAction
{
public:
  __fastcall TMkdirSessionAction(TActionLog * Log, const UnicodeString & FileName);
};
//---------------------------------------------------------------------------
class TRmSessionAction : public TFileSessionAction
{
public:
  __fastcall TRmSessionAction(TActionLog * Log, const UnicodeString & FileName);

  void __fastcall Recursive();
};
//---------------------------------------------------------------------------
class TMvSessionAction : public TFileLocationSessionAction
{
public:
  __fastcall TMvSessionAction(TActionLog * Log, const UnicodeString & FileName,
    const UnicodeString & Destination);
};
//---------------------------------------------------------------------------
class TCpSessionAction : public TFileLocationSessionAction
{
public:
  __fastcall TCpSessionAction(TActionLog * Log, const UnicodeString & FileName,
    const UnicodeString & Destination);
};
//---------------------------------------------------------------------------
class TCallSessionAction : public TSessionAction
{
public:
  __fastcall TCallSessionAction(TActionLog * Log, const UnicodeString & Command,
    const UnicodeString & Destination);

  void __fastcall AddOutput(const UnicodeString & Output, bool StdError);
  void __fastcall ExitCode(int ExitCode);
};
//---------------------------------------------------------------------------
class TLsSessionAction : public TSessionAction
{
public:
  __fastcall TLsSessionAction(TActionLog * Log, const UnicodeString & Destination);

  void __fastcall FileList(TRemoteFileList * FileList);
};
//---------------------------------------------------------------------------
class TStatSessionAction : public TFileSessionAction
{
public:
  __fastcall TStatSessionAction(TActionLog * Log, const UnicodeString & FileName);

  void __fastcall File(TRemoteFile * File);
};
//---------------------------------------------------------------------------
class TChecksumSessionAction : public TFileSessionAction
{
public:
  __fastcall TChecksumSessionAction(TActionLog * Log);

  void __fastcall Checksum(const UnicodeString & Alg, const UnicodeString & Checksum);
};
//---------------------------------------------------------------------------
class TCwdSessionAction : public TSessionAction
{
public:
  __fastcall TCwdSessionAction(TActionLog * Log, const UnicodeString & Path);
};
//---------------------------------------------------------------------------
class TDifferenceSessionAction : public TSessionAction
{
public:
  __fastcall TDifferenceSessionAction(TActionLog * Log, const TSynchronizeChecklist::TItem * Item);
};
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TAddLogEntryEvent)(const UnicodeString & S);
//---------------------------------------------------------------------------
class TSessionLog
{
friend class TApplicationLog;

public:
  __fastcall TSessionLog(TSessionUI* UI, TDateTime Started, TSessionData * SessionData,
    TConfiguration * Configuration);
  __fastcall ~TSessionLog();

  void __fastcall SetParent(TSessionLog * Parent, const UnicodeString & Name);

  void __fastcall Add(TLogLineType Type, const UnicodeString & Line);
  void __fastcall AddSystemInfo();
  void __fastcall AddStartupInfo();
  void __fastcall AddException(Exception * E);
  static UnicodeString GetSeparator();
  void __fastcall AddSeparator();

  void __fastcall ReflectSettings();

  __property bool Logging = { read = FLogging };
  __property UnicodeString Name = { read = FName };

protected:
  void __fastcall CloseLogFile();
  bool __fastcall LogToFile();
  static void __fastcall DoAddStartupInfo(TAddLogEntryEvent AddLogEntry, TConfiguration * AConfiguration, bool DoNotMaskPaswords);

private:
  TConfiguration * FConfiguration;
  TSessionLog * FParent;
  TCriticalSection * FCriticalSection;
  bool FLogging;
  void * FFile;
  UnicodeString FCurrentLogFileName;
  UnicodeString FCurrentFileName;
  __int64 FCurrentFileSize;
  TSessionUI * FUI;
  TSessionData * FSessionData;
  TDateTime FStarted;
  UnicodeString FName;
  bool FClosed;

  void __fastcall OpenLogFile();
  UnicodeString __fastcall GetLogFileName();
  void __fastcall DoAdd(TLogLineType Type, UnicodeString Line,
    void __fastcall (__closure *f)(TLogLineType Type, const UnicodeString & Line));
  void __fastcall DoAddToParent(TLogLineType aType, const UnicodeString & aLine);
  void __fastcall DoAddToSelf(TLogLineType aType, const UnicodeString & aLine);
  void __fastcall AddStartupInfo(bool System);
  void __fastcall DoAddStartupInfo(TSessionData * Data);
  UnicodeString __fastcall LogSensitive(const UnicodeString & Str);
  static UnicodeString __fastcall GetCmdLineLog(TConfiguration * AConfiguration);
  void __fastcall CheckSize(__int64 Addition);
  UnicodeString __fastcall LogPartFileName(const UnicodeString & BaseName, int Index);
  void __fastcall DoAddStartupInfoEntry(const UnicodeString & S);
};
//---------------------------------------------------------------------------
class TActionLog
{
friend class TSessionAction;
friend class TSessionActionRecord;
public:
  __fastcall TActionLog(TSessionUI* UI, TDateTime Started, TSessionData * SessionData,
    TConfiguration * Configuration);
  // For fatal failures for .NET assembly
  __fastcall TActionLog(TDateTime Started, TConfiguration * Configuration);
  __fastcall ~TActionLog();

  void __fastcall ReflectSettings();
  void __fastcall AddFailure(Exception * E);
  void __fastcall AddFailure(TStrings * Messages);
  void __fastcall BeginGroup(UnicodeString Name);
  void __fastcall EndGroup();

  __property UnicodeString CurrentFileName = { read = FCurrentFileName };
  __property bool Enabled = { read = FEnabled, write = SetEnabled };

protected:
  void __fastcall CloseLogFile();
  inline void __fastcall AddPendingAction(TSessionActionRecord * Action);
  void __fastcall RecordPendingActions();
  void __fastcall Add(const UnicodeString & Line);
  void __fastcall AddIndented(const UnicodeString & Line);
  void __fastcall AddMessages(UnicodeString Indent, TStrings * Messages);
  void __fastcall Init(TSessionUI * UI, TDateTime Started, TSessionData * SessionData,
    TConfiguration * Configuration);

private:
  TConfiguration * FConfiguration;
  TCriticalSection * FCriticalSection;
  bool FLogging;
  void * FFile;
  UnicodeString FCurrentLogFileName;
  UnicodeString FCurrentFileName;
  TSessionUI * FUI;
  TSessionData * FSessionData;
  TDateTime FStarted;
  TList * FPendingActions;
  bool FFailed;
  bool FClosed;
  bool FInGroup;
  UnicodeString FIndent;
  bool FEnabled;

  void __fastcall OpenLogFile();
  UnicodeString __fastcall GetLogFileName();
  void __fastcall SetEnabled(bool value);
};
//---------------------------------------------------------------------------
class TApplicationLog
{
public:
  TApplicationLog();
  ~TApplicationLog();
  void Enable(const UnicodeString & Path);
  void AddStartupInfo();
  void __fastcall Log(const UnicodeString & S);
  __property bool Logging = { read = FLogging };
  __property UnicodeString Path = { read = FPath };

private:
  UnicodeString FPath;
  void * FFile;
  bool FLogging;
  TDateTime FLastMemoryCheck;
  size_t FPeekReservedMemory;
  size_t FPeekCommittedMemory;
  std::unique_ptr<TCriticalSection> FCriticalSection;
};
//---------------------------------------------------------------------------
UnicodeString __fastcall XmlEscape(UnicodeString Str);
//---------------------------------------------------------------------------
#endif
