//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <stdio.h>
#include <lmcons.h>
#define SECURITY_WIN32
#include <sspi.h>
#include <secext.h>

#include "Common.h"
#include "SessionInfo.h"
#include "Exceptions.h"
#include "TextsCore.h"
#include "CoreMain.h"
#include "Script.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
UnicodeString __fastcall DoXmlEscape(UnicodeString Str, bool NewLine)
{
  for (int i = 1; i <= Str.Length(); i++)
  {
    const wchar_t * Repl = NULL;
    switch (Str[i])
    {
      case L'&':
        Repl = L"amp;";
        break;

      case L'>':
        Repl = L"gt;";
        break;

      case L'<':
        Repl = L"lt;";
        break;

      case L'"':
        Repl = L"quot;";
        break;

      case L'\n':
        if (NewLine)
        {
          Repl = L"#10;";
        }
        break;

      case L'\r':
        Str.Delete(i, 1);
        i--;
        break;
    }

    if (Repl != NULL)
    {
      Str[i] = L'&';
      Str.Insert(Repl, i + 1);
      i += wcslen(Repl);
    }
  }
  return Str;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall XmlEscape(UnicodeString Str)
{
  return DoXmlEscape(Str, false);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall XmlAttributeEscape(UnicodeString Str)
{
  return DoXmlEscape(Str, true);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#pragma warn -inl
class TSessionActionRecord
{
public:
  __fastcall TSessionActionRecord(TActionLog * Log, TLogAction Action) :
    FLog(Log),
    FAction(Action),
    FState(Opened),
    FRecursive(false),
    FErrorMessages(NULL),
    FNames(new TStringList()),
    FValues(new TStringList()),
    FFileList(NULL),
    FFile(NULL)
  {
    FLog->AddPendingAction(this);
  }

  __fastcall ~TSessionActionRecord()
  {
    delete FErrorMessages;
    delete FNames;
    delete FValues;
    delete FFileList;
    delete FFile;
  }

  void __fastcall Restart()
  {
    FState = Opened;
    FRecursive = false;
    delete FErrorMessages;
    FErrorMessages = NULL;
    delete FFileList;
    FFileList = NULL;
    delete FFile;
    FFile = NULL;
    FNames->Clear();
    FValues->Clear();
  }

  bool __fastcall Record()
  {
    bool Result = (FState != Opened);
    if (Result)
    {
      if (FLog->FLogging && (FState != Cancelled))
      {
        const wchar_t * Name = ActionName();
        UnicodeString Attrs;
        if (FRecursive)
        {
          Attrs = L" recursive=\"true\"";
        }
        FLog->AddIndented(FORMAT(L"<%s%s>", (Name,  Attrs)));
        for (int Index = 0; Index < FNames->Count; Index++)
        {
          UnicodeString Value = FValues->Strings[Index];
          if (Value.IsEmpty())
          {
            FLog->AddIndented(FORMAT(L"  <%s />", (FNames->Strings[Index])));
          }
          else
          {
            FLog->AddIndented(FORMAT(L"  <%s value=\"%s\" />",
              (FNames->Strings[Index], XmlAttributeEscape(Value))));
          }
        }
        if (FFileList != NULL)
        {
          FLog->AddIndented(L"  <files>");
          for (int Index = 0; Index < FFileList->Count; Index++)
          {
            TRemoteFile * File = FFileList->Files[Index];

            FLog->AddIndented(L"    <file>");
            FLog->AddIndented(FORMAT(L"      <filename value=\"%s\" />", (XmlAttributeEscape(File->FileName))));
            FLog->AddIndented(FORMAT(L"      <type value=\"%s\" />", (XmlAttributeEscape(File->Type))));
            if (!File->IsDirectory)
            {
              FLog->AddIndented(FORMAT(L"      <size value=\"%s\" />", (IntToStr(File->Size))));
            }
            FLog->AddIndented(FORMAT(L"      <modification value=\"%s\" />", (StandardTimestamp(File->Modification))));
            FLog->AddIndented(FORMAT(L"      <permissions value=\"%s\" />", (XmlAttributeEscape(File->Rights->Text))));
            if (File->Owner.IsSet)
            {
              FLog->AddIndented(FORMAT(L"      <owner value=\"%s\" />", (XmlAttributeEscape(File->Owner.DisplayText))));
            }
            if (File->Group.IsSet)
            {
              FLog->AddIndented(FORMAT(L"      <group value=\"%s\" />", (XmlAttributeEscape(File->Group.DisplayText))));
            }
            FLog->AddIndented(L"    </file>");
          }
          FLog->AddIndented(L"  </files>");
        }
        if (FFile != NULL)
        {
          FLog->AddIndented(L"  <file>");
          FLog->AddIndented(FORMAT(L"    <type value=\"%s\" />", (XmlAttributeEscape(FFile->Type))));
          if (!FFile->IsDirectory)
          {
            FLog->AddIndented(FORMAT(L"    <size value=\"%s\" />", (IntToStr(FFile->Size))));
          }
          FLog->AddIndented(FORMAT(L"    <modification value=\"%s\" />", (StandardTimestamp(FFile->Modification))));
          FLog->AddIndented(FORMAT(L"    <permissions value=\"%s\" />", (XmlAttributeEscape(FFile->Rights->Text))));
          FLog->AddIndented(L"  </file>");
        }
        if (FState == RolledBack)
        {
          if (FErrorMessages != NULL)
          {
            FLog->AddIndented(L"  <result success=\"false\">");
            FLog->AddMessages(L"    ", FErrorMessages);
            FLog->AddIndented(L"  </result>");
          }
          else
          {
            FLog->AddIndented(L"  <result success=\"false\" />");
          }
        }
        else
        {
          FLog->AddIndented(L"  <result success=\"true\" />");
        }
        FLog->AddIndented(FORMAT(L"</%s>", (Name)));
      }
      delete this;
    }
    return Result;
  }

  void __fastcall Commit()
  {
    Close(Committed);
  }

  void __fastcall Rollback(Exception * E)
  {
    DebugAssert(FErrorMessages == NULL);
    FErrorMessages = ExceptionToMoreMessages(E);
    Close(RolledBack);
  }

  void __fastcall Cancel()
  {
    Close(Cancelled);
  }

  void __fastcall FileName(const UnicodeString & FileName)
  {
    Parameter(L"filename", FileName);
  }

  void __fastcall Destination(const UnicodeString & Destination)
  {
    Parameter(L"destination", Destination);
  }

  void __fastcall Rights(const TRights & Rights)
  {
    Parameter(L"permissions", Rights.Text);
  }

  void __fastcall Modification(const TDateTime & DateTime)
  {
    Parameter(L"modification", StandardTimestamp(DateTime));
  }

  void __fastcall Recursive()
  {
    FRecursive = true;
  }

  void __fastcall Command(const UnicodeString & Command)
  {
    Parameter(L"command", Command);
  }

  void __fastcall AddOutput(UnicodeString Output, bool StdError)
  {
    const wchar_t * Name = (StdError ? L"erroroutput" : L"output");
    int Index = FNames->IndexOf(Name);
    if (Index >= 0)
    {
      FValues->Strings[Index] = FValues->Strings[Index] + L"\r\n" + Output;
    }
    else
    {
      Parameter(Name, Output);
    }
  }

  void __fastcall ExitCode(int ExitCode)
  {
    Parameter(L"exitcode", IntToStr(ExitCode));
  }

  void __fastcall Checksum(const UnicodeString & Alg, const UnicodeString & Checksum)
  {
    Parameter(L"algorithm", Alg);
    Parameter(L"checksum", Checksum);
  }

  void __fastcall Cwd(const UnicodeString & Path)
  {
    Parameter(L"cwd", Path);
  }

  void __fastcall FileList(TRemoteFileList * FileList)
  {
    if (FFileList == NULL)
    {
      FFileList = new TRemoteFileList();
    }
    FileList->DuplicateTo(FFileList);
  }

  void __fastcall File(TRemoteFile * File)
  {
    if (FFile != NULL)
    {
      delete FFile;
    }
    FFile = File->Duplicate(true);
  }

protected:
  enum TState { Opened, Committed, RolledBack, Cancelled };

  inline void __fastcall Close(TState State)
  {
    DebugAssert(FState == Opened);
    FState = State;
    FLog->RecordPendingActions();
  }

  const wchar_t * __fastcall ActionName()
  {
    switch (FAction)
    {
      case laUpload: return L"upload";
      case laDownload: return L"download";
      case laTouch: return L"touch";
      case laChmod: return L"chmod";
      case laMkdir: return L"mkdir";
      case laRm: return L"rm";
      case laMv: return L"mv";
      case laCall: return L"call";
      case laLs: return L"ls";
      case laStat: return L"stat";
      case laChecksum: return L"checksum";
      case laCwd: return L"cwd";
      default: DebugFail(); return L"";
    }
  }

  void __fastcall Parameter(const UnicodeString & Name, const UnicodeString & Value = L"")
  {
    FNames->Add(Name);
    FValues->Add(Value);
  }

private:
  TActionLog * FLog;
  TLogAction FAction;
  TState FState;
  bool FRecursive;
  TStrings * FErrorMessages;
  TStrings * FNames;
  TStrings * FValues;
  TRemoteFileList * FFileList;
  TRemoteFile * FFile;
};
#pragma warn .inl
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TSessionAction::TSessionAction(TActionLog * Log, TLogAction Action)
{
  if (Log->FLogging)
  {
    FRecord = new TSessionActionRecord(Log, Action);
  }
  else
  {
    FRecord = NULL;
  }
}
//---------------------------------------------------------------------------
__fastcall TSessionAction::~TSessionAction()
{
  if (FRecord != NULL)
  {
    Commit();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionAction::Restart()
{
  if (FRecord != NULL)
  {
    FRecord->Restart();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionAction::Commit()
{
  if (FRecord != NULL)
  {
    TSessionActionRecord * Record = FRecord;
    FRecord = NULL;
    Record->Commit();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionAction::Rollback(Exception * E)
{
  if (FRecord != NULL)
  {
    TSessionActionRecord * Record = FRecord;
    FRecord = NULL;
    Record->Rollback(E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionAction::Cancel()
{
  if (FRecord != NULL)
  {
    TSessionActionRecord * Record = FRecord;
    FRecord = NULL;
    Record->Cancel();
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TFileSessionAction::TFileSessionAction(TActionLog * Log, TLogAction Action) :
  TSessionAction(Log, Action)
{
}
//---------------------------------------------------------------------------
__fastcall TFileSessionAction::TFileSessionAction(
    TActionLog * Log, TLogAction Action, const UnicodeString & AFileName) :
  TSessionAction(Log, Action)
{
  FileName(AFileName);
}
//---------------------------------------------------------------------------
void __fastcall TFileSessionAction::FileName(const UnicodeString & FileName)
{
  if (FRecord != NULL)
  {
    FRecord->FileName(FileName);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TFileLocationSessionAction::TFileLocationSessionAction(
    TActionLog * Log, TLogAction Action) :
  TFileSessionAction(Log, Action)
{
}
//---------------------------------------------------------------------------
__fastcall TFileLocationSessionAction::TFileLocationSessionAction(
    TActionLog * Log, TLogAction Action, const UnicodeString & FileName) :
  TFileSessionAction(Log, Action, FileName)
{
}
//---------------------------------------------------------------------------
void __fastcall TFileLocationSessionAction::Destination(const UnicodeString & Destination)
{
  if (FRecord != NULL)
  {
    FRecord->Destination(Destination);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TUploadSessionAction::TUploadSessionAction(TActionLog * Log) :
  TFileLocationSessionAction(Log, laUpload)
{
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TDownloadSessionAction::TDownloadSessionAction(TActionLog * Log) :
  TFileLocationSessionAction(Log, laDownload)
{
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TChmodSessionAction::TChmodSessionAction(
    TActionLog * Log, const UnicodeString & FileName) :
  TFileSessionAction(Log, laChmod, FileName)
{
}
//---------------------------------------------------------------------------
void __fastcall TChmodSessionAction::Recursive()
{
  if (FRecord != NULL)
  {
    FRecord->Recursive();
  }
}
//---------------------------------------------------------------------------
__fastcall TChmodSessionAction::TChmodSessionAction(
    TActionLog * Log, const UnicodeString & FileName, const TRights & ARights) :
  TFileSessionAction(Log, laChmod, FileName)
{
  Rights(ARights);
}
//---------------------------------------------------------------------------
void __fastcall TChmodSessionAction::Rights(const TRights & Rights)
{
  if (FRecord != NULL)
  {
    FRecord->Rights(Rights);
  }
}
//---------------------------------------------------------------------------
__fastcall TTouchSessionAction::TTouchSessionAction(
    TActionLog * Log, const UnicodeString & FileName, const TDateTime & Modification) :
  TFileSessionAction(Log, laTouch, FileName)
{
  if (FRecord != NULL)
  {
    FRecord->Modification(Modification);
  }
}
//---------------------------------------------------------------------------
__fastcall TMkdirSessionAction::TMkdirSessionAction(
    TActionLog * Log, const UnicodeString & FileName) :
  TFileSessionAction(Log, laMkdir, FileName)
{
}
//---------------------------------------------------------------------------
__fastcall TRmSessionAction::TRmSessionAction(
    TActionLog * Log, const UnicodeString & FileName) :
  TFileSessionAction(Log, laRm, FileName)
{
}
//---------------------------------------------------------------------------
void __fastcall TRmSessionAction::Recursive()
{
  if (FRecord != NULL)
  {
    FRecord->Recursive();
  }
}
//---------------------------------------------------------------------------
__fastcall TMvSessionAction::TMvSessionAction(TActionLog * Log,
    const UnicodeString & FileName, const UnicodeString & ADestination) :
  TFileLocationSessionAction(Log, laMv, FileName)
{
  Destination(ADestination);
}
//---------------------------------------------------------------------------
__fastcall TCallSessionAction::TCallSessionAction(TActionLog * Log,
    const UnicodeString & Command, const UnicodeString & Destination) :
  TSessionAction(Log, laCall)
{
  if (FRecord != NULL)
  {
    FRecord->Command(Command);
    FRecord->Destination(Destination);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCallSessionAction::AddOutput(const UnicodeString & Output, bool StdError)
{
  if (FRecord != NULL)
  {
    FRecord->AddOutput(Output, StdError);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCallSessionAction::ExitCode(int ExitCode)
{
  if (FRecord != NULL)
  {
    FRecord->ExitCode(ExitCode);
  }
}
//---------------------------------------------------------------------------
__fastcall TLsSessionAction::TLsSessionAction(TActionLog * Log,
    const UnicodeString & Destination) :
  TSessionAction(Log, laLs)
{
  if (FRecord != NULL)
  {
    FRecord->Destination(Destination);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLsSessionAction::FileList(TRemoteFileList * FileList)
{
  if (FRecord != NULL)
  {
    FRecord->FileList(FileList);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TStatSessionAction::TStatSessionAction(TActionLog * Log, const UnicodeString & FileName) :
  TFileSessionAction(Log, laStat, FileName)
{
}
//---------------------------------------------------------------------------
void __fastcall TStatSessionAction::File(TRemoteFile * File)
{
  if (FRecord != NULL)
  {
    FRecord->File(File);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TChecksumSessionAction::TChecksumSessionAction(TActionLog * Log) :
  TFileSessionAction(Log, laChecksum)
{
}
//---------------------------------------------------------------------------
void __fastcall TChecksumSessionAction::Checksum(const UnicodeString & Alg, const UnicodeString & Checksum)
{
  if (FRecord != NULL)
  {
    FRecord->Checksum(Alg, Checksum);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TCwdSessionAction::TCwdSessionAction(TActionLog * Log, const UnicodeString & Path) :
  TSessionAction(Log, laCwd)
{
  if (FRecord != NULL)
  {
    FRecord->Cwd(Path);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TSessionInfo::TSessionInfo()
{
  LoginTime = Now();
}
//---------------------------------------------------------------------------
TFileSystemInfo::TFileSystemInfo()
{
  memset(&IsCapable, false, sizeof(IsCapable));
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
FILE * __fastcall OpenFile(UnicodeString LogFileName, TSessionData * SessionData, bool Append, UnicodeString & NewFileName)
{
  FILE * Result;
  UnicodeString ANewFileName = GetExpandedLogFileName(LogFileName, SessionData);
  Result = _wfopen(ApiPath(ANewFileName).c_str(), (Append ? L"a" : L"w"));
  if (Result != NULL)
  {
    setvbuf(Result, NULL, _IONBF, BUFSIZ);
    NewFileName = ANewFileName;
  }
  else
  {
    throw ECRTExtException(FMTLOAD(LOG_OPENERROR, (ANewFileName)));
  }
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
const wchar_t *LogLineMarks = L"<>!.*";
__fastcall TSessionLog::TSessionLog(TSessionUI* UI, TSessionData * SessionData,
  TConfiguration * Configuration):
  TStringList()
{
  FCriticalSection = new TCriticalSection;
  FLogging = false;
  FConfiguration = Configuration;
  FParent = NULL;
  FUI = UI;
  FSessionData = SessionData;
  FFile = NULL;
  FLoggedLines = 0;
  FTopIndex = -1;
  FCurrentLogFileName = L"";
  FCurrentFileName = L"";
  FClosed = false;
}
//---------------------------------------------------------------------------
__fastcall TSessionLog::~TSessionLog()
{
  FClosed = true;
  ReflectSettings();
  DebugAssert(FFile == NULL);
  delete FCriticalSection;
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::Lock()
{
  FCriticalSection->Enter();
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::Unlock()
{
  FCriticalSection->Leave();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSessionLog::GetSessionName()
{
  DebugAssert(FSessionData != NULL);
  return FSessionData->SessionName;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSessionLog::GetLine(Integer Index)
{
  return Strings[Index - FTopIndex];
}
//---------------------------------------------------------------------------
TLogLineType __fastcall TSessionLog::GetType(int Index)
{
  return (TLogLineType)Objects[Index - FTopIndex];
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAddToParent(TLogLineType Type, const UnicodeString & Line)
{
  DebugAssert(FParent != NULL);
  FParent->Add(Type, Line);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAddToSelf(TLogLineType Type, const UnicodeString & Line)
{
  if (FTopIndex < 0)
  {
    FTopIndex = 0;
  }

  TStringList::AddObject(Line, (TObject*)Type);

  FLoggedLines++;

  if (LogToFile())
  {
    if (FFile == NULL)
    {
      OpenLogFile();
    }

    if (FFile != NULL)
    {
      UnicodeString Timestamp = FormatDateTime(L" yyyy-mm-dd hh:nn:ss.zzz ", Now());
      UTF8String UtfLine = UTF8String(UnicodeString(LogLineMarks[Type]) + Timestamp + Line + "\n");
      fwrite(UtfLine.c_str(), UtfLine.Length(), 1, (FILE *)FFile);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAdd(TLogLineType Type, UnicodeString Line,
  void __fastcall (__closure *f)(TLogLineType Type, const UnicodeString & Line))
{
  UnicodeString Prefix;

  if (!Name.IsEmpty())
  {
    Prefix = L"[" + Name + L"] ";
  }

  while (!Line.IsEmpty())
  {
    f(Type, Prefix + CutToChar(Line, L'\n', false));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::Add(TLogLineType Type, const UnicodeString & Line)
{
  DebugAssert(FConfiguration);
  if (Logging)
  {
    try
    {
      if (FParent != NULL)
      {
        DoAdd(Type, Line, DoAddToParent);
      }
      else
      {
        TGuard Guard(FCriticalSection);

        BeginUpdate();

        try
        {
          DoAdd(Type, Line, DoAddToSelf);
        }
        __finally
        {
          DeleteUnnecessary();

          EndUpdate();
        }
      }
    }
    catch (Exception &E)
    {
      // We failed logging, turn it off and notify user.
      FConfiguration->Logging = false;
      try
      {
        throw ExtException(&E, LoadStr(LOG_GEN_ERROR));
      }
      catch (Exception &E)
      {
        AddException(&E);
        FUI->HandleExtendedException(&E);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddException(Exception * E)
{
  if (E != NULL)
  {
    Add(llException, ExceptionLogString(E));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::ReflectSettings()
{
  TGuard Guard(FCriticalSection);

  bool ALogging =
    !FClosed &&
    ((FParent != NULL) || FConfiguration->Logging);

  bool Changed = false;
  if (FLogging != ALogging)
  {
    FLogging = ALogging;
    Changed = true;
  }

  // if logging to file was turned off or log file was changed -> close current log file
  if ((FFile != NULL) &&
      (!LogToFile() || (FCurrentLogFileName != FConfiguration->LogFileName)))
  {
    CloseLogFile();
  }

  DeleteUnnecessary();

  // trigger event only once we are in a consistent state
  if (Changed)
  {
    StateChange();
  }

}
//---------------------------------------------------------------------------
bool __fastcall TSessionLog::LogToFile()
{
  return Logging && FConfiguration->LogToFile && (FParent == NULL);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::CloseLogFile()
{
  if (FFile != NULL)
  {
    fclose((FILE *)FFile);
    FFile = NULL;
  }
  FCurrentLogFileName = L"";
  FCurrentFileName = L"";
  StateChange();
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::OpenLogFile()
{
  try
  {
    DebugAssert(FFile == NULL);
    DebugAssert(FConfiguration != NULL);
    FCurrentLogFileName = FConfiguration->LogFileName;
    FFile = OpenFile(FCurrentLogFileName, FSessionData, FConfiguration->LogFileAppend, FCurrentFileName);
  }
  catch (Exception & E)
  {
    // We failed logging to file, turn it off and notify user.
    FCurrentLogFileName = L"";
    FCurrentFileName = L"";
    FConfiguration->LogFileName = UnicodeString();
    try
    {
      throw ExtException(&E, LoadStr(LOG_GEN_ERROR));
    }
    catch (Exception & E)
    {
      AddException(&E);
      FUI->HandleExtendedException(&E);
    }
  }
  StateChange();
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::StateChange()
{
  if (FOnStateChange != NULL)
  {
    FOnStateChange(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DeleteUnnecessary()
{
  BeginUpdate();
  try
  {
    if (!Logging || (FParent != NULL))
    {
      Clear();
    }
    else
    {
      while (!FConfiguration->LogWindowComplete && (Count > FConfiguration->LogWindowLines))
      {
        Delete(0);
        FTopIndex++;
      }
    }
  }
  __finally
  {
    EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddSystemInfo()
{
  AddStartupInfo(true);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddStartupInfo()
{
  AddStartupInfo(false);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddStartupInfo(bool System)
{
  TSessionData * Data = (System ? NULL : FSessionData);
  if (Logging)
  {
    if (FParent != NULL)
    {
      // do not add session info for secondary session
      // (this should better be handled in the TSecondaryTerminal)
    }
    else
    {
      DoAddStartupInfo(Data);
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSessionLog::GetTlsVersionName(TTlsVersion TlsVersion)
{
  switch (TlsVersion)
  {
    default:
      DebugFail();
    case ssl2:
      return "SSLv2";
    case ssl3:
      return "SSLv3";
    case tls10:
      return "TLSv1.0";
    case tls11:
      return "TLSv1.1";
    case tls12:
      return "TLSv1.2";
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSessionLog::LogSensitive(const UnicodeString & Str)
{
  if (FConfiguration->LogSensitive && !Str.IsEmpty())
  {
    return Str;
  }
  else
  {
    return BooleanToEngStr(!Str.IsEmpty());
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSessionLog::GetCmdLineLog()
{
  UnicodeString Result = CmdLine;

  if (!Configuration->LogSensitive)
  {
    TManagementScript Script(StoredSessions, false);
    Script.MaskPasswordInCommandLine(Result, true);
  }

  return Result;
}
//---------------------------------------------------------------------------
template <typename T>
UnicodeString __fastcall EnumName(T Value, UnicodeString Names)
{
  int N = int(Value);

  do
  {
    UnicodeString Name = CutToChar(Names, L';', true);
    if (N == 0)
    {
      return Name;
    }
    N--;
  }
  while ((N >= 0) && !Names.IsEmpty());

  return L"(unknown)";
}
//---------------------------------------------------------------------------
#define ADSTR(S) DoAdd(llMessage, S, DoAddToSelf);
#define ADF(S, F) ADSTR(FORMAT(S, F));
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAddStartupInfo(TSessionData * Data)
{
  TGuard Guard(FCriticalSection);

  BeginUpdate();
  try
  {
    if (Data == NULL)
    {
      AddSeparator();
      UnicodeString OS = WindowsVersionLong();
      AddToList(OS, WindowsProductName(), L" - ");
      ADF(L"WinSCP %s (OS %s)", (FConfiguration->VersionStr, OS));
      THierarchicalStorage * Storage = FConfiguration->CreateConfigStorage();
      try
      {
        ADF(L"Configuration: %s", (Storage->Source));
      }
      __finally
      {
        delete Storage;
      }

      wchar_t UserName[UNLEN + 1];
      unsigned long UserNameSize = LENOF(UserName);
      if (DebugAlwaysFalse(!GetUserNameEx(NameSamCompatible, UserName, &UserNameSize)))
      {
        wcscpy(UserName, L"<Failed to retrieve username>");
      }
      UnicodeString LogStr;
      if (FConfiguration->LogProtocol <= 0)
      {
        LogStr = L"Normal";
      }
      else if (FConfiguration->LogProtocol == 1)
      {
        LogStr = L"Debug 1";
      }
      else if (FConfiguration->LogProtocol >= 2)
      {
        LogStr = L"Debug 2";
      }
      if (FConfiguration->LogSensitive)
      {
        LogStr += L", Logging passwords";
      }
      ADF(L"Log level: %s", (LogStr));
      ADF(L"Local account: %s", (UserName));
      ADF(L"Working directory: %s", (GetCurrentDir()));
      ADF(L"Process ID: %d", (int(GetCurrentProcessId())));
      ADF(L"Command-line: %s", (GetCmdLineLog()));
      if (FConfiguration->LogProtocol >= 1)
      {
        AddOptions(GetGlobalOptions());
      }
      ADF(L"Time zone: %s", (GetTimeZoneLogString()));
      if (!AdjustClockForDSTEnabled())
      {
        ADSTR(L"Warning: System option \"Automatically adjust clock for Daylight Saving Time\" is disabled, timestamps will not be represented correctly");
      }
      ADF(L"Login time: %s", (FormatDateTime(L"dddddd tt", Now())));
      AddSeparator();
    }
    else
    {
      ADF(L"Session name: %s (%s)", (Data->SessionName, Data->Source));
      ADF(L"Host name: %s (Port: %d)", (Data->HostNameExpanded, Data->PortNumber));
      ADF(L"User name: %s (Password: %s, Key file: %s)",
        (Data->UserNameExpanded, LogSensitive(Data->Password),
         LogSensitive(Data->PublicKeyFile)));
      if (Data->UsesSsh)
      {
        ADF(L"Tunnel: %s", (BooleanToEngStr(Data->Tunnel)));
        if (Data->Tunnel)
        {
          ADF(L"Tunnel: Host name: %s (Port: %d)", (Data->TunnelHostName, Data->TunnelPortNumber));
          ADF(L"Tunnel: User name: %s (Password: %s, Key file: %s)",
            (Data->TunnelUserName,
             LogSensitive(Data->TunnelPassword),
             LogSensitive(Data->TunnelPublicKeyFile)));
          ADF(L"Tunnel: Local port number: %d", (Data->TunnelLocalPortNumber));
        }
      }
      ADF(L"Transfer Protocol: %s", (Data->FSProtocolStr));
      if (Data->UsesSsh || (Data->FSProtocol == fsFTP))
      {
        TPingType PingType;
        int PingInterval;
        if (Data->FSProtocol == fsFTP)
        {
          PingType = Data->FtpPingType;
          PingInterval = Data->FtpPingInterval;
        }
        else
        {
          PingType = Data->PingType;
          PingInterval = Data->PingInterval;
        }
        ADF(L"Ping type: %s, Ping interval: %d sec; Timeout: %d sec",
          (EnumName(PingType, PingTypeNames), PingInterval, Data->Timeout));
        ADF(L"Disable Nagle: %s",
          (BooleanToEngStr(Data->TcpNoDelay)));
      }
      ADF(L"Proxy: %s",
        ((Data->FtpProxyLogonType != 0) ?
          FORMAT(L"FTP proxy %d", (Data->FtpProxyLogonType)) :
          EnumName(Data->ProxyMethod, ProxyMethodNames)));
      if ((Data->FtpProxyLogonType != 0) || (Data->ProxyMethod != ::pmNone))
      {
        ADF(L"HostName: %s (Port: %d); Username: %s; Passwd: %s",
          (Data->ProxyHost, Data->ProxyPort,
           Data->ProxyUsername, LogSensitive(Data->ProxyPassword)));
        if (Data->ProxyMethod == pmTelnet)
        {
          ADF(L"Telnet command: %s", (Data->ProxyTelnetCommand));
        }
        if (Data->ProxyMethod == pmCmd)
        {
          ADF(L"Local command: %s", (Data->ProxyLocalCommand));
        }
      }
      if (Data->UsesSsh || (Data->FSProtocol == fsFTP))
      {
        ADF(L"Send buffer: %d", (Data->SendBuf));
      }
      if (Data->UsesSsh)
      {
        ADF(L"SSH protocol version: %s; Compression: %s",
          (Data->SshProtStr, BooleanToEngStr(Data->Compression)));
        ADF(L"Bypass authentication: %s",
         (BooleanToEngStr(Data->SshNoUserAuth)));
        ADF(L"Try agent: %s; Agent forwarding: %s; TIS/CryptoCard: %s; KI: %s; GSSAPI: %s",
          (BooleanToEngStr(Data->TryAgent), BooleanToEngStr(Data->AgentFwd), BooleanToEngStr(Data->AuthTIS),
           BooleanToEngStr(Data->AuthKI), BooleanToEngStr(Data->AuthGSSAPI)));
        if (Data->AuthGSSAPI)
        {
          ADF(L"GSSAPI: Forwarding: %s",
            (BooleanToEngStr(Data->GSSAPIFwdTGT)));
        }
        ADF(L"Ciphers: %s; Ssh2DES: %s",
          (Data->CipherList, BooleanToEngStr(Data->Ssh2DES)));
        ADF(L"KEX: %s", (Data->KexList));
        UnicodeString Bugs;
        for (int Index = 0; Index < BUG_COUNT; Index++)
        {
          AddToList(Bugs, EnumName(Data->Bug[(TSshBug)Index], AutoSwitchNames), L",");
        }
        ADF(L"SSH Bugs: %s", (Bugs));
        ADF(L"Simple channel: %s", (BooleanToEngStr(Data->SshSimple)));
        ADF(L"Return code variable: %s; Lookup user groups: %s",
          ((Data->DetectReturnVar ? UnicodeString(L"Autodetect") : Data->ReturnVar),
           EnumName(Data->LookupUserGroups, AutoSwitchNames)));
        ADF(L"Shell: %s", ((Data->Shell.IsEmpty()? UnicodeString(L"default") : Data->Shell)));
        ADF(L"EOL: %s, UTF: %s", (EnumName(Data->EOLType, EOLTypeNames), EnumName(Data->NotUtf, NotAutoSwitchNames))); // NotUtf duplicated in FTP branch
        ADF(L"Clear aliases: %s, Unset nat.vars: %s, Resolve symlinks: %s; Follow directory symlinks: %s",
          (BooleanToEngStr(Data->ClearAliases), BooleanToEngStr(Data->UnsetNationalVars),
           BooleanToEngStr(Data->ResolveSymlinks), BooleanToEngStr(Data->FollowDirectorySymlinks)));
        ADF(L"LS: %s, Ign LS warn: %s, Scp1 Comp: %s",
          (Data->ListingCommand,
           BooleanToEngStr(Data->IgnoreLsWarnings),
           BooleanToEngStr(Data->Scp1Compatibility)));
      }
      if ((Data->FSProtocol == fsSFTP) || (Data->FSProtocol == fsSFTPonly))
      {
        UnicodeString Bugs;
        for (int Index = 0; Index < SFTP_BUG_COUNT; Index++)
        {
          AddToList(Bugs, EnumName(Data->SFTPBug[(TSftpBug)Index], AutoSwitchNames), L",");
        }
        ADF(L"SFTP Bugs: %s", (Bugs));
        ADF(L"SFTP Server: %s", ((Data->SftpServer.IsEmpty()? UnicodeString(L"default") : Data->SftpServer)));
      }
      bool FtpsOn = false;
      if (Data->FSProtocol == fsFTP)
      {
        ADF(L"UTF: %s", (EnumName(Data->NotUtf, NotAutoSwitchNames))); // duplicated in UsesSsh branch
        UnicodeString Ftps;
        switch (Data->Ftps)
        {
          case ftpsImplicit:
            Ftps = L"Implicit TLS/SSL";
            FtpsOn = true;
            break;

          case ftpsExplicitSsl:
            Ftps = L"Explicit SSL/TLS";
            FtpsOn = true;
            break;

          case ftpsExplicitTls:
            Ftps = L"Explicit TLS/SSL";
            FtpsOn = true;
            break;

          default:
            DebugAssert(Data->Ftps == ftpsNone);
            Ftps = L"None";
            break;
        }
        // kind of hidden option, so do not reveal it unless it is set
        if (Data->FtpTransferActiveImmediately != asAuto)
        {
          ADF(L"Transfer active immediately: %s", (EnumName(Data->FtpTransferActiveImmediately, AutoSwitchNames)));
        }
        ADF(L"FTPS: %s [Client certificate: %s]",
          (Ftps, LogSensitive(Data->TlsCertificateFile)));
        ADF(L"FTP: Passive: %s [Force IP: %s]; MLSD: %s [List all: %s]; HOST: %s",
          (BooleanToEngStr(Data->FtpPasvMode),
           EnumName(Data->FtpForcePasvIp, AutoSwitchNames),
           EnumName(Data->FtpUseMlsd, AutoSwitchNames),
           EnumName(Data->FtpListAll, AutoSwitchNames),
           EnumName(Data->FtpHost, AutoSwitchNames)));
      }
      if (Data->FSProtocol == fsWebDAV)
      {
        FtpsOn = (Data->Ftps != ftpsNone);
        ADF(L"HTTPS: %s [Client certificate: %s]",
          (BooleanToEngStr(FtpsOn), LogSensitive(Data->TlsCertificateFile)));
      }
      if (FtpsOn)
      {
        if (Data->FSProtocol == fsFTP)
        {
          ADF(L"Session reuse: %s", (BooleanToEngStr(Data->SslSessionReuse)));
        }
        ADF(L"TLS/SSL versions: %s-%s", (GetTlsVersionName(Data->MinTlsVersion), GetTlsVersionName(Data->MaxTlsVersion)));
      }
      ADF(L"Local directory: %s, Remote directory: %s, Update: %s, Cache: %s",
        ((Data->LocalDirectory.IsEmpty() ? UnicodeString(L"default") : Data->LocalDirectory),
         (Data->RemoteDirectory.IsEmpty() ? UnicodeString(L"home") : Data->RemoteDirectory),
         BooleanToEngStr(Data->UpdateDirectories),
         BooleanToEngStr(Data->CacheDirectories)));
      ADF(L"Cache directory changes: %s, Permanent: %s",
        (BooleanToEngStr(Data->CacheDirectoryChanges),
         BooleanToEngStr(Data->PreserveDirectoryChanges)));
      ADF(L"Recycle bin: Delete to: %s, Overwritten to: %s, Bin path: %s",
        (BooleanToEngStr(Data->DeleteToRecycleBin),
         BooleanToEngStr(Data->OverwrittenToRecycleBin),
         Data->RecycleBinPath));
      if (Data->TrimVMSVersions)
      {
        ADF(L"Trim VMS versions: %s",
          (BooleanToEngStr(Data->TrimVMSVersions)));
      }
      UnicodeString TimeInfo;
      if ((Data->FSProtocol == fsSFTP) || (Data->FSProtocol == fsSFTPonly) || (Data->FSProtocol == fsSCPonly) || (Data->FSProtocol == fsWebDAV))
      {
        AddToList(TimeInfo, FORMAT(L"DST mode: %s", (EnumName(Data->DSTMode, DSTModeNames))), L";");
      }
      if ((Data->FSProtocol == fsSCPonly) || (Data->FSProtocol == fsFTP))
      {
        int TimeDifferenceMin = TimeToMinutes(Data->TimeDifference);
        AddToList(TimeInfo, FORMAT(L"Timezone offset: %dh %dm", ((TimeDifferenceMin / MinsPerHour), (TimeDifferenceMin % MinsPerHour))), L";");
      }
      ADSTR(TimeInfo);

      if (Data->FSProtocol == fsWebDAV)
      {
        ADF(L"Compression: %s",
          (BooleanToEngStr(Data->Compression)));
      }

      AddSeparator();
    }
  }
  __finally
  {
    DeleteUnnecessary();

    EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddOption(const UnicodeString & LogStr)
{
  ADSTR(LogStr);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddOptions(TOptions * Options)
{
  Options->LogOptions(AddOption);
}
//---------------------------------------------------------------------------
#undef ADF
#undef ADSTR
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddSeparator()
{
  Add(llMessage, L"--------------------------------------------------------------------------");
}
//---------------------------------------------------------------------------
int __fastcall TSessionLog::GetBottomIndex()
{
  return (Count > 0 ? (TopIndex + Count - 1) : -1);
}
//---------------------------------------------------------------------------
bool __fastcall TSessionLog::GetLoggingToFile()
{
  DebugAssert((FFile == NULL) || LogToFile());
  return (FFile != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::Clear()
{
  TGuard Guard(FCriticalSection);

  FTopIndex += Count;
  TStringList::Clear();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TActionLog::TActionLog(TSessionUI * UI, TSessionData * SessionData,
  TConfiguration * Configuration)
{
  DebugAssert(UI != NULL);
  DebugAssert(SessionData != NULL);
  Init(UI, SessionData, Configuration);
}
//---------------------------------------------------------------------------
__fastcall TActionLog::TActionLog(TConfiguration * Configuration)
{
  Init(NULL, NULL, Configuration);
  // not associated with session, so no need to waiting for anything
  ReflectSettings();
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::Init(TSessionUI * UI, TSessionData * SessionData,
  TConfiguration * Configuration)
{
  FCriticalSection = new TCriticalSection;
  FConfiguration = Configuration;
  FUI = UI;
  FSessionData = SessionData;
  FFile = NULL;
  FCurrentLogFileName = L"";
  FCurrentFileName = L"";
  FLogging = false;
  FClosed = false;
  FFailed = false;
  FPendingActions = new TList();
  FIndent = L"  ";
  FInGroup = false;
  FEnabled = true;
}
//---------------------------------------------------------------------------
__fastcall TActionLog::~TActionLog()
{
  DebugAssert(FPendingActions->Count == 0);
  delete FPendingActions;
  FClosed = true;
  ReflectSettings();
  DebugAssert(FFile == NULL);
  delete FCriticalSection;
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::Add(const UnicodeString & Line)
{
  DebugAssert(FConfiguration);
  if (FLogging)
  {
    TGuard Guard(FCriticalSection);
    if (FFile == NULL)
    {
      OpenLogFile();
    }

    if (FFile != NULL)
    {
      try
      {
        UTF8String UtfLine = UTF8String(Line);
        size_t Written =
          fwrite(UtfLine.c_str(), 1, UtfLine.Length(), (FILE *)FFile);
        if (Written != static_cast<size_t>(UtfLine.Length()))
        {
          throw ECRTExtException(L"");
        }
        #ifdef _DEBUG
        #endif
        Written =
          fwrite("\n", 1, 1, (FILE *)FFile);
        if (Written != 1)
        {
          throw ECRTExtException(L"");
        }
      }
      catch (Exception &E)
      {
        FCriticalSection->Release();

        // avoid endless loop when trying to close tags when closing log, when logging has failed
        if (!FFailed)
        {
          FFailed = true;
          // We failed logging, turn it off and notify user.
          FConfiguration->LogActions = false;
          if (FConfiguration->LogActionsRequired)
          {
            throw EFatal(&E, LoadStr(LOG_FATAL_ERROR));
          }
          else
          {
            try
            {
              throw ExtException(&E, LoadStr(LOG_GEN_ERROR));
            }
            catch (Exception &E)
            {
              if (FUI != NULL)
              {
                FUI->HandleExtendedException(&E);
              }
            }
          }
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::AddIndented(const UnicodeString & Line)
{
  Add(FIndent + Line);
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::AddFailure(TStrings * Messages)
{
  AddIndented(L"<failure>");
  AddMessages(L"  ", Messages);
  AddIndented(L"</failure>");
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::AddFailure(Exception * E)
{
  TStrings * Messages = ExceptionToMoreMessages(E);
  if (Messages != NULL)
  {
    try
    {
      AddFailure(Messages);
    }
    __finally
    {
      delete Messages;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::AddMessages(UnicodeString Indent, TStrings * Messages)
{
  for (int Index = 0; Index < Messages->Count; Index++)
  {
    AddIndented(
      FORMAT(Indent + L"<message>%s</message>", (XmlEscape(Messages->Strings[Index]))));
  }
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::ReflectSettings()
{
  TGuard Guard(FCriticalSection);

  bool ALogging =
    !FClosed && FConfiguration->LogActions && Enabled;

  if (ALogging && !FLogging)
  {
    FLogging = true;
    Add(L"<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
    UnicodeString SessionName =
      (FSessionData != NULL) ? XmlAttributeEscape(FSessionData->SessionName) : UnicodeString(L"nosession");
    Add(FORMAT(L"<session xmlns=\"http://winscp.net/schema/session/1.0\" name=\"%s\" start=\"%s\">",
      (SessionName, StandardTimestamp())));
  }
  else if (!ALogging && FLogging)
  {
    if (FInGroup)
    {
      EndGroup();
    }
    // do not try to close the file, if it has not been opened, to avoid recursion
    if (FFile != NULL)
    {
      Add(L"</session>");
    }
    CloseLogFile();
    FLogging = false;
  }

}
//---------------------------------------------------------------------------
void __fastcall TActionLog::CloseLogFile()
{
  if (FFile != NULL)
  {
    fclose((FILE *)FFile);
    FFile = NULL;
  }
  FCurrentLogFileName = L"";
  FCurrentFileName = L"";
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::OpenLogFile()
{
  try
  {
    DebugAssert(FFile == NULL);
    DebugAssert(FConfiguration != NULL);
    FCurrentLogFileName = FConfiguration->ActionsLogFileName;
    FFile = OpenFile(FCurrentLogFileName, FSessionData, false, FCurrentFileName);
  }
  catch (Exception & E)
  {
    // We failed logging to file, turn it off and notify user.
    FCurrentLogFileName = L"";
    FCurrentFileName = L"";
    FConfiguration->LogActions = false;
    if (FConfiguration->LogActionsRequired)
    {
      throw EFatal(&E, LoadStr(LOG_FATAL_ERROR));
    }
    else
    {
      try
      {
        throw ExtException(&E, LoadStr(LOG_GEN_ERROR));
      }
      catch (Exception & E)
      {
        if (FUI != NULL)
        {
          FUI->HandleExtendedException(&E);
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::AddPendingAction(TSessionActionRecord * Action)
{
  FPendingActions->Add(Action);
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::RecordPendingActions()
{
  while ((FPendingActions->Count > 0) &&
         static_cast<TSessionActionRecord *>(FPendingActions->Items[0])->Record())
  {
    FPendingActions->Delete(0);
  }
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::BeginGroup(UnicodeString Name)
{
  DebugAssert(!FInGroup);
  FInGroup = true;
  DebugAssert(FIndent == L"  ");
  AddIndented(FORMAT(L"<group name=\"%s\" start=\"%s\">",
    (XmlAttributeEscape(Name), StandardTimestamp())));
  FIndent = L"    ";
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::EndGroup()
{
  DebugAssert(FInGroup);
  FInGroup = false;
  DebugAssert(FIndent == L"    ");
  FIndent = L"  ";
  // this is called from ReflectSettings that in turn is called when logging fails,
  // so do not try to close the group, if it has not been opened, to avoid recursion
  if (FFile != NULL)
  {
    AddIndented(L"</group>");
  }
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::SetEnabled(bool value)
{
  if (Enabled != value)
  {
    FEnabled = value;
    ReflectSettings();
  }
}
