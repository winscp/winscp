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
TStrings * __fastcall ExceptionToMessages(Exception * E)
{
  TStrings * Result = NULL;
  UnicodeString Message;
  if (ExceptionMessage(E, Message))
  {
    Result = new TStringList();
    Result->Add(Message);
    ExtException * EE = dynamic_cast<ExtException *>(E);
    if ((EE != NULL) && (EE->MoreMessages != NULL))
    {
      Result->AddStrings(EE->MoreMessages);
    }
  }
  return Result;
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
    assert(FErrorMessages == NULL);
    FErrorMessages = ExceptionToMessages(E);
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
    assert(FState == Opened);
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
      default: assert(false); return L"";
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
  UnicodeString ANewFileName = StripPathQuotes(ExpandEnvironmentVariables(LogFileName));
  TDateTime N = Now();
  for (int Index = 1; Index < ANewFileName.Length(); Index++)
  {
    if (ANewFileName[Index] == L'!')
    {
      UnicodeString Replacement;
      // keep consistent with TFileCustomCommand::PatternReplacement
      switch (tolower(ANewFileName[Index + 1]))
      {
        case L'y':
          Replacement = FormatDateTime(L"yyyy", N);
          break;

        case L'm':
          Replacement = FormatDateTime(L"mm", N);
          break;

        case L'd':
          Replacement = FormatDateTime(L"dd", N);
          break;

        case L't':
          Replacement = FormatDateTime(L"hhnnss", N);
          break;

        case L'@':
          Replacement = MakeValidFileName(SessionData->HostNameExpanded);
          break;

        case L's':
          Replacement = MakeValidFileName(SessionData->SessionName);
          break;

        case L'!':
          Replacement = L"!";
          break;

        default:
          Replacement = UnicodeString(L"!") + ANewFileName[Index + 1];
          break;
      }
      ANewFileName.Delete(Index, 2);
      ANewFileName.Insert(Replacement, Index);
      Index += Replacement.Length() - 1;
    }
  }
  Result = _wfopen(ANewFileName.c_str(), (Append ? L"a" : L"w"));
  if (Result != NULL)
  {
    setvbuf(Result, NULL, _IONBF, BUFSIZ);
    NewFileName = ANewFileName;
  }
  else
  {
    throw Exception(FMTLOAD(LOG_OPENERROR, (ANewFileName)));
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
  assert(FFile == NULL);
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
  assert(FSessionData != NULL);
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
  assert(FParent != NULL);
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
  assert(FConfiguration);
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
        throw ExtException(&E, LOG_GEN_ERROR);
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

  if (FLogging != ALogging)
  {
    FLogging = ALogging;
    StateChange();
  }

  // if logging to file was turned off or log file was changed -> close current log file
  if ((FFile != NULL) &&
      (!LogToFile() || (FCurrentLogFileName != FConfiguration->LogFileName)))
  {
    CloseLogFile();
  }

  DeleteUnnecessary();
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
    assert(FFile == NULL);
    assert(FConfiguration != NULL);
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
      throw ExtException(&E, LOG_GEN_ERROR);
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
void __fastcall TSessionLog::AddStartupInfo()
{
  if (Logging)
  {
    if (FParent != NULL)
    {
      // do not add session info for secondary session
      // (this should better be handled in the TSecondaryTerminal)
    }
    else
    {
      DoAddStartupInfo(FSessionData);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAddStartupInfo(TSessionData * Data)
{
  TGuard Guard(FCriticalSection);

  BeginUpdate();
  try
  {
    #define ADF(S, F) DoAdd(llMessage, FORMAT(S, F), DoAddToSelf);
    AddSeparator();
    ADF(L"WinSCP %s (OS %s)", (FConfiguration->VersionStr, FConfiguration->OSVersionStr));
    THierarchicalStorage * Storage = FConfiguration->CreateScpStorage(false);
    try
    {
      ADF(L"Configuration: %s", (Storage->Source));
    }
    __finally
    {
      delete Storage;
    }

    typedef BOOL WINAPI (* TGetUserNameEx)(EXTENDED_NAME_FORMAT NameFormat, LPWSTR lpNameBuffer, PULONG nSize);
    HINSTANCE Secur32 = LoadLibrary(L"secur32.dll");
    TGetUserNameEx GetUserNameEx =
      (Secur32 != NULL) ? (TGetUserNameEx)GetProcAddress(Secur32, "GetUserNameExW") : NULL;
    wchar_t UserName[UNLEN + 1];
    unsigned long UserNameSize = LENOF(UserName);
    if ((GetUserNameEx == NULL) || !GetUserNameEx(NameSamCompatible, UserName, &UserNameSize))
    {
      wcscpy(UserName, L"<Failed to retrieve username>");
    }
    ADF(L"Local account: %s", (UserName));
    ADF(L"Working directory: %s", (GetCurrentDir()));
    ADF(L"Command-line: %s", (CmdLine));
    ADF(L"Time zone: %s", (GetTimeZoneLogString()));
    ADF(L"Login time: %s", (FormatDateTime(L"dddddd tt", Now())));
    AddSeparator();
    ADF(L"Session name: %s (%s)", (Data->SessionName, Data->Source));
    ADF(L"Host name: %s (Port: %d)", (Data->HostNameExpanded, Data->PortNumber));
    ADF(L"User name: %s (Password: %s, Key file: %s)",
      (Data->UserNameExpanded, BooleanToEngStr(!Data->Password.IsEmpty()),
       BooleanToEngStr(!Data->PublicKeyFile.IsEmpty())))
    ADF(L"Tunnel: %s", (BooleanToEngStr(Data->Tunnel)));
    if (Data->Tunnel)
    {
      ADF(L"Tunnel: Host name: %s (Port: %d)", (Data->TunnelHostName, Data->TunnelPortNumber));
      ADF(L"Tunnel: User name: %s (Password: %s, Key file: %s)",
        (Data->TunnelUserName, BooleanToEngStr(!Data->TunnelPassword.IsEmpty()),
         BooleanToEngStr(!Data->TunnelPublicKeyFile.IsEmpty())))
      ADF(L"Tunnel: Local port number: %d", (Data->TunnelLocalPortNumber));
    }
    ADF(L"Transfer Protocol: %s", (Data->FSProtocolStr));
    wchar_t * PingTypes = L"-NC";
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
      (UnicodeString(PingTypes[PingType]), PingInterval, Data->Timeout));
    ADF(L"Proxy: %s", (ProxyMethodList[Data->ProxyMethod]));
    if (Data->ProxyMethod != ::pmNone)
    {
      ADF(L"HostName: %s (Port: %d); Username: %s; Passwd: %s",
        (Data->ProxyHost, Data->ProxyPort,
         Data->ProxyUsername, BooleanToEngStr(!Data->ProxyPassword.IsEmpty())));
      if (Data->ProxyMethod == pmTelnet)
      {
        ADF(L"Telnet command: %s", (Data->ProxyTelnetCommand));
      }
      if (Data->ProxyMethod == pmCmd)
      {
        ADF(L"Local command: %s", (Data->ProxyLocalCommand));
      }
    }
    wchar_t const * BugFlags = L"+-A";
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
        ADF(L"GSSAPI: Forwarding: %s; Server realm: %s",
          (BooleanToEngStr(Data->GSSAPIFwdTGT), Data->GSSAPIServerRealm));
      }
      ADF(L"Ciphers: %s; Ssh2DES: %s",
        (Data->CipherList, BooleanToEngStr(Data->Ssh2DES)));
      UnicodeString Bugs;
      for (int Index = 0; Index < BUG_COUNT; Index++)
      {
        Bugs += UnicodeString(BugFlags[Data->Bug[(TSshBug)Index]])+(Index<BUG_COUNT-1?L",":L"");
      }
      ADF(L"SSH Bugs: %s", (Bugs));
      Bugs = L"";
      for (int Index = 0; Index < SFTP_BUG_COUNT; Index++)
      {
        Bugs += UnicodeString(BugFlags[Data->SFTPBug[(TSftpBug)Index]])+(Index<SFTP_BUG_COUNT-1?L",":L"");
      }
      ADF(L"SFTP Bugs: %s", (Bugs));
      ADF(L"Return code variable: %s; Lookup user groups: %s",
        ((Data->DetectReturnVar ? UnicodeString(L"Autodetect") : Data->ReturnVar),
         BugFlags[Data->LookupUserGroups]));
      ADF(L"Shell: %s", ((Data->Shell.IsEmpty()? UnicodeString(L"default") : Data->Shell)));
      ADF(L"EOL: %d, UTF: %d", (Data->EOLType, Data->NotUtf));
      ADF(L"Clear aliases: %s, Unset nat.vars: %s, Resolve symlinks: %s",
        (BooleanToEngStr(Data->ClearAliases), BooleanToEngStr(Data->UnsetNationalVars),
         BooleanToEngStr(Data->ResolveSymlinks)));
      ADF(L"LS: %s, Ign LS warn: %s, Scp1 Comp: %s",
        (Data->ListingCommand,
         BooleanToEngStr(Data->IgnoreLsWarnings),
         BooleanToEngStr(Data->Scp1Compatibility)));
    }
    if (Data->FSProtocol == fsFTP)
    {
      UnicodeString Ftps;
      switch (Data->Ftps)
      {
        case ftpsImplicit:
          Ftps = L"Implicit SSL/TLS";
          break;

        case ftpsExplicitSsl:
          Ftps = L"Explicit SSL";
          break;

        case ftpsExplicitTls:
          Ftps = L"Explicit TLS";
          break;

        default:
          assert(Data->Ftps == ftpsNone);
          Ftps = L"None";
          break;
      }
      ADF(L"FTP: FTPS: %s; Passive: %s [Force IP: %s]",
        (Ftps, BooleanToEngStr(Data->FtpPasvMode),
         BugFlags[Data->FtpForcePasvIp]));
    }
    ADF(L"Local directory: %s, Remote directory: %s, Update: %s, Cache: %s",
      ((Data->LocalDirectory.IsEmpty() ? UnicodeString(L"default") : Data->LocalDirectory),
       (Data->RemoteDirectory.IsEmpty() ? UnicodeString(L"home") : Data->RemoteDirectory),
       BooleanToEngStr(Data->UpdateDirectories),
       BooleanToEngStr(Data->CacheDirectories)));
    ADF(L"Cache directory changes: %s, Permanent: %s",
      (BooleanToEngStr(Data->CacheDirectoryChanges),
       BooleanToEngStr(Data->PreserveDirectoryChanges)));
    ADF(L"DST mode: %d", (int(Data->DSTMode)));

    AddSeparator();

    #undef ADF
  }
  __finally
  {
    DeleteUnnecessary();

    EndUpdate();
  }
}
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
  assert((FFile == NULL) || LogToFile());
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
__fastcall TActionLog::TActionLog(TSessionUI* UI, TSessionData * SessionData,
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
  FPendingActions = new TList();
  FIndent = L"  ";
  FInGroup = false;
  FEnabled = true;
}
//---------------------------------------------------------------------------
__fastcall TActionLog::~TActionLog()
{
  assert(FPendingActions->Count == 0);
  delete FPendingActions;
  FClosed = true;
  ReflectSettings();
  assert(FFile == NULL);
  delete FCriticalSection;
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::Add(const UnicodeString & Line)
{
  assert(FConfiguration);
  if (FLogging)
  {
    try
    {
      TGuard Guard(FCriticalSection);
      if (FFile == NULL)
      {
        OpenLogFile();
      }

      if (FFile != NULL)
      {
        UTF8String UtfLine = UTF8String(Line);
        fwrite(UtfLine.c_str(), 1, UtfLine.Length(), (FILE *)FFile);
        fwrite("\n", 1, 1, (FILE *)FFile);
      }
    }
    catch (Exception &E)
    {
      // We failed logging, turn it off and notify user.
      FConfiguration->LogActions = false;
      try
      {
        throw ExtException(&E, LOG_GEN_ERROR);
      }
      catch (Exception &E)
      {
        FUI->HandleExtendedException(&E);
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
  TStrings * Messages = ExceptionToMessages(E);
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
    Add(FORMAT(L"<session xmlns=\"http://winscp.net/schema/session/1.0\" name=\"%s\" start=\"%s\">",
      (XmlAttributeEscape(FSessionData->SessionName), StandardTimestamp())));
  }
  else if (!ALogging && FLogging)
  {
    if (FInGroup)
    {
      EndGroup();
    }
    Add(L"</session>");
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
    assert(FFile == NULL);
    assert(FConfiguration != NULL);
    FCurrentLogFileName = FConfiguration->ActionsLogFileName;
    FFile = OpenFile(FCurrentLogFileName, FSessionData, false, FCurrentFileName);
  }
  catch (Exception & E)
  {
    // We failed logging to file, turn it off and notify user.
    FCurrentLogFileName = L"";
    FCurrentFileName = L"";
    FConfiguration->LogActions = false;
    try
    {
      throw ExtException(&E, LOG_GEN_ERROR);
    }
    catch (Exception & E)
    {
      FUI->HandleExtendedException(&E);
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
  assert(!FInGroup);
  FInGroup = true;
  assert(FIndent == L"  ");
  AddIndented(FORMAT("<group name=\"%s\" start=\"%s\">",
    (XmlAttributeEscape(Name), StandardTimestamp())));
  FIndent = L"    ";
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::EndGroup()
{
  assert(FInGroup);
  FInGroup = false;
  assert(FIndent == L"    ");
  FIndent = L"  ";
  AddIndented("</group>");
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
