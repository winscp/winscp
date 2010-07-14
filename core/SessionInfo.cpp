//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <stdio.h>

#include "Common.h"
#include "SessionInfo.h"
#include "Exceptions.h"
#include "TextsCore.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
AnsiString __fastcall XmlEscape(AnsiString Str)
{
  for (int i = 1; i <= Str.Length(); i++)
  {
    const char * Repl = NULL;
    switch (Str[i])
    {
      case '&':
        Repl = "amp;";
        break;

      case '>':
        Repl = "gt;";
        break;

      case '<':
        Repl = "lt;";
        break;

      case '"':
        Repl = "quot;";
        break;

      case '\r':
        Str.Delete(i, 1);
        i--;
        break;
    }

    if (Repl != NULL)
    {
      Str[i] = '&';
      Str.Insert(Repl, i + 1);
      i += strlen(Repl);
    }
  }
  Str = AnsiToUtf8(Str);
  return Str;
}
//---------------------------------------------------------------------------
AnsiString __fastcall XmlTimestamp(const TDateTime & DateTime)
{
  return FormatDateTime("yyyy'-'mm'-'dd'T'hh':'nn':'ss'.'zzz'Z'", ConvertTimestampToUTC(DateTime));
}
//---------------------------------------------------------------------------
AnsiString __fastcall XmlTimestamp()
{
  return XmlTimestamp(Now());
}
//---------------------------------------------------------------------------
#pragma warn -inl
class TSessionActionRecord
{
public:
  __fastcall TSessionActionRecord(TSessionLog * Log, TLogAction Action) :
    FLog(Log),
    FAction(Action),
    FState(Opened),
    FRecursive(false),
    FErrorMessages(NULL),
    FNames(new TStringList()),
    FValues(new TStringList()),
    FFileList(NULL)
  {
    FLog->AddPendingAction(this);
  }

  __fastcall ~TSessionActionRecord()
  {
    delete FErrorMessages;
    delete FNames;
    delete FValues;
    delete FFileList;
  }

  void __fastcall Restart()
  {
    FState = Opened;
    FRecursive = false;
    delete FErrorMessages;
    FErrorMessages = NULL;
    delete FFileList;
    FFileList = NULL;
    FNames->Clear();
    FValues->Clear();
  }

  bool __fastcall Record()
  {
    bool Result = (FState != Opened);
    if (Result)
    {
      if ((FLog->FLoggingActions) && (FState != Cancelled))
      {
        const char * Name = ActionName();
        AnsiString Attrs;
        if (FRecursive)
        {
          Attrs = " recursive=\"true\"";
        }
        FLog->Add(llAction, FORMAT("  <%s%s>", (Name,  Attrs)));
        for (int Index = 0; Index < FNames->Count; Index++)
        {
          AnsiString Value = FValues->Strings[Index];
          if (Value.IsEmpty())
          {
            FLog->Add(llAction, FORMAT("    <%s />", (FNames->Strings[Index])));
          }
          else
          {
            FLog->Add(llAction, FORMAT("    <%s value=\"%s\" />",
              (FNames->Strings[Index], XmlEscape(Value))));
          }
        }
        if (FFileList != NULL)
        {
          FLog->Add(llAction, "    <files>");
          for (int Index = 0; Index < FFileList->Count; Index++)
          {
            TRemoteFile * File = FFileList->Files[Index];

            FLog->Add(llAction, "      <file>");
            FLog->Add(llAction, FORMAT("        <filename value=\"%s\" />", (XmlEscape(File->FileName))));
            FLog->Add(llAction, FORMAT("        <type value=\"%s\" />", (XmlEscape(File->Type))));
            if (!File->IsDirectory)
            {
              FLog->Add(llAction, FORMAT("        <size value=\"%s\" />", (IntToStr(File->Size))));
            }
            FLog->Add(llAction, FORMAT("        <modification value=\"%s\" />", (XmlTimestamp(File->Modification))));
            FLog->Add(llAction, FORMAT("        <permissions value=\"%s\" />", (XmlEscape(File->Rights->Text))));
            FLog->Add(llAction, "      </file>");
          }
          FLog->Add(llAction, "    </files>");
        }
        if (FState == RolledBack)
        {
          if (FErrorMessages != NULL)
          {
            FLog->Add(llAction, "    <result success=\"false\">");
            for (int Index = 0; Index < FErrorMessages->Count; Index++)
            {
              FLog->Add(llAction,
                FORMAT("      <message>%s</message>", (XmlEscape(FErrorMessages->Strings[Index]))));
            }
            FLog->Add(llAction, "    </result>");
          }
          else
          {
            FLog->Add(llAction, "    <result success=\"false\" />");
          }
        }
        else
        {
          FLog->Add(llAction, "    <result success=\"true\" />");
        }
        FLog->Add(llAction, FORMAT("  </%s>", (Name)));
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
    FErrorMessages = new TStringList();
    if (!E->Message.IsEmpty())
    {
      FErrorMessages->Add(E->Message);
    }
    ExtException * EE = dynamic_cast<ExtException *>(E);
    if ((EE != NULL) && (EE->MoreMessages != NULL))
    {
      FErrorMessages->AddStrings(EE->MoreMessages);
    }
    if (FErrorMessages->Count == 0)
    {
      delete FErrorMessages;
      FErrorMessages = NULL;
    }
    Close(RolledBack);
  }

  void __fastcall Cancel()
  {
    Close(Cancelled);
  }

  void __fastcall FileName(const AnsiString & FileName)
  {
    Parameter("filename", FileName);
  }

  void __fastcall Destination(const AnsiString & Destination)
  {
    Parameter("destination", Destination);
  }

  void __fastcall Rights(const TRights & Rights)
  {
    Parameter("permissions", Rights.Text);
  }

  void __fastcall Modification(const TDateTime & DateTime)
  {
    Parameter("modification", XmlTimestamp(DateTime));
  }

  void __fastcall Recursive()
  {
    FRecursive = true;
  }

  void __fastcall Command(const AnsiString & Command)
  {
    Parameter("command", Command);
  }

  void __fastcall AddOutput(AnsiString Output, bool StdError)
  {
    const char * Name = (StdError ? "erroroutput" : "output");
    int Index = FNames->IndexOf(Name);
    if (Index >= 0)
    {
      FValues->Strings[Index] = FValues->Strings[Index] + "\r\n" + Output;
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

protected:
  enum TState { Opened, Committed, RolledBack, Cancelled };

  inline void __fastcall Close(TState State)
  {
    assert(FState == Opened);
    FState = State;
    FLog->RecordPendingActions();
  }

  const char * __fastcall ActionName()
  {
    switch (FAction)
    {
      case laUpload: return "upload";
      case laDownload: return "download";
      case laTouch: return "touch";
      case laChmod: return "chmod";
      case laMkdir: return "mkdir";
      case laRm: return "rm";
      case laMv: return "mv";
      case laCall: return "call";
      case laLs: return "ls";
      default: assert(false); return "";
    }
  }

  void __fastcall Parameter(const AnsiString & Name, const AnsiString & Value = "")
  {
    FNames->Add(Name);
    FValues->Add(Value);
  }

private:
  TSessionLog * FLog;
  TLogAction FAction;
  TState FState;
  bool FRecursive;
  TStrings * FErrorMessages;
  TStrings * FNames;
  TStrings * FValues;
  TRemoteFileList * FFileList;
};
#pragma warn .inl
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TSessionAction::TSessionAction(TSessionLog * Log, TLogAction Action)
{
  if (Log->FLoggingActions)
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
__fastcall TFileSessionAction::TFileSessionAction(TSessionLog * Log, TLogAction Action) :
  TSessionAction(Log, Action)
{
};
//---------------------------------------------------------------------------
__fastcall TFileSessionAction::TFileSessionAction(
    TSessionLog * Log, TLogAction Action, const AnsiString & AFileName) :
  TSessionAction(Log, Action)
{
  FileName(AFileName);
};
//---------------------------------------------------------------------------
void __fastcall TFileSessionAction::FileName(const AnsiString & FileName)
{
  if (FRecord != NULL)
  {
    FRecord->FileName(FileName);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TFileLocationSessionAction::TFileLocationSessionAction(
    TSessionLog * Log, TLogAction Action) :
  TFileSessionAction(Log, Action)
{
};
//---------------------------------------------------------------------------
__fastcall TFileLocationSessionAction::TFileLocationSessionAction(
    TSessionLog * Log, TLogAction Action, const AnsiString & FileName) :
  TFileSessionAction(Log, Action, FileName)
{
};
//---------------------------------------------------------------------------
void __fastcall TFileLocationSessionAction::Destination(const AnsiString & Destination)
{
  if (FRecord != NULL)
  {
    FRecord->Destination(Destination);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TUploadSessionAction::TUploadSessionAction(TSessionLog * Log) :
  TFileLocationSessionAction(Log, laUpload)
{
};
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TDownloadSessionAction::TDownloadSessionAction(TSessionLog * Log) :
  TFileLocationSessionAction(Log, laDownload)
{
};
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TChmodSessionAction::TChmodSessionAction(
    TSessionLog * Log, const AnsiString & FileName) :
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
    TSessionLog * Log, const AnsiString & FileName, const TRights & ARights) :
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
    TSessionLog * Log, const AnsiString & FileName, const TDateTime & Modification) :
  TFileSessionAction(Log, laTouch, FileName)
{
  if (FRecord != NULL)
  {
    FRecord->Modification(Modification);
  }
}
//---------------------------------------------------------------------------
__fastcall TMkdirSessionAction::TMkdirSessionAction(
    TSessionLog * Log, const AnsiString & FileName) :
  TFileSessionAction(Log, laMkdir, FileName)
{
}
//---------------------------------------------------------------------------
__fastcall TRmSessionAction::TRmSessionAction(
    TSessionLog * Log, const AnsiString & FileName) :
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
__fastcall TMvSessionAction::TMvSessionAction(TSessionLog * Log,
    const AnsiString & FileName, const AnsiString & ADestination) :
  TFileLocationSessionAction(Log, laMv, FileName)
{
  Destination(ADestination);
}
//---------------------------------------------------------------------------
__fastcall TCallSessionAction::TCallSessionAction(TSessionLog * Log,
    const AnsiString & Command, const AnsiString & Destination) :
  TSessionAction(Log, laCall)
{
  if (FRecord != NULL)
  {
    FRecord->Command(Command);
    FRecord->Destination(Destination);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCallSessionAction::AddOutput(const AnsiString & Output, bool StdError)
{
  if (FRecord != NULL)
  {
    FRecord->AddOutput(Output, StdError);
  }
}
//---------------------------------------------------------------------------
__fastcall TLsSessionAction::TLsSessionAction(TSessionLog * Log,
    const AnsiString & Destination) :
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
const char *LogLineMarks = "<>!.*";
__fastcall TSessionLog::TSessionLog(TSessionUI* UI, TSessionData * SessionData,
  TConfiguration * Configuration):
  TStringList()
{
  FCriticalSection = new TCriticalSection;
  FConfiguration = Configuration;
  FParent = NULL;
  FUI = UI;
  FSessionData = SessionData;
  FFile = NULL;
  FLoggedLines = 0;
  FTopIndex = -1;
  FCurrentLogFileName = "";
  FCurrentFileName = "";
  FLoggingActions = false;
  FClosed = false;
  FPendingActions = new TList();
}
//---------------------------------------------------------------------------
__fastcall TSessionLog::~TSessionLog()
{
  assert(FPendingActions->Count == 0);
  delete FPendingActions;
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
AnsiString __fastcall TSessionLog::GetSessionName()
{
  assert(FSessionData != NULL);
  return FSessionData->SessionName;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSessionLog::GetLine(Integer Index)
{
  return Strings[Index - FTopIndex];
}
//---------------------------------------------------------------------------
TLogLineType __fastcall TSessionLog::GetType(int Index)
{
  return (TLogLineType)Objects[Index - FTopIndex];
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAddToParent(TLogLineType Type, const AnsiString & Line)
{
  assert(FParent != NULL);
  FParent->Add(Type, Line);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAddToSelf(TLogLineType Type, const AnsiString & Line)
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
      if (Type != llAction)
      {
        AnsiString Timestamp = FormatDateTime(" yyyy-mm-dd hh:nn:ss.zzz ", Now());
        fputc(LogLineMarks[Type], (FILE *)FFile);
        fwrite(Timestamp.c_str(), Timestamp.Length(), 1, (FILE *)FFile);
      }
      // use fwrite instead of fprintf to make sure that even
      // non-ascii data (unicode) gets in.
      fwrite(Line.c_str(), Line.Length(), 1, (FILE *)FFile);
      fputc('\n', (FILE *)FFile);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAdd(TLogLineType Type, AnsiString Line,
  void __fastcall (__closure *f)(TLogLineType Type, const AnsiString & Line))
{
  AnsiString Prefix;

  if ((Type != llAction) && !Name.IsEmpty())
  {
    Prefix = "[" + Name + "] ";
  }

  while (!Line.IsEmpty())
  {
    f(Type, Prefix + CutToChar(Line, '\n', false));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::Add(TLogLineType Type, const AnsiString & Line)
{
  assert(FConfiguration);
  if (Logging && (FConfiguration->LogActions == (Type == llAction)))
  {
    try
    {
      if (FParent != NULL)
      {
        DoAdd(Type, Line, &DoAddToParent);
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
    ((FParent != NULL) || FConfiguration->Logging) &&
    ((FParent == NULL) || !FConfiguration->LogActions);

  bool LoggingActions = ALogging && FConfiguration->LogActions;
  if (LoggingActions && !FLoggingActions)
  {
    FLoggingActions = true;
    FLogging = ALogging;
    Add(llAction, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
    Add(llAction, FORMAT("<session xmlns=\"http://winscp.net/schema/session/1.0\" name=\"%s\" start=\"%s\">",
      (XmlEscape(FSessionData->SessionName), XmlTimestamp())));
    StateChange();
  }
  else if (!LoggingActions && FLoggingActions)
  {
    FLoggingActions = false;
    Add(llAction, "</session>");
    FLogging = ALogging;
    StateChange();
  }
  else if (FLogging != ALogging)
  {
    FLogging = ALogging;
    StateChange();
  }

  // if logging to file was turned off or log file was change -> close current log file
  // but disallow changing logfilename for xml logging
  if ((FFile != NULL) &&
      ((!LogToFile() || (FCurrentLogFileName != FConfiguration->LogFileName)) && !FLoggingActions))
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
  FCurrentLogFileName = "";
  FCurrentFileName = "";
  StateChange();
}
//---------------------------------------------------------------------------
void TSessionLog::OpenLogFile()
{
  try
  {
    assert(FFile == NULL);
    assert(FConfiguration != NULL);
    FCurrentLogFileName = FConfiguration->LogFileName;
    AnsiString NewFileName = StripPathQuotes(ExpandEnvironmentVariables(FCurrentLogFileName));
    TDateTime N = Now();
    for (int Index = 1; Index < NewFileName.Length(); Index++)
    {
      if (NewFileName[Index] == '!')
      {
        AnsiString Replacement;
        // keep consistent with TFileCustomCommand::PatternReplacement
        switch (tolower(NewFileName[Index + 1]))
        {
          case 'y':
            Replacement = FormatDateTime("yyyy", N);
            break;

          case 'm':
            Replacement = FormatDateTime("mm", N);
            break;

          case 'd':
            Replacement = FormatDateTime("dd", N);
            break;

          case 't':
            Replacement = FormatDateTime("hhnnss", N);
            break;

          case '@':
            Replacement = MakeValidFileName(FSessionData->HostName);
            break;

          case 's':
            Replacement = MakeValidFileName(FSessionData->SessionName);
            break;

          case '!':
            Replacement = "!";
            break;

          default:
            Replacement = AnsiString("!") + NewFileName[Index + 1];
            break;
        }
        NewFileName.Delete(Index, 2);
        NewFileName.Insert(Replacement, Index);
        Index += Replacement.Length() - 1;
      }
    }
    FFile = fopen(NewFileName.c_str(),
      (FConfiguration->LogFileAppend && !FLoggingActions ? "a" : "w"));
    if (FFile)
    {
      setvbuf((FILE *)FFile, NULL, _IONBF, BUFSIZ);
      FCurrentFileName = NewFileName;
    }
    else
    {
      throw Exception(FMTLOAD(LOG_OPENERROR, (NewFileName)));
    }
  }
  catch (Exception & E)
  {
    // We failed logging to file, turn it off and notify user.
    FCurrentLogFileName = "";
    FCurrentFileName = "";
    FConfiguration->LogToFile = false;
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
void TSessionLog::StateChange()
{
  if (FOnStateChange != NULL)
  {
    FOnStateChange(this);
  }
}
//---------------------------------------------------------------------------
void TSessionLog::DeleteUnnecessary()
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
  if (Logging && !FConfiguration->LogActions)
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
  assert(Logging);

  TGuard Guard(FCriticalSection);

  BeginUpdate();
  try
  {
    #define ADF(S, F) DoAdd(llMessage, FORMAT(S, F), DoAddToSelf);
    AddSeparator();
    ADF("WinSCP %s (OS %s)", (FConfiguration->VersionStr, FConfiguration->OSVersionStr));
    ADF("Login time: %s", (FormatDateTime("dddddd tt", Now())));
    AddSeparator();
    ADF("Session name: %s", (Data->SessionName));
    ADF("Host name: %s (Port: %d)", (Data->HostName, Data->PortNumber));
    ADF("User name: %s (Password: %s, Key file: %s)",
      (Data->UserName, BooleanToEngStr(!Data->Password.IsEmpty()),
       BooleanToEngStr(!Data->PublicKeyFile.IsEmpty())))
    ADF("Tunnel: %s", (BooleanToEngStr(Data->Tunnel)));
    if (Data->Tunnel)
    {
      ADF("Tunnel: Host name: %s (Port: %d)", (Data->TunnelHostName, Data->TunnelPortNumber));
      ADF("Tunnel: User name: %s (Password: %s, Key file: %s)",
        (Data->TunnelUserName, BooleanToEngStr(!Data->TunnelPassword.IsEmpty()),
         BooleanToEngStr(!Data->TunnelPublicKeyFile.IsEmpty())))
      ADF("Tunnel: Local port number: %d", (Data->TunnelLocalPortNumber));
    }
    ADF("Transfer Protocol: %s", (Data->FSProtocolStr));
    char * PingTypes = "-NC";
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
    ADF("Ping type: %s, Ping interval: %d sec; Timeout: %d sec",
      (AnsiString(PingTypes[PingType]), PingInterval, Data->Timeout));
    ADF("Proxy: %s", (ProxyMethodList[Data->ProxyMethod]));
    if (Data->ProxyMethod != pmNone)
    {
      ADF("HostName: %s (Port: %d); Username: %s; Passwd: %s",
        (Data->ProxyHost, Data->ProxyPort,
         Data->ProxyUsername, BooleanToEngStr(!Data->ProxyPassword.IsEmpty())));
      if (Data->ProxyMethod == pmTelnet)
      {
        ADF("Telnet command: %s", (Data->ProxyTelnetCommand));
      }
      if (Data->ProxyMethod == pmCmd)
      {
        ADF("Local command: %s", (Data->ProxyLocalCommand));
      }
    }
    if (Data->UsesSsh)
    {
      ADF("SSH protocol version: %s; Compression: %s",
        (Data->SshProtStr, BooleanToEngStr(Data->Compression)));
      ADF("Bypass authentication: %s",
       (BooleanToEngStr(Data->SshNoUserAuth)));
      ADF("Try agent: %s; Agent forwarding: %s; TIS/CryptoCard: %s; KI: %s; GSSAPI: %s",
        (BooleanToEngStr(Data->TryAgent), BooleanToEngStr(Data->AgentFwd), BooleanToEngStr(Data->AuthTIS),
         BooleanToEngStr(Data->AuthKI), BooleanToEngStr(Data->AuthGSSAPI)));
      if (Data->AuthGSSAPI)
      {
        ADF("GSSAPI: Forwarding: %s; Server realm: %s",
          (BooleanToEngStr(Data->GSSAPIFwdTGT), Data->GSSAPIServerRealm));
      }
      ADF("Ciphers: %s; Ssh2DES: %s",
        (Data->CipherList, BooleanToEngStr(Data->Ssh2DES)));
      AnsiString Bugs;
      char const * BugFlags = "A+-";
      for (int Index = 0; Index < BUG_COUNT; Index++)
      {
        Bugs += AnsiString(BugFlags[Data->Bug[(TSshBug)Index]])+(Index<BUG_COUNT-1?",":"");
      }
      ADF("SSH Bugs: %s", (Bugs));
      Bugs = "";
      for (int Index = 0; Index < SFTP_BUG_COUNT; Index++)
      {
        Bugs += AnsiString(BugFlags[Data->SFTPBug[(TSftpBug)Index]])+(Index<SFTP_BUG_COUNT-1?",":"");
      }
      ADF("SFTP Bugs: %s", (Bugs));
      ADF("Return code variable: %s; Lookup user groups: %s",
        ((Data->DetectReturnVar ? AnsiString("Autodetect") : Data->ReturnVar),
         BooleanToEngStr(Data->LookupUserGroups)));
      ADF("Shell: %s", ((Data->Shell.IsEmpty()? AnsiString("default") : Data->Shell)));
      ADF("EOL: %d, UTF: %d", (Data->EOLType, Data->NotUtf));
      ADF("Clear aliases: %s, Unset nat.vars: %s, Resolve symlinks: %s",
        (BooleanToEngStr(Data->ClearAliases), BooleanToEngStr(Data->UnsetNationalVars),
         BooleanToEngStr(Data->ResolveSymlinks)));
      ADF("LS: %s, Ign LS warn: %s, Scp1 Comp: %s",
        (Data->ListingCommand,
         BooleanToEngStr(Data->IgnoreLsWarnings),
         BooleanToEngStr(Data->Scp1Compatibility)));
    }
    if (Data->FSProtocol == fsFTP)
    {
      AnsiString Ftps;
      switch (Data->Ftps)
      {
        case ftpsImplicit:
          Ftps = "Implicit SSL/TLS";
          break;

        case ftpsExplicitSsl:
          Ftps = "Explicit SSL";
          break;

        case ftpsExplicitTls:
          Ftps = "Explicit TLS";
          break;

        default:
          assert(Data->Ftps == ftpsNone);
          Ftps = "None";
          break;
      }
      ADF("FTP: FTPS: %s; Passive: %s [Force IP: %s]",
        (Ftps, BooleanToEngStr(Data->FtpPasvMode),
         BooleanToEngStr(Data->FtpForcePasvIp)));
    }
    ADF("Local directory: %s, Remote directory: %s, Update: %s, Cache: %s",
      ((Data->LocalDirectory.IsEmpty() ? AnsiString("default") : Data->LocalDirectory),
       (Data->RemoteDirectory.IsEmpty() ? AnsiString("home") : Data->RemoteDirectory),
       BooleanToEngStr(Data->UpdateDirectories),
       BooleanToEngStr(Data->CacheDirectories)));
    ADF("Cache directory changes: %s, Permanent: %s",
      (BooleanToEngStr(Data->CacheDirectoryChanges),
       BooleanToEngStr(Data->PreserveDirectoryChanges)));
    ADF("DST mode: %d", (int(Data->DSTMode)));

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
  Add(llMessage, "--------------------------------------------------------------------------");
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
void __fastcall TSessionLog::AddPendingAction(TSessionActionRecord * Action)
{
  FPendingActions->Add(Action);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::RecordPendingActions()
{
  while ((FPendingActions->Count > 0) &&
         static_cast<TSessionActionRecord *>(FPendingActions->Items[0])->Record())
  {
    FPendingActions->Delete(0);
  }
}
