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
  ReflectSettings();
}
//---------------------------------------------------------------------------
__fastcall TSessionLog::~TSessionLog()
{
  CloseLogFile();
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
      AnsiString Timestamp = FormatDateTime(" yyyy-mm-dd hh:nn:ss.zzz ", Now());
      fputc(LogLineMarks[Type], (FILE *)FFile);
      fwrite(Timestamp.c_str(), Timestamp.Length(), 1, (FILE *)FFile);
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

  if (!Name.IsEmpty())
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
  if (Logging)
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
        FUI->ShowExtendedException(&E);
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

  FLogging = (FParent != NULL) || FConfiguration->Logging;

  // if logging to file was turned off or log file was change -> close current log file
  if ((FFile != NULL) &&
      (!LogToFile() || (FCurrentLogFileName != FConfiguration->LogFileName)))
  {
    CloseLogFile();
  }

  DeleteUnnecessary();
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::SetParent(TSessionLog * value)
{
  if (FParent != value)
  {
    FParent = value;
    ReflectSettings();
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
  FCurrentLogFileName = "";
  FCurrentFileName = "";
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
    FFile = fopen(NewFileName.c_str(), (FConfiguration->LogFileAppend ? "a" : "w"));
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
      FUI->ShowExtendedException(&E);
    }
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
      ADF("Shell: %s, EOL: %d", ((Data->Shell.IsEmpty()? AnsiString("default") : Data->Shell), Data->EOLType));
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
      ADF("FTP: Passive: %s", (BooleanToEngStr(Data->FtpPasvMode)));
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
