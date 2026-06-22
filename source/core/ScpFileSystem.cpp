//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ScpFileSystem.h"

#include "Terminal.h"
#include "Common.h"
#include "Exceptions.h"
#include "Interface.h"
#include "TextsCore.h"
#include "HelpCore.h"
#include "SecureShell.h"
#include <StrUtils.hpp>

#include <stdio.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_TERMINAL FTerminal
//---------------------------------------------------------------------------
const int coRaiseExcept = 1;
const int coExpectNoOutput = 2;
const int coWaitForLastLine = 4;
const int coOnlyReturnCode = 8;
const int coIgnoreWarnings = 16;
const int coReadProgress = 32;
const int coIgnoreStdErr = 64;

const int ecRaiseExcept = 0x01;
const int ecIgnoreWarnings = 0x02;
const int ecReadProgress = 0x04;
const int ecIgnoreStdErr = 0x08;
const int ecNoEnsureLocation = 0x10;
const int ecDefault = ecRaiseExcept;
//---------------------------------------------------------------------------
DERIVE_EXT_EXCEPTION(EScpFileSkipped, ESkipFile);
//===========================================================================
#define MaxShellCommand fsLang
#define ShellCommandCount MaxShellCommand + 1
#define MaxCommandLen 40
struct TCommandType
{
  int MinLines;
  int MaxLines;
  bool ModifiesFiles;
  bool ChangesDirectory;
  bool InteractiveCommand;
  wchar_t Command[MaxCommandLen];
};

// Only one character! See TSCPFileSystem::ReadCommandOutput()
#define LastLineSeparator L":"
#define LAST_LINE L"WinSCP: this is end-of-file"
#define FIRST_LINE L"WinSCP: this is begin-of-file"
extern const TCommandType DefaultCommandSet[];

#define CHECK_CMD DebugAssert((Cmd >=0) && (Cmd <= MaxShellCommand))

class TSessionData;
//---------------------------------------------------------------------------
class TCommandSet
{
private:
  TCommandType CommandSet[ShellCommandCount];
  TSessionData * FSessionData;
  UnicodeString FReturnVar;
  int __fastcall GetMaxLines(TFSCommand Cmd);
  int __fastcall GetMinLines(TFSCommand Cmd);
  bool __fastcall GetModifiesFiles(TFSCommand Cmd);
  bool __fastcall GetChangesDirectory(TFSCommand Cmd);
  bool __fastcall GetOneLineCommand(TFSCommand Cmd);
  void __fastcall SetCommands(TFSCommand Cmd, UnicodeString value);
  UnicodeString __fastcall GetCommands(TFSCommand Cmd);
  UnicodeString __fastcall GetFirstLine();
  bool __fastcall GetInteractiveCommand(TFSCommand Cmd);
  UnicodeString __fastcall GetLastLine();
  UnicodeString __fastcall GetReturnVar();
public:
  __fastcall TCommandSet(TSessionData *aSessionData);
  void __fastcall Default();
  void __fastcall CopyFrom(TCommandSet * Source);
  UnicodeString __fastcall Command(TFSCommand Cmd, const TVarRec * args, int size);
  TStrings * __fastcall CreateCommandList();
  UnicodeString __fastcall FullCommand(TFSCommand Cmd, const TVarRec * args, int size);
  static UnicodeString __fastcall ExtractCommand(UnicodeString Command);
  __property int MaxLines[TFSCommand Cmd]  = { read=GetMaxLines};
  __property int MinLines[TFSCommand Cmd]  = { read=GetMinLines };
  __property bool ModifiesFiles[TFSCommand Cmd]  = { read=GetModifiesFiles };
  __property bool ChangesDirectory[TFSCommand Cmd]  = { read=GetChangesDirectory };
  __property bool OneLineCommand[TFSCommand Cmd]  = { read=GetOneLineCommand };
  __property UnicodeString Commands[TFSCommand Cmd]  = { read=GetCommands, write=SetCommands };
  __property UnicodeString FirstLine = { read = GetFirstLine };
  __property bool InteractiveCommand[TFSCommand Cmd] = { read = GetInteractiveCommand };
  __property UnicodeString LastLine  = { read=GetLastLine };
  __property TSessionData * SessionData  = { read=FSessionData, write=FSessionData };
  __property UnicodeString ReturnVar  = { read=GetReturnVar, write=FReturnVar };
};
//===========================================================================
const wchar_t NationalVars[][15] =
  {L"LANG", L"LANGUAGE", L"LC_CTYPE", L"LC_COLLATE", L"LC_MONETARY", L"LC_NUMERIC",
   L"LC_TIME", L"LC_MESSAGES", L"LC_ALL", L"HUMAN_BLOCKS", L"BLOCK_SIZE", L"LS_BLOCK_SIZE" };
const wchar_t FullTimeOption[] = L"--full-time";
//---------------------------------------------------------------------------
#define F false
#define T true
// TODO: remove "mf" and "cd", it is implemented in TTerminal already
const TCommandType DefaultCommandSet[ShellCommandCount] = {
//                       min max mf cd ia  command
/*Null*/                { -1, -1, F, F, F, L"" },
/*VarValue*/            { -1, -1, F, F, F, L"echo \"$%s\"" /* variable */ },
/*LastLine*/            { -1, -1, F, F, F, L"echo \"%s" LastLineSeparator "%s\"" /* last line, return var */ },
/*FirstLine*/           { -1, -1, F, F, F, L"echo \"%s\"" /* first line */ },
/*CurrentDirectory*/    {  1,  1, F, F, F, L"pwd" },
/*ChangeDirectory*/     {  0,  0, F, T, F, L"cd %s" /* directory */ },
// list directory can be empty on permission denied, this is handled in ReadDirectory
/*ListDirectory*/       { -1, -1, F, F, F, L"%s %s \"%s\"" /* listing command, options, directory */ },
/*ListCurrentDirectory*/{ -1, -1, F, F, F, L"%s %s" /* listing command, options */ },
/*ListFile*/            {  1,  1, F, F, F, L"%s -d %s \"%s\"" /* listing command, options, file/directory */ },
/*LookupUserGroups*/    {  0,  1, F, F, F, L"groups" },
/*CopyToRemote*/        { -1, -1, T, F, T, L"scp -r %s -d -t \"%s\"" /* options, directory */ },
/*CopyToLocal*/         { -1, -1, F, F, T, L"scp -r %s -d -f \"%s\"" /* options, file */ },
/*DeleteFile*/          {  0,  0, T, F, F, L"rm -f -r \"%s\"" /* file/directory */},
/*RenameFile*/          {  0,  0, T, F, F, L"mv -f \"%s\" \"%s\"" /* file/directory, new name*/},
/*CreateDirectory*/     {  0,  0, T, F, F, L"mkdir \"%s\"" /* new directory */},
/*ChangeMode*/          {  0,  0, T, F, F, L"chmod %s %s \"%s\"" /* options, mode, filename */},
/*ChangeGroup*/         {  0,  0, T, F, F, L"chgrp %s \"%s\" \"%s\"" /* options, group, filename */},
/*ChangeOwner*/         {  0,  0, T, F, F, L"chown %s \"%s\" \"%s\"" /* options, owner, filename */},
/*HomeDirectory*/       {  0,  0, F, T, F, L"cd" },
/*Unset*/               {  0,  0, F, F, F, L"unset \"%s\"" /* variable */ },
/*Unalias*/             {  0,  0, F, F, F, L"unalias \"%s\"" /* alias */ },
/*CreateLink*/          {  0,  0, T, F, F, L"ln %s \"%s\" \"%s\"" /*symbolic (-s), filename, point to*/},
/*CopyFile*/            {  0,  0, T, F, F, L"cp -p -r -f %s \"%s\" \"%s\"" /* file/directory, target name*/},
/*AnyCommand*/          {  0, -1, T, T, F, L"%s" },
/*Lang*/                {  0,  1, F, F, F, L"printenv LANG"}
};
#undef F
#undef T
//---------------------------------------------------------------------------
__fastcall TCommandSet::TCommandSet(TSessionData *aSessionData):
  FSessionData(aSessionData), FReturnVar(L"")
{
  DebugAssert(FSessionData);
  Default();
}
//---------------------------------------------------------------------------
void __fastcall TCommandSet::CopyFrom(TCommandSet * Source)
{
  memmove(&CommandSet, Source->CommandSet, sizeof(CommandSet));
}
//---------------------------------------------------------------------------
void __fastcall TCommandSet::Default()
{
  DebugAssert(sizeof(CommandSet) == sizeof(DefaultCommandSet));
  memmove(&CommandSet, &DefaultCommandSet, sizeof(CommandSet));
}
//---------------------------------------------------------------------------
int __fastcall TCommandSet::GetMaxLines(TFSCommand Cmd)
{
  CHECK_CMD;
  return CommandSet[Cmd].MaxLines;
}
//---------------------------------------------------------------------------
int __fastcall TCommandSet::GetMinLines(TFSCommand Cmd)
{
  CHECK_CMD;
  return CommandSet[Cmd].MinLines;
}
//---------------------------------------------------------------------------
bool __fastcall TCommandSet::GetModifiesFiles(TFSCommand Cmd)
{
  CHECK_CMD;
  return CommandSet[Cmd].ModifiesFiles;
}
//---------------------------------------------------------------------------
bool __fastcall TCommandSet::GetChangesDirectory(TFSCommand Cmd)
{
  CHECK_CMD;
  return CommandSet[Cmd].ChangesDirectory;
}
//---------------------------------------------------------------------------
bool __fastcall TCommandSet::GetInteractiveCommand(TFSCommand Cmd)
{
  CHECK_CMD;
  return CommandSet[Cmd].InteractiveCommand;
}
//---------------------------------------------------------------------------
bool __fastcall TCommandSet::GetOneLineCommand(TFSCommand /*Cmd*/)
{
  //CHECK_CMD;
  // #56: we send "echo last line" from all commands on same line
  // just as it was in 1.0
  return True; //CommandSet[Cmd].OneLineCommand;
}
//---------------------------------------------------------------------------
void __fastcall TCommandSet::SetCommands(TFSCommand Cmd, UnicodeString value)
{
  CHECK_CMD;
  wcscpy(CommandSet[Cmd].Command, value.SubString(1, MaxCommandLen - 1).c_str());
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCommandSet::GetCommands(TFSCommand Cmd)
{
  CHECK_CMD;
  return CommandSet[Cmd].Command;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCommandSet::Command(TFSCommand Cmd, const TVarRec * args, int size)
{
  if (args) return Format(Commands[Cmd], args, size);
    else return Commands[Cmd];
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCommandSet::FullCommand(TFSCommand Cmd, const TVarRec * args, int size)
{
  UnicodeString Separator;
  if (OneLineCommand[Cmd]) Separator = L" ; ";
    else Separator = L"\n";
  UnicodeString Line = Command(Cmd, args, size);
  UnicodeString LastLineCmd =
    Command(fsLastLine, ARRAYOFCONST((LastLine, ReturnVar)));
  UnicodeString FirstLineCmd;
  if (InteractiveCommand[Cmd])
    FirstLineCmd = Command(fsFirstLine, ARRAYOFCONST((FirstLine))) + Separator;

  UnicodeString Result;
  if (!Line.IsEmpty())
    Result = FORMAT(L"%s%s%s%s", (FirstLineCmd, Line, Separator, LastLineCmd));
  else
    Result = FORMAT(L"%s%s", (FirstLineCmd, LastLineCmd));
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCommandSet::GetFirstLine()
{
  return FIRST_LINE;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCommandSet::GetLastLine()
{
  return LAST_LINE;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCommandSet::GetReturnVar()
{
  DebugAssert(SessionData);
  if (!FReturnVar.IsEmpty())
  {
    return UnicodeString(L'$') + FReturnVar;
  }
  else if (SessionData->DetectReturnVar)
  {
    return L'0';
  }
  else
  {
    return UnicodeString(L'$') + SessionData->ReturnVar;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCommandSet::ExtractCommand(UnicodeString Command)
{
  int P = Command.Pos(L" ");
  if (P > 0)
  {
    Command.SetLength(P-1);
  }
  return Command;
}
//---------------------------------------------------------------------------
TStrings * __fastcall TCommandSet::CreateCommandList()
{
  TStrings * CommandList = new TStringList();
  for (Integer Index = 0; Index < ShellCommandCount; Index++)
  {
    UnicodeString Cmd = Commands[(TFSCommand)Index];
    if (!Cmd.IsEmpty())
    {
      Cmd = ExtractCommand(Cmd);
      if ((Cmd != L"%s") && (CommandList->IndexOf(Cmd) < 0))
        CommandList->Add(Cmd);
    }
  }
  return CommandList;
}
//===========================================================================
__fastcall TSCPFileSystem::TSCPFileSystem(TTerminal * ATerminal, TSecureShell * SecureShell):
  TCustomFileSystem(ATerminal)
{
  FSecureShell = SecureShell;
  FCommandSet = new TCommandSet(FTerminal->SessionData);
  FLsFullTime = FTerminal->SessionData->SCPLsFullTime;
  FOutput = new TStringList();
  FProcessingCommand = false;
  FOnCaptureOutput = NULL;

  FFileSystemInfo.ProtocolBaseName = L"SCP";
  FFileSystemInfo.ProtocolName = FFileSystemInfo.ProtocolBaseName;
}
//---------------------------------------------------------------------------
__fastcall TSCPFileSystem::~TSCPFileSystem()
{
  delete FCommandSet;
  delete FOutput;
  delete FSecureShell;
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::Open()
{
  // this is used for reconnects only
  FSecureShell->Open();
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::Close()
{
  FSecureShell->Close();
}
//---------------------------------------------------------------------------
bool __fastcall TSCPFileSystem::GetActive()
{
  return FSecureShell->Active;
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CollectUsage()
{
  FSecureShell->CollectUsage();
}
//---------------------------------------------------------------------------
const TSessionInfo & __fastcall TSCPFileSystem::GetSessionInfo()
{
  return FSecureShell->GetSessionInfo();
}
//---------------------------------------------------------------------------
const TFileSystemInfo & __fastcall TSCPFileSystem::GetFileSystemInfo(bool Retrieve)
{
  if (FFileSystemInfo.AdditionalInfo.IsEmpty() && Retrieve)
  {
    UnicodeString UName;
    FTerminal->ExceptionOnFail = true;
    try
    {
      try
      {
        AnyCommand(L"uname -a", NULL);
        for (int Index = 0; Index < Output->Count; Index++)
        {
          if (Index > 0)
          {
            UName += L"; ";
          }
          UName += Output->Strings[Index];
        }
      }
      catch(...)
      {
        if (!FTerminal->Active)
        {
          throw;
        }
      }
    }
    __finally
    {
      FTerminal->ExceptionOnFail = false;
    }

    FFileSystemInfo.RemoteSystem = UName;
  }

  return FFileSystemInfo;
}
//---------------------------------------------------------------------------
bool __fastcall TSCPFileSystem::TemporaryTransferFile(const UnicodeString & /*FileName*/)
{
  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TSCPFileSystem::GetStoredCredentialsTried()
{
  return FSecureShell->GetStoredCredentialsTried();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSCPFileSystem::GetUserName()
{
  return FSecureShell->UserName;
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::Idle()
{
  // Keep session alive
  if ((FTerminal->SessionData->PingType != ptOff) &&
      (Now() - FSecureShell->LastDataSent > FTerminal->SessionData->PingIntervalDT))
  {
    if ((FTerminal->SessionData->PingType == ptDummyCommand) &&
        FSecureShell->Ready)
    {
      if (!FProcessingCommand)
      {
        ExecCommand(fsNull, NULL, 0, 0);
      }
      else
      {
        FTerminal->LogEvent(L"Cannot send keepalive, command is being executed");
        // send at least SSH-level keepalive, if nothing else, it at least updates
        // LastDataSent, no the next keepalive attempt is postponed
        FSecureShell->KeepAlive();
      }
    }
    else
    {
      FSecureShell->KeepAlive();
    }
  }

  FSecureShell->Idle();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSCPFileSystem::AbsolutePath(UnicodeString Path, bool /*Local*/)
{
  return ::AbsolutePath(CurrentDirectory, Path);
}
//---------------------------------------------------------------------------
bool __fastcall TSCPFileSystem::IsCapable(int Capability) const
{
  DebugAssert(FTerminal);
  switch (Capability) {
    case fcUserGroupListing:
    case fcModeChanging:
    case fcModeChangingUpload:
    case fcPreservingTimestampUpload:
    case fcGroupChanging:
    case fcOwnerChanging:
    case fcAnyCommand:
    case fcShellAnyCommand:
    case fcHardLink:
    case fcSymbolicLink:
    case fcResolveSymlink:
    case fcRename:
    case fcRemoteMove:
    case fcRemoteCopy:
    case fcRemoveCtrlZUpload:
    case fcRemoveBOMUpload:
    case fcCalculatingChecksum:
    case fcMoveOverExistingFile:
      return true;

    case fcTextMode:
      return FTerminal->SessionData->EOLType != FTerminal->Configuration->LocalEOLType;

    case fcAclChangingFiles:
    case fcNativeTextMode:
    case fcNewerOnlyUpload:
    case fcTimestampChanging:
    case fcLoadingAdditionalProperties:
    case fcCheckingSpaceAvailable:
    case fcIgnorePermErrors:
    case fcSecondaryShell: // has fcShellAnyCommand
    case fcGroupOwnerChangingByID: // by name
    case fcMoveToQueue:
    case fcLocking:
    case fcPreservingTimestampDirs:
    case fcResumeSupport:
    case fcSkipTransfer:
    case fcParallelTransfers: // does not implement cpNoRecurse
    case fcParallelFileTransfers:
    case fcTransferOut:
    case fcTransferIn:
    case fcTags:
      return false;

    case fcChangePassword:
      return FSecureShell->CanChangePassword();

    default:
      DebugFail();
      return false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::EnsureLocation()
{
  if (!FCachedDirectoryChange.IsEmpty())
  {
    FTerminal->LogEvent(FORMAT(L"Locating to cached directory \"%s\".",
      (FCachedDirectoryChange)));
    UnicodeString Directory = FCachedDirectoryChange;
    FCachedDirectoryChange = L"";
    try
    {
      ChangeDirectory(Directory);
    }
    catch(...)
    {
      // when location to cached directory fails, pretend again
      // location in cached directory
      // here used to check (CurrentDirectory != Directory), but it is
      // false always (current directory is already set to cached directory),
      // making the condition below useless. check removed.
      if (FTerminal->Active)
      {
        FCachedDirectoryChange = Directory;
      }
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SendCommand(const UnicodeString & Cmd, bool NoEnsureLocation)
{
  if (!NoEnsureLocation)
  {
    EnsureLocation();
  }

  UnicodeString Line;
  FSecureShell->ClearStdError();
  FReturnCode = 0;
  FOutput->Clear();
  // We suppose, that 'Cmd' already contains command that ensures,
  // that 'LastLine' will be printed
  FSecureShell->SendLine(Cmd);
  FProcessingCommand = true;
}
//---------------------------------------------------------------------------
bool __fastcall TSCPFileSystem::IsTotalListingLine(const UnicodeString Line)
{
  // On some hosts there is not "total" but "totalt". What's the reason??
  // see mail from "Jan Wiklund (SysOp)" <jan@park.se>
  return !Line.SubString(1, 5).CompareIC(L"total");
}
//---------------------------------------------------------------------------
bool __fastcall TSCPFileSystem::RemoveLastLine(UnicodeString & Line,
    int & ReturnCode, UnicodeString LastLine)
{
  bool IsLastLine = false;
  if (LastLine.IsEmpty()) LastLine = LAST_LINE;
  // #55: fixed so, even when last line of command output does not
  // contain CR/LF, we can recognize last line
  int Pos = Line.Pos(LastLine);
  if (Pos)
  {
    // 2003-07-14: There must be nothing after return code number to
    // consider string as last line. This fixes bug with 'set' command
    // in console window
    UnicodeString ReturnCodeStr = Line.SubString(Pos + LastLine.Length() + 1,
      Line.Length() - Pos + LastLine.Length());
    if (TryStrToInt(ReturnCodeStr, ReturnCode))
    {
      IsLastLine = true;
      Line.SetLength(Pos - 1);
    }
  }
  return IsLastLine;
}
//---------------------------------------------------------------------------
bool __fastcall TSCPFileSystem::TryRemoveLastLine(UnicodeString & Line)
{
  bool Result = false;
  try
  {
    Result = RemoveLastLine(Line, FReturnCode, FCommandSet->LastLine);
  }
  catch (Exception &E)
  {
    FTerminal->TerminalError(&E, LoadStr(CANT_DETECT_RETURN_CODE));
  }
  return Result;
}
//---------------------------------------------------------------------------
bool TSCPFileSystem::IsLastLine(const UnicodeString & ALine)
{
  UnicodeString Line = ALine;
  return TryRemoveLastLine(Line);
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SkipFirstLine()
{
  UnicodeString Line = FSecureShell->ReceiveLine();
  if (Line != FCommandSet->FirstLine)
  {
    FTerminal->TerminalError(NULL, FMTLOAD(FIRST_LINE_EXPECTED, (Line)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ReadCommandOutput(int Params, const UnicodeString * Cmd)
{
  try
  {
    if (FLAGSET(Params, coWaitForLastLine))
    {
      UnicodeString Line;
      bool IsLast;
      unsigned int Total = 0;
      // #55: fixed so, even when last line of command output does not
      // contain CR/LF, we can recognize last line
      do
      {
        Line = FSecureShell->ReceiveLine();
        IsLast = TryRemoveLastLine(Line);
        if (!IsLast || !Line.IsEmpty())
        {
          FOutput->Add(Line);
          if (FLAGSET(Params, coReadProgress))
          {
            Total++;

            if (Total % 10 == 0)
            {
              bool Cancel; //dummy
              FTerminal->DoReadDirectoryProgress(Total, 0, Cancel);
            }
          }
        }
      }
      while (!IsLast);
    }

    if (FLAGSET(Params, coRaiseExcept))
    {
      UnicodeString Message = FSecureShell->GetStdError();
      if (FLAGSET(Params, coExpectNoOutput) && (FOutput->Count > 0))
      {
        AddToList(Message, FOutput->Text, L"\n");
      }

      while (!Message.IsEmpty() && (Message.LastDelimiter(L"\n\r") == Message.Length()))
      {
        Message.SetLength(Message.Length() - 1);
      }

      bool WrongReturnCode =
        (ReturnCode > 1) ||
        ((ReturnCode == 1) && FLAGCLEAR(Params, coIgnoreWarnings));

      if (FOnCaptureOutput != NULL)
      {
        FOnCaptureOutput(IntToStr(ReturnCode), cotExitCode);
      }

      if (FLAGSET(Params, coOnlyReturnCode))
      {
        if (WrongReturnCode)
        {
          FTerminal->TerminalError(FMTLOAD(COMMAND_FAILED_CODEONLY, (ReturnCode)));
        }
      }
      else
      {
        bool IsStdErrOnlyError = (FLAGCLEAR(Params, coIgnoreWarnings) && FLAGCLEAR(Params, coIgnoreStdErr));
        bool WrongOutput = !Message.IsEmpty() && ((FOutput->Count == 0) || IsStdErrOnlyError);
        if (WrongOutput || WrongReturnCode)
        {
          DebugAssert(Cmd != NULL);
          if (Message.IsEmpty())
          {
            FTerminal->TerminalError(FMTLOAD(COMMAND_FAILED_CODEONLY, (ReturnCode)));
          }
          else
          {
            throw ETerminal(MainInstructions(FMTLOAD(COMMAND_FAILED2, (*Cmd, ReturnCode))), Message);
          }
        }
      }
    }
  }
  __finally
  {
    FProcessingCommand = false;
  }
}
//---------------------------------------------------------------------------
NORETURN void TSCPFileSystem::InvalidOutputError(const UnicodeString & Command)
{
  FTerminal->TerminalError(FMTLOAD(INVALID_OUTPUT_ERROR, (Command, Output->Text)));
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ExecCommand(TFSCommand Cmd, const TVarRec * args,
  int size, int Params)
{
  if (Params < 0) Params = ecDefault;
  UnicodeString FullCommand = FCommandSet->FullCommand(Cmd, args, size);
  UnicodeString Command = FCommandSet->Command(Cmd, args, size);

  TOperationVisualizer Visualizer(FTerminal->UseBusyCursor);

  SendCommand(FullCommand, FLAGSET(Params, ecNoEnsureLocation));

  int COParams =
    coWaitForLastLine |
    FLAGMASK(FLAGSET(Params, ecRaiseExcept), coRaiseExcept) |
    FLAGMASK(FLAGSET(Params, ecIgnoreWarnings), coIgnoreWarnings) |
    FLAGMASK(FLAGSET(Params, ecReadProgress), coReadProgress) |
    FLAGMASK(FLAGSET(Params, ecIgnoreStdErr), coIgnoreStdErr);

  ReadCommandOutput(COParams, &Command);

  if (Params & ecRaiseExcept)
  {
    Integer MinL = FCommandSet->MinLines[Cmd];
    Integer MaxL = FCommandSet->MaxLines[Cmd];
    if (((MinL >= 0) && (MinL > FOutput->Count)) ||
        ((MaxL >= 0) && (MaxL > FOutput->Count)))
    {
      InvalidOutputError(FullCommand);
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSCPFileSystem::GetCurrentDirectory()
{
  return FCurrentDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::DoStartup()
{
  // Capabilities of SCP protocol are fixed
  FTerminal->SaveCapabilities(FFileSystemInfo);

  // SkipStartupMessage and DetectReturnVar must succeed,
  // otherwise session is to be closed.
  try
  {
    FTerminal->ExceptionOnFail = true;
    SkipStartupMessage();
    if (FTerminal->SessionData->DetectReturnVar) DetectReturnVar();
    FTerminal->ExceptionOnFail = false;
  }
  catch (Exception & E)
  {
    FTerminal->FatalError(&E, L"");
  }

  // Needs to be done before UnsetNationalVars()
  DetectUtf();

  #define COND_OPER(OPER) if (FTerminal->SessionData->OPER) OPER()
  COND_OPER(ClearAliases);
  COND_OPER(UnsetNationalVars);
  #undef COND_OPER
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::DetectUtf()
{
  switch (FTerminal->SessionData->NotUtf)
  {
    case asOn:
      FSecureShell->UtfStrings = false; // noop
      break;

    case asOff:
      FSecureShell->UtfStrings = true;
      break;

    default:
      DebugFail();
    case asAuto:
      FSecureShell->UtfStrings = false; // noop
      try
      {
        ExecCommand(fsLang, NULL, 0, false);

        if ((FOutput->Count >= 1) &&
            ContainsText(FOutput->Strings[0], L"UTF-8"))
        {
          FSecureShell->UtfStrings = true;
        }
      }
      catch (Exception &)
      {
        // ignore non-fatal errors
        if (!FTerminal->Active)
        {
          throw;
        }
      }
      break;
  }

  if (FSecureShell->UtfStrings)
  {
    FTerminal->LogEvent(L"We will use UTF-8");
  }
  else
  {
    FTerminal->LogEvent(L"We will not use UTF-8");
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SkipStartupMessage()
{
  try
  {
    FTerminal->LogEvent(L"Skipping host startup message (if any).");
    ExecCommand(fsNull, NULL, 0, 0);
  }
  catch (Exception & E)
  {
    FTerminal->CommandError(&E, LoadStr(SKIP_STARTUP_MESSAGE_ERROR), 0, HELP_SKIP_STARTUP_MESSAGE_ERROR);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::LookupUsersGroups()
{
  ExecCommand(fsLookupUsersGroups);
  FTerminal->FUsers.Clear();
  FTerminal->FGroups.Clear();
  if (FOutput->Count > 0)
  {
    UnicodeString Groups = FOutput->Strings[0];
    while (!Groups.IsEmpty())
    {
      UnicodeString NewGroup = CutToChar(Groups, L' ', false);
      FTerminal->FGroups.Add(TRemoteToken(NewGroup));
      FTerminal->FMembership.Add(TRemoteToken(NewGroup));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::DetectReturnVar()
{
  // This suppose that something was already executed (probably SkipStartupMessage())
  // or return code variable is already set on start up.

  try
  {
    // #60 17.10.01: "status" and "?" switched
    UnicodeString ReturnVars[2] = { L"status", L"?" };
    UnicodeString NewReturnVar = L"";
    FTerminal->LogEvent(L"Detecting variable containing return code of last command.");
    for (int Index = 0; Index < 2; Index++)
    {
      bool Success = true;

      try
      {
        FTerminal->LogEvent(FORMAT(L"Trying \"$%s\".", (ReturnVars[Index])));
        ExecCommand(fsVarValue, ARRAYOFCONST((ReturnVars[Index])));
        if ((Output->Count != 1) || (StrToIntDef(Output->Strings[0], 256) > 255))
        {
          FTerminal->LogEvent(L"The response is not numerical exit code");
          Abort();
        }
      }
      catch (EFatal &)
      {
        // if fatal error occurs, we need to exit ...
        throw;
      }
      catch (Exception &)
      {
        // ...otherwise, we will try next variable (if any)
        Success = false;
      }

      if (Success)
      {
        NewReturnVar = ReturnVars[Index];
        break;
      }
    }

    if (NewReturnVar.IsEmpty())
    {
      EXCEPTION;
    }
    else
    {
      FCommandSet->ReturnVar = NewReturnVar;
      FTerminal->LogEvent(FORMAT(L"Return code variable \"%s\" selected.",
        (FCommandSet->ReturnVar)));
    }
  }
  catch (Exception &E)
  {
    FTerminal->CommandError(&E, LoadStr(DETECT_RETURNVAR_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ClearAlias(UnicodeString Alias)
{
  if (!Alias.IsEmpty())
  {
    // this command usually fails, because there will never be
    // aliases on all commands -> see last False parameter
    ExecCommand(fsUnalias, ARRAYOFCONST((Alias)), false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ClearAliases()
{
  try
  {
    FTerminal->LogEvent(L"Clearing all aliases.");
    ClearAlias(TCommandSet::ExtractCommand(FTerminal->SessionData->ListingCommand));
    TStrings * CommandList = FCommandSet->CreateCommandList();
    try
    {
      for (int Index = 0; Index < CommandList->Count; Index++)
      {
        ClearAlias(CommandList->Strings[Index]);
      }
    }
    __finally
    {
      delete CommandList;
    }
  }
  catch (Exception &E)
  {
    FTerminal->CommandError(&E, LoadStr(UNALIAS_ALL_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::UnsetNationalVars()
{
  try
  {
    FTerminal->LogEvent(L"Clearing national user variables.");
    for (size_t Index = 0; Index < LENOF(NationalVars); Index++)
    {
      ExecCommand(fsUnset, ARRAYOFCONST((NationalVars[Index])), false);
    }
  }
  catch (Exception &E)
  {
    FTerminal->CommandError(&E, LoadStr(UNSET_NATIONAL_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ReadCurrentDirectory()
{
  if (FCachedDirectoryChange.IsEmpty())
  {
    ExecCommand(fsCurrentDirectory);
    FCurrentDirectory = UnixExcludeTrailingBackslash(FOutput->Strings[0]);
  }
  else
  {
    FCurrentDirectory = FCachedDirectoryChange;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::HomeDirectory()
{
  ExecCommand(fsHomeDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::AnnounceFileListOperation()
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ChangeDirectory(const UnicodeString ADirectory)
{
  int Params = ecDefault;
  UnicodeString Directory = ADirectory;
  try
  {
    EnsureLocation();
  }
  catch (...)
  {
    if (FTerminal->Active && DebugAlwaysTrue(!FCachedDirectoryChange.IsEmpty()))
    {
      Params |= ecNoEnsureLocation;
      Directory = ::AbsolutePath(AbsolutePath(FCachedDirectoryChange, true), Directory);
      FTerminal->LogEvent(
        FORMAT(L"Cannot locate to cached directory, assuming that target absolute path is \"%s\".", (Directory)));
    }
    else
    {
      throw;
    }
  }

  UnicodeString ToDir;
  // This effectively disallows entering subdirectories starting with ~ and containing space
  if (!Directory.IsEmpty() &&
      ((Directory[1] != L'~') || (Directory.SubString(1, 2) == L"~ ")))
  {
    ToDir = ShellQuoteStr(Directory);
  }
  else
  {
    ToDir = DelimitStr(Directory);
  }
  ExecCommand(fsChangeDirectory, ARRAYOFCONST((ToDir)), Params);
  FCachedDirectoryChange = L"";
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CachedChangeDirectory(const UnicodeString Directory)
{
  FCachedDirectoryChange = UnixExcludeTrailingBackslash(Directory);
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ReadDirectory(TRemoteFileList * FileList)
{
  DebugAssert(FileList);
  // emptying file list moved before command execution
  FileList->Reset();

  bool Again;

  do
  {
    Again = false;
    try
    {
      int Params = ecDefault | ecReadProgress |
        FLAGMASK(FTerminal->SessionData->IgnoreLsWarnings, ecIgnoreWarnings);
      const wchar_t * Options =
        ((FLsFullTime == asAuto) || (FLsFullTime == asOn)) ? FullTimeOption : L"";
      bool ListCurrentDirectory = (FileList->Directory == FTerminal->CurrentDirectory);
      if (ListCurrentDirectory)
      {
        FTerminal->LogEvent(L"Listing current directory.");
        ExecCommand(fsListCurrentDirectory,
          ARRAYOFCONST((FTerminal->SessionData->ListingCommand, Options)), Params);
      }
      else
      {
        FTerminal->LogEvent(FORMAT(L"Listing directory \"%s\".",
          (FileList->Directory)));
        ExecCommand(fsListDirectory,
          ARRAYOFCONST((FTerminal->SessionData->ListingCommand, Options,
            DelimitStr(FileList->Directory))),
          Params);
      }

      // If output is not empty, we have successfully got file listing,
      // otherwise there was an error, in case it was "permission denied"
      // we try to get at least parent directory (see "else" statement below)
      if (FOutput->Count > 0)
      {
        // Copy LS command output, because eventual symlink analysis would
        // modify FTerminal->Output
        TStringList * OutputCopy = new TStringList();
        try
        {
          OutputCopy->Assign(FOutput);

          // delete leading "total xxx" line
          // On some hosts there is not "total" but "totalt". What's the reason??
          // see mail from "Jan Wiklund (SysOp)" <jan@park.se>
          if (IsTotalListingLine(OutputCopy->Strings[0]))
          {
            OutputCopy->Delete(0);
          }

          for (int Index = 0; Index < OutputCopy->Count; Index++)
          {
            UnicodeString OutputLine = OutputCopy->Strings[Index];
            if (!OutputLine.IsEmpty())
            {
              std::unique_ptr<TRemoteFile> File(CreateRemoteFile(OutputCopy->Strings[Index]));
              if (FTerminal->IsValidFile(File.get()))
              {
                FileList->AddFile(File.release());
              }
            }
          }
        }
        __finally
        {
          delete OutputCopy;
        }
      }
      else
      {
        bool Empty;
        if (ListCurrentDirectory)
        {
          // Empty file list -> probably "permission denied", we
          // at least get link to parent directory ("..")
          TRemoteFile * File = FTerminal->ReadFile(UnixCombinePaths(FTerminal->FFiles->Directory, PARENTDIRECTORY));
          Empty = (File == NULL);
          if (!Empty)
          {
            DebugAssert(File->IsParentDirectory);
            FileList->AddFile(File);
          }
        }
        else
        {
          Empty = true;
        }

        if (Empty)
        {
          throw ExtException(
            NULL, FMTLOAD(EMPTY_DIRECTORY, (FileList->Directory)),
            HELP_EMPTY_DIRECTORY);
        }
      }

      if (FLsFullTime == asAuto)
      {
          FTerminal->LogEvent(
            FORMAT(L"Directory listing with %s succeed, next time all errors during "
              "directory listing will be displayed immediately.",
              (FullTimeOption)));
          FLsFullTime = asOn;
      }
    }
    catch(Exception & E)
    {
      if (FTerminal->Active)
      {
        if (FLsFullTime == asAuto)
        {
          FTerminal->Log->AddException(&E);
          FLsFullTime = asOff;
          Again = true;
          FTerminal->LogEvent(
            FORMAT(L"Directory listing with %s failed, try again regular listing.",
            (FullTimeOption)));
        }
        else
        {
          throw;
        }
      }
      else
      {
        throw;
      }
    }
  }
  while (Again);
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ReadSymlink(TRemoteFile * SymlinkFile,
  TRemoteFile *& File)
{
  CustomReadFile(SymlinkFile->LinkTo, File, SymlinkFile);
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ReadFile(const UnicodeString FileName,
  TRemoteFile *& File)
{
  CustomReadFile(FileName, File, NULL);
}
//---------------------------------------------------------------------------
TRemoteFile * __fastcall TSCPFileSystem::CreateRemoteFile(
  const UnicodeString & ListingStr, TRemoteFile * LinkedByFile)
{
  TRemoteFile * File = new TRemoteFile(LinkedByFile);
  try
  {
    File->Terminal = FTerminal;
    File->ListingStr = ListingStr;
    File->ShiftTimeInSeconds(TimeToSeconds(FTerminal->SessionData->TimeDifference));
    File->Complete();
  }
  catch(...)
  {
    delete File;
    throw;
  }

  return File;
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CustomReadFile(const UnicodeString FileName,
  TRemoteFile *& File, TRemoteFile * ALinkedByFile)
{
  File = NULL;
  int Params = ecDefault |
    FLAGMASK(FTerminal->SessionData->IgnoreLsWarnings, ecIgnoreWarnings);
  // the auto-detection of --full-time support is not implemented for fsListFile,
  // so we use it only if we already know that it is supported (asOn).
  const wchar_t * Options = (FLsFullTime == asOn) ? FullTimeOption : L"";
  ExecCommand(fsListFile,
    ARRAYOFCONST((FTerminal->SessionData->ListingCommand, Options, DelimitStr(FileName))),
    Params);
  if (FOutput->Count)
  {
    int LineIndex = 0;
    if (IsTotalListingLine(FOutput->Strings[LineIndex]) && FOutput->Count > 1)
    {
      LineIndex++;
    }

    File = CreateRemoteFile(FOutput->Strings[LineIndex], ALinkedByFile);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::DeleteFile(const UnicodeString FileName,
  const TRemoteFile * File, int Params, TRmSessionAction & Action)
{
  DebugUsedParam(File);
  DebugUsedParam(Params);
  Action.Recursive();
  DebugAssert(FLAGCLEAR(Params, dfNoRecursive) || (File && File->IsSymLink));
  ExecCommand(fsDeleteFile, ARRAYOFCONST((DelimitStr(FileName))));
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::RenameFile(
  const UnicodeString & FileName, const TRemoteFile *, const UnicodeString & NewName, bool DebugUsedArg(Overwrite))
{
  ExecCommand(fsRenameFile, ARRAYOFCONST((DelimitStr(FileName), DelimitStr(NewName))));
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CopyFile(
  const UnicodeString & FileName, const TRemoteFile *, const UnicodeString & NewName, bool DebugUsedArg(Overwrite))
{
  UnicodeString DelimitedFileName = DelimitStr(FileName);
  UnicodeString DelimitedNewName = DelimitStr(NewName);
  const UnicodeString AdditionalSwitches = L"-T";
  try
  {
    ExecCommand(fsCopyFile, ARRAYOFCONST((AdditionalSwitches, DelimitedFileName, DelimitedNewName)));
  }
  catch (Exception &)
  {
    if (FTerminal->Active)
    {
      // The -T is GNU switch and may not be available on all platforms.
      // https://lists.gnu.org/archive/html/bug-coreutils/2004-07/msg00000.html
      FTerminal->LogEvent(FORMAT(L"Attempt with %s failed, trying without", (AdditionalSwitches)));
      ExecCommand(fsCopyFile, ARRAYOFCONST((L"", DelimitedFileName, DelimitedNewName)));
    }
    else
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CreateDirectory(const UnicodeString & DirName, bool /*Encrypt*/)
{
  ExecCommand(fsCreateDirectory, ARRAYOFCONST((DelimitStr(DirName))));
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CreateLink(const UnicodeString FileName,
  const UnicodeString PointTo, bool Symbolic)
{
  ExecCommand(fsCreateLink,
    ARRAYOFCONST((Symbolic ? L"-s" : L"", DelimitStr(PointTo), DelimitStr(FileName))));
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ChangeFileToken(const UnicodeString & DelimitedName,
  const TRemoteToken & Token, TFSCommand Cmd, const UnicodeString & RecursiveStr)
{
  UnicodeString Str;
  if (Token.IDValid)
  {
    Str = IntToStr(int(Token.ID));
  }
  else if (Token.NameValid)
  {
    Str = Token.Name;
  }

  if (!Str.IsEmpty())
  {
    ExecCommand(Cmd, ARRAYOFCONST((RecursiveStr, Str, DelimitedName)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ChangeFileProperties(const UnicodeString FileName,
  const TRemoteFile * File, const TRemoteProperties * Properties,
  TChmodSessionAction & Action)
{
  DebugAssert(Properties);
  int Directory = ((File == NULL) ? -1 : (File->IsDirectory ? 1 : 0));
  bool IsDirectory = (Directory > 0);
  bool Recursive = Properties->Recursive && IsDirectory;
  UnicodeString RecursiveStr = Recursive ? L"-R" : L"";

  UnicodeString DelimitedName = DelimitStr(FileName);
  // change group before permissions as chgrp change permissions
  if (Properties->Valid.Contains(vpGroup))
  {
    ChangeFileToken(DelimitedName, Properties->Group, fsChangeGroup, RecursiveStr);
  }
  if (Properties->Valid.Contains(vpOwner))
  {
    ChangeFileToken(DelimitedName, Properties->Owner, fsChangeOwner, RecursiveStr);
  }
  if (Properties->Valid.Contains(vpRights))
  {
    TRights Rights = Properties->Rights;

    // if we don't set modes recursively, we may add X at once with other
    // options. Otherwise we have to add X after recursive command
    if (!Recursive && IsDirectory && Properties->AddXToDirectories)
      Rights.AddExecute();

    Action.Rights(Rights);
    if (Recursive)
    {
      Action.Recursive();
    }

    if ((Rights.NumberSet | Rights.NumberUnset) != TRights::rfNo)
    {
      ExecCommand(fsChangeMode,
        ARRAYOFCONST((RecursiveStr, Rights.GetChmodStr(Directory), DelimitedName)));
    }

    // if file is directory and we do recursive mode settings with
    // add-x-to-directories option on, add those X
    if (Recursive && IsDirectory && Properties->AddXToDirectories)
    {
      Rights.AddExecute();
      ExecCommand(fsChangeMode,
        ARRAYOFCONST((L"", Rights.GetChmodStr(Directory), DelimitedName)));
    }
  }
  else
  {
    Action.Cancel();
  }
  DebugAssert(!Properties->Valid.Contains(vpLastAccess));
  DebugAssert(!Properties->Valid.Contains(vpModification));
}
//---------------------------------------------------------------------------
bool __fastcall TSCPFileSystem::LoadFilesProperties(TStrings * /*FileList*/ )
{
  DebugFail();
  return false;
}
//---------------------------------------------------------------------------
UnicodeString TSCPFileSystem::CalculateFilesChecksumInitialize(const UnicodeString & Alg)
{
  std::unique_ptr<TStrings> Algs(new TStringList());
  GetSupportedChecksumAlgs(Algs.get());
  return FindIdent(Alg, Algs.get());
}
//---------------------------------------------------------------------------
UnicodeString TSCPFileSystem::ParseFileChecksum(
  const UnicodeString & Line, const UnicodeString & FileName, const UnicodeString & Command)
{
  int P = Line.Pos(L" ");
  if ((P < 0) ||
      (P == Line.Length()) ||
      ((Line[P + 1] != L' ') && (Line[P + 1] != L'*')) ||
      (Line.SubString(P + 2, Line.Length() - P - 1) != FileName))
  {
    InvalidOutputError(Command);
  }
  return Line.SubString(1, P - 1);
}
//---------------------------------------------------------------------------
void TSCPFileSystem::ProcessFileChecksum(
  TCalculatedChecksumEvent OnCalculatedChecksum, TChecksumSessionAction & Action, TFileOperationProgressType * OperationProgress,
  bool FirstLevel, const UnicodeString & FileName, const UnicodeString & Alg, const UnicodeString & Checksum)
{
  bool Success = !Checksum.IsEmpty();
  if (Success)
  {
    if (OnCalculatedChecksum != NULL)
    {
      OnCalculatedChecksum(UnixExtractFileName(FileName), Alg, Checksum);
    }
    Action.Checksum(Alg, Checksum);
  }
  if (FirstLevel)
  {
    TOnceDoneOperation OnceDoneOperation; // not used
    OperationProgress->Finish(FileName, Success, OnceDoneOperation);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CalculateFilesChecksum(
  const UnicodeString & Alg, TStrings * FileList, TCalculatedChecksumEvent OnCalculatedChecksum,
  TFileOperationProgressType * OperationProgress, bool FirstLevel)
{
  FTerminal->CalculateSubFoldersChecksum(Alg, FileList, OnCalculatedChecksum, OperationProgress, FirstLevel);

  TStrings * AlgDefs = FTerminal->GetShellChecksumAlgDefs();
  int AlgIndex = AlgDefs->IndexOfName(Alg);
  UnicodeString AlgCommand = (AlgIndex >= 0) ? AlgDefs->ValueFromIndex[AlgIndex] : Alg;

  int Index = 0;
  while ((Index < FileList->Count) && !OperationProgress->Cancel)
  {
    std::unique_ptr<TStrings> BatchFileList(new TStringList());
    UnicodeString FileListCommandLine;
    __int64 BatchSize = 0;
    while (Index < FileList->Count)
    {
      TRemoteFile * File = DebugNotNull(dynamic_cast<TRemoteFile *>(FileList->Objects[Index]));
      if (!File->IsDirectory)
      {
        UnicodeString FileName = FileList->Strings[Index];
        UnicodeString FileListCommandLineBak = FileListCommandLine;
        AddToShellFileListCommandLine(FileListCommandLine, FileName);
        BatchSize += File->Size;
        if (!FileListCommandLineBak.IsEmpty() &&
            ((FileListCommandLine.Length() > 2048) ||
             (BatchSize > 1024*1024*1024)))
        {
          FileListCommandLine = FileListCommandLineBak;
          break;
        }
        else
        {
          BatchFileList->AddObject(FileName, File);
        }
      }
      Index++;
    }

    if (!FileListCommandLine.IsEmpty())
    {
      bool IndividualCommands = false;
      std::unique_ptr<TStrings> BatchChecksums(new TStringList());
      try
      {
        UnicodeString BatchCommand = FORMAT(L"%s %s", (AlgCommand, FileListCommandLine));
        AnyCommand(BatchCommand, NULL);
        if (Output->Count != BatchFileList->Count)
        {
          InvalidOutputError(BatchCommand);
        }

        // First do everything that can throw.
        // Only once everything is checked, distribute the results.
        for (int BatchIndex = 0; BatchIndex < BatchFileList->Count; BatchIndex++)
        {
          UnicodeString FileName = BatchFileList->Strings[BatchIndex];
          UnicodeString Line = Output->Strings[BatchIndex];
          UnicodeString Checksum = ParseFileChecksum(Line, FileName, BatchCommand);
          BatchChecksums->Add(Checksum);
        }
      }
      catch (Exception &)
      {
        if (!FTerminal->Active)
        {
          throw;
        }
        else
        {
          IndividualCommands = true;
        }
      }

      if (!IndividualCommands)
      {
        // None of this should throw
        for (int BatchIndex = 0; BatchIndex < BatchFileList->Count; BatchIndex++)
        {
          UnicodeString FileName = BatchFileList->Strings[BatchIndex];
          TRemoteFile * File = DebugNotNull(dynamic_cast<TRemoteFile *>(BatchFileList->Objects[BatchIndex]));
          TChecksumSessionAction Action(FTerminal->ActionLog);
          Action.FileName(File->FullFileName);
          OperationProgress->SetFile(FileName);
          UnicodeString Checksum = BatchChecksums->Strings[BatchIndex];
          ProcessFileChecksum(OnCalculatedChecksum, Action, OperationProgress, FirstLevel, FileName, Alg, Checksum);
        }
      }
      else
      {
        FTerminal->LogEvent(
          L"Batch checksum calculation failed, falling back to calculating checksum individually for each file...");

        for (int BatchIndex = 0; BatchIndex < BatchFileList->Count; BatchIndex++)
        {
          TRemoteFile * File = DebugNotNull(dynamic_cast<TRemoteFile *>(BatchFileList->Objects[BatchIndex]));
          TChecksumSessionAction Action(FTerminal->ActionLog);
          try
          {
            UnicodeString FileName = BatchFileList->Strings[BatchIndex];
            OperationProgress->SetFile(FileName);
            Action.FileName(File->FullFileName);

            UnicodeString Checksum;
            try
            {
              UnicodeString Command = FORMAT(L"%s %s", (AlgCommand, ShellQuoteStr(FileName)));
              AnyCommand(Command, NULL);
              if (Output->Count != 1)
              {
                InvalidOutputError(Command);
              }
              UnicodeString Line = Output->Strings[0];
              Checksum = ParseFileChecksum(Line, FileName, Command);
            }
            __finally
            {
              ProcessFileChecksum(OnCalculatedChecksum, Action, OperationProgress, FirstLevel, FileName, Alg, Checksum);
            }
          }
          catch (Exception & E)
          {
            FTerminal->RollbackAction(Action, OperationProgress, &E);

            UnicodeString Error = FMTLOAD(CHECKSUM_ERROR, (File->FullFileName));
            FTerminal->CommandError(&E, Error);
            // Abort loop.
            // TODO: retries? resume?
            BatchIndex = BatchFileList->Count;
            Index = FileList->Count;
          }
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CustomCommandOnFile(const UnicodeString FileName,
    const TRemoteFile * File, UnicodeString Command, int Params,
    TCaptureOutputEvent OutputEvent)
{
  DebugAssert(File);
  bool Dir = File->IsDirectory && FTerminal->CanRecurseToDirectory(File);
  if (Dir && (Params & ccRecursive))
  {
    TCustomCommandParams AParams;
    AParams.Command = Command;
    AParams.Params = Params;
    AParams.OutputEvent = OutputEvent;
    FTerminal->ProcessDirectory(FileName, FTerminal->CustomCommandOnFile,
      &AParams);
  }

  if (!Dir || (Params & ccApplyToDirectories))
  {
    TCustomCommandData Data(FTerminal);
    UnicodeString Cmd = TRemoteCustomCommand(
      Data, FTerminal->CurrentDirectory, FileName, L"").
      Complete(Command, true);

    if (!FTerminal->DoOnCustomCommand(Cmd))
    {
      AnyCommand(Cmd, OutputEvent);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CaptureOutput(const UnicodeString & AddedLine, TCaptureOutputType OutputType)
{
  int ReturnCode;
  UnicodeString Line = AddedLine;
  // TSecureShell never uses cotExitCode
  DebugAssert((OutputType == cotOutput) || (OutputType == cotError));
  if ((OutputType == cotError) || DebugAlwaysFalse(OutputType == cotExitCode) ||
      !RemoveLastLine(Line, ReturnCode) ||
      !Line.IsEmpty())
  {
    DebugAssert(FOnCaptureOutput != NULL);
    FOnCaptureOutput(Line, OutputType);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::AnyCommand(const UnicodeString Command,
  TCaptureOutputEvent OutputEvent)
{
  DebugAssert(FSecureShell->OnCaptureOutput == NULL);
  if (OutputEvent != NULL)
  {
    FSecureShell->OnCaptureOutput = CaptureOutput;
    FOnCaptureOutput = OutputEvent;
  }

  try
  {
    int Params =
      ecDefault |
      (FTerminal->SessionData->ExitCode1IsError ? ecIgnoreStdErr : ecIgnoreWarnings);

    ExecCommand(fsAnyCommand, ARRAYOFCONST((Command)), Params);
  }
  __finally
  {
    FOnCaptureOutput = NULL;
    FSecureShell->OnCaptureOutput = NULL;
  }
}
//---------------------------------------------------------------------------
TStrings * __fastcall TSCPFileSystem::GetFixedPaths()
{
  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SpaceAvailable(const UnicodeString Path,
  TSpaceAvailable & /*ASpaceAvailable*/)
{
  DebugFail();
  DebugUsedParam(Path);
}
//---------------------------------------------------------------------------
// transfer protocol
//---------------------------------------------------------------------------
unsigned int __fastcall TSCPFileSystem::ConfirmOverwrite(
  const UnicodeString & SourceFullFileName, const UnicodeString & TargetFileName, TOperationSide Side,
  const TOverwriteFileParams * FileParams, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress)
{
  TSuspendFileOperationProgress Suspend(OperationProgress);

  TQueryButtonAlias Aliases[3];
  Aliases[0] = TQueryButtonAlias::CreateAllAsYesToNewerGrouppedWithYes();
  Aliases[1] = TQueryButtonAlias::CreateYesToAllGrouppedWithYes();
  Aliases[2] = TQueryButtonAlias::CreateNoToAllGrouppedWithNo();
  TQueryParams QueryParams(qpNeverAskAgainCheck);
  QueryParams.Aliases = Aliases;
  QueryParams.AliasesCount = LENOF(Aliases);
  unsigned int Answer =
    FTerminal->ConfirmFileOverwrite(
      SourceFullFileName, TargetFileName, FileParams,
      qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll | qaAll,
      &QueryParams, Side, CopyParam, Params, OperationProgress);
  return Answer;
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SCPResponse(bool * GotLastLine)
{
  // Taken from scp.c response() and modified

  unsigned char Resp;
  FSecureShell->Receive(&Resp, 1);

  switch (Resp)
  {
    case 0:     /* ok */
      FTerminal->LogEvent(L"SCP remote side confirmation (0)");
      return;

    default:
    case 1:     /* error */
    case 2:     /* fatal error */
      // pscp adds 'Resp' to 'Msg', why?
      UnicodeString Msg = FSecureShell->ReceiveLine();
      UnicodeString Line = UnicodeString(static_cast<char>(Resp)) + Msg;
      if (TryRemoveLastLine(Line))
      {
        if (GotLastLine != NULL)
        {
          *GotLastLine = true;
        }

        /* TODO 1 : Show stderror to user? */
        FSecureShell->ClearStdError();

        try
        {
          ReadCommandOutput(coExpectNoOutput | coRaiseExcept | coOnlyReturnCode);
        }
        catch(...)
        {
          // when ReadCommandOutput() fails than remote SCP is terminated already
          if (GotLastLine != NULL)
          {
            *GotLastLine = true;
          }
          throw;
        }
      }
      else if (Resp == 1)
      {
        // While the OpenSSH scp client distinguishes the 1 for error and 2 for fatal errors,
        // the OpenSSH scp server always sends 1 even for fatal errors. Using the error message to tell
        // which errors are fatal and which are not.
        // This error list is valid for OpenSSH 5.3p1 and 7.2p2
        if (SameText(Msg, L"scp: ambiguous target") ||
            StartsText(L"scp: error: unexpected filename: ", Msg) ||
            StartsText(L"scp: protocol error: ", Msg))
        {
          FTerminal->LogEvent(L"SCP remote side error (1), fatal error detected from error message");
          Resp = 2;
          FScpFatalError = true;
        }
        else
        {
          FTerminal->LogEvent(L"SCP remote side error (1)");
        }
      }
      else
      {
        FTerminal->LogEvent(L"SCP remote side fatal error (2)");
        FScpFatalError = true;
      }

      if (Resp == 1)
      {
        throw EScpFileSkipped(NULL, Msg);
      }
      else
      {
        throw EScp(NULL, Msg);
      }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CopyToRemote(TStrings * FilesToCopy,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  // scp.c: source(), toremote()
  DebugAssert(FilesToCopy && OperationProgress);

  Params &= ~(cpAppend | cpResume);
  UnicodeString Options = L"";
  bool CheckExistence = UnixSamePath(TargetDir, FTerminal->CurrentDirectory) &&
    (FTerminal->FFiles != NULL) && FTerminal->FFiles->Loaded;
  bool CopyBatchStarted = false;
  bool Failed = true;
  bool GotLastLine = false;

  UnicodeString TargetDirFull = UnixIncludeTrailingBackslash(TargetDir);

  if (CopyParam->PreserveRights) Options = L"-p";
  if (FTerminal->SessionData->Scp1Compatibility) Options += L" -1";

  FScpFatalError = false;
  SendCommand(FCommandSet->FullCommand(fsCopyToRemote,
    ARRAYOFCONST((Options, DelimitStr(UnixExcludeTrailingBackslash(TargetDir))))));
  SkipFirstLine();

  try
  {
    try
    {
      SCPResponse(&GotLastLine);

      // This can happen only if SCP command is not executed and return code is 0
      // It has never happened to me (return code is usually 127)
      if (GotLastLine)
      {
        throw Exception(L"");
      }
    }
    catch(Exception & E)
    {
      if (GotLastLine && FTerminal->Active)
      {
        FTerminal->TerminalError(&E, LoadStr(SCP_INIT_ERROR));
      }
      else
      {
        throw;
      }
    }
    CopyBatchStarted = true;

    for (int IFile = 0; (IFile < FilesToCopy->Count) &&
      !OperationProgress->Cancel; IFile++)
    {
      UnicodeString FileName = FilesToCopy->Strings[IFile];
      bool CanProceed;

      UnicodeString FileNameOnly =
        FTerminal->ChangeFileName(
          CopyParam, ExtractFileName(FileName), osLocal, true);

      if (CheckExistence)
      {
        // previously there was assertion on FTerminal->FFiles->Loaded, but it
        // fails for scripting, if 'ls' is not issued before.
        // formally we should call CheckRemoteFile here but as checking is for
        // free here (almost) ...
        TRemoteFile * File = FTerminal->FFiles->FindFile(FileNameOnly);
        if (File != NULL)
        {
          unsigned int Answer;
          if (File->IsDirectory)
          {
            UnicodeString Message = MainInstructions(FMTLOAD(DIRECTORY_OVERWRITE, (FileNameOnly)));
            TQueryParams QueryParams(qpNeverAskAgainCheck);

            TSuspendFileOperationProgress Suspend(OperationProgress);
            Answer = FTerminal->ConfirmFileOverwrite(
              FileName, FileNameOnly, NULL,
              qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll,
              &QueryParams, osRemote, CopyParam, Params, OperationProgress, Message);
          }
          else
          {
            __int64 MTime;
            TOverwriteFileParams FileParams;
            FTerminal->OpenLocalFile(FileName, GENERIC_READ,
              NULL, NULL, NULL, &MTime, NULL,
              &FileParams.SourceSize);
            FileParams.SourceTimestamp = UnixToDateTime(MTime,
              FTerminal->SessionData->DSTMode);
            FileParams.DestSize = File->Size;
            FileParams.DestTimestamp = File->Modification;

            Answer = ConfirmOverwrite(FileName, FileNameOnly, osRemote,
              &FileParams, CopyParam, Params, OperationProgress);
          }

          switch (Answer)
          {
            case qaYes:
              CanProceed = true;
              break;

            case qaCancel:
              OperationProgress->SetCancelAtLeast(csCancel);
            case qaNo:
              CanProceed = false;
              break;

            default:
              CanProceed = false; // shut up
              DebugFail();
              break;
          }
        }
        else
        {
          CanProceed = true;
        }
      }
      else
      {
        CanProceed = true;
      }

      if (CanProceed)
      {
        if (FTerminal->SessionData->CacheDirectories)
        {
          FTerminal->DirectoryModified(TargetDir, false);

          if (DirectoryExists(ApiPath(FileName)))
          {
            FTerminal->DirectoryModified(UnixIncludeTrailingBackslash(TargetDir)+
              FileNameOnly, true);
          }
        }

        void * Item = static_cast<void *>(FilesToCopy->Objects[IFile]);

        try
        {
          SCPSource(FileName, TargetDirFull,
            CopyParam, Params, OperationProgress, 0);
          FTerminal->OperationFinish(OperationProgress, Item, FileName, true, OnceDoneOperation);
        }
        catch (EScpFileSkipped &E)
        {
          TQueryParams Params(qpAllowContinueOnError);

          TSuspendFileOperationProgress Suspend(OperationProgress);

          if (FTerminal->QueryUserException(FMTLOAD(COPY_ERROR, (FileName)), &E,
            qaOK | qaAbort, &Params, qtError) == qaAbort)
          {
            OperationProgress->SetCancel(csCancel);
          }
          FTerminal->OperationFinish(OperationProgress, Item, FileName, false, OnceDoneOperation);
          if (!FTerminal->HandleException(&E))
          {
            throw;
          }
        }
        catch (ESkipFile &E)
        {
          FTerminal->OperationFinish(OperationProgress, Item, FileName, false, OnceDoneOperation);

          {
            TSuspendFileOperationProgress Suspend(OperationProgress);
            // If ESkipFile occurs, just log it and continue with next file
            if (!FTerminal->HandleException(&E))
            {
              throw;
            }
          }
        }
        catch (...)
        {
          FTerminal->OperationFinish(OperationProgress, Item, FileName, false, OnceDoneOperation);
          throw;
        }
      }
    }
    Failed = false;
  }
  __finally
  {
    // Tell remote side, that we're done.
    if (FTerminal->Active)
    {
      try
      {
        if (!GotLastLine)
        {
          if (CopyBatchStarted && !FScpFatalError)
          {
            // What about case, remote side sends fatal error ???
            // (Not sure, if it causes remote side to terminate scp)
            FSecureShell->SendLine(L"E");
            SCPResponse();
          }
          /* TODO 1 : Show stderror to user? */
          FSecureShell->ClearStdError();

          ReadCommandOutput(coExpectNoOutput | coWaitForLastLine | coOnlyReturnCode |
            (Failed ? 0 : coRaiseExcept));
        }
      }
      catch (Exception &E)
      {
        // Only log error message (it should always succeed, but
        // some pending error maybe in queue) }
        FTerminal->Log->AddException(&E);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::Source(
  TLocalFileHandle & /*Handle*/, const UnicodeString & /*TargetDir*/, UnicodeString & /*DestFileName*/,
  const TCopyParamType * /*CopyParam*/, int /*Params*/,
  TFileOperationProgressType * /*OperationProgress*/, unsigned int /*Flags*/,
  TUploadSessionAction & /*Action*/, bool & /*ChildError*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SCPSource(const UnicodeString FileName,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, int Level)
{
  UnicodeString DestFileName =
    FTerminal->ChangeFileName(
      CopyParam, ExtractFileName(FileName), osLocal, Level == 0);

  FTerminal->LogEvent(FORMAT(L"File: \"%s\"", (FileName)));

  OperationProgress->SetFile(FileName, false);

  if (!FTerminal->AllowLocalFileTransfer(FileName, NULL, CopyParam, OperationProgress))
  {
    throw ESkipFile();
  }

  TLocalFileHandle Handle;
  FTerminal->OpenLocalFile(FileName, GENERIC_READ, Handle);

  OperationProgress->SetFileInProgress();

  if (Handle.Directory)
  {
    SCPDirectorySource(FileName, TargetDir, CopyParam, Params, OperationProgress, Level);
  }
  else
  {
    UnicodeString AbsoluteFileName = FTerminal->AbsolutePath(TargetDir + DestFileName, false);

    DebugAssert(Handle.Handle);
    std::unique_ptr<TStream> Stream(new TSafeHandleStream((THandle)Handle.Handle));

    // File is regular file (not directory)
    FTerminal->LogEvent(FORMAT(L"Copying \"%s\" to remote directory started.", (FileName)));

    OperationProgress->SetLocalSize(Handle.Size);

    // Suppose same data size to transfer as to read
    // (not true with ASCII transfer)
    OperationProgress->SetTransferSize(OperationProgress->LocalSize);
    OperationProgress->SetTransferringFile(false);

    if (Handle.Size > 512*1024*1024)
    {
      OperationProgress->SetAsciiTransfer(false);
      FTerminal->LogEvent(FORMAT(L"Binary transfer mode selected as the file is too large (%s) to be uploaded in Ascii mode using SCP protocol.", (IntToStr(Handle.Size))));
    }
    else
    {
      FTerminal->SelectSourceTransferMode(Handle, CopyParam);
    }

    TUploadSessionAction Action(FTerminal->ActionLog);
    Action.FileName(ExpandUNCFileName(FileName));
    Action.Destination(AbsoluteFileName);

    TRights Rights = CopyParam->RemoteFileRights(Handle.Attrs);

    try
    {
      TValueRestorer<TSecureShellMode> SecureShellModeRestorer(FSecureShell->Mode, ssmUploading);

      // During ASCII transfer we will load whole file to this buffer
      // than convert EOL and send it at once, because before converting EOL
      // we can't know its size
      TFileBuffer AsciiBuf;
      bool ConvertToken = false;
      do
      {
        // Buffer for one block of data
        TFileBuffer BlockBuf;

        // This is crucial, if it fails during file transfer, it's fatal error
        FILE_OPERATION_LOOP_BEGIN
        {
          BlockBuf.LoadStream(Stream.get(), OperationProgress->LocalBlockSize(), true);
        }
        FILE_OPERATION_LOOP_END_EX(
          FMTLOAD(READ_ERROR, (FileName)),
          FLAGMASK(!OperationProgress->TransferringFile, folAllowSkip));

        OperationProgress->AddLocallyUsed(BlockBuf.Size);

        // We do ASCII transfer: convert EOL of current block
        // (we don't convert whole buffer, cause it would produce
        // huge memory-transfers while inserting/deleting EOL characters)
        // Then we add current block to file buffer
        if (OperationProgress->AsciiTransfer)
        {
          int ConvertParams =
            FLAGMASK(CopyParam->RemoveCtrlZ, cpRemoveCtrlZ) |
            FLAGMASK(CopyParam->RemoveBOM, cpRemoveBOM);
          BlockBuf.Convert(FTerminal->Configuration->LocalEOLType,
            FTerminal->SessionData->EOLType,
            ConvertParams, ConvertToken);
          BlockBuf.Memory->Seek(0, soFromBeginning);
          AsciiBuf.ReadStream(BlockBuf.Memory, BlockBuf.Size, true);
          // We don't need it any more
          BlockBuf.Memory->Clear();
          // Calculate total size to sent (assume that ratio between
          // size of source and size of EOL-transformed data would remain same)
          // First check if file contains anything (div by zero!)
          if (OperationProgress->LocallyUsed)
          {
            __int64 X = OperationProgress->LocalSize;
            X *= AsciiBuf.Size;
            X /= OperationProgress->LocallyUsed;
            OperationProgress->ChangeTransferSize(X);
          }
            else
          {
            OperationProgress->ChangeTransferSize(0);
          }
        }

        // We send file information on first pass during BINARY transfer
        // and on last pass during ASCII transfer
        // BINARY: We succeeded reading first buffer from file, hopefully
        // we will be able to read whole, so we send file info to remote side
        // This is done, because when reading fails we can't interrupt sending
        // (don't know how to tell other side that it failed)
        if (!OperationProgress->TransferringFile &&
            (!OperationProgress->AsciiTransfer || OperationProgress->IsLocallyDone()))
        {
          UnicodeString Buf;

          if (CopyParam->PreserveTime)
          {
            // Send last file access and modification time
            // TVarRec don't understand 'unsigned int' -> we use sprintf()
            Buf.sprintf(L"T%lu 0 %lu 0", static_cast<unsigned long>(Handle.MTime),
              static_cast<unsigned long>(Handle.ATime));
            FSecureShell->SendLine(Buf);
            SCPResponse();
          }

          // Send file modes (rights), filesize and file name
          // TVarRec don't understand 'unsigned int' -> we use sprintf()
          Buf.sprintf(L"C%s %Ld %s",
            Rights.Octal.data(),
            (OperationProgress->AsciiTransfer ? (__int64)AsciiBuf.Size :
              OperationProgress->LocalSize),
            DestFileName.data());
          FSecureShell->SendLine(Buf);
          SCPResponse();
          // Indicate we started transferring file, we need to finish it
          // If not, it's fatal error
          OperationProgress->SetTransferringFile(true);

          // If we're doing ASCII transfer, this is last pass
          // so we send whole file
          /* TODO : We can't send file above 32bit size in ASCII mode! */
          if (OperationProgress->AsciiTransfer)
          {
            FTerminal->LogEvent(FORMAT(L"Sending ASCII data (%u bytes)",
              (AsciiBuf.Size)));
            // Should be equal, just in case it's rounded (see above)
            OperationProgress->ChangeTransferSize(AsciiBuf.Size);
            while (!OperationProgress->IsTransferDoneChecked())
            {
              unsigned long BlockSize = OperationProgress->TransferBlockSize();
              FSecureShell->Send(
                reinterpret_cast<unsigned char *>(AsciiBuf.Data + (unsigned int)OperationProgress->TransferredSize),
                BlockSize);
              OperationProgress->AddTransferred(BlockSize);
              if (OperationProgress->Cancel == csCancelTransfer)
              {
                throw Exception(MainInstructions(LoadStr(USER_TERMINATED)));
              }
            }
          }
        }

        // At end of BINARY transfer pass, send current block
        if (!OperationProgress->AsciiTransfer)
        {
          if (!OperationProgress->TransferredSize)
          {
            FTerminal->LogEvent(FORMAT(L"Sending BINARY data (first block, %u bytes)",
              (BlockBuf.Size)));
          }
          else if (FTerminal->Configuration->ActualLogProtocol >= 1)
          {
            FTerminal->LogEvent(FORMAT(L"Sending BINARY data (%u bytes)",
              (BlockBuf.Size)));
          }
          FSecureShell->Send(reinterpret_cast<const unsigned char *>(BlockBuf.Data), BlockBuf.Size);
          OperationProgress->AddTransferred(BlockBuf.Size);
        }

        if ((OperationProgress->Cancel == csCancelTransfer) ||
            (OperationProgress->Cancel == csCancel && !OperationProgress->TransferringFile))
        {
          throw Exception(MainInstructions(LoadStr(USER_TERMINATED)));
        }
      }
      while (!OperationProgress->IsLocallyDone() || !OperationProgress->IsTransferDoneChecked());

      FSecureShell->SendNull();
      try
      {
        SCPResponse();
        // If one of two following exceptions occurs, it means, that remote
        // side already know, that file transfer finished, even if it failed
        // so we don't have to throw EFatal
      }
      catch (EScp &)
      {
        // SCP protocol fatal error
        OperationProgress->SetTransferringFile(false);
        throw;
      }
      catch (EScpFileSkipped &)
      {
        // SCP protocol non-fatal error
        OperationProgress->SetTransferringFile(false);
        throw;
      }

      // We succeeded transferring file, from now we can handle exceptions
      // normally -> no fatal error
      OperationProgress->SetTransferringFile(false);
    }
    catch (Exception &E)
    {
      // EScpFileSkipped is derived from ESkipFile,
      // but is does not indicate file skipped by user here
      if (dynamic_cast<EScpFileSkipped *>(&E) != NULL)
      {
        Action.Rollback(&E);
      }
      else
      {
        FTerminal->RollbackAction(Action, OperationProgress, &E);
      }

      // Every exception during file transfer is fatal
      if (OperationProgress->TransferringFile)
      {
        FTerminal->FatalError(&E, FMTLOAD(COPY_FATAL, (FileName)));
      }
      else
      {
        throw;
      }
    }

    // With SCP we are not able to distinguish reason for failure
    // (upload itself, touch or chmod).
    // So we always report error with upload action and
    // log touch and chmod actions only if upload succeeds.
    if (CopyParam->PreserveTime)
    {
      TTouchSessionAction(FTerminal->ActionLog, AbsoluteFileName, Handle.Modification);
    }
    if (CopyParam->PreserveRights)
    {
      TChmodSessionAction(FTerminal->ActionLog, AbsoluteFileName,
        Rights);
    }

    FTerminal->LogFileDone(OperationProgress, AbsoluteFileName, Action);
    // Stream is disposed here
  }

  Handle.Release();

  FTerminal->UpdateSource(Handle, CopyParam, Params);

  FTerminal->LogEvent(FORMAT(L"Copying \"%s\" to remote directory finished.", (FileName)));
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SCPDirectorySource(const UnicodeString DirectoryName,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, int Level)
{
  int Attrs;

  FTerminal->LogEvent(FORMAT(L"Entering directory \"%s\".", (DirectoryName)));

  OperationProgress->SetFile(DirectoryName);
  UnicodeString DestFileName =
    FTerminal->ChangeFileName(
      CopyParam, ExtractFileName(DirectoryName), osLocal, Level == 0);

  // Get directory attributes
  FILE_OPERATION_LOOP_BEGIN
  {
    Attrs = FileGetAttrFix(ApiPath(DirectoryName));
    if (Attrs < 0) RaiseLastOSError();
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(CANT_GET_ATTRS, (DirectoryName)));

  UnicodeString TargetDirFull = UnixIncludeTrailingBackslash(TargetDir + DestFileName);

  UnicodeString Buf;

  /* TODO 1: maybe send filetime */

  // Send directory modes (rights), filesize and file name
  Buf = FORMAT(L"D%s 0 %s",
    (CopyParam->RemoteFileRights(Attrs).Octal, DestFileName));
  FSecureShell->SendLine(Buf);
  SCPResponse();

  try
  {
    TSearchRecOwned SearchRec;
    bool FindOK = FTerminal->LocalFindFirstLoop(IncludeTrailingBackslash(DirectoryName) + L"*.*", SearchRec);

    while (FindOK && !OperationProgress->Cancel)
    {
      UnicodeString FileName = SearchRec.GetFilePath();
      try
      {
        if (SearchRec.IsRealFile())
        {
          SCPSource(FileName, TargetDirFull, CopyParam, Params, OperationProgress, Level + 1);
        }
      }
      // Previously we caught ESkipFile, making error being displayed
      // even when file was excluded by mask. Now the ESkipFile is special
      // case without error message.
      catch (EScpFileSkipped &E)
      {
        TQueryParams Params(qpAllowContinueOnError);
        TSuspendFileOperationProgress Suspend(OperationProgress);

        if (FTerminal->QueryUserException(FMTLOAD(COPY_ERROR, (FileName)), &E,
              qaOK | qaAbort, &Params, qtError) == qaAbort)
        {
          OperationProgress->SetCancel(csCancel);
        }
        if (!FTerminal->HandleException(&E))
        {
          throw;
        }
      }
      catch (ESkipFile &E)
      {
        // If ESkipFile occurs, just log it and continue with next file
        TSuspendFileOperationProgress Suspend(OperationProgress);
        if (!FTerminal->HandleException(&E))
        {
          throw;
        }
      }

      FindOK = FTerminal->LocalFindNextLoop(SearchRec);
    }

    SearchRec.Close();

    /* TODO : Delete also read-only directories. */
    /* TODO : Show error message on failure. */
    if (!OperationProgress->Cancel)
    {
      if (FLAGSET(Params, cpDelete))
      {
        RemoveDir(ApiPath(DirectoryName));
      }
      else if (CopyParam->ClearArchive && FLAGSET(Attrs, faArchive))
      {
        FILE_OPERATION_LOOP_BEGIN
        {
          THROWOSIFFALSE(FileSetAttr(ApiPath(DirectoryName), Attrs & ~faArchive) == 0);
        }
        FILE_OPERATION_LOOP_END(FMTLOAD(CANT_SET_ATTRS, (DirectoryName)));
      }
    }
  }
  __finally
  {
    if (FTerminal->Active)
    {
      // Tell remote side, that we're done.
      FTerminal->LogEvent(FORMAT(L"Leaving directory \"%s\".", (DirectoryName)));
      FSecureShell->SendLine(L"E");
      SCPResponse();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CopyToLocal(TStrings * FilesToCopy,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  bool CloseSCP = False;
  Params &= ~(cpAppend | cpResume);
  UnicodeString Options = L"";
  if (CopyParam->PreserveRights || CopyParam->PreserveTime) Options = L"-p";
  if (FTerminal->SessionData->Scp1Compatibility) Options += L" -1";

  FTerminal->LogEvent(FORMAT(L"Copying %d files/directories to local directory "
    "\"%s\"", (FilesToCopy->Count, TargetDir)));
  FTerminal->LogEvent(0, CopyParam->LogStr);

  try
  {
    for (int IFile = 0; (IFile < FilesToCopy->Count) &&
      !OperationProgress->Cancel; IFile++)
    {
      UnicodeString FileName = FilesToCopy->Strings[IFile];
      TRemoteFile * File = (TRemoteFile *)FilesToCopy->Objects[IFile];
      DebugAssert(File);

      try
      {
        bool Success = true; // Have to be set to True (see ::SCPSink)
        SendCommand(FCommandSet->FullCommand(fsCopyToLocal,
          ARRAYOFCONST((Options, DelimitStr(FileName)))));
        SkipFirstLine();

        // Filename is used for error messaging and excluding files only
        // Send in full path to allow path-based excluding
        UnicodeString FullFileName = UnixExcludeTrailingBackslash(File->FullFileName);
        SCPSink(TargetDir, FullFileName, UnixExtractFilePath(FullFileName),
          CopyParam, Success, OperationProgress, Params, 0);
        // operation succeeded (no exception), so it's ok that
        // remote side closed SCP, but we continue with next file
        if (OperationProgress->Cancel == csRemoteAbort)
        {
          OperationProgress->SetCancel(csContinue);
        }

        // Move operation -> delete file/directory afterwards
        // but only if copying succeeded
        if ((Params & cpDelete) && Success && !OperationProgress->Cancel)
        {
          try
          {
            FTerminal->ExceptionOnFail = true;
            try
            {
              FILE_OPERATION_LOOP_BEGIN
              {
                // pass full file name in FileName, in case we are not moving
                // from current directory
                FTerminal->DeleteFile(FileName, File);
              }
              FILE_OPERATION_LOOP_END(FMTLOAD(DELETE_FILE_ERROR, (FileName)));
            }
            __finally
            {
              FTerminal->ExceptionOnFail = false;
            }
          }
          catch (EFatal &)
          {
            throw;
          }
          catch (...)
          {
            // If user selects skip (or abort), nothing special actually occurs
            // we just run DoFinished with Success = False, so file won't
            // be deselected in panel (depends on assigned event handler)

            // On csCancel we would later try to close remote SCP, but it
            // is closed already
            if (OperationProgress->Cancel == csCancel)
            {
              OperationProgress->SetCancel(csRemoteAbort);
            }
            Success = false;
          }
        }

        FTerminal->OperationFinish(
          OperationProgress, File, FileName, (!OperationProgress->Cancel && Success), OnceDoneOperation);
      }
      catch (...)
      {
        FTerminal->OperationFinish(OperationProgress, File, FileName, false, OnceDoneOperation);
        CloseSCP = (OperationProgress->Cancel != csRemoteAbort);
        throw;
      }
    }
  }
  __finally
  {
    // In case that copying doesn't cause fatal error (ie. connection is
    // still active) but wasn't successful (exception or user termination)
    // we need to ensure, that SCP on remote side is closed
    if (FTerminal->Active && (CloseSCP ||
        (OperationProgress->Cancel == csCancel) ||
        (OperationProgress->Cancel == csCancelTransfer)))
    {
      // If we get LastLine, it means that remote side 'scp' is already
      // terminated, so we need not to terminate it. There is also
      // possibility that remote side waits for confirmation, so it will hang.
      // This should not happen (hope)
      if (!IsLastLine(FSecureShell->ReceiveLine()))
      {
        SCPSendError((OperationProgress->Cancel ? L"Terminated by user." : L"Exception"), true);
        // Just in case, remote side already sent some more data (it's probable)
        // but we don't want to raise exception (user asked to terminate, it's not error)
        ReadCommandOutput(coOnlyReturnCode | coWaitForLastLine);
      }
      else
      {
        ReadCommandOutput(coOnlyReturnCode);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::Sink(
  const UnicodeString & /*FileName*/, const TRemoteFile * /*File*/,
  const UnicodeString & /*TargetDir*/, UnicodeString & /*DestFileName*/, int /*Attrs*/,
  const TCopyParamType * /*CopyParam*/, int /*Params*/, TFileOperationProgressType * /*OperationProgress*/,
  unsigned int /*Flags*/, TDownloadSessionAction & /*Action*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SCPError(const UnicodeString Message, bool Fatal)
{
  SCPSendError(Message, Fatal);
  throw EScpFileSkipped(NULL, Message);
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SCPSendError(const UnicodeString Message, bool Fatal)
{
  DebugUsedParam(Message);
  unsigned char ErrorLevel = (char)(Fatal ? 2 : 1);
  FTerminal->LogEvent(FORMAT(L"Sending SCP error (%d) to remote side:",
    ((int)ErrorLevel)));
  FSecureShell->Send(&ErrorLevel, 1);
  // We don't send exact error message, because some unspecified
  // characters can terminate remote scp
  FSecureShell->SendLine(L"scp: error");
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SCPSink(const UnicodeString TargetDir,
  const UnicodeString FileName, const UnicodeString SourceDir,
  const TCopyParamType * CopyParam, bool & Success,
  TFileOperationProgressType * OperationProgress, int Params,
  int Level)
{
  struct
  {
    int SetTime;
    TDateTime Modification;
    TRights RemoteRights;
    int Attrs;
    bool Exists;
  } FileData;

  bool SkipConfirmed = false;
  bool Initialized = (Level > 0);

  FileData.SetTime = 0;

  TValueRestorer<TSecureShellMode> SecureShellModeRestorer(FSecureShell->Mode, ssmDownloading);

  FSecureShell->SendNull();

  while (!OperationProgress->Cancel)
  {
    // See (switch ... case 'T':)
    if (FileData.SetTime) FileData.SetTime--;

    // In case of error occurred before control record arrived.
    // We can finally use full path here, as we get current path in FileName param
    // (we used to set the file into OperationProgress->FileName, but it collided
    // with progress outputting, particularly for scripting)
    UnicodeString FullFileName = FileName;

    try
    {
      // Receive control record
      UnicodeString Line = FSecureShell->ReceiveLine();

      if (Line.Length() == 0) FTerminal->FatalError(NULL, LoadStr(SCP_EMPTY_LINE));

      if (TryRemoveLastLine(Line))
      {
        // Remote side finished copying, so remote SCP was closed
        // and we don't need to terminate it manually, see CopyToLocal()
        OperationProgress->SetCancel(csRemoteAbort);
        /* TODO 1 : Show stderror to user? */
        FSecureShell->ClearStdError();
        try
        {
          // coIgnoreWarnings should allow batch transfer to continue when
          // download of one the files fails (user denies overwriting
          // of target local file, no read permissions...)
          ReadCommandOutput(coExpectNoOutput | coRaiseExcept |
            coOnlyReturnCode | coIgnoreWarnings);
          if (!Initialized)
          {
            throw Exception(L"");
          }
        }
        catch(Exception & E)
        {
          if (!Initialized && FTerminal->Active)
          {
            FTerminal->TerminalError(&E, LoadStr(SCP_INIT_ERROR));
          }
          else
          {
            throw;
          }
        }
        return;
      }
      else
      {
        Initialized = true;

        // First character distinguish type of control record
        wchar_t Ctrl = Line[1];
        Line.Delete(1, 1);

        switch (Ctrl) {
          case 1:
            // Error (already logged by ReceiveLine())
            throw EScpFileSkipped(NULL, FMTLOAD(REMOTE_ERROR, (Line)));

          case 2:
            // Fatal error, terminate copying
            FTerminal->TerminalError(Line);
            UNREACHABLE_AFTER_NORETURN(return);

          case L'E': // Exit
            FSecureShell->SendNull();
            return;

          case L'T':
            unsigned long MTime, ATime;
            if (swscanf(Line.c_str(), L"%ld %*d %ld %*d",  &MTime, &ATime) == 2)
            {
              FileData.Modification = UnixToDateTime(MTime, FTerminal->SessionData->DSTMode);
              FSecureShell->SendNull();
              // File time is only valid until next pass
              FileData.SetTime = 2;
              continue;
            }
              else
            {
              SCPError(LoadStr(SCP_ILLEGAL_TIME_FORMAT), False);
            }

          case L'C':
          case L'D':
            break; // continue pass switch{}

          default:
            FTerminal->FatalError(NULL, FMTLOAD(SCP_INVALID_CONTROL_RECORD, (Ctrl, Line)));
        }

        TFileMasks::TParams MaskParams;
        MaskParams.Modification = FileData.Modification;

        // We reach this point only if control record was 'C' or 'D'
        try
        {
          FileData.RemoteRights.Octal = CutToChar(Line, L' ', True);
          // do not trim leading spaces of the filename
          __int64 TSize = StrToInt64(CutToChar(Line, L' ', False).TrimRight());
          MaskParams.Size = TSize;
          // Security fix: ensure the file ends up where we asked for it.
          // (accept only filename, not path)
          UnicodeString OnlyFileName = UnixExtractFileName(Line);
          if (Line != OnlyFileName)
          {
            FTerminal->LogEvent(FORMAT(L"Warning: Remote host set a compound pathname '%s'", (Line)));
          }
          if ((Level == 0) && (OnlyFileName != UnixExtractFileName(FileName)))
          {
            SCPError(LoadStr(UNREQUESTED_FILE), False);
          }

          FullFileName = SourceDir + OnlyFileName;
          OperationProgress->SetFile(FullFileName);
          OperationProgress->SetTransferSize(TSize);
        }
        catch (Exception &E)
        {
          {
            TSuspendFileOperationProgress Suspend(OperationProgress);
            FTerminal->Log->AddException(&E);
          }
          SCPError(LoadStr(SCP_ILLEGAL_FILE_DESCRIPTOR), false);
        }

        // last possibility to cancel transfer before it starts
        if (OperationProgress->Cancel)
        {
          throw ESkipFile(NULL, MainInstructions(LoadStr(USER_TERMINATED)));
        }

        bool Dir = (Ctrl == L'D');
        UnicodeString BaseFileName = FTerminal->GetBaseFileName(FullFileName);
        if (!CopyParam->AllowTransfer(BaseFileName, osRemote, Dir, MaskParams, IsUnixHiddenFile(BaseFileName)))
        {
          FTerminal->LogEvent(FORMAT(L"File \"%s\" excluded from transfer",
            (FullFileName)));
          SkipConfirmed = true;
          SCPError(L"", false);
        }

        if (CopyParam->SkipTransfer(FullFileName, Dir))
        {
          SkipConfirmed = true;
          OperationProgress->AddSkippedFileSize(MaskParams.Size);
          SCPError(L"", false);
        }

        FTerminal->LogFileDetails(FileName, FileData.Modification, MaskParams.Size);

        UnicodeString DestFileNameOnly =
          FTerminal->ChangeFileName(
            CopyParam, OperationProgress->FileName, osRemote,
            Level == 0);
        UnicodeString DestFileName =
          IncludeTrailingBackslash(TargetDir) + DestFileNameOnly;

        FileData.Attrs = FileGetAttrFix(ApiPath(DestFileName));
        // If getting attrs fails, we suppose, that file/folder doesn't exists
        FileData.Exists = (FileData.Attrs != -1);
        if (Dir)
        {
          if (FileData.Exists && !(FileData.Attrs & faDirectory))
          {
            SCPError(FMTLOAD(NOT_DIRECTORY_ERROR, (DestFileName)), false);
          }

          if (!FileData.Exists)
          {
            FILE_OPERATION_LOOP_BEGIN
            {
              THROWOSIFFALSE(ForceDirectories(ApiPath(DestFileName)));
            }
            FILE_OPERATION_LOOP_END(FMTLOAD(CREATE_DIR_ERROR, (DestFileName)));
            /* SCP: can we set the timestamp for directories ? */
          }
          UnicodeString FullFileName = SourceDir + OperationProgress->FileName;
          SCPSink(DestFileName, FullFileName, UnixIncludeTrailingBackslash(FullFileName),
            CopyParam, Success, OperationProgress, Params, Level + 1);
          continue;
        }
        else if (Ctrl == L'C')
        {
          TDownloadSessionAction Action(FTerminal->ActionLog);
          Action.FileName(FTerminal->AbsolutePath(FullFileName, true));

          try
          {
            HANDLE File = NULL;
            TStream * FileStream = NULL;

            /* TODO 1 : Turn off read-only attr */

            try
            {
              try
              {
                if (FileExists(ApiPath(DestFileName)))
                {
                  __int64 MTime;
                  TOverwriteFileParams FileParams;
                  FileParams.SourceSize = OperationProgress->TransferSize;
                  FileParams.SourceTimestamp = FileData.Modification;
                  FTerminal->OpenLocalFile(DestFileName, GENERIC_READ,
                    NULL, NULL, NULL, &MTime, NULL,
                    &FileParams.DestSize);
                  FileParams.DestTimestamp = UnixToDateTime(MTime,
                    FTerminal->SessionData->DSTMode);

                  unsigned int Answer =
                    ConfirmOverwrite(OperationProgress->FileName, DestFileNameOnly, osLocal,
                      &FileParams, CopyParam, Params, OperationProgress);

                  switch (Answer)
                  {
                    case qaCancel:
                      OperationProgress->SetCancel(csCancel); // continue on next case
                    case qaNo:
                      SkipConfirmed = true;
                      EXCEPTION;
                  }
                }

                Action.Destination(DestFileName);

                if (!FTerminal->CreateLocalFile(DestFileName, OperationProgress,
                       &File, FLAGSET(Params, cpNoConfirmation)))
                {
                  SkipConfirmed = true;
                  EXCEPTION;
                }

                FileStream = new TSafeHandleStream((THandle)File);
              }
              catch (Exception &E)
              {
                // In this step we can still cancel transfer, so we do it
                SCPError(E.Message, false);
                UNREACHABLE_AFTER_NORETURN(throw);
              }

              // We succeeded, so we confirm transfer to remote side
              FSecureShell->SendNull();
              // From now we need to finish file transfer, if not it's fatal error
              OperationProgress->SetTransferringFile(true);

              // Suppose same data size to transfer as to write
              // (not true with ASCII transfer)
              OperationProgress->SetLocalSize(OperationProgress->TransferSize);

              // Will we use ASCII of BINARY file transfer?
              OperationProgress->SetAsciiTransfer(
                CopyParam->UseAsciiTransfer(BaseFileName, osRemote, MaskParams));
              FTerminal->LogEvent(
                0, UnicodeString((OperationProgress->AsciiTransfer ? L"Ascii" : L"Binary")) +
                  L" transfer mode selected.");

              try
              {
                // Buffer for one block of data
                TFileBuffer BlockBuf;
                bool ConvertToken = false;

                do
                {
                  BlockBuf.Size = OperationProgress->TransferBlockSize();
                  BlockBuf.Reset();

                  FSecureShell->Receive(reinterpret_cast<unsigned char *>(BlockBuf.Data), BlockBuf.Size);
                  OperationProgress->AddTransferred(BlockBuf.Size);

                  if (OperationProgress->AsciiTransfer)
                  {
                    unsigned int PrevBlockSize = BlockBuf.Size;
                    BlockBuf.Convert(FTerminal->SessionData->EOLType,
                      FTerminal->Configuration->LocalEOLType, 0, ConvertToken);
                    OperationProgress->SetLocalSize(
                      OperationProgress->LocalSize - PrevBlockSize + BlockBuf.Size);
                  }

                  // This is crucial, if it fails during file transfer, it's fatal error
                  FILE_OPERATION_LOOP_BEGIN
                  {
                    BlockBuf.WriteToStream(FileStream, BlockBuf.Size);
                  }
                  FILE_OPERATION_LOOP_END_EX(FMTLOAD(WRITE_ERROR, (DestFileName)), folNone);

                  OperationProgress->AddLocallyUsed(BlockBuf.Size);

                  if (OperationProgress->Cancel == csCancelTransfer)
                  {
                    throw Exception(MainInstructions(LoadStr(USER_TERMINATED)));
                  }
                }
                while (!OperationProgress->IsLocallyDone() || !OperationProgress->IsTransferDoneChecked());
              }
              catch (Exception &E)
              {
                // Every exception during file transfer is fatal
                FTerminal->FatalError(&E,
                  FMTLOAD(COPY_FATAL, (OperationProgress->FileName)));
              }

              OperationProgress->SetTransferringFile(false);

              try
              {
                SCPResponse();
                // If one of following exception occurs, we still need
                // to send confirmation to other side
              }
              catch (EScp &)
              {
                FSecureShell->SendNull();
                throw;
              }
              catch (EScpFileSkipped &)
              {
                FSecureShell->SendNull();
                throw;
              }

              FSecureShell->SendNull();

              if (FileData.SetTime && CopyParam->PreserveTime)
              {
                FTerminal->UpdateTargetTime(File, FileData.Modification, mfFull, FTerminal->SessionData->DSTMode);
              }
            }
            __finally
            {
              if (File) CloseHandle(File);
              if (FileStream) delete FileStream;
            }
          }
          catch(Exception & E)
          {
            if (SkipConfirmed)
            {
              Action.Cancel();
            }
            else
            {
              FTerminal->RollbackAction(Action, OperationProgress, &E);
            }
            throw;
          }

          if (FileData.Attrs == -1) FileData.Attrs = faArchive;
          int NewAttrs = CopyParam->LocalFileAttrs(FileData.RemoteRights);
          if ((NewAttrs & FileData.Attrs) != NewAttrs)
          {
            FILE_OPERATION_LOOP_BEGIN
            {
              THROWOSIFFALSE(FileSetAttr(ApiPath(DestFileName), FileData.Attrs | NewAttrs) == 0);
            }
            FILE_OPERATION_LOOP_END(FMTLOAD(CANT_SET_ATTRS, (DestFileName)));
          }

          FTerminal->LogFileDone(OperationProgress, DestFileName, Action);
        }
      }
    }
    catch (EScpFileSkipped &E)
    {
      if (!SkipConfirmed)
      {
        TSuspendFileOperationProgress Suspend(OperationProgress);
        TQueryParams Params(qpAllowContinueOnError);
        if (FTerminal->QueryUserException(FMTLOAD(COPY_ERROR, (FullFileName)),
              &E, qaOK | qaAbort, &Params, qtError) == qaAbort)
        {
          OperationProgress->SetCancel(csCancel);
        }
        FTerminal->Log->AddException(&E);
      }
      // this was inside above condition, but then transfer was considered
      // successful, even when for example user refused to overwrite file
      Success = false;
    }
    catch (ESkipFile &E)
    {
      SCPSendError(E.Message, false);
      Success = false;
      if (!FTerminal->HandleException(&E)) throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::GetSupportedChecksumAlgs(TStrings * Algs)
{
  FTerminal->GetShellChecksumAlgs(Algs);
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::LockFile(const UnicodeString & /*FileName*/, const TRemoteFile * /*File*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::UnlockFile(const UnicodeString & /*FileName*/, const TRemoteFile * /*File*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::UpdateFromMain(TCustomFileSystem * /*MainFileSystem*/)
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ClearCaches()
{
  // noop
}
