//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Terminal.h"
#include "ScpFileSystem.h"

#include "Common.h"
#include "Interface.h"
#include "TextsCore.h"

#include <stdio.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_EX(ALLOW_SKIP, MESSAGE, OPERATION) \
  FILE_OPERATION_LOOP_CUSTOM(FTerminal, ALLOW_SKIP, MESSAGE, OPERATION)
//---------------------------------------------------------------------------
const coRaiseExcept = 1;
const coExpectNoOutput = 2;
const coWaitForLastLine = 4;
const coOnlyReturnCode = 8;
const coIgnoreWarnings = 16;
const coReadProgress = 32;

const ecRaiseExcept = 1;
const ecIgnoreWarnings = 2;
const ecReadProgress = 4;
const ecDefault = ecRaiseExcept;
//---------------------------------------------------------------------------
#define THROW_FILE_SKIPPED(EXCEPTION, MESSAGE) \
  throw EScpFileSkipped(EXCEPTION, MESSAGE)

#define THROW_SCP_ERROR(EXCEPTION, MESSAGE) \
  throw EScp(EXCEPTION, MESSAGE)
//===========================================================================
#define MaxShellCommand fsAnyCommand
#define ShellCommandCount MaxShellCommand + 1
#define MaxCommandLen 40
struct TCommandType
{
  int MinLines;
  int MaxLines;
  bool ModifiesFiles;
  bool ChangesDirectory;
  bool InteractiveCommand;
  char Command[MaxCommandLen];
};

// Only one character! See TSCPFileSystem::ReadCommandOutput()
#define LastLineSeparator ":"
#define LAST_LINE "WinSCP: this is end-of-file"
#define FIRST_LINE "WinSCP: this is begin-of-file"
extern const TCommandType DefaultCommandSet[];

#define NationalVarCount 10
extern const char NationalVars[NationalVarCount][15];

#define CHECK_CMD assert((Cmd >=0) && (Cmd <= MaxShellCommand))

class TSessionData;
//---------------------------------------------------------------------------
class TCommandSet
{
private:
  TCommandType CommandSet[ShellCommandCount];
  TSessionData * FSessionData;
  AnsiString FReturnVar;
  int __fastcall GetMaxLines(TFSCommand Cmd);
  int __fastcall GetMinLines(TFSCommand Cmd);
  bool __fastcall GetModifiesFiles(TFSCommand Cmd);
  bool __fastcall GetChangesDirectory(TFSCommand Cmd);
  bool __fastcall GetOneLineCommand(TFSCommand Cmd);
  void __fastcall SetCommands(TFSCommand Cmd, AnsiString value);
  AnsiString __fastcall GetCommands(TFSCommand Cmd);
  AnsiString __fastcall GetFirstLine();
  bool __fastcall GetInteractiveCommand(TFSCommand Cmd);
  AnsiString __fastcall GetLastLine();
  AnsiString __fastcall GetReturnVar();
public:
  __fastcall TCommandSet(TSessionData *aSessionData);
  void __fastcall Default();
  void __fastcall CopyFrom(TCommandSet * Source);
  AnsiString __fastcall Command(TFSCommand Cmd, const TVarRec * args, int size);
  TStrings * __fastcall CreateCommandList();
  AnsiString __fastcall FullCommand(TFSCommand Cmd, const TVarRec * args, int size);
  __property int MaxLines[TFSCommand Cmd]  = { read=GetMaxLines};
  __property int MinLines[TFSCommand Cmd]  = { read=GetMinLines };
  __property bool ModifiesFiles[TFSCommand Cmd]  = { read=GetModifiesFiles };
  __property bool ChangesDirectory[TFSCommand Cmd]  = { read=GetChangesDirectory };
  __property bool OneLineCommand[TFSCommand Cmd]  = { read=GetOneLineCommand };
  __property AnsiString Commands[TFSCommand Cmd]  = { read=GetCommands, write=SetCommands };
  __property AnsiString FirstLine = { read = GetFirstLine };
  __property bool InteractiveCommand[TFSCommand Cmd] = { read = GetInteractiveCommand };
  __property AnsiString LastLine  = { read=GetLastLine };
  __property TSessionData * SessionData  = { read=FSessionData, write=FSessionData };
  __property AnsiString ReturnVar  = { read=GetReturnVar, write=FReturnVar };
};
//===========================================================================
const char NationalVars[NationalVarCount][15] =
  {"LANG", "LANGUAGE", "LC_CTYPE", "LC_COLLATE", "LC_MONETARY", "LC_NUMERIC",
   "LC_TIME", "LC_MESSAGES", "LC_ALL", "HUMAN_BLOCKS" };
const char FullTimeOption[] = "--full-time";
//---------------------------------------------------------------------------
#define F false
#define T true
// TODO: remove "mf" and "cd", it is implemented in TTerminal already 
const TCommandType DefaultCommandSet[ShellCommandCount] = {
//                       min max mf cd ia  command
/*Null*/                { -1, -1, F, F, F, "" },
/*VarValue*/            { -1, -1, F, F, F, "echo \"$%s\"" /* variable */ },
/*LastLine*/            { -1, -1, F, F, F, "echo \"%s" LastLineSeparator "%s\"" /* last line, return var */ },
/*FirstLine*/           { -1, -1, F, F, F, "echo \"%s\"" /* first line */ },
/*CurrentDirectory*/    {  1,  1, F, F, F, "pwd" },
/*ChangeDirectory*/     {  0,  0, F, T, F, "cd %s" /* directory */ },
// list directory can be empty on permission denied, this is handled in ReadDirectory
/*ListDirectory*/       { -1, -1, F, F, F, "ls -la %s \"%s\"" /* directory */ },
/*ListCurrentDirectory*/{ -1, -1, F, F, F, "ls -la %s" },
/*ListFile*/            {  1,  1, F, F, F, "ls -lad %s \"%s\"" /* file/directory */ },
/*LookupUserGroups*/    {  0,  1, F, F, F, "groups" },
/*CopyToRemote*/        { -1, -1, T, F, T, "scp -r %s -d -t \"%s\"" /* options, directory */ },
/*CopyToLocal*/         { -1, -1, F, F, T, "scp -r %s -d -f \"%s\"" /* options, file */ },
/*DeleteFile*/          {  0,  0, T, F, F, "rm -f -r \"%s\"" /* file/directory */},
/*RenameFile*/          {  0,  0, T, F, F, "mv -f \"%s\" \"%s\"" /* file/directory, new name*/},
/*CreateDirectory*/     {  0,  0, T, F, F, "mkdir \"%s\"" /* new directory */},
/*ChangeMode*/          {  0,  0, T, F, F, "chmod %s %s \"%s\"" /* options, mode, filename */},
/*ChangeGroup*/         {  0,  0, T, F, F, "chgrp %s \"%s\" \"%s\"" /* options, group, filename */},
/*ChangeOwner*/         {  0,  0, T, F, F, "chown %s \"%s\" \"%s\"" /* options, owner, filename */},
/*HomeDirectory*/       {  0,  0, F, T, F, "cd" },
/*Unset*/               {  0,  0, F, F, F, "unset \"%s\"" /* variable */ },
/*Unalias*/             {  0,  0, F, F, F, "unalias \"%s\"" /* alias */ },
/*AliasGroupList*/      {  0,  0, F, F, F, "alias ls=\"ls -g\"" },
/*CreateLink*/          {  0,  0, T, F, F, "ln %s \"%s\" \"%s\"" /*symbolic (-s), filename, point to*/},
/*CopyFile*/            {  0,  0, T, F, F, "cp -p -r -f \"%s\" \"%s\"" /* file/directory, target name*/},
/*AnyCommand*/          {  0, -1, T, T, F, "%s" }
};
#undef F
#undef T
//---------------------------------------------------------------------------
__fastcall TCommandSet::TCommandSet(TSessionData *aSessionData):
  FSessionData(aSessionData), FReturnVar("")
{
  assert(FSessionData);
  Default();
}
//---------------------------------------------------------------------------
void __fastcall TCommandSet::CopyFrom(TCommandSet * Source)
{
  memcpy(&CommandSet, Source->CommandSet, sizeof(CommandSet));
}
//---------------------------------------------------------------------------
void __fastcall TCommandSet::Default()
{
  assert(sizeof(CommandSet) == sizeof(DefaultCommandSet));
  memcpy(&CommandSet, &DefaultCommandSet, sizeof(CommandSet));
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
void __fastcall TCommandSet::SetCommands(TFSCommand Cmd, AnsiString value)
{
  CHECK_CMD;
  strcpy(CommandSet[Cmd].Command, value.SubString(1, MaxCommandLen - 1).c_str());
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCommandSet::GetCommands(TFSCommand Cmd)
{
  CHECK_CMD;
  return CommandSet[Cmd].Command;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCommandSet::Command(TFSCommand Cmd, const TVarRec * args, int size)
{
  if (args) return Format(Commands[Cmd], args, size);
    else return Commands[Cmd];
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCommandSet::FullCommand(TFSCommand Cmd, const TVarRec * args, int size)
{
  AnsiString Separator;
  if (OneLineCommand[Cmd]) Separator = " ; ";
    else Separator = "\n";
  AnsiString Line = Command(Cmd, args, size);
  AnsiString LastLineCmd =
    Command(fsLastLine, ARRAYOFCONST((LastLine, ReturnVar)));
  AnsiString FirstLineCmd;
  if (InteractiveCommand[Cmd])
    FirstLineCmd = Command(fsFirstLine, ARRAYOFCONST((FirstLine))) + Separator;

  AnsiString Result;
  if (!Line.IsEmpty())
    Result = FORMAT("%s%s%s%s", (FirstLineCmd, Line, Separator, LastLineCmd));
  else
    Result = FORMAT("%s%s", (FirstLineCmd, LastLineCmd));
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCommandSet::GetFirstLine()
{
  return FIRST_LINE;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCommandSet::GetLastLine()
{
  return LAST_LINE;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCommandSet::GetReturnVar()
{
  assert(SessionData);
  if (!FReturnVar.IsEmpty()) return AnsiString('$') + FReturnVar;
    else
  if (SessionData->DetectReturnVar) return '0';
    else return AnsiString('$') + SessionData->ReturnVar;
}
//---------------------------------------------------------------------------
TStrings * __fastcall TCommandSet::CreateCommandList()
{
  TStrings * CommandList = new TStringList();
  for (Integer Index = 0; Index < ShellCommandCount; Index++)
  {
    AnsiString Cmd = Commands[(TFSCommand)Index];
    if (!Cmd.IsEmpty())
    {
      Integer P = Cmd.Pos(" ");
      if (P) Cmd.SetLength(P-1);
      if ((Cmd != "%s") && (CommandList->IndexOf(Cmd) < 0))
        CommandList->Add(Cmd);
    }
  }
  return CommandList;
}
//===========================================================================
__fastcall TSCPFileSystem::TSCPFileSystem(TTerminal * ATerminal):
  TCustomFileSystem(ATerminal)
{
  FCommandSet = new TCommandSet(FTerminal->SessionData);
  FLsFullTime = FTerminal->SessionData->SCPLsFullTime;
  FOutput = new TStringList();
  FProcessingCommand = false;
  FOnCaptureOutput = NULL;
}
//---------------------------------------------------------------------------
__fastcall TSCPFileSystem::~TSCPFileSystem()
{
  delete FCommandSet;
  delete FOutput;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSCPFileSystem::GetProtocolName() const
{
  return "SCP";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSCPFileSystem::AbsolutePath(AnsiString Path)
{
  AnsiString Result;
  if (Path.IsEmpty())
  {
    Result = CurrentDirectory;
  }
  else if (Path[1] == '/')
  {
    Result = UnixExcludeTrailingBackslash(Path);
  }
  else
  {
    Result = UnixIncludeTrailingBackslash(
      UnixIncludeTrailingBackslash(CurrentDirectory) + Path);
    int P;
    while ((P = Result.Pos("/../")) > 0)
    {
      int P2 = Result.SubString(1, P-1).LastDelimiter("/");
      assert(P2 > 0);
      Result.Delete(P2, P - P2 + 3); 
    }
    while ((P = Result.Pos("/./")) > 0)
    {
      Result.Delete(P, 2); 
    }
    Result = UnixExcludeTrailingBackslash(Result);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TSCPFileSystem::IsCapable(int Capability) const
{
  assert(FTerminal);
  switch (Capability) {
    case fcUserGroupListing:
    case fcModeChanging:
    case fcGroupChanging:
    case fcOwnerChanging:
    case fcAnyCommand:
    case fcHardLink:
    case fcSymbolicLink:
    case fcResolveSymlink:
    case fcRename:
    case fcRemoteMove:
    case fcRemoteCopy:
      return true;

    case fcTextMode:
      return FTerminal->SessionData->EOLType != FTerminal->Configuration->LocalEOLType;

    case fcNativeTextMode:
    case fcNewerOnlyUpload:
    case fcTimestampChanging:
      return false;

    default:
      assert(false);
      return false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::KeepAlive()
{
  if (!FProcessingCommand)
  {
    ExecCommand(fsNull, NULL, 0, 0);
  }
  else
  {
    FTerminal->LogEvent("Cannot send keepalive, command is being executed");
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::AdditionalInfo(TStrings * AdditionalInfo,
  bool Initial)
{
  if (Initial)
  {
    AnsiString UName;
    FTerminal->ExceptionOnFail = true;
    try
    {
      try
      {
        AnyCommand("uname -a", NULL);
        for (int Index = 0; Index < Output->Count; Index++)
        {
          if (Index > 0)
          {
            UName += "; ";
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

    if (!UName.IsEmpty())
    {
      AdditionalInfo->Add(LoadStr(SCP_UNIX_NAME));
      AdditionalInfo->Add(UName);
    }
    else
    {
      AdditionalInfo->Add(LoadStr(SCP_NO_UNIX_NAME));
    }
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSCPFileSystem::DelimitStr(AnsiString Str)
{
  if (!Str.IsEmpty())
  {
    Str = ::DelimitStr(Str, "\\`$\"");
    if (Str[1] == '-') Str = "./"+Str;
  }
  return Str;
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::EnsureLocation()
{
  if (!FCachedDirectoryChange.IsEmpty())
  {
    FTerminal->LogEvent(FORMAT("Locating to cached directory \"%s\".",
      (FCachedDirectoryChange)));
    AnsiString Directory = FCachedDirectoryChange;
    FCachedDirectoryChange = "";
    try
    {
      ChangeDirectory(Directory);
    }
    catch(...)
    {
      // when location to cached directory fails, pretend again
      // location in cached directory
      // here used to be check (CurrentDirectory != Directory), but it is
      // false always (currentdirectory is already set to cached directory),
      // making the condition below useless. check remove.
      if (FTerminal->Active)
      {
        FCachedDirectoryChange = Directory;
      }
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SendCommand(const AnsiString Cmd)
{
  EnsureLocation();

  AnsiString Line;
  FTerminal->ClearStdError();
  FReturnCode = 0;
  FOutput->Clear();
  // We suppose, that 'Cmd' already contains command that ensures,
  // that 'LastLine' will be printed
  FTerminal->SendLine(Cmd);
  FProcessingCommand = true;
}
//---------------------------------------------------------------------------
bool __fastcall TSCPFileSystem::IsTotalListingLine(const AnsiString Line)
{
  // On some hosts there is not "total" but "totalt". What's the reason??
  // see mail from "Jan Wiklund (SysOp)" <jan@park.se>
  return !Line.SubString(1, 5).AnsiCompareIC("total");
}
//---------------------------------------------------------------------------
bool __fastcall TSCPFileSystem::RemoveLastLine(AnsiString & Line,
    int & ReturnCode, AnsiString LastLine)
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
    AnsiString ReturnCodeStr = Line.SubString(Pos + LastLine.Length() + 1,
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
bool __fastcall TSCPFileSystem::IsLastLine(AnsiString & Line)
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
void __fastcall TSCPFileSystem::SkipFirstLine()
{
  AnsiString Line = FTerminal->ReceiveLine();
  if (Line != FCommandSet->FirstLine)
  {
    FTerminal->TerminalError(NULL, FMTLOAD(FIRST_LINE_EXPECTED, (Line)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ReadCommandOutput(int Params)
{
  try
  {
    if (Params & coWaitForLastLine)
    {
      AnsiString Line;
      bool IsLast;
      unsigned int Total = 0;
      // #55: fixed so, even when last line of command output does not
      // contain CR/LF, we can recognize last line
      do
      {
        Line = FTerminal->ReceiveLine();
        IsLast = IsLastLine(Line);
        if (!IsLast || !Line.IsEmpty()) 
        {
          FOutput->Add(Line);
          if (FLAGSET(Params, coReadProgress))
          {
            Total++;
          
            if (Total % 10 == 0)
            {
              FTerminal->DoReadDirectoryProgress(Total);
            }
          }
        }
      }
      while (!IsLast);
    }
    if (Params & coRaiseExcept)
    {
      AnsiString Message = FTerminal->StdError;
      if ((Params & coExpectNoOutput) && FOutput->Count)
      {
        if (!Message.IsEmpty()) Message += "\n";
        Message += FOutput->Text;
      }
      while (!Message.IsEmpty() && (Message.LastDelimiter("\n\r") == Message.Length()))
        Message.SetLength(Message.Length() - 1);

      bool WrongReturnCode =
        (ReturnCode > 1) || (ReturnCode == 1 && !(Params & coIgnoreWarnings));

      if (Params & coOnlyReturnCode && WrongReturnCode)
      {
        FTerminal->TerminalError(FMTLOAD(COMMAND_FAILED_CODEONLY, (ReturnCode)));
      }
        else
      if (!(Params & coOnlyReturnCode) &&
          ((!Message.IsEmpty() && ((FOutput->Count == 0) || !(Params & coIgnoreWarnings))) ||
           WrongReturnCode))
      {
        FTerminal->TerminalError(FMTLOAD(COMMAND_FAILED, ("%s", ReturnCode, Message)));
      }
    }
  }
  __finally
  {
    FProcessingCommand = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ExecCommand(const AnsiString Cmd, int Params)
{
  if (Params < 0) Params = ecDefault;
  if (FTerminal->UseBusyCursor)
  {
    Busy(true);
  }
  try
  {
    SendCommand(Cmd);
    try
    {
      int COParams = coWaitForLastLine;
      if (Params & ecRaiseExcept) COParams |= coRaiseExcept;
      if (Params & ecIgnoreWarnings) COParams |= coIgnoreWarnings;
      if (Params & ecReadProgress) COParams |= coReadProgress;
      ReadCommandOutput(COParams);
    }
    catch (ETerminal &E)
    {
      AnsiString OnlyCmd = Cmd;
      Integer P = OnlyCmd.Pos('\n');
      if (!P) P = OnlyCmd.Pos(";");
      if (P) OnlyCmd.Delete(P, Cmd.Length() - P + 1);
      FTerminal->TerminalError(FORMAT(E.Message, (OnlyCmd.Trim())));
    }
  }
  __finally
  {
    if (FTerminal->UseBusyCursor)
    {
      Busy(false);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ExecCommand(TFSCommand Cmd, const TVarRec * args,
  int size, int Params)
{
  if (Params < 0) Params = ecDefault;
  AnsiString FullCommand = FCommandSet->FullCommand(Cmd, args, size);
  ExecCommand(FullCommand, Params);
  if (Params & ecRaiseExcept)
  {
    Integer MinL = FCommandSet->MinLines[Cmd];
    Integer MaxL = FCommandSet->MaxLines[Cmd];
    if (((MinL >= 0) && (MinL > FOutput->Count)) ||
        ((MaxL >= 0) && (MaxL > FOutput->Count)))
    {
      FTerminal->TerminalError(FmtLoadStr(INVALID_OUTPUT_ERROR,
        ARRAYOFCONST((FullCommand, Output->Text))));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SetCurrentDirectory(AnsiString value)
{
  assert(false);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSCPFileSystem::GetCurrentDirectory()
{
  return FCurrentDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::DoStartup()
{
  // SkipStartupMessage and DetectReturnVar must succeed,
  // otherwise session is to be closed.
  FTerminal->ExceptionOnFail = true;
  SkipStartupMessage();
  if (FTerminal->SessionData->DetectReturnVar) DetectReturnVar();
  FTerminal->ExceptionOnFail = false;

  #define COND_OPER(OPER) if (FTerminal->SessionData->OPER) OPER()
  COND_OPER(ClearAliases);
  COND_OPER(UnsetNationalVars);
  COND_OPER(AliasGroupList);
  #undef COND_OPER
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SkipStartupMessage()
{
  try
  {
    FTerminal->LogEvent("Skipping host startup message (if any).");
    ExecCommand(fsNull, NULL, 0, 0);
  }
  catch (Exception & E)
  {
    FTerminal->CommandError(&E, LoadStr(SKIP_STARTUP_MESSAGE_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::LookupUsersGroups()
{
  ExecCommand(fsLookupUsersGroups);
  FTerminal->FUsers->Clear();
  FTerminal->FGroups->Clear();
  if (FOutput->Count > 0)
  {
    FTerminal->FGroups->BeginUpdate();
    try
    {
      AnsiString Groups = FOutput->Strings[0];
      while (!Groups.IsEmpty())
      {
        AnsiString NewGroup = CutToChar(Groups, ' ', False);
        FTerminal->FGroups->Add(NewGroup);
      }
    }
    __finally
    {
      FTerminal->FGroups->EndUpdate();
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
    AnsiString ReturnVars[2] = { "status", "?" };
    AnsiString NewReturnVar = "";
    FTerminal->LogEvent("Detecting variable containing return code of last command.");
    for (int Index = 0; Index < 2; Index++)
    {
      bool Success = true;

      try
      {
        FTerminal->LogEvent(FORMAT("Trying \"$%s\".", (ReturnVars[Index])));
        ExecCommand(fsVarValue, ARRAYOFCONST((ReturnVars[Index])));
        if ((Output->Count != 1) || (StrToIntDef(Output->Strings[0], 256) > 255)) Abort();
      }
      catch (EFatal &E)
      {
        // if fatal error occurs, we need to exit ...
        throw;
      }
      catch (Exception &E)
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
      Abort();
    }
      else
    {
      FCommandSet->ReturnVar = NewReturnVar;
      FTerminal->LogEvent(FORMAT("Return code variable \"%s\" selected.",
        (FCommandSet->ReturnVar)));
    }
  }
  catch (Exception &E)
  {
    FTerminal->CommandError(&E, LoadStr(DETECT_RETURNVAR_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ClearAliases()
{
  try
  {
    FTerminal->LogEvent("Clearing all aliases.");
    TStrings * CommandList = FCommandSet->CreateCommandList();
    try
    {
      for (int Index = 0; Index < CommandList->Count; Index++)
      {
        // this command usually fails, becouse there will never be
        // aliases on all commands -> see last False parametr
        ExecCommand(fsUnalias, ARRAYOFCONST((CommandList->Strings[Index])), false);
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
void __fastcall TSCPFileSystem::AliasGroupList()
{
  try
  {
    FTerminal->LogEvent("Aliasing LS to display file group.");
    ExecCommand(fsAliasGroupList);
  }
  catch (Exception &E)
  {
    FTerminal->CommandError(&E, LoadStr(ALIAS_GROUPLIST_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::UnsetNationalVars()
{
  try
  {
    FTerminal->LogEvent("Clearing national user variables.");
    for (int Index = 0; Index < NationalVarCount; Index++)
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
void __fastcall TSCPFileSystem::ChangeDirectory(const AnsiString Directory)
{
  AnsiString ToDir;
  if (!Directory.IsEmpty() &&
      ((Directory[1] != '~') || (Directory.SubString(1, 2) == "~ ")))
  {
    ToDir = "\"" + DelimitStr(Directory) + "\"";
  }
  else
  {
    ToDir = DelimitStr(Directory);
  }
  ExecCommand(fsChangeDirectory, ARRAYOFCONST((ToDir)));
  FCachedDirectoryChange = "";
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CachedChangeDirectory(const AnsiString Directory)
{
  FCachedDirectoryChange = Directory;
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ReadDirectory(TRemoteFileList * FileList)
{
  assert(FileList);
  TStringList * OutputCopy = NULL;
  try
  {
    // emtying file list moved before command execution
    FileList->Clear();

    bool Again;

    do
    {
      Again = false;
      try
      {
        int Params = ecDefault | ecReadProgress |
          FLAGMASK(FTerminal->SessionData->IgnoreLsWarnings, ecIgnoreWarnings);
        const char * Options =
          ((FLsFullTime == asAuto) || (FLsFullTime == asOn)) ? FullTimeOption : "";
        bool ListCurrentDirectory = (FileList->Directory == FTerminal->CurrentDirectory);
        if (ListCurrentDirectory)
        {
          FTerminal->LogEvent("Listing current directory.");
          ExecCommand(fsListCurrentDirectory, ARRAYOFCONST((Options)), Params);
        }
          else
        {
          FTerminal->LogEvent(FORMAT("Listing directory \"%s\".",
            (FileList->Directory)));
          ExecCommand(fsListDirectory, ARRAYOFCONST((Options, DelimitStr(FileList->Directory))),
            Params);
        }

        TRemoteFile * File;

        // If output is not empty, we have succesfully got file listing,
        // otherwise there was an error, in case it was "permission denied"
        // we try to get at least parent directory (see "else" statement below)
        if (FOutput->Count > 0)
        {
          // Copy LS command output, because eventual symlink analysis would
          // modify FTerminal->Output
          OutputCopy = new TStringList();
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
            File = CreateRemoteFile(OutputCopy->Strings[Index]);
            FileList->AddFile(File);
          }
        }
          else
        {
          bool Empty;
          if (ListCurrentDirectory)
          {
            // Empty file list -> probably "permision denied", we
            // at least get link to parent directory ("..")
            FTerminal->ReadFile(
              UnixIncludeTrailingBackslash(FTerminal->FFiles->Directory) +
                PARENTDIRECTORY, File);
            Empty = (File == NULL);
            if (!Empty)
            {
              assert(File->IsParentDirectory);
              FileList->AddFile(File);
            }
          }
          else
          {
            Empty = true;
          }

          if (Empty)
          {
            throw Exception(FMTLOAD(EMPTY_DIRECTORY, (FileList->Directory)));
          }
        }

        if (FLsFullTime == asAuto)
        {
            FTerminal->LogEvent(
              FORMAT("Directory listing with %s succeed, next time all errors during "
                "directory listing will be displayed immediatelly.",
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
            FTerminal->DoHandleExtendedException(&E);
            FLsFullTime = asOff;
            Again = true;
            FTerminal->LogEvent(
              FORMAT("Directory listing with %s failed, try again regular listing.",
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
  __finally
  {
    delete OutputCopy;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ReadSymlink(TRemoteFile * SymlinkFile,
  TRemoteFile *& File)
{
  CustomReadFile(SymlinkFile->LinkTo, File, SymlinkFile);
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ReadFile(const AnsiString FileName,
  TRemoteFile *& File)
{
  CustomReadFile(FileName, File, NULL);
}
//---------------------------------------------------------------------------
TRemoteFile * __fastcall TSCPFileSystem::CreateRemoteFile(
  const AnsiString & ListingStr, TRemoteFile * LinkedByFile)
{
  TRemoteFile * File = new TRemoteFile(LinkedByFile);
  try
  {
    File->Terminal = FTerminal;
    File->ListingStr = ListingStr;
    File->ShiftTime(FTerminal->SessionData->TimeDifference);
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
void __fastcall TSCPFileSystem::CustomReadFile(const AnsiString FileName,
  TRemoteFile *& File, TRemoteFile * ALinkedByFile)
{
  File = NULL;
  int Params = ecDefault |
    FLAGMASK(FTerminal->SessionData->IgnoreLsWarnings, ecIgnoreWarnings);
  // the auto-detection of --full-time support is not implemented for fsListFile,
  // so we use it only if we already know that it is supported (asOn).
  const char * Options = (FLsFullTime == asOn) ? FullTimeOption : "";
  ExecCommand(fsListFile, ARRAYOFCONST((Options, DelimitStr(FileName))), Params);
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
void __fastcall TSCPFileSystem::DeleteFile(const AnsiString FileName,
  const TRemoteFile * File, bool Recursive)
{
  USEDPARAM(Recursive);
  USEDPARAM(File);
  assert(Recursive || (File && File->IsSymLink));
  ExecCommand(fsDeleteFile, ARRAYOFCONST((DelimitStr(FileName))));
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::RenameFile(const AnsiString FileName,
  const AnsiString NewName)
{
  ExecCommand(fsRenameFile, ARRAYOFCONST((DelimitStr(FileName), DelimitStr(NewName))));
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CopyFile(const AnsiString FileName,
  const AnsiString NewName)
{
  ExecCommand(fsCopyFile, ARRAYOFCONST((DelimitStr(FileName), DelimitStr(NewName))));
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CreateDirectory(const AnsiString DirName,
  const TRemoteProperties * Properties)
{
  USEDPARAM(Properties);
  assert(!Properties); // not implemented yet
  ExecCommand(fsCreateDirectory, ARRAYOFCONST((DelimitStr(DirName))));
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CreateLink(const AnsiString FileName,
  const AnsiString PointTo, bool Symbolic)
{
  ExecCommand(fsCreateLink,
    ARRAYOFCONST((Symbolic ? "-s" : "", DelimitStr(PointTo), DelimitStr(FileName))));
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::ChangeFileProperties(const AnsiString FileName,
  const TRemoteFile * File, const TRemoteProperties * Properties)
{
  assert(Properties);
  bool IsDirectory = File && File->IsDirectory;
  bool Recursive = Properties->Recursive && IsDirectory;
  AnsiString RecursiveStr = Recursive ? "-R" : "";

  AnsiString DelimitedName = DelimitStr(FileName);
  if (Properties->Valid.Contains(vpRights))
  {
    TRights Rights = Properties->Rights;

    // if we don't set modes recursively, we may add X at once with other
    // options. Otherwise we have to add X after recusive command
    if (!Recursive && IsDirectory && Properties->AddXToDirectories)
      Rights.AddExecute();

    ExecCommand(fsChangeMode,
      ARRAYOFCONST((RecursiveStr, Rights.SimplestStr, DelimitedName)));

    // if file is directory and we do recursive mode settings with
    // add-x-to-directories option on, add those X
    if (Recursive && IsDirectory && Properties->AddXToDirectories)
    {
      Rights.AddExecute();
      ExecCommand(fsChangeMode,
        ARRAYOFCONST(("", Rights.SimplestStr, DelimitedName)));
    }
  }
  if (Properties->Valid.Contains(vpGroup))
  {
    ExecCommand(fsChangeGroup,
      ARRAYOFCONST((RecursiveStr, Properties->Group, DelimitedName)));
  }
  if (Properties->Valid.Contains(vpOwner))
  {
    ExecCommand(fsChangeOwner,
      ARRAYOFCONST((RecursiveStr, DelimitStr(Properties->Owner), DelimitedName)));
  }
  assert(!Properties->Valid.Contains(vpLastAccess));
  assert(!Properties->Valid.Contains(vpModification));
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CustomCommandOnFile(const AnsiString FileName,
    const TRemoteFile * File, AnsiString Command, int Params, 
    TLogAddLineEvent OutputEvent)
{
  assert(File);
  bool Dir = File->IsDirectory && !File->IsSymLink;
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
    AnsiString Cmd = TRemoteCustomCommand(FileName, "").Complete(Command, true);

    AnyCommand(Cmd, OutputEvent);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CaptureOutput(TObject * Sender,
  TLogLineType Type, const AnsiString AddedLine)
{
  assert((Type == llOutput || Type == llStdError));
  int ReturnCode;
  AnsiString Line = AddedLine;
  if ((Type == llStdError) ||
      !RemoveLastLine(Line, ReturnCode) ||
      !Line.IsEmpty())
  {
    assert(FOnCaptureOutput != NULL);
    FOnCaptureOutput(Sender, Type, AddedLine);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::AnyCommand(const AnsiString Command,
  TLogAddLineEvent OutputEvent)
{
  assert(FTerminal->FOnCaptureOutput == NULL);
  if (OutputEvent != NULL)
  {
    FTerminal->FOnCaptureOutput = CaptureOutput;
    FOnCaptureOutput = OutputEvent;
  }

  try
  {
    ExecCommand(fsAnyCommand, ARRAYOFCONST((Command)),
      ecDefault | ecIgnoreWarnings);
  }
  __finally
  {
    FOnCaptureOutput = NULL;
    FTerminal->FOnCaptureOutput = NULL;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSCPFileSystem::FileUrl(const AnsiString FileName)
{
  assert(FileName.Length() > 0);
  return AnsiString("scp://") + FTerminal->SessionData->SessionName + 
    (FileName[1] == '/' ? "" : "/") + FileName;
}
//---------------------------------------------------------------------------
TStrings * __fastcall TSCPFileSystem::GetFixedPaths()
{
  return NULL;
}
//---------------------------------------------------------------------------
// transfer protocol
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SCPResponse(bool * GotLastLine)
{
  // Taken from scp.c response() and modified

  char Resp;
  FTerminal->Receive(&Resp, 1);

  switch (Resp)
  {
    case 0:     /* ok */
      FTerminal->LogEvent("SCP remote side confirmation (0)");
      return;

    default:
    case 1:     /* error */
    case 2:     /* fatal error */
      // pscp adds 'Resp' to 'Msg', why?
      AnsiString Msg = FTerminal->ReceiveLine();
      AnsiString Line = AnsiString(Resp) + Msg;
      if (IsLastLine(Line))
      {
        if (GotLastLine)
        {
          *GotLastLine = true;
        }

        /* TODO 1 : Show stderror to user? */
        FTerminal->ClearStdError();

        try
        {
          ReadCommandOutput(coExpectNoOutput | coRaiseExcept | coOnlyReturnCode);
        }
        catch(...)
        {
          // when ReadCommandOutput() fails than remote SCP is terminated already
          *GotLastLine = true;
          throw;
        }
      }
        else
      if (Resp == 1)
      {
        FTerminal->LogEvent("SCP remote side error (1):");
      }
        else
      {
        FTerminal->LogEvent("SCP remote side fatal error (2):");
      }

      if (Resp == 1)
      {
        THROW_FILE_SKIPPED(NULL, Msg);
      }
        else
      {
        THROW_SCP_ERROR(NULL, Msg);
      }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CopyToRemote(TStrings * FilesToCopy,
  const AnsiString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  bool & DisconnectWhenComplete)
{
  // scp.c: source(), toremote()
  assert(FilesToCopy && OperationProgress);

  AnsiString Options = "";
  bool CheckExistence = UnixComparePaths(TargetDir, FTerminal->CurrentDirectory) &&
    (FTerminal->FFiles != NULL) && FTerminal->FFiles->Loaded;
  bool CopyBatchStarted = false;
  bool Failed = true;
  bool GotLastLine = false;

  if (CopyParam->PreserveRights) Options = "-p";
  if (FTerminal->SessionData->Scp1Compatibility) Options += " -1";

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
        throw Exception("");
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
      AnsiString FileName = FilesToCopy->Strings[IFile];
      bool CanProceed;

      AnsiString FileNameOnly =
        CopyParam->ChangeFileName(ExtractFileName(FileName), osLocal, true);

      if (CheckExistence)
      {
        // previously there was assertion on FTerminal->FFiles->Loaded, but it
        // fails for scripting, is 'ls' is not issued before
        TRemoteFile * File = FTerminal->FFiles->FindFile(FileNameOnly);
        if (File && OperationProgress->NoToAll)
        {
          CanProceed = false;
        }
        else if (File && !OperationProgress->YesToAll &&
          FTerminal->Configuration->ConfirmOverwriting && !(Params & cpNoConfirmation))
        {
          int Answer;
          if (File->IsDirectory)
          {
            TQueryParams Params(qpNeverAskAgainCheck);
            SUSPEND_OPERATION
            (
              Answer = FTerminal->DoQueryUser(
                FMTLOAD(DIRECTORY_OVERWRITE, (FileNameOnly)),
                qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll,
                &Params);
            );
            switch (Answer)
            {
              case qaNeverAskAgain:
                FTerminal->Configuration->ConfirmOverwriting = false;
                Answer = qaYes;
                break;

              case qaYesToAll:
                OperationProgress->YesToAll = true;
                Answer = qaYes;
                break;

              case qaNoToAll:
                OperationProgress->NoToAll = true;
                Answer = qaNo;
                break;
            }
          }
          else
          {
            __int64 MTime;
            TOverwriteFileParams FileParams;
            FTerminal->OpenLocalFile(FileName, GENERIC_READ,
              NULL, NULL, NULL, &MTime, NULL,
              &FileParams.SourceSize);
            FileParams.SourceTimestamp = UnixToDateTime(MTime,
              FTerminal->SessionData->ConsiderDST);
            FileParams.DestSize = File->Size;
            FileParams.DestTimestamp = File->Modification;

            TQueryButtonAlias Aliases[1];
            Aliases[0].Button = qaAll;
            Aliases[0].Alias = LoadStr(YES_TO_NEWER_BUTTON);
            TQueryParams Params(qpNeverAskAgainCheck);
            Params.Aliases = Aliases;
            Params.AliasesCount = LENOF(Aliases);
            SUSPEND_OPERATION
            (
              Answer = FTerminal->ConfirmFileOverwrite(
                FileNameOnly, &FileParams,
                qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll | qaAll,
                &Params, osRemote, OperationProgress);
            );
          }
          switch (Answer) {
            case qaYes:
              CanProceed = true;
              break;

            case qaCancel:
              if (!OperationProgress->Cancel) OperationProgress->Cancel = csCancel;
            case qaNo:
              CanProceed = false;
              break;

            default:
              assert(false);
          }
        }
          else CanProceed = true;
      }
        else CanProceed = true;

      if (CanProceed)
      {
        if (FTerminal->SessionData->CacheDirectories)
        {
          FTerminal->DirectoryModified(TargetDir, false);

          if (DirectoryExists(FileName))
          {
            FTerminal->DirectoryModified(UnixIncludeTrailingBackslash(TargetDir)+
              FileNameOnly, true);
          }
        }

        try
        {
          SCPSource(FileName, CopyParam, Params, OperationProgress, 0);
          OperationProgress->Finish(FileName, true, DisconnectWhenComplete);
        }
        catch (EScpFileSkipped &E)
        {
          TQueryParams Params(qpAllowContinueOnError);
          SUSPEND_OPERATION (
            if (FTerminal->DoQueryUser(FMTLOAD(COPY_ERROR, (FileName)), E.Message,
              qaOK | qaAbort, &Params, qtError) == qaAbort)
            {
              OperationProgress->Cancel = csCancel;
            }
            OperationProgress->Finish(FileName, false, DisconnectWhenComplete);
            if (!FTerminal->HandleException(&E)) throw;
          );
        }
        catch (EScpSkipFile &E)
        {
          OperationProgress->Finish(FileName, false, DisconnectWhenComplete);
          // If ESkipFile occurs, just log it and continue with next file
          SUSPEND_OPERATION (
            if (!FTerminal->HandleException(&E)) throw;
          );
        }
        catch (...)
        {
          OperationProgress->Finish(FileName, false, DisconnectWhenComplete);
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
          if (CopyBatchStarted)
          {
            // What about case, remote side sends fatal error ???
            // (Not sure, if it causes remote side to terminate scp)
            FTerminal->SendLine("E");
            SCPResponse();
          };
          /* TODO 1 : Show stderror to user? */
          FTerminal->ClearStdError();

          ReadCommandOutput(coExpectNoOutput | coWaitForLastLine | coOnlyReturnCode |
            (Failed ? 0 : coRaiseExcept));
        }
      }
      catch (Exception &E)
      {
        // Only show error message (it should always succeed, but
        // some pending error maybe in queque) }
        FTerminal->DoHandleExtendedException(&E);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SCPSource(const AnsiString FileName,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, int Level)
{
  AnsiString DestFileName = CopyParam->ChangeFileName(
    ExtractFileName(FileName), osLocal, Level == 0);

  FTerminal->LogEvent(FORMAT("File: \"%s\"", (FileName)));

  OperationProgress->SetFile(FileName);

  HANDLE File;
  int Attrs;
  __int64 MTime, ATime;
  __int64 Size;

  FTerminal->OpenLocalFile(FileName, GENERIC_READ,
    &Attrs, &File, NULL, &MTime, &ATime, &Size);

  bool Dir = FLAGSET(Attrs, faDirectory);

  if (FLAGCLEAR(Params, cpDelete) &&
      !CopyParam->AllowTransfer(FileName, osLocal, Dir))
  {
    FTerminal->LogEvent(FORMAT("File \"%s\" excluded from transfer", (FileName)));
    THROW_SKIP_FILE_NULL;
  }

  if (Dir)
  {
    SCPDirectorySource(FileName, CopyParam, Params, OperationProgress, Level);
  }
    else
  try
  {
    assert(File);
    
    // File is regular file (not directory)
    FTerminal->LogEvent(FORMAT("Copying \"%s\" to remote directory started.", (FileName)));

    OperationProgress->SetLocalSize(Size);

    // Suppose same data size to transfer as to read
    // (not true with ASCII transfer)
    OperationProgress->SetTransferSize(OperationProgress->LocalSize);
    OperationProgress->TransferingFile = false;

    // Will we use ASCII of BINARY file tranfer?
    OperationProgress->SetAsciiTransfer(CopyParam->UseAsciiTransfer(FileName, osLocal));
    FTerminal->LogEvent(
      AnsiString((OperationProgress->AsciiTransfer ? "Ascii" : "Binary")) +
        " transfer mode selected.");

    try
    {
      // During ASCII transfer we will load whole file to this buffer
      // than convert EOL and send it at once, because before converting EOL
      // we can't know its size
      TFileBuffer AsciiBuf;
      do
      {
        // Buffer for one block of data
        TFileBuffer BlockBuf;

        // This is crucial, if it fails during file transfer, it's fatal error
        FILE_OPERATION_LOOP_EX (!OperationProgress->TransferingFile,
            FMTLOAD(READ_ERROR, (FileName)),
          try
          {
            BlockBuf.LoadFile(File, OperationProgress->LocalBlockSize(), true);
          }
          catch(...)
          {
            RaiseLastOSError();
          }
        );

        OperationProgress->AddLocalyUsed(BlockBuf.Size);

        // We do ASCII transfer: convert EOL of current block
        // (we don't convert whole buffer, cause it would produce
        // huge memory-transfers while inserting/deleting EOL characters)
        // Than we add current block to file buffer
        if (OperationProgress->AsciiTransfer)
        {
          BlockBuf.Convert(FTerminal->Configuration->LocalEOLType,
            FTerminal->SessionData->EOLType, cpRemoveCtrlZ);
          BlockBuf.Memory->Seek(0, soFromBeginning);
          AsciiBuf.ReadStream(BlockBuf.Memory, BlockBuf.Size, true);
          // We don't need it any more
          BlockBuf.Memory->Clear();
          // Calculate total size to sent (assume that ratio between
          // size of source and size of EOL-transformed data would remain same)
          // First check if file contains anything (div by zero!)
          if (OperationProgress->LocalyUsed)
          {
            __int64 X = OperationProgress->LocalSize;
            X *= AsciiBuf.Size;
            X /= OperationProgress->LocalyUsed;
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
        if (!OperationProgress->TransferingFile &&
            (!OperationProgress->AsciiTransfer || OperationProgress->IsLocalyDone()))
        {
          AnsiString Buf;

          if (CopyParam->PreserveTime)
          {
            // Send last file access and modification time
            // TVarRec don't understand 'unsigned int' -> we use sprintf()
            Buf.sprintf("T%lu 0 %lu 0", static_cast<unsigned long>(MTime),
              static_cast<unsigned long>(ATime));
            FTerminal->SendLine(Buf);
            SCPResponse();
          }

          // Send file modes (rights), filesize and file name
          // TVarRec don't understand 'unsigned int' -> we use sprintf()
          Buf.sprintf("C%s %Ld %s",
            CopyParam->RemoteFileRights(Attrs).Octal.data(),
            (OperationProgress->AsciiTransfer ? (__int64)AsciiBuf.Size :
              OperationProgress->LocalSize),
            DestFileName.data());
          FTerminal->SendLine(Buf);
          SCPResponse();
          // Indicate we started transfering file, we need to finish it
          // If not, it's fatal error
          OperationProgress->TransferingFile = true;

          // If we're doing ASCII transfer, this is last pass
          // so we send whole file
          /* TODO : We can't send file above 32bit size in ASCII mode! */
          if (OperationProgress->AsciiTransfer)
          {
            FTerminal->LogEvent(FORMAT("Sending ASCII data (%ud bytes)",
              (AsciiBuf.Size)));
            // Should be equal, just in case it's rounded (see above)
            OperationProgress->ChangeTransferSize(AsciiBuf.Size);
            while (!OperationProgress->IsTransferDone())
            {
              FTerminal->Send(
                AsciiBuf.Data + (unsigned int)OperationProgress->TransferedSize,
                OperationProgress->TransferBlockSize());
              OperationProgress->AddTransfered(OperationProgress->TransferBlockSize());
            }
          }
        }

        // At end of BINARY transfer pass, send current block
        if (!OperationProgress->AsciiTransfer)
        {
          if (!OperationProgress->TransferedSize)
          {
            FTerminal->LogEvent(FORMAT("Sending BINARY data (first block, %u bytes)",
              (BlockBuf.Size)));
          }
          else if (FTerminal->Configuration->LogProtocol >= 1)
          {
            FTerminal->LogEvent(FORMAT("Sending BINARY data (%u bytes)",
              (BlockBuf.Size)));
          }
          FTerminal->Send(BlockBuf.Data, BlockBuf.Size);
          OperationProgress->AddTransfered(BlockBuf.Size);
        }

        if ((OperationProgress->Cancel == csCancelTransfer) ||
            (OperationProgress->Cancel == csCancel && !OperationProgress->TransferingFile))
        {
          throw Exception(USER_TERMINATED);
        }
      }
      while (!OperationProgress->IsLocalyDone() || !OperationProgress->IsTransferDone());

      FTerminal->SendNull();
      try
      {
        SCPResponse();
        // If one of two following exceptions occurs, is means, that remote
        // side already know, that file transfer finished, even if it failed
        // so we don't have to throw EFatal
      }
      catch (EScp &E)
      {
        // SCP protocol fatal error
        OperationProgress->TransferingFile = false;
        throw;
      }
      catch (EScpFileSkipped &E)
      {
        // SCP protocol non-fatal error
        OperationProgress->TransferingFile = false;
        throw;
      }

      // We succeded transfering file, from now we can handle exceptions
      // normally -> no fatal error
      OperationProgress->TransferingFile = false;
    }
    catch (Exception &E)
    {
      // Every exception during file transfer is fatal
      if (OperationProgress->TransferingFile)
      {
        FTerminal->FatalError(&E, FMTLOAD(COPY_FATAL, (FileName)));
      }
        else
      {
        throw;
      }
    }
  }
  __finally
  {
    CloseHandle(File);
  }

  /* TODO : Delete also read-only files. */
  /* TODO : Show error message on failure. */
  if (FLAGSET(Params, cpDelete)) 
  {
    Sysutils::DeleteFile(FileName);
  }
  else if (CopyParam->ClearArchive && FLAGSET(Attrs, faArchive))
  {
    FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (FileName)),
      THROWIFFALSE(FileSetAttr(FileName, Attrs & ~faArchive) == 0);
    )
  }

  FTerminal->LogEvent(FORMAT("Copying \"%s\" to remote directory finished.", (FileName)));
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SCPDirectorySource(const AnsiString DirectoryName,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, int Level)
{
  int Attrs;

  FTerminal->LogEvent(FORMAT("Entering directory \"%s\".", (DirectoryName)));

  OperationProgress->SetFile(DirectoryName);
  AnsiString DestFileName = CopyParam->ChangeFileName(
    ExtractFileName(DirectoryName), osLocal, Level == 0);

  // Get directory attributes
  FILE_OPERATION_LOOP (FMTLOAD(CANT_GET_ATTRS, (DirectoryName)),
    Attrs = FileGetAttr(DirectoryName);
    if (Attrs == -1) EXCEPTION;
  )

  AnsiString Buf;

  /* TODO 1: maybe send filetime */

  // Send directory modes (rights), filesize and file name
  Buf = FORMAT("D%s 0 %s",
    (CopyParam->RemoteFileRights(Attrs).Octal, DestFileName));
  FTerminal->SendLine(Buf);
  SCPResponse();

  try
  {
    int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
    TSearchRec SearchRec;
    bool FindOK;

    FILE_OPERATION_LOOP (FMTLOAD(LIST_DIR_ERROR, (DirectoryName)),
      FindOK = (bool)(FindFirst(IncludeTrailingBackslash(DirectoryName) + "*.*",
        FindAttrs, SearchRec) == 0);
    );

    while (FindOK && !OperationProgress->Cancel)
    {
      AnsiString FileName = IncludeTrailingBackslash(DirectoryName) + SearchRec.Name;
      try
      {
        if ((SearchRec.Name != ".") && (SearchRec.Name != ".."))
        {
          SCPSource(FileName, CopyParam, Params, OperationProgress, Level + 1);
        }
      }
      // Previously we catched EScpSkipFile, making error being displayed
      // even when file was excluded by mask. Now the EScpSkipFile is special
      // case without error message.
      catch (EScpFileSkipped &E)
      {
        TQueryParams Params(qpAllowContinueOnError);
        SUSPEND_OPERATION (
          if (FTerminal->DoQueryUser(FMTLOAD(COPY_ERROR, (FileName)), E.Message,
                qaOK | qaAbort, &Params, qtError) == qaAbort)
          {
            OperationProgress->Cancel = csCancel;
          }
          if (!FTerminal->HandleException(&E)) throw;
        );
      }
      catch (EScpSkipFile &E)
      {
        // If ESkipFile occurs, just log it and continue with next file
        SUSPEND_OPERATION (
          if (!FTerminal->HandleException(&E)) throw;
        );
      }

      FILE_OPERATION_LOOP (FMTLOAD(LIST_DIR_ERROR, (DirectoryName)),
        FindOK = (FindNext(SearchRec) == 0);
      );
    };

    FindClose(SearchRec);

    /* TODO : Delete also read-only directories. */
    /* TODO : Show error message on failure. */
    if (!OperationProgress->Cancel)
    {
      if (FLAGSET(Params, cpDelete)) 
      {
        RemoveDir(DirectoryName);
      }
      else if (CopyParam->ClearArchive && FLAGSET(Attrs, faArchive))
      {
        FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (DirectoryName)),
          THROWIFFALSE(FileSetAttr(DirectoryName, Attrs & ~faArchive) == 0);
        )
      }
    }
  }
  __finally
  {
    if (FTerminal->Active)
    {
      // Tell remote side, that we're done.
      FTerminal->LogEvent(FORMAT("Leaving directory \"%s\".", (DirectoryName)));
      FTerminal->SendLine("E");
      SCPResponse();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::CopyToLocal(TStrings * FilesToCopy,
  const AnsiString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  bool & DisconnectWhenComplete)
{
  bool CloseSCP = False;
  AnsiString Options = "";
  if (CopyParam->PreserveRights || CopyParam->PreserveTime) Options = "-p";
  if (FTerminal->SessionData->Scp1Compatibility) Options += " -1";

  FTerminal->LogEvent(FORMAT("Copying %d files/directories to local directory "
    "\"%s\"", (FilesToCopy->Count, TargetDir)));
  FTerminal->LogEvent(CopyParam->LogStr);

  try
  {
    for (int IFile = 0; (IFile < FilesToCopy->Count) &&
      !OperationProgress->Cancel; IFile++)
    {
      AnsiString FileName = FilesToCopy->Strings[IFile];
      TRemoteFile * File = (TRemoteFile *)FilesToCopy->Objects[IFile];
      assert(File);

      try
      {
        bool Success = true; // Have to be set to True (see ::SCPSink)
        SendCommand(FCommandSet->FullCommand(fsCopyToLocal,
          ARRAYOFCONST((Options, DelimitStr(FileName)))));
        SkipFirstLine();

        // Filename is used for error messaging and excluding files only
        // Send in full path to allow path-based excluding
        AnsiString FullFileName = UnixExcludeTrailingBackslash(File->FullFileName);
        SCPSink(TargetDir, FullFileName, UnixExtractFilePath(FullFileName),
          CopyParam, Success, OperationProgress, Params, 0);
        // operation succeded (no exception), so it's ok that
        // remote side closed SCP, but we continue with next file
        if (OperationProgress->Cancel == csRemoteAbort)
        {
          OperationProgress->Cancel = csContinue;
        }

        // Move operation -> delete file/directory afterwards
        // but only if copying succeded
        if ((Params & cpDelete) && Success && !OperationProgress->Cancel)
        {
          try
          {
            FTerminal->ExceptionOnFail = true;
            try
            {
              FILE_OPERATION_LOOP(FMTLOAD(DELETE_FILE_ERROR, (FileName)),
                // pass full file name in FileName, in case we are not moving
                // from current directory
                FTerminal->DeleteFile(FileName, File)
              );
            }
            __finally
            {
              FTerminal->ExceptionOnFail = false;
            }
          }
          catch (EFatal &E)
          {
            throw;
          }
          catch (...)
          {
            // If user selects skip (or abort), nothing special actualy occures
            // we just run DoFinished with Success = False, so file won't
            // be deselected in panel (depends on assigned event handler)

            // On csCancel we would later try to close remote SCP, but it
            // is closed already
            if (OperationProgress->Cancel == csCancel)
            {
              OperationProgress->Cancel = csRemoteAbort;
            }
            Success = false;
          }
        }

        OperationProgress->Finish(FileName,
          (!OperationProgress->Cancel && Success), DisconnectWhenComplete);
      }
      catch (...)
      {
        OperationProgress->Finish(FileName, false, DisconnectWhenComplete);
        CloseSCP = (OperationProgress->Cancel != csRemoteAbort);
        throw;
      }
    }
  }
  __finally
  {
    // In case that copying don't causes fatal error (ie. connection is
    // still active) but weren't succesful (exception or user termination)
    // we need to ensure, that SCP on remote side is closed
    if (FTerminal->Active && (CloseSCP ||
        (OperationProgress->Cancel == csCancel) ||
        (OperationProgress->Cancel == csCancelTransfer)))
    {
      bool LastLineRead;

      // If we get LastLine, it means that remote side 'scp' is already
      // terminated, so we need not to terminate it. There is also
      // possibility that remote side waits for confirmation, so it will hang.
      // This should not happen (hope)
      AnsiString Line = FTerminal->ReceiveLine();
      LastLineRead = IsLastLine(Line);
      if (!LastLineRead)
      {
        SCPSendError((OperationProgress->Cancel ? "Terminated by user." : "Exception"), true);
      }
      // Just in case, remote side already sent some more data (it's probable)
      // but we don't want to raise exception (user asked to terminate, it's not error)
      int ECParams = coOnlyReturnCode;
      if (!LastLineRead) ECParams |= coWaitForLastLine;
      ReadCommandOutput(ECParams);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SCPError(const AnsiString Message, bool Fatal)
{
  SCPSendError(Message, Fatal);
  THROW_FILE_SKIPPED(NULL, Message);
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SCPSendError(const AnsiString Message, bool Fatal)
{
  char ErrorLevel = (char)(Fatal ? 2 : 1);
  FTerminal->LogEvent(FORMAT("Sending SCP error (%d) to remote side:",
    ((int)ErrorLevel)));
  FTerminal->Send(&ErrorLevel, 1);
  // We don't send exact error message, because some unspecified
  // characters can terminate remote scp
  FTerminal->SendLine("scp: error");
}
//---------------------------------------------------------------------------
void __fastcall TSCPFileSystem::SCPSink(const AnsiString TargetDir,
  const AnsiString FileName, const AnsiString SourceDir,
  const TCopyParamType * CopyParam, bool & Success,
  TFileOperationProgressType * OperationProgress, int Params,
  int Level)
{
  struct
  {
    int SetTime;
    FILETIME AcTime;
    FILETIME WrTime;
    TRights RemoteRights;
    int Attrs;
    bool Exists;
  } FileData;
  TDateTime SourceTimestamp;

  bool SkipConfirmed = false;
  bool Initialized = (Level > 0);

  FileData.SetTime = 0;

  FTerminal->SendNull();

  while (!OperationProgress->Cancel)
  {
    // See (switch ... case 'T':)
    if (FileData.SetTime) FileData.SetTime--;

    // In case of error occured before control record arrived.
    // We can finally use full path here, as we get current path in FileName param
    // (we used to set the file into OperationProgress->FileName, but it collided
    // with progress outputing, particularly for scripting)
    AnsiString ErrorFileName = FileName;

    try
    {
      // Receive control record
      AnsiString Line = FTerminal->ReceiveLine();

      if (Line.Length() == 0) FTerminal->FatalError(LoadStr(SCP_EMPTY_LINE));

      if (IsLastLine(Line))
      {
        // Remote side finished copying, so remote SCP was closed
        // and we don't need to terminate it manualy, see CopyToLocal()
        OperationProgress->Cancel = csRemoteAbort;
        /* TODO 1 : Show stderror to user? */
        FTerminal->ClearStdError();
        try
        {
          // coIgnoreWarnings should allow batch transfer to continue when
          // download of one the files failes (user denies overwritting
          // of target local file, no read permissions...)
          ReadCommandOutput(coExpectNoOutput | coRaiseExcept |
            coOnlyReturnCode | coIgnoreWarnings);
          if (!Initialized)
          {
            throw Exception("");
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

        // First characted distinguish type of control record
        char Ctrl = Line[1];
        Line.Delete(1, 1);

        switch (Ctrl) {
          case 1:
            // Error (already logged by ReceiveLine())
            THROW_FILE_SKIPPED(NULL, FMTLOAD(REMOTE_ERROR, (Line)));

          case 2:
            // Fatal error, terminate copying
            FTerminal->TerminalError(Line);
            return; // Unreachable

          case 'E': // Exit
            FTerminal->SendNull();
            return;

          case 'T':
            unsigned long MTime, ATime;
            if (sscanf(Line.c_str(), "%ld %*d %ld %*d",  &MTime, &ATime) == 2)
            {
              FileData.AcTime = DateTimeToFileTime(UnixToDateTime(ATime,
                FTerminal->SessionData->ConsiderDST), FTerminal->SessionData->ConsiderDST);
              FileData.WrTime = DateTimeToFileTime(UnixToDateTime(MTime,
                FTerminal->SessionData->ConsiderDST), FTerminal->SessionData->ConsiderDST);
              SourceTimestamp = UnixToDateTime(MTime,
                FTerminal->SessionData->ConsiderDST);
              FTerminal->SendNull();
              // File time is only valid until next pass
              FileData.SetTime = 2;
              continue;
            }
              else
            {
              SCPError(LoadStr(SCP_ILLEGAL_TIME_FORMAT), False);
            }

          case 'C':
          case 'D':
            break; // continue pass switch{}

          default:
            FTerminal->FatalError(FMTLOAD(SCP_INVALID_CONTROL_RECORD, (Ctrl, Line)));
        }

        // We reach this point only if control record was 'C' or 'D'
        try
        {
          FileData.RemoteRights.Octal = CutToChar(Line, ' ', True);
          // do not trim leading spaces of the filename
          __int64 TSize = StrToInt64(CutToChar(Line, ' ', False).TrimRight());
          // Security fix: ensure the file ends up where we asked for it.
          // (accept only filename, not path)
          AnsiString OnlyFileName = UnixExtractFileName(Line);
          if (Line != OnlyFileName)
          {
            FTerminal->LogEvent(FORMAT("Warning: Remote host set a compound pathname '%s'", (Line)));
          }

          OperationProgress->SetFile(OnlyFileName);
          ErrorFileName = SourceDir + OnlyFileName;
          OperationProgress->SetTransferSize(TSize);
        }
        catch (Exception &E)
        {
          SUSPEND_OPERATION (
            FTerminal->DoHandleExtendedException(&E);
          );
          SCPError(LoadStr(SCP_ILLEGAL_FILE_DESCRIPTOR), false);
        }

        // last possibility to cancel transfer before it starts
        if (OperationProgress->Cancel)
        {
          THROW_SKIP_FILE(NULL, LoadStr(USER_TERMINATED));
        }

        // Security fix
        if (IsDots(OperationProgress->FileName))
        {
          FTerminal->FatalError(LoadStr(ATTEMPT_TO_WRITE_TO_PARENT_DIR));
        }

        bool Dir = (Ctrl == 'D');
        AnsiString SourceFullName = SourceDir + OperationProgress->FileName;
        if (FLAGCLEAR(Params, cpDelete) &&
            !CopyParam->AllowTransfer(SourceFullName, osRemote, Dir))
        {
          FTerminal->LogEvent(FORMAT("File \"%s\" excluded from transfer",
            (ErrorFileName)));
          SkipConfirmed = true;
          SCPError("", false);
        }

        AnsiString DestFileName =
          IncludeTrailingBackslash(TargetDir) +
          CopyParam->ChangeFileName(OperationProgress->FileName, osRemote,
            Level == 0);

        FileData.Attrs = FileGetAttr(DestFileName);
        // If getting attrs failes, we suppose, that file/folder doesn't exists
        FileData.Exists = (FileData.Attrs != -1);
        if (Dir)
        {
          if (FileData.Exists && !(FileData.Attrs & faDirectory))
          {
            SCPError(FMTLOAD(NOT_DIRECTORY_ERROR, (DestFileName)), false);
          }

          if (!FileData.Exists)
          {
            FILE_OPERATION_LOOP (FMTLOAD(CREATE_DIR_ERROR, (DestFileName)),
              if (!ForceDirectories(DestFileName)) EXCEPTION;
            );
            /* SCP: can we set the timestamp for directories ? */
          }
          AnsiString FullFileName = SourceDir + OperationProgress->FileName;
          SCPSink(DestFileName, FullFileName, UnixIncludeTrailingBackslash(FullFileName),
            CopyParam, Success, OperationProgress, Params, Level + 1);
          continue;
        }
          else
        if (Ctrl == 'C')
        {
          HANDLE File = NULL;
          TStream * FileStream = NULL;

          /* TODO 1 : Turn off read-only attr */

          try
          {
            try
            {
              if (FileExists(DestFileName))
              {
                int Answer;
                if (OperationProgress->NoToAll)
                {
                  Answer = qaNo;
                }
                // if overwrite confirmation is required or
                // only newer files should be transfered, get both file timestamps
                else if (FLAGSET(Params, cpNewerOnly) ||
                    (!OperationProgress->YesToAll &&
                     FTerminal->Configuration->ConfirmOverwriting &&
                     FLAGCLEAR(Params, cpNoConfirmation)))
                {
                  __int64 MTime;
                  TOverwriteFileParams FileParams;
                  FileParams.SourceSize = OperationProgress->TransferSize;
                  FileParams.SourceTimestamp = SourceTimestamp;
                  FTerminal->OpenLocalFile(DestFileName, GENERIC_READ,
                    NULL, NULL, NULL, &MTime, NULL,
                    &FileParams.DestSize);
                  FileParams.DestTimestamp = UnixToDateTime(MTime,
                    FTerminal->SessionData->ConsiderDST);

                  TQueryButtonAlias Aliases[1];
                  Aliases[0].Button = qaAll;
                  Aliases[0].Alias = LoadStr(YES_TO_NEWER_BUTTON);
                  TQueryParams Params(qpNeverAskAgainCheck);
                  Params.Aliases = Aliases;
                  Params.AliasesCount = LENOF(Aliases);
                  SUSPEND_OPERATION (
                    Answer = FTerminal->ConfirmFileOverwrite(
                      OperationProgress->FileName, &FileParams,
                      qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll | qaAll,
                      &Params, osLocal, OperationProgress);
                  );
                }

                switch (Answer)
                {
                  case qaCancel: OperationProgress->Cancel = csCancel; // continue on next case
                  case qaNo: SkipConfirmed = true; EXCEPTION;
                }
              }

              if (!FTerminal->CreateLocalFile(DestFileName, OperationProgress,
                     &File, FLAGSET(Params, cpNoConfirmation)))
              {
                SkipConfirmed = true;
                EXCEPTION;
              }

              FileStream = new THandleStream((THandle)File);
            }
            catch (Exception &E)
            {
              // In this step we can still cancel transfer, so we do it
              SCPError(E.Message, false);
              throw;
            }

            // We succeded, so we confirm transfer to remote side
            FTerminal->SendNull();
            // From now we need to finish file transfer, if not it's fatal error
            OperationProgress->TransferingFile = true;

            // Suppose same data size to transfer as to write
            // (not true with ASCII transfer)
            OperationProgress->SetLocalSize(OperationProgress->TransferSize);

            // Will we use ASCII of BINARY file tranfer?
            OperationProgress->SetAsciiTransfer(
              CopyParam->UseAsciiTransfer(SourceFullName, osRemote));
            FTerminal->LogEvent(AnsiString((OperationProgress->AsciiTransfer ? "Ascii" : "Binary")) +
              " transfer mode selected.");

            try
            {
              // Buffer for one block of data
              TFileBuffer BlockBuf;

              do
              {
                BlockBuf.Size = OperationProgress->TransferBlockSize();
                BlockBuf.Position = 0;

                FTerminal->Receive(BlockBuf.Data, BlockBuf.Size);
                OperationProgress->AddTransfered(BlockBuf.Size);

                if (OperationProgress->AsciiTransfer)
                {
                  unsigned int PrevBlockSize = BlockBuf.Size;
                  BlockBuf.Convert(FTerminal->SessionData->EOLType,
                    FTerminal->Configuration->LocalEOLType, 0);
                  OperationProgress->SetLocalSize(
                    OperationProgress->LocalSize - PrevBlockSize + BlockBuf.Size);
                }

                // This is crucial, if it fails during file transfer, it's fatal error
                FILE_OPERATION_LOOP_EX (false, FMTLOAD(WRITE_ERROR, (DestFileName)),
                  try
                  {
                    BlockBuf.WriteToStream(FileStream, BlockBuf.Size);
                  }
                  catch(...)
                  {
                    RaiseLastOSError();
                  }
                );

                OperationProgress->AddLocalyUsed(BlockBuf.Size);

                if (OperationProgress->Cancel == csCancelTransfer)
                {
                  throw Exception(USER_TERMINATED);
                }
              }
              while (!OperationProgress->IsLocalyDone() || !
                  OperationProgress->IsTransferDone());
            }
            catch (Exception &E)
            {
              // Every exception during file transfer is fatal
              FTerminal->FatalError(&E,
                FMTLOAD(COPY_FATAL, (OperationProgress->FileName)));
            }

            OperationProgress->TransferingFile = false;

            try
            {
              SCPResponse();
              // If one of following exception occurs, we still need
              // to send confirmation to other side
            }
            catch (EScp &E)
            {
              FTerminal->SendNull();
              throw;
            }
            catch (EScpFileSkipped &E)
            {
              FTerminal->SendNull();
              throw;
            }

            FTerminal->SendNull();

            if (FileData.SetTime && CopyParam->PreserveTime)
            {
              SetFileTime(File, NULL, &FileData.AcTime, &FileData.WrTime);
            }
          }
          __finally
          {
            if (File) CloseHandle(File);
            if (FileStream) delete FileStream;
          }

          if (FileData.Attrs == -1) FileData.Attrs = faArchive;
          int NewAttrs = CopyParam->LocalFileAttrs(FileData.RemoteRights);
          if ((NewAttrs & FileData.Attrs) != NewAttrs)
          {
            FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (DestFileName)),
              FileSetAttr(DestFileName, FileData.Attrs | NewAttrs);
            );
          }
        }
      }
    }
    catch (EScpFileSkipped &E)
    {
      if (!SkipConfirmed)
      {
        SUSPEND_OPERATION (
          TQueryParams Params(qpAllowContinueOnError);
          if (FTerminal->DoQueryUser(FMTLOAD(COPY_ERROR, (ErrorFileName)),
            E.Message, qaOK | qaAbort, &Params, qtError) == qaAbort)
          {
            OperationProgress->Cancel = csCancel;
          }
          FTerminal->DoHandleExtendedException(&E);
        );
      }
      // this was inside above condition, but then transfer was considered
      // succesfull, even when for example user refused to overwrite file
      Success = false;
    }
    catch (EScpSkipFile &E)
    {
      SCPSendError(E.Message, false);
      Success = false;
      if (!FTerminal->HandleException(&E)) throw;
    }
  }
}


