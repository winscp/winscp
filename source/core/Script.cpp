//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <SysUtils.hpp>

#include "Common.h"
#include "Exceptions.h"
#include "TextsCore.h"
#include "Script.h"
#include "Terminal.h"
#include "SessionData.h"
#include "CoreMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const wchar_t * ToggleNames[] = { L"off", L"on" };
const UnicodeString InOutParam(TraceInitStr(L"-"));
//---------------------------------------------------------------------------
__fastcall TScriptProcParams::TScriptProcParams(const UnicodeString & FullCommand, const UnicodeString & ParamsStr)
{
  int P = FSwitchMarks.Pos(L"/");
  DebugAssert(P > 0);
  if (P > 0)
  {
    FSwitchMarks.Delete(P, 1);
  }

  FFullCommand = FullCommand;
  FParamsStr = ParamsStr;
  Parse(ParamsStr);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TScriptCommands : TStringList
{
public:
  typedef void __fastcall (__closure *TCommandProc)(TScriptProcParams * Parameters);

  __fastcall TScriptCommands(TScript * Script);
  virtual __fastcall ~TScriptCommands();

  void __fastcall Execute(const UnicodeString & Command, const UnicodeString & Params);
  UnicodeString __fastcall ResolveCommand(const UnicodeString & Command);

  void __fastcall Register(const wchar_t * Command,
    const UnicodeString Description, const UnicodeString Help, TCommandProc Proc,
    int MinParams, int MaxParams, bool Switches);
  void __fastcall Register(const wchar_t * Command,
    int Description, int Help, TCommandProc Proc,
    int MinParams, int MaxParams, bool Switches);

  bool __fastcall Info(const UnicodeString Command,
    UnicodeString * Description, UnicodeString * Help);
  bool __fastcall Enumerate(int Index,
    UnicodeString * Command, UnicodeString * Description, UnicodeString * Help);
  static int __fastcall FindCommand(TStrings * Commands, const UnicodeString Command,
    UnicodeString * Matches = NULL);
  static int __fastcall FindCommand(const wchar_t ** Commands, size_t Count,
    const UnicodeString Command, UnicodeString * Matches = NULL);

  static void __fastcall CheckParams(TOptions * Parameters, bool Switches);

protected:
  struct TScriptCommand
  {
    UnicodeString Description;
    UnicodeString Help;
    TCommandProc Proc;
    int MinParams;
    int MaxParams;
    bool Switches;
  };

  TScript * FScript;
};
//---------------------------------------------------------------------------
__fastcall TScriptCommands::TScriptCommands(TScript * Script)
{
  FScript = Script;
  Sorted = true;
  CaseSensitive = false;
}
//---------------------------------------------------------------------------
__fastcall TScriptCommands::~TScriptCommands()
{
  for (int Index = 0; Index < Count; Index++)
  {
    delete reinterpret_cast<TScriptCommand *>(Objects[Index]);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScriptCommands::Register(const wchar_t * Command,
  const UnicodeString Description, const UnicodeString Help, TCommandProc Proc,
  int MinParams, int MaxParams, bool Switches)
{
  TScriptCommand * ScriptCommand = new TScriptCommand;
  ScriptCommand->Description = Description;
  ScriptCommand->Help = Help;
  ScriptCommand->Proc = Proc;
  ScriptCommand->MinParams = MinParams;
  ScriptCommand->MaxParams = MaxParams;
  ScriptCommand->Switches = Switches;

  AddObject(Command, reinterpret_cast<TObject *>(ScriptCommand));
}
//---------------------------------------------------------------------------
void __fastcall TScriptCommands::Register(const wchar_t * Command,
  int Description, int Help, TCommandProc Proc,
  int MinParams, int MaxParams, bool Switches)
{
  UnicodeString ADescription;
  if (Description > 0)
  {
    ADescription = LoadStr(Description);
  }
  UnicodeString AHelp;
  if (Help > 0)
  {
    AHelp = LoadStr(Help, 10240);
  }

  Register(Command, ADescription, AHelp, Proc, MinParams, MaxParams, Switches);
}
//---------------------------------------------------------------------------
bool __fastcall TScriptCommands::Info(const UnicodeString Command,
  UnicodeString * Description, UnicodeString * Help)
{
  int Index = FindCommand(this, Command);
  bool Result = (Index >= 0);

  if (Result)
  {
    TScriptCommand * ScriptCommand = reinterpret_cast<TScriptCommand *>(Objects[Index]);
    if (Description != NULL)
    {
      *Description = ScriptCommand->Description;
    }

    if (Help != NULL)
    {
      *Help = ScriptCommand->Help;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TScriptCommands::Enumerate(int Index,
  UnicodeString * Command, UnicodeString * Description, UnicodeString * Help)
{
  bool Result = (Index < Count);

  if (Result)
  {
    TScriptCommand * ScriptCommand = reinterpret_cast<TScriptCommand *>(Objects[Index]);
    if (Command != NULL)
    {
      *Command = Strings[Index];
    }

    if (Description != NULL)
    {
      *Description = ScriptCommand->Description;
    }

    if (Help != NULL)
    {
      *Help = ScriptCommand->Help;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TScriptCommands::FindCommand(TStrings * Commands,
  const UnicodeString Command, UnicodeString * Matches)
{
  int Result = Commands->IndexOf(Command);

  if (Result < 0)
  {
    int MatchesCount = 0;

    for (int i = 0; i < Commands->Count; i++)
    {
      if ((Command.Length() <= Commands->Strings[i].Length()) &&
          SameText(Command, Commands->Strings[i].SubString(1, Command.Length())))
      {
        if (Matches != NULL)
        {
          if (!Matches->IsEmpty())
          {
            *Matches += L", ";
          }
          *Matches += Commands->Strings[i];
        }
        MatchesCount++;
        Result = i;
      }
    }

    if (MatchesCount == 0)
    {
      Result = -1;
    }
    else if (MatchesCount > 1)
    {
      Result = -2;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TScriptCommands::FindCommand(const wchar_t ** Commands, size_t Count,
  const UnicodeString Command, UnicodeString * Matches)
{
  int Result;
  TStringList * Strings = new TStringList;
  try
  {
    Strings->CaseSensitive = false;

    for (unsigned int i = 0; i < Count; i++)
    {
      Strings->Add(Commands[i]);
    }

    Result = FindCommand(Strings, Command, Matches);
  }
  __finally
  {
    delete Strings;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScriptCommands::CheckParams(TOptions * Parameters,
  bool Switches)
{
  UnicodeString Switch;
  if (!Switches && Parameters->UnusedSwitch(Switch))
  {
    throw Exception(FMTLOAD(SCRIPT_UNKNOWN_SWITCH, (Switch)));
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TScriptCommands::ResolveCommand(const UnicodeString & Command)
{
  UnicodeString Matches;
  int Index = FindCommand(this, Command, &Matches);
  if (Index >= 0)
  {
    return Strings[Index];
  }
  else
  {
    return UnicodeString();
  }
}
//---------------------------------------------------------------------------
// parameters are by purpose passed by (constant) reference.
// because if passed by value (copy), UnicodeString reference is not for some reason
// decreased on exit by exception, leading to memory leak
void __fastcall TScriptCommands::Execute(const UnicodeString & Command, const UnicodeString & Params)
{
  UnicodeString Matches;
  int Index = FindCommand(this, Command, &Matches);

  if (Index == -2)
  {
    throw Exception(FMTLOAD(SCRIPT_COMMAND_AMBIGUOUS, (Command, Matches)));
  }
  else if (Index < 0)
  {
    throw Exception(FMTLOAD(SCRIPT_COMMAND_UNKNOWN, (Command)));
  }

  TScriptCommand * ScriptCommand = reinterpret_cast<TScriptCommand *>(Objects[Index]);
  UnicodeString FullCommand = Strings[Index];

  std::unique_ptr<TScriptProcParams> Parameters(new TScriptProcParams(FullCommand, Params));
  if (Parameters->ParamCount < ScriptCommand->MinParams)
  {
    throw Exception(FMTLOAD(SCRIPT_MISSING_PARAMS, (FullCommand)));
  }
  else if ((ScriptCommand->MaxParams >= 0) && (Parameters->ParamCount > ScriptCommand->MaxParams))
  {
    throw Exception(FMTLOAD(SCRIPT_TOO_MANY_PARAMS, (FullCommand)));
  }
  else
  {
    CheckParams(Parameters.get(), ScriptCommand->Switches);

    ScriptCommand->Proc(Parameters.get());
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// keep in sync with Session constructor in .NET
const int BatchSessionReopenTimeout = 2 * MSecsPerSec * SecsPerMin; // 2 mins
//---------------------------------------------------------------------------
__fastcall TScript::TScript(bool LimitedOutput)
{
  FLimitedOutput = LimitedOutput;
  FTerminal = NULL;
  FLoggingTerminal = NULL;
  FGroups = false;
  FWantsProgress = false;
  FUsageWarnings = true;
  FOnTransferOut = NULL;
  FOnTransferIn = NULL;
  FIncludeFileMaskOptionUsed = false;
  FPendingLogLines = new TStringList();
  FPrintInformation = false;

  Init();
}
//---------------------------------------------------------------------------
__fastcall TScript::~TScript()
{
  delete FCommands;
  delete FPendingLogLines;
}
//---------------------------------------------------------------------------
void __fastcall TScript::Init()
{
  FBatch = BatchAbort;
  FInteractiveBatch = BatchOff;
  FConfirm = false;
  FInteractiveConfirm = true;
  FSessionReopenTimeout = Configuration->SessionReopenTimeout;
  FInteractiveSessionReopenTimeout = FSessionReopenTimeout;
  if (FSessionReopenTimeout == 0)
  {
    FSessionReopenTimeout = BatchSessionReopenTimeout;
  }

  FEcho = false;
  FFailOnNoMatch = false;
  FSynchronizeParams = 0;
  FOnPrint = NULL;
  FOnTerminalSynchronizeDirectory = NULL;
  FOnSynchronizeStartStop = NULL;
  FSynchronizeMode = -1;
  FKeepingUpToDate = false;
  FWarnNonDefaultCopyParam = false;
  FWarnNonDefaultSynchronizeParams = false;

  FCommands = new TScriptCommands(this);
  FCommands->Register(L"help", SCRIPT_HELP_DESC, SCRIPT_HELP_HELP, &HelpProc, 0, -1, false);
  FCommands->Register(L"man", 0, SCRIPT_HELP_HELP, &HelpProc, 0, -1, false);
  // the call command does not have switches itself, but the commands may have
  FCommands->Register(L"call", SCRIPT_CALL_DESC2, SCRIPT_CALL_HELP2, &CallProc, 1, -1, true);
  FCommands->Register(L"!", 0, SCRIPT_CALL_HELP2, &CallProc, 1, -1, true);
  FCommands->Register(L"pwd", SCRIPT_PWD_DESC, SCRIPT_PWD_HELP, &PwdProc, 0, 0, false);
  FCommands->Register(L"cd", SCRIPT_CD_DESC, SCRIPT_CD_HELP, &CdProc, 0, 1, false);
  FCommands->Register(L"ls", SCRIPT_LS_DESC, SCRIPT_LS_HELP2, &LsProc, 0, 1, false);
  FCommands->Register(L"dir", 0, SCRIPT_LS_HELP2, &LsProc, 0, 1, false);
  FCommands->Register(L"rm", SCRIPT_RM_DESC, SCRIPT_RM_HELP2, &RmProc, 1, -1, true);
  FCommands->Register(L"rmdir", SCRIPT_RMDIR_DESC, SCRIPT_RMDIR_HELP, &RmDirProc, 1, -1, false);
  FCommands->Register(L"mv", SCRIPT_MV_DESC, SCRIPT_MV_HELP2, &MvProc, 2, -1, false);
  FCommands->Register(L"rename", 0, SCRIPT_MV_HELP2, &MvProc, 2, -1, false);
  FCommands->Register(L"cp", SCRIPT_CP_DESC, SCRIPT_CP_HELP, &CpProc, 2, -1, false);
  FCommands->Register(L"chmod", SCRIPT_CHMOD_DESC, SCRIPT_CHMOD_HELP2, &ChModProc, 2, -1, false);
  FCommands->Register(L"ln", SCRIPT_LN_DESC, SCRIPT_LN_HELP, &LnProc, 2, 2, false);
  FCommands->Register(L"symlink", 0, SCRIPT_LN_HELP, &LnProc, 2, 2, false);
  FCommands->Register(L"mkdir", SCRIPT_MKDIR_DESC, SCRIPT_MKDIR_HELP, &MkDirProc, 1, 1, false);
  FCommands->Register(L"get", SCRIPT_GET_DESC, SCRIPT_GET_HELP8, &GetProc, 0, -1, true);
  FCommands->Register(L"recv", 0, SCRIPT_GET_HELP8, &GetProc, 0, -1, true);
  FCommands->Register(L"mget", 0, SCRIPT_GET_HELP8, &GetProc, 0, -1, true);
  FCommands->Register(L"put", SCRIPT_PUT_DESC, SCRIPT_PUT_HELP8, &PutProc, 0, -1, true);
  FCommands->Register(L"send", 0, SCRIPT_PUT_HELP8, &PutProc, 0, -1, true);
  FCommands->Register(L"mput", 0, SCRIPT_PUT_HELP8, &PutProc, 0, -1, true);
  FCommands->Register(L"option", SCRIPT_OPTION_DESC, SCRIPT_OPTION_HELP7, &OptionProc, -1, 2, false);
  FCommands->Register(L"ascii", 0, SCRIPT_OPTION_HELP7, &AsciiProc, 0, 0, false);
  FCommands->Register(L"binary", 0, SCRIPT_OPTION_HELP7, &BinaryProc, 0, 0, false);
  FCommands->Register(L"synchronize", SCRIPT_SYNCHRONIZE_DESC, SCRIPT_SYNCHRONIZE_HELP7, &SynchronizeProc, 0, -1, true);
  FCommands->Register(L"keepuptodate", SCRIPT_KEEPUPTODATE_DESC, SCRIPT_KEEPUPTODATE_HELP5, &KeepUpToDateProc, 0, 2, true);
  // the echo command does not have switches actually, but it must handle dashes in its arguments
  FCommands->Register(L"echo", SCRIPT_ECHO_DESC, SCRIPT_ECHO_HELP, &EchoProc, -1, -1, true);
  FCommands->Register(L"stat", SCRIPT_STAT_DESC, SCRIPT_STAT_HELP, &StatProc, 1, 1, false);
  FCommands->Register(L"checksum", SCRIPT_CHECKSUM_DESC, SCRIPT_CHECKSUM_HELP, &ChecksumProc, 2, 2, false);
  FCommands->Register(COPYID_COMMAND, 0, 0, &CopyIdProc, 1, 1, false);
}
//---------------------------------------------------------------------------
void __fastcall TScript::RequireParams(TScriptProcParams * Parameters, int MinParams)
{
  if (Parameters->ParamCount < MinParams)
  {
    throw Exception(FMTLOAD(SCRIPT_MISSING_PARAMS, (Parameters->FullCommand)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::CheckDefaultCopyParam()
{
  if (FWarnNonDefaultCopyParam)
  {
    // started with non-factory settings and still have them, warn
    if (HasNonDefaultCopyParams())
    {
      PrintLine(LoadStr(SCRIPT_NON_DEFAULT_COPY_PARAM));
    }
    FWarnNonDefaultCopyParam = false;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TScript::HasNonDefaultCopyParams()
{
  return !(FCopyParam == TCopyParamType());
}
//---------------------------------------------------------------------------
void __fastcall TScript::SetCopyParam(const TCopyParamType & value)
{
  FCopyParam.Assign(&value);
  FWarnNonDefaultCopyParam = HasNonDefaultCopyParams();
}
//---------------------------------------------------------------------------
void __fastcall TScript::CheckDefaultSynchronizeParams()
{
  if (FWarnNonDefaultSynchronizeParams)
  {
    // as opposite to CheckDefaultCopyParam(), not checking
    // current params as we cannot override any of those we accept anyway
    PrintLine(LoadStr(SCRIPT_NON_DEFAULT_SYNC_PARAM));
    FWarnNonDefaultSynchronizeParams = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::SetSynchronizeParams(int value)
{
  const int AcceptedParams =
    TTerminal::spExistingOnly | TTerminal::spTimestamp |
    TTerminal::spNotByTime | TTerminal::spBySize | TTerminal::spByChecksum | TTerminal::spCaseSensitive;
  FSynchronizeParams = (value & AcceptedParams);
  FWarnNonDefaultSynchronizeParams =
    (FSynchronizeParams != (TTerminal::spDefault & AcceptedParams));
}
//---------------------------------------------------------------------------
bool __fastcall TScript::IsTerminalLogging(TTerminal * ATerminal)
{
  return (ATerminal != NULL) && ATerminal->Log->Logging;
}
//---------------------------------------------------------------------------
const static UnicodeString ScriptLogFormat(L"Script: %s");
void __fastcall TScript::Log(TLogLineType Type, const UnicodeString & AStr, TTerminal * ATerminal)
{
  UnicodeString Str = FORMAT(ScriptLogFormat, (AStr));
  TTerminal * LoggingTerminal = (ATerminal != NULL ? ATerminal : (FLoggingTerminal != NULL ? FLoggingTerminal : Terminal));
  if (IsTerminalLogging(LoggingTerminal))
  {
    LoggingTerminal->Log->Add(Type, Str);
  }
  else if (Configuration->Logging)
  {
    FPendingLogLines->AddObject(Str, reinterpret_cast<TObject *>(Type));
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::LogOption(const UnicodeString & LogStr)
{
  Log(llInput, LogStr);
}
//---------------------------------------------------------------------------
void __fastcall TScript::LogPendingLines(TTerminal * ATerminal)
{
  if (IsTerminalLogging(ATerminal) && (FPendingLogLines->Count > 0))
  {
    // not using Log(), as we want to log to ATerminal, not Terminal,
    // what is different here, as we are called from TManagementScript::Connect()
    ATerminal->Log->Add(llMessage, FORMAT(ScriptLogFormat, (L"Retrospectively logging previous script records:")));

    for (int Index = 0; Index < FPendingLogLines->Count; Index++)
    {
      ATerminal->Log->Add(
        static_cast<TLogLineType>(reinterpret_cast<uintptr_t>(FPendingLogLines->Objects[Index])),
        FPendingLogLines->Strings[Index]);
    }
    FPendingLogLines->Clear();
    ATerminal->Log->AddSeparator();
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TScript::GetLogCmd(const UnicodeString & FullCommand,
  const UnicodeString & /*Command*/, const UnicodeString & /*Params*/)
{
  return FullCommand;
}
//---------------------------------------------------------------------------
void __fastcall TScript::StartInteractive()
{
  FBatch = FInteractiveBatch;
  FConfirm = FInteractiveConfirm;
  FSessionReopenTimeout = FInteractiveSessionReopenTimeout;
}
//---------------------------------------------------------------------------
void __fastcall TScript::Command(UnicodeString Cmd)
{
  try
  {
    if (!Cmd.Trim().IsEmpty() && (Cmd[1] != L';') && (Cmd[1] != L'#'))
    {
      UnicodeString FullCmd = Cmd;
      UnicodeString Command;
      if (CutToken(Cmd, Command))
      {
        UnicodeString LogCmd = GetLogCmd(FullCmd, Command, Cmd);
        Log(llInput, LogCmd);

        if (Configuration->ActualLogProtocol >= 1)
        {
          UnicodeString LogCmdParams = LogCmd;
          UnicodeString DummyLogCmd;
          if (DebugAlwaysTrue(CutToken(LogCmdParams, DummyLogCmd)))
          {
            std::unique_ptr<TScriptProcParams> Parameters(new TScriptProcParams(FCommands->ResolveCommand(Cmd), LogCmdParams));
            Parameters->LogOptions(LogOption);
          }
        }

        if (FEcho)
        {
          PrintLine(LogCmd);
        }

        TTerminal * BeforeGroupTerminal = FGroups ? Terminal : NULL;
        if (BeforeGroupTerminal != NULL)
        {
          BeforeGroupTerminal->ActionLog->BeginGroup(LogCmd);
        }
        int ASessionReopenTimeout = Configuration->SessionReopenTimeout;
        try
        {
          Configuration->SessionReopenTimeout = FSessionReopenTimeout;
          try
          {
            FCommands->Execute(Command, Cmd);
          }
          catch(Exception & E)
          {
            // seemingly duplicate (to the method-level one) catch clause,
            // ensures the <failure/> tag is enclosed in <group/> tag
            if (!HandleExtendedException(&E))
            {
              throw;
            }
          }
        }
        __finally
        {
          Configuration->SessionReopenTimeout = ASessionReopenTimeout;

          TTerminal * AfterGroupTerminal = FGroups ? Terminal : NULL;
          if (AfterGroupTerminal != NULL)
          {
            // this happens for "open" command
            if (AfterGroupTerminal != BeforeGroupTerminal)
            {
              AfterGroupTerminal->ActionLog->BeginGroup(LogCmd);
            }
            AfterGroupTerminal->ActionLog->EndGroup();
          }
        }
      }
    }
  }
  catch(Exception & E)
  {
    if (!HandleExtendedException(&E))
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
TStrings * __fastcall TScript::CreateFileList(TScriptProcParams * Parameters, int Start,
  int End, TFileListType ListType)
{
  TStrings * Result = new TStringList();
  try
  {
    TStringList * FileLists = NULL;
    try
    {
      for (int i = Start; i <= End; i++)
      {
        UnicodeString FileName = Parameters->Param[i];

        if (SimpleUnixExcludeTrailingBackslash(FileName) != FileName)
        {
          PrintLine(LoadStr(SCRIPT_AMBIGUOUS_SLASH_IN_PATH));
        }

        if (FLAGSET(ListType, fltDirectories))
        {
          TRemoteFile * File = new TRemoteFile();
          File->FileName = FileName;
          File->Type = FILETYPE_DIRECTORY;
          Result->AddObject(FileName, File);
        }
        else if (FLAGSET(ListType, fltMask) && TFileMasks::IsMask(FileName))
        {
          UnicodeString FileDirectory = UnixExtractFilePath(FileName);
          UnicodeString Directory = FileDirectory;
          if (Directory.IsEmpty())
          {
            Directory = UnixIncludeTrailingBackslash(FTerminal->CurrentDirectory);
          }
          TRemoteFileList * FileList = NULL;
          if (FileLists != NULL)
          {
            int Index = FileLists->IndexOf(Directory);
            if (Index > 0)
            {
              FileList = dynamic_cast<TRemoteFileList *>(FileLists->Objects[Index]);
            }
          }
          if (FileList == NULL)
          {
            FileList = FTerminal->CustomReadDirectoryListing(Directory, false);
            if (FileLists == NULL)
            {
              FileLists = new TStringList();
              FileLists->OwnsObjects = true;
            }
            FileLists->AddObject(Directory, FileList);
          }

          TFileMasks Mask;
          Mask.SetMask(UnixExtractFileName(FileName));
          bool AnyFound = false;

          // Can happen in "batch continue" mode
          if (FileList != NULL)
          {
            for (int i = 0; i < FileList->Count; i++)
            {
              TRemoteFile * File = FileList->Files[i];
              TFileMasks::TParams Params;
              Params.Size = File->Size;
              Params.Modification = File->Modification;
              if (IsRealFile(File->FileName) &&
                  Mask.MatchesFileName(File->FileName, false, &Params))
              {
                Result->AddObject(FileDirectory + File->FileName,
                  FLAGSET(ListType, fltQueryServer) ? File->Duplicate() : NULL);
                AnyFound = true;
              }
            }
          }

          if (!AnyFound)
          {
            NoMatch(Mask.Masks, UnicodeString());
          }
        }
        else
        {
          TRemoteFile * File = NULL;
          if (FLAGSET(ListType, fltQueryServer))
          {
            FTerminal->ExceptionOnFail = true;
            try
            {
              File = FTerminal->ReadFile(UnixExcludeTrailingBackslash(FileName));
              if (!File->HaveFullFileName)
              {
                File->FullFileName = FileName;
              }
            }
            __finally
            {
              FTerminal->ExceptionOnFail = false;
            }
          }
          Result->AddObject(FileName, File);
        }
      }
    }
    __finally
    {
      delete FileLists;
    }

    if (FLAGSET(ListType, fltLatest) && (Result->Count > 1))
    {
      // otherwise we do not have TRemoteFile's
      DebugAssert(FLAGSET(ListType, fltQueryServer));
      int LatestIndex = 0;

      for (int Index = 1; Index < Result->Count; Index++)
      {
        TRemoteFile * File = dynamic_cast<TRemoteFile *>(Result->Objects[Index]);
        if (dynamic_cast<TRemoteFile *>(Result->Objects[LatestIndex])->Modification < File->Modification)
        {
          LatestIndex = Index;
        }
      }

      TRemoteFile * File = dynamic_cast<TRemoteFile *>(Result->Objects[LatestIndex]);
      UnicodeString Path = Result->Strings[LatestIndex];
      Result->Delete(LatestIndex);
      FreeFiles(Result);
      Result->Clear();
      Result->AddObject(Path, File);
    }

    if (FLAGSET(ListType, fltOnlyFile))
    {
      for (int Index = 0; Index < Result->Count; Index++)
      {
        TRemoteFile * File = dynamic_cast<TRemoteFile *>(Result->Objects[Index]);
        if (File->IsDirectory)
        {
          throw Exception(FMTLOAD(NOT_FILE_ERROR, (File->FileName)));
        }
      }
    }
  }
  catch (...)
  {
    FreeFileList(Result);
    throw;
  }

  return Result;
}
//---------------------------------------------------------------------------
TStrings * __fastcall TScript::CreateLocalFileList(TScriptProcParams * Parameters,
  int Start, int End, TFileListType ListType)
{
  TStringList * Result = new TStringList();
  try
  {
    Result->OwnsObjects = true;
    UnicodeString LatestFileName;
    TDateTime LatestModification; // initialized to 0

    for (int i = Start; i <= End; i++)
    {
      // FindFirstFile called (indirectly) below fails if path ends with slash.
      // (it actually won't make a difference functionally as we fall back to adding
      // the path as is in "else" branch, but the comment "let it fail later" won't stand)
      UnicodeString FileName = ExcludeTrailingBackslash(Parameters->Param[i]);

      if (FileName != Parameters->Param[i])
      {
        PrintLine(LoadStr(SCRIPT_AMBIGUOUS_SLASH_IN_PATH));
      }

      if (FLAGSET(ListType, fltMask))
      {
        TSearchRecOwned SearchRec;
        int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
        UnicodeString Error;
        bool AnyFound = false;
        if (FindFirstUnchecked(FileName, FindAttrs, SearchRec) == 0)
        {
          do
          {
            if (SearchRec.IsRealFile())
            {
              UnicodeString FileName = SearchRec.GetFilePath();
              TLocalFile * LocalFile = new TLocalFile;
              CopySearchRec(SearchRec, LocalFile->SearchRec);
              Result->AddObject(FileName, LocalFile);
              if (SearchRec.TimeStamp > LatestModification)
              {
                LatestFileName = FileName;
                LatestModification = SearchRec.TimeStamp;
              }
              AnyFound = true;
            }
          }
          while (FindNextChecked(SearchRec) == 0);
        }
        else
        {
          if (FileName.LastDelimiter(L"?*") == 0)
          {
            // No match, and it is not a mask, let it fail latter.
            // But with -latest, we have to fail straight away
            // (so maybe we should fail unconditionally now,
            // once we need to have the code in place anyway)
            if (FLAGSET(ListType, fltLatest))
            {
              throw Exception(FMTLOAD(FILE_NOT_EXISTS, (FileName)));
            }
            Result->Add(FileName);
            AnyFound = true;
          }
          else
          {
            Error = ListingSysErrorMessage();
          }
        }

        if (!AnyFound)
        {
          NoMatch(ExtractFileName(FileName), Error);
        }
      }
      else
      {
        DebugAssert(FLAGCLEAR(ListType, fltLatest));
        // this branch is currently never used
        Result->Add(FileName);
      }
    }

    if (FLAGSET(ListType, fltLatest))
    {
      Result->Clear();
      if (!LatestFileName.IsEmpty())
      {
        Result->Add(LatestFileName);
      }
    }
  }
  catch(...)
  {
    delete Result;
    throw;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TScript::ListingSysErrorMessage()
{
  UnicodeString Result;
  int LastError = GetLastError();
  // System error text for ERROR_FILE_NOT_FOUND is more or less redundant to ours
  // SCRIPT_MATCH_NO_MATCH. Also the system error does not look nice/user friendly
  // so avoid using it for this frequent case.
  if (LastError != ERROR_FILE_NOT_FOUND)
  {
    Result = SysErrorMessageForError(LastError);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScript::NoMatch(const UnicodeString & Message)
{
  if (FFailOnNoMatch)
  {
    throw Exception(Message);
  }
  else
  {
    PrintLine(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::NoMatch(const UnicodeString & Mask, const UnicodeString & Error)
{
  UnicodeString Message = FMTLOAD(SCRIPT_MATCH_NO_MATCH, (Mask));
  if (!Error.IsEmpty())
  {
    Message += FORMAT(L" (%s)", (Error));
  }

  NoMatch(Message);
}
//---------------------------------------------------------------------------
void __fastcall TScript::FreeFiles(TStrings * FileList)
{
  for (int i = 0; i < FileList->Count; i++)
  {
    if (FileList->Objects[i] != NULL)
    {
      TRemoteFile * File = dynamic_cast<TRemoteFile *>(FileList->Objects[i]);
      delete File;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::FreeFileList(TStrings * FileList)
{
  FreeFiles(FileList);
  delete FileList;
}
//---------------------------------------------------------------------------
void __fastcall TScript::ConnectTerminal(TTerminal * ATerminal)
{
  ATerminal->Open();
}
//---------------------------------------------------------------------------
void __fastcall TScript::Print(const UnicodeString Str, bool Error)
{
  if (FOnPrint != NULL)
  {
    FOnPrint(this, Str, Error);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::PrintLine(const UnicodeString Str, bool Error, TTerminal * ATerminal)
{
  Log(llOutput, Str, ATerminal);
  Print(Str + L"\n", Error);
}
//---------------------------------------------------------------------------
bool __fastcall TScript::HandleExtendedException(Exception * E, TTerminal * ATerminal)
{
  bool Result = (OnShowExtendedException != NULL);

  if (Result)
  {
    if (ATerminal == NULL)
    {
      ATerminal = FTerminal;
    }

    OnShowExtendedException(ATerminal, E, NULL);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScript::CheckSession()
{
  if (FTerminal == NULL)
  {
    throw Exception(LoadStr(SCRIPT_NO_SESSION));
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::CheckMultiFilesToOne(TStrings * FileList, const UnicodeString & Target, bool Unix)
{
  UnicodeString Name;
  if (Unix)
  {
    Name = UnixExtractFileName(Target);
  }
  else
  {
    Name = ExtractFileName(Target);
  }

  if (!IsFileNameMask(Name) && (FileList->Count > 1))
  {
    UnicodeString Message =
      RemoveEmptyLines(UnformatMessage(FormatMultiFilesToOneConfirmation(Target, Unix)));
    PrintLine(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::CheckParams(TScriptProcParams * Parameters)
{
  TScriptCommands::CheckParams(Parameters, false);
}
//---------------------------------------------------------------------------
void __fastcall TScript::TransferParamParams(int & Params, TScriptProcParams * Parameters)
{
  Params |= FLAGMASK(!FConfirm, cpNoConfirmation);

  if (Parameters->FindSwitch(DELETE_SWITCH))
  {
    Params |= cpDelete;
  }

  if (Parameters->FindSwitch(L"resume"))
  {
    Params |= cpResume;
  }
  else if (Parameters->FindSwitch(L"append"))
  {
    Params |= cpAppend;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::CopyParamParams(TCopyParamType & CopyParam, TScriptProcParams * Parameters)
{
  UnicodeString Value;

  if (!FWantsProgress)
  {
    // total size is not visualized, hence it makes no sense to calculate it
    CopyParam.CalculateSize = false;
  }

  if (Parameters->FindSwitch(NOPRESERVETIME_SWITCH))
  {
    CopyParam.PreserveTime = false;
    CopyParam.PreserveTimeDirs = false;
  }

  if (Parameters->FindSwitch(PRESERVETIME_SWITCH, Value))
  {
    CopyParam.PreserveTime = true;

    if (SameText(Value, PRESERVETIMEDIRS_SWITCH_VALUE))
    {
      CopyParam.PreserveTimeDirs = true;
    }
  }

  if (Parameters->FindSwitch(NOPERMISSIONS_SWITCH))
  {
    CopyParam.PreserveRights = false;
  }

  if (Parameters->FindSwitch(PERMISSIONS_SWITCH, Value))
  {
    CopyParam.PreserveRights = true;
    CopyParam.Rights.Octal = Value;
  }

  if (Parameters->FindSwitch(SPEED_SWITCH, Value))
  {
    int CPSLimit;
    if (Value.IsEmpty())
    {
      CPSLimit = 0;
    }
    else
    {
      CPSLimit = StrToInt(Value) * 1024;
      if (CPSLimit < 0)
      {
        CPSLimit = 0;
      }
    }
    CopyParam.CPSLimit = CPSLimit;
  }

  if (Parameters->FindSwitch(TRANSFER_SWITCH, Value))
  {
    CopyParam.TransferMode = ParseTransferModeName(Value);
  }

  if (Parameters->FindSwitch(FILEMASK_SWITCH, Value))
  {
    CopyParam.IncludeFileMask = Value;
    if (FIncludeFileMaskOptionUsed)
    {
      PrintLine(LoadStr(SCRIPT_FILEMASK_INCLUDE_EXCLUDE));
    }
  }

  if (Parameters->FindSwitch(RESUMESUPPORT_SWITCH, Value))
  {
    int ToggleValue = TScriptCommands::FindCommand(ToggleNames,
      LENOF(ToggleNames), Value);
    if (ToggleValue >= 0)
    {
      switch (ToggleValue)
      {
        case ToggleOff:
          CopyParam.ResumeSupport = rsOff;
          break;

        case ToggleOn:
          CopyParam.ResumeSupport = rsOn;
          break;

        default:
          DebugFail();
          break;
      }
    }
    else
    {
      int ThresholdValue;
      if (!TryStrToInt(Value, ThresholdValue))
      {
        throw Exception(FMTLOAD(SCRIPT_VALUE_UNKNOWN, (RESUMESUPPORT_SWITCH, Value)));
      }
      CopyParam.ResumeSupport = rsSmart;
      CopyParam.ResumeThreshold = ThresholdValue * 1024;
    }
  }

  if (Parameters->FindSwitch(NONEWERONLY_SWICH))
  {
    CopyParam.NewerOnly = false;
  }

  if (Parameters->FindSwitch(NEWERONLY_SWICH))
  {
    CopyParam.NewerOnly = true;
  }

  std::unique_ptr<TStrings> RawSettings(new TStringList());
  if (Parameters->FindSwitch(RAWTRANSFERSETTINGS_SWITCH, RawSettings.get()))
  {
    std::unique_ptr<TOptionsStorage> OptionsStorage(new TOptionsStorage(RawSettings.get(), false));
    CopyParam.Load(OptionsStorage.get());
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::ResetTransfer()
{
}
//---------------------------------------------------------------------------
bool __fastcall TScript::EnsureCommandSessionFallback(
  TFSCapability Capability, TSessionAction * Action)
{
  bool Result = FTerminal->IsCapable[Capability] ||
    FTerminal->CommandSessionOpened;

  if (!Result)
  {
    try
    {
      ConnectTerminal(FTerminal->CommandSession);
      Result = true;
    }
    catch(Exception & E)
    {
      if (Action != NULL)
      {
        Action->Rollback(&E);
      }
      HandleExtendedException(&E, FTerminal->CommandSession);
      Result = false;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScript::HelpProc(TScriptProcParams * Parameters)
{
  UnicodeString Output;

  if (Parameters->ParamCount == 0)
  {
    UnicodeString Command;
    UnicodeString Description;
    int Index = 0;
    while (FCommands->Enumerate(Index, &Command, &Description, NULL))
    {
      if (!Description.IsEmpty())
      {
        Output += FORMAT(L"%-8s %s\n", (Command, Description));
      }
      Index++;
    }
  }
  else
  {
    for (int i = 1; i <= Parameters->ParamCount; i++)
    {
      UnicodeString Help;
      if (FCommands->Info(Parameters->Param[i], NULL, &Help))
      {
        Output += Help;
      }
      else
      {
        throw Exception(FMTLOAD(SCRIPT_COMMAND_UNKNOWN, (Parameters->Param[i])));
      }
    }
  }

  Print(Output);
}
//---------------------------------------------------------------------------
void __fastcall TScript::CallProc(TScriptProcParams * Parameters)
{
  CheckSession();
  if (!FTerminal->IsCapable[fcAnyCommand] &&
      !FTerminal->IsCapable[fcSecondaryShell])
  {
    NotSupported();
  }

  // this is used only to log failures to open separate shell session,
  // the actual call logging is done in TTerminal::AnyCommand
  TCallSessionAction Action(
    FTerminal->ActionLog, Parameters->ParamsStr, FTerminal->CurrentDirectory);
  if (EnsureCommandSessionFallback(fcAnyCommand, &Action))
  {
    Action.Cancel();
    FTerminal->AnyCommand(Parameters->ParamsStr, TerminalCaptureLog);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::EchoProc(TScriptProcParams * Parameters)
{
  PrintLine(Parameters->ParamsStr);
}
//---------------------------------------------------------------------------
void __fastcall TScript::StatProc(TScriptProcParams * Parameters)
{
  CheckSession();

  UnicodeString Path = UnixExcludeTrailingBackslash(Parameters->Param[1]);
  FTerminal->ExceptionOnFail = true;
  TRemoteFile * File = NULL;
  try
  {
    File = FTerminal->ReadFileListing(Path);
    PrintLine(File->ListingStr);
  }
  __finally
  {
    FTerminal->ExceptionOnFail = false;
    delete File;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::DoCalculatedChecksum(
  const UnicodeString & FileName, const UnicodeString & DebugUsedArg(Alg), const UnicodeString & Hash)
{
  PrintLine(FORMAT(L"%s %s", (Hash, FileName)));
}
//---------------------------------------------------------------------------
void __fastcall TScript::ChecksumProc(TScriptProcParams * Parameters)
{
  CheckSession();
  if (!FTerminal->IsCapable[fcCalculatingChecksum] &&
      (!FTerminal->IsCapable[fcSecondaryShell] || FTerminal->IsEncryptingFiles()))
  {
    NotSupported();
  }

  // this is used only to log failures to open separate shell session,
  // the actual call logging is done in TTerminal::CalculateFilesChecksum
  TChecksumSessionAction Action(FTerminal->ActionLog);
  if (EnsureCommandSessionFallback(fcCalculatingChecksum, &Action))
  {
    Action.Cancel();

    UnicodeString Alg = Parameters->Param[1];
    TStrings * FileList = CreateFileList(Parameters, 2, 2, fltQueryServer);
    FTerminal->ExceptionOnFail = true;
    try
    {
      if ((FileList->Count != 1) ||
          DebugNotNull(dynamic_cast<TRemoteFile *>(FileList->Objects[0]))->IsDirectory)
      {
        throw Exception(FMTLOAD(NOT_FILE_ERROR, (FileList->Strings[0])));
      }

      FTerminal->CalculateFilesChecksum(Alg, FileList, DoCalculatedChecksum);
    }
    __finally
    {
      FTerminal->ExceptionOnFail = false;
      FreeFileList(FileList);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::CopyIdProc(TScriptProcParams * Parameters)
{
  CheckSession();

  UnicodeString FileName = Parameters->Param[1];
  TAutoFlag AutoFlag(FPrintInformation);
  FTerminal->UploadPublicKey(FileName);
}
//---------------------------------------------------------------------------
void __fastcall TScript::TerminalCaptureLog(const UnicodeString & AddedLine,
  TCaptureOutputType OutputType)
{
  if ((OutputType == cotOutput) || (OutputType == cotError))
  {
    PrintLine(AddedLine);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::PwdProc(TScriptProcParams * /*Parameters*/)
{
  CheckSession();

  PrintLine(FTerminal->CurrentDirectory);
  TCwdSessionAction Action(FTerminal->ActionLog, FTerminal->CurrentDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TScript::CdProc(TScriptProcParams * Parameters)
{
  CheckSession();

  if (Parameters->ParamCount == 0)
  {
    FTerminal->HomeDirectory();
  }
  else
  {
    FTerminal->ChangeDirectory(Parameters->Param[1]);
  }

  PrintLine(FTerminal->CurrentDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TScript::LsProc(TScriptProcParams * Parameters)
{
  CheckSession();

  UnicodeString Directory;
  TFileMasks Mask;
  bool HaveMask = false;
  if (Parameters->ParamCount > 0)
  {
    Directory = Parameters->Param[1];
    UnicodeString MaskStr = UnixExtractFileName(Directory);
    HaveMask = TFileMasks::IsMask(MaskStr);
    if (HaveMask)
    {
      Mask.SetMask(MaskStr);
      Directory = UnixExtractFilePath(Directory);
    }
  }

  if (Directory.IsEmpty())
  {
    Directory = FTerminal->CurrentDirectory;
  }

  TRemoteFileList * FileList = FTerminal->ReadDirectoryListing(Directory, Mask);
  // on error user may select "skip", then we get NULL
  if (FileList != NULL)
  {
    try
    {
      if (FileList->Count > 0)
      {
        for (int i = 0; i < FileList->Count; i++)
        {
          PrintLine(FileList->Files[i]->ListingStr);
        }
      }
      else
      {
        if (HaveMask)
        {
          NoMatch(Mask.Masks, UnicodeString());
        }
      }
    }
    __finally
    {
      delete FileList;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::RmProc(TScriptProcParams * Parameters)
{
  CheckSession();

  bool OnlyFile = Parameters->FindSwitch(L"onlyfile");
  TStrings * FileList = CreateFileList(
    Parameters, 1, Parameters->ParamCount,
    (TFileListType)(fltQueryServer | fltMask| FLAGMASK(OnlyFile, fltOnlyFile)));
  try
  {
    CheckParams(Parameters);
    FTerminal->DeleteFiles(FileList);
  }
  __finally
  {
    FreeFileList(FileList);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::RmDirProc(TScriptProcParams * Parameters)
{
  CheckSession();

  TStrings * FileList = CreateFileList(Parameters, 1, Parameters->ParamCount, fltDirectories);
  try
  {
    FTerminal->DeleteFiles(FileList);
  }
  __finally
  {
    FreeFileList(FileList);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::DoMvOrCp(TScriptProcParams * Parameters, TFSCapability Capability, bool Cp)
{
  CheckSession();

  if (!FTerminal->IsCapable[Capability])
  {
    NotSupported();
  }

  TStrings * FileList =
    CreateFileList(Parameters, 1, Parameters->ParamCount - 1, TFileListType(fltMask | fltQueryServer));
  try
  {
    DebugAssert(Parameters->ParamCount >= 1);
    UnicodeString Target = Parameters->Param[Parameters->ParamCount];
    UnicodeString TargetDirectory = UnixExtractFilePath(Target);
    UnicodeString FileMask = UnixExtractFileName(Target);

    Target = UnixIncludeTrailingBackslash(TargetDirectory) + FileMask;
    CheckMultiFilesToOne(FileList, Target, true);
    bool DontOverwrite = true; // might use FConfirm eventually, but that would be breaking change
    if (Cp)
    {
      FTerminal->CopyFiles(FileList, TargetDirectory, FileMask, DontOverwrite);
    }
    else
    {
      FTerminal->MoveFiles(FileList, TargetDirectory, FileMask, DontOverwrite);
    }
  }
  __finally
  {
    FreeFileList(FileList);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::MvProc(TScriptProcParams * Parameters)
{
  DoMvOrCp(Parameters, fcRemoteMove, false);
}
//---------------------------------------------------------------------------
void __fastcall TScript::CpProc(TScriptProcParams * Parameters)
{
  DoMvOrCp(Parameters, fcRemoteCopy, true);
}
//---------------------------------------------------------------------------
void __fastcall TScript::ChModProc(TScriptProcParams * Parameters)
{
  CheckSession();
  if (!FTerminal->IsCapable[fcModeChanging])
  {
    NotSupported();
  }

  TStrings * FileList = CreateFileList(Parameters, 2, Parameters->ParamCount,
    fltMask);
  try
  {
    TRemoteProperties Properties;
    Properties.Valid = TValidProperties() << vpRights;
    Properties.Rights.Octal = Parameters->Param[1];

    FTerminal->ChangeFilesProperties(FileList, &Properties);
  }
  __finally
  {
    FreeFileList(FileList);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::LnProc(TScriptProcParams * Parameters)
{
  CheckSession();
  if (!FTerminal->IsCapable[fcSymbolicLink])
  {
    NotSupported();
  }

  DebugAssert(Parameters->ParamCount == 2);

  FTerminal->CreateLink(Parameters->Param[2], Parameters->Param[1], true);
}
//---------------------------------------------------------------------------
void __fastcall TScript::MkDirProc(TScriptProcParams * Parameters)
{
  CheckSession();

  TRemoteProperties Properties;
  Properties.Valid = TValidProperties() << vpEncrypt;
  Properties.Encrypt = FCopyParam.EncryptNewFiles;
  FTerminal->CreateDirectory(Parameters->Param[1], &Properties);
}
//---------------------------------------------------------------------------
void __fastcall TScript::GetProc(TScriptProcParams * Parameters)
{
  CheckSession();
  ResetTransfer();

  bool Latest = Parameters->FindSwitch(L"latest");
  bool OnlyFile = Parameters->FindSwitch(L"onlyfile");
  CheckDefaultCopyParam();
  TCopyParamType CopyParam = FCopyParam;
  CopyParamParams(CopyParam, Parameters);
  int Params = 0;
  TransferParamParams(Params, Parameters);

  RequireParams(Parameters, 1);
  int LastFileParam = (Parameters->ParamCount == 1 ? 1 : Parameters->ParamCount - 1);
  DebugAssert(CopyParam.OnTransferOut == NULL);
  if ((OnTransferOut != NULL) && (Parameters->ParamCount > 1) && SameText(Parameters->Param[Parameters->ParamCount], InOutParam))
  {
    CopyParam.OnTransferOut = OnTransferOut;
    OnlyFile = true;
  }
  TStrings * FileList = CreateFileList(Parameters, 1, LastFileParam,
    (TFileListType)(fltQueryServer | fltMask | FLAGMASK(Latest, fltLatest) | FLAGMASK(OnlyFile, fltOnlyFile)));
  try
  {
    UnicodeString TargetDirectory;
    if (CopyParam.OnTransferOut == NULL)
    {
      if (Parameters->ParamCount == 1)
      {
        TargetDirectory = GetCurrentDir();
        CopyParam.FileMask = L"";
      }
      else
      {
        UnicodeString Target = Parameters->Param[Parameters->ParamCount];
        TargetDirectory = ExtractFilePath(Target);
        if (TargetDirectory.IsEmpty())
        {
          TargetDirectory = GetCurrentDir();
        }
        CopyParam.FileMask = ExtractFileName(Target);
        Target = IncludeTrailingBackslash(TargetDirectory) + CopyParam.FileMask;
        CheckMultiFilesToOne(FileList, Target, false);
      }
    }

    CheckParams(Parameters);
    CopyParam.IncludeFileMask.SetRoots(TargetDirectory, FileList);

    FTerminal->CopyToLocal(FileList, TargetDirectory, &CopyParam, Params, NULL);
  }
  __finally
  {
    FreeFileList(FileList);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::PutProc(TScriptProcParams * Parameters)
{
  CheckSession();
  ResetTransfer();

  bool Latest = Parameters->FindSwitch(L"latest");
  CheckDefaultCopyParam();
  TCopyParamType CopyParam = FCopyParam;
  CopyParamParams(CopyParam, Parameters);
  int Params = 0;
  TransferParamParams(Params, Parameters);

  RequireParams(Parameters, 1);
  int LastFileParam = (Parameters->ParamCount == 1 ? 1 : Parameters->ParamCount - 1);
  DebugAssert(CopyParam.OnTransferIn == NULL);
  TStrings * FileList;
  // We use stdin only if - is the very first parameter
  if ((OnTransferIn != NULL) && SameText(Parameters->Param[1], InOutParam))
  {
    if (Parameters->ParamCount > 2)
    {
      throw Exception(LoadStr(STREAM_IN_SCRIPT_ERROR));
    }

    CopyParam.OnTransferIn = OnTransferIn;
    FileList = new TStringList();
  }
  else
  {
    FileList = CreateLocalFileList(Parameters, 1, LastFileParam, (TFileListType)(fltMask | FLAGMASK(Latest, fltLatest)));
  }
  try
  {
    UnicodeString TargetDirectory;
    if (Parameters->ParamCount == 1)
    {
      TargetDirectory = FTerminal->CurrentDirectory;
      CopyParam.FileMask = L"";
    }
    else
    {
      UnicodeString Target = Parameters->Param[Parameters->ParamCount];
      TargetDirectory = UnixExtractFilePath(Target);
      if (TargetDirectory.IsEmpty())
      {
        TargetDirectory = FTerminal->CurrentDirectory;
      }
      CopyParam.FileMask = UnixExtractFileName(Target);
      Target = UnixIncludeTrailingBackslash(TargetDirectory) + CopyParam.FileMask;
      CheckMultiFilesToOne(FileList, Target, true);
    }

    if (CopyParam.OnTransferIn != NULL)
    {
      if (IsFileNameMask(CopyParam.FileMask))
      {
        throw Exception(LoadStr(STREAM_IN_SCRIPT_ERROR));
      }
      FileList->Add(CopyParam.FileMask);
    }

    CheckParams(Parameters);
    CopyParam.IncludeFileMask.SetRoots(FileList, TargetDirectory);

    FTerminal->CopyToRemote(FileList, TargetDirectory, &CopyParam, Params, NULL);
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
TTransferMode __fastcall TScript::ParseTransferModeName(UnicodeString Name)
{
  DebugAssert((tmBinary == 0) && (tmAscii == 1) && (tmAutomatic == 2));

  int Value = TScriptCommands::FindCommand(TransferModeNames,
    TransferModeNamesCount, Name);
  if (Value < 0)
  {
    throw Exception(FMTLOAD(SCRIPT_VALUE_UNKNOWN, (L"transfer", Name)));
  }

  return (TTransferMode)Value;
}
//---------------------------------------------------------------------------
void __fastcall TScript::OptionImpl(UnicodeString OptionName, UnicodeString ValueName)
{
  enum { Echo, Batch, Confirm, Transfer, SynchDelete, Exclude, Include, ReconnectTime, FailOnNoMatch };
  static const wchar_t * Names[] = { L"echo", L"batch", L"confirm", L"transfer",
    L"synchdelete", L"exclude", L"include", L"reconnecttime", L"failonnomatch" };

  DebugAssert((BatchOff == 0) && (BatchOn == 1) && (BatchAbort == 2) && (BatchContinue == 3));
  static const wchar_t * BatchModeNames[] = { L"off", L"on", L"abort", L"continue" };

  int Option = -1;
  if (!OptionName.IsEmpty())
  {
    Option = TScriptCommands::FindCommand(Names, LENOF(Names), OptionName);
    if (Option < 0)
    {
      throw Exception(FMTLOAD(SCRIPT_OPTION_UNKNOWN, (OptionName)));
    }
    else
    {
      OptionName = Names[Option];
    }
  }

  #define OPT(OPT) ((Option < 0) || (Option == OPT))
  const wchar_t * ListFormat = L"%-15s %-10s";
  bool SetValue = !ValueName.IsEmpty();
  bool PrintReconnectTime = false;

  if (OPT(Echo))
  {
    if (SetValue)
    {
      int Value = TScriptCommands::FindCommand(ToggleNames, LENOF(ToggleNames), ValueName);
      if (Value < 0)
      {
        throw Exception(FMTLOAD(SCRIPT_VALUE_UNKNOWN, (ValueName, OptionName)));
      }
      FEcho = (Value == ToggleOn);
    }

    PrintLine(FORMAT(ListFormat, (Names[Echo], ToggleNames[FEcho ? ToggleOn : ToggleOff])));
  }

  if (OPT(Batch))
  {
    if (SetValue)
    {
      int Value = TScriptCommands::FindCommand(BatchModeNames, LENOF(BatchModeNames), ValueName);
      if (Value < 0)
      {
        throw Exception(FMTLOAD(SCRIPT_VALUE_UNKNOWN, (ValueName, OptionName)));
      }
      FBatch = (TBatchMode)Value;
      FInteractiveBatch = FBatch;

      if (SetValue && (FBatch != BatchOff) && (FSessionReopenTimeout == 0))
      {
        FSessionReopenTimeout = BatchSessionReopenTimeout;
        FInteractiveSessionReopenTimeout = FSessionReopenTimeout;
        PrintReconnectTime = true;
      }
    }

    PrintLine(FORMAT(ListFormat, (Names[Batch], BatchModeNames[FBatch])));
  }

  if (OPT(Confirm))
  {
    if (SetValue)
    {
      int Value = TScriptCommands::FindCommand(ToggleNames, LENOF(ToggleNames), ValueName);
      if (Value < 0)
      {
        throw Exception(FMTLOAD(SCRIPT_VALUE_UNKNOWN, (ValueName, OptionName)));
      }
      FConfirm = (Value == ToggleOn);
      FInteractiveConfirm = FConfirm;
    }

    PrintLine(FORMAT(ListFormat, (Names[Confirm], ToggleNames[FConfirm ? ToggleOn : ToggleOff])));
  }

  // omit the option in listing
  if (Option == Transfer)
  {
    if (SetValue)
    {
      FCopyParam.TransferMode = ParseTransferModeName(ValueName);
    }

    DebugAssert(FCopyParam.TransferMode < (TTransferMode)TransferModeNamesCount);
    const wchar_t * Value = TransferModeNames[FCopyParam.TransferMode];
    PrintLine(FORMAT(ListFormat, (Names[Transfer], Value)));
  }

  // omit the option in listing
  if (Option == SynchDelete)
  {
    if (SetValue)
    {
      int Value = TScriptCommands::FindCommand(ToggleNames, LENOF(ToggleNames), ValueName);
      if (Value < 0)
      {
        throw Exception(FMTLOAD(SCRIPT_VALUE_UNKNOWN, (ValueName, OptionName)));
      }
      FSynchronizeParams =
        (FSynchronizeParams & ~TTerminal::spDelete) |
        FLAGMASK(Value == ToggleOn, TTerminal::spDelete);
    }

    PrintLine(FORMAT(ListFormat, (Names[SynchDelete],
      ToggleNames[FLAGSET(FSynchronizeParams, TTerminal::spDelete) ? ToggleOn : ToggleOff])));
  }

  static const wchar_t * Clear = L"clear";

  // omit the option in listing
  if (Option == Include)
  {
    if (SetValue)
    {
      FCopyParam.IncludeFileMask =
        (ValueName == Clear ? UnicodeString() : ValueName);
      FIncludeFileMaskOptionUsed = (ValueName != Clear);
    }

    PrintLine(FORMAT(ListFormat, (Names[Include], FCopyParam.IncludeFileMask.Masks)));
  }

  // omit the option in listing
  if (Option == Exclude)
  {
    if (SetValue)
    {
      // will throw if ValueName already includes IncludeExcludeFileMasksDelimiter
      FCopyParam.IncludeFileMask =
        (ValueName == Clear ? UnicodeString() : UnicodeString(IncludeExcludeFileMasksDelimiter) + ValueName);
      FIncludeFileMaskOptionUsed = (ValueName != Clear);
    }

    PrintLine(FORMAT(ListFormat, (Names[Include], FCopyParam.IncludeFileMask.Masks)));
  }

  if (OPT(ReconnectTime) || PrintReconnectTime)
  {
    if (SetValue && !PrintReconnectTime)
    {
      int Value;
      if (SameText(ValueName, ToggleNames[ToggleOff]))
      {
        Value = 0;
      }
      else
      {
        if (!TryStrToInt(ValueName, Value))
        {
          throw Exception(FMTLOAD(SCRIPT_VALUE_UNKNOWN, (ValueName, OptionName)));
        }
        else
        {
          Value *= MSecsPerSec;
        }
      }
      FSessionReopenTimeout = Value;
      FInteractiveSessionReopenTimeout = FSessionReopenTimeout;
    }

    if (FSessionReopenTimeout == 0)
    {
      ValueName = ToggleNames[ToggleOff];
    }
    else
    {
      ValueName = IntToStr(FSessionReopenTimeout / MSecsPerSec);
    }
    PrintLine(FORMAT(ListFormat, (Names[ReconnectTime], ValueName)));
  }

  if (OPT(FailOnNoMatch))
  {
    if (SetValue)
    {
      int Value = TScriptCommands::FindCommand(ToggleNames, LENOF(ToggleNames), ValueName);
      if (Value < 0)
      {
        throw Exception(FMTLOAD(SCRIPT_VALUE_UNKNOWN, (ValueName, OptionName)));
      }
      FFailOnNoMatch = (Value == ToggleOn);
    }

    PrintLine(FORMAT(ListFormat, (Names[FailOnNoMatch], ToggleNames[FFailOnNoMatch ? ToggleOn : ToggleOff])));
  }

  #undef OPT
}
//---------------------------------------------------------------------------
void __fastcall TScript::OptionProc(TScriptProcParams * Parameters)
{
  UnicodeString OptionName;
  UnicodeString ValueName;

  if (Parameters->ParamCount >= 1)
  {
    OptionName = Parameters->Param[1];
  }

  if (Parameters->ParamCount >= 2)
  {
    ValueName = Parameters->Param[2];
  }

  OptionImpl(OptionName, ValueName);
}
//---------------------------------------------------------------------------
void __fastcall TScript::AsciiProc(TScriptProcParams * /*Parameters*/)
{
  OptionImpl(L"transfer", L"ascii");
}
//---------------------------------------------------------------------------
void __fastcall TScript::BinaryProc(TScriptProcParams * /*Parameters*/)
{
  OptionImpl(L"transfer", L"binary");
}
//---------------------------------------------------------------------------
void __fastcall TScript::SynchronizeDirectories(TScriptProcParams * Parameters,
  UnicodeString & LocalDirectory, UnicodeString & RemoteDirectory, int FirstParam)
{
  if (Parameters->ParamCount >= FirstParam)
  {
    LocalDirectory = Parameters->Param[FirstParam];
  }
  else
  {
    LocalDirectory = GetCurrentDir();
  }

  if (Parameters->ParamCount >= FirstParam + 1)
  {
    RemoteDirectory = Parameters->Param[FirstParam + 1];
  }
  else
  {
    RemoteDirectory = FTerminal->CurrentDirectory;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TScript::SynchronizeFileRecord(
  const UnicodeString & RootDirectory, const TSynchronizeChecklist::TItem * Item, bool Local)
{
  const TSynchronizeChecklist::TItem::TFileInfo & FileInfo =
    Local ? Item->Local : Item->Remote;
  UnicodeString Path;
  if (Local)
  {
    Path = IncludeTrailingBackslash(FileInfo.Directory) + FileInfo.FileName;
  }
  else
  {
    Path = UnixIncludeTrailingBackslash(FileInfo.Directory) + FileInfo.FileName;
  }

  if (SameText(RootDirectory, Path.SubString(1, RootDirectory.Length())))
  {
    Path[1] = L'.';
    Path.Delete(2, RootDirectory.Length() - 2);
  }

  UnicodeString Result;
  if (Item->IsDirectory)
  {
    if (Local)
    {
      Result = IncludeTrailingBackslash(Path);
    }
    else
    {
      Result = UnixIncludeTrailingBackslash(Path);
    }
  }
  else
  {
    UnicodeString SizeStr = IntToStr(FileInfo.Size);
    UnicodeString ModificationStr =
      ::ModificationStr(FileInfo.Modification, FileInfo.ModificationFmt);
    Result = FORMAT("%s [%s, %s]", (Path, SizeStr, ModificationStr));
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScript::SynchronizePreview(
  UnicodeString LocalDirectory, UnicodeString RemoteDirectory,
  TSynchronizeChecklist * Checklist)
{

  LocalDirectory = IncludeTrailingBackslash(LocalDirectory);
  RemoteDirectory = UnixIncludeTrailingBackslash(RemoteDirectory);

  int Index = 0;
  const TSynchronizeChecklist::TItem * Item;
  while (Checklist->GetNextChecked(Index, Item))
  {
    TDifferenceSessionAction Action(FTerminal->ActionLog, Item);

    UnicodeString Message;
    UnicodeString LocalRecord = SynchronizeFileRecord(LocalDirectory, Item, true);
    UnicodeString RemoteRecord = SynchronizeFileRecord(RemoteDirectory, Item, false);

    switch (Item->Action)
    {
      case TSynchronizeChecklist::saUploadNew:
        Message =
          FMTLOAD(SCRIPT_SYNC_UPLOAD_NEW, (LocalRecord));
        break;

      case TSynchronizeChecklist::saDownloadNew:
        Message =
          FMTLOAD(SCRIPT_SYNC_DOWNLOAD_NEW, (RemoteRecord));
        break;

      case TSynchronizeChecklist::saUploadUpdate:
        Message =
          FMTLOAD(SCRIPT_SYNC_UPLOAD_UPDATE,
            (LocalRecord, RemoteRecord));
        break;

      case TSynchronizeChecklist::saDownloadUpdate:
        Message =
          FMTLOAD(SCRIPT_SYNC_DOWNLOAD_UPDATE,
            (RemoteRecord, LocalRecord));
        break;

      case TSynchronizeChecklist::saDeleteRemote:
        Message =
          FMTLOAD(SCRIPT_SYNC_DELETE_REMOTE, (RemoteRecord));
        break;

      case TSynchronizeChecklist::saDeleteLocal:
        Message =
          FMTLOAD(SCRIPT_SYNC_DELETE_LOCAL, (LocalRecord));
        break;

    default:
      DebugFail();
    }
    PrintLine(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::SynchronizeProc(TScriptProcParams * Parameters)
{
  CheckSession();
  ResetTransfer();

  static const wchar_t * ModeNames[] = { L"remote", L"local", L"both" };

  CheckDefaultCopyParam();
  TCopyParamType CopyParam = FCopyParam;
  CopyParamParams(CopyParam, Parameters);

  RequireParams(Parameters, 1);
  if (Parameters->ParamCount > 3)
  {
    throw Exception(FMTLOAD(SCRIPT_TOO_MANY_PARAMS, (L"synchronize")));
  }
  UnicodeString ModeName = Parameters->Param[1];
  DebugAssert(FSynchronizeMode < 0);
  FSynchronizeMode = TScriptCommands::FindCommand(ModeNames, LENOF(ModeNames), ModeName);

  try
  {
    if (FSynchronizeMode < 0)
    {
      throw Exception(FMTLOAD(SCRIPT_OPTION_UNKNOWN, (ModeName)));
    }

    UnicodeString LocalDirectory;
    UnicodeString RemoteDirectory;

    SynchronizeDirectories(Parameters, LocalDirectory, RemoteDirectory, 2);
    CopyParam.IncludeFileMask.SetRoots(LocalDirectory, RemoteDirectory);

    CheckDefaultSynchronizeParams();
    int SynchronizeParams = FSynchronizeParams | TTerminal::spNoConfirmation;

    UnicodeString Value;
    if (Parameters->FindSwitch(L"delete"))
    {
      SynchronizeParams |= TTerminal::spDelete;
    }
    if (Parameters->FindSwitch(L"mirror") &&
        (FSynchronizeMode != TTerminal::smBoth))
    {
      SynchronizeParams |= TTerminal::spMirror;
    }
    if (Parameters->FindSwitch(L"criteria", Value))
    {
      enum { None, Either, EitherBoth };
      static const wchar_t * CriteriaNames[] = { L"none", L"either", L"both" };
      int Criteria = TScriptCommands::FindCommand(CriteriaNames, LENOF(CriteriaNames), Value);
      if (Criteria >= 0)
      {
        switch (Criteria)
        {
          case None:
            SynchronizeParams |= TTerminal::spNotByTime;
            SynchronizeParams &= ~TTerminal::spBySize;
            break;

          case Either:
          case EitherBoth:
            SynchronizeParams &= ~TTerminal::spNotByTime;
            SynchronizeParams |= TTerminal::spBySize;
            break;
        }
      }
      else
      {
        int Params = TTerminal::spNotByTime;
        while ((Params >= 0) && !Value.IsEmpty())
        {
          UnicodeString Token = CutToChar(Value, L',', true);
          enum { Time, Size, Checksum };
          static const wchar_t * CriteriaNames[] = { L"time", L"size", L"checksum" };
          int Criteria = TScriptCommands::FindCommand(CriteriaNames, LENOF(CriteriaNames), Token);
          switch (Criteria)
          {
            case Time:
              Params &= ~TTerminal::spNotByTime;
              break;

            case Size:
              Params |= TTerminal::spBySize;
              break;

            case Checksum:
              Params |= TTerminal::spByChecksum;
              break;

            default:
              Params = -1;
              break;
          }
        }

        if (Params >= 0)
        {
          SynchronizeParams &= ~(TTerminal::spNotByTime | TTerminal::spBySize | TTerminal::spByChecksum);
          SynchronizeParams |= Params;
        }
      }
    }
    bool Preview = Parameters->FindSwitch(L"preview");

    // enforce rules
    if (FSynchronizeMode == TTerminal::smBoth)
    {
      SynchronizeParams &= ~(TTerminal::spNotByTime | TTerminal::spBySize | TTerminal::spByChecksum);
    }

    CheckParams(Parameters);

    bool Continue = true;
    if (FLAGSET(SynchronizeParams, TTerminal::spByChecksum) &&
        !FTerminal->IsCapable[fcCalculatingChecksum])
    {
      if (!FTerminal->IsCapable[fcSecondaryShell])
      {
        NotSupported();
      }
      else
      {
        Continue = EnsureCommandSessionFallback(fcCalculatingChecksum, NULL);
      }
    }

    if (Continue)
    {
      PrintLine(LoadStr(SCRIPT_SYNCHRONIZE_COLLECTING));

      TSynchronizeChecklist * Checklist =
        FTerminal->SynchronizeCollect(LocalDirectory, RemoteDirectory,
          static_cast<TTerminal::TSynchronizeMode>(FSynchronizeMode),
          &CopyParam, SynchronizeParams, OnTerminalSynchronizeDirectory, NULL);
      try
      {
        int Index = 0;
        const TSynchronizeChecklist::TItem * DummyItem;
        if (Checklist->GetNextChecked(Index, DummyItem))
        {
          if (Preview)
          {
            PrintLine(LoadStr(SCRIPT_SYNCHRONIZE_CHECKLIST));
            SynchronizePreview(LocalDirectory, RemoteDirectory, Checklist);
          }
          else
          {
            PrintLine(LoadStr(SCRIPT_SYNCHRONIZE_SYNCHRONIZING));
            FTerminal->SynchronizeApply(
              Checklist, &CopyParam, SynchronizeParams, OnTerminalSynchronizeDirectory, NULL, NULL, NULL, NULL);
          }
        }
        else
        {
          NoMatch(LoadStr(SCRIPT_SYNCHRONIZE_NODIFFERENCE));
        }
      }
      __finally
      {
        delete Checklist;
      }
    }
  }
  __finally
  {
    FSynchronizeMode = -1;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::Synchronize(const UnicodeString LocalDirectory,
  const UnicodeString RemoteDirectory, const TCopyParamType & CopyParam,
  int SynchronizeParams, TSynchronizeChecklist ** Checklist)
{
  try
  {
    FKeepingUpToDate = true;

    TSynchronizeChecklist * AChecklist =
      FTerminal->SynchronizeCollect(LocalDirectory, RemoteDirectory, TTerminal::smRemote,
        &CopyParam, SynchronizeParams, NULL, NULL);
    try
    {
      if (AChecklist->Count > 0)
      {
        FTerminal->SynchronizeApply(
          AChecklist, &CopyParam, SynchronizeParams, OnTerminalSynchronizeDirectory, NULL, NULL, NULL, NULL);
      }
    }
    __finally
    {
      if (Checklist == NULL)
      {
        delete AChecklist;
      }
      else
      {
        *Checklist = AChecklist;
      }
    }

    // to break line after the last transfer (if any);
    Print(L"");

    FKeepingUpToDate = false;
  }
  catch(Exception & E)
  {
    FKeepingUpToDate = false;

    HandleExtendedException(&E);
    throw;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::KeepUpToDateProc(TScriptProcParams * Parameters)
{
  if (OnSynchronizeStartStop == NULL)
  {
    Abort();
  }

  CheckSession();
  ResetTransfer();

  CheckDefaultCopyParam();
  TCopyParamType CopyParam = FCopyParam;
  CopyParamParams(CopyParam, Parameters);

  UnicodeString LocalDirectory;
  UnicodeString RemoteDirectory;

  SynchronizeDirectories(Parameters, LocalDirectory, RemoteDirectory, 1);

  CheckDefaultSynchronizeParams();
  int SynchronizeParams = FSynchronizeParams | TTerminal::spNoConfirmation |
    TTerminal::spNoRecurse | TTerminal::spUseCache | TTerminal::spDelayProgress |
    TTerminal::spSubDirs;

  if (Parameters->FindSwitch(L"delete"))
  {
    SynchronizeParams |= TTerminal::spDelete;
  }

  CheckParams(Parameters);

  PrintLine(LoadStr(SCRIPT_KEEPING_UP_TO_DATE));

  OnSynchronizeStartStop(this, LocalDirectory, RemoteDirectory, CopyParam, SynchronizeParams);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TManagementScript::TManagementScript(TStoredSessionList * StoredSessions,
  bool LimitedOutput) :
  TScript(LimitedOutput)
{
  DebugAssert(StoredSessions != NULL);
  FOnInput = NULL;
  FOnTerminalPromptUser = NULL;
  FOnShowExtendedException = NULL;
  FOnTerminalQueryUser = NULL;
  FStoredSessions = StoredSessions;
  FTerminalList = new TTerminalList(Configuration);
  FOnQueryCancel = NULL;
  FContinue = true;

  OnTerminalSynchronizeDirectory = TerminalSynchronizeDirectory;

  FCommands->Register(EXIT_COMMAND, SCRIPT_EXIT_DESC, SCRIPT_EXIT_HELP, &ExitProc, 0, 0, false);
  FCommands->Register(L"bye", 0, SCRIPT_EXIT_HELP, &ExitProc, 0, 0, false);
  FCommands->Register(L"open", SCRIPT_OPEN_DESC, SCRIPT_OPEN_HELP11, &OpenProc, 0, -1, true);
  FCommands->Register(L"close", SCRIPT_CLOSE_DESC, SCRIPT_CLOSE_HELP, &CloseProc, 0, 1, false);
  FCommands->Register(L"session", SCRIPT_SESSION_DESC, SCRIPT_SESSION_HELP, &SessionProc, 0, 1, false);
  FCommands->Register(L"lpwd", SCRIPT_LPWD_DESC, SCRIPT_LPWD_HELP, &LPwdProc, 0, 0, false);
  FCommands->Register(L"lcd", SCRIPT_LCD_DESC, SCRIPT_LCD_HELP, &LCdProc, 1, 1, false);
  FCommands->Register(L"lls", SCRIPT_LLS_DESC, SCRIPT_LLS_HELP2, &LLsProc, 0, 1, false);
}
//---------------------------------------------------------------------------
__fastcall TManagementScript::~TManagementScript()
{
  while (FTerminalList->Count > 0)
  {
    FreeTerminal(FTerminalList->Terminals[0]);
  }

  delete FTerminalList;
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::FreeTerminal(TTerminal * ATerminal)
{
  TSessionData * Data = StoredSessions->FindSame(ATerminal->SessionData);
  if (Data != NULL)
  {
    ATerminal->SessionData->RemoteDirectory = ATerminal->CurrentDirectory;

    bool Changed = false;
    if (ATerminal->SessionData->UpdateDirectories)
    {
      Data->RemoteDirectory = ATerminal->SessionData->RemoteDirectory;
      Changed = true;
    }

    if (Changed)
    {
      // only modified, implicit
      StoredSessions->Save(false, false);
    }
  }

  FTerminalList->FreeTerminal(ATerminal);
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::Input(const UnicodeString Prompt,
  UnicodeString & Str, bool AllowEmpty)
{
  do
  {
    Str = L"";
    if (FOnInput != NULL)
    {
      FOnInput(this, Prompt, Str);
    }
    else
    {
      Abort();
    }
  }
  while (Str.Trim().IsEmpty() && !AllowEmpty);
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::PrintProgress(bool First, const UnicodeString Str)
{
  if (FOnPrintProgress != NULL)
  {
    FOnPrintProgress(this, First, Str);
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::ResetTransfer()
{
  TScript::ResetTransfer();
  FLastProgressFile = EmptyStr;
  FLastProgressTime = 0;
  FLastProgressEventTime = 0;
  FLastProgressEventDoneFileName = EmptyStr;
  FLastProgressOverallDone = false;
  FLastProgressMessage = EmptyStr;
}
//---------------------------------------------------------------------------
bool __fastcall TManagementScript::QueryCancel()
{
  bool Result = false;

  if (OnQueryCancel != NULL)
  {
    OnQueryCancel(this, Result);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::TerminalInformation(
  TTerminal * ATerminal, const UnicodeString & Str, int Phase, const UnicodeString & DebugUsedArg(Additional))
{
  DebugAssert(ATerminal != NULL);
  if ((Phase < 0) && ((ATerminal->Status == ssOpening) || FPrintInformation))
  {
    PrintLine(Str, false, ATerminal);
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::TerminalPromptUser(TTerminal * ATerminal,
  TPromptKind Kind, UnicodeString Name, UnicodeString Instructions, TStrings * Prompts,
  TStrings * Results, bool & Result, void * Arg)
{
  // When authentication using stored password fails,
  // do not ask user for another password.
  if ((!ATerminal->StoredCredentialsTried ||
       !IsAuthenticationPrompt(Kind) ||
       (Prompts->Count == 0)) && // allow instructions-only prompts
      (OnTerminalPromptUser != NULL))
  {
    OnTerminalPromptUser(ATerminal, Kind, Name, Instructions, Prompts, Results, Result, Arg);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TManagementScript::Synchronizing()
{
  return (FKeepingUpToDate || (FSynchronizeMode >= 0));
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::ShowPendingProgress()
{
  if (!FSynchronizeIntro.IsEmpty())
  {
    if (Synchronizing())
    {
      PrintLine(FSynchronizeIntro);
    }
    FSynchronizeIntro = L"";
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::TerminalOperationProgress(
  TFileOperationProgressType & ProgressData)
{
  if (ProgressData.IsTransfer())
  {
    if (ProgressData.InProgress  && ProgressData.FileInProgress &&
        !ProgressData.FileName.IsEmpty())
    {
      bool DoPrint = false;
      bool First = false;
      UnicodeString ProgressFileName = ProgressData.FileName;
      if (ProgressData.Side == osLocal)
      {
        ProgressFileName = ExcludeTrailingBackslash(ProgressFileName);
      }
      else
      {
        ProgressFileName = UnixExcludeTrailingBackslash(ProgressFileName);
      }

      if (ProgressFileName != FLastProgressFile)
      {
        First = true;
        DoPrint = true;
        ShowPendingProgress();
      }

      time_t Time = time(NULL);

      if ((OnProgress != NULL) && WantsProgress)
      {
        int OverallProgress = ProgressData.OverallProgress();
        bool OverallDone = (OverallProgress == 100);

        if (DoPrint ||
            (FLastProgressEventTime != Time) ||
            (ProgressData.IsTransferDone() && (FLastProgressEventDoneFileName != ProgressData.FullFileName)) ||
            (OverallDone && !FLastProgressOverallDone))
        {
          FLastProgressEventTime = Time;
          // When transferring a growing file, we would report the progress constantly
          FLastProgressEventDoneFileName = ProgressData.IsTransferDone() ? ProgressData.FullFileName : EmptyStr;
          FLastProgressOverallDone = OverallDone;

          TScriptProgress Progress;
          Progress.Operation = ProgressData.Operation;
          Progress.Side = ProgressData.Side;
          Progress.FileName = ProgressData.FullFileName;
          Progress.Directory = ProgressData.Directory;
          Progress.OverallProgress = OverallProgress;
          Progress.FileProgress = ProgressData.TransferProgress();
          Progress.CPS = ProgressData.CPS();
          Progress.Cancel = false;
          OnProgress(this, Progress);

          if (Progress.Cancel)
          {
            ProgressData.SetCancel(csCancel);
          }
        }
      }

      if (!DoPrint && ((FLastProgressTime != Time) || ProgressData.IsTransferDone()))
      {
        DoPrint = true;
      }

      if (DoPrint)
      {
        UnicodeString FileName;
        if (FLimitedOutput)
        {
          bool Unix = (ProgressData.Side == osRemote);
          FileName = MinimizeName(ProgressFileName, Configuration->ScriptProgressFileNameLimit, Unix);
        }
        else
        {
          FileName = ProgressFileName;
        }
        UnicodeString TransferredSizeStr;
        if (ProgressData.TransferredSize < 1024)
        {
          TransferredSizeStr = FORMAT("%d B", (static_cast<int>(ProgressData.TransferredSize)));
        }
        else
        {
          TransferredSizeStr = FORMAT("%d KB", (static_cast<int>(ProgressData.TransferredSize / 1024)));
        }

        UnicodeString ProgressMessage = FORMAT(L"%-*s | %14s | %6.1f KB/s | %-6.6s | %3d%%",
          (Configuration->ScriptProgressFileNameLimit, FileName,
           TransferredSizeStr,
           static_cast<long double>(ProgressData.CPS()) / 1024,
           ProgressData.AsciiTransfer ? L"ascii" : L"binary",
           ProgressData.TransferProgress()));
        if (FLastProgressMessage != ProgressMessage)
        {
          FLastProgressTime = Time;
          PrintProgress(First, ProgressMessage);
          FLastProgressMessage = ProgressMessage;
          FLastProgressFile = ProgressFileName;
        }
      }
    }
    else
    {
      FLastProgressFile = L"";
    }
  }

  if (QueryCancel())
  {
    ProgressData.SetCancel(csCancel);
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::TerminalOperationFinished(
  TFileOperation Operation, TOperationSide /*Side*/,
  bool /*Temp*/, const UnicodeString & FileName, bool Success, bool NotCancelled,
  TOnceDoneOperation & /*OnceDoneOperation*/)
{
  if (Success && NotCancelled &&
      (Operation != foCalculateSize) && (Operation != foCalculateChecksum) &&
      !TFileOperationProgressType::IsTransferOperation(Operation))
  {
    ShowPendingProgress();
    // For FKeepingUpToDate we should send events to synchronize controller eventually.
    if (Synchronizing() && (Operation == foDelete))
    {
      // Note that this is duplicated with "keep up to date" log.
      PrintLine(FMTLOAD(SCRIPT_SYNCHRONIZE_DELETED, (FileName)));
    }
    else
    {
      PrintLine(FileName);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::TerminalSynchronizeDirectory(
  const UnicodeString & LocalDirectory, const UnicodeString & RemoteDirectory,
  bool & Continue, bool Collect, const TSynchronizeOptions *)
{
  int SynchronizeMode = FSynchronizeMode;
  if (FKeepingUpToDate)
  {
    SynchronizeMode = TTerminal::smRemote;
  }

  UnicodeString Arrow;
  switch (SynchronizeMode)
  {
    case TTerminal::smRemote:
      Arrow = L"=>";
      break;

    case TTerminal::smLocal:
      Arrow = L"<=";
      break;

    case TTerminal::smBoth:
      Arrow = L"<=>";
      break;
  }

  UnicodeString Progress = FMTLOAD(SCRIPT_SYNCHRONIZE, (ExcludeTrailingBackslash(LocalDirectory),
    Arrow, UnixExcludeTrailingBackslash(RemoteDirectory)));

  if (Collect)
  {
    PrintProgress(false, Progress);
  }
  else
  {
    FSynchronizeIntro = Progress;
  }

  if (QueryCancel())
  {
    Continue = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::TerminalInitializeLog(TObject * Sender)
{
  TTerminal * ATerminal = dynamic_cast<TTerminal *>(Sender);
  if (DebugAlwaysTrue(ATerminal != NULL))
  {
    LogPendingLines(ATerminal);
  }
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TManagementScript::FindSession(const UnicodeString Index)
{
  int i = StrToIntDef(Index, -1);

  if ((i <= 0) || (i > FTerminalList->Count))
  {
    throw Exception(FMTLOAD(SCRIPT_SESSION_INDEX_INVALID, (Index)));
  }
  else
  {
    return FTerminalList->Terminals[i - 1];
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::PrintActiveSession()
{
  DebugAssert(FTerminal != NULL);
  PrintLine(FMTLOAD(SCRIPT_ACTIVE_SESSION,
    (FTerminalList->IndexOf(FTerminal) + 1, FTerminal->SessionData->SessionName)));
}
//---------------------------------------------------------------------------
bool __fastcall TManagementScript::HandleExtendedException(Exception * E,
  TTerminal * ATerminal)
{
  bool Result = TScript::HandleExtendedException(E, ATerminal);

  if (ATerminal == NULL)
  {
    ATerminal = FTerminal;
  }

  if ((ATerminal != NULL) && (ATerminal == FTerminal) && (dynamic_cast<EFatal*>(E) != NULL))
  {
    try
    {
      DoClose(ATerminal);
    }
    catch(...)
    {
      // ignore disconnect errors
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::MaskPasswordInCommandLine(UnicodeString & Command, bool Recurse)
{
  UnicodeString Url;
  UnicodeString MaskedParamsPre;
  UnicodeString MaskedParamsPost;

  UnicodeString Param;
  UnicodeString RawParam;
  UnicodeString Separator;
  UnicodeString Separator2;
  UnicodeString OptionWithParameters;
  bool AnyMaskedParam = false;

  TOptions Options;
  UnicodeString ACommand = Command;
  bool SubCommands = false;

  while (CutToken(ACommand, Param, &RawParam, &Separator2))
  {
    Options.Add(Param);

    // "Param 1" is the "winscp.exe" or "open"
    if ((Options.ParamCount == 2) && Url.IsEmpty() && !SubCommands)
    {
      Url = Param;
    }
    else
    {
      UnicodeString & MaskedParams = Url.IsEmpty() ? MaskedParamsPre : MaskedParamsPost;

      UnicodeString Switch, Value;
      wchar_t SwitchMark;
      if (Options.WasSwitchAdded(Switch, Value, SwitchMark))
      {
        OptionWithParameters = L"";
        if (TSessionData::IsSensitiveOption(Switch, Value))
        {
          // We should use something like TProgramParams::FormatSwitch here
          RawParam = FORMAT(L"%s%s=%s", (SwitchMark, Switch, PasswordMask));
          AnyMaskedParam = true;
        }
        else if (TSessionData::IsOptionWithParameters(Switch))
        {
          OptionWithParameters = Switch;
        }

        SubCommands = SameText(Switch, COMMAND_SWITCH);
      }
      else
      {
        if (!OptionWithParameters.IsEmpty())
        {
          if (TSessionData::MaskPasswordInOptionParameter(OptionWithParameters, Param))
          {
            RawParam = Param;
            AnyMaskedParam = true;
          }
        }

        if (Recurse && SubCommands)
        {
          UnicodeString Cmd2 = Param;
          UnicodeString Command2;
          if (CutToken(Cmd2, Command2))
          {
            UnicodeString MaskedParam = MaskPasswordInCommand(Param, Command2);
            if (MaskedParam != Param)
            {
              RawParam = AddQuotes(EscapeParam(MaskedParam));
              AnyMaskedParam = true;
            }
          }
        }
      }
      // Separator is empty on the first loop, but so is the MaskedParams
      AddToList(MaskedParams, RawParam, Separator);
    }

    Separator = Separator2;
  }

  if (!Url.IsEmpty() || AnyMaskedParam)
  {
    UnicodeString MaskedUrl;

    if (!Url.IsEmpty())
    {
      bool DefaultsOnly;
      std::unique_ptr<TSessionData> Data(
        StoredSessions->ParseUrl(Url, &Options, DefaultsOnly, NULL, NULL, &MaskedUrl));
    }

    if ((Url != MaskedUrl) || AnyMaskedParam)
    {
      Command = MaskedParamsPre;
      // AddToList is noop, when respective component is empty
      AddToList(Command, AddQuotes(MaskedUrl), L" ");
      AddToList(Command, MaskedParamsPost, L" ");
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TManagementScript::MaskPasswordInCommand(const UnicodeString & FullCommand,
  const UnicodeString & Command)
{
  UnicodeString Result = FullCommand;
  if (SameText(FCommands->ResolveCommand(Command), L"open") &&
      !Configuration->LogSensitive)
  {
    MaskPasswordInCommandLine(Result, false);
  }

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TManagementScript::GetLogCmd(const UnicodeString & FullCommand,
  const UnicodeString & Command, const UnicodeString & Params)
{
  UnicodeString Result = MaskPasswordInCommand(FullCommand, Command);

  return TScript::GetLogCmd(Result, Command, Params);
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::Connect(const UnicodeString Session,
  TOptions * Options, bool CheckParams)
{
  try
  {
    bool DefaultsOnly;

    TSessionData * Data;
    if (Options->FindSwitch(L"filezilla"))
    {
      UnicodeString Error;
      std::unique_ptr<TStoredSessionList> FilezillaSessionList(
        Configuration->SelectFilezillaSessionsForImport(StoredSessions, Error));
      if (!Error.IsEmpty())
      {
        throw Exception(Error);
      }

      Data = dynamic_cast<TSessionData *>(FilezillaSessionList->FindByName(Session));
      if (Data == NULL)
      {
        throw Exception(FMTLOAD(FILEZILLA_SITE_NOT_EXIST, (Session)));
      }
      else
      {
        Data = Data->Clone();
      }
      DefaultsOnly = false;
    }
    else
    {
      if (FStoredSessions->IsFolder(Session) ||
          FStoredSessions->IsWorkspace(Session))
      {
        throw Exception(LoadStr(CANNOT_OPEN_SESSION_FOLDER));
      }

      Data = FStoredSessions->ParseUrl(Session, Options, DefaultsOnly);
    }

    try
    {
      if (CheckParams)
      {
        if (Options->ParamCount > 1)
        {
          throw Exception(FMTLOAD(SCRIPT_TOO_MANY_PARAMS, (L"open")));
        }

        TScriptCommands::CheckParams(Options, false);
      }

      if (!Session.IsEmpty() && (Data->Source != ::ssNone) && (Batch != TScript::BatchOff) && UsageWarnings)
      {
        std::unique_ptr<TSessionData> DataWithFingerprint(Data->Clone());
        DataWithFingerprint->LookupLastFingerprint();
        DataWithFingerprint->MaskPasswords();

        PrintLine(LoadStr(SCRIPT_SITE_WARNING));
        PrintLine(L"open " + DataWithFingerprint->GenerateOpenCommandArgs(false));
      }

      DebugAssert(Data != NULL);

      if (!Data->CanLogin || DefaultsOnly)
      {
        if (Data->HostName.IsEmpty())
        {
          UnicodeString Value;
          Input(LoadStr(SCRIPT_HOST_PROMPT), Value, false);
          Data->HostName = Value;
        }

        DebugAssert(Data->CanLogin);
      }

      bool WasLogActions = Configuration->LogActions;
      TTerminal * ATerminal = FTerminalList->NewTerminal(Data);
      DebugAssert(FLoggingTerminal == NULL);
      FLoggingTerminal = ATerminal;
      try
      {
        try
        {
          ATerminal->AutoReadDirectory = false;

          ATerminal->OnInformation = TerminalInformation;
          ATerminal->OnPromptUser = TerminalPromptUser;
          ATerminal->OnShowExtendedException = OnShowExtendedException;
          ATerminal->OnQueryUser = OnTerminalQueryUser;
          ATerminal->OnProgress = TerminalOperationProgress;
          ATerminal->OnFinished = TerminalOperationFinished;
          ATerminal->OnInitializeLog = TerminalInitializeLog;

          ConnectTerminal(ATerminal);
        }
        catch(Exception & E)
        {
          // fatal error, most probably caused by XML logging failure (as it has been turned off),
          // and XML log is required => abort
          if ((dynamic_cast<EFatal *>(&E) != NULL) &&
              WasLogActions && !Configuration->LogActions &&
              Configuration->LogActionsRequired)
          {
            FContinue = false;
          }
          // make sure errors (mainly fatal ones) are associated
          // with this terminal, not the last active one
          bool Handled = HandleExtendedException(&E, ATerminal);
          FTerminalList->FreeTerminal(ATerminal);
          ATerminal = NULL;
          if (!Handled)
          {
            throw;
          }
        }
      }
      __finally
      {
        FLoggingTerminal = NULL;
      }

      if (ATerminal != NULL)
      {
        FTerminal = ATerminal;

        if (!Data->LocalDirectory.IsEmpty())
        {
          try
          {
            DoChangeLocalDirectory(Data->LocalDirectoryExpanded);
          }
          catch(Exception & E)
          {
            if (!HandleExtendedException(&E))
            {
              throw;
            }
          }
        }

        PrintActiveSession();
      }
    }
    __finally
    {
      delete Data;
    }
  }
  catch(Exception & E)
  {
    if (!HandleExtendedException(&E))
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::DoClose(TTerminal * ATerminal)
{
  int Index = FTerminalList->IndexOf(ATerminal);
  DebugAssert(Index >= 0);

  bool WasActiveTerminal = (FTerminal == ATerminal);

  try
  {
    if (ATerminal->Active)
    {
      ATerminal->Close();
    }

    UnicodeString SessionName = ATerminal->SessionData->SessionName;
    FreeTerminal(ATerminal);
    if (WasActiveTerminal)
    {
      FTerminal = NULL;
    }

    PrintLine(FMTLOAD(SCRIPT_SESSION_CLOSED, (SessionName)));
  }
  __finally
  {
    if (WasActiveTerminal)
    {
      if (FTerminalList->Count > 0)
      {
        if (Index < FTerminalList->Count)
        {
          FTerminal = FTerminalList->Terminals[Index];
        }
        else
        {
          FTerminal = FTerminalList->Terminals[0];
        }
        PrintActiveSession();
      }
      else
      {
        PrintLine(LoadStr(SCRIPT_NO_SESSION));
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::DoChangeLocalDirectory(UnicodeString Directory)
{
  if (!SetCurrentDir(ApiPath(Directory)))
  {
    throw EOSExtException(FMTLOAD(CHANGE_DIR_ERROR, (Directory)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::ExitProc(TScriptProcParams * /*Parameters*/)
{
  FContinue = false;
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::OpenProc(TScriptProcParams * Parameters)
{
  Connect(Parameters->ParamCount > 0 ? Parameters->Param[1] : UnicodeString(),
    Parameters, true);
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::CloseProc(TScriptProcParams * Parameters)
{
  CheckSession();

  TTerminal * ATerminal;

  if (Parameters->ParamCount == 0)
  {
    ATerminal = FTerminal;
  }
  else
  {
    ATerminal = FindSession(Parameters->Param[1]);
  }

  DoClose(ATerminal);
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::SessionProc(TScriptProcParams * Parameters)
{
  CheckSession();

  if (Parameters->ParamCount == 0)
  {
    for (int i = 0; i < FTerminalList->Count; i++)
    {
      PrintLine(FORMAT(L"%3d  %s",
        (i + 1, FTerminalList->Terminals[i]->SessionData->SessionName)));
    }

    PrintActiveSession();
  }
  else
  {
    FTerminal = FindSession(Parameters->Param[1]);

    PrintActiveSession();
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::LPwdProc(TScriptProcParams * /*Parameters*/)
{
  PrintLine(GetCurrentDir());
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::LCdProc(TScriptProcParams * Parameters)
{
  DebugAssert(Parameters->ParamCount == 1);

  DoChangeLocalDirectory(Parameters->Param[1]);
  PrintLine(GetCurrentDir());
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::LLsProc(TScriptProcParams * Parameters)
{
  UnicodeString Directory;
  UnicodeString Mask;
  if (Parameters->ParamCount > 0)
  {
    Directory = Parameters->Param[1];
    Mask = ExtractFileName(Directory);
    if (TFileMasks::IsMask(Mask))
    {
      Directory = ExtractFilePath(Directory);
    }
    else
    {
      Mask = L"";
    }
  }

  if (Directory.IsEmpty())
  {
    Directory = GetCurrentDir();
  }

  if (Mask.IsEmpty())
  {
    Mask = L"*.*";
  }

  TSearchRecOwned SearchRec;
  int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
  if (FindFirstUnchecked(IncludeTrailingBackslash(Directory) + Mask, FindAttrs, SearchRec) != 0)
  {
    NoMatch(Mask, ListingSysErrorMessage());
  }
  else
  {
    UnicodeString TimeFormat = FixedLenDateTimeFormat(FormatSettings.ShortTimeFormat);
    UnicodeString DateFormat = FixedLenDateTimeFormat(FormatSettings.ShortDateFormat);
    int DateLen = 0;
    int TimeLen = 0;
    bool First = true;

    do
    {
      if (SearchRec.Name != L".")
      {
        TDateTime DateTime = SearchRec.GetLastWriteTime();
        UnicodeString TimeStr = FormatDateTime(TimeFormat, DateTime);
        UnicodeString DateStr = FormatDateTime(DateFormat, DateTime);
        if (First)
        {
          if (TimeLen < TimeStr.Length())
          {
            TimeLen = TimeStr.Length();
          }
          if (DateLen < DateStr.Length())
          {
            DateLen = DateStr.Length();
          }
          First = false;
        }
        UnicodeString SizeStr;
        if (SearchRec.IsDirectory())
        {
          SizeStr = L"<DIR>";
        }
        else
        {
          SizeStr = FORMAT(L"%14.0n", (static_cast<long double>(SearchRec.Size)));
        }
        PrintLine(FORMAT(L"%-*s  %-*s    %-14s %s", (
          DateLen, DateStr, TimeLen, TimeStr, SizeStr, SearchRec.Name)));
      }
    }
    while (FindNextChecked(SearchRec) == 0);
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::ReflectSettings()
{
  for (int i = 0; i < FTerminalList->Count; i++)
  {
    FTerminalList->Terminals[i]->ReflectSettings();
  }
}
//---------------------------------------------------------------------------
