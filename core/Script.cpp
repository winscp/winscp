//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <SysUtils.hpp>

#include "Common.h"
#include "TextsCore.h"
#include "Script.h"
#include "Terminal.h"
#include "SessionData.h"
#include "ScpMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TScriptProcParams::TScriptProcParams(TStrings * Params,
  const AnsiString & ParamsStr)
{
  FParams = Params;
  FParamsStr = ParamsStr;
  FSkipParams = 0;
  FArg = 0;
}
//---------------------------------------------------------------------------
void __fastcall TScriptProcParams::SkipParam()
{
  assert(FSkipParams < FParams->Count);
  FSkipParams++;
}
//---------------------------------------------------------------------------
int __fastcall TScriptProcParams::GetParamCount()
{
  return (FSkipParams > FParams->Count ? 0 : FParams->Count - FSkipParams);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TScriptProcParams::GetParam(int Index)
{
  return FParams->Strings[FSkipParams + Index];
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TScriptCommands : TStringList
{
public:
  typedef void __fastcall (__closure *TCommandProc)(TScriptProcParams * Parameters);

  __fastcall TScriptCommands();

  void __fastcall Execute(TScriptProcParams * Parameters);
  void __fastcall Execute(TStrings * Tokens, AnsiString Params);

  void __fastcall Register(const char * Command,
    const AnsiString Description, const AnsiString Help, TCommandProc Proc,
    int MinParams, int MaxParams, void * Arg = NULL);
  void __fastcall Register(const char * Command,
    int Description, int Help, TCommandProc Proc,
    int MinParams, int MaxParams, void * Arg = NULL);
  void __fastcall Register(const char * Command, TCommandProc Proc,
    int MinParams, int MaxParams, void * Arg = NULL);

  bool __fastcall Info(const AnsiString Command,
    AnsiString * Description, AnsiString * Help);
  bool __fastcall Enumerate(int Index,
    AnsiString * Command, AnsiString * Description, AnsiString * Help);
  static int __fastcall FindCommand(TStrings * Commands, const AnsiString Command,
    AnsiString * Matches = NULL);
  static int __fastcall FindCommand(const char ** Commands, size_t Count,
    const AnsiString Command, AnsiString * Matches = NULL);

  __property TCommandProc DefaultProc = { read = FDefaultProc, write = FDefaultProc };

protected:
  TCommandProc FDefaultProc;

  struct TScriptCommand
  {
    AnsiString Description;
    AnsiString Help;
    TCommandProc Proc;
    int MinParams;
    int MaxParams;
    void * Arg;
  };
};
//---------------------------------------------------------------------------
__fastcall TScriptCommands::TScriptCommands()
{
  Sorted = true;
  CaseSensitive = false;
  FDefaultProc = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TScriptCommands::Register(const char * Command,
  const AnsiString Description, const AnsiString Help, TCommandProc Proc,
  int MinParams, int MaxParams, void * Arg)
{
  TScriptCommand * ScriptCommand = new TScriptCommand;
  ScriptCommand->Description = Description;
  ScriptCommand->Help = Help;
  ScriptCommand->Proc = Proc;
  ScriptCommand->MinParams = MinParams;
  ScriptCommand->MaxParams = MaxParams;
  ScriptCommand->Arg = Arg;

  AddObject(Command, reinterpret_cast<TObject *>(ScriptCommand));
}
//---------------------------------------------------------------------------
void __fastcall TScriptCommands::Register(const char * Command,
  int Description, int Help, TCommandProc Proc,
  int MinParams, int MaxParams, void * Arg)
{
  Register(Command,
    (Description > 0 ? LoadStr(Description) : AnsiString()),
    (Help > 0 ? LoadStr(Help, 10240) : AnsiString()),
    Proc, MinParams, MaxParams, Arg);
}
//---------------------------------------------------------------------------
void __fastcall TScriptCommands::Register(const char * Command, TCommandProc Proc,
  int MinParams, int MaxParams, void * Arg)
{
  Register(Command, "", "", Proc, MinParams, MaxParams, Arg);
}
//---------------------------------------------------------------------------
bool __fastcall TScriptCommands::Info(const AnsiString Command,
  AnsiString * Description, AnsiString * Help)
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
  AnsiString * Command, AnsiString * Description, AnsiString * Help)
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
  const AnsiString Command, AnsiString * Matches)
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
            *Matches += ", ";
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
int __fastcall TScriptCommands::FindCommand(const char ** Commands, size_t Count,
  const AnsiString Command, AnsiString * Matches)
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
void __fastcall TScriptCommands::Execute(TScriptProcParams * Parameters)
{
  assert(Parameters->ParamCount > 0);
  AnsiString Command = Parameters->Param[0];

  AnsiString Matches;
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
  Command = Strings[Index];

  Parameters->SkipParam();
  Parameters->Arg = ScriptCommand->Arg;

  if (Parameters->ParamCount < ScriptCommand->MinParams)
  {
    throw Exception(FMTLOAD(SCRIPT_MISSING_PARAMS, (Command)));
  }
  else if ((ScriptCommand->MaxParams >= 0) && (Parameters->ParamCount > ScriptCommand->MaxParams))
  {
    throw Exception(FMTLOAD(SCRIPT_TOO_MANY_PARAMS, (Command)));
  }
  else
  {
    ScriptCommand->Proc(Parameters);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScriptCommands::Execute(TStrings * Tokens, AnsiString Params)
{
  TScriptProcParams * Parameters = new TScriptProcParams(Tokens, Params);
  try
  {
    Execute(Parameters);
  }
  __finally
  {
    delete Parameters;
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TScript::TScript()
{
  FTerminal = NULL;

  Init();
}
//---------------------------------------------------------------------------
__fastcall TScript::~TScript()
{
  delete FCommands;
}
//---------------------------------------------------------------------------
__fastcall TScript::TScript(TTerminal * Terminal)
{
  FTerminal = Terminal;

  Init();
}
//---------------------------------------------------------------------------
void __fastcall TScript::Init()
{
  FBatch = BatchOff;
  FConfirm = true;
  FSynchronizeParams = 0;
  FOnPrint = NULL;
  FOnTerminalSynchronizeDirectory = NULL;
  FOnSynchronizeStartStop = NULL;
  FSynchronizeMode = -1;
  FKeepingUpToDate = false;

  FCommands = new TScriptCommands;
  FCommands->Register(";", 0, 0, &DummyProc, 0, -1);
  FCommands->Register("#", 0, 0, &DummyProc, 0, -1);
  FCommands->Register("help", SCRIPT_HELP_DESC, SCRIPT_HELP_HELP, &HelpProc, 0, -1);
  FCommands->Register("man", 0, SCRIPT_HELP_HELP, &HelpProc, 0, -1);
  FCommands->Register("call", SCRIPT_CALL_DESC, SCRIPT_CALL_HELP, &CallProc, 1, -1);
  FCommands->Register("!", 0, SCRIPT_CALL_HELP, &CallProc, 1, -1);
  FCommands->Register("pwd", SCRIPT_PWD_DESC, SCRIPT_PWD_HELP, &PwdProc, 0, 0);
  FCommands->Register("cd", SCRIPT_CD_DESC, SCRIPT_CD_HELP, &CdProc, 0, 1);
  FCommands->Register("ls", SCRIPT_LS_DESC, SCRIPT_LS_HELP, &LsProc, 0, 1);
  FCommands->Register("dir", 0, SCRIPT_LS_HELP, &LsProc, 0, 1);
  FCommands->Register("rm", SCRIPT_RM_DESC, SCRIPT_RM_HELP, &RmProc, 1, -1);
  FCommands->Register("rmdir", SCRIPT_RMDIR_DESC, SCRIPT_RMDIR_HELP, &RmDirProc, 1, -1);
  FCommands->Register("mv", SCRIPT_MV_DESC, SCRIPT_MV_HELP, &MvProc, 2, -1);
  FCommands->Register("rename", 0, SCRIPT_MV_HELP, &MvProc, 2, -1);
  FCommands->Register("chmod", SCRIPT_CHMOD_DESC, SCRIPT_CHMOD_HELP, &ChModProc, 2, -1);
  FCommands->Register("ln", SCRIPT_LN_DESC, SCRIPT_LN_HELP, &LnProc, 2, 2);
  FCommands->Register("symlink", 0, SCRIPT_LN_HELP, &LnProc, 2, 2);
  FCommands->Register("mkdir", SCRIPT_MKDIR_DESC, SCRIPT_MKDIR_HELP, &MkDirProc, 1, 1);
  FCommands->Register("get", SCRIPT_GET_DESC, SCRIPT_GET_HELP, &GetProc, 1, -1);
  FCommands->Register("recv", 0, SCRIPT_GET_HELP, &GetProc, 1, -1);
  FCommands->Register("put", SCRIPT_PUT_DESC, SCRIPT_PUT_HELP, &PutProc, 1, -1);
  FCommands->Register("send", 0, SCRIPT_PUT_HELP, &PutProc, 1, -1);
  FCommands->Register("option", SCRIPT_OPTION_DESC, SCRIPT_OPTION_HELP2, &OptionProc, -1, 2);
  FCommands->Register("ascii", 0, SCRIPT_OPTION_HELP2, &AsciiProc, 0, 0);
  FCommands->Register("binary", 0, SCRIPT_OPTION_HELP2, &BinaryProc, 0, 0);
  FCommands->Register("synchronize", SCRIPT_SYNCHRONIZE_DESC, SCRIPT_SYNCHRONIZE_HELP, &SynchronizeProc, 1, 3);
  FCommands->Register("keepuptodate", SCRIPT_KEEPUPTODATE_DESC, SCRIPT_KEEPUPTODATE_HELP, &KeepUpToDateProc, 0, 2);
}
//---------------------------------------------------------------------------
void __fastcall TScript::SetCopyParam(const TCopyParamType & value)
{
  FCopyParam.Assign(&value);
}
//---------------------------------------------------------------------------
void __fastcall TScript::SetSynchronizeParams(int value)
{
  FSynchronizeParams = (value &
    (TTerminal::spDelete | TTerminal::spExistingOnly | TTerminal::spTimestamp |
     TTerminal::spNotByTime | TTerminal::spBySize));
}
//---------------------------------------------------------------------------
void __fastcall TScript::Command(const AnsiString Cmd)
{
  try
  {
    TStrings * Tokens = new TStringList();
    try
    {
      AnsiString AllButFirst;
      Tokenize(Cmd, Tokens, AllButFirst);
      if (Tokens->Count > 0)
      {
        FCommands->Execute(Tokens, AllButFirst);
      }
    }
    __finally
    {
      delete Tokens;
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
void __fastcall TScript::Tokenize(const AnsiString Str, TStrings * Tokens,
  AnsiString & AllButFirst)
{
  assert(Tokens->Count == 0);
  // inspired by Putty's sftp_getcmd() from PSFTP.C
  int Index = 1;
  while (Index <= Str.Length())
  {
    while ((Index <= Str.Length()) &&
      ((Str[Index] == ' ') || (Str[Index] == '\t')))
    {
      Index++;
    }

    if (Tokens->Count == 1)
    {
      AllButFirst = Str.SubString(Index, Str.Length() - Index + 1);
    }

    if (Index <= Str.Length())
    {
      bool Quoting = false;
      AnsiString Token;

      while (Index <= Str.Length())
      {
        if (!Quoting && ((Str[Index] == ' ') || (Str[Index] == '\t')))
        {
          break;
        }
        else if ((Str[Index] == '"') && (Index + 1 <= Str.Length()) &&
          (Str[Index + 1] == '"'))
        {
          Index += 2;
          Token += '"';
        }
        else if (Str[Index] == '"')
        {
          Index++;
          Quoting = !Quoting;
        }
        else
        {
          Token += Str[Index];
          Index++;
        }
      }

      if (Index <= Str.Length())
      {
        Index++;
      }

      Tokens->Add(Token);
    }
  }
}
//---------------------------------------------------------------------------
TStrings * __fastcall TScript::CreateFileList(TScriptProcParams * Parameters, int Start,
  int End, TFileListType ListType)
{
  TStrings * Result = new TStringList();
  TStrings * FileLists = NULL;
  try
  {
    try
    {
      for (int i = Start; i <= End; i++)
      {
        AnsiString FileName = Parameters->Param[i];
        if (FLAGSET(ListType, fltDirectories))
        {
          TRemoteFile * File = new TRemoteFile();
          File->FileName = FileName;
          File->Type = FILETYPE_DIRECTORY;
          Result->AddObject(FileName, File);
        }
        else if (FLAGSET(ListType, fltMask) && TFileMasks::IsMask(FileName))
        {
          AnsiString FileDirectory = UnixExtractFilePath(FileName);
          AnsiString Directory = FileDirectory;
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
            FileList = FTerminal->ReadDirectoryListing(Directory, false);
            if (FileLists == NULL)
            {
              FileLists = new TStringList();
            }
            FileLists->AddObject(Directory, FileList);
          }

          AnsiString Mask = UnixExtractFileName(FileName);
          for (int i = 0; i < FileList->Count; i++)
          {
            TRemoteFile * File = FileList->Files[i];
            if (!File->IsThisDirectory && !File->IsParentDirectory &&
                TFileMasks::SingleMaskMatch(Mask, File->FileName))
            {
              Result->AddObject(FileDirectory + File->FileName,
                FLAGSET(ListType, fltQueryServer) ? File->Duplicate() : NULL);
            }
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
              FTerminal->ReadFile(FileName, File);
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
    catch(...)
    {
      FreeFileList(Result);
      throw;
    }
  }
  __finally
  {
    if (FileLists != NULL)
    {
      for (int i = 0; i < FileLists->Count; i++)
      {
        delete FileLists->Objects[i];
      }
      delete FileLists;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
TStrings * __fastcall TScript::CreateLocalFileList(TScriptProcParams * Parameters,
  int Start, int End, TFileListType ListType)
{
  TStrings * Result = new TStringList();
  try
  {
    for (int i = Start; i <= End; i++)
    {
      AnsiString FileName = Parameters->Param[i];
      if (FLAGSET(ListType, fltMask))
      {
        TSearchRec SearchRec;
        int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
        if (FindFirst(FileName, FindAttrs, SearchRec) == 0)
        {
          AnsiString Directory = ExtractFilePath(FileName);
          try
          {
            do
            {
              if ((SearchRec.Name != ".") && (SearchRec.Name != ".."))
              {
                Result->Add(Directory + SearchRec.Name);
              }
            }
            while (FindNext(SearchRec) == 0);
          }
          __finally
          {
            FindClose(SearchRec);
          }
        }
        else
        {
          // not match, let it fail latter however
          Result->Add(FileName);
        }
      }
      else
      {
        Result->Add(FileName);
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
void __fastcall TScript::FreeFileList(TStrings * FileList)
{
  for (int i = 0; i < FileList->Count; i++)
  {
    if (FileList->Objects[i] != NULL)
    {
      TRemoteFile * File = dynamic_cast<TRemoteFile *>(FileList->Objects[i]);
      delete File;
    }
  }
  delete FileList;
}
//---------------------------------------------------------------------------
void __fastcall TScript::ConnectTerminal(TTerminal * Terminal)
{
  Terminal->Open();
}
//---------------------------------------------------------------------------
void __fastcall TScript::Print(const AnsiString Str)
{
  if (FOnPrint != NULL)
  {
    FOnPrint(this, Str);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::PrintLine(const AnsiString Str)
{
  Print(Str + "\n");
}
//---------------------------------------------------------------------------
bool __fastcall TScript::HandleExtendedException(Exception * E, TTerminal * Terminal)
{
  bool Result = (OnShowExtendedException != NULL);

  if (Result)
  {
    if (Terminal == NULL)
    {
      Terminal = FTerminal;
    }

    OnShowExtendedException(Terminal, E, NULL);
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
void __fastcall TScript::ResetTransfer()
{
}
//---------------------------------------------------------------------------
bool __fastcall TScript::EnsureCommandSessionFallback(TFSCapability Capability)
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
      HandleExtendedException(&E, FTerminal->CommandSession);
      Result = false;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScript::SecondaryProc(TScriptProcParams * Parameters)
{
  TScriptCommands * Commands = static_cast<TScriptCommands *>(Parameters->Arg);
  assert(Commands != NULL);

  if (Parameters->ParamCount == 0)
  {
    assert(Commands->DefaultProc != NULL);
    Commands->DefaultProc(Parameters);
  }
  else
  {
    Commands->Execute(Parameters);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::DummyProc(TScriptProcParams * /*Parameters*/)
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TScript::HelpProc(TScriptProcParams * Parameters)
{
  AnsiString Output;

  if (Parameters->ParamCount == 0)
  {
    AnsiString Command;
    AnsiString Description;
    int Index = 0;
    while (FCommands->Enumerate(Index, &Command, &Description, NULL))
    {
      if (!Description.IsEmpty())
      {
        Output += FORMAT("%-8s %s\n", (Command, Description));
      }
      Index++;
    }
  }
  else
  {
    for (int i = 0; i < Parameters->ParamCount; i++)
    {
      AnsiString Help;
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

  if (EnsureCommandSessionFallback(fcAnyCommand))
  {
    FTerminal->AnyCommand(Parameters->ParamsStr, TerminalCaptureLog);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::TerminalCaptureLog(TObject* /*Sender*/,
  TLogLineType Type, const AnsiString AddedLine)
{
  USEDPARAM(Type);
  assert((Type == llOutput) || (Type == llStdError));

  PrintLine(AddedLine);
}
//---------------------------------------------------------------------------
void __fastcall TScript::PwdProc(TScriptProcParams * /*Parameters*/)
{
  CheckSession();

  PrintLine(FTerminal->CurrentDirectory);
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
    FTerminal->ChangeDirectory(Parameters->Param[0]);
  }

  PrintLine(FTerminal->CurrentDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TScript::LsProc(TScriptProcParams * Parameters)
{
  CheckSession();

  AnsiString Directory;
  AnsiString Mask;
  if (Parameters->ParamCount > 0)
  {
    Directory = Parameters->Param[0];
    Mask = UnixExtractFileName(Directory);
    if (TFileMasks::IsMask(Mask))
    {
      Directory = UnixExtractFilePath(Directory);
    }
    else
    {
      Mask = "";
    }
  }

  if (Directory.IsEmpty())
  {
    Directory = FTerminal->CurrentDirectory;
  }

  TRemoteFileList * FileList = FTerminal->ReadDirectoryListing(Directory, false);
  try
  {
    for (int i = 0; i < FileList->Count; i++)
    {
      TRemoteFile * File = FileList->Files[i];
      if (Mask.IsEmpty() || TFileMasks::SingleMaskMatch(Mask, File->FileName))
      {
        PrintLine(FileList->Files[i]->ListingStr);
      }
    }
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::RmProc(TScriptProcParams * Parameters)
{
  CheckSession();

  TStrings * FileList = CreateFileList(Parameters, 0, Parameters->ParamCount - 1, fltMask);
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
void __fastcall TScript::RmDirProc(TScriptProcParams * Parameters)
{
  CheckSession();

  TStrings * FileList = CreateFileList(Parameters, 0, Parameters->ParamCount - 1, fltDirectories);
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
void __fastcall TScript::MvProc(TScriptProcParams * Parameters)
{
  CheckSession();

  TStrings * FileList = CreateFileList(Parameters, 0, Parameters->ParamCount - 2,
    fltMask);
  try
  {
    assert(Parameters->ParamCount >= 1);
    AnsiString Target = Parameters->Param[Parameters->ParamCount - 1];
    AnsiString TargetDirectory = UnixExtractFilePath(Target);
    AnsiString FileMask = UnixExtractFileName(Target);
    FTerminal->MoveFiles(FileList, TargetDirectory, FileMask);
  }
  __finally
  {
    FreeFileList(FileList);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::ChModProc(TScriptProcParams * Parameters)
{
  CheckSession();

  TStrings * FileList = CreateFileList(Parameters, 1, Parameters->ParamCount - 1,
    fltMask);
  try
  {
    TRemoteProperties Properties;
    Properties.Valid = TValidProperties() << vpRights;
    Properties.Rights.Octal = Parameters->Param[0];

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

  assert(Parameters->ParamCount == 2);

  FTerminal->CreateLink(Parameters->Param[1], Parameters->Param[0], true);
}
//---------------------------------------------------------------------------
void __fastcall TScript::MkDirProc(TScriptProcParams * Parameters)
{
  CheckSession();

  FTerminal->CreateDirectory(Parameters->Param[0]);
}
//---------------------------------------------------------------------------
void __fastcall TScript::GetProc(TScriptProcParams * Parameters)
{
  CheckSession();
  ResetTransfer();

  int LastFileParam = (Parameters->ParamCount == 1 ? 0 : Parameters->ParamCount - 2);
  TStrings * FileList = CreateFileList(Parameters, 0, LastFileParam,
    fltQueryServer | fltMask);
  try
  {
    TCopyParamType CopyParam = FCopyParam;
    CopyParam.CalculateSize = false;

    AnsiString TargetDirectory;
    if (Parameters->ParamCount == 1)
    {
      TargetDirectory = GetCurrentDir();
      CopyParam.FileMask = "";
    }
    else
    {
      AnsiString Target = Parameters->Param[Parameters->ParamCount - 1];
      TargetDirectory = ExtractFilePath(Target);
      if (TargetDirectory.IsEmpty())
      {
        TargetDirectory = GetCurrentDir();
      }
      CopyParam.FileMask = ExtractFileName(Target);
    }

    int Params = FLAGMASK(!FConfirm, cpNoConfirmation);
    FTerminal->CopyToLocal(FileList, TargetDirectory, &CopyParam, Params);
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

  int LastFileParam = (Parameters->ParamCount == 1 ? 0 : Parameters->ParamCount - 2);
  TStrings * FileList = CreateLocalFileList(Parameters, 0, LastFileParam, fltMask);
  try
  {
    TCopyParamType CopyParam = FCopyParam;
    CopyParam.CalculateSize = false;

    AnsiString TargetDirectory;
    if (Parameters->ParamCount == 1)
    {
      TargetDirectory = FTerminal->CurrentDirectory;
      CopyParam.FileMask = "";
    }
    else
    {
      AnsiString Target = Parameters->Param[Parameters->ParamCount - 1];
      TargetDirectory = UnixExtractFilePath(Target);
      if (TargetDirectory.IsEmpty())
      {
        TargetDirectory = FTerminal->CurrentDirectory;
      }
      CopyParam.FileMask = UnixExtractFileName(Target);
    }

    int Params = FLAGMASK(!FConfirm, cpNoConfirmation);
    FTerminal->CopyToRemote(FileList, TargetDirectory, &CopyParam, Params);
  }
  __finally
  {
    FreeFileList(FileList);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::OptionImpl(AnsiString OptionName, AnsiString ValueName)
{
  enum { Batch, Confirm, Transfer, SynchDelete, Exclude, Include };
  static const char * Names[] = { "batch", "confirm", "transfer", "synchdelete",
    "exclude", "include" };

  enum { Off, On };
  static const char * ToggleNames[] = { "off", "on" };

  assert((BatchOff == 0) && (BatchOn == 1) && (BatchAbort == 2));
  static const char * BatchModeNames[] = { "off", "on", "abort" };

  assert((tmBinary == 0) && (tmAscii == 1) && (tmAutomatic == 2));
  static const char * TransferModeNames[] = { "binary", "ascii", "automatic" };

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
  const char * ListFormat = "%-12s %-10s";
  bool SetValue = !ValueName.IsEmpty();

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
      FConfirm = (Value == On);
    }

    PrintLine(FORMAT(ListFormat, (Names[Confirm], ToggleNames[FConfirm ? On : Off])));
  }

  if (OPT(Transfer))
  {
    if (SetValue)
    {
      int Value = TScriptCommands::FindCommand(TransferModeNames,
        LENOF(TransferModeNames), ValueName);
      if (Value < 0)
      {
        throw Exception(FMTLOAD(SCRIPT_VALUE_UNKNOWN, (ValueName, OptionName)));
      }
      FCopyParam.TransferMode = (TTransferMode)Value;
    }

    assert(FCopyParam.TransferMode < LENOF(TransferModeNames));
    const char * Value = TransferModeNames[FCopyParam.TransferMode];
    PrintLine(FORMAT(ListFormat, (Names[Transfer], Value)));
  }

  if (OPT(SynchDelete))
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
        FLAGMASK(Value == On, TTerminal::spDelete);
    }

    PrintLine(FORMAT(ListFormat, (Names[SynchDelete],
      ToggleNames[FLAGSET(FSynchronizeParams, TTerminal::spDelete) ? On : Off])));
  }

  static const char * Clear = "clear";

  if (OPT(Include))
  {
    if (SetValue)
    {
      FCopyParam.NegativeExclude = true;
      FCopyParam.ExcludeFileMask =
        (ValueName == Clear ? AnsiString() : ValueName);
    }

    if (SetValue ||
        (FCopyParam.NegativeExclude && !FCopyParam.ExcludeFileMask.Masks.IsEmpty()))
    {
      PrintLine(FORMAT(ListFormat, (Names[Include], FCopyParam.ExcludeFileMask.Masks)));
    }
  }

  if (OPT(Exclude))
  {
    if (SetValue)
    {
      FCopyParam.NegativeExclude = false;
      FCopyParam.ExcludeFileMask =
        (ValueName == Clear ? AnsiString() : ValueName);
    }

    if (SetValue ||
        (!FCopyParam.NegativeExclude && !FCopyParam.ExcludeFileMask.Masks.IsEmpty()))
    {
      PrintLine(FORMAT(ListFormat, (Names[Exclude], FCopyParam.ExcludeFileMask.Masks)));
    }
  }

  #undef OPT
}
//---------------------------------------------------------------------------
void __fastcall TScript::OptionProc(TScriptProcParams * Parameters)
{
  AnsiString OptionName;
  AnsiString ValueName;

  if (Parameters->ParamCount >= 1)
  {
    OptionName = Parameters->Param[0];
  }

  if (Parameters->ParamCount >= 2)
  {
    ValueName = Parameters->Param[1];
  }

  OptionImpl(OptionName, ValueName);
}
//---------------------------------------------------------------------------
void __fastcall TScript::AsciiProc(TScriptProcParams * /*Parameters*/)
{
  OptionImpl("transfer", "ascii");
}
//---------------------------------------------------------------------------
void __fastcall TScript::BinaryProc(TScriptProcParams * /*Parameters*/)
{
  OptionImpl("transfer", "binary");
}
//---------------------------------------------------------------------------
void __fastcall TScript::SynchronizeDirectories(TScriptProcParams * Parameters,
  AnsiString & LocalDirectory, AnsiString & RemoteDirectory, int FirstParam)
{
  if (Parameters->ParamCount > FirstParam)
  {
    LocalDirectory = Parameters->Param[FirstParam];
  }
  else
  {
    LocalDirectory = GetCurrentDir();
  }

  if (Parameters->ParamCount > FirstParam + 1)
  {
    RemoteDirectory = Parameters->Param[FirstParam + 1];
  }
  else
  {
    RemoteDirectory = FTerminal->CurrentDirectory;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::SynchronizeProc(TScriptProcParams * Parameters)
{
  CheckSession();
  ResetTransfer();

  static const char * ModeNames[] = { "remote", "local", "both" };

  AnsiString ModeName = Parameters->Param[0];
  assert(FSynchronizeMode < 0);
  FSynchronizeMode = TScriptCommands::FindCommand(ModeNames, LENOF(ModeNames), ModeName);

  try
  {
    if (FSynchronizeMode < 0)
    {
      throw Exception(FMTLOAD(SCRIPT_OPTION_UNKNOWN, (ModeName)));
    }

    AnsiString LocalDirectory;
    AnsiString RemoteDirectory;

    SynchronizeDirectories(Parameters, LocalDirectory, RemoteDirectory, 1);

    TCopyParamType CopyParam = FCopyParam;
    CopyParam.CalculateSize = false;

    int SynchronizeParams = FSynchronizeParams | TTerminal::spNoConfirmation;

    PrintLine(FMTLOAD(SCRIPT_SYNCHRONIZE_COLLECT,
      (ExcludeTrailingBackslash(LocalDirectory),
       UnixExcludeTrailingBackslash(RemoteDirectory))));

    TSynchronizeChecklist * Checklist =
      FTerminal->SynchronizeCollect(LocalDirectory, RemoteDirectory,
        static_cast<TTerminal::TSynchronizeMode>(FSynchronizeMode),
        &CopyParam, SynchronizeParams, NULL, NULL);
    try
    {
      if (Checklist->Count > 0)
      {
        FTerminal->SynchronizeApply(Checklist, LocalDirectory, RemoteDirectory,
          &CopyParam, SynchronizeParams, OnTerminalSynchronizeDirectory);
      }
    }
    __finally
    {
      delete Checklist;
    }

  }
  __finally
  {
    FSynchronizeMode = -1;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScript::Synchronize(const AnsiString LocalDirectory,
  const AnsiString RemoteDirectory, const TCopyParamType & ACopyParam,
  TSynchronizeChecklist ** Checklist)
{
  try
  {
    FKeepingUpToDate = true;

    TCopyParamType CopyParam = ACopyParam;
    CopyParam.CalculateSize = false;

    int SynchronizeParams = FSynchronizeParams | TTerminal::spNoConfirmation |
      TTerminal::spNoRecurse | TTerminal::spUseCache | TTerminal::spDelayProgress |
      TTerminal::spSubDirs;

    TSynchronizeChecklist * AChecklist =
      FTerminal->SynchronizeCollect(LocalDirectory, RemoteDirectory, TTerminal::smRemote,
        &CopyParam, SynchronizeParams, NULL, NULL);
    try
    {
      if (AChecklist->Count > 0)
      {
        FTerminal->SynchronizeApply(AChecklist, LocalDirectory, RemoteDirectory,
          &CopyParam, SynchronizeParams, OnTerminalSynchronizeDirectory);
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
    Print("");

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

  AnsiString LocalDirectory;
  AnsiString RemoteDirectory;

  SynchronizeDirectories(Parameters, LocalDirectory, RemoteDirectory, 0);

  PrintLine(LoadStr(SCRIPT_KEEPING_UP_TO_DATE));

  OnSynchronizeStartStop(this, LocalDirectory, RemoteDirectory);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TManagementScript::TManagementScript(TStoredSessionList * StoredSessions) :
  TScript()
{
  assert(StoredSessions != NULL);
  FOnInput = NULL;
  FOnTerminalUpdateStatus = NULL;
  FOnTerminalPromptUser = NULL;
  FOnShowExtendedException = NULL;
  FOnTerminalQueryUser = NULL;
  FStoredSessions = StoredSessions;
  FTerminalList = new TTerminalList(Configuration);
  FOnQueryCancel = NULL;
  FContinue = true;

  OnTerminalSynchronizeDirectory = TerminalSynchronizeDirectory;

  FCommands->Register("exit", SCRIPT_EXIT_DESC, SCRIPT_EXIT_HELP, &ExitProc, 0, 0);
  FCommands->Register("bye", 0, SCRIPT_EXIT_HELP, &ExitProc, 0, 0);
  FCommands->Register("open", SCRIPT_OPEN_DESC, SCRIPT_OPEN_HELP, &OpenProc, 0, 1);
  FCommands->Register("close", SCRIPT_CLOSE_DESC, SCRIPT_CLOSE_HELP, &CloseProc, 0, 1);
  FCommands->Register("session", SCRIPT_SESSION_DESC, SCRIPT_SESSION_HELP, &SessionProc, 0, 1);
  FCommands->Register("lpwd", SCRIPT_LPWD_DESC, SCRIPT_LPWD_HELP, &LPwdProc, 0, 0);
  FCommands->Register("lcd", SCRIPT_LCD_DESC, SCRIPT_LCD_HELP, &LCdProc, 1, 1);
  FCommands->Register("lls", SCRIPT_LLS_DESC, SCRIPT_LLS_HELP, &LLsProc, 0, 1);
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
void __fastcall TManagementScript::FreeTerminal(TTerminal * Terminal)
{
  if (!Terminal->SessionData->Name.IsEmpty())
  {
    Terminal->SessionData->RemoteDirectory = Terminal->CurrentDirectory;

    TSessionData * Data;
    Data = (TSessionData *)StoredSessions->FindByName(Terminal->SessionData->Name);
    if (Data != NULL)
    {
      bool Changed = false;
      if (Terminal->SessionData->UpdateDirectories)
      {
        Data->RemoteDirectory = Terminal->SessionData->RemoteDirectory;
        Changed = true;
      }

      if (Changed)
      {
        StoredSessions->Save();
      }
    }
  }

  FTerminalList->FreeTerminal(Terminal);
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::Input(const AnsiString Prompt,
  AnsiString & Str, bool AllowEmpty)
{
  do
  {
    Str = "";
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
void __fastcall TManagementScript::PrintProgress(bool First, const AnsiString Str)
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
  FLastProgressFile = "";
  FLastProgressTime = 0;
  FLastProgressMessage = "";
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
void __fastcall TManagementScript::TerminalOnStdError(TObject * Sender,
  TLogLineType /*Type*/, const AnsiString AddedLine)
{
  TTerminal * Terminal = dynamic_cast<TTerminal*>(Sender);
  assert(Terminal != NULL);
  if (Terminal->Status == sshAuthenticate)
  {
    PrintLine(AddedLine);
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::TerminalPromptUser(TSecureShell * SecureShell,
  AnsiString Prompt, TPromptKind Kind, AnsiString & Response, bool & Result,
  void * Arg)
{
  if (!SecureShell->StoredPasswordTried && (OnTerminalPromptUser != NULL))
  {
    OnTerminalPromptUser(SecureShell, Prompt, Kind, Response, Result, Arg);
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
    FSynchronizeIntro = "";
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::TerminalOperationProgress(
  TFileOperationProgressType & ProgressData, TCancelStatus & Cancel)
{
  if (ProgressData.Operation == foCopy)
  {
    if (ProgressData.InProgress  && !ProgressData.FileName.IsEmpty())
    {
      bool DoPrint = false;
      bool First = false;
      AnsiString ProgressFileName = ProgressData.FileName;
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

      if (!DoPrint && ((FLastProgressTime != time(NULL)) || ProgressData.IsTransferDone()))
      {
        DoPrint = true;
      }

      if (DoPrint)
      {
        static int MaxFileName = 25;
        AnsiString FileName = MinimizeName(ProgressFileName, MaxFileName,
          ProgressData.Side == osRemote);
        AnsiString ProgressMessage = FORMAT("%-*.*s | %10d kB | %6.1f kB/s | %-6.6s | %3d%%",
          (MaxFileName, MaxFileName, FileName,
           static_cast<int>(ProgressData.TransferedSize / 1024),
           static_cast<float>(ProgressData.CPS()) / 1024,
           ProgressData.AsciiTransfer ? "ascii" : "binary",
           ProgressData.TransferProgress()));
        if (FLastProgressMessage != ProgressMessage)
        {
          FLastProgressTime = time(NULL);
          PrintProgress(First, ProgressMessage);
          FLastProgressMessage = ProgressMessage;
          FLastProgressFile = ProgressFileName;
        }
      }
    }
    else
    {
      FLastProgressFile = "";
    }
  }

  if (QueryCancel())
  {
    Cancel = csCancel;
  }
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::TerminalOperationFinished(
  TFileOperation Operation, TOperationSide /*Side*/,
  bool /*Temp*/, const AnsiString FileName, Boolean Success,
  bool & /*DisconnectWhenComplete*/)
{
  if (Success && (Operation != foCalculateSize) && (Operation != foCopy))
  {
    ShowPendingProgress();
    // For FKeepingUpToDate we should send events to synchronize controller eventuelly.
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
  const AnsiString LocalDirectory, const AnsiString RemoteDirectory,
  bool & Continue)
{
  int SynchronizeMode = FSynchronizeMode;
  if (FKeepingUpToDate)
  {
    SynchronizeMode = TTerminal::smRemote;
  }

  AnsiString Arrow;
  switch (SynchronizeMode)
  {
    case TTerminal::smRemote:
      Arrow = "=>";
      break;

    case TTerminal::smLocal:
      Arrow = "<=";
      break;

    case TTerminal::smBoth:
      Arrow = "<=>";
      break;
  }

  FSynchronizeIntro = FMTLOAD(SCRIPT_SYNCHRONIZE, (ExcludeTrailingBackslash(LocalDirectory),
    Arrow, UnixExcludeTrailingBackslash(RemoteDirectory)));

  if (QueryCancel())
  {
    Continue = false;
  }
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TManagementScript::FindSession(const AnsiString Index)
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
  assert(FTerminal != NULL);
  PrintLine(FMTLOAD(SCRIPT_ACTIVE_SESSION,
    (FTerminalList->IndexOf(FTerminal) + 1, FTerminal->SessionData->SessionName)));
}
//---------------------------------------------------------------------------
bool __fastcall TManagementScript::HandleExtendedException(Exception * E,
  TTerminal * Terminal)
{
  bool Result = TScript::HandleExtendedException(E);

  if (Terminal == NULL)
  {
    Terminal = FTerminal;
  }

  if ((Terminal != NULL) && (Terminal == FTerminal) && (dynamic_cast<EFatal*>(E) != NULL))
  {
    try
    {
      DoClose(Terminal);
    }
    catch(...)
    {
      // ignore disconnect errors
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::Connect(const AnsiString Session)
{
  try
  {
    DoConnect(Session);
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
void __fastcall TManagementScript::DoConnect(const AnsiString Session)
{
  bool DefaultsOnly;

  TSessionData * Data = FStoredSessions->ParseUrl(Session, DefaultsOnly,
    puDecodeUrlChars, NULL);
  try
  {
    assert(Data != NULL);

    if (!Data->CanLogin || DefaultsOnly)
    {
      if (Data->HostName.IsEmpty())
      {
        AnsiString Value;
        Input(LoadStr(SCRIPT_HOST_PROMPT), Value, false);
        Data->HostName = Value;
      }

      if (Data->UserName.IsEmpty())
      {
        AnsiString Value;
        Input(LoadStr(SCRIPT_USERNAME_PROMPT), Value, false);
        Data->UserName = Value;
      }

      assert(Data->CanLogin);
    }

    TTerminal * Terminal = FTerminalList->NewTerminal(Data);
    try
    {
      Terminal->AutoReadDirectory = false;

      Terminal->OnStdError = TerminalOnStdError;
      Terminal->OnPromptUser = TerminalPromptUser;
      Terminal->OnShowExtendedException = OnShowExtendedException;
      Terminal->OnQueryUser = OnTerminalQueryUser;
      Terminal->OnProgress = TerminalOperationProgress;
      Terminal->OnFinished = TerminalOperationFinished;
      Terminal->OnUpdateStatus = OnTerminalUpdateStatus;

      ConnectTerminal(Terminal);
    }
    catch(...)
    {
      FTerminalList->FreeTerminal(Terminal);
      throw;
    }

    FTerminal = Terminal;
  }
  __finally
  {
    delete Data;
  }

  PrintActiveSession();
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::DoClose(TTerminal * Terminal)
{
  int Index = FTerminalList->IndexOf(Terminal);
  assert(Index >= 0);

  try
  {
    Terminal->Active = false;

    AnsiString SessionName = Terminal->SessionData->SessionName;
    FreeTerminal(Terminal);

    PrintLine(FMTLOAD(SCRIPT_SESSION_CLOSED, (SessionName)));
  }
  __finally
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
      FTerminal = NULL;
      PrintLine(LoadStr(SCRIPT_NO_SESSION));
    }
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
  Connect(Parameters->ParamCount > 0 ? Parameters->Param[0] : AnsiString());
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::CloseProc(TScriptProcParams * Parameters)
{
  CheckSession();

  TTerminal * Terminal;

  if (Parameters->ParamCount == 0)
  {
    Terminal = FTerminal;
  }
  else
  {
    Terminal = FindSession(Parameters->Param[0]);
  }

  DoClose(Terminal);
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::SessionProc(TScriptProcParams * Parameters)
{
  CheckSession();

  if (Parameters->ParamCount == 0)
  {
    for (int i = 0; i < FTerminalList->Count; i++)
    {
      PrintLine(FORMAT("%3d  %s",
        (i + 1, FTerminalList->Terminals[i]->SessionData->SessionName)));
    }

    PrintActiveSession();
  }
  else
  {
    FTerminal = FindSession(Parameters->Param[0]);

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
  assert(Parameters->ParamCount == 1);

  AnsiString Directory = Parameters->Param[0];
  if (!SetCurrentDir(Directory))
  {
    throw Exception(FMTLOAD(CHANGE_DIR_ERROR, (Directory)));
  }
  PrintLine(GetCurrentDir());
}
//---------------------------------------------------------------------------
void __fastcall TManagementScript::LLsProc(TScriptProcParams * Parameters)
{
  AnsiString Directory;
  AnsiString Mask;
  if (Parameters->ParamCount > 0)
  {
    Directory = Parameters->Param[0];
    Mask = ExtractFileName(Directory);
    if (TFileMasks::IsMask(Mask))
    {
      Directory = ExtractFilePath(Directory);
    }
    else
    {
      Mask = "";
    }
  }

  if (Directory.IsEmpty())
  {
    Directory = GetCurrentDir();
  }

  if (Mask.IsEmpty())
  {
    Mask = "*.*";
  }

  TSearchRec SearchRec;
  int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
  if (FindFirst(IncludeTrailingBackslash(Directory) + Mask, FindAttrs, SearchRec) != 0)
  {
    throw Exception(FMTLOAD(LIST_DIR_ERROR, (Directory)));
  }

  try
  {
    AnsiString TimeFormat = FixedLenDateTimeFormat(ShortTimeFormat);
    AnsiString DateFormat = FixedLenDateTimeFormat(ShortDateFormat);
    int DateLen = 0;
    int TimeLen = 0;
    bool First = true;

    do
    {
      if (SearchRec.Name != ".")
      {
        TDateTime DateTime = FileDateToDateTime(SearchRec.Time);
        AnsiString TimeStr = FormatDateTime(TimeFormat, DateTime);
        AnsiString DateStr = FormatDateTime(DateFormat, DateTime);
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
        PrintLine(FORMAT("%-*s  %-*s    %-14s %s", (
          DateLen, DateStr, TimeLen, TimeStr,
          (FLAGSET(SearchRec.Attr, faDirectory) ?
            AnsiString("<DIR>") : FORMAT("%14.0n", (double(SearchRec.Size)))),
          SearchRec.Name)));
      }
    }
    while (FindNext(SearchRec) == 0);
  }
  __finally
  {
    FindClose(SearchRec);
  }
}
//---------------------------------------------------------------------------

