//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "VCLCommon.h"
#include "GenerateUrl.h"
#include "CoreMain.h"
#include "WinConfiguration.h"
#include <StrUtils.hpp>
#include <Tools.h>
#include <PuttyTools.h>
#include <TextsWin.h>
#include <ProgParams.h>
#include <GUITools.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
const UnicodeString AllFilesMask(L"*");
const UnicodeString NoOpOperationMask(L"*");
//---------------------------------------------------------------------------
static UnicodeString ExcludeTrailingBackslashUnlessRoot(const UnicodeString & Path)
{
  UnicodeString Result = ExcludeTrailingBackslash(Path);
  if (SameText(Result, ExtractFileDrive(Result)))
  {
    Result = IncludeTrailingBackslash(Path);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall DoGenerateUrlDialog(TSessionData * Data, TStrings * Paths)
{
  std::unique_ptr<TGenerateUrlDialog> Dialog(
    new TGenerateUrlDialog(GetFormOwner(), Data, fsList, Paths, false, false, false, 0, UnicodeString(), TCopyParamType()));
  Dialog->Execute();
}
//---------------------------------------------------------------------------
void __fastcall DoGenerateTransferCodeDialog(
  bool ToRemote, bool Move, int CopyParamAttrs, TSessionData * Data, TFilesSelected FilesSelected, TStrings * FileList, const UnicodeString & Path,
  const TCopyParamType & CopyParam)
{
  std::unique_ptr<TGenerateUrlDialog> Dialog(
    new TGenerateUrlDialog(GetFormOwner(), Data, FilesSelected, FileList, true, ToRemote, Move, CopyParamAttrs, Path, CopyParam));
  Dialog->Execute();
}
//---------------------------------------------------------------------------
class TRichEditWithLinks : public TRichEdit
{
public:
  virtual __fastcall TRichEditWithLinks(TComponent * AOwner);

protected:
  virtual void __fastcall CreateWnd();
  void __fastcall Dispatch(void * Message);
};
//---------------------------------------------------------------------------
__fastcall TRichEditWithLinks::TRichEditWithLinks(TComponent * AOwner) :
  TRichEdit(AOwner)
{
}
//---------------------------------------------------------------------------
void __fastcall TRichEditWithLinks::CreateWnd()
{
  TRichEdit::CreateWnd();
  int Mask = SendMessage(Handle, EM_GETEVENTMASK, 0, 0);
  SendMessage(Handle, EM_SETEVENTMASK, 0, Mask | ENM_LINK);
  SendMessage(Handle, EM_SETEDITSTYLEEX, 0, SES_EX_HANDLEFRIENDLYURL);
}
//---------------------------------------------------------------------------
void __fastcall TRichEditWithLinks::Dispatch(void * AMessage)
{
  TMessage & Message = *reinterpret_cast<TMessage *>(AMessage);
  if (Message.Msg == CN_NOTIFY)
  {
    TWMNotify & WMNotify = *reinterpret_cast<TWMNotify *>(AMessage);
    if (WMNotify.NMHdr->code == EN_LINK)
    {
      TENLink & ENLink = *reinterpret_cast<TENLink *>(Message.LParam);
      if (ENLink.msg == WM_LBUTTONDOWN)
      {
        UnicodeString AText = Text;
        // The cpMin and cpMax refer to indexes in a script with a single-byte EOL,
        // while the Text (GetWindowText) uses two-byte EOL
        AText = ReplaceStr(AText, L"\r\n", L"\n");

        if (DebugAlwaysTrue(ENLink.chrg.cpMax < AText.Length()))
        {
          UnicodeString Url = AText.SubString(ENLink.chrg.cpMin + 1, ENLink.chrg.cpMax - ENLink.chrg.cpMin);
          if (IsHttpOrHttpsUrl(Url))
          {
            OpenBrowser(Url);
          }
          else
          {
            ShowHelp(Url);
          }
        }
      }
    }
    TRichEdit::Dispatch(AMessage);
  }
  else
  {
    TRichEdit::Dispatch(AMessage);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TGenerateUrlDialog::TGenerateUrlDialog(
  TComponent * Owner, TSessionData * Data, TFilesSelected FilesSelected, TStrings * Paths,
  bool Transfer, bool ToRemote, bool Move, int CopyParamAttrs, const UnicodeString & Path, const TCopyParamType & CopyParam)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FData = Data;

  if (Paths != NULL)
  {
    FPaths.reset(new TStringList());
    FPaths->AddStrings(Paths);
  }

  FTransfer = Transfer;
  FToRemote = ToRemote;
  FMove = Move;
  FCopyParamAttrs = CopyParamAttrs;
  FCopyParam = CopyParam;
  FFilesSelected = FilesSelected;
  FPathsSample = false;
  FUrlCounted = false;
  FScriptCounted = false;
  FAssemblyCounted = false;

  if (FTransfer)
  {
    DebugAssert(FPaths.get() != NULL);

    const int MaxSample = 3;
    if ((FFilesSelected == fsList) && (FPaths->Count > MaxSample))
    {
      FPathsSample = true;
      while (FPaths->Count > MaxSample)
      {
        FPaths->Delete(FPaths->Count - 1);
      }
    }

    if (FToRemote)
    {
      UnicodeString FirstPath = Paths->Strings[0];
      FSourcePath = FToRemote ? ExcludeTrailingBackslashUnlessRoot(ExtractFilePath(FirstPath)) : UnixExtractFilePath(FirstPath);
      for (int Index = 0; Index < FPaths->Count; Index++)
      {
        FPaths->Strings[Index] = ExtractFileName(FPaths->Strings[Index]);
      }
    }
    else
    {
      FSourcePath = Data->RemoteDirectory;
      // should be noop as we get only file names for remote files
      for (int Index = 0; Index < FPaths->Count; Index++)
      {
        FPaths->Strings[Index] = UnixExtractFileName(FPaths->Strings[Index]);
      }
    }
  }

  FPath = Path;
  FChanging = false;

  FResultMemoWithLinks = new TRichEditWithLinks(this);
  FResultMemoWithLinks->Parent = ResultMemo->Parent;
  FResultMemoWithLinks->SetBounds(ResultMemo->Left, ResultMemo->Top, ResultMemo->Width, ResultMemo->Height);
  FResultMemoWithLinks->Anchors = ResultMemo->Anchors;
  FResultMemoWithLinks->BevelInner = ResultMemo->BevelInner;
  FResultMemoWithLinks->BevelOuter = ResultMemo->BevelOuter;
  FResultMemoWithLinks->BorderStyle = ResultMemo->BorderStyle;
  FResultMemoWithLinks->PopupMenu = ResultMemo->PopupMenu;
  FResultMemoWithLinks->TabOrder = ResultMemo->TabOrder;
  FResultMemoWithLinks->PlainText = false;
  FResultMemoWithLinks->WantReturns = false; // affects Esc too, what we want
  ResultMemo->Visible = false;

  ReadOnlyControl(FResultMemoWithLinks);
}
//---------------------------------------------------------------------------
bool __fastcall TGenerateUrlDialog::IsFileUrl()
{
  return (FPaths.get() != NULL) && !FTransfer;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TGenerateUrlDialog::GenerateUrl(UnicodeString Path)
{
  UnicodeString Url =
    FData->GenerateSessionUrl(
      FLAGMASK(WinSCPSpecificCheck->Checked, sufSpecific) |
      FLAGMASK(UserNameCheck->Enabled && UserNameCheck->Checked, sufUserName) |
      FLAGMASK(PasswordCheck->Enabled && PasswordCheck->Checked, sufPassword) |
      FLAGMASK(HostKeyCheck->Enabled && HostKeyCheck->Checked, sufHostKey) |
      FLAGMASK(RawSettingsCheck->Enabled && RawSettingsCheck->Checked, sufRawSettings) |
      FLAGMASK(CustomWinConfiguration->HttpForWebDAV, sufHttpForWebDAV));

  if ((RemoteDirectoryCheck->Enabled && RemoteDirectoryCheck->Checked) ||
      IsFileUrl())
  {
    if (StartsStr(L"/", Path));
    {
      Path.Delete(1, 1);
    }

    Url += EncodeUrlPath(Path);
  }

  if (SaveExtensionCheck->Enabled && SaveExtensionCheck->Checked)
  {
    Url += UnicodeString(UrlParamSeparator) + UrlSaveParamName;
  }

  return Url;
}
//---------------------------------------------------------------------------
static UnicodeString __fastcall RtfColorEntry(int Color)
{
  return FORMAT(L"\\red%d\\green%d\\blue%d;", ((Color & 0xFF0000) >> 16, (Color & 0x00FF00) >> 8, (Color & 0x0000FF) >> 0));
}
//---------------------------------------------------------------------
static UnicodeString __fastcall RtfScriptComment(const UnicodeString & Text)
{
  return RtfColorItalicText(7, Text);
}
//---------------------------------------------------------------------
static UnicodeString __fastcall RtfScriptPlaceholder(const UnicodeString & Text)
{
  return RtfColorText(7, Text);
}
//---------------------------------------------------------------------
static UnicodeString __fastcall RtfScriptCommand(const UnicodeString & Command)
{
  return RtfLink(ScriptCommandLink(Command), RtfKeyword(Command));
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfCommandlineSwitch(const UnicodeString & Switch, const UnicodeString & Anchor)
{
  return RtfLink(L"commandline#" + Anchor, RtfParameter(TProgramParams::FormatSwitch(Switch.LowerCase())));
}
//---------------------------------------------------------------------------
static UnicodeString __fastcall QuoteStringParam(UnicodeString S)
{
  return AddQuotes(RtfEscapeParam(S, false));
}
//---------------------------------------------------------------------------
// Keep in sync with .NET Session.EscapeFileMask
static UnicodeString __fastcall EscapeFileMask(UnicodeString S)
{
  return
    ReplaceStr(ReplaceStr(ReplaceStr(ReplaceStr(ReplaceStr(
      S, L"[", L"[[]"), L"*", L"[*]"), L"?", L"[?]"), L">", L">>"), L"<", L"<<");
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TGenerateUrlDialog::GenerateUrl()
{
  UnicodeString Result;
  if (!IsFileUrl())
  {
    UnicodeString Path = FData->RemoteDirectory;
    if (!Path.IsEmpty() && !EndsStr(L"/", Path))
    {
      Path += L"/";
    }
    Result = RtfText(GenerateUrl(Path));
  }
  else
  {
    for (int Index = 0; Index < FPaths->Count; Index++)
    {
      UnicodeString Url = GenerateUrl(FPaths->Strings[Index]);
      Result += RtfText(Url) + RtfPara;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::AddSampleDescription(UnicodeString & Description)
{
  if (FPathsSample)
  {
    Description += LoadStr(GENERATE_URL_FILE_SAMPLE) + L"\n";
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TGenerateUrlDialog::GenerateScript(UnicodeString & ScriptDescription)
{
  UnicodeString Result;

  UnicodeString ExeName = Application->ExeName;
  UnicodeString BaseExeName = ExtractFileBaseName(ExeName);
  UnicodeString OpenCommand = FData->GenerateOpenCommandArgs(true);
  UnicodeString CommandPlaceholder1 = FMTLOAD(GENERATE_URL_COMMAND, (1));
  UnicodeString CommandPlaceholder2 = FMTLOAD(GENERATE_URL_COMMAND, (2));
  UnicodeString LogPath = LoadStr(GENERATE_URL_WRITABLE_PATH_TO_LOG) + RtfText(BaseExeName + L".log");
  UnicodeString LogParameter =
    RtfCommandlineSwitch(LOG_SWITCH, L"logging") + RtfText(L"=") +
    RtfScriptPlaceholder(L"\"" + LogPath + L"\"");
  UnicodeString IniParameter =
    RtfCommandlineSwitch(INI_SWITCH, L"configuration") + RtfText(UnicodeString(L"=") + INI_NUL);
  UnicodeString CommandParameter = RtfCommandlineSwitch(COMMAND_SWITCH, L"scripting");

  typedef std::vector<UnicodeString> TCommands;
  TCommands Commands;

  Commands.push_back(RtfScriptCommand(L"open") + L" " + OpenCommand);
  Commands.push_back(UnicodeString());

  if (FTransfer)
  {
    UnicodeString TransferCommand;

    if (FToRemote)
    {
      Commands.push_back(RtfScriptCommand(L"lcd") + L" " + RtfText(QuoteStringParam(FSourcePath)));
      Commands.push_back(RtfScriptCommand(L"cd") + L" " + RtfText(QuoteStringParam(UnixExcludeTrailingBackslash(FPath))));
      TransferCommand = L"put";
    }
    else
    {
      Commands.push_back(RtfScriptCommand(L"cd") + L" " + RtfText(QuoteStringParam(FSourcePath)));
      Commands.push_back(RtfScriptCommand(L"lcd") + L" " + RtfText(QuoteStringParam(ExcludeTrailingBackslashUnlessRoot(FPath))));
      TransferCommand = L"get";
    }

    Commands.push_back(UnicodeString());

    UnicodeString TransferCommandLink = ScriptCommandLink(TransferCommand);
    UnicodeString TransferCommandArgs;
    if (FMove)
    {
      TransferCommandArgs += RtfSwitch(DELETE_SWITCH, TransferCommandLink);
    }
    TransferCommandArgs += FCopyParam.GenerateTransferCommandArgs(FCopyParamAttrs, TransferCommandLink);

    AddSampleDescription(ScriptDescription);

    TransferCommand = RtfScriptCommand(TransferCommand) + TransferCommandArgs;

    if (FFilesSelected == fsList)
    {
      for (int Index = 0; Index < FPaths->Count; Index++)
      {
        UnicodeString Path = ExtractFileName(FPaths->Strings[Index], !FToRemote);
        if (!FToRemote)
        {
          Path = EscapeFileMask(Path);
        }
        Commands.push_back(TransferCommand + L" " + RtfText(QuoteStringParam(Path)));
      }
    }
    else
    {
      Commands.push_back(TransferCommand + L" " + RtfText(AllFilesMask));
    }
  }
  else
  {
    Commands.push_back(L"# " + CommandPlaceholder1);
    Commands.push_back(L"# " + CommandPlaceholder2);
  }

  Commands.push_back(UnicodeString());
  Commands.push_back(RtfScriptCommand(L"exit"));

  UnicodeString ComExeName = ChangeFileExt(ExeName, L".com");

  if (ScriptFormatCombo->ItemIndex == sfScriptFile)
  {
    for (TCommands::const_iterator I = Commands.begin(); I != Commands.end(); I++)
    {
      UnicodeString Command = *I;
      if (!Command.IsEmpty())
      {
        if (Command[1] == L'#')
        {
          Result += RtfScriptComment(Command);
        }
        else
        {
          Result += Command;
        }
      }
      Result += RtfPara;
    }

    UnicodeString ScriptCommandLine =
      FORMAT("\"%s\" /%s=\"%s\" /%s=%s /%s=\"%s\"",
        (ExeName, LowerCase(LOG_SWITCH), LogPath, LowerCase(INI_SWITCH), INI_NUL, LowerCase(SCRIPT_SWITCH), LoadStr(GENERATE_URL_PATH_TO_SCRIPT)));
    Result +=
      RtfPara +
      RtfScriptComment(L"# " + LoadStr(GENERATE_URL_SCRIPT_DESC)) + RtfPara +
      RtfScriptComment(L"# " + ScriptCommandLine) + RtfPara;
  }
  else if (ScriptFormatCombo->ItemIndex == sfBatchFile)
  {
    Result =
      RtfScriptPlaceholder(L"@echo off") + RtfPara +
      RtfPara +
      RtfText(L"\"" + ComExeName + "\" ^") + RtfPara +
      RtfText(L"  ") + LogParameter + L" " + IniParameter + RtfText(L" ^") + RtfPara +
      RtfText(L"  ") + CommandParameter;

    for (TCommands::const_iterator I = Commands.begin(); I != Commands.end(); I++)
    {
      UnicodeString Command = *I;
      if (!Command.IsEmpty())
      {
        Result +=
          RtfText(L" ^") + RtfPara +
          RtfText(L"    \"");

        if (Command[1] == L'#')
        {
          Command.Delete(1, 1);
          Result += RtfScriptPlaceholder(Command.TrimLeft());
        }
        else
        {
          Result += RtfEscapeParam(ReplaceStr(Command, L"%", L"%%"), false);
        }
        Result += L"\"";
      }
    }

    Result +=
      RtfPara +
      RtfPara +
      RtfKeyword(L"set") + RtfText(L" WINSCP_RESULT=%ERRORLEVEL%") + RtfPara +
      RtfKeyword(L"if") + RtfText(L" %WINSCP_RESULT% ") + RtfKeyword(L"equ") + RtfText(L" 0 (") + RtfPara +
      RtfText(L"  ") + RtfKeyword(L"echo") + RtfText(L" Success") + RtfPara +
      RtfText(L") ") + RtfKeyword(L"else") + RtfText(L" (") + RtfPara +
      RtfText(L"  ") + RtfKeyword(L"echo") + RtfText(L" Error") + RtfPara +
      RtfText(L")") + RtfPara +
      RtfPara +
      RtfKeyword(L"exit") + RtfText(L" /b %WINSCP_RESULT%") + RtfPara;
  }
  else if (ScriptFormatCombo->ItemIndex == sfCommandLine)
  {
    Result =
      LogParameter + L" " +
      IniParameter + L" " +
      CommandParameter;

    for (TCommands::const_iterator I = Commands.begin(); I != Commands.end(); I++)
    {
      UnicodeString Command = *I;
      if (!Command.IsEmpty())
      {
        Result += RtfText(L" \"");
        if (Command[1] == L'#')
        {
          Command.Delete(1, 1);
          Result += RtfScriptPlaceholder(Command.TrimLeft());
        }
        else
        {
          Result += RtfEscapeParam(Command, false);
        }
        Result += L"\"";
      }
    }
  }
  else if (ScriptFormatCombo->ItemIndex == sfPowerShell)
  {
    // https://stackoverflow.com/q/74440303/850848
    UnicodeString PsArgPassingLink(L"https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_preference_variables#psnativecommandargumentpassing");

    Result =
      RtfLink(PsArgPassingLink, RtfScriptComment(FORMAT(L"# %s", (LoadStr(GENERATE_URL_PS_ARG_PASSING))))) + RtfPara +
      RtfLink(PsArgPassingLink, L"$PSNativeCommandArgumentPassing") + L" = " + AssemblyString(alPowerShell, L"Legacy") + RtfPara +
      RtfPara +
      RtfText(L"& \"" + ComExeName + "\" `") + RtfPara +
      RtfText(L"  ") + LogParameter + L" " + IniParameter + RtfText(L" `") + RtfPara +
      RtfText(L"  ") + CommandParameter;

    for (TCommands::const_iterator I = Commands.begin(); I != Commands.end(); I++)
    {
      UnicodeString Command = *I;
      if (!Command.IsEmpty())
      {
        Result += RtfText(L" `") + RtfPara + RtfText(L"    \"");
        if (Command[1] == L'#')
        {
          Command.Delete(1, 1);
          Result += RtfScriptPlaceholder(Command.TrimLeft());
        }
        else
        {
          Command = ReplaceStr(Command, L"`", L"``");
          Command = ReplaceStr(Command, L"$", L"`$");
          Result += RtfEscapeParam(Command, true);
        }
        Result += L"\"";
      }
    }

    Result +=
      RtfPara +
      RtfPara +
      RtfText(L"$winscpResult = $LastExitCode") + RtfPara +
      RtfKeyword(L"if") + RtfText(L" ($winscpResult -eq 0)") + RtfPara +
      RtfText(L"{") + RtfPara +
      RtfText(L"  ") + RtfKeyword(L"Write-Host") + L" " + RtfString(L"\"Success\"") + RtfPara +
      RtfText(L"}") + RtfPara +
      RtfText(L"else") + RtfPara +
      RtfText(L"{") + RtfPara +
      RtfText(L"  ") + RtfKeyword(L"Write-Host") + L" " + RtfString(L"\"Error\"") + RtfPara +
      RtfText(L"}") + RtfPara +
      RtfPara +
      RtfKeyword(L"exit") + RtfText(L" $winscpResult") + RtfPara;
  }

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TGenerateUrlDialog::GenerateAssemblyCode(UnicodeString & AssemblyDescription)
{
  TAssemblyLanguage Language = static_cast<TAssemblyLanguage>(AssemblyLanguageCombo->ItemIndex);

  UnicodeString Head;
  UnicodeString Tail;
  int Indent;

  FData->GenerateAssemblyCode(Language, Head, Tail, Indent);

  UnicodeString Result = Head;

  UnicodeString Code;
  if (FTransfer)
  {
    UnicodeString CopyParamProperties = FCopyParam.GenerateAssemblyCode(Language, FCopyParamAttrs);

    bool HasTransferOptions = !CopyParamProperties.IsEmpty();
    if (HasTransferOptions)
    {
      Code +=
        AssemblyCommentLine(Language, LoadStr(GENERATE_URL_COPY_PARAM)) +
        CopyParamProperties +
        RtfPara;
    }

    Code += AssemblyCommentLine(Language, LoadStr(GENERATE_URL_TRANSFER_FILES));

    AddSampleDescription(AssemblyDescription);

    UnicodeString DestPath = FPath;
    UnicodeString TransferMethodName;
    if (FToRemote)
    {
      TransferMethodName = L"PutFiles";
      DestPath = UnixIncludeTrailingBackslash(DestPath);
    }
    else
    {
      TransferMethodName = L"GetFiles";
      DestPath = IncludeTrailingBackslash(DestPath);
    }
    DestPath += NoOpOperationMask;

    UnicodeString DestPathVariableName = AssemblyVariableName(Language, L"remotePath");
    UnicodeString StatementSeparator = AssemblyStatementSeparator(Language);
    UnicodeString DestPathString = AssemblyString(Language, DestPath);
    UnicodeString DestPathCode;
    if ((FFilesSelected != fsList) || (FPaths->Count == 1))
    {
      DestPathCode = DestPathString;
    }
    else
    {
      switch (Language)
      {
        case alCSharp:
          Code += RtfKeyword(L"const") + L" " + RtfKeyword(L"string") + L" " + DestPathVariableName;
          break;

        case alVBNET:
          Code += RtfKeyword(L"Const") + L" " + DestPathVariableName;
          break;

        case alPowerShell:
          Code += DestPathVariableName;
          break;
      }
      Code += L" = " + DestPathString + StatementSeparator + RtfPara;
      DestPathCode = DestPathVariableName;
    }

    UnicodeString TransferMethodCallStart =
      AssemblyVariableName(Language, SessionClassName) + L"." +
      RtfLibraryMethod(SessionClassName, TransferMethodName, false) + L"(";
    const UnicodeString ParameterSeparator = L", ";
    UnicodeString TransferMethodCallEnd = ParameterSeparator + DestPathCode;
    if (FMove || HasTransferOptions)
    {
      TransferMethodCallEnd += ParameterSeparator + AssemblyBoolean(Language, FMove);
    }
    if (HasTransferOptions)
    {
      TransferMethodCallEnd += ParameterSeparator + AssemblyVariableName(Language, TransferOptionsClassName);
    }

    TransferMethodCallEnd +=
      L")." + RtfLibraryMethod(L"OperationResultBase", L"Check", true) + L"()" +
      StatementSeparator + RtfPara;

    if (FFilesSelected == fsList)
    {
      for (int Index = 0; Index < FPaths->Count; Index++)
      {
        UnicodeString FileName = FPaths->Strings[Index];
        UnicodeString Path;
        if (!FToRemote)
        {
          Path = UnixIncludeTrailingBackslash(FSourcePath) + FileName;
        }
        else
        {
          Path = IncludeTrailingBackslash(FSourcePath) + FileName;
        }
        UnicodeString PathCode = AssemblyString(Language, Path);
        if (!FToRemote && (FileName != EscapeFileMask(FileName)))
        {
          PathCode =
            AssemblyVariableName(Language, SessionClassName) + L"." +
            RtfLibraryMethod(SessionClassName, L"EscapeFileMask", false) + L"(" + PathCode + L")";
        }
        Code += TransferMethodCallStart + PathCode + TransferMethodCallEnd;
      }
    }
    else
    {
      UnicodeString SourcePath = FSourcePath;
      if (FToRemote)
      {
        SourcePath = IncludeTrailingBackslash(SourcePath);
      }
      else
      {
        SourcePath = UnixIncludeTrailingBackslash(SourcePath);
      }
      SourcePath += AllFilesMask;
      Code += TransferMethodCallStart + AssemblyString(Language, SourcePath) + TransferMethodCallEnd;
    }
  }
  else
  {
    Code = AssemblyCommentLine(Language, LoadStr(GENERATE_URL_YOUR_CODE));
  }

  UnicodeString Indentation = UnicodeString::StringOfChar(L' ', Indent);
  Code = Indentation + ReplaceStr(Code, RtfPara, RtfPara + Indentation);
  if (DebugAlwaysTrue(Code.SubString(Code.Length() - Indentation.Length() + 1, Indentation.Length()) == Indentation))
  {
    Code.SetLength(Code.Length() - Indentation.Length());
  }

  Result += Code;

  Result += Tail;

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::UpdateControls()
{
  if (!FChanging)
  {
    int CaptionId;
    if (FTransfer)
    {
      CaptionId = GENERATE_URL_TRANSFER_TITLE;
    }
    else
    {
      CaptionId = IsFileUrl() ? GENERATE_URL_FILE_TITLE : GENERATE_URL_SESSION_TITLE;
    }
    Caption = LoadStr(CaptionId);

    UrlSheet->TabVisible = !FTransfer;
    ScriptSheet->TabVisible = !IsFileUrl();
    AssemblySheet->TabVisible = !IsFileUrl();

    bool HostKeyUnknown = FData->UsesSsh && FData->HostKey.IsEmpty();

    UnicodeString ResultGroupCaption;
    if (OptionsPageControl->ActivePage == UrlSheet)
    {
      ResultGroupCaption = LoadStr(GENERATE_URL_URL);
    }
    else if (OptionsPageControl->ActivePage == ScriptSheet)
    {
      if (ScriptFormatCombo->ItemIndex == sfCommandLine)
      {
        ResultGroupCaption = LoadStr(GENERATE_URL_COMMANDLINE_LABEL);
      }
      else
      {
        ResultGroupCaption = ScriptFormatCombo->Items->Strings[ScriptFormatCombo->ItemIndex];
      }
    }
    else if (DebugAlwaysTrue(OptionsPageControl->ActivePage == AssemblySheet))
    {
      ResultGroupCaption = LoadStr(GENERATE_URL_CODE);
    }
    ResultGroup->Caption = ResultGroupCaption;

    EnableControl(UserNameCheck, !FData->UserNameExpanded.IsEmpty());
    bool UserNameIncluded = UserNameCheck->Enabled && UserNameCheck->Checked;
    EnableControl(PasswordCheck, UserNameIncluded && FData->HasPassword());
    EnableControl(HostKeyCheck, UserNameIncluded && !FData->HostKey.IsEmpty());
    EnableControl(RemoteDirectoryCheck, !FData->RemoteDirectory.IsEmpty() && !IsFileUrl());
    EnableControl(SaveExtensionCheck, !IsFileUrl());
    EnableControl(RawSettingsCheck, UserNameIncluded && FData->HasRawSettingsForUrl());

    UnicodeString Result;

    bool * Counted = NULL;
    UnicodeString CounterName;
    bool WordWrap = false; // shut up
    bool FixedWidth = false; // shut up
    if (OptionsPageControl->ActivePage == UrlSheet)
    {
      Counted = &FUrlCounted;
      CounterName = L"GeneratedUrls";

      Result = GenerateUrl();
      WordWrap = true;
      FixedWidth = false;
    }
    else if (OptionsPageControl->ActivePage == ScriptSheet)
    {
      Counted = &FScriptCounted;
      CounterName = FTransfer ? L"GeneratedScriptsTransfer" : L"GeneratedScripts";

      UnicodeString ScriptDescription;
      if (ScriptFormatCombo->ItemIndex == sfScriptFile)
      {
        WordWrap = false;
        FixedWidth = true;
      }
      else if (ScriptFormatCombo->ItemIndex == sfBatchFile)
      {
        WordWrap = false;
        FixedWidth = true;
      }
      else if (ScriptFormatCombo->ItemIndex == sfCommandLine)
      {
        WordWrap = true;
        FixedWidth = false;

        ScriptDescription = FMTLOAD(GENERATE_URL_COMMANDLINE_DESC, (FORMAT("\"%s\"", (Application->ExeName)))) + L"\n";
      }
      else if (ScriptFormatCombo->ItemIndex == sfPowerShell)
      {
        WordWrap = false;
        FixedWidth = true;
      }

      if (HostKeyUnknown)
      {
        ScriptDescription += LoadStr(GENERATE_URL_HOSTKEY_UNKNOWN) + L"\n";
      }

      Result = GenerateScript(ScriptDescription);

      ScriptDescriptionLabel->Caption = ScriptDescription;
    }
    else if (DebugAlwaysTrue(OptionsPageControl->ActivePage == AssemblySheet))
    {
      Counted = &FAssemblyCounted;
      CounterName = FTransfer ? L"GeneratedCodesTransfer" : L"GeneratedCodes";

      UnicodeString AssemblyDescription;
      if (HostKeyUnknown)
      {
        AssemblyDescription += LoadStr(GENERATE_URL_HOSTKEY_UNKNOWN) + L"\n";
      }

      Result = GenerateAssemblyCode(AssemblyDescription);

      AssemblyDescriptionLabel->Caption = AssemblyDescription;

      WordWrap = false;
      FixedWidth = true;
    }

    if (FixedWidth)
    {
      FResultMemoWithLinks->Font->Name = CustomWinConfiguration->DefaultFixedWidthFontName;
    }
    else
    {
      FResultMemoWithLinks->Font->Name = Font->Name;
    }

    if (!CounterName.IsEmpty() && !(*Counted))
    {
      (*Counted) = true;
      Configuration->Usage->Inc(CounterName);
    }

    Result =
      L"{\\rtf1\n"
       "{\\colortbl ;" +
       // The same RGB as on wiki
       RtfColorEntry(0x010101) + // near-black fake color to be used with no-style link to override the default blue underline
       RtfColorEntry(0x008000) + // code comment (green)
       RtfColorEntry(0x008080) + // class (teal)
       RtfColorEntry(0x800000) + // string (maroon)
       RtfColorEntry(0x0000FF) + // keyword (blue)
       RtfColorEntry(0x993333) + // command-line argument (reddish)
       RtfColorEntry(0x808080) + // script command (gray)
      L"}\n"
       "{\\fonttbl{\\f0\\fnil\\fcharset0 " + FResultMemoWithLinks->Font->Name + L";}}\n"
       "\\f0\\fs" + IntToStr(FResultMemoWithLinks->Font->Size * 2) + L" " +
       Result +
      "}";

    FResultMemoWithLinks->WordWrap = WordWrap;
    FResultMemoWithLinks->ScrollBars = WordWrap ? ssVertical : ssBoth;

    std::unique_ptr<TMemoryStream> Stream(new TMemoryStream());
    UTF8String ResultUtf = Result;
    Stream->Write(ResultUtf.c_str(), ResultUtf.Length());
    Stream->Position = 0;

    FResultMemoWithLinks->Perform(WM_VSCROLL, SB_TOP, 0);
    FResultMemoWithLinks->Lines->LoadFromStream(Stream.get(), TEncoding::UTF8);
  }
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::Execute()
{
  int Components = WinConfiguration->GenerateUrlComponents;
  if (Components < 0)
  {
    Components = UserNameCheck->Tag | RemoteDirectoryCheck->Tag;
  }
  TGenerateUrlCodeTarget Target = WinConfiguration->GenerateUrlCodeTarget;

  {
    TAutoFlag ChangingFlag(FChanging);

    if (IsFileUrl())
    {
      OptionsPageControl->ActivePage = UrlSheet;
    }
    else
    {
      switch (Target)
      {
        case guctUrl:
          OptionsPageControl->ActivePage = UrlSheet;
          break;

        case guctScript:
          OptionsPageControl->ActivePage = ScriptSheet;
          break;

        case guctAssembly:
          OptionsPageControl->ActivePage = AssemblySheet;
          break;

        default:
          DebugFail();
      }
    }

    for (int Index = 0; Index < UrlSheet->ControlCount; Index++)
    {
      TCheckBox * CheckBox = dynamic_cast<TCheckBox *>(UrlSheet->Controls[Index]);

      if (DebugAlwaysTrue((CheckBox != NULL) && (CheckBox->Tag != 0)))
      {
        CheckBox->Checked = FLAGSET(Components, CheckBox->Tag);
      }
    }

    ScriptFormatCombo->ItemIndex = WinConfiguration->GenerateUrlScriptFormat;

    AssemblyLanguageCombo->ItemIndex = WinConfiguration->GenerateUrlAssemblyLanguage;
  }

  UpdateControls();

  if (OptionsPageControl->ActivePage != UrlSheet)
  {
    ClientWidth = ScaleByTextHeightRunTime(this, 777);
    ClientHeight = ScaleByTextHeightRunTime(this, 666);
  }

  ShowModal();

  // Do not save the selection for files as the "URL" was selected implicitly
  if (!IsFileUrl())
  {
    if (OptionsPageControl->ActivePage == UrlSheet)
    {
      Target = guctUrl;
    }
    else if (OptionsPageControl->ActivePage == ScriptSheet)
    {
      Target = guctScript;
    }
    else if (OptionsPageControl->ActivePage == AssemblySheet)
    {
      Target = guctAssembly;
    }
    else
    {
      DebugFail();
    }
    WinConfiguration->GenerateUrlCodeTarget = Target;
  }

  if (Target == guctUrl)
  {
    Components = 0;
    for (int Index = 0; Index < UrlSheet->ControlCount; Index++)
    {
      TCheckBox * CheckBox = dynamic_cast<TCheckBox *>(UrlSheet->Controls[Index]);

      if (DebugAlwaysTrue((CheckBox != NULL) && (CheckBox->Tag != 0)) &&
          CheckBox->Checked)
      {
        Components |= CheckBox->Tag;
      }
    }
    WinConfiguration->GenerateUrlComponents = Components;
  }
  else if (Target == guctScript)
  {
    WinConfiguration->GenerateUrlScriptFormat = static_cast<TScriptFormat>(ScriptFormatCombo->ItemIndex);
  }
  else if (Target == guctAssembly)
  {
    WinConfiguration->GenerateUrlAssemblyLanguage = static_cast<TAssemblyLanguage>(AssemblyLanguageCombo->ItemIndex);
  }
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::ClipboardButtonClick(TObject * /*Sender*/)
{
  TInstantOperationVisualizer Visualizer;

  // Cannot read the text from FResultMemoWithLinks->Lines as TRichEdit (as opposite to TMemo)
  // breaks wrapped lines
  UnicodeString Text = FResultMemoWithLinks->Text;
  UnicodeString EOL = sLineBreak;
  int P = Pos(EOL, Text);
  // Trim the EOL of the only string, what CopyToClipboard(FResultMemoWithLinks->Lines) would have done.
  // It probably never happens as rich edit does not return EOL on the last line.
  if (DebugAlwaysFalse(P == Text.Length() - EOL.Length() + 1))
  {
    Text.SetLength(Text.Length() - EOL.Length());
  }
  // Add trailing EOL, if there are multiple lines (see above)
  else if ((P > 0) && !EndsStr(EOL, Text))
  {
    Text += EOL;
  }

  Text = RtfRemoveHyperlinks(Text);

  CopyToClipboard(Text);
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::WMNCCreate(TWMNCCreate & Message)
{
  // bypass TForm::WMNCCreate to prevent disabling "resize"
  // (which is done for bsDialog, see comments in CreateParams)
  DefaultHandler(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::Dispatch(void * AMessage)
{
  TMessage & Message = *reinterpret_cast<TMessage *>(AMessage);
  if (Message.Msg == WM_NCCREATE)
  {
    WMNCCreate(*reinterpret_cast<TWMNCCreate *>(AMessage));
  }
  else
  {
    TForm::Dispatch(AMessage);
  }
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::CreateParams(TCreateParams & Params)
{
  TForm::CreateParams(Params);

  // Allow resizing of the window, even if this is bsDialog.
  // This makes it more close to bsSizeable, but bsSizeable cannot for some
  // reason receive focus, if window is shown atop non-main window
  // (like editor)
  Params.Style = Params.Style | WS_THICKFRAME;
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::FormShow(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
