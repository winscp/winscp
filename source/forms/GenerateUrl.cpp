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
//---------------------------------------------------------------------------
#pragma package(smart_init)
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
void __fastcall DoGenerateUrlDialog(TSessionData * Data, TStrings * Paths)
{
  std::unique_ptr<TGenerateUrlDialog> Dialog(new TGenerateUrlDialog(GetFormOwner(), Data, Paths));
  Dialog->Execute();
}
//---------------------------------------------------------------------------
__fastcall TGenerateUrlDialog::TGenerateUrlDialog(
  TComponent * Owner, TSessionData * Data, TStrings * Paths)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FData = Data;
  FPaths = Paths;
  FChanging = false;
  ReadOnlyControl(ResultMemo);
}
//---------------------------------------------------------------------------
bool __fastcall TGenerateUrlDialog::IsFileUrl()
{
  return (FPaths != NULL);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TGenerateUrlDialog::GenerateUrl(UnicodeString Path)
{
  UnicodeString Url =
    FData->GenerateSessionUrl(
      FLAGMASK(WinSCPSpecificCheck->Checked, sufSpecific) |
      FLAGMASK(UserNameCheck->Enabled && UserNameCheck->Checked, sufUserName) |
      FLAGMASK(PasswordCheck->Enabled && PasswordCheck->Checked, sufPassword) |
      FLAGMASK(HostKeyCheck->Enabled && HostKeyCheck->Checked, sufHostKey));

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
  return RtfColorItalicText(6, Text);
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::UpdateControls()
{
  if (!FChanging)
  {
    UnicodeString ExeName = Application->ExeName;
    Caption = LoadStr(IsFileUrl() ? GENERATE_URL_FILE_TITLE : GENERATE_URL_SESSION_TITLE);

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
      UnicodeString ScriptDescription;
      if (ScriptFormatCombo->ItemIndex == sfCommandLine)
      {
        ResultGroupCaption = LoadStr(GENERATE_URL_COMMANDLINE_LABEL);
        ScriptDescription = FMTLOAD(GENERATE_URL_COMMANDLINE_DESC, (FORMAT("\"%s\"", (ExeName)))) + L"\n";
      }
      else
      {
        ResultGroupCaption = ScriptFormatCombo->Items->Strings[ScriptFormatCombo->ItemIndex];
      }
      if (HostKeyUnknown)
      {
        ScriptDescription += LoadStr(GENERATE_URL_HOSTKEY_UNKNOWN) + L"\n";
      }
      ScriptDescriptionLabel->Caption = ScriptDescription;
    }
    else if (DebugAlwaysTrue(OptionsPageControl->ActivePage == AssemblySheet))
    {
      ResultGroupCaption = LoadStr(GENERATE_URL_CODE);
      UnicodeString AssemblyDescription;
      if (HostKeyUnknown)
      {
        AssemblyDescription += LoadStr(GENERATE_URL_HOSTKEY_UNKNOWN) + L"\n";
      }
      AssemblyDescriptionLabel->Caption = AssemblyDescription;
    }
    ResultGroup->Caption = ResultGroupCaption;

    EnableControl(UserNameCheck, !FData->UserNameExpanded.IsEmpty());
    bool UserNameIncluded = UserNameCheck->Enabled && UserNameCheck->Checked;
    EnableControl(PasswordCheck, UserNameIncluded && FData->HasPassword());
    EnableControl(HostKeyCheck, UserNameIncluded && !FData->HostKey.IsEmpty());
    EnableControl(RemoteDirectoryCheck, !FData->RemoteDirectory.IsEmpty() && !IsFileUrl());
    EnableControl(SaveExtensionCheck, !IsFileUrl());

    UnicodeString Result;

    bool WordWrap = false;
    bool FixedWidth = false;
    FPlainResult = L"";
    if (OptionsPageControl->ActivePage == UrlSheet)
    {
      if (!IsFileUrl())
      {
        UnicodeString Path = FData->RemoteDirectory;
        if (!Path.IsEmpty() && !EndsStr(L"/", Path))
        {
          Path += L"/";
        }
        FPlainResult = GenerateUrl(Path);
        Result = RtfText(FPlainResult);
      }
      else
      {
        for (int Index = 0; Index < FPaths->Count; Index++)
        {
          UnicodeString Url = GenerateUrl(FPaths->Strings[Index]);
          Result += RtfText(Url) + RtfPara;
          FPlainResult +=
            Url +
            // What CopyToClipboard would have done could we pass in ResultMemo->Lines
            ((FPaths->Count > 0) ? L"\n" : L"");
        }
      }
      WordWrap = true;
    }
    else if (OptionsPageControl->ActivePage == ScriptSheet)
    {
      UnicodeString BaseExeName = ExtractFileBaseName(ExeName);
      UnicodeString OpenCommand = FData->GenerateOpenCommandArgs();
      UnicodeString CommandPlaceholder1 = FMTLOAD(GENERATE_URL_COMMAND, (1));
      UnicodeString CommandPlaceholder2 = FMTLOAD(GENERATE_URL_COMMAND, (2));

      if (ScriptFormatCombo->ItemIndex == sfScriptFile)
      {
        Result =
          FORMAT(
            RtfKeyword(L"open") + L" %s" + RtfPara +
            RtfPara +
            RtfScriptComment("; %s") + RtfPara +
            RtfScriptComment("; %s") + RtfPara +
            RtfPara +
            RtfKeyword(L"exit") + RtfPara,
            (OpenCommand, CommandPlaceholder1, CommandPlaceholder2));
        WordWrap = false;
        FixedWidth = true;
      }
      else if (ScriptFormatCombo->ItemIndex == sfBatchFile)
      {
        UnicodeString ComExeName = ChangeFileExt(ExeName, L".com");

        Result =
          RtfScriptComment(L"@echo off") + RtfPara +
          RtfPara +
          RtfText(L"\"" + ComExeName + "\" ") + RtfParameter(L"/log") + RtfText(L"=" + BaseExeName + L".log ") + RtfParameter(L"/ini") + RtfText(L"=nul ") + RtfParameter(L"/command") + RtfText(L" ^") + RtfPara +
          RtfText(L"  \"") + RtfKeyword(L"open") + RtfText(L" ") + EscapeParam(ReplaceStr(OpenCommand, L"%", L"%%")) + RtfText(L"\" ^") + RtfPara +
          RtfText(L"  \"") + RtfScriptComment(CommandPlaceholder1) + RtfText(L"\" ^") + RtfPara +
          RtfText(L"  \"") + RtfScriptComment(CommandPlaceholder2) + RtfText(L"\" ^") + RtfPara +
          RtfText(L"  \"") + RtfKeyword(L"exit") + RtfText(L"\"") + RtfPara +
          RtfPara +
          RtfKeyword(L"set") + RtfText(L" WINSCP_RESULT=%ERRORLEVEL%") + RtfPara +
          RtfKeyword(L"if") + RtfText(L" %WINSCP_RESULT% ") + RtfKeyword(L"equ") + RtfText(L" 0 (") + RtfPara +
          RtfText(L"  ") + RtfKeyword(L"echo") + RtfText(L" Success") + RtfPara +
          RtfText(L") ") + RtfKeyword(L"else") + RtfText(L" (") + RtfPara +
          RtfText(L"  ") + RtfKeyword(L"echo") + RtfText(L" Error") + RtfPara +
          RtfText(L")") + RtfPara +
          RtfPara +
          RtfKeyword(L"exit") + RtfText(L" /b %WINSCP_RESULT%") + RtfPara;
        WordWrap = false;
        FixedWidth = true;
      }
      else if (ScriptFormatCombo->ItemIndex == sfCommandLine)
      {
        Result =
          RtfParameter(L"/log") + RtfText(L"=" + BaseExeName + L".log ") +
          RtfParameter(L"/ini") + RtfText(L"=nul ") +
          RtfParameter(L"/command") + RtfText(L" ") +
            RtfText(L"\"") + RtfKeyword(L"open") + RtfText(L" ") + EscapeParam(OpenCommand) + RtfText(L"\" ") +
            RtfText(L"\"") + RtfScriptComment(CommandPlaceholder1) + RtfText(L"\" ") +
            RtfText(L"\"") + RtfScriptComment(CommandPlaceholder2) + RtfText(L"\" ") +
            RtfText(L"\"") + RtfKeyword(L"exit") + RtfText(L"\"");
        WordWrap = true;
        FixedWidth = false;
      }
    }
    else if (DebugAlwaysTrue(OptionsPageControl->ActivePage == AssemblySheet))
    {
      Result = FData->GenerateAssemblyCode(static_cast<TAssemblyLanguage>(AssemblyLanguageCombo->ItemIndex));
      WordWrap = false;
      FixedWidth = true;
    }

    if (FixedWidth)
    {
      ResultMemo->Font->Name = CustomWinConfiguration->DefaultFixedWidthFontName;
      ResultMemo->DefAttributes->Color = clWindowText;
    }
    else
    {
      ResultMemo->ParentFont = true;
    }

    Result =
      L"{\\rtf1\n"
       "{\\colortbl ;" +
       // The same RGB as on wiki
       RtfColorEntry(0x008000) + // cde comment (green)
       RtfColorEntry(0x008080) + // class (teal)
       RtfColorEntry(0x800000) + // string (maroon)
       RtfColorEntry(0x0000FF) + // keyword (blue)
       RtfColorEntry(0x993333) + // command-line argument (reddish)
       RtfColorEntry(0x808080) + // script command (gray)
      L"}\n"
       "{\\fonttbl{\\f0\\fnil\\fcharset0 " + ResultMemo->Font->Name + L";}}\n"
       "\\f0\\fs" + IntToStr(ResultMemo->Font->Size * 2) + L" " +
       Result +
      L"}";

    ResultMemo->WordWrap = WordWrap;
    ResultMemo->ScrollBars = WordWrap ? ssVertical : ssBoth;

    std::unique_ptr<TMemoryStream> Stream(new TMemoryStream());
    UTF8String ResultUtf = Result;
    Stream->Write(ResultUtf.c_str(), ResultUtf.Length());
    Stream->Position = 0;

    Stream->SaveToFile(L"b:\\rtf\\code.rtf");
    Stream->Position = 0;

    ResultMemo->Lines->LoadFromStream(Stream.get(), TEncoding::UTF8);
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
  if (ResultMemo->WordWrap)
  {
    // Cannot read the text from ResultMemo->Lines as TRichEdit (as opposite to TMemo)
    // breaks wrapped lines

    if (!FPlainResult.IsEmpty())
    {
      CopyToClipboard(FPlainResult);
    }
    else
    {
      // We get here with command-line only,
      // where we know to have a single line only
      DebugAssert((OptionsPageControl->ActivePage == ScriptSheet) && (ScriptFormatCombo->ItemIndex == sfCommandLine));
      UnicodeString Text;
      for (int Index = 0; Index < ResultMemo->Lines->Count; Index++)
      {
        Text += ResultMemo->Lines->Strings[Index];
      }
      CopyToClipboard(Text);
    }
  }
  else
  {
    // On the other hand, the FResult contains RTF markup
    // in which case we want to use ResultMemo->Lines
    CopyToClipboard(ResultMemo->Lines);
  }
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
