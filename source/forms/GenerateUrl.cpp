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
void __fastcall TGenerateUrlDialog::UpdateControls()
{
  if (!FChanging)
  {
    Caption = LoadStr(IsFileUrl() ? GENERATE_URL_FILE_TITLE : GENERATE_URL_SESSION_TITLE);

    ScriptSheet->TabVisible = !IsFileUrl();
    AssemblySheet->TabVisible = !IsFileUrl();

    UnicodeString ResultGroupCaption;
    if (OptionsPageControl->ActivePage == UrlSheet)
    {
      ResultGroupCaption = LoadStr(GENERATE_URL_URL);
    }
    else if (OptionsPageControl->ActivePage == ScriptSheet)
    {
      ResultGroupCaption = ScriptFormatCombo->Items->Strings[ScriptFormatCombo->ItemIndex];
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

    UnicodeString Result;

    bool WordWrap = false;
    bool FixedWidth = false;
    if (OptionsPageControl->ActivePage == UrlSheet)
    {
      if (!IsFileUrl())
      {
        UnicodeString Path = FData->RemoteDirectory;
        if (!Path.IsEmpty() && !EndsStr(L"/", Path))
        {
          Path += L"/";
        }
        Result = GenerateUrl(Path);
      }
      else
      {
        for (int Index = 0; Index < FPaths->Count; Index++)
        {
          Result += GenerateUrl(FPaths->Strings[Index]) + L"\n";
        }
      }
      WordWrap = true;
    }
    else if (OptionsPageControl->ActivePage == ScriptSheet)
    {
      UnicodeString ExeName = Application->ExeName;
      UnicodeString BaseExeName = ExtractFileBaseName(ExeName);
      UnicodeString OpenCommand = FData->GenerateOpenCommandArgs();
      UnicodeString CommandPlaceholder1 = FMTLOAD(GENERATE_URL_COMMAND, (1));
      UnicodeString CommandPlaceholder2 = FMTLOAD(GENERATE_URL_COMMAND, (2));

      if (ScriptFormatCombo->ItemIndex == sfScriptFile)
      {
        Result =
          FORMAT(
            L"open %s\n"
             "\n"
             "; %s\n"
             "; %s\n"
             "\n"
             "exit\n",
            (OpenCommand, CommandPlaceholder1, CommandPlaceholder2));
        WordWrap = false;
        FixedWidth = true;
      }
      else if (ScriptFormatCombo->ItemIndex == sfBatchFile)
      {
        UnicodeString ComExeName = ChangeFileExt(ExeName, L".com");

        Result =
          FORMAT(
            L"@echo off\n"
             "\n"
             "\"%s\" /log=%s.log /ini=nul /command ^\n"
             "  \"open %s\" ^\n"
             "  \"%s\" ^\n"
             "  \"%s\" ^\n"
             "  \"exit\"\n"
             "\n"
             "set WINSCP_RESULT=%%ERRORLEVEL%%\n"
             "if %%WINSCP_RESULT%% equ 0 (\n"
             "  echo Success\n"
             ") else (\n"
             "  echo Error\n"
             ")\n"
             "\n"
             "exit /b %%WINSCP_RESULT%%\n",
             (ComExeName, BaseExeName, EscapeParam(ReplaceStr(OpenCommand, L"%", L"%%")), CommandPlaceholder1, CommandPlaceholder2));
        WordWrap = false;
        FixedWidth = true;
      }
      else if (ScriptFormatCombo->ItemIndex == sfCommandLine)
      {
        Result =
          FORMAT(
            L"\"%s\" /console /log=%s.log /ini=nul /command \"open %s\" \"%s\" \"%s\" \"exit\"",
            (ExeName, BaseExeName, EscapeParam(OpenCommand), CommandPlaceholder1, CommandPlaceholder2));
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

    ResultMemo->WordWrap = WordWrap;
    ResultMemo->ScrollBars = WordWrap ? ssVertical : ssBoth;
    ResultMemo->Lines->Text = Result;

    if (FixedWidth)
    {
      ResultMemo->Font->Name = CustomWinConfiguration->DefaultFixedWidthFontName;
    }
    else
    {
      ResultMemo->ParentFont = true;
    }
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
  CopyToClipboard(ResultMemo->Lines);
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
