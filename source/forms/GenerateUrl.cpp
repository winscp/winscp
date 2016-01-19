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
// Rich edit 4.1 supports "Friendly name hyperlinks"
class TRichEdit41 : public TRichEdit
{
public:
  virtual __fastcall TRichEdit41(TComponent * AOwner);

protected:
  virtual void __fastcall CreateWnd();
  virtual void __fastcall CreateParams(TCreateParams & Params);
  virtual void __fastcall DestroyWnd();
  void __fastcall Dispatch(void * Message);

private:
  HINSTANCE FLibrary;
};
//---------------------------------------------------------------------------
__fastcall TRichEdit41::TRichEdit41(TComponent * AOwner) :
  TRichEdit(AOwner),
  FLibrary(0)
{
}
//---------------------------------------------------------------------------
void __fastcall TRichEdit41::CreateParams(TCreateParams & Params)
{
  UnicodeString RichEditModuleName(L"MSFTEDIT.DLL");
  long int OldError;

  OldError = SetErrorMode(SEM_NOOPENFILEERRORBOX);
  FLibrary = LoadLibrary(RichEditModuleName.c_str());
  SetErrorMode(OldError);

  TCustomMemo::CreateParams(Params);
  // Should not happen as
  if (FLibrary != 0)
  {
    // MSDN says that we should use MSFTEDIT_CLASS to load Rich Edit 4.1:
    // https://msdn.microsoft.com/en-us/library/windows/desktop/bb787873.aspx
    // But MSFTEDIT_CLASS is defined as "RICHEDIT50W",
    // so not sure what version we are loading.
    // Seem to work on Windows XP SP3.
    CreateSubClass(Params, MSFTEDIT_CLASS);
  }
}
//---------------------------------------------------------------------------
void __fastcall TRichEdit41::CreateWnd()
{
  TRichEdit::CreateWnd();
  int Mask = SendMessage(Handle, EM_GETEVENTMASK, 0, 0);
  SendMessage(Handle, EM_SETEVENTMASK, 0, Mask | ENM_LINK);
}
//---------------------------------------------------------------------------
void __fastcall TRichEdit41::Dispatch(void * AMessage)
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
          ShowHelp(Url);
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
void __fastcall TRichEdit41::DestroyWnd()
{
  TRichEdit::DestroyWnd();

  if (FLibrary != 0)
  {
    FreeLibrary(FLibrary);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TGenerateUrlDialog::TGenerateUrlDialog(
  TComponent * Owner, TSessionData * Data, TStrings * Paths)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FData = Data;
  FPaths = Paths;
  FChanging = false;

  FResultMemo41 = new TRichEdit41(this);
  FResultMemo41->Parent = ResultMemo->Parent;
  FResultMemo41->SetBounds(ResultMemo->Left, ResultMemo->Top, ResultMemo->Width, ResultMemo->Height);
  FResultMemo41->Anchors = ResultMemo->Anchors;
  FResultMemo41->BevelInner = ResultMemo->BevelInner;
  FResultMemo41->BevelOuter = ResultMemo->BevelOuter;
  FResultMemo41->BorderStyle = ResultMemo->BorderStyle;
  FResultMemo41->PopupMenu = ResultMemo->PopupMenu;
  FResultMemo41->TabOrder = ResultMemo->TabOrder;
  FResultMemo41->PlainText = false;
  ResultMemo->Visible = false;

  ReadOnlyControl(FResultMemo41);
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
  return RtfLink(L"scriptcommand_" + Command, RtfKeyword(Command));
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfCommandlineSwitch(const UnicodeString & Switch, const UnicodeString & Anchor)
{
  return RtfLink(L"commandline#" + Anchor, RtfParameter(TProgramParams::FormatSwitch(Switch.LowerCase())));
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
    if (OptionsPageControl->ActivePage == UrlSheet)
    {
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
      WordWrap = true;
    }
    else if (OptionsPageControl->ActivePage == ScriptSheet)
    {
      UnicodeString BaseExeName = ExtractFileBaseName(ExeName);
      UnicodeString OpenCommand = FData->GenerateOpenCommandArgs();
      UnicodeString CommandPlaceholder1 = FMTLOAD(GENERATE_URL_COMMAND, (1));
      UnicodeString CommandPlaceholder2 = FMTLOAD(GENERATE_URL_COMMAND, (2));
      UnicodeString LogParameter =
        RtfCommandlineSwitch(LOG_SWITCH, L"logging") + RtfText(L"=") +
        RtfScriptPlaceholder(L"\"" + LoadStr(GENERATE_URL_WRITABLE_PATH_TO_LOG) + RtfText(BaseExeName + L".log") + L"\"");
      UnicodeString IniParameter =
        RtfCommandlineSwitch(INI_SWITCH, L"configuration") + RtfText(UnicodeString(L"=") + INI_NUL);
      UnicodeString CommandParameter = RtfCommandlineSwitch(COMMAND_SWITCH, L"scripting");

      if (ScriptFormatCombo->ItemIndex == sfScriptFile)
      {
        Result =
          FORMAT(
            RtfScriptCommand(L"open") + L" %s" + RtfPara +
            RtfPara +
            RtfScriptComment("# %s") + RtfPara +
            RtfScriptComment("# %s") + RtfPara +
            RtfPara +
            RtfScriptCommand(L"exit") + RtfPara,
            (OpenCommand, CommandPlaceholder1, CommandPlaceholder2));
        WordWrap = false;
        FixedWidth = true;
      }
      else if (ScriptFormatCombo->ItemIndex == sfBatchFile)
      {
        UnicodeString ComExeName = ChangeFileExt(ExeName, L".com");

        Result =
          RtfScriptPlaceholder(L"@echo off") + RtfPara +
          RtfPara +
          RtfText(L"\"" + ComExeName + "\" ^") + RtfPara +
          RtfText(L"  ") + LogParameter + L" " + IniParameter + RtfText(L" ^") + RtfPara +
          RtfText(L"  ") + CommandParameter + RtfText(L" ^") + RtfPara +
          RtfText(L"    \"") + RtfScriptCommand(L"open") + RtfText(L" ") + EscapeParam(ReplaceStr(OpenCommand, L"%", L"%%")) + RtfText(L"\" ^") + RtfPara +
          RtfText(L"    \"") + RtfScriptPlaceholder(CommandPlaceholder1) + RtfText(L"\" ^") + RtfPara +
          RtfText(L"    \"") + RtfScriptPlaceholder(CommandPlaceholder2) + RtfText(L"\" ^") + RtfPara +
          RtfText(L"    \"") + RtfScriptCommand(L"exit") + RtfText(L"\"") + RtfPara +
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
          LogParameter + L" " +
          IniParameter + L" " +
          CommandParameter + L" " +
            RtfText(L"\"") + RtfScriptCommand(L"open") + RtfText(L" ") + EscapeParam(OpenCommand) + RtfText(L"\" ") +
            RtfText(L"\"") + RtfScriptPlaceholder(CommandPlaceholder1) + RtfText(L"\" ") +
            RtfText(L"\"") + RtfScriptPlaceholder(CommandPlaceholder2) + RtfText(L"\" ") +
            RtfText(L"\"") + RtfScriptCommand(L"exit") + RtfText(L"\"");
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
      FResultMemo41->Font->Name = CustomWinConfiguration->DefaultFixedWidthFontName;
    }
    else
    {
      FResultMemo41->ParentFont = true;
    }

    Result =
      L"{\\rtf1\n"
       "{\\colortbl ;" +
       // The same RGB as on wiki
       RtfColorEntry(0x010101) + // near-black fake color to be used with no-style link to ovreride the default blue underline
       RtfColorEntry(0x008000) + // code comment (green)
       RtfColorEntry(0x008080) + // class (teal)
       RtfColorEntry(0x800000) + // string (maroon)
       RtfColorEntry(0x0000FF) + // keyword (blue)
       RtfColorEntry(0x993333) + // command-line argument (reddish)
       RtfColorEntry(0x808080) + // script command (gray)
      L"}\n"
       "{\\fonttbl{\\f0\\fnil\\fcharset0 " + FResultMemo41->Font->Name + L";}}\n"
       "\\f0\\fs" + IntToStr(FResultMemo41->Font->Size * 2) + L" " +
       Result +
      "}";

    FResultMemo41->WordWrap = WordWrap;
    FResultMemo41->ScrollBars = WordWrap ? ssVertical : ssBoth;

    std::unique_ptr<TMemoryStream> Stream(new TMemoryStream());
    UTF8String ResultUtf = Result;
    Stream->Write(ResultUtf.c_str(), ResultUtf.Length());
    Stream->Position = 0;

    FResultMemo41->Perform(WM_VSCROLL, SB_TOP, 0);
    FResultMemo41->Lines->LoadFromStream(Stream.get(), TEncoding::UTF8);
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

  // Cannot read the text from FResultMemo41->Lines as TRichEdit (as opposite to TMemo)
  // breaks wrapped lines
  UnicodeString Text = FResultMemo41->Text;
  UnicodeString EOL = sLineBreak;
  int P = Pos(EOL, Text);
  // Trim the EOL of the only string, what CopyToClipbaord(FResultMemo41->Lines) would have done.
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

  // Remove all tags HYPERLINK "http://www.example.com"
  int Index = 1;
  while ((P = PosFrom(RtfHyperlinkFieldPrefix, Text, Index)) > 0)
  {
    int Index2 = P + RtfHyperlinkFieldPrefix.Length();
    UnicodeString RtfHyperlinkFieldSuffix = L"\" ";
    int P2 = PosFrom(RtfHyperlinkFieldSuffix, Text, Index2);
    if (P2 > 0)
    {
      Text.Delete(P, P2 - P + RtfHyperlinkFieldSuffix.Length());
    }
    else
    {
      Index = Index2;
    }
  }

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
void __fastcall TGenerateUrlDialog::ResultMemoContextPopup(TObject * Sender,
  TPoint & MousePos, bool & Handled)
{
  MenuPopup(Sender, MousePos, Handled);
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::FormShow(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
