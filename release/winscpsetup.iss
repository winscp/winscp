#define MainFileSource "..\WinSCP3.exe"
#define ParentRegistryKey "Software\Martin Prikryl"
#define RegistryKey ParentRegistryKey+"\WinSCP 2"
#define PuttySourceDir "c:\Program Files\PuTTY"
#define TranslationMask "translations\WinSCP3.???"
#define TranslationDir ".\translations"
#define DefaultLang "en"

#ifexist "winscpsetup.tmp"
#include "winscpsetup.tmp"
#else
#define Status "unofficial"
#endif

#define Major
#define Minor
#define Rev
#define Build
#expr ParseVersion(MainFileSource, Major, Minor, Rev, Build)
#define Version Str(Major)+"."+Str(Minor)+(Rev > 0 ? "."+Str(Rev) : "")+(Status != "" ? " "+Status : "")

#define SetupExt
#define FullLangs

#ifdef INTL
  #expr SetupExt="intl"
  #expr FullLangs="fulllangs"
#endif

; Translation support functions/variables

[Setup]
AppId=winscp3
AppName=WinSCP3
AppMutex=winscp3
AppPublisher=Martin Prikryl
AppPublisherURL=http://winscp.sourceforge.net/
AppSupportURL=http://winscp.sourceforge.net/forum/
AppUpdatesURL=http://winscp.sourceforge.net/eng/download.php
DefaultDirName={pf}\WinSCP3
DefaultGroupName=WinSCP3
AllowNoIcons=yes
LicenseFile=licence
UninstallDisplayIcon={app}\WinSCP3.exe
OutputDir=files\
DisableStartupPrompt=yes
AppVersion={#Version}
AppVerName=WinSCP {#Version}
OutputBaseFilename=winscp{#Major}{#Minor}{#Rev}setup{#SetupExt}
Compression=bzip/9
ShowTasksTreeLines=yes

#define FindHandle
#define ProcessTranslationMode
#define ForceLang 0
#dim Languages[100]
#define RuntimeMessageCount
#dim RuntimeMessageID[100]
#dim RuntimeMessages[100][100]
#define LanguageCount 0
#define LangI
#define MessageID
#define MessagesPath

#expr RuntimeMessageCount = 15
#define UserSettingsTitle 0
#expr RuntimeMessageID[UserSettingsTitle] = "UserSettingsTitle"
#define UserSettingsPrompt 1
#expr RuntimeMessageID[UserSettingsPrompt] = "UserSettingsPrompt"
#define UserInterfaceStyle 2
#expr RuntimeMessageID[UserInterfaceStyle] = "UserInterfaceStyle"
#define NortonCommanderInterface 3
#expr RuntimeMessageID[NortonCommanderInterface] = "NortonCommanderInterface"
#define NortonCommanderInterface1 4
#expr RuntimeMessageID[NortonCommanderInterface1] = "NortonCommanderInterface1"
#define NortonCommanderInterface2 5
#expr RuntimeMessageID[NortonCommanderInterface2] = "NortonCommanderInterface2"
#define NortonCommanderInterface3 6
#expr RuntimeMessageID[NortonCommanderInterface3] = "NortonCommanderInterface3"
#define NortonCommanderInterface4 7
#expr RuntimeMessageID[NortonCommanderInterface4] = "NortonCommanderInterface4"
#define ExplorerInterface 8
#expr RuntimeMessageID[ExplorerInterface] = "ExplorerInterface"
#define ExplorerInterface1 9
#expr RuntimeMessageID[ExplorerInterface1] = "ExplorerInterface1"
#define ExplorerInterface2 10
#expr RuntimeMessageID[ExplorerInterface2] = "ExplorerInterface2"
#define ExplorerInterface3 11
#expr RuntimeMessageID[ExplorerInterface3] = "ExplorerInterface3"
#define AdditionalOptions 12
#expr RuntimeMessageID[AdditionalOptions] = "AdditionalOptions"
#define AdvancedLoginOptions 13
#expr RuntimeMessageID[AdvancedLoginOptions] = "AdvancedLoginOptions"
#define UserSettingsOverview 14
#expr RuntimeMessageID[UserSettingsOverview] = "UserSettingsOverview"

#sub AddRuntimeMessage
  #expr RuntimeMessages[LanguageCount * RuntimeMessageCount + MessageID] = \
    ReadIni(MessagesPath, "WinSCP Runtime", RuntimeMessageID[MessageID])
#endsub

#sub ProcessTranslationFile

#if FindHandle
  #define FileName FindGetFileName(FindHandle)
  #define Lang Copy(FileName, Pos(".", FileName)+1)
#else
  #define Lang DefaultLang
#endif

#define MessagesFile "WinSCP3." + Lang + ".isl"
#expr MessagesPath = TranslationDir + "\" + MessagesFile
#define LangName ReadIni(MessagesPath, "LangOptions", "LanguageName")
#define LangID ReadIni(MessagesPath, "LangOptions", "LanguageID")
#define Transl(S) ReadIni(MessagesPath, "WinSCP", S)

#if Lang != DefaultLang
  #expr Languages[LanguageCount*3] = Lang
  #expr Languages[LanguageCount*3+1] = LangName
  #expr Languages[LanguageCount*3+2] = LangID
  #expr LanguageCount++
#endif

#for {MessageID = 0; MessageID < RuntimeMessageCount; MessageID++} \
  AddRuntimeMessage

[Languages]
Name: {#Lang}; MessagesFile: {#MessagesPath}

[Types]
Name: full; Description: {#Transl("FullInstallation")}; Languages: {#Lang}
#ifdef INTL
Name: fulllangs; Description: {#Transl("FullInstallationLangs")}; \
  Languages: {#Lang}
#endif
Name: compact; Description: {#Transl("CompactInstallation")}; \
  Languages: {#Lang}
Name: custom; Description: {#Transl("CustomInstallation")}; \
  Flags: iscustom; Languages: {#Lang}

[Components]
Name: main; Description: {#Transl("ApplicationComponent")}; \
  Types: {#FullLangs} full custom compact; Flags: fixed; Languages: {#Lang}
Name: pageant; Description: {#Transl("PuTTYgenComponent")}; \
  Types: {#FullLangs} full; Languages: {#Lang}
Name: puttygen; Description: {#Transl("PageantComponent")}; \
  Types: {#FullLangs} full; Languages: {#Lang}
#ifdef INTL
Name: transl; Description: {#Transl("TranslationsComponent")}; \
  Types: full fulllangs; Languages: {#Lang}
#endif

[Tasks]
; Windows integration
Name: desktopicon; Description: {#Transl("DesktopIconTask")}; Languages: {#Lang}
Name: desktopicon\user; Description: {#Transl("DesktopIconUserTask")}; \
  Flags: exclusive; Languages: {#Lang}
Name: desktopicon\common; Description: {#Transl("DesktopIconCommonTask")}; \
  Flags: exclusive unchecked; Languages: {#Lang}
Name: quicklaunchicon; Description: {#Transl("QuickLaunchIconTask")}; \
  Flags: unchecked; Languages: {#Lang}
Name: sendtohook; Description: {#Transl("SendToHookTask")}; Languages: {#Lang}

[INI]
Filename: "{app}\{#Transl("SupportForum")}.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://winscp.sourceforge.net/forum/"; Languages: {#Lang}
Filename: "{app}\WinSCP.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://winscp.sourceforge.net/"; Languages: {#Lang}
Filename: "{app}\PuTTY\PuTTY.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://www.chiark.greenend.org.uk/~sgtatham/putty/"; Components: pageant puttygen; Languages: {#Lang}

[Icons]
; This is created always (unless user checks Don't create a Start menu folder, Setup\AllowNoIcons=yes)
Name: "{group}\WinSCP"; Filename: "{app}\WinSCP3.exe"; \
  Components: main; Languages: {#Lang}
Name: "{group}\{#Transl("WebSite")}"; Filename: "{app}\WinSCP.url"; \
  Components: main; Languages: {#Lang}
Name: "{group}\{#Transl("SupportForum")}"; \
  Filename: "{app}\{#Transl("SupportForum")}.url"; Components: main; \
  Languages: {#Lang}
; This is created when pageant/puttygen component is selected (unless user checks Don't create a Start
; menu folder, Setup\AllowNoIcons=yes). Flag createonlyiffileexists is used instead of "Components: xxx",
; because it would force creating the icons even when user doesn't want to create start menu folder.
Name: "{group}\{#Transl("RSAKeyTools")}\PuTTYgen"; \
  Filename: "{app}\PuTTY\puttygen.exe"; Components: puttygen; Languages: {#Lang}
Name: "{group}\{#Transl("RSAKeyTools")}\{#Transl("PuTTYgenManual")}"; \
  Filename: "winhlp32.exe"; Parameters: "-iputtygen.general {app}\PuTTY\putty.hlp"; \
  Components: puttygen; Languages: {#Lang}
Name: "{group}\{#Transl("RSAKeyTools")}\Pageant"; \
  Filename: "{app}\PuTTY\pageant.exe"; Components: pageant; Languages: {#Lang}
Name: "{group}\{#Transl("RSAKeyTools")}\{#Transl("PageantManual")}"; \
  Filename: "winhlp32.exe"; Parameters: "-ipageant.general {app}\PuTTY\putty.hlp"; \
  Components: pageant; Languages: {#Lang}
Name: "{group}\{#Transl("RSAKeyTools")}\{#Transl("KeysManual")}"; \
  Filename: "winhlp32.exe"; Parameters: "-it00000112 {app}\PuTTY\putty.hlp"; \
  Components: pageant puttygen; Languages: {#Lang}
Name: "{group}\{#Transl("RSAKeyTools")}\{#Transl("PuttyWebSite")}"; \
  Filename: "{app}\PuTTY\PuTTY.url"; Components: pageant puttygen; \
  Languages: {#Lang}
; This is created when desktopicon task is selected
Name: "{userdesktop}\WinSCP3"; Filename: "{app}\WinSCP3.exe"; \
  Tasks: desktopicon\user; Languages: {#Lang}
Name: "{commondesktop}\WinSCP3"; Filename: "{app}\WinSCP3.exe"; \
  Tasks: desktopicon\common; Languages: {#Lang}
; This is created when quicklaunchicon task is selected
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\WinSCP3"; \
  Filename: "{app}\WinSCP3.exe"; Tasks: quicklaunchicon; Languages: {#Lang}
; This is created when sendtohook task is selected
Name: "{sendto}\{#Transl("SendToHook")}"; Filename: "{app}\WinSCP3.exe"; \
  Parameters: "/upload"; Tasks: sendtohook; Languages: {#Lang}

[InstallDelete]
Type: files; Name: "{sendto}\WinSCP3 (upload using SCP).lnk"; Languages: {#Lang}
Type: files; Name: "{sendto}\{#Transl("SendToHook")}.lnk"; Languages: {#Lang}

[Run]
Filename: "{app}\WinSCP3.exe"; Description: "{#Transl("Launch")}"; Flags: nowait postinstall skipifsilent; Languages: {#Lang}

[UninstallDelete]
; These additional files are created by installer
Type: files; Name: "{app}\WinSCP.url"; Languages: {#Lang}
Type: files; Name: "{app}\{#Transl("SupportForum")}.url"; Languages: {#Lang}
Type: files; Name: "{app}\PuTTY\PuTTY.url"; Components: pageant puttygen; Languages: {#Lang}
; These additional files are created by application
Type: files; Name: "{app}\WinSCP3.ini"; Languages: {#Lang}

#endsub

#define FindHandle 0
#expr ProcessTranslationFile

#ifdef INTL

#if FindHandle = FindFirst(TranslationMask, 0)
  #define FResult 1
  #for {0; FResult; FResult = FindNext(FindHandle)} ProcessTranslationFile
  #expr FindClose(FindHandle)
#endif

[Components]
Name: transl\eng; Description: "English"; Types: fulllangs full custom compact; \
  Flags: fixed

#endif

[Files]
Source: "{#MainFileSource}"; DestDir: "{app}"; Components: main; \
  Flags: ignoreversion
Source: "licence"; DestName: "licence"; DestDir: "{app}"; \
  Components: main; Flags: ignoreversion
Source: "{#PuttySourceDir}\LICENCE"; DestDir: "{app}\PuTTY"; \
  Components: pageant puttygen; Flags: ignoreversion
Source: "{#PuttySourceDir}\putty.hlp"; DestDir: "{app}\PuTTY"; \
  Components: pageant puttygen; Flags: ignoreversion
Source: "{#PuttySourceDir}\pageant.exe"; DestDir: "{app}\PuTTY"; \
  Components: pageant; Flags: ignoreversion
Source: "{#PuttySourceDir}\puttygen.exe"; DestDir: "{app}\PuTTY"; \
  Components: puttygen; Flags: ignoreversion

[Registry]
Root: HKCU; Subkey: "{#ParentRegistryKey}"; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: "{#RegistryKey}"; Flags: uninsdeletekeyifempty
; Norton Commander interface
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType:dword; \
  ValueName: "Interface"; ValueData: 0; Check: IsTrue(10)
; Explorer-like interface
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType:dword; \
  ValueName: "Interface"; ValueData: 1; Check: IsTrue(11)
; Advanced tab on login dialog
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType:dword; \
  ValueName: "ShowAdvancedLoginOptions"; ValueData: 0; Check: IsTrue(20)
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType:dword; \
  ValueName: "ShowAdvancedLoginOptions"; ValueData: 1; Check: IsTrue(21)

#ifdef INTL

#sub EmitLang

[Components]
Name: transl\{#Languages[LangI*3]}; Description: {#Languages[LangI*3+1]}; \
  Types: fulllangs full compact custom; Check: IsLang({#Languages[LangI*3]})
Name: transl\{#Languages[LangI*3]}; Description: {#Languages[LangI*3+1]}; \
  Types: fulllangs; Check: IsNotLang({#Languages[LangI*3]})

[Files]
Source: "{#TranslationDir}\WinSCP3.{#Languages[LangI*3]}"; DestDir: "{app}"; \
  Components: transl\{#Languages[LangI*3]}; Flags: ignoreversion

[Registry]
; set program default language to setup language, but only if user installs it
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; \
  ValueType: dword; ValueName: "LocaleSafe"; ValueData: {#Languages[LangI*3+2]}; \
  Components: transl\{#Languages[LangI*3]}; Languages: {#Languages[LangI*3]}

#endsub

#for {LangI = 0; LangI < LanguageCount; LangI++} EmitLang

[Components]
Name: transl\ru; Description: "Russian"; Types: fulllangs custom compact

[Files]
Source: "translations.nosetup\WinSCP3.ru"; DestDir: "{app}"; \
  Components: transl\ru; Flags: ignoreversion

#endif

[UninstallRun]
Filename: "{app}\WinSCP3.exe"; Parameters: "/RandomSeedFileCleanup"; RunOnceId: "RandomSeedFileCleanup"

[Code]
var
  UserInterface: Cardinal;
  AdvancedTabs: Cardinal;
  RuntimeMessages: array[0..{#RuntimeMessageCount - 1}] of string;

procedure InitRuntimeMessages;
begin

#sub EmitRuntimeMessage
    RuntimeMessages[{#MessageID}] :=
      '{#RuntimeMessages[LangI * RuntimeMessageCount + MessageID]}';
#endsub

#sub EmitLangRuntimeMessages

  if ActiveLanguage = '{#LangI == 0 ? DefaultLang : Languages[(LangI - 1) * 3]}' then
  begin
    #for {MessageID = 0; MessageID < RuntimeMessageCount; MessageID++} \
      EmitRuntimeMessage
  end;

#endsub

#for {LangI = 0; LangI <= LanguageCount; LangI++} EmitLangRuntimeMessages

end;

function IsLang(Lang: String): Boolean;
begin
  Result := (Lang = ActiveLanguage);
end;

function IsNotLang(Lang: String): Boolean;
begin
  Result := (Lang <> ActiveLanguage);
end;

function IsTrue(Check: String): Boolean;
var
  Variable: Integer;
  Value: Integer;
  CheckInt: Integer;
begin
  CheckInt := StrToInt(Check);
  if CheckInt div 10 = 1 then Variable := UserInterface
    else Variable := AdvancedTabs;
  Value := CheckInt mod 10;
  Result := (Variable = Value);
end;

function InitializeSetup(): Boolean;
begin
  { read settings of current installation (if any) }
  UserInterface := 0; { default is commander }
  RegQueryDWordValue(HKCU, '{#RegistryKey}\Configuration\Interface',
    'Interface', UserInterface);
  AdvancedTabs := 0; { advanced tabs are off by default }
  RegQueryDWordValue(HKCU, '{#RegistryKey}\Configuration\Interface',
    'ShowAdvancedLoginOptions', AdvancedTabs);

  InitRuntimeMessages();

  { let setup run }
  Result := True;
end;

function ScriptDlgPages(CurPage: Integer; BackClicked: Boolean): Boolean;
var
  CommanderRadioButton: TRadioButton;
  ExplorerRadioButton: TRadioButton;
  AdvancedTabsCheckbox: TCheckbox;
  Caption, Caption2: TLabel;
  Next: Boolean;
begin
  if (not BackClicked and (CurPage = wpSelectTasks)) or
     (BackClicked and (CurPage = wpReady)) then
  begin
    ScriptDlgPageOpen();
    ScriptDlgPageSetCaption(RuntimeMessages[{#UserSettingsTitle}]);
    ScriptDlgPageSetSubCaption1(RuntimeMessages[{#UserSettingsPrompt}]);
    {ScriptDlgPageSetSubCaption2(
      'Note: All these options can be changed later.');}

    OutputMsg(RuntimeMessages[{#UserInterfaceStyle}], False);

    Caption := TLabel.Create(WizardForm.ScriptDlgPanel);
    Caption.Caption := 'User interface style';
    Caption.Width := WizardForm.ScriptDlgPanel.Width;
    Caption.Parent := WizardForm.ScriptDlgPanel;

    CommanderRadioButton := TRadioButton.Create(WizardForm.ScriptDlgPanel);
    CommanderRadioButton.Caption := RuntimeMessages[{#NortonCommanderInterface}];
    CommanderRadioButton.Checked := (UserInterface = 0);
    CommanderRadioButton.Width := WizardForm.ScriptDlgPanel.Width;
    CommanderRadioButton.Parent := WizardForm.ScriptDlgPanel;
    CommanderRadioButton.Top := Caption.Top + Caption.Height + 6;

    Caption2 := TLabel.Create(WizardForm.ScriptDlgPanel);
    Caption2.WordWrap := True;
    Caption2.Caption :=
        RuntimeMessages[{#NortonCommanderInterface1}]+#13#10+
        RuntimeMessages[{#NortonCommanderInterface2}]+#13#10+
        RuntimeMessages[{#NortonCommanderInterface3}]+#13#10+
        RuntimeMessages[{#NortonCommanderInterface4}];
    Caption2.Left := 20;
    Caption2.Width := WizardForm.ScriptDlgPanel.Width - Caption.Left;
    Caption2.Top := CommanderRadioButton.Top + CommanderRadioButton.Height + 6;
    Caption2.Parent := WizardForm.ScriptDlgPanel;

    ExplorerRadioButton := TRadioButton.Create(WizardForm.ScriptDlgPanel);
    ExplorerRadioButton.Caption := RuntimeMessages[{#ExplorerInterface}];
    ExplorerRadioButton.Checked := (UserInterface <> 0);
    ExplorerRadioButton.Width := WizardForm.ScriptDlgPanel.Width;
    ExplorerRadioButton.Parent := WizardForm.ScriptDlgPanel;
    ExplorerRadioButton.Top := Caption2.Top + Caption2.Height + 6;

    Caption2 := TLabel.Create(WizardForm.ScriptDlgPanel);
    Caption.WordWrap := True;
    Caption2.Caption :=
        RuntimeMessages[{#ExplorerInterface1}]+#13#10+
        RuntimeMessages[{#ExplorerInterface2}]+#13#10+
        RuntimeMessages[{#ExplorerInterface3}];
    Caption2.Left := 20;
    Caption2.Width := WizardForm.ScriptDlgPanel.Width - Caption.Left;
    Caption2.Top := ExplorerRadioButton.Top + ExplorerRadioButton.Height + 6;
    Caption2.Parent := WizardForm.ScriptDlgPanel;

    Caption := TLabel.Create(WizardForm.ScriptDlgPanel);
    Caption.Caption := RuntimeMessages[{#AdditionalOptions}];
    Caption.Width := WizardForm.ScriptDlgPanel.Width;
    Caption.Parent := WizardForm.ScriptDlgPanel;
    Caption.Top := Caption2.Top + Caption2.Height + 10;

    AdvancedTabsCheckbox := TCheckbox.Create(WizardForm.ScriptDlgPanel);
    AdvancedTabsCheckbox.Caption := RuntimeMessages[{#AdvancedLoginOptions}];
    AdvancedTabsCheckbox.Checked := (AdvancedTabs <> 0);
    AdvancedTabsCheckbox.Width := WizardForm.ScriptDlgPanel.Width;
    AdvancedTabsCheckbox.Parent := WizardForm.ScriptDlgPanel;
    AdvancedTabsCheckbox.Top := Caption.Top + Caption.Height + 6;

    Next := ScriptDlgPageProcessCustom();

    if CommanderRadioButton.Checked then UserInterface := 0
        else UserInterface := 1;

    if AdvancedTabsCheckbox.Checked then AdvancedTabs := 1
        else AdvancedTabs := 0;

    if not BackClicked then
      Result := Next
    else
      Result := not Next;

    ScriptDlgPageClose(not Result);
  end
    else
  begin
    Result := True;
  end;
end;

function NextButtonClick(CurPage: Integer): Boolean;
begin
  Result := ScriptDlgPages(CurPage, False);
end;

function BackButtonClick(CurPage: Integer): Boolean;
begin
  Result := ScriptDlgPages(CurPage, True);
end;

function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
  S: String;
  S2: String;
begin
  S := '';

  S := S + MemoDirInfo + NewLine + NewLine;

  S := S + MemoTypeInfo + NewLine + NewLine;

  S := S + MemoComponentsInfo + NewLine + NewLine;

  if Length(MemoGroupInfo) > 0 then
    S := S + MemoGroupInfo + NewLine + NewLine;

  if Length(MemoTasksInfo) > 0 then
    S := S + MemoTasksInfo + NewLine + NewLine;

  S := S + RuntimeMessages[{#UserSettingsOverview}] + NewLine;
  S := S + Space;
  if UserInterface = 0 then S2 := RuntimeMessages[{#NortonCommanderInterface}]
    else S2 := RuntimeMessages[{#ExplorerInterface}];
  StringChange(S2, '&', '');
  S := S + S2;
  S := S + NewLine;

  if AdvancedTabs <> 0 then
  begin
    S2 := RuntimeMessages[{#AdvancedLoginOptions}];
    StringChange(S2, '&', '');
    S := S + Space + S2 + NewLine;
  end;

  Result := S;
end;
