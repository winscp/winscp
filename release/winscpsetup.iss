#define MainFileSource "..\WinSCP3.exe"
#define ShellExtFileSource "..\DragExt.dll"
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
VersionInfoCompany=Martin Prikryl
VersionInfoDescription=Setup for WinSCP {#Version} (Freeware SCP/SFTP client for Windows)
VersionInfoVersion={#Major}.{#Minor}.{#Rev}.{#Build}
VersionInfoTextVersion={#Version}
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
SolidCompression=yes
ShowTasksTreeLines=yes

#define FindHandle
#dim Languages[100]
#define LanguageCount 0
#define LangI
#define MessagesPath

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

[Languages]
Name: {#Lang}; MessagesFile: {#MessagesPath}

#endsub /* sub ProcessTranslationFile */

#define FindHandle 0
#expr ProcessTranslationFile

#ifdef INTL

  #if FindHandle = FindFirst(TranslationMask, 0)
    #define FResult 1
    #for {0; FResult; FResult = FindNext(FindHandle)} ProcessTranslationFile
    #expr FindClose(FindHandle)
  #endif

#endif /* ifdef INTL */

[Types]
Name: full; Description: {cm:FullInstallation}
#ifdef INTL
Name: fulllangs; Description: {cm:FullInstallationLangs}
#endif
Name: compact; Description: {cm:CompactInstallation}
Name: custom; Description: {cm:CustomInstallation}; Flags: iscustom

[Components]
Name: main; Description: {cm:ApplicationComponent}; \
  Types: {#FullLangs} full custom compact; Flags: fixed
Name: shellext; Description: {cm:ShellExtComponent}; \
  Types: {#FullLangs} compact full
Name: pageant; Description: {cm:PageantComponent}; \
  Types: {#FullLangs} full
Name: puttygen; Description: {cm:PuTTYgenComponent}; \
  Types: {#FullLangs} full
#ifdef INTL
Name: transl; Description: {cm:TranslationsComponent}; \
  Types: full fulllangs
#endif

[Tasks]
; Windows integration
Name: desktopicon; Description: {cm:DesktopIconTask}
Name: desktopicon\user; Description: {cm:DesktopIconUserTask}; \
  Flags: exclusive
Name: desktopicon\common; Description: {cm:DesktopIconCommonTask}; \
  Flags: exclusive unchecked
Name: quicklaunchicon; Description: {cm:QuickLaunchIconTask}; \
  Flags: unchecked
Name: sendtohook; Description: {cm:SendToHookTask}
Name: urlhandler; Description: {cm:RegisterAsUrlHandler}

[INI]
Filename: "{app}\{cm:SupportForum}.url"; Section: "InternetShortcut"; \
  Key: "URL"; String: "http://winscp.sourceforge.net/forum/"
Filename: "{app}\WinSCP.url"; Section: "InternetShortcut"; \
  Key: "URL"; String: "http://winscp.sourceforge.net/"
Filename: "{app}\PuTTY\PuTTY.url"; Section: "InternetShortcut"; \
  Key: "URL"; String: "http://www.chiark.greenend.org.uk/~sgtatham/putty/"; \
  Components: pageant puttygen

[Icons]
; This is created always (unless user checks Don't create a Start menu folder, 
; Setup\AllowNoIcons=yes)
Name: "{group}\WinSCP"; Filename: "{app}\WinSCP3.exe"; Components: main
Name: "{group}\{cm:WebSite}"; Filename: "{app}\WinSCP.url"; Components: main
Name: "{group}\{cm:SupportForum}"; \
  Filename: "{app}\{cm:SupportForum}.url"; Components: main
; This is created when pageant/puttygen component is selected (unless user 
; checks Don't create a Start menu folder, Setup\AllowNoIcons=yes). Flag 
; createonlyiffileexists is used instead of "Components: xxx",
; because it would force creating the icons even when user doesn't want to 
; create start menu folder.
Name: "{group}\{cm:RSAKeyTools}\PuTTYgen"; \
  Filename: "{app}\PuTTY\puttygen.exe"; Components: puttygen
Name: "{group}\{cm:RSAKeyTools}\{cm:PuTTYgenManual}"; \
  Filename: "winhlp32.exe"; Parameters: "-iputtygen.general {app}\PuTTY\putty.hlp"; \
  Components: puttygen
Name: "{group}\{cm:RSAKeyTools}\Pageant"; \
  Filename: "{app}\PuTTY\pageant.exe"; Components: pageant
Name: "{group}\{cm:RSAKeyTools}\{cm:PageantManual}"; \
  Filename: "winhlp32.exe"; Parameters: "-ipageant.general {app}\PuTTY\putty.hlp"; \
  Components: pageant
Name: "{group}\{cm:RSAKeyTools}\{cm:KeysManual}"; \
  Filename: "winhlp32.exe"; Parameters: "-it00000112 {app}\PuTTY\putty.hlp"; \
  Components: pageant puttygen
Name: "{group}\{cm:RSAKeyTools}\{cm:PuttyWebSite}"; \
  Filename: "{app}\PuTTY\PuTTY.url"; Components: pageant puttygen
; This is created when desktopicon task is selected
Name: "{userdesktop}\WinSCP3"; Filename: "{app}\WinSCP3.exe"; \
  Tasks: desktopicon\user
Name: "{commondesktop}\WinSCP3"; Filename: "{app}\WinSCP3.exe"; \
  Tasks: desktopicon\common
; This is created when quicklaunchicon task is selected
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\WinSCP3"; \
  Filename: "{app}\WinSCP3.exe"; Tasks: quicklaunchicon
; This is created when sendtohook task is selected
Name: "{sendto}\{cm:SendToHook}"; Filename: "{app}\WinSCP3.exe"; \
  Parameters: "/upload"; Tasks: sendtohook

[InstallDelete]
Type: files; Name: "{sendto}\WinSCP3 (upload using SCP).lnk"
Type: files; Name: "{sendto}\{cm:SendToHook}.lnk"

[Run]
Filename: "{app}\WinSCP3.exe"; Description: "{cm:Launch}"; \
  Flags: nowait postinstall skipifsilent
; This is called when urlhandler task is selected
Filename: "{app}\WinSCP3.exe"; Parameters: "/RegisterAsUrlHandler"; \
  Tasks: urlhandler

[UninstallDelete]
; These additional files are created by installer
Type: files; Name: "{app}\WinSCP.url"
Type: files; Name: "{app}\{cm:SupportForum}.url"
Type: files; Name: "{app}\PuTTY\PuTTY.url"; Components: pageant puttygen
; These additional files are created by application
Type: files; Name: "{app}\WinSCP3.ini"

[Files]
Source: "{#MainFileSource}"; DestDir: "{app}"; \
  Components: main; Flags: ignoreversion
Source: "licence"; DestName: "licence"; DestDir: "{app}"; \
  Components: main; Flags: ignoreversion
Source: "{#ShellExtFileSource}"; DestDir: "{app}"; \
  Components: shellext; \
  Flags: ignoreversion regserver restartreplace restartreplace uninsrestartdelete
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
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; \
  ValueName: "Interface"; ValueData: 0; Check: IsTrue(10)
; Explorer-like interface
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; \
  ValueName: "Interface"; ValueData: 1; Check: IsTrue(11)
; Advanced tab on login dialog
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; \
  ValueName: "ShowAdvancedLoginOptions"; ValueData: 0; Check: IsTrue(20)
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; \
  ValueName: "ShowAdvancedLoginOptions"; ValueData: 1; Check: IsTrue(21)
; This will remove url handler on uninstall 
; (when urlhandler task was selected when installing)
Root: HKCR; Subkey: "SFTP"; Flags: dontcreatekey uninsdeletekey; \
  Tasks: urlhandler
Root: HKCR; Subkey: "SCP"; Flags: dontcreatekey uninsdeletekey; \
  Tasks: urlhandler
Root: HKCU; Subkey: "Software\Classes\SFTP"; Flags: dontcreatekey uninsdeletekey; \
  Tasks: urlhandler
Root: HKCU; Subkey: "Software\Classes\SCP"; Flags: dontcreatekey uninsdeletekey; \
  Tasks: urlhandler

#ifdef INTL

[Components]
Name: transl\eng; Description: "English"; Types: fulllangs full custom compact; \
  Flags: fixed

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

  #endsub /* sub EmitLang */

  #for {LangI = 0; LangI < LanguageCount; LangI++} EmitLang

#endif /* ifdef INTL */

[UninstallRun]
Filename: "{app}\WinSCP3.exe"; Parameters: "/UninstallCleanup"; \
  RunOnceId: "UninstallCleanup"

[Code]
var
  UserInterface: Cardinal;
  AdvancedTabs: Cardinal;

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
    
    ScriptDlgPageSetCaption(ExpandConstant('{cm:UserSettingsPrompt}'));
    //ScriptDlgPageSetCaption(ExpandConstant('{cm:UserSettingsTitle}'));
    ScriptDlgPageSetSubCaption1(ExpandConstant('{cm:UserSettingsPrompt}'));

    OutputMsg(ExpandConstant('{cm:UserInterfaceStyle}'), False);
  
    Caption := TLabel.Create(WizardForm.ScriptDlgPanel);
    Caption.Caption := 'User interface style';
    Caption.Width := WizardForm.ScriptDlgPanel.Width;
    Caption.Parent := WizardForm.ScriptDlgPanel;

    CommanderRadioButton := TRadioButton.Create(WizardForm.ScriptDlgPanel);
    CommanderRadioButton.Caption := ExpandConstant('{cm:NortonCommanderInterface}');
    CommanderRadioButton.Checked := (UserInterface = 0);
    CommanderRadioButton.Width := WizardForm.ScriptDlgPanel.Width;
    CommanderRadioButton.Parent := WizardForm.ScriptDlgPanel;
    CommanderRadioButton.Top := Caption.Top + Caption.Height + 6;

    Caption2 := TLabel.Create(WizardForm.ScriptDlgPanel);
    Caption2.WordWrap := True;
    Caption2.Caption :=
        ExpandConstant('{cm:NortonCommanderInterface1}') + #13#10 +
        ExpandConstant('{cm:NortonCommanderInterface2}') + #13#10 +
        ExpandConstant('{cm:NortonCommanderInterface3}') + #13#10 +
        ExpandConstant('{cm:NortonCommanderInterface4}');
    Caption2.Left := 20;
    Caption2.Width := WizardForm.ScriptDlgPanel.Width - Caption.Left;
    Caption2.Top := CommanderRadioButton.Top + CommanderRadioButton.Height + 6;
    Caption2.Parent := WizardForm.ScriptDlgPanel;

    ExplorerRadioButton := TRadioButton.Create(WizardForm.ScriptDlgPanel);
    ExplorerRadioButton.Caption := ExpandConstant('{cm:ExplorerInterface}');
    ExplorerRadioButton.Checked := (UserInterface <> 0);
    ExplorerRadioButton.Width := WizardForm.ScriptDlgPanel.Width;
    ExplorerRadioButton.Parent := WizardForm.ScriptDlgPanel;
    ExplorerRadioButton.Top := Caption2.Top + Caption2.Height + 6;

    Caption2 := TLabel.Create(WizardForm.ScriptDlgPanel);
    Caption.WordWrap := True;
    Caption2.Caption :=
        ExpandConstant('{cm:ExplorerInterface1}') + #13#10 +
        ExpandConstant('{cm:ExplorerInterface2}') + #13#10 +
        ExpandConstant('{cm:ExplorerInterface3}');
    Caption2.Left := 20;
    Caption2.Width := WizardForm.ScriptDlgPanel.Width - Caption.Left;
    Caption2.Top := ExplorerRadioButton.Top + ExplorerRadioButton.Height + 6;
    Caption2.Parent := WizardForm.ScriptDlgPanel;
    
    Caption := TLabel.Create(WizardForm.ScriptDlgPanel);
    Caption.Caption := ExpandConstant('{cm:AdditionalOptions}');
    Caption.Width := WizardForm.ScriptDlgPanel.Width;
    Caption.Parent := WizardForm.ScriptDlgPanel;
    Caption.Top := Caption2.Top + Caption2.Height + 10;

    AdvancedTabsCheckbox := TCheckbox.Create(WizardForm.ScriptDlgPanel);
    AdvancedTabsCheckbox.Caption := ExpandConstant('{cm:AdvancedLoginOptions}');
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

  S := S + ExpandConstant('{cm:UserSettingsOverview}') + NewLine;
  S := S + Space;
  if UserInterface = 0 then S2 := ExpandConstant('{cm:NortonCommanderInterface}')
    else S2 := ExpandConstant('{cm:ExplorerInterface}');
  StringChange(S2, '&', '');
  S := S + S2;
  S := S + NewLine;

  if AdvancedTabs <> 0 then
  begin
    S2 := ExpandConstant('{cm:AdvancedLoginOptions}');
    StringChange(S2, '&', '');
    S := S + Space + S2 + NewLine;
  end;

  Result := S;
end;
