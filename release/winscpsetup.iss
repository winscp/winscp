#define MainFileSource "..\WinSCP3.exe"
#define ShellExtFileSource "..\DragExt.dll"
#define ConsoleFileSource "..\Console.com"
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
AppMutex=WinSCP
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
Name: searchpath; Description: {cm:AddSearchPath}; \
  Flags: unchecked

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
; checks Don't create a Start menu folder, Setup\AllowNoIcons=yes).
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
  Filename: "winhlp32.exe"; Parameters: "-it00000117 {app}\PuTTY\putty.hlp"; \
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
  StatusMsg: {cm:RegisteringAsUrlHandler}; Tasks: urlhandler
Filename: "{app}\WinSCP3.exe"; Parameters: "/AddSearchPath"; \
  StatusMsg: {cm:AddingSearchPath}; Tasks: searchpath
Filename: "{app}\WinSCP3.exe"; Parameters: "/InvalidDefaultTranslation"; \
  StatusMsg: {cm:RemovingInvalidDefaultTranslation}

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
Source: "{#ConsoleFileSource}"; DestName: "WinSCP3.com"; DestDir: "{app}"; \
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
  ValueName: "Interface"; ValueData: 0; Check: UserSettings(1)
; Explorer-like interface
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; \
  ValueName: "Interface"; ValueData: 1; Check: not UserSettings(1)
; Advanced tab on login dialog
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; \
  ValueName: "ShowAdvancedLoginOptions"; ValueData: 0; Check: not UserSettings(2)
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; \
  ValueName: "ShowAdvancedLoginOptions"; ValueData: 1; Check: UserSettings(2)
; If installer enabled ddext, let it reset the settings on uninstall,
; so the default is used on the next run
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; \
  ValueName: "DDExtEnabled"; ValueData: 1; Components: shellext; \
  Flags: uninsdeletevalue
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
  Types: fulllangs full compact custom; Check: IsLang('{#Languages[LangI*3]}')
Name: transl\{#Languages[LangI*3]}; Description: {#Languages[LangI*3+1]}; \
  Types: fulllangs; Check: not IsLang('{#Languages[LangI*3]}')

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
Filename: "{app}\WinSCP3.exe"; Parameters: "/RemoveSearchPath"; \
  RunOnceId: "RemoveSearchPath"

[Code]
var
  CommanderRadioButton: TRadioButton;
  ExplorerRadioButton: TRadioButton;
  AdvancedTabsCheckbox: TCheckbox;

function IsLang(Lang: String): Boolean;
begin
  Result := (Lang = ActiveLanguage);
end;

function UserSettings(Settings: Integer): Boolean;
begin
  case Settings of
    1: Result := CommanderRadioButton.Checked;
    2: Result := AdvancedTabsCheckbox.Checked;
    else Result := False;
  end;
end;

procedure InitializeWizard();
var
  UserInterface: Cardinal;
  AdvancedTabs: Cardinal;
  InterfacePage: TWizardPage;
  Caption, Caption2: TLabel;
begin
  InterfacePage := CreateCustomPage(wpSelectTasks,
    ExpandConstant('{cm:UserSettingsTitle}'),
    ExpandConstant('{cm:UserSettingsPrompt}'));

  UserInterface := 0; { default is commander }
  RegQueryDWordValue(HKCU, '{#RegistryKey}\Configuration\Interface',
    'Interface', UserInterface);
  AdvancedTabs := 0; { advanced tabs are off by default }
  RegQueryDWordValue(HKCU, '{#RegistryKey}\Configuration\Interface',
    'ShowAdvancedLoginOptions', AdvancedTabs);

  Caption := TLabel.Create(InterfacePage);
  Caption.Caption := ExpandConstant('{cm:UserInterfaceStyle}');
  Caption.Width := InterfacePage.SurfaceWidth;
  Caption.Parent := InterfacePage.Surface;

  CommanderRadioButton := TRadioButton.Create(InterfacePage);
  CommanderRadioButton.Caption := ExpandConstant('{cm:NortonCommanderInterface}');
  CommanderRadioButton.Checked := (UserInterface = 0);
  CommanderRadioButton.Left := ScaleX(4);
  CommanderRadioButton.Width := InterfacePage.SurfaceWidth -
    CommanderRadioButton.Left;
  CommanderRadioButton.Top := Caption.Top + Caption.Height + ScaleY(6);
  CommanderRadioButton.Parent := InterfacePage.Surface;

  Caption := TLabel.Create(InterfacePage);
  Caption.WordWrap := True;
  Caption.Caption :=
      ExpandConstant('{cm:NortonCommanderInterface1}') + #13#10 +
      ExpandConstant('{cm:NortonCommanderInterface2}') + #13#10 +
      ExpandConstant('{cm:NortonCommanderInterface3}');
  Caption.Left := ScaleX(4) + ScaleX(20);
  Caption.Width := InterfacePage.SurfaceWidth - Caption.Left;
  Caption.Top := CommanderRadioButton.Top + CommanderRadioButton.Height +
    ScaleY(6);
  Caption.Parent := InterfacePage.Surface;

  ExplorerRadioButton := TRadioButton.Create(InterfacePage);
  ExplorerRadioButton.Caption := ExpandConstant('{cm:ExplorerInterface}');
  ExplorerRadioButton.Checked := (UserInterface <> 0);
  ExplorerRadioButton.Left := ScaleX(4);
  ExplorerRadioButton.Width := InterfacePage.SurfaceWidth -
    ExplorerRadioButton.Left;
  ExplorerRadioButton.Top := Caption.Top + Caption.Height + ScaleY(6);
  ExplorerRadioButton.Parent := InterfacePage.Surface;

  Caption := TLabel.Create(InterfacePage);
  Caption.WordWrap := True;
  Caption.Caption :=
      ExpandConstant('{cm:ExplorerInterface1}') + #13#10 +
      ExpandConstant('{cm:ExplorerInterface2}') + #13#10 +
      ExpandConstant('{cm:ExplorerInterface3}');
  Caption.Left := ScaleX(4) + ScaleX(20);
  Caption.Width := InterfacePage.SurfaceWidth - Caption.Left;
  Caption.Top := ExplorerRadioButton.Top + ExplorerRadioButton.Height +
    ScaleY(6);
  Caption.Parent := InterfacePage.Surface;

  Caption2 := TLabel.Create(InterfacePage);
  Caption2.Caption := ExpandConstant('{cm:AdditionalOptions}');
  Caption2.Width := InterfacePage.SurfaceWidth;
  Caption2.Top := Caption.Top + Caption.Height + ScaleY(10);
  Caption2.Parent := InterfacePage.Surface;

  AdvancedTabsCheckbox := TCheckbox.Create(InterfacePage);
  AdvancedTabsCheckbox.Caption := ExpandConstant('{cm:AdvancedLoginOptions}');
  AdvancedTabsCheckbox.Checked := (AdvancedTabs <> 0);
  AdvancedTabsCheckbox.Left := ScaleX(4);
  AdvancedTabsCheckbox.Width := InterfacePage.SurfaceWidth -
    AdvancedTabsCheckbox.Left;
  AdvancedTabsCheckbox.Top := Caption2.Top + Caption2.Height + ScaleY(6);
  AdvancedTabsCheckbox.Parent := InterfacePage.Surface;
end;

function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo,
  MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
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
  if CommanderRadioButton.Checked then S2 := ExpandConstant('{cm:NortonCommanderInterface}')
    else S2 := ExpandConstant('{cm:ExplorerInterface}');
  StringChange(S2, '&', '');
  S := S + S2;
  S := S + NewLine;

  if AdvancedTabsCheckbox.Checked then
  begin
    S2 := ExpandConstant('{cm:AdvancedLoginOptions}');
    StringChange(S2, '&', '');
    S := S + Space + S2 + NewLine;
  end;

  Result := S;
end;
