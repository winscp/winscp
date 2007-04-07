#define AppId "winscp3"
#define ParentRegistryKey "Software\Martin Prikryl"
#define RegistryKey ParentRegistryKey+"\WinSCP 2"
#define PuttySourceDir "c:\Program Files\PuTTY"
#define DefaultLang "en"
#define WebRoot "http://winscp.net/"
#define WebForum WebRoot+"forum/"
#define WebDocumentation WebRoot+"eng/docs/"
#define WebPuTTY "http://www.chiark.greenend.org.uk/~sgtatham/putty/"
#define Year 2007
#define EnglishLang "English"
#define SetupTypeData "SetupType"
#define InnoSetupReg "Software\Microsoft\Windows\CurrentVersion\Uninstall\" + AppId + "_is1"
#define InnoSetupAppPathReg "Inno Setup: App Path"

#ifexist "interm\winscpsetup.inc.iss"
  #include "interm\winscpsetup.inc.iss"
#else
  #define Status "unofficial"
  #define SourceDir ".."
  #define TranslationDirRel "translations"
  #define TranslationDir "translations"
  #define OutputDir ""
#endif

#define TranslationMask TranslationDir + "\WinSCP.???"
#define MainFileSource SourceDir+"\WinSCP.exe"
#define ShellExtFileSource SourceDir+"\DragExt.dll"
#define ConsoleFileSource SourceDir+"\WinSCP.com"

#define Major
#define Minor
#define Rev
#define Build
#expr ParseVersion(MainFileSource, Major, Minor, Rev, Build)
#define Version Str(Major)+"."+Str(Minor)+(Rev > 0 ? "."+Str(Rev) : "")+(Status != "" ? " "+Status : "")

#define SetupExt

#ifdef INTL
  #expr SetupExt="intl"
#endif

; Translation support functions/variables

[Setup]
AppId={#AppId}
AppName=WinSCP
AppMutex=WinSCP
AppPublisher=Martin Prikryl
AppPublisherURL={#WebRoot}
AppSupportURL={#WebForum}
AppUpdatesURL={#WebRoot}eng/download.php
VersionInfoCompany=Martin Prikryl
VersionInfoDescription=Setup for WinSCP {#Version} (Freeware SCP/SFTP client for Windows)
VersionInfoVersion={#Major}.{#Minor}.{#Rev}.{#Build}
VersionInfoTextVersion={#Version}
VersionInfoCopyright=(c) 2000-{#Year} Martin Prikryl
DefaultDirName={pf}\WinSCP
DefaultGroupName=WinSCP
AllowNoIcons=yes
LicenseFile=licence.setup
UninstallDisplayIcon={app}\WinSCP.exe
OutputDir={#OutputDir}
DisableStartupPrompt=yes
AppVersion={#Version}
AppVerName=WinSCP {#Version}
OutputBaseFilename=winscp{#Major}{#Minor}{#Rev}setup{#SetupExt}
SolidCompression=yes
ShowTasksTreeLines=yes
PrivilegesRequired=none

; Some features of ISCC requires path relative to script,
; some path relative to CWD
#define MessagesPathRel(L) TranslationDirRel + "\" + "WinSCP." + L + ".isl"
#define MessagesPath(L) TranslationDir + "\" + "WinSCP." + L + ".isl"

[Languages]
Name: {#DefaultLang}; MessagesFile: {#MessagesPathRel(DefaultLang)}

#ifdef INTL

  #define FindHandle
  #dim Languages[100]
  #define LanguageCount 0
  #define LangI

  #sub ProcessTranslationFile

    #define FileName FindGetFileName(FindHandle)
    #define Lang Copy(FileName, Pos(".", FileName)+1)

    #define LangName ReadIni(MessagesPath(Lang), "LangOptions", "LanguageName")
    #define LangID ReadIni(MessagesPath(Lang), "LangOptions", "LanguageID")

    #expr Languages[LanguageCount*3] = Lang
    #expr Languages[LanguageCount*3+1] = LangName
    #expr Languages[LanguageCount*3+2] = LangID
    #expr LanguageCount++

[Languages]
Name: {#Lang}; MessagesFile: {#MessagesPathRel(Lang)}

  #endsub /* sub ProcessTranslationFile */

  #if FindHandle = FindFirst(TranslationMask, 0)
    #define FResult 1
    #for {0; FResult; FResult = FindNext(FindHandle)} ProcessTranslationFile
    #expr FindClose(FindHandle)
  #endif

#endif /* ifdef INTL */

; Types are not used anymore, they are preserved only to let setup
; detect previous installation type and decide between typical/custom setup
[Types]
Name: full; Description: "full"
Name: compact; Description: "compact"
Name: custom; Description: "custom"; Flags: iscustom

[Components]
Name: main; Description: {cm:ApplicationComponent}; \
  Types: full custom compact; Flags: fixed
Name: shellext; Description: {cm:ShellExtComponent}; \
  Types: full compact
Name: pageant; Description: {cm:PageantComponent}; \
  Types: full
Name: puttygen; Description: {cm:PuTTYgenComponent}; \
  Types: full
#ifdef INTL
Name: transl; Description: {cm:TranslationsComponent}; \
  Types: full
#endif

[Tasks]
Name: enableupdates; Description: {cm:EnableUpdates}
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

[Icons]
; This is created always (unless user checks Don't create a Start menu folder,
; Setup\AllowNoIcons=yes)
Name: "{group}\WinSCP"; Filename: "{app}\WinSCP.exe"; Components: main; \
  Comment: "{cm:ProgramComment}"
Name: "{group}\{cm:WebSite}"; Filename: "{#WebRoot}"; Components: main
Name: "{group}\{cm:SupportForum}"; \
  Filename: "{#WebForum}"; Components: main
Name: "{group}\{cm:DocumentationPage}"; \
  Filename: "{#WebDocumentation}"; Components: main
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
Name: "{group}\{cm:RSAKeyTools}\{cm:PuttyWebSite}"; \
  Filename: "{#WebPuTTY}"; Components: pageant puttygen
; This is created when desktopicon task is selected
Name: "{userdesktop}\WinSCP"; Filename: "{app}\WinSCP.exe"; \
  Tasks: desktopicon\user
Name: "{commondesktop}\WinSCP"; Filename: "{app}\WinSCP.exe"; \
  Tasks: desktopicon\common
; This is created when quicklaunchicon task is selected
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\WinSCP"; \
  Filename: "{app}\WinSCP.exe"; Tasks: quicklaunchicon
; This is created when sendtohook task is selected
Name: "{sendto}\{cm:SendToHookNew}"; Filename: "{app}\WinSCP.exe"; \
  Parameters: "/upload"; Tasks: sendtohook

[InstallDelete]
Type: files; Name: "{sendto}\WinSCP3 (upload using SCP).lnk"
Type: files; Name: "{group}\{cm:RSAKeyTools}\{cm:KeysManual}.lnk"
Type: files; Name: "{app}\{cm:SupportForum}.url"
Type: files; Name: "{app}\{cm:DocumentationPage}.url"
Type: files; Name: "{app}\WinSCP.url"
Type: files; Name: "{app}\PuTTY\PuTTY.url"
; Remove links to winscp3
Type: files; Name: "{app}\WinSCP3.exe"
Type: files; Name: "{app}\WinSCP3.com"
Type: files; Name: "{group}\WinSCP3.lnk"
Type: files; Name: "{userdesktop}\WinSCP3.lnk"
Type: files; Name: "{commondesktop}\WinSCP3.lnk"
Type: files; Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\WinSCP3.lnk"
Type: files; Name: "{sendto}\{cm:SendToHook}.lnk"

[Run]
Filename: "{app}\WinSCP.exe"; Description: "{cm:Launch}"; \
  Flags: nowait postinstall skipifsilent
; This is called when urlhandler task is selected
Filename: "{app}\WinSCP.exe"; Parameters: "/RegisterAsUrlHandler"; \
  StatusMsg: {cm:RegisteringAsUrlHandler}; Tasks: urlhandler
Filename: "{app}\WinSCP.exe"; Parameters: "/AddSearchPath"; \
  StatusMsg: {cm:AddingSearchPath}; Tasks: searchpath
Filename: "{app}\WinSCP.exe"; Parameters: "/InvalidDefaultTranslation"; \
  StatusMsg: {cm:RemovingInvalidDefaultTranslation}

[UninstallDelete]
; These additional files are created by application
Type: files; Name: "{app}\WinSCP.ini"
Type: files; Name: "{app}\WinSCP.cgl"
; WinSCP3 may remain from previous version, note that we do not delete it on
; upgrade, only duplicate into WinSCP.ini, see [Files]
Type: files; Name: "{app}\WinSCP3.ini"

[Files]
Source: "{#MainFileSource}"; DestDir: "{app}"; \
  Components: main; Flags: ignoreversion
; If WinSCP3.ini already exists on target system, copy it into WinSCP.ini
; (if WinSCP.ini does not exist yet)
Source: "{app}\WinSCP3.ini"; DestName: "WinSCP.ini"; DestDir: "{app}"; \
  Components: main; Flags: ignoreversion external skipifsourcedoesntexist onlyifdoesntexist
Source: "{#ConsoleFileSource}"; DestDir: "{app}"; \
  Components: main; Flags: ignoreversion
Source: "licence"; DestDir: "{app}"; \
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
; Updates
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface\Updates"; \
  ValueType: dword; ValueName: "Period"; ValueData: 7; \
  Tasks: enableupdates; Check: not UpdatesEnabled

#ifdef INTL

[Components]
Name: transl\eng; Description: {#EnglishLang}; Types: full custom compact; \
  Flags: fixed

  #sub EmitLang

[Components]
Name: transl\{#Languages[LangI*3]}; Description: {#Languages[LangI*3+1]}; \
  Types: full compact custom; Check: IsLang('{#Languages[LangI*3]}')
Name: transl\{#Languages[LangI*3]}; Description: {#Languages[LangI*3+1]}; \
  Check: not IsLang('{#Languages[LangI*3]}')

[Files]
Source: "{#TranslationDirRel}\WinSCP.{#Languages[LangI*3]}"; DestDir: "{app}"; \
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
Filename: "{app}\WinSCP.exe"; Parameters: "/UninstallCleanup"; \
  RunOnceId: "UninstallCleanup"
Filename: "{app}\WinSCP.exe"; Parameters: "/RemoveSearchPath"; \
  RunOnceId: "RemoveSearchPath"

[Code]
const
  wpSetupType = 100;
  wpInterface = 101;

var
  TypicalTypeButton: TRadioButton;
  CustomTypeButton: TRadioButton;
  CommanderRadioButton: TRadioButton;
  ExplorerRadioButton: TRadioButton;
  AdditionalOptionsCaption: TLabel;
  AdvancedTabsCheckbox: TCheckbox;
  AreUpdatesEnabled: Boolean;
  Upgrade: Boolean;

function IsLang(Lang: String): Boolean;
begin
  Result := (Lang = ActiveLanguage);
end;

function UpdatesEnabled: Boolean;
begin
  Result := AreUpdatesEnabled;
end;

function UserSettings(Settings: Integer): Boolean;
begin
  case Settings of
    1: Result := CommanderRadioButton.Checked;
    2: Result := AdvancedTabsCheckbox.Checked;
    else Result := False;
  end;
end;

#ifdef INTL
function LanguageName(Lang: String): String;
begin
  #sub EmitLang2
  if Lang = '{#Languages[LangI*3]}' then Result := '{#Languages[LangI*3+1]}'
    else
  #endsub /* sub EmitLang2 */

  #for {LangI = 0; LangI < LanguageCount; LangI++} EmitLang2

  Result := 'unknown';
end;
#endif /* ifdef INTL */

procedure OpenHelp;
var
  ErrorCode: Integer;
begin
  ShellExec('open', '{#WebDocumentation}installation', '', '',
    SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;

procedure HelpButtonClick(Sender: TObject);
begin
  OpenHelp;
end;

procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 112 { VK_F1 } then
  begin
    OpenHelp;
    Key := 0;
  end;
end;

procedure CaptionClick(Sender: TObject);
begin
  WizardForm.ActiveControl := TLabel(Sender).FocusControl;
end;

procedure InitializeWizard;
var
  UserInterface: Cardinal;
  AdvancedTabs: Cardinal;
  UpdatesPeriod: Cardinal;
  InterfacePage: TWizardPage;
  SetupTypePage: TWizardPage;
  Caption: TLabel;
  HelpButton: TButton;
  S: String;
begin
  Upgrade :=
    RegQueryStringValue(HKLM, '{#InnoSetupReg}', '{#InnoSetupAppPathReg}', S) or
    RegQueryStringValue(HKCU, '{#InnoSetupReg}', '{#InnoSetupAppPathReg}', S)

  WizardForm.KeyPreview := True;
  WizardForm.OnKeyDown := @FormKeyDown;

  // allow installation without requiring user to accept licence
  WizardForm.LicenseAcceptedRadio.Checked := True;
  WizardForm.LicenseAcceptedRadio.Visible := False;
  WizardForm.LicenseLabel1.Visible := False;
  WizardForm.LicenseNotAcceptedRadio.Visible := False;
  WizardForm.LicenseMemo.Top := WizardForm.LicenseLabel1.Top;
  WizardForm.LicenseMemo.Height :=
    WizardForm.LicenseNotAcceptedRadio.Top +
    WizardForm.LicenseNotAcceptedRadio.Height -
    WizardForm.LicenseMemo.Top - 5;

  // hide installation types combo
  WizardForm.TypesCombo.Visible := False;
  WizardForm.ComponentsList.Height :=
    WizardForm.ComponentsList.Top + WizardForm.ComponentsList.Height -
    WizardForm.TypesCombo.Top;
  WizardForm.ComponentsList.Top := WizardForm.TypesCombo.Top;

  // add help button
  HelpButton := TButton.Create(WizardForm);
  HelpButton.Parent := WizardForm;
  HelpButton.Left :=
    WizardForm.ClientWidth -
    (WizardForm.CancelButton.Left + WizardForm.CancelButton.Width);
  HelpButton.Top := WizardForm.CancelButton.Top;
  HelpButton.Width := WizardForm.CancelButton.Width;
  HelpButton.Height := WizardForm.CancelButton.Height;
  HelpButton.Caption := ExpandConstant('{cm:HelpButton}');
  HelpButton.OnClick := @HelpButtonClick;

  // installation type page

  SetupTypePage := CreateCustomPage(wpLicense,
    ExpandConstant('{cm:SetupTypeTitle}'),
    ExpandConstant('{cm:SetupTypePrompt}'));

  TypicalTypeButton := TRadioButton.Create(SetupTypePage);
  if not Upgrade then
    S := ExpandConstant('{cm:TypicalType}')
  else
    S := ExpandConstant('{cm:TypicalUpgradeType}');
  TypicalTypeButton.Caption :=
    FmtMessage(ExpandConstant('{cm:Recommended}'), [S]);
  // check typical install, if typical install was installed before or
  // when version without setup type support was installed with
  // "full" installation or when there were no installation before
  // ("full" installation is default)
  TypicalTypeButton.Checked :=
    ((GetPreviousData('{#SetupTypeData}', '') = 'typical')) or
    ((GetPreviousData('{#SetupTypeData}', '') = '') and
     (WizardSetupType(False) = 'full'));
  TypicalTypeButton.Left := ScaleX(4);
  TypicalTypeButton.Width := SetupTypePage.SurfaceWidth -
    TypicalTypeButton.Left;
  TypicalTypeButton.Parent := SetupTypePage.Surface;

  Caption := TLabel.Create(SetupTypePage);
  Caption.WordWrap := True;
  if not Upgrade then
  begin
#ifdef INTL
    if ActiveLanguage = '{#DefaultLang}' then
      S := ExpandConstant('{cm:TypicalType2Eng}')
    else
      S := FmtMessage(ExpandConstant('{cm:TypicalType2Intl}'), [LanguageName(ActiveLanguage)]);
#else
    S := ExpandConstant('{cm:TypicalType2}');
#endif
    Caption.Caption :=
        ExpandConstant('{cm:TypicalType1}') + #13#10 +
        S + #13#10 +
        ExpandConstant('{cm:TypicalType3}');
  end
    else
  begin
    Caption.Caption :=
      ExpandConstant('{cm:TypicalUpgradeType1}');
  end;
  Caption.Left := ScaleX(4) + ScaleX(20);
  Caption.Width := SetupTypePage.SurfaceWidth - Caption.Left;
  Caption.Top := TypicalTypeButton.Top + TypicalTypeButton.Height + ScaleY(6);
  Caption.Parent := SetupTypePage.Surface;
  Caption.FocusControl := TypicalTypeButton;
  Caption.OnClick := @CaptionClick;

  CustomTypeButton := TRadioButton.Create(SetupTypePage);
  if not Upgrade then
    CustomTypeButton.Caption := ExpandConstant('{cm:CustomType}')
  else
    CustomTypeButton.Caption := ExpandConstant('{cm:CustomUpgradeType}');
  CustomTypeButton.Checked := (not TypicalTypeButton.Checked);
  CustomTypeButton.Left := ScaleX(4);
  CustomTypeButton.Width := SetupTypePage.SurfaceWidth -
    CustomTypeButton.Left;
  CustomTypeButton.Top := Caption.Top + Caption.Height + ScaleY(10);
  CustomTypeButton.Parent := SetupTypePage.Surface;

  Caption := TLabel.Create(SetupTypePage);
  Caption.WordWrap := True;
  if not Upgrade then
  begin
    Caption.Caption :=
      ExpandConstant('{cm:CustomType1}');
  end
    else
  begin
    Caption.Caption :=
      ExpandConstant('{cm:CustomUpgradeType1}') + #13#10 +
      ExpandConstant('{cm:CustomUpgradeType2}');
  end;
  Caption.Left := ScaleX(4) + ScaleX(20);
  Caption.Width := SetupTypePage.SurfaceWidth - Caption.Left;
  Caption.Top := CustomTypeButton.Top + CustomTypeButton.Height +
    ScaleY(6);
  Caption.Parent := SetupTypePage.Surface;
  Caption.FocusControl := CustomTypeButton;
  Caption.OnClick := @CaptionClick;

  // interface page

  InterfacePage := CreateCustomPage(wpSelectTasks,
    ExpandConstant('{cm:UserSettingsTitle}'),
    ExpandConstant('{cm:UserSettingsPrompt}'));

  UpdatesPeriod := 0;
  RegQueryDWordValue(HKCU, '{#RegistryKey}\Configuration\Interface\Updates',
    'Period', UpdatesPeriod);
  AreUpdatesEnabled := (UpdatesPeriod <> 0);

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
  Caption.FocusControl := CommanderRadioButton;
  Caption.OnClick := @CaptionClick;

  ExplorerRadioButton := TRadioButton.Create(InterfacePage);
  ExplorerRadioButton.Caption := ExpandConstant('{cm:ExplorerInterface}');
  ExplorerRadioButton.Checked := (UserInterface <> 0);
  ExplorerRadioButton.Left := ScaleX(4);
  ExplorerRadioButton.Width := InterfacePage.SurfaceWidth -
    ExplorerRadioButton.Left;
  ExplorerRadioButton.Top := Caption.Top + Caption.Height + ScaleY(10);
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
  Caption.FocusControl := ExplorerRadioButton;
  Caption.OnClick := @CaptionClick;

  AdditionalOptionsCaption := TLabel.Create(InterfacePage);
  AdditionalOptionsCaption.Caption := ExpandConstant('{cm:AdditionalOptions}');
  AdditionalOptionsCaption.Width := InterfacePage.SurfaceWidth;
  AdditionalOptionsCaption.Top := Caption.Top + Caption.Height + ScaleY(10);
  AdditionalOptionsCaption.Parent := InterfacePage.Surface;

  AdvancedTabsCheckbox := TCheckbox.Create(InterfacePage);
  AdvancedTabsCheckbox.Caption := ExpandConstant('{cm:AdvancedLoginOptions}');
  AdvancedTabsCheckbox.Checked := (AdvancedTabs <> 0);
  AdvancedTabsCheckbox.Left := ScaleX(4);
  AdvancedTabsCheckbox.Width := InterfacePage.SurfaceWidth -
    AdvancedTabsCheckbox.Left;
  AdvancedTabsCheckbox.Top :=
    AdditionalOptionsCaption.Top + AdditionalOptionsCaption.Height + ScaleY(6);
  AdvancedTabsCheckbox.Parent := InterfacePage.Surface;
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
var
  S: String;
begin
  if TypicalTypeButton.Checked then S := 'typical'
    else S := 'custom';

  SetPreviousData(PreviousDataKey, '{#SetupTypeData}', S);
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  if CurPageID = wpInterface then
  begin
    AdditionalOptionsCaption.Visible := not TypicalTypeButton.Checked;
    AdvancedTabsCheckbox.Visible := not TypicalTypeButton.Checked;
  end;
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  { Hide most pages during typical installation }
  Result :=
    TypicalTypeButton.Checked and
    ((PageID = wpSelectDir) or (PageID = wpSelectComponents) or
     (PageID = wpSelectProgramGroup) or (PageID = wpSelectTasks) or
     { Hide Interface page for upgrades only, show for fresch installs }
     ((PageID = wpInterface) and Upgrade));
end;

function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo,
  MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
  S: String;
  S2: String;
begin
  S := '';

  S := S + MemoDirInfo + NewLine + NewLine;

  if not Upgrade then
  begin
    if TypicalTypeButton.Checked then S2 := ExpandConstant('{cm:TypicalType}')
      else S2 := ExpandConstant('{cm:CustomType}');
  end
    else
  begin
    if TypicalTypeButton.Checked then S2 := ExpandConstant('{cm:TypicalUpgradeType}')
      else S2 := ExpandConstant('{cm:CustomUpgradeType}');
  end;
  StringChange(S2, '&', '');
  S := S + SetupMessage(msgReadyMemoType) + NewLine + Space + S2 + NewLine + NewLine;

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

function InitializeUninstall: Boolean;
begin
  // let application know that we are running silent uninstall,
  // this turns UninstallCleanup to noop
  if UninstallSilent then
    CreateMutex('WinSCPSilentUninstall');
  Result := True;
end;
