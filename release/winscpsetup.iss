#define AppId "winscp3"
#define ParentRegistryKey "Software\Martin Prikryl"
#define RegistryKey ParentRegistryKey+"\WinSCP 2"
#define PuttySourceDir "c:\Program Files\PuTTY"
#define DefaultLang "en"
#define WebRoot "http://winscp.net/"
#define WebForum WebRoot+"forum/"
#define WebDocumentation WebRoot+"eng/docs/"
#define WebPuTTY "http://www.chiark.greenend.org.uk/~sgtatham/putty/"
#define Year 2008
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

#define TranslationFileMask "WinSCP.???"
#define MainFileSource SourceDir+"\WinSCP.exe"
#define ShellExtFileSource SourceDir+"\DragExt.dll"
#define ShellExt64FileSource SourceDir+"\DragExt64.dll"
#define ConsoleFileSource SourceDir+"\WinSCP.com"

#define Major
#define Minor
#define Rev
#define Build
#expr ParseVersion(MainFileSource, Major, Minor, Rev, Build)
#define Version Str(Major)+"."+Str(Minor)+(Rev > 0 ? "."+Str(Rev) : "")+(Status != "" ? " "+Status : "")

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
VersionInfoDescription=Setup for WinSCP {#Version} (SFTP, FTP and SCP client)
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
OutputBaseFilename=winscp{#Major}{#Minor}{#Rev}setup
SolidCompression=yes
ShowTasksTreeLines=yes
PrivilegesRequired=none

; Some features of ISCC requires path relative to script,
; some path relative to CWD
#define MessagesPathRel(L) TranslationDirRel + "\" + "WinSCP." + L + ".isl"

[Languages]
Name: {#DefaultLang}; MessagesFile: {#MessagesPathRel(DefaultLang)}

#define FindHandle
#dim Languages[200]
#define LanguageCount 0
#define AnyLanguageComplete 0
#define LangI
#define Complete
#define DirName

#sub ProcessTranslationFile

  #define FileName FindGetFileName(FindHandle)
  #define Lang Copy(FileName, Pos(".", FileName)+1)
  #define MessagesPath DirName + "\" + "WinSCP." + Lang + ".isl"

  #define LangNameFull ReadIni(MessagesPath, "LangOptions", "LanguageName")
  #define Sep Pos(" - ", LangNameFull)
  #if Sep > 0
    #define LangName Copy(LangNameFull, 1, Sep - 1)
  #else
    #define LangName LangNameFull
  #endif
  #define LangID ReadIni(MessagesPath, "LangOptions", "LanguageID")

  #expr Languages[LanguageCount*4] = Lang
  #expr Languages[LanguageCount*4+1] = LangName
  #expr Languages[LanguageCount*4+2] = LangID
  #expr Languages[LanguageCount*4+3] = Complete
  #expr LanguageCount++

#if Complete == 1
Name: {#Lang}; MessagesFile: {#MessagesPathRel(Lang)}
  #expr AnyLanguageComplete = 1
#endif

#endsub /* sub ProcessTranslationFile */

#sub ProcessTranslationDir

  #if FindHandle = FindFirst(DirName + "\" + TranslationFileMask, 0)
    #define FResult 1
    #for {0; FResult; FResult = FindNext(FindHandle)} ProcessTranslationFile
    #expr FindClose(FindHandle)
  #endif

#endsub /* sub ProcessTranslationDir */

#expr Complete = 1
#expr DirName = TranslationDir
#emit ProcessTranslationDir

#ifdef TranslationIncompleteDir
  #expr Complete = 0
  #expr DirName = TranslationIncompleteDir
  #emit ProcessTranslationDir
#endif

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
#if AnyLanguageComplete == 1
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
; Legacy sendto hook (SCP-only), only english link is removed
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
  Flags: ignoreversion regserver restartreplace uninsrestartdelete; \
  Check: not IsWin64
Source: "{#ShellExt64FileSource}"; DestDir: "{app}"; \
  Components: shellext; \
  Flags: ignoreversion regserver restartreplace uninsrestartdelete; \
  Check: IsWin64
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

#if AnyLanguageComplete == 1

[Components]
Name: transl\eng; Description: {#EnglishLang}; Types: full custom compact; \
  Flags: fixed

#endif

#sub EmitLang

  #if Languages[LangI*4+3] == 1

[Components]
Name: transl\{#Languages[LangI*4]}; Description: {#Languages[LangI*4+1]}; \
  Types: full compact custom; Check: IsLang('{#Languages[LangI*4]}')
Name: transl\{#Languages[LangI*4]}; Description: {#Languages[LangI*4+1]}; \
  Check: not IsLang('{#Languages[LangI*4]}')

[Files]
Source: "{#TranslationDirRel}\WinSCP.{#Languages[LangI*4]}"; DestDir: "{app}"; \
  Components: transl\{#Languages[LangI*4]}; Flags: ignoreversion

[Registry]
; set program default language to setup language, but only if user installs it
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; \
  ValueType: dword; ValueName: "LocaleSafe"; ValueData: {#Languages[LangI*4+2]}; \
  Components: transl\{#Languages[LangI*4]}; Languages: {#Languages[LangI*4]}

  #endif

#endsub /* sub EmitLang */

#for {LangI = 0; LangI < LanguageCount; LangI++} EmitLang

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
  MissingTranslations: string;

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

function LanguageName(Lang: String; Unknown: String): String;
begin
  #sub EmitLang2
  if Lang = '{#Languages[LangI*4]}' then Result := '{#Languages[LangI*4+1]}'
    else
  #endsub /* sub EmitLang2 */

  #for {LangI = 0; LangI < LanguageCount; LangI++} EmitLang2

  Result := Unknown;
end;

function ContainsLanguage(Lang: String): Boolean;
begin
  #sub EmitLang3
    #if Languages[LangI*4+3] == 1
  if (Lang = '{#Languages[LangI*4]}') then Result := True
    else
    #endif
  #endsub /* sub EmitLang3 */

  #for {LangI = 0; LangI < LanguageCount; LangI++} EmitLang3

  Result := False;
end;

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

type
  TProcessTranslationEvent = procedure(Lang: string; FileName: string);

procedure CollectNames(Lang: string; FileName: string);
begin
  if Length(MissingTranslations) > 0 then
    MissingTranslations := MissingTranslations + ', ';
  MissingTranslations := MissingTranslations + LanguageName(Lang, Lang);
end;

procedure DeleteTranslation(Lang: string; FileName: string);
begin
  DeleteFile(FileName);
end;

procedure ProcessMissingTranslations(OnProcessTranslation: TProcessTranslationEvent);
var
  Path: string;
  FindRec: TFindRec;
  Ext: string;
  VersionMS, VersionLS: Cardinal;
begin
  Path := AddBackslash(WizardDirValue);

  if FindFirst(ExpandConstant(Path + '{#TranslationFileMask}'), FindRec) then
  begin
    try
      repeat
        if FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY = 0 then
        begin
          Ext := Uppercase(ExtractFileExt(FindRec.Name));
          // any binary (has version info), which is not EXE or DLL,
          // is suspected to be a language file
          if (Pos('.', Ext) = 1) and
             (Ext <> '.INI') and (Ext <> '.EXE') and (Ext <> '.COM') and
             (Ext <> '.DLL') and
             GetVersionNumbers(Path + FindRec.Name, VersionMS, VersionLS) then
          begin
            Ext := Lowercase(Copy(Ext, 2, Length(Ext) - 1));
            if not ContainsLanguage(Ext) then
              OnProcessTranslation(Ext, Path + FindRec.Name);
          end;
        end;
      until not FindNext(FindRec);
    finally
      FindClose(FindRec);
    end;
  end;
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

  ProcessMissingTranslations(@CollectNames);

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
    if ActiveLanguage = '{#DefaultLang}' then
      S := ExpandConstant('{cm:TypicalType2Eng}')
    else
      S := FmtMessage(ExpandConstant('{cm:TypicalType2Intl}'), [LanguageName(ActiveLanguage, 'Unknown')]);
    Caption.Caption :=
      ExpandConstant('{cm:TypicalType1}') + #13#10 +
      S + #13#10 +
      ExpandConstant('{cm:TypicalType3}');
  end
    else
  begin
    if Length(MissingTranslations) > 0 then
    begin
      #if AnyLanguageComplete
        S := FmtMessage(ExpandConstant('{cm:TypicalUpgradeTypeMissingTransl}'), [MissingTranslations]);
      #else
        S := ExpandConstant('{cm:TypicalUpgradeTypeNoTransl}');
      #endif
      S := #13#10 + S;
    end
      else S := '';

    Caption.Caption :=
      ExpandConstant('{cm:TypicalUpgradeType1}') + S;
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

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall then
  begin
    if Length(MissingTranslations) > 0 then
    begin
      WizardForm.StatusLabel.Caption :=
        ExpandConstant('{cm:RemovingObsoleteTranslations}');
      ProcessMissingTranslations(@DeleteTranslation);
    end;
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
