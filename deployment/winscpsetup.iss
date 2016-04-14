#define AppId "winscp3"
#define AppMutex "WinSCP"
#define ParentRegistryKey "Software\Martin Prikryl"
#define RegistryKey ParentRegistryKey+"\WinSCP 2"
#define DefaultLang "en"
#define WebRoot "https://winscp.net/"
#define WebForum WebRoot+"forum/"
#define WebDocumentation WebRoot+"eng/docs/"
#define WebReport "http://winscp.net/install.php"
#define Year 2016
#define EnglishLang "English"
#define SetupTypeData "SetupType"
#define InnoSetupReg "Software\Microsoft\Windows\CurrentVersion\Uninstall\" + AppId + "_is1"
#define InnoSetupAppPathReg "Inno Setup: App Path"

#ifndef CompletenessThreshold
  #define CompletenessThreshold 100
#else
  #define CompletenessThreshold Int(CompletenessThreshold)
#endif

#ifndef PuttySourceDir
  #define PuttySourceDir "c:\Program Files\PuTTY"
#endif
#ifndef Status
  #define Status "unofficial"
#endif
#ifndef SourceDir
  #define SourceDir "..\source"
#endif
#ifndef BinariesDir
  #define BinariesDir SourceDir + "\Win32\Release"
#endif
#ifndef BinariesDir64
  #define BinariesDir64 SourceDir + "\Win64\Release"
#endif
#ifndef BinariesDirAssembly
  #define BinariesDirAssembly "..\dotnet\Win32\Release"
#endif
#ifndef AllTranslations
  #define AllTranslations
#endif

#define TranslationDir "translations"
#define ImagesDir "images"
#define OutputDir "."

#define TranslationFileMask "WinSCP.???"
#define MainFileName "WinSCP.exe"
#define MainFileSource BinariesDir+"\"+MainFileName
#define ShellExtFileName "DragExt.dll"
#define ShellExtFileSource BinariesDir+"\"+ShellExtFileName
#define ShellExt64FileName "DragExt64.dll"
#define ShellExt64FileSource BinariesDir64+"\"+ShellExt64FileName
#define ConsoleFileSource BinariesDir+"\WinSCP.com"
#define MapFileSource BinariesDir+"\WinSCP.map"
#define AssemblyFileSource BinariesDirAssembly+"\WinSCPnet.dll"

#ifdef Donations
#define PayPalCardImage "PayPalCard.bmp"
#endif

#define Major
#define Minor
#define Rev
#define Build
#expr ParseVersion(MainFileSource, Major, Minor, Rev, Build)
#define VersionOnly Str(Major)+"."+Str(Minor)+(Rev > 0 ? "."+Str(Rev) : "")
#define Version VersionOnly+(Status != "" ? " "+Status : "")
#define FTag VersionOnly+(Status != "" ? "."+Status : "")

#define WebArguments "ver=" +VersionOnly + "&lang={language}&utm_source=winscp&utm_medium=setup&utm_campaign=" + VersionOnly
#define WebGettingStarted WebRoot + "eng/installed.php?" + WebArguments + "&prevver="

#define MessagesPath(L) TranslationDir + "\" + "WinSCP." + L + ".islu"

#define ExplorerFileBase "Explorer"
#define CommanderFileBase "Commander"
#define WizardImageFileBase "Tall"
#define WizardSmallImageFileBase "Square"
#define SelectDirFileBase "Opened bookmark folder-stored session folder"

[Setup]
AppId={#AppId}
AppName=WinSCP
AppPublisher=Martin Prikryl
AppPublisherURL={#WebRoot}
AppSupportURL={#WebForum}
AppUpdatesURL={#WebRoot}eng/download.php
VersionInfoCompany=Martin Prikryl
VersionInfoDescription=Setup for WinSCP {#Version} (SFTP, FTP, WebDAV and SCP client)
VersionInfoVersion={#Major}.{#Minor}.{#Rev}.{#Build}
VersionInfoTextVersion={#Version}
VersionInfoCopyright=(c) 2000-{#Year} Martin Prikryl
DefaultDirName={pf}\WinSCP
LicenseFile=license.setup.txt
UninstallDisplayIcon={app}\WinSCP.exe
OutputDir={#OutputDir}
DisableStartupPrompt=yes
AppVersion={#Version}
AppVerName=WinSCP {#Version}
OutputBaseFilename=WinSCP-{#FTag}-Setup
SolidCompression=yes
WizardImageFile={#ImagesDir}\{#WizardImageFileBase} 100.bmp
WizardSmallImageFile={#ImagesDir}\{#WizardSmallImageFileBase} 100.bmp
ShowTasksTreeLines=yes
PrivilegesRequired=none
ShowLanguageDialog=auto
UsePreviousLanguage=yes
DisableProgramGroupPage=yes
MinVersion=0,5.1
SetupIconFile=winscpsetup.ico
DisableDirPage=no
#ifdef Sign
SignTool=sign $f "WinSCP Installer" https://winscp.net/eng/docs/installation
#endif

[Languages]
; English has to be first so that it is pre-selected
; on Setup Select Language window, when no translation matching
; Windows UI locale is available
Name: {#DefaultLang}; MessagesFile: {#MessagesPath(DefaultLang)}

#define FindHandle
#dim Languages[200]
#define LanguageCount 0
#define AnyLanguageComplete 0
#define LangI

#sub ProcessTranslationFile

  #define FileName FindGetFileName(FindHandle)
  #define Lang Copy(FileName, Pos(".", FileName)+1)

  #define LangNameFull ReadIni(MessagesPath(Lang), "LangOptions", "LanguageName")
  #define Sep Pos(" -", LangNameFull)
  #if Sep > 0
    #define LangName Copy(LangNameFull, 1, Sep - 1)
  #else
    #define LangName LangNameFull
  #endif
  #define LangID ReadIni(MessagesPath(Lang), "LangOptions", "LanguageID")
  #define LangCompleteness Int(ReadIni(MessagesPath(Lang), "CustomOptions", "TranslationCompleteness"))

  #expr Languages[LanguageCount*4] = Lang
  #expr Languages[LanguageCount*4+1] = LangName
  #expr Languages[LanguageCount*4+2] = LangID
  #expr Languages[LanguageCount*4+3] = LangCompleteness
  #expr LanguageCount++

#if LangCompleteness > CompletenessThreshold
Name: {#Lang}; MessagesFile: {#MessagesPath(Lang)}
  #expr AnyLanguageComplete = 1
#endif

#endsub /* sub ProcessTranslationFile */

#if FindHandle = FindFirst(TranslationDir + "\" + TranslationFileMask, 0)
  #define FResult 1
  #for {0; FResult; FResult = FindNext(FindHandle)} ProcessTranslationFile
  #expr FindClose(FindHandle)
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
Name: enableupdates\enablecollectusage; Description: {cm:EnableCollectUsage}
; Windows integration
Name: desktopicon; Description: {cm:DesktopIconTask}
Name: desktopicon\user; Description: {cm:DesktopIconUserTask}; \
  Flags: exclusive unchecked
Name: desktopicon\common; Description: {cm:DesktopIconCommonTask}; \
  Flags: exclusive
; No Quick Launch on Win7
Name: quicklaunchicon; Description: {cm:QuickLaunchIconTask}; \
  Flags: unchecked; OnlyBelowVersion: 6.1.7600
Name: sendtohook; Description: {cm:SendToHookTask}
Name: urlhandler; Description: {cm:RegisterAsUrlHandlers}
Name: searchpath; Description: {cm:AddSearchPath}; \
  Flags: unchecked; Check: IsAdminLoggedOn

[Icons]
Name: "{commonprograms}\WinSCP"; Filename: "{app}\WinSCP.exe"; Components: main; \
  Comment: "{cm:ProgramComment2}"
; This is created when desktopicon task is selected
Name: "{userdesktop}\WinSCP"; Filename: "{app}\WinSCP.exe"; \
  Tasks: desktopicon\user; Comment: "{cm:ProgramComment2}"
Name: "{commondesktop}\WinSCP"; Filename: "{app}\WinSCP.exe"; \
  Tasks: desktopicon\common; Comment: "{cm:ProgramComment2}"
; This is created when quicklaunchicon task is selected
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\WinSCP"; \
  Filename: "{app}\WinSCP.exe"; Tasks: quicklaunchicon
; This is created when sendtohook task is selected
Name: "{sendto}\{cm:SendToHookNew}"; Filename: "{app}\WinSCP.exe"; \
  Parameters: "/upload"; Tasks: sendtohook

[InstallDelete]
; Remove pre-524 licence file (without .txt extension)
Type: files; Name: "{app}\license"
; Remove pre-520 start menu folders
Type: filesandordirs; Name: "{commonprograms}\WinSCP"
Type: filesandordirs; Name: "{userprograms}\WinSCP"; Check: HasUserPrograms

[Run]
Filename: "{app}\WinSCP.exe"; Parameters: "/RegisterForDefaultProtocols"; \
  StatusMsg: {cm:RegisteringAsUrlHandlers}; Tasks: urlhandler
Filename: "{app}\WinSCP.exe"; Parameters: "/AddSearchPath"; \
  StatusMsg: {cm:AddingSearchPath}; Tasks: searchpath
Filename: "{app}\WinSCP.exe"; Parameters: "/ImportSitesIfAny"; \
  StatusMsg: {cm:ImportSites}; Flags: skipifsilent

[UninstallDelete]
; These additional files are created by application
Type: files; Name: "{app}\WinSCP.ini"
Type: files; Name: "{app}\WinSCP.cgl"

[Files]
; Put these to the top as we extract them on demand and
; that can take long with solid compression enabled
Source: "{#ImagesDir}\{#ExplorerFileBase} *.bmp"; Flags: dontcopy
Source: "{#ImagesDir}\{#CommanderFileBase} *.bmp"; Flags: dontcopy
; We do not need 100% images here, they are embedded already automatically
; by WizardImageFile and WizardSmallImageFile
Source: "{#ImagesDir}\{#WizardImageFileBase} *.bmp"; Excludes: "* 100.bmp"; \
  Flags: dontcopy
Source: "{#ImagesDir}\{#WizardSmallImageFileBase} *.bmp"; Excludes: "* 100.bmp"; \
  Flags: dontcopy
Source: "{#ImagesDir}\{#SelectDirFileBase} *.bmp"; Flags: dontcopy
#ifdef Donations
Source: "{#ImagesDir}\{#PayPalCardImage}"; Flags: dontcopy
#endif
Source: "{#MainFileSource}"; DestDir: "{app}"; \
  Components: main; Flags: ignoreversion
Source: "{#ConsoleFileSource}"; DestDir: "{app}"; \
  Components: main; Flags: ignoreversion
Source: "{#MapFileSource}"; DestDir: "{app}"; \
  Components: main; Flags: ignoreversion
Source: "{#AssemblyFileSource}"; DestDir: "{app}"; \
  Components: main; Flags: ignoreversion
Source: "license.txt"; DestDir: "{app}"; \
  Components: main; Flags: ignoreversion
Source: "{#ShellExtFileSource}"; DestDir: "{app}"; \
  Components: shellext; \
  Flags: regserver restartreplace uninsrestartdelete; \
  Check: not IsWin64 and IsShellExtNewer(ExpandConstant('{app}\{#ShellExtFileName}'), '{#GetFileVersion(ShellExtFileSource)}')
Source: "{#ShellExt64FileSource}"; DestDir: "{app}"; \
  Components: shellext; \
  Flags: regserver restartreplace uninsrestartdelete; \
  Check: IsWin64 and IsShellExtNewer(ExpandConstant('{app}\{#ShellExt64FileName}'), '{#GetFileVersion(ShellExt64FileSource)}')
Source: "{#PuttySourceDir}\LICENCE"; DestDir: "{app}\PuTTY"; \
  Components: pageant puttygen; Flags: ignoreversion
Source: "{#PuttySourceDir}\putty.chm"; DestDir: "{app}\PuTTY"; \
  Components: pageant puttygen; Flags: ignoreversion
Source: "{#PuttySourceDir}\pageant.exe"; DestDir: "{app}\PuTTY"; \
  Components: pageant; Flags: ignoreversion
Source: "{#PuttySourceDir}\puttygen.exe"; DestDir: "{app}\PuTTY"; \
  Components: puttygen; Flags: ignoreversion
Source: "Extensions\*.*"; DestDir: "{app}\Extensions"

[Registry]
Root: HKCU; Subkey: "{#ParentRegistryKey}"; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: "{#RegistryKey}"; Flags: uninsdeletekeyifempty
; Norton Commander interface
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; \
  ValueName: "Interface"; ValueData: 0; Check: UserSettings(1)
Root: HKLM; SubKey: "{#RegistryKey}"; ValueType: dword; \
  ValueName: "DefaultInterfaceInterface"; ValueData: 0; \
  Check: UserSettings(1); Flags: noerror
; Explorer-like interface
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; \
  ValueName: "Interface"; ValueData: 1; Check: not UserSettings(1)
Root: HKLM; SubKey: "{#RegistryKey}"; ValueType: dword; \
  ValueName: "DefaultInterfaceInterface"; ValueData: 1; \
  Check: not UserSettings(1); Flags: noerror
; If installer enabled ddext, let it reset the settings on uninstall,
; so the default is used on the next run
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; \
  ValueName: "DDExtEnabled"; ValueData: 1; Components: shellext; \
  Flags: uninsdeletevalue
; Updates
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface\Updates"; \
  ValueType: dword; ValueName: "Period"; ValueData: 7; \
  Tasks: enableupdates; Check: not UpdatesEnabled
Root: HKLM; SubKey: "{#RegistryKey}"; \
  ValueType: dword; ValueName: "DefaultUpdatesPeriod"; ValueData: 7; \
  Tasks: enableupdates; Flags: noerror
Root: HKLM; SubKey: "{#RegistryKey}"; \
  ValueType: dword; ValueName: "DefaultCollectUsage"; ValueData: 1; \
  Tasks: enableupdates\enablecollectusage; Flags: noerror

#if AnyLanguageComplete == 1

[Components]
Name: transl\eng; Description: {#EnglishLang}; Types: full custom compact; \
  Flags: fixed

#endif

#sub EmitLang

  #if Languages[LangI*4+3] > CompletenessThreshold

[Components]
Name: transl\{#Languages[LangI*4]}; Description: {#Languages[LangI*4+1]}; \
  Types: full compact custom; Check: IsLang('{#Languages[LangI*4]}')
Name: transl\{#Languages[LangI*4]}; Description: {#Languages[LangI*4+1]}; \
  Check: not IsLang('{#Languages[LangI*4]}')

[Files]
Source: "{#TranslationDir}\WinSCP.{#Languages[LangI*4]}"; DestDir: "{app}"; \
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
; Make sure no later uninstall task recreate the configuration
Filename: "{app}\WinSCP.exe"; Parameters: "/UninstallCleanup"; \
  RunOnceId: "UninstallCleanup"
Filename: "{app}\WinSCP.exe"; Parameters: "/RemoveSearchPath"; \
  RunOnceId: "RemoveSearchPath"
Filename: "{app}\WinSCP.exe"; Parameters: "/UnregisterForProtocols"; \
  RunOnceId: "UnregisterForProtocols"

[Code]
const
  NewLine = #13#10;

var
  TypicalTypeButton: TRadioButton;
  CustomTypeButton: TRadioButton;
  CommanderRadioButton: TRadioButton;
  ExplorerRadioButton: TRadioButton;
  LaunchCheckbox: TCheckbox;
  OpenGettingStartedCheckbox: TCheckbox;
  AreUpdatesEnabled: Boolean;
  AutomaticUpdate: Boolean;
  Upgrade: Boolean;
  MissingTranslations: string;
  PrevVersion: string;
  ShellExtNewerCacheFileName: string;
  ShellExtNewerCacheResult: Boolean;
#ifdef Donations
  DonationPanel: TPanel;
  AboutDonationCaption: TLabel;
#endif
  InstallationDone: Boolean;
  LicenseAccepted: Boolean;
  InitDir: string;
  InitComponents: string;
  InitTasks: string;
  InitInterface: Integer;
  Donated: Boolean;
  InterfacePage: TWizardPage;
  SetupTypePage: TWizardPage;

procedure ShowMessage(Text: string);
begin
  MsgBox(Text, mbInformation, MB_OK);
end;

function IsLang(Lang: string): Boolean;
begin
  Result := (Lang = ActiveLanguage);
end;

function IsWinVista: Boolean;
begin
  Result := (GetWindowsVersion >= $06000000);
end;

procedure CutVersionPart(var VersionString: string; var VersionPart: Word);
var
  P: Integer;
begin
  P := Pos('.', VersionString);
  if P > 0 then
  begin
    VersionPart := StrToIntDef(Copy(VersionString, 1, P - 1), 0);
    Delete(VersionString, 1, P);
  end
    else
  begin
    VersionPart := StrToIntDef(VersionString, 0);
    VersionString := '';
  end;
end;

function IsShellExtNewer(FileName: string; InstalledVersion: string): Boolean;
var
  ExistingMS, ExistingLS: Cardinal;
  ExistingMajor, ExistingMinor, ExistingRev, ExistingBuild: Cardinal;
  InstalledMajor, InstalledMinor, InstalledRev, InstalledBuild: Word;
begin
  if ShellExtNewerCacheFileName = FileName then
  begin
    if ShellExtNewerCacheResult then
    begin
      Log(Format('Allowing installation of shell extension %s as already decided', [FileName]));
      Result := True;
    end
      else
    begin
      Log(Format('Skipping installation of shell extension %s as already decided', [FileName]));
      Result := False;
    end;
  end
    else
  if not FileExists(FileName) then
  begin
    Log(Format('Shell extension %s does not exist yet, allowing installation', [FileName]));
    Result := True;
  end
    else
  if not GetVersionNumbers(FileName, ExistingMS, ExistingLS) then
  begin
    Log(Format('Cannot retrieve version of existing shell extension %s, allowing installation', [FileName]));
    Result := True;
  end
    else
  begin
    ExistingMajor := ExistingMS shr 16;
    ExistingMinor := ExistingMS and $FFFF;
    ExistingRev := ExistingLS shr 16;
    ExistingBuild := ExistingLS and $FFFF;
    Log(Format('Existing shell extension %s version: %d.%d.%d[.%d]', [FileName, ExistingMajor, ExistingMinor, ExistingRev, ExistingBuild]));

    Log(Format('Installed extension version string: %s', [InstalledVersion]));
    CutVersionPart(InstalledVersion, InstalledMajor);
    CutVersionPart(InstalledVersion, InstalledMinor);
    CutVersionPart(InstalledVersion, InstalledRev);
    CutVersionPart(InstalledVersion, InstalledBuild);
    Log(Format('Installed extension version: %d.%d.%d[.%d]', [InstalledMajor, InstalledMinor, InstalledRev, InstalledBuild]));

    if ((InstalledMajor > ExistingMajor)) or
       ((InstalledMajor = ExistingMajor) and (InstalledMinor > ExistingMinor)) or
       ((InstalledMajor = ExistingMajor) and (InstalledMinor = ExistingMinor) and (InstalledRev > ExistingRev)) then
    begin
      Log('Installed extension is newer than existing extension, allowing installation');
      Result := True;
    end
      else
    begin
      Log('Installed extension is same or older than existing extension, skipping installation');
      Result := False;
    end;
  end;

  ShellExtNewerCacheFileName := FileName;
  ShellExtNewerCacheResult := Result;
end;

function UpdatesEnabled: Boolean;
begin
  Result := AreUpdatesEnabled;
end;

function UserSettings(Settings: Integer): Boolean;
begin
  case Settings of
    1: Result := CommanderRadioButton.Checked;
    else Result := False;
  end;
end;

function LanguageName(Lang: string; Unknown: string): string;
begin
  #sub EmitLang2
  if Lang = '{#Languages[LangI*4]}' then Result := '{#Languages[LangI*4+1]}'
    else
  #endsub /* sub EmitLang2 */

  #for {LangI = 0; LangI < LanguageCount; LangI++} EmitLang2

  Result := Unknown;
end;

function ContainsLanguage(Lang: string): Boolean;
begin
  #sub EmitLang3
    #if Languages[LangI*4+3] > CompletenessThreshold
  if (Lang = '{#Languages[LangI*4]}') then Result := True
    else
    #endif
  #endsub /* sub EmitLang3 */

  #for {LangI = 0; LangI < LanguageCount; LangI++} EmitLang3

  Result := False;
end;

function LanguageCompleteness(Lang: string): Integer;
begin
  #sub EmitLang4
  if (Lang = '{#Languages[LangI*4]}') then
  begin
    Result := {#Languages[LangI*4+3]};
  end
    else
  #endsub /* sub EmitLang4 */

  #for {LangI = 0; LangI < LanguageCount; LangI++} EmitLang4

  // used also for the default language
  Result := 100;
end;

procedure OpenBrowser(Url: string);
var
  ErrorCode: Integer;
begin
  ShellExec('open', Url, '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;

function IsRestartingApplicationsPage: Boolean;
begin
  Result := WizardForm.PreparingMemo.Visible;
end;

function IsRestartPage: Boolean;
begin
  Result := WizardForm.YesRadio.Visible;
end;

procedure OpenHelp;
var
  HelpKeyword: string;
begin
  HelpKeyword := 'ui_installer'; // default

  case WizardForm.CurPageID of
    wpLicense:
      HelpKeyword := 'ui_installer_license';

    wpSelectDir:
      HelpKeyword := 'ui_installer_selectdir';

    wpSelectComponents:
      HelpKeyword := 'ui_installer_selectcomponents';

    wpSelectTasks:
      HelpKeyword := 'ui_installer_selecttasks';

    wpReady:
      HelpKeyword := 'ui_installer_ready';

    wpPreparing:
      if IsRestartingApplicationsPage then
        HelpKeyword := 'ui_installer_restartingapplications';

    wpFinished:
      HelpKeyword := 'ui_installer_finished';

    SetupTypePage.ID:
      HelpKeyword := 'ui_installer_setuptype';

    InterfacePage.ID:
      HelpKeyword := 'ui_installer_interface';
  end;

  OpenBrowser('{#WebDocumentation}' + HelpKeyword + '?' + ExpandConstant('{#WebArguments}'));
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

procedure ImageClick(Sender: TObject);
begin
  WizardForm.ActiveControl := TWinControl(TControl(Sender).Tag);
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
  LExt: string;
begin
  Path := AddBackslash(WizardDirValue);

  if FindFirst(Path + '{#TranslationFileMask}', FindRec) then
  begin
    try
      repeat
        if FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY = 0 then
        begin
          Ext := Uppercase(ExtractFileExt(FindRec.Name));
          if Pos('.', Ext) = 1  then
          begin
            Ext := Uppercase(Copy(Ext, 2, Length(Ext) - 1));
            LExt := Lowercase(Ext);
            if (Pos('-' + Ext + '-', '-{#AllTranslations}-') > 0) and
               not ContainsLanguage(LExt) then
              OnProcessTranslation(LExt, Path + FindRec.Name);
          end;
        end;
      until not FindNext(FindRec);
    finally
      FindClose(FindRec);
    end;
  end;
end;

function WillRestart: Boolean;
begin
  Result := WizardForm.YesRadio.Visible and WizardForm.YesRadio.Checked;
end;

procedure UpdatePostInstallRunCheckboxes(Sender: TObject);
begin
  LaunchCheckbox.Enabled := not WillRestart;
  OpenGettingStartedCheckbox.Enabled :=
    LaunchCheckbox.Enabled
end;

procedure LinkLabel(Control: TLabel);
begin
  Control.ParentFont := True;
  Control.Font.Style := Control.Font.Style + [fsUnderline];
  Control.Font.Color := clBlue;
  Control.Cursor := crHand;
end;

function IsTypicalInstallation: Boolean;
begin
  Result := TypicalTypeButton.Checked;
end;

#ifdef Donations

procedure AboutDonationsLinkClick(Sender: TObject);
begin
  OpenBrowser('{#WebRoot}eng/donate.php?' + ExpandConstant('{#WebArguments}'));
  Donated := true;
end;

procedure DonateLinkClick(Sender: TObject);
var
  Control: TControl;
begin
  Control := TControl(Sender);
  OpenBrowser('{#WebRoot}eng/donate.php?amount=' + IntToStr(Control.Tag) + '&currency=' + CustomMessage('Currency') + '&' + ExpandConstant('{#WebArguments}'));
  Donated := true;
end;

procedure CreateDonateLink(Amount: Integer; var Top: Integer);
var
  Caption: TLabel;
begin
  Caption := TLabel.Create(DonationPanel);
  Caption.Left := 0;
  Caption.Top := Top;
  Caption.Tag := Amount;
  Caption.Parent := DonationPanel;
  Caption.Caption := Format(CustomMessage('Donate'), ['$' + IntToStr(Amount)]);
  Caption.OnClick := @DonateLinkClick;
  LinkLabel(Caption);
  Top := Top + ScaleY(16);
end;

#endif

const
  fsSurface = 0;

procedure LoadEmbededBitmap(Image: TBitmapImage; Name: string; BackgroundColor: TColor);
var
  FileName: string;
  Bitmap: TAlphaBitmap;
begin
  ExtractTemporaryFile(Name);
  Bitmap := TAlphaBitmap.Create();
  Bitmap.AlphaFormat := afDefined;
  FileName := ExpandConstant('{tmp}\' + Name);
  Bitmap.LoadFromFile(FileName);
  // we won't need this anymore
  DeleteFile(FileName);

  Image.Bitmap := Bitmap;
  Bitmap.Free;
  Image.BackColor := BackgroundColor;
end;

function GetScalingFactor: Integer;
begin
  if WizardForm.Font.PixelsPerInch >= 192 then Result := 200
    else
  if WizardForm.Font.PixelsPerInch >= 144 then Result := 150
    else
  if WizardForm.Font.PixelsPerInch >= 120 then Result := 125
    else Result := 100;
end;

procedure LoadEmbededScaledBitmap(Image: TBitmapImage; NameBase: string; SizeBase: Integer; BackgroundColor: TColor);
var
  Name: String;
begin
  Name := Format('%s %d.bmp', [NameBase, SizeBase * GetScalingFactor div 100]);
  LoadEmbededBitmap(Image, Name, BackgroundColor);
end;

procedure LoadEmbededScaledIcon(Image: TBitmapImage; NameBase: string; SizeBase: Integer; BackgroundColor: TColor);
begin
  LoadEmbededScaledBitmap(Image, NameBase, SizeBase, BackgroundColor);
  Image.AutoSize := True;
end;

// WORKAROUND
// Checkboxes and Radio buttons created on runtime do
// not scale their height automatically
procedure ScaleFixedHeightControl(Control: TButtonControl);
begin
  Control.Height := ScaleY(Control.Height);
end;

function GetBottom(Control: TControl): Integer;
begin
  Result := Control.Top + Control.Height;
end;

function CmdLineParamExists(const Value: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
  begin
    if CompareText(ParamStr(I), Value) = 0 then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function InitializeSetup: Boolean;
var
  WaitInterval: Integer;
  Wait: Integer;
begin
  AutomaticUpdate := CmdLineParamExists('/AutomaticUpdate');
  if AutomaticUpdate then
  begin
    Log('Automatic update');

    Wait := 10000;
  end
    else
  begin
    Wait := 0;
  end;

  WaitInterval := 250;
  while (Wait > 0) and CheckForMutexes('{#AppMutex}') do
  begin
    Log('Application is still running, waiting');
    Sleep(WaitInterval);
    Wait := Wait - WaitInterval;
  end;

  while CheckForMutexes('{#AppMutex}') do
  begin
    if MsgBox(
         FmtMessage(SetupMessage(msgSetupAppRunningError), ['WinSCP']),
         mbError, MB_OKCANCEL) <> IDOK then
    begin
      Abort;
    end;
  end;

  Result := True;
end;

function IsElevated: Boolean;
begin
  Result := IsAdminLoggedOn or IsPowerUserLoggedOn;
end;

function HaveWriteAccessToApp: Boolean;
var
  FileName: string;
begin
  FileName := AddBackslash(WizardDirValue) + 'writetest.tmp';
  Result := SaveStringToFile(FileName, 'test', False);
  if Result then
  begin
    Log(Format('Have write access to the last installation path [%s]', [WizardDirValue]));
    DeleteFile(FileName);
  end
    else
  begin
    Log(Format('Does not have write access to the last installation path [%s]', [WizardDirValue]));
  end;
end;

procedure ExitProcess(uExitCode: UINT);
  external 'ExitProcess@kernel32.dll stdcall';
function ShellExecute(hwnd: HWND; lpOperation: string; lpFile: string;
  lpParameters: string; lpDirectory: string; nShowCmd: Integer): THandle;
  external 'ShellExecuteW@shell32.dll stdcall';

function Elevate: Boolean;
var
  I: Integer;
  RetVal: Integer;
  Params: string;
  S: string;
begin
  // Collect current instance parameters
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    // Unique log file name for the elevated instance
    if CompareText(Copy(S, 1, 5), '/LOG=') = 0 then
    begin
      S := S + '-elevated';
    end;
    // Do not pass our /SL5 switch
    if CompareText(Copy(S, 1, 5), '/SL5=') <> 0 then
    begin
      Params := Params + AddQuotes(S) + ' ';
    end;
  end;

  // ... and add selected language
  Params := Params + '/LANG=' + ActiveLanguage;

  Log(Format('Elevating setup with parameters [%s]', [Params]));
  RetVal := ShellExecute(0, 'runas', ExpandConstant('{srcexe}'), Params, '', SW_SHOW);
  Log(Format('Running elevated setup returned [%d]', [RetVal]));
  Result := (RetVal > 32);
  // if elevated executing of this setup succeeded, then...
  if Result then
  begin
    Log('Elevation succeeded');
    // exit this non-elevated setup instance
    ExitProcess(0);
  end
    else
  begin
    Log(Format('Elevation failed [%s]', [SysErrorMessage(RetVal)]));
  end;
end;

procedure InitializeWizard;
var
  DefaultLang: Boolean;
  UserInterface: Cardinal;
  UpdatesPeriod: Cardinal;
  Caption: TLabel;
  Image: TBitmapImage;
  HelpButton: TButton;
  P, P2: Integer;
  S: string;
  Completeness: Integer;
begin
  InstallationDone := False;
  LicenseAccepted := False;
  InitInterface := -1;

  DefaultLang := (ActiveLanguage = '{#DefaultLang}');

  Upgrade :=
    RegQueryStringValue(HKLM, '{#InnoSetupReg}', '{#InnoSetupAppPathReg}', S) or
    RegQueryStringValue(HKCU, '{#InnoSetupReg}', '{#InnoSetupAppPathReg}', S);

  if Upgrade then
  begin
    Log('Upgrade');
  end
    else
  begin
    Log('New installation');
  end;

  if Upgrade and GetVersionNumbersString(AddBackslash(WizardDirValue) + '{#MainFileName}', PrevVersion) and
     (PrevVersion[2] = '.') and (PrevVersion[4] = '.') and (PrevVersion[6] = '.') then
  begin
    PrevVersion := Copy(PrevVersion, 1, 5);
  end;

  Completeness := LanguageCompleteness(ActiveLanguage);
  if (Completeness < 100) and (not WizardSilent) then
  begin
    ShowMessage(FmtMessage(CustomMessage('IncompleteTranslation'), [IntToStr(Completeness)]));
  end;

  ProcessMissingTranslations(@CollectNames);

  WizardForm.KeyPreview := True;
  WizardForm.OnKeyDown := @FormKeyDown;
  // to accomodate one more task
  WizardForm.TasksList.Height := WizardForm.TasksList.Height + ScaleY(8);

  // allow installation without requiring user to accept license
  WizardForm.LicenseAcceptedRadio.Checked := True;
  WizardForm.LicenseAcceptedRadio.Visible := False;
  WizardForm.LicenseNotAcceptedRadio.Visible := False;
  WizardForm.LicenseMemo.Height :=
    GetBottom(WizardForm.LicenseNotAcceptedRadio) -
    WizardForm.LicenseMemo.Top - 5;

  // hide installation types combo
  WizardForm.TypesCombo.Visible := False;
  WizardForm.ComponentsList.Height :=
    GetBottom(WizardForm.ComponentsList) -
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
  HelpButton.Caption := CustomMessage('HelpButton');
  HelpButton.OnClick := @HelpButtonClick;

  // elevate

  if not IsWinVista then
  begin
    Log(Format('This version of Windows [%x] does not support elevation', [GetWindowsVersion]));
  end
    else
  if IsElevated then
  begin
    Log('Running elevated');
  end
    else
  begin
    Log('Running non-elevated');
    if Upgrade then
    begin
      if not HaveWriteAccessToApp then
      begin
        Elevate;
      end;
    end
      else
    begin
      if not Elevate then
      begin
        WizardForm.DirEdit.Text := ExpandConstant('{localappdata}\WinSCP');
        Log(Format('Falling back to local application user folder [%s]', [WizardForm.DirEdit.Text]));
      end;
    end;
  end;

  // installation type page

  SetupTypePage := CreateCustomPage(wpLicense,
    CustomMessage('SetupTypeTitle'),
    CustomMessage('SetupTypePrompt'));

  TypicalTypeButton := TRadioButton.Create(SetupTypePage);
  if not Upgrade then
    S := CustomMessage('TypicalType')
  else
    S := CustomMessage('TypicalUpgradeType');
  TypicalTypeButton.Caption :=
    FmtMessage(CustomMessage('Recommended'), [S]);
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
  ScaleFixedHeightControl(TypicalTypeButton);
  TypicalTypeButton.Parent := SetupTypePage.Surface;

  Caption := TLabel.Create(SetupTypePage);
  Caption.WordWrap := True;
  if not Upgrade then
  begin
    if DefaultLang then
      S := CustomMessage('TypicalType2Eng')
    else
      S := FmtMessage(CustomMessage('TypicalType2Intl'), [CustomMessage('LocalLanguageName')]);
    Caption.Caption :=
      CustomMessage('TypicalType1') + NewLine +
      S + NewLine +
      CustomMessage('TypicalType3');
  end
    else
  begin
    if Length(MissingTranslations) > 0 then
    begin
      #if AnyLanguageComplete
        S := FmtMessage(CustomMessage('TypicalUpgradeTypeMissingTransl'), [MissingTranslations]);
      #else
        S := CustomMessage('TypicalUpgradeTypeNoTransl');
      #endif
      S := NewLine + S;
    end
      else S := '';

    Caption.Caption :=
      CustomMessage('TypicalUpgradeType1') + S;
  end;
  Caption.Left := ScaleX(4) + ScaleX(20);
  Caption.Width := SetupTypePage.SurfaceWidth - Caption.Left;
  Caption.Top := GetBottom(TypicalTypeButton) + ScaleY(6);
  Caption.Parent := SetupTypePage.Surface;
  Caption.FocusControl := TypicalTypeButton;
  Caption.OnClick := @CaptionClick;

  CustomTypeButton := TRadioButton.Create(SetupTypePage);
  if not Upgrade then
    CustomTypeButton.Caption := CustomMessage('CustomType')
  else
    CustomTypeButton.Caption := CustomMessage('CustomUpgradeType');
  CustomTypeButton.Checked := (not TypicalTypeButton.Checked);
  CustomTypeButton.Left := ScaleX(4);
  CustomTypeButton.Width := SetupTypePage.SurfaceWidth -
    CustomTypeButton.Left;
  CustomTypeButton.Top := GetBottom(Caption) + ScaleY(10);
  ScaleFixedHeightControl(CustomTypeButton);
  CustomTypeButton.Parent := SetupTypePage.Surface;

  Caption := TLabel.Create(SetupTypePage);
  Caption.WordWrap := True;
  if not Upgrade then
  begin
    Caption.Caption :=
      CustomMessage('CustomType1');
  end
    else
  begin
    Caption.Caption :=
      CustomMessage('CustomUpgradeType1') + NewLine +
      CustomMessage('CustomUpgradeType2');
  end;
  Caption.Left := ScaleX(4) + ScaleX(20);
  Caption.Width := SetupTypePage.SurfaceWidth - Caption.Left;
  Caption.Top := GetBottom(CustomTypeButton) + ScaleY(6);
  Caption.Parent := SetupTypePage.Surface;
  Caption.FocusControl := CustomTypeButton;
  Caption.OnClick := @CaptionClick;

  // interface page

  InterfacePage := CreateCustomPage(wpSelectTasks,
    CustomMessage('UserSettingsTitle'),
    CustomMessage('UserSettingsPrompt'));

  UpdatesPeriod := 0;
  RegQueryDWordValue(HKCU, '{#RegistryKey}\Configuration\Interface\Updates',
    'Period', UpdatesPeriod);
  AreUpdatesEnabled := (UpdatesPeriod <> 0);

  UserInterface := 0; { default is commander }
  RegQueryDWordValue(HKCU, '{#RegistryKey}\Configuration\Interface',
    'Interface', UserInterface);

  Caption := TLabel.Create(InterfacePage);
  Caption.Caption := CustomMessage('UserInterfaceStyle');
  Caption.Width := InterfacePage.SurfaceWidth;
  Caption.Parent := InterfacePage.Surface;

  CommanderRadioButton := TRadioButton.Create(InterfacePage);
  CommanderRadioButton.Caption := CustomMessage('NortonCommanderInterfaceC');
  CommanderRadioButton.Checked := (UserInterface = 0);
  CommanderRadioButton.Left := ScaleX(4);
  CommanderRadioButton.Width := ScaleX(116);
  CommanderRadioButton.Top := GetBottom(Caption) + ScaleY(6);
  ScaleFixedHeightControl(CommanderRadioButton);
  CommanderRadioButton.Parent := InterfacePage.Surface;

  Image := TBitmapImage.Create(InterfacePage);
  Image.Top := GetBottom(CommanderRadioButton) + ScaleY(6);
  Image.Left := CommanderRadioButton.Left + ScaleX(45);
  Image.Parent := InterfacePage.Surface;
  LoadEmbededScaledIcon(Image, '{#CommanderFileBase}', 32, InterfacePage.Surface.Color);
  Image.OnClick := @ImageClick;
  Image.Tag := Integer(CommanderRadioButton);

  Caption := TLabel.Create(InterfacePage);
  Caption.WordWrap := True;
  Caption.Caption :=
      CustomMessage('NortonCommanderInterface1') + NewLine +
      CustomMessage('NortonCommanderInterface2') + NewLine +
      CustomMessage('NortonCommanderInterface3');
  Caption.Left := CommanderRadioButton.Left + CommanderRadioButton.Width;
  Caption.Width := InterfacePage.SurfaceWidth - Caption.Left;
  Caption.Top := CommanderRadioButton.Top;
  Caption.Parent := InterfacePage.Surface;
  Caption.FocusControl := CommanderRadioButton;
  Caption.OnClick := @CaptionClick;

  ExplorerRadioButton := TRadioButton.Create(InterfacePage);
  ExplorerRadioButton.Caption := CustomMessage('ExplorerInterfaceC');
  ExplorerRadioButton.Checked := (UserInterface <> 0);
  ExplorerRadioButton.Left := ScaleX(4);
  ExplorerRadioButton.Width := CommanderRadioButton.Width;
  ExplorerRadioButton.Top := GetBottom(Caption) + ScaleY(10);
  ScaleFixedHeightControl(ExplorerRadioButton);
  ExplorerRadioButton.Parent := InterfacePage.Surface;

  Image := TBitmapImage.Create(InterfacePage);
  Image.Top := GetBottom(ExplorerRadioButton) + ScaleY(6);
  Image.Left := ExplorerRadioButton.Left + ScaleX(45);
  Image.Parent := InterfacePage.Surface;
  LoadEmbededScaledIcon(Image, '{#ExplorerFileBase}', 32, InterfacePage.Surface.Color);
  Image.OnClick := @ImageClick;
  Image.Tag := Integer(ExplorerRadioButton);

  Caption := TLabel.Create(InterfacePage);
  Caption.WordWrap := True;
  Caption.Caption :=
      CustomMessage('ExplorerInterface1') + NewLine +
      CustomMessage('ExplorerInterface2') + NewLine +
      CustomMessage('ExplorerInterface3');
  Caption.Left := ExplorerRadioButton.Left + ExplorerRadioButton.Width;
  Caption.Width := InterfacePage.SurfaceWidth - Caption.Left;
  Caption.Top := ExplorerRadioButton.Top;
  Caption.Parent := InterfacePage.Surface;
  Caption.FocusControl := ExplorerRadioButton;
  Caption.OnClick := @CaptionClick;

  // run checkbox
  LaunchCheckbox := TCheckbox.Create(WizardForm.FinishedPage);
  LaunchCheckbox.Caption := CustomMessage('Launch');
  LaunchCheckbox.Checked := True;
  LaunchCheckbox.Left := WizardForm.YesRadio.Left;
  LaunchCheckbox.Width := WizardForm.YesRadio.Width;
  ScaleFixedHeightControl(LaunchCheckbox);
  LaunchCheckbox.Parent := WizardForm.FinishedPage;
  OpenGettingStartedCheckbox := TCheckbox.Create(WizardForm.FinishedPage);
  OpenGettingStartedCheckbox.Caption := CustomMessage('OpenGettingStarted');
  OpenGettingStartedCheckbox.Checked := True;
  OpenGettingStartedCheckbox.Left := WizardForm.YesRadio.Left;
  OpenGettingStartedCheckbox.Width := WizardForm.YesRadio.Width;
  ScaleFixedHeightControl(OpenGettingStartedCheckbox);
  OpenGettingStartedCheckbox.Parent := WizardForm.FinishedPage;

#ifdef Donations

  DonationPanel := TPanel.Create(WizardForm.FinishedPage);
  DonationPanel.Left := WizardForm.YesRadio.Left;
  DonationPanel.Width := WizardForm.YesRadio.Width;
  DonationPanel.Parent := WizardForm.FinishedPage;
  DonationPanel.BevelInner := bvNone;
  DonationPanel.BevelOuter := bvNone;
  DonationPanel.Color := WizardForm.FinishedPage.Color;

  Caption := TLabel.Create(DonationPanel);
  Caption.WordWrap := True;
  Caption.Caption := CustomMessage('PleaseDonate');
  Caption.Left := 0;
  Caption.Top := 0;
  Caption.Width := DonationPanel.Width;
  Caption.Parent := DonationPanel;

  P := GetBottom(Caption) + ScaleY(12);
  P2 := P;

  CreateDonateLink( 9, P);
  CreateDonateLink(19, P);
  CreateDonateLink(49, P);

  AboutDonationCaption := TLabel.Create(DonationPanel);
  AboutDonationCaption.Left := 0;
  AboutDonationCaption.Top := P;
  AboutDonationCaption.Parent := DonationPanel;
  AboutDonationCaption.Caption := CustomMessage('AboutDonations');
  AboutDonationCaption.OnClick := @AboutDonationsLinkClick;
  LinkLabel(AboutDonationCaption);

  Image := TBitmapImage.Create(DonationPanel);
  LoadEmbededBitmap(Image, '{#PayPalCardImage}', DonationPanel.Color);
  Image.AutoSize := True;
  Image.Cursor := crHand;
  Image.Parent := DonationPanel;
  Image.Left := ScaleX(108);
  Image.Top := P2 + ScaleX(8);
  Image.Hint := CustomMessage('AboutDonations');
  Image.ShowHint := True;
  Image.OnClick := @AboutDonationsLinkClick;

  DonationPanel.Height := GetBottom(AboutDonationCaption);

#endif

  WizardForm.YesRadio.OnClick := @UpdatePostInstallRunCheckboxes;
  WizardForm.NoRadio.OnClick := @UpdatePostInstallRunCheckboxes;
  UpdatePostInstallRunCheckboxes(nil);

  // 100% images are automatically loaded by
  // WizardImageFile and WizardSmallImageFile
  if GetScalingFactor > 100 then
  begin
    LoadEmbededScaledBitmap(WizardForm.WizardBitmapImage, '{#WizardImageFileBase}', 100, 0);
    LoadEmbededScaledBitmap(WizardForm.WizardBitmapImage2, '{#WizardImageFileBase}', 100, 0);
    LoadEmbededScaledBitmap(WizardForm.WizardSmallBitmapImage, '{#WizardSmallImageFileBase}', 100, 0);
  end;

  // Text does not scale as quick as with DPI,
  // so the icon may overlap the labels. Shift them.
  P := WizardForm.SelectDirBitmapImage.Width;
  LoadEmbededScaledIcon(WizardForm.SelectDirBitmapImage, '{#SelectDirFileBase}', 32, WizardForm.SelectDirPage.Color);
  P := (WizardForm.SelectDirBitmapImage.Width - P);
  // Vertical change should be the same as horizontal
  WizardForm.SelectDirLabel.Left := WizardForm.SelectDirLabel.Left + P;
  WizardForm.SelectDirBrowseLabel.Top := WizardForm.SelectDirBrowseLabel.Top + P;
  WizardForm.DirEdit.Top := WizardForm.DirEdit.Top + P;
  WizardForm.DirBrowseButton.Top := WizardForm.DirBrowseButton.Top + P;
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
var
  S: string;
begin
  if IsTypicalInstallation then S := 'typical'
    else S := 'custom';

  SetPreviousData(PreviousDataKey, '{#SetupTypeData}', S);
end;

function SaveCheckListBoxState(ListBox: TNewCheckListBox): string;
var
  I: Integer;
begin
  for I := 0 to ListBox.Items.Count - 1 do
  begin
    Result := Result + IntToStr(Integer(ListBox.State[I]));
  end;
end;

procedure CurPageChanged(CurPageID: Integer);
var
  Delta: Integer;
  LineHeight: Integer;
  LaunchCheckboxTop: Integer;
begin
  if CurPageID = wpLicense then
  begin
    WizardForm.NextButton.Caption := CustomMessage('AcceptButton')
  end;

  if CurPageID = wpSelectDir then
  begin
    if InitDir = '' then
      InitDir := WizardForm.DirEdit.Text;
  end
    else
  if CurPageID = wpSelectComponents then
  begin
    if InitComponents = '' then
      InitComponents := SaveCheckListBoxState(WizardForm.ComponentsList);
  end
    else
  if CurPageID = wpSelectTasks then
  begin
    if InitTasks = '' then
      InitTasks := SaveCheckListBoxState(WizardForm.TasksList);
  end
    else
  if CurPageID = InterfacePage.ID then
  begin
    if InitInterface < 0 then
      InitInterface := Integer(CommanderRadioButton.Checked);
  end
    else
  if CurPageID = wpFinished then
  begin
    LineHeight := (WizardForm.NoRadio.Top - WizardForm.YesRadio.Top);

    // Are we at the "Restart?" screen
    if IsRestartPage then
    begin
      WizardForm.FinishedLabel.Caption :=
        CustomMessage('FinishedRestartDragExtLabel') + NewLine;

      Delta := WizardForm.AdjustLabelHeight(WizardForm.FinishedLabel);
      WizardForm.YesRadio.Top := WizardForm.YesRadio.Top + Delta;
      WizardForm.NoRadio.Top := WizardForm.NoRadio.Top + Delta;

      LaunchCheckboxTop := WizardForm.NoRadio.Top + LineHeight;
#ifdef Donations
      DonationPanel.Visible := False;
#endif
    end
      else
    begin
      LaunchCheckboxTop := WizardForm.RunList.Top;
    end;

    LaunchCheckbox.Top := LaunchCheckboxTop;
    OpenGettingStartedCheckbox.Top := LaunchCheckbox.Top + LineHeight;

    UpdatePostInstallRunCheckboxes(nil);

#ifdef Donations
    if DonationPanel.Visible then
    begin
      DonationPanel.Top := GetBottom(OpenGettingStartedCheckbox) + ScaleY(12);

      // Hide "about donations" if it does not fit nicely
      // (happens on "long" languages, as German)
      if (DonationPanel.Top + GetBottom(AboutDonationCaption)) >
         (WizardForm.FinishedPage.Height - ScaleY(8)) then
      begin
        AboutDonationCaption.Visible := False;
      end;
    end;
#endif
  end
    else
  if CurPageID = SetupTypePage.ID then
  begin
    Log('License accepted');
    LicenseAccepted := True;
  end
    else
  if CurPageID = wpPreparing then
  begin
    // Are we at the "Restart applications?" screen.
    // If PreparingMemo is hidden, it's "installation/removal was not completed" screen
    if IsRestartingApplicationsPage then
    begin
      WizardForm.PreparingLabel.Caption :=
        CustomMessage('ApplicationsFoundDragExt');
      WizardForm.IncTopDecHeight(WizardForm.PreparingMemo,
        WizardForm.AdjustLabelHeight(WizardForm.PreparingLabel));
    end;
  end;
end;

function AskedRestart: Boolean;
begin
  Result := IsRestartPage;
end;

procedure DeinitializeSetup;
var
  WinHttpReq: Variant;
  ReportUrl: string;
  ReportData: string;
begin
  // cannot send report, unless user already accepted license
  // (with privacy policy)
  if LicenseAccepted then
  begin
    Log('Preparing installation report');

    ReportData := Format(
      'installed=%d&silent=%d&ver=%s&lang=%s&prevver=%s&', [
       Integer(InstallationDone), Integer(WizardSilent),
       '{#VersionOnly}', ActiveLanguage,
       PrevVersion]);

    try
      ReportUrl := '{#WebReport}?' + ReportData;

      Log('Sending installation report: ' + ReportUrl);

      WinHttpReq := CreateOleObject('WinHttp.WinHttpRequest.5.1');
      WinHttpReq.Open('GET', ReportUrl, False);
      WinHttpReq.Send('');

      Log('Installation report send result: ' + IntToStr(WinHttpReq.Status) + ' ' + WinHttpReq.StatusText);
    except
      Log('Error sending installation report: ' + GetExceptionMessage);
    end;
  end;
end;

procedure ExecApp(Params: string; ShowCmd: Integer; Wait: TExecWait);
var
  Path: string;
  ErrorCode: Integer;
begin
  Path := ExpandConstant('{app}\{#MainFileName}');
  ExecAsOriginalUser(Path, Params, '', ShowCmd, Wait, ErrorCode)
end;

procedure OpenBrowserGettingStarted;
var
  WebGettingStarted: string;
begin
  WebGettingStarted :=
    ExpandConstant('{#WebGettingStarted}') + PrevVersion +
    '&automatic=' + IntToStr(Integer(AutomaticUpdate));
  Log('Opening getting started page: ' + WebGettingStarted);
  OpenBrowser(WebGettingStarted);
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  ShowCmd: Integer;
  OpenGettingStarted: Boolean;
  UsageData: string;
  CanPostInstallRuns: Boolean;
  Installations: Cardinal;
begin
  if CurStep = ssPostInstall then
  begin
    Log('Post install');
    if Length(MissingTranslations) > 0 then
    begin
      Log('Removing obsolete translations');
      WizardForm.StatusLabel.Caption :=
        CustomMessage('RemovingObsoleteTranslations');
      ProcessMissingTranslations(@DeleteTranslation);
    end;
    InstallationDone := True;
  end
    else
  if CurStep = ssDone then
  begin
    Log('Done');
    // bug in InnoSetup causes it using ssDone even when
    // setup failed because machine was not restarted to complete previous
    // installation. double check that ssPostInstall was called
    if InstallationDone then
    begin
      CanPostInstallRuns := (not WizardSilent) and (not WillRestart);

      OpenGettingStarted :=
        OpenGettingStartedCheckbox.Enabled and
         OpenGettingStartedCheckbox.Checked;

      UsageData := '/Usage=';

      // old style counter
      UsageData := UsageData + Format('TypicalInstallation:%d,', [Integer(IsTypicalInstallation)]);

      UsageData := UsageData + 'InstallationsUser+,';

      Installations := 0; // default, if the counter does not exist
      RegQueryDWordValue(HKEY_LOCAL_MACHINE, '{#RegistryKey}', 'Installations', Installations);
      Inc(Installations);
      if not RegWriteDWordValue(HKEY_LOCAL_MACHINE, '{#RegistryKey}', 'Installations', Installations) then
      begin
        Log('Cannot increment administrator installations counter, probably a non-elevated installation');
      end;

      // new style counters
      if not Upgrade then
      begin
        if IsTypicalInstallation then
          UsageData := UsageData + 'InstallationsFirstTypical+,'
        else
          UsageData := UsageData + 'InstallationsFirstCustom+,';
      end
        else
      begin
        if AutomaticUpdate then
          UsageData := UsageData + 'InstallationsUpgradeAutomatic+,'
        else if IsTypicalInstallation then
          UsageData := UsageData + 'InstallationsUpgradeTypical+,'
        else
          UsageData := UsageData + 'InstallationsUpgradeCustom+,';
      end;

      if (InitDir <> '') and (InitDir <> WizardForm.DirEdit.Text) then
        UsageData := UsageData + 'InstallationsCustomDir+,';
      if (InitComponents <> '') and (InitComponents <> SaveCheckListBoxState(WizardForm.ComponentsList)) then
        UsageData := UsageData + 'InstallationsCustomComponents+,';
      if (InitTasks <> '') and (InitTasks <> SaveCheckListBoxState(WizardForm.TasksList)) then
        UsageData := UsageData + 'InstallationsCustomTasks+,';
      if (InitInterface >= 0) and (InitInterface <> Integer(CommanderRadioButton.Checked)) then
        UsageData := UsageData + 'InstallationsCustomInterface+,';
      if CanPostInstallRuns and OpenGettingStarted then
        UsageData := UsageData + 'InstallationsGettingStarted+,';
      if CanPostInstallRuns and LaunchCheckbox.Checked then
        UsageData := UsageData + 'InstallationsLaunch+,';
      if WizardSilent then
        UsageData := UsageData + 'InstallationsSilent+,';
      if AskedRestart then
        UsageData := UsageData + 'InstallationsNeedRestart+,';
      if WillRestart then
        UsageData := UsageData + 'InstallationsRestart+,';
      if Donated then
        UsageData := UsageData + 'InstallationsDonate+,';
      if not IsElevated then
        UsageData := UsageData + 'InstallationsNonElevated+,';

      // have to do this before running WinSCP GUI instance below,
      // otherwise it loads the empty/previous counters and overwrites our changes,
      // when it's closed
      Log('Recording installer usage statistics: ' + UsageData);
      // make sure we write the counters using the "normal" account
      // (the account that will be used to report the counters)
      ExecApp(UsageData, SW_HIDE, ewWaitUntilTerminated);

      if AutomaticUpdate then
      begin
        Log('Launching WinSCP after automatic update');
        ExecApp('', SW_SHOWNORMAL, ewNoWait);

        if CmdLineParamExists('/OpenGettingStarted') then
        begin
          OpenBrowserGettingStarted;
        end;
      end
        else
      if CanPostInstallRuns then
      begin
        if OpenGettingStarted then
        begin
          OpenBrowserGettingStarted;
        end;

        if LaunchCheckbox.Checked then
        begin
          if OpenGettingStarted then
          begin
            Log('Will launch WinSCP minimized');
            ShowCmd := SW_SHOWMINIMIZED
          end
            else
          begin
            ShowCmd := SW_SHOWNORMAL;
          end;

          Log('Launching WinSCP');
          ExecApp('', ShowCmd, ewNoWait);
        end;
      end;
    end;
  end;
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  Result :=
    { Hide most pages during typical installation }
    IsTypicalInstallation and
    ((PageID = wpSelectDir) or (PageID = wpSelectComponents) or
     (PageID = wpSelectTasks) or
     { Hide Interface page for upgrades only, show for fresh installs }
     ((PageID = InterfacePage.ID) and Upgrade));
end;

function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo,
  MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: string): string;
var
  S: string;
  S2: string;
begin
  S := '';

  S := S + MemoDirInfo + NewLine + NewLine;

  if not Upgrade then
  begin
    if IsTypicalInstallation then S2 := CustomMessage('TypicalType')
      else S2 := CustomMessage('CustomType');
  end
    else
  begin
    if IsTypicalInstallation then S2 := CustomMessage('TypicalUpgradeType')
      else S2 := CustomMessage('CustomUpgradeType');
  end;
  StringChange(S2, '&', '');
  S := S + SetupMessage(msgReadyMemoType) + NewLine + Space + S2 + NewLine + NewLine;

  S := S + MemoComponentsInfo + NewLine + NewLine;

  if Length(MemoGroupInfo) > 0 then
    S := S + MemoGroupInfo + NewLine + NewLine;

  if Length(MemoTasksInfo) > 0 then
    S := S + MemoTasksInfo + NewLine + NewLine;

  S := S + CustomMessage('UserSettingsOverview') + NewLine;
  S := S + Space;
  if CommanderRadioButton.Checked then S2 := CustomMessage('NortonCommanderInterfaceC')
    else S2 := CustomMessage('ExplorerInterfaceC');
  StringChange(S2, '&', '');
  S := S + S2;
  S := S + NewLine;

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

function HasUserPrograms: Boolean;
begin
  // To avoid the installer failing when the {userprograms}
  // cannot be resolved (when installing via system account or SCCM)
  try
    ExpandConstant('{userprograms}');
    Log('Have user programs');
    Result := True;
  except
    Log('Does not have user programs');
    Result := False;
  end;
end;

#expr SaveToFile(AddBackslash(SourcePath) + "Preprocessed.iss")
