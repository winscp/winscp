#define AppId "winscp3"
#define AppMutex "WinSCP"
#define ParentRegistryKey "Software\Martin Prikryl"
#define RegistryKey ParentRegistryKey+"\WinSCP 2"
#define DefaultLang "en"
#define WebRoot "https://winscp.net/"
#define WebForum WebRoot+"forum/"
#define WebDocumentation WebRoot+"eng/docs/"
#define WebReport "https://winscp.net/install.php"
#define Year 2025
#define EnglishLang "English"
#define SetupTypeData "SetupType"
#define InnoSetupReg "Software\Microsoft\Windows\CurrentVersion\Uninstall\" + AppId + "_is1"
#define InnoSetupAppPathReg "Inno Setup: App Path"

#ifndef CompletenessThreshold
  #define CompletenessThreshold 100
#else
  #define CompletenessThreshold Int(CompletenessThreshold)
#endif

#ifndef InclusionThreshold
  #define InclusionThreshold 100
#else
  #define InclusionThreshold Int(InclusionThreshold)
#endif

#ifndef PuttySourceDir
  #if DirExists("c:\Program Files (x86)\PuTTY")
    #define PuttySourceDir "c:\Program Files (x86)\PuTTY"
  #else
    #define PuttySourceDir "c:\Program Files\PuTTY"
  #endif
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
#expr GetVersionComponents(MainFileSource, Major, Minor, Rev, Build)
#define VersionOnly Str(Major)+"."+Str(Minor)+(Rev > 0 ? "."+Str(Rev) : "")
#define Version VersionOnly+(Status != "" ? " "+Status : "")

#ifndef BaseFilename
  #define FTag VersionOnly+(Status != "" ? "."+Status : "")
  #define BaseFilename "WinSCP-" + FTag + "-Setup"
#endif

#define WebArguments "ver=" +VersionOnly + "&lang={language}&utm_source=winscp&utm_medium=setup&utm_campaign=" + VersionOnly
#define WebGettingStarted WebRoot + "eng/installed.php?" + WebArguments + "&prevver="

#define MessagesPath(L) TranslationDir + "\" + "WinSCP." + L + ".islu"

#define ExplorerFileBase "Explorer"
#define CommanderFileBase "Commander"
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
VersionInfoOriginalFileName={#BaseFilename}.exe
DefaultDirName={autopf}\WinSCP
LicenseFile=license.setup.txt
UninstallDisplayIcon={app}\WinSCP.exe
OutputDir={#OutputDir}
DisableStartupPrompt=yes
AppVersion={#Version}
AppVerName=WinSCP {#Version}
OutputBaseFilename={#BaseFilename}
SolidCompression=yes
#ifdef ImagesDir
WizardImageFile={#ImagesDir}\Tall *.bmp
WizardSmallImageFile={#ImagesDir}\Square *.bmp
#endif
ShowTasksTreeLines=yes
PrivilegesRequired=admin
PrivilegesRequiredOverridesAllowed=commandline dialog
ShowLanguageDialog=auto
UsePreviousLanguage=no
DisableProgramGroupPage=yes
SetupIconFile=winscpsetup.ico
DisableDirPage=no
WizardStyle=modern
; We do not want the Explorer restarts as that is not pleasant to the user
CloseApplications=no
UsedUserAreasWarning=no
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
; For some reason the variable cannot be defined near the code where we use it
#define AllTranslationsBuf

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
  ; Not used atm
  #expr Languages[LanguageCount*4+1] = LangName
  ; Not used atm
  #expr Languages[LanguageCount*4+2] = LangID
  #expr Languages[LanguageCount*4+3] = LangCompleteness
  #expr LanguageCount++

#if LangCompleteness >= CompletenessThreshold
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
; Because the files for the component have Check parameters, they are ignored for the size calculation
Name: shellext; Description: {cm:ShellExtComponent}; \
  ExtraDiskSpaceRequired: {#Max(FileSize(ShellExtFileSource), FileSize(ShellExt64FileSource))}; \
  Types: full compact;
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
Name: sendtohook; Description: {cm:SendToHookTask}
Name: urlhandler; Description: {cm:RegisterAsUrlHandlers}
Name: searchpath; Description: {cm:AddSearchPath}; \
  Flags: unchecked; Check: IsAdminInstallMode

[Icons]
Name: "{autoprograms}\WinSCP"; Filename: "{app}\WinSCP.exe"; Components: main; \
  Comment: "{cm:ProgramComment2}"
; This is created when desktopicon task is selected
Name: "{autodesktop}\WinSCP"; Filename: "{app}\WinSCP.exe"; \
  Tasks: desktopicon; Comment: "{cm:ProgramComment2}"
; This is created when sendtohook task is selected
Name: "{usersendto}\{cm:SendToHookNew}"; Filename: "{app}\WinSCP.exe"; \
  Parameters: "/upload"; Tasks: sendtohook

[InstallDelete]
; Remove pre-5.8.2 PuTTY help file
Type: files; Name: "{app}\PuTTY\putty.hlp"
; Remove pre-524 licence file (without .txt extension)
Type: files; Name: "{app}\license"
; Remove pre-520 start menu folders
Type: filesandordirs; Name: "{commonprograms}\WinSCP"
Type: filesandordirs; Name: "{userprograms}\WinSCP"; Check: HasUserPrograms

[Run]
Filename: "{app}\WinSCP.exe"; Parameters: "{code:GetTaskCmdLine|RegisterForDefaultProtocols}"; \
  StatusMsg: {cm:RegisteringAsUrlHandlers}; Tasks: urlhandler
Filename: "{app}\WinSCP.exe"; Parameters: "{code:GetTaskCmdLine|AddSearchPath}"; \
  StatusMsg: {cm:AddingSearchPath}; Tasks: searchpath
Filename: "{app}\WinSCP.exe"; Parameters: "{code:GetTaskCmdLine|ImportSitesIfAny}"; \
  StatusMsg: {cm:ImportSites}; Flags: skipifsilent

[UninstallDelete]
; These additional files are created by application
Type: files; Name: "{app}\WinSCP.ini"
Type: files; Name: "{app}\WinSCP.cgl"

[Files]
#ifdef ImagesDir
; Put these to the top as we extract them on demand and
; that can take long with solid compression enabled
Source: "{#ImagesDir}\{#ExplorerFileBase} *.bmp"; Flags: dontcopy
Source: "{#ImagesDir}\{#CommanderFileBase} *.bmp"; Flags: dontcopy
Source: "{#ImagesDir}\{#SelectDirFileBase} *.bmp"; Flags: dontcopy
#ifdef Donations
Source: "{#ImagesDir}\{#PayPalCardImage}"; Flags: dontcopy
#endif
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
; If the Check is ever removed, remove the ExtraDiskSpaceRequired parameter of the component too
Source: "{#ShellExtFileSource}"; DestDir: "{app}"; \
  Components: shellext; \
  Flags: regserver restartreplace uninsrestartdelete ignoreversion; \
  Check: not IsWin64 and ShouldInstallShellExt(ExpandConstant('{app}\{#ShellExtFileName}'), '{#GetVersionNumbersString(ShellExtFileSource)}')
Source: "{#ShellExt64FileSource}"; DestDir: "{app}"; \
  Components: shellext; \
  Flags: regserver restartreplace uninsrestartdelete ignoreversion; \
  Check: IsWin64 and ShouldInstallShellExt(ExpandConstant('{app}\{#ShellExt64FileName}'), '{#GetVersionNumbersString(ShellExt64FileSource)}')
Source: "{#PuttySourceDir}\LICENCE"; DestDir: "{app}\PuTTY"; \
  Components: pageant puttygen; Flags: ignoreversion
Source: "{#PuttySourceDir}\putty.chm"; DestDir: "{app}\PuTTY"; \
  Components: pageant puttygen; Flags: ignoreversion
Source: "{#PuttySourceDir}\pageant.exe"; DestDir: "{app}\PuTTY"; \
  Components: pageant; Flags: ignoreversion
Source: "{#PuttySourceDir}\puttygen.exe"; DestDir: "{app}\PuTTY"; \
  Components: puttygen; Flags: ignoreversion
#ifdef ExtensionsDir
Source: "{#ExtensionsDir}\*.*"; DestDir: "{app}\Extensions"
#endif
#ifdef Sponsor
Source: "{#Sponsor}\*.*"; Flags: dontcopy skipifsourcedoesntexist

  #define SponsorImages
  #if FindHandle = FindFirst(Sponsor + "\*.*", 0)
    #define FResult 1
    #for {0; FResult; FResult = FindNext(FindHandle)} SponsorImages = SponsorImages + FindGetFileName(FindHandle) + ","
    #expr FindClose(FindHandle)
    #expr SponsorImages = Copy(SponsorImages, 1, Len(SponsorImages) - 1)
  #endif
#endif

[Registry]
Root: HKCU; Subkey: "{#ParentRegistryKey}"; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: "{#RegistryKey}"; Flags: uninsdeletekeyifempty
; Norton Commander interface
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; \
  ValueName: "Interface"; ValueData: 0; Check: UserSettings(1)
Root: HKLM; SubKey: "{#RegistryKey}"; ValueType: dword; \
  ValueName: "DefaultInterfaceInterface"; ValueData: 0; \
  Check: UserSettings(1) and IsAdminInstallMode; Flags: noerror
; Explorer-like interface
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; \
  ValueName: "Interface"; ValueData: 1; Check: not UserSettings(1)
Root: HKLM; SubKey: "{#RegistryKey}"; ValueType: dword; \
  ValueName: "DefaultInterfaceInterface"; ValueData: 1; \
  Check: (not UserSettings(1)) and IsAdminInstallMode; Flags: noerror
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
  Tasks: enableupdates; Flags: noerror; Check: IsAdminInstallMode
Root: HKLM; SubKey: "{#RegistryKey}"; \
  ValueType: dword; ValueName: "DefaultCollectUsage"; ValueData: 1; \
  Tasks: enableupdates\enablecollectusage; Flags: noerror; Check: IsAdminInstallMode

#sub EmitLang

  #if Languages[LangI*4+3] >= InclusionThreshold

[Files]
Source: "{#TranslationDir}\WinSCP.{#Languages[LangI*4]}"; DestDir: "{app}\Translations"; \
  Components: transl; Flags: ignoreversion

  #endif

#endsub /* sub EmitLang */

#for {LangI = 0; LangI < LanguageCount; LangI++} EmitLang

; Delete translations from installation root folder (pre-5.10)
[InstallDelete]
#expr AllTranslationsBuf = AllTranslations + '-'

#sub DeleteRootTranslation
  #define P Pos('-', AllTranslationsBuf)
  #define Lang Copy(AllTranslationsBuf, 1, P - 1)
  #expr AllTranslationsBuf = Copy(AllTranslationsBuf, P + 1)
Type: files; Name: "{app}\WinSCP.{#Lang}"
#endsub

#for { 0; Len(AllTranslationsBuf) > 0; 0 } DeleteRootTranslation

[UninstallRun]
; Make sure no later uninstall task recreate the configuration
Filename: "{app}\WinSCP.exe"; Parameters: "{code:GetTaskCmdLine|UninstallCleanup}"; \
  RunOnceId: "UninstallCleanup"
Filename: "{app}\WinSCP.exe"; Parameters: "{code:GetTaskCmdLine|RemoveSearchPath}"; \
  RunOnceId: "RemoveSearchPath"
Filename: "{app}\WinSCP.exe"; Parameters: "{code:GetTaskCmdLine|UnregisterForProtocols}"; \
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
  PrevVersion: string;
  ShellExtNewerCacheFileName: string;
  ShellExtNewerCacheResult: Boolean;
  ShellExtNoRestart: Boolean;
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
#ifdef Sponsor
  SponsorReq: Variant;
  SponsorPage: TWizardPage;
  Sponsor: string;
  SponsorStatus: string;
#endif

procedure ShowMessage(Text: string);
begin
  MsgBox(Text, mbInformation, MB_OK);
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

function ShouldInstallShellExt(FileName: string; InstalledVersion: string): Boolean;
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
    // Keeping ShellExtNoRestart value
  end
    else
  if not FileExists(FileName) then
  begin
    Log(Format('Shell extension %s does not exist yet, allowing installation', [FileName]));
    ShellExtNoRestart := False;
    Result := True;
  end
    else
  if not GetVersionNumbers(FileName, ExistingMS, ExistingLS) then
  begin
    Log(Format('Cannot retrieve version of existing shell extension %s, allowing installation', [FileName]));
    ShellExtNoRestart := False;
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

    if (InstalledMajor <> ExistingMajor) or
       ((ExistingMajor = 1) and (ExistingMinor <= 1)) then
    begin
      if InstalledMajor <> ExistingMajor then
      begin
        Log('Existing extension has different major version, allowing installation, and will require restart, if it is locked.')
      end
        else
      begin
        // 1.1 uses Ansi encoding, and is incompatible with 1.2 and newer which uses Unicode
        Log('Existing extension is 1.1 or older, allowing installation, and will require restart, if it is locked.');
      end;

      Result := True;
      ShellExtNoRestart := False;
    end
      else
    if (InstalledMinor > ExistingMinor) or
       ((InstalledMinor = ExistingMinor) and (InstalledRev > ExistingRev)) then
    begin
      if IsAdminInstallMode then
      begin
        Log('Installed extension is newer than existing extension, but major version is the same, allowing installation, but we will delay replacing the extension until the next system start, if it is locked.');
        Result := True;
        ShellExtNoRestart := True;
      end
        else
      begin
        Log('Installed extension is newer than existing extension, but major version is the same, and installer does not have administrator privileges, so delayed replacement is not possible, skipping installation (extension will be upgraded only with the next major release)');
        ShellExtNoRestart := False;
        Result := False;
      end;
    end
      else
    begin
      Log('Installed extension is same or older than existing extension (but the same major version), skipping installation');
      ShellExtNoRestart := False;
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

function IsRestartPage: Boolean;
begin
  Result := WizardForm.YesRadio.Visible;
end;

#ifdef Sponsor
var
  SponsoringClicked: Boolean;

procedure SponsoringLinkLabelClick(Sender: TObject);
begin
  SponsoringClicked := True;
  OpenBrowser('{#WebReport}?mode=sponsoring' + Format('&sponsor=%s&', [Sponsor]) + ExpandConstant('{#WebArguments}'));
end;
#endif

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

    wpFinished:
      HelpKeyword := 'ui_installer_finished';

    SetupTypePage.ID:
      HelpKeyword := 'ui_installer_setuptype';

    InterfacePage.ID:
      HelpKeyword := 'ui_installer_interface';

#ifdef Sponsor
    SponsorPage.ID:
      begin
        SponsoringLinkLabelClick(nil);
        Exit;
      end;
#endif
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

procedure LoadBitmap(Image: TBitmapImage; FileName: string);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create();
  Bitmap.AlphaFormat := afDefined;
  Bitmap.LoadFromFile(FileName);
  Image.Bitmap := Bitmap;
  Bitmap.Free;
end;

procedure LoadEmbededBitmap(Image: TBitmapImage; Name: string);
var
  FileName: string;
begin
  ExtractTemporaryFile(Name);
  FileName := ExpandConstant('{tmp}\' + Name);
  LoadBitmap(Image, FileName);
  // we won't need this anymore
  DeleteFile(FileName);
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

procedure LoadEmbededScaledIcon(Image: TBitmapImage; NameBase: string; SizeBase: Integer);
var
  Name: String;
begin
  Name := Format('%s %d.bmp', [NameBase, SizeBase * GetScalingFactor div 100]);
  LoadEmbededBitmap(Image, Name);
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

function GetRight(Control: TControl): Integer;
begin
  Result := Control.Left + Control.Width;
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

function MsiEnumRelatedProducts(
  lpUpgradeCode: string; dwReserved, iProductIndex: DWORD; lpProductBuf: string): UINT;
  external 'MsiEnumRelatedProductsW@msi.dll stdcall delayload setuponly';

function MsiGetProductInfo(szProduct, szAttribute, lpValueBuf: string; var pcchValueBuf: DWORD): UINT;
  external 'MsiGetProductInfoW@msi.dll stdcall delayload setuponly';

const
  ERROR_SUCCESS = 0;
  ERROR_MORE_DATA = 234;
  ERROR_NO_MORE_ITEMS = 259;

function GetProductInfo(ProductCode, Attribute: string): string;
var
  BufSize: DWORD;
  ErrorCode: Integer;
begin
  BufSize := 256;
  SetLength(Result, BufSize);
  ErrorCode := MsiGetProductInfo(ProductCode, Attribute, Result, BufSize);
  if ErrorCode = ERROR_MORE_DATA then
  begin
    Inc(BufSize);
    SetLength(Result, BufSize);
    ErrorCode := MsiGetProductInfo(ProductCode, Attribute, Result, BufSize);
  end;

  if ErrorCode <> ERROR_SUCCESS then
  begin
    Log(Format('Error %d reading MSI installation %s: %s', [ErrorCode, Attribute, SysErrorMessage(ErrorCode)]));
    Result := '';
  end
    else
  begin
    SetLength(Result, BufSize);
    Log(Format('MSI installation %s: %s [%d]', [Attribute, Result, Integer(BufSize)]));
  end;
end;

function InitializeSetup: Boolean;
var
  WaitInterval: Integer;
  Wait: Integer;
  ProductCode, VersionString: string;
  ErrorCode: Integer;
begin
  Log('Initializing...');
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

  Log('Checking for mutexes...');
  WaitInterval := 250;
  while (Wait > 0) and CheckForMutexes('{#AppMutex}') do
  begin
    Log('Application is still running, waiting...');
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

  if CmdLineParamExists('/OverrideMsi') then
  begin
    Log('Skipping MSI installation check');
  end
    else
  begin
    Log('Checking MSI installation...');
    try
      SetLength(ProductCode, 38);
      ErrorCode := MsiEnumRelatedProducts('{029F9450-CFEF-4408-A2BB-B69ECE29EB18}', 0, 0, ProductCode);
      if ErrorCode <> ERROR_SUCCESS then
      begin
        if ErrorCode = ERROR_NO_MORE_ITEMS then
        begin
          Log('MSI installation not detected');
        end
          else
        begin
          Log(Format('Error %d detecting MSI installation: %s', [ErrorCode, SysErrorMessage(ErrorCode)]));
        end;
      end
        else
      begin
        Log('Product code: "' + ProductCode + '"');
        GetProductInfo(ProductCode, 'InstalledProductName');
        GetProductInfo(ProductCode, 'InstallDate');
        GetProductInfo(ProductCode, 'InstallLocation');
        GetProductInfo(ProductCode, 'Publisher');
        VersionString := GetProductInfo(ProductCode, 'VersionString');
        GetProductInfo(ProductCode, 'VersionMajor');
        GetProductInfo(ProductCode, 'VersionMinor');
        if VersionString = '' then
        begin
          Log('Corrupted MSI installation, proceeding...');
        end
          else
        begin
          MsgBox(CustomMessage('MsiInstallation'), mbError, MB_OK);
          Result := False;
        end;
      end;
    except
      Log('Error checking MSI installations: ' + GetExceptionMessage);
    end;
  end;
end;

// Keep in sync with similar function on Preferences dialog
function Bullet(S: string): string;
begin
  if Copy(S, 1, 1) = '-' then S := #$2022'  ' + Trim(Copy(S, 2, Length(S) - 1));
  Result := S;
end;

procedure InitializeWizard;
var
  UserInterface: Cardinal;
  UpdatesPeriod: Cardinal;
  Caption: TLabel;
#ifdef ImagesDir
  Image: TBitmapImage;
#endif
  HelpButton: TButton;
#ifdef Donations
  P: Integer;
#ifdef ImagesDir
  P2: Integer;
#endif
#endif
  S: string;
  Completeness: Integer;
begin
  InstallationDone := False;
  LicenseAccepted := False;
  InitInterface := -1;

  Upgrade :=
    // We may want to use HKA to work correctly with side-by-side installations.
    // But as Updade is really used for changing REGISTRY configuration options only,
    // which are shared between all-users and current-user installations, it does not really matter.
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

  WizardForm.KeyPreview := True;
  WizardForm.OnKeyDown := @FormKeyDown;

  // allow installation without requiring user to accept license
  WizardForm.LicenseAcceptedRadio.Checked := True;
  WizardForm.LicenseAcceptedRadio.Visible := False;
  WizardForm.LicenseNotAcceptedRadio.Visible := False;
  WizardForm.LicenseMemo.Height :=
    GetBottom(WizardForm.LicenseNotAcceptedRadio) -
    WizardForm.LicenseMemo.Top - ScaleY(5);

  // hide installation types combo
  WizardForm.TypesCombo.Visible := False;
  WizardForm.ComponentsList.Height :=
    GetBottom(WizardForm.ComponentsList) -
    WizardForm.TypesCombo.Top;
  WizardForm.ComponentsList.Top := WizardForm.TypesCombo.Top;

  // add help button
  HelpButton := TButton.Create(WizardForm);
  HelpButton.Parent := WizardForm;
  HelpButton.Anchors := [akLeft, akBottom];
  HelpButton.Left := WizardForm.ClientWidth - GetRight(WizardForm.CancelButton);
  HelpButton.Top := WizardForm.CancelButton.Top;
  HelpButton.Width := WizardForm.CancelButton.Width;
  HelpButton.Height := WizardForm.CancelButton.Height;
  HelpButton.Caption := CustomMessage('HelpButton');
  HelpButton.OnClick := @HelpButtonClick;

  Completeness := LanguageCompleteness(ActiveLanguage);
  if (Completeness < 100) and (not WizardSilent) then
  begin
    ShowMessage(FmtMessage(CustomMessage('IncompleteTranslation'), [IntToStr(Completeness)]));
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
    Caption.Caption :=
      Bullet(CustomMessage('TypicalType1')) + NewLine +
      Bullet(CustomMessage('TypicalType2')) + NewLine +
      Bullet(CustomMessage('TypicalType3'));
  end
    else
  begin
    Caption.Caption :=
      Bullet(CustomMessage('TypicalUpgradeType1'));
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
      Bullet(CustomMessage('CustomType1'));
  end
    else
  begin
    Caption.Caption :=
      Bullet(CustomMessage('CustomUpgradeType1')) + NewLine +
      Bullet(CustomMessage('CustomUpgradeType2'));
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

#ifdef ImagesDir
  Image := TBitmapImage.Create(InterfacePage);
  Image.Top := GetBottom(CommanderRadioButton) + ScaleY(6);
  Image.Left := CommanderRadioButton.Left + ScaleX(45);
  Image.Parent := InterfacePage.Surface;
  LoadEmbededScaledIcon(Image, '{#CommanderFileBase}', 32);
  Image.OnClick := @ImageClick;
  Image.Tag := Integer(CommanderRadioButton);
#endif

  Caption := TLabel.Create(InterfacePage);
  Caption.WordWrap := True;
  Caption.Caption :=
      Bullet(CustomMessage('NortonCommanderInterface1')) + NewLine +
      Bullet(CustomMessage('NortonCommanderInterface2')) + NewLine +
      Bullet(CustomMessage('NortonCommanderInterface3'));
  Caption.Anchors := [akLeft, akTop, akRight];
  Caption.Left := GetRight(CommanderRadioButton);
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

#ifdef ImagesDir
  Image := TBitmapImage.Create(InterfacePage);
  Image.Top := GetBottom(ExplorerRadioButton) + ScaleY(6);
  Image.Left := ExplorerRadioButton.Left + ScaleX(45);
  Image.Parent := InterfacePage.Surface;
  LoadEmbededScaledIcon(Image, '{#ExplorerFileBase}', 32);
  Image.OnClick := @ImageClick;
  Image.Tag := Integer(ExplorerRadioButton);
#endif

  Caption := TLabel.Create(InterfacePage);
  Caption.WordWrap := True;
  Caption.Caption :=
      Bullet(CustomMessage('ExplorerInterface1')) + NewLine +
      Bullet(CustomMessage('ExplorerInterface2')) + NewLine +
      Bullet(CustomMessage('ExplorerInterface3'));
  Caption.Anchors := [akLeft, akTop, akRight];
  Caption.Left := GetRight(ExplorerRadioButton);
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
  Caption.Anchors := [akLeft, akTop, akRight];
  Caption.Left := 0;
  Caption.Top := 0;
  Caption.Width := DonationPanel.Width;
  Caption.Parent := DonationPanel;

  P := GetBottom(Caption) + ScaleY(12);
#ifdef ImagesDir
  P2 := P;
#endif

  CreateDonateLink(19, P);
  CreateDonateLink(29, P);
  CreateDonateLink(49, P);

  AboutDonationCaption := TLabel.Create(DonationPanel);
  AboutDonationCaption.Left := 0;
  AboutDonationCaption.Top := P;
  AboutDonationCaption.Parent := DonationPanel;
  AboutDonationCaption.Caption := CustomMessage('AboutDonations');
  AboutDonationCaption.OnClick := @AboutDonationsLinkClick;
  LinkLabel(AboutDonationCaption);

#ifdef ImagesDir
  Image := TBitmapImage.Create(DonationPanel);
  LoadEmbededBitmap(Image, '{#PayPalCardImage}');
  Image.BackColor := DonationPanel.Color;
  Image.AutoSize := True;
  Image.Cursor := crHand;
  Image.Parent := DonationPanel;
  Image.Left := ScaleX(108);
  Image.Top := P2 + ScaleX(8);
  Image.Hint := CustomMessage('AboutDonations');
  Image.ShowHint := True;
  Image.OnClick := @AboutDonationsLinkClick;
#endif

  DonationPanel.Height := GetBottom(AboutDonationCaption);

#endif

  WizardForm.YesRadio.OnClick := @UpdatePostInstallRunCheckboxes;
  WizardForm.NoRadio.OnClick := @UpdatePostInstallRunCheckboxes;
  UpdatePostInstallRunCheckboxes(nil);

#ifdef ImagesDir
  // Text does not scale as quick as with DPI,
  // so the icon may overlap the labels. Shift them.
  P := WizardForm.SelectDirBitmapImage.Width;
  LoadEmbededScaledIcon(WizardForm.SelectDirBitmapImage, '{#SelectDirFileBase}', 32);
  P := (WizardForm.SelectDirBitmapImage.Width - P);
  // Vertical change should be the same as horizontal
  WizardForm.SelectDirLabel.Left := WizardForm.SelectDirLabel.Left + P;
  WizardForm.SelectDirBrowseLabel.Top := WizardForm.SelectDirBrowseLabel.Top + P;
  WizardForm.DirEdit.Top := WizardForm.DirEdit.Top + P;
  WizardForm.DirBrowseButton.Top := WizardForm.DirBrowseButton.Top + P;
#endif
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
  S: string;
#ifdef Sponsor
  SponsorQueryUrl: string;
  PreferredSponsor: string;
#endif
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
    // Note that it's not possible to get to the "finished" page more than once,
    // so the code below does not expect re-entry
    if IsRestartPage then
    begin
      if ShellExtNoRestart then
      begin
        Log('Hiding restart page as it''s not critical to replace the shell extension');
        WizardForm.YesRadio.Visible := False;
        WizardForm.NoRadio.Visible := False;
        WizardForm.NoRadio.Checked := True;

        S := SetupMessage(msgFinishedLabel);
        StringChange(S, '[name]', 'WinSCP');
        WizardForm.FinishedLabel.Caption :=
          S + NewLine + NewLine +
          // The additional new line is a padding for the "launch check box",
          // as the same padding is there for the YesRadio too.
          SetupMessage(msgClickFinish) + NewLine;
        Log(WizardForm.FinishedLabel.Caption);
        Delta := WizardForm.AdjustLabelHeight(WizardForm.FinishedLabel);
        LaunchCheckboxTop := WizardForm.YesRadio.Top + Delta;
      end
        else
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
      end;
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
      // Should not happen anymore with "modern" style of IS6.
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

#ifdef Sponsor
    if VarIsEmpty(SponsorReq) then
    begin
      if WizardSilent or CmdLineParamExists('/NoSponsor') then
      begin
        Log('Skipping sponsor query request');
        SponsorStatus := 'N';
      end
        else
      begin
        SponsorPage :=
          CreateCustomPage(wpInstalling, 'Release sponsor', 'Please read a message from the sponsor of this release.');

        SponsorQueryUrl :=
          '{#WebReport}?' +
          Format('mode=sponsorrequest&ver=%s&lang=%s&prevver=%s&scale=%d&images=%s', [
            '{#VersionOnly}', ActiveLanguage, PrevVersion, GetScalingFactor, '{#SponsorImages}']);
        PreferredSponsor := ExpandConstant('{param:Sponsor}');
        if PreferredSponsor <> '' then
        begin
          SponsorQueryUrl := SponsorQueryUrl + Format('&sponsor=%s', [PreferredSponsor]);
        end;

        Log('Sending sponsor query request: ' + SponsorQueryUrl);

        SponsorReq := CreateOleObject('WinHttp.WinHttpRequest.5.1');

        SponsorReq.Open('GET', SponsorQueryUrl, True);
        SponsorReq.Send('');
      end;
    end;
#endif
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
      'mode=report&installed=%d&silent=%d&ver=%s&lang=%s&prevver=%s&', [
       Integer(InstallationDone), Integer(WizardSilent),
       '{#VersionOnly}', ActiveLanguage,
       PrevVersion]);
#ifdef Sponsor
    ReportData := ReportData +
      Format('sponsorstatus=%s&sponsor=%s&sponsoringclicked=%d&', [SponsorStatus, Sponsor, Integer(SponsoringClicked)]);
#endif

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

function GetTaskLogCmdLine(Task: string): string;
var
  LogPath: string;
  Ext: string;
begin
  if CmdLineParamExists('/LogTasks') then
  begin
    LogPath := Trim(ExpandConstant('{param:Log}'));
    if LogPath <> '' then
    begin
      Ext := ExtractFileExt(LogPath);
      LogPath := Copy(LogPath, 1, Length(LogPath) - Length(Ext)) + '.' + Task + Ext;
      Result := ' /AppLog="' + LogPath + '"';
    end;
  end;
end;

procedure ExecApp(Params: string; ShowCmd: Integer; Wait: TExecWait; Task: string);
var
  Path: string;
  ErrorCode: Integer;
begin
  Path := ExpandConstant('{app}\{#MainFileName}');
  Params := Params + GetTaskLogCmdLine(Task);
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

      UsageData := UsageData + 'InstallationsUser+,InstallationParentProcess@,';

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

      UsageData := UsageData + Format('LastInstallationAutomaticUpgrade:%d,', [Integer(AutomaticUpdate)]);

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
      if not IsAdminInstallMode then
        UsageData := UsageData + 'InstallationsNonElevated+,';

      // have to do this before running WinSCP GUI instance below,
      // otherwise it loads the empty/previous counters and overwrites our changes,
      // when it's closed
      Log('Recording installer usage statistics: ' + UsageData);
      // make sure we write the counters using the "normal" account
      // (the account that will be used to report the counters)
      ExecApp(UsageData, SW_HIDE, ewWaitUntilTerminated, 'Usage');

      if AutomaticUpdate then
      begin
        Log('Launching WinSCP after automatic update');
        ExecApp('', SW_SHOWNORMAL, ewNoWait, 'AutomaticUpdateRun');

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
          ExecApp('', ShowCmd, ewNoWait, 'Run');
        end;
      end;
    end;
  end;
end;

#ifdef Sponsor
function CryptStringToBinary(
  sz: string; cch: LongWord; flags: LongWord; binary: string; var size: LongWord;
  skip: LongWord; flagsused: LongWord): Integer;
  external 'CryptStringToBinaryW@crypt32.dll stdcall';

const
  CRYPT_STRING_HEX = $04;
  SHCONTCH_NOPROGRESSBOX = 4;
  SHCONTCH_RESPONDYESTOALL = 16;

var
  ShowSponsor: Integer;
  SponsoringLinkLabel, SponsorLinkLabel: TLabel;
  SponsorImage: TBitmapImage;

procedure SponsorImageClick(Sender: TObject);
begin
  SponsorStatus := 'C';
  OpenBrowser('{#WebReport}?mode=sponsor' + Format('&sponsor=%s&', [Sponsor]) + ExpandConstant('{#WebArguments}'));
end;

function GetSponsorAreaHeight: Integer;
begin
  Result := SponsorPage.SurfaceHeight - SponsorLinkLabel.Height - ScaleY(12);
end;

procedure CenterSponsorImage;
begin
  if (Extended(SponsorImage.Bitmap.Width) / SponsorPage.SurfaceWidth) <
       (Extended(SponsorImage.Bitmap.Height) / GetSponsorAreaHeight())  then
  begin
    SponsorImage.Top := 0;
    SponsorImage.Height := GetSponsorAreaHeight();
    SponsorImage.Width :=
      Trunc((Extended(SponsorImage.Bitmap.Width) / SponsorImage.Bitmap.Height) * SponsorImage.Height);
    SponsorImage.Left := (SponsorPage.SurfaceWidth - SponsorImage.Width) div 2;
  end
    else
  begin
    SponsorImage.Left := 0;
    SponsorImage.Width := SponsorPage.SurfaceWidth;
    SponsorImage.Height :=
      Trunc((Extended(SponsorImage.Bitmap.Height) / SponsorImage.Bitmap.Width) * SponsorImage.Width);
    SponsorImage.Top := (GetSponsorAreaHeight() - SponsorImage.Height) div 2;
  end;
  SponsorLinkLabel.Left := SponsorImage.Left;
  SponsorLinkLabel.Top := GetBottom(SponsorImage) + ScaleX(6);
  SponsoringLinkLabel.Left := GetRight(SponsorImage) - SponsoringLinkLabel.Width;
  SponsoringLinkLabel.Top := SponsorLinkLabel.Top;
end;

procedure WizardFormResize(Sender: TObject);
begin
  CenterSponsorImage;
end;

function CheckSponsorReq: Boolean;
var
  R, Succeeded: Integer;
  Lines: TStrings;
  I, P: Integer;
  L, Key, Value: string;
  Stream: TStream;
  Buffer: string;
  Size: LongWord;
  ZipPath, TargetPath, ImagePath: string;
  Shell, ZipFile, TargetFolder: Variant;
  SponsorArea: TBitmapImage;
  ImageSize: Integer;
begin
  if ShowSponsor = 0 then
  begin
    SponsorImage := nil;

    Log('Checking for response to sponsor request');
    try
      Succeeded := 0;
      // Not testing return value, as it always returns -1 for some reason
      R := SponsorReq.WaitForResponse(1, Succeeded);
      if R = 0 then
      begin
        Log('Timed out waiting for a response to sponsor request');
        SponsorStatus := 'T';
        ShowSponsor := -1;
      end
        else
      if SponsorReq.Status <> 200 then
      begin
        Log('Sponsor request failed with HTTP error: ' + IntToStr(SponsorReq.Status) + ' ' + SponsorReq.StatusText);
        SponsorStatus := 'H';
        ShowSponsor := -1;
      end
        else
      begin
        Log('Sponsor request succeeded');

        if CmdLineParamExists('/SponsorArea') then
        begin
          SponsorArea := TBitmapImage.Create(SponsorPage);
          SponsorArea.Parent := SponsorPage.Surface;
          SponsorArea.Visible := CmdLineParamExists('/SponsorArea');
          SponsorArea.BackColor := clTeal;
          SponsorArea.Anchors := [akLeft, akTop, akRight, akBottom];
        end
          else
        begin
          SponsorArea := nil;
        end;

        SponsorLinkLabel := TLabel.Create(SponsorPage);
        SponsorLinkLabel.Parent := SponsorPage.Surface;
        SponsorLinkLabel.Caption := 'Visit release sponsor';
        SponsorLinkLabel.OnClick := @SponsorImageClick;
        LinkLabel(SponsorLinkLabel);

        SponsoringLinkLabel := TLabel.Create(SponsorPage);
        SponsoringLinkLabel.Parent := SponsorPage.Surface;
        SponsoringLinkLabel.Caption := 'Become next release sponsor';
        SponsoringLinkLabel.OnClick := @SponsoringLinkLabelClick;
        LinkLabel(SponsoringLinkLabel);

        Lines := TStringList.Create;
        try
          Lines.Text := SponsorReq.ResponseText;
          for I := 0 to Lines.Count - 1 do
          begin
            L := Lines[I];
            P := Pos('=', L);
            if P = 0 then
            begin
              Log('Malformed sponsor response directive: ' + L);
              SponsorStatus := 'P';
              ShowSponsor := -1;
              break;
            end
              else
            begin
              Key := Trim(Copy(L, 1, P - 1));
              Value := Trim(Copy(L, P + 1, Length(L) - P));

              if CompareText(Key, 'result') = 0 then
              begin
                if Value <> '-' then
                begin
                  Log('No sponsor returned');
                  SponsorStatus := Copy(Value, 1, 1);
                  ShowSponsor := -1;
                  break;
                end
                  else
                begin
                  Log('Sponsor returned');
                end;
              end
                else
              if (CompareText(Key, 'image') = 0) or
                 (CompareText(Key, 'localimage') = 0) then
              begin
                if CompareText(Key, 'localimage') = 0 then
                begin
                  Log(Format('Extracting embedded sponsor image (%s)', [Value]));
                  ExtractTemporaryFile(Value);
                  ImagePath := ExpandConstant('{tmp}\' + Value);
                end
                  else
                begin
                  Log(Format('Extracting returned sponsor image (%d bytes)', [Length(Value)]));

                  ZipPath := ExpandConstant('{tmp}\sponsor.zip');
                  Stream := TFileStream.Create(ZipPath, fmCreate);
                  try
                    SetLength(Buffer, (Length(Value) div 4) + 1);
                    Size := Length(Value) div 2;
                    if (CryptStringToBinary(Value, Length(Value), CRYPT_STRING_HEX, Buffer, Size, 0, 0) = 0) or
                       (Size <> Length(Value) div 2) then
                    begin
                      Log('Error decoding binary string');
                      SponsorStatus := 'P';
                      ShowSponsor := -1;
                      break;
                    end;

                    Stream.WriteBuffer(Buffer, Size);
                  finally
                    Stream.Free;
                  end;

                  Shell := CreateOleObject('Shell.Application');
                  ZipFile := Shell.NameSpace(ZipPath);
                  if VarIsClear(ZipFile) then
                  begin
                    RaiseException(Format('ZIP file "%s" does not exist or cannot be opened', [ZipPath]));
                  end
                    else
                  begin
                    TargetPath := ExpandConstant('{tmp}');
                    TargetFolder := Shell.NameSpace(TargetPath);
                    if VarIsClear(TargetFolder) then
                    begin
                      RaiseException(Format('Target path "%s" does not exist', [TargetPath]));
                    end
                      else
                    begin
                      TargetFolder.CopyHere(ZipFile.Items, SHCONTCH_NOPROGRESSBOX or SHCONTCH_RESPONDYESTOALL);

                      ImagePath := ExpandConstant('{tmp}\sponsor.bmp');
                    end;
                  end;
                end;

                SponsorImage := TBitmapImage.Create(SponsorPage);
                SponsorImage.Parent := SponsorPage.Surface;
                SponsorImage.Hint := SponsorLinkLabel.Caption;
                SponsorImage.ShowHint := True;
                SponsorImage.Stretch := True;
                try
                  LoadBitmap(SponsorImage, ImagePath);
                except
                  Log('Error loading sponsor image: ' + GetExceptionMessage);
                  SponsorStatus := 'I';
                  ShowSponsor := -1;
                end;

                if ShowSponsor = 0 then
                begin
                  if Assigned(SponsorArea) then
                  begin
                    SponsorArea.Left := 0;
                    SponsorArea.Top := 0;
                    SponsorArea.Width := SponsorPage.Surface.Width;
                    SponsorArea.Height := GetSponsorAreaHeight();
                    Log(Format('Sponsor area is %dx%d', [SponsorArea.Width, SponsorArea.Height]));
                  end;

                  CenterSponsorImage;

                  SponsorImage.Cursor := crHand;
                  SponsorImage.OnClick := @SponsorImageClick;

                  WizardForm.OnResize := @WizardFormResize;

                  FileSize(ImagePath, ImageSize);
                  Log(Format('Sponsor image loaded (%d bytes, %dx%d) and displayed (%dx%d)', [
                    Integer(ImageSize), SponsorImage.Bitmap.Width, SponsorImage.Bitmap.Height,
                    SponsorImage.Width, SponsorImage.Height]));
                end;
              end
                else
              if CompareText(Key, 'sponsor') = 0 then
              begin
                Sponsor := Value;
              end
                else
              if CompareText(Key, 'description') = 0 then
              begin
                SponsorPage.Description := Value;
                Log('Sponsor page description: ' + Value);
              end
                else
              if CompareText(Key, 'caption') = 0 then
              begin
                SponsorPage.Caption := Value;
                Log('Sponsor page caption: ' + Value);
              end
                else
              if CompareText(Key, 'sponsor_caption') = 0 then
              begin
                SponsorLinkLabel.Caption := Value;
                Log('Sponsor link caption: ' + Value);
              end
                else
              if CompareText(Key, 'sponsoring_caption') = 0 then
              begin
                if Value = '' then
                begin
                  SponsoringLinkLabel.Visible := False;
                  Log('Hiding sponsoring link');
                end
                  else
                begin
                  SponsoringLinkLabel.Caption := Value;
                  Log('Sponsoring link caption: ' + Value);
                end;
              end
                else
              begin
                Log('Unknown sponsor directive: ' + Key);
              end;
            end;
          end;
        finally
          Lines.Free;
        end;

        if ShowSponsor = 0 then
        begin
          if SponsorImage = nil then
          begin
            Log('Incomplete sponsor data');
            SponsorStatus := 'P';
            ShowSponsor := -1;
          end
            else
          begin
            SponsorStatus := 'S';
            ShowSponsor := 1;
          end;
        end;
      end;
    except
      Log('Error processing response to sponsor request: ' + GetExceptionMessage);
      SponsorStatus := 'E';
      ShowSponsor := -1;
    end;
  end;

  Result := (ShowSponsor > 0);
end;
#endif

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  Result :=
    { Hide most pages during typical installation }
    (IsTypicalInstallation and
     ((PageID = wpSelectDir) or (PageID = wpSelectComponents) or
      (PageID = wpSelectTasks) or
      { Hide Interface page for upgrades only, show for fresh installs }
      ((PageID = InterfacePage.ID) and Upgrade)))
#ifdef Sponsor
    or
    ((SponsorPage <> nil) and (PageID = SponsorPage.ID) and (not CheckSponsorReq))
#endif
    ;
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

function GetTaskCmdLine(Param: string): string;
begin
  Result := '/' + Param + GetTaskLogCmdLine(Param);
  Log(Format('Command-line for %s task: %s', [Param, Result]));
end;

#expr SaveToFile(AddBackslash(SourcePath) + "Preprocessed.iss")
