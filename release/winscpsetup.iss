#define MainFileSource "..\WinSCP3.exe"
#define ParentRegistryKey "Software\Martin Prikryl"
#define RegistryKey ParentRegistryKey+"\WinSCP 2"

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
OutputBaseFilename=winscp{#Major}{#Minor}{#Rev}setup
Compression=bzip/9
ShowTasksTreeLines=yes

[Types]
Name: full; Description: "Full installation"
Name: compact; Description: "Compact installation"
Name: custom; Description: "Custom installation"; Flags: iscustom

[Components]
Name: main; Description: "WinSCP application"; Types: full custom compact; Flags: fixed
Name: pageant; Description: "PuTTYgen (key generator)"; Types: full
Name: puttygen; Description: "Pageant (SSH authentication agent)"; Types: full

[Tasks]
; Windows integration
Name: desktopicon; Description: "Create a &desktop icon"
Name: desktopicon\user; Description: "For the current user only"; Flags: exclusive
Name: desktopicon\common; Description: "For all users"; Flags: exclusive unchecked
Name: quicklaunchicon; Description: "Create a &Quick Launch icon"; Flags: unchecked
Name: sendtohook; Description: "Add upload shortcut to Explorer's 'Send to' context menu"

; User interface style
;Name: "commander"; Description: "Norton &Commander interface"; GroupDescription: "User interface style (can be changed later)"; Flags: exclusive
;Name: "explorer"; Description: "&Explorer-like interface"; GroupDescription: "User interface style (can be changed later)"; Flags: unchecked exclusive
; Additional options
;Name: "advancedtabs"; Description: "Show &advanced login options"; GroupDescription: "Additional options (can be changed later)"; Flags: unchecked

[Files]
Source: "{#MainFileSource}"; DestDir: "{app}"; Components: main; Flags: ignoreversion
Source: "licence"; DestName: "licence"; DestDir: "{app}"; Components: main; Flags: ignoreversion
Source: "C:\Program Files\PuTTY\LICENCE"; DestDir: "{app}\PuTTY"; Components: pageant puttygen; Flags: ignoreversion
Source: "C:\Program Files\PuTTY\putty.hlp"; DestDir: "{app}\PuTTY"; Components: pageant puttygen; Flags: ignoreversion
Source: "C:\Program Files\PuTTY\pageant.exe"; DestDir: "{app}\PuTTY"; Components: pageant; Flags: ignoreversion
Source: "C:\Program Files\PuTTY\puttygen.exe"; DestDir: "{app}\PuTTY"; Components: puttygen; Flags: ignoreversion

[INI]
Filename: "{app}\WinSCP.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://winscp.sourceforge.net/"
Filename: "{app}\Support forum.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://winscp.sourceforge.net/forum/"
Filename: "{app}\PuTTY\PuTTY.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://www.chiark.greenend.org.uk/~sgtatham/putty/"; Components: pageant puttygen

[Icons]
; This is created always (unless user checks Don't create a Start menu folder, Setup\AllowNoIcons=yes)
Name: "{group}\WinSCP"; Filename: "{app}\WinSCP3.exe"; Components: main
Name: "{group}\WinSCP Web Site"; Filename: "{app}\WinSCP.url"; Components: main
Name: "{group}\Support forum"; Filename: "{app}\Support forum.url"; Components: main
; This is created when pageant/puttygen component is selected (unless user checks Don't create a Start
; menu folder, Setup\AllowNoIcons=yes). Flag createonlyiffileexists is used instead of "Compomnents: xxx",
; because it would force creating the icons even when user doesn't want to create start menu folder.
Name: "{group}\RSA key tools\PuTTYgen"; Filename: "{app}\PuTTY\puttygen.exe"; Components: puttygen
Name: "{group}\RSA key tools\PuTTYgen Manual"; Filename: "winhlp32.exe"; Parameters: "-iputtygen.general {app}\PuTTY\putty.hlp"; Components: puttygen
Name: "{group}\RSA key tools\Pageant"; Filename: "{app}\PuTTY\pageant.exe"; Components: pageant
Name: "{group}\RSA key tools\Pageant Manual"; Filename: "winhlp32.exe"; Parameters: "-ipageant.general {app}\PuTTY\putty.hlp"; Components: pageant
Name: "{group}\RSA key tools\Using public keys for SSH authentication"; Filename: "winhlp32.exe"; Parameters: "-it00000112 {app}\PuTTY\putty.hlp"; Components: pageant puttygen
Name: "{group}\RSA key tools\PuTTY Web Site"; Filename: "{app}\PuTTY\PuTTY.url"; Components: pageant puttygen
; This is created when desktopicon task is selected
Name: "{userdesktop}\WinSCP3"; Filename: "{app}\WinSCP3.exe"; Tasks: desktopicon\user
Name: "{commondesktop}\WinSCP3"; Filename: "{app}\WinSCP3.exe"; Tasks: desktopicon\common
; This is created when quicklaunchicon task is selected
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\WinSCP3"; Filename: "{app}\WinSCP3.exe"; Tasks: quicklaunchicon
; This is created when sendtohook task is selected
Name: "{sendto}\WinSCP3 (upload using SFTP or SCP)"; Filename: "{app}\WinSCP3.exe"; Parameters: "/upload"; Tasks: sendtohook

[InstallDelete]
Type: files; Name: "{sendto}\WinSCP3 (upload using SCP).lnk"
Type: files; Name: "{sendto}\WinSCP3 (upload using SFTP or SCP).lnk"

[Run]
Filename: "{app}\WinSCP3.exe"; Description: "Launch &WinSCP"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
; These additional files are created by installer
Type: files; Name: "{app}\WinSCP.url"
Type: files; Name: "{app}\Support forum.url"
Type: files; Name: "{app}\PuTTY\PuTTY.url"; Components: pageant puttygen
; These additional files are created by application
Type: files; Name: "{app}\WinSCP3.ini"

[UninstallRun]
Filename: "{app}\WinSCP3.exe"; Parameters: "/RandomSeedFileCleanup"; RunOnceId: "RandomSeedFileCleanup"

[Registry]
Root: HKCU; Subkey: "{#ParentRegistryKey}"; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: "{#RegistryKey}"; Flags: uninsdeletekeyifempty
; Norton Commander interface
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; ValueName: "Interface"; ValueData: 0; Check: IsTrue(10)
; Explorer-like interface
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; ValueName: "Interface"; ValueData: 1; Check: IsTrue(11)
; Advanced tab on login dialog
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; ValueName: "ShowAdvancedLoginOptions"; ValueData: 0; Check: IsTrue(20)
Root: HKCU; SubKey: "{#RegistryKey}\Configuration\Interface"; ValueType: dword; ValueName: "ShowAdvancedLoginOptions"; ValueData: 1; Check: IsTrue(21)

[Code]
var
  UserInterface: Cardinal;
  AdvancedTabs: Cardinal;

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
    ScriptDlgPageSetCaption('Initial user settings');
    ScriptDlgPageSetSubCaption1(
      'Please, select your preferred user interface options.');
    ScriptDlgPageSetSubCaption2(
      'Note: All these options can be changed later.');

    OutputMsg('User interface style', False);

    Caption := TLabel.Create(WizardForm.ScriptDlgPanel);
    Caption.Caption := 'User interface style';
    Caption.Width := WizardForm.ScriptDlgPanel.Width;
    Caption.Parent := WizardForm.ScriptDlgPanel;

    CommanderRadioButton := TRadioButton.Create(WizardForm.ScriptDlgPanel);
    CommanderRadioButton.Caption := 'Norton &Commander interface';
    CommanderRadioButton.Checked := (UserInterface = 0);
    CommanderRadioButton.Width := WizardForm.ScriptDlgPanel.Width;
    CommanderRadioButton.Parent := WizardForm.ScriptDlgPanel;
    CommanderRadioButton.Top := Caption.Top + Caption.Height + 6;

    Caption2 := TLabel.Create(WizardForm.ScriptDlgPanel);
    Caption2.WordWrap := True;
    Caption2.Caption :=
        '- two panels (left for local directory, right for remote directory)'+#13#10+
        '- keyboard shortcuts like in Norton Commander (and other similar programs as Total Commander, Midnight Commander...)'+#13#10+
        '- drag && drop to/from both panels'+#13#10+
        '- synchronization';
    Caption2.Left := 20;
    Caption2.Width := WizardForm.ScriptDlgPanel.Width - Caption.Left;
    Caption2.Top := CommanderRadioButton.Top + CommanderRadioButton.Height + 6;
    Caption2.Parent := WizardForm.ScriptDlgPanel;

    ExplorerRadioButton := TRadioButton.Create(WizardForm.ScriptDlgPanel);
    ExplorerRadioButton.Caption := '&Explorer-like interface';
    ExplorerRadioButton.Checked := (UserInterface <> 0);
    ExplorerRadioButton.Width := WizardForm.ScriptDlgPanel.Width;
    ExplorerRadioButton.Parent := WizardForm.ScriptDlgPanel;
    ExplorerRadioButton.Top := Caption2.Top + Caption2.Height + 6;

    Caption2 := TLabel.Create(WizardForm.ScriptDlgPanel);
    Caption.WordWrap := True;
    Caption2.Caption :=
        '- only remote directory'+#13#10+
        '- keyboard shortcuts like in Windows Explorer'+#13#10+
        '- drag && drop';
    Caption2.Left := 20;
    Caption2.Width := WizardForm.ScriptDlgPanel.Width - Caption.Left;
    Caption2.Top := ExplorerRadioButton.Top + ExplorerRadioButton.Height + 6;
    Caption2.Parent := WizardForm.ScriptDlgPanel;

    Caption := TLabel.Create(WizardForm.ScriptDlgPanel);
    Caption.Caption := 'Additional options';
    Caption.Width := WizardForm.ScriptDlgPanel.Width;
    Caption.Parent := WizardForm.ScriptDlgPanel;
    Caption.Top := Caption2.Top + Caption2.Height + 10;

    AdvancedTabsCheckbox := TCheckbox.Create(WizardForm.ScriptDlgPanel);
    AdvancedTabsCheckbox.Caption := 'Show &advanced login options';
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
begin
  S := '';

  S := S + MemoDirInfo + NewLine + NewLine;

  S := S + MemoTypeInfo + NewLine + NewLine;

  S := S + MemoComponentsInfo + NewLine + NewLine;

  if Length(MemoGroupInfo) > 0 then
    S := S + MemoGroupInfo + NewLine + NewLine;

  if Length(MemoTasksInfo) > 0 then
    S := S + MemoTasksInfo + NewLine + NewLine;

  S := S + 'Initial user settings (can be changed later)' + NewLine;
  S := S + Space;
  if UserInterface = 0 then S := S + 'Norton Commander interface'
    else S := S + 'Explorer-like interface';
  S := S + NewLine;

  if AdvancedTabs <> 0 then
    S := S + Space + 'Show advanced login options' + NewLine;

  Result := S;
end;
