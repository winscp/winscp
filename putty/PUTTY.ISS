; -*- no -*-
; putty.iss
;
; -- Inno Setup installer script for PuTTY and its related tools.
;
; TODO for future releases:
;
;  - It would be neighbourly to set up an [UninstallRun] entry that ran
;    some close cousin of `putty -cleanup', only it should prompt first
;    in case the user wants to keep stuff. And make the `leave it alone'
;    button the DEFAULT. And perhaps warn that on NT-style systems not
;    everything will be caught by this.
;
;  - The Quick Launch bar is an interesting thought. Certainly a fair
;    number of people actually _believe_ my silly joke about how PuTTY
;    is the only thing that makes Windows usable, so perhaps they'd like
;    that. Unchecked by default, though, I think.
;     * does this need to be conditional on the Windows version?

[Setup]
AppName=PuTTY
AppVerName=PuTTY version 0.54
DefaultDirName={pf}\PuTTY
DefaultGroupName=PuTTY
UninstallDisplayIcon={app}\putty.exe
ChangesAssociations=yes
Compression=zip/9

[Files]
Source: "putty.exe"; DestDir: "{app}"
Source: "pageant.exe"; DestDir: "{app}"
Source: "puttygen.exe"; DestDir: "{app}"
Source: "pscp.exe"; DestDir: "{app}"
Source: "psftp.exe"; DestDir: "{app}"
Source: "plink.exe"; DestDir: "{app}"
Source: "website.url"; DestDir: "{app}"
Source: "doc\putty.hlp"; DestDir: "{app}"
Source: "doc\putty.cnt"; DestDir: "{app}"
Source: "LICENCE"; DestDir: "{app}"
Source: "README.txt"; DestDir: "{app}"; Flags: isreadme

[Icons]
Name: "{group}\PuTTY"; Filename: "{app}\putty.exe"; Tasks: startmenu
Name: "{group}\PuTTY Manual"; Filename: "{app}\putty.hlp"; Tasks: startmenu
Name: "{group}\PuTTY Web Site"; Filename: "{app}\website.url"; Tasks: startmenu
Name: "{group}\PSFTP"; Filename: "{app}\psftp.exe"; Tasks: startmenu
Name: "{group}\PuTTYgen"; Filename: "{app}\puttygen.exe"; Tasks: startmenu
Name: "{group}\Pageant"; Filename: "{app}\pageant.exe"; Tasks: startmenu
Name: "{userdesktop}\PuTTY"; Filename: "{app}\putty.exe"; Tasks: desktopicon

[Tasks]
Name: startmenu; Description: "Create a &Start Menu group"
Name: desktopicon; Description: "Create a &desktop icon for PuTTY"
Name: associate; Description: "&Associate .PPK files (PuTTY Private Key) with Pageant"

[Registry]
Root: HKCR; Subkey: ".ppk"; ValueType: string; ValueName: ""; ValueData: "PuTTYPrivateKey"; Flags: uninsdeletevalue; Tasks: associate
Root: HKCR; Subkey: "PuTTYPrivateKey"; ValueType: string; ValueName: ""; ValueData: "PuTTY Private Key File"; Flags: uninsdeletekey; Tasks: associate
Root: HKCR; Subkey: "PuTTYPrivateKey\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\pageant.exe,0"; Tasks: associate
Root: HKCR; Subkey: "PuTTYPrivateKey\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\pageant.exe"" ""%1"""; Tasks: associate
Root: HKCR; Subkey: "PuTTYPrivateKey\shell\edit"; ValueType: string; ValueName: ""; ValueData: "&Edit"; Tasks: associate
Root: HKCR; Subkey: "PuTTYPrivateKey\shell\edit\command"; ValueType: string; ValueName: ""; ValueData: """{app}\puttygen.exe"" ""%1"""; Tasks: associate
