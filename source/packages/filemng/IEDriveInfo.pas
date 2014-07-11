unit IEDriveInfo;

{==================================================================

 Component TDriveInfo  /  Version 2.6  / January 2000
 ====================================================


    Description:
    ============
    Central drive management class. Provides information about all
    installed drives, for example wether drive is valid, disk is inserted
    displayname or volume size.

    Author:
    =======
    (c) Ingo Eckel 1999
    Sodener Weg 38
    65812 Bad Soden
    Germany

    For detailed documentation and history see the documentation in TDriveInfo.htm.

{==================================================================}

{Required compiler options:}
{$A+,B-,X+,H+,P+}

interface

uses
  Windows, Registry, SysUtils, Classes, ComCtrls, ShellApi, ShlObj, CommCtrl, Forms,
  BaseUtils;

const
  {Flags used by TDriveInfo.ReadDriveStatus and TDriveView.RefreshRootNodes:}
  dsValid = 0;        {checks only whether drive is still valid}
  dsImageIndex = 1;   {Fetch imageindex, if not allready fetched}
  dsSize = 2;         {Fetch disk size and serialnumber}
  dsDisplayName = 4;  {Fetch drives displayname}
  dsAll = dsImageIndex or dsSize or dsDisplayName;
  FirstDrive          = 'A';
  FirstFixedDrive     = 'C';
  LastDrive           = 'Z';
  FirstSpecialFolder  = CSIDL_DESKTOP;
  LastSpecialFolder   = CSIDL_PRINTHOOD;

type
  TDrive    = Char;
  PDriveInfoRec = ^TDriveInfoRec;
  TDriveInfoRec = record
    PIDL        : PItemIDList; {Fully qualyfied PIDL}
    Init        : Boolean;     {Drivestatus was updated once}
    Valid       : Boolean;     {Drivestatus is valid}
    DriveReady  : Boolean;     {Drive is ready}
    DriveType   : Integer;     {DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_CDROM, DRIVE_RAMDISK, DRIVE_REMOTE}
    DisplayName : string;      {Windows displayname}
    PrettyName  : string;      {Prettyfied displayname}
    DriveSerial : DWORD;       {Serial number of the drive}
    Size        : Int64;       {Drivesize}
    ImageIndex  : Integer;     {Drive imageIndex}
  end;

  TSpecialFolder = FirstSpecialFolder..LastSpecialFolder;
  PSpecialFolderRec = ^TSpecialFolderRec;
  TSpecialFolderRec = record
    Valid: Boolean;
    Location: string;
    DisplayName: string;
    ImageIndex: Integer;
    PIDL: PItemIDList;
  end;

  TDriveInfo = class(TObject)
  private
    FData: array[FirstDrive..LastDrive] of TDriveInfoRec;
    FNoDrives: DWORD;
    FDesktop: IShellFolder;
    FFolders: array[TSpecialFolder] of TSpecialFolderRec;
    function GetData(Drive: TDrive): PDriveInfoRec;
    function GetFolder(Folder: TSpecialFolder): PSpecialFolderRec;
    procedure ReadDriveBasicStatus(Drive: TDrive);
    procedure ResetDrive(Drive: TDrive);

  public
    property Data[Drive: TDrive]: PDriveInfoRec read GetData; default;
    property SpecialFolder[Folder: TSpecialFolder]: PSpecialFolderRec read GetFolder;

    function GetImageIndex(Drive: TDrive): Integer;
    function GetDisplayName(Drive: TDrive): string;
    function GetPrettyName(Drive: TDrive): string;
    function ReadDriveStatus(Drive: TDrive; Flags: Integer): Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Load;
  end;

function GetShellFileName(const Name: string): string; overload;
function GetShellFileName(PIDL: PItemIDList): string; overLoad;
function GetNetWorkName(Drive: Char): string;
function GetNetWorkConnected(Drive: Char): Boolean;

{Central drive information object instance of TDriveInfo}
var
  DriveInfo : TDriveInfo;

resourceString
  ErrorInvalidDrive = '%s is a invalid drive letter.';

implementation

uses
  Math, CompThread;

constructor TDriveInfo.Create;
begin
  inherited;

  Load;
end; {TDriveInfo.Create}

destructor TDriveInfo.Destroy;
var
  Drive: TDrive;
begin
  for Drive := FirstDrive to LastDrive do
    with FData[Drive] do
    begin
      SetLength(DisplayName, 0);
      SetLength(PrettyName, 0);
      // This causes access violation
      // FreePIDL(PIDL);
    end;
  inherited;
end; {TDriveInfo.Destroy}

function TDriveInfo.GetFolder(Folder: TSpecialFolder): PSpecialFolderRec;
var
  FileInfo: TShFileInfo;
  Path: PChar;
  Flags: Word;
begin
  Assert((Folder >= Low(FFolders)) and (Folder <= High(FFolders)));

  with FFolders[Folder] do
  begin
    if not Valid then
    begin
      SpecialFolderLocation(Folder, Location, PIDL);
      if Assigned(PIDL) then
      begin
        Path := PChar(PIDL);
        Flags := SHGFI_PIDL;
      end
        else
      begin
        Path := PChar(Location);
        Flags := 0;
      end;

      SHGetFileInfo(Path, 0, FileInfo, SizeOf(FileInfo),
        SHGFI_DISPLAYNAME or SHGFI_SYSICONINDEX or SHGFI_SMALLICON or Flags);
      ImageIndex := FileInfo.iIcon;
      DisplayName := FileInfo.szDisplayName;
      Valid := True;
    end;
  end;

  Result := @FFolders[Folder];
end;

procedure TDriveInfo.ReadDriveBasicStatus(Drive: TDrive);
begin
  with FData[Drive] do
  begin
    DriveType := Windows.GetDriveType(PChar(Drive + ':\'));
    Valid := not Bool((1 shl (Ord(Drive) - 65)) and FNoDrives) and
      (DriveType in [DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_CDROM, DRIVE_RAMDISK, DRIVE_REMOTE]);
  end;
end;

procedure TDriveInfo.ResetDrive(Drive: TDrive);
begin
  with FData[Drive] do
  begin
    DriveReady := False;
    DisplayName := '';
    PrettyName := '';
    DriveSerial := 0;
    Size := -1;
    ImageIndex := 0;
  end;
end;

procedure TDriveInfo.Load;
var
  Drive: TDrive;
  Reg: TRegistry;
  Folder: TSpecialFolder;
begin
  FNoDrives := 0;
  Reg := TRegistry.Create;
  try
    if Reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Policies\Explorer') Then
       Reg.ReadBinaryData('NoDrives', FNoDrives, SizeOf(FNoDrives));
  except
    try
      FNoDrives := Reg.ReadInteger('NoDrives');
    except
    end;
  end;
  Reg.Free;

  FDesktop := nil;

  for Drive := FirstDrive to LastDrive do
  begin
    with FData[Drive] do
    begin
      ReadDriveBasicStatus(Drive);
      Init := False;
      PIDL := nil;
      ResetDrive(Drive);
    end;
  end;

  for Folder := Low(FFolders) to High(FFolders) do
    FFolders[Folder].Valid := False;
end;

function TDriveInfo.GetImageIndex(Drive: TDrive): Integer;
begin
  if (Drive < FirstDrive) or (Drive > LastDrive) then
    raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  Result := 0;

  if FData[Drive].Valid then
  begin
    if FData[Drive].ImageIndex = 0 then
      ReadDriveStatus(Drive, dsImageIndex);
    Result := FData[Drive].ImageIndex;
  end;
end; {TDriveInfo.GetImageIndex}

function TDriveInfo.GetDisplayName(Drive: TDrive): string;
begin
  if (Drive < FirstDrive) or (Drive > LastDrive) then
    raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  Result := Drive + ':';

  if FData[Drive].Valid then
  begin
    if Length(FData[Drive].DisplayName) = 0 then
      ReadDriveStatus(Drive, dsDisplayName);
    Result := FData[Drive].DisplayName;
  end;
end; {TDriveInfo.GetDisplayname}

function TDriveInfo.GetPrettyName(Drive: TDrive): string;
begin
  if (Drive < FirstDrive) or (Drive > LastDrive) then
    raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  Result := Drive + ':';

  if FData[Drive].Valid then
  begin
    if Length(FData[Drive].PrettyName) = 0 then
      ReadDriveStatus(Drive, dsDisplayName);
    Result := FData[Drive].PrettyName;
  end;
end; {TDriveInfo.GetPrettyName}

function TDriveInfo.GetData(Drive: TDrive): PDriveInfoRec;
begin
  if not CharInSet(Upcase(Drive), ['A'..'Z']) then
    raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  Result := @FData[Upcase(Drive)];
end; {TDriveInfo.GetData}

type
  TParseDisplayNameThread = class(TCompThread)
  private
    FDesktop: IShellFolder;
    FDrive: TDrive;
    FPIDL: PItemIDList;

  protected
    procedure Execute; override;

  public
    constructor Create(Desktop: IShellFolder; Drive: TDrive);

    property PIDL: PItemIDList read FPIDL;
  end;

constructor TParseDisplayNameThread.Create(Desktop: IShellFolder; Drive: TDrive);
begin
  inherited Create(True);
  FDesktop := Desktop;
  FDrive := Drive;
end;

procedure TParseDisplayNameThread.Execute;
var
  Eaten: ULONG;
  ShAttr: ULONG;
begin
  ShAttr := 0;
  FDesktop.ParseDisplayName(Application.Handle, nil, PChar(FDrive + ':\'), Eaten, FPIDL, ShAttr);
end;

function TDriveInfo.ReadDriveStatus(Drive: TDrive; Flags: Integer): Boolean;
var
  ErrorMode: Word;
  FileInfo: TShFileInfo;
  DriveID: string;
  CPos: Integer;
  Thread: TParseDisplayNameThread;
  Eaten: ULONG;
  ShAttr: ULONG;
  MaxFileNameLength: DWORD;
  FileSystemFlags: DWORD;
begin
  if not Assigned(FDesktop) then
    SHGetDesktopFolder(FDesktop);

  Drive := Upcase(Drive);
  if (Drive < FirstDrive) or (Drive > LastDrive) then
    raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  with FData[Drive] do
  begin
    Init := True;
    ReadDriveBasicStatus(Drive);

    if Valid then
    begin
      if (not Assigned(PIDL)) and (Drive >= FirstFixedDrive) then
      begin
        if DriveType = DRIVE_REMOTE then
        begin
          Thread := TParseDisplayNameThread.Create(FDesktop, Drive);
          Thread.Resume;
          if Thread.WaitFor(2 * MSecsPerSec) then
          begin
            PIDL := Thread.PIDL;
            Thread.Free;
          end
            else
          begin
            // There's a chance for memory leak, if thread is terminated
            // between WaitFor() and this line
            Thread.FreeOnTerminate := True;
          end;
        end
          else
        begin
          ShAttr := 0;
          FDesktop.ParseDisplayName(Application.Handle, nil, PChar(Drive + ':\'), Eaten, PIDL, ShAttr);
        end;
      end;

      {Read driveStatus:}
      if (Flags and dsSize) <> 0 then
      begin
        { turn off critical errors }
        ErrorMode := SetErrorMode(SEM_FailCriticalErrors or SEM_NOOPENFILEERRORBOX);
        try
          { drive 1 = a, 2 = b, 3 = c, etc. }

          Size := DiskSize(Ord(Drive) - $40);
          DriveReady := (Size >= 0);
          if DriveReady then
          begin
            {Access the physical drive:}
            if GetVolumeInformation(PChar(Drive + ':\'), nil, 0,
                 @DriveSerial, MaxFileNameLength, FileSystemFlags,
                 nil, 0) then
            begin
            end
              else
            begin
              DriveSerial := 0;
            end;
          end
            else
          begin
            DriveSerial := 0;
          end;
        finally
          { restore old error mode }
          SetErrorMode(ErrorMode);
        end;
      end;

      {DisplayName:}
      if (Flags and dsDisplayName <> 0) then
      begin
        {Fetch drives displayname:}
        if Assigned(PIDL) then DisplayName := GetShellFileName(PIDL)
          else
        if Drive < FirstFixedDrive then DisplayName := GetShellFileName(Drive + ':\')
          // typical reason we do not have PIDL is that it took too long to
          // call ParseDisplayName, in what case calling SHGetFileInfo with
          // path (instead of PIDL) will take long too, avoiding that and using
          // fallback
          else DisplayName := '(' + Drive + ':)';

        if DriveType <> DRIVE_REMOTE then
        begin
          PrettyName := Drive + ': ' + DisplayName;

          CPos := Pos(' (' + Drive + ':)', PrettyName);
          if CPos > 0 then
            Delete(PrettyName, CPos, 5);
        end
          else
        begin
          DriveID := GetNetWorkName(Drive);
          PrettyName := Drive + ': ' + ExtractFileName(DriveID);
        end;
      end;

      {ImageIndex:}
      if ((Flags and dsImageIndex) <> 0) and (ImageIndex < 5) then
      begin
        if Assigned(PIDL) then
          SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_PIDL)
        else
          SHGetFileInfo(PChar(Drive + ':\'), 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
        ImageIndex := FileInfo.iIcon;
      end;
    end
      else
    begin
      if Assigned(PIDL) then
        FreePIDL(PIDL);
      ResetDrive(Drive);
    end;
    Result := Valid and DriveReady;
  end;
end; {TDriveInfo.ReadDriveStatus}

function GetShellFileName(const Name: string): string;
var
  SFI: TSHFileInfo;
  E: Integer;
begin
  E := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if SHGetFileInfo(PChar(Name), 0, SFI, SizeOf(TSHFileInfo), SHGFI_DISPLAYNAME) <> 0 then
      Result := SFI.szDisplayName;
  finally
    SetErrorMode(E);
  end;
end; {GetShellFileName}

function GetShellFileName(PIDL: PItemIDList): string;
var
  SFI: TSHFileInfo;
  E: Integer;
begin
  E := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if SHGetFileInfo(PChar(PIDL), 0, SFI, SizeOf(TSHFileInfo), SHGFI_PIDL or SHGFI_DISPLAYNAME) <> 0 then
      Result := SFI.szDisplayName;
  finally
    SetErrorMode(E);
  end;
end; {GetShellFileName}

function GetNetWorkName(Drive: Char): string;
var
  P: array[0..MAX_PATH] of Char;
  MaxLen : DWORD;
begin
  MaxLen := MAX_PATH;
  if WNetGetConnection(PChar(string(Drive + ':')), P, MaxLen) = NO_ERROR then
    Result := P
  else
    Result := '';
end; {GetNetWorkName}

type
  LPBYTE = ^BYTE;
  LMSTR = LPWSTR;
  NET_API_STATUS = DWORD;
  _USE_INFO_1 = record
    ui1_local: LMSTR;
    ui1_remote: LMSTR;
    ui1_password: LMSTR;
    ui1_status: DWORD;
    ui1_asg_type: DWORD;
    ui1_refcount: DWORD;
    ui1_usecount: DWORD;
  end;
  USE_INFO_1 = _USE_INFO_1;
  PUSE_INFO_1 = ^USE_INFO_1;
  LPVOID = Pointer;

const
  USE_OK       = 0;
  USE_PAUSED   = 1;
  USE_SESSLOST = 2;
  USE_DISCONN  = 2;
  USE_NETERR   = 3;
  USE_CONN     = 4;
  USE_RECONN   = 5;

function NetUseGetInfo(UncServerName: LMSTR; UseName: LMSTR; Level: DWORD; var BufPtr: LPBYTE): NET_API_STATUS; stdcall; external 'netapi32.dll';
function NetApiBufferFree(Buffer: Pointer): DWORD; stdcall; external 'netapi32.dll';

function GetNetWorkConnected(Drive: Char): Boolean;
var
  BufPtr: LPBYTE;
  NetResult: Integer;
begin
  NetResult := NetUseGetInfo(nil, PChar(Drive + ':'), 1, BufPtr);
  if NetResult = 0 then
  begin
    Result := (PUSE_INFO_1(BufPtr)^.ui1_status = USE_OK);
    NetApiBufferFree(LPVOID(BufPtr));
  end
    else
  begin
    // NetUseGetInfo works for DFS shares only, hence when it fails
    // we suppose different share type and fallback to "connected"
    Result := True;
  end;
end;

initialization
  if not Assigned(DriveInfo) then
    DriveInfo := TDriveInfo.Create;

finalization
  if Assigned(DriveInfo) then
  begin
    DriveInfo.Free;
    DriveInfo := nil;
  end;
end.
