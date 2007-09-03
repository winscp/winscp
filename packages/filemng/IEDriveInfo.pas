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

type
  TDrive    = Char;
  TDriveInfoRec = record
    PIDL        : PItemIDList; {Fully qualyfied PIDL}
    Init        : Boolean;     {Drivestatus was updated once}
    Valid       : Boolean;     {Drivestatus is valid}
    DriveReady  : Boolean;     {Drive is ready}
    DriveType   : Integer;     {DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_CDROM, DRIVE_RAMDISK, DRIVE_REMOTE}
    DisplayName : string;      {Windows displayname}
    Prettyname  : string;      {Prettyfied displayname}
    LongPrettyName : string;   {UNC-Network name on Network drives or same as PrettyName}
    DriveSerial : DWORD;       {Serial number of the drive}
    Size        : Int64;       {Drivesize}
    ImageIndex  : Integer;     {Drive imageIndex}
    FileSystemName  : string;  {Filesystemname as returned by GetVolumeInformation}
    MaxFileNameLength : DWORD; {Maximum length of filenames}
    FileSystemFlags : DWORD;   {Filesystem flags as returned by GetVolumeInformation}
  end;

  TDriveInfo = class(TObject)
  private
    FData: array[FirstDrive..LastDrive] of TDriveInfoRec;
    FNoDrives: DWORD;
    FDesktop: IShellFolder;
    function GetData(Drive: TDrive): TDriveInfoRec;

  public
    property Data[Drive: TDrive]: TDriveInfoRec read GetData; default;

    function GetImageIndex(Drive: TDrive): Integer;
    function GetDisplayName(Drive: TDrive): string;
    function GetPrettyName(Drive: TDrive): string;
    function GetLongPrettyName(Drive: TDrive): string;
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
  Math;

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
      SetLength(LongPrettyName, 0);
      SetLength(FileSystemName, 0);
      // This causes access violation
      // FreePIDL(PIDL);
    end;
  inherited;
end; {TDriveInfo.Destroy}

procedure TDriveInfo.Load;
var
  Drive: TDrive;
  Reg: TRegistry;
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
    with FData[Drive] do
      begin
        PIDL := nil;
        Init := False;
        DriveType := Windows.GetDriveType(PChar(Drive + ':\'));
        Valid := not Bool((1 shl (Ord(Drive) - 65)) and FNoDrives) and
          (DriveType in [DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_CDROM, DRIVE_RAMDISK, DRIVE_REMOTE]);
        Init := False;
        DriveReady := False;
        DisplayName := '';
        PrettyName := '';
        LongPrettyName := '';
        FileSystemName := '';
        DriveSerial := 0;
        Size := -1;
        ImageIndex := 0;
        FileSystemFlags := 0;
        MaxFileNameLength := 0;
      end;
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

function TDriveInfo.GetLongPrettyName(Drive: TDrive): String;
begin
  if (Drive < FirstDrive) or (Drive > LastDrive) then
    raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  Result := Drive + ':';

  if FData[Drive].Valid then
  begin
    if Length(FData[Drive].PrettyName) = 0 then
      ReadDriveStatus(Drive, dsDisplayName);
    Result := FData[Drive].LongPrettyName;
  end;
end; {TDriveInfo.GetLongPrettyName}

function TDriveInfo.GetData(Drive: TDrive): TDriveInfoRec;
begin
  if not (Upcase(Drive) in ['A'..'Z']) then
    raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  Result := FData[Upcase(Drive)];
end; {TDriveInfo.GetData}

function TDriveInfo.ReadDriveStatus(Drive: TDrive; Flags: Integer): Boolean;
var
  ErrorMode: Word;
  FileInfo: TShFileInfo;
  FileSystemNameBuffer: string;
  DriveID: string;
  CPos: Integer;
  WStr: WideString;
  Eaten: ULONG;
  ShAttr: ULONG;
begin
  if not Assigned(FDesktop) then
    SHGetDesktopFolder(FDesktop);

  Drive := Upcase(Drive);
  DriveID := '';
  if (Drive < FirstDrive) or (Drive > LastDrive) then
    raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  with FData[Drive] do
  begin
    Init := True;
    DriveType := Windows.GetDriveType(PChar(Drive + ':\'));
    Valid := not Bool((1 shl (Ord(Drive) - 65)) and FNoDrives) and
      (DriveType in [DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_CDROM, DRIVE_RAMDISK, DRIVE_REMOTE]);

    if Valid then
    begin
      if (not Assigned(PIDL)) and (Drive >= FirstFixedDrive) then
      begin
        WStr := Drive + ':\';
        FDesktop.ParseDisplayName(Application.Handle, nil, PWideChar(WStr), Eaten, PIDL, ShAttr);
      end;

      {Read driveStatus:}
      if (Flags and dsSize) <> 0 then
      begin
        { turn off critical errors }
        ErrorMode := SetErrorMode(SEM_FailCriticalErrors or SEM_NOOPENFILEERRORBOX);
        try
          { drive 1 = a, 2 = b, 3 = c, etc. }

          Size := BaseUtils.DiskSize(Ord(Drive) - $40);
          DriveReady := (Size >= 0);
          if DriveReady then
          begin
            SetLength(FileSystemNameBuffer, 500) ;
            SetLength(DriveID, 24);
            {Access the physical drive:}
            if GetVolumeInformation(PChar(Drive + ':\'), PChar(DriveID), 24,
                 @DriveSerial, MaxFileNameLength, FileSystemFlags,
                 PChar(FileSystemNameBuffer), 499) then
            begin
              FileSystemName := StrPas(PChar(FileSystemNameBuffer));
              DriveID := StrPas(PChar(DriveID));
            end
              else
            begin
              DriveSerial := 0;
              FileSystemName := '';
            end;
            SetLength(FileSystemNameBuffer, 0);
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
        if DriveReady or (Flags and dsSize = 0) then
        begin
          {Fetch drives displayname:}
          {Due to a bug in shGetFileInfo, this function returns allways the displayname of
           the first inserted disk, even if a disk change has occured. So, better use the
           Volume ID to build the drives displayname:}
          if (DriveType = DRIVE_CDROM) and (Length(DriveID) > 0) then
              DisplayName := DriveID[1] + LowerCase(Copy(DriveID, 2, 24)) + ' ('+ Drive + ':)'
            else
          begin
            if Assigned(PIDL) then DisplayName := GetShellFileName(PIDL)
              else DisplayName := GetShellFileName(Drive + ':\')
          end;

          PrettyName := Drive + ': ' + DisplayName;

          CPos := Pos('(' + Drive, PrettyName);
          if CPos > 0 then
            SetLength(PrettyName, Pred(CPos));

          if DriveType = DRIVE_REMOTE then
          begin
            DriveID := GetNetWorkName(Drive);
            PrettyName := Drive + ': ' + ExtractFileName(DriveID);
            LongPrettyName := Drive + ': ' + DriveID;
          end
            else
          begin
            LongPrettyName := Copy(PrettyName, 1, 3) + DisplayName;
            CPos := Pos('(' + Drive, LongPrettyName);
            if CPos > 0 then
              SetLength(LongPrettyName, Pred(CPos));
          end;
        end
          else
        begin
          DisplayName := Drive + ':';
          PrettyName  := DisplayName;
          LongPrettyName := DisplayName;
          FreePIDL(PIDL);
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
      Size := 0;
      DriveReady := False;
      DisplayName := '';
      PrettyName := '';
      LongPrettyName := '';
      DriveSerial := 0;
      ImageIndex  := 0;
      if Assigned(PIDL) then
        FreePIDL(PIDL);
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

function NetUseGetInfo(UncServerName: LMSTR; UseName: LMSTR; Level: DWORD;
  var BufPtr: LPBYTE): NET_API_STATUS; stdcall; external 'netapi32.dll' name 'NetUseGetInfo';
function NetApiBufferFree(Buffer: LPVOID): NET_API_STATUS; stdcall;
  external 'netapi32.dll' name 'NetApiBufferFree';

function GetNetWorkConnected(Drive: Char): Boolean;
var
  BufPtr: LPBYTE;
  Use: WideString;
  NetResult: Integer;
begin
  Use := Drive + ':';
  NetResult := NetUseGetInfo(nil, PWideChar(Use), 1, BufPtr);
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
