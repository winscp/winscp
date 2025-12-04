unit IEDriveInfoInt;

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
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Registry, SysUtils, Classes, ComCtrls, ShellApi, ShlObj, CommCtrl, Forms,
  BaseUtils, System.Generics.Collections, Vcl.Graphics, Winapi.Messages;

const
  {Flags used by TDriveInfoInt.ReadDriveStatus and TDriveView.RefreshRootNodes:}
  dsValid = 0;        {checks only whether drive is still valid}
  dsImageIndex = 1;   {Fetch imageindex, if not allready fetched}
  dsSize = 2;         {Fetch disk size and serialnumber}
  dsDisplayName = 4;  {Fetch drives displayname}
  dsSynchronous = dsImageIndex or dsDisplayName;
  dsAll = dsSynchronous or dsSize;
  FirstDrive          = 'A';
  SystemDrive         = 'C';
  LastDrive           = 'Z';
  FirstSpecialFolder  = CSIDL_DESKTOP;
  LastSpecialFolder   = CSIDL_PRINTHOOD;
  WM_USER_SHCHANGENOTIFY = WM_USER + $2000 + 13;
  WM_DRIVEINFO_PROCESS = WM_USER + $2000 + 18;

type
  TDriveInfoRec = class
    PIDL        : PItemIDList; {Fully qualyfied PIDL}
    Init        : Boolean;     {Drivestatus was updated once}
    Valid       : Boolean;     {Drivestatus is valid}
    ValidButHiddenByDrivePolicy: Boolean;
    DriveReady  : Boolean;     {Drive is ready}
    DriveType   : Integer;     {DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_CDROM, DRIVE_RAMDISK, DRIVE_REMOTE}
    DisplayName : string;      {Windows displayname}
    PrettyName  : string;      {Prettyfied displayname}
    DriveSerial : DWORD;       {Serial number of the drive}
    Size        : Int64;       {Drivesize}
    ImageIndex  : Integer;     {Drive imageIndex}
    DriveHandle: THandle;
    NotificationHandle: HDEVNOTIFY;
    SubscribeDriveNotifications: Boolean;
  end;

  TRealDrive = char;
  TSpecialFolder = FirstSpecialFolder..LastSpecialFolder;
  PSpecialFolderRec = ^TSpecialFolderRec;
  TSpecialFolderRec = record
    Valid: Boolean;
    Location: string;
    DisplayName: string;
    ImageIndex: Integer;
    PIDL: PItemIDList;
  end;

  TDriveNotification = (dnRefresh, dnRemoving);
  TDriveNotificationEvent = procedure(Notification: TDriveNotification; Drive: string) of object;

  TDriveInfoInt = class(TObject)
  private
    FData: TObjectDictionary<string, TDriveInfoRec>;
    FNoDrives: DWORD;
    FNoViewOnDrive: DWORD;
    FDesktop: IShellFolder;
    FFolders: array[TSpecialFolder] of TSpecialFolderRec;
    FHonorDrivePolicy: Integer;
    FUseABDrives: Boolean;
    FLoaded: Boolean;
    FHandlers: TList<TDriveNotificationEvent>;
    FChangeNotify: ULONG;
    function GetFolder(Folder: TSpecialFolder): PSpecialFolderRec;
    procedure ReadDriveBasicStatus(Drive: string);
    procedure ResetDrive(Drive: string);
    procedure SetHonorDrivePolicy(Value: Integer);
    function GetFirstFixedDrive: Char;
    procedure Load;
    function AddDrive(Drive: string): TDriveInfoRec;
    function GetDriveBitMask(Drive: string): Integer;
    function DoAnyValidPath(DriveType: Integer; CanBeHidden: Boolean; var Path: string): Boolean;
    function ReadDriveMask(Reg: TRegistry; ValueName: string): DWORD;
    procedure ScheduleDriveRefresh;
    procedure CancelDriveRefresh;
    procedure InternalWndProc(var Msg: TMessage);
    procedure InvokeHandlers(DriveNotification: TDriveNotification; Drive: string);
    procedure UpdateDriveNotifications(Drive: string);
    procedure UpdateDrivesNotifications;
    procedure ProcessThreadResults;
    procedure ReadAsynchronous;
    procedure DoReadDriveStatus(Drive: string; Flags: Integer);
    procedure DriveRemoving(Drive: string);

  protected
    constructor Create;

  public
    function Get(Drive: string): TDriveInfoRec;
    property SpecialFolder[Folder: TSpecialFolder]: PSpecialFolderRec read GetFolder;

    procedure NeedData;
    function AnyValidPath: string;
    function GetDriveKey(Path: string): string;
    function GetDriveRoot(Drive: string): string;
    function IsRealDrive(Drive: string): Boolean;
    function IsFixedDrive(Drive: string): Boolean;
    function GetImageIndex(Drive: string): Integer;
    function GetSimpleName(Drive: string): string;
    function GetDisplayName(Drive: string): string;
    function GetPrettyName(Drive: string): string;
    procedure ReadDriveStatus(Drive: string; Flags: Integer);
    procedure OverrideDrivePolicy(Drive: string);
    property HonorDrivePolicy: Integer read FHonorDrivePolicy write SetHonorDrivePolicy;
    property FirstFixedDrive: Char read GetFirstFixedDrive;
    property UseABDrives: Boolean read FUseABDrives write FUseABDrives;

    destructor Destroy; override;

    procedure AddHandler(Handler: TDriveNotificationEvent);
    procedure RemoveHandler(Handler: TDriveNotificationEvent);
    procedure DriveRefresh;
    procedure SubscribeDriveNotifications(Drive: string);
  end;

function GetShellFileName(PIDL: PItemIDList): string; overload;
function GetNetWorkName(Drive: string): string;
function GetNetWorkConnected(Drive: string): Boolean;
function IsRootPath(Path: string): Boolean;
function GetThumbnail(Path: string; Size: TSize): TBitmap;

{Central drive information object instance of TDriveInfoInt}
var
  DriveInfo : TDriveInfoInt = nil;

procedure DriveInfoInit(ADriveInfo: TDriveInfoInt);

resourceString
  ErrorInvalidDrive = '%s is a invalid drive letter.';

implementation

uses
  Math, PIDL, OperationWithTimeout, PasTools, CompThread;

type
  PRGBQuadArray = ^TRGBQuadArray;    // From graphics.pas
  TRGBQuadArray = array[Byte] of TRGBQuad;  // From graphics.pas

// Globals so that we do not have to fear that thread run after DriveInfo is released
var
  InternalWindowHandle: HWND;
  ThreadLock: TRTLCriticalSection;
  ReadyDrives: string;

type
  TDriveInfoThread = class(TCompThread)
  public
    constructor Create(Drives: string);
  protected
    procedure Execute; override;
  private
    FDrives: string;
  end;

constructor TDriveInfoThread.Create(Drives: string);
begin
  inherited Create(True);
  FDrives := Drives;
  FreeOnTerminate := True;
  Resume;
end;

procedure TDriveInfoThread.Execute;
var
  I: Integer;
  FreeSpace, Size: Int64;
  DriveRoot: string;
  Drive: Char;
begin
  if Length(FDrives) = 1 then
  begin
    Drive := FDrives[1];
    DriveRoot := DriveInfo.GetDriveRoot(Drive);
    if GetDiskFreeSpaceEx(PChar(DriveRoot), FreeSpace, Size, nil) then
    begin
      EnterCriticalSection(ThreadLock);
      ReadyDrives := ReadyDrives + Drive;
      LeaveCriticalSection(ThreadLock);
    end;
  end
    else
  begin
    for I := 1 to Length(FDrives) do
    begin
      TDriveInfoThread.Create(FDrives[I]);
      Sleep(100);
    end;
  end;
end;

constructor TDriveInfoInt.Create;
begin
  inherited;

  FHonorDrivePolicy := 1;
  FUseABDrives := True;
  FLoaded := False;
  FData := TObjectDictionary<string, TDriveInfoRec>.Create([doOwnsValues]);
  FHandlers := TList<TDriveNotificationEvent>.Create;
  FChangeNotify := 0;
end; {TDriveInfoInt.Create}

destructor TDriveInfoInt.Destroy;
begin
  Assert(FHandlers.Count = 0);
  FHandlers.Free;
  FData.Free;
  inherited;
end; {TDriveInfoInt.Destroy}

procedure TDriveInfoInt.NeedData;
begin
  if not FLoaded then
  begin
    Load;
    FLoaded := True;
  end;

  ProcessThreadResults;
end;

procedure TDriveInfoInt.ProcessThreadResults;
var
  I: Integer;
  Drive: Char;
begin
  EnterCriticalSection(ThreadLock);
  try
    for I := 1 to Length(ReadyDrives) do
    begin
      Drive := ReadyDrives[I];
      Assert(FData.ContainsKey(Drive));
      FData[Drive].DriveReady := True;
      UpdateDriveNotifications(Drive);
      AppLog(Format('Drive "%s" is ready', [Drive]))
    end;
    ReadyDrives := '';
  finally
    LeaveCriticalSection(ThreadLock);
  end;
end;

function TDriveInfoInt.DoAnyValidPath(DriveType: Integer; CanBeHidden: Boolean; var Path: string): Boolean;
var
  Drive: TRealDrive;
  DriveInfoRec: TDriveInfoRec;
begin
  for Drive := SystemDrive to LastDrive do
  begin
    DriveInfoRec := Get(Drive);
    if (DriveInfoRec.Valid or
        (CanBeHidden and DriveInfoRec.ValidButHiddenByDrivePolicy)) and
       (DriveInfoRec.DriveType = DriveType) and
       DirectoryExists(ApiPath(GetDriveRoot(Drive))) then
    begin
      Result := True;
      Path := GetDriveRoot(Drive);
      Exit;
    end;
  end;

  Result := False;
end;

function TDriveInfoInt.AnyValidPath: string;
begin
  if (not DoAnyValidPath(DRIVE_FIXED, False, Result)) and
     (not DoAnyValidPath(DRIVE_FIXED, True, Result)) and
     (not DoAnyValidPath(DRIVE_REMOTE, False, Result)) then
  begin
    raise Exception.Create(SNoValidPath);
  end;
end;

function TDriveInfoInt.IsRealDrive(Drive: string): Boolean;
begin
  Result := (Length(Drive) = 1);
  Assert((not Result) or ((Drive[1] >= FirstDrive) and (Drive[1] <= LastDrive)));
end;

function TDriveInfoInt.IsFixedDrive(Drive: string): Boolean;
begin
  Result := True;
  if IsRealDrive(Drive) and (Drive[1] < FirstFixedDrive) then Result := False;
end;

function TDriveInfoInt.GetDriveKey(Path: string): string;
begin
  Result := ExtractFileDrive(Path);
  if (Length(Result) = 2) and (Result[2] = DriveDelim) then
  begin
    Result := Upcase(Result[1]);
  end
    else
  if IsUncPath(Path) then
  begin
    Result := LowerCase(Result);
  end
    else
  begin
    raise EConvertError.Create(Format(ErrorInvalidDrive, [Path]))
  end;
end;

function TDriveInfoInt.GetDriveRoot(Drive: string): string;
begin
  if IsRealDrive(Drive) then
  begin
    Result := Drive + ':\'
  end
    else
  begin
    Assert(IsUncPath(Drive));
    Result := IncludeTrailingBackslash(Drive);
  end;
end;

function TDriveInfoInt.GetFolder(Folder: TSpecialFolder): PSpecialFolderRec;
var
  FileInfo: TShFileInfo;
  Path: PChar;
  Flags: Word;
begin
  NeedData;
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

procedure TDriveInfoInt.SetHonorDrivePolicy(Value: Integer);
var
  Drive: TRealDrive;
begin
  if HonorDrivePolicy <> Value then
  begin
    FHonorDrivePolicy := Value;
    if FLoaded then
    begin
      for Drive := FirstDrive to LastDrive do
      begin
        ReadDriveBasicStatus(Drive);
      end;
    end;
  end;
end;

function TDriveInfoInt.GetFirstFixedDrive: Char;
begin
  if UseABDrives then Result := FirstDrive
    else Result := SystemDrive;
end;

function TDriveInfoInt.GetDriveBitMask(Drive: string): Integer;
begin
  Assert(IsRealDrive(Drive));
  Result := (1 shl (Ord(Drive[1]) - Ord('A')));
end;

procedure TDriveInfoInt.ReadDriveBasicStatus(Drive: string);
var
  ValidDriveType: Boolean;
  InaccessibleByDrivePolicy, HiddenByDrivePolicy: Boolean;
  DriveBitMask: Integer;
begin
  Assert(FData.ContainsKey(Drive));
  with FData[Drive] do
  begin
    DriveType := Windows.GetDriveType(PChar(GetDriveRoot(Drive)));
    if IsRealDrive(Drive) then DriveBitMask := GetDriveBitMask(Drive)
      else DriveBitMask := 0;
    InaccessibleByDrivePolicy :=
      ((HonorDrivePolicy and 2) <> 0) and ((DriveBitMask and FNoViewOnDrive) <> 0);
    HiddenByDrivePolicy :=
      ((HonorDrivePolicy and 1) <> 0) and ((DriveBitMask and FNoDrives) <> 0);
    ValidDriveType :=
      (DriveType in [DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_CDROM, DRIVE_RAMDISK, DRIVE_REMOTE]) and
      (not InaccessibleByDrivePolicy);
    ValidButHiddenByDrivePolicy := ValidDriveType and HiddenByDrivePolicy;
    Valid := ValidDriveType and (not HiddenByDrivePolicy);
  end;
end;

procedure TDriveInfoInt.ResetDrive(Drive: string);
begin
  with FData[Drive] do
  begin
    DriveReady := False;
    DisplayName := '';
    PrettyName := '';
    DriveSerial := 0;
    Size := -1;
    ImageIndex := 0;
    DriveHandle := INVALID_HANDLE_VALUE;
    NotificationHandle := nil;
    SubscribeDriveNotifications := False;
  end;
end;

function TDriveInfoInt.ReadDriveMask(Reg: TRegistry; ValueName: string): DWORD;
var
  DataInfo: TRegDataInfo;
begin
  Result := 0;
  if Reg.GetDataInfo(ValueName, DataInfo) then
  begin
    if (DataInfo.RegData = rdBinary) and (DataInfo.DataSize >= SizeOf(Result)) then
    begin
      Reg.ReadBinaryData(ValueName, Result, SizeOf(Result));
    end
      else
    if DataInfo.RegData = rdInteger then
    begin
      Result := Reg.ReadInteger(ValueName);
    end;
  end;
end;

procedure TDriveInfoInt.Load;
var
  Drive: TRealDrive;
  Reg: TRegistry;
  Folder: TSpecialFolder;
begin
  AppLog('Loading drives');
  Reg := TRegistry.Create;
  FNoDrives := 0;
  FNoViewOnDrive := 0;
  try
    if Reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Policies\Explorer') then
    begin
      FNoDrives := ReadDriveMask(Reg, 'NoDrives');
      FNoViewOnDrive := ReadDriveMask(Reg, 'NoViewOnDrive');
    end;
  finally
    Reg.Free;
  end;
  AppLog(Format('NoDrives mask: %d', [Integer(FNoDrives)]));
  AppLog(Format('NoViewOnDrive mask: %d', [Integer(FNoViewOnDrive)]));

  FDesktop := nil;

  for Drive := FirstDrive to LastDrive do
  begin
    AddDrive(Drive);
  end;

  ReadAsynchronous;

  for Folder := Low(FFolders) to High(FFolders) do
    FFolders[Folder].Valid := False;
end;

procedure TDriveInfoInt.ReadAsynchronous;
var
  Drive: TRealDrive;
  Drives: string;
begin
  for Drive := FirstDrive to LastDrive do
  begin
    // Not using Get as that would recurse into Load
    if FData[Drive].Valid then
      Drives := Drives + Drive;
  end;
    TDriveInfoThread.Create(Drives);

  if Length(Drives) > 0 then
  begin
    AppLog(Format('Drives to check in the background: %s', [Drives]));
    TDriveInfoThread.Create(Drives);
  end;
end;

function TDriveInfoInt.AddDrive(Drive: string): TDriveInfoRec;
begin
  Result := TDriveInfoRec.Create;
  FData.Add(Drive, Result);
  ResetDrive(Drive);
  if IsFixedDrive(Drive) or (not IsRealDrive(Drive)) then // not floppy
    DoReadDriveStatus(Drive, dsSynchronous)
  else
    ReadDriveBasicStatus(Drive);
end;

function TDriveInfoInt.GetImageIndex(Drive: string): Integer;
begin
  NeedData;

  Result := 0;

  if Get(Drive).Valid then
  begin
    if Get(Drive).ImageIndex = 0 then
      ReadDriveStatus(Drive, dsImageIndex);
    Result := Get(Drive).ImageIndex;
  end;
end; {TDriveInfoInt.GetImageIndex}

function TDriveInfoInt.GetDisplayName(Drive: string): string;
begin
  if Get(Drive).Valid then
  begin
    if Length(Get(Drive).DisplayName) = 0 then
      ReadDriveStatus(Drive, dsDisplayName);
    Result := Get(Drive).DisplayName;
  end
    else
  begin
    Result := GetSimpleName(Drive);
  end;
end; {TDriveInfoInt.GetDisplayname}

function TDriveInfoInt.GetPrettyName(Drive: string): string;
begin
  if Get(Drive).Valid then
  begin
    if Length(Get(Drive).PrettyName) = 0 then
      ReadDriveStatus(Drive, dsDisplayName);
    Result := Get(Drive).PrettyName;
  end;

  if Length(Result) = 0 then
    Result := GetSimpleName(Drive);
end; {TDriveInfoInt.GetPrettyName}

function TDriveInfoInt.GetSimpleName(Drive: string): string;
begin
  Result := Drive;
  if IsRealDrive(Result) then Result := Result + ':';
end;

function TDriveInfoInt.Get(Drive: string): TDriveInfoRec;
begin
  NeedData;

  // We might want to wait for FReadyDrives to be empty before returning
  // (or even better do that only in DriveReady getter)

  if not FData.TryGetValue(Drive, Result) then
  begin
    Assert(IsUncPath(Drive));
    Result := AddDrive(Drive);
    DriveRefresh;
  end;
end; {TDriveInfoInt.GetData}

procedure TDriveInfoInt.ReadDriveStatus(Drive: string; Flags: Integer);
begin
  // Among other, this makes sure the pending drive-ready status from the background thread are collected,
  // before we overwrite it with fresh status here.
  NeedData;
  DoReadDriveStatus(Drive, Flags);
end;

procedure TDriveInfoInt.DoReadDriveStatus(Drive: string; Flags: Integer);
var
  FileInfo: TShFileInfo;
  DriveRoot: string;
  DriveID: string;
  CPos: Integer;
  Eaten: ULONG;
  ShAttr: ULONG;
  MaxFileNameLength: DWORD;
  FileSystemFlags: DWORD;
  FreeSpace: Int64;
  SimpleName: string;
  DriveInfoRec: TDriveInfoRec;
  S: string;
begin
  if not Assigned(FDesktop) then
    SHGetDesktopFolder(FDesktop);

  DriveRoot := GetDriveRoot(Drive);

  // When this method is called, the entry always exists already
  Assert(FData.ContainsKey(Drive));
  DriveInfoRec := FData[Drive];
  with DriveInfoRec do
  begin
    Init := True;
    ReadDriveBasicStatus(Drive);

    if Valid then
    begin
      if (not Assigned(PIDL)) and IsFixedDrive(Drive) then
      begin
        ShAttr := 0;
        if DriveType = DRIVE_REMOTE then
        begin
          ShellFolderParseDisplayNameWithTimeout(
            FDesktop, Application.Handle, nil, PChar(DriveRoot), Eaten, PIDL, ShAttr, 2 * MSecsPerSec);
        end
          else
        begin
          FDesktop.ParseDisplayName(Application.Handle, nil, PChar(DriveRoot), Eaten, PIDL, ShAttr);
        end;
      end;

      {Read driveStatus:}
      if (Flags and dsSize) <> 0 then
      begin
        DriveReady := GetDiskFreeSpaceEx(PChar(DriveRoot), FreeSpace, Size, nil);
        if DriveReady then
        begin
          {Access the physical drive:}
          if GetVolumeInformation(PChar(DriveRoot), nil, 0,
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
        // Particularly when removing drive fails (as other app has it locked), we end up with not monitoring the
        // drive. When the drive is visited again in panel, it calls into here, and we take the opportunity
        // to resume monitoring
        UpdateDriveNotifications(Drive);
      end;

      {DisplayName:}
      if (Flags and dsDisplayName) <> 0 then
      begin
        {Fetch drives displayname:}
        SimpleName := GetSimpleName(Drive);
        if Assigned(PIDL) then DisplayName := GetShellFileName(PIDL)
          else
        begin
          // typical reason we do not have PIDL is that it took too long to
          // call ParseDisplayName, in what case calling SHGetFileInfo with
          // path (instead of PIDL) will take long too, avoiding that and using
          // fallback
          DisplayName := '(' + SimpleName + ')';
        end;

        if DriveType <> DRIVE_REMOTE then
        begin
          PrettyName := SimpleName + ' ' + DisplayName;

          S := ' (' + SimpleName + ')';
          CPos := Pos(S, PrettyName);
          if CPos > 0 then
            Delete(PrettyName, CPos, Length(S));
        end
          else
        if IsRealDrive(Drive) then
        begin
          DriveID := GetNetWorkName(Drive);
          PrettyName := Format('%s %s (%s)', [SimpleName, ExtractFileName(DriveID), ExtractFileDir(DriveID)]);
        end
          else
        begin
          Assert(IsUncPath(DriveRoot));
          PrettyName := SimpleName;
        end;
      end;

      {ImageIndex:}
      if (Flags and dsImageIndex) <> 0 then
      begin
        if Assigned(PIDL) then
        begin
          SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_PIDL)
        end
          else
        begin
          SHGetFileInfo(PChar(DriveRoot), 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
        end;
        ImageIndex := FileInfo.iIcon;
      end;
    end
      else
    begin
      if Assigned(PIDL) then
        FreePIDL(PIDL);
      ResetDrive(Drive);
    end;
  end;
end; {TDriveInfoInt.ReadDriveStatus}

procedure TDriveInfoInt.OverrideDrivePolicy(Drive: string);
var
  Mask: DWORD;
begin
  Assert(FData.ContainsKey(Drive));
  Assert(FData[Drive].ValidButHiddenByDrivePolicy);
  Mask := (not GetDriveBitMask(Drive));
  FNoDrives := FNoDrives and Mask;
  ReadDriveStatus(Drive, dsAll);
  Assert(FData[Drive].Valid);
  DriveRefresh;
end;

procedure TDriveInfoInt.AddHandler(Handler: TDriveNotificationEvent);
var
  ChangeNotifyEntry: TSHChangeNotifyEntry;
  Dummy: string;
begin
  if not FHandlers.Contains(Handler) then
  begin
    FHandlers.Add(Handler);

    if FHandlers.Count = 1 then
    begin
      // Source: petr.solin 2022-02-25
      if SpecialFolderLocation(CSIDL_DESKTOP, Dummy, ChangeNotifyEntry.pidl) then
      begin
        ChangeNotifyEntry.fRecursive := False;

        FChangeNotify :=
          SHChangeNotifyRegister(
            InternalWindowHandle, SHCNRF_ShellLevel or SHCNRF_NewDelivery,
            SHCNE_RENAMEFOLDER or SHCNE_MEDIAINSERTED or SHCNE_MEDIAREMOVED,
            WM_USER_SHCHANGENOTIFY, 1, ChangeNotifyEntry);
      end;
      UpdateDrivesNotifications;
    end;
  end;
end;

procedure TDriveInfoInt.RemoveHandler(Handler: TDriveNotificationEvent);
begin
  if (FHandlers.Remove(Handler) >= 0) and (FHandlers.Count = 0) then
  begin
    if FChangeNotify <> 0 then
    begin
      SHChangeNotifyDeregister(FChangeNotify);
      FChangeNotify := 0;
    end;
    UpdateDrivesNotifications;
  end;
end;

procedure TDriveInfoInt.InvokeHandlers(DriveNotification: TDriveNotification; Drive: string);
var
  Handler: TDriveNotificationEvent;
begin
  for Handler in FHandlers do
    Handler(DriveNotification, Drive);
end;

type
  PDevBroadcastHdr = ^TDevBroadcastHdr;
  TDevBroadcastHdr = record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
  end;

  PDevBroadcastVolume = ^TDevBroadcastVolume;
  TDevBroadcastVolume = record
    dbcv_size: DWORD;
    dbcv_devicetype: DWORD;
    dbcv_reserved: DWORD;
    dbcv_unitmask: DWORD;
    dbcv_flags: WORD;
  end;

  PDEV_BROADCAST_HANDLE = ^DEV_BROADCAST_HANDLE;
  DEV_BROADCAST_HANDLE = record
    dbch_size       : DWORD;
    dbch_devicetype : DWORD;
    dbch_reserved   : DWORD;
    dbch_handle     : THandle;
    dbch_hdevnotify : HDEVNOTIFY  ;
    dbch_eventguid  : TGUID;
    dbch_nameoffset : LongInt;
    dbch_data       : Byte;
  end;

  PPItemIDList = ^PItemIDList;

const
  DBT_DEVTYP_HANDLE = $00000006;
  DBT_CONFIGCHANGED = $0018;
  DBT_DEVICEARRIVAL = $8000;
  DBT_DEVICEQUERYREMOVE = $8001;
  DBT_DEVICEREMOVEPENDING = $8003;
  DBT_DEVICEREMOVECOMPLETE = $8004;
  DBT_DEVTYP_VOLUME = $00000002;

// WORKAROUND Declaration in Winapi.ShlObj.pas is wrong
function SHChangeNotification_Lock(hChange: THandle; dwProcId: DWORD;
  var PPidls: PPItemIDList; var plEvent: Longint): THANDLE; stdcall;
external 'shell32.dll' name 'SHChangeNotification_Lock';

procedure TDriveInfoInt.InternalWndProc(var Msg: TMessage);
var
  DeviceType: DWORD;
  UnitMask: DWORD;
  DeviceHandle: THandle;
  Drive: Char;
  PPIDL: PPItemIDList;
  Event: LONG;
  Lock: THandle;
  DrivePair: TPair<string, TDriveInfoRec>;
begin
  with Msg do
  begin
    if Msg = WM_USER_SHCHANGENOTIFY then
    begin
      Lock := SHChangeNotification_Lock(wParam, lParam, PPIDL, Event);
      try
        if (Event = SHCNE_RENAMEFOLDER) or // = drive rename
           (Event = SHCNE_MEDIAINSERTED) or // also bitlocker drive unlock (also sends SHCNE_UPDATEDIR)
           (Event = SHCNE_MEDIAREMOVED) then
        begin
          ScheduleDriveRefresh;
        end;
      finally
        SHChangeNotification_Unlock(Lock);
      end;
    end
      else
    // from RegisterDeviceNotification
    if Msg = WM_DEVICECHANGE then
    begin
      if (wParam = DBT_CONFIGCHANGED) or
         (wParam = DBT_DEVICEARRIVAL) or
         (wParam = DBT_DEVICEREMOVECOMPLETE) then
      begin
        ScheduleDriveRefresh;
      end
        else
      if (wParam = DBT_DEVICEQUERYREMOVE) or
         (wParam = DBT_DEVICEREMOVEPENDING) then
      begin
        DeviceType := PDevBroadcastHdr(lParam)^.dbch_devicetype;
        // This is specifically for VeraCrypt.
        // For normal drives, see DBT_DEVTYP_HANDLE below
        // (and maybe now that we have generic implementation, this specific code for VeraCrypt might not be needed anymore)
        if DeviceType = DBT_DEVTYP_VOLUME then
        begin
          UnitMask := PDevBroadcastVolume(lParam)^.dbcv_unitmask;
          Drive := FirstDrive;
          while UnitMask > 0 do
          begin
            if UnitMask and $01 <> 0 then
            begin
              DriveRemoving(Drive);
            end;
            UnitMask := UnitMask shr 1;
            Drive := Chr(Ord(Drive) + 1);
          end;
        end
          else
        if DeviceType = DBT_DEVTYP_HANDLE then
        begin
          DeviceHandle := PDEV_BROADCAST_HANDLE(lParam)^.dbch_handle;
          for DrivePair in FData do
            if DrivePair.Value.DriveHandle = DeviceHandle then
            begin
              DriveRemoving(DrivePair.Key);
            end;
        end;
      end;
    end
      else
    if Msg = WM_TIMER then
    begin
      CancelDriveRefresh;
      try
        for Drive := FirstFixedDrive to LastDrive do
          ReadDriveStatus(Drive, dsSynchronous);
        ReadAsynchronous;
        DriveRefresh;
      except
        Application.HandleException(Self);
      end;
    end
      else
    if Msg = WM_DRIVEINFO_PROCESS then
    begin
      ProcessThreadResults;
    end;

    Result := DefWindowProc(InternalWindowHandle, Msg, wParam, lParam);
  end;
end;

procedure TDriveInfoInt.DriveRemoving(Drive: string);
begin
  FData[Drive].DriveReady := False;
  UpdateDriveNotifications(Drive);
  AppLog(Format('Removing drive "%s"', [Drive]));
  InvokeHandlers(dnRemoving, Drive);
end;

procedure TDriveInfoInt.CancelDriveRefresh;
begin
  KillTimer(InternalWindowHandle, 1);
end;

procedure TDriveInfoInt.ScheduleDriveRefresh;
begin
  CancelDriveRefresh;
  // Delay refreshing drives for a sec.
  // Particularly with CD/DVD drives, if we query display name
  // immediately after receiving DBT_DEVICEARRIVAL, we do not get media label.
  // Actually one sec does not help usually, but we do not want to wait any longer,
  // because we want to add USB drives asap.
  // And this problem might be solved now by SHChangeNotifyRegister/SHCNE_RENAMEFOLDER.
  SetTimer(InternalWindowHandle, 1, MSecsPerSec, nil);
end;

procedure TDriveInfoInt.UpdateDriveNotifications(Drive: string);
var
  NeedNotifications: Boolean;
  Path: string;
  DevBroadcastHandle: DEV_BROADCAST_HANDLE;
  Size: Integer;
  DriveInfoRec: TDriveInfoRec;
begin
  if IsFixedDrive(Drive) then
  begin
    // Not using Get to avoid recursion
    DriveInfoRec := FData[Drive];
    NeedNotifications :=
      (FHandlers.Count > 0) and
      (DriveInfoRec.DriveType <> DRIVE_REMOTE) and
      DriveInfoRec.DriveReady and
      DriveInfoRec.SubscribeDriveNotifications;

    if NeedNotifications <> (DriveInfoRec.DriveHandle <> INVALID_HANDLE_VALUE) then
    begin
      Path := GetDriveRoot(Drive);
      if NeedNotifications then
      begin
        DriveInfoRec.DriveHandle :=
          CreateFile(PChar(Path), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
          OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_ATTRIBUTE_NORMAL, 0);
        if DriveInfoRec.DriveHandle <> INVALID_HANDLE_VALUE then
        begin
          Size := SizeOf(DevBroadcastHandle);
          ZeroMemory(@DevBroadcastHandle, Size);
          DevBroadcastHandle.dbch_size := Size;
          DevBroadcastHandle.dbch_devicetype := DBT_DEVTYP_HANDLE;
          DevBroadcastHandle.dbch_handle := DriveInfoRec.DriveHandle;

          DriveInfoRec.NotificationHandle :=
            RegisterDeviceNotification(InternalWindowHandle, @DevBroadcastHandle, DEVICE_NOTIFY_WINDOW_HANDLE);
          if DriveInfoRec.NotificationHandle <> nil then
          begin
            AppLog(Format('Registered drive notification for "%s"', [Path]));
          end
            else
          begin
            CloseHandle(DriveInfoRec.DriveHandle);
            DriveInfoRec.DriveHandle := INVALID_HANDLE_VALUE;
          end;
        end;
      end
        else
      begin
        AppLog(Format('Unregistered drive notification for "%s"', [Path]));
        UnregisterDeviceNotification(DriveInfoRec.NotificationHandle);
        DriveInfoRec.NotificationHandle := nil;

        CloseHandle(DriveInfoRec.DriveHandle);
        DriveInfoRec.DriveHandle := INVALID_HANDLE_VALUE;
      end;
    end;
  end;
end;

procedure TDriveInfoInt.UpdateDrivesNotifications;
var
  Drive: string;
begin
  for Drive in FData.Keys do
    UpdateDriveNotifications(Drive);
end;

procedure TDriveInfoInt.DriveRefresh;
begin
  InvokeHandlers(dnRefresh, '');
end;

procedure TDriveInfoInt.SubscribeDriveNotifications(Drive: string);
begin
  Get(Drive).SubscribeDriveNotifications := True;
  UpdateDriveNotifications(Drive);
end;

// ===================

function GetShellFileName(PIDL: PItemIDList): string;
var
  SFI: TSHFileInfo;
begin
  if SHGetFileInfo(PChar(PIDL), 0, SFI, SizeOf(TSHFileInfo), SHGFI_PIDL or SHGFI_DISPLAYNAME) <> 0 then
    Result := SFI.szDisplayName;
end; {GetShellFileName}

function GetNetWorkName(Drive: string): string;
var
  Path: string;
  P: array[0..MAX_PATH] of Char;
  MaxLen : DWORD;
begin
  Path := ExcludeTrailingBackslash(DriveInfo.GetDriveRoot(Drive));
  MaxLen := MAX_PATH;
  if WNetGetConnection(PChar(Path), P, MaxLen) = NO_ERROR then
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

function GetNetWorkConnected(Drive: string): Boolean;
var
  BufPtr: LPBYTE;
  NetResult: Integer;
  ServerName: string;
  PServerName: PChar;
  Name: string;
  P: Integer;
begin
  Name := '';
  PServerName := nil;
  if DriveInfo.IsRealDrive(Drive) then
  begin
    Name := Drive + ':';
  end
    else
  if IsUncPath(Drive) then
  begin
    Name := Copy(Drive, 3, Length(Drive) - 2);
    P := Pos('\', Name);
    if P > 0 then
    begin
      ServerName := Copy(Name, P + 1, Length(Name) - P);
      PServerName := PChar(ServerName);
      SetLength(Name, P - 1);
    end
      else
    begin
      Assert(False);
    end;
  end
    else
  begin
    Assert(False);
  end;

  if Name = '' then
  begin
    Result := False;
  end
    else
  begin
    NetResult := NetUseGetInfo(PServerName, PChar(Name), 1, BufPtr);
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
end;

function IsRootPath(Path: string): Boolean;
begin
  Result := SameText(ExcludeTrailingBackslash(ExtractFileDrive(Path)), ExcludeTrailingBackslash(Path));
end;

function GetThumbnail(Path: string; Size: TSize): TBitmap;
var
  ImageFactory: IShellItemImageFactory;
  X, Y: Integer;
  Row: PRGBQuadArray;
  Pixel: PRGBQuad;
  Alpha: Byte;
  Handle: HBITMAP;
begin
  Result := nil;
  SHCreateItemFromParsingName(PChar(Path), nil, IShellItemImageFactory, ImageFactory);
  if Assigned(ImageFactory) then
  begin
    if Succeeded(ImageFactory.GetImage(Size, SIIGBF_RESIZETOFIT, Handle)) then
    begin
      Result := TBitmap.Create;
      try
        Result.Handle := Handle;
        Result.PixelFormat := pf32bit;

        for Y := 0 to Result.Height - 1 do
        begin
          Row  := Result.ScanLine[Y];
          for X := 0 to Result.Width - 1 do
          begin
            Pixel := @Row[X];
            Alpha := Pixel.rgbReserved;
            Pixel.rgbBlue := (Pixel.rgbBlue * Alpha) div 255;
            Pixel.rgbGreen := (Pixel.rgbGreen * Alpha) div 255;
            Pixel.rgbRed := (Pixel.rgbRed * Alpha) div 255;
          end;
        end;
      except
        Result.Free;
        raise;
      end;
    end;

    ImageFactory := nil; // Redundant?
  end;
end;

procedure DriveInfoInit(ADriveInfo: TDriveInfoInt);
begin
  InitializeCriticalSection(ThreadLock);
  if not Assigned(DriveInfo) then
  begin
    DriveInfo := ADriveInfo;
    InternalWindowHandle := Classes.AllocateHWnd(DriveInfo.InternalWndProc);
  end;
end;

initialization

finalization
  if Assigned(DriveInfo) then
  begin
    EnterCriticalSection(ThreadLock);
    Classes.DeallocateHWnd(InternalWindowHandle);
    InternalWindowHandle := 0;
    DriveInfo.Free;
    DriveInfo := nil;
    LeaveCriticalSection(ThreadLock);
  end;
  DeleteCriticalSection(ThreadLock);
end.
