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
  Windows,
  Registry,
  SysUtils,
  Classes,
  ComCtrls,
  ShellApi,
  ShlObj,
  CommCtrl,
  Forms,
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


{---------------------------------------------------------------}
  TDriveInfo = Class(TObject)
{---------------------------------------------------------------}
  Private
{---------------------------------------------------------------}
    FData : Array[FirstDrive..LastDrive] Of TDriveInfoRec;
    FNoDrives : DWORD;
    FDesktop  : IShellFolder;
    Function GetData(Drive : TDrive) : TDriveInfoRec;
{---------------------------------------------------------------}
  Public
{---------------------------------------------------------------}
    Property Data[Drive : TDrive]          : TDriveInfoRec Read GetData; default;

    Function GetImageIndex(Drive : TDrive) : Integer;
    Function GetDisplayName(Drive : TDrive) : String;
    Function GetPrettyName(Drive : TDrive) : String;
    Function GetLongPrettyName(Drive : TDrive) : String;
    Function ReadDriveStatus(Drive : TDrive; Flags : Integer) : Boolean;
    Constructor Create;
    Destructor Destroy;  Override;
  End;
{---------------------------------------------------------------}

Function GetShellFileName (Const Name : String ) : String;  Overload;
Function GetShellFileName (PIDL : PItemIDList)   : String;  OverLoad;            
Function GetNetWorkName   (Drive : Char) : String;


{Central drive information object instance of TDriveInfo}
Var DriveInfo : TDriveInfo;

ResourceString
  ErrorInvalidDrive = '%s is a invalid drive letter.';


{---------------------------------------------------------------}
implementation
{---------------------------------------------------------------}

uses
  Math;

// ===========================================================
// Class TDriveInfo:
// ===========================================================

Constructor TDriveInfo.Create;
Var Drive    : TDrive;
    Reg      : TRegistry;

Begin
  Inherited Create;
  FNoDrives := 0;
  Reg := TRegistry.Create;
  Try
    IF Reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Policies\Explorer') Then
       Reg.ReadBinaryData('NoDrives', FNoDrives, SizeOf(FNoDrives));
  Except
    Try
       FNoDrives := Reg.ReadInteger('NoDrives');
    Except
    End;
  End;
  Reg.Free;

  FDesktop := NIL;

  For Drive := FirstDrive To LastDrive Do
  With FData[Drive] Do
  Begin
   PIDL        := NIL;
   Init        := False;
   DriveType   := Windows.GetDriveType(PChar(Drive + ':\'));
   Valid       := Not Bool((1 SHL (Ord(Drive) - 65)) And FNoDrives)
                  And (DriveType in [DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_CDROM, DRIVE_RAMDISK, DRIVE_REMOTE]);
   Init        := False;
   DriveReady  := False;
   DisplayName := '';
   PrettyName  := '';
   LongPrettyName := '';
   FileSystemName := '';
   DriveSerial := 0;
   Size        := -1;
   ImageIndex  := 0;
   FileSystemFlags := 0;
   MaxFileNameLength := 0;
  End;
End; {TDriveInfo.Create}


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

Function TDriveInfo.GetImageIndex(Drive : TDrive) : Integer;
Begin
  if (Drive < FirstDrive) Or (Drive > LastDrive) then
    Raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  Result := 0;

  IF FData[Drive].Valid Then
  Begin
    IF (FData[Drive].ImageIndex = 0) Then
    ReadDriveStatus(Drive, dsImageIndex);
    Result := FData[Drive].ImageIndex;
  End;
End; {TDriveInfo.GetImageIndex}


Function TDriveInfo.GetDisplayName(Drive : TDrive) : String;
Begin
  if (Drive < FirstDrive) Or (Drive > LastDrive) then
    Raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  Result := Drive + ':';

  IF FData[Drive].Valid Then
  Begin
    IF (Length(FData[Drive].DisplayName) = 0) Then
    ReadDriveStatus(Drive, dsDisplayName);
    Result := FData[Drive].DisplayName;
  End;
End; {TDriveInfo.GetDisplayname}


Function TDriveInfo.GetPrettyName(Drive : TDrive) : String;
Begin
  if (Drive < FirstDrive) Or (Drive > LastDrive) then
    Raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  Result := Drive + ':';

  IF FData[Drive].Valid Then
  Begin
    IF (Length(FData[Drive].PrettyName) = 0) Then
    ReadDriveStatus(Drive, dsDisplayName);
    Result := FData[Drive].PrettyName;
  End;
End; {TDriveInfo.GetPrettyName}


Function TDriveInfo.GetLongPrettyName(Drive : TDrive) : String;
Begin
  if (Drive < FirstDrive) Or (Drive > LastDrive) then
    Raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  Result := Drive + ':';

  IF FData[Drive].Valid Then
  Begin
    IF (Length(FData[Drive].PrettyName) = 0) Then
    ReadDriveStatus(Drive, dsDisplayName);
    Result := FData[Drive].LongPrettyName;
  End;
End; {TDriveInfo.GetLongPrettyName}



Function TDriveInfo.GetData(Drive: TDrive) : TDriveInfoRec;
Begin
  if not (Upcase(Drive) in ['A'..'Z']) then
    Raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  Result := FData[Upcase(Drive)];
End; {TDriveInfo.GetData}


Function TDriveInfo.ReadDriveStatus(Drive : TDrive; Flags : Integer) : Boolean;
var  ErrorMode            : word;
     FileInfo             : TShFileInfo;
     FileSystemNameBuffer : String;
     DriveID              : String;
     CPos                 : Integer;
     WStr                 : WideString;
     Eaten                : ULONG;
     shAttr               : ULONG;

Begin
  If Not Assigned(FDesktop) Then
  SHGetDesktopFolder(FDesktop);

  Drive := Upcase(Drive);
  DriveID := '';
  if (Drive < FirstDrive) Or (Drive > LastDrive) then
    Raise EConvertError.Create(Format(ErrorInvalidDrive, [Drive]));

  With FData[Drive] Do
  Begin
    Init      := True;
    DriveType := Windows.GetDriveType(PChar(Drive + ':\'));
    Valid     := Not Bool((1 SHL (Ord(Drive) - 65)) And FNoDrives)
                 And (DriveType in [DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_CDROM, DRIVE_RAMDISK, DRIVE_REMOTE]);


    IF Valid Then
    Begin

      IF Not Assigned(PIDL) And (Drive >= FirstFixedDrive) Then
      Begin
        WStr := Drive + ':\';
        FDesktop.ParseDisplayName(Application.Handle, NIL, PWideChar(WStr), Eaten, PIDL, ShAttr);
      End;

      {Read driveStatus:}
      IF (Flags And dsSize) <> 0 Then
      Begin
        { turn off critical errors }
        ErrorMode := SetErrorMode(SEM_FailCriticalErrors or SEM_NOOPENFILEERRORBOX);
        try
         { drive 1 = a, 2 = b, 3 = c, etc. }

         Size := BaseUtils.DiskSize(Ord(Drive) - $40);
         DriveReady := Size >= 0;
         IF DriveReady Then
         Begin
           SetLength(FilesystemNamebuffer,500) ;
           SetLength(DriveID, 24);
           {Access the physical drive:}
           If GetVolumeInformation(PChar(Drive + ':\'),
                                   PChar(DriveID), 24,
                                   @DriveSerial,
                                   MaxFileNameLength,
                                   FileSystemFlags,
                                   PChar(filesystemnamebuffer), 499) Then
           Begin
             FileSystemName := StrPas(PChar(FileSystemNameBuffer));
             DriveID        := StrPas(PChar(DriveID));
           End
           Else
           Begin
             DriveSerial := 0;
             FileSystemName := '';
           End;
           SetLength(FileSystemNameBuffer, 0);
         End
         Else
         Begin
           DriveSerial := 0;
         End;
        finally    { restore old error mode }
          SetErrorMode(ErrorMode);
        end;
      End;

      {DisplayName:}
      IF (Flags And dsDisplayName <> 0)Then
      Begin
        IF DriveReady or (Flags And dsSize = 0) Then
        Begin
         {Fetch drives displayname:}
         {Due to a bug in shGetFileInfo, this function returns allways the displayname of
          the first inserted disk, even if a disk change has occured. So, better use the
          Volume ID to build the drives displayname:}
         IF (DriveType = DRIVE_CDROM) And (Length(DriveID) > 0) Then
           DisplayName := DriveID[1] + LowerCase(Copy(DriveID, 2, 24)) + ' ('+ Drive + ':)'
         Else
         Begin
           IF Assigned(PIDL) Then
             DisplayName    := GetShellFileName(PIDL)
           Else
             DisplayName    := GetShellFileName(Drive + ':\')
         End;

          PrettyName := Drive + ': ' + DisplayName;

          CPos := Pos('(' + Drive, PrettyName);
          IF CPos > 0 Then
          SetLength(PrettyName, Pred(CPos));

          IF DriveType = DRIVE_REMOTE Then
          Begin
            DriveID := GetNetWorkName(Drive);
            PrettyName     := Drive + ': ' + ExtractFileName(DriveID);
            LongPrettyName := Drive + ': ' + DriveID;
          End
          Else
          Begin
            LongPrettyName := Copy(PrettyName, 1, 3) + DisplayName;
            CPos := Pos('(' + Drive, LongPrettyName);
            IF CPos > 0 Then
            SetLength(LongPrettyName, Pred(CPos));
          End;
         End
        Else
        Begin
         DisplayName := Drive + ':';
         PrettyName  := DisplayName;
         LongPrettyName := DisplayName;
         FreePIDL(PIDL);
        End;
      End;

      {ImageIndex:}
      IF ((Flags And dsImageIndex) <> 0) And (ImageIndex < 5) Then
      Begin
        IF Assigned(PIDL) Then
        SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON Or SHGFI_PIDL)
        Else
        SHGetFileInfo(PChar(Drive + ':\'), 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
        ImageIndex := FileInfo.iIcon;
      End;
    End
    Else
    Begin
     Size        := 0;
     DriveReady  := False;
     DisplayName := '';
     PrettyName  := '';
     LongPrettyName := '';
     DriveSerial := 0;
     ImageIndex  := 0;
     IF Assigned(PIDL) Then
     FreePIDL(PIDL);
    End;
    Result := Valid And DriveReady;
  End;
End; {TDriveInfo.ReadDriveStatus}

// ===========================================================
// Other service functions and procedures:
// ===========================================================





// returns the filename as displayed by the shell
Function GetShellFileName ( const Name : string ) : String;
var sfi : TSHFileInfo;
    E   : Integer;
begin
     E := SetErrorMode(SEM_FAILCRITICALERRORS);
     Try
       If SHGetFileInfo(PChar( Name ), 0, sfi, SizeOf(TSHFileInfo), SHGFI_DISPLAYNAME) <> 0 Then
          Result := sfi.szDisplayName;
     finally
          SetErrorMode(E);
     End;
end; {GetShellFileName}


Function GetShellFileName (PIDL : PItemIDList) : String;
var sfi : TSHFileInfo;
    E   : Integer;

begin
     E := SetErrorMode(SEM_FAILCRITICALERRORS);
     Try
       If SHGetFileInfo(PChar(PIDL), 0, sfi, SizeOf(TSHFileInfo), SHGFI_PIDL Or SHGFI_DISPLAYNAME) <> 0 Then
          Result := sfi.szDisplayName;
     finally
          SetErrorMode(E);
     End;
end; {GetShellFileName}


// Gets the network UNC-Name of a mounted drive:
Function GetNetWorkName(Drive : Char) : String;
Var P : Array[0..MAX_PATH] Of Char;
    MaxLen : DWORD;

Begin
  MaxLen := MAX_PATH;
  IF WNetGetConnection(PChar(String(Drive + ':')), P, MaxLen) = NO_ERROR Then
  Result := P
  Else
  Result := '';
End; {GetNetWorkName}


// ======================================================
// Initialization
// ======================================================

Initialization
  IF Not Assigned(DriveInfo) Then
  DriveInfo := TDriveInfo.Create;


// ======================================================
// Finalization
// ======================================================
Finalization
  IF Assigned(DriveInfo) Then
  Begin
    DriveInfo.Free;
    DriveInfo := NIL;
  End;


end.
