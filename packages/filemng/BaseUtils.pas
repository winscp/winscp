unit BaseUtils;

{==================================================================

 Basic file handling utilities  /  Version 1.2  / 11.1999
 ========================================================


    Description:
    ============
    Basic utilities useful for handling files or directories.
    Used by the components TDriveView and TDirView.

    Author:
    =======
    (c) Ingo Eckel 5/1998
    Sodener Weg 38
    65812 Bad Soden
    Germany

    Modifications (for WinSCP):
    ===========================
    (c) Martin Prikryl 2001, 2002

{==================================================================}

interface

uses
  SysUtils, Windows, Forms, ShlObj, PIDL, Classes, Controls;

type
  TDateTimePrecision = (tpDay, tpMinute, tpSecond, tpMillisecond);

function CheckFileExists(FileName: string): Boolean;
function DirExists(Dir: string): Boolean; overload;
function DirExists(Dir: string; var Attrs: Integer): Boolean; overload;
function DiskSize(Drive: Byte): Int64;
function ExtractFileNameOnly(Name: string): string;
function FileOrDirExists(FileName: string): Boolean;
function FormatSize(Size: Integer): string; overload;
function FormatSize(Size: Cardinal): string; overload;
function FormatSize(Size: Int64): string; overload;
procedure FreePIDL(var PIDL: PItemIDList);
function StrContains(Str1, Str2: string): Boolean;
procedure StrTranslate(var Str: string; Code: string);
function IsUncPath(Path: string): Boolean;
function AnyValidPath: string;

procedure ReduceDateTimePrecision(var DateTime: TDateTime;
  Precision: TDateTimePrecision);
function SpecialFolderLocation(Folder: Integer; var Path: string;
  var PIDL: PItemIDList): Boolean; overload;
function SpecialFolderLocation(Folder: Integer; var Path: string): Boolean; overload;
function ShellImageList(Owner: TComponent; Flags: UINT): TImageList;
procedure Trace(Msg: string);

resourcestring
  SNoValidPath = 'Can''t find any valid path.';
  SUcpPathsNotSupported = 'UNC paths are not supported.';

implementation

uses
  IEDriveInfo, DateUtils, ShellApi;

var
  GetDiskFreeSpaceEx: function (Directory: PChar;
    var FreeAvailable, TotalSpace: TLargeInteger; TotalFree: PLargeInteger): Bool stdcall = nil;

function AnyValidPath: string;
var
  Drive: TDrive;
begin
  for Drive := 'C' to 'Z' do
    if (DriveInfo[Drive].DriveType = DRIVE_FIXED) and
       DirectoryExists(Drive + ':\') then
    begin
      Result := Drive + ':\';
      Exit;
    end;
  for Drive := 'C' to 'Z' do
    if (DriveInfo[Drive].DriveType = DRIVE_REMOTE) and
       DirectoryExists(Drive + ':\') then
    begin
      Result := Drive + ':\';
      Exit;
    end;
  raise Exception.Create(SNoValidPath);
end;

function IsUncPath(Path: string): Boolean;
begin
  Result := (Copy(Path, 1, 2) = '\\') or (Copy(Path, 1, 2) = '//');
end;

procedure StrTranslate(var Str: string; Code: string);
var
  Index: Integer;
begin
  if (Length(Code) > 0) and (Length(Code) mod 2 = 0) then
    for Index := 1 to Length(Code) div 2 do
      while Pos(Code[Index*2-1], Str) <> 0 do
        Str[Pos(Code[Index*2-1], Str)] := Code[Index*2];
end; {StrTranslate}

function StrContains(Str1, Str2: string): Boolean;
var
  Index: Integer;
begin
  for Index := 1 to Length(Str1) do
    if Pos(Str1[Index], Str2) <> 0 then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end; {StringCountains}

function FileOrDirExists(FileName: string): Boolean;
var
  SRec : TSearchRec;
begin
  if Length(FileName) = 0 then Result := False
    else
  begin
    Result := (FindFirst(FileName, faAnyFile, SRec) = 0);
    SysUtils.FindCLose(SRec);
  end;
end; {FileOrDirExists}

function CheckFileExists(FileName: string): Boolean;
var
  SaveFileMode : Integer;
  F: file;
begin
  SaveFileMode := System.FileMode;
  System.FileMode := 0;
  try
    AssignFile(F, FileName);
    Reset(F, 1);
    Result := IOResult = 0;
    if Result then
      CloseFile(F);
  finally
    System.FileMode := SaveFileMode;
  end;
end; {CheckFileExists}

function DirExists(Dir: string; var Attrs: Integer): Boolean;
var
  SRec: TSearchRec;
begin
  Result := ((Length(Dir) <= 3) and (Length(Dir) >= 2)) and (Dir[2] = ':');
  if Result then Attrs := 0
    else
  begin
    if FindFirst(Dir, faAnyFile, SRec) = 0 then
    begin
      Result := (SRec.Attr and faDirectory <> 0);
      Attrs := SRec.Attr;
    end;
    SysUtils.FindClose(SRec);
  end;
end; {DirExists}

function DirExists(Dir: string): Boolean;
var
  Dummy: Integer;
begin
  Result := DirExists(Dir, Dummy);
end; {DirExists}

function ExtractFileNameOnly(Name: string): string;
var
  Ext: string;
begin
  Result := ExtractFileName(Name);
  Ext := ExtractFileExt(Name);
  if Ext <> '' then
    Delete(Result, Length(Result)-Length(Ext)+1, Length(Ext));
end; {ExtractFileNameOnly}

function FormatSize(Size: Integer): string;
begin
  Result := FormatSize(Cardinal(Abs(Size)));
end; {FormatSize}

function FormatSize(Size: Cardinal): string;
var
  i: Integer;
  p: Integer;
begin
  p := 0;
  i := 3;
  Result := IntToStr(Size);
  while i + p < Length(Result) do
  begin
    Insert(ThousandSeparator, Result, Length(Result) - (i + p)+ 1);
    Inc(p);
    INC(i, 3);
  end;
end; {FormatSize}

function FormatSize(Size: Int64): String;
var
  i: Integer;
  p: Integer;
begin
  p := 0;
  i := 3;
  Result := IntToStr(Size);
  while i + p < Length(Result) do
  begin
    Insert(ThousandSeparator, Result, Length(Result) - (i + p)+ 1);
    Inc(p);
    Inc(i, 3);
  end;
end; {FormatSize}

procedure FreePIDL(var PIDL: PItemIDList);
begin
  if PIDL <> nil then
  begin
    try
      ShellMalloc.Free(PIDL);
      PIDL := NIL;
    except
      PIDL := NIL;
    end;
  end;
end; {FreePIDL}

function InternalGetDiskSpace(Drive: Byte;
  var TotalSpace, FreeSpaceAvailable: Int64): Bool;
var
  RootPath: array[0..4] of Char;
  RootPtr: PChar;
begin
  RootPtr := nil;
  if Drive > 0 then
  begin
    RootPath[0] := Char(Drive + $40);
    RootPath[1] := ':';
    RootPath[2] := '\';
    RootPath[3] := #0;
    RootPtr := RootPath;
  end;
  Result := GetDiskFreeSpaceEx(RootPtr, FreeSpaceAvailable, TotalSpace, nil);
end;

// This function is used if the OS doesn't support GetDiskFreeSpaceEx
function BackfillGetDiskFreeSpaceEx(Directory: PChar; var FreeAvailable,
  TotalSpace: TLargeInteger; TotalFree: PLargeInteger): Bool; stdcall;
var
  SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters: LongWord;
  Temp: Int64;
  Dir: PChar;
begin
  if Directory <> nil then
    Dir := Directory
  else
    Dir := nil;
  Result := GetDiskFreeSpaceA(Dir, SectorsPerCluster, BytesPerSector,
    FreeClusters, TotalClusters);
  Temp := SectorsPerCluster * BytesPerSector;
  FreeAvailable := Temp * FreeClusters;
  TotalSpace := Temp * TotalClusters;
end;

function DiskSize(Drive: Byte): Int64;
var
  FreeSpace: Int64;
begin
  if not InternalGetDiskSpace(Drive, Result, FreeSpace) then
    Result := -1;
end;

procedure InitDriveSpacePtr;
var
  Kernel: THandle;
begin
  Kernel := GetModuleHandle(Windows.Kernel32);
  if Kernel <> 0 then
    @GetDiskFreeSpaceEx := GetProcAddress(Kernel, 'GetDiskFreeSpaceExA');
  if not Assigned(GetDiskFreeSpaceEx) then
    GetDiskFreeSpaceEx := @BackfillGetDiskFreeSpaceEx;
end;

// duplicated in RemoteFiles.cpp
procedure ReduceDateTimePrecision(var DateTime: TDateTime;
  Precision: TDateTimePrecision);
var
  Y, M, D, H, N, S, MS: Word;
begin
  if Precision <> tpMillisecond then
  begin
    DecodeDateTime(DateTime, Y, M, D, H, N, S, MS);
    case Precision of
      tpDay:
        begin
          H := 0;
          N := 0;
          S := 0;
          MS := 0;
        end;

      tpMinute:
        begin
          S := 0;
          MS := 0;
        end;

      tpSecond:
        begin
          MS := 0;
        end;
    end;

    DateTime := EncodeDate(Y, M, D) + EncodeTime(H, N, S, MS);
  end;
end;


function SpecialFolderLocation(Folder: Integer; var Path: string;
  var PIDL: PItemIDList): Boolean;
begin
  SetLength(Path, MAX_PATH);
  Result :=
    (not Failed(SHGetSpecialFolderLocation(Application.Handle, Folder, PIDL))) and
    SHGetPathFromIDList(PIDL, PChar(Path));
  if Result then SetLength(Path, StrLen(PChar(Path)));
end;

function SpecialFolderLocation(Folder: Integer; var Path: string): Boolean;
var
  PIDL: PItemIDList;
begin
  Result := SpecialFolderLocation(Folder, Path, PIDL);
end;

function ShellImageList(Owner: TComponent; Flags: UINT): TImageList;
var
  FileInfo: TShFileInfo;
begin
  Result := TImageList.Create(Owner);
  Result.Handle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo),
      SHGFI_SYSICONINDEX or Flags);
  Result.ShareImages := True;
end;

procedure Trace(Msg: string);
const
  TRACEENV = 'WINSCPTRACE';
var
  FileName: string;
  F: Text;
begin
  FileName := GetEnvironmentVariable(TRACEENV);
  if FileName = '' then FileName := 'C:\winscptrace.log';
  AssignFile(F, FileName);
  Append(F);
  Write(F, Format('[%s] %s:%d:%s'#13#10'  %s'#13#10,
    [TimeToStr(Now), 'PAS', 0, 'unk', Msg]));
  CloseFile(F);
end;

initialization
  InitDriveSpacePtr;
end.
