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
  SysUtils, Windows, Forms, ShlObj, PIDL;

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

procedure UnifyDateTimePrecision(var DateTime1: TDateTime; var DateTime2: TDateTime);

//from math.pas of VCL
{function Min(const A, B: Integer): Integer; overload;
function Min(const A, B: Int64): Int64; overload;
function Min(const A, B: Single): Single; overload;
function Min(const A, B: Double): Double; overload;
function Min(const A, B: Extended): Extended; overload;
function Max(const A, B: Integer): Integer; overload;
function Max(const A, B: Int64): Int64; overload;
function Max(const A, B: Single): Single; overload;
function Max(const A, B: Double): Double; overload;
function Max(const A, B: Extended): Extended; overload;}

resourcestring
  SNoValidPath = 'Can''t find any valid path.';
  SUcpPathsNotSupported = 'UNC paths are not supported.';

implementation

uses
  IEDriveInfo, DateUtils;

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

procedure UnifyDateTimePrecision(var DateTime1: TDateTime; var DateTime2: TDateTime);

  function Unify(var V1, V2: Word): Boolean;
  begin
    Result := (V1 = 0) or (V2 = 0);
    if Result then
    begin
      V1 := 0;
      V2 := 0;
    end;
  end;

var
  Y1, M1, D1, H1, N1, S1, MS1: Word;
  Y2, M2, D2, H2, N2, S2, MS2: Word;
  Changed: Boolean;
begin
  DecodeDateTime(DateTime1, Y1, M1, D1, H1, N1, S1, MS1);
  DecodeDateTime(DateTime2, Y2, M2, D2, H2, N2, S2, MS2);
  Changed := Unify(MS1, MS2);
  if Changed and Unify(S1, S2) and Unify(N1, N2) and Unify(H1, H2) and
     Unify(D1, D2) and Unify(M1, M2) then Unify(Y1, Y2);
  if Changed then
  begin
    DateTime1 := EncodeDate(Y1, M1, D1) + EncodeTime(H1, N1, S1, MS1);
    DateTime2 := EncodeDate(Y2, M2, D2) + EncodeTime(H2, N2, S2, MS2);
  end;
end;

{function Min(const A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: Int64): Int64;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: Single): Single;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: Double): Double;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: Extended): Extended;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Int64): Int64;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Single): Single;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Double): Double;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Extended): Extended;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;
    }
initialization
  InitDriveSpacePtr;
end.
