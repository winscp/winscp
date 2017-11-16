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
  TDateTimePrecision = (tpNone, tpDay, tpMinute, tpSecond, tpMillisecond);
  // order choosen so that for previous bool value, false maps to fbNone,
  // and true maps to new default fbKilobytes, although functionaly it is fbShort
  TFormatBytesStyle = (fbNone, fbKilobytes, fbShort);

function CheckFileExists(FileName: string): Boolean;
function DirExists(Dir: string): Boolean; overload;
function ExtractFileNameOnly(Name: string): string;
function FileOrDirExists(FileName: string): Boolean;
function FormatBytes(Bytes: Int64; Style: TFormatBytesStyle = fbShort; UseUnitsForBytes: Boolean = True): string;
function FormatPanelBytes(Bytes: Int64; Style: TFormatBytesStyle): string;
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

function FormatLastOSError(Message: string): string;

resourcestring
  SNoValidPath = 'Can''t find any valid path.';
  SUcpPathsNotSupported = 'UNC paths are not supported.';
  SByte = 'B';
  SKiloByte = 'KB';
  SMegaByte = 'MB';
  SGigaByte = 'GB';

implementation

uses
  IEDriveInfo, DateUtils, ShellApi, SysConst, PasTools, Math;

function AnyValidPath: string;
var
  Drive: TDrive;
begin
  for Drive := 'C' to 'Z' do
    if DriveInfo[Drive].Valid and
       (DriveInfo[Drive].DriveType = DRIVE_FIXED) and
       DirectoryExists(ApiPath(Drive + ':\')) then
    begin
      Result := Drive + ':\';
      Exit;
    end;
  for Drive := 'C' to 'Z' do
    if DriveInfo[Drive].Valid and
       (DriveInfo[Drive].DriveType = DRIVE_REMOTE) and
       DirectoryExists(ApiPath(Drive + ':\')) then
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
    Result := (FindFirst(ApiPath(FileName), faAnyFile, SRec) = 0);
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
    AssignFile(F, ApiPath(FileName));
    Reset(F, 1);
    Result := IOResult = 0;
    if Result then
      CloseFile(F);
  finally
    System.FileMode := SaveFileMode;
  end;
end; {CheckFileExists}

function DirExists(Dir: string): Boolean;
var
  SRec: TSearchRec;
begin
  Result := ((Length(Dir) <= 3) and (Length(Dir) >= 2)) and (Dir[2] = ':');
  if not Result then
  begin
    if FindFirst(ApiPath(Dir), faAnyFile, SRec) = 0 then
    begin
      Result := (SRec.Attr and faDirectory <> 0);
    end;
    SysUtils.FindClose(SRec);
  end;
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

// Windows Explorer size formatting
// size  properties/status bar      size column
// 1023             1023 bytes             1 KB
// 1 KB                1,00 KB             1 KB
// 2 KB                2,00 KB             2 KB
// 2 KB + 1 B          2,00 KB             3 KB
// 2 KB + 12 B         2,01 KB             3 KB
// 10 KB - 1B          9,99 KB            10 KB
// 10 KB              10,0  KB            10 KB
// 12 KB              12,0  KB            12 KB
// 12 KB + 1 B        12,0  KB            13 KB
// 12 KB + 12 B       12,0  KB            13 KB
// 12 KB + 128 B      12,1  KB            13 KB
// 100 KB            100    KB           100 KB
// 100 KB + 1 B      100    KB           101 KB
// 500 KB            500    KB           500 KB
// 1000 KB - 1 B     999    KB         1 000 KB
// 1000 KB             0,97 MB         1 000 KB
// 1 MB                1,00 MB         1 024 KB
// 1000 MB - 1 B     999    MB     1 024 000 KB
// 1000 MB             0,97 GB     1 024 000 KB
// 1 GB - 1            0,99 GB     1 048 576 KB
// 1 GB                1,00 GB     1 048 576 KB
// 1 GB + 10 MB        1,00 GB     1 058 816 KB
// 1 GB + 12 MB        1,01 GB     1 060 864 KB

function FormatBytes(Bytes: Int64; Style: TFormatBytesStyle; UseUnitsForBytes: Boolean): string;
var
  SizeUnit: string;
  Value: Int64;
  Order: Int64;
  EValue: Extended;
  Format: string;
begin
  if (Style = fbNone) or ((Style = fbShort) and (Bytes < Int64(1024))) then
  begin
    EValue := Bytes;
    Format := '#,##0';
    if UseUnitsForBytes then
      Format := Format + ' "' + SByte + '"';
  end
    else
  if Style = fbKilobytes then
  begin
    Value := Bytes div 1024;
    if (Bytes mod 1024) > 0 then
      Inc(Value);
    EValue := Value;
    Format := '#,##0 "' + SKiloByte + '"';
  end
    else
  begin
    if Bytes < Int64(1000*1024) then
    begin
      Order := 1024;
      SizeUnit := SKiloByte;
    end
      else
    if Bytes < Int64(1000*1024*1024) then
    begin
      Order := 1024*1024;
      SizeUnit := SMegaByte;
    end
      else
    begin
      Order := 1024*1024*1024;
      SizeUnit := SGigaByte;
    end;

    EValue := Bytes / Order;
    if EValue >= 100 then
    begin
      EValue := Floor(EValue);
      Format := '#,##0';
    end
      else
    if EValue >= 10 then
    begin
      EValue := Floor(EValue * 10) / 10;
      Format := '#0.0';
    end
      else
    begin
      EValue := Floor(EValue * 100) / 100;
      Format := '0.00';
    end;

    Format := Format + ' "' + SizeUnit + '"';
  end;

  Result := FormatFloat(Format, EValue);
end;

function FormatPanelBytes(Bytes: Int64; Style: TFormatBytesStyle): string;
begin
  Result := FormatBytes(Bytes, Style, (Style <> fbNone));
end;

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

// duplicated in RemoteFiles.cpp
procedure ReduceDateTimePrecision(var DateTime: TDateTime;
  Precision: TDateTimePrecision);
var
  Y, M, D, H, N, S, MS: Word;
begin
  if Precision = tpNone then DateTime := 0
    else
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
  if Result then
  begin
    SetLength(Path, StrLen(PChar(Path)));
  end
    else
  begin
    Path := '';
  end;
end;

function SpecialFolderLocation(Folder: Integer; var Path: string): Boolean;
var
  PIDL: PItemIDList;
begin
  Result := SpecialFolderLocation(Folder, Path, PIDL);
end;

function FormatLastOSError(Message: string): string;
var
  LastError: Integer;
begin
  Result := Message;
  LastError := GetLastError;
  if LastError <> 0 then
    Result := Result + #13#10 + #13#10 + Format(SOSError, [LastError, SysErrorMessage(LastError), '']);
end;

initialization
end.
