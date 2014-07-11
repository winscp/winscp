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
function DirExists(Dir: string; var Attrs: Integer): Boolean; overload;
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
function ShellImageList(Owner: TComponent; Flags: UINT): TImageList;

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
  IEDriveInfo, DateUtils, ShellApi, SysConst, PasTools;

function AnyValidPath: string;
var
  Drive: TDrive;
begin
  for Drive := 'C' to 'Z' do
    if (DriveInfo[Drive].DriveType = DRIVE_FIXED) and
       DirectoryExists(ApiPath(Drive + ':\')) then
    begin
      Result := Drive + ':\';
      Exit;
    end;
  for Drive := 'C' to 'Z' do
    if (DriveInfo[Drive].DriveType = DRIVE_REMOTE) and
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

function DirExists(Dir: string; var Attrs: Integer): Boolean;
var
  SRec: TSearchRec;
begin
  Result := ((Length(Dir) <= 3) and (Length(Dir) >= 2)) and (Dir[2] = ':');
  if Result then Attrs := 0
    else
  begin
    if FindFirst(ApiPath(Dir), faAnyFile, SRec) = 0 then
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

function FormatBytes(Bytes: Int64; Style: TFormatBytesStyle; UseUnitsForBytes: Boolean): string;
var
  SizeUnit: string;
  Value: Extended;
begin
  if (Style = fbNone) or ((Style = fbShort) and (Bytes < Int64(100*1024))) then
  begin
    Value := Bytes;
    if UseUnitsForBytes then
      SizeUnit := SByte;
  end
    else
  if (Style = fbKilobytes) or (Bytes < Int64(100*1024*1024)) then
  begin
    Value := Bytes / 1024;
    SizeUnit := SKiloByte;
  end
    else
  if Bytes < Int64(Int64(100)*1024*1024*1024) then
  begin
    Value := Bytes / (1024*1024);
    SizeUnit := SMegaByte;
  end
    else
  begin
    Value := Bytes / Int64(1024*1024*1024);
    SizeUnit := SGigaByte;
  end;

  Result := FormatFloat('#,##0', Value);
  if SizeUnit <> '' then
    Result := Result + ' ' + SizeUnit;
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

function FormatLastOSError(Message: string): string;
var
  LastError: Integer;
begin
  Result := Message;
  LastError := GetLastError;
  if LastError <> 0 then
    Result := Result + #13#10 + #13#10 + Format(SOSError, [LastError, SysErrorMessage(LastError)]);
end;

initialization
end.
