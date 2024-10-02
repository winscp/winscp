{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclUnitVersioning.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen.                                 }
{ Portions created by Andreas Hausladen are Copyright (C) Andreas Hausladen. All rights reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Andreas Hausladen (ahuser)                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ A unit version information system. It collects information from prepared units by each module.   }
{ It also works with units in DLLs.                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclUnitVersioning;

{$I jcl.inc}

interface

uses
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  System.SysUtils, System.Contnrs;
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Contnrs;
  {$ENDIF ~HAS_UNITSCOPE}

type
  PUnitVersionInfo = ^TUnitVersionInfo;
  TUnitVersionInfo = record
    RCSfile: PChar;   // $'RCSfile$
    Revision: PChar;  // $'Revision$
    Date: PChar;      // $'Date$     in UTC (GMT)
    LogPath: PChar;   // logical file path
    Extra: PChar;     // user defined string
    Data: Pointer;    // user data
  end;

  TUnitVersion = class(TObject)
  private
    FInfo: PUnitVersionInfo;
  public
    constructor Create(AInfo: PUnitVersionInfo);
    function RCSfile: string;
    function Revision: string;
    function Date: string;
    function Extra: string;
    function LogPath: string;
    function Data: Pointer;
    function DateTime: TDateTime;
    function Summary: string;
  end;

  TUnitVersioningModule = class(TObject)
  private
    FInstance: THandle;
    FItems: TObjectList;

    function GetItems(Index: Integer): TUnitVersion;
    function GetCount: Integer;

    procedure Add(Info: PUnitVersionInfo);
    function IndexOfInfo(Info: PUnitVersionInfo): Integer;
  public
    constructor Create(AInstance: THandle);
    destructor Destroy; override;

    function IndexOf(const RCSfile: string; const LogPath: string = '*'): Integer;
    function FindUnit(const RCSfile: string; const LogPath: string = '*'): TUnitVersion;

    property Instance: THandle read FInstance;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TUnitVersion read GetItems; default;
  end;

  TCustomUnitVersioningProvider = class(TObject)
  public
    constructor Create; virtual;
    procedure LoadModuleUnitVersioningInfo(Instance: THandle); virtual;
    procedure ReleaseModuleUnitVersioningInfo(Instance: THandle); virtual;
  end;

  TUnitVersioningProviderClass = class of TCustomUnitVersioningProvider;

  TUnitVersioning = class(TObject)
  private
    FModules: TObjectList;
    FProviders: TObjectList;

    function GetItem(Index: Integer): TUnitVersion;
    function GetCount: Integer;
    function GetModuleCount: Integer;
    function GetModule(Index: Integer): TUnitVersioningModule;

    procedure UnregisterModule(Module: TUnitVersioningModule); overload;
    procedure ValidateModules;
    // These two methods must be virtual because they can be invoked by a DLL.
    // Static linking would mean that the DLL's TUnitVersioning methods handle
    // the call which leads to an access violation.
    procedure Add(Instance: THandle; Info: PUnitVersionInfo); virtual;
    procedure UnregisterModule(Instance: THandle); overload; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterProvider(AProviderClass: TUnitVersioningProviderClass);
    procedure LoadModuleUnitVersioningInfo(Instance: THandle);

    function IndexOf(const RCSfile: string; const LogPath: string = '*'): Integer;
    function FindUnit(const RCSfile: string; const LogPath: string = '*'): TUnitVersion;

    // units by modules
    property ModuleCount: Integer read GetModuleCount;
    property Modules[Index: Integer]: TUnitVersioningModule read GetModule;

    // all units
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TUnitVersion read GetItem; default;
  end;

procedure RegisterUnitVersion(Instance: THandle; const Info: TUnitVersionInfo);
procedure UnregisterUnitVersion(Instance: THandle);

function GetUnitVersioning: TUnitVersioning;

procedure ExportUnitVersioningToFile(iFileName : string);

const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
  );

implementation

uses
  // make TObjectList functions inlined
  {$IFDEF HAS_UNITSCOPE}
  System.Types, // inlining of TObjectList.Remove
  System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  JclSysUtils, JclSynch;

// Delphi 5 does not know this function //(usc) D6/7 Per does have StartsWith
// a fast version of Pos(SubStr, S) = 1
function StartsWith(const SubStr, S: string): Boolean;
var
  I, Len: Integer;
begin
  Result := False;
  Len := Length(SubStr);
  if Len <= Length(S) then
  begin
    for I := 1 to Len do
      if S[I] <> SubStr[I] then
        Exit;
    Result := True;
  end;
end;

function CompareFilenames(const Fn1, Fn2: string): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := CompareText(Fn1, Fn2);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Result := CompareStr(Fn1, Fn2);
  {$ENDIF UNIX}
end;

//=== { TUnitVersion } =======================================================

constructor TUnitVersion.Create(AInfo: PUnitVersionInfo);
begin
  inherited Create;
  FInfo := AInfo;
end;

function TUnitVersion.RCSfile: string;
var
  I, P: Integer;
begin
  Result := Trim(FInfo.RCSfile);
  // the + is to have CVS not touch the string
  if StartsWith('$' + 'RCSfile: ', Result) then // a CVS command
  begin
    Delete(Result, 1, 10);
    Delete(Result, Length(Result) - 1, 2);
    for I := Length(Result) downto 1 do
      if Result[I] = ',' then
      begin
        Delete(Result, I, MaxInt);
        Break;
      end;
  end;
  // the + is to have SVN not touch the string
  if StartsWith('$' + 'URL: ', Result) then // a SVN command
  begin
    Delete(Result, 1, 6);
    Delete(Result, Length(Result) - 1, 2);
    { TODO -oUSc : Is there any need for a function that returns the URL? }
    P := Pos('/', Result);
    while P > 0 do
    begin
      Delete(Result, 1, P);
      P := Pos('/', Result);
    end;
  end;
end;

function TUnitVersion.Revision: string;
begin
  Result := Trim(FInfo.Revision);
  if StartsWith('$' + 'Revision: ', Result) then // a CVS command
    Result := Copy(Result, 12, Length(Result) - 11 - 2);
end;

function TUnitVersion.Date: string;
begin
  Result := Trim(FInfo.Date);
  if StartsWith('$' + 'Date: ', Result) then // a CVS command
  begin
    Delete(Result, 1, 7);
    Delete(Result, Length(Result) - 1, 2);
  end;
end;

function TUnitVersion.Data: Pointer;
begin
  Result := FInfo.Data;
end;

function TUnitVersion.Extra: string;
begin
  Result := Trim(FInfo.Extra);
end;

function TUnitVersion.LogPath: string;
begin
  Result := Trim(FInfo.LogPath);
end;

function TUnitVersion.DateTime: TDateTime;
var
  Ps: Integer;
  S: string;
  Error: Integer;
  Year, Month, Day, Hour, Minute, Second: Word;
  TimeSep: Char;
begin
  Result := 0;
  S := Date;

  // date:   yyyy/mm/dd | yyyy-mm-dd | mm/dd/yyyy | mm-dd-yyyy | dd.mm.yyyy
  Ps := Pos('/', S);
  if Ps = 0 then
    Ps := Pos('-', S);
  if Ps <> 0 then
  begin
    if Ps = 5 then
    begin
      // yyyy/mm/dd  |  yyyy-mm-dd
      Val(Copy(S, 1, 4), Year, Error);
      Val(Copy(S, 6, 2), Month, Error);
      Val(Copy(S, 9, 2), Day, Error);
    end
    else
    begin
      // mm/dd/yyyy  |  mm-dd-yyyy
      Val(Copy(S, 1, 2), Month, Error);
      Val(Copy(S, 4, 2), Day, Error);
      Val(Copy(S, 7, 4), Year, Error);
    end;
  end
  else
  begin
    Ps := Pos('.', S);
    if Ps <> 0 then
    begin
      // dd.mm.yyyy
      Val(Copy(S, 1, 2), Day, Error);
      Val(Copy(S, 4, 2), Month, Error);
      Val(Copy(S, 7, 4), Year, Error);
    end
    else
      Exit;
  end;

  // time:   hh:mm:ss  |  hh/mm/ss
  Ps := Pos(' ', S);
  S := Trim(Copy(S, Ps + 1, MaxInt));

  Ps := Pos(':', S);
  if Ps <> 0 then
    TimeSep := ':'
  else
  begin
    Ps := Pos('/', S);
    TimeSep := '/';
  end;
  Val(Copy(S, 1, Ps - 1), Hour, Error);
  Delete(S, 1, Ps);
  Ps := Pos(TimeSep, S);
  Val(Copy(S, 1, Ps - 1), Minute, Error);
  Delete(S, 1, Ps);
  Ps := Pos(TimeSep, S);
  if Ps = 0 then
    Ps := Length(S) + 1;
  Val(Copy(S, 1, Ps - 1), Second, Error);

  Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
end;

function TUnitVersion.Summary: string;
begin
  Result := LogPath + #9 + RCSFile + #9 + Revision + #9 + Date;
  if Extra <> '' then
    Result := Result + #9 + Extra;
end;

//=== { TUnitVersioningModule } ==============================================

constructor TUnitVersioningModule.Create(AInstance: THandle);
begin
  inherited Create;
  FInstance := AInstance;
  FItems := TObjectList.Create;
end;

destructor TUnitVersioningModule.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TUnitVersioningModule.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TUnitVersioningModule.GetItems(Index: Integer): TUnitVersion;
begin
  Result := TUnitVersion(FItems[Index]);
end;

procedure TUnitVersioningModule.Add(Info: PUnitVersionInfo);
begin
  FItems.Add(TUnitVersion.Create(Info));
end;

function TUnitVersioningModule.IndexOfInfo(Info: PUnitVersionInfo): Integer;
begin
  for Result := 0 to FItems.Count - 1 do
    if Items[Result].FInfo = Info then
      Exit;
  Result := -1;
end;

function TUnitVersioningModule.FindUnit(const RCSfile: string; const LogPath: string): TUnitVersion;
var
  Index: Integer;
begin
  Index := IndexOf(RCSfile, LogPath);
  if Index <> -1 then
    Result := Items[Index]
  else
    Result := nil;
end;

function TUnitVersioningModule.IndexOf(const RCSfile: string; const LogPath: string): Integer;
var
  Item: TUnitVersion;
begin
  for Result := 0 to FItems.Count - 1 do
  begin
    Item := Items[Result];
    if CompareFilenames(Item.RCSfile, RCSfile) = 0 then
      if LogPath = '*' then
        Exit
      else
      if CompareFilenames(LogPath, Trim(Item.LogPath)) = 0 then
        Exit;
  end;
  Result := -1;
end;

//=== { TCustomUnitVersioningProvider } ======================================

constructor TCustomUnitVersioningProvider.Create;
begin
  inherited Create;
end;

procedure TCustomUnitVersioningProvider.LoadModuleUnitVersioningInfo(Instance: THandle);
begin
//
end;

procedure TCustomUnitVersioningProvider.ReleaseModuleUnitVersioningInfo(Instance: THandle);
begin
//
end;

//=== { TUnitVersioning } ====================================================

constructor TUnitVersioning.Create;
begin
  inherited Create;
  FModules := TObjectList.Create;
  FProviders := TObjectList.Create;
end;

destructor TUnitVersioning.Destroy;
begin
  FProviders.Free;
  FModules.Free;
  inherited Destroy;
end;

procedure TUnitVersioning.Add(Instance: THandle; Info: PUnitVersionInfo);
var
  I: Integer;
  Module: TUnitVersioningModule;
begin
  for I := 0 to FModules.Count - 1 do
  begin
    Module := Modules[I];
    if Module.Instance = Instance then
    begin
      if Module.IndexOfInfo(Info) = -1 then
        Module.Add(Info);
      Exit;
    end;
  end;
  // create a new module entry
  Module := TUnitVersioningModule.Create(Instance);
  FModules.Add(Module);
  Module.Add(Info);
end;

procedure TUnitVersioning.UnregisterModule(Instance: THandle);
var
  I: Integer;
begin
  for I := FModules.Count - 1 downto 0 do
    if Modules[I].Instance = Instance then
    begin
      FModules.Delete(I);
      Break;
    end;
  for I := 0 to FProviders.Count -1 do
    TCustomUnitVersioningProvider(FProviders[I]).ReleaseModuleUnitVersioningInfo(Instance);
end;

procedure TUnitVersioning.UnregisterModule(Module: TUnitVersioningModule);
begin
  FModules.Remove(Module);
end;

function TUnitVersioning.GetCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  ValidateModules;
  for I := 0 to FModules.Count - 1 do
    Inc(Result, Modules[I].Count);
end;

function TUnitVersioning.GetItem(Index: Integer): TUnitVersion;
var
  Cnt, I: Integer;
  Module: TUnitVersioningModule;
begin
  Result := nil;
  ValidateModules;
  Cnt := 0;
  for I := 0 to FModules.Count - 1 do
  begin
    Module := Modules[I];
    if Index < Cnt + Module.Count then
    begin
      Result := Module.Items[Index - Cnt];
      Break;
    end;
    Inc(Cnt, Module.Count);
  end;
end;

function TUnitVersioning.GetModuleCount: Integer;
begin
  ValidateModules;
  Result := FModules.Count;
end;

function TUnitVersioning.GetModule(Index: Integer): TUnitVersioningModule;
begin
  Result := TUnitVersioningModule(FModules[Index]);
end;

{$UNDEF FPCUNIX}   // Temporary, will move to .inc's in time.
{$IFDEF FPC}
 {$IFDEF UNIX}
 {$DEFIN FPCUNIX}
{$ENDIF}
{$ENDIF}

procedure TUnitVersioning.ValidateModules;
var
  I: Integer;
  {$IFNDEF FPCUNIX}
  Buffer: string;
  {$ENDIF ~FPCUNIX}
  Module: TUnitVersioningModule;
begin
  {$IFNDEF FPCUNIX}
  SetLength(Buffer, 1024);
  {$ENDIF ~FPCUNIX}
  for I := FModules.Count - 1 downto 0 do
  begin
    Module := Modules[I];
    {$IFDEF FPCUNIX}
    if dlsym(Pointer(Module.Instance), '_init') = nil then
    {$ELSE ~FPCUNIX}
    if GetModuleFileName(Module.Instance, PChar(Buffer), 1024) = 0 then
    {$ENDIF ~FPCUNIX}
      // This module is no more in memory but has not unregistered itself so
      // unregister it here.
      UnregisterModule(Module);
  end;
end;

function TUnitVersioning.FindUnit(const RCSfile: string; const LogPath: string): TUnitVersion;
var
  I: Integer;
begin
  for I := 0 to FModules.Count - 1 do
  begin
    Result := Modules[I].FindUnit(RCSfile, LogPath);
    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

function TUnitVersioning.IndexOf(const RCSfile: string; const LogPath: string): Integer;
var
  I, Cnt, Index: Integer;
  Module: TUnitVersioningModule;
begin
  Result := -1;
  Cnt := 0;
  for I := 0 to FModules.Count - 1 do
  begin
    Module := Modules[I];
    Index := Module.IndexOf(RCSfile, LogPath);
    if Index <> -1 then
    begin
      Result := Cnt + Index;
      Break;
    end;
    Inc(Cnt, Module.Count);
  end;
end;

procedure TUnitVersioning.RegisterProvider(AProviderClass: TUnitVersioningProviderClass);
var
  I, Idx: Integer;
begin
  Idx := -1;
  for I := 0 to FProviders.Count - 1 do
    if TObject(FProviders[I]).ClassType = AProviderClass then
    begin
      Idx := I;
      Break;
    end;
  if Idx = -1 then
    FProviders.Add(AProviderClass.Create);
end;

procedure TUnitVersioning.LoadModuleUnitVersioningInfo(Instance: THandle);
var
  I: Integer;
begin
  for I := 0 to FProviders.Count - 1 do
    TCustomUnitVersioningProvider(FProviders[I]).LoadModuleUnitVersioningInfo(Instance);
end;

type
  PUnitVersioning = ^TUnitVersioning;

var
  UnitVersioningOwner: Boolean = False;
  GlobalUnitVersioning: TUnitVersioning = nil;
  UnitVersioningNPA: PUnitVersioning = nil;
  UnitVersioningMutex: TJclMutex;
  UnitVersioningFinalized: Boolean = False;

function GetUnitVersioning: TUnitVersioning;
begin
  if UnitVersioningFinalized then
  begin
    Result := nil;
    Exit;
  end;

  if UnitVersioningMutex = nil then
    UnitVersioningMutex := TJclMutex.Create(nil, False, 'MutexNPA_UnitVersioning_' + IntToStr(GetCurrentProcessId));

  if GlobalUnitVersioning = nil then
  begin
    UnitVersioningMutex.WaitFor(INFINITE);
    try
      if UnitVersioningNPA = nil then
        SharedGetMem(UnitVersioningNPA, 'ShmNPA_UnitVersioning_' + IntToStr(GetCurrentProcessId), SizeOf(TUnitVersioning));
      if UnitVersioningNPA <> nil then
      begin
        GlobalUnitVersioning := UnitVersioningNPA^;
        if GlobalUnitVersioning = nil then
        begin
          GlobalUnitVersioning := TUnitVersioning.Create;
          UnitVersioningNPA^ := GlobalUnitVersioning;
          UnitVersioningOwner := True;
        end;
      end
      else
      begin
        GlobalUnitVersioning := TUnitVersioning.Create;
        UnitVersioningOwner := True;
      end;
    finally
      UnitVersioningMutex.Release;
    end;
  end
  else
  if UnitVersioningNPA <> nil then
  begin
    UnitVersioningMutex.WaitFor(INFINITE);
    try
      GlobalUnitVersioning := UnitVersioningNPA^; // update (maybe the owner has destroyed the instance)
    finally
      UnitVersioningMutex.Release;
    end;
  end;
  Result := GlobalUnitVersioning;
end;

procedure FinalizeUnitVersioning;
begin
  UnitVersioningFinalized := True;
  try
    if UnitVersioningNPA <> nil then
      SharedCloseMem(UnitVersioningNPA);
    if (GlobalUnitVersioning <> nil) and UnitVersioningOwner then
      FreeAndNil(GlobalUnitVersioning)
    else
      GlobalUnitVersioning := nil;
  except
    // ignore - should never happen
  end;
  FreeAndNil(UnitVersioningMutex);
end;

procedure RegisterUnitVersion(Instance: THandle; const Info: TUnitVersionInfo);
var
  UnitVersioning: TUnitVersioning;
begin
  UnitVersioning := GetUnitVersioning;
  if Assigned(UnitVersioning) then
    UnitVersioning.Add(Instance, @Info);
end;

procedure UnregisterUnitVersion(Instance: THandle);
var
  UnitVersioning: TUnitVersioning;
begin
  UnitVersioning := GetUnitVersioning;
  if Assigned(UnitVersioning) then
    UnitVersioning.UnregisterModule(Instance);
end;

procedure ExportUnitVersioningToFile(iFileName : string);
var
  I: Integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    for I := 0 to GetUnitVersioning.Count - 1 do
      sl.Add(GetUnitVersioning.Items[I].Summary);
    sl.Sort;
    sl.SaveToFile(iFileName);
  finally
    sl.Free;
  end;
end;

initialization
{$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
{$ENDIF UNITVERSIONING}

finalization
{$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
  FinalizeUnitVersioning;

end.
