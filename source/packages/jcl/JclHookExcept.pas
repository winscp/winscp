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
{ The Original Code is JclHookExcept.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) Petr Vones. All Rights Reserved.                                                   }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Petr Vones (pvones)                                                                            }
{   Robert Marquardt (marquardt)                                                                   }
{   Andreas Hausladen (ahuser)                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Exception hooking routines                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclHookExcept;

interface

{$I jcl.inc}
{$I windowsonly.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  Winapi.Windows, System.SysUtils, System.Classes;
  {$ELSE ~HAS_UNITSCOPE}
  Windows, SysUtils, Classes;
  {$ENDIF ~HAS_UNITSCOPE}

type
  // Exception hooking notifiers routines
{$IFDEF BORLAND}
  TJclExceptFilterProc = function(ExceptRecord: PExceptionRecord): Exception;
{$ENDIF BORLAND}
  TJclExceptNotifyProc = procedure(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
  TJclExceptNotifyProcEx = procedure(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean; StackPointer: Pointer);
  TJclExceptNotifyMethod = procedure(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean) of object;

  TJclExceptNotifyPriority = (npNormal, npFirstChain);

{$IFDEF BORLAND}
function JclAddExceptFilter(const FilterProc: TJclExceptFilterProc; Priority: TJclExceptNotifyPriority = npNormal): Boolean;
{$ENDIF BORLAND}
function JclAddExceptNotifier(const NotifyProc: TJclExceptNotifyProc; Priority: TJclExceptNotifyPriority = npNormal): Boolean; overload;
function JclAddExceptNotifier(const NotifyProc: TJclExceptNotifyProcEx; Priority: TJclExceptNotifyPriority = npNormal): Boolean; overload;
function JclAddExceptNotifier(const NotifyMethod: TJclExceptNotifyMethod; Priority: TJclExceptNotifyPriority = npNormal): Boolean; overload;

{$IFDEF BORLAND}
function JclRemoveExceptFilter(const FilterProc: TJclExceptFilterProc): Boolean;
{$ENDIF BORLAND}
function JclRemoveExceptNotifier(const NotifyProc: TJclExceptNotifyProc): Boolean; overload;
function JclRemoveExceptNotifier(const NotifyProc: TJclExceptNotifyProcEx): Boolean; overload;
function JclRemoveExceptNotifier(const NotifyMethod: TJclExceptNotifyMethod): Boolean;  overload;

procedure JclReplaceExceptObj(NewExceptObj: Exception);

// Exception hooking routines
function JclHookExceptions: Boolean;
function JclUnhookExceptions: Boolean;
function JclExceptionsHooked: Boolean;

function JclHookExceptionsInModule(Module: HMODULE): Boolean;
function JclUnhookExceptionsInModule(Module: HMODULE): Boolean;

// Exceptions hooking in libraries
type
  TJclModuleArray = array of HMODULE;

function JclInitializeLibrariesHookExcept: Boolean;
function JclHookedExceptModulesList(out ModulesList: TJclModuleArray): Boolean;

// Hooking routines location info helper
function JclBelongsHookedCode(Address: Pointer): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclBase,
  JclPeImage,
  JclSysInfo, JclSysUtils;

type
  PExceptionArguments = ^TExceptionArguments;
  TExceptionArguments = record
    ExceptAddr: Pointer;
    ExceptObj: Exception;
  end;

{$IFDEF BORLAND}
  TFilterItem = class(TObject)
  private
    FExceptFilterProc: TJclExceptFilterProc;
    FPriority: TJclExceptNotifyPriority;
  public
    constructor Create(const ExceptFilterProc: TJclExceptFilterProc; APriority: TJclExceptNotifyPriority);
    function DoFilterException(ExceptRecord: PExceptionRecord; out ExceptObj: Exception): Boolean;
    property Priority: TJclExceptNotifyPriority read FPriority;
  end;
{$ENDIF BORLAND}

  TNotifierItem = class(TObject)
  private
    FNotifyMethod: TJclExceptNotifyMethod;
    FNotifyProc: TJclExceptNotifyProc;
    FNotifyProcEx: TJclExceptNotifyProcEx;
    FPriority: TJclExceptNotifyPriority;
  public
    constructor Create(const NotifyProc: TJclExceptNotifyProc; Priority: TJclExceptNotifyPriority); overload;
    constructor Create(const NotifyProc: TJclExceptNotifyProcEx; Priority: TJclExceptNotifyPriority); overload;
    constructor Create(const NotifyMethod: TJclExceptNotifyMethod; Priority: TJclExceptNotifyPriority); overload;
    procedure DoNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean; StackPointer: Pointer);
    property Priority: TJclExceptNotifyPriority read FPriority;
  end;

var
  ExceptionsHooked: Boolean;
  Kernel32_RaiseException: procedure (dwExceptionCode, dwExceptionFlags,
    nNumberOfArguments: DWORD; lpArguments: PDWORD); stdcall;
  {$IFDEF BORLAND}
  SysUtils_ExceptObjProc: function (P: PExceptionRecord): Exception;
  {$ENDIF BORLAND}
  {$IFDEF FPC}
  SysUtils_ExceptProc: TExceptProc;
  {$ENDIF FPC}
  Notifiers: TThreadList;
  {$IFDEF BORLAND}
  Filters: TThreadList;
  {$ENDIF BORLAND}

{$IFDEF HOOK_DLL_EXCEPTIONS}
const
  JclHookExceptDebugHookName = '__JclHookExcept';

type
  TJclHookExceptDebugHook = procedure(Module: HMODULE; Hook: Boolean); stdcall;

  TJclHookExceptModuleList = class(TObject)
  private
    FModules: TThreadList;
  protected
    procedure HookStaticModules;
  public
    constructor Create;
    destructor Destroy; override;
    class function JclHookExceptDebugHookAddr: Pointer;
    procedure HookModule(Module: HMODULE);
    procedure List(out ModulesList: TJclModuleArray);
    procedure UnhookModule(Module: HMODULE);
  end;

var
  HookExceptModuleList: TJclHookExceptModuleList;
  JclHookExceptDebugHook: Pointer;

exports
  JclHookExceptDebugHook name JclHookExceptDebugHookName;
{$ENDIF HOOK_DLL_EXCEPTIONS}

{$STACKFRAMES OFF}

threadvar
  Recursive: Boolean;
  NewResultExc: Exception;

//=== Helper routines ========================================================

function RaiseExceptionAddress: Pointer;
begin
  Result := GetProcAddress(GetModuleHandle(kernel32), 'RaiseException');
  Assert(Result <> nil);
end;

procedure FreeThreadObjList(var TheList: TThreadList);
var
  I: Integer;
begin
  with TheList.LockList do
    try
      for I := 0 to Count - 1 do
        TObject(Items[I]).Free;
    finally
      TheList.UnlockList;
    end;
  FreeAndNil(TheList);
end;

//=== { TFilterItem } ========================================================

{$IFDEF BORLAND}
constructor TFilterItem.Create(const ExceptFilterProc: TJclExceptFilterProc; APriority: TJclExceptNotifyPriority);
begin
  FExceptFilterProc := ExceptFilterProc;
  FPriority := APriority;
end;

function TFilterItem.DoFilterException(ExceptRecord: PExceptionRecord; out ExceptObj: Exception): Boolean;
begin
  if Assigned(FExceptFilterProc) then
  begin
    ExceptObj := FExceptFilterProc(ExceptRecord);
    Result := ExceptObj <> nil;
  end
  else
    Result := False;
end;
{$ENDIF BORLAND}

//=== { TNotifierItem } ======================================================

constructor TNotifierItem.Create(const NotifyProc: TJclExceptNotifyProc; Priority: TJclExceptNotifyPriority);
begin
  inherited Create;
  FNotifyProc := NotifyProc;
  FPriority := Priority;
end;

constructor TNotifierItem.Create(const NotifyProc: TJclExceptNotifyProcEx; Priority: TJclExceptNotifyPriority);
begin
  inherited Create;
  FNotifyProcEx := NotifyProc;
  FPriority := Priority;
end;

constructor TNotifierItem.Create(const NotifyMethod: TJclExceptNotifyMethod; Priority: TJclExceptNotifyPriority);
begin
  inherited Create;
  FNotifyMethod := NotifyMethod;
  FPriority := Priority;
end;

procedure TNotifierItem.DoNotify(ExceptObj: TObject; ExceptAddr: Pointer;
  OSException: Boolean; StackPointer: Pointer);
begin
  if Assigned(FNotifyProc) then
    FNotifyProc(ExceptObj, ExceptAddr, OSException)
  else
  if Assigned(FNotifyProcEx) then
    FNotifyProcEx(ExceptObj, ExceptAddr, OSException, StackPointer)
  else
  if Assigned(FNotifyMethod) then
    FNotifyMethod(ExceptObj, ExceptAddr, OSException);
end;

function GetFramePointer: Pointer;
asm
        {$IFDEF CPU32}
        MOV     EAX, EBP
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        MOV     RAX, RBP
        {$ENDIF CPU64}
end;

{$STACKFRAMES ON}

{$IFDEF BORLAND}
function DoExceptFilter(ExceptRecord: PExceptionRecord): Exception;
var
  Priorities: TJclExceptNotifyPriority;
  I: Integer;
begin
  if Recursive then
    Exit;
  if Assigned(Filters) then
  begin
    Recursive := True;
    try
      with Filters.LockList do
      try
        for Priorities := High(Priorities) downto Low(Priorities) do
          for I := 0 to Count - 1 do
            with TFilterItem(Items[I]) do
              if Priority = Priorities then
                if DoFilterException(ExceptRecord, Result) then
                  Exit;
      finally
        Filters.UnlockList;
      end;
      // Nobody wanted to handle the external exception. Call the default handler.
      Result := SysUtils_ExceptObjProc(ExceptRecord);
    finally
      Recursive := False;
    end;
  end;
end;
{$ENDIF BORLAND}

procedure DoExceptNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean; StackPointer: Pointer);
var
  Priorities: TJclExceptNotifyPriority;
  I: Integer;
begin
  if Recursive then
    Exit;
  if Assigned(Notifiers) then
  begin
    Recursive := True;
    NewResultExc := nil;
    try
      with Notifiers.LockList do
      try
        if Count = 1 then
        begin
          with TNotifierItem(Items[0]) do
            DoNotify( ExceptObj, ExceptAddr, OSException, StackPointer);
        end
        else
        begin
          for Priorities := High(Priorities) downto Low(Priorities) do
            for I := 0 to Count - 1 do
              with TNotifierItem(Items[I]) do
                if Priority = Priorities then
                  DoNotify(ExceptObj, ExceptAddr, OSException, StackPointer);
        end;
      finally
        Notifiers.UnlockList;
      end;
    finally
      Recursive := False;
    end;
  end;
end;

procedure HookedRaiseException(ExceptionCode, ExceptionFlags, NumberOfArguments: DWORD;
  Arguments: PExceptionArguments); stdcall;
const
  MS_VC_EXCEPTION = $406D1388;
  cDelphiException = $0EEDFADE;
  cNonContinuable = 1;                  // Delphi exceptions
  cNonContinuableException = $C0000025; // C++Builder exceptions (sounds like a bug)
  DelphiNumberOfArguments = 7;
  CBuilderNumberOfArguments = 8;
begin
  if ((ExceptionFlags = cNonContinuable) or (ExceptionFlags = cNonContinuableException)) and
    (ExceptionCode = cDelphiException) and
    (NumberOfArguments in [DelphiNumberOfArguments, CBuilderNumberOfArguments])
    //TODO: The difference for Win64 is bigger than 100 Byte and the comment of JVCS revision 0.3 of
    //  JclDebug.pas, where HookedRaiseException has been added by Petr, isn't very informative
    {$IFDEF CPU32}
    and (TJclAddr(Arguments) = TJclAddr(@Arguments) + SizeOf(Pointer))
    {$ENDIF CPU32}
    and (ExceptionCode <> MS_VC_EXCEPTION) // ignore TThread.NameThreadForDebugging
    then
  begin
    DoExceptNotify(Arguments.ExceptObj, Arguments.ExceptAddr, False, GetFramePointer);
  end;
  Kernel32_RaiseException(ExceptionCode, ExceptionFlags, NumberOfArguments, PDWORD(Arguments));
end;

{$IFDEF BORLAND}
function HookedExceptObjProc(P: PExceptionRecord): Exception;
const
  MS_VC_EXCEPTION = $406D1388;
var
  NewResultExcCache: Exception; // TLS optimization
begin
  if P.ExceptionCode <> MS_VC_EXCEPTION then
  begin
    Result := DoExceptFilter(P);
    DoExceptNotify(Result, P^.ExceptionAddress, True, GetFramePointer);
    NewResultExcCache := NewResultExc;
    if NewResultExcCache <> nil then
      Result := NewResultExcCache;
  end
  else
    Result := SysUtils_ExceptObjProc(P);
end;
{$ENDIF BORLAND}

{$IFDEF FPC}
procedure HookedExceptProc(Obj : TObject; Addr : Pointer; FrameCount:Longint; Frame: PPointer);
var
  NewResultExcCache: Exception; // TLS optimization
begin
  DoExceptNotify(Obj, Addr, True, GetFramePointer);
  NewResultExcCache := NewResultExc;
  if NewResultExcCache <> nil then
    SysUtils_ExceptProc(NewResultExcCache, Addr, FrameCount, Frame)
  else
    SysUtils_ExceptProc(Obj, Addr, FrameCount, Frame)
end;
{$ENDIF FPC}

{$IFNDEF STACKFRAMES_ON}
{$STACKFRAMES OFF}
{$ENDIF ~STACKFRAMES_ON}

// Do not change ordering of HookedRaiseException, HookedExceptObjProc and JclBelongsHookedCode routines

function JclBelongsHookedCode(Address: Pointer): Boolean;
begin
  Result := (TJclAddr(@HookedRaiseException) < TJclAddr(@JclBelongsHookedCode)) and
    (TJclAddr(@HookedRaiseException) <= TJclAddr(Address)) and
    (TJclAddr(@JclBelongsHookedCode) > TJclAddr(Address));
end;

{$IFDEF BORLAND}
function JclAddExceptFilter(const FilterProc: TJclExceptFilterProc; Priority: TJclExceptNotifyPriority = npNormal): Boolean;
begin
  Result := Assigned(FilterProc);
  if Result then
    with Filters.LockList do
    try
      Add(TFilterItem.Create(FilterProc, Priority));
    finally
      Filters.UnlockList;
    end;
end;
{$ENDIF BORLAND}

function JclAddExceptNotifier(const NotifyProc: TJclExceptNotifyProc; Priority: TJclExceptNotifyPriority): Boolean;
begin
  Result := Assigned(NotifyProc);
  if Result then
    with Notifiers.LockList do
    try
      Add(TNotifierItem.Create(NotifyProc, Priority));
    finally
      Notifiers.UnlockList;
    end;
end;

function JclAddExceptNotifier(const NotifyProc: TJclExceptNotifyProcEx; Priority: TJclExceptNotifyPriority): Boolean;
begin
  Result := Assigned(NotifyProc);
  if Result then
    with Notifiers.LockList do
    try
      Add(TNotifierItem.Create(NotifyProc, Priority));
    finally
      Notifiers.UnlockList;
    end;
end;

function JclAddExceptNotifier(const NotifyMethod: TJclExceptNotifyMethod; Priority: TJclExceptNotifyPriority): Boolean;
begin
  Result := Assigned(NotifyMethod);
  if Result then
    with Notifiers.LockList do
    try
      Add(TNotifierItem.Create(NotifyMethod, Priority));
    finally
      Notifiers.UnlockList;
    end;
end;

{$IFDEF BORLAND}
function JclRemoveExceptFilter(const FilterProc: TJclExceptFilterProc): Boolean;
var
  O: TFilterItem;
  I: Integer;
begin
  Result := Assigned(FilterProc);
  if Result then
    with Filters.LockList do
    try
      for I := 0 to Count - 1 do
      begin
        O := TFilterItem(Items[I]);
        if @O.FExceptFilterProc = @FilterProc then
        begin
          O.Free;
          Items[I] := nil;
        end;
      end;
      Pack;
    finally
      Filters.UnlockList;
    end;
end;
{$ENDIF BORLAND}

function JclRemoveExceptNotifier(const NotifyProc: TJclExceptNotifyProc): Boolean;
var
  O: TNotifierItem;
  I: Integer;
begin
  Result := Assigned(NotifyProc);
  if Result then
    with Notifiers.LockList do
    try
      for I := 0 to Count - 1 do
      begin
        O := TNotifierItem(Items[I]);
        if @O.FNotifyProc = @NotifyProc then
        begin
          O.Free;
          Items[I] := nil;
        end;
      end;
      Pack;
    finally
      Notifiers.UnlockList;
    end;
end;

function JclRemoveExceptNotifier(const NotifyProc: TJclExceptNotifyProcEx): Boolean;
var
  O: TNotifierItem;
  I: Integer;
begin
  Result := Assigned(NotifyProc);
  if Result then
    with Notifiers.LockList do
    try
      for I := 0 to Count - 1 do
      begin
        O := TNotifierItem(Items[I]);
        if @O.FNotifyProcEx = @NotifyProc then
        begin
          O.Free;
          Items[I] := nil;
        end;
      end;
      Pack;
    finally
      Notifiers.UnlockList;
    end;
end;

function JclRemoveExceptNotifier(const NotifyMethod: TJclExceptNotifyMethod): Boolean;
var
  O: TNotifierItem;
  I: Integer;
begin
  Result := Assigned(NotifyMethod);
  if Result then
    with Notifiers.LockList do
    try
      for I := 0 to Count - 1 do
      begin
        O := TNotifierItem(Items[I]);
        if (TMethod(O.FNotifyMethod).Code = TMethod(NotifyMethod).Code) and
          (TMethod(O.FNotifyMethod).Data = TMethod(NotifyMethod).Data) then
        begin
          O.Free;
          Items[I] := nil;
        end;
      end;
      Pack;
    finally
      Notifiers.UnlockList;
    end;
end;

procedure JclReplaceExceptObj(NewExceptObj: Exception);
begin
  Assert(Recursive);
  NewResultExc := NewExceptObj;
end;

{$IFDEF BORLAND}
function GetCppRtlBase: Pointer;
const
  {$IFDEF COMPILER6} { Delphi/C++Builder 6 }
  CppRtlVersion = 60;
  {$ELSE ~COMPILER6}
  {$IFDEF RTL185} { Delphi/C++Builder 2007 were aiming for
                    binary compatibility with BDS2006, which
                    complicates things a bit }
  CppRtlVersion = 80;
  {$ELSE ~RTL185}
  { Successive RTLDLL version numbers in the remaining cases: CB2006 has cc3270mt.dll,
    CB2009 (= CB2006 + 2 releases) has cc3290mt.dll, CB2010 has cc32100mt.dll etc. }
  CppRtlVersion = 70 + Trunc(RtlVersion - 18.0) * 10;
  {$ENDIF ~RTL185}
  {$ENDIF ~COMPILER6}
begin
  Result := Pointer(GetModuleHandle(PChar(Format('cc32%dmt.dll', [CppRtlVersion]))));
  { 'Result = nil' means that the C++ RTL has been linked statically or is not available at all;
    in this case TJclPeMapImgHooks.ReplaceImport() is a no-op. The base module is also being
    hooked separately, so we're covered. }
end;

function HasCppRtl: Boolean;
begin
  Result := GetCppRtlBase <> TJclPeMapImgHooks.SystemBase;
end;
{$ENDIF BORLAND}

function JclHookExceptions: Boolean;
var
  RaiseExceptionAddressCache: Pointer;
begin
  RaiseExceptionAddressCache := RaiseExceptionAddress;
  { Detect C++Builder applications and C++ packages loaded into Delphi applications.
    Hook the C++ RTL regardless of ExceptionsHooked so that users can call JclHookException() after
    loading a C++ package which might pull in the C++ RTL DLL. }
  {$IFDEF BORLAND}
  if HasCppRtl then
    TJclPeMapImgHooks.ReplaceImport(GetCppRtlBase, kernel32, RaiseExceptionAddressCache, @HookedRaiseException);
  {$ENDIF BORLAND}
  if not ExceptionsHooked then
  begin
    Recursive := False;
    with TJclPeMapImgHooks do
      Result := ReplaceImport(SystemBase, kernel32, RaiseExceptionAddressCache, @HookedRaiseException);
    if Result then
    begin
      @Kernel32_RaiseException := RaiseExceptionAddressCache;
      {$IFDEF BORLAND}
      SysUtils_ExceptObjProc := System.ExceptObjProc;
      System.ExceptObjProc := @HookedExceptObjProc;
      {$ENDIF BORLAND}
      {$IFDEF FPC}
      SysUtils_ExceptProc := System.ExceptProc;
      System.ExceptProc := @HookedExceptProc;
      {$ENDIF FPC}
    end;
    ExceptionsHooked := Result;
  end
  else
    Result := True;
end;

function JclUnhookExceptions: Boolean;
begin
  {$IFDEF BORLAND}
  if HasCppRtl then
    TJclPeMapImgHooks.ReplaceImport (GetCppRtlBase, kernel32, @HookedRaiseException, @Kernel32_RaiseException);
  {$ENDIF BORLAND}
  if ExceptionsHooked then
  begin
    with TJclPeMapImgHooks do
      ReplaceImport(SystemBase, kernel32, @HookedRaiseException, @Kernel32_RaiseException);
    {$IFDEF BORLAND}
    System.ExceptObjProc := @SysUtils_ExceptObjProc;
    @SysUtils_ExceptObjProc := nil;
    {$ENDIF BORLAND}
    {$IFDEF FPC}
    System.ExceptProc := @SysUtils_ExceptProc;
    @SysUtils_ExceptProc := nil;
    {$ENDIF FPC}
    @Kernel32_RaiseException := nil;
    Result := True;
    ExceptionsHooked := False;
  end
  else
    Result := True;
end;

function JclExceptionsHooked: Boolean;
begin
  Result := ExceptionsHooked;
end;

function JclHookExceptionsInModule(Module: HMODULE): Boolean;
begin
  Result := ExceptionsHooked and
    TJclPeMapImgHooks.ReplaceImport(Pointer(Module), kernel32, RaiseExceptionAddress, @HookedRaiseException);
end;

function JclUnhookExceptionsInModule(Module: HMODULE): Boolean;
begin
  Result := ExceptionsHooked and
    TJclPeMapImgHooks.ReplaceImport(Pointer(Module), kernel32, @HookedRaiseException, @Kernel32_RaiseException);
end;

{$IFDEF HOOK_DLL_EXCEPTIONS}
// Exceptions hooking in libraries

procedure JclHookExceptDebugHookProc(Module: HMODULE; Hook: Boolean); stdcall;
begin
  if Hook then
    HookExceptModuleList.HookModule(Module)
  else
    HookExceptModuleList.UnhookModule(Module);
end;

function CallExportedHookExceptProc(Module: HMODULE; Hook: Boolean): Boolean;
var
  HookExceptProcPtr: PPointer;
  HookExceptProc: TJclHookExceptDebugHook;
begin
  HookExceptProcPtr := TJclHookExceptModuleList.JclHookExceptDebugHookAddr;
  Result := Assigned(HookExceptProcPtr);
  if Result then
  begin
    @HookExceptProc := HookExceptProcPtr^;
    if Assigned(HookExceptProc) then
      HookExceptProc(Module, True);
  end;
end;
{$ENDIF HOOK_DLL_EXCEPTIONS}

function JclInitializeLibrariesHookExcept: Boolean;
begin
  {$IFDEF HOOK_DLL_EXCEPTIONS}
  if IsLibrary then
    Result := CallExportedHookExceptProc(SystemTObjectInstance, True)
  else
  begin
    if not Assigned(HookExceptModuleList) then
      HookExceptModuleList := TJclHookExceptModuleList.Create;
    Result := True;
  end;
  {$ELSE HOOK_DLL_EXCEPTIONS}
  Result := True;
  {$ENDIF HOOK_DLL_EXCEPTIONS}
end;

function JclHookedExceptModulesList(out ModulesList: TJclModuleArray): Boolean;
begin
  {$IFDEF HOOK_DLL_EXCEPTIONS}
  Result := Assigned(HookExceptModuleList);
  if Result then
    HookExceptModuleList.List(ModulesList);
  {$ELSE HOOK_DLL_EXCEPTIONS}
  Result := False;
  SetLength(ModulesList, 0);
  {$ENDIF HOOK_DLL_EXCEPTIONS}
end;

{$IFDEF HOOK_DLL_EXCEPTIONS}
procedure FinalizeLibrariesHookExcept;
begin
  FreeAndNil(HookExceptModuleList);
  if IsLibrary then
    CallExportedHookExceptProc(SystemTObjectInstance, False);
end;

//=== { TJclHookExceptModuleList } ===========================================

constructor TJclHookExceptModuleList.Create;
begin
  inherited Create;
  FModules := TThreadList.Create;
  HookStaticModules;
  JclHookExceptDebugHook := @JclHookExceptDebugHookProc;
end;

destructor TJclHookExceptModuleList.Destroy;
begin
  JclHookExceptDebugHook := nil;
  FreeAndNil(FModules);
  inherited Destroy;
end;

procedure TJclHookExceptModuleList.HookModule(Module: HMODULE);
begin
  with FModules.LockList do
  try
    if IndexOf(Pointer(Module)) = -1 then
    begin
      Add(Pointer(Module));
      JclHookExceptionsInModule(Module);
    end;
  finally
    FModules.UnlockList;
  end;
end;

procedure TJclHookExceptModuleList.HookStaticModules;
var
  ModulesList: TStringList;
  I: Integer;
  Module: HMODULE;
begin
  ModulesList := nil;
  with FModules.LockList do
  try
    ModulesList := TStringList.Create;
    if LoadedModulesList(ModulesList, GetCurrentProcessId, True) then
      for I := 0 to ModulesList.Count - 1 do
      begin
        Module := HMODULE(ModulesList.Objects[I]);
        if GetProcAddress(Module, JclHookExceptDebugHookName) <> nil then
          HookModule(Module);
      end;
  finally
    FModules.UnlockList;
    ModulesList.Free;
  end;
end;

class function TJclHookExceptModuleList.JclHookExceptDebugHookAddr: Pointer;
var
  HostModule: HMODULE;
begin
  HostModule := GetModuleHandle(nil);
  Result := GetProcAddress(HostModule, JclHookExceptDebugHookName);
end;

procedure TJclHookExceptModuleList.List(out ModulesList: TJclModuleArray);
var
  I: Integer;
begin
  with FModules.LockList do
  try
    SetLength(ModulesList, Count);
    for I := 0 to Count - 1 do
      ModulesList[I] := HMODULE(Items[I]);
  finally
    FModules.UnlockList;
  end;
end;

procedure TJclHookExceptModuleList.UnhookModule(Module: HMODULE);
begin
  with FModules.LockList do
  try
    Remove(Pointer(Module));
  finally
    FModules.UnlockList;
  end;
end;
{$ENDIF HOOK_DLL_EXCEPTIONS}

initialization
  Notifiers := TThreadList.Create;
  {$IFDEF BORLAND}
  Filters := TThreadList.Create;
  {$ENDIF BORLAND}
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  {$IFDEF HOOK_DLL_EXCEPTIONS}
  FinalizeLibrariesHookExcept;
  {$ENDIF HOOK_DLL_EXCEPTIONS}
  FreeThreadObjList(Notifiers);
  {$IFDEF BORLAND}
  FreeThreadObjList(Filters);
  {$ENDIF BORLAND}

end.
