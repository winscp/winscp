unit TB2Hook;

{
  Toolbar2000
  Copyright (C) 1998-2004 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2Hook.pas,v 1.10 2004/02/26 07:05:57 jr Exp $
}

interface

uses
  Windows;

type
  THookProcCode = (hpSendActivateApp, hpSendWindowPosChanged, hpPreDestroy,
    hpGetMessage);
  THookProcCodes = set of THookProcCode;

  THookProc = procedure(Code: THookProcCode; Wnd: HWND; WParam: WPARAM; LParam: LPARAM);

procedure InstallHookProc(AProc: THookProc; ACodes: THookProcCodes;
  OnlyIncrementCount: Boolean);
procedure UninstallHookProc(AProc: THookProc);

implementation

uses
  SysUtils, Classes, Messages;

type
  PHookProcData = ^THookProcData;
  THookProcData = record
    Proc: THookProc;
    RefCount: Longint;
    Codes: THookProcCodes;
  end;
  THookType = (htCallWndProc, htCBT, htGetMessage);
  THookTypes = set of THookType;

var
  HookHandles: array[THookType] of HHOOK;
  HookProcList: TList = nil;
  HookCounts: array[THookType] of Longint;


function CallWndProcHook(Code: Integer; WParam: WPARAM; LParam: LPARAM): LRESULT;
stdcall;
type
  THookProcCodeMsgs = hpSendActivateApp..hpSendWindowPosChanged;
const
  MsgMap: array[THookProcCodeMsgs] of UINT =
    (WM_ACTIVATEAPP, WM_WINDOWPOSCHANGED);
var
  J: THookProcCodeMsgs;
  I: Integer;
begin
  if Assigned(HookProcList) and (Code = HC_ACTION) then
    with PCWPStruct(LParam)^ do begin
      for J := Low(J) to High(J) do
        if Message = MsgMap[J] then begin
          for I := 0 to HookProcList.Count-1 do
            try
              with PHookProcData(HookProcList.List[I])^ do
                if J in Codes then
                  Proc(J, hwnd, WParam, LParam);
            except
            end;
          Break;
        end;
    end;
  Result := CallNextHookEx(HookHandles[htCallWndProc], Code, WParam, LParam);
end;

function CBTHook(Code: Integer; WParam: WPARAM; LParam: LPARAM): LRESULT;
stdcall;
var
  I: Integer;
begin
  if Assigned(HookProcList) and (Code = HCBT_DESTROYWND) then
    for I := 0 to HookProcList.Count-1 do
      try
        with PHookProcData(HookProcList.List[I])^ do
          if hpPreDestroy in Codes then
            Proc(hpPreDestroy, HWND(WParam), 0, 0);
      except
      end;
  Result := CallNextHookEx(HookHandles[htCBT], Code, WParam, LParam);
end;

function GetMessageHook(Code: Integer; WParam: WPARAM; LParam: LPARAM): LRESULT;
stdcall;
var
  I: Integer;
begin
  if Assigned(HookProcList) and (Code = HC_ACTION) then
    for I := 0 to HookProcList.Count-1 do
      try
        with PHookProcData(HookProcList.List[I])^ do
          if hpGetMessage in Codes then
            Proc(hpGetMessage, 0, WParam, LParam);
      except
      end;
  Result := CallNextHookEx(HookHandles[htGetMessage], Code, WParam, LParam);
end;

function HookCodesToTypes(Codes: THookProcCodes): THookTypes;
const
  HookCodeToType: array[THookProcCode] of THookType =
    (htCallWndProc, htCallWndProc, htCBT, htGetMessage);
var
  J: THookProcCode;
begin
  Result := [];
  for J := Low(J) to High(J) do
    if J in Codes then
      Include(Result, HookCodeToType[J]);
end;

const
  HookProcs: array[THookType] of TFNHookProc =
    (CallWndProcHook, CBTHook, GetMessageHook);
  HookIDs: array[THookType] of Integer =
    (WH_CALLWNDPROC, WH_CBT, WH_GETMESSAGE);

procedure InstallHooks(ATypes: THookTypes);
var
  T: THookType;
begin
  for T := Low(T) to High(T) do
    if T in ATypes then begin
      Inc(HookCounts[T]);
      if HookHandles[T] = 0 then begin
        { On Windows NT platforms, SetWindowsHookExW is used to work around an
          apparent bug in Windows NT/2000/XP: if an 'ANSI' WH_GETMESSAGE hook
          is called *before* a 'wide' WH_GETMESSAGE hook, then WM_*CHAR
          messages passed to the 'wide' hook use ANSI character codes.
          This is needed for compatibility with the combination of Tnt Unicode
          Controls and Keyman. See "Widechar's and tb2k" thread on the
          newsgroup from 2003-09-23 for more information. }
        if Win32Platform = VER_PLATFORM_WIN32_NT then
          HookHandles[T] := SetWindowsHookExW(HookIDs[T], HookProcs[T],
            0, GetCurrentThreadId)
        else
          HookHandles[T] := SetWindowsHookEx(HookIDs[T], HookProcs[T],
            0, GetCurrentThreadId);
      end;
    end;
end;

procedure UninstallHooks(const ATypes: THookTypes; const Force: Boolean);
var
  T: THookType;
begin
  for T := Low(T) to High(T) do
    if T in ATypes then begin
      if HookCounts[T] > 0 then
        Dec(HookCounts[T]);
      if (Force or (HookCounts[T] = 0)) and (HookHandles[T] <> 0) then begin
        UnhookWindowsHookEx(HookHandles[T]);
        HookHandles[T] := 0;
      end;
    end;
end;

procedure InstallHookProc(AProc: THookProc; ACodes: THookProcCodes;
  OnlyIncrementCount: Boolean);
var
  Found: Boolean;
  I: Integer;
  Data: PHookProcData;
begin
  if HookProcList = nil then
    HookProcList := TList.Create;
  Found := False;
  for I := 0 to HookProcList.Count-1 do
    with PHookProcData(HookProcList[I])^ do
      if @Proc = @AProc then begin
        Inc(RefCount);
        Found := True;
        Break;
      end;
  if not Found then begin
    New(Data);
    with Data^ do begin
      Proc := AProc;
      RefCount := 1;
      Codes := ACodes;
    end;
    HookProcList.Add(Data);
  end;
  if not OnlyIncrementCount then
    InstallHooks(HookCodesToTypes(ACodes));
end;

procedure UninstallHookProc(AProc: THookProc);
var
  I: Integer;
  Data: PHookProcData;
  T: THookTypes;
begin
  if HookProcList = nil then Exit;
  for I := 0 to HookProcList.Count-1 do begin
    Data := PHookProcData(HookProcList[I]);
    if @Data.Proc = @AProc then begin
      T := HookCodesToTypes(Data.Codes);
      Dec(Data.RefCount);
      if Data.RefCount = 0 then begin
        HookProcList.Delete(I);
        Dispose(Data);
      end;
      UninstallHooks(T, False);
      Break;
    end;
  end;
  if HookProcList.Count = 0 then begin
    HookProcList.Free;
    HookProcList := nil;
  end;
end;


initialization
finalization
  UninstallHooks([Low(THookType)..High(THookType)], True);
  HookProcList.Free;
  { Following line needed because, under certain circumstances, HookProcList
    may be referenced after the 'finalization' section is processed. (This
    can happen if a 'Halt' call is placed in the main form's OnCreate
    handler, for example.) }
  HookProcList := nil;
end.
