unit TB2Hook;

// Workaround for bug in C++Builder XE2 that makes threadvar's in this unit
// overwrite each other when optimization is enabled
{$O-}

{
  Toolbar2000
  Copyright (C) 1998-2005 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    https://jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    https://jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2Hook.pas,v 1.15 2005/06/26 18:21:33 jr Exp $
}

interface

uses
  Windows;

type
  THookProcCode = (hpSendActivate, hpSendActivateApp, hpSendWindowPosChanged,
    hpPreDestroy, hpGetMessage);
  THookProcCodes = set of THookProcCode;

  THookProc = procedure(Code: THookProcCode; Wnd: HWND; WParam: WPARAM; LParam: LPARAM);

procedure InstallHookProc(AUser: TObject; AProc: THookProc; ACodes: THookProcCodes);
procedure UninstallHookProc(AUser: TObject; AProc: THookProc);

implementation

uses
  SysUtils, Classes, Messages;

type
  THookType = (htCallWndProc, htCBT, htGetMessage);
  THookTypes = set of THookType;

  PHookUserData = ^THookUserData;
  THookUserData = record
    Prev: PHookUserData;
    User: TObject;
    InstalledHookTypes: THookTypes;
  end;

  PHookProcData = ^THookProcData;
  THookProcData = record
    Proc: THookProc;
    Codes: THookProcCodes;
    LastUserData: PHookUserData;
  end;

threadvar
  HookHandles: array[THookType] of HHOOK;
  HookProcList: TList;
  HookCounts: array[THookType] of Longint;


function CallWndProcHook(Code: Integer; WParam: WPARAM; LParam: LPARAM): LRESULT;
stdcall;
type
  THookProcCodeMsgs = hpSendActivate..hpSendWindowPosChanged;
const
  MsgMap: array[THookProcCodeMsgs] of UINT =
    (WM_ACTIVATE, WM_ACTIVATEAPP, WM_WINDOWPOSCHANGED);
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
    (htCallWndProc, htCallWndProc, htCallWndProc, htCBT, htGetMessage);
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

procedure InstallHooks(ATypes: THookTypes; var InstalledTypes: THookTypes);
var
  T: THookType;
begin
  { Don't increment reference counts for hook types that were already
    installed previously }
  ATypes := ATypes - InstalledTypes;

  { Increment reference counts first. This should never raise an exception. }
  for T := Low(T) to High(T) do
    if T in ATypes then begin
      Inc(HookCounts[T]);
      Include(InstalledTypes, T);
    end;

  { Then install the hooks }
  for T := Low(T) to High(T) do
    if T in InstalledTypes then begin
      if HookHandles[T] = 0 then begin
        { On Windows NT platforms, SetWindowsHookExW is used to work around an
          apparent bug in Windows NT/2000/XP: if an 'ANSI' WH_GETMESSAGE hook
          is called *before* a 'wide' WH_GETMESSAGE hook, then WM_*CHAR
          messages passed to the 'wide' hook use ANSI character codes.
          This is needed for compatibility with the combination of Tnt Unicode
          Controls and Keyman. See "Widechar's and tb2k" thread on the
          newsgroup from 2003-09-23 for more information. }
        HookHandles[T] := SetWindowsHookExW(HookIDs[T], HookProcs[T],
          0, GetCurrentThreadId)
      end;
    end;
end;

procedure UninstallHooks(const ATypes: THookTypes; const Force: Boolean);
var
  T: THookType;
begin
  { Decrement reference counts first. This should never raise an exception. }
  if not Force then
    for T := Low(T) to High(T) do
      if T in ATypes then
        Dec(HookCounts[T]);

  { Then uninstall the hooks }
  for T := Low(T) to High(T) do
    if T in ATypes then begin
      if (Force or (HookCounts[T] = 0)) and (HookHandles[T] <> 0) then begin
        UnhookWindowsHookEx(HookHandles[T]);
        HookHandles[T] := 0;
      end;
    end;
end;

procedure InstallHookProc(AUser: TObject; AProc: THookProc; ACodes: THookProcCodes);
var
  Found: Boolean;
  I: Integer;
  UserData: PHookUserData;
  ProcData: PHookProcData;
label 1;
begin
  if HookProcList = nil then
  begin
    HookProcList := TList.Create;
  end;
  Found := False;
  UserData := nil;  { avoid warning }
  for I := 0 to HookProcList.Count-1 do begin
    ProcData := PHookProcData(HookProcList[I]);
    if @ProcData.Proc = @AProc then begin
      UserData := ProcData.LastUserData;
      while Assigned(UserData) do begin
        if UserData.User = AUser then begin
          { InstallHookProc was already called for AUser/AProc. Go ahead and
            call InstallHooks again just in case the hooks weren't successfully
            installed last time. }
          goto 1;
        end;
        UserData := UserData.Prev;
      end;
      New(UserData);
      UserData.Prev := ProcData.LastUserData;
      UserData.User := AUser;
      UserData.InstalledHookTypes := [];
      ProcData.LastUserData := UserData;
      Found := True;
      Break;
    end;
  end;
  if not Found then begin
    New(UserData);
    try
      UserData.Prev := nil;
      UserData.User := AUser;
      UserData.InstalledHookTypes := [];
      HookProcList.Expand;
      New(ProcData);
    except
      Dispose(UserData);
      raise;
    end;
    ProcData.Proc := AProc;
    ProcData.Codes := ACodes;
    ProcData.LastUserData := UserData;
    HookProcList.Add(ProcData);
  end;
1:InstallHooks(HookCodesToTypes(ACodes), UserData.InstalledHookTypes);
end;

procedure UninstallHookProc(AUser: TObject; AProc: THookProc);
var
  I: Integer;
  ProcData: PHookProcData;
  NextUserData, UserData: PHookUserData;
  T: THookTypes;
begin
  if HookProcList = nil then Exit;
  for I := 0 to HookProcList.Count-1 do begin
    ProcData := PHookProcData(HookProcList[I]);
    if @ProcData.Proc = @AProc then begin
      { Locate the UserData record }
      NextUserData := nil;
      UserData := ProcData.LastUserData;
      while Assigned(UserData) and (UserData.User <> AUser) do begin
        NextUserData := UserData;
        UserData := UserData.Prev;
      end;
      if UserData = nil then
        Exit;

      { Remove record from linked list }
      if NextUserData = nil then begin
        { It's the last item in the list }
        if UserData.Prev = nil then begin
          { It's the only item in the list, so destroy the ProcData record }
          HookProcList.Delete(I);
          Dispose(ProcData);
        end
        else
          ProcData.LastUserData := UserData.Prev;
      end
      else
        NextUserData.Prev := UserData.Prev;

      T := UserData.InstalledHookTypes;
      Dispose(UserData);
      UninstallHooks(T, False);
      Break;
    end;
  end;
  if HookProcList.Count = 0 then
    FreeAndNil(HookProcList);
end;


initialization
finalization
  UninstallHooks([Low(THookType)..High(THookType)], True);
end.
