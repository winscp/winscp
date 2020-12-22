unit TB2Common;

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

  $jrsoftware: tb2k/Source/TB2Common.pas,v 1.31 2005/06/29 20:10:10 jr Exp $
}

interface

{$I TB2Ver.inc}

uses
  Windows, Classes, SysUtils, Messages, Controls, Forms;

type
  TListSortExCompare = function(const Item1, Item2, ExtraData: Pointer): Integer;
  THandleWMPrintNCPaintProc = procedure(Control: TControl; Wnd: HWND; DC: HDC; AppData: Longint);

function AddToFrontOfList(var List: TList; Item: Pointer): Boolean;
function AddToList(var List: TList; Item: Pointer): Boolean;
function ApplicationIsActive: Boolean;
function AreFlatMenusEnabled: Boolean;
function AreKeyboardCuesEnabled: Boolean;
function CallTrackMouseEvent(const Wnd: HWND; const Flags: DWORD): Boolean;
function CreateHalftoneBrush: HBRUSH;
function CreateNullRegion: HRGN;
function CreateRotatedFont(DC: HDC): HFONT;
function DivRoundUp(const Dividend, Divisor: Integer): Integer;
procedure DrawHalftoneInvertRect(const DC: HDC; const NewRect, OldRect: PRect;
  const NewSize, OldSize: TSize);
procedure DrawRotatedText(const DC: HDC; AText: String; const ARect: TRect;
  const AFormat: Cardinal);
procedure DrawInvertRect(const DC: HDC; const NewRect, OldRect: PRect;
  const NewSize, OldSize: TSize; const Brush: HBRUSH; BrushLast: HBRUSH);
function EscapeAmpersands(const S: String): String;
function FindAccelChar(const S: String): Char;
function GetInputLocaleCodePage: UINT;
function GetMenuShowDelay: Integer;
function GetRectOfMonitorContainingPoint(const P: TPoint; const WorkArea: Boolean): TRect;
function GetRectOfMonitorContainingRect(const R: TRect; const WorkArea: Boolean): TRect;
function GetRectOfMonitorContainingWindow(const W: HWND; const WorkArea: Boolean): TRect;
function GetRectOfPrimaryMonitor(const WorkArea: Boolean): TRect;
function GetTextHeight(const DC: HDC): Integer;
function GetTextWidth(const DC: HDC; S: String; const Prefix: Boolean): Integer;
procedure HandleWMPrint(Control: TControl; const Wnd: HWND; var Message: TMessage;
  const NCPaintFunc: THandleWMPrintNCPaintProc; const AppData: Longint);
procedure HandleWMPrintClient(const Control: TWinControl;
  var Message: TMessage);
procedure ListSortEx(const List: TList; const Compare: TListSortExCompare;
  const ExtraData: Pointer);
function Max(A, B: Integer): Integer;
function Min(A, B: Integer): Integer;
function MethodsEqual(const M1, M2: TMethod): Boolean;
function NeedToPlaySound(const Alias: String): Boolean;
procedure ProcessPaintMessages;
procedure RemoveMessages(const AMin, AMax: Integer);
procedure RemoveFromList(var List: TList; Item: Pointer);
procedure SelectNCUpdateRgn(Wnd: HWND; DC: HDC; Rgn: HRGN);
function StripAccelChars(const S: String; IncludingStandaloneKey: Boolean = False {MP}): String;
function StripTrailingPunctuation(const S: String): String;
function UsingMultipleMonitors: Boolean;

const
  PopupMenuWindowNCSize = 3;
  DT_HIDEPREFIX = $00100000;{$EXTERNALSYM DT_HIDEPREFIX}

var
  TrackMouseEventFunc: function(var EventTrack: TTrackMouseEvent): BOOL; stdcall;

implementation

uses
  TB2Version, Types, System.Character {MP};

function ApplicationIsActive: Boolean;
{ Returns True if the application is in the foreground }
begin
  Result := GetActiveWindow <> 0;
end;

procedure ListSortEx(const List: TList; const Compare: TListSortExCompare;
  const ExtraData: Pointer);
{ Similar to TList.Sort, but lets you pass a user-defined ExtraData pointer }
  procedure QuickSortEx(L: Integer; const R: Integer);
  var
    I, J: Integer;
    P: Pointer;
  begin
    repeat
      I := L;
      J := R;
      P := List[(L + R) shr 1];
      repeat
        while Compare(List[I], P, ExtraData) < 0 do Inc(I);
        while Compare(List[J], P, ExtraData) > 0 do Dec(J);
        if I <= J then
        begin
          List.Exchange(I, J);
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSortEx(L, J);
      L := I;
    until I >= R;
  end;
begin
  if List.Count > 1 then
    QuickSortEx(0, List.Count-1);
end;

type
  PPrintEnumProcData = ^TPrintEnumProcData;
  TPrintEnumProcData = record
    PrintChildren: Boolean;
    ParentWnd: HWND;
    DC: HDC;
    PrintFlags: LPARAM;
  end;

function PrintEnumProc(Wnd: HWND; LParam: LPARAM): BOOL; stdcall;
var
  R: TRect;
  SaveIndex: Integer;
begin
  Result := True;  { continue enumerating }
  with PPrintEnumProcData(LParam)^ do begin
    { Skip window if it isn't a child/owned window of ParentWnd or isn't visible }
    if (HWND(GetWindowLong(Wnd, GWL_HWNDPARENT)) <> ParentWnd) or
       (GetWindowLong(Wnd, GWL_STYLE) and WS_VISIBLE = 0) then
         { ^ don't use IsWindowVisible since it returns False if the window's
           parent window is not visible }
      Exit;
    GetWindowRect(Wnd, R);
    MapWindowPoints(0, ParentWnd, R, 2);
    SaveIndex := SaveDC(DC);
    { Like Windows, offset the window origin to the top-left coordinates of
      the child/owned window }
    MoveWindowOrg(DC, R.Left, R.Top);
    { Like Windows, intersect the clipping region with the entire rectangle of
      the child/owned window }
    OffsetRect(R, -R.Left, -R.Top);
    IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
    { Send a WM_PRINT message to the child/owned window }
    SendMessage(Wnd, WM_PRINT, WPARAM(DC), PrintFlags);
    { Restore the DC's state, in case the WM_PRINT handler didn't put things
      back the way it found them }
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure HandleWMPrint(Control: TControl; const Wnd: HWND; var Message: TMessage;
  const NCPaintFunc: THandleWMPrintNCPaintProc; const AppData: Longint);
{ note: AppData is an application-defined value which is passed to NCPaintFunc }
var
  DC: HDC;
  SaveIndex, SaveIndex2: Integer;
  R: TRect;
  P: TPoint;
  Data: TPrintEnumProcData;
begin
  if (Message.LParam and PRF_CHECKVISIBLE = 0) or IsWindowVisible(Wnd) then begin
    DC := HDC(Message.WParam);
    SaveIndex2 := SaveDC(DC);
    try
      if Message.LParam and PRF_NONCLIENT <> 0 then begin
        SaveIndex := SaveDC(DC);
        if Assigned(NCPaintFunc) then
          NCPaintFunc(Control, Wnd, DC, AppData);
        RestoreDC(DC, SaveIndex);
      end;
      { Calculate the difference between the top-left corner of the window
        and the top-left corner of its client area }
      GetWindowRect(Wnd, R);
      P.X := 0;  P.Y := 0;
      ClientToScreen(Wnd, P);
      Dec(P.X, R.Left);  Dec(P.Y, R.Top);
      if Message.LParam and PRF_CLIENT <> 0 then begin
        { Like Windows, the flags PRF_ERASEBKGND, PRF_CHILDREN, and PRF_OWNED
          are ignored if PRF_CLIENT isn't also specified }
        if Message.LParam and PRF_ERASEBKGND <> 0 then begin
          { Send WM_ERASEBKGND }
          SaveIndex := SaveDC(DC);
          if Message.LParam and PRF_NONCLIENT <> 0 then
            MoveWindowOrg(DC, P.X, P.Y);
          SendMessage(Wnd, WM_ERASEBKGND, Message.WParam, 0);
          RestoreDC(DC, SaveIndex);
        end;
        { Send WM_PRINTCLIENT }
        SaveIndex := SaveDC(DC);
        if Message.LParam and PRF_NONCLIENT <> 0 then
          MoveWindowOrg(DC, P.X, P.Y);
        SendMessage(Wnd, WM_PRINTCLIENT, Message.WParam, 0);
        RestoreDC(DC, SaveIndex);
        { Like Windows, always offset child/owned windows by the size of the
          client area even if PRF_NONCLIENT isn't specified (a bug?) }
        MoveWindowOrg(DC, P.X, P.Y);
        Data.ParentWnd := Wnd;
        Data.DC := DC;
        { Send WM_PRINT to child/owned windows }
        if Message.LParam and PRF_CHILDREN <> 0 then begin
          Data.PrintChildren := True;
          Data.PrintFlags := PRF_NONCLIENT or PRF_CLIENT or PRF_ERASEBKGND or
            PRF_CHILDREN;  { same flags as Windows passes to children }
          EnumChildWindows(Wnd, @PrintEnumProc, LPARAM(@Data));
        end;
        if Message.LParam and PRF_OWNED <> 0 then begin
          Data.PrintChildren := False;
          Data.PrintFlags := Message.LParam;
          EnumWindows(@PrintEnumProc, LPARAM(@Data));
        end;
      end;
    finally
      RestoreDC(DC, SaveIndex2);
    end;
  end;
  { Windows' WM_PRINT returns 1. I'm not sure why. }
  Message.Result := 1;
end;

type
  TWinControlAccess = class(TWinControl);

procedure HandleWMPrintClient(const Control: TWinControl; var Message: TMessage);
var
  Msg: TWMPaint;
  SaveIndex: Integer;
begin
  Msg.Msg := WM_PAINT;
  Msg.DC := HDC(Message.WParam);
  Msg.Unused := 0;
  Msg.Result := 0;
  SaveIndex := SaveDC(HDC(Message.WParam));
  try
    TWinControlAccess(Control).PaintHandler(Msg);
  finally
    RestoreDC(HDC(Message.WParam), SaveIndex);
  end;
end;

function DivRoundUp(const Dividend, Divisor: Integer): Integer;
{ Similar to the 'div' operator, but if there is a remainder it always rounds
  the result up one (or down if the result is negative). }
asm
  mov  ecx, edx
  cdq
  idiv ecx
  test edx, edx
  jz   @@1
  test eax, eax
  jns  @@2
  dec  eax
  jmp  @@1
  @@2:
  inc  eax
  @@1:
end;

function GetTextHeight(const DC: HDC): Integer;
var
  TextMetric: TTextMetric;
begin
  GetTextMetrics(DC, TextMetric);
  Result := TextMetric.tmHeight;
end;

function StripAccelChars(const S: String; IncludingStandaloneKey: Boolean {MP}): String;
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do begin
    if not CharInSet(Result[I], LeadBytes) then begin
      if Result[I] = '&' then
      begin
        {MP}
        // Trim trailing artificial accelerators typical for asian translation
        // e.g. "foobar (&L)"
        if IncludingStandaloneKey and
           (I = Length(Result) - 2) and
           (Result[I - 1] = '(') and
           Result[I + 1].IsLetter() and
           (Result[I + 2] = ')') then
        begin
          System.Delete(Result, I - 1, 4);
        end
          else
        begin
          System.Delete(Result, I, 1);
        end;
      end;
      Inc(I);
    end
    else
      Inc(I, 2);
  end;
end;

function EscapeAmpersands(const S: String): String;
{ Replaces any '&' characters with '&&' }
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do begin
    if not CharInSet(Result[I], LeadBytes) then begin
      if Result[I] = '&' then begin
        Inc(I);
        Insert('&', Result, I);
      end;
      Inc(I);
    end
    else
      Inc(I, 2);
  end;
end;

function StripTrailingPunctuation(const S: String): String;
{ Removes any colon (':') or ellipsis ('...') from the end of S and returns
  the resulting string }
var
  L: Integer;
begin
  Result := S;
  L := Length(Result);
  if (L > 1) and (Result[L] = ':') and (ByteType(Result, L) = mbSingleByte) then
    SetLength(Result, L-1)
  else if (L > 3) and (Result[L-2] = '.') and (Result[L-1] = '.') and
     (Result[L] = '.') and (ByteType(Result, L-2) = mbSingleByte) then
    SetLength(Result, L-3);
end;

function GetTextWidth(const DC: HDC; S: String; const Prefix: Boolean): Integer;
{ Returns the width of the specified string using the font currently selected
  into DC. If Prefix is True, it first removes "&" characters as necessary. }
var
  Size: TSize;
begin
  { This procedure is 10x faster than using DrawText with the DT_CALCRECT flag }
  if Prefix then
    S := StripAccelChars(S);
  GetTextExtentPoint32(DC, PChar(S), Length(S), Size);
  Result := Size.cx;
end;

procedure ProcessPaintMessages;
{ Dispatches all pending WM_PAINT messages. In effect, this is like an
  'UpdateWindow' on all visible windows }
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, 0, WM_PAINT, WM_PAINT, PM_NOREMOVE) do begin
    case Integer(GetMessage(Msg, 0, WM_PAINT, WM_PAINT)) of
      -1: Break; { if GetMessage failed }
      0: begin
           { Repost WM_QUIT messages }
           PostQuitMessage(Msg.WParam);
           Break;
         end;
    end;
    DispatchMessage(Msg);
  end;
end;

procedure RemoveMessages(const AMin, AMax: Integer);
{ Removes any messages with the specified ID from the queue }
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, 0, AMin, AMax, PM_REMOVE) do begin
    if Msg.message = WM_QUIT then begin
      { Repost WM_QUIT messages }
      PostQuitMessage(Msg.WParam);
      Break;
    end;
  end;
end;

procedure SelectNCUpdateRgn(Wnd: HWND; DC: HDC; Rgn: HRGN);
var
  R: TRect;
  NewClipRgn: HRGN;
begin
  if (Rgn <> 0) and (Rgn <> 1) then begin
    GetWindowRect(Wnd, R);
    if SelectClipRgn(DC, Rgn) = ERROR then begin
      NewClipRgn := CreateRectRgnIndirect(R);
      SelectClipRgn(DC, NewClipRgn);
      DeleteObject(NewClipRgn);
    end;
    OffsetClipRgn(DC, -R.Left, -R.Top);
  end;
end;

function AddToList(var List: TList; Item: Pointer): Boolean;
{ Returns True if Item didn't already exist in the list }
begin
  if List = nil then
    List := TList.Create;
  Result := List.IndexOf(Item) = -1;
  if Result then
    List.Add(Item);
end;

function AddToFrontOfList(var List: TList; Item: Pointer): Boolean;
{ Returns True if Item didn't already exist in the list }
begin
  if List = nil then
    List := TList.Create;
  Result := List.IndexOf(Item) = -1;
  if Result then
    List.Insert(0, Item);
end;

procedure RemoveFromList(var List: TList; Item: Pointer);
begin
  if Assigned(List) then begin
    List.Remove(Item);
    if List.Count = 0 then begin
      List.Free;
      List := nil;
    end;
  end;
end;

var
  RegMenuShowDelay: Integer;
  RegMenuShowDelayInited: BOOL = False;
function GetMenuShowDelay: Integer;
const
  DefaultMenuShowDelay = 400;
  function ReadMenuShowDelayFromRegistry: Integer;
  var
    K: HKEY;
    Typ, DataSize: DWORD;
    Data: array[0..31] of Char;
    Res: Longint;
    E: Integer;
  begin
    Result := DefaultMenuShowDelay;
    if RegOpenKeyEx(HKEY_CURRENT_USER, 'Control Panel\Desktop', 0,
       KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      DataSize := SizeOf(Data);
      Res := RegQueryValueEx(K, 'MenuShowDelay', nil, @Typ, @Data, @DataSize);
      RegCloseKey(K);
      if Res <> ERROR_FILE_NOT_FOUND then begin
        if (Res <> ERROR_SUCCESS) or (Typ <> REG_SZ) then
          Result := 0
        else begin
          Val(Data, Result, E);
          if E <> 0 then Result := 0;
        end;
      end;
    end;
  end;
begin
  if Lo(GetVersion) >= 4 then begin
    if not SystemParametersInfo(106{SPI_GETMENUSHOWDELAY}, 0, @Result, 0) then begin
      { SPI_GETMENUSHOWDELAY is only supported by Windows NT 4.0 and Windows 98.
        On Windows 95, it must use the registry to retrieve this setting. }
      if not RegMenuShowDelayInited then begin
        RegMenuShowDelay := ReadMenuShowDelayFromRegistry;
        InterlockedExchange(Integer(RegMenuShowDelayInited), Ord(True));
      end;
      Result := RegMenuShowDelay;
    end;
    if Result < 0 then Result := 0;
  end
  else
    Result := DefaultMenuShowDelay;
end;

function AreFlatMenusEnabled: Boolean;
{ Returns True if "flat menus" are enabled. Always returns False on pre-XP
  Windows versions. }
const
  SPI_GETFLATMENU = $1022;
var
  FlatMenusEnabled: BOOL;
begin
  Result := SystemParametersInfo(SPI_GETFLATMENU, 0,
    @FlatMenusEnabled, 0) and FlatMenusEnabled;
end;

function AreKeyboardCuesEnabled: Boolean;
{ Returns True if "keyboard cues" are enabled. Always returns True on
  pre-2000 Windows versions. }
const
  SPI_GETKEYBOARDCUES = $100A;
var
  CuesEnabled: BOOL;
begin
  Result :=
    not SystemParametersInfo(SPI_GETKEYBOARDCUES, 0, @CuesEnabled, 0) or
    CuesEnabled;
end;

function CreateNullRegion: HRGN;
var
  R: TRect;
begin
  SetRectEmpty(R);
  Result := CreateRectRgnIndirect(R);
end;

procedure DrawInvertRect(const DC: HDC; const NewRect, OldRect: PRect;
  const NewSize, OldSize: TSize; const Brush: HBRUSH; BrushLast: HBRUSH);
{ Draws a dragging outline, hiding the old one if neccessary. This code is
  based on MFC sources.

  Either NewRect or OldRect can be nil or empty. }
var
  SaveIndex: Integer;
  rgnNew, rgnOutside, rgnInside, rgnLast, rgnUpdate: HRGN;
  R: TRect;
begin
  rgnLast := 0;
  rgnUpdate := 0;

  { First, determine the update region and select it }
  if NewRect = nil then begin
    SetRectEmpty(R);
    rgnOutside := CreateRectRgnIndirect(R);
  end
  else begin
    R := NewRect^;
    rgnOutside := CreateRectRgnIndirect(R);
    InflateRect(R, -NewSize.cx, -NewSize.cy);
    IntersectRect(R, R, NewRect^);
  end;
  rgnInside := CreateRectRgnIndirect(R);
  rgnNew := CreateNullRegion;
  CombineRgn(rgnNew, rgnOutside, rgnInside, RGN_XOR);

  if BrushLast = 0 then
    BrushLast := Brush;

  if OldRect <> nil then begin
    { Find difference between new region and old region }
    rgnLast := CreateNullRegion;
    with OldRect^ do
      SetRectRgn(rgnOutside, Left, Top, Right, Bottom);
    R := OldRect^;
    InflateRect(R, -OldSize.cx, -OldSize.cy);
    IntersectRect(R, R, OldRect^);
    SetRectRgn(rgnInside, R.Left, R.Top, R.Right, R.Bottom);
    CombineRgn(rgnLast, rgnOutside, rgnInside, RGN_XOR);

    { Only diff them if brushes are the same }
    if Brush = BrushLast then begin
      rgnUpdate := CreateNullRegion;
      CombineRgn(rgnUpdate, rgnLast, rgnNew, RGN_XOR);
    end;
  end;

  { Save the DC state so that the clipping region can be restored }
  SaveIndex := SaveDC(DC);
  try
    if (Brush <> BrushLast) and (OldRect <> nil) then begin
      { Brushes are different -- erase old region first }
      SelectClipRgn(DC, rgnLast);
      GetClipBox(DC, R);
      SelectObject(DC, BrushLast);
      PatBlt(DC, R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top, PATINVERT);
    end;

    { Draw into the update/new region }
    if rgnUpdate <> 0 then
      SelectClipRgn(DC, rgnUpdate)
    else
      SelectClipRgn(DC, rgnNew);
    GetClipBox(DC, R);
    SelectObject(DC, Brush);
    PatBlt(DC, R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top, PATINVERT);
  finally
    { Clean up DC }
    RestoreDC(DC, SaveIndex);
  end;

  { Free regions }
  if rgnNew <> 0 then DeleteObject(rgnNew);
  if rgnOutside <> 0 then DeleteObject(rgnOutside);
  if rgnInside <> 0 then DeleteObject(rgnInside);
  if rgnLast <> 0 then DeleteObject(rgnLast);
  if rgnUpdate <> 0 then DeleteObject(rgnUpdate);
end;

function CreateHalftoneBrush: HBRUSH;
const
  Patterns: array[Boolean] of Word = ($5555, $AAAA);
var
  I: Integer;
  GrayPattern: array[0..7] of Word;
  GrayBitmap: HBITMAP;
begin
  for I := 0 to 7 do
    GrayPattern[I] := Patterns[Odd(I)];
  GrayBitmap := CreateBitmap(8, 8, 1, 1, @GrayPattern);
  Result := CreatePatternBrush(GrayBitmap);
  DeleteObject(GrayBitmap);
end;

procedure DrawHalftoneInvertRect(const DC: HDC; const NewRect, OldRect: PRect;
  const NewSize, OldSize: TSize);
var
  Brush: HBRUSH;
begin
  Brush := CreateHalftoneBrush;
  try
    DrawInvertRect(DC, NewRect, OldRect, NewSize, OldSize, Brush, Brush);
  finally
    DeleteObject(Brush);
  end;
end;

function MethodsEqual(const M1, M2: TMethod): Boolean;
begin
  Result := (M1.Code = M2.Code) and (M1.Data = M2.Data);
end;

function GetRectOfPrimaryMonitor(const WorkArea: Boolean): TRect;
begin
  if not WorkArea or not SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0) then
    Result := Rect(0, 0, Screen.Width, Screen.Height);
end;

function UsingMultipleMonitors: Boolean;
{ Returns True if the system has more than one display monitor configured. }
var
  NumMonitors: Integer;
begin
  NumMonitors := GetSystemMetrics(80 {SM_CMONITORS});
  Result := (NumMonitors <> 0) and (NumMonitors <> 1);
  { ^ NumMonitors will be zero if not running Win98, NT 5, or later }
end;

type
  HMONITOR = type Integer;
  PMonitorInfo = ^TMonitorInfo;
  TMonitorInfo = record
    cbSize: DWORD;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: DWORD;
  end;
const
  MONITOR_DEFAULTTONEAREST = $2;
type
  TMultiMonApis = record
    funcMonitorFromRect: function(lprcScreenCoords: PRect; dwFlags: DWORD): HMONITOR; stdcall;
    funcMonitorFromPoint: function(ptScreenCoords: TPoint; dwFlags: DWORD): HMONITOR; stdcall;
    funcMonitorFromWindow: function(hWnd: HWND; dwFlags: DWORD): HMONITOR; stdcall;
    funcGetMonitorInfoW: function(hMonitor: HMONITOR; lpMonitorInfo: PMonitorInfo): BOOL; stdcall;
  end;

{ Under D4 I could be using the MultiMon unit for the multiple monitor
  function imports, but its stubs for MonitorFromRect and MonitorFromPoint
  are seriously bugged... So I chose to avoid the MultiMon unit entirely. }

function InitMultiMonApis(var Apis: TMultiMonApis): Boolean;
var
  User32Handle: THandle;
begin
  User32Handle := GetModuleHandle(user32);
  Apis.funcMonitorFromRect := GetProcAddress(User32Handle, 'MonitorFromRect');
  Apis.funcMonitorFromPoint := GetProcAddress(User32Handle, 'MonitorFromPoint');
  Apis.funcMonitorFromWindow := GetProcAddress(User32Handle, 'MonitorFromWindow');
  Apis.funcGetMonitorInfoW := GetProcAddress(User32Handle, 'GetMonitorInfoW');
  Result := Assigned(Apis.funcMonitorFromRect) and
    Assigned(Apis.funcMonitorFromPoint) and Assigned(Apis.funcGetMonitorInfoW);
end;

function GetRectOfMonitorContainingRect(const R: TRect;
  const WorkArea: Boolean): TRect;
{ Returns the work area of the monitor which the rectangle R intersects with
  the most, or the monitor nearest R if no monitors intersect. }
var
  Apis: TMultiMonApis;
  M: HMONITOR;
  MonitorInfo: TMonitorInfo;
begin
  if UsingMultipleMonitors and InitMultiMonApis(Apis) then begin
    M := Apis.funcMonitorFromRect(@R, MONITOR_DEFAULTTONEAREST);
    MonitorInfo.cbSize := SizeOf(MonitorInfo);
    if Apis.funcGetMonitorInfoW(M, @MonitorInfo) then begin
      if not WorkArea then
        Result := MonitorInfo.rcMonitor
      else
        Result := MonitorInfo.rcWork;
      Exit;
    end;
  end;
  Result := GetRectOfPrimaryMonitor(WorkArea);
end;

function GetRectOfMonitorContainingPoint(const P: TPoint;
  const WorkArea: Boolean): TRect;
{ Returns the screen area of the monitor containing the point P, or the monitor
  nearest P if P isn't in any monitor's work area. }
var
  Apis: TMultiMonApis;
  M: HMONITOR;
  MonitorInfo: TMonitorInfo;
begin
  if UsingMultipleMonitors and InitMultiMonApis(Apis) then begin
    M := Apis.funcMonitorFromPoint(P, MONITOR_DEFAULTTONEAREST);
    MonitorInfo.cbSize := SizeOf(MonitorInfo);
    if Apis.funcGetMonitorInfoW(M, @MonitorInfo) then begin
      if not WorkArea then
        Result := MonitorInfo.rcMonitor
      else
        Result := MonitorInfo.rcWork;
      Exit;
    end;
  end;
  Result := GetRectOfPrimaryMonitor(WorkArea);
end;

function GetRectOfMonitorContainingWindow(const W: HWND;
  const WorkArea: Boolean): TRect;
var
  Apis: TMultiMonApis;
  M: HMONITOR;
  MonitorInfo: TMonitorInfo;
begin
  if UsingMultipleMonitors and InitMultiMonApis(Apis) then begin
    M := Apis.funcMonitorFromWindow(W, MONITOR_DEFAULTTONEAREST);
    MonitorInfo.cbSize := SizeOf(MonitorInfo);
    if Apis.funcGetMonitorInfoW(M, @MonitorInfo) then begin
      if not WorkArea then
        Result := MonitorInfo.rcMonitor
      else
        Result := MonitorInfo.rcWork;
      Exit;
    end;
  end;
  Result := GetRectOfPrimaryMonitor(WorkArea);
end;

function CallTrackMouseEvent(const Wnd: HWND; const Flags: DWORD): Boolean;
var
  Track: TTrackMouseEvent;
begin
  Track.cbSize := SizeOf(Track);
  Track.dwFlags := Flags;
  Track.hwndTrack := Wnd;
  Track.dwHoverTime := 0;
  Result := TrackMouseEvent(Track);
end;

function EnumFontsProc(const lplf: TLogFont; const lptm: TTextMetric;
  dwType: DWORD; lpData: LPARAM): Integer; stdcall;
begin
  Boolean(Pointer(lpData)^) := True;
  Result := 0;
end;

function CreateRotatedFont(DC: HDC): HFONT;
{ Creates a font based on the DC's current font, but rotated 270 degrees }
var
  LogFont: TLogFont;
  TM: TTextMetric;
  VerticalFontName: array[0..LF_FACESIZE-1] of Char;
  VerticalFontExists: Boolean;
begin
  if GetObject(GetCurrentObject(DC, OBJ_FONT), SizeOf(LogFont),
     @LogFont) = 0 then begin
    { just in case... }
    Result := 0;
    Exit;
  end;
  LogFont.lfEscapement := 2700;
  LogFont.lfOrientation := 2700;
  LogFont.lfOutPrecision := OUT_TT_ONLY_PRECIS;  { needed for Win9x }

  { Don't let a random TrueType font be substituted when MS Sans Serif or
    Microsoft Sans Serif are used. Hard-code Arial. }
  if (StrIComp(LogFont.lfFaceName, 'MS Sans Serif') = 0) or
     (StrIComp(LogFont.lfFaceName, 'Microsoft Sans Serif') = 0) then begin
    StrPCopy(LogFont.lfFaceName, 'Arial');
    { Set lfHeight to the actual height of the current font. This is needed
      to work around a Windows 98 issue: on a clean install of the OS,
      SPI_GETNONCLIENTMETRICS returns -5 for lfSmCaptionFont.lfHeight. This is
      wrong; it should return -11 for an 8 pt font. With normal, unrotated text
      this actually displays correctly, since MS Sans Serif doesn't support
      sizes below 8 pt. However, when we change to a TrueType font like Arial,
      this becomes a problem because it'll actually create a font that small. }
    if GetTextMetrics(DC, TM) then begin
      { If the original height was negative, keep it negative }
      if LogFont.lfHeight <= 0 then
        LogFont.lfHeight := -(TM.tmHeight - TM.tmInternalLeading)
      else
        LogFont.lfHeight := TM.tmHeight;
    end;
  end;

  { Use a vertical font if available so that Asian characters aren't drawn
    sideways }
  if StrLen(LogFont.lfFaceName) < SizeOf(VerticalFontName)-1 then begin
    VerticalFontName[0] := '@';
    StrCopy(@VerticalFontName[1], LogFont.lfFaceName);
    VerticalFontExists := False;
    EnumFonts(DC, VerticalFontName, @EnumFontsProc, @VerticalFontExists);
    if VerticalFontExists then
      StrCopy(LogFont.lfFaceName, VerticalFontName);
  end;

  Result := CreateFontIndirect(LogFont);
end;

procedure DrawRotatedText(const DC: HDC; AText: String; const ARect: TRect;
  const AFormat: Cardinal);
{ Like DrawText, but draws the text at a 270 degree angle.
  The format flag this function respects are
  DT_NOPREFIX, DT_HIDEPREFIX, DT_CENTER, DT_END_ELLIPSIS, DT_NOCLIP }
var
  RotatedFont, SaveFont: HFONT;
  TextMetrics: TTextMetric;
  X, Y, P, I, SU, FU, W: Integer;
  SaveAlign: UINT;
  SavePen, Pen: HPEN;
  Clip: Boolean;

  function GetSize(DC: HDC; const S: string): Integer;
  var
    Size: TSize;
  begin
    GetTextExtentPoint32(DC, PChar(S), Length(S), Size);
    Result := Size.cx;
  end;

begin
  if Length(AText) = 0 then Exit;

  RotatedFont := CreateRotatedFont(DC);
  SaveFont := SelectObject(DC, RotatedFont);

  GetTextMetrics(DC, TextMetrics);
  X := ARect.Left + ((ARect.Right - ARect.Left) - TextMetrics.tmHeight) div 2;

  Clip := (AFormat and DT_NOCLIP) <> DT_NOCLIP;

  { Find the index of the character that should be underlined. Delete '&'
    characters from the string. Like DrawText, only the last prefixed character
    will be underlined. }
  P := 0;
  I := 1;
  if (AFormat and DT_NOPREFIX) <> DT_NOPREFIX then
    while I <= Length(AText) do begin
      if CharInSet(AText[I], LeadBytes) then
        Inc(I)
      else if AText[I] = '&' then begin
        Delete(AText, I, 1);
        { Note: PChar cast is so that if Delete deleted the last character in
          the string, we don't step past the end of the string (which would cause
          an AV if AText is now empty), but rather look at the null character
          and treat it as an accelerator key like DrawText. }
        if PChar(AText)[I-1] <> '&' then
          P := I;
      end;
      Inc(I);
    end;

  if (AFormat and DT_END_ELLIPSIS) = DT_END_ELLIPSIS then
  begin
    if (Length(AText) > 1) and (GetSize(DC, AText) > ARect.Bottom - ARect.Top) then
    begin
      W := ARect.Bottom - ARect.Top;
      if W > 2 then
      begin
        Delete(AText, Length(AText), 1);
        while (Length(AText) > 1) and (GetSize(DC, AText + '...') > W) do
          Delete(AText, Length(AText), 1);
      end
      else AText := AText[1];
      if P > Length(AText) then P := 0;
      AText := AText + '...';
    end;
  end;

  if (AFormat and DT_CENTER) = DT_CENTER then
    Y := ARect.Top + ((ARect.Bottom - ARect.Top) - GetSize(DC, AText)) div 2
  else
    Y := ARect.Top;

  if Clip then
  begin
    SaveDC(DC);
    with ARect do IntersectClipRect(DC, Left, Top, Right, Bottom);
  end;

  SaveAlign := SetTextAlign(DC, TA_BOTTOM);
  TextOut(DC, X, Y, PChar(AText), Length(AText));
  SetTextAlign(DC, SaveAlign);
  { Underline }
  if (P > 0) and (AFormat and DT_HIDEPREFIX = 0) then begin
    SU := GetTextWidth(DC, Copy(AText, 1, P-1), False);
    FU := SU + GetTextWidth(DC, PChar(AText)[P-1], False);
    Inc(X, TextMetrics.tmDescent - 2);
    Pen := CreatePen(PS_SOLID, 1, GetTextColor(DC));
    SavePen := SelectObject(DC, Pen);
    MoveToEx(DC, X, Y + SU, nil);
    LineTo(DC, X, Y + FU);
    SelectObject(DC, SavePen);
    DeleteObject(Pen);
  end;

  if Clip then RestoreDC(DC, -1);

  SelectObject(DC, SaveFont);
  DeleteObject(RotatedFont);
end;

function NeedToPlaySound(const Alias: String): Boolean;
{ This function checks the registry to see if the specified sound event alias
  is assigned to a file.
  The purpose of having this function is so it can avoid calls to PlaySound if
  possible, because on Windows 2000 there is an annoying 1/3 second delay on
  the first call to PlaySound.
  Windows Explorer actually uses this same technique when playing sounds for
  the Start menu. }
var
  K: HKEY;
  Data: array[0..3] of WideChar;
  DataSize: DWORD;
  ErrorCode: Longint;
begin
  Result := False;
  if RegOpenKeyEx(HKEY_CURRENT_USER,
     PChar('AppEvents\Schemes\Apps\.Default\' + Alias + '\.Current'),
     0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
    DataSize := SizeOf(Data);
    { Note: Use the 'W' version of RegQueryValueEx for more speed }
    ErrorCode := RegQueryValueExW(K, nil, nil, nil, @Data, @DataSize);
    if ((ErrorCode = ERROR_SUCCESS) and (Data[0] <> #0)) or
       (ErrorCode = ERROR_MORE_DATA) then
      Result := True;
    RegCloseKey(K);
  end;
end;

function Max(A, B: Integer): Integer;
begin
  if A >= B then
    Result := A
  else
    Result := B;
end;

function Min(A, B: Integer): Integer;
begin
  if A <= B then
    Result := A
  else
    Result := B;
end;

function FindAccelChar(const S: String): Char;
{ Finds the last accelerator key in S. Returns #0 if no accelerator key was
  found. '&&' is ignored. }
var
  P: PChar;
begin
  P := PChar(S);
  Result := #0;
  while True do begin
    P := AnsiStrScan(P, '&');
    if P = nil then Break;
    Inc(P);
    if P^ <> '&' then begin
      if P^ = #0 then Break;
      Result := P^;
    end;
    Inc(P);
  end;
end;

function GetInputLocaleCodePage: UINT;
{ Returns the code page identifier of the active input locale, or CP_ACP if
  for some unknown reason it couldn't be determined. }
var
  Buf: array[0..15] of Char;
  ErrorCode: Integer;
begin
  if GetLocaleInfo(GetKeyboardLayout(0) and $FFFF, LOCALE_IDEFAULTANSICODEPAGE,
     Buf, SizeOf(Buf)) > 0 then begin
    Buf[High(Buf)] := #0;  { ensure null termination, just in case... }
    Val(Buf, Result, ErrorCode);
    { Just to be *completely* safe, verify that the code page returned by
      GetLocaleInfo actually exists. The result of this function may be fed
      into WideCharToMultiByte, and we don't want WideCharToMultiByte to fail
      entirely because of a bad code page. }
    if (ErrorCode <> 0) or not IsValidCodePage(Result) then
      Result := CP_ACP;
  end
  else
    Result := CP_ACP;
end;

end.
