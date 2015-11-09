unit TB2Acc;

{
  Toolbar2000
  Copyright (C) 1998-2005 by Jordan Russell
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

  $jrsoftware: tb2k/Source/TB2Acc.pas,v 1.7 2005/01/06 03:56:50 jr Exp $

  This unit is used internally to implement the IAccessible interface on
  TTBView and TTBItemViewer for Microsoft Active Accessibility support.
}

interface

{$I TB2Ver.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TB2Item;

type
  { Our declaration for IAccessible }
  ITBAccessible = interface(IDispatch)
    ['{618736E0-3C3D-11CF-810C-00AA00389B71}']
    function get_accParent(out ppdispParent: IDispatch): HRESULT; stdcall;
    function get_accChildCount(out pcountChildren: Integer): HRESULT; stdcall;
    function get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HRESULT; stdcall;
    function get_accName(varChild: OleVariant; out pszName: WideString): HRESULT; stdcall;
    function get_accValue(varChild: OleVariant; out pszValue: WideString): HRESULT; stdcall;
    function get_accDescription(varChild: OleVariant; out pszDescription: WideString): HRESULT; stdcall;
    function get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HRESULT; stdcall;
    function get_accState(varChild: OleVariant; out pvarState: OleVariant): HRESULT; stdcall;
    function get_accHelp(varChild: OleVariant; out pszHelp: WideString): HRESULT; stdcall;
    function get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant; out pidTopic: Integer): HRESULT; stdcall;
    function get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HRESULT; stdcall;
    function get_accFocus(out pvarID: OleVariant): HRESULT; stdcall;
    function get_accSelection(out pvarChildren: OleVariant): HRESULT; stdcall;
    function get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HRESULT; stdcall;
    function accSelect(flagsSelect: Integer; varChild: OleVariant): HRESULT; stdcall;
    function accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer;
      out pcyHeight: Integer; varChild: OleVariant): HRESULT; stdcall;
    function accNavigate(navDir: Integer; varStart: OleVariant; out pvarEnd: OleVariant): HRESULT; stdcall;
    function accHitTest(xLeft: Integer; yTop: Integer; out pvarID: OleVariant): HRESULT; stdcall;
    function accDoDefaultAction(varChild: OleVariant): HRESULT; stdcall;
    function put_accName(varChild: OleVariant; const pszName: WideString): HRESULT; stdcall;
    function put_accValue(varChild: OleVariant; const pszValue: WideString): HRESULT; stdcall;
  end;

  TTBCustomAccObject = class(TTBBaseAccObject, IUnknown, IDispatch)
  private
    FPrevious, FNext: TTBCustomAccObject;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTBViewAccObject = class(TTBCustomAccObject, IUnknown, IDispatch, ITBAccessible)
  private
    FView: TTBView;
    function Check(const varChild: OleVariant; var ErrorCode: HRESULT): Boolean;
    { ITBAccessible }
    function accDoDefaultAction(varChild: OleVariant): HRESULT; stdcall;
    function accHitTest(xLeft: Integer; yTop: Integer; out pvarID: OleVariant): HRESULT; stdcall;
    function accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer;
      out pcyHeight: Integer; varChild: OleVariant): HRESULT; stdcall;
    function accNavigate(navDir: Integer; varStart: OleVariant; out pvarEnd: OleVariant): HRESULT; stdcall;
    function accSelect(flagsSelect: Integer; varChild: OleVariant): HRESULT; stdcall;
    function get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HRESULT; stdcall;
    function get_accChildCount(out pcountChildren: Integer): HRESULT; stdcall;
    function get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HRESULT; stdcall;
    function get_accDescription(varChild: OleVariant; out pszDescription: WideString): HRESULT; stdcall;
    function get_accFocus(out pvarID: OleVariant): HRESULT; stdcall;
    function get_accHelp(varChild: OleVariant; out pszHelp: WideString): HRESULT; stdcall;
    function get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant; out pidTopic: Integer): HRESULT; stdcall;
    function get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HRESULT; stdcall;
    function get_accName(varChild: OleVariant; out pszName: WideString): HRESULT; stdcall;
    function get_accParent(out ppdispParent: IDispatch): HRESULT; stdcall;
    function get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HRESULT; stdcall;
    function get_accSelection(out pvarChildren: OleVariant): HRESULT; stdcall;
    function get_accState(varChild: OleVariant; out pvarState: OleVariant): HRESULT; stdcall;
    function get_accValue(varChild: OleVariant; out pszValue: WideString): HRESULT; stdcall;
    function put_accName(varChild: OleVariant; const pszName: WideString): HRESULT; stdcall;
    function put_accValue(varChild: OleVariant; const pszValue: WideString): HRESULT; stdcall;
  public
    constructor Create(AView: TTBView);
    destructor Destroy; override;
    procedure ClientIsDestroying; override;
  end;

  TTBItemViewerAccObject = class(TTBCustomAccObject, IUnknown, IDispatch, ITBAccessible)
  private
    FViewer: TTBItemViewer;
    function Check(const varChild: OleVariant; var ErrorCode: HRESULT): Boolean;
    function IsActionable: Boolean;
    function IsAvailable: Boolean;
    function IsFocusable: Boolean;
    { ITBAccessible }
    function accDoDefaultAction(varChild: OleVariant): HRESULT; stdcall;
    function accHitTest(xLeft: Integer; yTop: Integer; out pvarID: OleVariant): HRESULT; stdcall;
    function accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer;
      out pcyHeight: Integer; varChild: OleVariant): HRESULT; stdcall;
    function accNavigate(navDir: Integer; varStart: OleVariant; out pvarEnd: OleVariant): HRESULT; stdcall;
    function accSelect(flagsSelect: Integer; varChild: OleVariant): HRESULT; stdcall;
    function get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HRESULT; stdcall;
    function get_accChildCount(out pcountChildren: Integer): HRESULT; stdcall;
    function get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HRESULT; stdcall;
    function get_accDescription(varChild: OleVariant; out pszDescription: WideString): HRESULT; stdcall;
    function get_accFocus(out pvarID: OleVariant): HRESULT; stdcall;
    function get_accHelp(varChild: OleVariant; out pszHelp: WideString): HRESULT; stdcall;
    function get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant; out pidTopic: Integer): HRESULT; stdcall;
    function get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HRESULT; stdcall;
    function get_accName(varChild: OleVariant; out pszName: WideString): HRESULT; stdcall;
    function get_accParent(out ppdispParent: IDispatch): HRESULT; stdcall;
    function get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HRESULT; stdcall;
    function get_accSelection(out pvarChildren: OleVariant): HRESULT; stdcall;
    function get_accState(varChild: OleVariant; out pvarState: OleVariant): HRESULT; stdcall;
    function get_accValue(varChild: OleVariant; out pszValue: WideString): HRESULT; stdcall;
    function put_accName(varChild: OleVariant; const pszName: WideString): HRESULT; stdcall;
    function put_accValue(varChild: OleVariant; const pszValue: WideString): HRESULT; stdcall;
  public
    constructor Create(AViewer: TTBItemViewer);
    destructor Destroy; override;
    procedure ClientIsDestroying; override;
    procedure HandleAccSelect(const AExecute: Boolean);
  end;

procedure CallNotifyWinEvent(event: DWORD; hwnd: HWND; idObject: DWORD;
  idChild: Longint);
function InitializeOleAcc: Boolean;

var
  LresultFromObjectFunc: function(const riid: TGUID; wParam: WPARAM;
    pUnk: IUnknown): LRESULT; stdcall;
  AccessibleObjectFromWindowFunc: function(hwnd: HWND; dwId: DWORD;
    const riid: TGUID; out ppvObject: Pointer): HRESULT; stdcall;

  { For debugging purposes only: }
  ViewAccObjectInstances: Integer = 0;
  ItemViewerAccObjectInstances: Integer = 0;

implementation

uses
  {$IFDEF JR_D6} Variants, {$ENDIF} ActiveX, Menus, TB2Common;

const
  { Constants from OleAcc.h }
  ROLE_SYSTEM_MENUBAR = $2;
  ROLE_SYSTEM_CLIENT = $a;
  ROLE_SYSTEM_MENUPOPUP = $b;
  ROLE_SYSTEM_MENUITEM = $c;
  ROLE_SYSTEM_SEPARATOR = $15;
  ROLE_SYSTEM_TOOLBAR = $16;
  ROLE_SYSTEM_PUSHBUTTON = $2b;
  ROLE_SYSTEM_BUTTONMENU = $39;

  STATE_SYSTEM_HASPOPUP = $40000000;

  NAVDIR_UP         = 1;
  NAVDIR_DOWN       = 2;
  NAVDIR_LEFT       = 3;
  NAVDIR_RIGHT      = 4;
  NAVDIR_NEXT       = 5;
  NAVDIR_PREVIOUS   = 6;
  NAVDIR_FIRSTCHILD = 7;
  NAVDIR_LASTCHILD  = 8;

  SELFLAG_TAKEFOCUS = 1;

type
  TControlAccess = class(TControl);
  TTBViewAccess = class(TTBView);
  TTBCustomItemAccess = class(TTBCustomItem);
  TTBItemViewerAccess = class(TTBItemViewer);

var
  LastAccObject: TTBCustomAccObject;  { last object in the linked list }
  LastAccObjectCritSect: TRTLCriticalSection;

  NotifyWinEventInited: BOOL;
  NotifyWinEventFunc: procedure(event: DWORD; hwnd: HWND; idObject: DWORD;
    idChild: Longint); stdcall;

procedure CallNotifyWinEvent(event: DWORD; hwnd: HWND; idObject: DWORD;
  idChild: Longint);
begin
  if not NotifyWinEventInited then begin
    NotifyWinEventFunc := GetProcAddress(GetModuleHandle(user32), 'NotifyWinEvent');
    InterlockedExchange(Integer(NotifyWinEventInited), Ord(True));
  end;
  if Assigned(NotifyWinEventFunc) then
    NotifyWinEventFunc(event, hwnd, idObject, idChild);
end;

var
  OleAccInited: BOOL;
  OleAccAvailable: BOOL;

function InitializeOleAcc: Boolean;
var
  M: HMODULE;
begin
  if not OleAccInited then begin
    M := {$IFDEF JR_D5} SafeLoadLibrary {$ELSE} LoadLibrary {$ENDIF} ('oleacc.dll');
    if M <> 0 then begin
      LresultFromObjectFunc := GetProcAddress(M, 'LresultFromObject');
      AccessibleObjectFromWindowFunc := GetProcAddress(M, 'AccessibleObjectFromWindow');
      if Assigned(LresultFromObjectFunc) and
         Assigned(AccessibleObjectFromWindowFunc) then
        OleAccAvailable := True;
    end;
    InterlockedExchange(Integer(OleAccInited), Ord(True));
  end;
  Result := OleAccAvailable;
end;

function AccObjectFromWindow(const Wnd: HWND; out ADisp: IDispatch): Boolean;
var
  P: Pointer;
begin
  if Assigned(AccessibleObjectFromWindowFunc) and
     (AccessibleObjectFromWindowFunc(Wnd, OBJID_WINDOW, IDispatch, P) = S_OK) then begin
    ADisp := IDispatch(P);
    IDispatch(P)._Release;
    Result := True;
  end
  else
    Result := False;
end;

procedure DisconnectAccObjects;
{ This procedure calls CoDisconnectObject() on all acc. objects still
  allocated. This is needed to prevent potential AV's when TB2k is compiled
  into a DLL, since a DLL may be freed by the application while an MSAA
  client still holds acc. object references. }
var
  Obj, PrevObj: TTBCustomAccObject;
begin
  Obj := LastAccObject;
  while Assigned(Obj) do begin
    { Make a copy of Obj.FPrevious since CoDisconnectObject may cause Obj
      to be freed }
    PrevObj := Obj.FPrevious;
    { CoDisconnectObject should cause remote MSAA clients to release all
      references to the object, thus destroying it (assuming the local
      application doesn't have references of its own). }
    CoDisconnectObject(Obj, 0);
    Obj := PrevObj;
  end;
end;

function GetAltKeyName: String;
{ This silly function is needed since ShortCutToText(VK_MENU) fails on Delphi
  and C++Builder versions <= 4 }
var
  ScanCode: UINT;
  KeyName: array[0..255] of Char;
begin
  ScanCode := MapVirtualKey(VK_MENU, 0) shl 16;
  if (ScanCode <> 0) and
     (GetKeyNameText(ScanCode, KeyName, SizeOf(KeyName)) > 0) then
    Result := KeyName
  else
    Result := 'Alt';  { shouldn't get here, but just in case... }
end;

{ TTBCustomAccObject }

constructor TTBCustomAccObject.Create;
begin
  inherited Create;
  { Add Self to linked list of objects }
  EnterCriticalSection(LastAccObjectCritSect);
  try
    FPrevious := LastAccObject;
    if Assigned(FPrevious) then
      FPrevious.FNext := Self;
    LastAccObject := Self;
  finally
    LeaveCriticalSection(LastAccObjectCritSect);
  end;
end;

destructor TTBCustomAccObject.Destroy;
begin
  { Remove Self from linked list of objects }
  EnterCriticalSection(LastAccObjectCritSect);
  try
    if LastAccObject = Self then
      LastAccObject := FPrevious;
    if Assigned(FPrevious) then
      FPrevious.FNext := FNext;
    if Assigned(FNext) then
      FNext.FPrevious := FPrevious;
  finally
    LeaveCriticalSection(LastAccObjectCritSect);
  end;
  inherited;
end;

{ TTBViewAccObject }

constructor TTBViewAccObject.Create(AView: TTBView);
begin
  inherited Create;
  FView := AView;
  InterlockedIncrement(ViewAccObjectInstances);
end;

destructor TTBViewAccObject.Destroy;
begin
  InterlockedDecrement(ViewAccObjectInstances);
  if Assigned(FView) then begin
    TTBViewAccess(FView).FAccObjectInstance := nil;
    FView := nil;
  end;
  inherited;
end;

procedure TTBViewAccObject.ClientIsDestroying;
begin
  FView := nil;
end;

function TTBViewAccObject.Check(const varChild: OleVariant;
  var ErrorCode: HRESULT): Boolean;
begin
  if FView = nil then begin
    ErrorCode := E_FAIL;
    Result := False;
  end
  else if (VarType(varChild) <> varInteger) or (varChild <> CHILDID_SELF) then begin
    ErrorCode := E_INVALIDARG;
    Result := False;
  end
  else
    Result := True;
end;

function TTBViewAccObject.accDoDefaultAction(varChild: OleVariant): HRESULT;
begin
  Result := S_FALSE;
end;

function TTBViewAccObject.accHitTest(xLeft, yTop: Integer;
  out pvarID: OleVariant): HRESULT;
var
  ViewWnd, W: HWND;
  R: TRect;
  P: TPoint;
  D: IDispatch;
  V: TTBItemViewer;
begin
  try
    if FView = nil then begin
      Result := E_FAIL;
      Exit;
    end;
    ViewWnd := FView.Window.Handle;
    GetWindowRect(ViewWnd, R);
    P.X := xLeft;
    P.Y := yTop;
    if PtInRect(R, P) then begin
      P := FView.Window.ScreenToClient(P);
      W := ChildWindowFromPointEx(ViewWnd, P, CWP_SKIPINVISIBLE);
      if (W <> 0) and (W <> ViewWnd) then begin
        { Point is inside a child window (most likely belonging to a
          TTBControlItem) }
        if AccObjectFromWindow(W, D) then begin
          pvarID := D;
          Result := S_OK;
        end
        else
          Result := E_UNEXPECTED;
      end
      else begin
        V := FView.ViewerFromPoint(P);
        if Assigned(V) then
          pvarID := V.GetAccObject
        else
          pvarID := CHILDID_SELF;
        Result := S_OK;
      end;
    end
    else
      Result := S_FALSE;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBViewAccObject.accLocation(out pxLeft, pyTop, pcxWidth,
  pcyHeight: Integer; varChild: OleVariant): HRESULT;
var
  R: TRect;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    GetWindowRect(FView.Window.Handle, R);
    pxLeft := R.Left;
    pyTop := R.Top;
    pcxWidth := R.Right - R.Left;
    pcyHeight := R.Bottom - R.Top;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBViewAccObject.accNavigate(navDir: Integer; varStart: OleVariant;
  out pvarEnd: OleVariant): HRESULT;
var
  I: Integer;
begin
  try
    if not Check(varStart, Result) then
      Exit;
    Result := S_FALSE;
    case navDir of
      NAVDIR_FIRSTCHILD: begin
          for I := 0 to FView.ViewerCount-1 do
            if FView.Viewers[I].IsAccessible then begin
              pvarEnd := FView.Viewers[I].GetAccObject;
              Result := S_OK;
              Break;
            end;
        end;
      NAVDIR_LASTCHILD: begin
          for I := FView.ViewerCount-1 downto 0 do
            if FView.Viewers[I].IsAccessible then begin
              pvarEnd := FView.Viewers[I].GetAccObject;
              Result := S_OK;
              Break;
            end;
        end;
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBViewAccObject.accSelect(flagsSelect: Integer;
  varChild: OleVariant): HRESULT;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

function TTBViewAccObject.get_accChild(varChild: OleVariant;
  out ppdispChild: IDispatch): HRESULT;
var
  I, J: Integer;
  Viewer: TTBItemViewer;
  Ctl: TControl;
begin
  try
    if FView = nil then begin
      Result := E_FAIL;
      Exit;
    end;
    if VarType(varChild) <> varInteger then begin
      Result := E_INVALIDARG;
      Exit;
    end;
    I := varChild;
    if I = CHILDID_SELF then begin
      ppdispChild := Self;
      Result := S_OK;
    end
    else begin
      { Convert a one-based child index (I) into a real viewer index (J) }
      J := 0;
      while J < FView.ViewerCount do begin
        if FView.Viewers[J].IsAccessible then begin
          if I = 1 then Break;
          Dec(I);
        end;
        Inc(J);
      end;
      if J >= FView.ViewerCount then begin
        { 'I' was either negative or too high }
        Result := E_INVALIDARG;
        Exit;
      end;
      Viewer := FView.Viewers[J];
      if Viewer.Item is TTBControlItem then begin
        { For windowed controls, return the window's accessible object instead
          of the item viewer's }
        Ctl := TTBControlItem(Viewer.Item).Control;
        if (Ctl is TWinControl) and TWinControl(Ctl).HandleAllocated then begin
          if AccObjectFromWindow(TWinControl(Ctl).Handle, ppdispChild) then
            Result := S_OK
          else
            Result := E_UNEXPECTED;
          Exit;
        end;
      end;
      ppdispChild := Viewer.GetAccObject;
      Result := S_OK;
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBViewAccObject.get_accChildCount(out pcountChildren: Integer): HRESULT;
var
  Count, I: Integer;
begin
  try
    if Assigned(FView) then begin
      Count := 0;
      for I := 0 to FView.ViewerCount-1 do
        if FView.Viewers[I].IsAccessible then
          Inc(Count);
      pCountChildren := Count;
      Result := S_OK;
    end
    else
      Result := E_FAIL;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBViewAccObject.get_accDefaultAction(varChild: OleVariant;
  out pszDefaultAction: WideString): HRESULT;
begin
  Result := S_FALSE;
end;

function TTBViewAccObject.get_accDescription(varChild: OleVariant;
  out pszDescription: WideString): HRESULT;
begin
  Result := S_FALSE;
end;

function TTBViewAccObject.get_accFocus(out pvarID: OleVariant): HRESULT;
begin
  Result := S_FALSE;
end;

function TTBViewAccObject.get_accHelp(varChild: OleVariant;
  out pszHelp: WideString): HRESULT;
begin
  Result := S_FALSE;
end;

function TTBViewAccObject.get_accHelpTopic(out pszHelpFile: WideString;
  varChild: OleVariant; out pidTopic: Integer): HRESULT;
begin
  pidTopic := 0;  { Delphi doesn't implicitly clear Integer 'out' parameters }
  Result := S_FALSE;
end;

function TTBViewAccObject.get_accKeyboardShortcut(varChild: OleVariant;
  out pszKeyboardShortcut: WideString): HRESULT;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    if vsMenuBar in FView.Style then begin
      pszKeyboardShortcut := GetAltKeyName;
      Result := S_OK;
    end
    else
      Result := S_FALSE;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBViewAccObject.get_accName(varChild: OleVariant;
  out pszName: WideString): HRESULT;
var
  S: String;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    if Assigned(FView.ParentView) and Assigned(FView.ParentView.OpenViewer) then
      S := StripAccelChars(TTBItemViewerAccess(FView.ParentView.OpenViewer).GetCaptionText);
    if S = '' then
      S := TControlAccess(FView.Window).Caption;
    pszName := S;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBViewAccObject.get_accParent(out ppdispParent: IDispatch): HRESULT;
begin
  try
    if Assigned(FView) then begin
      if Assigned(FView.ParentView) and Assigned(FView.ParentView.OpenViewer) then begin
        ppdispParent := FView.ParentView.OpenViewer.GetAccObject;
        Result := S_OK;
      end
      else begin
        if AccObjectFromWindow(FView.Window.Handle, ppdispParent) then
          Result := S_OK
        else
          Result := E_UNEXPECTED;
      end;
    end
    else
      Result := E_FAIL;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBViewAccObject.get_accRole(varChild: OleVariant;
  out pvarRole: OleVariant): HRESULT;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    if FView.IsPopup then
      pvarRole := ROLE_SYSTEM_MENUPOPUP
    else begin
      if vsMenuBar in FView.Style then
        pvarRole := ROLE_SYSTEM_MENUBAR
      else
        pvarRole := ROLE_SYSTEM_TOOLBAR;
    end;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBViewAccObject.get_accSelection(out pvarChildren: OleVariant): HRESULT;
begin
  Result := S_FALSE;
end;

function TTBViewAccObject.get_accState(varChild: OleVariant;
  out pvarState: OleVariant): HRESULT;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    pvarState := 0;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBViewAccObject.get_accValue(varChild: OleVariant;
  out pszValue: WideString): HRESULT;
begin
  Result := S_FALSE;
end;

function TTBViewAccObject.put_accName(varChild: OleVariant;
  const pszName: WideString): HRESULT;
begin
  Result := S_FALSE;
end;

function TTBViewAccObject.put_accValue(varChild: OleVariant;
  const pszValue: WideString): HRESULT;
begin
  Result := S_FALSE;
end;

{ TTBItemViewerAccObject }

constructor TTBItemViewerAccObject.Create(AViewer: TTBItemViewer);
begin
  inherited Create;
  FViewer := AViewer;
  InterlockedIncrement(ItemViewerAccObjectInstances);
end;

destructor TTBItemViewerAccObject.Destroy;
begin
  InterlockedDecrement(ItemViewerAccObjectInstances);
  if Assigned(FViewer) then begin
    TTBItemViewerAccess(FViewer).FAccObjectInstance := nil;
    FViewer := nil;
  end;
  inherited;
end;

procedure TTBItemViewerAccObject.ClientIsDestroying;
begin
  FViewer := nil;
end;

function TTBItemViewerAccObject.Check(const varChild: OleVariant;
  var ErrorCode: HRESULT): Boolean;
begin
  if FViewer = nil then begin
    ErrorCode := E_FAIL;
    Result := False;
  end
  else if (VarType(varChild) <> varInteger) or (varChild <> CHILDID_SELF) then begin
    ErrorCode := E_INVALIDARG;
    Result := False;
  end
  else
    Result := True;
end;

function TTBItemViewerAccObject.IsActionable: Boolean;
{ Returns True if 'doDefaultAction' may be performed on the viewer, i.e. if
  it's visible/off-edge/clipped, enabled & selectable, and the view is
  focusable. }
begin
  Result := FViewer.IsAccessible and IsAvailable and IsFocusable;
end;

function TTBItemViewerAccObject.IsAvailable: Boolean;
{ Returns True if the viewer's item is enabled and selectable }
begin
  Result := FViewer.Item.Enabled and
    (tbisSelectable in TTBCustomItemAccess(FViewer.Item).ItemStyle);
end;

function TTBItemViewerAccObject.IsFocusable: Boolean;
{ Returns True if viewers on the view can be 'focused' (i.e. the view's window
  doesn't have the csDesigning state, the window is visible and enabled, and
  the application is active). }

  function IsWindowAndParentsEnabled(W: HWND): Boolean;
  begin
    Result := True;
    repeat
      if not IsWindowEnabled(W) then begin
        Result := False;
        Break;
      end;
      W := GetParent(W);
    until W = 0;
  end;

var
  ViewWnd, ActiveWnd: HWND;
begin
  Result := False;
  if csDesigning in FViewer.View.Window.ComponentState then
    Exit;
  ViewWnd := FViewer.View.Window.Handle;
  if IsWindowVisible(ViewWnd) and IsWindowAndParentsEnabled(ViewWnd) then begin
    if vsModal in FViewer.View.State then
      Result := True
    else begin
      ActiveWnd := GetActiveWindow;
      if (ActiveWnd <> 0) and
         ((ActiveWnd = ViewWnd) or IsChild(ActiveWnd, ViewWnd)) then
        Result := True;
    end;
  end;
end;

procedure TTBItemViewerAccObject.HandleAccSelect(const AExecute: Boolean);
begin
  if Assigned(FViewer) and
     ((AExecute and IsActionable) or (not AExecute and IsFocusable)) then begin
    FViewer.View.Selected := FViewer;
    FViewer.View.ScrollSelectedIntoView;
    if vsModal in FViewer.View.State then begin
      if AExecute then
        FViewer.View.ExecuteSelected(False);
    end
    else if (FViewer.View.ParentView = nil) and (GetCapture = 0) then begin
      if AExecute then
        FViewer.View.EnterToolbarLoop([tbetExecuteSelected, tbetFromMSAA])
      else
        FViewer.View.EnterToolbarLoop([tbetFromMSAA]);
    end;
  end;
end;

function TTBItemViewerAccObject.accDoDefaultAction(varChild: OleVariant): HRESULT;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    { NOTE: This must be kept in synch with get_accDefaultAction }
    if IsActionable then begin
      Result := S_OK;
      if FViewer.View.OpenViewer = FViewer then begin
        FViewer.View.CancelChildPopups;
        { Like standard menus, cancel the modal loop when a top-level menu
          is closed }
        if (vsModal in FViewer.View.State) and not FViewer.View.IsPopup then
          FViewer.View.EndModal;
      end
      else begin
        FViewer.View.Selected := FViewer;
        FViewer.View.ScrollSelectedIntoView;
        TTBItemViewerAccess(FViewer).PostAccSelect(True);
      end;
    end
    else
      { Note: Standard menus return DISP_E_MEMBERNOTFOUND in this case but
        that doesn't make much sense. The member is there but just isn't
        currently available. }
      Result := E_FAIL;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.accHitTest(xLeft, yTop: Integer;
  out pvarID: OleVariant): HRESULT;
var
  P: TPoint;
begin
  try
    if FViewer = nil then begin
      Result := E_FAIL;
      Exit;
    end;
    P := FViewer.View.Window.ScreenToClient(Point(xLeft, yTop));
    if PtInRect(FViewer.BoundsRect, P) then begin
      pvarID := CHILDID_SELF;
      Result := S_OK;
    end
    else
      Result := S_FALSE;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.accLocation(out pxLeft, pyTop, pcxWidth,
  pcyHeight: Integer; varChild: OleVariant): HRESULT;
var
  R: TRect;
  P: TPoint;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    R := FViewer.BoundsRect;
    P := FViewer.View.Window.ClientToScreen(Point(0, 0));
    OffsetRect(R, P.X, P.Y);
    pxLeft := R.Left;
    pyTop := R.Top;
    pcxWidth := R.Right - R.Left;
    pcyHeight := R.Bottom - R.Top;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.accNavigate(navDir: Integer; varStart: OleVariant;
  out pvarEnd: OleVariant): HRESULT;
var
  I, J: Integer;
  View: TTBView;
begin
  try
    if not Check(varStart, Result) then
      Exit;
    Result := S_FALSE;
    if (navDir = NAVDIR_FIRSTCHILD) or (navDir = NAVDIR_LASTCHILD) then begin
      { Return the child view's acc. object }
      View := FViewer.View.OpenViewerView;
      if Assigned(View) then begin
        pvarEnd := View.GetAccObject;
        Result := S_OK;
      end;
    end
    else begin
      I := FViewer.View.IndexOf(FViewer);
      if I >= 0 then begin
        case navDir of
          NAVDIR_UP, NAVDIR_LEFT, NAVDIR_PREVIOUS:
            for J := I-1 downto 0 do
              if FViewer.View.Viewers[J].IsAccessible then begin
                pvarEnd := FViewer.View.Viewers[J].GetAccObject;
                Result := S_OK;
                Break;
              end;
          NAVDIR_DOWN, NAVDIR_RIGHT, NAVDIR_NEXT:
            for J := I+1 to FViewer.View.ViewerCount-1 do
              if FViewer.View.Viewers[J].IsAccessible then begin
                pvarEnd := FViewer.View.Viewers[J].GetAccObject;
                Result := S_OK;
                Break;
              end;
        end;
      end;
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.accSelect(flagsSelect: Integer;
  varChild: OleVariant): HRESULT;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    if flagsSelect <> SELFLAG_TAKEFOCUS then begin
      Result := E_INVALIDARG;
      Exit;
    end;
    if IsFocusable and (FViewer.Show or FViewer.Clipped) then begin
      FViewer.View.Selected := FViewer;
      FViewer.View.ScrollSelectedIntoView;
      if not(vsModal in FViewer.View.State) and
         (FViewer.View.ParentView = nil) then
        TTBItemViewerAccess(FViewer).PostAccSelect(False);
    end
    else
      Result := E_FAIL;
      { ^ what Office XP returns when you try focusing an off-edge item }
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.get_accChild(varChild: OleVariant;
  out ppdispChild: IDispatch): HRESULT;
var
  View: TTBView;
begin
  try
    if FViewer = nil then begin
      Result := E_FAIL;
      Exit;
    end;
    Result := E_INVALIDARG;
    if VarType(varChild) = varInteger then begin
      if varChild = CHILDID_SELF then begin
        ppdispChild := Self;
        Result := S_OK;
      end
      else if varChild = 1 then begin
        { Return the child view's acc. object }
        View := FViewer.View.OpenViewerView;
        if Assigned(View) then begin
          ppdispChild := View.GetAccObject;
          Result := S_OK;
        end;
      end;
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.get_accChildCount(out pcountChildren: Integer): HRESULT;
begin
  try
    if FViewer = nil then begin
      Result := E_FAIL;
      Exit;
    end;
    { Return 1 if the viewer has a child view }
    if FViewer.View.OpenViewer = FViewer then
      pCountChildren := 1
    else
      pCountChildren := 0;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.get_accDefaultAction(varChild: OleVariant;
  out pszDefaultAction: WideString): HRESULT;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    if IsActionable then begin
      { I'm not sure if these should be localized, or even if any screen
        readers make use of this text...
        NOTE: This must be kept in synch with accDoDefaultAction }
      if FViewer.View.OpenViewer = FViewer then
        pszDefaultAction := 'Close'
      else if tbisSubmenu in TTBCustomItemAccess(FViewer.Item).ItemStyle then
        pszDefaultAction := 'Open'
      else if FViewer.View.IsPopup or (vsMenuBar in FViewer.View.Style) then
        pszDefaultAction := 'Execute'
      else
        pszDefaultAction := 'Press';
      Result := S_OK;
    end
    else
      Result := S_FALSE;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.get_accDescription(varChild: OleVariant;
  out pszDescription: WideString): HRESULT;
begin
  Result := S_FALSE;
end;

function TTBItemViewerAccObject.get_accFocus(out pvarID: OleVariant): HRESULT;
begin
  try
    if FViewer = nil then begin
      Result := E_FAIL;
      Exit;
    end;
    if (vsModal in FViewer.View.State) and
       (FViewer.View.Selected = FViewer) then begin
      pvarID := CHILDID_SELF;
      Result := S_OK;
    end
    else
      Result := S_FALSE;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.get_accHelp(varChild: OleVariant;
  out pszHelp: WideString): HRESULT;
begin
  Result := S_FALSE;
end;

function TTBItemViewerAccObject.get_accHelpTopic(out pszHelpFile: WideString;
  varChild: OleVariant; out pidTopic: Integer): HRESULT;
begin
  pidTopic := 0;  { Delphi doesn't implicitly clear Integer 'out' parameters }
  Result := S_FALSE;
end;

function TTBItemViewerAccObject.get_accKeyboardShortcut(varChild: OleVariant;
  out pszKeyboardShortcut: WideString): HRESULT;
var
  C: Char;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    Result := S_FALSE;
    if TTBItemViewerAccess(FViewer).CaptionShown then begin
      C := FindAccelChar(TTBItemViewerAccess(FViewer).GetCaptionText);
      if C <> #0 then begin
        CharLowerBuff(@C, 1);  { like standard menus, always use lowercase... }
        if FViewer.View.IsPopup then
          pszKeyboardShortcut := C
        else begin
          { Prefix 'Alt+' }
          pszKeyboardShortcut := GetAltKeyName + '+' + C;
        end;
        Result := S_OK;
      end;
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.get_accName(varChild: OleVariant;
  out pszName: WideString): HRESULT;
var
  C, S: String;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    C := StripAccelChars(TTBItemViewerAccess(FViewer).GetCaptionText);
    if not FViewer.IsToolbarStyle then
      S := FViewer.Item.GetShortCutText;
    if S = '' then
      pszName := C
    else
      pszName := C + #9 + S;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.get_accParent(out ppdispParent: IDispatch): HRESULT;
begin
  try
    if Assigned(FViewer) then begin
      ppdispParent := FViewer.View.GetAccObject;
      Result := S_OK;
    end
    else
      Result := E_FAIL;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.get_accRole(varChild: OleVariant;
  out pvarRole: OleVariant): HRESULT;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    pvarRole := TTBItemViewerAccess(FViewer).GetAccRole;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.get_accSelection(out pvarChildren: OleVariant): HRESULT;
begin
  Result := S_FALSE;
end;

function TTBItemViewerAccObject.get_accState(varChild: OleVariant;
  out pvarState: OleVariant): HRESULT;
var
  Flags: Integer;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    Flags := 0;
    if FViewer.View.Selected = FViewer then begin
      Flags := Flags or STATE_SYSTEM_HOTTRACKED;
      if vsModal in FViewer.View.State then
        Flags := Flags or STATE_SYSTEM_FOCUSED;
      if FViewer.View.MouseOverSelected and FViewer.View.Capture then
        { ^ based on "IsPushed :=" code in TTBView.DrawItem }
        Flags := Flags or STATE_SYSTEM_PRESSED;
    end;
    if tbisSubmenu in TTBCustomItemAccess(FViewer.Item).ItemStyle then
      Flags := Flags or STATE_SYSTEM_HASPOPUP;
    if FViewer.Show or FViewer.Clipped then begin
      if IsFocusable then
        Flags := Flags or STATE_SYSTEM_FOCUSABLE;
    end
    else begin
      { Mark off-edge items as invisible, like Office }
      Flags := Flags or STATE_SYSTEM_INVISIBLE;
    end;
    if not IsAvailable then
      Flags := Flags or STATE_SYSTEM_UNAVAILABLE;
    if FViewer.Item.Checked then
      Flags := Flags or STATE_SYSTEM_CHECKED;
    pvarState := Flags;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.get_accValue(varChild: OleVariant;
  out pszValue: WideString): HRESULT;
begin
  try
    if not Check(varChild, Result) then
      Exit;
    if TTBItemViewerAccess(FViewer).GetAccValue(pszValue) then
      Result := S_OK
    else begin
      pszValue := '';
      Result := S_FALSE;
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTBItemViewerAccObject.put_accName(varChild: OleVariant;
  const pszName: WideString): HRESULT;
begin
  Result := S_FALSE;
end;

function TTBItemViewerAccObject.put_accValue(varChild: OleVariant;
  const pszValue: WideString): HRESULT;
begin
  Result := S_FALSE;
end;

{ Note: This COM initialization code based on code from DBTables } 
var
  SaveInitProc: Pointer;
  NeedToUninitialize: Boolean;

procedure InitCOM;
begin
  if SaveInitProc <> nil then TProcedure(SaveInitProc);
  NeedToUninitialize := SUCCEEDED(CoInitialize(nil));
end;

initialization
  InitializeCriticalSection(LastAccObjectCritSect);
  if not IsLibrary then begin
    SaveInitProc := InitProc;
    InitProc := @InitCOM;
  end;
finalization
  DisconnectAccObjects;
  if NeedToUninitialize then
    CoUninitialize;
  DeleteCriticalSection(LastAccObjectCritSect);
end.
