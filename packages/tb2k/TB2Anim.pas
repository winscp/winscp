unit TB2Anim;

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

  $jrsoftware: tb2k/Source/TB2Anim.pas,v 1.7 2004/05/29 20:29:26 jr Exp $
}

interface

{$I TB2Ver.inc}
{$Q-}

uses
  Windows, Messages, SysUtils, Classes;

const
  WM_TB2K_STEPANIMATION = WM_USER + $555;
  WM_TB2K_ANIMATIONENDED = WM_USER + $556;

type
  TTBAnimationDirection = set of (tbadLeft, tbadRight, tbadDown, tbadUp);

procedure TBStartAnimation(const AWnd: HWND; const ATime: Integer;
  const ABlend: Boolean; const ADirection: TTBAnimationDirection);
procedure TBStepAnimation(const Msg: TMessage);
procedure TBEndAnimation(const Wnd: HWND);

implementation

{ Notes to self:
  - It originally had the NOMIRRORBITMAP flag on the BitBlt calls, because
    Windows 2000's AnimateWindow function has it. But it had to be removed
    because on Windows 98 with the Standard VGA or VMware video driver, it
    caused no bits to be blitted, even though Windows 98 is supposed to
    support NOMIRRORBITMAP according to the documentation. I don't think it's
    necessary anyway.
}

const
  DCX_USESTYLE = $10000;
  WS_EX_LAYERED = $80000;
  NOMIRRORBITMAP = $80000000;
  ULW_ALPHA = 2;

type
  PAnimateThreadFuncData = ^TAnimateThreadFuncData;
  TAnimateThreadFuncData = record
    Wnd: HWND;
    Time: Integer;
    Blending: Boolean;
    CurStep: Integer;
    DC, BmpDC: HDC;
    Bmp: HBITMAP;
    ScreenClientRect: TRect;
    Size: TPoint;
    LastPos: TPoint;
    Direction: TTBAnimationDirection;
    AnimateThreadAbort: BOOL;
    AnimationEnded: BOOL;
    StepMessagePending: BOOL;
  end;

var
  AnimateThreadHandle: THandle;
  AnimateData: TAnimateThreadFuncData;
  AnimationSequenceID: Integer;
  FuncsInited: BOOL;
  //SetLayoutProc: function(hdc: HDC; dwLayout: DWORD): DWORD; stdcall;
  UpdateLayeredWindowProc: function(Handle: THandle; hdcDest: HDC;
    pptDst: PPoint; _psize: PSize; hdcSrc: HDC; pptSrc: PPoint;
    crKey: COLORREF; pblend: PBLENDFUNCTION; dwFlags: DWORD): BOOL; stdcall;

procedure InitializeFunctions;
begin
  if FuncsInited then
    Exit;
  //SetLayoutProc := GetProcAddress(GetModuleHandle(gdi32), 'SetLayout');
  UpdateLayeredWindowProc := GetProcAddress(GetModuleHandle(user32),
    'UpdateLayeredWindow');
  InterlockedExchange(Integer(FuncsInited), Ord(True));
end;

function AnimateThreadFunc(Parameter: Pointer): Integer;
const
  BlendStep = 16;  { must be power of 2 }
var
  Freq, Start, Cur: Int64;
  I, ElapsedTime, ElapsedBlend, Diff: Integer;
  P: TPoint;
begin
  Result := 0;
  QueryPerformanceFrequency(Freq);
  Freq := Freq div 1000;
  QueryPerformanceCounter(Start);
  with AnimateData do begin
    while not AnimateThreadAbort do begin
      QueryPerformanceCounter(Cur);
      ElapsedTime := (Cur - Start) div Freq;
      ElapsedBlend := MulDiv(255, ElapsedTime, Time);
      I := ElapsedBlend and not (BlendStep-1);
      if (I < 0) or (I >= 255) or (ElapsedTime >= Time) then
        Break;
      GetCursorPos(P);
      if (P.X <> LastPos.X) or (P.Y <> LastPos.Y) then begin
        if PtInRect(ScreenClientRect, P) then
          Break;
        LastPos := P;
      end;
      if I > CurStep then begin
        CurStep := I;
        if InterlockedExchange(Integer(StepMessagePending), 1) = 0 then
          SendNotifyMessage(Wnd, WM_TB2K_STEPANIMATION, 0, AnimationSequenceID);
      end
      else begin
        Diff := MulDiv((CurStep + BlendStep) - ElapsedBlend, Time, 255);
        if Diff < 1 then
          Diff := 1;
        Sleep(Diff);
      end;
    end;
    AnimationEnded := True;
    SendNotifyMessage(Wnd, WM_TB2K_STEPANIMATION, 0, AnimationSequenceID);
  end;
end;

procedure FinalizeAnimation;
begin
  with AnimateData do begin
    if Blending then
      SetWindowLong(Wnd, GWL_EXSTYLE,
        GetWindowLong(Wnd, GWL_EXSTYLE) and not WS_EX_LAYERED)
    else
      SetWindowRgn(Wnd, 0, False);
    BitBlt(DC, 0, 0, Size.X, Size.Y, BmpDC, 0, 0, SRCCOPY);
    DeleteDC(BmpDC);
    DeleteObject(Bmp);
    ReleaseDC(Wnd, DC);
    SendNotifyMessage(Wnd, WM_TB2K_ANIMATIONENDED, 0, 0);
  end;
end;

procedure TBEndAnimation(const Wnd: HWND);
begin
  if (AnimateThreadHandle <> 0) and
     ((Wnd = 0) or (AnimateData.Wnd = Wnd)) then begin
    AnimateData.AnimateThreadAbort := True;
    WaitForSingleObject(AnimateThreadHandle, INFINITE);
    CloseHandle(AnimateThreadHandle);
    AnimateThreadHandle := 0;

    FinalizeAnimation;
  end;
end;

procedure TBStartAnimation(const AWnd: HWND; const ATime: Integer;
  const ABlend: Boolean; const ADirection: TTBAnimationDirection);
var
  ZeroPt: TPoint;
  R: TRect;
  ThreadID: DWORD;
  Blend: TBlendFunction;
  Rgn: HRGN;
begin
  TBEndAnimation(0);

  ZeroPt.X := 0;
  ZeroPt.Y := 0;

  InitializeFunctions;

  FillChar(AnimateData, SizeOf(AnimateData), 0);
  with AnimateData do begin
    Wnd := AWnd;
    Time := ATime;
    Blending := ABlend and Assigned(UpdateLayeredWindowProc);
    Direction := ADirection;
    GetCursorPos(LastPos);
    GetClientRect(Wnd, ScreenClientRect);
    MapWindowPoints(Wnd, 0, ScreenClientRect, 2);
    GetWindowRect(Wnd, R);
    DC := GetDCEx(Wnd, 0, DCX_WINDOW or DCX_CACHE {or DCX_USESTYLE ?});
    Size.X := R.Right - R.Left;
    Size.Y := R.Bottom - R.Top;
    Bmp := CreateCompatibleBitmap(DC, Size.X, Size.Y {or $01000000 ?});
    BmpDC := CreateCompatibleDC(DC);
    // AnimateWindow calls SetLayout, but I'm not sure that we need to. 
    //if Assigned(SetLayoutProc) then
    //  SetLayoutProc(BmpDC, 0);
    SelectObject(BmpDC, Bmp);
    //SetBoundsRect(BmpDC, nil, DCB_RESET or DCB_ENABLE);
    SendMessage(Wnd, WM_PRINT, WPARAM(BmpDC), PRF_NONCLIENT or PRF_CLIENT or
      PRF_ERASEBKGND or PRF_CHILDREN);
    //GetBoundsRect
    if Blending then begin
      SetWindowLong(Wnd, GWL_EXSTYLE, GetWindowLong(Wnd, GWL_EXSTYLE) or WS_EX_LAYERED);
      CurStep := 32;
      Longint(Blend) := 0;
      Blend.BlendOp := AC_SRC_OVER;
      Blend.SourceConstantAlpha := CurStep;
      UpdateLayeredWindowProc(Wnd, 0, @R.TopLeft, @Size, BmpDC, @ZeroPt, 0,
        @Blend, ULW_ALPHA);
    end
    else begin
      CurStep := 0;
      Rgn := CreateRectRgn(0, 0, 0, 0);
      if not BOOL(SetWindowRgn(Wnd, Rgn, False)) then
        DeleteObject(Rgn);  { just in case }
    end;
    { These are the same flags AnimateWindow uses. SWP_ASYNCWINDOWPOS is
      needed or else it doesn't "save bits" properly.
      Note: SWP_ASYNCWINDOWPOS seems to have no effect on Windows 95 & NT 4.0,
      so bits behind the window are not saved & restored correctly. }
    SetWindowPos(Wnd, 0, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or
      SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOREDRAW or
      SWP_NOOWNERZORDER or SWP_ASYNCWINDOWPOS);
  end;

  AnimateThreadHandle := BeginThread(nil, 0, AnimateThreadFunc, nil,
    CREATE_SUSPENDED, ThreadID);
  if AnimateThreadHandle = 0 then begin
    { just in case... }
    FinalizeAnimation;
    Exit;
  end;
  Inc(AnimationSequenceID);
  ResumeThread(AnimateThreadHandle);
end;

procedure TBStepAnimation(const Msg: TMessage);
var
  Rgn: HRGN;
  Blend: TBlendFunction;
  X, Y: Integer;
begin
  if Msg.LParam <> AnimationSequenceID then
    { ignore messages dangling in the queue from aborted animation sequences }
    Exit;
  with AnimateData do begin
    if not AnimationEnded then begin
      if Blending then begin
        InitializeFunctions;
        Longint(Blend) := 0;
        Blend.BlendOp := AC_SRC_OVER;
        Blend.SourceConstantAlpha := AnimateData.CurStep;
        UpdateLayeredWindowProc(Wnd, 0, nil, nil, 0, nil, 0, @Blend, 2);
      end
      else begin
        if tbadDown in Direction then
          Y := MulDiv(Size.Y, AnimateData.CurStep, 255) - Size.Y
        else if tbadUp in Direction then
          Y := Size.Y - MulDiv(Size.Y, AnimateData.CurStep, 255)
        else
          Y := 0;
        if tbadRight in Direction then
          X := MulDiv(Size.X, AnimateData.CurStep, 255) - Size.X
        else if tbadLeft in Direction then
          X := Size.X - MulDiv(Size.X, AnimateData.CurStep, 255)
        else
          X := 0;
        Rgn := CreateRectRgn(X, Y, X + Size.X, Y + Size.Y);
        if not BOOL(SetWindowRgn(Wnd, Rgn, False)) then
          DeleteObject(Rgn);  { just in case }
        BitBlt(DC, X, Y, Size.X, Size.Y, BmpDC, 0, 0, SRCCOPY);
      end;
    end
    else
      TBEndAnimation(Wnd);
    StepMessagePending := False;
  end;
end;

initialization
finalization
  TBEndAnimation(0);
end.
