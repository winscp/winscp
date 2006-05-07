unit XPThemes;

interface

{ This code is inspired by Windows XP Theme Manager }
{ (C) 2001-2002 Mike Lischke. All Rights Reserved.  }
{ (public@lischke-online.de, www.lischke-online.de) }

{$J+}

uses
  StdCtrls, Messages, Windows, Classes, Forms, ComCtrls;

type
  TXPTheme = class
  public
    constructor Create;
    destructor Destroy; override;

    function XPComCtl: Boolean;

    procedure ShowFocus(Form: TCustomForm);
    procedure ShowAccelerators(Form: TCustomForm);

  private
    FThemeLib: THandle;
    FWindowHandle: HWND;
    FThemesActive: Boolean;

    FDrawThemeBackground: function(hTheme: THandle; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
      pClipRect: PRECT): HRESULT; stdcall;
    FOpenThemeData: function(hwnd: HWND; pszClassList: LPCWSTR): THandle; stdcall;
    FCloseThemeData: function(hTheme: THandle): HRESULT; stdcall;
    FDrawThemeText: function(hTheme: THandle; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR; iCharCount: Integer;
      dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall;
    FGetThemeTextExtent: function(hTheme: THandle; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR; iCharCount: Integer;
      dwTextFlags: DWORD; pBoundingRect: PRECT; var pExtentRect: TRect): HRESULT; stdcall;
    FIsThemeBackgroundPartiallyTransparent: function(hTheme: THandle; iPartId, iStateId: Integer): BOOL; stdcall;
    FDrawThemeParentBackground: function(hwnd: HWND; hdc: HDC; prc: PRECT): HRESULT; stdcall;
    FGetThemeAppProperties: function: DWORD; stdcall;
    FIsAppThemed: function: BOOL; stdcall;
    FIsThemeActive: function: BOOL; stdcall;

    procedure WndProc(var Msg: TMessage);
    procedure UpdateThemesActive;
  end;

type
  TXPGroupBox = class(TGroupBox)
  private
    FButtonThemeData: THandle;
    function GetButtonThemeData: THandle;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateAccel(CharCode: Word);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  XPTheme: TXPTheme;

procedure XPPageControl(PageControl: TPageControl);

procedure Register;

implementation

uses
  Graphics, Types, Controls, SysUtils;

const
  BP_GROUPBOX = 4;
  STAP_ALLOW_CONTROLS = (1 shl 1);
  WM_THEMECHANGED = $031A;

type
  // These message records are not declared in Delphi 6 and lower.
  TWMPrint = packed record
    Msg: Cardinal;
    DC: HDC;
    Flags: Cardinal;
    Result: Integer;
  end;

  TWMPrintClient = TWMPrint;

  { TXPTheme }

constructor TXPTheme.Create;
begin
  FThemeLib := LoadLibrary('uxtheme.dll');
  if FThemeLib > 0 then
  begin
    FDrawThemeBackground := GetProcAddress(FThemeLib, 'DrawThemeBackground');
    FOpenThemeData := GetProcAddress(FThemeLib, 'OpenThemeData');
    FCloseThemeData := GetProcAddress(FThemeLib, 'CloseThemeData');
    FDrawThemeText := GetProcAddress(FThemeLib, 'DrawThemeText');
    FGetThemeTextExtent := GetProcAddress(FThemeLib, 'GetThemeTextExtent');
    FIsThemeBackgroundPartiallyTransparent := GetProcAddress(FThemeLib, 'IsThemeBackgroundPartiallyTransparent');
    FDrawThemeParentBackground := GetProcAddress(FThemeLib, 'DrawThemeParentBackground');
    FGetThemeAppProperties := GetProcAddress(FThemeLib, 'GetThemeAppProperties');
    FIsThemeActive := GetProcAddress(FThemeLib, 'IsThemeActive');
    FIsAppThemed := GetProcAddress(FThemeLib, 'IsAppThemed');
    FWindowHandle := Classes.AllocateHWnd(WndProc);
  end
    else
  begin
    FWindowHandle := 0;
  end;

  UpdateThemesActive;
end;

destructor TXPTheme.Destroy;
begin
  FreeLibrary(FThemeLib);
  FThemeLib := 0;
  Classes.DeallocateHWnd(FWindowHandle);
end;

procedure TXPTheme.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg = WM_THEMECHANGED then
    begin
      UpdateThemesActive;
    end
      else
    Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;

function TXPTheme.XPComCtl: Boolean;
begin
  Result := (GetComCtlVersion >= $00060000);
end;

procedure TXPTheme.UpdateThemesActive;
begin
  FThemesActive :=
    (FThemeLib > 0) and (GetComCtlVersion >= $00060000) and
    FIsAppThemed and FIsThemeActive and
    ((FGetThemeAppProperties and STAP_ALLOW_CONTROLS) <> 0);
end;

procedure TXPTheme.ShowFocus(Form: TCustomForm);
begin
  if (Win32MajorVersion >= 5) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    PostMessage(Form.Handle, WM_CHANGEUISTATE, MakeLong(UIS_CLEAR, UISF_HIDEFOCUS), 0);
end;

procedure TXPTheme.ShowAccelerators(Form: TCustomForm);
begin
  if (Win32MajorVersion >= 5) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    PostMessage(Form.Handle, WM_CHANGEUISTATE, MakeLong(UIS_CLEAR, UISF_HIDEACCEL), 0);
end;

  { TXPGroupBox }

constructor TXPGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FButtonThemeData := 0;
end;

destructor TXPGroupBox.Destroy;
begin
  if XPTheme.FThemesActive and (FButtonThemeData <> 0) then
  begin
    XPTheme.FCloseThemeData(FButtonThemeData);
  end;
  inherited;
end;

function TXPGroupBox.GetButtonThemeData: THandle;
begin
  if XPTheme.FThemesActive and (FButtonThemeData = 0) then
  begin
    FButtonThemeData := XPTheme.FOpenThemeData(Handle, 'button');
  end;
  Result := FButtonThemeData;
end;

procedure TXPGroupBox.WndProc(var Message: TMessage);

  procedure DoPaint(DC: HDC);
  var
    PrevFont: HFONT;
    TextR: TRect;
    R: TRect;
    WText: WideString;
    StateID: Integer;
  begin
    if Enabled then StateID := 1
      else StateID := 2;
    PrevFont := SelectObject(DC, Font.Handle);
    if Text <> '' then
    begin
      SetTextColor(DC, Graphics.ColorToRGB(Font.Color));
      WText := Text;
      XPTheme.FGetThemeTextExtent(GetButtonThemeData, DC, BP_GROUPBOX, StateID,
        PWideChar(WText), Length(WText), DT_LEFT, nil, TextR);
      if not UseRightToLeftAlignment then
        OffsetRect(TextR, 8, 0)
      else
        OffsetRect(TextR, Width - 8 - TextR.Right, 0);
    end
      else
    begin
      TextR := Rect(0, 0, 0, 0);
    end;

    R := ClientRect;
    R.Top := (TextR.Bottom - TextR.Top) div 2;
    with TextR do
      ExcludeClipRect(DC, Left, Top, Right, Bottom);

    XPTheme.FDrawThemeBackground(GetButtonThemeData, DC, BP_GROUPBOX, StateID, R, nil);

    SelectClipRgn(DC, 0);
    if Text <> '' then
    begin
      XPTheme.FDrawThemeText(GetButtonThemeData, DC, BP_GROUPBOX, StateID,
        PWideChar(WText), Length(WText), DT_LEFT, 0, TextR);
    end;
    SelectObject(DC, PrevFont);
  end;

var
  PaintS: TPaintStruct;
  WindOrig: TPoint;
begin
  if Message.Msg = WM_THEMECHANGED then
  begin
    Invalidate;
    inherited;
  end
    else
  if XPTheme.FThemesActive then
    case Message.Msg of
      WM_PAINT:
        begin
          BeginPaint(Handle, PaintS);
          DoPaint(PaintS.hdc);
          PaintControls(PaintS.hdc, nil);
          EndPaint(Handle, PaintS);
          Message.Result := 0;
        end;
      WM_ERASEBKGND:
        with TWMEraseBkGnd(Message) do
        begin
          if Parent.DoubleBuffered then
          begin
            GetWindowOrgEx(DC, WindOrig);
            SetWindowOrgEx(DC, WindOrig.X + Left, WindOrig.Y + Top, nil);
            Parent.Perform(WM_ERASEBKGND, Integer(DC), Integer(DC));
            SetWindowOrgEx(DC, WindOrig.X, WindOrig.Y, nil);
          end
            else
          begin
            if XPTheme.FIsThemeBackgroundPartiallyTransparent(GetButtonThemeData, BP_GROUPBOX, 1) then
              XPTheme.FDrawThemeParentBackground(Handle, DC, nil);
          end;
          Result := 1;
        end;
      WM_SYSKEYDOWN, CN_KEYDOWN, WM_KEYDOWN:
        begin
          UpdateAccel(TWMKey(Message).CharCode);
          inherited;
        end;
      else
        inherited;
    end
  else
    inherited;
end;

procedure TXPGroupBox.UpdateAccel(CharCode: Word);
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Assigned(Form) then
    case CharCode of
      VK_LEFT..VK_DOWN,
      VK_TAB:
        Form.Perform(WM_CHANGEUISTATE, MakeLong(UIS_CLEAR, UISF_HIDEFOCUS), 0);
      VK_MENU:
        Form.Perform(WM_CHANGEUISTATE, MakeLong(UIS_CLEAR, UISF_HIDEACCEL), 0);
    end;
end;

type
  TPublicControl = class(TWinControl)
  end;

procedure CallControlWndProc(Control: TWinControl; var Message: TMessage);
begin
  TPublicControl(Control).WndProc(Message);
end;

procedure XPTabSheetWindowProc(Data: Pointer; var Message: TMessage);
var
  TabSheet: TTabSheet;
  DrawRect: TRect;
  DC: HDC;
begin
  TabSheet := TTabSheet(Data);

  case Message.Msg of
    // Paint the border (and erase the background)
    WM_NCPAINT:
      with TabSheet do
      begin
        DC := GetWindowDC(Handle);
        try
          // Exclude the client area from painting. We only want to erase the non-client area.
          DrawRect := ClientRect;
          OffsetRect(DrawRect, BorderWidth, BorderWidth);
          with DrawRect do
            ExcludeClipRect(DC, Left, Top, Right, Bottom);
          // The parent paints relative to the control's client area. We have to compensate for this by
          // shifting the dc's window origin.
          SetWindowOrgEx(DC, -BorderWidth, -BorderWidth, nil);
          XPTheme.FDrawThemeParentBackground(TabSheet.Handle, DC, nil);
        finally
          ReleaseDC(Handle, DC);
        end;
        Message.Result := 0;
      end;
    WM_PRINTCLIENT,
    WM_ERASEBKGND:
      begin
        if Message.Msg = WM_PRINTCLIENT then
          DC := TWMPrintClient(Message).DC
        else
          DC := TWMEraseBkGnd(Message).DC;
        XPTheme.FDrawThemeParentBackground(TabSheet.Handle, DC, nil);
        Message.Result := 1;
      end;
    else
      CallControlWndProc(TabSheet, Message);
  end;
end;

procedure XPPageControl(PageControl: TPageControl);
var
  WindowProc: TWndMethod;
  I: Integer;
begin
  TMethod(WindowProc).Code := @XPTabSheetWindowProc;

  for I := 0 to PageControl.PageCount - 1 do
  begin
    TMethod(WindowProc).Data := PageControl.Pages[I];
    PageControl.Pages[I].WindowProc := WindowProc;
  end;
end;

procedure Register;
begin
  RegisterComponents('Martin', [TXPGroupBox]);
end;

initialization
  XPTheme := TXPTheme.Create;
finalization
  FreeAndNil(XPTheme);
end.
