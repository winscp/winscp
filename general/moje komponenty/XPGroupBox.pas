unit XPGroupBox;

interface

{$J+}

uses
  StdCtrls, Messages, Windows, Classes;

type
	TXPGroupBox = class(TGroupBox)
  private
    FButtonThemeData: THandle;
    FThemesActive: Boolean;
    function GetButtonThemeData: THandle;
	protected
		procedure WndProc(var Message: TMessage); override;
    procedure UpdateAccel(CharCode: Word);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
	end;

procedure Register;

implementation

uses
  Graphics, Types, Controls, Forms, ComCtrls;

const
  BP_GROUPBOX = 4;
  STAP_ALLOW_CONTROLS = (1 shl 1);
  WM_THEMECHANGED = $031A;

const
  ThemeLib: THandle = 0;

var
  DrawThemeBackground: function(hTheme: THandle; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
    pClipRect: PRECT): HRESULT; stdcall;
  OpenThemeData: function(hwnd: HWND; pszClassList: LPCWSTR): THandle; stdcall;
  CloseThemeData: function(hTheme: THandle): HRESULT; stdcall;
  DrawThemeText: function(hTheme: THandle; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR; iCharCount: Integer;
    dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall;
  IsThemeBackgroundPartiallyTransparent: function(hTheme: THandle; iPartId, iStateId: Integer): BOOL; stdcall;
  DrawThemeParentBackground: function(hwnd: HWND; hdc: HDC; prc: PRECT): HRESULT; stdcall;
  GetThemeAppProperties: function: DWORD; stdcall;
  IsAppThemed: function: BOOL; stdcall;
  IsThemeActive: function: BOOL; stdcall;

function ThemesActive: Boolean;
begin
  Result :=
    (ThemeLib > 0) and (GetComCtlVersion >= $00060000) and
    IsAppThemed and IsThemeActive and
    ((GetThemeAppProperties and STAP_ALLOW_CONTROLS) <> 0);
end;

constructor TXPGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FThemesActive := ThemesActive;

  FButtonThemeData := 0;
end;

destructor TXPGroupBox.Destroy;
begin
  if FThemesActive and (FButtonThemeData <> 0) then
  begin
    CloseThemeData(FButtonThemeData);
  end;
  inherited;
end;

function TXPGroupBox.GetButtonThemeData: THandle;
begin
  if FThemesActive and (FButtonThemeData = 0) then
  begin
    FButtonThemeData := OpenThemeData(Handle, 'button');
  end;
  Result := FButtonThemeData;
end;

procedure TXPGroupBox.WndProc(var Message: TMessage);

  procedure DoPaint(DC: HDC);
  var
    PrevFont: HFONT;
    TextR: TRect;
    R: TRect;
    Size: TSize;
    WText: WideString;
    StateID: Integer;
  begin
    PrevFont := SelectObject(DC, Font.Handle);
    if Text <> '' then
    begin
      SetTextColor(DC, Graphics.ColorToRGB(Font.Color));
      GetTextExtentPoint32(DC, PChar(Text), Length(Text), Size);
      TextR := Rect(0, 0, Size.cx, Size.cy);
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

    if Enabled then StateID := 1
      else StateID := 2;
    DrawThemeBackground(GetButtonThemeData, DC, BP_GROUPBOX, StateID, R, nil);

    SelectClipRgn(DC, 0);
    if Text <> '' then
    begin
      WText := Text;
      DrawThemeText(GetButtonThemeData, DC, BP_GROUPBOX, StateID,
        PWideChar(WText), Length(WText), DT_LEFT, 0, TextR);
    end;
    SelectObject(DC, PrevFont);
  end;

var
  PaintS: TPaintStruct;
  WindOrig: TPoint;
begin
  if FThemesActive then
    case Message.Msg of
      WM_PAINT:
        begin
          BeginPaint(Handle, PaintS);
          DoPaint(PaintS.hdc);
          PaintControls(PaintS.hdc, nil);
          EndPaint(Handle, PaintS);
          Message.Result := 0;
        end;
      WM_THEMECHANGED:
        begin
          FThemesActive := ThemesActive;
          Invalidate;
          inherited;
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
            if IsThemeBackgroundPartiallyTransparent(GetButtonThemeData, BP_GROUPBOX, 1) then
              DrawThemeParentBackground(Handle, DC, nil);
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

procedure Register;
begin
  RegisterComponents('Martin', [TXPGroupBox]);
end;

initialization
  ThemeLib := LoadLibrary('uxtheme.dll');
  if ThemeLib > 0 then
  begin
    DrawThemeBackground := GetProcAddress(ThemeLib, 'DrawThemeBackground');
    OpenThemeData := GetProcAddress(ThemeLib, 'OpenThemeData');
    CloseThemeData := GetProcAddress(ThemeLib, 'CloseThemeData');
    DrawThemeText := GetProcAddress(ThemeLib, 'DrawThemeText');
    IsThemeBackgroundPartiallyTransparent := GetProcAddress(ThemeLib, 'IsThemeBackgroundPartiallyTransparent');
    DrawThemeParentBackground := GetProcAddress(ThemeLib, 'DrawThemeParentBackground');
    GetThemeAppProperties := GetProcAddress(ThemeLib, 'GetThemeAppProperties');
    IsThemeActive := GetProcAddress(ThemeLib, 'IsThemeActive');
    IsAppThemed := GetProcAddress(ThemeLib, 'IsAppThemed');
  end;
finalization
  FreeLibrary(ThemeLib);
  ThemeLib := 0;
  DrawThemeBackground := nil;
  OpenThemeData := nil;
  CloseThemeData := nil;
  DrawThemeText := nil;
  IsThemeBackgroundPartiallyTransparent := nil;
  DrawThemeParentBackground := nil;
  GetThemeAppProperties := nil;
  IsThemeActive := nil;
  IsAppThemed := nil;
end.
