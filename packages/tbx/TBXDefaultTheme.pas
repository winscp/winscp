unit TBXDefaultTheme;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// Id: TBXDefaultTheme.pas 16 2004-05-26 02:02:55Z Alex@ZEISS

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Messages, Graphics, TBXThemes, ImgList;

type
  TTBXDefaultTheme = class(TTBXTheme)
  private
    procedure TBXSysCommand(var Message: TMessage); message TBX_SYSCOMMAND;
  protected
    ToolbarColor: TColor;
    ToolbarText: TColor;
    DockPanelColor: TColor;
    StatusBarColor: TColor;
    procedure SetupColorCache; virtual;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    { Metrics Access, etc. }
    function  GetBooleanMetrics(Index: Integer): Boolean; override;
    function  GetImageOffset(Canvas: TCanvas; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList): TPoint; override;
    function  GetIntegerMetrics(Index: Integer): Integer; override;
    procedure GetMargins(MarginID: Integer; out Margins: TTBXMargins); override;

    function  GetItemColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetPopupShadowType: Integer; override;
    procedure GetViewBorder(ViewType: Integer; out Border: TPoint); override;
    function  GetViewColor(ViewType: Integer): TColor; override;
    procedure GetViewMargins(ViewType: Integer; out Margins: TTBXMargins); override;

    { Painting routines }
    procedure PaintBackgnd(Canvas: TCanvas; const ADockRect, ARect, AClipRect: TRect; AColor: TColor; Transparent: Boolean; AViewType: Integer); override;
    procedure PaintButton(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintCaption(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo; const ACaption: string; AFormat: Cardinal; Rotated: Boolean); override;
    procedure PaintCheckMark(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintChevron(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintDock(Canvas: TCanvas; const ClientRect, DockRect: TRect; DockPosition: Integer); override;
    procedure PaintDockPanelNCArea(Canvas: TCanvas; R: TRect; const DockPanelInfo: TTBXDockPanelInfo); override;
    procedure PaintDropDownArrow(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintEditButton(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo); override;
    procedure PaintEditFrame(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo); override;
    procedure PaintFloatingBorder(Canvas: TCanvas; const ARect: TRect; const WindowInfo: TTBXWindowInfo); override;
    procedure PaintFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintImage(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer); override;
    procedure PaintMDIButton(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal); override;
    procedure PaintMenuItem(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo); override;
    procedure PaintMenuItemFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintPageScrollButton(Canvas: TCanvas; const ARect: TRect; ButtonType: Integer; Hot: Boolean); override;
    procedure PaintPopupNCArea(Canvas: TCanvas; R: TRect; const PopupInfo: TTBXPopupInfo); override;
    procedure PaintSeparator(Canvas: TCanvas; ARect: TRect; ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean); override;
    procedure PaintToolbarNCArea(Canvas: TCanvas; R: TRect; const ToolbarInfo: TTBXToolbarInfo); override;
    procedure PaintFrameControl(Canvas: TCanvas; R: TRect; Kind, State: Integer; Params: Pointer); override;
    procedure PaintStatusBar(Canvas: TCanvas; R: TRect; Part: Integer); override;
  end;

implementation

uses
  Classes, Controls, CommCtrl, TBXUtils, TBXUxThemes, TB2Common, TB2Item, TBX, Forms;

var
  SmCaptionFont: TFont;

procedure InitializeStock;
var
  NonClientMetrics: TNonClientMetrics;
begin
  SmCaptionFont := TFont.Create;
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    SmCaptionFont.Handle := CreateFontIndirect(NonClientMetrics.lfSmCaptionFont);
end;

procedure FinalizeStock;
begin
  SmCaptionFont.Free;
  SmCaptionFont := nil;
end;

procedure DrawButtonBitmap(Canvas: TCanvas; R: TRect);
const
  Pattern: array [0..15] of Byte = ($C6, 0, $6C, 0, $38, 0, $38, 0, $6C, 0, $C6, 0, 0, 0, 0, 0);
begin
  DrawGlyph(Canvas.Handle, R, 7, 6, Pattern[0], clBtnText);
end;

{ TTBXDefaultTheme }

constructor TTBXDefaultTheme.Create;
begin
  inherited;
  AddTBXSysChangeNotification(Self);
  SetupColorCache;
end;

destructor TTBXDefaultTheme.Destroy;
begin
  RemoveTBXSysChangeNotification(Self);
  inherited;
end;

function TTBXDefaultTheme.GetBooleanMetrics(Index: Integer): Boolean;
begin
  case Index of
    TMB_OFFICEXPPOPUPALIGNMENT:    Result := False;
    TMB_EDITMENUFULLSELECT:        Result := False;
    TMB_EDITHEIGHTEVEN:            Result := False;
    TMB_PAINTDOCKBACKGROUND:       Result := USE_THEMES;
    TMB_SOLIDTOOLBARNCAREA:        Result := False;
    TMB_SOLIDTOOLBARCLIENTAREA:    Result := False;
  else
    Result := False;
  end;
end;

function TTBXDefaultTheme.GetIntegerMetrics(Index: Integer): Integer;
const
  DEFAULT = -1;
var
  Sz: TSize;
begin
  case Index of
    TMI_SPLITBTN_ARROWWIDTH:
      if USE_THEMES then
      begin
        if GetThemePartSize(TOOLBAR_THEME, StockCompatibleBitmap.Canvas.Handle,
          TP_SPLITBUTTONDROPDOWN, TS_NORMAL, nil, TS_TRUE, Sz) = S_OK then
        begin
          Result := Sz.cx + 2;
        end
        else Result := 11;
      end
      else Result := 11;

    TMI_DROPDOWN_ARROWWIDTH:         Result := 8;
    TMI_DROPDOWN_ARROWMARGIN:        Result := 3;

    TMI_MENU_IMGTEXTSPACE:           Result := 1;
    TMI_MENU_LCAPTIONMARGIN:         Result := 2;
    TMI_MENU_RCAPTIONMARGIN:         Result := 2;
    TMI_MENU_SEPARATORSIZE:          Result := DEFAULT;
    TMI_MENU_MDI_DW:                 Result := 2;
    TMI_MENU_MDI_DH:                 Result := 4;

    TMI_TLBR_SEPARATORSIZE:          Result := 6;

    TMI_EDIT_FRAMEWIDTH:             Result := 2;
    TMI_EDIT_TEXTMARGINHORZ:         Result := 2;
    TMI_EDIT_TEXTMARGINVERT:         Result := 1;
    TMI_EDIT_BTNWIDTH:               Result := 13;
    TMI_EDIT_MENURIGHTINDENT:        Result := 0;
  else
    Result := DEFAULT;
  end;
end;

function TTBXDefaultTheme.GetViewColor(ViewType: Integer): TColor;
begin
  Result := ToolbarColor;
  if (ViewType and VT_TOOLBAR) = VT_TOOLBAR then Result := ToolbarColor
  else if (ViewType and VT_POPUP) = VT_POPUP then
  begin
    if (ViewType and PVT_POPUPMENU) = PVT_POPUPMENU then Result := clPopup
    else if (ViewType and PVT_LISTBOX) = PVT_LISTBOX then Result := clWindow
    else if (ViewType and PVT_TOOLBOX) = PVT_TOOLBOX then Result := ToolbarColor
    else if (ViewType and PVT_CHEVRONMENU) = PVT_CHEVRONMENU then Result := clPopup;
  end
  else if (ViewType and VT_DOCKPANEL) = VT_DOCKPANEL then Result := DockPanelColor
  else if (ViewType and VT_STATUSBAR) = VT_STATUSBAR then Result := StatusBarColor
  else Result := clBtnFace;
end;

function TTBXDefaultTheme.GetItemColor(const ItemInfo: TTBXItemInfo): TColor;
var
  IsMenuItem: Boolean;
begin
  with ItemInfo do
  begin
    IsMenuItem := ((ViewType and PVT_POPUPMENU) = PVT_POPUPMENU) and ((ItemOptions and IO_TOOLBARSTYLE) = 0);
    if not USE_THEMES then
    begin
      if IsMenuItem and (ItemInfo.HoverKind <> hkNone) then Result := clHighlight
      else Result := GetViewColor(ItemInfo.ViewType);
    end
    else
      Result := GetViewColor(ItemInfo.ViewType);
  end;
end;

function TTBXDefaultTheme.GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor;
var
  InMenuBar, ToolbarStyle, ShowInactive: Boolean;
begin
  Result := clBtnText;
  with ItemInfo do
  begin
    InMenuBar := (ViewType and TVT_MENUBAR) = TVT_MENUBAR;
    ToolbarStyle := Boolean(ItemOptions and IO_TOOLBARSTYLE);
    ShowInactive := InMenubar and not Boolean(ItemOptions and IO_APPACTIVE);

    if not ToolbarStyle and not Enabled and (HoverKind = hkKeyboardHover) then Result := clGrayText
    else if Enabled then
    begin
      if not ToolbarStyle or (InMenuBar and USE_FLATMENUS) then
      begin
        if HoverKind <> hkNone then Result := clHighlightText
        else if ShowInactive then Result := clGrayText
        else Result := clPopupText
      end
      else if ShowInactive then Result := clGrayText;
    end
    else Result := clGrayText;
  end;
end;

function TTBXDefaultTheme.GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor;
var
  IsFlatMenuItem, InFlatMenuBar, InFlatChevronBar: Boolean;
begin
  with ItemInfo do
  begin
    InFlatMenuBar := ((ViewType and TVT_MENUBAR) = TVT_MENUBAR) and USE_FLATMENUS;
    InFlatChevronBar := ((ViewType and PVT_CHEVRONMENU) = PVT_CHEVRONMENU) and USE_FLATMENUS;
    IsFlatMenuItem := ((ViewType and PVT_POPUPMENU) = PVT_POPUPMENU) and ((ItemOptions and IO_TOOLBARSTYLE) = 0) and USE_FLATMENUS;

    if InFlatMenuBar and (HoverKind <> hkNone) then Result := clHighlight
    else if InFlatChevronBar or IsFlatMenuItem and (HoverKind <> hkNone) then Result := ToolbarColor
    else Result := GetViewColor(ViewType);
  end;
end;

procedure TTBXDefaultTheme.GetViewBorder(ViewType: Integer; out Border: TPoint);
const
  XMetrics: array [Boolean] of Integer = (SM_CXDLGFRAME, SM_CXFRAME);
  YMetrics: array [Boolean] of Integer = (SM_CYDLGFRAME, SM_CYFRAME);
var
  Resizable: Boolean;
  Sz: Integer;
begin
  Sz := 0;
  if (ViewType and VT_TOOLBAR) = VT_TOOLBAR then
  begin
    if (ViewType and TVT_FLOATING) = TVT_FLOATING then
    begin
      Resizable := (ViewType and TVT_RESIZABLE) = TVT_RESIZABLE;
      Border.X := GetSystemMetrics(XMetrics[Resizable]);
      Border.Y := GetSystemMetrics(YMetrics[Resizable]);
      Exit;
    end
    else Sz := 2;
  end
  else if (ViewType and VT_POPUP) = VT_POPUP then
  begin
{$IFNDEF OFFICE2K_COMBOS}
    if (ViewType and PVT_LISTBOX) = PVT_LISTBOX then Sz := 1
    else Sz := 3;
{$ELSE}
    Sz := 3;
{$ENDIF}
  end
  else if (ViewType and VT_DOCKPANEL) = VT_DOCKPANEL then
  begin
    if (ViewType and DPVT_FLOATING) = DPVT_FLOATING then
    begin
      Resizable := (ViewType and DPVT_RESIZABLE) = DPVT_RESIZABLE;
      Border.X := GetSystemMetrics(XMetrics[Resizable]);
      Border.Y := GetSystemMetrics(YMetrics[Resizable]);
      Exit;
    end
    else Sz := 2;
  end;
  Border.X := Sz;
  Border.Y := Sz;
end;

procedure TTBXDefaultTheme.GetMargins(MarginID: Integer; out Margins: TTBXMargins);
var
  R, R2: TRect;
begin
  with Margins do
    case MarginID of
      MID_TOOLBARITEM:
        begin
          LeftWidth := 2; RightWidth := 2;
          TopHeight := 2; BottomHeight := 2;
          if USE_THEMES then
            GetThemeMargins(TOOLBAR_THEME, StockBitmap1.Canvas.Handle, TP_BUTTON, TS_HOT, TMT_CAPTIONMARGINS,
              nil, TMargins(Margins));
        end;

      MID_MENUITEM:
        begin
          LeftWidth := 0; RightWidth := 0;
          TopHeight := 2; BottomHeight := 2;
        end;

      MID_STATUSPANE:
        begin
          if USE_THEMES then
          begin
            R := Rect(0, 0, 100, 100);
            GetThemeBackgroundContentRect(STATUSBAR_THEME, StockBitmap1.Canvas.Handle, SP_PANE, 0, R, @R2);
            LeftWidth := R2.Left - R.Left;
            RightWidth := R.Right - R2.Right;
            TopHeight := R2.Top - R.Top;
            BottomHeight := R.Bottom - R2.Bottom;
          end
          else
          begin
            LeftWidth := 1; RightWidth := 3;
            TopHeight := 1; BottomHeight := 1;
          end;
        end;
    else
      LeftWidth := 0; RightWidth := 0;
      TopHeight := 0; BottomHeight := 0;
    end;
end;

procedure TTBXDefaultTheme.PaintBackgnd(Canvas: TCanvas; const ADockRect, ARect, AClipRect: TRect;
  AColor: TColor; Transparent: Boolean; AViewType: Integer);
var
  DC: HDC;
  R: TRect;
begin
  DC := Canvas.Handle;
  if not Transparent then
  begin
    IntersectRect(R, ARect, AClipRect);
    FillRectEx(DC, R, AColor);
  end;
end;

procedure TTBXDefaultTheme.PaintCaption(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo; const ACaption: string;
  AFormat: Cardinal; Rotated: Boolean);
var
  R: TRect;
  C: TColor;
  InMenuBar, ToolbarStyle: Boolean;

  procedure _Draw(Color: TColor);
  begin
    Canvas.Font.Color := Color;
    if not Rotated then Windows.DrawText(Canvas.Handle, PChar(ACaption), Length(ACaption), R, AFormat)
    else DrawRotatedText(Canvas.Handle, ACaption, R, AFormat);
  end;

begin
  with ItemInfo, Canvas do
  begin
    R := ARect;
    C := Font.Color;
    { Apply theme-dependent color only when Font.Color = clNone }
    if C = clNone then C := GetItemTextColor(ItemInfo);
    Brush.Style := bsClear;
    InMenuBar := (ViewType and TVT_MENUBAR) = TVT_MENUBAR;
    ToolbarStyle := Boolean(ItemOptions and IO_TOOLBARSTYLE);
    if not ToolbarStyle and not Enabled and (HoverKind = hkKeyboardHover) then _Draw(C)
    else if Enabled then
    begin
      if ToolbarStyle and (Pushed or Selected) and not (InMenuBar and USE_FLATMENUS) then
        OffsetRect(R, 1, 1);
      _Draw(C);
    end
    else if USE_THEMES then _Draw(C)
    else
    begin
      OffsetRect(R, 1, 1);
      _Draw(clBtnHighlight);
      OffsetRect(R, -1, -1);
      _Draw(clBtnShadow);
    end;
    Brush.Style := bsSolid;
  end;
end;

procedure TTBXDefaultTheme.PaintCheckMark(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  DC: HDC;
  X, Y: Integer;
  C: TColor;
begin
  DC := Canvas.Handle;
  X := (ARect.Left + ARect.Right) div 2 - 1;
  Y := (ARect.Top + ARect.Bottom) div 2 + 1;
  if ItemInfo.Enabled
    then C := clBtnText
    else C := clGrayText;
  if ItemInfo.ItemOptions and IO_RADIO > 0 then
  begin
    RoundRectEx(DC, X-2, Y-4, X+4, Y+2, 2, 2,
      MixColors(C, clBtnHighlight, 128), clNone);
    RoundRectEx(DC, X-2, Y-4, X+4, Y+2, 6, 6, C, C);
    if ItemInfo.Enabled then
      RoundRectEx(DC, X-3, Y-5, X+5, Y+3, 6, 6, clBtnHighlight, clNone);
  end
  else begin
    PolyLineEx(DC, [Point(X-2, Y-2), Point(X, Y), Point(X+4, Y-4),
      Point(X+4, Y-3), Point(X, Y+1), Point(X-2, Y-1), Point(X-2, Y-2)], C);
    if ItemInfo.Enabled then
      PolyLineEx(DC, [Point(X-3, Y-2), Point(X-3, Y-1), Point(X, Y+2),
        Point(X+5, Y-3), Point(X+5, Y-5)], clBtnHighlight);
  end;
end;

procedure TTBXDefaultTheme.PaintChevron(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo);
const
  Pattern: array [Boolean, 0..15] of Byte = (
    ($CC, 0, $66, 0, $33, 0, $66, 0, $CC, 0, 0, 0, 0, 0, 0, 0),
    ($88, 0, $D8, 0, $70, 0, $20, 0, $88, 0, $D8, 0, $70, 0, $20, 0));
var
  DC: HDC;
  R2: TRect;
  P: PByte;
  W, H: Integer;
begin
  DC := Canvas.Handle;
  R2 := ARect;
  PaintButton(Canvas, R2, ItemInfo);

  if not ItemInfo.IsVertical then
  begin
    Inc(R2.Top, 4);
    R2.Bottom := R2.Top + 5;
    W := 8;
    H := 5;
  end
  else
  begin
    R2.Left := R2.Right - 9;
    R2.Right := R2.Left + 5;
    W := 5;
    H := 8;
  end;
  if ItemInfo.Pushed then OffsetRect(R2, 1, 1);

  P := @Pattern[ItemInfo.IsVertical][0];
  if ItemInfo.Enabled then DrawGlyph(DC, R2, W, H, P^, clBtnText)
  else
  begin
    OffsetRect(R2, 1, 1);
    DrawGlyph(DC, R2, W, H, P^, clBtnHighlight);
    OffsetRect(R2, -1, -1);
    DrawGlyph(DC, R2, W, H, P^, clBtnShadow);
  end;
end;

procedure TTBXDefaultTheme.PaintEditButton(Canvas: TCanvas; const ARect: TRect;
  var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo);
var
  DC: HDC;
  BtnDisabled, BtnHot, BtnPressed, Embedded: Boolean;
  StateFlags: Integer;
  R, BR: TRect;
  C: TColor;
  X, Y: Integer;

  procedure DrawEnabled(var R: TRect);
  begin
    if BtnPressed then
      Windows.DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST)
    else if BtnHot then
      Windows.DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT or BF_ADJUST)
    else if not Embedded then
      FrameRectEx(DC, R, clWindow, False);
  end;

  procedure DrawUp;
  begin
    X := (R.Left + R.Right) div 2 + Ord(BtnPressed);
    Y := (R.Top * 3 + R.Bottom + 3) div 4 + Ord(BtnPressed);
    if not BtnDisabled then
    begin
      if Boolean(ItemInfo.ItemOptions and IO_TOOLBARSTYLE) then C := clPopupText
      else C := clBtnText;
    end
    else with Canvas do
    begin
      Inc(X); Inc(Y);
      Pen.Color := clBtnHighlight; Brush.Color := clBtnHighlight;
      Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y - 2)]);
      Dec(X); Dec(Y);
      C := clBtnShadow;
    end;
    Canvas.Pen.Color := C; Canvas.Brush.Color := C;
    Canvas.Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y - 2)])
  end;

  procedure DrawDn;
  begin
    X := (R.Left + R.Right) div 2 + Ord(BtnPressed);
    Y := (R.Top + R.Bottom * 3 - 4) div 4 + Ord(BtnPressed);
    if not BtnDisabled then
    begin
      if Boolean(ItemInfo.ItemOptions and IO_TOOLBARSTYLE) then C := clPopupText
      else C := clBtnText;
    end
    else with Canvas do
    begin
      Inc(X); Inc(Y);
      Pen.Color := clBtnHighlight; Brush.Color := clBtnHighlight;
      Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)]);
      Dec(X); Dec(Y);
      C := clBtnShadow;
    end;
    Canvas.Pen.Color := C; Canvas.Brush.Color := C;
    Canvas.Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)])
  end;

begin
  DC := Canvas.Handle;
  R := ARect;
  with Canvas, ItemInfo do
  begin
    Embedded := ((ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
      ((ViewType and TVT_EMBEDDED) = TVT_EMBEDDED);

    if ButtonInfo.ButtonType = EBT_DROPDOWN then
    begin
      { DropDown button }
      BtnDisabled := (ButtonInfo.ButtonState and EBDS_DISABLED) <> 0;
      BtnHot := (ButtonInfo.ButtonState and EBDS_HOT) <> 0;
      BtnPressed := (ButtonInfo.ButtonState and EBDS_PRESSED) <> 0;
      if USE_THEMES then
      begin
        if BtnDisabled then StateFlags := CBXS_DISABLED
        else if BtnPressed then StateFlags := CBXS_PRESSED
        else if BtnHot then StateFlags := CBXS_HOT
        else StateFlags := CBXS_NORMAL;
        if BtnHot then InflateRect(R, 1, 1);
        DrawThemeBackground(COMBO_THEME, Handle, CP_DROPDOWNBUTTON, StateFlags, R, nil);
      end
      else
      begin
        Inc(R.Left, 2);
        if not BtnDisabled then with R do
        begin
          if Embedded then FillRectEx(DC, R, clBtnFace);
          if BtnPressed or BtnHot then
            DrawLineEx(DC, Left - 1, Top, Left - 1, Bottom + 1, ToolbarColor)
          else if Embedded then
            DrawLineEx(DC, Left - 1, Top, Left - 1, Bottom, clBtnShadow)
          else
            DrawLineEx(DC, Left - 1, Top, Left - 1, Bottom, clWindow);
          DrawEnabled(R);
        end;
        PaintDropDownArrow(Canvas, R, ItemInfo);
      end;
    end
    else if ButtonInfo.ButtonType = EBT_SPIN then
    begin
      { Paint spin buttons }
      BtnDisabled := (ButtonInfo.ButtonState and EBSS_DISABLED) <> 0;
      BtnHot := (ButtonInfo.ButtonState and EBSS_HOT) <> 0;
      if USE_THEMES then
      begin
        if BtnHot then InflateRect(R, 1, 1);

        { Upper with XP themes }
        BR := R;
        BR.Bottom := (R.Top + R.Bottom - 1) div 2;
        BtnPressed := (ButtonInfo.ButtonState and EBSS_UP) <> 0;
        if BtnDisabled then StateFlags := UPS_DISABLED
        else if BtnPressed then StateFlags := UPS_PRESSED
        else if BtnHot then StateFlags := UPS_HOT
        else StateFlags := UPS_NORMAL;
        DrawThemeBackground(SPIN_THEME, Handle, SPNP_UP, StateFlags, BR, nil);

        { Lower with XP themes }
        BR := R;
        BR.Top := (R.Top + R.Bottom) div 2;
        BtnPressed := (ButtonInfo.ButtonState and EBSS_DOWN) <> 0;
        if BtnDisabled then StateFlags := DNS_DISABLED
        else if BtnPressed then StateFlags := DNS_PRESSED
        else if BtnHot then StateFlags := DNS_HOT
        else StateFlags := DNS_NORMAL;
        DrawThemeBackground(SPIN_THEME, Handle, SPNP_DOWN, StateFlags, BR, nil);
      end
      else
      begin
        Inc(R.Left, 2);

        if not BtnDisabled then with R do
          if BtnPressed or BtnHot then
            DrawLineEx(DC, Left - 1, Top - 1, Left - 1, Bottom + 1, ToolbarColor)
          else if Embedded then
            DrawLineEx(DC, Left - 1, Top, Left - 1, Bottom, clBtnShadow)
          else
            DrawLineEx(DC, Left - 1, Top, Left - 1, Bottom, clWindow);


        BR := R;
        BR.Bottom := (R.Top + R.Bottom + 1) div 2;
        BtnPressed := (ButtonInfo.ButtonState and EBSS_UP) <> 0;
        if BtnHot or BtnPressed then Dec(BR.Bottom);
        if not BtnDisabled then DrawEnabled(BR);
        DrawUp;

        BR := R;
        BR.Top := (R.Top + R.Bottom) div 2;
        BtnPressed := (ButtonInfo.ButtonState and EBSS_DOWN) <> 0;
        if BtnHot or BtnPressed then Inc(BR.Top);
        if not BtnDisabled then DrawEnabled(BR);
        DrawDn;
      end;
    end;
  end;
end;

procedure TTBXDefaultTheme.PaintEditFrame(Canvas: TCanvas;
  const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo);
var
  R: TRect;
  W: Integer;
begin
  R := ARect;
  PaintFrame(Canvas, R, ItemInfo);
  W := EditFrameWidth;
  InflateRect(R, -W, -W);
  with EditInfo do if RightBtnWidth > 0 then Dec(R.Right, RightBtnWidth - 2);
  Canvas.Brush.Color := clWindow;
  if ItemInfo.Enabled then Canvas.FillRect(R);
  with EditInfo do if LeftBtnWidth > 0 then Inc(R.Left, LeftBtnWidth - 2);
  if EditInfo.RightBtnWidth > 0 then
  begin
    R := ARect;
    InflateRect(R, -W, -W);
    R.Left := R.Right - EditInfo.RightBtnWidth;
    PaintEditButton(Canvas, R, ItemInfo, EditInfo.RightBtnInfo);
  end;
end;

procedure TTBXDefaultTheme.PaintDropDownArrow(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  X, Y: Integer;

  procedure Draw(AColor: TColor);
  begin
    Canvas.Pen.Color := AColor;
    Canvas.Brush.Color := AColor;
    if ItemInfo.IsVertical then Canvas.Polygon([Point(X, Y + 2), Point(X, Y - 2), Point(X - 2, Y)])
    else Canvas.Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)]);
  end;

begin
  with ItemInfo, ARect do
  begin
    X := (Left + Right) div 2;
    Y := (Top + Bottom) div 2 - 1;

    if (Pushed or Selected) and (ComboPart <> cpSplitRight) then
    begin
      Inc(X); Inc(Y);
    end;

    if Enabled then
    begin
      if Boolean(ItemOptions and IO_TOOLBARSTYLE) then Draw(clPopupText)
      else Draw(clBtnText);
    end
    else
    begin
      Inc(X); Inc(Y);
      Draw(clBtnHighlight);
      Dec(X); Dec(Y);
      Draw(clBtnShadow);
    end;
  end;
end;

procedure TTBXDefaultTheme.PaintButton(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo);
const
  XPPart: array [TTBXComboPart] of Integer = (TP_BUTTON, TP_DROPDOWNBUTTON,
    TP_SPLITBUTTON, TP_SPLITBUTTONDROPDOWN);
  Edge: array [Boolean] of Integer = (BDR_RAISEDINNER, EDGE_RAISED);
var
  R: TRect;
  Flags, RegionFlags: Cardinal;
  InMenuBar, ShowHover, Embedded, ShowFlatSL: Boolean;
  Region: HRGN;
  DC: HDC;
  Brush, OldBrush: HBrush;
begin
  R := ARect;
  DC := Canvas.Handle;
  with ItemInfo do
  begin
    ShowHover := (Enabled and (HoverKind <> hkNone)) or
      (not Enabled and (HoverKind = hkKeyboardHover));

    InMenuBar := (ViewType and TVT_MENUBAR) = TVT_MENUBAR;
    Embedded := ((ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
      ((ViewType and TVT_EMBEDDED) = TVT_EMBEDDED);

    if not InMenuBar and USE_THEMES then
    begin
      { The support for XP themes in menus is not yet implemented since standard
        XP themes seem to have no theming for menus }

      if not Enabled then
      begin
        if HoverKind = hkKeyboardHover then Flags := TS_HOT
        else Flags := TS_DISABLED;
      end
      else if ItemInfo.Pushed then Flags := TS_PRESSED
      else if ItemInfo.Selected then
      begin
        if HoverKind <> hkNone then Flags := TS_HOTCHECKED
        else Flags := TS_CHECKED;
      end
      else if HoverKind <> hkNone then Flags := TS_HOT
      else Flags := TS_NORMAL;

      if Embedded or Boolean(ItemOptions and IO_DESIGNING) then
      begin
        { There is no state for non-transparent normal toolbar button, trying to
          simulate it with regions... }
        RegionFlags := TS_HOT;
        if ComboPart = cpSplitRight then Dec(R.Left);
        GetThemeBackgroundRegion(TOOLBAR_THEME, Canvas.Handle, XPPart[ComboPart], RegionFlags, R, Region);
        if ComboPart = cpSplitRight then Inc(R.Left);
        if Embedded or not Boolean(ItemOptions and IO_DESIGNING) then
        begin
          Brush := CreateBrushEx(ToolbarColor);
          OldBrush := SelectObject(DC, Brush);
          FillRgn(DC, Region, Brush);
          SelectObject(DC, OldBrush);
          DeleteObject(Brush);
        end;
        Brush := CreateBrushEx(clBtnShadow);
        OldBrush := SelectObject(DC, Brush);
        FrameRgn(DC, Region, Brush, 1, 1);
        SelectObject(DC, OldBrush);
        DeleteObject(Brush);
        DeleteObject(Region);
      end;

      DrawThemeBackground(TOOLBAR_THEME, DC, XPPart[ComboPart], Flags, R, nil);
    end
    else
    begin
      if InMenuBar and USE_FLATMENUS then
      begin
        if ((Pushed or Selected) and Enabled) or ShowHover then
          FillRectEx(DC, R, clHighlight);
        Exit;
      end;
{$IFDEF NARROWCOMBOBUTTON}
      if (ItemInfo.ComboPart = cpSplitRight) and not (InMenuBar or USE_THEMES) then Dec(R.Right, 2);
{$ENDIF}
      if USE_FLATMENUS and (((Pushed or Selected) and Enabled) or ShowHover) then
        FillRectEx(DC, R, ToolbarColor);
      if Embedded then with Canvas do
      begin
        Flags := BF_RECT or BF_MIDDLE or BF_ADJUST;
        if not ShowHover or (Pushed or Selected or not Enabled) then Flags := Flags or BF_FLAT;
        ShowFlatSL := (ComboPart = cpSplitLeft) and not (ShowHover or Pushed);
        if ShowFlatSL then Inc(R.Right);
        Windows.DrawEdge(Handle, R, EDGE_RAISED, Flags);
        if Selected and Enabled and (HoverKind = hkNone) then
          DitherRect(DC, R, ToolbarColor, clBtnHighlight);
        if ShowFlatSL then Dec(R.Right);
      end
      else if (Pushed or Selected) and Enabled then with Canvas do
      begin
        Windows.DrawEdge(Handle, R, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
        if not Pushed and (HoverKind = hkNone) then
          DitherRect(DC, R, ToolbarColor, clBtnHighlight);
      end
      else if ShowHover or Boolean(ItemOptions and IO_DESIGNING) then
        Windows.DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT);
      if ComboPart = cpSplitRight then PaintDropDownArrow(Canvas, R, ItemInfo);
    end;
  end;
end;

procedure TTBXDefaultTheme.PaintFloatingBorder(Canvas: TCanvas; const ARect: TRect; const WindowInfo: TTBXWindowInfo);
const
  SPI_GETGRADIENTCAPTIONS = $1008;
  DC_GRADIENT = $20;
  ActiveCaptionFlags: array [Boolean] of Integer = (0, DC_ACTIVE);
  GradientCaptionFlags: array [Boolean] of Integer = (0, DC_GRADIENT);
  CaptionBkColors: array [Boolean, Boolean] of Integer =
    ((COLOR_INACTIVECAPTION, COLOR_ACTIVECAPTION),
    (COLOR_GRADIENTINACTIVECAPTION, COLOR_GRADIENTACTIVECAPTION));
  ButtonStateFlags: array [Boolean] of Integer = (0, DFCS_PUSHED);
var
  R, R2: TRect;
  DC: HDC;
  Flags: Integer;
  Gradient, ShowCloseBtn: Boolean;
  B: BOOL;
begin
  DC := Canvas.Handle;

  with WindowInfo do
    if not USE_THEMES then
    begin
      R := ARect;
      if (WRP_BORDER and RedrawPart) <> 0 then
      begin
        R2 := R;
        with FloatingBorderSize do InflateRect(R2, -X, -Y);
        SaveDC(DC);
        with R2 do ExcludeClipRect(DC, Left, Top, Right, Bottom);
        Windows.DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE);
        RestoreDC(DC, -1);
      end;

      if not WindowInfo.ShowCaption then Exit;
      Gradient := SystemParametersInfo(SPI_GETGRADIENTCAPTIONS, 0, @B, 0) and B;
      ShowCloseBtn := (CDBS_VISIBLE and CloseButtonState) <> 0;
      R := GetTBXCloseButtonRect(WindowInfo, True);

      if (WRP_CAPTION and RedrawPart) <> 0 then
      begin
        if ShowCloseBtn then
        begin
          SaveDC(DC);
          with R do ExcludeClipRect(DC, Left, Top, Right, Bottom);
        end;
        R2 := GetTBXCaptionRect(WindowInfo, True, ShowCloseBtn);
        DrawCaption(ParentHandle, DC, R2, DC_TEXT or DC_SMALLCAP or
          ActiveCaptionFlags[Active] or GradientCaptionFlags[Gradient]);
        if ShowCloseBtn then RestoreDC(DC, -1);
        R2 := GetTBXCaptionRect(WindowInfo, True, False);
        R2.Top := R2.Bottom;
        Inc(R2.Bottom);
        FillRect(DC, R2, GetSysColorBrush(COLOR_BTNFACE));
      end;

      if ShowCloseBtn then
      begin
        R2 := R;
        InflateRect(R2, -2, -2);
        if (WRP_CAPTION and RedrawPart) <> 0 then
        begin
          SaveDC(DC);
          with R2 do ExcludeClipRect(DC, Left, Top, Right, Bottom);
          FillRect(DC, R, GetSysColorBrush(CaptionBkColors[Gradient, WindowInfo.Active]));
          RestoreDC(DC, -1);
        end;
        if (WRP_CLOSEBTN and RedrawPart) <> 0 then
          DrawFrameControl(DC, R2, DFC_CAPTION, DFCS_CAPTIONCLOSE or
            ButtonStateFlags[(CDBS_PRESSED and CloseButtonState) <> 0]);
      end;
    end
    else { Use WindowsXP visual styles }
    begin
      if (WRP_BORDER and RedrawPart) <> 0 then
      begin
        if Active then Flags := FS_ACTIVE else Flags := FS_INACTIVE;
        R := ARect;
        R.Top := R.Bottom - FloatingBorderSize.Y;
        DrawThemeBackground(WINDOW_THEME, DC, WP_SMALLFRAMEBOTTOM, Flags, R, nil);
        R.Top := ARect.Top;
        R.Bottom := R.Top + FloatingBorderSize.Y;
        {if WindowInfo.ShowCaption then} { TODO : how to paint a captionless window frame }
          Inc(R.Bottom, GetSystemMetrics(SM_CYSMCAPTION));
        DrawThemeBackground(WINDOW_THEME, DC, WP_SMALLCAPTION, Flags, R, nil);
        R.Top := R.Bottom;
        R.Bottom := ARect.Bottom - FloatingBorderSize.Y;
        R.Right := R.Left + FloatingBorderSize.X;
        DrawThemeBackground(WINDOW_THEME, DC, WP_SMALLFRAMELEFT, Flags, R, nil);
        R.Right := ARect.Right;
        R.Left := R.Right - FloatingBorderSize.X;
        DrawThemeBackground(WINDOW_THEME, DC, WP_SMALLFRAMERIGHT, Flags, R, nil);
      end;

      if not ShowCaption then Exit;

      { Get the caption area }
      R := ARect;
      with FloatingBorderSize do InflateRect(R, -X, -Y);
      Dec(R.Bottom, ClientHeight);

      if (WRP_CAPTION and RedrawPart) <> 0 then
      begin
        R2 := R;
        if ((CDBS_VISIBLE and CloseButtonState) <> 0) and ((WRP_CLOSEBTN and RedrawPart) <> 0) then
          Dec(R2.Right, GetSystemMetrics(SM_CYSMCAPTION));

        Canvas.Font.Assign(SmCaptionFont);
        if Active then Canvas.Font.Color := clCaptionText
        else Canvas.Font.Color := clInactiveCaptionText;
        Canvas.Brush.Style := bsClear;

       { This is strange... the DrawThemeText function refuses to work...
         Use standard API...   }
        DrawText(Canvas.Handle, WindowInfo.Caption, -1, R2,
          DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_HIDEPREFIX);
      
        Canvas.Brush.Style := bsSolid;
      end;

      if (CDBS_VISIBLE and CloseButtonState) <> 0 then
      begin
        Dec(R.Bottom);
        R.Left := R.Right - R.Bottom + R.Top;
        InflateRect(R, -2, -2);
        if (CDBS_PRESSED and CloseButtonState) <> 0 then Flags := CBS_PUSHED
        else if (CDBS_HOT and CloseButtonState) <> 0 then Flags := CBS_HOT
        else Flags := CBS_NORMAL;
        DrawThemeBackground(WINDOW_THEME, DC, WP_SMALLCLOSEBUTTON, Flags, R, nil);
      end;
    end;
end;

procedure TTBXDefaultTheme.PaintFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  DC: HDC;
  R: TRect;
  E, Embedded: Boolean;
  Flags, Border: Integer;
  C: TColor;
begin
  R := ARect;
  DC := Canvas.Handle;
  with ItemInfo do
  begin
    E := (Enabled and (HoverKind <> hkNone)) or
      (not Enabled and (HoverKind = hkKeyboardHover));
    Embedded := ((ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
      ((ViewType and TVT_EMBEDDED) = TVT_EMBEDDED);
    if USE_THEMES then
    begin
      InflateRect(R, -1, -1);
      if Embedded then C := clBtnShadow
      else C := ToolbarColor;
      FrameRectEx(DC, R, C, False);
      InflateRect(R, 1, 1);
      if Pushed or Selected or E or ((ItemOptions and IO_DESIGNING) <> 0)
        then DrawThemeBackground(COMBO_THEME, DC, 0, 0, R, nil);
      InflateRect(R, -2, -2);
      FrameRectEx(DC, R, clWindow, False);
    end
    else
    begin
      if Embedded then
      begin
        Flags := BF_RECT;
        if not (Pushed or Selected or E) then
        begin
          InflateRect(R, -1, -1);
          Flags := Flags or BF_FLAT;
          Border := BDR_SUNKENOUTER;
        end
        else Border := EDGE_SUNKEN;
        Windows.DrawEdge(DC, R, Border, Flags);
        if (Pushed or Selected or E) then InflateRect(R, -1, -1);
      end
      else
      begin
        if Pushed or Selected or E or ((ItemOptions and IO_DESIGNING) <> 0) then
          Windows.DrawEdge(DC, R, BDR_SUNKENOUTER, BF_RECT);
        InflateRect(R, -1, -1);
        FrameRectEx(DC, R, ToolbarColor, True);
        FrameRectEx(DC, R, clWindow, False);
      end;
    end;
  end;
end;

function TTBXDefaultTheme.GetImageOffset(Canvas: TCanvas; const ItemInfo: TTBXItemInfo;
  ImageList: TCustomImageList): TPoint;
const
  Offsets: array [Boolean] of TPoint = ((X:0; Y:0), (X:1; Y:1));
begin
  with ItemInfo do
    Result := Offsets[Pushed or Selected];
end;

procedure TTBXDefaultTheme.PaintImage(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer);
var
  BgColor: TColor;
  HiContrast: Boolean;
  IsMenuItem: Boolean;
begin
  with ItemInfo do
  begin
    if ImageList is TTBCustomImageList then
    begin
      if Pushed or Selected then OffsetRect(ARect, 1, 1);
      TTBCustomImageList(ImageList).DrawState(Canvas, ARect.Left, ARect.Top,
        ImageIndex, Enabled, (HoverKind <> hkNone), Selected);
      Exit;
    end;

    IsMenuItem := ((ViewType and PVT_POPUPMENU) = PVT_POPUPMENU) and
      ((ItemOptions and IO_TOOLBARSTYLE) = 0);

    if (IsMenuItem and USE_FLATMENUS) or (not IsMenuItem and USE_THEMES) then
    begin
    { The icon painting here is not really made by the uxtheme.dll, this is
      just a simulation until I figure out how to work with DrawThemedIcon function }
      if Pushed or Selected then OffsetRect(ARect, 1, 1);
      BgColor := GetItemImageBackground(ItemInfo);
      HiContrast := not IsMenuItem and IsDarkColor(BGColor);
      if not Enabled then
        DrawTBXIconShadow(Canvas, ARect, ImageList, ImageIndex, 0)
      else if Selected or Pushed or (HoverKind <> hkNone) then
        DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast)
      else if HiContrast or TBXHiContrast or TBXLoColor then
        DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast)
      else
        HighlightTBXIcon(Canvas, ARect, ImageList, ImageIndex, clWindow, 178);
    end
    else
    begin
      if Pushed or Selected then OffsetRect(ARect, 1, 1);
      ImageList.Draw(Canvas, ARect.Left, ARect.Top, ImageIndex, Enabled);
    end;
  end;
end;

procedure TTBXDefaultTheme.PaintMDIButton(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal);
const
  PushedFlags: array[Boolean] of UINT = (0, DFCS_PUSHED);
var
  XPPart, XPFlags: Cardinal;
begin
  if USE_THEMES then
  begin
    case ButtonKind of
      DFCS_CAPTIONMIN: XPPart := WP_MDIMINBUTTON;
      DFCS_CAPTIONRESTORE: XPPart := WP_MDIRESTOREBUTTON;
      DFCS_CAPTIONCLOSE: XPPart := WP_MDICLOSEBUTTON;
    else
      XPPart := 0;
    end;
    if ItemInfo.Pushed then XPFlags := CBS_PUSHED
    else if ItemInfo.HoverKind <> hkNone then XPFlags := CBS_HOT
    else XPFlags := CBS_NORMAL;
    DrawThemeBackground(WINDOW_THEME, Canvas.Handle, XPPart, XPFLags, ARect, nil);
  end
  else
  begin
    DrawFrameControl(Canvas.Handle, ARect, DFC_CAPTION,
      ButtonKind or PushedFlags[ItemInfo.Pushed]);
  end;
end;

procedure TTBXDefaultTheme.PaintMenuItemFrame(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  DC: HDC;
begin
  DC := Canvas.Handle;
  with ItemInfo do if (Enabled and (HoverKind <> hkNone)) or
    (not Enabled and (HoverKind = hkKeyboardHover)) then
    FillRectEx(DC, ARect, clHighlight);
end;

procedure TTBXDefaultTheme.PaintMenuItem(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo);
var
  DC: HDC;
  R: TRect;
  ShowImageOrCheck: Boolean;
  ShowHover: Boolean;
  IsComboItem: Boolean;
  X, Y: Integer;
  ArrowWidth: Integer;

  procedure DrawArrow(AColor: TColor);
  begin
    PolygonEx(DC, [Point(X, Y - 3), Point(X, Y + 3), Point(X + 3, Y)], AColor, AColor);
  end;

begin
  DC := Canvas.Handle;
  with ItemInfo do
  begin
    ShowImageOrCheck := (ImageWidth > 0) or Selected;
    ShowHover := (Enabled and (HoverKind <> hkNone)) or
      (not Enabled and (HoverKind = hkKeyboardHover));
    ArrowWidth := GetSystemMetrics(SM_CXMENUCHECK);

    R := ARect;
    if ShowImageOrCheck then Inc(R.Left, ItemInfo.PopupMargin + MenuImageTextSpace);
    IsComboItem := ((ItemOptions and IO_COMBO) <> 0);
    if IsComboItem and Enabled then Dec(R.Right, ArrowWidth);

    PaintMenuItemFrame(Canvas, R, ItemInfo);

    if IsComboItem then
    begin
      R.Left := ARect.Right - ArrowWidth;
      R.Right := ARect.Right;
      if Enabled and (HoverKind <> hkNone) then
        Windows.DrawEdge(DC, R, BDR_SUNKENOUTER, BF_RECT)
      else
      begin
        Dec(R.Left);
        if not ShowHover then DrawEdge(DC, R, EDGE_ETCHED, BF_LEFT)
        else DrawEdge(DC, R, BDR_SUNKENOUTER, BF_LEFT);
      end;
    end;

    if (ItemOptions and IO_SUBMENUITEM) <> 0 then
    begin
      Y := ARect.Bottom div 2;
      X := ARect.Right - ArrowWidth * 2 div 3 - 2;
      if not Enabled then
      begin
        if HoverKind = hkKeyboardHover then DrawArrow(clBtnShadow)
        else
        begin
          Inc(X); Inc(Y);
          DrawArrow(clBtnHighlight);
          Dec(X); Dec(Y);
          DrawArrow(clBtnShadow);
        end;
      end
      else if (HoverKind <> hkNone) and not IsComboItem then DrawArrow(clHighlightText)
      else DrawArrow(clPopupText);
    end;

    if Enabled and ShowImageOrCheck and ((HoverKind <> hkNone) or Selected) then
    begin
      R.Left := ARect.Left;
      R.Right := R.Left + ItemInfo.PopupMargin;
      if USE_FLATMENUS then FillRectEx(DC, R, ToolbarColor);
      PaintButton(Canvas, R, ItemInfo);
    end;
  end;
end;

procedure TTBXDefaultTheme.PaintPopupNCArea(Canvas: TCanvas; R: TRect; const PopupInfo: TTBXPopupInfo);
var
  DC: HDC;
begin
  DC := Canvas.Handle;

{$IFNDEF OFFICE2K_COMBOS}
  if (PopupInfo.ViewType and PVT_LISTBOX) = PVT_LISTBOX then
  begin
    FrameRectEx(DC, R, clWindowFrame, True);
    FrameRectEx(DC, R, clWindow, True);
    FrameRectEx(DC, R, clWindow, False);
  end
  else
{$ENDIF}
  if USE_FLATMENUS and ((PopupInfo.ViewType and PVT_TOOLBOX) <> PVT_TOOLBOX) then
  begin
    FrameRectEx(DC, R, clBtnShadow, True);
    FrameRectEx(DC, R, clPopup, True);
    FrameRectEx(DC, R, clPopup, False);
  end
  else if (PopupInfo.ViewType and PVT_TOOLBOX) = PVT_TOOLBOX then
  begin
    Windows.DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_ADJUST);
    FrameRectEx(DC, R, ToolbarColor, False);
  end
  else
  begin
    Windows.DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_ADJUST);
    FrameRectEx(DC, R, clPopup, False);
  end;
end;

procedure TTBXDefaultTheme.PaintSeparator(Canvas: TCanvas; ARect: TRect;
  ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean);
const
  XPFlags: array [Boolean] of Integer = (TP_SEPARATOR, TP_SEPARATORVERT);
var
  D: Integer;
begin
  { Note: for blank separators, Enabled = False }
  with ItemInfo, ARect do if Enabled then
  begin
    if not USE_THEMES or ((ViewType and PVT_POPUPMENU) = PVT_POPUPMENU) then
    begin
      D := 0;
      if LineSeparator then
        if (ViewType and TVT_FLOATING) <> 0 then D := 1
        else D := 4;

      if Horizontal then
      begin
        if (ItemOptions and IO_TOOLBARSTYLE) = 0 then D := 12;
        Top := (Top + Bottom) div 2 - 1;
        Inc(Left, D); Dec(Right, D);
        Windows.DrawEdge(Canvas.Handle, ARect, EDGE_ETCHED, BF_TOP);
      end
      else
      begin
        Left := (Left + Right) div 2 - 1;
        Inc(Top, D); Dec(Bottom, D);
        Windows.DrawEdge(Canvas.Handle, ARect, EDGE_ETCHED, BF_LEFT);
      end;
    end
    else
      DrawThemeBackground(TOOLBAR_THEME, Canvas.Handle, XPFlags[Horizontal], TS_NORMAL, ARect, nil);
  end;
end;

procedure TTBXDefaultTheme.PaintToolbarNCArea(Canvas: TCanvas; R: TRect; const ToolbarInfo: TTBXToolbarInfo);
const
  DragHandleSizes: array [Boolean, DHS_DOUBLE..DHS_SINGLE] of Integer = ((9, 0, 6), (14, 14, 14));
  DragHandleOffsets: array [Boolean, DHS_DOUBLE..DHS_SINGLE] of Integer = ((2, 0, 2), (3, 0, 5));
  GripperPart: array [Boolean] of Cardinal = (RP_GRIPPER, RP_GRIPPERVERT);
  Pattern: array [0..15] of Byte = (0, 0, $CC, 0, $78, 0, $30, 0, $78, 0, $CC, 0, 0, 0, 0, 0);
var
//  DC: HDC;
  DHSize: Integer;
//  Sz: TSize;
  R2: TRect;
  Flags: Cardinal;
  Z: Integer;
  BtnVisible, Horz, CloseButtondown, CloseButtonHover: Boolean;
begin
{  DC := Canvas.Handle;
  if ToolbarInfo.EffectiveColor <> clNone then FillRectEx(DC, R, ToolbarInfo.EffectiveColor);     }
  Canvas.FillRect(R);

  { Border }
  if ToolbarInfo.BorderStyle = bsSingle then
  if USE_THEMES then
  begin
    Frame3D(Canvas.Handle, R, Lighten(ToolbarInfo.EffectiveColor, 24), Lighten(ToolbarInfo.EffectiveColor, -32), False);
//    DrawThemeEdge(TOOLBAR_THEME, Canvas.Handle, RP_BAND, 0, R, BDR_RAISEDINNER, BF_RECT or BF_ADJUST, @R)
  end
  else
    Windows.DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT or BF_ADJUST);

  if not ToolbarInfo.AllowDrag then Exit;

  BtnVisible := (ToolbarInfo.CloseButtonState and CDBS_VISIBLE) <> 0;
  Horz := not ToolbarInfo.IsVertical;

  DHSize := GetTBXDragHandleSize(ToolbarInfo);
  if Horz then R.Right := R.Left + DHSize
  else R.Bottom := R.Top + DHSize;

  { Drag handle area }
  if ToolbarInfo.DragHandleStyle <> DHS_NONE then
  begin
    if USE_THEMES then
    begin
      R2 := R;
      if BtnVisible then
        if Horz then Inc(R2.Top, DHSize - 1)
        else Dec(R2.Right, DHSize - 1);

      { Since GetThemePartSize does not seem to work properly, assume we use default
        WindowsXP themes where the gripper pattern repeats itself every 4 pixels }

      if Horz then
      begin
        R2.Left := (R2.Left + R2.Right - 6) div 2;
        R2.Right := R2.Left + 6;
        Z := R2.Bottom - R2.Top;
        R2.Top := R2.Top  - 1 + (Z and $3) shr 1;
        R2.Bottom := R2.Top + Z and not $3 + 1;
      end
      else
      begin
        R2.Top := (R2.Top + R2.Bottom - 6) div 2;
        R2.Bottom := R2.Top + 6;
        Z := R2.Right - R2.Left;
        R2.Left := R2.Left - 1 + (Z and $3) shr 1;
        R2.Right := R2.Left + Z and not $3 + 2;
      end;

      DrawThemeBackground(REBAR_THEME, Canvas.Handle, GripperPart[ToolbarInfo.IsVertical], 0, R2, nil)
    end
    else
    begin
      R2 := R;
      if Horz then
      begin
        Inc(R2.Left, DragHandleOffsets[BtnVisible, ToolbarInfo.DragHandleStyle]);
        if BtnVisible then Inc(R2.Top, DHSize - 2);
        R2.Right := R2.Left + 3;
        InflateRect(R2, 0, -1);
      end
      else
      begin
        Inc(R2.Top, DragHandleOffsets[BtnVisible, ToolbarInfo.DragHandleStyle]);
        if BtnVisible then Dec(R2.Right, DHSize - 2);
        R2.Bottom := R2.Top + 3;
        InflateRect(R2, -1, 0);
      end;

      Windows.DrawEdge(Canvas.Handle, R2, BDR_RAISEDINNER, BF_RECT);
      Canvas.Pixels[R2.Left, R2.Bottom - 1] := clBtnHighlight;
      if ToolbarInfo.DragHandleStyle = DHS_DOUBLE then
      begin
        if Horz then OffsetRect(R2, 3, 0)
        else OffsetRect(R2, 0, 3);
        Windows.DrawEdge(Canvas.Handle, R2, BDR_RAISEDINNER, BF_RECT);
        Canvas.Pixels[R2.Left, R2.Bottom - 1] := clBtnHighlight;
      end;
    end;
  end;

  { Close Button }
  if BtnVisible then
  begin
    CloseButtonDown := (ToolbarInfo.CloseButtonState and CDBS_PRESSED) <> 0;
    CloseButtonHover := (ToolbarInfo.CloseButtonState and CDBS_HOT) <> 0;
    R2 := GetTBXDockedCloseButtonRect(ToolbarInfo);
    Z := 2;
    if USE_THEMES then Z := 1;
    if Horz then
    begin
      Dec(R2.Bottom, Z);
      Dec(R2.Right, Z);
    end
    else
    begin
      Dec(R2.Bottom, Z);
      Inc(R2.Left, Z);
    end;
    if USE_THEMES then
    begin
      Flags := TS_NORMAL;
      if CloseButtonDown then Flags := TS_PRESSED
      else if CloseButtonHover then Flags := TS_HOT;
      DrawThemeBackground(TOOLBAR_THEME, Canvas.Handle, TP_BUTTON, Flags, R2, nil);
      if CloseButtonDown then OffsetRect(R2, 1, 1);
      DrawGlyph(Canvas.Handle, R2, 7, 7, Pattern[0], clBtnText);
    end
    else
    begin
      if CloseButtonDown then
      begin
        Windows.DrawEdge(Canvas.Handle, R2, BDR_SUNKENOUTER, BF_RECT);
        OffsetRect(R2, 1, 1);
      end
      else if CloseButtonHover then
        Windows.DrawEdge(Canvas.Handle, R2, BDR_RAISEDINNER, BF_RECT);
      DrawGlyph(Canvas.Handle, R2, 7, 7, Pattern[0], clBtnText);
    end;
  end;
end;

procedure TTBXDefaultTheme.PaintDock(Canvas: TCanvas; const ClientRect, DockRect: TRect; DockPosition: Integer);
var
  R: TRect;
begin
  if not USE_THEMES then Exit;
  if DockPosition in [DP_LEFT, DP_RIGHT] then
  begin
    R := DockRect;
    Inc(R.Bottom, 1);
    DrawThemeBackground(REBAR_THEME, Canvas.Handle, 0, 0, R, nil);
  end
  else DrawThemeBackground(REBAR_THEME, Canvas.Handle, 0, 0, DockRect, nil);
end;

procedure TTBXDefaultTheme.PaintDockPanelNCArea(Canvas: TCanvas; R: TRect; const DockPanelInfo: TTBXDockPanelInfo);
var
  DC: HDC;
  Sz: Integer;
  R2: TRect;
  Flags: Integer;
  CloseButtonDown, CloseButtonHover: Boolean;

  procedure CaptionFill(R: TRect);
  const
    GRAD: array [Boolean] of TGradientKind = (gkHorz, gkVert);
  begin
    if USE_THEMES then
      GradFill(DC, R, Lighten(ToolbarColor, 12), Lighten(ToolbarColor, -12), GRAD[DockPanelInfo.IsVertical])
    else
      FillRectEx(DC, R, ToolbarColor);
  end;

begin
  DC := Canvas.Handle;
  with Canvas, DockPanelInfo do
  begin
    Sz := GetSystemMetrics(SM_CYSMCAPTION);

    { Border }
    FrameRectEx(DC, R, ToolbarColor, True);
    R2 := R;
    if ShowCaption then
      if IsVertical then Inc(R2.Top, Sz)
      else Inc(R2.Left, Sz);
    FrameRectEx(DC, R2, clWindow, False);

    if not ShowCaption then Exit;

    { Caption area }
    if IsVertical then R.Bottom := R.Top + Sz
    else R.Right := R.Left + Sz;
    Windows.DrawEdge(Handle, R, BDR_RAISEDINNER, BF_RECT or BF_ADJUST);

    { Close button }
    if (CDBS_VISIBLE and CloseButtonState) <> 0 then
    begin
      CloseButtonDown := (CloseButtonState and CDBS_PRESSED) <> 0;
      CloseButtonHover := (CloseButtonState and CDBS_HOT) <> 0;
      R2 := R;
      Brush.Color := ToolbarColor;
      if IsVertical then
      begin
        R2.Left := R2.Right - Sz;
        R.Right := R2.Left;
        CaptionFill(R2);
        InflateRect(R2, -1, -1);
        Inc(R2.Left);
      end
      else
      begin
        R2.Top := R2.Bottom - Sz;
        R.Bottom := R2.Top;
        CaptionFill(R2);
        InflateRect(R2, -1, -1);
        Dec(R2.Bottom);
      end;

      if USE_THEMES then
      begin
        Flags := TS_NORMAL;
        if CloseButtonDown then Flags := TS_PRESSED
        else if CloseButtonHover then Flags := TS_HOT;
        DrawThemeBackground(TOOLBAR_THEME, DC, TP_BUTTON, Flags, R2, nil);
        if CloseButtonDown then OffsetRect(R2, 1, 1);
        InflateRect(R2, -2, -2);
      end
      else
      begin
        if CloseButtonDown then
        begin
          Windows.DrawEdge(DC, R2, BDR_SUNKENOUTER, BF_RECT);
          OffsetRect(R2, 1, 1);
        end
        else if CloseButtonHover then
          Windows.DrawEdge(DC, R2, BDR_RAISEDINNER, BF_RECT);
        InflateRect(R2, -2, -2);
      end;
      DrawButtonBitmap(Canvas, R2);
    end;

    { Caption }
    CaptionFill(R);
    if IsVertical then InflateRect(R, -2, 0)
    else Inflaterect(R, 0, -2);
    Font.Assign(SmCaptionFont);
    Font.Color := clBtnText;
    Brush.Style := bsClear;
    Flags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX;
    if IsVertical then DrawText(Canvas.Handle, Caption, -1, R, Flags)
    else DrawRotatedText(Canvas.Handle, string(Caption), R, Flags);
    Brush.Style := bsSolid;
  end;
end;

function TTBXDefaultTheme.GetPopupShadowType: Integer;
begin
  Result := PST_WINDOWSXP;
end;

procedure TTBXDefaultTheme.GetViewMargins(ViewType: Integer; out Margins: TTBXMargins);
begin
  with Margins do
    if ((ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
      ((ViewType and TVT_FLOATING) = TVT_FLOATING) then
    begin
      LeftWidth := 4;
      TopHeight := 2;
      RightWidth := 4;
      BottomHeight := 1;
    end
    else
    begin
      LeftWidth := 0;
      TopHeight := 0;
      RightWidth := 0;
      BottomHeight := 0;
    end;
end;

procedure TTBXDefaultTheme.PaintPageScrollButton(Canvas: TCanvas;
  const ARect: TRect; ButtonType: Integer; Hot: Boolean);
var
  R: TRect;
  Flags: Integer;
  X, Y, Sz: Integer;
begin
  R := ARect;
  if USE_THEMES then
  begin
    if Hot then Flags := TS_PRESSED
    else Flags := TS_HOT;
    DrawThemeBackground(TOOLBAR_THEME, Canvas.Handle, TP_BUTTON, Flags, ARect, nil);
    X := (R.Left + R.Right) div 2;
    Y := (R.Top + R.Bottom) div 2;
    Sz := Min(X - R.Left, Y - R.Top) * 3 div 4;
    if Hot then Canvas.Pen.Color := clBtnText;
    Canvas.Brush.Color := Canvas.Pen.Color;
    case ButtonType of
      PSBT_UP:
        begin
          Inc(Y, Sz div 2);
          Canvas.Polygon([Point(X + Sz, Y), Point(X, Y - Sz), Point(X - Sz, Y)]);
        end;
      PSBT_DOWN:
        begin
          Y := (R.Top + R.Bottom - 1) div 2;
          Dec(Y, Sz div 2);
          Canvas.Polygon([Point(X + Sz, Y), Point(X, Y + Sz), Point(X - Sz, Y)]);
        end;
      PSBT_LEFT:
        begin
          Inc(X, Sz div 2);
          Canvas.Polygon([Point(X, Y + Sz), Point(X - Sz, Y), Point(X, Y - Sz)]);
        end;
      PSBT_RIGHT:
        begin
          X := (R.Left + R.Right - 1) div 2;
          Dec(X, Sz div 2);
          Canvas.Polygon([Point(X, Y + Sz), Point(X + Sz, Y), Point(X, Y - Sz)]);
        end;
    end;
  end
  else
  begin
    if Hot then Flags := DFCS_FLAT
    else Flags := 0;
    case ButtonType of
      PSBT_UP: Flags := Flags or DFCS_SCROLLUP;
      PSBT_DOWN: Flags := Flags or DFCS_SCROLLDOWN;
      PSBT_LEFT: Flags := Flags or DFCS_SCROLLLEFT;
      PSBT_RIGHT: Flags := Flags or DFCS_SCROLLRIGHT;
    end;
    Windows.DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, Flags);
  end;
end;

procedure TTBXDefaultTheme.PaintFrameControl(Canvas: TCanvas; R: TRect; Kind, State: Integer; Params: Pointer);
var
  DC: HDC;
  X, Y, Flags: Integer;
  C: TColor;
  Pen, OldPen: HPen;
  Brush, OldBrush: HBrush;

  function FrameColor: TColor;
  begin
    if Boolean(State and PFS_DISABLED) then Result := clBtnShadow
    else if Boolean(State and (PFS_PUSHED or PFS_HOT)) then Result := clNone
    else Result := clBtnShadow;
  end;

  procedure DiagLine(C: TColor);
  begin
    with R do
      DrawLineEx(DC, Right - 2 - X, Bottom - 2, Right - 1, Bottom - X - 3, C);
    Inc(X);
  end;

begin
  DC := Canvas.Handle;
  case Kind of
    PFC_CHECKBOX:
      begin
        if USE_THEMES then
        begin
          if Boolean(State and PFS_CHECKED) then Flags := CBS_CHECKEDNORMAL
          else if Boolean(State and PFS_MIXED) then Flags := CBS_MIXEDNORMAL
          else Flags := CBS_UNCHECKEDNORMAL;
          if Boolean(State and PFS_DISABLED) then Inc(Flags, 3)
          else if Boolean(State and PFS_PUSHED) then Inc(Flags, 2)
          else if Boolean(State and PFS_HOT) then Inc(Flags);
          DrawThemeBackground(BUTTON_THEME, DC, BP_CHECKBOX, Flags, R, nil);
        end
        else
        begin
          C := FrameColor;
          if C = clNone then
          begin
            if Boolean(State and PFS_MIXED) then Flags := DFCS_BUTTON3STATE or DFCS_CHECKED
            else Flags := DFCS_BUTTONCHECK;
            if Boolean(State and PFS_CHECKED) then Flags := Flags or DFCS_CHECKED;
            if Boolean(State and PFS_PUSHED) then Flags := Flags or DFCS_PUSHED;
            DrawFrameControl(DC, R, DFC_BUTTON, Flags);
          end
          else
          begin
            InflateRect(R, -1, -1);
            FrameRectEx(DC, R, C, True);
            if Boolean(State and (PFS_DISABLED or PFS_PUSHED)) then FillRectEx(DC, R, clBtnFace)
            else if Boolean(State and PFS_MIXED) then DitherRect(DC, R, clWindow, clBtnFace)
            else FillRectEx(DC, R, clWindow);
          
            if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
            begin
              X := (R.Left + R.Right) div 2 - 1;
              Y := (R.Top + R.Bottom) div 2 + 1;
              if Boolean(State and PFS_DISABLED) then C := clGrayText
              else if Boolean(State and PFS_MIXED) then C := clBtnShadow
              else C := clBtnText;
              PolygonEx(DC, [Point(X-2, Y), Point(X, Y+2), Point(X+4, Y-2),
                Point(X+4, Y-4), Point(X, Y), Point(X-2, Y-2), Point(X-2, Y)], C, C);
            end;
          end;
        end;
      end;
    PFC_RADIOBUTTON:
      begin
        if USE_THEMES then
        begin
          if Boolean(State and PFS_CHECKED) then Flags := RBS_CHECKEDNORMAL
          else Flags := RBS_UNCHECKEDNORMAL;
          if Boolean(State and PFS_DISABLED) then Inc(Flags, 3)
          else if Boolean(State and PFS_PUSHED) then Inc(Flags, 2)
          else if Boolean(State and PFS_HOT) then Inc(Flags);
          DrawThemeBackground(BUTTON_THEME, DC, BP_RADIOBUTTON, Flags, R, nil);
        end
        else
        begin
          C := FrameColor;
          if C = clNone then
          begin
            Flags := DFCS_BUTTONRADIO;
            if Boolean(State and PFS_CHECKED) then Flags := Flags or DFCS_CHECKED;
            if Boolean(State and PFS_PUSHED) then Flags := Flags or DFCS_PUSHED;
            DrawFrameControl(DC, R, DFC_BUTTON, Flags);
          end
          else
          begin
            Pen := CreatePenEx(C);
            if Boolean(State and (PFS_DISABLED or PFS_PUSHED)) then C := clBtnFace
            else C := clWindow;
            Brush := CreateBrushEx(C);
            
            InflateRect(R, -1, -1); Inc(R.Left); Dec(R.Bottom);
            OldPen := SelectObject(DC, Pen);
            OldBrush := SelectObject(DC, Brush);
            with R do
              Windows.RoundRect(DC, Left, Top, Right, Bottom, Right - Left - 2, Bottom - Top - 2);
            SelectObject(DC, OldPen);
            SelectObject(DC, OldBrush);
            DeleteObject(Pen);
            DeleteObject(Brush);

            if Boolean(State and PFS_CHECKED) then
            begin
              InflateRect(R, -3, -3);
              Pen := CreatePenEx(clBtnText);
              Brush := CreateBrushEx(clBtnText);
              OldPen := SelectObject(DC, Pen);
              OldBrush := SelectObject(DC, Brush);
              with R do Windows.RoundRect(DC, Left, Top, Right, Bottom, Right - Left, Bottom - Top);
              SelectObject(DC, OldPen);
              SelectObject(DC, OldBrush);
              DeleteObject(Pen);
              DeleteObject(Brush);
            end;
          end;
        end;
      end;
  end;
end;

procedure TTBXDefaultTheme.PaintStatusBar(Canvas: TCanvas; R: TRect; Part: Integer);
var
  DC: HDC;
  D, Sz, I, Flags: Integer;

  procedure DiagLine(C: TColor);
  begin
    with R do
      DrawLineEx(DC, Right - 2 - D, Bottom - 2, Right - 1, Bottom - D - 3, C);
    Inc(D);
  end;

begin
  DC := Canvas.Handle;
  case Part of
    SBP_BODY:
      begin
        if USE_THEMES then
          DrawThemeBackground(STATUSBAR_THEME, DC, 0, 0, R, nil)
        else
          FillRectEx(DC, R, StatusBarColor);
      end;
    SBP_PANE, SBP_LASTPANE:
      begin
        if USE_THEMES then
        begin
          if Part = SBP_LASTPANE then Flags := SP_GRIPPERPANE
          else Flags := SP_PANE;
          DrawThemeBackground(STATUSBAR_THEME, DC, Flags, 0, R, nil);
        end
        else
        begin
          if Part = SBP_PANE then Dec(R.Right, 2);
          Frame3D(DC, R, clBtnShadow, clBtnHighlight, False);
        end;
      end;
    SBP_GRIPPER:
      begin
        if USE_THEMES then
          DrawThemeBackground(STATUSBAR_THEME, DC, SP_GRIPPER, 0, R, nil)
        else
        begin
          D := 0;
          Sz := Min(R.Right - R.Left, R.Bottom - R.Top);
          for I := 1 to 3 do
            case Sz of
              0..8:
                begin
                  DiagLine(clBtnShadow);
                  DiagLine(clBtnHighlight);
                end;
              9..11:
                begin
                  DiagLine(StatusBarColor);
                  DiagLine(clBtnShadow);
                  DiagLine(clBtnHighlight);
                end;
              12..14:
                begin
                  DiagLine(clBtnShadow);
                  DiagLine(clBtnShadow);
                  DiagLine(clBtnHighlight);
                end;
            else
              DiagLine(StatusBarColor);
              DiagLine(clBtnShadow);
              DiagLine(clBtnShadow);
              DiagLine(clBtnHighlight);
            end;

          with Canvas, R do
          begin
            Pen.Color := StatusBarColor;
            PolylineEx(DC, [
              Point(Right - D - 1, Bottom - 1),
              Point(Right - 1, Bottom - 1),
              Point(Right - 1, Bottom - D - 2)],
              StatusBarColor);
          end;
        end;
      end;
  end;
end;

procedure TTBXDefaultTheme.SetupColorCache;
begin
  ToolbarColor := clBtnFace;
  ToolbarText := clBtnText;
  StatusBarColor := clBtnFace;
  if USE_THEMES then
  begin
    GetThemeColor(TOOLBAR_THEME, 0, 0, TMT_FILLCOLOR, Cardinal(ToolbarColor));
    GetThemeColor(TOOLBAR_THEME, 0, 0, TMT_TEXTCOLOR, Cardinal(ToolbarText));
    GetThemeColor(STATUSBAR_THEME, 0, 0, TMT_FILLCOLOR, Cardinal(StatusBarColor));
  end;
  DockPanelColor := NearestMixedColor(ToolbarColor, clWindow, 64);
end;

procedure TTBXDefaultTheme.TBXSysCommand(var Message: TMessage);
begin
  if Message.WParam = TSC_VIEWCHANGE then SetupColorCache;
end;

initialization
  InitializeStock;

finalization
  FinalizeStock;

end.
