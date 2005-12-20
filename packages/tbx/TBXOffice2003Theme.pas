unit TBXOffice2003Theme;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// Office2003 theme
// Copyright (c) Yury Plashenkov (Sep. 2005) <mailto:plashenkov@mail.ru>
//
// Version for TBX version 2.1

interface

{$I TB2Ver.inc}
{$I TBX.inc}

// PngComponents is a set of components that allows you to include in your
// application semitransparent PNG images
// I advise you to get it from http://www.thany.org/
// After installing, use TPngImageList (from PngComponents package) instead of
// "classic" TImageList

// Comment next string if you use Toolbar2000 version different from 2.1.6 
{$DEFINE TB2K_VER216} 

// Uncomment next string if you want to see highlighted icons
//{$DEFINE HIGHLIGHTTOOLBARICONS}

// Uncomment next string to turn on gradient captions on dockpanels
//{$DEFINE DOCKPANELGRADIENTCAPTION}

uses
  Windows, Messages, Graphics, Classes, TBXThemes, ImgList, TBXUxThemes;

type
  TItemPart = (ipBody, ipText, ipFrame);
  TBtnItemState = (bisNormal, bisDisabled, bisSelected, bisPressed, bisHot,
    bisDisabledHot, bisSelectedHot, bisPopupParent);
  TMenuItemState = (misNormal, misDisabled, misHot, misDisabledHot);
  TWinFramePart = (wfpBorder, wfpCaption, wfpCaptionText);
  
  TOffice2003Scheme = (osBlue, osMetallic, osGreen, osUnknown);

  TTBXOffice2003Theme = class(TTBXTheme)
  private
    FOnSetupColorCache: TNotifyEvent;
    procedure TBXSysCommand(var Message: TMessage); message TBX_SYSCOMMAND;
  protected
    DockColor: TColor;

    ToolbarColor1: TColor;
    ToolbarColor2: TColor;
    ToolbarFrameColor1: TColor;
    ToolbarFrameColor2: TColor;
    SeparatorColor1: TColor;
    SeparatorColor2: TColor;
    DragHandleColor1: TColor;
    DragHandleColor2: TColor;

    EmbeddedColor: TColor;
    EmbeddedFrameColor: TColor;
    EmbeddedDisabledColor: TColor;
    
    PopupColor: TColor;
    PopupFrameColor: TColor;
    
    DockPanelColor: TColor;
    WinFrameColors: array[TWinFramePart] of TColor;
    MenuItemColors: array[TMenuItemState, TItemPart] of TColor;
    BtnItemColors: array[TBtnItemState, ipText..ipFrame] of TColor;
    BtnBodyColors: array[TBtnItemState, Boolean] of TColor;

    StatusPanelFrameColor: TColor;
    FMDIAreaColor: TColor;

    procedure SetupColorCache; virtual;
  protected
    function GetPartColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
    function GetBtnColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart; GradColor2: Boolean = False): TColor;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    function  GetBooleanMetrics(Index: Integer): Boolean; override;
    function  GetIntegerMetrics(Index: Integer): Integer; override;
    procedure GetMargins(MarginID: Integer; out Margins: TTBXMargins); override;
    function  GetImageOffset(Canvas: TCanvas; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList): TPoint; override;
    function  GetItemColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetPopupShadowType: Integer; override;
    procedure GetViewBorder(ViewType: Integer; out Border: TPoint); override;
    function  GetViewColor(AViewType: Integer): TColor; override;
    procedure GetViewMargins(ViewType: Integer; out Margins: TTBXMargins); override;

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

    property MDIAreaColor: TColor read FMDIAreaColor;
    
    property OnSetupColorCache: TNotifyEvent read FOnSetupColorCache write FOnSetupColorCache;
  end;

function GetOffice2003Scheme: TOffice2003Scheme;
procedure PaintGradient(DC: HDC; const ARect: TRect; Color1, Color2: TColor);
procedure PaintIrregularGradient(DC: HDC; const ARect: TRect; Color1, Color2: TColor; Horz: Boolean);
function GetMDIWorkspaceColor: TColor;

implementation

uses TBXUtils, TB2Common, TB2Item, Controls, CommCtrl, Forms, SysUtils;

function GetOffice2003Scheme: TOffice2003Scheme;
const
  MaxChars = 1024;
var
  pszThemeFileName, pszColorBuff, pszSizeBuf: PWideChar;
  S: string;
begin
  Result := osUnknown;
  if USE_THEMES then
  begin
    GetMem(pszThemeFileName, 2 * MaxChars);
    GetMem(pszColorBuff,     2 * MaxChars);
    GetMem(pszSizeBuf,       2 * MaxChars);
    try
      if not Failed(GetCurrentThemeName(pszThemeFileName, MaxChars, pszColorBuff, MaxChars, pszSizeBuf, MaxChars)) then
        if UpperCase(ExtractFileName(pszThemeFileName)) = 'LUNA.MSSTYLES' then
        begin
          S := UpperCase(pszColorBuff);
          if S = 'NORMALCOLOR' then
            Result := osBlue
          else if S = 'METALLIC' then
            Result := osMetallic
          else if S = 'HOMESTEAD' then
            Result := osGreen;
        end;
    finally
      FreeMem(pszSizeBuf);
      FreeMem(pszColorBuff);
      FreeMem(pszThemeFileName);
    end;
  end;
end;

procedure PaintGradient(DC: HDC; const ARect: TRect; Color1, Color2: TColor);
var
  r1, g1, b1, r2, g2, b2: Byte;
  I, Size: Integer;
  hbr: HBRUSH;
  lprc: TRect;
begin
  Color1 := ColorToRGB(Color1);
  Color2 := ColorToRGB(Color2);
  if Color1 = Color2 then
    FillRectEx(DC, ARect, Color1)
  else
  begin
    Size := ARect.Bottom - ARect.Top;
    if Size <= 0 then Exit;

    r1 := GetRValue(Color1);
    g1 := GetGValue(Color1);
    b1 := GetBValue(Color1);
    r2 := GetRValue(Color2);
    g2 := GetGValue(Color2);
    b2 := GetBValue(Color2);

    lprc := ARect;
    lprc.Bottom := lprc.Top + 1;

    Dec(Size);
    for I := 0 to Size do
    begin
      hbr := CreateSolidBrush(RGB((r2 - r1) * I div Size + r1, (g2 - g1) * I div Size + g1, (b2 - b1) * I div Size + b1));
      FillRect(DC, lprc, hbr);
      DeleteObject(hbr);
      OffsetRect(lprc, 0, 1);
    end;
  end;
end;

var
  IrregularGradientValue: Integer;

procedure PaintIrregularGradient(DC: HDC; const ARect: TRect; Color1, Color2: TColor; Horz: Boolean);
var
  r1, g1, b1, r2, g2, b2: Integer;
  rm1, gm1, bm1, rm2, gm2, bm2: Integer;
  lprc: TRect;
  I, Size, Middle: Integer;
  hbr: HBRUSH;
begin
  Color1 := ColorToRGB(Color1);
  Color2 := ColorToRGB(Color2);
  if Color1 = Color2 then
    FillRectEx(DC, ARect, Color1)
  else
  begin
    if IsRectEmpty(ARect) then Exit;

    r1 := GetRValue(Color1);
    g1 := GetGValue(Color1);
    b1 := GetBValue(Color1);
    r2 := GetRValue(Color2);
    g2 := GetGValue(Color2);
    b2 := GetBValue(Color2);

    lprc := ARect;

    if Horz then
    begin
      Size := ARect.Right - ARect.Left;
      lprc.Right := lprc.Left + 1;
    end
    else
    begin
      Size := ARect.Bottom - ARect.Top;
      lprc.Bottom := lprc.Top + 1;
    end;

    Middle := Size div 2;

    rm1 := (r2 - r1) * IrregularGradientValue div 40 + r1;
    gm1 := (g2 - g1) * IrregularGradientValue div 40 + g1;
    bm1 := (b2 - b1) * IrregularGradientValue div 40 + b1;
    rm2 := (rm1 - r1) * Size div Middle + r1;
    gm2 := (gm1 - g1) * Size div Middle + g1;
    bm2 := (bm1 - b1) * Size div Middle + b1;

    Dec(Size);
    for I := 0 to Middle - 1 do
    begin
      hbr := CreateSolidBrush(RGB((rm2 - r1) * I div Size + r1, (gm2 - g1) * I div Size + g1, (bm2 - b1) * I div Size + b1));
      FillRect(DC, lprc, hbr);
      DeleteObject(hbr);
      OffsetRect(lprc, Ord(Horz), Ord(not Horz));
    end;

    hbr := CreateSolidBrush(RGB(rm1, gm1, bm1));
    FillRect(DC, lprc, hbr);
    DeleteObject(hbr);
    OffsetRect(lprc, Ord(Horz), Ord(not Horz));

    rm1 := rm1 * 2 - r2;
    gm1 := gm1 * 2 - g2;
    bm1 := bm1 * 2 - b2;

    for I := Middle + 1 to Size do
    begin
      hbr := CreateSolidBrush(RGB((r2 - rm1) * I div Size + rm1, (g2 - gm1) * I div Size + gm1, (b2 - bm1) * I div Size + bm1));
      FillRect(DC, lprc, hbr);
      DeleteObject(hbr);
      OffsetRect(lprc, Ord(Horz), Ord(not Horz));
    end;
  end;
end;

function GetMDIWorkspaceColor: TColor;
const
  MDIColors: array[TOffice2003Scheme] of TColor = ($AE9990, $B39A9B, $7BA097, clBtnShadow);
begin
  Result := MDIColors[GetOffice2003Scheme];
end;

var
  StockImgList: TImageList;
  CounterLock: Integer;

procedure InitializeStock;
begin
  StockImgList := TImageList.Create(nil);
  StockImgList.Handle := ImageList_LoadBitmap(HInstance, 'TBXGLYPHS', 16, 0, clWhite);
end;

procedure FinalizeStock;
begin
  StockImgList.Free;
end;

{ TTBXOffice2003Theme }

function TTBXOffice2003Theme.GetBooleanMetrics(Index: Integer): Boolean;
begin
  case Index of
    TMB_OFFICEXPPOPUPALIGNMENT: Result := True;
    TMB_EDITMENUFULLSELECT:     Result := True;
    TMB_EDITHEIGHTEVEN:         Result := False;
    TMB_SOLIDTOOLBARNCAREA:     Result := False;
    TMB_SOLIDTOOLBARCLIENTAREA: Result := True;
    TMB_PAINTDOCKBACKGROUND:    Result := True;
  else
    Result := False;
  end;
end;

function TTBXOffice2003Theme.GetIntegerMetrics(Index: Integer): Integer;
begin
  case Index of
    TMI_SPLITBTN_ARROWWIDTH:  Result := 12;

    TMI_DROPDOWN_ARROWWIDTH:  Result := 8;
    TMI_DROPDOWN_ARROWMARGIN: Result := 3;

    TMI_MENU_IMGTEXTSPACE:    Result := 5;
    TMI_MENU_LCAPTIONMARGIN:  Result := 3;
    TMI_MENU_RCAPTIONMARGIN:  Result := 3;
    TMI_MENU_SEPARATORSIZE:   Result := 3;
    TMI_MENU_MDI_DW:          Result := 2;
    TMI_MENU_MDI_DH:          Result := 2;

    TMI_TLBR_SEPARATORSIZE:   Result := 6;

    TMI_EDIT_FRAMEWIDTH:      Result := 1;
    TMI_EDIT_TEXTMARGINHORZ:  Result := 2;
    TMI_EDIT_TEXTMARGINVERT:  Result := 2;
    TMI_EDIT_BTNWIDTH:        Result := 14;
    TMI_EDIT_MENURIGHTINDENT: Result := 1;
  else
    Result := -1;
  end;
end;

function TTBXOffice2003Theme.GetViewColor(AViewType: Integer): TColor;
begin
  Result := DockColor;
  if (AViewType and VT_TOOLBAR) = VT_TOOLBAR then
  begin
    if (AViewType and TVT_MENUBAR) = TVT_MENUBAR then Result := DockColor
    else Result := ToolbarColor1;
  end
  else if (AViewType and VT_POPUP) = VT_POPUP then
  begin
    if (AViewType and PVT_LISTBOX) = PVT_LISTBOX then Result := clWindow
    else Result := PopupColor;
  end
  else if (AViewType and VT_DOCKPANEL) = VT_DOCKPANEL then Result := DockPanelColor;
end;

function TTBXOffice2003Theme.GetBtnColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart; GradColor2: Boolean = False): TColor;
const
  BFlags1: array [Boolean] of TBtnItemState = (bisDisabled, bisDisabledHot);
  BFlags2: array [Boolean] of TBtnItemState = (bisSelected, bisSelectedHot);
  BFlags3: array [Boolean] of TBtnItemState = (bisNormal, bisHot);
var
  B: TBtnItemState;
  Embedded: Boolean;
begin
  with ItemInfo do
  begin
    Embedded := (ViewType and VT_TOOLBAR = VT_TOOLBAR) and (ViewType and TVT_EMBEDDED = TVT_EMBEDDED);
    if not Enabled then B := BFlags1[HoverKind = hkKeyboardHover]
    else if ItemInfo.IsPopupParent then B := bisPopupParent
    else if Pushed then B := bisPressed
    else if Selected then B := BFlags2[HoverKind <> hkNone]
    else B := BFlags3[HoverKind <> hkNone];
    if ItemPart = ipBody then
      Result := BtnBodyColors[B, GradColor2]
    else
      Result := BtnItemColors[B, ItemPart];
    if Embedded and (Result = clNone) then
    begin
      if ItemPart = ipBody then Result := EmbeddedColor;
      if (ItemPart = ipFrame) and not Selected then Result := EmbeddedFrameColor;
    end;
  end;
end;

function TTBXOffice2003Theme.GetPartColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
const
  MFlags1: array [Boolean] of TMenuItemState = (misDisabled, misDisabledHot);
  MFlags2: array [Boolean] of TMenuItemState = (misNormal, misHot);
  BFlags1: array [Boolean] of TBtnItemState = (bisDisabled, bisDisabledHot);
  BFlags2: array [Boolean] of TBtnItemState = (bisSelected, bisSelectedHot);
  BFlags3: array [Boolean] of TBtnItemState = (bisNormal, bisHot);
var
  IsMenuItem, Embedded: Boolean;
  M: TMenuItemState;
  B: TBtnItemState;
begin
  with ItemInfo do
  begin
    IsMenuItem := ((ViewType and PVT_POPUPMENU) = PVT_POPUPMENU) and
      ((ItemOptions and IO_TOOLBARSTYLE) = 0);
    Embedded := ((ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
      ((ViewType and TVT_EMBEDDED) = TVT_EMBEDDED);
    if IsMenuItem then
    begin
      if not Enabled then M := MFlags1[HoverKind = hkKeyboardHover]
      else M := MFlags2[HoverKind <> hkNone];
      Result := MenuItemColors[M, ItemPart];
    end
    else
    begin
      if not Enabled then B := BFlags1[HoverKind = hkKeyboardHover]
      else if ItemInfo.IsPopupParent then B := bisPopupParent
      else if Pushed then B := bisPressed
      else if Selected then B := BFlags2[HoverKind <> hkNone]
      else B := BFlags3[HoverKind <> hkNone];
      if ItemPart = ipBody then
        Result := BtnBodyColors[B, False]
      else
        Result := BtnItemColors[B, ItemPart];
      if Embedded and (Result = clNone) then
      begin
        if ItemPart = ipBody then Result := EmbeddedColor;
        if ItemPart = ipFrame then Result := EmbeddedFrameColor;
      end;
    end;
  end;
end;

function TTBXOffice2003Theme.GetItemColor(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetPartColor(ItemInfo, ipBody);
  if Result = clNone then Result := GetViewColor(ItemInfo.ViewType);
end;

function TTBXOffice2003Theme.GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetPartColor(ItemInfo, ipText);
end;

function TTBXOffice2003Theme.GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetBtnColor(ItemInfo, ipBody);
  if Result = clNone then Result := GetViewColor(ItemInfo.ViewType);
end;

procedure TTBXOffice2003Theme.GetViewBorder(ViewType: Integer; out Border: TPoint);
const
  XMetrics: array [Boolean] of Integer = (SM_CXDLGFRAME, SM_CXFRAME);
  YMetrics: array [Boolean] of Integer = (SM_CYDLGFRAME, SM_CYFRAME);
var
  Resizable: Boolean;

  procedure SetBorder(X, Y: Integer);
  begin
    Border.X := X;
    Border.Y := Y;
  end;

begin
  if (ViewType and VT_TOOLBAR) = VT_TOOLBAR then
  begin
    if (ViewType and TVT_FLOATING) = TVT_FLOATING then
    begin
      Resizable := (ViewType and TVT_RESIZABLE) = TVT_RESIZABLE;
      Border.X := GetSystemMetrics(XMetrics[Resizable]) - 1;
      Border.Y := GetSystemMetrics(YMetrics[Resizable]) - 1;
    end
    else SetBorder(2, 2);
  end
  else if (ViewType and VT_POPUP) = VT_POPUP then
  begin
    if (ViewType and PVT_POPUPMENU) = PVT_POPUPMENU then Border.X := 1
    else Border.X := 2;
    Border.Y := 2;
  end
  else if (ViewType and VT_DOCKPANEL) = VT_DOCKPANEL then
  begin
    if (ViewType and DPVT_FLOATING) = DPVT_FLOATING then
    begin
      Resizable := (ViewType and DPVT_RESIZABLE) = DPVT_RESIZABLE;
      Border.X := GetSystemMetrics(XMetrics[Resizable]) - 1;
      Border.Y := GetSystemMetrics(YMetrics[Resizable]) - 1;
    end
    else SetBorder(2, 2);
  end
  else SetBorder(0, 0);
end;

procedure TTBXOffice2003Theme.GetMargins(MarginID: Integer; out Margins: TTBXMargins);
begin
  with Margins do
    case MarginID of
      MID_TOOLBARITEM:
        begin
          LeftWidth := 2; RightWidth := 2;
          TopHeight := 2; BottomHeight := 2;
        end;
      MID_MENUITEM:
        begin
          LeftWidth := 1; RightWidth := 1;
          TopHeight := 3; BottomHeight := 3;
        end;
      MID_STATUSPANE:
        begin
          LeftWidth := 1; RightWidth := 3;
          TopHeight := 1; BottomHeight := 1;
        end;
    else
      LeftWidth := 0;
      RightWidth := 0;
      TopHeight := 0;
      BottomHeight := 0;
    end;
end;

procedure TTBXOffice2003Theme.PaintBackgnd(Canvas: TCanvas; const ADockRect, ARect, AClipRect: TRect;
  AColor: TColor; Transparent: Boolean; AViewType: Integer);
var
  DC: HDC;
  R: TRect;
  Horz: Boolean;
begin
  DC := Canvas.Handle;
  if not Transparent then
  begin
    IntersectRect(R, ARect, AClipRect);
    if ((AViewType and TVT_NORMALTOOLBAR = TVT_NORMALTOOLBAR) or (AViewType and TVT_TOOLWINDOW = TVT_TOOLWINDOW)) and not (AViewType and TVT_EMBEDDED = TVT_EMBEDDED) then
    begin
      if IsRectEmpty(ADockRect) then
        Horz := (ARect.Right > ARect.Bottom)
      else
        Horz := Abs(R.Right - R.Left) > Abs(R.Bottom - R.Top);
      PaintIrregularGradient(Canvas.Handle, R, ToolbarColor1, ToolbarColor2, not Horz);
    end
    else
      FillRectEx(DC, R, AColor);
  end;
end;

procedure TTBXOffice2003Theme.PaintCaption(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo; const ACaption: string;
  AFormat: Cardinal; Rotated: Boolean);
var
  R: TRect;
begin
  with ItemInfo, Canvas do
  begin
    R := ARect;
    Brush.Style := bsClear;
    if Font.Color = clNone then Font.Color := GetPartColor(ItemInfo, ipText);
    if not Rotated then Windows.DrawText(Handle, PChar(ACaption), Length(ACaption), R, AFormat)
    else DrawRotatedText(Handle, ACaption, R, AFormat);
    Brush.Style := bsSolid;
  end;
end;

procedure TTBXOffice2003Theme.PaintCheckMark(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  DC: HDC;
  X, Y: Integer;
  C: TColor;
begin
  DC := Canvas.Handle;
  X := (ARect.Left + ARect.Right) div 2 - 2;
  Y := (ARect.Top + ARect.Bottom) div 2 + 1;
  C := GetBtnColor(ItemInfo, ipText);
  if ItemInfo.ItemOptions and IO_RADIO > 0 then
  begin
    RoundRectEx(DC, X - 1, Y - 4, X + 5, Y + 2, 2, 2, MixColors(C, ToolbarColor1, 200), clNone);
    RoundRectEx(DC, X - 1, Y - 4, X + 5, Y + 2, 6, 6, C, C);
  end
  else
    PolylineEx(DC, [Point(X - 2, Y - 2), Point(X, Y), Point(X + 4, Y - 4),
      Point(X + 4, Y - 3), Point(X, Y + 1), Point(X - 2, Y - 1), Point(X - 2, Y - 2)], C);
end;

procedure TTBXOffice2003Theme.PaintChevron(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo);
const
  Pattern: array[Boolean, 0..15] of Byte = (
    ($CC, 0, $66, 0, $33, 0, $66, 0, $CC, 0, 0, 0, 0, 0, 0, 0),
    ($88, 0, $D8, 0, $70, 0, $20, 0, $88, 0, $D8, 0, $70, 0, $20, 0));
var
  R2: TRect;
  W, H: Integer;
begin
  R2 := ARect;
  PaintButton(Canvas, ARect, ItemInfo);
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
  DrawGlyph(Canvas.Handle, R2, W, H, Pattern[ItemInfo.IsVertical][0], GetPartColor(ItemInfo, ipText));
end;

procedure TTBXOffice2003Theme.PaintEditButton(Canvas: TCanvas; const ARect: TRect;
  var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo);
var
  DC: HDC;
  BtnDisabled, BtnHot, BtnPressed, Embedded: Boolean;
  R, BR: TRect;
  X, Y: Integer;
  SaveItemInfoPushed: Boolean;
  C: TColor;
begin
  DC := Canvas.Handle;
  R := ARect;
  Embedded := ((ItemInfo.ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
    ((ItemInfo.ViewType and TVT_EMBEDDED) = TVT_EMBEDDED);

  InflateRect(R, 1, 1);
  Inc(R.Left);
  with Canvas do
    if ButtonInfo.ButtonType = EBT_DROPDOWN then
    begin
      BtnDisabled := (ButtonInfo.ButtonState and EBDS_DISABLED) <> 0;
      BtnHot := (ButtonInfo.ButtonState and EBDS_HOT) <> 0;
      BtnPressed := (ButtonInfo.ButtonState and EBDS_PRESSED) <> 0;
      if not BtnDisabled then
      begin
        if BtnPressed or BtnHot or Embedded then PaintButton(Canvas, R, ItemInfo)
        else if (ItemInfo.ViewType and VT_TOOLBAR) = VT_TOOLBAR then
        begin
          R := ARect;
          if not Embedded then
          begin
            FrameRectEx(DC, R, clWindow, False);
            C := clWindow;
          end
          else C := GetBtnColor(ItemInfo, ipFrame);
          DrawLineEx(DC, R.Left - 1, R.Top, R.Left - 1, R.Bottom, C);
        end;
      end;
      PaintDropDownArrow(Canvas, R, ItemInfo);
    end
    else if ButtonInfo.ButtonType = EBT_SPIN then
    begin
      BtnDisabled := (ButtonInfo.ButtonState and EBSS_DISABLED) <> 0;
      BtnHot := (ButtonInfo.ButtonState and EBSS_HOT) <> 0;

      { Upper button }
      BR := R;
      BR.Bottom := (R.Top + R.Bottom + 1) div 2;
      BtnPressed := (ButtonInfo.ButtonState and EBSS_UP) <> 0;
      SaveItemInfoPushed := ItemInfo.Pushed;
      ItemInfo.Pushed := BtnPressed;
      if not BtnDisabled then
      begin
        if BtnPressed or BtnHot or Embedded then PaintButton(Canvas, BR, ItemInfo)
        else if (ItemInfo.ViewType and VT_TOOLBAR) = VT_TOOLBAR then
        begin
          BR.Left := ARect.Left; BR.Top := ARect.Top; BR.Right := ARect.Right;
          if not Embedded then
          begin
            FrameRectEx(DC, BR, clWindow, False);
            C := clWindow;
          end
          else C := GetBtnColor(ItemInfo, ipFrame);
          DrawLineEx(DC, BR.Left - 1, BR.Top, BR.Left - 1, BR.Bottom, C);
        end;
      end;
      X := (BR.Left + BR.Right) div 2;
      Y := (BR.Top + BR.Bottom - 1) div 2;
      Pen.Color := GetPartColor(ItemInfo, ipText);
      Brush.Color := Pen.Color;
      Polygon([Point(X - 2, Y + 1), Point(X + 2, Y + 1), Point(X, Y - 1)]);

      { Lower button }
      BR := R;
      BR.Top := (R.Top + R.Bottom) div 2;
      BtnPressed := (ButtonInfo.ButtonState and EBSS_DOWN) <> 0;
      ItemInfo.Pushed := BtnPressed;
      if not BtnDisabled then
      begin
        if BtnPressed or BtnHot or Embedded then PaintButton(Canvas, BR, ItemInfo)
        else if (ItemInfo.ViewType and VT_TOOLBAR) = VT_TOOLBAR then
        begin
          BR.Left := ARect.Left; BR.Bottom := ARect.Bottom; BR.Right := ARect.Right;
          if not Embedded then
          begin
            FrameRectEx(DC, BR, clWindow, False);
            C := clWindow;
          end
          else C := GetBtnColor(ItemInfo, ipFrame);
          DrawLineEx(DC, BR.Left - 1, BR.Top, BR.Left - 1, BR.Bottom, C);
        end;
      end;
      X := (BR.Left + BR.Right) div 2;
      Y := (BR.Top + BR.Bottom) div 2;
      C := GetPartColor(ItemInfo, ipText);
      PolygonEx(DC, [Point(X - 2, Y - 1), Point(X + 2, Y - 1), Point(X, Y + 1)], C, C);

      ItemInfo.Pushed := SaveItemInfoPushed;
    end;
end;

procedure TTBXOffice2003Theme.PaintEditFrame(Canvas: TCanvas;
  const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo);
var
  DC: HDC;
  R: TRect;
  W: Integer;
  Embedded: Boolean;
begin
  DC := Canvas.Handle;
  R := ARect;
  PaintFrame(Canvas, R, ItemInfo);
  W := EditFrameWidth;
  InflateRect(R, -W, -W);
  Embedded := ((ItemInfo.ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
    ((ItemInfo.ViewType and TVT_EMBEDDED) = TVT_EMBEDDED);
  if not (ItemInfo.Enabled or Embedded) then
    FrameRectEx(DC, R, BtnItemColors[bisDisabled, ipText], False);

  with EditInfo do if RightBtnWidth > 0 then Dec(R.Right, RightBtnWidth - 2);

  if ItemInfo.Enabled then
  begin
    if ((ItemInfo.ViewType and VT_TOOLBAR) <> VT_TOOLBAR) and (GetPartColor(ItemInfo, ipFrame) = clNone) then
      FrameRectEx(DC, R, ToolbarColor2, False)
    else
      FrameRectEx(DC, R, clWindow, False);

    InflateRect(R, -1, -1);
    FillRectEx(DC, R, clWindow);
    if ((ItemInfo.ViewType and VT_TOOLBAR) <> VT_TOOLBAR) and (GetPartColor(ItemInfo, ipFrame) = clNone) then
    begin
      R := ARect;
      InflateRect(R, -1, -1);
      FrameRectEx(DC, R, ToolbarColor2, False);
    end;
  end
  else InflateRect(R, -1, -1);

  with EditInfo do if LeftBtnWidth > 0 then Inc(R.Left, LeftBtnWidth - 2);

  if EditInfo.RightBtnWidth > 0 then
  begin
    R := ARect;
    InflateRect(R, -W, -W);
    R.Left := R.Right - EditInfo.RightBtnWidth;
    PaintEditButton(Canvas, R, ItemInfo, EditInfo.RightBtnInfo);
  end;
end;

procedure TTBXOffice2003Theme.PaintDropDownArrow(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  X, Y: Integer;
begin
  with ARect, Canvas do
  begin
    X := (Left + Right) div 2;
    Y := (Top + Bottom) div 2 - 1;
    Pen.Color := GetPartColor(ItemInfo, ipText);
    Brush.Color := Pen.Color;
    if ItemInfo.IsVertical then Polygon([Point(X, Y + 2), Point(X, Y - 2), Point(X - 2, Y)])
    else Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)]);
  end;
end;

procedure TTBXOffice2003Theme.PaintButton(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  DC: HDC;
  R: TRect;
  tmpColor1, tmpColor2: TColor;
begin
  DC := Canvas.Handle;
  with ItemInfo do
  begin
    R := ARect;
    if ((ItemOptions and IO_DESIGNING) <> 0) and not Selected then
    begin
      if ComboPart = cpSplitRight then Dec(R.Left);
      FrameRectEx(DC, R, clNavy, False);
    end
    else
    begin
      FrameRectEx(DC, R, GetBtnColor(ItemInfo, ipFrame), True);
      if (ComboPart = cpSplitLeft) and IsPopupParent then Inc(R.Right);
      if ComboPart = cpSplitRight then Dec(R.Left);

      tmpColor1 := GetBtnColor(ItemInfo, ipBody);
      tmpColor2 := GetBtnColor(ItemInfo, ipBody, True);

      if (ItemInfo.ViewType and VT_POPUP = VT_POPUP) or (ItemInfo.ViewType and VT_TOOLBAR = VT_TOOLBAR) and (ItemInfo.ViewType and TVT_EMBEDDED = TVT_EMBEDDED) then
        FillRectEx(DC, R, tmpColor1)
      else if (tmpColor1 <> clNone) and (tmpColor2 <> clNone) then
        PaintGradient(DC, R, tmpColor1, tmpColor2);
    end;
    if ComboPart = cpSplitRight then PaintDropDownArrow(Canvas, R, ItemInfo);
  end;
end;

procedure TTBXOffice2003Theme.PaintFloatingBorder(Canvas: TCanvas; const ARect: TRect;
  const WindowInfo: TTBXWindowInfo);

  function GetBtnItemState(BtnState: Integer): TBtnItemState;
  begin
    if not WindowInfo.Active then Result := bisDisabled
    else if (BtnState and CDBS_PRESSED) <> 0 then Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0 then Result := bisHot
    else Result := bisNormal;
  end;

var
  BtnItemState: TBtnItemState;
  SaveIndex, X, Y: Integer;
  Sz: TPoint;
  R: TRect;
  BodyColor, CaptionColor, CaptionText: TColor;
  IsDockPanel: Boolean;
begin
  with Canvas do
  begin
    IsDockPanel := (WindowInfo.ViewType and VT_DOCKPANEL) = VT_DOCKPANEL;
    BodyColor := Brush.Color;

    if (WRP_BORDER and WindowInfo.RedrawPart) <> 0 then
    begin
      R := ARect;

      if not IsDockPanel then Brush.Color := WinFrameColors[wfpBorder]
      else Brush.Color := WinFrameColors[wfpBorder];
      
      SaveIndex := SaveDC(Canvas.Handle);
      Sz := WindowInfo.FloatingBorderSize;
      with R, Sz do ExcludeClipRect(Canvas.Handle, Left + X, Top + Y, Right - X, Bottom - Y);
      FillRect(R);
      RestoreDC(Canvas.Handle, SaveIndex);
      InflateRect(R, -Sz.X, -Sz.Y);
      Pen.Color := BodyColor;
      with R do
        if not IsDockPanel then
          Canvas.Polyline([
            Point(Left, Top - 1), Point(Right - 1, Top - 1),
            Point(Right, Top), Point(Right, Bottom - 1),
            Point(Right - 1, Bottom),
            Point(Left, Bottom), Point(Left - 1, Bottom - 1),
            Point(Left - 1, Top), Point(Left, Top - 1)
            ])
        else
          Canvas.Polyline([
            Point(Left, Top - 1), Point(Right - 1, Top - 1),
            Point(Right, Top), Point(Right, Bottom),
            Point(Left - 1, Bottom),
            Point(Left - 1, Top), Point(Left, Top - 1)
            ]);
    end;

    if not WindowInfo.ShowCaption then Exit;

    if (WindowInfo.ViewType and VT_TOOLBAR) = VT_TOOLBAR then
    begin
      CaptionColor := WinFrameColors[wfpCaption];
      CaptionText := WinFrameColors[wfpCaptionText];
    end
    else
    begin
      CaptionColor := WinFrameColors[wfpCaption];
      CaptionText := WinFrameColors[wfpCaptionText];
    end;

    { Caption }
    if (WRP_CAPTION and WindowInfo.RedrawPart) <> 0 then
    begin
      R := Rect(0, 0, WindowInfo.ClientWidth, GetSystemMetrics(SM_CYSMCAPTION) - 1);
      with WindowInfo.FloatingBorderSize do OffsetRect(R, X, Y);
      DrawLineEx(Canvas.Handle, R.Left, R.Bottom, R.Right, R.Bottom, BodyColor);

      if ((CDBS_VISIBLE and WindowInfo.CloseButtonState) <> 0) and
        ((WRP_CLOSEBTN and WindowInfo.RedrawPart) <> 0) then
        Dec(R.Right, GetSystemMetrics(SM_CYSMCAPTION) - 1);

      Brush.Color := CaptionColor;
      FillRect(R);
      InflateRect(R, -2, 0);
      Font.Assign(SmCaptionFont);
      Font.Color := CaptionText;
      DrawText(Canvas.Handle, WindowInfo.Caption, -1, R,
        DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX);
    end;

    { Close button }
    if (CDBS_VISIBLE and WindowInfo.CloseButtonState) <> 0 then
    begin
      R := Rect(0, 0, WindowInfo.ClientWidth, GetSystemMetrics(SM_CYSMCAPTION) - 1);
      with WindowInfo.FloatingBorderSize do OffsetRect(R, X, Y);
      R.Left := R.Right - (R.Bottom - R.Top);
      DrawLineEx(Canvas.Handle, R.Left - 1, R.Bottom, R.Right, R.Bottom, BodyColor);
      Brush.Color := CaptionColor;
      FillRect(R);
      with R do
      begin
        X := (Left + Right - StockImgList.Width + 1) div 2;
        Y := (Top + Bottom - StockImgList.Height) div 2;
      end;
      BtnItemState := GetBtnItemState(WindowInfo.CloseButtonState);
      FrameRectEx(Canvas.Handle, R, BtnItemColors[BtnItemState, ipFrame], True);
      if FillRectEx(Canvas.Handle, R, BtnBodyColors[BtnItemState, False]) then
        DrawGlyph(Canvas.Handle, X, Y, StockImgList, 0, BtnItemColors[BtnItemState, ipText])
      else
        DrawGlyph(Canvas.Handle, X, Y, StockImgList, 0, CaptionText);
    end;
  end;
end;

procedure TTBXOffice2003Theme.PaintFrame(Canvas: TCanvas; const ARect: TRect;
  const ItemInfo: TTBXItemInfo);
var
  DC: HDC;
  R: TRect;
begin
  DC := Canvas.Handle;
  R := ARect;
  FrameRectEx(DC, R, GetPartColor(ItemInfo, ipFrame), True);
  FillRectEx(DC, R, GetPartColor(ItemInfo, ipBody));
end;

function TTBXOffice2003Theme.GetImageOffset(Canvas: TCanvas;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList): TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

procedure TTBXOffice2003Theme.PaintImage(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer);
var
  HiContrast: Boolean;
begin
  with ItemInfo do
  begin
    if ImageList is TTBCustomImageList then
    begin
      TTBCustomImageList(ImageList).DrawState(Canvas, ARect.Left, ARect.Top,
        ImageIndex, Enabled, (HoverKind <> hkNone), Selected);
      Exit;
    end;

    HiContrast := ColorIntensity(GetItemImageBackground(ItemInfo)) < 80;
    if not Enabled then
    begin
      if not HiContrast then
        DrawTBXIconShadow(Canvas, ARect, ImageList, ImageIndex, 0)
      else
        DrawTBXIconFlatShadow(Canvas, ARect, ImageList, ImageIndex, SeparatorColor1);
    end
    else {$IFDEF HIGHLIGHTTOOLBARICONS}if Selected or Pushed or (HoverKind <> hkNone) or HiContrast or TBXHiContrast or TBXLoColor then{$ENDIF}
      DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast)
    {$IFDEF HIGHLIGHTTOOLBARICONS}
    else
      HighlightTBXIcon(Canvas, ARect, ImageList, ImageIndex, clWindow, 178){$ENDIF};
  end;
end;

procedure TTBXOffice2003Theme.PaintMDIButton(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal);
var
  Index: Integer;
begin
  PaintButton(Canvas, ARect, ItemInfo);
  Dec(ARect.Bottom);
  case ButtonKind of
    DFCS_CAPTIONMIN: Index := 2;
    DFCS_CAPTIONRESTORE: Index := 3;
    DFCS_CAPTIONCLOSE: Index := 0;
  else
    Exit;
  end;
  DrawGlyph(Canvas.Handle, ARect, StockImgList, Index, GetPartColor(ItemInfo, ipText));
end;

procedure TTBXOffice2003Theme.PaintMenuItemFrame(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  R: TRect;
begin
  R := ARect;
  if (ItemInfo.ViewType and PVT_TOOLBOX) <> PVT_TOOLBOX then with Canvas do
  begin
    R.Right := R.Left + ItemInfo.PopupMargin + 2;
    PaintIrregularGradient(Handle, R, ToolbarColor1, ToolbarColor2, True);
    Inc(R.Left);
    R.Right := ARect.Right - 1;
  end;
  PaintFrame(Canvas, R, ItemInfo);
end;

procedure TTBXOffice2003Theme.PaintMenuItem(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo);
var
  DC: HDC;
  R: TRect;
  X, Y: Integer;
  ArrowWidth: Integer;
  C, ClrText: TColor;
begin
  DC := Canvas.Handle;
  with ItemInfo do
  begin
    ArrowWidth := GetSystemMetrics(SM_CXMENUCHECK);
    PaintMenuItemFrame(Canvas, ARect, ItemInfo);
    ClrText := GetPartColor(ItemInfo, ipText);
    R := ARect;

    if (ItemOptions and IO_COMBO) <> 0 then
    begin
      X := R.Right - ArrowWidth - 1;
      if not ItemInfo.Enabled then C := ClrText
      else if HoverKind = hkMouseHover then C := GetPartColor(ItemInfo, ipFrame)
      else C := PopupFrameColor;
      DrawLineEx(DC, X, R.Top + 1, X, R.Bottom - 1, C);
    end;

    if (ItemOptions and IO_SUBMENUITEM) <> 0 then
    begin
      Y := ARect.Bottom div 2;
      X := ARect.Right - ArrowWidth * 2 div 3 - 1;
      PolygonEx(DC, [Point(X, Y - 3), Point(X, Y + 3), Point(X + 3, Y)], ClrText, ClrText);
    end;

    if Selected and Enabled then
    begin
      R := ARect;
      R.Left := ARect.Left + 1;
      R.Right := R.Left + ItemInfo.PopupMargin;
      InflateRect(R, -1, -1);
      FrameRectEx(DC, R, GetBtnColor(ItemInfo, ipFrame), True);
      FillRectEx(DC, R, GetBtnColor(ItemInfo, ipBody));
    end;
  end;
end;

procedure TTBXOffice2003Theme.PaintPopupNCArea(Canvas: TCanvas; R: TRect; const PopupInfo: TTBXPopupInfo);
var
  PR: TRect;
begin
  with Canvas do
  begin
    Brush.Color := PopupFrameColor;
    FrameRect(R);
    InflateRect(R, -1, -1);
    Brush.Color := PopupColor;
    FillRect(R);

    if not IsRectEmpty(PopupInfo.ParentRect) then
    begin
      PR := PopupInfo.ParentRect;
      if not IsRectEmpty(PR) then with PR do
      begin
        Pen.Color := PopupColor;
        if Bottom = R.Top then
        begin
          if Left <= R.Left then Left := R.Left - 1;
          if Right >= R.Right then Right := R.Right + 1;
          MoveTo(Left + 1, Bottom - 1); LineTo(Right - 1, Bottom - 1);
        end
        else if Top = R.Bottom then
        begin
          if Left <= R.Left then Left := R.Left - 1;
          if Right >= R.Right then Right := R.Right + 1;
          MoveTo(Left + 1, Top); LineTo(Right - 1, Top);
        end;
        if Right = R.Left then
        begin
          if Top <= R.Top then Top := R.Top - 1;
          if Bottom >= R.Bottom then Bottom := R.Bottom + 1;
          MoveTo(Right - 1, Top + 1); LineTo(Right - 1, Bottom - 1);
        end
        else if Left = R.Right then
        begin
          if Top <= R.Top then Top := R.Top - 1;
          if Bottom >= R.Bottom then Bottom := R.Bottom + 1;
          MoveTo(Left, Top + 1); LineTo(Left, Bottom - 1);
        end;
      end;
    end;
  end;
end;

procedure TTBXOffice2003Theme.PaintSeparator(Canvas: TCanvas; ARect: TRect;
  ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean);
var
  DC: HDC;
  IsToolbox: Boolean;
  R: TRect;
  NewWidth: Integer;
begin
  { Note: for blank separators, Enabled = False }
  DC := Canvas.Handle;
  with ItemInfo, ARect do
  begin
    if Horizontal then
    begin
      IsToolbox := (ViewType and PVT_TOOLBOX) = PVT_TOOLBOX;
      if ((ItemOptions and IO_TOOLBARSTYLE) = 0) and not IsToolBox then
      begin
        R := ARect;
        R.Right := ItemInfo.PopupMargin + 2;
        PaintIrregularGradient(DC, R, ToolbarColor1, ToolbarColor2, True);
        Inc(Left, ItemInfo.PopupMargin + 9);
        Top := (Top + Bottom) div 2;
        if Enabled then DrawLineEx(DC, Left, Top, Right, Top, SeparatorColor1);
      end
      else
      begin
        Top := (Top + Bottom) div 2;
        Right := Right + 1;
        NewWidth := Round(0.6 * (Right - Left));
        Left := (Right - Left - NewWidth) div 2;
        Right := Left + NewWidth;
        if Enabled then
        begin
          DrawLineEx(DC, Left, Top, Right, Top, SeparatorColor1);
          OffsetRect(ARect, 1, 1);
          DrawLineEx(DC, Left, Top, Right, Top, SeparatorColor2);
        end;
      end;
    end
    else if Enabled then
    begin
      Left := (Left + Right) div 2;
      Bottom := Bottom + 1;
      NewWidth := Round(0.6 * (Bottom - Top));
      Top := (Bottom - Top - NewWidth) div 2;
      Bottom := Top + NewWidth;
      DrawLineEx(DC, Left, Top, Left, Bottom, SeparatorColor1);
      OffsetRect(ARect, 1, 1);
      DrawLineEx(DC, Left, Top, Left, Bottom, SeparatorColor2);
    end;
  end;
end;

procedure DrawButtonBitmap(DC: HDC; R: TRect; Color: TColor);
const
{$IFNDEF SMALL_CLOSE_BUTTON}
  Pattern: array [0..15] of Byte =
    ($C3, 0, $66, 0, $3C, 0, $18, 0, $3C, 0, $66, 0, $C3, 0, 0, 0);
{$ELSE}
  Pattern: array [0..15] of Byte =
    (0, 0, $63, 0, $36, 0, $1C, 0, $1C, 0, $36, 0, $63, 0, 0, 0);
{$ENDIF}
begin
  DrawGlyph(DC, R, 8, 7, Pattern[0], Color);
end;

procedure TTBXOffice2003Theme.PaintToolbarNCArea(Canvas: TCanvas; R: TRect; const ToolbarInfo: TTBXToolbarInfo);
const
  DragHandleOffsets: array [Boolean, DHS_DOUBLE..DHS_SINGLE] of Integer = ((2, 0, 1), (5, 0, 5));

  function GetBtnItemState(BtnState: Integer): TBtnItemState;
  begin
    if (BtnState and CDBS_PRESSED) <> 0 then Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0 then Result := bisHot
    else Result := bisNormal;
  end;

  procedure DrawHandleElement(const P: TPoint);
  begin
    Canvas.Brush.Color := DragHandleColor2;
    Canvas.FillRect(Rect(P.X + 1, P.Y + 1, P.X + 3, P.Y + 3));
    Canvas.Brush.Color := DragHandleColor1;
    Canvas.FillRect(Rect(P.X, P.Y, P.X + 2, P.Y + 2));
  end;

var
  DC: HDC;
  Sz: Integer;
  R2: TRect;
  I: Integer;
  BtnVisible, Horz: Boolean;
  BtnItemState: TBtnItemState;
  IsMenuBar: Boolean;
begin
  DC := Canvas.Handle;
  Horz := not ToolbarInfo.IsVertical;
  with Canvas do
  begin
    IsMenuBar := ToolbarInfo.ViewType and TVT_MENUBAR = TVT_MENUBAR;
    if (ToolbarInfo.BorderStyle = bsSingle) and not IsMenuBar then
    begin
      I := ColorIntensity(clBtnFace);
      if not (TBXLoColor or not (I in [50..254])) then
      begin
        InflateRect(R, -1, -1);
        Pen.Style := psSolid;
        with R do
        begin
          if Horz then
          begin
            {left}
            PaintIrregularGradient(Handle, Rect(Left, Top + 1, Left + 1, Bottom - 1), ToolbarColor1, ToolbarColor2, False);

            {right}
            PaintIrregularGradient(Handle, Rect(Right - 1, Top + 1, Right, Bottom - 1), ToolbarColor1, ToolbarColor2, False);
            PaintIrregularGradient(Handle, Rect(Right, Top + 1, Right + 1, Bottom - 1), ToolbarColor2, ToolbarFrameColor1, False);

            {top}
            Pen.Color := ToolbarColor1;
            Polyline([Point(Left + 2, Top), Point(Right - 1, Top)]);

            {bottom}
            Pen.Color := ToolbarColor2;
            Polyline([Point(Left + 1, Bottom - 1), Point(Right - 1, Bottom - 1)]);
            Pen.Color := ToolbarFrameColor1;
            Polyline([Point(Left + 2, Bottom), Point(Right - 1, Bottom)]);

            {pixels}
            SetPixelV(Handle, Left + 1, Top, ToolbarFrameColor2);
            SetPixelV(Handle, Left, Top + 1, ToolbarFrameColor2);
            SetPixelV(Handle, Right - 1, Top, ToolbarFrameColor2);
            SetPixelV(Handle, Right, Top + 1, ToolbarFrameColor2);
            SetPixelV(Handle, Right - 1, Bottom - 1, ToolbarFrameColor1);
          end
          else
          begin
            {left}
            Pen.Color := ToolbarColor1;
            Polyline([Point(Left, Top + 2), Point(Left, Bottom - 1)]);

            {right}
            Pen.Color := ToolbarColor2;
            Polyline([Point(Right - 1, Top + 1), Point(Right - 1, Bottom - 1)]);
            Pen.Color := ToolbarFrameColor1;
            Polyline([Point(Right, Top + 2), Point(Right, Bottom - 1)]);

            {top}
            PaintIrregularGradient(Handle, Rect(Left + 1, Top, Right - 1, Top + 1), ToolbarColor1, ToolbarColor2, True);

            {bottom}
            PaintIrregularGradient(Handle, Rect(Left + 2, Bottom - 1, Right - 1, Bottom), ToolbarColor1, ToolbarColor2, True);
            PaintIrregularGradient(Handle, Rect(Left + 2, Bottom, Right - 1, Bottom + 1), ToolbarColor2, ToolbarFrameColor1, True);

            {pixels}
            SetPixelV(Handle, Left + 1, Top, ToolbarFrameColor2);
            SetPixelV(Handle, Left, Top + 1, ToolbarFrameColor2);
            SetPixelV(Handle, Left + 1, Bottom - 1, ToolbarColor2);
            SetPixelV(Handle, Left, Bottom - 2, ToolbarFrameColor2);
            SetPixelV(Handle, Right - 1, Bottom - 1, ToolbarFrameColor1);
          end;
        end;
        Brush.Style := bsSolid;
        Inc(R.Left);
        Inc(R.Top);
      end
      else
      begin
        Brush.Bitmap := AllocPatternBitmap(ToolbarColor1, BtnItemColors[bisDisabled, ipText]);
        with R do
        begin
          FillRect(Rect(Left + 1, Top, Right - 1, Top + 1));
          FillRect(Rect(Left + 1, Bottom - 1, Right - 1, Bottom));
          FillRect(Rect(Left, Top + 1, Left + 1, Bottom - 1));
          FillRect(Rect(Right - 1, Top + 1, Right, Bottom - 1));
        end;
        InflateRect(R, -1, -1);     
        Brush.Color := ToolbarColor1;
        FillRect(R);
      end;
    end
    else
      InflateRect(R, -1, -1);
    InflateRect(R, -1, -1);

    if not ToolbarInfo.AllowDrag then Exit;

    BtnVisible := (ToolbarInfo.CloseButtonState and CDBS_VISIBLE) <> 0;
    Sz := GetTBXDragHandleSize(ToolbarInfo);
    if Horz then R.Right := R.Left + Sz
    else R.Bottom := R.Top + Sz;

    { Drag Handle }
    if ToolbarInfo.DragHandleStyle <> DHS_NONE then
    begin
      R2 := R;
      if Horz then
      begin
        Inc(R2.Left, DragHandleOffsets[BtnVisible, ToolbarInfo.DragHandleStyle]);
        if BtnVisible then Inc(R2.Top, Sz - 2);
        R2.Right := R2.Left + 3;
      end
      else
      begin
        Inc(R2.Top, DragHandleOffsets[BtnVisible, ToolbarInfo.DragHandleStyle]);
        if BtnVisible then Dec(R2.Right, Sz - 2);
        R2.Bottom := R2.Top + 3;
      end;

      if not IsMenuBar then
        PaintIrregularGradient(Canvas.Handle, Rect(R.Left - 1, R.Top - 1, R.Right, R.Bottom), ToolbarColor1, ToolbarColor2, not Horz)
      else
      begin
        Inc(R2.Left);
        Inc(R2.Top);
      end;
      
      if Horz then
      begin
        I := R2.Top + Sz div 2;
        while I < R2.Bottom - Sz div 2 - 2 do
        begin
          DrawHandleElement(Point(R2.Left, I));
          Inc(I, 4);
        end;
      end
      else
      begin
        I := R2.Left + Sz div 2;
        while I < R2.Right - Sz div 2 - 2 do
        begin
          DrawHandleElement(Point(I, R2.Top));
          Inc(I, 4);
        end;
      end;
    end;

    { Close button }
    if BtnVisible then
    begin
      R2 := R;
      if Horz then
      begin
        Dec(R2.Right);
        R2.Bottom := R2.Top + R2.Right - R2.Left;
      end
      else
      begin
        Dec(R2.Bottom);
        R2.Left := R2.Right - R2.Bottom + R2.Top;
      end;

      BtnItemState := GetBtnItemState(ToolbarInfo.CloseButtonState);
      FrameRectEx(DC, R2, BtnItemColors[BtnItemState, ipFrame], True);
      FillRectEx(DC, R2, BtnBodyColors[BtnItemState, False]);
      DrawButtonBitmap(DC, R2, BtnItemColors[BtnItemState, ipText]);
    end;
  end;
end;

procedure TTBXOffice2003Theme.PaintDock(Canvas: TCanvas; const ClientRect,
  DockRect: TRect; DockPosition: Integer);
begin
  FillRectEx(Canvas.Handle, DockRect, DockColor);
end;

procedure TTBXOffice2003Theme.PaintDockPanelNCArea(Canvas: TCanvas; R: TRect; const DockPanelInfo: TTBXDockPanelInfo);

  function GetBtnItemState(BtnState: Integer): TBtnItemState;
  begin
    if (BtnState and CDBS_PRESSED) <> 0 then Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0 then Result := bisHot
    else Result := bisNormal;
  end;

var
  DC: HDC;
  C: TColor;
  I, Sz, Flags: Integer;
  R2: TRect;
  BtnItemState: TBtnItemState;
  B: HBRUSH;
  OldBkMode: Cardinal;
  OldFont: HFONT;
  OldTextColor: TColorRef;
begin
  DC := Canvas.Handle;
  with DockPanelInfo do
  begin
    I := ColorIntensity(ColorToRGB(clBtnFace));
    R2 := R;
    if not TBXLoColor and (I in [64..250]) then
    begin
      FrameRectEx(DC, R, ToolbarColor2, True);
      FrameRectEx(DC, R, EffectiveColor, False);
      with R do
      begin
        C := ToolbarColor2;
        SetPixelV(DC, Left, Top, C);
        if IsVertical then SetPixelV(DC, Right - 1, Top, C)
        else SetPixelV(DC, Left, Bottom - 1, C);
      end;
    end
    else
    begin
      FrameRectEx(DC, R, EffectiveColor, True);

      if I < 64 then B := CreateDitheredBrush(EffectiveColor, clWhite)
      else B := CreateDitheredBrush(EffectiveColor, clBtnShadow);
      Windows.FrameRect(DC, R, B);
      DeleteObject(B);

      with R do
      begin
        SetPixelV(DC, Left, Top, EffectiveColor);
        if IsVertical then SetPixelV(DC, Right -  1, Top, EffectiveColor)
        else SetPixelV(DC, Left, Bottom - 1, EffectiveColor);
      end;
      InflateRect(R, -1, -1);
      FrameRectEx(DC, R, EffectiveColor, False);
    end;
    R := R2;
    InflateRect(R, -BorderSize.X, -BorderSize.Y);
    Sz := GetSystemMetrics(SM_CYSMCAPTION);
    if IsVertical then
    begin
      R.Bottom := R.Top + Sz - 1;
      DrawLineEx(DC, R.Left, R.Bottom, R.Right, R.Bottom, EffectiveColor);
    end
    else
    begin
      R.Right := R.Left + Sz - 1;
      DrawLineEx(DC, R.Right, R.Top, R.Right, R.Bottom, EffectiveColor);
    end;
    {$IFDEF DOCKPANELGRADIENTCAPTION}
    PaintGradient(DC, R, ToolbarColor1, ToolbarColor2);
    {$ELSE}
    FillRectEx(DC, R, DockColor);
    {$ENDIF}

    if (CDBS_VISIBLE and CloseButtonState) <> 0 then
    begin
      R2 := R;
      if IsVertical then
      begin
        R2.Left := R2.Right - Sz + 1;
        R.Right := R2.Left;
      end
      else
      begin
        R2.Top := R2.Bottom - Sz + 1;
        R.Bottom := R2.Top;
      end;

      BtnItemState := GetBtnItemState(CloseButtonState);
      FrameRectEx(DC, R2, BtnItemColors[BtnItemState, ipFrame], True);
      FillRectEx(DC, R2, BtnBodyColors[BtnItemState, False]);
      DrawButtonBitmap(DC, R2, BtnItemColors[BtnItemState, ipText]);
    end;

    if IsVertical then InflateRect(R, -4, 0)
    else InflateRect(R, 0, -4);

    OldFont := SelectObject(DC, SmCaptionFont.Handle);
    OldBkMode := SetBkMode(DC, TRANSPARENT);
    OldTextColor := SetTextColor(DC, ColorToRGB(SmCaptionFont.Color));
    Flags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX;
    if IsVertical then DrawText(DC, Caption, -1, R, Flags)
    else DrawRotatedText(DC, string(Caption), R, Flags);
    SetTextColor(DC, OldTextColor);
    SetBkMode(DC, OldBkMode);
    SelectObject(DC, OldFont);
  end;
end;

procedure TTBXOffice2003Theme.SetupColorCache;
const
  Office2003Colors: array[osBlue..osGreen, 0..27] of TColor = (
    ($F5BE9E, $FEECDD, $E2A981, $9C613B, $FAD5BD, $CB8C6A, $FFF9F1, $764127,
     $FFFFFF, $F0C7A9, $B99D7F, $DEDEDE, $F6F6F6, $962D00, $000000, $8D8D8D,
     $800000, $C9662A, $8CD5FF, $55ADFF, $4E91FE, $8ED3FF, $CCF4FF, $91D0FF,
     $FFEFE3, $E7B593, $C2EEFF, $AE9990),
    ($E5D7D7, $FAF4F3, $B59799, $947C7C, $EFE5E5, $8F6D6E, $FFFFFF, $755454,
     $FFFFFF, $D3C0C0, $B2ACA5, $DEDEDE, $FFFAFD, $947C7C, $000000, $8D8D8D,
     $6F4B4B, $99797A, $8CD5FF, $55ADFF, $4E91FE, $8ED3FF, $CCF4FF, $91D0FF,
     $F1E9E8, $CDB9BA, $C2EEFF, $B39A9B),
    ($A7D9D9, $DEF7F4, $91C6B7, $588060, $C0E7E5, $588060, $DEF7F4, $335E51,
     $FFFFFF, $9FD4C5, $7FB9A4, $DEDEDE, $EEF4F4, $5E8D75, $000000, $8D8D8D,
     $385D3F, $5E8674, $8CD5FF, $55ADFF, $4E91FE, $8ED3FF, $CCF4FF, $91D0FF,
     $D5F0EC, $9FCEC2, $C2EEFF, $7BA097) 
  );
  Office2003IrregularGradientValues: array[TOffice2003Scheme] of Integer = (8, 8, 25, 15);

var
  DC: HDC;
  MenuItemFrame, EnabledText, DisabledText: TColor;
  Scheme: TOffice2003Scheme;

  procedure Undither(var C: TColor);
  begin
    if C <> clNone then C := GetNearestColor(DC, ColorToRGB(C));
  end;

begin
  DC := StockCompatibleBitmap.Canvas.Handle;

  if TBXLoColor then
  begin
    DockColor             := clBtnFace;

    ToolbarColor1         := clBtnFace;
    ToolbarColor2         := clBtnFace;
    ToolbarFrameColor1    := clBtnShadow;
    ToolbarFrameColor2    := clBtnShadow;
    SeparatorColor1       := clBtnShadow;
    SeparatorColor2       := clBtnHighlight;
    DragHandleColor1      := clBtnText;
    DragHandleColor2      := clBtnHighlight;

    EmbeddedColor         := clBtnFace;
    EmbeddedFrameColor    := clBtnShadow;
    EmbeddedDisabledColor := clBtnFace;

    PopupColor            := clWindow;
    PopupFrameColor       := clBtnText;

    DockPanelColor        := clWindow;

    DisabledText          := clBtnShadow;
    MenuItemFrame         := clHighlight;

    WinFrameColors[wfpBorder]      := clBtnShadow;
    WinFrameColors[wfpCaption]     := clBtnShadow;
    WinFrameColors[wfpCaptionText] := clBtnHighlight;

    BtnItemColors[bisNormal, ipText]        := clBtnText;
    BtnItemColors[bisNormal, ipFrame]       := clNone;
    BtnItemColors[bisDisabled, ipText]      := DisabledText;
    BtnItemColors[bisDisabled, ipFrame]     := clNone;
    BtnItemColors[bisSelected, ipText]      := clWindowText;
    BtnItemColors[bisSelected, ipFrame]     := MenuItemFrame;
    BtnItemColors[bisPressed, ipText]       := clHighlightText;
    BtnItemColors[bisPressed, ipFrame]      := MenuItemFrame;
    BtnItemColors[bisHot, ipText]           := clWindowText;
    BtnItemColors[bisHot, ipFrame]          := MenuItemFrame;
    BtnItemColors[bisDisabledHot, ipText]   := DisabledText;
    BtnItemColors[bisDisabledHot, ipFrame]  := MenuItemFrame;
    BtnItemColors[bisSelectedHot, ipText]   := clHighlightText;
    BtnItemColors[bisSelectedHot, ipFrame]  := MenuItemFrame;
    BtnItemColors[bisPopupParent, ipText]   := clBtnText;
    BtnItemColors[bisPopupParent, ipFrame]  := PopupFrameColor;

    BtnBodyColors[bisNormal, False]         := clNone;
    BtnBodyColors[bisNormal, True]          := clNone;
    BtnBodyColors[bisDisabled, False]       := clNone;
    BtnBodyColors[bisDisabled, True]        := clNone;
    BtnBodyColors[bisSelected, False]       := clWindow;
    BtnBodyColors[bisSelected, True]        := clWindow;
    BtnBodyColors[bisPressed, False]        := clHighlight;
    BtnBodyColors[bisPressed, True]         := clHighlight;
    BtnBodyColors[bisHot, False]            := clWindow;
    BtnBodyColors[bisHot, True]             := clWindow;
    BtnBodyColors[bisDisabledHot, False]    := clWindow;
    BtnBodyColors[bisDisabledHot, True]     := clWindow;
    BtnBodyColors[bisSelectedHot, False]    := clHighlight;
    BtnBodyColors[bisSelectedHot, True]     := clHighlight;
    BtnBodyColors[bisPopupParent, False]    := clBtnFace;
    BtnBodyColors[bisPopupParent, True]     := clBtnFace;

    MenuItemColors[misNormal, ipBody]       := clNone;
    MenuItemColors[misNormal, ipText]       := clWindowText;
    MenuItemColors[misNormal, ipFrame]      := clNone;
    MenuItemColors[misDisabled, ipBody]     := clNone;
    MenuItemColors[misDisabled, ipText]     := clGrayText;
    MenuItemColors[misDisabled, ipFrame]    := clNone;
    MenuItemColors[misHot, ipBody]          := clWindow;
    MenuItemColors[misHot, ipText]          := clWindowText;
    MenuItemColors[misHot, ipFrame]         := MenuItemFrame;
    MenuItemColors[misDisabledHot, ipBody]  := PopupColor;
    MenuItemColors[misDisabledHot, ipText]  := clGrayText;
    MenuItemColors[misDisabledHot, ipFrame] := MenuItemFrame;

    StatusPanelFrameColor := clBtnShadow;
    FMDIAreaColor := clBtnShadow;
  end
  else
  begin

    Scheme := GetOffice2003Scheme;
    if Scheme <> osUnknown then
    begin
      DockColor             := Office2003Colors[Scheme, 0];

      ToolbarColor1         := Office2003Colors[Scheme, 1];
      ToolbarColor2         := Office2003Colors[Scheme, 2];
      ToolbarFrameColor1    := Office2003Colors[Scheme, 3];
      ToolbarFrameColor2    := Office2003Colors[Scheme, 4];
      SeparatorColor1       := Office2003Colors[Scheme, 5];
      SeparatorColor2       := Office2003Colors[Scheme, 6];
      DragHandleColor1      := Office2003Colors[Scheme, 7];
      DragHandleColor2      := Office2003Colors[Scheme, 8];

      EmbeddedColor         := Office2003Colors[Scheme, 9];
      EmbeddedFrameColor    := Office2003Colors[Scheme, 10];
      EmbeddedDisabledColor := Office2003Colors[Scheme, 11];

      PopupColor            := Office2003Colors[Scheme, 12];
      PopupFrameColor       := Office2003Colors[Scheme, 13];

      DockPanelColor        := ToolbarColor1;

      EnabledText           := Office2003Colors[Scheme, 14];
      DisabledText          := Office2003Colors[Scheme, 15];
      MenuItemFrame         := Office2003Colors[Scheme, 16];

      WinFrameColors[wfpBorder]      := Office2003Colors[Scheme, 17];
      WinFrameColors[wfpCaption]     := WinFrameColors[wfpBorder];
      WinFrameColors[wfpCaptionText] := clWhite;

      BtnItemColors[bisNormal, ipText]        := EnabledText;
      BtnItemColors[bisNormal, ipFrame]       := clNone;
      BtnItemColors[bisDisabled, ipText]      := DisabledText;
      BtnItemColors[bisDisabled, ipFrame]     := clNone;
      BtnItemColors[bisSelected, ipText]      := EnabledText;
      BtnItemColors[bisSelected, ipFrame]     := MenuItemFrame;
      BtnItemColors[bisPressed, ipText]       := EnabledText;
      BtnItemColors[bisPressed, ipFrame]      := MenuItemFrame;
      BtnItemColors[bisHot, ipText]           := EnabledText;
      BtnItemColors[bisHot, ipFrame]          := MenuItemFrame;
      BtnItemColors[bisDisabledHot, ipText]   := DisabledText;
      BtnItemColors[bisDisabledHot, ipFrame]  := MenuItemFrame;
      BtnItemColors[bisSelectedHot, ipText]   := EnabledText;
      BtnItemColors[bisSelectedHot, ipFrame]  := MenuItemFrame;
      BtnItemColors[bisPopupParent, ipText]   := EnabledText;
      BtnItemColors[bisPopupParent, ipFrame]  := PopupFrameColor;

      BtnBodyColors[bisNormal, False]         := clNone;
      BtnBodyColors[bisNormal, True]          := clNone;
      BtnBodyColors[bisDisabled, False]       := clNone;
      BtnBodyColors[bisDisabled, True]        := clNone;
      BtnBodyColors[bisSelected, False]       := Office2003Colors[Scheme, 18];
      BtnBodyColors[bisSelected, True]        := Office2003Colors[Scheme, 19];
      BtnBodyColors[bisPressed, False]        := Office2003Colors[Scheme, 20];
      BtnBodyColors[bisPressed, True]         := Office2003Colors[Scheme, 21];
      BtnBodyColors[bisHot, False]            := Office2003Colors[Scheme, 22];
      BtnBodyColors[bisHot, True]             := Office2003Colors[Scheme, 23];
      BtnBodyColors[bisDisabledHot, False]    := BtnBodyColors[bisHot, False];
      BtnBodyColors[bisDisabledHot, True]     := BtnBodyColors[bisHot, True];
      BtnBodyColors[bisSelectedHot, False]    := BtnBodyColors[bisPressed, False];
      BtnBodyColors[bisSelectedHot, True]     := BtnBodyColors[bisPressed, True];
      BtnBodyColors[bisPopupParent, False]    := Office2003Colors[Scheme, 24];
      BtnBodyColors[bisPopupParent, True]     := Office2003Colors[Scheme, 25];

      MenuItemColors[misNormal, ipBody]       := clNone;
      MenuItemColors[misNormal, ipText]       := EnabledText;
      MenuItemColors[misNormal, ipFrame]      := clNone;
      MenuItemColors[misDisabled, ipBody]     := clNone;
      MenuItemColors[misDisabled, ipText]     := DisabledText;
      MenuItemColors[misDisabled, ipFrame]    := clNone;
      MenuItemColors[misHot, ipBody]          := Office2003Colors[Scheme, 26];
      MenuItemColors[misHot, ipText]          := MenuItemColors[misNormal, ipText];
      MenuItemColors[misHot, ipFrame]         := MenuItemFrame;
      MenuItemColors[misDisabledHot, ipBody]  := PopupColor;
      MenuItemColors[misDisabledHot, ipText]  := DisabledText;
      MenuItemColors[misDisabledHot, ipFrame] := MenuItemColors[misHot, ipFrame];

      StatusPanelFrameColor := Blend(ToolbarColor2, clBlack, 90);
      FMDIAreaColor := Office2003Colors[Scheme, 27];
    end
    else
    begin
      DockColor             := clBtnFace;

      ToolbarColor1         := Blend(clBtnFace, clWindow, 23);
      ToolbarColor2         := Blend(clBtnFace, clWindow, 96);
      ToolbarFrameColor1    := Blend(clBtnFace, clWindow, 85);
      ToolbarFrameColor2    := Blend(clBtnFace, clWindow, 62);
      SeparatorColor1       := Blend(clBtnShadow, clWindow, 70);
      SeparatorColor2       := clWhite;
      DragHandleColor1      := Blend(clBtnShadow, clWindow, 75);
      DragHandleColor2      := clWindow;

      EmbeddedColor         := clBtnFace;
      EmbeddedFrameColor    := clBtnShadow;
      EmbeddedDisabledColor := clBtnFace;

      PopupColor            := Blend(clBtnFace, clWindow, 15);
      PopupFrameColor       := Blend(clBtnText, clBtnShadow, 20);

      DockPanelColor        := ToolbarColor1;

      EnabledText           := clBtnText;
      DisabledText          := clGrayText;
      MenuItemFrame         := clHighlight;

      WinFrameColors[wfpBorder]      := Blend(clBtnText, clBtnShadow, 15);
      WinFrameColors[wfpCaption]     := clBtnShadow;
      WinFrameColors[wfpCaptionText] := clBtnHighlight;

      BtnBodyColors[bisNormal, False]         := clNone;
      BtnBodyColors[bisNormal, True]          := clNone;
      BtnBodyColors[bisDisabled, False]       := clNone;
      BtnBodyColors[bisDisabled, True]        := clNone;
      BtnBodyColors[bisSelected, False]       := Blend(clHighlight, Blend(clBtnFace, clWindow, 50), 10);

      BtnItemColors[bisNormal, ipText]        := EnabledText;
      BtnItemColors[bisNormal, ipFrame]       := clNone;
      BtnItemColors[bisDisabled, ipText]      := DisabledText;
      BtnItemColors[bisDisabled, ipFrame]     := clNone;
      BtnItemColors[bisSelected, ipText]      := EnabledText;
      BtnItemColors[bisSelected, ipFrame]     := MenuItemFrame;
      BtnItemColors[bisPressed, ipText]       := EnabledText;
      BtnItemColors[bisPressed, ipFrame]      := MenuItemFrame;
      BtnItemColors[bisHot, ipText]           := EnabledText;
      BtnItemColors[bisHot, ipFrame]          := MenuItemFrame;
      BtnItemColors[bisDisabledHot, ipText]   := DisabledText;
      BtnItemColors[bisDisabledHot, ipFrame]  := MenuItemFrame;
      BtnItemColors[bisSelectedHot, ipText]   := EnabledText;
      BtnItemColors[bisSelectedHot, ipFrame]  := MenuItemFrame;
      BtnItemColors[bisPopupParent, ipText]   := EnabledText;
      BtnItemColors[bisPopupParent, ipFrame]  := PopupFrameColor;

      BtnBodyColors[bisSelected, True]        := BtnBodyColors[bisSelected, False];
      BtnBodyColors[bisPressed, False]        := Blend(clHighlight, clWindow, 50);
      BtnBodyColors[bisPressed, True]         := BtnBodyColors[bisPressed, False];
      BtnBodyColors[bisHot, False]            := Blend(clHighlight, clWindow, 30);
      BtnBodyColors[bisHot, True]             := BtnBodyColors[bisHot, False];
      BtnBodyColors[bisDisabledHot, False]    := BtnBodyColors[bisHot, False];
      BtnBodyColors[bisDisabledHot, True]     := BtnBodyColors[bisHot, True];
      BtnBodyColors[bisSelectedHot, False]    := BtnBodyColors[bisPressed, False];
      BtnBodyColors[bisSelectedHot, True]     := BtnBodyColors[bisPressed, True];
      BtnBodyColors[bisPopupParent, False]    := Blend(clBtnFace, clWindow, 16);
      BtnBodyColors[bisPopupParent, True]     := Blend(clBtnFace, clWindow, 42);

      MenuItemColors[misNormal, ipBody]       := clNone;
      MenuItemColors[misNormal, ipText]       := EnabledText;
      MenuItemColors[misNormal, ipFrame]      := clNone;
      MenuItemColors[misDisabled, ipBody]     := clNone;
      MenuItemColors[misDisabled, ipText]     := DisabledText;
      MenuItemColors[misDisabled, ipFrame]    := clNone;
      MenuItemColors[misHot, ipBody]          := BtnBodyColors[bisHot, False];
      MenuItemColors[misHot, ipText]          := MenuItemColors[misNormal, ipText];
      MenuItemColors[misHot, ipFrame]         := MenuItemFrame;
      MenuItemColors[misDisabledHot, ipBody]  := PopupColor;
      MenuItemColors[misDisabledHot, ipText]  := DisabledText;
      MenuItemColors[misDisabledHot, ipFrame] := MenuItemColors[misHot, ipFrame];

      StatusPanelFrameColor := Blend(ToolbarColor2, clBlack, 90);
      FMDIAreaColor := clBtnShadow;
    end;

    IrregularGradientValue := Office2003IrregularGradientValues[Scheme];

    Undither(DockColor);
    
    Undither(ToolbarColor1);
    Undither(ToolbarColor2);
    Undither(ToolbarFrameColor1);
    Undither(ToolbarFrameColor2);
    Undither(SeparatorColor1);
    Undither(SeparatorColor2);
    Undither(DragHandleColor1);
    Undither(DragHandleColor2);

    Undither(EmbeddedColor);
    Undither(EmbeddedFrameColor);
    Undither(EmbeddedDisabledColor);

    Undither(PopupColor);
    Undither(PopupFrameColor);
    
    Undither(DockPanelColor);
    
    Undither(WinFrameColors[wfpBorder]);
    Undither(WinFrameColors[wfpCaption]);
    Undither(WinFrameColors[wfpCaptionText]);
    
    Undither(BtnItemColors[bisNormal, ipText]);
    Undither(BtnItemColors[bisNormal, ipFrame]);
    Undither(BtnItemColors[bisDisabled, ipText]);
    Undither(BtnItemColors[bisDisabled, ipFrame]);
    Undither(BtnItemColors[bisSelected, ipText]);
    Undither(BtnItemColors[bisSelected, ipFrame]);
    Undither(BtnItemColors[bisPressed, ipText]);
    Undither(BtnItemColors[bisPressed, ipFrame]);
    Undither(BtnItemColors[bisHot, ipText]);
    Undither(BtnItemColors[bisHot, ipFrame]);
    Undither(BtnItemColors[bisDisabledHot, ipText]);
    Undither(BtnItemColors[bisDisabledHot, ipFrame]);
    Undither(BtnItemColors[bisSelectedHot, ipText]);
    Undither(BtnItemColors[bisSelectedHot, ipFrame]);
    Undither(BtnItemColors[bisPopupParent, ipText]);
    Undither(BtnItemColors[bisPopupParent, ipFrame]);
    
    Undither(BtnBodyColors[bisNormal, False]);
    Undither(BtnBodyColors[bisNormal, True]);
    Undither(BtnBodyColors[bisDisabled, False]);
    Undither(BtnBodyColors[bisDisabled, True]);
    Undither(BtnBodyColors[bisSelected, False]);
    Undither(BtnBodyColors[bisSelected, True]);
    Undither(BtnBodyColors[bisPressed, False]);
    Undither(BtnBodyColors[bisPressed, True]);
    Undither(BtnBodyColors[bisHot, False]);
    Undither(BtnBodyColors[bisHot, True]);
    Undither(BtnBodyColors[bisDisabledHot, False]);
    Undither(BtnBodyColors[bisDisabledHot, True]);
    Undither(BtnBodyColors[bisSelectedHot, False]);
    Undither(BtnBodyColors[bisSelectedHot, True]);
    Undither(BtnBodyColors[bisPopupParent, False]);
    Undither(BtnBodyColors[bisPopupParent, True]);
    
    Undither(MenuItemColors[misNormal, ipBody]);
    Undither(MenuItemColors[misNormal, ipText]);
    Undither(MenuItemColors[misNormal, ipFrame]);
    Undither(MenuItemColors[misDisabled, ipBody]);
    Undither(MenuItemColors[misDisabled, ipText]);
    Undither(MenuItemColors[misDisabled, ipFrame]);
    Undither(MenuItemColors[misHot, ipBody]);
    Undither(MenuItemColors[misHot, ipText]);
    Undither(MenuItemColors[misHot, ipFrame]);
    Undither(MenuItemColors[misDisabledHot, ipBody]);
    Undither(MenuItemColors[misDisabledHot, ipText]);
    Undither(MenuItemColors[misDisabledHot, ipFrame]);

    Undither(StatusPanelFrameColor);
    Undither(FMDIAreaColor);
  end;
  
  if Assigned(FOnSetupColorCache) then FOnSetupColorCache(Self);
end;

function TTBXOffice2003Theme.GetPopupShadowType: Integer;
begin
  Result := PST_OFFICEXP;
end;

constructor TTBXOffice2003Theme.Create(const AName: string);
begin
  inherited;
  if CounterLock = 0 then InitializeStock;
  Inc(CounterLock);
  AddTBXSysChangeNotification(Self);
  SetupColorCache;
end;

destructor TTBXOffice2003Theme.Destroy;
begin
  RemoveTBXSysChangeNotification(Self);
  Dec(CounterLock);
  if CounterLock = 0 then FinalizeStock;
  inherited;
end;

procedure TTBXOffice2003Theme.GetViewMargins(ViewType: Integer; out Margins: TTBXMargins);
begin
  Margins.LeftWidth := 0;
  Margins.TopHeight := 0;
  Margins.RightWidth := 0;
  Margins.BottomHeight := 0;
end;

procedure TTBXOffice2003Theme.PaintPageScrollButton(Canvas: TCanvas;
  const ARect: TRect; ButtonType: Integer; Hot: Boolean);
var
  DC: HDC;
  R: TRect;
  X, Y, Sz: Integer;
  C: TColor;
begin
  DC := Canvas.Handle;
  R := ARect;
  if Hot then C := BtnItemColors[bisHot, ipFrame]
  else C := EmbeddedFrameColor;
  FrameRectEx(DC, R, C, False);
  InflateRect(R, -1, -1);
  if Hot then C := BtnBodyColors[bisHot, False]
  else C := EmbeddedColor;
  FillRectEx(DC, R, C);
  X := (R.Left + R.Right) div 2;
  Y := (R.Top + R.Bottom) div 2;
  Sz := Min(X - R.Left, Y - R.Top) * 3 div 4;

  if Hot then C := BtnItemColors[bisHot, ipText]
  else C := BtnItemColors[bisNormal, ipText];

  case ButtonType of
    PSBT_UP:
      begin
        Inc(Y, Sz div 2);
        PolygonEx(DC, [Point(X + Sz, Y), Point(X, Y - Sz), Point(X - Sz, Y)], C, C);
      end;
    PSBT_DOWN:
      begin
        Y := (R.Top + R.Bottom - 1) div 2;
        Dec(Y, Sz div 2);
        PolygonEx(DC, [Point(X + Sz, Y), Point(X, Y + Sz), Point(X - Sz, Y)], C, C);
      end;
    PSBT_LEFT:
      begin
        Inc(X, Sz div 2);
        PolygonEx(DC, [Point(X, Y + Sz), Point(X - Sz, Y), Point(X, Y - Sz)], C, C);
      end;
    PSBT_RIGHT:
      begin
        X := (R.Left + R.Right - 1) div 2;
        Dec(X, Sz div 2);
        PolygonEx(DC, [Point(X, Y + Sz), Point(X + Sz, Y), Point(X, Y - Sz)], C, C);
      end;
  end;
end;

procedure TTBXOffice2003Theme.PaintFrameControl(Canvas: TCanvas; R: TRect; Kind, State: Integer; Params: Pointer);
var
  DC: HDC;
  X, Y: Integer;
  Pen, OldPen: HPen;
  Brush, OldBrush: HBRUSH;
  C: TColor;

  function GetPenColor: TColor;
  begin
    if Boolean(State and PFS_DISABLED) then Result := clBtnShadow
    else if Boolean(State and PFS_PUSHED) then Result := BtnItemColors[bisPressed, ipFrame]
    else if Boolean(State and PFS_HOT) then Result := BtnItemColors[bisHot, ipFrame]
    else Result := EmbeddedFrameColor;
  end;

  function GetBrush: HBRUSH;
  begin
    if Boolean(State and PFS_DISABLED) then Result := CreateDitheredBrush(clWindow, clBtnFace)
    else if Boolean(State and PFS_PUSHED) then Result := CreateBrushEx(BtnBodyColors[bisPressed, False])
    else if Boolean(State and PFS_HOT) then Result := CreateBrushEx(BtnBodyColors[bisHot, False])
    else if Boolean(State and PFS_MIXED) then Result := CreateDitheredBrush(clWindow, clBtnFace)
    else Result := CreateBrushEx(clNone);
  end;

  function GetTextColor: TColor;
  begin
    if Boolean(State and PFS_DISABLED) then Result := BtnItemColors[bisDisabled, ipText]
    else if Boolean(State and PFS_PUSHED) then Result := BtnItemColors[bisPressed, ipText]
    else if Boolean(State and PFS_MIXED) then Result := clBtnShadow
    else if Boolean(State and PFS_HOT) then Result := BtnItemColors[bisHot, ipText]
    else Result := BtnItemColors[bisNormal, ipText];
  end;

begin
  DC := Canvas.Handle;
  case Kind of
    PFC_CHECKBOX:
      begin
        InflateRect(R, -1, -1);
        FrameRectEx(DC, R, GetPenColor, True);
        Brush := GetBrush;
        Windows.FillRect(DC, R, Brush);
        DeleteObject(Brush);
        InflateRect(R, 1, 1);

        if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
        begin
          X := (R.Left + R.Right) div 2 - 1;
          Y := (R.Top + R.Bottom) div 2 + 1;
          C := GetTextColor;
          PolygonEx(DC, [Point(X - 2, Y), Point(X, Y + 2), Point(X + 4, Y - 2),
            Point(X + 4, Y - 4), Point(X, Y), Point(X - 2, Y - 2), Point(X - 2, Y)], C, C);
        end;
      end;
    PFC_RADIOBUTTON:
      begin
        InflateRect(R, -1, -1);

        with R do
        begin
          Brush := GetBrush;
          OldBrush := SelectObject(DC, Brush);
          Pen := CreatePenEx(GetPenColor);
          OldPen := SelectObject(DC, Pen);
          Windows.Ellipse(DC, Left, Top, Right, Bottom);
          SelectObject(DC, OldPen);
          DeleteObject(Pen);
          SelectObject(DC, OldBrush);
          DeleteObject(Brush);
        end;

        if Boolean(State and PFS_CHECKED) then
        begin
          InflateRect(R, -3, -3);
          C := GetTextColor;
          Brush := CreateBrushEx(C);
          OldBrush := SelectObject(DC, Brush);
          Pen := CreatePenEx(C);
          OldPen := SelectObject(DC, Pen);
          with R do Windows.Ellipse(DC, Left, Top, Right, Bottom);
          SelectObject(DC, OldPen);
          DeleteObject(Pen);
          SelectObject(DC, OldBrush);
          DeleteObject(Brush);
        end;
      end;
  end;
end;

procedure TTBXOffice2003Theme.PaintStatusBar(Canvas: TCanvas; R: TRect; Part: Integer);

  procedure DrawHandleElement(const P: TPoint);
  begin
    Canvas.Brush.Color := DragHandleColor2;
    Canvas.FillRect(Rect(P.X - 2, P.Y - 2, P.X, P.Y));
    Canvas.Brush.Color := DragHandleColor1;
    Canvas.FillRect(Rect(P.X - 3, P.Y - 3, P.X - 1, P.Y - 1));
  end;
  
begin
  case Part of
    SBP_BODY:
      PaintIrregularGradient(Canvas.Handle, R, ToolbarColor1, ToolbarColor2, False);
    SBP_PANE, SBP_LASTPANE:
      begin
        if Part = SBP_PANE then Dec(R.Right, 2);
        FrameRectEx(Canvas.Handle, R, StatusPanelFrameColor, False);
      end;
    SBP_GRIPPER:
      begin
        Dec(R.Right);
        Dec(R.Bottom);        
        DrawHandleElement(Point(R.Right, R.Bottom));
        DrawHandleElement(Point(R.Right, R.Bottom - 4));
        DrawHandleElement(Point(R.Right, R.Bottom - 8));
        
        Dec(R.Right, 4);
        DrawHandleElement(Point(R.Right, R.Bottom));
        DrawHandleElement(Point(R.Right, R.Bottom - 4));

        Dec(R.Right, 4);
        DrawHandleElement(Point(R.Right, R.Bottom));
      end;
  end;
end;

procedure TTBXOffice2003Theme.TBXSysCommand(var Message: TMessage);
begin
  if Message.WParam = TSC_VIEWCHANGE then SetupColorCache;
end;

initialization
  if not IsTBXThemeAvailable('Office2003') then
    RegisterTBXTheme('Office2003', TTBXOffice2003Theme);

end.
